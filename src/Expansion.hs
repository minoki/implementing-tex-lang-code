{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Expansion where
import Control.Monad (when, unless, void)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict hiding (State)
import Data.Char
import Data.List (isSuffixOf)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as T
import State
import Common
import Input
import Show

data EContext = EContext { maxExpansionDepth :: Int
                         , maxPendingToken :: Int
                         , maxInputStack :: Int
                         , isincsname :: Bool
                         , allowOuter :: Bool
                         }

defaultEContext :: EContext
defaultEContext = EContext { maxExpansionDepth = 1000
                           , maxPendingToken = 1000
                           , maxInputStack = 1000
                           , isincsname = False
                           , allowOuter = True
                           }

-- M は展開器を動作させるモナド
-- M a は EContext -> State -> Either String (a, State) と等価
type M = ReaderT EContext (StateT State (Either String))

checkOuterValidity :: M ()
checkOuterValidity = do
  allowed <- asks allowOuter
  unless allowed $ do
    -- Forbidden control sequence found while scanning ...
    throwError "Forbidden control sequence found"

allowingOuter :: M a -> M a
allowingOuter =
  local (\context -> context { allowOuter = True })

disallowingOuter :: M a -> M a
disallowingOuter =
  local (\context -> context { allowOuter = False })

getLocalState :: M LocalState
getLocalState = gets (NE.head . localStates)

lookupCommand :: CommandName -> M Command
lookupCommand name = do
  LocalState { commandMap } <- getLocalState
  pure $ case Map.lookup name commandMap of
           Nothing -> Expandable Undefined
           Just v  -> v

-- See get_token in tex.web.
nextETokenWithoutExpansionRaw :: M (Maybe EToken)
nextETokenWithoutExpansionRaw = do
  currentStack <- gets inputStack
  case currentStack of
    -- 入力スタックが空の場合
    [] -> pure Nothing
    -- 先読み済みのトークンが存在しない場合：字句解析器を呼び出す
    ([], is):ii -> do
      LocalState { catcodeMap, endlinechar } <- getLocalState
      let inputContext = InputContext { catcodeMap = catcodeMap, endlinechar = endlinechar }
      m <- lift $ lift $ nextToken inputContext is
      case m of
        Nothing -> do
          -- 入力ファイルの終端に達した：
          -- allowOuterをチェックして、上位の入力ストリームに移る
          checkOuterValidity -- end of input
          modify (\s -> s { inputStack = ii })
          nextETokenWithoutExpansionRaw
        Just (t, is') -> do
          -- 字句解析でトークンを生成した
          modify (\s -> s { inputStack = ([], is'):ii })
          pure $ Just $ EToken { depth = 0, token = t, notexpanded = False }
    -- 先読み済みのトークンが存在する場合：それを返す
    (t:ts, is):ii -> do
      modify (\s -> s { inputStack = (ts, is):ii })
      pure $ Just t

-- See get_token in tex.web.
nextETokenWithoutExpansion :: M (Maybe (EToken, Command))
nextETokenWithoutExpansion = do
  m <- nextETokenWithoutExpansionRaw
  case m of
    Nothing -> pure Nothing
    Just et@(EToken { notexpanded = True }) ->
      pure $ Just (et, Unexpandable $ Nrelax { noexpand = True })
    Just et@(EToken { token = TCommandName name }) -> do
      v <- lookupCommand name
      case v of
        Expandable (Macro { outer = True }) ->
          checkOuterValidity
        _ -> pure ()
      pure $ Just (et, v)
    Just et@(EToken { token = TCharacter c cc }) ->
      pure $ Just (et, Unexpandable $ Character c cc)
    Just et@(EToken { token = TFrozenRelax }) ->
      pure $ Just (et, Unexpandable $ Nrelax { noexpand = False })

-- See get_token in tex.web.
nextTokenWithoutExpansion :: M (Maybe (Token, Command))
nextTokenWithoutExpansion = do
  m <- nextETokenWithoutExpansion
  pure $ case m of
    Just (EToken { token }, v) -> Just (token, v)
    Nothing -> Nothing

unreadTokens :: Int -> [EToken] -> M ()
unreadTokens depth tokens = do
  EContext { maxExpansionDepth, maxPendingToken } <- ask
  when (depth >= maxExpansionDepth) $
    throwError "recursion too deep"
  let tokens' = map (\t -> t { depth = depth }) tokens
  currentStack <- gets inputStack
  case currentStack of
    [] -> do
      when (length tokens' > maxPendingToken) $
        throwError "token list too long"
      modify $ \s ->
        s { inputStack = [(tokens', nullInputState)] }
    (tokens'', is):ii -> do
      when (length tokens' + length tokens'' > maxPendingToken) $
        throwError "token list too long"
      modify $ \s ->
        s { inputStack = (tokens' ++ tokens'', is):ii }

unreadToken :: EToken -> M ()
unreadToken token@(EToken { depth }) =
  unreadTokens depth [token]

-- See get_x_token in tex.web.
nextExpandedEToken :: M (Maybe (EToken, Unexpandable))
nextExpandedEToken = do
  m <- nextETokenWithoutExpansion
  case m of
    Nothing -> pure Nothing
    Just (EToken { depth, token }, Expandable e) -> do
      -- 意味が展開可能だったら展開結果を「先読みしたトークン列」に追加し、
      -- 再び展開を試みる
      p <- expand e token
      unreadTokens (depth + 1) p
      nextExpandedEToken
    -- 意味が展開不能だったらそれを返す
    Just (t, Unexpandable v) -> pure $ Just (t, v)

-- See get_x_token in tex.web.
nextExpandedToken :: M (Maybe (Token, Unexpandable))
nextExpandedToken = do
  m <- nextExpandedEToken
  pure $ case m of
    Nothing -> Nothing
    Just (EToken { token }, v) -> Just (token, v)

expand :: Expandable  -- 展開したい命令
       -> Token       -- 展開しようとしているトークン
       -> M [EToken]
expand Undefined t =
  throwError $ "Undefined control sequence " ++ show t
-- 右辺の処理は後述
expand Eexpandafter _ = expandafterCommand
expand Enoexpand _ = noexpandCommand
expand Ecsname _ = map fromPlainToken <$> csnameCommand
expand Ebegincsname _ =
  map fromPlainToken <$> begincsnameCommand
expand (Macro { long, delimiterBeforeFirstParam, paramSpec, replacement }) name = disallowingOuter $ do
  d <- testDelimiter [] delimiterBeforeFirstParam
  unless d $ throwError $ "Use of " ++ show name ++ " doesn't match its definition"
  let allowPar = if long then AllowPar else DisallowPar
  args <- mapM (readArgument allowPar) paramSpec
  let goReplacement (RToken t) = [t]
      goReplacement (RPlaceholder i) = args !! i
  pure $ map fromPlainToken $ concatMap goReplacement replacement
expand Eexpanded _ = map fromPlainToken <$> expandedCommand
expand Eunexpanded _ =
  map fromPlainToken <$> unexpandedCommand
expand Ecsstring _ = map fromPlainToken <$> csstringCommand
expand Edetokenize _ =
  map fromPlainToken <$> detokenizeCommand
expand Estring _ = map fromPlainToken <$> stringCommand

(<??>) :: Monad m => m (Maybe a) -> m a -> m a
(<??>) action e = do r <- action
                     case r of
                       Nothing -> e
                       Just x  -> pure x

noexpandCommand :: M [EToken]
noexpandCommand = do
  (t, v) <- allowingOuter nextETokenWithoutExpansion <??> throwError "\\noexpand"
  pure $ case v of
    Expandable _ -> [t { notexpanded = True }]
    Unexpandable _ -> [t]

expandafterCommand :: M [EToken]
expandafterCommand = do
  (t1, _) <- nextETokenWithoutExpansion <??> throwError "\\expandafter"
  (t2, v2) <- nextETokenWithoutExpansion <??> throwError "\\expandafter"
  expanded <- case v2 of
    Expandable e -> expand e (token t2)
    Unexpandable _ -> pure [t2 { notexpanded = False }]
  pure $ t1 { notexpanded = False } : expanded

readUntilEndcsname :: M Text
readUntilEndcsname =
  local (\ctx -> ctx { isincsname = True }) $ loop []
  where
    loop revAcc = do
      m <- nextExpandedToken <??> throwError "Unexpected end of input" -- Missing \endcsname inserted
      case m of
        (TCharacter c _, _) -> loop (c : revAcc)
        (_, Nendcsname) -> pure $ T.pack $ reverse revAcc
        _ -> throwError "Unexpected token while looking for \\endcsname" -- Missing \endcsname inserted

csnameCommand :: M [Token]
csnameCommand = do
  name <- ControlSeq <$> readUntilEndcsname
  m <- lookupCommand name
  when (m == Expandable Undefined) $ do
    -- THE DREADED SIDE EFFECT OF \csname
    modify $ \s@(State { localStates = l :| ls }) ->
      let relax = Unexpandable $ Nrelax { noexpand = False }
          l' = l { commandMap = Map.insert name relax (commandMap l) }
      in s { localStates = l' :| ls }
  pure [TCommandName name]

begincsnameCommand :: M [Token]
begincsnameCommand = do
  name <- ControlSeq <$> readUntilEndcsname
  m <- lookupCommand name
  pure $ case m of
    Expandable Undefined -> [] -- empty
    _ -> [TCommandName name]

readCommandName :: M CommandName
readCommandName = do
  (t, _) <- nextTokenWithoutExpansion <??> throwError "expected a command name"
  case t of
    TCommandName name -> pure name
    _ -> throwError $ "unexpected character token: " ++ show t

readEqualsWithoutExpansion :: M ()
readEqualsWithoutExpansion = do
  m <- nextETokenWithoutExpansion
  case m of
    Nothing -> pure ()
    Just (EToken { token = TCharacter '=' CCOther }, _) ->
      pure () -- consume '='
    Just (_, Unexpandable (Character _ CCSpace)) ->
      readEqualsWithoutExpansion
    Just (t, _) -> unreadToken t -- keep notexpanded flag

readOneOptionalSpaceWithoutExpansion :: M ()
readOneOptionalSpaceWithoutExpansion = do
  m <- nextETokenWithoutExpansion
  case m of
    Nothing -> pure ()
    Just (_, Unexpandable (Character _ CCSpace)) ->
      pure () -- consume a space (explicit or implicit)
    Just (t, _) -> unreadToken t -- keep notexpanded flag

-- 最初の引数の前のデリミター、引数指定、置換後のテキストに追加されるべきトークン列」を返す
readParameterText :: M ([Token], [ParamSpec], [Token])
readParameterText = disallowingOuter $ go 1 [] [] where
  go i revParamSpecs revDelimiter = do
    u <- nextTokenWithoutExpansion <??> throwError "Unexpected end of input"
    case u of
      (_, Unexpandable (Character paramChar CCParam)) -> do
        (t2, _) <- nextTokenWithoutExpansion <??> throwError "Unexpected end of input"
        case t2 of
          TCharacter c CCOther
            | isDigit c && i == digitToInt c ->
              go (i + 1) ((paramChar, revDelimiter) : revParamSpecs) []
          TCharacter _ CCBeginGroup -> pure $ finalize [t2] [] (t2 : revDelimiter) revParamSpecs
          _ -> throwError "Parameters must be numbered consecutively"
      (TCharacter _ CCBeginGroup, _) ->
        pure $ finalize [] [] revDelimiter revParamSpecs
      (TCharacter _ CCEndGroup, _) ->
        throwError "Unexpected `}' while reading parameter text"
      (t, _) -> go i revParamSpecs (t : revDelimiter)
  -- 引数とデリミターを関連づける
  finalize extraBrace paramSpecs revDelimiter []
    = (reverse revDelimiter, paramSpecs, extraBrace)
  finalize extraBrace paramSpecs revDelimiter1 ((paramChar, revDelimiter2) : xs)
    = finalize extraBrace (ParamSpec paramChar (reverse revDelimiter1) : paramSpecs) revDelimiter2 xs

data AllowPar = AllowPar | DisallowPar deriving Eq

readUntilEndGroup :: AllowPar -> M [Token]
readUntilEndGroup allowPar = disallowingOuter $ fst <$> readUntilEndGroup' allowPar

-- Returns 'end group' character
readUntilEndGroup' :: AllowPar -> M ([Token], Token)
readUntilEndGroup' allowPar = loop 0 [] where
  loop :: Int -> [Token] -> M ([Token], Token)
  loop depth revTokens = do
    (t, _) <- nextTokenWithoutExpansion <??> throwError "expected '}', but got EOF"
    case t of
      TCharacter _ CCEndGroup
        | depth == 0 -> pure (reverse revTokens, t)
        | otherwise -> loop (depth - 1) (t : revTokens)
      TCharacter _ CCBeginGroup ->
        loop (depth + 1) (t : revTokens)
      TCommandName (ControlSeq "par")
        | allowPar == DisallowPar ->
          throwError "Paragraph ended before argument was complete"
      _ -> loop depth (t : revTokens)

testDelimiter :: [EToken] -> [Token] -> M Bool
testDelimiter _revAcc [] = pure True
testDelimiter revAcc (t : ts) = do
  r <- nextETokenWithoutExpansion
  case r of
    Just (e@(EToken { token = u }), _)
      | t == u -> testDelimiter (e : revAcc) ts
      | otherwise -> mapM_ unreadToken (e : revAcc) >> pure False
    Nothing -> mapM_ unreadToken revAcc >> pure False

readDelimitedArgument :: AllowPar -> [Token] -> M [Token]
readDelimitedArgument allowPar delimiter = loop [] where
  loop revAcc = do
    end <- testDelimiter [] delimiter
    if end then
      pure (reverse revAcc)
    else do
      (t, _) <- nextTokenWithoutExpansion <??> throwError "Unexpected end of input"
      case t of
        beginGroup@(TCharacter _ CCBeginGroup) -> do
          (grouped, endGroup) <- readUntilEndGroup' allowPar
          loop (endGroup : reverse grouped ++ beginGroup : revAcc)
        TCharacter _ CCEndGroup ->
          throwError "Argument of macro has an extra }"
        TCommandName (ControlSeq "par")
          | allowPar == DisallowPar ->
            throwError "Paragraph ended before argument was complete"
        _ -> loop (t : revAcc)

readArgument :: AllowPar -> ParamSpec -> M [Token]
readArgument allowPar paramSpec@(ParamSpec { delimiter = [] }) = do
  (t, _) <- nextTokenWithoutExpansion <??> throwError "Unexpected end of input"
  case t of
    TCharacter _ CCSpace ->
      readArgument allowPar paramSpec -- skip
    TCharacter _ CCEndGroup ->
      throwError "unexpected end of group"
    TCharacter _ CCBeginGroup -> readUntilEndGroup allowPar
    TCommandName (ControlSeq "par")
      | allowPar == DisallowPar ->
        throwError "Paragraph ended before argument was complete"
    _ -> pure [t]
readArgument allowPar (ParamSpec { delimiter })
  = readDelimitedArgument allowPar delimiter

edefReadUntilEndGroup :: M [Token]
edefReadUntilEndGroup = disallowingOuter $ loop 0 [] where
  loop :: Int -> [Token] -> M [Token]
  loop level revTokens = do
    (EToken { depth = d, token = t }, m)
      <- nextETokenWithoutExpansion <??> throwError "expected '}', but got EOF"
    case (t, m) of
      (TCharacter _ CCEndGroup, _)
        | level == 0 -> pure $ reverse revTokens
        | otherwise -> loop (level - 1) (t : revTokens)
      (TCharacter _ CCBeginGroup, _) ->
        loop (level + 1) (t : revTokens)
      (_, Expandable e) -> do
        u <- expandInEdef e t
        case u of
          Nothing -> do
            -- 通常：再帰的に展開する
            p <- expand e t
            unreadTokens (d + 1) p
            loop level revTokens
          Just p ->
            -- 一部の命令：再帰的な展開は行わない
            loop level (reverse p ++ revTokens)
      (_, Unexpandable _) -> loop level (t : revTokens)

expandInEdef :: Expandable -> Token -> M (Maybe [Token])
expandInEdef Eunexpanded _ = Just <$> unexpandedCommand
expandInEdef (Macro { protected = True }) t =
  pure $ Just [t]
-- 後で\theに対する処理を追加する
-- ...
expandInEdef _ _ = pure Nothing

readFillerAndLBrace :: M ()
readFillerAndLBrace = disallowingOuter loop where
  loop :: M ()
  loop = do
    (t, v) <- nextExpandedToken <??> throwError "reached EOF while looking for <general text>"
    case v of
      Character _ CCBeginGroup ->
        pure () -- explicit or implicit left brace
      Nrelax {} -> loop -- \relax: ignored
      Character _ CCSpace ->
        loop -- optional spaces: ignored
      _ -> throwError $ "got " ++ show t ++ " while looking for <general text>" -- Missing { inserted

readGeneralTextWithoutExpansion :: M [Token]
readGeneralTextWithoutExpansion = do
  readFillerAndLBrace
  readUntilEndGroup AllowPar

unexpandedCommand :: M [Token]
unexpandedCommand = readGeneralTextWithoutExpansion

readExpandedGeneralText :: M [Token]
readExpandedGeneralText = do
  readFillerAndLBrace
  edefReadUntilEndGroup

expandedCommand :: M [Token]
expandedCommand = readExpandedGeneralText

enter :: ScopeType -> M ()
enter st = modify $
  \s@(State { localStates = localStates@(ls :| _) }) ->
    s { localStates = ls { scopeType = st } NE.<| localStates }

leave :: ScopeType -> M ()
leave st = do
  LocalState { scopeType } :| lss <- gets localStates
  if scopeType == st then
    case lss of
      [] -> throwError ("Extra " ++ ending st) -- or "Too many }'s"
      ls:lss' ->
        modify $ \s -> s { localStates = ls :| lss' }
  else
    throwError $ "mismatched braces: begun by " ++ beginning st ++ ", ended by " ++ ending st
  where
    beginning ScopeByBrace      = "left brace `{'"
    beginning ScopeByBeginGroup = "\\begingroup"
    beginning GlobalScope       = "<beginning of input>"
    beginning ScopeByLeftRight  = "\\left"
    beginning ScopeByMathShift  = "`$' or `$$'"
    ending ScopeByBrace      = "right brace `}'"
    ending ScopeByBeginGroup = "\\endgroup"
    ending GlobalScope       = "<end of input>"
    ending ScopeByLeftRight  = "\\right"
    ending ScopeByMathShift  = "`$' or `$$'"

charToToken :: Char -> Token
charToToken ' ' = TCharacter ' ' CCSpace
charToToken c = TCharacter c CCOther

stringCommand :: M [Token]
stringCommand = do
  (t, _) <- allowingOuter nextTokenWithoutExpansion <??> throwError "\\string"
  let s = case t of
        TCommandName name -> showCommandName name
        TCharacter c _ -> const [c]
        TFrozenRelax -> showCommandName (ControlSeq "relax")
  map charToToken <$> Show.run s

csstringCommand :: M [Token]
csstringCommand = do
  (t, _) <- allowingOuter nextTokenWithoutExpansion <??> throwError "\\csstring"
  pure $ map charToToken $ case t of
    TCommandName (ControlSeq name) -> T.unpack name
    TCommandName (ActiveChar c) -> [c]
    TCharacter c _ -> [c]
    TFrozenRelax -> "relax"

detokenizeCommand :: M [Token]
detokenizeCommand = do
  toks <- readGeneralTextWithoutExpansion
  map charToToken <$> Show.run (mconcat $ map Show.showToken toks)
