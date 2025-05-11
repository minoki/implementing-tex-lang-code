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
expand Enumber _ = map fromPlainToken <$> numberCommand
expand Eromannumeral _ =
  map fromPlainToken <$> romannumeralCommand
expand (BooleanConditional b) _ =
  expandBooleanConditional (evalBooleanConditional b)
expand Eelse self = map fromPlainToken <$> elseCommand self
expand Efi self = map fromPlainToken <$> fiCommand self
expand Eifcase _ = ifcaseCommand
expand Eor self = map fromPlainToken <$> orCommand self
expand Eunless _ = unlessCommand

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

-- <optional signs>とその直後の展開不能トークンを読み取り、
-- 符号（±1）、展開不能トークン、展開不能命令を返す
readOptionalSigns :: Integer -- 引数は±1
                  -> M (Integer, Token, Unexpandable)
readOptionalSigns s = do
  x <- nextExpandedToken <??> throwError "Unexpected end of input"
  case x of
    (TCharacter '+' CCOther, _) -> readOptionalSigns s
    (TCharacter '-' CCOther, _) -> readOptionalSigns (-s)
    (_, Character _ CCSpace) ->
      readOptionalSigns s -- space: ignored
    (t, v) -> pure (s, t, v)

readNumber :: M Integer
readNumber = do
  (sign, t, v) <- readOptionalSigns 1
  (sign *) <$> case t of
    TCharacter '\'' CCOther -> readUnsignedOctal
    TCharacter '"' CCOther -> readUnsignedHex
    TCharacter '`' CCOther -> readCharacterAsCode
    TCharacter c CCOther | isDigit c ->
      readUnsignedDecimalInteger $ toInteger $ digitToInt c
    _ -> case getQuantity v of
      Just (QInteger getInteger) -> getInteger
      Just (QIntegerVariable var) -> value <$> var
      Just (QIntVariable var) -> toInteger . value <$> var
      _ -> throwError $ "Unexpected token while reading a number: " ++ show t -- Missing number, treated as zero.

-- 十進表記を読み取る。引数は最初の数字
readUnsignedDecimalInteger :: Integer -> M Integer
readUnsignedDecimalInteger = readGenericInteger 10 isDigit

-- 八進表記を読み取る
readUnsignedOctal :: M Integer
readUnsignedOctal = do
  (t, _) <- nextExpandedToken <??> throwError "Unexpected end of input"
  case t of
    TCharacter c CCOther | isOctDigit c ->
      readGenericInteger 8 isOctDigit (toInteger $ digitToInt c)
    _ -> throwError $ "unexpected token while reading an octal integer: " ++ show t

-- 十六進表記を読み取る
readUnsignedHex :: M Integer
readUnsignedHex = do
  (t, _) <- nextExpandedToken <??> throwError "Unexpected end of input"
  case t of
    TCharacter c CCOther
      | isUpperHexDigit c -> go $ toInteger $ digitToInt c
    TCharacter c CCLetter
      | 'A' <= c, c <= 'F' -> go $ toInteger $ digitToInt c
    _ -> throwError $ "unexpected token while reading a hexadecimal integer: " ++ show t
  where
    go = readGenericInteger 16 isUpperHexDigit
    isUpperHexDigit c = isDigit c || ('A' <= c && c <= 'F')

-- 十進表記または八進表記または十六進表記を読み取る
-- 引数は基数と、数字を判定する関数と、最初の数字
readGenericInteger :: Integer -> (Char -> Bool) -> Integer -> M Integer
readGenericInteger base isXDigit = go where
  go x = do
    m <- nextExpandedEToken
    case m of
      Just (EToken { token = TCharacter c CCOther }, _)
        | isXDigit c ->
          go (base * x + toInteger (digitToInt c))
      Just (EToken { token = TCharacter c CCLetter }, _)
        | base == 16, 'A' <= c, c <= 'F' ->
          go (16 * x + toInteger (digitToInt c))
      Just (_, Character _ CCSpace) ->
        pure x -- space: consumed
      Just (t, _) -> do
        -- strip 'notexpanded' flag
        unreadToken $ t { notexpanded = False }
        pure x
      Nothing -> pure x

readOneOptionalSpace :: M ()
readOneOptionalSpace = do
  m <- nextExpandedEToken
  case m of
    Just (_, Character _ CCSpace) ->
      pure () -- space: consumed
    Just (t, _) ->
      -- strip 'notexpanded' flag
      unreadToken $ t { notexpanded = False }
    Nothing -> pure ()

-- 文字を読む
readCharacterAsCode :: M Integer
readCharacterAsCode = do
  (t, _) <- nextTokenWithoutExpansion <??> throwError "Unexpected end of input"
  readOneOptionalSpace
  toInteger . ord <$> case t of
    TCommandName (ControlSeq name) -> case T.unpack name of
      [c] -> pure c
      _ -> throwError "Improper alphabetic constant"
    TCommandName (ActiveChar c) -> pure c
    TCharacter c _ -> pure c
    TFrozenRelax ->
      throwError "Improper alphabetic constant"

readSmallInt :: M Int
readSmallInt = do
  v <- readNumber
  unless (-0x7fffffff <= v && v <= 0x7fffffff) $
    throwError "Number too big"
  pure $ fromInteger v

readCharacterCode :: M Char
readCharacterCode = do
  i <- readSmallInt
  unless (isUnicodeScalarValue i) $
    throwError "Bad character code"
  pure $ chr i

readRegisterCode :: M Int
readRegisterCode = do
  i <- readSmallInt
  unless (0 <= i && i < 65536) $
    throwError "Bad register code"
  pure i

numberCommand :: M [Token]
numberCommand = map charToToken . show <$> readNumber

romannumeralCommand :: M [Token]
romannumeralCommand =
  map charToToken . showRomannumeral <$> readSmallInt

showRomannumeral :: Int -> String
showRomannumeral x | x <= 0 = ""
                   | otherwise =
  let (e1,d1) = x `quotRem` 10
      (e2,d2) = e1 `quotRem` 10
      (d4,d3) = e2 `quotRem` 10
  in replicate d4 'm' ++ (a3 !! d3) ++ (a2 !! d2) ++ (a1 !! d1)
  where
    a3 = ["", "c", "cc", "ccc", "cd", "d", "dc", "dcc", "dccc", "cm"]
    a2 = ["", "x", "xx", "xxx", "xl", "l", "lx", "lxx", "lxxx", "xc"]
    a1 = ["", "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix"]

readExpandedEquals :: M ()
readExpandedEquals = do
  m <- nextExpandedEToken
  case m of
    Nothing -> pure () -- end of input
    Just (_, Character _ CCSpace) ->
      readExpandedEquals -- consume space
    Just (EToken { token = TCharacter '=' CCOther }, _) ->
      pure () -- consume '='
    Just (t, _) -> unreadToken $ t { notexpanded = False }

readKeyword :: String -> M Bool
readKeyword "" = pure False -- should not occur
readKeyword keyword@(k0:ks) = do
  m <- nextExpandedEToken
  case m of
    Nothing -> pure False -- end of input
    Just (_, Character _ CCSpace) -> 
      readKeyword keyword
    Just (t@EToken { token = TCharacter c _ }, _)
      | toUpper c == toUpper k0 -> readRest [t] ks
    Just (t, _) -> do
      unreadToken $ t { notexpanded = False }
      pure False
  where
    readRest _revAcc [] = pure True
    readRest revAcc (k:kss) = do
      m <- nextExpandedEToken
      case m of
        Just (t@EToken { token = TCharacter c _ }, _)
          | toUpper c == toUpper k ->
            readRest (t:revAcc) kss
        Just (t, _) -> do
          unreadToken $ t { notexpanded = False }
          mapM_ unreadToken revAcc
          pure False
        Nothing -> do
          mapM_ unreadToken revAcc
          pure False

readOptionalKeyword :: String -> M ()
readOptionalKeyword keyword = void $ readKeyword keyword

etexDiv :: Integer -> Integer -> Integer
etexDiv _ 0 = error "divide by zero"
etexDiv x y | x % y >= 0 = floor (x % y + 1/2)
            | otherwise = ceiling (x % y - 1/2)

parseExpression :: Int -> M Integer
parseExpression level = parseTerm >>= readAddOp where
  readAddOp :: Integer -> M Integer
  readAddOp acc = do
    m <- nextExpandedEToken
    case m of
      Nothing -> pure acc
      Just (EToken { token = TCharacter '+' CCOther }, _) ->
        do y <- parseTerm
           readAddOp (acc + y)
      Just (EToken { token = TCharacter '-' CCOther }, _) ->
        do y <- parseTerm
           readAddOp (acc - y)
      Just (EToken { token = TCharacter ')' CCOther }, _)
        | level > 0 -> pure acc
      Just (_, Character _ CCSpace) ->
        readAddOp acc -- consume space
      Just (t, _) -> do
        unreadToken t -- keep 'notexpanded' flag
        pure acc

  parseTerm = parseFactor >>= readMulOp

  readMulOp :: Integer -> M Integer
  readMulOp acc = do
    m <- nextExpandedEToken
    case m of
      Nothing -> pure acc
      Just (EToken { token = TCharacter '*' CCOther }, _) ->
        do y <- parseIntegerFactor level
           readMulOp (acc * y)
      Just (EToken { token = TCharacter '/' CCOther }, _) ->
        do y <- parseIntegerFactor level
           if y == 0
             then throwError "Divide by zero"
             else readMulOp (acc `etexDiv` y)
      Just (EToken { token = TCharacter ')' CCOther }, _)
        | level > 0 -> pure acc
      Just (_, Character _ CCSpace) ->
        readMulOp acc -- consume space
      Just (t, _) -> do
        unreadToken t -- keep 'notexpanded' flag
        pure acc
  
  parseFactor :: M Integer
  parseFactor = do
    (t, v) <- nextExpandedEToken <??> throwError "Unexpected end of input"
    case (t, v) of
      (EToken { token = TCharacter '(' CCOther }, _) ->
        parseExpression (level + 1)
      (_, Character _ CCSpace) -> parseFactor
      _ -> do
        unreadToken $ t { notexpanded = False }
        readNumber

parseIntegerFactor :: Int -> M Integer
parseIntegerFactor level = do
  (t, v) <- nextExpandedEToken <??> throwError "Unexpected end of input"
  case (t, v) of
    (EToken { token = TCharacter '(' CCOther }, _) ->
      parseExpression (level + 1)
    (_, Character _ CCSpace) -> parseIntegerFactor level
    _ -> do
      unreadToken $ t { notexpanded = False }
      readNumber

numexprCommand :: M Integer
numexprCommand = do
  x <- parseExpression 0
  m <- nextExpandedEToken
  case m of
    Just (_, Nrelax {}) -> pure ()
    Just (t, _) -> unreadToken t
    _ -> pure ()
  pure x

getQuantity :: Unexpandable -> Maybe (Quantity M)
getQuantity (DefinedCharacter c) =
  Just $ QInteger $ pure $ toInteger $ ord c
getQuantity Ncount = Just $ QIntegerVariable $ do
  i <- readRegisterCode
  LocalState { countReg = cr } <- getLocalState
  pure $ Variable
    { value = Map.findWithDefault 0 i cr
    , set = \newValue -> Assign $ \s ->
        s { countReg = Map.insert i newValue (countReg s) }
    }
getQuantity (DefinedCount i) = Just $ QIntegerVariable $ do
  LocalState { countReg = cr } <- getLocalState
  pure $ Variable
    { value = Map.findWithDefault 0 i cr
    , set = \newValue -> Assign $ \s ->
        s { countReg = Map.insert i newValue (countReg s) }
    }
getQuantity Ncatcode = Just $ QInteger $ do
  c <- readCharacterCode
  LocalState { catcodeMap } <- getLocalState
  pure $ toInteger $ fromEnum $ Input.getCatcode catcodeMap c
getQuantity Nendlinechar = Just $ QIntVariable $ do
  LocalState { endlinechar } <- getLocalState
  pure $ Variable
    { value = endlinechar
    , set = \newValue -> Assign $
        \s -> s { State.endlinechar = newValue }
    }
getQuantity Nescapechar = Just $ QIntVariable $ do
  LocalState { escapechar } <- getLocalState
  pure $ Variable
    { value = escapechar
    , set = \newValue -> Assign $
        \s -> s { State.escapechar = newValue }
    }
getQuantity Nnewlinechar = Just $ QIntVariable $ do
  LocalState { newlinechar } <- getLocalState
  pure $ Variable
    { value = newlinechar
    , set = \newValue -> Assign $
        \s -> s { newlinechar = newValue }
    }
getQuantity Nnumexpr = Just $ QInteger numexprCommand
getQuantity _ = Nothing

-- 対応する\elseまたは\fiまでスキップする
-- \elseに遭遇した場合はTrueを、\fiに遭遇した場合はFalseを返す
-- 引数は追加でスキップするネストの深さ
skipUntilElse :: Int -> M Bool
skipUntilElse = disallowingOuter . go where
  go :: Int -> M Bool
  go level = do
    (_, v) <- nextETokenWithoutExpansion <??> throwError "Unexpected end of input"
    case v of
      Expandable Eor | level == 0 -> throwError "Extra \\or"
      Expandable Eelse | level == 0 -> pure True
      Expandable Efi | level == 0 -> pure False
                     | otherwise -> go (level - 1)
      Expandable e | isConditional e -> go (level + 1)
      _ -> go level

-- 対応する\fiまでスキップする
skipUntilFi :: M ()
skipUntilFi = disallowingOuter $ go 0 where
  go :: Int -> M ()
  go level = do
    (_, v) <- nextETokenWithoutExpansion <??> throwError "Unexpected end of input"
    case v of
      Expandable Efi | level == 0 -> pure ()
                     | otherwise -> go (level - 1)
      Expandable e | isConditional e -> go (level + 1)
      _ -> go level

data SkipUntilOr = FoundOr | FoundElse | FoundFi

-- 対応する\orまたは\elseまたは\fiまでスキップする
-- 遭遇したトークンの種類を返す
-- 引数は追加でスキップするネストの深さ
skipUntilOr :: Int -> M SkipUntilOr
skipUntilOr = disallowingOuter . go where
  go :: Int -> M SkipUntilOr
  go level = do
    (_, v) <- nextETokenWithoutExpansion <??> throwError "Unexpected end of input"
    case v of
      Expandable Eor | level == 0 -> pure FoundOr
      Expandable Eelse | level == 0 -> pure FoundElse
      Expandable Efi | level == 0 -> pure FoundFi
                     | otherwise -> go (level - 1)
      Expandable e | isConditional e -> go (level + 1)
      _ -> go level

-- \if系の命令を判別する補助関数
isConditional :: Expandable -> Bool
isConditional (BooleanConditional _) = True
isConditional Eifcase = True
isConditional _ = False

-- 条件分岐スタックの指定した位置の値を置き換える
updateConditionalStack :: Int -> ConditionalKind -> M ()
updateConditionalStack i k = do
  cs <- gets conditionalStack
  case splitAt (length cs - i - 1) cs of
    (cs0, CondTest : cs1) -> modify' $
      \s -> s { conditionalStack = cs0 ++ k : cs1 }
    _ -> throwError "Unmatched conditional"

expandBooleanConditional :: M Bool -> M [EToken]
expandBooleanConditional condition = do
  -- 条件分岐スタックにCondTestを積む
  cs0 <- gets conditionalStack
  modify' $ \s -> s { conditionalStack = CondTest : cs0 }
  b <- condition
  if b then
    -- 条件が真：
    -- 積んだ値をCondTruthyに変える
    updateConditionalStack (length cs0) CondTruthy
  else do
    -- 条件が偽：対応する\elseか\fiまでスキップする
    -- 条件部の展開により条件分岐のネストが深くなっている可能性も考慮する
    cs1 <- gets conditionalStack
    e <- skipUntilElse (length cs1 - length cs0 - 1)
    if e then
      -- スキップの結果、\elseに遭遇した
      -- 積んだ値をCondFalsyに変える
      modify' $ \s -> s { conditionalStack = CondFalsy : cs0 }
    else do
      -- スキップの結果、\fiに遭遇した
      -- 積んだCondTestを取り除く
      -- 条件部の展開により\ifのネストが深くなっているかもしれないので
      -- 単に「一番上の値を取り除く」のではダメ
      modify' $ \s -> s { conditionalStack = cs0 }
  -- 展開結果は空のトークン列となる
  pure []

evalBooleanConditional :: BooleanConditional -> M Bool
evalBooleanConditional Bif = ifCommand
evalBooleanConditional Bifcat = ifcatCommand
evalBooleanConditional Bifcsname = ifcsnameCommand
evalBooleanConditional Bifdefined = ifdefinedCommand
evalBooleanConditional Biffalse = iffalseCommand
evalBooleanConditional Bifincsname = ifincsnameCommand
evalBooleanConditional Bifnum = ifnumCommand
evalBooleanConditional Bifodd = ifoddCommand
evalBooleanConditional Biftrue = iftrueCommand
evalBooleanConditional Bifx = ifxCommand

elseCommand :: Token -> M [Token]
elseCommand self = do
  cs <- gets conditionalStack
  case cs of
    CondTruthy : css -> do
      -- \iftrue ... >>>\else<<< ... \fi
      skipUntilFi
      modify' $ \s -> s { conditionalStack = css }
      pure []
    CondCase : css -> do
      -- \ifcase ... \or ... >>>\else<<< ... \fi
      skipUntilFi
      modify' $ \s -> s { conditionalStack = css }
      pure []
    CondTest : _ -> do
      -- 条件部で\elseが使われた：frozen relaxを挿入する
      -- 例えば、'\ifodd1\else\fi' の一回展開は '\relax \else \fi' となる
      pure [TFrozenRelax, self]
    _ -> throwError "Extra \\else"

fiCommand :: Token -> M [Token]
fiCommand self = do
  cs <- gets conditionalStack
  case cs of
    [] -> throwError "Extra \\fi"
    CondTest : _ -> do
      -- 条件部で\fiが使われた：frozen relaxを挿入する
      -- 例えば、'\ifodd1\fi' の一回展開は '\relax \fi' となる
      pure [TFrozenRelax, self]
    _ : css -> do
      modify' $ \s -> s { conditionalStack = css }
      pure []

orCommand :: Token -> M [Token]
orCommand self = do
  cs <- gets conditionalStack
  case cs of
    CondCase : css -> do
      -- \ifcase N ... >>>\or<<< ... \fi
      skipUntilFi
      modify' $ \s -> s { conditionalStack = css }
      pure []
    CondTest : _ -> do
      -- 条件部で\orが使われた：frozen relaxを挿入する
      -- 例えば、'\ifcase0\or\fi' の一回展開は '\relax \or \fi' となる
      pure [TFrozenRelax, self]
    _ -> throwError "Extra \\or"

iftrueCommand, iffalseCommand :: M Bool
iftrueCommand = pure True
iffalseCommand = pure False

ifxCommand :: M Bool
ifxCommand = allowingOuter $ do
  (_, v1) <- nextETokenWithoutExpansion <??> throwError "Unexpected end of input"
  (_, v2) <- nextETokenWithoutExpansion <??> throwError "Unexpected end of input"
  pure $ v1 == v2

-- Used by \if and \ifcat
getCharCodeAndCatCode :: (EToken, Unexpandable)
                      -> Maybe (Char, CatCode)
getCharCodeAndCatCode (_, Character c cc)
  = Just (c, cc) -- explicit or implicit character token
getCharCodeAndCatCode (EToken { token = TCommandName (ActiveChar c), notexpanded = True }, _)
  = Just (c, CCActive) -- \noexpand-ed active character
getCharCodeAndCatCode _ = Nothing

-- \if: test character codes
ifCommand :: M Bool
ifCommand = disallowingOuter $ do
  t1 <- nextExpandedEToken <??> throwError "Unexpected end of input"
  t2 <- nextExpandedEToken <??> throwError "Unexpected end of input"
  pure $ fmap fst (getCharCodeAndCatCode t1)
           == fmap fst (getCharCodeAndCatCode t2)

-- \ifcat: test category codes
ifcatCommand :: M Bool
ifcatCommand = disallowingOuter $ do
  t1 <- nextExpandedEToken <??> throwError "Unexpected end of input"
  t2 <- nextExpandedEToken <??> throwError "Unexpected end of input"
  pure $ fmap snd (getCharCodeAndCatCode t1)
           == fmap snd (getCharCodeAndCatCode t2)

ifoddCommand :: M Bool
ifoddCommand = odd <$> readNumber

ifnumCommand :: M Bool
ifnumCommand = do a <- readNumber
                  rel <- readRelation
                  b <- readNumber
                  pure $ rel == compare a b
  where
    readRelation = do
      m <- nextExpandedToken <??> throwError "Unexpected end of input"
      case m of
        -- (implicit) space: ignored
        (_, Character _ CCSpace) -> readRelation
        (TCharacter '<' CCOther, _) -> pure LT
        (TCharacter '=' CCOther, _) -> pure EQ
        (TCharacter '>' CCOther, _) -> pure GT
        _ -> throwError "invalid relation for \\ifnum"

ifcaseCommand :: M [EToken]
ifcaseCommand = do
  cs <- gets conditionalStack
  modify' $ \s -> s { conditionalStack = CondTest : cs }
  x <- readNumber
  let go 0 _ = updateConditionalStack (length cs) CondCase
      go n extraLevel = do
        k <- skipUntilOr extraLevel
        case k of
          FoundFi ->
            modify' $ \s -> s { conditionalStack = cs }
          FoundElse ->
            modify' $ \s -> s { conditionalStack = CondFalsy : cs }
          FoundOr -> do
            modify' $ \s -> s { conditionalStack = CondTest : cs }
            go (n - 1) 0
  -- 条件部の展開により条件分岐のネストが深くなっている可能性を考慮する
  cs1 <- gets conditionalStack
  go x (length cs1 - length cs - 1)
  pure []

ifdefinedCommand :: M Bool
ifdefinedCommand = do
  (_, v) <- allowingOuter nextETokenWithoutExpansion <??> throwError "Unexpected end of input"
  pure $ v /= Expandable Undefined

ifcsnameCommand :: M Bool
ifcsnameCommand = do
  name <- readUntilEndcsname
  v <- lookupCommand (ControlSeq name)
  pure $ v /= Expandable Undefined

unlessCommand :: M [EToken]
unlessCommand = do
  (t, v) <- nextETokenWithoutExpansion <??> throwError "Unexpected end of input"
  case v of
    Expandable (BooleanConditional b) ->
      expandBooleanConditional
        (not <$> evalBooleanConditional b)
    _ -> throwError $ "You can't use `\\unless' before `" ++ show t ++ "'"

ifincsnameCommand :: M Bool
ifincsnameCommand = asks isincsname
