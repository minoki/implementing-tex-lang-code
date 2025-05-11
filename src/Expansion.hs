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
-- ...

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
-- ...

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
