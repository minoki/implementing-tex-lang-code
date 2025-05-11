{-# LANGUAGE OverloadedStrings #-}
module Execution where
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.State
import Data.Bits ((.|.), (.&.))
import Data.Char (isDigit, digitToInt)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Common
import Expansion
import State
import Show

-- 命令名に対する代入（\letや\defなどで使う）
assignCommand :: CommandName -> Command -> Assignment
assignCommand name command = Assign $
  \s@(LocalState { commandMap }) ->
    s { commandMap = Map.insert name command commandMap }

runLocalAssignment :: Assignment -> M ()
runLocalAssignment (Assign f) = modify $
  -- スタックの先頭だけに変更を適用する
  \s@(State { localStates = x :| xs }) ->
    s { localStates = f x :| xs }

runGlobalAssignment :: Assignment -> M ()
runGlobalAssignment (Assign f) = modify $
  -- スタックの全てに変更を適用する
  \s@(State { localStates }) ->
    s { localStates = NE.map f localStates }

-- \globalの有無で代入の範囲を切り替える(後述)
runPrefixAssignment :: Prefix -> Assignment -> M ()
runPrefixAssignment p a
  | p == globalPrefix = runGlobalAssignment a
  | p == 0 = runLocalAssignment a
  | otherwise = throwError "You can't use `\\long' or `\\outer' or `\\protected' with an assignment."

-- 前置命令の集合
type Prefix = Word

noPrefix, globalPrefix, longPrefix, outerPrefix, protectedPrefix :: Prefix
noPrefix = 0
globalPrefix = 1
longPrefix = 2
outerPrefix = 4
protectedPrefix = 8

-- 前置命令の処理。これまでに使われた前置命令の集合を引数に取る
doPrefix :: Prefix -> M ()
doPrefix p = do
  et <- nextExpandedEToken
  case et of
    -- 前置命令が続く場合は「使われた前置命令の集合」にそれを追加する
    Just (_, Nglobal) -> doPrefix (p .|. globalPrefix)
    Just (_, Nlong) -> doPrefix (p .|. longPrefix)
    Just (_, Nouter) -> doPrefix (p .|. outerPrefix)
    Just (_, Nprotected) -> doPrefix (p .|. protectedPrefix)

    -- \relaxと空白は無視する
    Just (_, Nrelax {}) -> doPrefix p
    Just (_, Character _ CCSpace) -> doPrefix p

    -- 前置命令を使った代入を行う
    Just (_, Nfuturelet) -> do
      a <- futureletCommand
      runPrefixAssignment p a
    Just (_, Nlet) -> do
      a <- letCommand
      runPrefixAssignment p a
    Just (_, Ndef) -> do
      a <- defCommand p
      runPrefixAssignment (p .&. globalPrefix) a
    Just (_, Ngdef) -> do a <- defCommand p
                          runGlobalAssignment a
    Just (_, Nedef) -> do
      a <- edefCommand p
      runPrefixAssignment (p .&. globalPrefix) a
    Just (_, Nxdef) -> do
      a <- edefCommand p
      runGlobalAssignment a
    Just (_, DefinedCount i) -> do
      a <- definedCountCommand i
      runPrefixAssignment p a
    Just (_, Nadvance) -> do
      a <- advanceCommand
      runPrefixAssignment p a
    Just (_, Ncatcode) -> do
      a <- catcodeCommand
      runPrefixAssignment p a
    Just (_, Nchardef) -> do
      a <- chardefCommand
      runPrefixAssignment p a
    Just (_, Ncount) -> do
      a <- countCommand
      runPrefixAssignment p a
    Just (_, Ncountdef) -> do
      a <- countdefCommand
      runPrefixAssignment p a
    Just (_, Ndivide) -> do
      a <- divideCommand
      runPrefixAssignment p a
    Just (_, Nendlinechar) -> do
      a <- endlinecharCommand
      runPrefixAssignment p a
    Just (_, Nescapechar) -> do
      a <- escapecharCommand
      runPrefixAssignment p a
    Just (_, Nmultiply) -> do
      a <- multiplyCommand
      runPrefixAssignment p a
    Just (_, Nnewlinechar) -> do
      a <- newlinecharCommand
      runPrefixAssignment p a
    Just (_, DefinedToks i) -> do
      a <- definedToksCommand i
      runPrefixAssignment p a
    Just (_, Ntoks) -> do
      a <- toksCommand
      runPrefixAssignment p a
    Just (_, Ntoksdef) -> do
      a <- toksdefCommand
      runPrefixAssignment p a
    Just (_, m) -> throwError $ "You can't use a prefix with " ++ show m
    Nothing -> throwError "Unexpected end of input after a prefix"

letCommand :: M Assignment
letCommand = do
  name <- readCommandName
  readEqualsWithoutExpansion
  readOneOptionalSpaceWithoutExpansion
  (_, command) <- nextETokenWithoutExpansion <??> throwError "Unexpected end of input"
  pure $ assignCommand name command

futureletCommand :: M Assignment
futureletCommand = do
  name <- readCommandName
  (t1, _) <- nextETokenWithoutExpansion <??> throwError "Unexpected end of input"
  (t2, command) <- nextETokenWithoutExpansion <??> throwError "Unexpected end of input"
  unreadToken $ t2 { notexpanded = False }
  unreadToken $ t1 { notexpanded = False }
  pure $ assignCommand name command

defCommand :: Prefix -> M Assignment
defCommand = defOrEdefCommand (readUntilEndGroup AllowPar)

defOrEdefCommand :: M [Token] -> Prefix -> M Assignment
defOrEdefCommand readReplacementText prefix = do
  name <- readCommandName
  (delimiterBeforeFirstParam, paramSpec, extraBrace)
    <- readParameterText
  replacement <- readReplacementText
  let numberOfParameters = length paramSpec
  LocalState { commandMap } <- getLocalState
  -- 展開後のテキストを処理する関数
  let processReplacement :: [Replacement] -> [Token]
                         -> M [Replacement]
      processReplacement revAcc [] = pure $ reverse revAcc
      processReplacement revAcc (t1:t2:xs)
        | isParam commandMap t1, isParam commandMap t2
        = processReplacement (RToken t2 : revAcc) xs
      processReplacement revAcc (t1:TCharacter c CCOther:xs)
        | isParam commandMap t1, isDigit c, let i = digitToInt c
        , 1 <= i, i <= numberOfParameters
        = processReplacement (RPlaceholder (i - 1) : revAcc) xs
      processReplacement revAcc (t:xs)
        | isParam commandMap t = throwError $ "Illegal parameter number in definition of " ++ show name
        | otherwise
        = processReplacement (RToken t : revAcc) xs
  replacement' <- processReplacement [] replacement
  pure $ assignCommand name $ Expandable $ Macro
    { long = prefix .&. longPrefix /= 0
    , outer = prefix .&. outerPrefix /= 0
    , protected = prefix .&. protectedPrefix /= 0
    , delimiterBeforeFirstParam = delimiterBeforeFirstParam
    , paramSpec = paramSpec
    , replacement = replacement' ++ map RToken extraBrace
    }

-- トークンが（明示的な、あるいは暗黙の）パラメーター文字か判別する関数
isParam :: Map CommandName Command -> Token -> Bool
isParam _ (TCharacter _ cc) = cc == CCParam
isParam commandMap (TCommandName name) =
  case Map.lookup name commandMap of
    Just (Unexpandable (Character _ CCParam)) -> True
    _ -> False
isParam _ TFrozenRelax = False

edefCommand :: Prefix -> M Assignment
edefCommand = defOrEdefCommand edefReadUntilEndGroup

chardefCommand :: M Assignment
chardefCommand = do
  name <- readCommandName
  runLocalAssignment $ assignCommand name $ Unexpandable $ Nrelax False
  readExpandedEquals
  value <- readCharacterCode
  pure $ assignCommand name $ Unexpandable $ DefinedCharacter value

countCommand :: M Assignment
countCommand = do
  i <- readRegisterCode
  readExpandedEquals
  value <- readNumber
  pure $ Assign $ \s@(LocalState { countReg }) ->
    s { countReg = Map.insert i value countReg }

catcodeCommand :: M Assignment
catcodeCommand = do
  c <- readCharacterCode
  readExpandedEquals
  value <- readSmallInt
  unless (0 <= value && value <= 15) $
    throwError "\\catcode: Invalid category code"
  pure $ Assign $ \s@LocalState { State.catcodeMap = m } ->
    s { State.catcodeMap = Map.insert c (toEnum value) m }

endlinecharCommand :: M Assignment
endlinecharCommand = do
  readExpandedEquals
  value <- readSmallInt
  pure $ Assign $ \s -> s { endlinechar = value }

escapecharCommand :: M Assignment
escapecharCommand = do
  readExpandedEquals
  value <- readSmallInt
  pure $ Assign $ \s -> s { State.escapechar = value }

newlinecharCommand :: M Assignment
newlinecharCommand = do
  readExpandedEquals
  value <- readSmallInt
  pure $ Assign $ \s -> s { newlinechar = value }

advanceCommand :: M Assignment
advanceCommand = do
  (t, v) <- nextExpandedToken <??> throwError "\\advance"
  case getQuantity v of
    Just (QIntegerVariable var) -> do
      Variable { value, set } <- var
      readOptionalKeyword "by"
      delta <- readNumber
      pure $ set $ value + delta
    Just (QIntVariable var) -> do
      Variable { value, set } <- var
      readOptionalKeyword "by"
      delta <- readNumber
      let newValue = toInteger value + delta
          minInt = toInteger (minBound :: Int)
          maxInt = toInteger (maxBound :: Int)
      if minInt <= newValue && newValue <= maxInt then
        pure $ set $ fromInteger newValue
      else
        throwError "Arithmetic overflow"
    _ -> throwError $ "You can't use `" ++ show t ++ "' after \\advance"

multiplyCommand :: M Assignment
multiplyCommand = do
  (t, v) <- nextExpandedToken <??> throwError "\\multiply"
  case getQuantity v of
    Just (QIntegerVariable var) -> do
      Variable { value, set } <- var
      readOptionalKeyword "by"
      factor <- readNumber
      pure $ set $ value * factor
    Just (QIntVariable var) -> do
      Variable { value, set } <- var
      readOptionalKeyword "by"
      factor <- readNumber
      let newValue = toInteger value * factor
          minInt = toInteger (minBound :: Int)
          maxInt = toInteger (maxBound :: Int)
      if minInt <= newValue && newValue <= maxInt then
        pure $ set $ fromInteger newValue
      else
        throwError "Arithmetic overflow"
    _ -> throwError $ "You can't use `" ++ show t ++ "' after \\multiply"

divideCommand :: M Assignment
divideCommand = do
  (t, v) <- nextExpandedToken <??> throwError "\\divide"
  case getQuantity v of
    Just (QIntegerVariable var) -> do
      Variable { value, set } <- var
      readOptionalKeyword "by"
      divisor <- readNumber
      if divisor == 0 then
        throwError "Arithmetic overflow"
      else
        pure $ set $ value `quot` divisor
    Just (QIntVariable var) -> do
      Variable { value, set } <- var
      readOptionalKeyword "by"
      divisor <- readNumber
      if divisor == 0 then
        throwError "Arithmetic overflow"
      else do
        let newValue = toInteger value `quot` divisor
            minInt = toInteger (minBound :: Int)
            maxInt = toInteger (maxBound :: Int)
        if minInt <= newValue && newValue <= maxInt then
          pure $ set $ fromInteger newValue
        else
          throwError "Arithmetic overflow"
    _ -> throwError $ "You can't use `" ++ show t ++ "' after \\divide"

messageCommand :: M String
messageCommand = do
  text <- readExpandedGeneralText
  Show.run $ mconcat $ map Show.showToken text

countdefCommand :: M Assignment
countdefCommand = do
  name <- readCommandName
  runLocalAssignment $ assignCommand name $ Unexpandable $ Nrelax False
  readExpandedEquals
  i <- readRegisterCode
  pure $ assignCommand name $ Unexpandable $ DefinedCount i

definedCountCommand :: Int -> M Assignment
definedCountCommand i = do
  readExpandedEquals
  newValue <- readNumber
  pure $ Assign $ \s@(LocalState { countReg }) -> s { countReg = Map.insert i newValue countReg }

toksCommand :: M Assignment
toksCommand = do
  i <- readRegisterCode
  readExpandedEquals
  newValue <- readGeneralTextWithoutExpansion
  pure $ Assign $ \s@(LocalState { toksReg }) ->
    s { toksReg = Map.insert i newValue toksReg }

toksdefCommand :: M Assignment
toksdefCommand = do
  name <- readCommandName
  runLocalAssignment $ assignCommand name $ Unexpandable $ Nrelax False
  readExpandedEquals
  i <- readRegisterCode
  pure $ assignCommand name $ Unexpandable $ DefinedToks i

definedToksCommand :: Int -> M Assignment
definedToksCommand i = do
  readExpandedEquals
  newValue <- readGeneralTextWithoutExpansion
  pure $ Assign $ \s@(LocalState { toksReg }) ->
    s { toksReg = Map.insert i newValue toksReg }

data ExecutionResult = ERCharacter Char
                     | ERBeginGroup
                     | EREndGroup
                     | ERMathShift
                     | ERAlignmentTab
                     | ERSup
                     | ERSub
                     | ERSpace
                     | ERMessage String -- \messageの出力

execute :: M (Maybe ExecutionResult)
execute = do
  et <- nextExpandedEToken
  case et of
    Nothing -> pure Nothing -- end of input
    Just (_, Character _ CCBeginGroup) -> do
      enter ScopeByBrace -- スコープに入る
      pure $ Just ERBeginGroup
    Just (_, Character _ CCEndGroup) -> do
      leave ScopeByBrace -- スコープを抜ける
      pure $ Just EREndGroup
    Just (_, Character _ CCMathShift) ->
      pure $ Just ERMathShift
    Just (_, Character _ CCAlignmentTab) ->
      pure $ Just ERAlignmentTab
    Just (_, Character _ CCParam) ->
      throwError "You can't use `macro parameter character #' in current mode"
    Just (_, Character _ CCSup) -> pure $ Just ERSup
    Just (_, Character _ CCSub) -> pure $ Just ERSub
    Just (_, Character _ CCSpace) -> pure $ Just ERSpace
    Just (_, Character c _) ->
      pure $ Just $ ERCharacter c -- CCLetter, CCOther
    Just (_, DefinedCharacter c) ->
      pure $ Just $ ERCharacter c
    Just (_, Nrelax {}) -> execute -- 何もせず続行する
    Just (_, Nbegingroup) -> do
      enter ScopeByBeginGroup -- スコープに入る
      execute
    Just (_, Nendgroup) -> do
      leave ScopeByBeginGroup -- スコープを抜ける
      execute
    Just (_, Nendcsname) ->
      throwError "Extra \\endcsname"
    Just (_, Nfuturelet) -> do a <- futureletCommand
                               runLocalAssignment a
                               execute
    Just (_, Nglobal) -> doPrefix globalPrefix >> execute
    Just (_, Nlet) -> do a <- letCommand
                         runLocalAssignment a
                         execute
    Just (_, Nlong) -> doPrefix longPrefix >> execute
    Just (_, Nouter) -> doPrefix outerPrefix >> execute
    Just (_, Nprotected) -> doPrefix protectedPrefix >> execute
    Just (_, Ndef) -> do a <- defCommand noPrefix
                         runLocalAssignment a
                         execute
    Just (_, Ngdef) -> do a <- defCommand noPrefix
                          runGlobalAssignment a
                          execute
    Just (_, Nedef) -> do a <- edefCommand noPrefix
                          runLocalAssignment a
                          execute
    Just (_, Nxdef) -> do a <- edefCommand noPrefix
                          runGlobalAssignment a
                          execute
    Just (_, Nmessage) ->
      Just . ERMessage <$> messageCommand
    Just (_, DefinedCount i) -> do
      a <- definedCountCommand i
      runLocalAssignment a
      execute
    Just (_, Nadvance) -> do
      a <- advanceCommand
      runLocalAssignment a
      execute
    Just (_, Ncatcode) -> do
      a <- catcodeCommand
      runLocalAssignment a
      execute
    Just (_, Nchardef) -> do
      a <- chardefCommand
      runLocalAssignment a
      execute
    Just (_, Ncount) -> do
      a <- countCommand
      runLocalAssignment a
      execute
    Just (_, Ncountdef) -> do
      a <- countdefCommand
      runLocalAssignment a
      execute
    Just (_, Ndivide) -> do
      a <- divideCommand
      runLocalAssignment a
      execute
    Just (_, Nendlinechar) -> do
      a <- endlinecharCommand
      runLocalAssignment a
      execute
    Just (_, Nescapechar) -> do
      a <- escapecharCommand
      runLocalAssignment a
      execute
    Just (_, Nmultiply) -> do
      a <- multiplyCommand
      runLocalAssignment a
      execute
    Just (_, Nnewlinechar) -> do
      a <- newlinecharCommand
      runLocalAssignment a
      execute
    Just (_, Nnumexpr) -> throwError "You can't use `\\numexpr' in current mode"
    Just (_, DefinedToks i) -> do a <- definedToksCommand i
                                  runLocalAssignment a
                                  execute
    Just (_, Ntoks) -> do a <- toksCommand
                          runLocalAssignment a
                          execute
    Just (_, Ntoksdef) -> do a <- toksdefCommand
                             runLocalAssignment a
                             execute
