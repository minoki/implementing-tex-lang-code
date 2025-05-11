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
-- ...

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
    -- ...
