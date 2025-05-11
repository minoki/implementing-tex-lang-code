{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
module State where
import Data.Char (isLetter, toLower, toUpper)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Common
import Input

-- 展開可能命令
data Expandable
  = Undefined
  | BooleanConditional BooleanConditional
  | Ebegincsname | Ecsname | Ecsstring | Edetokenize
  | Eelse | Eendinput | Eexpandafter | Eexpanded | Efi
  | Eifcase | Emeaning | Enoexpand | Enumber | Eor
  | Eromannumeral | Escantokens | Estring | Ethe
  | Eunexpanded | Eunless
  -- ...
  | Macro { long :: Bool
          , outer :: Bool
          , protected :: Bool
          , delimiterBeforeFirstParam :: [Token] -- empty if undelimited
          , paramSpec :: [ParamSpec]
          , replacement :: [Replacement]
          }
  deriving (Eq, Show)

data BooleanConditional
  = Bif | Bifcat | Bifcsname | Bifdefined | Biffalse
  | Bifincsname | Bifnum | Bifodd | Biftrue | Bifx
  -- ...
  deriving (Eq, Show)

data ParamSpec =
  ParamSpec { paramChar :: Char
            , delimiter :: [Token] -- empty if undelimited
            }
  deriving (Eq, Show)

data Replacement = RToken Token
                 | RPlaceholder Int -- 0-based index
                 deriving (Eq, Show)

-- 展開不能命令
data Unexpandable
  = Character Char CatCode
  | DefinedCharacter Char -- defined by \chardef
  | DefinedCount Int -- defined by \countdef
  | DefinedToks Int -- defined by \toksdef
  | Nrelax { noexpand :: Bool }
  | Nadvance | Nbegingroup | Ncatcode | Nchardef | Ncount
  | Ncountdef | Ndef | Ndivide | Nedef | Nendcsname
  | Nendgroup | Nendlinechar | Nescapechar | Nfuturelet
  | Ngdef | Nglobal | Nignorespaces | Ninputlineno
  | Nlccode | Nlet | Nlong | Nlowercase | Nmessage
  | Nmultiply | Nnewlinechar | Nnumexpr | Nouter
  | Nprotected | Nshow | Nshowthe | Ntoks | Ntoksdef
  | Nuccode | Nuppercase | Nxdef
  -- ...
  deriving (Eq, Show)

-- 命令
data Command = Expandable Expandable
             | Unexpandable Unexpandable
             deriving (Eq, Show)

data ScopeType
  = GlobalScope
  | ScopeByBrace -- { .. }
  | ScopeByBeginGroup -- \begingroup .. \endgroup
  | ScopeByLeftRight -- \left .. \right
  | ScopeByMathShift -- $ .. $ or $$ .. $$
  deriving (Eq, Show)

data LocalState = LocalState
  { scopeType    :: ScopeType
  , commandMap   :: Map CommandName Command
  , catcodeMap   :: Map Char CatCode
  , lccodeMap    :: Map Char Char
  , uccodeMap    :: Map Char Char
  , endlinechar  :: Int
  , escapechar   :: Int
  , newlinechar  :: Int
  , countReg     :: Map Int Integer
  , toksReg      :: Map Int [Token]
  -- 本格的なTeX処理系を作るなら
  --   mathcodeMap, delcodeMap, sfcodeMap
  --   dimenReg, skipReg, muskipReg, box registers
  --   thinmuskip, medmuskip, thickmuskip
  -- などを追加する
  }

data ConditionalKind = CondTruthy
                     | CondFalsy
                     | CondCase
                     | CondTest

data State = State
  { inputStack       :: [([EToken], InputState)]
  , localStates      :: NonEmpty LocalState
  , conditionalStack :: [ConditionalKind]
  }

data EToken = EToken { depth :: Int
                     , token :: Token
                     , notexpanded :: Bool
                     }
            deriving Show

fromPlainToken :: Token -> EToken
fromPlainToken t =
  EToken { depth = 0, token = t, notexpanded = False }

newtype Assignment = Assign (LocalState -> LocalState)

data Variable a = Variable { value :: a
                           , set :: a -> Assignment
                           }

data Quantity m
  = QInteger (m Integer) -- <internal integer>
  | QIntegerVariable (m (Variable Integer)) -- <integer variable>
  | QIntVariable (m (Variable Int)) -- <integer variable>
  | QToks (m [Token])
  -- 組版もやるなら
  --   QDimension: <internal dimen>
  --   QGlue: <internal glue>
  --   QMuGlue: <internal muglue>
  -- を追加する
