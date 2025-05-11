{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (runStateT)
import Data.Bifunctor (bimap)
import Data.Char (ord)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Prelude hiding (lines)
import System.Console.Haskeline
import Common
import State
import Input
import Execution
import Expansion

booleanPrimitives :: Map CommandName Command
booleanPrimitives = Map.fromList $ map (bimap ControlSeq (Expandable . BooleanConditional))
  [("if", Bif), ("ifcat", Bifcat), ("ifcsname", Bifcsname)
  ,("ifdefined", Bifdefined), ("iffalse", Biffalse)
  ,("ifincsname", Bifincsname), ("ifnum", Bifnum)
  ,("ifodd", Bifodd), ("iftrue", Biftrue), ("ifx", Bifx)
  ]

expandablePrimitives :: Map CommandName Command
expandablePrimitives =
  Map.fromList $ map (bimap ControlSeq Expandable)
  [("begincsname", Ebegincsname), ("csname", Ecsname)
  ,("csstring", Ecsstring), ("detokenize", Edetokenize)
  ,("else", Eelse), ("endinput", Eendinput)
  ,("expandafter", Eexpandafter), ("expanded", Eexpanded)
  ,("fi", Efi), ("ifcase", Eifcase), ("meaning", Emeaning)
  ,("noexpand", Enoexpand), ("number", Enumber)
  ,("or", Eor), ("romannumeral", Eromannumeral)
  ,("scantokens", Escantokens), ("string", Estring)
  ,("the", Ethe), ("unexpanded", Eunexpanded)
  ,("unless", Eunless)
  ]

unexpandablePrimitives :: Map CommandName Command
unexpandablePrimitives =
  Map.fromList $ map (bimap ControlSeq Unexpandable)
  [("advance", Nadvance), ("begingroup", Nbegingroup)
  ,("catcode", Ncatcode), ("chardef", Nchardef)
  ,("count", Ncount), ("countdef", Ncountdef)
  ,("def", Ndef), ("divide", Ndivide), ("edef", Nedef)
  ,("endcsname", Nendcsname), ("endgroup", Nendgroup)
  ,("endlinechar", Nendlinechar)
  ,("escapechar", Nescapechar), ("futurelet", Nfuturelet)
  ,("gdef", Ngdef), ("global", Nglobal)
  ,("ignorespaces", Nignorespaces)
  ,("inputlineno", Ninputlineno), ("lccode", Nlccode)
  ,("let", Nlet), ("long", Nlong), ("lowercase", Nlowercase)
  ,("message", Nmessage), ("multiply", Nmultiply)
  ,("newlinechar", Nnewlinechar), ("numexpr", Nnumexpr)
  ,("outer", Nouter), ("protected", Nprotected)
  ,("relax", Nrelax False), ("show", Nshow)
  ,("showthe", Nshowthe), ("toks", Ntoks)
  ,("toksdef", Ntoksdef), ("uccode", Nuccode)
  ,("uppercase", Nuppercase), ("xdef", Nxdef)
  ]

initialCommandMap :: Map CommandName Command
initialCommandMap = booleanPrimitives <>
  expandablePrimitives <> unexpandablePrimitives

initialLocalState :: LocalState
initialLocalState = LocalState
  { scopeType = GlobalScope
  , commandMap = initialCommandMap
  , State.catcodeMap = defaultCatcodeMap
  , lccodeMap = Map.empty
  , uccodeMap = Map.empty
  , State.endlinechar = 13 -- ord '\r'
  , escapechar = ord '\\'
  , newlinechar = -1
  , countReg = Map.empty
  , toksReg = Map.empty
  }

initialState :: State
initialState = State
  { inputStack = [([], nullInputState)]
  , localStates = NE.singleton initialLocalState
  , conditionalStack = []
  }

main :: IO ()
main = runInputT defaultSettings $ repl initialState

repl :: State -> InputT IO ()
repl s0 = do
  minput <- getInputLine "*"
  case minput of
    Nothing -> return ()
    Just "quit" -> return ()
    Just input -> do
      let execLoop :: State -> InputT IO State
          execLoop s = do
            let result = runStateT (runReaderT execute defaultEContext) s
            case result of
              Left err -> do
                outputStrLn $ "Error: " ++ err
                pure s0
              Right (action, s') -> case action of
                Nothing -> pure s'
                Just (ERCharacter c) -> do
                  outputStrLn $ "<character " ++ show c ++ ">"
                  execLoop s'
                Just (ERMessage msg) -> do
                  outputStrLn msg
                  execLoop s'
                Just ERBeginGroup -> execLoop s'
                Just EREndGroup -> execLoop s'
                Just ERMathShift -> do
                  outputStrLn "<math shift>"
                  execLoop s'
                Just ERAlignmentTab -> do
                  outputStrLn "<alignment tab>"
                  execLoop s'
                Just ERSup -> do
                  outputStrLn "<superscript>"
                  execLoop s'
                Just ERSub -> do
                  outputStrLn "<subscript>"
                  execLoop s'
                Just ERSpace -> do
                  outputStrLn "<space>"
                  execLoop s'
          LocalState { State.catcodeMap, State.endlinechar } = NE.head (localStates s0)
          inputContext = InputContext { Input.catcodeMap = catcodeMap, Input.endlinechar = endlinechar }
          inputState = InputState
            { lines = [appendEndlinechar inputContext (T.pack input)]
            , lineState = NewLine
            , lineNo = 1 }
      s' <- execLoop (s0 { inputStack = [([], inputState)] })
      repl s'
