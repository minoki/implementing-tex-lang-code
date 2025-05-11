{-# LANGUAGE OverloadedStrings #-}
module Show where
import Control.Monad.State hiding (State)
import Data.Char (chr, ord, intToDigit, toUpper)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Text qualified as T
import Numeric (showHex)
import Common
import State hiding (catcodeMap, escapechar)
import State qualified
import Input (getCatcode)

data ShowContext = ShowContext
  { catcodeMap :: Map Char CatCode
  , escapechar :: Int
  }

-- See print_cs in tex.web.
prependEscapechar :: String -> ShowContext -> String
prependEscapechar s (ShowContext { escapechar = e })
  | isUnicodeScalarValue e = chr e : s
  | otherwise = s

-- Show a token, with a space appended if necessary.
-- See show_token_list in tex.web.
showToken :: Token -> ShowContext -> String
showToken (TCommandName (ControlSeq s)) = case T.unpack s of
  [] -> prependEscapechar "csname" <> prependEscapechar "endcsname "
  s'@[c] -> \ctx ->
    let cc = getCatcode (catcodeMap ctx) c
        t = if cc == CCLetter then [c, ' '] else s'
    in prependEscapechar t ctx
  s' -> prependEscapechar (s' ++ " ")
showToken (TCommandName (ActiveChar c)) = const [c]
showToken (TCharacter c CCParam) = const [c, c]
showToken (TCharacter c _) = const [c]
showToken TFrozenRelax = prependEscapechar "relax "

run :: MonadState State m => (ShowContext -> a) -> m a
run text = do
  LocalState { State.catcodeMap, State.escapechar } <- gets (NE.head . localStates)
  pure $ text $ ShowContext { catcodeMap, escapechar }

-- Show a command name, without a space appended.
showCommandName :: CommandName -> ShowContext -> String
showCommandName (ControlSeq "") = prependEscapechar "csname" <> prependEscapechar "endcsname"
showCommandName (ControlSeq name) =
  prependEscapechar (T.unpack name)
showCommandName (ActiveChar c) = const [c]
