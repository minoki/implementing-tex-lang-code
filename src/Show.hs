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

meaningBooleanConditional :: BooleanConditional
                          -> ShowContext -> String
meaningBooleanConditional p =
  prependEscapechar $ drop 1 $ show p

meaningExpandable :: Bool -> Expandable -> ShowContext -> String
meaningExpandable _ Undefined = const "undefined"
meaningExpandable _ (BooleanConditional b) = meaningBooleanConditional b
meaningExpandable multiline (Macro { long, outer, protected, delimiterBeforeFirstParam, paramSpec, replacement }) =
  let prefixes = ["protected" | protected] ++
                 ["long" | long] ++ ["outer" | outer]
      prefix = if null prefixes then
                 const ""
               else
                 mconcat (map prependEscapechar prefixes) <> const " "
      doParamSpec i (ParamSpec { paramChar, delimiter }) =
        const [paramChar, i] <> mconcat (map showToken delimiter)
      doReplacement (RToken t) = showToken t
      doReplacement (RPlaceholder i) =
        const [paramChar, intToDigit (i + 1)]
        where ParamSpec { paramChar } = last paramSpec
  in prefix <> const (if multiline then "macro:\n" else "macro:")
       <> mconcat (map showToken delimiterBeforeFirstParam ++ zipWith doParamSpec ['1'..] paramSpec)
       <> const "->" <> mconcat (map doReplacement replacement)
meaningExpandable _ p = prependEscapechar $ drop 1 $ show p

meaningUnexpandable :: Unexpandable -> ShowContext -> String
meaningUnexpandable (Character c cc) =
  const (showCC cc <> [c])
  where
    showCC CCBeginGroup = "begin-group character "
    showCC CCEndGroup = "end-group character "
    showCC CCMathShift = "math shift character "
    showCC CCAlignmentTab = "alignment tab character "
    showCC CCParam = "macro parameter character "
    showCC CCSup = "superscript character "
    showCC CCSub = "subscript character "
    showCC CCSpace = "blank space "
    showCC CCLetter = "the letter "
    showCC _ = "the character " -- CCOther
meaningUnexpandable (DefinedCharacter i) =
  prependEscapechar "char\"" <> const (map toUpper $ showHex (ord i) "")
meaningUnexpandable (DefinedCount i) =
  prependEscapechar "count" <> const (show i)
meaningUnexpandable (DefinedToks i) =
  prependEscapechar "toks" <> const (show i)
meaningUnexpandable (Nrelax {}) = prependEscapechar "relax"
meaningUnexpandable p = prependEscapechar $ drop 1 $ show p

meaning :: Bool -> Command -> ShowContext -> String
meaning ml (Expandable p) = meaningExpandable ml p
meaning _ (Unexpandable p) = meaningUnexpandable p
