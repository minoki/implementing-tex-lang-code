module Common where
import Data.Text (Text)

-- data CatCode
-- data CommandName
-- data Token
-- isUnicodeScalarValue

data CatCode = CCEscape       -- 0, escape character
             | CCBeginGroup   -- 1, beginning of group
             | CCEndGroup     -- 2, end of group
             | CCMathShift    -- 3, math shift
             | CCAlignmentTab -- 4, alignment tab
             | CCEndLine      -- 5, end of line
             | CCParam        -- 6, parameter
             | CCSup          -- 7, superscript
             | CCSub          -- 8, subscript
             | CCIgnored      -- 9, ignored character
             | CCSpace        -- 10, space
             | CCLetter       -- 11, letter
             | CCOther        -- 12, other character
             | CCActive       -- 13, active character
             | CCComment      -- 14, comment character
             | CCInvalid      -- 15, invalid character
             deriving (Eq, Show, Enum, Bounded)

data CommandName = ControlSeq Text
                 | ActiveChar Char
                 deriving (Eq, Ord, Show)

data Token = TCommandName CommandName
           | TCharacter Char CatCode
           | TFrozenRelax
           deriving (Eq, Show)

isUnicodeScalarValue :: Integral i => i -> Bool
isUnicodeScalarValue x = 0 <= x && x <= 0x10FFFF && not (0xD800 <= x && x <= 0xDFFF)
