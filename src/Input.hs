{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Input where
import Control.Applicative ((<|>))
import Control.Monad (guard, when, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT), hoistMaybe)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.Char (chr, digitToInt, isLetter, isDigit, ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as T (hexadecimal)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Prelude hiding (lines)
import Common

-- data InputContext
-- initialInputContext
-- data InputState
-- appendEndlinechar
-- nextChar
-- data LineState
-- nextToken
-- readControlSequence
-- nullInputState, readFileAsLines, newInputState

data InputContext
  = InputContext { catcodeMap  :: Map Char CatCode
                 , endlinechar :: Int
                 }
  deriving Show

initialInputContext :: InputContext
initialInputContext = InputContext
  { catcodeMap = defaultCatcodeMap
  , endlinechar = 13 -- '\r'
  }

getCatcode :: Map Char CatCode -> Char -> CatCode
getCatcode m c = case Map.lookup c m of
                   Just cc -> cc
                   Nothing -> if isLetter c then
                                CCLetter
                              else
                                CCOther

defaultCatcodeMap :: Map Char CatCode
defaultCatcodeMap = Map.fromList
  [('\\', CCEscape) -- IniTeX
  ,('{', CCBeginGroup)
  ,('}', CCEndGroup)
  ,('$', CCMathShift)
  ,('&', CCAlignmentTab)
  ,('\r', CCEndLine)
  ,('#', CCParam)
  ,('^', CCSup)
  ,('_', CCSub)
  ,('\NUL', CCIgnored)
  ,(' ', CCSpace)
  ,('~', CCActive)
  ,('%', CCComment) -- IniTeX
  ,('\DEL', CCInvalid) -- IniTeX
  ]

data InputState
  = InputState { lines     :: [Text]
               , lineState :: LineState
               , lineNo    :: Int -- Used by \inputlineno
               }
  deriving Show

appendEndlinechar :: InputContext -> Text -> Text
appendEndlinechar (InputContext { endlinechar = i }) line
  | isUnicodeScalarValue i = T.snoc line (chr i)
  | otherwise = line

isLowerHexDigit :: Char -> Bool
isLowerHexDigit c = isDigit c || ('a' <= c && c <= 'f')

nextChar :: InputContext -> Text
         -> Either String (Maybe (Char, CatCode, Text))
nextChar (InputContext { catcodeMap }) s = runMaybeT $ do
  (c0, rest0) <- hoistMaybe $ T.uncons s
  let cc0 = getCatcode catcodeMap c0
      superscriptNotation = do
        guard (cc0 == CCSup) -- 1文字目のカテゴリーコードが6なら
        trySuperscriptNotation c0 rest0 -- ^^記法を試す
  -- <|> は、「左側のパースを試みて、失敗したら右側を実行する」演算子
  superscriptNotation <|> pure (c0, cc0, rest0)
  where
    trySuperscriptNotation :: Char -> Text
      -> MaybeT (Either String) (Char, CatCode, Text)
    trySuperscriptNotation c0 rest0 = do
      (c1, rest1) <- hoistMaybe $ T.uncons rest0
      guard (c0 == c1)
      (c2, rest2) <- hoistMaybe $ T.uncons rest1
      let twoHexDigits :: Maybe (Char, CatCode, Text)
          twoHexDigits = do
            guard (isLowerHexDigit c2)
            (c3, rest3) <- T.uncons rest2
            guard (isLowerHexDigit c3)
            let c = chr (digitToInt c2 * 16 + digitToInt c3)
            pure (c, getCatcode catcodeMap c, rest3)
      let xor64 = let c = chr (ord c2 `xor` 64)
                  in (c, getCatcode catcodeMap c, rest2)
      -- ^^^^^^記法→^^^^記法→十六進2桁→1文字の順で試す
      result <- fourOrSixHexDigits c2 rest2
        <|> hoistMaybe twoHexDigits <|> pure xor64
      case result of
        (c, CCSup, rest) ->
          -- 記法の処理結果のカテゴリーコードが6なら、さらに試す
          trySuperscriptNotation c rest <|> pure result
        _ -> pure result
      where
        -- ^^^^^^記法と^^^^記法の実装
        fourOrSixHexDigits :: Char -> Text
          -> MaybeT (Either String) (Char, CatCode, Text)
        fourOrSixHexDigits c2 rest2 = do
          guard (c0 == c2)
          (c3, rest3) <- hoistMaybe $ T.uncons rest2
          guard (c0 == c3)
          sixHexDigits rest3 <|> lift (fourHexDigits rest3)
        -- 以下の実装ではバックトラックを行わない（LuaTeX風）
        -- unlessをguardに変えればバックトラックする実装になる
        fourHexDigits rest3 = do
          let (digits, rest4) = T.splitAt 4 rest3
          when (T.length digits < 4 ||
                not (T.all isLowerHexDigit digits)) $
            throwError "^^^^ needs four hex digits"
          (x, _) <- T.hexadecimal digits
          unless (isUnicodeScalarValue x) $
            throwError "invalid Unicode scalar value"
          let c = chr x
          pure (c, getCatcode catcodeMap c, rest4)
        sixHexDigits rest3 = do
          (c4, rest4) <- hoistMaybe $ T.uncons rest3
          guard (c0 == c4)
          (c5, rest5) <- hoistMaybe $ T.uncons rest4
          guard (c0 == c5)
          let (digits, rest6) = T.splitAt 6 rest5
          when (T.length digits < 6 ||
                not (T.all isLowerHexDigit digits)) $
            throwError "^^^^^^ needs six hex digits"
          (x, _) <- lift $ T.hexadecimal digits
          unless (isUnicodeScalarValue x) $
            throwError "invalid Unicode scalar value"
          let c = chr x
          pure (c, getCatcode catcodeMap c, rest6)

data LineState = NewLine -- State 'N': 空白を読み飛ばす（行頭）
               | SkipSpaces -- State 'S': 空白を読み飛ばす（行中）
               | MiddleOfLine -- State 'M': 空白を読み飛ばさない
               deriving (Eq, Show)

nextToken :: InputContext -> InputState
          -> Either String (Maybe (Token, InputState))
nextToken _ctx (InputState { lines = [] })
  = pure Nothing
nextToken ctx state@(InputState { lines = currentLine : rest, lineState, lineNo }) = do
  n <- nextChar ctx currentLine
  case n of
    Nothing -> nextToken ctx nextStateWithNewLine
    Just (c, cc, restOfLine) -> case cc of
      -- 一部のカテゴリーコードの文字は状態を変える等の特殊な動作をする
      CCEscape -> -- 制御綴（後述）
        readControlSequence ctx restOfLine rest lineNo
      CCEndLine -> case lineState of
        NewLine -> -- 空行：\parを生成する
          pure $ Just (parToken, nextStateWithNewLine)
        MiddleOfLine -> -- 半角空白を生成する
          pure $ Just (spaceToken, nextStateWithNewLine)
        SkipSpaces ->
          nextToken ctx nextStateWithNewLine -- ignored
      CCIgnored ->
        -- 無視する。lineStateも変更しない
        nextToken ctx (state { lines = restOfLine : rest })
      CCSpace -> case lineState of
        MiddleOfLine ->
          -- 直前の文字が空白以外：
          -- 常に文字コードU+0020の空白トークンを生成する
          pure $ Just (spaceToken, state { lines = restOfLine : rest, lineState = SkipSpaces })
        _ ->
          -- 直前の文字が空白：空白の後の空白は無視する
          nextToken ctx (state { lines = restOfLine : rest })
      CCComment -> nextToken ctx nextStateWithNewLine
      CCInvalid -> throwError "invalid character"

      -- 他のカテゴリーコードの場合は、対応する文字トークンを生成する
      CCBeginGroup ->
        pure $ Just (TCharacter c CCBeginGroup, nextState)
      CCEndGroup ->
        pure $ Just (TCharacter c CCEndGroup, nextState)
      CCMathShift ->
        pure $ Just (TCharacter c CCMathShift, nextState)
      CCAlignmentTab ->
        pure $ Just (TCharacter c CCAlignmentTab, nextState)
      CCParam ->
        pure $ Just (TCharacter c CCParam, nextState)
      CCSup -> pure $ Just (TCharacter c CCSup, nextState)
      CCSub -> pure $ Just (TCharacter c CCSub, nextState)
      CCLetter ->
        pure $ Just (TCharacter c CCLetter, nextState)
      CCOther ->
        pure $ Just (TCharacter c CCOther, nextState)
      CCActive ->
        pure $ Just (TCommandName (ActiveChar c), nextState)
      where
        nextState = state { lines = restOfLine : rest
                          , lineState = MiddleOfLine }
    where
      nextStateWithNewLine = InputState
        { lines = case rest of
                    l : ls -> appendEndlinechar ctx l : ls
                    [] -> []
        , lineState = NewLine
        , lineNo = lineNo + 1
        }

parToken, spaceToken :: Token
parToken = TCommandName (ControlSeq "par")
spaceToken = TCharacter ' ' CCSpace

readControlSequence :: InputContext
  -> Text -- この行の残り
  -> [Text] -- 次行以降
  -> Int -- 行番号
  -> Either String (Maybe (Token, InputState))
readControlSequence ctx restOfLine rest lineNo = do
  n' <- nextChar ctx restOfLine
  case n' of
    Nothing ->
      -- 行末に達した
      -- 制御綴により\endlinecharが変更される可能性があるので、
      -- この段階では次の行に移らない
      let token = TCommandName (ControlSeq "")
          nextState = InputState
            { lines = T.empty : rest
            , lineState = SkipSpaces, lineNo = lineNo }
      in pure $ Just (token, nextState)
    Just (c1, CCLetter, restOfLine1) -> go restOfLine1 [c1]
      where -- 制御語 (control word)
        go l acc = nextChar ctx l >>= \case
          Nothing -> do
            -- 行末に達した
            -- 制御綴により\endlinecharが変更される可能性があるので、
            -- この段階では次の行に移らない
            let token = TCommandName
                  (ControlSeq (T.pack (reverse acc)))
                nextState = InputState
                  { lines = T.empty : rest
                  , lineState = SkipSpaces
                  , lineNo = lineNo }
            pure $ Just (token, nextState)
          Just (c', CCLetter, restOfLine') ->
            go restOfLine' (c' : acc)
          Just (c', _, restOfLine') ->
            -- 読み取った文字がletterではなかった
            -- 読み取った文字はストリームに戻す
            let token = TCommandName
                  (ControlSeq (T.pack (reverse acc)))
                nextState = InputState
                  { lines = T.cons c' restOfLine' : rest
                  , lineState = SkipSpaces
                  , lineNo = lineNo }
            in pure $ Just (token, nextState)
    Just (c1, cc1, restOfLine1) -> do
      -- 制御記号・制御空白 (control symbol / space)
      let token = TCommandName (ControlSeq (T.singleton c1))
          lineState' = if cc1 == CCSpace then SkipSpaces
                                         else MiddleOfLine
          nextState = InputState
            { lines = restOfLine1 : rest
            , lineState = lineState', lineNo = lineNo }
      pure $ Just (token, nextState)

nullInputState :: InputState
nullInputState = InputState { lines = []
                            , lineState = NewLine
                            , lineNo = 1
                            }

readFileAsLines :: FilePath -> IO [Text]
readFileAsLines path = do
  content <- BS.readFile path
  let content' = TE.decodeUtf8 content
  -- 行末の半角空白を削除する (The TeXbook, Chapter 8, page 46)
  return $ map (T.dropWhileEnd (== ' ')) $ T.lines content'

newInputState :: InputContext -> [Text] -> InputState
newInputState ctx lines = InputState
  { lines = case lines of
              -- 最初の行末に\endlinecharを付加する
              -- 2行目以降は必要になった時点で付加する
              l : ls -> appendEndlinechar ctx l : ls
              [] -> []
  , lineState = NewLine
  , lineNo = 1
  }
