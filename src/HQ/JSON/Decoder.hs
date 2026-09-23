{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HQ.JSON.Decoder where

import Control.Monad.Error.Class (MonadError (throwError))
import Data.Bits (Bits (..), shiftL)
import Data.Char (digitToInt, isDigit, isHexDigit)
import Data.Scientific (Scientific, scientific)
import qualified Data.Text as T
import HQ.JSON.Decoder.StringBuffer
import HQ.JSON.Event (JSONEvent (..))
import Relude hiding (Compose, id, many, some, state)
import Streaming (Of, Stream)
import qualified Streaming.Prelude as S

data Decoder = Decoder
  { decoderInput :: Text,
    decoderStack :: [Context],
    decoderState :: ParserState
  }
  deriving (Show, Eq)

data ParserState
  = ParserStateValue
  | ParserStateObjectKey
  | ParserStateObjectColon
  | ParserStateObjectComma
  | ParserStateArrayComma
  | ParserStateString StringState
  | ParserStateNumber NumberState
  | ParserStateKeyword KeywordState
  | ParserStateFinished
  deriving (Eq, Show)

data Context = ContextArray | ContextObject
  deriving (Eq, Show)

newtype ReversedString = ReversedString {unReversedString :: String}
  deriving (Eq, Show)

reversedStringToText :: ReversedString -> Text
reversedStringToText (ReversedString str) = T.reverse $ T.pack str

-- | JSON number state machine.
data NumberState = NumberState
  { numberBuffer :: ReversedString,
    numberPhase :: NumberPhase
  }
  deriving (Eq, Show)

-- | Tracks how much of a JSON number has been consumed.
-- -? (0 | [1-9][0-9]*) (\.[0-9]+)? ([eE][+-]?[0-9]+)?
data NumberPhase
  = -- | Saw leading '-'
    NumSign
  | -- | Saw '0' (must not be followed by more digits)
    NumZero
  | -- | Saw [1-9] and possibly more digits
    NumNonZero
  | -- | Saw '.' after integer part (need fractional digits)
    NumDot
  | -- | Saw '.' + at least one digit
    NumFraction
  | -- | Saw 'e'/'E' (waiting for optional sign or digit)
    NumExpSign
  | -- | Saw 'e'/'E' then '+'/'-' (need digits)
    NumExpAfterSign
  | -- | Saw exponent digits
    NumExpDigit
  deriving (Eq, Show)

-- | Is this phase valid at end-of-input?
isValidNumberFinal :: NumberPhase -> Bool
isValidNumberFinal NumZero = True
isValidNumberFinal NumNonZero = True
isValidNumberFinal NumFraction = True
isValidNumberFinal NumExpDigit = True
isValidNumberFinal _ = False

-- | Result of checking a character against the number state machine.
data NumberStep
  = -- | Valid continuation, advance to this phase
    NumStep NumberPhase
  | -- | Delimiter, number is complete
    NumEnd
  | -- | Invalid character in this context
    NumError

-- | Advance the number state machine for one character.
advanceNumber :: NumberPhase -> Char -> NumberStep
advanceNumber phase c
  | isJsonDelimiter c = NumEnd
  | otherwise = case phase of
      NumSign
        | c == '0' -> NumStep NumZero
        | isDigit c -> NumStep NumNonZero
        | otherwise -> NumError
      NumZero
        | c == '.' -> NumStep NumDot
        | c == 'e' || c == 'E' -> NumStep NumExpSign
        | otherwise -> NumError
      NumNonZero
        | isDigit c -> NumStep NumNonZero
        | c == '.' -> NumStep NumDot
        | c == 'e' || c == 'E' -> NumStep NumExpSign
        | otherwise -> NumError
      NumDot
        | isDigit c -> NumStep NumFraction
        | otherwise -> NumError
      NumFraction
        | isDigit c -> NumStep NumFraction
        | c == 'e' || c == 'E' -> NumStep NumExpSign
        | otherwise -> NumError
      NumExpSign
        | isDigit c -> NumStep NumExpDigit
        | c == '+' || c == '-' -> NumStep NumExpAfterSign
        | otherwise -> NumError
      NumExpAfterSign
        | isDigit c -> NumStep NumExpDigit
        | otherwise -> NumError
      NumExpDigit
        | isDigit c -> NumStep NumExpDigit
        | otherwise -> NumError

-- | Is this character a valid JSON delimiter (one that can follow a number)?
isJsonDelimiter :: Char -> Bool
isJsonDelimiter c = isWhitespace c || c `elem` [',', ']', '}']

-- | Determine the starting number phase for the first character of a number.
numberPhaseFromFirstChar :: Char -> Maybe NumberPhase
numberPhaseFromFirstChar '-' = Just NumSign
numberPhaseFromFirstChar '0' = Just NumZero
numberPhaseFromFirstChar c
  | isDigit c = Just NumNonZero
  | otherwise = Nothing

-- | Create an initial NumberState from the first character.
startNumberState :: Char -> NumberState
startNumberState c =
  NumberState
    (ReversedString $ one c)
    (fromMaybe NumSign (numberPhaseFromFirstChar c))

data StringTarget = StringKey | StringValue
  deriving (Eq, Show)

data BufferedStringTarget = BufferedStringTarget StringTarget StringBuffer
  deriving (Eq, Show)

data Unicode = Unicode {unicodeValue :: Int, unicodeDigits :: Int}
  deriving (Eq, Show)

data StringState
  = InString BufferedStringTarget
  | AfterEscape BufferedStringTarget
  | InUnicodeEscape BufferedStringTarget Unicode
  | AfterHighSurrogate BufferedStringTarget Int
  deriving (Eq, Show)

data KeywordState
  = KeywordNull Int
  | KeywordTrue Int
  | KeywordFalse Int
  deriving (Eq, Show)

data DecoderResult
  = Emit JSONEvent Decoder
  | NeedInput Decoder
  | Done Decoder
  deriving (Eq, Show)

data ParseError
  = UnexpectedEnd
  | UnexpectedChar Char
  | UnexpectedToken Text
  | ExpectedColon
  | ExpectedCommaOrEnd
  | ExpectedObjectKey
  | ExpectedValue
  | InvalidEscape Char
  | InvalidUnicodeEscape
  | InvalidSurrogatePair
  | InvalidNumber Text
  | InvalidKeyword Text
  | TrailingInput
  deriving (Eq, Show)

initialDecoder :: Decoder
initialDecoder =
  Decoder
    { decoderInput = mempty,
      decoderStack = [],
      decoderState = ParserStateValue
    }

feed :: Text -> Decoder -> Either ParseError DecoderResult
feed input decoder = step decoder {decoderInput = decoderInput decoder <> input}

finish :: Decoder -> Either ParseError DecoderResult
finish decoder = case decoderState decoder of
  ParserStateString (AfterHighSurrogate _ _) -> Left InvalidSurrogatePair
  ParserStateString _ -> Left UnexpectedEnd
  ParserStateNumber numState
    | isValidNumberFinal (numberPhase numState) -> finalizeNumber decoder
    | otherwise ->
        Left $ InvalidNumber (reversedStringToText $ numberBuffer numState)
  ParserStateKeyword _ -> finalizeKeyword decoder
  ParserStateFinished -> case decoderStack decoder of
    [] ->
      let isNull = T.null (decoderInput decoder)
       in if isNull then Right (Done decoder) else Left TrailingInput
    _ -> Left UnexpectedEnd
  _ -> Left UnexpectedEnd

step :: Decoder -> Either ParseError DecoderResult
step decoder = case decoderState decoder of
  ParserStateString state -> stepString decoder state
  ParserStateNumber numState -> stepNumber decoder numState
  ParserStateKeyword state -> stepKeyword decoder state
  _ -> stepStructural decoder

stepStructural :: Decoder -> Either ParseError DecoderResult
stepStructural decoder = case T.uncons input of
  Nothing -> case decoderState decoder of
    ParserStateFinished ->
      let newDecoder = decoder {decoderInput = mempty}
       in Right
            $ if null (decoderStack decoder)
              then Done newDecoder
              else NeedInput newDecoder
    _ -> Right (NeedInput decoder {decoderInput = mempty})
  Just (c, rest) -> parseStructuralChar c rest decoder
  where
    input = T.dropWhile isWhitespace (decoderInput decoder)

parseStructuralChar ::
  Char -> Text -> Decoder -> Either ParseError DecoderResult
parseStructuralChar c rest decoder = case decoderState decoder of
  ParserStateValue -> parseValueChar c rest decoder
  ParserStateObjectKey
    | c == '}' -> case decoderStack decoder of
        (_ : contexts) ->
          let newDecoder = decoder {decoderStack = contexts}
           in emitContainerEnd JSONEndObject rest newDecoder
        [] -> Left ExpectedObjectKey
    | c == '"' -> startString StringKey rest decoder
    | otherwise -> Left ExpectedObjectKey
  ParserStateObjectColon
    | c == ':' ->
        step decoder {decoderInput = rest, decoderState = ParserStateValue}
    | otherwise -> Left ExpectedColon
  ParserStateObjectComma
    | c == ',' ->
        step
          decoder {decoderInput = rest, decoderState = ParserStateObjectKey}
    | c == '}' ->
        case decoderStack decoder of
          (_ : contexts) ->
            let newDecoder = decoder {decoderStack = contexts}
             in emitContainerEnd JSONEndObject rest newDecoder
          [] -> Left ExpectedCommaOrEnd
    | otherwise -> Left ExpectedCommaOrEnd
  ParserStateArrayComma
    | c == ',' ->
        step decoder {decoderInput = rest, decoderState = ParserStateValue}
    | c == ']' ->
        case decoderStack decoder of
          (_ : contexts) ->
            emitContainerEnd JSONEndArray rest decoder {decoderStack = contexts}
          [] -> Left ExpectedCommaOrEnd
    | otherwise -> Left ExpectedCommaOrEnd
  ParserStateFinished -> Left TrailingInput
  ParserStateString _ -> Left (UnexpectedChar c)
  ParserStateNumber _ -> Left (UnexpectedChar c)
  ParserStateKeyword _ -> Left (UnexpectedChar c)

parseValueChar :: Char -> Text -> Decoder -> Either ParseError DecoderResult
parseValueChar c rest decoder = case decoderStack decoder of
  ContextArray : contexts
    | c == ']' ->
        emitContainerEnd JSONEndArray rest decoder {decoderStack = contexts}
  _ -> startValue c rest decoder

startValue :: Char -> Text -> Decoder -> Either ParseError DecoderResult
startValue c rest decoder = case c of
  '{' ->
    Right
      $ Emit
        JSONBeginObject
        decoder
          { decoderInput = rest,
            decoderStack = ContextObject : decoderStack decoder,
            decoderState = ParserStateObjectKey
          }
  '[' ->
    Right
      $ Emit
        JSONBeginArray
        decoder
          { decoderInput = rest,
            decoderStack = ContextArray : decoderStack decoder,
            decoderState = ParserStateValue
          }
  '"' -> startString StringValue rest decoder
  'n' -> startKeyword decoder rest "null" 1
  't' -> startKeyword decoder rest "true" 1
  'f' -> startKeyword decoder rest "false" 1
  _
    | isNumberStart c ->
        let numState = startNumberState c
         in stepNumber
              decoder {decoderInput = rest, decoderState = ParserStateNumber numState}
              numState
    | otherwise -> Left (UnexpectedChar c)

startString :: StringTarget -> Text -> Decoder -> Either ParseError DecoderResult
startString target input = consumeString target input emptyStringBuffer

consumeString ::
  StringTarget -> Text -> StringBuffer -> Decoder -> Either ParseError DecoderResult
consumeString target input buffer decoder = case T.uncons rest of
  Nothing ->
    let bufferedTarget = BufferedStringTarget target newBuffer
        parserString = ParserStateString (InString bufferedTarget)
        decoder' = decoder {decoderInput = mempty, decoderState = parserString}
     in Right $ NeedInput decoder'
  Just (c, rest')
    | c == '"' -> finishString target newBuffer rest' decoder
    | c == '\\' -> consumeStringEscape target rest' newBuffer decoder
    | otherwise -> Left (UnexpectedChar c)
  where
    (chunk, rest) = T.span (\c -> c /= '"' && c /= '\\' && ord c >= 0x20) input
    !newBuffer = appendStringBuffer chunk buffer

consumeStringEscape ::
  StringTarget -> Text -> StringBuffer -> Decoder -> Either ParseError DecoderResult
consumeStringEscape target input buffer decoder = case T.uncons input of
  Nothing ->
    let parserString =
          ParserStateString $ AfterEscape $ BufferedStringTarget target buffer
     in Right $ NeedInput decoder {decoderInput = mempty, decoderState = parserString}
  Just (c, rest) -> case c of
    '"' -> consumeString target rest (appendCharStringBuffer '"' buffer) decoder
    '\\' -> consumeString target rest (appendCharStringBuffer '\\' buffer) decoder
    '/' -> consumeString target rest (appendCharStringBuffer '/' buffer) decoder
    'b' -> consumeString target rest (appendCharStringBuffer '\b' buffer) decoder
    'f' -> consumeString target rest (appendCharStringBuffer '\f' buffer) decoder
    'n' -> consumeString target rest (appendCharStringBuffer '\n' buffer) decoder
    'r' -> consumeString target rest (appendCharStringBuffer '\r' buffer) decoder
    't' -> consumeString target rest (appendCharStringBuffer '\t' buffer) decoder
    'u' -> consumeUnicode target rest buffer decoder
    _ -> Left (InvalidEscape c)

consumeUnicode ::
  StringTarget -> Text -> StringBuffer -> Decoder -> Either ParseError DecoderResult
consumeUnicode target input buffer = consumeUnicode' target input buffer 0 0

consumeUnicode' ::
  StringTarget ->
  Text ->
  StringBuffer ->
  Int ->
  Int ->
  Decoder ->
  Either ParseError DecoderResult
consumeUnicode' target input buffer value digits decoder
  | newDigits >= 4 = finishUnicode target rest buffer newValue decoder
  | T.null rest =
      let unicode = Unicode newValue newDigits
          bufferedString = BufferedStringTarget target buffer
          parserString = ParserStateString $ InUnicodeEscape bufferedString unicode
          newDecoder = decoder {decoderInput = "", decoderState = parserString}
       in Right $ NeedInput newDecoder
  | otherwise = Left InvalidUnicodeEscape
  where
    needed = 4 - digits
    -- Take at most the digits still needed to complete the escape: a
    -- further hex digit belongs to the text following the escape.
    hex = T.take needed (T.takeWhile isHexDigit input)
    rest = T.drop (T.length hex) input
    newValue = T.foldl' (\v c -> v * 16 + digitToInt c) value hex
    newDigits = digits + T.length hex

finishUnicode ::
  StringTarget ->
  Text ->
  StringBuffer ->
  Int ->
  Decoder ->
  Either ParseError DecoderResult
finishUnicode target input buffer value decoder
  | isHighSurrogate value =
      let bufferedStringT = BufferedStringTarget target buffer
          parserString = ParserStateString $ AfterHighSurrogate bufferedStringT value
          newDecoder = decoder {decoderInput = input, decoderState = parserString}
       in Right $ NeedInput newDecoder
  | isLowSurrogate value = Left InvalidSurrogatePair
  | otherwise =
      consumeString target input (appendCharStringBuffer (chr value) buffer) decoder

stepString :: Decoder -> StringState -> Either ParseError DecoderResult
stepString decoder state =
  case state of
    InString (BufferedStringTarget target buffer) ->
      consumeString target (decoderInput decoder) buffer decoder
    AfterEscape (BufferedStringTarget target buffer) ->
      consumeStringEscape target (decoderInput decoder) buffer decoder
    InUnicodeEscape (BufferedStringTarget target buffer) (Unicode value digits) ->
      consumeUnicode' target (decoderInput decoder) buffer value digits decoder
    AfterHighSurrogate (BufferedStringTarget target buffer) high ->
      consumeLowSurrogate target (decoderInput decoder) buffer high decoder

consumeLowSurrogate ::
  StringTarget -> Text -> StringBuffer -> Int -> Decoder -> Either ParseError DecoderResult
consumeLowSurrogate target input buffer high decoder = case T.uncons input of
  Nothing -> Right $ NeedInput decoder {decoderInput = mempty}
  Just ('\\', rest) -> case T.uncons rest of
    Nothing ->
      Right
        $ NeedInput
          decoder
            { decoderInput = mempty,
              decoderState =
                ParserStateString
                  $ AfterHighSurrogate (BufferedStringTarget target buffer) high
            }
    Just ('u', rest') ->
      consumeLowSurrogateDigits target rest' buffer high 0 0 decoder
    _ -> Left InvalidSurrogatePair
  _ -> Left InvalidSurrogatePair

consumeLowSurrogateDigits ::
  StringTarget ->
  Text ->
  StringBuffer ->
  Int ->
  Int ->
  Int ->
  Decoder ->
  Either ParseError DecoderResult
consumeLowSurrogateDigits target input buffer high value digits decoder
  | digits == 4 =
      if isLowSurrogate value
        then
          let codepoint =
                0x10000
                  + ((high - 0xD800) `shiftL` 10)
                  + (value - 0xDC00)
              newBuffer = appendCharStringBuffer (chr codepoint) buffer
           in consumeString target input newBuffer decoder
        else Left InvalidSurrogatePair
  | otherwise =
      let remaining = 4 - digits
          limitedInput = T.take remaining input
          (hexDigits, _) = T.span isHexDigit limitedInput
          consumed = T.length hexDigits
          newValue = T.foldl' (\ac c -> ac * 16 + digitToInt c) value hexDigits
          newDigits = digits + consumed
          rest = T.drop consumed input
       in if newDigits == 4
            then
              if isLowSurrogate newValue
                then
                  let codepoint =
                        0x10000
                          + ((high - 0xD800) `shiftL` 10)
                          + (newValue - 0xDC00)
                      newBuffer = appendCharStringBuffer (chr codepoint) buffer
                   in consumeString target rest newBuffer decoder
                else Left InvalidSurrogatePair
            else
              if T.null rest
                then
                  Right
                    $ NeedInput
                      decoder
                        { decoderInput = mempty,
                          decoderState =
                            ParserStateString
                              $ InUnicodeEscape
                                (BufferedStringTarget target buffer)
                                (Unicode newValue newDigits)
                        }
                else
                  Left InvalidUnicodeEscape

finishString ::
  StringTarget ->
  StringBuffer ->
  Text ->
  Decoder ->
  Either ParseError DecoderResult
finishString target value remaining decoder = case target of
  StringValue ->
    emitScalar (JSONString (finishStringBuffer value)) remaining decoder
  StringKey -> do
    let key = JSONObjectKey (finishStringBuffer value)
    let newDecoder =
          decoder
            { decoderInput = remaining,
              decoderState = ParserStateObjectColon
            }
    Right $ Emit key newDecoder

-- | Continue parsing a number from the saved state.
stepNumber :: Decoder -> NumberState -> Either ParseError DecoderResult
stepNumber decoder numState = case T.uncons (decoderInput decoder) of
  Nothing ->
    Right
      $ NeedInput
        decoder
          { decoderInput = mempty,
            decoderState = ParserStateNumber numState
          }
  Just (c, rest) -> case advanceNumber (numberPhase numState) c of
    NumEnd ->
      finalizeNumber
        decoder
          { decoderInput = T.cons c rest,
            decoderState = ParserStateNumber numState
          }
    NumError -> Left (InvalidNumber (reversedStringToText (numberBuffer numState) <> one c))
    NumStep nextPhase ->
      let numState' =
            numState
              { numberBuffer = ReversedString $ c : unReversedString (numberBuffer numState),
                numberPhase = nextPhase
              }
       in stepNumber decoder {decoderInput = rest} numState'

finalizeNumber :: Decoder -> Either ParseError DecoderResult
finalizeNumber decoder = case decoderState decoder of
  ParserStateNumber numState
    | isValidNumberFinal (numberPhase numState) ->
        let value = parseNumberBuffer (reversedStringToText $ numberBuffer numState)
         in emitScalar
              (JSONNumber value)
              (decoderInput decoder)
              decoder
    | otherwise -> Left (InvalidNumber (reversedStringToText $ numberBuffer numState))
  _ -> Left (InvalidNumber mempty)

-- | Parse the accumulated number buffer into a Scientific value.
-- We build it manually to stay within the JSON grammar.
--
-- The buffer is scanned once: every digit accumulates into a single
-- coefficient while post-dot digits are counted, so no @10 ^ n@
-- bignum power is needed. A leading @-@ sets only the overall sign;
-- @-@/@+@ later in the buffer is an exponent sign.
parseNumberBuffer :: Text -> Scientific
parseNumberBuffer buf = go (0 :: Integer) (0 :: Integer) (0 :: Int) pos0 False False False
  where
    len = T.length buf
    (negative, pos0)
      | not (T.null buf) && T.index buf 0 == '-' = (True, 1)
      | otherwise = (False, 0)
    go :: Integer -> Integer -> Int -> Int -> Bool -> Bool -> Bool -> Scientific
    go !coeff !expP !nFrac !pos !hasDot !hasExp !expNeg
      | pos >= len =
          let expVal = (if expNeg then negate else identity) expP
              finalExp = fromInteger (expVal - fromIntegral nFrac :: Integer) :: Int
           in (if negative then negate else identity) coeff `scientific` finalExp
      | otherwise =
          let c = T.index buf pos
           in case c of
                '-' -> go coeff expP nFrac (pos + 1) hasDot hasExp True
                '+' -> go coeff expP nFrac (pos + 1) hasDot hasExp False
                '.' -> go coeff expP nFrac (pos + 1) True hasExp expNeg
                'e' -> go coeff expP nFrac (pos + 1) hasDot True expNeg
                'E' -> go coeff expP nFrac (pos + 1) hasDot True expNeg
                _
                  | isDigit c ->
                      let d = fromIntegral (digitToInt c) :: Integer
                       in if hasExp
                            then go coeff (expP * 10 + d) nFrac (pos + 1) hasDot True expNeg
                            else
                              if hasDot
                                then go (coeff * 10 + d) expP (nFrac + 1) (pos + 1) True False expNeg
                                else go (coeff * 10 + d) expP nFrac (pos + 1) False False expNeg
                  | otherwise -> go coeff expP nFrac (pos + 1) hasDot hasExp expNeg

startKeyword :: Decoder -> Text -> Text -> Int -> Either ParseError DecoderResult
startKeyword decoder input keyword consumed =
  let state = keywordState keyword consumed
   in stepKeyword
        decoder {decoderInput = input, decoderState = ParserStateKeyword state}
        state

keywordState :: Text -> Int -> KeywordState
keywordState keyword n
  | keyword == "null" = KeywordNull n
  | keyword == "true" = KeywordTrue n
  | keyword == "false" = KeywordFalse n
  | otherwise = KeywordNull n

stepKeyword :: Decoder -> KeywordState -> Either ParseError DecoderResult
stepKeyword decoder state =
  let (keyword, index) = case state of
        KeywordNull i -> ("null", i)
        KeywordTrue i -> ("true", i)
        KeywordFalse i -> ("false", i)
   in if index == T.length keyword
        then case T.uncons (decoderInput decoder) of
          Nothing -> finalizeKeyword decoder
          Just (c, _) | isJsonDelimiter c -> finalizeKeyword decoder
          Just _ -> Left (InvalidKeyword keyword)
        else case T.uncons (decoderInput decoder) of
          Nothing -> Right $ NeedInput decoder {decoderState = ParserStateKeyword state}
          Just (c, rest)
            | c == T.index keyword index ->
                stepKeyword
                  decoder
                    { decoderInput = rest,
                      decoderState = ParserStateKeyword (advanceKeyword state)
                    }
                  (advanceKeyword state)
          _ -> Left (InvalidKeyword keyword)

advanceKeyword :: KeywordState -> KeywordState
advanceKeyword state = case state of
  KeywordNull n -> KeywordNull (n + 1)
  KeywordTrue n -> KeywordTrue (n + 1)
  KeywordFalse n -> KeywordFalse (n + 1)

finalizeKeyword :: Decoder -> Either ParseError DecoderResult
finalizeKeyword decoder = case decoderState decoder of
  ParserStateKeyword state ->
    let (keyword, index) = case state of
          KeywordNull i -> ("null", i)
          KeywordTrue i -> ("true", i)
          KeywordFalse i -> ("false", i)
     in if index == T.length keyword
          then
            let event = case state of
                  KeywordNull _ -> JSONNull
                  KeywordTrue _ -> JSONBool True
                  KeywordFalse _ -> JSONBool False
             in emitScalar
                  event
                  (decoderInput decoder)
                  decoder
          else Left (InvalidKeyword keyword)
  _ -> Left (InvalidKeyword mempty)

emitScalar :: JSONEvent -> Text -> Decoder -> Either ParseError DecoderResult
emitScalar event remaining decoder =
  let newDec = decoder {decoderInput = remaining}
   in Right $ Emit event $ finishValue newDec

-- | A JSON value has just been completed.
-- Determine what the enclosing context expects next.
finishValue :: Decoder -> Decoder
finishValue decoder = case decoderStack decoder of
  [] -> decoder {decoderState = ParserStateFinished}
  ContextArray : _ ->
    decoder {decoderState = ParserStateArrayComma}
  ContextObject : _ ->
    decoder {decoderState = ParserStateObjectComma}

emitContainerEnd :: JSONEvent -> Text -> Decoder -> Either ParseError DecoderResult
emitContainerEnd event remaining decoder =
  let decoder' = decoder {decoderInput = remaining}
   in Right $ Emit event (finishValue decoder')

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\t', '\n', '\r']

isNumberStart :: Char -> Bool
isNumberStart c = c == '-' || isDigit c

isHighSurrogate :: Int -> Bool
isHighSurrogate x = x >= 0xD800 && x <= 0xDBFF

isLowSurrogate :: Int -> Bool
isLowSurrogate x = x >= 0xDC00 && x <= 0xDFFF

-- | The root value is complete: empty stack + finished state.
isRootDone :: Decoder -> Bool
isRootDone decoder =
  decoderState decoder == ParserStateFinished && null (decoderStack decoder)

decode ::
  (Monad m) =>
  Stream (Of Text) m r -> Stream (Of JSONEvent) m (Either ParseError r)
decode = runDecoder initialDecoder

runDecoder ::
  (Monad m) =>
  Decoder ->
  Stream (Of Text) m r ->
  Stream (Of JSONEvent) m (Either ParseError r)
runDecoder decoder input = do
  result <- lift $ S.next input
  case result of
    Left r
      | isRootDone decoder || decoder == initialDecoder ->
          pure (Right r)
      | otherwise -> drainFinish decoder r
    Right (chunk, rest) -> case feed chunk decoder of
      Left err -> pure (Left err)
      Right dr -> drain dr rest

drain ::
  (Monad m) =>
  DecoderResult ->
  Stream (Of Text) m r ->
  Stream (Of JSONEvent) m (Either ParseError r)
drain (Emit event nextDecoder) rest =
  S.yield event >> drainStep (step nextDecoder) rest
drain (NeedInput nextDecoder) rest
  | isRootDone nextDecoder =
      drainDone nextDecoder rest
  | otherwise = runDecoder nextDecoder rest
drain (Done nextDecoder) rest = drainDone nextDecoder rest

drainStep ::
  (Monad m) =>
  Either ParseError DecoderResult ->
  Stream (Of Text) m r ->
  Stream (Of JSONEvent) m (Either ParseError r)
drainStep (Left err) _ = pure (Left err)
drainStep (Right result) rest = drain result rest

drainDone ::
  (Monad m) =>
  Decoder ->
  Stream (Of Text) m r ->
  Stream (Of JSONEvent) m (Either ParseError r)
drainDone nextDecoder rest = do
  more <- lift $ S.next rest
  case more of
    Left r -> drainFinish nextDecoder r
    Right (chunk, rest')
      | isRootDone nextDecoder ->
          if T.all isWhitespace chunk
            then drainDone nextDecoder rest'
            else pure (Left TrailingInput)
      | otherwise -> pure (Left TrailingInput)

drainFinish ::
  (Monad m) => Decoder -> r -> Stream (Of JSONEvent) m (Either ParseError r)
drainFinish decoder r = case finish decoder of
  Left UnexpectedEnd
    | getAll
        $ foldMap
          All
          [ decoderState decoder == ParserStateValue,
            null $ decoderStack decoder
          ] ->
        pure (Right r)
  Left err -> pure (Left err)
  Right (Done _) -> pure (Right r)
  Right (NeedInput _) -> pure (Left UnexpectedEnd)
  Right (Emit event nextDecoder) -> S.yield event >> drainFinish nextDecoder r

type StreamIO s = Stream (Of s) (ExceptT Text IO)

decodeIO :: StreamIO Text () -> StreamIO JSONEvent ()
decodeIO input = do
  result <- decode input
  case result of
    Left err -> throwError (show err)
    Right () -> pure ()
