module HQ.JSON.Decoder.StringBuffer where

import qualified Data.Text as T
import Relude

newtype StringBuffer = StringBuffer [Text] deriving (Eq, Show)

emptyStringBuffer :: StringBuffer
emptyStringBuffer = StringBuffer []

appendStringBuffer :: Text -> StringBuffer -> StringBuffer
appendStringBuffer t (StringBuffer ts)
  | T.null t = StringBuffer ts
  | otherwise = StringBuffer (t : ts)

appendCharStringBuffer :: Char -> StringBuffer -> StringBuffer
appendCharStringBuffer c = appendStringBuffer (T.singleton c)

finishStringBuffer :: StringBuffer -> Text
finishStringBuffer (StringBuffer ts) = T.concat (reverse ts)
