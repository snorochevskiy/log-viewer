module Slf4jLogs
  (
    takeBlock
  , isNewLogLine
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Monoid

-- |Reads log entry from ByteString and return the log entry and the rest part of string
takeBlock :: BS.ByteString -> (BS.ByteString, BS.ByteString)
takeBlock bs = takeFirestLine bs
  where
    takeFirestLine bs' =
      let
        (line, rest) = BS.breakSubstring (BS.pack "\n") bs'
        (nextLines, nextRest) = takeNextLine rest
      in (line <> nextLines, nextRest)
    takeNextLine bs' =
      case BS.uncons bs' of
        Just (newLineChar, lTrimmed) ->
          if isNewLogLine lTrimmed
            then
              (BS.pack "", bs')
            else
              let
                (line, rest) = BS.breakSubstring (BS.pack "\n") lTrimmed
                (nextLines, nextRest) = takeNextLine rest
              in (BS.pack "\n" <> line <> nextLines, nextRest)
        Nothing -> (BS.pack "", BS.pack "")

-- |Checks if given line is a start of another log entry or a part of the previous log entry
isNewLogLine :: BS.ByteString -> Bool
isNewLogLine bs =
  case BS.uncons bs of
    Just (first, rest) -> (first >= '0' && first <= '9')
    Nothing -> False
