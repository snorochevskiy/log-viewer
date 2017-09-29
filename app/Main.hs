{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as TE

import qualified Data.List as L
import Conduit
import Data.Conduit
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.Combinators as CC
import Control.Monad (unless, when)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Monoid

import Web.Scotty
import Network.HTTP.Types

logFiltrationConduit :: MonadResource m => FilePath -> String -> ConduitM a c m BS.ByteString
logFiltrationConduit logFile searchString =
  C.sourceFile logFile
  .| splitToLogEntries
  .| filerLogBlocks searchString
  .| CC.fold

filerLogBlocks :: (Monad m, MonadIO m) => String -> Conduit BS.ByteString m BS.ByteString
filerLogBlocks searchString = do
  block <- await
  case block of
    Just txt ->
      if BS.isInfixOf (BS.pack searchString) txt
        then do
          yield txt
          filerLogBlocks searchString
        else filerLogBlocks searchString
    Nothing -> return ()

-- altered "lines" function from conduit-extra
splitToLogEntries :: Monad m => Conduit BS.ByteString m BS.ByteString
splitToLogEntries =
    loop []
  where
    loop acc = await >>= maybe (finish acc) (go acc)

    finish acc =
        let final = BS.concat $ reverse acc
         in unless (BS.null final) (yield final)

    go acc more =
        case BS.uncons second of
            Just (_, second') -> yield (BS.concat $ reverse $ (first:acc ++ [BS.pack "\n"]) ) >> go [] second'
            Nothing -> loop $ more:acc
      where
        (first, second) = takeBlock more

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

isNewLogLine :: BS.ByteString -> Bool
isNewLogLine bs =
  case BS.uncons bs of
    Just (first, rest) -> (first >= '0' && first <= '9')
    Nothing -> False

main :: IO ()
main = do
  params@(port:logFile:[]) <- getArgs
  scotty (read port) $ do
    get "/incident/:incidentId" $ do
      (incidentId :: String) <- param "incidentId"
      resp <- liftIO $ runConduitRes $ logFiltrationConduit logFile ("incident:" ++ incidentId)
      html $ mconcat ["<h1>Logs with incidentId:", T.pack incidentId, "</h1>\n\n", "<pre>", T.fromStrict $ TE.decodeUtf8 resp, "</pre>"]

    notFound $ do
      status notFound404
      html "<h1>Not such page</h1>"
