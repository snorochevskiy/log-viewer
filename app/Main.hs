{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as TE

import qualified Data.List as L
import Data.Conduit
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Monoid

import Web.Scotty
import Network.HTTP.Types

-- |Conduit that processes log lines as log entries.
-- Log entry starts with a timestamp and can have trailing lines
-- that start from any symbol except timestamp.
-- If line starts with timestamp and contains given search string, then is passed by.
-- If line doesnt start from timestamp (just a next line of a log entry) and previouls line
-- relates to an entry that contains search string , then it is also passed by.
-- 'found' indicates if previouls line relates to a log entry that contains search string
-- 'searchString' is a search string
logBlocks :: (Monad m, MonadIO m) => Bool -> String -> Conduit BS.ByteString m BS.ByteString
logBlocks found searchString = do
  client <- await
  case client of
    Nothing -> return ()
    Just c -> do
      let unpaked = BS.unpack c
      if match found unpaked searchString
        then do
          yield $ c <> BS.pack "\n"
          logBlocks True searchString
        else
          logBlocks False searchString


-- |Checks  if given line should be included into resulting log
match :: Bool -> String -> String -> Bool
match found line searchingStr = (found && isNewLogEntry line) || L.isInfixOf searchingStr line
    where
      isNewLogEntry :: String -> Bool
      isNewLogEntry line' =  (L.length line' > 0) && ((L.head line' < '0') || (L.head line' > '9'))

findLogsWithIncident :: (MonadResource m, MonadIO m) => FilePath -> String -> m BS.ByteString
findLogsWithIncident logFile searchString =
  (C.sourceFile logFile)
    $= C.lines
    $= (logBlocks False (searchString))
    $$ CC.fold

main :: IO ()
main = do
  params@(port:logFile:[]) <- getArgs
  scotty (read port) $ do
    get "/incident/:incidentId" $ do
      (incidentId :: String) <- param "incidentId"
      resp <- liftIO $ runResourceT $ findLogsWithIncident logFile ("incident:" ++ incidentId)
      html $ mconcat ["<h1>Logs with incidentId:", T.pack incidentId, "</h1>\n\n", "<pre>", T.fromStrict $ TE.decodeUtf8 resp, "</pre>"]

    notFound $ do
      status notFound404
      html "<h1>Not such page</h1>"
