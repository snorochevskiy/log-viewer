{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Data.Conduit
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Binary as C
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Monoid

logBlocks :: (Monad m, MonadIO m) => Bool -> String -> Conduit BS.ByteString m BS.ByteString
logBlocks found srch = do
  client <- await
  case client of
    Nothing -> return ()
    Just c -> do
      let unpaked = BS.unpack c
      if match found unpaked srch
        then do
          yield $ c <> BS.pack "\n"
          logBlocks True srch
        else
          logBlocks False srch


-- Check if starts with timestamp and contains given substring
match :: Bool -> String -> String -> Bool
match found str srch = (found && (length str > 0) && ((head str < '0') || (head str > '9'))) || isInfixOf srch str

main :: IO ()
main = do
  let source = C.sourceFile "/home/stas/Dropbox/Dropbox/workflow.log"
      sink   = C.sinkFile   "dst.txt"
  runResourceT $ source $= C.lines $= (logBlocks False "Found cycle for field 'market' in type 'Site' for path 'site'") $$ sink
