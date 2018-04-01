{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy (hGetContents)
import Data.List (sortBy)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import System.IO (stdin)
-- import Prelude hiding (hGetContents, putStrLn)

import Format (showTransaction)
import Hledger (Journal(..), Transaction(..), readJournalFile)
import Options (Options(..), getOptions)
import Debug.Trace


main :: IO ()
main =
  runWithOpts =<< getOptions

runWithOpts :: Options -> IO ()
runWithOpts opts = do
  journal <- journalFile $ file opts
  let txs = sortTx opts (jtxns journal)
      out = showTransaction (amountColumn opts) <$> txs
  putStrLn $ unlines out

-- Read a journal file or stdin if the file is '-'
journalFile :: FilePath -> IO Journal
journalFile path =
  either fail pure =<<
    readJournalFile (Just "journal") Nothing False path

-- Sort transactions
sortTx :: Options -> [Transaction] -> [Transaction]
sortTx opts txs =
  if sort opts
     then sortBy dates txs
     else txs
  where dates t1 t2 = compare (tdate t1) (tdate t2)

