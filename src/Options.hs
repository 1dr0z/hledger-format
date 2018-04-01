module Options
  ( Options(..)
  , getOptions
  ) where

import Control.Monad (liftM)
import Data.Monoid ((<>))
import Options.Applicative
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.FilePath((</>), isRelative)


-- The different command-line options
data Options = Options
  { file         :: String
  , amountColumn :: Int
  , sort         :: Bool
  } deriving (Eq, Show)

-- Get the command-line options for the current program
getOptions :: IO Options
getOptions = do
  currDir <- getCurrentDirectory
  options <- execParser optionsInfo
  newPath <- expandPath currDir (file options)
  pure $ options { file = newPath }

-- Normalize path to be absolute
expandPath :: FilePath -> FilePath -> IO FilePath
expandPath _ "-" = pure "-"
expandPath curdir p = (if isRelative p then (curdir </>) else id) `liftM` expandPath' p
  where
    expandPath' ('~':'/':p)  = (</> p) <$> getHomeDirectory
    expandPath' ('~':'\\':p) = (</> p) <$> getHomeDirectory
    expandPath' ('~':_)      = ioError $ userError "~USERNAME in paths is not supported"
    expandPath' p            = pure p

-- Options info
optionsInfo :: ParserInfo Options
optionsInfo = info (optionsParser <**> helper)
  (  fullDesc
  <> progDesc "Format a ledger file" )

-- Parser for options object
optionsParser :: Parser Options
optionsParser =
  Options
    <$> fileParser
    <*> amountColumnParser
    <*> sortParser

-- Parser for file option
fileParser :: Parser String
fileParser =
  strOption
    (  long "file"
    <> short 'f'
    <> metavar "FILE"
    <> help "Use - for stdin" )

-- Parser for align amount option
amountColumnParser :: Parser Int
amountColumnParser =
  option auto
    (  long "amount-column"
    <> short 'c'
    <> metavar "INT"
    <> value 75
    <> showDefault
    <> help "Align amount at given column" )

-- Parser for sort option
sortParser :: Parser Bool
sortParser =
  switch
    (  long "sort"
    <> short 's'
    <> help "Whether to sort transactions by date" )

