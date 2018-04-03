module Options
  ( Options(..)
  , Sort(..)
  , getOptions
  ) where

import Control.Monad (liftM)
import Data.Monoid ((<>))
import Options.Applicative
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.FilePath((</>), isRelative)


data Sort
  = Date
  | Date2
  deriving (Eq, Show)

-- Parse a string for sort type
parseSort :: ReadM Sort
parseSort = eitherReader parse
  where
    parse "date"  = Right Date
    parse "date2" = Right Date2
    parse option  = Left $ "Invalid sort option: " <> option

-- The different command-line options
data Options = Options
  { file           :: String
  , amountColumn   :: Int
  , sort           :: Maybe Sort
  , showLastAmount :: Bool
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
    <*> showLastAmountParser

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
sortParser :: Parser (Maybe Sort)
sortParser =
  optional $ option parseSort
    (  long "sort"
    <> short 's'
    <> help "Whether to sort transactions by date" )

showLastAmountParser :: Parser Bool
showLastAmountParser =
  flag False True
    (  long "show-last-amount"
    <> help "Whether to show the amount for the last posting" )

