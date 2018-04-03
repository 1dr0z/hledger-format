module Format
  ( module Hledger
  , indent
  , showTransaction
  , showDescriptionLine
  , showPosting
  ) where


import Data.Text (Text)
import qualified Data.Text as Text
import Data.String.Conversions (cs)
import Options (Options(..))

import Hledger hiding (showPosting, showTransaction)


-- Format a transaction
showTransaction :: Options -> Transaction -> String
showTransaction opts tx = unlines $
  [ showDescriptionLine tx ++ samelinecomment ]
  ++ newlinecomments
  ++ showPostings (tpostings tx)
  where
    -- Show all postings with special treatment for the last one
    showPostings []     = []
    showPostings [p]    = [showPosting opts (showLastAmount opts) p]
    showPostings (p:ps) = showPosting opts True p : showPostings ps

    (samelinecomment, newlinecomments) =
      parseCommentLines (tcomment tx)

-- Format the description line of a transaction
showDescriptionLine :: Transaction -> String
showDescriptionLine tx = rstrip $ concat
  [ showDate (tdate tx)
  , showAuxDate (tdate2 tx)
  , showStatus (tstatus tx)
  , showCode (tcode tx)
  , showDescription (tdescription tx)
  ]

  where
    -- Format the auxillary date
    showAuxDate =
      maybe "" (("=" ++) . showDate)

    -- Format the status
    showStatus Cleared  = " *"
    showStatus Pending  = " !"
    showStatus Unmarked = ""

    -- Format the code
    showCode code
      | Text.null code = ""
      | otherwise      = " (" ++ cs code ++ ")"

    -- Format description
    showDescription desc
      | Text.null desc = ""
      | otherwise      = " " ++ cs desc

-- Parse multiline comment string
-- Distinguish between same line and new line comments
parseCommentLines :: Text -> (String, [String])
parseCommentLines t =
  case commentlines t of
    []   -> ("", [])
    x:xs -> (x, xs)
  where
    commentprefix :: String -> String
    commentprefix =  indent 4 . ("; " ++)

    commentlines :: Text -> [String]
    commentlines t =
      case lines $ cs t of
        ("":ls) -> "" : map commentprefix ls
        ls      -> map commentprefix ls

-- Format a single posting
showPosting :: Options -> Bool -> Posting -> String
showPosting opts showAmt pt = indent 4 $ rstrip $
  unlines $
    concat
      [ statusAndAccount
      , amount
      , assertion
      , samelinecomment
      ]
    : newlinecomments
  where
    -- Format the posting status if there is one
    status = case pstatus pt of
      Pending  -> "! "
      Cleared  -> "* "
      Unmarked -> ""

    -- Format the account name based on the posting type
    account = showAccountName Nothing (ptype pt) (paccount pt)

    -- Format the mixed currency amount
    amount =
      if showAmt
         then showMixedAmountOneLine (pamount pt)
         else ""  -- Hide the amount

    -- Ensure at least two spaces between account and amount
    padding = max (2 + length (status ++ account)) (amountColumn opts - length amount)
    statusAndAccount = fitString (Just padding) Nothing False True $ status ++ account

    -- Format balance assertion
    assertion = maybe "" ((" = " ++) . showAmountWithZeroCommodity . fst) $ pbalanceassertion pt

    -- Parse multiline comment string
    (samelinecomment, newlinecomments) = parseCommentLines (pcomment pt)


indent :: Int -> String -> String
indent n s = replicate n ' ' ++ s
