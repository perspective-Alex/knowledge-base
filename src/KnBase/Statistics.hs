module KnBase.Statistics where

import Data.List (isInfixOf)
import KnBase.Text
import KnBase.Structures

sentencesWithKeyword :: String -> SentText -> SentText 
sentencesWithKeyword kw txt = filter (isInfixOf kw) txt
