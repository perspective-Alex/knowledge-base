module KnBase.Statistics where

import Data.List (isInfixOf)

import KnBase.Text
import KnBase.Structures

sentencesWithKeyword :: String -> SentText -> SentText 
sentencesWithKeyword kw txt = filter (containKeyword kw) txt
  where
    containKeyword :: String -> Sentence -> Bool 
    containKeyword str sent =
      isInfixOf (words str) (words sent)
