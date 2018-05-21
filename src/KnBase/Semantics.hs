module KnBase.Semantics where

import Data.List (isInfixOf)
import Text.Show.Unicode
import qualified Data.HashMap.Strict as HMap

import KnBase.Text
import KnBase.Structures
import KnBase.Keywords

sentencesWithKeyword :: Dictionary -> String -> SentText -> SentText 
sentencesWithKeyword dict kw txt = filter (containKeyword lemmedKW) txt
  where
    lemmedKW = lemmOrigContent dict kw
    containKeyword :: Text -> Sentence -> Bool 
    containKeyword txt sent =
      isInfixOf txt (lemmOrigContent dict sent)

printOntology :: Keywords -> String -> Dictionary -> FilePath -> IO ()
printOntology kwds txt dict outFilePath = do
  let
    { owKwds = map (fst) $ oneWord kwds
    ; allSentences = initSentText txt
    ; ontology = getKeywordContext dict owKwds allSentences HMap.empty
    }
  uprint $ length allSentences
  mapM_ (printOntElem) (HMap.toList ontology)
