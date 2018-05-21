module KnBase where

import KnBase.Structures
import KnBase.Semantics
import KnBase.Keywords
import KnBase.Text

import Data.Text.Encoding
import Data.Time.Clock.System
import Text.Show.Unicode
import Data.List ( intersectBy, unionBy )
import qualified Data.HashMap.Strict as HMap
import qualified Data.ByteString.Char8 as DBC
import qualified Data.Text as T
import System.IO
import System.Environment

defMain :: IO ()
defMain = do
  time1 <- getSystemTime
  outfp:files <- getArgs
  writeFile outfp ""
  contents <- readMorphandDataFiles files
  let 
    { texts = map (preProcess) $ take (length contents `div` 2) contents
    ; morphs = drop (length contents `div` 2) contents
    ; dicts     = map initDictionary morphs 

    ; keywordsAndIcounts  = zipWith (getKeywords) texts dicts
    ; keywords = map fst keywordsAndIcounts
    }
  printKeywordsAndIcounts keywordsAndIcounts
  let
    { kIntersection = foldKeywords intersectBy keywords
    ; kUnion = foldKeywords unionBy keywords
    }
  printKeywordsToSpecFile "./resources/Tomita/keywords.txt" kIntersection 
  printOntology kUnion (concat texts)
       (foldr1 HMap.union dicts) outfp
  time3 <- getSystemTime
  putStr "Time of work = "
  print $ systemSeconds time3 - systemSeconds time1

readMorphandDataFiles :: [FilePath] -> IO [String]
readMorphandDataFiles files = do
  contents <- mapM (DBC.readFile) files
  return $ map (T.unpack.decodeUtf8) contents
