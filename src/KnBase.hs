{-# LANGUAGE OverloadedStrings #-}

module KnBase where

import KnBase.Structures
import KnBase.Statistics
import KnBase.Keywords
import KnBase.Text

import Data.Text.Encoding
import Data.Time.Clock.System
import Text.Show.Unicode
import qualified Data.ByteString.Char8 as DBC
import qualified Data.Text as T
import System.IO
import System.Environment

defMain :: IO ()
defMain = do
  time1 <- getSystemTime
  fp:mfp:outfp:_ <- getArgs
  mf <- DBC.readFile mfp 
  f <- DBC.readFile fp
  writeFile outfp ""
  let 
    { mfData    = T.unpack(decodeUtf8 mf)
    ; origTxt   = surroundDelimeters (T.unpack(decodeUtf8 f))
    }
  keywords <- getKeywords origTxt mfData
  postKeyw keywords origTxt outfp
  time3 <- getSystemTime
  print $ systemSeconds time3 - systemSeconds time1

getKeywords :: String -> String -> IO (Keywords)
getKeywords origTxt mfData = do
  let 
    { txt       = changeExtraSymb origTxt
    ; allWords  = words txt
    ; dict      = initDict allWords mfData
    ; graph     = buildGraph dict mfData allWords 
    }
  let
    { algRes = loopComputations threshold (graph,0)
    ; resGraph = fst algRes
    ; iterCount = snd algRes
    ; resVertices = gVertices resGraph
    ; proportion = truncate (fromIntegral (length resVertices) / 3) 
    ; owCandidates = take 30 $ sortByScore resVertices
    ; reqEdges     = filter (isOneWordCand owCandidates) (gEdges resGraph)  
    ; dwCandidates = sortByScore
                        (multiCand txt (dWordSeq dict reqEdges))
    ; twCandidates = sortByScore
                        (multiCand txt (tWordSeq dict reqEdges))
    ; keywords = Kwds owCandidates dwCandidates
    }
  --mapM_ uprint $ listCoOccur (gVertices graph) mfData allWords
  --uprint $ length (gEdges graph)
  --mapM_ uprint $ alData dict 
  printKeywords keywords
  {-
  putStrLn "One-Word-Candidates:" 
  mapM_ uprint owCandidates 
  putStrLn "\nDouble-Word-Candidates:" 
  mapM_ uprint dwCandidates 
  putStrLn "\nTriple-Word-Candidates:" 
  mapM_ uprint twCandidates 
  -}
  putStrLn $ "iterCount = " ++ show(iterCount) ++ "\n"
  return (keywords)


postKeyw :: Keywords -> String -> FilePath -> IO ()
postKeyw kwds txt outFilePath = do
  let
    { owKwds = oneWord kwds
    ; dwKwds = twoWords kwds
    ; sentences = initSentText txt
    }
  writeKeywords outFilePath owKwds sentences
  writeKeywords outFilePath dwKwds sentences

writeKeywords :: FilePath -> [Vertice] -> SentText -> IO ()
writeKeywords _ [] _ = return ()
writeKeywords fp (v:vs) sentences = do
  appendFile fp $ "\n\n" ++ ushow v ++ "\n\n"
  appendFile fp $ ushow (sentencesWithKeyword (fst v) sentences)
  writeKeywords fp vs sentences
