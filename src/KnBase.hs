module KnBase where

import KnBase.Structures
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
  fp:mfp:_ <- getArgs
  mf <- DBC.readFile mfp 
  f <- DBC.readFile fp
  let 
    { mfData    = T.unpack(decodeUtf8 mf)
    ; origTxt   = T.unpack(decodeUtf8 f)
    ; txt       = changeExtraSymb origTxt
    ; allWords  = words txt
    ; dict      = initDict allWords mfData
    ; graph     = buildGraph dict mfData allWords 
    }
  let
    { resGraph = fst $ loopComputations threshold (graph,0)
    ; iterCount = snd $ loopComputations threshold (graph,0)
    ; resVertices = gVertices resGraph
    ; proportion = truncate (fromIntegral (length resVertices) / 5) 
    ; owCandidates = take proportion $ sortByScore resVertices
    ; reqEdges     = filter (isOneWordCand owCandidates) (gEdges resGraph)  
    ; dwCandidates = take proportion $ sortByScore
                        (multiCand txt (dWordSeq dict reqEdges))
    ; twCandidates = take proportion $ sortByScore
                        (multiCand txt (tWordSeq dict reqEdges))
    }
  time1 <- getSystemTime
  --mapM_ uprint $ listCoOccur (gVertices graph) mfData allWords
  --uprint $ length (gEdges graph)
  --mapM_ uprint $ alData dict 
  putStrLn "One-Word-Candidates:" 
  mapM_ uprint owCandidates 
  putStrLn "\nDouble-Word-Candidates:" 
  mapM_ uprint dwCandidates 
  {-
  putStrLn "\nTriple-Word-Candidates:" 
  mapM_ uprint twCandidates 
  -}
  time3 <- getSystemTime
  print $ systemSeconds time3 - systemSeconds time1
  putStrLn $ "iterCount = " ++ show(iterCount) ++ "\n"


postKeyw :: IO ()
postKeyw = do
  fp:mfp:_ <- getArgs
  mf <- DBC.readFile mfp 
  f <- DBC.readFile fp
  let 
    { mfData   = T.unpack(decodeUtf8 mf)
    ; txt   = T.unpack(decodeUtf8 f)
    ; lemmedTxt = lemmOrigContent mfData txt
    }
  mapM_ uprint lemmedTxt
