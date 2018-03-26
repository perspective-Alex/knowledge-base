module KnBase where

import KnBase.Types
import KnBase.Process
import Data.Text.Encoding
import Data.Time.Clock.System
import Text.Show.Unicode
import qualified Data.ByteString.Char8 as DBC
import qualified Data.Text as T
import qualified Data.List as L
import System.IO
import System.Environment

defMain :: IO ()
defMain = do
  files <- getArgs
  let 
    { morphFilePath = head $ tail files
    ; filePath = head files
    }
  mf <- DBC.readFile morphFilePath
  f <- DBC.readFile filePath 
  let 
    { mfData    = T.unpack(decodeUtf8 mf)
    ; origTxt   = T.unpack(decodeUtf8 f)
    ; txt       = changeExtraSymb origTxt
    ; wordsInfo = getWordProp (words txt) mfData
    ; wLemm     = map (\x -> getWordLemma x mfData)
    ; rdTxt     = txt --unwords $ wLemm (words txt) 
    ; vertices  = buildVertices mfData (words txt)
    ; edges     = buildEdges mfData (words txt)
    ; graph     = Graph vertices edges
    }
 -- print (getPartOfSpeech "и" mfData)
  let
    { resGraph = fst $ loopComputations threshold (graph,0)
    ; iterCount = snd $ loopComputations threshold (graph,0)
    ; resVertices = gVertices resGraph
    ; proportion = truncate (fromIntegral (length resVertices) / 3) 
    ; owCandidates = take proportion $ sortByScore resVertices
    ; dwCandidates = (mCand rdTxt (dWordSeq mfData owCandidates))
    ; twCandidates = L.nub (mCand rdTxt (tWordSeq mfData
                                            dwCandidates owCandidates))
    }
  time1 <- getSystemTime
  putStrLn "Final Graph:" 
  uprint resGraph 
  time2 <- getSystemTime
  print (systemSeconds time2 - systemSeconds time1)
  putStrLn "One-Word-Candidates:" 
  mapM_ uprint owCandidates 
  putStrLn "\nDouble-Word-Candidates:" 
  mapM_ uprint dwCandidates 
  putStrLn "\nTriple-Word-Candidates:" 
  mapM_ uprint twCandidates 
  time3 <- getSystemTime
  print $ systemSeconds time3 - systemSeconds time1
  putStrLn $ "iterCount = " ++ show(iterCount) ++ "\n"
--  putStrLn txt

-- iterate computations until score-diff < threshold and get iter_count
loopComputations :: Float -> (Graph,Int) -> (Graph,Int)
loopComputations eps (g@(Graph vv ee), n) = 
  case (scoreDiff vv (gVertices newg)) < threshold of
    True  -> (g,n)
    False -> loopComputations eps (newg, n+1) 
  where
    scoreDiff :: [Vertice] -> [Vertice] -> Float
    scoreDiff vv1 vv2 = 
        foldr1 max (zipWith (\ v1 v2 -> abs(snd v1 - snd v2)) vv1 vv2)
    newg :: Graph
    newg = iterComputations g

testF :: Graph -> IO ()
testF graph = do
  t1 <- getSystemTime
  let newg = iterComputations graph
      g    = graph
  uprint g
  t2 <- getSystemTime
  print $ systemSeconds t2 - systemSeconds t1

testMain :: IO ()
testMain = do
  files <- getArgs
  let 
    { morphFilePath = head $ tail files
    ; filePath = head files
    }
  mf <- DBC.readFile morphFilePath
  f <- DBC.readFile filePath 
  let 
    { mfData   = T.unpack(decodeUtf8 mf)
    ; origTxt   = T.unpack(decodeUtf8 f)
    ; txt       = changeExtraSymb origTxt
    ; wLemm     = map (\x -> getWordLemma x mfData)
    ; rdTxt     = unwords $ wLemm (words txt)
    }
  uprint $ getWordProp (words txt) mfData
