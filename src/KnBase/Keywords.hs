module KnBase.Keywords where

import KnBase.Structures
import KnBase.Text

import Prelude hiding (Word)
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as DBC
import qualified Data.Text as T
import Data.List
import Data.Ord
import System.IO
import System.Environment

-- used to stop computing
threshold :: Float
threshold = 0.001

-- parameter - damping factor in score-formula
paramD :: Float
paramD = 0.85

eqW :: Vertice -> Vertice -> Bool
eqW v1 v2 = fst v1 == fst v2

formula :: Vertice -> [Edge] -> Float
formula v ee = (1.0-paramD) + summand
  where
    summand :: Float
    summand =
      case (length (adjWithVert v ee)) of
        0 -> 0.0
        _ -> paramD * jSum (adjWithVert v ee)
    jSum :: [Vertice] -> Float
    jSum [] = 0.0
    jSum (x:xs) = (snd x) / (fromIntegral (length (adjWithVert x ee)))
                      + jSum xs

{-
-- list of pairs of words met together (in window) in text
listCoOccur :: [Vertice] -> MorphFile -> Text -> [(Word,Word)]
listCoOccur vl mf txt = filter (\dw -> dw `elem` closeWords) plist
  where
    closeWords = wNCoOccur (lemmTxt mf txt) windowSize
    plist = pairsFromList wlist
    wlist = map (fst) vl 
-}
{-
-- list of pairs of words met together (in window) in text
listCoOccur :: [Vertice] -> MorphFile -> Text -> [(Word,Word)]
listCoOccur vl mf txt = filter (existEdge (lemmTxt mf txt)) plist
  where
    plist = pairsFromList wlist
    wlist = map (fst) vl 
-}
-- list of pairs of words met together (in window) in text
listCoOccur :: [Vertice] -> Dictionary -> [(Word,Word)]
listCoOccur [] d = []
listCoOccur (v:vs) d = (createEdges v d) ++ listCoOccur vs d
  where
    createEdges v d = foldr (crEdge) [] (window)
      where
        window = getDictWordWindow (fst v) d
        crEdge w acc =
          case getDictLemma w d of 
            Nothing -> acc
            Just wl -> (fst v, wl) : acc

-- pairs of different elements from list
pairsFromList :: [a] -> [(a,a)]
pairsFromList [] = []
pairsFromList (x:xs) = [(x,y) | y <- xs] ++ pairsFromList xs

similarCortege :: (Word,Word) -> (Word,Word) -> Bool
similarCortege (x1,y1) (x2,y2) =
    (x1 == y2 && x2 == y1) || (x1 == y1 && x2 == y2)

-- together (windowSize = required)
existEdge :: Text -> (Word, Word) -> Bool
existEdge txt (w1,w2) = wNCoOccur w1 w2 txt windowSize

-- candidates can be only nouns and adjectives
buildVertices :: Dictionary -> [Vertice]
buildVertices d = map (defVert)
               (alFoldr (\ el acc -> fst el : acc) [] d) 

defVert :: Word -> Vertice
defVert w = (w, 1.0)


buildEdges :: [Vertice] -> Dictionary -> [Edge]
buildEdges vl d = map (\(w1,w2) -> (defVert w1, defVert w2)) 
                     (nub $ listCoOccur vl d)

-- define adjacent vertices with given one
adjWithVert :: Vertice -> [Edge] -> [Vertice]
adjWithVert v [] = []
adjWithVert v (e:es)
      | fst e == v = (snd e) : adjWithVert v es
      | snd e == v = (fst e) : adjWithVert v es
      | otherwise  = adjWithVert v es

-- checks if edge's elements contained in owCandidates
isOneWordCand :: [Vertice] -> Edge -> Bool
isOneWordCand vv (v1,v2) = (any (== v1) vv) && (any (== v2) vv)

doubleVer :: Dictionary -> Vertice -> Vertice -> [Vertice]
doubleVer d v1 v2 = [(unwords [x,y],snd v1 + snd v2)
                    | x <- l1, y <- l2]
  where
    l1 = getDictWords (fst v1) d
    l2 = getDictWords (fst v2) d

-- extract all possible double-elem-collocations
dWordSeq :: Dictionary -> [Edge] -> [[Vertice]]
dWordSeq d le = map (\(v1,v2) -> doubleVer d v1 v2) src
  where
    src = [(x,y) | (x,y) <- le, containNoun2 d x y]
    containNoun2 d x y = getDictPOS (fst x) d == Noun ||
                         getDictPOS (fst y) d == Noun


tripleVer :: Dictionary -> Vertice -> Vertice -> Vertice -> [Vertice]
tripleVer d v1 v2 v3 = [(unwords [x,y,z], snd v1 + snd v2 + snd v3)
                    | x <- l1, y <- l2, z <- l3]
  where
    l1 = getDictWords (fst v1) d
    l2 = getDictWords (fst v2) d
    l3 = getDictWords (fst v3) d

-- extract different triangles from graph
triangles :: [Edge] -> [(Vertice,Vertice,Vertice)]
triangles [] = []
triangles ((v1,v2):es) = [(v1,v2,v3) | v3 <- (adjWithVert v1 es)] ++ 
                         [(v1,v2,v4) | v4 <- (adjWithVert v2 es)] ++
                         triangles es

-- extract all possible triple-elem-sequences from list
tWordSeq :: Dictionary -> [Edge] -> [[Vertice]]
tWordSeq d le = map (\(v1,v2,v3) -> tripleVer d v1 v2 v3) src
  where
    triples = triangles le
    src = [(x,y,z) | (x,y,z) <- triples, containNoun2 d x y z]
    containNoun2 d x y z = getDictPOS (fst x) d == Noun ||
                           getDictPOS (fst y) d == Noun ||
                           getDictPOS (fst z) d == Noun

-- | shell for mCand function, it discards variations of same candidates
multiCand :: String -> [[Vertice]] -> [Vertice]
multiCand _ [] = []
multiCand txt (x:xs) =
  case mCand txt x of
    []     -> multiCand txt xs
    (y:ys) -> y : multiCand txt xs -- add only 1 instance of suggested

-- create possible multi-elem-candidates
mCand :: String -> [Vertice] -> [Vertice]
mCand _ [] = []
mCand txt ((w1,s1):xs) -- w1 contains more than 1 word
  | isInfixOf (" " ++ w1 ++ " ") 
        (" " ++ txt ++ " ") = 
      (w1,s1) : mCand txt xs
  | otherwise = mCand txt xs 

iterComputations :: Graph -> Graph
iterComputations (Graph vv ee) = Graph newVertices
                                  (map (updEdge newVertices) ee)
  where
    updScore :: Vertice -> [Edge] -> Vertice
    updScore v1 ee = (fst v1, formula v1 ee)

    newVertices = map (\ver -> updScore ver ee) vv

    updEdge :: [Vertice] -> Edge -> Edge
    updEdge vv2 ((w1,s1),(w2,s2)) = ((w1,news w1),(w2,news w2))
      where
        news :: Word -> Score
        news w = case (find (\el -> fst el == w) vv2) of
                  Just a -> snd a
                  Nothing -> -1000.0

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

sortByScore :: [Vertice] -> [Vertice]
sortByScore vv = sortBy sCompare vv
  where
    sCompare :: Vertice -> Vertice -> Ordering
    sCompare (_,s1) (_,s2)
      | s2 < s1   = LT
      | s2 > s1   = GT
      | otherwise = EQ

buildGraph ::
  Dictionary ->
  String     -> -- MorphFile data
  Text       -> -- Original Content
  Graph
buildGraph dict mfData allWords = graph
  where
    nounsAndAdj = getReqWords dict
    vertices    = buildVertices dict
    edges       = buildEdges vertices dict
    graph       = Graph vertices edges
