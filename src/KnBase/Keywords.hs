module KnBase.Keywords where

import KnBase.Structures
import KnBase.Text

import Prelude hiding (Word)
import Data.Text.Encoding
import qualified Data.HashMap.Strict as HMap
import qualified Data.ByteString.Char8 as DBC
import qualified Data.Text as T
import Data.List
import Data.Ord
import System.IO
import System.Environment

-- | used to stop computing scores in graph
-- necessary level of approximation is reached
threshold :: Float
threshold = 0.001

-- | parameter - damping factor in score-formula
paramD :: Float
paramD = 0.85

eqWords :: Vertice -> Vertice -> Bool
eqWords v1 v2 = fst v1 == fst v2

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

-- list of pairs of words met together (in window) in text
listCoOccur :: [Vertice] -> LemmaProperties -> [(Word,Word)]
listCoOccur [] d = []
listCoOccur (v:vs) d = (createEdges v d) ++ listCoOccur vs d
  where
    createEdges v d = foldr (crEdge) [] (window)
      where
        window = getWordWindow (fst v) d
        crEdge w acc =
          case getLemma w d of 
            Nothing -> acc
            Just wl -> (fst v, wl) : acc

-- pairs of different elements from list
pairsFromList :: [a] -> [(a,a)]
pairsFromList [] = []
pairsFromList (x:xs) = [(x,y) | y <- xs] ++ pairsFromList xs

similarCortege :: (Word,Word) -> (Word,Word) -> Bool
similarCortege (x1,y1) (x2,y2) =
    (x1 == y2 && x2 == y1) || (x1 == y1 && x2 == y2)

-- candidates can be only nouns and adjectives
buildVertices :: LemmaProperties -> [Vertice]
buildVertices d = map (defVert)
               (alFoldr (\ el acc -> fst el : acc) [] d) 

defVert :: Word -> Vertice
defVert w = (w, 1.0)


buildEdges :: [Vertice] -> LemmaProperties -> [Edge]
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

doubleVer :: LemmaProperties -> Vertice -> Vertice -> [Vertice]
doubleVer d v1 v2 = [(unwords [x,y],snd v1 + snd v2)
                    | x <- l1, y <- l2]
  where
    l1 = getWords (fst v1) d
    l2 = getWords (fst v2) d

-- | extract all possible double-elem-collocations
dWordSeq :: LemmaProperties -> [Edge] -> [[Vertice]]
dWordSeq d le = map (\(v1,v2) -> doubleVer d v1 v2) src
  where
    src = [(x,y) | (x,y) <- le, containNoun2 d x y]
    containNoun2 d x y = getPOS (fst x) d == Noun ||
                         getPOS (fst y) d == Noun


tripleVer :: LemmaProperties -> Vertice -> Vertice -> Vertice -> [Vertice]
tripleVer d v1 v2 v3 = [(unwords [x,y,z], snd v1 + snd v2 + snd v3)
                    | x <- l1, y <- l2, z <- l3]
  where
    l1 = getWords (fst v1) d
    l2 = getWords (fst v2) d
    l3 = getWords (fst v3) d

-- | extract different triangles from graph
triangles :: [Edge] -> [(Vertice,Vertice,Vertice)]
triangles [] = []
triangles ((v1,v2):es) = [(v1,v2,v3) | v3 <- (adjWithVert v1 es)] ++ 
                         [(v1,v2,v4) | v4 <- (adjWithVert v2 es)] ++
                         triangles es

-- | extract all possible triple-elem-sequences from list
tWordSeq :: LemmaProperties -> [Edge] -> [[Vertice]]
tWordSeq d le = map (\(v1,v2,v3) -> tripleVer d v1 v2 v3) src
  where
    triples = triangles le
    src = [(x,y,z) | (x,y,z) <- triples, containNoun2 d x y z]
    containNoun2 d x y z = getPOS (fst x) d == Noun ||
                           getPOS (fst y) d == Noun ||
                           getPOS (fst z) d == Noun

-- | shell for mCand function, it discards variations of same candidates
multiCand :: Text -> [[Vertice]] -> [Vertice]
multiCand _ [] = []
multiCand txt (x:xs) =
  case mCand txt x of
    []     -> multiCand txt xs
    (y:ys) -> y : multiCand txt xs -- add only 1 instance of suggested

-- | create possible multi-elem-candidates
mCand :: Text -> [Vertice] -> [Vertice]
mCand _ [] = []
mCand txt ((w1,s1):xs) -- w1 contains more than 1 word
  | isInfixOf (words w1) txt = (w1,s1) : mCand txt xs
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

-- | iterate computations until score-diff < threshold and get iter_count
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

isPartTerm :: Vertice -> Vertice -> Bool
isPartTerm v1 v2 = isInfixOf words1 words2 || isInfixOf words2 words1
  where
    words1 = words (fst v1)
    words2 = words (fst v2)

buildGraph ::
  LemmaProperties ->
  Graph
buildGraph dict = graph
  where
    vertices    = buildVertices dict
    edges       = buildEdges vertices dict
    graph       = Graph vertices edges

-- | returns final kerwords and count of iterations
-- iterations take place while computing final values in graph
getKeywords :: String -> Dictionary -> (Keywords, Int)
getKeywords origTxt dict = (keywords, iterCount)
  where
    txt       = origTxt
    allWords  = words txt
    lp        = initLemmaProps allWords dict 
    graph     = buildGraph lp
    
    algRes = loopComputations threshold (graph,0)
    resGraph = fst algRes
    iterCount = snd algRes
    resVertices = gVertices resGraph
    proportion = truncate (fromIntegral (length resVertices) / 3) 
    owCandidates = take proportion $ sortByScore resVertices
    reqEdges     = filter (isOneWordCand owCandidates) (gEdges resGraph)  
    dwCandidates = (multiCand allWords (dWordSeq lp reqEdges))
    twCandidates = nub $ (multiCand allWords (tWordSeq lp reqEdges))
    --mulwCandidates = nubBy isPartTerm $
    --                 sortByScore $ dwCandidates ++ twCandidates
    mulwCandidates = sortByScore dwCandidates
    keywords = Kwds owCandidates mulwCandidates
    

foldKeywords :: ((Vertice -> Vertice -> Bool) ->
                  [Vertice] -> [Vertice] -> [Vertice]) ->
                [Keywords] -> Keywords
foldKeywords f kwds = 
  foldr1 (\el1 el2 ->
  (Kwds (f (eqWords) (oneWord el1) (oneWord el2))
        (f (eqWords) (mulWords el1) (mulWords el2))))
  kwds 

-- | now works with lemmatized one-word keywords only
getKeywordContext :: Dictionary -> [Word] -> SentText
                     -> KeywordsInfo -> KeywordsInfo
getKeywordContext _ _ [] hm = hm
getKeywordContext dict ww (s:st) hm =
  getKeywordContext dict ww st (foldr (addSent) hm ww)
    where
      addSent w acc =
        if w `elem` ls
        then ((HMap.insertWith (++) w [s] acc) :: KeywordsInfo)
        else acc
      ls = lemmOrigContent dict s
