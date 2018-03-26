module KnBase.Process where

import KnBase.Types
import Prelude hiding (Word)
import Data.List
import Data.Ord

-- ? need to ADD LEMMATIZATION of words before everything

windowSize :: Int
windowSize = 3

threshold :: Float
threshold = 0.005

-- parameter - damping factor in score-formula
paramD :: Float
paramD = 0.85

eqW :: Vertice -> Vertice -> Bool
eqW v1 v2 = fst v1 == fst v2

delDups2 :: [Vertice] -> [Vertice]
delDups2 vv = nubBy eqDWVertCortege vv
  where
    eqDWVertCortege :: Vertice -> Vertice -> Bool
    eqDWVertCortege v1 v2 = eqCortege (cort v1) (cort v2)
      where
        transform [x,y] = (x,y)
        cort v = transform (words $ fst v)

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

--file - result of morphological analysis
morphFilePath :: String
morphFilePath = "/home/alex/6sem/haskell/NLP/knBase/data/mf_astronomy.txt"

filePath :: String
filePath = "/home/alex/6sem/haskell/NLP/knBase/data/astronomy.txt"

getPartOfSpeech :: String -> MorphFile -> PartOfSpeech
getPartOfSpeech word content
    | pOS == "S"    = Noun
    | pOS == "A"    = Adjective
    | pOS == "V"    = Verb
    | pOS == "ADV"  = Adverb
    | otherwise = Smth pOS
  where
    pOS = getPOS word (lines content)
    getPOS :: String -> [String] -> String
    getPOS _ [] = "List of words doest contain this word"
    getPOS [] _ = "Word is undefined"
    getPOS w (x:xs)
      | takeWhile (/= '{') x == w = gPOS x
      | otherwise = getPOS w xs
    gPOS :: String -> String
    gPOS (y:ys)
      | y == '='  = extrPOS ys
      | otherwise = gPOS ys
    extrPOS (z:zs) = if [el|el <- ['A' .. 'Z'], el == z] == []
                      then []
                      else z : extrPOS zs

-- words occur together in window with given size
wCoOccur :: Word -> Word -> Text -> Int -> Bool 
wCoOccur w1 w2 txt n = any (==True) (zipWith together txt (drop (n-1) txt))
  where
    together el1 el2 = (el1 == w1) && (el2 == w2)
                    || (el1 == w2) && (el2 == w1)

wNCoOccur :: Word -> Word -> Text -> Int -> Bool
wNCoOccur w1 w2 txt n =
    case n of
      1 -> False 
      _ -> (wCoOccur w1 w2 txt n) || (wNCoOccur w1 w2 txt (n-1))

listCoOccur :: Text -> [(Word,Word)]
listCoOccur txt = 
  delDups $ filter (existEdge txt) ([(x,y)|x <- txt, y <- txt, x /= y])
  where
    delDups :: [(Word,Word)] -> [(Word,Word)]
    delDups = nubBy eqCortege

eqCortege :: (Word,Word) -> (Word,Word) -> Bool
eqCortege (x1,y1) (x2,y2) =
    if (x1 == y2 && x2 == y1) || (x1 == x2 && y1 == y2)
    then True
    else False

existEdge :: Text -> (Word, Word) -> Bool
existEdge txt (w1,w2) = wNCoOccur w1 w2 txt windowSize

-- candidates can be only nouns and adjectives
buildVertices :: MorphFile -> Text -> [Vertice]
buildVertices mf txt = map (defVert)
                          (nub $ filter (\w -> nounOrAdj w) txt)
  where
    nounOrAdj :: Word -> Bool
    nounOrAdj w = (getPartOfSpeech w mf == Adjective) 
                || (getPartOfSpeech w mf == Noun)

defVert :: Word -> Vertice
defVert word = (word, 1.0)

{-
defVert :: MorphFile -> Word -> Vertice
defVert mf word = (tail (initForm (lines mf) word), 1.0)
  where
    initForm :: Text -> Word -> Word
    initForm (x:xs) w
      | takeWhile (/= '{') x == w = takeWhile (/= '=') 
                                      (dropWhile (/= '{') x) 
      | otherwise  = initForm xs w
-}
    

buildEdges :: MorphFile -> Text -> [Edge]
buildEdges mf txt = map (\(w1,w2) -> (defVert w1, defVert w2)) 
                     (filter isVertice (listCoOccur txt))
  where
    vertices = buildVertices mf txt
    isVertice :: (Word, Word) -> Bool
    isVertice (w1,w2) = w1 `elem` (map (\(x,_) -> x) vertices) &&
                        w2 `elem` (map (\(x,_) -> x) vertices)

genGraph :: [Vertice] -> [Edge] -> Graph
genGraph v e = Graph v e

-- define adjacent vertices with given one
adjWithVert :: Vertice -> [Edge] -> [Vertice]
adjWithVert v [] = []
adjWithVert v (e:es)
      | fst e == v = (snd e) : adjWithVert v es
      | snd e == v = (fst e) : adjWithVert v es
      | otherwise  = adjWithVert v es

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

sortByScore :: [Vertice] -> [Vertice]
sortByScore vv = sortBy sCompare vv
  where
    sCompare :: Vertice -> Vertice -> Ordering
    sCompare (_,s1) (_,s2)
      | s2 < s1   = LT
      | s2 > s1   = GT
      | otherwise = EQ

-- extract all possible double-elem-sequences from list
dWordSeq :: MorphFile -> [Vertice] -> [(Vertice,Vertice)]
dWordSeq mf l = [(x,y) | x <- l, y <- l, x /= y && containNoun2 mf x y]
  where
    containNoun2 mf x y = getPartOfSpeech (fst x) mf == Noun ||
                       getPartOfSpeech (fst y) mf == Noun

-- extract all possible triple-elem-sequences from list
tWordSeq :: MorphFile -> [Vertice] -> [(Vertice,Vertice,Vertice)]
tWordSeq mf l = [(x,y,z) | x <- l, y <- l, z <- l, containNoun3 mf x y z &&
                                 (x /= y && y /= z && x /= z)]
  where
    containNoun3 mf x y z = getPartOfSpeech (fst x) mf == Noun ||
                         getPartOfSpeech (fst y) mf == Noun ||
                         getPartOfSpeech (fst z) mf == Noun

-- create possible double-elem-candidates
dCand :: String -> [(Vertice,Vertice)] -> [Vertice]
dCand _ [] = []
dCand txt (((w1,s1),(w2,s2)):xs)
  | isInfixOf (" " ++ (unwords [w1,w2]) ++ " ") 
        (" " ++ txt ++ " ") = 
      (unwords [w1,w2], s1 + s2) : dCand txt xs
  | isInfixOf (" " ++ (unwords [w2,w1]) ++ " ")
        (" " ++ txt ++ " ") = 
      (unwords [w2,w1], s1 + s2) : dCand txt xs
  | otherwise = dCand txt xs 

-- create possible triple-elem-candidates
tCand :: String -> [(Vertice,Vertice,Vertice)] -> [Vertice]
tCand _ [] = []
tCand txt (((w1,s1),(w2,s2),(w3,s3)):xs)
  | isInfixOf (" " ++ (unwords [w1,w2,w3]) ++ " ") 
        (" " ++ txt ++ " ") = 
      (unwords [w1,w2,w3], s1 + s2 + s3) : tCand txt xs
  | isInfixOf (" " ++ (unwords [w1,w3,w2]) ++ " ")
        (" " ++ txt ++ " ") = 
      (unwords [w1,w3,w2], s1 + s2 + s3) : tCand txt xs
  | isInfixOf (" " ++ (unwords [w2,w1,w3]) ++ " ") 
        (" " ++ txt ++ " ") = 
      (unwords [w2,w1,w3], s1 + s2 + s3) : tCand txt xs
  | isInfixOf (" " ++ (unwords [w2,w3,w1]) ++ " ")
        (" " ++ txt ++ " ") = 
      (unwords [w2,w3,w1], s1 + s2 + s3) : tCand txt xs
  | isInfixOf (" " ++ (unwords [w3,w1,w2]) ++ " ") 
        (" " ++ txt ++ " ") = 
      (unwords [w3,w1,w2], s1 + s2 + s3) : tCand txt xs
  | isInfixOf (" " ++ (unwords [w3,w2,w1]) ++ " ")
        (" " ++ txt ++ " ") = 
      (unwords [w3,w2,w1], s1 + s2 + s3) : tCand txt xs
  | otherwise = tCand txt xs 

changeExtraSymb :: String -> String
changeExtraSymb str = map extraSymb str
  where
    extraSymb s =
      case s of
        '.' -> ' '
        '?' -> ' '
        '!' -> ' '
        ',' -> ' '
        _   -> s
