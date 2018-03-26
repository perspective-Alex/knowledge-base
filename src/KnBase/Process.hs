module KnBase.Process where

import KnBase.Types
import Prelude hiding (Word)
import Data.List
import Data.Ord

-- ? need to ADD LEMMATIZATION of words before everything

windowSize :: Int
windowSize = 3

threshold :: Float
threshold = 0.01

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

-- extract word properties from MorphFile
getWordProp :: Text -> MorphFile -> [WordProp]
getWordProp txt mf = map (\w -> Prop w
     (getWordLemma w mf) (getPartOfSpeech w mf)) txt 

-- get word properties from MorphFile
getWordInfo :: Word -> [String] -> Maybe String
getWordInfo _ [] = Nothing 
getWordInfo w (x:xs)
    | takeWhile (/= '{') x == w = Just x
    | otherwise = getWordInfo w xs

getPartOfSpeech :: Word -> MorphFile -> PartOfSpeech
getPartOfSpeech w mf 
    | pOS == "S"    = Noun
    | pOS == "A"    = Adjective
    | pOS == "V"    = Verb
    | pOS == "ADV"  = Adverb
    | otherwise = Smth pOS
  where
    pOS = gPOS $ getWordInfo w (lines mf)
    gPOS :: Maybe String -> String
    gPOS Nothing  = "undefined" -- its temporary decision, ofc its wrong
    gPOS (Just []) = "undefined" -- its temporary decision, ofc its wrong
    gPOS (Just (y:ys))
      | y == '='  = extrPOS ys
      | otherwise = gPOS (Just ys)
    extrPOS (z:zs) = if [el | el <- ['A' .. 'Z'], el == z] == []
                      then []
                      else z : extrPOS zs

getWordLemma :: Word -> MorphFile -> Word
getWordLemma w mf = lemm $ getWordInfo w (lines mf) 
  where
    lemm :: Maybe String -> Word
    lemm info =
      case info of 
        Nothing -> w
        Just str ->
          if isInfixOf "??" iTail
          then "NotRussianWord"
          else takeWhile (/= '=') iTail 
            where
              iTail = tail (dropWhile (/= '{') str)

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
buildVertices mf txt = map (defVert mf)
                          (nub $ filter (\w -> nounOrAdj w) txt)
  where
    nounOrAdj :: Word -> Bool
    nounOrAdj w = (getPartOfSpeech w mf == Adjective) 
                || (getPartOfSpeech w mf == Noun)

defVert :: MorphFile -> Word -> Vertice
defVert mf w = (w, 1.0)

{-
defVert :: MorphFile -> Word -> Vertice
defVert mf w = (getWordLemma w (lines mf), 1.0)
-}
    

buildEdges :: MorphFile -> Text -> [Edge]
buildEdges mf txt = map (\(w1,w2) -> (defVert mf w1, defVert mf w2)) 
                     (filter isVertice (listCoOccur txt))
  where
    vertices = buildVertices mf txt
    isVertice :: (Word, Word) -> Bool
    isVertice (w1,w2) = w1 `elem` (map (\(x,_) -> x) vertices) &&
                        w2 `elem` (map (\(x,_) -> x) vertices)

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
dWordSeq :: MorphFile -> [Vertice] -> [Vertice]
dWordSeq mf l = [(unwords [fst x, fst y],snd x + snd y)
                    | x <- l, y <- l, x /= y && containNoun2 mf x y]
  where
    containNoun2 mf x y = getPartOfSpeech (fst x) mf == Noun ||
                       getPartOfSpeech (fst y) mf == Noun

-- extract all possible triple-elem-sequences from list
tWordSeq :: MorphFile -> [Vertice] -> [Vertice] -> [Vertice]
tWordSeq mf ldw lw = [(unwords [fst x, fst y],snd x + snd y)
                  | x <- ldw, y <- lw]
                      ++
                     [(unwords [fst x, fst y],snd x + snd y)
                  | x <- lw, y <- ldw]

-- create possible multi-elem-candidates
mCand :: String -> [Vertice] -> [Vertice]
mCand _ [] = []
mCand txt ((w1,s1):xs) -- w1 contains more than 1 word
  | isInfixOf (" " ++ w1 ++ " ") 
        (" " ++ txt ++ " ") = 
      (w1,s1) : mCand txt xs
  | otherwise = mCand txt xs 

changeExtraSymb :: String -> String
changeExtraSymb str = map extraSymb str
  where
    extraSymb s =
      case s of
        '.' -> ' '
        '?' -> ' '
        '!' -> ' '
        ',' -> ' '
        '_' -> ' '
        _   -> s
