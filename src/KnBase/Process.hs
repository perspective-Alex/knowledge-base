module KnBase.Process where

import KnBase.Types
import Prelude hiding (Word)
import Data.List
import Data.Ord

windowSize :: Int
windowSize = 2

-- used to stop computing
threshold :: Float
threshold = 0.001

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
{-
-- extract word properties from MorphFile
getWordProp :: Text -> MorphFile -> [WordProp]
getWordProp txt mf = map (\w -> Prop w
     (getWordLemma w mf) (getPartOfSpeech w mf)) txt 
-}

-- make dictionary based on word's lemmas
initDict :: Text -> MorphFile -> Dictionary
initDict txt mf = foldr (addElem) (AssocList []) txt
  where
    curPOS w = getPartOfSpeech w mf
    addElem :: Word -> Dictionary -> Dictionary 
    addElem w d
      | curPOS w == Adjective || curPOS w == Noun =
            alAdd (getWordLemma w mf) (curPOS w) w d
      | otherwise = d

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

normTxt :: MorphFile -> Text -> Text
normTxt mf txt = map (\w -> getWordLemma w mf) txt

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

-- list of pairs of words met together (in window) in text
listCoOccur :: Dictionary -> MorphFile -> Text -> [(Word,Word)]
listCoOccur d mf txt = 
  delDups $ filter (existEdge (normTxt mf txt))
        ([(x,y)|x <- wlist, y <- wlist, x /= y])
  where
    wlist = map (fst) (buildVertices d)
    delDups :: [(Word,Word)] -> [(Word,Word)]
    delDups = nubBy eqCortege

eqCortege :: (Word,Word) -> (Word,Word) -> Bool
eqCortege (x1,y1) (x2,y2) =
    if (x1 == y2 && x2 == y1) || (x1 == x2 && y1 == y2)
    then True
    else False

-- together (windowSize = required)
existEdge :: Text -> (Word, Word) -> Bool
existEdge txt (w1,w2) = wNCoOccur w1 w2 txt windowSize

-- together (windowSize = 2)
together :: Text -> (Word, Word) -> Bool
together txt (w1,w2) = wNCoOccur w1 w2 txt 2

-- candidates can be only nouns and adjectives
buildVertices :: Dictionary -> [Vertice]
buildVertices d = map (defVert)
               (alFoldr (\ el acc -> fst el : acc) [] d) 

defVert :: Word -> Vertice
defVert w = (w, 1.0)


buildEdges :: Dictionary -> MorphFile -> Text -> [Edge]
buildEdges d mf txt = map (\(w1,w2) -> (defVert w1, defVert w2)) 
                     (listCoOccur d mf txt)

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

doubleVer :: Dictionary -> Vertice -> Vertice -> [Vertice]
doubleVer d v1 v2 = [(unwords [x,y],snd v1 + snd v2)
                    | x <- l1, y <- l2]
  where
    l1 = getDictWords (fst v1) d
    l2 = getDictWords (fst v2) d

-- extract all possible double-elem-collocations
dWordSeq :: Dictionary -> [Vertice] -> [[Vertice]]
dWordSeq d l = map (\(v1,v2) -> doubleVer d v1 v2) src
  where
    src = [(x,y) | x <- l, y <- l, x /= y && containNoun2 d x y]
    containNoun2 d x y = getDictPOS (fst x) d == Noun ||
                         getDictPOS (fst y) d == Noun


tripleVer :: Dictionary -> Vertice -> Vertice -> Vertice -> [Vertice]
tripleVer d v1 v2 v3 = [(unwords [x,y,z], snd v1 + snd v2 + snd v3)
                    | x <- l1, y <- l2, z <- l3]
  where
    l1 = getDictWords (fst v1) d
    l2 = getDictWords (fst v2) d
    l3 = getDictWords (fst v3) d

-- extract all possible triple-elem-sequences from list
tWordSeq :: Dictionary -> [Vertice] -> [[Vertice]]
tWordSeq d l = map (\(v1,v2,v3) -> tripleVer d v1 v2 v3) src
  where
    src = [(x,y,z) | x <- l, y <- l, z <- l , containNoun2 d x y z]
    containNoun2 d x y z = getDictPOS (fst x) d == Noun ||
                           getDictPOS (fst y) d == Noun ||
                           getDictPOS (fst z) d == Noun


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
