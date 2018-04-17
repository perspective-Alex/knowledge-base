module KnBase.Text where

import KnBase.Structures
import Prelude hiding (Word)
import Data.List
import Data.List.Split (wordsBy)

{-
-- extract word properties from MorphFile
getWordProp :: Text -> MorphFile -> [WordProp]
getWordProp txt mf = map (\w -> Prop w
     (getWordLemma w mf) (getPartOfSpeech w mf)) txt 
-}

cyrillicSymbols :: String
cyrillicSymbols = ['а' .. 'я'] ++ ['А' .. 'Я']

sentDelimiters :: String
sentDelimiters = ".?!"

delimiters :: String
delimiters = ".?!@#$%^&*(){}[]\\|/`~№:;"

windowSize :: Int
windowSize = 2

initSentText :: String -> SentText
initSentText str = wordsBy (\c -> c `elem` sentDelimiters) str

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

-- make dictionary based on word's lemmas
initDict :: Text -> MorphFile -> Dictionary
initDict txt mf = textToDict (AssocList []) txt
  where
    textToDict :: Dictionary -> Text -> Dictionary
    textToDict dict [] = dict
    textToDict dict (x:xs) = textToDict (addElem x dict) xs 
      where
        addElem :: Word -> Dictionary -> Dictionary 
        addElem w d
          | pos == Adjective || pos == Noun =
                alAdd lemma pos w window d
          | otherwise = d
          where
            pos    = getPartOfSpeech w mf
            lemma  = getWordLemma w mf
            window = take windowSize xs
            {-
            nub $ map (\w -> getWordLemma w mf) $
              filter (nounOrAdjective) (take windowSize xs)
                where
                  nounOrAdjective w = (getPartOfSpeech w mf) `elem`
                                          [Noun,Adjective]
                                          -}

-- get all nouns and adjectives contained in text
getReqWords :: Dictionary -> [Word]
getReqWords = alFoldr (\(_,(_,wl,_)) acc -> wl ++ acc) []

-- get word properties from MorphFile
-- (Nothing for ariph symbols, delims, etc.)
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

-- text with lemmed words (required text with words only)
lemmTxt :: MorphFile -> Text -> Text
lemmTxt mf txt = map (\w -> getWordLemma w mf) txt

lemmOrigContent :: MorphFile -> String -> Text
lemmOrigContent mf str = lemmTxt mf ((words . surroundDelimeters) str)

surroundDelimeters :: String -> String
surroundDelimeters = foldMap (surrDelims)
  where
    surrDelims c = 
      if c `elem` delimiters
      then [' ',c,' ']
      else [c]

{-
-- list of pairs of words occur together in window with given size
wCoOccur :: Text -> Int -> [(Word,Word)]
wCoOccur txt n = zipWith (\w1 w2 -> (w1,w2)) txt (drop (n-1) txt)

wNCoOccur :: Text -> Int -> [(Word,Word)]
wNCoOccur txt n =
    case n of
      1 -> []
      _ -> (wCoOccur txt n) ++ (wNCoOccur txt (n-1))
-}

-- case: words occur together in window with given size
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
