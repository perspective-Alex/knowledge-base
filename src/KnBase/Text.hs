module KnBase.Text where

import KnBase.Structures
import Prelude hiding (Word)
import Data.List
import qualified Data.HashMap.Strict as HMap
import Data.List.Split (wordsBy)

cyrillicSymbols :: String
cyrillicSymbols = ['а' .. 'я'] ++ ['А' .. 'Я']

sentDelimiters :: String
sentDelimiters = ".?!"

delimiters :: String
delimiters = ",.?!@#$%^&*(){}[]\\|/`~№\":;"

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

initDictionary :: MorphFile -> Dictionary
initDictionary mf = initDict (lines mf) HMap.empty
  where
    initDict :: [String] -> Dictionary -> Dictionary
    initDict [] hm = hm
    initDict (x:xs) hm = initDict xs newDict
      where
        newDict = HMap.insert k properties hm
        properties = parseRaw raw
        finalPos = wPOS properties
        (k,raw) = span (/= '{') x
        parseRaw :: String -> WordProperties
        parseRaw str =
          if isInfixOf "??" str
          then Prop k (Smth "EnglishWord")
          else Prop (drop 1 finLemma) pos -- drop '{'
            where
              (lemma, rtail) = span (/= '=') str
              finLemma = delete '?' lemma --delete morphological ambiguity
              parsePos str2 =
                takeWhile (\l -> l /= ',' && l /= '=') (drop 1 str2)
              pos =
                case parsePos rtail of
                  "S" -> Noun
                  "V" -> Verb
                  "A" -> Adjective
                  "Adv" -> Adverb
                  _ -> Smth (parsePos rtail)


-- | connect lemma with it's properties form text and MorphFile
initLemmaProps :: Text -> Dictionary -> LemmaProperties
initLemmaProps txt dict = textToLP (AssocList []) txt
  where
    textToLP :: LemmaProperties -> Text -> LemmaProperties
    textToLP lp [] = lp
    textToLP lp (x:xs) = textToLP (addElem x) xs 
      where
        addElem w =
          case maybePos of 
            Nothing -> lp
            Just p -> if p == Adjective || p == Noun 
                      then alAdd lemma p w window lp
                      else lp
          where
            (maybePos,lemma) = parseVal $ HMap.lookup w dict
            parseVal v =
              case v of
                Nothing -> (Nothing, "NoLemm")
                Just prop -> (Just (wPOS prop), wLemma prop)
            window = take windowSize xs

getDictWordPOS :: Word -> Dictionary -> Maybe PartOfSpeech
getDictWordPOS w d =
  case HMap.lookup w d of
    Nothing -> Nothing 
    Just v -> Just (wPOS v)

getDictWordLemma :: Word -> Dictionary -> Maybe WordLemma 
getDictWordLemma w d =
  case HMap.lookup w d of
    Nothing -> Nothing
    Just v -> Just (wLemma v)

-- | text with lemmed words (required text with words only)
lemmTxt :: Dictionary -> Text -> Text
lemmTxt dict txt = map (\w -> changeWithLemma w dict) txt

-- | Lemmatize original text, it may contain english words,
-- some signs and digits which have to stay unchanged
lemmOrigContent :: Dictionary -> String -> Text
lemmOrigContent dict str = map (\w -> changeWithLemma w dict) (words str)

-- | unknown words stay unchanged
changeWithLemma :: Word -> Dictionary -> Word
changeWithLemma w dict =
  case getDictWordLemma w dict of
    Nothing -> w
    Just l -> l

-- | surround all delimeters with brackets to make them separate tokens
-- >>> surroundDelimeters "word."
-- "word . "
surroundDelimeters :: String -> String
surroundDelimeters = foldMap (surrDelims)
  where
    surrDelims c = 
      if c `elem` delimiters
      then [' ',c,' ']
      else [c]

delLineBreak :: String -> String
delLineBreak [] = []
delLineBreak (x:xs) =
  if x == '\n'
  then delLineBreak xs
  else x : delLineBreak xs

delWordBreak :: String -> String
delWordBreak [] = []
delWordBreak [x] = [x]
delWordBreak (x:y:xs) =
  if [x,y] == "-\n"
  then delWordBreak xs
  else x : y : delWordBreak xs

preProcess :: String -> String
preProcess = surroundDelimeters

-- | Extract substrings begins with word1 and ends with word2
subStrings :: Text -> (Word, Word) -> [Text]
subStrings [] _ = []
subStrings txt (f,l) = 
  if snd slice1 /= [] && snd slice2 /= [] -- txt still contains substring 
  then (fst slice2 ++ [border]) : subStrings sTail (f,l)
  else []
  where
    slice1 = span (/= f) txt
    slice2 = span (/= l) (snd slice1)
    sTail = tail $ snd slice2
    border = head $ snd slice2
