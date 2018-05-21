module KnBase.Structures where

import Prelude hiding (Word)
import Text.Show.Unicode
import System.IO
import qualified Data.HashMap.Strict as HMap

type Word = String

type Sentence = String
type SentText = [Sentence]
type Text = [Word]
type MorphFile = String

type Score = Float
type Vertice = (Word, Score)
type Edge = (Vertice, Vertice)

data Graph = Graph
  { gVertices :: [Vertice]
  , gEdges :: [Edge]
  }
  deriving(Eq)
instance Show Graph where
  show (Graph v e) = "MyGraph:\n" ++ 
                     "Vertices: " ++ ushow v ++ "\n" ++
                     "Edges: " ++ ushow e ++ "\n"
printGraph :: Graph -> IO ()
printGraph (Graph vv ee) = do
  putStrLn "Vertices:"
  mapM_ (uprint) vv
  putStrLn "Edges:"
  mapM_ (uprint) ee

data PartOfSpeech = Noun | Adjective | Verb | Adverb | Smth String
  deriving(Eq,Show)

data WordProperties = Prop
  { wLemma :: WordLemma
  , wPOS :: PartOfSpeech
  }
 deriving(Eq,Show)


-- | Dictionary with information from MorphFile (lemmas and POS only)
-- it stores info for all wordforms, its helpful to transform original text
-- example: normalize text - change all words with its lemmas
type Dictionary = HMap.HashMap Word WordProperties

newtype AssocList k v = AssocList {alData :: [(k, v)]}
 deriving (Eq,Show)

type WordWindow = [Word]
type WordForms  = [Word]
type WordLemma  = Word
type LemmaInfo = (PartOfSpeech, WordForms, WordWindow)
type LemmaProperties = AssocList WordLemma LemmaInfo

alAdd :: WordLemma -> PartOfSpeech -> Word -> WordWindow
      -> LemmaProperties 
      -> LemmaProperties 
alAdd wl pos w window al1 = case lookup wl (alData al1) of
  Nothing -> AssocList ((wl, (pos, [w], window)) : alData al1)
  Just _ -> AssocList (map change (alData al1))
    where
      change x@(xwl, (xpos, xws, xwindow)) =
          if xwl == wl
          then
            if w `elem` xws
            then x
            else (xwl, (xpos, w:xws, window ++ xwindow))
          else x

alFoldr :: ((k,a) -> b -> b) -> b -> AssocList k a -> b
alFoldr f acc al = go acc al 
  where
    go accum (AssocList []) = accum
    go accum (AssocList (x:xs)) = f x (go accum (AssocList xs))

getWords :: WordLemma -> LemmaProperties -> [Word]
getWords wl d = case lookup wl (alData d) of 
  Nothing -> ["NoSuchLemmaInDict"]
  Just (_,ww,_) -> ww

getPOS:: WordLemma -> LemmaProperties -> PartOfSpeech 
getPOS wl d = case lookup wl (alData d) of 
  Nothing -> Smth "NoSuchLemmaInDict"
  Just (pos,_,_) -> pos

getWordWindow :: WordLemma -> LemmaProperties -> WordWindow
getWordWindow wl d = case lookup wl (alData d) of 
  Nothing -> ["NoSuchLemmaInDict"]
  Just (_,_,window) -> window

getLemma :: Word -> LemmaProperties -> Maybe WordLemma
getLemma w d = findLemma (alData d)
  where
    findLemma :: [(WordLemma, LemmaInfo)] -> Maybe WordLemma
    findLemma [] = Nothing 
    findLemma ((wl,(_,wf,_)):xs) =
      if w `elem` wf then Just wl
                     else findLemma xs

data Keywords = Kwds
  { oneWord  :: [Vertice]
  , mulWords :: [Vertice]
  }
  deriving (Eq)

instance Show Keywords where
  show (Kwds ow mw) = "Keywords:\n" ++ 
                     "One-Word-instances:" ++ ushow ow ++ "\n" ++
                     "Mul-Word-instances:" ++ ushow mw ++ "\n"

printKeywords :: Keywords -> IO ()
printKeywords (Kwds ow mw) = do
  putStrLn "One-Word-instances:"
  mapM_ (uprint) ow
  putStrLn "Mul-Word-instances:"
  mapM_ (uprint) mw

printKeywordsAndIcounts :: [(Keywords,Int)] -> IO ()
printKeywordsAndIcounts [] = return ()
printKeywordsAndIcounts all@(x:xs) = do
  putStrLn $ "\n" ++ show (length all) ++ " text:\n"
  printKeywords $ fst x 
  putStrLn $ "Count of iterations = " ++ (ushow $ snd x)
  printKeywordsAndIcounts xs

-- | only one-word keywords now
printKeywordsToSpecFile :: FilePath -> Keywords -> IO ()
printKeywordsToSpecFile fp (Kwds ow mw) = do
  withFile fp WriteMode (\handle -> do
    mapM_ (hPutStrLn handle) $ map (ushow . fst) ow)

type KeywordsInfo = HMap.HashMap Word SentText

printOntElem :: (Word,SentText) -> IO ()
printOntElem (w,st) = do
  putStr "\n"
  uprint w
  putStr "\n"
  mapM_ (uprint) st
