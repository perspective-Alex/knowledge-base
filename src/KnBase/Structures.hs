module KnBase.Structures where

import Prelude hiding (Word)
import Text.Show.Unicode

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

type WordWindow = [Word]
type WordForms  = [Word]
type WordLemma  = Word

type WordInfo = (PartOfSpeech, WordForms, WordWindow)

data WordProp = Prop
  { word :: Word
  , wLemma :: Word
  , wPOS :: PartOfSpeech
  }
 deriving(Eq,Show)

newtype AssocList k v = AssocList {alData :: [(k, v)]}
 deriving (Eq,Show)

type Dictionary = AssocList WordLemma WordInfo


alAdd :: WordLemma -> PartOfSpeech -> Word -> WordWindow
      -> Dictionary 
      -> Dictionary 
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

getDictWords :: WordLemma -> Dictionary -> [Word]
getDictWords wl d = case lookup wl (alData d) of 
  Nothing -> ["NoSuchLemmaInDict"]
  Just (_,ww,_) -> ww

getDictPOS:: WordLemma -> Dictionary -> PartOfSpeech 
getDictPOS wl d = case lookup wl (alData d) of 
  Nothing -> Smth "NoSuchLemmaInDict"
  Just (pos,_,_) -> pos

getDictWordWindow :: WordLemma -> Dictionary -> WordWindow
getDictWordWindow wl d = case lookup wl (alData d) of 
  Nothing -> ["NoSuchLemmaInDict"]
  Just (_,_,window) -> window

getDictLemma :: Word -> Dictionary -> Maybe WordLemma
getDictLemma w d = findLemma (alData d)
  where
    findLemma :: [(WordLemma, WordInfo)] -> Maybe WordLemma
    findLemma [] = Nothing 
    findLemma ((wl,(_,wf,_)):xs) =
      if w `elem` wf then Just wl
                     else findLemma xs

data Keywords = Kwds
  { oneWord  :: [Vertice]
  , twoWords :: [Vertice]
  }
  deriving (Eq)

instance Show Keywords where
  show (Kwds ow dw) = "Keywords:\n" ++ 
                     "One-Word-instances:" ++ ushow ow ++ "\n" ++
                     "Double-Word-instances:" ++ ushow dw ++ "\n"

printKeywords :: Keywords -> IO ()
printKeywords (Kwds ow dw) = do
  putStrLn "One-Word-instances:"
  mapM_ (uprint) ow
  putStrLn "Double-Word-instances:"
  mapM_ (uprint) dw
