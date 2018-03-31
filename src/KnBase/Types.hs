module KnBase.Types where

import Prelude hiding (Word)
import Text.Show.Unicode

type Word = String
type WordLemma = Word
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

data PartOfSpeech = Noun | Adjective | Verb | Adverb | Smth String
  deriving(Eq,Show)

data WordProp = Prop
  { word :: Word
  , wLemma :: Word
  , wPOS :: PartOfSpeech
  }
 deriving(Eq,Show)

newtype AssocList k v = AssocList {alData :: [(k, v)]}
 deriving (Eq,Show)

alAdd :: WordLemma -> PartOfSpeech -> Word 
      -> Dictionary 
      -> Dictionary 
alAdd wl pos w al1 = case lookup wl (alData al1) of
  Nothing -> AssocList ((wl, (pos,[w])) : alData al1)
  Just val -> AssocList (map change (alData al1))
    where
      change x@(xwl, (xpos, xws)) =
          if xwl == wl
          then
            if w `elem` xws
            then x
            else (xwl, (xpos, w:xws))
          else x

alFoldr :: ((k,a) -> b -> b) -> b -> AssocList k a -> b
alFoldr f acc al = go acc al 
  where
    go acc (AssocList []) = acc
    go acc (AssocList (x:xs)) = f x (go acc (AssocList xs))

type Dictionary = AssocList WordLemma (PartOfSpeech, [Word])

getDictWords :: WordLemma -> Dictionary -> [Word]
getDictWords wl d = case lookup wl (alData d) of 
  Nothing -> ["NoSuchLemmaInDict"]
  Just (_,ww) -> ww

getDictPOS:: WordLemma -> Dictionary -> PartOfSpeech 
getDictPOS wl d = case lookup wl (alData d) of 
  Nothing -> Smth "NoSuchLemmaInDict"
  Just (pos,_) -> pos
