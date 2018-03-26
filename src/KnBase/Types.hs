module KnBase.Types where

import Prelude hiding (Word)
import Text.Show.Unicode

type Word = String
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
