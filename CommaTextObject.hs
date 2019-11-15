module CommaTextObject where

import Lex
import qualified Data.Map as Map
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck


data Token
  = Comma
  | LeftParen
  | RightParen
  | LeftBox
  | RightBox
  | LeftBrace
  | RightBrace
  | CarriageReturn
  | LineFeed
  deriving Show

lexer :: String -> [Located Token]
lexer = runLexer lexTree . makeLocatedString
  where
    lexTree :: LexTree Token
    lexTree = makeLexTree $ Map.fromList
      [ (",", Comma)
      , ("(", LeftParen)
      , (")", RightParen)
      , ("[", LeftBox)
      , ("]", RightBox)
      , ("{", LeftBrace)
      , ("}", RightBrace)
      , ("\r", CarriageReturn)
      , ("\n", LineFeed)
      ]

tests :: Tasty.TestTree
tests = Tasty.testGroup "module CommaTextObject"
  [ Tasty.testGroup "QuickCheck"
    [ --QuickCheck.testProperty "Lexer Works" prop_LexerWorks
    ]

  , Tasty.testGroup "Regression"
    [ -- HUnit.testCase "Extra character consumed when one token is a prefix of another" $ pure ()
    ]
  ]

{-
data Location = Location
  { line :: Integer
  , column :: Integer
  }

data Range = Range
  { from :: Location
  , to :: Location
  }

--   ( x -> ( a -> b ) -> c -> ( d -> e -> f ) )

tokens =
  [ ("->", Arrow)
  , ("(", LeftParen)
  , (")", RightParen)
  , ("::", TypeAnnotation)
  , ("\n", NewLine)
  ]

tokenize :: [Char] -> [Token]

data Node
  = Leaf Range Range -- inner and outer ranges
  | Node [Node]

data Context
  = Root
  | Context [Node] Context [Node]

data Selection
  = Selection [Node] Context

extendLeft :: Selection -> Maybe Selection
extendLeft (Selection _ Root) = Nothing
extendLeft (Selection nodes (Context [] _ _)) = Nothing
extendLeft (Selection nodes (Context (l : ls) c rs)) =
  Just $ Selection (l : nodes) (Context ls c rs)

extendUp :: Selection -> Maybe Selection
extendUp (Selection _ Root) = Nothing
extendUp (Selection nodes (Context ls c rs)) =
  Just $ Selection (reverse ls ++ nodes ++ rs) c
-}
