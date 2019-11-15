module CommaTextObject where

import Lex
import qualified Data.Map as Map
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck
import Data.Maybe (Maybe(..), listToMaybe)


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

  , Tasty.testGroup "Unit"
    [ HUnit.testCase "Mapping string offset to line and column location" $ do
        let fromOffset = offsetToLocation [ "1st line", "line number II", "line #3" ]
        HUnit.assertEqual "" (Just (Location 1 1)) (fromOffset 0)
        HUnit.assertEqual "" (Just (Location 1 5)) (fromOffset 4)
        HUnit.assertEqual "" (Just (Location 1 9)) (fromOffset 8)
        HUnit.assertEqual "" (Just (Location 2 1)) (fromOffset 9)
        HUnit.assertEqual "" (Just (Location 2 15)) (fromOffset 23)
        HUnit.assertEqual "" (Just (Location 3 1)) (fromOffset 24)
        HUnit.assertEqual "" (Just (Location 3 8)) (fromOffset 31)
        HUnit.assertEqual "" Nothing (fromOffset 32)
        HUnit.assertEqual "" Nothing (fromOffset 99)
    ]
  ]


data LocalBuffer = LocalBuffer
  { lbLines :: [String]
  , lbGetLocation :: Int 
  }



offsetToLocation :: [String] -> Offset -> Maybe Location
offsetToLocation lines offset = do
  let lineIndexPairs = zip [ 0 .. ] $ scanl (\n line -> n + length line + 1) 0 lines
  (lineNumber, lineOffset) <- listToMaybe $ dropWhile ((<= offset) . snd) lineIndexPairs
  pure $ Location
    { lLine = lineNumber
    , lColumn = offset - snd (lineIndexPairs !! (lineNumber - 1)) + 1
    }
    


makeLocalBuffer :: [String] -> LocalBuffer
makeLocalBuffer = undefined


--indexToLocation :: Integral a => a -> Location
--indexToLocation n = 

data Location = Location
  { lLine :: Int
  , lColumn :: Int
  }
  deriving (Eq, Show)

data Range = Range
  { rFrom :: Location
  , rTo :: Location
  }
  deriving (Eq, Show)

--   ( x -> ( a -> b ) -> c -> ( d -> e -> f ) )

{-
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
