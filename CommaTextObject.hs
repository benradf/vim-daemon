{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module CommaTextObject where

import Lex
import Stream (Stream)
import qualified Stream as Stream
import qualified Data.Map as Map
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Data.Maybe (Maybe(..), listToMaybe)
import Data.Functor.Identity (Identity(..))


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
lexer = runIdentity . runLexer lexTree . makeStringStream
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


offsetToLocation :: [String] -> Offset -> Maybe Location
offsetToLocation lines offset = do
  let lineIndexPairs = zip [ 0 .. ] $ scanl (\n line -> n + length line + 1) 0 lines
  (lineNumber, lineOffset) <- listToMaybe $ dropWhile ((<= offset) . snd) lineIndexPairs
  pure $ Location
    { lLine = LineNumber lineNumber
    , lColumn = ColumnNumber $ offset - snd (lineIndexPairs !! (lineNumber - 1)) + 1
    }
    

newtype ReversedString = ReversedString String
  deriving (Eq, Show)


data LineBuffer2 = LineBuffer2
  { lbLines :: [String]
  , lbSourceRange :: Range LineNumber
  , lbGetLocation :: Offset -> Maybe Location
  }


makeLineBuffer :: [String] -> LineBuffer2
makeLineBuffer = undefined


--indexToLocation :: Integral a => a -> Location
--indexToLocation n = 


newtype LineNumber = LineNumber Int
  deriving (Eq, Num, Show)

newtype ColumnNumber = ColumnNumber Int
  deriving (Eq, Num, Show)


data Location = Location
  { lLine :: LineNumber
  , lColumn :: ColumnNumber
  }
  deriving (Eq, Show)

data Range a = Range
  { rFrom :: a
  , rTo :: a
  }
  deriving (Eq, Show)



--data BufOp



--data Selection m a
--  = Selection (Stream m a) [a] (Stream m a)


type Line = String

makeBufferView
  :: (LineNumber -> LineNumber -> m [Line])
  -> Location
  -> IO (Stream m Line, Stream m Line)

makeBufferView getLines cursor = do
  let lineNum = lLine cursor
  mvar <- liftIO $ newMVar (lineNum - 1, lineNum)

  nextAfter <- liftIO $ newMVar lineNum
  nextBefore <- liftIO $ newMVar (lineNum - 1)


-- modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b

  let {-getAfter = do
        n <- snd <$> liftIO (readIORef ref)
        getLines n (n + 4)
        undefined-}


      -- NOTE: Cannot use modifyMVar because can't unliftIO the Monad m
      getAfter = liftIO $ modifyMVar nextAfter $ \n ->
        (n + 5,) <$> getLines n (n + 4)

      getBefore = do
        undefined

  (,)
    <$> Stream.fromAction getBefore
    <*> Stream.fromAction getAfter


data BufferView m = BufferView
  { bvLineCount :: Int
  , bvCursor :: Location
  , bvLinesBefore :: Stream m String
  , bvLinesAfter :: Stream m String
  }


--data Selection a
--  = Selection [a] (Context a)
--
--data Context a
--  = NoContext
--  | Context [a] (Context a) [a]



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


tests :: Tasty.TestTree
tests = Tasty.testGroup "module CommaTextObject"
  [ Tasty.testGroup "QuickCheck" [ ]
  , Tasty.testGroup "Regression" [ ]

  , Tasty.testGroup "Unit"
    [ HUnit.testCase "Mapping string offset to line and column location" $ do
        let fromOffset = offsetToLocation [ "1st line", "line number II", "line #3" ]
        HUnit.assertEqual "" (Just (Location (LineNumber 1) (ColumnNumber 1))) (fromOffset 0)
        HUnit.assertEqual "" (Just (Location (LineNumber 1) (ColumnNumber 5))) (fromOffset 4)
        HUnit.assertEqual "" (Just (Location (LineNumber 1) (ColumnNumber 9))) (fromOffset 8)
        HUnit.assertEqual "" (Just (Location (LineNumber 2) (ColumnNumber 1))) (fromOffset 9)
        HUnit.assertEqual "" (Just (Location (LineNumber 2) (ColumnNumber 15))) (fromOffset 23)
        HUnit.assertEqual "" (Just (Location (LineNumber 3) (ColumnNumber 1))) (fromOffset 24)
        HUnit.assertEqual "" (Just (Location (LineNumber 3) (ColumnNumber 8))) (fromOffset 31)
        HUnit.assertEqual "" Nothing (fromOffset 32)
        HUnit.assertEqual "" Nothing (fromOffset 99)
    ]
  ]
