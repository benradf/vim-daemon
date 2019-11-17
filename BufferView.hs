module BufferView
  ( Location(..)
  , exampleLines
  , makeBufferView
  , makeBufferViewFromLines
  , tests
  ) where

import Lex (Offset)
import Stream (Stream)
import qualified Stream as Stream
import Control.Monad (guard, join)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (Bifunctor(..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..), listToMaybe)

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck


data Bistream m a b
  = Bistream (Stream m a) (Stream m b)

instance Functor m => Bifunctor (Bistream m) where
  bimap f g (Bistream s t) = Bistream (f <$> s) (g <$> t)


type LineNumber = Int
--newtype LineNumber = LineNumber Int
--  deriving (Eq, Num, Show)

type ColumnNumber = Int
--newtype ColumnNumber = ColumnNumber Int
--  deriving (Eq, Num, Show)

data Location = Location LineNumber ColumnNumber
  deriving (Eq, Show)


data Range a = Range
  { rFrom :: a
  , rTo :: a
  }
  deriving (Eq, Show)

type Line = String


data BufferView m = BufferView
  { bvWindow :: [Char]
  , bvBefore :: Stream m Char
  , bvAfter :: Stream m Char
  }


makeBufferView
  :: MonadIO m
  => Location
  -> (LineNumber -> LineNumber -> m [Line])
  -> m (Stream m Line, Stream m Line)

makeBufferView cursor@(Location line column) getLines = do

  nextAfter <- liftIO $ newIORef $ line
  nextBefore <- liftIO $ newIORef $ line - 1

  let
    getAfter = do
      n <- liftIO $ readIORef nextAfter
      lines <- getLines n (n + chunkSize - 1)
      liftIO $ writeIORef nextAfter $ n + length lines
      pure lines

    getBefore = do
      n <- liftIO $ readIORef nextBefore
      lines <- getLines (n - chunkSize + 1) n
      liftIO $ writeIORef nextBefore $ n - length lines
      pure $ reverse $ reverse <$> lines

  (,)
    <$> Stream.fromAction getBefore
    <*> Stream.fromAction getAfter

  where
    chunkSize = 5


newtype ReversedString = ReversedString String
  deriving (Eq, Show)

offsetToLocation :: [String] -> Offset -> Maybe Location
offsetToLocation lines offset = do
  let lineIndexPairs = zip [ 0 .. ] $ scanl (\n line -> n + length line + 1) 0 lines
  (lineNumber, lineOffset) <- listToMaybe $ dropWhile ((<= offset) . snd) lineIndexPairs
  pure $ Location
    (lineNumber)
    (offset - snd (lineIndexPairs !! (lineNumber - 1)) + 1)


tests :: Tasty.TestTree
tests = Tasty.testGroup "module BufferView"
  [ Tasty.testGroup "QuickCheck" [ ]
  , Tasty.testGroup "Regression" [ ]

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

    , HUnit.testCase "Stream lines before and after cursor" $ do
        let cursor = Location 13 5
        (streamBefore, streamAfter) <- makeBufferViewFromLines cursor exampleLines
        beforeLines <- Stream.toList streamBefore
        afterLines <- Stream.toList streamAfter
        HUnit.assertEqual "" exampleLines ((reverse <$> reverse beforeLines) ++ afterLines)
    ]
  ]


-- TODO: Tag lines and chars with correct Located Location.
makeBufferViewFromLines
  :: MonadIO m
  => Location
  -> [String]
  -> m (Stream m Line, Stream m Line)

makeBufferViewFromLines cursor lines =
  makeBufferView cursor $ \i j -> pure $ do
    let (from, to) = (max i 1, max j 0)
    guard (from <= to)
    take (to - from + 1) $ drop (from - 1) lines

exampleLines :: [String]
exampleLines =
  [ "{-  1 -} makeBufferView"
  , "{-  2 -}   :: MonadIO m"
  , "{-  3 -}   => (LineNumber -> LineNumber -> m [Line])"
  , "{-  4 -}   -> Location"
  , "{-  5 -}   -> m (Stream m Line, Stream m Line)"
  , "{-  6 -} "
  , "{-  7 -} makeBufferView getLines cursor@(Location line column) = do"
  , "{-  8 -} "
  , "{-  9 -}   nextAfter <- liftIO $ newIORef $ line + 1"
  , "{- 10 -}   nextBefore <- liftIO $ newIORef $ line - 1"
  , "{- 11 -} "
  , "{- 12 -}   let"
  , "{- 13 -}     getAfter = do"
  , "{- 14 -}       n <- liftIO $ readIORef nextAfter"
  , "{- 15 -}       lines <- getLines n (n + chunkSize - 1)"
  , "{- 16 -}       liftIO $ writeIORef nextAfter $ n + length lines"
  , "{- 17 -}       pure lines"
  , "{- 18 -} "
  , "{- 19 -}     getBefore = do"
  , "{- 20 -}       n <- liftIO $ readIORef nextBefore"
  , "{- 21 -}       lines <- getLines (n - chunkSize + 1) n"
  , "{- 22 -}       liftIO $ writeIORef nextBefore $ n - length lines"
  , "{- 23 -}       pure lines"
  , "{- 24 -} "
  , "{- 25 -}   (,)"
  , "{- 26 -}     <$> Stream.fromAction getBefore"
  , "{- 27 -}     <*> Stream.fromAction getAfter"
  , "{- 28 -} "
  , "{- 29 -}   where"
  , "{- 30 -}     chunkSize = 5"
  ]


--indexToLocation :: Integral a => a -> Location
--indexToLocation n = 

--data BufferView m = BufferView
--  { bvLineCount :: Int
--  , bvCursor :: Location
--  , bvLinesBefore :: Stream m String
--  , bvLinesAfter :: Stream m String
--  }


--data Selection a
--  = Selection [a] (Context a)
--
--data Context a
--  = NoContext
--  | Context [a] (Context a) [a]


--data BufOp



--data Selection m a
--  = Selection (Stream m a) [a] (Stream m a)


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



{-
    λ :t join $ join bitraverse Stream.toList <$> makeBufferView (\_ _ -> pure []) (Location 1 1)
    join $ join bitraverse Stream.toList <$> makeBufferView (\_ _ -> pure []) (Location 1 1)
      :: MonadIO m => m ([Line], [Line])
-}