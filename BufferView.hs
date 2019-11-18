{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module BufferView
  ( exampleLines
  , makeStreamPair
  , makeStreamPairFromLines
  , tests
  ) where

import Location (LineNumber, Located(..), Location(..), Offset, unLocated)
import Stream (Stream)
import qualified Stream as Stream
import Control.Monad (guard, join)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (Bifunctor(..), bimap)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Vector as Vector
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..), listToMaybe)

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck


data Bistream m a b
  = Bistream (Stream m a) (Stream m b)

instance Functor m => Bifunctor (Bistream m) where
  bimap f g (Bistream s t) = Bistream (f <$> s) (g <$> t)

type Line = String


data BufferView m = BufferView
  { bvBefore :: Stream m (Located Char)
  , bvAfter :: Stream m (Located Char)
-- TODO: Add a function to request cached lines to BufferView.
  }


makeBufferView
  :: MonadIO m
  => Location
  -> (LineNumber -> LineNumber -> m [Line])
  -> m (BufferView m)

makeBufferView cursor@(Location lineNum columnNum) getLines = do

  cache <- liftIO $ newIORef $ IntMap.empty

  -- TODO: Make the split function zip the column number with each character.
  let toCharStream = Stream.split $ \(n, line) -> NonEmpty.fromList $
        zipWith (Located . Location n) [ 1 .. ] $ line ++ "\n"
  let toCharStream2 = Stream.split $ \(n, line) -> NonEmpty.fromList $
        zipWith (Located . Location n) [ 1 .. ] $ reverse $ line ++ "\n"

  (before, after) <- bimap toCharStream2 toCharStream <$> makeStreamPair lineNum (getLinesViaCache cache)

  pure $ BufferView
    { bvBefore = before
    , bvAfter = after
    }

  where
    getLinesViaCache cache from to = do
      cached <- lookupRange from to <$> liftIO (readIORef cache)

      if length cached == from - to + 1
        then pure cached
        else do
          lines <- getLines from to
          liftIO $ modifyIORef cache $ addLinesToCache from lines
          pure lines

    lookupRange from to map =
      IntMap.elems $ fst $ IntMap.split (to + 1) $ snd $ IntMap.split from $ map

    addLinesToCache from lines map =
      IntMap.fromAscList (zip [ from .. ] lines) `IntMap.union` map


makeStreamPair
  :: MonadIO m
  => Int
  -> (Int -> Int -> m [a])
  -> m (Stream m (Int, a), Stream m (Int, a))

makeStreamPair index getRange = do

  nextAfter <- liftIO $ newIORef $ index
  nextBefore <- liftIO $ newIORef $ index - 1

  let
    getAfter = do
      n <- liftIO $ readIORef nextAfter
      elems <- getRange n (n + chunkSize - 1)
      liftIO $ writeIORef nextAfter $ n + length elems
      pure $ zip [ n .. ] elems

    getBefore = do
      n <- liftIO $ readIORef nextBefore
      elems <- getRange (n - chunkSize + 1) n
      liftIO $ writeIORef nextBefore $ n - length elems
      pure $ reverse $ zip [ n - chunkSize + 1 .. ] elems

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
        bv <- makeBufferViewFromLines 13 exampleLines
        -- (streamBefore, streamAfter) <- makeStreamPairFromLines 13 exampleLines
        beforeLines <- Stream.toList $ unLocated <$> bvBefore bv
        afterLines <- Stream.toList $  unLocated <$> bvAfter bv
  -- TODO: deal with fact that we are now streaming CharS instead of LineS
        HUnit.assertEqual "" (unlines exampleLines) ((reverse beforeLines) ++ afterLines)
    ]
  ]


makeBufferViewFromLines
  :: MonadIO m
  => LineNumber
  -> [Line]
  -> m (BufferView m)

makeBufferViewFromLines cursorLineNum lines =
  let location = Location cursorLineNum 1
  in makeBufferView location $ \i j -> pure $ do
    let (from, to) = (max i 1, max j 0)
    guard (from <= to)
    take (to - from + 1) $ drop (from - 1) lines


-- TODO: Tag lines and chars with correct Located Location.
makeStreamPairFromLines
  :: MonadIO m
  => LineNumber
  -> [String]
  -> m (Stream m Line, Stream m Line)

makeStreamPairFromLines lineNum lines =
  fmap (join bimap $ fmap snd) $
    makeStreamPair lineNum $ \i j -> pure $ do
      let (from, to) = (max i 1, max j 0)
      guard (from <= to)
      take (to - from + 1) $ drop (from - 1) lines

exampleLines :: [String]
exampleLines =
  [ "{-  1 -} makeStreamPair"
  , "{-  2 -}   :: MonadIO m"
  , "{-  3 -}   => (LineNumber -> LineNumber -> m [Line])"
  , "{-  4 -}   -> Location"
  , "{-  5 -}   -> m (Stream m Line, Stream m Line)"
  , "{-  6 -} "
  , "{-  7 -} makeStreamPair getLines cursor@(Location line column) = do"
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
    λ :t join $ join bitraverse Stream.toList <$> makeStreamPair (\_ _ -> pure []) (Location 1 1)
    join $ join bitraverse Stream.toList <$> makeStreamPair (\_ _ -> pure []) (Location 1 1)
      :: MonadIO m => m ([Line], [Line])
-}
