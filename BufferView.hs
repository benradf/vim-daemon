{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module BufferView
  ( BufferView(..)
  , exampleLines
  , makeBufferView
  , makeBufferViewFromLines
  , tests
  ) where

import Control.Arrow ((&&&))
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Monad (guard, when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.IntMap as IntMap
import Data.List (sort)
import Data.Maybe (Maybe(..), listToMaybe)
import Data.Monoid ((<>))
import qualified Data.Vector.Mutable as MVector
import qualified Streaming as S
import Streaming.Prelude (Of, Stream)
import qualified Streaming.Prelude as S
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

import Location (ColumnNumber(..), LineNumber(..), Located(..), Location(..), Offset, unLocated)


--data Bistream m a b
--  = Bistream (Stream m a) (Stream m b)
--
--instance Functor m => Bifunctor (Bistream m) where
--  bimap f g (Bistream s t) = Bistream (f <$> s) (g <$> t)

type Line = String


data BufferView m = BufferView
  { bvBefore :: Stream (Of (Located Char)) m ()
  , bvAfter :: Stream (Of (Located Char)) m ()
-- TODO: Add a function to request cached lines to BufferView.
  }


type GetLines m = LineNumber -> LineNumber -> m [(LineNumber, Line)]  -- expects: i <= j


type ChunkSize = Int


makeBufferView :: MonadIO m => ChunkSize -> Location -> GetLines m -> m (BufferView m)
makeBufferView chunkSize cursor@(Location lineNum columnNum) getLines = do

  cache <- liftIO $ newIORef $ IntMap.empty

  let lhs = makeStream lineNum (advance chunkSize pred minBound) ((fmap . fmap . fmap) reverse . flip $ getLinesViaCache cache)
      rhs = makeStream lineNum (advance chunkSize succ maxBound) (getLinesViaCache cache)

  let mkLocated (i, (j, char)) = Located (Location i j) char
  let toCharStream f = S.concat . S.map (map mkLocated . f . traverse (zip [ 1 .. ] . (++ "\n")))

  before <- memoizeStream $ toCharStream reverse lhs
  after <- memoizeStream $ toCharStream id rhs

  pure $ BufferView
    { bvBefore = before
    , bvAfter = after
    }

  where
    advance :: Eq a => Int -> (a -> a) -> a -> a -> (a, Maybe a)
    advance n step end i
      | n <= 0 = (i, Just i)
      | i == end = (i, Nothing)
      | n == 1 = (i, Just (step i))
      | otherwise = advance (n - 1) step end (step i)

    getLinesViaCache cache from to = do
      let fromKey = coerce from
          toKey = coerce to

      cached <- lookupRange fromKey toKey <$> liftIO (readIORef cache)

      if length cached == toKey - fromKey + 1
        then pure $ first coerce <$> cached
        else do
          lines <- getLines from to
          liftIO $ modifyIORef cache $ addLinesToCache $ first coerce <$> lines
          pure lines

    lookupRange fromKey toKey map =
      IntMap.toAscList $ fst $ IntMap.split (toKey + 1) $ snd $ IntMap.split fromKey $ map

    addLinesToCache lines map =
      IntMap.fromAscList lines `IntMap.union` map


makeStream
  :: (Enum a, Eq a, Ord a, MonadIO m)
  => a
  -> (a -> (a, Maybe a))
  -> (a -> a -> m [(a, b)])
  -> Stream (Of (a, b)) m ()

makeStream index advance getRange = do
  next <- liftIO $ newIORef $ Just index

  loop next

  where
    loop next = S.effect $ liftIO (readIORef next) >>= \case
      Nothing -> pure mempty
      Just i -> do
        let (j, i') = advance i
        elems <- getRange i j

        liftIO $ writeIORef next $
          if sort (map fst elems) == [ min i j .. max i j ]
            then i'
            else Nothing

        pure $ S.each elems <> loop next


memoizeStream :: MonadIO m => Stream (Of a) m r -> m (Stream (Of a) m r)
memoizeStream stream = liftIO $ do

  vector <- MVector.new 1
  MVector.write vector 0 (Left stream)

  memo 0 <$> newMVar vector

  where
    memo i vectorRef = S.effect $ do
      vector <- liftIO $ readMVar vectorRef
      liftIO (MVector.read vector i) >>= \case

        Left stream ->
          S.next stream >>= \case
            Left r -> pure (pure r)

            Right (x, stream') -> liftIO $ do
              let len = MVector.length vector
              when (i + 1 == len) $
                modifyMVar_ vectorRef $ flip MVector.grow len

              vector <- readMVar vectorRef
              MVector.write vector i (Right x)
              MVector.write vector (i + 1) (Left stream')

              pure $ memo i vectorRef

        Right x -> pure $ do
          S.yield x
          memo (i + 1) vectorRef



newtype ReversedString = ReversedString String
  deriving (Eq, Show)

offsetToLocation :: [String] -> Offset -> Maybe Location
offsetToLocation lines offset = do
  let lineIndexPairs = zip [ 0 .. ] $ scanl (\n line -> n + length line + 1) 0 lines
  (lineNumber, lineOffset) <- listToMaybe $ dropWhile ((<= offset) . snd) lineIndexPairs
  pure $ Location
    (lineNumber)
    (coerce $ offset - snd (lineIndexPairs !! (coerce lineNumber - 1)) + 1)


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

    , HUnit.testCase "Streaming makes chunked requests for lines" $ do
        (requests, getLines) <- mockGetLines 20
        let location = Location (LineNumber 12) (ColumnNumber 1)
        bv <- makeBufferView 5 location getLines
        HUnit.assertEqual "before string incorrect"
          (reverse $ concat $ map (\n -> show n ++ "\n") [ 1 .. 12 ])
          =<< map unLocated <$> S.toList_ (bvBefore bv)
        HUnit.assertEqual "after string incorrect"
          (concat $ map (\n -> show n ++ "\n") [ 12 .. 20 ])
          =<< map unLocated <$> S.toList_ (bvAfter bv)
        HUnit.assertEqual "requested lines incorrect"
          [ (8, 12), (3, 7), (1, 2), (12, 16), (17, 21) ]
          =<< readIORef requests

    , HUnit.testCase "Lines are not requested multiple times" $ do
        (requests, getLines) <- mockGetLines 11
        let location = Location (LineNumber 9) (ColumnNumber 1)
        bv <- makeBufferView 2 location getLines
        S.toList_ (bvAfter bv)
        S.toList_ (bvBefore bv)
        S.toList_ (bvAfter bv)
        S.toList_ (bvBefore bv)
        HUnit.assertEqual ""
          [ (1, 1), (2, 3), (4, 5), (6, 7), (8, 9), (9, 10), (11, 12) ]
          =<< sort <$> readIORef requests
    ]
  ]


mockGetLines :: MonadIO m => Int -> m (IORef [(LineNumber, LineNumber)], GetLines m)
mockGetLines lineCount = do
  requestedRanges <- liftIO $ newIORef []

  pure $ (requestedRanges,) $ \i j ->
    if i < 0 || j < 0 
      then pure []
      else do
        let i' = max i 1
            j' = min j (coerce lineCount)
        liftIO $ modifyIORef requestedRanges (<> [(i, j)])
        pure $ (id &&& show) <$> [ i' .. j' ]




makeBufferViewFromLines
  :: MonadIO m
  => ChunkSize
  -> Location
  -> [Line]
  -> m (BufferView m)

makeBufferViewFromLines chunkSize cursorLocation lines =
  makeBufferView chunkSize cursorLocation $ \i j -> pure $ do
    --trace ("\x1b[33m[ " ++ show i ++ " .. " ++ show j ++ " ]\x1b[0m") (pure ())
    let (from, to) = (clamp (i :: LineNumber), clamp (j :: LineNumber))
    guard $ coerce i <= length lines && coerce j >= (1 :: Int)
    --trace ("\x1b[1;33m[ " ++ show from ++ " .. " ++ show to ++ " ]\x1b[0m") (pure ())
    take (coerce $ to - from + 1) $ drop (coerce $ from - 1) $ zip [ 1 .. ] lines
  where
    clamp n = coerce n `max` 1 `min` length lines


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
