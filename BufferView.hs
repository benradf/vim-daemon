{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module BufferView
  ( BufferView(..)
  , exampleLines
  , makeBufferView
  , makeBufferViewFromLines
  , makeStreamPair
  , tests
  ) where

import Location (LineNumber, Located(..), Location(..), Offset, unLocated)
import Stream (Stream)
import qualified Stream as Stream
import Control.Arrow ((&&&))
import Control.Monad (guard, join)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (Bifunctor(..), bimap)
import Data.Foldable (foldMap)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..), (<>))
import qualified Data.Vector as Vector
import qualified Data.List.NonEmpty as NonEmpty
import Data.Function ((&))
import Data.Functor (void)
import Data.Maybe (Maybe(..), listToMaybe)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Streaming.Prelude (Of)

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck

import Debug.Trace (trace)


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
  -> (LineNumber -> LineNumber -> m [(LineNumber, Line)])
  -> m (BufferView m)

makeBufferView cursor@(Location lineNum columnNum) getLines = do

  cache <- liftIO $ newIORef $ IntMap.empty

  let toCharStream f = Stream.split $ \(n, line) -> NonEmpty.fromList $ f $
        zipWith (Located . Location n) [ 1 .. length line + 1 ] $ line ++ "\n"

  lhs <- makeStream lineNum (advance 2 pred &&& advance 3 pred) (flip compare) (getLinesViaCache cache)
  rhs <- makeStream lineNum (advance 2 succ &&& advance 3 pred) compare (getLinesViaCache cache)

  bistream <- bimap (toCharStream reverse) (toCharStream id) <$>
              pure (lhs, rhs)
              --makeStreamPair lineNum (getLinesViaCache cache)

  (before, after) <- fromMaybe bistream <$> Stream.seek (columnNum - 1) bistream

  pure $ BufferView
    { bvBefore = before
    , bvAfter = after
    }

  where
    advance n = appEndo . foldMap Endo . replicate n

    getLinesViaCache cache from to = do
      cached <- lookupRange from to <$> liftIO (readIORef cache)

      if length cached == from - to + 1
        then pure cached
        else do
          lines <- getLines from to
          liftIO $ modifyIORef cache $ addLinesToCache lines
          pure lines

    lookupRange from to map =
      IntMap.toAscList $ fst $ IntMap.split (to + 1) $ snd $ IntMap.split from $ map

    addLinesToCache lines map =
      IntMap.fromAscList lines `IntMap.union` map


--    getNext next advance =
--      liftIO (readIORef next) >>= \case
--        Nothing -> pure []
--        Just i -> do
--          let j = pred $ advance i
--          elems <- getRange i j
--          liftIO $ writeIORef next $
--            if map fst elems == [ i .. j ]
--              then Just $ succ j
--              else Nothing
--          pure elems


makeStream
  :: (Enum a, Eq a, Ord a, Show a, Show b, MonadIO m) -- Remove `Show a` and `Show b`
  => a
  -> (a -> (a, a))
  -> (a -> a -> Ordering)
  -> (a -> a -> m [(a, b)])
  -> m (Stream m (a, b))

makeStream index advance compare getRange = do
  next <- liftIO $ newIORef $ Just index

  Stream.fromAction $
    liftIO (readIORef next) >>= \case
      Nothing -> pure []

      Just i -> do
        let (j, i') = advance i
        elems <- getRange (min i j) (max i j)

        liftIO $ writeIORef next $
          if map fst elems == [ min i j .. max i j ]
            then Just i'
            else trace ("Nothing for i = " ++ show i ++ ", j = " ++ show j ++ "\n  map fst elems = " ++ show (map fst elems) ++ ", [ min i j .. max i j ] = " ++ show [ min i j .. max i j ] ++ ", elems = " ++ show elems) Nothing

        pure $ sortBy (\(i, _) (j, _) -> compare i j) elems


makeStream2
  :: (Enum a, Eq a, Ord a, Show a, Show b, MonadIO m) -- Remove `Show a` and `Show b`
  => a
  -> (a -> (a, a))
  -> (a -> a -> Ordering)
  -> (a -> a -> m [(a, b)])
  -> S.Stream (Of (a, b)) m ()

makeStream2 index advance compare getRange = do
  next <- liftIO $ newIORef $ Just index

  loop next

  where
    loop next = S.effect $ liftIO (readIORef next) >>= \case
      Nothing -> pure mempty
      Just i -> do
        let (j, i') = advance i
        elems <- getRange (min i j) (max i j)

        liftIO $ writeIORef next $
          if map fst elems == [ min i j .. max i j ]
            then Just i'
            else trace ("Nothing for i = " ++ show i ++ ", j = " ++ show j ++ "\n  map fst elems = " ++ show (map fst elems) ++ ", [ min i j .. max i j ] = " ++ show [ min i j .. max i j ] ++ ", elems = " ++ show elems) Nothing

        let sortedElems = sortBy (\(i, _) (j, _) -> compare i j) elems

        pure $ S.each sortedElems <> loop next


-- TODO: Use `Enum a` instead of `Int` here?
makeStreamPair
  :: MonadIO m
  => Int
  -> (Int -> Int -> m [(Int, a)])
  -> m (Stream m (Int, a), Stream m (Int, a))

makeStreamPair index getRange = do

  nextAfter <- liftIO $ newIORef $ index
  nextBefore <- liftIO $ newIORef $ index - 1

  let
    getNext2 next advance =
      liftIO (readIORef next) >>= \case
        Nothing -> pure []
        Just i -> do
          let j = pred $ advance i
          elems <- getRange i j
          liftIO $ writeIORef next $
            if map fst elems == [ i .. j ]
              then Just $ succ j
              else Nothing
          pure elems

    getAfter = do
      n <- liftIO $ readIORef nextAfter
      elems <- getRange n (n + chunkSize - 1)
      liftIO $ writeIORef nextAfter $ n + length elems
      pure elems

    getBefore = do
      n <- liftIO $ readIORef nextBefore
      elems <- getRange (n - chunkSize + 1) n
      liftIO $ writeIORef nextBefore $ n - length elems
      pure $ reverse elems

-- TODO: Fix negative range problems etc

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

    {-, HUnit.testCase "Stream lines before and after cursor" $ do
        bv <- makeBufferViewFromLines (Location 14 21) exampleLines
        beforeLines <- Stream.toList $ unLocated <$> bvBefore bv
        afterLines <- Stream.toList $  unLocated <$> bvAfter bv
  -- TODO: deal with fact that we are now streaming CharS instead of LineS
        HUnit.assertEqual "" (unlines exampleLines) ((reverse beforeLines) ++ afterLines) -}
    ]
  ]


makeBufferViewFromLines
  :: MonadIO m
  => Location
  -> [Line]
  -> m (BufferView m)

makeBufferViewFromLines cursorLocation lines =
  makeBufferView cursorLocation $ \i j -> pure $ do
    trace ("\x1b[33m[ " ++ show i ++ " .. " ++ show j ++ " ]\x1b[0m") (pure ())
    let (from, to) = (max i 1, max j 0)
    trace ("\x1b[1;33m[ " ++ show from ++ " .. " ++ show to ++ " ]\x1b[0m") (pure ())
    guard (from <= to)
    take (to - from + 1) $ drop (from - 1) $ zip [ 1 .. ] lines


-- TODO: Tag lines and chars with correct Located Location.
--makeStreamPairFromLines
--  :: MonadIO m
--  => LineNumber
--  -> [String]
--  -> m (Stream m Line, Stream m Line)
--
--makeStreamPairFromLines lineNum lines =
--  fmap (join bimap $ fmap snd) $
--    makeStreamPair lineNum $ \i j -> pure $ do
--      let (from, to) = (max i 1, max j 0)
--        guard (from <= to)
--      take (to - from + 1) $ drop (from - 1) lines

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
