{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Stream
  ( Stream(..)  -- TODO: do not export constructors
  , drop
  , extract
  , fromAction
  , fromList
  , mapStream
  , peek
  , prepend
  , seek
  , split
  , take
  , tests
  , toList
  ) where

import Control.Monad.State (get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), execStateT)
import Data.Foldable (and)
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isNothing)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Tuple (swap)
import qualified Prelude as Prelude
import Prelude hiding (drop, take)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.QuickCheck as QuickCheck


data Stream m a
  = Stream a (m (Stream m a))
  | EndOfStream

instance Functor m => Semigroup (Stream m a) where
  (<>) = curry $ \case
    (EndOfStream, rhs) -> rhs
    (Stream x xs, rhs) -> Stream x $ xs <&> (<> rhs)

instance Functor m => Monoid (Stream m a) where
  mempty = EndOfStream
  mappend = (<>)

instance Functor m => Functor (Stream m) where
  fmap f (Stream x xs) = Stream (f x) (fmap f <$> xs)
  fmap _ EndOfStream = EndOfStream

instance Applicative m => Applicative (Stream m) where
  pure x = Stream x (pure EndOfStream)
  (<*>) = curry $ \case
    (lhs@(Stream f fs), rhs@(Stream x xs)) ->
      let g lhs' rhs' = (lhs' <*> rhs) <> (lhs <*> rhs')
      in  Stream (f x) (g <$> fs <*> xs)
    (_, EndOfStream) -> EndOfStream
    (EndOfStream, _) -> EndOfStream

extract :: Applicative m => Stream m a -> m (Maybe (a, Stream m a))
extract = \case
  Stream x xs -> Just . (x,) <$> xs
  EndOfStream -> pure Nothing

mapStream :: Monad m => s -> (s -> a -> (s, [b])) -> Stream m a -> m (Stream m b)
mapStream s f = \case
  Stream x xs ->
    let (s', ys) = f s x
    in (fromList ys <> ) <$> (mapStream s' f =<< xs)
  EndOfStream -> pure EndOfStream

peek :: Applicative m => Stream m a -> m (Maybe a)
peek = \case
  Stream x _ -> pure $ Just x
  EndOfStream -> pure Nothing

prepend :: Applicative m => a -> Stream m a -> Stream m a
prepend x xs = Stream x (pure xs)

drop :: Monad m => Int -> Stream m a -> m (Stream m a)
drop = curry $ \case
  (_, EndOfStream) -> pure EndOfStream
  (n, stream@(Stream x xs))
    | n <= 0 -> pure stream
    | otherwise -> drop (n - 1) =<< xs

take :: Monad m => Int -> Stream m a -> Stream m a
take = curry $ \case
  (_, EndOfStream) -> EndOfStream
  (n, Stream x xs)
    | n <= 0 -> EndOfStream
    | otherwise -> Stream x $ take (n - 1) <$> xs

type Bistream m a = (Stream m a, Stream m a)

seek :: Monad m => Int -> Bistream m a -> m (Maybe (Bistream m a))
seek n
  | n > 0 = seekMany n
  | n < 0 = seekMany (abs n) . swap
  | otherwise = pure . Just

  where
    seekMany :: Monad m => Int -> Bistream m a -> m (Maybe (Bistream m a))
    seekMany n = runMaybeT . execStateT (foldr1 (*>) (replicate n seekOne))

    seekOne :: Monad m => StateT (Bistream m a) (MaybeT m) ()
    seekOne = do
      (lhs, rhs) <- get
      (x, rhs') <- lift $ MaybeT $ Stream.extract rhs
      put (Stream.prepend x lhs, rhs')

fromList :: Applicative m => [a] -> Stream m a
fromList = \case
  x : xs -> Stream x (pure $ fromList xs)
  [] -> EndOfStream

toList :: Monad m => Stream m a -> m [a]
toList = \case
  Stream x xs -> (x :) <$> (toList =<< xs)
  EndOfStream -> pure []

split :: Applicative m => (a -> NonEmpty b) -> Stream m a -> Stream m b
split f = \case
  Stream x xs ->
    let y :| ys = f x
    in Stream y $ (fromList ys <>) . split f <$> xs
  EndOfStream -> mempty

fromAction :: Monad m => m [a] -> m (Stream m a)
fromAction action = action >>= \case
  [] -> pure EndOfStream
  elems -> foldr (fmap pure . Stream) (fromAction action) elems


prop_SplitWithNewlineIsUnlines :: [String] -> Bool
prop_SplitWithNewlineIsUnlines strings =
  let lineStream = fromList strings
      charStream = split (NonEmpty.fromList . (++ "\n")) lineStream
  in runIdentity (toList charStream) == unlines strings

prop_SeekLengthReversesStream :: String -> Bool
prop_SeekLengthReversesStream string = runIdentity $ do
  let bistream = (mempty, fromList string)
  seek (length string) bistream >>= \case
    Just (lhs, rhs) -> toList lhs <&> (== reverse string)
    Nothing -> pure False

prop_CannotSeekPastEndOfStream :: String -> Bool
prop_CannotSeekPastEndOfStream string = runIdentity $ do
  let stream = fromList string
      bistream = (stream, stream)
  and <$> sequenceA
    [ isNothing <$> seek (length string + 1) bistream
    , isNothing <$> seek (negate $ length string + 1) bistream
    ]

prop_StreamTakeListTakeEquivalence :: (String, Int) -> Bool
prop_StreamTakeListTakeEquivalence (string, n) = runIdentity $
  toList (Stream.take n (fromList string)) <&> (== Prelude.take n string)

prop_StreamDropListDropEquivalence :: (String, Int) -> Bool
prop_StreamDropListDropEquivalence (string, n) = runIdentity $
  (Stream.toList =<< Stream.drop n (fromList string)) <&> (== Prelude.drop n string)

-- TODO: QuickCheck test that `(MaybeT . seek n) <=< (MaybeT . seek (-n)) == id`

tests :: Tasty.TestTree
tests = Tasty.testGroup "module Stream"
  [ Tasty.testGroup "QuickCheck"
    [ QuickCheck.testProperty "Stream.take is equivalent to Data.List.take" prop_StreamTakeListTakeEquivalence
    , QuickCheck.testProperty "Stream.drop is equivalent to Data.List.drop" prop_StreamDropListDropEquivalence
    , QuickCheck.testProperty "Split with newline is same as unlines function" prop_SplitWithNewlineIsUnlines
    , QuickCheck.testProperty "Seeking to end reverses string" prop_SeekLengthReversesStream
    , QuickCheck.testProperty "Cannot seek past end of stream" prop_CannotSeekPastEndOfStream
    ]

  , Tasty.testGroup "Regression" [ ]
  , Tasty.testGroup "Unit" [ ]
  ]


(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
infixl 1 <&>
