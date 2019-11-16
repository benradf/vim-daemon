{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Stream
  ( Stream
  , extract
  , fromAction
  , fromList
  , peek
  , prepend
  , split
  , tests
  , toList
  ) where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck

import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))


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

peek :: Applicative m => Stream m a -> m (Maybe a)
peek = \case
  Stream x _ -> pure $ Just x
  EndOfStream -> pure Nothing

prepend :: Applicative m => a -> Stream m a -> Stream m a
prepend x xs = Stream x (pure xs)

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
  elems -> (fromList elems <>) <$> fromAction action


prop_SplitWithNewlineIsUnlines :: [String] -> Bool
prop_SplitWithNewlineIsUnlines strings =
  let lineStream = fromList strings
      charStream = split (NonEmpty.fromList . (++ "\n")) lineStream
  in runIdentity (toList charStream) == unlines strings


tests :: Tasty.TestTree
tests = Tasty.testGroup "module Stream"
  [ Tasty.testGroup "QuickCheck"
    [ QuickCheck.testProperty "Split with newline is same as unlines function" prop_SplitWithNewlineIsUnlines
    ]

  , Tasty.testGroup "Regression" [ ]
  , Tasty.testGroup "Unit" [ ]
  ]


(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
infixl 1 <&>
