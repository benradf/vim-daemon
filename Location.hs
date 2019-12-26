{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Location
  ( ColumnNumber(..)
  , LineNumber(..)
  , Located(..)
  , Location(..)
  , Range(..)
  , Offset
  , makeLocatedString
  , tests
  , unLocated
  ) where

import Data.Coerce (coerce)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck
import qualified Test.QuickCheck as QuickCheck

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON

type Offset = Int

--type LineNumber = Int

newtype LineNumber = LineNumber Int
  deriving newtype (Num, Show, JSON.FromJSON, JSON.ToJSON)
  deriving stock (Eq, Ord)

instance Enum LineNumber where
  fromEnum = coerce
  toEnum = positive LineNumber

instance Bounded LineNumber where
  minBound = LineNumber 1
  maxBound = LineNumber maxBound

newtype ColumnNumber = ColumnNumber Int
  deriving newtype (Num, Show, JSON.FromJSON, JSON.ToJSON)
  deriving stock (Eq, Ord)

instance Enum ColumnNumber where
  fromEnum = coerce
  toEnum = positive ColumnNumber

instance Bounded ColumnNumber where
  minBound = ColumnNumber 1
  maxBound = ColumnNumber maxBound

positive :: (Int -> a) -> Int -> a
positive f n
    | n < 1 = error "LineNumber.toEnum: bad argument"
    | otherwise = f n


--type ColumnNumber = Int

data Location = Location LineNumber ColumnNumber
  deriving (Eq, Show)

data Located a = Located Location a
  deriving (Eq, Show)

instance Functor Located where
  fmap f (Located n x) = Located n (f x)

unLocated :: Located a -> a
unLocated (Located _ x) = x

type LocatedString = [Located Char]

makeLocatedString :: String -> LocatedString
makeLocatedString = zipWith Located $ Location 1 <$> [ 1 .. ]


data Range a = Range
  { rFrom :: a
  , rTo :: a
  }
  deriving (Eq, Show)



tests :: Tasty.TestTree
tests = Tasty.testGroup "module Location"
  [ Tasty.testGroup "QuickCheck" [ ]
  , Tasty.testGroup "Regression" [ ]
  , Tasty.testGroup "Unit" [ ]
  ]
