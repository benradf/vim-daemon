module Location
  ( ColumnNumber
  , LineNumber
  , Located(..)
  , Location(..)
  , Range(..)
  , Offset
  , makeLocatedString
  , tests
  , unLocated
  ) where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck
import qualified Test.QuickCheck as QuickCheck


type Offset = Int

data Located a = Located Offset a
  deriving Show

instance Functor Located where
  fmap f (Located n x) = Located n (f x)

unLocated :: Located a -> a
unLocated (Located _ x) = x

type LocatedString = [Located Char]

makeLocatedString :: String -> LocatedString
makeLocatedString = zipWith Located [ 0 .. ]


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



tests :: Tasty.TestTree
tests = Tasty.testGroup "module Location"
  [ Tasty.testGroup "QuickCheck" [ ]
  , Tasty.testGroup "Regression" [ ]
  , Tasty.testGroup "Unit" [ ]
  ]
