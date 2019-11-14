{-# LANGUAGE LambdaCase #-}

module Lex where

import Control.Applicative (Alternative(..))
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow ((&&&))
import Control.Monad (guard, join, when)
import Control.Monad.State (State, get, gets, modify, put, runState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..))
import Data.Bifunctor (bimap, first, second)
import Data.Bitraversable (bitraverse)
import Data.List (groupBy)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, maybeToList, mapMaybe)
import Data.Maybe (maybeToList)
import Data.Traversable (sequenceA)
import Data.Semigroup ((<>))
import Data.Tree (Tree(..))
import qualified Data.Tree as Tree

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.QuickCheck as Tasty
import qualified Test.QuickCheck as QuickCheck

import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Tree.Pretty as Pretty


newtype TokenList a = TokenList
  { unTokenString :: [a]
  }

instance Show a => Show (TokenList a) where
  show (TokenList ts) = ts >>= show

prop_LexerWorks :: [Token] -> Bool
prop_LexerWorks tokens =
  let string = show =<< tokens
  in (lexer lexTree string >>= show) == string

instance QuickCheck.Arbitrary Token where
  arbitrary = QuickCheck.arbitraryBoundedEnum


main :: IO ()
main = Tasty.defaultMain $
  Tasty.testGroup "Lexer Tests"
    [ Tasty.testProperty "Lexer Works" prop_LexerWorks
    ]


data LexTree a
  = ConsumeChar (Char -> [LexTree a])
  | YieldToken a


makeLexTree :: Map String a -> LexTree a
makeLexTree = go . Map.toList . Map.delete ""
  where
    go :: [(String, a)] -> LexTree a
    go
      = makeConsumeNode
      . map
        ( makeChildTrees
        . extractFirstChar
        )
      . groupByFirstChar
      . coerceNonEmpty

    makeConsumeNode :: [(Char, [LexTree a])] -> LexTree a
    makeConsumeNode choices = ConsumeChar $ \c ->
      join $ maybeToList $ snd <$> List.find ((c ==) . fst) choices

    makeChildTrees :: (Char, NonEmpty ([Char], a)) -> (Char, [LexTree a])
    makeChildTrees = fmap $ \case
      ([], t) :| [] ->
        [ YieldToken t ]
      elems ->
        let makeYieldLeaves = map $ YieldToken . snd
            splitOngoingFromFinished = NonEmpty.partition (not . null . fst)
        in uncurry (:) . bimap go makeYieldLeaves . splitOngoingFromFinished $ elems

    extractFirstChar :: NonEmpty (NonEmpty Char, a) -> (Char, NonEmpty ([Char], a))
    extractFirstChar group@((c :| _, _) :| _) = (c, NonEmpty.map (first NonEmpty.tail) group)

    groupByFirstChar :: [(NonEmpty Char, a)] -> [NonEmpty (NonEmpty Char, a)]
    groupByFirstChar = NonEmpty.groupBy $ curry $ uncurry (==) . join bimap (NonEmpty.head . fst)

    coerceNonEmpty :: [(String, a)] -> [(NonEmpty Char, a)]
    coerceNonEmpty = map (first NonEmpty.fromList)


lexer :: LexTree a -> String -> [a]
lexer tree string = do
  guard $ not $ null string

  case runStateT attempt (pure tree, string) of
    Just (token, (_, string')) -> token : lexer tree string'
    Nothing -> lexer tree $ tail string

  where
    attempt :: StateT ([LexTree a], String) Maybe a
    attempt = gets fst >>= \case
      YieldToken token : _ -> pure token
      [] -> empty
      _ -> step

    step = gets (second NonEmpty.nonEmpty) >>= \case
      (trees, Just (c :| cs)) ->
        put (runLexTree c =<< trees, cs) *> attempt
      (_, Nothing) -> empty

    runLexTree c = \case
      ConsumeChar f -> f c
      t@(YieldToken _) -> pure t


data Token = Alpha | Beta | Gamma | Delta | Epsilon | X
  deriving (Bounded, Enum, Eq, Ord, Show)

tokens :: [Token]
tokens = [ minBound .. maxBound ]

lexTree :: LexTree Token
lexTree
  = makeLexTree
  $ Map.fromList
  $ map (show &&& id)
  $ tokens


-- Idea For Another Vim Service / Plugin:
-- Sorting of comma separated fields within parentheses.
