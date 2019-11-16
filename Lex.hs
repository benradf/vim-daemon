{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Lex
  ( Offset
  , Located
  , LexTree
  , makeLexTree
  , makeLocatedString
  , runLexer
  , tests
  , unLocated
  ) where

import Control.Applicative (Alternative(..), (<|>))
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow ((&&&))
import Control.Monad (guard, join, when)
import Control.Monad.State (State, get, gets, modify, put)
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
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck
import qualified Test.QuickCheck as QuickCheck

import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Tree.Pretty as Pretty

import Stream (Stream(..))
import qualified Stream as Stream


newtype TokenList a = TokenList
  { unTokenString :: [a]
  }

instance Show a => Show (TokenList a) where
  show (TokenList ts) = ts >>= show

prop_LexerWorks :: [Token] -> Bool
prop_LexerWorks tokens =
  let string = show =<< tokens
  in (lexer string >>= show . unLocated) == string

instance QuickCheck.Arbitrary Token where
  arbitrary = QuickCheck.arbitraryBoundedEnum


tests :: Tasty.TestTree
tests = Tasty.testGroup "module Lex"
  [ Tasty.testGroup "QuickCheck"
    [ QuickCheck.testProperty "Lexer Works" prop_LexerWorks
    ]

  , Tasty.testGroup "Regression"
    [ HUnit.testCase "Extra character consumed when one token is a prefix of another" $ do
        let string = "<-=>"
        let lexTree = makeLexTree $ Map.fromList $ join (,) <$> [ "<-", "<--", "=>" ]
        let result = concat $ unLocated <$> runLexer lexTree (makeLocatedString string)
        HUnit.assertEqual "Lexed correctly" string result
    ]
  ]

{-
    Consider rewriting LexTree data type to be:
        data LexTree a
            = ConsumeChar (Char -> LexTree a)
            | YieldToken a
            | NoToken
    as it originally was.
    Currently the tree can express ambiguity (such
    as when distinct tokens are represented by the
    same string).
    However the makeLexTree function will never
    produce a LexTree that exercises this ambiguity
    because its input type prevents it from receiving
    distinct tokens with the same string representation.

    Perhaps better still:
        data LexTree a
            = LexTree (Maybe (Char -> LexTree a)) (Maybe a)

    The runLexer function could probably be simplified
    if its LexTree input type was more restricted like this.
-}

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

type StringStream m = Stream m (Located Char)


runLexer :: LexTree a -> LocatedString -> [Located a]
runLexer tree string = do
  guard $ not $ null string

  case runStateT attempt (pure tree, string) of
    Just (token, (_, string')) ->
      (token <$ head string) : runLexer tree string'

    Nothing ->
      runLexer tree $ tail string

  where
    attempt :: StateT ([LexTree a], LocatedString) Maybe a
    attempt = gets fst >>= \case
      [] -> empty
      trees -> step <|> lift (listToMaybe $ mapMaybe fromYield trees)

    step = gets (second NonEmpty.nonEmpty) >>= \case
      (trees, Just (Located _ c :| cs)) ->
        put (runLexTree c =<< trees, cs) *> attempt
      (_, Nothing) -> empty

    runLexTree c = \case
      ConsumeChar f -> f c
      _ -> []

    fromYield = \case
      YieldToken t -> Just t
      _ -> Nothing


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

lexer :: String -> [Located Token]
lexer = runLexer lexTree . makeLocatedString


-- Idea For Another Vim Service / Plugin:
-- Sorting of comma separated fields within parentheses.

(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as

infixl 1 <&>
