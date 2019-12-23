{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Lex
  ( LexTree
  , Located(..)  -- TODO: Do not export data constructors.
  , LocatedStream
  , StringStream
  , makeLexTree
  , makeLocatedString
  , makeStringStream
  , runLexer
  , tests
  , unLocated
  ) where

import Control.Applicative (Alternative(..), (<|>))
import Control.Error.Util (hoistMaybe)
import Control.Monad.State.Class (MonadState(..))
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow ((&&&))
import Control.Monad (guard, join, when)
import Control.Monad.State (State, get, gets, modify, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
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
import qualified Streaming.Prelude as Streaming
--import qualified Streaming as Streaming

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck
import qualified Test.QuickCheck as QuickCheck

import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Tree.Pretty as Pretty

import Location (Located(..), Location(..), makeLocatedString, unLocated)
import Stream (Stream(..))
import qualified Stream as Stream


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


type LocatedStream m a = Stream m (Located a)

type StringStream m = Stream m (Located Char)

makeStringStream :: String -> StringStream Identity
makeStringStream = Stream.fromList . makeLocatedString


runLexer :: Monad m => LexTree a -> StringStream m -> m (LocatedStream m a)
runLexer tree stream =
  Stream.extract stream >>= \case
    Nothing -> pure mempty

    Just (char, stream') ->
      runMaybeT (runStateT attempt (pure tree, stream)) >>= \case
        Just (token, (_, stream')) ->
          pure $ Stream (token <$ char) (runLexer tree stream')

        Nothing ->
          runLexer tree stream'

  where
    attempt :: Monad m => StateT ([LexTree a], StringStream m) (MaybeT m) a
    attempt = gets fst >>= \case
      [] -> empty
      trees -> step <|> yielded trees

    step :: Monad m => StateT ([LexTree a], StringStream m) (MaybeT m) a
    step = (join $ gets $ lift . lift . traverse Stream.extract) >>= \case
      (trees, Just (Located _ c, cs)) ->
        put (runLexTree c =<< trees, cs) *> attempt
      (_, Nothing) -> empty

    yielded trees = lift
      $ hoistMaybe $ listToMaybe
      $ flip mapMaybe trees $ \case
        YieldToken t -> Just t
        _ -> Nothing

    runLexTree c = \case
      ConsumeChar f -> f c
      _ -> []


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
lexer = runIdentity . Stream.toList . runIdentity . runLexer lexTree . makeStringStream


-- Idea For Another Vim Service / Plugin:
-- Sorting of comma separated fields within parentheses.

newtype TokenList a = TokenList
  { unTokenString :: [a]
  }

instance Show a => Show (TokenList a) where
  show (TokenList ts) = ts >>= show

prop_StringWithOnlyTokensLexedCorrectly :: [Token] -> Bool
prop_StringWithOnlyTokensLexedCorrectly tokens =
  let string = show =<< tokens
  in (lexer string >>= show . unLocated) == string

instance QuickCheck.Arbitrary Token where
  arbitrary = QuickCheck.arbitraryBoundedEnum


tests :: Tasty.TestTree
tests = Tasty.testGroup "module Lex"
  [ Tasty.testGroup "QuickCheck"
    [ QuickCheck.testProperty
        "String with only tokens is lexed correctly"
        prop_StringWithOnlyTokensLexedCorrectly
    ]

  , Tasty.testGroup "Regression"
    [ HUnit.testCase "Extra character consumed when one token is a prefix of another" $ do
        let string = "<-=>"
        let lexTree = makeLexTree $ Map.fromList $ join (,) <$> [ "<-", "<--", "=>" ]
        let result = concat $ runIdentity $ Stream.toList $ unLocated <$> runIdentity (runLexer lexTree (makeStringStream string))
        HUnit.assertEqual "Lexed correctly" string result
    ]

  , Tasty.testGroup "Unit"
    [ HUnit.testCase "Infinite stream is lexed lazily" $ do
        infiniteTokenStream <- runLexer lexTree =<< Stream.fromAction (pure [ Located (Location 1 1) 'X' ])
        someTokens <- Stream.toList $ Stream.take 10 infiniteTokenStream
        HUnit.assertEqual "" (replicate 10 X) (unLocated <$> someTokens)
    ]
  ]


(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
infixl 1 <&>
