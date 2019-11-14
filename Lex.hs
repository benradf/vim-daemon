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
  in (lexer testLexTree string >>= show) == string

instance QuickCheck.Arbitrary Token where
  arbitrary = QuickCheck.arbitraryBoundedEnum

--instance QuickCheck.Arbitrary a => QuickCheck.Arbitrary (LexTree a) where
--  arbitrary = QuickCheck.sized $ \case
--    0 -> YieldToken <$> QuickCheck.arbitrary
--    n -> do
--      QuickCheck.Positive m <- QuickCheck.arbitrary
--      undefined


main :: IO ()
main = Tasty.defaultMain $
  Tasty.testGroup "Lexer Tests"
    [ Tasty.testProperty "Lexer Works" prop_LexerWorks
    ]






data LexTree a
  = ConsumeChar (Char -> [LexTree a])
  | YieldToken a

instance Functor LexTree where
  fmap f = \case
    ConsumeChar g -> ConsumeChar (map (fmap f) . g)
    YieldToken x -> YieldToken (f x)

instance Applicative LexTree where
  pure = YieldToken
  (<*>) = curry $ \case
    (YieldToken f, YieldToken x) -> YieldToken (f x)
    (ConsumeChar f, x@(YieldToken _)) -> ConsumeChar $ \c -> [ f' <*> x | f' <- f c ]
    (f@(YieldToken _), ConsumeChar x) -> ConsumeChar $ \c -> [ f <*> x' | x' <- x c ]
    (ConsumeChar f, ConsumeChar x) -> ConsumeChar $ \c -> [ f' <*> x' | f' <- f c, x' <- x c ]

instance Monad LexTree where
  m >>= f =
    let join = \case
          YieldToken x -> x
          ConsumeChar g -> ConsumeChar $ fmap join . g
    in join $ fmap f m
      
instance Alternative LexTree where
  empty = ConsumeChar mempty
  x@(YieldToken _) <|> _ = x
  _ <|> y = y


lexTreeToTree
  :: (Bounded a, Enum a, Ord a, Show a)
  => [a] -> LexTree a -> Tree String

-- TODO: take title and create node one layer down

lexTreeToTree tokens = \case
  ConsumeChar f ->
    let g c = Tree.Node (show c) (lexTreeToTree tokens <$> f c)
    in Tree.Node "ConsumeChar" $ g <$> characters

  YieldToken a ->
    Tree.Node ("YieldToken '" <> show a <> "'") []

  where
    characters :: [Char]
    characters = List.nub $ show =<< tokens

makeLexTreeOld :: Show a => [a] -> Either String (LexTree a)
makeLexTreeOld = go . map (NonEmpty.fromList . show &&& id)
  where
    go :: [(NonEmpty Char, a)] -> Either String (LexTree a)
    go
      = fmap ConsumeChar 
      . fmap (\xs c -> join $ maybeToList $ snd <$> List.find ((c ==) . fst) xs)
      . sequenceA
      . map
        ( traverse
          ( {- fmap (uncurry $ flip (:))
          . bitraverse
              (Right . map (YieldToken . snd))
              (go . map (first NonEmpty.fromList))
          . NonEmpty.partition (null . fst) -}
          -- Case match here and when list is singleton, just map YieldToken onto it.
          -- This way we should avoid a ConsumeChar function that only returns empty
          -- list (and thereby avoid eating an extra character after the token).

          -- NonEmpty ([Char], a1) -> Either String [LexTree a1]

          \case
            ([], t) :| [] -> Right [ YieldToken t ]
            xs -> fmap (uncurry $ flip (:))
                . bitraverse
                    (Right . map (YieldToken . snd))
                    (go . map (first NonEmpty.fromList))
                . NonEmpty.partition (null . fst)
                $ xs

          )
        . factorFirstChar
        )
      . groupByFirstChar

    groupByFirstChar :: Eq a => [(NonEmpty a, b)] -> [NonEmpty (NonEmpty a, b)]
    groupByFirstChar = NonEmpty.groupBy $ curry $ uncurry (==) . join bimap (NonEmpty.head . fst)

    factorFirstChar :: NonEmpty (NonEmpty a, b) -> (a, NonEmpty ([a], b))
    factorFirstChar g@((c :| _, _) :| _) = (c, NonEmpty.map (first NonEmpty.tail) g)

makeLexTree2 :: Map String a -> LexTree a
makeLexTree2 = go . map (first NonEmpty.fromList) . Map.toList
  where
    go :: [(NonEmpty Char, a)] -> LexTree a
    go
      = consumeChar
      . map (fmap groupToLexTree . factorFirstChar)
      . groupByFirstChar

    consumeChar :: [(Char, [LexTree a])] -> LexTree a
    consumeChar choices = ConsumeChar $ \c ->
      join $ maybeToList $ snd <$> List.find ((c ==) . fst) choices

    groupToLexTree :: NonEmpty ([Char], a1) -> [LexTree a1]
    groupToLexTree = \case
      ([], t) :| [] -> [ YieldToken t ]
      xs -> (uncurry $ flip (:))
          . bimap
              (map (YieldToken . snd))
              (go . map (first NonEmpty.fromList))
          . NonEmpty.partition (null . fst)
          $ xs

    factorFirstChar :: NonEmpty (NonEmpty a, b) -> (a, NonEmpty ([a], b))
    factorFirstChar g@((c :| _, _) :| _) = (c, NonEmpty.map (first NonEmpty.tail) g)

    groupByFirstChar :: Eq a => [(NonEmpty a, b)] -> [NonEmpty (NonEmpty a, b)]
    groupByFirstChar = NonEmpty.groupBy $ curry $ uncurry (==) . join bimap (NonEmpty.head . fst)

makeLexTree :: Show a => [a] -> Either String (LexTree a)
makeLexTree = go . map (NonEmpty.fromList . show &&& id)
  where
    go :: [(NonEmpty Char, a)] -> Either String (LexTree a)
    go
      = fmap consumeChar
      . sequenceA
      . map (traverse groupToLexTree . factorFirstChar)
      . groupByFirstChar

    consumeChar :: [(Char, [LexTree a])] -> LexTree a
    consumeChar choices = ConsumeChar $ \c ->
      join $ maybeToList $ snd <$> List.find ((c ==) . fst) choices

    groupToLexTree :: NonEmpty ([Char], a1) -> Either String [LexTree a1]
    groupToLexTree = \case
      ([], t) :| [] -> Right [ YieldToken t ]
      xs -> fmap (uncurry $ flip (:))
          . bitraverse
              (Right . map (YieldToken . snd))
              (go . map (first NonEmpty.fromList))
          . NonEmpty.partition (null . fst)
          $ xs

    factorFirstChar :: NonEmpty (NonEmpty a, b) -> (a, NonEmpty ([a], b))
    factorFirstChar g@((c :| _, _) :| _) = (c, NonEmpty.map (first NonEmpty.tail) g)

    groupByFirstChar :: Eq a => [(NonEmpty a, b)] -> [NonEmpty (NonEmpty a, b)]
    groupByFirstChar = NonEmpty.groupBy $ curry $ uncurry (==) . join bimap (NonEmpty.head . fst)


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


printLexTree
  :: (Bounded a, Enum a, Ord a, Show a)
  => [a] -> LexTree a -> String

printLexTree tokens lt = Pretty.drawVerticalTree
                       $ lexTreeToTree tokens lt




-- Idea For Another Vim Service / Plugin:
-- Sorting of comma separated fields within parentheses.



data Token = Alpha | Beta | Gamma | Delta | Epsilon | X
  deriving (Bounded, Enum, Eq, Ord, Show)

tokens :: [Token]
tokens = [ minBound .. maxBound ]

testLexTree :: LexTree Token
testLexTree = either (const $ ConsumeChar mempty) id $ makeLexTreeOld tokens
