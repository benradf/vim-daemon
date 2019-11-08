{-# LANGUAGE LambdaCase #-}

module Lex where

import Control.Applicative (Alternative(..))
import Data.Functor (($>))
import Control.Arrow ((&&&))
import Control.Monad (join, when)
import Control.Monad.State (State, get, gets, modify, put, runState)
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

import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Tree.Pretty as Pretty



data LexTree a
    = ConsumeChar (Char -> [LexTree a])  -- list for back tracking
    | YieldToken a

fromYield :: LexTree a -> Maybe a
fromYield (YieldToken x) = Just x
fromYield _ = Nothing

instance Functor LexTree where
  fmap f = \case
    ConsumeChar g -> ConsumeChar (map (fmap f) . g)
    YieldToken x -> YieldToken (f x)

--instance Applicative LexTree where
--  pure = YieldToken
--  (<*>) = curry $ \case
--    (YieldToken _, YieldToken _) -> undefined
--    (ConsumeChar f, _) -> undefin
--instance Alternative L


lexTreeToForest
  :: (Bounded a, Enum a, Ord a, Show a)
  => [a] -> LexTree a -> Tree.Forest String

lexTreeToForest tokens = \case
  ConsumeChar f ->
    let g c = Tree.Node ("ConsumeChar " <> show c) <$> (lexTreeToForest tokens <$> f c)
    in g =<< characters

  YieldToken a ->
    pure $ Tree.Node ("YieldToken '" <> show a <> "'") []

  where
    characters = List.nub $ show =<< tokens

makeLexTree :: Show a => [a] -> Either String (LexTree a)
makeLexTree = lexTree . map (NonEmpty.fromList . show &&& id)


lexTree :: [(NonEmpty Char, a)] -> Either String (LexTree a)
lexTree
  = fmap ConsumeChar 
  . fmap (\xs c -> join $ maybeToList $ snd <$> List.find ((c ==) . fst) xs)
  . sequenceA
  . map
    ( traverse
      ( fmap (uncurry $ flip (:))
      . bitraverse
          (Right . map (YieldToken . snd))
          (lexTree . map (first NonEmpty.fromList))
      . NonEmpty.partition (null . fst)
      )
    . factorFirstChar
    )
  . groupByFirstChar


--fix :: (a -> a) -> a
--fix f = f (fix f)


factorFirstChar :: NonEmpty (NonEmpty a, b) -> (a, NonEmpty ([a], b))
factorFirstChar g@((c :| _, _) :| _) = (c, NonEmpty.map (first NonEmpty.tail) g)




runLexTree :: Char -> LexTree a -> [LexTree a]
runLexTree c = \case
  ConsumeChar f -> f c
  t@(YieldToken _) -> pure t

step :: Char -> [LexTree a] -> [LexTree a]
step c trees = runLexTree c =<< trees

lexer :: Show a => [LexTree a] -> NonEmpty Char -> State [LexTree a] [a]
lexer init (c :| cs) = do
  trees0 <- get

  trees' <- gets $ step c

  put $ if null trees' then init else trees'

  foo <- get
  

  case (NonEmpty.nonEmpty cs, trees') of
    (Just s, YieldToken t : _) -> (t :) <$> (put init *> lexer init s)    -- put init $> (t :) <*> lexer init s
    (Just s, _               ) ->                       lexer init s
    (Nothing, _              ) -> pure $ maybeToList . listToMaybe
                                       $ mapMaybe fromYield trees'

printLexTree
  :: (Bounded a, Enum a, Ord a, Show a)
  => [a] -> LexTree a -> String

printLexTree tokens lt = Pretty.drawVerticalForest
                       $ lexTreeToForest tokens lt




--  emitToken <- case trees' of
--                  YieldToken t : _ -> trace "emitToken: YIELD" $
--                    put init $> fmap (t :)
--                  _ -> trace "emitToken: NONE" $ pure id
--
--  emitToken $ case NonEmpty.nonEmpty cs of
--    Just string' -> trace "RECURSE" $ lexer init string'
--    Nothing -> trace "DONE" $ pure $ maybeToList . listToMaybe
--                    $ mapMaybe fromYield trees'

--  modify $ second $ const $
--    if trace ("resetLexTree = " <> show resetLexTree) resetLexTree
--      then init
--      else trees'

--  action <- case trees' of
--                    YieldToken t : _ -> trace ("action: yield") $ do
--                      modify $ second $ const init
--                      pure $ fmap (t :)
--                    _ -> pure id
--
--  action $ case NonEmpty.nonEmpty string' of
--                    Just s -> trace "string' NOT empty" $ do
--                      modify $ first $ const s
--                      lexer init
--                    Nothing -> trace ("string' IS empty, trees' = " <> show (map printLexTree trees')) $
--                      pure $ maybeToList . listToMaybe $ mapMaybe fromYield trees'

--testTree = lexTreeToForest tokens testLexTree
--putStrLn $ Pretty.drawVerticalTree testTree


--printLexTree :: Show a => LexTree a -> String
--printLexTree v = show (hashStableName $ unsafePerformIO (makeStableName v)) <> ": " <> case v of
--  ConsumeChar f -> "ConsumeChar"
--  YieldToken t -> "YieldToken " <> show t

--  if null trees'
--    then modify $ second $ const init
--    else put state'

--  case trees' of
--    [] -> next
--    YieldToken t : _ -> (t :) <$> next
--    _ -> next
--




-- :t sequenceA . map (traverse (fmap (uncurry $ flip (:)) . bitraverse (Right . map (YieldToken . snd)) (lexTree . map (first NonEmpty.fromList)). NonEmpty.partition (null . fst)) . factorFirstChar) . groupByFirstChar

-- :t sequenceA . map (traverse (bitraverse (Right . map (YieldToken . snd)) (lexTree . map (first NonEmpty.fromList)). NonEmpty.partition (null . fst)) . factorFirstChar) . groupByFirstChar

-- :t map (second (bimap id (lexTree . map (first NonEmpty.fromList)). NonEmpty.partition (null . fst)) . factorFirstChar) . groupByFirstChar


--instance Alternative LexTreeOld where
--    NoToken <|> rhs = rhs
--    YieldTokenOld x <|> _ = YieldTokenOld x
-- etc


data LexTreeOld t
  = ConsumeCharOld [Map Char (LexTreeOld t)]
  | YieldTokenOld t
-- TODO: data type isn't quite right ... what about yielding a token or continuing?


makeLexTree2 :: Show token => [token] -> LexTreeOld token
makeLexTree2 tokens =
  undefined
--  let strings = show <$> tokens
--  in undefined
  where
    lexTree2 :: [(NonEmpty Char, token)] -> Maybe (LexTreeOld token)
    lexTree2 pairs =
      --let (leaves, nodes) = partitionGroups $ groupByFirstChar pairs
      undefined

    test1 = NonEmpty.partition $ \(_ :| cs, _) -> null cs


-- Idea For Another Vim Service / Plugin:
-- Sorting of comma separated fields within parentheses.

partitionGroups = NonEmpty.partition $ \(_ :| cs, _) -> null cs

groupByFirstChar :: Eq a => [(NonEmpty a, b)] -> [NonEmpty (NonEmpty a, b)]
groupByFirstChar = NonEmpty.groupBy $ curry $ uncurry (==) . join bimap (NonEmpty.head . fst)

annotateGroup :: NonEmpty (NonEmpty a, b) -> (a, NonEmpty (NonEmpty a, b))
annotateGroup g@((c :| _, _) :| _) = (c, g)

-- :t map ( \case { (c, ([], ps)) -> undefined; (c, ([(_, t)], ps)) -> YieldTokenOld t; _ -> error "ambiguous"  } . second ( second (map (first (NonEmpty.fromList . NonEmpty.tail))) . partitionGroups) . annotateGroup) . groupByFirstChar


-- :t map (second ( \case { ([], []) -> undefined; ([(c :| _, t)], ps) -> YieldTokenOld t : []; _ -> error "ambiguous" } . second (map (first (NonEmpty.fromList . NonEmpty.tail))) . partitionGroups) . annotateGroup) . groupByFirstChar

-- :t map (second (bimap (map (YieldTokenOld . snd)) (id) . partitionGroups) . annotateGroup) . groupByFirstChar







data Token = Alpha | Beta | Gamma | Delta | Epsilon
  deriving (Bounded, Enum, Eq, Ord, Show)

tokens :: [Token]
tokens = [ minBound .. maxBound ]

testLexTree = either (const $ ConsumeChar mempty) id $ makeLexTree tokens
