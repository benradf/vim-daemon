module Lex where


import Control.Monad (join)
import Data.Bifunctor (bimap, first, second)
import Data.Bitraversable (bitraverse)
import Data.List (groupBy)
import Data.Maybe (maybeToList)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Traversable (sequenceA)
import Control.Arrow ((&&&))


data LexTree a
    = ConsumeChar (Char -> [LexTree a])  -- list for back tracking
    | YieldToken a
    | NoToken

instance Functor LexTree where
  fmap f (ConsumeChar g) = ConsumeChar (map (fmap f) . g)
  fmap f (YieldToken a) = YieldToken (f a)
  fmap f NoToken = NoToken

makeLexTree :: Show a => [a] -> Either String (LexTree a)
makeLexTree = lexTree . map (NonEmpty.fromList . show &&& id)


lexTree :: [(NonEmpty Char, a)] -> Either String (LexTree a)
lexTree = fmap ConsumeChar . fmap (\xs c -> join $ maybeToList $ snd <$> List.find ((c ==) . fst) xs) . sequenceA . map (traverse (fmap (uncurry (flip (:))) . bitraverse (Right . map (YieldToken . snd)) (lexTree . map (first NonEmpty.fromList)). NonEmpty.partition (null . fst)) . factorFirstChar) . groupByFirstChar

factorFirstChar :: NonEmpty (NonEmpty a, b) -> (a, NonEmpty ([a], b))
factorFirstChar g@((c :| _, _) :| _) = (c, NonEmpty.map (first NonEmpty.tail) g)


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
  deriving (Bounded, Enum, Show)

tokens :: [Token]
tokens = [ minBound .. maxBound ]
