module Lex where


import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.List (groupBy)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map(..))
import qualified Data.Map as Map


data LexTree t
  = ConsumeChar [Map Char (LexTree t)]
  | YieldToken t
-- TODO: data type isn't quite right ... what about yielding a token or continuing?


makeLexTree :: Show token => [token] -> LexTree token
makeLexTree tokens =
  undefined
--  let strings = show <$> tokens
--  in undefined
  where
    lexTree :: [(NonEmpty Char, token)] -> Maybe (LexTree token)
    lexTree pairs =
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

-- :t map ( \case { (c, ([], ps)) -> undefined; (c, ([(_, t)], ps)) -> YieldToken t; _ -> error "ambiguous"  } . second ( second (map (first (NonEmpty.fromList . NonEmpty.tail))) . partitionGroups) . annotateGroup) . groupByFirstChar


-- :t map (second ( \case { ([], []) -> undefined; ([(c :| _, t)], ps) -> YieldToken t : []; _ -> error "ambiguous" } . second (map (first (NonEmpty.fromList . NonEmpty.tail))) . partitionGroups) . annotateGroup) . groupByFirstChar

-- :t map (second (bimap (map (YieldToken . snd)) (id) . partitionGroups) . annotateGroup) . groupByFirstChar







data Token = Alpha | Beta | Gamma | Delta | Epsilon
  deriving (Bounded, Enum, Show)

tokens :: [Token]
tokens = [ minBound .. maxBound ]
