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
  , memoize
  , runLexer
  , tests
  , unLocated
  ) where

import Debug.Trace (trace)

import Control.Applicative (Alternative(..), (<|>))
import Control.Arrow ((&&&))
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Error.Util (hoistMaybe)
import Control.Monad (join, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..))
import Data.Bifunctor (bimap, first)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity(..))
import Data.IORef (newIORef)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, maybeToList, mapMaybe)
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector
import GHC.Prim (RealWorld)
import qualified Streaming as S
import Streaming.Prelude (Of, Stream)
import qualified Streaming.Prelude as S
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck

import Location (Located(..), Location(..), makeLocatedString, unLocated)


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


--type LocatedStream m a = Stream m (Located a)
type LocatedStream m a = Stream (Of (Located a)) m ()

--type StringStream m = Stream m (Located Char)
type StringStream m = Stream (Of (Located Char)) m ()

makeStringStream :: String -> StringStream Identity
makeStringStream = S.each . makeLocatedString


memoize :: MonadIO m => Stream (Of a) m r -> m (Stream (Of a) m r)
memoize stream = do

  memRef <- liftIO $ newMVar =<< MVector.new 1
  mem <- liftIO $ readMVar memRef
  liftIO $ MVector.write mem 0 (Left stream)

  pure $ loop 0 memRef

  where
    loop
      :: MonadIO m
      => Int
      -> MVar (MVector RealWorld (Either (Stream (Of a) m r) a))
      -> Stream (Of a) m r

    loop i memRef = S.effect $ do
      mem <- liftIO $ readMVar memRef
      liftIO (MVector.read mem i) >>= \case

        Left stream -> {-trace ("Left @ " ++ show i) $ -}
          S.next stream >>= \case
            Left r -> pure (pure r)

            Right (x, stream') -> liftIO $ do
              let len = MVector.length mem
              when (i + 1 == len) $
                {-trace ("GROW from " ++ show len ++ " to " ++ show (2 * len)) $ -}
                modifyMVar_ memRef $ flip MVector.grow len

              mem <- readMVar memRef
              MVector.write mem i (Right x)
              MVector.write mem (i + 1) (Left stream')

              pure $ loop i memRef

        Right x -> {-trace ("Right @ " ++ show i) $ -} pure $ do
          S.yield x
          loop (i + 1) memRef

--      when (i >= MVector.length mem) $ liftIO $ do
--        let len = MVector.length mem
--        MVector.grow mem len
--        traverse_ (flip (MVector.write mem) Nothing) [ len .. 2 * len - 1 ]
--
--      read mem i >>= \case
--        Just x -> do
--          yield x
--          loop (i + 1) 
--
--      undefined

{-read mem i >>= \case
      Just x -> do
        yield x
        loop (i + 1)-}
      


{-S.next stream >>= \case
      Left _ -> pure mempty
      Right (x, stream') -> do-}
        
      
    


runLexer :: (Show a, Monad m) => LexTree a -> StringStream m -> LocatedStream m a
runLexer tree stream =
  S.effect $ S.next stream >>= \case
    Left _ -> pure mempty

    Right (char, stream') -> {-trace "runLexer.next" $ -}
      runMaybeT (runStateT attempt (pure tree, stream)) <&> \case
        Just (token, (_, stream')) -> {-trace ("yield " ++ show token ) $ -} do
          S.yield (token <$ char)
          runLexer tree stream'  -- TODO: Factor out this line since it's in both branches of case

        Nothing ->
          runLexer tree stream'

  where
    attempt :: Monad m => StateT ([LexTree a], StringStream m) (MaybeT m) a
    attempt = gets fst >>= \case
      [] -> empty  -- No more potential tokens
      trees -> step <|> yielded trees

    step :: Monad m => StateT ([LexTree a], StringStream m) (MaybeT m) a
    step = (join $ gets $ lift . lift . traverse S.next) >>= \case
      (trees, Right foo@(Located _ c, cs)) -> {-trace (show (fst foo)) $ -}
        put (runLexTree c =<< trees, cs) *> attempt
      (_, Left _) -> {-trace "EndOfStream" -} empty  -- End of stream

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
lexer = runIdentity . S.toList_ . runLexer lexTree . makeStringStream


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
        let result = concat $ map unLocated $ runIdentity $ S.toList_ (runLexer lexTree (makeStringStream string))
        HUnit.assertEqual "Lexed correctly" string result
    ]

  , Tasty.testGroup "Unit"
    [ HUnit.testCase "Infinite stream is lexed lazily" $ do
        let infiniteTokenStream = runLexer lexTree $ S.repeat $ Located (Location 1 1) 'X'
        someTokens <- S.toList_ $ S.take 10 infiniteTokenStream
        HUnit.assertEqual "" (replicate 10 X) (unLocated <$> someTokens)
    ]
  ]


(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
infixl 1 <&>
