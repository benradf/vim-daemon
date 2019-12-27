{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module CommaTextObject where

import Control.Applicative (Alternative(empty))
import Control.Error.Util (hush)
import Control.Monad.State (get, put)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.State (evalStateT)
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Streaming.Prelude (Of, Stream)
import qualified Streaming.Prelude as S
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

import BufferView (BufferView(..), exampleLines, makeBufferViewFromLines)
import Lex
import Location (Located(..), Location(..))

data Token
  = Comma
  | LeftParen
  | RightParen
  | LeftBox
  | RightBox
  | LeftBrace
  | RightBrace
--  | CarriageReturn
--  | LineFeed
  deriving (Eq, Show)

lexer :: Monad m => StringStream m -> LocatedStream m Token
lexer = runLexer lexTree
  where
    lexTree :: LexTree Token
    lexTree = makeLexTree $ Map.fromList
      [ (",", Comma)
      , ("(", LeftParen)
      , (")", RightParen)
      , ("[", LeftBox)
      , ("]", RightBox)
      , ("{", LeftBrace)
      , ("}", RightBrace)
--      , ("\r", CarriageReturn)
--      , ("\n", LineFeed)
      ]


findBoundary
  :: (Monad m, Monoid r)
  => (Token -> Bool)
  -> (Token -> Token -> Bool)
  -> Stream (Of (Located Token)) m r
  -> m (Either r (Located Token))

findBoundary isOpen matches stream =
  evalStateT (runExceptT step) ([], stream)

  where
    step = get >>= traverse (ExceptT . lift . S.next) >>= \case
      (stack, (lt@(Located _ token), stream'))
        | isOpen token ->
            put (token : stack, stream') *> step
        | otherwise -> case NonEmpty.nonEmpty stack of
            Just (top :| stack')
              | token `matches` top ->
                  put (stack', stream') *> step
              | token == Comma ->
                  put (stack, stream') *> step
              | otherwise -> empty
            Nothing -> pure lt


testFindBoundary :: String -> Maybe (Located Token)
testFindBoundary string = hush $ runIdentity $ findBoundary isOpen matches (lexer $ Lex.makeStringStream string)
  where
    isOpen = \case
      LeftParen -> True
      LeftBox -> True
      LeftBrace -> True
      otherwise -> False
    matches = curry $ \case
      (RightParen, LeftParen) -> True
      (RightBox, LeftBox) -> True
      (RightBrace, LeftBrace) -> True
      otherwise -> False


tests :: Tasty.TestTree
tests = Tasty.testGroup "module CommaTextObject"
  [ Tasty.testGroup "QuickCheck" [ ]
  , Tasty.testGroup "Regression" [ ]

  , Tasty.testGroup "Unit"
    [ HUnit.testCase "Mismatched brackets stops boundary search" $ do
        let b = testFindBoundary "1 + 2 * (3 + 4} , 5"
        HUnit.assertEqual "" Nothing b

    , HUnit.testCase "End of stream is not a boundary" $ do
        let b = testFindBoundary "foo ( bar ) baz qux"
        HUnit.assertEqual "" Nothing b

    , HUnit.testCase "Ignore inner commas when finding boundary" $ do
        let b = testFindBoundary "(test + [ 1, 2, 3 ]) , foo } ]"
        HUnit.assertEqual "" (Just (Located (Location 1 22) Comma)) b

    , HUnit.testCase "Run lexer on example lines" $ do
        bv <- makeBufferViewFromLines 2 (Location 7 50) exampleLines
        HUnit.assertEqual "tokens before incorrect"
          [ Located (Location 7 41) LeftParen
          , Located (Location 7 8) RightBrace
          , Located (Location 7 1) LeftBrace
          , Located (Location 6 8) RightBrace
          , Located (Location 6 1) LeftBrace
          , Located (Location 5 46) RightParen
          , Located (Location 5 31) Comma
          , Located (Location 5 17) LeftParen
          , Located (Location 5 8) RightBrace
          , Located (Location 5 1) LeftBrace
          , Located (Location 4 8) RightBrace
          , Located (Location 4 1) LeftBrace
          , Located (Location 3 52) RightParen
          , Located (Location 3 51) RightBox
          , Located (Location 3 46) LeftBox
          , Located (Location 3 15) LeftParen
          , Located (Location 3 8) RightBrace
          , Located (Location 3 1) LeftBrace
          , Located (Location 2 8) RightBrace
          , Located (Location 2 1) LeftBrace
          , Located (Location 1 8) RightBrace
          , Located (Location 1 1) LeftBrace
          ]
          =<< S.toList_ (lexer $ bvBefore bv)
        HUnit.assertEqual "tokens after incorrect"
          [ Located (Location 7 62) RightParen
          , Located (Location 8 1) LeftBrace
          , Located (Location 8 8) RightBrace
          , Located (Location 9 1) LeftBrace
          , Located (Location 9 8) RightBrace
          , Located (Location 10 1) LeftBrace
          , Located (Location 10 8) RightBrace
          , Located (Location 11 1) LeftBrace
          , Located (Location 11 8) RightBrace
          , Located (Location 12 1) LeftBrace
          , Located (Location 12 8) RightBrace
          , Located (Location 13 1) LeftBrace
          , Located (Location 13 8) RightBrace
          , Located (Location 14 1) LeftBrace
          , Located (Location 14 8) RightBrace
          , Located (Location 15 1) LeftBrace
          , Located (Location 15 8) RightBrace
          , Located (Location 15 36) LeftParen
          , Located (Location 15 54) RightParen
          , Located (Location 16 1) LeftBrace
          , Located (Location 16 8) RightBrace
          , Located (Location 17 1) LeftBrace
          , Located (Location 17 8) RightBrace
          , Located (Location 18 1) LeftBrace
          , Located (Location 18 8) RightBrace
          , Located (Location 19 1) LeftBrace
          , Located (Location 19 8) RightBrace
          , Located (Location 20 1) LeftBrace
          , Located (Location 20 8) RightBrace
          , Located (Location 21 1) LeftBrace
          , Located (Location 21 8) RightBrace
          , Located (Location 21 34) LeftParen
          , Located (Location 21 52) RightParen
          , Located (Location 22 1) LeftBrace
          , Located (Location 22 8) RightBrace
          , Located (Location 23 1) LeftBrace
          , Located (Location 23 8) RightBrace
          , Located (Location 24 1) LeftBrace
          , Located (Location 24 8) RightBrace
          , Located (Location 25 1) LeftBrace
          , Located (Location 25 8) RightBrace
          , Located (Location 25 12) LeftParen
          , Located (Location 25 13) Comma
          , Located (Location 25 14) RightParen
          , Located (Location 26 1) LeftBrace
          , Located (Location 26 8) RightBrace
          , Located (Location 27 1) LeftBrace
          , Located (Location 27 8) RightBrace
          , Located (Location 28 1) LeftBrace
          , Located (Location 28 8) RightBrace
          , Located (Location 29 1) LeftBrace
          , Located (Location 29 8) RightBrace
          , Located (Location 30 1) LeftBrace
          , Located (Location 30 8) RightBrace
          ]
          =<< S.toList_ (lexer $ bvAfter bv)
    ]
  ]
