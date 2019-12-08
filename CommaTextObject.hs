{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module CommaTextObject where

import BufferView (BufferView(..), exampleLines, makeBufferViewFromLines)
import Location (Located(..), Location(..), unLocated)
import Lex
import Stream (Stream)
import qualified Stream as Stream
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck
import Data.Functor.Identity (Identity(..))
import qualified Data.List.NonEmpty as NonEmpty
import Control.Applicative (Alternative(empty))
import Control.Monad.State (get, put)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.Maybe (MaybeT(..))


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

lexer :: Monad m => StringStream m -> m (LocatedStream m Token)
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
  :: Monad m
  => (Token -> Bool)
  -> (Token -> Token -> Bool)
  -> Stream m (Located Token)
  -> m (Maybe (Located Token))

findBoundary isOpen matches stream =
  evalStateT (runMaybeT step) ([], stream)

  where
    step = get >>= traverse (MaybeT . lift . Stream.extract) >>= \case
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



tests :: Tasty.TestTree
tests = Tasty.testGroup "module CommaTextObject"
  [ Tasty.testGroup "QuickCheck" [ ]
  , Tasty.testGroup "Regression" [ ]

  , Tasty.testGroup "Unit"
    [ HUnit.testCase "Run lexer on before and after streams" $ do
        bv <- makeBufferViewFromLines (Location 7 50) exampleLines
        tokens <- lexer (bvBefore bv) >>= Stream.toList

        putStrLn ""
        putStr "\x1b[36;40m"
        putStrLn . map unLocated =<< Stream.toList (bvBefore bv)
        putStr "\x1b[0m"

        putStrLn ""
        mapM_ (putStrLn . show) tokens

        -- TODO: Finish this unit test
        -- Should probably assert that all the token locations are correct.

        --HUnit.assertEqual "" exampleLines ((reverse <$> reverse beforeLines) ++ afterLines)
        pure ()

    , HUnit.testCase "Mismatched brackets stops boundary search" $ do
        let b = testFindBoundary "1 + 2 * (3 + 4} , 5"
        HUnit.assertEqual "" Nothing b

    , HUnit.testCase "End of stream is not a boundary" $ do
        let b = testFindBoundary "foo ( bar ) baz qux"
        HUnit.assertEqual "" Nothing b

    , HUnit.testCase "Ignore inner commas when finding boundary" $ do
        let b = testFindBoundary "(test + [ 1, 2, 3 ]) , foo } ]"
        HUnit.assertEqual "" (Just (Located (Location 1 22) Comma)) b
    ]
  ]

testFindBoundary :: String -> Maybe (Located Token)
testFindBoundary string = runIdentity $ findBoundary isOpen matches (runIdentity $ lexer $ Lex.makeStringStream string)
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
