{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module CommaTextObject where

import BufferView (BufferView(..), exampleLines, makeBufferViewFromLines)
import Location (Located(..), Location(..), unLocated)
import Lex
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Streaming.Prelude (Of, Stream)
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
import Control.Error.Util (hush)
import Control.Monad.State (get, put)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
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



tests :: Tasty.TestTree
tests = Tasty.testGroup "module CommaTextObject"
  [ Tasty.testGroup "QuickCheck" [ ]
  , Tasty.testGroup "Regression" [ ]

  , Tasty.testGroup "Unit"
    [ HUnit.testCase "Run lexer on before and after streams" $ do
        bv <- makeBufferViewFromLines (Location 7 50) exampleLines
        tokens <- S.toList_ $ lexer (bvBefore bv)
        tokens2 <- S.toList_ $ lexer (bvAfter bv)

        -- TODO: Race condition here because of IORef?
        -- Keeping references to earlier parts of a Stream.
        -- Need to make a Stream usable only once by encoding
        -- this restriction into its type.
        putStrLn ""
        putStr "\x1b[36;40m"
        putStrLn . map unLocated =<< S.toList_ (bvBefore bv)
        putStr "\x1b[0m"
        putStrLn ""
        mapM_ (putStrLn . show) tokens

        putStrLn ""
        putStr "\x1b[35;40m"
        putStrLn . map unLocated =<< S.toList_ (bvAfter bv)
        putStr "\x1b[0m"
        putStrLn ""
        mapM_ (putStrLn . show) tokens2

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
