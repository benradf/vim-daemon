{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module CommaTextObject where

import BufferView (exampleLines, makeStreamPairFromLines)
import Location (Location(..))
import Lex
import Stream (Stream)
import qualified Stream as Stream
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QuickCheck
import Data.Functor.Identity (Identity(..))


data Token
  = Comma
  | LeftParen
  | RightParen
  | LeftBox
  | RightBox
  | LeftBrace
  | RightBrace
  | CarriageReturn
  | LineFeed
  deriving Show

lexer :: Monad m => StringStream m -> m [Located Token]
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
      , ("\r", CarriageReturn)
      , ("\n", LineFeed)
      ]





tests :: Tasty.TestTree
tests = Tasty.testGroup "module CommaTextObject"
  [ Tasty.testGroup "QuickCheck" [ ]
  , Tasty.testGroup "Regression" [ ]

  , Tasty.testGroup "Unit"
    [ HUnit.testCase "Run lexer on before and after streams" $ do
        (streamBefore, streamAfter) <- makeStreamPairFromLines 13 exampleLines
        tokens <- lexer (Located 0 <$> Stream.split (NonEmpty.fromList . (++ "\n")) streamBefore)
        --mapM_ (putStrLn . show) tokens
        -- TODO: Finish this unit test
        -- Should probably assert that all the token locations are correct.

        --HUnit.assertEqual "" exampleLines ((reverse <$> reverse beforeLines) ++ afterLines)
        pure ()
    ]
  ]
