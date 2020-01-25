{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified BufferView as BufferView
import qualified CommaTextObject as CommaTextObject
import qualified Lex as Lex
import qualified Loader as Loader
import qualified Location as Location
import qualified Stream as Stream
import qualified Test.Tasty as Tasty


main :: IO ()
main = do
  Tasty.defaultMain $ Tasty.testGroup "All Tests"
    [ BufferView.tests
    , CommaTextObject.tests
    , Lex.tests
    , Loader.tests
    , Location.tests
    , Stream.tests
    ]
