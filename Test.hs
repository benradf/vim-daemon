{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Free
import Control.Monad.Trans.Reader
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Functor
import Data.Functor.Identity
import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Semigroup
import qualified Data.Text as T

import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import Prelude
import System.IO

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad.State
import Data.Bifunctor
import Data.Bitraversable
import Debug.Trace (trace)

import qualified Test.Tasty as Tasty
import qualified CommaTextObject as CommaTextObject
import qualified BufferView as BufferView
import qualified Lex as Lex
import qualified Location as Location
import qualified Stream as Stream

main :: IO ()
main = do
  Tasty.defaultMain $ Tasty.testGroup "All Tests"
    [ BufferView.tests
    , CommaTextObject.tests
    , Lex.tests
    , Location.tests
    , Stream.tests
    ]
