{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (join, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import System.IO (BufferMode(..), hSetBuffering, stdout)

import qualified BufferView as BufferView
import CommaTextObject (FindBoundary(..))
import qualified CommaTextObject as CommaTextObject
import qualified Loader as Loader
import Location (Location(..), Located(..))
import Vim (VimT, runVimT)
import qualified Vim as Vim


-- call ch_logfile('/tmp/channel.log', 'w')
-- let g:job = job_start(['dist/build/vim-server/vim-server'], {'mode': 'json'})
-- let g:channel = job_getchannel(g:job)
-- echo ch_evalexpr(g:channel, winlayout())

-- TODO: functionality to "pin visual selection"
-- this would open a new window just big enough to hold the current selection
-- and set win min size etc appropriately
-- it should also return the cursor to exactly where it was in the original
-- window
-- maybe it should be bound to a g "go" command?

-- Idea For Another Vim Service / Plugin:
-- Sorting of comma separated fields within parentheses.


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let defaultHandler value = do
        Vim.ex $ "call ch_setoptions(g:channel, {'timeout': 100})"
        Vim.ex $ "echom 'defaultHandler got message: " <> show @String value <> "'"
        lastLine <- Vim.evaluate @Integer "line('$')"
        Vim.ex "echom 'test'"
        Vim.normal "gg"

        pure $ JSON.object
          [ T.pack "message" .= "The result message"
          ]

  let defaultHandler2 :: MonadIO m => String -> VimT m JSON.Value
      defaultHandler2 value = do
        loc@(Location.Location l c) <- getLocation
        liftIO $ appendFile "/tmp/vim-server.log" $ "defaultHandler2 received " <> show @String value <> "\n"

        bv <- BufferView.makeBufferView 2 loc $ \from to -> fmap (zip [ from .. to ]) $
                Vim.evaluate $ "getline(" ++ show from ++ ", " ++ show to ++ ")"
        let showLoc (Location.Located (Location.Location n m) x) = show n ++ ":" ++ show m ++ ":" ++ show x
        let FindBoundary {..} = CommaTextObject.findBoundaryDefault
        lhs <- findBoundaryLhs $ BufferView.bvBefore bv
        rhs <- findBoundaryRhs $ BufferView.bvAfter bv

        case (lhs, rhs) of
          (Nothing, _) -> pure ()
          (_, Nothing) -> pure ()
          (Just (Located (Location lhsLine lhsCol) _), Just (Located (Location rhsLine rhsCol) _)) -> do
            Vim.ex $ "call setpos('.', [0, " ++ show lhsLine ++ ", " ++ show (lhsCol + 1) ++ ", 0, " ++ show (lhsCol + 1) ++ "])"  -- curswant?
            Vim.normal "v"
            Vim.ex $ "call setpos('.', [0, " ++ show rhsLine ++ ", " ++ show (rhsCol - 1) ++ ", 0, " ++ show (rhsCol - 1) ++ "])"  -- curswant?
            Vim.redraw False


        Vim.ex "if 1 == 1 | echo 10 | endif"
        Vim.evaluate @Integer "if 1 == 1 | echo 10 | endif"
--        Vim.ex "  echo 10"
--        Vim.ex "endif"

--        foo <- Vim.evaluate @Integer "if 1 == 2 | return 5 | else | return 9 | endif"
--        error $ show foo

        pure $ JSON.toJSON ()

  let defaultHandler3 :: MonadIO m => String -> VimT m JSON.Value
      defaultHandler3 _ = Vim.ex "call ch_evalexpr(g:channel, \"test\")<CR>" $> JSON.object []


  inputCh <- runVimT (B.putStrLn . JSON.encode) defaultHandler2 $ do
    lineNum <- Vim.evaluate @Integer "line('.')"
    liftIO $ appendFile "/tmp/vim-server.log" $ "evaluate result is " <> show lineNum <> "\n"
    -- nmap <F2> <Plug>EditVimrc
    Vim.ex "call ch_logfile('/tmp/channel.log', 'w')"
    Vim.ex ":let g:channel = job_getchannel(g:job)"
    Vim.ex "nmap <F9> :call ch_evalexpr(g:channel, \"test\")<CR>"
    Loader.initialise

  let decode
        = fromMaybe (error "invalid json")
        . JSON.decode @JSON.Value

  void $ join $ traverse (inputCh . decode) . B.lines <$> B.getContents


getLocation :: VimT m Location.Location
getLocation = Location.Location <$> Vim.evaluate "line('.')" <*> Vim.evaluate "col('.')"
