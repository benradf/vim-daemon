{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-

  When binary starts it does not initially change vim at all.
  Instead it first determines which of the following two cases holds:

  1. This is the first/only process running as a job.
  2. There is an existing process running as a job.

  When case 1 holds the binary installs itself as the primary process.
  And when case 2 holds the process waits idly for the primary process
  to hand over to it.

  Another approach could be to iterate through all global variables of
  type v:t_job and find the one with pid matching the current process.
  In the unexpected scenario that none of the jobs has a matching pid,
  the current process should immediately exit.

  Assuming the current process finds the global variable anchoring it,
  it is then in a position to remain idle or install itself as needed.

  --

  let g:job1 = job_start(['/bin/bash', '-c', 'echo "[\"normal\", \"G\"]"'], {'mode': 'json'})
  echo g:
  echo type(g:)
  echo g:.job1
  echo type(g:.job1)

  for j in job_info() | if job_info(j).cmd == job_info(g:job).cmd | call job_stop(j) | endif | endfor

-}





{-

function! s:launch_vim_server()
  call ch_logfile('/tmp/channel.log')

  let l:cmd = 'dist/build/vim-server/vim-server'

  for j in job_info()
    if job_info(j).cmd[0] == l:cmd
      call job_stop(j)
    endif
  endfor

  let g:job = job_start([l:cmd], {'mode': 'json'})
  let g:channel = job_getchannel(g:job)

  "echo ch_evalexpr(g:channel, winlayout())
endfunction

command! -nargs=0 LaunchVimServer call <SID>launch_vim_server()
nnoremap <silent> <F5> :LaunchVimServer<CR>

-}

module Loader
  ( initialise
  , tests
  ) where

import Control.Error.Util (hush)
import Control.Exception (ErrorCall, Exception, IOException, bracket, evaluate, try)
import Control.Monad (guard, join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bifunctor (second)
import Data.Bits ((.&.))
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (catMaybes, fromJust, maybeToList)
import Data.Semigroup ((<>))
import qualified System.Posix.Directory as Posix
import qualified System.Posix.Files as Posix
import System.Posix.Types (ProcessID)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

import Vim (VimT)
import qualified Vim as Vim


initialise :: MonadIO m => VimT m ()
initialise = do
  --Vim.ex "echo 'job_info() = ' . string(job_info()) . ', g:job = ' . string(g:job)"
  pid <- liftIO $ Posix.readSymbolicLink "/proc/self"
  Vim.exs
    [ "for j in job_info()"
    , "  if job_info(j).process == '" <> pid <> "'"
    , "    call ch_evalexpr(job_getchannel(j), j == g:job ? 'install' : 'selftest')"
    , "  endif"
    , "let j = v:none"
    ]
  pure ()









findJobs :: IO [ProcessID]
findJobs = undefined


catching :: forall e a . Exception e => IO a -> MaybeT IO a
catching = MaybeT . fmap hush . try @e


readDirectory :: FilePath -> IO [FilePath]
readDirectory path =
  bracket (Posix.openDirStream path) Posix.closeDirStream loop
  where
    loop dir = Posix.readDirStream dir >>= \case
      "" -> pure []
      entry -> (entry :) <$> loop dir


getPipeId :: FilePath -> IO (Maybe Int)
getPipeId path = runMaybeT $ do
  target <- catching @IOException $ Posix.readSymbolicLink path
  guard $ "pipe:[" `isPrefixOf` target
  guard $ "]" `isSuffixOf` target
  let len = length target
  catching @ErrorCall $ evaluate $ read @Int $ take (len - 7) $ drop 6 $ target


findVimProcesses :: IO [ProcessID]
findVimProcesses = fmap (join . maybeToList) . runMaybeT $ do
  --pipeStdin <- MaybeT $ getPipeId "/proc/self/0"
  --pipeStdout <- MaybeT $ getPipeId "/proc/self/1"
  lift $ catMaybes <$> (traverse onlyVim =<< readDirectory "/proc")
  where
    onlyVim :: FilePath -> IO (Maybe ProcessID)
    onlyVim entry = runMaybeT $ do
      pid <- catching @ErrorCall $ evaluate $ read @ProcessID entry
      let procPath suffix = "/proc/" <> show pid <> "/" <> suffix
      exe <- catching @IOException $ Posix.readSymbolicLink $ procPath "exe"
      guard $ "vim" `isInfixOf` exe
      let fullFdPath fd = procPath $ "fd/" <> fd
      pipes <- lift $ catMaybes <$> (traverse (getPipeId . fullFdPath) =<< readDirectory (procPath "fd"))
      liftIO $ putStrLn $ show exe
      liftIO $ putStrLn $ show pipes
      pure pid
      -- TODO: Check if self stdin/stdout pipes are open by each potential vim process
  






tests :: Tasty.TestTree
tests = Tasty.testGroup "module Loader"
  [ Tasty.testGroup "QuickCheck" [ ]
  , Tasty.testGroup "Regression" [ ]

  , Tasty.testGroup "Unit"
    [ HUnit.testCase "Find executable path" $ do
        putStrLn ""

        putStrLn =<< Posix.readSymbolicLink "/proc/self/exe"
        --putStrLn =<< Posix.readSymbolicLink "/proc/11900/fd/0"

        mode <- Posix.fileMode <$> Posix.getSymbolicLinkStatus "/proc/self/fd/0"
        putStrLn $ "ownerReadMode: " ++ show (mode .&. Posix.ownerReadMode /= 0)
        putStrLn $ "ownerWriteMode: " ++ show (mode .&. Posix.ownerWriteMode /= 0)

        putStrLn ""

        res <- hush <$> try @IOException (Posix.getSymbolicLinkStatus "/does/not/exist")

        --putStrLn $ show $ Posix.fileMode $ fromJust res

        --traverse putStrLn =<< readDirectory "/proc"
        traverse (putStrLn . show) =<< findVimProcesses

        pure ()

        --HUnit.assertEqual "" "" ""
    ]
  ]
