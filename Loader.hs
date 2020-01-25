--{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Loader
  ( initialise
  , tests
  ) where

import Control.Applicative (liftA2)
import Control.Arrow (Kleisli(..))
import Control.Category 
import Control.Error.Util (hush)
import Control.Exception (ErrorCall, Exception, IOException, bracket, evaluate, try)
import Control.Monad (guard, join, (>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (Reader(..))
import Control.Monad.State (State(..), evalState, gets, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.RWS (RWS, runRWS)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Bifunctor (second)
import Data.Bits ((.&.))
import Data.Coerce (coerce)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (catMaybes, fromJust, maybeToList)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector
import Prelude hiding (id, (.))
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
--    , "    let x = " <> embed "" (do  -- Call back into Haskell
--                          pure ()
--                          pure ()
--                          pure ()
--                          )
    , "  endif"
    , "let j = v:none"
    ]
  pure ()


embed :: String -> VimT m a -> String
embed = undefined






--newtype Hybrid = Hybrid (Either String ())


--data ExecutionContext m = ExecutionContext
--  { ecCallbacks :: forall a b . (JSON.FromJSON a, JSON.ToJSON b) => [a -> VimT m b]
--  , ecVimScript :: Reader String String
--  }


-- 


--data Message = Message
--  { mSeqNum :: Int
--  , mPayload :: JSON.Value
--  }
--  deriving Show
--
--instance JSON.ToJSON Message where
--  toJSON message = JSON.toJSON
--    [ JSON.toJSON $ mSeqNum message
--    , mPayload message
--    ]
--
--instance JSON.FromJSON Message where
--  parseJSON = JSON.withArray "Message" $ \array -> Message
--    <$> maybe (fail "missing seq num") JSON.parseJSON (array !? 0)
--    <*> maybe (fail "missing payload") JSON.parseJSON (array !? 1)



data ContinuationCall = ContinuationCall
  { cIndex :: Int
  , cArg :: JSON.Value
  }

instance JSON.ToJSON ContinuationCall where
  toJSON continuation = JSON.toJSON
    [ JSON.toJSON $ cIndex continuation
    , cArg continuation
    ]

instance JSON.FromJSON ContinuationCall where
  parseJSON = JSON.withArray "ContinuationCall" $ \array -> ContinuationCall
    <$> maybe (fail "missing index") JSON.parseJSON (array !? 0)
    <*> maybe (fail "missing arg") JSON.parseJSON (array !? 1)



continuation :: (JSON.FromJSON a, JSON.ToJSON b) => String -> (a -> VimT m b) -> RemoteExec m
continuation arg k = RemoteExec $ do
  len <- gets Vector.length
  modify (`Vector.snoc` Continuation k)
  pure $ "ch_evalexpr(g:vimd,[" <> show len <> "," <> arg <> "])"

-- let RemoteExec s = continuation "a:mode" $ \() -> pure 10 in evalState s (Vector.singleton undefined)

data Continuation m where
  Continuation
    :: forall a b m . (JSON.FromJSON a, JSON.ToJSON b)
    => (a -> VimT m b)
    -> Continuation m


newtype RemoteExec m = RemoteExec
  { runRemoteExec :: State (Vector (Continuation m)) String
  }

instance Semigroup (RemoteExec m) where
  RemoteExec lhs <> RemoteExec rhs = RemoteExec $ liftA2 (<>) lhs rhs

instance IsString (RemoteExec m) where
  fromString = RemoteExec . pure . fromString


test123 :: RemoteExec m
test123 = "test"


newtype Local m = Local
  { runLocal :: forall a b . (JSON.FromJSON a, JSON.ToJSON b) => a -> VimT m b
  }


--data Execution m a b where
----HybridExec :: Execution 'Remote m b c -> Execution 'Local m a b -> Execution 'Local m a c
--  LocalExec :: (a -> VimT m b) -> Execution m a b
--  RemoteExec :: [Local m] -> String -> Execution m () b

--RemoteEval  -- todo


--instance Category (Execution m) where
--  id = LocalExec $ runKleisli id
--  (.) = curry $ \case
--    (LocalExec lhs, LocalExec rhs) ->
--      LocalExec $ runKleisli $ Kleisli lhs . Kleisli rhs
--
--    (RemoteExec lhs, RemoteExec rhs) ->
--      RemoteExec $ rhs <> " | " <> lhs
--
--    (RemoteExec lhs, LocalExec rhs) ->
--      LocalExec $ rhs >=> \() -> Vim.evaluate lhs
--
--    (lhs@(LocalExec _), rhs@(RemoteExec _)) ->
--      undefined



--instance Semigroup (Execution k) where
--  (<>) = curry $ \case
--    (RemoteExec lhs, RemoteExec rhs) ->
--      RemoteExec (lhs <> rhs)
--
--    (LocalExec lhs, LocalExec rhs) ->
--      LocalExec $ \arg -> do
--        json <- JSON.toJSON <$> lhs arg
--        case json of
--          JSON.Success -> 
--          JSON.Error e -> error e

    --(LocalExec lhs, LocalExec rhs) -> LocalExec (const @_ @() $ pure ())
 -- (lhs >=> pure . fromJSON . toJSON >=> rhs)  -- TBC
    -- (LocalExec lhs, LocalExec rhs) -> RemoteExec undefined -- (lhs >=> pure . fromJSON . toJSON >=> rhs)  -- TBC
    
    


--compileHybrid :: Execution k -> VimT m ()
--compileHybrid = {-\case
--  RemoteExec script -> -}undefined



{- Need to matching up levels:


  1   ( L R             R R L R L
  2         ( L R L R )


-}







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
--Disable        putStrLn ""

--Disable        putStrLn =<< Posix.readSymbolicLink "/proc/self/exe"
        Posix.readSymbolicLink "/proc/self/exe"
        --putStrLn =<< Posix.readSymbolicLink "/proc/11900/fd/0"

        mode <- Posix.fileMode <$> Posix.getSymbolicLinkStatus "/proc/self/fd/0"
--Disable        putStrLn $ "ownerReadMode: " ++ show (mode .&. Posix.ownerReadMode /= 0)
--Disable        putStrLn $ "ownerWriteMode: " ++ show (mode .&. Posix.ownerWriteMode /= 0)

--Disable        putStrLn ""

        res <- hush <$> try @IOException (Posix.getSymbolicLinkStatus "/does/not/exist")

        --putStrLn $ show $ Posix.fileMode $ fromJust res

        --traverse putStrLn =<< readDirectory "/proc"
--Disable        traverse (putStrLn . show) =<< findVimProcesses

        pure ()

        --HUnit.assertEqual "" "" ""

    , HUnit.testCase "RemoteExec registers continuation in vector" $ do
        let RemoteExec s = continuation "a:mode" $ \() -> pure (10 :: Integer)
        HUnit.assertEqual "" "ch_evalexpr(g:vimd,[7,a:mode])" $
          evalState s (Vector.replicate 7 undefined)
    ]
  ]




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











{-

IDEA:


ghc errors have several ranges
write an extension to highlight those ranges in vim


Loader.hs:142:38: error:
    • Couldn't match type ‘b’ with ‘b1’
      ‘b’ is a rigid type variable bound by
        the type signature for:
          continuation :: forall a b (m :: * -> *).
                          (JSON.FromJSON a, JSON.ToJSON b) =>
                          String -> (a -> VimT m b) -> RemoteExec m
        at Loader.hs:139:1-93
      ‘b1’ is a rigid type variable bound by
        a type expected by the context:
          forall a1 b1. (JSON.FromJSON a1, JSON.ToJSON b1) => a1 -> VimT m b1
        at Loader.hs:142:25-38
      Expected type: Control.Monad.Trans.Cont.ContT
                       ()
                       (Control.Monad.Trans.Reader.ReaderT
                          (GHC.MVar.MVar (Vim.ChannelState m)) m)
                       b1
        Actual type: Control.Monad.Trans.Cont.ContT
                       ()
                       (Control.Monad.Trans.Reader.ReaderT
                          (GHC.MVar.MVar (Vim.ChannelState m)) m)
                       b
    • In the first argument of ‘Continuation’, namely ‘k’
      In the second argument of ‘Vector.snoc’, namely ‘Continuation k’
      In the first argument of ‘modify’, namely
        ‘(`Vector.snoc` Continuation k)’
    • Relevant bindings include
        k :: a -> VimT m b (bound at Loader.hs:140:18)
        continuation :: String -> (a -> VimT m b) -> RemoteExec m
          (bound at Loader.hs:140:1)
    |
142 |   modify (`Vector.snoc` Continuation k)
    |                           



-}
