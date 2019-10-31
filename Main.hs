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
import qualified Data.Text as T
import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import Prelude
import System.IO


-- let g:job = job_start(['bash', '-c', 'tee -a /tmp/vim-server.log | dist/build/vim-server/vim-server 2>&1 | tee -a /tmp/vim-server.log'], {'mode': 'json'})
-- let g:channel = job_getchannel(g:job)
-- echo ch_evalexpr(g:channel, winlayout())

--putStrLn "[\"ex\", \"echo RECEIVED\"]"
--putStrLn "[\"normal\", \"0\"]"
--putStrLn "[\"redraw\", \"\"]"


data Command
  = RedrawCommand Bool
  | ExCommand String
  | NormalCommand String
  | ExprCommand String
  | CallCommand String [String]


--data VimT m a = VimT
--  { vimCallbacks :: IntMap (JSON.Value -> VimT m ())
--  , vimCallbackIndex :: Int
--  }


data Callback m where
  Callback
    :: forall a m . JSON.FromJSON a
    => (a -> ReaderT (MVar (ChannelState m)) m ())
    -> Callback m


type Channel m = JSON.Value -> m ()

data ChannelState m = ChannelState
  { csCallbacks :: IntMap (Callback m)
  , csNextSeqNum :: Int
  , csOutputChannel :: Channel m
  , csDefaultHandler :: Callback m
  }

type VimT m a = ContT () (ReaderT (MVar (ChannelState m)) m) a

runVimT :: MonadIO m => Channel m -> (JSON.Value -> VimT m a) -> VimT m a -> m (Channel m)
runVimT output handler m = do
  csRef <- liftIO $ newMVar $ ChannelState
    { csCallbacks = mempty
    , csNextSeqNum = -1
    , csOutputChannel = output
    , csDefaultHandler = Callback $ flip runContT (const $ pure ()) . handler
    }
  runReaderT (runContT m (const $ pure ())) csRef

  -- TODO: Clean up all the case statements (use ExceptT instead?).
  pure $ \value -> case JSON.parseEither JSON.parseJSON value of
    Left e -> error e
    Right (Message seqNum payload) -> do
      cs <- liftIO $ readMVar csRef
      let handler (Callback callback) =
            case JSON.parseEither JSON.parseJSON payload of
              Left e2 -> error e2
              Right x -> runReaderT (callback x) csRef
      case IntMap.lookup seqNum (csCallbacks cs) of
        Just callback -> handler callback  -- TODO: Need to remove callback from map.
        Nothing -> handler $ csDefaultHandler cs


data Message = Message
  { mSeqNum :: Int
  , mPayload :: JSON.Value
  }
  deriving Show

instance JSON.ToJSON Message where
  toJSON message = JSON.toJSON
    [ JSON.toJSON $ mSeqNum message
    , mPayload message
    ]

instance JSON.FromJSON Message where
  parseJSON = JSON.withArray "Message" $ \array -> Message
    <$> maybe (fail "missing seq num") JSON.parseJSON (array !? 0)
    <*> maybe (fail "missing payload") JSON.parseJSON (array !? 1)


evaluate :: (JSON.FromJSON a, MonadIO m) => String -> VimT m a
evaluate expression = ContT $ \k -> do
  csRef <- ask
  seqNum <- liftIO $ modifyMVar csRef $ \cs ->
    let seqNum = pred $ csNextSeqNum cs
    in pure $ (, seqNum) $ cs
      { csCallbacks = IntMap.insert seqNum (Callback k) $ csCallbacks cs
      , csNextSeqNum = seqNum
      }

  liftIO $ B.putStrLn $ JSON.encode $ JSON.Array $ V.fromList
    [ JSON.String $ T.pack "expr"
    , JSON.String $ T.pack expression
    , JSON.Number $ fromInteger $ toInteger seqNum
    ]

  pure ()


ex :: MonadIO m => String -> VimT m ()
ex expression = ContT $ \k -> do
  liftIO $ B.putStrLn $ JSON.encode $ JSON.Array $ V.fromList
    [ JSON.String $ T.pack "ex"
    , JSON.String $ T.pack expression
    ]

  k ()


handleMessage :: JSON.Value -> JSON.Value
handleMessage = id

process :: Maybe JSON.Value -> IO ()
process msg = case msg of
  Just v@(JSON.Array values) -> do
    let sequence_number = values ! 0
    B.putStrLn $ JSON.encode $ JSON.Array $ V.fromList
      [ sequence_number
      , handleMessage (values ! 1)
      ]
  _ -> pure ()


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let defaultHandler value =
        ex $ "echo 'defaultHandler got message: " <> show value <> "'"

  inputCh <- runVimT (B.putStrLn . JSON.encode) defaultHandler $ do
    lineNum <- evaluate @Integer "line('.')"
    liftIO $ appendFile "/tmp/vim-server.log" $ "evaluate result is " <> show lineNum <> "\n"
    pure ()

  let decode
        = fromMaybe (error "invalid json")
        . JSON.decode @JSON.Value

  void $ join $ traverse (inputCh . decode) . B.lines <$> B.getContents
