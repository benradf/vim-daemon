{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Vim
  ( VimT
  , evaluate
  , ex
  , exs
  , normal
  , redraw
  , runVimT
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as B
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Text as T
import Data.Vector ((!?))
import qualified Data.Vector as V


data Callback m where
  Callback
    :: forall a b m . (JSON.FromJSON a, JSON.ToJSON b)
    => (a -> ReaderT (MVar (ChannelState m)) m b)
    -> Callback m


type Channel m = JSON.Value -> m ()

data ChannelState m = ChannelState
  { csCallbacks :: IntMap (Callback m)
  , csNextSeqNum :: Int
  , csOutputChannel :: Channel m
  , csDefaultHandler :: Int -> Callback m
  }

type VimT m a = MonadIO m => ContT () (ReaderT (MVar (ChannelState m)) m) a

runVimT
  :: ( MonadIO m
     , JSON.FromJSON i
     , JSON.ToJSON o
     )
  => Channel m -> (i -> VimT m o) -> VimT m a -> m (Channel m)
runVimT output handler m = do
  csRef <- liftIO $ newMVar $ ChannelState
    { csCallbacks = mempty
    , csNextSeqNum = -1
    , csOutputChannel = output
    , csDefaultHandler = \seqNum -> Callback $ \request -> flip runContT (const $ pure ()) $ do
        response <- handler request
        lift $ lift $ output $ JSON.toJSON $ Message seqNum $ JSON.toJSON response
    }
  runReaderT (runContT m (const $ pure ())) csRef

  -- TODO: Clean up all the case statements (use ExceptT instead?).
  pure $ \value -> case JSON.parseEither JSON.parseJSON value of
    Left e -> error e
    Right (Message seqNum payload) -> do
      cs <- liftIO $ readMVar csRef
      let handler (Callback callback) =
            case JSON.parseEither JSON.parseJSON payload of
              Left e2 -> error $ e2 ++ "\n" ++ "PAYLOAD:\n" ++ show payload
              Right x -> JSON.toJSON <$> runReaderT (callback x) csRef
      void $ case IntMap.lookup seqNum (csCallbacks cs) of
        Just callback -> handler callback  -- TODO: Need to remove callback from map.
        Nothing -> handler (csDefaultHandler cs seqNum)

  where
    --defaultHandlerCallback = Callback $ flip runContT (lift . output . JSON.toJSON) . handler
    defaultHandlerCallback = Callback $
      \request -> case JSON.parseEither JSON.parseJSON request of
        Left e2 -> error e2
        Right (Message seqNum payload) -> flip runContT (lift . output . JSON.toJSON) $
          case JSON.fromJSON payload of
            JSON.Error e3 -> error e3
            JSON.Success x -> Message seqNum . JSON.toJSON <$> handler x


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


evaluate :: JSON.FromJSON a => String -> VimT m a
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


ex :: String -> VimT m ()
ex expression =
  liftIO $ B.putStrLn $ JSON.encode $ JSON.Array $ V.fromList
    [ JSON.String $ T.pack "ex"
    , JSON.String $ T.pack expression
    ]

exs :: [String] -> VimT m ()
exs = ex . List.intercalate " | "  -- "\n" also works

normal :: String -> VimT m ()
normal commands =
  liftIO $ B.putStrLn $ JSON.encode $ JSON.Array $ V.fromList
    [ JSON.String $ T.pack "normal"
    , JSON.String $ T.pack commands
    ]

redraw :: Bool -> VimT m ()
redraw force =
  liftIO $ B.putStrLn $ JSON.encode $ JSON.Array $ V.fromList
    [ JSON.String $ T.pack "redraw"
    , JSON.String $ T.pack $ if force then "force" else ""
    ]
