{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Coerce (coerce)
import Data.Functor
import Data.Functor.Identity
import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup
import qualified Data.Text as T
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Streaming.Prelude (Of, Stream)

import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import Prelude
import System.IO
import GHC.Generics

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


-- call ch_logfile('/tmp/channel.log', 'w')
-- let g:job = job_start(['dist/build/vim-server/vim-server'], {'mode': 'json'})
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

redraw :: Bool -> VimT m ()
redraw force =
  liftIO $ B.putStrLn $ JSON.encode $ JSON.Array $ V.fromList
    [ JSON.String $ T.pack "redraw"
    , JSON.String $ T.pack $ if force then "force" else ""
    ]

normal :: String -> VimT m ()
normal commands =
  liftIO $ B.putStrLn $ JSON.encode $ JSON.Array $ V.fromList
    [ JSON.String $ T.pack "normal"
    , JSON.String $ T.pack commands
    ]


-- TODO: functionality to "pin visual selection"
-- this would open a new window just big enough to hold the current selection
-- and set win min size etc appropriately
-- it should also return the cursor to exactly where it was in the original
-- window
-- maybe it should be bound to a g "go" command?
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let defaultHandler value = do
        ex $ "call ch_setoptions(g:channel, {'timeout': 100})"
        ex $ "echom 'defaultHandler got message: " <> show @String value <> "'"
        lastLine <- evaluate @Integer "line('$')"
        ex "echom 'test'"
        normal "gg"

        pure $ JSON.object
          [ T.pack "message" .= "The result message"
          ]

  let defaultHandler2 :: MonadIO m => String -> VimT m JSON.Value
      defaultHandler2 value = do
        loc@(Location.Location l c) <- getLocation
        liftIO $ appendFile "/tmp/vim-server.log" $ "defaultHandler2 received " <> show @String value <> "\n"
        bv <- BufferView.makeBufferView loc $ \from to ->
                evaluate $ "getline(" ++ show from ++ ", " ++ show to ++ ")"
        let showLoc (Location.Located (Location.Location n m) x) = show n ++ ":" ++ show m ++ ":" ++ show x
        tokensBefore <- S.toList_ $ S.take 10 $ lexer $ BufferView.bvBefore bv
        tokensAfter <- S.toList_ $ S.take 10 $ lexer $ BufferView.bvAfter bv
--      tokensBefore <- Stream.toList =<< Stream.take 10 <$> lexer (BufferView.bvBefore bv)
--      tokensAfter <- Stream.toList =<< Stream.take 10 <$>  lexer (BufferView.bvAfter bv)
        pure $ JSON.object
          [ T.pack "line" .= l
          , T.pack "column" .= c
          , T.pack "tokensBefore" .= (Location.unLocated <$> tokensBefore)
          , T.pack "tokensAfter" .= (Location.unLocated <$> tokensAfter)
          ]

  inputCh <- runVimT (B.putStrLn . JSON.encode) defaultHandler2 $ do
    lineNum <- evaluate @Integer "line('.')"
    liftIO $ appendFile "/tmp/vim-server.log" $ "evaluate result is " <> show lineNum <> "\n"
    pure ()

  let decode
        = fromMaybe (error "invalid json")
        . JSON.decode @JSON.Value

  void $ join $ traverse (inputCh . decode) . B.lines <$> B.getContents





getLocation :: VimT m Location.Location
getLocation = Location.Location <$> evaluate "line('.')" <*> evaluate "col('.')"



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
  | Fmap
  | Apply
  | Arrow
  | Implies
  | Mappend
  deriving (Generic, JSON.ToJSON, Show)

--lexer :: Monad m => Lex.StringStream m -> m [Location.Located Token]
lexer :: Monad m => Lex.StringStream m -> Lex.LocatedStream m Token
lexer = Lex.runLexer lexTree
  where
    lexTree = Lex.makeLexTree $ Map.fromList
      [ (",", Comma)
      , ("(", LeftParen)
      , (")", RightParen)
      , ("[", LeftBox)
      , ("]", RightBox)
      , ("{", LeftBrace)
      , ("}", RightBrace)
      , ("\r", CarriageReturn)
      , ("\n", LineFeed)
      , ("<$>", Fmap)
      , ("<*>", Apply)
      , ("->", Arrow)
      , ("=>", Implies)
      , ("<>", Mappend)
      ]



