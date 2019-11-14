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

import qualified Lex as Lex


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
    :: forall a b m . JSON.FromJSON a
    => (a -> ReaderT (MVar (ChannelState m)) m b)
    -> Callback m


type Channel m = JSON.Value -> m ()

data ChannelState m = ChannelState
  { csCallbacks :: IntMap (Callback m)
  , csNextSeqNum :: Int
  , csOutputChannel :: Channel m
  , csDefaultHandler :: Callback m
  }

type VimT m a = ContT () (ReaderT (MVar (ChannelState m)) m) a

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
              Right x -> void $ runReaderT (callback x) csRef
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
ex expression =
  liftIO $ B.putStrLn $ JSON.encode $ JSON.Array $ V.fromList
    [ JSON.String $ T.pack "ex"
    , JSON.String $ T.pack expression
    ]

redraw :: MonadIO m => Bool -> VimT m ()
redraw force =
  liftIO $ B.putStrLn $ JSON.encode $ JSON.Array $ V.fromList
    [ JSON.String $ T.pack "redraw"
    , JSON.String $ T.pack $ if force then "force" else ""
    ]

normal :: MonadIO m => String -> VimT m ()
normal commands =
  liftIO $ B.putStrLn $ JSON.encode $ JSON.Array $ V.fromList
    [ JSON.String $ T.pack "normal"
    , JSON.String $ T.pack commands
    ]

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


-- TODO: functionality to "pin visual selection"
-- this would open a new window just big enough to hold the current selection
-- and set win min size etc appropriately
-- it should also return the cursor to exactly where it was in the original
-- window
-- maybe it should be bound to a g "go" command?
main :: IO ()
main = do
  Lex.main

oldMain = do
  hSetBuffering stdout LineBuffering

  let defaultHandler value = do
        ex $ "call ch_setoptions(g:channel, {'timeout': 100})"
        ex $ "echom 'defaultHandler got message: " <> show @String value <> "'"
        lastLine <- evaluate @Integer "line('$')"
        ex "echom 'test'"
        normal "gg"
        --redraw True

  inputCh <- runVimT (B.putStrLn . JSON.encode) defaultHandler $ do
    lineNum <- evaluate @Integer "line('.')"
    liftIO $ appendFile "/tmp/vim-server.log" $ "evaluate result is " <> show lineNum <> "\n"
    pure ()

  let decode
        = fromMaybe (error "invalid json")
        . JSON.decode @JSON.Value

  void $ join $ traverse (inputCh . decode) . B.lines <$> B.getContents















type Token = Int

type LexerState = [(NE.NonEmpty Char, Token)]

  --case filter (null . fst) newState of
      --put $ catMaybes $ bitraverse identity Just . first NE.nonEmpty <$> newState

--trace _ = id

lexer :: [Char] -> State LexerState [Token]
lexer [] = pure []
lexer string = do
  (token, string') <- step string
  case token of
    Just t -> (t :) <$> lexer string'
    Nothing -> lexer string'

step :: [Char] -> State LexerState (Maybe Token, [Char])
step (c : cs) = do
  existingState <- get
  
  reduceResult <- gets $ catMaybes . fmap (reduce c)
  let newState = catMaybes $ bitraverse id Just . first NE.nonEmpty <$> reduceResult

  if trace ("  c = " <> show c <> "\n  cs = " <> show cs <> "\n  reduceResult = " <> show reduceResult <> "\n  newState = " <> show newState) $ not (null newState) && not (null cs)
    then trace "CONT" $ put newState $> (Nothing, cs)                         -- Continue trying to match tokens.
    else case reduceResult of
      [([], token)] -> trace ("EMIT: " <> show token) $ put tokens $> (Just token, c : cs)  -- Emit a token.
      [] -> trace ("SKIP") $ put tokens $> (Nothing, cs)  -- Skip letters not part of known token.
      _ -> error "ambiguous"
step [] = trace "DONE" $ pure (Nothing, [])


-- TODO: Move the string being parsed into the state and use it for back tracking
data TokenMatcher
  = TokenMatching Token (NE.NonEmpty Char)
  | TokenMatched Token [Char]  -- where [Char] is string to continue from if we back track here

reduce
  :: Char
  -> (NE.NonEmpty Char, Token)
  -> Maybe ([Char], Token)
reduce c' (c :| cs, token) = guard (c == c') *> Just (cs, token)

tokens :: [(NE.NonEmpty Char, Token)]
tokens =
  [ (NE.fromList "->", 1)
  , (NE.fromList "-->", 2)
  , (NE.fromList "<-", 3)
  , (NE.fromList "::", 4)
  , (NE.fromList ":::", 5)
  , (NE.fromList "=>", 6)
  , (NE.fromList "$", 7)
  ]













{-
data Location = Location
  { line :: Integer
  , column :: Integer
  }

data Range = Range
  { from :: Location
  , to :: Location
  }

--   ( x -> ( a -> b ) -> c -> ( d -> e -> f ) )

tokens =
  [ ("->", Arrow)
  , ("(", LeftParen)
  , (")", RightParen)
  , ("::", TypeAnnotation)
  , ("\n", NewLine)
  ]

tokenize :: [Char] -> [Token]

data Node
  = Leaf Range Range -- inner and outer ranges
  | Node [Node]

data Context
  = Root
  | Context [Node] Context [Node]

data Selection
  = Selection [Node] Context

extendLeft :: Selection -> Maybe Selection
extendLeft (Selection _ Root) = Nothing
extendLeft (Selection nodes (Context [] _ _)) = Nothing
extendLeft (Selection nodes (Context (l : ls) c rs)) =
  Just $ Selection (l : nodes) (Context ls c rs)

extendUp :: Selection -> Maybe Selection
extendUp (Selection _ Root) = Nothing
extendUp (Selection nodes (Context ls c rs)) =
  Just $ Selection (reverse ls ++ nodes ++ rs) c
-}
