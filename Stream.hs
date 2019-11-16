{-# LANGUAGE LambdaCase #-}

module Stream
  ( Stream
  , extract
  , fromList
  , peek
  ) where


data Stream m a
  = Stream (m a) (Stream m a) 
  | EndOfStream


extract :: Applicative m => Stream m a -> m (Maybe (a, Stream m a))
extract = \case
  Stream x xs -> Just <$> ((,) <$> x <*> pure xs)
  EndOfStream -> pure Nothing

peek :: Applicative m => Stream m a -> m (Maybe a)
peek = \case
  Stream x _ -> Just <$> x
  EndOfStream -> pure Nothing

fromList :: Applicative m => [a] -> Stream m a
fromList = \case
  x : xs -> Stream (pure x) (fromList xs)
  [] -> EndOfStream
