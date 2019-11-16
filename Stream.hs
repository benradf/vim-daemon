{-# LANGUAGE LambdaCase #-}

module Stream where


data Stream m a
  = Stream (m a) (Stream m a) 
  | EndOfStream


fromList :: Applicative m => [a] -> Stream m a
fromList = \case
  x : xs -> Stream (pure x) (fromList xs)
  [] -> EndOfStream

