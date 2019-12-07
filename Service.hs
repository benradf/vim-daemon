{-# LANGUAGE TypeFamilies #-}

module Service where


type VimT m a = ()  -- TODO: Extract VimT into its own module.

--class Service s where
--  type Request s
--  type Response s
--
--  handler :: Request s -> VimT m (Response s)
