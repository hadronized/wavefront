-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------------

module Codec.Wavefront.Location where

-- |A location is a 4-floating vector. You can access to its components by pattern matching on them:
--
-- @
--   let Location x y z w = Location 1 2 3 4
-- @
--
-- That type is strict and unboxed.
data Location = Location {
    locX :: {-# UNPACK #-} !Float
  , locY :: {-# UNPACK #-} !Float
  , locZ :: {-# UNPACK #-} !Float
  , locW :: {-# UNPACK #-} !Float
  } deriving (Eq,Show)
