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

module Codec.Wavefront.Normal where

-- |A normal is a 3-floating vector. You can access to its components by pattern matching on them:
--
-- @
--   let Normal nx ny nz = Normal 0.1 0.2 0.3
-- @
--
-- That type is strict and unboxed.
data Normal = Normal {
    norX :: {-# UNPACK #-} !Float
  , norY :: {-# UNPACK #-} !Float
  , norZ :: {-# UNPACK #-} !Float
  } deriving (Eq,Show)
