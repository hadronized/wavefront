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

module Codec.Wavefront.TexCoord where

-- |A texture coordinate is a 3D-floating vector. You can access to its components by pattern
-- matching on them:
--
-- @
--   let TexCoord r s t = TexCoord 0.1 0.2 0.3
-- @
--
-- That type is strcit and unboxed.
data TexCoord = TexCoord {
    texcoordR :: {-# UNPACK #-} !Float
  , texcoordS :: {-# UNPACK #-} !Float
  , texcoordT :: {-# UNPACK #-} !Float
  } deriving (Eq,Show)

