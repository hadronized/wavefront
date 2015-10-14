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

module Codec.Wavefront.Face where

-- |A face is a triplet of indices. @'Face' vi vti vni@ is a face that indexes the locations with
-- @vi@, the texture coordinates with @vti@ and the normals with @vni@. An index set to 'Nothing'
-- means /no information/. That is, if @vni == 'Nothing'@, then that 'Face' doesn’t have a normal
-- associated with.
--
-- Keep in mind that 'Face' doesn’t represent a polygonal face directly. It represents a face index,
-- which is a triplet. In theory, a polygonal face is 3 'Face's.
data Face = Face {
    faceLocIndex :: {-# UNPACK #-} !Int
  , faceTexCoordIndex :: !(Maybe Int)
  , faceNorIndex :: !(Maybe Int)
  } deriving (Eq,Show)
