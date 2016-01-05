{-# LANGUAGE PatternSynonyms #-}

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

-- |A face index is a triplet of indices. @'FaceIndex' vi vti vni@ is a face that indexes the
-- locations with @vi@, the texture coordinates with @vti@ and the normals with @vni@. An index set
-- to 'Nothing' means /no information/. That is, if @vni == 'Nothing'@, then that 'FaceIndex'
-- doesn’t have a normal associated with.
data FaceIndex = FaceIndex {
    faceLocIndex :: {-# UNPACK #-} !Int
  , faceTexCoordIndex :: !(Maybe Int)
  , faceNorIndex :: !(Maybe Int)
  } deriving (Eq,Show)

-- |A face gathers several 'FaceIndex' to build up faces. It has a least three vertices
data Face = Face FaceIndex FaceIndex FaceIndex [FaceIndex] deriving (Eq,Show)

pattern Triangle :: FaceIndex -> FaceIndex -> FaceIndex -> Face
pattern Triangle a b c = Face a b c []

pattern Quad :: FaceIndex -> FaceIndex -> FaceIndex -> FaceIndex -> Face
pattern Quad a b c d = Face a b c [d]
