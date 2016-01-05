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
-- Currently, you can parse a file and get a 'WavefrontOBJ' with the 'fromFile'
-- function.
-----------------------------------------------------------------------------

module Codec.Wavefront (
    -- * Vertex location
    Location(..)
    -- * Vertex texture coordinates
  , TexCoord(..)
    -- * Vertex normals
  , Normal(..)
    -- * Points
  , Point(..)
    -- * Lines
  , Line(..)
  , LineIndex(..)
    -- * Faces
  , Face(..)
  , FaceIndex(..)
  , pattern Triangle
  , pattern Quad
    -- * Element
  , Element(..)
    -- * Object
  , WavefrontOBJ(..)
    -- * Re-exports
  , module Codec.Wavefront.IO
  ) where

import Codec.Wavefront.Element
import Codec.Wavefront.Face
import Codec.Wavefront.IO
import Codec.Wavefront.Line
import Codec.Wavefront.Location
import Codec.Wavefront.Normal
import Codec.Wavefront.Object
import Codec.Wavefront.Point
import Codec.Wavefront.TexCoord
