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
    Location
  , locX
  , locY
  , locZ
  , locW
    -- * Vertex texture coordinates
  , TexCoord
  , texcoordR
  , texcoordS
  , texcoordT
    -- * Vertex normals
  , Normal
  , norX
  , norY
  , norZ
    -- * Points
  , Point
  , pointLocIndex
    -- * Lines
  , Line
  , lineLocIndex
  , lineTexCoordIndex
    -- * Faces
  , Face
  , faceLocIndex
  , faceTexCoordIndex
  , faceNorIndex
    -- * Element
  , Element
  , elObject
  , elGroups
  , elMtl
  , elValue
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
