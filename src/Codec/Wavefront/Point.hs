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

module Codec.Wavefront.Point where

-- |A point is a single index that references the locations. It’s a canonical type that truly
-- represents a polygonal point.
data Point = Point {
    pointLocIndex :: {-# UNPACK #-} !Int
  } deriving (Eq,Show)
