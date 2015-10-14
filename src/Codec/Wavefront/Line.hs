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

module Codec.Wavefront.Line where

-- |A line is a pair of indexes. @'Line' vi vti@. @vi@ references the locations and @vti@ indexes
-- the texture coordinates. If @vti == 'Nothing'@, then that 'Line' doesn’t have texture coordinates
-- associated with.
--
-- Keep in mind that 'Line' doesn’t represent a polygonal line directly. It represents a line index,
-- which is a pair. In theory, a polygonal line is 2 'Line's.
data Line = Line {
    lineLocIndex :: {-# UNPACK #-} !Int
  , lineTexCoordIndex :: !(Maybe Int)
  } deriving (Eq,Show)

