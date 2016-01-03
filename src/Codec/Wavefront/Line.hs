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

-- |A line index is a pair of indices. @'LineIndex' vi vti@. @vi@ references the locations and @vti@
-- indexes the texture coordinates. If @vti == 'Nothing'@, then that 'LineIndex' doesn’t have
-- texture coordinates associated with.
data LineIndex = LineIndex {
    lineLocIndex :: {-# UNPACK #-} !Int
  , lineTexCoordIndex :: !(Maybe Int)
  } deriving (Eq,Show)

-- A line gathers two line indices accessible by pattern matching or 'lineIndexA' and 'lineIndexB'.
data Line = Line {
    lineIndexA :: LineIndex
  , lineIndexB :: LineIndex
  } deriving (Eq,Show)
