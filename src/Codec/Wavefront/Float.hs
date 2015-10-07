-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------

module Codec.Wavefront.Float (
    -- * Float parser
    float
  ) where

import Data.Attoparsec.Text

float :: Parser Float
float = fmap realToFrac double
