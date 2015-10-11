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
    module Codec.Wavefront.Object
  , module Codec.Wavefront.IO
  ) where

import Codec.Wavefront.Object
import Codec.Wavefront.IO
