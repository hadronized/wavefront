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

module Codec.Wavefront.Element (
    -- * Element
    Element(..)
  ) where

import Data.Text ( Text )
import Numeric.Natural ( Natural )

-- |An element holds a value along with the user-defined object’s name (if any), the associated
-- groups, the used material and the smoothing group the element belongs to (if any). Those values
-- can be used to sort the data per object or per group and to lookup materials.
data Element a = Element {
    elObject :: Maybe Text
  , elGroups :: [Text]
  , elMtl :: Maybe Text
  , elSmoothingGroup :: Natural
  , elValue :: a
  } deriving (Eq,Show)
