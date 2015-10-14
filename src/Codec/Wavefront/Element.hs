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

-- |An element holds a value along with the user-defined object’s name (if exists), the associated
-- groups and the used material. Those values can be used to sort the data per object or per group
-- and to lookup materials.
data Element a = Element {
    elObject :: Maybe Text
  , elGroups :: [Text]
  , elMtl    :: Maybe Text
  , elValue  :: a
  } deriving (Eq,Show)
