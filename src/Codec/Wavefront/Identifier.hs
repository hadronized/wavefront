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

module Codec.Wavefront.Identifier (
    -- * Identifier
    identifier
  ) where

import Data.Attoparsec.Text
import Data.Char ( isDigit, isLetter )
import Data.Text ( Text )

identifier :: Parser Text
identifier = takeWhile1 $ \c -> isDigit c || isLetter c
