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

module Codec.Wavefront.IO where

import Codec.Wavefront.Object ( WavefrontOBJ, lexer )
import Codec.Wavefront.Token ( tokenize )
import Control.Monad.IO.Class ( MonadIO(..) )
import qualified Data.Text.IO as T ( readFile )

fromFile :: (MonadIO m) => FilePath -> m (Either String WavefrontOBJ)
fromFile fd = liftIO $ fmap (fmap lexer . tokenize) (T.readFile fd)
