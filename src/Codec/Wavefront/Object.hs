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

module Codec.Wavefront.Object where

import Codec.Wavefront.Face
import Codec.Wavefront.Line
import Codec.Wavefront.Location
import Codec.Wavefront.Normal
import Codec.Wavefront.Point
import Codec.Wavefront.Token
import Codec.Wavefront.TexCoord
import Control.Monad.State ( execState, gets, modify )
import Data.DList ( DList, append, empty, fromList, snoc )
import Data.Text ( Text )
import Data.Foldable ( traverse_ )

data Object = Object {
    -- |Name of the object. 'Nothing' means that the object is not user-defined.
    objName :: Maybe Text
    -- |Groups.
  , objGroups :: DList Text
    -- |Locations.
  , objLocations :: DList Location
    -- |Texture coordinates.
  , objTexCoords :: DList TexCoord
    -- |Normals.
  , objNormals :: DList Normal
    -- |Points.
  , objPoints :: DList Point
    -- |Lines.
  , objLines :: DList Line
    -- |Faces.
  , objFaces :: DList Face
  } deriving (Eq,Show)

emptyObject :: Object
emptyObject = Object {
    objName = Nothing
  , objGroups = empty
  , objLocations = empty
  , objTexCoords = empty
  , objNormals = empty
  , objPoints = empty
  , objLines = empty
  , objFaces = empty
  }

lexer :: TokenStream -> [Object]
lexer stream = execState (traverse_ consume stream) [emptyObject]
  where
    consume tk = case tk of
      TknV v -> do
        locations <- gets $ objLocations . head
        modify $ \(obj:r) -> obj { objLocations = locations `snoc` v } : r
      TknVN vn -> do
        normals <- gets $ objNormals . head
        modify $ \(obj:r) -> obj { objNormals = normals `snoc` vn } : r
      TknVT vt -> do
        texCoords <- gets $ objTexCoords . head
        modify $ \(obj:r) -> obj { objTexCoords = texCoords `snoc` vt } : r
      TknP p -> do
        points <- gets $ objPoints . head
        modify $ \(obj:r) -> obj { objPoints = points `append` fromList p } : r
      TknL l -> do
        lines <- gets $ objLines . head
        modify $ \(obj:r) -> obj { objLines = lines `append` fromList l } : r
      TknF f -> do
        faces <- gets $ objFaces . head
        modify $ \(obj:r) -> obj { objFaces = faces `append` fromList f } : r
      TknG g -> do
        groups <- gets $ objGroups . head
        modify $ \(obj:r) -> obj { objGroups = groups `append` fromList g } : r
      TknO o -> modify $ (emptyObject { objName = Just o } :)
