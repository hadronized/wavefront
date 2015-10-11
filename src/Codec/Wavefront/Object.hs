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
import Control.Monad.State ( MonadState, State, execState, gets, modify )
import Data.DList ( DList, append, empty, fromList, snoc )
import Data.Text ( Text )
import Data.Foldable ( traverse_ )

data Element a = Element {
    elGroups :: [Text]
  , elMtl    :: Maybe Text
  , elValue  :: a
  } deriving (Eq,Show)

data Ctxt = Ctxt {
    -- |Name of the object. 'Nothing' means that the object is not user-defined.
    ctxtName :: Maybe Text
    -- |Locations.
  , ctxtLocations :: DList Location
    -- |Texture coordinates.
  , ctxtTexCoords :: DList TexCoord
    -- |Normals.
  , ctxtNormals :: DList Normal
    -- |Points.
  , ctxtPoints :: DList (Element Point)
    -- |Lines.
  , ctxtLines :: DList (Element Line)
    -- |Faces.
  , ctxtFaces :: DList (Element Face)
    -- |Current groups.
  , ctxtCurrentGroups :: [Text]
    -- |Current material.
  , ctxtCurrentMtl :: Maybe Text
    -- |Material libraries.
  , ctxtMtlLibs :: DList Text
  } deriving (Eq,Show)

emptyObject :: Ctxt 
emptyObject = Ctxt {
    ctxtName = Nothing
  , ctxtLocations = empty
  , ctxtTexCoords = empty
  , ctxtNormals = empty
  , ctxtPoints = empty
  , ctxtLines = empty
  , ctxtFaces = empty
  , ctxtCurrentGroups = []
  , ctxtCurrentMtl = Nothing
  , ctxtMtlLibs = empty
  }

lexer :: TokenStream -> [Ctxt]
lexer stream = execState (traverse_ consume stream) [emptyObject]
  where
    consume tk = case tk of
      TknV v -> do
        locations <- gets (ctxtLocations . head)
        modifyHead $ \ctxt -> ctxt { ctxtLocations = locations `snoc` v }
      TknVN vn -> do
        normals <- gets $ ctxtNormals . head
        modifyHead $ \ctxt -> ctxt { ctxtNormals = normals `snoc` vn }
      TknVT vt -> do
        texCoords <- gets $ ctxtTexCoords . head
        modifyHead $ \ctxt -> ctxt { ctxtTexCoords = texCoords `snoc` vt }
      TknP p -> do
        (points,element) <- prepareElement ctxtPoints
        modifyHead $ \ctxt -> ctxt { ctxtPoints = points `append` fmap element (fromList p) }
      TknL l -> do
        (lines,element) <- prepareElement ctxtLines
        modifyHead $ \ctxt -> ctxt { ctxtLines = lines `append` fmap element (fromList l) }
      TknF f -> do
        (faces,element) <- prepareElement ctxtFaces
        modifyHead $ \ctxt -> ctxt { ctxtFaces = faces `append` fmap element (fromList f) }
      TknG g -> modifyHead $ \ctxt -> ctxt { ctxtCurrentGroups = g }
      TknO o -> modify $ (emptyObject { ctxtName = Just o } :)
      TknMtlLib l -> do
        libs <- gets (ctxtMtlLibs . head)
        modifyHead $ \ctxt -> ctxt { ctxtMtlLibs = libs `append` fromList l }
      TknUseMtl mtl -> modifyHead $ \ctxt -> ctxt { ctxtCurrentMtl = Just mtl }

modifyHead :: (MonadState [s] m) => (s -> s) -> m ()
modifyHead f = modify $ \(h:t) -> f h : t

-- Prepare to create a new Element by retrieving its associated list.
prepareElement :: (Ctxt -> DList (Element a)) -> State [Ctxt] (DList (Element a),a -> Element a)
prepareElement field = do
  (aList,grp,mtl) <- gets $ (\ctxt -> (field ctxt,ctxtCurrentGroups ctxt,ctxtCurrentMtl ctxt)) . head
  pure (aList,Element grp mtl)
