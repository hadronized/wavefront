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

module Codec.Wavefront.Lexer where

import Codec.Wavefront.Element
import Codec.Wavefront.Face
import Codec.Wavefront.Line
import Codec.Wavefront.Location
import Codec.Wavefront.Normal
import Codec.Wavefront.Point
import Codec.Wavefront.Token
import Codec.Wavefront.TexCoord
import Data.DList ( DList, append, empty, fromList, snoc )
import Data.Text ( Text )
import Control.Monad.State ( State, execState, gets, modify )
import Data.Foldable ( traverse_ )
import Numeric.Natural ( Natural )

-- |The lexer context. The result of lexing a stream of tokens is this exact type.
data Ctxt = Ctxt {
    -- |Locations.
    ctxtLocations :: DList Location
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
    -- |Current object.
  , ctxtCurrentObject :: Maybe Text
    -- |Current groups.
  , ctxtCurrentGroups :: [Text]
    -- |Current material.
  , ctxtCurrentMtl :: Maybe Text
    -- |Material libraries.
  , ctxtMtlLibs :: DList Text
    -- |Current smoothing group.
  , ctxtCurrentSmoothingGroup :: Natural
  } deriving (Eq,Show)

-- |The empty 'Ctxt'. Such a context exists at the beginning of the token stream and gets altered
-- as we consume tokens.
emptyCtxt :: Ctxt 
emptyCtxt = Ctxt {
    ctxtLocations = empty
  , ctxtTexCoords = empty
  , ctxtNormals = empty
  , ctxtPoints = empty
  , ctxtLines = empty
  , ctxtFaces = empty
  , ctxtCurrentObject = Nothing
  , ctxtCurrentGroups = ["default"]
  , ctxtCurrentMtl = Nothing
  , ctxtMtlLibs = empty
  , ctxtCurrentSmoothingGroup = 0
  }

-- |The lexer function, consuming tokens and yielding a 'Ctxt'.
lexer :: TokenStream -> Ctxt
lexer stream = execState (traverse_ consume stream) emptyCtxt
  where
    consume tk = case tk of
      TknV v -> do
        locations <- gets ctxtLocations
        modify $ \ctxt -> ctxt { ctxtLocations = locations `snoc` v }
      TknVN vn -> do
        normals <- gets ctxtNormals
        modify $ \ctxt -> ctxt { ctxtNormals = normals `snoc` vn }
      TknVT vt -> do
        texCoords <- gets ctxtTexCoords
        modify $ \ctxt -> ctxt { ctxtTexCoords = texCoords `snoc` vt }
      TknP p -> do
        (pts,element) <- prepareElement ctxtPoints
        modify $ \ctxt -> ctxt { ctxtPoints = pts `append` fmap element (fromList p) }
      TknL l -> do
        (lns,element) <- prepareElement ctxtLines
        modify $ \ctxt -> ctxt { ctxtLines = lns `append` fmap element (fromList l) }
      TknF f -> do
        (fcs,element) <- prepareElement ctxtFaces
        modify $ \ctxt -> ctxt { ctxtFaces = fcs `snoc` element f }
      TknG g -> modify $ \ctxt -> ctxt { ctxtCurrentGroups = g }
      TknO o -> modify $ \ctxt -> ctxt { ctxtCurrentObject = Just o }
      TknMtlLib l -> do
        libs <- gets ctxtMtlLibs
        modify $ \ctxt -> ctxt { ctxtMtlLibs = libs `append` fromList l }
      TknUseMtl mtl -> modify $ \ctxt -> ctxt { ctxtCurrentMtl = Just mtl }
      TknS sg -> modify $ \ctxt -> ctxt { ctxtCurrentSmoothingGroup = sg }

-- Prepare to create a new 'Element' by retrieving its associated list.
prepareElement :: (Ctxt -> DList (Element a)) -> State Ctxt (DList (Element a),a -> Element a)
prepareElement field = do
  (aList,obj,grp,mtl,sg) <- gets $ (\ctxt -> (field ctxt,ctxtCurrentObject ctxt,ctxtCurrentGroups ctxt,ctxtCurrentMtl ctxt,ctxtCurrentSmoothingGroup ctxt))
  pure (aList,Element obj grp mtl sg)
