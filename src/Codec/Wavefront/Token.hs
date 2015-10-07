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

module Codec.Wavefront.Token where

import Codec.Wavefront.Float
import Codec.Wavefront.Identifier
import Control.Applicative ( Alternative(..), empty )
import Data.Attoparsec.Text as AP
import Data.Text ( Text )
import qualified Data.Text as T ( empty )
import Prelude hiding ( lines )

----------------------------------------------------------------------------------------------------
-- Token -------------------------------------------------------------------------------------------

data Token
  = TknV Location
  | TknVN Normal
  | TknVT TexCoord
  | TknP [Point]
  | TknL [Line]
  | TknF [Face]
  | TknG [Group]
  | TknO Object
    deriving (Eq,Show)

type TokenStream = [Token]

tokenize :: Text -> Either String TokenStream
tokenize = analyseResult False . parse (many1 tokenizer <* endOfInput)
  where
    tokenizer = foldl (<|>) empty
      [
        fmap TknV location
      , fmap TknVN normal
      , fmap TknP points
      , fmap TknL lines
      , fmap TknF faces
      , fmap TknG groups
      , fmap TknO object
      ]

analyseResult :: Bool -> Result TokenStream -> Either String TokenStream
analyseResult partial r = case r of
  Done _ tkns -> Right tkns
  Fail _ _ e -> Left e
  Partial p -> if partial then Left "not completely tokenized" else analyseResult True (p T.empty)

----------------------------------------------------------------------------------------------------
-- Location ----------------------------------------------------------------------------------------

data Location = Location
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
    deriving (Eq,Show)

location :: Parser Location
location = char 'v' *> skipSpace *> parseXYZW <* eol
  where
    parseXYZW = do
      xyz <- float `sepBy1` skipSpace
      case xyz of
        [x,y,z] -> pure (Location x y z 1)
        [x,y,z,w] -> pure (Location x y z w)
        _ -> fail "wrong number of x, y and z arguments for location"

----------------------------------------------------------------------------------------------------
-- Normal ------------------------------------------------------------------------------------------

data Normal = Normal
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
    deriving (Eq,Show)

normal :: Parser Normal
normal = string "vn" *> skipSpace *> parseIJK <* eol
  where
    parseIJK = do
      ijk <- float `sepBy1` skipSpace
      case ijk of
        [i,j,k] -> pure (Normal i j k)
        _ -> fail "wrong number of i, j and k arguments for normal"

----------------------------------------------------------------------------------------------------
-- Texture coordinates -----------------------------------------------------------------------------

data TexCoord = TexCoord
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
    deriving (Eq,Show)

texCoord :: Parser TexCoord
texCoord = string "vt" *> skipSpace *> parseUVW <* eol
  where
    parseUVW = do
      uvw <- float `sepBy1` skipSpace
      case uvw of
        [u,v] -> pure (TexCoord u v 0)
        [u,v,w] -> pure (TexCoord u v w)
        _ -> fail "wrong number of u, v and w arguments for texture coordinates"

----------------------------------------------------------------------------------------------------
-- Points ------------------------------------------------------------------------------------------

data Point = Point {-# UNPACK #-} !Int deriving (Eq,Show)

points :: Parser [Point]
points = char 'p' *> skipSpace *> fmap Point decimal `sepBy1` skipSpace <* eol

----------------------------------------------------------------------------------------------------
-- Lines -------------------------------------------------------------------------------------------

data Line = Line {-# UNPACK #-} !(Int,Maybe Int) deriving (Eq,Show)

-- TODO: ensure we have at least 2 pairs, otherwise it should fail
lines :: Parser [Line]
lines = char 'l' *> skipSpace *> fmap Line parseLinePair `sepBy1` skipSpace <* eol
  where
    parseLinePair = do
      v <- decimal
      slashThenElse (fmap (\vt -> (v, Just vt)) decimal) (pure (v,Nothing))

----------------------------------------------------------------------------------------------------
-- Faces -------------------------------------------------------------------------------------------

data Face = Face {-# UNPACK #-} !(Int,Maybe Int,Maybe Int) deriving (Eq,Show)

-- TODO: ensure we have at least 3 triples, otherwise it should fail
faces :: Parser [Face]
faces = char 'f' *> skipSpace *> fmap Face parseFaceTriple `sepBy1` skipSpace <* eol
  where
    parseFaceTriple = do
      v <- decimal
      slashThenElse (parseVT v) (pure (v,Nothing,Nothing))
    parseVT v = slashThenElse (parseVN v Nothing) $ do
      vt <- decimal
      slashThenElse (parseVN v $ Just vt) (pure (v,Just vt,Nothing))
    parseVN v vt = do
      vn <- decimal
      pure (v,vt,Just vn)

----------------------------------------------------------------------------------------------------
-- Groups ------------------------------------------------------------------------------------------

data Group = Group {-# UNPACK #-} !Text deriving (Eq,Show)

groups :: Parser [Group]
groups = char 'g' *> skipSpace *> fmap Group identifier `sepBy1` skipSpace <* eol

----------------------------------------------------------------------------------------------------
-- Objects -----------------------------------------------------------------------------------------

data Object = Object {-# UNPACK #-} !Text deriving (Eq,Show)

object :: Parser Object
object = char 'o' *> skipSpace *> fmap Object identifier <* eol

----------------------------------------------------------------------------------------------------
-- Special parsers ---------------------------------------------------------------------------------

-- Read a slash ('/') and run the @thenP@ parser on success. Otherwise, call the @elseP@ parser.
slashThenElse :: Parser a -> Parser a -> Parser a
slashThenElse thenP elseP = do
  c <- peekChar
  case c of
    Just '/' -> AP.take 1 *> thenP
    _ -> elseP

eol :: Parser ()
eol = skipSpace *> (endOfLine <|> endOfInput)
