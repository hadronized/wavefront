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

module Codec.Wavefront.Token where

import Codec.Wavefront.Face
import Codec.Wavefront.Line
import Codec.Wavefront.Location
import Codec.Wavefront.Normal
import Codec.Wavefront.Point
import Codec.Wavefront.TexCoord
import Control.Applicative ( Alternative(..) )
import Data.Attoparsec.Text as AP
import Data.Char ( isSpace )
import Data.Maybe ( catMaybes )
import Data.Text ( Text, unpack, strip )
import qualified Data.Text as T ( empty )
import Numeric.Natural ( Natural )
import Prelude hiding ( lines )

----------------------------------------------------------------------------------------------------
-- Token -------------------------------------------------------------------------------------------

data Token
  = TknV Location
  | TknVN Normal
  | TknVT TexCoord
  | TknP [Point]
  | TknL [Line]
  | TknF Face
  | TknG [Text]
  | TknO Text
  | TknMtlLib [Text]
  | TknUseMtl Text
  | TknS Natural
    deriving (Eq,Show)

-- |A stream of 'Token'.
type TokenStream = [Token]

tokenize :: Text -> Either String TokenStream
tokenize = fmap cleanupTokens . analyseResult False . parse (untilEnd tokenizer)
  where
    tokenizer = choice
      [
        fmap (Just . TknV) location
      , fmap (Just . TknVN) normal
      , fmap (Just . TknVT) texCoord
      , fmap (Just . TknP) points
      , fmap (Just . TknL) lines
      , fmap (Just . TknF) face
      , fmap (Just . TknG) groups
      , fmap (Just . TknO) object
      , fmap (Just . TknMtlLib) mtllib
      , fmap (Just . TknUseMtl) usemtl
      , fmap (Just . TknS) smoothingGroup
      , Nothing <$ comment
      ]

analyseResult :: Bool -> Result [Maybe Token] -> Either String [Maybe Token]
analyseResult partial r = case r of
  Done _ tkns -> Right tkns
  Fail i _ e -> Left $ "`" ++ Prelude.take 10 (unpack i) ++ "` [...]: " ++ e
  Partial p -> if partial then Left "not completely tokenized" else analyseResult True (p T.empty)

cleanupTokens :: [Maybe Token] -> TokenStream
cleanupTokens = catMaybes

----------------------------------------------------------------------------------------------------
-- Location ----------------------------------------------------------------------------------------

location :: Parser Location
location = skipSpace *> string "v " *> skipHSpace *> parseXYZW <* eol
  where
    parseXYZW = do
      xyz <- float `sepBy1` skipHSpace
      case xyz of
        [x,y,z] -> pure (Location x y z 1)
        [x,y,z,w] -> pure (Location x y z w)
        _ -> fail "wrong number of x, y and z arguments for location"

----------------------------------------------------------------------------------------------------
-- Normal ------------------------------------------------------------------------------------------

normal :: Parser Normal
normal = skipSpace *> string "vn " *> skipHSpace *> parseIJK <* eol
  where
    parseIJK = do
      ijk <- float `sepBy1` skipHSpace
      case ijk of
        [i,j,k] -> pure (Normal i j k)
        _ -> fail "wrong number of i, j and k arguments for normal"

----------------------------------------------------------------------------------------------------
-- Texture coordinates -----------------------------------------------------------------------------

texCoord :: Parser TexCoord
texCoord = skipSpace *> string "vt " *> skipHSpace *> parseUVW <* eol
  where
    parseUVW = do
      uvw <- float `sepBy1` skipHSpace
      case uvw of
        [u,v] -> pure (TexCoord u v 0)
        [u,v,w] -> pure (TexCoord u v w)
        _ -> fail "wrong number of u, v and w arguments for texture coordinates"

----------------------------------------------------------------------------------------------------
-- Points ------------------------------------------------------------------------------------------

points :: Parser [Point]
points = skipSpace *> string "p " *> skipHSpace *> fmap Point decimal `sepBy1` skipHSpace <* eol

----------------------------------------------------------------------------------------------------
-- Lines -------------------------------------------------------------------------------------------
lines :: Parser [Line]
lines = do
    skipSpace
    _ <- string "l "
    skipHSpace
    pointIndices <- parsePointIndices
    pts <- case pointIndices of
      _:_:_ -> pure $ zipWith Line pointIndices (tail pointIndices)
      _ -> fail "line doesn't have at least two points"
    eol
    pure pts
  where
    parsePointIndices = fmap (\(i,j) -> LineIndex i j) parseLinePair `sepBy1` skipHSpace
    parseLinePair = do
      v <- decimal
      slashThenElse (fmap (\vt -> (v, Just vt)) decimal) (pure (v,Nothing))

----------------------------------------------------------------------------------------------------
-- Faces -------------------------------------------------------------------------------------------
face :: Parser Face
face = do
    skipSpace
    _ <- string "f "
    skipHSpace
    faceIndices <- parseFaceIndices
    f <- case faceIndices of
      a:b:c:s -> pure (Face a b c s)
      _ -> fail "face doesn't have at least three points"
    eol
    pure f
  where
    parseFaceIndices = fmap (\(i,k,j) -> FaceIndex i k j) parseFaceTriple `sepBy1` skipHSpace
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

groups :: Parser [Text]
groups = skipSpace *> string "g " *> skipHSpace *> name `sepBy` skipHSpace <* eol

----------------------------------------------------------------------------------------------------
-- Objects -----------------------------------------------------------------------------------------

object :: Parser Text
object = skipSpace *> string "o " *> skipHSpace *> spacedName <* eol

----------------------------------------------------------------------------------------------------
-- Material libraries ------------------------------------------------------------------------------

mtllib :: Parser [Text]
mtllib = skipSpace *> string "mtllib " *> skipHSpace *> name `sepBy1` skipHSpace <* eol

----------------------------------------------------------------------------------------------------
-- Using materials ---------------------------------------------------------------------------------

usemtl :: Parser Text
usemtl = skipSpace *> string "usemtl " *> skipHSpace *> spacedName <* eol

----------------------------------------------------------------------------------------------------
-- Smoothing groups --------------------------------------------------------------------------------
smoothingGroup :: Parser Natural
smoothingGroup = skipSpace *> string "s " *> skipHSpace *> offOrIndex <* skipHSpace <* eol
  where
    offOrIndex = string "off" *> pure 0 <|> decimal

----------------------------------------------------------------------------------------------------
-- Comments ----------------------------------------------------------------------------------------
comment :: Parser ()
comment = skipSpace *> string "#" *> (() <$ manyTill anyChar eol)

----------------------------------------------------------------------------------------------------
-- Special parsers ---------------------------------------------------------------------------------

-- Read a slash ('/') and run the @thenP@ parser on success. Otherwise, call the @elseP@ parser.
slashThenElse :: Parser a -> Parser a -> Parser a
slashThenElse thenP elseP = do
  c <- peekChar
  case c of
    Just '/' -> AP.take 1 *> thenP
    _ -> elseP

-- End of line.
eol :: Parser ()
eol = skipMany (satisfy isHorizontalSpace) *> (endOfLine <|> endOfInput)

-- Parse a name (any character but space).
name :: Parser Text
name = takeWhile1 $ not . isSpace

spacedName :: Parser Text
spacedName = strip <$> AP.takeWhile (flip notElem ("\n\r" :: String))

skipHSpace :: Parser ()
skipHSpace = () <$ AP.takeWhile isHorizontalSpace

float :: Parser Float
float = fmap realToFrac double

-- Loop a parser and collect its values until we hit the end of the stream. Fails on the first
-- failure.
untilEnd :: Parser a -> Parser [a]
untilEnd p = go
  where
    go = do
      a <- p
      end <- atEnd
      if end then pure [a] else fmap (a:) go
