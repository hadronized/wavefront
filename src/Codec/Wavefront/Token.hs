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

import Codec.Wavefront.Face
import Codec.Wavefront.Float
import Codec.Wavefront.Line
import Codec.Wavefront.Location
import Codec.Wavefront.Identifier
import Codec.Wavefront.Normal
import Codec.Wavefront.Point
import Codec.Wavefront.TexCoord
import Control.Applicative ( Alternative(..), empty )
import Data.Attoparsec.Text as AP
import Data.Maybe ( catMaybes )
import Data.Text ( Text, unpack )
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
  | TknG [Text]
  | TknO Text 
    deriving (Eq,Show)

type TokenStream = [Token]

tokenize :: Text -> Either String TokenStream
tokenize = fmap cleanupTokens . analyseResult False . parse (many1 tokenizer)
  where
    tokenizer = foldl (<|>) empty
      [
        fmap (Just . TknV) location
      , fmap (Just . TknVN) normal
      , fmap (Just . TknVT) texCoord
      , fmap (Just . TknP) points
      , fmap (Just . TknL) lines
      , fmap (Just . TknF) faces
      , fmap (Just . TknG) groups
      , fmap (Just . TknO) object
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
location = skipSpace *> string "v " *> skipSpace *> parseXYZW <* eol
  where
    parseXYZW = do
      xyz <- float `sepBy1` skipSpace
      case xyz of
        [x,y,z] -> pure (Location x y z 1)
        [x,y,z,w] -> pure (Location x y z w)
        _ -> fail "wrong number of x, y and z arguments for location"

----------------------------------------------------------------------------------------------------
-- Normal ------------------------------------------------------------------------------------------

normal :: Parser Normal
normal = skipSpace *> string "vn " *> skipSpace *> parseIJK <* eol
  where
    parseIJK = do
      ijk <- float `sepBy1` skipSpace
      case ijk of
        [i,j,k] -> pure (Normal i j k)
        _ -> fail "wrong number of i, j and k arguments for normal"

----------------------------------------------------------------------------------------------------
-- Texture coordinates -----------------------------------------------------------------------------

texCoord :: Parser TexCoord
texCoord = skipSpace *> string "vt " *> skipSpace *> parseUVW <* eol
  where
    parseUVW = do
      uvw <- float `sepBy1` skipSpace
      case uvw of
        [u,v] -> pure (TexCoord u v 0)
        [u,v,w] -> pure (TexCoord u v w)
        _ -> fail "wrong number of u, v and w arguments for texture coordinates"

----------------------------------------------------------------------------------------------------
-- Points ------------------------------------------------------------------------------------------

points :: Parser [Point]
points = skipSpace *> string "p " *> skipSpace *> fmap Point decimal `sepBy1` skipSpace <* eol

----------------------------------------------------------------------------------------------------
-- Lines -------------------------------------------------------------------------------------------

-- TODO: ensure we have at least 2 pairs, otherwise it should fail
lines :: Parser [Line]
lines = skipSpace *> string "l " *> skipSpace *> fmap Line parseLinePair `sepBy1` skipSpace <* eol
  where
    parseLinePair = do
      v <- decimal
      slashThenElse (fmap (\vt -> (v, Just vt)) decimal) (pure (v,Nothing))

----------------------------------------------------------------------------------------------------
-- Faces -------------------------------------------------------------------------------------------

-- TODO: ensure we have at least 3 triples, otherwise it should fail
faces :: Parser [Face]
faces = skipSpace *> string "f " *> skipSpace *> fmap Face parseFaceTriple `sepBy1` skipSpace <* eol
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

groups :: Parser [Text]
groups = skipSpace *> string "g " *> skipSpace *> identifier `sepBy1` skipSpace <* eol

----------------------------------------------------------------------------------------------------
-- Objects -----------------------------------------------------------------------------------------

object :: Parser Text
object = skipSpace *> string "o " *> skipSpace *> identifier <* eol

----------------------------------------------------------------------------------------------------
-- Comments ----------------------------------------------------------------------------------------
comment :: Parser ()
comment = skipSpace *> string "# " *> (() <$ manyTill anyChar eol)

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
eol = skipMany (satisfy isHorizontalSpace) *> (endOfLine <|> endOfInput)
