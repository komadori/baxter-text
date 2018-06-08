module Graphics.Baxter.Text.Render where

import Data.Word
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Data.Vector.Storable (Vector)

import Graphics.Baxter.Text.Internal.Binding
import Graphics.Baxter.Text.Font

data GlyphImage = GlyphImage {
    imageWidth  :: !Int,
    imageHeight :: !Int,
    imageData   :: Vector Word8
} deriving Show

renderGlyph :: GlyphFont -> GlyphID -> GlyphMetrics -> IO GlyphImage
renderGlyph (GlyphFont font) glyph metrics =
    let w = ceiling $ glyphWidth metrics
        h = ceiling $ glyphHeight metrics
    in do
        v <- MV.new $ w * h
        MV.unsafeWith v $ \ptr ->
            btcbRenderGlyph font glyph ptr
                (fromIntegral w) (fromIntegral h) (fromIntegral w)
        d <- V.freeze v
        return $ GlyphImage { imageWidth = w, imageHeight = h, imageData = d }
