module Graphics.Baxter.Text.Layout (
    GlyphID,
    GlyphInfo(
        glyphID,
        glyphX,
        glyphY),
    GlyphRun(
        runFont,
        runGlyphs),
    layoutText,
    getMetrics,
) where

import Data.Text (Text)
import Data.Vector.Storable (Vector)

import Graphics.Baxter.Text.Internal.Binding
import Graphics.Baxter.Text.Font

data GlyphRun = GlyphRun {
    runFont   :: GlyphFont,
    runGlyphs :: Vector GlyphInfo
} deriving Show

layoutText :: Text -> FontHandle -> IO [GlyphRun]
layoutText txt (FontHandle fd) =
    let procRun (Just run) = do
            glyphs <- getGlyphRunVector run
            font <- btcbGetRunFont run
            nextRun <- btcbGetNextRun run
            rest <- procRun nextRun
            return $ GlyphRun (GlyphFont font) glyphs : rest
        procRun Nothing = return []
    in btcbLayoutText txt fd >>= procRun

getMetrics :: GlyphFont -> GlyphID -> IO GlyphMetrics
getMetrics (GlyphFont font) glyph =
    getMetricsImpl font glyph
