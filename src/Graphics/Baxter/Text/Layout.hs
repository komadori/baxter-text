module Graphics.Baxter.Text.Layout where

import Data.Text (Text)
import Data.Vector.Storable (Vector)

import Graphics.Baxter.Text.Internal.Binding
import Graphics.Baxter.Text.Font

data GlyphRun = GlyphRun {
    glyphs :: Vector GlyphInfo
} deriving Show

layoutText :: Text -> FontHandle -> IO [GlyphRun]
layoutText txt (FontHandle fd) =
    let procRun (Just run) = do
            glyphs <- getGlyphRunVector run
            nextRun <- btcbGetNextRun run
            rest <- procRun nextRun
            return $ GlyphRun glyphs : rest
        procRun Nothing = return []
    in btcbLayoutText txt fd >>= procRun
