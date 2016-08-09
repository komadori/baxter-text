module Graphics.Baxter.Text.Layout where

import Data.Text (Text)
import Data.Vector.Storable (Vector)

import Graphics.Baxter.Text.Internal.Binding
import Graphics.Baxter.Text.Font

newtype Font = Font BTCBFont

instance Show Font where
    showsPrec _ (Font _) = showString "Font"

data GlyphRun = GlyphRun {
    runFont   :: Font,
    runGlyphs :: Vector GlyphInfo
} deriving Show

layoutText :: Text -> FontHandle -> IO [GlyphRun]
layoutText txt (FontHandle fd) =
    let procRun (Just run) = do
            glyphs <- getGlyphRunVector run
            font <- btcbGetRunFont run
            nextRun <- btcbGetNextRun run
            rest <- procRun nextRun
            return $ GlyphRun (Font font) glyphs : rest
        procRun Nothing = return []
    in btcbLayoutText txt fd >>= procRun
