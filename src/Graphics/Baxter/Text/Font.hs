module Graphics.Baxter.Text.Font where

import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe
import Graphics.Baxter.Text.Internal.Binding

data FontDesc = FontDesc {
    fontFamilies :: [Text],
    fontSize     :: Double
}

newtype FontHandle = FontHandle BTCBFontDesc

createFont :: FontDesc -> FontHandle
createFont fd = unsafePerformIO $ do
    hndl <- btcbCreateFontDesc (fontFamilies fd) (fontSize fd)
    return $ FontHandle hndl
