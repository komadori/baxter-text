module Graphics.Baxter.Text.Font where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe
import Graphics.Baxter.Text.Internal.Binding

data FontDesc = FontDesc {
    fontFamilies :: [Text],
    fontSize     :: Double
}

newtype FontHandle = FontHandle BTCBFontDesc

createFont :: FontDesc -> FontHandle
createFont fd = unsafePerformIO $ do
    hndl <- withMany T.withCStringLen (fontFamilies fd) $ \familyPtrLens ->
        withArray0 nullPtr (map fst familyPtrLens) $ \familyPtrsPtr ->
            withArray0 0 (map (fromIntegral . snd) familyPtrLens) $
            \familyLensPtr ->
                btcbCreateFontDesc familyPtrsPtr familyLensPtr (fontSize fd)
    return $ FontHandle hndl

withMany :: (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
withMany func as cont =
    let rec (a:as') bs = func a (\b -> rec as' (bs . (b:)))
        rec []      bs = cont $ bs []
    in rec as id
