{-# LANGUAGE ForeignFunctionInterface,
    StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}

module Graphics.Baxter.Text.Internal.Binding where

import Control.Monad
import Control.Monad.ST
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Foreign as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Vector.Storable (MVector(MVector), Vector)
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable

#include <HsFFI.h>
#include "btcb.h"

{#pointer *BTCB_String as ^ newtype #}

deriving instance Storable BTCBString

intSize :: Int
intSize = sizeOf (0 :: CInt)

withString :: Text -> (BTCBString -> IO a) -> IO a
#ifdef BTCB_WIDE_CHARS
withString txt cont =
    let len = T.lengthWord16 txt
    in allocaBytes (intSize + 2 * len + 2) $ \ptr -> do
        poke (castPtr ptr) (fromIntegral len :: CInt)
        let strPtr = plusPtr (castPtr ptr) intSize
        T.unsafeCopyToPtr txt strPtr
        pokeElemOff strPtr len (0 :: Word16)
        cont $ BTCBString ptr
#else
withString txt cont =
    let bs = LBS.toStrict $ BS.toLazyByteString $
             (mconcat $ replicate intSize (BS.word8 0)) <>
                 T.encodeUtf8Builder txt <> BS.word8 0
        len = BS.length bs - intSize - 1
    in BS.unsafeUseAsCString bs $ \ptr -> do
        poke (castPtr ptr) (fromIntegral len :: CInt)
        cont $ BTCBString $ castPtr ptr
#endif

withStringList :: [Text] -> (Ptr BTCBString -> IO a) -> IO a
withStringList txts = withManyArray0 withString txts (BTCBString nullPtr)

withMany :: (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
withMany func as cont =
    let rec (a:as') bs = func a (\b -> rec as' (bs . (b:)))
        rec []      bs = cont $ bs []
    in rec as id

withManyArray0 :: Storable b =>
    (a -> (b -> IO c) -> IO c) -> [a] -> b -> (Ptr b -> IO c) -> IO c
withManyArray0 func as term cont =
    withMany func as $ \ptrs -> withArray0 term ptrs cont

{#pointer *BTCB_FontDesc as ^ foreign newtype #}

foreign import ccall "btcb.h &btcb_free_font_desc"
    btcbFreeFontDesc :: FunPtr (Ptr (BTCBFontDesc) -> IO ())

newFontDesc :: Ptr BTCBFontDesc -> IO BTCBFontDesc
newFontDesc = fmap BTCBFontDesc . newForeignPtr btcbFreeFontDesc 

{#fun btcb_create_font_desc as ^
    {withStringList* `[Text]',
     `Double'} ->
    `BTCBFontDesc' newFontDesc* #}

{#pointer *BTCB_GlyphRun as ^ foreign newtype #}

foreign import ccall "btcb.h &btcb_free_run"
    btcbFreeRun :: FunPtr (Ptr BTCBGlyphRun -> IO ())

newMaybeGlyphRun :: Ptr BTCBGlyphRun -> IO (Maybe BTCBGlyphRun)
newMaybeGlyphRun ptr =
    if ptr /= nullPtr
    then fmap (Just . BTCBGlyphRun) $ newForeignPtr btcbFreeRun ptr
    else return Nothing

newtype GlyphID = GlyphID CInt deriving (Eq, Ord, Show)

unGlyphID :: GlyphID -> CInt
unGlyphID (GlyphID i) = i

data GlyphInfo = GlyphInfo {
    glyphID :: !GlyphID,
    glyphX  :: !Double,
    glyphY  :: !Double
} deriving (Eq, Ord, Show)

instance Storable GlyphInfo where
    sizeOf _    = {#sizeof BTCB_Glyph #}
    alignment _ = {#alignof BTCB_Glyph #}
    peek ptr = GlyphInfo
        <$> liftM GlyphID    ({#get BTCB_Glyph->glyph #} ptr)
        <*> liftM realToFrac ({#get BTCB_Glyph->x #} ptr)
        <*> liftM realToFrac ({#get BTCB_Glyph->y #} ptr)
    poke ptr glyph = do
        {#set BTCB_Glyph.glyph #} ptr (unGlyphID $ glyphID glyph)
        {#set BTCB_Glyph.x #} ptr (realToFrac $ glyphX glyph)
        {#set BTCB_Glyph.y #} ptr (realToFrac $ glyphY glyph)

{#fun btcb_layout_text as ^
    {withString* `Text',
     withBTCBFontDesc* `BTCBFontDesc'} ->
    `Maybe BTCBGlyphRun' newMaybeGlyphRun* #}

{#fun btcb_get_run_length as ^
    {withBTCBGlyphRun* `BTCBGlyphRun'} ->
    `Int' #}

getGlyphRunVector :: BTCBGlyphRun -> IO (Vector GlyphInfo)
getGlyphRunVector run@(BTCBGlyphRun runPtr) = do
    len <- btcbGetRunLength run
    V.unsafeFreeze $ MVector len (castForeignPtr runPtr)

{#pointer *BTCB_Font as ^ foreign newtype #}

foreign import ccall "btcb.h &btcb_free_font"
    btcbFreeFont :: FunPtr (Ptr BTCBFont -> IO ())

newFont :: Ptr BTCBFont -> IO BTCBFont
newFont = fmap BTCBFont . newForeignPtr btcbFreeFont 

{#fun btcb_get_run_font as ^
    {withBTCBGlyphRun* `BTCBGlyphRun'} ->
    `BTCBFont' newFont* #}

{#fun btcb_get_next_run as ^
    {withBTCBGlyphRun* `BTCBGlyphRun'} ->
    `Maybe BTCBGlyphRun' newMaybeGlyphRun* #}
