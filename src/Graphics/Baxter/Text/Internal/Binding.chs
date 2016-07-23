{-# LANGUAGE ForeignFunctionInterface,
    StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}

module Graphics.Baxter.Text.Internal.Binding where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable

#include <HsFFI.h>
#include "btcb.h"

{#pointer *BTCB_String as ^ newtype #}

deriving instance Storable BTCBString

withString :: Text -> (BTCBString -> IO a) -> IO a
#ifdef BTCB_WIDE_CHARS
withString txt cont =
    let len = T.lengthWord16 txt
    let off = sizeOf (0 :: CInt)
    in allocaBytes (off + 2 * len) $ \ptr -> do
        poke (castPtr ptr) (fromIntegral len :: CInt)
        unsafeCopyToPtr txt (addPtr (castPtr ptr) off)
        cont $ BTCBString ptr
#else
withString txt cont = error "Unimplemented"
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
