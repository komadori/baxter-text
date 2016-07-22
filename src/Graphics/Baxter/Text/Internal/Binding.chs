{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}

module Graphics.Baxter.Text.Internal.Binding where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr

#include <HsFFI.h>
#include "btcb.h"

{#pointer *BTCB_FontDesc as ^ foreign newtype #}

foreign import ccall "btcb.h &btcb_free_font_desc"
    btcbFreeFontDesc :: FunPtr (Ptr (BTCBFontDesc) -> IO ())

newFontDesc :: Ptr BTCBFontDesc -> IO BTCBFontDesc
newFontDesc = fmap BTCBFontDesc . newForeignPtr btcbFreeFontDesc 

{#fun btcb_create_font_desc as ^
    {id `Ptr (Ptr CChar)',
     id `Ptr CInt',
     `Double'} ->
    `BTCBFontDesc' newFontDesc* #}
