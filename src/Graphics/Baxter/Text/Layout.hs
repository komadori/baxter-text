module Graphics.Baxter.Text.Layout where

import Data.Text (Text)

import Graphics.Baxter.Text.Internal.Binding
import Graphics.Baxter.Text.Font

layoutText :: Text -> FontHandle -> IO ()
layoutText txt (FontHandle fd) = btcbLayoutText txt fd
