-- |
-- Module: BigE.TextRenderer.Types
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.TextRenderer.Types
    ( Info (..)
    , Padding (..)
    , Spacing (..)
    , Common (..)
    , Page (..)
    , Character (..)
    , Kerning (..)
    ) where

-- | Info data for a bitmap font.
-- See <http://www.angelcode.com/products/bmfont/doc/file_format.html>
data Info = Info
    { face     :: !String
      -- ^ This is the name of the true type font.

    , size     :: !Int
      -- ^ The size of the true type font.

    , bold     :: !Bool
      -- ^ The font is bold.

    , italic   :: !Bool
      -- ^ The font is italic.

    , charset  :: !String
      -- ^ The name of the OEM charset used (when not unicode).

    , unicode  :: !Bool
      -- ^ Set to True if it is the unicode charset.

    , stretchH :: !Int
      -- ^ The font height stretch in percentage. 100% means no stretch.

    , smooth   :: !Bool
      -- ^ Set to True if smoothing was turned on.

    , aa       :: !Int
      -- ^ The supersampling level used. 1 means no supersampling was used.

    , padding  :: !Padding
      -- ^ The padding for each character (up, right, down, left).

    , spacing  :: !Spacing
      -- ^ The spacing for each character (horizontal, vertical).-}
    } deriving (Eq, Show)

-- | Padding to be used in 'Info'.
data Padding = Padding
    { up    :: !Int
    , right :: !Int
    , down  :: !Int
    , left  :: !Int
    } deriving (Eq, Show)

-- | Spacing to be used in 'Info'.
data Spacing = Spacing
    { horizontal :: !Int
    , vertical   :: !Int
    } deriving (Eq, Show)

-- | Common data for a bitmap font.
-- See <http://www.angelcode.com/products/bmfont/doc/file_format.html>
data Common = Common
    { lineHeight :: !Int
      -- ^ This is the distance in pixels between each line of text.

    , base       :: !Int
      -- ^ The number of pixels from the absolute top of the line
      -- to the base of the characters.

    , scaleW     :: !Int
      -- ^ The width of the texture, normally used to scale the
      -- x pos of the character image.

    , scaleH     :: !Int
      -- ^ The height of the texture, normally used to scale the
      -- y pos of the character image.

    , pages      :: !Int
      -- ^ The number of texture pages included in the font.

    , packed     :: !Bool
      -- ^ Set to True if the monochrome characters have been packed
      -- into each of the texture channels.
    } deriving (Eq, Show)

-- | Page data for a bitmap font.
-- See <http://www.angelcode.com/products/bmfont/doc/file_format.html>
data Page = Page
    { pageId :: !Int
      -- ^ The page id.

    , file   :: !String
      -- ^ The texture file name.
    } deriving (Eq, Show)

-- | Character data for a bitmap font.
-- See <http://www.angelcode.com/products/bmfont/doc/file_format.html>
data Character = Character
    { charId   :: !Int
      -- ^ The character id (ascii code).

    , x        :: !Int
      -- ^ The left position of the character image in the texture.

    , y        :: !Int
      -- The top position of the character image in the texture.

    , width    :: !Int
      -- ^ The width of the character image in the texture.

    , height   :: !Int
      -- ^ The height of the character image in the texture.

    , xOffset  :: !Int
      -- ^ How much the current position should be offset when copying
      -- the image from the texture to the screen.

    , yOffset  :: !Int
      -- ^ How much the current position should be offset when copying
      -- the image from the texture to the screen.

    , xAdvance :: !Int
      -- ^ How much the current position should be advanced after
      -- drawing the character.

    , pageNo   :: !Int
      -- ^ The texture page where the character image is found.

    , chnl     :: !Int
      -- ^ The texture channel where the character image is found
      -- (1 = blue, 2 = green, 4 = red, 8 = alpha, 15 = all channels).
    } deriving (Eq, Show)

-- | Kerning data for a bitmap font.
-- See <http://www.angelcode.com/products/bmfont/doc/file_format.html>
data Kerning = Kerning
    { first  :: !Int
      -- ^ The first character id.

    , second :: !Int
      -- ^ The second character id.

    , amount :: !Int
      -- ^ How much the x position should be adjusted when drawing the
      -- second character immediately following the first.
    }
    deriving (Eq, Show)
