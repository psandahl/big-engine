-- |
-- Module: BigE.TextRenderer.Font
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.TextRenderer.Font
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

    {-, size     :: !Int
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

data Common = Common
    deriving (Eq, Show)

data Page = Page
    deriving (Eq, Show)

data Character = Character
    deriving (Eq, Show)

data Kerning = Kerning
    deriving (Eq, Show)
