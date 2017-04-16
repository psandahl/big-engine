{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
-- |
-- Module: BigE.MousePicker
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.MousePicker
    ( MousePicker (..)
    , PickObject (..)
    , Pickable (..)
    , init
    , enable
    , disable
    , delete
    , render
    , getObjectId
    ) where

import           BigE.Runtime.Render    (Render)
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString.Char8  (ByteString)
import           Graphics.GL            (GLfloat)
import           Linear                 (M44)
import           Prelude                hiding (init)

data MousePicker = MousePicker

class PickObject a where
    objectId :: a -> Int
    modelMatrix :: a -> M44 GLfloat
    renderForPicking :: a -> Render app ()

data Pickable = forall a. PickObject a => Pickable a

init :: MonadIO m => Int -> Int -> m (Either String MousePicker)
init = undefined

enable :: MonadIO m => MousePicker -> m ()
enable = undefined

disable :: MonadIO m => m ()
disable = undefined

delete :: MonadIO m => MousePicker -> m ()
delete = undefined

render :: M44 GLfloat -> [Pickable] -> Render app ()
render = undefined

getObjectId :: MonadIO m => (Int, Int) -> MousePicker -> m Int
getObjectId = undefined

vertexShader :: ByteString
vertexShader =
    "#version 330 core\n\
    \\n\
    \layout (location = 0) in vec3 position;\n\
    \uniform mat4 mvp;\n\
    \\n\
    \void main()\n\
    \{\n\
    \  gl_Position = mvp * vec4(position, 1.0);\n\
    \}"

fragmentShader :: ByteString
fragmentShader =
    "#version 330 core\n\
    \\n\
    \uniform int objectId;\n\
    \out vec4 color;\n\
    \\n\
    \void main()\n\
    \{\n\
    \  int r = (objectId >> 16) & 0xFF;\n\
    \  int g = (objectId >> 8) & 0xFF;\n\
    \  int b = objectId & 0xFF;\n\
    \  color = vec4(float(r), float(g), float(b), 1.0);\n\
    \}"
