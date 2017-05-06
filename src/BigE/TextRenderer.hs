{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: BigE.TextRenderer
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.TextRenderer
    ( TextRenderer (..)
    , init
    , render
    , delete
    ) where

import qualified BigE.Program           as Program
import           BigE.Runtime           (Render)
import           BigE.TextRenderer.Text (Text (..))
import           BigE.Types             (Location, Program, ShaderType (..))
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString.Char8  (ByteString)
import           Prelude                hiding (init)

data TextRenderer = TextRenderer
    { program      :: !Program
    , transformLoc :: !Location
    , fontColorLoc :: !Location
    , fontAtlasLoc :: !Location
    } deriving Show

-- | Initialize the 'TextRenderer'.
init :: MonadIO m => m (Either String TextRenderer)
init = do
    eProgram <-
        Program.fromByteString
            [ (VertexShader, "builtin/TextRenderer/vertex.glsl", vertexShader)
            , (FragmentShader, "builtin/TextRenderer/fragment.glsl", fragmentShader)
            ]
    case eProgram of
        Right program' -> do
            transformLoc' <- Program.getUniformLocation program' "transform"
            fontColorLoc' <- Program.getUniformLocation program' "fontColor"
            fontAtlasLoc' <- Program.getUniformLocation program' "fontAtlas"

            return $ Right TextRenderer
                { program = program'
                , transformLoc = transformLoc'
                , fontColorLoc = fontColorLoc'
                , fontAtlasLoc = fontAtlasLoc'
                }

        Left err -> return $ Left err

-- | Delete the 'TextRenderer'.
delete :: MonadIO m => TextRenderer -> m ()
delete = Program.delete . program

render :: Text -> TextRenderer -> Render state ()
render = undefined

vertexShader :: ByteString
vertexShader =
    "#version 330 core\n\
    \\n\
    \layout (location = 0) in vec3 position;\n\
    \layout (location = 1) in vec2 texCoord;\n\
    \\n\
    \uniform mat4 transform;\n\
    \\n\
    \out vec2 vTexCoord;\n\
    \\n\
    \void main()\n\
    \{\n\
    \  vTexCoord = texCoord;\n\
    \  gl_Position = transform * vec4(position, 1.0);\n\
    \}\n\
    \"

fragmentShader :: ByteString
fragmentShader =
    "#version 330 core\n\
    \\n\
    \in vec2 vTexCoord;\n\
    \uniform vec3 fontColor;\n\
    \uniform sampler2D fontAtlas;\n\
    \\n\
    \out vec4 color;\n\
    \\n\
    \void main()\n\
    \{\n\
    \  float alpha = texture(fontAtlas, vTexCoord).a;\n\
    \  color = vec4(fontColor, alpha);\n\
    \}\n\
    \"
