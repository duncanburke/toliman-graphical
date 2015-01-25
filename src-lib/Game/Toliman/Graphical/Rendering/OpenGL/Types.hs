{-# LANGUAGE TemplateHaskell #-}

module Game.Toliman.Graphical.Rendering.OpenGL.Types where

import Foreign.C.Types (CInt)

import Graphics.Rendering.OpenGL.Raw.Core31 (
  GLenum,
  gl_INVALID_ENUM,
  gl_INVALID_VALUE,
  gl_INVALID_OPERATION,
  gl_INVALID_FRAMEBUFFER_OPERATION,
  gl_OUT_OF_MEMORY)
import Graphics.UI.SDL

import Game.Toliman.Graphical.Internal.Types

data GLErrorFlag =
  GL_INVALID_ENUM |
  GL_INVALID_VALUE |
  GL_INVALID_OPERATION |
  GL_INVALID_FRAMEBUFFER_OPERATION |
  GL_OUT_OF_MEMORY |
  GL_OTHER_ERROR GLenum
  deriving (Show, Eq)

toGLErrorFlag :: GLenum -> GLErrorFlag
toGLErrorFlag fl = if
  | fl == gl_INVALID_ENUM -> GL_INVALID_ENUM
  | fl == gl_INVALID_VALUE -> GL_INVALID_VALUE
  | fl == gl_INVALID_OPERATION -> GL_INVALID_OPERATION
  | fl == gl_INVALID_FRAMEBUFFER_OPERATION -> GL_INVALID_FRAMEBUFFER_OPERATION
  | fl == gl_OUT_OF_MEMORY -> GL_OUT_OF_MEMORY
  | otherwise -> GL_OTHER_ERROR fl

data GLAttrs =
  GLAttrs {
    _gl_doublebuffer :: !Bool,
    _gl_debug :: !Bool,
    _gl_red_size :: !CInt,
    _gl_green_size :: !CInt,
    _gl_blue_size :: !CInt,
    _gl_alpha_size :: !CInt,
    _gl_depth_size :: !CInt,
    _gl_stencil_size :: !CInt,
    _gl_multisamples :: !CInt}
  deriving (Show)

makeUnderscoreFields ''GLAttrs

glAttrsDefault :: GLAttrs
glAttrsDefault = GLAttrs {
  _gl_doublebuffer = True,
  _gl_debug = False,
  _gl_red_size = 8,
  _gl_green_size = 8,
  _gl_blue_size = 8,
  _gl_alpha_size = 8,
  _gl_depth_size = 24,
  _gl_stencil_size = 8,
  _gl_multisamples = 0}

type GLAttrList = [(String,GLattr,CInt)]

toGLAttrList :: GLAttrs -> GLAttrList
toGLAttrList GLAttrs {..} =
  [("GL_DOUBLEBUFFER", SDL_GL_DOUBLEBUFFER, castBool _gl_doublebuffer),
   ("GL_DEBUG", SDL_GL_CONTEXT_FLAGS, if _gl_debug then SDL_GL_CONTEXT_DEBUG_FLAG else 0),
   ("GL_RED_SIZE", SDL_GL_RED_SIZE, _gl_red_size),
   ("GL_GREEN_SIZE", SDL_GL_GREEN_SIZE, _gl_green_size),
   ("GL_BLUE_SIZE", SDL_GL_BLUE_SIZE, _gl_blue_size),
   ("GL_ALPHA_SIZE", SDL_GL_ALPHA_SIZE, _gl_alpha_size),
   ("GL_DEPTH_SIZE", SDL_GL_DEPTH_SIZE, _gl_depth_size),
   ("GL_STENCIL_SIZE", SDL_GL_STENCIL_SIZE, _gl_stencil_size),
   ("GL_MULTISAMPLEBUFFERS", SDL_GL_MULTISAMPLEBUFFERS, castBool (_gl_multisamples /= 0)),
   ("GL_MULTISAMPLESAMPLES", SDL_GL_MULTISAMPLESAMPLES, _gl_multisamples)]
  where castBool :: Bool -> CInt
        castBool True = 1
        castBool False = 0

data VSyncMode =
  VSyncNone |
  VSyncSynchronous |
  VSyncAdaptive
  deriving (Show)

fromVSyncMode :: VSyncMode -> CInt
fromVSyncMode VSyncNone = 0
fromVSyncMode VSyncSynchronous = 1
fromVSyncMode VSyncAdaptive = -1

data GLConfig =
  GLConfig {
    _gl_vsync_mode :: !VSyncMode}
  deriving (Show)

makeUnderscoreFields ''GLConfig

glConfigDefault :: GLConfig
glConfigDefault = GLConfig {
  _gl_vsync_mode = VSyncNone}
