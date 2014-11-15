
module Game.Toliman.Graphical.Rendering.OpenGL
       (module Graphics.Rendering.OpenGL.GL,
        glSetAttrs,
        gldoublebuffer,
        gldebug,
        glframebuffer,
        glmsaa,
        glwindowflags)
       where

import Foreign.C.Types (CInt)
import Data.Bits ((.|.))
import GHC.Word (Word32)

import Graphics.Rendering.OpenGL.GL
import Graphics.UI.SDL

import Game.Toliman.Graphical.SDL

castBool :: Bool -> CInt
castBool True = 1
castBool False = 0

orFlags :: [Word32] -> Word32
orFlags = foldr (.|.) 0

filterPred :: [(Bool,a)] -> [a]
filterPred l = [a | (p,a) <- l, p]

type GLAttrList = [(String,GLattr,CInt)]

gldoublebuffer :: Bool -> GLAttrList
gldoublebuffer b = [("GL_DOUBLEBUFFER",SDL_GL_DOUBLEBUFFER, castBool b)]
gldebug :: Bool -> GLAttrList
gldebug b = [("GL_DEBUG", SDL_GL_CONTEXT_FLAGS, if b then SDL_GL_CONTEXT_DEBUG_FLAG else 0)]

glframebuffer :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> GLAttrList
glframebuffer r g b a d s = [
  ("GL_RED_SIZE",SDL_GL_RED_SIZE,r),
  ("GL_GREEN_SIZE",SDL_GL_GREEN_SIZE,g),
  ("GL_BLUE_SIZE",SDL_GL_BLUE_SIZE,b),
  ("GL_ALPHA_SIZE",SDL_GL_ALPHA_SIZE,a),
  ("GL_DEPTH_SIZE",SDL_GL_DEPTH_SIZE,d),
  ("GL_STENCIL_SIZE",SDL_GL_STENCIL_SIZE,s)]

glmsaa :: CInt -> GLAttrList
glmsaa s = [
  ("GL_MULTISAMPLEBUFFERS",SDL_GL_MULTISAMPLEBUFFERS, castBool (s /= 0)),
  ("GL_MULTISAMPLESAMPLES",SDL_GL_MULTISAMPLESAMPLES, s)]

glwindowflags :: Bool -> Bool -> Bool -> Bool -> Bool -> Word32
glwindowflags fullscreen borderless input_grabbed maximized highdpi =
  orFlags $ filterPred [
    (fullscreen, SDL_WINDOW_FULLSCREEN),
    (True, SDL_WINDOW_OPENGL),
    (borderless, SDL_WINDOW_BORDERLESS),
    (input_grabbed, SDL_WINDOW_INPUT_GRABBED),
    (maximized, SDL_WINDOW_MAXIMIZED),
    (highdpi, SDL_WINDOW_ALLOW_HIGHDPI)]

glSetAttrs :: GLAttrList -> IO ()
glSetAttrs attrs = sequence_ [glSetAttribute attr val >>= checkRet desc | (desc, attr, val) <- attrs]
