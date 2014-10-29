module Game.Centauri.Graphical.OpenGL
       (module Graphics.Rendering.OpenGL.GL,
        glSetAttrs,
        glversion,
        gldoublebuffer,
        glaccelerated,
        gldebug,
        glframebuffer,
        glaccumbuffer,
        glmsaa,
        glwindowflags)
       where

import Graphics.Rendering.OpenGL.GL
import Game.Centauri.Graphical.SDL
import Foreign.C.Types
import Data.Bits
import GHC.Word

castBool :: Bool -> CInt
castBool True = 1
castBool False = 0

orFlags :: [GHC.Word.Word32] -> GHC.Word.Word32
orFlags = foldr (.|.) 0

filterPred :: [(Bool,a)] -> [a]
filterPred l = [a | (p,a) <- l, p]

glversion :: [(String,GLattr,CInt)]
glversion = [
  ("GL_MAJOR",glAttrContextMajorVersion,3),
  ("GL_MINOR",glAttrContextMinorVersion,2)]

gldoublebuffer b = [("GL_DOUBLEBUFFER",glAttrDoubleBuffer, castBool b)]

glaccelerated b = [("GL_ACCELERATED_VISUAL", glAttrAcceleratedVisual, castBool b)]

gldebug b = [("GL_DEBUG", glAttrContextFlags, if b then glContextFlagDebug else 0)]

glframebuffer r g b a d s = [
  ("GL_RED_SIZE",glAttrRedSize,r),
  ("GL_GREEN_SIZE",glAttrGreenSize,g),
  ("GL_BLUE_SIZE",glAttrBlueSize,b),
  ("GL_ALPHA_SIZE",glAttrAlphaSize,a),
  ("GL_DEPTH_SIZE",glAttrDepthSize,d),
  ("GL_STENCIL_SIZE",glAttrStencilSize,s)]

glaccumbuffer r g b a = [
  ("GL_ACCUM_RED_SIZE",glAttrAccumRedSize,r),
  ("GL_ACCUM_GREEN_SIZE",glAttrAccumGreenSize,g),
  ("GL_ACCUM_BLUE_SIZE",glAttrAccumBlueSize,b),
  ("GL_ACCUM_ALPHA_SIZE",glAttrAccumAlphaSize,a)]

glmsaa s = [
  ("GL_MULTISAMPLEBUFFERS",glAttrMultiSampleBuffers, castBool (s /= 0)),
  ("GL_MULTISAMPLESAMPLES",glAttrMultiSampleSamples, s)]

glwindowflags fullscreen borderless input_grabbed maximized highdpi =
  orFlags $ filterPred $ [
    (fullscreen, windowFlagFullscreen),
    (True, windowFlagOpenGL),
    (borderless, windowFlagBorderless),
    (input_grabbed, windowFlagInputGrabbed),
    (maximized, windowFlagMaximized),
    (highdpi, windowFlagAllowHighDPI)]

glSetAttrs attrs = foldr (>>) (return ()) [glSetAttribute attr val >>= checkRet desc | (desc, attr, val) <- attrs]
