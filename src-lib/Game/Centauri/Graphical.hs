{-# LANGUAGE ScopedTypeVariables #-}
{- |
   module      : Game.Centauri.Graphical
   copyright   : (c) Duncan Burke
   license     : MPL
   maintaner   : Duncan Burke <duncankburke@gmail.cmo>
-}

module Game.Centauri.Graphical
       ( graphicalMain ) where

import Game.Centauri.Configuration

import Control.Monad
import Control.Monad.Cont
import Control.Applicative
import Control.Exception
import Data.Bits
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL
import Graphics.UI.SDL as SDL
import Text.Printf

bracketC :: IO a -> (a -> IO b) -> ContT r IO a
bracketC start end = ContT (bracket start end)

bracketC_ :: IO a -> IO b -> ContT r IO a
bracketC_ start end = bracketC start (const end)

nullBracketC start = bracketC_ start (return ())

-- type signature self-explanatory
runBracketC :: ContT (IO a) IO (IO a) -> IO a
runBracketC c = join $ flip runContT return $ c

graphicalMain :: GameConfig -> IO ()
graphicalMain cfg = runBracketC $ do
  bracketC_ (initSDL) (finalSDL)
  drivers <- nullBracketC sdlGetDrivers
  nullBracketC $ print drivers
  bracketC_ (initGL cfg) (finalGL)
  window <- bracketC (initWindow cfg) (finalWindow)
  win_ctx <- bracketC (initGLContext cfg window) (finalGLContext)
  nullBracketC $ graphicalLoop cfg window
  return $ return ()

graphicalLoop :: GameConfig -> SDL.Window -> IO ()
graphicalLoop cfg window = forever $ do clearColor $= Color4 0 0 1 0
                                        clear  [ColorBuffer]
                                        print "swapping..."
                                        -- checkError "swap_window" $ SDL.glSwapWindow window
                                        SDL.glSwapWindow window
                                        SDL.delay 1000


glversion = [("GL_MAJOR",SDL.glAttrContextMajorVersion,3),
           ("GL_MINOR",SDL.glAttrContextMinorVersion,2)]

castBool :: Bool -> CInt
castBool True = 1
castBool False = 0

orFlags = foldr (.|.) 0

filterPred :: [(Bool,a)] -> [a]
filterPred l = [a | (p,a) <- l, p]

gldoublebuffer b = [("GL_DOUBLEBUFFER",SDL.glAttrDoubleBuffer, castBool b)]

glaccelerated b = [("GL_ACCELERATED_VISUAL", SDL.glAttrAcceleratedVisual, castBool b)]

gldebug b = [("GL_DEBUG", SDL.glAttrContextFlags, if b then SDL.glContextFlagDebug else 0)]

glframebuffer r g b a d s = [("GL_RED_SIZE",SDL.glAttrRedSize,r),
                             ("GL_GREEN_SIZE",SDL.glAttrGreenSize,g),
                             ("GL_BLUE_SIZE",SDL.glAttrBlueSize,b),
                             ("GL_ALPHA_SIZE",SDL.glAttrAlphaSize,a),
                             ("GL_DEPTH_SIZE",SDL.glAttrDepthSize,d),
                             ("GL_STENCIL_SIZE",SDL.glAttrStencilSize,s)]

glaccumbuffer r g b a = [("GL_ACCUM_RED_SIZE",SDL.glAttrAccumRedSize,r),
                         ("GL_ACCUM_GREEN_SIZE",SDL.glAttrAccumGreenSize,g),
                         ("GL_ACCUM_BLUE_SIZE",SDL.glAttrAccumBlueSize,b),
                         ("GL_ACCUM_ALPHA_SIZE",SDL.glAttrAccumAlphaSize,a)]

glmsaa samples = [("GL_MULTISAMPLEBUFFERS",SDL.glAttrMultiSampleBuffers, castBool (samples /= 0)),
                  ("GL_MULTISAMPLESAMPLES",SDL.glAttrMultiSampleSamples, samples)]



checkRet :: String -> CInt -> IO CInt
checkRet desc ret = case ret of
  -1 -> fail $ printf "SDL error: %s" desc
  _ -> return ret

checkError :: String -> IO a -> IO a
checkError desc call = do SDL.clearError
                          a <- call
                          err <- SDL.getError
                          c <- peek err
                          case c of
                            0 -> return a
                            _ -> do s <- peekCString err
                                    fail $ printf "SDL error: %s (%s)" desc s

glSetAttrs attrs = foldr (>>) (return ()) [glSetAttribute attr val >>= checkRet desc | (desc, attr, val) <- attrs]


sdlGetDrivers :: IO [String]
sdlGetDrivers = do n_drivers <- checkRet "num_video_drivers" =<< SDL.getNumVideoDrivers
                   sequence $ (\n -> (SDL.getVideoDriver n) >>= peekCString) <$> [0..(n_drivers-1)]

initSDL = (checkRet "sdl_init" <$> SDL.init SDL.initFlagVideo ) >> print "sdl init"

initGL cfg = do checkRet "video_subsystem_init" <$> SDL.initSubSystem SDL.initFlagVideo
                print "video subsystem init"
                runBracketC $ do
                  driver <- case (config_driver cfg) of
                    "" -> return nullPtr
                    _ -> bracketC (newCString $ config_driver cfg) (free)
                  return $ checkRet "video_init" <$> SDL.videoInit driver
                print "video init"
                glSetAttrs $ glversion
                glSetAttrs $ glframebuffer 8 8 8 8 24 8
                glSetAttrs $ glaccumbuffer 8 8 8 8
                glSetAttrs $ gldoublebuffer True
                glSetAttrs $ glaccelerated True
                glSetAttrs $ gldebug False
                glSetAttrs $ glmsaa 0

finalSDL = do print "final SDL"
              SDL.quit

finalGL = do print "final GL"
             SDL.videoQuit
             SDL.quitSubSystem SDL.initFlagVideo

glwindowflags fullscreen borderless input_grabbed maximized highdpi =
  orFlags $ filterPred $ [(fullscreen, SDL.windowFlagFullscreen),
                                 (True, SDL.windowFlagOpenGL),
                                 (borderless, SDL.windowFlagBorderless),
                                 (input_grabbed, SDL.windowFlagInputGrabbed),
                                 (maximized, SDL.windowFlagMaximized),
                                 (highdpi, SDL.windowFlagAllowHighDPI)]

initWindow cfg = runBracketC $ do
  let fullscreen = config_fullscreen cfg
      borderless = config_borderless cfg
      width = config_width cfg
      height = config_height cfg
      x = SDL.windowPosUndefined
      y = SDL.windowPosUndefined
      flags = glwindowflags fullscreen borderless False False False
  title <- bracketC (newCString "OpenCentauri") (free)
  return $ checkError "create_window" $ createWindow title x y width height flags

finalWindow window = do print "final window"
                        SDL.destroyWindow window


initGLContext cfg window = do let vsync = config_vsync cfg
                                  late_tear = config_vsync_late_tear cfg
                                  swap_interval = case () of
                                    _ | vsync && not late_tear -> 1
                                    _ | vsync && late_tear -> -1
                                    _ -> 0
                              ctx <- checkError "create_gl_context" $ SDL.glCreateContext window
                              checkRet "gl_set_swap_interval" <$> SDL.glSetSwapInterval swap_interval
                              return ctx

finalGLContext ctx = do print "final GL context"
                        SDL.glDeleteContext ctx
