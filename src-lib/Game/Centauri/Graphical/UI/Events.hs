{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Game.Centauri.Graphical.UI.Events
       (dispatchEvents) where

import Game.Centauri.Graphical.SDL as SDL
import Game.Centauri.Graphical.UI.State as State

import Codec.Binary.UTF8.String as UTF8
import Control.Monad.State
import Control.Applicative
import Data.Foldable
import Data.Monoid

instance Monoid (State UIState ()) where
  mempty = return ()
  mappend = (>>)

dispatchEvents :: [Event] -> State UIState ()
dispatchEvents evl = fold $ dispatchEvent <$> evl

dispatchEvent :: SDL.Event ->  State UIState ()

dispatchEvent (QuitEvent {}) = uiQuitEvent


dispatchEvent (MouseButtonEvent {eventType,
                                    mouseButtonEventButton = b,
                                    mouseButtonEventClicks = clicks,
                                    mouseButtonEventX = x,
                                    mouseButtonEventY = y}) =
  let button = case button of
        _ | b == buttonLeft -> Just MouseLeft
          | b == buttonRight -> Just MouseRight
          | otherwise -> Nothing
      clicks_ = fromIntegral clicks
      [x_, y_] = fromIntegral <$> [x, y]
      isdown = case False of
        _ | eventType == eventTypeMouseButtonDown -> Just True
          | eventType == eventTypeMouseButtonUp -> Just False
          | otherwise -> Nothing
      sendEvent = do
        button_ <- button
        isdown_ <- isdown
        return $ uiMouseButtonEvent button_ isdown_ clicks_ x_ y_
  in
  case sendEvent of
    Just ev -> ev
    Nothing -> return ()

dispatchEvent (KeyboardEvent {eventType,
                                 keyboardEventRepeat = isrepeat,
                                 keyboardEventKeysym = keysym}) =
  let isdown = case False of
        _ | eventType == eventTypeKeyDown -> Just True
          | eventType == eventTypeKeyUp -> Just False
          | otherwise -> Nothing
      isrepeat_ = isrepeat /= 0
      sendEvent = do
        isdown_ <- isdown
        return $ uiKeyboardEvent keysym isdown_ isrepeat_
  in
  case sendEvent of
    Just ev -> ev
    Nothing -> return ()

dispatchEvent (TextInputEvent {textInputEventText = buf}) =
  uiTextInputEvent $ UTF8.decode $ fromIntegral <$> buf

dispatchEvent _ = return ()
