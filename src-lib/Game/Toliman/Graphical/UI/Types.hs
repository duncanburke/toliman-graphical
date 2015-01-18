{-# LANGUAGE TemplateHaskell #-}

module Game.Toliman.Graphical.UI.Types where

import Graphics.UI.SDL as SDL
import FRP.Sodium as Sodium

import Game.Toliman.Graphical.Internal.Types

type ScreenCoord = (Int,Int)

data UIKeyBoardEvent =
  KeyDown Keysym |
  KeyUp Keysym |
  KeyPress Keysym deriving (Show)


data UIKeyboardState =
  UIKeyboardState {}
  deriving (Show)

makeUnderscoreFields ''UIKeyboardState

data UIMouseButton =
  MouseLeft |
  MouseRight deriving (Show)

data UIMouseEvent =
  MouseEnter |
  MouseLeave |
  MouseMove ScreenCoord |
  MouseDown ScreenCoord UIMouseButton |
  MouseUp ScreenCoord UIMouseButton |
  MouseClick UIMouseButton deriving (Show)

data UIInputEvent =
  UIKeyBoardEvent |
  UIMouseEvent deriving (Show)

data UIMouseState =
  UIMouseState {}
  deriving (Show)

makeUnderscoreFields ''UIMouseState

data UIState =
  UIState {
    ui_reactor :: Sodium.Reactive (),
    ui_mouse :: !UIMouseState,
    ui_keyboard :: !UIKeyboardState}

makeUnderscoreFields ''UIState
