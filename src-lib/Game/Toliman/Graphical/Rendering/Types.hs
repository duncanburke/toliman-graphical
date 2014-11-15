{-# LANGUAGE TemplateHaskell #-}

module Game.Toliman.Graphical.Rendering.Types where

import Graphics.UI.SDL (Window, GLContext)

import Game.Toliman.Graphical.Internal.Types

data RendererConfig = RendererConfig {
  _rd_resolution :: (Int,Int),
  _rd_fullscreen :: Bool,
  _rd_borderless :: Bool } deriving (Show)

makeUnderscoreFields ''RendererConfig

rendererConfigDefault :: RendererConfig
rendererConfigDefault = RendererConfig {
  _rd_resolution = (640,480),
  _rd_fullscreen = False,
  _rd_borderless = False }

data RendererState = RendererState {
  _rd_cfg :: RendererConfig,
  _rd_window :: Maybe Window,
  _rd_glctx :: Maybe GLContext }

makeUnderscoreFields ''RendererState

rendererStateDefault :: RendererState
rendererStateDefault = RendererState {
  _rd_cfg = rendererConfigDefault,
  _rd_window = Nothing,
  _rd_glctx = Nothing }
