{-# LANGUAGE RecursiveDo #-}

module Game.Toliman.Graphical.UI.Events (
  processEvents,
  translateSDLEvent,
  widgetEvents) where

import Data.Functor ((<$>))
import Data.Time.Clock (DiffTime)

import qualified Graphics.UI.SDL as SDL

import Game.Toliman.Internal.Sodium as Sodium
import Game.Toliman.Internal.Lens
import Game.Toliman.Graphical.UI.Types

processEvents :: [InputEvent] -> MonadUI ()
processEvents ev = do
  e <- access ev_in
  sync $ mapM_ (e ^. push) ev

translateSDLEvent :: SDL.Event -> InputEvent
translateSDLEvent = SDLEvent

getWidget :: WidgetID -> Maybe (a, WidgetEvent -> a -> a)
getWidget = undefined

getWidgetBehaviour ::  (WidgetID, WidgetEvent) -> Maybe (a -> a)
getWidgetBehaviour (w_id, w_ev) = do
  (_, w_f) <- getWidget w_id
  return $ w_f w_ev

widgetEvents :: Sodium.Behaviour DiffTime -> Sodium.Behaviour UIState' -> Sodium.Event InputEvent -> Reactive ()
widgetEvents bv_t bv_s ei = do
  (t :: DiffTime) <- sample bv_t
  (s :: UIState') <- sample bv_s
  rec
    st <- hold inputStateDefault est
    let evst = snapshot inputFSM ei st
        (ev :: Sodium.Event (WidgetID, WidgetEvent)) = split $ fst <$> evst
        est = snd <$> evst
  return ()
  let ev' = filterJust $ getWidgetBehaviour <$> ev
  return ()

inputFSM :: InputEvent -> InputState -> ([(WidgetID, WidgetEvent)], InputState)
inputFSM = undefined

collectE :: (a -> s -> (b, s)) -> s -> Event a -> Reactive (Event b)
collectE f z ea = do
  rec
    s <- hold z es
    let ebs = snapshot f ea s
        eb = fst <$> ebs
        es = snd <$> ebs
  return eb
