{-# LANGUAGE Rank2Types #-}

module Gloss.FRP.Reactive.Banana (playReactive, InputEvent) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Control.Monad.IO.Class
type InputEvent = G.Event


playReactive :: Display -> Color -> Int
             -> (Event Float -> Event InputEvent -> MomentIO (Behavior Picture))
             -> IO ()
playReactive display colour frequency mPicture = do
  pictureref <- newIORef blank
  (tickHandler,  tick)  <- newAddHandler
  (eventHandler, event) <- newAddHandler
  compile (makeNetwork tickHandler eventHandler (writeIORef pictureref)) >>= actuate
  playIO display colour frequency ()
    (\      _ -> readIORef pictureref)
    (\ ev   _ -> () <$ event ev)
    (\ time _ -> () <$ tick time)
  where
    makeNetwork tickHandler eventHandler change = do
      eTick  <- fromAddHandler tickHandler
      eEvent <- fromAddHandler eventHandler
      bRawPicture <- mPicture eTick eEvent

      -- make sure the Behavior doesn't leak memory if mPicture ignores
      -- one or both kind of events
      bTick  <- stepper undefined eTick
      bEvent <- stepper undefined eEvent
      let bPicture = bRawPicture <* bTick <* bEvent

      changes bPicture >>= reactimate' . fmap (fmap change)
