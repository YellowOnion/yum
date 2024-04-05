-- |
{-# LANGUAGE TypeFamilies #-}
module Yummy where

import GI.Gtk (IsWidget)

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar qualified as MVar

class Yummy a where
  type Widget a

  view :: (IsWidget (Widget a))
       => a
       -> Widget a

  init    :: MVar.MVar () -> IO a
  updateView  :: a -> IO ()

delay1 :: IO ()
delay1 = threadDelay $ 1000 * 1000
