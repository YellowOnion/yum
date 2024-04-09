-- |
{-# LANGUAGE TypeFamilies #-}
module Yummy where

import GI.Gtk (IsWidget)

import Control.Concurrent (threadDelay, forkOn, threadCapability, myThreadId)
import Control.Concurrent.MVar qualified as MVar

import Control.Monad (void)
import Control.Monad.IO.Class ( MonadIO )

import GI.GLib qualified as GLib

class Yummy a where
  type Widget a

  view :: (IsWidget (Widget a))
       => a
       -> Widget a

  init    :: MVar.MVar () -> IO a
  updateView  :: a -> IO ()

delay1 :: IO ()
delay1 = threadDelay $ 1000 * 1000

defer :: MonadIO m => IO a -> m ()
defer f = void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (f >> return True)

forkHere f = do
  (i, _) <- threadCapability =<< myThreadId
  forkOn i f
