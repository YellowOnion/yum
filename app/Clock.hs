{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- |

module Clock where

import Data.Text qualified as T

import Control.Monad            ( when )
import Control.Concurrent       ( forkIO      )
import Control.Concurrent.MVar  ( MVar, newMVar, takeMVar, isEmptyMVar, putMVar)

import Data.GI.Base
import GI.Gtk             qualified as Gtk

import Formatting
import Formatting.Time  qualified as FTime
import Data.Time  qualified as Time

import Yummy

data Clock where
  Clock :: Gtk.Label -> MVar () -> Clock

instance Yummy Clock where
  type Widget Clock = Gtk.Label

  view :: Gtk.IsWidget (Widget Clock) => Clock -> Widget Clock
  view (Clock lbl _) = lbl

  init :: MVar () -> IO Clock
  init mvar = do
    var <- newMVar ()
    let go = do
          b <- isEmptyMVar mvar
          when b $ delay1 >> putMVar var () >> go
    _ <- forkIO go

    label <- new Gtk.Label [ #widthRequest := 128, #label := "0" ]

    return $ Clock label var

  updateView (Clock lbl var) = do
    () <- takeMVar var
    time <- Time.getCurrentTime
    let time_s = sformat (FTime.hms) time
    set lbl [#label := time_s ]
