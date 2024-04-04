{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad

import Control.Concurrent (forkOS)
import Control.Concurrent.MVar qualified as MVar

import Data.GI.Base

import GI.Gdk             qualified as Gdk
import GI.Gtk             qualified as Gtk
import GI.GLib            qualified as GLib
import GI.Gtk4LayerShell  qualified as GLS

import Data.Text qualified as T

import Yummy qualified as Yummy

import Clock
import Cpu

main :: IO ()
main = do
  Gtk.init
  css <- new Gtk.CssProvider []

  let cssText = T.unlines [ "* { color: pink; background-color: black; } "
                                 , "* { font-family: Kode Mono; }"
                                 , "box { border: 0; margin: 8px; }"
                                 , "levelbar, trough { border-style: none; }"
                                 ]

  #loadFromData css cssText (fromIntegral $ T.length cssText)

  Just display <- Gdk.displayGetDefault

  Gtk.styleContextAddProviderForDisplay display css (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)


  win   <- new Gtk.Window   [ #title        := "Test"                    ]
  box   <- new Gtk.Box      [ #orientation  := Gtk.OrientationHorizontal ]
  img   <- Gtk.imageNewFromFile "./static/mem.svg"

  --  _ <- on win #destroy Gtk.mainQuit
  #setChild win (Just box)
  #append box img

  GLS.initForWindow win
  GLS.autoExclusiveZoneEnable win

  GLS.setMargin win GLS.EdgeTop 3
  GLS.setMargin win GLS.EdgeBottom 3
  GLS.setMargin win GLS.EdgeLeft 3
  GLS.setMargin win GLS.EdgeRight 3
  GLS.setAnchor win GLS.EdgeBottom True
  GLS.setAnchor win GLS.EdgeRight True
  GLS.setAnchor win GLS.EdgeLeft True

  (clock :: Clock) <- Yummy.init=<< MVar.newEmptyMVar
  (cpu :: Cpu) <- Yummy.init =<< MVar.newEmptyMVar

  #append box (Yummy.view cpu)
  #append box (Yummy.view clock)
  #present win

  _ <- forkOS $ forever $ Yummy.updateView cpu
  _ <- forkOS $ forever $ Yummy.updateView clock

  putStrLn $ "Hello World"
  mainCtx <- GLib.mainContextDefault
  let loop = do
        _ <- GLib.mainContextIteration mainCtx True
        when True loop
  loop
