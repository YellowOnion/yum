{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Control.Monad
import Data.Maybe
import Data.Functor ( (<&>) )

import Control.Concurrent ( threadDelay, runInBoundThread)
import Control.Concurrent.MVar qualified as MVar
import Data.IORef

import Data.GI.Base

import GI.Gdk             qualified as Gdk
import GI.Gtk             qualified as Gtk
import GI.Gio             qualified as GIO
import GI.Gtk4LayerShell  qualified as GLS

import Data.Text    qualified as T
import Data.Text.IO qualified as T


import Yummy qualified

import Clock
import Cpu

import Control.Monad.Loops (firstM)
import Formatting ((%), sformat, int)
import Data.GI.Base.Utils (whenJust)

joinMaybe :: Monad m
          => (b -> m (Maybe a))
          -> Maybe b
          -> m (Maybe a)
joinMaybe f = fmap join . mapM f

-- | encapsulate pattern:

joinMaybe' :: Monad m => (b -> m (Maybe a)) -> Maybe b -> m (Maybe a)
joinMaybe' f (Just a) = f a
joinMaybe' _ Nothing = return Nothing
joinMaybeFail :: Monad m => (m z) -> (b -> m (Maybe a)) -> Maybe b -> m (Maybe a)
joinMaybeFail _ f (Just a) = f a
joinMaybeFail g f Nothing = g >> return Nothing


getMonitors :: IO (Maybe [Gdk.Monitor])
getMonitors = do
  display :: Maybe Gdk.Display <- Gdk.displayGetDefault
  monitorsList <- maybe (error "can't find default display") Gdk.displayGetMonitors display
  n <- GIO.listModelGetNItems monitorsList
  forM [0..n - 1] $ \i -> do
      o <- GIO.listModelGetItem monitorsList (fromIntegral i)
      joinMaybeFail (error $ "failed list lookup index:" ++ show i) (castTo Gdk.Monitor) o
  <&> Just . catMaybes

matchConn :: Gdk.Monitor -> IO Bool
matchConn m = do
  desc <- Gdk.monitorGetConnector m
  return $ (Just "DP-1" ==) desc

setMonitor :: Gtk.ApplicationWindow -> Maybe [Gdk.Monitor] -> IO ()
setMonitor win monitors = do
  case monitors of
    Just ms@(m:_) -> do
      match <- firstM (matchConn) ms
      let monitor = maybe m id match
      desc <- Gdk.monitorGetDescription monitor
      fmaybe (error "no monitor description") desc $ \d -> T.putStrLn $ "Default monitor: " <> d
      Yummy.delay1 >> GLS.setMonitor win monitor
      return ()

      where
        fmaybe = flip . maybe
    _ -> error "No monitors!!??!?!?"

data Defer a = NeedWork a | Done

gtkMain  :: Gtk.Application -> IO ()
gtkMain app = do
  display <- fromMaybe (error "can't find default display") <$> Gdk.displayGetDefault
  monitorsList <- Gdk.displayGetMonitors display

  win   <- new Gtk.ApplicationWindow [ #application := app
                                     , #title := "Yummy"
                                     ]
  GLS.initForWindow win

  monitors <- getMonitors
  setMonitor win monitors

  _ <- on monitorsList #itemsChanged $ \i r a -> do
    T.putStrLn $ sformat ("pos: " % int % ", rem: " % int % ", add: " % int) i r a
    if a == 0
      then getMonitors >>= setMonitor win
      else do
      o <- GIO.listModelGetItem ?self i
      monitor <- joinMaybeFail (putStrLn "failed to get monitor change") (castTo Gdk.Monitor) o
      case monitor of
        Just m -> do
          doneVar <- newIORef False
          varA <- newIORef False
          varB <- newIORef False
          let syncVar v = readIORef doneVar >>=
                \isDone -> unless isDone $ do
                  writeIORef v True
                  states <- mapM readIORef [varA, varB]
                  let ready = and states
                  putStrLn $ "sync is " ++ if ready then "ready" else "not ready"
                  when ready $ getMonitors >>= setMonitor win

              syncCB v self spec = do
                p <- get self spec
                case p of
                  Just _ -> syncVar v
                  Nothing -> return ()

          _ <- on m (PropertyNotify #connector) $ \_ -> syncCB varA ?self #connector

          _ <- on m (PropertyNotify #description) $ \_ -> syncCB varB ?self #description

          return ()

        Nothing -> putStrLn "failed to cast to Gdk.Monitor"




  css <- new Gtk.CssProvider []

  let cssText = T.unlines [ "* {"
                          , "  color: pink; background-color: black;"
                          , "  font-family: Kode Mono; font-size: 16px;"
                          , "}"
                          , "box {"
                          , "  margin: 4px 4px;"
                          , "}"
                          , "image { margin: 0; }"
                          ]

  #loadFromData css cssText (fromIntegral $ T.length cssText)

  Gtk.styleContextAddProviderForDisplay display css (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

  box   <- new Gtk.Box               [ #orientation  := Gtk.OrientationHorizontal
                                     ]
  img   <- Gtk.pictureNewForFilename (Just "./static/mem.svg")
  set img [ #canShrink := True
          , #keepAspectRatio := True
          , #contentFit := Gtk.ContentFitContain
          ]

  --  _ <- on win #destroy Gtk.mainQuit
  #setChild win (Just box)
  #append box img

  (cpu :: Cpu) <- Yummy.init =<< MVar.newEmptyMVar
  (clock :: Clock) <- Yummy.init=<< MVar.newEmptyMVar

  phantom <- new Gtk.Label [#hexpand := True, #halign := Gtk.AlignFill ]

  #append box (Yummy.view cpu)
  #append box phantom
  #append box (Yummy.view clock)

  _ <- Yummy.forkHere $ forever $ Yummy.updateView cpu
  _ <- Yummy.forkHere $ forever $ Yummy.updateView clock

  putStrLn $ "Hello World"

  GLS.autoExclusiveZoneEnable win

  GLS.setMargin win GLS.EdgeTop 6
  GLS.setMargin win GLS.EdgeLeft 6
  GLS.setMargin win GLS.EdgeRight 6
  GLS.setAnchor win GLS.EdgeRight True
  GLS.setAnchor win GLS.EdgeLeft True
  GLS.setAnchor win GLS.EdgeTop True
  GLS.setMargin win GLS.EdgeBottom 6

  #present win

main :: IO ()
main = runInBoundThread $ do
  app <- new Gtk.Application [ #applicationId := "org.yummy", On #activate (gtkMain ?self) ]
  void $ #run app Nothing
