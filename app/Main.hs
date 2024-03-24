{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative

import Control.Concurrent (threadDelay, forkIO)
import System.IO

import Data.GI.Base

import GI.Gdk             qualified as Gdk
import GI.Gtk             qualified as Gtk
import GI.GtkLayerShell   qualified as GLS

import Data.Text qualified as T
import Data.ByteString.Char8 qualified as BS
import GI.Gtk (CssProvider(CssProvider))
import GHC.Float (fromRat'')
import GI.Gdk (getDisplayManagerDefaultDisplay)

data Event = Exit

data State = State


liftCpu f (Cpu a b c d) = Cpu (f a) (f b) (f c) (f d)
lift2Cpu f (Cpu a b c d) (Cpu a' b' c' d' ) = Cpu (f a a') (f b b') (f c c') (f d d')

data Cpu = Cpu { cpuUser :: Int
               , cpuNice :: Int
               , cpuSys  :: Int
               , cpuIdle :: Int
               } deriving (Eq, Ord, Show)

instance Num Cpu where
  (+) = lift2Cpu (+)
  (-) = lift2Cpu (-)
  (*) = lift2Cpu (*)
  abs = liftCpu abs
  signum = liftCpu signum
  fromInteger a = let a' = fromInteger a in Cpu a' a' a' a'


readInt :: String -> Int
readInt = read

readCpu :: IO Cpu
readCpu = do
  withFile "/proc/stat" ReadMode $ \h -> do
    l <- hGetLine h
    let (_:a:b:c:d:_) = map readInt $ words l
    return $ Cpu a b c d

percentCpu Cpu{..} = (total - fromIntegral cpuIdle) / total
  where
    total = fromIntegral $ cpuUser + cpuNice + cpuSys + cpuIdle


delay1 = threadDelay $ 1000 * 1000

clock :: Gtk.Label -> Int -> IO ()
clock lbl = go
  where go n = do
          delay1
          set lbl [ #label := T.pack $ show n ]
          go $ n + 1


cpu :: Gtk.LevelBar -> Maybe Cpu -> IO ()
cpu bar = go
  where
    go mc = do
      delay1
      c2 <- readCpu
      case mc of
        Nothing -> return ()
        (Just c1) -> do
          set bar [ #value := percentCpu (c2 - c1) ]
      go (Just c2)


main :: IO ()
main = do
  _ <- Gtk.init Nothing

  css <- new Gtk.CssProvider []

  #loadFromData css $ BS.unlines [ "* { color: pink; background-color: black;} "
                                 , "text { font-family: orbit; }"
                                 , "box { border: 0; margin: 8px; }"
                                 , "levelbar, trough { border-style: none; }"
                                 ]

  Just display <- Gdk.screenGetDefault

  Gtk.styleContextAddProviderForScreen display css (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)


  win <- new Gtk.Window [ #title := "Test" ]
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal ]
  bar <- new Gtk.LevelBar [ #widthRequest := 48 ]
  img <- Gtk.imageNewFromFile "./static/mem.svg"

  _ <- on win #destroy Gtk.mainQuit
  label <- new Gtk.Label [ #widthRequest := 48, #label := "0" ]
  #add win box
  #add box label
  #add box bar
  #add box img

  GLS.initForWindow win
  GLS.autoExclusiveZoneEnable win
  GLS.setMargin win GLS.EdgeTop 3
  GLS.setMargin win GLS.EdgeBottom 3
  GLS.setAnchor win GLS.EdgeBottom True
  GLS.setAnchor win GLS.EdgeRight True
  GLS.setAnchor win GLS.EdgeLeft True

  _ <- forkIO $ clock label 0
  _ <- forkIO $ cpu bar Nothing

  #showAll win

  Gtk.main
