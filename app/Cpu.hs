{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- |

module Cpu where

import Data.Text qualified as T
import Text.Read (readMaybe)

import System.IO

import Control.Monad            ( when )
import Control.Concurrent       ( forkIO      )
import Control.Concurrent.MVar  ( MVar, newMVar, takeMVar, isEmptyMVar, putMVar)

import Data.GI.Base
import GI.Gtk             qualified as Gtk

import Formatting

import Yummy

liftCpu f (CpuStat a b c d) = CpuStat (f a) (f b) (f c) (f d)
lift2Cpu f (CpuStat a b c d) (CpuStat a' b' c' d' ) = CpuStat (f a a') (f b b') (f c c') (f d d')

data CpuStat = CpuStat {
                 cpuUser :: Int
               , cpuNice :: Int
               , cpuSys  :: Int
               , cpuIdle :: Int
               } deriving (Eq, Ord, Show)

instance Num CpuStat where
  (+) = lift2Cpu (+)
  (-) = lift2Cpu (-)
  (*) = lift2Cpu (*)
  abs = liftCpu abs
  signum = liftCpu signum
  fromInteger a = let a' = fromInteger a in CpuStat a' a' a' a'


readInt :: String -> Maybe Int
readInt = readMaybe

readCpu :: IO [CpuStat]
readCpu = do
  withFile "/proc/stat" ReadMode $ \h -> do
    l <- hGetLine h
    let (_:(Just a): (Just b):(Just c):(Just d):_) = map readInt $ words l
    return $ [CpuStat a b c d]

percentCpu :: Fractional a => CpuStat -> a
percentCpu CpuStat{..} = (total - fromIntegral cpuIdle) / total
  where
    total = fromIntegral $ cpuUser + cpuNice + cpuSys + cpuIdle


data Cpu where
  Cpu :: Gtk.Label -> MVar [CpuStat] -> Cpu

instance Yummy Cpu where
  type Widget Cpu = Gtk.Label

  view (Cpu w _) = w


  init :: MVar () -> IO Cpu
  init mvar = do
    bar <- new Gtk.Label [ #widthRequest := 128 ]
    cpu <- readCpu
    var <- newMVar cpu
    let
      go :: [CpuStat] -> IO ()
      go cpu1 = do
          b <- isEmptyMVar mvar
          delay1
          cpu2 <- readCpu
          when b $ putMVar var (zipWith (-) cpu1 cpu2) >> go cpu2

    _ <- forkIO $ go cpu
    return $ Cpu bar var

  updateView (Cpu bar var) = do
    cpus <- takeMVar var
    set bar [ #label := sformat (fixed 1 % " %") $ (*100) . percentCpu $ maximum cpus ]
