{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.Gdk.Config as Gdk
import qualified GI.Gtk.Config as Gtk


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "Gtk4LayerShell"
        version = "1.0"
        pkgName = "gi-gtk4-layer-shell"
        pkgVersion = "0.1.0"
        overridesFile = Just "Gtk4LayerShell.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:Gdk" Gdk.overrides, TaggedOverride "inherited:Gtk" Gtk.overrides]
