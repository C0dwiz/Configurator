{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Configurator (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Configurator"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A type-safe configuration parser."
copyright :: String
copyright = "2025 C0dwiz"
homepage :: String
homepage = "https://github.com/C0dwizigurator"
