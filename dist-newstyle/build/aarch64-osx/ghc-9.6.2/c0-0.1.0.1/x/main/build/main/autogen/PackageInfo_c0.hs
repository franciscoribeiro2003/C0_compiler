{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_c0 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "c0"
version :: Version
version = Version [0,1,0,1] []

synopsis :: String
synopsis = "Compilador simples"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
