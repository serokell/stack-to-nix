module Nixage.Convert.Nix
       ( projectNativeToPrettyNix
       ) where

import Universum

import Nix.Convert (toNix)
import Nix.Pretty (prettyNix)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Nixage.Project.Native (ProjectNative)

projectNativeToPrettyNix :: ProjectNative -> Doc
projectNativeToPrettyNix = prettyNix . runIdentity . toNix
