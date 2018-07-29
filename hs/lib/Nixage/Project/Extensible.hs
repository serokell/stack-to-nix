{-# LANGUAGE UndecidableInstances #-}

module Nixage.Project.Extensible
  ( Project (..)
  , XProject

  , ExtraDepVersion (..)
  , XHackageDepVersion
  , XSourceDepVersion
  , XXExtraDepVersion
  ) where

import Universum

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import GHC.Generics (Generic)

import Nixage.Project.Types (ExternalSource, GhcOptions, NixHash, NixpkgsVersion, PackageName,
                             PackageVersion, StackageVersion)


-- | Extensible project specification AST
data Project x = Project
    { pXProject   :: !(XProject x)

    , pResolver   :: Text
    , pNixpkgs    :: Maybe NixpkgsVersion
    , pStackage   :: Maybe StackageVersion

    , pPackages   :: HashMap PackageName FilePath

    , pExtraDeps  :: HashMap PackageName (ExtraDepVersion x)

    , pGhcOptions :: Maybe GhcOptions
    }
  deriving (Generic)

type family XProject x :: *

-- | Extensible package version specification
data ExtraDepVersion x
    = HackageDepVersion !(XHackageDepVersion x) PackageVersion
    | SourceDepVersion !(XSourceDepVersion x) ExternalSource NixHash (Maybe FilePath)
    | XExtraDepVersion !(XXExtraDepVersion x)
  deriving (Generic)

deriving instance
  ( Eq (XHackageDepVersion x)
  , Eq (XSourceDepVersion x)
  , Eq (XXExtraDepVersion x)
  ) => Eq (ExtraDepVersion x)

type family XHackageDepVersion x :: *
type family XSourceDepVersion x :: *
type family XXExtraDepVersion x :: *
