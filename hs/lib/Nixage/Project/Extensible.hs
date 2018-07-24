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

import Data.Aeson (FromJSON)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Nixage.Project.Types ( NixHash, NixpkgsVersion, StackageVersion
                            , PackageName, PackageVersion, ExternalSource)


-- | Extensible project specification AST
data Project x = Project
    { pXProject :: !(XProject x)

    , pResolver :: Text
    , pNixpkgs  :: Maybe NixpkgsVersion
    , pStackage :: Maybe StackageVersion

    , pPackages :: Map PackageName FilePath

    , pExtraDeps :: Map PackageName (ExtraDepVersion x)
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
