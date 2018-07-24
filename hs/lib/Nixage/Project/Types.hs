module Nixage.Project.Types
    ( NixHash

    , NixpkgsVersion (..)
    , StackageVersion (..)

    , PackageName
    , PackageVersion
    , PackagePath

    , ExternalSource (..)
    ) where

import Universum

import Data.Aeson (FromJSON, ToJSON, (.:))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)


-- | Hash used for nix source (currently sha256)
type NixHash = Text

-- | Version of @nixpkgs@
data NixpkgsVersion = NixpkgsVersion
    { nvUrl  :: Text
    , nvSha256 :: NixHash
    }
  deriving (Eq, Generic, Show)

deriveJSON defaultOptions ''NixpkgsVersion

-- | Version of @nixpkgs-stackage@
data StackageVersion = StackageVersion
    { svUrl    :: Text
    , svSha256 :: Text
    }
  deriving (Eq, Generic, Show)

deriveJSON defaultOptions ''StackageVersion

-- | Name of a Haskell package
type PackageName = Text

-- | Version of a Haskell package
type PackageVersion = Text

-- | Path to a Haskell package
type PackagePath = Text


-- | Description of a way to obtain the source of the package
data ExternalSource
    = GitSource
        { gsGit    :: Text
        , gsRev    :: Text
        }
  deriving (Eq, Generic, Show)
