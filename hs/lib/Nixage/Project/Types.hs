module Nixage.Project.Types
    ( NixHash

    , NixpkgsVersion (..)
    , StackageVersion (..)

    , PackageName
    , PackageVersion

    , ExternalSource (..)
    ) where

import Data.Text (Text)
import GHC.Generics (Generic)


-- | Hash used for nix source (currently sha256)
type NixHash = Text

-- | Version of @nixpkgs@
data NixpkgsVersion = NixpkgsVersion
    { nvUrl  :: Text
    , nvHash :: NixHash
    }
  deriving (Eq, Generic, Show)

-- | Version of @nixpkgs-stackage@
data StackageVersion = StackageVersion
    { svUrl    :: Text
    , svSha256 :: Text
    }
  deriving (Eq, Generic, Show)


-- | Name of a Haskell package
type PackageName = Text

-- | Version of a Haskell package
type PackageVersion = Text


-- | Description of a way to obtain the source of the package
data ExternalSource
    = GitSource
        { gsUrl    :: Text
        , gsRev    :: Text
        , gsSubdir :: Maybe FilePath
        }
  deriving (Eq, Generic, Show)
