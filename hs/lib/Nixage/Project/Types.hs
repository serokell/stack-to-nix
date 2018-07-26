module Nixage.Project.Types
    ( NixHash

    , NixpkgsVersion (..)
    , StackageVersion (..)

    , PackageName
    , PackageVersion

    , ExternalSource (..)

    , NixageError(..)
    ) where

import Universum

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


-- | Description of a way to obtain the source of the package
data ExternalSource
    = GitSource
        { gsGit    :: Text
        , gsRev    :: Text
        }
  deriving (Eq, Generic, Show)

data NixageError =
      ProjectNativeToStackConfigError Text
    | YamlDecodingError Text
    | OtherError Text
    deriving (Show, Typeable)

instance Exception NixageError

