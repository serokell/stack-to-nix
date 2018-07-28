module Nixage.Project.Types
    ( NixHash

    , NixpkgsVersion (..)
    , StackageVersion (..)

    , PackageName
    , PackageVersion

    , ExternalSource (..)

    , NixageException(..)
    , GhcOptions (..)
    ) where

import Universum hiding (Show)

import qualified Data.Map.Strict as M
import Data.Aeson ( ToJSON(..), FromJSON(..), withObject
                  , (.:?), Value(..))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Show (Show(..))


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

type GhcOption = Text

data GhcOptions
    = GhcOptions
        { goLocals :: Maybe GhcOption
        , goEverything :: Maybe GhcOption
        , goPackageOptions :: Map PackageName GhcOption
        }
    deriving (Show)

instance ToJSON GhcOptions where
    toJSON (GhcOptions locals everything packageOptions) = toJSON
        . M.alter (const locals) "$locals"
        . M.alter (const everything) "$everything"
        $ packageOptions

instance FromJSON GhcOptions where
    parseJSON = withObject "GhcOptions" $ \v ->
        GhcOptions
            <$> v .:? "$locals"
            <*> v .:? "$everything"
            <*> (parseJSON (Object v) <&> M.delete "$locals" . M.delete "$everything")


data NixageException
    = ProjectNativeToStackConfigException Text
    | YamlDecodingException Text
    | ProjectNativeToNixException Text
    | ConvertException Text
      deriving (Typeable)

instance Show NixageException where
    show (ProjectNativeToStackConfigException t) =
        "ProjectNativeToStackConfigException: " <> toString t
    show (YamlDecodingException t) =
        "YamlDecodingException: " <> toString t
    show (ProjectNativeToNixException t) =
        "ProjectNativeToNixException: " <> toString t
    show (ConvertException t) =
        "ConvertException: " <> toString t

instance Exception NixageException
