module Nixage.Project.Yaml
  ( ProjectYaml
  , pattern ProjectYaml
  , pResolver
  , pNixpkgs
  , pStackage
  , pExtraDeps

  , pattern HackageDepVersionYaml
  , pattern SourceDepVersionYaml

  , projectYamlToProjectNative
  ) where

import Universum

import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?), (.!=), withObject)
import Data.Map (Map)
import Data.Text (Text)
import Data.Void (Void)

import Nixage.Project.Extensible
import Nixage.Project.Native ( ProjectNative, pattern ProjectNative
                             , pattern HackageDepVersionNative
                             , pattern SourceDepVersionNative )
import Nixage.Project.Types ( NixHash, NixpkgsVersion, StackageVersion
                            , PackageName, PackageVersion, ExternalSource(..)
                            , GhcOptions)

-- | Yaml AST marker
data AstYaml

-- | Yaml project AST
type ProjectYaml = Project AstYaml

type instance XProject AstYaml = ()

type instance XHackageDepVersion AstYaml = ()
type instance XSourceDepVersion AstYaml = ()
type instance XXExtraDepVersion AstYaml = Void

deriving instance Show (Project AstYaml)
deriving instance Show (ExtraDepVersion AstYaml)

pattern ProjectYaml :: Text
                    -> (Maybe NixpkgsVersion)
                    -> (Maybe StackageVersion)
                    -> Map PackageName FilePath
                    -> Map PackageName (ExtraDepVersion AstYaml)
                    -> Maybe GhcOptions
                    -> Project AstYaml
pattern ProjectYaml r mnv msv ps eds mgo = Project () r mnv msv ps eds mgo

pattern HackageDepVersionYaml :: PackageVersion
                              -> ExtraDepVersion AstYaml
pattern HackageDepVersionYaml pv = HackageDepVersion () pv

pattern SourceDepVersionYaml :: ExternalSource
                             -> NixHash
                             -> Maybe FilePath
                             -> ExtraDepVersion AstYaml
pattern SourceDepVersionYaml es nh msd = SourceDepVersion () es nh msd

instance FromJSON (ExtraDepVersion AstYaml) where
    parseJSON (String s) = pure $ HackageDepVersionYaml s
    parseJSON (Object v) =
        SourceDepVersionYaml
            <$> externalSourceP
            <*> v .: "sha256"
            <*> v .: "subdir"
      where
        externalSourceP =
            GitSource
                <$> v .: "git"
                <*> v .: "rev"
    parseJSON _ = fail "Invalid extra-dep specificatoin"

instance FromJSON ProjectYaml where
    parseJSON = withObject "Nixage project specification" $ \v ->
        ProjectYaml
            <$> v .:  "resolver"
            <*> v .:? "nixpkgs"
            <*> v .:? "stackage"
            <*> v .:  "packages"
            <*> v .:? "extra-deps" .!= mempty
            <*> v .:? "ghc-options"

projectYamlToProjectNative :: ProjectYaml -> ProjectNative
projectYamlToProjectNative (Project () r mnv msv ps eds mgo) =
    ProjectNative r mnv msv ps eds' mgo
  where
    eds' = eds <&> \case
      HackageDepVersion () v        -> HackageDepVersionNative v
      SourceDepVersion () es nh msb -> SourceDepVersionNative es nh msb
      XExtraDepVersion v            -> absurd v
