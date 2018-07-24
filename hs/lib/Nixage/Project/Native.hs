module Nixage.Project.Native
  ( ProjectNative
  , pattern ProjectNative
  , pResolver
  , pNixpkgs
  , pStackage
  , pExtraDeps

  , pattern HackageDepVersionNative
  , pattern SourceDepVersionNative
  ) where

import Universum

import Data.Map (Map)
import Data.Text (Text)
import Data.Void (Void)

import Nixage.Project.Extensible
import Nixage.Project.Types ( NixHash, NixpkgsVersion, StackageVersion
                            , PackageName, PackageVersion, ExternalSource)


-- | Nixage native AST marker
data AstNixage

-- | Nixage native project AST
type ProjectNative = Project AstNixage

type instance XProject AstNixage = ()

type instance XHackageDepVersion AstNixage = ()
type instance XSourceDepVersion AstNixage = ()
type instance XXExtraDepVersion AstNixage = Void

deriving instance Show (Project AstNixage)
deriving instance Show (ExtraDepVersion AstNixage)


pattern ProjectNative :: Text
                      -> (Maybe NixpkgsVersion)
                      -> (Maybe StackageVersion)
                      -> Map PackageName FilePath
                      -> Map PackageName (ExtraDepVersion AstNixage)
                      -> Project AstNixage
pattern ProjectNative r mnv msv ps eds = Project () r mnv msv ps eds


pattern HackageDepVersionNative :: PackageVersion
                                -> ExtraDepVersion AstNixage
pattern HackageDepVersionNative pv = HackageDepVersion () pv

pattern SourceDepVersionNative :: ExternalSource
                               -> NixHash
                               -> Maybe FilePath
                               -> ExtraDepVersion AstNixage
pattern SourceDepVersionNative es nh msd = SourceDepVersion () es nh msd
