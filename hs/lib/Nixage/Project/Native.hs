{-# OPTIONS_GHC -fno-warn-orphans #-}

module Nixage.Project.Native
  ( ProjectNative
  , pattern ProjectNative
  , pResolver
  , pNixpkgs
  , pStackage
  , pExtraDeps
  , AstNixage

  , pattern HackageDepVersionNative
  , pattern SourceDepVersionNative
  ) where

import Universum hiding (toList)

import Data.Map (Map, toList)
import Data.Text (Text)
import Data.Void (Void)
import Nix.Convert (ToNix(..))
import Nix.Expr.Shorthands (mkNonRecSet, mkStr, ($=), attrsE)
import Nix.Expr.Types (NExpr)

import Nixage.Project.Extensible
import Nixage.Project.Types ( NixHash, NixpkgsVersion(..), StackageVersion(..)
                            , PackageName, PackageVersion, ExternalSource(..))


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


instance Monad m => ToNix (ExtraDepVersion AstNixage) m NExpr where
    toNix (HackageDepVersion () s) = return $ mkStr s
    toNix (SourceDepVersion () (GitSource git rev) sha256 subdir) =
        return $ mkNonRecSet $
            [ "git" $= mkStr git
            , "rev" $= mkStr rev
            , "sha256" $= mkStr sha256
            ]
            <> maybeToList (("subdir" $=) . mkStr . toText <$> subdir)
    toNix (XExtraDepVersion v) = absurd v

instance Monad m => ToNix StackageVersion m NExpr where
    toNix (StackageVersion url sha256) = return $ mkNonRecSet
        [ "url" $= mkStr url
        , "sha256" $= mkStr sha256
        ]

instance Monad m => ToNix NixpkgsVersion m NExpr where
    toNix (NixpkgsVersion url sha256) = return $ mkNonRecSet
        [ "url" $= mkStr url
        , "sha256" $= mkStr sha256
        ]

instance Monad m => ToNix ProjectNative m NExpr where
    toNix (Project () r mnv msv ps eds) = do
        mnvExpr <- mapM toNix mnv
        msvExpr <- mapM toNix msv
        let packagesExpr = attrsE $ second (mkStr . toText) <$> toList ps
        edsExpr <- attrsE <$> mapM (sequence . second toNix) (toList eds)

        return $ mkNonRecSet $
            [ "resolver" $= mkStr r
            ]
            <> maybeToList (("nixpkgs" $=) <$> mnvExpr)
            <> maybeToList (("stackage" $=) <$> msvExpr)
            <>
            [ "packages" $= packagesExpr
            , "extra-deps" $= edsExpr
            ]
