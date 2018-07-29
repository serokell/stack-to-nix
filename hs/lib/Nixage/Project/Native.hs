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

import Universum

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Void (Void)
import Nix.Convert (ToNix(..))
import Nix.Expr.Shorthands (mkNonRecSet, mkStr, ($=), attrsE)
import Nix.Expr.Types (NExpr)

import Nixage.Project.Extensible
import Nixage.Project.Types ( NixHash, NixpkgsVersion(..), StackageVersion(..)
                            , PackageName, PackageVersion, ExternalSource(..)
                            , GhcOptions(..))


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
                      -> HashMap PackageName FilePath
                      -> HashMap PackageName (ExtraDepVersion AstNixage)
                      -> Maybe GhcOptions
                      -> Project AstNixage
pattern ProjectNative r mnv msv ps eds mgo = Project () r mnv msv ps eds mgo


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

instance Monad m => ToNix GhcOptions m NExpr where
    toNix (GhcOptions locals everything ps) = do
        let psExpr = uncurry ($=) . second mkStr <$> HM.toList ps
        return $ mkNonRecSet $
               maybeToList (("\"$locals\"" $=) . mkStr <$> locals)
            <> maybeToList (("\"$everything\"" $=) . mkStr <$> everything)
            <> psExpr

instance Monad m => ToNix ProjectNative m NExpr where
    toNix (Project () r mnv msv ps eds mgo) = do
        mnvExpr <- mapM toNix mnv
        msvExpr <- mapM toNix msv
        let packagesExpr = attrsE $ second (mkStr . toText) <$> HM.toList ps
        edsExpr <- attrsE <$> mapM (sequence . second toNix) (HM.toList eds)
        mgoExpr <- sequence $ toNix <$> mgo

        return $ mkNonRecSet $
            [ "resolver" $= mkStr r
            ]
            <> maybeToList (("nixpkgs" $=) <$> mnvExpr)
            <> maybeToList (("stackage" $=) <$> msvExpr)
            <>
            [ "packages" $= packagesExpr
            , "extra-deps" $= edsExpr
            ]
            <> maybeToList (("ghc-options" $=) <$> mgoExpr)
