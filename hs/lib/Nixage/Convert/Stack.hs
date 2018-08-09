{-# LANGUAGE RecordWildCards #-}

module Nixage.Convert.Stack
       ( projectNativeToStackFiles
       , encodeToStack
       , StackFilesInfo (..)
       ) where

import Universum

import Control.Arrow ((>>>))
import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import qualified Data.HashMap.Strict as HM
import Data.Yaml (encode, encodeFile)

import Nixage.Project.Extensible (ExtraDepVersion (..), Project (..))
import Nixage.Project.Native (AstNixage, ProjectNative)
import Nixage.Project.Types (ExternalSource (GitSource), GhcOptions, PackageName, PackageVersion)


data StackExtraDepVersion =
      StackHackageDepVersion PackageVersion
    | StackGitDepVersion Text Text (Maybe FilePath) -- git, rev, subdir
    deriving Show

-- | Stack snapshot (name, resolver, packages)
data StackCustomSnapshot = StackCustomSnapshot
    { scsName     :: Text
    , scsResolver :: Text
    , scsPackages :: HashMap PackageName StackExtraDepVersion
    } deriving Show

data StackConfig = StackConfig
    { scCustomSnapshot :: StackCustomSnapshot
    , scPackages       :: HashMap PackageName FilePath
    , scGhcOptions     :: Maybe GhcOptions
    } deriving Show

data StackFilesInfo = StackFilesInfo
    { sfiSnapshot    :: FilePath
    , sfiShell       :: FilePath
    , sfiShellSource :: FilePath
    , sfiRoot        :: FilePath
    }


instance ToJSON StackCustomSnapshot where
    toJSON StackCustomSnapshot{..} =
        object [ "name" .= scsName
               , "resolver" .= scsResolver
               , "packages" .= map packageToJson (HM.toList scsPackages)
               ]
      where
        packageToJson :: (PackageName, StackExtraDepVersion) -> Value
        packageToJson (packageName, StackHackageDepVersion packageVersion) =
            String $ packageName <> "-" <> packageVersion
        packageToJson (_, StackGitDepVersion url rev subdir) =
            object [ "git"     .= url
                   , "commit"  .= rev
                   , "subdirs" .= [fromMaybe "." subdir]
                   ]

-- | StackConfig json split into stack and snapshot jsons.
stackToJSON :: StackFilesInfo -> StackConfig -> (Value, Value, Text)
stackToJSON StackFilesInfo{..} StackConfig{..}
    = (stack, toJSON scCustomSnapshot, stackShell)
  where
    stack = object
        [ "resolver" .= sfiSnapshot
        , "packages" .= elems scPackages
        , "nix" .= nix
        , "ghc-options" .= scGhcOptions
        ]

    nix = object
        [ "enable" .= True
        , "shell-file" .= sfiShell
        ]

    stackShell = toText $ "import " <> sfiShellSource <> " " <> sfiRoot

-- | Convert ProjectNative AST to StackConfig
projectNativeToStackConfig :: ProjectNative -> StackConfig
projectNativeToStackConfig (Project () resolver _ _ ps eds go) =
    let packages = map toStackExtraDep eds
        snapshot = StackCustomSnapshot "nixage-stack-snapshot" resolver packages
    in StackConfig snapshot ps go
  where
    toStackExtraDep :: ExtraDepVersion AstNixage -> StackExtraDepVersion
    toStackExtraDep (HackageDepVersion () v) =
        StackHackageDepVersion v
    toStackExtraDep (SourceDepVersion () (GitSource git rev) _ msd) =
        StackGitDepVersion git rev msd
    toStackExtraDep (XExtraDepVersion v) =
        absurd v

-- | Pure native to stack conversion
projectNativeToStackFiles :: StackFilesInfo
                          -> ProjectNative
                          -> (ByteString, ByteString, ByteString)
projectNativeToStackFiles stackFilesInfo  =
        projectNativeToStackConfig
    >>> stackToJSON stackFilesInfo
    >>> (\(x,y,z) -> (encode x, encode y, encodeUtf8 z))


-- | Conversion + IO that writes stack and snapshot yaml files
encodeToStack :: (MonadIO m, MonadThrow m)
              => FilePath        -- ^ Stack yaml path
              -> StackFilesInfo
              -> ProjectNative
              -> m ()
encodeToStack stackPath sfi@StackFilesInfo{sfiSnapshot, sfiShell} =
        projectNativeToStackConfig
    >>> stackToJSON sfi
    >>> \(stack, snapshot, shell) -> liftIO $ do
            encodeFile sfiSnapshot snapshot
            encodeFile stackPath stack
            writeFile sfiShell shell
