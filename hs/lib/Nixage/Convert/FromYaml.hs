module Nixage.Convert.FromYaml
       ( decodeFromYaml
       ) where

import Universum

import Data.Yaml (decodeFileEither)

import Nixage.Project (ProjectNative)
import Nixage.Project.Types (NixageException (..))
import Nixage.Project.Yaml (projectYamlToProjectNative)


-- | read ProjectYaml from project.yaml and convert to ProjectNative
decodeFromYaml :: (MonadIO m, MonadThrow m) => FilePath -> m ProjectNative
decodeFromYaml projectYamlPath =
    liftIO (decodeFileEither projectYamlPath) >>= \case
      Left err -> throwM $ YamlDecodingException (show err)
      Right projectYaml -> return $ projectYamlToProjectNative projectYaml


