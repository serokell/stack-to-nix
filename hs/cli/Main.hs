import Universum

import Data.Yaml (decodeFileEither)

import Nixage.Project.Yaml (ProjectYaml)

main :: IO ()
main =
    decodeFileEither "project.yaml" >>= \case
      Left err -> print err
      Right (proj :: ProjectYaml) -> do
        putLTextLn "My project.yaml: "
        print proj
