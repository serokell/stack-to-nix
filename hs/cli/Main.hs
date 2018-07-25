import Universum

import Data.Yaml (decodeFileEither, encodeFile)
import Options.Applicative ( Parser, info, fullDesc, progDesc, header
                           , execParser, hsubparser, command
                           , forwardOptions, strArgument, ParserInfo)
import System.IO.Temp (withSystemTempFile, withTempFile)
import System.Process (waitForProcess, createProcess, delegate_ctlc, proc)

import Nixage.Project.Yaml (projectYamlToProjectNative)
import Nixage.Project.Types (NixageError(..))
import Nixage.Convert.Stack (writeStackConfig, projectNativeToStackConfig)

main :: IO ()
main = execParser (info nixageP infoMod) >>= \case
    StackCmd args -> stackAction args
  where
    infoMod = header "Nixage"
           <> progDesc "Build Haskell packages with Nix and Stackage"
           <> fullDesc


-- | * Nixage cli command type and parsers

data NixageCmd = StackCmd StackArgs
type StackArgs = [Text]

nixageP :: Parser NixageCmd
nixageP = hsubparser $
    command "stack" stackPI

stackPI :: ParserInfo NixageCmd
stackPI = info (StackCmd <$> stackP)
        $ progDesc "Stack command" <> forwardOptions

-- | Parse all arguments after 'stack' command as raw [Text]
stackP :: Parser StackArgs
stackP = many $ strArgument mempty


-- | * Stack command actions

-- | Create temporary '[standard-tmp-dir]/nixage-snapshot[rand-num].yaml'
-- and './nixage-stack[rand-num].yaml', and run 'stack' on them.
stackAction :: (MonadIO m, MonadThrow m, MonadMask m) => StackArgs-> m ()
stackAction args = do
    liftIO (decodeFileEither "project.yaml") >>= \case
      Left err -> throwM $ YamlDecodingError (show err)
      Right projectYaml -> do
        let projectNative = projectYamlToProjectNative projectYaml
        let stackConfig = projectNativeToStackConfig projectNative
        withSystemTempFile "nixage-stack-snapshot.yaml" $ \snapshotPath _ ->
          withTempFile "." "nixage-stack.yaml" $ \stackPath _ -> do
            let (snapshot, stack) = writeStackConfig stackConfig snapshotPath
            let  args' = ["--stack-yaml", toText stackPath] <> args
            liftIO $ do
                encodeFile snapshotPath snapshot
                encodeFile stackPath stack
                (_,_,_,handle) <- createProcess $
                    (proc "stack" (toString <$> args')) { delegate_ctlc = True }
                void $ waitForProcess handle
