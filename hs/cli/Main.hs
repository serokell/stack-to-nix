import Universum

import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile, withTempFile)
import System.Process (createProcess, delegate_ctlc, proc, waitForProcess)

import Nixage.Convert.FromYaml (decodeFromYaml)
import Nixage.Convert.Nix (projectNativeToPrettyNix)
import Nixage.Convert.Stack (StackFilesInfo (..), encodeToStack)
import Paths_nixage (getDataFileName)

import Parser
import Types


main :: IO ()
main = execParser (info (helper <*> nixageP) infoMod) >>= \case
    StackCmd stackArgs     -> stackAction stackArgs
    ConvertCmd convertArgs -> convertAction convertArgs
  where
    infoMod = header "Nixage"
           <> progDesc "Build Haskell packages with Nix and Stackage"
           <> fullDesc


-- | * Nixage command actions

-- | Create temporary '[standard-tmp-dir]/nixage-snapshot[rand-num].yaml'
-- and './nixage-stack[rand-num].yaml', and run 'stack' on them.
stackAction :: (MonadIO m, MonadThrow m, MonadMask m) => StackArgs-> m ()
stackAction args = do
    projectNative <- decodeFromYaml "project.yaml"
    withSystemTempFile "nixage-stack-snapshot.yaml" $ \snapshotPath snapshotH ->
      withTempFile "." "nixage-stack.yaml" $ \stackPath stackH -> do
        withTempFile "." "nixage-stack-shell.nix" $ \stackShellPath stackShellH -> do
          stackShellSourcePath <- liftIO $ getDataFileName "stack-shell.nix"
          let stackFilesInfo = StackFilesInfo snapshotPath stackShellPath stackShellSourcePath "./."
          liftIO $ hClose stackShellH >> hClose stackH >> hClose snapshotH
          encodeToStack stackPath stackFilesInfo projectNative
          let  args' = ["--stack-yaml", toText stackPath] <> args
          liftIO $ do
              (_,_,_,handle) <- createProcess $
                   (proc "stack" (toString <$> args')) { delegate_ctlc = True }
              void $ waitForProcess handle

-- | Conversion between project specification formats.
convertAction :: (MonadIO m, MonadThrow m) => ConvertArgs -> m ()
convertAction (ConvertArgs convertIn convertOut) = do
    projectNative <- case convertIn of
      YamlConvertIn yamlPath -> decodeFromYaml (toString yamlPath)
    case convertOut of
      StackConvertOut stackPath snapshotPath stackShellPath -> do
        stackShellSourcePath <- liftIO $ getDataFileName "stack-shell.nix"
        let stackFilesInfo = StackFilesInfo snapshotPath stackShellPath stackShellSourcePath "./."
        encodeToStack (toString stackPath) stackFilesInfo projectNative
      NixConvertOut -> do
          print $ projectNativeToPrettyNix projectNative
