-- |  Nixage cli command parsers
module Parser where

import Universum

import Options.Applicative (CommandFields, Mod, Parser, command, commandGroup, forwardOptions,
                            hsubparser, info, metavar, optional, progDesc, strArgument)
import Types

nixageP :: Parser NixageCmd
nixageP = hsubparser $ mconcat
    [ command "stack" $ info (StackCmd <$> stackArgsP) $
           progDesc "Run stack on stack.yaml generated from project.yaml"
        <> forwardOptions
    , command "convert" $ info (ConvertCmd <$> convertArgsP) $
           progDesc "Convert between input formats (Yaml, Stack) "
    ]

-- | * Stack command parser
--
-- | Parse all arguments after 'stack' command as raw [Text]
stackArgsP :: Parser StackArgs
stackArgsP = many $ strArgument mempty



-- | * Convert command parsers

convertArgsP :: Parser ConvertArgs
convertArgsP = hsubparser convertArgsInMod

-- | ConvertArgs parser for first command (In or Out with default In)
-- For each (inP, outP) from (Is + [defI]) x Os:
--    apply (ConvertArgs <$> inP <*> outP) parser after nested commands
convertArgsInMod :: Mod CommandFields ConvertArgs
convertArgsInMod =
       (flip foldMap convertInPs $ \(inCmd, inP, defIn) ->
           command' inCmd $ hsubparser $ convertArgsOutMod inP defIn)
    <> convertArgsOutMod defConvertInP defConvertIn
    <> metavar "COMMAND"
    <> commandGroup "Input/output format command"

-- | ConvertArgs parser for second command (Out)
convertArgsOutMod :: Parser ConvertIn -> ConvertIn -> Mod CommandFields ConvertArgs
convertArgsOutMod inP defIn=
       (flip foldMap convertOutPs $ \(outCmd, outP, defOut) ->
            command' outCmd $ fmap (fromMaybe $ ConvertArgs defIn defOut) $
                optional $ ConvertArgs <$> inP <*> outP)
    <> metavar "OUT_COMMAND"
    <> commandGroup "Output format command"

-- | ConvertIn parsers for each ConvertIn constructor
-- Each command should specify its default value
convertInPs  :: [(String, Parser ConvertIn, ConvertIn)]
convertInPs =
    [ ( "from-yaml"
      , YamlConvertIn <$> (strArgument $ metavar "project-yaml")
      , defConvertIn
      )
    ]

-- Default ConvertIn
defConvertInP :: Parser ConvertIn
defConvertInP = pure defConvertIn

defConvertIn :: ConvertIn
defConvertIn = YamlConvertIn "project.yaml"


-- | ConvertOut parsers for each ConvertOut constructor
convertOutPs  :: [(String, Parser ConvertOut, ConvertOut)]
convertOutPs =
    [ ( "to-stack"
      , StackConvertOut
        <$> (strArgument $ metavar "stack-yaml")
        <*> (strArgument $ metavar "snapshot-yaml")
        <*> (strArgument $ metavar "stack-shell")
      , StackConvertOut "stack.yaml" "snapshot.yaml" "stack-shell.nix"
      )
    , ( "to-nix"
      , pure NixConvertOut
      , NixConvertOut
      )
    ]

-- | Helper function
command' :: String -> Parser a -> Mod CommandFields a
command' cmd subP = command cmd $ info subP $ mempty
