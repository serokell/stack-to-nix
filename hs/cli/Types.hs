module Types where

-- |  Nixage cli command types

import Universum

data NixageCmd
    = StackCmd StackArgs
    | ConvertCmd ConvertArgs
    deriving (Show)

type StackArgs = [Text]

data ConvertArgs = ConvertArgs ConvertIn ConvertOut deriving (Show)

data ConvertIn
    = YamlConvertIn FilePath
    deriving (Show)

data ConvertOut
    = StackConvertOut FilePath FilePath FilePath
    | NixConvertOut
    deriving (Show)
