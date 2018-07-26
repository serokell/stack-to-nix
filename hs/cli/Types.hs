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
    = YamlConvertIn Text
    deriving (Show)

data ConvertOut
    = StackConvertOut Text Text
    deriving (Show)
