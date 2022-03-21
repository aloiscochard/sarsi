module Rosetta where

import Data.List (find)

fromExtension :: String -> Maybe LanguageTag
fromExtension ext = find (\t -> languageExtension t == ext) languageTags

fromTool :: String -> Maybe ProjectTag
fromTool tool = find (\t -> projectTool t == tool) projectTags

data LanguageTag = CC | CP | CS | FS | HS | JV | NX | RS | SC
  deriving (Bounded, Enum, Eq, Ord)

languageExtension :: LanguageTag -> String
languageExtension CC = "c"
languageExtension CP = "cpp"
languageExtension CS = "cs"
languageExtension FS = "fs"
languageExtension HS = "hs"
languageExtension JV = "java"
languageExtension NX = "nix"
languageExtension RS = "rs"
languageExtension SC = "scala"

languageTags :: [LanguageTag]
languageTags = [minBound ..]

languageLabel :: LanguageTag -> String
languageLabel CC = "c"
languageLabel CP = "c++"
languageLabel CS = "csharp"
languageLabel FS = "fsharp"
languageLabel HS = "haskell"
languageLabel JV = "java"
languageLabel NX = "nix"
languageLabel RS = "rust"
languageLabel SC = "scala"

data ProjectTag = CABAL | CARGO | DOTNET | MAKE | NIX | SBT | STACK
  deriving (Bounded, Enum, Eq, Ord)

projectTags :: [ProjectTag]
projectTags = [minBound ..]

projectLanguages :: ProjectTag -> [LanguageTag]
projectLanguages CABAL = [HS]
projectLanguages CARGO = [RS]
projectLanguages DOTNET = [CS, FS]
projectLanguages MAKE = [CC, CP]
projectLanguages NIX = [NX]
projectLanguages SBT = [JV, SC]
projectLanguages STACK = [HS]

projectTool :: ProjectTag -> String
projectTool CABAL = "cabal"
projectTool CARGO = "cargo"
projectTool DOTNET = "dotnet"
projectTool MAKE = "make"
projectTool NIX = "nix-shell"
projectTool SBT = "sbt"
projectTool STACK = "stack"

projectToolBuildArgs :: ProjectTag -> [String]
projectToolBuildArgs MAKE = []
projectToolBuildArgs NIX = ["."]
projectToolBuildArgs SBT = ["compile"]
projectToolBuildArgs _ = ["build"]
