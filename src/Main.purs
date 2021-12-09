module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut as Argonaut
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow, log)
import GenCode (genCode)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as AFS
import Node.Path as Path
import Options.Applicative (Parser, argument, command, execParser, fullDesc, header, help, helper, hsubparser, info, long, metavar, progDesc, short, str, strOption, value, (<**>))

data Settings = Import
  { nodeModule :: String
  , outputDir :: String
  }

sample :: Parser Settings
sample = hsubparser
  ( command "import" (info progInfo (progDesc "Import a typescript NODE_MODULE as purescript ffi"))
  )
  where
  progInfo = ado
    nodeModule <- argument str (metavar "NODE_MODULE")
    outputDir <- strOption
      ( long "output-dir"
          <> short 'o'
          <> metavar "DIR"
          <> value "generated-src/"
          <> help "Write output to the directory DIR. Defaults to generated-src/"
      )
    in Import { nodeModule, outputDir }

main :: Effect Unit
main = run =<< execParser opts
  where
  opts = info (sample <**> helper)
    ( fullDesc
        <> progDesc "Runs the given command"
        <> header "purescript <≡> typescript importer"
    )

type PackageJson =
  { "types" :: Maybe String
  , "typings" :: Maybe String
  }

fromJson :: forall t. Argonaut.DecodeJson t => String -> Either Argonaut.JsonDecodeError t
fromJson = Argonaut.parseJson >=> Argonaut.decodeJson

run :: Settings -> Effect Unit
run (Import settings) = launchAff_ do
  let
    modulePath = [ "node_modules", settings.nodeModule ]
    packageJsonPath = Path.concat $ modulePath <> [ "package.json" ]
    modulesDir = Path.concat modulePath
  packageJson <- AFS.readTextFile UTF8 packageJsonPath
  case fromJson packageJson :: Either Argonaut.JsonDecodeError PackageJson of
    Right pkgJson -> case pkgJson."types" <|> pkgJson."typings" of
      Just typingsRelPath -> do
        let
          typingsAbsPath = Path.concat [ modulesDir, typingsRelPath ]
        let
          files = [ typingsAbsPath ]
        generatedCode <- liftEffect $ genCode files
        logShow generatedCode

      Nothing -> log $ "Module " <> settings.nodeModule <> " doesn't seem to contain a valid \"types\" section."
    Left _ -> log $ "Module " <> settings.nodeModule <> " doesn't seem to contain a valid package.json."
