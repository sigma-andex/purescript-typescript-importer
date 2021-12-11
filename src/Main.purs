module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut as Argonaut
import Data.Array (catMaybes)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import GenCode (genCode)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as AFS
import Node.Path as Path
import Options.Applicative (Parser, argument, command, execParser, fullDesc, header, help, helper, hsubparser, info, long, metavar, progDesc, short, str, strOption, value, (<**>))

data Commands
  = ImportCmd
      { nodeModule :: String
      , outputDir :: String
      }
  | ListCmd

cmdParser :: Parser Commands
cmdParser = hsubparser
  ( command "import" (info importInfo (progDesc "Import a typescript module as purescript ffi"))
      <> command "list" (info listInfo (progDesc "List all typescript modules"))
  )
  where
  importInfo = ado
    nodeModule <- argument str (metavar "NODE_MODULE")
    outputDir <- strOption
      ( long "output-dir"
          <> short 'o'
          <> metavar "DIR"
          <> value "generated-src/"
          <> help "Write output to the directory DIR. Defaults to generated-src/"
      )
    in ImportCmd { nodeModule, outputDir }

  listInfo = pure ListCmd

main :: Effect Unit
main = run =<< execParser opts
  where
  opts = info (cmdParser <**> helper)
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

run :: Commands -> Effect Unit
run (ImportCmd { nodeModule, outputDir }) = launchAff_ do
  let
    modulePath = [ "node_modules", nodeModule ]
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

      Nothing -> log $ "Module " <> nodeModule <> " doesn't seem to contain a valid \"types\" section."
    Left _ -> log $ "Module " <> nodeModule <> " doesn't seem to contain a valid package.json."
run (ListCmd) = launchAff_ do
  nodeModules <- AFS.readdir "node_modules"
  modulesWithTypings <- for nodeModules \nodeModule -> do
    let
      packageJsonPath = Path.concat $ [ "node_modules", nodeModule, "package.json" ]
    eitherPackageJson <- attempt $ AFS.readTextFile UTF8 packageJsonPath
    let
      tryParse = (eitherPackageJson # lmap show) >>= (fromJson >>> lmap Argonaut.printJsonDecodeError)

      packageJson :: Either String PackageJson
      packageJson = tryParse
    case packageJson of
      Right { "types": Just tps } -> pure $ Just { nodeModule, path: Path.concat [ "node_modules", nodeModule, tps ] }
      Right { "typings": Just tps } -> pure $ Just { nodeModule, path: Path.concat [ "node_modules", nodeModule, tps ] }
      _ -> pure Nothing
  let
    output = modulesWithTypings # catMaybes <#> \{ nodeModule, path } -> nodeModule <> " (" <> path <> ")"
  log $ joinWith "\n" output
