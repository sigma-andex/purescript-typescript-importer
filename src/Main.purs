module Main where

import Prelude

import Data.Array (filter)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow, log)
import GenCode (genCode)
import Node.FS.Aff as AFS
import Node.Path as Path
import Node.Process as Process
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

run :: Settings -> Effect Unit
run (Import settings) = launchAff_ do
  cwd <- liftEffect Process.cwd
  let
    paths = [ cwd ] <> [ "node_modules", settings.nodeModule ]
    dir = Path.concat paths
  listedFiles <- AFS.readdir dir
  let
    files = listedFiles
      # filter (\p -> Path.extname p == ".ts")
      <#>
        (\file -> Path.concat $ paths <> [ file ])
  log $ "Importing files:\n" <> joinWith "," files
  generatedCode <- liftEffect $ genCode files
  logShow generatedCode
