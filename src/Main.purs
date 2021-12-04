module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Options.Applicative (Parser, argument, command, execParser, fullDesc, header, help, helper, hsubparser, info, long, metavar, progDesc, short, str, strOption, value, (<**>))

data Opts = Import
  { nodeModule :: String
  , outputDir :: String
  }

sample :: Parser Opts
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

run :: Opts -> Effect Unit
run (Import h) = log $ show h
