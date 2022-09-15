{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Applicative ((<**>))

import Options.Applicative
  ( Parser, execParser, strArgument, help, metavar
  , helper, info, fullDesc, progDesc, header
  )

import Jingle.Driver (compileFile)

data Opts = Opts
  { srcFile :: String
  , dstFile :: String
  }

opts :: Parser Opts
opts =
  Opts
    <$> strArgument (help "Source file" <> metavar "SRC")
    <*> strArgument (help "Destination MIDI file" <> metavar "DST")

main :: IO ()
main = do
  Opts{..} <- execParser $
    info (opts <**> helper) $ mconcat
      [ fullDesc
      , progDesc "Compile a jingle to MIDI"
      , header "jingle - a CLI tool for jingle music notation"
      ]
  compileFile srcFile dstFile
