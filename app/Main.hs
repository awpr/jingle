{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Applicative ((<**>))
import Control.Exception (bracket)
import System.Directory (removeFile)
import System.IO (openBinaryTempFile, hClose)
import System.Process (callProcess)

import Data.ByteString.Lazy qualified as BL
import Options.Applicative
  ( Parser, execParser, strArgument, help, metavar
  , subparser, command, switch, long
  , helper, info, fullDesc, progDesc, header
  )
import Sound.MIDI.File.Save qualified as MIDI

import Jingle.Driver (Options(..), compileToFile, compileToMIDI)


srcFile :: Parser FilePath
srcFile = strArgument (help "Source file" <> metavar "SRC")

dstFile :: Parser FilePath
dstFile = strArgument (help "Destination MIDI file" <> metavar "DST")

data Action
  = Compile FilePath FilePath
  | Play FilePath

options :: Parser Options
options =
  Options
    <$> switch (long "dump-ast" <> help "Dump the parsed syntax tree")
    <*> switch (long "dump-core" <> help "Dump the desugared core representation")

action :: Parser Action
action = subparser $ mconcat
  [ command "compile" $
      info (Compile <$> srcFile <*> dstFile <**> helper) $ mconcat
        [ progDesc "Compile a jingle to MIDI."
        ]
  , command "play" $
      info (Play <$> srcFile) $ mconcat
        [ progDesc "Play a jingle via a synthesizer."
        ]
  ]

data Invocation = Invocation Options Action

invocation :: Parser Invocation
invocation =
  Invocation
    <$> options
    <*> action

main :: IO ()
main = do
  Invocation opts act <- execParser $
    info (invocation <**> helper) $ mconcat
      [ fullDesc
      , header "jingle - a CLI tool for jingle music notation"
      ]

  case act of
    Compile src dst -> compileToFile opts src dst
    Play src ->
      bracket
        (openBinaryTempFile "/tmp" "jingle.midi")
        (\ (name, h) -> hClose h >> removeFile name)
        (\ (name, h) -> do
          midi <- compileToMIDI opts src
          BL.hPut h (MIDI.toByteString midi)
          hClose h
          callProcess "fluidsynth"
            [ "-a", "alsa", "-m", "alsa_seq", "-l", "-g", "0.75"
            , "-i", "/usr/share/soundfonts/FluidR3_GM.sf2"
            , name
            ]
          return ())
