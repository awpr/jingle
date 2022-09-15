{-# LANGUAGE ImportQualifiedPost #-}

module Jingle.Driver (compileToMIDI, compileToFile) where

import Control.Exception (throwIO)

import Data.Text.IO qualified as T
import Text.Megaparsec (parse, errorBundlePretty)
import Sound.MIDI.File qualified as MIDI
import Sound.MIDI.File.Save qualified as MIDI

import Jingle.Parser (comp)
import Jingle.Desugar (dsTrackContents)
import Jingle.ToMIDI (toMIDIFile)

compileToMIDI :: FilePath -> IO MIDI.T
compileToMIDI src = do
  contents <- T.readFile src
  ast <- case parse comp src contents of
    Left err -> throwIO $ userError $ errorBundlePretty err
    Right r -> pure r
  return $ toMIDIFile $ fmap (dsTrackContents 1 Nothing) ast

compileToFile :: FilePath -> FilePath -> IO ()
compileToFile src dst = MIDI.toFile dst =<< compileToMIDI src
