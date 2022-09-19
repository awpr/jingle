{-# LANGUAGE ImportQualifiedPost #-}

module Jingle.Driver (Options(..), compileToMIDI, compileToFile) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Text.IO qualified as T

import Data.Portray.Prettyprinter (pp)
import Text.Megaparsec (parse, errorBundlePretty)
import Sound.MIDI.File qualified as MIDI
import Sound.MIDI.File.Save qualified as MIDI

import Jingle.Parser (score)
import Jingle.Desugar (dsScore, lowerNotes)
import Jingle.ToMIDI (toMIDIFile)
import Orphans ()

data Options = Options
  { _optDumpAST :: Bool
  , _optDumpCore :: Bool
  }

compileToMIDI :: Options -> FilePath -> IO MIDI.T
compileToMIDI opts src = do
  contents <- T.readFile src
  ast <- case parse score src contents of
    Left err -> throwIO $ userError $ errorBundlePretty err
    Right r -> pure r

  when (_optDumpAST opts) $ pp ast

  let core = dsScore ast

  when (_optDumpCore opts) $ pp core

  let lowered = lowerNotes core

  return $ toMIDIFile lowered

compileToFile :: Options -> FilePath -> FilePath -> IO ()
compileToFile opts src dst = MIDI.toFile dst =<< compileToMIDI opts src
