{-# LANGUAGE ImportQualifiedPost #-}

module Jingle.Driver (Options(..), compileToMIDI, compileToFile) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Text.IO qualified as T

import Data.Portray.Prettyprinter (pp)
import Text.Megaparsec (parse, errorBundlePretty)
import Sound.MIDI.File qualified as MIDI
import Sound.MIDI.File.Save qualified as MIDI

import Jingle.Parser (comp)
import Jingle.Desugar (dsTrackContents)
import Jingle.ToMIDI (toMIDIFile)
import Orphans ()

data Options = Options
  { _optDumpAST :: Bool
  , _optDumpCore :: Bool
  }

compileToMIDI :: Options -> FilePath -> IO MIDI.T
compileToMIDI opts src = do
  contents <- T.readFile src
  ast <- case parse comp src contents of
    Left err -> throwIO $ userError $ errorBundlePretty err
    Right r -> pure r

  when (_optDumpAST opts) $ pp ast

  let core = fmap (dsTrackContents 1 Nothing) ast

  when (_optDumpCore opts) $ pp core

  return $ toMIDIFile core

compileToFile :: Options -> FilePath -> FilePath -> IO ()
compileToFile opts src dst = MIDI.toFile dst =<< compileToMIDI opts src
