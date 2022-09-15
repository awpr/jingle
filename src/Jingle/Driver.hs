{-# LANGUAGE ImportQualifiedPost #-}

module Jingle.Driver (compileFile) where

import Control.Exception (throwIO)

import Data.Text.IO qualified as T
import Text.Megaparsec (parse, errorBundlePretty)

import Jingle.Parser (comp)
import Jingle.Desugar (dsTrackContents)
import Jingle.ToMIDI (writeMIDIFile)

compileFile :: FilePath -> FilePath -> IO ()
compileFile src dst = do
  contents <- T.readFile src
  ast <- case parse comp src contents of
    Left err -> throwIO $ userError $ errorBundlePretty err
    Right r -> pure r
  writeMIDIFile dst $ fmap (dsTrackContents 0 Nothing) ast
