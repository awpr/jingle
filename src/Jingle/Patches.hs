{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Jingle.Patches (generalMIDI, toGeneralMIDIProgram) where

import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T

import Sound.MIDI.Message.Channel.Voice qualified as Voice

generalMIDI :: M.Map Text Int
generalMIDI = M.fromList
  -- N.B. the 'midi' package treats these as zero-based, while the MIDI docs
  -- generally treat them as 1-based, so subtract 1 when necessary.
  [ ("piano", 0)
  , ("honky-tonk", 3)
  , ("celesta", 8)
  , ("hammond", 16)
  , ("harmonica", 22)
  , ("classical guitar", 24)
  , ("acoustic guitar", 25)
  , ("jazz guitar", 26)
  , ("clean guitar", 27)
  , ("muted guitar", 28)
  , ("overdrive", 29)
  , ("distortion", 30)
  , ("harmonics", 31)
  , ("upright bass", 32)
  , ("electric bass", 33)
  , ("slap bass", 37)
  , ("viola", 41)
  , ("choir", 52)
  , ("trombone", 57)
  , ("brass", 61)
  , ("tenor", 66)
  , ("bassoon", 70)
  , ("flute", 73)
  , ("ocarina", 79)
  , ("square", 80)
  , ("pad", 90)
  , ("steel drum", 114)
  , ("breath", 121)
  ]

toGeneralMIDIProgram :: Text -> Maybe Voice.Program
toGeneralMIDIProgram =
  fmap Voice.toProgram .
  flip M.lookup generalMIDI . T.toLower
