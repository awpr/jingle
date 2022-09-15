{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Jingle.ToMIDI (toMIDIFile, writeMIDIFile) where

import Data.Function ((&))
import Data.List (foldl')
import Data.Ratio (denominator)

import Control.Lens ((%~))

import Sound.MIDI.File qualified as MIDI
import Sound.MIDI.File.Save qualified as MIDI
import Sound.MIDI.File.Event qualified as Event
import Sound.MIDI.Message.Channel qualified as Channel
import Sound.MIDI.Message.Channel.Voice qualified as Voice
import Data.EventList.Relative.TimeBody qualified as TimeBody
import Data.EventList.Relative.TimeTime qualified as TimeTime
import Numeric.NonNegative.Class qualified as NN (C)
import Numeric.NonNegative.Wrapper qualified as NN

import Jingle.Core
import Jingle.Syntax
  ( ChordQuality(..), Chord(..), Interval(..)
  , Articulation(..)
  )
import Jingle.Types (Comp(..), Track(..), Note(..))

type FlatTrack t ann a = TimeTime.T t (Phonon t ann a)

flatten :: forall t ann a. NN.C t => TrackContents t ann a -> FlatTrack t ann a
flatten (Sequence el) = TimeTime.foldr (&) f TimeTime.pause el
 where
  f
    :: Item t (Phonon t ann a)
    -> FlatTrack t ann a
    -> t
    -> FlatTrack t ann a
  f (Single x) rest dt = TimeTime.cons dt x rest
  f (Rep (Repeat (Sequence content) (Sequence end) n)) rest dt =
    TimeTime.delay dt $ rep content end n rest

  rep
    :: TimeTime.T t (Item t (Phonon t ann a))
    -> TimeTime.T t (Item t (Phonon t ann a))
    -> Int
    -> FlatTrack t ann a
    -> FlatTrack t ann a
  rep _ _ n rest | n <= 0 = rest
  rep cont _ 1 rest =
    TimeTime.foldr (&) f (flip TimeTime.delay rest) cont
  rep cont end n rest =
    TimeTime.foldr (&) f
      (flip TimeTime.delay $ rep cont end (n-1) rest)
      (TimeTime.append cont end)

-- Random wishlist items:
-- * Default to the octave closest to the last note
-- * Repeat the last chord
-- * Distinguish "simultaneous" notes from "grouped" notes:
--     C,E,G/4 as a chord held for a 16th note, vs.
--     C&E&G/4 having C,E for a quarter and G for a 16th

expandChordQuality :: ChordQuality -> Note -> [Note]
expandChordQuality q root = root : case q of
  Fifth -> [fifth, octave]
  Maj sv -> [third, fifth] ++ [seventh | sv]
  Min sv -> [third - 1, fifth] ++ [seventh - 1 | sv]
  Dom -> [third, fifth, seventh - 1]
  Aug sv -> [third, fifth + 1] ++ [seventh - 1 | sv]
  Dim sv -> [third - 1, fifth - 1] ++ [seventh - 2 | sv]
  HalfDim -> [third - 1, fifth - 1, seventh - 1]
 where
  third = root + 4
  fifth = root + 7
  seventh = root + 11
  octave = root + 12

expandChord :: Chord -> [Note]
expandChord (Chord root q adds) =
  maybe pure expandChordQuality q root ++
  map (\ (Interval x) -> Note x + root) adds

noteToMIDI :: Note -> Voice.Pitch
noteToMIDI (Note x) = Voice.toPitch $ x + 12

quantizeTimes
  :: Integer
  -> FlatTrack NN.Rational ann a
  -> FlatTrack NN.Integer ann a
quantizeTimes denom =
  fmap (phDuration %~ truncate . (* fromIntegral denom)) .
  TimeTime.mapTime (truncate . (* fromIntegral denom))

interpArticulation :: Maybe Articulation -> (NN.Integer, Int)
interpArticulation art =
  case art of
    Nothing -> (7, 96)
    Just Staccato -> (4, 96)
    Just Marcato -> (4, 127)
    Just Accent -> (7, 127)
    Just Tenuto -> (8, 112)
    Just Legato -> (8, 96)

toMIDIEvents :: Phonon NN.Integer (Maybe Articulation) Note -> [(NN.Integer, Event.T)]
toMIDIEvents (Phonon dur art x) =
  [ (0, mkEvent Voice.NoteOn)
  , ((durFactor * dur) `div` 8, mkEvent Voice.NoteOff)
  ]
 where
  mkEvent f =
    Event.MIDIEvent $
      Channel.Cons
        (Channel.toChannel 0)
        (Channel.Voice $ f (noteToMIDI x) (Voice.toVelocity vel))
  (durFactor, vel) = interpArticulation art

toTimeBody :: TimeTime.T t a -> TimeBody.T t a
toTimeBody =
  TimeTime.foldr (&)
    (\ x rest dt -> TimeBody.cons dt x rest)
    (const TimeBody.empty)

toTrack
  :: Integer
  -> FlatTrack NN.Rational (Maybe Articulation) Chord
  -> MIDI.Track
toTrack denom =
  toTimeBody .
  TimeTime.moveBackward .
  TimeTime.flatten .
  fmap (concatMap toMIDIEvents . phNote expandChord) .
  quantizeTimes denom

toMIDIFile :: Comp (TrackContents NN.Rational (Maybe Articulation) Chord) -> MIDI.T
toMIDIFile (Comp _tempo ts) =
  MIDI.Cons
    (if length ts > 1 then MIDI.Parallel else MIDI.Mixed)
    (MIDI.Ticks (NN.fromNumber $ fromIntegral denom))
    (map (toTrack denom . _trContents) flattened)
 where
  flattened = fmap flatten <$> ts
  denom = 8 * foldl' lcm 1
    [ denominator (NN.toNumber x)
    | t <- flattened
    , x <- TimeTime.getTimes (_trContents t)
    ]

writeMIDIFile
  :: FilePath
  -> Comp (TrackContents NN.Rational (Maybe Articulation) Chord)
  -> IO ()
writeMIDIFile p = MIDI.toFile p . toMIDIFile
