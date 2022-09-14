{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Jingle.Syntax
    ( Note(..)
    , ChordQuality(..), Chord(..), Interval(..)
    , Phonon(..), phDuration, phContent
    , Advance(..)
    , Articulation(..), Articulated(..), arArticulation, arVal
    , Repeat(..), TrackPiece(..), TrackContents
    , durations
    ) where

import GHC.Generics (Generic)

import Control.Lens.TH (makeLenses)

import Data.Portray (Portray)
import Data.Wrapped (Wrapped(..))

import Jingle.Types (Note(..))

data ChordQuality
  = Fifth    -- 1    5
  | Maj Bool -- 1  3 5  [  7]
  | Min Bool -- 1 b3 5  [ b7]
  | Dom      -- 1  3 5    b7
  | Aug Bool -- 1  3 #5 [ b7]
  | Dim Bool -- 1 b3 b5 [bb7]
  | HalfDim  -- 1 b3 b5   b7
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic ChordQuality

newtype Interval = Interval { intervalValue :: Int }
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic Interval

data Chord = Chord
  { _cRoot :: Note
  , _cQuality :: Maybe ChordQuality
  , _cAdd :: [Interval]
  }
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic Chord

data Articulation
  = Staccato
  | Marcato
  | Accent
  | Tenuto
  | Legato
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic Articulation

data Articulated a = Articulated
  { _arVal :: a
  , _arArticulation :: Maybe Articulation
  }
  deriving (Generic, Eq, Ord, Read, Show, Functor)
  deriving Portray via Wrapped Generic (Articulated a)

$(makeLenses ''Articulated)

-- One "thing" to be played in the track: a chord, note, or rest, along with
-- any articulation, duration, etc.
data Phonon t a = Phonon
  { _phDuration :: t
  , _phContent :: Maybe a
  }
  deriving (Generic, Eq, Ord, Read, Show, Functor)
  deriving Portray via Wrapped Generic (Phonon t a)

$(makeLenses ''Phonon)

data Advance a = Advance Bool a
  deriving (Generic, Eq, Ord, Read, Show, Functor)
  deriving Portray via Wrapped Generic (Advance a)

data Repeat = Repeat
  { _repContents :: [TrackPiece]
  , _repEnding :: [TrackPiece]
  , _repCount :: Int
  }
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic Repeat

data TrackPiece
  = Single (Advance (Phonon Rational (Articulated Chord)))
  | Group [TrackPiece] Rational (Maybe Articulation)
  | Rep Repeat
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic TrackPiece

durations :: TrackPiece -> [Rational]
durations (Single (Advance _ (Phonon d _))) = [d]
durations (Group ps d _) = concatMap (map (*d) . durations) ps
durations (Rep (Repeat cont end _)) = concatMap durations (cont ++ end)

type TrackContents = [TrackPiece]
