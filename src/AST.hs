{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module AST
    ( Note(..)
    , ChordQuality(..), Chord(..), Interval(..), Voicing(..)
    , Phonon(..), phAdvance, phVoicing
    , Articulation(..), Articulated(..), arArticulation, arVal
    , Musel(..), muContent, muDuration
    , Repeat(..), TrackPiece(..), Track(..), Comp(..)
    , durations
    ) where

import GHC.Generics (Generic)

import Control.Lens.TH (makeLenses)
import Data.Text (Text)

import Data.Portray (Portray)
import Data.Wrapped (Wrapped(..))

newtype Note = Note
  { noteValue :: Int
    -- ^ Chromatically ascending 12-tet, with C0 assigned to "0"
  }
  deriving (Generic, Eq, Ord, Read, Show, Num, Enum, Real, Integral)
  deriving Portray via Wrapped Generic Note

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
  { _cQuality :: Maybe ChordQuality
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

data Voicing = Voicing
  { _voPitch :: Note
  , _voChord :: Chord
  }
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic Voicing

data Articulated a = Articulated
  { _arVal :: a
  , _arArticulation :: Maybe Articulation
  }
  deriving (Generic, Eq, Ord, Read, Show, Functor)
  deriving Portray via Wrapped Generic (Articulated a)

$(makeLenses ''Articulated)

-- One "thing" to be played in the track: a chord, note, etc. along with any
-- articulation, duration, etc.
data Phonon a = Phonon
  { _phVoicing :: a
  , _phAdvance :: Bool
  }
  deriving (Generic, Eq, Ord, Read, Show, Functor)
  deriving Portray via Wrapped Generic (Phonon a)

$(makeLenses ''Phonon)

data Musel t a = Musel
  { _muDuration :: t
  , _muContent :: Maybe (Phonon a)
  }
  deriving (Generic, Eq, Ord, Read, Show, Functor)
  deriving Portray via Wrapped Generic (Musel t a)

$(makeLenses ''Musel)

data Repeat = Repeat
  { _repContents :: [TrackPiece]
  , _repEnding :: [TrackPiece]
  , _repCount :: Int
  }
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic Repeat

data TrackPiece
  = Single (Musel Rational (Articulated Voicing))
  | Group [TrackPiece] Rational (Maybe Articulation)
  | Rep Repeat
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic TrackPiece

durations :: TrackPiece -> [Rational]
durations (Single (Musel d _)) = [d]
durations (Group ps d _) = concatMap (map (*d) . durations) ps
durations (Rep (Repeat cont end _)) = concatMap durations (cont ++ end)

data Track = Track
  { _trContents :: [TrackPiece]
  , _trVoice :: Text
  }
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic Track

data Comp = Comp
  { _coTempo :: Int
  , _coTracks :: [Track]
  }
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic Comp
