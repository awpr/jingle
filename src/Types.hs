{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Miscellaneous types that don't fit squarely into a particular module.
module Types (Note(..), Track(..), Comp(..)) where

import GHC.Generics (Generic)

import Data.Portray (Portray)
import Data.Wrapped (Wrapped(..))
import Data.Text (Text)

-- | A single 12-tone-equal-temperament note (in a specific octave).
newtype Note = Note
  { noteValue :: Int
    -- ^ Chromatically ascending 12-tet, with C0 assigned to "0"
  }
  deriving (Generic, Eq, Ord, Read, Show, Num, Enum, Real, Integral)
  deriving Portray via Wrapped Generic Note

data Track a = Track
  { _trVoice :: Text
  , _trContents :: a
  }
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic (Track a)

data Comp a = Comp
  { _coTempo :: Int
  , _coTracks :: [Track a]
  }
  deriving (Generic, Eq, Ord, Read, Show)
  deriving Portray via Wrapped Generic (Comp a)
