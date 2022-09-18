{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Miscellaneous types that don't fit squarely into a particular module.
module Jingle.Types
  ( Track(..), trVoice, trContents
  , Score(..), scTempo, scTracks
  ) where

import GHC.Generics (Generic)

import Control.Lens (makeLenses)
import Data.Portray (Portray)
import Data.Wrapped (Wrapped(..))
import Data.Text (Text)

data Track a = Track
  { _trVoice :: Text
  , _trContents :: a
  }
  deriving (Generic, Eq, Ord, Show, Functor)
  deriving Portray via Wrapped Generic (Track a)

$(makeLenses ''Track)

data Score a = Score
  { _scTempo :: Int
  , _scTracks :: [Track a]
  }
  deriving (Generic, Eq, Ord, Show, Functor)
  deriving Portray via Wrapped Generic (Score a)

$(makeLenses ''Score)
