{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Orphans () where

import Data.Portray (Portray(..))
import qualified Numeric.NonNegative.Wrapper as NN

instance Portray a => Portray (NN.T a) where
  portray = portray . NN.toNumber
