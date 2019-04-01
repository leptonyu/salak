-- |
-- Module:      Salak
-- Copyright:   (c) 2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- This module is designed for implementating configuration file loading.
--
module Salak.Load(
  -- * Reload
    Reload(..)
  , defReload
  -- * SourcePack
  , Source
  , SourcePack
  , SourcePackT
  , addErr'
  -- * Selector
  , Selector(..)
  , simpleSelectors
  -- * Source
  , insertSource
  , updateSources
  , updateSource
  -- * Load
  , tryLoadFile
  , loadFile
  ) where

import           Salak.Types
import           Salak.Types.Selector
import           Salak.Types.Source
