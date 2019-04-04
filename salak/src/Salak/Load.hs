-- |
-- Module:      Salak
-- Copyright:   (c) 2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- This module is designed for implementating configuration file loading.
-- Please don't use if you are not implemanting a new configuration file.
--
module Salak.Load(
  -- * Reload
    Reload(..)
  , defReload
  -- * SourcePack
  , Source
  , SourcePack
  , addErr
  -- * Selector
  , Selector(..)
  , simpleSelectors
  -- * Source
  , emptySource
  , nullSource
  , insertSource
  , updateSources
  , updateSource
  -- * Load
  , tryLoadFile
  , loadFile
  , loading
  ) where

import           Salak.Types
import           Salak.Types.Selector
import           Salak.Types.Source
