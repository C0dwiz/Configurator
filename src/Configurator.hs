-- SPDX-License-Identifier: MIT
-- Copyright (C) 2026 CodWiz

module Configurator
  ( parseConfig
  , required
  , optional
  , withDefault
  , getConfig
  , validateValue
  , keyExists
  , showConfig
  -- Validator module
  , module Configurator.Validator
  ) where

import           Configurator.Internal
import           Configurator.Validator