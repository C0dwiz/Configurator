-- SPDX-License-Identifier: MIT
-- Copyright (C) 2026 CodWiz

{-# LANGUAGE OverloadedStrings #-}

module Configurator.Internal.Test
  ( testConfig
  , nestedConfig
  ) where

import qualified Data.Map.Strict as Map
import Data.Aeson (Value(..))
import Configurator.Internal (Config)

-- | Test data: simple configuration
testConfig :: Config
testConfig = Map.fromList
  [ ("database.host", String "localhost")
  , ("database.port", Number 5432)
  , ("database.user", String "admin")
  , ("log_level", String "DEBUG")
  , ("metrics.enabled", Bool True)
  , ("metrics.interval_seconds", Number 30)
  ]

-- | Test data: nested configuration
nestedConfig :: Config
nestedConfig = Map.fromList
  [ ("app.name", String "MyApp")
  , ("app.version", String "1.0.0")
  , ("server.host", String "0.0.0.0")
  , ("server.port", Number 8080)
  , ("server.ssl.enabled", Bool False)
  , ("server.ssl.cert_path", String "/etc/ssl/cert.pem")
  ]
