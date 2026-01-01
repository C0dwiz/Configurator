-- SPDX-License-Identifier: MIT
-- Copyright (C) 2026 CodWiz

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import           Configurator 
import           Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import           Data.Text (Text, unpack)
import qualified Data.Aeson as Aeson

-- Define a simple data type for a database configuration
data DbConfig = DbConfig
  { dbHost :: Text
  , dbPort :: Int
  , dbUser :: Text
  , dbPass :: Maybe Text
  }

-- The 'FromJSON' instance tells aeson how to parse this type
instance FromJSON DbConfig where
  parseJSON = withObject "DbConfig" $ \o ->
    DbConfig
      <$> o .: "host"
      <*> o .: "port"
      <*> o .: "user"
      <*> o .:? "password"

-- The magic line: use the quasi-quoter to load and validate the config file
[parseConfig|app/config.yaml|]

main :: IO ()
main = do
  putStrLn "=== Configurator Example ==="
  putStrLn ""
  
  -- Use the `required` function to get a type-safe value
  let dbConfig = required "database" configMap :: DbConfig
  putStrLn "Database Configuration:"
  putStrLn $ "  Host: " ++ unpack (dbHost dbConfig)
  putStrLn $ "  Port: " ++ show (dbPort dbConfig)
  putStrLn $ "  User: " ++ unpack (dbUser dbConfig)
  putStrLn ""

  -- Use 'optional' for a value that might not exist
  let password = optional "database.password" configMap :: Maybe Text
  case password of
    Just p  -> putStrLn $ "  Password: " ++ unpack p
    Nothing -> putStrLn "  Password: Not set"
  putStrLn ""

  -- Use 'withDefault' for a value with a fallback
  let logLevel = withDefault "INFO" "log_level" configMap :: Text
  putStrLn $ "Log Level: " ++ unpack logLevel
  putStrLn ""

  -- Use 'getConfig' to directly access raw values
  case getConfig "metrics.enabled" configMap of
    Just val -> putStrLn $ "Metrics Enabled: " ++ show val
    Nothing  -> putStrLn "Metrics Enabled: Not found"
  
  case getConfig "metrics.interval_seconds" configMap of
    Just val -> putStrLn $ "Metrics Interval: " ++ show val
    Nothing  -> putStrLn "Metrics Interval: Not found"
  putStrLn ""

  -- Access array elements with new syntax
  putStrLn "Database Servers:"
  case getConfig "database.servers[0]" configMap of
    Just (String val) -> putStrLn $ "  Server 1: " ++ unpack val
    _ -> putStrLn "  Server 1: Not found"
  
  case getConfig "database.servers[1]" configMap of
    Just (String val) -> putStrLn $ "  Server 2: " ++ unpack val
    _ -> putStrLn "  Server 2: Not found"
  
  case getConfig "database.servers[2]" configMap of
    Just (String val) -> putStrLn $ "  Server 3: " ++ unpack val
    _ -> putStrLn "  Server 3: Not found"
  putStrLn ""

  -- Validation example
  putStrLn "Validation Examples:"
  case validateValue "database.port" configMap of
    Just (Right (port :: Int)) -> 
      putStrLn $ "✓ Database port is valid: " ++ show port
    Just (Left err) -> 
      putStrLn $ "✗ Validation error: " ++ err
    Nothing -> 
      putStrLn "✗ Key not found"
  
  case validateValue "log_level" configMap of
    Just (Right (level :: Text)) -> 
      putStrLn $ "✓ Log level is valid: " ++ unpack level
    Just (Left err) -> 
      putStrLn $ "✗ Validation error: " ++ err
    Nothing -> 
      putStrLn "✗ Key not found"
  putStrLn ""

  -- Check if required keys exist
  putStrLn "Key Existence Checks:"
  putStrLn $ "  database.host exists: " ++ show (keyExists "database.host" configMap)
  putStrLn $ "  nonexistent.key exists: " ++ show (keyExists "nonexistent.key" configMap)
  putStrLn ""

  -- Debug: show entire configuration
  putStrLn "Complete Configuration:"
  putStrLn (showConfig configMap)