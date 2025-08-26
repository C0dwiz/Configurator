{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import           Configurator (parseConfig, required, optional, withDefault)
import           Data.Text    (Text, unpack)
import           Data.Aeson (FromJSON(..), withObject, (.:), (.:?))

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
  -- Use the `required` function to get a type-safe value
  let dbConfig = required "database" configMap :: DbConfig
  putStrLn $ "Database Host: " ++ unpack (dbHost dbConfig)
  putStrLn $ "Database Port: " ++ show (dbPort dbConfig)
  putStrLn $ "Database User: " ++ unpack (dbUser dbConfig)

  -- Use 'optional' for a value that might not exist
  let password = optional "database.password" configMap :: Maybe Text
  case password of
    Just p  -> putStrLn $ "Database Password: " ++ unpack p
    Nothing -> putStrLn "Database Password: Not set"

  -- Use 'withDefault' for a value with a fallback
  let logLevel = withDefault "INFO" "log_level" configMap :: Text
  putStrLn $ "Log Level: " ++ unpack logLevel