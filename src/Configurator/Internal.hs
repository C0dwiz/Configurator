-- SPDX-License-Identifier: MIT
-- Copyright (C) 2026 CodWiz

{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Configurator.Internal where

import           Configurator.TH (mapE, toPairExp)
import           Control.Monad (unless, foldM)
import           Data.Aeson    (FromJSON, Value(..), (.:), (.:?), parseJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.Maybe    (fromMaybe)
import           Data.List     (intercalate)
import           Data.Text     (Text, unpack, pack, splitOn, replace, toUpper)
import qualified Data.Text     as T
import qualified Data.Yaml     as Yaml
import qualified Data.Aeson    as Aeson
import qualified Data.Vector   as V
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           GHC.Generics  (Generic)
import           System.Directory (doesFileExist)
import           System.Environment (lookupEnv)
import           System.FilePath (takeExtension)


type Config = Map.Map Text Value


-- Helper for nested value lookup
lookupValue :: Text -> Value -> Maybe Value
lookupValue key val = go (parseKeyPath key) val
  where
    go :: [KeyComponent] -> Value -> Maybe Value
    go [] v = Just v
    go (KeyName k:ks) (Object o) = KeyMap.lookup (Key.fromText k) o >>= go ks
    go (KeyIndex idx:ks) (Array arr) = 
      if idx >= 0 && idx < length arr
        then arr V.!? idx >>= go ks
        else Nothing
    go _ _ = Nothing

-- Parse key path like "database.servers[0].name" into components
data KeyComponent = KeyName Text | KeyIndex Int
  deriving (Show, Eq)

parseKeyPath :: Text -> [KeyComponent]
parseKeyPath path = concatMap parseSegment (T.splitOn "." path)
  where
    parseSegment seg = case T.breakOn "[" seg of
      (name, "") -> [KeyName name | not (T.null name)]
      (name, rest) -> 
        let base = [KeyName name | not (T.null name)]
            indices = extractIndices rest
        in base ++ indices
    
    extractIndices text
      | T.null text = []
      | otherwise = 
          case T.stripPrefix "[" text of
            Nothing -> []
            Just rest -> 
              case T.breakOn "]" rest of
                (idx, after) -> 
                  case reads (T.unpack idx) of
                    [(n, "")] -> KeyIndex n : extractIndices (T.drop 1 after)
                    _ -> []

required :: forall a. (FromJSON a) => Text -> Config -> a
required key config = 
  let val = Object (KeyMap.fromMapText config)
  in case lookupValue key val of 
       Just v -> case Yaml.parseEither parseJSON v of 
         Right res -> res
         Left err -> error $ "Key '" ++ unpack key ++ "' has incorrect type. Parser error: " ++ err
       Nothing -> error $ "Required key '" ++ unpack key ++ "' not found."

optional :: (FromJSON a) => Text -> Config -> Maybe a
optional key config = 
  let val = Object (KeyMap.fromMapText config)
  in case lookupValue key val of 
       Just v -> case Yaml.parseEither parseJSON v of 
         Right res -> Just res
         Left _ -> Nothing
       Nothing -> Nothing

withDefault :: (FromJSON a) => a -> Text -> Config -> a
withDefault defVal key config = fromMaybe defVal (optional key config)

-- | Read configuration from file (auto-detects YAML or JSON by extension)
readConfigFile :: FilePath -> IO (Either String Value)
readConfigFile filePath = 
  case takeExtension filePath of
    ".json" -> do
      content <- BS.readFile filePath
      return $ case Aeson.eitherDecodeStrict content of
        Right val -> Right val
        Left err  -> Left $ "JSON parsing error: " ++ err
    _ -> do  -- Default to YAML for .yaml, .yml, and others
      result <- Yaml.decodeFileEither filePath
      return $ case result of
        Left err -> Left $ "YAML parsing error: " ++ show err
        Right val -> Right val

-- Functions for environment variable override
getAllPaths :: Value -> [Text]
getAllPaths = go ""
  where
    go prefix (Object obj) = concat $ do
      (k, v) <- KeyMap.toList obj
      let newPrefix = if T.null prefix then Key.toText k else prefix <> "." <> Key.toText k
      return $ newPrefix : go newPrefix v
    go _ _ = []

updatePath :: [Text] -> Value -> Value -> Value
updatePath [] _ newValue = newValue
updatePath (k:ks) (Object obj) newValue = 
  let key = Key.fromText k
      innerVal = fromMaybe Null (KeyMap.lookup key obj)
  in Object (KeyMap.insert key (updatePath ks innerVal newValue) obj)
updatePath _ oldValue _ = oldValue

overrideFromEnv :: Text -> Value -> Text -> IO Value
overrideFromEnv prefix val path = do
  let envVarName = unpack $ toUpper (prefix <> "_" <> replace "." "_" path)
  maybeEnv <- lookupEnv envVarName
  case maybeEnv of 
    Nothing -> return val
    Just str -> 
      let newValue = String (pack str)
      in return (updatePath (splitOn "." path) val newValue)

-- | Get a value from config by dot-separated path
getConfig :: Text -> Config -> Maybe Value
getConfig key config = 
  let val = Object (KeyMap.fromMapText config)
  in lookupValue key val

-- | Validate a configuration value
validateValue :: (FromJSON a) => Text -> Config -> Maybe (Either String a)
validateValue key config = 
  case getConfig key config of
    Just v -> Just $ case Yaml.parseEither parseJSON v of
      Right res -> Right res
      Left err  -> Left $ "Validation error at '" ++ unpack key ++ "': " ++ err
    Nothing -> Nothing

-- | Check if a required key exists in config
keyExists :: Text -> Config -> Bool
keyExists key config = case getConfig key config of
  Just _ -> True
  Nothing -> False

-- | Pretty print configuration for debugging
showConfig :: Config -> String
showConfig config = unlines $
  [ "Configuration Map:"
  , "==================" ] ++
  map formatEntry (Map.toList config)
  where
    formatEntry (k, v) = "  " ++ unpack k ++ " => " ++ valueToString v
    
    valueToString :: Value -> String
    valueToString (String t) = "\"" ++ unpack t ++ "\""
    valueToString (Number n) = show n
    valueToString (Bool b) = show b
    valueToString Null = "null"
    valueToString (Array arr) = "[" ++ intercalate ", " (map valueToString (V.toList arr)) ++ "]"
    valueToString (Object o) = "{" ++ show (KeyMap.toList o) ++ "}"

parseConfig :: QuasiQuoter
parseConfig = QuasiQuoter
  { quoteExp  = \_ -> fail "You should use `parseConfig` with a file path, e.g., [parseConfig|config.yaml|]"
  , quotePat  = \_ -> fail "Not supported."
  , quoteType = \_ -> fail "Not supported."
  , quoteDec  = \str -> do
      (prefix, filePath) <- case words str of
                                 [p, f] -> return (pack p, f)
                                 [f]    -> return ("", f)
                                 _      -> fail "Usage: [parseConfig \"PREFIX\"|config.yaml] or [parseConfig|config.yaml|]"

      fileExists <- runIO $ doesFileExist filePath
      unless fileExists (fail $ "Config file not found: " ++ filePath)

      content <- runIO $ readConfigFile filePath
      case content of 
        Left err -> fail $ "Config parsing error in '" ++ filePath ++ "': " ++ err
        Right (v :: Value) -> 
          case Yaml.parseEither (parseJSON :: Value -> Yaml.Parser Config) v of 
            Left err -> fail $ "Configuration format error: " ++ err
            Right config -> do
              let initialValue = Object (KeyMap.fromMapText config)
              let paths = getAllPaths initialValue
              finalValue <- runIO $ if T.null prefix then return initialValue else foldM (overrideFromEnv prefix) initialValue paths

              let finalConfig = case finalValue of 
                                  Object m -> KeyMap.toMapText m
                                  _        -> Map.empty

              let configName = mkName "configMap"
              mapExp <- mapE (listE (map toPairExp (Map.toList finalConfig)))
              let sig = SigD configName (ConT ''Config)
              let val = ValD (VarP configName) (NormalB mapExp) []
              return [sig, val]
  }
