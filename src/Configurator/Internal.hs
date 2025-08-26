{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Configurator.Internal where

import           Configurator.TH (mapE, toPairExp)
import           Control.Monad (unless, foldM)
import           Data.Aeson    (FromJSON, Value(..), (.:), (.:?), parseJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import           Data.Maybe    (fromMaybe)
import           Data.Text     (Text, unpack, pack, splitOn, replace, toUpper)
import qualified Data.Text     as T
import qualified Data.Yaml     as Yaml
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           GHC.Generics  (Generic)
import           System.Directory (doesFileExist)
import           System.Environment (lookupEnv)


type Config = Map.Map Text Value


-- Helper for nested value lookup
lookupValue :: Text -> Value -> Maybe Value
lookupValue key val = go (splitOn "." key) val
  where
    go :: [Text] -> Value -> Maybe Value
    go [] v = Just v
    go (k:ks) (Object o) = KeyMap.lookup (Key.fromText k) o >>= go ks
    go _ _ = Nothing

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

parseConfig :: QuasiQuoter
parseConfig = QuasiQuoter
  { quoteExp  = \_ -> fail "You should use `parseConfig` with a file path, e.g., [parseConfig|config.yaml|]"
  , quotePat  = \_ -> fail "Not supported."
  , quoteType = \_ -> fail "Not supported."
  , quoteDec  = \str -> do
      (prefix, filePath) <- case words str of
                                 [p, f] -> return (pack p, f)
                                 [f]    -> return ("", f)
                                 _      -> fail "Usage: [parseConfig \"PREFIX\"|config.yaml] or [parseConfig|config.yaml]"

      fileExists <- runIO $ doesFileExist filePath
      unless fileExists (fail $ "File not found: " ++ filePath)

      content <- runIO $ Yaml.decodeFileEither filePath
      case content of 
        Left err -> fail $ "YAML parsing error: " ++ show err
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
