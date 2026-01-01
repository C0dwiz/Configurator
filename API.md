# Configurator API Documentation

## Overview

Configurator provides a type-safe configuration parsing interface for Haskell applications. All configuration parsing happens at compile-time through Template Haskell quasi-quoters.

## Core Functions

### Configuration Loading

#### `parseConfig :: QuasiQuoter`

Loads and parses a configuration file at compile time.

**Syntax:**
```haskell
[parseConfig|path/to/config.yaml|]
[parseConfig|"PREFIX"|path/to/config.yaml|]
```

**Parameters:**
- File path: Relative path to YAML or JSON configuration file
- Prefix (optional): Environment variable prefix for overriding values

**Returns:** Generates `configMap :: Config` binding

**Example:**
```haskell
[parseConfig|config/app.yaml|]
-- Creates: configMap :: Config
```

---

### Value Access

#### `required :: (FromJSON a) => Text -> Config -> a`

Retrieves a required configuration value. Throws a runtime error if the key is missing or cannot be parsed as the requested type.

**Parameters:**
- Key: Dot-separated path (e.g., `"database.host"`)
- Config: Configuration map from `parseConfig`

**Returns:** Parsed value of type `a`

**Throws:** `error` if key is missing or type mismatch

**Example:**
```haskell
let dbHost = required "database.host" configMap :: String
let dbPort = required "database.port" configMap :: Int
```

---

#### `optional :: (FromJSON a) => Text -> Config -> Maybe a`

Retrieves an optional configuration value, returning `Nothing` if not found or parsing fails.

**Parameters:**
- Key: Dot-separated path
- Config: Configuration map

**Returns:** `Just value` if found and valid, `Nothing` otherwise

**Example:**
```haskell
let password = optional "database.password" configMap :: Maybe String
```

---

#### `withDefault :: (FromJSON a) => a -> Text -> Config -> a`

Retrieves a configuration value with a fallback default.

**Parameters:**
- Default: Value to use if key is missing
- Key: Dot-separated path
- Config: Configuration map

**Returns:** Configuration value, or default if missing

**Example:**
```haskell
let port = withDefault 5432 "database.port" configMap :: Int
```

---

#### `getConfig :: Text -> Config -> Maybe Value`

Gets a raw configuration value without type conversion.

**Parameters:**
- Key: Dot-separated path (supports array syntax: `[0]`, `[1]`)
- Config: Configuration map

**Returns:** `Just Value` if found, `Nothing` otherwise

**Example:**
```haskell
case getConfig "database.servers[0]" configMap of
  Just (String url) -> putStrLn url
  Nothing -> putStrLn "Not found"
```

---

### Utility Functions

#### `keyExists :: Text -> Config -> Bool`

Checks whether a configuration key exists.

**Parameters:**
- Key: Dot-separated path
- Config: Configuration map

**Returns:** `True` if key exists, `False` otherwise

**Example:**
```haskell
if keyExists "logging.enabled" configMap
  then putStrLn "Logging is configured"
  else putStrLn "Logging is not configured"
```

---

#### `showConfig :: Config -> String`

Generates a human-readable representation of the entire configuration.

**Parameters:**
- Config: Configuration map

**Returns:** String representation suitable for logging/debugging

**Example:**
```haskell
putStrLn (showConfig configMap)
```

---

#### `validateValue :: (FromJSON a) => Text -> Config -> Maybe (Either String a)`

Validates and parses a configuration value with detailed error messages.

**Parameters:**
- Key: Dot-separated path
- Config: Configuration map

**Returns:**
- `Just (Right value)` if valid
- `Just (Left error)` if parsing fails
- `Nothing` if key is missing

**Example:**
```haskell
case validateValue "database.port" configMap of
  Just (Right port) -> putStrLn $ "Port: " ++ show (port :: Int)
  Just (Left err)   -> putStrLn $ "Error: " ++ err
  Nothing           -> putStrLn "Key not found"
```

---

## Validation Module

### Constraints

A `Constraint` is a function that validates a single `Value` and returns a list of error messages (empty if valid).

```haskell
type Constraint = Value -> [ValidationError]
```

### Comparison Operators

#### `(>.) :: Text -> Scientific -> Constraint`

Greater than constraint for numeric values.

**Example:**
```haskell
let constraint = "port" >. 1024
let errors = constraint (Number 8080)  -- [] (valid)
```

#### `(<.) :: Text -> Scientific -> Constraint`

Less than constraint.

#### `(>=.) :: Text -> Scientific -> Constraint`

Greater than or equal constraint.

#### `(<=.) :: Text -> Scientific -> Constraint`

Less than or equal constraint.

---

### Range Validation

#### `inRange :: Text -> (Scientific, Scientific) -> Constraint`

Validates that a numeric value is within a range (inclusive).

**Example:**
```haskell
let constraint = inRange "port" (1024, 65535)
```

---

### String Validation

#### `nonEmpty :: Text -> Constraint`

Validates that a string is not empty and not null.

**Example:**
```haskell
let constraint = nonEmpty "username"
```

---

#### `matchesPattern :: Text -> String -> Constraint`

Validates that a string matches a regex pattern.

**Parameters:**
- Key: Configuration key name (for error messages)
- Pattern: Regular expression pattern

**Example:**
```haskell
let emailConstraint = matchesPattern "email" "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
let errors = emailConstraint (String "user@example.com")
```

---

## Key Path Syntax

### Dot Notation

Separate nested keys with dots:

```haskell
"database.host"
"server.ssl.certificate_path"
```

### Array Indexing

Access array elements with bracket notation:

```haskell
"database.servers[0]"
"servers[1].name"
"items[0].config[1].value"
```

### Combination

Mix both notations freely:

```haskell
"database.replication.servers[0].host"
```

---

## Type Conversion

Values are converted using Aeson's `FromJSON` instance. Common conversions:

```haskell
String "value"          -> String          (Text)
Number 42               -> Int, Double, Scientific
Bool True               -> Bool
Null                    -> Maybe a (becomes Nothing)
Array [...]             -> [a]
Object {...}            -> Custom types (via FromJSON)
```

---

## Environment Variable Overrides

When loading with a prefix:

```haskell
[parseConfig|"MY_APP"|config.yaml|]
```

Environment variables will override YAML values:
- `MY_APP_DATABASE_HOST` overrides `database.host`
- `MY_APP_SERVER_PORT` overrides `server.port`

---

## Error Handling

### Compile Time

Errors detected at compile-time include:
- Missing configuration file
- Invalid YAML/JSON syntax
- Configuration format issues

### Runtime

Errors at runtime include:
- Missing required keys
- Type conversion failures
- Validation constraint violations

---

## Performance Characteristics

- **Parsing Time**: O(1) - configuration is pre-parsed at compile time
- **Access Time**: O(log n) for key lookup
- **Memory**: O(n) where n is configuration size
- **Zero Runtime Overhead**: No parsing, only lookups

---

## Complete Example

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Configurator
import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Text (Text)

data DatabaseConfig = DatabaseConfig
  { dbHost :: String
  , dbPort :: Int
  } deriving (Show)

instance FromJSON DatabaseConfig where
  parseJSON = withObject "DatabaseConfig" $ \o ->
    DatabaseConfig
      <$> o .: "host"
      <*> o .: "port"

-- YAML file:
-- database:
--   host: "localhost"
--   port: 5432
-- servers:
--   - "server1.example.com"
--   - "server2.example.com"

[parseConfig|config.yaml|]

main :: IO ()
main = do
  -- Type-safe structured access
  let dbConfig = required "database" configMap :: DatabaseConfig
  putStrLn $ "Database: " ++ show dbConfig

  -- Optional with default
  let maxConnections = withDefault 100 "database.max_connections" configMap :: Int
  putStrLn $ "Max connections: " ++ show maxConnections

  -- Array access
  case getConfig "servers[0]" configMap of
    Just (String server) -> putStrLn $ "Primary server: " ++ show server
    _ -> putStrLn "Primary server not configured"

  -- Validation
  case validateValue "database.port" configMap of
    Just (Right port) -> putStrLn $ "Port is valid: " ++ show (port :: Int)
    Just (Left err) -> putStrLn $ "Invalid port: " ++ err
    Nothing -> putStrLn "Port not configured"

  -- Debug output
  putStrLn "\nFull configuration:"
  putStrLn (showConfig configMap)
```
