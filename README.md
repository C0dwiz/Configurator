# Configurator

[![Haskell CI](https://github.com/C0dwiz/Configurator/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/C0dwiz/Configurator/actions/workflows/haskell-ci.yml)

**Configurator** is a type-safe configuration parsing library for Haskell. It guarantees that your application's required settings and their types are validated at **compile time**, eliminating an entire class of runtime errors. By leveraging Haskell's type system and Template Haskell, it ensures that your configuration schema is a first-class citizen in your code.

-----

### ✨ Features

  * **Compile-Time Type Safety**: Utilizes **Template Haskell** and **Quasi-Quoters** to read and validate configuration files before your application even runs. If a required field is missing or has an incorrect type, your code simply won't compile.
  * **YAML & JSON Support**: Seamlessly parses configuration files in the widely-used YAML and JSON formats. Format is auto-detected by file extension.
  * **Array & Nested Access**: Access array elements with intuitive syntax like `"database.servers[0]"` and deeply nested values.
  * **Runtime Validation**: Provides constraint-based validation with operators like `>`, `<`, pattern matching, and custom validators.
  * **Debug Support**: Use `showConfig` to pretty-print the entire configuration for debugging purposes.
  * **Intuitive API**: Provides a straightforward API with functions like `required`, `optional`, and `withDefault` for accessing your settings.

-----

### 📦 Installation

To get started, you can add `Configurator` to your project's dependencies.

#### **Using Cabal**

1. Add the `Configurator` Git repository to your `cabal.project` file:

    ```
    source-repository-package
        type: git
        location: https://github.com/C0dwiz/Configurator.git
        tag: v0.2.0.0
    ```

2. Add `Configurator` to your `build-depends`:

    ```cabal
    library
      ...
      build-depends:
          base >= 4.7 && < 5
        , Configurator
        ...
    ```

3. Run `cabal build`

#### **Using Stack**

Add to your `stack.yaml`:

```yaml
extra-deps:
  - git: https://github.com/C0dwiz/Configurator.git
    commit: <hash>
```

-----

### 🚀 Quick Start

**`config.yaml`**

```yaml
server:
  host: "0.0.0.0"
  port: 8080
  cors:
    enabled: true
    origins:
      - "https://example.com"
      - "https://app.example.com"

database:
  host: "localhost"
  port: 5432
  user: "admin"
  password: "secret"
  pool_size: 10

log_level: "DEBUG"
```

**`Main.hs`**

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Configurator
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import Data.Text (Text)

data ServerConfig = ServerConfig
  { host :: Text
  , port :: Int
  } deriving (Show)

instance FromJSON ServerConfig where
  parseJSON = withObject "ServerConfig" $ \o ->
    ServerConfig <$> o .: "host" <*> o .: "port"

-- Load config at compile time
[parseConfig|config.yaml|]

main :: IO ()
main = do
  -- Type-safe required values
  let server = required "server" configMap :: ServerConfig
  putStrLn $ "Server: " ++ show server

  -- Optional values with fallback
  let logLevel = withDefault "INFO" "log_level" configMap :: Text
  putStrLn $ "Log Level: " ++ show logLevel

  -- Access array elements
  case getConfig "server.cors.origins[0]" configMap of
    Just val -> putStrLn $ "CORS Origin 1: " ++ show val
    Nothing  -> putStrLn "CORS Origin 1: Not found"

  -- Debug: show all config
  putStrLn (showConfig configMap)
```

-----

### 📖 Core API

#### Access Functions

```haskell
-- Load configuration from file (auto-detects YAML/JSON)
[parseConfig|config.yaml|]

-- Get a required value (fails at runtime if missing/invalid)
required "database.host" configMap :: String

-- Get an optional value (returns Nothing if missing)
optional "database.password" configMap :: Maybe String

-- Get a value with a default fallback
withDefault 5432 "database.port" configMap :: Int

-- Get raw Value without type conversion
getConfig "database.host" configMap :: Maybe Value

-- Check if key exists
keyExists "database.host" configMap :: Bool

-- Print configuration for debugging
showConfig configMap :: String
```

#### Validation

```haskell
import Configurator (Constraint, (>.), (<.), inRange, nonEmpty, matchesPattern)

-- Use constraints to validate values
let portConstraint = "port" >. 1024  -- Port must be > 1024
let result = portConstraint (Number 8080)  -- Returns [] if valid, [error] if not

-- Available validators:
-- (>.) , (<.) , (>=.) , (<=.)  -- Numeric comparisons
-- inRange "key" (min, max)       -- Range validation
-- nonEmpty "key"                 -- Non-empty string check
-- matchesPattern "key" "regex"   -- Regex pattern matching
```

#### Environment Variable Override

Environment variables can override config values with a prefix:

```haskell
-- Load with ENV prefix
[parseConfig|"ENV"|config.yaml|]

-- Now ENV_DATABASE_HOST, ENV_DATABASE_PORT, etc. will override YAML values
```

-----

### 🧪 Testing

The library includes comprehensive unit tests:

```bash
cabal test
```

Tests cover:
- Basic value access (`required`, `optional`, `withDefault`)
- Nested and array access
- Type preservation
- Validation constraints
- File format detection

-----

### 🔧 Advanced Usage

#### Array Access

```haskell
-- Access array elements by index
getConfig "database.servers[0]" configMap
getConfig "database.servers[1]" configMap

-- Works with nested paths too
getConfig "servers[0].name" configMap
```

#### Multiple File Formats

```haskell
-- YAML (default for .yaml, .yml)
[parseConfig|config.yaml|]

-- JSON (auto-detected for .json)
[parseConfig|config.json|]

-- Auto-detection by extension
[parseConfig|settings.yaml|]
[parseConfig|settings.json|]
```

#### Validation Example

```haskell
import Configurator (validateValue)

case validateValue "database.port" configMap of
  Just (Right port) -> putStrLn $ "Port: " ++ show (port :: Int)
  Just (Left err)   -> putStrLn $ "Error: " ++ err
  Nothing           -> putStrLn "Key not found"
```

-----

### 📋 Performance

- Configuration files are parsed at **compile time**
- Zero runtime overhead for parsing
- Lazy evaluation of nested values
- Minimal memory footprint

-----

### 🤝 Contributing

Contributions are welcome! Please ensure:
- All tests pass: `cabal test`
- Code is formatted: `fourmolu`
- No hlint warnings: `hlint src`

-----

### 📜 License

This project is licensed under the **MIT License**. See [LICENSE](LICENSE) file for details.

