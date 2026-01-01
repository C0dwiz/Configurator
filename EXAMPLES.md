# Configurator Examples

This directory contains practical examples of using the Configurator library.

## Basic Example

See [app/Main.hs](../app/Main.hs) for a complete working example.

### Configuration File (`config.yaml`)

```yaml
database:
  host: "localhost"
  port: 5432
  user: "admin"
  password: "supersecretpassword"
  servers:
    - "db1.example.com"
    - "db2.example.com"
    - "db3.example.com"

log_level: "DEBUG"

metrics:
  enabled: true
  interval_seconds: 30
  endpoints:
    - "http://metrics.example.com:8080"
    - "http://backup.example.com:8080"
```

### Haskell Code

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Configurator
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import Data.Text (Text, unpack)

-- Define your configuration structures
data DbConfig = DbConfig
  { dbHost :: Text
  , dbPort :: Int
  , dbUser :: Text
  , dbPass :: Maybe Text
  }

instance FromJSON DbConfig where
  parseJSON = withObject "DbConfig" $ \o ->
    DbConfig
      <$> o .: "host"
      <*> o .: "port"
      <*> o .: "user"
      <*> o .:? "password"

-- Load configuration at compile time
[parseConfig|app/config.yaml|]

main :: IO ()
main = do
  -- Type-safe structured access
  let dbConfig = required "database" configMap :: DbConfig
  putStrLn $ "Host: " ++ unpack (dbHost dbConfig)
  putStrLn $ "Port: " ++ show (dbPort dbConfig)

  -- Optional values
  let password = optional "database.password" configMap :: Maybe Text
  putStrLn $ "Password set: " ++ show (password /= Nothing)

  -- Default values
  let logLevel = withDefault "INFO" "log_level" configMap :: Text
  putStrLn $ "Log level: " ++ unpack logLevel

  -- Array access
  case getConfig "database.servers[0]" configMap of
    Just (String server) -> putStrLn $ "Primary DB: " ++ unpack server
    _ -> putStrLn "No primary DB configured"

  -- Debug the entire config
  putStrLn "\n" ++ showConfig configMap
```

---

## Web Server Configuration Example

### Configuration (`web.yaml`)

```yaml
server:
  name: "MyWebApp"
  host: "0.0.0.0"
  port: 8080
  ssl:
    enabled: true
    cert_path: "/etc/ssl/certs/server.crt"
    key_path: "/etc/ssl/private/server.key"
  cors:
    enabled: true
    origins:
      - "https://example.com"
      - "https://app.example.com"
      - "https://admin.example.com"
  rate_limiting:
    enabled: true
    requests_per_second: 100

database:
  primary:
    host: "db1.internal"
    port: 5432
    user: "webapp_user"
    password: "secure_password"
  replicas:
    - "db2.internal"
    - "db3.internal"
    - "db4.internal"

caching:
  redis:
    host: "redis.internal"
    port: 6379
    ttl_seconds: 3600

logging:
  level: "INFO"
  format: "json"
  output: "stdout"
```

### Implementation

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Configurator
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import Data.Text (Text, unpack)
import qualified Data.Vector as V

data SSLConfig = SSLConfig
  { sslEnabled :: Bool
  , certPath :: Text
  , keyPath :: Text
  } deriving (Show)

instance FromJSON SSLConfig where
  parseJSON = withObject "SSLConfig" $ \o ->
    SSLConfig
      <$> o .: "enabled"
      <*> o .: "cert_path"
      <*> o .: "key_path"

data ServerConfig = ServerConfig
  { serverName :: Text
  , serverHost :: String
  , serverPort :: Int
  , serverSSL :: SSLConfig
  } deriving (Show)

instance FromJSON ServerConfig where
  parseJSON = withObject "ServerConfig" $ \o ->
    ServerConfig
      <$> o .: "name"
      <*> o .: "host"
      <*> o .: "port"
      <*> o .: "ssl"

[parseConfig|web.yaml|]

main :: IO ()
main = do
  -- Load server configuration
  let serverCfg = required "server" configMap :: ServerConfig
  putStrLn $ "Server: " ++ unpack (serverName serverCfg)
  putStrLn $ "Listen: " ++ serverHost serverCfg ++ ":" ++ show (serverPort serverCfg)

  -- Check SSL configuration
  if sslEnabled (serverSSL serverCfg)
    then do
      putStrLn "SSL: Enabled"
      putStrLn $ "  Cert: " ++ unpack (certPath (serverSSL serverCfg))
    else putStrLn "SSL: Disabled"

  -- Rate limiting
  let rateLimitEnabled = withDefault False "server.rate_limiting.enabled" configMap :: Bool
  let rps = withDefault 50 "server.rate_limiting.requests_per_second" configMap :: Int
  putStrLn $ "Rate limiting: " ++ show rateLimitEnabled ++ " (" ++ show rps ++ " req/s)"

  -- Database replicas
  putStrLn "\nDatabase replicas:"
  case getConfig "database.replicas" configMap of
    Just (Array arr) -> mapM_ printReplica (zip [0..] (V.toList arr))
    _ -> putStrLn "No replicas configured"

  -- Redis configuration
  let redisHost = withDefault "localhost" "caching.redis.host" configMap :: String
  let redisPort = withDefault 6379 "caching.redis.port" configMap :: Int
  putStrLn $ "\nRedis: " ++ redisHost ++ ":" ++ show redisPort

  putStrLn "\n=== Full Configuration ==="
  putStrLn (showConfig configMap)

printReplica :: (Int, Value) -> IO ()
printReplica (i, String host) = putStrLn $ "  [" ++ show i ++ "] " ++ unpack host
printReplica (i, _) = putStrLn $ "  [" ++ show i ++ "] <invalid>"
```

---

## Environment Variable Overrides

### Configuration with Prefix

```haskell
[parseConfig|"MYAPP"|config.yaml|]
```

Environment variables will override YAML:

```bash
export MYAPP_SERVER_PORT=9000
export MYAPP_DATABASE_PRIMARY_HOST=prod-db.example.com
export MYAPP_LOGGING_LEVEL=DEBUG

./myapp  # Will use overridden values
```

---

## Validation Example

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Configurator
import qualified Data.Aeson as Aeson

[parseConfig|config.yaml|]

main :: IO ()
main = do
  -- Validate numeric ranges
  case validateValue "database.port" configMap of
    Just (Right port) -> 
      if (port :: Int) > 0 && port < 65536
        then putStrLn "✓ Port is valid"
        else putStrLn "✗ Port out of range"
    Just (Left err) -> putStrLn $ "✗ " ++ err
    Nothing -> putStrLn "✗ Port not configured"

  -- Validate required strings
  case getConfig "database.user" configMap of
    Just (String user) -> 
      if not (null user)
        then putStrLn $ "✓ Database user: " ++ show user
        else putStrLn "✗ Database user is empty"
    _ -> putStrLn "✗ Database user not configured"

  -- Check enabled features
  let metricsEnabled = withDefault False "metrics.enabled" configMap :: Bool
  putStrLn $ "Metrics: " ++ if metricsEnabled then "Enabled" else "Disabled"
```

---

## JSON Configuration Example

Configurator automatically detects file format:

### Configuration (`config.json`)

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "credentials": {
      "user": "admin",
      "password": "secret"
    }
  },
  "features": {
    "caching": true,
    "analytics": false
  }
}
```

### Usage

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Configurator

-- Works exactly the same as YAML!
[parseConfig|config.json|]

main :: IO ()
main = do
  let dbHost = required "database.host" configMap :: String
  let caching = withDefault False "features.caching" configMap :: Bool
  putStrLn $ "DB: " ++ dbHost
  putStrLn $ "Caching: " ++ show caching
```

---

## Running Examples

```bash
# Build the project
cabal build

# Run the example
cabal run example-usage

# Run tests
cabal test
```

---

## Tips and Best Practices

### 1. Use Custom Types

```haskell
-- Good: Type-safe access
data DbConfig = DbConfig { host :: String, port :: Int }
instance FromJSON DbConfig where ...
let config = required "database" configMap :: DbConfig

-- Avoid: Loose typing
let host = required "database.host" configMap :: String
let port = required "database.port" configMap :: Int
```

### 2. Validate Early

```haskell
-- Validate at startup
case validateValue "database.port" configMap of
  Just (Right _) -> startApp
  _ -> exitFailure
```

### 3. Use showConfig for Debugging

```haskell
-- In development
when isDebugMode $ putStrLn (showConfig configMap)
```

### 4. Environment Overrides

```haskell
-- Support environment overrides
[parseConfig|"MYAPP"|config.yaml|]
```

### 5. Default Sensible Values

```haskell
-- Use withDefault for optional settings
let timeout = withDefault 30 "request.timeout_seconds" configMap :: Int
```
