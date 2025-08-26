# Configurator

[](https://www.google.com/search?q=https://github.com/your-username/Configurator/actions/workflows/haskell-cabal.yml)
[](https://www.google.com/search?q=https://hackage.haskell.org/package/Configurator)

**Configurator** is a type-safe configuration parsing library for Haskell. It guarantees that your application's required settings and their types are validated at **compile time**, eliminating an entire class of runtime errors. By leveraging Haskell's type system and Template Haskell, it ensures that your configuration schema is a first-class citizen in your code.

-----

### ✨ Features

  * **Compile-Time Type Safety**: Utilizes **Template Haskell** and **Quasi-Quoters** to read and validate configuration files before your application even runs. If a required field is missing or has an incorrect type, your code simply won't compile.
  * **YAML & JSON Support**: Seamlessly parses configuration files in the widely-used YAML and JSON formats.
  * **Intuitive API**: Provides a straightforward and expressive API with functions like `required`, `optional`, and `withDefault` for accessing your settings.
  * **Automatic Documentation**: (Future Feature) The library is designed to enable automatic generation of configuration documentation from your Haskell schema.

-----

### 📦 Installation

To get started, you can add `Configurator` to your project's dependencies. The easiest way is to use a modern build tool like **Cabal** or **Stack**.

#### **Using Cabal (Recommended)**

If you're using Cabal, you can add the Git repository to a `cabal.project` file in the root of your project.

1.  **Create a `cabal.project` file** if it doesn't exist, and add the following lines:

    ```
    packages: .
    ```

2.  **Add the `Configurator` Git repository** to your `cabal.project` file, specifying a `tag` or `commit` for stability.

    ```
    source-repository-package
        type: git
        location: https://github.com/C0dwiz/Configurator.git
        tag: v0.1.0.0
    ```

3.  **Add `Configurator` to your `build-depends`** in your project's main `.cabal` file.

    ```cabal
    library
      ...
      build-depends:
          base >= 4.7 && < 5
        , Configurator
        ...
    ```

4.  **Run `cabal build`** to automatically download the library and its dependencies from the Git repository.

#### **Using Stack**

If you prefer Stack, add the Git repository to your `stack.yaml` file under the `extra-deps` section.

1.  **Add the repository to `stack.yaml`**, specifying a `commit` for a stable build.

    ```yaml
    extra-deps:
    - git: https://github.com/C0dwiz/Configurator.git
      commit: <hash-commit>
    ```

2.  **Add `Configurator` to your `build-depends`** in your project's `.cabal` file, just as you would for any other dependency.

    ```cabal
    library
      ...
      build-depends:
          base
        , Configurator
        ...
    ```

3.  **Run `stack build`** to fetch and build the library directly from the Git repository.

### 🚀 Getting Started

Imagine you have a configuration file for a simple web service.

**`config.yaml`**

```yaml
server:
  host: "0.0.0.0"
  port: 8080

database:
  connection_string: "postgresql://user:password@localhost:5432/mydb"
  pool_size: 10
```

Here's how you can use **Configurator** to load and validate it in your Haskell code.

**`Main.hs`**

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import           Configurator (parseConfig, required, optional, withDefault)
import           Data.Text    (Text, unpack)
import           GHC.Generics (Generic)
import           Data.Aeson

-- Define your data types to represent the configuration schema
data ServerConfig = ServerConfig
  { serverHost :: Text
  , serverPort :: Int
  } deriving (Show, Generic)

-- Derive 'FromJSON' to enable automatic parsing from JSON/YAML
instance FromJSON ServerConfig where
  parseJSON = withObject "ServerConfig" $ \o ->
    ServerConfig
      <$> o .: "host"
      <*> o .: "port"

-- Load and validate the configuration file at compile time
[parseConfig|config.yaml|]

main :: IO ()
main = do
  -- Use 'required' for a field that must exist. If "server" is missing, compilation will fail.
  let serverConfig = required "server" configMap :: ServerConfig
  putStrLn $ "Server Host: " ++ unpack (serverHost serverConfig)
  putStrLn $ "Server Port: " ++ show (serverPort serverConfig)

  -- Use 'withDefault' for optional fields with a fallback value.
  let poolSize = withDefault 5 "database.pool_size" configMap :: Int
  putStrLn $ "DB Pool Size: " ++ show poolSize

  -- Use 'optional' for a field that may or may not exist.
  let connectionString = optional "database.connection_string" configMap :: Maybe Text
  case connectionString of
    Just connStr -> putStrLn $ "DB Connection String: " ++ unpack connStr
    Nothing      -> putStrLn "DB Connection String: Not found"
```

-----

### 📚 API Reference

  * `parseConfig :: QuasiQuoter`: The core quasi-quoter for loading and validating a configuration file path at compile time.
  * `required :: (FromJSON a) => Text -> Config -> a`: Access a required configuration value. If the key is not found or parsing fails, this will throw an error at runtime.
  * `optional :: (FromJSON a) => Text -> Config -> Maybe a`: Access an optional value, returning `Just a` if found, or `Nothing` otherwise.
  * `withDefault :: (FromJSON a) => a -> Text -> Config -> a`: Access a value, providing a default value if the key is not found.

-----

### 📜 License

This project is licensed under the **MIT License**.