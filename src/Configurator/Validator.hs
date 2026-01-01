-- SPDX-License-Identifier: MIT
-- Copyright (C) 2026 CodWiz

{-# LANGUAGE OverloadedStrings #-}

module Configurator.Validator
  ( ConfigValidator
  , Constraint
  , ValidationError
  , validate
  , (>.)
  , (<.)
  , (>=.)
  , (<=.)
  , inRange
  , nonEmpty
  , matchesPattern
  ) where

import           Data.Aeson (Value(..))
import qualified Data.Text as T
import           Data.Text (Text, unpack)
import           Data.Scientific (Scientific)
import           Text.Regex.TDFA ((=~))

-- | Type alias for validation errors
type ValidationError = String

-- | A constraint is a function that checks a Value and returns errors
type Constraint = Value -> [ValidationError]

-- | A validator collects multiple constraints
data ConfigValidator = ConfigValidator
  { validationName :: String
  , constraints :: [Constraint]
  }

-- | Run validation on a value
validate :: ConfigValidator -> Value -> Either [ValidationError] Value
validate validator val = 
  case constraints validator of
    [] -> Right val
    cs -> let errors = concatMap (\c -> c val) cs
          in if Prelude.null errors 
             then Right val 
             else Left errors

-- | Helper to create a constraint for numeric comparisons
numericConstraint :: String -> (Scientific -> Bool) -> Constraint
numericConstraint errMsg predicate val = 
  case val of
    Number n -> if predicate n then [] else [errMsg]
    _ -> ["Expected a number, got " ++ show val]

-- | Greater than constraint
(>.) :: Text -> Scientific -> Constraint
name >. limit = numericConstraint 
  ("Value at '" ++ unpack name ++ "' must be greater than " ++ show limit)
  (> limit)

-- | Less than constraint
(<.) :: Text -> Scientific -> Constraint
name <. limit = numericConstraint
  ("Value at '" ++ unpack name ++ "' must be less than " ++ show limit)
  (< limit)

-- | Greater than or equal constraint
(>=.) :: Text -> Scientific -> Constraint
name >=. limit = numericConstraint
  ("Value at '" ++ unpack name ++ "' must be >= " ++ show limit)
  (>= limit)

-- | Less than or equal constraint
(<=.) :: Text -> Scientific -> Constraint
name <=. limit = numericConstraint
  ("Value at '" ++ unpack name ++ "' must be <= " ++ show limit)
  (<= limit)

-- | Check if value is in range
inRange :: Text -> (Scientific, Scientific) -> Constraint
inRange name (minVal, maxVal) val =
  case val of
    Number n -> 
      if n >= minVal && n <= maxVal
        then []
        else ["Value at '" ++ unpack name ++ "' must be between " ++ show minVal ++ " and " ++ show maxVal]
    _ -> ["Expected a number, got " ++ show val]

-- | Check if text is not empty
nonEmpty :: Text -> Constraint
nonEmpty name val =
  case val of
    String t -> if T.null t then ["Value at '" ++ unpack name ++ "' cannot be empty"] else []
    Null     -> ["Value at '" ++ unpack name ++ "' cannot be null"]
    _        -> ["Expected a string at '" ++ unpack name ++ "'"]

-- | Check if text matches regex pattern
matchesPattern :: Text -> String -> Constraint
matchesPattern name pattern val =
  case val of
    String t -> 
      if unpack t =~ pattern
        then []
        else ["Value at '" ++ unpack name ++ "' does not match pattern '" ++ pattern ++ "'"]
    _ -> ["Expected a string at '" ++ unpack name ++ "'"]