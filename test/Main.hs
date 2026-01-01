-- SPDX-License-Identifier: MIT
-- Copyright (C) 2026 CodWiz

{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Control.Exception (try, SomeException)
import qualified Data.Map.Strict as Map
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson (Value(..))

import Configurator.Internal
import Configurator.Validator

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

-- Tests for lookupValue
testLookupValueSimple :: Test
testLookupValueSimple = TestCase $ do
  let result = lookupValue "database.host" (Object (KeyMap.fromMapText testConfig))
  assertEqual "Should find simple nested value" (Just (String "localhost")) result

testLookupValueNested :: Test
testLookupValueNested = TestCase $ do
  let result = lookupValue "database.port" (Object (KeyMap.fromMapText testConfig))
  assertEqual "Should find numeric nested value" (Just (Number 5432)) result

testLookupValueNotFound :: Test
testLookupValueNotFound = TestCase $ do
  let result = lookupValue "nonexistent.key" (Object (KeyMap.fromMapText testConfig))
  assertEqual "Should return Nothing for missing key" Nothing result

-- Tests for required function
testRequiredString :: Test
testRequiredString = TestCase $ do
  let result = required "database.host" testConfig :: String
  assertEqual "Should extract string value" "localhost" result

testRequiredInt :: Test
testRequiredInt = TestCase $ do
  let result = required "database.port" testConfig :: Int
  assertEqual "Should extract numeric value" 5432 result

testRequiredMissing :: Test
testRequiredMissing = TestCase $ do
  result <- try $ evaluate (required "missing.key" testConfig :: String) :: IO (Either SomeException String)
  case result of
    Left _ -> return ()  -- Exception was thrown as expected
    Right _ -> assertFailure "Should throw error for missing key"

-- Tests for optional function
testOptionalExists :: Test
testOptionalExists = TestCase $ do
  let result = optional "log_level" testConfig :: Maybe String
  assertEqual "Should return Just when key exists" (Just "DEBUG") result

testOptionalMissing :: Test
testOptionalMissing = TestCase $ do
  let result = optional "missing.key" testConfig :: Maybe String
  assertEqual "Should return Nothing when key is missing" Nothing result

-- Tests for withDefault function
testWithDefaultExists :: Test
testWithDefaultExists = TestCase $ do
  let result = withDefault "INFO" "log_level" testConfig :: String
  assertEqual "Should return actual value when key exists" "DEBUG" result

testWithDefaultMissing :: Test
testWithDefaultMissing = TestCase $ do
  let result = withDefault "INFO" "missing.key" testConfig :: String
  assertEqual "Should return default value when key is missing" "INFO" result

-- Tests for getConfig function
testGetConfigExists :: Test
testGetConfigExists = TestCase $ do
  let result = getConfig "database.host" testConfig
  assertEqual "Should get raw value" (Just (String "localhost")) result

testGetConfigNotExists :: Test
testGetConfigNotExists = TestCase $ do
  let result = getConfig "nonexistent" testConfig
  assertEqual "Should return Nothing for missing key" Nothing result

-- Tests for keyExists function
testKeyExistsTrue :: Test
testKeyExistsTrue = TestCase $ do
  let result = keyExists "database.host" testConfig
  assertEqual "Should return True for existing key" True result

testKeyExistsFalse :: Test
testKeyExistsFalse = TestCase $ do
  let result = keyExists "nonexistent.key" testConfig
  assertEqual "Should return False for missing key" False result

-- Tests for nested access
testDeepNestedAccess :: Test
testDeepNestedAccess = TestCase $ do
  let result = getConfig "server.ssl.cert_path" nestedConfig
  assertEqual "Should access deeply nested values" (Just (String "/etc/ssl/cert.pem")) result

-- Tests for multiple values at same level
testMultipleLevelAccess :: Test
testMultipleLevelAccess = TestCase $ do
  let host = getConfig "server.host" nestedConfig
  let port = getConfig "server.port" nestedConfig
  assertEqual "Should get host" (Just (String "0.0.0.0")) host
  assertEqual "Should get port" (Just (Number 8080)) port

-- Tests for boolean values
testBooleanValue :: Test
testBooleanValue = TestCase $ do
  let result = optional "metrics.enabled" testConfig :: Maybe Bool
  assertEqual "Should extract boolean value" (Just True) result

-- Test for numeric comparisons
testNumericComparison :: Test
testNumericComparison = TestCase $ do
  let portVal = required "database.port" testConfig :: Int
  assertBool "Port should be 5432" (portVal == 5432)
  assertBool "Port should be > 5000" (portVal > 5000)
  assertBool "Port should be < 6000" (portVal < 6000)

-- Tests for data type preservation
testTypePreservation :: Test
testTypePreservation = TestCase $ do
  let strVal = required "app.name" nestedConfig :: String
  let numVal = required "server.port" nestedConfig :: Int
  let boolVal = required "server.ssl.enabled" nestedConfig :: Bool
  assertEqual "String type preserved" "MyApp" strVal
  assertEqual "Numeric type preserved" 8080 numVal
  assertEqual "Boolean type preserved" False boolVal
-- ============= VALIDATOR TESTS =============

-- | Tests for numeric constraints
testGreaterThanTrue :: Test
testGreaterThanTrue = TestCase $ do
  let constraint = "port" >. 1000
  let result = constraint (Number 5432)
  assertEqual "Should pass for greater value" [] result

testGreaterThanFalse :: Test
testGreaterThanFalse = TestCase $ do
  let constraint = "port" >. 5000
  let result = constraint (Number 1000)
  assertBool "Should fail for smaller value" (length result > 0)

testLessThanTrue :: Test
testLessThanTrue = TestCase $ do
  let constraint = "port" <. 10000
  let result = constraint (Number 5432)
  assertEqual "Should pass for smaller value" [] result

testLessThanFalse :: Test
testLessThanFalse = TestCase $ do
  let constraint = "port" <. 1000
  let result = constraint (Number 5432)
  assertBool "Should fail for greater value" (length result > 0)

testInRangeTrue :: Test
testInRangeTrue = TestCase $ do
  let constraint = inRange "port" (1000, 10000)
  let result = constraint (Number 5432)
  assertEqual "Should pass for value in range" [] result

testInRangeFalse :: Test
testInRangeFalse = TestCase $ do
  let constraint = inRange "port" (1000, 3000)
  let result = constraint (Number 5432)
  assertBool "Should fail for value out of range" (length result > 0)

testNonEmptyTrue :: Test
testNonEmptyTrue = TestCase $ do
  let constraint = nonEmpty "name"
  let result = constraint (String "admin")
  assertEqual "Should pass for non-empty string" [] result

testNonEmptyFalse :: Test
testNonEmptyFalse = TestCase $ do
  let constraint = nonEmpty "name"
  let result = constraint (String "")
  assertBool "Should fail for empty string" (length result > 0)

testPatternMatchTrue :: Test
testPatternMatchTrue = TestCase $ do
  let constraint = matchesPattern "email" "^[a-zA-Z0-9+_.-]+@[a-zA-Z0-9.-]+$"
  let result = constraint (String "user@example.com")
  assertEqual "Should pass for matching pattern" [] result

testPatternMatchFalse :: Test
testPatternMatchFalse = TestCase $ do
  let constraint = matchesPattern "email" "^[a-zA-Z0-9+_.-]+@[a-zA-Z0-9.-]+$"
  let result = constraint (String "invalid-email")
  assertBool "Should fail for non-matching pattern" (length result > 0)
-- Suite of all tests
allTests :: Test
allTests = TestList
  [ TestLabel "lookupValue_simple" testLookupValueSimple
  , TestLabel "lookupValue_nested" testLookupValueNested
  , TestLabel "lookupValue_notFound" testLookupValueNotFound
  , TestLabel "required_string" testRequiredString
  , TestLabel "required_int" testRequiredInt
  , TestLabel "required_missing" testRequiredMissing
  , TestLabel "optional_exists" testOptionalExists
  , TestLabel "optional_missing" testOptionalMissing
  , TestLabel "withDefault_exists" testWithDefaultExists
  , TestLabel "withDefault_missing" testWithDefaultMissing
  , TestLabel "getConfig_exists" testGetConfigExists
  , TestLabel "getConfig_notExists" testGetConfigNotExists
  , TestLabel "keyExists_true" testKeyExistsTrue
  , TestLabel "keyExists_false" testKeyExistsFalse
  , TestLabel "deepNestedAccess" testDeepNestedAccess
  , TestLabel "multipleLevelAccess" testMultipleLevelAccess
  , TestLabel "booleanValue" testBooleanValue
  , TestLabel "numericComparison" testNumericComparison
  , TestLabel "typePreservation" testTypePreservation
  , TestLabel "validator_greaterThan_true" testGreaterThanTrue
  , TestLabel "validator_greaterThan_false" testGreaterThanFalse
  , TestLabel "validator_lessThan_true" testLessThanTrue
  , TestLabel "validator_lessThan_false" testLessThanFalse
  , TestLabel "validator_inRange_true" testInRangeTrue
  , TestLabel "validator_inRange_false" testInRangeFalse
  , TestLabel "validator_nonEmpty_true" testNonEmptyTrue
  , TestLabel "validator_nonEmpty_false" testNonEmptyFalse
  , TestLabel "validator_pattern_true" testPatternMatchTrue
  , TestLabel "validator_pattern_false" testPatternMatchFalse
  ]

main :: IO ()
main = do
  putStrLn "============================================"
  putStrLn "  Running Configurator Unit Tests"
  putStrLn "============================================"
  putStrLn ""
  testResults <- runTestTT allTests
  putStrLn ""
  putStrLn "============================================"
  if failures testResults == 0 && errors testResults == 0
    then do
      putStrLn "✓ All tests passed!"
      putStrLn ("  Tests run: " ++ show (cases testResults))
    else do
      putStrLn "✗ Some tests failed!"
      putStrLn ("  Tests run: " ++ show (cases testResults))
      if errors testResults > 0
        then putStrLn $ "  Errors: " ++ show (errors testResults)
        else return ()
      if failures testResults > 0
        then putStrLn $ "  Failures: " ++ show (failures testResults)
        else return ()
  putStrLn "============================================"

-- Helper for strictness
evaluate :: a -> IO a
evaluate a = return a