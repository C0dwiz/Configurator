-- SPDX-License-Identifier: MIT
-- Copyright (C) 2026 CodWiz

{-# LANGUAGE OverloadedStrings #-}

module ValidatorTests where

import Test.HUnit
import Data.Aeson (Value(..))
import Data.Scientific (fromFloatDigits)
import Configurator.Validator

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

testGreaterOrEqualTrue :: Test
testGreaterOrEqualTrue = TestCase $ do
  let constraint = "port" >=. 5432
  let result = constraint (Number 5432)
  assertEqual "Should pass for equal value" [] result

testGreaterOrEqualFalse :: Test
testGreaterOrEqualFalse = TestCase $ do
  let constraint = "port" >=. 6000
  let result = constraint (Number 5432)
  assertBool "Should fail for smaller value" (length result > 0)

-- | Tests for range constraints
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

testInRangeEdge :: Test
testInRangeEdge = TestCase $ do
  let constraint1 = inRange "port" (5432, 10000)
  let constraint2 = inRange "port" (1000, 5432)
  assertEqual "Should pass for lower edge" [] (constraint1 (Number 5432))
  assertEqual "Should pass for upper edge" [] (constraint2 (Number 5432))

-- | Tests for string constraints
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

testNonEmptyNull :: Test
testNonEmptyNull = TestCase $ do
  let constraint = nonEmpty "name"
  let result = constraint Null
  assertBool "Should fail for null value" (length result > 0)

-- | Tests for pattern matching
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

testPatternNumberFormat :: Test
testPatternNumberFormat = TestCase $ do
  let constraint = matchesPattern "version" "^[0-9]+\\.[0-9]+\\.[0-9]+$"
  let result = constraint (String "1.2.3")
  assertEqual "Should pass for version format" [] result

-- | Tests for type validation
testConstraintWrongType :: Test
testConstraintWrongType = TestCase $ do
  let constraint = "port" >. 1000
  let result = constraint (String "not a number")
  assertBool "Should fail for wrong type" (length result > 0)

-- | Combined test suite
validatorTests :: Test
validatorTests = TestList
  [ TestLabel "greaterThan_true" testGreaterThanTrue
  , TestLabel "greaterThan_false" testGreaterThanFalse
  , TestLabel "lessThan_true" testLessThanTrue
  , TestLabel "lessThan_false" testLessThanFalse
  , TestLabel "greaterOrEqual_true" testGreaterOrEqualTrue
  , TestLabel "greaterOrEqual_false" testGreaterOrEqualFalse
  , TestLabel "inRange_true" testInRangeTrue
  , TestLabel "inRange_false" testInRangeFalse
  , TestLabel "inRange_edge" testInRangeEdge
  , TestLabel "nonEmpty_true" testNonEmptyTrue
  , TestLabel "nonEmpty_false" testNonEmptyFalse
  , TestLabel "nonEmpty_null" testNonEmptyNull
  , TestLabel "pattern_true" testPatternMatchTrue
  , TestLabel "pattern_false" testPatternMatchFalse
  , TestLabel "pattern_version" testPatternNumberFormat
  , TestLabel "wrongType" testConstraintWrongType
  ]
