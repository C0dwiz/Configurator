{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Configurator.TH (mapE, toPairExp) where

import           Data.Aeson    (Value)
import qualified Data.Map.Strict as Map
import           Data.Text     (Text)
import           Language.Haskell.TH

-- | Convert a (key, value) pair to a TH expression of a tuple.
toPairExp :: (Text, Value) -> Q Exp
toPairExp (key, val) = [e| (key, val) |]

-- | Given a TH expression of a list of pairs, create a TH expression for a Map.
mapE :: Q Exp -> Q Exp
mapE listExp = [e| Map.fromList $listExp |]
