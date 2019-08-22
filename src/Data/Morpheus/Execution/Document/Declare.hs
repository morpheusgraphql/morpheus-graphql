{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Execution.Document.Declare
  ( declareTypes
  ) where

import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Internal.Declare (declareType)
import           Data.Morpheus.Types.Internal.DataD       (TypeD (..))

declareTypes :: [TypeD] -> [Dec]
declareTypes = map (declareType [])
