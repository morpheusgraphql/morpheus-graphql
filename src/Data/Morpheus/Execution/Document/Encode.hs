{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.Encode
  ( deriveEncode
  ) where

import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Encode (ObjectResolvers (..))
import           Data.Morpheus.Types.Internal.DataD    (TypeD (..))
import           Data.Morpheus.Types.Internal.TH       (instanceHeadMultiT, typeT)

deriveEncode :: TypeD -> Q [Dec]
deriveEncode TypeD {tName, tCons = [cons]} = pure <$> instanceD (cxt []) appHead methods
  where
    appHead = instanceHeadMultiT ''ObjectResolvers (typeT (mkName tName) ["m"]) ["value"]
    methods = [funD 'objectResolvers [clause argsE (normalB body) []]]
      where
        argsE = map (varP . mkName) ["o"]
        body = [|[]|]
deriveEncode _ = pure []
