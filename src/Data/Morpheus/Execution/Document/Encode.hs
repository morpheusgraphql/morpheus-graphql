{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.Encode
  ( deriveEncode
  ) where

import           Data.Text                             (unpack)
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Encode (Encode (..), ObjectResolvers (..))
import           Data.Morpheus.Types.Internal.Data     (DataField (..))
import           Data.Morpheus.Types.Internal.DataD    (ConsD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.TH       (instanceHeadMultiT, typeT)

deriveEncode :: TypeD -> Q [Dec]
deriveEncode TypeD {tName, tCons = [ConsD {cFields}]} = pure <$> instanceD (cxt []) appHead methods
  where
    appHead = instanceHeadMultiT ''ObjectResolvers (typeT (mkName tName) ["m"]) ["value"]
    methods = [funD 'objectResolvers [clause argsE (normalB body) []]]
      where
        argsE = [conP (mkName tName) $ map (varP . mkName) varNames]
        body = listE $ map decodeVar [] --varNames
        decodeVar name = [|(name, encode $(varName))|]
          where
            varName = varE $ mkName name
        varNames = map (unpack . fieldName) cFields
deriveEncode _ = pure []
