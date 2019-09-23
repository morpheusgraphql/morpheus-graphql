{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.Encode
  ( deriveEncode
  ) where

import           Data.Text                               (unpack)
import           Data.Typeable                           (Typeable)
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Encode   (Encode (..), ObjectResolvers (..))
import           Data.Morpheus.Types.Internal.Data       (DataField (..))
import           Data.Morpheus.Types.Internal.DataD      (ConsD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.TH         (applyT, instanceHeadMultiT, typeT)
import           Data.Morpheus.Types.Internal.Validation (ResolveT)
import           Data.Morpheus.Types.Internal.Value      (GQLValue (..))
import           Data.Morpheus.Types.Resolver

deriveEncode :: TypeD -> Q [Dec]
deriveEncode TypeD {tName, tCons = [ConsD {cFields}]} = pure <$> instanceD (cxt constrains) appHead methods
  where
    resolveT = typeT ''ResolveT ["m", "value"]
    -- defines Type : ResolveT m value
    -------------------------------------------------
    -- defines Constraint: (Typeable m, Monad m, GQLValue (ResolveT m value), GQLValue value)
    constrains =
      [typeT ''Typeable ["m"], typeT ''Monad ["m"], typeT ''GQLValue ["value"], appT (conT ''GQLValue) resolveT]
    -------------------------------------------------------------------
    -- defines: instance <constraint> =>  ObjectResolvers (<Type> (ResolveT m)) (ResolveT m value) where
    appHead = instanceHeadMultiT ''ObjectResolvers (applyT (mkName tName) [typeT ''Resolver ["m"]]) [resolveT]
    ------------------------------------------------------------------
    -- defines: objectResolvers <Type field1 field2 ...> = [("field1",encode field1),("field2",encode field2), ...]
    methods = [funD 'objectResolvers [clause argsE (normalB body) []]]
      where
        argsE = [conP (mkName tName) $ map (varP . mkName) varNames]
        body = listE $ map decodeVar varNames
        decodeVar name = [|(name, encode $(varName))|]
          where
            varName = varE $ mkName name
        varNames = map (unpack . fieldName) cFields
deriveEncode _ = pure []
