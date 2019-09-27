{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.Encode
  ( deriveEncode
  ) where

import           Data.Text                                (unpack)
import           Data.Typeable                            (Typeable)
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Encode    (Encode (..), ObjectResolvers (..))
import           Data.Morpheus.Types.GQLType              (TRUE)
import           Data.Morpheus.Types.Internal.Data        (DataField (..), isSubscription)
import           Data.Morpheus.Types.Internal.DataD       (ConsD (..), GQLTypeD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.TH          (applyT, instanceHeadMultiT, typeT)
import           Data.Morpheus.Types.Internal.Validation  (ResolveT)
import           Data.Morpheus.Types.Internal.Value       (GQLValue (..))
import           Data.Morpheus.Types.Resolver

--(ObjectResolvers TRUE (Subscription (IOSubRes EVENT ()) IORes) (ResolveT (SubscribeStream IO EVENT) (Event EVENT ()) -> ResolveT IO Value))
deriveEncode :: GQLTypeD -> Q [Dec]
-- TODO: derive subscription
deriveEncode GQLTypeD{ typeKindD } | isSubscription typeKindD = pure []
deriveEncode GQLTypeD {typeKindD, typeD = TypeD {tName, tCons = [ConsD {cFields}]}} =
  pure <$> instanceD (cxt constrains) appHead methods
    -- (Event EVENT () -> ResolveT IO Value)
  where
    result
      | isSubscription typeKindD = varT $ mkName "value"
      | otherwise = typeT ''ResolveT ["m", "value"]
    -- (ResolveT (SubscribeStream IO EVENT)
    mainType
      | isSubscription typeKindD = applyT (mkName tName) [typeT ''Resolver ["subM"], typeT ''Resolver ["m"]]
      | otherwise = applyT (mkName tName) [typeT ''Resolver ["m"]]
    -- defines Type : ResolveT m value
    -------------------------------------------------
    -- defines Constraint: (Typeable m, Monad m, GQLValue (ResolveT m value), GQLValue value)
    constrains =
      [typeT ''Typeable ["m"], typeT ''Monad ["m"], typeT ''GQLValue ["value"], appT (conT ''GQLValue) result]
    -------------------------------------------------------------------
    -- defines: instance <constraint> =>  ObjectResolvers ('TRUE) (<Type> (ResolveT m)) (ResolveT m value) where
    appHead = instanceHeadMultiT ''ObjectResolvers (conT ''TRUE) [mainType, result]
    ------------------------------------------------------------------
    -- defines: objectResolvers <Type field1 field2 ...> = [("field1",encode field1),("field2",encode field2), ...]
    methods = [funD 'objectResolvers [clause argsE (normalB body) []]]
      where
        argsE = [varP (mkName "_"), conP (mkName tName) (map (varP . mkName) varNames)]
        body = listE $ map decodeVar varNames
        decodeVar name = [|(name, encode $(varName))|]
          where
            varName = varE $ mkName name
        varNames = map (unpack . fieldName) cFields
deriveEncode _ = pure []
