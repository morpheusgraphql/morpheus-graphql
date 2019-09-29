{-# LANGUAGE DataKinds         #-}
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
import           Data.Morpheus.Types.GQLType             (TRUE)
import           Data.Morpheus.Types.Internal.Data       (DataField (..), isSubscription)
import           Data.Morpheus.Types.Internal.DataD      (ConsD (..), GQLTypeD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.TH         (applyT, instanceHeadMultiT, typeT)
import           Data.Morpheus.Types.Internal.Validation (ResolveT)
import           Data.Morpheus.Types.Internal.Value      (GQLValue (..), Value)
import           Data.Morpheus.Types.Resolver

-- @Subscription:
--
--     instance (Monad m, Typeable m) => ObjectResolvers 'True (Subscription (SubResolver m e c)) (SubResolveT m e c Value) where
--
--          objectResolvers _ (Subscription x y) = [("newAddress", encode x), ("newUser", encode y)]
--
-- @Object:
--
--
--
--
--
deriveEncode :: GQLTypeD -> Q [Dec]
deriveEncode GQLTypeD {typeKindD, typeD = TypeD {tName, tCons = [ConsD {cFields}]}} =
  pure <$> instanceD (cxt constrains) appHead methods
  where
    result
      | isSubscription typeKindD = applyT ''SubResolveT $ map (varT . mkName) ["m", "e", "c"] <> [conT ''Value] -- (SubResolveT m e c Value)
      | otherwise = typeT ''ResolveT ["m", "value"] -- (ResolveT (SubscribeStream IO EVENT)
    mainType = applyT (mkName tName) [mainTypeArg] -- defines  (<Type> (SubResolver m e c)) or (<Type> (Resolver m))
      where
        mainTypeArg
          | isSubscription typeKindD = typeT ''SubResolver ["m", "e", "c"] -- (SubResolver m e c)
          | otherwise = typeT ''Resolver ["m"] -- (Resolver m)
    -----------------------------------------------------------------------------------------
    -- defines Constraint: (Typeable m, Monad m, GQLValue (ResolveT m value), GQLValue value)
    constrains
      | isSubscription typeKindD = baseConstrains
      | otherwise = baseConstrains <> [typeT ''GQLValue ["value"], appT (conT ''GQLValue) result]
      where
        baseConstrains = [typeT ''Monad ["m"], typeT ''Typeable ["m"]]
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
