{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Document.Introspection
  ( deriveObjectRep
  ) where

import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Introspect (ObjectRep (..))
import           Data.Morpheus.Types.Internal.Data         (DataTypeKind (..))
import           Data.Morpheus.Types.Internal.DataD        (GQLTypeD (..), KindD (..), TypeD (..), unKindD)

deriveObjectRep :: GQLTypeD -> Q [Dec]
deriveObjectRep GQLTypeD {typeD = TypeD {tName}, typeKindD} =
  pure <$> instanceD (cxt []) (appT (conT ''ObjectRep) genHeadSig) methods
  where
    gqlKind = unKindD typeKindD
    withVar = gqlKind == KindObject || gqlKind == KindUnion
    isSubscription = typeKindD == SubscriptionD
    genHeadSig
      | isSubscription = appT (appT (conT $ mkName tName) (varT $ mkName "subscriptionM")) (varT $ mkName "m")
      | withVar = appT (conT $ mkName tName) (varT $ mkName "m")
      | otherwise = conT $ mkName tName
     -- objectFieldTypes :: Proxy rep -> [((Text, DataField t), TypeUpdater)]
    methods = [funD 'objectFieldTypes [clause [] (normalB [|[]|]) []]]
