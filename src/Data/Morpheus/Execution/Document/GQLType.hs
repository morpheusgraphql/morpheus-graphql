{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.GQLType
  ( deriveGQLType
  ) where

import           Language.Haskell.TH

import           Data.Morpheus.Kind                 (ENUM, INPUT_OBJECT, INPUT_UNION, OBJECT, SCALAR, UNION, WRAPPER)

--
-- MORPHEUS
import           Data.Morpheus.Types.GQLType        (GQLType (..))
import           Data.Morpheus.Types.Internal.Data  (DataTypeKind (..))
import           Data.Morpheus.Types.Internal.DataD (GQLTypeD (..), KindD (..), TypeD (..), unKindD)
import           Data.Morpheus.Types.Internal.TH    (applyT, headT)
import           Data.Typeable                      (Typeable)

deriveGQLType :: GQLTypeD -> Q [Dec]
deriveGQLType GQLTypeD {typeD = TypeD {tName}, typeKindD} =
  pure <$> instanceD (cxt constrains) (applyT ''GQLType [headSig]) [methods]
  where
    gqlKind = unKindD typeKindD
    withVar = gqlKind == KindObject || gqlKind == KindUnion
    isSubscription = typeKindD == SubscriptionD
    headVars
      | isSubscription = ["subscriptionM", "m"]
      | withVar = ["m"]
      | otherwise = []
    headSig = headT (mkName tName) headVars
    ----------
    constrains
      | isSubscription = map consTypeable ["subscriptionM", "m"]
      | withVar = [consTypeable "m"]
      | otherwise = []
    consTypeable = appT (conT ''Typeable) . (varT . mkName)
    ----
    methods = do
      typeN <- headSig
      pure $ TySynInstD ''KIND (TySynEqn [typeN] (ConT $ toKIND gqlKind))
    toKIND KindScalar      = ''SCALAR
    toKIND KindEnum        = ''ENUM
    toKIND KindObject      = ''OBJECT
    toKIND KindUnion       = ''UNION
    toKIND KindInputObject = ''INPUT_OBJECT
    toKIND KindList        = ''WRAPPER
    toKIND KindNonNull     = ''WRAPPER
    toKIND KindInputUnion  = ''INPUT_UNION
