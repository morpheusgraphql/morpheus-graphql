{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Document.Declare
  ( declareTypes
  ) where

import           Data.Semigroup                              ((<>))
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Document.Decode     (deriveDecode)
import           Data.Morpheus.Execution.Document.Encode     (deriveEncode)
import           Data.Morpheus.Execution.Document.GQLType    (deriveGQLType)
import           Data.Morpheus.Execution.Document.Introspect (deriveObjectRep)
import           Data.Morpheus.Execution.Internal.Declare    (declareGQLT)
import           Data.Morpheus.Types.Internal.Data           (isInput, isObject)
import           Data.Morpheus.Types.Internal.DataD          (GQLTypeD (..))

declareTypes :: Bool -> [GQLTypeD] -> Q [Dec]
declareTypes namespace = fmap concat . traverse (declareGQLType namespace)

declareGQLType :: Bool -> GQLTypeD -> Q [Dec]
declareGQLType namespace gqlType@GQLTypeD {typeD, typeKindD, typeArgD} = do
  mainType <- declareMainType
  argTypes <- declareArgTypes
  gqlInstances <- deriveGQLInstances
  typeClasses <- deriveGQLType gqlType
  pure $ mainType <> typeClasses <> argTypes <> gqlInstances
  where
    deriveGQLInstances = concat <$> sequence gqlInstances
      where
        gqlInstances
          | isObject typeKindD && isInput typeKindD = [deriveObjectRep (typeD, Just typeKindD), deriveDecode typeD]
          | isObject typeKindD = [deriveObjectRep (typeD, Just typeKindD), deriveEncode typeD]
          | otherwise = []
    --------------------------------------------------
    declareArgTypes = do
      introspectArgs <- concat <$> traverse deriveArgsRep typeArgD
      decodeArgs <- concat <$> traverse deriveDecode typeArgD
      return $ argsTypeDecs <> decodeArgs <> introspectArgs
      where
        deriveArgsRep args = deriveObjectRep (args, Nothing)
        ----------------------------------------------------
        argsTypeDecs = map (declareGQLT namespace Nothing []) typeArgD
    --------------------------------------------------
    declareMainType
      | isInput typeKindD = declareT -- TODO: declareLenses
      | otherwise = declareT
      where
        declareT = pure [declareGQLT namespace (Just typeKindD) derivingClasses typeD]
        derivingClasses
          | isInput typeKindD = [''Show]
          | otherwise = []
