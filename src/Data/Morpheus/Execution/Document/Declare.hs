{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Document.Declare
  ( declareTypes
  ) where

import           Control.Lens                                (declareLenses)
import           Data.Function                               ((&))
import           Data.Semigroup                              ((<>))
import           Language.Haskell.TH

import           Data.Morpheus.Execution.Document.Decode     (deriveDecode)

--
-- MORPHEUS
import           Data.Morpheus.Execution.Document.GQLType    (deriveGQLType)
import           Data.Morpheus.Execution.Document.Introspect (deriveObjectRep)
import           Data.Morpheus.Execution.Internal.Declare    (declareGQLT)
import           Data.Morpheus.Types.Internal.DataD          (GQLTypeD (..), isInput, isObject)

declareTypes :: [GQLTypeD] -> Q [Dec]
declareTypes = fmap concat . traverse declareGQLType

declareGQLType :: GQLTypeD -> Q [Dec]
declareGQLType gqlType@GQLTypeD {typeD, typeKindD, typeArgD} = do
  mainType <- declareMainType
  argTypes <- declareArgTypes
  gqlInstances <- deriveGQLInstances
  typeClasses <- deriveGQLType gqlType
  pure $ mainType <> typeClasses <> argTypes <> gqlInstances
  where
    deriveGQLInstances = concat <$> traverse (typeD &) gqlInstances
      where
        gqlInstances
          | isObject typeKindD && isInput typeKindD = [deriveObjectRep, deriveDecode]
          | isObject typeKindD = [deriveObjectRep]
          | otherwise = []
    --------------------------------------------------
    declareArgTypes = do
      introspectArgs <- concat <$> traverse deriveObjectRep typeArgD
      decodeArgs <- concat <$> traverse deriveDecode typeArgD
      lenses <- declareLenses (pure (map (declareGQLT False Nothing []) typeArgD))
      return $ decodeArgs <> introspectArgs <> lenses
    --------------------------------------------------
    declareMainType
      | isInput typeKindD = declareLenses declareT
      | otherwise = declareT
      where
        declareT = pure [declareGQLT True (Just typeKindD) derivingClasses typeD]
        derivingClasses
          | isInput typeKindD = [''Show]
          | otherwise = []
