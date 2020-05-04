{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.Document.Declare
  ( declareTypes,
  )
where

-- MORPHEUS

import Data.Morpheus.Internal.Declare
  ( Scope (..),
    declareType,
  )
import Data.Morpheus.Server.Document.Decode
  ( deriveDecode,
  )
import Data.Morpheus.Server.Document.Encode
  ( deriveEncode,
  )
import Data.Morpheus.Server.Document.GQLType
  ( deriveGQLType,
  )
import Data.Morpheus.Server.Document.Introspect
  ( deriveObjectRep,
    instanceIntrospect,
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLTypeD (..),
    isInput,
    isObject,
  )
import Data.Semigroup ((<>))
import Language.Haskell.TH

declareTypes :: Bool -> [GQLTypeD] -> Q [Dec]
declareTypes namespace = fmap concat . traverse (declareGQLType namespace)

declareGQLType :: Bool -> GQLTypeD -> Q [Dec]
declareGQLType namespace gqlType@GQLTypeD {typeD, typeKindD, typeArgD, typeOriginal} =
  do
    mainType <- declareMainType
    argTypes <- declareArgTypes
    gqlInstances <- deriveGQLInstances
    typeClasses <- deriveGQLType gqlType
    introspectEnum <- instanceIntrospect typeOriginal
    pure $ mainType <> typeClasses <> argTypes <> gqlInstances <> introspectEnum
  where
    deriveGQLInstances = concat <$> sequence gqlInstances
      where
        gqlInstances
          | isObject typeKindD && isInput typeKindD =
            [deriveObjectRep (typeD, Nothing), deriveDecode typeD]
          | isObject typeKindD =
            [deriveObjectRep (typeD, Just typeKindD), deriveEncode gqlType]
          | otherwise =
            []
    --------------------------------------------------
    declareArgTypes = do
      introspectArgs <- concat <$> traverse deriveArgsRep typeArgD
      decodeArgs <- concat <$> traverse deriveDecode typeArgD
      return $ argsTypeDecs <> decodeArgs <> introspectArgs
      where
        deriveArgsRep args = deriveObjectRep (args, Nothing)
        ----------------------------------------------------
        argsTypeDecs = map (declareType SERVER namespace Nothing []) typeArgD
    --------------------------------------------------
    declareMainType = declareT
      where
        declareT =
          pure [declareType SERVER namespace (Just typeKindD) derivingClasses typeD]
        derivingClasses
          | isInput typeKindD = [''Show]
          | otherwise = []
