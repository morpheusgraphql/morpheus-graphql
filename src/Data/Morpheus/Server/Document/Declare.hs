{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Server.Document.Declare
  ( declare,
  )
where

-- MORPHEUS

import Data.Morpheus.Internal.TH
  ( Scope (..),
    declareType,
  )
import Data.Morpheus.Server.Document.Declare.Decode
  ( deriveDecode,
  )
import Data.Morpheus.Server.Document.Declare.Encode
  ( deriveEncode,
  )
import Data.Morpheus.Server.Document.Declare.GQLType
  ( deriveGQLType,
  )
import Data.Morpheus.Server.Document.Declare.Introspect
  ( deriveObjectRep,
    instanceIntrospect,
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLTypeD (..),
    TypeD (..),
    TypeKind (..),
    isInput,
    isObject,
  )
import Data.Semigroup ((<>))
import Language.Haskell.TH

class Declare a where
  type DeclareCtx a :: *
  declare :: DeclareCtx a -> a -> Q [Dec]

instance Declare a => Declare [a] where
  type DeclareCtx [a] = DeclareCtx a
  declare namespace = fmap concat . traverse (declare namespace)

instance Declare GQLTypeD where
  type DeclareCtx GQLTypeD = Bool
  declare namespace gqlType@GQLTypeD {typeD = typeD@TypeD {tKind}, typeArgD, typeOriginal} =
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
            | isObject tKind && isInput tKind =
              [deriveObjectRep (typeD, Just typeOriginal, Nothing), deriveDecode typeD]
            | isObject tKind =
              [deriveObjectRep (typeD, Just typeOriginal, Just tKind), deriveEncode typeD]
            | otherwise =
              []
      --------------------------------------------------
      declareArgTypes = do
        introspectArgs <- concat <$> traverse deriveArgsRep typeArgD
        decodeArgs <- concat <$> traverse deriveDecode typeArgD
        return $ argsTypeDecs <> decodeArgs <> introspectArgs
        where
          deriveArgsRep args = deriveObjectRep (args, Nothing, Nothing)
          ----------------------------------------------------
          argsTypeDecs = map (declareType SERVER namespace Nothing []) typeArgD
      --------------------------------------------------
      declareMainType = declare (namespace, tKind) typeD

instance Declare TypeD where
  type DeclareCtx TypeD = (Bool, TypeKind)
  declare (_, KindScalar) _ = pure []
  declare (namespace, tKind) typeD =
    pure [declareType SERVER namespace (Just tKind) derivingClasses typeD]
    where
      derivingClasses
        | isInput tKind = [''Show]
        | otherwise = []
