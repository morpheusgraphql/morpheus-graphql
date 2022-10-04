{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Data.Morpheus.Server.Deriving.Schema.TypeContent
  ( buildTypeContent,
    insertTypeContent,
    deriveTypeContentWith,
  )
where

import Data.Morpheus.Server.Deriving.Schema.Directive (deriveTypeDirectives, visitTypeDescription)
import Data.Morpheus.Server.Deriving.Schema.Enum
  ( buildEnumTypeContent,
  )
import Data.Morpheus.Server.Deriving.Schema.Internal
  ( KindedType (..),
    TyContent,
  )
import Data.Morpheus.Server.Deriving.Schema.Object
  ( buildObjectTypeContent,
  )
import Data.Morpheus.Server.Deriving.Schema.Union (buildUnionTypeContent)
import Data.Morpheus.Server.Deriving.Utils
  ( ConsRep (..),
    DeriveTypeOptions,
    DeriveWith,
    deriveTypeWith,
    isEmptyConstraint,
    unpackMonad,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CategoryValue (..),
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    deriveFingerprint,
    deriveTypename,
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    updateSchema,
  )
import Data.Morpheus.Types.Internal.AST
import GHC.Generics (Rep)

buildTypeContent ::
  (GQLType a, CategoryValue kind) =>
  KindedType kind a ->
  [ConsRep (TyContent kind)] ->
  SchemaT kind (TypeContent TRUE kind CONST)
buildTypeContent scope cons | all isEmptyConstraint cons = buildEnumTypeContent scope (consName <$> cons)
buildTypeContent scope [ConsRep {consFields}] = buildObjectTypeContent scope consFields
buildTypeContent scope cons = buildUnionTypeContent scope cons

insertTypeContent ::
  (GQLType a, CategoryValue kind) =>
  (f kind a -> SchemaT c (TypeContent TRUE kind CONST)) ->
  f kind a ->
  SchemaT c ()
insertTypeContent f proxy =
  updateSchema
    (deriveFingerprint proxy)
    deriveD
    proxy
  where
    deriveD x = do
      content <- f x
      dirs <- deriveTypeDirectives proxy
      pure $
        TypeDefinition
          (visitTypeDescription proxy $ description proxy)
          (deriveTypename proxy)
          dirs
          content

deriveTypeContentWith ::
  ( CategoryValue kind,
    DeriveWith c (SchemaT kind (TyContent kind)) (Rep a),
    GQLType a
  ) =>
  DeriveTypeOptions kind c (SchemaT kind (TyContent kind)) ->
  KindedType kind a ->
  SchemaT kind (TypeContent TRUE kind CONST)
deriveTypeContentWith x kindedProxy =
  unpackMonad
    ( deriveTypeWith x kindedProxy
    )
    >>= buildTypeContent kindedProxy
