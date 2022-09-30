{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Data.Morpheus.Server.Deriving.Schema.TypeContent
  ( buildTypeContent,
    insertTypeContent,
  )
where

import Data.Morpheus.Server.Deriving.Schema.Directive (deriveTypeDirectives)
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
    isEmptyConstraint,
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
          (description proxy)
          (deriveTypename proxy)
          dirs
          content
