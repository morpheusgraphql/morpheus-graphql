{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Data.Morpheus.Server.Deriving.Schema.TypeContent
  ( buildTypeContent,
  )
where

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
import Data.Morpheus.Server.Types.GQLType (GQLType)
import Data.Morpheus.Server.Types.SchemaT (SchemaT)
import Data.Morpheus.Types.Internal.AST

buildTypeContent ::
  (GQLType a, CategoryValue kind) =>
  KindedType kind a ->
  [ConsRep (TyContent kind)] ->
  SchemaT kind (TypeContent TRUE kind CONST)
buildTypeContent scope cons | all isEmptyConstraint cons = buildEnumTypeContent scope (consName <$> cons)
buildTypeContent scope [ConsRep {consFields}] = buildObjectTypeContent scope consFields
buildTypeContent scope cons = buildUnionTypeContent scope cons
