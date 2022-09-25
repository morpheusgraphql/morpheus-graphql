{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Visitors
  ( ASTVisitor (..),
    SchemaVisitors (..),
    VisitorFieldDefinition (..),
    VisitorTypeDefinition (..),
    defaultSchemaVisitors,
    VisitorDefinition (..),
  )
where

import Data.Morpheus.App.Internal.Stitching (Stitching (..))
import Data.Morpheus.Internal.Ext
import Data.Morpheus.Types.Internal.AST
import Relude

-- data VisitorObject = VisitorObject
--   { visitorObjectName :: TypeName,
--     visitorObjectImplements :: [TypeName],
--     visitorObjectFields :: FieldsDefinition OUT VALID
--   }

-- data VisitorInputObject = VisitorInputObject
--   { visitorInputObjectName :: TypeName,
--     visitorInputObjectFields :: FieldsDefinition IN VALID
--   }

-- newtype VisitorUnion = VisitorUnion
--   { visitorUnionMembers :: [TypeName]
--   }

-- newtype VisitorUnion = VisitorUnion
--   { visitorEnumValues :: [DataEnumValue s]
--   }
data VisitorTypeDefinition = VisitorTypeDefinition
  { visitorTypeDescription :: Maybe Description,
    visitorTypeName :: TypeName
  }
  deriving (Show, Eq)

data VisitorFieldDefinition = FieldDefinition
  { visitorFieldDescription :: Maybe Description,
    visitorFieldName :: FieldName,
    visitorFieldType :: TypeRef
  }
  deriving (Show, Eq)

data VisitorDefinition
  = VisitFieldDefinition VisitorFieldDefinition
  | VisitTypeDefinition VisitorTypeDefinition

type VisitorFunction = Directives VALID -> VisitorDefinition -> GQLResult VisitorDefinition

newtype SchemaVisitors = SchemaVisitors
  { schemaVisitors :: Map FieldName VisitorFunction
  }

defaultSchemaVisitors :: SchemaVisitors
defaultSchemaVisitors = SchemaVisitors mempty

instance Stitching SchemaVisitors where
  stitch (SchemaVisitors v1) (SchemaVisitors v2) = pure SchemaVisitors {schemaVisitors = v1 <> v2}

class ASTVisitor a where
  visit :: SchemaVisitors -> a VALID -> GQLResult (a VALID)

instance ASTVisitor Schema where
  visit
    visitors
    Schema
      { types,
        query,
        mutation,
        subscription,
        directiveDefinitions
      } =
      Schema
        <$> traverse (visit visitors) types
        <*> visit visitors query
        <*> traverse (visit visitors) mutation
        <*> traverse (visit visitors) subscription
        <*> pure directiveDefinitions

instance ASTVisitor (TypeDefinition cat) where
  visit
    SchemaVisitors {schemaVisitors}
    TypeDefinition
      { typeName,
        typeDescription,
        typeDirectives,
        typeContent
      } = do
      content <- visitContent typeContent
      VisitorTypeDefinition {..} <- schemaVisitors typeDirectives (VisitorTypeDefinition typeDescription typeName)
      pure $
        TypeDefinition
          visitorTypeDescription
          visitorTypeName
          typeDirectives
          content
      where
        visitContent = pure

-- instance ASTVisitor (FieldDefinition cat) where
--   visit FieldDefinition {..} = do
--     visitField <- asks (fieldVisitors . localContext)
--     visitField fieldDirectives $
--       FieldDefinition
--         fieldDescription
--         fieldName
--         fieldType
--         fieldContent
--         fieldDirectives

-- instance ASTVisitor ArgumentDefinition where
--   visit (ArgumentDefinition FieldDefinition {..}) =
--     ArgumentDefinition
--       <$> ( FieldDefinition
--               fieldDescription
--               fieldName
--               fieldType
--               <$> traverse checkArgumentDefaultValue fieldContent
--               <*> validateDirectives ARGUMENT_DEFINITION fieldDirectives
--           )
--     where
--       checkArgumentDefaultValue (DefaultInputValue value) =
--         DefaultInputValue
--           <$> validateDefaultValue fieldType (Just fieldName) value

-- instance ASTVisitor DataEnumValue where
--   visit DataEnumValue {enumDirectives = directives, ..} =
--     DataEnumValue enumDescription enumName
--       <$> validateDirectives ENUM_VALUE directives

-- instance ASTVisitor (UnionMember cat) where
--   visit UnionMember {..} = pure UnionMember {..}
