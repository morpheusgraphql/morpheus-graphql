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

module Data.Morpheus.Validation.Document.Visitors
  ( ASTVisitor (..),
    Visitors (..),
  )
where

import Data.Morpheus.Types.Internal.AST
  ( Directives,
    FieldDefinition (..),
    Schema (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    VALID,
  )
import Data.Morpheus.Types.Internal.AST.TypeCategory (ANY)
import Data.Morpheus.Types.Internal.Validation
  ( Validator,
    ValidatorContext (localContext),
  )
import Relude hiding (empty, local)

data Visitors = Visitors
  { typeVisitors :: Directives VALID -> TypeDefinition ANY VALID -> VisitorM (TypeDefinition ANY VALID),
    fieldVisitors :: Directives VALID -> TypeDefinition ANY VALID -> VisitorM (TypeDefinition ANY VALID)
  }

type VisitorM = Validator VALID Visitors

class ASTVisitor a where
  visit :: a VALID -> VisitorM (a VALID)

-- instance ASTVisitor Schema where
--   visit
--     Schema
--       { types,
--         query,
--         mutation,
--         subscription,
--         directiveDefinitions
--       } =
--       Schema
--         <$> traverse visit types
--         <*> visit query
--         <*> traverse visit mutation
--         <*> traverse visit subscription
--         <*> traverse visit directiveDefinitions

-- instance ASTVisitor (TypeDefinition cat) where
--   visit
--     TypeDefinition
--       { typeName,
--         typeDescription,
--         typeDirectives,
--         typeContent
--       } = do
--       visitType <- asks (typeVisitors . localContext)
--       content <- visit typeContent
--       visitType
--         typeDirectives
--         ( TypeDefinition
--             typeDescription
--             typeName
--             typeDirectives
--             content
--         )

-- instance ASTVisitor (TypeContent TRUE cat) where
--   visit DataObject {objectImplements, objectFields} = do
--     fields <- traverse visit objectFields
--     pure $ DataObject objectImplements fields
--   -- visit DataInputObject {inputObjectFields} =
--   --   DataInputObject <$> traverse visit inputObjectFields
--   -- visit DataScalar {..} = pure DataScalar {..}
--   -- visit DataEnum {enumMembers} = DataEnum <$> traverse visit enumMembers
--   -- visit DataInputUnion {inputUnionMembers} =
--   --   DataInputUnion <$> traverse visit inputUnionMembers
--   -- visit DataUnion {unionMembers} = DataUnion <$> traverse visit unionMembers
--   -- visit (DataInterface fields) = DataInterface <$> traverse visit fields
--   visit x = pure x

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

-- instance ASTVisitor DirectiveDefinition where
--   visit DirectiveDefinition {directiveDefinitionArgs = arguments, ..} =
--     inType "Directive" $
--       inField directiveDefinitionName $ do
--         directiveDefinitionArgs <- traverse visit arguments
--         pure DirectiveDefinition {..}

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
