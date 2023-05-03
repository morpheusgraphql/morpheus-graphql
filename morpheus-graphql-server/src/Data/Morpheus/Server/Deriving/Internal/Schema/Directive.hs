{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Internal.Schema.Directive
  ( deriveFieldDirectives,
    deriveTypeDirectives,
    deriveEnumDirectives,
    visitEnumValueDescription,
    visitFieldDescription,
    visitTypeDescription,
    visitFieldDefaultValue,
    visitFieldContent,
    visitEnumName,
    visitFieldName,
    toFieldRes,
    UseDeriving (..),
    deriveDirectiveDefinition,
  )
where

import Control.Monad.Except
import Data.Morpheus.Generic
  ( GRepField (..),
  )
import Data.Morpheus.Internal.Ext (GQLResult, unsafeFromList)
import Data.Morpheus.Internal.Utils (Empty (..), fromElems, lookup)
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    inputType,
  )
import Data.Morpheus.Server.Deriving.Utils.Types
  ( coerceArguments,
    typeToArguments,
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
    UseGQLValue (..),
  )
import Data.Morpheus.Server.Types.Directives
  ( GDirectiveUsage (..),
    GDirectiveUsages (..),
    GQLDirective (..),
    applyEnumDescription,
    applyEnumName,
    applyFieldDefaultValue,
    applyFieldDescription,
    applyFieldName,
    applyTypeDescription,
    applyTypeEnumNames,
    applyTypeFieldNames,
    getLocations,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    CONST,
    Description,
    Directive (..),
    DirectiveDefinition (..),
    Directives,
    FieldContent (..),
    FieldName,
    GQLError,
    IN,
    Position (..),
    TRUE,
    TypeDefinition,
    TypeName,
    Value (..),
  )
import GHC.Generics ()
import GHC.TypeLits ()
import Relude hiding (empty)

-- derive directives

deriveDirectiveDefinition :: (MonadError GQLError m, gql a, GQLDirective a, val a) => UseDeriving gql val -> f a -> TypeDefinition IN CONST -> m (DirectiveDefinition CONST)
deriveDirectiveDefinition ctx proxy t = do
  directiveDefinitionArgs <- typeToArguments t
  pure
    ( DirectiveDefinition
        { directiveDefinitionName = deriveDirectiveName ctx proxy,
          directiveDefinitionDescription = visitTypeDescription ctx proxy Nothing,
          directiveDefinitionArgs,
          directiveDefinitionLocations = getLocations proxy
        }
    )

deriveEnumDirectives :: (gql a) => UseDeriving gql args -> f a -> TypeName -> GQLResult (Directives CONST)
deriveEnumDirectives ctx proxy = deriveDirectiveUsages ctx . getEnumDirectiveUsages ctx proxy

deriveFieldDirectives :: (gql a) => UseDeriving gql args -> f a -> FieldName -> GQLResult (Directives CONST)
deriveFieldDirectives ctx proxy = deriveDirectiveUsages ctx . getFieldDirectiveUsages ctx proxy

deriveTypeDirectives :: (gql a) => UseDeriving gql args -> f a -> GQLResult (Directives CONST)
deriveTypeDirectives ctx = deriveDirectiveUsages ctx . typeDirectives . useDirectives ctx

deriveDirectiveUsages :: UseDeriving gql args -> [GDirectiveUsage gql args] -> GQLResult (Directives CONST)
deriveDirectiveUsages options = fmap unsafeFromList . traverse (toDirective options) . filter isIncluded

-- others

toDirective ::
  UseDeriving gql args ->
  GDirectiveUsage gql args ->
  GQLResult (FieldName, Directive CONST)
toDirective ctx (GDirectiveUsage x) = do
  args <- useEncodeValue ctx x >>= coerceArguments
  directiveArgs <- fromElems (map (visitArgument ctx x) (toList args))
  pure
    ( directiveName,
      Directive
        { directivePosition = Position 0 0,
          directiveName,
          directiveArgs
        }
    )
  where
    directiveName = deriveDirectiveName ctx (Identity x)

getDirHM :: (Ord k, Hashable k, Empty a) => k -> HashMap k a -> a
getDirHM name xs = fromMaybe empty $ name `lookup` xs

isIncluded :: GDirectiveUsage gql args -> Bool
isIncluded (GDirectiveUsage x) = not $ excludeFromSchema (Identity x)

getEnumDirectiveUsages :: (gql a) => UseDeriving gql args -> f a -> TypeName -> [GDirectiveUsage gql args]
getEnumDirectiveUsages UseDeriving {..} proxy name = getDirHM name $ enumValueDirectives $ useDirectives proxy

getFieldDirectiveUsages :: (gql a) => UseDeriving gql args -> f a -> FieldName -> [GDirectiveUsage gql args]
getFieldDirectiveUsages UseDeriving {..} proxy name = getDirHM name $ fieldDirectives $ useDirectives proxy

-- visit
visitEnumValueDescription :: (gql a) => UseDeriving gql args -> f a -> TypeName -> Maybe Description -> Maybe Description
visitEnumValueDescription options proxy name desc = foldr applyEnumDescription desc (getEnumDirectiveUsages options proxy name)

visitEnumName :: (gql a) => UseDeriving gql args -> f a -> TypeName -> TypeName
visitEnumName options proxy name = foldr applyEnumName (withTypeDirectives name) (getEnumDirectiveUsages options proxy name)
  where
    withTypeDirectives dirName = foldr applyTypeEnumNames dirName (typeDirectives $ useDirectives options proxy)

visitFieldDescription :: (gql a) => UseDeriving gql args -> f a -> FieldName -> Maybe Description -> Maybe Description
visitFieldDescription options proxy name desc = foldr applyFieldDescription desc (getFieldDirectiveUsages options proxy name)

visitFieldDefaultValue :: (gql a) => UseDeriving gql args -> f a -> FieldName -> Maybe (Value CONST) -> Maybe (Value CONST)
visitFieldDefaultValue options proxy name desc = foldr applyFieldDefaultValue desc (getFieldDirectiveUsages options proxy name)

visitFieldContent ::
  (gql a) =>
  UseDeriving gql args ->
  CatType kind a ->
  FieldName ->
  Maybe (FieldContent TRUE kind CONST) ->
  Maybe (FieldContent TRUE kind CONST)
visitFieldContent options proxy@InputType name x =
  DefaultInputValue
    <$> visitFieldDefaultValue options proxy name (defaultInputValue <$> x)
visitFieldContent _ OutputType _ x = x

visitArgument :: (gql a) => UseDeriving gql args -> a -> Argument valid -> Argument valid
visitArgument ctx x Argument {..} = Argument {argumentName = visitFieldName ctx (Identity x) argumentName, ..}

visitFieldName :: (gql a) => UseDeriving gql args -> f a -> FieldName -> FieldName
visitFieldName options proxy name = foldr applyFieldName (visitTypeFieldNames options proxy name) (getFieldDirectiveUsages options proxy name)

visitTypeFieldNames :: (gql a) => UseDeriving gql args -> f a -> FieldName -> FieldName
visitTypeFieldNames ctx proxy name = foldr applyTypeFieldNames name (typeDirectives $ useDirectives ctx proxy)

visitTypeDescription :: (gql a) => UseDeriving gql args -> f a -> Maybe Description -> Maybe Description
visitTypeDescription options proxy desc = foldr applyTypeDescription desc (typeDirectives $ useDirectives options proxy)

toFieldRes :: (gql a) => UseDeriving gql args -> f a -> GRepField v -> (FieldName, v)
toFieldRes options proxy GRepField {..} = (visitFieldName options proxy fieldSelector, fieldValue)

deriveDirectiveName :: (UseGQLType ctx gql, gql a) => ctx -> f a -> FieldName
deriveDirectiveName options = coerce . useTypename options . inputType
