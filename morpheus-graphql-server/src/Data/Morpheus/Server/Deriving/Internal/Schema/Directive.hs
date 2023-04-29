{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Morpheus.Internal.Ext (GQLResult, resultOr, unsafeFromList)
import Data.Morpheus.Internal.Utils (Empty (..), fromElems, lookup)
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    inputType,
  )
import Data.Morpheus.Server.Deriving.Utils.Types
  ( typeToArguments,
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
    UseValue (..),
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
    Arguments,
    CONST,
    Description,
    Directive (..),
    DirectiveDefinition (..),
    Directives,
    FieldContent (..),
    FieldName,
    GQLError,
    IN,
    ObjectEntry (..),
    Position (..),
    TRUE,
    TypeDefinition,
    TypeName,
    Value (..),
    internal,
  )
import GHC.Generics ()
import GHC.TypeLits ()
import Relude hiding (empty)

deriveDirectiveDefinition :: (MonadError GQLError m, gql a, GQLDirective a, args a) => UseDeriving gql args -> f a -> TypeDefinition IN CONST -> m (DirectiveDefinition CONST)
deriveDirectiveDefinition options@UseDeriving {..} proxy t = do
  directiveDefinitionArgs <- typeToArguments t
  pure
    ( DirectiveDefinition
        { directiveDefinitionName = deriveDirectiveName useGQL proxy,
          directiveDefinitionDescription = visitTypeDescription options proxy Nothing,
          directiveDefinitionArgs,
          directiveDefinitionLocations = getLocations proxy
        }
    )

deriveDirectiveUsages :: UseDeriving gql args -> [GDirectiveUsage gql args] -> GQLResult (Directives CONST)
deriveDirectiveUsages options = fmap unsafeFromList . traverse (toDirectiveTuple options)

encodeDirectiveArguments :: (val a) => UseValue val -> a -> GQLResult (Arguments CONST)
encodeDirectiveArguments val x = resultOr (const $ throwError err) pure (useEncodeValue val x) >>= unpackValue
  where
    err = internal "could not encode arguments. Arguments should be an object like type!"
    unpackValue (Object v) = pure $ fmap toArgument v
    unpackValue _ = throwError err
    toArgument ObjectEntry {..} = Argument (Position 0 0) entryName entryValue

toDirectiveTuple ::
  UseDeriving gql args ->
  GDirectiveUsage gql args ->
  GQLResult (FieldName, Directive CONST)
toDirectiveTuple drv (GDirectiveUsage x) = do
  args <- toList <$> encodeDirectiveArguments (useValue drv) x
  directiveArgs <- fromElems (map editArg args)
  pure
    ( directiveName,
      Directive
        { directivePosition = Position 0 0,
          directiveName,
          directiveArgs
        }
    )
  where
    directiveName = deriveDirectiveName (useGQL drv) (Identity x)
    editArg Argument {..} = Argument {argumentName = applyGQLFieldOptions drv (Identity x) argumentName, ..}

getDirHM :: (Ord k, Hashable k, Empty a) => k -> HashMap k a -> a
getDirHM name xs = fromMaybe empty $ name `lookup` xs

isIncluded :: GDirectiveUsage gql args -> Bool
isIncluded (GDirectiveUsage x) = not $ excludeFromSchema (Identity x)

getEnumDirectiveUsages :: (gql a) => UseDeriving gql args -> f a -> TypeName -> [GDirectiveUsage gql args]
getEnumDirectiveUsages UseDeriving {..} proxy name = getDirHM name $ enumValueDirectives $ useDirectives proxy

getFieldDirectiveUsages :: (gql a) => UseDeriving gql args -> FieldName -> f a -> [GDirectiveUsage gql args]
getFieldDirectiveUsages UseDeriving {..} name proxy = getDirHM name $ fieldDirectives $ useDirectives proxy

-- derive directives
deriveEnumDirectives :: (gql a) => UseDeriving gql args -> f a -> TypeName -> GQLResult (Directives CONST)
deriveEnumDirectives options proxy name = deriveDirectiveUsages options $ filter isIncluded $ getEnumDirectiveUsages options proxy name

deriveFieldDirectives :: (gql a) => UseDeriving gql args -> f a -> FieldName -> GQLResult (Directives CONST)
deriveFieldDirectives options proxy name = deriveDirectiveUsages options $ filter isIncluded $ getFieldDirectiveUsages options name proxy

deriveTypeDirectives :: (gql a) => UseDeriving gql args -> f a -> GQLResult (Directives CONST)
deriveTypeDirectives options proxy = deriveDirectiveUsages options $ filter isIncluded $ typeDirectives $ useDirectives options proxy

-- visit

visitEnumValueDescription :: (gql a) => UseDeriving gql args -> f a -> TypeName -> Maybe Description -> Maybe Description
visitEnumValueDescription options proxy name desc = foldr applyEnumDescription desc (getEnumDirectiveUsages options proxy name)

visitEnumName :: (gql a) => UseDeriving gql args -> f a -> TypeName -> TypeName
visitEnumName options proxy name = foldr applyEnumName (withTypeDirectives name) (getEnumDirectiveUsages options proxy name)
  where
    withTypeDirectives dirName = foldr applyTypeEnumNames dirName (typeDirectives $ useDirectives options proxy)

visitFieldDescription :: (gql a) => UseDeriving gql args -> f a -> FieldName -> Maybe Description -> Maybe Description
visitFieldDescription options proxy name desc = foldr applyFieldDescription desc (getFieldDirectiveUsages options name proxy)

visitFieldDefaultValue :: (gql a) => UseDeriving gql args -> f a -> FieldName -> Maybe (Value CONST) -> Maybe (Value CONST)
visitFieldDefaultValue options proxy name desc = foldr applyFieldDefaultValue desc (getFieldDirectiveUsages options name proxy)

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

applyGQLFieldOptions :: (gql a) => UseDeriving gql args -> f a -> FieldName -> FieldName
applyGQLFieldOptions options proxy = withTypeDirectives
  where
    withTypeDirectives name = foldr applyTypeFieldNames name (typeDirectives $ useDirectives options proxy)

visitFieldName :: (gql a) => UseDeriving gql args -> f a -> FieldName -> FieldName
visitFieldName options proxy name = foldr applyFieldName (applyGQLFieldOptions options proxy name) (getFieldDirectiveUsages options name proxy)

visitTypeDescription :: (gql a) => UseDeriving gql args -> f a -> Maybe Description -> Maybe Description
visitTypeDescription options proxy desc = foldr applyTypeDescription desc (typeDirectives $ useDirectives options proxy)

toFieldRes :: (gql a) => UseDeriving gql args -> f a -> GRepField v -> (FieldName, v)
toFieldRes options proxy GRepField {..} = (visitFieldName options proxy fieldSelector, fieldValue)

deriveDirectiveName :: (gql a) => UseGQLType gql -> f a -> FieldName
deriveDirectiveName options = coerce . useTypename options . inputType
