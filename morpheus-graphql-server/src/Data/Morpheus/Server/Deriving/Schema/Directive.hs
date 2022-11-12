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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Schema.Directive
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
    UseDirective (..),
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.Internal.Ext (unsafeFromList)
import Data.Morpheus.Internal.Utils (Empty (..), fromElems)
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    KindedProxy (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Types (FieldRep (..))
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseArguments (..),
    UseDirective (..),
    UseGQLType (..),
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
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    insertDirectiveDefinition,
    outToAny,
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
    IN,
    Position (Position),
    TRUE,
    TypeCategory (..),
    TypeName,
    Value,
  )
import GHC.Generics ()
import GHC.TypeLits ()
import Relude hiding (empty)

deriveDirectiveDefinition ::
  forall a b kind gql args.
  (gql a, GQLDirective a, args a) =>
  UseDirective gql args ->
  a ->
  b ->
  SchemaT kind (DirectiveDefinition CONST)
deriveDirectiveDefinition options _ _ = do
  directiveDefinitionArgs <- outToAny (useDeriveArguments (dirArgs options) (Proxy @a))
  pure
    ( DirectiveDefinition
        { directiveDefinitionName = deriveDirectiveName (dirGQL options) proxy,
          directiveDefinitionDescription = visitTypeDescription options proxy Nothing,
          directiveDefinitionArgs,
          directiveDefinitionLocations = getLocations proxy
        }
    )
  where
    proxy = Proxy @a

deriveDirectiveUsages :: UseDirective gql args -> [GDirectiveUsage gql args] -> SchemaT kind (Directives CONST)
deriveDirectiveUsages options = fmap unsafeFromList . traverse (toDirectiveTuple options)

toDirectiveTuple ::
  UseDirective gql args ->
  GDirectiveUsage gql args ->
  SchemaT kind (FieldName, Directive CONST)
toDirectiveTuple options (GDirectiveUsage x) = do
  insertDirective options (deriveDirectiveDefinition options x) x
  let directiveName = deriveDirectiveName (dirGQL options) (Identity x)
  args <- toList <$> useEncodeArguments (dirArgs options) x
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
    editArg Argument {..} = Argument {argumentName = applyGQLFieldOptions options (Identity x) argumentName, ..}

insertDirective ::
  forall gql args a k.
  gql a =>
  UseDirective gql args ->
  (KindedProxy IN a -> SchemaT k (DirectiveDefinition CONST)) ->
  a ->
  SchemaT k ()
insertDirective ops f _ = insertDirectiveDefinition (__useFingerprint (dirGQL ops) IN (Proxy :: Proxy a)) f proxy
  where
    proxy = KindedProxy :: KindedProxy IN a

getDirHM :: (Ord k, Hashable k, Empty a) => k -> HashMap k a -> a
getDirHM name xs = fromMaybe empty $ name `HM.lookup` xs

isIncluded :: GDirectiveUsage gql args -> Bool
isIncluded (GDirectiveUsage x) = not $ excludeFromSchema (Identity x)

getEnumDirectiveUsages :: gql a => UseDirective gql args -> f a -> TypeName -> [GDirectiveUsage gql args]
getEnumDirectiveUsages UseDirective {..} proxy name = getDirHM name $ enumValueDirectives $ __directives proxy

getFieldDirectiveUsages :: gql a => UseDirective gql args -> FieldName -> f a -> [GDirectiveUsage gql args]
getFieldDirectiveUsages UseDirective {..} name proxy = getDirHM name $ fieldDirectives $ __directives proxy

-- derive directives
deriveEnumDirectives :: gql a => UseDirective gql args -> f a -> TypeName -> SchemaT k (Directives CONST)
deriveEnumDirectives options proxy name = deriveDirectiveUsages options $ filter isIncluded $ getEnumDirectiveUsages options proxy name

deriveFieldDirectives :: gql a => UseDirective gql args -> f a -> FieldName -> SchemaT kind (Directives CONST)
deriveFieldDirectives options proxy name = deriveDirectiveUsages options $ filter isIncluded $ getFieldDirectiveUsages options name proxy

deriveTypeDirectives :: gql a => UseDirective gql args -> f a -> SchemaT kind (Directives CONST)
deriveTypeDirectives options proxy = deriveDirectiveUsages options $ filter isIncluded $ typeDirectives $ __directives options proxy

-- visit

visitEnumValueDescription :: gql a => UseDirective gql args -> f a -> TypeName -> Maybe Description -> Maybe Description
visitEnumValueDescription options proxy name desc = foldr applyEnumDescription desc (getEnumDirectiveUsages options proxy name)

visitEnumName :: gql a => UseDirective gql args -> f a -> TypeName -> TypeName
visitEnumName options proxy name = foldr applyEnumName (withTypeDirectives name) (getEnumDirectiveUsages options proxy name)
  where
    withTypeDirectives dirName = foldr applyTypeEnumNames dirName (typeDirectives $ __directives options proxy)

visitFieldDescription :: gql a => UseDirective gql args -> f a -> FieldName -> Maybe Description -> Maybe Description
visitFieldDescription options proxy name desc = foldr applyFieldDescription desc (getFieldDirectiveUsages options name proxy)

visitFieldDefaultValue :: gql a => UseDirective gql args -> f a -> FieldName -> Maybe (Value CONST) -> Maybe (Value CONST)
visitFieldDefaultValue options proxy name desc = foldr applyFieldDefaultValue desc (getFieldDirectiveUsages options name proxy)

visitFieldContent ::
  gql a =>
  UseDirective gql args ->
  CatType kind a ->
  FieldName ->
  Maybe (FieldContent TRUE kind CONST) ->
  Maybe (FieldContent TRUE kind CONST)
visitFieldContent options proxy@InputType name x =
  DefaultInputValue
    <$> visitFieldDefaultValue options proxy name (defaultInputValue <$> x)
visitFieldContent _ OutputType _ x = x

applyGQLFieldOptions :: gql a => UseDirective gql args -> f a -> FieldName -> FieldName
applyGQLFieldOptions options proxy = withTypeDirectives
  where
    withTypeDirectives name = foldr applyTypeFieldNames name (typeDirectives $ __directives options proxy)

visitFieldName :: gql a => UseDirective gql args -> f a -> FieldName -> FieldName
visitFieldName options proxy name = foldr applyFieldName (applyGQLFieldOptions options proxy name) (getFieldDirectiveUsages options name proxy)

visitTypeDescription :: gql a => UseDirective gql args -> f a -> Maybe Description -> Maybe Description
visitTypeDescription options proxy desc = foldr applyTypeDescription desc (typeDirectives $ __directives options proxy)

toFieldRes :: gql a => UseDirective gql args -> f a -> FieldRep v -> (FieldName, v)
toFieldRes options proxy FieldRep {..} = (visitFieldName options proxy fieldSelector, fieldValue)

deriveDirectiveName :: gql a => UseGQLType gql -> f a -> FieldName
deriveDirectiveName options proxy = coerce $ __useTypename options IN proxy
