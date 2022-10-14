{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
    visitEnumName,
    visitFieldName,
    toFieldRes,
  )
where

import Control.Monad.Except (throwError)
import Data.HashMap.Lazy qualified as HM
import Data.Morpheus.Internal.Ext (resultOr, unsafeFromList)
import Data.Morpheus.Internal.Utils (Empty (..), fromElems)
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( KindedProxy (..),
  )
import Data.Morpheus.Server.Deriving.Utils.Types (FieldRep (..))
import Data.Morpheus.Server.Types.Directives
  ( GQLDirective (..),
    ToLocations,
    getLocations,
  )
import Data.Morpheus.Server.Types.GQLType
  ( DeriveArguments (..),
    DirectiveUsage (..),
    DirectiveUsages (..),
    GQLType (..),
    applyEnumDescription,
    applyEnumName,
    applyFieldDescription,
    applyFieldName,
    applyTypeDescription,
    applyTypeEnumNames,
    applyTypeFieldNames,
    deriveFingerprint,
    deriveTypename,
    encodeArguments,
  )
import Data.Morpheus.Server.Types.Internal
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
    FieldName,
    IN,
    Position (Position),
    TypeName,
  )
import GHC.Generics ()
import GHC.TypeLits ()
import Relude hiding (empty)

type DirectiveDefinitionConstraint a =
  ( GQLDirective a,
    GQLType a,
    DeriveArguments (KIND a) a,
    ToLocations (DIRECTIVE_LOCATIONS a)
  )

deriveDirectiveDefinition ::
  forall a b kind.
  (DirectiveDefinitionConstraint a) =>
  a ->
  b ->
  SchemaT kind (DirectiveDefinition CONST)
deriveDirectiveDefinition _ _ = do
  directiveDefinitionArgs <- outToAny (deriveArgumentsDefinition (KindedProxy :: KindedProxy (KIND a) a))
  pure
    ( DirectiveDefinition
        { directiveDefinitionName = deriveDirectiveName proxy,
          directiveDefinitionDescription = description proxy,
          directiveDefinitionArgs,
          directiveDefinitionLocations = getLocations proxy
        }
    )
  where
    proxy = Proxy @a

deriveDirectiveUsages :: [DirectiveUsage] -> SchemaT c (Directives CONST)
deriveDirectiveUsages = fmap unsafeFromList . traverse toDirectiveTuple

deriveDirectiveName :: forall f a. GQLType a => f a -> FieldName
deriveDirectiveName _ = coerce $ deriveTypename (KindedProxy :: KindedProxy IN a)

toDirectiveTuple :: DirectiveUsage -> SchemaT c (FieldName, Directive CONST)
toDirectiveTuple (DirectiveUsage x) = do
  insertDirective (deriveDirectiveDefinition x) x
  let directiveName = deriveDirectiveName (Identity x)
  args <- toList <$> resultOr (const $ throwError "TODO: fix me") pure (encodeArguments x)
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
    editArg Argument {..} = Argument {argumentName = applyGQLFieldOptions (Identity x) argumentName, ..}

insertDirective ::
  forall a c.
  (GQLType a) =>
  (KindedProxy IN a -> SchemaT c (DirectiveDefinition CONST)) ->
  a ->
  SchemaT c ()
insertDirective f _ = insertDirectiveDefinition (deriveFingerprint proxy) f proxy
  where
    proxy = KindedProxy :: KindedProxy IN a

getDirHM :: (Ord k, Hashable k, Empty a) => k -> HashMap k a -> a
getDirHM name xs = fromMaybe empty $ name `HM.lookup` xs

isIncluded :: DirectiveUsage -> Bool
isIncluded (DirectiveUsage x) = not $ excludeFromSchema (Identity x)

getEnumDirectiveUsages :: GQLType a => f a -> TypeName -> [DirectiveUsage]
getEnumDirectiveUsages proxy name = getDirHM name $ enumValueDirectives $ directives proxy

getFieldDirectiveUsages :: GQLType a => FieldName -> f a -> [DirectiveUsage]
getFieldDirectiveUsages name proxy = getDirHM name $ fieldDirectives $ directives proxy

getOptions :: GQLType a => f a -> GQLTypeOptions
getOptions proxy = typeOptions proxy defaultTypeOptions

-- derive directives
deriveEnumDirectives :: GQLType a => f a -> TypeName -> SchemaT c (Directives CONST)
deriveEnumDirectives proxy name = deriveDirectiveUsages $ filter isIncluded $ getEnumDirectiveUsages proxy name

deriveFieldDirectives :: GQLType a => f a -> FieldName -> SchemaT c (Directives CONST)
deriveFieldDirectives proxy name = deriveDirectiveUsages $ filter isIncluded $ getFieldDirectiveUsages name proxy

deriveTypeDirectives :: forall c f a. GQLType a => f a -> SchemaT c (Directives CONST)
deriveTypeDirectives proxy = deriveDirectiveUsages $ filter isIncluded $ typeDirectives $ directives proxy

-- visit
visitEnumValueDescription :: GQLType a => f a -> TypeName -> Maybe Description -> Maybe Description
visitEnumValueDescription proxy name desc = foldr applyEnumDescription desc (getEnumDirectiveUsages proxy name)

visitEnumName :: GQLType a => f a -> TypeName -> TypeName
visitEnumName proxy name = foldr applyEnumName (withTypeDirectives $ withOptions name) (getEnumDirectiveUsages proxy name)
  where
    withOptions = fromString . constructorTagModifier (getOptions proxy) . toString
    withTypeDirectives dirName = foldr applyTypeEnumNames dirName (typeDirectives $ directives proxy)

visitFieldDescription :: GQLType a => f a -> FieldName -> Maybe Description -> Maybe Description
visitFieldDescription proxy name desc = foldr applyFieldDescription desc (getFieldDirectiveUsages name proxy)

applyGQLFieldOptions :: (GQLType a) => f a -> FieldName -> FieldName
applyGQLFieldOptions proxy = withTypeDirectives . withOptions
  where
    withOptions = fromString . fieldLabelModifier (getOptions proxy) . toString
    withTypeDirectives name = foldr applyTypeFieldNames name (typeDirectives $ directives proxy)

visitFieldName :: GQLType a => f a -> FieldName -> FieldName
visitFieldName proxy name = foldr applyFieldName (applyGQLFieldOptions proxy name) (getFieldDirectiveUsages name proxy)

visitTypeDescription :: GQLType a => f a -> Maybe Description -> Maybe Description
visitTypeDescription proxy desc = foldr applyTypeDescription desc (typeDirectives $ directives proxy)

toFieldRes :: GQLType a => f a -> FieldRep v -> (FieldName, v)
toFieldRes proxy FieldRep {..} = (visitFieldName proxy fieldSelector, fieldValue)
