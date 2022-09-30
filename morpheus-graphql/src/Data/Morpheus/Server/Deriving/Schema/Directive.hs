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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Schema.Directive
  ( deriveFieldDirectives,
    deriveTypeDirectives,
    deriveEnumDirectives,
  )
where

import Control.Monad.Except (throwError)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import Data.Morpheus.CodeGen.Internal.AST (CONST, TypeName)
import Data.Morpheus.Internal.Ext (resultOr, unsafeFromList)
import Data.Morpheus.Internal.Utils (Empty (..), (<:>))
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( KindedProxy (..),
  )
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
    deriveFingerprint,
    deriveTypename,
    encodeArguments,
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    insertDirectiveDefinition,
    outToAny,
  )
import Data.Morpheus.Types.Internal.AST
  ( Directive (..),
    DirectiveDefinition (..),
    Directives,
    FieldName,
    IN,
    Position (Position),
    unpackName,
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

deriveTypeDirectives :: forall c f a. GQLType a => f a -> SchemaT c (Directives CONST)
deriveTypeDirectives proxy = deriveDirectiveUsages (typeDirectives $ directives proxy)

deriveDirectiveUsages :: [DirectiveUsage] -> SchemaT c (Directives CONST)
deriveDirectiveUsages = fmap unsafeFromList . traverse toDirectiveTuple

deriveDirectiveName :: forall f a. GQLType a => f a -> FieldName
deriveDirectiveName _ = coerce $ deriveTypename (KindedProxy :: KindedProxy IN a)

toDirectiveTuple :: DirectiveUsage -> SchemaT c (FieldName, Directive CONST)
toDirectiveTuple (DirectiveUsage x) = do
  insertDirective (deriveDirectiveDefinition x) x
  let directiveName = deriveDirectiveName (Identity x)
  directiveArgs <- resultOr (const $ throwError "TODO: fix me") pure (encodeArguments x)
  pure
    ( directiveName,
      Directive
        { directivePosition = Position 0 0,
          directiveName,
          directiveArgs
        }
    )

insertDirective ::
  forall a c.
  (GQLType a) =>
  (KindedProxy IN a -> SchemaT c (DirectiveDefinition CONST)) ->
  a ->
  SchemaT c ()
insertDirective f _ = insertDirectiveDefinition (deriveFingerprint proxy) f proxy
  where
    proxy = KindedProxy :: KindedProxy IN a

getDir :: (Ord k, Empty a) => k -> Map k a -> a
getDir name xs = fromMaybe empty $ name `M.lookup` xs

getDirHM :: (Ord k, Hashable k, Empty a) => k -> HashMap k a -> a
getDirHM name xs = fromMaybe empty $ name `HM.lookup` xs

deriveFieldDirectives :: GQLType a => f a -> FieldName -> SchemaT c (Directives CONST)
deriveFieldDirectives proxy name = do
  dirs <- deriveDirectiveUsages $ getDirHM name $ fieldDirectives $ directives proxy
  getDir (unpackName name) (getDirectives proxy) <:> dirs

deriveEnumDirectives :: GQLType a => f a -> TypeName -> SchemaT c (Directives CONST)
deriveEnumDirectives proxy name = do
  dirs <- deriveDirectiveUsages $ getDirHM name $ enumValueDirectives $ directives proxy
  getDir (unpackName name) (getDirectives proxy) <:> dirs
