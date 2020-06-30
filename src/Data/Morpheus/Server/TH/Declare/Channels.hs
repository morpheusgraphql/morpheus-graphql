{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.TH.Declare.Channels
  ( deriveChannels,
  )
where

import Data.Morpheus.Internal.TH
  ( _',
    apply,
    destructRecord,
    funDSimple,
    m',
    mkFieldsE,
  )
import Data.Morpheus.Server.Deriving.Channels
  ( ExploreChannels (..),
    GetChannel (..),
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.Internal.TH.Utils
  ( e',
  )
import Data.Morpheus.Types.Internal.AST
  ( ConsD (..),
    FieldDefinition (..),
    FieldName,
    SUBSCRIPTION,
    Selection,
    TRUE,
    TypeName (..),
    VALID,
    isSubscription,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Channel,
    Eventless,
    Resolver,
  )
import Data.Semigroup ((<>))
import Language.Haskell.TH

mkEntry ::
  GetChannel e a =>
  FieldName ->
  a ->
  ( FieldName,
    Selection VALID -> Eventless (Channel e)
  )
mkEntry name field = (name, getChannel field)

-- | defines :: "MyType" ==> MyType (Resolver SUBSCRIPTION e m)
mkType :: TypeName -> Type
mkType tName = apply tName [apply ''Resolver [ConT ''SUBSCRIPTION, e', m']]

-- | defines: ExploreChannels ('TRUE) (<Type> (Resolver SUBSCRIPTION e m)) e
mkTypeClass :: TypeName -> Type
mkTypeClass tName = apply ''ExploreChannels [ConT ''TRUE, mkType tName, e']

exploreChannelsD :: TypeName -> [FieldDefinition cat] -> DecQ
exploreChannelsD tName fields = funDSimple 'exploreChannels args body
  where
    args = [_', destructRecord tName fields]
    body = pure (mkFieldsE 'mkEntry fields)

deriveChannels :: ServerTypeDefinition cat -> Q [Dec]
deriveChannels ServerTypeDefinition {tName, tCons = [ConsD {cFields}], tKind}
  | isSubscription tKind =
    pure <$> instanceD context typeDef funDefs
  where
    context = cxt []
    typeDef = pure (mkTypeClass tName)
    funDefs = [exploreChannelsD tName cFields]
deriveChannels _ = pure []
