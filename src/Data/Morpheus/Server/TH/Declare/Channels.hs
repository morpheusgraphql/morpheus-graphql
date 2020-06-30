{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.TH.Declare.Channels
  ( deriveChannels,
  )
where

import Data.Morpheus.Internal.TH
  ( applyT,
    destructRecord,
    instanceHeadMultiT,
    m_,
    mkFieldsE,
    mkTypeName,
    nameVarP,
    nameVarT,
    simpleFunD,
  )
import Data.Morpheus.Server.Deriving.Channels
  ( ExploreChannels (..),
    GetChannel (..),
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.Internal.TH.Utils
  ( e_,
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

encodeVars :: [Q Type]
encodeVars = conT ''SUBSCRIPTION : map (varT . mkTypeName) [e_, m_]

genHeadType :: TypeName -> [Q Type]
genHeadType tName = mainType : instanceArgs
  where
    instanceArgs = map nameVarT [e_]
    mainTypeArg = [applyT ''Resolver encodeVars]
    mainType = applyT (mkTypeName tName) mainTypeArg

mkEntry ::
  GetChannel e a =>
  FieldName ->
  a ->
  ( FieldName,
    Selection VALID -> Eventless (Channel e)
  )
mkEntry name field = (name, getChannel field)

-- | defines: ExploreChannels ('TRUE) (<Type> (Resolver SUBSCRIPTION e m)) e
instanceType :: TypeName -> Q Type
instanceType tName =
  instanceHeadMultiT
    ''ExploreChannels
    (conT ''TRUE)
    (genHeadType tName)

exploreChannelsD :: TypeName -> [FieldDefinition cat] -> DecQ
exploreChannelsD tName fields = simpleFunD 'exploreChannels args body
  where
    args = [nameVarP "_", destructRecord tName fields]
    body = pure (mkFieldsE 'mkEntry fields)

deriveChannels :: ServerTypeDefinition cat -> Q [Dec]
deriveChannels ServerTypeDefinition {tName, tCons = [ConsD {cFields}], tKind}
  | isSubscription tKind =
    pure <$> instanceD context typeDef funDefs
  where
    context = cxt []
    typeDef = instanceType tName
    funDefs = [exploreChannelsD tName cFields]
deriveChannels _ = pure []
