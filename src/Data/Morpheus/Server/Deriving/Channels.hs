{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Server.Deriving.Channels
  ( getChannels,
    ChannelCon,
    GetChannel (..),
    ExploreChannels (..),
  )
where

-- MORPHEUS
import Data.Morpheus.Error (internalError)
import Data.Morpheus.Internal.Utils (elems)
import Data.Morpheus.Server.Deriving.Decode
  ( DecodeType,
    decodeArguments,
  )
import Data.Morpheus.Server.Types.GQLType (GQLType (..))
import Data.Morpheus.Types.Internal.AST
  ( FALSE,
    FieldName (..),
    InternalError,
    SUBSCRIPTION,
    Selection (..),
    SelectionContent (..),
    VALID,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Channel,
    Eventless,
    Resolver,
    SubscriptionField (..),
  )
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Text
  ( pack,
  )
import GHC.Generics

data CustomProxy (c :: Bool) e = CustomProxy

type ChannelCon e m a =
  ExploreChannels
    (CUSTOM (a (Resolver SUBSCRIPTION e m)))
    (a (Resolver SUBSCRIPTION e m))
    e

getChannels ::
  forall e m subs.
  ChannelCon e m subs =>
  subs (Resolver SUBSCRIPTION e m) ->
  Selection VALID ->
  Eventless (Channel e)
getChannels value sel =
  selectBy sel $
    exploreChannels (CustomProxy :: CustomProxy (CUSTOM (subs (Resolver SUBSCRIPTION e m))) e) value

selectBy ::
  Selection VALID ->
  [ ( FieldName,
      Selection VALID -> Eventless (Channel e)
    )
  ] ->
  Eventless (Channel e)
selectBy Selection {selectionContent = SelectionSet selSet} ch =
  case elems selSet of
    [sel@Selection {selectionName}] -> case lookup selectionName ch of
      Nothing -> "invalid subscription: no channel is selected." :: InternalError
      Just f -> f sel
    _ -> "invalid subscription: there can be only one top level selection" :: InternalError
selectBy _ _ = "invalid subscription: expected selectionSet" :: InternalError

class GetChannel e a | a -> e where
  getChannel :: a -> Selection VALID -> Eventless (Channel e)

instance GetChannel e (SubscriptionField (Resolver SUBSCRIPTION e m a)) where
  getChannel SubscriptionField {channel} = const (pure channel)

instance
  (Generic arg, DecodeType arg) =>
  GetChannel e (arg -> SubscriptionField (Resolver SUBSCRIPTION e m a))
  where
  getChannel f sel@Selection {selectionArguments} =
    decodeArguments selectionArguments >>= (`getChannel` sel) . f

------------------------------------------------------
class ExploreChannels (custom :: Bool) a e where
  exploreChannels :: CustomProxy custom e -> a -> [(FieldName, Selection VALID -> Eventless (Channel e))]

instance
  ( TypeRep e (Rep (subs (Resolver SUBSCRIPTION e m))),
    Generic (subs (Resolver SUBSCRIPTION e m))
  ) =>
  ExploreChannels FALSE (subs (Resolver SUBSCRIPTION e m)) e
  where
  exploreChannels _ = typeRep (Proxy @e) . from

------------------------------------------------------
class TypeRep e f where
  typeRep :: Proxy e -> f a -> [(FieldName, Selection VALID -> Eventless (Channel e))]

instance TypeRep e f => TypeRep e (M1 D d f) where
  typeRep c (M1 src) = typeRep c src

instance FieldRep e f => TypeRep e (M1 C c f) where
  typeRep c (M1 src) = fieldRep c src

--- FIELDS
class FieldRep e f where
  fieldRep :: Proxy e -> f a -> [(FieldName, Selection VALID -> Eventless (Channel e))]

instance (FieldRep e f, FieldRep e g) => FieldRep e (f :*: g) where
  fieldRep e (a :*: b) = fieldRep e a <> fieldRep e b

instance (Selector s, GetChannel e a) => FieldRep e (M1 S s (K1 s2 a)) where
  fieldRep _ m@(M1 (K1 src)) = [(FieldName $ pack (selName m), getChannel src)]

instance FieldRep e U1 where
  fieldRep _ _ = []
