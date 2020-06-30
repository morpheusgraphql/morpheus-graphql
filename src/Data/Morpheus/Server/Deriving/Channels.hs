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
  ( GetChannels (..),
    ChannelCon,
    GetChannel (..),
    ExploreChannels (..),
  )
where

-- MORPHEUS
import Data.Morpheus.Error (internalError)
import Data.Morpheus.Internal.Utils (elems, failure)
import Data.Morpheus.Server.Deriving.Decode
  ( DecodeType,
    decodeArguments,
  )
import Data.Morpheus.Types.Internal.AST
  ( FALSE,
    FieldName (..),
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

toProxy :: forall c e. CustomProxy c e -> Proxy e
toProxy _ = Proxy @e

type ChannelCon e m a =
  ( ExploreChannels FALSE (a (Resolver SUBSCRIPTION e m)) e,
    TypeRep e (Rep (a (Resolver SUBSCRIPTION e m))),
    Generic (a (Resolver SUBSCRIPTION e m))
  )

class GetChannels (e :: *) a | a -> e where
  getChannels :: a -> Selection VALID -> Eventless (Channel e)

instance
  ChannelCon e m subs =>
  GetChannels e (subs (Resolver SUBSCRIPTION e m))
  where
  getChannels value sel = selectBy sel $ exploreChannels (CustomProxy :: CustomProxy FALSE e) value

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
      Nothing -> internalError "invalid subscription: no channel is selected."
      Just f -> f sel
    _ -> internalError "invalid subscription: there can be only one top level selection"
selectBy _ _ = internalError "invalid subscription: expected selectionSet"

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

instance ChannelCon e m subs => ExploreChannels FALSE (subs (Resolver SUBSCRIPTION e m)) e where
  exploreChannels proxy = typeRep (toProxy proxy) . from

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
