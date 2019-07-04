{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Resolve.Resolve
  ( resolveStateless
  , resolveByteString
  , resolveStreamByteString
  , resolveStream
  , packStream
  , RootResCon
  ) where

import           Control.Monad.Trans.Except                 (ExceptT (..), runExceptT)
import           Data.Aeson                                 (eitherDecode, encode)
import           Data.ByteString.Lazy.Char8                 (ByteString)
import           Data.Morpheus.Error.Utils                  (badRequestError, renderErrors)
import           Data.Morpheus.Parser.Parser                (parseGQL)
import           Data.Morpheus.Resolve.Encode               (ObjectFieldResolvers (..), resolveBySelection, resolversBy)
import           Data.Morpheus.Resolve.Generics.TypeRep     (ObjectRep (..), resolveTypes)
import           Data.Morpheus.Schema.SchemaAPI             (hiddenRootFields, schemaAPI, schemaTypes)
import           Data.Morpheus.Server.ClientRegister        (GQLState, publishUpdates)
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator (..), Operator' (..))
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataArguments, DataFingerprint (..), DataType (..),
                                                             DataTypeLib (..), initTypeLib)
import           Data.Morpheus.Types.Internal.Stream        (ResponseEvent (..), ResponseStream, StreamState (..),
                                                             StreamT (..), closeStream, mapEvent)
import           Data.Morpheus.Types.Internal.Validation    (ResolveT, SchemaValidation)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Morpheus.Types.IO                     (GQLRequest (..), GQLResponse (..))
import           Data.Morpheus.Types.Resolver               (EventContent, GQLRootResolver (..), PubStreamT, SubStreamT)
import           Data.Morpheus.Validation.Validation        (validateRequest)
import           Data.Proxy
import           Data.Typeable                              (Typeable)
import           GHC.Generics

type EventCon event = Eq event

type IntroCon a = (Generic a, ObjectRep (Rep a) DataArguments)

type EncodeCon m a = (Generic a, Typeable a, ObjectFieldResolvers (Rep a) m)

type RootResCon m event query mutation subscription
   = ( Monad m
     , EventCon event
      -- Introspection
     , IntroCon query
     , IntroCon mutation
     , IntroCon subscription
     -- Resolving
     , EncodeCon m query
     , EncodeCon (PubStreamT m event) mutation
     , EncodeCon (SubStreamT m event) subscription)

resolveByteString :: RootResCon m s a b c => GQLRootResolver m s a b c -> ByteString -> m ByteString
resolveByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ badRequestError aesonError'
    Right req        -> encode <$> resolveStateless rootResolver req

resolveStreamByteString ::
     RootResCon m s a b c => GQLRootResolver m s a b c -> ByteString -> ResponseStream m s ByteString
resolveStreamByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ badRequestError aesonError'
    Right req        -> encode <$> resolveStream rootResolver req

resolveStateless :: RootResCon m s a b c => GQLRootResolver m s a b c -> GQLRequest -> m GQLResponse
resolveStateless root request = snd <$> closeStream (resolveStream root request)

resolveStream ::
     Monad m
  => RootResCon m s a b c =>
       GQLRootResolver m s a b c -> GQLRequest -> ResponseStream m s GQLResponse
resolveStream root@GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver} request =
  case fullSchema root of
    Left error' -> pure $ Errors $ renderErrors error'
    Right validSchema -> do
      value <- runExceptT $ _resolve validSchema
      case value of
        Left x       -> pure $ Errors $ renderErrors x
        Right value' -> pure $ Data value'
  where
    _resolve gqlSchema = (ExceptT $ pure (parseGQL request >>= validateRequest gqlSchema)) >>= ExceptT . execOperator
      where
        execOperator (Query Operator' {operatorSelection}) =
          StreamT $ StreamState [] <$> runExceptT (encodeQuery gqlSchema queryResolver operatorSelection)
        execOperator (Mutation Operator' {operatorSelection}) =
          mapEvent Publish (runExceptT $encodeStreamRes mutationResolver operatorSelection)
        execOperator (Subscription Operator' {operatorSelection}) =
          mapEvent mapTuple (runExceptT $ encodeStreamRes subscriptionResolver operatorSelection)
          where
            mapTuple (x, y) = Subscribe (x, subRes y)
            subRes :: Monad m => (EventContent s -> ResolveT m Value) -> EventContent s -> m GQLResponse
            subRes resolver arg = do
              value <- runExceptT $ resolver arg
              case value of
                Left x  -> pure $ Errors $ renderErrors x
                Right v -> return $ Data v

type Encode m a = ResolveT m a -> SelectionSet -> ResolveT m Value

encodeQuery :: (Monad m, EncodeCon m a) => DataTypeLib -> Encode m a
encodeQuery types rootResolver sel =
  fmap resolversBy rootResolver >>= resolveBySelection sel . (++) (resolversBy $ schemaAPI types)

encodeStreamRes :: (Monad m, EncodeCon m a) => Encode m a
encodeStreamRes rootResolver sel = rootResolver >>= resolveBySelection sel . resolversBy

packStream ::
     EventCon s => GQLState IO s -> (ByteString -> ResponseStream IO s ByteString) -> ByteString -> IO ByteString
packStream state streamAPI request = do
  (actions, value) <- closeStream (streamAPI request)
  errors <- mapM execute actions
  case errors of
    [] -> pure value
    _  -> pure value
  where
    execute (Publish updates) = publishUpdates state updates >> pure []
    execute Subscribe {}      = pure [True] -- TODO Better solution

fullSchema ::
     forall m s a b c. (IntroCon a, IntroCon b, IntroCon c)
  => GQLRootResolver m s a b c
  -> SchemaValidation DataTypeLib
fullSchema _ = querySchema >>= mutationSchema >>= subscriptionSchema
  where
    querySchema = resolveTypes (initTypeLib (operatorType (hiddenRootFields ++ fields) "Query")) (schemaTypes : types)
      where
        (fields, types) = unzip $ objectFieldTypes (Proxy :: Proxy (Rep a))
    mutationSchema lib = resolveTypes (lib {mutation = maybeOperator fields "Mutation"}) types
      where
        (fields, types) = unzip $ objectFieldTypes (Proxy :: Proxy (Rep b))
    subscriptionSchema lib = resolveTypes (lib {subscription = maybeOperator fields "Subscription"}) types
      where
        (fields, types) = unzip $ objectFieldTypes (Proxy :: Proxy (Rep c))
     -- maybeOperator :: [a] -> Text -> Maybe (Text, DataType [a])
    maybeOperator []     = const Nothing
    maybeOperator fields = Just . operatorType fields
    -- operatorType :: [a] -> Text -> (Text, DataType [a])
    operatorType typeData typeName =
      (typeName, DataType {typeData, typeName, typeFingerprint = SystemFingerprint typeName, typeDescription = ""})
