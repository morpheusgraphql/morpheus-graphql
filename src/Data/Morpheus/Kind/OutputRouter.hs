{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Kind.OutputRouter where

import           Control.Monad.Trans                        (lift)
import           Control.Monad.Trans.Except
import           Data.Morpheus.Error.Selection              (fieldNotResolved)
import           Data.Morpheus.Generics.DeriveResolvers     (DeriveResolvers (..))
import           Data.Morpheus.Generics.EnumRep             (EnumRep (..))
import           Data.Morpheus.Generics.ObjectRep           (ObjectRep (..), resolveTypes)
import           Data.Morpheus.Generics.UnionRep            (UnionRep (..))
import           Data.Morpheus.Generics.UnionResolvers      (UnionResolvers (..))
import           Data.Morpheus.Generics.Utils               (RecSel, SelOf)
import qualified Data.Morpheus.Kind.GQLArgs                 as Args (GQLArgs (..))
import           Data.Morpheus.Kind.GQLKinds                (Encode_, EnumConstraint, Intro_, OField_, ObjectConstraint,
                                                             UnionConstraint, encodeObject, encodeUnion, introspectEnum,
                                                             introspectObject, introspectUnion)
import qualified Data.Morpheus.Kind.GQLScalar               as S (GQLScalar (..))
import           Data.Morpheus.Kind.GQLType                 (GQLType (..))
import           Data.Morpheus.Kind.Internal                (ENUM, KIND, OBJECT, SCALAR, UNION, WRAPPER)
import           Data.Morpheus.Kind.Utils                   (encodeList, encodeMaybe, listField, maybeField)
import           Data.Morpheus.Schema.TypeKind              (TypeKind (..))
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..))
import           Data.Morpheus.Types.Internal.Data          (DataField (..), DataOutputField)
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.Internal.Value         (ScalarValue (..), Value (..))
import           Data.Morpheus.Types.Resolver               (Resolver (..), Result (..))
import           Data.Proxy                                 (Proxy (..))
import           Data.Text                                  (Text, pack)
import           GHC.Generics

type MResult = Result Value


-- { ENCODE }
class OutputTypeRouter a kind value where
  __encode :: Proxy kind -> Encode_ a value

_encode ::
     forall a v. OutputTypeRouter a (KIND a) v
  => Encode_ a v
_encode = __encode (Proxy @(KIND a))

-- Output Router for Queries
-- Output Router for Mutations and Subscriptions
instance (S.GQLScalar a, GQLType a) => OutputTypeRouter a SCALAR MResult where
  __encode _ _ = pure . pure . S.encode

instance EnumConstraint a => OutputTypeRouter a ENUM MResult where
  __encode _ _ = pure . pure . Scalar . String . encodeRep . from

instance ObjectConstraint a => OutputTypeRouter a OBJECT MResult where
  __encode _ = encodeObject

instance UnionConstraint a => OutputTypeRouter a UNION MResult where
  __encode _ = encodeUnion

instance OutputTypeRouter a (KIND a) MResult => OutputTypeRouter (Maybe a) WRAPPER MResult where
  __encode _ = encodeMaybe _encode

instance OutputTypeRouter a (KIND a) MResult => OutputTypeRouter [a] WRAPPER MResult where
  __encode _ = encodeList _encode

instance (OutputTypeRouter a (KIND a) MResult, Args.GQLArgs p) =>
         OutputTypeRouter (Resolver c p a) WRAPPER MResult where
  __encode _ selection'@(key', Selection {selectionArguments = astArgs', selectionPosition = position'}) (Resolver resolver) = do
    args <- ExceptT $ pure $ Args.decode astArgs'
    Result value1 effects1 <- liftResolver position' key' (resolver args)
    Result value2 effects2 <- _encode selection' value1
    return $ Result value2 (effects1 ++ effects2)

instance OutputTypeRouter a (KIND a) MResult => DeriveResolvers (K1 s a) where
  deriveResolvers key' (K1 src) = [(key', (`_encode` src))]

instance (OutputTypeRouter a OBJECT MResult, ObjectConstraint a) => UnionRep (RecSel s a) where
  possibleTypes _ = [(field_ OBJECT (Proxy @a) () "", introspectObject (Proxy @a))]

instance (GQLType a, OutputTypeRouter a (KIND a) MResult) => UnionResolvers (K1 s a) where
  currentResolver (K1 src) = (typeID (Proxy @a), (`_encode` src))

liftResolver :: Int -> Text -> IO (Either String a) -> ResolveIO a
liftResolver position' typeName' x = do
  result <- lift x
  case result of
    Left message' -> failResolveIO $ fieldNotResolved position' typeName' (pack message')
    Right value   -> pure value
