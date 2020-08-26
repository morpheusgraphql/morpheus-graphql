{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.TH.Declare.Introspect
  ( instanceIntrospect,
  )
where

-- MORPHEUS
import Data.Morpheus.Internal.TH
  ( _',
    _2',
    apply,
    applyVars,
    cat',
    funDSimple,
    toCon,
    toVarT,
    tyConArgs,
  )
import Data.Morpheus.Internal.Utils
  ( concatUpdates,
  )
import Data.Morpheus.Server.Deriving.Introspect
  ( Introspect (..),
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.Internal.TH.Utils
  ( mkTypeableConstraints,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (__typeName, implements),
    TypeUpdater,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    CONST,
    ConsD (..),
    FieldContent (..),
    FieldDefinition (..),
    IN,
    LEAF,
    OUT,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    insertType,
    unsafeFromFields,
  )
import Data.Proxy (Proxy (..))
import Language.Haskell.TH

instanceIntrospect :: Maybe (TypeDefinition cat s) -> Q [Dec]
instanceIntrospect
  ( Just
      typeDef@TypeDefinition
        { typeName,
          typeContent = DataEnum {}
        }
    ) =
    pure <$> instanceD (cxt []) iHead [defineIntrospect]
    where
      iHead = pure (apply ''Introspect [cat', toCon typeName])
      defineIntrospect = funDSimple 'introspect [_'] [|insertType (typeDef :: TypeDefinition LEAF CONST)|]
instanceIntrospect _ = pure []
