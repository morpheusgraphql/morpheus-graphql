{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Declare.Type
  ( declareType,
  )
where

import Data.Morpheus.CodeGen.Internal.AST (DerivingClass (..), FIELD_TYPE_WRAPPER (..), ServerConstructorDefinition (..), ServerFieldDefinition (..), ServerTypeDefinition (..), unpackName)
import Data.Morpheus.CodeGen.Internal.TH
  ( apply,
    declareTypeRef,
    toCon,
    toName,
    wrappedType,
  )
import Data.Morpheus.Server.TH.Utils
  ( m',
    m_,
    renderTypeVars,
  )
import Data.Morpheus.Types
  ( Arg,
    GQLType,
    SubscriptionField,
    TypeGuard,
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeKind (..),
  )
import qualified Data.Text as T
import Language.Haskell.TH
import Relude hiding (Type)

{- ORMOLU_DISABLE -}

declareType :: ServerTypeDefinition -> [Dec]
declareType (ServerInterfaceDefinition name interfaceName unionName) =
  [ TySynD
      (toName name)
#if MIN_VERSION_template_haskell(2,17,0)
      [PlainTV m_ ()]
#else
      [PlainTV m_]
#endif
      (apply ''TypeGuard [apply interfaceName [m'], apply unionName [m']])
  ]
declareType ServerTypeDefinition {tKind = KindScalar} = []
declareType
  ServerTypeDefinition
    { tName,
      tCons,
      derives,
      typeParameters
    } = [DataD [] (toName tName) vars Nothing cons [derivingClause]]
    where
      derivingClause = DerivClause Nothing (map (ConT . genName) derives)
      cons = map declareCons tCons

#if MIN_VERSION_template_haskell(2,17,0)
      vars = map (flip PlainTV ()) (renderTypeVars typeParameters)
#else
      vars = map PlainTV (renderTypeVars typeParameters)
#endif
declareType
  DirectiveTypeDefinition {..} 
    = [DataD [] name [] Nothing [declareCons directiveConstructor] [derivingClause]]
    where
      name = toName (constructorName directiveConstructor)
      derivingClause = DerivClause Nothing (map (ConT . genName) directiveDerives)
{- ORMOLU_ENABLE -}

genName :: DerivingClass -> Name
genName GENERIC = ''Generic
genName SHOW = ''Show
genName GQL_TYPE = ''GQLType

declareCons :: ServerConstructorDefinition -> Con
declareCons ServerConstructorDefinition {constructorName, constructorFields} =
  RecC
    (toName constructorName)
    (map declareField constructorFields)

declareField :: ServerFieldDefinition -> (Name, Bang, Type)
declareField
  ServerFieldDefinition
    { fieldName,
      fieldType,
      wrappers
    } =
    ( toName fieldName,
      Bang NoSourceUnpackedness NoSourceStrictness,
      foldr applyWrapper (toCon fieldType) wrappers
    )

applyWrapper :: FIELD_TYPE_WRAPPER -> Type -> Type
applyWrapper PARAMETRIZED = (`AppT` m')
applyWrapper MONAD = AppT m'
applyWrapper SUBSCRIPTION = AppT (ConT ''SubscriptionField)
applyWrapper (ARG typeName) = InfixT (ConT (toName typeName)) ''Function
applyWrapper (GQL_WRAPPER wrappers) = wrappedType wrappers
applyWrapper (TAGGED_ARG fieldName typeRef) = InfixT arg ''Function
  where
    arg =
      AppT
        ( AppT
            (ConT ''Arg)
            (LitT $ StrTyLit $ T.unpack $ unpackName fieldName)
        )
        (declareTypeRef toCon typeRef)

type Function = (->)
