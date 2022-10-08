{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.TH.Type
  ( compileDocument,
    gqlDocument,
  )
where

import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
    DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
    ServerConstructorDefinition (..),
    ServerFieldDefinition (..),
    ServerTypeDefinition (..),
    unpackName,
  )
import Data.Morpheus.CodeGen.Server.Interpreting.Transform (parseServerTypeDefinitions)
import Data.Morpheus.CodeGen.Server.TH.GQLDirective (deriveGQLDirective)
import Data.Morpheus.CodeGen.Server.TH.GQLType (deriveGQLType)
import Data.Morpheus.CodeGen.Server.TH.Utils
  ( ServerDec,
    m',
    m_,
    renderTypeVars,
  )
import Data.Morpheus.CodeGen.TH
  ( apply,
    declareTypeRef,
    toCon,
    toName,
    toTypeVars,
    wrappedType,
  )
import Data.Morpheus.Server.Types
  ( Arg,
    SubscriptionField,
    TypeGuard,
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeKind (..),
  )
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Relude hiding (ByteString, Type)

gqlDocument :: QuasiQuoter
gqlDocument = mkQuasiQuoter CodeGenConfig {namespace = False}

mkQuasiQuoter :: CodeGenConfig -> QuasiQuoter
mkQuasiQuoter ctx =
  QuasiQuoter
    { quoteExp = notHandled "Expressions",
      quotePat = notHandled "Patterns",
      quoteType = notHandled "Types",
      quoteDec = compileDocument ctx . LB.pack
    }
  where
    notHandled things =
      error $ things <> " are not supported by the GraphQL QuasiQuoter"

compileDocument :: CodeGenConfig -> LB.ByteString -> Q [Dec]
compileDocument ctx = parseServerTypeDefinitions ctx >=> runDeclare ctx

runDeclare :: Declare a => CodeGenConfig -> a -> Q [Dec]
runDeclare ctx a = runReaderT (declare a) ctx

class Declare a where
  declare :: a -> ServerDec [Dec]

instance Declare a => Declare [a] where
  declare = fmap concat . traverse declare

instance Declare ServerTypeDefinition where
  declare typeDef = do
    let typeDecs = declareType typeDef
    gqlDirDecs <- deriveGQLDirective typeDef
    gqlTypeDecs <- deriveGQLType typeDef
    pure (typeDecs <> gqlDirDecs <> gqlTypeDecs)

declareType :: ServerTypeDefinition -> [Dec]
declareType (ServerInterfaceDefinition name interfaceName unionName) =
  [ TySynD
      (toName name)
      (toTypeVars [m_])
      (apply ''TypeGuard [apply interfaceName [m'], apply unionName [m']])
  ]
declareType ServerTypeDefinition {tKind = KindScalar} = []
declareType ServerTypeDefinition {..} = [DataD [] (toName tName) vars Nothing cons [derivingClause]]
  where
    derivingClause = DerivClause Nothing (map (ConT . genName) derives)
    cons = map declareCons tCons
    vars = toTypeVars (renderTypeVars typeParameters)
declareType
  DirectiveTypeDefinition {..} =
    [DataD [] name [] Nothing [declareCons directiveConstructor] [derivingClause]]
    where
      name = toName (constructorName directiveConstructor)
      derivingClause = DerivClause Nothing (map (ConT . genName) directiveDerives)

genName :: DerivingClass -> Name
genName GENERIC = ''Generic
genName SHOW = ''Show

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
