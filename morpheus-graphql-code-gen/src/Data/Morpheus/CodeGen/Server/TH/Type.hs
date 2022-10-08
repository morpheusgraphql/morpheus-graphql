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
    ServerConstructorDefinition (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.CodeGen.Server.Interpreting.Transform (parseServerTypeDefinitions)
import Data.Morpheus.CodeGen.Server.TH.GQLDirective (deriveGQLDirective)
import Data.Morpheus.CodeGen.Server.TH.GQLType (deriveGQLType)
import Data.Morpheus.CodeGen.Server.TH.Utils
  ( renderTypeVars,
  )
import Data.Morpheus.CodeGen.TH
  ( apply,
    m',
    m_,
    printDerivClause,
    printField,
    toName,
    toTypeVars,
  )
import Data.Morpheus.Server.Types
  ( TypeGuard,
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeKind (..),
  )
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
runDeclare _ = declare

class Declare a where
  declare :: a -> Q [Dec]

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
declareType ServerTypeDefinition {..} = [DataD [] (toName tName) vars Nothing cons [printDerivClause derives]]
  where
    cons = map declareCons tCons
    vars = toTypeVars (renderTypeVars typeParameters)
declareType
  DirectiveTypeDefinition {..} =
    [DataD [] name [] Nothing [declareCons directiveConstructor] [printDerivClause directiveDerives]]
    where
      name = toName (constructorName directiveConstructor)

declareCons :: ServerConstructorDefinition -> Con
declareCons ServerConstructorDefinition {constructorName, constructorFields} =
  RecC
    (toName constructorName)
    (map printField constructorFields)
