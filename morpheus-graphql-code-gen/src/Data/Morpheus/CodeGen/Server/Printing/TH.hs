{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Printing.TH
  ( compileDocument,
    gqlDocument,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Morpheus.CodeGen.Internal.AST
  ( AssociatedType (..),
    CodeGenTypeName (..),
    MethodArgument (ProxyArgument),
    TypeClassInstance (..),
  )
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
    GQLDirectiveTypeClass (..),
    GQLTypeDefinition (..),
    InterfaceDefinition (..),
    ServerDeclaration (..),
    ServerDirectiveUsage,
    ServerMethod (..),
  )
import Data.Morpheus.CodeGen.Server.Interpreting.Transform
  ( parseServerTypeDefinitions,
  )
import Data.Morpheus.CodeGen.TH
  ( PrintExp (..),
    ToName (..),
    apply,
    m',
    m_,
    printDec,
    printTypeSynonym,
  )
import Data.Morpheus.Server.Types
  ( GQLDirective (..),
    GQLType (..),
    TypeGuard (..),
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
      quoteDec = compileDocument ctx . pack
    }
  where
    notHandled things =
      error $ things <> " are not supported by the GraphQL QuasiQuoter"

compileDocument :: CodeGenConfig -> ByteString -> Q [Dec]
compileDocument ctx = parseServerTypeDefinitions ctx >=> printDecQ

class PrintDecQ a where
  printDecQ :: a -> Q [Dec]

instance PrintDecQ a => PrintDecQ [a] where
  printDecQ = fmap concat . traverse printDecQ

instance PrintDecQ InterfaceDefinition where
  printDecQ InterfaceDefinition {..} =
    pure [printTypeSynonym aliasName [m_] (apply ''TypeGuard [apply interfaceName [m'], apply unionName [m']])]

instance PrintDecQ GQLTypeDefinition where
  printDecQ GQLTypeDefinition {..} =
    pure
      <$> printDec
        TypeClassInstance
          { typeClassName = ''GQLType,
            typeClassContext = map ((''Typeable,) . toName) (typeParameters gqlTarget),
            typeClassTarget = gqlTarget,
            assoc = [(''KIND, AssociatedTypeName (toName gqlKind))],
            typeClassMethods =
              [ ('defaultValues, ProxyArgument, ServerMethod [|gqlTypeDefaultValues|]),
                ('directives, ProxyArgument, ServerMethod $ printDirectiveUsages gqlTypeDirectiveUses)
              ]
          }

instance PrintDecQ ServerDeclaration where
  printDecQ (InterfaceType interface) = printDecQ interface
  printDecQ ScalarType {} = pure []
  printDecQ (DataType dataType) = pure <$> printDec dataType
  printDecQ (GQLTypeInstance gql) = printDecQ gql
  printDecQ (GQLDirectiveInstance dir) = printDecQ dir

instance PrintDecQ GQLDirectiveTypeClass where
  printDecQ GQLDirectiveTypeClass {..} = do
    pure
      <$> printDec
        ( TypeClassInstance
            { typeClassName = ''GQLDirective,
              typeClassContext = [],
              typeClassTarget = directiveTypeName,
              assoc = [(''DIRECTIVE_LOCATIONS, AssociatedLocations directiveLocations)],
              typeClassMethods = []
            } ::
            TypeClassInstance ServerMethod
        )

printDirectiveUsages :: [ServerDirectiveUsage] -> ExpQ
printDirectiveUsages = foldr (appE . appE [|(<>)|] . printExp) [|mempty|]
