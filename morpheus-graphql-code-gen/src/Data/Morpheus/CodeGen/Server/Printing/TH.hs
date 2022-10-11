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

import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Morpheus.CodeGen.Internal.AST (CodeGenTypeName (..))
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
    GQLDirectiveTypeClass (..),
    GQLTypeDefinition (..),
    InterfaceDefinition (..),
    ServerDeclaration (..),
    ServerDirectiveUsage,
    TypeKind,
  )
import Data.Morpheus.CodeGen.Server.Interpreting.Transform
  ( parseServerTypeDefinitions,
  )
import Data.Morpheus.CodeGen.TH
  ( PrintExp (..),
    PrintType (..),
    ToName (..),
    apply,
    m',
    m_,
    printDec,
    printTypeClass,
    printTypeSynonym,
    toCon,
    _',
  )
import Data.Morpheus.Server.Types
  ( GQLDirective (..),
    GQLType (..),
    TypeGuard (..),
    dropNamespaceOptions,
  )
import Data.Morpheus.Types.Internal.AST (DirectiveLocation)
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
compileDocument ctx = parseServerTypeDefinitions ctx >=> printDecQ

class PrintDecQ a where
  printDecQ :: a -> Q [Dec]

instance PrintDecQ a => PrintDecQ [a] where
  printDecQ = fmap concat . traverse printDecQ

instance PrintDecQ InterfaceDefinition where
  printDecQ InterfaceDefinition {..} =
    pure [printTypeSynonym aliasName [m_] (apply ''TypeGuard [apply interfaceName [m'], apply unionName [m']])]

instance PrintDecQ GQLTypeDefinition where
  printDecQ GQLTypeDefinition {..} = do
    let params = map toName (typeParameters gqlTarget)
    associatedTypes <- fmap (pure . (''KIND,)) (printType gqlKind)
    pure <$> printTypeClass (map (''Typeable,) params) ''GQLType (printType gqlTarget) associatedTypes methods
    where
      methods =
        [ ('defaultValues, [_'], [|gqlTypeDefaultValues|]),
          ('directives, [_'], printDirectiveUsages gqlTypeDirectiveUses)
        ]
          <> map printTypeOptions (maybeToList dropNamespace)

instance PrintDecQ ServerDeclaration where
  printDecQ (InterfaceType interface) = printDecQ interface
  printDecQ ScalarType {} = pure []
  printDecQ (DataType dataType) = pure [printDec dataType]
  printDecQ (GQLTypeInstance gql) = printDecQ gql
  printDecQ (GQLDirectiveInstance dir) = printDecQ dir

instance PrintDecQ GQLDirectiveTypeClass where
  printDecQ GQLDirectiveTypeClass {..} =
    pure
      <$> printTypeClass
        []
        ''GQLDirective
        (toCon directiveTypeName)
        [(''DIRECTIVE_LOCATIONS, promotedList directiveLocations)]
        []

promotedList :: [DirectiveLocation] -> Type
promotedList = foldr (AppT . AppT PromotedConsT . PromotedT . toName) PromotedNilT

printTypeOptions :: (TypeKind, Text) -> (Name, [PatQ], ExpQ)
printTypeOptions (kind, tName) = ('typeOptions, [_'], [|dropNamespaceOptions kind tName|])

printDirectiveUsages :: [ServerDirectiveUsage] -> ExpQ
printDirectiveUsages = foldr (appE . appE [|(<>)|] . printExp) [|mempty|]
