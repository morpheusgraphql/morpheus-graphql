{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.CodeGen.Interpreting.Core
  ( LocalM,
    compileError,
    getType,
    typeFrom,
    deprecationWarning,
    printClientType,
    defaultDerivations,
    warning,
    LocalContext (..),
    withPosition,
    getNameByPath,
    registerFragment,
    existFragment,
    removeDuplicates,
    clientConfig,
    lookupField,
    lookupType,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Morpheus.Client.CodeGen.AST
  ( ClientDeclaration (..),
    ClientTypeDefinition (..),
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenType (..),
    CodeGenTypeName (..),
    DerivingClass (..),
    TypeClassInstance (..),
    fromTypeName,
  )
import Data.Morpheus.CodeGen.Utils
  ( CodeGenT,
  )
import Data.Morpheus.Core (Config (..), VALIDATION_MODE (WITHOUT_VARIABLES))
import Data.Morpheus.Internal.Ext
  ( Result (..),
  )
import Data.Morpheus.Internal.Utils
  ( empty,
    selectBy,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    Description,
    Directives,
    FieldDefinition (..),
    FieldName,
    FragmentName,
    GQLError,
    Msg,
    OUT,
    Position,
    RAW,
    Schema (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    VALID,
    VariableDefinitions,
    internal,
    lookupDataType,
    lookupDeprecated,
    lookupDeprecatedReason,
    mkTypeRef,
    msg,
  )
import Data.Set (insert, member)
import Relude hiding (empty)

clientConfig :: Config
clientConfig =
  Config
    { debug = False,
      introspection = True,
      validationMode = WITHOUT_VARIABLES
    }

data LocalContext = LocalContext
  { ctxSchema :: Schema VALID,
    ctxVariables :: VariableDefinitions RAW,
    ctxPosition :: Maybe Position,
    ctxFragments :: Set FragmentName
  }

getKey :: ClientDeclaration -> String
getKey (InstanceDeclaration _ x) = show (typeClassName x) <> show (typeClassTarget x)
getKey (ClientTypeDeclaration x) = show x

removeDuplicates :: [ClientDeclaration] -> [ClientDeclaration]
removeDuplicates = collect []
  where
    collect seen [] = seen
    collect seen (x : xs)
      | getKey x `elem` map getKey seen = collect seen xs
      | otherwise = collect (seen <> [x]) xs

registerFragment :: FragmentName -> LocalM a -> LocalM a
registerFragment name = local (\ctx -> ctx {ctxFragments = insert name (ctxFragments ctx)})

existFragment :: FragmentName -> LocalM Bool
existFragment name = (name `member`) <$> asks ctxFragments

withPosition :: Position -> LocalM a -> LocalM a
withPosition pos = local (\ctx -> ctx {ctxPosition = Just pos})

type LocalM a = CodeGenT LocalContext (Result GQLError) a

compileError :: GQLError -> GQLError
compileError x = internal $ "Unhandled Compile Time Error: \"" <> x <> "\" ;"

lookupType :: TypeName -> LocalM (Maybe (TypeDefinition ANY VALID))
lookupType name = asks (lookupDataType name . ctxSchema)

getType :: TypeName -> LocalM (TypeDefinition ANY VALID)
getType name = do
  x <- lookupType name
  maybe (throwError $ compileError $ " can't find Type" <> msg name) pure x

typeFrom :: [FieldName] -> TypeDefinition a VALID -> CodeGenTypeName
typeFrom path TypeDefinition {typeName, typeContent} = __typeFrom typeContent
  where
    __typeFrom DataObject {} = getNameByPath path typeName
    __typeFrom DataInterface {} = getNameByPath path typeName
    __typeFrom DataUnion {} = getNameByPath path typeName
    __typeFrom _ = fromTypeName typeName

getNameByPath :: [FieldName] -> TypeName -> CodeGenTypeName
getNameByPath path tName = case reverse path of
  (p : ps) -> CodeGenTypeName {namespace = reverse ps, typeParameters = [], typename = coerce p}
  [] -> CodeGenTypeName {namespace = [], typeParameters = [], typename = tName}

deprecationWarning :: (Maybe Description -> GQLError) -> Directives s -> LocalM ()
deprecationWarning f = traverse_ warning . toList . fmap (f . lookupDeprecatedReason) . lookupDeprecated

warning :: GQLError -> LocalM ()
warning w = lift $ Success {result = (), warnings = [w]}

defaultDerivations :: [DerivingClass]
defaultDerivations = [GENERIC, SHOW, CLASS_EQ]

printClientType :: ClientTypeDefinition -> CodeGenType
printClientType ClientTypeDefinition {..} =
  CodeGenType
    { cgTypeName = clientTypeName,
      cgConstructors = clientCons,
      cgDerivations = defaultDerivations
    }

lookupField :: FieldName -> TypeContent TRUE ANY VALID -> LocalM (FieldDefinition OUT VALID)
lookupField selectionName _
  | selectionName == "__typename" =
      pure
        FieldDefinition
          { fieldName = "__typename",
            fieldDescription = Nothing,
            fieldType = mkTypeRef "String",
            fieldDirectives = empty,
            fieldContent = Nothing
          }
lookupField selectionName x@DataObject {objectFields} = selectBy (selError selectionName x) selectionName objectFields
lookupField selectionName x@DataInterface {interfaceFields} = selectBy (selError selectionName x) selectionName interfaceFields
lookupField _ dt = throwError (compileError $ "Type should be output Object \"" <> msg (show dt :: String))

selError :: (Msg a, Show b) => a -> b -> GQLError
selError selectionName con = compileError $ "can't find field " <> msg selectionName <> " on type: " <> msg (show con :: String)
