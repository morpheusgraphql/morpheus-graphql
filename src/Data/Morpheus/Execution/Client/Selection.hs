{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Execution.Client.Selection
  ( operationTypes
  )
where

import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text
                                                , pack
                                                )
--
-- MORPHEUS
import           Data.Morpheus.Error.Client.Client
                                                ( deprecatedField )
import           Data.Morpheus.Error.Utils      ( globalErrorMessage )
import           Data.Morpheus.Execution.Internal.Utils
                                                ( nameSpaceType )
import           Data.Morpheus.Types.Internal.AST
                                                ( Operation(..)
                                                , Key
                                                , Name
                                                , RAW
                                                , ValidOperation
                                                , Variable(..)
                                                , VariableDefinitions
                                                , Selection(..)
                                                , SelectionContent(..)
                                                , ValidSelectionSet
                                                , ValidSelection
                                                , Ref(..)
                                                , FieldDefinition(..)
                                                , DataTypeContent(..)
                                                , DataType(..)
                                                , DataTypeKind(..)
                                                , Schema(..)
                                                , TypeRef(..)
                                                , DataEnumValue(..)
                                                , Listable(..)
                                                , Selectable(..)
                                                , ConsD(..)
                                                , ClientType(..)
                                                , TypeD(..)
                                                , ArgumentsDefinition(..)
                                                , getOperationName
                                                , getOperationDataType
                                                , lookupDeprecated
                                                , lookupDeprecatedReason
                                                , removeDuplicates
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLErrors
                                                , Validation
                                                , Failure(..)
                                                , Result(..)
                                                , Position
                                                , LibUpdater
                                                , resolveUpdates
                                                )


compileError :: Text -> GQLErrors
compileError x =
  globalErrorMessage $ "Unhandled Compile Time Error: \"" <> x <> "\" ;"

operationTypes
  :: Schema
  -> VariableDefinitions
  -> ValidOperation
  -> Validation (Maybe TypeD, [ClientType])
operationTypes lib variables = genOperation
 where
  genOperation operation@Operation { operationName, operationSelection } = do
    datatype            <- getOperationDataType operation lib
    (queryTypes, enums) <- genRecordType []
                                         (getOperationName operationName)
                                         datatype
                                         operationSelection
    inputTypeRequests <- resolveUpdates []
      $ map (scanInputTypes lib . typeConName . variableType . snd) variables
    inputTypesAndEnums <- buildListedTypes (inputTypeRequests <> enums)
    pure
      ( rootArguments (getOperationName operationName <> "Args")
      , queryTypes <> inputTypesAndEnums
      )
  -------------------------------------------------------------------------
  buildListedTypes =
    fmap concat . traverse (buildInputType lib) . removeDuplicates
  -------------------------------------------------------------------------
  -- generates argument types for Operation Head
  rootArguments :: Text -> Maybe TypeD
  rootArguments argsName | null variables = Nothing
                         | otherwise      = Just rootArgumentsType

   where
    rootArgumentsType :: TypeD
    rootArgumentsType = TypeD
      { tName      = argsName
      , tNamespace = []
      , tCons = [ConsD { cName = argsName, cFields = map fieldD variables }]
      , tMeta      = Nothing
      }
     where
      fieldD :: (Text, Variable RAW) -> FieldDefinition
      fieldD (key, Variable { variableType }) = FieldDefinition
        { fieldName     = key
        , fieldArgs     = NoArguments
        , fieldType     = variableType
        , fieldMeta     = Nothing
        }
  ---------------------------------------------------------
  -- generates selection Object Types
  genRecordType
    :: [Name]
    -> Name
    -> DataType
    -> ValidSelectionSet
    -> Validation ([ClientType], [Name])
  genRecordType path tName dataType recordSelSet = do
    (con, subTypes, requests) <- genConsD tName dataType recordSelSet
    pure
      ( ClientType
          { clientType = TypeD { tName
                               , tNamespace = path
                               , tCons      = [con]
                               , tMeta      = Nothing
                               }
          , clientKind = KindObject Nothing
          }
        : subTypes
      , requests
      )
   where
    genConsD
      :: Name
      -> DataType
      -> ValidSelectionSet
      -> Validation (ConsD, [ClientType], [Text])
    genConsD cName datatype selSet = do
      (cFields, subTypes, requests) <- unzip3 <$> traverse genField selSet
      pure (ConsD { cName, cFields }, concat subTypes, concat requests)
     where
      genField
        :: (Text, ValidSelection)
        -> Validation (FieldDefinition, [ClientType], [Text])
      genField (fName, sel@Selection { selectionAlias, selectionPosition }) =
        do
          (fieldDataType, fieldType) <- lookupFieldType lib
                                                        fieldPath
                                                        datatype
                                                        selectionPosition
                                                        fName
          (subTypes, requests) <- subTypesBySelection fieldDataType sel
          pure
            ( FieldDefinition { fieldName
                        , fieldArgs     = NoArguments
                        , fieldType
                        , fieldMeta     = Nothing
                        }
            , subTypes
            , requests
            )
       where
        fieldPath = path <> [fieldName]
        -------------------------------
        fieldName = fromMaybe fName selectionAlias
        ------------------------------------------
        subTypesBySelection
          :: DataType -> ValidSelection -> Validation ([ClientType], [Text])
        subTypesBySelection dType Selection { selectionContent = SelectionField }
          = leafType dType
          --withLeaf buildLeaf dType
        subTypesBySelection dType Selection { selectionContent = SelectionSet selectionSet }
          = genRecordType fieldPath (typeFrom [] dType) dType selectionSet
          ---- UNION
        subTypesBySelection dType Selection { selectionContent = UnionSelection unionSelections }
          = do
            (tCons, subTypes, requests) <-
              unzip3 <$> mapM getUnionType unionSelections
            pure
              ( ClientType
                  { clientType = TypeD { tNamespace = fieldPath
                                       , tName      = typeFrom [] dType
                                       , tCons
                                       , tMeta      = Nothing
                                       }
                  , clientKind = KindUnion
                  }
                : concat subTypes
              , concat requests
              )
         where
          getUnionType (selectedTyName, selectionVariant) = do
            conDatatype <- getType lib selectedTyName
            genConsD selectedTyName conDatatype selectionVariant

scanInputTypes :: Schema -> Key -> LibUpdater [Key]
scanInputTypes lib name collected | name `elem` collected = pure collected
                                  | otherwise = getType lib name >>= scanInpType
 where
  scanInpType DataType { typeContent, typeName } = scanType typeContent
   where
    scanType (DataInputObject fields) = resolveUpdates
      (name : collected) (map toInputTypeD $ toList fields)
     where
      toInputTypeD :: (Text, FieldDefinition) -> LibUpdater [Key]
      toInputTypeD (_, FieldDefinition { fieldType = TypeRef { typeConName } }) =
        scanInputTypes lib typeConName
    scanType (DataEnum _) = pure (collected <> [typeName])
    scanType _            = pure collected

buildInputType :: Schema -> Text -> Validation [ClientType]
buildInputType lib name = getType lib name >>= generateTypes
 where
  generateTypes DataType { typeName, typeContent } = subTypes typeContent
   where
    subTypes (DataInputObject inputFields) = do
      fields <- traverse toFieldD (toList inputFields)
      pure
        [ ClientType
            { clientType = TypeD
                             { tName      = typeName
                             , tNamespace = []
                             , tCons      = [ ConsD { cName   = typeName
                                                    , cFields = fields
                                                    }
                                            ]
                             , tMeta      = Nothing
                             }
            , clientKind = KindInputObject
            }
        ]
     where
      toFieldD :: (Text, FieldDefinition) -> Validation (FieldDefinition)
      toFieldD (_, field@FieldDefinition { fieldType }) = do
        typeConName <- typeFrom [] <$> getType lib (typeConName fieldType)
        pure $ field { fieldType = fieldType { typeConName  }  }
    subTypes (DataEnum enumTags) = pure
      [ ClientType
          { clientType = TypeD { tName      = typeName
                               , tNamespace = []
                               , tCons      = map enumOption enumTags
                               , tMeta      = Nothing
                               }
          , clientKind = KindEnum
          }
      ]
     where
      enumOption DataEnumValue { enumName } =
        ConsD { cName = enumName, cFields = [] }
    subTypes _ = pure []

lookupFieldType
  :: Schema
  -> [Key]
  -> DataType
  -> Position
  -> Text
  -> Validation (DataType, TypeRef)
lookupFieldType lib path DataType { typeContent = DataObject { objectFields }, typeName } refPosition key
  = selectBy selError key objectFields >>= processDeprecation
  where
    selError = compileError $ "cant find field \"" <> pack (show objectFields) <> "\""
    processDeprecation FieldDefinition { fieldType = alias@TypeRef { typeConName }, fieldMeta } = 
      checkDeprecated >> (trans <$> getType lib typeConName)
     where
      trans x =
        (x, alias { typeConName = typeFrom path x, typeArgs = Nothing })
      ------------------------------------------------------------------
      checkDeprecated :: Validation ()
      checkDeprecated = case fieldMeta >>= lookupDeprecated of
        Just deprecation -> Success { result = (), warnings, events = [] }
         where
          warnings = deprecatedField typeName
                                     Ref { refName = key, refPosition }
                                     (lookupDeprecatedReason deprecation)
        Nothing -> pure ()
lookupFieldType _ _ dt _ _ =
  failure (compileError $ "Type should be output Object \"" <> pack (show dt))


leafType :: DataType -> Validation ([ClientType], [Text])
leafType DataType { typeName, typeContent } = fromKind typeContent
 where
  fromKind :: DataTypeContent -> Validation ([ClientType], [Text])
  fromKind DataEnum{} = pure ([], [typeName])
  fromKind DataScalar{} = pure ([], [])
  fromKind _ = failure $ compileError "Invalid schema Expected scalar"

getType :: Schema -> Text -> Validation DataType
getType lib typename = selectBy (compileError typename) typename lib 

typeFromScalar :: Name -> Name
typeFromScalar "Boolean" = "Bool"
typeFromScalar "Int"     = "Int"
typeFromScalar "Float"   = "Float"
typeFromScalar "String"  = "Text"
typeFromScalar "ID"      = "ID"
typeFromScalar _         = "ScalarValue"

typeFrom :: [Name] -> DataType -> Name
typeFrom path DataType { typeName, typeContent } = __typeFrom typeContent
 where
  __typeFrom DataScalar{} = typeFromScalar typeName
  __typeFrom DataObject{} = nameSpaceType path typeName
  __typeFrom DataUnion{}  = nameSpaceType path typeName
  __typeFrom _            = typeName
