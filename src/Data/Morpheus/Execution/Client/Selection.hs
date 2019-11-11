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
                                                , unpack
                                                )
--
-- MORPHEUS
import           Data.Morpheus.Error.Utils      ( globalErrorMessage )
import           Data.Morpheus.Execution.Internal.GraphScanner
                                                ( LibUpdater
                                                , resolveUpdates
                                                )
import           Data.Morpheus.Execution.Internal.Utils
                                                ( nameSpaceType )
import           Data.Morpheus.Types.Internal.AST.Operation
                                                ( DefaultValue
                                                , Operation(..)
                                                , ValidOperation
                                                , Variable(..)
                                                , VariableDefinitions
                                                , getOperationName
                                                , getOperationDataType
                                                )
import           Data.Morpheus.Types.Internal.AST.Selection
                                                ( Selection(..)
                                                , SelectionRec(..)
                                                , SelectionSet
                                                , ValidSelection
                                                )
import           Data.Morpheus.Types.Internal.Data
                                                ( DataField(..)
                                                , DataTyCon(..)
                                                , DataType(..)
                                                , DataTypeKind(..)
                                                , DataTypeLib(..)
                                                , Key
                                                , TypeAlias(..)
                                                , DataEnumValue(..)
                                                , allDataTypes
                                                , lookupType
                                                )
import           Data.Morpheus.Types.Internal.DataD
                                                ( ConsD(..)
                                                , ClientType(..)
                                                , TypeD(..)
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( GQLErrors
                                                , Validation
                                                )
import           Data.Set                       ( fromList
                                                , toList
                                                )

removeDuplicates :: [Text] -> [Text]
removeDuplicates = toList . fromList

compileError :: Text -> GQLErrors
compileError x =
  globalErrorMessage $ "Unhandled Compile Time Error: \"" <> x <> "\" ;"

operationTypes
  :: DataTypeLib
  -> VariableDefinitions
  -> ValidOperation
  -> Validation (Maybe TypeD, [ClientType])
operationTypes lib variables = genOperation
 where
  genOperation operation@Operation { operationName, operationSelection } = do
    datatype            <- DataObject <$> getOperationDataType operation lib
    (queryTypes, enums) <- genRecordType []
                                         (getOperationName operationName)
                                         datatype
                                         operationSelection
    inputTypeRequests <- resolveUpdates []
      $ map (scanInputTypes lib . variableType . snd) variables
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
      { tName      = unpack argsName
      , tNamespace = []
      , tCons      = [ ConsD { cName   = unpack argsName
                             , cFields = map fieldD variables
                             }
                     ]
      , tMeta      = Nothing
      }
     where
      fieldD :: (Text, Variable DefaultValue) -> DataField
      fieldD (key, Variable { variableType, variableTypeWrappers }) = DataField
        { fieldName     = key
        , fieldArgs     = []
        , fieldArgsType = Nothing
        , fieldType     = TypeAlias { aliasWrappers = variableTypeWrappers
                                    , aliasTyCon    = variableType
                                    , aliasArgs     = Nothing
                                    }
        , fieldMeta     = Nothing
        }
  ---------------------------------------------------------
  -- generates selection Object Types
  genRecordType
    :: [Key]
    -> Key
    -> DataType
    -> SelectionSet
    -> Validation ([ClientType], [Text])
  genRecordType path name dataType recordSelSet = do
    (con, subTypes, requests) <- genConsD (unpack name) dataType recordSelSet
    pure
      ( ClientType
          { clientType = TypeD { tName
                               , tNamespace = map unpack path
                               , tCons      = [con]
                               , tMeta      = Nothing
                               }
          , clientKind = KindObject Nothing
          }
        : subTypes
      , requests
      )
   where
    tName = unpack name
    genConsD
      :: String
      -> DataType
      -> SelectionSet
      -> Validation (ConsD, [ClientType], [Text])
    genConsD cName datatype selSet = do
      cFields              <- traverse genField selSet
      (subTypes, requests) <- newFieldTypes datatype selSet
      pure (ConsD { cName, cFields }, concat subTypes, concat requests)
     where
      genField :: (Text, ValidSelection) -> Validation DataField
      genField (fieldName, sel@Selection { selectionAlias }) = genFieldD sel
       where
        fieldPath = path <> [fromMaybe fieldName selectionAlias]
        -------------------------------
        genFieldD Selection { selectionAlias = Just aliasFieldName } = do
          fieldType <- snd <$> lookupFieldType lib fieldPath datatype fieldName
          pure $ DataField { fieldName     = aliasFieldName
                           , fieldArgs     = []
                           , fieldArgsType = Nothing
                           , fieldType
                           , fieldMeta     = Nothing
                           }
        genFieldD _ = do
          fieldType <- snd <$> lookupFieldType lib fieldPath datatype fieldName
          pure $ DataField { fieldName
                           , fieldArgs     = []
                           , fieldArgsType = Nothing
                           , fieldType
                           , fieldMeta     = Nothing
                           }
      ------------------------------------------------------------------------------------------------------------
      newFieldTypes
        :: DataType -> SelectionSet -> Validation ([[ClientType]], [[Text]])
      newFieldTypes parentType seSet = unzip <$> mapM valSelection seSet
       where
        valSelection (key, selection@Selection { selectionAlias }) = do
          fieldDatatype <- fst <$> lookupFieldType lib fieldPath parentType key
          validateSelection fieldDatatype selection

         where
          fieldPath = path <> [fromMaybe key selectionAlias]
          --------------------------------------------------------------------
          validateSelection
            :: DataType -> ValidSelection -> Validation ([ClientType], [Text])
          validateSelection dType Selection { selectionRec = SelectionField } =
            leafType dType
          --withLeaf buildLeaf dType
          validateSelection dType Selection { selectionRec = SelectionSet selectionSet }
            = genRecordType fieldPath (typeFrom [] dType) dType selectionSet
          ---- UNION
          validateSelection dType Selection { selectionRec = UnionSelection unionSelections }
            = do
              (tCons, subTypes, requests) <-
                unzip3 <$> mapM getUnionType unionSelections
              pure
                ( ClientType
                    { clientType = TypeD { tNamespace = map unpack fieldPath
                                         , tName = unpack $ typeFrom [] dType
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
              genConsD (unpack selectedTyName) conDatatype selectionVariant

scanInputTypes :: DataTypeLib -> Key -> LibUpdater [Key]
scanInputTypes lib name collected | name `elem` collected = pure collected
                                  | otherwise = getType lib name >>= scanType
 where
  scanType (DataInputObject DataTyCon { typeData }) = resolveUpdates
    (name : collected)
    (map toInputTypeD typeData)
   where
    toInputTypeD :: (Text, DataField) -> LibUpdater [Key]
    toInputTypeD (_, DataField { fieldType = TypeAlias { aliasTyCon } }) =
      scanInputTypes lib aliasTyCon
  scanType (DataEnum DataTyCon { typeName }) = pure (collected <> [typeName])
  scanType _ = pure collected

buildInputType :: DataTypeLib -> Text -> Validation [ClientType]
buildInputType lib name = getType lib name >>= subTypes
 where
  subTypes (DataInputObject DataTyCon { typeName, typeData }) = do
    fields <- traverse toFieldD typeData
    pure
      [ ClientType
          { clientType =
            TypeD
              { tName      = unpack typeName
              , tNamespace = []
              , tCons = [ConsD { cName = unpack typeName, cFields = fields }]
              , tMeta      = Nothing
              }
          , clientKind = KindInputObject
          }
      ]

   where
    toFieldD :: (Text, DataField) -> Validation DataField
    toFieldD (_, field@DataField { fieldType }) = do
      aliasTyCon <- typeFrom [] <$> getType lib (aliasTyCon fieldType)
      pure $ field { fieldType = fieldType { aliasTyCon } }
  subTypes (DataEnum DataTyCon { typeName, typeData }) = pure
    [ ClientType
        { clientType = TypeD { tName      = unpack typeName
                             , tNamespace = []
                             , tCons      = map enumOption typeData
                             , tMeta      = Nothing
                             }
        , clientKind = KindEnum
        }
    ]
   where
    enumOption DataEnumValue { enumName } =
      ConsD { cName = unpack enumName, cFields = [] }
  subTypes _ = pure []

lookupFieldType
  :: DataTypeLib
  -> [Key]
  -> DataType
  -> Text
  -> Validation (DataType, TypeAlias)
lookupFieldType lib path (DataObject DataTyCon { typeData }) key =
  case lookup key typeData of
    Just DataField { fieldType = alias@TypeAlias { aliasTyCon } } ->
      trans <$> getType lib aliasTyCon
     where
      trans x =
        (x, alias { aliasTyCon = typeFrom path x, aliasArgs = Nothing })
    Nothing ->
      Left (compileError $ "cant find field \"" <> pack (show typeData) <> "\"")
lookupFieldType _ _ dt _ =
  Left (compileError $ "Type should be output Object \"" <> pack (show dt))


leafType :: DataType -> Validation ([ClientType], [Text])
leafType (DataEnum DataTyCon { typeName }) = pure ([], [typeName])
leafType DataScalar{} = pure ([], [])
leafType _ = Left $ compileError "Invalid schema Expected scalar"

getType :: DataTypeLib -> Text -> Validation DataType
getType lib typename =
  lookupType (compileError typename) (allDataTypes lib) typename

typeFromScalar :: Text -> Text
typeFromScalar "Boolean" = "Bool"
typeFromScalar "Int"     = "Int"
typeFromScalar "Float"   = "Float"
typeFromScalar "String"  = "Text"
typeFromScalar "ID"      = "ID"
typeFromScalar _         = "ScalarValue"

typeFrom :: [Key] -> DataType -> Text
typeFrom _ (DataScalar DataTyCon { typeName }) = typeFromScalar typeName
typeFrom _ (DataEnum x) = typeName x
typeFrom _ (DataInputObject x) = typeName x
typeFrom path (DataObject x) = pack $ nameSpaceType path $ typeName x
typeFrom path (DataUnion x) = pack $ nameSpaceType path $ typeName x
typeFrom _ (DataInputUnion x) = typeName x
