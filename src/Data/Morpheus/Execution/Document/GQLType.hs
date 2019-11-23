{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.GQLType
  ( deriveGQLType
  )
where

import           Data.Text                      ( pack
                                                , unpack
                                                )
import           Language.Haskell.TH
import           Data.Semigroup                 ( (<>) )
--
-- MORPHEUS
import           Data.Morpheus.Execution.Internal.Declare
                                                ( tyConArgs )
import           Data.Morpheus.Kind             ( ENUM
                                                , INPUT_OBJECT
                                                , INPUT_UNION
                                                , OBJECT
                                                , SCALAR
                                                , UNION
                                                , WRAPPER
                                                )
import           Data.Morpheus.Types.GQLType    ( GQLType(..)
                                                , TRUE
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( DataTypeKind(..)
                                                , Meta(..)
                                                , isObject
                                                , isSchemaTypeName
                                                , GQLTypeD(..)
                                                , TypeD(..)
                                                )
import           Data.Morpheus.Types.Internal.TH
                                                ( instanceHeadT
                                                , typeT
                                                , typeInstanceDec
                                                , instanceProxyFunD
                                                )
import           Data.Typeable                  ( Typeable )

genTypeName :: String -> String
genTypeName ('S' : name) | isSchemaTypeName (pack name) = name
genTypeName name = name

deriveGQLType :: GQLTypeD -> Q [Dec]
deriveGQLType GQLTypeD { typeD = TypeD { tName, tMeta }, typeKindD } =
  pure <$> instanceD (cxt constrains) iHead (functions <> typeFamilies)
 where
  functions = map
    instanceProxyFunD
    [ ('__typeName , [|pack (genTypeName tName)|])
    , ('description, descriptionValue)
    ]
   where
    descriptionValue = case tMeta >>= metaDescription of
      Nothing -> [| Nothing  |]
      Just x  -> [| Just (pack desc)|] where desc = unpack x
  -------------------------------------------------
  typeArgs   = tyConArgs typeKindD
  ----------------------------------------------
  iHead      = instanceHeadT ''GQLType tName typeArgs
  headSig    = typeT (mkName tName) typeArgs
  -----------------------------------------------
  constrains = map conTypeable typeArgs
    where conTypeable name = typeT ''Typeable [name]
  -----------------------------------------------
  typeFamilies | isObject typeKindD = [deriveCUSTOM, deriveKind]
               | otherwise          = [deriveKind]
   where
    deriveCUSTOM = do
      typeN <- headSig
      pure $ typeInstanceDec ''CUSTOM typeN (ConT ''TRUE)
    ---------------------------------------------------------------
    deriveKind = do
      typeN <- headSig
      pure $ typeInstanceDec ''KIND typeN (ConT $ toKIND typeKindD)
    ---------------------------------
    toKIND KindScalar      = ''SCALAR
    toKIND KindEnum        = ''ENUM
    toKIND (KindObject _)  = ''OBJECT
    toKIND KindUnion       = ''UNION
    toKIND KindInputObject = ''INPUT_OBJECT
    toKIND KindList        = ''WRAPPER
    toKIND KindNonNull     = ''WRAPPER
    toKIND KindInputUnion  = ''INPUT_UNION
