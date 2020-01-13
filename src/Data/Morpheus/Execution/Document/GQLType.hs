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
                                                , SCALAR
                                                , WRAPPER
                                                , INPUT
                                                , OUTPUT
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
                                                , Key
                                                )
import           Data.Morpheus.Types.Internal.TH
                                                ( instanceHeadT
                                                , typeT
                                                , typeInstanceDec
                                                , instanceProxyFunD
                                                )
import           Data.Typeable                  ( Typeable )

deriveGQLType :: GQLTypeD -> Q [Dec]
deriveGQLType GQLTypeD { typeD = TypeD { tName, tMeta }, typeKindD } =
  pure <$> instanceD (cxt constrains) iHead (functions <> typeFamilies)
 where
  functions = map
    instanceProxyFunD
    [('__typeName, [|toHSTypename tName|]), ('description, descriptionValue)]
   where
    descriptionValue = case tMeta >>= metaDescription of
      Nothing   -> [| Nothing   |]
      Just desc -> [| Just desc |]
  --------------------------------
  typeArgs   = tyConArgs typeKindD
  --------------------------------
  iHead      = instanceHeadT ''GQLType tName typeArgs
  headSig    = typeT (mkName $ unpack tName) typeArgs
  ---------------------------------------------------
  constrains = map conTypeable typeArgs
   where conTypeable name = typeT ''Typeable [name]
  -------------------------------------------------
  typeFamilies | isObject typeKindD = [deriveCUSTOM, deriveKind]
               | otherwise          = [deriveKind]
   where
    deriveCUSTOM = do
      typeN <- headSig
      pure $ typeInstanceDec ''CUSTOM typeN (ConT ''TRUE)
    -----------------------------------------------------
    deriveKind = do
      typeN <- headSig
      pure $ typeInstanceDec ''KIND typeN (ConT $ kindName typeKindD)


kindName :: DataTypeKind -> Name
kindName KindObject {}  = ''OUTPUT
kindName KindScalar      = ''SCALAR
kindName KindEnum        = ''ENUM
kindName KindUnion       = ''OUTPUT
kindName KindInputObject = ''INPUT
kindName KindList        = ''WRAPPER
kindName KindNonNull     = ''WRAPPER
kindName KindInputUnion  = ''INPUT

toHSTypename :: Key -> Key
toHSTypename = pack . hsTypename . unpack
 where
  hsTypename ('S' : name) | isSchemaTypeName (pack name) = name
  hsTypename name = name