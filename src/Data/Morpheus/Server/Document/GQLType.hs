{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.Document.GQLType
  ( deriveGQLType,
  )
where

--
-- MORPHEUS
import Data.Morpheus.Internal.TH
  ( instanceHeadT,
    instanceProxyFunD,
    tyConArgs,
    typeInstanceDec,
    typeT,
  )
import Data.Morpheus.Kind
  ( ENUM,
    INPUT,
    OUTPUT,
    SCALAR,
    WRAPPER,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    TRUE,
  )
import Data.Morpheus.Types.Internal.AST
  ( DataTypeKind (..),
    GQLTypeD (..),
    Key,
    Meta (..),
    TypeD (..),
    isObject,
    isSchemaTypeName,
  )
import Data.Semigroup ((<>))
import Data.Text
  ( pack,
    unpack,
  )
import Data.Typeable (Typeable)
import Language.Haskell.TH

deriveGQLType :: GQLTypeD -> Q [Dec]
deriveGQLType GQLTypeD {typeD = TypeD {tName, tMeta}, typeKindD} =
  pure <$> instanceD (cxt constrains) iHead (functions <> typeFamilies)
  where
    functions =
      map
        instanceProxyFunD
        [('__typeName, [|toHSTypename tName|]), ('description, descriptionValue)]
      where
        descriptionValue = case tMeta >>= metaDescription of
          Nothing -> [|Nothing|]
          Just desc -> [|Just desc|]
    --------------------------------
    typeArgs = tyConArgs typeKindD
    --------------------------------
    iHead = instanceHeadT ''GQLType tName typeArgs
    headSig = typeT (mkName $ unpack tName) typeArgs
    ---------------------------------------------------
    constrains = map conTypeable typeArgs
      where
        conTypeable name = typeT ''Typeable [name]
    -------------------------------------------------
    typeFamilies
      | isObject typeKindD = [deriveKIND, deriveCUSTOM]
      | otherwise = [deriveKIND]
      where
        deriveCUSTOM = deriveInstance ''CUSTOM ''TRUE
        deriveKIND = deriveInstance ''KIND (kindName typeKindD)
        -------------------------------------------------------
        deriveInstance :: Name -> Name -> Q Dec
        deriveInstance insName tyName = do
          typeN <- headSig
          pure $ typeInstanceDec insName typeN (ConT tyName)

kindName :: DataTypeKind -> Name
kindName KindObject {} = ''OUTPUT
kindName KindScalar = ''SCALAR
kindName KindEnum = ''ENUM
kindName KindUnion = ''OUTPUT
kindName KindInputObject = ''INPUT
kindName KindList = ''WRAPPER
kindName KindNonNull = ''WRAPPER
kindName KindInputUnion = ''INPUT
kindName KindInterface = ''OUTPUT

toHSTypename :: Key -> Key
toHSTypename = pack . hsTypename . unpack
  where
    hsTypename ('S' : name) | isSchemaTypeName (pack name) = name
    hsTypename name = name
