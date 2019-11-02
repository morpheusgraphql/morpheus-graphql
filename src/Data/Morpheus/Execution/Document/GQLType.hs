{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.GQLType
  ( deriveGQLType
  ) where

import           Data.Text                                (pack)
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Internal.Declare (tyConArgs)
import           Data.Morpheus.Kind                       (ENUM, INPUT_OBJECT, INPUT_UNION, OBJECT, SCALAR, UNION,
                                                           WRAPPER)
import           Data.Morpheus.Types.GQLType              (GQLType (..), TRUE)
import           Data.Morpheus.Types.Internal.Data        (DataTypeKind (..), isObject, isSchemaTypeName)
import           Data.Morpheus.Types.Internal.DataD       (GQLTypeD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.TH          (instanceHeadT, typeT, typeInstanceDec)
import           Data.Typeable                            (Typeable)

genTypeName :: String -> String
genTypeName ('S':name)
  | isSchemaTypeName (pack name) = name
genTypeName name = name



deriveGQLType :: GQLTypeD -> Q [Dec]
deriveGQLType GQLTypeD {typeD = TypeD {tName}, typeKindD} =
  pure <$> instanceD (cxt constrains) iHead (def__typeName : typeFamilies)
  where
    def__typeName = funD '__typeName [clause argsE (normalB body) []]
      where
        body = [|pack name|]
          where
            name = genTypeName tName
    -- defines method: __typeName _ = tName
    argsE = map (varP . mkName) ["_"]
    typeArgs = tyConArgs typeKindD
    ----------------------------------------------
    iHead = instanceHeadT ''GQLType tName typeArgs
    headSig = typeT (mkName tName) typeArgs
    -----------------------------------------------
    constrains = map conTypeable typeArgs
      where
        conTypeable name = typeT ''Typeable [name]
    -----------------------------------------------
    typeFamilies
      | isObject typeKindD = [deriveCUSTOM, deriveKind]
      | otherwise = [deriveKind]
    ---------------------------------------------
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
