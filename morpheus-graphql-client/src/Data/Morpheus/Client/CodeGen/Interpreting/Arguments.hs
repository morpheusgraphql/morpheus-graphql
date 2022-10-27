{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.CodeGen.Interpreting.Arguments
  ( genArguments,
  )
where

import Data.Morpheus.Client.CodeGen.AST
  ( ClientPreDeclaration (..),
    DERIVING_MODE (..),
  )
import Data.Morpheus.Client.CodeGen.Interpreting.Core
  ( defaultDerivations,
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenField (..),
    CodeGenType (..),
    FIELD_TYPE_WRAPPER (..),
    fromTypeName,
    getFullName,
  )
import Data.Morpheus.Types.Internal.AST
  ( RAW,
    TypeName,
    TypeRef (..),
    Variable (..),
    VariableDefinitions,
    isNullable,
  )
import Relude hiding (empty, show)

genArguments :: TypeName -> VariableDefinitions RAW -> (TypeName, [ClientPreDeclaration])
genArguments operationTypeName variables
  | null variables = ("()", [])
  | otherwise = (getFullName cgTypeName, [ClientType def, ToJSONClass TYPE_MODE def])
  where
    def =
      CodeGenType
        { cgTypeName,
          cgConstructors =
            [ CodeGenConstructor
                { constructorName = cgTypeName,
                  constructorFields = packAsCodeGenField <$> toList variables
                }
            ],
          cgDerivations = defaultDerivations
        }
    cgTypeName = fromTypeName $ operationTypeName <> "Args"

packAsCodeGenField :: Variable RAW -> CodeGenField
packAsCodeGenField Variable {variableName, variableType = ref@TypeRef {..}} =
  CodeGenField
    { fieldName = variableName,
      fieldType = typeConName,
      wrappers = [GQL_WRAPPER typeWrappers],
      fieldIsNullable = isNullable ref
    }
