{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Internal.Schema.Internal
  ( CatType (..),
    fromSchema,
    withObject,
    deriveTypeAsArguments,
  )
where

-- MORPHEUS

import Control.Monad.Except (throwError)
import Data.Morpheus.Internal.Ext
  ( GQLResult,
    Result (Failure, Success, errors),
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    inputType,
  )
import Data.Morpheus.Server.Deriving.Utils.SchemaBuilder (SchemaBuilder)
import Data.Morpheus.Server.Deriving.Utils.Use (UseGQLType (..), useDeriveType)
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    FieldsDefinition,
    Msg (..),
    Schema (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    VALID,
    fieldsToArguments,
  )
import Language.Haskell.TH (Exp, Q)
import Relude hiding (empty)

fromSchema :: GQLResult (Schema VALID) -> Q Exp
fromSchema Success {} = [|()|]
fromSchema Failure {errors} = fail (show errors)

withObject :: (gql a) => UseGQLType gql -> CatType c a -> TypeContent TRUE any s -> SchemaBuilder (FieldsDefinition c s)
withObject _ InputType DataInputObject {inputObjectFields} = pure inputObjectFields
withObject _ OutputType DataObject {objectFields} = pure objectFields
withObject gql x _ = failureOnlyObject gql x

failureOnlyObject :: (gql a) => UseGQLType gql -> CatType c a -> SchemaBuilder b
failureOnlyObject gql proxy = throwError $ msg (useTypename gql proxy) <> " should have only one nonempty constructor"

deriveTypeAsArguments :: gql a => UseGQLType gql -> f a -> SchemaBuilder (ArgumentsDefinition CONST)
deriveTypeAsArguments gql arg =
  fieldsToArguments
    <$> ( useDeriveType gql (inputType arg)
            >>= withObject gql (inputType arg) . typeContent
        )
