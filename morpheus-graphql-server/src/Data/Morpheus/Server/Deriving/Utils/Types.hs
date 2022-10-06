{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.Types
  ( ConsRep (..),
    FieldRep (..),
    DataType (..),
    enumerate,
    isEmptyConstraint,
    fieldTypeName,
    isUnionRef,
    unpackMonad,
  )
where

import Data.Morpheus.Types.Internal.AST
import qualified Data.Text as T
import Relude

data DataType (v :: Type) = DataType
  { dataTypeName :: TypeName,
    tyIsUnion :: Bool,
    tyCons :: ConsRep v
  }
  deriving (Functor)

data ConsRep (v :: Type) = ConsRep
  { consName :: TypeName,
    consFields :: [FieldRep v]
  }
  deriving (Functor)

data FieldRep (a :: Type) = FieldRep
  { fieldSelector :: FieldName,
    fieldTypeRef :: TypeRef,
    fieldValue :: a
  }
  deriving (Functor)

-- setFieldNames ::  Power Int Text -> Power { _1 :: Int, _2 :: Text }
enumerate :: [FieldRep a] -> [FieldRep a]
enumerate = zipWith setFieldName ([0 ..] :: [Int])
  where
    setFieldName i field = field {fieldSelector = packName $ "_" <> T.pack (show i)}

isEmptyConstraint :: ConsRep a -> Bool
isEmptyConstraint ConsRep {consFields = []} = True
isEmptyConstraint _ = False

fieldTypeName :: FieldRep k -> TypeName
fieldTypeName = typeConName . fieldTypeRef

isUnionRef :: TypeName -> ConsRep k -> Bool
isUnionRef baseName ConsRep {consName, consFields = [fieldRep]} =
  consName == baseName <> fieldTypeName fieldRep
isUnionRef _ _ = False

unpackMonad :: Monad m => [ConsRep (m a)] -> m [ConsRep a]
unpackMonad = traverse unpackMonadFromCons

unpackMonadFromField :: Monad m => FieldRep (m a) -> m (FieldRep a)
unpackMonadFromField FieldRep {..} = do
  cont <- fieldValue
  pure (FieldRep {fieldValue = cont, ..})

unpackMonadFromCons :: Monad m => ConsRep (m a) -> m (ConsRep a)
unpackMonadFromCons ConsRep {..} = ConsRep consName <$> traverse unpackMonadFromField consFields
