{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.Internal.TH.Utils
  ( kindName,
    constraintTypeable,
  )
where

import Data.Morpheus.Internal.TH
  ( typeT,
  )
import Data.Morpheus.Kind
  ( ENUM,
    INPUT,
    INTERFACE,
    OUTPUT,
    SCALAR,
    WRAPPER,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ConsD (..),
    FieldName,
    IN,
    TypeDefinition,
    TypeKind (..),
    TypeName,
  )
import Data.Typeable (Typeable)
import Language.Haskell.TH
  ( Name,
    Q,
    Type,
  )

constraintTypeable :: TypeName -> Q Type
constraintTypeable name = typeT ''Typeable [name]

kindName :: TypeKind -> Name
kindName KindObject {} = ''OUTPUT
kindName KindScalar = ''SCALAR
kindName KindEnum = ''ENUM
kindName KindUnion = ''OUTPUT
kindName KindInputObject = ''INPUT
kindName KindList = ''WRAPPER
kindName KindNonNull = ''WRAPPER
kindName KindInputUnion = ''INPUT
kindName KindInterface = ''INTERFACE
