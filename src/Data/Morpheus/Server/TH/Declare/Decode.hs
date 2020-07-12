{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.TH.Declare.Decode
  ( deriveDecode,
  )
where

--
-- MORPHEUS

import Data.Morpheus.Internal.TH
  ( applyCons,
    decodeObjectE,
    funDSimple,
    v',
  )
import Data.Morpheus.Server.Deriving.Decode
  ( Decode (..),
    DecodeType (..),
  )
import Data.Morpheus.Server.Internal.TH.Decode
  ( decodeFieldWith,
    withInputObject,
  )
import Data.Morpheus.Server.Internal.TH.Types (ServerTypeDefinition (..))
import Data.Morpheus.Types.Internal.AST
  ( ConsD (..),
    FieldName,
    TypeName,
    ValidValue,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( ResolverState,
  )
import Language.Haskell.TH

decodeFieldValue :: Decode a => ValidValue -> FieldName -> ResolverState a
decodeFieldValue value selectorName = withInputObject (decodeFieldWith decode selectorName) value

mkTypeClass :: TypeName -> Q Type
mkTypeClass tName = applyCons ''DecodeType [tName]

decodeValueD :: ConsD cat s -> DecQ
decodeValueD ConsD {cName, cFields} = funDSimple 'decodeType [v'] body
  where
    body = decodeObjectE (const 'decodeFieldValue) cName cFields

deriveDecode :: ServerTypeDefinition cat s -> Q [Dec]
deriveDecode
  ServerTypeDefinition
    { tName,
      tCons = [cons]
    } =
    pure <$> instanceD (cxt []) (mkTypeClass tName) [decodeValueD cons]
deriveDecode _ = pure []
