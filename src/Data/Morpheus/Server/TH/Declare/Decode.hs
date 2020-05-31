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
  ( instanceHeadT,
    nameVarP,
  )
import Data.Morpheus.Server.Deriving.Decode
  ( Decode (..),
    DecodeType (..),
  )
import Data.Morpheus.Server.Internal.TH.Decode
  ( decodeFieldWith,
    decodeObjectExpQ,
    withObject,
  )
import Data.Morpheus.Server.Internal.TH.Types (ServerTypeDefinition (..))
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    ValidValue,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Language.Haskell.TH

(.:) :: Decode a => ValidValue -> FieldName -> Eventless a
value .: selectorName = withObject (decodeFieldWith decode selectorName) value

deriveDecode :: ServerTypeDefinition cat -> Q [Dec]
deriveDecode ServerTypeDefinition {tName, tCons = [cons]} =
  pure <$> instanceD (cxt []) appHead methods
  where
    appHead = instanceHeadT ''DecodeType tName []
    methods = [funD 'decodeType [clause argsE (normalB body) []]]
      where
        argsE = map nameVarP ["o"]
        body = decodeObjectExpQ [|(.:)|] cons
deriveDecode _ = pure []
