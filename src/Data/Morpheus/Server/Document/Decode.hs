{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.Document.Decode
  ( deriveDecode,
  )
where

--
-- MORPHEUS

import Data.Morpheus.Internal.TH
  ( instanceHeadT,
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
import Data.Morpheus.Types.Internal.AST
  ( TypeD (..),
    TypeName,
    ValidValue,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Language.Haskell.TH

(.:) :: Decode a => ValidValue -> TypeName -> Eventless a
value .: selectorName = withObject (decodeFieldWith decode selectorName) value

deriveDecode :: TypeD -> Q [Dec]
deriveDecode TypeD {tName, tCons = [cons]} =
  pure <$> instanceD (cxt []) appHead methods
  where
    appHead = instanceHeadT ''DecodeType tName []
    methods = [funD 'decodeType [clause argsE (normalB body) []]]
      where
        argsE = map (varP . mkName) ["o"]
        body = decodeObjectExpQ [|(.:)|] cons
deriveDecode _ = pure []
