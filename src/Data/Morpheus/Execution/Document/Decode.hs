{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.Decode
  ( deriveDecode
  )
where

import           Data.Text                      ( Text )
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Internal.Decode
                                                ( decodeFieldWith
                                                , decodeObjectExpQ
                                                , withObject
                                                )
import           Data.Morpheus.Execution.Server.Decode
                                                ( Decode(..)
                                                , DecodeType(..)
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( TypeD(..)
                                                , ValidValue
                                                )
import           Data.Morpheus.Types.Internal.TH
                                                ( instanceHeadT )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation )

(.:) :: Decode a => ValidValue -> Text -> Validation a
value .: selectorName = withObject (decodeFieldWith decode selectorName) value

deriveDecode :: TypeD -> Q [Dec]
deriveDecode TypeD { tName, tCons = [cons] } =
  pure <$> instanceD (cxt []) appHead methods
 where
  appHead = instanceHeadT ''DecodeType tName []
  methods = [funD 'decodeType [clause argsE (normalB body) []]]
   where
    argsE = map (varP . mkName) ["o"]
    body  = decodeObjectExpQ [|(.:)|] cons
deriveDecode _ = pure []
