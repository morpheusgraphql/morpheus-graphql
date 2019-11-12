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
                                                )
import           Data.Morpheus.Execution.Server.Decode
                                                ( Decode(..)
                                                , DecodeObject(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.DataD
                                                ( TypeD(..) )
import           Data.Morpheus.Types.Internal.TH
                                                ( instanceHeadT )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Object )

(.:) :: Decode a => Object -> Text -> Validation a
object .: selectorName = decodeFieldWith decode selectorName object

deriveDecode :: TypeD -> Q [Dec]
deriveDecode TypeD { tName, tCons = [cons] } =
  pure <$> instanceD (cxt []) appHead methods
 where
  appHead = instanceHeadT ''DecodeObject tName []
  methods = [funD 'decodeObject [clause argsE (normalB body) []]]
   where
    argsE = map (varP . mkName) ["o"]
    body  = decodeObjectExpQ [|(.:)|] cons
deriveDecode _ = pure []
