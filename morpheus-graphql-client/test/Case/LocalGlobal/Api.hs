{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.LocalGlobal.Api
  ( declareAPITypes,
  )
where

import Data.Morpheus.Client
import Spec.Utils
  ( fixedSchemaPath,
  )

declareAPITypes = declareClientTypes (fixedSchemaPath "LocalGlobal")
