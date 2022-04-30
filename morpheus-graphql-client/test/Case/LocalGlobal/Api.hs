{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.LocalGlobal.Api
  ( schema,
    loc,
  )
where

import Relude
import Spec.Utils
  ( path,
  )

loc :: FilePath -> FilePath
loc x = path ("LocalGlobal/" <> x)

schema :: FilePath
schema = loc "schema.gql"
