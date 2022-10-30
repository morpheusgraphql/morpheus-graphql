{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Data.Morpheus.App.Haxl
  ( Haxl,
    getNamedIds,
    getNamedResponseById,
    withDeityHaxl,
  )
where

import qualified Data.Map as M
import Data.Morpheus.App.Internal.Resolving.Haxl
  ( Haxl,
    NamedArg,
    NamedResponse,
    State (ReqState),
    getIds,
    getResponseById,
    withHaxl,
  )
import Data.Morpheus.Types.Internal.AST (TypeName, ValidValue, Value (..))
import Haxl.Core
  ( GenHaxl,
  )

withDeityHaxl :: GenHaxl () w b -> IO b
withDeityHaxl = withHaxl (ReqState resMap fetchDeityIds)

getNamedIds :: Haxl [NamedArg]
getNamedIds = getIds

getNamedResponseById :: NamedArg -> Haxl NamedResponse
getNamedResponseById = getResponseById

fetchDeityIds :: IO [NamedArg]
fetchDeityIds = do
  print ("Fetch Ids" :: String)
  pure [("", "Morpheus"), ("Zeus", Null), ("Ares", Null)]

type ResMap = M.Map TypeName ([ValidValue] -> IO [ValidValue])

resMap :: ResMap
resMap =
  M.fromList
    [ ("Deity", fetchDeityNames),
      ("Power", fetchDeityPowers)
    ]

fetchDeityNames :: [ValidValue] -> IO [ValidValue]
fetchDeityNames ids = do
  print ("Fetch Name for: " <> show ids)
  pure ids

fetchDeityPowers :: [ValidValue] -> IO [ValidValue]
fetchDeityPowers ids = do
  print ("Fetch Power for: " <> show ids)
  pure $ map (const "Shapeshifting") ids
