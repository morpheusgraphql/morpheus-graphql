{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module HConf.Cabal
  ( checkCabals,
  )
where

import qualified Data.ByteString.Char8 as BS (unpack)
import Data.Map (lookup)
import Data.Text (breakOn, drop, null, pack, split, strip, unpack)
import HConf.ConfigT (ConfigT)
import HConf.Log (field, label, log, task)
import HConf.Package (Package (..), resolvePackages)
import HConf.Utils (Name)
import HConf.Version (Parse (..), Version)
import HConf.Yaml (read)
import Relude hiding (Undefined, break, drop, length, null, replicate)

parseFields :: ByteString -> Map Text Text
parseFields =
  fromList
    . filter (not . null . fst)
    . map (bimap strip strip . (second (drop 1) . breakOn ":") . strip)
    . split (== '\n')
    . pack
    . BS.unpack

getField :: (MonadFail m, Ord k) => k -> Map k a -> m a
getField k = maybe (fail "") pure . lookup k

getCabalFields :: FilePath -> Name -> ConfigT (Name, Version)
getCabalFields path pkgName = do
  bs <- read (path <> "/" <> unpack pkgName <> ".cabal")
  let fields = parseFields bs
  name <- getField "name" fields
  version <- getField "version" fields >>= parse
  field (unpack name) (show version)
  pure (name, version)

checkCabal :: (Name, Package) -> ConfigT ()
checkCabal (path, Package {..}) = task path $ do
  (pkgName, pkgVersion) <- getCabalFields (unpack path) name
  if pkgVersion == version && pkgName == name then pure () else fail (unpack path <> "mismatching version or name")

checkCabals :: ConfigT ()
checkCabals = label "cabal" $ resolvePackages >>= traverse_ checkCabal