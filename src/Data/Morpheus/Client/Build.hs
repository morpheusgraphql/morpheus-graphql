module Data.Morpheus.Client.Build
  ( buildRecord
  ) where

import           Data.Char           (toLower)
import           Language.Haskell.TH

buildRecord :: String -> [(String, Name)] -> Q [Dec]
buildRecord strName fields = (pure . pure) $ DataD [] typeName [] Nothing [recordCon] []
  where
    typeName = mkName strName
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    recordCon = RecC typeName (map genField fields)
      where
        genField (fieldName, fType) = (mkName $ recBaseName strName <> nameBase (mkName fieldName), defBang, ConT fType)
          where
            recBaseName []     = []
            recBaseName (x:xs) = toLower x : xs
