module Data.Morpheus.Client.Build
  ( define
  ) where

import           Data.Char           (toLower, toUpper)
import           Language.Haskell.TH

define :: (String, [(String, String)]) -> Q [Dec]
define (strName, fields) = (pure . pure) $ DataD [] typeName [] Nothing [recordCon] []
  where
    typeName = mkName strName
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    recordCon = RecC typeName (map genField fields)
      where
        genField (fieldName, fType) =
          (mkName $ unCapitalize strName <> capitalize fieldName, defBang, ConT $ mkName fType)
          where
            unCapitalize []     = []
            unCapitalize (x:xs) = toLower x : xs
            capitalize []     = []
            capitalize (x:xs) = toUpper x : xs
