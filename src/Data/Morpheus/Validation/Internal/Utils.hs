module Data.Morpheus.Validation.Internal.Utils
  ( differKeys
 -- , existsObjectType
  , lookupType
  , lookupField
  , checkNameCollision
  , checkForUnknownKeys
  , VALIDATION_MODE(..)
  ) where

import           Data.List                               ((\\))
import           Data.Morpheus.Error.Variable            (unknownType)
import           Data.Morpheus.Types.Internal.Base       (EnhancedKey (..), Key, Position, enhanceKeyWithNull)
import           Data.Morpheus.Types.Internal.Data       (DataObject, DataTypeLib (..))
import           Data.Morpheus.Types.Internal.Validation (Validation)
import qualified Data.Set                                as S
import           Data.Text                               (Text)

type GenError error a = error -> Either error a

data VALIDATION_MODE
  = WITHOUT_VARIABLES
  | FULL_VALIDATION
  deriving (Eq, Show)

lookupType :: error -> [(Text, a)] -> Text -> Either error a
lookupType error' lib' typeName' =
  case lookup typeName' lib' of
    Nothing -> Left error'
    Just x  -> pure x

lookupField :: Text -> [(Text, fType)] -> GenError error fType
lookupField id' lib' error' =
  case lookup id' lib' of
    Nothing    -> Left error'
    Just field -> pure field

--existsObjectType :: Position -> Text -> DataTypeLib -> Validation DataObject
--existsObjectType position' typeName' lib = lookupType error' (object lib) typeName'
--  where
--    error' = unknownType typeName' position'

differKeys :: [EnhancedKey] -> [Key] -> [EnhancedKey]
differKeys enhanced keys = enhanced \\ map enhanceKeyWithNull keys

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = S.toList . S.fromList

elementOfKeys :: [Text] -> EnhancedKey -> Bool
elementOfKeys keys' EnhancedKey {uid = id'} = id' `elem` keys'

checkNameCollision :: [EnhancedKey] -> ([EnhancedKey] -> error) -> Either error [EnhancedKey]
checkNameCollision enhancedKeys errorGenerator =
  case enhancedKeys \\ removeDuplicates enhancedKeys of
    []         -> pure enhancedKeys
    duplicates -> Left $ errorGenerator duplicates

checkForUnknownKeys :: [EnhancedKey] -> [Text] -> ([EnhancedKey] -> error) -> Either error [EnhancedKey]
checkForUnknownKeys enhancedKeys' keys' errorGenerator' =
  case filter (not . elementOfKeys keys') enhancedKeys' of
    []           -> pure enhancedKeys'
    unknownKeys' -> Left $ errorGenerator' unknownKeys'
