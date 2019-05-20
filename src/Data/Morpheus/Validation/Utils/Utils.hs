module Data.Morpheus.Validation.Utils.Utils
  ( differKeys
  , existsObjectType
  , lookupType
  , getInputType
  , lookupField
  , checkNameCollision
  , checkForUnknownKeys
  ) where

import           Data.List                        ((\\))
import           Data.Morpheus.Error.Variable     (unknownType)
import           Data.Morpheus.Types.Core         (EnhancedKey (..), Key, enhanceKeyWithNull)
import           Data.Morpheus.Types.Error        (Validation)
import           Data.Morpheus.Types.Internal.AST (ASTInputType, ASTKind (..), ASTLeaf (..), ASTOutputObject,
                                                   ASTTypeLib (..))
import           Data.Morpheus.Types.MetaInfo     (Position)
import qualified Data.Set                         as S
import           Data.Text                        (Text)

type GenError error a = error -> Either error a

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

getInputType :: Text -> ASTTypeLib -> GenError error ASTInputType
getInputType typeName' lib error' =
  case lookup typeName' (inputObject lib) of
    Just x -> pure (ObjectKind x)
    Nothing ->
      case lookup typeName' (leaf lib) of
        Nothing             -> Left error'
        Just (LeafScalar x) -> pure (ScalarKind x)
        Just (LeafEnum x)   -> pure (EnumKind x)

existsObjectType :: Position -> Text -> ASTTypeLib -> Validation ASTOutputObject
existsObjectType position' typeName' lib = lookupType error' (object lib) typeName'
  where
    error' = unknownType typeName' position'

differKeys :: [EnhancedKey] -> [Key] -> [EnhancedKey]
differKeys enhanced keys = enhanced \\ map enhanceKeyWithNull keys

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = S.toList . S.fromList

elementOfKeys :: [Text] -> EnhancedKey -> Bool
elementOfKeys keys' EnhancedKey {uid = id'} = id' `elem` keys'

checkNameCollision :: [EnhancedKey] -> [Text] -> ([EnhancedKey] -> error) -> Either error [EnhancedKey]
checkNameCollision enhancedKeys' keys' errorGenerator' =
  case differKeys enhancedKeys' (removeDuplicates keys') of
    []          -> pure enhancedKeys'
    duplicates' -> Left $ errorGenerator' duplicates'

checkForUnknownKeys :: [EnhancedKey] -> [Text] -> ([EnhancedKey] -> error) -> Either error [EnhancedKey]
checkForUnknownKeys enhancedKeys' keys' errorGenerator' =
  case filter (not . elementOfKeys keys') enhancedKeys' of
    []           -> pure enhancedKeys'
    unknownKeys' -> Left $ errorGenerator' unknownKeys'
