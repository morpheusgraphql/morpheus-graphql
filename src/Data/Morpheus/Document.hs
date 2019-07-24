{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document
  ( toGraphQLDocument
  ) where

import           Data.ByteString.Lazy.Char8        (ByteString, pack)
import           Data.Morpheus.Resolve.Resolve     (RootResCon, fullSchema)
import           Data.Morpheus.Types               (GQLRootResolver)
import           Data.Morpheus.Types.Internal.Data (DataArgument, DataField (..), DataFullType (..), DataLeaf (..),
                                                    DataType (..), allDataTypes, showWrappedType)
import           Data.Semigroup                    ((<>))
import           Data.Text                         (Text, intercalate)
import qualified Data.Text.Lazy                    as LT (fromStrict)
import           Data.Text.Lazy.Encoding           (encodeUtf8)

toGraphQLDocument :: RootResCon m a query mut sub => GQLRootResolver m a query mut sub -> ByteString
toGraphQLDocument x =
  case fullSchema x of
    Left validationError -> pack (show validationError)
    Right lib -> encodeUtf8 $ LT.fromStrict $ intercalate "\n\n" $ map renderType visibleTypes
      where visibleTypes = filter (isVisible . snd) (allDataTypes lib)

isVisible :: DataFullType -> Bool
isVisible (Leaf (LeafScalar DataType {typeVisibility})) = typeVisibility
isVisible (Leaf (LeafEnum DataType {typeVisibility}))   = typeVisibility
isVisible (Union DataType {typeVisibility})             = typeVisibility
isVisible (InputObject DataType {typeVisibility})       = typeVisibility
isVisible (InputUnion DataType {typeVisibility})        = typeVisibility
isVisible (OutputObject DataType {typeVisibility})      = typeVisibility

renderType :: (Text, DataFullType) -> Text
renderType (name, Leaf (LeafScalar _)) = "scalar " <> name
renderType (name, Leaf (LeafEnum DataType {typeData})) = "enum " <> name <> " = " <> intercalate "\n | " typeData
renderType (name, Union DataType {typeData}) = "union " <> name <> " = " <> intercalate "\n | " (map fieldType typeData)
renderType (name, InputObject DataType {typeData}) = "input " <> name <> renderObject renderInputField typeData
renderType (name, InputUnion DataType {typeData}) = "input " <> name <> renderObject renderInputField (mapKeys typeData)
renderType (name, OutputObject DataType {typeData}) = "type " <> name <> renderObject renderField typeData

mapKeys :: [DataField a] -> [(Text, DataField a)]
mapKeys = map (\x -> (fieldName x, x))

renderObject :: ((Text, DataField a) -> Text) -> [(Text, DataField a)] -> Text
renderObject f list = " { \n  " <> intercalate "\n  " (map f (ignoreHidden list)) <> "  \n}"

renderInputField :: (Text, DataField ()) -> Text
renderInputField (key, DataField {fieldTypeWrappers, fieldType}) =
  key <> ": " <> showWrappedType fieldTypeWrappers fieldType

ignoreHidden :: [(Text, DataField a)] -> [(Text, DataField a)]
ignoreHidden = filter (not . fieldHidden . snd)

renderArguments :: [(Text, DataArgument)] -> Text
renderArguments []   = ""
renderArguments list = "( " <> intercalate ", " (map renderInputField list) <> " )"

renderField :: (Text, DataField [(Text, DataArgument)]) -> Text
renderField (key, DataField {fieldTypeWrappers, fieldType, fieldArgs}) =
  key <> renderArguments fieldArgs <> ": " <> showWrappedType fieldTypeWrappers fieldType
