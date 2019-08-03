{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.Rendering.Values
  ( renderRootResolver
  , renderResolver
  , Scope(..)
  ) where

import           Data.Semigroup                         ((<>))
import           Data.Text                              (Text)

-- MORPHEUS
import           Data.Morpheus.Document.Rendering.Terms (Scope (..), renderAssignment, renderCon, renderReturn,
                                                         renderSet, renderUnionCon)
import           Data.Morpheus.Types.Internal.Data      (DataField (..), DataFullType (..), DataLeaf (..),
                                                         DataType (..), DataTypeLib (..), DataTypeWrapper (..))

renderRootResolver :: DataTypeLib -> Text
renderRootResolver DataTypeLib {mutation, subscription} = renderSignature <> renderBody <> "\n\n"
  where
    renderSignature =
      "rootResolver :: GQLRootResolver IO () () Query " <> maybeOperator mutation <> " " <> maybeOperator subscription <>
      "\n"
      where
        maybeOperator (Just (name, _)) = name
        maybeOperator Nothing          = "()"
    renderBody = "rootResolver =\n  GQLRootResolver" <> renderResObject fields
      where
        fields =
          [ ("queryResolver", "resolveQuery")
          , ("mutationResolver", maybeRes mutation)
          , ("subscriptionResolver", maybeRes subscription)
          ]
      ---------------------------------------------
        maybeRes (Just (name, _)) = "resolve" <> name
        maybeRes Nothing          = "return ()"

renderResolver :: Scope -> (Text, DataFullType) -> Text
renderResolver conScope (name, dataType) = renderSig dataType
  where
    renderSig (Leaf LeafScalar {}) = defFunc <> renderReturn <> "$ " <> renderCon name <> "0 0"
    renderSig (Leaf (LeafEnum DataType {typeData})) = defFunc <> renderReturn <> renderCon (head typeData)
    renderSig (Union DataType {typeData}) = defFunc <> renderUnionCon name typeCon <> " <$> " <> "resolve" <> typeCon
      where
        typeCon = fieldType $ head typeData
    renderSig (OutputObject DataType {typeData}) = defFunc <> renderReturn <> renderCon name <> renderObjFields
      where
        renderObjFields = renderResObject (map renderFieldRes typeData)
        renderFieldRes (key, DataField {fieldType, fieldTypeWrappers}) =
          (key, "const " <> withScope conScope (renderValue fieldTypeWrappers fieldType))
          where
            renderValue []                             = const $ "$ " <> renderReturn <> "Nothing"
            renderValue [NonNullType]                  = fieldValue
            renderValue (ListType:_)                   = const $ "$ " <> renderReturn <> "Just []"
            renderValue (NonNullType:(ListType:_))     = const $ "$ " <> renderReturn <> "[]"
            renderValue (NonNullType:(NonNullType:xs)) = renderValue (NonNullType : xs)
            ----------------------------------------------------------------------------
            fieldValue "String" = "$ return \"\""
            fieldValue "Int"    = "$ return 0"
            fieldValue fName    = "resolve" <> fName
            -------------------------------------------
            withScope Subscription x = "$ Event { channels = [{- TODO: Channel -}], content = const " <> x <> " }"
            withScope _ x            = x
    renderSig _ = "" -- INPUT Types Does not Need Resolvers
    --------------------------------
    defFunc = renderSignature <> renderFunc
    ----------------------------------------------------------------------------------------------------------
    renderSignature = renderAssignment ("resolve" <> name) (renderMonad name) <> "\n"
    ---------------------------------------------------------------------------------
    renderMonad "Mutation"     = "IOMutRes () () Mutation"
    renderMonad "Subscription" = "SubRootRes IO () Subscription"
    renderMonad tName          = "IORes " <> tName
    ----------------------------------------------------------------------------------------------------------
    renderFunc = "resolve" <> name <> " = "
    ---------------------------------------

renderResObject :: [(Text, Text)] -> Text
renderResObject = renderSet . map renderEntry
  where
    renderEntry (key, value) = key <> " = " <> value
