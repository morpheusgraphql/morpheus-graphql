{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Rendering.Haskell.Values
  ( renderRootResolver
  , renderResolver
  , Scope(..)
  ) where

import           Data.Semigroup                        ((<>))
import           Data.Text                             (Text)

-- MORPHEUS
import           Data.Morpheus.Rendering.Haskell.Terms (Context (..), Scope (..), renderAssignment, renderCon,
                                                        renderEqual, renderReturn, renderSet, renderUnionCon)
import           Data.Morpheus.Types.Internal.Data     (DataField (..), DataFullType (..), DataLeaf (..), DataType (..),
                                                        DataTypeLib (..), DataTypeWrapper (..))

renderRootResolver :: Context -> DataTypeLib -> Text
renderRootResolver _ DataTypeLib {mutation, subscription} =
  renderSignature <> renderBody <> "\n\n"
  where
    renderSignature =
      "rootResolver :: " <> renderRootSig (fst <$> subscription) <> "\n"
      where
        renderRootSig (Just sub) =
          "GQLRootResolver IO Channel Content Query " <> maybeOperator mutation <>
          " " <>
          sub
        renderRootSig Nothing =
          "GQLRootResolver IO () () Query " <> maybeOperator mutation <> " ()"
        ----------------------
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

renderResolver :: Context -> (Text, DataFullType) -> Text
renderResolver Context {scope, pubSub = (channel, content)} (name, dataType) =
  renderSig dataType
  where
    renderSig (Leaf BaseScalar {}) =
      defFunc <> renderReturn <> "$ " <> renderCon name <> "0 0"
    renderSig (Leaf CustomScalar {}) =
      defFunc <> renderReturn <> "$ " <> renderCon name <> "0 0"
    renderSig (Leaf (LeafEnum DataType {typeData})) =
      defFunc <> renderReturn <> renderCon (head typeData)
    renderSig (Union DataType {typeData}) =
      defFunc <> renderUnionCon name typeCon <> " <$> " <> "resolve" <> typeCon
      where
        typeCon = fieldType $ head typeData
    renderSig (OutputObject DataType {typeData}) =
      defFunc <> renderReturn <> renderCon name <> renderObjFields
      where
        renderObjFields = renderResObject (map renderFieldRes typeData)
        renderFieldRes (key, DataField {fieldType, fieldTypeWrappers}) =
          ( key
          , "const " <>
            withScope scope (renderValue fieldTypeWrappers fieldType))
          where
            renderValue [] = const $ "$ " <> renderReturn <> "Nothing"
            renderValue [NonNullType] = fieldValue
            renderValue (ListType:_) = const $ "$ " <> renderReturn <> "Just []"
            renderValue (NonNullType:(ListType:_)) =
              const $ "$ " <> renderReturn <> "[]"
            renderValue (NonNullType:(NonNullType:xs)) =
              renderValue (NonNullType : xs)
            ----------------------------------------------------------------------------
            fieldValue "String" = "$ return \"\""
            fieldValue "Int"    = "$ return 0"
            fieldValue fName    = "resolve" <> fName
            -------------------------------------------
            withScope Subscription x =
              "$ Event { channels = [Channel], content = const " <> x <> " }"
            withScope Mutation x =
              case (channel, content) of
                ("()", "()") -> x
                _ ->
                  "$ toMutResolver [Event {channels = [Channel], content = Content}] " <>
                  x
            withScope _ x = x
    renderSig _ = "" -- INPUT Types Does not Need Resolvers
    --------------------------------
    defFunc = renderSignature <> renderFunc
    ----------------------------------------------------------------------------------------------------------
    renderSignature =
      renderAssignment ("resolve" <> name) (renderMonad name) <> "\n"
    ---------------------------------------------------------------------------------
    renderMonad "Mutation" =
      "IOMutRes " <> channel <> " " <> content <> " Mutation"
    renderMonad "Subscription" = "SubRootRes IO " <> channel <> " Subscription"
    renderMonad tName = "IORes " <> tName
    ----------------------------------------------------------------------------------------------------------
    renderFunc = "resolve" <> name <> " = "
    ---------------------------------------

renderResObject :: [(Text, Text)] -> Text
renderResObject = renderSet . map renderEntry
  where
    renderEntry (key, value) = renderEqual key value
