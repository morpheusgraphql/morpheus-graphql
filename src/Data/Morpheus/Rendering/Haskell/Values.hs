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
import           Data.Morpheus.Types.Internal.Data     (DataField (..), DataFullType (..), DataLeaf (..),
                                                        DataTyCon (..), DataTypeLib (..), TypeAlias (..), WrapperD (..))

renderRootResolver :: Context -> DataTypeLib -> Text
renderRootResolver _ DataTypeLib {mutation, subscription} = renderSignature <> renderBody <> "\n\n"
  where
    renderSignature = "rootResolver :: " <> renderRootSig (fst <$> subscription) <> "\n"
      where
        renderRootSig (Just sub) = "GQLRootResolver IO Channel Content Query " <> maybeOperator mutation <> " " <> sub
        renderRootSig Nothing    = "GQLRootResolver IO () () Query " <> maybeOperator mutation <> " ()"
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
renderResolver Context {scope, pubSub = (channel, content)} (name, dataType) = renderSig dataType
  where
    renderSig (Leaf DataScalar {}) = defFunc <> renderReturn <> "$ " <> renderCon name <> "0 0"
    renderSig (Leaf (DataEnum DataTyCon {typeData})) = defFunc <> renderReturn <> renderCon (head typeData)
    renderSig (Union DataTyCon {typeData}) = defFunc <> renderUnionCon name typeCon <> " <$> " <> "resolve" <> typeCon
      where
        typeCon = aliasTyCon $ fieldType $ head typeData
    renderSig (OutputObject DataTyCon {typeData}) = defFunc <> renderReturn <> renderCon name <> renderObjFields
      where
        renderObjFields = renderResObject (map renderFieldRes typeData)
        renderFieldRes (key, DataField {fieldType = TypeAlias {aliasWrappers, aliasTyCon}}) =
          (key, "const " <> withScope scope (renderValue aliasWrappers aliasTyCon))
          where
            renderValue (MaybeD:_) = const $ "$ " <> renderReturn <> "Nothing"
            renderValue (ListD:_)  = const $ "$ " <> renderReturn <> "[]"
            renderValue []         = fieldValue
            ----------------------------------------------------------------------------
            fieldValue "String" = "$ return \"\""
            fieldValue "Int"    = "$ return 0"
            fieldValue fName    = "resolve" <> fName
            -------------------------------------------
            withScope Subscription x = "$ Event { channels = [Channel], content = const " <> x <> " }"
            withScope Mutation x =
              case (channel, content) of
                ("()", "()") -> x
                _            -> "$ toMutResolver [Event {channels = [Channel], content = Content}] " <> x
            withScope _ x = x
    renderSig _ = "" -- INPUT Types Does not Need Resolvers
    --------------------------------
    defFunc = renderSignature <> renderFunc
    ----------------------------------------------------------------------------------------------------------
    renderSignature = renderAssignment ("resolve" <> name) (renderMonad name) <> "\n"
    ---------------------------------------------------------------------------------
    renderMonad "Mutation"     = "IOMutRes " <> channel <> " " <> content <> " Mutation"
    renderMonad "Subscription" = "SubRootRes IO " <> channel <> " Subscription"
    renderMonad tName          = "IORes " <> tName
    ----------------------------------------------------------------------------------------------------------
    renderFunc = "resolve" <> name <> " = "
    ---------------------------------------

renderResObject :: [(Text, Text)] -> Text
renderResObject = renderSet . map renderEntry
  where
    renderEntry (key, value) = renderEqual key value
