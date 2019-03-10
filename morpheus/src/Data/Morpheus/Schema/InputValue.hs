{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Schema.InputValue
  ( name
  , isRequired
  , inputValueMeta
  , typeName
  )
where

import           Data.Text                      ( Text(..) )
import           Data.Morpheus.Types.Types      ( (::->)(..) )
import           Data.Morpheus.Types.Introspection
                                                ( GQL__InputValue
                                                , GQL__Type
                                                )
import qualified Data.Morpheus.Types.MetaInfo  as M ( MetaInfo(..) , Position )
import qualified Data.Morpheus.Schema.GQL__InputValue
                                               as I
                                                ( GQL__InputValue(..) )
import qualified Data.Morpheus.Schema.GQL__Type
                                               as T
                                                ( GQL__Type(..) )

name :: GQL__InputValue -> Text
name = I.name

isRequired :: GQL__InputValue -> Bool
isRequired x = I.defaultValue x /= "Nothing"

typeName :: GQL__InputValue -> Text
typeName x = case I._type x of
  Nothing -> "Error"
  Just t  -> T.name t

inputValueMeta :: M.Position -> GQL__InputValue -> M.MetaInfo
inputValueMeta pos input =
  M.MetaInfo { M.typeName = "TODO: Type", M.key = I.name input , M.position = pos }
