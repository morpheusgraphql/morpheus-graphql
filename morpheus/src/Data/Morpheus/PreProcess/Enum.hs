{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.Enum
    ( validateEnum
    )
where


import           Data.Text                      ( Text )
import           Data.Morpheus.Types.Introspection
                                                ( GQL__Type(..) )
import           Data.Morpheus.Types.Types      ( Validation(..)
                                                , (::->)(Some)
                                                , Argument(..)
                                                )
import           Data.Morpheus.Types.JSType     ( JSType(..) )
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..), Position(..) )
import qualified Data.Morpheus.Schema.GQL__Type
                                               as T
import           Data.Morpheus.ErrorMessage     ( invalidEnumOption )
import           Data.Morpheus.Schema.GQL__EnumValue
                                                ( isEnumOf )


validateEnum :: GQL__Type -> Argument -> Validation Argument
validateEnum _type (Argument (JSEnum argument)) =
    if isEnumOf argument (unwrapField $ T.enumValues _type)
        then pure (Argument (JSEnum argument))
        else error
  where
    unwrapField (Some x) = x
    error = Left $ invalidEnumOption [] meta
    meta = MetaInfo {
        typeName = (T.name _type) ,
        key = argument,
        position = Position 0
    }
