module Data.Morpheus.PreProcess.Input.Enum
  ( validateEnum
  ) where

import           Data.Morpheus.Error.Variable     (invalidEnumOption)
import           Data.Morpheus.Schema.EnumValue   (isEnumOf)
import qualified Data.Morpheus.Schema.Type        as T (enumValues, name)
import           Data.Morpheus.Schema.Utils.Utils (Type)
import           Data.Morpheus.Types.Describer    ((::->) (Some))
import           Data.Morpheus.Types.Error        (Validation)
import           Data.Morpheus.Types.JSType       (JSType (..))
import           Data.Morpheus.Types.MetaInfo     (MetaInfo (..))
import           Data.Morpheus.Types.Types        (Argument (..))

validateEnum :: Type -> Argument -> Validation Argument
validateEnum _type (Argument (JSEnum argument) pos) =
  if isEnumOf argument (unwrapField $ T.enumValues _type)
    then pure (Argument (JSEnum argument) pos)
    else Left $ invalidEnumOption meta
  where
    unwrapField (Some x) = x
    meta = MetaInfo {typeName = T.name _type, key = argument, position = pos}
--validateEnum _ (Argument _ _) = Left $ InternError "validateEnum: didnot got JSEnum as Input"
