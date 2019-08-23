{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Morpheus.Execution.Internal.Declare
  ( declareType
  ) where

import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Types.Internal.DataD (AppD (..), ConsD (..), FieldD (..), TypeD (..))

declareType :: [String] -> TypeD -> Dec
declareType derivings TypeD {tName, tCons} =
  DataD [] (mkName tName) [] Nothing (map cons tCons) $ map derive (["Show", "Generic"] ++ derivings)
  where
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    derive className = DerivClause Nothing [ConT (mkName className)]
    cons ConsD {cName, cFields} = RecC (mkName cName) (map genField cFields)
      where
        genField FieldD {fieldNameD, fieldTypeD} = (mkName fieldNameD, defBang, genFieldT fieldTypeD)
          where
            genFieldT (ListD td)     = AppT (ConT ''[]) (genFieldT td)
            genFieldT (MaybeD td)    = AppT (ConT ''Maybe) (genFieldT td)
            genFieldT (BaseD name)   = ConT (mkName name)
            genFieldT (FuncD arg td) = AppT (ConT $ mkName arg) (AppT ArrowT (genFieldT td))
