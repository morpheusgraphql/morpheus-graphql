{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Data.Morpheus.Execution.Internal.Declare
  ( declareType
  ) where

import Language.Haskell.TH

--
-- MORPHEUS
import Data.Morpheus.Types.Internal.DataD (AppD(..), ConsD(..), FieldD(..), TypeD(..))

declareType :: [String] -> TypeD -> Dec
declareType derivingList TypeD {tName, tCons} =
  DataD [] (mkName tName) [] Nothing (map cons tCons) $ map derive ("Generic" : derivingList)
  where
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    derive className = DerivClause Nothing [ConT (mkName className)]
    cons ConsD {cName, cFields} = RecC (mkName cName) (map genField cFields)
      where
        genField FieldD {fieldNameD, fieldTypeD} = (mkName fieldNameD, defBang, genFieldT fieldTypeD)
          where
            genFieldT (ListD td) = AppT (ConT ''[]) (genFieldT td)
            genFieldT (MaybeD td) = AppT (ConT ''Maybe) (genFieldT td)
            genFieldT (BaseD name) = ConT (mkName name)
            genFieldT (ResD arg mon td) = InfixT argType arrow resultType
              where
                argType = ConT $ mkName arg
                arrow = ''->
                resultType = AppT (ConT $ mkName mon) (genFieldT td)