{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Document.Introspection
  ( deriveObjectRep
  ) where

import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Introspect (ObjectRep (..))
import           Data.Morpheus.Types.Internal.Data         (DataField (..))
import           Data.Morpheus.Types.Internal.DataD        (GQLTypeD (..), TypeD (..), unKindD)
import           GHC.Generics
import           Data.Text (pack)

deriveObjectRep :: GQLTypeD -> Q [Dec]
deriveObjectRep GQLTypeD {typeD = TypeD {tName}} = pure <$> instanceD (cxt []) appHead methods
  where
    appHead = appT (appT (conT ''ObjectRep) (conT $ mkName tName)) (conT ''())
     -- objectFieldTypes :: Proxy rep -> [((Text, DataField t), TypeUpdater)]
    methods =
      [ funD
          'objectFieldTypes
          [ clause
              []
              (normalB
                 [|const
                     [ ( ( Data.Text.pack ""
                         , DataField
                             { fieldArgs = ()
                             , fieldName = "boo"
                             , fieldType = "JOE"
                             , fieldTypeWrappers = []
                             , fieldHidden = False
                             })
                       , pure)
                     ]|])
              []
          ]
      ]
