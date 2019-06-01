module Data.Morpheus.Resolve.Generics.Utils
  ( SelOf
  , RecSel
  ) where

import           GHC.Generics

type SelOf s = M1 S s (Rec0 ()) ()

type RecSel s a = M1 S s (Rec0 a)
