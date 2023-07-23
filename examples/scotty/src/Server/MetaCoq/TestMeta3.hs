{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, ConstraintKinds, UndecidableInstances, KindSignatures, DeriveGeneric, TypeApplications, DeriveAnyClass #-}

module Server.MetaCoq.TestMeta3 where


import Data.Morpheus.Server -- .Types.GQLType
import Data.Morpheus.App
import Data.Morpheus.Server.Types
--import GHC.Types
import Prelude(Show, pure, (.), show, Char, Maybe(..), Monad, return)
-- import Data.Morpheus.Server.Deriving.Utils.GRep
import Server.MetaCoq.TestMeta
--import Test.Tasty
--import Test.Tasty.HUnit
import Data.String
-- import Data.Generics.Traversable
-- import Data.Generics.Traversable.Zipper
-- import Data.Generics.Traversable.TH
import Server.MetaCoq.TestMeta2
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types (RootResolver (..), Undefined, ID, GQLType, QUERY,
                            ResolverQ,
                            Resolver)
import           GHC.Generics
import Data.Text (Text)

--class U a
--instance U a
-- instance GTraversable c0 (TestMeta.Prod TestMeta.Global_env TestMeta.Term)
--mshow :: d -> Char
--mshow x = 'c'

--  foo1 :: String
-- foo1 = 
-- ==  gfoldMap @Show (pure . show) rec_def_term  -- :: [String]--[GHC.Types.Char]
data Query (m :: * -> *) = Query
  { globals :: ID -> m (Maybe (Prod Global_env Term))
  } deriving (Generic, GQLType)

--globalsResolver :: Monad m => ID -> Resolver QUERY m (Maybe (Prod Global_env Term))
--globalsResolver id = pure (Just (rec_def_term id))

--globalsResolver :: ID -> ResolverQ e Haxl (Prod Global_env Term)
--globalsResolver id = return (Just rec_def_term)

--(GHC.Generics.Rep (Prod Global_env Term)))

  -- hidden Data.Morpheus.Server.Deriving.Utils.GRep.GRep
-- foo =   GQLType (
--       (Resolver
--       (QUERY      ()       IO)
--         (Resolver QUERY   () IO
--          (ResolverValue (Resolver QUERY ()  IO)))
--         (GHC.Generics.Rep (Prod Global_env Term))


