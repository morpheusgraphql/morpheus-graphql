{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Morpheus.Validation.Query.UnionSelection
  ( validateUnionSelection
  )
where


import           Control.Monad                  ((>=>))

-- MORPHEUS
import           Data.Morpheus.Error.Selection  ( unknownSelectionField )
import           Data.Morpheus.Types.Internal.AST
                                                ( Selection(..)
                                                , SelectionContent(..)
                                                , Fragment(..)
                                                , SelectionSet
                                                , FieldsDefinition(..)
                                                , Name
                                                , RAW
                                                , VALID
                                                , SelectionSet
                                                , UnionTag(..)
                                                , Ref(..)
                                                , DataUnion
                                                )
import qualified Data.Morpheus.Types.Internal.AST.MergeSet as MS
                                                ( join )
import           Data.Morpheus.Types.Internal.Operation
                                                ( Listable(..) 
                                                , selectOr
                                                , empty
                                                , singleton
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( SelectionValidator
                                                , askTypeMember
                                                , askScopeTypeName
                                                )
import           Data.Morpheus.Validation.Query.Fragment
                                                ( castFragmentType
                                                , resolveSpread
                                                )


type TypeDef = (Name, FieldsDefinition)

-- returns all Fragments used in Union
exploreUnionFragments
  :: [Name]
  -> Selection RAW
  -> SelectionValidator [Fragment]
exploreUnionFragments unionTags = splitFrag
 where
  packFragment fragment = [fragment]
  splitFrag
    :: Selection RAW -> SelectionValidator [Fragment]
  splitFrag (Spread ref) = packFragment <$> resolveSpread unionTags ref 
  splitFrag Selection { selectionName = "__typename",selectionContent = SelectionField } = pure []
  splitFrag Selection { selectionName, selectionPosition } = do
    typeName <- askScopeTypeName
    failure $ unknownSelectionField typeName (Ref selectionName selectionPosition)
  splitFrag (InlineFragment fragment) = packFragment <$>
    castFragmentType Nothing (fragmentPosition fragment) unionTags fragment

-- sorts Fragment by contitional Types
-- [
--   ( Type for Tag User , [ Fragment for User] )
--   ( Type for Tag Product , [ Fragment for Product] )
-- ]
tagUnionFragments
  :: [TypeDef] -> [Fragment] -> [(TypeDef, [Fragment])]
tagUnionFragments types fragments 
    = filter notEmpty 
    $ map categorizeType types
 where
  notEmpty = not . null . snd
  categorizeType :: (Name, FieldsDefinition) -> (TypeDef, [Fragment])
  categorizeType datatype = (datatype, filter matches fragments)
    where matches fragment = fragmentType fragment == fst datatype


{-
    - all Variable and Fragment references will be: resolved and validated
    - unionTypes: will be clustered under type names
      ...A on T1 {<SelectionA>}
      ...B on T2 {<SelectionB>}
      ...C on T2 {<SelectionC>}
      will be become : [
          UnionTag "T1" {<SelectionA>},
          UnionTag "T2" {<SelectionB>,<SelectionC>}
      ]
 -}
validateCluster
      :: (TypeDef -> SelectionSet RAW -> SelectionValidator (SelectionSet VALID))
      -> SelectionSet RAW
      -> [(TypeDef, [Fragment])]
      -> SelectionValidator (SelectionContent VALID)
validateCluster validator __typename = traverse _validateCluster >=> fmap UnionSelection . fromList
 where
  _validateCluster :: (TypeDef, [Fragment]) -> SelectionValidator UnionTag
  _validateCluster  (unionType, fragmets) = do
        fragmentSelections <- MS.join (__typename:map fragmentSelection fragmets)
        UnionTag (fst unionType) <$> validator unionType fragmentSelections

validateUnionSelection 
    :: (TypeDef -> SelectionSet RAW -> SelectionValidator (SelectionSet VALID)) 
    -> SelectionSet RAW 
    -> DataUnion
    -> SelectionValidator (SelectionContent VALID)
validateUnionSelection validate  selectionSet members = do
    let (__typename :: SelectionSet RAW) = selectOr empty singleton "__typename" selectionSet
    -- get union Types defined in GraphQL schema -> (union Tag, union Selection set)
    -- [("User", FieldsDefinition { ... }), ("Product", FieldsDefinition { ...
    unionTypes <- traverse askTypeMember members
    -- find all Fragments used in Selection
    spreads <- concat <$> traverse (exploreUnionFragments members) (toList selectionSet)
    let categories = tagUnionFragments unionTypes spreads
    validateCluster validate __typename categories