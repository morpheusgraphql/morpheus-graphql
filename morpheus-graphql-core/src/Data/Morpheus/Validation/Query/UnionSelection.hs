{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Query.UnionSelection
  ( validateUnionSelection,
    validateInterfaceSelection,
  )
where

import Data.Morpheus.Ext.SemigroupM
  ( join,
  )
import Data.Morpheus.Internal.Utils
  ( elems,
    empty,
    fromElems,
  )
import Data.Morpheus.Types.Internal.AST
  ( DataUnion,
    Fragment (..),
    IMPLEMENTABLE,
    Position (..),
    RAW,
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    SelectionSet,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    UnionTag (..),
    VALID,
    mkType,
    toCategory,
  )
import Data.Morpheus.Types.Internal.Validation
  ( FragmentValidator,
    Scope (..),
    askInterfaceTypes,
    askTypeMember,
    asksScope,
  )
import Data.Morpheus.Validation.Query.Fragment
  ( ResolveFragment (resolveValidFragment),
    castFragmentType,
  )
import Relude hiding (empty, join)

-- returns all Fragments used for Possible Types
splitFragment ::
  (ResolveFragment s) =>
  ( Fragment RAW ->
    FragmentValidator s (SelectionSet VALID)
  ) ->
  [TypeDefinition IMPLEMENTABLE VALID] ->
  Selection RAW ->
  FragmentValidator s ([UnionTag], [Selection RAW])
splitFragment _ _ x@Selection {} = pure ([], [x])
splitFragment f types (Spread _ ref) = pureFirst <$> resolveValidFragment f (typeName <$> types) ref
splitFragment f types (InlineFragment fragment@Fragment {fragmentType}) =
  pureFirst . UnionTag fragmentType
    <$> ( castFragmentType Nothing (fragmentPosition fragment) (typeName <$> types) fragment
            >>= f
        )

pureFirst :: a1 -> ([a1], [a2])
pureFirst x = ([x], [])

exploreFragments ::
  (ResolveFragment s) =>
  ( Fragment RAW ->
    FragmentValidator s (SelectionSet VALID)
  ) ->
  [TypeDefinition IMPLEMENTABLE VALID] ->
  SelectionSet RAW ->
  FragmentValidator s ([UnionTag], SelectionSet RAW)
exploreFragments validateFragment types selectionSet = do
  (tags, selections) <- unzip <$> traverse (splitFragment validateFragment types) (elems selectionSet)
  selectionPosition <- fromMaybe (Position 0 0) <$> asksScope position
  (concat tags,)
    <$> fromElems
      ( ( Selection
            { selectionName = "__typename",
              selectionAlias = Nothing,
              selectionPosition,
              selectionArguments = empty,
              selectionContent = SelectionField,
              selectionDirectives = empty
            }
        )
          : concat selections
      )

-- sorts Fragment by conditional Types
-- [
--   ( Type for Tag User , [ Fragment for User] )
--   ( Type for Tag Product , [ Fragment for Product] )
-- ]
tagUnionFragments ::
  [UnionTag] ->
  [TypeDefinition IMPLEMENTABLE VALID] ->
  [(TypeName, [SelectionSet VALID])]
tagUnionFragments fragments = filter (not . null . snd) . map categorizeType
  where
    -- TODO: fragments should be merged and not selected by type
    categorizeType ::
      TypeDefinition IMPLEMENTABLE VALID ->
      (TypeName, [SelectionSet VALID])
    categorizeType datatype =
      ( typeName datatype,
        unionTagSelection
          <$> filter
            (isSubTypeOf datatype . unionTagName)
            fragments
      )

isSubTypeOf :: TypeDefinition a s -> TypeName -> Bool
isSubTypeOf TypeDefinition {typeName, typeContent = DataObject {objectImplements}} name =
  name `elem` (typeName : objectImplements)
isSubTypeOf TypeDefinition {typeName} name = typeName == name

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
joinClusters ::
  forall s.
  SelectionSet VALID ->
  [(TypeName, [SelectionSet VALID])] ->
  FragmentValidator s (SelectionContent VALID)
joinClusters selSet [] = pure (SelectionSet selSet)
joinClusters selSet typedSelections =
  traverse mkUnionTag typedSelections
    >>= fmap (UnionSelection selSet) . fromElems
  where
    mkUnionTag :: (TypeName, [SelectionSet VALID]) -> FragmentValidator s UnionTag
    mkUnionTag (typeName, fragments) = UnionTag typeName <$> join (selSet :| fragments)

validateInterfaceSelection ::
  ResolveFragment s =>
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  (TypeDefinition IMPLEMENTABLE VALID -> SelectionSet RAW -> FragmentValidator s (SelectionSet VALID)) ->
  TypeDefinition IMPLEMENTABLE VALID ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionContent VALID)
validateInterfaceSelection
  validateFragment
  validate
  typeDef@TypeDefinition {typeName}
  inputSelectionSet = do
    possibleTypes <- askInterfaceTypes typeDef
    (spreads, selectionSet) <- exploreFragments validateFragment possibleTypes inputSelectionSet
    validSelectionSet <- validate typeDef selectionSet
    let tags = tagUnionFragments spreads possibleTypes
    let interfaces = concatMap snd $ filter ((typeName ==) . fst) tags
    let onlyTypes = filter ((typeName /=) . fst) tags
    xs <- join (validSelectionSet :| interfaces)
    joinClusters xs onlyTypes

mkUnionRootType :: FragmentValidator s (TypeDefinition IMPLEMENTABLE VALID)
mkUnionRootType = (`mkType` DataObject [] empty) <$> asksScope currentTypeName

validateUnionSelection ::
  ResolveFragment s =>
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  (TypeDefinition IMPLEMENTABLE VALID -> SelectionSet RAW -> FragmentValidator s (SelectionSet VALID)) ->
  DataUnion VALID ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionContent VALID)
validateUnionSelection validateFragment validate members selectionSet = do
  -- get union Types defined in GraphQL schema -> (union Tag, union Selection set)
  -- [("User", FieldsDefinition { ... }), ("Product", FieldsDefinition { ...
  unionTypes <- traverse (fmap toCategory . askTypeMember) members
  -- find all Fragments used in Selection
  (spreads, selSet) <- exploreFragments validateFragment unionTypes selectionSet
  typeDef <- mkUnionRootType
  validSelection <- validate typeDef selSet
  joinClusters validSelection (tagUnionFragments spreads unionTypes)
