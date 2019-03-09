{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.Spread
    ( spreadFieldsWhile
    )
where

import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Map                      as M
                                                ( lookup )
import           Data.List                      ( find )
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import           Data.Morpheus.Error.Fragment   ( unknownFragment )
import           Data.Morpheus.Types.Types      ( Validation(..)
                                                , QuerySelection(..)
                                                , SelectionSet
                                                , FragmentLib
                                                , Fragment(..)
                                                , GQLQueryRoot(..)
                                                )
import           Data.Morpheus.Types.Error      ( GQLError(..)
                                                , Position(..)
                                                , ErrorLocation(..)
                                                )
import qualified Data.Text                     as T
import           Data.List                      ( elemIndex )

shouldSpread :: [(Text, QuerySelection)] -> Bool
shouldSpread list = case find isFragment list of
    Just _  -> True
    Nothing -> False

isFragment :: (Text, QuerySelection) -> Bool
isFragment (key, Spread _ _) = True
isFragment (key, _         ) = False


generateGQLError (Position loc) lines = [ErrorLocation loc 0]

spreadError :: GQLQueryRoot -> Position -> MetaInfo -> [[Int] -> GQLError]
spreadError root loc meta =
    [ \_ -> GQLError
          { message   = T.concat ["Unknown fragment \"", key meta, "\"."]
          , locations = [errorLocation loc root]
          }
    ]

lineIndexAndNumber position lines = (length linesBefore + 1, maximum $ linesBefore)
    where linesBefore = filter (position >=) lines

errorLocation (Position loc) root = do
    let (line, position) = lineIndexAndNumber loc (lineMarks root)
    ErrorLocation line (loc - position)

validateSpread
    :: GQLQueryRoot
    -> FragmentLib
    -> Position
    -> Text
    -> Validation [(Text, QuerySelection)]
validateSpread root frags location key = case M.lookup key frags of
    Nothing -> Left $ spreadError root location metaData
    Just (Fragment _ _ (SelectionSet _ gqlObj)) -> pure gqlObj
    where metaData = MetaInfo { className = "", cons = "", key = key }

propagateSpread
    :: GQLQueryRoot
    -> (Text, QuerySelection)
    -> Validation [(Text, QuerySelection)]
propagateSpread root (key, Spread _ location) =
    validateSpread root (fragments root) location key
propagateSpread root (text, value) = pure [(text, value)]

spreadFields :: GQLQueryRoot -> SelectionSet -> Validation SelectionSet
spreadFields root selectors = concat <$> mapM (propagateSpread root) selectors

spreadFieldsWhile :: GQLQueryRoot -> SelectionSet -> Validation SelectionSet
spreadFieldsWhile root selectors = spreadFields root selectors >>= checkUpdate
  where
    checkUpdate x = if shouldSpread x then spreadFieldsWhile root x else pure x
