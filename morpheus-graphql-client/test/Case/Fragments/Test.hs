{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.Fragments.Test
  ( test,
  )
where

import Data.Morpheus.Client
  ( FetchError (..),
    declareLocalTypes,
  )
import Relude
import Spec.Utils
  ( assertFetch,
    path,
  )
import Test.Tasty
  ( TestTree,
  )

declareLocalTypes
  (path "Fragments/schema.gql")
  (path "Fragments/query.gql")

result :: Either (FetchError MyQuery) MyQuery
result =
  Right
    ( MyQuery
        { character =
            [ MyQueryCharacterVariantDeity
                ( FragmentDEITY
                    { __typename = "Deity",
                      name = "Deity Name",
                      power = "Deity Power"
                    }
                ),
              MyQueryCharacter,
              MyQueryCharacterVariantHero
                ( FragmentHERO
                    { __typename = "Hero",
                      name = "Hero Name",
                      hobby = "Deity Power"
                    }
                )
            ],
          withNewTypes =
            [ MyQueryWithNewTypesVariantDeity
                ( MyQueryWithNewTypesDeity
                    { lastName = Nothing,
                      __typename = "Deity",
                      name = "Deity Name",
                      power = "Deity Power"
                    }
                ),
              MyQueryWithNewTypesVariantCharacter
                ( MyQueryWithNewTypesCharacter
                    { lastName = Nothing
                    }
                ),
              MyQueryWithNewTypesVariantCharacter
                ( MyQueryWithNewTypesCharacter
                    { lastName = Nothing
                    }
                )
            ],
          person =
            [ FragmentPerson
                { name = "test name",
                  lastName = Nothing
                }
            ],
          hero =
            [ MyQueryHeroVariantHero
                ( FragmentHERO
                    { __typename = "Hero",
                      name = "Hero name2",
                      hobby = "Hero Hobby"
                    }
                ),
              MyQueryHero,
              MyQueryHero
            ],
          superhero =
            [ MyQuerySuperhero,
              MyQuerySuperheroVariantHero
                ( FragmentHERO
                    { __typename = "Hero",
                      name = "Hero name2",
                      hobby = "Hero Hobby"
                    }
                ),
              MyQuerySuperhero
            ]
        }
    )

test :: TestTree
test = assertFetch "Fragments" Nothing () result