{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.Interface.Test
  ( test,
  )
where

import Data.Morpheus.Client
  ( declareLocalTypes,
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
  (path "Interface/schema.gql")
  (path "Interface/query.gql")

test :: TestTree
test =
  assertFetch
    "Interface"
    Nothing
    ()
    ( Right
        MyQuery
          { character =
              [ MyQueryCharacterDeity
                  { __typename = "Deity",
                    name = "Deity Name",
                    power = "Deity Power"
                  },
                MyQueryCharacterCharacter
                  { __typename = "Character",
                    name = "Character Name"
                  },
                MyQueryCharacterHero
                  { __typename = "Hero",
                    name = "Hero Name",
                    hobby = "Deity Power"
                  }
              ],
            character2 =
              [ MyQueryCharacter2Character
                  { name1 = "test name",
                    name = "test name"
                  }
              ],
            character3 =
              [ MyQueryCharacter3Hero
                  { __typename = "Hero",
                    name2 = "Hero name2",
                    hobby = "Hero Hobby"
                  },
                MyQueryCharacter3Character
                  { name2 = "This should be a Deity!"
                  },
                MyQueryCharacter3Character
                  { name2 = "Character name2"
                  }
              ],
            character4 =
              [ MyQueryCharacter4Character,
                MyQueryCharacter4Hero
                  { __typename = "Hero",
                    hobby = "Hero Hobby"
                  },
                MyQueryCharacter4Character
              ]
          }
    )
