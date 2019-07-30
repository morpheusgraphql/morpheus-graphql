data Query = Query { 
  deity :: ArgDeity -> ResM Deity
} deriving (Generic)

data ArgDeity = ArgDeity { 
  name :: [[String!]]!
  mythology :: String
} deriving (Generic)

data Query = Query { 
  deity :: ArgDeity -> ResM Deity
} deriving (Generic)

data ArgDeity = ArgDeity { 
  name :: [[String!]]!
  mythology :: String
} deriving (Generic)

data Deity = Deity { 
  fullName :: () -> ResM String!
  power :: () -> ResM String
} deriving (Generic)