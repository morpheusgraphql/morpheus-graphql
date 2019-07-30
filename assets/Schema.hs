data Query = Query
  { deity :: ArgDeity -> ResM (Maybe Deity)
  } deriving (Generic)

data ArgDeity = ArgDeity
  { name      :: Maybe [Maybe [Maybe [[Maybe [String]]]]]
  , mythology :: Maybe String
  } deriving (Generic)

data Deity = Deity
  { fullName :: () -> ResM (String)
  , power    :: () -> ResM (Maybe String)
  } deriving (Generic)
