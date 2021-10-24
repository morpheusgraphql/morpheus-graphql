# Morpheus and Freer-Simple
[Freer-Simple is a friendly effect system for Haskell](https://hackage.haskell.org/package/freer-simple)

For additional context on why one would choose to use an effect system to stucture their programs, and further, why one would pick `Freer-Simple` over the other optinons, please see this [excellent blog post](https://reasonablypolymorphic.com/blog/freer-monads/) by Sandy Maguire.

## Example
### Application Core
To start, let's come up with the core "types" for our example service.

```haskell
module Types where

import           Data.Text (Text)

data Deity = Deity {
  name  :: Name,
  power :: Power
} deriving (Show, Eq)

type Name = Text
type Power = Maybe Text
```

Nothing fancy, just modeling our world in Haskell.

After doing this, we know we need a way to interact with a database, and so we can implement an "interface" for this using a repository pattern, and keeping it abstract with the `freer-simple` library.

Let's define a DeityRepo:
```haskell
data Error = DeityDoesNotExist Name | Unknown deriving Show

data DeityRepo r where
  GetDeityByName :: Name -> DeityRepo (Either Error Deity)
  CreateDeity :: Deity -> DeityRepo (Either Error Deity)
```

We have just described the effects we need to link our pure core to the outside world. We want to be able to fetch a deity by name and to create a deity.

The `DeityRepo` type is our effect, and you can see that the funtions each indicate that they require the `DeityRepo` effect in order to run.

Next we can define a couple functions using `freer-simple` to expose this behavior.
```haskell
getDeityByName :: Member DeityRepo effs => Name -> Eff effs (Either Error Deity)
getDeityByName name = send $ GetDeityByName name

createDeity :: Member DeityRepo effs => Deity -> Eff effs (Either Error Deity)
createDeity deity = send $ CreateDeity deity
```

These can be read as

`getDeityByName` takes a name and returns either the deity or an error and in the process performs the DeityRepo effect.

Similarly, `createDeity` takes a deity and returns either the created deity or an error and in the process performs the DeityRepo effect.

### An Example Handler
Let's just throw together a way to handle these effects using an "in memory" implementation of this repo.
```haskell
deityIORef :: IO (IORef [Deity])
deityIORef = newIORef []

type DeityIORef = IORef [Deity]

exampleDeityRepoHandler :: DeityIORef -> Eff '[DeityRepo, IO] a -> IO a
exampleDeityRepoHandler dbRef =
  runM . interpretM handle
  where
    handle :: DeityRepo v -> IO v
    handle (GetDeityByName name) = do
      (deities :: [Deity]) <- readIORef dbRef
      let result = find (\(Deity name' _) -> name == name') deities
      pure $ toEither (DeityDoesNotExist name) result

    handle (CreateDeity diety) = do
      (deities :: [Deity]) <- readIORef dbRef
      writeIORef dbRef $ addOrReplace diety deities
      pure (Right diety)

addOrReplace :: Eq a => a -> [a] -> [a]
addOrReplace a as = a : filter (/= a) as

toEither :: b -> Maybe a -> Either b a
toEither b Nothing  = Left b
toEither _ (Just a) = Right a
```

Let's go over the type signature first
`exampleDeityRepoHandler :: DeityIORef -> Eff '[DeityRepo, IO] a -> IO a`
This handle is a function that takes an IO reference (so it can maintain state), and generates a function that can "handle" the DeityRepo effects and returns the result wrapped in an IO.

The implementation function is some `freer-simple` wiring with specific handlers for each effect. These handlers are using the IORef to maintain state.

### Graphql / Morpheus
Now lets come up with a simple schema and wire everything up.
```gql
type Query {
  deity(name: String!): Deity!
}

type Mutation {
  createDeity(name: String!, power: String): Deity!
}

type Deity {
  name: String!
  power: String @deprecated(reason: "some reason for")
}
```

Perfect! Now let's look at creating some resolvers.
```haskell
rootResolver :: Member DR.DeityRepo effs => RootResolver (Eff effs) () Query Mutation Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {deity = deityResolver},
      mutationResolver = Mutation {createDeity = createDeityResolver},
      subscriptionResolver = Undefined
    }
  where
    deityResolver (Arg name) =
      liftEither $ toResponse <$> DR.getDeityByName name

    createDeityResolver CreateDeityArgs {name, power}=
      liftEither $ toResponse <$> DR.createDeity (T.Deity name power)
```

This should look like the IO way of handling effects, except now the type signature of our get and create functions has bubbled up into `rootResolver` instead of IO. So
`rootResolver :: Member DR.DeityRepo effs => RootResolver (Eff effs) () Query Mutation Undefined`

instead of

`rootResolver :: RootResolver IO () Query Mutation Undefined`

While in this "effectful" context, we can more or less treat our DietyRepo functions as regular functions returning `Either`.

We can create an `api` with this root resolver:
```haskell
api :: (Member DR.DeityRepo effs, Typeable effs) => ByteString -> Eff effs ByteString
api = interpreter rootResolver
```

and we can continue to try to bubble up the effects as high as possible so we can defer actually applying handlers.

### Scotty
Scotty eventually because the bottleneck for this, since it eventually requires IO instead of `Eff`, but we can push it a little further using `Scotty.Trans` which is slightly more generic than normal Scotty.

```haskell
routes
  :: ( ScottyError e
     , Typeable effs
     , Member DeityRepo effs
     , LastMember IO effs
     )
  => ScottyT e (Eff effs) ()
routes = do
  post "/graphql" $ body >>= lift . api >>= raw
```
Here we can define our routes while still deferring handling the effects.

And finally we reach the furthest we can push the application of the handlers out
```haskell
server :: IO ()
server = server' True (pure ())

server' :: Bool -> IO () -> IO ()
server' showStart readyAction = do
    deityIORef' <- deityIORef
    scottyOptsT
      (Options showStartMessage settings)
      (exampleDeityRepoHandler deityIORef')
      routes
  where
    settings =
        setBeforeMainLoop readyAction $
            setPort 8080
            defaultSettings
    showStartMessage = if showStart then 1 else 0
```

We pass in our handlers into the scottyOptsT as the (m Response) -> (IO Response) args and the types are happy!