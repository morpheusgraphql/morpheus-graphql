## [0.1.0] - TODO: release Date

### Added

- support for Union Types: `type instance KIND <type> = UNION`
- add `Interpreter` class with instances:

  - `ByteString`
  - `Text`
  - Lazy `ByteString`,
  - Lazy `Text`

- support of Parsing input values: `Objects`,`Arrays`

- TypeFamily `KIND` with:

  - `SCALAR`
  - `OBJECT`,
  - `ENUM`
  - `INPUT_OBJECT`
  - `UNION`

- `::->` Resolver is Monad

### Changed

- `GQLKind` renamed as `GQLType`
- types can be derived just with `(Generic,GQLType)`
- public API (all other modules are hidden):
  - Data.Morpheus
  - Data.Morpheus.Kind
  - Data.Morpheus.Types

## Fixed:

- introspection:
  - argument supports `Non-Null` and `List`
  - every field has correct kind

### Removed

- `GQLObject`: replaced with instance `type instance KIND <Type> = OBJECT`
- `GQLEnum`: replaced with instance `type instance KIND <Type> = ENUM`
- `GQLInput`: replaced with instance `type instance KIND <Type> = INPUT_OBJECT`
- `Typeable` : with new deriving it is not required anymore
- `Wrapper`: with TypeFamilies there is no need for `Wrapper`
