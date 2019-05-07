# 0.0.1 to 0.1.0
- interpreter can parse input Objects and Arrays (all input values)
- fix: argument supports nonNull and List Types in introspection
- add class Interpreter and instances for: ByteString , Text , Lazy ByteString, Lazy Text
- define public API, hide all internal modules.
  exposed-modules: Data.Morpheus, Data.Morpheus.Kind, Data.Morpheus.Types
- ::-> Resolver is now Monad
- Remove GQLObject, GQLEnum, GQLInput and Typeable, all objects can be derived just with (Generic,GQLKind)
