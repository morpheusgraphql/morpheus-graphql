## 0.28.0 (2024-05-22)

#### Breaking Change

- [#811](https://github.com/morpheusgraphql/morpheus-graphql/issues/811): Unified Deriving
  - 👤 [@nalchevanidze](https://github.com/nalchevanidze)
  - <details>
      **breaking change:** 
      
      directive defintinitions are required to be associated with `DIRECTIVE` type.
      ```hs
      instance GQLType MyDirective where
        type KIND Deprecated = DIRECTIVE
      ```
    </details>

#### New features

- [#836](https://github.com/morpheusgraphql/morpheus-graphql/issues/836): Add Suffixes directive
  - 👤 [@eunmin](https://github.com/eunmin)
  - <details>
      Hi~! :)
      I'm having a great time with morpheus-graphql. This time I wanted to rename a type, so I tried to use Prefixes, Rename, DropNamespaces, but there was nothing suitable, so I created the Suffixes directive. Please take a look and merge or comment. Thanks.
    </details>
- [#849](https://github.com/morpheusgraphql/morpheus-graphql/issues/849): Feature: disable introspection
  - 👤 [@nalchevanidze](https://github.com/nalchevanidze)
  - <details>
      In production, introspection can lead to safety problems, this function allows disabling introspection by configuration.
      
      you can disable introspection on your apps by : `disableIntrospection` function
    </details>

#### Bug Fixes

- [#805](https://github.com/morpheusgraphql/morpheus-graphql/issues/805): Support graphql-ws protocol, breaks existing subscriptions-transport-ws protocol
  - 📦 [client](https://hackage.haskell.org/package/morpheus-graphql-client)
  - 👤 [@ropwareJB](https://github.com/ropwareJB)
  - <details>
      + Bump support to newer graphql-ws subscription protocol from deprecated
      + yesod example with node client using graphql-ws npm package.
      + Bump version to v0.28.0
      
      This PR will cease support for the older protocol due to line, which changes the name of an event from 'data' to 'next':
      https://github.com/morpheusgraphql/morpheus-graphql/pull/805/files#diff-d7568c77ceef4d42fcddd7e99c510f1d7d53592431ab6ff0027f1667b6de68deR160
      
      Deprecation notice:
      https://www.npmjs.com/package/graphql-ws#disclaimer
      
      Graphql-ws protocol:
      https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md
      
      Deprecated subscriptions-transport-ws protocol:
      https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md
    </details>

#### Minor Changes

- [#832](https://github.com/morpheusgraphql/morpheus-graphql/issues/832): Optimize argument derivation
  - 👤 [@nalchevanidze](https://github.com/nalchevanidze)
- [#833](https://github.com/morpheusgraphql/morpheus-graphql/issues/833): useGQL classes
  - 👤 [@nalchevanidze](https://github.com/nalchevanidze)
- [#834](https://github.com/morpheusgraphql/morpheus-graphql/issues/834): improve deriving
  - 👤 [@nalchevanidze](https://github.com/nalchevanidze)
- [#835](https://github.com/morpheusgraphql/morpheus-graphql/issues/835): Gnmap proxy
  - 👤 [@nalchevanidze](https://github.com/nalchevanidze)
- [#840](https://github.com/morpheusgraphql/morpheus-graphql/issues/840): Fix Yesod ws example subscription
  - 👤 [@ropwareJB](https://github.com/ropwareJB)
  - <details>
      Yesod-pubsub example had a minor issue where the client was sending a malformed subscription request.
    </details>
- [#845](https://github.com/morpheusgraphql/morpheus-graphql/issues/845): Update slack channel link
  - 👤 [@nalchevanidze](https://github.com/nalchevanidze)
- [#856](https://github.com/morpheusgraphql/morpheus-graphql/issues/856): hlint
  - 👤 [@nalchevanidze](https://github.com/nalchevanidze)
- [#855](https://github.com/morpheusgraphql/morpheus-graphql/issues/855): Allow morpheus-graphql-code-gen-utils to build with GHC 9.8
  - 👤 [@ocharles](https://github.com/ocharles)
  - <details>
      Fixes #854
    </details>
- [#857](https://github.com/morpheusgraphql/morpheus-graphql/issues/857): attoparsec-aeson
  - 👤 [@nalchevanidze](https://github.com/nalchevanidze)
- [#853](https://github.com/morpheusgraphql/morpheus-graphql/issues/853): compat: Support Aeson 2.2.x
  - 👤 [@414owen](https://github.com/414owen)
- [#858](https://github.com/morpheusgraphql/morpheus-graphql/issues/858): setup-examples
  - 👤 [@nalchevanidze](https://github.com/nalchevanidze)
