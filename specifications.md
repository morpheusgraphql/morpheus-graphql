# GHC and LTS Haskell Versions:

- 7.10 (ghc-8.0.1): ?
- 8.0 (ghc-8.0.2): ?
- 10.2 (ghc-8.2.2): ?
- 11.10 (ghc-8.2.2): ⛔️ crashes
- lts-12.0 (ghc-8.4.3): ✅ (>=v0.01)
- nightly-2018-09-26: ✅ (>=v0.01)

# Feature Checklist

- Types:
  - `ID`: ⛔️ (will be in v0.1.0)
  - `Float`: ✅
  - `Int`: ✅
  - `String`: ✅
  - `Enum`: ✅
  - `Scalar`: ✅
  - `List`: ✅
  - `Object`: ✅
  - `Union`: ✅ (>= 0.1.0 )
  - `InputObject`: ✅
  - `Query`: ✅
  - `Mutation`: ✅
  - `Subscription`: 🚧 (>= 0.1.0)

# Case Definitions:

## Errors:

### InputValue:

- **expectedAFoundB**: input value does matches to schema type ✅
- **undefinedField**: required field not found on input value ✅
- **unknownField**: field does not exists on inputObject ✅

### Mutation:

- **mutationNotSupported**: mutation is not defined by schema ✅

### Subscription:

- **subscriptionNotSupported**: subscription is not defined by schema ✅ (>= 0.1.0)

### Query

- **Arguments:** ✅

  - **unknownArgument**: ✅
  - **undefinedArgument**: ✅
  - **invalidInputValue -> InputValue**: ✅
  - **nameConflict**: ✅

- **Variable:** input Value Will be validated on query argument validation, because there will not be unused Variable all inputValues(variables) will be checked

  - **nameConflict**: ⛔️
  - **uninitializedVariable**: variable defined in query head ,but not found request body ✅
  - **undefinedVariable**: referenced variable is not defined by operation QueryName | MutationName ✅
  - **unknownType**: variable type does not exists ✅
  - **incompatibleVariableType**: argument references variable with different type ⛔️
  - **invalidInputValue**: validation of input fails ✅
  - **unusedVariable**: ✅

- **Selection:** ✅ +tests

  - **unknownField**: requested field does not exist on type ✅
  - **hasNoSubFields**: requested subFields but type is scalar Type ✅
  - **mustHaveSubFields**: requested as scalar but is object ✅
  - **nameConflict**: ✅

- **Spread:**

  - **unknownFragment**: ✅
  - **cannotBeSpreadOnType**: ✅
  - **nameConflict**: ⛔️

- **Fragment**: whole spread will be done there (for performance reason), Selection and Argument will be Validated on Query, because there will be no unused fragment we will validate all subfields.

  - **unusedFragment**: ⛔️
  - **unknownTargetType**: ✅
  - **cannotSpreadWithinItself**: ✅
  - **nameConflict**: duplicate fragment with same name ⛔️
