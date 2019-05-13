# Definitions:

- ✅ : supports for all version
- ✅ (>= x.y.z) : supports from version x.y.z
- ⛔️ : does not supports
- 🚧 : work In Progress
- 🧪 : Unit Test

# GHC and LTS Haskell Versions:

- 7.10 (ghc-8.0.1): ?
- 8.0 (ghc-8.0.2): ?
- 10.2 (ghc-8.2.2): ?
- 11.10 (ghc-8.2.2): ⛔️ crashes
- lts-12.0 (ghc-8.4.3): ✅
- nightly-2018-09-26: ✅

# Feature Checklist

- Types in Schema:

  - `ID`: ⛔️ (will be added in v0.1.0)
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

- Parser can read:
  - Selection: ✅
  - arguments:
    - Values:
      - int: ✅
      - float: ✅
      - String: ✅
      - Array: ✅ (>= 0.1.0 )
      - Input Object: ✅ (>= 0.1.0 )
      - Enum: ✅
  - Fragments: ✅
  - Inline Fragments: ⛔️
  - Aliases: ⛔️
  - Operator
    - Query: ✅
    - Muatation: ✅
    - Subscription : ✅ (>= 0.1.0 )
    - Operator Arguments:
      - argumentTypes : ✅
      - wrappedTypes(`[T] or T!`): ⛔️ will be solved in v0.1.0

# Rules:

### Operator Errors:

- `Mutation`:

  - **mutationNotSupported**: mutation is not defined by schema ✅

- `Subscription`:

  - **subscriptionNotSupported**: subscription is not defined by schema ✅ (>= 0.1.0)

## Selection Errors:

- `Arguments`: ✅

  - **unknownArgument**: ✅
  - **undefinedArgument**: ✅
  - **invalidInputValue -> InputValue**: ✅
  - **nameConflict**: ✅

- `Selection`: ✅ + 🧪

  - **unknownField**: requested field does not exist on type ✅ + 🧪
  - **hasNoSubFields**: requested subFields but type is scalar Type ✅ + 🧪
  - **mustHaveSubFields**: requested as scalar but is object ✅ + 🧪
  - **nameConflict**: ✅ + 🧪

* **Spread:**

  - **unknownFragment**: ✅
  - **cannotBeSpreadOnType**: ✅
  - **nameConflict**: ⛔️

### Fragment Errors:

**Fragment**: whole spread will be done there (for performance reason), Selection and Argument will be Validated on Query, because there will be no unused fragment we will validate all subfields.

- **unusedFragment**: ⛔️
- **unknownTargetType**: ✅
- **cannotSpreadWithinItself**: ✅ + 🧪
- **nameConflict**: duplicate fragment with same name ⛔️

### Variable Errors:

- **Variable:** input Value Will be validated on query argument validation, because there will not be unused Variable all inputValues(variables) will be checked

  - **nameConflict**: ⛔️
  - **uninitializedVariable**: variable defined in query head ,but not found request body ✅
  - **undefinedVariable**: referenced variable is not defined by operation QueryName | MutationName ✅
  - **unknownType**: variable type does not exists ✅
  - **incompatibleVariableType**: argument references variable with different type ⛔️
  - **invalidInputValue**: validation of input fails ✅
  - **unusedVariable**: ✅

### InputValue Errors:

- **expectedAFoundB**: input value does matches to schema type ✅
- **undefinedField**: required field not found on input value ✅
- **unknownField**: field does not exists on inputObject ✅
