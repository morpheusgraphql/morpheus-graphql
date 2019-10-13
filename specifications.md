# Definitions:

- ✅ : supports for all version
- ✅ (>= x.y.z) : supports from version x.y.z
- ⛔️ : does not supports for all version
- 🚧 : work In Progress
- 🧪 : Unit Test

# GHC and LTS Haskell Versions:

- lts-11.10 (ghc-8.2.2): ✅ (>= 0.1.0)
- lts-12.16 (ghc-8.4.4): ✅
- lts-13.24(ghc-8.6.5): ✅

# Feature Checklist

- Types in Schema:

  - `ID`: ✅ (v0.1.0)
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
  - `Subscription`: ✅ (>= 0.1.0)

- Parser can read:

  - Query

    - Selection: ✅
    - arguments:
      - Values:
        - int: ✅
        - float: ✅
        - String: ✅
        - Array: ✅ (>= 0.1.0 )
        - Input Object: ✅ (>= 0.1.0 )
        - Enum: ✅
        - null : ✅ (>= 0.1.0 )
    - Fragments: ✅
    - Inline Fragments: (>= 0.1.0)
    - Aliases: ✅ (>= 0.1.0)
    - DefaultValues:  ✅ (>= 0.3.2)
    - AnonymousOperations: ✅  (>= 4.1)
    - Operations:
      - Query: ✅
      - Mutation: ✅
      - Subscription : ✅ (>= 0.1.0 )

  - Document:

    - type ✅ (>= 0.2.1 )
    - scalar ✅ (>= 0.2.1 )
    - input ✅ (>= 0.2.1 )
    - union ✅ (>= 0.2.1 )
    - interface, implements ✅ (>= 0.2.2 )

  - Comments: single & multi line comments (>= 0.2.2)

  - Arguments:

    - argumentTypes : ✅
    - wrappedTypes(`[T] or T!`): ✅ (>= v0.1.0)

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
  - **cannotBeSpreadOnType**: ✅ + 🧪
  - **nameConflict**: ⛔️

### Fragment Errors: ✅ + 🧪 (>= 2.0)

**Fragment**: whole spread will be done there (for performance reason),
Selection and Argument will be Validated on Query,
because there will be no unused fragment we will validate all sub fields.

- **unusedFragment**: ✅ + 🧪 (>= 2.0)
- **unknownTargetType**: ✅ + 🧪
- **cannotSpreadWithinItself**: ✅ + 🧪
- **nameConflict**: duplicate fragment with same name ✅ + 🧪 (>= 2.0)

### Variable Errors:

- **Variable:** input Value Will be validated on query argument validation, because there will not be unused Variable all inputValues(variables) will be checked

  - **nameConflict**: duplicated fields result in a parsing error ✅ + 🧪
  - **valueNotProvided**: variable defined in query head, but not found request body ✅ + 🧪
  - **undefinedVariable**: referenced variable is not defined by operation QueryName | MutationName ✅ + 🧪
  - **unknownType**: variable type does not exists ✅ + 🧪
  - **incompatibleVariableType**: argument references variable with different type ✅ + 🧪 (>= 0.1.0)
  - **invalidInputValue**: validation of input fails ✅ + 🧪
  - **unusedVariable**: ✅ + 🧪

### InputValue Errors:

- **expectedAFoundB**: input value does matches to schema type ✅
- **undefinedField**: required field not found on input value ✅
- **unknownField**: field does not exists on inputObject ✅
