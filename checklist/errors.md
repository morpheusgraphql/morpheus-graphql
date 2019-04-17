# Errors:

### InputValue:

- **expectedAFoundB**: input value does matches to schema type ✅
- **undefinedField**: required field not found on input value ✅
- **unknownField**: field does not exists on inputObject ✅

### Mutation:

- **mutationNotSupported**: mutation is not defined by schema ✅

### Query

- **Arguments:** ✅

  - **unknownArgument**: ✅
  - **undefinedArgument**: ✅
  - **invalidInputValue -> InputValue**: ✅
  - **nameConflict**: ✅

- **Variable:** input Value Will be validated on query argument validation, because there will not be unused Variable all inputValues(variables) will be checked

  - **nameConflict**: ❌
  - **uninitializedVariable**: variable defined in query head ,but not found request body ✅
  - **undefinedVariable**: referenced variable is not defined by operation QueryName | MutationName ✅
  - **unknownType**: variable type does not exists ✅
  - **incompatibleVariableType**: argument references variable with different type ❌
  - **invalidInputValue**: validation of input fails ✅
  - **unusedVariable**: ✅

- **Selection:** ✅

  - **unknownField**: requested field does not exist on type ✅
  - **hasNoSubfields**: requested subfields but type is scalar Type ✅
  - **mustHaveSubfields**: requested as scalar but is object ✅
  - **nameConflict**: ✅

- **Spread:**

  - **unknownFragment**: ✅
  - **cannotBeSpreadOnType**: ✅
  - **nameConflict**: ❌

- **Fragment**: whole spread will be done there (for performance reason), Selection and Argument will be Validated on Query, because there will be no unused fragment we will validate all subfields.

  - **unusedFragment**: ❌
  - **unknownTargetType**: ✅
  - **cannotSpreadWithinItself**: ✅
  - **nameConflict**: duplicate fragment with same name ❌
