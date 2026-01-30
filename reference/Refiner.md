# Refiner R6 Class

Refiner R6 Class

Refiner R6 Class

## Public fields

- `agent`:

  The underlying Agent object

- `library`:

  AgentLibrary reference

## Methods

### Public methods

- [`Refiner$new()`](#method-Refiner-new)

- [`Refiner$refine()`](#method-Refiner-refine)

- [`Refiner$clone()`](#method-Refiner-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize Refiner

#### Usage

    Refiner$new(capabilities_text, library, model)

#### Arguments

- `capabilities_text`:

  Formatted capabilities table

- `library`:

  AgentLibrary instance

- `model`:

  Model to use

------------------------------------------------------------------------

### Method `refine()`

Analyze failure and generate improvement strategy

#### Usage

    Refiner$refine(task, plan, result, evaluation)

#### Arguments

- `task`:

  Original task description

- `plan`:

  Execution plan that was used

- `result`:

  Execution result

- `evaluation`:

  Evaluation from Evaluator

#### Returns

List with refinement strategy

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Refiner$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
