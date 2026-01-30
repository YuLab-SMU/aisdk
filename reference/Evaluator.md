# Evaluator R6 Class

Evaluator R6 Class

Evaluator R6 Class

## Public fields

- `agent`:

  The underlying Agent object

## Methods

### Public methods

- [`Evaluator$new()`](#method-Evaluator-new)

- [`Evaluator$evaluate()`](#method-Evaluator-evaluate)

- [`Evaluator$clone()`](#method-Evaluator-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize Evaluator

#### Usage

    Evaluator$new(model)

#### Arguments

- `model`:

  Model to use

------------------------------------------------------------------------

### Method `evaluate()`

Evaluate a task result

#### Usage

    Evaluator$evaluate(task, success_criteria, result)

#### Arguments

- `task`:

  Original task description

- `success_criteria`:

  Success criteria from Architect

- `result`:

  Execution result to evaluate

#### Returns

List with evaluation details

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Evaluator$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
