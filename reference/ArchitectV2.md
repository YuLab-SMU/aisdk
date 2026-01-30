# Architect V2 R6 Class

Architect V2 R6 Class

Architect V2 R6 Class

## Public fields

- `agent`:

  The underlying Agent object

## Methods

### Public methods

- [`ArchitectV2$new()`](#method-ArchitectV2-new)

- [`ArchitectV2$plan()`](#method-ArchitectV2-plan)

- [`ArchitectV2$clone()`](#method-ArchitectV2-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize Architect V2

#### Usage

    ArchitectV2$new(capabilities_text, model)

#### Arguments

- `capabilities_text`:

  Formatted capabilities table

- `model`:

  Model to use

------------------------------------------------------------------------

### Method `plan()`

Plan a task with success criteria

#### Usage

    ArchitectV2$plan(task, execution_history = list())

#### Arguments

- `task`:

  Task description

- `execution_history`:

  List of previous execution attempts

#### Returns

List with plan including success criteria

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ArchitectV2$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
