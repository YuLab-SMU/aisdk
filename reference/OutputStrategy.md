# Output Strategy Interface

Output Strategy Interface

Output Strategy Interface

## Details

Abstract R6 class defining the interface for output strategies.
Subclasses must implement `get_instruction()` and `validate()`.

## Methods

### Public methods

- [`OutputStrategy$new()`](#method-OutputStrategy-new)

- [`OutputStrategy$get_instruction()`](#method-OutputStrategy-get_instruction)

- [`OutputStrategy$validate()`](#method-OutputStrategy-validate)

- [`OutputStrategy$clone()`](#method-OutputStrategy-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the strategy.

#### Usage

    OutputStrategy$new()

------------------------------------------------------------------------

### Method `get_instruction()`

Get the system prompt instruction for this strategy.

#### Usage

    OutputStrategy$get_instruction()

#### Returns

A character string with instructions for the LLM.

------------------------------------------------------------------------

### Method `validate()`

Parse and validate the output text.

#### Usage

    OutputStrategy$validate(text, is_final = FALSE)

#### Arguments

- `text`:

  The raw text output from the LLM.

- `is_final`:

  Logical, TRUE if this is the final output (not streaming).

#### Returns

The parsed and validated object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputStrategy$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
