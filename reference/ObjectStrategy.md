# Object Strategy

Object Strategy

Object Strategy

## Details

Strategy for generating structured objects based on a JSON Schema. This
strategy instructs the LLM to produce valid JSON matching the schema,
and handles parsing and validation of the output.

## Super class

[`aisdk::OutputStrategy`](https://YuLab-SMU.github.io/aisdk/reference/OutputStrategy.md)
-\> `ObjectStrategy`

## Public fields

- `schema`:

  The schema definition (from z_object, etc.).

- `schema_name`:

  Human-readable name for the schema.

## Methods

### Public methods

- [`ObjectStrategy$new()`](#method-ObjectStrategy-new)

- [`ObjectStrategy$get_instruction()`](#method-ObjectStrategy-get_instruction)

- [`ObjectStrategy$validate()`](#method-ObjectStrategy-validate)

- [`ObjectStrategy$clone()`](#method-ObjectStrategy-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the ObjectStrategy.

#### Usage

    ObjectStrategy$new(schema, schema_name = "json_schema")

#### Arguments

- `schema`:

  A schema object created by z_object, z_array, etc.

- `schema_name`:

  An optional name for the schema (default: "json_schema").

------------------------------------------------------------------------

### Method `get_instruction()`

Generate the instruction for the LLM to output valid JSON.

#### Usage

    ObjectStrategy$get_instruction()

#### Returns

A character string with the prompt instruction.

------------------------------------------------------------------------

### Method `validate()`

Validate and parse the LLM output as JSON.

#### Usage

    ObjectStrategy$validate(text, is_final = FALSE)

#### Arguments

- `text`:

  The raw text output from the LLM.

- `is_final`:

  Logical, TRUE if this is the final output.

#### Returns

The parsed R object (list), or NULL if parsing fails.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ObjectStrategy$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
