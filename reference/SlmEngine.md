# SLM Engine Class

R6 class for managing local Small Language Model inference. Provides a
unified interface for loading model weights, running inference, and
managing model lifecycle.

## Public fields

- `model_path`:

  Path to the model weights file.

- `model_name`:

  Human-readable model name.

- `backend`:

  The inference backend ("onnx", "torch", "gguf").

- `config`:

  Model configuration parameters.

- `loaded`:

  Whether the model is currently loaded in memory.

## Methods

### Public methods

- [`SlmEngine$new()`](#method-SlmEngine-new)

- [`SlmEngine$load()`](#method-SlmEngine-load)

- [`SlmEngine$unload()`](#method-SlmEngine-unload)

- [`SlmEngine$generate()`](#method-SlmEngine-generate)

- [`SlmEngine$stream()`](#method-SlmEngine-stream)

- [`SlmEngine$info()`](#method-SlmEngine-info)

- [`SlmEngine$print()`](#method-SlmEngine-print)

- [`SlmEngine$clone()`](#method-SlmEngine-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new SLM Engine instance.

#### Usage

    SlmEngine$new(model_path, backend = "gguf", config = list())

#### Arguments

- `model_path`:

  Path to the model weights file (GGUF, ONNX, or PT format).

- `backend`:

  Inference backend to use: "gguf" (default), "onnx", or "torch".

- `config`:

  Optional list of configuration parameters.

#### Returns

A new SlmEngine object.

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

Load the model into memory.

#### Usage

    SlmEngine$load()

#### Returns

Self (invisibly).

------------------------------------------------------------------------

### Method `unload()`

Unload the model from memory.

#### Usage

    SlmEngine$unload()

#### Returns

Self (invisibly).

------------------------------------------------------------------------

### Method `generate()`

Generate text completion from a prompt.

#### Usage

    SlmEngine$generate(
      prompt,
      max_tokens = 256,
      temperature = 0.7,
      top_p = 0.9,
      stop = NULL
    )

#### Arguments

- `prompt`:

  The input prompt text.

- `max_tokens`:

  Maximum number of tokens to generate.

- `temperature`:

  Sampling temperature (0.0 to 2.0).

- `top_p`:

  Nucleus sampling parameter.

- `stop`:

  Optional stop sequences.

#### Returns

A list with generated text and metadata.

------------------------------------------------------------------------

### Method `stream()`

Stream text generation with a callback function.

#### Usage

    SlmEngine$stream(
      prompt,
      callback,
      max_tokens = 256,
      temperature = 0.7,
      top_p = 0.9,
      stop = NULL
    )

#### Arguments

- `prompt`:

  The input prompt text.

- `callback`:

  Function called with each generated token.

- `max_tokens`:

  Maximum number of tokens to generate.

- `temperature`:

  Sampling temperature.

- `top_p`:

  Nucleus sampling parameter.

- `stop`:

  Optional stop sequences.

#### Returns

A list with the complete generated text and metadata.

------------------------------------------------------------------------

### Method `info()`

Get model information and statistics.

#### Usage

    SlmEngine$info()

#### Returns

A list with model metadata.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for SlmEngine.

#### Usage

    SlmEngine$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SlmEngine$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
