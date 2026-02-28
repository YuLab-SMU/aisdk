# Volcengine Language Model Class

Language model implementation for Volcengine's chat completions API.
Inherits from OpenAI model but adds support for Volcengine-specific
features like reasoning content extraction from models that support
`reasoning_content`.

## Super classes

[`aisdk::LanguageModelV1`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.md)
-\>
[`aisdk::OpenAILanguageModel`](https://YuLab-SMU.github.io/aisdk/reference/OpenAILanguageModel.md)
-\> `VolcengineLanguageModel`

## Methods

### Public methods

- [`VolcengineLanguageModel$parse_response()`](#method-VolcengineLanguageModel-parse_response)

- [`VolcengineLanguageModel$clone()`](#method-VolcengineLanguageModel-clone)

Inherited methods

- [`aisdk::LanguageModelV1$generate()`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.html#method-generate)
- [`aisdk::LanguageModelV1$has_capability()`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.html#method-has_capability)
- [`aisdk::LanguageModelV1$stream()`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.html#method-stream)
- [`aisdk::OpenAILanguageModel$build_payload()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAILanguageModel.html#method-build_payload)
- [`aisdk::OpenAILanguageModel$build_stream_payload()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAILanguageModel.html#method-build_stream_payload)
- [`aisdk::OpenAILanguageModel$do_generate()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAILanguageModel.html#method-do_generate)
- [`aisdk::OpenAILanguageModel$do_stream()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAILanguageModel.html#method-do_stream)
- [`aisdk::OpenAILanguageModel$execute_request()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAILanguageModel.html#method-execute_request)
- [`aisdk::OpenAILanguageModel$format_tool_result()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAILanguageModel.html#method-format_tool_result)
- [`aisdk::OpenAILanguageModel$get_config()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAILanguageModel.html#method-get_config)
- [`aisdk::OpenAILanguageModel$get_history_format()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAILanguageModel.html#method-get_history_format)
- [`aisdk::OpenAILanguageModel$initialize()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAILanguageModel.html#method-initialize)

------------------------------------------------------------------------

### Method `parse_response()`

Parse the API response into a GenerateResult. Overrides parent to
extract Volcengine-specific reasoning_content.

#### Usage

    VolcengineLanguageModel$parse_response(response)

#### Arguments

- `response`:

  The parsed API response.

#### Returns

A GenerateResult object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    VolcengineLanguageModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
