# aisdk: AI SDK for R

A production-grade AI SDK for R featuring a layered architecture,
middleware support, robust error handling, and support for multiple AI
model providers.

## Architecture

The SDK uses a 4-layer architecture:

- **Specification Layer**: Abstract interfaces (LanguageModelV1,
  EmbeddingModelV1)

- **Utilities Layer**: Shared tools (HTTP, retry, registry, middleware)

- **Provider Layer**: Concrete implementations (OpenAIProvider, etc.)

- **Core Layer**: High-level API (generate_text, stream_text, embed)

## Quick Start

    library(aisdk)

    # Create an OpenAI provider
    openai <- create_openai()

    # Generate text
    result <- generate_text(
      model = openai$language_model("gpt-4o"),
      prompt = "Explain R in one sentence."
    )
    print(result$text)

    # Or use the registry for cleaner syntax
    get_default_registry()$register("openai", openai)
    result <- generate_text("openai:gpt-4o", "Hello!")

## See also

Useful links:

- <https://github.com/YuLab-SMU/aisdk>

- <https://YuLab-SMU.github.io/aisdk/>

- Report bugs at <https://github.com/YuLab-SMU/aisdk/issues>

## Author

**Maintainer**: Yonghe Xia <xiayh17@gmail.com>
