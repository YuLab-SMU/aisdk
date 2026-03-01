# Model Metadata Collection & Curation Guidelines

## 1. Overview & Purpose

Aisdk uses centralized, static JSON files (stored in `inst/extdata/models/`) to manage the model lists and capabilities for various AI providers (e.g., Stepfun, Volcengine, DeepSeek). 

While the framework can automatically fetch the basic model IDs through APIs, **human curation is required** to enrich these files with advanced metadata. This enriched data powers core framework features, including:
- **Intelligent Context Routing**: Automatically limiting inputs based on `context_window` and `max_input_tokens`.
- **Cost Estimation**: Providing accurate pricing calculations based on token usage.
- **Capability Filtering**: Allowing users to search for models that specifically support vision or tool calling.
- **Dynamic Documentation**: Generating rich Roxygen helps (`?create_provider`) with capability tags and context hints.

This guide ensures high quality, consistency, and reliability across all model data collection efforts.

---

## 2. Standard Workflow

When adding a new provider or updating an existing one, please follow this standard operating procedure:

1. **Auto-Sync First**: Never write the JSON from scratch manually. Use the built-in R tool to pull the latest endpoints directly from the provider:
   ```r
   Sys.setenv(PROVIDER_API_KEY = "your_key")
   aisdk:::update_provider_models("provider_name")
   ```
   *This safely updates the ID list while preserving any existing manual data.*

2. **Locate Official Sources**: Find the official provider pricing page and model capabilities documentation. 
   *(Example: [Volcengine Doubao Pricing](https://console.volcengine.com/ark/region:ark+cn-beijing/model/detail?Id=doubao-seed-2-0-pro))*

3. **Manual Enrichment**: Open the generated `inst/extdata/models/<provider>.json` file and manually add the advanced schema fields (defined below) to the high-value or flagship models.

4. **Verification**: Run `devtools::document()` to ensure the JSON parses correctly and the Roxygen docs update without errors.

---

## 3. Schema Data Dictionary

Every model in the JSON array is a dictionary. Below is the full schema. **All fields except `id` and `type` are optional.** Do not guess values; if information is missing from official docs, omit the field.

### Core Identity
- **`id`** *(String, Required)*: The exact model string passed to the API (e.g., `"doubao-seed-2-0-pro-260215"`).
- **`type`** *(String, Required)*: The modality type. Defaults to `"language"`. Can also be `"audio"`.
- **`description`** *(String, Optional)*: A concise, 1-2 sentence description of the model's intended use case.
- **`family`** *(String, Optional)*: The model family or series (e.g., `"Doubao-Seed-2.0"`). Useful for grouping.

### Capabilities (`capabilities` object)
Booleans representing what the model supports natively.
- **`reasoning`**: `true` if it's a deep-thinking model (like DeepSeek-R1 or o1/o3).
- **`vision`**: `true` if it accepts image bounding/understanding.
- **`audio_input`**: `true` if it accepts raw audio input.
- **`function_call`**: `true` if it reliably supports tool/function calling APIs.
- **`structured_output`**: `true` if it guarantees JSON schema adherence.
- **`search`**: `true` if it supports built-in web search grounding.

### Context Limits (`context` object)
Strictly integer values representing token limits.
- **`context_window`**: The total theoretical context window (e.g., `256000`).
- **`max_input_tokens`**: The hard limit on user inputs (often the same as `context_window`).
- **`max_output_tokens`**: The maximum tokens the model can generate in the response.
- **`max_reasoning_tokens`**: If a reasoning model, the limit specifically for the thinking sequence.

### Pricing (`pricing` object)
- **`input`**: Cost for input tokens (Float).
- **`output`**: Cost for output/generation tokens.
- **`cache_hit`**: Cost for cached input tokens (if applicable).
- **`unit`**: The standard pricing unit. **Must** be formatted clearly, typically `"CNY/M tokens"` (元/百万token) or `"USD/M tokens"`.

### Rate Limits (`rate_limits` object)
Default Tier 1 or general availability limits.
- **`tpm`**: Tokens per minute.
- **`rpm`**: Requests per minute.

### Example Fully Enriched JSON Object

```json
{
  "id": "doubao-seed-2-0-pro-260215",
  "type": "language",
  "description": "旗舰级全能通用模型，面向 complex reasoning 与 agent 场景。",
  "family": "Doubao-Seed-2.0",
  "capabilities": {
    "reasoning": true,
    "vision": true,
    "function_call": true,
    "search": true
  },
  "context": {
    "context_window": 256000,
    "max_output_tokens": 128000
  },
  "pricing": {
    "input": 3.2,
    "output": 16.0,
    "cache_hit": 0.64,
    "unit": "CNY/M tokens"
  },
  "rate_limits": {
    "tpm": 5000000,
    "rpm": 30000
  }
}
```

---

## 4. Best Practices & Quality Control

### Consistency Rules
1. **Pricing Units**: Always standardize to "per 1 Million tokens" (`CNY/M tokens` or `USD/M tokens`). Do not use K-tokens. If a provider prices per 1K, multiply by 1000.
2. **Boolean Strictness**: Capabilities must be explicitly `true` or omitted (`false` is assumed if omitted). Do not use string "true".
3. **No Placeholders**: Do not fill fields with `"N/A"`, `"0"`, or `""`. If a context window or price is unknown, completely remove the key.

### Submitting PRs
When team members or contributors submit PRs to update `.json` files:
- The PR description **must include a URL** to the official pricing or model capability documentation used as the source of truth.
- Reviewers should spot-check the pricing math and context bounds against the provided link. 
- Only enrich production-ready or heavily utilized models. Do not waste time manually enriching legacy or deprecated model versions unless specifically requested.
