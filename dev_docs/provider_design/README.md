# aisdk Provider 设计与实现手册

> 本文档记录 V2 架构升级后 Provider 模块的设计原则、类继承体系、流式通信管线和扩展规范。  
> 新增 Provider 或修改现有逻辑前，请确保理解并遵循本文档。

---

## 1. 整体架构概览

```
┌─────────────────────────────────────────────────────────────────┐
│                         公共 API 层                             │
│  generate_text()  /  stream_text()     (core_api.R)            │
│  language_model("openai:gpt-4o")      (utils_registry.R)       │
└────────────────────────────┬────────────────────────────────────┘
                             │
┌────────────────────────────▼────────────────────────────────────┐
│                     抽象规范层 (spec_model.R)                    │
│  LanguageModelV1        GenerateResult       EmbeddingModelV1   │
└──────┬─────────────────────┬───────────────────────┬────────────┘
       │                     │                       │
┌──────▼─────────────┐ ┌─────▼──────────┐ ┌─────────▼────────────┐ ┌──────▼──────────┐
│ OpenAILanguageModel│ │Anthropic       │ │OpenAIEmbeddingModel  │ │GeminiLanguage-   │
│ (provider_openai)  │ │LanguageModel   │ │                      │ │Model (provider_  │
├────────────────────┤ │(provider_      │ └──────────────────────┘ │        gemini)   │
│ DeepSeekLM (继承)  │ │ anthropic)     │                        │                  │
│ NvidiaLM   (继承)  │ └────────────────┘                        └──────────────────┘
│ DoubaoLM   (继承)  │
│ XAI LM     (继承)  │
└────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                        流式管线层                                │
│  SSEAggregator + map_openai_chunk + map_anthropic_chunk         │
│  (sse_aggregator.R)                                             │
│  stream_from_api / stream_anthropic / stream_responses_api      │
│  (utils_http.R / provider_anthropic.R / provider_openai.R)      │
└─────────────────────────────────────────────────────────────────┘
```

---

## 2. 抽象规范层

### 2.1 LanguageModelV1 (基类)

**文件**: `R/spec_model.R`

所有 LLM Provider 必须继承此类。关键字段和方法：

| 字段/方法 | 说明 |
|-----------|------|
| `provider` | 字符串标识符，如 `"openai"`, `"anthropic"` |
| `model_id` | 模型 ID，如 `"gpt-4o"`, `"claude-sonnet-4-20250514"` |
| `capabilities` | 能力标签列表，如 `list(is_reasoning_model = TRUE)` |
| `has_capability(cap)` | 查询能力标签，返回 `TRUE/FALSE` |
| `do_generate(params)` | 非流式生成（子类必须实现） |
| `do_stream(params, callback)` | 流式生成（子类必须实现） |
| `format_tool_result(id, name, content)` | 格式化工具结果为 API 消息 |
| `get_history_format()` | 返回 `"openai"` 或 `"anthropic"` |

### 2.2 GenerateResult

统一的生成结果对象（`lock_objects = FALSE`，允许 ReAct 循环动态附加字段）：

| 字段 | 说明 |
|------|------|
| `text` | 生成文本 |
| `reasoning` | 推理链内容（支持 o1/o3、DeepSeek、Claude Extended Thinking） |
| `tool_calls` | 工具调用列表，每项含 `id`, `name`, `arguments` |
| `finish_reason` | 停止原因：`"stop"`, `"tool_calls"`, `"tool_failure"` 等 |
| `usage` | Token 用量 `list(prompt_tokens, completion_tokens, total_tokens)` |
| `raw_response` | 原始 API 响应 |
| `response_id` | Responses API 的响应 ID（多轮对话） |
| `steps` / `all_tool_calls` | ReAct 循环动态注入 |

### 2.3 能力标签系统 (Capability Flags)

取代硬编码的 `grepl("^o[0-9]", model_id)` 检测。

```r
# 实例化时注入
OpenAILanguageModel$new(model_id, config, capabilities = list(
  is_reasoning_model = TRUE
))

# 使用时查询
if (self$has_capability("is_reasoning_model")) {
  body$max_completion_tokens <- params$max_tokens
} else {
  body$max_tokens <- params$max_tokens
}
```

OpenAI Provider 的 `initialize()` 会自动检测 `"^o[0-9]"` 模式并设置该标签，也支持手动覆盖。

---

## 3. 模板方法模式 (Template Method)

### 3.1 非流式生成

OpenAI 系 Provider 的 `do_generate()` 拆分为三步：

```
build_payload(params) → execute_request(url, headers, body) → parse_response(response)
```

- **`build_payload()`** — 构建请求 URL、Headers、Body（含 token 映射、工具格式化）
- **`execute_request()`** — 调用 `post_to_api()`（含重试和错误处理）
- **`parse_response()`** — 解析 choice → GenerateResult

**子类只需覆盖 `parse_response()`** 来提取特有字段：

```r
# DeepSeek: 提取 reasoning_content
DeepSeekLanguageModel <- R6::R6Class(
  inherit = OpenAILanguageModel,
  public = list(
    parse_response = function(response) {
      result <- super$parse_response(response)
      result$reasoning <- response$choices[[1]]$message$reasoning_content
      result
    }
  )
)
```

NVIDIA 的实现完全相同。这使得 DeepSeek 从 ~70 行缩减到 ~15 行，NVIDIA 从 ~80 行到 ~15 行。

### 3.2 流式生成

流式同样采用两步：

```
build_stream_payload(params) → do_stream() 使用 SSEAggregator
```

`do_stream()` 的核心逻辑：

```r
do_stream = function(params, callback) {
  payload <- self$build_stream_payload(params)
  agg <- SSEAggregator$new(callback)
  stream_from_api(payload$url, payload$headers, payload$body,
    callback = function(data, done) {
      map_openai_chunk(data, done, agg)
    })
  agg$build_result()
}
```

---

## 4. 流式通信管线

### 4.1 SSEAggregator (sse_aggregator.R)

**纯状态管理器**，不包含任何 HTTP 或 SSE 解析逻辑。

```
                      ┌───────────────┐
  SSE Event ──────▶   │ Event Mapper  │ ──────▶  SSEAggregator
  (provider-specific) │ (翻译函数)     │          (通用聚合器)
                      └───────────────┘
                                                      │
                                                      ▼
                                              GenerateResult
```

**核心方法**：

| 方法 | 适用格式 | 说明 |
|------|----------|------|
| `on_text_delta(text)` | 通用 | 累积文本，自动关闭推理块 |
| `on_reasoning_delta(text)` | 通用 | 累积推理文本，自动开启 `<think>` |
| `on_reasoning_start()` | Anthropic | 显式开启推理块 |
| `on_block_stop()` | Anthropic | 关闭推理块 |
| `on_tool_call_delta(tool_calls)` | OpenAI | 处理分块的工具调用 delta |
| `on_tool_start(index, id, name)` | Anthropic | 开始工具调用（block start） |
| `on_tool_input_delta(index, json)` | Anthropic | 累积 partial JSON 参数 |
| `on_finish_reason(reason)` | 通用 | |
| `on_usage(usage)` | 通用 | |
| `on_done()` | 通用 | 关闭推理块，发送 done 回调 |
| `build_result()` | 通用 | 解析工具参数，返回 `GenerateResult` |

### 4.2 Event Mappers

| 函数 | 适用场景 | 说明 |
|------|----------|------|
| `map_openai_chunk(data, done, agg)` | OpenAI + 所有 OAI 兼容的 Proxy | 翻译 `choices[0].delta` → aggregator |
| `map_anthropic_chunk(event_type, event_data, agg)` | 原生 Anthropic | 翻译 `content_block_*` 事件 → aggregator |

### 4.3 底层传输函数

| 函数 | 文件 | 说明 |
|------|------|------|
| `stream_from_api()` | `utils_http.R` | OpenAI 标准 SSE（`data: {...}`, `data: [DONE]`） |
| `stream_anthropic()` | `provider_anthropic.R` | 双路复用：自动识别 native Anthropic 或 OpenAI-proxy |
| `stream_responses_api()` | `provider_openai.R` | OpenAI Responses API 特有事件（`response.*`） |

### 4.4 推理内容 (Reasoning) 的流式处理

所有推理内容通过 `<think>` / `</think>` 标签包裹，由 SSEAggregator 自动管理状态转换。

| 来源 | 触发字段 |
|------|----------|
| DeepSeek / Doubao | `delta.reasoning_content` |
| Claude Extended Thinking | `thinking_delta` / `content_block_start.thinking` |
| OpenAI o1/o3 Responses API | `response.reasoning.delta` |

---

## 5. Provider 类继承体系

### 5.1 OpenAI 系

```
LanguageModelV1
  └── OpenAILanguageModel          # Chat Completions API
        ├── DeepSeekLanguageModel   # 仅覆盖 parse_response()
        ├── NvidiaLanguageModel     # 仅覆盖 parse_response()
        ├── VolcengineLanguageModel # 仅覆盖 parse_response()
        ├── StepfunLanguageModel    # 直接继承，OpenAI 全兼容
        └── XAILanguageModel       # 直接继承，OpenAI 全兼容
  └── OpenAIResponsesLanguageModel # Responses API (独立实现)
```

### 5.5 Bailian 系 (DashScope)

```
LanguageModelV1
  └── OpenAILanguageModel
        └── BailianLanguageModel    # 仅覆盖 parse_response()
```

### 5.6 OpenRouter 系

```
LanguageModelV1
  └── OpenAILanguageModel
        └── OpenRouterLanguageModel # 仅覆盖 parse_response()
```

### 5.2 Anthropic 系

```
LanguageModelV1
  └── AnthropicLanguageModel       # Messages API
```

### 5.3 Gemini 系

```
LanguageModelV1
  └── GeminiLanguageModel          # generateContent API
```

### 5.4 Provider 工厂类

每个 Provider 都有一个工厂类和对应的 `create_*()` 函数：

| 工厂类 | 创建函数 | 继承 | 模型创建方法 |
|--------|----------|------|-------------|
| `OpenAIProvider` | `create_openai()` | — | `language_model()`, `embedding_model()`, `responses_model()`, `smart_model()` |
| `DeepSeekProvider` | `create_deepseek()` | `OpenAIProvider` | `language_model()` |
| `NvidiaProvider` | `create_nvidia()` | `OpenAIProvider` | `language_model()` |
| `VolcengineProvider` | `create_volcengine()` | `OpenAIProvider` | `language_model()` |
| `BailianProvider` | `create_bailian()` | `OpenAIProvider` | `language_model()` |
| `OpenRouterProvider` | `create_openrouter()` | `OpenAIProvider` | `language_model()` |
| `XAIProvider` | `create_xai()` | `OpenAIProvider` | `language_model()` |
| `StepfunProvider` | `create_stepfun()` | `OpenAIProvider` | `language_model()` |
| `AnthropicProvider` | `create_anthropic()` | — | `language_model()` |
| `GeminiProvider` | `create_gemini()` | — | `language_model()` |
| `CustomProvider` | `create_custom_provider()` | — | `language_model()` (Dynamic Routing) |

- `smart_model(model_id, api_format=)` — 根据 model_id 自动选择 Chat Completions 或 Responses API
- 跨格式桥：`create_deepseek_anthropic()` — 使用 Anthropic API 格式访问 DeepSeek
- **动态工厂**：`create_custom_provider()` — 允许用户通过表单参数（Base URL, API Key, API Format 等）在运行时动态实例化底层基类，将其注册到全局 Registration 中，而无需编写新的 Provider 类。

### 5.5 代理 URL 兼容性 (Proxy URL Compatibility)

在构建 Provider （特别是像 Gemini 这样具有非标准路径结构的模型）时，必须考虑到用户可能会使用第三方 API 代理（如 `aihubmix.com` 等）。

为了达到与官方 Python/Node.js SDK 一致的“开箱即用”体验（即用户只需要将 `base_url` 指向代理域名的根路径）：
1. **防守性拼接**：`build_payload_internal()` 在组合最终 URL 前，应当检查 `base_url` 是否包含了必要的路由路径后缀（例如 Gemini 的 `/models` 或 OpenAI 的 `/v1`）。如果用户直接传入了简短的域名代理地址（如 `https://aihubmix.com/gemini`），代码需要自动补全后缀，而不是直接报错 404。
2. **去除末尾斜杠**：在 Provider 的 `initialize()` 中，统一使用 `sub("/$", "", base_url)` 去除用户可能意外传入的末尾斜杠。

**参考案例（GeminiProvider）：**
```R
base <- private$config$base_url
if (!grepl("/models$", base)) {
    base <- paste0(base, "/models")
}
url <- paste0(base, "/", self$model_id, ":", endpoint)
```

---

## 6. Token 限制映射

### Chat Completions API

```
用户传入 max_tokens →
  is_reasoning_model?
    YES → body$max_completion_tokens
    NO  → body$max_tokens
```

### Responses API

```
用户传入 max_tokens →
  显式 max_output_tokens? → body$max_output_tokens (总量限制)
  显式 max_answer_tokens? → body$max_tokens (仅回答)
  默认              → body$max_output_tokens (安全默认)
```

---

## 7. 工具调用 (Tool Calling)

### 7.1 工具格式适配

```r
# OpenAI 格式
Tool$to_api_format("openai") → list(type="function", function=list(name=..., ...))

# Anthropic 格式
Tool$to_api_format("anthropic") → list(name=..., description=..., input_schema=...)
```

### 7.2 工具结果回传

| Provider | `format_tool_result()` 格式 |
|----------|---------------------------|
| OpenAI | `list(role="tool", tool_call_id=..., content=...)` |
| Anthropic | `list(role="user", content=list(list(type="tool_result", ...)))` |
| Gemini | `list(role="tool", tool_call_id=..., tool_name=..., content=...)` |

### 7.3 工具断路器 (Circuit Breaker)

在 `core_api.R` 的 ReAct 循环中，两种熔断机制：

1. **重复调用检测** — 模型重复发起相同的工具调用 ≥3 次 → `finish_reason = "tool_failure"`
2. **连续错误检测** — 工具执行连续失败 ≥3 次 → `finish_reason = "tool_failure"`

非致命错误会作为工具结果返回模型，给予自我修正的机会。

---

## 8. 注册中心 (Registry)

**文件**: `R/utils_registry.R`

```r
# 使用 "provider:model" 格式
model <- language_model("openai:gpt-4o")
model <- language_model("ollama:llama3:70b")   # 多冒号安全

# 内部只按第一个冒号分割
pos <- regexpr(":", id)
provider_name <- substring(id, 1, pos - 1)
model_id <- substring(id, pos + 1)
```

Provider 通过 `register_provider(name, provider)` 注册到全局 `ProviderRegistry`。

### 8.1 动态注册 Custom Provider

对于前端表单化配置或运行时动态挂载的场景，可以使用 `create_custom_provider()` 和 `ProviderRegistry$register()`：

```r
# 1. 创建自定义 Provider
my_provider <- create_custom_provider(
  provider_name = "my_custom",
  base_url = "https://api.example.com/v1",
  api_key = "sk-xxxx",
  api_format = "chat_completions",
  use_max_completion_tokens = TRUE
)

# 2. 动态注册到系统
get_default_registry()$register("my_custom", my_provider)

# 3. 像内置 Provider 一样使用
agent_generate("my_custom:special-model-v1", prompt = "Hello")
```

---

## 9. 新增 Provider 检查清单

新增一个 OpenAI 兼容的 Provider 时，遵循以下步骤：

### 必须实现

- [ ] 创建 `R/provider_xxx.R`
- [ ] 模型类继承 `OpenAILanguageModel`，仅覆盖 `parse_response()` 提取特有字段
- [ ] Provider 类继承 `OpenAIProvider`，覆盖 `initialize()` 和 `language_model()`
- [ ] 创建 `create_xxx()` 工厂函数并 `@export`
- [ ] 在 `utils_registry.R` 中注册（如果需要 registry 支持）

### 不需要重写

- `build_payload()` / `build_stream_payload()` — 除非 API 格式有差异
- `do_generate()` / `do_stream()` — 模板方法自动组合
- 工具调用聚合逻辑 — `SSEAggregator` 统一处理
- Token 映射 — 由 capability 系统驱动

### 如果 API 不兼容 OpenAI

- 继承 `LanguageModelV1` 直接实现所有 `do_*` 方法
- 实现 `format_tool_result()` 和 `get_history_format()`
- 如需自定义流式处理，创建专用传输函数并使用 `SSEAggregator`

### 测试

- [ ] 创建 `tests/testthat/test-provider-xxx.R`
- [ ] 离线测试（Provider 创建、模型实例化、API key 警告）
- [ ] 在线测试（`skip_if_no_api_key()`，文本生成、工具调用）

---

## 10. 结构化输出 (Structured Outputs) 兼容策略

部分厂商（如 Stepfun 阶跃星辰）的 API 不支持标准的 `response_format = list(type="json_object")` 或复杂的 JSON Schema。强行传入可能导致 API 返回 400 错误。

为保证所有 Provider 都能统一通过 `generate_text(response_format = schema)` 获得结构化输出，对于不兼容的 Provider，SDK 采用**软降级注入**策略：

1. **Schema 拦截**：在 `LanguageModel` 的 payload 构建阶段，拦截原本要发往远端的 `response_format` 参数。
2. **System Prompt 注入**：将 Schema JSON 序列化后，连同强制要求输出 JSON 的指令（如 `"You must return your output strictly in valid JSON format. The JSON must adhere to the following schema: ..."`）自动追加到 `messages` 列表的 `system` prompt 中。
3. **参数降级/剔除**：将发送给 API 的真实 `response_format` 置空（`NULL` 或视情况降级为 `json_object`），彻底规避接口报错。

**参考实现：** 见 `StepfunLanguageModel` 中的 `process_response_format` 私有方法。

---

## 11. 文件索引

| 文件 | 职责 |
|------|------|
| `R/spec_model.R` | 抽象基类：`LanguageModelV1`, `EmbeddingModelV1`, `GenerateResult` |
| `R/sse_aggregator.R` | `SSEAggregator` + `map_openai_chunk()` + `map_anthropic_chunk()` |
| `R/utils_http.R` | `post_to_api()`, `stream_from_api()` |
| `R/provider_openai.R` | `OpenAILanguageModel`, `OpenAIResponsesLanguageModel`, `OpenAIProvider` |
| `R/provider_anthropic.R` | `AnthropicLanguageModel`, `stream_anthropic()`, `AnthropicProvider` |
| `R/provider_gemini.R` | `GeminiLanguageModel`, `GeminiProvider` |
| `R/provider_custom.R` | `create_custom_provider()` (动态路由工厂) |
| `R/provider_deepseek.R` | `DeepSeekLanguageModel`, `DeepSeekProvider`, `create_deepseek_anthropic()` |
| `R/provider_nvidia.R` | `NvidiaLanguageModel`, `NvidiaProvider` |
| `R/provider_volcengine.R` | `VolcengineLanguageModel`, `VolcengineProvider` |
| `R/provider_bailian.R` | `BailianLanguageModel`, `BailianProvider` |
| `R/provider_openrouter.R` | `OpenRouterLanguageModel`, `OpenRouterProvider` |
| `R/provider_xai.R` | `XAILanguageModel`, `XAIProvider` |
| `R/provider_stepfun.R` | `StepfunLanguageModel`, `StepfunProvider` (含 Structured Outputs 兼容处理) |
| `R/utils_registry.R` | `ProviderRegistry`, `language_model()`, `embedding_model()` |
| `R/core_api.R` | `generate_text()`, `stream_text()`，ReAct 循环 + 断路器 |
