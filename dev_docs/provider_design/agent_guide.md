# AI Agent Guide: 自动维护与生成 Provider 的标准流程

> **Target Audience**: AI 编程助手、Agent 流程编排系统 (如 `aisdk` 自身构建的 SDK Maintainer Agent)。
> **Purpose**: 指导 Agent 如何在 `aisdk` V2 架构下，通过阅读外部 API 文档，全自动地生成、测试并注册一个新的大模型 API 供应商 (Provider)。

`aisdk` 的 V2 Provider 架构高度**解耦**且**模板化**，专门为“元数据驱动”和“AI 自动代码生成”优化。只要 Agent 遵循以下标准工作流，生成 Provider 代码的成功率接近 100%。

---

## 1. 为什么 V2 架构对 Agent 极其友好？

作为 Agent，你在生成代码时**不需要**关心底层的 HTTP 请求重试、SSE 流式块解析或工具调用的状态拼接，因为这些都已经被基类和底层聚合器处理好了。

1. **模板方法 (Template Method)**：对于 90% 兼容 OpenAI 格式的 API，你只需继承 `OpenAILanguageModel`，并重写 `parse_response()` 即可提取特殊字段（如 `<think>` 推理过程）。
2. **能力标签 (Capability Flags)**：通过在工厂函数注入 `capabilities = list(is_reasoning_model = TRUE)` 等标签，基类会自动处理 token 字段映射（如 `max_completion_tokens` vs `max_tokens`），你**无需编写 if-else**。
3. **黑盒流式聚合 (SSEAggregator)**：所有工具调用 (`tool_calls`) 分块拼接、推理状态切换 (`<think>`/`</think>`) 全由 `SSEAggregator` 自动处理，你只需确保 `map_openai_chunk` 能够解析 API 的 SSE delta 即可。
4. **严格的契约**：无论底层 API 是什么形状，你最终的唯一目标是填充并返回 `GenerateResult` 对象（包含 `text`, `reasoning`, `tool_calls`, `usage`, `finish_reason`）。

---

## 2. Agent 标准实施工作流 (Workflow)

当你接到形如“帮我接入 Groq / Together / SiliconFlow API”的任务时，请严格按以下四步执行：

### Step 1: 文档抓取与信息抽取 (Information Extraction)

阅读目标大模型的官方 API 文档，提取以下**元数据**：
- **Base URL**: 聊天补全接口的完整端点（例如 `https://api.groq.com/openai/v1`）。
- **Auth 方式**: 需要哪种 Header？是标准的 `Authorization: Bearer <token>` 还是定制化的如 `x-api-key: <token>`？
- **OpenAI 兼容度**: 
  - 是完全兼容吗？（响应体结构是否为标准的 `choices[0].message` / SSE 为 `choices[0].delta`）
  - 如果不完全兼容，差异在哪里？
- **特殊字段**: 是否有类似 DeepSeek 的 `reasoning_content`，或者在流式返回中是否有特殊的 token 使用量字段位置？
- **环境变量**: 社区通常使用什么环境变量来存储它的 API Key？（例如 `GROQ_API_KEY`）

### Step 2: 方案决策 (Decision Making)

- **Scenario A (完全兼容 OpenAI)**：只需要配置，不需要写任何解析逻辑。直接继承 `OpenAILanguageModel`。如果是完全换皮的服务，甚至只需配置 Base URL。
- **Scenario B (微小差异，如特殊的 Reasoning 字段)**：继承 `OpenAILanguageModel`，提供自定义的 `parse_response()`（必要时提供微调过的流式 mapper，但优先考虑通用 `map_openai_chunk` 是否能兼容）。
- **Scenario C (完全独立格式，类似 Anthropic)**：继承更底层的 `LanguageModelV1`，全量实现 `do_generate()`、`do_stream()`、`format_tool_result()` 等方法（难度较高，参考 `provider_anthropic.R`）。

### Step 3: 代码生成 (Code Generation)

在 `R/` 目录下创建 `provider_[名称].R`。代码结构通常包含两部分：

1. **模型类 (Language Model Class)**
   ```r
   #' @title [名称] Language Model
   #' @keywords internal
   [名称]LanguageModel <- R6::R6Class(
     "[名称]LanguageModel",
     inherit = OpenAILanguageModel, # 或 LanguageModelV1
     public = list(
       # 如果Auth头不同，重写基类的 private$get_headers()，但这需要在 public 初始化中处理，
       # 或最好在工厂函数 config 中传递 headers
       
       # 如果返回字段有特殊位置，重写 parse_response
       parse_response = function(response) {
         result <- super$parse_response(response)
         # 例如：提取特有字段
         result$reasoning <- response$choices[[1]]$message$some_special_field
         result
       }
     )
   )
   ```

2. **工厂函数 (Factory Function)**
   ```r
   #' @title Create [名称] Provider
   #' @description Creates a provider instance for the [名称] API.
   #' @param api_key API key (defaults to Sys.getenv("XXX_API_KEY")).
   #' @return A Provider object.
   #' @export
   create_[名称] <- function(api_key = Sys.getenv("XXX_API_KEY")) {
     config <- list(
       api_key = api_key,
       base_url = "https://api.example.com/v1",
       # 可以在这里注入特定的 Header
       headers = list(`x-custom-auth` = api_key)
     )
     
     # 能力标签检测逻辑
     detect_capabilities <- function(model_id) {
       caps <- list()
       if (grepl("reasoning", tolower(model_id))) caps$is_reasoning_model <- TRUE
       caps
     }
     
     list(
       language_model = function(model_id) {
         [名称]LanguageModel$new(
           model_id = model_id,
           config = config,
           capabilities = detect_capabilities(model_id)
         )
       }
     )
   }
   ```

### Step 4: 注册与测试 (Registration & Testing)

1. **注册中心集成**：
   修改 `R/utils_registry.R`，在 `ProviderRegistry` 中注册你的新 Provider：
   ```r
   # 在 utils_registry.R 的适当位置
   register_provider("[名称]", create_[名称]())
   ```

2. **生成测试用例**：
   在 `tests/testthat/` 目录下创建 `test-provider-[名称].R`。
   **必须包含**的离线测试：
   - 工厂函数和 API Key 回退机制测试（无 Key 时是否使用环境变量）。
   - HTTP Headers 组装是否正确（尤其是包含了特有的自定义头时）。
   - （可选）如果你重写了 `parse_response()`，写一个 mock 的 list() 来验证解析逻辑。

   **必须包含**的在线金丝雀测试（使用 `skip_if_no_api_key` 包裹）：
   - `test_that("generate_text works with [名称]", { ... })`
   - `test_that("stream_text works with [名称]", { ... })`

---

## 3. 常见陷阱与建议 (Gotchas & Tips)

1. **不要重复造轮子 (DRY)**：看到大段的工具调用累加、`<think>` 标签拼接逻辑，**立刻停手**。在流式处理中，直接引用 `SSEAggregator` 并尝试 `map_openai_chunk`。
2. **遵守 GenerateResult 契约**：不要在给用户的返回值里随意增加顶层自由字段。所有特殊的 API 返回内容，应当放入 `raw_response` 字段，只有规范抽象过的字段（`text`, `reasoning`, `tool_calls` 等）才能放在顶层。
3. **安全使用 Bash 工具**：当你完成代码生成后，使用 `Rscript -e 'devtools::test(filter = "provider-[名称]")'` 单独运行你的测试，如果成功即可宣告胜利。
4. **统一命名风格**：小写下划线用于包内函数与文件名 (`provider_groq.R`, `create_groq`)，大驼峰用于 R6 类 (`GroqLanguageModel`)。

祝你（Agent）好运！自动扩展 SDK 生态是未来的浪漫所在。
