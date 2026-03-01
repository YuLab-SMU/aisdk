# R-Native Programmatic Sandbox Design (V1)

## 概述

随着大模型能力边界的扩展，基于 ReAct 范式的传统 function calling 暴露出一些局限性：
1. **多步骤长延迟**：模型需要执行 N 次 tool call 才能聚合 N 条数据，导致 N 次网络往返。
2. **上下文污染**：大量中间 JSON 数据被返回给模型，挤占了有效上下文窗口，极易触发 Token 限制。

为此，我们在 `aisdk` 引入了 **R-Native Programmatic Sandbox**（R 原生编程式沙盒）。核心思想：利用 R 语言本身强大的元编程和环境隔离能力，允许 LLM 通过执行一段包含控制流（循环/条件）和数据处理（`dplyr`/`purrr`）的 R 代码，在本地统一处理数据，最后仅通过 `print()` 返回提纯后的结论。

---

## 核心架构与机制

### 架构图

```text
  User Tools ──► SandboxManager ──► 隔离的 R 执行环境
                   │                  ├── user_tool_1()
                   │                  ├── user_tool_2()
                   │                  ├── dplyr::*
                   │                  └── purrr::*
                   │
                   ▼
               create_r_code_tool() ──► 单一的 “meta-tool” (execute_r_code)
                                      (注册送给 LLM)
```

### 1. `SandboxManager` (核心调度器)

`SandboxManager` 是一个 R6 类，负责管理整个沙盒环境：

*   **隔离环境 (`parent = baseenv()`)**：通过 `rlang::env()` 创建干净的执行环境，默认预加载基础统计函数、安全的基础控制流以及用户指定的 `preload_packages`（如 `dplyr` 和 `purrr`），防止模型执行极度危险的系统级指令。
*   **工具绑定 (`bind_tools`)**：将用户注册的 `Tool` 对象转换并在沙盒环境中定义为同名的纯 R 函数。LLM 生成的代码就像调用普通 R 函数一样调用这些工具。
*   **安全执行 (`execute`)**：使用 `eval(parse(text=...))` 在沙盒环境中执行代码。
    *   **输出捕获**：使用 `capture.output()` 静默捕获控制台输出（stdout）。
    *   **异常拦截**：通过 `tryCatch` 捕获所有代码级错误，并将明确的错误堆栈作为字符串返回，赋予 LLM **自我纠错 (Self-correction)** 的能力。
    *   **输出截断**：通过 `max_output_chars`（默认 8000）防止 `print(1:1e6)` 这种失控输出撑爆上下文。
*   **状态持久化 (Stateful)**：环境本身是持久的，LLM 可以在第一步执行 `x <- 1`，并在随后的生成步骤中读取 `x`。当集成在 `ChatSession` 中时，它直接挂载在 session 变量环境的下层，实现跨 Agent/对话的数据共享。
*   **工具签名提取 (`get_tool_signatures`)**：动态解析传入工具的 `z_schema`，按照 Roxygen/Markdown 风格生成每个工具函数的签名和文档，用于构建系统提示词。

### 2. `create_r_code_tool` (元工具生成器)

将 `SandboxManager` 包装为一个单一的、符合 LLM 标准通信接口的 `Tool` 对象，名为 `execute_r_code`。
当启用 sandbox 模式时，这个单一工具将**替换**用户传入的所有零散工具。LLM 只看到这一个工具，其描述中详细附带了沙盒的用法以及沙盒内可用工具函数的列表。

### 3. `create_sandbox_system_prompt` (提示词注入器)

生成一组强有力的 Guidelines，追加到主 `system_prompt` 中，引导 LLM 放弃“一步一个 tool call”的传统思维，转而思考如何“编写 `purrr::map()` 批量拉取数据并使用 `dplyr` 进行过滤”。

### 4. `core_api.R` 深度集成

沙盒模式对终端用户是透明的。在 `generate_text()` 和 `stream_text()` 中：
```r
res <- generate_text(..., tools = my_tools, sandbox = TRUE)
```
底层引擎会自动拦截，实例化 `SandboxManager`，提取签名，改写提示词，并将 `my_tools` 替换为单例 `execute_r_code`。原有的 ReAct 循环 (Max Steps) 和 Circuit Breaker 机制完全兼容这种运行方式。

---

## 优势与限制

### 优势

*   **极简的网络请求**：N 次查询合并为 1 次请求。
*   **Token 节约**：只返回聚合后的标量/小矩阵，抛弃中间大量的脏 JSON 数据。
*   **模型异构支持**：底层依旧是遵循 OpenAI/Anthropic 工具调用规范的通信范式，只是把代码解释器跑在本地。这使得**任何**具备 R 代码编写能力的大模型都能享受到高级的数据处理沙盒（如 DeepSeek-Coder, GPT-4o, Claude 3.5 Sonnet 等）。

### 限制 / 注意事项

*   **安全风险**：尽管环境变量被隔离，但本地 R 解释器依旧拥有访问宿主系统的能力。当前沙盒适用于本地工作流或可信内网环境。如果要对外作为 SaaS 开放，需结合 Docker / gVisor 级别的容器硬隔离。
*   **强依赖模型的代码能力**：模型必须熟练掌握 R 语法，尤其是 `dplyr` 数据操作。能力较弱的模型可能会陷入不断的语法错误修正循环中。
