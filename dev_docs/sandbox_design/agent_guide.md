# Agent Guide: Maintaining & Extending the R-Native Programmatic Sandbox

这份指南旨在为后续维护、扩展或调试 `aisdk` R 原生编程式沙盒环境（**R-Native Programmatic Sandbox**）的 AI Agent 提供核心准则。沙盒机制是打破传统 Function Calling 性能瓶颈的关键基础设施。

## 1. SandboxManager 的基础准则

`SandboxManager` (`R/sandbox.R`) 的本质是为一个隔离的 `rlang::env()` 提供了一层调度防弹衣，使其能够应对不可预测的大模型代码输出。

### 核心设计禁忌
- **绝对不要**直接向执行环境中暴露 `eval()`, `system()`, `system2()`, `source()` 或极易越权的危险函数（即除非特定用途，不要解除 `parent = baseenv()` 的隔离）。
- **拦截输出极度重要**：由于 LLM 非常喜欢 `print(large_dataframe)`，我们必须在 `SandboxManager$execute` 内坚持使用 `capture.output` 和硬截断逻辑（当前是 `max_output_chars`），否则长下文会被撑爆。
- **自我纠错原则**：错误不应该直接抛出给宿主程序（`stop()`）。你必须始终在 `tryCatch` 内捕获执行沙盒的过程，把详尽的错误堆栈转为普通字符串，随同工具调用一同返回给 LLM（详见 `SandboxManager$execute` 的实现）。

### 环境持久化约定
- 由于大模型习惯于像在 Jupyter Notebook 中一样使用沙盒（写一点代码保存变量，下一步再提取处理），我们通过拦截 `generate_text/stream_text` 的 `session`，使用 `session$get_envir()` 充当 `SandboxManager` 的 `parent_env` 以实现多轮对话下的变量继承。改动相关机制时，请务必保证对这套继承逻辑进行测试（`tests/testthat/test-sandbox.R` 中有对应的 `parent_env` 验证用例）。

## 2. 编写与注入 Tool 的相关要求

沙盒的一大优势在于将原本通过 HTTP JSON 的工具请求转换为本地的纯 R 函数执行。
当新加入一种全局特性或更改 `Tool` 对象定义 (`R/tool.R`) 时：
- **`run` 方法是灵魂**：任何工具最终在沙盒里都被包装为 `wrapper <- function(...) { tool_obj$run(...) }`。你需要确保所有的工具实现其逻辑均完整覆盖在 `$run()` 中。
- **工具参数与签名**：在实现新的 `create_sandbox_system_prompt()` 增强时，我们依赖 `extract_params_info()` 从工具的 `z_schema` 提取出函数签名（即把 JSON Schema 组装成类似于 Roxygen 的形态）。当更改任何 Schema 内部结构时（如支持了新的数组嵌套模式），请必定同步升级这套抽取逻辑。

## 3. 测试与验证策略

在每次修改沙盒行为（比如增加预加载的库、修改 `max_output_chars` 行为，或者更改错误捕捉格式）之后，务必执行完整的单元测试：

```bash
Rscript -e 'devtools::test(filter = "sandbox")'
```

### 添加新测试的场合
- **加入新的“预置库”**：当你在 `SandboxManager` 初始化函数中加入了类似于 `stringr` 或 `tidyr` 的包白名单时，应在 `test-sandbox.R` 中追加对于这些包导出函数的验证（类似测试用例 `"dplyr functions are available when preloaded"`）。
- **加强边界抗毁性**：如果发现了由特定古怪的 R 代码（闭包逃逸、内存死锁等）引发的崩溃问题，在修复调度器后必须添加针对性的 `tryCatch` 死锁跳脱测试。

## 4. Prompt Engineering 与集成机制

沙盒强依赖于良好的系统提示。`execute_r_code` 不仅仅是一个沙盒入口，更是一段包含所有子工具声明的指导性 Prompt。

- **Prompt 构建器 (`create_sandbox_system_prompt`)**：这是指导 LLM 如何使用 `purrr/dplyr`，如何 `print()`，以及一旦报错如何读取 traceback 的关键。如果观察到特定模型经常写出无效的 R 循环代码，可在这其中加入微型的 `Few-shot` 样本优化系统提示。
- **改写替换思想**：千万记住，对 `generate_text()` / `stream_text()` 来说，所谓开启沙盒 (`sandbox = TRUE`) 仅仅是把传入的一大串 `tools` 列表清空，替换为一个单一的名为 `execute_r_code` 的工具，并把之前的工具说明文字追加到系统提示词里（The Meta-Tool Pattern）。修改流程时应遵循这一拦截解耦原则，不要污染通用的 ReAct Loop 和核心引擎。
