# R 语言环境变量与 LLM 上下文交互设计

在 `aisdk` 项目中，R 语言的环境变量是连接底层 LLM（大语言模型）服务和上层上下文交互的核心桥梁。环境变量使得 API 密钥、模型名称、Base URL 等敏感和可变配置与纯代码逻辑解耦。

本文档详细说明了环境变量在进行 LLM 上下文交互的过程中的几个关键机制。

## 1. 基础配置读取：`Sys.getenv()`

一切的起点是 R 的内置函数 `Sys.getenv()`。在启动 R 进程时，系统环境变量（如由 `.bashrc`、`.zshrc` 或者 R 特有的 `.Renviron` 文件定义）被加载到内存中。
可以通过 `Sys.getenv("OPENAI_API_KEY")` 快速读取相关变量。

## 2. Provider（按提供商）降级初始化模式

当创建具体的 LLM 交互对象时（例如在 `provider_openai.R` 或 `provider_bailian.R` 中），系统采用了**“显式参数优先，环境变量兜底”**的模式。

以 `provider_bailian.R` 为例，内部在构造连接实例时使用了类似如下逻辑：

```r
api_key = api_key %||% Sys.getenv("DASHSCOPE_API_KEY")
base_url = base_url %||% Sys.getenv("DASHSCOPE_BASE_URL", "https://dashscope.aliyuncs.com/compatible-mode/v1")
model_id <- model_id %||% Sys.getenv("DASHSCOPE_MODEL", "qwen-plus")
```

*注：`%||%` 为内部函数，当左侧为空 (NULL/缺失) 时，则使用右侧的环境变量。*

这样确保了在构建提示词上下文并发送 HTTP 请求之前，鉴权凭证已经被动态绑定到当前的 LLM 的运行时 Context 中。

## 3. 配置集散管理层：`utils_env.R`

为了避免 `Sys.getenv()` 在代码中随意散落引发的管理真空，`aisdk` 将它封装在 `R/utils_env.R` 中提供统一的 API。

这些工具类函数提供了：
- **默认后备地址**：例如 `get_openai_base_url()` 会先读取 `OPENAI_BASE_URL`，如果未设置则自动重定向到默认的 API 地址（例如 `"https://api.openai.com/v1"`）。
- **动态热重载**：提供 `reload_env(path = ".Renviron")` 和 `update_renviron()` 函数。这允许用户在不重启 R 的交互式 Session 的情况下，更新并立刻应用新的环境变量进行大模型对话。

## 4. 驱动可视化应用上下文：`shiny_config_server.R`

在提供基于 Shiny 的图形交互界面时，UI 的初始化状态完全是由当前环境变量映射渲染的：

```r
openai_api_key = Sys.getenv("OPENAI_API_KEY"),
nvidia_model = Sys.getenv("NVIDIA_MODEL") %||% "z-ai/glm4.7"
```

这意味着用户在终端里的环境变量设定，将直接决定了基于 Web 的 Agent 交互能看到哪些默认的基础设施配置。这就保证了 CLI 行为与 GUI 行为之间的一致性。

## 5. MCP 客户端与外部执行环境继承：`mcp_client.R`

当 LLM 具备调用外部工具/沙箱的能力（Model Context Protocol）时，它需要确保执行端与 R 主进程拥有同样的配置上下文：

```r
# mcp_client.R 中构建执行环境
env = c(Sys.getenv(), env)
```

子进程通过继承当前的 `Sys.getenv()` 内容，保证了当大模型在运行脚本、访问外部网络 (比如 `Sys.getenv("GITHUB_TOKEN")`) 时使用的是当前交互会话的权限。

## 总结交互生命周期

总结 `aisdk` 中的环境变量交互生命周期如下：

1. **[OS / `.Renviron`]** 操作系统或配置文件加载环境变量。
2. **[R Runtime]** `Sys.getenv()` 将该配置驻留为全 Session 全局可用。
3. **[SDK 实例创建]** 调用如 `create_openai()` 等工厂方法，SDK 到系统环境中寻找 Token。
4. **[构建 Request]** 当触发如 `generate_text()` 时，底层将 Token 通过 `httr2::req_headers(Authorization = paste("Bearer", api_key))` 封装进请求头发送给模型。
5. **[子系统辐射]** 配置随之波及给 Shiny UI 或 MCP 工具流引擎，保持全链路的上下文和鉴权对齐。
