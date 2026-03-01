# Agent Autonomy & Safety Design (Agent 自主性与安全性设计)

## 核心理念 (Core Philosophy)
在构建 AI Agent 时，自主性（Autonomy）与安全性（Safety）之间的张力是核心难题。传统框架往往受限于被动的“黑名单沙盒”或“完全放权”的执行模式。
`aisdk` 将“基于正则表达式黑名单的被动沙盒”演进为了一个**具备“知情同意”（Human-in-the-Loop）、细粒度资产保护和主动权限分级的现代 Agent OS 环境**。

本文档概述了 `aisdk` 如何实现 Agent 的 Autonomy 与 Safety 的平衡及核心模块。

## 架构组成部分 (Architecture Components)

### 1. 静态抽象语法树安全分析 (AST Static Analysis)
- **位置**: `R/utils_ast.R`
- **机制**:
  取代了脆弱的纯文本正则匹配，我们在将代码提交至 `eval()` 执行前，利用 R 自带的 `parse()` 解析为抽象语法树 (AST)，并使用自定义的 `walk_ast()` 函数递归遍历每一个节点。
  系统会精准捕获所有危险函数调用（如 `system`, `eval`, `parse` 等），无论是显式调用 (`system("ls")`) 还是通过命名空间前缀 (`base::system("ls")`)，彻底杜绝字符串拼接或混淆方式的绕过。

### 2. 变量注册表与状态锁 (Variable Registry & State Lock)
- **位置**: `R/variable_registry.R`
- **机制**:
  在 R 的“写时复制 (Copy-on-Modify)”机制下，Agent 如果在沙盒内意外修改一个大型全局对象（如 `my_expensive_model`），可能导致高昂的计算或内存副本开销。
  开发者可通过 `sdk_protect_var()` 标记重要变量（如 `[PROTECTED: Cost High]`）。在 AST 分析阶段，一旦检测到 Agent 试图通过赋值语句（`<-`, `=`, `assign`）去覆盖受保护变量，执行将被立即拦截，并提示 LLM 使用其他变量名（例如 `my_expensive_model_subset`）。同时，`list_session_variables` 工具返回的变量清单也会明确标识出变量的保护等级。

### 3. Human-in-the-Loop (HITL) 动态授权
- **位置**: `R/auth_hook.R`
- **机制**:
  将潜在的风险操作分级：
  - `GREEN` (绿色): 基础的数据清洗和安全沙盒内操作，直接放行。
  - `YELLOW` (黄色): 存在一定风险（如修改全局对象环境、删除文件）。拦截并挂起执行流程，向用户发出询问（`Y/N/Modify`）。
  - `RED` (红色): 严格禁止的操作（如系统破坏级调用）。直接报错。
  当授权被用户挂起且最终被拒绝时，系统并非简单抛错终止进程，而是将 **用户的拒绝反馈** 重新注入对话上下文，迫使 Agent 根据真实反馈更新自己的策略，形成闭环。

### 4. “读/写”沙盒工具解耦 (Separation of Tools)
- **位置**: `R/agent_library.R` (主要是 `CoderAgent` 控制)
- **机制**:
  通过权限显式分离引导大模型行为模式，我们将单一的执行工具分解为：
  - `execute_readonly_analysis`: 用于探索性数据分析（EDA）。它在一个临时的派生沙盒环境中运行，强制确保顶层工作区不被污染。
  - `execute_state_mutation`: 专门用于最终意图的状态变更。它在受保护的共享 Session 环境中执行，并通过前置的 AST 审查及 HITL 机制进行风险把控。

## 开发一致性保证
后续对 `aisdk` 的任何权限提升模块、新的 R 代码沙盒入口或新特性的工具，都必须：
1. **调用 `check_ast_safety()`** 进行静态安全校验。
2. 对于具备不同副作用的操作，需在 `auth_hook.R` 中新增对应的拦截触发并经过用户侧审核 (HITL)。
3. 在增加“只读”工具时，坚持分配 Ephemeral 衍生环境。

欲了解如何给 Agent 配置对应的权限或如何指导大语言模型遵循上述设计，请参考 [agent_guide.md](./agent_guide.md)。
