# Agent 开发与 Prompt 编写指南 (Autonomy & Safety)

在使用 `aisdk` 框架开发自定义智能体（如针对科研分析或数据处理的专属 Agent），或者编写 Prompt 模板时，理解并融入系统的安全隔离设计至关重要。这不仅能避免非预期的系统崩溃，更能有效利用 Human-in-the-Loop 授权模型与用户协同。

## 核心原则

### 1. 区分“探索环境”与“状态变更”
`aisdk` 系统中的最佳实践是**强制工具粒度的安全职责分离**。在编写 Prompt 时，必须教导 Agent 正确区分探索阶段和写入阶段：

**推荐的 System Prompt 指导语：**
> "在进行初步的数据探索、尝试模型跑通或统计时，你必须使用 `execute_readonly_analysis` 工具，这能确保系统状态不被意外污染。只有在确信逻辑正确且必须保存/推进持久化状态时，才能使用 `execute_state_mutation` 工具并指明用途。"

- `execute_readonly_analysis` 会自动生成一个派生环境运行。任何局部的赋值都将在执行结束后销毁。
- `execute_state_mutation` 会向全局/共享上下文写入数据，但可能触发额外的授权 (HITL)。

### 2. 尊重受保护变量 (Variable Asset Protection)
Agent 必须懂得“变量成本”。当用户锁定了一个变量（例如调用了 `sdk_protect_var("large_df", locked=TRUE)`），如果在 Agent 尝试修改它时遭遇报错：

> `AST Safety Violation: Variable 'large_df' is protected (Cost: High). Modifying it is not allowed. Please assign your result to a new variable.`

**期望的应对策略：**
不应当停止或重复相同的修改尝试。必须教导大语言模型（特别是 CoderAgent）捕捉到该错误后，自动在接下来的代码中将其结果赋给一个新的派生变量。

**推荐的 System Prompt 指导语：**
> "使用 `list_session_variables` 观察当前可用变量时，留意带有 `[PROTECTED: Cost xxx]` 后缀的变量。它们是昂贵或受保护的共享资源。不要使用 `<-` 或 `assign()` 尝试覆盖它们。如果需要过滤或清洗这些敏感数据，请在运算后将其存储至全新命名的变量。"

### 3. 处理 HITL 的“拒绝”反馈
对于涉及到较高系统操作风险的任务（诸如处理本地系统文件或覆写对象），操作权限往往属于 `YELLOW` 风险级别。此时，`aisdk` 将向用户请求许可。

如果用户拒绝，工具并不只返回 "Error"；它会返回详细的 `User Rejected Action with Feedback: [用户的特定反馈]`。

**期望的应对策略：**
Agent 不应该向用户发送“抱歉报错了，我不知道怎么办”，而必须：
- 分析用户的拒绝原因（Feedback）。
- 根据用户意愿，调整代码方案重试。
- 如果用户禁止某种特定路径，自动寻找 Alternative（替代方案）。

### 4. 最佳测试与落地流
在开发一个新的 Agent 时：
1. 将其配置处于 `sandbox_mode = "strict"` 模式。
2. 尝试让它编写非法越权代码，验证其能否从 `AST checker` 中存活并反思。
3. 提供模拟的交互阻扰（如故意在 console 回复 "N" 或给出修改意见），来调优 Agent 面对安全防御组件时的“再规划 (Re-planning)”韧性。
