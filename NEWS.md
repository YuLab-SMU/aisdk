# aisdk 1.4.10

* Skip `r_eval` process-tree-reaping test on CRAN (it polls `pgrep`
  for `sh` + `sleep` grandchildren that the Linux kernel may reparent
  to PID 1 before `processx::kill_tree()` can finish; the TIMEOUT
  user-visible behaviour is exercised by a separate test).

# aisdk 1.4.9 (not released on CRAN)
* Layered architecture (Specification, Utilities, Providers, Core)
  with R6 classes for `Agent`, `Tool`, `Skill`, `Computer`, and
  `Telemetry`.
* Unified interface to multiple AI model providers (e.g. 'OpenAI',
  'Anthropic'), with request interception, robust error handling,
  and exponential retry delays.
* Local small language model inference, distributed 'MCP' ecosystem,
  multi-agent orchestration, progressive knowledge loading through
  skills, and a global skill store for sharing AI capabilities.
* Optional companion packages (aisdk.channels for Feishu/messaging
  integration, aisdk.skills for skill-forge tooling) are detected at
  runtime and degrade gracefully when not installed.
