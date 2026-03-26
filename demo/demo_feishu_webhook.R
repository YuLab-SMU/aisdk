# Demo: run a local Feishu webhook endpoint backed by aisdk's channel runtime.
#
# Usage:
#   FEISHU_APP_ID=... \
#   FEISHU_APP_SECRET=... \
#   FEISHU_VERIFICATION_TOKEN=... \
#   FEISHU_ENCRYPT_KEY=... \
#   Rscript demo/demo_feishu_webhook.R
#
# Optional:
#   FEISHU_HOST=127.0.0.1
#   FEISHU_PORT=8788
#   FEISHU_PATH=/feishu/webhook
#   FEISHU_MODEL=openai:gpt-4o-mini
#   OPENAI_API_KEY=...

if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) {
  pkgload::load_all(".")
} else if (requireNamespace("aisdk", quietly = TRUE)) {
  library(aisdk)
} else {
  stop("Need either installed package 'aisdk' or package 'pkgload' to run this demo.")
}

app_id <- Sys.getenv("FEISHU_APP_ID", "")
app_secret <- Sys.getenv("FEISHU_APP_SECRET", "")
verification_token <- Sys.getenv("FEISHU_VERIFICATION_TOKEN", "")
encrypt_key <- Sys.getenv("FEISHU_ENCRYPT_KEY", "")
host <- Sys.getenv("FEISHU_HOST", "127.0.0.1")
port <- as.integer(Sys.getenv("FEISHU_PORT", "8788"))
path <- Sys.getenv("FEISHU_PATH", "/feishu/webhook")
model_id <- Sys.getenv("FEISHU_MODEL", "")
workdir <- normalizePath(Sys.getenv("FEISHU_WORKDIR", getwd()), winslash = "/", mustWork = FALSE)
sandbox_mode <- Sys.getenv("FEISHU_SANDBOX_MODE", "strict")
session_root <- Sys.getenv("FEISHU_SESSION_ROOT", file.path(workdir, ".aisdk", "feishu"))

if (!nzchar(app_id) || !nzchar(app_secret)) {
  stop("FEISHU_APP_ID and FEISHU_APP_SECRET must be set.")
}

dir.create(session_root, recursive = TRUE, showWarnings = FALSE)

if (nzchar(model_id)) {
  agent_tools <- create_computer_tools(
    working_dir = workdir,
    sandbox_mode = sandbox_mode
  )
  feishu_agent <- create_agent(
    name = "FeishuAgent",
    description = "Feishu chat agent with local computer tools for code, files, shell, and R execution",
    system_prompt = paste0(
      "You are an execution-capable AI agent operating through Feishu chat.\n",
      "Use tools when they materially improve the answer.\n",
      "Do not ask the user to use terminal-only controls.\n",
      "If you generate a local file and include its absolute path in the final reply, the runtime will try to send it back as a Feishu attachment automatically.\n",
      "When a file has been generated successfully, state that it has been generated and include the absolute path once.\n",
      "Do not claim that a file, image, or attachment has already been sent unless you have explicit confirmation from the system. Generating a local file is not the same as sending it.\n",
      "When writing code, prefer concise runnable snippets.\n",
      "Working directory: ", workdir, "\n",
      "Sandbox mode: ", sandbox_mode, "\n",
      "If a task can be solved without tools, answer directly.\n",
      "If you use tools, summarize the result clearly in chat."
    ),
    tools = agent_tools,
    model = model_id
  )

  runtime <- create_feishu_channel_runtime(
    session_store = create_file_channel_session_store(session_root),
    app_id = app_id,
    app_secret = app_secret,
    verification_token = if (nzchar(verification_token)) verification_token else NULL,
    encrypt_key = if (nzchar(encrypt_key)) encrypt_key else NULL,
    model = model_id,
    agent = feishu_agent
  )
} else {
  DemoMockModel <- R6::R6Class(
    "DemoFeishuMockModel",
    inherit = aisdk:::LanguageModelV1,
    public = list(
      provider = "mock",
      model_id = "feishu-demo-mock",
      do_generate = function(params) {
        last <- params$messages[[length(params$messages)]]
        list(
          text = paste("Feishu demo received:", last$content),
          tool_calls = NULL,
          finish_reason = "stop"
        )
      },
      format_tool_result = function(tool_call_id, tool_name, result) {
        list(role = "tool", tool_call_id = tool_call_id, name = tool_name, content = result)
      }
    )
  )

  registry <- ProviderRegistry$new()
  registry$register("mock", function(model_id) DemoMockModel$new())

  runtime <- create_feishu_channel_runtime(
    session_store = create_file_channel_session_store(session_root),
    app_id = app_id,
    app_secret = app_secret,
    verification_token = if (nzchar(verification_token)) verification_token else NULL,
    encrypt_key = if (nzchar(encrypt_key)) encrypt_key else NULL,
    model = "mock:feishu-demo",
    registry = registry
  )
}

cat("Feishu demo session store:", session_root, "\n")
cat("Callback path:", path, "\n")
cat("Starting webhook server...\n")
run_feishu_webhook_server(
  runtime = runtime,
  host = host,
  port = port,
  path = path
)
