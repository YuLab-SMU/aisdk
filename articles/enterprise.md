# Enterprise Operations

**aisdk** includes features for production deployment, including
telemetry, hooks, and cost tracking.

## Telemetry

Telemetry hooks allow you to trace execution flows.

``` r
library(aisdk)

# Create standard telemetry
telemetry <- create_telemetry(trace_id = "request-123")
hooks <- telemetry$as_hooks()

# Pass hooks to generation
generate_text(
  model = "openai:gpt-4o", 
  prompt = "Hello", 
  hooks = hooks
)
```

## Permission Hooks

You can intercept sensitive tool calls (like file writing) and require
approval.

Common mistake: assigning `hooks$on_tool_approval <- ...` fails with
“cannot add bindings to a locked environment” because `HookHandler` is
an R6 object. Always assign inside the `hooks` list.

``` r
# Permission hook: only allow "read_file" automatically
# "write_file" will trigger a request for approval (or denial in non-interactive)
perm_hook <- create_permission_hook(
  mode = "escalate", 
  allowlist = c("read_file") 
)

# Combine with other hooks
hooks$hooks$on_tool_approval <- perm_hook$hooks$on_tool_approval
```

## Tool Caching

``` r
Wrap tools with `cache_tool()` to memoize their results. The `...` in the example is a placeholder and cannot be run directly in the console.
slow_tool <- tool(..., execute = function(args) { Sys.sleep(5); ... })

# For example, we have a time-consuming tool `slow_tool` that sleeps for 5 seconds and then returns the input parameter `x`.
slow_tool <- tool(
  name = "slow_tool",
  description = "Sleep 5 seconds and return x",
  parameters = z_object(x = z_number("Input number")),
  execute = function(args) { Sys.sleep(5); args$x }
)

# Cached version
fast_tool <- cache_tool(slow_tool)

# First call: 5 seconds
fast_tool$run(list(x = 1))

# Second call: Instant
fast_tool$run(list(x = 1))
```
