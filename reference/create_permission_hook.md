# Create Permission Hook

Create a hook that enforces a permission mode for tool execution.

## Usage

``` r
create_permission_hook(
  mode = c("implicit", "explicit", "escalate"),
  allowlist = c("search_web", "read_resource", "read_file")
)
```

## Arguments

- mode:

  Permission mode:

  - "implicit" (default): Auto-approve all tools.

  - "explicit": Ask for confirmation in the console for every tool.

  - "escalate": Ask for confirmation only for tools not in the
    allowlist.

- allowlist:

  List of tool names that are auto-approved in "escalate" mode. Default
  includes read-only tools like "search_web", "read_file".

## Value

A HookHandler object.
