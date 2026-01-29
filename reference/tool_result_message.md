# Create Tool Result Message

Create a message representing the result of a tool call. Used to send
tool execution results back to the LLM.

## Usage

``` r
tool_result_message(tool_call_id, result, is_error = FALSE)
```

## Arguments

- tool_call_id:

  The ID of the tool call this result responds to.

- result:

  The result content (will be converted to string if needed).

- is_error:

  If TRUE, indicates this result is an error message.

## Value

A list representing a tool result message.
