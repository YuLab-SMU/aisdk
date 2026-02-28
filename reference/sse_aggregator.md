# SSE Stream Aggregator

Universal chunk aggregator for Server-Sent Events (SSE) streaming.
Manages all chunk-level state: text accumulation, reasoning/thinking
transitions, tool call assembly, usage tracking, and result
finalization.

This is a pure aggregator — it does not know about HTTP, SSE parsing, or
provider-specific event types. Provider event mappers call its methods.
