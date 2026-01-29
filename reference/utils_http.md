# Utilities: HTTP and Retry Logic

Provides standardized HTTP request handling with exponential backoff
retry.

Implements multi-layer defense strategy for handling API responses:

- Empty response body handling (returns instead of parse error)

- JSON parsing with repair fallback

- SSE stream error recovery

- Graceful degradation on malformed data
