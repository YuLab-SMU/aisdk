# Channel Runtime And Feishu Foundation Plan

Date: 2026-03-24

## Goal

Add a low-level messaging channel layer to `aisdk` so external IM platforms can drive `ChatSession` objects without coupling transport concerns to providers, tools, or UI surfaces. Feishu is the first adapter, but the core design must remain transport-agnostic.

## Core Decisions

1. Channel integration sits above `ChatSession` and `Agent`, not inside provider code.
2. External messages are normalized into a transport-neutral inbound envelope before session routing.
3. Group chats default to one shared session per chat, but each inbound message preserves sender identity in both metadata and the rendered prompt.
4. Session state is persisted to local files so future runs and parallel child sessions can coordinate through durable state.
5. Runtime delivers final replies only to external messaging surfaces. No streaming chunks are emitted externally.

## First-Phase Scope

- Add reusable channel runtime, registry, and file-backed session store.
- Add a generic channel adapter interface.
- Add a Feishu adapter skeleton that can:
  - handle `url_verification`
  - normalize `im.message.receive_v1` text events
  - route them into `ChatSession`
  - send final text replies back through Feishu APIs
- Preserve room, sender, and thread identity in session metadata.
- Track parent/child session relationships in the local session index.

## Data Model

### Normalized inbound message

- `channel_id`
- `account_id`
- `event_id`
- `chat_id`
- `chat_type`
- `thread_id`
- `sender_id`
- `sender_name`
- `text`
- `mentions`
- `attachments`
- `raw`
- `metadata`

### Session record

- `session_key`
- `channel_id`
- `account_id`
- `chat_id`
- `chat_type`
- `thread_id`
- `participants`
- `created_at`
- `updated_at`
- `parent_session_key`
- `child_session_keys`
- `metadata`
- `session_file`

## Session Key Policy

- Direct chat: `feishu:<account_id>:direct:<chat_id-or-sender_id>`
- Group chat: `feishu:<account_id>:group:<chat_id>`
- Threaded group chat: `feishu:<account_id>:group:<chat_id>:thread:<thread_id>`

The group session is shared by default. Sender identity remains available in:

- normalized message fields
- session record participants
- rendered prompt prefix for inbound group turns

## Runtime Flow

1. Channel adapter receives raw request.
2. Adapter validates and parses request into a normalized event result.
3. Runtime resolves the session key using channel policy.
4. Runtime loads or creates a `ChatSession`.
5. Runtime updates local session index and participant metadata.
6. Runtime formats the inbound user turn.
7. Runtime runs `session$send()` with the configured agent or model.
8. Runtime stores the updated session.
9. Runtime hands the final outbound message to the adapter.

## Parallel Child Sessions

The file session store will keep `parent_session_key` and `child_session_keys` in the session index. This does not yet execute child sessions automatically, but it provides the durable graph required for future parallel coordination.

## Files To Add

- `R/channel_types.R`
- `R/channel_session_store.R`
- `R/channel_runtime.R`
- `R/channel_feishu.R`
- `tests/testthat/test-channel-runtime.R`

## Files To Update

- `R/session.R`

## First-Phase Constraints

- No generic HTTP server abstraction yet.
- No Feishu encrypted event support in phase 1.
- Text messages first; rich cards and files later.
- Final outbound replies only.
