# Framework Selection

Use this file when you need to choose an implementation path.

## Default Recommendation

Recommend `single-file HTML` first when:
- the user is exploring
- speed matters
- the deck is modest
- they want AI to scaffold and then iterate

Recommend `reveal.js` first when:
- the user already knows they want a deck engine
- fragments are important
- speaker-style navigation is core
- they do not benefit from rebuilding slide infrastructure

Recommend `scroll-snap` when:
- the experience is primarily browsed, not presented
- the page should feel like a narrative scroll document

## Comparison

### 1. Single-file HTML

Pros:
- fastest to scaffold
- easy to inspect
- good for AI iteration
- minimal moving parts

Cons:
- features accumulate quickly
- maintenance can degrade if upgrades are added without structure

Use when:
- prototype first
- one-off custom deck
- controlled feature scope

### 2. Enhanced Vanilla Deck

Pros:
- full control
- no framework dependency
- can stay lean if curated

Cons:
- the author now owns navigation, fragments, URL sync, and edge cases

Use when:
- the user needs custom behavior
- the team can maintain bespoke front-end logic

### 3. Scroll Snap

Pros:
- elegant for story pages
- native browser behavior
- fits self-guided browsing

Cons:
- weaker for speaker pacing
- awkward for fragment-first narration

Use when:
- “scroll through the story” is the product

### 4. WAAPI

Pros:
- better playback control
- good for chained or synchronized sequences

Cons:
- complexity rises quickly
- unnecessary for basic slide transitions

Use when:
- time sequencing is part of the message

### 5. View Transition API

Pros:
- smooth scene continuity
- expressive for state-to-state transitions

Cons:
- not a replacement for deck architecture
- browser and implementation reality may limit payoff

Use when:
- the state model is already clean
- the user explicitly values polished view transitions

### 6. reveal.js

Pros:
- deck primitives already solved
- fragments, themes, auto-animate, markdown support
- good long-term value

Cons:
- less bespoke than fully custom systems
- users may over-customize before content is stable

Use when:
- the user wants reliable presentation infrastructure

## Rule of Thumb

If the user is still deciding what the deck should do, do not start with advanced animation APIs.

If the user already knows they need a real slide framework, do not waste time rebuilding reveal.js badly.
