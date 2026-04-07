# Article Summary: PPT as Code

Source: Russell on X, “PPT as Code：用网页高效做出比PPT还惊艳的演示文稿！！”
Captured: 2026-04-07
URL: https://x.com/Russell3402/status/2040428571064484221

## Core Thesis

Traditional PPT workflows waste time on tool friction instead of expression.

The better alternative for many modern presentations is to treat the deck as a webpage asset:
- reusable
- iterable
- extensible
- shareable by link
- versionable in code

The point is not that code is cooler.
The point is that the presentation stops being a one-off file and becomes a content system.

## The Underlying Model

The article argues that a presentation system is defined by five pieces:
- `container`
- `slides`
- `index`
- `controls`
- `motion`

Animation is not the foundation.
State transitions are.

This means the minimal viable deck is:
- page-sized sections
- one viewport
- one current index
- button and keyboard navigation
- transform/opacity transitions

## Recommended Build Sequence

1. Smallest runnable shell
2. Progress + dots
3. URL sync
4. Fragments
5. Media preload
6. Mobile adaptation
7. Visual system
8. Image workflow

## Framework Positioning

### Hand-written HTML
Best for:
- smallest prototype
- direct AI iteration
- light bespoke decks

### Scroll Snap
Best for:
- narrative scrolling
- self-browsed experiences

Not best for:
- precise speaker-paced progression
- multi-step fragment logic

### WAAPI
Best for:
- stronger timing control
- chained motion
- JS-controlled animation flows

### View Transition API
Best for:
- view-to-view camera-like continuity

Not best when:
- state logic is still messy
- browser support constraints matter

### reveal.js
Best for:
- mature deck features
- fragments
- auto-animate
- markdown-driven decks
- not wanting to rebuild deck mechanics

## Visual System Advice

The article strongly argues against styling slide by slide.

Define first:
- font system
- color system
- spacing system
- component system

It also recommends:
- restrained, modern visual tone
- not defaulting to cyberpunk purple
- using multiple visual directions before deepening one
- changing CSS more than HTML/JS during restyling

## Image Workflow Advice

Use AI first as a visual research assistant:
1. infer the visual goal
2. propose multiple metaphors
3. build a search package
4. only then write an image prompt if needed

The sequence matters because it avoids generic, low-quality “AI aesthetic” results.

## Product Judgment

The article ends with a broader claim:

What matters is not whether the tool is HTML or PPT.
What matters is whether the author is building:
- a one-time presentation file
or
- a lasting expression system

The durable competitive edge is judgment about:
- structure
- pacing
- maintainability
- clarity
- accessibility
