[![Watch the video](https://img.youtube.com/vi/84vCYWuGxcU/0.jpg)](https://www.youtube.com/watch?v=84vCYWuGxcU)

Noita-like 2D "particle" "simulation" game.

## Info
- 14 (or so) particle kinds
- thread pool used for processing subregions in parallel (no performance gain though)
- mouse, keyboard inputs
- microui UI
- scanline shader

## Dependencies
- raylib
- odin

## Particle Interactions
- water dissolves salt
- fire produces smoke and ash
- oil burns and interacts with water
- acid corrodes materials
- lava ignites flammable materials
- seeds grow into vines
- corruption spreads to nearby particles

## Controls
- **Left Click**: place particles
- **Right Click**: remove particles
- **Mouse Wheel**: adjust sprinkle radius
- **1-9, 0**: select particle type
