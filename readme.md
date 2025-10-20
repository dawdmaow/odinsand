<video src="video.mov" width="320" height="240" controls></video>

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
