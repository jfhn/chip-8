package main

import rl "vendor:raylib"

SCREEN_WIDTH :: 64
SCREEN_HEIGHT :: 32
PIXEL_SCALE :: 16

MEMORY_SIZE :: 4 * 1024

screen_buffer: [SCREEN_WIDTH * SCREEN_HEIGHT]rl.Color
memory: [MEMORY_SIZE]i32

main :: proc() {
	// Init screen buffer
	for y := 0; y < SCREEN_HEIGHT; y += 1 {
		for x := 0; x < SCREEN_WIDTH; x += 1 {
			i := y * SCREEN_WIDTH + x
			screen_buffer[i] = rl.RED
		}
	}

	for i := 0; i < MEMORY_SIZE; i += 1 {
		memory[i] = 0;
	}

	window_width :: SCREEN_WIDTH * PIXEL_SCALE
	window_height :: SCREEN_HEIGHT * PIXEL_SCALE

	rl.InitWindow(window_width, window_height, "CHIP-8")
	defer rl.CloseWindow()

	for !rl.WindowShouldClose() {
		// Rendering
		rl.BeginDrawing()
		rl.ClearBackground({ 0x18, 0x18, 0x18, 0xff })

		for y := 0; y < SCREEN_HEIGHT; y += 1 {
			for x := 0; x < SCREEN_WIDTH; x += 1 {
				tx := cast(i32)x * PIXEL_SCALE
				ty := cast(i32)y * PIXEL_SCALE
				i := y * SCREEN_WIDTH + x
				rl.DrawRectangle(tx, ty, PIXEL_SCALE, PIXEL_SCALE, screen_buffer[i])
			}
		}
		rl.EndDrawing()
	}
}
