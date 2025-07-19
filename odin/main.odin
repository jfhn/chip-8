package main

import rl "vendor:raylib"

SCREEN_WIDTH :: 64
SCREEN_HEIGHT :: 32
PIXEL_SCALE :: 16

MEMORY_SIZE :: 4 * 1024

screen_buffer: [SCREEN_WIDTH * SCREEN_HEIGHT]rl.Color
memory: [MEMORY_SIZE]u8

Register :: enum {
	V0, V1, V2, V3,
	V4, V5, V6, V7,
	V8, V9, VA, VB,
	VC, VD, VE, VF,
}

Encoded_Instruction :: [2]u8

State :: struct {
	PC: u8,
}

fetch :: proc(state: ^State) -> Instruction {

}

decode :: proc()

// Font
// 0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
// 0x20, 0x60, 0x20, 0x20, 0x70, // 1
// 0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
// 0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
// 0x90, 0x90, 0xF0, 0x10, 0x10, // 4
// 0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
// 0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
// 0xF0, 0x10, 0x20, 0x40, 0x40, // 7
// 0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
// 0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
// 0xF0, 0x90, 0xF0, 0x90, 0x90, // A
// 0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
// 0xF0, 0x80, 0x80, 0x80, 0xF0, // C
// 0xE0, 0x90, 0x90, 0x90, 0xE0, // D
// 0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
// 0xF0, 0x80, 0xF0, 0x80, 0x80  // F

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
