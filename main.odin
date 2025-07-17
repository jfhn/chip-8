package main

import "core:fmt"
import rl "vendor:raylib"

SCREEN_WIDTH :: 64
SCREEN_HEIGHT :: 32
PIXEL_SCALE :: 4

main :: proc() {
	screen_buffer := make([]uint, SCREEN_WIDTH * SCREEN_HEIGHT)
	fmt.printfln("Resolution: %dx%d", SCREEN_WIDTH, SCREEN_HEIGHT)

	window_width :: SCREEN_WIDTH * PIXEL_SCALE
	window_height :: SCREEN_HEIGHT * PIXEL_SCALE

	rl.InitWindow(window_width, window_height, "CHIP-8")

	for !rl.WindowShouldClose() {
		rl.ClearBackground({ 0x18, 0x18, 0x18, 0xff })
		rl.DrawText("CHIP-8", 50, 50, 50, rl.RAYWHITE)
	}

	defer rl.CloseWindow()
}
