// This implementation an experiment where code quality is measured by the
// amount of codepaths where less codepaths is better.
// See https://www.rfleury.com/p/the-codepath-combinatoric-explosion as a
// reference to the definition of codepaths.

#include <stdio.h>
#include <raylib.h>
#include <stdint.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef int8_t s8;
typedef int16_t s16;

#define REG_COUNT 16
#define MEM_BYTES 4096
#define WIDTH 64
#define HEIGHT 32

int main() {
  InitWindow(800, 600, "CHIP-8");
  SetTargetFPS(60);

  // Layout: PC low, PC high, I, SP, V0, ..., VF, Stack
  u8 memory[MEM_BYTES] = {0};
  u16 *pc = (u16* )memory;
  *pc = 0x0200;
  u8 *index = memory + 2;
  u8 *sp = memory + 3;
  u8 *v = memory + 4;
  u8 screen[WIDTH*HEIGHT] = {0};

  while (!WindowShouldClose()) {
    // Fetch
    u16 inst = *((u16 *)(memory + *pc));

    // Decode and execute
    if ((inst & 0x00F0) == 0x00E0) { // CLS
      for (int i = 0; i < WIDTH*HEIGHT; i++) {
        screen[i] = 0;
      }
    } else if ((inst & 0x00FF) == 0x00EE) { // RET
      *pc = memory[(*sp)--];
    } else if ((inst & 0xF000) == 0x1000) { // JP nnn
      *pc = inst & 0x0FFF;
    } else if ((inst & 0xF000) == 0x2000) { // CALL nnn
      ++(*sp);
      u16 *m = (u16 *)(memory + *sp);
      *m = *pc;
      *pc = inst & 0x0FFF;
    } else if ((inst & 0xF000) == 0x3000) { // SE Vx, kk
      u8 vx = v[inst & 0x0F00];
      u8 kk = inst & 0x00FF;
      if (vx == kk) *pc += 2;
      else *pc += 1;
    } else if ((inst & 0xF000) == 0x4000) { // SNE Vx, kk
      u8 vx = v[inst & 0x0F00];
      u8 kk = inst & 0x00FF;
      if (vx != kk) *pc += 2;
      else *pc += 1;
    }

    BeginDrawing();
    ClearBackground(BLACK);
    EndDrawing();
  }

  CloseWindow();
  return 0;
}
