// This implementation an experiment where code quality is measured by the
// amount of codepaths where less codepaths is better.
// See https://www.rfleury.com/p/the-codepath-combinatoric-explosion as a
// reference to the definition of codepaths.

#include <stdio.h>
#include <raylib.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef int8_t s8;
typedef int16_t s16;

#define REG_COUNT 16
#define DIGIT_COUNT 16
#define MEM_USER_BYTES (0x0fff - 0x200)
#define WIDTH 64
#define HEIGHT 32
#define PIXEL_SCALE 16

#define BYTES(x) (sizeof(x)/sizeof(char))

struct digit { u8 rows[5]; };

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "Usage: %s [options] <path-to-file>\n  Options: --debug\n", argv[0]);
    return 1;
  }

  char *infile = NULL;
  bool debug = false;

  for (int i = 0; i < argc; i++) {
    if (strncmp(argv[i], "--debug", 7) == 0) debug = true;
    else infile = argv[i];
  }

  u8 memory[MEM_USER_BYTES] = {0};
  memset(memory, 0, MEM_USER_BYTES);
  {
    FILE *stream = fopen(argv[1], "rb");
    if (stream == NULL) {
      perror("Could not open file");
      return 1;
    }

    fseek(stream, 0, SEEK_END);
    size_t size = (size_t)ftell(stream);
    fseek(stream, 0, SEEK_SET);

    size_t read = fread(memory, 1, size, stream);
    fclose(stream);
    if (read != size) {
      fprintf(stderr, "Failed to read input file: %s\n", infile);
      return 1;
    }
  }

  // TODO: Dynamic scale?
  InitWindow(WIDTH*PIXEL_SCALE, HEIGHT*PIXEL_SCALE, "CHIP-8");
  SetTargetFPS(60);

  srand((unsigned int)time(NULL));

  u8 screen[WIDTH*HEIGHT] = {0};

  u16 pc = 0x200;
  u16 index = 0;
  u8 sp = 0;
  u8 sound = 0;
  u8 delay = 0;
  u8 v[16] = {0};
  u16 digits_begin = 24;
  struct digit digits[16] = {
    {{0xF0, 0x90, 0x90, 0x90, 0xF0}}, // 0
    {{0x20, 0x60, 0x20, 0x20, 0x70}}, // 1
    {{0xF0, 0x10, 0xF0, 0x80, 0xF0}}, // 2
    {{0xF0, 0x10, 0xF0, 0x10, 0xF0}}, // 3
    {{0x90, 0x90, 0xF0, 0x10, 0x10}}, // 4
    {{0xF0, 0x80, 0xF0, 0x10, 0xF0}}, // 5
    {{0xF0, 0x80, 0xF0, 0x90, 0xF0}}, // 6
    {{0xF0, 0x10, 0x20, 0x40, 0x40}}, // 7
    {{0xF0, 0x90, 0xF0, 0x90, 0xF0}}, // 8
    {{0xF0, 0x90, 0xF0, 0x10, 0xF0}}, // 9
    {{0xF0, 0x90, 0xF0, 0x90, 0x90}}, // A
    {{0xE0, 0x90, 0xE0, 0x90, 0xE0}}, // B
    {{0xF0, 0x80, 0x80, 0x80, 0xF0}}, // C
    {{0xE0, 0x90, 0x90, 0x90, 0xE0}}, // D
    {{0xF0, 0x80, 0xF0, 0x80, 0xF0}}, // E
    {{0xF0, 0x80, 0xF0, 0x80, 0x80}}  // F
  };
  (void)digits;
  u16 stack[408]; // 0x200 - previous memory.

  int keymap[] = {
    KEY_ONE, KEY_TWO, KEY_THREE, KEY_FOUR,
    KEY_Q, KEY_W, KEY_E, KEY_R,
    KEY_A, KEY_S, KEY_D, KEY_F,
    KEY_Z, KEY_X, KEY_C, KEY_F,
  };

  bool next_debug_step = true;

  while (!WindowShouldClose()) {
    // Fetch, memory starts at 0x200 -> always reduce access by 0x200.
    u16 inst = ((u16)memory[pc - 0x200] << 8) + (u16)(memory[pc + 1 - 0x200]);

    if (debug) {
      if (next_debug_step) {
        printf("Next instruction 0x%04x, PC = 0x%04x\n", inst, pc);
        for (int i = 0; i < REG_COUNT - 1; i++) {
          printf("V%x = %02x, ", i, v[i]);
        }
        printf("V%x = %u\n", REG_COUNT - 1, v[REG_COUNT - 1]);
        printf("SP 0x%02x, I = 0x%02x, Sound = 0x%02x, Delay = 0x%02x\n",
               sp, index, sound, delay);
        printf("Stack: [");
        for (u8 i = 0; i < sp - 1; sp++) {
          printf("0x%04x, ", stack[i]);
        }
        printf("0x%04x]\n\n", stack[sp - 1]);

        printf("s - step\nq - quit\n");
        next_debug_step = false;
      }

      int key = GetKeyPressed();
      if (key == KEY_Q) {
        CloseWindow();
        break;
      }
      if (key != KEY_S) {
        BeginDrawing(); // These calls are needed for Raylib to update.
        EndDrawing();
        continue;
      }
      next_debug_step = true;
    }

    pc += 2;

#define VX  (v[(inst & 0x0F00) >> 8])
#define VY  (v[(inst & 0x00F0) >> 4])
#define NNN (inst & 0x0FFF)
#define N   ((u8)(inst & 0x000F))
#define KK  ((u8)(inst & 0x00FF))
#define VF  v[15]

    // Decode and execute
    if ((inst & 0x00F0) == 0x00E0) { // CLS
      for (int i = 0; i < WIDTH*HEIGHT; i++) screen[i] = 0;
    } else if ((inst & 0x00FF) == 0x00EE) { // RET
      pc = stack[(sp)--];
    } else if ((inst & 0xF000) == 0x1000) { // JP nnn
      pc = NNN;
    } else if ((inst & 0xF000) == 0x2000) { // CALL nnn
      ++sp;
      u16 *m = (u16 *)(stack + sp);
      *m = pc;
      pc = inst & 0x0FFF;
    } else if ((inst & 0xF000) == 0x3000) { // SE Vx, kk
      if (VX == KK) pc += 2;
    } else if ((inst & 0xF000) == 0x4000) { // SNE Vx, kk
      if (VX != KK) pc += 2;
    } else if ((inst & 0xF00F) == 0x5000) { // SE Vx, Vy
      if (VX == VY) pc += 2;
    } else if ((inst & 0xF000) == 0x6000) { // LD Vx, kk
      VX = KK;
    } else if ((inst & 0xF000) == 0x7000) { // ADD Vx, kk
      VX = VX + KK;
    } else if ((inst & 0xF00F) == 0x8000) { // LD Vx, Vy
      VX = VY;
    } else if ((inst & 0xF00F) == 0x8001) { // OR Vx, Vy
      VX = VX | VY;
    } else if ((inst & 0xF00F) == 0x8002) { // AND Vx, Vy
      VX = VX & VY;
    } else if ((inst & 0xF00F) == 0x8003) { // XOR Vx, Vy
      VX = VX ^ VY;
    } else if ((inst & 0xF00F) == 0x8004) { // ADD Vx, Vy
      u16 result = (u16)VX + (u16)VY;
      VF = result > 0x00FF;
      VX = (u8)result;
    } else if ((inst & 0xF00F) == 0x8005) { // SUB Vx, Vy
      VF = VX > VY;
      VX = VX - VY;
    } else if ((inst & 0xF00F) == 0x8006) { // SHR Vx
      VF = (VX & 1) == 1;
      VX = VX >> 1;
    } else if ((inst & 0xF00F) == 0x8007) { // SUBN Vx, Vy
      VF = VY > VX;
      VX = VY - VX;
    } else if ((inst & 0xF00F) == 0x800E) { // SHR Vx
      VF = (VX & 0x80) == 0x80;
      VX = VX << 1;
    } else if ((inst & 0xF00F) == 0x9000) { // SNE Vx, Vy
      if (VX != VY) pc += 2;
    } else if ((inst & 0xF000) == 0xA000) { // LD I, nnn
      index = NNN;
    } else if ((inst & 0xF000) == 0xB000) { // JP V0, nnn
      pc = v[0] + NNN;
    } else if ((inst & 0xF000) == 0xC000) { // RND Vx, kk
      VX = ((u8)rand()) & KK;
    } else if ((inst & 0xF000) == 0xD000) { // DRW Vx, Vy, n
      u8 *bytes = memory + index - 0x200; // Memory begins at 0x200.
      printf("draw for 0x%04x to 0x%04x:", index, index + N);
      for (u8 i = 0; i < N; i++) printf(" %02x", bytes[i]);
      printf("\n");
      for (u8 y = 0; y < N; y++) {
        for (u8 x = 0; x < 8; x++) {
          u8 tx = VX + x;
          if (tx > WIDTH) tx -= WIDTH;
          u8 ty = VY + y;
          if (ty > HEIGHT) ty -= WIDTH*HEIGHT;
          u8 before = screen[ty*WIDTH + tx];
          screen[ty*WIDTH + tx] ^= bytes[y] & (1 << (8 - x));
          if (before != screen[ty*WIDTH + tx]) VF = 1;
        }
      }
      int x = 0; // Debug, remove later.
    } else if ((inst & 0xF0FF) == 0xE09E) { // SKP Vx
      if (IsKeyDown(keymap[VX])) pc += 2;
    } else if ((inst & 0xF0FF) == 0xE0A1) { // SKNP Vx
      if (!IsKeyDown(keymap[VX])) pc += 2;
    } else if ((inst & 0xF0FF) == 0xF007) { // LD Vx, DT
      VX = delay;
    } else if ((inst & 0xF0FF) == 0xF00A) { // LD Vx, K
      int key = GetKeyPressed();
      if (key != KEY_NULL) VX = (u8)key;
      else pc -= 2; // Stay at this instruction
    } else if ((inst & 0xF0FF) == 0xF015) { // LD DT, Vx
      delay = VX;
    } else if ((inst & 0xF0FF) == 0xF018) { // LD ST, Vx
      sound = VX;
    } else if ((inst & 0xF0FF) == 0xF01E) { // ADD I, Vx
      index = index + VX;
    } else if ((inst & 0xF0FF) == 0xF029) { // LD F, Vx
      index = digits_begin + (u16)(VX*sizeof(struct digit));
    } else if ((inst & 0xF0FF) == 0xF033) { // LD B, Vx
      memory[index - 0x200 + 0] = VX/100;
      memory[index - 0x200 + 1] = VX%100/10;
      memory[index - 0x200 + 2] = VX%10;
    } else if ((inst & 0xF0FF) == 0xF055) { // LD [I], Vx
      u8 vx = (u8)((inst & 0x0F00) >> 8);
      for (u8 i = 0; i < vx; i++) memory[index - 0x200 + i] = v[i];
    } else if ((inst & 0xF0FF) == 0xF065) { // LD Vx, [I]
      u8 vx = (u8)((inst & 0x0F00) >> 8);
      for (u8 i = 0; i < vx; i++) v[i] = memory[index - 0x200 + i];
    }

    sound--;
    delay--;

    BeginDrawing();
    ClearBackground(BLACK);
    for (int y = 0; y < HEIGHT; y++) {
      for (int x = 0; x < WIDTH; x++) {
        if (screen[y*WIDTH + x])
          DrawRectangle(x*PIXEL_SCALE, y*PIXEL_SCALE,
                        PIXEL_SCALE, PIXEL_SCALE, RAYWHITE);
      }
    }
    EndDrawing();
    printf(".");
    fflush(stdout);
  }

  CloseWindow();
  return 0;
}
