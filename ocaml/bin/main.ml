open Base

exception Invalid
exception Halt

(** *)
module Instruction = struct
  type encoded = int

  (** *)
  type decoded =
    | Invalid (* TODO: Not an actual instruction *)
    | Halt (* TODO: Not an actual instruction *)
    | Clear_Screen (** Clears the screen *)
    | Return (** PC = top stack *)
    | Jump of int (** PC = given address *)
    | Call of int (** ++SP = PC, PC = given address *)
    | Skip_if_val_eq of int * int (** SE 3xkk: if Vx = kk then PC += 2 *)
    | Skip_if_val_neq of int * int (** SNE 4xkk: if Vx <> kk then PC += 2 *)
    | Skip_if_reg_eq of int * int (** SE 5xy0: if Vx = Vy then PC += 2 *)
    | Put_val_in_reg of int * int (** LD 6xkk: Vx = kk *)
    | Add_val_to_reg of int * int (** ADD 7xkk: Vx = Vx + kk *)
    | Put_reg_in_reg of int * int (** LD 8xy0: Vx = Vy *)
    | Or_reg_with_reg of int * int (** OR 8xy1: Vx OR Vy *)
    | And_reg_with_reg of int * int (** AND 8xy2: Vx AND Vy *)
    | Xor_reg_with_reg of int * int (** XOR 8xy3: Vx XOR Vy *)
    | Add_reg_to_reg of int * int (** ADD 8xy4: Vx = Vx + Vy, VF = carry *)
    | Sub_reg_from_reg of int * int (** SUB 8xy5: Vx = Vx - Vy, VF = Vx > Vy *)
    | Shift_right of int (** SHR 8xy6: Vx = Vx >> 1, VF = LSB(Vx) = 1 *)
    | Sub_reg_from_reg_rev of int * int (** SUBN 8xy7: Vx = Vy - Vx, VF = Vy > Vx *)
    | Shift_left of int (** SHL 8xyE: Vx = Vx << 1, VF = MSB(Vx) = 1 *)
    | Skip_if_reg_neq of int * int (** SE 9xy0: if Vx <> Vy then PC += 2 *)
    | Set_index of int (** LD Annn: I = nnn *)
    | Jump_relative of int (** JP Bnnn: PC = nnn + V0 *)
    | Random_byte of int * int (** RND Cxkk: Vx = random AND kk *)

  let show : decoded -> string =
    let open Printf in
    function
    | Invalid -> "INVALID"
    | Halt -> "HALT"
    | Clear_Screen -> "CLS"
    | Return -> "RET"
    | Jump address -> sprintf "JUMP %03x" address
    | Call address -> sprintf "JUMP %03x" address
    | Skip_if_val_eq (vx, v) -> sprintf "SE %x %02x" vx v
    | Skip_if_val_neq (vx, v) -> sprintf "SNE %x %02x" vx v
    | Skip_if_reg_eq (vx, vy) -> sprintf "SE %x %x" vx vy
    | Put_val_in_reg (vx, v) -> sprintf "LD V%x %02x" vx v
    | Add_val_to_reg (vx, v) -> sprintf "ADD V%x %02x" vx v
    | Put_reg_in_reg (vx, vy) -> sprintf "LD V%x V%x" vx vy
    | Or_reg_with_reg (vx, vy) -> sprintf "OR V%x V%x" vx vy
    | And_reg_with_reg (vx, vy) -> sprintf "AND V%x V%x" vx vy
    | Xor_reg_with_reg (vx, vy) -> sprintf "XOR V%x V%x" vx vy
    | Add_reg_to_reg (vx, vy) -> sprintf "ADD V%x V%x" vx vy
    | Sub_reg_from_reg (vx, vy) -> sprintf "SUB V%x V%x" vx vy
    | Shift_right vx -> sprintf "SHR V%x" vx
    | Sub_reg_from_reg_rev (vx, vy) -> sprintf "SUBN V%x V%x" vx vy
    | Shift_left vx -> sprintf "SHL V%x" vx
    | Skip_if_reg_neq (vx, vy) -> sprintf "SNE V%x V%x" vx vy
    | Set_index v -> sprintf "LD I %03x" v
    | Jump_relative address -> sprintf "JP V0 %03x" address
    | Random_byte (vx, v) -> sprintf "RND V%x %02x" vx v
  ;;
end

type config = { pixel_scale : int }

let config = { pixel_scale = 8 }

type facts =
  { width : int
  ; height : int
  ; background_color : Raylib.Color.t
  ; memory : int
  }

(** *)
let facts : facts =
  { width = 64; height = 32; background_color = Raylib.Color.black; memory = 4096 }
;;

type memory = char array

(** *)
type state =
  { pc : int ref
  ; registers : int array
  ; i : int ref
  ; stack : int list ref
      (* TODO: Emulates stack until usage within memory block is figured out *)
  ; memory : memory
  ; screen : Raylib.Color.t array
  }

(** *)
let setup () : state =
  let screen_len = facts.width * facts.height in
  { pc = ref 0x200
  ; registers = Array.create ~len:16 0
  ; i = ref 0
  ; stack = ref []
  ; memory = Array.create ~len:facts.memory (Char.of_int_exn 0)
  ; screen = Array.create ~len:screen_len facts.background_color
  }
;;

let ( << ) = Int.shift_left
let ( >> ) = Int.shift_right
let ( &: ) = Int.bit_and
let ( |: ) = Int.bit_or
let ( ^: ) = Int.bit_xor

(** *)
let fetch state : Instruction.encoded =
  let pc = !(state.pc) in
  let high = Char.to_int state.memory.(pc) << 8 in
  let low = Char.to_int state.memory.(pc + 1) in
  high + low
;;

let decode (code : Instruction.encoded) : Instruction.decoded =
  let address = code &: 0x0FFF in
  let vx = code &: 0x0F00 >> 8 in
  let vy = code &: 0x00F0 >> 4 in
  let v = code &: 0x00FF in
  match code with
  | 0x0000 -> Invalid
  | 0x0001 -> Halt
  | 0x00E0 -> Clear_Screen
  | 0x00EE -> Return
  | code when code &: 0x1000 = 0x1000 -> Jump address
  | code when code &: 0x2000 = 0x2000 -> Call address
  | code when code &: 0x3000 = 0x3000 -> Skip_if_val_eq (vx, v)
  | code when code &: 0x4000 = 0x4000 -> Skip_if_val_neq (vx, v)
  | code when code &: 0x5000 = 0x5000 -> Skip_if_reg_eq (vx, vy)
  | code when code &: 0x6000 = 0x6000 -> Put_val_in_reg (vx, v)
  | code when code &: 0x7000 = 0x7000 -> Add_val_to_reg (vx, v)
  | code when code &: 0x8000 = 0x8000 -> Put_reg_in_reg (vx, vy)
  | code when code &: 0x8001 = 0x8001 -> Or_reg_with_reg (vx, vy)
  | code when code &: 0x8002 = 0x8002 -> And_reg_with_reg (vx, vy)
  | code when code &: 0x8003 = 0x8003 -> Xor_reg_with_reg (vx, vy)
  | code when code &: 0x8004 = 0x8004 -> Add_reg_to_reg (vx, vy)
  | code when code &: 0x8005 = 0x8005 -> Sub_reg_from_reg (vx, vy)
  | code when code &: 0x8006 = 0x8006 -> Shift_right vx
  | code when code &: 0x8007 = 0x8007 -> Sub_reg_from_reg_rev (vx, vy)
  | code when code &: 0x800E = 0x800E -> Shift_left vx
  | code when code &: 0x9000 = 0x9000 -> Skip_if_reg_neq (vx, vy)
  | code when code &: 0xA000 = 0xA000 -> Set_index address
  | code when code &: 0xB000 = 0xB000 -> Jump_relative address
  | code when code &: 0xC000 = 0xC000 -> Random_byte (vx, v)
  | code -> Printf.sprintf "unknown instruction: 0x%04x\n" code |> failwith
;;

(** Simulates one instruction and updates the state *)
let execute state : Instruction.decoded -> unit =
  fun instruction ->
  Stdio.printf "execute %s\n" (Instruction.show instruction);
  match instruction with
  | Invalid -> raise Invalid
  | Halt -> raise Halt
  | Clear_Screen ->
    let len = facts.width * facts.height in
    for i = 0 to len - 1 do
      state.screen.(i) <- facts.background_color
    done;
    state.pc := !(state.pc) + 2
  | Return ->
    (match !(state.stack) with
     | address :: stack ->
       state.pc := address;
       state.stack := stack
     | [] -> failwith "internal error: return from empty stack")
  | Jump address -> state.pc := address
  | Call address ->
    state.stack := !(state.pc) :: !(state.stack);
    state.pc := address
  | Skip_if_val_eq (vx, v) ->
    let increment = if state.registers.(vx) = v then 4 else 2 in
    state.pc := !(state.pc) + increment
  | Skip_if_val_neq (vx, v) ->
    let increment = if state.registers.(vx) <> v then 4 else 2 in
    state.pc := !(state.pc) + increment
  | Skip_if_reg_eq (vx, vy) ->
    let increment = if state.registers.(vx) = state.registers.(vy) then 4 else 2 in
    state.pc := !(state.pc) + increment
  | Put_val_in_reg (vx, v) ->
    state.registers.(vx) <- v;
    state.pc := !(state.pc) + 2
  | Add_val_to_reg (vx, v) ->
    state.registers.(vx) <- state.registers.(vx) + v;
    state.pc := !(state.pc) + 2
  | Put_reg_in_reg (vx, vy) ->
    state.registers.(vx) <- state.registers.(vy);
    state.pc := !(state.pc) + 2
  | Or_reg_with_reg (vx, vy) ->
    state.registers.(vx) <- state.registers.(vx) |: state.registers.(vy);
    state.pc := !(state.pc) + 2
  | And_reg_with_reg (vx, vy) ->
    state.registers.(vx) <- state.registers.(vx) &: state.registers.(vy);
    state.pc := !(state.pc) + 2
  | Xor_reg_with_reg (vx, vy) ->
    state.registers.(vx) <- state.registers.(vx) ^: state.registers.(vy);
    state.pc := !(state.pc) + 2
  | Add_reg_to_reg (vx, vy) ->
    let result = state.registers.(vx) + state.registers.(vy) in
    state.registers.(15) <- (if result > 255 then 1 else 0);
    state.registers.(vx) <- result % 255;
    state.pc := !(state.pc) + 2
  | Sub_reg_from_reg (vx, vy) ->
    let x, y = state.registers.(vx), state.registers.(vy) in
    state.registers.(15) <- (if x > y then 1 else 0);
    state.registers.(vx) <- x - y;
    state.pc := !(state.pc) + 2
  | Shift_right vx ->
    let x = state.registers.(vx) in
    state.registers.(15) <- (if x &: 0x01 = 0x01 then 1 else 0);
    state.registers.(vx) <- x >> 1;
    state.pc := !(state.pc) + 2
  | Sub_reg_from_reg_rev (vx, vy) ->
    let x, y = state.registers.(vx), state.registers.(vy) in
    state.registers.(15) <- (if y > x then 1 else 0);
    state.registers.(vx) <- y - x;
    state.pc := !(state.pc) + 2
  | Shift_left vx ->
    let x = state.registers.(vx) in
    state.registers.(15) <- (if x &: 0x80 = 0x80 then 1 else 0);
    state.registers.(vx) <- state.registers.(vx) << 1;
    state.pc := !(state.pc) + 2
  | Skip_if_reg_neq (vx, vy) ->
    let increment = if state.registers.(vx) <> state.registers.(vy) then 4 else 2 in
    state.pc := !(state.pc) + increment
  | Set_index v ->
    state.i := v;
    state.pc := !(state.pc) + 2
  | Jump_relative address ->
    state.pc := state.registers.(0) + address;
    state.pc := !(state.pc) + 2
  | Random_byte (vx, v) ->
    state.registers.(vx) <- Random.int_incl 0 255 &: v;
    state.pc := !(state.pc) + 2
;;

(** *)
let draw state : unit =
  let open Raylib in
  begin_drawing ();
  clear_background Color.black;
  for y = 0 to facts.height - 1 do
    for x = 0 to facts.width - 1 do
      let scale = config.pixel_scale in
      let i = (y * facts.width) + x in
      draw_rectangle (x * scale) (y * scale) scale scale state.screen.(i)
    done
  done;
  end_drawing ()
;;

(** *)
let run state : unit =
  let instruction = state |> fetch |> decode in
  execute state instruction
;;

let load_example state : unit =
  let ch = Char.of_int_exn in
  let pc = !(state.pc) in
  state.memory.(pc + 0) <- ch 0;
  (* CLS *)
  state.memory.(pc + 1) <- ch 0xE0;
  state.memory.(pc + 2) <- ch 0x12;
  (* JUMP 0x204 *)
  state.memory.(pc + 3) <- ch 0x04;
  state.memory.(pc + 4) <- ch 0;
  (* HALT *)
  state.memory.(pc + 5) <- ch 1;
  Stdio.printf
    "%02x%02x%02x%02x%02x%02x\n"
    (Char.to_int state.memory.(pc + 0))
    (Char.to_int state.memory.(pc + 1))
    (Char.to_int state.memory.(pc + 2))
    (Char.to_int state.memory.(pc + 3))
    (Char.to_int state.memory.(pc + 4))
    (Char.to_int state.memory.(pc + 5))
;;

let () =
  let state = setup () in
  load_example state;
  let open Raylib in
  init_window 800 600 "CHIP-8";
  set_target_fps 60;
  while not @@ window_should_close () do
    run state;
    draw state
  done;
  close_window ()
;;
