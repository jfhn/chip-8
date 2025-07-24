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
    | Skip_if_reg_eq (vx, vy) -> sprintf "SNE %x %x" vx vy
    | Put_val_in_reg (vx, v) -> sprintf "LD V%x %02x" vx v
    | Add_val_to_reg (vx, v) -> sprintf "ADD V%x %02x" vx v
    | Put_reg_in_reg (vx, vy) -> sprintf "LD V%x V%x" vx vy
    | Or_reg_with_reg (vx, vy) -> sprintf "OR V%x V%x" vx vy
    | And_reg_with_reg (vx, vy) -> sprintf "AND V%x V%x" vx vy
    | Xor_reg_with_reg (vx, vy) -> sprintf "XOR V%x V%x" vx vy
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
  | code when code &: 0x5000 = 0x5000 ->
    (* TODO: Is this pattern correct? *)
    Skip_if_reg_eq (vx, vy)
  | code when code &: 0x6000 = 0x6000 -> Put_val_in_reg (vx, v)
  | code when code &: 0x7000 = 0x7000 -> Add_val_to_reg (vx, v)
  | code when code &: 0x8000 = 0x8000 ->
    (* TODO: Is this pattern correct? *)
    Put_reg_in_reg (vx, vy)
  | code when code &: 0x8001 = 0x8001 -> Or_reg_with_reg (vx, vy)
  | code when code &: 0x8002 = 0x8002 -> And_reg_with_reg (vx, vy)
  | code when code &: 0x8003 = 0x8003 -> Xor_reg_with_reg (vx, vy)
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
    let increment = if state.registers.(vx) <> state.registers.(vy) then 4 else 2 in
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
