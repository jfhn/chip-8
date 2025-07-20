open Base

(** *)
module Instruction = struct
  type encoded = int * int

  (** *)
  type decoded =
    | Clear_Screen (** Clears the screen *)
    | Return (** PC = top stack *)
    | Jump of int (** PC = given address *)
    | Call of int (** ++SP = PC, PC = given address *)
    | Skip_if_val_eq of int * int (** SE 3xkk: if Vx = kk then PC += 2 *)
    | Skip_if_val_neq of int * int (** SNE 4xkk: if Vx <> kk then PC += 2 *)
    | Skip_if_reg_eq of int * int (** SE 5xy0: if Vx = Vy then PC += 2 *)
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

(** *)
let fetch (_memory : memory) : Instruction.encoded = 0, 0xE0

let decode : Instruction.encoded -> Instruction.decoded = function
  | 0, 0xE0 -> Clear_Screen
  | 0, 0xEE -> Return
  | high, low when Int.bit_and high 0x10 = 0x10 ->
    let address = (Int.bit_and high 0x0F |> Fn.flip Int.shift_left 2) + low in
    Jump address
  | high, low when Int.bit_and high 0x20 = 0x20 ->
    let address = (Int.bit_and high 0x0F |> Fn.flip Int.shift_left 2) + low in
    Call address
  | high, low when Int.bit_and high 0x30 = 0x30 ->
    let register = Int.bit_and high 0x0F in
    Skip_if_val_eq (register, low)
  | high, low when Int.bit_and high 0x40 = 0x40 ->
    let register = Int.bit_and high 0x0F in
    Skip_if_val_neq (register, low)
  | high, low when Int.bit_and high 0x50 = 0x50 && Int.bit_and low 0 = 0 ->
    (* TODO: Is this pattern correct? *)
    let register1 = Int.bit_and high 0x0F in
    let register2 = Int.bit_and low 0xF0 in
    Skip_if_reg_eq (register1, register2)
  | _ -> failwith "unknown instruction"
;;

(** Simulates one instruction and updates the state *)
let execute state : Instruction.decoded -> unit = function
  | Clear_Screen ->
    let len = facts.width * facts.height in
    for i = 0 to len - 1 do
      state.screen.(i) <- facts.background_color
    done;
    state.pc := !(state.pc) + 1
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
  | Skip_if_val_eq (register, value) ->
    let increment = if state.registers.(register) = value then 2 else 1 in
    state.pc := !(state.pc) + increment
  | Skip_if_val_neq (register, value) ->
    let increment = if state.registers.(register) <> value then 2 else 1 in
    state.pc := !(state.pc) + increment
  | Skip_if_reg_eq (register1, register2) ->
    let increment =
      if state.registers.(register1) <> state.registers.(register2) then 2 else 1
    in
    state.pc := !(state.pc) + increment
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
  let instruction = state.memory |> fetch |> decode in
  execute state instruction
;;

let () =
  let state = setup () in
  let open Raylib in
  init_window 800 600 "CHIP-8";
  set_target_fps 60;
  while not @@ window_should_close () do
    run state;
    draw state
  done;
  close_window ()
;;
