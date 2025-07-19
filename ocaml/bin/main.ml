open Base

(** *)
module Instruction = struct
  type encoded = int * int

  (** *)
  type decoded =
    | Clear_Screen (** Clears the screen *)
    | Return (** PC = top stack *)
  (*
     | Jump of int (** PC = given address *)
     | Call of int (** ++SP = PC, PC = given address *)
     | Skip_if_val_eq of int (** PC += 2 if equal *)
     | Skip_if_val_neq of int (** PC += 2 if not equal *)
     | Skip_if_reg_eq of int (** PC += 2 if equal *)
     | Skip_if_reg_neq of int (** PC += 2 if not equal *)
  *)
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
  ; stack : int list ref
      (* TODO: Emulates stack until usage within memory block is figured out *)
  ; memory : memory
  ; screen : Raylib.Color.t array
  }

(** *)
let setup () : state =
  let screen_len = facts.width * facts.height in
  { pc = ref 0x200
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
