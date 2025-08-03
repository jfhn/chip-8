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
    | Draw of int * int * int
    (** DRW Dxyn: draw n-byte sprite at Vx, Vy, set VF = collision *)
    | Skip_if_key_down of int (** SKP Ex9E: If key in Vx is pressed then PC += 2 *)
    | Skip_if_not_key_down of int
    (** SKNP Ex9E: If key in Vx is not pressed then PC += 2 *)
    | Read_delay of int (** LD Fx07: Vx = delay *)
    | Wait_for_key_press of int (** LD Fx0A: Vx = key *)
    | Set_delay of int (** LD Fx15: delay = Vx *)
    | Set_sound_timer of int (** LD Fx18: sound timer = Vx *)
    | Increment_index of int (** ADD Fx1E: I = I + Vx *)
    | Set_index_sprite_digit of int (** LD Fx29: I = location of sprite for digit Vx *)
    | Store_bcd of int (** LD Fx33: Vx = nmk, I = n00, I+1 = m0, I+2 = k *)
    | Save_registers of int (** LD Fx55: I+0 = V0, ..., I+x = Vx *)
    | Restore_registers of int (** LD Fx65: V0 = I+0, ... Vx = I+x *)

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
    | Draw (vx, vy, n) -> sprintf "DRW V%x V%x %x" vx vy n
    | Skip_if_key_down vx -> sprintf "SKP V%x" vx
    | Skip_if_not_key_down vx -> sprintf "SKNP V%x" vx
    | Read_delay vx -> sprintf "LD V%x DT" vx
    | Wait_for_key_press vx -> sprintf "LD V%x K" vx
    | Set_delay vx -> sprintf "LD DT V%x" vx
    | Set_sound_timer vx -> sprintf "LD ST V%x" vx
    | Increment_index vx -> sprintf "ADD I V%x" vx
    | Set_index_sprite_digit vx -> sprintf "LD F V%x" vx
    | Store_bcd vx -> sprintf "LD B V%x" vx
    | Save_registers vx -> sprintf "LD [I] V%x" vx
    | Restore_registers vx -> sprintf "LD V%x [I]" vx
  ;;
end

type config =
  { pixel_scale : int
  ; pixel_white : Raylib.Color.t
  ; digits_begin : int
  ; debug : bool
  }

type facts =
  { width : int
  ; height : int
  ; background_color : Raylib.Color.t
  ; memory : int
  ; digit_sprite_size : int
  }

(** *)
let facts : facts =
  { width = 64
  ; height = 32
  ; background_color = Raylib.Color.black
  ; memory = 4096
  ; digit_sprite_size = 5 * 4
  }
;;

type memory = char array

(** *)
type state =
  { pc : int ref
  ; i : int ref
  ; registers : int array
  ; keys : bool array
  ; delay_timer : int ref
  ; sound_timer : int ref
  ; stack : int list ref
    (* TODO: Emulates stack until usage within memory block is figured out *)
  ; memory : memory
  ; screen : bool array
  }

(** *)
let setup () : state =
  let screen_len = facts.width * facts.height in
  { pc = ref 0x200
  ; i = ref 0
  ; registers = Array.create ~len:16 0
  ; keys = Array.create ~len:16 false
  ; delay_timer = ref 0
  ; sound_timer = ref 0
  ; stack = ref []
  ; memory = Array.create ~len:facts.memory (Char.of_int_exn 0)
  ; screen = Array.create ~len:screen_len false
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
  let n = code &: 0x000F in
  match code with
  | 0x0000 -> Invalid
  | 0x0001 -> Halt
  | 0x00E0 -> Clear_Screen
  | 0x00EE -> Return
  | code when code &: 0xF000 = 0x1000 -> Jump address
  | code when code &: 0xF000 = 0x2000 -> Call address
  | code when code &: 0xF000 = 0x3000 -> Skip_if_val_eq (vx, v)
  | code when code &: 0xF000 = 0x4000 -> Skip_if_val_neq (vx, v)
  | code when code &: 0xF00F = 0x5000 -> Skip_if_reg_eq (vx, vy)
  | code when code &: 0xF000 = 0x6000 -> Put_val_in_reg (vx, v)
  | code when code &: 0xF000 = 0x7000 -> Add_val_to_reg (vx, v)
  | code when code &: 0xF00F = 0x8000 -> Put_reg_in_reg (vx, vy)
  | code when code &: 0xF00F = 0x8001 -> Or_reg_with_reg (vx, vy)
  | code when code &: 0xF00F = 0x8002 -> And_reg_with_reg (vx, vy)
  | code when code &: 0xF00F = 0x8003 -> Xor_reg_with_reg (vx, vy)
  | code when code &: 0xF00F = 0x8004 -> Add_reg_to_reg (vx, vy)
  | code when code &: 0xF00F = 0x8005 -> Sub_reg_from_reg (vx, vy)
  | code when code &: 0xF00F = 0x8006 -> Shift_right vx
  | code when code &: 0xF00F = 0x8007 -> Sub_reg_from_reg_rev (vx, vy)
  | code when code &: 0xF00F = 0x800E -> Shift_left vx
  | code when code &: 0xF00F = 0x9000 -> Skip_if_reg_neq (vx, vy)
  | code when code &: 0xF000 = 0xA000 -> Set_index address
  | code when code &: 0xF000 = 0xB000 -> Jump_relative address
  | code when code &: 0xF000 = 0xC000 -> Random_byte (vx, v)
  | code when code &: 0xF000 = 0xD000 -> Draw (vx, vy, n)
  | code when code &: 0xF0FF = 0xE09E -> Skip_if_key_down vx
  | code when code &: 0xF0FF = 0xE0A1 -> Skip_if_not_key_down vx
  | code when code &: 0xF0FF = 0xF007 -> Read_delay vx
  | code when code &: 0xF0FF = 0xF00A -> Wait_for_key_press vx
  | code when code &: 0xF0FF = 0xF015 -> Set_delay vx
  | code when code &: 0xF0FF = 0xF018 -> Set_sound_timer vx
  | code when code &: 0xF0FF = 0xF01E -> Increment_index vx
  | code when code &: 0xF0FF = 0xF029 -> Set_index_sprite_digit vx
  | code when code &: 0xF0FF = 0xF033 -> Store_bcd vx
  | code when code &: 0xF0FF = 0xF055 -> Save_registers vx
  | code when code &: 0xF0FF = 0xF065 -> Restore_registers vx
  | code -> Printf.sprintf "unknown instruction: 0x%04x\n" code |> failwith
;;

(** Simulates one instruction and updates the state *)
let execute state config : Instruction.decoded -> unit =
  fun instruction ->
  Stdio.printf "execute %s\n" (Instruction.show instruction);
  match instruction with
  | Invalid -> raise Invalid
  | Halt -> raise Halt
  | Clear_Screen ->
    let len = facts.width * facts.height in
    for i = 0 to len - 1 do
      state.screen.(i) <- false
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
  | Draw (vx, vy, n) ->
    (* Draw 8xn sprites *)
    let char_to_bool (ch : char) : bool = Char.to_int ch <> 0 in
    let xor a b = Bool.(a <> b) in
    for y = 0 to n - 1 do
      for x = 0 to 7 do
        let tx = vx + x + if vx + x > facts.width then -facts.width else 0 in
        let ty =
          vy + y + if vy + y > facts.height then -(facts.width * facts.height) else 0
        in
        let ti = (ty * facts.width) + tx in
        let source = char_to_bool state.memory.(!(state.i) + (y * 8) + x) in
        (* If the value is erased (i.e. it *was* set), we set VF = 1 *)
        if state.screen.(ti) && not source then state.registers.(15) <- 1;
        state.screen.(ti) <- xor state.screen.(ti) source
      done
    done;
    state.pc := !(state.pc) + 2
  | Skip_if_key_down vx ->
    let key = state.registers.(vx) in
    if key >= 16
    then failwith "invalid key"
    else (
      let increment = if state.keys.(state.registers.(vx)) then 4 else 2 in
      state.pc := !(state.pc) + increment)
  | Skip_if_not_key_down vx ->
    let key = state.registers.(vx) in
    if key >= 16
    then failwith "invalid key"
    else (
      let increment = if state.keys.(state.registers.(vx)) then 4 else 2 in
      state.pc := !(state.pc) + increment)
  | Read_delay vx ->
    state.registers.(vx) <- !(state.delay_timer);
    state.pc := !(state.pc) + 2
  | Wait_for_key_press vx ->
    let open Raylib in
    let key : int ref = Key.Null |> Key.to_int |> ref in
    while !key = Key.to_int Key.Null do
      key := get_key_pressed () |> Key.to_int
    done;
    state.registers.(vx) <- !key
  | Set_delay vx ->
    state.delay_timer := state.registers.(vx);
    state.pc := !(state.pc) + 2
  | Set_sound_timer vx ->
    state.sound_timer := state.registers.(vx);
    state.pc := !(state.pc) + 2
  | Increment_index vx ->
    state.i := !(state.i) + state.registers.(vx);
    state.pc := !(state.pc) + 2
  | Set_index_sprite_digit vx ->
    let i = state.registers.(vx) in
    if i >= 16 then failwith "internal error: too large digit index";
    state.i := config.digits_begin + (i * facts.digit_sprite_size)
  | Store_bcd vx ->
    (match Printf.sprintf "%03d" state.registers.(vx) |> String.to_list with
     | [ _; _; _ ] as digits ->
       let store offset ch : int =
         state.memory.(!(state.i) + offset) <- ch;
         offset + 1
       in
       List.fold_left digits ~init:0 ~f:store |> ignore
     | _ -> failwith "internal error: BCD has not exactly 3 digits")
  | Save_registers vx ->
    let store vi =
      state.memory.(!(state.i) + vi) <- Char.of_int_exn state.registers.(vi)
    in
    List.range ~stop:`inclusive 0 vx |> List.iter ~f:store
  | Restore_registers vx ->
    let restore vi = state.registers.(vi) <- Char.to_int state.memory.(!(state.i) + vi) in
    List.range ~stop:`inclusive 0 vx |> List.iter ~f:restore
;;

(** *)
let draw state config : unit =
  let open Raylib in
  begin_drawing ();
  clear_background Color.black;
  for y = 0 to facts.height - 1 do
    for x = 0 to facts.width - 1 do
      let scale = config.pixel_scale in
      let i = (y * facts.width) + x in
      let color =
        if state.screen.(i) then config.pixel_white else facts.background_color
      in
      draw_rectangle (x * scale) (y * scale) scale scale color
    done
  done;
  end_drawing ()
;;

(** *)
let step state config : unit =
  if !(state.delay_timer) < 0 then state.delay_timer := !(state.delay_timer) - 1;
  if !(state.sound_timer) < 0 then state.sound_timer := !(state.sound_timer) - 1;
  let instruction = state |> fetch |> decode in
  execute state config instruction
;;

let load_program state path : unit =
  let content = Stdio.In_channel.read_all path |> String.to_list in
  let f i ch : int =
    state.memory.(i) <- ch;
    i + 1
  in
  List.fold_left ~init:!(state.pc) ~f content |> ignore
;;

let parse_args () : string =
  match Sys.get_argv () |> Array.to_list with
  | [ _; path ] -> path
  | program :: _ -> "Usage: " ^ program ^ " <path-to-file>" |> failwith
  | _ -> failwith "unreachable"
;;

(** Debug layout:
    [ actions ]
    [ registers, PC, I, V..., float left
      ... ]
    [ stack, growing -> ]
 *)
type debug_gui_config = { button_height : int; button_min_width : int; margin_top: int }

let debug_gui_config : debug_gui_config =
  { button_height = 30; button_min_width = 100; margin_top = 10 }
;;

type window_info = { screen_dim : int * int; }

let debug state config window_info : unit =
  let open Raylib in
  (* Input handling *)
  (match get_char_pressed () |> Uchar.to_char_exn with
   | 'q' -> close_window ()
   | 's' -> step state config
   | 'k' -> failwith "key input not implemented"
   | _ -> ());
  (* Draw *)
  draw state config;
  let { button_height; _ } = debug_gui_config in
  let text_padding = 2 in
  let font_size = button_height - 2 * text_padding in
  let draw_button x y text : int =
    let text_width = measure_text text font_size in
    let width = max debug_gui_config.button_min_width (text_width + 2 * text_padding) in
    draw_rectangle x y width debug_gui_config.button_height Color.lightgray;
    draw_text text (x + text_padding) (y + text_padding) font_size Color.black;
    width
  in
  (* Draw actions *)
  let actions: (string * string) list = [ "q", "quit"; "s", "step"; "k", "key input" ] in
  let _, sy = window_info.screen_dim in
  let y = sy + debug_gui_config.margin_top in
  let f x (key, desc) = x + (draw_button x y (key ^ " - " ^ desc)) + 10 in
  actions |> List.fold_left ~init:0 ~f |> ignore;
  (* Draw first row registers *)
  let open Printf in
  let y = sy + debug_gui_config.button_height + 2 * debug_gui_config.margin_top in
  let pc_width = draw_button 0 y (sprintf "PC: %x" !(state.pc)) in
  let i_width = draw_button (pc_width + 10) y (sprintf "I: %x" !(state.i)) in
  (* NOTE: Manual screen wrap *)
  let f x i = x + (draw_button x y (sprintf "V%x: %x" i state.registers.(i))) + 10 in
  Sequence.range 0 7 |> Sequence.to_list |> List.fold_left ~init:(pc_width + i_width + 20) ~f |> ignore;
  (* Draw second row registers *)
  let y = sy + 2 * debug_gui_config.button_height + 3 * debug_gui_config.margin_top in
  let f x i = x + (draw_button x y (sprintf "V%x: %x" i state.registers.(i))) + 10 in
  Sequence.range 7 16 |> Sequence.to_list |> List.fold_left ~init:0 ~f |> ignore;
  (* Draw stack *)
  let y = sy + 3 * debug_gui_config.button_height + 4 * debug_gui_config.margin_top in
  let f x item = x + (draw_button x y (sprintf "%x" item)) + 10 in
  !(state.stack) |> Fn.flip List.take 10 |> List.fold_left ~init:0 ~f |> ignore
;;

let run state config : unit =
  step state config;
  draw state config
;;

let with_window config (f : window_info -> unit) : unit =
  let open Raylib in
  let width = facts.width * config.pixel_scale in
  let height = facts.height * config.pixel_scale in
  let window_info = (match config.debug with
   | false ->
     init_window width height "CHIP-8";
     { screen_dim = width, height }
   | true ->
     let { button_height; button_min_width; margin_top } = debug_gui_config in
     let button_rows = 2 + (button_min_width * 18 / width + 1) in
     let debug_height = button_rows * button_height + button_rows * margin_top in
     Stdio.printf "open debug window screen %dx%d and full width %d\n" width height (height + debug_height);
     init_window width (height + debug_height) "CHIP-8 (Debug)";
     { screen_dim = width, height }) in
  set_target_fps 60;
  while not @@ window_should_close () do
    f window_info
  done;
  close_window ()
;;

(* TODO? Make config passable via cmd args? *)
let config : config =
  { pixel_scale = 16
  ; pixel_white = Raylib.Color.raywhite
  ; digits_begin = 0
  ; debug = true
  }
;;

let () =
  let input_path = parse_args () in
  let state = setup () in
  load_program state input_path;
  match config.debug with
  | true -> with_window config (fun info -> debug state config info)
  | false -> with_window config (fun _ -> run state config)
;;
