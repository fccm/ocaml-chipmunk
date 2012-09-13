#!/usr/bin/env ocaml
#load "str.cma"

type t = {
    fun_name_get: string;
    fun_name_set: string;
    item_type: string;
    param_name: string;
    struct_member: string;
    type_conv_c2ml: string -> string * string;
    type_conv_ml2c: unit -> string * string;
    ml_type: string;
  }

exception Not_used ;;

let conv_ml_to_c = function
  | "int" -> (fun () -> "", "Int_val(val)")
  | "cpFloat"
  | "float" -> (fun () -> "", "Double_val(val)")
  | "cpVect" -> (fun () -> "
        cpVect _val;
        _val.x = Double_field(val,0);
        _val.y = Double_field(val,1);\n", "_val")

  (* Read only members *)
  | "int_readonly" -> (fun () -> raise Not_used)
  | "cpArray" -> (fun () -> raise Not_used)
  | "cpShape" -> (fun () -> raise Not_used)
  | "cpVectArray" -> (fun () -> raise Not_used)
  | unknown -> invalid_arg unknown
;;


let conv_c_to_ml ml_type str = match ml_type with
  | "int" -> ("return Val_int(", ");")
  | "int_readonly" -> ("return Val_int(", ");")
  | "cpFloat"
  | "float" -> ("return caml_copy_double(", ");")
  | "cpArray" -> ("return (value) ", ";")
  | "cpShape" -> ("return (value) ", ";")
  | "cpVect" -> ("\
        CAMLparam0();
        CAMLlocal1( ml_ret );

        cpVect _ret = ", ";
        
        ml_ret = caml_alloc(2 * Double_wosize, Double_array_tag);
        Store_double_field( ml_ret, 0, _ret.x );
        Store_double_field( ml_ret, 1, _ret.y );

        CAMLreturn( ml_ret );")

  | "cpVectArray" -> (Printf.sprintf "\
        CAMLparam0();
        CAMLlocal2( ml_ret, vert_i );

        int num = ((%s *)this)->numVerts;
        cpVect *verts = " str, ";

        ml_ret = caml_alloc(num, 0);

        for (int i=0; i<num; i++) {

                vert_i = caml_alloc(2 * Double_wosize, Double_array_tag);

                Store_double_field( vert_i, 0, verts[i].x );
                Store_double_field( vert_i, 1, verts[i].y );

                Store_field( ml_ret, i, vert_i );
        }

        CAMLreturn( ml_ret );")

  | unknown -> invalid_arg unknown
;;


let get_ml_type t = match t with
  | "int"
  | "float"
  | "cpShape"
  | "cpArray"
  | "cpVect"
  | "cpVectArray" -> t
  | "cpFloat" -> "float"
  | "int_readonly" -> "int"
  | other ->
      Printf.fprintf stderr "Warning: ml type not recognised: %s" other;
      (other)
;;


let find_pos str c =
  let len = String.length str in
  let rec aux i =
    if i >= len then
      (Printf.printf "character '%c' not found in:\n%s\n%!" c str;
       raise Not_found)
    else if str.[i] = c then i
    else aux (succ i)
  in
  aux 0
;;


let strip_spaces str =
  let r = Array.of_list(Str.split (Str.regexp "[ \t\r\n]+") str) in
  r.(0)
;;


let parse_member st memb =
try
  let open_par = find_pos memb '('
  and close_par = find_pos memb ')'
  in
  let access_func = strip_spaces(String.sub memb 0 open_par)
  and cont_str = String.sub memb (open_par +1) (close_par - open_par -1)
  in
  let membs = Array.of_list(Str.split (Str.regexp "[ :]+") cont_str) in
  if Array.length membs <> 2 &&
     Array.length membs <> 3 then (
             Array.iter (Printf.printf "%s\n%!") membs;
             invalid_arg cont_str);
  {
    fun_name_get = st.(0) ^ "Get" ^ access_func;
    fun_name_set = st.(0) ^ "Set" ^ access_func;
    item_type = st.(0);
    param_name = st.(1);
    struct_member = membs.(1);
    type_conv_ml2c = conv_ml_to_c membs.(0);
    type_conv_c2ml = conv_c_to_ml membs.(0);
    ml_type = get_ml_type membs.(0);
  }
with
  Not_found ->
    Printf.printf "parse_member:\n%s\n" memb;
    exit 1;
;;


let parse_struct struct_ =
try
  (* structure name and type *)
  let open_bra = find_pos struct_ '{'
  and close_bra = find_pos struct_ '}' in
  let st_str = String.sub struct_ 0 open_bra in
  let st = Array.of_list(Str.split (Str.regexp "[ \r\n]+") st_str) in
  if Array.length st <> 2 then
    invalid_arg st_str;

  (* members of the structure *)
  let membs_str = String.sub struct_ (open_bra +1) (close_bra - open_bra -1) in
  let membs_li = Str.split (Str.regexp ",[ \r\n]*") membs_str in
  let membs = List.map (parse_member st) membs_li in
  (membs)
with
  Not_found ->
    Printf.printf "parse_struct:\n%s\n" struct_;
    exit 1;
;;


let parse_input structures =
  let structs_s = Str.split (Str.regexp ";[ \r\n]*") structures in
  let structs_t = List.map parse_struct structs_s in
  (structs_t)
;;


let print_set_func_c t =
try
  Printf.printf "
CAMLprim value
ml_%s(value this, value val)
{%s
        ((%s *)this)->%s = %s;
        return Val_unit;
}\n"
      t.fun_name_set
      (fst(t.type_conv_ml2c()))
      t.item_type
      t.struct_member
      (snd(t.type_conv_ml2c()));
with
  Not_used -> ()
;;


let print_get_func_c t =
  Printf.printf "
CAMLprim value
ml_%s(value this)
{
        %s ((%s *)this)->%s %s
}\n"
      t.fun_name_get
      (fst(t.type_conv_c2ml  t.item_type))
      t.item_type
      t.struct_member
      (snd(t.type_conv_c2ml  t.item_type));
;;


let print_set_func_ml t =
try
  ignore(t.type_conv_ml2c()); (* skip read only members *)
  Printf.printf "
external %s: %s:%s -> %s:%s -> unit = \"ml_%s\"\n"
      t.fun_name_set
      t.param_name
      t.item_type
      t.struct_member
      t.ml_type
      t.fun_name_set;
with
  Not_used -> ()
;;


let print_get_func_ml t =
  Printf.printf "
external %s: %s:%s -> %s = \"ml_%s\"\n"
      t.fun_name_get
      t.param_name
      t.item_type
      t.ml_type
      t.fun_name_get;
;;


let there_is_arg arg =
  let argc = Array.length Sys.argv in
  let rec aux i =
    if i >= argc then false
    else if Sys.argv.(i) = arg then true
      else aux(succ i)
  in
  aux 0
;;


let is_comment line =
  let len = String.length line in
  if len >= 2 && line.[0] = '/' && line.[1] = '/'
  then true else false
;;

let input_stdin () =
  let buf = Buffer.create 2000 in
  try
    let rec aux() =
      let line = input_line stdin in
      if not(is_comment line) then
        Buffer.add_string buf line;
        Buffer.add_char buf '\n';
      aux()
    in
    aux();
  with
    End_of_file -> (Buffer.contents buf)
;;


let () =
  let structures = input_stdin() in
  let structs = List.flatten(parse_input structures) in
  if there_is_arg "--gen-c" then begin
    List.iter print_set_func_c structs;
    List.iter print_get_func_c structs;
  end;
  if there_is_arg "--gen-ml" then begin
    List.iter print_set_func_ml structs;
    List.iter print_get_func_ml structs;
  end;
;;

