#!/usr/bin/env ocaml
(* {{{ COPYING *(

  Code generator for wrapping C functions to OCaml.
  Copyright (C) 2008  Florent Monnier  <fmonnier@linux-nantes.org>

  This program is free software: you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation, either version 3
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see  <http://www.gnu.org/licenses/>

)* }}} *)
#load "str.cma"


let find_pos c line len =
  let rec aux i =
    if i >= len then raise Not_found else
    if line.[i] = c then i
    else aux(succ i)
  in
  aux 0
;;


let swap_star parts =
  (* if string was "type *var" *)
  if parts.(1).[0] <> '*'
  then (parts.(0), parts.(1))
  else
    let len = String.length parts.(1) in
    let arg_name = String.sub parts.(1) 1 (len-1) in
    ( parts.(0) ^ " *", arg_name )
;;


let detach_star parts =
  (* if string was "type *var" *)
  if parts.(1).[0] <> '*'
  then ( (parts.(0), ""), parts.(1))
  else
    let len = String.length parts.(1) in
    let arg_name = String.sub parts.(1) 1 (len-1) in
    ( (parts.(0), "*"), arg_name )
;;


let get_func_name line =
  let len = String.length line in
  let open_pos = find_pos '(' line len in
  let func =
    String.sub line 0 (open_pos)
  in
  let parts = Array.of_list(Str.split (Str.regexp "[ ]+") func) in
  if Array.length parts <> 2 then invalid_arg func;
  let func_type, func_name = swap_star parts in
  (func_type, func_name)
;;


let get_args line =
  let len = String.length line in
  let open_pos = find_pos '(' line len
  and close_pos = find_pos ')' line len in
  let args = String.sub line (succ open_pos) (pred close_pos - open_pos) in
  (args)
;;


let parse_arg arg =
  let parts = Array.of_list(Str.split (Str.regexp "[ ]+") arg) in
  let parts =
    if parts = [|"void"|] then [|"void"; " "|] else parts
  in
  if Array.length parts <> 2 then invalid_arg arg;
  let arg_type, arg_name = detach_star parts in
  (arg_type, arg_name)
;;


type arg_t = { arg_name:string; ml_type:string; from_ml:string; at_call:string; prev_conv:string }


let prepare_arg ((arg_type, pointer), arg_name) =
  match arg_type, pointer with
  | "void", "" ->
      { arg_name= "";
        ml_type= "unit";
        from_ml= "unit";
        at_call= "";
        prev_conv= "" }
  | "int", "" ->
      { arg_name= arg_name ^":";
        ml_type= "int";
        from_ml= arg_name;
        at_call= Printf.sprintf "Int_val(%s)" arg_name;
        prev_conv= "" }
  | "cpFloat", "" ->
      { arg_name= arg_name ^":";
        ml_type= "float";
        from_ml= arg_name;
        at_call= Printf.sprintf "Double_val(%s)" arg_name;
        prev_conv= "" }
  | _, "*" ->
      { arg_name= arg_name ^":";
        ml_type= arg_type;
        from_ml= arg_name;
        at_call= Printf.sprintf "(%s *) %s" arg_type arg_name;
        prev_conv= "" }
  | "cpVect", "" ->
      { arg_name= arg_name ^":";
        ml_type= "cpVect";
        from_ml= "ml_" ^ arg_name;
        at_call= arg_name;
        prev_conv= Printf.sprintf "
    cpVect %s;
    %s.x = Double_field(ml_%s,0);
    %s.y = Double_field(ml_%s,1);\n"
    arg_name arg_name arg_name arg_name arg_name; }
  | _,_ ->
      failwith(Printf.sprintf "argument (%s) %s" arg_type arg_name)
;;


let args_call_f arr_arg len =
  let buf = Buffer.create 30 in
  for i=0 to pred len do
    Buffer.add_string buf (
      Printf.sprintf " %s," arr_arg.(i).at_call)
  done;
  let len = Buffer.length buf
  and content = Buffer.contents buf in
  content.[pred len] <- ' ';
  (content)
;;


let from_ml_f arr_arg len =
  let buf = Buffer.create 30 in
  for i=0 to pred len do
    Buffer.add_string buf (
      Printf.sprintf " value %s," arr_arg.(i).from_ml)
  done;
  let len = Buffer.length buf
  and content = Buffer.contents buf in
  content.[pred len] <- ' ';
  (content)
;;


let prev_conv_f arr_arg len =
  let buf = Buffer.create 30 in
  for i=0 to pred len do
    Buffer.add_string buf arr_arg.(i).prev_conv
  done;
  (Buffer.contents buf)
;;


let get_c_return func_type =
  match func_type with
  | "void" -> ("", "", "return Val_unit;")
  | "cpJoint *" -> ("", "cpJoint *joint_p = ", "return (value) joint_p;")
  | "cpVect" ->
      let mem_gc = Printf.sprintf "
    CAMLparam0();
    CAMLlocal1( ml_ret );\n"

      and func_return = "cpVect _ret = "
      and return_to_ml = Printf.sprintf "
    ml_ret = caml_alloc(2 * Double_wosize, Double_array_tag);

    Store_double_field( ml_ret, 0, _ret.x );
    Store_double_field( ml_ret, 1, _ret.y );

    CAMLreturn( ml_ret );"
      in
      (mem_gc, func_return, return_to_ml)
  | _ ->
      failwith(Printf.sprintf "C return type TOD0: '%s'" func_type)
;;


let get_ml_return func_type =
  match func_type with
  | "void" -> ("unit")
  | "cpJoint *" -> ("cpJoint")
  | "cpVect" -> ("cpVect")
  | _ ->
      failwith(Printf.sprintf "ML return type TOD0: '%s'" func_type)
;;


let ml_func_type_f arr_arg len =
  let buf = Buffer.create 30 in
  for i=0 to pred len do
    Buffer.add_string buf (
      Printf.sprintf " %s%s ->" arr_arg.(i).arg_name  arr_arg.(i).ml_type)
  done;
  (Buffer.contents buf)
;;


let print_ml_func (func_type, func_name) argv_t =
  let arr_arg = Array.of_list argv_t in
  let len = Array.length arr_arg in

  let ml_func_type = ml_func_type_f arr_arg len
  and ml_return = get_ml_return func_type
  and bc_call =
    if len <= 5 then "" else (Printf.sprintf " \"ml_%s_bc\"" func_name)
  in
  Printf.printf "\nexternal %s: %s %s" func_name ml_func_type ml_return;
  Printf.printf " =%s \"ml_%s\"" bc_call func_name;
  print_newline();
;;


let print_bc_func func_name len =
  let rec aux i acc =
    if i >= len then 
      let len = String.length acc in
      (acc.[pred len] <- ' '; acc)
    else
      let arg = Printf.sprintf " argv[%d]," i in
      aux (succ i) (acc ^ arg)
  in
  let argv = aux 0 "" in
  Printf.printf "
CAMLprim value
ml_%s_bc(value * argv, int argn)
{
    return ml_%s(
     %s);
}
"   func_name func_name argv;
;;


let print_c_func (func_type, func_name) argv_t =
  let arr_arg = Array.of_list argv_t in
  let len = Array.length arr_arg in

  let from_ml = from_ml_f arr_arg len
  and prev_conv = prev_conv_f arr_arg len
  and args_call = args_call_f arr_arg len
  and mem_gc, func_ret, ret_to_ml  = get_c_return func_type
  in
  Printf.printf "\nCAMLprim value\nml_%s(%s)\n{" func_name from_ml;
  Printf.printf "%s" mem_gc;
  Printf.printf "%s\n" prev_conv;
  Printf.printf "    %s%s(%s);\n" func_ret func_name args_call;
  Printf.printf "    %s\n}\n" ret_to_ml;
  if len > 5 then
    print_bc_func func_name len;
  print_newline();
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


let () =
  try
    while true do
      let line = input_line stdin in
      if String.length line <> 0 then
      if line.[0] <> '/' then
      begin
        let args = get_args line in
        let argv = Str.split (Str.regexp "[,]") args in
        let argv_s = List.map parse_arg argv in
        let argv_t = List.map prepare_arg argv_s in
        let func = get_func_name line in
        if there_is_arg "--gen-c" then
          print_c_func func argv_t;
        if there_is_arg "--gen-ml" then
          print_ml_func func argv_t;
      end
    done
  with
    End_of_file -> ()
;;

(* vim: sw=2 sts=2 ts=2 et fdm=marker
*)
