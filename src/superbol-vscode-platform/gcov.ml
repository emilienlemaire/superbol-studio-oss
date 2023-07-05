(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Interop
open Promise.Syntax

module LineData = struct
  include Interface.Make ()

  include
    [%js:
      val count:            t -> int [@@js.get]
      val function_name:    t -> string [@@js.get]
      val line_number:      t -> string [@@js.get]
      val unexecuted_block: t -> bool [@@js.get]]
end

module FunctionData = struct
  include Interface.Make ()

  include
    [%js:
      val blocks:          t -> int [@@js.get]
      val blocks_executed: t -> int [@@js.get]
      val demangled_name:  t -> string [@@js.get]
      val start_column:    t -> int [@@js.get]
      val start_line:      t -> int [@@js.get]
      val end_column:      t -> int [@@js.get]
      val end_line:        t -> int [@@js.get]
      val execution_count: t -> int [@@js.get]
      val name:            t -> string [@@js.get]]
end

module FileData = struct
  include Interface.Make ()

  include
    [%js:
      val file:      t -> string [@@js.get]
      val lines:     t -> LineData.t list [@@js.get]
      val functions: t -> FunctionData.t list [@@js.get]]
end

module GCovData = struct
  include Interface.Make ()

  include
    [%js:
      val files:                     t -> FileData.t list [@@js.get]
      val current_working_directory: t -> string [@@js.get]
      val data_file:                 t -> string [@@js.get]]
end

(*TODO: Make this a setting. *)
let get_gcov_binary () =
  "gcov"

let is_gcov_compatible () =
  let bin = get_gcov_binary () in
  let command = bin ^ " --help" in
  Node.ChildProcess.exec command
  >>= (fun {exitCode; stdout; stderr} ->
      if exitCode <> 0 then
        Vscode.Window.showErrorMessage ()
          ~message: ("Could not run gcov, error message: "^stderr)
      else
        let re_out = Str.regexp_string "--stdout" in
        let re_json = Str.regexp_string "--json-format" in
        if try
            ignore (Str.search_forward re_out stdout 0);
            ignore (Str.search_forward re_json stdout 0); true
          with _ -> false then
          Vscode.Window.showErrorMessage ()
            ~message: ("Gcov version is incompatible, please use at least version 9.")
        else
          Promise.resolve (Some true))

let load_gcov_data paths =
  match paths with
  | [] -> Promise.return []
  | _ ->
    let files = String.concat " " paths in
    let command = (get_gcov_binary ()) ^ " --stdout --json-format " ^ files in
    Node.ChildProcess.exec command
    >>= (fun {exitCode; stdout; stderr} ->
        if exitCode <> 0 then
          begin
            Printf.eprintf "Exec error: %s" stderr;
            Promise.reject []
          end
        else
          let parts = String.split_on_char '\n' stdout in
          Promise.resolve
          @@ List.fold_left (fun acc part ->
              match part with
              | "" -> acc
              | _ ->
                match Jsonoo.try_parse_opt part with
                | Some json -> GCovData.t_of_js (Jsonoo.t_to_js json) :: acc
                | None -> acc)
            []
            parts)
