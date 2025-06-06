(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2025 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Cobol_common

type error = string                            (* description string, for now *)

let supplier tokens =
  let state = ref tokens in
  fun () ->
    let token, locstart, locend =
      match !state with
      | [] -> (Grammar.EOF, Lexing.dummy_pos, Lexing.dummy_pos)
      | t :: s ->
          state := s;
          t
    in
    (token, locstart, locend)

let parse text =
  let module OM = Sql_overlay_manager in
  let emit ~loc ~acc t =
    let prev_right_limit, tokens = acc in
    let lstart, lend = OM.limits loc in
    (match prev_right_limit with
     | None -> ()
     | Some l -> OM.link_limits l lstart);
    ((t, lstart, lend) :: tokens, Some lend)
  in
  let tokens =
    List.fold_left
      begin
        fun (tokens, prev_right_limit) w' ->
          match w'.payload with
          | Cobol_preproc.Text.TextWord tw ->
              let tokenizer ~loc:_ lexbuf = Lexer.token lexbuf in
              let f (t' : 'a with_loc) (tokens, prev_right_limit) =
                emit ~loc:t'.loc t'.payload ~acc:(prev_right_limit, tokens)
              in
              let tw' : string with_loc = Srcloc.locfrom tw w' in
              Tokenizing.fold_tokens ~tokenizer ~until:(( = ) Grammar.EOF) ~f tw'
                (tokens, prev_right_limit)
          | Cobol_preproc.Text.Separator s ->
              let t =
                match s with
                | ',' -> Grammar.COMMA
                | ';' -> Grammar.SEMICOLON
                | _ -> Grammar.SPECHAR s
              in
              emit ~loc:w'.loc t ~acc:(prev_right_limit, tokens)
          | pl ->
              emit ~loc:w'.loc ~acc:(prev_right_limit, tokens)
              @@ Grammar.STRING
                (Format.asprintf "%a" Cobol_preproc.Text.pp_word pl)
      end
      ([], None) text
    |> fst |> List.rev
  in
  match tokens with
  | []
  | Grammar.((EXECUTE, _, _) :: (SQL, _, _) :: (IGNORE, _, _) :: _)
  | Grammar.((EXEC, _, _) :: (SQL, _, _) :: (IGNORE, _, _) :: _) ->
      (* failsafe to avoid parsing potentially breaking on SQL IGNORE
         sections *)
      Sql_ast.Ignore []
  | (_, first_pos, _) :: _ ->
      let first_loc = Srcloc.raw (first_pos, first_pos) in
      let leftmost_limit, _ = OM.limits first_loc in
      try
        let init_checkpoint = Grammar.Incremental.main leftmost_limit in
        Grammar.MenhirInterpreter.loop (supplier tokens) init_checkpoint
      with Grammar.Error ->
        Sql_ast.Ignore []

let parse_string str =
  try
    let ast = Grammar.main Lexer.token (Lexing.from_string str) in
    (*   Format.fprintf Format.std_formatter "\n%a\n" Sql_ast.Printer.pp ast; *)
    Ok ast
  with Grammar.Error ->
    Error "syntax error"                                (* that's all for now *)
