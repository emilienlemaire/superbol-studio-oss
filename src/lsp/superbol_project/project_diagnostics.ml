(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

type error =
  | Invalid_config of
      {
        loc: location;
        msg: string;
    }
  | Unknown_dialect of string

and location =
  | Toml_loc of Toml.Parser.location
  | Toml_file of string

let pp_error ppf = function
  | Invalid_config { loc = Toml_loc { source; line; column; _ }; msg = _ } ->
      (* Here we ignore the message as most error messages from `toml` just
         repeat source location information. *)
      Pretty.print ppf "%s:%d,%d: Syntax error" source line column
  | Invalid_config { loc = Toml_file filename; msg } ->
      Pretty.print ppf "%s: %s" filename msg
  | Unknown_dialect name ->
      Pretty.print ppf "Unknown@ dialect: `%s'" name
