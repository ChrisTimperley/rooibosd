open Core_kernel
open Format
open Lwt
open Opium.Std

open Rooibos

let (^/) = Filename.concat
let (>>>=) = Result.Monad_infix.(>>=)

let bad_request message =
  Lwt_log.ign_log_f ~level:Info "%s" message;
  respond'
    (* Cohttp.code.mli *)
    ~code:`Bad_request
    (`String message)

(* Unfortunate, must convert Or_error to Result.t *)
let to_result = function
  | Ok x -> Result.Ok x
  | Error e -> Result.Error (Error.to_string_hum e)

let to_term s =
  let lexbuf = Lexing.from_string s in
  try Parser.main Lexer.read lexbuf with
  | Parser.Error ->
    failwith (Format.asprintf "Syntax error in %s\n" s)

let rewrite =
  App.get "/rewrite/:source/:match_template/:rewrite_template" (fun request ->
      let source = "source" |> param request in
      let match_template = "match_template" |> param request in
      let rewrite_template = "rewrite_template" |> param request in
      Lwt_log.ign_log_f ~level:Info "Rewrite GET request.";
      Lwt_log.ign_log_f ~level:Info "Source  : %s" source;
      Lwt_log.ign_log_f ~level:Info "Matcher : %s" match_template;
      Lwt_log.ign_log_f ~level:Info "Rewrite : %s" rewrite_template;
      match
        Match.all (to_term match_template) (to_term source) |>
        Sequence.to_list
      with
      | env::_ ->
        let rewritten =
          Environment.substitute env (to_term rewrite_template)
          |> Printer.to_string
        in
        Lwt_log.ign_log_f ~level:Info "Rewrite succeeded.";
        respond' ~code:`OK (`String rewritten)
      | _ ->
        Lwt_log.ign_log_f ~level:Info "Rewrite failed.";
        respond' ~code:`Unprocessable_entity (`String "No match"))

let _ =
  Lwt_log_core.Section.set_level Lwt_log_core.Section.main Debug;
  App.empty
  |> rewrite
  |> App.run_command
