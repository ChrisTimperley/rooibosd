open Core_kernel
open Format
open Lwt
open Opium.Std

open Rooibos

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

let to_var s = (s, 0)

let json_args_to_env (args: Ezjsonm.value) : Environment.t =
  let open Ezjsonm in
  let var_to_term : (Term.variable * Term.t) list =
    get_dict args |>
    List.map ~f:(fun (k, v) -> ((k |> to_var), (v |> get_string |> to_term)))
  in
  List.fold_left
    ~init:(Environment.create ())
    ~f:(fun env (var, term) -> Environment.add env var term)
    var_to_term

let substitute =
  App.post "/substitute" (fun request ->
      let open Ezjsonm in
      request |> App.json_of_body_exn >>= (fun jsn ->
        let template = find (value jsn) ["template"] |> get_string in
        let args = find (value jsn) ["arguments"] in
        let env = json_args_to_env args in
        Lwt_log.ign_log_f ~level:Info "POST /substitute";
        Lwt_log.ign_log_f ~level:Info "Arguments: %s" (args |> wrap |> to_string);
        Lwt_log.ign_log_f ~level:Info "Environment: %s" (Environment.to_string env);
        let subbed =
            Environment.substitute env (to_term template)
            |> Printer.to_string
        in
        Lwt_log.ign_log_f ~level:Info "Substitution was successful.";
        respond' ~code:`OK (`String subbed)))

let _ =
  Lwt_log_core.Section.set_level Lwt_log_core.Section.main Debug;
  App.empty
  |> substitute
  |> App.run_command
