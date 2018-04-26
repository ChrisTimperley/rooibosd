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

let location_to_json loc : Ezjsonm.value =
  loc |> Location.Range.to_string |> Ezjsonm.string

let bound_term_to_json var term : Ezjsonm.value =
  let var_name, _ = var in
  let open Ezjsonm in
  [
    ("term", (string var_name));
    ("content", (string "CONTENT"));
    ("location", (term |> Term.range |> location_to_json))
  ] |> dict

let environment_to_json env : Ezjsonm.value =
  let open Ezjsonm in
  (Environment.vars env) |>
  list (fun v -> bound_term_to_json v (Environment.lookup env v)) |>
  unwrap

let match_to_json (m : Match.t) : Ezjsonm.value =
  let open Ezjsonm in
  let env = Match.environment m in
  let location = Match.range m in
  let properties =
    [("location", (location_to_json mock_location));
     ("environment", (environment_to_json env))]
  in
    dict properties

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

let matches =
  App.post "/matches" (fun request ->
      let open Ezjsonm in
      request |> App.json_of_body_exn >>= (fun jsn ->
        let source = find (value jsn) ["source"] |> get_string in
        let template = find (value jsn) ["template"] |> get_string in
        Lwt_log.ign_log_f ~level:Info "POST /matches";
        Lwt_log.ign_log_f ~level:Info "Source: %s" source;
        Lwt_log.ign_log_f ~level:Info "Template: %s" template;
        Lwt_log.ign_log_f ~level:Info "Source (Term): %s" (source |> to_term |> Term.to_string);
        Lwt_log.ign_log_f ~level:Info "Template (Term): %s" (source |> to_term |> Term.to_string);

        (* find all of the matches *)
        let matches =
          Match.all (to_term template) (to_term source) |>
          Sequence.to_list
        in
        let jsn_reply : Ezjsonm.t =
          list (fun m -> match_to_json m) matches
        in
        `Json jsn_reply |> respond'))

let _ =
  Lwt_log_core.Section.set_level Lwt_log_core.Section.main Debug;
  App.empty
  |> substitute
  |> matches
  |> App.run_command
