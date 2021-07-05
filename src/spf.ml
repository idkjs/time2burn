open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open! Bootstrap
open! Bootstrap.Basic

module Levels = struct
  type t =
    | SPF_0
    | SPF_15
    | SPF_30
    | SPF_50
    | SPF_60
    | SPF_80
    | SPF_100
  [@@deriving sexp, equal, enumerate]

  let to_string = function
  | SPF_0 -> "None"
  | SPF_15 -> "SPF 15"
  | SPF_30 -> "SPF 30"
  | SPF_50 -> "SPF 50"
  | SPF_60 -> "SPF 60"
  | SPF_80 -> "SPF 80"
  | SPF_100 -> "SPF 100"

  let to_coeff = function
  | SPF_0 -> 1.0
  | SPF_15 -> 15.0
  | SPF_30 -> 30.0
  | SPF_50 -> 50.0
  | SPF_60 -> 60.0
  | SPF_80 -> 80.0
  | SPF_100 -> 100.0
end

let storage_key = "spf"

let component =
  let default_model = Local_storage.parse_item storage_key [%of_sexp: Levels.t option] |> Option.join in
  let%sub component = Bonsai.state_opt [%here] ?default_model (module Levels) in
  return
  @@ let%map state, update = component in
     let node =
       let options =
         let make x =
           let attrs =
             Attr.[ value (sprintf !"%{sexp: Levels.t option}" x) ]
             |> add_if ([%equal: Levels.t option] x state) Attr.selected
             |> add_if (Option.is_none x) Attr.disabled
           in
           let text = Option.value_map x ~default:"Select one" ~f:(sprintf !"%{Levels}") in
           Node.option attrs [ Node.text text ]
         in
         make None :: List.map Levels.all ~f:(fun x -> make (Some x))
       in
       let handler _evt s =
         Local_storage.set_item ~key:storage_key ~data:s
         |> Result.iter_error ~f:(fun err ->
                print_endline (sprintf "Could not store to local storage '%s'. Error: '%s'" s err));
         Sexp.of_string_conv_exn s [%of_sexp: Levels.t option] |> update
       in
       let icon =
         if Option.is_some state
         then Icon.svg Check_lg ~container:Span Attr.[ class_ "text-success" ]
         else Icon.svg X ~container:Span Attr.[ class_ "text-danger" ]
       in
       Node.div []
         [
           Node.select
             Attr.
               [
                 classes [ "form-select"; "d-inline"; "me-2" ];
                 style Css_gen.(width (`Em 20));
                 create "aria-label" "Levels skin scale";
                 on_change handler;
               ]
             options;
           icon;
         ]
     in
     state, node
