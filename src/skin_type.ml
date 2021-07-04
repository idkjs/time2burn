open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open! Bootstrap
open! Bootstrap.Basic

module Fitzpatrick = struct
  type t =
    | I
    | II
    | III
    | IV
    | V
    | VI
  [@@deriving sexp, equal, enumerate]

  let to_string = function
  | I -> "Type I"
  | II -> "Type II"
  | III -> "Type III"
  | IV -> "Type IV"
  | V -> "Type V"
  | VI -> "Type VI"

  let to_coeff = function
  | I -> 5.0
  | II -> 10.0
  | III -> 15.0
  | IV -> 25.0
  | V -> 45.0
  | VI -> 90.0
end

let storage_key = "skin-type"

let component =
  let default_model =
    Local_storage.parse_item storage_key [%of_sexp: Fitzpatrick.t option] |> Option.join
  in
  let%sub component = Bonsai.state_opt [%here] ?default_model (module Fitzpatrick) in
  return
  @@ let%map state, update = component in
     let node =
       let options =
         let make x =
           let attrs =
             Attr.[ value (sprintf !"%{sexp: Fitzpatrick.t option}" x) ]
             |> add_if ([%equal: Fitzpatrick.t option] x state) Attr.selected
             |> add_if (Option.is_none x) Attr.disabled
           in
           let text = Option.value_map x ~default:"Select one" ~f:(sprintf !"%{Fitzpatrick}") in
           Node.option attrs [ Node.text text ]
         in
         make None :: List.map Fitzpatrick.all ~f:(fun x -> make (Some x))
       in
       let handler _evt s =
         Local_storage.set_item ~key:storage_key ~data:s
         |> Result.iter_error ~f:(fun err ->
                print_endline (sprintf "Could not store to local storage '%s'. Error: '%s'" s err));
         Sexp.of_string_conv_exn s [%of_sexp: Fitzpatrick.t option] |> update
       in
       Node.div []
         [
           Node.create "img" Attr.[ src "/fitzpatrick.png"; class_ "img-fluid" ] [];
           Node.select
             Attr.
               [
                 class_ "form-select";
                 style Css_gen.(width (`Em 20));
                 create "aria-label" "Fitzpatrick skin scale";
                 on_change handler;
               ]
             options;
         ]
     in
     state, node
