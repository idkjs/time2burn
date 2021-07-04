open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open! Bootstrap
open! Bootstrap.Basic

let application =
  print_endline "Made by SGrondin for his one true love ❤️";
  print_endline "🐫 Source code: https://github.com/SGrondin/time2burn";
  let%sub form = Form.component in
  return
  @@ let%map form = form in
     Node.div Attr.[ classes [ "container-fluid" ] ] [ Node.h3 [] [ Node.text "Time to Sunburn" ]; form ]

let generate_and_save_graph computation =
  let regex = Re.Perl.re ~opts:[ `Caseless ] "with-model-resetter_[0-9]*" |> Re.compile in
  let data =
    Bonsai.Private.to_dot computation
    |> Re.replace regex ~all:true ~f:(fun g -> sprintf {|"%s"|} (Re.Group.get g 0))
  in
  Local_storage.set_item ~key:"graph" ~data

let _app =
  (* let _result = generate_and_save_graph () in *)
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"main" application
