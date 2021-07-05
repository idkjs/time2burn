open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open! Bootstrap
open! Bootstrap.Basic
open Js_of_ocaml

let id_ = "chart"

type dataset = {
  label: string option; [@default None]
  data: float list;
  border_color: string; [@key "borderColor"]
  background_color: string option; [@default None] [@key "backgroundColor"]
  fill: bool;
  cubic_interpolation_mode: string option; [@default None] [@key "cubicInterpolationMode"]
  tension: float option; [@default None]
}
[@@deriving to_yojson]

type data = {
  labels: string list;
  datasets: dataset list;
}
[@@deriving to_yojson]

module Options = struct
  type title = {
    display: bool;
    text: string option; [@default None]
  }
  [@@deriving to_yojson]

  type plugins = { legend: title } [@@deriving to_yojson] [@@unboxed]

  type interaction = {
    intersect: bool;
    mode: string;
  }
  [@@deriving to_yojson]

  type scale = { max: int } [@@deriving to_yojson] [@@unboxed]

  type scales = { y: scale } [@@deriving to_yojson] [@@unboxed]

  type t = {
    responsive: bool;
    plugins: plugins;
    interaction: interaction;
    scales: scales;
  }
  [@@deriving to_yojson]
end

type config = {
  type_: string; [@key "type"]
  data: data;
  options: Options.t;
}
[@@deriving to_yojson]

let pct_color pct (r1, g1, b1) (r2, g2, b2) =
  let open Float in
  let color pct x = x * pct * 256.0 / 100.0 in
  let r = color (100.0 - pct) r1 + color pct r2 in
  let g = color (100.0 - pct) g1 + color pct g2 in
  let b = color (100.0 - pct) b1 + color pct b2 in
  sprintf "#%02x%02x%02x" (to_int r) (to_int g) (to_int b)

let load ~labels ~data ~width:_ ~height canvas =
  let canvas = Js.Unsafe.coerce canvas in
  let ctx = canvas##getContext "2d" in
  let constructor = window##._Chart in
  let backgroud_gradient =
    let gradient = ctx##createLinearGradient 0 height 0 0 in
    let () = gradient##addColorStop 0 "#FFFFFF" in
    let () = gradient##addColorStop 0.6 "#FFF75D" in
    let () = gradient##addColorStop 0.80 "#FFC11F" in
    let () = gradient##addColorStop 0.85 "#FE650D" in
    let () = gradient##addColorStop 0.90 "#F33C04" in
    let () = gradient##addColorStop 0.95 "#DA1F05" in
    let () = gradient##addColorStop 1 "#A10100" in
    gradient
  in
  let options =
    {
      type_ = "line";
      data =
        {
          labels;
          datasets =
            [
              {
                label = None;
                data;
                border_color = "#F33C04";
                background_color = Some "#A10100";
                fill = true;
                cubic_interpolation_mode = Some "monotone";
                tension = Some 0.4;
              };
            ];
        };
      options =
        {
          responsive = false;
          plugins = { legend = { display = false; text = None } };
          interaction = { intersect = false; mode = "index" };
          scales = { y = { max = 100 } };
        };
    }
    |> [%to_yojson: config]
    |> Yojson.Safe.pretty_to_string
    |> Js.string
  in
  let parsed = Js._JSON##parse options in
  parsed##.data##.datasets##._0##.backgroundColor := backgroud_gradient;
  let _chart = new%js constructor ctx parsed in
  ()

let render ~labels ~data ~key =
  let width =
    window##.outerWidth
    |> Js.Optdef.return
    |> Js.Optdef.to_option
    |> Option.value_map ~default:650 ~f:(fun x -> min 650 (Float.to_int x - 20))
  in
  let height = width / 2 in
  Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
      Dom_html.getElementById_opt id_ |> Option.iter ~f:(load ~labels ~data ~width ~height);
      Lwt.return_unit);
  Node.div []
    [
      Node.create "canvas" ~key
        Attr.[ id id_; create "width" (Int.to_string width); create "height" (Int.to_string height) ]
        [];
    ]
