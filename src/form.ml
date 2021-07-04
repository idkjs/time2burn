open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open! Bootstrap
open! Bootstrap.Basic

type data = {
  weather: Weather.t;
  skin_type: Skin_type.Fitzpatrick.t;
  spf: Spf.Levels.t;
}
[@@deriving sexp]

type subforms = {
  geo_node: Node.t;
  skin_type_node: Node.t;
  spf_node: Node.t;
  data: data option;
}

let subforms =
  let%sub geo = Geo.component in
  let%sub skin_type = Skin_type.component in
  let%sub spf = Spf.component in
  return
  @@ let%map geo_data, geo_node = geo
     and skin_type_data, skin_type_node = skin_type
     and spf_data, spf_node = spf in
     let data =
       match geo_data, skin_type_data, spf_data with
       | Some weather, Some skin_type, Some spf -> Some { weather; skin_type; spf }
       | _ -> None
     in
     { geo_node; skin_type_node; spf_node; data }

type slice = {
  dt: Weather.DT.t;
  uvi: float;
}
[@@deriving sexp, equal]

type computed = {
  slice: slice;
  cost: float;
  total_at_start: float;
}
[@@deriving sexp, equal]

let how_long_rev ~num_slices { weather; skin_type; spf } =
  let slice_minutes = 60 // num_slices in
  let ( +* ) time = function
    | 0 -> time
    | n -> Time.add time (Time.Span.of_min Float.(of_int n * slice_minutes))
  in
  let skin_type = Skin_type.Fitzpatrick.to_coeff skin_type in
  let spf = Spf.Levels.to_coeff spf in
  let slices =
    let rec loop acc : Weather.hourly list -> slice list = function
      | []
       |[ _ ] ->
        acc
      | x :: (y :: _ as rest) ->
        let open Float in
        let ll =
          List.init num_slices ~f:(fun i ->
              let gradient = of_int i * (1 // 12) in
              { dt = x.dt +* i; uvi = (x.uvi * (1.0 - gradient)) + (y.uvi * gradient) })
          |> List.rev_filter ~f:(fun { dt; _ } -> Time.(dt >= weather.current.dt))
        in
        loop (ll @ acc) rest
    in
    loop [] (List.take weather.hourly 13) |> List.rev
  in
  let going_outside = List.hd slices |> Option.map ~f:(fun { dt; _ } -> dt) in
  let num_points, points =
    List.fold_until slices ~init:(0.0, 0, [])
      ~finish:(fun (_, n, ll) -> n, ll)
      ~f:(fun (total, n, ll) slice ->
        let open Float in
        let would_be_100 = skin_type * 8.0 / max 1.0 slice.uvi * spf in
        let cost = slice_minutes * 100.0 / would_be_100 in
        let next = { slice; cost; total_at_start = total } in
        if total >= 100.0 then Stop (succ n, next :: ll) else Continue (total + cost, succ n, next :: ll))
  in
  going_outside, num_points, points

let best_fit_how_long_rev data =
  let rec loop = function
    | [ num_slices ] ->
      let going_outside, _, points = how_long_rev ~num_slices data in
      going_outside, points
    | num_slices :: rest ->
      let going_outside, num_points, points = how_long_rev ~num_slices data in
      if num_points > 10 then going_outside, points else loop rest
    | [] -> failwith "Impossible case, there should always be hourly values"
  in
  loop [ 2; 4; 6; 12 ]

let component =
  let%sub subforms = subforms in
  let module Component = struct
    module Input = struct
      type t = subforms
    end

    module Model = struct
      type t = unit [@@deriving sexp, equal]
    end

    module Action = Unit

    let name = Source_code_position.to_string [%here]

    let apply_action ~inject:_ ~schedule_event:_ (_subforms : Input.t) (prev : Model.t) _action = prev

    let compute ~inject:_ ({ geo_node; skin_type_node; spf_node; data } : Input.t) (_model : Model.t) =
      let make_section title nodes =
        Node.div Attr.[ classes [ "my-4" ] ] (Node.h5 Attr.[ class_ "pb-1" ] [ Node.text title ] :: nodes)
      in
      let chart =
        Option.value_map data ~default:Node.none ~f:(fun subform_data ->
            let going_outside, points = best_fit_how_long_rev subform_data in
            let ttb =
              match going_outside, points with
              | Some _, { total_at_start; _ } :: _ when Float.( < ) total_at_start 100.0 ->
                Node.div Attr.[ classes [ "alert"; "alert-success" ] ] [ Node.text "You should be fine!" ]
              | Some start, _ :: { slice = { dt = burn; _ }; _ } :: _ ->
                Node.div
                  Attr.[ classes [ "alert"; "alert-warning" ] ]
                  [
                    Node.textf
                      !"If you go outside at %{Weather.DT}, you will have a sunburn around %{Weather.DT}"
                      start burn;
                  ]
              | _ -> Node.none
            in
            let labels, data =
              List.fold points ~init:([], []) ~f:(fun (labels, pct) computed ->
                  Weather.DT.to_string computed.slice.dt :: labels, computed.total_at_start :: pct)
            in
            Node.div
              Attr.[ classes [ "d-inline-flex"; "flex-column" ] ]
              [ ttb; Chart.render ~labels ~data ~key:(sprintf !"%{sexp: data}" subform_data) ])
      in
      Node.div []
        [
          make_section "Your sensitivity to UV" [ skin_type_node ];
          make_section "Sunscreen" [ spf_node ];
          make_section "Your location" [ geo_node ];
          chart;
        ]

    module Result = Node
  end in
  Bonsai.of_module1 (module Component) ~default_model:() subforms
