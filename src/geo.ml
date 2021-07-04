open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open! Bootstrap
open! Bootstrap.Basic
open Lwt.Syntax

module Position = struct
  type t = {
    latitude: float;
    longitude: float;
  }
  [@@deriving sexp, equal]
end

let get_location () =
  let p, w = Lwt.wait () in
  let () =
    window##.navigator##.geolocation##getCurrentPosition (fun position ->
        Or_error.try_with ~backtrace:true (fun () ->
            let () = window##.console##log position in
            let latitude = position##.coords##.latitude in
            let longitude = position##.coords##.longitude in
            Position.{ latitude; longitude })
        |> Lwt.wakeup_later w)
  in
  p

let component =
  let module Component = struct
    module Input = Unit

    module Model = struct
      type status =
        | Blank
        | Fetching_geo
        | Fetching_weather
        | Completed
      [@@deriving sexp, equal]

      type t = {
        weather: Weather.t option Or_error.t;
        status: status;
      }
      [@@deriving sexp, equal]
    end

    module Action = struct
      type t =
        | Fetching_geo
        | Fetching_weather of Position.t
        | Fetched_weather  of Weather.t
        | Errored          of Error.t
      [@@deriving sexp_of]
    end

    let name = Source_code_position.to_string [%here]

    let apply_action ~inject ~schedule_event (() : Input.t) (_prev : Model.t) : Action.t -> Model.t =
      function
    | Fetching_geo ->
      Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
          let+ result = get_location () in
          (match result with
          | Ok position -> Action.Fetching_weather position
          | Error err -> Action.Errored err)
          |> inject
          |> schedule_event);
      { weather = Ok None; status = Fetching_geo }
    | Fetching_weather { longitude; latitude } ->
      Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
          let+ result = Weather.get_weather ~longitude ~latitude in
          (match result with
          | Ok weather -> Action.Fetched_weather weather
          | Error err -> Action.Errored err)
          |> inject
          |> schedule_event);
      { weather = Ok None; status = Fetching_weather }
    | Fetched_weather weather -> { weather = Ok (Some weather); status = Completed }
    | Errored err -> { weather = Error err; status = Blank }

    let compute ~inject (() : Input.t) (model : Model.t) =
      let node =
        match model.status, model.weather with
        | _, Error err ->
          Node.div
            Attr.[ classes [ "alert"; "alert-danger" ] ]
            [ Node.textf !"%{Error.to_string_hum}" err ]
        | Blank, _ ->
          let handler _evt = inject Action.Fetching_geo in
          Node.div
            Attr.[ classes [ "btn"; "btn-primary" ]; on_click handler ]
            [ Node.text "Use my location" ]
        | Fetching_geo, _ ->
          Node.div []
            [
              Node.div
                Attr.[ classes [ "spinner-border"; "text-info" ]; create "role" "status" ]
                [ Node.span Attr.[ class_ "visually-hidden" ] [ Node.text "Loading..." ] ];
            ]
        | Fetching_weather, _ ->
          Node.div []
            [
              Node.div
                Attr.[ classes [ "spinner-border"; "text-success" ]; create "role" "status" ]
                [ Node.span Attr.[ class_ "visually-hidden" ] [ Node.text "Loading..." ] ];
            ]
        | Completed, _ -> Node.div [] [ Icon.svg Check_lg ~container:Span Attr.[ class_ "text-success" ] ]
      in
      let data =
        match model.weather with
        | Error _
         |Ok None ->
          None
        | Ok (Some x) -> Some x
      in
      data, node

    module Result = struct
      type t = Weather.t option * Node.t
    end
  end in
  Bonsai.of_module0 (module Component) ~default_model:{ weather = Ok None; status = Blank }
