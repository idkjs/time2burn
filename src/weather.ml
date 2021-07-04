open! Core_kernel
open Lwt.Syntax
open Bootstrap.Basic

module DT = struct
  type t = Time.t [@@deriving equal]

  let of_yojson : Yojson.Safe.t -> (t, string) result = function
  | `Int x -> x |> Int.to_float |> Time.Span.of_sec |> Time.of_span_since_epoch |> Result.return
  | `Float x -> x |> Time.Span.of_sec |> Time.of_span_since_epoch |> Result.return
  | json -> Error (sprintf !"Invalid JSON for DT: '%{Yojson.Safe}" json)

  let sexp_of_t x = Sexp.Atom (Time.to_string_iso8601_basic ~zone:Time.Zone.utc x)

  let t_of_sexp = function
  | Sexp.Atom x -> Time.of_string x
  | sexp -> failwithf !"Invalid sexp for DT: '%{Sexp}'" sexp ()

  let to_string dt =
    let parts = Time.to_ofday ~zone:browser_timezone dt |> Time.Ofday.to_parts in
    let hour, ampm =
      match parts.hr with
      | 0 -> 12, "am"
      | h when h < 12 -> h, "am"
      | 12 -> 12, "pm"
      | h -> h - 12, "pm"
    in
    sprintf "%d:%02d%s" hour parts.min ampm
end

type overview = {
  id: int;
  main: string;
  description: string;
  icon: string;
}
[@@deriving sexp, equal, of_yojson { strict = false }]

type current = {
  dt: DT.t;
  temp: float;
  feels_like: float;
  pressure: int;
  humidity: int;
  uvi: float;
  clouds: int;
  wind_speed: float;
  wind_gust: float;
  weather: overview list;
}
[@@deriving sexp, equal, of_yojson { strict = false }]

type hourly = {
  dt: DT.t;
  temp: float;
  feels_like: float;
  pressure: int;
  humidity: int;
  uvi: float;
  clouds: int;
  wind_speed: float;
  wind_gust: float;
  weather: overview list;
  pop: float;
}
[@@deriving sexp, equal, of_yojson { strict = false }]

type t = {
  current: current;
  hourly: hourly list;
}
[@@deriving sexp, equal, of_yojson { strict = false }]

let openweather_api_key = "7d892e70cd3986f7e4bd3f32c58dc8a6"

let weather_uri = Uri.of_string "https://api.openweathermap.org/data/2.5/onecall"

open Cohttp
open Cohttp_lwt_xhr
module Body = Cohttp_lwt.Body

let get_weather ~latitude ~longitude =
  Lwt.catch
    (fun () ->
      let uri =
        Uri.with_query weather_uri
          [
            "lat", [ Float.to_string latitude ];
            "lon", [ Float.to_string longitude ];
            "exclude", [ "minutely"; "daily"; "alerts" ];
            "lang", [ "en_us" ];
            "units", [ "imperial" ];
            "appid", [ openweather_api_key ];
          ]
      in
      let* res, body = Client.get uri in
      let+ raw = Body.to_string body in
      match Response.status res |> Code.code_of_status with
      | 200 -> Yojson.Safe.from_string raw |> [%of_yojson: t] |> Result.map_error ~f:Error.of_string
      | code ->
        print_endline raw;
        failwithf "Could not get weather data. Error %d" code ())
    (fun exn -> Or_error.of_exn exn |> Lwt.return)

let get_weather2 ~latitude:_ ~longitude:_ =
  {|{"lat":36.1575,"lon":-95.9921,"timezone":"America/Chicago","timezone_offset":-18000,"current":{"dt":1625414356,"sunrise":1625397112,"sunset":1625449479,"temp":81.55,"feels_like":83.97,"pressure":1015,"humidity":61,"dew_point":66.81,"uvi":6.41,"clouds":1,"visibility":10000,"wind_speed":1.99,"wind_deg":145,"wind_gust":5.01,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}]},"hourly":[{"dt":1625410800,"temp":80.58,"feels_like":82.92,"pressure":1015,"humidity":63,"dew_point":66.83,"uvi":4.46,"clouds":13,"visibility":10000,"wind_speed":5.46,"wind_deg":196,"wind_gust":6.58,"weather":[{"id":801,"main":"Clouds","description":"few clouds","icon":"02d"}],"pop":0},{"dt":1625414400,"temp":81.55,"feels_like":83.97,"pressure":1015,"humidity":61,"dew_point":66.81,"uvi":6.41,"clouds":1,"visibility":10000,"wind_speed":5.5,"wind_deg":193,"wind_gust":5.93,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625418000,"temp":81.52,"feels_like":83.75,"pressure":1015,"humidity":60,"dew_point":66.29,"uvi":8.38,"clouds":10,"visibility":10000,"wind_speed":5.66,"wind_deg":180,"wind_gust":6.17,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625421600,"temp":82,"feels_like":84.25,"pressure":1015,"humidity":59,"dew_point":66.27,"uvi":9.48,"clouds":22,"visibility":10000,"wind_speed":6.24,"wind_deg":166,"wind_gust":6.55,"weather":[{"id":801,"main":"Clouds","description":"few clouds","icon":"02d"}],"pop":0},{"dt":1625425200,"temp":82.74,"feels_like":84.9,"pressure":1014,"humidity":57,"dew_point":65.97,"uvi":9.31,"clouds":8,"visibility":10000,"wind_speed":6.8,"wind_deg":158,"wind_gust":6.31,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625428800,"temp":83.53,"feels_like":85.59,"pressure":1014,"humidity":55,"dew_point":65.68,"uvi":8.02,"clouds":19,"visibility":10000,"wind_speed":6.78,"wind_deg":154,"wind_gust":5.7,"weather":[{"id":801,"main":"Clouds","description":"few clouds","icon":"02d"}],"pop":0},{"dt":1625432400,"temp":84.27,"feels_like":86.41,"pressure":1014,"humidity":54,"dew_point":65.62,"uvi":5.93,"clouds":35,"visibility":10000,"wind_speed":6.44,"wind_deg":149,"wind_gust":5.19,"weather":[{"id":802,"main":"Clouds","description":"scattered clouds","icon":"03d"}],"pop":0},{"dt":1625436000,"temp":83.89,"feels_like":85.87,"pressure":1013,"humidity":54,"dew_point":65.7,"uvi":3.79,"clouds":45,"visibility":10000,"wind_speed":6.29,"wind_deg":144,"wind_gust":4.99,"weather":[{"id":802,"main":"Clouds","description":"scattered clouds","icon":"03d"}],"pop":0},{"dt":1625439600,"temp":83.1,"feels_like":85.62,"pressure":1013,"humidity":58,"dew_point":66.7,"uvi":1.85,"clouds":47,"visibility":10000,"wind_speed":5.91,"wind_deg":142,"wind_gust":5.08,"weather":[{"id":802,"main":"Clouds","description":"scattered clouds","icon":"03d"}],"pop":0},{"dt":1625443200,"temp":81.73,"feels_like":84.78,"pressure":1013,"humidity":64,"dew_point":68.38,"uvi":0.65,"clouds":41,"visibility":10000,"wind_speed":5.06,"wind_deg":145,"wind_gust":5.97,"weather":[{"id":802,"main":"Clouds","description":"scattered clouds","icon":"03d"}],"pop":0},{"dt":1625446800,"temp":77.92,"feels_like":78.69,"pressure":1013,"humidity":70,"dew_point":67.37,"uvi":0.13,"clouds":0,"visibility":10000,"wind_speed":4.74,"wind_deg":143,"wind_gust":5.93,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625450400,"temp":74.48,"feels_like":74.97,"pressure":1013,"humidity":71,"dew_point":64.49,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":3.65,"wind_deg":149,"wind_gust":3.78,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625454000,"temp":73.35,"feels_like":73.76,"pressure":1013,"humidity":72,"dew_point":63.79,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":3.09,"wind_deg":146,"wind_gust":3.27,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625457600,"temp":72.12,"feels_like":72.52,"pressure":1013,"humidity":74,"dew_point":63.19,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":4.45,"wind_deg":141,"wind_gust":4.68,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625461200,"temp":71.04,"feels_like":71.28,"pressure":1014,"humidity":73,"dew_point":62.08,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":3.91,"wind_deg":154,"wind_gust":4.14,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625464800,"temp":69.98,"feels_like":70.07,"pressure":1014,"humidity":72,"dew_point":60.55,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":5.01,"wind_deg":145,"wind_gust":5.5,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625468400,"temp":68.47,"feels_like":68.4,"pressure":1014,"humidity":72,"dew_point":59.04,"uvi":0,"clouds":4,"visibility":10000,"wind_speed":4.92,"wind_deg":167,"wind_gust":6.35,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625472000,"temp":67.3,"feels_like":67.01,"pressure":1014,"humidity":70,"dew_point":57.38,"uvi":0,"clouds":5,"visibility":10000,"wind_speed":4.7,"wind_deg":143,"wind_gust":5.86,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625475600,"temp":66.31,"feels_like":65.98,"pressure":1014,"humidity":71,"dew_point":56.43,"uvi":0,"clouds":4,"visibility":10000,"wind_speed":3.96,"wind_deg":139,"wind_gust":4.16,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625479200,"temp":65.52,"feels_like":65.1,"pressure":1014,"humidity":71,"dew_point":55.8,"uvi":0,"clouds":6,"visibility":10000,"wind_speed":3.96,"wind_deg":129,"wind_gust":3.98,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625482800,"temp":64.76,"feels_like":64.33,"pressure":1014,"humidity":72,"dew_point":55.62,"uvi":0,"clouds":6,"visibility":10000,"wind_speed":3.69,"wind_deg":128,"wind_gust":3.65,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625486400,"temp":65.01,"feels_like":64.78,"pressure":1015,"humidity":76,"dew_point":57.36,"uvi":0.2,"clouds":5,"visibility":10000,"wind_speed":4.38,"wind_deg":125,"wind_gust":4.92,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625490000,"temp":68.16,"feels_like":68.2,"pressure":1015,"humidity":75,"dew_point":59.86,"uvi":0.85,"clouds":0,"visibility":10000,"wind_speed":4.27,"wind_deg":141,"wind_gust":6.62,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625493600,"temp":71.85,"feels_like":72.12,"pressure":1016,"humidity":72,"dew_point":62.1,"uvi":2.26,"clouds":0,"visibility":10000,"wind_speed":5.26,"wind_deg":147,"wind_gust":7.43,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625497200,"temp":75.47,"feels_like":75.92,"pressure":1016,"humidity":68,"dew_point":64.33,"uvi":4.36,"clouds":1,"visibility":10000,"wind_speed":5.57,"wind_deg":153,"wind_gust":7.72,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625500800,"temp":78.64,"feels_like":79.12,"pressure":1016,"humidity":62,"dew_point":64.27,"uvi":6.78,"clouds":1,"visibility":10000,"wind_speed":6.22,"wind_deg":160,"wind_gust":8.14,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625504400,"temp":80.98,"feels_like":82.35,"pressure":1016,"humidity":55,"dew_point":63.39,"uvi":8.86,"clouds":1,"visibility":10000,"wind_speed":6.73,"wind_deg":158,"wind_gust":7.76,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625508000,"temp":82.56,"feels_like":83.73,"pressure":1016,"humidity":52,"dew_point":63.21,"uvi":10.03,"clouds":1,"visibility":10000,"wind_speed":7.07,"wind_deg":152,"wind_gust":7.72,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625511600,"temp":83.59,"feels_like":84.61,"pressure":1015,"humidity":50,"dew_point":63.19,"uvi":9.86,"clouds":0,"visibility":10000,"wind_speed":6.91,"wind_deg":145,"wind_gust":7.34,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625515200,"temp":84.18,"feels_like":85.15,"pressure":1015,"humidity":49,"dew_point":63.14,"uvi":8.5,"clouds":0,"visibility":10000,"wind_speed":7.05,"wind_deg":138,"wind_gust":7.43,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625518800,"temp":84.49,"feels_like":85.53,"pressure":1014,"humidity":49,"dew_point":63.12,"uvi":6.29,"clouds":0,"visibility":10000,"wind_speed":6.91,"wind_deg":137,"wind_gust":7.7,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625522400,"temp":84.27,"feels_like":85.26,"pressure":1014,"humidity":49,"dew_point":63.34,"uvi":3.9,"clouds":0,"visibility":10000,"wind_speed":6.38,"wind_deg":135,"wind_gust":7.54,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625526000,"temp":83.79,"feels_like":85.5,"pressure":1014,"humidity":53,"dew_point":64.67,"uvi":1.9,"clouds":0,"visibility":10000,"wind_speed":5.53,"wind_deg":135,"wind_gust":7.2,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625529600,"temp":82.45,"feels_like":85.08,"pressure":1013,"humidity":60,"dew_point":67.37,"uvi":0.67,"clouds":0,"visibility":10000,"wind_speed":4.25,"wind_deg":125,"wind_gust":6.49,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625533200,"temp":78.73,"feels_like":78.73,"pressure":1013,"humidity":66,"dew_point":66.29,"uvi":0.13,"clouds":0,"visibility":10000,"wind_speed":3.62,"wind_deg":108,"wind_gust":3.89,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625536800,"temp":75.31,"feels_like":75.63,"pressure":1014,"humidity":66,"dew_point":63.28,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":2.93,"wind_deg":85,"wind_gust":3.2,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625540400,"temp":73.98,"feels_like":74.26,"pressure":1014,"humidity":68,"dew_point":62.83,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":3.58,"wind_deg":66,"wind_gust":3.65,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625544000,"temp":72.7,"feels_like":73,"pressure":1015,"humidity":71,"dew_point":62.71,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":3.36,"wind_deg":63,"wind_gust":3.33,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625547600,"temp":71.62,"feels_like":71.96,"pressure":1015,"humidity":74,"dew_point":62.82,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":2.71,"wind_deg":95,"wind_gust":3.04,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625551200,"temp":70.65,"feels_like":71.02,"pressure":1015,"humidity":77,"dew_point":62.82,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":2.28,"wind_deg":128,"wind_gust":2.53,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625554800,"temp":70.05,"feels_like":70.43,"pressure":1015,"humidity":78,"dew_point":62.83,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":0.81,"wind_deg":155,"wind_gust":1.03,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625558400,"temp":69.82,"feels_like":70.16,"pressure":1015,"humidity":78,"dew_point":62.64,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":2.46,"wind_deg":15,"wind_gust":2.51,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625562000,"temp":68.88,"feels_like":69.22,"pressure":1015,"humidity":80,"dew_point":62.42,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":3.65,"wind_deg":40,"wind_gust":3.87,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625565600,"temp":68.13,"feels_like":68.49,"pressure":1015,"humidity":82,"dew_point":62.49,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":3.11,"wind_deg":71,"wind_gust":3.2,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625569200,"temp":67.68,"feels_like":68.09,"pressure":1015,"humidity":84,"dew_point":62.62,"uvi":0,"clouds":0,"visibility":10000,"wind_speed":3.18,"wind_deg":81,"wind_gust":3.11,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01n"}],"pop":0},{"dt":1625572800,"temp":68.74,"feels_like":69.26,"pressure":1016,"humidity":84,"dew_point":63.61,"uvi":0.2,"clouds":0,"visibility":10000,"wind_speed":3.18,"wind_deg":85,"wind_gust":3.24,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625576400,"temp":72.21,"feels_like":72.81,"pressure":1016,"humidity":78,"dew_point":64.94,"uvi":0.84,"clouds":0,"visibility":10000,"wind_speed":3.87,"wind_deg":102,"wind_gust":5.91,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0},{"dt":1625580000,"temp":75.52,"feels_like":76.15,"pressure":1016,"humidity":72,"dew_point":65.71,"uvi":2.25,"clouds":0,"visibility":10000,"wind_speed":4.63,"wind_deg":123,"wind_gust":6.42,"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"pop":0}]}|}
  |> Yojson.Safe.from_string
  |> [%of_yojson: t]
  |> Result.map_error ~f:Error.of_string
  |> Lwt_result.lift
