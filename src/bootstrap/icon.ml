open! Core_kernel
open! Bonsai_web
open! Vdom

type t =
  | Check_lg
  | X
[@@deriving sexp]

let get_fill = function
| Check_lg
 |X ->
  "currentColor"

let get_viewbox = function
| Check_lg
 |X ->
  0, 0, 16, 16

type container =
  | Div
  | Span

let svg ?(width = 1.0) ?(height = 1.0) ?(bold = false) ?fill ?(container = Div) ?(raw_extra_classes = [])
   icon attrs =
  let fill =
    Option.value fill ~default:(get_fill icon) |> fun fill -> Yojson.Basic.to_string (`String fill)
  in
  let v1, v2, v3, v4 = get_viewbox icon in
  let paths =
    match icon with
    | Check_lg ->
      {svg|  <path d="M13.485 1.431a1.473 1.473 0 0 1 2.104 2.062l-7.84 9.801a1.473 1.473 0 0 1-2.12.04L.431 8.138a1.473 1.473 0 0 1 2.084-2.083l4.111 4.112 6.82-8.69a.486.486 0 0 1 .04-.045z"/>|svg}
    | X ->
      {svg|<path d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"/>|svg}
  in
  let html =
    sprintf
      {svg|<svg width="%frem" height="%frem" viewBox="%d %d %d %d" class="%s" %s fill=%s xmlns="http://www.w3.org/2000/svg">
%s
</svg>
|svg}
      width height v1 v2 v3 v4
      (String.concat ~sep:" " raw_extra_classes)
      (if bold then sprintf {|stroke=%s stroke-width="0.75"|} fill else "")
      fill paths
  in
  match container with
  | Div ->
    Node.inner_html ~tag:"div" ~this_html_is_sanitized_and_is_totally_safe_trust_me:html
      ([ attrs; Attr.[ classes [ "v-center"; "align-items-center" ] ] ]
      |> List.concat_no_order
      |> Attrs.merge_classes_and_styles
      )
  | Span -> Node.inner_html ~tag:"span" ~this_html_is_sanitized_and_is_totally_safe_trust_me:html attrs
