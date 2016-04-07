(******************************************************************************)
(* Copyright (c) 2016 DooMeeR                                                 *)
(*                                                                            *)
(* Permission is hereby granted, free of charge, to any person obtaining      *)
(* a copy of this software and associated documentation files (the            *)
(* "Software"), to deal in the Software without restriction, including        *)
(* without limitation the rights to use, copy, modify, merge, publish,        *)
(* distribute, sublicense, and/or sell copies of the Software, and to         *)
(* permit persons to whom the Software is furnished to do so, subject to      *)
(* the following conditions:                                                  *)
(*                                                                            *)
(* The above copyright notice and this permission notice shall be             *)
(* included in all copies or substantial portions of the Software.            *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,            *)
(* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF         *)
(* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                      *)
(* NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE     *)
(* LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION     *)
(* OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION      *)
(* WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.            *)
(******************************************************************************)

type t = Dom.node Js.t

let alert x =
  Dom_html.window##alert(Js.string x)

let text value =
  let text = Dom_html.document##createTextNode(Js.string value) in
  (text :> t)

let text' value =
  let text = Dom_html.document##createTextNode(Js.string value) in
  let set_text value = text##replaceData(0, text##length, Js.string value) in
  (text :> t), set_text

let img ?(class_ = "") ?alt src =
  let alt = match alt with None -> src | Some alt -> alt in
  let img = Dom_html.(createImg document) in
  img##src <- Js.string src;
  img##alt <- Js.string alt;
  img##className <- Js.string class_;
  (img :> t)

let a ?(class_ = "") ?(href = "") items =
  let a = Dom_html.(createA document) in
  let append_node node =
    let _: Dom.node Js.t = a##appendChild(node) in
    ()
  in
  List.iter append_node items;
  a##className <- Js.string class_;
  a##href <- Js.string href;
  (a :> t)

let append_node parent node =
  let _: Dom.node Js.t = parent##appendChild(node) in
  ()

let set_items parent (items: t list) =
  List.iter
    (fun child -> let _: Dom.node Js.t = parent##removeChild(child) in ())
    (Dom.list_of_nodeList parent##childNodes);
  List.iter (append_node parent) items

let p' ?(class_ = "") items =
  let p = Dom_html.(createP document) in
  List.iter (append_node p) items;
  p##className <- Js.string class_;
  (p :> t), set_items p

let p ?class_ items =
  let p, _ = p' ?class_ items in
  p

let p_text ?class_ string =
  p ?class_ [ text string ]

let div' ?(class_ = "") items =
  let div = Dom_html.(createDiv document) in
  List.iter (append_node div) items;
  div##className <- Js.string class_;
  (div :> t), set_items div

let div ?class_ items =
  let div, _ = div' ?class_ items in
  div

let span' ?(class_ = "") items =
  let span = Dom_html.(createSpan document) in
  List.iter (append_node span) items;
  span##className <- Js.string class_;
  (span :> t), set_items span

let span ?class_ items =
  let span, _ = span' ?class_ items in
  span

let checkbox_input' ?(class_ = "") ?(on_change = fun _ -> ()) checked =
  let input = Dom_html.(createInput ~_type: (Js.string "checkbox") document) in
  input##checked <- Js.bool checked;
  let on_click _ = on_change (Js.to_bool input##checked); Js._true in
  input##onclick <- Dom.handler on_click;
  input##className <- Js.string class_;
  let set_checked checked = input##checked <- Js.bool checked in
  (input :> t), set_checked

let checkbox_input ?class_ ?on_change checked =
  let checkbox_input, _ = checkbox_input' ?class_ ?on_change checked in
  checkbox_input

let text_input' ?(class_ = "") ?(on_change = fun _ -> ()) value =
  let input = Dom_html.(createInput ~_type: (Js.string "text") document) in
  input##value <- Js.string value;
  let on_input _ = on_change (Js.to_string input##value); Js._true in
  input##oninput <- Dom.handler on_input;
  input##className <- Js.string class_;
  let set_value value = input##value <- Js.string value in
  (input :> t), set_value

let text_input ?class_ ?on_change value =
  let text_input, _ = text_input' ?class_ ?on_change value in
  text_input

let run html =
  let on_load _ =
    let html = html () in
    let body =
      let find_tag name =
        let elements =
          Dom_html.window##document##getElementsByTagName(Js.string name)
        in
        let element =
          Js.Opt.get elements##item(0)
            (fun () -> failwith ("find_tag("^name^")"))
        in
        element
      in
      find_tag "body"
    in
    let _: t = body##appendChild(html) in
    Js._false
  in
  Dom_html.window##onload <- Dom.handler on_load

let get_hash () =
  let fragment = Dom_html.window##location##hash |> Js.to_string in
  if fragment = "" then
    ""
  else if fragment.[0] = '#' then
    String.sub fragment 1 (String.length fragment - 1)
  else
    fragment

let set_hash hash =
  Dom_html.window##location##hash <- Js.string hash

let on_hash_change handler =
  let handler _ = handler (); Js._true in
  Dom_html.window##onhashchange <- Dom.handler handler
