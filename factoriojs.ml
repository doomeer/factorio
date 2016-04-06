open Factorio
open Html

type gui_ressource =
  {
    ressource: ressource;
    mutable count: string;
    mutable set_gui_global: bool -> unit;
    mutable set_gui_count: string -> unit;
  }

let ressources =
  let make_ressource ressource =
    {
      ressource;
      count = "0";
      set_gui_global = (fun _ -> ());
      set_gui_count = (fun _ -> ());
    }
  in
  List.map make_ressource ressources

let parse_float string =
  try
    float_of_string string
  with Failure _ ->
    0.

let fs x =
  let s = Printf.sprintf "%F" (ceil (x *. 1000.) /. 1000.) in
  let len = String.length s in
  if len > 0 && s.[len - 1] = '.' then
    String.sub s 0 (len - 1)
  else
    s

let ft float = text (fs float)

let special_icons =
  let list =
    [
      inserter.name, "Inserter-icon";
      low_density_structure.name, "Rocket-structure";
      electric_mining_drill.name, "Basic-mining-drill";
    ]
  in
  let table = Hashtbl.create 16 in
  List.iter (fun (name, src) -> Hashtbl.add table name src) list;
  table

let special_hrefs =
  let list =
    [
      (* No uppercase. *)
      iron_ore.name, "Iron_ore";
      copper_ore.name, "Copper_ore";
      copper_cable.name, "Copper_cable";
      plastic_bar.name, "Plastic_bar";
      fast_inserter.name, "Fast_inserter";
      smart_inserter.name, "Smart_inserter";
      basic_accumulator.name, "Basic_accumulator";
      speed_module.name, "Speed_module";
      low_density_structure.name, "Low_density_structure";
      rocket_control_unit.name, "Rocket_control_unit";
      solid_fuel.name, "Solid_fuel";
      rocket_fuel.name, "Rocket_fuel";
      rocket_part.name, "Rocket_part";
      petroleum_gas_basic.name, "Basic_oil_processing";
      assembling_machine_1.name, "Assembling_machine";
      assembling_machine_2.name, "Assembling_machine_2";
      assembling_machine_3.name, "Assembling_machine_3";
      (* Special. *)
      basic_transport_belt.name, "Transport_Belt";
    ]
  in
  let table = Hashtbl.create 16 in
  List.iter (fun (name, src) -> Hashtbl.add table name src) list;
  table

let gui_icon alt =
  let src =
    match Hashtbl.find special_icons alt with
      | src ->
          src
      | exception Not_found ->
          let src = Bytes.of_string alt in
          for i = 0 to Bytes.length src - 1 do
            let chr = Bytes.get src i in
            if chr = ' ' then
              Bytes.set src i '-'
            else if i > 0 then
              Bytes.set src i (Char.lowercase chr)
          done;
          src
  in
  let src = "http://wiki.factorio.com/images/"^src^".png" in
  let href =
    match Hashtbl.find special_hrefs alt with
      | href ->
          href
      | exception Not_found ->
          let href = Bytes.of_string alt in
          for i = 0 to Bytes.length href - 1 do
            let chr = Bytes.get href i in
            if chr = ' ' then Bytes.set href i '_'
          done;
          href
  in
  let href = "http://wiki.factorio.com/index.php?title="^href in
  a ~href [ img ~class_: "icon" ~alt src ]

let gui_goals (goals: summary list) =
  let rec gui_goal (goal: summary) =
    let makers =
      match goal.makers with
        | [] ->
            []
        | hd :: tl ->
            let gui_maker first (count, name) =
              span [
                if first then text "" else text " or ";
                gui_icon name;
                text " × ";
                ft count;
              ]
            in
            [
              span ~class_: "makerspace" [ text " " ];
              text "(";
              gui_maker true hd;
            ]
            @ List.map (gui_maker false) tl
            @ [
              text ")";
            ]
    in
    div ~class_: "goal" (
      [
        gui_icon goal.goal;
        text " × ";
        ft goal.throughput;
        text "/s";
      ]
      @ makers
      @ List.map gui_goal goal.subgoals
    )
  in
  List.map gui_goal goals

let last_hash = ref ""

let make_hash () =
  let make_ressource_hash (ressource: gui_ressource) =
    let style =
      match ressource.ressource.style with
        | Global -> "g"
        | Local -> ""
    in
    let count = parse_float ressource.count in
    let count = "-" ^ if count = 0. then "" else fs count in
    count^style
  in
  List.map make_ressource_hash ressources |> String.concat ""

let parse_hash hash =
  let len = String.length hash in
  let i = ref 0 in
  let next () =
    if !i < 0 || !i >= len then '-' else (
      let c = hash.[!i] in
      incr i;
      c
    )
  in
  let parse_ressource_hash (ressource: gui_ressource) =
    if next () = '-' then (
      let chars = ref [] in
      let global = ref false in
      let stop = ref false in
      while not !stop do
        match next () with
          | '-' ->
              (* Go back so that the next ressource can parse this dash. *)
              decr i;
              stop := true
          | 'g' ->
              global := true;
              stop := true
          | ('0' .. '9' | '.' as c) ->
              chars := c :: !chars
          | _ ->
              i := -1;
              stop := true
      done;
      if !i >= 0 then
        let count =
          String.concat "" (List.map (String.make 1) (List.rev !chars))
        in
        (* We parse the float to avoid security issues. *)
        let count = parse_float count |> fs in
        ressource.count <- count;
        ressource.set_gui_count count;
        ressource.ressource.style <- (if !global then Global else Local);
        ressource.set_gui_global !global
    ) else
      i := -1; (* next always returns '-' for the next ressources *)
    (* if !i < 0 then alert ("Failed to parse: "^ressource.ressource.name); *)
  in
  List.iter parse_ressource_hash ressources

let () =
  Html.run @@ fun () ->

  let gui, update =
    let output, set_output = Html.div' ~class_: "output" [] in
    let update () =
      let ressources =
        let get_ressource_request (ressource: gui_ressource) =
          parse_float ressource.count, ressource.ressource
        in
        List.map get_ressource_request ressources
      in
      let meta_ressource = res "" [] 0. ressources in
      let remove_zero = List.filter (fun goal -> goal.throughput <> 0.) in
      let global = summarize_global 1. meta_ressource |> remove_zero in
      let local = (summarize_local 1. meta_ressource).subgoals |> remove_zero in
      let output =
        match global, local with
          | [], [] ->
              [
                text "Set the number next to each ressource to the \
                      requested throughput. The checkbox controls whether \
                      the ressource is global.";
              ]
          | _ :: _, []
          | [], _ :: _ ->
              gui_goals (global @ local)
          | _ :: _, _ :: _ ->
              [
                div ~class_: "outputh1" [ text "Global Goals" ];
                div ~class_: "goals" (gui_goals global);
                div ~class_: "outputh1" [ text "Local Goals" ];
                div ~class_: "goals" (gui_goals local);
              ]
      in
      set_output output;
      let hash = make_hash () in
      if hash <> !last_hash then (
        last_hash := hash;
        Html.set_hash hash
      )
    in
    let ressource_input (ressource: gui_ressource) =
      let global, set_global =
        checkbox_input'
          ~on_change:
            (fun new_value ->
               let new_value = if new_value then Global else Local in
               ressource.ressource.style <- new_value; update ())
          (ressource.ressource.style = Global)
      in
      ressource.set_gui_global <- set_global;
      let count, set_count =
        text_input'
          ~class_: "count"
          ~on_change: (fun new_value -> ressource.count <- new_value; update ())
          ressource.count
      in
      ressource.set_gui_count <- set_count;
      div ~class_: "ressourceinput" [
        global;
        count;
        gui_icon ressource.ressource.name;
      ]
    in
    div ~class_: "main" [
      div ~class_: "input" (
        List.map ressource_input ressources
      );
      output;
    ],
    update
  in
  last_hash := Html.get_hash ();
  parse_hash !last_hash;
  update ();
  let hash_change () =
    let new_hash = Html.get_hash () in
    if new_hash <> !last_hash then (
      last_hash := new_hash;
      parse_hash new_hash;
      update ();
    )
  in
  on_hash_change hash_change;
  gui
