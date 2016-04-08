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

open Factorio
open Recipes
open Html

type gui_resource =
  {
    resource: resource;
    mutable count: string;
    mutable set_gui_global: bool -> unit;
    mutable set_gui_count: string -> unit;
  }

let resources =
  let make_resource resource =
    {
      resource;
      count = "0";
      set_gui_global = (fun _ -> ());
      set_gui_count = (fun _ -> ());
    }
  in
  List.map make_resource resources

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
      raw_wood.name, "Raw-wood";
      alien_artifact.name, "Alien-artifact";
      heavy_oil.name, "Heavy-oil";
      engine_unit.name, "Engine-unit";
      electric_engine_unit.name, "Electric-engine-unit";
      flying_robot_frame.name, "Flying-robot-frame";
      alien_science_pack.name, "Alien-science-pack";
      empty_barrel.name, "Barrel-empty";
      transport_belt.name, "Basic-transport-belt";
      underground_belt.name, "Basic-transport-belt-to-ground";
      fast_underground_belt.name, "Fast-transport-belt-to-ground";
      express_underground_belt.name, "Express-transport-belt-to-ground";
      splitter.name, "Basic-splitter";
      inserter.name, "Inserter-icon";
      efficiency_module.name, "Effectivity-module";
      efficiency_module_2.name, "Effectivity-module-2";
      efficiency_module_3.name, "Effectivity-module-3";
      low_density_structure.name, "Rocket-structure";
      electric_mining_drill.name, "Basic-mining-drill";
      active_provider_chest.name, "Logistic-chest-active-provider";
      passive_provider_chest.name, "Logistic-chest-passive-provider";
      storage_chest.name, "Logistic-chest-storage";
      requester_chest.name, "Logistic-chest-requester";
      wall.name, "Stone-wall";
      medium_electric_pole.name, "Medium-electric-pole";
      lamp.name, "Small-lamp";
      regular_magazine.name, "Basic-bullet-magazine";
      piercing_rounds_magazine.name, "Piercing-bullet-magazine";
      flamethrower_ammo.name, "Flame-thrower-ammo";
      cannon_shells.name, "Cannon-shell";
      explosive_cannon_shells.name, "Explosive-cannon-shell";
      land_mine.name, "Land-mine-research";
    ]
  in
  let table = Hashtbl.create 16 in
  List.iter (fun (name, src) -> Hashtbl.add table name src) list;
  table

let special_hrefs =
  let list =
    [
      (* No uppercase. *)
      raw_wood.name, "Raw_wood";
      alien_artifact.name, "Alien_artifact";
      iron_ore.name, "Iron_ore";
      copper_ore.name, "Copper_ore";
      copper_cable.name, "Copper_cable";
      plastic_bar.name, "Plastic_bar";
      heavy_oil.name, "Heavy_oil";
      engine_unit.name, "Engine_unit";
      electric_engine_unit.name, "Electric_engine_unit";
      flying_robot_frame.name, "Flying_robot_frame";
      alien_science_pack.name, "Alien_science_pack";
      empty_barrel.name, "Empty_barrel";
      fast_inserter.name, "Fast_inserter";
      smart_inserter.name, "Smart_inserter";
      basic_accumulator.name, "Basic_accumulator";
      efficiency_module.name, "Efficiency_module";
      productivity_module.name, "Productivity_module";
      efficiency_module_2.name, "Efficiency_module_2";
      efficiency_module_3.name, "Efficiency_module_3";
      productivity_module_2.name, "Productivity_module_2";
      productivity_module_3.name, "Productivity_module_3";
      speed_module.name, "Speed_module";
      speed_module_2.name, "Speed_module_2";
      speed_module_3.name, "Speed_module_3";
      logistic_robot.name, "Logistic_robot";
      construction_robot.name, "Construction_robot";
      low_density_structure.name, "Low_density_structure";
      rocket_control_unit.name, "Rocket_control_unit";
      solid_fuel.name, "Solid_fuel";
      rocket_fuel.name, "Rocket_fuel";
      rocket_part.name, "Rocket_part";
      petroleum_gas_basic.name, "Basic_oil_processing";
      assembling_machine_1.name, "Assembling_machine";
      assembling_machine_2.name, "Assembling_machine_2";
      assembling_machine_3.name, "Assembling_machine_3";
      underground_belt.name, "Underground_belt";
      fast_underground_belt.name, "Fast_underground_belt";
      express_underground_belt.name, "Express_underground_belt";
      long_handed_inserter.name, "Long_handed_inserter";
      wooden_chest.name, "Wooden_chest";
      gun_turret.name, "Gun_turret";
      small_electric_pole.name, "Small_electric_pole";
      medium_electric_pole.name, "Medium_electric_pole";
      big_electric_pole.name, "Big_electric_pole";
      steam_engine.name, "Steam_engine";
      pipe_to_ground.name, "Pipe-to-Ground";
      shotgun_shells.name, "Shotgun_shells";
      piercing_shotgun_shells.name, "Piercing_shotgun_shells";
      explosive_rocket.name, "Explosive_rocket";
      explosive_cannon_shells.name, "Explosive_cannon_shells";
      defender_capsule.name, "Defender_capsule";
      poison_capsule.name, "Poison_capsule";
      slowdown_capsule.name, "Slowdown_capsule";
      distractor_capsule.name, "Distractor_capsule";
      destroyer_capsule.name, "Destroyer_capsule";
      land_mine.name, "Land_mine";
      basic_grenade.name, "Basic_grenade";
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
  a ~href [ img ~class_: "icon" ~alt ~title: alt src ]

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

let get_resource_request (resource: gui_resource) =
  (* The user can prefix the resource count with a letter,
     to automatically convert the count into a number of makers
     that should be running at full time. *)
  if resource.count <> "" then
    let maker =
      (* Makers are indexed from best to worst. *)
      let get_maker index =
        let rec find index = function
          | [] -> None
          | hd :: _ when index = 0 -> Some hd
          | _ :: tl -> find (index - 1) tl
        in
        let makers =
          (* Sort the makers by crafting speed, from best to worst. *)
          let compare_makers (a: maker) (b: maker) =
            compare b.crafting_speed a.crafting_speed
          in
          List.sort compare_makers resource.resource.makers
        in
        find index makers
      in
      match resource.count.[0] with
        | 'a' | 'A' -> get_maker 0
        | 'b' | 'B' -> get_maker 1
        | 'c' | 'C' -> get_maker 2
        | _ -> None
    in
    let count =
      match maker with
        | None ->
            parse_float resource.count
        | Some maker ->
            let maker_count =
              parse_float
                (String.sub resource.count 1
                   (String.length resource.count - 1))
            in
            maker_count *. maker.crafting_speed /.
            resource.resource.time
    in
    count, resource.resource
  else
    0., resource.resource

let last_hash = ref ""

let make_hash () =
  let make_resource_hash (resource: gui_resource) =
    let style =
      match resource.resource.style with
        | Global -> "g"
        | Local -> ""
    in
    let count, _ = get_resource_request resource in
    let count = "-" ^ if count = 0. then "" else fs count in
    count^style
  in
  List.map make_resource_hash resources |> String.concat ""

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
  let parse_resource_hash (resource: gui_resource) =
    if next () = '-' then (
      let chars = ref [] in
      let global = ref false in
      let stop = ref false in
      while not !stop do
        match next () with
          | '-' ->
              (* Go back so that the next resource can parse this dash. *)
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
        resource.count <- count;
        resource.set_gui_count count;
        resource.resource.style <- (if !global then Global else Local);
        resource.set_gui_global !global
    ) else
      i := -1; (* next always returns '-' for the next resources *)
    (* if !i < 0 then alert ("Failed to parse: "^resource.resource.name); *)
  in
  List.iter parse_resource_hash resources

let () =
  Html.run @@ fun () ->

  let gui, update =
    let output, set_output = Html.div' ~class_: "output" [] in
    let update () =
      let resources =
        List.map get_resource_request resources
      in
      let meta_resource = res "" [] 0. resources in
      let remove_zero = List.filter (fun goal -> goal.throughput <> 0.) in
      let global = summarize_global 1. meta_resource |> remove_zero in
      let local = (summarize_local 1. meta_resource).subgoals |> remove_zero in
      let output =
        match global, local with
          | [], [] ->
              [
                div ~class_: "outputh1" [ text "Getting Started" ];
                p_text
                  "Set the number next to each resource to the \
                   requested throughput. The checkbox controls whether \
                   the resource is global.";
                div ~class_: "outputh1" [ text "Sharing" ];
                p_text
                  "See the #---g--g-g-g-g-------------------------- part \
                   in the URL? It encodes your settings and updates \
                   automatically. It means you can share your settings \
                   easily with other people. You can also bookmark them and \
                   even use the Back button.";
                div ~class_: "outputh1" [ text "Tips" ];
                p_text
                  "Instead of entering the requested throughput, \
                   you can enter a number of machines that should run \
                   at full speed. To this end, simply prefix the number by \
                   the letter A to use the fastest machine, B to use the \
                   second best or C to use the third best. For instance, \
                   requesting b2 laser turrets means that \
                   2 blue assembling machines should run at full speed.";
                div ~class_: "outputh1" [ text "Additional Resources" ];
                p [
                  text "Source code is available on ";
                  a ~href: "https://github.com/doomeer/factorio" [
                    text "GitHub";
                  ];
                  text ".";
                ];
                p [
                  text "See also the ";
                  a ~href: "https://www.reddit.com/r/factorio/comments/4dmxib/my_factorio_planner_now_has_a_web_interface/" [
                    text "Reddit post";
                  ];
                  text ".";
                ];
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
    let resource_input (resource: gui_resource) =
      let global, set_global =
        checkbox_input'
          ~on_change:
            (fun new_value ->
               let new_value = if new_value then Global else Local in
               resource.resource.style <- new_value; update ())
          (resource.resource.style = Global)
      in
      resource.set_gui_global <- set_global;
      let count, set_count =
        text_input'
          ~class_: "count"
          ~on_change: (fun new_value -> resource.count <- new_value; update ())
          resource.count
      in
      resource.set_gui_count <- set_count;
      div ~class_: "resourceinput" [
        global;
        count;
        gui_icon resource.resource.name;
      ]
    in
    div ~class_: "main" [
      div ~class_: "input" (
        List.map resource_input resources
        @ [
          p ~class_: "reset" [
            a ~class_: "reset" ~href: "index.html" [ text "Reset" ];
          ];
        ]
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
