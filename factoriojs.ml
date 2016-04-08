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
      "Basic Oil Processing", "Basic_oil_processing";
      "Advanced Oil Processing", "Advanced_oil_processing";
      "Heavy Oil Cracking", "Oil_processing";
      "Light Oil Cracking", "Oil_processing";
    ]
  in
  let table = Hashtbl.create 16 in
  List.iter (fun (name, src) -> Hashtbl.add table name src) list;
  table

let rec gui_icon alt =
  if alt = "Advanced Oil Processing + Cracking" then
    span [
      gui_icon "Advanced Oil Processing";
      text "+";
      gui_icon "Heavy Oil Cracking";
      text "+";
      gui_icon "Light Oil Cracking";
    ]
  else
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

type module_settings =
  {
    speed_bonus: float;
    productivity_bonus: float;
  }

(* If you add a setting, don't forget to:
   - update [make_hash];
   - update [parse_hash];
   - update [apply_settings]. *)
type settings =
  {
    mutable drill_burner: bool;
    mutable drill_electric: bool;
    mutable furnace_stone: bool;
    mutable furnace_steel: bool;
    mutable furnace_electric: bool;
    mutable assembling_machine_1: bool;
    mutable assembling_machine_2: bool;
    mutable assembling_machine_3: bool;
    mutable petroleum_gas: [ `basic | `advanced | `cracking ];
    mutable drill_electric_modules: module_settings;
    mutable furnace_electric_modules: module_settings;
    mutable assembling_machine_2_modules: module_settings;
    mutable assembling_machine_3_modules: module_settings;
    mutable chemical_plant_modules: module_settings;
    (* update_gui: called to copy the values above into the GUI inputs *)
    mutable update_gui: (unit -> unit) list;
  }

let no_bonuses = { speed_bonus = 0.; productivity_bonus = 0. }

let settings =
  {
    drill_burner = false;
    drill_electric = true;
    furnace_stone = false;
    furnace_steel = false;
    furnace_electric = true;
    assembling_machine_1 = false;
    assembling_machine_2 = true;
    assembling_machine_3 = true;
    petroleum_gas = `cracking;
    drill_electric_modules = no_bonuses;
    furnace_electric_modules = no_bonuses;
    assembling_machine_2_modules = no_bonuses;
    assembling_machine_3_modules = no_bonuses;
    chemical_plant_modules = no_bonuses;
    update_gui = [];
  }

let rec apply_settings (resource: resource) =
  let filter_maker (maker: maker) =
    let name = maker.name in
    if name = burner_mining_drill.name then
      settings.drill_burner
    else if name = electric_mining_drill.name then
      settings.drill_electric
    else if name = stone_furnace.name then
      settings.furnace_stone
    else if name = steel_furnace.name then
      settings.furnace_steel
    else if name = electric_furnace.name then
      settings.furnace_electric
    else if name = assembling_machine_1.name then
      settings.assembling_machine_1
    else if name = assembling_machine_2.name then
      settings.assembling_machine_2
    else if name = assembling_machine_3.name then
      settings.assembling_machine_3
    else
      true (* No setting for this maker, keep it. *)
  in
  let productivity_bonus = ref 0. in
  let apply_speed_modules (maker: maker) =
    let name = maker.name in
    let apply modules =
      (* We use the last maker's productivity bonus. *)
      productivity_bonus := modules.productivity_bonus;
      {
        maker with
          crafting_speed =
            maker.crafting_speed *.
            (1. +. modules.speed_bonus /. 100.);
      }
    in
    if name = electric_mining_drill.name then
      apply settings.drill_electric_modules
    else if name = electric_furnace.name then
      apply settings.furnace_electric_modules
    else if name = assembling_machine_2.name then
      apply settings.assembling_machine_2_modules
    else if name = assembling_machine_3.name then
      apply settings.assembling_machine_3_modules
    else if name = chemical_plant_.name then
      apply settings.chemical_plant_modules
    else
      maker
  in
  if resource.name = petroleum_gas.name then
    match settings.petroleum_gas with
      | `basic -> petroleum_gas_basic
      | `advanced -> petroleum_gas_advanced
      | `cracking -> petroleum_gas_cracking
  else
    (* Need to compute the makers first so that the [productivity_bonus]
       reference is updated before we use it. *)
    let makers =
      List.map
        apply_speed_modules
        (List.filter filter_maker resource.makers)
    in
    let ingredients =
      List.map
        (fun (count, ingredient) -> count, apply_settings ingredient)
        resource.ingredients
    in
    let count = resource.count *. (1. +. !productivity_bonus /. 100.) in
    {
      resource with
        makers;
        ingredients;
        count;
    }

let make_hash () =
  let float prefix f = prefix ^ if f = 0. then "" else fs f in
  let make_module_hash (modules: module_settings) =
    float "b" modules.speed_bonus ^ float "b" modules.productivity_bonus
  in
  let make_resource_hash (resource: gui_resource) =
    let style =
      match resource.resource.style with
        | Global -> "g"
        | Local -> ""
    in
    let count, _ = get_resource_request resource in
    float "r" count ^ style
  in
  let bools a b c =
    match a, b, c with
      | false, false, false -> "0"
      | false, false, true -> "1"
      | false, true, false -> "2"
      | false, true, true -> "3"
      | true, false, false -> "4"
      | true, false, true -> "5"
      | true, true, false -> "6"
      | true, true, true -> "7"
  in
  (
    bools false settings.drill_burner settings.drill_electric ::
    bools settings.furnace_stone settings.furnace_steel
      settings.furnace_electric ::
    bools settings.assembling_machine_1 settings.assembling_machine_2
      settings.assembling_machine_3 ::
    (
      match settings.petroleum_gas with
        | `basic -> "0"
        | `advanced -> "1"
        | `cracking -> "2"
    ) ::
    make_module_hash settings.drill_electric_modules ::
    make_module_hash settings.furnace_electric_modules ::
    make_module_hash settings.assembling_machine_2_modules ::
    make_module_hash settings.assembling_machine_3_modules ::
    make_module_hash settings.chemical_plant_modules ::
    List.map make_resource_hash resources
  )
  |> String.concat ""

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
  let parse_bools a b c =
    match next () with
      | '0' -> a false; b false; c false
      | '1' -> a false; b false; c true
      | '2' -> a false; b true; c false
      | '3' -> a false; b true; c true
      | '4' -> a true; b false; c false
      | '5' -> a true; b false; c true
      | '6' -> a true; b true; c false
      | '7' -> a true; b true; c true
      | _ -> ()
  in
  let parse_resource_hash (resource: gui_resource) =
    if next () = 'r' then (
      let chars = ref [] in
      let global = ref false in
      let stop = ref false in
      while not !stop do
        match next () with
          | 'r' ->
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
  let parse_module_bonus () =
    if next () = 'b' then (
      let chars = ref [] in
      let stop = ref false in
      while not !stop do
        match next () with
          | 'b' | 'r' ->
              (* Go back so that the next parser can parse this character. *)
              decr i;
              stop := true
          | ('0' .. '9' | '.' | '-' as c) ->
              chars := c :: !chars
          | _ ->
              i := -1;
              stop := true
      done;
      if !i >= 0 then
        let count =
          String.concat "" (List.map (String.make 1) (List.rev !chars))
        in
        parse_float count
      else
        0.
    )
    else
      0.
  in
  let parse_module_bonuses set =
    let speed_bonus = parse_module_bonus () in
    let productivity_bonus = parse_module_bonus () in
    set { speed_bonus; productivity_bonus }
  in
  parse_bools
    (fun _ -> ())
    (fun x -> settings.drill_burner <- x)
    (fun x -> settings.drill_electric <- x);
  parse_bools
    (fun x -> settings.furnace_stone <- x)
    (fun x -> settings.furnace_steel <- x)
    (fun x -> settings.furnace_electric <- x);
  parse_bools
    (fun x -> settings.assembling_machine_1 <- x)
    (fun x -> settings.assembling_machine_2 <- x)
    (fun x -> settings.assembling_machine_3 <- x);
  (
    match next () with
      | '0' -> settings.petroleum_gas <- `basic
      | '1' -> settings.petroleum_gas <- `advanced
      | '2' -> settings.petroleum_gas <- `cracking
      | _ -> ()
  );
  parse_module_bonuses
    (fun x -> settings.drill_electric_modules <- x);
  parse_module_bonuses
    (fun x -> settings.furnace_electric_modules <- x);
  parse_module_bonuses
    (fun x -> settings.assembling_machine_2_modules <- x);
  parse_module_bonuses
    (fun x -> settings.assembling_machine_3_modules <- x);
  parse_module_bonuses
    (fun x -> settings.chemical_plant_modules <- x);
  List.iter parse_resource_hash resources;
  List.iter (fun f -> f ()) settings.update_gui

let () =
  Html.run @@ fun () ->

  let settings_visible = ref false in
  let settings_div, set_settings_div = div' ~class_: "settings" [] in

  (* [update_settings_div] will be set after [settings] and [show_settings]
     are defined. *)
  let update_settings_div = ref (fun () -> ()) in

  (* [settings_changed] will be set to [update] once [update] is defined. *)
  let settings_changed = ref (fun () -> ()) in
  let settings =
    let update () = !settings_changed () in
    let cb get set =
      let on_change value =
        if get () <> value then (
          set value;
          update ()
        )
      in
      let cb, set_gui = checkbox_input' ~on_change (get ()) in
      let new_update_gui () = set_gui (get ()) in
      settings.update_gui <- new_update_gui :: settings.update_gui;
      cb
    in
    let rb name get set =
      let on_change value =
        if value then (
          set ();
          update ()
        )
      in
      let rb, set_gui = radio_input' ~name ~on_change (get ()) in
      let new_update_gui () = set_gui (get ()) in
      settings.update_gui <- new_update_gui :: settings.update_gui;
      rb
    in
    let ti get set =
      let on_change value =
        if get () <> value then (
          set value;
          update ()
        )
      in
      let cb, set_gui = text_input' ~on_change (get ()) in
      let new_update_gui () = set_gui (get ()) in
      settings.update_gui <- new_update_gui :: settings.update_gui;
      cb
    in
    let module_settings ?(help = "") name get set =
      div ~class_: "setting" [
        gui_icon name;
        text " speed bonus: ";
        ti
          (fun () -> fs (get ()).speed_bonus)
          (fun x -> set { (get ()) with speed_bonus = parse_float x });
        text "% productivity bonus: ";
        ti
          (fun () -> fs (get ()).productivity_bonus)
          (fun x ->
             set { (get ()) with productivity_bonus = parse_float x });
        text ("%"^help);
      ]
    in
    [
      button
        ~class_: "setvisible"
        ~on_click:
          (fun () -> settings_visible := false; !update_settings_div ())
        [ text "Hide Advanced Settings" ];
      div ~class_: "setting" [
        text "Drills: ";
        cb
          (fun () -> settings.drill_burner)
          (fun x -> settings.drill_burner <- x);
        gui_icon "Burner Mining Drill";
        cb
          (fun () -> settings.drill_electric)
          (fun x -> settings.drill_electric <- x);
        gui_icon "Electric Mining Drill";
      ];
      div ~class_: "setting" [
        text "Furnaces: ";
        cb
          (fun () -> settings.furnace_stone)
          (fun x -> settings.furnace_stone <- x);
        gui_icon "Stone Furnace";
        cb
          (fun () -> settings.furnace_steel)
          (fun x -> settings.furnace_steel <- x);
        gui_icon "Steel Furnace";
        cb
          (fun () -> settings.furnace_electric)
          (fun x -> settings.furnace_electric <- x);
        gui_icon "Electric Furnace";
      ];
      div ~class_: "setting" [
        text "Assembling Machines: ";
        cb
          (fun () -> settings.assembling_machine_1)
          (fun x -> settings.assembling_machine_1 <- x);
        gui_icon "Assembling Machine 1";
        cb
          (fun () -> settings.assembling_machine_2)
          (fun x -> settings.assembling_machine_2 <- x);
        gui_icon "Assembling Machine 2";
        cb
          (fun () -> settings.assembling_machine_3)
          (fun x -> settings.assembling_machine_3 <- x);
        gui_icon "Assembling Machine 3";
      ];
      div ~class_: "setting" [
        text "Petroleum Gas: ";
        rb "oilprocessing"
          (fun () -> settings.petroleum_gas = `basic)
          (fun () -> settings.petroleum_gas <- `basic);
        gui_icon "Basic Oil Processing";
        rb "oilprocessing"
          (fun () -> settings.petroleum_gas = `advanced)
          (fun () -> settings.petroleum_gas <- `advanced);
        gui_icon "Advanced Oil Processing";
        rb "oilprocessing"
          (fun () -> settings.petroleum_gas = `cracking)
          (fun () -> settings.petroleum_gas <- `cracking);
        gui_icon "Advanced Oil Processing + Cracking";
      ];
      module_settings "Electric Mining Drill"
        (fun () -> settings.drill_electric_modules)
        (fun x -> settings.drill_electric_modules <- x);
      module_settings "Electric Furnace"
        (fun () -> settings.furnace_electric_modules)
        (fun x -> settings.furnace_electric_modules <- x);
      module_settings "Assembling Machine 2"
        (fun () -> settings.assembling_machine_2_modules)
        (fun x -> settings.assembling_machine_2_modules <- x);
      module_settings "Assembling Machine 3"
        (fun () -> settings.assembling_machine_3_modules)
        (fun x -> settings.assembling_machine_3_modules <- x);
      module_settings "Chemical Plant"
        ~help: " (not applied to Petroleum Gas)"
        (fun () -> settings.chemical_plant_modules)
        (fun x -> settings.chemical_plant_modules <- x);
      text "Note: if you use both blue and yellow machines, \
            the productivity bonus which is used is the yellow one.";
    ]
  in

  let show_settings =
    [
      button
        ~class_: "setvisible"
        ~on_click:
          (fun () -> settings_visible := true; !update_settings_div ())
        [ text "Advanced Settings" ];
    ]
  in

  update_settings_div := (
    fun () ->
      if !settings_visible then
        set_settings_div settings
      else
        set_settings_div show_settings
  );
  !update_settings_div ();

  let gui, update =
    let result, set_result = Html.div' ~class_: "result" [] in
    let update () =
      let resources =
        List.map get_resource_request resources
      in
      let meta_resource = res "" [] 0. resources |> apply_settings in
      let remove_zero = List.filter (fun goal -> goal.throughput <> 0.) in
      let global = summarize_global 1. meta_resource |> remove_zero in
      let local = (summarize_local 1. meta_resource).subgoals |> remove_zero in
      let result =
        match global, local with
          | [], [] ->
              [
                div ~class_: "outputh1" [ text "Getting Started" ];
                p_text
                  "Set the number next to each resource to the \
                   requested throughput.";
                div ~class_: "outputh1" [ text "Shared Resources" ];
                p_text
                  "The checkbox controls whether \
                   the resource is shared. \
                   For instance, to produce Assembling Machine 1, you \
                   need iron plates for the machines themselves, but also \
                   for gears and for electronic circuits. \
                   Not sharing the plates means having \
                   three sets of furnaces, one for the circuits, one for \
                   the gears and one for the machines. \
                   Sharing the plates means having one set of furnaces \
                   to produce the plates which you then dispatch to \
                   other assemblers.";
                div ~class_: "outputh1" [ text "Share Your Settings" ];
                p_text
                  "See the part after the # \
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
                div ~class_: "outputh1" [ text "Links" ];
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
                div ~class_: "outputh1" [ text "Shared Resources" ];
                div ~class_: "goals" (gui_goals global);
                div ~class_: "outputh1" [ text "Non-Shared Resources" ];
                div ~class_: "goals" (gui_goals local);
              ]
      in
      set_result result;
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
      div ~class_: "leftcolumn" (
        List.map resource_input resources
        @ [
          p ~class_: "button" [
            a ~class_: "button" ~href: "index.html" [ text "Reset" ];
          ];
        ]
      );
      div ~class_: "rightcolumn" [
        settings_div;
        div ~class_: "result" [ result ];
      ];
    ],
    update
  in
  settings_changed := update;
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
