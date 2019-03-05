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

let resources_by_category: (string * gui_resource list) list =
  let make_resource resource =
    {
      resource;
      count = "0";
      set_gui_global = (fun _ -> ());
      set_gui_count = (fun _ -> ());
    }
  in
  List.map
    (fun (category, resources) -> category, List.map make_resource resources)
    resources

let resources: gui_resource list =
  List.map snd resources_by_category |> List.flatten

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
      power_armor_mk2.name, "Power_armor_MK2";
      personal_battery.name, "Battery-equipment";
      personal_battery_mk2.name, "Battery_MK2";
      energy_shield_mk2.name, "Energy_shield_MK2";
    ]
  in
  let table = Hashtbl.create 16 in
  List.iter (fun (name, src) -> Hashtbl.add table name src) list;
  table

let special_hrefs =
  let list =
    [
      power_armor_mk2.name, "Power_armor_MK2";
      personal_battery.name, "Battery_MK1";
      personal_battery_mk2.name, "Battery_MK2";
      energy_shield_mk2.name, "Energy_shield_MK2";
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
                Bytes.set src i '_'
              else if i > 0 then
                Bytes.set src i (Char.lowercase_ascii chr)
            done;
            src
    in
    let src = "https://wiki.factorio.com/images/"^src^".png" in
    let href =
      match Hashtbl.find special_hrefs alt with
        | href ->
            href
        | exception Not_found ->
            let href = Bytes.of_string alt in
            for i = 0 to Bytes.length href - 1 do
              let chr = Bytes.get href i in
              if chr = ' ' then
                Bytes.set href i '_'
              else if i > 0 then
                Bytes.set href i (Char.lowercase_ascii chr)
            done;
            href
    in
    let href = "https://wiki.factorio.com/index.php?title="^href in
    a ~href [ img ~class_: "icon" ~alt ~title: alt src ]

let last_hash = ref ""

let get_resource_request (resource: gui_resource) =
  (* The user can prefix the resource count with a letter,
     to automatically convert the count into a number of makers
     that should be running at full time. *)
  if resource.count <> "" then
    let maker =
      (* Makers are indexed from best to worst. *)
      let get_maker original_index =
        let rec find index = function
          | [] -> None
          | hd :: _ when index = 0 -> Some (hd, original_index)
          | _ :: tl -> find (index - 1) tl
        in
        let makers =
          (* Sort the makers by crafting speed, from best to worst. *)
          let compare_makers (a: maker) (b: maker) =
            compare b.crafting_speed a.crafting_speed
          in
          List.sort compare_makers resource.resource.makers
        in
        find original_index makers
      in
      match resource.count.[0] with
        | 'a' | 'A' -> get_maker 0
        | 'b' | 'B' -> get_maker 1
        | 'c' | 'C' -> get_maker 2
        | _ -> None
    in
    let count, index, should_apply_time_unit =
      match maker with
        | None ->
            parse_float resource.count, None, true
        | Some (maker, index) ->
            let maker_count =
              parse_float
                (String.sub resource.count 1
                   (String.length resource.count - 1))
            in
            maker_count *. maker.crafting_speed /.
            resource.resource.time *. resource.resource.count,
            Some (index, maker_count),
            false
    in
    count, resource.resource, index, should_apply_time_unit
  else
    0., resource.resource, None, false

type module_settings =
  {
    speed_bonus: float;
    productivity_bonus: float;
  }

type time_unit =
  | Seconds
  | Minutes
  | Hours

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
    mutable solid_fuel: [ `heavy | `light | `petroleum ];
    mutable drill_electric_modules: module_settings;
    mutable pumpjack_modules: module_settings;
    mutable furnace_electric_modules: module_settings;
    mutable assembling_machine_2_modules: module_settings;
    mutable assembling_machine_3_modules: module_settings;
    mutable chemical_plant_modules: module_settings;
    mutable rocket_silo_modules: module_settings;
    mutable time_unit: time_unit;
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
    solid_fuel = `petroleum;
    drill_electric_modules = no_bonuses;
    pumpjack_modules = no_bonuses;
    furnace_electric_modules = no_bonuses;
    assembling_machine_2_modules = no_bonuses;
    assembling_machine_3_modules = no_bonuses;
    chemical_plant_modules = no_bonuses;
    rocket_silo_modules = no_bonuses;
    time_unit = Seconds;
    update_gui = [];
  }

let gui_goals (global: summary list) (goals: summary list) =
  let get_global_goal_throughput goal =
    match List.find (fun global_goal -> global_goal.goal = goal) global with
      | exception Not_found ->
          None
      | global_goal ->
          Some global_goal.throughput
  in
  let rec gui_goal (goal: summary) =
    let makers =
      match goal.makers with
        | [] ->
            (
              match get_global_goal_throughput goal.goal with
                | None ->
                    []
                | Some global_throughput ->
                    let percent =
                      int_of_float
                        (goal.throughput *. 100. /. global_throughput)
                    in
                    [
                      span ~class_: "sharedpercent" [
                        text "  (";
                        text (string_of_int percent);
                        text "%)";
                      ];
                    ]
            )
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
      let throughput, unit =
        match settings.time_unit with
          | Seconds -> goal.throughput, "/s"
          | Minutes -> goal.throughput *. 60., "/min"
          | Hours -> goal.throughput *. 3600., "/h"
      in
      [
        gui_icon goal.goal;
        text " × ";
        ft throughput;
        text unit;
      ]
      @ makers
      @ List.map gui_goal goal.subgoals
    )
  in
  List.map gui_goal goals

let get_resource_request_and_apply_time_unit (resource: gui_resource) =
  let count, resource, _, should_apply_time_unit =
    get_resource_request resource
  in
  let count =
    if should_apply_time_unit then
      match settings.time_unit with
        | Seconds -> count
        | Minutes -> count /. 60.
        | Hours -> count /. 3600.
    else
      count
  in
  count, resource

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
    else if name = pumpjack.name then
      apply settings.pumpjack_modules
    else if name = electric_furnace.name then
      apply settings.furnace_electric_modules
    else if name = assembling_machine_2.name then
      apply settings.assembling_machine_2_modules
    else if name = assembling_machine_3.name then
      apply settings.assembling_machine_3_modules
    else if name = chemical_plant_.name then
      apply settings.chemical_plant_modules
    else if name = rocket_silo.name then
      apply settings.rocket_silo_modules
    else
      maker
  in
  let resource =
    if resource.name = petroleum_gas.name then
      match settings.petroleum_gas with
        | `basic -> petroleum_gas_basic
        | `advanced -> petroleum_gas_advanced
        | `cracking -> petroleum_gas_cracking
    else
      resource
  in
  let resource =
    if resource.name = solid_fuel.name then
      match settings.solid_fuel with
        | `heavy -> solid_fuel_from_heavy_oil
        | `light -> solid_fuel_from_light_oil
        | `petroleum -> solid_fuel_from_petroleum_gas
    else
      resource
  in
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
  let productivity_bonus =
    if resource.allow_productivity then !productivity_bonus else 0.
  in
  let count = resource.count *. (1. +. productivity_bonus /. 100.) in
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
    let count, _, index, _ = get_resource_request resource in
    (
      match index with
        | Some (index, maker_count) when index >= 0 && index < 26 ->
            let index = String.make 1 (Char.chr (Char.code 'A' + index)) in
            "r" ^ index ^ float "" maker_count
        | Some _
        | None ->
            float "r" count
    ) ^
    style
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
    (
      match settings.time_unit with
        | Seconds -> "s"
        | Minutes -> "m"
        | Hours -> "h"
    ) ::
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
    (
      match settings.solid_fuel with
        | `heavy -> "H"
        | `light -> "L"
        | `petroleum -> "P"
    ) ::
    make_module_hash settings.drill_electric_modules ::
    make_module_hash settings.pumpjack_modules ::
    make_module_hash settings.furnace_electric_modules ::
    make_module_hash settings.assembling_machine_2_modules ::
    make_module_hash settings.assembling_machine_3_modules ::
    make_module_hash settings.chemical_plant_modules ::
    make_module_hash settings.rocket_silo_modules ::
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
      let index = ref "" in
      (
        (* Try to read the maker index if any. *)
        let old_i = !i in
        match next () with
          | 'A'..'Z' as c ->
              index := String.make 1 c
          | _ ->
              i := old_i
      );
      while not !stop do
        match next () with
          | 'r' ->
              (* Go back so that the next resource can parse this 'r'. *)
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
        let count = !index ^ (parse_float count |> fs) in
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
  (
    match next () with
      | 's' -> settings.time_unit <- Seconds
      | 'm' -> settings.time_unit <- Minutes
      | 'h' -> settings.time_unit <- Hours
      | _ -> (* backward compatibility *) decr i
  );
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
  (
    match next () with
      | 'H' -> settings.solid_fuel <- `heavy
      | 'L' -> settings.solid_fuel <- `light
      | 'P' -> settings.solid_fuel <- `petroleum
      | _ -> ()
  );
  parse_module_bonuses
    (fun x -> settings.drill_electric_modules <- x);
  parse_module_bonuses
    (fun x -> settings.pumpjack_modules <- x);
  parse_module_bonuses
    (fun x -> settings.furnace_electric_modules <- x);
  parse_module_bonuses
    (fun x -> settings.assembling_machine_2_modules <- x);
  parse_module_bonuses
    (fun x -> settings.assembling_machine_3_modules <- x);
  parse_module_bonuses
    (fun x -> settings.chemical_plant_modules <- x);
  parse_module_bonuses
    (fun x -> settings.rocket_silo_modules <- x);
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
  let settings_panel =
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
      div ~class_: "" [
        text "Time Unit: ";
        rb "timeunit"
          (fun () -> settings.time_unit = Seconds)
          (fun () -> settings.time_unit <- Seconds);
        text "seconds ";
        rb "timeunit"
          (fun () -> settings.time_unit = Minutes)
          (fun () -> settings.time_unit <- Minutes);
        text "minutes ";
        rb "timeunit"
          (fun () -> settings.time_unit = Hours)
          (fun () -> settings.time_unit <- Hours);
        text "hours ";
      ];
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
      div ~class_: "setting" [
        text "Solid fuel: ";
        rb "solidfuel"
          (fun () -> settings.solid_fuel = `heavy)
          (fun () -> settings.solid_fuel <- `heavy);
        gui_icon "Solid fuel from heavy oil";
        rb "solidfuel"
          (fun () -> settings.solid_fuel = `light)
          (fun () -> settings.solid_fuel <- `light);
        gui_icon "Solid fuel from light oil";
        rb "solidfuel"
          (fun () -> settings.solid_fuel = `petroleum)
          (fun () -> settings.solid_fuel <- `petroleum);
        gui_icon "Solid fuel from petroleum gas";
      ];
      module_settings "Electric Mining Drill"
        (fun () -> settings.drill_electric_modules)
        (fun x -> settings.drill_electric_modules <- x);
      module_settings "Pumpjack"
        (fun () -> settings.pumpjack_modules)
        (fun x -> settings.pumpjack_modules <- x);
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
      module_settings "Rocket Silo"
        (fun () -> settings.rocket_silo_modules)
        (fun x -> settings.rocket_silo_modules <- x);
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
        set_settings_div settings_panel
      else
        set_settings_div show_settings
  );
  !update_settings_div ();

  let gui, update =
    let result, set_result = Html.div' ~class_: "result" [] in
    let update () =
      let resources =
        List.map
          (fun x ->
             let a, b =
               get_resource_request_and_apply_time_unit
                 { x with resource = apply_settings x.resource }
             in
             a, b)
          resources
      in
      let meta_resource = res "" [] 0. resources in
      let remove_zero = List.filter (fun goal -> goal.throughput <> 0.) in
      let global = summarize_global 1. meta_resource |> remove_zero in
      let local = (summarize_local 1. meta_resource).subgoals |> remove_zero in
      let result =
        match global, local with
          | [], [] ->
              [
                div ~class_: "outputh1" [ text "Current Version" ];
                p [
                  text
                    "Check out the new solid fuel settings added by \
                     DeCristoforis in Advanced Settings! \
                     I also split recipes into categories. ";
                  a ~href: "https://github.com/doomeer/factorio/issues/51"
                    [ text "Discuss it on GitHub." ];
                ];
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
                p_text
                  "When shared resources are used, they have a blue percentage \
                   next to them which shows how much of the total is used for \
                   this particular use.";
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
                   if you allow yellow and blue assembly machines in \
                   Advanced Settings, \
                   requesting b2 laser turrets means that \
                   2 blue assembling machines should run at full speed. \
                   If you only allow blue assembly machines, b2 means \
                   nothing as there is no second-fastest machine. \
                   In this case, a2 would mean that 2 blue assembly machines \
                   should run at full speed.";
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
                div ~class_: "outputh1" [ text "Acknowledgments" ];
                p [
                  text
                    "Thanks to Nick Sheffield for the great style sheet! \
                     Thanks to s3bash, dwahler and DeCristoforis \
                     for 0.15 recipes! \
                     Thanks to jab416171 for 0.16 recipes! \
                     Thanks to NixonK for patching image links! \
                     Thanks to TheOddler for the better CSS for small \
                     screens! \
                     Thanks to Iidebyo for patching several wrong recipes! \
                     Thanks to JuicyJuuce for even more recipes! \
                     Thanks to DeCristoforis for the Solid Fuel setting! \
                     Thanks to Saintis for the science pack category! \
                     Thanks to Lava84flow, SamuelWr and keeshoekzema for \
                     the 0.17 recipes! \
                     And thanks to everyone who helped finding bugs \
                     or posted suggestions. This tool greatly improved \
                     thanks to you.";
                ];
              ]
          | _ :: _, []
          | [], _ :: _ ->
              gui_goals global (global @ local)
          | _ :: _, _ :: _ ->
              [
                div ~class_: "outputh1" [ text "Shared Resources" ];
                div ~class_: "goals" (gui_goals global global);
                div ~class_: "outputh1" [ text "Non-Shared Resources" ];
                div ~class_: "goals" (gui_goals global local);
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
    let resource_input_category (name, resources) =
      p ~class_: "resource_category" [ text name ] ::
      List.map resource_input resources
    in
    div ~class_: "main" [
      div ~class_: "leftcolumn" (
        List.flatten (List.map resource_input_category resources_by_category)
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
