(* To execute: just run "ocaml factorio.ml" *)

(* You can decide whether ressources are Global or Local.
   A Global ressource appears in the "Global Ressources" section
   and is not detailed in the "Goal" section.
   It is typically suited for ressources that are put on a main bus.

   By default, ressources are Local.
   To set a ressource to Global, add "~style: Global" to its declaration. *)

(* Coal is only counted to make Plastic Bars, not by Furnaces.
   You can change this by adding the Coal ingredient to the plate recipes. *)

(* I assume 1 Crude Oil per second per pumpjack, but obviously this changes
   all the time. *)

(* I assume Basic Oil Processing is used.
   You can change the line:
     let petroleum_gas = petroleum_gas_basic
   to one of:
     let petroleum_gas = petroleum_gas_advanced
     let petroleum_gas = petroleum_gas_cracking
   to change the oil+water to gas ratio.

   Basically:
   - with Basic Oil Processing, you need 2.5 Oil for 1 Gas;
   - with Advanced Oil Processing, you need 1.9 Oil (and 1 Water) for 1 Gas;
   - with Basic Oil Processing, you need 1.2 Oil (and 1.3 Water) for 1 Gas.
   So you can simply keep the Basic Oil Processing and know that the amount
   of oil can be divided by roughly 2 with Advanced Oil Processing and
   cracking. (Who cares about Water?)
   The amount of refineries is also devided by 2 but you need to add
   chemical plants (with 5 refineries you have a perfect ratio
   with 1 heavy cracking and 7 light cracking plants)
   and water pumps (1 pump for 6 groups of 5 refineries + 8 cracking plants). *)

(* I use Petroleum Gas for Rocket Fuel but this is obviously non optimal,
   it's just for simplification's sake. *)

(* Makers *)

type maker =
  {
    name: string;
    crafting_speed: float;
  }

let maker name crafting_speed = { name; crafting_speed }

let burner_mining_drill = maker "Burner Mining Drill" 0.7
let electric_mining_drill = maker "Electric Mining Drill" 1.
let assembling_machine_1 = maker "Assembling Machine 1" 0.5
let assembling_machine_2 = maker "Assembling Machine 2" 0.75
let assembling_machine_3 = maker "Assembling Machine 3" 1.25
let stone_furnace = maker "Stone Furnace" 1.
let steel_furnace = maker "Steel Furnace" 2.
let electric_furnace = maker "Electric Furnace" 2.
let chemical_plant = maker "Chemical Plant" 1.25

let drill = [ burner_mining_drill; electric_mining_drill ]
let am1 = [ assembling_machine_1; assembling_machine_2; assembling_machine_3 ]
let am2 = [ assembling_machine_2; assembling_machine_3 ]
let furnace = [ stone_furnace; steel_furnace(*; electric_furnace *) ]
let chemical_plant = [ chemical_plant ]

(* Ressources *)

type style =
  | Global
  | Local

type ressource =
  {
    name: string;
    makers: maker list;
    time: float;
    ingredients: (float * ressource) list;
    count: float; (* Usually 1, but sometimes 2 like for Copper Cables. *)
    mutable style: style;
  }

let res ?(count = 1.) ?(style = Local) name makers time ingredients =
  { name; makers; time; ingredients; count; style }

(* Red Potions *)

let iron_ore =
  res "Iron Ore" drill 2.
    []
let copper_ore =
  res "Copper Ore" drill 2.
    []
let iron_plate =
  res "Iron Plate" furnace 3.5 ~style: Global
    [ 1., iron_ore ]
let copper_plate =
  res "Copper Plate" furnace 3.5 ~style: Global
    [ 1., copper_ore ]
let iron_gear_wheel =
  res "Iron Gear Wheel" am1 0.5
    [ 2., iron_plate ]
let science_pack_1 =
  res "Science Pack 1" am1 5.
    [ 1., copper_plate; 1., iron_gear_wheel ]

(* Green Potions *)

let basic_transport_belt =
  res "Basic Transport Belt" am1 0.5 ~count: 2.
    [ 1., iron_plate; 1., iron_gear_wheel ]
let copper_cable =
  res "Copper Cable" am1 0.5 ~count: 2.
    [ 1., copper_plate ]
let electronic_circuit =
  res "Electronic Circuit" am1 0.5
    [ 1., iron_plate; 3., copper_cable ]
let inserter =
  res "Inserter" am2 0.5
    [ 1., iron_plate; 1., iron_gear_wheel; 1., electronic_circuit ]
let science_pack_2 =
  res "Science Pack 2" am1 6.
    [ 1., inserter; 1., basic_transport_belt ]

(* Blue Potions *)

let crude_oil =
  res "Crude Oil" [ maker "Pumpjack" 1. ] 1.
    []
let water =
  res "Water" [ maker "Offshore Pump" 1. ] 1. ~count: 60. ~style: Global
    []
let petroleum_gas_basic =
  res "Petroleum Gas" [ maker "Basic Oil Processing" 1. ] 5.
    ~count: 4. ~style: Global
    [ 10., crude_oil ]
let petroleum_gas_advanced =
  res "Petroleum Gas" [ maker "Advanced Oil Processing" 1. ] 5.
    ~count: 5.5 ~style: Global
    [ 10., crude_oil; 5., water ]
let petroleum_gas_cracking =
  (* 1 Heavy Oil becomes 3/4 = 0.75 Light Oil.
     0.75 + 4.5 = 5.25 Light Oil becomes 5.25 * 2 / 3 = 3.5 Petroleum Gas.
     So the end result is 5.5 + 3.5 = 9 Petroleum Gas.
     1 Heavy Oil requires 0.75 Water to be cracked.
     5.25 Light Oil requires 5.25 Water to be cracked.
     So the end result is that 0.75 + 5.25 + 5 = 11 Water is required. *)
  res "Petroleum Gas" [ maker "Advanced Oil Processing + Cracking" 1. ] 5.
    ~count: 9. ~style: Global
    [ 10., crude_oil; 11., water ]
let petroleum_gas = petroleum_gas_basic
let sulfur =
  res "Sulfur" chemical_plant 1. ~count: 2.
    [ 3., water; 3., petroleum_gas ]
let sulfuric_acid =
  res "Sulfuric Acid" chemical_plant 1. ~count: 5.
    [ 1., iron_plate; 5., sulfur; 10., water ]
let battery =
  res "Battery" chemical_plant 5.
    [ 1., iron_plate; 1., copper_plate; 2., sulfuric_acid ]
let coal =
  res "Coal" drill 2. ~style: Global
    []
let plastic_bar =
  res "Plastic Bar" chemical_plant 1. ~count: 2.
    [ 1., coal; 3., petroleum_gas ]
let advanced_circuit =
  res "Advanced Circuit" am2 8.
    [ 2., electronic_circuit; 2., plastic_bar; 4., copper_cable ]
let fast_inserter =
  res "Fast Inserter" am2 0.5
    [ 1., inserter; 2., iron_plate; 2., electronic_circuit ]
let smart_inserter =
  res "Smart Inserter" am2 0.5
    [ 1., fast_inserter; 4., electronic_circuit ]
let steel_plate =
  res "Steel Plate" furnace 17.5
    [ 5., iron_plate ]
let science_pack_3 =
  res "Science Pack 3" am2 12.
    [ 1., battery; 1., advanced_circuit; 1., smart_inserter; 1., steel_plate ]

(* Useful Stuff *)

let solar_panel =
  res "Solar Panel" am2 10.
    [ 5., steel_plate; 15., electronic_circuit; 5., copper_plate ]
let basic_accumulator =
  res "Basic Accumulator" am1 10.
    [ 2., iron_plate; 5., battery ]
let laser_turret =
  res "Laser Turret" am2 20.
    [ 20., steel_plate; 20., electronic_circuit; 12., battery ]
let processing_unit =
  res "Processing Unit" am2 30.
    [ 20., electronic_circuit; 2., advanced_circuit; 0.5, sulfuric_acid ]

(* Modules *)

let speed_module =
  res "Speed Module" am2 20.
    [ 5., advanced_circuit; 5., electronic_circuit ]

(* Rocket Parts *)

let low_density_structure =
  res "Low Density Structure" am2 30.
    [ 10., steel_plate; 5., copper_plate; 5., plastic_bar ]
let rocket_control_unit =
  res "Rocket Control Unit" am1 30.
    [ 1., processing_unit; 1., speed_module ]
let solid_fuel_from_petroleum_gas =
  res "Solid Fuel" chemical_plant 3.
    [ 2., petroleum_gas ]
let solid_fuel = solid_fuel_from_petroleum_gas
let rocket_fuel =
  res "Rocket Fuel" am1 30.
    [ 10., solid_fuel ]
let rocket_part =
  res "Rocket Part" [ maker "Rocket Silo" 1. ] 3.
    [ 10., low_density_structure; 10., rocket_fuel; 10., rocket_control_unit ]

(* (\* Summarize (old text version) *\) *)

(* let line level x = *)
(*   Printf.ksprintf *)
(*     (fun s -> print_string (String.make (level * 2) ' '); print_endline s) *)
(*     x *)

(* let s x = Printf.sprintf x *)

(* let fs x = *)
(*   let s = Printf.sprintf "%F" (ceil (x *. 10.) /. 10.) in *)
(*   let len = String.length s in *)
(*   if len > 0 && s.[len - 1] = '.' then *)
(*     String.sub s 0 (len - 1) *)
(*   else *)
(*     s *)

(* (\* Summarize in a tree style. Don't detail global ressources. *\) *)
(* let rec summarize_local ?(level = 0) throughput (ressource: ressource) = *)
(*   if level = 0 && ressource.name <> "" then line 0 ""; *)
(*   match ressource.style with *)
(*     | Local -> *)
(*         let maker_string = *)
(*           String.concat " or " *)
(*             (List.map *)
(*                (fun maker -> s "%s %s" *)
(*                    (fs (throughput /. float ressource.count *. *)
(*                         ressource.time /. maker.crafting_speed)) *)
(*                    maker.name) *)
(*                ressource.makers) *)
(*         in *)
(*         let level = *)
(*           if ressource.name = "" then *)
(*             (\* Meta-ressource. *\) *)
(*             level *)
(*           else ( *)
(*             line level "* %s/s %s (%s)" (fs throughput) ressource.name *)
(*               maker_string; *)
(*             level + 1 *)
(*           ) *)
(*         in *)
(*         List.iter *)
(*           (fun (count, ingredient) -> *)
(*              summarize_local ~level *)
(*                (throughput /. float ressource.count *. float count) *)
(*                ingredient) *)
(*           ressource.ingredients; *)
(*         () *)
(*     | Global -> *)
(*         line level "* %s/s %s" (fs throughput) ressource.name *)

(* (\* Detail global ressources only. *\) *)
(* let summarize_global throughput (ressource: ressource) = *)
(*   let table = Hashtbl.create 16 in *)
(*   let add throughput ressource = *)
(*     let previous_throughput = *)
(*       match Hashtbl.find table ressource.name with *)
(*         | exception Not_found -> *)
(*             0. *)
(*         | tp, _ -> *)
(*             tp *)
(*     in *)
(*     Hashtbl.replace table ressource.name *)
(*       (previous_throughput +. throughput, ressource) *)
(*   in *)
(*   let rec search throughput ressource = *)
(*     begin *)
(*       match ressource.style with *)
(*         | Local -> () *)
(*         | Global -> add throughput ressource *)
(*     end; *)
(*     List.iter *)
(*       (fun (count, ingredient) -> *)
(*          search (throughput /. float ressource.count *. *)
(*                  float count) ingredient) *)
(*       ressource.ingredients *)
(*   in *)
(*   search throughput ressource; *)
(*   Hashtbl.iter *)
(*     (fun _ (throughput, ressource) -> *)
(*        summarize_local throughput { ressource with style = Local }) *)
(*     table *)

(* let summarize throughput ressources = *)
(*   let meta_ressource = res "" [] 0. ressources in *)
(*   line 0 "Global Ressources"; *)
(*   line 0 "================="; *)
(*   summarize_global throughput meta_ressource; *)
(*   line 0 ""; *)
(*   line 0 "Goal"; *)
(*   line 0 "===="; *)
(*   summarize_local throughput meta_ressource *)

(* Summarize (new, structured version) *)

type summary =
  {
    throughput: float; (* goal count per second *)
    goal: string; (* goal ressource name *)
    makers: (float * string) list; (* count, name *)
    subgoals: summary list;
  }

(* Summarize in a tree style. Don't detail global ressources. *)
let rec summarize_local throughput (ressource: ressource): summary =
  match ressource.style with
    | Local ->
        let makers =
          let make_maker (maker: maker) =
            let maker_count =
              throughput /. ressource.count *.
              ressource.time /. maker.crafting_speed
            in
            maker_count, maker.name
          in
          List.map make_maker ressource.makers
        in
        let subgoals =
          let make_subgoal (count, ingredient) =
            summarize_local
              (throughput /. ressource.count *. count)
              ingredient
          in
          List.map make_subgoal ressource.ingredients
        in
        {
          throughput;
          goal = ressource.name;
          makers;
          subgoals;
        }
    | Global ->
        {
          throughput;
          goal = ressource.name;
          makers = [];
          subgoals = [];
        }

(* Detail global ressources only. *)
let summarize_global throughput (ressource: ressource) =
  let table = Hashtbl.create 16 in
  let add throughput ressource =
    let previous_throughput =
      match Hashtbl.find table ressource.name with
        | exception Not_found ->
            0.
        | tp, _ ->
            tp
    in
    Hashtbl.replace table ressource.name
      (previous_throughput +. throughput, ressource)
  in
  let rec search throughput ressource =
    begin
      match ressource.style with
        | Local -> ()
        | Global -> add throughput ressource
    end;
    List.iter
      (fun (count, ingredient) ->
         search (throughput /. ressource.count *. count) ingredient)
      ressource.ingredients
  in
  search throughput ressource;
  let result = ref [] in
  Hashtbl.iter
    (fun _ (throughput, ressource) ->
       result :=
         summarize_local throughput { ressource with style = Local }
         :: !result)
    table;
  List.rev !result

(* (\* Test *\) *)

(* let lab_count = 12 *)
(* let lab_time = 30 (\* Time to consume 1 potion of each kind in 1 lab. *\) *)

(* let () = *)
(*   summarize (float lab_count /. float lab_time) [ *)
(*     1, science_pack_1; *)
(*     1, science_pack_2; *)
(*     1, science_pack_3; *)
(*   ]; *)
(*   line 0 ""; *)
(*   line 0 "#####################################################"; *)
(*   line 0 ""; *)
(*   summarize 0.1 [ *)
(*     1, solar_panel; *)
(*     1, basic_accumulator; *)
(*     1, laser_turret; *)
(*     1, speed_module_1; *)
(*   ]; *)
(*   line 0 ""; *)
(*   line 0 "#####################################################"; *)
(*   line 0 ""; *)
(*   summarize 0.1 [ *)
(*     1, rocket_part; *)
(*   ]; *)
(*   () *)

(* List of ressources *)

let ressources =
  [
    iron_ore;
    copper_ore;
    coal;
    crude_oil;
    water;
    (* petroleum_gas_basic; *)
    (* petroleum_gas_advanced; *)
    (* petroleum_gas_cracking; *)
    petroleum_gas;
    iron_plate;
    copper_plate;
    steel_plate;
    copper_cable;
    iron_gear_wheel;
    science_pack_1;
    science_pack_2;
    science_pack_3;
    electronic_circuit;
    advanced_circuit;
    processing_unit;
    speed_module;
    basic_transport_belt;
    inserter;
    fast_inserter;
    smart_inserter;
    sulfur;
    sulfuric_acid;
    battery;
    plastic_bar;
    solar_panel;
    basic_accumulator;
    laser_turret;
    low_density_structure;
    rocket_control_unit;
    solid_fuel;
    rocket_fuel;
    rocket_part;
  ]
