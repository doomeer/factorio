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

(* Type definitions and helpful functions are defined in factorio.ml. *)
open Factorio

(******************************************************************************)
(*                                   Makers                                   *)
(******************************************************************************)

(* Maker definitions. Makers are items such as assembling machines,
   that are used to make recipes. *)

(* Usage: maker name crafting_speed *)

let burner_mining_drill = maker "Burner Mining Drill" 0.7
let electric_mining_drill = maker "Electric Mining Drill" 1.
let assembling_machine_1 = maker "Assembling Machine 1" 0.5
let assembling_machine_2 = maker "Assembling Machine 2" 0.75
let assembling_machine_3 = maker "Assembling Machine 3" 1.25
let stone_furnace = maker "Stone Furnace" 1.
let steel_furnace = maker "Steel Furnace" 2.
let electric_furnace = maker "Electric Furnace" 2.
let chemical_plant = maker "Chemical Plant" 1.25

(* Shorthands for some commonly-used lists of makers. *)

let drill = [ burner_mining_drill; electric_mining_drill ]
let am1 = [ assembling_machine_1; assembling_machine_2; assembling_machine_3 ]
let am2 = [ assembling_machine_2; assembling_machine_3 ]
(* I commented out electric furnaces because their crafting time is the same
   as for steel furnaces. *)
let furnace = [ stone_furnace; steel_furnace(*; electric_furnace *) ]
let chemical_plant = [ chemical_plant ]

(******************************************************************************)
(*                                 Ressources                                 *)
(******************************************************************************)

(* Ressource definitions. Ressources are craftable items, such as potions.
   Note that makers themselves are craftable and are thus also ressources. *)

(* Usage: res name makers crafting_time ingredients

   [makers] is a list of makers.
   [ingredients] is a list of (float, ressource) pairs, where the float
   is the amount of the ressource which is required.

   Optional arguments:
   - use [~style: Global] (without the brackets) so that
     the ressource is global by default;
   - use [~count: 2.] for recipes which produce 2 items at a time. *)

(* DON'T FORGET TO ADD THE RESSOURCE TO THE LIST AT THE END OF THIS FILE! *)

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
   - with Advanced Oil Processing + Cracking, you need 1.2 Oil (and 1.3 Water)
     for 1 Gas.
   So you can simply keep the Basic Oil Processing and know that the amount
   of oil can be divided by roughly 2 with Advanced Oil Processing and
   cracking. (Who cares about Water?)
   The amount of refineries is also devided by 2 but you need to add
   chemical plants (with 5 refineries you have a perfect ratio
   with 1 heavy cracking and 7 light cracking plants)
   and water pumps (1 pump for 6 groups of 5 refineries + 8 cracking plants). *)

(* I use Petroleum Gas for Rocket Fuel but this is obviously non optimal,
   it's just for simplification's sake. *)

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
  res "Processing Unit" am2 15.
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

(******************************************************************************)
(*                             List of Ressources                             *)
(******************************************************************************)

(* These are the ressources that appear in the user interface. *)

let ressources =
  [
    iron_ore;
    copper_ore;
    coal;
    crude_oil;
    water;
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
