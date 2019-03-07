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

(* Lava84flow: miners now have a just mining speed function instead
 of needing mining power as well so this might break everything *)

let burner_mining_drill = maker "Burner Mining Drill" 0.25
let electric_mining_drill = maker "Electric Mining Drill" 0.5
let assembling_machine_1 = maker "Assembling Machine 1" 0.5
let assembling_machine_2 = maker "Assembling Machine 2" 0.75
let assembling_machine_3 = maker "Assembling Machine 3" 1.25
let stone_furnace = maker "Stone Furnace" 1.
let steel_furnace = maker "Steel Furnace" 2.
let electric_furnace = maker "Electric Furnace" 2.
let chemical_plant_ = maker "Chemical Plant" 1.
let centrifuge = maker "Centrifuge" 1.
let pumpjack = maker "Pumpjack" 1.
let rocket_silo = maker "Rocket Silo" 0.001

(* Shorthands for some commonly-used lists of makers. *)

let drill = [ burner_mining_drill; electric_mining_drill ]
let am1 = [ assembling_machine_1; assembling_machine_2; assembling_machine_3 ]
let am2 = [ assembling_machine_2; assembling_machine_3 ]
let am3 = [ assembling_machine_3 ]
let furnace = [ stone_furnace; steel_furnace; electric_furnace ]
let chemical_plant = [ chemical_plant_ ]

(******************************************************************************)
(*                                  Resources                                 *)
(******************************************************************************)

(* Resource definitions. Resources are craftable items, such as potions.
   Note that makers themselves are craftable and are thus also resources. *)

(* Usage: res name makers crafting_time ingredients

   [makers] is a list of makers.
   [ingredients] is a list of (float, resource) pairs, where the float
   is the amount of the resource which is required.

   Optional arguments:
   - use [~style: Global] (without the brackets) so that
     the resource is global by default;
   - use [~count: 2.] for recipes which produce 2 items at a time. *)

(* DON'T FORGET TO ADD THE RESOURCE TO THE LIST AT THE END OF THIS FILE! *)

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

(* The order in this file follows the order in the Resources page of
   the Wiki, except when this would mess the dependencies. *)

(* Resources *)

let wood =
  res "Wood" [] 1. ~style: Global
    []

    (* Lava84flow: the mining thing also means that ore hardness is no
    longer a thing, so more things to break here *)

    (* Lava84flow: Someone double check the craft times for raws *)

let coal =
  res "Coal" drill 1. ~style: Global ~allow_productivity: true
    []
let iron_ore =
  res "Iron Ore" drill 1. ~allow_productivity: true
    []
let copper_ore =
  res "Copper Ore" drill 1. ~allow_productivity: true
    []
let stone =
  res "Stone" drill 1. ~allow_productivity: true
    []
let water =
  res "Water" [ maker "Offshore Pump" 1. ] 1. ~count: 1200. ~style: Global
    []
let crude_oil =
  res "Crude Oil" [ pumpjack ] 1. ~count: 10. ~allow_productivity: true
    []
let uranium_ore =
  res "Uranium Ore" drill 2. ~allow_productivity: true
    []
(* TODO: This process also produces 0.993 Uranium 238 *)
let uranium_235 =
  res "Uranium-235" [ centrifuge ] 12. ~count: 0.007 ~allow_productivity: true
    [ 10., uranium_ore ]

(* Intermediate Products *)

(* Lava84flow: wood and raw wood have been merged*)

(* let wood =
  res "Wood" am1 0.5 ~count: 2. ~allow_productivity: true
    [ 1., raw_wood ] *)

let iron_plate =
  res "Iron Plate" furnace 3.2 ~style: Global ~allow_productivity: true
    [ 1., iron_ore ]
let copper_plate =
  res "Copper Plate" furnace 3.2 ~style: Global ~allow_productivity: true
    [ 1., copper_ore ]
let steel_plate =
  res "Steel Plate" furnace 16. ~allow_productivity: true
    [ 5., iron_plate ]
let stone_brick =
  res "Stone Brick" furnace 3.2 ~allow_productivity: true
    [ 2., stone ]

let petroleum_gas_basic =
  res "Petroleum Gas" [ maker "Basic Oil Processing" 1. ] 5.
    ~count: 40. ~style: Global ~allow_productivity: true
    [ 100., crude_oil ]
let petroleum_gas_advanced =
  res "Petroleum Gas" [ maker "Advanced Oil Processing" 1. ] 5.
    ~count: 55. ~style: Global ~allow_productivity: true
    [ 100., crude_oil; 50., water ]
let petroleum_gas_cracking =
  (* 1 Heavy Oil becomes 3/4 = 0.75 Light Oil.
     0.75 + 4.5 = 5.25 Light Oil becomes 5.25 * 2 / 3 = 3.5 Petroleum Gas.
     So the end result is 5.5 + 3.5 = 9 Petroleum Gas.
     1 Heavy Oil requires 0.75 Water to be cracked.
     5.25 Light Oil requires 5.25 Water to be cracked.
     So the end result is that 0.75 + 5.25 + 5 = 11 Water is required. *)
  res "Petroleum Gas" [ maker "Advanced Oil Processing + Cracking" 1. ] 5.
    ~count: 90. ~style: Global ~allow_productivity: true
    [ 100., crude_oil; 110., water ]
let petroleum_gas = petroleum_gas_basic
let sulfur =
  res "Sulfur" chemical_plant 1. ~count: 2. ~allow_productivity: true
    [ 30., water; 30., petroleum_gas ]
let sulfuric_acid =
  res "Sulfuric Acid" chemical_plant 1. ~count: 50. ~allow_productivity: true
    [ 1., iron_plate; 5., sulfur; 100., water ]
let plastic_bar =
  res "Plastic Bar" chemical_plant 1. ~count: 2. ~allow_productivity: true
    [ 1., coal; 20., petroleum_gas ]
let battery =
  res "Battery" chemical_plant 5. ~allow_productivity: true
    [ 1., iron_plate; 1., copper_plate; 20., sulfuric_acid ]
let iron_stick =
  res "Iron Stick" am1 0.5 ~count: 2. ~allow_productivity: true
    [ 1., iron_plate ]
let iron_gear_wheel =
  res "Iron Gear Wheel" am1 0.5 ~allow_productivity: true
    [ 2., iron_plate ]
let copper_cable =
  res "Copper Cable" am1 0.5 ~count: 2. ~allow_productivity: true
    [ 1., copper_plate ]
let electronic_circuit =
  res "Electronic Circuit" am1 0.5 ~allow_productivity: true
    [ 1., iron_plate; 3., copper_cable ]
let advanced_circuit =
  res "Advanced Circuit" am1 6. ~allow_productivity: true
    [ 2., electronic_circuit; 2., plastic_bar; 4., copper_cable ]
let processing_unit =
  res "Processing Unit" am2 10. ~allow_productivity: true
    [ 20., electronic_circuit; 2., advanced_circuit; 5., sulfuric_acid ]
let pipe =
  res "Pipe" am1 0.5
    [ 1., iron_plate ]
let engine_unit =
  res "Engine Unit" am1 10. ~allow_productivity: true
    [ 1., steel_plate; 1., iron_gear_wheel; 2., pipe ]
let heavy_oil =
  res "Heavy Oil" [ maker "Basic Oil Processing" 1. ] 5.
    ~count: 3. ~allow_productivity: true
    [ 10., crude_oil ]
let light_oil =
  res "Light Oil" [ maker "Basic Oil Processing" 1. ] 5.
    ~count: 3. ~allow_productivity: true
    [ 10., crude_oil ]
let lubricant =
  res "Lubricant" chemical_plant 1. ~count: 10. ~allow_productivity: true
    [ 10., heavy_oil ]
let electric_engine_unit =
  res "Electric Engine Unit" am2 10. ~allow_productivity: true
    [ 1., engine_unit; 2., electronic_circuit; 15., lubricant ]
let flying_robot_frame =
  res "Flying Robot Frame" am1 20. ~allow_productivity: true
    [ 1., electric_engine_unit; 2., battery; 1., steel_plate;
      3., electronic_circuit ]
let transport_belt =
  res "Transport Belt" am1 0.5 ~count: 2.
    [ 1., iron_plate; 1., iron_gear_wheel ]
let inserter =
  res "Inserter" am1 0.5
    [ 1., iron_plate; 1., iron_gear_wheel; 1., electronic_circuit ]
let fast_inserter =
  res "Fast Inserter" am1 0.5
    [ 1., inserter; 2., iron_plate; 2., electronic_circuit ]
let filter_inserter =
  res "Filter Inserter" am1 0.5
    [ 1., fast_inserter; 4., electronic_circuit ]
let empty_barrel =
  res "Empty Barrel" am1 1. ~allow_productivity: true
    [ 1., steel_plate ]
let explosives =
  res "Explosives" chemical_plant 5. ~count: 2. ~allow_productivity: true
    [ 1., sulfur; 1., coal; 10., water ]

(* Weapons *)

let land_mine =
  res "Land Mine" am1 5. ~count: 4.
    [ 1., steel_plate; 2., explosives ]
let grenade =
  res "Grenade" am1 8.
    [ 10., coal; 5., iron_plate ]
let cluster_grenade =
  res "Cluster Grenade" am1 8.
    [ 7., grenade; 5., explosives; 5., steel_plate ]
let poison_capsule =
  res "Poison Capsule" am1 8.
    [ 3., steel_plate; 3., electronic_circuit; 10., coal ]
let slowdown_capsule =
  res "Slowdown Capsule" am1 8.
    [ 2., steel_plate; 2., electronic_circuit; 5., coal ]
let atomic_bomb =
  res "Atomic Bomb" am1 50.
    [ 10., explosives; 20., processing_unit; 30., uranium_235 ]
let speed_module =
  res "Speed Module" am1 15.
    [ 5., advanced_circuit; 5., electronic_circuit ]

(* Ammo *)

let firearm_magazine =
  res "Firearm Magazine" am1 1.
    [ 4., iron_plate ]
let piercing_rounds_magazine =
  res "Piercing Rounds Magazine" am1 3.
    [ 1., firearm_magazine; 1., steel_plate; 5., copper_plate ]
let defender_capsule =
  res "Defender Capsule" am1 8.
    [ 3., iron_gear_wheel; 1., flying_robot_frame;
      1., piercing_rounds_magazine ]
let distractor_capsule =
  res "Distractor Capsule" am1 15.
    [ 3., advanced_circuit; 4., defender_capsule ]
let destroyer_capsule =
  res "Destroyer Capsule" am1 15.
    [ 1., speed_module; 4., distractor_capsule ]
let shotgun_shells =
  res "Shotgun Shells" am1 3.
    [ 2., copper_plate; 2., iron_plate ]
let piercing_shotgun_shells =
  res "Piercing Shotgun Shells" am1 8.
    [ 2., shotgun_shells; 5., copper_plate; 2., steel_plate ]
let rocket =
  res "Rocket" am1 8.
    [ 1., electronic_circuit; 1., explosives; 2., iron_plate ]
let explosive_rocket =
  res "Explosive Rocket" am1 8.
    [ 1., rocket; 2., explosives ]
let flamethrower_ammo =
  res "Flamethrower Ammo" chemical_plant 6.
    [ 5., steel_plate; 50., light_oil; 50., heavy_oil ]
let cannon_shell =
  res "Cannon Shell" am1 8.
    [ 2., steel_plate; 2., plastic_bar; 1., explosives ]
let explosive_cannon_shell =
  res "Explosive Cannon Shell" am1 8.
    [ 2., steel_plate; 2., plastic_bar; 2., explosives ]

(* Modules *)

let efficiency_module =
  res "Efficiency Module" am1 15.
    [ 5., advanced_circuit; 5., electronic_circuit ]
let efficiency_module_2 =
  res "Efficiency Module 2" am1 30.
    [ 5., advanced_circuit; 5., processing_unit; 4., efficiency_module ]
let efficiency_module_3 =
  res "Efficiency Module 3" am1 60.
    [ 5., advanced_circuit; 5., processing_unit; 5., efficiency_module_2 ]
let productivity_module =
  res "Productivity Module" am1 15.
    [ 5., advanced_circuit; 5., electronic_circuit ]
let productivity_module_2 =
  res "Productivity Module 2" am1 30.
    [ 5., advanced_circuit; 5., processing_unit; 4., productivity_module ]
let productivity_module_3 =
  res "Productivity Module 3" am1 60.
    [ 5., advanced_circuit; 5., processing_unit; 5., productivity_module_2 ]
let speed_module_2 =
  res "Speed Module 2" am1 30.
    [ 5., advanced_circuit; 5., processing_unit; 4., speed_module ]
let speed_module_3 =
  res "Speed Module 3" am1 60.
    [ 5., advanced_circuit; 5., processing_unit; 5., speed_module_2 ]

(* Special *)

let car =
  res "Car" am1  2.
    [ 8., engine_unit; 20., iron_plate; 5., steel_plate ]
let tank =
  res "Tank" am1 5.
    [ 32., engine_unit; 50., steel_plate; 15., iron_gear_wheel;
      10., advanced_circuit ]
let logistic_robot =
  res "Logistic Robot" am1 0.5
    [ 1., flying_robot_frame; 2., advanced_circuit ]
let construction_robot =
  res "Construction Robot" am1 0.5
    [ 1., flying_robot_frame; 2., electronic_circuit ]
let roboport =
  res "Roboport" am1 5.
    [ 45., steel_plate; 45., iron_gear_wheel; 45., advanced_circuit ]

(* Transport Belts *)

let underground_belt =
  res "Underground Belt" am1 1. ~count: 2.
    [ 5., transport_belt; 10., iron_plate ]
let splitter =
  res "Splitter" am1 1.
    [ 5., electronic_circuit; 5., iron_plate; 4., transport_belt ]
let fast_transport_belt =
  res "Fast Transport Belt" am1 0.5
    [ 1., transport_belt; 5., iron_gear_wheel ]
let fast_underground_belt =
  res "Fast Underground Belt" am1 2. ~count: 2.
    [ 40., iron_gear_wheel; 2., underground_belt ]
let fast_splitter =
  res "Fast Splitter" am1 2.
    [ 1., splitter; 10., iron_gear_wheel; 10., electronic_circuit ]
let express_transport_belt =
  res "Express Transport Belt" am2 0.5
    [ 1., fast_transport_belt; 10., iron_gear_wheel; 20., lubricant ]
let express_underground_belt =
  res "Express Underground Belt" am2 2. ~count: 2.
    [ 80., iron_gear_wheel; 2., fast_underground_belt; 40., lubricant ]
let express_splitter =
  res "Express Splitter" am2 2.
    [ 1., fast_splitter; 10., iron_gear_wheel; 10., advanced_circuit;
      80., lubricant ]

(* Inserters *)

let burner_inserter =
  res "Burner Inserter" am1 0.5
    [ 1., iron_plate; 1., iron_gear_wheel ]
let long_handed_inserter =
  res "Long Handed Inserter" am1 0.5
    [ 1., inserter; 1., iron_plate; 1., iron_gear_wheel ]
let stack_inserter =
  res "Stack Inserter" am1 0.5
    [ 15., iron_gear_wheel; 15., electronic_circuit; 1., advanced_circuit; 1., fast_inserter ]
let stack_filter_inserter =
  res "Stack Filter Inserter" am1 0.5
    [ 1., stack_inserter; 5., electronic_circuit ]

(* Storage *)

let wooden_chest =
  res "Wooden Chest" am1 0.5
    [ 4., wood ]
let iron_chest =
  res "Iron Chest" am1 0.5
    [ 8., iron_plate ]
let steel_chest =
  res "Steel Chest" am1 0.5
    [ 8., steel_plate ]
let active_provider_chest =
  res "Active Provider Chest" am1 0.5
    [ 1., steel_chest; 3., electronic_circuit; 1., advanced_circuit ]
let passive_provider_chest =
  res "Passive Provider Chest" am1 0.5
    [ 1., steel_chest; 3., electronic_circuit; 1., advanced_circuit ]
let storage_chest =
  res "Storage Chest" am1 0.5
    [ 1., steel_chest; 3., electronic_circuit; 1., advanced_circuit ]
let requester_chest =
  res "Requester Chest" am1 0.5
    [ 1., steel_chest; 3., electronic_circuit; 1., advanced_circuit ]

(* Defensive Structures *)

let wall =
  res "Wall" am1 0.5
    [ 5., stone_brick ]
let gate =
  res "Gate" am1 0.5
    [ 2., electronic_circuit; 2., steel_plate; 1., wall ]
let gun_turret =
  res "Gun Turret" am1 8.
    [ 20., iron_plate; 10., copper_plate; 10., iron_gear_wheel ]
let flamethrower_turret =
  res "Flamethrower Turret" am1 20.
    [ 30., steel_plate; 15., iron_gear_wheel; 10., pipe; 5., engine_unit ]
let laser_turret =
  res "Laser Turret" am1 20.
    [ 20., steel_plate; 20., electronic_circuit; 12., battery ]

(* Miscellaneous *)

let concrete =
  res "Concrete" am2 10. ~count: 10.
    [ 5., stone_brick; 1., iron_ore; 100., water ]

(* Machines & Furnaces *)

(* Because makers exist with the same name, makers as resources are
   prefixed with "r_". *)
let r_stone_furnace =
  res "Stone Furnace" am1 0.5
    [ 5., stone ]
let r_burner_mining_drill =
  res "Burner Mining Drill" am1 2.
    [ 1., r_stone_furnace; 3., iron_plate; 3., iron_gear_wheel ]
let r_electric_mining_drill =
  res "Electric Mining Drill" am1 2.
    [ 10., iron_plate; 5., iron_gear_wheel; 3., electronic_circuit ]
let r_steel_furnace =
  res "Steel Furnace" am1 3.
    [ 6., steel_plate; 10., stone_brick ]
let r_electric_furnace =
  res "Electric Furnace" am1 5.
    [ 10., steel_plate; 10., stone_brick; 5., advanced_circuit ]
let r_assembling_machine_1 =
  res assembling_machine_1.name am2 0.5
    [ 3., electronic_circuit; 5., iron_gear_wheel; 9., iron_plate ]
let r_assembling_machine_2 =
  res assembling_machine_2.name am2 0.5
    [ 3., electronic_circuit; 5., iron_gear_wheel; 2., steel_plate;
      1., r_assembling_machine_1 ]
let r_assembling_machine_3 =
  res assembling_machine_3.name am1 0.5
    [ 2., r_assembling_machine_2; 4., speed_module ]
let r_lab =
  res "Lab" am1 2.
    [ 4., transport_belt; 10., iron_gear_wheel; 10., electronic_circuit ]
let r_centrifuge =
   res "Centrifuge" am1 4.
     [ 100., concrete; 50.,steel_plate; 100., advanced_circuit;
       100., iron_gear_wheel]
let r_rocket_silo =
  res rocket_silo.name am1 30.
    [ 1000., concrete; 200., electric_engine_unit; 100., pipe;
      200., processing_unit; 1000., steel_plate ]
let beacon =
  res "Beacon" am1 15.
    [ 20., electronic_circuit; 20., advanced_circuit; 10., steel_plate;
      10., copper_cable ]
let radar =
  res "Radar" am1 0.5
    [ 5., electronic_circuit; 10., iron_plate; 5., iron_gear_wheel ]

(* Electric Network *)

let small_electric_pole =
  res "Small Electric Pole" am1 0.5 ~count: 2.
    [ 1., wood; 2., copper_cable ]
let medium_electric_pole =
  res "Medium Electric Pole" am1 0.5
    [ 2., steel_plate; 2., copper_plate; 4., iron_stick ]
let big_electric_pole =
  res "Big Electric Pole" am1 0.5
    [ 5., steel_plate; 5., copper_plate; 8., iron_stick ]
let substation =
  res "Substation" am1 0.5
    [ 10., steel_plate; 5., advanced_circuit; 5., copper_plate ]
let boiler =
  res "Boiler" am1 0.5
    [ 1., r_stone_furnace; 4., pipe ]
let steam_engine =
  res "Steam Engine" am1 0.5
    [ 8., iron_gear_wheel; 5., pipe; 10., iron_plate ]
let solar_panel =
  res "Solar Panel" am1 10.
    [ 5., steel_plate; 15., electronic_circuit; 5., copper_plate ]
let accumulator =
  res "Accumulator" am1 10.
    [ 2., iron_plate; 5., battery ]
let lamp =
  res "Lamp" am1 0.5
    [ 1., electronic_circuit; 3., iron_stick; 1., iron_plate ]

(* Railway Network *)

let straight_rail =
  res "Straight Rail" am1 0.5 ~count: 2.
    [ 1., stone; 1., iron_stick; 1., steel_plate ]

(* Liquid Network *)

let pipe_to_ground =
  res "Pipe To Ground" am1 0.5 ~count: 2.
    [ 10., pipe; 5., iron_plate ]
let r_offshore_pump =
  res "Offshore Pump" am1 0.5
    [ 2., electronic_circuit; 1., pipe; 1., iron_gear_wheel ]
let storage_tank =
  res "Storage Tank" am1 3.
    [ 20., iron_plate; 5., steel_plate ]
let r_oil_refinery =
  res "Oil Refinery" am1 8.
    [ 10., pipe; 15., steel_plate; 10., stone_brick; 10., iron_gear_wheel;
      10., electronic_circuit ]
let r_chemical_plant =
  res "Chemical Plant" am1 5.
    [ 5., steel_plate; 5., iron_gear_wheel; 5., electronic_circuit;
      5., pipe ]
let r_pumpjack =
  res "Pumpjack" am1 5.
    [ 5., steel_plate; 10., iron_gear_wheel; 5., electronic_circuit;
      10., pipe ]
let pump =
  res "Pump" am1 2.
    [ 1., engine_unit; 1., steel_plate; 1., pipe ]

(* Rocket Compenents *)

let low_density_structure =
  res "Low Density Structure" am1 20. ~allow_productivity: true
    [ 2., steel_plate; 20., copper_plate; 5., plastic_bar ]
let rocket_control_unit =
  res "Rocket Control Unit" am1 30. ~allow_productivity: true
    [ 1., processing_unit; 1., speed_module ]
let solid_fuel_from_heavy_oil =
  res "Solid Fuel" chemical_plant 3. ~allow_productivity: true
    [ 20., heavy_oil ]
let solid_fuel_from_light_oil =
  res "Solid Fuel" chemical_plant 3. ~allow_productivity: true
    [ 10., light_oil ]
let solid_fuel_from_petroleum_gas =
  res "Solid Fuel" chemical_plant 3. ~allow_productivity: true
    [ 20., petroleum_gas ]
let solid_fuel = solid_fuel_from_petroleum_gas
let rocket_fuel =
  res "Rocket Fuel" am1 30. ~allow_productivity: true
    [ 10., solid_fuel ]
let rocket_part =
  res "Rocket Part" [ maker "Rocket Silo" 1. ] 3. ~allow_productivity: true
    [ 10., low_density_structure; 10., rocket_fuel; 10., rocket_control_unit ]
let satellite =
  res "Satellite" am1 5.
    [ 100., low_density_structure; 100., solar_panel; 100., accumulator;
      5., radar; 100., processing_unit; 50., rocket_fuel ]

(* new in 0.15 *)

let nuclear_reactor =
  res "Nuclear reactor" am1 8.
    [ 500., concrete; 500., steel_plate; 500., advanced_circuit; 500., copper_plate ]
let heat_pipe =
  res "Heat pipe" am1 1.
    [ 10., steel_plate; 20., copper_plate ]
let heat_exchanger =
  res "Heat exchanger" am1 3.
    [ 10., steel_plate; 100., copper_plate; 10., pipe ]
let steam_turbine =
  res "Steam turbine" am1 3.
    [ 50., iron_gear_wheel; 50., copper_plate; 20., pipe ]
let fluid_wagon =
  res "Fluid wagon" am1 1.5
    [ 10., iron_gear_wheel; 16., steel_plate; 8., pipe; 3., storage_tank]

(* Science packs *)

let automation_science_pack =
  res "Automation Science Pack" am1 5. ~allow_productivity: true
    [ 1., copper_plate; 1., iron_gear_wheel; ]

let logistic_science_pack =
  res "Logistic Science Pack" am1 6. ~allow_productivity: true
    [ 1., inserter; 1., transport_belt; ]

(* Lava84flow: solid fuel for this one might need some fixing*)
let chemical_science_pack =
  res "Chemical Science Pack" am1 24. ~count: 2. ~allow_productivity: true
    [ 3., advanced_circuit; 2., engine_unit; 1., solid_fuel; ]

let military_science_pack =
  res "Military Science Pack" am1 10. ~count: 2. ~allow_productivity: true
    [  1., grenade; 1., piercing_rounds_magazine; 2., wall; ]

let production_science_pack =
  res "Production Science Pack" am1 21. ~count: 3. ~allow_productivity: true
    [ 1., r_electric_furnace; 1., productivity_module; 30., straight_rail; ]

let utility_science_pack =
  res "Utility Science Pack" am1 21. ~count: 3. ~allow_productivity: true
    [ 1., flying_robot_frame; 3., low_density_structure; 2., processing_unit; ]

let space_science_pack =
  (* TODO: we could replace the 1. by the time it takes to launch a rocket. *)
  res "Space Science Pack" [ rocket_silo ] 1. ~count: 1000.
    [ 100., rocket_part; 1., satellite ]

(* Armor *)

(* iron_armor; *)
(* heavy_armor; *)
let basic_modular_armor =
  res "Modular armor" am1 15.
    [ 30., advanced_circuit; 50., steel_plate ]

let power_armor =
  res "Power armor" am1 20.
    [ 40., processing_unit; 20., electric_engine_unit; 40., steel_plate ]

let power_armor_mk2 =
  res "Power armor MK2" am1 25.
    [ 25., efficiency_module_2; 25., speed_module_2; 60., processing_unit; 40., electric_engine_unit; 30., low_density_structure ]

(* Modular Armor *)

(* night_vision; *)
let personal_battery =
  res "Personal battery" am1 10.
    [ 5., battery; 10., steel_plate ]

let personal_battery_mk2 =
  res "Personal battery MK2" am1 10.
    [ 10., personal_battery; 15., processing_unit; 5., low_density_structure ]

let energy_shield =
  res "Energy shield" am1 10.
    [ 5., advanced_circuit; 10., steel_plate ]

let energy_shield_mk2 =
  res "Energy Shield MK2" am1 10.
    [ 10., energy_shield; 10., processing_unit; 5., low_density_structure ]

let portable_solar_panel =
  res "Portable solar panel" am2 10.
    [ 1., solar_panel; 2., advanced_circuit; 5., steel_plate ]

let portable_fusion_reactor =
  res "Portable fusion reactor" am1 10.
    [ 250., processing_unit; 50., low_density_structure ]

let personal_laser_defense =
  res "Personal laser defense" am1 10.
    [ 20., processing_unit; 5., low_density_structure; 5., laser_turret ]

let discharge_defense =
  res "Discharge defense" am1 10.
    [ 5., processing_unit; 20., steel_plate; 10., laser_turret ]

let basic_exoskeleton_equipment =
  res "Exoskeleton" am1 10.
    [ 10., processing_unit; 30., electric_engine_unit; 20., steel_plate ]

(* artillery shell *)
let artillery_shell =
  res "Artillery Shell" am1 15.
    [ 4., explosive_cannon_shell; 8., explosives; 1., radar ]


(******************************************************************************)
(*                              List of Resources                             *)
(******************************************************************************)

(* These are the resources that appear in the user interface. *)

let resources =
  [
    "Resources",
    [
      wood;
      coal;
      iron_ore;
      copper_ore;
      stone;
      (* raw_fish; *)
      water;
      crude_oil;
      uranium_ore;
    ];

    "Intermediate Products",
    [
      iron_plate;
      copper_plate;
      steel_plate;
      stone_brick;
      sulfur;
      plastic_bar;
      battery;
      iron_stick;
      iron_gear_wheel;
      copper_cable;
      electronic_circuit;
      advanced_circuit;
      processing_unit;
      engine_unit;
      electric_engine_unit;
      flying_robot_frame;
      empty_barrel;
      explosives;
      uranium_235;

      petroleum_gas;
      light_oil;
      heavy_oil;
      sulfuric_acid;
      lubricant;
    ];

    "Science Packs",
    [
      automation_science_pack;
      logistic_science_pack;
      chemical_science_pack;
      military_science_pack;
      production_science_pack;
      utility_science_pack;
      space_science_pack;
    ];

    "Player Equipment",
    [
      (* iron_axe; *)
      (* steel_axe; *)

      (* Weapons *)
      (* pistol; *)
      (* submachine_gun; *)
      (* rocket_launcher; *)
      (* flamethrower; *)
      land_mine;
      (* shotgun; *)
      (* combat_shotgun; *)
      grenade;
      cluster_grenade;
      defender_capsule;
      poison_capsule;
      slowdown_capsule;
      distractor_capsule;
      destroyer_capsule;
      (* basic_electric_discharge_defense_remote; *)
      atomic_bomb;

      (* Ammo *)
      firearm_magazine;
      piercing_rounds_magazine;
      shotgun_shells;
      piercing_shotgun_shells;
      rocket;
      explosive_rocket;
      flamethrower_ammo;
      cannon_shell;
      explosive_cannon_shell;
      artillery_shell;

      (* Armor *)
      (* iron_armor; *)
      (* heavy_armor; *)
      basic_modular_armor;
      power_armor;
      power_armor_mk2;

      (* Modular Armor *)
      (* night_vision; *)
      personal_battery;
      personal_battery_mk2;
      energy_shield;
      energy_shield_mk2;
      portable_solar_panel;
      portable_fusion_reactor;
      personal_laser_defense;
      discharge_defense;
      basic_exoskeleton_equipment;
    ];

    "Modules",
    [
      efficiency_module;
      efficiency_module_2;
      efficiency_module_3;
      productivity_module;
      productivity_module_2;
      productivity_module_3;
      speed_module;
      speed_module_2;
      speed_module_3;
    ];

    "Logistics",
    [
      transport_belt;
      underground_belt;
      splitter;
      fast_transport_belt;
      fast_underground_belt;
      fast_splitter;
      express_transport_belt;
      express_underground_belt;
      express_splitter;

      burner_inserter;
      inserter;
      long_handed_inserter;
      fast_inserter;
      filter_inserter;
      stack_inserter;
      stack_filter_inserter;

      wooden_chest;
      iron_chest;
      steel_chest;
      active_provider_chest;
      passive_provider_chest;
      storage_chest;
      requester_chest;

      straight_rail;
      fluid_wagon;
      (* train_stop; *)
      (* rail_signal; *)
      (* rail_chain_signal; *)
      (* diesel_locomotive; *)
      (* cargo_wagon; *)

      (* Liquid Network *)
      pipe;
      pipe_to_ground;
      storage_tank;
    ];

    "Machines & Furnaces & Pumps",
    [
      r_burner_mining_drill;
      r_electric_mining_drill;
      r_stone_furnace;
      r_steel_furnace;
      r_electric_furnace;
      r_assembling_machine_1;
      r_assembling_machine_2;
      r_assembling_machine_3;
      r_lab;
      r_centrifuge;
      r_rocket_silo;
      r_oil_refinery;
      r_chemical_plant;
      r_pumpjack;
      r_offshore_pump;
      pump;
    ];

    "Electric Network",
    [
      small_electric_pole;
      medium_electric_pole;
      big_electric_pole;
      substation;
      boiler;
      steam_engine;
      solar_panel;
      accumulator;
      lamp;
      nuclear_reactor;
      heat_pipe;
      heat_exchanger;
      steam_turbine;
    ];

    "Miscellaneous",
    [
      concrete;
      solid_fuel;
      car;
      tank;
      (* red_wire; *)
      (* green_wire; *)
      logistic_robot;
      construction_robot;
      roboport;
      (* blueprint; *)
      (* deconstruction_planner; *)

      wall;
      gate;
      gun_turret;
      flamethrower_turret;
      laser_turret;
      (* Rocket Silo *)

      beacon;
      radar;
    ];

    "Rocket Components",
    [
      low_density_structure;
      rocket_fuel;
      rocket_part;
      rocket_control_unit;
      satellite;
    ];
  ]
