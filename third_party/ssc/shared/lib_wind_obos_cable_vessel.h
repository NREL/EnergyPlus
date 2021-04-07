#ifndef __cable_vessel_h
#define __cable_vessel_h

#include <vector>
#include <cstddef>

// Individual cable
class cable {
 public:
  double cost;
  double area;
  double mass;
  double voltage;
  double currRating;
  double turbInterfaceCost;
  double subsInterfaceCost;
  cable();
  cable(const cable &obj);
};

// Family of cables
class cableFamily {
 public:
  std::vector<cable> cables;
  double voltage;

  void set_voltage(double inVolt);
  void set_all_cost(std::vector<double> inVal);
  void set_all_area(std::vector<double> inVal);
  void set_all_mass(std::vector<double> inVal);
  void set_all_current_rating(std::vector<double> inVal);
  void set_all_turbine_interface_cost(std::vector<double> inVal);
  void set_all_substation_interface_cost(std::vector<double> inVal);
  cableFamily();
  cableFamily(const cableFamily &obj);
 private:
  bool initializeFlag = false;
  void initialize_cables(int ncable);
  void initialize_cables(size_t ncable);
  void check_size(size_t nval);
};

  
  //Vessel vector***************************************************************************************************************************************
  //vessel defaults
  /*
    turbine and substructure install vessels by identifier value
    1: leg stabilized crane vessel
    2: mid height, mid sized jack-up vessel
    3: mid height, large sized jack-up vessel
    4: high height, mid sized jack-up vessel
    5: high height, large sized jack-up vessel
    6: shear leg crane vessel
    7: derrick crane vessel
    8: semi-submersible crane vessel
    9: heavy lift cargo vessel
    10: small AHST
    11: medium AHST
    12: large AHST

    array cable install vessels by identifier value
    13: medium array cable lay barge
    14: large array cable lay barge
    15: medium array cable lay vessel
    16: large array cable lay vessel

    export cable install vessels by identifier value
    17: medium export cable lay barge
    18: large export cable lay barge
    19: medium export cable lay vessel
    20: large export cable lay vessel

    offshore substation install vessels by identifier value
    8: semi-submersible crane vessel
    9: heavy lift cargo vessel
    10: small AHST
    11: medium AHST
    12: large AHST

    feeder barges by identifier value
    21: medium jack-up barge
    22: large jack-up barge
    23: medium jack-up barge with crane
    24: large jack-up barge with crane
    25: small barge
    26: medium barge
    27: large barge

    support vessels by identifier value
    10: small AHST
    11: medium AHST
    12: large AHST
    21: medium jack-up barge
    22: large jack-up barge
    23: medium jack-up barge with crane
    24: large jack-up barge with crane
    25: small barge
    26: medium barge
    27: large barge
    28: sea going support tug
    29: hotel vessel
    30: mother ship
    31: personnel transport vessel
    32: dive support vessel
    33: guard vessel
    34: semi-submersible cargo barge
    35: semi-submersible cargo barge
    36: backhoe dredger
    37: grab or clamshell dredger
    38: fall pipe or trailing suction dredger
    39: side rock dumper vessel
    40: ballasting vessel
    41: ballast hopper vessel
    42: environmental survey vessel
    43: geophysical survey vessel
    44: geotechnical survey vessel

    vessel vector columns by index value:
    0: identifier value to/from wrapper
    1: length
    2: breadth
    3: max draft
    4: max operational water depth
    5: leg length
    6: jack up/down speed
    7: deck space
    8: max payload
    9: max lift capacity
    10: max lift height
    11: max transit speed
    12: max operational wind speed
    13: max operational wave height
    14: day rate
    15: mob/demob time
    16: number of vessels required
    17: accommodation capacity
    18: crew capacity
    19: passenger capacity
    20: bollard pull
    21: tow speed
    22: carousel weight
    23: max spud depth
    24: max dredge depth
    25: bucket size
    26: grabber size
    27: hopper size
  */

enum {LEG_STABILIZED_CRANE, // 1
      MID_HEIGHT_MID_SIZED_JACKUP, // 2
      MID_HEIGHT_LARGE_SIZED_JACKUP, // 3
      HIGH_HEIGHT_MID_SIZED_JACKUP, // 4
      HIGH_HEIGHT_LARGE_SIZED_JACKUP, // 5
      SHEAR_LEG_CRANE, // 6
      DERRICK_CRANE, // 7
      SEMISUBMERSIBLE_CRANE, // 8
      HEAVY_LIFT_CARGO, // 9
      SMALL_AHST, // 10
      MEDIUM_AHST, // 11
      LARGE_AHST, // 12
      MEDIUM_ARRAY_CABLE_LAY_BARGE, // 13
      LARGE_ARRAY_CABLE_LAY_BARGE, // 14
      MEDIUM_ARRAY_CABLE_LAY, // 15
      LARGE_ARRAY_CABLE_LAY, // 16
      MEDIUM_EXPORT_CABLE_LAY_BARGE, // 17
      LARGE_EXPORT_CABLE_LAY_BARGE, // 18
      MEDIUM_EXPORT_CABLE_LAY, // 19
      LARGE_EXPORT_CABLE_LAY, // 20
      MEDIUM_JACKUP_BARGE, // 21
      LARGE_JACKUP_BARGE, // 22
      MEDIUM_JACKUP_BARGE_WITH_CRANE, // 23
      LARGE_JACKUP_BARGE_WITH_CRANE, // 24
      SMALL_BARGE, // 25
      MEDIUM_BARGE, // 26
      LARGE_BARGE, // 27
      SEA_GOING_SUPPORT_TUG, // 28
      HOTEL, // 29
      MOTHER_SHIP, // 30
      PERSONNEL_TRANSPORT, // 31
      DIVE_SUPPORT, // 32
      GUARD, // 33
      SEMISUBMERSIBLE_CARGO_BARGE, // 34, 35
      BACKHOE_DREDGER, // 36
      GRAB_OR_CLAMSHELL_DREDGER, // 37
      FALL_PIPE_OR_TRAILING_SUCTION_DREDGER, // 38
      SIDE_ROCK_DUMPER, // 39
      BALLASTING, // 40
      BALLAST_HOPPER, // 41
      ENVIRONMENTAL_SURVEY, // 42
      GEOPHYSICAL_SURVEY, // 43
      GEOTECHNICAL_SURVEY}; // 44
  
// Individual vessel
class vessel {
 public:
  double identifier; //0: identifier value to/from wrapper
  double length;     //1: length
  double breadth;    //2: breadth
  double draft;      //3: max draft
  double operational_depth; //4: max operational water depth
  double leg_length; //5: leg length
  double jackup_speed; //6: jack up/down speed
  double deck_space; //7: deck space
  double payload; //8: max payload
  double lift_capacity; //9: max lift capacity
  double lift_height; //10: max lift height
  double transit_speed; //11: max transit speed
  double max_wind_speed; //12: max operational wind speed
  double max_wave_height; //13: max operational wave height
  double day_rate; //14: day rate
  double mobilization_time; //15: mob/demob time
  double number_of_vessels; //16: number of vessels required
  double accomodation; //17: accommodation capacity
  double crew; //18: crew capacity
  double passengers; //19: passenger capacity
  double bollard_pull; //20: bollard pull
  double tow_speed; //21: tow speed
  double carousel_weight; //22: carousel weight
  double spud_depth; //23: max spud depth
  double dredge_depth; //24: max dredge depth
  double bucket_size; //25: bucket size
  double grabber_size; //26: grabber size
  double hopper_size; //27: hopper size
  
  vessel();
  vessel(const vessel &obj);
  double get_rate();
  double get_mobilization_cost();
};
#endif
