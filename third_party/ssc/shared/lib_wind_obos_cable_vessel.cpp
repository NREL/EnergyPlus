#include "lib_wind_obos_cable_vessel.h"
#include <stdexcept>
#include <vector>
#include <string>
#include <iostream>

// Individual cable constructor- initialize properties
cable::cable() {
  cost              = 0.0;
  area              = 0.0;
  mass              = 0.0;
  voltage           = 0.0;
  currRating        = 0.0;
  turbInterfaceCost = 0.0;
  subsInterfaceCost = 0.0;
}

cable::cable(const cable &obj) {
  cost              = obj.cost;
  area              = obj.area;
  mass              = obj.mass;
  voltage           = obj.voltage;
  currRating        = obj.currRating;
  turbInterfaceCost = obj.turbInterfaceCost;
  subsInterfaceCost = obj.subsInterfaceCost;
}

// Cable family constructor- initiative properties
cableFamily::cableFamily() {
  voltage = 0.0;
}

cableFamily::cableFamily( const cableFamily &obj ) {
  voltage = obj.voltage;
  cables.resize( obj.cables.size() );
  for (size_t k=0; k<cables.size(); k++) cables[k] = cable(obj.cables[k]);
  initializeFlag = true;
}

// Initialize individual cable objects in cable family
void cableFamily::initialize_cables(size_t ncable) {initialize_cables((int) ncable);}
void cableFamily::initialize_cables(int ncable) {
  cables.resize( ncable );
  for (int k=0; k<ncable; k++) cables[k] = cable();
  initializeFlag = true;
}

void cableFamily::check_size(size_t nval) {
  const std::string msg = "Size mismatch: " + std::to_string(cables.size()) + " vs " + std::to_string(nval);
  if (!initializeFlag) initialize_cables(nval);
  else if (cables.size() != nval)
    throw std::invalid_argument( msg );
}

// Set properties for all cables in family
void cableFamily::set_voltage(double inVolt) {
  voltage = inVolt;
  if (initializeFlag)
    for (size_t k=0; k<cables.size(); k++) cables[k].voltage = inVolt;
}

void cableFamily::set_all_cost(std::vector<double> inVal) {
  check_size(inVal.size());
  for (size_t k=0; k<cables.size(); k++) cables[k].cost = inVal[k];
}

void cableFamily::set_all_area(std::vector<double> inVal) {
  check_size(inVal.size());
  for (size_t k=0; k<cables.size(); k++) cables[k].area = inVal[k];
}

void cableFamily::set_all_mass(std::vector<double> inVal) {
  check_size(inVal.size());
  for (size_t k=0; k<cables.size(); k++) cables[k].mass = inVal[k];
}

void cableFamily::set_all_current_rating(std::vector<double> inVal) {
  check_size(inVal.size());
  for (size_t k=0; k<cables.size(); k++) cables[k].currRating = inVal[k];
}

void cableFamily::set_all_turbine_interface_cost(std::vector<double> inVal) {
  check_size(inVal.size());
  for (size_t k=0; k<cables.size(); k++) cables[k].turbInterfaceCost = inVal[k];
}

void cableFamily::set_all_substation_interface_cost(std::vector<double> inVal) {
  check_size(inVal.size());
  for (size_t k=0; k<cables.size(); k++) cables[k].subsInterfaceCost = inVal[k];
}


// Vessel constructor- initialize properties
vessel::vessel() {
  identifier        = 0.0;
  length            = 0.0;
  breadth           = 0.0;
  draft             = 0.0;
  operational_depth = 0.0;
  leg_length        = 0.0;
  jackup_speed      = 0.0;
  deck_space        = 0.0;
  payload           = 0.0;
  lift_capacity     = 0.0;
  lift_height       = 0.0;
  transit_speed     = 0.0;
  max_wind_speed    = 0.0;
  max_wave_height   = 0.0;
  day_rate          = 0.0;
  mobilization_time = 0.0;
  number_of_vessels = 0.0;
  accomodation      = 0.0;
  crew              = 0.0;
  passengers        = 0.0;
  bollard_pull      = 0.0;
  tow_speed         = 0.0;
  carousel_weight   = 0.0;
  spud_depth        = 0.0;
  dredge_depth      = 0.0;
  bucket_size       = 0.0;
  grabber_size      = 0.0;
  hopper_size       = 0.0;
}
vessel::vessel(const vessel &obj) {
  identifier        = obj.identifier        ;
  length            = obj.length            ;
  breadth           = obj.breadth           ;
  draft             = obj.draft             ;
  operational_depth = obj.operational_depth ;
  leg_length        = obj.leg_length        ;
  jackup_speed      = obj.jackup_speed      ;
  deck_space        = obj.deck_space        ;
  payload           = obj.payload           ;
  lift_capacity     = obj.lift_capacity     ;
  lift_height       = obj.lift_height       ;
  transit_speed     = obj.transit_speed     ;
  max_wind_speed    = obj.max_wind_speed    ;
  max_wave_height   = obj.max_wave_height   ;
  day_rate          = obj.day_rate          ;
  mobilization_time = obj.mobilization_time ;
  number_of_vessels = obj.number_of_vessels ;
  accomodation      = obj.accomodation      ;
  crew              = obj.crew              ;
  passengers        = obj.passengers        ;
  bollard_pull      = obj.bollard_pull      ;
  tow_speed         = obj.tow_speed         ;
  carousel_weight   = obj.carousel_weight   ;
  spud_depth        = obj.spud_depth        ;
  dredge_depth      = obj.dredge_depth      ;
  bucket_size       = obj.bucket_size       ;
  grabber_size      = obj.grabber_size      ;
  hopper_size       = obj.hopper_size       ;
}

double vessel::get_rate() { return (day_rate * number_of_vessels); }
double vessel::get_mobilization_cost() { return (get_rate() * mobilization_time); }
