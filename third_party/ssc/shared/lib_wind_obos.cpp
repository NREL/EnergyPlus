/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "lib_wind_obos.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>
#include <vector>
#include <set>
#include <tuple>
#include <map>
#include <string>
#include <algorithm>

#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif

#ifndef GRAVITY
#define GRAVITY 9.80633
#endif

using namespace std;

// responsibility of developers of this file to fix these warnings
#pragma warning (disable : 4458 ) // https://docs.microsoft.com/en-us/cpp/error-messages/compiler-warnings/compiler-warning-level-4-c4458?view=vs-2017


/*
// For Python wrapping with c_types
extern "C" {
  wobos* pywobos_new() {return new wobos();}
  void pywobos_run(wobos* obos) {obos->run();}
  void pywobos_set_vessel_defaults(wobos* obos) {obos->set_vessel_defaults();}
  void pywobos_map2variables(wobos* obos) {obos->map2variables();}
  void pywobos_variables2map(wobos* obos) {obos->variables2map();}
  void pywobos_set_map_variable(wobos* obos, const char* key, double val) {obos->set_map_variable(key, val);}
  double pywobos_get_map_variable(wobos* obos, const char* key) {return obos->get_map_variable(key);}
}
*/

// Default constructor loads values from text file
wobos::wobos() {
  // Set cable and vessel templates
  set_templates();
  /*
  // Load in defaults from csv-file and store locally for use in SAM and WISDEM
  wobos_default = wind_obos_defaults();

  // Store default variables locally
  for (int i=0; i<wobos_default.variables.size(); i++) {
    string keyStr = wobos_default.variables[i].name;
    string valStr = wobos_default.variables[i].valueStr;

    if ( (keyStr == "anchor") || (keyStr == "turbInstallMethod") || (keyStr == "substructure") ||
	 (keyStr == "towerInstallMethod") || (keyStr == "installStrategy") ||
	 (keyStr == "cableOptimizer") || (keyStr == "arrayCables") || (keyStr == "exportCables") ) {
      set_map_variable(keyStr, valStr);
    }
    else if (mapVars.find(keyStr) == mapVars.end()) {
      cout << "CANNOT FIND: " << keyStr << " = " << valStr << endl;
    }
    else if (wobos_default.variables[i].isDouble()) {
      set_map_variable(keyStr, wobos_default.variables[i].value);
    }
    else {
      cout << "CANNOT SET: " << keyStr << " = " << valStr << endl;
    }
  }

  // Store all doubles from map to actual class variables
  map2variables();
  */
}


// Take values in string-double map and store them in class variables.  This is useful for input from text file and external wrappings.
void wobos::map2variables() {
  // Non-double variables
  substructure = (int)mapVars["substructure"];
  anchor = (int)mapVars["anchor"];
  turbInstallMethod = (int)mapVars["turbInstallMethod"];
  towerInstallMethod = (int)mapVars["towerInstallMethod"];
  installStrategy = (int)mapVars["installStrategy"];
  cableOptimizer = (mapVars["cableOptimizer"] == 0.0) ? false : true;

  // called here to set values instead of when mapping
  set_vessel_defaults();


  // Turbine / Plant variables
  turbCapEx = mapVars["turbCapEx"];
  nTurb = mapVars["nTurb"];
  rotorD = mapVars["rotorD"];
  turbR = mapVars["turbR"];
  hubH = mapVars["hubH"];
  waterD = mapVars["waterD"];
  distShore = mapVars["distShore"];
  distPort = mapVars["distPort"];
  distPtoA = mapVars["distPtoA"];
  distAtoS = mapVars["distAtoS"];
  moorLines = mapVars["moorLines"];
  buryDepth = mapVars["buryDepth"];
  arrayY = mapVars["arrayY"];
  arrayX = mapVars["arrayX"];
  substructCont = mapVars["substructCont"];
  turbCont = mapVars["turbCont"];
  elecCont = mapVars["elecCont"];
  interConVolt = mapVars["interConVolt"];
  distInterCon = mapVars["distInterCon"];
  scrapVal = mapVars["scrapVal"];
  number_install_seasons = mapVars["number_install_seasons"];

  //General
  projLife = mapVars["projLife"];
  inspectClear = mapVars["inspectClear"];
  plantComm = mapVars["plantComm"];
  procurement_contingency = mapVars["procurement_contingency"];
  install_contingency = mapVars["install_contingency"];
  construction_insurance = mapVars["construction_insurance"];
  capital_cost_year_0 = mapVars["capital_cost_year_0"];
  capital_cost_year_1 = mapVars["capital_cost_year_1"];
  capital_cost_year_2 = mapVars["capital_cost_year_2"];
  capital_cost_year_3 = mapVars["capital_cost_year_3"];
  capital_cost_year_4 = mapVars["capital_cost_year_4"];
  capital_cost_year_5 = mapVars["capital_cost_year_5"];
  tax_rate = mapVars["tax_rate"];
  interest_during_construction = mapVars["interest_during_construction"];

  //Substructure & Foundation
  mpileCR = mapVars["mpileCR"];
  mtransCR = mapVars["mtransCR"];
  mpileD = mapVars["mpileD"];
  mpileL = mapVars["mpileL"];
  jlatticeCR = mapVars["jlatticeCR"];
  jtransCR = mapVars["jtransCR"];
  jpileCR = mapVars["jpileCR"];
  jlatticeA = mapVars["jlatticeA"];
  jpileL = mapVars["jpileL"];
  jpileD = mapVars["jpileD"];
  spStifColCR = mapVars["spStifColCR"];
  spTapColCR = mapVars["spTapColCR"];
  ballCR = mapVars["ballCR"];
  deaFixLeng = mapVars["deaFixLeng"];
  ssStifColCR = mapVars["ssStifColCR"];
  ssTrussCR = mapVars["ssTrussCR"];
  ssHeaveCR = mapVars["ssHeaveCR"];
  sSteelCR = mapVars["sSteelCR"];
  moorDia = mapVars["moorDia"];
  moorCR = mapVars["moorCR"];
  mpEmbedL = mapVars["mpEmbedL"];
  scourMat = mapVars["scourMat"];
  
  //Electrical Infrastructure
  pwrFac = mapVars["pwrFac"];
  buryFac = mapVars["buryFac"];
  arrVoltage = mapVars["arrVoltage"];
  arrCab1Size = mapVars["arrCab1Size"];
  arrCab1Mass = mapVars["arrCab1Mass"];
  cab1CurrRating = mapVars["cab1CurrRating"];
  cab1CR = mapVars["cab1CR"];
  cab1TurbInterCR = mapVars["cab1TurbInterCR"];
  arrCab2Size = mapVars["arrCab2Size"];
  arrCab2Mass = mapVars["arrCab2Mass"];
  cab2CurrRating = mapVars["cab2CurrRating"];
  cab2CR = mapVars["cab2CR"];
  cab2TurbInterCR = mapVars["cab2TurbInterCR"];
  cab2SubsInterCR = mapVars["cab2SubsInterCR"];
  catLengFac = mapVars["catLengFac"];
  exCabFac = mapVars["exCabFac"];
  subsTopFab = mapVars["subsTopFab"];
  subsTopDes = mapVars["subsTopDes"];
  topAssemblyFac = mapVars["topAssemblyFac"];
  subsJackCR = mapVars["subsJackCR"];
  subsPileCR = mapVars["subsPileCR"];
  dynCabFac = mapVars["dynCabFac"];
  shuntCR = mapVars["shuntCR"];
  highVoltSG = mapVars["highVoltSG"];
  medVoltSG = mapVars["medVoltSG"];
  backUpGen = mapVars["backUpGen"];
  workSpace = mapVars["workSpace"];
  otherAncillary = mapVars["otherAncillary"];
  mptCR = mapVars["mptCR"];
  expVoltage = mapVars["expVoltage"];
  expCabSize = mapVars["expCabSize"];
  expCabMass = mapVars["expCabMass"];
  expCabCR = mapVars["expCabCR"];
  expCurrRating = mapVars["expCurrRating"];
  expSubsInterCR = mapVars["expSubsInterCR"];
	
  //Assembly & Installation
  moorTimeFac = mapVars["moorTimeFac"];
  moorLoadout = mapVars["moorLoadout"];
  moorSurvey = mapVars["moorSurvey"];
  prepAA = mapVars["prepAA"];
  prepSpar = mapVars["prepSpar"];
  upendSpar = mapVars["upendSpar"];
  prepSemi = mapVars["prepSemi"];
  turbFasten = mapVars["turbFasten"];
  boltTower = mapVars["boltTower"];
  boltNacelle1 = mapVars["boltNacelle1"];
  boltNacelle2 = mapVars["boltNacelle2"];
  boltNacelle3 = mapVars["boltNacelle3"];
  boltBlade1 = mapVars["boltBlade1"];
  boltBlade2 = mapVars["boltBlade2"];
  boltRotor = mapVars["boltRotor"];
  vesselPosTurb = mapVars["vesselPosTurb"];
  vesselPosJack = mapVars["vesselPosJack"];
  vesselPosMono = mapVars["vesselPosMono"];
  subsVessPos = mapVars["subsVessPos"];
  monoFasten = mapVars["monoFasten"];
  jackFasten = mapVars["jackFasten"];
  prepGripperMono = mapVars["prepGripperMono"];
  prepGripperJack = mapVars["prepGripperJack"];
  placePiles = mapVars["placePiles"];
  prepHamMono = mapVars["prepHamMono"];
  removeHamMono = mapVars["removeHamMono"];
  prepHamJack = mapVars["prepHamJack"];
  removeHamJack = mapVars["removeHamJack"];
  placeJack = mapVars["placeJack"];
  levJack = mapVars["levJack"];
  placeTemplate = mapVars["placeTemplate"];
  hamRate = mapVars["hamRate"];
  placeMP = mapVars["placeMP"];
  instScour = mapVars["instScour"];
  placeTP = mapVars["placeTP"];
  groutTP = mapVars["groutTP"];
  tpCover = mapVars["tpCover"];
  prepTow = mapVars["prepTow"];
  spMoorCon = mapVars["spMoorCon"];
  ssMoorCon = mapVars["ssMoorCon"];
  spMoorCheck = mapVars["spMoorCheck"];
  ssMoorCheck = mapVars["ssMoorCheck"];
  ssBall = mapVars["ssBall"];
  surfLayRate = mapVars["surfLayRate"];
  cabPullIn = mapVars["cabPullIn"];
  cabTerm = mapVars["cabTerm"];
  cabLoadout = mapVars["cabLoadout"];
  buryRate = mapVars["buryRate"];
  subsPullIn = mapVars["subsPullIn"];
  shorePullIn = mapVars["shorePullIn"];
  landConstruct = mapVars["landConstruct"];
  expCabLoad = mapVars["expCabLoad"];
  subsLoad = mapVars["subsLoad"];
  placeTop = mapVars["placeTop"];
  pileSpreadDR = mapVars["pileSpreadDR"];
  pileSpreadMob = mapVars["pileSpreadMob"];
  groutSpreadDR = mapVars["groutSpreadDR"];
  groutSpreadMob = mapVars["groutSpreadMob"];
  seaSpreadDR = mapVars["seaSpreadDR"];
  seaSpreadMob = mapVars["seaSpreadMob"];
  compRacks = mapVars["compRacks"];
  cabSurveyCR = mapVars["cabSurveyCR"];
  cabDrillDist = mapVars["cabDrillDist"];
  cabDrillCR = mapVars["cabDrillCR"];
  mpvRentalDR = mapVars["mpvRentalDR"];
  diveTeamDR = mapVars["diveTeamDR"];
  winchDR = mapVars["winchDR"];
  civilWork = mapVars["civilWork"];
  elecWork = mapVars["elecWork"];
	
  //Port & Staging
  nCrane600 = mapVars["nCrane600"];
  nCrane1000 = mapVars["nCrane1000"];
  crane600DR = mapVars["crane600DR"];
  crane1000DR = mapVars["crane1000DR"];
  craneMobDemob = mapVars["craneMobDemob"];
  entranceExitRate = mapVars["entranceExitRate"];
  dockRate = mapVars["dockRate"];
  wharfRate = mapVars["wharfRate"];
  laydownCR = mapVars["laydownCR"];
	
  //Engineering & Management
  estEnMFac = mapVars["estEnMFac"];
	
  //Development
  preFEEDStudy = mapVars["preFEEDStudy"];
  feedStudy = mapVars["feedStudy"];
  stateLease = mapVars["stateLease"];
  outConShelfLease = mapVars["outConShelfLease"];
  saPlan = mapVars["saPlan"];
  conOpPlan = mapVars["conOpPlan"];
  nepaEisMet = mapVars["nepaEisMet"];
  physResStudyMet = mapVars["physResStudyMet"];
  bioResStudyMet = mapVars["bioResStudyMet"];
  socEconStudyMet = mapVars["socEconStudyMet"];
  navStudyMet = mapVars["navStudyMet"];
  nepaEisProj = mapVars["nepaEisProj"];
  physResStudyProj = mapVars["physResStudyProj"];
  bioResStudyProj = mapVars["bioResStudyProj"];
  socEconStudyProj = mapVars["socEconStudyProj"];
  navStudyProj = mapVars["navStudyProj"];
  coastZoneManAct = mapVars["coastZoneManAct"];
  rivsnHarbsAct = mapVars["rivsnHarbsAct"];
  cleanWatAct402 = mapVars["cleanWatAct402"];
  cleanWatAct404 = mapVars["cleanWatAct404"];
  faaPlan = mapVars["faaPlan"];
  endSpecAct = mapVars["endSpecAct"];
  marMamProtAct = mapVars["marMamProtAct"];
  migBirdAct = mapVars["migBirdAct"];
  natHisPresAct = mapVars["natHisPresAct"];
  addLocPerm = mapVars["addLocPerm"];
  metTowCR = mapVars["metTowCR"];
  decomDiscRate = mapVars["decomDiscRate"];

  
  // INPUTS OR OUTPUTS
  // Inputs if running connected to other modules in WISDEM
  // Outputs if running isolated
  
  // Turbine outputs
  hubD = mapVars["hubD"];
  bladeL = mapVars["bladeL"];
  chord = mapVars["chord"];
  nacelleW = mapVars["nacelleW"];
  nacelleL = mapVars["nacelleL"];
  rnaM = mapVars["rnaM"];
  towerD = mapVars["towerD"];
  towerM = mapVars["towerM"];

  //Substructure & Foundation outputs
  subTotM = mapVars["subTotM"];
  subTotCost = mapVars["subTotCost"];
  moorCost = mapVars["moorCost"];
  
  // OUTPUTS

  //Electrical Infrastructure outputs
  systAngle = mapVars["systAngle"];
  freeCabLeng = mapVars["freeCabLeng"];
  fixCabLeng = mapVars["fixCabLeng"];
  nExpCab = mapVars["nExpCab"];
  expCabLeng = mapVars["expCabLeng"];
  expCabCost = mapVars["expCabCost"];
  nSubstation = mapVars["nSubstation"];
  cab1Leng = mapVars["cab1Leng"];
  cab2Leng = mapVars["cab2Leng"];
  arrCab1Cost = mapVars["arrCab1Cost"];
  arrCab2Cost = mapVars["arrCab2Cost"];
  subsSubM = mapVars["subsSubM"];
  subsPileM = mapVars["subsPileM"];
  subsTopM = mapVars["subsTopM"];
  totElecCost = mapVars["totElecCost"];
  
  //Assembly & Installation outputs
  moorTime = mapVars["moorTime"];
  floatPrepTime = mapVars["floatPrepTime"];
  turbDeckArea = mapVars["turbDeckArea"];
  nTurbPerTrip = mapVars["nTurbPerTrip"];
  turbInstTime = mapVars["turbInstTime"];
  subDeckArea = mapVars["subDeckArea"];
  nSubPerTrip = mapVars["nSubPerTrip"];
  subInstTime = mapVars["subInstTime"];
  arrInstTime = mapVars["arrInstTime"];
  expInstTime = mapVars["expInstTime"];
  subsInstTime = mapVars["subsInstTime"];
  totInstTime = mapVars["totInstTime"];
  cabSurvey = mapVars["cabSurvey"];
  array_cable_install_cost = mapVars["array_cable_install_cost"];
  export_cable_install_cost = mapVars["export_cable_install_cost"];
  substation_install_cost = mapVars["substation_install_cost"];
  turbine_install_cost = mapVars["turbine_install_cost"];
  substructure_install_cost = mapVars["substructure_install_cost"];
  electrical_install_cost = mapVars["electrical_install_cost"];
  mob_demob_cost = mapVars["mob_demob_cost"];

  //Port & Staging outputs
  totPnSCost = mapVars["totPnSCost"];

  //Development outputs
  totDevCost = mapVars["totDevCost"];

  // Main Cost Outputs
  bos_capex = mapVars["bos_capex"];
  construction_insurance_cost = mapVars["construction_insurance_cost"];
  total_contingency_cost = mapVars["total_contingency_cost"];
  construction_finance_cost = mapVars["construction_finance_cost"];
  construction_finance_factor = mapVars["construction_finance_factor"];
  soft_costs = mapVars["soft_costs"];
  totAnICost = mapVars["totAnICost"];
  totEnMCost = mapVars["totEnMCost"];
  commissioning = mapVars["commissioning"];
  decomCost = mapVars["decomCost"];
  total_bos_cost = mapVars["total_bos_cost"];

}


void wobos::variables2map() {
  // Non-double variables
  mapVars["substructure"]       = (double)substructure;
  mapVars["anchor"]             = (double)anchor;
  mapVars["turbInstallMethod"]  = (double)turbInstallMethod;
  mapVars["towerInstallMethod"] = (double)towerInstallMethod;
  mapVars["installStrategy"]    = (double)installStrategy;
  mapVars["cableOptimizer"]     = (cableOptimizer) ? 1.0 : 0.0;

  // Turbine / Plant variables
  mapVars["turbCapEx"] = turbCapEx;
  mapVars["nTurb"] = nTurb;
  mapVars["rotorD"] = rotorD;
  mapVars["turbR"] = turbR;
  mapVars["hubH"] = hubH;
  mapVars["waterD"] = waterD;
  mapVars["distShore"] = distShore;
  mapVars["distPort"] = distPort;
  mapVars["distPtoA"] = distPtoA;
  mapVars["distAtoS"] = distAtoS;
  mapVars["moorLines"] = moorLines;
  mapVars["buryDepth"] = buryDepth;
  mapVars["arrayY"] = arrayY;
  mapVars["arrayX"] = arrayX;
  mapVars["substructCont"] = substructCont;
  mapVars["turbCont"] = turbCont;
  mapVars["elecCont"] = elecCont;
  mapVars["interConVolt"] = interConVolt;
  mapVars["distInterCon"] = distInterCon;
  mapVars["scrapVal"] = scrapVal;
  mapVars["number_install_seasons"] = number_install_seasons;

  //General
  mapVars["projLife"] = projLife;
  mapVars["inspectClear"] = inspectClear;
  mapVars["plantComm"] = plantComm;
  mapVars["procurement_contingency"] = procurement_contingency;
  mapVars["install_contingency"] = install_contingency;
  mapVars["construction_insurance"] = construction_insurance;
  mapVars["capital_cost_year_0"] = capital_cost_year_0;
  mapVars["capital_cost_year_1"] = capital_cost_year_1;
  mapVars["capital_cost_year_2"] = capital_cost_year_2;
  mapVars["capital_cost_year_3"] = capital_cost_year_3;
  mapVars["capital_cost_year_4"] = capital_cost_year_4;
  mapVars["capital_cost_year_5"] = capital_cost_year_5;
  mapVars["tax_rate"] = tax_rate;
  mapVars["interest_during_construction"] = interest_during_construction;

  //Substructure & Foundation
  mapVars["mpileCR"] = mpileCR;
  mapVars["mtransCR"] = mtransCR;
  mapVars["mpileD"] = mpileD;
  mapVars["mpileL"] = mpileL;
  mapVars["jlatticeCR"] = jlatticeCR;
  mapVars["jtransCR"] = jtransCR;
  mapVars["jpileCR"] = jpileCR;
  mapVars["jlatticeA"] = jlatticeA;
  mapVars["jpileL"] = jpileL;
  mapVars["jpileD"] = jpileD;
  mapVars["spStifColCR"] = spStifColCR;
  mapVars["spTapColCR"] = spTapColCR;
  mapVars["ballCR"] = ballCR;
  mapVars["deaFixLeng"] = deaFixLeng;
  mapVars["ssStifColCR"] = ssStifColCR;
  mapVars["ssTrussCR"] = ssTrussCR;
  mapVars["ssHeaveCR"] = ssHeaveCR;
  mapVars["sSteelCR"] = sSteelCR;
  mapVars["moorDia"] = moorDia;
  mapVars["moorCR"] = moorCR;
  mapVars["mpEmbedL"] = mpEmbedL;
  mapVars["scourMat"] = scourMat;
  
  //Electrical Infrastructure
  mapVars["pwrFac"] = pwrFac;
  mapVars["buryFac"] = buryFac;
  mapVars["arrVoltage"] = arrVoltage;
  mapVars["arrCab1Size"] = arrCab1Size;
  mapVars["arrCab1Mass"] = arrCab1Mass;
  mapVars["cab1CurrRating"] = cab1CurrRating;
  mapVars["cab1CR"] = cab1CR;
  mapVars["cab1TurbInterCR"] = cab1TurbInterCR;
  mapVars["arrCab2Size"] = arrCab2Size;
  mapVars["arrCab2Mass"] = arrCab2Mass;
  mapVars["cab2CurrRating"] = cab2CurrRating;
  mapVars["cab2CR"] = cab2CR;
  mapVars["cab2TurbInterCR"] = cab2TurbInterCR;
  mapVars["cab2SubsInterCR"] = cab2SubsInterCR;
  mapVars["catLengFac"] = catLengFac;
  mapVars["exCabFac"] = exCabFac;
  mapVars["subsTopFab"] = subsTopFab;
  mapVars["subsTopDes"] = subsTopDes;
  mapVars["topAssemblyFac"] = topAssemblyFac;
  mapVars["subsJackCR"] = subsJackCR;
  mapVars["subsPileCR"] = subsPileCR;
  mapVars["dynCabFac"] = dynCabFac;
  mapVars["shuntCR"] = shuntCR;
  mapVars["highVoltSG"] = highVoltSG;
  mapVars["medVoltSG"] = medVoltSG;
  mapVars["backUpGen"] = backUpGen;
  mapVars["workSpace"] = workSpace;
  mapVars["otherAncillary"] = otherAncillary;
  mapVars["mptCR"] = mptCR;
  mapVars["expVoltage"] = expVoltage;
  mapVars["expCabSize"] = expCabSize;
  mapVars["expCabMass"] = expCabMass;
  mapVars["expCabCR"] = expCabCR;
  mapVars["expCurrRating"] = expCurrRating;
  mapVars["expSubsInterCR"] = expSubsInterCR;
	
  //Assembly & Installation
  mapVars["moorTimeFac"] = moorTimeFac;
  mapVars["moorLoadout"] = moorLoadout;
  mapVars["moorSurvey"] = moorSurvey;
  mapVars["prepAA"] = prepAA;
  mapVars["prepSpar"] = prepSpar;
  mapVars["upendSpar"] = upendSpar;
  mapVars["prepSemi"] = prepSemi;
  mapVars["turbFasten"] = turbFasten;
  mapVars["boltTower"] = boltTower;
  mapVars["boltNacelle1"] = boltNacelle1;
  mapVars["boltNacelle2"] = boltNacelle2;
  mapVars["boltNacelle3"] = boltNacelle3;
  mapVars["boltBlade1"] = boltBlade1;
  mapVars["boltBlade2"] = boltBlade2;
  mapVars["boltRotor"] = boltRotor;
  mapVars["vesselPosTurb"] = vesselPosTurb;
  mapVars["vesselPosJack"] = vesselPosJack;
  mapVars["vesselPosMono"] = vesselPosMono;
  mapVars["subsVessPos"] = subsVessPos;
  mapVars["monoFasten"] = monoFasten;
  mapVars["jackFasten"] = jackFasten;
  mapVars["prepGripperMono"] = prepGripperMono;
  mapVars["prepGripperJack"] = prepGripperJack;
  mapVars["placePiles"] = placePiles;
  mapVars["prepHamMono"] = prepHamMono;
  mapVars["removeHamMono"] = removeHamMono;
  mapVars["prepHamJack"] = prepHamJack;
  mapVars["removeHamJack"] = removeHamJack;
  mapVars["placeJack"] = placeJack;
  mapVars["levJack"] = levJack;
  mapVars["placeTemplate"] = placeTemplate;
  mapVars["hamRate"] = hamRate;
  mapVars["placeMP"] = placeMP;
  mapVars["instScour"] = instScour;
  mapVars["placeTP"] = placeTP;
  mapVars["groutTP"] = groutTP;
  mapVars["tpCover"] = tpCover;
  mapVars["prepTow"] = prepTow;
  mapVars["spMoorCon"] = spMoorCon;
  mapVars["ssMoorCon"] = ssMoorCon;
  mapVars["spMoorCheck"] = spMoorCheck;
  mapVars["ssMoorCheck"] = ssMoorCheck;
  mapVars["ssBall"] = ssBall;
  mapVars["surfLayRate"] = surfLayRate;
  mapVars["cabPullIn"] = cabPullIn;
  mapVars["cabTerm"] = cabTerm;
  mapVars["cabLoadout"] = cabLoadout;
  mapVars["buryRate"] = buryRate;
  mapVars["subsPullIn"] = subsPullIn;
  mapVars["shorePullIn"] = shorePullIn;
  mapVars["landConstruct"] = landConstruct;
  mapVars["expCabLoad"] = expCabLoad;
  mapVars["subsLoad"] = subsLoad;
  mapVars["placeTop"] = placeTop;
  mapVars["pileSpreadDR"] = pileSpreadDR;
  mapVars["pileSpreadMob"] = pileSpreadMob;
  mapVars["groutSpreadDR"] = groutSpreadDR;
  mapVars["groutSpreadMob"] = groutSpreadMob;
  mapVars["seaSpreadDR"] = seaSpreadDR;
  mapVars["seaSpreadMob"] = seaSpreadMob;
  mapVars["compRacks"] = compRacks;
  mapVars["cabSurveyCR"] = cabSurveyCR;
  mapVars["cabDrillDist"] = cabDrillDist;
  mapVars["cabDrillCR"] = cabDrillCR;
  mapVars["mpvRentalDR"] = mpvRentalDR;
  mapVars["diveTeamDR"] = diveTeamDR;
  mapVars["winchDR"] = winchDR;
  mapVars["civilWork"] = civilWork;
  mapVars["elecWork"] = elecWork;
	
  //Port & Staging
  mapVars["nCrane600"] = nCrane600;
  mapVars["nCrane1000"] = nCrane1000;
  mapVars["crane600DR"] = crane600DR;
  mapVars["crane1000DR"] = crane1000DR;
  mapVars["craneMobDemob"] = craneMobDemob;
  mapVars["entranceExitRate"] = entranceExitRate;
  mapVars["dockRate"] = dockRate;
  mapVars["wharfRate"] = wharfRate;
  mapVars["laydownCR"] = laydownCR;
	
  //Engineering & Management
  mapVars["estEnMFac"] = estEnMFac;
	
  //Development
  mapVars["preFEEDStudy"] = preFEEDStudy;
  mapVars["feedStudy"] = feedStudy;
  mapVars["stateLease"] = stateLease;
  mapVars["outConShelfLease"] = outConShelfLease;
  mapVars["saPlan"] = saPlan;
  mapVars["conOpPlan"] = conOpPlan;
  mapVars["nepaEisMet"] = nepaEisMet;
  mapVars["physResStudyMet"] = physResStudyMet;
  mapVars["bioResStudyMet"] = bioResStudyMet;
  mapVars["socEconStudyMet"] = socEconStudyMet;
  mapVars["navStudyMet"] = navStudyMet;
  mapVars["nepaEisProj"] = nepaEisProj;
  mapVars["physResStudyProj"] = physResStudyProj;
  mapVars["bioResStudyProj"] = bioResStudyProj;
  mapVars["socEconStudyProj"] = socEconStudyProj;
  mapVars["navStudyProj"] = navStudyProj;
  mapVars["coastZoneManAct"] = coastZoneManAct;
  mapVars["rivsnHarbsAct"] = rivsnHarbsAct;
  mapVars["cleanWatAct402"] = cleanWatAct402;
  mapVars["cleanWatAct404"] = cleanWatAct404;
  mapVars["faaPlan"] = faaPlan;
  mapVars["endSpecAct"] = endSpecAct;
  mapVars["marMamProtAct"] = marMamProtAct;
  mapVars["migBirdAct"] = migBirdAct;
  mapVars["natHisPresAct"] = natHisPresAct;
  mapVars["addLocPerm"] = addLocPerm;
  mapVars["metTowCR"] = metTowCR;
  mapVars["decomDiscRate"] = decomDiscRate;
  
  // INPUTS or OUTPUTS
  // Inputs if running connected to other modules in WISDEM
  // Outputs if running isolated

  // Turbine outputs
  mapVars["hubD"] = hubD;
  mapVars["bladeL"] = bladeL;
  mapVars["chord"] = chord;
  mapVars["nacelleW"] = nacelleW;
  mapVars["nacelleL"] = nacelleL;
  mapVars["rnaM"] = rnaM;
  mapVars["towerD"] = towerD;
  mapVars["towerM"] = towerM;

  //Substructure & Foundation outputs
  mapVars["subTotM"] = subTotM;
  mapVars["subTotCost"] = subTotCost;
  mapVars["moorCost"] = moorCost;
  
  // OUTPUTS
  //Electrical Infrastructure outputs
  mapVars["systAngle"] = systAngle;
  mapVars["freeCabLeng"] = freeCabLeng;
  mapVars["fixCabLeng"] = fixCabLeng;
  mapVars["nExpCab"] = nExpCab;
  mapVars["expCabLeng"] = expCabLeng;
  mapVars["expCabCost"] = expCabCost;
  mapVars["nSubstation"] = nSubstation;
  mapVars["cab1Leng"] = cab1Leng;
  mapVars["cab2Leng"] = cab2Leng;
  mapVars["arrCab1Cost"] = arrCab1Cost;
  mapVars["arrCab2Cost"] = arrCab2Cost;
  mapVars["subsSubM"] = subsSubM;
  mapVars["subsPileM"] = subsPileM;
  mapVars["subsTopM"] = subsTopM;
  mapVars["totElecCost"] = totElecCost;
  
  //Assembly & Installation outputs
  mapVars["moorTime"] = moorTime;
  mapVars["floatPrepTime"] = floatPrepTime;
  mapVars["turbDeckArea"] = turbDeckArea;
  mapVars["nTurbPerTrip"] = nTurbPerTrip;
  mapVars["turbInstTime"] = turbInstTime;
  mapVars["subDeckArea"] = subDeckArea;
  mapVars["nSubPerTrip"] = nSubPerTrip;
  mapVars["subInstTime"] = subInstTime;
  mapVars["arrInstTime"] = arrInstTime;
  mapVars["expInstTime"] = expInstTime;
  mapVars["subsInstTime"] = subsInstTime;
  mapVars["totInstTime"] = totInstTime;
  mapVars["cabSurvey"] = cabSurvey;
  mapVars["array_cable_install_cost"] = array_cable_install_cost;
  mapVars["export_cable_install_cost"] = export_cable_install_cost;
  mapVars["substation_install_cost"] = substation_install_cost;
  mapVars["turbine_install_cost"] = turbine_install_cost;
  mapVars["substructure_install_cost"] = substructure_install_cost;
  mapVars["electrical_install_cost"] = electrical_install_cost;
  mapVars["mob_demob_cost"] = mob_demob_cost;

  //Port & Staging outputs
  mapVars["totPnSCost"] = totPnSCost;

  //Development outputs
  mapVars["totDevCost"] = totDevCost;

  // Main Cost Outputs
  mapVars["bos_capex"] = bos_capex;
  mapVars["construction_insurance_cost"] = construction_insurance_cost;
  mapVars["total_contingency_cost"] = total_contingency_cost;
  mapVars["construction_finance_cost"] = construction_finance_cost;
  mapVars["construction_finance_factor"] = construction_finance_factor;
  mapVars["soft_costs"] = soft_costs;
  mapVars["totAnICost"] = totAnICost;
  mapVars["totEnMCost"] = totEnMCost;
  mapVars["commissioning"] = commissioning;
  mapVars["decomCost"] = decomCost;
  mapVars["total_bos_cost"] = total_bos_cost;
}

void wobos::set_map_variable(string keyStr, string valStr) {
  if (keyStr == "substructure") {
    substructure = str2substructure[valStr];
    mapVars[keyStr] = (double)substructure;
    set_vessel_defaults();
    // TODO- if vessels are specified in the text file, this will have to be done before those are read
  }
  else if (keyStr == "anchor") {
    anchor = str2anchor[valStr];
    mapVars[keyStr] = (double)anchor;
  }
  else if (keyStr == "turbInstallMethod") {
    turbInstallMethod = str2turbInstallMethod[valStr];
    mapVars[keyStr] = (double)turbInstallMethod;
  }
  else if (keyStr == "towerInstallMethod") {
    towerInstallMethod = str2towerInstallMethod[valStr];
    mapVars[keyStr] = (double)towerInstallMethod;
  }
  else if (keyStr == "installStrategy") {
    installStrategy = str2installStrategy[valStr];
    mapVars[keyStr] = (double)installStrategy;
  }
  else if (keyStr == "cableOptimizer") {
    cableOptimizer = ((valStr=="FALSE") || (valStr=="0")) ? false : true;
    mapVars[keyStr] = (cableOptimizer) ? 1.0 : 0.0;
  }
  else if ( (keyStr == "arrayCables") || (keyStr == "exportCables") ) {
    vector<int> cableVoltages;
    stringstream iss( valStr );
    int temp;
    while ( iss >> temp ) {
      cableVoltages.push_back( temp );
      if (iss.peek() == ' ') iss.ignore();
    }
    if (keyStr == "arrayCables") arrCables = set_cables(cableVoltages);
    else expCables = set_cables(cableVoltages);
  }
}
void wobos::set_map_variable(string keyStr, double val) {
  if ( (val > 1.0) && (variable_percentage.find(keyStr) != variable_percentage.end()) )
	val *= 1e-2;

  mapVars[keyStr] = val;
}
void wobos::set_map_variable(const char* key, double val) {set_map_variable(string(key), val);}
double wobos::get_map_variable(const char* key) {return mapVars[string(key)];}


void wobos::set_templates() {
  cableFamily arrayCable33kV = cableFamily();
  arrayCable33kV.set_all_area( {95.0,   120.0,  150.0,  185.0,  240.0,  300.0,  400.0,  500.0,  630.0,  800.0,  1000.0} );
  arrayCable33kV.set_all_mass( {20.384, 21.854, 23.912, 25.676, 28.910, 32.242, 37.142, 42.336, 48.706, 57.428, 66.738} );
  arrayCable33kV.set_all_cost( {185.889, 202.788, 208.421, 236.586, 270.384, 315.448, 360.512, 422.475, 478.805, 585.832, 698.492} );
  arrayCable33kV.set_all_current_rating( {300.0,  340.0,  375.0,  420.0,  480.0,  530.0,  590.0,  655.0,  715.0,  775.0,  825.0} );
  arrayCable33kV.set_all_turbine_interface_cost( {8410., 8615., 8861., 9149., 9600., 10092., 10913., 11733., 12800., 14195., 15836.} );
  arrayCable33kV.set_all_substation_interface_cost( {19610., 19815., 20062., 20349., 20800., 21292., 22113., 22933., 24000., 25395., 27036.} );
  arrayCable33kV.set_voltage( 33.0 );
  arrayTemplates.insert( make_pair(33, arrayCable33kV) );

  cableFamily arrayCable66kV = cableFamily();
  arrayCable66kV.set_all_area( {95.0,   120.0,  150.0,  185.0,  240.0,  300.0,  400.0,  500.0,  630.0,  800.0,  1000.0} );
  arrayCable66kV.set_all_mass( {21.6, 23.8, 25.7, 28.0, 31.3, 34.3, 39.2, 45.4, 52.0, 60.1, 70.7} );
  arrayCable66kV.set_all_cost( {225.320, 242.219, 253.485, 281.650, 326.714, 383.044, 433.741, 506.970, 574.566, 704.125, 844.950} );
  arrayCable66kV.set_all_current_rating( {300., 340., 375., 420., 480., 530., 590., 655., 715., 775., 825.} );
  arrayCable66kV.set_all_turbine_interface_cost( {8830.50, 9045.75, 9304.05, 9606.45, 10080.00, 10596.60, 11458.65, 12319.65, 13440.00, 14904.75, 16627.80} );
  arrayCable66kV.set_all_substation_interface_cost( {20590.50, 20805.75, 21065.10, 21366.45, 21840.00, 22356.60, 23218.65, 24079.65, 25200.00, 26664.75, 28387.80} );
  arrayCable66kV.set_voltage( 66.0 );
  arrayTemplates.insert( make_pair(66, arrayCable66kV) );

  cableFamily exportCable132kV = cableFamily();
  exportCable132kV.set_all_area( {300.0, 400.0, 500.0, 630.0, 800.0, 1000.0, 1200.0, 1600.0, 2000.0, 2500.0} );
  exportCable132kV.set_all_mass( {48.000, 51.100, 58.000, 65.200, 74.000, 85.400, 113.147, 131.387, 149.627, 172.427} );
  exportCable132kV.set_all_cost({433.504, 520.489, 596.388, 689.479, 843.823, 1006.054, 1168.284, 1492.745, 1818.332, 2223.908} );
  exportCable132kV.set_all_current_rating( {530., 590., 655., 715., 775., 825., 990., 1061., 1299., 1375} );
  exportCable132kV.set_all_substation_interface_cost( {57500., 60000., 62500., 65000., 67500., 70000., 72500., 75000., 77500., 80000} );
  exportCable132kV.set_voltage( 132.0 );
  arrayTemplates.insert( make_pair(132, exportCable132kV) );

  cableFamily exportCable220kV = cableFamily();
  exportCable220kV.set_all_area( {300.0, 400.0, 500.0, 630.0, 800.0, 1000.0, 1200.0, 1600.0, 2000.0, 2500.0} );
  exportCable220kV.set_all_mass( {71.900, 76.500, 81.300, 86.700, 95.300, 104.000, 113.147, 131.387, 149.627, 172.427} );
  exportCable220kV.set_all_cost( {495.411, 578.187, 681.863, 788.620, 966.623, 1159.271, 1336.148, 1676.499, 2042.784, 2498.703} );
  exportCable220kV.set_all_current_rating( {530., 590., 655., 715., 775., 825., 960., 1025., 1181., 1248} );
  exportCable220kV.set_all_substation_interface_cost( {57500., 60000., 62500., 65000., 67500., 70000., 72500., 75000., 77500., 80000} );
  exportCable220kV.set_voltage( 220.0 );
  arrayTemplates.insert( make_pair(220, exportCable220kV) );

  vessel id31 = vessel();
  id31.identifier = PERSONNEL_TRANSPORT; // 31
  id31.length = 19;
  id31.draft = 1.5;
  id31.transit_speed = 23;
  id31.max_wind_speed = 15;
  id31.max_wave_height = 1.75;
  id31.day_rate = 3000;
  id31.mobilization_time = 1;
  id31.number_of_vessels = 1;
  id31.crew = 3;
  id31.passengers = 13;
  vesselTemplates.insert( make_pair("PERSONNEL_TRANSPORT", id31) );
  
  vessel id33 = vessel();
  id33.identifier = GUARD; // 33
  id33.length = 20.6;
  id33.breadth = 5.4;
  id33.draft = 3.1;
  id33.transit_speed = 15;
  id33.max_wave_height = 1.75;
  id33.day_rate = 3000;
  id33.mobilization_time = 1;
  id33.number_of_vessels = 1;
  vesselTemplates.insert( make_pair("GUARD", id33) );

  vessel id12 = vessel();
  id12.identifier = LARGE_AHST; // 12;
  id12.length = 87;
  id12.breadth = 20.9;
  id12.draft = 7.4;
  id12.operational_depth = 0;
  id12.deck_space = 637;
  id12.payload = 1440;
  id12.transit_speed = 15.2;
  id12.max_wave_height = 2.0;
  id12.day_rate = 90000;
  id12.mobilization_time = 3;
  id12.number_of_vessels = 1;
  id12.bollard_pull = 237;
  id12.tow_speed = 5;
  vesselTemplates.insert( make_pair("LARGE_AHST", id12) );

  vessel id11 = vessel();
  id11.identifier = MEDIUM_AHST; // 11;
  id11.length = 68.2;
  id11.breadth = 15.9;
  id11.draft = 6.3;
  id11.operational_depth = 0;
  id11.deck_space = 441;
  id11.payload = 738;
  id11.transit_speed = 12.7;
  id11.max_wave_height = 2.0;
  id11.day_rate = 60000;
  id11.mobilization_time = 3;
  id11.number_of_vessels = 1;
  id11.bollard_pull = 152;
  id11.tow_speed = 5;
  vesselTemplates.insert( make_pair("MEDIUM_AHST", id11) );

  vessel id28 = vessel();
  id28.identifier = SEA_GOING_SUPPORT_TUG; // 28;
  id28.length = 28.2;
  id28.breadth = 8.8;
  id28.draft = 3.1;
  id28.deck_space = 39.3;
  id28.payload = 8;
  id28.transit_speed = 9.6;
  id28.max_wave_height = 2.5;
  id28.day_rate = 27500;
  id28.mobilization_time = 1;
  id28.number_of_vessels = 2;
  id28.bollard_pull = 37;
  vesselTemplates.insert( make_pair("SEA_GOING_SUPPORT_TUG", id28) );

  vessel id22 = vessel();
  id22.identifier = LARGE_JACKUP_BARGE; // 22;
  id22.length = 75.6;
  id22.breadth = 32.5;
  id22.draft = 4.2;
  id22.operational_depth = 32.5;
  id22.leg_length = 50.4;
  id22.jackup_speed = 0.9;
  id22.deck_space = 2000;
  id22.payload = 1930;
  id22.lift_capacity = 510;
  id22.lift_height = 89.5;
  id22.transit_speed = 4;
  id22.max_wind_speed = 10;
  id22.max_wave_height = 2;
  id22.day_rate = 70000;
  id22.mobilization_time = 3;
  id22.number_of_vessels = 1;
  vesselTemplates.insert( make_pair("LARGE_JACKUP_BARGE", id22) );
    
  vessel id5 = vessel();
  id5.identifier = HIGH_HEIGHT_LARGE_SIZED_JACKUP; // 5
  id5.length = 150.3;
  id5.breadth = 47;
  id5.draft = 6.2;
  id5.operational_depth = 57.8;
  id5.leg_length = 99;
  id5.jackup_speed = 1.2;
  id5.deck_space = 5030;
  id5.payload = 8000;
  id5.lift_capacity = 1375;
  id5.lift_height = 105;
  id5.transit_speed = 12.0;
  id5.max_wind_speed = 18.0;
  id5.max_wave_height = 2.7;
  id5.day_rate = 242000;
  id5.mobilization_time = 3;
  id5.number_of_vessels = 1;
  vesselTemplates.insert( make_pair("HIGH_HEIGHT_LARGE_SIZED_JACKUP", id5) );

  vessel id8 = vessel();
  id8.identifier = SEMISUBMERSIBLE_CRANE; //8
  id8.length = 166;
  id8.breadth = 72.6;
  id8.draft = 24.3;
  id8.operational_depth = 0;
  id8.lift_capacity = 7826;
  id8.transit_speed = 7.3;
  id8.max_wind_speed = 20;
  id8.max_wave_height = 1.5;
  id8.day_rate = 525000;
  id8.mobilization_time = 3;
  id8.number_of_vessels = 1;
  vesselTemplates.insert( make_pair("SEMISUBMERSIBLE_CRANE", id8) );
  
  vessel id39 = vessel();
  id39.identifier = SIDE_ROCK_DUMPER; // 39;
  id39.length = 71;
  id39.breadth = 17.7;
  id39.draft = 3.3;
  id39.payload = 1490;
  id39.transit_speed = 7.8;
  id39.max_wave_height = 2.5;
  id39.day_rate = 84000;
  id39.mobilization_time = 3;
  id39.number_of_vessels = 1;
  vesselTemplates.insert( make_pair("SIDE_ROCK_DUMPER", id39) );
  
  vessel id21 = vessel();
  id21.identifier = MEDIUM_JACKUP_BARGE; // 21;
  id21.length = 34.9;
  id21.breadth = 20.5;
  id21.draft = 2.3;
  id21.operational_depth = 25;
  id21.leg_length = 45;
  id21.jackup_speed = 0.6667;
  id21.deck_space = 600;
  id21.payload = 395;
  id21.transit_speed = 4;
  id21.max_wind_speed = 10;
  id21.max_wave_height = 1.5;
  id21.day_rate = 50000;
  id21.mobilization_time = 3;
  id21.number_of_vessels = 3;
  vesselTemplates.insert( make_pair("MEDIUM_JACKUP_BARGE", id21) );
  
  vessel id40 = vessel();
  id40.identifier = BALLASTING; //40;
  id40.day_rate = 11500;
  id40.mobilization_time = 3;
  id40.number_of_vessels = 1;
  vesselTemplates.insert( make_pair("BALLASTING", id40) );
  
  vessel id41 = vessel();
  id41.identifier = BALLAST_HOPPER; //41;
  id41.day_rate = 20500;
  id41.mobilization_time = 3;
  id41.number_of_vessels = 1;
  vesselTemplates.insert( make_pair("BALLAST_HOPPER", id41) );

  vessel id16 = vessel();
  id16.identifier = LARGE_ARRAY_CABLE_LAY; // 16;
  id16.length = 111.1;
  id16.breadth = 23.6;
  id16.draft = 6.3;
  id16.operational_depth = 0;
  id16.deck_space = 1520;
  id16.transit_speed = 10.2;
  id16.max_wind_speed = 25;
  id16.max_wave_height = 1;
  id16.day_rate = 144000;
  id16.mobilization_time = 3;
  id16.number_of_vessels = 1;
  id16.bollard_pull = 72;
  id16.carousel_weight = 5900;
  vesselTemplates.insert( make_pair("LARGE_ARRAY_CABLE_LAY", id16) );

  vessel id20 = vessel();
  id20.identifier = LARGE_EXPORT_CABLE_LAY; // 20;
  id20.length = 102.7;
  id20.breadth = 25.2;
  id20.draft = 5.6;
  id20.operational_depth = 0;
  id20.deck_space = 1640;
  id20.transit_speed = 10.1;
  id20.max_wind_speed = 25;
  id20.max_wave_height = 1;
  id20.day_rate = 173000;
  id20.mobilization_time = 3;
  id20.number_of_vessels = 1;
  id20.carousel_weight = 5980;
  vesselTemplates.insert( make_pair("LARGE_EXPORT_CABLE_LAY", id20) );
}


// Helper function that chooses cables from an input vector of voltages
vector<cableFamily> wobos::set_cables(vector<int> cableVoltages) {
  vector<cableFamily> outvec;
  outvec.resize(cableVoltages.size());
  for (size_t i=0; i<cableVoltages.size(); i++)
    outvec[i] = cableFamily( arrayTemplates[cableVoltages[i]] );
  return outvec;
}


// Helper function that chooses vessels from an input vector of names
vector<vessel> wobos::set_vessels(vector<string> vesselNames) {
  vector<vessel> outvec;
  outvec.resize(vesselNames.size());
  for (size_t i=0; i<vesselNames.size(); i++)
    outvec[i] = vessel( vesselTemplates[vesselNames[i]] );
  return outvec;
}


void wobos::set_vessel_defaults() {

  scourProtVessel = vessel();

  elecSupportVessels = vector<vessel> {vesselTemplates["PERSONNEL_TRANSPORT"], vesselTemplates["GUARD"]} ;

  turbFeederBarge = vesselTemplates["LARGE_JACKUP_BARGE"];
  subFeederBarge  = vesselTemplates["LARGE_JACKUP_BARGE"];

  arrCabInstVessel = vesselTemplates["LARGE_ARRAY_CABLE_LAY"];
  expCabInstVessel = vesselTemplates["LARGE_EXPORT_CABLE_LAY"];

  if (isFixed()) {
    turbInstVessel   = vesselTemplates["HIGH_HEIGHT_LARGE_SIZED_JACKUP"];
    subInstVessel    = vesselTemplates["HIGH_HEIGHT_LARGE_SIZED_JACKUP"];
    substaInstVessel = vesselTemplates["SEMISUBMERSIBLE_CRANE"];
    
    turbSupportVessels = vector<vessel> {vesselTemplates["PERSONNEL_TRANSPORT"], vesselTemplates["GUARD"]} ;
    subSupportVessels  = vector<vessel> {vesselTemplates["PERSONNEL_TRANSPORT"], vesselTemplates["GUARD"]} ;
    elecTugs           = vector<vessel> {vesselTemplates["LARGE_AHST"]} ;
      
    if (substructure == MONOPILE)
      scourProtVessel = vesselTemplates["SIDE_ROCK_DUMPER"];
  }
  
  else if (substructure == SPAR) {
    
    turbInstVessel   = vesselTemplates["LARGE_AHST"];
    subInstVessel    = vesselTemplates["MEDIUM_AHST"];
    substaInstVessel = vesselTemplates["LARGE_AHST"];
    
    turbSupportVessels = vector<vessel> {vesselTemplates["MEDIUM_AHST"], vesselTemplates["MEDIUM_JACKUP_BARGE"],
					      vesselTemplates["SEA_GOING_SUPPORT_TUG"], vesselTemplates["PERSONNEL_TRANSPORT"],
					      vesselTemplates["GUARD"], vesselTemplates["BALLASTING"], vesselTemplates["BALLAST_HOPPER"] } ;

    subSupportVessels =  vector<vessel> {vesselTemplates["MEDIUM_JACKUP_BARGE"], vesselTemplates["SEA_GOING_SUPPORT_TUG"],
					      vesselTemplates["PERSONNEL_TRANSPORT"], vesselTemplates["GUARD"],
					      vesselTemplates["BALLASTING"], vesselTemplates["BALLAST_HOPPER"] } ;

    elecTugs = vector<vessel> {vesselTemplates["LARGE_AHST"], vesselTemplates["SEA_GOING_SUPPORT_TUG"]} ;
  }
  
  else if (substructure == SEMISUBMERSIBLE) {
    turbInstVessel   = vesselTemplates["MEDIUM_AHST"];
    subInstVessel    = vesselTemplates["MEDIUM_AHST"];
    substaInstVessel = vesselTemplates["LARGE_AHST"];

    turbSupportVessels = vector<vessel> {vesselTemplates["SEA_GOING_SUPPORT_TUG"], vesselTemplates["GUARD"]} ;
    subSupportVessels  = vector<vessel> {vesselTemplates["SEA_GOING_SUPPORT_TUG"], vesselTemplates["GUARD"]} ;
    elecTugs           = vector<vessel> {vesselTemplates["LARGE_AHST"], vesselTemplates["SEA_GOING_SUPPORT_TUG"]} ;
  }
  
}


//*******************************************************************************************
//Offshore BOS model 'Soft Costs' Module starts here and ends at  function definition
//*******************************************************************************************

void wobos::calculate_bos_cost() {
  // Commissioning cost
  commissioning = (totAnICost + totDevCost + totElecCost + totEnMCost + subTotCost + totPnSCost + turbCapEx*(turbR*nTurb*1000))*plantComm;

  // Calculate the present value in dollars of the cost of decommissioning the wind plant
  decomCost = isFloating() ?
    (((0.2*(moorTime / totInstTime) + 0.6*(turbInstTime / totInstTime) + 0.1*(arrInstTime / totInstTime) + 0.1*(expInstTime / totInstTime) +
       0.4*(subsInstTime / totInstTime))*totAnICost) - scrapVal) / pow((1 + decomDiscRate), projLife) :
    (((0.9*(subInstTime / totInstTime) + 0.7*(turbInstTime / totInstTime) + 0.2*(arrInstTime / totInstTime) + 0.2*(expInstTime / totInstTime) +
       0.8*(subsInstTime / totInstTime))*totAnICost) - scrapVal) / pow((1 + decomDiscRate), projLife);

  //calculate construction finance factor
  construction_finance_factor = capital_cost_year_0*(1+(1-tax_rate)*(pow((1+interest_during_construction),(capital_cost_year_0+0.5))-1))
    +capital_cost_year_1*(1+(1-tax_rate)*(pow((1+interest_during_construction),(capital_cost_year_1+0.5))-1))
    +capital_cost_year_2*(1+(1-tax_rate)*(pow((1+interest_during_construction),(capital_cost_year_2+0.5))-1))
    +capital_cost_year_3*(1+(1-tax_rate)*(pow((1+interest_during_construction),(capital_cost_year_3+0.5))-1))
    +capital_cost_year_4*(1+(1-tax_rate)*(pow((1+interest_during_construction),(capital_cost_year_4+0.5))-1))
    +capital_cost_year_5*(1+(1-tax_rate)*(pow((1+interest_during_construction),(capital_cost_year_5+0.5))-1));

  //calculate total soft costs
  // calculate the capital expeditures to get the plant running
  bos_capex = totAnICost + totDevCost + totElecCost + totEnMCost + totPnSCost + subTotCost;
  
  //calculate insurance cost during construction
  construction_insurance_cost = construction_insurance * (nTurb*turbCapEx + bos_capex);
  
  // Caclulate contingencies 
  total_contingency_cost = procurement_contingency*(nTurb*turbCapEx+bos_capex-totAnICost) + install_contingency*(totAnICost);
  
  // Compute coft cost total up until now
  soft_costs = construction_insurance_cost + commissioning + decomCost + total_contingency_cost;
  
  // calculate construction financing cost
  construction_finance_cost = (construction_finance_factor-1)*(nTurb*turbCapEx + bos_capex + soft_costs);
  
  // Add in financing cost
  soft_costs += construction_finance_cost;

  // Calculate total BOS costs
  total_bos_cost = totAnICost + totDevCost + totElecCost + totEnMCost + totPnSCost + subTotCost + soft_costs;
}


//*******************************************************************************************
//Offshore BOS model 'General' Module starts here and ends at TowerMass() function definition
//*******************************************************************************************

// Calculate turbine parameters
// NOTE: All these quantities can be inputs or outputs, depending on how the module is called
void wobos::set_turbine_parameters() {
  // Hub diameter
  if (hubD <= 0.0) 
    hubD = 0.25*turbR + 2; // meters

  // Blade length
  if (bladeL <= 0.0) 
    bladeL = 0.5 * (rotorD - hubD);
  
  // Blade chord TODO: Chord = hubD?  Smells like a bug
  if (chord <= 0.0) 
    chord = 0.25*turbR + 2; // meters

  // Nacelle width and length
  if (nacelleW <= 0.0) 
    nacelleW = hubD + 1.5; // meters
  if (nacelleL <= 0.0) 
    nacelleL = 2 * nacelleW; // meters

  // RNA mass
  if (rnaM <= 0.0) 
    rnaM = 2.082*pow(turbR, 2) + 44.59*turbR + 22.48; // kg

  // Tower diameter and mass
  if (towerD <= 0.0) 
    towerD = 0.5*turbR + 4; // meters
  if (towerM <= 0.0) 
    towerM   = (0.4*M_PI*pow((rotorD / 2), 2)*hubH - 1500) / 1000; // kg
}

//*******************************************************************************************
//Offshore BOS model 'Substructure & Foundation' module starts here and ends after
//SubstructTotCost() function definition
//*******************************************************************************************

tuple<double, double> wobos::calculate_monopile() {
  // Assign monopile length if it is not assigned
  if (mpileL <= 0) {mpileL = waterD + mpEmbedL + 5;}

  // Assign monopile depth if it is not assigned
  if (mpileD <= 0) {mpileD = turbR;}

  // Calculate monopile single pile mass in tonnes
  double mpileM     = (pow((turbR * 1000), 1.5) + (pow(hubH, 3.7) / 10) + 2100 *
		       pow(waterD, 2.25) + pow((rnaM * 1000), 1.13)) / 10000;

  // Calculate monopile single transition piece mass in tonnes
  double mtransM    = exp(2.77 + 1.04*pow(turbR, 0.5) + 0.00127*pow(waterD, 1.5));

  // Calculate monopile single pile cost in dollars
  double mPileCost  = mpileM * mpileCR;

  // Calculate monopile single transition piece cost in dollars
  double mTransCost = mtransM * mtransCR;

  return make_tuple(mpileM+mtransM, mPileCost+mTransCost);
}


tuple<double, double> wobos::calculate_jacket() {
  // Calculate single jacket lattice mass in tonnes
  double jlatticeM    = exp(3.71 + 0.00176*pow(turbR, 2.5) + 0.645*log(waterD));

  // Calculate single jacket transition piece mass in tonnes
  double jtransM      = 1 / (-0.0131 + 0.0381 / log(turbR) - 0.00000000227*pow(waterD, 3));
  
  // Calculate jacket pile mass in tonnes (total for 4 piles)
  double jpileM       = 8 * pow(jlatticeM, 0.5574);

  // Calculate single jacket lattice cost in dollars
  double jLatticeCost = jlatticeM * jlatticeCR;

  // Calculate single jacket transition piece cost in dollars
  double jTransCost   = jtransM * jtransCR;
  
  // Calculate total cost for 4 jacket piles in dollars
  double jPileCost    = jpileM * jpileCR;

  return make_tuple(jlatticeM+jtransM+jpileM, jLatticeCost+jTransCost+jPileCost);
}


tuple<double, double> wobos::calculate_spar() {
  // Calculate mass of the stiffened column for single spar in tonnes
  double spStifColM    = 535.93 + 17.664*pow(turbR, 2) + 0.02328*waterD*log(waterD);
  
  // Calculate mass of the tapered column for a single spar in tonnes
  double spTapColM     = 125.81*log(turbR) + 58.712;
  
  // Calculate the stiffened column cost for a single spar in dollars
  double spStifColCost = spStifColM * spStifColCR;

  // Calculate the tapered column cost for a single spar in dollars
  double spTapColCost  = spTapColM * spTapColCR;

  return make_tuple(spStifColM+spTapColM, spStifColCost+spTapColCost);
}


tuple<double, double> wobos::calculate_ballast() {
  // Calculate the ballast mass for a single spar in tonnes
  double ballM    = -16.536*pow(turbR, 2) + 1261.8*turbR - 1554.6;

  // Calculate the ballast cost for a single spar in dollars
  double ballCost = ballM * ballCR;

  return make_tuple(ballM, ballCost);
}


tuple<double, double> wobos::calculate_semi() {

  // Calculate the stiffened column mass for a single semisubmersible in tonnes
  double ssStifColM    = -0.9571*pow(turbR, 2) + 40.89*turbR + 802.09;

  // Calculate the truss mass for a single semisubmersible in tonnes
  double ssTrussM      = 2.7894*pow(turbR, 2) + 15.591*turbR + 266.03;

  // Calculate the heave plate mass for a single semisubmersible in tonnes
  double ssHeaveM      = -0.4397*pow(turbR, 2) + 21.545*turbR + 177.42;

  // Calculate the stiffened column cost for a single semisubmersible in dollars
  double ssStifColCost = ssStifColM * ssStifColCR;
  
  // Calculate the truss cost for a single semisubmersible in dollars
  double ssTrussCost   = ssTrussM * ssTrussCR;

  // Calculate the heave plate cost for a single semisubmersible in dollars
  double ssHeaveCost   = ssHeaveM * ssHeaveCR;
  
  return make_tuple(ssStifColM+ssTrussM+ssHeaveM, ssStifColCost+ssTrussCost+ssHeaveCost);
}


tuple<double, double> wobos::calculate_secondary_steel(int substructure) {
  // calculate the secondary steel mass in tonnes for a single substructure (ladders, boat landings, railing, etc.)
  double sSteelM=0;
  switch (substructure) {
  case MONOPILE:
  case JACKET:
    sSteelM = (turbR <= 4) ? 35 + (0.8*(18 + waterD)) : 40 + (0.8*(18 + waterD));
    break;
  case SPAR:
    sSteelM = exp(3.58 + 0.196*pow(turbR, 0.5)*log(turbR) + 0.00001*waterD*log(waterD));
    break;
  case SEMISUBMERSIBLE:
    sSteelM = -0.153*pow(turbR, 2) + 6.54*turbR + 128.34;
    break;
  }
  
  double sSteelCost = sSteelM * sSteelCR;

  return make_tuple(sSteelM, sSteelCost);
}
// Overload operator to pass in class variable if no argument is given
tuple<double, double> wobos::calculate_secondary_steel() {return calculate_secondary_steel(substructure);}


// Calculate the mooring system and anchor cost in dollars for a singe floating substructure (spar or semisubmersible)
// NOTE: This calculation may be provided as an input if run via WISDEM
void wobos::calculate_mooring() {
  
  if (moorCost <= 0.0) {
    
    // Check if mooring diameter was given as input.  If not, set standard sizes based on turbine rating
    if (moorDia <= 0.0) {
      double turbR_fit = -0.0004*pow(turbR, 2) + 0.0132*turbR + 0.0536;
      if (turbR_fit <= 0.09) {
	moorDia = 0.09;
      } else if (turbR_fit <= 0.12) {
	moorDia = 0.12;
      } else {
	moorDia = 0.15;
      }
    }
    
    // If moorling line cost is unset, select appropriate mooring line cost factor depending on the line diameter
    if (moorCR <= 0.0) {
      if (moorDia == 0.12) {
	moorCR = 721.0;
      } else if (moorDia == 0.15) {
	moorCR = 1088.0;
      } else {
	moorCR = 399.0;
      }
    }
    
    // Calculate mooring breaking load
    double moorBL = 419449 * pow(moorDia, 2) + 93415 * moorDia - 3577.9;
    
    //calculate mooring line length and anchor cost depending on anchor type
    double moorLeng=0, anchorCost=0;
    switch (anchor) {
    case DRAGEMBEDMENT:
      moorLeng   = 0.0002*pow(waterD, 2.0) + 1.264*waterD + 47.776 + deaFixLeng;
      anchorCost = moorBL / GRAVITY / 20.0 * 2000.0;
      break;
    case SUCTIONPILE:
      moorLeng   = 0.0002*pow(waterD, 2) + 1.264*waterD + 47.776;
      anchorCost = sqrt(moorBL / GRAVITY / 1250.0) * 150000.0;
      break;
    }
  
    // Finalize cost
    moorCost = moorLines*(anchorCost + moorLeng*moorCR);
  }
}


void wobos::calculate_substructure_mass_cost() {
  // Calculate total substructure mass (tonnes) for a single substructure depending on substructure type
  // Calculate the total substructure cost for entire project in dollars
  // NOTE: These quantities could be inputs if module is called within WISDEM
  if ( (subTotM <= 0.0) || (subTotCost <= 0.0) ) {
    switch (substructure) {
      
    case MONOPILE:
      tie(subTotM, subTotCost) = calculate_monopile();
      break;
      
    case JACKET:
      tie(subTotM, subTotCost) = calculate_jacket();
      break;
      
    case SPAR:
      double ballM, ballCost;
      tie(subTotM, subTotCost) = calculate_spar();
      tie(ballM, ballCost)     = calculate_ballast();
      subTotM    += ballM;
      subTotCost += ballCost;
      break;
      
    case SEMISUBMERSIBLE:
      tie(subTotM, subTotCost) = calculate_semi();
      break;
    }
    
    // All substructures get secondary steel additions
    double sSteelM, sSteelCost;
    tie(sSteelM, sSteelCost) = calculate_secondary_steel();
    subTotM    += sSteelM;
    subTotCost += sSteelCost;

    // Floaters have mooring lines too
    if (isFloating()) calculate_mooring();
    subTotCost += moorCost;
  }
  
  // Make cost be for total plant, not per turbine (need to do this even if using WISDEM)
  subTotCost *= nTurb;
}


//*******************************************************************************************
//Offshore BOS model 'Electrical Infrastructure' module starts here and ends after
//TotElectricalCost() function definition
//*******************************************************************************************

double wobos::calculate_export_cable_cost(double expCurrRating, double expVoltage, double expCabMass, double expSubsInterCR, double expCabCR) {
  // Calculate the total number of export cables that are required based on electrical limits of the cables
  nExpCab = ceil(((turbR*nTurb) / ((sqrt(3)*expCurrRating*expVoltage*pwrFac*(1 - (buryDepth - 1)*buryFac)) / 1000)));

  // Calculate the export cable length in meters
  expCabLeng = isFixed() ? (distShore * 1000 + waterD)*nExpCab*1.1 : (distShore * 1000 + freeCabLeng + 500)*nExpCab*1.1;
  
  // Calculate the total cost in dollars of the export cabling including interface costs
  expCabCost = isFloating() ?
    expCabCR*((expCabLeng - 500 - freeCabLeng) + dynCabFac*(500 + freeCabLeng)) + expSubsInterCR*nExpCab :
    expCabCR*expCabLeng + expSubsInterCR*nExpCab;
  
  // Calculate the mass of each section that makes up the export cable(s)
  double expCabSecM = expCabMass*expCabLeng / nExpCab / 1000;
  
  // Calculate the total number of cable sections per vessel trip for the export cable(s)
  double expCabSecPerTrip = expCabInstVessel.carousel_weight / expCabSecM;
  
  // Calculate the total duration in days required to install the export cable system
  double fac = (buryDepth > 0) ? 1 / buryRate : 0;
  expInstTime = ceil(ceil((ceil(nExpCab / expCabSecPerTrip) * (distPort / (expCabInstVessel.transit_speed * 1.852) + expCabLoad) +
			   (1 + exCabFac)*(distShore * 1000)*(1 / surfLayRate + fac) + (subsPullIn + shorePullIn + cabTerm)*nExpCab) / 24 + landConstruct) *
		     (1 / (1 - elecCont)));

  // Total cost includes material and installation costs- only used in optimization routine
  double totalCost = expInstTime*expCabInstVessel.day_rate + expCabCost + expCabLeng*cabSurveyCR;

  return totalCost;
}



double wobos::calculate_array_cable_cost(double cab1CurrRating, double cab2CurrRating, double arrVoltage, double arrCab1Mass, double arrCab2Mass,
					 double cab1CR, double cab2CR, double cab1TurbInterCR, double cab2TurbInterCR, double cab2SubsInterCR) {

  auto numTurbCable = [&] (double rating) {return (floor(((sqrt(3)*rating*arrVoltage*pwrFac*(1 - (buryDepth - 1)*buryFac)) / 1000) / turbR));};

  // Calculate the total number of full strings (string = a set of turbines that share the
  // same electrical line back to the substation from the array)
  double fullStrings = floor(nTurb / numTurbCable(cab2CurrRating) );
  
  // Calculate the number of turbines on a partial string (partial string = a string that
  // is created when the remainder of (number of turbines)/(full strings) is greater than zero
  // Using a lambda function to streamline
  double nTurbPS = fmod(nTurb, numTurbCable(cab2CurrRating)); 
  
  // Calculate the number of turbines that can fit onto array cable 1/2 given power transfer limits
  double nTurbCab1 = numTurbCable(cab1CurrRating);
  double nTurbCab2 = numTurbCable(cab2CurrRating);
  
  // Calculate the number of turbine interfaces on array cable 1/2
  double max1 = max(0.0, nTurbCab2 - nTurbCab1);
  double max2 = max(0.0, nTurbPS - nTurbCab1 - 1);
  double nTurbInter1 = (nTurbPS == 0) ? (nTurbCab1*fullStrings)*2.0 : (nTurbCab1*fullStrings + min((nTurbPS - 1), nTurbCab1)) * 2.0;
  double nTurbInter2 = (max1*fullStrings + max2) * 2.0;
  if (nTurbPS > 0.0) nTurbInter2 += 1.0;
  
  // Calculate the number of array cable substation interfaces: array cable 2 is used for all runs from array to substation
  double nSubsInter = (nTurbPS == 0) ? fullStrings : fullStrings + 1;
  double stringFac = (nSubstation > 0) ? nSubsInter / nSubstation : nSubsInter;
  
  // Calculate the length of array cable 1/2 in meters
  // TODO: THIS MAX1 FIX SEEMS LIKE A BUG
  // TODO: The max1 and max2 do not match the document in any equation used (nTurbInter, cabLeng, arrInstTime)!
  max1--;
  cab1Leng = isFixed() ?
    (arrayY*rotorD + waterD * 2)*(nTurbInter1 / 2)*(1 + exCabFac) :
    (2 * freeCabLeng + fixCabLeng)*(nTurbInter1 / 2)*(1 + exCabFac);

  cab2Leng = isFixed() ?
    (((arrayY*rotorD + waterD * 2)*(max1*fullStrings + max2)) + nSubstation *
     ((stringFac * ((rotorD*arrayY) + sqrt(pow(((rotorD*arrayX)*(stringFac - 1)), 2) + pow((rotorD*arrayY), 2)))) / 2 + stringFac*waterD))*(exCabFac + 1) :
    (((2 * freeCabLeng + fixCabLeng)*max1*fullStrings + max2) + nSubstation *
     (stringFac*((2 * freeCabLeng + fixCabLeng) + sqrt(pow(((stringFac - 1) *((2 * freeCabLeng) + (arrayX*rotorD) - (2 * ((tan(systAngle*(M_PI / 180))*waterD) + 70)))), 2)
						       + pow((2 * freeCabLeng + fixCabLeng), 2)))) / 2)*(exCabFac + 1);
  max1++;

  // Calculate the total cost in dollars of array cable 1/2 including interface costs
  arrCab1Cost = isFloating() ? dynCabFac*cab1Leng*cab1CR + cab1TurbInterCR*nTurbInter1 : cab1Leng*cab1CR + cab1TurbInterCR*nTurbInter1;
  arrCab2Cost = isFloating() ?
    dynCabFac*cab2Leng*cab2CR + nTurbInter2*cab2TurbInterCR + nSubsInter*cab2SubsInterCR :
    cab2Leng*cab2CR + nTurbInter2*cab2TurbInterCR + nSubsInter*cab2SubsInterCR;

  //calculate the mass of each section that makes up array cable 1
  auto cabSecMass = [&] (double arrMass) {return isFixed() ?
					 arrMass * (arrayY*rotorD + waterD * 2)*(1 + exCabFac) / 1000 :
					  arrMass * (freeCabLeng * 2 + fixCabLeng)*(1 + exCabFac) / 1000;};
  double cab1SecM = cabSecMass(arrCab1Mass);
  double cab2SecM = cabSecMass(arrCab2Mass);
  
  // Calculate the total number of cable sections per vessel trip for array cable 1/2
  auto cabSecPerTrip = [&] (double cabM) {return floor(arrCabInstVessel.carousel_weight / cabM);};
  double cab1SecPerTrip = cabSecPerTrip(cab1SecM);
  double cab2SecPerTrip = cabSecPerTrip(cab2SecM);
  
  //check if cable is buried or not
  double fac1 = (buryDepth > 0) ? 1 / buryRate : 0.0;

  //check if a partial string exists
  double fac2 = (nTurbPS == 0) ? (fullStrings*(fullStrings + 1)) / 2 : ((fullStrings + 1)*((fullStrings + 1) + 1)) / 2.0;

  // Calculate the total duration in days required to install the array cabling
  arrInstTime = ceil((1 + ((((cab1Leng + cab2Leng - (waterD*(nTurbInter1 + nTurbInter2 + nSubsInter))*(1 + exCabFac)) * (fac1 + 1 / surfLayRate) +
			     (cabPullIn + cabTerm)*(nTurbInter1 + nTurbInter2 + nSubsInter)) +
			    (ceil(((nTurbInter1*0.5) / cab1SecPerTrip)) + (ceil(((max1*fullStrings + max2 + fac2) / cab2SecPerTrip)))) *
			    (cabLoadout + distPort / (arrCabInstVessel.transit_speed * 1.852)))*(1 / (1 - elecCont)) / 24)));

  // Total cost includes material and installation costs- only used in optimization routine
  double totalCost = arrCab1Cost + arrCab2Cost + arrInstTime*arrCabInstVessel.day_rate + (cab1Leng + cab2Leng)*cabSurveyCR;

  return totalCost;
}


double wobos::calculate_subsea_cable_cost() {
  // Calculate system angle (system angle = a value used to calculate a hypotenuse distance
  // which is used to approximate the free hanging length of the array cable for floating wind plants)
  systAngle = -0.0047*waterD + 18.743;
  
  // Calculate the free hanging or catenary length in meters of cable that hangs from floating
  // turbines down to the sea floor
  freeCabLeng = (waterD / cos(systAngle*(M_PI / 180)))*(catLengFac + 1) + 190;
  
  // Calculate the fixed cable length in meters along the sea floor between the free hanging sections
  // located in between turbine interfaces
  fixCabLeng = (arrayY*rotorD) - (2 * (((tan(systAngle*(M_PI / 180)))*waterD) + 70));

  // If optimizing, set parameters that govern cost function
  if (cableOptimizer) ExportCabCostOptimizer();

  // Note, class variables that are set inside this function are used, not the output total that includes installation costs too
//  double expCabCostTot = calculate_export_cable_cost(expCurrRating, expVoltage, expCabMass, expSubsInterCR, expCabCR);
  calculate_export_cable_cost(expCurrRating, expVoltage, expCabMass, expSubsInterCR, expCabCR);

  // Calculate the number of substations that are required (this impacts array cable calculations)
  nSubstation = max(1.0, ceil(0.5 * nExpCab) );
  
  // If optimizing, set parameters that govern cost function
  if (cableOptimizer) ArrayCabCostOptimizer();

  // Note, class variables that are set inside this function are used, not the output total that includes installation costs too
//  double arrCabCostTot = calculate_array_cable_cost(cab1CurrRating, cab2CurrRating, arrVoltage, arrCab1Mass, arrCab2Mass,
//	  cab1CR, cab2CR, cab1TurbInterCR, cab2TurbInterCR, cab2SubsInterCR);
  calculate_array_cable_cost(cab1CurrRating, cab2CurrRating, arrVoltage, arrCab1Mass, arrCab2Mass,
	  cab1CR, cab2CR, cab1TurbInterCR, cab2TurbInterCR, cab2SubsInterCR);

  //calculate the total cost in dollars of the sub-sea cabling which includes export and array cabling
  return (arrCab1Cost + arrCab2Cost + expCabCost);
}


double wobos::calculate_substation_cost() {
  // calculate the total number of main power transformers (MPTs) that are required
  double nMPT = ceil(((nTurb*turbR) / 250));
  
  // calculate the rating in megavolt amperes of a single MPT
  //if the remainder of '((nTurb*turbR*1.15)/nMPT)/10' is greater than 5 round up, else round down
  double mptRating = (double)(roundf(float((nTurb*turbR*1.15) / nMPT / 10.0)) * 10.0);

  //calculate the total cost for all MPTs in dollars
  double mptCost = mptRating * nMPT * mptCR;
  
  // calculate the topside mass of the offshore substation(s) in tonnes
  subsTopM    = 3.85*(mptRating*nMPT) + 285.0;

  // calculate the offshore substation topside cost in dollars
  double subsTopCost = subsTopM*subsTopFab + subsTopDes;
  
  // calculate the cost in dollars of the shunt reactors used to dissipate capacitive reactance
  double shuntReactors = mptRating*nMPT*shuntCR*0.5;
  
  // calculate the cost in dollars of the switchgear used for redundancy and protection against electrical surges or faults
  double switchGear = nMPT*(highVoltSG + medVoltSG);
  
  //calculate the cost in dollars of any remaining, necessary ancillary systems
  double ancillarySys = backUpGen + workSpace + otherAncillary;
  
  // calculate the cost of assembling the offshore substation on land in dollars
  double subsLandAssembly = (switchGear + shuntReactors + mptCost)*topAssemblyFac;

  // calculate the substructure mass and cost- fraction of topside if fixed, double-large semi if floating
  double subsSubCost;
  if (isFixed()) {
    subsSubM    = 0.4 * subsTopM;
    
    // Calculate the mass of the jacket piles in tonnes used for offshore substation substructure for fixed turbine wind plants
    subsPileM   = 8.0 * pow(subsSubM, 0.5574);
    
    // Calculate the total substructure cost for the offshore substation
    subsSubCost = subsSubM*subsJackCR + subsPileCR*subsPileM;

  } else {
    
    subsPileM = 0.0;
    // Use a semi substructure because a spar wouldn't be able to handle it
    tie(subsSubM, subsSubCost) = calculate_semi();
    subsSubCost += moorCost;
    // All substructures get secondary steel additions
    double sSteelM, sSteelCost;
    tie(sSteelM, sSteelCost) = calculate_secondary_steel(SEMISUBMERSIBLE);
    subsSubM    += sSteelM;
    subsSubCost += sSteelCost;
    // Approximate doubling becuase substation topside mass is much larger than a turbine
    subsSubM    *= 2.0;
    subsSubCost *= 2.0;
    // Note that in original Maness C++ version sSteelCost was not included and sSteelM was grabbed from current substructure (spar/semi)
  }

  //calculate the total cost in dollars of the offshore substation
  return ( (subsTopCost + switchGear + shuntReactors + ancillarySys + mptCost + subsLandAssembly + subsSubCost) * nSubstation);
}


double wobos::calculate_onshore_transmission_cost() {
  // calculate the cost in dollars of the electrical switch yard
  double switchYard = 18115 * interConVolt + 165944;
  
  // calculate the onshore substation cost in dollars
  double onShoreSubs = 11652 * (interConVolt + turbR*nTurb) + 1200000;
  
  // calculate miscellaneous costs associated with the onshore substation in dollars
  double onshoreMisc = 11795 * pow((turbR*nTurb), 0.3549) + 350000;
  
  // calculate the cost in dollars of the overhead transmission line for connection back to grid
  double transLine = (1176 * interConVolt + 218257)*pow(distInterCon, -0.1063)*distInterCon;

  //calculate the total cost in dollars of the onshore transmission system which includes the onshore
  //substation, switch yard, connection to grid, and other misc. costs
  return (onShoreSubs + onshoreMisc + transLine + switchYard);
}



void wobos::calculate_electrical_infrastructure_cost() {

  // Calculate subsea cable cost
  double subCabCost = calculate_subsea_cable_cost();
  
  // Calculate the total cost in dollars of the offshore substation  
  double offSubsCost = calculate_substation_cost();

  // Calculate the total cost in dollars of the onshore transmission system
  double onshoreTransCost = calculate_onshore_transmission_cost();

  //calculate the total cost for the entire electrical infrastructure system
  totElecCost = subCabCost + offSubsCost + onshoreTransCost;
}



//*******************************************************************************************
//Offshore BOS model 'Assembly & Installation' module starts here and ends at TotInstCost()
//function definition
//*******************************************************************************************

double wobos::MinTurbDeckArea() {
  
  double area1;
  //check for turbine installation method
  switch (turbInstallMethod) {
  case ROTORASSEMBLED:
    area1 = (bladeL + inspectClear)*(chord + inspectClear) + (M_PI*pow((hubD / 2), 2)) / 2
      + (nacelleL + inspectClear)*(nacelleW + inspectClear);
    break;
  case BUNNYEARS:
    area1 = (bladeL + inspectClear)*(chord + inspectClear) + (nacelleL + inspectClear)
      *(nacelleW / 2 + bladeL / 2 * 1.73 + inspectClear);
    break;
  default:
  case INDIVIDUAL:
    area1 = (bladeL + inspectClear)*(chord + inspectClear) + (nacelleL + inspectClear)
      *(nacelleW + inspectClear);
    break;
  }

  double area2 = (towerInstallMethod == ONEPIECE) ?
    area1 + pow((towerD + inspectClear), 2.0) :
    area1 + pow((towerD + inspectClear), 2.0) * 2.0;

  return area2;
}


double wobos::TurbineInstall() {
  double sum=0;
  //check turbine installation method
  switch (turbInstallMethod) {
  case ROTORASSEMBLED:
    sum = vesselPosTurb + boltTower + boltNacelle3 + boltRotor;
    break;
  case BUNNYEARS:
    sum = vesselPosTurb + boltTower + boltNacelle2 + boltBlade2;
    break;
  case INDIVIDUAL:
    sum = vesselPosTurb + boltTower + boltNacelle1 + 3 * boltBlade1;
    break;
  }

  if (towerInstallMethod == TWOPIECE) sum += boltTower;

  if (isFloating()) sum -= vesselPosTurb + turbFasten;

  double sum2 = (installStrategy == PRIMARYVESSEL) ? 
    (ceil(nTurb / nTurbPerTrip))*(distPort / ((turbInstVessel.transit_speed * 1852) / 1000)) * 2 + turbFasten*nTurb : 0;

  //check substructure type
  if (substructure == SPAR) {
    return ceil(1.5*(((distPtoA / (turbInstVessel.tow_speed))*(nTurb / nTurbPerTrip)) / 24 + sum *
		     (1 / (1 - turbCont))*nTurb / 24) + (1 / (1 - substructCont))*(nTurb / 24) *
		(distAtoS / (turbInstVessel.tow_speed) + (spMoorCon + spMoorCheck + prepTow)) );

  } else if (substructure == SEMISUBMERSIBLE) {
    return ceil(sum*(1 / (1 - turbCont))*nTurb / 24 +
		((nTurb / 24)*(1 / (1 - substructCont)) * ((prepTow + ssBall + ssMoorCheck + ssMoorCon) +
							   (distPort / turbInstVessel.tow_speed))));
  } else { // ((substructure == MONOPILE) || (substructure == JACKET))
    return ceil((((sum + ((waterD + 10) / (turbInstVessel.jackup_speed * 60)) * 2) *
		  nTurb + (nTurb - ceil((nTurb / nTurbPerTrip)))*(arrayY*rotorD) /
		  (turbInstVessel.transit_speed * 1852) + sum2) / 24)*(1 / (1 - turbCont)));
  }
}


double wobos::SubstructureInstTime() {

  double fac1 = 0;
  if (installStrategy == PRIMARYVESSEL) {
    fac1 = (substructure == JACKET) ?
      ceil(nTurb / nSubPerTrip)*(distPort / (subInstVessel.transit_speed * 1.852)) * 2 * 2 + 2 * jackFasten*nTurb :
      ceil(nTurb / nSubPerTrip)*(distPort / (subInstVessel.transit_speed * 1.852)) * 2 * 1 + monoFasten*nTurb;
  }
  
  //check substructure type
  double sum1 = (substructure == JACKET) ?
    vesselPosJack * 2 + placeTemplate + prepGripperJack + placePiles + prepHamJack + removeHamJack + placeJack
    + levJack + ((jpileL - 5) / hamRate) * 4 + ((waterD + 10) / (subInstVessel.jackup_speed * 60)) * 2 * 2 :
    vesselPosMono + prepGripperMono + placeMP + prepHamMono + removeHamMono + placeTP + groutTP //change
    + tpCover + (mpEmbedL / hamRate) + ((waterD + 10) / (subInstVessel.jackup_speed * 60)) * 2;
  
  //check if fixed substructure type is selected
  return isFixed() ?
    ceil(((1 / (1 - substructCont))*((sum1*nTurb + (nTurb - ceil((nTurb / nSubPerTrip)))
				      *(rotorD*arrayX) / (subInstVessel.transit_speed * 1852)) + fac1)) / 24) :
    moorTime + floatPrepTime;
}


void wobos::calculate_assembly_and_installation() {

  // Calculate the total duration in days for the mooring system installation
  moorTime = (anchor == DRAGEMBEDMENT) ? 5.0 + waterD*moorTimeFac : 11.0 + waterD*moorTimeFac;
  moorTime = ceil((((moorLoadout + moorSurvey + moorTime)*moorLines + (waterD*moorTimeFac)*moorLines +
		    (distPort * 1000 * 2 / (subInstVessel.transit_speed * 1852)))*nTurb / 24)*(1 / (1 - substructCont)));

  // Calculate the total duration in days to prep floating substructures for turbine installation
  floatPrepTime = (substructure == SPAR) ?
    ceil(((((prepSpar + upendSpar) + (distPtoA / subInstVessel.tow_speed))*nTurb) / 24) + prepAA / 24) :
    ceil((prepSemi*nTurb) / 24);
  
  // Calculate the minimum deck area that is required to for a single turbine
  turbDeckArea = MinTurbDeckArea();
  
  // Calculated the maximum number of turbines that can be transported to the install site
  nTurbPerTrip = 1;
  if (isFixed()) {
    nTurbPerTrip = (installStrategy == PRIMARYVESSEL) ?
      floor(min((turbInstVessel.deck_space / turbDeckArea), (turbInstVessel.payload / (rnaM + towerM)))) :
      floor(min((turbFeederBarge.deck_space / turbDeckArea), (turbFeederBarge.payload / (rnaM + towerM))));
  } else if (substructure == SPAR) {
    nTurbPerTrip = floor(min((turbFeederBarge.deck_space / turbDeckArea), (turbFeederBarge.payload / (rnaM + towerM))));
  }

  // Calculate the total duration in days required to install all turbines
  turbInstTime = TurbineInstall();

  // Calculate the minimum turbine deck area that is required for a single substructure (fixed substructures only
  // return 0 for floating substructures)
  switch (substructure) {
  case MONOPILE:
    subDeckArea = (mpileL + inspectClear)*(mpileD + inspectClear) + pow((mpileD + inspectClear + 1), 2);
    break;
  case JACKET:
    subDeckArea = pow((jlatticeA + inspectClear), 2) + (jpileD + inspectClear)*(jpileL + inspectClear);
    break;
  default:
    subDeckArea = 0;
    break;
  }

  // Calculate the maximum number of substructures that can be transported to the install site
  nSubPerTrip = 1;
  if (isFixed()) {
    nSubPerTrip = (installStrategy == PRIMARYVESSEL) ?
      floor(min((subInstVessel.deck_space / subDeckArea), (subInstVessel.payload / subTotM))) :
      floor(min((subFeederBarge.deck_space / subDeckArea), (subFeederBarge.payload / subTotM)));
  }
  
  // Calculate the total duration in days required to install all substructures
  subInstTime = SubstructureInstTime();

  // Calculate the total duration in days required to install the offshore substation
  subsInstTime = isFixed() ?
    ceil(((subsLoad + subsVessPos + placeTop) + (distPort / (substaInstVessel.transit_speed * 1.852))) /
	 24 * (1 / (1 - elecCont))) :
    ceil(((subsLoad + subsVessPos + placeTop + ssMoorCon + ssMoorCheck) +
	  (distPort / (substaInstVessel.tow_speed * 1.852))) / 24 * (1 / (1 - elecCont)));
  
  // Calculate the total duration in days required to install the complete wind plant
  totInstTime = turbInstTime + arrInstTime + expInstTime + subsInstTime;
  totInstTime += isFloating() ? moorTime + floatPrepTime : subInstTime;
  
  // Calculate the cost of surveying and verifying cable installation
  cabSurvey =  (expCabLeng + cab1Leng + cab2Leng) * cabSurveyCR;
  
  TurbInstCost();
  SubInstCost();
  ElectricalInstCost();
  VesselMobDemobCost();
  totAnICost = TotInstCost();
}


//Calculate the cost of each vessel used for turbine installation
void wobos::TurbInstCost() {

  turbine_install_cost = turbInstVessel.get_rate() * turbInstTime;

  for (size_t i=0; i<turbSupportVessels.size(); i++)
    turbine_install_cost += turbSupportVessels[i].get_rate() * turbInstTime;

  if ((installStrategy == FEEDERBARGE) || (substructure == SPAR))
    turbine_install_cost += turbFeederBarge.get_rate() * turbInstTime;
}


// Calculate substructure installation cost
void wobos::SubInstCost() {

  double instTime1 = (substructure == SPAR) ? (subInstTime - moorTime) : subInstTime;
  double instTime2 = (substructure == SPAR) ? moorTime : subInstTime;

  substructure_install_cost = subInstVessel.get_rate() * instTime2;

  for (size_t i=0; i < subSupportVessels.size(); i++)
    substructure_install_cost += subSupportVessels[i].get_rate() * instTime1;
  
  if ((installStrategy == FEEDERBARGE) || (substructure == SPAR))
    substructure_install_cost += subFeederBarge.get_rate() * instTime1;
  
  if (substructure == MONOPILE)
    substructure_install_cost += (instScour/24) * nTurb * scourProtVessel.get_rate();
}


// Calculate electrical infrastructure installation cost
void wobos::ElectricalInstCost() {

  array_cable_install_cost  = arrCabInstVessel.get_rate() * arrInstTime;
  export_cable_install_cost = expCabInstVessel.get_rate() * expInstTime;

  substation_install_cost = 0.0;
  for (size_t i=0; i<elecTugs.size(); i++)
    substation_install_cost += elecTugs[i].get_rate() * subsInstTime;

  if (isFixed())
    substation_install_cost += substaInstVessel.get_rate() * subsInstTime;

  electrical_install_cost = array_cable_install_cost + export_cable_install_cost + substation_install_cost;
  for (size_t i=0; i<elecSupportVessels.size(); i++)
    electrical_install_cost += elecSupportVessels[i].get_rate() * (arrInstTime + expInstTime + subsInstTime);
}


// Quick helper function for repeated activities in mobilization-demobilizastion function
double my_mobilization_cost(vessel myvessel, set<int> *myset) {
  double cost = 0.0;
  if (myset->find((int)myvessel.identifier) == myset->end()) {
    cost = myvessel.get_mobilization_cost();
    myset->insert((int)myvessel.identifier);
  }
  return cost;
}

// Calculate mobilization and demobilization costs for unique vessels
void wobos::VesselMobDemobCost() {

  // Initialize output
  mob_demob_cost = 0.0;

  // Keep track of vessels we are using so that we don't duplicate contributions
  set<int> myset = set<int>();

  // Add contributions from every (unique) vessel
  mob_demob_cost += my_mobilization_cost( turbInstVessel, &myset);
  mob_demob_cost += my_mobilization_cost( subInstVessel, &myset);
  mob_demob_cost += my_mobilization_cost( arrCabInstVessel, &myset);
  mob_demob_cost += my_mobilization_cost( expCabInstVessel, &myset);
  mob_demob_cost += my_mobilization_cost( substaInstVessel, &myset);
  mob_demob_cost += my_mobilization_cost( scourProtVessel, &myset);

  for (size_t i=0; i<elecTugs.size(); i++)
    mob_demob_cost += my_mobilization_cost( elecTugs[i], &myset);
  
  for (size_t i=0; i < turbSupportVessels.size(); i++)
    mob_demob_cost += my_mobilization_cost( turbSupportVessels[i], &myset);
  
  for (size_t i=0; i<subSupportVessels.size(); i++)
    mob_demob_cost += my_mobilization_cost( subSupportVessels[i], &myset);
  
  for (size_t i=0; i<elecSupportVessels.size(); i++)
    mob_demob_cost += my_mobilization_cost( elecSupportVessels[i], &myset);
  
  if(installStrategy == FEEDERBARGE || substructure == SPAR) {
    mob_demob_cost += my_mobilization_cost( turbFeederBarge, &myset);
    mob_demob_cost += my_mobilization_cost( subFeederBarge, &myset);
  }

  // Expense this for number of installation seasons
  mob_demob_cost *= number_install_seasons;
}


//calculate the total cost in dollars for the complete assembly and installation of the wind plant
double wobos::TotInstCost() {

  // Initialize summation
  double sum = cabSurvey + cabDrillDist*cabDrillCR + (mpvRentalDR + diveTeamDR + winchDR)*landConstruct + civilWork + elecWork +
    turbine_install_cost + substructure_install_cost + electrical_install_cost + mob_demob_cost;
  
  if (isFixed()) {
    sum += subInstTime*(pileSpreadDR + groutSpreadDR) + groutSpreadMob + pileSpreadMob + compRacks;
  } else if (anchor == SUCTIONPILE) {// floating and suction pile
    sum += seaSpreadDR*moorTime + seaSpreadMob;
  }
  
  // Substructure specific
  if (substructure == MONOPILE) {
    sum += scourMat*nTurb;
  } else if (substructure == SPAR) {
    sum += compRacks;
  }
  
  return sum;
}

//*******************************************************************************************
//Offshore BOS model 'Port & Staging' module starts here and ends after TotalPnSCost()
//function definition
//*******************************************************************************************

double wobos::calculate_entrance_exit_costs() {
  //check substructure type
  if (substructure == SEMISUBMERSIBLE) {
    return ((nTurb*turbInstVessel.length * turbInstVessel.breadth) + 1)*entranceExitRate;
  } else if (substructure == SPAR) {
    return (ceil(nTurb / nTurbPerTrip)*(turbInstVessel.length * turbInstVessel.breadth + turbFeederBarge.length
					* turbFeederBarge.breadth) + 1)*entranceExitRate;
  } else {
    //check installation vessel strategy
    if (installStrategy == PRIMARYVESSEL) {
      return ((ceil(nTurb / nTurbPerTrip)*(turbInstVessel.length * turbInstVessel.breadth) +
	       ceil(nTurb / nSubPerTrip)*(subInstVessel.length * subInstVessel.breadth)) + substaInstVessel.length * substaInstVessel.breadth)
	*entranceExitRate;
    } else {
      return (ceil(nTurb / nTurbPerTrip)*(turbFeederBarge.length * turbFeederBarge.breadth)
	      + (ceil(nTurb / nSubPerTrip)*subFeederBarge.length * subFeederBarge.breadth)
	      + substaInstVessel.length * substaInstVessel.breadth)*entranceExitRate;
    }
  }
}


void wobos::calculate_port_and_staging_costs() {

  // Calculate the cost in dollars of port entrance and exit fees based on vessel size and number of entrances/exits
  double entrExitCost = calculate_entrance_exit_costs();

  // Calculate the cost in dollars of the wharf where loading and unloading operations will take place
  double wharfCost = isFixed() ?
    ((rnaM + towerM + subTotM)*nTurb + subsTopM + subsSubM + subsPileM)*wharfRate :
    ((rnaM + towerM)*nTurb + subsTopM)*wharfRate;

  // calculate the cost in dollars of the vessel docking cost as a function of installation duration
  double dockCost = isFloating() ?
    (moorTime + floatPrepTime + turbInstTime + subsInstTime)*dockRate :
    (turbInstTime + subInstTime + subsInstTime)*dockRate;
  
  // Calculate the required area in square meters for substructure staging and pre-assembly
  double subLaydownA;
  if (installStrategy == FEEDERBARGE) {
    subLaydownA = (subDeckArea*nSubPerTrip*subFeederBarge.number_of_vessels) * 2;
  } else {
    subLaydownA = isFixed() ? (subDeckArea*nSubPerTrip) * 2 : 0;
  }
  
  // Calculate the cost in dollars of the laydown and staging area for substructures
  double subLayCost = subInstTime * laydownCR * subLaydownA;
  
  // Calculate the required area in square meters for the turbine staging and pre-assembly
  double turbLaydownA = (installStrategy == FEEDERBARGE) ?
    turbDeckArea*nTurbPerTrip*turbFeederBarge.number_of_vessels * 2 :
    turbDeckArea*nTurbPerTrip * 2;
  
  // Calculate the cost in dollars of the staging area for the turbines
  double turbLayCost = turbInstTime * turbLaydownA * laydownCR;

  // Set number of cranes needed
  if (nCrane1000 <= 0) nCrane1000 = 1;
  if (nCrane600 <= 0) {
    nCrane600 = (substructure == SPAR) ? 3 : 1;
  }

  // Calculate the cost in dollars of the cranes that are required to carry out lifting operations at port
  double craneCost = isFloating() ? (nCrane600*crane600DR + nCrane1000*crane1000DR)*(turbInstTime + floatPrepTime + moorTime)
    + (crane1000DR*(ceil(subsTopM / 1000))*(placeTop / 24)) + craneMobDemob :
    (nCrane600*crane600DR + nCrane1000*crane1000DR)*(turbInstTime + subInstTime) + craneMobDemob;
  
  // Calculate the total port cost in dollars which includes the wharf, docking, and entrance/exit costs
  double totPortCost = entrExitCost + dockCost + wharfCost;

  // Calculate the total staging cost in dollars which includes the laydown, staging, and crane costs
  double totStageCost = turbLayCost + subLayCost + craneCost;
  
  // Calculate the total cost in dollars of all port and staging related costs and fees
  totPnSCost = totPortCost + totStageCost;
}


//*******************************************************************************************
//Offshore BOS model 'Engineering & Management' module starts here and ends at TotalEnMCost()
//function definition
//*******************************************************************************************

//calculate the total engineering and management cost based on a percentage of total hard costs
//(hard costs = procurement and installation costs excluding turbine costs)
void wobos::calculate_engineering_management_cost() {
  totEnMCost = estEnMFac*(subTotCost + totPnSCost + totElecCost + totAnICost);
}

//*******************************************************************************************
//Offshore BOS model 'Development' module starts here and ends after TotalDevCost() function
//definition
//*******************************************************************************************
void wobos::calculate_development_cost() {
  // Calculate the cost in dollars of the front end engineering design study pre-development
  double feedCost = preFEEDStudy + feedStudy;
  
  // Calculate the total compliance cost in dollars which includes various leases and legal compliance costs
  double permStudyComp = stateLease + outConShelfLease + saPlan + conOpPlan + nepaEisMet + physResStudyMet + bioResStudyMet
		+ socEconStudyMet + navStudyMet + nepaEisProj + physResStudyProj + bioResStudyProj + socEconStudyProj
		+ navStudyProj + coastZoneManAct + rivsnHarbsAct + cleanWatAct402 + cleanWatAct404 + faaPlan
		+ endSpecAct + marMamProtAct + migBirdAct + natHisPresAct + addLocPerm;

  // Calculate the cost in dollars of the meteorological tower used for wind resource and other site
  // assessment studies including installation costs
  double metFabCost = nTurb*turbR*metTowCR;
  
  // Calculate the total development cost in dollars
  totDevCost = metFabCost + permStudyComp + feedCost;
}

//*******************************************************************************************
//Offshore BOS model 'Electrical Cable Optimization' module starts here and ends after
//ExportCabCostOptimizer() function definition
//*******************************************************************************************

//This optimizer determines which cables for the array are of the lowest cost considering both
//procurement costs and installation costs
void wobos::ArrayCabCostOptimizer() {
  size_t nArrVolts = arrCables.size();
  size_t nArrCables = arrCables[0].cables.size();
  
  //The next 3 arrays contain default data for the array cables. note that if the number of cables
  //changes form the default of 11 then these tables will need the appropriate additions of the
  //data for the cables that you wish to add
  /*                      (turbine      (substation
			  ($/m)     ,(kg/m)     , $/interface), $/interface)*/
  /*double array33kvData[11][4] =  {
    {185.889 , 20.384   , 8410        , 19610},   //95 mm2
    {202.788 , 22.854   , 8615        , 19815},  //120 mm2
    {208.421 , 23.912   , 8861        , 20062},  //150 mm2
    {236.586 , 25.676   , 9149        , 20349},  //185 mm2
    {270.384 , 28.910   , 9600        , 20800},  //240 mm2
    {315.448 , 32.242   , 10092       , 21292},  //300 mm2
    {360.512 , 37.142   , 10913       , 22113},  //400 mm2
    {422.475 , 42.336   , 11733       , 22933},  //500 mm2
    {478.805 , 48.706   , 12800       , 24000},  //630 mm2
    {585.832 , 57.428   , 14195       , 25395},  //800 mm2
    {698.492 , 66.738   , 15836       , 27036}}; //1000 mm2
  */
  /*                  (turbine      (substation
		      ($/m)    ,(kg/m)  , $/interface), $/interface)*/
  /*  double array66kvData[11][4] = {
      {225.320 , 21.6   , 8831        , 20591},  //95 mm2
      {242.219 , 23.8   , 9046        , 20806},  //120 mm2
      {253.485 , 25.7   , 9304        , 21065},  //150 mm2
      {281.650 , 28.0   , 9606        , 21366},  //185 mm2
      {326.714 , 31.3   , 10080       , 21840},  //240 mm2
      {383.044 , 34.3   , 10597       , 22357},  //300 mm2
      {433.741 , 39.2   , 11459       , 23219},  //400 mm2
      {506.970 , 45.4   , 12320       , 24080},  //500 mm2
      {574.566 , 52.0   , 13440       , 25200},  //630 mm2
      {704.125 , 60.1   , 14905       , 26665},  //800 mm2
      {844.950 , 71.7   , 16628       , 28388}}; //1000 mm2
      
      double currentRating[] =      //amperes
      {300,   //95 mm2
      340,   //120 mm2
      375,   //150 mm2
      420,   //185 mm2
      480,   //240 mm2
      530,   //300 mm2
      590,   //400 mm2
      655,   //500 mm2
      715,   //630 mm2
      775,   //800 mm2
      825};  //1000 mm2*/

  size_t cabIndex1    = 0;
  size_t cabIndex2    = 0;
  size_t arrVoltIndex = 0;
  double oldCost      = 1e30;
  double newCost      = 0;

  for (size_t k = 0; k < nArrVolts; k++) { // volt loop
    for (size_t i = 0; i < nArrCables; i++) { // cable1 loop
      for (size_t j = i + 1; j < nArrCables; j++) { // cable 2 loop
	newCost = calculate_array_cable_cost(arrCables[k].cables[i].currRating, arrCables[k].cables[j].currRating, arrCables[k].voltage,
					     arrCables[k].cables[i].mass, arrCables[k].cables[j].mass, arrCables[k].cables[i].cost, arrCables[k].cables[j].cost,
					     arrCables[k].cables[i].turbInterfaceCost, arrCables[k].cables[j].turbInterfaceCost, arrCables[k].cables[j].subsInterfaceCost);
	if (newCost < oldCost) {
	    oldCost      = newCost;
	    cabIndex1    = i;
	    cabIndex2    = j;
	    arrVoltIndex = k;
	}
      }
    }
  }
  
  arrVoltage      = arrCables[arrVoltIndex].voltage;
  cab1CR          = arrCables[arrVoltIndex].cables[cabIndex1].cost;
  cab2CR          = arrCables[arrVoltIndex].cables[cabIndex2].cost;
  cab1CurrRating  = arrCables[arrVoltIndex].cables[cabIndex1].currRating;
  cab2CurrRating  = arrCables[arrVoltIndex].cables[cabIndex2].currRating;
  arrCab1Mass     = arrCables[arrVoltIndex].cables[cabIndex1].mass;
  arrCab2Mass     = arrCables[arrVoltIndex].cables[cabIndex2].mass;
  cab1TurbInterCR = arrCables[arrVoltIndex].cables[cabIndex1].turbInterfaceCost;
  cab2TurbInterCR = arrCables[arrVoltIndex].cables[cabIndex2].turbInterfaceCost;
  cab2SubsInterCR = arrCables[arrVoltIndex].cables[cabIndex2].subsInterfaceCost;
}


void wobos::ExportCabCostOptimizer() {
  size_t nExpVolts  = expCables.size();
  size_t nExpCables = expCables[0].cables.size();
  
  //($/m) ,(kg/m)  ,(ancillary cost $/interface)
  /*double export132kvData[10][3] =  {
    {433.504  , 48.000 , 57500},  //300 mm2
    {520.489  , 51.100 , 60000},  //400 mm2
    {596.388  , 58.000 , 62500},  //500 mm2
    {689.479  , 65.200 , 65000},  //630 mm2
    {843.823  , 74.000 , 67500},  //800 mm2
    {1006.054 , 85.400 , 70000},  //1000 mm2
    {1168.284 , 113.147, 72500},  //1200 mm2
    {1492.745 , 131.387, 75000},  //1600 mm2
    {1818.332 , 149.627, 77500},  //2000 mm2
    {2223.908 , 172.427, 80000}}; //2500 mm2
    
    //($/m) ,(kg/m)  ,(ancillary cost $/interface)
    double export220kvData[10][3] =  {
    {495.411  , 71.900 , 57500},  //300 mm2
    {578.187  , 76.500 , 60000},  //400 mm2
    {681.863  , 81.300 , 62500},  //500 mm2
    {788.620  , 86.700 , 65000},  //630 mm2
    {966.623  , 95.300 , 67500},  //800 mm2
    {1159.271 , 104.000, 70000},  //1000 mm2
    {1336.148 , 113.147, 72500},  //1200 mm2
    {1676.499 , 131.387, 75000},  //1600 mm2
    {2042.784 , 149.627, 77500},  //2000 mm2
    {2498.703 , 172.427, 80000}}; //2500 mm2
    
    double currentRating[] = {
    530,   //300 mm2
    590,   //400 mm2
    655,   //500 mm2
    715,   //630 mm2
    775,   //800 mm2
    825,   //1000 mm2
    990,   //1200 mm2
    1061,  //1600 mm2
    1299,  //2000 mm2
    1375}; //2500 mm2*/
  
  //calculate for each option of voltage 1 the necessary cable data
  double newCost      = 0.0;
  double oldCost      = 1e30;
  size_t expCabIndex  = 0;
  size_t expVoltIndex = 0;
  
  for (size_t k=0; k<nExpVolts; k++) {
    for (size_t i=0; i<nExpCables; i++) {
      newCost = calculate_export_cable_cost(expCables[k].cables[i].currRating, expCables[k].voltage, expCables[k].cables[i].mass,
					    expCables[k].cables[i].subsInterfaceCost, expCables[k].cables[i].cost);

      if (newCost < oldCost) {
	oldCost      = newCost;
	expCabIndex  = i;
	expVoltIndex = k;
      }
    }
  }

  expVoltage     = expCables[expVoltIndex].voltage;
  expCurrRating  = expCables[expVoltIndex].cables[expCabIndex].currRating;
  expCabMass     = expCables[expVoltIndex].cables[expCabIndex].mass;
  expSubsInterCR = expCables[expVoltIndex].cables[expCabIndex].subsInterfaceCost;
  expCabCR       = expCables[expVoltIndex].cables[expCabIndex].cost;
}


void wobos::run() {
  // Set turbine sizing
  set_turbine_parameters();

  // SUBSTRUCTURE
  calculate_substructure_mass_cost();

  // ELECTRICAL INFRASTRUCTURE
  calculate_electrical_infrastructure_cost();

  // ASSEMBLY & INSTALLATION
  calculate_assembly_and_installation();  

  // PORTS & STAGING
  calculate_port_and_staging_costs();

  // ENGINEERING & MANAGEMENT
  calculate_engineering_management_cost();

  // DEVELOPMENT
  calculate_development_cost();

  // FINAL
  calculate_bos_cost();
}

