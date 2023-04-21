/*******************************************************************************************************
 *  Copyright 2017 Alliance for Sustainable Energy, LLC
 *
 *  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
 *  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
 *  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
 *  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
 *  copies to the public, perform publicly and display publicly, and to permit others to do so.
 *
 *  Redistribution and use in source and binary forms, with or without modification, are permitted
 *  provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice, the above government
 *  rights notice, this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright notice, the above government
 *  rights notice, this list of conditions and the following disclaimer in the documentation and/or
 *  other materials provided with the distribution.
 *
 *  3. The entire corresponding source code of any redistribution, with or without modification, by a
 *  research entity, including but not limited to any contracting manager/operator of a United States
 *  National Laboratory, any institution of higher learning, and any non-profit organization, must be
 *  made publicly available under this license for as long as the redistribution is made available by
 *  the research entity.
 *
 *  4. Redistribution of this software, without modification, must refer to the software by the same
 *  designation. Redistribution of a modified version of this software (i) may not refer to the modified
 *  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
 *  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
 *  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
 *  designation may not be used to refer to any modified version of this software or any modified
 *  version of the underlying software originally provided by Alliance without the prior written consent
 *  of Alliance.
 *
 *  5. The name of the copyright holder, contributors, the United States Government, the United States
 *  Department of Energy, or any of their employees may not be used to endorse or promote products
 *  derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
 *  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
 *  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 *  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 *  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *******************************************************************************************************/

#include "core.h"
#include "lib_wind_obos.h"

//#include <iostream>
//#include <cmath>
//#include <math.h>
//#include <vector>
//#include <algorithm>
//#include <map>
#include <string>
//#include <array>
//#include <fstream>

//using namespace std;

/*
//substructure type
enum { MONOPILE, JACKET, SPAR, SEMISUBMERSIBLE };
//anchor types
enum  { DRAGEMBEDMENT, SUCTIONPILE };
//turbine installation methods
enum  { INDIVIDUAL, BUNNYEARS, ROTORASSEMBLED };
//turbine tower installation methods
enum  { ONEPIECE, TWOPIECE };
//installation vessel strategy
enum  { PRIMARYVESSEL, FEEDERBARGE };
//toggle cable cost optimizer on or off
enum  { ON, OFF };
*/
//static var_info* _cm_vtab_wind_obos;

static var_info vtab_wind_obos[] = {
  //  VARTYPE           DATATYPE         NAME                              LABEL                                                      UNITS                 META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS
  //#IN/OUT
  //#IN/OUT
  //# Turbine / Plant parameters
{ SSC_INPUT, SSC_NUMBER, "turbCapEx","Turbine Capital Cost","$/kW","","wobos","?=1605","","" },
{ SSC_INPUT, SSC_NUMBER, "nTurb","Number of Turbines","","","wobos","?=20","MIN=2,MAX=200","" },
{ SSC_INPUT, SSC_NUMBER, "turbR","Turbine Rating","MW","","wobos","?=5","MIN=1,MAX=15","" },
{ SSC_INPUT, SSC_NUMBER, "rotorD","Rotor Diameter","m","","wobos","?=120","","" },
{ SSC_INPUT, SSC_NUMBER, "hubH","Hub Height","m","","wobos","?=90","","" },
{ SSC_INPUT, SSC_NUMBER, "waterD","Max Water Depth","m","","wobos","?=30","MIN=3,MAX=1000","" },
{ SSC_INPUT, SSC_NUMBER, "distShore","Distance to Landfall","km","","wobos","?=90","MIN=5,MAX=1000","" },
{ SSC_INPUT, SSC_NUMBER, "distPort","Distance from Installation Port to Site","km","","wobos","?=90","MIN=5,MAX=1000","" },
{ SSC_INPUT, SSC_NUMBER, "distPtoA","Distance from Installation Port to Inshore Assembly Area","km","","wobos","?=90","MIN=5,MAX=1000","" },
{ SSC_INPUT, SSC_NUMBER, "distAtoS","Distance form Inshore Assembly Area to Site","km","","wobos","?=90","MIN=5,MAX=1000","" },
{ SSC_INPUT, SSC_NUMBER, "substructure","Substructure Type","","","wobos","?=MONOPILE","INTEGER","" },
{ SSC_INPUT, SSC_NUMBER, "anchor","Anchor Type","","","wobos","?=DRAGEMBEDMENT","INTEGER","" },
{ SSC_INPUT, SSC_NUMBER, "turbInstallMethod","Turbine Installation Method","","","wobos","?=INDIVIDUAL","INTEGER","" },
{ SSC_INPUT, SSC_NUMBER, "towerInstallMethod","Tower Installation Method","","","wobos","?=ONEPIECE","INTEGER","" },
{ SSC_INPUT, SSC_NUMBER, "installStrategy","Installation Vessel Strategy","","","wobos","?=PRIMARYVESSEL","INTEGER","" },
{ SSC_INPUT, SSC_NUMBER, "cableOptimizer","Electrical Cable Cost Optimization","","","wobos","?=FALSE","INTEGER","" },
{ SSC_INPUT, SSC_NUMBER, "moorLines","Number Of Mooring Lines","","","wobos","?=3","","" },
{ SSC_INPUT, SSC_NUMBER, "buryDepth","Electrical Cable Burial Depth","m","","wobos","?=2","MIN=0,MAX=15","" },
{ SSC_INPUT, SSC_NUMBER, "arrayY","Spacing Between Turbines in Rows","rotor diameters","","wobos","?=9","MIN=1","" },
{ SSC_INPUT, SSC_NUMBER, "arrayX","Spacing Between Turbine Rows","rotor diameters","","wobos","?=9","MIN=1","" },
{ SSC_INPUT, SSC_NUMBER, "substructCont","Substructure Install Weather Contingency","%","","wobos","?=0.3","","" },
{ SSC_INPUT, SSC_NUMBER, "turbCont","Turbine Install Weather Contingency","%","","wobos","?=0.3","","" },
{ SSC_INPUT, SSC_NUMBER, "elecCont","Electrical Install Weather Contingency","%","","wobos","?=0.3","","" },
{ SSC_INPUT, SSC_NUMBER, "interConVolt","Grid Interconnect Voltage","kV","","wobos","?=345","","" },
{ SSC_INPUT, SSC_NUMBER, "distInterCon","Distance Over Land to Grid Interconnect","miles","","wobos","?=3","","" },
{ SSC_INPUT, SSC_NUMBER, "scrapVal","Total Scrap Value of Decommissioned Components","$","","wobos","?=0","","" },
//#General
{ SSC_INPUT, SSC_NUMBER, "projLife","Project Economic Life","years","","wobos","?=20","","" },
{ SSC_INPUT, SSC_NUMBER, "inspectClear","Inspection Clearance","m","","wobos","?=2","","" },
{ SSC_INPUT, SSC_NUMBER, "plantComm","Plant Commissioning Cost Factor","","","wobos","?=0.01","","" },
{ SSC_INPUT, SSC_NUMBER, "procurement_contingency","Procurement Contingency","","","wobos","?=0.05","","" },
{ SSC_INPUT, SSC_NUMBER, "install_contingency","Installation Contingency","","","wobos","?=0.3","","" },
{ SSC_INPUT, SSC_NUMBER, "construction_insurance","Insurance During Construction (% of ICC)","","","wobos","?=0.01","","" },
{ SSC_INPUT, SSC_NUMBER, "capital_cost_year_0","Capital cost spent in year 0","","","wobos","?=0.2","","" },
{ SSC_INPUT, SSC_NUMBER, "capital_cost_year_1","Capital cost spent in year 1","","","wobos","?=0.6","","" },
{ SSC_INPUT, SSC_NUMBER, "capital_cost_year_2","Capital cost spent in year 2","","","wobos","?=0.1","","" },
{ SSC_INPUT, SSC_NUMBER, "capital_cost_year_3","Capital cost spent in year 3","","","wobos","?=0.1","","" },
{ SSC_INPUT, SSC_NUMBER, "capital_cost_year_4","Capital cost spent in year 4","","","wobos","?=0","","" },
{ SSC_INPUT, SSC_NUMBER, "capital_cost_year_5","Capital cost spent in year 5","","","wobos","?=0","","" },
{ SSC_INPUT, SSC_NUMBER, "tax_rate","Effective Tax Rate","","","wobos","?=0.4","","" },
{ SSC_INPUT, SSC_NUMBER, "interest_during_construction","Interest During Construction","","","wobos","?=0.08","","" },
//#Substructure & Foundation
{ SSC_INPUT, SSC_NUMBER, "mpileCR","Monopile Cost Rate","$/tonne","","wobos","?=2250","","" },
{ SSC_INPUT, SSC_NUMBER, "mtransCR","Monopile Transition Piece Cost Rate","$/tonne","","wobos","?=3230","","" },
{ SSC_INPUT, SSC_NUMBER, "mpileD","Monopile Diameter","m","","wobos","?=0","MIN=0.01","" },
{ SSC_INPUT, SSC_NUMBER, "mpileL","Monopile Length","m","","wobos","?=0","MIN=0.01","" },
{ SSC_INPUT, SSC_NUMBER, "mpEmbedL","Monopile Embedment Length","m","","wobos","?=30","","" },
{ SSC_INPUT, SSC_NUMBER, "jlatticeCR","Jacket Main Lattice Cost Rate","$/tonne","","wobos","?=4680","","" },
{ SSC_INPUT, SSC_NUMBER, "jtransCR","Jacket Transition Piece Cost Rate","$/tonne","","wobos","?=4500","","" },
{ SSC_INPUT, SSC_NUMBER, "jpileCR","Jacket Pile Cost Rate","$/tonne","","wobos","?=2250","","" },
{ SSC_INPUT, SSC_NUMBER, "jlatticeA","Jacket Main Lattice Footprint Area","m^2","","wobos","?=26","","" },
{ SSC_INPUT, SSC_NUMBER, "jpileL","Jacket Pile Length","m","","wobos","?=47.5","","" },
{ SSC_INPUT, SSC_NUMBER, "jpileD","Jacket Pile Diameter","m","","wobos","?=1.6","","" },
{ SSC_INPUT, SSC_NUMBER, "spStifColCR","Spar Stiffened Column Cost Rate","$/tonne","","wobos","?=3120","","" },
{ SSC_INPUT, SSC_NUMBER, "spTapColCR","Spar Tapered Column Cost Rate","$/tonne","","wobos","?=4220","","" },
{ SSC_INPUT, SSC_NUMBER, "ballCR","Floating Ballast Cost Rate","$/tonne","","wobos","?=100","","" },
{ SSC_INPUT, SSC_NUMBER, "deaFixLeng","Fixed Mooring Length for Drag Embedment Anchors","m","","wobos","?=500","","" },
{ SSC_INPUT, SSC_NUMBER, "ssStifColCR","Semi-submersible Stiffened Column Cost Rate","$/tonne","","wobos","?=3120","","" },
{ SSC_INPUT, SSC_NUMBER, "ssTrussCR","Semi-submersible Truss Cost Rate","$/tonne","","wobos","?=6250","","" },
{ SSC_INPUT, SSC_NUMBER, "ssHeaveCR","Semi-submersible Heave Plate Cost Rate","$/tonne","","wobos","?=6250","","" },
{ SSC_INPUT, SSC_NUMBER, "sSteelCR","Secondary/Outfitting Steel Cost Rate","$/tonne","","wobos","?=7250","","" },
{ SSC_INPUT, SSC_NUMBER, "moorDia","Mooring Line Diameter","m","","wobos","?=0","MIN=0.09","" },
{ SSC_INPUT, SSC_NUMBER, "moorCR","Mooring Line Cost Rate","$/m","","wobos","?=0","MIN=399","" },
{ SSC_INPUT, SSC_NUMBER, "scourMat","Scour Protection Material Cost","$/location","","wobos","?=250000","","" },
{ SSC_INPUT, SSC_NUMBER, "number_install_seasons","Number of Installation Seasons","","","wobos","?=1","","" },
//#Electrical Infrastructure
{ SSC_INPUT, SSC_NUMBER, "pwrFac","Power Transfer Efficiency Factor","","","wobos","?=0.95","","" },
{ SSC_INPUT, SSC_NUMBER, "buryFac","Cable Burial Depth Factor","1/m","","wobos","?=0.1","","" },
{ SSC_INPUT, SSC_NUMBER, "catLengFac","Catenary Cable Length Factor","","","wobos","?=0.04","","" },
{ SSC_INPUT, SSC_NUMBER, "exCabFac","Excess Cable Factor","","","wobos","?=0.1","","" },
{ SSC_INPUT, SSC_NUMBER, "subsTopFab","Offshore Substation Fabrication Cost","$/tonne","","wobos","?=14500","","" },
{ SSC_INPUT, SSC_NUMBER, "subsTopDes","Offshore Substation Design Cost","$","","wobos","?=4500000","","" },
{ SSC_INPUT, SSC_NUMBER, "topAssemblyFac","Offshore Substation Land-based Assembly Factor","","","wobos","?=0.075","","" },
{ SSC_INPUT, SSC_NUMBER, "subsJackCR","Offshore Substation Jacket Lattice Cost Rate","$/tonne","","wobos","?=6250","","" },
{ SSC_INPUT, SSC_NUMBER, "subsPileCR","Offshore Substation Jacket Pile Cost Rate","$/tonne","","wobos","?=2250","","" },
{ SSC_INPUT, SSC_NUMBER, "dynCabFac","Dynamic Cable Cost Premium Factor","","","wobos","?=2","","" },
{ SSC_INPUT, SSC_NUMBER, "shuntCR","Shunt Reactor Cost Rate","$/MVA","","wobos","?=35000","","" },
{ SSC_INPUT, SSC_NUMBER, "highVoltSG","High Voltage Switchgear Cost","$","","wobos","?=950000","","" },
{ SSC_INPUT, SSC_NUMBER, "medVoltSG","Medium Voltage Switchgear Cost","$","","wobos","?=500000","","" },
{ SSC_INPUT, SSC_NUMBER, "backUpGen","Back up Diesel Generator Cost","$","","wobos","?=1000000","","" },
{ SSC_INPUT, SSC_NUMBER, "workSpace","Offshore Substation Workspace & Accommodations Cost","$","","wobos","?=2000000","","" },
{ SSC_INPUT, SSC_NUMBER, "otherAncillary","Other Ancillary Systems Costs","$","","wobos","?=3000000","","" },
{ SSC_INPUT, SSC_NUMBER, "mptCR","Main Power Transformer Cost Rate","$/MVA","","wobos","?=12500","","" },
{ SSC_INPUT, SSC_NUMBER, "arrVoltage","Array cable voltage","kV","","wobos","?=33","","" },
{ SSC_INPUT, SSC_NUMBER, "cab1CR","Array cable 1 Cost Rate","$/m","","wobos","?=185.889","","" },
{ SSC_INPUT, SSC_NUMBER, "cab2CR","Array cable 2 Cost Rate","$/m","","wobos","?=202.788","","" },
{ SSC_INPUT, SSC_NUMBER, "cab1CurrRating","Array cable 1 current rating","A","","wobos","?=300","","" },
{ SSC_INPUT, SSC_NUMBER, "cab2CurrRating","Array cable 2 current rating","A","","wobos","?=340","","" },
{ SSC_INPUT, SSC_NUMBER, "arrCab1Mass","Array cable 1 mass","kg/m","","wobos","?=20.384","","" },
{ SSC_INPUT, SSC_NUMBER, "arrCab2Mass","Array cable 2 mass","kg/m","","wobos","?=21.854","","" },
{ SSC_INPUT, SSC_NUMBER, "cab1TurbInterCR","Cable 1 turbine interface cost","$/interface","","wobos","?=8410","","" },
{ SSC_INPUT, SSC_NUMBER, "cab2TurbInterCR","Cable 2 turbine interface cost","$/interface","","wobos","?=8615","","" },
{ SSC_INPUT, SSC_NUMBER, "cab2SubsInterCR","Cable 2 substation interface cost","$/interface","","wobos","?=19815","","" },
{ SSC_INPUT, SSC_NUMBER, "expVoltage","Export cable voltage","kV","","wobos","?=220","","" },
{ SSC_INPUT, SSC_NUMBER, "expCurrRating","Export cable current rating","A","","wobos","?=530","","" },
{ SSC_INPUT, SSC_NUMBER, "expCabMass","Export cable mass","kg/m","","wobos","?=71.9","","" },
{ SSC_INPUT, SSC_NUMBER, "expCabCR","Export cable cost rate","$/m","","wobos","?=495.411","","" },
{ SSC_INPUT, SSC_NUMBER, "expSubsInterCR","Export cable substation interface cost","$/interface","","wobos","?=57500","","" },
//# Vector inputs
{ SSC_INPUT, SSC_STRING, "arrayCables","Inter-array cables to consider by voltage","kV","","wobos","?=33 66","","" },
{ SSC_INPUT, SSC_STRING, "exportCables","Export cables to consider by voltage","kV","","wobos","?=132 220","","" },
//#Assembly & Installation
{ SSC_INPUT, SSC_NUMBER, "moorTimeFac","Anchor & Mooring Water Depth Time Factor","","","wobos","?=0.005","","" },
{ SSC_INPUT, SSC_NUMBER, "moorLoadout","Anchor & Mooring Loadout Time","hours","","wobos","?=5","","" },
{ SSC_INPUT, SSC_NUMBER, "moorSurvey","Survey Mooring Lines & Anchor Positions Time","hours","","wobos","?=4","","" },
{ SSC_INPUT, SSC_NUMBER, "prepAA","Prepare Inshore Assembly Area For Turbine Installation","hours","","wobos","?=168","","" },
{ SSC_INPUT, SSC_NUMBER, "prepSpar","Prepare Spar for Tow to Inshore Assembly Area","hours","","wobos","?=18","","" },
{ SSC_INPUT, SSC_NUMBER, "upendSpar","Upend and Ballast Spar","hours","","wobos","?=36","","" },
{ SSC_INPUT, SSC_NUMBER, "prepSemi","Prepare Semi-submersible for Turbine Installation","hours","","wobos","?=12","","" },
{ SSC_INPUT, SSC_NUMBER, "turbFasten","Prepare and Fasten Turbine for Transport","hours/turbine","","wobos","?=8","","" },
{ SSC_INPUT, SSC_NUMBER, "boltTower","Lift and Bolt Tower Section","hours","","wobos","?=7","","" },
{ SSC_INPUT, SSC_NUMBER, "boltNacelle1","Lift and Bolt Nacelle Individual Components Method","hours","","wobos","?=7","","" },
{ SSC_INPUT, SSC_NUMBER, "boltNacelle2","Lift and Bolt Nacelle Bunny Ears Method","hours","","wobos","?=7","","" },
{ SSC_INPUT, SSC_NUMBER, "boltNacelle3","Lift and Bolt Nacelle Fully Assembled Rotor Method","hours","","wobos","?=7","","" },
{ SSC_INPUT, SSC_NUMBER, "boltBlade1","Lift and Bolt Blade Individual Components Method","hours","","wobos","?=3.5","","" },
{ SSC_INPUT, SSC_NUMBER, "boltBlade2","Lift and Bolt Blade Bunny Ears Method","hours","","wobos","?=3.5","","" },
{ SSC_INPUT, SSC_NUMBER, "boltRotor","Lift and Bolt Rotor Fully Assembled Rotor Method","hours","","wobos","?=7","","" },
{ SSC_INPUT, SSC_NUMBER, "vesselPosTurb","Vessel Positioning Time Turbine Installation","hours","","wobos","?=2","","" },
{ SSC_INPUT, SSC_NUMBER, "vesselPosJack","Vessel Positioning Time Jacket Installation","hours","","wobos","?=8","","" },
{ SSC_INPUT, SSC_NUMBER, "vesselPosMono","Vessel Positioning Time Monopile Installation","hours","","wobos","?=3","","" },
{ SSC_INPUT, SSC_NUMBER, "subsVessPos","Vessel Positioning Time Offshore Substation Installation","hours","","wobos","?=6","","" },
{ SSC_INPUT, SSC_NUMBER, "monoFasten","Prepare and Fasten Monopile for Transport","hours/unit","","wobos","?=12","","" },
{ SSC_INPUT, SSC_NUMBER, "jackFasten","Prepare and Fasten Jacket for Transport","hours/unit","","wobos","?=20","","" },
{ SSC_INPUT, SSC_NUMBER, "prepGripperMono","Prepare Monopile Gripper and Upender","hours","","wobos","?=1.5","","" },
{ SSC_INPUT, SSC_NUMBER, "prepGripperJack","Prepare Jacket Gripper and Upender","hours","","wobos","?=8","","" },
{ SSC_INPUT, SSC_NUMBER, "placePiles","Place Jacket Piles","hours","","wobos","?=12","","" },
{ SSC_INPUT, SSC_NUMBER, "prepHamMono","Prepare Hammer for Monopile Installation","hours","","wobos","?=2","","" },
{ SSC_INPUT, SSC_NUMBER, "prepHamJack","Prepare Hammer for jacket Piles Installation","hours","","wobos","?=2","","" },
{ SSC_INPUT, SSC_NUMBER, "removeHamMono","Remove Hammer for Monopile Installation","hours","","wobos","?=2","","" },
{ SSC_INPUT, SSC_NUMBER, "removeHamJack","Remove Hammer for Jacket Piles Installation","hours","","wobos","?=4","","" },
{ SSC_INPUT, SSC_NUMBER, "placeTemplate","Place Jacket Pile Template on Seabed","hours","","wobos","?=4","","" },
{ SSC_INPUT, SSC_NUMBER, "placeJack","Place Jacket Main Lattice onto Piles","hours","","wobos","?=12","","" },
{ SSC_INPUT, SSC_NUMBER, "levJack","Level Jacket Main Lattice","hours","","wobos","?=24","","" },
{ SSC_INPUT, SSC_NUMBER, "hamRate","Pile Hammer Rate","m/hour","","wobos","?=20","","" },
{ SSC_INPUT, SSC_NUMBER, "placeMP","Lift and Place Monopile for Hammering","hours","","wobos","?=3","","" },
{ SSC_INPUT, SSC_NUMBER, "instScour","Install Scour Protection Around Monopile Base","hours","","wobos","?=6","","" },
{ SSC_INPUT, SSC_NUMBER, "placeTP","Place Transition Piece onto Monopile","hours","","wobos","?=3","","" },
{ SSC_INPUT, SSC_NUMBER, "groutTP","Grout Transition Piece/Monopile Interface","hours","","wobos","?=8","","" },
{ SSC_INPUT, SSC_NUMBER, "tpCover","Install Transition Piece Cover","hours","","wobos","?=1.5","","" },
{ SSC_INPUT, SSC_NUMBER, "prepTow","Prepare Floating Substructure for Tow to Site","hours","","wobos","?=12","","" },
{ SSC_INPUT, SSC_NUMBER, "spMoorCon","Connect Mooring Lines to Spar","hours","","wobos","?=20","","" },
{ SSC_INPUT, SSC_NUMBER, "ssMoorCon","Connect Mooring Lines to Semi-Submersible","hours","","wobos","?=22","","" },
{ SSC_INPUT, SSC_NUMBER, "spMoorCheck","Survey Spar Mooring Lines and Connections","hours","","wobos","?=16","","" },
{ SSC_INPUT, SSC_NUMBER, "ssMoorCheck","Survey Semi-submersible Mooing Lines and Connections","hours","","wobos","?=12","","" },
{ SSC_INPUT, SSC_NUMBER, "ssBall","Ballast Semi-submersible","hours","","wobos","?=6","","" },
{ SSC_INPUT, SSC_NUMBER, "surfLayRate","Cable Surface Lay Rate","m/hour","","wobos","?=375","","" },
{ SSC_INPUT, SSC_NUMBER, "cabPullIn","Array Cable Pull in to Interfaces","hours","","wobos","?=5.5","","" },
{ SSC_INPUT, SSC_NUMBER, "cabTerm","Cable Termination and Testing","hours","","wobos","?=5.5","","" },
{ SSC_INPUT, SSC_NUMBER, "cabLoadout","Array Cable Loadout for Installation","hours","","wobos","?=14","","" },
{ SSC_INPUT, SSC_NUMBER, "buryRate","Cable Burial Rate","m/hour","","wobos","?=125","","" },
{ SSC_INPUT, SSC_NUMBER, "subsPullIn","Cable Pull in to Offshore Substation","hours","","wobos","?=48","","" },
{ SSC_INPUT, SSC_NUMBER, "shorePullIn","Cable Pull in to Onshore Infrastructure","hours","","wobos","?=96","","" },
{ SSC_INPUT, SSC_NUMBER, "landConstruct","Onshore Infrastructure Construction","days","","wobos","?=7","","" },
{ SSC_INPUT, SSC_NUMBER, "expCabLoad","Export Cable Loadout for Installation","hours","","wobos","?=24","","" },
{ SSC_INPUT, SSC_NUMBER, "subsLoad","Offshore Substation Loadout for Installation","hours","","wobos","?=60","","" },
{ SSC_INPUT, SSC_NUMBER, "placeTop","Lift and Place Offshore Substation Topside","hours","","wobos","?=24","","" },
{ SSC_INPUT, SSC_NUMBER, "pileSpreadDR","Piling Spread Day Rate","$/day","","wobos","?=2500","","" },
{ SSC_INPUT, SSC_NUMBER, "pileSpreadMob","Piling Spread Mobilization Cost","$","","wobos","?=750000","","" },
{ SSC_INPUT, SSC_NUMBER, "groutSpreadDR","Grouting Spread Day Rate","$/day","","wobos","?=3000","","" },
{ SSC_INPUT, SSC_NUMBER, "groutSpreadMob","Grouting Spread Mobilization Cost","$","","wobos","?=1000000","","" },
{ SSC_INPUT, SSC_NUMBER, "seaSpreadDR","Suction Pile Anchor Spread Day Rate","$/day","","wobos","?=165000","","" },
{ SSC_INPUT, SSC_NUMBER, "seaSpreadMob","Suction Pile Anchor Spread Mobilization Cost","$","","wobos","?=4500000","","" },
{ SSC_INPUT, SSC_NUMBER, "compRacks","Component Racks Cost","$","","wobos","?=1000000","","" },
{ SSC_INPUT, SSC_NUMBER, "cabSurveyCR","Cable Route Survey Cost","$/m","","wobos","?=240","","" },
{ SSC_INPUT, SSC_NUMBER, "cabDrillDist","Horizontal Drilling distance for Cable Landfall","m","","wobos","?=500","","" },
{ SSC_INPUT, SSC_NUMBER, "cabDrillCR","Cost Rate for Horizontal Drilling","$/m","","wobos","?=3200","","" },
{ SSC_INPUT, SSC_NUMBER, "mpvRentalDR","MPV Rental Day Rate","$/day","","wobos","?=72000","","" },
{ SSC_INPUT, SSC_NUMBER, "diveTeamDR","Cable Landfall Dive Team Day Rate","$/day","","wobos","?=3200","","" },
{ SSC_INPUT, SSC_NUMBER, "winchDR","Cable Landfall Winch Day Rate","$/day","","wobos","?=1000","","" },
{ SSC_INPUT, SSC_NUMBER, "civilWork","Onshore Infrastructure Civil Work Cost","$","","wobos","?=40000","","" },
{ SSC_INPUT, SSC_NUMBER, "elecWork","Onshore Infrastructure Electrical Work Cost","$","","wobos","?=25000","","" },
//#Port & Staging
{ SSC_INPUT, SSC_NUMBER, "nCrane600","Number of 600 t Crawler Cranes","","","wobos","?=0","","" },
{ SSC_INPUT, SSC_NUMBER, "nCrane1000","Number of 1000 t Crawler Cranes","","","wobos","?=0","","" },
{ SSC_INPUT, SSC_NUMBER, "crane600DR","600 t Crawler Crane Day Rate","$/day","","wobos","?=5000","","" },
{ SSC_INPUT, SSC_NUMBER, "crane1000DR","1000 t Crawler Crane Day Rate","$/day","","wobos","?=8000","","" },
{ SSC_INPUT, SSC_NUMBER, "craneMobDemob","Port Crane Mobilization/Demobilization Cost","$","","wobos","?=150000","","" },
{ SSC_INPUT, SSC_NUMBER, "entranceExitRate","Port Entrance and Exit Cost Rate","$/occurrence","","wobos","?=0.525","","" },
{ SSC_INPUT, SSC_NUMBER, "dockRate","Quayside Docking Cost Rate","$/day","","wobos","?=3000","","" },
{ SSC_INPUT, SSC_NUMBER, "wharfRate","Wharf Loading and Unloading Cost Rate","$/tonne","","wobos","?=2.75","","" },
{ SSC_INPUT, SSC_NUMBER, "laydownCR","Laydown and Storage Cost Rate","$/m^2/day","","wobos","?=0.25","","" },
//#Engineering & Management
{ SSC_INPUT, SSC_NUMBER, "estEnMFac","Estimated Engineering & Management Cost Factor","","","wobos","?=0.04","","" },
//#Development
{ SSC_INPUT, SSC_NUMBER, "preFEEDStudy","Pre-FEED study Cost","$","","wobos","?=5000000","","" },
{ SSC_INPUT, SSC_NUMBER, "feedStudy","FEED Study Cost","$","","wobos","?=10000000","","" },
{ SSC_INPUT, SSC_NUMBER, "stateLease","State Leasing and Permitting Cost","$","","wobos","?=250000","","" },
{ SSC_INPUT, SSC_NUMBER, "outConShelfLease","Outer Continental Shelf Lease Cost","$","","wobos","?=1000000","","" },
{ SSC_INPUT, SSC_NUMBER, "saPlan","Site Assessment Plan Cost","$","","wobos","?=500000","","" },
{ SSC_INPUT, SSC_NUMBER, "conOpPlan","Construction Operations Plan Cost","$","","wobos","?=1000000","","" },
{ SSC_INPUT, SSC_NUMBER, "nepaEisMet","NEPA Environmental Impact Statement Met Tower Cost","$","","wobos","?=2000000","","" },
{ SSC_INPUT, SSC_NUMBER, "physResStudyMet","Physical Resource Study Met Tower Cost","$","","wobos","?=1500000","","" },
{ SSC_INPUT, SSC_NUMBER, "bioResStudyMet","Biological Resource Study Met Tower Cost","$","","wobos","?=1500000","","" },
{ SSC_INPUT, SSC_NUMBER, "socEconStudyMet","Socioeconomic and Land use Study Met Tower Cost","$","","wobos","?=500000","","" },
{ SSC_INPUT, SSC_NUMBER, "navStudyMet","Navigation and Transport Study Met Tower Cost","$","","wobos","?=500000","","" },
{ SSC_INPUT, SSC_NUMBER, "nepaEisProj","NEPA Environmental Impact Study Project Cost","$","","wobos","?=5000000","","" },
{ SSC_INPUT, SSC_NUMBER, "physResStudyProj","Physical Resource Study Project Cost","$","","wobos","?=500000","","" },
{ SSC_INPUT, SSC_NUMBER, "bioResStudyProj","Biological Resource Study Porject Cost","$","","wobos","?=500000","","" },
{ SSC_INPUT, SSC_NUMBER, "socEconStudyProj","Socioeconomic and Land use Study Project Cost","$","","wobos","?=200000","","" },
{ SSC_INPUT, SSC_NUMBER, "navStudyProj","Navigation and Transport Study Project Cost","$","","wobos","?=250000","","" },
{ SSC_INPUT, SSC_NUMBER, "coastZoneManAct","Coastal Zone Management Act Compliance Cost","$","","wobos","?=100000","","" },
{ SSC_INPUT, SSC_NUMBER, "rivsnHarbsAct","Rivers & Harbors Act Section 10 Compliance Cost","$","","wobos","?=100000","","" },
{ SSC_INPUT, SSC_NUMBER, "cleanWatAct402","Clean Water Act Section 402 Compliance Cost","$","","wobos","?=100000","","" },
{ SSC_INPUT, SSC_NUMBER, "cleanWatAct404","Clean Water Act Section 404 Compliance Cost","$","","wobos","?=100000","","" },
{ SSC_INPUT, SSC_NUMBER, "faaPlan","Federal Aviation Administration Plans & Mitigation Cost","$","","wobos","?=10000","","" },
{ SSC_INPUT, SSC_NUMBER, "endSpecAct","Endangered Species Act Compliance Cost","$","","wobos","?=500000","","" },
{ SSC_INPUT, SSC_NUMBER, "marMamProtAct","Marine Mammal Protection Act Compliance Cost","$","","wobos","?=500000","","" },
{ SSC_INPUT, SSC_NUMBER, "migBirdAct","Migratory Bird Treaty Compliance Cost","$","","wobos","?=500000","","" },
{ SSC_INPUT, SSC_NUMBER, "natHisPresAct","National Historic Preservation Act Compliance Cost","$","","wobos","?=250000","","" },
{ SSC_INPUT, SSC_NUMBER, "addLocPerm","Additional State and Local Permitting Cost","$","","wobos","?=200000","","" },
{ SSC_INPUT, SSC_NUMBER, "metTowCR","Meteorological (Met Tower Fabrication & Install Cost","$/MW","","wobos","?=11518","","" },
{ SSC_INPUT, SSC_NUMBER, "decomDiscRate","Decommissioning Cost Discount Rate","","","wobos","?=0.03","","" },
//#INPUTS/OUTPUTS: depending on how module is called
//#General
{ SSC_INPUT, SSC_NUMBER, "hubD","Hub Diameter","m","","wobos","?=0","","" },
{ SSC_INPUT, SSC_NUMBER, "bladeL","Blade Length","m","","wobos","?=0","","" },
{ SSC_INPUT, SSC_NUMBER, "chord","Blade Max Chord","m","","wobos","?=0","","" },
{ SSC_INPUT, SSC_NUMBER, "nacelleW","Nacelle Width","m","","wobos","?=0","","" },
{ SSC_INPUT, SSC_NUMBER, "nacelleL","Nacelle Length","m","","wobos","?=0","","" },
{ SSC_INPUT, SSC_NUMBER, "rnaM","Rotor-Nacelle Assembly Mass","tonne","","wobos","?=0","","" },
{ SSC_INPUT, SSC_NUMBER, "towerD","Tower Base Diameter","m","","wobos","?=0","","" },
{ SSC_INPUT, SSC_NUMBER, "towerM","Tower Mass","tonne","","wobos","?=0","","" },
//#Substructure & Foundation outputs
{ SSC_INPUT, SSC_NUMBER, "subTotM","Total Substructure Mass per Turbine","tonne","","wobos","?=0","","" },
{ SSC_INPUT, SSC_NUMBER, "subTotCost","Substructure & Foundation Total Cost","$","","wobos","?=0","","" },
{ SSC_INPUT, SSC_NUMBER, "moorCost","Capital cost of mooring lines and anchors","$","","wobos","?=0","","" },
//#OUTPUTS
//#Electrical Infrastructure outputs
{ SSC_OUTPUT, SSC_NUMBER, "systAngle","Floating System Angle","degrees","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "freeCabLeng","Free Hanging Cable Length","m","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "fixCabLeng","Fixed Cable Length","m","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "nExpCab","Number of Export Cables","","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "cab1Leng","Array Cable #1 Length","m","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "cab2Leng","Array Cabel #2 Length","m","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "expCabLeng","Export Cable Length","m","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "subsTopM","Substation Topside Mass","tonne","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "arrCab1Cost","Array Cable #1 and Ancillary Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "arrCab2Cost","Array Cable #2 and Ancillary Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "expCabCost","Export Cable and Ancillary Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "subsSubM","Offshore Substation Substructure Mass","tonne","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "subsPileM","Offshore Substation Jacket Piles Mass","tonne","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "totElecCost","Total Electrical Infrastructure Cost","$","","wobos","","","" },
//#Assembly & Installation outputs
{ SSC_OUTPUT, SSC_NUMBER, "moorTime","Mooring and Anchor System Installation Time","days","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "floatPrepTime","Floating Preparation Time","days","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "turbDeckArea","Deck Area Required per Turbine","m^2","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "nTurbPerTrip","Maximum Number of Turbines per Vessel Trip","","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "turbInstTime","Turbine Installation Time","days","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "subDeckArea","Deck Area Required per Substructure","m^2","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "nSubPerTrip","Maximum Number of Substructures per Vessel Trip","","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "subInstTime","Substructure Installation Time","days","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "arrInstTime","Array Cable System Installation Time","days","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "expInstTime","Export Cable Installation Time","days","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "subsInstTime","Offshore Substation Installation Time","days","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "totInstTime","Total Installation Time","days","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "totAnICost","Total Assembly & Installation Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "cabSurvey","Cable Route Survey Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "turbine_install_cost","Turbine Install Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "substructure_install_cost","Substructure Install Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "electrical_install_cost","Electrical Install Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "mob_demob_cost","Mobilization/Demobilization Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "array_cable_install_cost","Array Cable Installation Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "export_cable_install_cost","Export Cable Installation Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "substation_install_cost","Substation Installation Cost","$","","wobos","","","" },
//#Port & Staging outputs
{ SSC_OUTPUT, SSC_NUMBER, "totPnSCost","Total Port & Staging Cost","$","","wobos","","","" },
//#Engineering & Management outputs
{ SSC_OUTPUT, SSC_NUMBER, "totEnMCost","Total Engineering & Management Cost","$","","wobos","","","" },
//#Development outputs
{ SSC_OUTPUT, SSC_NUMBER, "totDevCost","Total Development Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "commissioning","Plant Commissioning Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "decomCost","Plant Decommissioning Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "bos_capex","BOS Capital Expenditures","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "soft_costs","Soft Costs","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "total_contingency_cost","Total Contingency Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "construction_insurance_cost","Construction Insurance Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "construction_finance_cost","Construction Finance Cost","$","","wobos","","","" },
{ SSC_OUTPUT, SSC_NUMBER, "construction_finance_factor","Construction Finance Factor","","","wobos","","","" },
//#Total BOS Cost
{ SSC_OUTPUT, SSC_NUMBER, "total_bos_cost","Total Balance of System Cost","$","","wobos","","","" },
var_info_invalid};


class cm_wind_obos : public compute_module
{
public:
	cm_wind_obos()
	{
		add_var_info(vtab_wind_obos);
	}

  wobos obos;
 // vector<var_info> variables_vec;
/*
  cm_wind_obos() {
    obos = wobos();

    for (int k=0; k<obos.wobos_default.variables.size(); k++) {
      
      var_info myvar    = var_info();
      myvar.var_type    = obos.wobos_default.variables[k].isInput() ? SSC_INPUT : SSC_OUTPUT;
      myvar.data_type   = SSC_NUMBER;
      myvar.name        = obos.wobos_default.variables[k].name.c_str();
      myvar.label       = obos.wobos_default.variables[k].description.c_str();
      myvar.units       = obos.wobos_default.variables[k].units_sam.c_str();
      myvar.meta        = "";
      myvar.group       = "wobos";
      std::string rqStr = obos.wobos_default.variables[k].isInput() ? "?="+obos.wobos_default.variables[k].valueStr : "";
      myvar.required_if = rqStr.c_str();
      myvar.constraints = obos.wobos_default.variables[k].constraints.c_str();
      myvar.ui_hint     = "";
      variables_vec.push_back(myvar);
    }
    variables_vec.push_back(var_info_invalid);

    //var_info _cm_vtab_wind_obos[variables_vec.size()];
    //std::copy(variables_vec.begin(), variables_vec.end(), _cm_vtab_wind_obos);
    _cm_vtab_wind_obos = &variables_vec[0];
    add_var_info(_cm_vtab_wind_obos);
  }
  */
  // BEGIN WRAPPER OF LIB_OBOS 
  // **********************************************************************

  void exec() override {
    //Assign inputs********************************************************
	  // use static vartable generated from wind_obos_defaults.csv using wind_obos.lk in SDKTool
	size_t k= 0;

	while (vtab_wind_obos[k].data_type != SSC_INVALID)
	{
		if (vtab_wind_obos[k].var_type == SSC_INPUT)
		{
			const char *vname = vtab_wind_obos[k].name;
			if (vtab_wind_obos[k].data_type == SSC_STRING)
				obos.set_map_variable(vname, as_string(vname));
			else
				obos.set_map_variable(vname, (double)as_number(vname));
		}
		k++;
	}
	/*
    int nvar = variables_vec.size();
    for (int k=0; k<=nvar; k++) {
      if (variables_vec[k].var_type != SSC_INPUT) continue;
      const char *vname = variables_vec[k].name;
      obos.set_map_variable(vname, (double)as_number(vname) );
    }
	*/
    //RUN COMPUTE MODULE***************************************************
    obos.map2variables();
    obos.run();
    obos.variables2map();
		
    //Assign outputs*******************************************************
	k = 0;
	while (vtab_wind_obos[k].data_type != SSC_INVALID)
	{
		if (vtab_wind_obos[k].var_type == SSC_OUTPUT)
		{
			const char *vname = vtab_wind_obos[k].name;
			double myval = obos.get_map_variable(vname);
			assign(vname, var_data((ssc_number_t)(ssc_number_t)myval));
		}
		k++;
	}
	/*
	for (int k=0; k<=nvar; k++) {
      if (variables_vec[k].var_type != SSC_OUTPUT) continue;
      const char *vname = variables_vec[k].name;
      double myval = obos.get_map_variable(vname);
      assign(vname, var_data((ssc_number_t)(ssc_number_t)myval));
    }
	*/
  }
};

DEFINE_MODULE_ENTRY(wind_obos, "Wind Offshore Balance of System cost model", 1)
