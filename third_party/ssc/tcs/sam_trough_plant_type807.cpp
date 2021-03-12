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

#define _TCSTYPEINTERFACE_
#include "tcstype.h"

enum {
	// parameters
	I_Qdesign,
	I_Edesign,
	I_T2EPLF0,
	I_T2EPLF1,
	I_T2EPLF2,
	I_T2EPLF3,
	I_T2EPLF4,
	I_E2TPLF0,
	I_E2TPLF1,
	I_E2TPLF2,
	I_E2TPLF3,
	I_E2TPLF4,
	I_TempCorrF,
	I_TempCorr0,
	I_TempCorr1,
	I_TempCorr2,
	I_TempCorr3,
	I_TempCorr4,
	I_TurTesEffAdj,
	I_TurTesOutAdj,
	I_MinGrOut,
	I_MaxGrOut,
	I_NUMTOU,
	// Array for dispatch periods 1-9
	I_FossilFill,
	I_PbFixPar,
	I_BOPPar,
	I_BOPParPF,
	I_BOPParF0,
	I_BOPParF1,
	I_BOPParF2,
	I_CtPar,
	I_CtParPF,
	I_CtParF0,
	I_CtParF1,
	I_CtParF2,
	I_HtrPar,
	I_HtrParPF,
	I_HtrParF0,
	I_HtrParF1,
	I_HtrParF2,
	I_LHVBoilEff,

	// inputs
	I_Qtpb,
	I_Qfts,
	I_Twetbulb,
	I_Tdrybulb,
	I_CtOpF,
	I_SFTotPar,
	I_EparHhtf,
	I_TOUPeriod,


	// outputs
	O_Enet,
	O_EgrSol,
	O_EMin,
	O_Edump,
	O_Pbload,
	O_EgrFos,
	O_Egr,
	O_Qgas,
	O_HtrLoad,
	O_Epar,
	O_EparPB,
	O_EparBOP,
	O_EparCT,
	O_EparHtr,
	O_EparOffLine,
	O_EparOnLine,

	N_MAX };

tcsvarinfo sam_trough_plant_type807_variables[] = {
	// vartype    datatype    index   name     label    units   meta   group   default_value

	// parameters
	{ TCS_INPUT,  TCS_NUMBER, I_Qdesign,	    "Qdesign", 	      "Design Turbine Thermal Input (MWt)", 					   "MWt",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_Edesign,	    "Edesign", 	      "Design Turbine Gross Output (MWe)", 					   "MWe",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_T2EPLF0,	    "T2EPLF0", 	      "Turbine Part Load Therm to Elec", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_T2EPLF1,	    "T2EPLF1", 	      "Turbine Part Load Therm to Elec", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_T2EPLF2,	    "T2EPLF2", 	      "Turbine Part Load Therm to Elec", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_T2EPLF3,	    "T2EPLF3", 	      "Turbine Part Load Therm to Elec", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_T2EPLF4,	    "T2EPLF4", 	      "Turbine Part Load Therm to Elec", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_E2TPLF0,	    "E2TPLF0", 	      "Turbine Part Load Elec  to Thermal (for fossil backup)", 		   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_E2TPLF1,	    "E2TPLF1", 	      "Turbine Part Load Elec  to Thermal (for fossil backup)", 		   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_E2TPLF2,	    "E2TPLF2", 	      "Turbine Part Load Elec  to Thermal (for fossil backup)", 		   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_E2TPLF3,	    "E2TPLF3", 	      "Turbine Part Load Elec  to Thermal (for fossil backup)", 		   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_E2TPLF4,	    "E2TPLF4", 	      "Turbine Part Load Elec  to Thermal (for fossil backup)", 		   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_TempCorrF,	    "TempCorrF",      "Temperature Correction Mode (0=wetbulb 1=drybulb basis)", 		   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_TempCorr0,	    "TempCorr0",      "Temperature Correction Coefficient 0", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_TempCorr1,	    "TempCorr1",      "Temperature Correction Coefficient 1", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_TempCorr2,	    "TempCorr2",      "Temperature Correction Coefficient 2", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_TempCorr3,	    "TempCorr3",      "Temperature Correction Coefficient 3", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_TempCorr4,	    "TempCorr4",      "Temperature Correction Coefficient 4", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_TurTesEffAdj,   "TurTesEffAdj",   "Turbine TES Adjustment - Efficiency", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_TurTesOutAdj,   "TurTesOutAdj",   "Turbine TES Adjustment - Gross Output", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_MinGrOut,	    "MinGrOut",       "Minimum gross electrical output from powerplant", 			   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_MaxGrOut,	    "MaxGrOut",       "Maximum gross electrical output from powerplant", 			   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_NUMTOU,			"NUMTOU", 	      "Number of time of use periods", 						   "",      "",      "",     "" },
	// array for disaptch periods
	{ TCS_INPUT,  TCS_ARRAY, I_FossilFill,	    "FossilFill",     "Fossil dispatch fraction control", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_PbFixPar,	    "PbFixPar",       "Fixed Power Block Parasitics", 						   "MW",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_BOPPar,			"BOPPar", 	      "Balance of Plant Parasitics", 						   "MWe",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_BOPParPF,	    "BOPParPF",       "Balance of Plant Parasitics - multiplier", 				   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_BOPParF0,	    "BOPParF0",       "Balance of Plant Parasitics - constant", 				   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_BOPParF1,	    "BOPParF1",       "Balance of Plant Parasitics - linear term", 				   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_BOPParF2,	    "BOPParF2",       "Balance of Plant Parasitics - quadratic term", 				   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_CtPar,			"CtPar", 	      "Cooling Tower Parasitics", 						   "MWe",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_CtParPF,	    "CtParPF", 	      "Cooling Tower Parasitics - multiplier", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_CtParF0,	    "CtParF0", 	      "Cooling Tower Parasitics - constant", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_CtParF1,	    "CtParF1", 	      "Cooling Tower Parasitics - linear term", 				   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_CtParF2,	    "CtParF2", 	      "Cooling Tower Parasitics - quadratic term", 				   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_HtrPar,			"HtrPar", 	      "Auxiliary heater/boiler operation parasitics", 				   "MWe",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_HtrParPF,	    "HtrParPF",       "Auxiliary heater/boiler operation parasitics - multiplier", 		   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_HtrParF0,	    "HtrParF0",       "Auxiliary heater/boiler operation parasitics - constant", 		   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_HtrParF1,	    "HtrParF1",       "Auxiliary heater/boiler operation parasitics - linear term", 		   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_HtrParF2,	    "HtrParF2",       "Auxiliary heater/boiler operation parasitics - quadratic term", 		   "",      "",      "",     "" },

	// inputs
	{ TCS_INPUT,  TCS_NUMBER, I_LHVBoilEff,	    "LHVBoilEff",     "Lower Heating Value Boiler Efficiency", 					   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_Qtpb,			"Qtpb", 	      "Heat to Power Block (output from TS/Dispatch type)", 			   "MWt",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_Qfts,			"Qfts", 	      "Heat from Thermal Storage", 						   "MWt",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_Twetbulb,	    "Twetbulb",       "Wet Bulb Temperature", 							   "C",      "",      "",     "" },
// testing with TRNSYS input in lk script and read in in init
//	{ TCS_INPUT,  TCS_ARRAY, I_Twetbulb,	    "Twetbulb",       "Wet Bulb Temperature", 							   "C",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_Tdrybulb,	    "Tdrybulb",       "Ambient Temperature (dry bulb)", 					   "C",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_CtOpF,			"CtOpF", 	      "CT Operation Flag (0 = CT par. a function of load, 1 = CT at full/half)",   "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_SFTotPar,	    "SFTotPar",       "Solar Field Parasitics (EparSF + EparCHTF + EparAnti)", 			   "MWe",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_EparHhtf,	    "EparHhtf",       "Thermal Storage Parasitics", 						   "MWe",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER, I_TOUPeriod,	    "TOUPeriod",      "Current Time of Use Period", 						   "",      "",      "",     "" },



	// outputs
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Enet,		   "Enet", 	    "Net electricity produced, after parasitic loss", 		         "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_EgrSol,		   "EgrSol", 	    "Gross electric production from the solar resource", 		 "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_EMin,		   "EMin", 	    "Solar Electric Generation below minimum required output", 		 "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Edump,		   "Edump", 	    "Solar Electric Generation that is in excess of powerplant max", 	 "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Pbload,		   "Pbload", 	    "Fraction of current Powerblock output to design output", 		 "",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_EgrFos,		   "EgrFos", 	    "Gross electric production from the fossil resource", 		 "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Egr,		   "Egr", 	    "Gross electricity produced, before parasitic loss", 		 "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Qgas,		   "Qgas", 	    "Gas Thermal Energy Input", 					 "MW",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_HtrLoad,		   "HtrLoad", 	    "Auxiliary heater load as ratio vs. rated output", 			 "",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Epar,		   "Epar", 	    "Total Parasitics for entire system", 				 "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_EparPB,		   "EparPB", 	    "Fixed Power Block Parasitics", 					 "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_EparBOP,		   "EparBOP", 	    "Balance of Plant Parasitics", 					 "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_EparCT,		   "EparCT", 	    "Cooling Tower Parasitic Load", 					 "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_EparHtr,		   "EparHtr", 	    "Auxiliary heater parasitic load", 					 "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_EparOffLine,	   "EparOffLine",   "Parasitics incurred while plant is not producing electricity", 	 "MWe",    "",      "",     "" },
	{ TCS_OUTPUT,  TCS_NUMBER,   O_EparOnLine,	   "EparOnLine",    "Parasitics incurred while plant is producing electricity", 	 "MWe",    "",      "",     "" },

	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};


class sam_trough_plant_type807 : public tcstypeinterface
{
private:
	// testing with TRNSYS wetbulb input
	//double *m_twetbulb;
public:
	sam_trough_plant_type807( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{
	}

	virtual ~sam_trough_plant_type807()
	{
	}

	virtual int init()
	{
	// testing TRNSYS input
	//	int len;
	//	m_twetbulb = value(I_Twetbulb, &len);

		return 0;
	}

	virtual int call( double /*time*/, double /*step*/, int /*ncall*/ )
	{
		// inputs
		double Qtpb = value(I_Qtpb);
		double Qfts = value(I_Qfts);

		double Tdrybulb = value(I_Tdrybulb);
	
		double Twetbulb = value(I_Twetbulb);
		// testing TRNSYS input
//		int ndx = (int)(time/step)-1;
//		if ((ndx<0) || (ndx>8759))
//		{
//			// should not be here
//			ndx=-1;
//		}
//		double Twetbulb = m_twetbulb[ndx];  // TRNSYS input testing
		

//                      0 = CT parasitics a function of load
//                      1 = CT at 50% or 100%
		double CtOpF  = value(I_CtOpF);
		double SFTotPar = value(I_SFTotPar);
		double EparHhtf = value(I_EparHhtf);
		int TOUPeriod = (int)value(I_TOUPeriod)-1;


		// parameters
		double Qdesign = value(I_Qdesign);
		double Edesign = value(I_Edesign);
		double T2EPLF0 = value(I_T2EPLF0);
		double T2EPLF1 = value(I_T2EPLF1);
		double T2EPLF2 = value(I_T2EPLF2);
		double T2EPLF3 = value(I_T2EPLF3);
		double T2EPLF4 = value(I_T2EPLF4);
		double E2TPLF0 = value(I_E2TPLF0);
		double E2TPLF1 = value(I_E2TPLF1);
		double E2TPLF2 = value(I_E2TPLF2);
		double E2TPLF3 = value(I_E2TPLF3);
		double E2TPLF4 = value(I_E2TPLF4);
		double TempCorrF = value(I_TempCorrF) + 1;	// Input table convention is 0=wet, 1=dry, so add 1 for calculations
		double TempCorr0 = value(I_TempCorr0);
		double TempCorr1 = value(I_TempCorr1);
		double TempCorr2 = value(I_TempCorr2);
		double TempCorr3 = value(I_TempCorr3);
		double TempCorr4 = value(I_TempCorr4);
		double TurTesEffAdj = value(I_TurTesEffAdj);
		//double TurTesOutAdj = value(I_TurTesOutAdj);
		double MinGrOut = value(I_MinGrOut);
		double MaxGrOut = value(I_MaxGrOut);
		//double NUMTOU = value(I_NUMTOU);

//Fossil Operation: This fills gas use to this fraction for every hour of month
// len should be 9 or current number of dispatch periods
		int len;
		double *FossilFill = value( I_FossilFill, &len );


		double PbFixPar = value(I_PbFixPar);
		double BOPPar = value(I_BOPPar);
		//double BOPParPF = value(I_BOPParPF);
		double BOPParF0 = value(I_BOPParF0);
		double BOPParF1 = value(I_BOPParF1);
		double BOPParF2 = value(I_BOPParF2);
		double CtPar = value(I_CtPar);
		//double CtParPF = value(I_CtParPF);
		double CtParF0 = value(I_CtParF0);
		double CtParF1 = value(I_CtParF1);
		double CtParF2 = value(I_CtParF2);
		double HtrPar = value(I_HtrPar);
		//double HtrParPF = value(I_HtrParPF);
		double HtrParF0 = value(I_HtrParF0);
		double HtrParF1 = value(I_HtrParF1);
		double HtrParF2 = value(I_HtrParF2);
		double LHVBoilEff = value(I_LHVBoilEff);

//  Power Plant Calculation
   
// Design Point Electric Generation (10-1-03)
// EgrSol in MWe gross generation
// Nth - Normalized thermal to power cycle
// Nel - Normalized electric output

		double Nth = Qtpb / Qdesign;
		double Nel = T2EPLF0 + T2EPLF1 * Nth + T2EPLF2 * pow(Nth,2) + T2EPLF3 * pow(Nth,3) + T2EPLF4 * pow(Nth,4);
		double EgrSol = Edesign * Nel;

		double Ttc = 0;
		double Ntc = 0;
		double Emin = 0;
		double Edump = 0;
		double PbLoad = 0;
		double EgrFos = 0;
		double GN = 0;
		double Qgas = 0;
		double Egr = 0;
		double HtrLoad = 0;
		double EparHtr = 0;
		double EparPb = 0;
		double EparBop = 0;
		double Epar = 0;
		double EparCt = 0;
		double EparOffLine = 0;
		double EparOnLine = 0;
		double Enet;
      
//  Electric Generation Temperature Correction (10-1-03)
//  If TempCorrF = 1 then
//     temperature correction is based on the Wet Bulb Temp (make sure Twetbulb is in data set
//  If TempCorrF = 2 then
//     temperature correction is based on the Dry Bulb Temp
//  Else No Correction

		if ( (TempCorrF==1) || (TempCorrF==2))
		{
			 if (TempCorrF==1)
				Ttc = Twetbulb;
			 else
				Ttc = Tdrybulb;
			 Ntc = TempCorr0 + TempCorr1 * Ttc + TempCorr2 * pow(Ttc,2) + TempCorr3 * pow(Ttc,3) + TempCorr4 * pow(Ttc,4);
		}
		else
		{
			 Ttc = 0;
			 Ntc = 1;
		}
		EgrSol = EgrSol * Ntc;
      
		//   Correction for TES  10-1-03
		if (Qtpb>0)	EgrSol = EgrSol * ((1 - Qfts / Qtpb) + Qfts / Qtpb * TurTesEffAdj);
      
		Emin = 0.0;
		Edump = 0.0;
      
		if (EgrSol<(Edesign * MinGrOut))  //  if the solar provided is less than the minimum needed to run the turbine
		{
			 if (EgrSol > 0.0) Emin = EgrSol;            //  then set the emin equal to the solar provided
			 EgrSol = 0.0;
		}
		else
		{
			 if (EgrSol > (Edesign * MaxGrOut)) //  if the solar provided is greater= than the maximum used by the turbine
			 {
				Edump =  EgrSol - (Edesign * MaxGrOut); //  then dump the extra 
				EgrSol = Edesign * MaxGrOut;
			 }
		}  
		
		PbLoad = EgrSol / Edesign; //  what is the fraction of the solar output compared to the design point 
   
		//  FOSSIL BACKUP SECTION

		if (EgrSol < (FossilFill[TOUPeriod] * Edesign)) //  if the solar provided is less than the fraction of fossil required.
		{
			 EgrFos = Edesign * FossilFill[TOUPeriod] - EgrSol; //  then the fossil used is the maximum amount minus the solar provided ???
			 // If (FossilFill(TOUPeriod).EQ.0) Then
			 if (FossilFill[TOUPeriod] < 0.99)   // MJW 11/20/09  decide whether to use full-load turbine model or part load turbine model for Qgas calculation
			 {
				GN = (EgrSol + EgrFos) / Edesign;
				Qgas = (Qdesign * (E2TPLF0 + E2TPLF1*GN + E2TPLF2*pow(GN,2) + E2TPLF3*pow(GN,3) + E2TPLF4*pow(GN,4)) - Qtpb) / LHVBoilEff; //  .9 is boiler LHV Efficiency
			 }
			 else
			 {
				Qgas = EgrFos * Qdesign / Edesign / LHVBoilEff; //  .9 is boiler LHV Efficiency
			 }
		}
		else
		{
			  EgrFos = 0;
			  Qgas = 0;
		}


		HtrLoad = EgrFos / Edesign;    // First Order Estimate of the fraction of design output due to fossil
		PbLoad = (EgrSol + EgrFos) / Edesign; //  this is the amount of design load met by both fossil and solar
		Egr = EgrFos + EgrSol; // the gross electric output is the sum of solar and fossil
      

		//   Heater Parasitics
   
		if (HtrLoad>0)               //  Solar Field in Operation
		{
		//       EparHtr = HtrPar * HtrParPF * (HtrParF0 + HtrParF1 * HtrLoad + HtrParF2 * (HtrLoad**2))	
			  EparHtr = HtrPar  * (HtrParF0 + HtrParF1 * HtrLoad + HtrParF2 * pow(HtrLoad,2));	// HP Sam input accounts for PF 12-12-06
		}
		else                               //  Heater is not in operation
		{
			  EparHtr = 0;
		}
		//    Power Block Parasitics
   
		EparPb = PbFixPar;               //  Fixed Power Block Parasitics (24 hr)
   
		if (PbLoad > 0)             //  Power Block is in Operation
		{
		//   Turbine Cycle Plant Parasitics (BOP)
		// 		EparBop = BopPar * BopParPF * (BopParF0 + BopParF1 * PbLoad + BopParF2 * (PbLoad**2))
			EparBop = BOPPar * (BOPParF0 + BOPParF1 * PbLoad + BOPParF2 * pow(PbLoad,2));	// HP Sam input accounts for PF 12-12-06
      
			//   Cooling Tower Parasitics
			if (CtOpF == 0)               //   CtOptF - when 1 runs at 50% or 100% only
			{
		// 		  EparCt = CtPar * CtParPF * (CtParF0 + CtParF1 * PbLoad + CtParF2 * (PbLoad**2))
				  EparCt = CtPar  * (CtParF0 + CtParF1 * PbLoad + CtParF2 * pow(PbLoad,2));	// HP Sam input accounts for PF 12-12-06
			}
			else                              //   Hot HTF Pumps (HTF from TS to PB)
			{
				 if (PbLoad <= 0.5)
				 {
		// 			EparCt = CtPar * CtParPF * 0.5	// HP Sam input accounts for PF 12-12-06
					EparCt = CtPar * 0.5;
				 }
				 else
				 {
		// 			EparCt = CtPar * CtParPF	// HP Sam input accounts for PF 12-12-06
					EparCt = CtPar;
				 }
			}
		}
		else                                //  Power Block is not in operation
		{
		   EparCt = 0;                       //  No CT Operation
		   EparBop = 0;                      //  No BOP Operation
		}

		//  sum all the parasitics including current component, solar field and thermal storage
		Epar = SFTotPar + EparHtr + EparHhtf + EparBop + EparCt + EparPb;

		if (PbLoad == 0) 
		{
		   EparOffLine =  Epar;  //  if powerblock not running, then all parasitics are offline parasitics
		   EparOnLine = 0;
		}
		else
		{
			if ((Egr - Epar) > 0) //  if powerblock running and gross output exceeds parasitics 
			{
				EparOnLine =  Epar;
				EparOffLine = 0;
			}
			else //  if powerblock is running but the gross output does NOT exceed parasitics (why is it running then??)
			{
				EparOnLine =  Egr;
				EparOffLine =  Epar -  Egr;
			}
		}
   
		//  Calculate the net plant electric production by subtracting all the parasitics 
		//  including the solar field and thermal storage 
		Enet = Egr - Epar;

   
		//  set outputs
		value(O_Enet, Enet);        // | Net Electric Energy Production (Gross-Parasitics)              |      MWe       |     MWe
		value(O_EgrSol, EgrSol);        // | Gross Solar Electric Generation                                |      MW        |     MW
		value(O_EMin, Emin);          // | Solar Electric Generation below minimum powerplant output      |      MW        |     MW
		value(O_Edump, Edump);         // | Solar Electric Generation that is in excess of powerplant max  |      MW        |     MW
		value(O_Pbload, PbLoad);        // | Fraction of current Powerblock output to design output         | Dimensionless  | Dimensionless
		value(O_EgrFos, EgrFos);        // | Gross Fossil Electric Generation                               |      MW        |     MW
		value(O_Egr, Egr);           // | Gross Total Electric Generation                                |      MW        |     MW
		value(O_Qgas, Qgas);          // | Gas Thermal Energy Input                                       |      MW        |     MW
		value(O_HtrLoad, HtrLoad);       // | Heater Load Factor vs. rated output                            | Dimensionless  | Dimensionless
		value(O_Epar, Epar);         //  | Total Parasitics for entire system                            |      MW        |     MW
		value(O_EparPB, EparPb);        // | Fixed Power Block Parasitics (24 hr)
		value(O_EparBOP, EparBop);       // | Balance of Plant Parasitics                                    |      MW        |     MW
		value(O_EparCT, EparCt);        // | Cooling Tower Parasitic Loads                                  |      MW        |     MW
		value(O_EparHtr, EparHtr);       // | Heater Parasitics ???
		value(O_EparOffLine, EparOffLine);   // | Offline Parasitics ???
		value(O_EparOnLine, EparOnLine);    // | Online Parasitics ???

		return 0;
	}
};

TCS_IMPLEMENT_TYPE( sam_trough_plant_type807, "SAM Trough Plant", "Steven Janzou", 1, sam_trough_plant_type807_variables, NULL, 0 )

