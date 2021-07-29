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
//#include <shared/lib_util.h>
#include "lib_util.h"

#ifndef MAX
#define MAX(a,b) ( (a)>(b) ? (a) : (b) )
#endif

#ifndef MIN
#define MIN(a,b) ( (a)<(b) ? (a) : (b) )
#endif

enum {
	I_TsHours,
	I_NumTOU,
	I_E2TPLF0,
	I_E2TPLF1,
	I_E2TPLF2,
	I_E2TPLF3,
	I_E2TPLF4,
	// matrix - first index is dispatch periods (9) and second is type of dispatch  without solar (,1), with solar (,2), turbine load (,3)
	I_TSLOGIC,  
	I_E_TES_INI,

	I_Qsf,
	I_TOUPeriod,
	I_TnkHL,
	I_PTSmax,
	I_PFSmax,
	I_PTTMAX,
	I_PTTMIN,
	I_TurSUE,
	I_Qdesign,
	I_HhtfPar,
	I_HhtfParPF,
	I_HhtfParF0,
	I_HhtfParF1,
	I_HhtfParF2,
	I_QhtfFreezeProt,

	O_Qtts,
	O_Qfts,
	O_Ets,
	O_QTsHl,
	O_Qtpb,
	O_QTsFull,
	O_Qmin,
	O_Qdump,
	O_QTurSu,
	O_PbStartF,
	O_HhtfLoad,
	O_EparHhtf,
	O_PBMode,
	O_QhtfFpTES,
	O_QhtfFpHtr,
 O_tslogic00, 
 O_tslogic01, 
 O_tslogic02, 
 O_tslogic10, 
 O_tslogic11, 
 O_tslogic12, 
 O_tslogic20, 
 O_tslogic21, 
 O_tslogic22, 
 O_tslogic30, 
 O_tslogic31, 
 O_tslogic32, 
 O_tslogic40, 
 O_tslogic41, 
 O_tslogic42, 
 O_tslogic50, 
 O_tslogic51, 
 O_tslogic52, 
 O_tslogic60, 
 O_tslogic61, 
 O_tslogic62, 
 O_tslogic70, 
 O_tslogic71, 
 O_tslogic72, 
 O_tslogic80, 
 O_tslogic81, 
 O_tslogic82, 
	N_MAX };


double max( double a, double b )
{
	return (a > b) ? a : b;
}


tcsvarinfo sam_trough_storage_type806_variables[] = {
	// vartype    datatype    index   name     label    units   meta   group   default_value

	// parameters
	{ TCS_INPUT,  TCS_NUMBER,  I_TsHours,	    "TSHOURS", 	      "Number of equivalent full-load hours of thermal storage", 	       "hours",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_NumTOU,	    "NUMTOU", 	      "Number of time-of-use periods", 					       "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_E2TPLF0,	    "E2TPLF0", 	      "Turbine part-load electric to thermal conversion (fossil) - const",     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_E2TPLF1,	    "E2TPLF1", 	      "Turbine part-load electric to thermal conversion (fossil) - linear",    "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_E2TPLF2,	    "E2TPLF2", 	      "Turbine part-load electric to thermal conversion (fossil) - quad.",     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_E2TPLF3,	    "E2TPLF3", 	      "Turbine part-load electric to thermal conversion (fossil) - cubic",     "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_E2TPLF4,	    "E2TPLF4", 	      "Turbine part-load electric to thermal conversion(fossil) - quartic",    "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_MATRIX,  I_TSLOGIC,	    "TSLogic", 	      "Dispatch logic without solar (,1), with solar (,2), turbine load (,3)",    "",      "",      "",     "" },
	{ TCS_PARAM,  TCS_NUMBER,  I_E_TES_INI,     "E_tes_ini",      "Initial amount of energy in thermal storage - fraction of max storage energy", "-",     "",      "",     "" },
	
	// inputs
	{ TCS_INPUT,  TCS_NUMBER,  I_Qsf,	       "Qsf", 	         "Thermal energy available from the solar field", 			       "MWt",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_TOUPeriod,	   "TOUPeriod",      "The time-of-use period", 					       "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_TnkHL,	       "TnkHL", 	      "Tank heat losses", 						       "MWt",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_PTSmax,	    "PTSmax", 	      "Maximum power rate into the thermal storage", 			       "MWt",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_PFSmax,	    "PFSmax", 	      "Maximum discharge rate of power from storage", 			       "MWt",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_PTTMAX,	    "PTTMAX", 	      "Maximum ratio of turbine operation", 				       "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_PTTMIN,	    "PTTMIN", 	      "Minimum turbine turn-down fraction", 				       "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_TurSUE,	    "TurSUE", 	      "Equivalent full-load hours required for turbine startup", 	       "hours",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_Qdesign,	    "Qdesign", 	      "Thermal input to the power block under design conditions", 	       "MWt",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_HhtfPar,	    "HhtfPar", 	      "TES HTF pump parasitics", 					       "MWe",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_HhtfParPF,	    "HhtfParPF",      "Part-load TES HTF pump parasitics - multiplier", 		       "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_HhtfParF0,	    "HhtfParF0",      "Part-load TES HTF pump parasitics - constant coef", 		       "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_HhtfParF1,	    "HhtfParF1",      "Part-load TES HTF pump parasitics - linear coef", 		       "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_HhtfParF2,	    "HhtfParF2",      "Part-load TES HTF pump parasitics - quadratic coef", 		       "",      "",      "",     "" },
	{ TCS_INPUT,  TCS_NUMBER,  I_QhtfFreezeProt,"QhtfFreezeProt", "HTF Freeze Protection Requirement (from 805)", 			       "MWt",      "",      "",     "" },

	// outputs
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Qtts, 	     "Qtts", 	       "Energy to Thermal Storage", 					    "MWt",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Qfts, 	     "Qfts", 	       "Energy from Thermal Storage", 					    "MWt",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Ets, 	     "Ets", 	       "Energy in Thermal Storage", 					    "MWt.hr",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_QTsHl, 	     "QTsHl", 	       "Energy losses from Thermal Storage", 				    "MWt",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Qtpb, 	     "Qtpb", 	       "Energy to the Power Block", 					    "MWt",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_QTsFull,      "QTsFull",        "Energy dumped because the thermal storage is full", 		    "MWt",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Qmin, 	     "Qmin", 	       "Energy dumped due to minimum load requirement", 		    "MWt",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_Qdump, 	     "Qdump", 	       "The amount of energy dumped (more than turbine and storage)", 	    "MWt",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_QTurSu, 	     "QTurSu", 	       "The energy needed to startup the turbine", 			    "MWt",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_PbStartF,     "PbStartF",       "Power block startup flag (1 = starting up, 0 = not starting up)",   "",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_HhtfLoad,     "HhtfLoad",       "Hot HTF pump load (energy from storage)", 			    "MWe",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_EparHhtf,     "EparHhtf",       "Hot HTF pump parasitics", 					    "MWe",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_PBMode, 	     "PBMode", 	       "Power block mode (0 = off, 1 = startup, 2 = running)", 		    "",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_QhtfFpTES,    "QhtfFpTES",      "Thermal energy storage freeze protection energy", 		    "MWt",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_QhtfFpHtr,    "QhtfFpHtr",      "Freeze protection provided by auxiliary heater", 		    "MWt",    "",      "",     "" }, 

// testing
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic00,    "O_tslogic00",      "m_TSLogic(0,0)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic01,    "O_tslogic01",      "m_TSLogic(0,1)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic02,    "O_tslogic02",      "m_TSLogic(0,2)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic10,    "O_tslogic10",      "m_TSLogic(1,0)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic11,    "O_tslogic11",      "m_TSLogic(1,1)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic12,    "O_tslogic12",      "m_TSLogic(1,2)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic20,    "O_tslogic20",      "m_TSLogic(2,0)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic21,    "O_tslogic21",      "m_TSLogic(2,1)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic22,    "O_tslogic22",      "m_TSLogic(2,2)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic30,    "O_tslogic30",      "m_TSLogic(3,0)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic31,    "O_tslogic31",      "m_TSLogic(3,1)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic32,    "O_tslogic32",      "m_TSLogic(3,2)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic40,    "O_tslogic40",      "m_TSLogic(4,0)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic41,    "O_tslogic41",      "m_TSLogic(4,1)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic42,    "O_tslogic42",      "m_TSLogic(4,2)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic50,    "O_tslogic50",      "m_TSLogic(5,0)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic51,    "O_tslogic51",      "m_TSLogic(5,1)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic52,    "O_tslogic52",      "m_TSLogic(5,2)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic60,    "O_tslogic60",      "m_TSLogic(6,0)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic61,    "O_tslogic61",      "m_TSLogic(6,1)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic62,    "O_tslogic62",      "m_TSLogic(6,2)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic70,    "O_tslogic70",      "m_TSLogic(7,0)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic71,    "O_tslogic71",      "m_TSLogic(7,1)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic72,    "O_tslogic72",      "m_TSLogic(7,2)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic80,    "O_tslogic80",      "m_TSLogic(8,0)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic81,    "O_tslogic81",      "m_TSLogic(8,1)", 	"",    "",      "",     "" }, 
	{ TCS_OUTPUT,  TCS_NUMBER,   O_tslogic82,    "O_tslogic82",      "m_TSLogic(8,2)", 	"",    "",      "",     "" }, 



	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};


class sam_trough_storage_type806 : public tcstypeinterface
{
private:
	int m_PBMode0; // previous timestep
	double m_TurSuE0; // previous timestep
	double m_Ets0; // previous timestep
	util::matrix_t<double> m_TSLogic;
	tcsvalue *m_TSLogicin;
	double m_TSHOURS;
	int m_NUMTOU;
	double m_E2TPLF0;
	double m_E2TPLF1; 
	double m_E2TPLF2; 
	double m_E2TPLF3; 
	double m_E2TPLF4; 
	double m_ESMAX;
	double m_PTTMAXin;
	double m_PTTMINin;
	

public:
	sam_trough_storage_type806( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{
	}

	virtual ~sam_trough_storage_type806()
	{
	}

	virtual int init()
	{
		// implicit initializations in TRSNYS
		m_PBMode0 = 0; 
		m_TurSuE0 = 0;
		//m_Ets0 = 0;

		// 4.17.14, twn: initial storage energy should be initialized from UI value
		m_TSHOURS = value(I_TsHours);			//  Hours of Thermal Storage
		double Qdesign = value(I_Qdesign);		// [MWth] Design thermal input to power cycle
		double E_tes_max = Qdesign*m_TSHOURS;	// [MWth-hr] Maximum stored thermal energy
		double f_tes_ini = value(I_E_TES_INI);	// [-] Fraction of max stored thermal energy at initialization
		if( f_tes_ini < 0.0 )
		{
			message(TCS_WARNING, "Fraction of TES at initialization was less than 0: %d. It was reset to the minimum of 0 for this simulation", f_tes_ini);
			f_tes_ini = 0.0;
		}
		else if( f_tes_ini > 1.0)
		{
			message(TCS_WARNING, "Fraction of TES at initialization was greater than 1: %d. It was reset to the maximum of 1 for this simulation", f_tes_ini);
			f_tes_ini = 1.0;
		}
		m_Ets0 = f_tes_ini*E_tes_max;	// [MWth-hr] Initial stored thermal energy
		//***********************************************************************************************
		//***********************************************************************************************

		m_TSLogicin = var( I_TSLOGIC );
		int tsl_rows = 0, tsl_cols = 0;
		value( I_TSLOGIC, &tsl_rows, &tsl_cols );
		m_TSLogic.resize( tsl_rows, tsl_cols-1 );

		

		m_PTTMAXin = value(I_PTTMAX);
		m_PTTMINin = value(I_PTTMIN);

		//  Always Read parameters		
		m_NUMTOU = (int)value(I_NumTOU);
		m_E2TPLF0 = value(I_E2TPLF0);       //  | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless
		m_E2TPLF1 = value(I_E2TPLF1);       //  | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless
		m_E2TPLF2 = value(I_E2TPLF2);       //  | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless
		m_E2TPLF3 = value(I_E2TPLF3);       //  | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless
		m_E2TPLF4 = value(I_E2TPLF4);       //  | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless

		return 0;
	}

	virtual int call( double /*time*/, double /*step*/, int /*ncall*/ )
	{

		double Qsf = value(I_Qsf);
		int TOUperiod = (int)value(I_TOUPeriod)-1; // control value between 1 & 9, have to change to 0-8 for array index
		double TnkHL = value(I_TnkHL);
		double PTSMax = value(I_PTSmax);
		double PFSMAX = value(I_PFSmax);
		double TurSUE  = value(I_TurSUE);
		// m_ESMAX     = XIN(10)
		double HhtfPar = value(I_HhtfPar);        // Hot HTF pump parasitics coefficient 1.1000
		//double HhtfParPF = value(I_HhtfParPF);      //  Hot HTF Pump parasitics coefficient 1.000
		double HhtfParF0 = value(I_HhtfParF0);      //  Hot HTF Pump parasitics coefficient	-0.036
		double HhtfParF1 = value(I_HhtfParF1);      //  Hot HTF Pump parasitics coefficient	0.242
		double HhtfParF2 = value(I_HhtfParF2);			//  Hot HTF Pump parasitics coefficient	0.794
		double QhtfFreezeProt = value(I_QhtfFreezeProt);			//  HTF Freeze Protection 

		int PBMode=m_PBMode0;


		// since Qdesign connected to trough model - the following code cannot be in the init function
		double Qdesign = value(I_Qdesign);
		m_ESMAX = m_TSHOURS*Qdesign;     // 10-4-06 m_ESMAX now calculated and not an input

		for (int p = 0; p<m_NUMTOU; p++)
		{
/*
			m_TSLogic.at(p, 0) = TCS_MATRIX_INDEX(m_TSLogicin, p, 0) * m_ESMAX;
			m_TSLogic.at(p, 1) = TCS_MATRIX_INDEX(m_TSLogicin, p, 1) * m_ESMAX;
			m_TSLogic.at(p, 2) = Qdesign * (m_E2TPLF0 + m_E2TPLF1 * TCS_MATRIX_INDEX(m_TSLogicin, p, 2) + m_E2TPLF2 * pow(TCS_MATRIX_INDEX(m_TSLogicin, p, 2), 2) + m_E2TPLF3 * pow(TCS_MATRIX_INDEX(m_TSLogicin, p, 2), 3) + m_E2TPLF4 * pow(TCS_MATRIX_INDEX(m_TSLogicin, p, 2), 4));
			//  Check to make sure that dispatch logic operates within the turbine max and min
			if (TCS_MATRIX_INDEX(m_TSLogicin, p, 2)>m_PTTMAXin) m_TSLogic.at(p, 2) = Qdesign * (m_E2TPLF0 + m_E2TPLF1 * m_PTTMAXin + m_E2TPLF2 * pow(m_PTTMAXin, 2) + m_E2TPLF3 * pow(m_PTTMAXin, 3) + m_E2TPLF4 * pow(m_PTTMAXin, 4));
			if (TCS_MATRIX_INDEX(m_TSLogicin, p, 2)<m_PTTMINin) m_TSLogic.at(p, 2) = Qdesign * (m_E2TPLF0 + m_E2TPLF1 * m_PTTMINin + m_E2TPLF2 * pow(m_PTTMINin, 2) + m_E2TPLF3 * pow(m_PTTMINin, 3) + m_E2TPLF4 * pow(m_PTTMINin, 4));
*/
			m_TSLogic.at(p, 0) = TCS_MATRIX_INDEX(m_TSLogicin, p, 1) * m_ESMAX;
			m_TSLogic.at(p, 1) = TCS_MATRIX_INDEX(m_TSLogicin, p, 2) * m_ESMAX;
			m_TSLogic.at(p, 2) = Qdesign * (m_E2TPLF0 + m_E2TPLF1 * TCS_MATRIX_INDEX(m_TSLogicin, p, 3) + m_E2TPLF2 * pow(TCS_MATRIX_INDEX(m_TSLogicin, p, 3), 2) + m_E2TPLF3 * pow(TCS_MATRIX_INDEX(m_TSLogicin, p, 3), 3) + m_E2TPLF4 * pow(TCS_MATRIX_INDEX(m_TSLogicin, p, 3), 4));
			//  Check to make sure that dispatch logic operates within the turbine max and min
			if (TCS_MATRIX_INDEX(m_TSLogicin, p, 3)>m_PTTMAXin) m_TSLogic.at(p, 2) = Qdesign * (m_E2TPLF0 + m_E2TPLF1 * m_PTTMAXin + m_E2TPLF2 * pow(m_PTTMAXin, 2) + m_E2TPLF3 * pow(m_PTTMAXin, 3) + m_E2TPLF4 * pow(m_PTTMAXin, 4));
			if (TCS_MATRIX_INDEX(m_TSLogicin, p, 3)<m_PTTMINin) m_TSLogic.at(p, 2) = Qdesign * (m_E2TPLF0 + m_E2TPLF1 * m_PTTMINin + m_E2TPLF2 * pow(m_PTTMINin, 2) + m_E2TPLF3 * pow(m_PTTMINin, 3) + m_E2TPLF4 * pow(m_PTTMINin, 4));
		}

		//int nrows, ncols;
		//double *m_TSLogic = value( I_TSLOGIC, &nrows, &ncols );		
		//double *m_TSLogicin = value( I_TSLOGIC, &nrows, &ncols );		

		/*
		int nstart = 7
		do p = 1, m_NUMTOU
			m_TSLogicin(p,1) = PAR(nStart+3*p-2) //  read in the with Sol level for each TOU period
			m_TSLogicin(p,2) = PAR(nStart+3*p-1) //  read in the without Sol level for each TOU period
			m_TSLogicin(p,3) = PAR(nstart+3*p)   //  read in the  load level for each TOU period
			m_TSLogic(p,1) = m_TSLogicin(p,1) * m_ESMAX
			m_TSLogic(p,2) = m_TSLogicin(p,2) * m_ESMAX
			m_TSLogic(p,3) = Qdesign * (m_E2TPLF0 + m_E2TPLF1 * m_TSLogicin(p,3) + m_E2TPLF2 * m_TSLogicin(p,3)**2 + m_E2TPLF3 * m_TSLogicin(p,3)**3 + m_E2TPLF4 * m_TSLogicin(p,3)**4)
		//  Check to make sure that dispatch logic operates within the turbine max and min
			If (m_TSLogicin(p,3)>m_PTTMAXin) m_TSLogic(p,3) = Qdesign * (m_E2TPLF0 + m_E2TPLF1 * m_PTTMAXin + m_E2TPLF2 * m_PTTMAXin**2 + m_E2TPLF3 * m_PTTMAXin**3 + m_E2TPLF4 * m_PTTMAXin**4)
			If (m_TSLogicin(p,3)<m_PTTMINin) m_TSLogic(p,3) = Qdesign * (m_E2TPLF0 + m_E2TPLF1 * m_PTTMINin + m_E2TPLF2 * m_PTTMINin**2 + m_E2TPLF3 * m_PTTMINin**3 + m_E2TPLF4 * m_PTTMINin**4)
		enddo
		*/
		// Set QTTMAX to be the actual thermal max energy possible to the turbine
		double QTTMAX = Qdesign * (m_E2TPLF0 + m_E2TPLF1 * m_PTTMAXin + m_E2TPLF2 * pow(m_PTTMAXin,2) + m_E2TPLF3 * pow(m_PTTMAXin,3) + m_E2TPLF4 * pow(m_PTTMAXin,4));
		double QTTMIN = Qdesign * (m_E2TPLF0 + m_E2TPLF1 * m_PTTMINin + m_E2TPLF2 * pow(m_PTTMINin,2) + m_E2TPLF3 * pow(m_PTTMINin,3) + m_E2TPLF4 * pow(m_PTTMINin,4));



		double Delt = 1.0; // Aron?? set to actual time step

		// Delt is decimal fraction of hour to produce 1, 2 or 4 timesteps/hr
		// NB TimeSteps = 1.0 / Delt
		int TimeSteps = int(1.0 / Delt);
		//  initialize outputs to 0
		double Qtts      = 0;     // | Energy to Thermal Storage                                      |      MW        |     MW
		double Qfts      = 0;     // | Energy from Thermal Storage                                    |      MW        |     MW
		double Ets       = 0;     // | Energy in Thermal Storage
		double QTsHl     = 0;     // | Energy losses from Thermal Storage
		double Qtpb      = 0;     // | Energy to the Power Block
		double QTsFull   = 0;     // | Energy dumped because the thermal storage is full
		double Qmin      = 0;     // | Indicator of being below minimum operation level
		double Qdump     = 0;     // | The amount of energy dumped (more than turbine and storage)
		double QTurSu    = 0;     // | The energy needed to startup the turbine
		double PbStartF  = 0;     // | is 1 during the period when powerblock starts up otherwise 0
		double HhtfLoad  = 0;     // | Hot HTF pump load (energy from storage)                        | Fraction between 0 and 1
		double EparHhtf  = 0;     // | Hot HTF pump parasitics										|      MWhe 
		double QhtfFpTES = 0;     // | HTF Freeze Protection from Thermal Eneryg Storage				|      MWht HP 12-12-06
		double QhtfFpHtr = 0;	  // | HTF Freeze Protection from Auxiliary Heater					|      MWht 

		double TStemp;

		int p;
		//    Select Case m_TSHOURS  ' Evaluate m_TSHOURS - TS dispatch strategy if storage is present
		if ( m_TSHOURS <= 0)  //  No Storage
		{
			if ((m_PBMode0==0) || (m_PBMode0==1))  //  if plant is not already operating in last timestep
			{
				if (Qsf>0) 
				{
					if (Qsf>(m_TurSuE0 * TimeSteps))  //   Starts plant as exceeds startup energy needed
					{
						Qtpb = Qsf - m_TurSuE0 * TimeSteps;
						QTurSu = m_TurSuE0 * TimeSteps;
						PBMode = 2;
						PbStartF = 1;
						m_TurSuE0 = 0.;		//mjw 5/31/13 Reset the startup energy to zero here.
					}
					else //   Plant starting up but not enough energy to make it run - will probably finish in the next timestep
					{
						Qtpb = 0;
						m_TurSuE0 = m_TurSuE0 - Qsf / TimeSteps;
						QTurSu = Qsf;
						PBMode = 1;
						PbStartF = 0;
					}
				}
				else //  No solar field output so still need same amount of energy as before and nothing changes
				{
					m_TurSuE0 = TurSUE * Qdesign;
					PBMode = 0;
					PbStartF = 0;
				}      
			}
			else //  if the powerblock mode is already 2 (running previous timestep)
			{
				if (Qsf>0)      //  Plant operated last hour and this one
				{
					Qtpb = Qsf;          //  all power goes from solar field to the powerblock
					PBMode = 2;          //  powerblock continuing to operate
					PbStartF = 0;        //  powerblock did not start during this timestep
				}
				else                   //   Plant operated last hour but not this one
				{
					Qtpb = 0;            //  No energy to the powerblock
					PBMode = 0;          //  turned off powrblock
					PbStartF = 0;        //  it didn't start this timeperiod 
					m_TurSuE0 = m_TurSuE0 - Qsf / TimeSteps; //  Qsf is 0 so this statement is confusing
				}
			}      
			//  following happens no matter what state the powerblock was in previously      
			HhtfLoad = 0;
		//  This happens after convergence      PbMode0 = PbMode  //  set the value for the next period 
      
      
			if (Qtpb<QTTMIN)  //  Energy to powerblock less than the minimum that the turbine can run at
			{
				Qmin =  Qtpb;         //  The minimum energy (less than the minimum)
				Qtpb = 0;             //  Energy to PB is now 0
				PBMode = 0;           //  PB turned off
			}

			if (Qtpb>QTTMAX)    //  Energy to powerblock greater than what the PB can handle (max)
			{
				Qdump =  Qtpb - QTTMAX; //  The energy dumped 
				Qtpb = QTTMAX;          //  the energy to the PB is exactly the maximum
			}

			QhtfFpHtr = QhtfFreezeProt;
		}
		//       Case 1   ' Solergy Dispatch Approach
		else if (m_TSHOURS>0)  
		{
			p = TOUperiod;

			//  initialize a bunch of values
			QTurSu = 0;
			PbStartF = 0;
			QTsHl = TnkHL; //  thermal storage heat losses are equal to the tank losses
			Qdump = 0;
			Qfts = 0;						//  HP Added 11-26-06
			QhtfFpTES = QhtfFreezeProt;	//  HP Added 12-12-06
      
			// **********************************************************
			// ******        plant is not already operating         *****
			// **********************************************************
      
			if (PBMode==0)         //  if plant is not already operating nor starting up
			{
//           If  ((((Qsf>0).AND.(ETs0.GE.m_TSLogic(p,1)).AND.((Qsf+ETs0).GE.m_TSLogic(p,3))).OR. &
//                ((Qsf==0).AND.(ETs0.GE.m_TSLogic(p,2)).AND.(ETs0.GE.m_TSLogic(p,3))).OR. &
//                 (Qsf>PTSmax))) Then
         //mjw 1.13.2011
          //If  ((((Qsf.GT.0).AND.(ETs0.GE.m_TSLogic(p,1)).AND.((Qsf+ETs0).GE.QTTMin)).OR. &
          //     ((Qsf.EQ.0).AND.(ETs0.GE.m_TSLogic(p,2)).AND.(ETs0.GE.QTTMin)).OR. &
          //      (Qsf.GT.PTSmax))) Then
          
          //mjw 5.8.12 Calculate the startup energy requirement 
				QTurSu = TurSUE * Qdesign * TimeSteps;
          //mjw 5.8.12 Account for the startup energy
          //Start the plant if:
          // -> Solar resource is greater than zero AND total available energy exceeds startup + minimum operation requirement
          // -> Solar resource is not available AND energy in storage exceeds startup + minimum operation requirement
          // -> Solar resource exceeds the maximum charge rate and the plant would have to dump energy
          // Note: this revision gets rid of the requirement that storage energy be above the minimum dispatch fraction in order to start up.
          if  ( ( (Qsf>0)&& ( ( Qsf+ m_Ets0- m_TSLogic.at(p,0) ) >= (QTTMIN+QTurSu) ) ) ||
			  ( (Qsf == 0) && ( ( m_Ets0 - m_TSLogic.at(p,1) ) >= (QTTMIN+QTurSu) ) ) ||
               (Qsf > PTSMax) )
		  {

                //'  Starts plant if any condition is met
                            
				// Assumes Operator started plant during previous time period
				// But TRNSYS cannot do this, so start-up energy is deducted during current timestep.     
				     
					PBMode = 1;								//  HP Added 11-26-06
					QTurSu = TurSUE * Qdesign * TimeSteps;	//  HP Added 11-26-06
			    
// resolve this
					//Qtpb = m_TSLogic(p,3) ! set the energy to powerblock equal to the load for this TOU period
				//mjw 5.8.12 Don't automatically set to the dispatch level.. limit by what's available!
					if (Qsf>0.) 
						Qtpb = MIN(m_TSLogic.at(p,2), Qsf+m_Ets0-m_TSLogic.at(p,0)-QTurSu);
					else
						Qtpb = MIN(m_TSLogic.at(p,2), Qsf+m_Ets0-m_TSLogic.at(p,1)-QTurSu);


					if (Qsf>Qtpb)   //  if solar field output is greater than what the necessary load ?
					{
						Qtts = Qsf - Qtpb; //  the extra goes to thermal storage
               
						if (Qtts>PTSMax)  //  if q to thermal storage exceeds thermal storage max rate Added 9-10-02
						{
							Qdump = Qtts - PTSMax;  //  then dump the excess for this period Added 9-10-02
							Qtts = PTSMax;
						}				   
// 				   Qfts = 0 //  the energy from thermal storage is 0
						Qfts = QTurSu;  //  HP 12-07-06
					}
					else //  q from solar field not greater than needed by the powerblock
					{
						Qtts = 0;
	// 				    Qfts = (1 -  Qsf /  Qtpb) * PFSmax //  Added 9-10-02 energy from thermal storage cannot exceed max rate out
	//  HP 12-07-06	    Qfts = Qfts + (1 -  Qsf /  Qtpb) * PFSmax //  HP Added 11-26-06
						Qfts = QTurSu + (1 -  Qsf /  Qtpb) * PFSMAX; //  HP Added 11-26-06
			// 		    Qtpb = Qfts + Qsf                //  q to PB sum of thermal storage and solar field Added 9-10-02
						if (Qfts>PFSMAX) Qfts = PFSMAX; // ' Added 1-26-08 ***********
						Qtpb = Qsf + (1 -  Qsf /  Qtpb) * PFSMAX; //  HP Added 11-26-06






					}            
//  HP 12-07-06	Ets = ETs0 + (Qsf - Qtpb) / TimeSteps //  thermal storage energy is initial + what was left 
					Ets = m_Ets0 - QTurSu + (Qsf - Qtpb) / TimeSteps; //  HP Added 12-07-06
					PBMode = 2;   //  powerblock is now running
					PbStartF = 1; //  the powerblock turns on during this timeperiod.
				}            
				else // Store energy not enough stored to start plant
				{
					Qtts = Qsf; //  everything goes to thermal storage
					Qfts = 0;   //  nothing from thermal storage
					Ets = m_Ets0 + Qtts / TimeSteps;  
					Qtpb = 0;
					QTurSu = 0; 
				}      
			// **********************************************************
			// ******        plant is already operating             *****
			// **********************************************************
			}
			else        //  Power block operated last period or was starting up
			{
				// MJW 7/09 Determine the current fractional thermal storage dispatch control
/*				if (Qsf>0)
					TStemp = TCS_MATRIX_INDEX( m_TSLogicin, p,0 );
				else
					TStemp = TCS_MATRIX_INDEX( m_TSLogicin,p,1);
				*/
				if (Qsf>0)
					TStemp = TCS_MATRIX_INDEX(m_TSLogicin, p, 1);
				else
					TStemp = TCS_MATRIX_INDEX(m_TSLogicin, p, 2);

				if ( (Qsf + max(0.,m_Ets0-m_ESMAX*TStemp) * TimeSteps) > m_TSLogic.at(p,2) ) 
				{
				//  If there is sufficient energy to operate at dispatch target output
					Qtpb = m_TSLogic.at(p,2);
            
					if (Qsf>Qtpb)
					{
						Qtts = Qsf - Qtpb; // extra from what is needed put in thermal storage
               
				//  Added 9-10-02
						if (Qtts>PTSMax)   // check if max power rate to storage exceeded            ' Added 9-10-02
						{
							Qdump = Qtts - PTSMax; //  if so, dump extra         ' Added 9-10-02
							Qtts = PTSMax;                  // Added 9-10-02
						}               
						Qfts = 0;
					}
					else //  solar field outptu less than what powerblock needs
					{
						Qtts = 0;
						Qfts = (1 - Qsf / Qtpb) * PFSMAX; // Added 9-10-02
						if (Qfts>PFSMAX) Qfts = PFSMAX; //  Added 1-26-08
						Qtpb = Qfts + Qsf;                //  Added 9-10-02
					}				
					Ets = m_Ets0 + (Qsf - Qtpb - Qdump) / TimeSteps; //  energy of thermal storage is the extra
            
				//  Check to see if throwing away energy HP010701
					if ((Ets>m_ESMAX) && (Qtpb<QTTMAX))  //  QTTMAX (MWt) - power to turbine max
					{
						if ( ( (Ets - m_ESMAX) * TimeSteps)< (QTTMAX - Qtpb) ) 
						{
							Qtpb = Qtpb + (Ets - m_ESMAX) * TimeSteps;
							Ets = m_ESMAX;
						}
						else
						{
							Ets = Ets - (QTTMAX - Qtpb) / TimeSteps;  //  should this be Ets0 instead of Ets on RHS ??
							Qtpb = QTTMAX;
						}
						Qtts = Qsf - Qtpb;
					}
				}            
				else  // Empties TS to dispatch level if above min load level
				{
					if ( (Qsf + max(0.,m_Ets0-m_ESMAX*TStemp) / TimeSteps) > QTTMIN)   // Modified 7/2009 by MJW
					{
						Qfts = max(0.,m_Ets0-m_ESMAX*TStemp)/TimeSteps;
						Qtpb = Qsf + Qfts;
						Qtts = 0;
						Ets = m_Ets0 - Qfts;
					}
					else
					{
						Qtpb = 0;
						Qfts = 0;
						Qtts = Qsf;
						Ets = m_Ets0 + Qtts / TimeSteps;
					}
				}    
			}

			if (Qtpb>0) 
				PBMode = 2;
			else
				PBMode = 0;

			Ets = Ets - (QTsHl + QhtfFpTES)/ TimeSteps; //  should this be Ets or ETS0 on the RHS ?
      
			if (Ets>m_ESMAX)  //  trying to put in more than storage can handle
			{
				QTsFull = (Ets - m_ESMAX) * TimeSteps;  // this is the amount dumped when storage is completely full
				Ets = m_ESMAX;
				Qtts = Qtts - QTsFull;
			}
			else
			{
				QTsFull = 0; //  nothing is dumped if not overfilled
			}      
			//  Check min and max on turbine
			if (Qtpb<QTTMIN) 
			{
				Qmin = Qtpb;
				Qtpb = 0;
				PBMode = 0;
			}
			else
			{
				Qmin = 0;
			}      
      
			HhtfLoad = Qfts / Qdesign;
			m_PBMode0 = PBMode;
	// DONE AFTER CONVERGENCE		  ETs0 = .Ets
	  }       
	  else		   //  No Storage  NOT SURE WHY THIS IS HERE. IT SHOULD CRASH if the user enters a number other than 0 and 1
	  {
			Qtts = 0;
			Qfts = 0;
			Ets = 0;
			Qtpb = Qsf;
	  } //  end case on dispatch method





		//  Thermal Storage Parasitics (Hot Pump) TAKEN FROM PARASITICS SECTION OF EXCELERGY CODE
      
		//       If HBypassF = 0 Then          '  Hot pump bypass - when 1 the hot pump is bypassed when the solar field is in operation.

		// EparHhtf = HhtfPar * HhtfParPF * (HhtfParF0 + HhtfParF1 * HhtfLoad + HhtfParF2 * (HhtfLoad**2))
		EparHhtf = HhtfPar  * (HhtfParF0 + HhtfParF1 * HhtfLoad + HhtfParF2 * pow(HhtfLoad,2) );		// HP Changed 12-12-06 (SAM input accounts for PF)

		if (EparHhtf<0.0) 			// HP Added 12-11-06
			EparHhtf=0;

		// HhtfPar	1.1000
		// HhtfParPF	1.000
		// HhtfParF0	-0.036
		// HhtfParF1	0.242
		// HhtfParF2	0.794
		//       Else                              '  Hot HTF Pumps (HTF from TS to PB)
		//        .EparHhtf = 0
		//       End If

// shj - does not make sense - TRNSYS output is negative
		if (Ets < 0)
		{
		// 	out(2) = 0  NB Changed on 6-11-09 because it should just be setting ets to 0 instead of negative
			//Ets = 0;
			m_Ets0 = 0;
		}
		else
		{
			m_Ets0 = Ets;
		}
		
		//  set outputs
		value(O_Qtts, Qtts);          // | Energy to Thermal Storage                                      |      MW        |     MW
		value(O_Qfts, Qfts);          // | Energy from Thermal Storage                                    |      MW        |     MW
		value(O_Ets, Ets);           // | Energy in Thermal Storage
		value(O_QTsHl, QTsHl);         // | Energy losses from Thermal Storage
		value(O_Qtpb, Qtpb);          // | Energy to the Power Block
		value(O_QTsFull, QTsFull);       // | Energy dumped because the thermal storage is full
		value(O_Qmin, Qmin);          // | Indicator of being below minimum operation level
		value(O_Qdump, Qdump);         // | The amount of energy dumped (more than turbine and storage)
		value(O_QTurSu, QTurSu);        // | The energy needed to startup the turbine
		value(O_PbStartF, PbStartF);      // | is 1 during the period when powerblock starts up otherwise 0
		value(O_HhtfLoad, HhtfLoad);      // | Hot HTF pump load (energy from storage)                         | MW?
		value(O_EparHhtf, EparHhtf);      // | Hot HTF pump parasitics

		value(O_PBMode, PBMode);
		value(O_QhtfFpTES, QhtfFpTES);      //  MWht   
		value(O_QhtfFpHtr, QhtfFpHtr);       //  MWht  // 

		// testing
		value(O_tslogic00, m_TSLogic.at(0,0));
		value(O_tslogic01, m_TSLogic.at(0,1));
		value(O_tslogic02, m_TSLogic.at(0,2));
		value(O_tslogic10, m_TSLogic.at(1,0));
		value(O_tslogic11, m_TSLogic.at(1,1));
		value(O_tslogic12, m_TSLogic.at(1,2));
		value(O_tslogic20, m_TSLogic.at(2,0));
		value(O_tslogic21, m_TSLogic.at(2,1));
		value(O_tslogic22, m_TSLogic.at(2,2));
		value(O_tslogic30, m_TSLogic.at(3,0));
		value(O_tslogic31, m_TSLogic.at(3,1));
		value(O_tslogic32, m_TSLogic.at(3,2));
		value(O_tslogic40, m_TSLogic.at(4,0));
		value(O_tslogic41, m_TSLogic.at(4,1));
		value(O_tslogic42, m_TSLogic.at(4,2));
		value(O_tslogic50, m_TSLogic.at(5,0));
		value(O_tslogic51, m_TSLogic.at(5,1));
		value(O_tslogic52, m_TSLogic.at(5,2));
		value(O_tslogic60, m_TSLogic.at(6,0));
		value(O_tslogic61, m_TSLogic.at(6,1));
		value(O_tslogic62, m_TSLogic.at(6,2));
		value(O_tslogic70, m_TSLogic.at(7,0));
		value(O_tslogic71, m_TSLogic.at(7,1));
		value(O_tslogic72, m_TSLogic.at(7,2));
		value(O_tslogic80, m_TSLogic.at(8,0));
		value(O_tslogic81, m_TSLogic.at(8,1));
		value(O_tslogic82, m_TSLogic.at(8,2));

		// store variables for next iteration - TRNSYS post convergence call
		//m_Ets0 = Ets;
		m_PBMode0 = PBMode;
		//m_TurSuE0 = TurSUE;//? TurSue0 in TRNSYS

		return 0;
	}
};


TCS_IMPLEMENT_TYPE( sam_trough_storage_type806, "SAM Trough Storage", "Steven Janzou", 1, sam_trough_storage_type806_variables, NULL, 0 )

