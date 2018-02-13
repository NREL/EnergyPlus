// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without the U.S. Department of Energy's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

// C++ Headers
#include <HybridEvapCoolingModel.hh>

#include <UtilityRoutines.hh>

#include <string>
#include <list>
#include <cmath>
//#include <windows.h>
#include <ScheduleManager.hh>
#include <General.hh>
#include <CurveManager.hh>
#include <DataGlobals.hh>
#include <DataGlobalConstants.hh>
#include <DataHVACGlobals.hh>
#include <Psychrometrics.hh>
#include <DataEnvironment.hh>
#include <DataZoneEquipment.hh>
// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>


namespace EnergyPlus {//***************

	namespace HybridEvapCoolingModel {
		// Module containing the EvaporativeCoolers simulation routines

		// MODULE INFORMATION:
		//       AUTHOR         Spencer Dutton
		//       DATE WRITTEN   May 2017
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS MODULE:
		// To encapsulate the data and algorithms required for the HybridUnitaryHVAC system. 

		// METHODOLOGY EMPLOYED:
		// uses 6D lookup tables to provide performance data that describe 8 key performance metrics .

		//Supply Air Temperature, Supply Air Humidity Ratio, Electric Power, Supply Fan Electric Power, External Static Pressure
		//System Second Fuel Consumption, System Third Fuel Consumption, System Water Use

		//Lookups include


		// REFERENCES: none

		// OTHER NOTES: none

		// USE STATEMENTS:
		// Use statements for data only modules
		// Using/Aliasing
		using DataHVACGlobals::SysTimeElapsed;
		using DataHVACGlobals::NumOfSysTimeSteps;
		using DataHVACGlobals::NumOfSysTimeStepsLastZoneTimeStep;
		using DataHVACGlobals::LimitNumSysSteps;
		using DataHVACGlobals::TimeStepSys;
		using DataGlobals::SecInHour;
		using namespace EnergyPlus::DataEnvironment;
		using namespace Psychrometrics;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using CurveManager::GetCurveMinMaxValues;
		using CurveManager::CurveValue;
		using EnergyPlus::CurveManager::GetNormalPoint;
		using ScheduleManager::GetCurrentScheduleValue;
		typedef int* MyType;

		#define  DEF_Tdb 0
		#define  DEF_RH 1

		#define  TEMP_CURVE 0 
		#define  W_CURVE 1
		#define  POWER_CURVE 2
		#define  SUPPLY_FAN_POWER 3
		#define  EXTERNAL_STATIC_PRESSURE 4
		#define  SECOND_FUEL_USE 5
		#define  THIRD_FUEL_USE 6
		#define  WATER_USE 7

		CMode::CMode() : 
			ModeID(0.0),
			Max_Msa(0.0),
			Min_Msa(0.0),
			Min_OAF(0.0),
			Max_OAF(0.0),
			Minimum_Outside_Air_Temperature(0.0),
			Maximum_Outside_Air_Temperature(0.0),
			Minimum_Outside_Air_Humidity_Ratio(0.0),
			Maximum_Outside_Air_Humidity_Ratio(0.0),
			NormalizationReference(0.0),
			Correction(0.0) 
		{
			MODE_BLOCK_OFFSET_Alpha = 9;
			BLOCK_HEADER_OFFSET_Alpha = 19;
			MODE1_BLOCK_OFFSET_Number = 2;
			MODE_BLOCK_OFFSET_Number = 16;
			BLOCK_HEADER_OFFSET_Number = 6;
		}

		bool CMode::InitializeOutsideAirTemperatureConstraints(Real64 min, Real64 max)
		{
			//note If this field is blank, there should be no lower constraint on outside air temperature
			Minimum_Outside_Air_Temperature = min;
			Maximum_Outside_Air_Temperature = max;
			return true;
		}
		bool CMode::InitializeOutsideAirHumidityRatioConstraints(Real64 min, Real64 max)
		{
			//minimum 0.00 maximum 0.10, units kgWater / kgDryAir
			//note Mode0 will not be considerd when outside air absolute humidity is below the value in this field.
			//note If this field is blank, the lower constraint on outside air humidity ratio will be 0.00 kgWater / kgDryAir., default 0.00
			//the upper constraint on outside air humidity ratio will be 0.10 kgWater / kgDryAir, default 0.10
			Minimum_Outside_Air_Humidity_Ratio = min;
			Maximum_Outside_Air_Humidity_Ratio = max;
			return true;
		}
		bool CMode::InitializeOutsideAirRelativeHumidityConstraints(Real64 min, Real64 max)
		{
			//minimum 0.00,maximum 100.00, units percent, Mode0 will not be considered when the outside air relative humidity is below the value in this field.
			// note If this field is blank, the lower constraint on outside air relative humidity will be 0.00% (default 0.00), the upper constraint on outside air relative humidity will be 100.00%, (default 100.00)
			Minimum_Outside_Air_Relative_Humidity = min;
			Maximum_Outside_Air_Relative_Humidity = max;
			return true;
		}
		bool CMode::InitializeReturnAirTemperatureConstraints(Real64 min, Real64 max)
		{
			//will not be considered when the return air temperature is below the value in this field.
			//If this field is blank, there will be no lower constraint on return air temperature
			Minimum_Return_Air_Temperature = min;
			Maximum_Return_Air_Temperature = max;
			return true;
		}
		bool CMode::InitializeReturnAirHumidityRatioConstraints(Real64 min, Real64 max)
		{
			//minimum 0.00 maximum 0.10, units kgWater / kgDryAir
			//note Mode0 will not be considerd when outside air absolute humidity is below the value in this field.
			//note If this field is blank, the lower constraint on outside air humidity ratio will be 0.00 kgWater / kgDryAir., default 0.00
			//the upper constraint on outside air humidity ratio will be 0.10 kgWater / kgDryAir, default 0.10
			Minimum_Return_Air_Humidity_Ratio = min;
			Maximum_Return_Air_Humidity_Ratio = max;
			return true;
		}
		bool CMode::InitializeReturnAirRelativeHumidityConstraints(Real64 min, Real64 max)
		{
			//minimum 0.00,maximum 100.00, units percent, Mode0 will not be considered when the outside air relative humidity is below the value in this field.
			// note If this field is blank, the lower constraint on outside air relative humidity will be 0.00% (default 0.00), the upper constraint on outside air relative humidity will be 100.00%, (default 100.00)
			Minimum_Return_Air_Relative_Humidity = min;
			Maximum_Return_Air_Relative_Humidity = max;
			return true;
		}


		bool CMode::InitializeOSAFConstraints(Real64 minOSAF, Real64 maxOSAF)
		{
			//minimum 0.00, maximum 1.00, Outside air fractions below this value will not be considered.
			//If this field is blank, the lower constraint on outside air fraction will be 0.00,default 0.10
			Min_OAF = minOSAF;
			Max_OAF = maxOSAF;
			return true;
		}
		bool CMode::InitializeMsaRatioConstraints(Real64 minMsa, Real64 maxMsa)
		{
			//minimum 0.00, maximum 1.00, Supply air mass flow rate ratios below this value will not be considered.
			//Supply air mass flow rate ratio describes supply air mass flow rate as a fraction of mass flow rate associated with the value in field : "System Maximum Supply Air Flow Rate".
			//If this field is blank, the lower constraint on outside air fraction will be 0.00,default 0.10
			Min_Msa = minMsa;
			Max_Msa = maxMsa;
			return true;
		}
		bool CMode::ValidPointer(int curve_pointer) {
			if (curve_pointer >= 0) return true;
			else  return false;
		}
		Real64 CMode::CalculateCurveVal( Real64 X_1, Real64 X_2, Real64 X_3, Real64 X_4, Real64 X_5, Real64 X_6, int curveType)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Returns the normalized or scaled output from the performance curve as specified by curveType.  6 independent variables are required. 

			// METHODOLOGY EMPLOYED:
			// Makes a call to the curve manager, then multiplies the result by either the NormalizationReference for intensive variables or by the scaling Correction
			// for extensive variables.
			// The Normalization reference is specified as a model input, the Correction is equal to the ScaledSystemMaximumSupplyAirMassFlowRate, which is calculated 
			// as  Correction=ScaledSystemMaximumSupplyAirMassFlowRate= SystemMaximumSupplyAirFlowRate*ScalingFactor*AirDensity, where SystemMaximumSupplyAirFlowRate, and ScalingFactor are 
			// model inputs.
			// The following tables are for intensive variables : Supply Air Temperature, Supply Air Humidity, External Static Pressure,
			// The following tables are for extensive variables : Electric Power, Fan Electric Power, Second Fuel Consumption, Third Fuel Consumption, Water Use Lookup Table
			// 
			// X_1 is the outside air temperature (Tosa), X_2 is the outside humidity ratio (Wosa), 
			// X_3 return air temp (Tra),X_4 return humidity ratio Wra,
			// X_5 supply air mass flow rate, Msa, X_6 outside air fraction OSAF
			// the curveType 

			// REFERENCES:
			// na

			// Using/Aliasing
			Real64 Y_val = 0;

			switch (curveType)
			{
			case TEMP_CURVE:
				if (ValidPointer(Tsa_curve_pointer))
				{
					Y_val = NormalizationReference*CurveValue(Tsa_curve_pointer, X_1, X_2, X_3, X_4, X_5, X_6);
				}
				else {
					Y_val = X_3;//return air temp
				}
				break;
			case W_CURVE:

				if (ValidPointer(HRsa_curve_pointer))
				{
					Y_val = NormalizationReference*CurveValue(HRsa_curve_pointer, X_1, X_2, X_3, X_4, X_5, X_6);
				}
				else {
					Y_val = X_4;//return HR
				}
				break;
			case POWER_CURVE:

				if (ValidPointer(Psa_curve_pointer))
				{ // Correction= scaling factor *System Maximum Supply Air Flow Rate*StdRhoAir
					Y_val = Correction*CurveValue(Psa_curve_pointer, X_1, X_2, X_3, X_4, X_5, X_6);
				}
				else {
					Y_val = 0;
				}
				break;
			case SUPPLY_FAN_POWER:
				 
				if (ValidPointer(SFPsa_curve_pointer))
				{ // Correction= scaling factor *System Maximum Supply Air Flow Rate*StdRhoAir
					Y_val = Correction*CurveValue(SFPsa_curve_pointer, X_1, X_2, X_3, X_4, X_5, X_6);
				}
				else {
					Y_val = 0;
				}
				break;
			case EXTERNAL_STATIC_PRESSURE:

				if (ValidPointer(ESPsa_curve_pointer))
				{ // Correction= scaling factor *System Maximum Supply Air Flow Rate*StdRhoAir
					Y_val = NormalizationReference*CurveValue(ESPsa_curve_pointer, X_1, X_2, X_3, X_4, X_5, X_6);
				}
				else {
					Y_val = 0;//or set a more reasonable default
				}
				break;
			case SECOND_FUEL_USE:
				
				if (ValidPointer(SFUsa_curve_pointer))
				{ // Correction= scaling factor *System Maximum Supply Air Flow Rate*StdRhoAir
					Y_val = Correction*CurveValue(SFUsa_curve_pointer, X_1, X_2, X_3, X_4, X_5, X_6);
				}
				else {
					Y_val = 0;//or set a more reasonable default
				}
				break;
			case THIRD_FUEL_USE:

				if (ValidPointer(TFUsa_curve_pointer))
				{ // Correction= scaling factor *System Maximum Supply Air Flow Rate*StdRhoAir
					Y_val = Correction*CurveValue(TFUsa_curve_pointer, X_1, X_2, X_3, X_4, X_5, X_6);
				}
				else {
					Y_val = 0;//or set a more reasonable default
				}
				break;
			case WATER_USE:

				if (ValidPointer(WUsa_curve_pointer))
				{ // Correction= scaling factor *System Maximum Supply Air Flow Rate*StdRhoAir
					Y_val = Correction*CurveValue(WUsa_curve_pointer, X_1, X_2, X_3, X_4, X_5, X_6);
				}
				else {
					Y_val = 0;//or set a more reasonable default
				}
				break;

			default:
				break;
			}

			return Y_val;
		}
		void CMode::InitializeCurve(int curveType, int curve_ID)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Sets the curve ID assigned by the curve manager for the specific lookup Table, to a member variable. 

			// METHODOLOGY EMPLOYED:
			// 

			// REFERENCES:
			// na

			// Using/Aliasing
			switch (curveType)
			{
			case TEMP_CURVE:
				Tsa_curve_pointer = curve_ID;
				break;
			case W_CURVE:
				HRsa_curve_pointer = curve_ID;
				break;
			case POWER_CURVE:
				Psa_curve_pointer = curve_ID;
				break;
			case SUPPLY_FAN_POWER:
				SFPsa_curve_pointer = curve_ID;
				break;
			case EXTERNAL_STATIC_PRESSURE:
				ESPsa_curve_pointer = curve_ID;
				break;
			case SECOND_FUEL_USE:
				SFUsa_curve_pointer = curve_ID;
				break;
			case THIRD_FUEL_USE:
				TFUsa_curve_pointer = curve_ID;
				break;
			case WATER_USE:
				WUsa_curve_pointer = curve_ID;
				break;
			default:
				break;
			}
		}

		bool CMode::GenerateSolutionSpace(Real64 ResolutionMsa, Real64 ResolutionOSA)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Generates a matrix of all possible combinations of OSAF and supply air mass flow rates. 

			// METHODOLOGY EMPLOYED:
			// Calculate the range of supply air mass flow rate (SAMF) and OSAF the mode can operate under.
			// Calculate a step size to increment through the ranges of SAMF and OSAF, based on the ResolutionMsa and 
			// ResolutionOSAs respectively (these are hard coded in the constructor, this is an important consideration 
			// because the number of increments greatly effects the simulation run time).
			// 
			// if the range is smaller than the minimum resolution,  then use the minimum resolution size as the minimum range over which to iterate
			// this should have the effect of keeping the number of solutions constant even if the range size varies.


			// REFERENCES:
			// na

			// Using/Aliasing
			Real64 deltaMsa = Max_Msa - Min_Msa;
			Real64 deltaOAF = Max_OAF - Min_OAF;
			if (deltaMsa < ResolutionMsa)
			{
				deltaMsa = ResolutionMsa;
			}
			if (deltaOAF < ResolutionOSA)
			{
				deltaOAF = ResolutionOSA;
			}
			Real64 Msastep_size = (deltaMsa * ResolutionMsa);
			Real64 OAFsteps_size = (deltaOAF * ResolutionOSA);

			for (Real64 Msa_val = Max_Msa; Msa_val >= Min_Msa; Msa_val = Msa_val - Msastep_size)
			{
				for (Real64 OAF_val = Max_OAF; OAF_val >= Min_OAF; OAF_val = OAF_val - OAFsteps_size)
				{
					sol.AddItem(Msa_val, OAF_val, 0);
				}

			}
			return true;
		}
		bool CMode::ValidateArrays(Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields, std::string cCurrentModuleObject)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Check to make sure that there is not a misalignment between the number of modes spcified in the idf, and the number of mode objects generated.

			// METHODOLOGY EMPLOYED:
			// Each mode has a fixed number of Numbers and Alphas inputs to process. So the total number of inputs for all the operating modes 
			// should be less than total number of parameters. Also the ModeID increment should never exceed the number the maximum number of modes
			// specified in the idf (OpperatingModes)
			// REFERENCES:
			// na

			// Using/Aliasing
			int alphas_len = Alphas.size();
			int numbers_len = Numbers.size();
			int OpperatingModes = Numbers(4);
			int parmsnumber = alphas_len + numbers_len;
			int MinimumExpectedLength = OpperatingModes*(MODE_BLOCK_OFFSET_Number + MODE_BLOCK_OFFSET_Alpha) + BLOCK_HEADER_OFFSET_Alpha + BLOCK_HEADER_OFFSET_Number;
			if (MinimumExpectedLength > parmsnumber)
			{
				return false;
			}
			if (OpperatingModes < ModeID)
			{
				return false;
			}
			return true;
		}

		bool Model::ParseMode(Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields, Array1D<bool>  lAlphaBlanks, std::string cCurrentModuleObject)
		{
			CMode newMode;
			bool error = newMode.ParseMode(ModeCounter, &OperatingModes, ScaledSystemMaximumSupplyAirMassFlowRate, Alphas, cAlphaFields, Numbers, cNumericFields, lAlphaBlanks, cCurrentModuleObject);
			ModeCounter++;
			return error;
		}

		bool CMode::ParseMode(int ModeCounter, std::vector<CMode>* OperatingModes, Real64 scaledCorrection, Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields, Array1D<bool>  lAlphaBlanks, std::string cCurrentModuleObject)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Does the processing of each of the seperate modes

			// METHODOLOGY EMPLOYED:
			// As the number of modes defined in the idf is not known until its read in, this method uses two counters to keep track of the inputs in the 
			// Alphas and Numbers arrays. Three constants are used, BLOCK_HEADER_OFFSET_Alpha, BLOCK_HEADER_OFFSET_Number and MODE1_BLOCK_OFFSET_Number
			// if ever additional input parameters are added to the idf these offset counters would have to be adjusted accordingly.

			// REFERENCES:
			// na

			// Using/Aliasing
			ModeID = ModeCounter;
			Correction = scaledCorrection;
			NormalizationReference = GetNormalPoint(1);
			if (NormalizationReference == -1)
			{
				NormalizationReference = 1;
			}
			
			if (!ValidateArrays(Alphas, cAlphaFields, Numbers, cNumericFields, cCurrentModuleObject))
			{
				ShowSevereError("There was a misalignment between the number of modes spcified in the idf and the number of mode objects generated, in" + cCurrentModuleObject);
				return false;
			}
			int inter_Number; 
			bool ErrorsFound = false;
			int inter_Alpha = BLOCK_HEADER_OFFSET_Alpha + MODE_BLOCK_OFFSET_Alpha * ModeID;
			if (ModeID == 1)
			{
				inter_Number = BLOCK_HEADER_OFFSET_Number + MODE1_BLOCK_OFFSET_Number* ModeID;
			}
			else
			{
				inter_Number = BLOCK_HEADER_OFFSET_Number + MODE1_BLOCK_OFFSET_Number + MODE_BLOCK_OFFSET_Number* (ModeID - 1);
			}
			std::ostringstream strs;
			strs << ModeID;

			int curveID = -1;
			if (lAlphaBlanks(inter_Alpha - 1)) {
				ModeName = "Mode" + strs.str();
			}
			else
			{
				ModeName = Alphas(inter_Alpha - 1);
			}

			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(TEMP_CURVE, curveID);//as this is invalid curve id CalculateCurveVal will return a default when called
			}
			else
			{
				curveID = GetCurveIndex(Alphas(inter_Alpha));
				if (curveID == 0) {
					ShowSevereError("Invalid " + cAlphaFields(inter_Alpha) + '=' + Alphas(inter_Alpha));
					ShowContinueError("Entered in " + cCurrentModuleObject);
					ErrorsFound = true;
					InitializeCurve(TEMP_CURVE, -1);
				}
				else { InitializeCurve(TEMP_CURVE, curveID); }

			}

			inter_Alpha = inter_Alpha + 1;

			//A20, \field Mode0 Supply Air Humidity Ratio Lookup Table Name
			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(W_CURVE, curveID);//as this is invalid curve id CalculateCurveVal will return a default  when called
			}
			else
			{
				curveID = GetCurveIndex(Alphas(inter_Alpha));
				if (curveID == 0) {
					ShowSevereError("Invalid " + cAlphaFields(inter_Alpha) + '=' + Alphas(inter_Alpha));
					ShowContinueError("Entered in " + cCurrentModuleObject);
					ErrorsFound = true;
					InitializeCurve(W_CURVE, -1);
				}
				else { InitializeCurve(W_CURVE, curveID); }

			}
			inter_Alpha = inter_Alpha + 1;
			//A21, \field Mode0 System Electric Power Lookup Table Name
			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(POWER_CURVE, curveID);//as this is invalid curve id CalculateCurveVal will return a default 
			}
			else
			{
				curveID = GetCurveIndex(Alphas(inter_Alpha));
				if (curveID == 0) {
					ShowSevereError("Invalid " + cAlphaFields(inter_Alpha) + '=' + Alphas(inter_Alpha));
					ShowContinueError("Entered in " + cCurrentModuleObject);
					ErrorsFound = true;
					InitializeCurve(POWER_CURVE, -1);
				}
				else { InitializeCurve(POWER_CURVE, curveID); }

			}
			//A22, \field Mode0 Supply Fan Electric Power Lookup Table Name
			inter_Alpha = inter_Alpha + 1;
			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(SUPPLY_FAN_POWER, curveID);//as this is invalid curve id CalculateCurveVal will return a default 
			}
			else
			{
				curveID = GetCurveIndex(Alphas(inter_Alpha));
				if (curveID == 0) {
					ShowSevereError("Invalid " + cAlphaFields(inter_Alpha) + '=' + Alphas(inter_Alpha));
					ShowContinueError("Entered in " + cCurrentModuleObject);
					ErrorsFound = true;
					InitializeCurve(SUPPLY_FAN_POWER, -1);
				}
				else { InitializeCurve(SUPPLY_FAN_POWER, curveID); }

			}
			//A23, \field Mode0 External Static Pressure Lookup Table Name
			inter_Alpha = inter_Alpha + 1;
			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(EXTERNAL_STATIC_PRESSURE, curveID);//as this is invalid curve id CalculateCurveVal will return a default 
			}
			else
			{
				curveID = GetCurveIndex(Alphas(inter_Alpha));
				if (curveID == 0) {
					ShowSevereError("Invalid " + cAlphaFields(inter_Alpha) + '=' + Alphas(inter_Alpha));
					ShowContinueError("Entered in " + cCurrentModuleObject);
					ErrorsFound = true;
					InitializeCurve(EXTERNAL_STATIC_PRESSURE, -1);
				}
				else { InitializeCurve(EXTERNAL_STATIC_PRESSURE, curveID); }

			}
			//  
			//A24, \field Mode0 System Second Fuel Consumption Lookup Table Nam
			inter_Alpha = inter_Alpha + 1;
			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(SECOND_FUEL_USE, curveID);//as this is invalid curve id CalculateCurveVal will return a default 
			}
			else
			{
				curveID = GetCurveIndex(Alphas(inter_Alpha));
				if (curveID == 0) {
					ShowSevereError("Invalid " + cAlphaFields(inter_Alpha) + '=' + Alphas(inter_Alpha));
					ShowContinueError("Entered in " + cCurrentModuleObject);
					ErrorsFound = true;
					InitializeCurve(SECOND_FUEL_USE, -1);
				}
				else { InitializeCurve(SECOND_FUEL_USE, curveID); }

			}
			//A25, \field Mode0 System Third Fuel Consumption Lookup Table Name
			inter_Alpha = inter_Alpha + 1;
			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(THIRD_FUEL_USE, curveID);//as this is invalid curve id CalculateCurveVal will return a default 
			}
			else
			{
				curveID = GetCurveIndex(Alphas(inter_Alpha));
				if (curveID == 0) {
					ShowSevereError("Invalid " + cAlphaFields(inter_Alpha) + '=' + Alphas(inter_Alpha));
					ShowContinueError("Entered in " + cCurrentModuleObject);
					ErrorsFound = true;
					InitializeCurve(THIRD_FUEL_USE, -1);
				}
				else { InitializeCurve(THIRD_FUEL_USE, curveID); }

			}
			//A26, \field Mode0 System Water Use Lookup Table Name
			inter_Alpha = inter_Alpha + 1;
			curveID = -1;
			if (lAlphaBlanks(inter_Alpha)) {
				InitializeCurve(WATER_USE, curveID);//as this is invalid curve id CalculateCurveVal will return a default 
			}
			else
			{
				curveID = GetCurveIndex(Alphas(inter_Alpha));
				if (curveID == 0) {
					ShowSevereError("Invalid " + cAlphaFields(inter_Alpha) + '=' + Alphas(inter_Alpha));
					ShowContinueError("Entered in " + cCurrentModuleObject);
					ErrorsFound = true;
					InitializeCurve(WATER_USE, -1);
				}
				else { InitializeCurve(WATER_USE, curveID); }

			}
			if (ModeID == 0)
			{
				(*OperatingModes).push_back(*this);
				return ErrorsFound;
			}
			//N6, \field Mode0  Minimum Outside Air Temperature
			//N7, \field Mode0  Maximum Outside Air Temperature
			bool ok = InitializeOutsideAirTemperatureConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N8, \field Mode0  Minimum Outside Air Humidity Ratio
			//N9, \field Mode0  Maximum Outside Air Humidity Ratio
			ok = InitializeOutsideAirHumidityRatioConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N10, \field Mode0 Minimum Outside Air Relative Humidity
			//N11, \field Mode0 Maximum Outside Air Relative Humidity
			ok = InitializeOutsideAirRelativeHumidityConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N12, \field Mode0 Minimum Return Air Temperature
			//N13, \field Mode0 Maximum Return Air Temperature
			ok = InitializeReturnAirTemperatureConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N14, \field Mode0 Minimum Return Air Humidity Ratio 
			//N15, \field Mode0 Maximum Return Air Humidity Ratio
			ok = InitializeReturnAirHumidityRatioConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Invalid " + cNumericFields(inter_Number) + "Or Invalid" + cNumericFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N16, \field Mode0 Minimum Return Air Relative HumidityInitialize
			//N17, \field Mode0 Maximum Return Air Relative Humidity
			ok = InitializeReturnAirRelativeHumidityConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Invalid " + cAlphaFields(inter_Number) + '=' + Alphas(inter_Number) + "Or Invalid" + cAlphaFields(inter_Number + 1) + '=' + Alphas(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			inter_Number = inter_Number + 2;
			//N18, \field Mode0 Minimum Outside Air Fraction
			//N19, \field Mode0 Maximum Outside Air Fraction

			ok = InitializeOSAFConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Error in OSAFConstraints" + cAlphaFields(inter_Number) + "through" + cAlphaFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			//N20, \field Mode0 Minimum Supply Air Mass Flow Rate Ratio
			//N21, \field Mode0 Maximum Supply Air Mass Flow Rate Ratio
			inter_Number = inter_Number + 2;
			ok = InitializeMsaRatioConstraints(Numbers(inter_Number), Numbers(inter_Number + 1));
			if (!ok) {
				ShowSevereError("Error in OSAFConstraints" + cAlphaFields(inter_Number) + "through" + cAlphaFields(inter_Number + 1));
				ShowContinueError("Entered in " + cCurrentModuleObject);
				ErrorsFound = true;
			}
			(*OperatingModes).push_back(*this);
			return ErrorsFound;
		}
		


		bool CMode::MeetsOAEnvConstraints(Real64 Tosa, Real64 Wosa, Real64 RHosa)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// To check to see if this mode of operation is able to operate given the specified outdoor environmental conditions.

			// METHODOLOGY EMPLOYED:
			// Constraining certain modes to only operate over certain environmental conditions gives the user greater control in which
			// modes the algorithm selects.

			// REFERENCES:
			// na

			// Using/Aliasing
			bool OATempConstraintmet = false;
			bool OAHRConstraintmet = false;
			bool OARHConstraintmet = false;

			if (Tosa >= Minimum_Outside_Air_Temperature && Tosa <= Maximum_Outside_Air_Temperature)
			{
				OATempConstraintmet = true;
			}

			if (Wosa >= Minimum_Outside_Air_Humidity_Ratio && Wosa <= Maximum_Outside_Air_Humidity_Ratio)
			{
				OAHRConstraintmet = true;
			}

			if (RHosa >= Minimum_Outside_Air_Relative_Humidity  && RHosa <= Maximum_Outside_Air_Relative_Humidity)
			{
				OARHConstraintmet = true;
			}
			if (OATempConstraintmet && OAHRConstraintmet && OARHConstraintmet)
			{
				return true;
			}
			else
			{
				return false;
			}
		}

		bool Model::MeetsSupplyAirTOC(Real64 Tsupplyair)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// To check to see if this this particular setting (combination of mode, OSAF and Msa) meets the required minumum
			// supply air temperature specified in the schedules

			// METHODOLOGY EMPLOYED:
			// Checks the minimum and maximum supply air temperatures and tests to see if the proposed supply air temperature is in the acceptable range.

			// REFERENCES:
			// na

			// Using/Aliasing		
			Real64 MinSAT = 10;
			Real64 MaxSAT = 20;
			if (TsaMin_schedule_pointer > 0) {
				MinSAT = GetCurrentScheduleValue(TsaMin_schedule_pointer);
			}
			if (TsaMax_schedule_pointer > 0) {
				MaxSAT = GetCurrentScheduleValue(TsaMax_schedule_pointer);
			}
			if (Tsupplyair < MinSAT || Tsupplyair > MaxSAT)
				return false;
			return true;
		}

		bool Model::MeetsSupplyAirRHOC(Real64 SupplyW)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// To check to see if this this particular setting (combination of mode, OSAF and Msa) meets the required minumum
			// supply air relative humidity specified in the schedules

			// METHODOLOGY EMPLOYED:
			// Checks the scheduled minimum and maximum supply air RH and tests to see if the proposed supply air RH is in the acceptable range.


			// REFERENCES:
			// na

			// Using/Aliasing
			Real64 MinRH = 0;
			Real64 MaxRH = 1;
			if (RHsaMin_schedule_pointer > 0) {
				MinRH = GetCurrentScheduleValue(RHsaMin_schedule_pointer);
			}
			if (RHsaMax_schedule_pointer > 0) {
				MaxRH = GetCurrentScheduleValue(RHsaMax_schedule_pointer);
			}
			if (SupplyW < MinRH || SupplyW > MaxRH)
				return false;
			return true;
		}

		Model::Model() :
			Initialized(false),
			ZoneNum(0),
			SchedPtr(0),
			SystemMaximumSupplyAirFlowRate(0.0),
			ScalingFactor(0.0),
			ScaledSystemMaximumSupplyAirMassFlowRate(0.0),
			UnitOn(0),
			UnitTotalCoolingRate(0.0),
			UnitTotalCoolingEnergy(0.0),
			UnitSensibleCoolingRate(0.0),
			UnitSensibleCoolingEnergy(0.0),
			UnitLatentCoolingRate(0.0),
			UnitLatentCoolingEnergy(0.0),
			SystemTotalCoolingRate(0.0),
			SystemTotalCoolingEnergy(0.0),
			SystemSensibleCoolingRate(0.0),
			SystemSensibleCoolingEnergy(0.0),
			SystemLatentCoolingRate(0.0),
			SystemLatentCoolingEnergy(0.0),
			UnitTotalHeatingRate(0.0),
			UnitTotalHeatingEnergy(0.0),
			UnitSensibleHeatingRate(0.0),
			UnitSensibleHeatingEnergy(0.0),
			UnitLatentHeatingRate(0.0),
			UnitLatentHeatingEnergy(0.0),
			SystemTotalHeatingRate(0.0),
			SystemTotalHeatingEnergy(0.0),
			SystemSensibleHeatingRate(0.0),
			SystemSensibleHeatingEnergy(0.0),
			SystemLatentHeatingRate(0.0),
			SystemLatentHeatingEnergy(0.0),
			RequestedLoadToHeatingSetpoint(0.0),
			RequestedLoadToCoolingSetpoint(0.0),
			SupplyFanElectricPower(0.0),
			SupplyFanElectricEnergy(0.0),
			SecondaryFuelConsumptionRate(0.0),
			SecondaryFuelConsumption(0.0),
			ThirdFuelConsumptionRate(0.0),
			ThirdFuelConsumption(0.0),
			WaterConsumptionRate(0.0),
			WaterConsumption(0.0),
			ExternalStaticPressure(0.0),
			RequestedHumdificationMass(0.0),
			RequestedHumdificationLoad(0.0),
			RequestedHumdificationEnergy(0.0),
			RequestedDeHumdificationMass(0.0),
			RequestedDeHumdificationLoad(0.0),
			RequestedDeHumdificationEnergy(0.0),
			QLatentZoneOut( 0),
			QSensZoneOut(0),
			TsaMin_schedule_pointer(0),
			TsaMax_schedule_pointer(0),
			RHsaMin_schedule_pointer(0),
			RHsaMax_schedule_pointer(0),
			PrimaryMode(0),
			PrimaryModeRuntimeFraction(0.0),
			averageOSAF(0),
			ErrorCode(0),
			InletNode(0),
			OutletNode(0),
			SecondaryInletNode(0),
			SecondaryOutletNode(0),
			FinalElectricalPower(0.0),
			FinalElectricalEnergy(0.0),
			InletMassFlowRate(0.0),
			InletTemp(0.0),
			InletWetBulbTemp(0.0),
			InletHumRat(0.0),
			InletEnthalpy(0.0),
			InletPressure(0.0),
			InletRH(0.0),
			OutletMassFlowRate(0.0),
			OutletVolumetricFlowRate(0.0),
			OutletTemp(0.0),
			OutletWetBulbTemp(0.0),
			OutletHumRat(0.0),
			OutletEnthalpy(0.0),
			OutletPressure(0.0),
			OutletRH(0.0),
			SecInletMassFlowRate(0.0),
			SecInletTemp(0.0),
			SecInletWetBulbTemp(0.0),
			SecInletHumRat(0.0),
			SecInletEnthalpy(0.0),
			SecInletPressure(0.0),
			SecInletRH(0.0),
			SecOutletMassFlowRate(0.0),
			SecOutletTemp(0.0),
			SecOutletWetBulbTemp(0.0),
			SecOutletHumRat(0.0),
			SecOutletEnthalpy(0.0),
			SecOutletPressure(0.0),
			SecOutletRH(0.0),
			Wsa(0.0),
			SupplyVentilationAir(0.0),
			SupplyVentilationVolume(0.0),
			OutdoorAir(false),
			MinOA_Msa(0.0),
			OARequirementsPtr(0),
			Tsa(0.0),
			ModeCounter(0),
			CoolingRequested(false),
			HeatingRequested(false),
			VentilationRequested(false),
			DehumidificationRequested(false),
			HumidificationRequested(false)
		{
			WarnOnceFlag = false;
			count_EnvironmentConditionsMetOnce = 0;
			count_EnvironmentConditionsNotMet = 0;
			count_SAHR_OC_MetOnce = 0;
			count_SAT_OC_MetOnce = 0;
			count_DidWeMeetLoad = 0;
			count_DidWeNotMeetLoad = 0;
			// vector below used store the modes in each timestep that don't meet humidity or temperature limits, used in warnings
			std::vector<int> temp(25);
			SAT_OC_MetinMode_v= temp;
			SAHR_OC_MetinMode_v= temp;
			
			int MAXIMUM_OPERATIONAL_SETTINGS = 5;
			ModeCounter = 0;
			//!!!!!!!!!!!!!!!!!!!!!!!!! this debug code will be removed nearer the code freeze, please don't comment on it, it will be gone.
			DebugBreak = 12; // remove
	
			CurrentOperatingSettings.resize(5);

			InitializeModelParams();
		}
		
		void Model::ResetOutputs()
		{
			UnitTotalCoolingRate = 0;
			UnitTotalCoolingEnergy = 0;
			UnitSensibleCoolingRate = 0;
			UnitSensibleCoolingEnergy = 0;
			UnitLatentCoolingRate = 0;
			UnitLatentCoolingEnergy = 0;
			SystemTotalCoolingRate = 0;
			SystemTotalCoolingEnergy = 0;
			SystemSensibleCoolingRate = 0;
			SystemSensibleCoolingEnergy = 0;
			SystemLatentCoolingRate = 0;
			SystemLatentCoolingEnergy = 0;
			UnitTotalHeatingRate = 0;
			UnitTotalHeatingEnergy = 0;
			UnitSensibleHeatingRate = 0;
			UnitSensibleHeatingEnergy = 0;
			UnitLatentHeatingRate = 0;
			UnitLatentHeatingEnergy = 0;
			SystemTotalHeatingRate = 0;
			SystemTotalHeatingEnergy = 0;
			SystemSensibleHeatingRate = 0;
			SystemSensibleHeatingEnergy = 0;
			SystemLatentHeatingRate = 0;
			SystemLatentHeatingEnergy = 0;
			SupplyFanElectricPower = 0;	   
			SupplyFanElectricEnergy = 0;    
			SecondaryFuelConsumptionRate = 0;
			SecondaryFuelConsumption = 0;   
			ThirdFuelConsumptionRate = 0;   
			ThirdFuelConsumption = 0;	   
			WaterConsumptionRate = 0;	   
			WaterConsumption = 0;		   
			ExternalStaticPressure = 0;	 
			
		}

		void Model::InitializeModelParams()
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Reset calculation values 

			// METHODOLOGY EMPLOYED:
			// 

			// REFERENCES:
			// na

			// Using/Aliasing
			ResetOutputs();
			PrimaryMode = 0;
			PrimaryModeRuntimeFraction = 0;
			optimal_EnvCondMet = false;
			Tsa = 0;
			//reset the power use to a high value, this is replaced during the calculation keeping the "best" setting. 
		
			RunningPeakCapacity_EnvCondMet = false;
			Settings.clear();
		}



		void Model::Initialize(int ZoneNumber)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Specify solution space resolution, and populate the solution spaces in each mode

			// METHODOLOGY EMPLOYED:
			// Solution spaces are the matrices of possible settings settings (combination of OSA fraction and supply air mass flow rate) 
			// This method calls the GenerateSolutionSpace for each of the modes defined in the idf. 
			// REFERENCES:
			// na

			// Using/Aliasing

			ZoneNum = ZoneNumber;
			if (Initialized){
				return;
			}
			Initialized = true;
			// this variable, at this point hard coded, sets the fidelity of the matrix of possible OSAF and Msa combinations. 
			ResolutionMsa = 0.2; //msa/msaRATED
			ResolutionOSA = 0.2; //OSAF as absolute fraction (not %)
		
			//Iterate through modes of operation generating a matrix of OSAF and Msa to test in the algorithm.
			for (auto & thisOperatingMode : OperatingModes) {
				thisOperatingMode.GenerateSolutionSpace(ResolutionMsa, ResolutionOSA);
			}

			Initialized = true;
		}

		/*
		Real64 Model::Part_press(Real64 P, Real64 W)
		{
			// Function to compute partial vapor pressure in [kPa]
			// From page 6.9 equation 38 in ASHRAE Fundamentals handbook (2005)
			//   P = ambient pressure [kPa]
			//   W = humidity ratio [kg/kg dry air]

			return (P * W / (0.62198 + W));
		}
		
		Real64 Model::Sat_press(Real64 Tdb)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Function to compute saturation vapor pressure in [kPa]

			// METHODOLOGY EMPLOYED:
			//ASHRAE Fundamentals handbood (2005) p 6.2, equation 5 and 6
			//   Tdb = Dry bulb temperature [degC]
			// Valid from -100C to 200 C

			// REFERENCES:
			// na

			Real64  C1 = -5674.5359;
			Real64  C2 = 6.3925247;
			Real64  C3 = -0.009677843;
			Real64  C4 = 0.00000062215701;
			Real64  C5 = 2.0747825E-09;
			Real64  C6 = -9.484024E-13;
			Real64  C7 = 4.1635019;
			Real64  C8 = -5800.2206;
			Real64  C9 = 1.3914993;
			Real64  C10 = -0.048640239;
			Real64  C11 = 0.000041764768;
			Real64  C12 = -0.000000014452093;
			Real64  C13 = 6.5459673;
			Real64  Sat_press_val = 0;

			Real64   TK = Tdb + 273.15;         //Converts from degC to degK

			if (TK <= 273.15)
			{
				Sat_press_val = exp(C1 / TK + C2 + C3 * TK + C4 * pow(TK, 2) + C5 * pow(TK, 3) + C6 * pow(TK, 4) + C7 * log(TK)) / 1000;
			}
			else
			{
				Sat_press_val = exp(C8 / TK + C9 + C10 * TK + C11 * pow(TK, 2) + C12 * pow(TK, 3) + C13 * log(TK)) / 1000;
			}
			return Sat_press_val;

		}*/
		/*		Real64 Model::CalcHum_ratio_W(Real64 Tdb, Real64 RH, Real64 P)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Function to calculate humidity ratio [kg H2O/kg air]
			// Given dry bulb and wet bulb temperature inputs [degC]

			// METHODOLOGY EMPLOYED:
			// ASHRAE Fundamentals handbood (2005)
			// Tdb = Dry bulb temperature [degC]
			// RH = Relative Humidity [Fraction or %]
			// P = Ambient Pressure [kPa] 

			// REFERENCES:
			// na

			// Using/Aliasing

			Real64 Pws = Sat_press(Tdb);
			Real64 Hum_rat = 0.62198 * RH * Pws / (P - RH * Pws);   // Equation 22, 24, p6.8
			return Hum_rat;
		}*/



		Real64 Model::CheckVal_W(Real64 W, Real64 T, Real64 P)
		{
			//P must be in pascals NOT kPa
			Real64 OutletRHtest = PsyRhFnTdbWPb(T, W, P); //could also use outlet pressure instead of fixed
			Real64 OutletW = PsyWFnTdbRhPb(T, OutletRHtest, P, "Humidity ratio exceeded realistic range error called in "+ Name + ", check performance curve");
			return OutletW;
		}
		Real64 Model::CheckVal_T(Real64 T)
		{
			if ((T > 100) || (T < 0))
			{
				ShowWarningError("Supply air temperature exceeded realistic range error called in " + Name + ", check performance curve");
			}
			return T;
		}
		bool Model::SetStandByMode(CMode Mode0, Real64 Tosa, Real64 Wosa, Real64 Tra, Real64 Wra)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Set the supply air mass flow rate, power use, and all the other parameters for a setting. 

			// METHODOLOGY EMPLOYED:
			// Uses the relevant lookup take to specify the parameters, or uses default conditions.
			// In setting the supply air temperature for now just use return air future improvement will use look up table


			// REFERENCES:
			// na

			// Using/Aliasing
			// using the pointer to the first of the operating modes (which is always the standby mode by definition.
			//Get a handel to the map of possible solutions (valid combinations of OSA fraction and Msa)
			CModeSolutionSpace solutionspace = Mode0.sol;
			//assess the sixe of the solution space to make sure it holds at least 1 combination
			int solution_map_sizeX = solutionspace.PointX.size();
			//Check the normalization reference is set if not set a default
			Real64 NormalizationReference = GetNormalPoint(1);// assumes the model has at least 1 curve and that all are set the same, this is explained in the IO reference
			if (NormalizationReference == -1)
			{
				NormalizationReference = 1;
				return true;
			}
			// if the map of the solution space looks valid then populate the class member oStandBy (CSetting) with the settings data (what OSAF it runs at, 
			// and how much power it uses etc.
			if (solution_map_sizeX > 0)
			{
				Real64 MsaRatio = solutionspace.PointX[0];
				Real64 UnscaledMsa = NormalizationReference * MsaRatio;
				Real64 OSAF = solutionspace.PointY[0];
				Real64 ElectricalPower = Mode0.CalculateCurveVal(Tosa, Wosa, Tra, Wra, UnscaledMsa, OSAF, POWER_CURVE);
				oStandBy.ElectricalPower = ElectricalPower;
				oStandBy.Unscaled_Supply_Air_Mass_Flow_Rate = UnscaledMsa;
				oStandBy.ScaledSupply_Air_Mass_Flow_Rate = MsaRatio * ScaledSystemMaximumSupplyAirMassFlowRate;
				oStandBy.ScaledSupply_Air_Ventilation_Volume = MsaRatio * ScaledSystemMaximumSupplyAirMassFlowRate / StdRhoAir;
				oStandBy.Supply_Air_Mass_Flow_Rate_Ratio = MsaRatio;
				oStandBy.Outside_Air_Fraction = OSAF;
				oStandBy.SupplyAirTemperature = Tra;
				oStandBy.SupplyAirW = Wra;
				oStandBy.Mode = 0;
				oStandBy.Mixed_Air_Temperature = Tra;
				oStandBy.Mixed_Air_W = Wra;
			}
			else
			{
				//if the solution space is invalid return true that an error occured.
				return true;
			}

			return false;
		}


		Real64 Model::CalculateTimeStepAverage(SYSTEMOUTPUTS val)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Calculates the resultant supply air conditions when the system operates in 
			// multiple settings within a timestep. 

			// METHODOLOGY EMPLOYED:
			// For longer simulation timesteps this model can consider partial runtime fractions
			// operating in different settings for a fraction of the total simulation time step reducing the likelyhood of over conditioning.
			// Intensive variables that do not depend on system size (like temperature, pressure,etc), and extensive variables (variable whose values depend on the quantity of substance) are handled differently
			//  
			// Extensive variables ( Mass Flow, Volume flow, Fuel use etc), are averaged weighted by the amount of time spent in each setting.
			// for example if then system operates for 25% of the time with a mass flow of 4kg/s, and 75% of the time at a mass flow of 0kg/s then the resultant 
			// time step average mass flow rate would be  1 kg/s.

			// Intensive values in each part runtime fraction are first multiplied by the Scaled Supply Air Mass Flow Rate for each setting
			// and then once all the various runtime fractions are added up, the resultant is divided by the overal time step average Scaled Supply Air Mass Flow Rate
			// 
			// REFERENCES:
			// na

			// Using/Aliasing
			Real64 averagedVal = 0;
			Real64 MassFlowDependentDenominator = 0;
			Real64 value = 0;

			for (auto & thisOperatingSettings : CurrentOperatingSettings) {
				switch (val)
				{
				case SYSTEMOUTPUTS::VENTILATION_AIR_V:
					value = thisOperatingSettings.ScaledSupply_Air_Ventilation_Volume;
					break;
				case SYSTEMOUTPUTS::SYSTEM_FUEL_USE:
					value = thisOperatingSettings.ElectricalPower;
					break;
				case SYSTEMOUTPUTS::OSUPPLY_FAN_POWER:
					value = thisOperatingSettings.SupplyFanElectricPower;
					break;
				case SYSTEMOUTPUTS::OSECOND_FUEL_USE:
					value = thisOperatingSettings.SecondaryFuelConsumptionRate;
					break;
				case SYSTEMOUTPUTS::OTHIRD_FUEL_USE:
					value = thisOperatingSettings.ThirdFuelConsumptionRate;
					break;
				case SYSTEMOUTPUTS::OEXTERNAL_STATIC_PRESSURE:
					value = thisOperatingSettings.ExternalStaticPressure* thisOperatingSettings.ScaledSupply_Air_Mass_Flow_Rate;
					break;
				case SYSTEMOUTPUTS::OWATER_USE:
					value = thisOperatingSettings.WaterConsumptionRate;
					break;
				case SYSTEMOUTPUTS::SUPPLY_AIR_TEMP:
					value = thisOperatingSettings.SupplyAirTemperature * thisOperatingSettings.ScaledSupply_Air_Mass_Flow_Rate;
					break;
				case SYSTEMOUTPUTS::MIXED_AIR_TEMP:
					value = thisOperatingSettings.Mixed_Air_Temperature * thisOperatingSettings.ScaledSupply_Air_Mass_Flow_Rate;
					break;
				case SYSTEMOUTPUTS::SUPPLY_MASS_FLOW:
					value = thisOperatingSettings.ScaledSupply_Air_Mass_Flow_Rate;
					break;
				case SYSTEMOUTPUTS::SUPPLY_AIR_HR:
					value = thisOperatingSettings.SupplyAirW * thisOperatingSettings.ScaledSupply_Air_Mass_Flow_Rate;
					break;
				case SYSTEMOUTPUTS::MIXED_AIR_HR:
					value = thisOperatingSettings.Mixed_Air_W * thisOperatingSettings.ScaledSupply_Air_Mass_Flow_Rate;
					break;

					}
				Real64 part_run = thisOperatingSettings.Runtime_Fraction;
				averagedVal = averagedVal + value * part_run;
				MassFlowDependentDenominator = thisOperatingSettings.ScaledSupply_Air_Mass_Flow_Rate*part_run + MassFlowDependentDenominator;
			}

			CSetting StandbyMode = (*(CurrentOperatingSettings.begin()));
			switch (val)
			{
				case SYSTEMOUTPUTS::SUPPLY_AIR_TEMP:  
					if (MassFlowDependentDenominator==0)
					{
						averagedVal = StandbyMode.SupplyAirTemperature;
					}
					else averagedVal = averagedVal/ MassFlowDependentDenominator; break;
				case SYSTEMOUTPUTS::OEXTERNAL_STATIC_PRESSURE:
					if (MassFlowDependentDenominator == 0)
					{
						averagedVal = StandbyMode.ExternalStaticPressure;
					}
					else averagedVal = averagedVal / MassFlowDependentDenominator; break;
				case SYSTEMOUTPUTS::SUPPLY_AIR_HR:  
					if (MassFlowDependentDenominator == 0)
					{
						averagedVal = StandbyMode.SupplyAirW;
					}
					else averagedVal = averagedVal / MassFlowDependentDenominator; break;
				case SYSTEMOUTPUTS::MIXED_AIR_TEMP:  
					if (MassFlowDependentDenominator == 0)
					{
						averagedVal = StandbyMode.Mixed_Air_Temperature;
					}
					else averagedVal = averagedVal / MassFlowDependentDenominator; break;
				case SYSTEMOUTPUTS::MIXED_AIR_HR: 
					if (MassFlowDependentDenominator == 0)
					{
						averagedVal = StandbyMode.Mixed_Air_W;
					}
					else averagedVal = averagedVal / MassFlowDependentDenominator; break;
				default:
					break;
			}
			return averagedVal;
		}

		Real64 Model::CalculatePartRuntimeFraction(Real64 MinOA_Msa, Real64 Mvent, Real64 RequestedCoolingLoad, Real64 RequestedHeatingLoad, Real64 SensibleRoomORZone, Real64 RequestedDehumidificationLoad, Real64 RequestedMoistureLoad, Real64 LatentRoomORZone)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Calculates the minimum runtime fraction in a given setting needed to meet the
			// sensible cooling, sensible heating, dehumidification and humidification loads
			// and ventilation loads.

			// METHODOLOGY EMPLOYED:
			// Calculate the minimum runtime fractions for each load that needs to be met and find the lowest of those runtime fractions. 
			// Go through each of the requirements (ventilation, heating, cooling, dehumidifcation, humidification and work out what the minimum runtime fraction you would need in order to meet all these rewuirements.
			// Importantly the SensibleRoomORZone is either (-) for heating or (+) for cooling, where as the RequestedCoolingLoad and RequestedHeatingLoad, are both possitive (never below 0).

			// REFERENCES:
			// na

			// Using/Aliasing
			Real64 PLHumidRatio, PLDehumidRatio, PLVentRatio, PLSensibleCoolingRatio, PLSensibleHeatingRatio, PartRuntimeFraction;
			PLHumidRatio=PLDehumidRatio = PLVentRatio = PLSensibleCoolingRatio = PLSensibleHeatingRatio = 0;

			if (Mvent > 0) {
				PLVentRatio = MinOA_Msa / Mvent;
			}
			PartRuntimeFraction = PLVentRatio; 
			
			if (SensibleRoomORZone > 0) {
				PLSensibleCoolingRatio = abs(RequestedCoolingLoad) / abs(SensibleRoomORZone);
			}
			if (PLSensibleCoolingRatio > PartRuntimeFraction) {
				PartRuntimeFraction = PLSensibleCoolingRatio;
			}
			
			if (SensibleRoomORZone < 0) {
				PLSensibleHeatingRatio = abs(RequestedHeatingLoad) / abs(SensibleRoomORZone);
			}

			if (PLSensibleHeatingRatio > PartRuntimeFraction) {
				PartRuntimeFraction = PLSensibleHeatingRatio;
			}

			if (RequestedDehumidificationLoad > 0) { 
				PLDehumidRatio = abs(RequestedDehumidificationLoad) / abs(LatentRoomORZone); 
			}
			
			if (PLDehumidRatio > PartRuntimeFraction) {
				PartRuntimeFraction = PLDehumidRatio;
			}

			if (RequestedMoistureLoad > 0) {
				PLHumidRatio = abs(RequestedMoistureLoad) / abs(LatentRoomORZone);
			}
			if (PLHumidRatio > PartRuntimeFraction) {
				PartRuntimeFraction = PLHumidRatio;
			}

			if (PartRuntimeFraction < 0)
			{
				PartRuntimeFraction = 0;
			}
			if (PartRuntimeFraction > 1)
			{
				PartRuntimeFraction = 1;
			}

			return PartRuntimeFraction;
		}

		int Model::SetOperatingSetting(CStepInputs StepIns)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Dutton
			//       DATE WRITTEN   May 2017
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// This subroutine determines the set of operating settings for the HybridUniaryHVAC 
			// It is called from Model::doStep, the main calculation step
			// at the system time step.

			// METHODOLOGY EMPLOYED:
			// 1) Clear out the set of operating settings from the previous time step.
			// 2) Iterate through each operating mode and weed out modes that are not intended to operate in current environmental conditions.    
			//		-> For each mode that is viable iterate thought the solution space and identify settings that meet the ventilation requirements 
			//      -> settings that do are stored in the a container (Settings)
			// 3) Iterate through all the settings in Settings
			//
			// 4) Calculate the setting zone sensible cooling and heating load and humidifcation and dehumidifcation. 
			// 5) Test to see if conditioning and humidification loads are met. 
			// 6) Calculate setting power consumption, use the setting delivered ventilation and loads to calculate the
			// 7) minimum runtime fraction needed to meet those loads, then assuming that part runtime fraction calculate the setting part run time power use.
			// 8)   If the setting meets both the conditioning and humidification loads then test to see if its optimal in terms of energy use.
			//  		->if so, save that setting as the current optimal.
			//  		->if not ignore it.
			//  	If the setting failed ot meet either the conditioning or humidification loads, then
			//  	-> firstly check to see if no previous other setting (in this calculation step) has met both the load and humidification requirements
			//  			-> if so
			//  				-> check if this setting meets the conditioning load (only) 
			//  					-> if so
			//  						->check to see if this setting is better at meeting the dehumidification or humidification lad than any previous setting this step.
			//  						-> if its not, ignore it.
			// 		  				-> if not 
			// 		  					->check to see if any previous setting met the conditioning load 
			// 		  						->if not:
			// 		  							->see if this setting is better at meeting the conditioning load than any previous setting this calculation step.
			// 		  								-> if so save as current optimal
			// 		  								-> if its not, ignore it.
			// 		  						-> if so: then ignore it.
			//  			->if not, then a previous setting is better than this one by default, and so ignore it. 
			// 9) Identify error states if the no setting meets the environmental conditions, or the supply air humidity or temperature constraints. 
			// 10) if we met the load set operating settings to be a combination of the optimal setting at the minium required runtime fraction 
			// 11) if we partly met the load then do the best we can and run full out in that optimal setting.
			// 12) if we didn't even partially meet the load make sure the operational settings are just the standby mode.
			// 13) generate summary statistics for warnings.

			// REFERENCES:
			// na

			// Using/Aliasing
			using General::RoundSigDigits;
			// Locals
			// SUBROUTINE ARGUMENT DEFINITIONS:
			// The CStepInputs are defined in the CStepInputs class definition.
			// SUBROUTINE PARAMETER DEFINITIONS:
			// na
			// INTERFACE BLOCK SPECIFICATIONS
			// na
			// DERIVED TYPE DEFINITIONS
			// na

			// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
			bool DidWeMeetLoad = false;
			bool DidWeMeetHumidificaiton = false;
			bool DidWePartlyMeetLoad = false;
			int modenumber = 0;
			int point_number = 0;
			Real64 MsaRatio = 0;
			Real64 OSAF = 0;
			//Real64 Msa = 0;
			Real64 UnscaledMsa = 0;
			Real64 ScaledMsa = 0;
			Real64 Mvent = 0;
			Real64 OptimalSetting_RunFractionTotalFuel = IMPLAUSIBLE_POWER;
			Real64 EIR;
			Real64 ElectricalPower;
			Real64 Tma;
			Real64 Wma;
			Real64 Hsa;
			Real64 Hma;
			Real64 TotalSystem;
			Real64 PreviousMaxiumConditioningOutput = 0;
			Real64 PreviousMaxiumHumidOrDehumidOutput = 0;
			std::string ObjectID = Name.c_str();
			int size = CurrentOperatingSettings.size();
			CSetting empty_setting;
			for (int i = 0; i < size; i++) 
			{
				CurrentOperatingSettings[i] = empty_setting;
			}
			if (StepIns.RHosa > 1) 
			{ 
				ShowSevereError("Unitary hybrid system error, required relative humidity value 0-1, called in object" + ObjectID + ".Check inputs" ); 
				assert(true);
				return -1;
			} //because it should be fractional, this should only really be possible if its called from a unit test
			
			if (StepIns.RHra > 1) 
			{ 
				ShowSevereError("Unitary hybrid system error,  required relative humidity value 0-1, called in object" + ObjectID + ".Check inputs");
				assert(true);
				return -1;
			} //because it should be fractional, this should only really be possible if its called from a unit test

			Real64 Wosa = PsyWFnTdbRhPb(StepIns.Tosa, StepIns.RHosa, 101325);
			Real64 Wra = PsyWFnTdbRhPb(StepIns.Tra, StepIns.RHra, 101325);
			bool EnvironmentConditionsMet, EnvironmentConditionsMetOnce, MinVRMet, MinVRMetOnce, SAT_OC_Met, SAT_OC_MetOnce, SARH_OC_Met, SAHR_OC_MetOnce;
			EnvironmentConditionsMetOnce = SAT_OC_Met = SAT_OC_MetOnce = SARH_OC_Met = SAHR_OC_MetOnce = false;

			MinOA_Msa = StepIns.MinimumOA; // Set object version of minimum VR Kg/s
		
			std::vector<CMode>::const_iterator iterator;
			iterator = OperatingModes.begin();
			// skip the first one becuase that is standby
			++iterator;
			for (; iterator != OperatingModes.end(); ++iterator) // iterate though the modes.
			{
				CMode Mode = *iterator;
				CModeSolutionSpace solutionspace = Mode.sol;
				bool SAHR_OC_MetinMode = false;
				bool SAT_OC_MetinMode = false;
				int solution_map_sizeX = solutionspace.PointX.size() - 1;
				int solution_map_sizeY = solutionspace.PointY.size() - 1;
				
				if (solution_map_sizeX != solution_map_sizeY)
				{
					ShowWarningError("Error in solution space mapping, suggest adjusting operating constraints.");
					return -2;
				}
				// Check that in this mode the //Outside Air Relative Humidity(0 - 100 % )	//Outside Air Humidity Ratio(g / g)//Outside Air Temperature(°C)
				if (Mode.MeetsOAEnvConstraints(StepIns.Tosa, Wosa, 100 * StepIns.RHosa)) 
				{
					EnvironmentConditionsMet = EnvironmentConditionsMetOnce = true;
				}
				else
				{
					EnvironmentConditionsMet = false;
				}

				if (EnvironmentConditionsMet)
				{
					Real64 NormalizationReference = GetNormalPoint(1);// assumes the model has at least 1 curve and that all are set the same.
					if (NormalizationReference == -1)
					{
						NormalizationReference = 1;
						return true;
					}
					for (point_number = 0; point_number != solution_map_sizeX; point_number++) // within each mode go though all the combinations of solution spaces.
					{
						//Supply Air Mass Flow Rate(kg / s)
						//Outside Air Fraction(0 - 1)
						
						MsaRatio = solutionspace.PointX[point_number];// fractions of rated mass flow rate, so for some modes this might be low but others hi
						OSAF = solutionspace.PointY[point_number];
						UnscaledMsa = NormalizationReference * MsaRatio;
						ScaledMsa = ScaledSystemMaximumSupplyAirMassFlowRate * MsaRatio;
						Real64 Supply_Air_Ventilation_Volume = 0;
						//Calculate the ventilation mass flow rate
						Mvent = ScaledMsa * OSAF;

						if (StdRhoAir > 1)
						{
							Supply_Air_Ventilation_Volume = Mvent / StdRhoAir;
						}
						else
						{
							Supply_Air_Ventilation_Volume = Mvent / 1.225;  //stored as volumetric flow for reporting
						}

						if (Mvent > MinOA_Msa)
						{
							MinVRMet = MinVRMetOnce = true;
						}
						else
						{
							MinVRMet = false;
						}

						if (MinVRMet)
						{
							//all these points meet the minimum VR requirement
							solutionspace.PointMeta[point_number] = 1;
							// Calculate prospective supply air temperature
							Tsa = Mode.CalculateCurveVal( StepIns.Tosa, Wosa, StepIns.Tra, Wra, UnscaledMsa, OSAF, TEMP_CURVE);
							// Check it meets constraints
							if (MeetsSupplyAirTOC(Tsa))
							{
								SAT_OC_Met = SAT_OC_MetOnce = SAT_OC_MetinMode = true;
							}
							else
							{
								SAT_OC_Met = false;
							}
							// Calculate prospective supply air Humidity Ratio
							Wsa = Mode.CalculateCurveVal( StepIns.Tosa, Wosa, StepIns.Tra, Wra, UnscaledMsa, OSAF, W_CURVE);
							//Return Air Relative Humidity(0 - 100 % ) //Return Air Humidity Ratio(g / g)
							if (MeetsSupplyAirRHOC(Wsa))
							{
								SARH_OC_Met = SAHR_OC_MetOnce = SAHR_OC_MetinMode = true;
							}
							else
							{
								SARH_OC_Met = false;
							}

							if (SARH_OC_Met && SAT_OC_Met)
							{
								CSetting CandidateSetting;
								CandidateSetting.Supply_Air_Ventilation_Volume = Supply_Air_Ventilation_Volume;
								CandidateSetting.Mode = Mode.ModeID;
								CandidateSetting.Outside_Air_Fraction = OSAF;
								CandidateSetting.Supply_Air_Mass_Flow_Rate_Ratio = MsaRatio;
								CandidateSetting.Unscaled_Supply_Air_Mass_Flow_Rate = UnscaledMsa;
								CandidateSetting.ScaledSupply_Air_Mass_Flow_Rate = MsaRatio * ScaledSystemMaximumSupplyAirMassFlowRate; // spencer is this the same as Correction if so make them the same.
								CandidateSetting.ScaledSupply_Air_Ventilation_Volume = MsaRatio * ScaledSystemMaximumSupplyAirMassFlowRate / StdRhoAir;
								CandidateSetting.oMode = Mode;
								CandidateSetting.SupplyAirTemperature = Tsa;
								CandidateSetting.SupplyAirW = CheckVal_W(Wsa, Tsa, OutletPressure);
								CandidateSetting.Mode = Mode.ModeID;
								Settings.push_back(CandidateSetting);
							}
						}
					}
				}
				if (!WarmupFlag)
				{
					// Keep an account of the number of times the supply air temperature and humidity constraints were not met for a given mode but only do this when its not warmup. 
					if (!SAT_OC_MetinMode)
					{
						SAT_OC_MetinMode_v[Mode.ModeID] = SAT_OC_MetinMode_v[Mode.ModeID] + 1;	
					}
					if (!SAHR_OC_MetinMode) 
					{
						SAHR_OC_MetinMode_v[Mode.ModeID] = SAHR_OC_MetinMode_v[Mode.ModeID] + 1;
					}
				}
				modenumber++;
			}
	
			for (auto & thisSetting : Settings) {
				//Calculate the delta H 
				OSAF = thisSetting.Outside_Air_Fraction;
				UnscaledMsa = thisSetting.Unscaled_Supply_Air_Mass_Flow_Rate;
				Real64 ScaledMsa = thisSetting.ScaledSupply_Air_Mass_Flow_Rate;

				// send the scales Msa to calculate energyies and the unscaled for sending to curves.
				Tsa = thisSetting.SupplyAirTemperature;
				modenumber = thisSetting.Mode;
				Wsa = thisSetting.SupplyAirW;
				Tma = StepIns.Tra + OSAF * (StepIns.Tosa - StepIns.Tra);
				Wma = Wra + OSAF * (Wosa - Wra);
				thisSetting.Mixed_Air_Temperature = Tma;
				thisSetting.Mixed_Air_W = Wma;

				Hma = PsyHFnTdbW(Tma, Wma);
				// Calculate Enthalpy of return air
				Real64 Hra = PsyHFnTdbW(StepIns.Tra, Wra);

				Hsa = 1.006 * Tsa * (2501 + 1.86 * Tsa);
				Hsa = PsyHFnTdbW(Tsa, Wsa);

				Real64 SupplyAirCp = PsyCpAirFnWTdb(Wsa, Tsa); //J/degreesK.kg 
				Real64 ReturnAirCP = PsyCpAirFnWTdb(Wra, StepIns.Tra); //J/degreesK.kg 
				Real64 OutsideAirCP = PsyCpAirFnWTdb(Wosa, StepIns.Tosa); //J/degreesK.kg 

																		  // Calculations below of system cooling and heating capacity are ultimately reassessed when the resultant part runtime fraction is assessed.
																		  // However its valuable that they are calculated here to at least provide a check. 

																		  //System Sensible Cooling{ W } = m'SA {kg/s} * 0.5*(cpRA + OSAF*(cpOSA-cpRA) + cpSA) {kJ/kg-C} * (T_RA + OSAF*(T_OSA - T_RA)  - T_SA) 
																		  //System Latent Cooling{ W } = m'SAdryair {kg/s} * L {kJ/kgWater} * (HR_RA + OSAF *(HR_OSA - HR_RA) - HR_SA) {kgWater/kgDryAir}
																		  //System Total Cooling{ W } = m'SAdryair {kg/s} * (h_RA + OSAF*(h_OSA - h_RA) - h_SA) {kJ/kgDryAir}
				Real64 SystemCp = ReturnAirCP + OSAF * (OutsideAirCP - ReturnAirCP) + SupplyAirCp; //J/degreesK.kg 
				Real64 SensibleSystem = ScaledMsa * 0.5* SystemCp * (Tma - Tsa) / 1000;//kw  dynamic cp
				Real64 MsaDry = ScaledMsa * (1 - Wsa);
				Real64 LambdaSa = Psychrometrics::PsyHfgAirFnWTdb(0, Tsa);
				Real64 LatentSystem = LambdaSa * MsaDry*(Wma - Wsa); //kw
																	 // Total system cooling
				TotalSystem = (Hma - Hsa) * ScaledMsa / 1000;
				// Perform latent check
				Real64 latentCheck = TotalSystem - SensibleSystem;

				//Zone Sensible Cooling{ W } = m'SA {kg/s} * 0.5*(cpRA+cpSA) {kJ/kg-C} * (T_RA - T_SA) {C}
				//Zone Latent Cooling{ W } = m'SAdryair {kg/s} * L {kJ/kgWater} * (HR_RA - HR_SA) {kgWater/kgDryAir}
				//Zone Total Cooling{ W } = m'SAdryair {kg/s} * (h_RA - h_SA) {kJ/kgDryAir}
				Real64 SensibleRoomORZone = ScaledMsa * 0.5*(SupplyAirCp + ReturnAirCP) * (StepIns.Tra - Tsa) / 1000;//kw  dynamic cp
				Real64 latentRoomORZone = LambdaSa * MsaDry*(Wra - Wsa);//kw
																		// Total room cooling
				Real64 TotalRoomORZone = (Hra - Hsa) * ScaledMsa / 1000; //kw
																		 //Perform latent check 
				Real64 latentRoomORZoneCheck = TotalRoomORZone - SensibleRoomORZone;

				thisSetting.TotalSystem = TotalSystem;
				thisSetting.SensibleSystem = SensibleSystem;
				thisSetting.LatentSystem = LatentSystem;
				thisSetting.TotalZone = TotalRoomORZone;
				thisSetting.SensibleZone = SensibleRoomORZone;
				thisSetting.LatentZone = latentRoomORZone;

				bool Conditioning_load_met = false;
				if (CoolingRequested && (SensibleRoomORZone > StepIns.RequestedCoolingLoad))
				{
					Conditioning_load_met = true;
				}
				if (HeatingRequested && (SensibleRoomORZone < StepIns.RequestedHeatingLoad))
				{
					Conditioning_load_met = true;
				}
				if (!(HeatingRequested || CoolingRequested))
				{
					Conditioning_load_met = true;
				}

				bool Humidification_load_met = false;

				Real64 RequestedDeHumdificationLoad_kw = StepIns.ZoneDehumidificationLoad / 1000;
				if (DehumidificationRequested && latentRoomORZone > RequestedDeHumdificationLoad_kw)
				{
					Humidification_load_met = true;
				}
				Real64 RequestedHumdificationLoad_kw = StepIns.ZoneMoistureLoad / 1000;
				if (HumidificationRequested && latentRoomORZone < RequestedHumdificationLoad_kw)
				{
					Humidification_load_met = true;
				}

				if (!(HumidificationRequested || DehumidificationRequested))
				{
					Humidification_load_met = true;
				}

				Real64 Y_val = thisSetting.oMode.CalculateCurveVal(StepIns.Tosa, Wosa, StepIns.Tra, Wra, UnscaledMsa, OSAF, POWER_CURVE); //fix modenumber not set
				ElectricalPower = Y_val;  // [Kw] calculations for fuel in Kw
				thisSetting.ElectricalPower = ElectricalPower;

				thisSetting.SupplyFanElectricPower = thisSetting.oMode.CalculateCurveVal(StepIns.Tosa, Wosa, StepIns.Tra, Wra, UnscaledMsa, OSAF, SUPPLY_FAN_POWER);
				thisSetting.ExternalStaticPressure = thisSetting.oMode.CalculateCurveVal(StepIns.Tosa, Wosa, StepIns.Tra, Wra, UnscaledMsa, OSAF, EXTERNAL_STATIC_PRESSURE);
				thisSetting.SecondaryFuelConsumptionRate = thisSetting.oMode.CalculateCurveVal(StepIns.Tosa, Wosa, StepIns.Tra, Wra, UnscaledMsa, OSAF, SECOND_FUEL_USE);
				thisSetting.ThirdFuelConsumptionRate = thisSetting.oMode.CalculateCurveVal(StepIns.Tosa, Wosa, StepIns.Tra, Wra, UnscaledMsa, OSAF, THIRD_FUEL_USE);
				thisSetting.WaterConsumptionRate = thisSetting.oMode.CalculateCurveVal(StepIns.Tosa, Wosa, StepIns.Tra, Wra, UnscaledMsa, OSAF, WATER_USE);

				//Calculate EIR
				EIR = ElectricalPower / TotalSystem;

				// Calculate partload fraction required to meet all requirements
				Real64 PartRuntimeFraction = 0;
				PartRuntimeFraction = CalculatePartRuntimeFraction(MinOA_Msa, thisSetting.Supply_Air_Ventilation_Volume*StdRhoAir, StepIns.RequestedCoolingLoad, StepIns.RequestedHeatingLoad, SensibleRoomORZone, StepIns.ZoneDehumidificationLoad, StepIns.ZoneMoistureLoad, latentRoomORZone);//

				Real64 RunFractionTotalFuel = ElectricalPower * PartRuntimeFraction; // fraction can be above 1 meaning its not able to do it completely in a time step.
				thisSetting.Runtime_Fraction = PartRuntimeFraction;

				if (Conditioning_load_met && Humidification_load_met)
				{
					//store best performing mode
					if (RunFractionTotalFuel < OptimalSetting_RunFractionTotalFuel)
					{
						OptimalSetting_RunFractionTotalFuel = RunFractionTotalFuel;
						OptimalSetting = thisSetting;
						DidWeMeetLoad = true;
						DidWeMeetHumidificaiton = true;
					}
				}
				else
				{
					if (!DidWeMeetLoad && !DidWeMeetHumidificaiton)
					{
						bool store_best_attempt = false;

						if (Conditioning_load_met)
						{
							DidWeMeetLoad = true;
							if (HumidificationRequested && (latentRoomORZone < PreviousMaxiumHumidOrDehumidOutput))
							{
								store_best_attempt = true;
							}
							if (DehumidificationRequested && (latentRoomORZone > PreviousMaxiumHumidOrDehumidOutput))
							{
								store_best_attempt = true;
							}
							if (store_best_attempt)
							{
								PreviousMaxiumHumidOrDehumidOutput = latentRoomORZone;
							}
						}
						else
						{
							if (!DidWeMeetLoad)
							{
								if (CoolingRequested && (SensibleRoomORZone > PreviousMaxiumConditioningOutput))
								{
									store_best_attempt = true;
								}
								if (HeatingRequested && (SensibleRoomORZone < PreviousMaxiumConditioningOutput))
								{
									store_best_attempt = true;
								}
								if (store_best_attempt)
								{
									PreviousMaxiumConditioningOutput = SensibleRoomORZone;
								}
							}
						}
						if (store_best_attempt)
						{
							OptimalSetting_RunFractionTotalFuel = RunFractionTotalFuel;
							OptimalSetting = thisSetting;
							DidWePartlyMeetLoad = true;
						}
					}
				}
			}

			if (!EnvironmentConditionsMetOnce)
			{
				ErrorCode = 1;
				count_EnvironmentConditionsNotMet++;
			}
			if (!SAHR_OC_MetOnce)
			{
				count_SAHR_OC_MetOnce++;
				ErrorCode = 2;
			}
			if (!SAT_OC_MetOnce)
			{
				count_SAT_OC_MetOnce++;
				ErrorCode = 3;
			}
			// if we met the load set operating settings to be a combination of the optimal setting at the minium required runtime fraction 
			if (DidWeMeetLoad)
			{
				//add first setting to operating modes
				ErrorCode = 0;
				//save the optimal setting in the 
				CurrentOperatingSettings[0] = OptimalSetting;
				PrimaryModeRuntimeFraction=OptimalSetting.Runtime_Fraction;
				oStandBy.Runtime_Fraction = (1 - PrimaryModeRuntimeFraction );
				if (oStandBy.Runtime_Fraction < 0)
				{
					oStandBy.Runtime_Fraction = 0;
				}
				CurrentOperatingSettings[1] = oStandBy;
			}
			else
			{
			// if we partly met the load then do the best we can and run full out in that optimal setting. 
				if (!DidWeMeetLoad && DidWePartlyMeetLoad)
				{
					ErrorCode = 0;
					count_DidWeNotMeetLoad++;
					if (OptimalSetting.ElectricalPower == IMPLAUSIBLE_POWER)
					{
						ShowWarningError("Model was not able to provide cooling for a time step, called in HybridEvapCooling:dostep");
						OptimalSetting.ElectricalPower = 0;
					}
					OptimalSetting.Runtime_Fraction = 1;
					CurrentOperatingSettings[0] = OptimalSetting;
					PrimaryMode = OptimalSetting.Mode;
					PrimaryModeRuntimeFraction = 1;
				}
			//if we didn't even partially meet the load make sure the operational settings are just the standby mode.
				else
				{
					oStandBy.Runtime_Fraction=1;
					CurrentOperatingSettings[0] = oStandBy;
					ErrorCode = -1;
					StandBy = true;
					count_DidWeNotMeetLoad++;
				}
			}
			
			Real64 TimeElapsed = DataGlobals::HourOfDay + DataGlobals::TimeStep * DataGlobals::TimeStepZone + SysTimeElapsed;

			// Use the elapsed time to only give a summary of warnings related to the number of Timesteps environmental conditions, or supply air temperature constraints were not met for a given day.
			// ideally there would be a clear flag that indicates "this is the last timestep of the day, so report", but that doesn't seem to exist. 
			if ((TimeElapsed > 24) && WarnOnceFlag && !WarmupFlag)
			{
				if (count_EnvironmentConditionsNotMet>0) ShowWarningError("In day " + RoundSigDigits((Real64)DayOfSim, 1) + " of simulation, system " + Name.c_str() + " was unable to operate for " + RoundSigDigits((Real64)count_EnvironmentConditionsNotMet, 1)+ " timesteps because environment conditions were beyond the allowable operating range for any mode.");
				if (count_SAHR_OC_MetOnce>0)  ShowWarningError("In day " + RoundSigDigits((Real64)DayOfSim, 1) + " of simulation, " + Name.c_str() + " failed to meet supply air humidity ratio for " + RoundSigDigits(Real64(count_SAHR_OC_MetOnce), 1) + " time steps. For these time steps For these time steps" + Name.c_str() + " was set to mode 0");
				if (count_SAT_OC_MetOnce>0) ShowWarningError("In day " + RoundSigDigits((Real64)DayOfSim, 1) + " of simulation, " + Name.c_str() + " failed to meet supply air temperature constraints for " + RoundSigDigits(Real64(count_SAT_OC_MetOnce), 1) + " time steps. For these time steps For these time steps" + Name.c_str()+" was set to mode 0" );
				
				ShowWarningError("In day " + RoundSigDigits((Real64)DayOfSim, 1) + " of simulation, " + Name.c_str() + " failed to  satisfy sensible load for " + RoundSigDigits((Real64)count_DidWeNotMeetLoad, 1) + " time steps. For these time steps settings were selected to provide as much sensible cooling or heating as possible, given other constraints.");
				
				count_SAT_OC_MetOnce = 0;
				count_DidWeNotMeetLoad = 0;
				count_SAHR_OC_MetOnce = 0;
				count_EnvironmentConditionsMetOnce = 0;
				count_EnvironmentConditionsNotMet = 0;
				WarnOnceFlag = false;
			}
			if (DataGlobals::HourOfDay == 1 && !WarnOnceFlag && !WarmupFlag)
			{
				WarnOnceFlag = true;
			}
			return ErrorCode; 
		}

		int Model::CurrentPrimaryMode()
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// retunrs the primary mode of operation 

			// METHODOLOGY EMPLOYED:
			// 

			// REFERENCES:
			// na

			// Using/Aliasing
			
			if (CurrentOperatingSettings.size() > 0)
			{
				return CurrentOperatingSettings[0].Mode;
			}
			else return -1;
		}
		Real64 Model::CurrentPrimaryRuntimeFraction()
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// returns the runtime fraction of the primary setting.

			// METHODOLOGY EMPLOYED:
			// 

			// REFERENCES:
			// na

			// Using/Aliasing
			if (CurrentOperatingSettings.size() > 0)
			{
				return CurrentOperatingSettings[0].Runtime_Fraction;
			}
			else return -1;
		}
		void Model::DetermineCoolingVentilationOrHumidificationNeeds(CStepInputs &StepIns)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017 
			//       MODIFIED       
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Sets member boolean variables to establish if the Cooling, Heating, ventilation or dehumidifcation needs are met. 

			// METHODOLOGY EMPLOYED:
			// 

			// REFERENCES:
			// na

			// Using/Aliasing
			CoolingRequested = false;
			HeatingRequested = false;
			VentilationRequested = false;
			DehumidificationRequested = false;
			HumidificationRequested = false;
			// establish if conditioning needed 
			if (StepIns.RequestedCoolingLoad >= MINIMUM_LOAD_TO_ACTIVATE)
			{
				CoolingRequested = true;
				StepIns.RequestedHeatingLoad = 0;
			}
			if (StepIns.RequestedHeatingLoad <= -MINIMUM_LOAD_TO_ACTIVATE)
			{
				HeatingRequested = true;
				StepIns.RequestedCoolingLoad = 0;
			}
			// establish if ventilation needed 
			if (MinOA_Msa > 0) VentilationRequested = true;
			// Load required to meet dehumidifying setpoint (<0 = a dehumidify load)  [kgWater/s]
			if (StepIns.ZoneDehumidificationLoad < 0)
			{
				DehumidificationRequested = true;
				StepIns.ZoneMoistureLoad = 0;
			}
			// Load required to meet humidifying setpoint (>0 = a humidify load) [kgWater/s]
			if (StepIns.ZoneMoistureLoad > 0)
			{
				StepIns.ZoneDehumidificationLoad = 0;
				HumidificationRequested = true;
			}
		}

		//doStep is passed some variables that could have just used the class members, but this adds clarity about whats needed, especially helpful in unit testing
		void Model::doStep(
			Real64 RequestedCoolingLoad, // in joules, cooling load as negitive
			Real64 RequestedHeatingLoad, // in joules, heating load as positive
			Real64 OutputRequiredToHumidify, // Load required to meet humidifying setpoint (>0 = a humidify load) [kgWater/s]
			Real64 OutputRequiredToDehumidify, // Load required to meet dehumidifying setpoint (<0 = a dehumidify load)  [kgWater/s]
			Real64 DesignMinVR) //mass flow rate of design ventilation air kg/s
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Dutton
			//       DATE WRITTEN   May 2017
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// This subroutine Model::doStep, the main calculation steps
			//1)Collate required inputs into a CStepInputs, this helps with the unit tests 
			//   so we always know what values need to be set.
			//2)Calculate W humidity ratios for outdoor air and return air.
			//3)Sets boolean values for each potential conditioning requirement;
			//  CoolingRequested, HeatingRequested, VentilationRequested, DehumidificationRequested, HumidificationRequested 
			//4)Take the first operating mode which is always standby and calculate the NormalizationReference 
			//  and then use curves to determine performance metrics for the standby mode including energy use and other outputs
			//5)Test system availbility status and go into standby if unit is off or not needed (booleans listed in 3 are all false)
			//6) Set the operating conditions and respective part load fractions.
			//7) Set timestep average outlet condition, considering all operating conditions and runtimes.
			// METHODOLOGY EMPLOYED:
			// na

			// REFERENCES: OutletVolumetricFlowRate, SupplyVentilationVolume, MinOA_Msa, SupplyVentilationAir
			// na
			//!!!!!!!!!!!!!!!!!!!!!!!!! this debug code will be removed nearer the code freeze, please don't comment on it, it will be gone.
			if ((DataGlobals::HourOfDay == DebugBreak) && !WarmupFlag)
			{
				int k = 1;//debug step
			}
			// set requested loads to output variables
			RequestedLoadToHeatingSetpoint = RequestedCoolingLoad;
			RequestedLoadToCoolingSetpoint = RequestedHeatingLoad;
			Real64 LambdaRa = Psychrometrics::PsyHfgAirFnWTdb(0, InletTemp);
			RequestedHumdificationMass = OutputRequiredToHumidify;
			RequestedHumdificationLoad = OutputRequiredToHumidify * LambdaRa / 1000; // [kW];
			RequestedHumdificationEnergy = OutputRequiredToHumidify * LambdaRa * TimeStepSys * SecInHour; // [j]

			RequestedDeHumdificationMass = OutputRequiredToDehumidify;
			RequestedDeHumdificationLoad = OutputRequiredToDehumidify * LambdaRa; // [kW]; 
			RequestedDeHumdificationEnergy = OutputRequiredToDehumidify * LambdaRa * TimeStepSys * SecInHour; // [j] 

			MinOA_Msa = DesignMinVR;// as mass flow kg/s

			//Collate all the inputs required for calculation into one local data structure CStepInputs, this helps with the unit tests so we always know what values need to be set
			CStepInputs StepIns;
			StepIns.Tosa= SecInletTemp;// degrees C
			StepIns.Tra = InletTemp;//degrees C
			StepIns.RHosa = SecInletRH;// RH as 0-1
			StepIns.RHra = InletRH;
			// For historical reasons cooling is  possitive, heating negitive throughout the calculation
			StepIns.RequestedCoolingLoad = -RequestedCoolingLoad / 1000; //convert to kw Cooling possitive now, heating negitive
			StepIns.RequestedHeatingLoad = -RequestedHeatingLoad / 1000; //convert to kw Cooling possitive now, heating negitive
	
			StepIns.ZoneMoistureLoad = RequestedHumdificationLoad;  StepIns.ZoneDehumidificationLoad = RequestedDeHumdificationLoad; 
			StepIns.MinimumOA = DesignMinVR;
			// calculate W humidity ratios for outdoor air and return air
			Real64 Wosa = PsyWFnTdbRhPb(StepIns.Tosa, StepIns.RHosa, 101325);
			Real64 Wra = PsyWFnTdbRhPb(StepIns.Tra, StepIns.RHra, 101325);
			// Sets boolean values for each potential conditioning requirement;  CoolingRequested, HeatingRequested, VentilationRequested, DehumidificationRequested, HumidificationRequested 
			DetermineCoolingVentilationOrHumidificationNeeds(StepIns);
			// Take the first operating mode which is always standby and calculate the NormalizationReference 
			// and then use curves to determine performance metrics for the standby mode including energy use and other outputs
			
			CMode Mode = *(OperatingModes.begin());
			// 
			if (SetStandByMode(Mode, StepIns.Tosa, Wosa, StepIns.Tra, Wra))
			{
				std::string ObjectID = Name.c_str();
				ShowSevereError("Standby mode not defined correctly, as the mode is defined there are zero combinations of acceptible outside air fractions and supply air mass flow rate, called in object " + ObjectID);
			}
			// Test system availbility status 
			UnitOn = 1;
			StandBy = false;
			if (GetCurrentScheduleValue(SchedPtr) <= 0) {
				UnitOn = 0;
			}
			// Go into standby if unit is off or not needed
			if (((!CoolingRequested && !HeatingRequested) && !VentilationRequested &&!HumidificationRequested &&!DehumidificationRequested) || !UnitOn) // what about humid / dehumid
			{
				StandBy = true;
				oStandBy.Runtime_Fraction = 1;
				CurrentOperatingSettings[0]= oStandBy;
				ErrorCode = 0;
				PrimaryMode = 0;
				PrimaryModeRuntimeFraction = 0;
			}
			else
			{
				// set the operating conditions and respective part load fractions.
				ErrorCode = SetOperatingSetting(StepIns); 
			}
			
			Real64 QTotZoneOut = 0;
			// now class members QSensZoneOut = 0;
			// QLatentZoneOut = 0;

			Real64 QTotSystemOut = 0;
			Real64 QSensSystemOut = 0;
			Real64 QLatentSystemOut = 0;
			// Even if its off or in standby we still need to continue to calculate standby loads
			// All powers are calculated in Watts amd energies in Joules
			
			SupplyVentilationVolume = CalculateTimeStepAverage(SYSTEMOUTPUTS::VENTILATION_AIR_V);
			if (StdRhoAir > 1)
			{
				SupplyVentilationAir = SupplyVentilationVolume * StdRhoAir;
			}
			else
			{
				SupplyVentilationAir = SupplyVentilationVolume * 1.225;
			}
			// set timestep average outlet condition, considering all operating conditions and runtimes.
			OutletTemp = CheckVal_T(CalculateTimeStepAverage(SYSTEMOUTPUTS::SUPPLY_AIR_TEMP));
			OutletHumRat = CheckVal_W(CalculateTimeStepAverage(SYSTEMOUTPUTS::SUPPLY_AIR_HR), OutletTemp, OutletPressure);
	
			OutletRH = PsyRhFnTdbWPb(OutletTemp, OutletHumRat, OutletPressure); //could also use outlet pressure instead of fixed
			Real64 OperatingAverageMixedAirTemperature = CalculateTimeStepAverage(SYSTEMOUTPUTS::MIXED_AIR_TEMP);
			Real64 OperatingMixedAirW = CalculateTimeStepAverage(SYSTEMOUTPUTS::MIXED_AIR_HR);
			Real64 MixedAirEnthalpy = PsyHFnTdbW(OperatingAverageMixedAirTemperature, OperatingMixedAirW);
			OutletEnthalpy = PsyHFnTdbRhPb(OutletTemp, OutletRH, InletPressure); //consider if inlet and outlet presures are different
			OutletMassFlowRate = CalculateTimeStepAverage(SYSTEMOUTPUTS::SUPPLY_MASS_FLOW);

			if (StdRhoAir > 1)
			{
				OutletVolumetricFlowRate = OutletMassFlowRate / StdRhoAir;
			}
			else
			{
				OutletVolumetricFlowRate = OutletMassFlowRate / 1.225;
			}
			
			if (!StandBy)
			{
				if (OutletMassFlowRate > 0)
				{
					averageOSAF = SupplyVentilationAir / OutletMassFlowRate;
				}
				else
				{
					std::string ObjectID = Name.c_str();
					if (CoolingRequested || HeatingRequested)
					{
						ShowSevereError("Outlet air mass flow rate of zero during period with conditioning need, check mode definition. Called in object " + Name);
					}
					averageOSAF = 1;
				}
				//Calculate timestep average unit and system 
				PrimaryMode = CurrentPrimaryMode();
				PrimaryModeRuntimeFraction = CurrentPrimaryRuntimeFraction();
				Real64 Outletcp = PsyCpAirFnWTdb(OutletHumRat, OutletTemp); //J/degreesK.kg 
				Real64 Returncp = PsyCpAirFnWTdb(Wra, StepIns.Tra); //J/degreesK.kg 
				Real64 Outdoorcp = PsyCpAirFnWTdb(Wosa, StepIns.Tosa); //J/degreesK.kg 
				//Zone Sensible Cooling{ W } = m'SA {kg/s} * 0.5*(cpRA+cpSA) {kJ/kg-C} * (T_RA - T_SA) {C}
				//Zone Latent Cooling{ W } = m'SAdryair {kg/s} * L {kJ/kgWater} * (HR_RA - HR_SA) {kgWater/kgDryAir}
				//Zone Total Cooling{ W } = m'SAdryair {kg/s} * (h_RA - h_SA) {kJ/kgDryAir}		
				QSensZoneOut = OutletMassFlowRate* 0.5* (Returncp + Outletcp)*(StepIns.Tra - OutletTemp); //Watts
				Real64 OutletMassFlowRateDry = OutletMassFlowRate * (1 - Wsa);
				Real64 LambdaSa = Psychrometrics::PsyHfgAirFnWTdb(0, OutletTemp);
				QLatentZoneOut = 1000 * LambdaSa * OutletMassFlowRateDry* (InletHumRat - OutletHumRat); //Watts
				QTotZoneOut = OutletMassFlowRateDry * (InletEnthalpy - OutletEnthalpy); //Watts
				Real64 QLatentCheck = QTotZoneOut - QSensZoneOut;//Watts

				//System Sensible Cooling{ W } = m'SA {kg/s} * 0.5*(cpRA + OSAF*(cpOSA-cpRA) + cpSA) {kJ/kg-C} * (T_RA + OSAF*(T_OSA - T_RA)  - T_SA) 
				//System Latent Cooling{ W } = m'SAdryair {kg/s} * L {kJ/kgWater} * (HR_RA + OSAF *(HR_OSA - HR_RA) - HR_SA) {kgWater/kgDryAir}
				//System Total Cooling{ W } = m'SAdryair {kg/s} * (h_RA + OSAF*(h_OSA - h_RA) - h_SA) {kJ/kgDryAir}

				Real64 SystemTimeStepCp = Returncp + averageOSAF * (Outdoorcp - Returncp) + Outletcp;//cpRA + OSAF*(cpOSA-cpRA) + cpSA //J/degreesK.kg 
				Real64 SystemTimeStepW = InletHumRat + averageOSAF * (Wosa - Wra) - OutletHumRat;//HR_RA + OSAF *(HR_OSA - HR_RA) - HR_SA
				Real64 SystemTimeStepT = StepIns.Tra + averageOSAF * (StepIns.Tosa - StepIns.Tra) - OutletTemp;//T_RA + OSAF *(T_OSA - T_RA) - T_SA
				QSensSystemOut =  0.5* SystemTimeStepCp *OutletMassFlowRate* SystemTimeStepT;//w

				QLatentSystemOut = 1000 * LambdaSa * OutletMassFlowRateDry*SystemTimeStepW; //Watts
				QTotSystemOut = OutletMassFlowRateDry * (MixedAirEnthalpy - OutletEnthalpy);//Watts
				QLatentCheck = QTotSystemOut - QSensSystemOut;//Watts

				// reset outputs 
				ResetOutputs();
				//set UNIT outputs for cooling and heating 
				if (QTotZoneOut > 0) // zone cooling is possitive, else remain zero
				{
					UnitTotalCoolingRate = std::abs(QTotZoneOut); //Watts
					UnitTotalCoolingEnergy = UnitTotalCoolingRate * TimeStepSys * SecInHour; //J
				}
				else
				{
					UnitTotalHeatingRate = std::abs(QTotZoneOut); //Watts
					UnitTotalHeatingEnergy = UnitTotalHeatingRate * TimeStepSys * SecInHour; //J
				}

				if (QSensZoneOut > 0) // zone cooling is possitive, else remain zero
				{
					UnitSensibleCoolingRate = std::abs(QSensZoneOut); //Watts
					UnitSensibleCoolingEnergy = UnitSensibleCoolingRate * TimeStepSys * SecInHour;//J
				}
				else
				{
					UnitSensibleHeatingRate = std::abs(QSensZoneOut); //Watts
					UnitSensibleHeatingEnergy = UnitSensibleHeatingRate * TimeStepSys * SecInHour;//J
				}

				if ((UnitTotalCoolingRate - UnitSensibleCoolingRate) > 0)
				{
					UnitLatentCoolingRate = UnitTotalCoolingRate - UnitSensibleCoolingRate; //Watts
					UnitLatentCoolingEnergy = UnitTotalCoolingEnergy - UnitSensibleCoolingEnergy;//J
				}
				if ((UnitTotalCoolingRate - UnitSensibleCoolingRate) < 0)
				{
					UnitLatentHeatingRate = UnitTotalHeatingRate - UnitSensibleHeatingRate;//Watts
					UnitLatentHeatingEnergy = UnitTotalHeatingEnergy - UnitSensibleHeatingEnergy;//J
				}

				//set SYSTEM outputs
				if (QTotSystemOut > 0) // system cooling
				{
					SystemTotalCoolingRate = std::abs(QTotSystemOut);
					SystemTotalCoolingEnergy = SystemTotalCoolingRate * TimeStepSys * SecInHour;
				}
				else
				{
					SystemTotalHeatingRate = std::abs(QTotSystemOut);
					SystemTotalHeatingEnergy = SystemTotalHeatingRate * TimeStepSys * SecInHour;
				}

				if (QSensSystemOut > 0) //system sensible cooling
				{
					SystemSensibleCoolingRate = std::abs(QSensSystemOut);
					SystemSensibleCoolingEnergy = SystemSensibleCoolingRate * TimeStepSys * SecInHour;
				}
				else
				{
					SystemSensibleHeatingRate = std::abs(QSensSystemOut);
					SystemSensibleHeatingEnergy = SystemSensibleHeatingRate * TimeStepSys * SecInHour;
				}
				if ((SystemTotalCoolingRate - SystemSensibleCoolingRate) > 0)
				{
					SystemLatentCoolingRate = SystemTotalCoolingRate - SystemSensibleCoolingRate;
					SystemLatentCoolingEnergy = SystemTotalCoolingEnergy - SystemSensibleCoolingEnergy;
				}
				if ((SystemTotalHeatingRate - SystemSensibleHeatingRate) < 0)
				{
					SystemLatentHeatingRate = SystemTotalHeatingRate - SystemSensibleHeatingRate;
					SystemLatentHeatingEnergy = SystemTotalHeatingEnergy - SystemSensibleHeatingEnergy;
				}
			}
			else // unit is in standby so reset conditioning outputs
			{
				QTotZoneOut = 0;
				QSensZoneOut = 0;
				QLatentZoneOut = 0;
				QTotSystemOut = 0;
				QSensSystemOut = 0;
				QLatentSystemOut = 0;
				// reset outputs
				ResetOutputs();
				
	
			}

			// set timestep outputs calculated considering different runtime fractions.
			SupplyFanElectricPower = 1000 * CalculateTimeStepAverage(SYSTEMOUTPUTS::OSUPPLY_FAN_POWER); //Watts
			SupplyFanElectricEnergy = SupplyFanElectricPower*TimeStepSys * SecInHour;
			SecondaryFuelConsumptionRate = 1000 * CalculateTimeStepAverage(SYSTEMOUTPUTS::OSECOND_FUEL_USE);
			SecondaryFuelConsumption = SecondaryFuelConsumptionRate*TimeStepSys * SecInHour;
			ThirdFuelConsumptionRate = 1000 * CalculateTimeStepAverage(SYSTEMOUTPUTS::OTHIRD_FUEL_USE);
			ThirdFuelConsumption = ThirdFuelConsumptionRate*TimeStepSys * SecInHour;
			WaterConsumptionRate = CalculateTimeStepAverage(SYSTEMOUTPUTS::OWATER_USE);
			WaterConsumption = WaterConsumptionRate*TimeStepSys * SecInHour;
			ExternalStaticPressure=CalculateTimeStepAverage(SYSTEMOUTPUTS::OEXTERNAL_STATIC_PRESSURE);
					
					
			// fuel use in calculation is in Kw, powers are typically output in EP in Watts, so do conversion here. 
			FinalElectricalPower = 1000 * CalculateTimeStepAverage(SYSTEMOUTPUTS::SYSTEM_FUEL_USE);
			FinalElectricalEnergy = FinalElectricalPower*TimeStepSys * SecInHour;
			
		
	
		}

	}
}
