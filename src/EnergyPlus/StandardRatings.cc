// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// C++ Headers
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <StandardRatings.hh>
#include <CurveManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <OutputReportPredefined.hh>
#include <Psychrometrics.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace StandardRatings {

	// MODULE INFORMATION:
	//       AUTHOR         Chandan Sharma
	//       DATE WRITTEN   February 2012
	//       MODIFIED       February 2013, Bereket Nigusse
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module contains the subroutines required to calculate the following standard ratings of HVAC equipment
	// 1) Integrated Part Load Value (IPLV) rating for EIR and Reformulated EIR chillers
	// 2) a) Standard Rated (net) Cooling Capacity
	//    b) Seasonal Energy Efficiency Ratio (SEER)
	//    c) Energy Efficiency Ratio (EER),
	//    d) Integrated Energy Efficiency Ratio (IEER)
	//       for Air-to-Air Direct Expansion Air Conditioner and Heat Pumps having a single-speed compressor,
	//       fixed speed indoor supply air fan, and air-cooled condensers.
	// 3) Heating Seasonal Performance Factor (HSPF) for Air-Source Direct Expansion Heat Pumps having a single-speed compressor,
	//       fixed speed indoor supply air fan
	// 4) Seasonal Energy Efficiency Ratio (SEER) for Air-Source Direct Expansion multi-speed compressor Heat Pumps
	// 5) Heating Seasonal Performance Factor (HSPF) for Air-Source Direct Expansion multi-speed compressor Heat Pumps
	// METHODOLOGY EMPLOYED:
	// Using the user specified reference capacity, reference COP and performance curves, the chiller or DX coil models are executed
	// for standard test conditions as specified in ANSI/AHRI 550/590, 210/240 and 340/360. Then results of the simulated test points
	// are processed into standard ratings according to standard's procedures.

	// REFERENCES:
	// (1) AHRI Standard 550/590-2011:  Standard for Performance Rating of Water-Chilling Packages using the Vapor
	//                                  Compression Cycle. Arlington, VA:  Air-Conditioning, Heating,
	//                                  and Refrigeration Institute.
	// (2) ANSI/AHRI Standard 210/240-2008:  Standard for Performance Rating of Unitary Air-Conditioning and
	//                                       Air-Source Heat Pumps. Arlington, VA:  Air-Conditioning, Heating
	//                                       , and Refrigeration Institute.
	// (3) ANSI/AHRI Standard 340/360-2007:  Standard for Performance Rating of Commercial and Industrial
	//                                       Unitary Air-Conditioning and Heat Pump Equipment.  Arlington,
	//                                       VA:  Air-Conditioning, Heating, and Refrigeration Institute.

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	Real64 const IndoorCoilInletAirWetBulbTempRated( 19.44 ); // 19.44C (67F)  Tests A2, B2, B1, and F1
	Real64 const OutdoorCoilInletAirDryBulbTempRated( 35.0 ); // 35.00C (95F)  Tests A2, B2, B1, and F1
	Real64 const OutdoorCoilInletAirDryBulbTempTestA2( 35.0 ); // 35.00C (95F)  Test A2 (high speed)
	Real64 const OutdoorCoilInletAirDryBulbTempTestB2( 27.78 ); // 27.78C (82F)  Test B2 (high speed)
	Real64 const OutdoorCoilInletAirDryBulbTempTestB1( 27.78 ); // 27.78C (82F)  Test B1 (Low speed)
	Real64 const OutdoorCoilInletAirDryBulbTempTestF1( 19.44 ); // 19.44C (67F)  Test B1 (Low speed)

	// AHRI Standard 210/240-2008 Performance Test Conditions for Unitary Air-to-Air Air-Conditioning and Heat Pump Equipment
	Real64 const CoolingCoilInletAirWetBulbTempRated( 19.44 ); // 19.44C (67F)  Tests A and B
	Real64 const OutdoorUnitInletAirDryBulbTemp( 27.78 ); // 27.78C (82F)  Test B (for SEER)
	Real64 const OutdoorUnitInletAirDryBulbTempRated( 35.0 ); // 35.00C (95F)  Test A (rated capacity)
	Real64 const AirMassFlowRatioRated( 1.0 ); // AHRI test is at the design flow rate
	// and hence AirMassFlowRatio is 1.0
	Real64 const ConvFromSIToIP( 3.412141633 ); // Conversion from SI to IP [3.412 Btu/hr-W]
	Real64 const DefaultFanPowerPerEvapAirFlowRate( 773.3 ); // 365 W/1000 scfm or 773.3 W/(m3/s). The AHRI standard
	// specifies a nominal/default fan electric power consumption per rated air
	// volume flow rate to account for indoor fan electric power consumption
	// when the standard tests are conducted on units that do not have an
	// indoor air circulting fan. Used if user doesn't enter a specific value.
	Real64 const PLRforSEER( 0.5 ); // Part-load ratio for SEER calculation (single speed DX cooling coils)
	Array1D< Real64 > const ReducedPLR( 4, { 1.0, 0.75, 0.50, 0.25 } ); // Reduced Capacity part-load conditions
	Array1D< Real64 > const IEERWeightingFactor( 4, { 0.020, 0.617, 0.238, 0.125 } ); // EER Weighting factors (IEER)
	Real64 const OADBTempLowReducedCapacityTest( 18.3 ); // Outdoor air dry-bulb temp in degrees C (65F)
	// Std. AHRI AHRI 340/360 Dry-bulb Temp at reduced capacity, <= 0.444

	// Defrost control  (heat pump only)
	int const Timed( 1 ); // defrost cycle is timed
	int const OnDemand( 2 ); // defrost cycle occurs only when required
	int const TotalNumOfStandardDHRs( 16 ); // Total number of standard design heating requirements
	Array1D_int const TotalNumOfTemperatureBins( 6, { 9, 10, 13, 15, 18, 9 } ); // Total number of temperature
	// bins for a region
	Array1D< Real64 > const StandardDesignHeatingRequirement( 16, { 1465.36, 2930.71, 4396.07, 5861.42, 7326.78, 8792.14, 10257.49, 11722.85, 14653.56, 17584.27, 20514.98, 23445.70, 26376.41, 29307.12, 32237.83, 38099.26 } );
	// Standardized DHRs from ANSI/AHRI 210/240
	Real64 const CorrectionFactor( 0.77 ); // A correction factor which tends to improve the agreement
	// between calculated and measured building loads, dimensionless.
	Real64 const CyclicDegradationCoeff( 0.25 );
	Array1D< Real64 > const OutdoorDesignTemperature( 6, { 2.78, -2.78, -8.33, -15.0, -23.33, -1.11 } );
	// Outdoor design temperature for a region from ANSI/AHRI 210/240
	Array1D< Real64 > const OutdoorBinTemperature( 18, { 16.67, 13.89, 11.11, 8.33, 5.56, 2.78, 0.00, -2.78, -5.56, -8.33, -11.11, -13.89, -16.67, -19.44, -22.22, -25.00, -27.78, -30.56 } );
	// Fractional bin hours for different bin temperatures for region one, from ANSI/AHRI 210/240
	Array1D< Real64 > const RegionOneFracBinHoursAtOutdoorBinTemp( 18, { 0.291, 0.239, 0.194, 0.129, 0.081, 0.041, 0.019, 0.005, 0.001, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } );
	// Fractional bin hours for different bin temperatures for region two, from ANSI/AHRI 210/240
	Array1D< Real64 > const RegionTwoFracBinHoursAtOutdoorBinTemp( 18, { 0.215, 0.189, 0.163, 0.143, 0.112, 0.088, 0.056, 0.024, 0.008, 0.002, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } );
	// Fractional bin hours for different bin temperatures for region three, from ANSI/AHRI 210/240
	Array1D< Real64 > const RegionThreeFracBinHoursAtOutdoorBinTemp( 18, { 0.153, 0.142, 0.138, 0.137, 0.135, 0.118, 0.092, 0.047, 0.021, 0.009, 0.005, 0.002, 0.001, 0.0, 0.0, 0.0, 0.0, 0.0 } );
	// Fractional bin hours for different bin temperatures for region four, from ANSI/AHRI 210/240
	Array1D< Real64 > const RegionFourFracBinHoursAtOutdoorBinTemp( 18, { 0.132, 0.111, 0.103, 0.093, 0.1, 0.109, 0.126, 0.087, 0.055, 0.036, 0.026, 0.013, 0.006, 0.002, 0.001, 0.0, 0.0, 0.0 } );
	// Fractional bin hours for different bin temperatures for region five, from ANSI/AHRI 210/240
	Array1D< Real64 > const RegionFiveFracBinHoursAtOutdoorBinTemp( 18, { 0.106, 0.092, 0.086, 0.076, 0.078, 0.087, 0.102, 0.094, 0.074, 0.055, 0.047, 0.038, 0.029, 0.018, 0.01, 0.005, 0.002, 0.001 } );
	// Fractional bin hours for different bin temperatures for region six, from ANSI/AHRI 210/240
	Array1D< Real64 > const RegionSixFracBinHoursAtOutdoorBinTemp( 18, { 0.113, 0.206, 0.215, 0.204, 0.141, 0.076, 0.034, 0.008, 0.003, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } );

	// Representative cooling season Outdoor air temperature bin from ANSI/AHRI 210/240-2008
	int const NumOfOATempBins( 8 ); // number of outdoor temperature bins for cooling season
	Array1D< Real64 > const OutdoorBinTemperatureSEER( NumOfOATempBins, { 19.44, 22.22, 25.00, 27.78, 30.56, 33.33, 36.11, 38.89 } );
	// Fractional bin hours for different bin temperatures for cooling, from ANSI/AHRI 210/240 - 2008
	Array1D< Real64 > const CoolFracBinHoursAtOutdoorBinTemp( NumOfOATempBins, { 0.214, 0.231, 0.216, 0.161, 0.104, 0.052, 0.018, 0.004 } );

	Real64 const HeatingIndoorCoilInletAirDBTempRated( 21.11 ); // Heating coil entering air dry-bulb temperature in
	// degrees C (70F) Test H1, H2 and H3
	// (low and High Speed) Std. AHRI 210/240
	Real64 const HeatingOutdoorCoilInletAirDBTempH0Test( 16.67 ); // Outdoor air dry-bulb temp in degrees C (47F)
	// Test H0 (low and High Speed) Std. AHRI 210/240
	Real64 const HeatingOutdoorCoilInletAirDBTempRated( 8.33 ); // Outdoor air dry-bulb temp in degrees C (47F)
	// Test H1 or rated (low and High Speed) Std. AHRI 210/240
	Real64 const HeatingOutdoorCoilInletAirDBTempH2Test( 1.67 ); // Outdoor air dry-bulb temp in degrees C (35F)
	// Test H2 (low and High Speed) Std. AHRI 210/240
	Real64 const HeatingOutdoorCoilInletAirDBTempH3Test( -8.33 ); // Outdoor air dry-bulb temp in degrees C (17F)
	// Test H3 (low and High Speed) Std. AHRI 210/240

	// ANSI/ASHRAE Standard 127-2012 -- Method of Testing for Rating Computer and Data Processing Room Unitary Air Conditioners
	//  Class 1 23.9°C( 75.0°F ) 23.9°C( 75.0°F ) 23.9°C( 75.0°F ) 23.9°C( 75.0°F )
	//	Class 2 29.4°C( 85.0°F ) 29.4°C( 85.0°F ) 29.4°C( 85.0°F ) 29.4°C( 85.0°F )
	//	Class 3 35.0°C( 95.0°F ) 35.0°C( 95.0°F ) 35.0°C( 95.0°F ) 35.0°C( 95.0°F )
	//	Class 4 40.5°C( 105°F ) 40.5°C( 105°F ) 40.5°C( 105°F ) 40.5°C( 105°F )
	Array1D < Real64 > const IndoorDBTempClassI2IV( 4, { 23.9, 29.4, 35.0, 40.5 } );
	Real64 const IndoorTDPA2D( 11.1 );
	//35.0°C( 95.0°F ) 26.7°C( 80.0°F ) 18.3°C( 65.0°F ) 4.4°C( 40.0°F )
	Array1D < Real64 > const OutdoorDBTempAllClassA2D( 4, { 35.0, 26.7, 18.3, 4.4 } );

	// Functions

	void
	CalcChillerIPLV(
		std::string const & ChillerName, // Name of Chiller for which IPLV is calculated
		int const ChillerType, // Type of Chiller - EIR or Reformulated EIR
		Real64 const RefCap, // Reference capacity of chiller [W]
		Real64 const RefCOP, // Reference coefficient of performance [W/W]
		int const CondenserType, // Type of Condenser - Air Cooled, Water Cooled or Evap Cooled
		int const CapFTempCurveIndex, // Index for the total cooling capacity modifier curve
		int const EIRFTempCurveIndex, // Index for the energy input ratio modifier curve
		int const EIRFPLRCurveIndex, // Index for the EIR vs part-load ratio curve
		Real64 const MinUnloadRat, // Minimum unloading ratio
		Optional< Real64 const > EvapVolFlowRate, // Reference water volumetric flow rate through the evaporator [m3/s]
		Optional_int_const CondLoopNum, // condenser water plant loop index number
		Optional< Real64 const > OpenMotorEff // Open chiller motor efficiency [fraction, 0 to 1]
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma, FSEC
		//       DATE WRITTEN   January 2012
		//       Modified       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//     Calculates Integrated Part Load Value (IPLV) for EIR and reformulated EIR chillers.
		//     Writes the result to EIO file.
		// METHODOLOGY EMPLOYED:
		// (1) Obtains the reference cooling capacity, reference COP and performance curves of the chiller
		// (2) Evaluates the cooling capacity at AHRI test conditions (Per AHRI 551/591,2011 Table 3)
		// (3) Evaluates the EIR at AHRI test conditions (Per AHRI 551/591,2011 Table 3)
		// (4) The EER is evaluated from the total cooling capacity and total electric power
		//     evaluated at the standard rated test conditions.  The IPLV is a weighted value of the COP evaluated
		//     at four different capacities of 100%, 75%, 50% and 25%.  The reduced capacity COPs are evaluated
		//     at different outdoor coil entering temperatures.
		// REFERENCES:
		// (1) AHRI Standard 551/591-2011:  Standard for Performance Rating of Water-Chilling Packages using the Vapor
		//                                  Compression Cycle. Arlington, VA:  Air-Conditioning, Heating,
		//                                  and Refrigeration Institute.

		// USE STATEMENTS:

		// Using/Aliasing
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using General::SolveRegulaFalsi;
		using General::RoundSigDigits;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_Chiller_ElectricEIR;
		using DataPlant::TypeOf_Chiller_ElectricReformEIR;
		using CurveManager::CurveValue;
		using CurveManager::GetCurveType;
		using CurveManager::GetCurveName;

		// Locals
		Real64 const ConvFromSIToIP( 3.412141633 ); // Conversion from SI to IP [3.412 Btu/hr-W]
		static Array1D< Real64 > const ReducedPLR( 4, { 1.0, 0.75, 0.50, 0.25 } ); // Reduced Capacity part-load conditions

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// (function of leaving chilled water temperature and
		//  entering condenser fluid temperature)
		// (function of leaving chilled water temperature and
		//  entering condenser fluid temperature)

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const AirCooled( 1 );
		int const WaterCooled( 2 );

		Real64 const EvapOutletTemp( 6.67 ); // (44F)
		Real64 const Acc( 0.0001 ); // Accuracy of result
		int const NumOfReducedCap( 4 ); // Number of reduced capacity test conditions (100%,75%,50%,and 25%)
		int const IterMax( 500 ); // Maximum number of iterations
		static Array1D< Real64 > const IPLVWeightingFactor( 4, { 0.010, 0.42, 0.45, 0.12 } ); // EER Weighting factors (IPLV)
		static std::string const RoutineName( "CalcChillerIPLV" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 AvailChillerCap( 0.0 ); // Chiller available capacity at current operating conditions [W]
		static Real64 EnteringWaterTempReduced( 0.0 ); // Entering Condenser Water Temperature at reduced conditions [C]
		static Real64 EnteringAirDryBulbTempReduced( 0.0 ); // Outdoor unit entering air dry-bulb temperature
		// at reduced capacity [C]
		static Real64 EnteringAirWetBulbTempReduced( 0.0 ); // Outdoor unit entering air wet-bulb temperature
		// at reduced capacity [C]
		static Real64 CondenserInletTemp( 0.0 ); // Entering Condenser Temperature at reduced conditions [C]
		static Real64 CondenserOutletTemp0( 0.0 ); // Lower bound for condenser outlet temperature [C]
		static Real64 CondenserOutletTemp1( 0.0 ); // Upper bound for condenser outlet temperature [C]
		static Real64 CondenserOutletTemp( 0.0 ); // Calculated condenser outlet temperature which corresponds
		// to EnteringWaterTempReduced above [C]
		static Real64 Cp( 0.0 ); // Water specific heat [J/(kg*C)]
		static Real64 Rho( 0.0 ); // Water density [kg/m3]
		static Real64 IPLV( 0.0 ); // Integerated Part Load Value in SI [W/W]
		static Real64 EIR( 0.0 ); // Inverse of COP at reduced capacity test conditions (100%, 75%, 50%, and 25%)
		static Real64 Power( 0.0 ); // Power at reduced capacity test conditions (100%, 75%, 50%, and 25%)
		static Real64 COPReduced( 0.0 ); // COP at reduced capacity test conditions (100%, 75%, 50%, and 25%)
		static Real64 LoadFactor( 0.0 ); // Fractional "on" time for last stage at the desired reduced capacity,
		// (dimensionless)
		static Real64 DegradationCoeff( 0.0 ); // Degradation coeficient, (dimenssionless)
		static Real64 ChillerCapFT( 0.0 ); // Chiller capacity fraction (evaluated as a function of temperature)
		static Real64 ChillerEIRFT( 0.0 ); // Chiller electric input ratio (EIR = 1 / COP) as a function of temperature
		static Real64 ChillerEIRFPLR( 0.0 ); // Chiller EIR as a function of part-load ratio (PLR)
		static Real64 PartLoadRatio( 0.0 ); // Part load ratio (PLR) at which chiller is operatign at reduced capacity
		int RedCapNum; // Integer counter for reduced capacity
		int SolFla; // Flag of solver
		Array1D< Real64 > Par( 11 ); // Parameter array need for RegulaFalsi routine

		// Initialize local variables
		AvailChillerCap = 0.0;
		EnteringWaterTempReduced = 0.0;
		EnteringAirDryBulbTempReduced = 0.0;
		EnteringAirWetBulbTempReduced = 0.0;
		CondenserInletTemp = 0.0;
		CondenserOutletTemp0 = 0.0;
		CondenserOutletTemp1 = 0.0;
		CondenserOutletTemp = 0.0;
		Cp = 0.0;
		Rho = 0.0;
		IPLV = 0.0;
		EIR = 0.0;
		Power = 0.0;
		COPReduced = 0.0;
		LoadFactor = 0.0;
		DegradationCoeff = 0.0;
		ChillerCapFT = 0.0;
		ChillerEIRFT = 0.0;
		ChillerEIRFPLR = 0.0;
		PartLoadRatio = 0.0;

		CheckCurveLimitsForIPLV( ChillerName, ChillerType, CondenserType, CapFTempCurveIndex, EIRFTempCurveIndex );

		// IPLV calculations:
		for ( RedCapNum = 1; RedCapNum <= NumOfReducedCap; ++RedCapNum ) {
			if ( CondenserType == WaterCooled ) {
				// get the entering water temperature for the reduced capacity test conditions
				if ( ReducedPLR( RedCapNum ) > 0.50 ) {
					EnteringWaterTempReduced = 8.0 + 22.0 * ReducedPLR( RedCapNum );
				} else {
					EnteringWaterTempReduced = 19.0;
				}
				CondenserInletTemp = EnteringWaterTempReduced;
			} else if ( CondenserType == AirCooled ) {
				// get the outdoor air dry bulb temperature for the reduced capacity test conditions
				if ( ReducedPLR( RedCapNum ) > 0.3125 ) {
					EnteringAirDryBulbTempReduced = 3.0 + 32.0 * ReducedPLR( RedCapNum );
				} else {
					EnteringAirDryBulbTempReduced = 13.0;
				}
				CondenserInletTemp = EnteringAirDryBulbTempReduced;
			} else { // EvaporativelyCooled Condenser
				// get the outdoor air wet bulb temperature for the reduced capacity test conditions
				EnteringAirWetBulbTempReduced = 10.0 + 14.0 * ReducedPLR( RedCapNum );
				CondenserInletTemp = EnteringAirWetBulbTempReduced;
			}

			{ auto const SELECT_CASE_var( ChillerType );

			if ( SELECT_CASE_var == TypeOf_Chiller_ElectricEIR ) {
				// Get capacity curve info with respect to CW setpoint and entering condenser temps
				ChillerCapFT = CurveValue( CapFTempCurveIndex, EvapOutletTemp, CondenserInletTemp );

				ChillerEIRFT = CurveValue( EIRFTempCurveIndex, EvapOutletTemp, CondenserInletTemp );

				if ( ReducedPLR( RedCapNum ) >= MinUnloadRat ) {
					ChillerEIRFPLR = CurveValue( EIRFPLRCurveIndex, ReducedPLR( RedCapNum ) );
					PartLoadRatio = ReducedPLR( RedCapNum );
				} else {
					ChillerEIRFPLR = CurveValue( EIRFPLRCurveIndex, MinUnloadRat );
					PartLoadRatio = MinUnloadRat;
				}

			} else if ( SELECT_CASE_var == TypeOf_Chiller_ElectricReformEIR ) {
				Cp = GetSpecificHeatGlycol( PlantLoop( CondLoopNum ).FluidName, EnteringWaterTempReduced, PlantLoop( CondLoopNum ).FluidIndex, RoutineName );

				Rho = GetDensityGlycol( PlantLoop( CondLoopNum ).FluidName, EnteringWaterTempReduced, PlantLoop( CondLoopNum ).FluidIndex, RoutineName );

				Par( 1 ) = EnteringWaterTempReduced;
				Par( 2 ) = EvapOutletTemp;
				Par( 3 ) = Cp;
				Par( 4 ) = ReducedPLR( RedCapNum );
				Par( 5 ) = EvapVolFlowRate * Rho;
				Par( 6 ) = CapFTempCurveIndex;
				Par( 7 ) = EIRFTempCurveIndex;
				Par( 8 ) = EIRFPLRCurveIndex;
				Par( 9 ) = RefCap;
				Par( 10 ) = RefCOP;
				Par( 11 ) = OpenMotorEff;
				CondenserOutletTemp0 = EnteringWaterTempReduced + 0.1;
				CondenserOutletTemp1 = EnteringWaterTempReduced + 10.0;
				SolveRegulaFalsi( Acc, IterMax, SolFla, CondenserOutletTemp, ReformEIRChillerCondInletTempResidual, CondenserOutletTemp0, CondenserOutletTemp1, Par );
				if ( SolFla == -1 ) {
					ShowWarningError( "Iteration limit exceeded in calculating Reform Chiller IPLV" );
					ShowContinueError( "Reformulated Chiller IPLV calculation failed for " + ChillerName );
				} else if ( SolFla == -2 ) {
					ShowWarningError( "Bad starting values for calculating Reform Chiller IPLV" );
					ShowContinueError( "Reformulated Chiller IPLV calculation failed for " + ChillerName );
				}

				ChillerCapFT = CurveValue( CapFTempCurveIndex, EvapOutletTemp, CondenserOutletTemp );

				ChillerEIRFT = CurveValue( EIRFTempCurveIndex, EvapOutletTemp, CondenserOutletTemp );

				if ( ReducedPLR( RedCapNum ) >= MinUnloadRat ) {
					ChillerEIRFPLR = CurveValue( EIRFPLRCurveIndex, CondenserOutletTemp, ReducedPLR( RedCapNum ) );
					PartLoadRatio = ReducedPLR( RedCapNum );
				} else {
					ChillerEIRFPLR = CurveValue( EIRFPLRCurveIndex, CondenserOutletTemp, MinUnloadRat );
					PartLoadRatio = MinUnloadRat;
				}
			} else {
				// should not come here, do nothing
			}}

			// Available chiller capacity as a function of temperature
			if ( RefCap > 0.0 && RefCOP > 0.0 && ChillerCapFT > 0.0 && ChillerEIRFT > 0.0 ) {
				AvailChillerCap = RefCap * ChillerCapFT;
				Power = ( AvailChillerCap / RefCOP ) * ChillerEIRFPLR * ChillerEIRFT;
				EIR = Power / ( PartLoadRatio * AvailChillerCap );

				if ( ReducedPLR( RedCapNum ) >= MinUnloadRat ) {
					COPReduced = 1.0 / EIR;
				} else {
					LoadFactor = ( ReducedPLR( RedCapNum ) * RefCap ) / ( MinUnloadRat * AvailChillerCap );
					DegradationCoeff = 1.130 - 0.130 * LoadFactor;
					COPReduced = 1.0 / ( DegradationCoeff * EIR );
				}
				IPLV += IPLVWeightingFactor( RedCapNum ) * COPReduced;
			} else {
				{ auto const SELECT_CASE_var( ChillerType );
				if ( SELECT_CASE_var == TypeOf_Chiller_ElectricEIR ) {
					ShowWarningError( "Chiller:Electric:EIR = " + ChillerName + ":  Integrated Part Load Value (IPLV) cannot be calculated." );
				} else if ( SELECT_CASE_var == TypeOf_Chiller_ElectricReformEIR ) {

					ShowWarningError( "Chiller:Electric:ReformulatedEIR = " + ChillerName + ":  Integrated Part Load Value (IPLV) cannot be calculated." );
				}}
				if ( RefCap <= 0.0 ) {
					ShowContinueError( " Check the chiller autosized or user specified capacity. Autosized or specified chiller capacity = " + RoundSigDigits( RefCap, 2 ) );
				}
				if ( RefCOP <= 0.0 ) {
					ShowContinueError( " Check the chiller reference or rated COP specified. Specified COP = " + RoundSigDigits( RefCOP, 2 ) );
				}
				if ( ChillerCapFT <= 0.0 ) {
					ShowContinueError( " Check limits in Cooling Capacity Function of Temperature Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFTempCurveIndex ) + '.' );
					ShowContinueError( " ..ChillerCapFT value at standard test condition = " + RoundSigDigits( ChillerCapFT, 2 ) );
				}
				if ( ChillerEIRFT <= 0.0 ) {
					ShowContinueError( " Check limits in EIR Function of Temperature Curve, Curve Type = " + GetCurveType( EIRFTempCurveIndex ) + ", Curve Name = " + GetCurveName( EIRFTempCurveIndex ) + '.' );
					ShowContinueError( " ..ChillerEIRFT value at standard test condition = " + RoundSigDigits( ChillerEIRFT, 2 ) );
				}
				IPLV = 0.0;
				break;
			}
		}

		// Writes the IPLV value to the EIO file and standard tabular output tables
		ReportChillerIPLV( ChillerName, ChillerType, IPLV, IPLV * ConvFromSIToIP );

	}

	Real64
	ReformEIRChillerCondInletTempResidual(
		Real64 const CondenserOutletTemp, // Condenser outlet temperature (boundary condition or guess value) [C]
		Array1< Real64 > const & Par // par(1)  = Condenser inlet temperature at AHRI Standard
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   February 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculates residual function as described below
		// Residuum = (CondenserInletTempAtAHRIConditions - CondenserInletTemp) / CondenserInletTempAtAHRIConditions.
		// CondenserInletTemp here depends on the CondenserOutletTemp which is being varied to zero the residual.

		// METHODOLOGY EMPLOYED:
		// Varies CondenserOutletTemp until a balance point exists where the model output corresponds to the desired
		// independent variable (i.e. CondenserInletTemp is within tolerance of CondenserInletTempAtAHRIConditions)

		// REFERENCES:

		// Using/Aliasing
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using CurveManager::CurveValue;

		// Return value
		Real64 Residuum; // Residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//           551/591 conditons[C]
		// par(2)  = Evaporator outlet temperature [C]
		// par(3)  = Water specific heat [J/(kg*C)]
		// par(4)  = Part load ratio
		// par(5)  = Evaporator mass flow rate [kg/s]
		// par(6)  = Index for the total cooling capacity modifier curve
		// par(7)  = Index for the energy input ratio modifier curve
		// par(8)  = Index for the EIR vs part-load ratio curve
		// par(9)  = Reference capacity of chiller [W]
		// par(10) = Reference coefficient of performance [W/W]
		// par(11) = Open chiller motor efficiency [fraction, 0 to 1]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static Real64 AvailChillerCap( 0.0 ); // Chiller available capacity at current operating conditions [W]
		static Real64 CondenserInletTemp( 0.0 ); // Calculated condenser inlet temperature [C]
		static Real64 EvapOutletTemp( 0.0 ); // Evaporator outlet temperature temperature [C]
		static Real64 QEvap( 0.0 ); // Rate of heat transfer to the evaporator coil [W]
		static Real64 QCond( 0.0 ); // Rate of heat transfer to the condenser coil [W]
		static Real64 Power( 0.0 ); // Power at reduced capacity test conditions (100%, 75%, 50%, and 25%)
		static Real64 ReformEIRChillerCapFT( 0.0 ); // Chiller capacity fraction (evaluated as a function of temperature)
		static Real64 ReformEIRChillerEIRFT( 0.0 ); // Chiller electric input ratio (EIR = 1 / COP) as a function of temperature
		static Real64 ReformEIRChillerEIRFPLR( 0.0 ); // Chiller EIR as a function of part-load ratio (PLR)

		EvapOutletTemp = Par( 2 );

		ReformEIRChillerCapFT = CurveValue( int( Par( 6 ) ), EvapOutletTemp, CondenserOutletTemp );

		ReformEIRChillerEIRFT = CurveValue( int( Par( 7 ) ), EvapOutletTemp, CondenserOutletTemp );

		// Available chiller capacity as a function of temperature
		AvailChillerCap = Par( 9 ) * ReformEIRChillerCapFT;

		ReformEIRChillerEIRFPLR = CurveValue( int( Par( 8 ) ), CondenserOutletTemp, Par( 4 ) );

		Power = ( AvailChillerCap / Par( 10 ) ) * ReformEIRChillerEIRFPLR * ReformEIRChillerEIRFT;

		QEvap = AvailChillerCap * Par( 4 );

		QCond = Power * Par( 11 ) + QEvap;

		if ( Par( 6 ) > MassFlowTolerance ) {
			CondenserInletTemp = CondenserOutletTemp - QCond / Par( 5 ) / Par( 3 );
		}

		Residuum = ( Par( 1 ) - CondenserInletTemp ) / Par( 1 );

		return Residuum;
	}

	void
	ReportChillerIPLV(
		std::string const & ChillerName, // Name of Chiller for which IPLV is calculated
		int const ChillerType, // Type of Chiller - EIR or Reformulated EIR
		Real64 const IPLVValueSI, // IPLV value in SI units {W/W}
		Real64 const IPLVValueIP // IPLV value in IP units {Btu/W-h}
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   January 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine writes the IPLV values in SI and IP units to
		// the "eio" and tabular output files for EIR Chillers.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace OutputReportPredefined;
		using DataGlobals::OutputFileInits;
		using General::RoundSigDigits;
		using DataPlant::TypeOf_Chiller_ElectricEIR;
		using DataPlant::TypeOf_Chiller_ElectricReformEIR;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );

		// Formats
		static gio::Fmt Format_990( "('! <Chiller Standard Rating Information>, Component Type, Component Name, ','IPLV in SI Units {W/W}, ','IPLV in IP Units {Btu/W-h}')" );
		static gio::Fmt Format_991( "(' Chiller Standard Rating Information, ',A,', ',A,', ',A,', ',A)" );

		if ( MyOneTimeFlag ) {
			gio::write( OutputFileInits, Format_990 );
			MyOneTimeFlag = false;
		}

		{ auto const SELECT_CASE_var( ChillerType );
		if ( SELECT_CASE_var == TypeOf_Chiller_ElectricEIR ) {

			gio::write( OutputFileInits, Format_991 ) << "Chiller:Electric:EIR" << ChillerName << RoundSigDigits( IPLVValueSI, 2 ) << RoundSigDigits( IPLVValueIP, 2 );
			PreDefTableEntry( pdchMechType, ChillerName, "Chiller:Electric:EIR" );

		} else if ( SELECT_CASE_var == TypeOf_Chiller_ElectricReformEIR ) {

			gio::write( OutputFileInits, Format_991 ) << "Chiller:Electric:ReformulatedEIR" << ChillerName << RoundSigDigits( IPLVValueSI, 2 ) << RoundSigDigits( IPLVValueIP, 2 );
			PreDefTableEntry( pdchMechType, ChillerName, "Chiller:Electric:ReformulatedEIR" );

		}}

		PreDefTableEntry( pdchMechIPLVSI, ChillerName, RoundSigDigits( IPLVValueSI, 2 ) );
		PreDefTableEntry( pdchMechIPLVIP, ChillerName, RoundSigDigits( IPLVValueIP, 2 ) );

	}

	void
	CheckCurveLimitsForIPLV(
		std::string const & ChillerName, // Name of Chiller
		int const ChillerType, // Type of Chiller - EIR or ReformulatedEIR
		int const CondenserType, // Type of Condenser - Air Cooled, Water Cooled or Evap Cooled
		int const CapFTempCurveIndex, // Index for the total cooling capacity modifier curve
		int const EIRFTempCurveIndex // Index for the energy input ratio modifier curve
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR            Chandan Sharma, FSEC
		//       DATE WRITTEN      January 2012
		//       MODIFIED          na
		//       RE-ENGINEERED     na

		// PURPOSE OF THIS SUBROUTINE:
		// Checks the limits of the various curves used in EIR chiller and returns .FALSE. if the limits do not include
		// the standard test condition(s).

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::DisplayExtraWarnings;
		using CurveManager::GetCurveMinMaxValues;
		using CurveManager::GetCurveType;
		using CurveManager::GetCurveName;
		using DataPlant::TypeOf_Chiller_ElectricEIR;
		using DataPlant::TypeOf_Chiller_ElectricReformEIR;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// (function of leaving chilled water temperature and
		//  entering condenser fluid temperature)
		// (function of leaving chilled water temperature and
		//  entering condenser fluid temperature)

		// SUBROUTINE PARAMETER DEFINITIONS:

		int const AirCooled( 1 );
		int const WaterCooled( 2 );

		// Following parameters are taken from AHRI 551/591,2011 Table 3
		Real64 const HighEWTemp( 30.0 ); // Entering water temp in degrees C at full load capacity (85F)
		Real64 const LowEWTemp( 19.0 ); // Entering water temp in degrees C at minimum reduced capacity (65F)
		Real64 const OAHighEDBTemp( 35.0 ); // Outdoor air dry-bulb temp in degrees C at full load capacity (95F)
		Real64 const OAHighEWBTemp( 24.0 ); // Outdoor air wet-bulb temp in degrees C at full load capacity (75F)
		Real64 const LeavingWaterTemp( 6.67 ); // Evaporator leaving water temperature in degrees C [44 F]

		static std::string const RoutineName( "CheckCurveLimitsForIPLV: " ); // Include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		//  Minimum and Maximum independent variable limits from Total Cooling Capacity Function of Temperature Curve
		static Real64 CapacityLWTempMin( 0.0 ); // Capacity modifier Min value (leaving water temp), from the Curve:BiQuadratic object
		static Real64 CapacityLWTempMax( 0.0 ); // Capacity modifier Max value (leaving water temp), from the Curve:BiQuadratic object
		static Real64 CapacityEnteringCondTempMin( 0.0 ); // Capacity modifier Min value (entering cond temp),
		// from the Curve:BiQuadratic object
		static Real64 CapacityEnteringCondTempMax( 0.0 ); // Capacity modifier Max value (entering cond temp),
		// from the Curve:BiQuadratic object

		//  Minimum and Maximum independent variable limits from Energy Input Ratio (EIR) Function of Temperature Curve
		static Real64 EIRLWTempMin( 0.0 ); // EIR modifier Min value (leaving water temp), from the Curve:BiQuadratic object
		static Real64 EIRLWTempMax( 0.0 ); // EIR modifier Max value (leaving water temp), from the Curve:BiQuadratic object
		static Real64 EIREnteringCondTempMin( 0.0 ); // EIR modifier Min value (entering cond temp),
		// from the Curve:BiQuadratic object
		static Real64 EIREnteringCondTempMax( 0.0 ); // EIR modifier Max value (entering cond temp),
		// from the Curve:BiQuadratic object

		static Real64 HighCondenserEnteringTempLimit( 0.0 ); // High limit of entering condenser temperature
		static Real64 LowCondenserEnteringTempLimit( 0.0 ); // Low limit of entering condenser temperature

		static bool CapCurveIPLVLimitsExceeded( false ); // Logical for capacity curve temperature limits being exceeded (IPLV calcs)
		static bool EIRCurveIPLVLimitsExceeded( false ); // Logical for EIR temperature limits being exceeded (IPLV calcs)

		GetCurveMinMaxValues( CapFTempCurveIndex, CapacityLWTempMin, CapacityLWTempMax, CapacityEnteringCondTempMin, CapacityEnteringCondTempMax );
		GetCurveMinMaxValues( EIRFTempCurveIndex, EIRLWTempMin, EIRLWTempMax, EIREnteringCondTempMin, EIREnteringCondTempMax );

		if ( CondenserType == WaterCooled ) {
			HighCondenserEnteringTempLimit = HighEWTemp;
			LowCondenserEnteringTempLimit = LowEWTemp;
		} else if ( CondenserType == AirCooled ) {
			HighCondenserEnteringTempLimit = OAHighEDBTemp;
			LowCondenserEnteringTempLimit = OAHighEDBTemp;
		} else { // Evaporatively Cooled Condenser
			HighCondenserEnteringTempLimit = OAHighEWBTemp;
			LowCondenserEnteringTempLimit = OAHighEWBTemp;
		}

		// Checking the limits of capacity modifying curve for temperatures (IPLV high and low test conditions)
		if ( CapacityEnteringCondTempMax < HighCondenserEnteringTempLimit || CapacityEnteringCondTempMin > LowCondenserEnteringTempLimit || CapacityLWTempMax < LeavingWaterTemp || CapacityLWTempMin > LeavingWaterTemp ) {
			CapCurveIPLVLimitsExceeded = true;
		}
		// Checking the limits of EIR modifying curve for temperatures (IPLV high and low test conditions)
		if ( EIREnteringCondTempMax < HighCondenserEnteringTempLimit || EIREnteringCondTempMin > LowCondenserEnteringTempLimit || EIRLWTempMax < LeavingWaterTemp || EIRLWTempMin > LeavingWaterTemp ) {
			EIRCurveIPLVLimitsExceeded = true;
		}

		// For IPLV:
		if ( CapCurveIPLVLimitsExceeded || EIRCurveIPLVLimitsExceeded ) {
			if ( DisplayExtraWarnings ) {
				{ auto const SELECT_CASE_var( ChillerType );

				if ( SELECT_CASE_var == TypeOf_Chiller_ElectricEIR ) {

					ShowWarningError( "Chiller:Electric:EIR = " + ChillerName + ":  Integrated Part Load Value (IPLV) calculated is not at the AHRI test condition." );
				} else if ( SELECT_CASE_var == TypeOf_Chiller_ElectricReformEIR ) {

					ShowWarningError( "Chiller:Electric:ReformulatedEIR = " + ChillerName + ":  Integrated Part Load Value (IPLV) calculated is not at the AHRI test condition." );
				}}
				if ( CapCurveIPLVLimitsExceeded ) {
					ShowContinueError( " Check limits in Cooling Capacity Function of Temperature Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFTempCurveIndex ) );
				}
				if ( EIRCurveIPLVLimitsExceeded ) {
					ShowContinueError( " Check limits in EIR Function of Temperature Curve, Curve Type = " + GetCurveType( EIRFTempCurveIndex ) + ", Curve Name = " + GetCurveName( EIRFTempCurveIndex ) );
				}
			}
		}

	}

	void
	CalcDXCoilStandardRating(
		std::string const & DXCoilName, // Name of DX coil for which HSPF is calculated
		std::string const & DXCoilType, // Type of DX coil for which HSPF is calculated
		int const DXCoilType_Num, // Integer Type of DX coil - heating or cooling
		int const ns, // Number of compressor speeds
		Array1A< Real64 > const RatedTotalCapacity, // Reference capacity of DX coil [W]
		Array1A< Real64 > const RatedCOP, // Reference coefficient of performance [W/W]
		Array1A_int const CapFFlowCurveIndex, // Index for the capacity as a function of flow fraction modifier curve
		Array1A_int const CapFTempCurveIndex, // Index for the capacity as a function of temperature modifier curve
		Array1A_int const EIRFFlowCurveIndex, // Index for the EIR as a function of flow fraction modifier curve
		Array1A_int const EIRFTempCurveIndex, // Index for the EIR as a function of temperature modifier curve
		Array1A_int const PLFFPLRCurveIndex, // Index for the PLF vs part-load ratio curve
		Array1A< Real64 > const RatedAirVolFlowRate, // Reference air flow rate of DX coil [m3/s]
		Array1A< Real64 > const FanPowerPerEvapAirFlowRateFromInput, // Reference fan power per evap air flow rate [W/(m3/s)]
		Optional_int_const RegionNum, // Region number for calculating HSPF of single speed DX heating coil //Autodesk:OPTIONAL Used without PRESENT check
		Optional< Real64 const > MinOATCompressor, // Minimum OAT for heat pump compressor operation [C] //Autodesk:OPTIONAL Used without PRESENT check
		Optional< Real64 const > OATempCompressorOn, // The outdoor temperature when the compressor is automatically turned //Autodesk:OPTIONAL Used without PRESENT check
		Optional_bool_const OATempCompressorOnOffBlank, // Flag used to determine low temperature cut out factor //Autodesk:OPTIONAL Used without PRESENT check
		Optional_int_const DefrostControl, // defrost control; 1=timed, 2=on-demand //Autodesk:OPTIONAL Used without PRESENT check
		Optional_bool_const ASHRAE127StdRprt // true if user wishes to report ASHRAE 127 standard ratings
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, Chandan Sharma FSEC
		//       DATE WRITTEN   February 2010,
		//                      B. Nigusse, May 2010  Added EER and IEER Calculation
		//                      C. Sharma, March 2012  Added HSPF Calculation for single speed HP
		//                      B. Nigusse, August 2012 Added SEER Calculation for Multi-speed HP
		//                      B. Nigusse, November 2012 Added HSPF Calculation for Multi-speed HP

		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//     Calculates:
		//                 (1) Standard Rated (net) Cooling Capacity
		//                 (2) Seasonal Energy Efficiency Ratio (SEER)
		//                 (3) Energy Efficiency Ratio (EER),
		//                 (4) Integrated Energy Efficiency Ratio (IEER)
		//                 for Air-to-Air Direct Expansion Air Conditioner and Heat Pumps having a single-speed compressor,
		//                 fixed speed indoor supply air fan, and air-cooled condensers. Writes the result to EIO file.
		//                 (5) Heating Seasonal Performance Factor (HSPF) for Air-Source Direct Expansion Heat Pumps having
		//                  a single-speed compressor, fixed speed indoor supply air fan
		//                 (6) Standard Rated (net) Cooling Capacity; and
		//                 (7) Seasonal Energy Efficiency Ratio (SEER) for Air-to-Air Heat Pumps having multi-speed
		//                     compressor.
		//                 (8) Heating Seasonal Performance Factor (HSPF) for Air-to-Air Heat Pumps having multi-speed
		//                     compressor.
		// METHODOLOGY EMPLOYED:
		// (A) Methodology for calculating standard ratings for DX air conditioners
		//     (1) Obtains the rated condition parameters:
		//         Cooling capacity (User specified or Autosized Value)
		//         Rated Air volume flow rate through the DX Cooling Coil (User specified or autosized value)
		//     (2) Evaluates the total cooling coil capacity at AHRI test conditions 26.7C/19.4C/27.8C. Then net
		//         cooling capacity is determined from the total cooling capacity of the DX coil at the AHRI test
		//         conditions and accounting for the INDOOR supply air fan heat.
		//     (3) Calculates the electric power consumed by the DX Coil Unit (compressor + outdoor condenser fan).
		//         Evaluates the EIR capacity and flow fraction modifiers at 26.7C/19.4C/27.8C. The net electric
		//         power consumption is determined by adding the indoor fan electric power to the electric power
		//         consumption by the DX Coil Condenser Fan and Compressor at the AHRI test conditions.
		//     (4) The EER is evaluated from the total net cooling capacity and total electric power
		//         evaluated at the standard rated test conditions.  The IEER is a weighted value of the EER evaluated
		//         at four different capacities of 100%, 75%, 50% and 25%.  The reduced capacity EERs are evaluated
		//         at different outdoor coil entering air dry-bulb temperatures.
		// (B) Methodology for calculating standard ratings for DX air air source heat pumps
		//     (1) Obtains the rated condition parameters:
		//         heating capacity (User specified or Autosized Value), COP,  Rated Air volume flow rate through the
		//         DX Cooling Coil (User specified or autosized value) and Fan power per rated air flow rate
		//     (2) Evaluates the heating coil capacities for AHRI tests H1, H2 and H3 using the performance cuves and
		//         input values specified at (1) above. Then net heating capacity is determined from the total heating capacity
		//         of the DX coil at the AHRI test conditions and accounting for the INDOOR supply air fan heat.
		//     (3) Calculates the electric power consumed by the DX Coil Unit (compressor + outdoor condenser fan).
		//         The net electric power consumption is determined by adding the indoor fan electric power to the
		//         electric power consumption by the DX Coil Condenser Fan and Compressor at the AHRI test conditions.
		//     (4) High Temperature Heating Standard (Net) Rating Capacity and Low Temperature Heating Standard (Net)
		//         Rating Capacity capacity are determined using tests H1 adn H3 per ANSI/AHRI 210/240 2008.
		//     (5) The HSPF is evaluated from the total net heating capacity and total electric power
		//         evaluated at the standard rated test conditions. For user specified region number, the outdoor temperatures
		//         are Binned (grouped) and fractioanl bin hours for each bin over the entire heating season are taken
		//         from AHRI 210/240. Then for each bin, building load, heat pump energy adn resistance space heating enegry are
		//         calculated. The sum of building load divided by sum of heat pump and resistance space heating over the
		//         entire heating season gives the HSPF. The detailed calculation algorithms of calculating HSPF
		//         are described in Engineering Reference.
		// (C) Methodology for calculating standard ratings for Multi-Speed Heat Pumps
		//     Net Total Cooling Capacity and SEER
		//     (1) Obtains the rated condition parameters:
		//         Cooling capacity (User specified or Autosized Value)
		//         Rated Air volume flow rate through the DX Cooling Coil (User specified or autosized value)
		//     (2) Evaluates the total cooling coil capacity at AHRI A2 test conditions 26.7C/19.4C/35.0C. Then net
		//         cooling capacity is determined from the total cooling capacity of the DX coil at the AHRI A2 test
		//         conditions and accounting for the INDOOR supply air fan effect.  The net total cooling capacity
		//         is reported at the high (maximum) speed only.
		//     (3) Calculates the electric power consumed by the DX Coil Unit (compressor + outdoor condenser fan).
		//         Evaluates the EIR capacity and flow fraction modifiers at A2, B2, B1, and F1 test coditions per
		//         AHRI/ANSI Std. 210/240 test procedure for multi-speed compressor.  For any inter-
		//         mediate operating conditions (speed), the successive lower and the higher speed performnace are
		//         weighed per the standard.  Electric Power consumption is determined by adding the indoor fan
		//         electric power to the electric power consumption by the outdoor DX Coil Fan and Compressor Power
		//         at the AHRI test conditions.  The net total cooling capacity is also corrected for the fan heat
		//         effect for SEER calculation.
		//     Net Heatingg Capacity and HSPF
		//     (4) Obtains the rated condition parameters:
		//         Heating capacity (User specified or Autosized Value)
		//         Rated Air volume flow rate through the DX Heating Coil (User specified or autosized value)
		//     (5) Evaluates the heating coil capacity at AHRI H12 test conditions 21.1C/15.6C/8.33C. Then net
		//         heating capacity is determined from the total heating capacity of the DX coil at the AHRI H12
		//         test conditions and accounting for the supply supply air fan effect.  The net heating capacity
		//         is reported at the high (maximum) speed only.
		//     (6) Calculates the electric power consumed by the DX Coil Unit (compressor + outdoor condenser fan).
		//         Evaluates the EIR capacity and flow fraction modifiers per AHRI/ANSI Std. 210/240 test procedures
		//         for two speed compressor (H01, H11, H21, H31, H12, H22, and H32 ). This procedure was modified
		//         for multispeed heat pumps. For any inter-mediate operating conditions (speed), the successive
		//         lower and the higher speed performnace are weighed per the standard.
		//         Electric Power consumption is determined by adding the supply fan electric power to the electric
		//         power consumption by the outdoor DX Coil Fan and Compressor Power at the AHRI test conditions.
		//         The net heating capacity is also corrected for the fan heat effect for SEER calculation.
		// REFERENCES:
		// (1) ANSI/AHRI Standard 210/240-2008:  Standard for Performance Rating of Unitary Air-Conditioning and
		//                                       Air-Source Heat Pumps. Arlington, VA:  Air-Conditioning, Heating
		//                                       , and Refrigeration Institute.
		// (2) ANSI/AHRI Standard 340/360-2007:  Standard for Performance Rating of Commercial and Industrial
		//                                       Unitary Air-Conditioning and Heat Pump Equipment.  Arlington,
		//                                       VA:  Air-Conditioning, Heating, and Refrigeration Institute.

		// USE STATEMENTS:

		// Using/Aliasing
		using DataHVACGlobals::CoilDX_CoolingSingleSpeed;
		using DataHVACGlobals::CoilDX_HeatingEmpirical;
		using DataHVACGlobals::CoilDX_MultiSpeedCooling;
		using DataHVACGlobals::CoilDX_MultiSpeedHeating;
		using CurveManager::CurveValue;
		using CurveManager::GetCurveMinMaxValues;
		using CurveManager::GetCurveType;
		using General::RoundSigDigits;

		// Argument array dimensioning
		RatedTotalCapacity.dim( ns );
		RatedCOP.dim( ns );
		CapFFlowCurveIndex.dim( ns );
		CapFTempCurveIndex.dim( ns );
		EIRFFlowCurveIndex.dim( ns );
		EIRFTempCurveIndex.dim( ns );
		PLFFPLRCurveIndex.dim( ns );
		RatedAirVolFlowRate.dim( ns );
		FanPowerPerEvapAirFlowRateFromInput.dim( ns );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// back on, if applicable, following automatic shut off. This field is
		// used only for HSPF calculation. [C]

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D< Real64 > FanPowerPerEvapAirFlowRate( ns ); // Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]

		// Intermediate values calculated from the inputs in the idf file
		int spnum; // compressor speed number

		// Calculated and reported to the EIO file
		static Real64 SEER( 0.0 ); // Seasonal Energy Efficiency Ratio in SI [W/W]
		static Real64 EER( 0.0 ); // Energy Efficiency Ratio in SI [W/W]
		static Real64 IEER( 0.0 ); // Integerated Energy Efficiency Ratio in SI [W/W]
		static Real64 HSPF( 0.0 ); // Heating Seasonal Performance Factor in SI [W/W]
		static Real64 NetHeatingCapRatedHighTemp( 0.0 ); // Net Rated heating capacity at high temp [W]
		static Real64 NetHeatingCapRatedLowTemp( 0.0 ); // Net Rated heating capacity at low temp [W]
		Array1D< Real64 > NetCoolingCapRated( ns ); // Net Cooling Coil capacity at Rated conditions, accounting for supply fan heat [W]
		Array1D< Real64 > NetTotCoolingCapRated( 16 ); // net total cooling capacity of DX Coils for the sixteen ASHRAE Std 127 Test conditions
		Array1D< Real64 > TotElectricPowerRated( 16 ); // total electric power of DX Coils for the sixteen ASHRAE Std 127 Test conditions

		NetCoolingCapRated = 0.0;

		{ auto const SELECT_CASE_var( DXCoilType_Num );

		if ( SELECT_CASE_var == CoilDX_CoolingSingleSpeed ) { // Coil:Cooling:DX:SingleSpeed

			CheckCurveLimitsForStandardRatings( DXCoilName, DXCoilType, DXCoilType_Num, CapFTempCurveIndex( 1 ), CapFFlowCurveIndex( 1 ), EIRFTempCurveIndex( 1 ), EIRFFlowCurveIndex( 1 ), PLFFPLRCurveIndex( 1 ) );

			// Calculated Net Cooling Capacity, SEER, EER, and IEER of single speed DX cooling coils
			SingelSpeedDXCoolingCoilStandardRatings( DXCoilName, DXCoilType, CapFTempCurveIndex( 1 ), CapFFlowCurveIndex( 1 ), EIRFTempCurveIndex( 1 ), EIRFFlowCurveIndex( 1 ), PLFFPLRCurveIndex( 1 ), RatedTotalCapacity( 1 ), RatedCOP( 1 ), RatedAirVolFlowRate( 1 ), FanPowerPerEvapAirFlowRateFromInput( 1 ), NetCoolingCapRated( 1 ), SEER, EER, IEER );

			// Writes the net rated cooling capacity, SEER, EER and IEER values to the EIO file and standard tabular output tables
			ReportDXCoilRating( DXCoilType, DXCoilName, DXCoilType_Num, NetCoolingCapRated( 1 ), SEER * ConvFromSIToIP, EER, EER * ConvFromSIToIP, IEER * ConvFromSIToIP, NetHeatingCapRatedHighTemp, NetHeatingCapRatedLowTemp, HSPF * ConvFromSIToIP, RegionNum );

			if ( ASHRAE127StdRprt ) {
				DXCoolingCoilDataCenterStandardRatings( DXCoilName, DXCoilType, CapFTempCurveIndex( 1 ), CapFFlowCurveIndex( 1 ), EIRFTempCurveIndex( 1 ), EIRFFlowCurveIndex( 1 ), PLFFPLRCurveIndex( 1 ), RatedTotalCapacity( 1 ), RatedCOP( 1 ), RatedAirVolFlowRate( 1 ), FanPowerPerEvapAirFlowRateFromInput( 1 ), NetTotCoolingCapRated, TotElectricPowerRated );
				ReportDXCoolCoilDataCenterApplication( DXCoilType, DXCoilName, DXCoilType_Num, NetTotCoolingCapRated, TotElectricPowerRated );
			}
		} else if ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) { // Coil:Heating:DX:SingleSpeed

			CheckCurveLimitsForStandardRatings( DXCoilName, DXCoilType, DXCoilType_Num, CapFTempCurveIndex( 1 ), CapFFlowCurveIndex( 1 ), EIRFTempCurveIndex( 1 ), EIRFFlowCurveIndex( 1 ), PLFFPLRCurveIndex( 1 ) );
			// Calculate the standard ratings for single speed DX heating coil
			SingleSpeedDXHeatingCoilStandardRatings( RatedTotalCapacity( 1 ), RatedCOP( 1 ), CapFFlowCurveIndex( 1 ), CapFTempCurveIndex( 1 ), EIRFFlowCurveIndex( 1 ), EIRFTempCurveIndex( 1 ), RatedAirVolFlowRate( 1 ), FanPowerPerEvapAirFlowRateFromInput( 1 ), NetHeatingCapRatedHighTemp, NetHeatingCapRatedLowTemp, HSPF, RegionNum, MinOATCompressor, OATempCompressorOn, OATempCompressorOnOffBlank, DefrostControl );

			// Writes the HSPF value to the EIO file and standard tabular output tables
			ReportDXCoilRating( DXCoilType, DXCoilName, DXCoilType_Num, NetCoolingCapRated( 1 ), SEER * ConvFromSIToIP, EER, EER * ConvFromSIToIP, IEER * ConvFromSIToIP, NetHeatingCapRatedHighTemp, NetHeatingCapRatedLowTemp, HSPF * ConvFromSIToIP, RegionNum );

		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) { // Coil:Cooling:DX:MultiSpeed,

			for ( spnum = 1; spnum <= ns; ++spnum ) {
				CheckCurveLimitsForStandardRatings( DXCoilName, DXCoilType, DXCoilType_Num, CapFTempCurveIndex( spnum ), CapFFlowCurveIndex( spnum ), EIRFTempCurveIndex( spnum ), EIRFFlowCurveIndex( spnum ), PLFFPLRCurveIndex( spnum ) );
			}
			// Calculate the standard ratings for multispeed DX cooling coil
			MultiSpeedDXCoolingCoilStandardRatings( DXCoilName, DXCoilType, CapFTempCurveIndex, CapFFlowCurveIndex, EIRFTempCurveIndex, EIRFFlowCurveIndex, PLFFPLRCurveIndex, RatedTotalCapacity, RatedCOP, RatedAirVolFlowRate, FanPowerPerEvapAirFlowRateFromInput, ns, NetCoolingCapRated( ns ), SEER );
			// Writes the SEER value to the EIO file and standard tabular output tables
			ReportDXCoilRating( DXCoilType, DXCoilName, DXCoilType_Num, NetCoolingCapRated( ns ), SEER * ConvFromSIToIP, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0 );

		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) { // Coil:Heating:DX:MultiSpeed

			for ( spnum = 1; spnum <= ns; ++spnum ) {
				CheckCurveLimitsForStandardRatings( DXCoilName, DXCoilType, DXCoilType_Num, CapFTempCurveIndex( spnum ), CapFFlowCurveIndex( spnum ), EIRFTempCurveIndex( spnum ), EIRFFlowCurveIndex( spnum ), PLFFPLRCurveIndex( spnum ) );
			}
			// Calculate Net heatig capacity and HSPF of multispeed DX heating coils
			MultiSpeedDXHeatingCoilStandardRatings( DXCoilName, DXCoilType, CapFTempCurveIndex, CapFFlowCurveIndex, EIRFTempCurveIndex, EIRFFlowCurveIndex, PLFFPLRCurveIndex, RatedTotalCapacity, RatedCOP, RatedAirVolFlowRate, FanPowerPerEvapAirFlowRateFromInput, ns, NetHeatingCapRatedHighTemp, NetHeatingCapRatedLowTemp, HSPF, RegionNum, MinOATCompressor, OATempCompressorOn, OATempCompressorOnOffBlank, DefrostControl );
			// Writes the HSPF value to the EIO file and standard tabular output tables
			ReportDXCoilRating( DXCoilType, DXCoilName, DXCoilType_Num, NetCoolingCapRated( ns ), SEER * ConvFromSIToIP, EER, EER * ConvFromSIToIP, IEER * ConvFromSIToIP, NetHeatingCapRatedHighTemp, NetHeatingCapRatedLowTemp, HSPF * ConvFromSIToIP, RegionNum );

		} else {
			//... other DX Coil types will follow here

		}}

	}

	void
	SingleSpeedDXHeatingCoilStandardRatings(
		Real64 const RatedTotalCapacity, // Reference capacity of DX coil [W]
		Real64 const RatedCOP, // Reference coefficient of performance [W/W]
		int const CapFFlowCurveIndex, // Index for the capacity as a function of flow fraction modifier curve
		int const CapFTempCurveIndex, // Index for the capacity as a function of temperature modifier curve
		int const EIRFFlowCurveIndex, // Index for the EIR as a function of flow fraction modifier curve
		int const EIRFTempCurveIndex, // Index for the EIR as a function of temperature modifier curve
		Real64 const RatedAirVolFlowRate, // Rated air volume flow rate [m3/s]
		Real64 const FanPowerPerEvapAirFlowRateFromInput, // Fan power per air volume flow rate [W/(m3/s)]
		Real64 & NetHeatingCapRated, // Net Heating Coil capacity at Rated conditions,
		Real64 & NetHeatingCapH3Test, // Net Heating Coil capacity at H3 test conditions
		Real64 & HSPF, // seasonale energy efficiency ratio of multi speed DX cooling coil
		Optional_int_const RegionNum, // Region number for calculating HSPF of single speed DX heating coil
		Optional< Real64 const > MinOATCompressor, // Minimum OAT for heat pump compressor operation [C]
		Optional< Real64 const > OATempCompressorOn, // The outdoor temperature when the compressor is automatically turned
		Optional_bool_const OATempCompressorOnOffBlank, // Flag used to determine low temperature cut out factor
		Optional_int_const DefrostControl // defrost control; 1=timed, 2=on-demand

	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chandan Sharma
		//       DATE WRITTEN   February 2012
		//       MODIFIED       B Nigusse, December 2012
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// na

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using CurveManager::GetCurveMinMaxValues;
		using CurveManager::GetCurveType;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// back on, if applicable, following automatic shut off. This field is
		// used only for HSPF calculation. [C]
		// accounting for supply fan heat [W]
		// accounting for supply fan heat [W]

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na
		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 TotalHeatingCapRated( 0.0 ); // Heating Coil capacity at Rated conditions, without accounting supply fan heat [W]
		static Real64 EIRRated( 0.0 ); // EIR at Rated conditions [-]
		static Real64 TotCapTempModFacRated( 0.0 ); // Total capacity as a function of temerature modifier at rated conditions [-]
		static Real64 EIRTempModFacRated( 0.0 ); // EIR as a function of temerature modifier at rated conditions [-]
		static Real64 TotalHeatingCapH2Test( 0.0 ); // Heating Coil capacity at H2 test conditions, without accounting supply fan heat [W]
		static Real64 TotalHeatingCapH3Test( 0.0 ); // Heating Coil capacity at H3 test conditions, without accounting supply fan heat [W]
		static Real64 CapTempModFacH2Test( 0.0 ); // Total capacity as a function of temerature modifier at H2 test conditions [-]
		static Real64 EIRTempModFacH2Test( 0.0 ); // EIR as a function of temerature modifier at H2 test conditions [-]
		static Real64 EIRH2Test( 0.0 ); // EIR at H2 test conditions [-]
		static Real64 CapTempModFacH3Test( 0.0 ); // Total capacity as a function of temerature modifier at H3 test conditions [-]
		static Real64 EIRTempModFacH3Test( 0.0 ); // EIR as a function of temerature modifier at H3 test conditions [-]
		static Real64 EIRH3Test( 0.0 ); // EIR at H3 test conditions [-]
		static Real64 TotCapFlowModFac( 0.0 ); // Total capacity modifier (function of actual supply air flow vs rated flow)
		static Real64 EIRFlowModFac( 0.0 ); // EIR modifier (function of actual supply air flow vs rated flow)
		static Real64 FanPowerPerEvapAirFlowRate( 0.0 ); // Fan power per air volume flow rate [W/(m3/s)]

		Real64 ElecPowerRated; // Total system power at Rated conditions accounting for supply fan heat [W]
		Real64 ElecPowerH2Test; // Total system power at H2 test conditions accounting for supply fan heat [W]
		Real64 ElecPowerH3Test; // Total system power at H3 test conditions accounting for supply fan heat [W]
		Real64 NetHeatingCapH2Test; // Net Heating Coil capacity at H2 test conditions accounting for supply fan heat [W]

		Real64 PartLoadFactor;
		static Real64 LowTempCutOutFactor( 0.0 ); // Factor which corresponds to compressor operation depending on outdoor temperature
		static Real64 OATempCompressorOff( 0.0 ); // Minimum outdoor air temperature to turn the commpressor off, [C]

		static Real64 FractionalBinHours( 0.0 ); // Fractional bin hours for the heating season  [-]
		static Real64 BuildingLoad( 0.0 ); // Building space conditioning load corresponding to an outdoor bin temperature [W]
		static Real64 HeatingModeLoadFactor( 0.0 ); // Heating mode load factor corresponding to an outdoor bin temperature  [-]
		static Real64 NetHeatingCapReduced( 0.0 ); // Net Heating Coil capacity corresponding to an outdoor bin temperature [W]
		static Real64 TotalBuildingLoad( 0.0 ); // Sum of building load over the entire heating season [W]
		static Real64 TotalElectricalEnergy( 0.0 ); // Sum of electrical energy consumed by the heatpump over the heating season [W]
		static Real64 DemandDeforstCredit( 1.0 ); // A factor to adjust HSPF if coil has demand defrost control  [-]
		static Real64 CheckCOP( 0.0 ); // Checking COP at an outdoor bin temperature against unity [-]
		static Real64 DesignHeatingRequirement( 0.0 ); // The amount of heating required to maintain a given indoor temperature
		// at a particular outdoor design temperature.  [W]
		static Real64 DesignHeatingRequirementMin( 0.0 ); // minimum design heating requirement [W]
		static Real64 ElectricalPowerConsumption( 0.0 ); // Electrical power corresponding to an outdoor bin temperature [W]
		static Real64 HeatPumpElectricalEnergy( 0.0 ); // Heatpump electrical energy corresponding to an outdoor bin temperature [W]
		static Real64 TotalHeatPumpElectricalEnergy( 0.0 ); // Sum of Heatpump electrical energy over the entire heating season [W]
		static Real64 ResistiveSpaceHeatingElectricalEnergy( 0.0 ); // resistance heating electrical energy corresponding to an
		// outdoor bin temperature [W]
		static Real64 TotalResistiveSpaceHeatingElectricalEnergy( 0.0 ); // Sum of resistance heating electrical energy over the
		// entire heating season [W]

		int BinNum; // bin number counter
		int StandardDHRNum; // Integer counter for standardized DHRs

		TotalBuildingLoad = 0.0;
		TotalHeatPumpElectricalEnergy = 0.0;
		TotalResistiveSpaceHeatingElectricalEnergy = 0.0;

		// Calculate the supply air fan electric power consumption.  The electric power consumption is estimated
		// using either user supplied or AHRI default value for fan power per air volume flow rate
		if ( FanPowerPerEvapAirFlowRateFromInput <= 0.0 ) {
			FanPowerPerEvapAirFlowRate = DefaultFanPowerPerEvapAirFlowRate;
		} else {
			FanPowerPerEvapAirFlowRate = FanPowerPerEvapAirFlowRateFromInput;
		}

		TotCapFlowModFac = CurveValue( CapFFlowCurveIndex, AirMassFlowRatioRated );
		EIRFlowModFac = CurveValue( EIRFFlowCurveIndex, AirMassFlowRatioRated );

		{ auto const SELECT_CASE_var( GetCurveType( CapFTempCurveIndex ) );

		if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
			TotCapTempModFacRated = CurveValue( CapFTempCurveIndex, HeatingOutdoorCoilInletAirDBTempRated );

			CapTempModFacH2Test = CurveValue( CapFTempCurveIndex, HeatingOutdoorCoilInletAirDBTempH2Test );

			CapTempModFacH3Test = CurveValue( CapFTempCurveIndex, HeatingOutdoorCoilInletAirDBTempH3Test );
		} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
			TotCapTempModFacRated = CurveValue( CapFTempCurveIndex, HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempRated );

			CapTempModFacH2Test = CurveValue( CapFTempCurveIndex, HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH2Test );

			CapTempModFacH3Test = CurveValue( CapFTempCurveIndex, HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH3Test );

		}}

		{ auto const SELECT_CASE_var( GetCurveType( EIRFTempCurveIndex ) );

		if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
			EIRTempModFacRated = CurveValue( EIRFTempCurveIndex, HeatingOutdoorCoilInletAirDBTempRated );

			EIRTempModFacH2Test = CurveValue( EIRFTempCurveIndex, HeatingOutdoorCoilInletAirDBTempH2Test );

			EIRTempModFacH3Test = CurveValue( EIRFTempCurveIndex, HeatingOutdoorCoilInletAirDBTempH3Test );
		} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
			EIRTempModFacRated = CurveValue( EIRFTempCurveIndex, HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempRated );

			EIRTempModFacH2Test = CurveValue( EIRFTempCurveIndex, HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH2Test );

			EIRTempModFacH3Test = CurveValue( EIRFTempCurveIndex, HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH3Test );
		}}

		TotalHeatingCapRated = RatedTotalCapacity * TotCapTempModFacRated * TotCapFlowModFac;
		NetHeatingCapRated = TotalHeatingCapRated + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;

		TotalHeatingCapH2Test = RatedTotalCapacity * CapTempModFacH2Test * TotCapFlowModFac;
		NetHeatingCapH2Test = TotalHeatingCapH2Test + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;

		TotalHeatingCapH3Test = RatedTotalCapacity * CapTempModFacH3Test * TotCapFlowModFac;
		NetHeatingCapH3Test = TotalHeatingCapH3Test + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;

		if ( RatedCOP > 0.0 ) { // RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"

			EIRRated = EIRTempModFacRated * EIRFlowModFac / RatedCOP;
			EIRH2Test = EIRTempModFacH2Test * EIRFlowModFac / RatedCOP;
			EIRH3Test = EIRTempModFacH3Test * EIRFlowModFac / RatedCOP;

		}

		ElecPowerRated = EIRRated * TotalHeatingCapRated + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
		ElecPowerH2Test = EIRH2Test * TotalHeatingCapH2Test + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
		ElecPowerH3Test = EIRH3Test * TotalHeatingCapH3Test + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;

		if ( RegionNum == 5 ) {
			DesignHeatingRequirementMin = NetHeatingCapRated;
		} else {
			DesignHeatingRequirementMin = NetHeatingCapRated * 1.8 * ( 18.33 - OutdoorDesignTemperature( RegionNum ) ) / 60.0;
		}

		for ( StandardDHRNum = 1; StandardDHRNum <= TotalNumOfStandardDHRs - 1; ++StandardDHRNum ) {
			if ( StandardDesignHeatingRequirement( StandardDHRNum ) <= DesignHeatingRequirementMin && StandardDesignHeatingRequirement( StandardDHRNum + 1 ) >= DesignHeatingRequirementMin ) {
				if ( ( DesignHeatingRequirementMin - StandardDesignHeatingRequirement( StandardDHRNum ) ) > ( StandardDesignHeatingRequirement( StandardDHRNum + 1 ) - DesignHeatingRequirementMin ) ) {
					DesignHeatingRequirementMin = StandardDesignHeatingRequirement( StandardDHRNum + 1 );
				} else {
					DesignHeatingRequirementMin = StandardDesignHeatingRequirement( StandardDHRNum );
				}
			}
		}
		if ( StandardDesignHeatingRequirement( 1 ) >= DesignHeatingRequirementMin ) {
			DesignHeatingRequirement = StandardDesignHeatingRequirement( 1 );
		} else if ( StandardDesignHeatingRequirement( TotalNumOfStandardDHRs ) <= DesignHeatingRequirementMin ) {
			DesignHeatingRequirement = StandardDesignHeatingRequirement( TotalNumOfStandardDHRs );
		} else {
			DesignHeatingRequirement = DesignHeatingRequirementMin;
		}

		for ( BinNum = 1; BinNum <= TotalNumOfTemperatureBins( RegionNum ); ++BinNum ) {

			if ( RegionNum == 1 ) {
				FractionalBinHours = RegionOneFracBinHoursAtOutdoorBinTemp( BinNum );
			} else if ( RegionNum == 2 ) {
				FractionalBinHours = RegionTwoFracBinHoursAtOutdoorBinTemp( BinNum );
			} else if ( RegionNum == 3 ) {
				FractionalBinHours = RegionThreeFracBinHoursAtOutdoorBinTemp( BinNum );
			} else if ( RegionNum == 4 ) {
				FractionalBinHours = RegionFourFracBinHoursAtOutdoorBinTemp( BinNum );
			} else if ( RegionNum == 5 ) {
				FractionalBinHours = RegionFiveFracBinHoursAtOutdoorBinTemp( BinNum );
			} else if ( RegionNum == 6 ) {
				FractionalBinHours = RegionSixFracBinHoursAtOutdoorBinTemp( BinNum );
			}

			BuildingLoad = ( 18.33 - OutdoorBinTemperature( BinNum ) ) / ( 18.33 - OutdoorDesignTemperature( RegionNum ) ) * CorrectionFactor * DesignHeatingRequirement;

			if ( ( OutdoorBinTemperature( BinNum ) <= -8.33 ) || ( OutdoorBinTemperature( BinNum ) >= 7.22 ) ) {
				NetHeatingCapReduced = NetHeatingCapH3Test + ( NetHeatingCapRated - NetHeatingCapH3Test ) * ( OutdoorBinTemperature( BinNum ) + 8.33 ) / ( 16.67 );
				ElectricalPowerConsumption = ElecPowerH3Test + ( ElecPowerRated - ElecPowerH3Test ) * ( OutdoorBinTemperature( BinNum ) + 8.33 ) / ( 16.67 );
			} else {
				NetHeatingCapReduced = NetHeatingCapH3Test + ( NetHeatingCapH2Test - NetHeatingCapH3Test ) * ( OutdoorBinTemperature( BinNum ) + 8.33 ) / ( 10.0 );
				ElectricalPowerConsumption = ElecPowerH3Test + ( ElecPowerH2Test - ElecPowerH3Test ) * ( OutdoorBinTemperature( BinNum ) + 8.33 ) / ( 10.0 );
			}

			if ( NetHeatingCapReduced != 0.0 ) {
				HeatingModeLoadFactor = BuildingLoad / NetHeatingCapReduced;
			}

			if ( HeatingModeLoadFactor > 1.0 ) {
				HeatingModeLoadFactor = 1.0;
			}

			PartLoadFactor = 1 - CyclicDegradationCoeff * ( 1 - HeatingModeLoadFactor );

			if ( ElectricalPowerConsumption != 0.0 ) {
				CheckCOP = NetHeatingCapReduced / ElectricalPowerConsumption;
			}

			OATempCompressorOff = MinOATCompressor;

			if ( CheckCOP < 1.0 ) {
				LowTempCutOutFactor = 0.0;
			} else {
				if ( ! OATempCompressorOnOffBlank ) {
					if ( OutdoorBinTemperature( BinNum ) <= OATempCompressorOff ) {
						LowTempCutOutFactor = 0.0;
					} else if ( OutdoorBinTemperature( BinNum ) > OATempCompressorOff && OutdoorBinTemperature( BinNum ) <= OATempCompressorOn ) {
						LowTempCutOutFactor = 0.5;
					} else {
						LowTempCutOutFactor = 1.0;
					}
				} else {
					LowTempCutOutFactor = 1.0;
				}
			}

			if ( PartLoadFactor != 0.0 ) {
				HeatPumpElectricalEnergy = ( HeatingModeLoadFactor * ElectricalPowerConsumption * LowTempCutOutFactor ) * FractionalBinHours / PartLoadFactor;
			}

			ResistiveSpaceHeatingElectricalEnergy = ( BuildingLoad - HeatingModeLoadFactor * NetHeatingCapReduced * LowTempCutOutFactor ) * FractionalBinHours;

			TotalBuildingLoad += ( BuildingLoad * FractionalBinHours );

			TotalHeatPumpElectricalEnergy += HeatPumpElectricalEnergy;

			TotalResistiveSpaceHeatingElectricalEnergy += ResistiveSpaceHeatingElectricalEnergy;
		}

		TotalElectricalEnergy = TotalHeatPumpElectricalEnergy + TotalResistiveSpaceHeatingElectricalEnergy;

		if ( DefrostControl == Timed ) {
			DemandDeforstCredit = 1.0; // Timed defrost control
		} else {
			DemandDeforstCredit = 1.03; // Demand defrost control
		}

		if ( TotalElectricalEnergy != 0.0 ) {
			HSPF = TotalBuildingLoad * DemandDeforstCredit / TotalElectricalEnergy;
		}

	}

	void
	SingelSpeedDXCoolingCoilStandardRatings(
		std::string const & DXCoilName, // Name of DX coil for which HSPF is calculated
		std::string const & DXCoilType, // Type of DX coil - heating or cooling
		int const CapFTempCurveIndex, // Index for the capacity as a function of temperature modifier curve
		int const CapFFlowCurveIndex, // Index for the capacity as a function of flow fraction modifier curve
		int const EIRFTempCurveIndex, // Index for the EIR as a function of temperature modifier curve
		int const EIRFFlowCurveIndex, // Index for the EIR as a function of flow fraction modifier curve
		int const PLFFPLRCurveIndex, // Index for the EIR vs part-load ratio curve
		Real64 const RatedTotalCapacity, // Rated gross total cooling capacity
		Real64 const RatedCOP, // Rated gross COP
		Real64 const RatedAirVolFlowRate, // air flow rate through the coil at rated condition
		Real64 const FanPowerPerEvapAirFlowRateFromInput, // Fan power per air volume flow rate through the evaporator coil
		Real64 & NetCoolingCapRated, // net cooling capacity of single speed DX cooling coil
		Real64 & SEER, // seasonale energy efficiency ratio of single speed DX cooling coil
		Real64 & EER, // energy efficiency ratio of single speed DX cooling coil
		Real64 & IEER // Integareted energy efficiency ratio of single speed DX cooling coil
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Nigusse, FSEC
		//       DATE WRITTEN   December 2012
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the standard ratings net cooling capacity and, SEER, EER and IEER values for single speed
		// DX cooling coils at the AHRI standard test condition(s).

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const NumOfReducedCap( 4 ); // Number of reduced capacity test conditions (100%,75%,50%,and 25%)

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 TotCapFlowModFac( 0.0 ); // Total capacity modifier f(actual flow vs rated flow) for each speed [-]
		static Real64 EIRFlowModFac( 0.0 ); // EIR modifier f(actual supply air flow vs rated flow) for each speed [-]
		static Real64 TotCapTempModFac( 0.0 ); // Total capacity modifier (function of entering wetbulb, outside drybulb) [-]
		static Real64 EIRTempModFac( 0.0 ); // EIR modifier (function of entering wetbulb, outside drybulb) [-]
		static Real64 TotCoolingCapAHRI( 0.0 ); // Total Cooling Coil capacity (gross) at AHRI test conditions [W]
		static Real64 NetCoolingCapAHRI( 0.0 ); // Net Cooling Coil capacity at AHRI TestB conditions, accounting for fan heat [W]
		static Real64 TotalElecPower( 0.0 ); // Net power consumption (Cond Fan+Compressor+Indoor Fan) at AHRI test conditions [W]
		static Real64 TotalElecPowerRated( 0.0 ); // Net power consumption (Cond Fan+Compressor+Indoor Fan) at Rated test conditions [W]
		static Real64 EIR( 0.0 ); // Energy Efficiency Ratio at AHRI test conditions for SEER [-]
		static Real64 PartLoadFactor( 0.0 ); // Part load factor, accounts for thermal lag at compressor startup [-]
		static Real64 EERReduced( 0.0 ); // EER at reduced capacity test conditions (100%, 75%, 50%, and 25%)
		static Real64 ElecPowerReducedCap( 0.0 ); // Net power consumption (Cond Fan+Compressor) at reduced test condition [W]
		static Real64 NetCoolingCapReduced( 0.0 ); // Net Cooling Coil capacity at reduced conditions, accounting for supply fan heat [W]
		static Real64 LoadFactor( 0.0 ); // Fractional "on" time for last stage at the desired reduced capacity, (dimensionless)
		static Real64 DegradationCoeff( 0.0 ); // Degradation coeficient, (dimenssionless)
		Real64 FanPowerPerEvapAirFlowRate; // Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]
		Real64 OutdoorUnitInletAirDryBulbTempReduced; // Outdoor unit entering air dry-bulb temperature at reduced capacity [C]
		int RedCapNum; // Integer counter for reduced capacity

		if ( FanPowerPerEvapAirFlowRateFromInput <= 0.0 ) {
			FanPowerPerEvapAirFlowRate = DefaultFanPowerPerEvapAirFlowRate;
		} else {
			FanPowerPerEvapAirFlowRate = FanPowerPerEvapAirFlowRateFromInput;
		}
		if ( RatedTotalCapacity > 0.0 ) {

			// Standard Rating Cooling (net) Capacity calculations:
			TotCapFlowModFac = CurveValue( CapFFlowCurveIndex, AirMassFlowRatioRated );
			TotCapTempModFac = CurveValue( CapFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempRated );
			NetCoolingCapRated = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;

			// SEER calculations:
			TotCapTempModFac = CurveValue( CapFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTemp );
			TotCoolingCapAHRI = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac;
			EIRTempModFac = CurveValue( EIRFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTemp );
			EIRFlowModFac = CurveValue( EIRFFlowCurveIndex, AirMassFlowRatioRated );
			if ( RatedCOP > 0.0 ) { // RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"
				EIR = EIRTempModFac * EIRFlowModFac / RatedCOP;
			} else {
				EIR = 0.0;
			}
			// Calculate net cooling capacity
			NetCoolingCapAHRI = TotCoolingCapAHRI - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
			TotalElecPower = EIR * TotCoolingCapAHRI + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
			// Calculate SEER value from the Energy Efficiency Ratio (EER) at the AHRI test conditions and the part load factor.
			// First evaluate the Part Load Factor curve at PLR = 0.5 (AHRI Standard 210/240)
			PartLoadFactor = CurveValue( PLFFPLRCurveIndex, PLRforSEER );
			if ( TotalElecPower > 0.0 ) {
				SEER = ( NetCoolingCapAHRI / TotalElecPower ) * PartLoadFactor;
			} else {
				SEER = 0.0;
			}

			// EER calculations:
			// Calculate the net cooling capacity at the rated conditions (19.44C WB and 35.0C DB )
			TotCapTempModFac = CurveValue( CapFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempRated );
			NetCoolingCapRated = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
			// Calculate Energy Efficiency Ratio (EER) at (19.44C WB and 35.0C DB ), ANSI/AHRI Std. 340/360
			EIRTempModFac = CurveValue( EIRFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempRated );
			if ( RatedCOP > 0.0 ) {
				// RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"
				EIR = EIRTempModFac * EIRFlowModFac / RatedCOP;
			} else {
				EIR = 0.0;
			}
			TotalElecPowerRated = EIR * ( RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac ) + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
			if ( TotalElecPowerRated > 0.0 ) {
				EER = NetCoolingCapRated / TotalElecPowerRated;
			} else {
				EER = 0.0;
			}

			// IEER calculations:
			IEER = 0.0;
			// Calculate the net cooling capacity at the rated conditions (19.44C WB and 35.0C DB )
			TotCapTempModFac = CurveValue( CapFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempRated );
			NetCoolingCapRated = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
			for ( RedCapNum = 1; RedCapNum <= NumOfReducedCap; ++RedCapNum ) {
				// get the outdoor air dry bulb temperature for the reduced capacity test conditions
				if ( ReducedPLR( RedCapNum ) > 0.444 ) {
					OutdoorUnitInletAirDryBulbTempReduced = 5.0 + 30.0 * ReducedPLR( RedCapNum );
				} else {
					OutdoorUnitInletAirDryBulbTempReduced = OADBTempLowReducedCapacityTest;
				}
				TotCapTempModFac = CurveValue( CapFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempReduced );
				NetCoolingCapReduced = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
				EIRTempModFac = CurveValue( EIRFTempCurveIndex, CoolingCoilInletAirWetBulbTempRated, OutdoorUnitInletAirDryBulbTempReduced );
				if ( RatedCOP > 0.0 ) {
					EIR = EIRTempModFac * EIRFlowModFac / RatedCOP;
				} else {
					EIR = 0.0;
				}
				if ( NetCoolingCapReduced > 0.0 ) {
					LoadFactor = ReducedPLR( RedCapNum ) * NetCoolingCapRated / NetCoolingCapReduced;
				} else {
					LoadFactor = 1.0;
				}
				DegradationCoeff = 1.130 - 0.130 * LoadFactor;
				ElecPowerReducedCap = DegradationCoeff * EIR * ( RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac );
				EERReduced = ( LoadFactor * NetCoolingCapReduced ) / ( LoadFactor * ElecPowerReducedCap + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate );
				IEER += IEERWeightingFactor( RedCapNum ) * EERReduced;
			}

		} else {
			ShowSevereError( "Standard Ratings: " + DXCoilType + ' ' + DXCoilName + " has zero rated total cooling capacity. Standard ratings cannot be calculated." );
		}

	}

	void
	DXCoolingCoilDataCenterStandardRatings(
		std::string const & DXCoilName, // Name of DX coil for which HSPF is calculated
		std::string const & DXCoilType, // Type of DX coil - heating or cooling
		int const CapFTempCurveIndex, // Index for the capacity as a function of temperature modifier curve
		int const CapFFlowCurveIndex, // Index for the capacity as a function of flow fraction modifier curve
		int const EIRFTempCurveIndex, // Index for the EIR as a function of temperature modifier curve
		int const EIRFFlowCurveIndex, // Index for the EIR as a function of flow fraction modifier curve
		int const EP_UNUSED( PLFFPLRCurveIndex ), // Index for the EIR vs part-load ratio curve
		Real64 const RatedTotalCapacity, // Rated gross total cooling capacity
		Real64 const RatedCOP, // Rated gross COP
		Real64 const RatedAirVolFlowRate, // air flow rate through the coil at rated condition
		Real64 const FanPowerPerEvapAirFlowRateFromInput, // Fan power per air volume flow rate through the evaporator coil
		Array1D< Real64 > & NetCoolingCapRated, // net cooling capacity of single speed DX cooling coil
		Array1D< Real64 > & TotElectricPowerRated // total electric power including supply fan
	) {

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Nigusse, FSEC
		//       DATE WRITTEN   October 2014
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the standard ratings net cooling capacity and total electric power
		// for room unitary air conditioners single speed DX cooling coils.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// ANSI/ASHRAE Standard 127-2012 - Method of Testing for Rating Computer and Data Processing
		//                                 Room Unitary Air Conditioners

		// Using/Aliasing
		using CurveManager::CurveValue;
		using Psychrometrics::PsyWFnTdpPb;
		using Psychrometrics::PsyTwbFnTdbWPb;
		using DataEnvironment::StdBaroPress;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 TotCapFlowModFac( 0.0 ); // Total capacity modifier f(actual flow vs rated flow) for each speed [-]
		static Real64 EIRFlowModFac( 0.0 ); // EIR modifier f(actual supply air flow vs rated flow) for each speed [-]
		static Real64 TotCapTempModFac( 0.0 ); // Total capacity modifier (function of entering wetbulb, outside drybulb) [-]
		static Real64 EIRTempModFac( 0.0 ); // EIR modifier (function of entering wetbulb, outside drybulb) [-]
		static Real64 TotCoolingCapRated( 0.0 ); // Total Cooling Coil capacity (gross) at one of the rated test conditions [W]
		static Real64 EIR( 0.0 ); // Energy Efficiency Ratio at AHRI test conditions for SEER [-]
		Real64 FanPowerPerEvapAirFlowRate; // Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]

		Real64 TWBIndoor; // indoor air dry bulb temperature
		Real64 TDBOutdoor; // outdor air dry bulb temperature
		int ClassNum; // class number (Class I, II, II, IV)
		int TestNum; // test number (Test A, B, C, D)
		int Num; // text number counter

		if ( FanPowerPerEvapAirFlowRateFromInput <= 0.0 ) {
			FanPowerPerEvapAirFlowRate = DefaultFanPowerPerEvapAirFlowRate;
		} else {
			FanPowerPerEvapAirFlowRate = FanPowerPerEvapAirFlowRateFromInput;
		}
		if ( RatedTotalCapacity > 0.0 ) {

			for ( ClassNum = 1; ClassNum <= 4; ++ClassNum ) {
				TWBIndoor = PsyTwbFnTdbWPb( IndoorDBTempClassI2IV( ClassNum ), PsyWFnTdpPb( IndoorTDPA2D, StdBaroPress ), StdBaroPress );
				for ( TestNum = 1; TestNum <= 4; ++TestNum ) {
					TDBOutdoor = OutdoorDBTempAllClassA2D( TestNum );
					Num = ( ClassNum - 1 ) * 4 + TestNum;
					// Standard Rating Net Cooling Capacity at Test A:
					TotCapFlowModFac = CurveValue( CapFFlowCurveIndex, AirMassFlowRatioRated );
					TotCapTempModFac = CurveValue( CapFTempCurveIndex, TWBIndoor, TDBOutdoor );
					TotCoolingCapRated = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac;
					NetCoolingCapRated( Num ) = TotCoolingCapRated - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
					// Standard Rating total electric power at Test A:
					EIRTempModFac = CurveValue( EIRFTempCurveIndex, TWBIndoor, TDBOutdoor );
					EIRFlowModFac = CurveValue( EIRFFlowCurveIndex, AirMassFlowRatioRated );
					EIR = 0.0;
					if ( RatedCOP > 0.0 ) {
						EIR = EIRTempModFac * EIRFlowModFac / RatedCOP;
					}
					// Calculate net cooling capacity at Test A:
					TotElectricPowerRated( Num ) = EIR * TotCoolingCapRated + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate;
				}
			}
		} else {
			ShowSevereError( "Standard Ratings: " + DXCoilType + ' ' + DXCoilName + " has zero rated total cooling capacity. Capacity and Power cannot be calculated." );
		}

	}

	void
	MultiSpeedDXCoolingCoilStandardRatings(
		std::string const & EP_UNUSED( DXCoilName ), // Name of DX coil for which HSPF is calculated
		std::string const & EP_UNUSED( DXCoilType ), // Type of DX coil for which HSPF is calculated
		Array1A_int const CapFTempCurveIndex, // Index for the capacity as a function of temperature modifier curve
		Array1A_int const CapFFlowCurveIndex, // Index for the capacity as a function of flow fraction modifier curve
		Array1A_int const EIRFTempCurveIndex, // Index for the EIR as a function of temperature modifier curve
		Array1A_int const EIRFFlowCurveIndex, // Index for the EIR as a function of flow fraction modifier curve
		Array1A_int const PLFFPLRCurveIndex, // Index for the PLF vs part-load ratio curve
		Array1A< Real64 > const RatedTotalCapacity, // Reference capacity of DX coil [W]
		Array1A< Real64 > const RatedCOP, // Reference coefficient of performance [W/W]
		Array1A< Real64 > const RatedAirVolFlowRate, // Reference air flow rate of DX coil [m3/s]
		Array1A< Real64 > const FanPowerPerEvapAirFlowRateFromInput, // rated fan power per evap air flow rate [W/(m3/s)]
		int const nsp, // Number of compressor speeds
		Real64 & NetCoolingCapRatedMaxSpeed, // net cooling capacity at maximum speed
		Real64 & SEER // seasonale energy efficiency ratio of multi speed DX cooling coil
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Nigusse, FSEC
		//       DATE WRITTEN   December 2012
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the standard ratings net cooling capacity and SEER values for multi speed DX cooling coils
		// at the AHRI standard test condition(s).

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;

		// Argument array dimensioning
		CapFTempCurveIndex.dim( nsp );
		CapFFlowCurveIndex.dim( nsp );
		EIRFTempCurveIndex.dim( nsp );
		EIRFFlowCurveIndex.dim( nsp );
		PLFFPLRCurveIndex.dim( nsp );
		RatedTotalCapacity.dim( nsp );
		RatedCOP.dim( nsp );
		RatedAirVolFlowRate.dim( nsp );
		FanPowerPerEvapAirFlowRateFromInput.dim( nsp );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		//CHARACTER(len=*), PARAMETER    :: RoutineName='MultiSpeedDXCoolingCoilStandardRatings: ' ! Include trailing blank space
		Real64 const SizingFactor( 1.10 ); // sizing factor per AHRI Std 210/240-2008
		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Intermediate values calculated from the inputs in the idf file
		Array1D< Real64 > FanPowerPerEvapAirFlowRate( nsp ); // Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]
		Array1D< Real64 > TotCoolCapTestA2( nsp ); // Total cooling capacity at A2 test condition (High speed)
		Array1D< Real64 > TotCoolCapTestB2( nsp ); // Total cooling capacity at B2 test condition (High speed)
		Array1D< Real64 > TotCoolCapTestB1( nsp ); // Total cooling capacity at B1 test condition (Low speed)
		Array1D< Real64 > TotCoolCapTestF1( nsp ); // Total cooling capacity at F1 test condition (Low speed)
		Array1D< Real64 > OutdoorUnitPowerTestA2( nsp ); // Outdoor Unit electric power at A2 test condition (High speed)
		Array1D< Real64 > OutdoorUnitPowerTestB2( nsp ); // Outdoor Unit electric power at B2 test condition (High speed)
		Array1D< Real64 > OutdoorUnitPowerTestB1( nsp ); // Outdoor Unit electric power at B1 test condition (Low speed)
		Array1D< Real64 > OutdoorUnitPowerTestF1( nsp ); // Outdoor Unit electric power at F1 test condition (Low speed)
		Array1D< Real64 > NetCoolingCapRated( nsp ); // net cooling capacity at each speed
		Array1D< Real64 > TotCapFlowModFac( nsp ); // Total capacity modifier f(actual flow vs rated flow) for each speed [-]
		Array1D< Real64 > EIRFlowModFac( nsp ); // EIR modifier f(actual supply air flow vs rated flow) for each speed [-]
		static Real64 CoolingCapacityLS( 0.0 ); // cooling capacity of Mult-speed DX coil at lower speed, [W]
		static Real64 CoolingCapacityHS( 0.0 ); // cooling capacity of Mult-speed DX coil at higher speed, [W]
		static Real64 CoolingElecPowerLS( 0.0 ); // outdoor unit electric power input at low speed, [W]
		static Real64 CoolingElecPowerHS( 0.0 ); // outdoor unit electric power input at high speed, [W]
		static Real64 CoolingCapacityMax( 0.0 ); // cooling capacity of Mult-speed DX coil at max speed, [W]
		static Real64 CoolingElecPowerMax( 0.0 ); // outdoor unit electric power input at Max speed, [W]
		static Real64 PartLoadRatio( 0.0 ); // compressor cycling ratio between successive speeds, [-]
		static Real64 PartLoadFraction( 0.0 ); // part-load fraction that account for the cyclic degradation, [-]
		static Real64 NetCoolingCapWeighted( 0.0 ); // net tot cooling cap weighted by the fraction of the binned cooling hours [W]
		static Real64 TotCoolingElecPowerWeighted( 0.0 ); // net total cooling electric power input weighted by the fraction of the
		// binned cooling hours
		static Real64 BuildingCoolingLoad( 0.0 ); // Building space cooling load corresponding to an outdoor bin temperature [W]
		static Real64 NetTotCoolCapBinned( 0.0 ); // Net tot cooling cap corresponding to an outdoor bin temperature [W]
		static Real64 TotCoolElecPowerBinned( 0.0 ); // Total cooling electric power corresponding to an outdoor bin temperature [W]
		static Real64 LoadFactor( 0.0 ); // "on" time for last stage at the desired reduced capacity, (dimensionless)
		int BinNum; // bin number counter
		int spnum; // compressor speed number

		TotCoolingElecPowerWeighted = 0.0;
		TotCoolingElecPowerWeighted = 0.0;

		for ( spnum = 1; spnum <= nsp; ++spnum ) {
			FanPowerPerEvapAirFlowRate( spnum ) = 0.0;
			if ( FanPowerPerEvapAirFlowRateFromInput( spnum ) <= 0.0 ) {
				FanPowerPerEvapAirFlowRate( spnum ) = DefaultFanPowerPerEvapAirFlowRate;
			} else {
				FanPowerPerEvapAirFlowRate( spnum ) = FanPowerPerEvapAirFlowRateFromInput( spnum );
			}
		}

		// Calculate the capacity and power for each speed
		for ( spnum = 1; spnum <= nsp; ++spnum ) {
			TotCapFlowModFac( spnum ) = CurveValue( CapFFlowCurveIndex( spnum ), AirMassFlowRatioRated );
			TotCoolCapTestA2( spnum ) = RatedTotalCapacity( spnum ) * CurveValue( CapFTempCurveIndex( spnum ), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestA2 ) * TotCapFlowModFac( spnum ) - FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );
			TotCoolCapTestB2( spnum ) = RatedTotalCapacity( spnum ) * CurveValue( CapFTempCurveIndex( spnum ), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestB2 ) * TotCapFlowModFac( spnum ) - FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );
			TotCoolCapTestB1( spnum ) = RatedTotalCapacity( spnum ) * CurveValue( CapFTempCurveIndex( spnum ), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestB1 ) * TotCapFlowModFac( spnum ) - FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );
			TotCoolCapTestF1( spnum ) = RatedTotalCapacity( spnum ) * CurveValue( CapFTempCurveIndex( spnum ), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestF1 ) * TotCapFlowModFac( spnum ) - FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );

			EIRFlowModFac( spnum ) = CurveValue( EIRFFlowCurveIndex( spnum ), AirMassFlowRatioRated );
			if ( RatedCOP( spnum ) > 0.0 ) {
				OutdoorUnitPowerTestA2( spnum ) = TotCoolCapTestA2( spnum ) * EIRFlowModFac( spnum ) * CurveValue( EIRFTempCurveIndex( spnum ), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestA2 ) / RatedCOP( spnum ) + FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );
				OutdoorUnitPowerTestB2( spnum ) = TotCoolCapTestB2( spnum ) * EIRFlowModFac( spnum ) * CurveValue( EIRFTempCurveIndex( spnum ), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestB2 ) / RatedCOP( spnum ) + FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );
				OutdoorUnitPowerTestB1( spnum ) = TotCoolCapTestB1( spnum ) * EIRFlowModFac( spnum ) * CurveValue( EIRFTempCurveIndex( spnum ), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestB1 ) / RatedCOP( spnum ) + FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );
				OutdoorUnitPowerTestF1( spnum ) = TotCoolCapTestF1( spnum ) * EIRFlowModFac( spnum ) * CurveValue( EIRFTempCurveIndex( spnum ), IndoorCoilInletAirWetBulbTempRated, OutdoorCoilInletAirDryBulbTempTestF1 ) / RatedCOP( spnum ) + FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );
			}
		}
		// Standard Rating cooling (net) capacity calculations:
		NetCoolingCapRated( nsp ) = TotCoolCapTestA2( nsp );
		NetCoolingCapRatedMaxSpeed = NetCoolingCapRated( nsp );

		// Calculate the SEER value based on contribution of each outdoor air bin temperature
		for ( BinNum = 1; BinNum <= NumOfOATempBins; ++BinNum ) {
			BuildingCoolingLoad = ( OutdoorBinTemperatureSEER( BinNum ) - 18.3 ) / ( 35.0 - 18.3 ) * ( TotCoolCapTestA2( nsp ) / SizingFactor );
			// determine the speed number
			CoolingCapacityMax = TotCoolCapTestB2( nsp ) + ( ( TotCoolCapTestA2( nsp ) - TotCoolCapTestB2( nsp ) ) / ( OutdoorCoilInletAirDryBulbTempTestA2 - OutdoorCoilInletAirDryBulbTempTestB2 ) ) * ( OutdoorBinTemperatureSEER( BinNum ) - OutdoorCoilInletAirDryBulbTempTestB2 );
			CoolingElecPowerMax = OutdoorUnitPowerTestB2( nsp ) + ( ( OutdoorUnitPowerTestA2( nsp ) - OutdoorUnitPowerTestB2( nsp ) ) / ( OutdoorCoilInletAirDryBulbTempTestA2 - OutdoorCoilInletAirDryBulbTempTestB2 ) ) * ( OutdoorBinTemperatureSEER( BinNum ) - OutdoorCoilInletAirDryBulbTempTestB2 );

			for ( spnum = 1; spnum <= nsp - 1; ++spnum ) {
				CoolingCapacityLS = TotCoolCapTestF1( spnum ) + ( ( TotCoolCapTestB1( spnum ) - TotCoolCapTestF1( spnum ) ) / ( OutdoorCoilInletAirDryBulbTempTestB1 - OutdoorCoilInletAirDryBulbTempTestF1 ) ) * ( OutdoorBinTemperatureSEER( BinNum ) - OutdoorCoilInletAirDryBulbTempTestF1 );
				CoolingElecPowerLS = OutdoorUnitPowerTestF1( spnum ) + ( ( OutdoorUnitPowerTestB1( spnum ) - OutdoorUnitPowerTestF1( spnum ) ) / ( OutdoorCoilInletAirDryBulbTempTestB1 - OutdoorCoilInletAirDryBulbTempTestF1 ) ) * ( OutdoorBinTemperatureSEER( BinNum ) - OutdoorCoilInletAirDryBulbTempTestF1 );
				CoolingCapacityHS = TotCoolCapTestB2( spnum + 1 ) + ( ( TotCoolCapTestA2( spnum + 1 ) - TotCoolCapTestB2( spnum + 1 ) ) / ( OutdoorCoilInletAirDryBulbTempTestA2 - OutdoorCoilInletAirDryBulbTempTestB2 ) ) * ( OutdoorBinTemperatureSEER( BinNum ) - OutdoorCoilInletAirDryBulbTempTestB2 );
				CoolingElecPowerHS = OutdoorUnitPowerTestB2( spnum + 1 ) + ( ( OutdoorUnitPowerTestA2( spnum + 1 ) - OutdoorUnitPowerTestB2( spnum + 1 ) ) / ( OutdoorCoilInletAirDryBulbTempTestA2 - OutdoorCoilInletAirDryBulbTempTestB2 ) ) * ( OutdoorBinTemperatureSEER( BinNum ) - OutdoorCoilInletAirDryBulbTempTestB2 );

				if ( BuildingCoolingLoad <= CoolingCapacityLS ) {
					PartLoadRatio = min( 1.0, BuildingCoolingLoad / CoolingCapacityLS );
					NetTotCoolCapBinned = PartLoadRatio * CoolingCapacityLS;
					PartLoadFraction = 1.0 - CyclicDegradationCoeff * ( 1.0 - PartLoadRatio );
					TotCoolElecPowerBinned = ( PartLoadRatio / PartLoadFraction ) * CoolingElecPowerLS;
					goto SpeedLoop_exit;
				} else if ( ( BuildingCoolingLoad > CoolingCapacityLS ) && ( BuildingCoolingLoad < CoolingCapacityHS ) ) {
					// cycle between speed "spnum" and "spnum + 1"
					LoadFactor = min( 1.0, ( CoolingCapacityHS - BuildingCoolingLoad ) / ( CoolingCapacityHS - CoolingCapacityLS ) );
					LoadFactor = max( 0.0, LoadFactor );
					NetTotCoolCapBinned = LoadFactor * CoolingCapacityLS + ( 1.0 - LoadFactor ) * CoolingCapacityHS;
					TotCoolElecPowerBinned = LoadFactor * CoolingElecPowerLS + ( 1.0 - LoadFactor ) * CoolingElecPowerHS;
					goto SpeedLoop_exit;
				} else if ( BuildingCoolingLoad >= CoolingCapacityMax ) {
					NetTotCoolCapBinned = CoolingCapacityMax;
					TotCoolElecPowerBinned = CoolingElecPowerMax;
					goto SpeedLoop_exit;
				}
			}
			SpeedLoop_exit: ;

			NetCoolingCapWeighted += NetTotCoolCapBinned * CoolFracBinHoursAtOutdoorBinTemp( BinNum );
			TotCoolingElecPowerWeighted += TotCoolElecPowerBinned * CoolFracBinHoursAtOutdoorBinTemp( BinNum );
		}
		if ( TotCoolingElecPowerWeighted > 0.0 ) {
			SEER = NetCoolingCapWeighted / TotCoolingElecPowerWeighted;
		} else {
			SEER = 0.0;
		}

	}

	void
	MultiSpeedDXHeatingCoilStandardRatings(
		std::string const & EP_UNUSED( DXCoilName ), // Name of DX coil for which HSPF is calculated
		std::string const & EP_UNUSED( DXCoilType ), // Type of DX coil for which HSPF is calculated
		Array1A_int const CapFTempCurveIndex, // Index for the capacity as a function of temperature modifier curve
		Array1A_int const CapFFlowCurveIndex, // Index for the capacity as a function of flow fraction modifier curve
		Array1A_int const EIRFTempCurveIndex, // Index for the EIR as a function of temperature modifier curve
		Array1A_int const EIRFFlowCurveIndex, // Index for the EIR as a function of flow fraction modifier curve
		Array1A_int const PLFFPLRCurveIndex, // Index for the PLF vs part-load ratio curve
		Array1A< Real64 > const RatedTotalCapacity, // Reference capacity of DX coil [W]
		Array1A< Real64 > const RatedCOP, // Reference coefficient of performance [W/W]
		Array1A< Real64 > const RatedAirVolFlowRate, // Reference air flow rate of DX coil [m3/s]
		Array1A< Real64 > const FanPowerPerEvapAirFlowRateFromInput, // rated fan power per evap air flow rate [W/(m3/s)]
		int const nsp, // Number of compressor speeds
		Real64 & NetHeatingCapRatedHighTemp, // net heating capacity at maximum speed and High Temp
		Real64 & NetHeatingCapRatedLowTemp, // net heating capacity at maximum speed and low Temp
		Real64 & HSPF, // seasonale energy efficiency ratio of multi speed DX cooling coil
		Optional_int_const RegionNum, // Region number for calculating HSPF of single speed DX heating coil
		Optional< Real64 const > MinOATCompressor, // Minimum OAT for heat pump compressor operation [C]
		Optional< Real64 const > OATempCompressorOn, // The outdoor temperature when the compressor is automatically turned
		Optional_bool_const OATempCompressorOnOffBlank, // Flag used to determine low temperature cut out factor
		Optional_int_const DefrostControl // defrost control; 1=timed, 2=on-demand
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Nigusse, FSEC
		//       DATE WRITTEN   December 2012
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the standard ratings net heating capacity and HSPF values for multi speed DX heating coils
		// at the AHRI standard test condition(s).

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using CurveManager::GetCurveType;

		// Argument array dimensioning
		CapFTempCurveIndex.dim( nsp );
		CapFFlowCurveIndex.dim( nsp );
		EIRFTempCurveIndex.dim( nsp );
		EIRFFlowCurveIndex.dim( nsp );
		PLFFPLRCurveIndex.dim( nsp );
		RatedTotalCapacity.dim( nsp );
		RatedCOP.dim( nsp );
		RatedAirVolFlowRate.dim( nsp );
		FanPowerPerEvapAirFlowRateFromInput.dim( nsp );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// back on, if applicable, following automatic shut off. This field is
		// used only for HSPF calculation. [C]

		// SUBROUTINE PARAMETER DEFINITIONS:
		//CHARACTER(len=*), PARAMETER      :: RoutineName='MultiSpeedDXHeatingCoilStandardRatings: ' ! Include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Intermediate values calculated from the inputs in the idf file
		Array1D< Real64 > FanPowerPerEvapAirFlowRate( nsp ); // Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]
		Array1D< Real64 > TotHeatCapTestH0( nsp ); // Total cooling capacity at A2 test condition (High speed)
		Array1D< Real64 > TotHeatCapTestH1( nsp ); // Total cooling capacity at B2 test condition (High speed)
		Array1D< Real64 > TotHeatCapTestH2( nsp ); // Total cooling capacity at B1 test condition (Low speed)
		Array1D< Real64 > TotHeatCapTestH3( nsp ); // Total cooling capacity at F1 test condition (Low speed)
		Array1D< Real64 > OutdoorUnitPowerTestH0( nsp ); // Outdoor Unit electric power at A2 test condition (High speed)
		Array1D< Real64 > OutdoorUnitPowerTestH1( nsp ); // Outdoor Unit electric power at B2 test condition (High speed)
		Array1D< Real64 > OutdoorUnitPowerTestH2( nsp ); // Outdoor Unit electric power at B1 test condition (Low speed)
		Array1D< Real64 > OutdoorUnitPowerTestH3( nsp ); // Outdoor Unit electric power at F1 test condition (Low speed)
		Real64 HeatingCapacityLS; // cooling capacity of Mult-speed DX coil at lower speed, [W]
		Real64 HeatingCapacityHS; // cooling capacity of Mult-speed DX coil at higher speed, [W]
		Real64 HeatingElecPowerLS; // outdoor unit electric power input at low speed, [W]
		Real64 HeatingElecPowerHS; // outdoor unit electric power input at high speed, [W]
		Real64 HeatingCapacityMax; // cooling capacity of Mult-speed DX coil at max speed, [W]
		Real64 HeatingElecPowerMax; // outdoor unit electric power input at Max speed, [W]
		Array1D< Real64 > TotHeatCapTestH1High( nsp ); // net heating capacity high speed at H1 test conditon, [W]

		// Intermediate values calculated from the inputs in the idf file
		Array1D< Real64 > TotCapFlowModFac( nsp ); // Total capacity modifier f(actual flow vs rated flow) for each speed [-]
		Array1D< Real64 > EIRFlowModFac( nsp ); // EIR modifier f(actual supply air flow vs rated flow) for each speed [-]

		static Real64 TotCapTempModFacH0( 0.0 ); // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H0 Test [-]
		static Real64 EIRTempModFacH0( 0.0 ); // EIR modifier (function of entering wetbulb, outside drybulb)  at H0 Test[-]
		static Real64 TotCapTempModFacH1( 0.0 ); // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H1 Test [-]
		static Real64 EIRTempModFacH1( 0.0 ); // EIR modifier (function of entering wetbulb, outside drybulb)  at H1 Test[-]
		static Real64 TotCapTempModFacH2( 0.0 ); // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H2 Test [-]
		static Real64 EIRTempModFacH2( 0.0 ); // EIR modifier (function of entering wetbulb, outside drybulb)  at H2 Test[-]
		static Real64 TotCapTempModFacH3( 0.0 ); // Tot capacity modifier (function of entering wetbulb, outside drybulb) at H3 Test [-]
		static Real64 EIRTempModFacH3( 0.0 ); // EIR modifier (function of entering wetbulb, outside drybulb)  at H3 Test[-]

		static Real64 OATempCompressorOff( 0.0 ); // Minimum outdoor air temperature to turn the commpressor off
		static Real64 PartLoadRatio( 0.0 ); // compressor cycling ratio between successive speeds, [-]
		static Real64 PartLoadFraction( 0.0 ); // part-load fraction that account for the cyclic degradation, [-]

		static Real64 NetHeatingCapWeighted( 0.0 ); // net total heating cap weighted by the fraction of the binned cooling hours [W]
		static Real64 TotHeatingElecPowerWeighted( 0.0 ); // net total heat pump and resistance heating electric Energy input weighted by
		// the fraction of the binned cooling hours
		static Real64 BuildingHeatingLoad( 0.0 ); // Building space heating load corresponding to an outdoor bin temperature [W]
		static Real64 NetTotHeatCapBinned( 0.0 ); // Net tot heatinging cap corresponding to an outdoor bin temperature [W]
		static Real64 TotHeatElecPowerBinnedHP( 0.0 ); // Total Heat Pump heating electric power consumption at outdoor bin temp [W]
		static Real64 TotHeatElecPowerBinnedRH( 0.0 ); // Total Resistance heating electric power consumption at outdoor bin temp [W]

		Real64 LoadFactor; // Fractional "on" time for last stage at the desired reduced capacity, (dimensionless)
		static Real64 LowTempCutOutFactor( 0.0 ); // Factor which corresponds to compressor operation depending on outdoor temperature

		static Real64 FractionalBinHours( 0.0 ); // Fractional bin hours for the heating season  [-]
		static Real64 DemandDeforstCredit( 1.0 ); // A factor to adjust HSPF if coil has demand defrost control  [-]
		static Real64 DesignHeatingRequirement( 0.0 ); // The amount of heating required to maintain a given indoor temperature
		// at a particular outdoor design temperature.  [W]
		static Real64 DesignHeatingRequirementMin( 0.0 ); // minimum design heating requirement [W]
		static Real64 DesignHeatingRequirementMax( 0.0 ); // maximum design heating requirement [W]
		int BinNum; // bin number counter
		int spnum; // compressor speed number
		int StandardDHRNum; // Integer counter for standardized DHRs

		NetHeatingCapWeighted = 0.0;
		TotHeatingElecPowerWeighted = 0.0;

		for ( spnum = 1; spnum <= nsp; ++spnum ) {
			FanPowerPerEvapAirFlowRate( spnum ) = 0.0;
			if ( FanPowerPerEvapAirFlowRateFromInput( spnum ) <= 0.0 ) {
				FanPowerPerEvapAirFlowRate( spnum ) = DefaultFanPowerPerEvapAirFlowRate;
			} else {
				FanPowerPerEvapAirFlowRate( spnum ) = FanPowerPerEvapAirFlowRateFromInput( spnum );
			}
		}

		// Proceed withe HSPF value calculation
		for ( spnum = 1; spnum <= nsp; ++spnum ) {
			TotCapFlowModFac( spnum ) = CurveValue( CapFFlowCurveIndex( spnum ), AirMassFlowRatioRated );
			{ auto const SELECT_CASE_var( GetCurveType( CapFTempCurveIndex( spnum ) ) );
			if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				TotCapTempModFacH0 = CurveValue( CapFTempCurveIndex( spnum ), HeatingOutdoorCoilInletAirDBTempH0Test );
				TotCapTempModFacH1 = CurveValue( CapFTempCurveIndex( spnum ), HeatingOutdoorCoilInletAirDBTempRated );
				TotCapTempModFacH2 = CurveValue( CapFTempCurveIndex( spnum ), HeatingOutdoorCoilInletAirDBTempH2Test );
				TotCapTempModFacH3 = CurveValue( CapFTempCurveIndex( spnum ), HeatingOutdoorCoilInletAirDBTempH3Test );
			} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
				TotCapTempModFacH0 = CurveValue( CapFTempCurveIndex( spnum ), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH0Test );
				TotCapTempModFacH1 = CurveValue( CapFTempCurveIndex( spnum ), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempRated );
				TotCapTempModFacH2 = CurveValue( CapFTempCurveIndex( spnum ), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH2Test );
				TotCapTempModFacH3 = CurveValue( CapFTempCurveIndex( spnum ), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH3Test );
			}}

			TotHeatCapTestH0( spnum ) = RatedTotalCapacity( spnum ) * TotCapTempModFacH0 * TotCapFlowModFac( spnum ) + FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );
			TotHeatCapTestH1( spnum ) = RatedTotalCapacity( spnum ) * TotCapTempModFacH1 * TotCapFlowModFac( spnum ) + FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );
			TotHeatCapTestH2( spnum ) = RatedTotalCapacity( spnum ) * TotCapTempModFacH2 * TotCapFlowModFac( spnum ) + FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );
			TotHeatCapTestH3( spnum ) = RatedTotalCapacity( spnum ) * TotCapTempModFacH3 * TotCapFlowModFac( spnum ) + FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );

			EIRFlowModFac( spnum ) = CurveValue( EIRFFlowCurveIndex( spnum ), AirMassFlowRatioRated );

			{ auto const SELECT_CASE_var( GetCurveType( EIRFTempCurveIndex( spnum ) ) );
			if ( ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) ) {
				EIRTempModFacH0 = CurveValue( EIRFTempCurveIndex( spnum ), HeatingOutdoorCoilInletAirDBTempH0Test );
				EIRTempModFacH1 = CurveValue( EIRFTempCurveIndex( spnum ), HeatingOutdoorCoilInletAirDBTempRated );
				EIRTempModFacH2 = CurveValue( EIRFTempCurveIndex( spnum ), HeatingOutdoorCoilInletAirDBTempH2Test );
				EIRTempModFacH3 = CurveValue( EIRFTempCurveIndex( spnum ), HeatingOutdoorCoilInletAirDBTempH3Test );
			} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
				EIRTempModFacH0 = CurveValue( EIRFTempCurveIndex( spnum ), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH0Test );
				EIRTempModFacH1 = CurveValue( EIRFTempCurveIndex( spnum ), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempRated );
				EIRTempModFacH2 = CurveValue( EIRFTempCurveIndex( spnum ), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH2Test );
				EIRTempModFacH3 = CurveValue( EIRFTempCurveIndex( spnum ), HeatingIndoorCoilInletAirDBTempRated, HeatingOutdoorCoilInletAirDBTempH3Test );
			}}
			if ( RatedCOP( spnum ) > 0.0 ) {
				OutdoorUnitPowerTestH0( spnum ) = TotHeatCapTestH0( spnum ) * EIRFlowModFac( spnum ) * EIRTempModFacH0 / RatedCOP( spnum ) + FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );
				OutdoorUnitPowerTestH1( spnum ) = TotHeatCapTestH1( spnum ) * EIRFlowModFac( spnum ) * EIRTempModFacH1 / RatedCOP( spnum ) + FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );
				OutdoorUnitPowerTestH2( spnum ) = TotHeatCapTestH2( spnum ) * EIRFlowModFac( spnum ) * EIRTempModFacH2 / RatedCOP( spnum ) + FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );
				OutdoorUnitPowerTestH3( spnum ) = TotHeatCapTestH3( spnum ) * EIRFlowModFac( spnum ) * EIRTempModFacH3 / RatedCOP( spnum ) + FanPowerPerEvapAirFlowRate( spnum ) * RatedAirVolFlowRate( spnum );
			}
		}

		// determine the HP capacity at the rated condition (AHRI H1 high speed test Condition); and determine the
		// the building heat requirement for the user specified region
		NetHeatingCapRatedHighTemp = TotHeatCapTestH1( nsp );
		NetHeatingCapRatedLowTemp = TotHeatCapTestH3( nsp );

		if ( RegionNum == 5 ) {
			DesignHeatingRequirementMin = NetHeatingCapRatedHighTemp;
			DesignHeatingRequirementMax = 2.20 * NetHeatingCapRatedHighTemp;
		} else {
			DesignHeatingRequirementMin = NetHeatingCapRatedHighTemp * ( 18.33 - OutdoorDesignTemperature( RegionNum ) ) / ( 60.0 / 1.80 );
			DesignHeatingRequirementMax = 2.20 * DesignHeatingRequirementMin;
		}
		// Set the Design Heating Requirement to nearest standard value (From Table 18, AHRI/ANSI Std 210/240)
		for ( StandardDHRNum = 1; StandardDHRNum <= TotalNumOfStandardDHRs - 1; ++StandardDHRNum ) {
			if ( DesignHeatingRequirementMin < StandardDesignHeatingRequirement( 1 ) ) {

				DesignHeatingRequirement = min( StandardDesignHeatingRequirement( 1 ), DesignHeatingRequirementMax );

			} else if ( DesignHeatingRequirementMin >= StandardDesignHeatingRequirement( StandardDHRNum ) && DesignHeatingRequirementMin < StandardDesignHeatingRequirement( StandardDHRNum + 1 ) ) {
				if ( ( DesignHeatingRequirementMin - StandardDesignHeatingRequirement( StandardDHRNum ) ) > ( StandardDesignHeatingRequirement( StandardDHRNum + 1 ) - DesignHeatingRequirementMin ) ) {

					DesignHeatingRequirement = min( StandardDesignHeatingRequirement( StandardDHRNum + 1 ), DesignHeatingRequirementMax );
				} else {
					DesignHeatingRequirement = min( StandardDesignHeatingRequirement( StandardDHRNum ), DesignHeatingRequirementMax );
				}
			} else if ( DesignHeatingRequirementMin >= StandardDesignHeatingRequirement( TotalNumOfStandardDHRs ) ) {
				DesignHeatingRequirement = min( StandardDesignHeatingRequirement( StandardDHRNum ), DesignHeatingRequirementMax );
			}
		}
		// The minimum temperature below which the compressor is turned off
		OATempCompressorOff = MinOATCompressor;

		for ( BinNum = 1; BinNum <= TotalNumOfTemperatureBins( RegionNum ); ++BinNum ) { //NumOfOATempBins

			{ auto const SELECT_CASE_var( RegionNum );
			if ( SELECT_CASE_var == 1 ) {
				FractionalBinHours = RegionOneFracBinHoursAtOutdoorBinTemp( BinNum );
			} else if ( SELECT_CASE_var == 2 ) {
				FractionalBinHours = RegionTwoFracBinHoursAtOutdoorBinTemp( BinNum );
			} else if ( SELECT_CASE_var == 3 ) {
				FractionalBinHours = RegionThreeFracBinHoursAtOutdoorBinTemp( BinNum );
			} else if ( SELECT_CASE_var == 4 ) {
				FractionalBinHours = RegionFourFracBinHoursAtOutdoorBinTemp( BinNum );
			} else if ( SELECT_CASE_var == 5 ) {
				FractionalBinHours = RegionFiveFracBinHoursAtOutdoorBinTemp( BinNum );
			} else if ( SELECT_CASE_var == 6 ) {
				FractionalBinHours = RegionSixFracBinHoursAtOutdoorBinTemp( BinNum );
			} else {
				FractionalBinHours = RegionFourFracBinHoursAtOutdoorBinTemp( BinNum );
			}}

			// Calculate the building heating load
			BuildingHeatingLoad = ( 18.33 - OutdoorBinTemperature( BinNum ) ) / ( 18.33 - OutdoorDesignTemperature( RegionNum ) ) * CorrectionFactor * DesignHeatingRequirement;

			if ( ( OutdoorBinTemperature( BinNum ) <= -8.33 ) || ( OutdoorBinTemperature( BinNum ) >= 7.20 ) ) {
				HeatingCapacityMax = TotHeatCapTestH3( nsp ) + ( ( TotHeatCapTestH1( nsp ) - TotHeatCapTestH3( nsp ) ) * ( OutdoorBinTemperature( BinNum ) - HeatingOutdoorCoilInletAirDBTempH3Test ) / ( HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test ) );
				HeatingElecPowerMax = OutdoorUnitPowerTestH3( nsp ) + ( ( OutdoorUnitPowerTestH1( nsp ) - OutdoorUnitPowerTestH3( nsp ) ) * ( OutdoorBinTemperature( BinNum ) - HeatingOutdoorCoilInletAirDBTempH3Test ) / ( HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test ) );
			} else {
				HeatingCapacityMax = TotHeatCapTestH3( nsp ) + ( ( TotHeatCapTestH2( nsp ) - TotHeatCapTestH3( nsp ) ) * ( OutdoorBinTemperature( BinNum ) - HeatingOutdoorCoilInletAirDBTempH3Test ) / ( HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test ) );
				HeatingElecPowerMax = OutdoorUnitPowerTestH3( nsp ) + ( ( OutdoorUnitPowerTestH2( nsp ) - OutdoorUnitPowerTestH3( nsp ) ) * ( OutdoorBinTemperature( BinNum ) - HeatingOutdoorCoilInletAirDBTempH3Test ) / ( HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test ) );
			}

			// determine the speed number
			for ( spnum = 1; spnum <= nsp - 1; ++spnum ) {
				// Low Speed
				if ( OutdoorBinTemperature( BinNum ) < -8.33 ) {
					HeatingCapacityLS = TotHeatCapTestH3( spnum ) + ( ( TotHeatCapTestH1( spnum ) - TotHeatCapTestH3( spnum ) ) * ( OutdoorBinTemperature( BinNum ) - HeatingOutdoorCoilInletAirDBTempH3Test ) / ( HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test ) );
					HeatingElecPowerLS = OutdoorUnitPowerTestH3( spnum ) + ( ( OutdoorUnitPowerTestH1( spnum ) - OutdoorUnitPowerTestH3( spnum ) ) * ( OutdoorBinTemperature( BinNum ) - HeatingOutdoorCoilInletAirDBTempH3Test ) / ( HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test ) );

				} else if ( OutdoorBinTemperature( BinNum ) >= 4.44 ) {
					HeatingCapacityLS = TotHeatCapTestH1( spnum ) + ( ( TotHeatCapTestH0( spnum ) - TotHeatCapTestH1( spnum ) ) * ( OutdoorBinTemperature( BinNum ) - HeatingOutdoorCoilInletAirDBTempRated ) / ( HeatingOutdoorCoilInletAirDBTempH0Test - HeatingOutdoorCoilInletAirDBTempRated ) );
					HeatingElecPowerLS = OutdoorUnitPowerTestH1( spnum ) + ( ( OutdoorUnitPowerTestH0( spnum ) - OutdoorUnitPowerTestH1( spnum ) ) * ( OutdoorBinTemperature( BinNum ) - HeatingOutdoorCoilInletAirDBTempRated ) / ( HeatingOutdoorCoilInletAirDBTempH0Test - HeatingOutdoorCoilInletAirDBTempRated ) );
				} else {
					HeatingCapacityLS = TotHeatCapTestH3( spnum ) + ( ( TotHeatCapTestH2( spnum ) - TotHeatCapTestH3( spnum ) ) * ( OutdoorBinTemperature( BinNum ) - HeatingOutdoorCoilInletAirDBTempH3Test ) / ( HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test ) );
					HeatingElecPowerLS = OutdoorUnitPowerTestH3( spnum ) + ( ( OutdoorUnitPowerTestH2( spnum ) - OutdoorUnitPowerTestH3( spnum ) ) * ( OutdoorBinTemperature( BinNum ) - HeatingOutdoorCoilInletAirDBTempH3Test ) / ( HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test ) );
				}
				// High Speed
				if ( ( OutdoorBinTemperature( BinNum ) <= -8.33 ) || ( OutdoorBinTemperature( BinNum ) >= 7.20 ) ) {
					HeatingCapacityHS = TotHeatCapTestH3( spnum + 1 ) + ( ( TotHeatCapTestH1( spnum + 1 ) - TotHeatCapTestH3( spnum + 1 ) ) * ( OutdoorBinTemperature( BinNum ) - HeatingOutdoorCoilInletAirDBTempH3Test ) / ( HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test ) );
					HeatingElecPowerHS = OutdoorUnitPowerTestH3( spnum + 1 ) + ( ( OutdoorUnitPowerTestH1( spnum + 1 ) - OutdoorUnitPowerTestH3( spnum + 1 ) ) * ( OutdoorBinTemperature( BinNum ) - HeatingOutdoorCoilInletAirDBTempH3Test ) / ( HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test ) );
				} else {
					HeatingCapacityHS = TotHeatCapTestH3( spnum + 1 ) + ( ( TotHeatCapTestH2( spnum + 1 ) - TotHeatCapTestH3( spnum + 1 ) ) * ( OutdoorBinTemperature( BinNum ) - HeatingOutdoorCoilInletAirDBTempH3Test ) / ( HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test ) );
					HeatingElecPowerHS = OutdoorUnitPowerTestH3( spnum + 1 ) + ( ( OutdoorUnitPowerTestH2( spnum + 1 ) - OutdoorUnitPowerTestH3( spnum + 1 ) ) * ( OutdoorBinTemperature( BinNum ) - HeatingOutdoorCoilInletAirDBTempH3Test ) / ( HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test ) );
				}
				LowTempCutOutFactor = 0.0;
				if ( ! OATempCompressorOnOffBlank ) {
					if ( OutdoorBinTemperature( BinNum ) <= OATempCompressorOff ) {
						LowTempCutOutFactor = 0.0;
					} else if ( OutdoorBinTemperature( BinNum ) > OATempCompressorOff && OutdoorBinTemperature( BinNum ) <= OATempCompressorOn ) {
						LowTempCutOutFactor = 0.5;
					} else {
						LowTempCutOutFactor = 1.0;
					}
				} else {
					LowTempCutOutFactor = 1.0;
				}

				if ( BuildingHeatingLoad <= HeatingCapacityLS ) {
					if ( HeatingCapacityLS > 0.0 ) PartLoadRatio = min( 1.0, BuildingHeatingLoad / HeatingCapacityLS );
					NetTotHeatCapBinned = BuildingHeatingLoad;
					PartLoadFraction = 1.0 - CyclicDegradationCoeff * ( 1.0 - PartLoadRatio );
					TotHeatElecPowerBinnedHP = ( PartLoadRatio / PartLoadFraction ) * HeatingElecPowerLS * LowTempCutOutFactor;
					TotHeatElecPowerBinnedRH = BuildingHeatingLoad * ( 1.0 - LowTempCutOutFactor );

					goto HeatSpeedLoop_exit;

				} else if ( ( BuildingHeatingLoad > HeatingCapacityLS ) && ( BuildingHeatingLoad < HeatingCapacityHS ) ) {
					// cycle between speed "spnum" and "spnum + 1"
					LoadFactor = min( 1.0, ( HeatingCapacityHS - BuildingHeatingLoad ) / ( HeatingCapacityHS - HeatingCapacityLS ) );
					LoadFactor = max( 0.0, LoadFactor );

					NetTotHeatCapBinned = BuildingHeatingLoad;
					TotHeatElecPowerBinnedHP = LoadFactor * HeatingElecPowerLS + ( 1.0 - LoadFactor ) * HeatingElecPowerHS;
					TotHeatElecPowerBinnedHP *= LowTempCutOutFactor;
					TotHeatElecPowerBinnedRH = BuildingHeatingLoad * ( 1.0 - LowTempCutOutFactor );

					goto HeatSpeedLoop_exit;

				} else if ( BuildingHeatingLoad >= HeatingCapacityMax ) {
					NetTotHeatCapBinned = BuildingHeatingLoad;
					if ( ! OATempCompressorOnOffBlank && HeatingElecPowerMax > 0.0 ) {
						if ( ( OutdoorBinTemperature( BinNum ) <= OATempCompressorOff ) || ( HeatingCapacityMax / HeatingElecPowerMax < 1.0 ) ) {
							LowTempCutOutFactor = 0.0;
						} else if ( ( OutdoorBinTemperature( BinNum ) > OATempCompressorOff && OutdoorBinTemperature( BinNum ) <= OATempCompressorOn ) && ( HeatingCapacityMax / HeatingElecPowerMax > 1.0 ) ) {
							LowTempCutOutFactor = 0.5;
						} else if ( ( OutdoorBinTemperature( BinNum ) > OATempCompressorOn ) && ( HeatingCapacityMax / HeatingElecPowerMax > 1.0 ) ) {
							LowTempCutOutFactor = 1.0;
						}
					} else {
						LowTempCutOutFactor = 1.0;
					}

					TotHeatElecPowerBinnedHP = HeatingElecPowerMax * LowTempCutOutFactor;
					TotHeatElecPowerBinnedRH = BuildingHeatingLoad - HeatingCapacityMax * LowTempCutOutFactor;

					goto HeatSpeedLoop_exit;
				}
			}
			HeatSpeedLoop_exit: ;

			NetHeatingCapWeighted += NetTotHeatCapBinned * FractionalBinHours;
			TotHeatingElecPowerWeighted += ( TotHeatElecPowerBinnedHP + TotHeatElecPowerBinnedRH ) * FractionalBinHours;
		}

		if ( DefrostControl == Timed ) {
			DemandDeforstCredit = 1.0; // Timed defrost control
		} else {
			DemandDeforstCredit = 1.03; // Demand defrost control
		}

		if ( TotHeatingElecPowerWeighted > 0.0 ) {
			HSPF = NetHeatingCapWeighted * DemandDeforstCredit / TotHeatingElecPowerWeighted;
		} else {
			HSPF = 0.0;
		}

	}

	void
	ReportDXCoilRating(
		std::string const & CompType, // Type of component
		std::string const & CompName, // Name of component
		int const CompTypeNum, // TypeNum of component
		Real64 const CoolCapVal, // Standard total (net) cooling capacity for AHRI Std. 210/240 {W}
		Real64 const SEERValueIP, // SEER value in IP units {Btu/W-h}
		Real64 const EERValueSI, // EER value in SI units {W/W}
		Real64 const EERValueIP, // EER value in IP units {Btu/W-h}
		Real64 const IEERValueIP, // IEER value in IP units {Btu/W-h}
		Real64 const HighHeatingCapVal, // High Temperature Heating Standard (Net) Rating Capacity
		Real64 const LowHeatingCapVal, // Low Temperature Heating Standard (Net) Rating Capacity
		Real64 const HSPFValueIP, // IEER value in IP units {Btu/W-h}
		int const RegionNum // Region Number for which HSPF is calculated
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse, Chandan Sharma
		//       DATE WRITTEN   February 2010
		//       MODIFIED       May 2010 (Added EER and IEER entries)
		//                      March 2012 (Added HSPF and High/Low Heating Capacity entries)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine writes the standard rating (net) cooling capacity, SEER, EER and IEER values to
		// the "eio" and tabular output files for Single Speed compressor DX Cooling Coils.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataPrecisionGlobals;
		using DataGlobals::OutputFileInits;
		using General::RoundSigDigits;
		using namespace OutputReportPredefined;
		using DataHVACGlobals::CoilDX_CoolingSingleSpeed;
		using DataHVACGlobals::CoilDX_HeatingEmpirical;
		using DataHVACGlobals::CoilDX_MultiSpeedCooling;
		using DataHVACGlobals::CoilDX_MultiSpeedHeating;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//   or ANSI/AHRI Std. 340/360 {W}

		// for AHRI Std. 210/240 {W}
		// for AHRI Std. 210/240 {W}

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyCoolOneTimeFlag( true );
		static bool MyHeatOneTimeFlag( true );

		// Formats
		static gio::Fmt Format_990( "('! <DX Cooling Coil Standard Rating Information>, Component Type, Component Name, ','Standard Rating (Net) Cooling Capacity {W}, ','Standard Rated Net COP {W/W}, ','EER {Btu/W-h}, ','SEER {Btu/W-h}, ','IEER {Btu/W-h}')" );
		static gio::Fmt Format_991( "(' DX Cooling Coil Standard Rating Information, ',A,', ',A,', ',A,', ',A,', ',A,', ',A,', ',A)" );
		static gio::Fmt Format_992( "('! <DX Heating Coil Standard Rating Information>, Component Type, Component Name, ','High Temperature Heating (net) Rating Capacity {W}, ','Low Temperature Heating (net) Rating Capacity {W}, ','HSPF {Btu/W-h}, ','Region Number')" );
		static gio::Fmt Format_993( "(' DX Heating Coil Standard Rating Information, ',A,', ',A,', ',A,', ',A,', ',A,', ',A)" );
		static gio::Fmt Format_994( "('! <DX Cooling Coil Standard Rating Information>, Component Type, Component Name, ','Standard Rating (Net) Cooling Capacity {W}, ','Standard Rated Net COP {W/W}, ','EER {Btu/W-h}, ','SEER {Btu/W-h}, ','IEER {Btu/W-h}')" );
		static gio::Fmt Format_995( "(' DX Cooling Coil Standard Rating Information, ',A,', ',A,', ',A,', ',A,', ',A,', ',A,', ',A)" );

		{ auto const SELECT_CASE_var( CompTypeNum );

		if ( SELECT_CASE_var == CoilDX_CoolingSingleSpeed ) {
			if ( MyCoolOneTimeFlag ) {
				gio::write( OutputFileInits, Format_990 );
				MyCoolOneTimeFlag = false;
			}

			gio::write( OutputFileInits, Format_991 ) << CompType << CompName << RoundSigDigits( CoolCapVal, 1 ) << RoundSigDigits( EERValueSI, 2 ) << RoundSigDigits( EERValueIP, 2 ) << RoundSigDigits( SEERValueIP, 2 ) << RoundSigDigits( IEERValueIP, 2 );

			PreDefTableEntry( pdchDXCoolCoilType, CompName, CompType );
			PreDefTableEntry( pdchDXCoolCoilNetCapSI, CompName, CoolCapVal, 1 );
			PreDefTableEntry( pdchDXCoolCoilCOP, CompName, RoundSigDigits( EERValueSI, 2 ) );
			PreDefTableEntry( pdchDXCoolCoilEERIP, CompName, RoundSigDigits( EERValueIP, 2 ) );
			PreDefTableEntry( pdchDXCoolCoilSEERIP, CompName, RoundSigDigits( SEERValueIP, 2 ) );
			PreDefTableEntry( pdchDXCoolCoilIEERIP, CompName, RoundSigDigits( IEERValueIP, 2 ) );
			addFootNoteSubTable( pdstDXCoolCoil, "ANSI/AHRI ratings account for supply air fan heat and electric power." );

		} else if ( ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) || ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) ) {
			if ( MyHeatOneTimeFlag ) {
				gio::write( OutputFileInits, Format_992 );
				MyHeatOneTimeFlag = false;
			}

			gio::write( OutputFileInits, Format_993 ) << CompType << CompName << RoundSigDigits( HighHeatingCapVal, 1 ) << RoundSigDigits( LowHeatingCapVal, 1 ) << RoundSigDigits( HSPFValueIP, 2 ) << RoundSigDigits( RegionNum );

			PreDefTableEntry( pdchDXHeatCoilType, CompName, CompType );
			PreDefTableEntry( pdchDXHeatCoilHighCap, CompName, HighHeatingCapVal, 1 );
			PreDefTableEntry( pdchDXHeatCoilLowCap, CompName, LowHeatingCapVal, 1 );
			PreDefTableEntry( pdchDXHeatCoilHSPFIP, CompName, RoundSigDigits( HSPFValueIP, 2 ) );
			PreDefTableEntry( pdchDXHeatCoilRegionNum, CompName, RoundSigDigits( RegionNum ) );
			addFootNoteSubTable( pdstDXHeatCoil, "ANSI/AHRI ratings account for supply air fan heat and electric power." );

		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) {
			if ( MyCoolOneTimeFlag ) {
				gio::write( OutputFileInits, Format_994 );
				MyCoolOneTimeFlag = false;
			}

			gio::write( OutputFileInits, Format_995 ) << CompType << CompName << RoundSigDigits( CoolCapVal, 1 ) << ' ' << ' ' << RoundSigDigits( SEERValueIP, 2 ) << ' ';

			PreDefTableEntry( pdchDXCoolCoilType, CompName, CompType );
			PreDefTableEntry( pdchDXCoolCoilNetCapSI, CompName, CoolCapVal, 1 );
			PreDefTableEntry( pdchDXCoolCoilSEERIP, CompName, RoundSigDigits( SEERValueIP, 2 ) );
			addFootNoteSubTable( pdstDXCoolCoil, "ANSI/AHRI ratings account for supply air fan heat and electric power." );

		} else {
		}}

	}

	void
	ReportDXCoolCoilDataCenterApplication(
		std::string const & CompType, // Type of component
		std::string const & CompName, // Name of component
		int const CompTypeNum, // TypeNum of component
		Array1D< Real64 > & NetCoolingCapRated, // net cooling capacity of single speed DX cooling coil
		Array1D< Real64 > & TotElectricPowerRated // total electric power including supply fan
	) {

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Bereket Nigusse
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine writes the standard rating (net) cooling capacity and Electric Power for
		// for room unitary air conditioners single speed DX cooling coils to the "eio" and tabular
		// output files.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// ANSI/ASHRAE Standard 127-2012 - Method of Testing for Rating Computer and Data Processing
		//                                 Room Unitary Air Conditioners

		// Using/Aliasing
		using namespace DataPrecisionGlobals;
		using DataGlobals::OutputFileInits;
		using General::RoundSigDigits;
		using namespace OutputReportPredefined;
		using DataHVACGlobals::CoilDX_CoolingSingleSpeed;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyCoolOneTimeFlag( true );
		int ClassNum; // class number (Class I, II, II, IV)
		int Num; // text number counter
		static std::string ClassName;
		static std::string CompNameNew;

		// Formats
		static gio::Fmt Format_101( "('! <DX Cooling Coil ASHRAE 127 Standard Ratings Information>, Component Type, Component Name, Standard 127 Classification, ','Rated Net Cooling Capacity Test A {W}, ','Rated Total Electric Power Test A {W}, ','Rated Net Cooling Capacity Test B {W}, ','Rated Total Electric Power Test B {W}, ','Rated Net Cooling Capacity Test C {W}, ','Rated Total Electric Power Test C {W}, ','Rated Net Cooling Capacity Test D {W}, ','Rated Total Electric Power Test D {W} ')" );
		static gio::Fmt Format_102( "(' DX Cooling Coil ASHRAE 127 Standard Ratings Information, ',A,', ',A,', ',A,', ',A,', ',A,', ',A,', ',A,', ',A,', ',A,', ',A,', ',A)" );


		{ auto const SELECT_CASE_var( CompTypeNum );

		if ( SELECT_CASE_var == CoilDX_CoolingSingleSpeed ) {
			if ( MyCoolOneTimeFlag ) {
				gio::write( OutputFileInits, Format_101 );
				MyCoolOneTimeFlag = false;
			}
			for ( ClassNum = 1; ClassNum <= 4; ++ClassNum ) {
				Num = ( ClassNum - 1 ) * 4;
				ClassName = "Class " + RoundSigDigits(ClassNum);
				CompNameNew = CompName + "(" + ClassName + ")";
				gio::write( OutputFileInits, Format_102 ) << CompType << CompName << ClassName << RoundSigDigits( NetCoolingCapRated( Num + 1 ), 1 ) << RoundSigDigits( TotElectricPowerRated( Num + 1 ), 1 ) << RoundSigDigits( NetCoolingCapRated( Num + 2 ), 1 ) << RoundSigDigits( TotElectricPowerRated( Num + 2 ), 1 ) << RoundSigDigits( NetCoolingCapRated( Num + 3 ), 1 ) << RoundSigDigits( TotElectricPowerRated( Num + 3 ), 1 ) << RoundSigDigits( NetCoolingCapRated( Num + 4 ), 1 ) << RoundSigDigits( TotElectricPowerRated( Num + 4 ), 1 );
				PreDefTableEntry( pdchDXCoolCoilType, CompNameNew, CompType );
				PreDefTableEntry( pdchDXCoolCoilNetCapSIA, CompNameNew, RoundSigDigits( NetCoolingCapRated( Num + 1 ), 1 ) );
				PreDefTableEntry( pdchDXCoolCoilElecPowerA, CompNameNew, RoundSigDigits( TotElectricPowerRated( Num + 1 ), 1 ) );
				PreDefTableEntry( pdchDXCoolCoilNetCapSIB, CompNameNew, RoundSigDigits( NetCoolingCapRated( Num + 2 ), 1 ) );
				PreDefTableEntry( pdchDXCoolCoilElecPowerB, CompNameNew, RoundSigDigits( TotElectricPowerRated( Num + 2 ), 1 ) );
				PreDefTableEntry( pdchDXCoolCoilNetCapSIC, CompNameNew, RoundSigDigits( NetCoolingCapRated( Num + 3 ), 1 ) );
				PreDefTableEntry( pdchDXCoolCoilElecPowerC, CompNameNew, RoundSigDigits( TotElectricPowerRated( Num + 3 ), 1 ) );
				PreDefTableEntry( pdchDXCoolCoilNetCapSID, CompNameNew, RoundSigDigits( NetCoolingCapRated( Num + 4 ), 1 ) );
				PreDefTableEntry( pdchDXCoolCoilElecPowerD, CompNameNew, RoundSigDigits( TotElectricPowerRated( Num + 4 ), 1 ) );
				addFootNoteSubTable( pdstDXCoolCoil2, "ANSI/ASHRAE Standard 127 includes supply fan heat effect and electric power." );
			}

		} else {
		}}

	}

	void
	CheckCurveLimitsForStandardRatings(
		std::string const & DXCoilName, // Name of DX coil for which HSPF is calculated
		std::string const & DXCoilType, // Type of DX coil - heating or cooling
		int const DXCoilTypeNum, // Integer type of DX coil - heating or cooling
		int const CapFTempCurveIndex, // Index for the capacity as a function of temperature modifier curve
		int const CapFFlowCurveIndex, // Index for the capacity as a function of flow fraction modifier curve
		int const EIRFTempCurveIndex, // Index for the EIR as a function of temperature modifier curve
		int const EIRFFlowCurveIndex, // Index for the EIR as a function of flow fraction modifier curve
		int const PLFFPLRCurveIndex // Index for the EIR vs part-load ratio curve
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         D. Shirey/B. Nigusse, FSEC
		//       DATE WRITTEN   May 2010
		//       MODIFIED       Chandan Sharma, March 2012
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Checks the limits of the various curves used in DXCoil and returns .FALSE. if the limits do not include
		// the standard test condition(s).

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using CurveManager::GetCurveMinMaxValues;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using CurveManager::GetCurveName;
		using DataGlobals::DisplayExtraWarnings;
		using DataHVACGlobals::CoilDX_CoolingSingleSpeed;
		using DataHVACGlobals::CoilDX_HeatingEmpirical;
		using DataHVACGlobals::CoilDX_MultiSpeedCooling;
		using DataHVACGlobals::CoilDX_MultiSpeedHeating;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		static std::string const RoutineName( "CheckCurveLimitsForStandardRatings: " ); // Include trailing blank space

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		//  Minimum and Maximum independent variable limits from Total Cooling Capacity Function of Temperature Curve
		static Real64 CapacityWBTempMin( 0.0 ); // Capacity modifier Min value (wet bulb temperature), from the Curve:BiQuadratic object
		static Real64 CapacityWBTempMax( 0.0 ); // Capacity modifier Max value (wet bulb temperature), from the Curve:BiQuadratic object
		static Real64 CapacityDBTempMin( 0.0 ); // Capacity modifier Min value (dry bulb temperature), from the Curve:BiQuadratic object
		static Real64 CapacityDBTempMax( 0.0 ); // Capacity modifier Max value (dry bulb temperature), from the Curve:BiQuadratic object

		//  Minimum and Maximum independent variable limits from Energy Input Ratio (EIR) Function of Temperature Curve
		static Real64 EIRWBTempMin( 0.0 ); // EIR modifier Min value (wet bulb temperature), from the Curve:BiQuadratic object
		static Real64 EIRWBTempMax( 0.0 ); // EIR modifier Max value (wet bulb temperature), from the Curve:BiQuadratic object
		static Real64 EIRDBTempMin( 0.0 ); // EIR modifier Min value (dry bulb temperature), from the Curve:BiQuadratic object
		static Real64 EIRDBTempMax( 0.0 ); // EIR modifier Max value (dry bulb temperature), from the Curve:BiQuadratic object

		//  Minimum and Maximum independent variable limits from Part Load Fraction Correlation Curve
		static Real64 PLFFPLRMin( 0.0 ); // Maximum value for Part Load Ratio, from the corresponding curve object
		static Real64 PLFFPLRMax( 0.0 ); // Minimum value for Part Load Ratio, from the corresponding curve object

		//  Minimum and Maximum independent variable limits from Total Cooling Capacity Function of Flow Fraction Curve
		static Real64 CapacityFlowRatioMin( 0.0 ); // Minimum value for flow fraction, from the corresponding curve object
		static Real64 CapacityFlowRatioMax( 0.0 ); // Maximum value for flow fraction, from the corresponding curve object

		//  Minimum and Maximum independent variable limits from Energy Input Ratio Function of Flow Fraction Curve
		static Real64 EIRFlowRatioMin( 0.0 ); // Minimum value for flow fraction, from the corresponding curve object
		static Real64 EIRFlowRatioMax( 0.0 ); // Maximum value for flow fraction, from the corresponding curve object

		//  Minimum and Maximum independent variable limits from Total Cooling Capacity Function of Temperature Curve
		static Real64 HeatingCapODBTempMin( 0.0 ); // Capacity modifier Min value (outdoor dry bulb temperature)
		static Real64 HeatingCapODBTempMax( 0.0 ); // Capacity modifier Max value (outdoor dry bulb temperature)
		static Real64 HeatingCapIDBTempMin( 0.0 ); // Capacity modifier Min value (indoor dry bulb temperature)
		static Real64 HeatingCapIDBTempMax( 0.0 ); // Capacity modifier Max value (indoor dry bulb temperature)

		//  Minimum and Maximum independent variable limits from Energy Input Ratio (EIR) Function of Temperature Curve
		static Real64 HeatingEIRODBTempMin( 0.0 ); // EIR modifier Min value (outdoor dry bulb temperature)
		static Real64 HeatingEIRODBTempMax( 0.0 ); // EIR modifier Max value (outdoor dry bulb temperature)
		static Real64 HeatingEIRIDBTempMin( 0.0 ); // EIR modifier Min value (indoor dry bulb temperature)
		static Real64 HeatingEIRIDBTempMax( 0.0 ); // EIR modifier Max value (indoor dry bulb temperature)

		static bool CapCurveOATLimitsExceeded( false ); // Logical for capacity curve OD temp. limits being exceeded (low and High)
		static bool CapCurveHighOATLimitsExceeded( false ); // Logical for capacity curve temperature limits being exceeded (high temp)
		static bool CapCurveFlowLimitsExceeded( false ); // Logical for capacity curve flow fraction limits being exceeded
		static bool EIRCurveHighOATLimitsExceeded( false ); // Logical for EIR curve temperature limits being exceeded (high temp)
		static bool EIRCurveFlowLimitsExceeded( false ); // Logical for EIR curve flow fraction limits being exceeded

		static bool CapCurveMidOATLimitsExceeded( false ); // Logical for capacity curve temperature limits being exceeded (mid temp)
		static bool EIRCurveMidOATLimitsExceeded( false ); // Logical for EIR curve temperature limits being exceeded (mid temp)
		static bool CapCurveLowOATLimitsExceeded( false ); // Logical for capacity curve temperature limits being exceeded (low temp)
		static bool EIRCurveLowOATLimitsExceeded( false ); // Logical for EIR curve temperature limits being exceeded (Low temp)
		static bool PLFfPLRforSEERLimitsExceeded( false ); // Logical for PLF function of PLR limits being exceeded

		static bool CapCurveIEERLimitsExceeded( false ); // Logical for capacity curve temperature limits being exceeded (IEER calcs)
		static bool EIRCurveIEERLimitsExceeded( false ); // Logical for EIR temperature limits being exceeded (IEER calcs)

		static bool HeatingCapCurveHSPFLimitsExceeded( false ); // Logical for capacity curve temperature limits being exceeded
		// (HSPF calcs)
		static bool HeatingEIRCurveHSPFLimitsExceeded( false ); // Logical for EIR curve temperature limits being exceeded
		// (HSPF calcs)

		{ auto const SELECT_CASE_var( DXCoilTypeNum );

		if ( SELECT_CASE_var == CoilDX_CoolingSingleSpeed ) {
			GetCurveMinMaxValues( CapFTempCurveIndex, CapacityWBTempMin, CapacityWBTempMax, CapacityDBTempMin, CapacityDBTempMax );
			GetCurveMinMaxValues( EIRFTempCurveIndex, EIRWBTempMin, EIRWBTempMax, EIRDBTempMin, EIRDBTempMax );
			GetCurveMinMaxValues( CapFFlowCurveIndex, CapacityFlowRatioMin, CapacityFlowRatioMax );
			GetCurveMinMaxValues( EIRFFlowCurveIndex, EIRFlowRatioMin, EIRFlowRatioMax );
			GetCurveMinMaxValues( PLFFPLRCurveIndex, PLFFPLRMin, PLFFPLRMax );

			// Checking the limits of capacity modifying curve for temperatures
			if ( CapacityDBTempMax < OutdoorCoilInletAirDryBulbTempRated || CapacityDBTempMin > OutdoorCoilInletAirDryBulbTempRated || CapacityWBTempMax < CoolingCoilInletAirWetBulbTempRated || CapacityWBTempMin > CoolingCoilInletAirWetBulbTempRated ) {
				CapCurveHighOATLimitsExceeded = true;
			}
			// Checking the limits of capacity modifying curve for flow fraction
			if ( CapacityFlowRatioMax < AirMassFlowRatioRated || CapacityFlowRatioMin > AirMassFlowRatioRated ) {
				CapCurveFlowLimitsExceeded = true;
			}
			// Checking the limits of EIR modifying curve for temperatures
			if ( EIRDBTempMax < OutdoorCoilInletAirDryBulbTempRated || EIRDBTempMin > OutdoorCoilInletAirDryBulbTempRated || EIRWBTempMax < CoolingCoilInletAirWetBulbTempRated || EIRWBTempMin > CoolingCoilInletAirWetBulbTempRated ) {
				EIRCurveHighOATLimitsExceeded = true;
			}
			// Checking the limits of EIR modifying curve for flow fraction
			if ( EIRFlowRatioMax < AirMassFlowRatioRated || EIRFlowRatioMin > AirMassFlowRatioRated ) {
				EIRCurveFlowLimitsExceeded = true;
			}
			// Checking the limits of capacity modifying curve for temperatures (SEER calculation)
			if ( CapacityDBTempMax < OutdoorCoilInletAirDryBulbTempTestB2 || CapacityDBTempMin > OutdoorCoilInletAirDryBulbTempTestB2 || CapacityWBTempMax < CoolingCoilInletAirWetBulbTempRated || CapacityWBTempMin > CoolingCoilInletAirWetBulbTempRated ) {
				CapCurveMidOATLimitsExceeded = true;
			}
			// Checking the limits of EIR modifying curve for temperatures (SEER calculation)
			if ( EIRDBTempMax < OutdoorCoilInletAirDryBulbTempTestB2 || EIRDBTempMin > OutdoorCoilInletAirDryBulbTempTestB2 || EIRWBTempMax < CoolingCoilInletAirWetBulbTempRated || EIRWBTempMin > CoolingCoilInletAirWetBulbTempRated ) {
				EIRCurveMidOATLimitsExceeded = true;
			}
			// Checking the limits of Part Load Fraction for PLR (SEER calculation)
			if ( PLFFPLRMax < PLRforSEER || PLFFPLRMin > PLRforSEER ) {
				PLFfPLRforSEERLimitsExceeded = true;
			}
			// Checking the limits of capacity modifying curve for temperatures (IEER high and low test conditions)
			if ( CapacityDBTempMax < OutdoorCoilInletAirDryBulbTempRated || CapacityDBTempMin > OADBTempLowReducedCapacityTest || CapacityWBTempMax < CoolingCoilInletAirWetBulbTempRated || CapacityWBTempMin > CoolingCoilInletAirWetBulbTempRated ) {
				CapCurveIEERLimitsExceeded = true;
			}
			// Checking the limits of EIR modifying curve for temperatures (IEER high and low test conditions)
			if ( EIRDBTempMax < OutdoorCoilInletAirDryBulbTempRated || EIRDBTempMin > OADBTempLowReducedCapacityTest || EIRWBTempMax < CoolingCoilInletAirWetBulbTempRated || EIRWBTempMin > CoolingCoilInletAirWetBulbTempRated ) {
				EIRCurveIEERLimitsExceeded = true;
			}

			if ( CapCurveHighOATLimitsExceeded || CapCurveFlowLimitsExceeded || EIRCurveHighOATLimitsExceeded || EIRCurveFlowLimitsExceeded || CapCurveMidOATLimitsExceeded || EIRCurveMidOATLimitsExceeded || PLFfPLRforSEERLimitsExceeded || CapCurveIEERLimitsExceeded || EIRCurveIEERLimitsExceeded ) {

				ShowWarningError( "The Standard Ratings is calculated for " + DXCoilType + " = " + DXCoilName + " but not at the AHRI test condition due to curve out of bound." );
				ShowContinueError( " Review the Standard Ratings calculations in the Engineering Reference for this coil type. Also, use Output:Diagnostics, DisplayExtraWarnings for further guidance." );

				if ( DisplayExtraWarnings ) {
					ShowContinueError( RoutineName + "The max and/or min limits specified in the corresponding curve objects" );
					ShowContinueError( " do not include the AHRI test conditions required to calculate one or more of the Standard Rating values." );
				}

				// For Standard Rating Cooling Capacity:
				if ( CapCurveHighOATLimitsExceeded || CapCurveFlowLimitsExceeded ) {
					if ( DisplayExtraWarnings ) {
						ShowContinueError( DXCoilType + '=' + DXCoilName + ":  Standard Rating Cooling Capacity calculated is not at the AHRI test condition." );
						if ( CapCurveHighOATLimitsExceeded ) {
							ShowContinueError( " Check limits in Total Cooling Capacity Function of Temperature Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFTempCurveIndex ) );
						}
						if ( CapCurveFlowLimitsExceeded ) {
							ShowContinueError( " Check limits in Total Cooling Capacity Function of Flow Fraction Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFFlowCurveIndex ) );
						}
					}
				}

				// For EER:
				if ( CapCurveHighOATLimitsExceeded || CapCurveFlowLimitsExceeded || EIRCurveHighOATLimitsExceeded || EIRCurveFlowLimitsExceeded ) {
					if ( DisplayExtraWarnings ) {
						ShowContinueError( DXCoilType + '=' + DXCoilName + ":  Energy Efficiency Ratio (EER) calculated is not at the AHRI test condition." );
						if ( CapCurveHighOATLimitsExceeded ) {
							ShowContinueError( " Check limits in Total Cooling Capacity Function of Temperature Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFTempCurveIndex ) );
						}
						if ( CapCurveFlowLimitsExceeded ) {
							ShowContinueError( " Check limits in Total Cooling Capacity Function of Flow Fraction Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFFlowCurveIndex ) );
						}
						if ( EIRCurveHighOATLimitsExceeded ) {
							ShowContinueError( " Check limits in Energy Input Ratio Function of Temperature Curve, Curve Type = " + GetCurveType( EIRFTempCurveIndex ) + ", Curve Name = " + GetCurveName( EIRFTempCurveIndex ) );
						}
						if ( EIRCurveFlowLimitsExceeded ) {
							ShowContinueError( " Check limits in Energy Input Ratio Function of Flow Fraction Curve, Curve Type = " + GetCurveType( EIRFFlowCurveIndex ) + ", Curve Name = " + GetCurveName( EIRFFlowCurveIndex ) );
						}
					}
				}

				// For SEER:
				if ( CapCurveMidOATLimitsExceeded || EIRCurveMidOATLimitsExceeded || CapCurveFlowLimitsExceeded || EIRCurveFlowLimitsExceeded || PLFfPLRforSEERLimitsExceeded ) {
					if ( DisplayExtraWarnings ) {
						ShowContinueError( DXCoilType + '=' + DXCoilName + ":  Seasonal Energy Efficiency Ratio (SEER) calculated is not at the AHRI test condition." );
						if ( CapCurveMidOATLimitsExceeded ) {
							ShowContinueError( " Check limits in Total Cooling Capacity Function of Temperature Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFTempCurveIndex ) );
						}
						if ( CapCurveFlowLimitsExceeded ) {
							ShowContinueError( " Check limits in Total Cooling Capacity Function of Flow Fraction Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFFlowCurveIndex ) );
						}
						if ( EIRCurveMidOATLimitsExceeded ) {
							ShowContinueError( " Check limits in Energy Input Ratio Function of Temperature Curve, Curve Type = " + GetCurveType( EIRFTempCurveIndex ) + ", Curve Name = " + GetCurveName( EIRFTempCurveIndex ) );
						}
						if ( EIRCurveFlowLimitsExceeded ) {
							ShowContinueError( " Check limits in Energy Input Ratio Function of Flow Fraction Curve, Curve Type = " + GetCurveType( EIRFFlowCurveIndex ) + ", Curve Name = " + GetCurveName( EIRFFlowCurveIndex ) );
						}
						if ( PLFfPLRforSEERLimitsExceeded ) {
							ShowContinueError( " Check limits in Part Load Fraction Correlation Curve, Curve Type = " + GetCurveType( PLFFPLRCurveIndex ) + ", Curve Name = " + GetCurveName( PLFFPLRCurveIndex ) );
						}
					}
				}

				// For IEER:
				if ( CapCurveIEERLimitsExceeded || CapCurveFlowLimitsExceeded || EIRCurveIEERLimitsExceeded || EIRCurveFlowLimitsExceeded ) {
					if ( DisplayExtraWarnings ) {
						ShowContinueError( DXCoilType + '=' + DXCoilName + ":  Integrated Energy Efficiency Ratio (IEER) calculated is not at the AHRI test condition." );
						if ( CapCurveIEERLimitsExceeded ) {
							ShowContinueError( " Check limits in Total Cooling Capacity Function of Temperature Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFTempCurveIndex ) );
						}
						if ( CapCurveFlowLimitsExceeded ) {
							ShowContinueError( " Check limits in Total Cooling Capacity Function of Flow Fraction Curve, Curve Type = " + GetCurveType( CapFFlowCurveIndex ) + ", Curve Name = " + GetCurveName( CapFFlowCurveIndex ) );
						}
						if ( EIRCurveIEERLimitsExceeded ) {
							ShowContinueError( " Check limits in EIR Function of Temperature Curve, Curve Type = " + GetCurveType( EIRFTempCurveIndex ) + ", Curve Name = " + GetCurveName( EIRFTempCurveIndex ) );
						}
						if ( EIRCurveFlowLimitsExceeded ) {
							ShowContinueError( " Check limits in Energy Input Ratio Function of Flow Fraction Curve, Curve Type = " + GetCurveType( EIRFFlowCurveIndex ) + ", Curve Name = " + GetCurveName( EIRFFlowCurveIndex ) );
						}
					}
				}

			} // End of curve error messages
		} else if ( SELECT_CASE_var == CoilDX_HeatingEmpirical ) {
			{ auto const SELECT_CASE_var1( GetCurveType( CapFTempCurveIndex ) );

			if ( ( SELECT_CASE_var1 == "QUADRATIC" ) || ( SELECT_CASE_var1 == "CUBIC" ) ) {
				GetCurveMinMaxValues( CapFTempCurveIndex, HeatingCapODBTempMin, HeatingCapODBTempMax );

				// Checking the limits of capacity modifying curve for temperatures (IEER high and low test conditions)
				if ( HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated || HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test ) {
					HeatingCapCurveHSPFLimitsExceeded = true;
				}
			} else if ( SELECT_CASE_var1 == "BIQUADRATIC" ) {
				GetCurveMinMaxValues( CapFTempCurveIndex, HeatingCapIDBTempMin, HeatingCapIDBTempMax, HeatingCapODBTempMin, HeatingCapODBTempMax );

				// Checking the limits of capacity modifying curve for temperatures (IEER high and low test conditions)
				if ( HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated || HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test || HeatingCapIDBTempMax < HeatingIndoorCoilInletAirDBTempRated || HeatingCapIDBTempMin > HeatingIndoorCoilInletAirDBTempRated ) {
					HeatingCapCurveHSPFLimitsExceeded = true;
				}
			}}
			{ auto const SELECT_CASE_var1( GetCurveType( EIRFTempCurveIndex ) );

			if ( ( SELECT_CASE_var1 == "QUADRATIC" ) || ( SELECT_CASE_var1 == "CUBIC" ) ) {
				GetCurveMinMaxValues( EIRFTempCurveIndex, HeatingEIRODBTempMin, HeatingEIRODBTempMax );

				// Checking the limits of EIR modifying curve for temperatures (HSPF high and low test conditions)
				if ( HeatingEIRODBTempMax < HeatingOutdoorCoilInletAirDBTempRated || HeatingEIRODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test ) {
					HeatingEIRCurveHSPFLimitsExceeded = true;
				}
			} else if ( SELECT_CASE_var1 == "BIQUADRATIC" ) {
				GetCurveMinMaxValues( EIRFTempCurveIndex, HeatingEIRIDBTempMin, HeatingEIRIDBTempMax, HeatingEIRODBTempMin, HeatingEIRODBTempMax );

				// Checking the limits of EIR modifying curve for temperatures (HSPF high and low test conditions)
				if ( HeatingEIRODBTempMax < HeatingOutdoorCoilInletAirDBTempRated || HeatingEIRODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test || HeatingEIRIDBTempMax < HeatingIndoorCoilInletAirDBTempRated || HeatingEIRIDBTempMin > HeatingIndoorCoilInletAirDBTempRated ) {
					HeatingEIRCurveHSPFLimitsExceeded = true;
				}
			}}
			if ( HeatingCapCurveHSPFLimitsExceeded || HeatingEIRCurveHSPFLimitsExceeded ) {
				ShowWarningError( "The Standard Ratings is calculated for " + DXCoilType + " = " + DXCoilName + " but not at the AHRI test condition due to curve out of bound." );
				ShowContinueError( " Review the Standard Ratings calculations in the Engineering Reference for this coil type. Also, use Output:Diagnostics, DisplayExtraWarnings for further guidance." );
				if ( DisplayExtraWarnings ) {
					ShowContinueError( RoutineName + "The max and/or min limits specified in the corresponding curve objects" );
					ShowContinueError( " do not include the AHRI test conditions required to calculate one or more of the Standard Rating values." );
				}
				if ( DisplayExtraWarnings ) {
					ShowWarningError( DXCoilType + '=' + DXCoilName + ":  Heating Seasonal Performance Factor calculated is not at the AHRI test condition." );
					ShowContinueError( " Review the Standard Ratings calculations in the Engineering Reference for this coil type." );
					if ( HeatingCapCurveHSPFLimitsExceeded ) {
						ShowContinueError( " Check limits in Total Heating Capacity Function of Temperature Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFTempCurveIndex ) );
					}
					if ( HeatingEIRCurveHSPFLimitsExceeded ) {
						ShowContinueError( " Check limits in EIR Function of Temperature Curve, Curve Type = " + GetCurveType( EIRFTempCurveIndex ) + ", Curve Name = " + GetCurveName( EIRFTempCurveIndex ) );
					}
				}
			}

			//   MultiSpeed DX Coil Net Cooling Capacity and SEER:
		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedCooling ) {
			GetCurveMinMaxValues( CapFTempCurveIndex, CapacityWBTempMin, CapacityWBTempMax, CapacityDBTempMin, CapacityDBTempMax );
			GetCurveMinMaxValues( EIRFTempCurveIndex, EIRWBTempMin, EIRWBTempMax, EIRDBTempMin, EIRDBTempMax );
			GetCurveMinMaxValues( CapFFlowCurveIndex, CapacityFlowRatioMin, CapacityFlowRatioMax );
			GetCurveMinMaxValues( EIRFFlowCurveIndex, EIRFlowRatioMin, EIRFlowRatioMax );
			//CALL GetCurveMinMaxValues(PLFFPLRCurveIndex,PLFFPLRMin,PLFFPLRMax)

			// Checking the limits of capacity modifying curve for temperatures
			if ( CapacityDBTempMax < OutdoorCoilInletAirDryBulbTempRated || CapacityDBTempMin > OutdoorCoilInletAirDryBulbTempRated || CapacityWBTempMax < CoolingCoilInletAirWetBulbTempRated || CapacityWBTempMin > CoolingCoilInletAirWetBulbTempRated ) {
				CapCurveHighOATLimitsExceeded = true;
			}
			// Checking the limits of capacity modifying curve for flow fraction
			if ( CapacityFlowRatioMax < AirMassFlowRatioRated || CapacityFlowRatioMin > AirMassFlowRatioRated ) {
				CapCurveFlowLimitsExceeded = true;
			}
			// Checking the limits of EIR modifying curve for temperatures
			if ( EIRDBTempMax < OutdoorCoilInletAirDryBulbTempRated || EIRDBTempMin > OutdoorCoilInletAirDryBulbTempRated || EIRWBTempMax < CoolingCoilInletAirWetBulbTempRated || EIRWBTempMin > CoolingCoilInletAirWetBulbTempRated ) {
				EIRCurveHighOATLimitsExceeded = true;
			}
			// Checking the limits of EIR modifying curve for flow fraction
			if ( EIRFlowRatioMax < AirMassFlowRatioRated || EIRFlowRatioMin > AirMassFlowRatioRated ) {
				EIRCurveFlowLimitsExceeded = true;
			}
			// Checking the limits of capacity modifying curve for temperatures (SEER calculation)
			if ( CapacityDBTempMax < OutdoorCoilInletAirDryBulbTempTestF1 || CapacityDBTempMin > OutdoorCoilInletAirDryBulbTempTestF1 || CapacityWBTempMax < CoolingCoilInletAirWetBulbTempRated || CapacityWBTempMin > CoolingCoilInletAirWetBulbTempRated ) {
				CapCurveLowOATLimitsExceeded = true;
			}
			// Checking the limits of EIR modifying curve for temperatures (SEER calculation)
			if ( EIRDBTempMax < OutdoorCoilInletAirDryBulbTempTestF1 || EIRDBTempMin > OutdoorCoilInletAirDryBulbTempTestF1 || EIRWBTempMax < CoolingCoilInletAirWetBulbTempRated || EIRWBTempMin > CoolingCoilInletAirWetBulbTempRated ) {
				EIRCurveLowOATLimitsExceeded = true;
			}

			if ( CapCurveHighOATLimitsExceeded || CapCurveFlowLimitsExceeded || EIRCurveHighOATLimitsExceeded || EIRCurveFlowLimitsExceeded || CapCurveLowOATLimitsExceeded || EIRCurveLowOATLimitsExceeded ) {

				ShowWarningError( "The Standard Ratings is calculated for " + DXCoilType + " = " + DXCoilName + " but not at the AHRI test condition due to curve out of bound." );
				ShowContinueError( " Review the Standard Ratings calculations in the Engineering Reference for this coil type. Also, use Output:Diagnostics, DisplayExtraWarnings for further guidance." );

				if ( DisplayExtraWarnings ) {
					ShowContinueError( RoutineName + "The max and/or min limits specified in the corresponding curve objects" );
					ShowContinueError( " do not include the AHRI test conditions required to calculate one or more of the Standard Rating values." );
				}

				// For Standard Rating Cooling Capacity:
				if ( CapCurveHighOATLimitsExceeded || CapCurveFlowLimitsExceeded ) {
					if ( DisplayExtraWarnings ) {
						ShowContinueError( DXCoilType + '=' + DXCoilName + ":  The Standard Rating Cooling Capacity calculated is not at the AHRI test condition." );
						if ( CapCurveHighOATLimitsExceeded ) {
							ShowContinueError( " Check limits in Total Cooling Capacity Function of Temperature Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFTempCurveIndex ) );
						}
						if ( CapCurveFlowLimitsExceeded ) {
							ShowContinueError( " Check limits in Total Cooling Capacity Function of Flow Fraction Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFFlowCurveIndex ) );
						}
					}
				}

				// For MultiSpeed DX Coil SEER:

				if ( CapCurveLowOATLimitsExceeded || EIRCurveLowOATLimitsExceeded || CapCurveFlowLimitsExceeded || EIRCurveFlowLimitsExceeded ) {
					if ( DisplayExtraWarnings ) {
						ShowContinueError( DXCoilType + '=' + DXCoilName + ":  The Seasonal Energy Efficiency Ratio (SEER) calculated is not at the AHRI test condition." );
						if ( CapCurveLowOATLimitsExceeded ) {
							ShowContinueError( " Check limits in Total Cooling Capacity Function of Temperature Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFTempCurveIndex ) );
						}
						if ( CapCurveFlowLimitsExceeded ) {
							ShowContinueError( " Check limits in Total Cooling Capacity Function of Flow Fraction Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFFlowCurveIndex ) );
						}
						if ( EIRCurveLowOATLimitsExceeded ) {
							ShowContinueError( " Check limits in Energy Input Ratio Function of Temperature Curve, Curve Type = " + GetCurveType( EIRFTempCurveIndex ) + ", Curve Name = " + GetCurveName( EIRFTempCurveIndex ) );
						}
						if ( EIRCurveFlowLimitsExceeded ) {
							ShowContinueError( " Check limits in Energy Input Ratio Function of Flow Fraction Curve, Curve Type = " + GetCurveType( EIRFFlowCurveIndex ) + ", Curve Name = " + GetCurveName( EIRFFlowCurveIndex ) );
						}
					}
				}

			} // End of curve error messages

		} else if ( SELECT_CASE_var == CoilDX_MultiSpeedHeating ) {

			{ auto const SELECT_CASE_var1( GetCurveType( CapFTempCurveIndex ) );

			if ( ( SELECT_CASE_var1 == "QUADRATIC" ) || ( SELECT_CASE_var1 == "CUBIC" ) ) {
				GetCurveMinMaxValues( CapFTempCurveIndex, HeatingCapODBTempMin, HeatingCapODBTempMax );

				if ( HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated || HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test ) {
					CapCurveOATLimitsExceeded = true;
				}
				// Checking the limits of capacity modifying curve for temperatures (HSPF high and low test conditions)
				if ( HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated || HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test || HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempH0Test ) {
					HeatingCapCurveHSPFLimitsExceeded = true;
				}

			} else if ( SELECT_CASE_var1 == "BIQUADRATIC" ) {
				GetCurveMinMaxValues( CapFTempCurveIndex, HeatingCapIDBTempMin, HeatingCapIDBTempMax, HeatingCapODBTempMin, HeatingCapODBTempMax );

				// Checking the limits of capacity modifying curve for temperatures (HSPF high and low test conditions)
				if ( HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated || HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test || HeatingCapIDBTempMax < HeatingIndoorCoilInletAirDBTempRated || HeatingCapIDBTempMin > HeatingIndoorCoilInletAirDBTempRated || HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempH0Test ) {
					HeatingCapCurveHSPFLimitsExceeded = true;
				}
			}}

			{ auto const SELECT_CASE_var1( GetCurveType( EIRFTempCurveIndex ) );

			if ( ( SELECT_CASE_var1 == "QUADRATIC" ) || ( SELECT_CASE_var1 == "CUBIC" ) ) {
				GetCurveMinMaxValues( EIRFTempCurveIndex, HeatingEIRODBTempMin, HeatingEIRODBTempMax );
				// Checking the limits of EIR modifying curve for temperatures (HSPF high and low test conditions)
				if ( HeatingEIRODBTempMax < HeatingOutdoorCoilInletAirDBTempRated || HeatingEIRODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test || HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempH0Test ) {
					HeatingEIRCurveHSPFLimitsExceeded = true;
				}
			} else if ( SELECT_CASE_var1 == "BIQUADRATIC" ) {
				GetCurveMinMaxValues( EIRFTempCurveIndex, HeatingEIRIDBTempMin, HeatingEIRIDBTempMax, HeatingEIRODBTempMin, HeatingEIRODBTempMax );

				// Checking the limits of EIR modifying curve for temperatures (HSPF high and low test conditions)
				if ( HeatingEIRODBTempMax < HeatingOutdoorCoilInletAirDBTempRated || HeatingEIRODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test || HeatingEIRIDBTempMax < HeatingIndoorCoilInletAirDBTempRated || HeatingEIRIDBTempMin > HeatingIndoorCoilInletAirDBTempRated || HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempH0Test ) {
					HeatingEIRCurveHSPFLimitsExceeded = true;
				}
			}}
			if ( HeatingCapCurveHSPFLimitsExceeded || HeatingEIRCurveHSPFLimitsExceeded || CapCurveOATLimitsExceeded ) {

				ShowWarningError( "The Standard Ratings is calculated for " + DXCoilType + " = " + DXCoilName + " but not at the AHRI test condition due to curve out of bound." );
				ShowContinueError( " Review the Standard Ratings calculations in the Engineering Reference for this coil type. Also, use Output:Diagnostics, DisplayExtraWarnings for further guidance." );

				if ( DisplayExtraWarnings ) {
					ShowContinueError( RoutineName + "The max and/or min limits specified in the corresponding curve objects" );
					ShowContinueError( " do not include the AHRI test conditions required to calculate one or more of the Standard Rating values." );
				}
			}
			if ( CapCurveOATLimitsExceeded ) {
				if ( DisplayExtraWarnings ) {
					ShowWarningError( DXCoilType + '=' + DXCoilName + ":  The Net Heating Capacity Calculated is not at the AHRI test condition." );
					ShowContinueError( " Check limits in Total Heating Capacity Function of Temperature Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFTempCurveIndex ) );
				}
			}
			if ( HeatingCapCurveHSPFLimitsExceeded || HeatingEIRCurveHSPFLimitsExceeded ) {
				if ( DisplayExtraWarnings ) {
					ShowWarningError( DXCoilType + '=' + DXCoilName + ":  The Heating Seasonal Performance Factor calculated is not at the AHRI test condition." );
					if ( HeatingCapCurveHSPFLimitsExceeded ) {
						ShowContinueError( " Check limits in Total Heating Capacity Function of Temperature Curve, Curve Type = " + GetCurveType( CapFTempCurveIndex ) + ", Curve Name = " + GetCurveName( CapFTempCurveIndex ) );
					}
					if ( HeatingEIRCurveHSPFLimitsExceeded ) {
						ShowContinueError( " Check limits in EIR Function of Temperature Curve, Curve Type = " + GetCurveType( EIRFTempCurveIndex ) + ", Curve Name = " + GetCurveName( EIRFTempCurveIndex ) );
					}
				}
			}
		} else {
		}}

	}

} // StandardRatings

} // EnergyPlus
