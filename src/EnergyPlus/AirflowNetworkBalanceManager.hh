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

#ifndef AirflowNetworkBalanceManager_hh_INCLUDED
#define AirflowNetworkBalanceManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace AirflowNetworkBalanceManager {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const VentCtrNum_None; // Wrong input
	extern int const VentCtrNum_Temp; // Temperature venting control
	extern int const VentCtrNum_Enth; // Enthalpy venting control
	extern int const VentCtrNum_Const; // Constant venting control
	extern int const VentCtrNum_ASH55;
	extern int const VentCtrNum_CEN15251;
	extern int const VentCtrNum_Novent; // No venting
	extern int const VentCtrNum_ZoneLevel; // ZoneLevel control for a heat transfer subsurface
	extern int const VentCtrNum_AdjTemp; // Temperature venting control based on adjacent zone conditions
	extern int const VentCtrNum_AdjEnth; // Enthalpy venting control based on adjacent zone conditions

	// DERIVED TYPE DEFINITIONS:
	// Report variables

	// MODULE VARIABLE DECLARATIONS:
	// Report variables
	extern Array1D< Real64 > PZ;
	// Inverse matrix
	extern Array1D< Real64 > MA;
	extern Array1D< Real64 > MV;
	extern Array1D_int IVEC;
	extern Array1D_int SplitterNodeNumbers;

	extern bool AirflowNetworkGetInputFlag;
	extern int VentilationCtrl; // Hybrid ventilation control type
	extern int NumOfExhaustFans; // Number of exhaust fans

	extern int NumAirflowNetwork;
	extern int AirflowNetworkNumOfDetOpenings;
	extern int AirflowNetworkNumOfSimOpenings;
	extern int AirflowNetworkNumOfHorOpenings;
	extern int AirflowNetworkNumOfStdCndns;
	extern int AirflowNetworkNumOfSurCracks;
	extern int AirflowNetworkNumOfSurELA;
	extern int AirflowNetworkNumOfExtNode;
	extern int AirflowNetworkNumOfCPArray;
	extern int AirflowNetworkNumOfCPValue;
	extern int AirflowNetworkNumOfSingleSideZones; // Total number of zones with advanced single sided wind pressure coefficient calculation
	extern int AirflowNetworkNumofWindDir;
	extern int DisSysNumOfNodes;
	extern int DisSysNumOfLeaks;
	extern int DisSysNumOfELRs;
	extern int DisSysNumOfDucts;
	extern int DisSysNumOfDampers;
	extern int DisSysNumOfCVFs;
	extern int DisSysNumOfDetFans;
	extern int DisSysNumOfCoils;
	extern int DisSysNumOfHXs;
	extern int DisSysNumOfCPDs;
	extern int DisSysNumOfTermUnits;
	extern int DisSysNumOfLinks;
	extern int NumOfExtNodes;
	extern int AirflowNetworkNumOfExtSurfaces;
	extern Real64 IncAng; // Wind incidence angle relative to facade normal (deg)
	extern Array1D< Real64 > FacadeAng; // Facade azimuth angle (for walls, angle of outward normal to facade measured clockwise from North) (deg)
	extern int WindDirNum; // Wind direction number
	extern Real64 WindAng; // Wind direction angle (degrees clockwise from North)
	extern int SupplyFanInletNode; // Supply air fan inlet node number
	extern int SupplyFanOutletNode; // Supply air fan outlet node number
	extern int SupplyFanType; // Supply air fan type
	extern Real64 OnOffFanRunTimeFraction; // Run time fraction for an On/Off fan flow rate
	extern int AirflowNetworkNumOfOccuVentCtrls;

	// SUBROUTINE SPECIFICATIONS FOR MODULE AirflowNetworkBalanceManager:
	// Name Public routines, optionally name Private routines within this module

	// Types

	struct AirflowNetworkReportVars
	{
		// Members
		Real64 MeanAirTemp; // Mean Air Temperature {C}
		Real64 OperativeTemp; // Average of Mean Air Temperature {C} and Mean Radiant Temperature {C}
		Real64 InfilHeatGain; // Heat Gain {W} due to infiltration
		Real64 InfilHeatLoss; // Heat Loss {W} due to infiltration
		Real64 InfilVolume; // Volume of Air {m3} due to infiltration
		Real64 InfilMass; // Mass of Air {kg} due to infiltration
		Real64 InfilAirChangeRate; // Infiltration air change rate {ach}
		Real64 VentilHeatLoss; // Heat Gain {W} due to ventilation
		Real64 VentilHeatGain; // Heat Loss {W} due to ventilation
		Real64 VentilVolume; // Volume of Air {m3} due to ventilation
		Real64 VentilMass; // Mass of Air {kg} due to ventilation
		Real64 VentilFanElec; // Fan Electricity {W} due to ventilation
		Real64 VentilAirTemp; // Air Temp {C} of ventilation
		Real64 MixVolume; // Mixing volume of Air {m3}
		Real64 MixMass; // Mixing mass of air {kg}

		// Default Constructor
		AirflowNetworkReportVars() :
			MeanAirTemp( 0.0 ),
			OperativeTemp( 0.0 ),
			InfilHeatGain( 0.0 ),
			InfilHeatLoss( 0.0 ),
			InfilVolume( 0.0 ),
			InfilMass( 0.0 ),
			InfilAirChangeRate( 0.0 ),
			VentilHeatLoss( 0.0 ),
			VentilHeatGain( 0.0 ),
			VentilVolume( 0.0 ),
			VentilMass( 0.0 ),
			VentilFanElec( 0.0 ),
			VentilAirTemp( 0.0 ),
			MixVolume( 0.0 ),
			MixMass( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< AirflowNetworkReportVars > AirflowNetworkZnRpt;

	// Functions

	void
	clear_state();

	void
	ManageAirflowNetworkBalance(
		Optional_bool_const FirstHVACIteration = _, // True when solution technique on first iteration
		Optional_int_const Iter = _, // Iteration number
		Optional_bool ResimulateAirZone = _ // True when solution technique on third iteration
	);

	void
	GetAirflowNetworkInput();

	void
	InitAirflowNetwork();

	void
	AllocateAndInitData();

	void
	CalcAirflowNetworkAirBalance();

	void
	CalcWindPressureCoeffs();

	Real64
	CalcWindPressure(
		int const CPVNum, // CP Value number
		Real64 const Vref, // Velocity at reference height
		Real64 const Height // Node height for outdoor temperature calculation
	);

	void
	CalcAirflowNetworkHeatBalance();

	void
	CalcAirflowNetworkMoisBalance();

	void
	CalcAirflowNetworkCO2Balance();

	void
	CalcAirflowNetworkGCBalance();

	void
	MRXINV( int const NORDER );

	void
	ReportAirflowNetwork();

	void
	UpdateAirflowNetwork( Optional_bool_const FirstHVACIteration = _ ); // True when solution technique on first iteration

	void
	AirflowNetworkVentingControl(
		int const i, // AirflowNetwork surface number
		Real64 & OpenFactor // Window or door opening factor (used to calculate airflow)
	);

	void
	ValidateDistributionSystem();

	void
	ValidateExhaustFanInput();

	void
	HybridVentilationControl();

	void
	CalcSingleSidedCps();

	Real64
	GetZoneInfilAirChangeRate( int const ZoneNum ); // hybrid ventilation system controlled zone number

	// derived class or struct
	struct OccupantVentilationControlProp {

		std::string Name; // Provide a unique object name
		Real64 MinOpeningTime; // Minimum Opening Time
		Real64 MinClosingTime; // Minimum Closing Time
		std::string ComfortLowTempCurveName; // Thermal Comfort Low Temperature Curve Name
		std::string ComfortHighTempCurveName; // Thermal Comfort High Temperature Curve Name
		int ComfortLowTempCurveNum; // Thermal Comfort Low Temperature Curve number
		int ComfortHighTempCurveNum; // Thermal Comfort high Temperature Curve number
		int OpeningProbSchNum; // Opening probability schedule pointer
		int ClosingProbSchNum; // Closing probability schedule pointer
		Real64 ComfortBouPoint; // Thermal Comfort Temperature Boundary Point
		bool OccupancyCheck; // Occupancy check
		std::string OpeningProbSchName; // Opening probability schedule name
		std::string ClosingProbSchName; // Closing probability schedule name
		Real64 MaxPPD; // Maximum PPD used to calculate comfort band (%)
		bool MinTimeControlOnly; // Chach minimum opening and closing time only

		// Default Constructor
		OccupantVentilationControlProp():
			MinOpeningTime( 0.0 ),
			MinClosingTime( 0.0 ),
			ComfortLowTempCurveNum( 0 ),
			ComfortHighTempCurveNum( 0 ),
			OpeningProbSchNum( 0 ),
			ClosingProbSchNum( 0 ),
			ComfortBouPoint( 10.0 ),
			OccupancyCheck( false ),
			MaxPPD( 10.0 ),
			MinTimeControlOnly( false )
		{}

		void calc(
			int const ZoneNum,
			int const SurfNum,
			int const PrevOpeningstatus,
			Real64 const TimeOpenDuration,
			Real64 const TimeCloseDuration,
			int & OpeningStatus,
			int & OpeningProbStatus,
			int & ClosingProbStatus
		); // function to perform calculations

		bool openingProbability(
			int const ZoneNum,
			Real64 const TimeCloseDuration
			); // function to perform calculations of opening probability

		bool closingProbability(
			Real64 const TimeCloseDuration
			); // function to perform calculations of closing probability
	};

	extern Array1D< OccupantVentilationControlProp > OccupantVentilationControl;

} // AirflowNetworkBalanceManager

} // EnergyPlus

#endif
