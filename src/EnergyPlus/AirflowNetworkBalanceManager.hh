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

		// Member Constructor
		AirflowNetworkReportVars(
			Real64 const MeanAirTemp, // Mean Air Temperature {C}
			Real64 const OperativeTemp, // Average of Mean Air Temperature {C} and Mean Radiant Temperature {C}
			Real64 const InfilHeatGain, // Heat Gain {W} due to infiltration
			Real64 const InfilHeatLoss, // Heat Loss {W} due to infiltration
			Real64 const InfilVolume, // Volume of Air {m3} due to infiltration
			Real64 const InfilMass, // Mass of Air {kg} due to infiltration
			Real64 const InfilAirChangeRate, // Infiltration air change rate {ach}
			Real64 const VentilHeatLoss, // Heat Gain {W} due to ventilation
			Real64 const VentilHeatGain, // Heat Loss {W} due to ventilation
			Real64 const VentilVolume, // Volume of Air {m3} due to ventilation
			Real64 const VentilMass, // Mass of Air {kg} due to ventilation
			Real64 const VentilFanElec, // Fan Electricity {W} due to ventilation
			Real64 const VentilAirTemp, // Air Temp {C} of ventilation
			Real64 const MixVolume, // Mixing volume of Air {m3}
			Real64 const MixMass // Mixing mass of air {kg}
		) :
			MeanAirTemp( MeanAirTemp ),
			OperativeTemp( OperativeTemp ),
			InfilHeatGain( InfilHeatGain ),
			InfilHeatLoss( InfilHeatLoss ),
			InfilVolume( InfilVolume ),
			InfilMass( InfilMass ),
			InfilAirChangeRate( InfilAirChangeRate ),
			VentilHeatLoss( VentilHeatLoss ),
			VentilHeatGain( VentilHeatGain ),
			VentilVolume( VentilVolume ),
			VentilMass( VentilMass ),
			VentilFanElec( VentilFanElec ),
			VentilAirTemp( VentilAirTemp ),
			MixVolume( MixVolume ),
			MixMass( MixMass )
		{}

	};

	// Object Data
	extern Array1D< AirflowNetworkReportVars > AirflowNetworkZnRpt;

	// Functions

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

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // AirflowNetworkBalanceManager

} // EnergyPlus

#endif
