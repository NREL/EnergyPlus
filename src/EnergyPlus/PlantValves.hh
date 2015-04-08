#ifndef PlantValves_hh_INCLUDED
#define PlantValves_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace PlantValves {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumTemperingValves;
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

	// Types

	struct TemperValveData
	{
		// Members
		// user input data
		std::string Name; // User identifier
		int PltInletNodeNum; // Node number on the inlet side of the plant
		int PltOutletNodeNum; // Node number on the outlet side of the plant
		int PltStream2NodeNum; // Node number on the outlet side of the second stream
		int PltSetPointNodeNum; // Node number for the setpoint node.
		int PltPumpOutletNodeNum; // node number for the pump outlet (for flow rate)
		// Calculated and from elsewhere
		bool Init; // flag for initializationL true means do the initializations
		Real64 FlowDivFract; // Fraction of flow sent down diversion path
		Real64 Stream2SourceTemp; // Temperature [C] of stream 2 being mixed
		Real64 InletTemp; // Temperature [C] of inlet to valve
		Real64 SetPointTemp; // setpoint Temperatures [C] at control node.
		Real64 MixedMassFlowRate; // Flow rate downstream of mixer [kg/s]
		Real64 DivertedFlowRate; // flow rate through tempering valve's diversion path [kg/s]
		//loop topology variables
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;

		// Default Constructor
		TemperValveData() :
			PltInletNodeNum( 0 ),
			PltOutletNodeNum( 0 ),
			PltStream2NodeNum( 0 ),
			PltSetPointNodeNum( 0 ),
			PltPumpOutletNodeNum( 0 ),
			Init( true ),
			FlowDivFract( 0.0 ),
			Stream2SourceTemp( 0.0 ),
			InletTemp( 0.0 ),
			SetPointTemp( 0.0 ),
			MixedMassFlowRate( 0.0 ),
			DivertedFlowRate( 0.0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 )
		{}

		// Member Constructor
		TemperValveData(
			std::string const & Name, // User identifier
			int const PltInletNodeNum, // Node number on the inlet side of the plant
			int const PltOutletNodeNum, // Node number on the outlet side of the plant
			int const PltStream2NodeNum, // Node number on the outlet side of the second stream
			int const PltSetPointNodeNum, // Node number for the setpoint node.
			int const PltPumpOutletNodeNum, // node number for the pump outlet (for flow rate)
			bool const Init, // flag for initializationL true means do the initializations
			Real64 const FlowDivFract, // Fraction of flow sent down diversion path
			Real64 const Stream2SourceTemp, // Temperature [C] of stream 2 being mixed
			Real64 const InletTemp, // Temperature [C] of inlet to valve
			Real64 const SetPointTemp, // setpoint Temperatures [C] at control node.
			Real64 const MixedMassFlowRate, // Flow rate downstream of mixer [kg/s]
			Real64 const DivertedFlowRate, // flow rate through tempering valve's diversion path [kg/s]
			int const LoopNum,
			int const LoopSideNum,
			int const BranchNum,
			int const CompNum
		) :
			Name( Name ),
			PltInletNodeNum( PltInletNodeNum ),
			PltOutletNodeNum( PltOutletNodeNum ),
			PltStream2NodeNum( PltStream2NodeNum ),
			PltSetPointNodeNum( PltSetPointNodeNum ),
			PltPumpOutletNodeNum( PltPumpOutletNodeNum ),
			Init( Init ),
			FlowDivFract( FlowDivFract ),
			Stream2SourceTemp( Stream2SourceTemp ),
			InletTemp( InletTemp ),
			SetPointTemp( SetPointTemp ),
			MixedMassFlowRate( MixedMassFlowRate ),
			DivertedFlowRate( DivertedFlowRate ),
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum )
		{}

	};

	// Object Data
	extern Array1D< TemperValveData > TemperValve; // dimension to No. of TemperingValve objects

	// Functions

	void
	SimPlantValves(
		int const CompTypeNum,
		std::string const & CompName,
		int & CompNum,
		bool const RunFlag, // unused1208
		bool & InitLoopEquip,
		Real64 & MyLoad, // unused1208
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration // TRUE if First iteration of simulation
	);

	void
	GetPlantValvesInput();

	void
	InitPlantValves(
		int const CompTypeNum,
		int const CompNum
	);

	void
	CalcPlantValves(
		int const CompTypeNum,
		int const CompNum
	);

	void
	UpdatePlantValves(
		int const CompTypeNum,
		int const CompNum
	);

	void
	ReportPlantValves();

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // PlantValves

} // EnergyPlus

#endif
