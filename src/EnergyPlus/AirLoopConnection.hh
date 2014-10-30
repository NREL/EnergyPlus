#ifndef AirLoopConnection_hh_INCLUDED
#define AirLoopConnection_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace AirLoopConnection {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na
	
	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	// Standard, run-of-the-mill variables...
	extern int NumOfAirLoopConnections; // Number of air loop connections present in input

	// SUBROUTINE SPECIFICATIONS FOR MODULE AirLoopConnection

	// Types

	struct AirLoopConnectionData
	{
		// Members
		// Input data
		std::string Name; // name of the air loop connection (could be zone name, but not really used for anything)
		int AirLoopInletNodeNum; // inlet node number from the air loop
		int AirLoopOutletNodeNum; // outlet node number to the air loop
		int ZoneInletNodeNum; // inlet node number to the zone
		int ZoneOutletNodeNum; // outlet node number from the zone
		// Report Variables
		Real64 SenHeatRate; // sensible heating rate by the air loop connection [W]
		Real64 LatHeatRate; // latent heating rate by the air loop connection [W]
		Real64 TotHeatRate; // total heating rate by the air loop connection [W]
		Real64 SenHeatEnergy; // sensible heating rate by the air loop connection [J]
		Real64 LatHeatEnergy; // latent heating rate by the air loop connection [J]
		Real64 TotHeatEnergy; // total heating rate by the air loop connection [J]
		Real64 SenCoolRate; // sensible cooling rate by the air loop connection [W]
		Real64 LatCoolRate; // latent cooling rate by the air loop connection [W]
		Real64 TotCoolRate; // total cooling rate by the air loop connection [W]
		Real64 SenCoolEnergy; // sensible cooling rate by the air loop connection [J]
		Real64 LatCoolEnergy; // latent cooling rate by the air loop connection [J]
		Real64 TotCoolEnergy; // total cooling rate by the air loop connection [J]
		Real64 MassFlowRate; // air mass flow rate through the air loop connection [kg/s]
		Real64 InletAirTemp; // air temperature entering the air loop connection [C]
		Real64 OutletAirTemp; // air temperature leaving the air loop connection [C]
		Real64 InletHumRat; // inlet air humidity ratio [kg/kg]
		Real64 OutletHumRat; // outlet air humidity ratio [kg/kg]

		// Default Constructor
		AirLoopConnectionData() :
			AirLoopInletNodeNum( 0 ),
			AirLoopOutletNodeNum( 0 ),
			ZoneInletNodeNum( 0 ),
			ZoneOutletNodeNum( 0 ),
			SenHeatRate( 0.0 ),
			LatHeatRate( 0.0 ),
			TotHeatRate( 0.0 ),
			SenHeatEnergy( 0.0 ),
			LatHeatEnergy( 0.0 ),
			TotHeatEnergy( 0.0 ),
			SenCoolRate( 0.0 ),
			LatCoolRate( 0.0 ),
			TotCoolRate( 0.0 ),
			SenCoolEnergy( 0.0 ),
			LatCoolEnergy( 0.0 ),
			TotCoolEnergy( 0.0 ),
			MassFlowRate( 0.0 ),
			InletAirTemp( 0.0 ),
			OutletAirTemp( 0.0 ),
			InletHumRat( 0.0 ),
			OutletHumRat( 0.0 )
		{}

		// Member Constructor
		AirLoopConnectionData(
			std::string const Name, // name of the air loop connection (could be zone name, but not really used for anything)
			int const AirLoopInletNodeNum, // inlet node number from the air loop
			int const AirLoopOutletNodeNum, // outlet node number to the air loop
			int const ZoneInletNodeNum, // inlet node number to the zone
			int const ZoneOutletNodeNum, // outlet node number from the zone
			// Report Variables
			Real64 const SenHeatRate, // sensible heating rate by the air loop connection [W]
			Real64 const LatHeatRate, // latent heating rate by the air loop connection [W]
			Real64 const TotHeatRate, // total heating rate by the air loop connection [W]
			Real64 const SenHeatEnergy, // sensible heating rate by the air loop connection [J]
			Real64 const LatHeatEnergy, // latent heating rate by the air loop connection [J]
			Real64 const TotHeatEnergy, // total heating rate by the air loop connection [J]
			Real64 const SenCoolRate, // sensible cooling rate by the air loop connection [W]
			Real64 const LatCoolRate, // latent cooling rate by the air loop connection [W]
			Real64 const TotCoolRate, // total cooling rate by the air loop connection [W]
			Real64 const SenCoolEnergy, // sensible cooling rate by the air loop connection [J]
			Real64 const LatCoolEnergy, // latent cooling rate by the air loop connection [J]
			Real64 const TotCoolEnergy, // total cooling rate by the air loop connection [J]
			Real64 const MassFlowRate, // air mass flow rate through the air loop connection [kg/s]
			Real64 const InletAirTemp, // air temperature entering the air loop connection [C]
			Real64 const OutletAirTemp, // air temperature leaving the air loop connection [C]
			Real64 const InletHumRat, // inlet air humidity ratio [kg/kg]
			Real64 const OutletHumRat // outlet air humidity ratio [kg/kg]
		) :
			Name( Name ),
			AirLoopInletNodeNum( AirLoopInletNodeNum ),
			AirLoopOutletNodeNum( AirLoopOutletNodeNum ),
			ZoneInletNodeNum( ZoneInletNodeNum ),
			ZoneOutletNodeNum( ZoneOutletNodeNum ),
			SenHeatRate( SenHeatRate ),
			LatHeatRate( 0.0 ),
			TotHeatRate( LatHeatRate ),
			SenHeatEnergy( SenHeatEnergy ),
			LatHeatEnergy( LatHeatEnergy ),
			TotHeatEnergy( TotHeatEnergy ),
			SenCoolRate( SenCoolRate ),
			LatCoolRate( LatCoolRate ),
			TotCoolRate( TotCoolRate ),
			SenCoolEnergy( SenCoolEnergy ),
			LatCoolEnergy( LatCoolEnergy ),
			TotCoolEnergy( TotCoolEnergy ),
			MassFlowRate( MassFlowRate ),
			InletAirTemp( InletAirTemp ),
			OutletAirTemp( OutletAirTemp ),
			InletHumRat( InletHumRat ),
			OutletHumRat( OutletHumRat )
		{}
	};

	// Object Data
	extern FArray1D< AirLoopConnectionData > AirLoopCon;

	// Functions

	void
	SimAirLoopConnection(
		std::string const & CompName, // name of the low temperature radiant system
		int CompIndex,
		bool const & FirstCall
	);

	void
	GetAirLoopConnection( );

	void
	InitAirLoopConnection(
		int const & CompIndex
	);

	void
	CalcAirLoopConnection(
		int const & CompIndex,
		bool const & FirstCall
	);

	void
	UpdateAirLoopConnection(
		int const & CompIndex
	);


	void
	ReportAirLoopConnection(
		int const & CompIndex
	);

	void
	GetAirLoopConnectionNum(
		int & ConnectNum, // index number in air loop connection derived type
		std::string const CompName // name of air loop connection passed in
	);

	void
	GetAirLoopConnectionNodeNums(
		int const ConnectNum, // index number in air loop connection derived type
		int & InletNode, // inlet node for component
		int & OutletNode // outlet node for component
	);

	void
	FoundAirLoopConnection(
		int const InletNodeNum, // index number in air loop connection derived type
		bool & FoundAMatch // set to true if we find a air loop connection that has this inlet node number
	);

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

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

} // AirLoopConnection

} // EnergyPlus

#endif
