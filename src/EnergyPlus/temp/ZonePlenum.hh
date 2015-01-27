#ifndef ZonePlenum_hh_INCLUDED
#define ZonePlenum_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ZonePlenum {

	// Using/Aliasing

	// Data
	// DERIVED TYPE DEFINITIONS

	extern int NumZonePlenums; // The Number of ZonePlenums found in the Input
	extern int NumZoneReturnPlenums; // The Number of ZoneReturnPlenums found in the Input
	extern int NumZoneSupplyPlenums; // The Number of ZoneSupplyPlenums found in the Input
	extern FArray1D_bool CheckRetEquipName;
	extern FArray1D_bool CheckSupEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE ZONEPLENUM

	// Types

	struct ZoneReturnPlenumConditions
	{
		// Members
		std::string ZonePlenumName;
		std::string ZoneName;
		std::string ZoneNodeName;
		Real64 ZoneTemp;
		Real64 ZoneHumRat;
		Real64 ZoneEnthalpy;
		Real64 OutletTemp;
		Real64 OutletHumRat;
		Real64 OutletEnthalpy;
		Real64 OutletPressure;
		int ZoneNodeNum;
		int ActualZoneNum;
		int OutletNode;
		Real64 OutletMassFlowRate; // MassFlow through the ZonePlenum being Simulated [kg/Sec]
		Real64 OutletMassFlowRateMaxAvail; // [kg/Sec]
		Real64 OutletMassFlowRateMinAvail; // [kg/Sec]
		int NumInducedNodes;
		FArray1D_int InducedNode;
		FArray1D< Real64 > InducedMassFlowRate;
		FArray1D< Real64 > InducedMassFlowRateMaxAvail;
		FArray1D< Real64 > InducedMassFlowRateMinAvail;
		FArray1D< Real64 > InducedTemp;
		FArray1D< Real64 > InducedHumRat;
		FArray1D< Real64 > InducedEnthalpy;
		FArray1D< Real64 > InducedPressure;
		bool InitFlag;
		int NumInletNodes;
		FArray1D_int InletNode;
		FArray1D< Real64 > InletMassFlowRate;
		FArray1D< Real64 > InletMassFlowRateMaxAvail;
		FArray1D< Real64 > InletMassFlowRateMinAvail;
		FArray1D< Real64 > InletTemp;
		FArray1D< Real64 > InletHumRat;
		FArray1D< Real64 > InletEnthalpy;
		FArray1D< Real64 > InletPressure;
		FArray1D_int ADUIndex; // index to AirDistUnit leaking to this plenum
		int NumADUs; // number of ADU's that can leak to this plenum
		FArray1D_int ZoneEqNum; // list of zone equip config indices for this plenum

		// Default Constructor
		ZoneReturnPlenumConditions() :
			ZoneTemp( 0.0 ),
			ZoneHumRat( 0.0 ),
			ZoneEnthalpy( 0.0 ),
			OutletTemp( 0.0 ),
			OutletHumRat( 0.0 ),
			OutletEnthalpy( 0.0 ),
			OutletPressure( 0.0 ),
			ZoneNodeNum( 0 ),
			ActualZoneNum( 0 ),
			OutletNode( 0 ),
			OutletMassFlowRate( 0.0 ),
			OutletMassFlowRateMaxAvail( 0.0 ),
			OutletMassFlowRateMinAvail( 0.0 ),
			NumInducedNodes( 0 ),
			InitFlag( false ),
			NumInletNodes( 0 )
		{}

		// Member Constructor
		ZoneReturnPlenumConditions(
			std::string const & ZonePlenumName,
			std::string const & ZoneName,
			std::string const & ZoneNodeName,
			Real64 const ZoneTemp,
			Real64 const ZoneHumRat,
			Real64 const ZoneEnthalpy,
			Real64 const OutletTemp,
			Real64 const OutletHumRat,
			Real64 const OutletEnthalpy,
			Real64 const OutletPressure,
			int const ZoneNodeNum,
			int const ActualZoneNum,
			int const OutletNode,
			Real64 const OutletMassFlowRate, // MassFlow through the ZonePlenum being Simulated [kg/Sec]
			Real64 const OutletMassFlowRateMaxAvail, // [kg/Sec]
			Real64 const OutletMassFlowRateMinAvail, // [kg/Sec]
			int const NumInducedNodes,
			FArray1_int const & InducedNode,
			FArray1< Real64 > const & InducedMassFlowRate,
			FArray1< Real64 > const & InducedMassFlowRateMaxAvail,
			FArray1< Real64 > const & InducedMassFlowRateMinAvail,
			FArray1< Real64 > const & InducedTemp,
			FArray1< Real64 > const & InducedHumRat,
			FArray1< Real64 > const & InducedEnthalpy,
			FArray1< Real64 > const & InducedPressure,
			bool const InitFlag,
			int const NumInletNodes,
			FArray1_int const & InletNode,
			FArray1< Real64 > const & InletMassFlowRate,
			FArray1< Real64 > const & InletMassFlowRateMaxAvail,
			FArray1< Real64 > const & InletMassFlowRateMinAvail,
			FArray1< Real64 > const & InletTemp,
			FArray1< Real64 > const & InletHumRat,
			FArray1< Real64 > const & InletEnthalpy,
			FArray1< Real64 > const & InletPressure,
			FArray1_int const & ADUIndex, // index to AirDistUnit leaking to this plenum
			int const NumADUs, // number of ADU's that can leak to this plenum
			FArray1_int const & ZoneEqNum // list of zone equip config indices for this plenum
		) :
			ZonePlenumName( ZonePlenumName ),
			ZoneName( ZoneName ),
			ZoneNodeName( ZoneNodeName ),
			ZoneTemp( ZoneTemp ),
			ZoneHumRat( ZoneHumRat ),
			ZoneEnthalpy( ZoneEnthalpy ),
			OutletTemp( OutletTemp ),
			OutletHumRat( OutletHumRat ),
			OutletEnthalpy( OutletEnthalpy ),
			OutletPressure( OutletPressure ),
			ZoneNodeNum( ZoneNodeNum ),
			ActualZoneNum( ActualZoneNum ),
			OutletNode( OutletNode ),
			OutletMassFlowRate( OutletMassFlowRate ),
			OutletMassFlowRateMaxAvail( OutletMassFlowRateMaxAvail ),
			OutletMassFlowRateMinAvail( OutletMassFlowRateMinAvail ),
			NumInducedNodes( NumInducedNodes ),
			InducedNode( InducedNode ),
			InducedMassFlowRate( InducedMassFlowRate ),
			InducedMassFlowRateMaxAvail( InducedMassFlowRateMaxAvail ),
			InducedMassFlowRateMinAvail( InducedMassFlowRateMinAvail ),
			InducedTemp( InducedTemp ),
			InducedHumRat( InducedHumRat ),
			InducedEnthalpy( InducedEnthalpy ),
			InducedPressure( InducedPressure ),
			InitFlag( InitFlag ),
			NumInletNodes( NumInletNodes ),
			InletNode( InletNode ),
			InletMassFlowRate( InletMassFlowRate ),
			InletMassFlowRateMaxAvail( InletMassFlowRateMaxAvail ),
			InletMassFlowRateMinAvail( InletMassFlowRateMinAvail ),
			InletTemp( InletTemp ),
			InletHumRat( InletHumRat ),
			InletEnthalpy( InletEnthalpy ),
			InletPressure( InletPressure ),
			ADUIndex( ADUIndex ),
			NumADUs( NumADUs ),
			ZoneEqNum( ZoneEqNum )
		{}

	};

	struct ZoneSupplyPlenumConditions
	{
		// Members
		std::string ZonePlenumName;
		std::string ZoneName;
		std::string ZoneNodeName;
		Real64 ZoneTemp;
		Real64 ZoneHumRat;
		Real64 ZoneEnthalpy;
		Real64 InletTemp;
		Real64 InletHumRat;
		Real64 InletEnthalpy;
		Real64 InletPressure;
		int ZoneNodeNum;
		int ActualZoneNum;
		int InletNode;
		Real64 InletMassFlowRate; // MassFlow through the ZonePlenum being Simulated [kg/Sec]
		Real64 InletMassFlowRateMaxAvail; // [kg/Sec]
		Real64 InletMassFlowRateMinAvail; // [kg/Sec]
		bool InitFlag;
		int NumOutletNodes;
		FArray1D_int OutletNode;
		FArray1D< Real64 > OutletMassFlowRate;
		FArray1D< Real64 > OutletMassFlowRateMaxAvail;
		FArray1D< Real64 > OutletMassFlowRateMinAvail;
		FArray1D< Real64 > OutletTemp;
		FArray1D< Real64 > OutletHumRat;
		FArray1D< Real64 > OutletEnthalpy;
		FArray1D< Real64 > OutletPressure;

		// Default Constructor
		ZoneSupplyPlenumConditions() :
			ZoneTemp( 0.0 ),
			ZoneHumRat( 0.0 ),
			ZoneEnthalpy( 0.0 ),
			InletTemp( 0.0 ),
			InletHumRat( 0.0 ),
			InletEnthalpy( 0.0 ),
			InletPressure( 0.0 ),
			ZoneNodeNum( 0 ),
			ActualZoneNum( 0 ),
			InletNode( 0 ),
			InletMassFlowRate( 0.0 ),
			InletMassFlowRateMaxAvail( 0.0 ),
			InletMassFlowRateMinAvail( 0.0 ),
			InitFlag( false ),
			NumOutletNodes( 0 )
		{}

		// Member Constructor
		ZoneSupplyPlenumConditions(
			std::string const & ZonePlenumName,
			std::string const & ZoneName,
			std::string const & ZoneNodeName,
			Real64 const ZoneTemp,
			Real64 const ZoneHumRat,
			Real64 const ZoneEnthalpy,
			Real64 const InletTemp,
			Real64 const InletHumRat,
			Real64 const InletEnthalpy,
			Real64 const InletPressure,
			int const ZoneNodeNum,
			int const ActualZoneNum,
			int const InletNode,
			Real64 const InletMassFlowRate, // MassFlow through the ZonePlenum being Simulated [kg/Sec]
			Real64 const InletMassFlowRateMaxAvail, // [kg/Sec]
			Real64 const InletMassFlowRateMinAvail, // [kg/Sec]
			bool const InitFlag,
			int const NumOutletNodes,
			FArray1_int const & OutletNode,
			FArray1< Real64 > const & OutletMassFlowRate,
			FArray1< Real64 > const & OutletMassFlowRateMaxAvail,
			FArray1< Real64 > const & OutletMassFlowRateMinAvail,
			FArray1< Real64 > const & OutletTemp,
			FArray1< Real64 > const & OutletHumRat,
			FArray1< Real64 > const & OutletEnthalpy,
			FArray1< Real64 > const & OutletPressure
		) :
			ZonePlenumName( ZonePlenumName ),
			ZoneName( ZoneName ),
			ZoneNodeName( ZoneNodeName ),
			ZoneTemp( ZoneTemp ),
			ZoneHumRat( ZoneHumRat ),
			ZoneEnthalpy( ZoneEnthalpy ),
			InletTemp( InletTemp ),
			InletHumRat( InletHumRat ),
			InletEnthalpy( InletEnthalpy ),
			InletPressure( InletPressure ),
			ZoneNodeNum( ZoneNodeNum ),
			ActualZoneNum( ActualZoneNum ),
			InletNode( InletNode ),
			InletMassFlowRate( InletMassFlowRate ),
			InletMassFlowRateMaxAvail( InletMassFlowRateMaxAvail ),
			InletMassFlowRateMinAvail( InletMassFlowRateMinAvail ),
			InitFlag( InitFlag ),
			NumOutletNodes( NumOutletNodes ),
			OutletNode( OutletNode ),
			OutletMassFlowRate( OutletMassFlowRate ),
			OutletMassFlowRateMaxAvail( OutletMassFlowRateMaxAvail ),
			OutletMassFlowRateMinAvail( OutletMassFlowRateMinAvail ),
			OutletTemp( OutletTemp ),
			OutletHumRat( OutletHumRat ),
			OutletEnthalpy( OutletEnthalpy ),
			OutletPressure( OutletPressure )
		{}

	};

	// Object Data
	extern FArray1D< ZoneReturnPlenumConditions > ZoneRetPlenCond;
	extern FArray1D< ZoneSupplyPlenumConditions > ZoneSupPlenCond;

	// Functions

	void
	SimAirZonePlenum(
		std::string const & CompName,
		int const iCompType,
		int & CompIndex,
		Optional_bool_const FirstHVACIteration = _, //Autodesk:OPTIONAL Used without PRESENT check
		Optional_bool_const FirstCall = _, //Autodesk:OPTIONAL Used without PRESENT check
		Optional_bool PlenumInletChanged = _ //Autodesk:OPTIONAL Used without PRESENT check
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetZonePlenumInput();

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitAirZoneReturnPlenum( int const ZonePlenumNum );

	void
	InitAirZoneSupplyPlenum(
		int const ZonePlenumNum,
		bool const FirstHVACIteration,
		bool const FirstCall
	);

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcAirZoneReturnPlenum( int const ZonePlenumNum );

	void
	CalcAirZoneSupplyPlenum(
		int const ZonePlenumNum,
		bool const FirstCall
	);

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the ZonePlenum Module
	// *****************************************************************************

	void
	UpdateAirZoneReturnPlenum( int const ZonePlenumNum );

	void
	UpdateAirZoneSupplyPlenum(
		int const ZonePlenumNum,
		bool & PlenumInletChanged,
		bool const FirstCall
	);

	//        End of Update subroutines for the ZonePlenum Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the ZonePlenum Module
	// *****************************************************************************

	void
	ReportZoneReturnPlenum( int const ZonePlenumNum ); // unused1208

	void
	ReportZoneSupplyPlenum( int const ZonePlenumNum ); // unused1208

	//        End of Reporting subroutines for the ZonePlenum Module
	// *****************************************************************************

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

} // ZonePlenum

} // EnergyPlus

#endif
