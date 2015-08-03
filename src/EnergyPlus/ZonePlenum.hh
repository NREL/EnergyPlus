#ifndef ZonePlenum_hh_INCLUDED
#define ZonePlenum_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
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
	extern Array1D_bool CheckRetEquipName;
	extern Array1D_bool CheckSupEquipName;

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
		Array1D_int InducedNode;
		Array1D< Real64 > InducedMassFlowRate;
		Array1D< Real64 > InducedMassFlowRateMaxAvail;
		Array1D< Real64 > InducedMassFlowRateMinAvail;
		Array1D< Real64 > InducedTemp;
		Array1D< Real64 > InducedHumRat;
		Array1D< Real64 > InducedEnthalpy;
		Array1D< Real64 > InducedPressure;
		Array1D< Real64 > InducedCO2;
		Array1D< Real64 > InducedGenContam;
		bool InitFlag;
		int NumInletNodes;
		Array1D_int InletNode;
		Array1D< Real64 > InletMassFlowRate;
		Array1D< Real64 > InletMassFlowRateMaxAvail;
		Array1D< Real64 > InletMassFlowRateMinAvail;
		Array1D< Real64 > InletTemp;
		Array1D< Real64 > InletHumRat;
		Array1D< Real64 > InletEnthalpy;
		Array1D< Real64 > InletPressure;
		Array1D_int ADUIndex; // index to AirDistUnit leaking to this plenum
		int NumADUs; // number of ADU's that can leak to this plenum
		Array1D_int ZoneEqNum; // list of zone equip config indices for this plenum

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
			Array1_int const & InducedNode,
			Array1< Real64 > const & InducedMassFlowRate,
			Array1< Real64 > const & InducedMassFlowRateMaxAvail,
			Array1< Real64 > const & InducedMassFlowRateMinAvail,
			Array1< Real64 > const & InducedTemp,
			Array1< Real64 > const & InducedHumRat,
			Array1< Real64 > const & InducedEnthalpy,
			Array1< Real64 > const & InducedPressure,
			Array1< Real64 > const & InducedCO2,
			Array1< Real64 > const & InducedGenContam,
			bool const InitFlag,
			int const NumInletNodes,
			Array1_int const & InletNode,
			Array1< Real64 > const & InletMassFlowRate,
			Array1< Real64 > const & InletMassFlowRateMaxAvail,
			Array1< Real64 > const & InletMassFlowRateMinAvail,
			Array1< Real64 > const & InletTemp,
			Array1< Real64 > const & InletHumRat,
			Array1< Real64 > const & InletEnthalpy,
			Array1< Real64 > const & InletPressure,
			Array1_int const & ADUIndex, // index to AirDistUnit leaking to this plenum
			int const NumADUs, // number of ADU's that can leak to this plenum
			Array1_int const & ZoneEqNum // list of zone equip config indices for this plenum
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
			InducedCO2( InducedCO2 ),
			InducedGenContam( InducedGenContam ),
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
		Array1D_int OutletNode;
		Array1D< Real64 > OutletMassFlowRate;
		Array1D< Real64 > OutletMassFlowRateMaxAvail;
		Array1D< Real64 > OutletMassFlowRateMinAvail;
		Array1D< Real64 > OutletTemp;
		Array1D< Real64 > OutletHumRat;
		Array1D< Real64 > OutletEnthalpy;
		Array1D< Real64 > OutletPressure;

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
			Array1_int const & OutletNode,
			Array1< Real64 > const & OutletMassFlowRate,
			Array1< Real64 > const & OutletMassFlowRateMaxAvail,
			Array1< Real64 > const & OutletMassFlowRateMinAvail,
			Array1< Real64 > const & OutletTemp,
			Array1< Real64 > const & OutletHumRat,
			Array1< Real64 > const & OutletEnthalpy,
			Array1< Real64 > const & OutletPressure
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
	extern Array1D< ZoneReturnPlenumConditions > ZoneRetPlenCond;
	extern Array1D< ZoneSupplyPlenumConditions > ZoneSupPlenCond;

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

} // ZonePlenum

} // EnergyPlus

#endif
