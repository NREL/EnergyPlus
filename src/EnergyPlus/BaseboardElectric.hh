#ifndef BaseboardElectric_hh_INCLUDED
#define BaseboardElectric_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace BaseboardElectric {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern std::string const cCMO_BBRadiator_Electric;
	extern Real64 const SimpConvAirFlowSpeed; // m/s

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumBaseboards;
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;

	//SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

	// Types

	struct BaseboardParams
	{
		// Members
		std::string EquipName;
		std::string EquipType;
		std::string Schedule;
		int SchedPtr;
		Real64 NominalCapacity;
		Real64 BaseboardEfficiency;
		Real64 AirInletTemp;
		Real64 AirInletHumRat;
		Real64 AirOutletTemp;
		Real64 Power;
		Real64 Energy;
		Real64 ElecUseLoad;
		Real64 ElecUseRate;
		int ZonePtr; // point to teh zone where the basebaord is located
		int HeatingCapMethod; // - Method for heating capacity scaledsizing calculation- (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 ScaledHeatingCapacity; // - scaled maximum heating capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}

		// Default Constructor
		BaseboardParams() :
			SchedPtr( 0 ),
			NominalCapacity( 0.0 ),
			BaseboardEfficiency( 0.0 ),
			AirInletTemp( 0.0 ),
			AirInletHumRat( 0.0 ),
			AirOutletTemp( 0.0 ),
			Power( 0.0 ),
			Energy( 0.0 ),
			ElecUseLoad( 0.0 ),
			ElecUseRate( 0.0 ),
			ZonePtr( 0 ),
			HeatingCapMethod( 0.0 ),
			ScaledHeatingCapacity( 0.0 )
		{}

		// Member Constructor
		BaseboardParams(
			std::string const & EquipName,
			std::string const & EquipType,
			std::string const & Schedule,
			int const SchedPtr,
			Real64 const NominalCapacity,
			Real64 const BaseboardEfficiency,
			Real64 const AirInletTemp,
			Real64 const AirInletHumRat,
			Real64 const AirOutletTemp,
			Real64 const Power,
			Real64 const Energy,
			Real64 const ElecUseLoad,
			Real64 const ElecUseRate,
			int const ZonePtr, // point to teh zone where the electric baseboard is located
			int const HeatingCapMethod, // - Method for electric baseboard heating capacity scalable sizing calculation
			Real64 const ScaledHeatingCapacity // - electric baseboard scaled maximum heating capacity {W} or scalable variable for sizing in {-}, or {W/m2}
		) :
			EquipName( EquipName ),
			EquipType( EquipType ),
			Schedule( Schedule ),
			SchedPtr( SchedPtr ),
			NominalCapacity( NominalCapacity ),
			BaseboardEfficiency( BaseboardEfficiency ),
			AirInletTemp( AirInletTemp ),
			AirInletHumRat( AirInletHumRat ),
			AirOutletTemp( AirOutletTemp ),
			Power( Power ),
			Energy( Energy ),
			ElecUseLoad( ElecUseLoad ),
			ElecUseRate( ElecUseRate ),
			ZonePtr( ZonePtr ),
			HeatingCapMethod( HeatingCapMethod ),
			ScaledHeatingCapacity( ScaledHeatingCapacity )
		{}

	};

	struct BaseboardNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		BaseboardNumericFieldData()
		{}

		// Member Constructor
		BaseboardNumericFieldData(
			Array1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
			) :
			FieldNames(FieldNames)
		{}
	};

	// Object Data
	extern Array1D< BaseboardParams > Baseboard;
	extern Array1D< BaseboardNumericFieldData > BaseboardNumericFields;

	// Functions

	void
	SimElectricBaseboard(
		std::string const & EquipName,
		int const ActualZoneNum,
		int const ControlledZoneNum,
		Real64 & PowerMet,
		int & CompIndex
	);

	void
	GetBaseboardInput();

	void
	InitBaseboard(
		int const BaseboardNum,
		int const ControlledZoneNum
	);

	void
	SizeElectricBaseboard( int const BaseboardNum );

	void
	SimElectricConvective(
		int const BaseboardNum,
		Real64 const LoadMet
	);

	void
	ReportBaseboard( int const BaseboardNum );

} // BaseboardElectric

} // EnergyPlus

#endif
