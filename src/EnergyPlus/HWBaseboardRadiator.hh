#ifndef HWBaseboardRadiator_hh_INCLUDED
#define HWBaseboardRadiator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HWBaseboardRadiator {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS

	extern std::string const cCMO_BBRadiator_Water;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumHWBaseboards;
	extern Array1D< Real64 > QBBRadSource; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > QBBRadSrcAvg; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone with no source

	// Record keeping variables used to calculate QBBRadSrcAvg locally
	extern Array1D< Real64 > LastQBBRadSrc; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;
	extern Array1D_bool SetLoopIndexFlag; // get loop number flag

	//SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

	// Types

	struct HWBaseboardParams
	{
		// Members
		std::string EquipID;
		int EquipType;
		std::string Schedule;
		Array1D_string SurfaceName;
		Array1D_int SurfacePtr;
		int ZonePtr;
		int SchedPtr;
		int WaterInletNode;
		int WaterOutletNode;
		int TotSurfToDistrib;
		int ControlCompTypeNum;
		int CompErrIndex;
		Real64 AirMassFlowRate;
		Real64 AirMassFlowRateStd;
		Real64 WaterTempAvg;
		Real64 RatedCapacity;
		Real64 UA;
		Real64 Offset;
		Real64 WaterMassFlowRate;
		Real64 WaterMassFlowRateMax;
		Real64 WaterMassFlowRateStd;
		Real64 WaterVolFlowRateMax;
		Real64 WaterInletTempStd;
		Real64 WaterInletTemp;
		Real64 WaterInletEnthalpy;
		Real64 WaterOutletTempStd;
		Real64 WaterOutletTemp;
		Real64 WaterOutletEnthalpy;
		Real64 AirInletTempStd;
		Real64 AirInletTemp;
		Real64 AirOutletTemp;
		Real64 AirInletHumRat;
		Real64 AirOutletTempStd;
		Real64 FracRadiant;
		Real64 FracConvect;
		Real64 FracDistribPerson;
		Array1D< Real64 > FracDistribToSurf;
		Real64 TotPower;
		Real64 Power;
		Real64 ConvPower;
		Real64 RadPower;
		Real64 TotEnergy;
		Real64 Energy;
		Real64 ConvEnergy;
		Real64 RadEnergy;
		int LoopNum; // plant loop index
		int LoopSideNum; // plant loop side index
		int BranchNum; // plant loop branch index
		int CompNum; // plant loop component index
		int BBLoadReSimIndex;
		int BBMassFlowReSimIndex;
		int BBInletTempFlowReSimIndex;
		int HeatingCapMethod; // - Method for heating capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 ScaledHeatingCapacity; // - scaled maximum heating capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}

		// Default Constructor
		HWBaseboardParams() :
			EquipType( 0 ),
			ZonePtr( 0 ),
			SchedPtr( 0 ),
			WaterInletNode( 0 ),
			WaterOutletNode( 0 ),
			TotSurfToDistrib( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			AirMassFlowRate( 0.0 ),
			AirMassFlowRateStd( 0.0 ),
			WaterTempAvg( 0.0 ),
			RatedCapacity( 0.0 ),
			UA( 0.0 ),
			Offset( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			WaterMassFlowRateMax( 0.0 ),
			WaterMassFlowRateStd( 0.0 ),
			WaterVolFlowRateMax( 0.0 ),
			WaterInletTempStd( 0.0 ),
			WaterInletTemp( 0.0 ),
			WaterInletEnthalpy( 0.0 ),
			WaterOutletTempStd( 0.0 ),
			WaterOutletTemp( 0.0 ),
			WaterOutletEnthalpy( 0.0 ),
			AirInletTempStd( 0.0 ),
			AirInletTemp( 0.0 ),
			AirOutletTemp( 0.0 ),
			AirInletHumRat( 0.0 ),
			AirOutletTempStd( 0.0 ),
			FracRadiant( 0.0 ),
			FracConvect( 0.0 ),
			FracDistribPerson( 0.0 ),
			TotPower( 0.0 ),
			Power( 0.0 ),
			ConvPower( 0.0 ),
			RadPower( 0.0 ),
			TotEnergy( 0.0 ),
			Energy( 0.0 ),
			ConvEnergy( 0.0 ),
			RadEnergy( 0.0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			BBLoadReSimIndex( 0 ),
			BBMassFlowReSimIndex( 0 ),
			BBInletTempFlowReSimIndex( 0 ),
			HeatingCapMethod( 0 ),
			ScaledHeatingCapacity( 0.0 )
		{}

		// Member Constructor
		HWBaseboardParams(
			std::string const & EquipID,
			int const EquipType,
			std::string const & Schedule,
			Array1_string const & SurfaceName,
			Array1_int const & SurfacePtr,
			int const ZonePtr,
			int const SchedPtr,
			int const WaterInletNode,
			int const WaterOutletNode,
			int const TotSurfToDistrib,
			int const ControlCompTypeNum,
			int const CompErrIndex,
			Real64 const AirMassFlowRate,
			Real64 const AirMassFlowRateStd,
			Real64 const WaterTempAvg,
			Real64 const RatedCapacity,
			Real64 const UA,
			Real64 const Offset,
			Real64 const WaterMassFlowRate,
			Real64 const WaterMassFlowRateMax,
			Real64 const WaterMassFlowRateStd,
			Real64 const WaterVolFlowRateMax,
			Real64 const WaterInletTempStd,
			Real64 const WaterInletTemp,
			Real64 const WaterInletEnthalpy,
			Real64 const WaterOutletTempStd,
			Real64 const WaterOutletTemp,
			Real64 const WaterOutletEnthalpy,
			Real64 const AirInletTempStd,
			Real64 const AirInletTemp,
			Real64 const AirOutletTemp,
			Real64 const AirInletHumRat,
			Real64 const AirOutletTempStd,
			Real64 const FracRadiant,
			Real64 const FracConvect,
			Real64 const FracDistribPerson,
			Array1< Real64 > const & FracDistribToSurf,
			Real64 const TotPower,
			Real64 const Power,
			Real64 const ConvPower,
			Real64 const RadPower,
			Real64 const TotEnergy,
			Real64 const Energy,
			Real64 const ConvEnergy,
			Real64 const RadEnergy,
			int const LoopNum, // plant loop index
			int const LoopSideNum, // plant loop side index
			int const BranchNum, // plant loop branch index
			int const CompNum, // plant loop component index
			int const BBLoadReSimIndex,
			int const BBMassFlowReSimIndex,
			int const BBInletTempFlowReSimIndex,
			int const HeatingCapMethod, // - Method for HW baseboard heating capacity scalable sizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
			Real64 const ScaledHeatingCapacity // - HW baseboard scaled maximum heating capacity {W} or scalable variable for sizing in {-}, or {W/m2}
		) :
			EquipID( EquipID ),
			EquipType( EquipType ),
			Schedule( Schedule ),
			SurfaceName( SurfaceName ),
			SurfacePtr( SurfacePtr ),
			ZonePtr( ZonePtr ),
			SchedPtr( SchedPtr ),
			WaterInletNode( WaterInletNode ),
			WaterOutletNode( WaterOutletNode ),
			TotSurfToDistrib( TotSurfToDistrib ),
			ControlCompTypeNum( ControlCompTypeNum ),
			CompErrIndex( CompErrIndex ),
			AirMassFlowRate( AirMassFlowRate ),
			AirMassFlowRateStd( AirMassFlowRateStd ),
			WaterTempAvg( WaterTempAvg ),
			RatedCapacity( RatedCapacity ),
			UA( UA ),
			Offset( Offset ),
			WaterMassFlowRate( WaterMassFlowRate ),
			WaterMassFlowRateMax( WaterMassFlowRateMax ),
			WaterMassFlowRateStd( WaterMassFlowRateStd ),
			WaterVolFlowRateMax( WaterVolFlowRateMax ),
			WaterInletTempStd( WaterInletTempStd ),
			WaterInletTemp( WaterInletTemp ),
			WaterInletEnthalpy( WaterInletEnthalpy ),
			WaterOutletTempStd( WaterOutletTempStd ),
			WaterOutletTemp( WaterOutletTemp ),
			WaterOutletEnthalpy( WaterOutletEnthalpy ),
			AirInletTempStd( AirInletTempStd ),
			AirInletTemp( AirInletTemp ),
			AirOutletTemp( AirOutletTemp ),
			AirInletHumRat( AirInletHumRat ),
			AirOutletTempStd( AirOutletTempStd ),
			FracRadiant( FracRadiant ),
			FracConvect( FracConvect ),
			FracDistribPerson( FracDistribPerson ),
			FracDistribToSurf( FracDistribToSurf ),
			TotPower( TotPower ),
			Power( Power ),
			ConvPower( ConvPower ),
			RadPower( RadPower ),
			TotEnergy( TotEnergy ),
			Energy( Energy ),
			ConvEnergy( ConvEnergy ),
			RadEnergy( RadEnergy ),
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum ),
			BBLoadReSimIndex( BBLoadReSimIndex ),
			BBMassFlowReSimIndex( BBMassFlowReSimIndex ),
			BBInletTempFlowReSimIndex( BBInletTempFlowReSimIndex ),
			HeatingCapMethod( HeatingCapMethod ),
			ScaledHeatingCapacity( ScaledHeatingCapacity )
		{}
	};

	struct HWBaseboardNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		HWBaseboardNumericFieldData()
		{}

		// Member Constructor
		HWBaseboardNumericFieldData(
			Array1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
			) :
			FieldNames(FieldNames)
		{}
	};

	// Object Data
	extern Array1D< HWBaseboardParams > HWBaseboard;
	extern Array1D< HWBaseboardNumericFieldData > HWBaseboardNumericFields;

	// Functions

	void
	SimHWBaseboard(
		std::string const & EquipName,
		int const ActualZoneNum,
		int const ControlledZoneNum,
		bool const FirstHVACIteration,
		Real64 & PowerMet,
		int & CompIndex
	);

	void
	GetHWBaseboardInput();

	void
	InitHWBaseboard(
		int const BaseboardNum,
		int const ControlledZoneNumSub,
		bool const FirstHVACIteration
	);

	void
	SizeHWBaseboard( int const BaseboardNum );

	void
	CalcHWBaseboard(
		int & BaseboardNum,
		Real64 & LoadMet
	);

	void
	UpdateHWBaseboard( int const BaseboardNum );

	void
	UpdateBBRadSourceValAvg( bool & HWBaseboardSysOn ); // .TRUE. if the radiant system has run this zone time step

	void
	DistributeBBRadGains();

	void
	ReportHWBaseboard( int const BaseboardNum );

	Real64
	SumHATsurf( int const ZoneNum ); // Zone number

	void
	UpdateHWBaseboardPlantConnection(
		int const BaseboardTypeNum, // type index
		std::string const & BaseboardName, // component name
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int const LoopNum, // Plant loop index for where called from
		int const LoopSide, // Plant loop side index for where called from
		int & CompIndex, // Chiller number pointer
		bool const FirstHVACIteration,
		bool & InitLoopEquip // If not zero, calculate the max load for operating conditions
	);

	//*****************************************************************************************
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

} // HWBaseboardRadiator

} // EnergyPlus

#endif
