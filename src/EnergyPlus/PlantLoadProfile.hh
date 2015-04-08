#ifndef PlantLoadProfile_hh_INCLUDED
#define PlantLoadProfile_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace PlantLoadProfile {

	// Using/Aliasing

	// Data
	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumOfPlantProfile;

	// SUBROUTINE SPECIFICATIONS:

	// Types

	struct PlantProfileData
	{
		// Members
		std::string Name; // Name of Plant Load Profile object
		int TypeNum; // Plant Side Connection: 'TypeOf_Num' assigned in DataPlant  !DSU
		int WLoopNum; // water plant loop index number                      !DSU
		int WLoopSideNum; // water plant loop side index                        !DSU
		int WLoopBranchNum; // water plant loop branch index                      !DSU
		int WLoopCompNum; // water plant loop component index                   !DSU
		bool Init; // Flag for initialization:  TRUE means do the init
		bool InitSizing; // Flag for initialization of plant sizing
		int InletNode;
		Real64 InletTemp; // Inlet temperature (C)
		int OutletNode;
		Real64 OutletTemp; // Outlet temperature (C)
		int LoadSchedule; // Pointer to schedule object
		bool EMSOverridePower; // if true, then EMS is calling to override power level
		Real64 EMSPowerValue; // value EMS is directing to use for power [W]
		Real64 PeakVolFlowRate; // Peak volumetric flow rate, also water consumption rate (m3/s)
		int FlowRateFracSchedule; // Pointer to schedule object
		Real64 VolFlowRate; // Volumetric flow rate (m3/s)
		Real64 MassFlowRate; // Mass flow rate (kg/s)
		bool EMSOverrideMassFlow;
		Real64 EMSMassFlowValue;
		// Report variables
		Real64 Power; // Power required to meet the load (W)
		Real64 Energy; // Energy required to meet the load (J)
		Real64 HeatingEnergy; // Heating Energy required to meet the load (J)
		Real64 CoolingEnergy; // Cooling Energy required to meet the load (J)
		bool SetLoopIndexFlag;

		// Default Constructor
		PlantProfileData() :
			WLoopNum( 0 ),
			WLoopSideNum( 0 ),
			WLoopBranchNum( 0 ),
			WLoopCompNum( 0 ),
			Init( true ),
			InitSizing( true ),
			InletNode( 0 ),
			InletTemp( 0.0 ),
			OutletNode( 0 ),
			OutletTemp( 0.0 ),
			LoadSchedule( 0 ),
			EMSOverridePower( false ),
			EMSPowerValue( 0.0 ),
			PeakVolFlowRate( 0.0 ),
			FlowRateFracSchedule( 0 ),
			VolFlowRate( 0.0 ),
			MassFlowRate( 0.0 ),
			EMSOverrideMassFlow( false ),
			EMSMassFlowValue( 0.0 ),
			Power( 0.0 ),
			Energy( 0.0 ),
			HeatingEnergy( 0.0 ),
			CoolingEnergy( 0.0 ),
			SetLoopIndexFlag( true )
		{}

		// Member Constructor
		PlantProfileData(
			std::string const & Name, // Name of Plant Load Profile object
			int const TypeNum, // Plant Side Connection: 'TypeOf_Num' assigned in DataPlant  !DSU
			int const WLoopNum, // water plant loop index number                      !DSU
			int const WLoopSideNum, // water plant loop side index                        !DSU
			int const WLoopBranchNum, // water plant loop branch index                      !DSU
			int const WLoopCompNum, // water plant loop component index                   !DSU
			bool const Init, // Flag for initialization:  TRUE means do the init
			bool const InitSizing, // Flag for initialization of plant sizing
			int const InletNode,
			Real64 const InletTemp, // Inlet temperature (C)
			int const OutletNode,
			Real64 const OutletTemp, // Outlet temperature (C)
			int const LoadSchedule, // Pointer to schedule object
			bool const EMSOverridePower, // if true, then EMS is calling to override power level
			Real64 const EMSPowerValue, // value EMS is directing to use for power [W]
			Real64 const PeakVolFlowRate, // Peak volumetric flow rate, also water consumption rate (m3/s)
			int const FlowRateFracSchedule, // Pointer to schedule object
			Real64 const VolFlowRate, // Volumetric flow rate (m3/s)
			Real64 const MassFlowRate, // Mass flow rate (kg/s)
			bool const EMSOverrideMassFlow,
			Real64 const EMSMassFlowValue,
			Real64 const Power, // Power required to meet the load (W)
			Real64 const Energy, // Energy required to meet the load (J)
			Real64 const HeatingEnergy, // Heating Energy required to meet the load (J)
			Real64 const CoolingEnergy, // Cooling Energy required to meet the load (J)
			bool const SetLoopIndexFlag
		) :
			Name( Name ),
			TypeNum( TypeNum ),
			WLoopNum( WLoopNum ),
			WLoopSideNum( WLoopSideNum ),
			WLoopBranchNum( WLoopBranchNum ),
			WLoopCompNum( WLoopCompNum ),
			Init( Init ),
			InitSizing( InitSizing ),
			InletNode( InletNode ),
			InletTemp( InletTemp ),
			OutletNode( OutletNode ),
			OutletTemp( OutletTemp ),
			LoadSchedule( LoadSchedule ),
			EMSOverridePower( EMSOverridePower ),
			EMSPowerValue( EMSPowerValue ),
			PeakVolFlowRate( PeakVolFlowRate ),
			FlowRateFracSchedule( FlowRateFracSchedule ),
			VolFlowRate( VolFlowRate ),
			MassFlowRate( MassFlowRate ),
			EMSOverrideMassFlow( EMSOverrideMassFlow ),
			EMSMassFlowValue( EMSMassFlowValue ),
			Power( Power ),
			Energy( Energy ),
			HeatingEnergy( HeatingEnergy ),
			CoolingEnergy( CoolingEnergy ),
			SetLoopIndexFlag( SetLoopIndexFlag )
		{}

	};

	// Object Data
	extern Array1D< PlantProfileData > PlantProfile;

	// Functions

	void
	SimulatePlantProfile(
		std::string const & EquipTypeName, // description of model (not used until different types of profiles)
		std::string const & EquipName, // the user-defined name
		int const EquipTypeNum, // the plant parameter ID for equipment model
		int & ProfileNum, // the index for specific load profile
		bool const FirstHVACIteration,
		bool const InitLoopEquip // flag indicating if called in special initialization mode.
	);

	void
	GetPlantProfileInput();

	void
	InitPlantProfile( int const ProfileNum );

	void
	UpdatePlantProfile( int const ProfileNum );

	void
	ReportPlantProfile( int const ProfileNum );

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

} // PlantLoadProfile

} // EnergyPlus

#endif
