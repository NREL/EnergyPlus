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

#ifndef UserDefinedComponents_hh_INCLUDED
#define UserDefinedComponents_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataPlant.hh>

namespace EnergyPlus {

namespace UserDefinedComponents {

	// Using/Aliasing
	using DataPlant::HowMet_Unknown;
	using DataPlant::LoopFlowStatus_Unknown;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	extern int NumUserPlantComps;
	extern int NumUserCoils;
	extern int NumUserZoneAir;
	extern int NumUserAirTerminals;

	extern Array1D_bool CheckUserPlantCompName;
	extern Array1D_bool CheckUserCoilName;
	extern Array1D_bool CheckUserZoneAirName;
	extern Array1D_bool CheckUserAirTerminal;
	extern bool GetInput;

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

	// Types

	struct PlantConnectionStruct
	{
		// Members
		int ErlInitProgramMngr; // points to an EMS:ProgramManager to run for setup and sizing
		int ErlSimProgramMngr; // points to an EMS:ProgramManager to run only when this connection is called
		int LoopNum; // plant loop connection index
		int LoopSideNum; // plant loop side connection index
		int BranchNum; // plant loop branch connection index
		int CompNum; // plant loop component connection index
		int InletNodeNum; // plant loop inlet node index
		int OutletNodeNum; // plant loop outlet node index
		int FlowPriority; // how component affects overall loop flow determination
		int HowLoadServed; // nature of component wrt to plant loop's loads
		Real64 LowOutTempLimit; // low limit for outlet temp if MEETSLOADWITHNOMINALCAPACITYLOWOUTLIMIT
		Real64 HiOutTempLimit; // hi limit for outlet temp if MEETSLOADWITHNOMINALCAPACITYHIOUTLIMIT
		Real64 MassFlowRateRequest; // request filled by actuator, might not be satisfied if plant constrained [kg/s]
		Real64 MassFlowRateMin; // filled by actuator, reports minimum (hardware) flow rate for component [kg/s]
		Real64 MassFlowRateMax; // filled by actuator, reports maximum (hardware) flow rate for component [kg/s]
		Real64 DesignVolumeFlowRate; // filled by actuator,
		Real64 MyLoad; // fills internal variable for user's model to know current load request of supply equip [W]
		Real64 MinLoad; // filled by actuator, reports back size for load dispatch routines [W]
		Real64 MaxLoad; // filled by actuator, reports back size for load dispatch [W]
		Real64 OptLoad; // filled by actuator, reports back size for load dispatch [W]
		Real64 InletRho; // fills internal variable, current density for fluid type and inlet temperature [kg/m3]
		Real64 InletCp; // fills internal Varaible, current specific heat for fluid type and inlet temperature [J/kg-C]
		Real64 InletTemp; // fills internal variable, current inlet fluid temperature [C]
		Real64 InletMassFlowRate; // fills internal variable, current inlet mass flow rate [kg/s]
		Real64 OutletTemp; // filled by actuator, componenent outlet temperature [C]

		// Default Constructor
		PlantConnectionStruct() :
			ErlInitProgramMngr( 0 ),
			ErlSimProgramMngr( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			FlowPriority( LoopFlowStatus_Unknown ),
			HowLoadServed( HowMet_Unknown ),
			LowOutTempLimit( 0.0 ),
			HiOutTempLimit( 0.0 ),
			MassFlowRateRequest( 0.0 ),
			MassFlowRateMin( 0.0 ),
			MassFlowRateMax( 0.0 ),
			DesignVolumeFlowRate( 0.0 ),
			MyLoad( 0.0 ),
			MinLoad( 0.0 ),
			MaxLoad( 0.0 ),
			OptLoad( 0.0 ),
			InletRho( 0.0 ),
			InletCp( 0.0 ),
			InletTemp( 0.0 ),
			InletMassFlowRate( 0.0 ),
			OutletTemp( 0.0 )
		{}

	};

	struct AirConnectionStruct
	{
		// Members
		int InletNodeNum; // air inlet node index
		int OutletNodeNum; // air outlet node index
		Real64 InletRho; // fills internal variable, current inlet air density [kg/m3]
		Real64 InletCp; // fills internal variable, current inlet air specific heat [J/kg-c]
		Real64 InletTemp; // fills internal variable, current inlet air temperature [C]
		Real64 InletHumRat; // fills internal variable, current inlet air humidity ratio [kg/kg]
		Real64 InletMassFlowRate; // fills internal variable, current inlet air mass flow rate [kg/s]
		Real64 OutletTemp; // filled by actuator, component outlet temperature [C]
		Real64 OutletHumRat; // filled by actuator, component outlet humidity ratio [kg/kg]
		Real64 OutletMassFlowRate; // filled by actuator, component outlet mass flow rate [kg/s]

		// Default Constructor
		AirConnectionStruct() :
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			InletRho( 0.0 ),
			InletCp( 0.0 ),
			InletTemp( 0.0 ),
			InletHumRat( 0.0 ),
			InletMassFlowRate( 0.0 ),
			OutletTemp( 0.0 ),
			OutletHumRat( 0.0 ),
			OutletMassFlowRate( 0.0 )
		{}

	};

	struct WaterUseTankConnectionStruct // data for interacting with water use storage system
	{
		// Members
		bool SuppliedByWaterSystem;
		int SupplyTankID; // index "pointer" to WaterStorage structure
		int SupplyTankDemandARRID; // index "pointer" to demand array inside WaterStorage structure
		Real64 SupplyVdotRequest;
		bool CollectsToWaterSystem;
		int CollectionTankID; // index "pointer" to Storage TAnk array WaterStorage
		int CollectionTankSupplyARRID; // index pointe to supply Vdot array in WaterStorage
		Real64 CollectedVdot;

		// Default Constructor
		WaterUseTankConnectionStruct() :
			SuppliedByWaterSystem( false ),
			SupplyTankID( 0 ),
			SupplyTankDemandARRID( 0 ),
			SupplyVdotRequest( 0.0 ),
			CollectsToWaterSystem( false ),
			CollectionTankID( 0 ),
			CollectionTankSupplyARRID( 0 ),
			CollectedVdot( 0.0 )
		{}

	};

	struct ZoneInternalGainsStruct
	{
		// Members
		bool DeviceHasInternalGains;
		int ZoneNum;
		Real64 ConvectionGainRate;
		Real64 ReturnAirConvectionGainRate;
		Real64 ThermalRadiationGainRate;
		Real64 LatentGainRate;
		Real64 ReturnAirLatentGainRate;
		Real64 CarbonDioxideGainRate;
		Real64 GenericContamGainRate;

		// Default Constructor
		ZoneInternalGainsStruct() :
			DeviceHasInternalGains( false ),
			ZoneNum( 0 ),
			ConvectionGainRate( 0.0 ),
			ReturnAirConvectionGainRate( 0.0 ),
			ThermalRadiationGainRate( 0.0 ),
			LatentGainRate( 0.0 ),
			ReturnAirLatentGainRate( 0.0 ),
			CarbonDioxideGainRate( 0.0 ),
			GenericContamGainRate( 0.0 )
		{}

	};

	struct UserPlantComponentStruct
	{
		// Members
		std::string Name; // user identifier
		int ErlSimProgramMngr; // EMS:ProgramManager to always run when this model is called
		int NumPlantConnections; // count of how many plant loop connections there are
		Array1D< PlantConnectionStruct > Loop; // collect data for each plant loop connection
		AirConnectionStruct Air;
		WaterUseTankConnectionStruct Water;
		ZoneInternalGainsStruct Zone;

		// Default Constructor
		UserPlantComponentStruct() :
			ErlSimProgramMngr( 0 ),
			NumPlantConnections( 0 )
		{}

	};

	struct UserCoilComponentStruct
	{
		// Members
		std::string Name; // user identifier
		int ErlSimProgramMngr; // EMS:ProgramManager to always run when this model is called
		int ErlInitProgramMngr; // EMS:ProgramManager to  run when this model is initialized and setup
		int NumAirConnections; // count of how many air connectiosn there are
		bool PlantIsConnected;
		Array1D< AirConnectionStruct > Air;
		PlantConnectionStruct Loop;
		WaterUseTankConnectionStruct Water;
		ZoneInternalGainsStruct Zone;

		// Default Constructor
		UserCoilComponentStruct() :
			ErlSimProgramMngr( 0 ),
			ErlInitProgramMngr( 0 ),
			NumAirConnections( 0 ),
			PlantIsConnected( false )
		{}

	};

	struct UserZoneHVACForcedAirComponentStruct
	{
		// Members
		std::string Name; // user identifier
		int ErlSimProgramMngr; // EMS:ProgramManager to always run when this model is called
		int ErlInitProgramMngr; // EMS:ProgramManager to  run when this model is initialized and setup
		AirConnectionStruct ZoneAir;
		AirConnectionStruct SourceAir;
		int NumPlantConnections; // count of how many plant loop (demand) connections there are
		Array1D< PlantConnectionStruct > Loop; // collect data for each plant loop connection
		WaterUseTankConnectionStruct Water;
		ZoneInternalGainsStruct Zone; // for skin losses
		Real64 RemainingOutputToHeatingSP; // sensible load remaining for device, to heating setpoint [W]
		Real64 RemainingOutputToCoolingSP; // sensible load remaining for device, negative means cooling [W]
		Real64 RemainingOutputReqToHumidSP; // latent load remaining for device, to humidification setpoint [kg/s]
		Real64 RemainingOutputReqToDehumidSP; // latent load remaining for device, Negative means dehumidify [kg/s]

		// Default Constructor
		UserZoneHVACForcedAirComponentStruct() :
			ErlSimProgramMngr( 0 ),
			ErlInitProgramMngr( 0 ),
			NumPlantConnections( 0 ),
			RemainingOutputToHeatingSP( 0.0 ),
			RemainingOutputToCoolingSP( 0.0 ),
			RemainingOutputReqToHumidSP( 0.0 ),
			RemainingOutputReqToDehumidSP( 0.0 )
		{}

	};

	struct UserAirTerminalComponentStruct
	{
		// Members
		std::string Name; // user identifier
		int ActualCtrlZoneNum;
		int ErlSimProgramMngr; // EMS:ProgramManager to always run when this model is called
		int ErlInitProgramMngr; // EMS:ProgramManager to  run when this model is initialized and setup
		AirConnectionStruct AirLoop;
		AirConnectionStruct SourceAir;
		int NumPlantConnections; // count of how many plant loop (demand) connections there are
		Array1D< PlantConnectionStruct > Loop; // collect data for each plant loop connection
		WaterUseTankConnectionStruct Water;
		ZoneInternalGainsStruct Zone; // for skin losses
		Real64 RemainingOutputToHeatingSP; // sensible load remaining for device, to heating setpoint [W]
		Real64 RemainingOutputToCoolingSP; // sensible load remaining for device, negative means cooling [W]
		Real64 RemainingOutputReqToHumidSP; // latent load remaining for device, to humidification setpoint [kg/s]
		Real64 RemainingOutputReqToDehumidSP; // latent load remaining for device, Negative means dehumidify [kg/s]

		// Default Constructor
		UserAirTerminalComponentStruct() :
			ActualCtrlZoneNum( 0 ),
			ErlSimProgramMngr( 0 ),
			ErlInitProgramMngr( 0 ),
			NumPlantConnections( 0 ),
			RemainingOutputToHeatingSP( 0.0 ),
			RemainingOutputToCoolingSP( 0.0 ),
			RemainingOutputReqToHumidSP( 0.0 ),
			RemainingOutputReqToDehumidSP( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< UserPlantComponentStruct > UserPlantComp;
	extern Array1D< UserCoilComponentStruct > UserCoil;
	extern Array1D< UserZoneHVACForcedAirComponentStruct > UserZoneAirHVAC;
	extern Array1D< UserAirTerminalComponentStruct > UserAirTerminal;

	// Functions

	void
	SimUserDefinedPlantComponent(
		int const LoopNum, // plant loop sim call originated from
		int const LoopSideNum, // plant loop side sim call originated from
		std::string const & EquipType, // type of equipment, 'PlantComponent:UserDefined'
		std::string const & EquipName, // user name for component
		int & CompIndex,
		bool & InitLoopEquip,
		Real64 const MyLoad,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap
	);

	void
	SimCoilUserDefined(
		std::string const & EquipName, // user name for component
		int & CompIndex,
		int const AirLoopNum,
		bool & HeatingActive,
		bool & CoolingActive
	);

	void
	SimZoneAirUserDefined(
		std::string const & CompName, // name of the packaged terminal heat pump
		int const ZoneNum, // number of zone being served
		Real64 & SensibleOutputProvided, // sensible capacity delivered to zone
		Real64 & LatentOutputProvided, // Latent add/removal  (kg/s), dehumid = negative
		int & CompIndex // index to zone hvac unit
	);

	void
	SimAirTerminalUserDefined(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum,
		int & CompIndex
	);

	void
	GetUserDefinedComponents();

	void
	InitPlantUserComponent(
		int const CompNum,
		int const LoopNum,
		Real64 const MyLoad
	);

	void
	InitCoilUserDefined( int const CompNum );

	void
	InitZoneAirUserDefined(
		int const CompNum,
		int const ZoneNum
	);

	void
	InitAirTerminalUserDefined(
		int const CompNum,
		int const ZoneNum
	);

	void
	ReportPlantUserComponent(
		int const CompNum,
		int const LoopNum
	);

	void
	ReportCoilUserDefined( int const CompNum );

	void
	ReportZoneAirUserDefined( int const CompNum );

	void
	ReportAirTerminalUserDefined( int const CompNum );

	void
	GetUserDefinedCoilIndex(
		std::string const & CoilName,
		int & CoilIndex,
		bool & ErrorsFound,
		std::string const & CurrentModuleObject
	);

	void
	GetUserDefinedCoilAirInletNode(
		std::string const & CoilName,
		int & CoilAirInletNode,
		bool & ErrorsFound,
		std::string const & CurrentModuleObject
	);

	void
	GetUserDefinedCoilAirOutletNode(
		std::string const & CoilName,
		int & CoilAirOutletNode,
		bool & ErrorsFound,
		std::string const & CurrentModuleObject
	);

} // UserDefinedComponents

} // EnergyPlus

#endif
