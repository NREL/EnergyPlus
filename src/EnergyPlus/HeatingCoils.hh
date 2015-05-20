#ifndef HeatingCoils_hh_INCLUDED
#define HeatingCoils_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HeatingCoils {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern Real64 const MinAirMassFlow;
	extern int NumDesuperheaterCoil; // Total number of desuperheater heating coil objects in input

	// reclaim heat object types
	extern int const COMPRESSORRACK_REFRIGERATEDCASE;
	extern int const COIL_DX_COOLING;
	extern int const COIL_DX_MULTISPEED;
	extern int const COIL_DX_MULTIMODE;
	extern int const CONDENSER_REFRIGERATION;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumHeatingCoils; // The Number of HeatingCoils found in the Input
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool ValidSourceType; // Used to determine if a source for a desuperheater heating coil is valid
	extern bool GetCoilsInputFlag; // Flag set to make sure you get input once
	extern bool CoilIsSuppHeater; // Flag set to indicate the heating coil is a supplemental heater
	extern Array1D_bool CheckEquipName;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Utility routines for module

	// Types

	struct HeatingCoilEquipConditions
	{
		// Members
		std::string Name; // Name of the HeatingCoil
		std::string HeatingCoilType; // Type of HeatingCoil ie. Heating or Cooling
		std::string HeatingCoilModel; // Type of HeatingCoil ie. Simple, Detailed, etc.
		int HCoilType_Num;
		std::string Schedule; // HeatingCoil Operation Schedule
		int SchedPtr; // Pointer to the correct schedule
		int InsuffTemperatureWarn; // Used for recurring error message
		Real64 InletAirMassFlowRate; // MassFlow through the HeatingCoil being Simulated [kg/Sec]
		Real64 OutletAirMassFlowRate;
		Real64 InletAirTemp;
		Real64 OutletAirTemp;
		Real64 InletAirHumRat;
		Real64 OutletAirHumRat;
		Real64 InletAirEnthalpy;
		Real64 OutletAirEnthalpy;
		Real64 HeatingCoilLoad; // Total Load on the Coil [J]
		Real64 HeatingCoilRate; // Total Coil Rate on the Coil [W]
		Real64 GasUseLoad; // Gas Usage of Coil [J]
		Real64 ElecUseLoad; // Electric Usage of Coil [J]
		Real64 GasUseRate; // Gas Usage of Coil [W]
		Real64 ElecUseRate; // Electric Usage of Coil [W]
		Real64 Efficiency; // HeatingCoil Efficiency Value
		Real64 NominalCapacity; // Nominal Capacity of Coil [W]
		Real64 DesiredOutletTemp;
		Real64 DesiredOutletHumRat;
		Real64 AvailTemperature; // Used in heat recovery test [C]
		int AirInletNodeNum;
		int AirOutletNodeNum;
		int TempSetPointNodeNum; // If applicable this is the node number that the temp setpoint exists.
		int Control;
		int PLFCurveIndex; // Index for part-load factor curve index for gas heating coil
		Real64 ParasiticElecLoad; // parasitic electric load associated with the gas heating coil
		Real64 ParasiticGasLoad; // parasitic gas load associated with the gas heating coil
		// (standing pilot light) [J]
		Real64 ParasiticGasRate; // avg. parasitic gas consumption rate with the gas heating coil
		// (standing pilot light) [J]
		Real64 ParasiticGasCapacity; // capacity of parasitic gas consumption rate, input by user [W]
		Real64 RTF; // Heater runtime fraction, including PLF curve impacts
		int RTFErrorIndex; // used in recurring error warnings
		int RTFErrorCount; // used in recurring error warnings
		int PLFErrorIndex; // used in recurring error warnings
		int PLFErrorCount; // used in recurring error warnings
		std::string ReclaimHeatingCoilName; // Name of reclaim heating coil
		int ReclaimHeatingSourceIndexNum; // Index to reclaim heating source (condenser) of a specific type
		int ReclaimHeatingSource; // The source for the Reclaim Heating Coil
		//                                                            COMPRESSOR RACK:REFRIGERATED CASE    = 1
		//                                                            COIL:DX:COOLINGBYPASSFACTOREMPIRICAL = 2
		//                                                            COIL:DX:MULTISPEED:COOLINGEMPIRICAL  = 3
		//                                                            COIL:DX:MultiMode:CoolingEmpirical   = 4
		//                                                            Refrigeration:Condenser              = 5
		int NumOfStages; // Number of speeds
		Array1D< Real64 > MSNominalCapacity; // Nominal Capacity MS AC Furnace [W]
		Array1D< Real64 > MSEfficiency; // Efficiency for MS AC Furnace [dimensionless]
		Array1D< Real64 > MSParasiticElecLoad; // Parasitic elec load MS AC Furnace (gas only) [W]

		// Default Constructor
		HeatingCoilEquipConditions() :
			HCoilType_Num( 0 ),
			SchedPtr( 0 ),
			InsuffTemperatureWarn( 0 ),
			InletAirMassFlowRate( 0.0 ),
			OutletAirMassFlowRate( 0.0 ),
			InletAirTemp( 0.0 ),
			OutletAirTemp( 0.0 ),
			InletAirHumRat( 0.0 ),
			OutletAirHumRat( 0.0 ),
			InletAirEnthalpy( 0.0 ),
			OutletAirEnthalpy( 0.0 ),
			HeatingCoilLoad( 0.0 ),
			HeatingCoilRate( 0.0 ),
			GasUseLoad( 0.0 ),
			ElecUseLoad( 0.0 ),
			GasUseRate( 0.0 ),
			ElecUseRate( 0.0 ),
			Efficiency( 0.0 ),
			NominalCapacity( 0.0 ),
			DesiredOutletTemp( 0.0 ),
			DesiredOutletHumRat( 0.0 ),
			AvailTemperature( 0.0 ),
			AirInletNodeNum( 0 ),
			AirOutletNodeNum( 0 ),
			TempSetPointNodeNum( 0 ),
			Control( 0 ),
			PLFCurveIndex( 0 ),
			ParasiticElecLoad( 0.0 ),
			ParasiticGasLoad( 0.0 ),
			ParasiticGasRate( 0.0 ),
			ParasiticGasCapacity( 0.0 ),
			RTF( 0.0 ),
			RTFErrorIndex( 0 ),
			RTFErrorCount( 0 ),
			PLFErrorIndex( 0 ),
			PLFErrorCount( 0 ),
			ReclaimHeatingSourceIndexNum( 0 ),
			ReclaimHeatingSource( 0 ),
			NumOfStages( 0 )
		{}

		// Member Constructor
		HeatingCoilEquipConditions(
			std::string const & Name, // Name of the HeatingCoil
			std::string const & HeatingCoilType, // Type of HeatingCoil ie. Heating or Cooling
			std::string const & HeatingCoilModel, // Type of HeatingCoil ie. Simple, Detailed, etc.
			int const HCoilType_Num,
			std::string const & Schedule, // HeatingCoil Operation Schedule
			int const SchedPtr, // Pointer to the correct schedule
			int const InsuffTemperatureWarn, // Used for recurring error message
			Real64 const InletAirMassFlowRate, // MassFlow through the HeatingCoil being Simulated [kg/Sec]
			Real64 const OutletAirMassFlowRate,
			Real64 const InletAirTemp,
			Real64 const OutletAirTemp,
			Real64 const InletAirHumRat,
			Real64 const OutletAirHumRat,
			Real64 const InletAirEnthalpy,
			Real64 const OutletAirEnthalpy,
			Real64 const HeatingCoilLoad, // Total Load on the Coil [J]
			Real64 const HeatingCoilRate, // Total Coil Rate on the Coil [W]
			Real64 const GasUseLoad, // Gas Usage of Coil [J]
			Real64 const ElecUseLoad, // Electric Usage of Coil [J]
			Real64 const GasUseRate, // Gas Usage of Coil [W]
			Real64 const ElecUseRate, // Electric Usage of Coil [W]
			Real64 const Efficiency, // HeatingCoil Efficiency Value
			Real64 const NominalCapacity, // Nominal Capacity of Coil [W]
			Real64 const DesiredOutletTemp,
			Real64 const DesiredOutletHumRat,
			Real64 const AvailTemperature, // Used in heat recovery test [C]
			int const AirInletNodeNum,
			int const AirOutletNodeNum,
			int const TempSetPointNodeNum, // If applicable this is the node number that the temp setpoint exists.
			int const Control,
			int const PLFCurveIndex, // Index for part-load factor curve index for gas heating coil
			Real64 const ParasiticElecLoad, // parasitic electric load associated with the gas heating coil
			Real64 const ParasiticGasLoad, // parasitic gas load associated with the gas heating coil
			Real64 const ParasiticGasRate, // avg. parasitic gas consumption rate with the gas heating coil
			Real64 const ParasiticGasCapacity, // capacity of parasitic gas consumption rate, input by user [W]
			Real64 const RTF, // Heater runtime fraction, including PLF curve impacts
			int const RTFErrorIndex, // used in recurring error warnings
			int const RTFErrorCount, // used in recurring error warnings
			int const PLFErrorIndex, // used in recurring error warnings
			int const PLFErrorCount, // used in recurring error warnings
			std::string const & ReclaimHeatingCoilName, // Name of reclaim heating coil
			int const ReclaimHeatingSourceIndexNum, // Index to reclaim heating source (condenser) of a specific type
			int const ReclaimHeatingSource, // The source for the Reclaim Heating Coil
			int const NumOfStages, // Number of speeds
			Array1< Real64 > const & MSNominalCapacity, // Nominal Capacity MS AC Furnace [W]
			Array1< Real64 > const & MSEfficiency, // Efficiency for MS AC Furnace [dimensionless]
			Array1< Real64 > const & MSParasiticElecLoad // Parasitic elec load MS AC Furnace (gas only) [W]
		) :
			Name( Name ),
			HeatingCoilType( HeatingCoilType ),
			HeatingCoilModel( HeatingCoilModel ),
			HCoilType_Num( HCoilType_Num ),
			Schedule( Schedule ),
			SchedPtr( SchedPtr ),
			InsuffTemperatureWarn( InsuffTemperatureWarn ),
			InletAirMassFlowRate( InletAirMassFlowRate ),
			OutletAirMassFlowRate( OutletAirMassFlowRate ),
			InletAirTemp( InletAirTemp ),
			OutletAirTemp( OutletAirTemp ),
			InletAirHumRat( InletAirHumRat ),
			OutletAirHumRat( OutletAirHumRat ),
			InletAirEnthalpy( InletAirEnthalpy ),
			OutletAirEnthalpy( OutletAirEnthalpy ),
			HeatingCoilLoad( HeatingCoilLoad ),
			HeatingCoilRate( HeatingCoilRate ),
			GasUseLoad( GasUseLoad ),
			ElecUseLoad( ElecUseLoad ),
			GasUseRate( GasUseRate ),
			ElecUseRate( ElecUseRate ),
			Efficiency( Efficiency ),
			NominalCapacity( NominalCapacity ),
			DesiredOutletTemp( DesiredOutletTemp ),
			DesiredOutletHumRat( DesiredOutletHumRat ),
			AvailTemperature( AvailTemperature ),
			AirInletNodeNum( AirInletNodeNum ),
			AirOutletNodeNum( AirOutletNodeNum ),
			TempSetPointNodeNum( TempSetPointNodeNum ),
			Control( Control ),
			PLFCurveIndex( PLFCurveIndex ),
			ParasiticElecLoad( ParasiticElecLoad ),
			ParasiticGasLoad( ParasiticGasLoad ),
			ParasiticGasRate( ParasiticGasRate ),
			ParasiticGasCapacity( ParasiticGasCapacity ),
			RTF( RTF ),
			RTFErrorIndex( RTFErrorIndex ),
			RTFErrorCount( RTFErrorCount ),
			PLFErrorIndex( PLFErrorIndex ),
			PLFErrorCount( PLFErrorCount ),
			ReclaimHeatingCoilName( ReclaimHeatingCoilName ),
			ReclaimHeatingSourceIndexNum( ReclaimHeatingSourceIndexNum ),
			ReclaimHeatingSource( ReclaimHeatingSource ),
			NumOfStages( NumOfStages ),
			MSNominalCapacity( MSNominalCapacity ),
			MSEfficiency( MSEfficiency ),
			MSParasiticElecLoad( MSParasiticElecLoad )
		{}

	};
	struct HeatingCoilNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		HeatingCoilNumericFieldData()
		{}

		// Member Constructor
		HeatingCoilNumericFieldData(
			Array1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
		) :
			FieldNames( FieldNames )
		{}
	};

	// Object Data
	extern Array1D< HeatingCoilEquipConditions > HeatingCoil;
	extern Array1D< HeatingCoilNumericFieldData > HeatingCoilNumericFields;

	// Functions

	void
	SimulateHeatingCoilComponents(
		std::string const & CompName,
		bool const FirstHVACIteration,
		Optional< Real64 const > QCoilReq = _, // coil load to be met
		Optional_int CompIndex = _,
		Optional< Real64 > QCoilActual = _, // coil load actually delivered returned to calling component
		Optional_bool_const SuppHeat = _, // True if current heating coil is a supplemental heating coil
		Optional_int_const FanOpMode = _, // fan operating mode, CycFanCycCoil or ContFanCycCoil
		Optional< Real64 const > PartLoadRatio = _, // part-load ratio of heating coil
		Optional_int StageNum = _,
		Optional< Real64 const > SpeedRatio = _ // Speed ratio of MultiStage heating coil
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetHeatingCoilInput();

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitHeatingCoil(
		int const CoilNum,
		bool const FirstHVACIteration,
		Real64 const QCoilRequired
	);

	void
	SizeHeatingCoil( int const CoilNum );

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcElectricHeatingCoil(
		int const CoilNum, // index to heating coil
		Real64 & QCoilReq,
		Real64 & QCoilActual, // coil load actually delivered (W)
		int const FanOpMode, // fan operating mode
		Real64 const PartLoadRatio // part-load ratio of heating coil
	);

	void
	CalcMultiStageElectricHeatingCoil(
		int & CoilNum, // the number of the electric heating coil to be simulated
		Real64 const SpeedRatio, // SpeedRatio varies between 1.0 (maximum speed) and 0.0 (minimum speed)
		Real64 const CycRatio, // cycling part load ratio
		int const StageNum, // Stage number
		int const FanOpMode // Fan operation mode
	);

	void
	CalcGasHeatingCoil(
		int const CoilNum, // index to heating coil
		Real64 const QCoilReq,
		Real64 & QCoilActual, // coil load actually delivered (W)
		int const FanOpMode, // fan operating mode
		Real64 const PartLoadRatio // part-load ratio of heating coil
	);

	void
	CalcMultiStageGasHeatingCoil(
		int & CoilNum, // the number of the Gas heating coil to be simulated
		Real64 const SpeedRatio, // SpeedRatio varies between 1.0 (maximum speed) and 0.0 (minimum speed)
		Real64 const CycRatio, // cycling part load ratio
		int const StageNum, // Speed number
		int const FanOpMode // Fan operation mode
	);

	void
	CalcDesuperheaterHeatingCoil(
		int const CoilNum, // index to desuperheater heating coil
		Real64 const QCoilReq, // load requested by the simulation for load based control [W]
		Real64 & QCoilActual // coil load actually delivered
	);

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the HeatingCoil Module
	// *****************************************************************************

	void
	UpdateHeatingCoil( int const CoilNum );

	//        End of Update subroutines for the HeatingCoil Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the HeatingCoil Module
	// *****************************************************************************

	void
	ReportHeatingCoil( int const CoilNum );

	//        End of Reporting subroutines for the HeatingCoil Module

	void
	GetCoilIndex(
		std::string const & HeatingCoilName,
		int & HeatingCoilIndex,
		bool & ErrorsFound
	);

	void
	CheckHeatingCoilSchedule(
		std::string const & CompType, // unused1208
		std::string const & CompName,
		Real64 & Value,
		int & CompIndex
	);

	Real64
	GetCoilCapacity(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilAvailScheduleIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilOutletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetHeatReclaimSourceIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilControlNodeNum(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetHeatingCoilTypeNum(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetHeatingCoilIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetHeatingCoilPLFCurveIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetHeatingCoilNumberOfStages(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	//        End of Utility subroutines for the HeatingCoil Module

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

} // HeatingCoils

} // EnergyPlus

#endif
