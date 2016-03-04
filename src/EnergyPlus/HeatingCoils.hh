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
		bool DesiccantRegenerationCoil; // true if it is a regeneration air heating coil defined in Desiccant Dehumidifier system
		int DesiccantDehumNum; // index to desiccant dehumidifier object

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
			NumOfStages( 0 ),
			DesiccantRegenerationCoil( false ),
			DesiccantDehumNum( 0 )
		{}

	};
	struct HeatingCoilNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		HeatingCoilNumericFieldData()
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

	// Clears the global data in HeatingCoils.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	// sets data to a coil that is used as a regeneration air heating coil in
	// desiccant dehumidification system
	void
	SetHeatingCoilData(
		int const CoilNum, // Number of electric or gas heating Coil
		bool & ErrorsFound, // Set to true if certain errors found
		Optional_bool DesiccantRegenerationCoil = _, // Flag that this coil is used as regeneration air heating coil
		Optional_int DesiccantDehumIndex = _ // Index for the desiccant dehum system where this caoil is used 
	);

	//        End of Utility subroutines for the HeatingCoil Module

} // HeatingCoils

} // EnergyPlus

#endif
