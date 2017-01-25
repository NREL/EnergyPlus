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

#ifndef ChillerIndirectAbsorption_hh_INCLUDED
#define ChillerIndirectAbsorption_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ChillerIndirectAbsorption {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	//chiller flow modes
	extern int const FlowModeNotSet;
	extern int const ConstantFlow;
	extern int const NotModulated;
	extern int const LeavingSetPointModulated;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumIndirectAbsorbers; // number of Absorption Chillers specified in input
	extern Real64 CondMassFlowRate; // Kg/s - condenser mass flow rate, water side
	extern Real64 EvapMassFlowRate; // Kg/s - evaporator mass flow rate, water side
	extern Real64 GenMassFlowRate; // Kg/s - steam mass flow rate, water side
	extern Real64 CondOutletTemp; // C - condenser outlet temperature, water side
	extern Real64 EvapOutletTemp; // C - evaporator outlet temperature, water side
	extern Real64 GenOutletTemp; // C - generator fluid outlet temperature
	extern Real64 SteamOutletEnthalpy; // J/kg - generator fluid outlet enthalpy
	extern Real64 PumpingPower; // W - rate of Absorber energy use
	extern Real64 PumpingEnergy; // J - Absorber energy use
	extern Real64 QGenerator; // W - rate of Absorber steam use
	extern Real64 GeneratorEnergy; // J - Absorber steam use
	extern Real64 QEvaporator; // W - rate of heat transfer to the evaporator coil
	extern Real64 EvaporatorEnergy; // J - heat transfer to the evaporator coil
	extern Real64 QCondenser; // W - rate of heat transfer to the condenser coil
	extern Real64 CondenserEnergy; // J - heat transfer to the condenser coil
	extern Real64 EnergyLossToEnvironment; // J - piping energy loss from generator outlet to pump inlet
	extern Real64 ChillerONOFFCyclingFrac; // fraction of time chiller is on

	// SUBROUTINE SPECIFICATIONS FOR MODULE:

	// Types

	struct IndirectAbsorberSpecs
	{
		// Members
		std::string Name; // user identifier
		Real64 NomCap; // W - design nominal capacity of Absorber
		bool NomCapWasAutoSized; //true if Nominal capacity was autosize on input
		Real64 NomPumpPower; // W - design nominal capacity of Absorber
		bool NomPumpPowerWasAutoSized; // true if nominal pump power was autosize on input
		Real64 EvapVolFlowRate; // m3/s - design nominal water volumetric flow rate through the evaporator
		bool EvapVolFlowRateWasAutoSized; //true if evaporator flow rate was autosize on input
		Real64 CondVolFlowRate; // m3/s - design nominal water volumetric flow rate through the condenser
		bool CondVolFlowRateWasAutoSized; //true if condenser flow rate was autosize on input
		Real64 EvapMassFlowRateMax; // kg/s - Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
		Real64 CondMassFlowRateMax; // Max Design Condeneser Mass Flow Rate [kg/s]
		Real64 GenMassFlowRateMax; // kg/s - Max Design Generator Mass Flow Rate converted from Volume Flow Rate
		Real64 MinPartLoadRat; // (BLAST MIN) min allowed operating frac full load
		Real64 MaxPartLoadRat; // (BLAST MAX) max allowed operating frac full load
		Real64 OptPartLoadRat; // (BLAST BEST) optimal operating frac full load
		Real64 TempDesCondIn; // C - (BLAST ADJTC(1)The design secondary loop fluid
		// temperature at the Absorber condenser side inlet
		Real64 MinCondInletTemp; // C - minimum condenser inlet temperature for chiller operation
		Real64 MinGeneratorInletTemp; // C - minimum generator inlet temperature for chiller operation
		Real64 TempLowLimitEvapOut; // C - low temperature shut off
		Real64 GeneratorVolFlowRate; // m3/s - hot water volumetric flow rate through generator
		bool GeneratorVolFlowRateWasAutoSized; //true if hot water flow was autosize on input
		Real64 GeneratorSubcool; // C - amount of subcooling in steam generator
		Real64 LoopSubcool; // C - amount of subcooling in steam generator
		Real64 GeneratorDeltaTemp; // C - generator fluid temperature difference (water only)
		bool GeneratorDeltaTempWasAutoSized; // true if generator delta T was autosize on input
		Real64 SizFac; // Sizing factor
		int EvapInletNodeNum; // Node number on the inlet side of the plant
		int EvapOutletNodeNum; // Node number on the outlet side of the plant
		int CondInletNodeNum; // Node number on the inlet side of the condenser
		int CondOutletNodeNum; // Node number on the outlet side of the condenser
		int GeneratorInletNodeNum; // Generator inlet node number, steam/water side
		int GeneratorOutletNodeNum; // Generator outlet node number, steam/water side
		int GeneratorInputCurvePtr; // Index to steam use curve as a function of PLR
		int PumpPowerCurvePtr; // Index to pump power curve as a function of PLR
		int CapFCondenserTempPtr; // Index to capacity as a function of absorber temp curve
		int CapFEvaporatorTempPtr; // Index to capacity as a function of evaporator temp curve
		int CapFGeneratorTempPtr; // Index to capacity as a function of generator temp curve
		int HeatInputFCondTempPtr; // Index to generator heat input as a function of absorber temp
		int HeatInputFEvapTempPtr; // Index to generator heat input as a function of absorber temp
		int ErrCount2; // error counter
		int GenHeatSourceType; // Generator heat source type, NodeType_Steam=3 or NodeType_Water=2
		int SteamFluidIndex; // index to generator fluid type
		bool Available; // need an array of logicals--load identifiers of available equipment
		bool ON; // simulate the machine at it's operating part load ratio
		int FlowMode; // one of 3 modes for componet flow during operation
		bool ModulatedFlowSetToLoop; // True if the setpoint is missing at the outlet node
		bool ModulatedFlowErrDone; // true if setpoint warning issued
		int MinCondInletTempCtr; // Low condenser temp warning message counter
		int MinCondInletTempIndex; // Low condenser temp warning message index
		int MinGenInletTempCtr; // Low generator temp warning message counter
		int MinGenInletTempIndex; // Low generator temp warning message index
		int CWLoopNum; // chilled water plant loop index number
		int CWLoopSideNum; // chilled water plant loop side index
		int CWBranchNum; // chilled water plant loop branch index
		int CWCompNum; // chilled water plant loop component index
		int CDLoopNum; // condenser water plant loop index number
		int CDLoopSideNum; // condenser water plant loop side index
		int CDBranchNum; // condenser water plant loop branch index
		int CDCompNum; // condenser water plant loop component index
		int GenLoopNum; // generator plant loop index number
		int GenLoopSideNum; // generator plant loop side index
		int GenBranchNum; // generator plant loop branch index
		int GenCompNum; // generator plant loop component index
		bool PossibleSubcooling; // flag to indicate chiller is doing less cooling that requested

		// Default Constructor
		IndirectAbsorberSpecs() :
			NomCap( 0.0 ),
			NomCapWasAutoSized( false ),
			NomPumpPower( 0.0 ),
			NomPumpPowerWasAutoSized( false ),
			EvapVolFlowRate( 0.0 ),
			EvapVolFlowRateWasAutoSized( false ),
			CondVolFlowRate( 0.0 ),
			CondVolFlowRateWasAutoSized( false ),
			EvapMassFlowRateMax( 0.0 ),
			CondMassFlowRateMax( 0.0 ),
			GenMassFlowRateMax( 0.0 ),
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			OptPartLoadRat( 0.0 ),
			TempDesCondIn( 0.0 ),
			MinCondInletTemp( 0.0 ),
			MinGeneratorInletTemp( 0.0 ),
			TempLowLimitEvapOut( 0.0 ),
			GeneratorVolFlowRate( 0.0 ),
			GeneratorVolFlowRateWasAutoSized( false ),
			GeneratorSubcool( 0.0 ),
			LoopSubcool( 0.0 ),
			GeneratorDeltaTemp( -99999.0 ),
			GeneratorDeltaTempWasAutoSized( true ),
			SizFac( 0.0 ),
			EvapInletNodeNum( 0 ),
			EvapOutletNodeNum( 0 ),
			CondInletNodeNum( 0 ),
			CondOutletNodeNum( 0 ),
			GeneratorInletNodeNum( 0 ),
			GeneratorOutletNodeNum( 0 ),
			GeneratorInputCurvePtr( 0 ),
			PumpPowerCurvePtr( 0 ),
			CapFCondenserTempPtr( 0 ),
			CapFEvaporatorTempPtr( 0 ),
			CapFGeneratorTempPtr( 0 ),
			HeatInputFCondTempPtr( 0 ),
			HeatInputFEvapTempPtr( 0 ),
			ErrCount2( 0 ),
			GenHeatSourceType( 0 ),
			SteamFluidIndex( 0 ),
			Available( false ),
			ON( false ),
			FlowMode( FlowModeNotSet ),
			ModulatedFlowSetToLoop( false ),
			ModulatedFlowErrDone( false ),
			MinCondInletTempCtr( 0 ),
			MinCondInletTempIndex( 0 ),
			MinGenInletTempCtr( 0 ),
			MinGenInletTempIndex( 0 ),
			CWLoopNum( 0 ),
			CWLoopSideNum( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			CDLoopNum( 0 ),
			CDLoopSideNum( 0 ),
			CDBranchNum( 0 ),
			CDCompNum( 0 ),
			GenLoopNum( 0 ),
			GenLoopSideNum( 0 ),
			GenBranchNum( 0 ),
			GenCompNum( 0 ),
			PossibleSubcooling( false )
		{}
	};

	struct ReportVars
	{
		// Members
		Real64 PumpingPower; // reporting: W - electric pumping power
		Real64 QGenerator; // reporting: W - steam heat transfer rate
		Real64 QEvap; // reporting: W - evaporator heat transfer rate
		Real64 QCond; // reporting: W - condensor heat transfer rate
		Real64 PumpingEnergy; // reporting: J - electric pumping power
		Real64 GeneratorEnergy; // reporting: J - steam heat transfer rate
		Real64 EvapEnergy; // reporting: J - evaporator heat transfer rate
		Real64 CondEnergy; // reporting: J - condensor heat transfer rate
		Real64 CondInletTemp; // reporting: C - condenser inlet temperature
		Real64 EvapInletTemp; // reporting: C - evaporator inlet temperature
		Real64 CondOutletTemp; // reporting: C - condenser outlet temperature
		Real64 EvapOutletTemp; // reporting: C - evaporator outlet temperature
		Real64 Evapmdot; // reporting: kg/ - evaporator mass flow rate
		Real64 Condmdot; // reporting: kg/ - condenser mass flow rate
		Real64 Genmdot; // reporting: generatore mass flow rate when connected to plant
		Real64 SteamMdot; // reporting: kg/s - steam mass flow rate
		Real64 ActualCOP; // reporting: coefficient of performance = QEvap/QGenerator
		Real64 ChillerPartLoadRatio; // reporting: part-load ratio
		Real64 ChillerCyclingFrac; // reporting: chiller on/off cycling fraction
		Real64 LoopLoss; // reporting: W - loop loss from absorber outlet to condensate pump inlet

		// Default Constructor
		ReportVars() :
			PumpingPower( 0.0 ),
			QGenerator( 0.0 ),
			QEvap( 0.0 ),
			QCond( 0.0 ),
			PumpingEnergy( 0.0 ),
			GeneratorEnergy( 0.0 ),
			EvapEnergy( 0.0 ),
			CondEnergy( 0.0 ),
			CondInletTemp( 0.0 ),
			EvapInletTemp( 0.0 ),
			CondOutletTemp( 0.0 ),
			EvapOutletTemp( 0.0 ),
			Evapmdot( 0.0 ),
			Condmdot( 0.0 ),
			Genmdot( 0.0 ),
			SteamMdot( 0.0 ),
			ActualCOP( 0.0 ),
			ChillerPartLoadRatio( 0.0 ),
			ChillerCyclingFrac( 0.0 ),
			LoopLoss( 0.0 )
		{}
	};

	// Object Data
	extern Array1D< IndirectAbsorberSpecs > IndirectAbsorber; // dimension to number of machines
	extern Array1D< ReportVars > IndirectAbsorberReport;

	// Functions

	void
	SimIndirectAbsorber(
		std::string const & AbsorberType, // type of Absorber
		std::string const & AbsorberName, // user specified name of Absorber
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int const LoopNum, // Plant loop index for where called from
		int const LoopSide, // Plant loop side index for where called from
		int & CompIndex, // Chiller number pointer
		bool const RunFlag, // simulate Absorber when TRUE
		bool const FirstIteration, // initialize variables when TRUE
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 & MyLoad, // loop demand component will meet
		Real64 & MaxCap, // W - maximum operating capacity of Absorber
		Real64 & MinCap, // W - minimum operating capacity of Absorber
		Real64 & OptCap, // W - optimal operating capacity of Absorber
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor, // sizing factor
		Real64 & TempCondInDesign
	);

	// End Absorption Chiller Module Driver Subroutines
	//******************************************************************************

	// Beginning of Absorption Chiller Module Get Input subroutines
	//******************************************************************************

	void
	GetIndirectAbsorberInput();

	// End of Get Input subroutines for the Absorption Chiller Module
	//******************************************************************************

	void
	InitIndirectAbsorpChiller(
		int const ChillNum, // number of the current electric chiller being simulated
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad // requested load
	);

	void
	SizeIndirectAbsorpChiller( int const ChillNum );

	// Beginning of Absorber model Subroutines
	// *****************************************************************************

	void
	CalcIndirectAbsorberModel(
		int const ChillNum, // Absorber number
		Real64 const MyLoad, // operating load
		bool const RunFlag, // TRUE when Absorber operating
		bool const FirstIteration, // TRUE when first iteration of timestep !unused1208
		int const EquipFlowCtrl // Flow control mode for the equipment
	);

	// End of Absorption Chiller Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

	void
	UpdateIndirectAbsorberRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if Absorber operating
		int const Num // Absorber number
	);

	// End of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

	void
	clear_state();


} // ChillerIndirectAbsorption

} // EnergyPlus

#endif
