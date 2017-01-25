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

#ifndef ChillerReformulatedEIR_hh_INCLUDED
#define ChillerReformulatedEIR_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ChillerReformulatedEIR {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// Chiller type parameters
	extern int const AirCooled; // Air-cooled condenser currently not allowed
	extern int const WaterCooled; // Only water-cooled condensers are currently allowed
	extern int const EvapCooled; // Evap-cooled condenser currently not allowed
	// Performance curve variable parameters
	extern int const LeavingCondenser;

	//chiller flow modes
	extern int const FlowModeNotSet;
	extern int const ConstantFlow;
	extern int const NotModulated;
	extern int const LeavingSetPointModulated;

	// MODULE VARIABLE DECLARATIONS:
	extern int NumElecReformEIRChillers; // Number of electric reformulated EIR chillers specified in input
	extern Real64 CondMassFlowRate; // Condenser mass flow rate [kg/s]
	extern Real64 EvapMassFlowRate; // Evaporator mass flow rate [kg/s]
	extern Real64 CondOutletTemp; // Condenser outlet temperature [C]
	extern Real64 EvapOutletTemp; // Evaporator outlet temperature [C]
	extern Real64 Power; // Rate of chiller electric energy use [W]
	extern Real64 QEvaporator; // Rate of heat transfer to the evaporator coil [W]
	extern Real64 QCondenser; // Rate of heat transfer to the condenser coil [W]
	extern Real64 QHeatRecovered; // Rate of heat transfer to the heat recovery coil [W]
	extern Real64 HeatRecOutletTemp; // Heat recovery outlet temperature [C]
	//REAL(r64)      :: CondenserFanPower       =0.0d0 ! Condenser Fan Power (fan cycles with compressor) [W]
	extern Real64 ChillerCapFT; // Chiller capacity fraction (evaluated as a function of temperature)
	extern Real64 ChillerEIRFT; // Chiller electric input ratio (EIR = 1 / COP) as a function of temperature
	extern Real64 ChillerEIRFPLR; // Chiller EIR as a function of part-load ratio (PLR)
	extern Real64 ChillerPartLoadRatio; // Chiller part-load ratio (PLR)
	extern Real64 ChillerCyclingRatio; // Chiller cycling ratio
	extern Real64 ChillerFalseLoadRate; // Chiller false load over and above the water-side load [W]
	extern Real64 AvgCondSinkTemp; // condenser temperature value for use in curves [C]

	extern bool GetInputREIR; // When TRUE, calls subroutine to read input file

	// SUBROUTINE SPECIFICATIONS FOR MODULE ChillerReformulatedEIR

	// Types

	struct ReformulatedEIRChillerSpecs
	{
		// Members
		std::string Name; // User identifier
		int TypeNum; // plant loop type identifier
		std::string CAPFTName; // CAPFT curve name
		std::string EIRFTName; // EIRFT curve name
		std::string EIRFPLRName; // EIRPLR curve name
		int CondenserType; // Type of Condenser. Water Cooled is the only available option for now
		int PartLoadCurveType; 	// Part Load Ratio Curve Type: 1_LeavingCondenserWaterTemperature; 2_Lift //zrp
		Real64 RefCap; // Reference capacity of the chiller [W]
		bool RefCapWasAutoSized; //reference capacity was autosized on input
		Real64 RefCOP; // Reference coefficient of performance [W/W]
		int FlowMode; // one of 3 modes for componet flow during operation
		bool ModulatedFlowSetToLoop; // True if the setpoint is missing at the outlet node
		bool ModulatedFlowErrDone; // true if setpoint warning issued
		Real64 EvapVolFlowRate; // Reference water volumetric flow rate through the evaporator [m3/s]
		bool EvapVolFlowRateWasAutoSized; // true if previous was autosize input
		Real64 EvapMassFlowRateMax; // Reference water mass flow rate through evaporator [kg/s]
		Real64 CondVolFlowRate; // Reference water volumetric flow rate through the condenser [m3/s]
		bool CondVolFlowRateWasAutoSized; // true if previous was set to autosize on input
		Real64 CondMassFlowRateMax; // Reference water mass flow rate through condenser [kg/s]
		Real64 CompPowerToCondenserFrac; // Fraction of compressor electric power rejected by condenser [0 to 1]
		int EvapInletNodeNum; // Node number on the inlet side of the plant (evaporator side)
		int EvapOutletNodeNum; // Node number on the outlet side of the plant (evaporator side)
		int CondInletNodeNum; // Node number on the inlet side of the condenser
		int CondOutletNodeNum; // Node number on the outlet side of the condenser
		Real64 MinPartLoadRat; // Minimum allowed operating fraction of full load
		Real64 MaxPartLoadRat; // Maximum allowed operating fraction of full load
		Real64 OptPartLoadRat; // Optimal operating fraction of full load
		Real64 MinUnloadRat; // Minimum unloading ratio
		Real64 TempRefCondIn; // The reference secondary loop fluid temperature at the
		// chiller condenser side inlet for the reformulated chiller [C]
		Real64 TempRefCondOut; // The reference secondary loop fluid temperature at the
		// chiller condenser side outlet for the reformulated chiller [C]
		Real64 TempRefEvapOut; // The reference primary loop fluid
		// temperature at the chiller evaporator side outlet [C]
		Real64 TempLowLimitEvapOut; // Low temperature shut off [C]
		Real64 DesignHeatRecVolFlowRate; // Design water volumetric flow rate through heat recovery loop [m3/s]
		bool DesignHeatRecVolFlowRateWasAutoSized; //true if previous input was autosize
		Real64 DesignHeatRecMassFlowRate; // Design water mass flow rate through heat recovery loop [kg/s]
		Real64 SizFac; // sizing factor
		bool HeatRecActive; // True when entered Heat Rec Vol Flow Rate > 0
		int HeatRecInletNodeNum; // Node number for the heat recovery inlet side of the condenser
		int HeatRecOutletNodeNum; // Node number for the heat recovery outlet side of the condenser
		Real64 HeatRecCapacityFraction; // user input for heat recovery capacity fraction []
		Real64 HeatRecMaxCapacityLimit; // Capacity limit for Heat recovery, one time calc [W]
		int HeatRecSetPointNodeNum; // index for system node with the heat recover leaving setpoint
		int HeatRecInletLimitSchedNum; // index for schedule for the inlet high limit for heat recovery operation
		int ChillerCapFT; // Index for the total cooling capacity modifier curve
		// (function of leaving evaporator and condenser water temperatures)
		int ChillerEIRFT; // Index for the energy input ratio modifier curve
		// (function of leaving evaporator and condenser water temperatures)
		int ChillerEIRFPLR; // Index for the energy input ratio vs part-load ratio curve
		// (function of leaving condenser water temperature and part-load ratio)
		//  INTEGER           :: CondFanPowerFCap         = 0   ! Condenser fan capacity as a function of chiller capacity
		int ChillerCapFTError; // Used for negative capacity as a function of temp warnings
		int ChillerCapFTErrorIndex; // Used for negative capacity as a function of temp warnings
		int ChillerEIRFTError; // Used for negative EIR as a function of temp warnings
		int ChillerEIRFTErrorIndex; // Used for negative EIR as a function of temp warnings
		int ChillerEIRFPLRError; // Used for negative EIR as a function of PLR warnings
		int ChillerEIRFPLRErrorIndex; // Used for negative EIR as a function of PLR warnings
		Real64 ChillerCAPFTXTempMin; // Minimum value of CAPFT curve X variable [C]
		Real64 ChillerCAPFTXTempMax; // Maximum value of CAPFT curve X variable [C]
		Real64 ChillerCAPFTYTempMin; // Minimum value of CAPFT curve Y variable [C]
		Real64 ChillerCAPFTYTempMax; // Maximum value of CAPFT curve Y variable [C]
		Real64 ChillerEIRFTXTempMin; // Minimum value of EIRFT curve X variable [C]
		Real64 ChillerEIRFTXTempMax; // Maximum value of EIRFT curve X variable [C]
		Real64 ChillerEIRFTYTempMin; // Minimum value of EIRFT curve Y variable [C]
		Real64 ChillerEIRFTYTempMax; // Maximum value of EIRFT curve Y variable [C]
		Real64 ChillerEIRFPLRTempMin; // Minimum value of EIRFPLR curve condenser outlet temperature [C]
		Real64 ChillerEIRFPLRTempMax; // Maximum value of EIRFPLR curve condenser outlet temperature [C]
		Real64 ChillerEIRFPLRPLRMin; // Minimum value of EIRFPLR curve part-load ratio
		Real64 ChillerEIRFPLRPLRMax; // Maximum value of EIRFPLR curve part-load ratio
		Real64 ChillerLiftNomMin;	 // Minimum value of EIRFPLR curve Normalized Chiller lift
		Real64 ChillerLiftNomMax;	 // Maximum value of EIRFPLR curve Normalized Chiller lift
		Real64 ChillerTdevNomMin;	 // Minimum value of EIRFPLR curve Normalized Tdev
		Real64 ChillerTdevNomMax;	 // Maximum value of EIRFPLR curve Normalized Tdev
		int CAPFTXIter; // Iteration counter for evaporator outlet temperature CAPFT warning messages
		int CAPFTXIterIndex; // Index for evaporator outlet temperature CAPFT warning messages
		int CAPFTYIter; // Iteration counter for condenser outlet temperature CAPFT warning messages
		int CAPFTYIterIndex; // Index for condenser outlet temperature CAPFT warning messages
		int EIRFTXIter; // Iteration counter for evaporator outlet temperature EIRFT warning messages
		int EIRFTXIterIndex; // Index for evaporator outlet temperature EIRFT warning messages
		int EIRFTYIter; // Iteration counter for condenser outlet temperature EIRFT warning messages
		int EIRFTYIterIndex; // Index for condenser outlet temperature EIRFT warning messages
		int EIRFPLRTIter; // Iteration counter for condenser outlet temperature EIRFPLR warning messages
		int EIRFPLRTIterIndex; // Index for condenser outlet temperature EIRFPLR warning messages
		int EIRFPLRPLRIter; // Iteration counter for part-load ratio EIRFPLR warning messages
		int EIRFPLRPLRIterIndex; // Index for part-load ratio EIRFPLR warning messages
		int IterLimitExceededNum; // Iteration limit exceeded for RegulaFalsi routine
		int IterLimitErrIndex; // Index to iteration limit warning for RegulaFalsi routine
		int IterFailed; // Iteration limit failed for RegulaFalsi routine
		int IterFailedIndex; // Index to iteration limit failed for RegulaFalsi routine
		int DeltaTErrCount; // Evaporator delta T equals 0 for variable flow chiller warning messages
		int DeltaTErrCountIndex; // Index to evaporator delta T = 0 for variable flow chiller warning messages
		int CWLoopNum; // chilled water plant loop index number
		int CWLoopSideNum; // chilled water plant loop side index
		int CWBranchNum; // chilled water plant loop branch index
		int CWCompNum; // chilled water plant loop component index
		int CDLoopNum; // condenser water plant loop index number
		int CDLoopSideNum; // condenser water plant loop side index
		int CDBranchNum; // condenser water plant loop branch index
		int CDCompNum; // condenser water plant loop component index
		int HRLoopNum; // heat recovery water plant loop index
		int HRLoopSideNum; // heat recovery water plant loop side index
		int HRBranchNum; // heat recovery water plant loop branch index
		int HRCompNum; // heat recovery water plant loop component index
		int CondMassFlowIndex;
		//  CHARACTER(len=220):: MsgBuffer1    = ' ' !- buffer to print warning messages on following time step
		//  CHARACTER(len=300):: MsgBuffer2    = ' ' !- buffer to print warning messages on following time step
		//  REAL(r64)         :: MsgDataLast   = 0.0d0 ! value of data when warning occurred (passed to Recurring Warn)
		//  LOGICAL           :: PrintMessage  = .FALSE. ! logical to determine if message is valid
		//  INTEGER           :: MsgErrorCount = 0   ! number of occurrences of warning
		//  INTEGER           :: ErrCount1     = 0   ! for recurring error messages
		bool PossibleSubcooling; // flag to indicate chiller is doing less cooling that requested

		// Default Constructor
		ReformulatedEIRChillerSpecs() :
			TypeNum( 0 ),
			CondenserType( 0 ),
			PartLoadCurveType( 0 ),
			RefCap( 0.0 ),
			RefCapWasAutoSized( false ),
			RefCOP( 0.0 ),
			FlowMode( FlowModeNotSet ),
			ModulatedFlowSetToLoop( false ),
			ModulatedFlowErrDone( false ),
			EvapVolFlowRate( 0.0 ),
			EvapVolFlowRateWasAutoSized( false ),
			EvapMassFlowRateMax( 0.0 ),
			CondVolFlowRate( 0.0 ),
			CondVolFlowRateWasAutoSized( false ),
			CondMassFlowRateMax( 0.0 ),
			CompPowerToCondenserFrac( 0.0 ),
			EvapInletNodeNum( 0 ),
			EvapOutletNodeNum( 0 ),
			CondInletNodeNum( 0 ),
			CondOutletNodeNum( 0 ),
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			OptPartLoadRat( 0.0 ),
			MinUnloadRat( 0.0 ),
			TempRefCondIn( 0.0 ),
			TempRefCondOut( 0.0 ),
			TempRefEvapOut( 0.0 ),
			TempLowLimitEvapOut( 0.0 ),
			DesignHeatRecVolFlowRate( 0.0 ),
			DesignHeatRecVolFlowRateWasAutoSized( false ),
			DesignHeatRecMassFlowRate( 0.0 ),
			SizFac( 0.0 ),
			HeatRecActive( false ),
			HeatRecInletNodeNum( 0 ),
			HeatRecOutletNodeNum( 0 ),
			HeatRecCapacityFraction( 0.0 ),
			HeatRecMaxCapacityLimit( 0.0 ),
			HeatRecSetPointNodeNum( 0 ),
			HeatRecInletLimitSchedNum( 0 ),
			ChillerCapFT( 0 ),
			ChillerEIRFT( 0 ),
			ChillerEIRFPLR( 0 ),
			ChillerCapFTError( 0 ),
			ChillerCapFTErrorIndex( 0 ),
			ChillerEIRFTError( 0 ),
			ChillerEIRFTErrorIndex( 0 ),
			ChillerEIRFPLRError( 0 ),
			ChillerEIRFPLRErrorIndex( 0 ),
			ChillerCAPFTXTempMin( 0.0 ),
			ChillerCAPFTXTempMax( 0.0 ),
			ChillerCAPFTYTempMin( 0.0 ),
			ChillerCAPFTYTempMax( 0.0 ),
			ChillerEIRFTXTempMin( 0.0 ),
			ChillerEIRFTXTempMax( 0.0 ),
			ChillerEIRFTYTempMin( 0.0 ),
			ChillerEIRFTYTempMax( 0.0 ),
			ChillerEIRFPLRTempMin( 0.0 ),
			ChillerEIRFPLRTempMax( 0.0 ),
			ChillerEIRFPLRPLRMin( 0.0 ),
			ChillerEIRFPLRPLRMax( 0.0 ),
			ChillerLiftNomMin( 0.0 ), //zrp_Aug2014
			ChillerLiftNomMax( 10.0 ),
			ChillerTdevNomMin( 0.0 ),
			ChillerTdevNomMax( 10.0 ),
			CAPFTXIter( 0 ),
			CAPFTXIterIndex( 0 ),
			CAPFTYIter( 0 ),
			CAPFTYIterIndex( 0 ),
			EIRFTXIter( 0 ),
			EIRFTXIterIndex( 0 ),
			EIRFTYIter( 0 ),
			EIRFTYIterIndex( 0 ),
			EIRFPLRTIter( 0 ),
			EIRFPLRTIterIndex( 0 ),
			EIRFPLRPLRIter( 0 ),
			EIRFPLRPLRIterIndex( 0 ),
			IterLimitExceededNum( 0 ),
			IterLimitErrIndex( 0 ),
			IterFailed( 0 ),
			IterFailedIndex( 0 ),
			DeltaTErrCount( 0 ),
			DeltaTErrCountIndex( 0 ),
			CWLoopNum( 0 ),
			CWLoopSideNum( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			CDLoopNum( 0 ),
			CDLoopSideNum( 0 ),
			CDBranchNum( 0 ),
			CDCompNum( 0 ),
			HRLoopNum( 0 ),
			HRLoopSideNum( 0 ),
			HRBranchNum( 0 ),
			HRCompNum( 0 ),
			CondMassFlowIndex( 0 ),
			PossibleSubcooling( false )
		{}
	};

	struct ReportVars
	{
		// Members
		Real64 ChillerPartLoadRatio; // reporting: Chiller PLR (Load/Capacity)
		Real64 ChillerCyclingRatio; // reporting: Chiller cycling ratio (time on/time step)
		Real64 ChillerFalseLoadRate; // reporting: Chiller false load over and above water side load [J]
		Real64 ChillerFalseLoad; // reporting: Chiller false load over and above water side load [W]
		Real64 Power; // reporting: Chiller power [W]
		Real64 QEvap; // reporting: Evaporator heat transfer rate [W]
		Real64 QCond; // reporting: Condenser heat transfer rate [W]
		Real64 Energy; // reporting: Chiller electric consumption [J]
		Real64 EvapEnergy; // reporting: Evaporator heat transfer energy [J]
		Real64 CondEnergy; // reporting: Condenser heat transfer energy [J]
		Real64 CondInletTemp; // reporting: Condenser inlet temperature [C]
		Real64 EvapInletTemp; // reporting: Evaporator inlet temperature [C]
		Real64 CondOutletTemp; // reporting: Condenser outlet temperature [C]
		Real64 EvapOutletTemp; // reporting: Evaporator outlet temperature [C]
		Real64 Evapmdot; // reporting: Evaporator mass flow rate [kg/s]
		Real64 Condmdot; // reporting: Condenser mass flow rate [kg/s]
		Real64 ActualCOP; // reporting: Coefficient of performance
		Real64 QHeatRecovery; // reporting: Heat recovered from water-cooled condenser [W]
		Real64 EnergyHeatRecovery; // reporting: Energy recovered from water-cooled condenser [J]
		Real64 HeatRecInletTemp; // reporting: Heat reclaim inlet temperature [C]
		Real64 HeatRecOutletTemp; // reporting: Heat reclaim outlet temperature [C]
		Real64 HeatRecMassFlow; // reporting: Heat reclaim mass flow rate [kg/s]
		Real64 ChillerCapFT; // reporting: Chiller capacity curve output value
		Real64 ChillerEIRFT; // reporting: Chiller EIRFT curve output value
		Real64 ChillerEIRFPLR; // reporting: Chiller EIRFPLR curve output value
		//  REAL(r64)    :: CondenserFanPowerUse  = 0.0d0 ! reporting: Air-cooled condenser fan power [W]
		//  REAL(r64)    :: CondenserFanEnergyConsumption = 0.0d0 ! reporting: Air-cooled condenser fan energy [J]
		Real64 ChillerCondAvgTemp; // reporting: average condenser temp for curves with Heat recovery [C]

		// Default Constructor
		ReportVars() :
			ChillerPartLoadRatio( 0.0 ),
			ChillerCyclingRatio( 0.0 ),
			ChillerFalseLoadRate( 0.0 ),
			ChillerFalseLoad( 0.0 ),
			Power( 0.0 ),
			QEvap( 0.0 ),
			QCond( 0.0 ),
			Energy( 0.0 ),
			EvapEnergy( 0.0 ),
			CondEnergy( 0.0 ),
			CondInletTemp( 0.0 ),
			EvapInletTemp( 0.0 ),
			CondOutletTemp( 0.0 ),
			EvapOutletTemp( 0.0 ),
			Evapmdot( 0.0 ),
			Condmdot( 0.0 ),
			ActualCOP( 0.0 ),
			QHeatRecovery( 0.0 ),
			EnergyHeatRecovery( 0.0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMassFlow( 0.0 ),
			ChillerCapFT( 0.0 ),
			ChillerEIRFT( 0.0 ),
			ChillerEIRFPLR( 0.0 ),
			ChillerCondAvgTemp( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< ReformulatedEIRChillerSpecs > ElecReformEIRChiller; // dimension to number of machines
	extern Array1D< ReportVars > ElecReformEIRChillerReport;

	// Functions

	void
	SimReformulatedEIRChiller(
		std::string const & EIRChillerType, // Type of chiller !unused1208
		std::string const & EIRChillerName, // User specified name of chiller
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int & CompIndex, // Chiller number pointer
		int const LoopNum, // plant loop index pointer
		bool const RunFlag, // Simulate chiller when TRUE
		bool const FirstIteration, // Initialize variables when TRUE
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 & MyLoad, // Loop demand component will meet [W]
		Real64 & MaxCap, // Maximum operating capacity of chiller [W]
		Real64 & MinCap, // Minimum operating capacity of chiller [W]
		Real64 & OptCap, // Optimal operating capacity of chiller [W]
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor, // sizing factor
		Real64 & TempCondInDesign,
		Real64 & TempEvapOutDesign
	);

	// End Reformulated EIR Chiller Module Driver Subroutine
	//******************************************************************************

	void
	GetElecReformEIRChillerInput();

	void
	InitElecReformEIRChiller(
		int const EIRChillNum, // Number of the current electric EIR chiller being simulated
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad // Current load put on chiller
	);

	void
	SizeElecReformEIRChiller( int const EIRChillNum );

	void
	ControlReformEIRChillerModel(
		int & EIRChillNum, // Chiller number
		Real64 & MyLoad, // Operating load [W]
		bool const RunFlag, // TRUE when chiller operating
		bool const FirstIteration, // TRUE when first iteration of timestep
		int const EquipFlowCtrl // Flow control mode for the equipment
	);

	void
	ReformEIRChillerHeatRecovery(
		int const EIRChillNum, // Number of the current electric EIR chiller being simulated
		Real64 & QCond, // Current condenser load [W]
		Real64 const CondMassFlow, // Current condenser mass flow [kg/s]
		Real64 const CondInletTemp, // Current condenser inlet temp [C]
		Real64 & QHeatRec // Amount of heat recovered [W]
	);

	void
	UpdateReformEIRChillerRecords(
		Real64 const MyLoad, // Current load [W]
		bool const RunFlag, // TRUE if chiller operating
		int const Num // Chiller number
	);

	Real64
	CondOutTempResidual(
		Real64 const FalsiCondOutTemp, // RegulaFalsi condenser outlet temperature result [C]
		Array1< Real64 > const & Par // Parameter array used to interface with RegulaFalsi solver
	);

	void
	CalcReformEIRChillerModel(
		int const EIRChillNum, // Chiller number
		Real64 & MyLoad, // Operating load [W]
		bool const RunFlag, // TRUE when chiller operating
		bool const FirstIteration, // TRUE when first iteration of timestep !unused1208
		int const EquipFlowCtrl, // Flow control mode for the equipment
		Real64 const FalsiCondOutTemp // RegulaFalsi condenser outlet temperature result [C]
	);

	void
	CheckMinMaxCurveBoundaries(
		int const EIRChillNum, // Number of the current electric EIR chiller being simulated
		bool const FirstIteration // TRUE when first iteration of timestep
	);

} // ChillerReformulatedEIR

} // EnergyPlus

#endif
