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

#ifndef PlantChillers_hh_INCLUDED
#define PlantChillers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace PlantChillers {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// Parameters for use in Chillers
	extern int const AirCooled;
	extern int const WaterCooled;
	extern int const EvapCooled;
	extern Real64 const KJtoJ; // convert Kjoules to joules

	//chiller flow modes
	extern int const FlowModeNotSet;
	extern int const ConstantFlow;
	extern int const NotModulated;
	extern int const LeavingSetPointModulated;

	// MODULE VARIABLE DECLARATIONS:
	extern int NumElectricChillers; // number of Electric chillers specified in input
	extern Real64 CondMassFlowRate; // Kg/s - condenser mass flow rate, water side
	extern Real64 EvapMassFlowRate; // Kg/s - evaporator mass flow rate, water side
	extern Real64 CondOutletTemp; // C - condenser outlet temperature, air or water side
	extern Real64 CondOutletHumRat; // kg/kg - condenser outlet humditiy ratio, air side
	extern Real64 EvapOutletTemp; // C - evaporator outlet temperature, water side
	extern Real64 Power; // W - rate of chiller energy use
	extern Real64 QEvaporator; // W - rate of heat transfer to the evaporator coil
	extern Real64 QCondenser; // W - rate of heat transfer to the condenser coil
	extern Real64 Energy; // J - chiller energy use
	extern Real64 EvaporatorEnergy; // J - rate of heat transfer to the evaporator coil
	extern Real64 CondenserEnergy; // J - rate of heat transfer to the condenser coil
	extern Real64 QHeatRecovered; // W - rate of heat transfer to the Heat Recovery coil
	extern Real64 HeatRecOutletTemp; // C - Heat Rec outlet temperature, water side
	extern Real64 AvgCondSinkTemp; // condenser temperature value for use in curves [C]
	extern Real64 ChillerCyclingRatio; // Cycling ratio for chiller when load is below MinPLR
	extern Real64 BasinHeaterPower; // Basin heater power (W)

	//engine driven:
	extern int NumEngineDrivenChillers; // number of EngineDriven chillers specified in input
	extern Real64 HeatRecInletTemp; // Inlet Temperature of the heat recovery fluid
	extern Real64 HeatRecMdotActual; // reporting: Heat Recovery Loop Mass flow rate
	extern Real64 HeatRecMdotDesign;
	extern Real64 QTotalHeatRecovered; // total heat recovered (W)
	extern Real64 QJacketRecovered; // heat recovered from jacket (W)
	extern Real64 QLubeOilRecovered; // heat recovered from lube (W)
	extern Real64 QExhaustRecovered; // exhaust gas heat recovered (W)
	extern Real64 FuelEnergyUseRate; // Fuel Energy used (W)
	extern Real64 TotalHeatEnergyRec; // total heat recovered (J)
	extern Real64 JacketEnergyRec; // heat recovered from jacket (J)
	extern Real64 LubeOilEnergyRec; // heat recovered from lube (J)
	extern Real64 ExhaustEnergyRec; // exhaust gas heat recovered (J)
	extern Real64 FuelEnergy; // Fuel Energy used (J)
	extern Real64 FuelMdot; // Fuel Amount used (Kg/s)
	extern Real64 ExhaustStackTemp; // Exhaust Stack Temperature (C)

	//gas turbine
	extern int NumGTChillers; // number of GT chillers specified in input

	// const COP
	extern int NumConstCOPChillers;
	extern Real64 EvapInletTemp;
	extern Real64 CondInletTemp;

	// DERIVED TYPE DEFINITIONS

	extern bool GetEngineDrivenInput; // then TRUE, calls subroutine to read input file.
	extern bool GetElectricInput; // then TRUE, calls subroutine to read input file.
	extern bool GetGasTurbineInput; // then TRUE, calls subroutine to read input file.
	extern bool GetConstCOPInput;

	//Merged routines

	// Electric Chiller

	// Engine Driven Chiller

	// Gas Turbine Chiller

	// Const COP

	// Types

	struct BaseChillerSpecs
	{
		// Members
		std::string Name; // user identifier
		int CondenserType; // Type of Condenser - Air or Water Cooled
		Real64 NomCap; // design nominal capacity of chiller
		bool NomCapWasAutoSized; // true if NomCap was autosize on input
		Real64 COP; // COP
		int FlowMode; // one of 3 modes for componet flow during operation
		bool ModulatedFlowSetToLoop; // True if the setpoint is missing at the outlet node
		bool ModulatedFlowErrDone; // true if setpoint warning issued
		bool HRSPErrDone; // TRUE if set point warning issued for heat recovery loop
		int EvapInletNodeNum; // Node number on the inlet side of the plant
		int EvapOutletNodeNum; // Node number on the outlet side of the plant
		int CondInletNodeNum; // Node number on the inlet side of the condenser
		int CondOutletNodeNum; // Node number on the outlet side of the condenser
		Real64 EvapVolFlowRate; // m**3/s - design nominal water volumetric flow rate through the evaporator
		bool EvapVolFlowRateWasAutoSized; //true if autosized design evap flow rate on input
		Real64 EvapMassFlowRateMax; // kg/s - design water mass flow rate through evaporator
		Real64 CondVolFlowRate; // m**3/s - design nominal water volumetric flow rate through the condenser
		bool CondVolFlowRateWasAutoSized; // true if previous was autosized
		Real64 CondMassFlowRateMax; // kg/s - design water mass flow rate through condenser
		int CWLoopNum; // chilled water plant loop index number
		int CWLoopSideNum; // chilled water plant loop side index
		int CWBranchNum; // chilled water plant loop branch index
		int CWCompNum; // chilled water plant loop component index
		int CDLoopNum; // condenser water plant loop index number
		int CDLoopSideNum; // condenser water plant loop side index
		int CDBranchNum; // condenser water plant loop branch index
		int CDCompNum; // condenser water plant loop component index
		Real64 SizFac; // sizing factor
		Real64 BasinHeaterPowerFTempDiff; // Basin heater capacity per degree C below setpoint (W/C)
		Real64 BasinHeaterSetPointTemp; // Setpoint temperature for basin heater operation (C)
		int BasinHeaterSchedulePtr; // Pointer to basin heater schedule
		int ErrCount1; // for recurring error messages
		int ErrCount2; // for recurring error messages
		std::string MsgBuffer1; // - buffer to print warning messages on following time step
		std::string MsgBuffer2; // - buffer to print warning messages on following time step
		Real64 MsgDataLast; // value of data when warning occurred (passed to Recurring Warn)
		bool PrintMessage; // logical to determine if message is valid
		int MsgErrorCount; // number of occurrences of warning
		bool CheckEquipName;
		bool PossibleSubcooling; // flag to indicate chiller is doing less cooling that requested
		int CondMassFlowIndex;

		// Default Constructor
		BaseChillerSpecs() :
			CondenserType( 0 ),
			NomCap( 0.0 ),
			NomCapWasAutoSized( false ),
			COP( 0.0 ),
			FlowMode( FlowModeNotSet ),
			ModulatedFlowSetToLoop( false ),
			ModulatedFlowErrDone( false ),
			HRSPErrDone( false ),
			EvapInletNodeNum( 0 ),
			EvapOutletNodeNum( 0 ),
			CondInletNodeNum( 0 ),
			CondOutletNodeNum( 0 ),
			EvapVolFlowRate( 0.0 ),
			EvapVolFlowRateWasAutoSized( false ),
			EvapMassFlowRateMax( 0.0 ),
			CondVolFlowRate( 0.0 ),
			CondVolFlowRateWasAutoSized( false ),
			CondMassFlowRateMax( 0.0 ),
			CWLoopNum( 0 ),
			CWLoopSideNum( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			CDLoopNum( 0 ),
			CDLoopSideNum( 0 ),
			CDBranchNum( 0 ),
			CDCompNum( 0 ),
			SizFac( 0.0 ),
			BasinHeaterPowerFTempDiff( 0.0 ),
			BasinHeaterSetPointTemp( 0.0 ),
			BasinHeaterSchedulePtr( 0 ),
			ErrCount1( 0 ),
			ErrCount2( 0 ),
			MsgDataLast( 0.0 ),
			PrintMessage( false ),
			MsgErrorCount( 0 ),
			CheckEquipName( true ),
			PossibleSubcooling( false ),
			CondMassFlowIndex( 0 )
		{}
	};

	struct ElectricChillerSpecs
	{
		// Members
		BaseChillerSpecs Base;
		Real64 MinPartLoadRat; // (Electric MIN) min allowed operating frac full load
		Real64 MaxPartLoadRat; // (Electric MAX) max allowed operating frac full load
		Real64 OptPartLoadRat; // (Electric BEST) optimal operating frac full load
		Real64 TempDesCondIn; // C - (Electric ADJTC(1)The design secondary loop fluid
		// temperature at the chiller condenser side inlet
		Real64 TempRiseCoef; // (Electric ADJTC(2)) correction factor for off ChillDesign oper.
		Real64 TempDesEvapOut; // C - (Electric ADJTC(3)The design primary loop fluid
		// temperature at the chiller evaporator side outlet
		Array1D< Real64 > CapRatCoef; // (Electric RCAVC() ) coeff of cap ratio poly fit
		Array1D< Real64 > PowerRatCoef; // (Electric ADJEC() ) coeff of power rat poly fit
		Array1D< Real64 > FullLoadCoef; // (Electric RPWRC() ) coeff of full load poly. fit
		Real64 TempLowLimitEvapOut; // C - low temperature shut off
		Real64 DesignHeatRecVolFlowRate; // m3/s, Design Water mass flow rate through heat recovery loop
		bool DesignHeatRecVolFlowRateWasAutoSized; // true if previous was input autosize.
		Real64 DesignHeatRecMassFlowRate; // kg/s, Design Water mass flow rate through heat recovery loop
		bool HeatRecActive; // True entered Heat Rec Vol Flow Rate >0
		int HeatRecInletNodeNum; // Node number on the heat recovery inlet side of the condenser
		int HeatRecOutletNodeNum; // Node number on the heat recovery outlet side of the condenser
		Real64 HeatRecCapacityFraction; // user input for heat recovery capacity fraction []
		Real64 HeatRecMaxCapacityLimit; // Capacity limit for Heat recovery, one time calc [W]
		int HeatRecSetPointNodeNum; // index for system node with the heat recover leaving setpoint
		int HeatRecInletLimitSchedNum; // index for schedule for the inlet high limit for heat recovery operation
		int HRLoopNum; // heat recovery water plant loop side index
		int HRLoopSideNum; // heat recovery water plant loop side index
		int HRBranchNum; // heat recovery water plant loop branch index
		int HRCompNum; // heat recovery water plant loop component index

		// Default Constructor
		ElectricChillerSpecs() :
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			OptPartLoadRat( 0.0 ),
			TempDesCondIn( 0.0 ),
			TempRiseCoef( 0.0 ),
			TempDesEvapOut( 0.0 ),
			CapRatCoef( 3, 0.0 ),
			PowerRatCoef( 3, 0.0 ),
			FullLoadCoef( 3, 0.0 ),
			TempLowLimitEvapOut( 0.0 ),
			DesignHeatRecVolFlowRate( 0.0 ),
			DesignHeatRecVolFlowRateWasAutoSized( false ),
			DesignHeatRecMassFlowRate( 0.0 ),
			HeatRecActive( false ),
			HeatRecInletNodeNum( 0 ),
			HeatRecOutletNodeNum( 0 ),
			HeatRecCapacityFraction( 0.0 ),
			HeatRecMaxCapacityLimit( 0.0 ),
			HeatRecSetPointNodeNum( 0 ),
			HeatRecInletLimitSchedNum( 0 ),
			HRLoopNum( 0 ),
			HRLoopSideNum( 0 ),
			HRBranchNum( 0 ),
			HRCompNum( 0 )
		{}
	};

	struct EngineDrivenChillerSpecs
	{
		// Members
		BaseChillerSpecs Base;
		std::string FuelType; // Type of Fuel - DIESEL, GASOLINE, GAS
		Real64 MinPartLoadRat; // (EngineDriven MIN) min allowed operating frac full load
		Real64 MaxPartLoadRat; // (EngineDriven MAX) max allowed operating frac full load
		Real64 OptPartLoadRat; // (EngineDriven BEST) optimal operating frac full load
		Real64 TempDesCondIn; // C - (EngineDriven ADJTC(1)The design secondary loop fluid
		// temperature at the chiller condenser side inlet
		Real64 TempRiseCoef; // (EngineDriven ADJTC(2)) correction factor for off ChillDesign oper.
		Real64 TempDesEvapOut; // C - (EngineDriven ADJTC(3)The design primary loop fluid
		// temperature at the chiller evaporator side outlet
		Array1D< Real64 > CapRatCoef; // (EngineDriven RCAVC() ) coeff of cap ratio poly fit
		Array1D< Real64 > PowerRatCoef; // (EngineDriven ADJEC() ) coeff of power rat poly fit
		Array1D< Real64 > FullLoadCoef; // (EngineDriven RPWRC() ) coeff of full load poly. fit
		Real64 TempLowLimitEvapOut; // C - low temperature shut off
		int ClngLoadtoFuelCurve; // Coeff of Shaft Power to Fuel Energy Input Coeff Poly Fit
		int RecJacHeattoFuelCurve; // Curve Index for Ratio of Recoverable Jacket Heat to
		int RecLubeHeattoFuelCurve; // Curve Index for Ratio of Recoverable Lube Oil Heat to
		int TotExhausttoFuelCurve; // Curve Index for Total Exhaust heat Input to Fuel Energy Input Coeffs Poly Fit
		Real64 ExhaustTemp; // (TEXDC) Exhaust Gas Temp to Fuel Energy Input
		int ExhaustTempCurve; // Curve Index for Exhaust Gas Temp to Fuel Energy Input Coeffs Poly Fit
		Real64 UA; // (UACDC) exhaust gas Heat Exchanger UA to Capacity
		Array1D< Real64 > UACoef; // Heat Exchanger UA Coeffs Poly Fit
		Real64 MaxExhaustperPowerOutput; // MAX EXHAUST FLOW PER W DSL POWER OUTPUT COEFF
		Real64 DesignMinExitGasTemp; // Steam Saturation Temperature
		Real64 FuelHeatingValue; // Heating Value of Fuel in kJ/kg
		Real64 DesignHeatRecVolFlowRate; // m3/s, Design Water mass flow rate through heat recovery loop
		Real64 DesignHeatRecMassFlowRate; // kg/s, Design Water mass flow rate through heat recovery loop
		bool HeatRecActive; // True entered Heat Rec Vol Flow Rate >0
		int HeatRecInletNodeNum; // Node number on the heat recovery inlet side of the condenser
		int HeatRecOutletNodeNum; // Node number on the heat recovery outlet side of the condenser
		Real64 HeatRecMaxTemp; // Max Temp that can be produced in heat recovery
		int HRLoopNum; // heat recovery water plant loop side index
		int HRLoopSideNum; // heat recovery water plant loop side index
		int HRBranchNum; // heat recovery water plant loop branch index
		int HRCompNum; // heat recovery water plant loop component index

		// Default Constructor
		EngineDrivenChillerSpecs() :
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			OptPartLoadRat( 0.0 ),
			TempDesCondIn( 0.0 ),
			TempRiseCoef( 0.0 ),
			TempDesEvapOut( 0.0 ),
			CapRatCoef( 3, 0.0 ),
			PowerRatCoef( 3, 0.0 ),
			FullLoadCoef( 3, 0.0 ),
			TempLowLimitEvapOut( 0.0 ),
			ClngLoadtoFuelCurve( 0 ),
			RecJacHeattoFuelCurve( 0 ),
			RecLubeHeattoFuelCurve( 0 ),
			TotExhausttoFuelCurve( 0 ),
			ExhaustTemp( 0.0 ),
			ExhaustTempCurve( 0 ),
			UA( 0.0 ),
			UACoef( 2, 0.0 ),
			MaxExhaustperPowerOutput( 0.0 ),
			DesignMinExitGasTemp( 0.0 ),
			FuelHeatingValue( 0.0 ),
			DesignHeatRecVolFlowRate( 0.0 ),
			DesignHeatRecMassFlowRate( 0.0 ),
			HeatRecActive( false ),
			HeatRecInletNodeNum( 0 ),
			HeatRecOutletNodeNum( 0 ),
			HeatRecMaxTemp( 0.0 ),
			HRLoopNum( 0 ),
			HRLoopSideNum( 0 ),
			HRBranchNum( 0 ),
			HRCompNum( 0 )
		{}

	};

	struct GTChillerSpecs
	{
		// Members
		BaseChillerSpecs Base;
		std::string FuelType; // Type of Fuel - DIESEL, GASOLINE, GAS
		Real64 MinPartLoadRat; // (GT MIN) min allowed operating frac full load
		Real64 MaxPartLoadRat; // (GT MAX) max allowed operating frac full load
		Real64 OptPartLoadRat; // (GT BEST) optimal operating frac full load
		Real64 TempDesCondIn; // C - (GT ADJTC(1)The design secondary loop fluid
		// temperature at the chiller condenser side inlet
		Real64 TempRiseCoef; // (GT ADJTC(2)) correction factor for off ChillDesign oper.
		Real64 TempDesEvapOut; // C - (GT ADJTC(3)The design primary loop fluid
		// temperature at the chiller evaporator side outlet
		Array1D< Real64 > CapRatCoef; // (GT RCAVC() ) coeff of cap ratio poly fit
		Array1D< Real64 > PowerRatCoef; // (GT ADJEC() ) coeff of power rat poly fit
		Array1D< Real64 > FullLoadCoef; // (GT RPWRC() ) coeff of full load poly. fit
		Real64 TempLowLimitEvapOut; // C - low temperature shut off
		// "special" GT chiller input parameters
		Real64 FuelEnergyIn; // (EFUEL) Amount of Fuel Energy Required to run gas turbine
		Array1D< Real64 > PLBasedFuelInputCoef; // (FUL1GC) Part Load Ratio Based Fuel Input Coefficients Poly Fit
		Array1D< Real64 > TempBasedFuelInputCoef; // (FUL2GC) Ambient Temperature Based Fuel Input Coeff Poly Fit
		Real64 ExhaustFlow; // (FEX) Exhaust Gas Flow Rate cubic meters per second
		Array1D< Real64 > ExhaustFlowCoef; // (FEXGC) Exhaust Gas Flow Rate Input Coef Poly Fit
		Real64 ExhaustTemp; // (TEX) Exhaust Gas Temperature in C
		Array1D< Real64 > PLBasedExhaustTempCoef; // (TEX1GC) Part Load Ratio Based Exhaust Temperature Input Coeffs Poly Fit
		Array1D< Real64 > TempBasedExhaustTempCoef; // (TEX2GC) Ambient Temperature Based Exhaust Gas Temp to
		// Fuel Energy Input Coeffs Poly Fit
		Real64 HeatRecLubeEnergy; // (ELUBE) Recoverable Lube Oil Energy
		Real64 HeatRecLubeRate; // (ELUBE) Recoverable Lube Oil Rate of Rwecovery (W)
		Array1D< Real64 > HeatRecLubeEnergyCoef; // (ELUBEGC)  Recoverable Lube Oil Energy Input Coef Poly Fit
		Real64 UAtoCapRat; // (UACGC) Heat Exchanger UA to Capacity
		Array1D< Real64 > UAtoCapCoef; // Heat Exchanger UA to Capacity Coeffs Poly Fit
		Real64 GTEngineCapacity; // Capacity of GT Unit attached to Chiller
		bool GTEngineCapacityWasAutoSized; // true if previous field was autosize on inpt
		Real64 MaxExhaustperGTPower; // Max Exhaust Flow per KW Power Out
		Real64 DesignSteamSatTemp; // Steam Saturation Temperature
		Real64 ExhaustStackTemp; // Temperature of Exhaust Gases
		int HeatRecInletNodeNum; // Node number on the heat recovery inlet side of the condenser
		int HeatRecOutletNodeNum; // Node number on the heat recovery outlet side of the condenser
		Real64 HeatRecInletTemp; // Inlet Temperature of the heat recovery fluid
		Real64 HeatRecOutletTemp; // Outlet Temperature of the heat recovery fluid
		Real64 HeatRecMdot; // reporting: Heat Recovery Loop Mass flow rate
		Real64 DesignHeatRecVolFlowRate; // m3/s, Design Water mass flow rate through heat recovery loop
		Real64 DesignHeatRecMassFlowRate; // kg/s, Design Water mass flow rate through heat recovery loop
		bool HeatRecActive; // True entered Heat Rec Vol Flow Rate >0
		Real64 FuelHeatingValue; // Heating Value of Fuel in kJ/kg
		Real64 HeatRecMaxTemp; // Max Temp that can be produced in heat recovery
		int HRLoopNum; // heat recovery water plant loop side index
		int HRLoopSideNum; // heat recovery water plant loop side index
		int HRBranchNum; // heat recovery water plant loop branch index
		int HRCompNum; // heat recovery water plant loop component index

		// Default Constructor
		GTChillerSpecs() :
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			OptPartLoadRat( 0.0 ),
			TempDesCondIn( 0.0 ),
			TempRiseCoef( 0.0 ),
			TempDesEvapOut( 0.0 ),
			CapRatCoef( 3, 0.0 ),
			PowerRatCoef( 3, 0.0 ),
			FullLoadCoef( 3, 0.0 ),
			TempLowLimitEvapOut( 0.0 ),
			FuelEnergyIn( 0.0 ),
			PLBasedFuelInputCoef( 3, 0.0 ),
			TempBasedFuelInputCoef( 3, 0.0 ),
			ExhaustFlow( 0.0 ),
			ExhaustFlowCoef( 3, 0.0 ),
			ExhaustTemp( 0.0 ),
			PLBasedExhaustTempCoef( 3, 0.0 ),
			TempBasedExhaustTempCoef( 3, 0.0 ),
			HeatRecLubeEnergy( 0.0 ),
			HeatRecLubeRate( 0.0 ),
			HeatRecLubeEnergyCoef( 3, 0.0 ),
			UAtoCapRat( 0.0 ),
			UAtoCapCoef( 3, 0.0 ),
			GTEngineCapacity( 0.0 ),
			GTEngineCapacityWasAutoSized( false ),
			MaxExhaustperGTPower( 0.0 ),
			DesignSteamSatTemp( 0.0 ),
			ExhaustStackTemp( 0.0 ),
			HeatRecInletNodeNum( 0 ),
			HeatRecOutletNodeNum( 0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMdot( 0.0 ),
			DesignHeatRecVolFlowRate( 0.0 ),
			DesignHeatRecMassFlowRate( 0.0 ),
			HeatRecActive( false ),
			FuelHeatingValue( 0.0 ),
			HeatRecMaxTemp( 0.0 ),
			HRLoopNum( 0 ),
			HRLoopSideNum( 0 ),
			HRBranchNum( 0 ),
			HRCompNum( 0 )
		{}
	};

	struct ConstCOPChillerSpecs
	{
		// Members
		BaseChillerSpecs Base;

		// Default Constructor
		ConstCOPChillerSpecs()
		{}

	};

	struct BaseReportVars
	{
		// Members
		Real64 Power;
		Real64 QEvap;
		Real64 QCond;
		Real64 Energy;
		Real64 EvapEnergy;
		Real64 CondEnergy;
		Real64 CondInletTemp;
		Real64 EvapInletTemp;
		Real64 CondOutletTemp;
		Real64 EvapOutletTemp;
		Real64 Evapmdot;
		Real64 Condmdot;
		Real64 BasinHeaterPower; // Basin heater power (W)
		Real64 BasinHeaterConsumption; // Basin heater energy consumption (J)

		// Default Constructor
		BaseReportVars() :
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
			BasinHeaterPower( 0.0 ),
			BasinHeaterConsumption( 0.0 )
		{}
	};

	struct ElectricReportVars
	{
		// Members
		BaseReportVars Base;
		Real64 ActualCOP;
		Real64 QHeatRecovery;
		Real64 EnergyHeatRecovery;
		Real64 HeatRecInletTemp;
		Real64 HeatRecOutletTemp;
		Real64 HeatRecMassFlow;
		Real64 ChillerCondAvgTemp; // the effective condenser temperature for chiller performance [C]

		// Default Constructor
		ElectricReportVars() :
			ActualCOP( 0.0 ),
			QHeatRecovery( 0.0 ),
			EnergyHeatRecovery( 0.0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMassFlow( 0.0 ),
			ChillerCondAvgTemp( 0.0 )
		{}
	};

	struct EngineDrivenReportVars
	{
		// Members
		BaseReportVars Base;
		Real64 QJacketRecovered; // reporting: Heat Recovered from Jacket (W)
		Real64 QLubeOilRecovered; // reporting: Heat Recovered from Lubricant (W)
		Real64 QExhaustRecovered; // reporting: exhaust gas heat recovered (W)
		Real64 QTotalHeatRecovered; // reporting: Total Heat Recovered (W)
		Real64 TotalHeatEnergyRec; // reporting: total heat recovered (J)
		Real64 JacketEnergyRec; // reporting: heat recovered from jacket (J)
		Real64 LubeOilEnergyRec; // reporting: heat recovered from lube (J)
		Real64 ExhaustEnergyRec; // reporting: exhaust gas heat recovered (J)
		Real64 FuelEnergy; // reporting: Fuel Energy used (J)
		Real64 FuelEnergyUseRate; // reporting: Fuel Energy used (W)
		Real64 FuelMdot; // reporting: Fuel used (Kg/s)
		Real64 ExhaustStackTemp; // reporting: Exhaust Stack Temperature (C)
		Real64 HeatRecInletTemp; // reporting: Heat Recovery Loop Inlet Temperature (C)
		Real64 HeatRecOutletTemp; // reporting: Heat Recovery Loop Outlet Temperature (C)
		Real64 HeatRecMdot; // reporting: Heat Recovery Loop Mass flow rate (kg/s)
		Real64 FuelCOP; // reporting: Fuel COP [delivered cooling rate/fuel energy input rate] (W/W)

		// Default Constructor
		EngineDrivenReportVars() :
			QJacketRecovered( 0.0 ),
			QLubeOilRecovered( 0.0 ),
			QExhaustRecovered( 0.0 ),
			QTotalHeatRecovered( 0.0 ),
			TotalHeatEnergyRec( 0.0 ),
			JacketEnergyRec( 0.0 ),
			LubeOilEnergyRec( 0.0 ),
			ExhaustEnergyRec( 0.0 ),
			FuelEnergy( 0.0 ),
			FuelEnergyUseRate( 0.0 ),
			FuelMdot( 0.0 ),
			ExhaustStackTemp( 0.0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMdot( 0.0 ),
			FuelCOP( 0.0 )
		{}

	};

	struct GasTurbineReportVars
	{
		// Members
		BaseReportVars Base;
		Real64 HeatRecLubeEnergy; // reporting: Heat Recovered from Lubricant(J)
		Real64 HeatRecLubeRate; // reporting: Recoverable Lube Oil Rate of Rwecovery (W)
		Real64 FuelEnergyUsed; // reporting: Fuel Energy used
		Real64 FuelEnergyUsedRate; // reporting: Fuel energy used rate (fuel consumption rate)
		Real64 FuelMassUsed; // reporting: Fuel Amount used
		Real64 FuelMassUsedRate; // reporting: Fuel amount used (fuel Mass consumption rate)
		Real64 ExhaustStackTemp; // reporting: Exhaust Stack Temperature
		Real64 HeatRecInletTemp; // reporting: Heat Recovery Loop Inlet Temperature
		Real64 HeatRecOutletTemp; // reporting: Heat Recovery Loop Outlet Temperature
		Real64 HeatRecMdot; // reporting: Heat Recovery Loop Mass flow rate
		Real64 FuelCOP; // reporting: Fuel coefficient of performance (Qevap/FuelEnergyUsedRate)

		// Default Constructor
		GasTurbineReportVars() :
			HeatRecLubeEnergy( 0.0 ),
			HeatRecLubeRate( 0.0 ),
			FuelEnergyUsed( 0.0 ),
			FuelEnergyUsedRate( 0.0 ),
			FuelMassUsed( 0.0 ),
			FuelMassUsedRate( 0.0 ),
			ExhaustStackTemp( 0.0 ),
			HeatRecInletTemp( 0.0 ),
			HeatRecOutletTemp( 0.0 ),
			HeatRecMdot( 0.0 ),
			FuelCOP( 0.0 )
		{}
	};

	struct ConstCOPReportVars
	{
		// Members
		BaseReportVars Base;
		Real64 ActualCOP;

		// Default Constructor
		ConstCOPReportVars() :
			ActualCOP( 0.0 )
		{}
	};

	// Object Data
	extern Array1D< ElectricChillerSpecs > ElectricChiller; // dimension to number of machines
	extern Array1D< ElectricReportVars > ElectricChillerReport;
	extern Array1D< EngineDrivenChillerSpecs > EngineDrivenChiller; // dimension to number of machines
	extern Array1D< EngineDrivenReportVars > EngineDrivenChillerReport;
	extern Array1D< GTChillerSpecs > GTChiller; // dimension to number of machines
	extern Array1D< GasTurbineReportVars > GTChillerReport;
	extern Array1D< ConstCOPChillerSpecs > ConstCOPChiller; // dimension to number of machines
	extern Array1D< ConstCOPReportVars > ConstCOPChillerReport;

	// Functions

	void
	clear_state();

	void
	SimChiller(
		int const LoopNum, // Flow control mode for the equipment
		int const LoopSide, // chiller number pointer
		int const ChillerType, // type of chiller
		std::string const & ChillerName, // user specified name of chiller
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int & CompIndex, // chiller number pointer
		bool const RunFlag, // simulate chiller when TRUE
		bool const FirstHVACIteration, // initialize variables when TRUE
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 & MyLoad, // loop demand component will meet
		Real64 & MaxCap, // W - maximum operating capacity of chiller
		Real64 & MinCap, // W - minimum operating capacity of chiller
		Real64 & OptCap, // W - optimal operating capacity of chiller
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor, // sizing factor
		Real64 & TempCondInDesign, // design condenser inlet temperature, water side
		Real64 & TempEvapOutDesign // design evaporator outlet temperature, water side
	);

	void
	GetElectricChillerInput();

	void
	GetEngineDrivenChillerInput();

	void
	GetGTChillerInput();

	void
	GetConstCOPChillerInput();

	void
	InitElectricChiller(
		int const ChillNum, // number of the current electric chiller being simulated
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad
	);

	void
	InitEngineDrivenChiller(
		int const ChillNum, // number of the current engine driven chiller being simulated
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad
	);

	void
	InitGTChiller(
		int const ChillNum, // number of the current engine driven chiller being simulated
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad
	);

	void
	InitConstCOPChiller(
		int const ChillNum, // number of the current electric chiller being simulated
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad
	);

	void
	SizeElectricChiller( int const ChillNum );

	void
	SizeEngineDrivenChiller( int const ChillNum );

	void
	SizeGTChiller( int const ChillNum );

	void
	SizeConstCOPChiller( int const ChillNum );

	void
	CalcElectricChillerModel(
		int & ChillNum, // chiller number
		Real64 & MyLoad, // operating load
		int const EquipFlowCtrl, // Flow control mode for the equipment
		bool const RunFlag // TRUE when chiller operating
	);

	void
	CalcEngineDrivenChillerModel(
		int & ChillerNum, // chiller number
		Real64 & MyLoad, // operating load
		bool const RunFlag, // TRUE when chiller operating
		int const EquipFlowCtrl // Flow control mode for the equipment
	);

	void
	CalcGTChillerModel(
		int & ChillerNum, // chiller number
		Real64 & MyLoad, // operating load
		bool const RunFlag, // TRUE when chiller operating
		int const EquipFlowCtrl // Flow control mode for the equipment
	);

	void
	CalcConstCOPChillerModel(
		int const ChillNum,
		Real64 & MyLoad,
		bool const RunFlag,
		int const EquipFlowCtrl // Flow control mode for the equipment
	);

	void
	CalcElectricChillerHeatRecovery(
		int const ChillNum, // number of the current electric chiller being simulated
		Real64 & QCond, // current condenser load
		Real64 const CondMassFlow, // current condenser Mass Flow
		Real64 const CondInletTemp, // current condenser Inlet Temp
		Real64 & QHeatRec // amount of heat recovered
	);

	void
	CalcEngineChillerHeatRec(
		int const ChillerNum, // Chiller number
		Real64 const EnergyRecovered, // Amount of heat recovered
		Real64 & HeatRecRatio // Max Heat recovery ratio
	);

	void
	UpdateElectricChillerRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if chiller operating
		int const Num // chiller number
	);

	// End of EngineDriven Chiller Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the EngineDriven Chiller Module
	// *****************************************************************************

	void
	UpdateEngineDrivenChiller(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if chiller operating
		int const Num // chiller number
	);

	void
	UpdateGTChillerRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if chiller operating
		int const Num // chiller number
	);

	void
	UpdateConstCOPChillerRecords(
		Real64 const MyLoad, // unused1208
		bool const RunFlag, // unused1208
		int const Num
	);

	// End of Record Keeping subroutines for the Const COP Chiller Module
	// *****************************************************************************

} // PlantChillers


} // EnergyPlus

#endif
