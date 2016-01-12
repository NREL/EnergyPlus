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

#ifndef PlantCentralGSHP_hh_INCLUDED
#define PlantCentralGSHP_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

// Contents:
// CentralHeatPumpSystem (CGSHP) System
// ChillerHeaterPerformance:Electric:EIR

namespace PlantCentralGSHP {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// Chiller type parameters: Only water cooled chiller is allowed
	extern int const WaterCooled;
	extern int const SmartMixing;
	extern int const FullyMixed;
	extern bool SimulClgDominant;
	extern bool SimulHtgDominant;

	// MODULE VARIABLE DECLARATIONS:
	extern bool GetInputWrapper; // When TRUE, calls subroutine to read input file.
	extern int NumWrappers; // Number of Wrappers specified in input
	extern int NumChillerHeaters; // Number of Chiller/heaters specified in input
	extern Real64 CondenserFanPower; // Condenser Fan Power (fan cycles with compressor) [W]
	extern Real64 ChillerCapFT; // Chiller/heater capacity fraction (evaluated as a function of temperature)
	extern Real64 ChillerEIRFT; // Chiller/heater electric input ratio (EIR = 1 / COP) as a function of temperature
	extern Real64 ChillerEIRFPLR; // Chiller/heater EIR as a function of part-load ratio (PLR)
	extern Real64 ChillerPartLoadRatio; // Chiller/heater part-load ratio (PLR)
	extern Real64 ChillerCyclingRatio; // Chiller/heater cycling ratio
	extern Real64 ChillerFalseLoadRate; // Chiller/heater false load over and above the water-side load [W]

	// Type defining the component specifications

	extern Array1D_bool CheckEquipName;
	extern Array1D_bool CHCheckEquipName;
	extern Array1D_bool HPCheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE ChillerElectricEIR

	// Types

	struct CGSHPNodeData
	{
		// Members
		Real64 Temp; // {C}
		Real64 TempMin; // {C}
		Real64 TempSetPoint; // SensedNodeFlagValue ! {C}
		Real64 MassFlowRate; // {kg/s}
		Real64 MassFlowRateMin; // {kg/s}
		Real64 MassFlowRateMax; // SensedNodeFlagValue ! {kg/s}
		Real64 MassFlowRateMinAvail; // {kg/s}
		Real64 MassFlowRateMaxAvail; // {kg/s}
		Real64 MassFlowRateSetPoint; // {kg/s}
		Real64 MassFlowRateRequest; // {kg/s}

		// Default Constructor
		CGSHPNodeData() :
			Temp( 0.0 ),
			TempMin( 0.0 ),
			TempSetPoint( 0.0 ),
			MassFlowRate( 0.0 ),
			MassFlowRateMin( 0.0 ),
			MassFlowRateMax( 0.0 ),
			MassFlowRateMinAvail( 0.0 ),
			MassFlowRateMaxAvail( 0.0 ),
			MassFlowRateSetPoint( 0.0 ),
			MassFlowRateRequest( 0.0 )
		{}
	};

	struct WrapperComponentSpecs
	{
		// Members
		std::string WrapperPerformanceObjectType; // Component type
		std::string WrapperComponentName; // Component name
		int WrapperPerformanceObjectIndex; // Component index in the input array
		std::string WrapperPerformanceObjectSch; // Component operation schedule
		int WrapperIdenticalObjectNum; // Number of identical objects
		int CHSchedPtr; // Index to schedule

		// Default Constructor
		WrapperComponentSpecs() :
			WrapperPerformanceObjectIndex( 0 ),
			WrapperIdenticalObjectNum( 0 ),
			CHSchedPtr( 0 )
		{}
	};

	struct ChillerHeaterSpecs
	{
		// Members
		std::string Name; // Name of the Chiller Heater object
		std::string CondModeCooling; // Cooling mode temperature curve input variable
		std::string CondModeHeating; // Clg/Htg mode temperature curve input variable
		std::string CondMode; // Current mode temperature curve input variable
		bool ConstantFlow; // True if this is a Constant Flow Chiller
		bool VariableFlow; // True if this is a Variable Flow Chiller
		bool CoolSetPointSetToLoop; // True if the setpoint is missing at the outlet node
		bool HeatSetPointSetToLoop; // True if the setpoint is missing at the outlet node
		bool CoolSetPointErrDone; // true if setpoint warning issued
		bool HeatSetPointErrDone; // true if setpoint warning issued
		bool PossibleSubcooling; // flag to indicate chiller is doing less cooling that requested
		int ChillerHeaterNum; // Chiller heater number
		int CondenserType; // Type of Condenser - only water cooled is allowed
		int ChillerCapFTCooling; // Cooling capacity function of temperature curve index
		int ChillerEIRFTCooling; // Elec Input to Cooling Output ratio function of temperature curve index
		int ChillerEIRFPLRCooling; // Elec Input to cooling output ratio function of PLR curve index
		int ChillerCapFTHeating; // Clg/Htg capacity function of temperature curve index
		int ChillerEIRFTHeating; // Elec Input to Clg/Htg Output ratio function of temperature curve index
		int ChillerEIRFPLRHeating; // Elec Input to Clg/Htg output ratio function of PLR curve index
		int ChillerCapFT; // Capacity function of temperature curve index
		int ChillerEIRFT; // Elec Input to demand output ratio function of temperature curve index
		int ChillerEIRFPLR; // Elec Input to demand output ratio function of PLR curve index
		int EvapInletNodeNum; // Node number on the inlet side of the plant (evaporator side)
		int EvapOutletNodeNum; // Node number on the outlet side of the plant (evaporator side)
		int CondInletNodeNum; // Node number on the inlet side of the condenser
		int CondOutletNodeNum; // Node number on the outlet side of the condenser
		int ChillerCapFTError; // Used for negative capacity as a function of temp warnings
		int ChillerCapFTErrorIndex; // Used for negative capacity as a function of temp warnings
		int ChillerEIRFTError; // Used for negative EIR as a function of temp warnings
		int ChillerEIRFTErrorIndex; // Used for negative EIR as a function of temp warnings
		int ChillerEIRFPLRError; // Used for negative EIR as a function of PLR warnings
		int ChillerEIRFPLRErrorIndex; // Used for negative EIR as a function of PLR warnings
		int ChillerEIRRefTempErrorIndex; // Used for reference temperature problems
		int DeltaTErrCount; // Evaporator delta T equals 0 for variable flow chiller warning messages
		int DeltaTErrCountIndex; // Index to evaporator delta T = 0 for variable flow chiller warning messages
		int CondMassFlowIndex; // Index to condenser mass flow rate
		Real64 RefCapCooling; // Reference cooling-mode evaporator capacity [W]
		bool RefCapCoolingWasAutoSized; // true if reference cooling capacity was autosize on input
		Real64 RefCOPCooling; // Reference cooling-mode COP
		Real64 TempRefEvapOutCooling; // Reference cooling-mode evaporator leaving temperature [C]
		Real64 TempRefCondInCooling; // Reference cooling-mode condenser entering temperature [C]
		Real64 TempRefCondOutCooling; // Reference cooling-mode condenser leaving temperature [C]
		Real64 MaxPartLoadRatCooling; // Maximum Part load ratio in cooling mode
		Real64 OptPartLoadRatCooling; // Optimum Part load ratio in cooling mode
		Real64 MinPartLoadRatCooling; // minimum Part load ratio in cooling mode
		Real64 ClgHtgToCoolingCapRatio; // ratio of clg/htg-mode evaporator capacity to cooling-mode evap. cap
		Real64 ClgHtgtoCogPowerRatio; // ratio of clg/htg-mode evaporator power to cooling-mode evap. power
		Real64 RefCapClgHtg; // Reference clg/htg-mode evaporator capacity [W]
		Real64 RefCOPClgHtg; // Reference clg/htg-mode COP
		Real64 RefPowerClgHtg; // Reference clg/htg-mode evaporator power [W]
		Real64 TempRefEvapOutClgHtg; // Reference clg/htg-mode evaporator leaving temperature [C]
		Real64 TempRefCondInClgHtg; // Reference clg/htg-mode condenser entering temperature [C]
		Real64 TempRefCondOutClgHtg; // Reference clg/htg-mode condenser leaving temperature [C]
		Real64 TempLowLimitEvapOut; // Low temperature shut off [C]
		Real64 MaxPartLoadRatClgHtg; // Maximum Part load ratio in simultaneous heating/cooling mode
		Real64 OptPartLoadRatClgHtg; // Optimum Part load ratio in simultaneous heating/cooling mode
		Real64 MinPartLoadRatClgHtg; // minimum Part load ratio in simultaneous heating/cooling mode
		CGSHPNodeData EvapInletNode; // Chiller heater evaperator inlet node
		CGSHPNodeData EvapOutletNode; // Chiller heater evaperator outlet node
		CGSHPNodeData CondInletNode; // Chiller heater condenser inlet node
		CGSHPNodeData CondOutletNode; // Chiller heater condenser outlet node
		Real64 EvapVolFlowRate; // Reference water volumetric flow rate through the evaporator [m3/s]
		bool EvapVolFlowRateWasAutoSized; // true if evaporator flow rate was autosize on input
		Real64 tmpEvapVolFlowRate; // temporary ref water vol flow rate for intermediate sizing [m3/s]
		Real64 CondVolFlowRate; // Reference water volumetric flow rate through the condenser [m3/s]
		bool CondVolFlowRateWasAutoSized; // true if condenser flow rate was autosize on input
		Real64 tmpCondVolFlowRate; // temporary ref water vol flow rate for intermediate sizing [m3/s]
		Real64 CondMassFlowRateMax; // Reference water mass flow rate through condenser [kg/s]
		Real64 EvapMassFlowRateMax; // Reference water mass flow rate through evaporator [kg/s]
		Real64 Evapmdot; // Evaporator mass flow rate [kg/s]
		Real64 Condmdot; // Condenser mass flow rate [kg/s]
		Real64 DesignHotWaterVolFlowRate; // Design hot water volumetric flow rate through the condenser [m3/s]
		Real64 OpenMotorEff; // Open chiller motor efficiency [fraction, 0 to 1]
		Real64 SizFac; // sizing factor
		Real64 RefCap; // Reference evaporator capacity [W]
		Real64 RefCOP; // Reference COP
		Real64 TempRefEvapOut; // Reference evaporator leaving temperature [C]
		Real64 TempRefCondIn; // Reference condenser entering temperature [C]
		Real64 TempRefCondOut; // Reference condenser leaving temperature [C]
		Real64 OptPartLoadRat; // Optimal operating fraction of full load
		Real64 ChillerEIRFPLRMin; // Minimum value of PLR from EIRFPLR curve
		Real64 ChillerEIRFPLRMax; // Maximum value of PLR from EIRFPLR curve

		// Default Constructor
		ChillerHeaterSpecs() :
			ConstantFlow( false ),
			VariableFlow( false ),
			CoolSetPointSetToLoop( false ),
			HeatSetPointSetToLoop( false ),
			CoolSetPointErrDone( false ),
			HeatSetPointErrDone( false ),
			PossibleSubcooling( false ),
			ChillerHeaterNum( 1 ),
			CondenserType( 0 ),
			ChillerCapFTCooling( 0 ),
			ChillerEIRFTCooling( 0 ),
			ChillerEIRFPLRCooling( 0 ),
			ChillerCapFTHeating( 0 ),
			ChillerEIRFTHeating( 0 ),
			ChillerEIRFPLRHeating( 0 ),
			ChillerCapFT( 0 ),
			ChillerEIRFT( 0 ),
			ChillerEIRFPLR( 0 ),
			EvapInletNodeNum( 0 ),
			EvapOutletNodeNum( 0 ),
			CondInletNodeNum( 0 ),
			CondOutletNodeNum( 0 ),
			ChillerCapFTError( 0 ),
			ChillerCapFTErrorIndex( 0 ),
			ChillerEIRFTError( 0 ),
			ChillerEIRFTErrorIndex( 0 ),
			ChillerEIRFPLRError( 0 ),
			ChillerEIRFPLRErrorIndex( 0 ),
			ChillerEIRRefTempErrorIndex( 0 ),
			DeltaTErrCount( 0 ),
			DeltaTErrCountIndex( 0 ),
			CondMassFlowIndex( 0 ),
			RefCapCooling( 0.0 ),
			RefCapCoolingWasAutoSized( false ),
			RefCOPCooling( 0.0 ),
			TempRefEvapOutCooling( 0.0 ),
			TempRefCondInCooling( 0.0 ),
			TempRefCondOutCooling( 0.0 ),
			MaxPartLoadRatCooling( 0.0 ),
			OptPartLoadRatCooling( 0.0 ),
			MinPartLoadRatCooling( 0.0 ),
			ClgHtgToCoolingCapRatio( 0.0 ),
			ClgHtgtoCogPowerRatio( 0.0 ),
			RefCapClgHtg( 0.0 ),
			RefCOPClgHtg( 0.0 ),
			RefPowerClgHtg( 0.0 ),
			TempRefEvapOutClgHtg( 0.0 ),
			TempRefCondInClgHtg( 0.0 ),
			TempRefCondOutClgHtg( 0.0 ),
			TempLowLimitEvapOut( 0.0 ),
			MaxPartLoadRatClgHtg( 0.0 ),
			OptPartLoadRatClgHtg( 0.0 ),
			MinPartLoadRatClgHtg( 0.0 ),
			EvapVolFlowRate( 0.0 ),
			EvapVolFlowRateWasAutoSized( false ),
			tmpEvapVolFlowRate( 0.0 ),
			CondVolFlowRate( 0.0 ),
			CondVolFlowRateWasAutoSized( false ),
			tmpCondVolFlowRate( 0.0 ),
			CondMassFlowRateMax( 0.0 ),
			EvapMassFlowRateMax( 0.0 ),
			Evapmdot( 0.0 ),
			Condmdot( 0.0 ),
			DesignHotWaterVolFlowRate( 0.0 ),
			OpenMotorEff( 0.0 ),
			SizFac( 0.0 ),
			RefCap( 0.0 ),
			RefCOP( 0.0 ),
			TempRefEvapOut( 0.0 ),
			TempRefCondIn( 0.0 ),
			TempRefCondOut( 0.0 ),
			OptPartLoadRat( 0.0 ),
			ChillerEIRFPLRMin( 0.0 ),
			ChillerEIRFPLRMax( 0.0 )
		{}
	};

	struct CHReportVars
	{
		// Members
		int CurrentMode; // 0-off; 1-cooling only; 2-heating-only; 3-simutaneouls heat/cool
		Real64 ChillerPartLoadRatio; // Chiller PLR (Load/Capacity)
		Real64 ChillerCyclingRatio; // Chiller cycling ratio (time on/time step)
		Real64 ChillerFalseLoad; // Chiller false load over and above water side load [J]
		Real64 ChillerFalseLoadRate; // Chiller false load rate over and above water side load [W]
		Real64 CoolingPower; // Chiller power, W
		Real64 HeatingPower; // Chiller power, W
		Real64 QEvap; // Evaporator heat transfer rate [W]
		Real64 QCond; // Condenser heat transfer rate [W]
		Real64 CoolingEnergy; // Chiller electric consumption [J]
		Real64 HeatingEnergy; // Chiller electric consumption [J]
		Real64 EvapEnergy; // Evaporator heat transfer energy [J]
		Real64 CondEnergy; // Condenser heat transfer energy [J]
		Real64 CondInletTemp; // Condenser inlet temperature [C]
		Real64 EvapInletTemp; // Evaporator inlet temperature [C]
		Real64 CondOutletTemp; // Condenser outlet temperature [C]
		Real64 EvapOutletTemp; // Evaporator outlet temperature [C]
		Real64 Evapmdot; // Evaporator mass flow rate [kg/s]
		Real64 Condmdot; // Condenser mass flow rate [kg/s]
		Real64 ActualCOP; // Coefficient of performance
		Real64 ChillerCapFT; // Chiller capacity curve output value
		Real64 ChillerEIRFT; // Chiller EIRFT curve output value
		Real64 ChillerEIRFPLR; // Chiller EIRFPLR curve output value
		Real64 CondenserFanPowerUse; // Air-cooled condenser fan power [W]
		Real64 CondenserFanEnergy; // Air-cooled condenser fan energy [J]
		Real64 CondenserFanEnergyConsumption; // ""Should be checked"" For now, leave it
		Real64 ChillerPartLoadRatioSimul; // Chiller PLR (Load/Capacity) for simul clg/htg mode
		Real64 ChillerCyclingRatioSimul; // Chiller cycling ratio (time on/time step) for simul clg/htg mode
		Real64 ChillerFalseLoadSimul; // Chiller false load for simul clg/htg mode [J]
		Real64 ChillerFalseLoadRateSimul; // Chiller false load rate for simul clg/htg mode [W]
		Real64 CoolingPowerSimul; // Chiller power for simul clg/htg mode [W]
		Real64 QEvapSimul; // Evaporator heat transfer rate for simul clg/htg mode [W]
		Real64 QCondSimul; // Evaporator heat transfer rate for simul clg/htg mode [W]
		Real64 CoolingEnergySimul; // Chiller electric consumption for simul clg/htg mode [J]
		Real64 EvapEnergySimul; // Evaporator heat transfer energy for simul clg/htg mode [J]
		Real64 CondEnergySimul; // Condenser heat transfer energy for simul clg/htg mode [J]
		Real64 EvapInletTempSimul; // Evaporator inlet temperature for simul clg/htg mode [C]
		Real64 EvapOutletTempSimul; // Evaporator outlet temperature for simul clg/htg mode [C]
		Real64 EvapmdotSimul; // Evaporator mass flow rate for simul clg/htg mode [kg/s]
		Real64 CondInletTempSimul; // Condenser inlet temperature for simul clg/htg mode [C]
		Real64 CondOutletTempSimul; // Condenser outlet temperature for simul clg/htg mode [C]
		Real64 CondmdotSimul; // Condenser mass flow rate for simul clg/htg mode [kg/s]
		Real64 ChillerCapFTSimul; // Chiller capacity curve output value for simul clg/htg mode
		Real64 ChillerEIRFTSimul; // Chiller EIRFT curve output value for simul clg/htg mode
		Real64 ChillerEIRFPLRSimul; // Chiller EIRFPLR curve output value for simul clg/htg mode

		// Default Constructor
		CHReportVars() :
			CurrentMode( 0 ),
			ChillerPartLoadRatio( 0.0 ),
			ChillerCyclingRatio( 0.0 ),
			ChillerFalseLoad( 0.0 ),
			ChillerFalseLoadRate( 0.0 ),
			CoolingPower( 0.0 ),
			HeatingPower( 0.0 ),
			QEvap( 0.0 ),
			QCond( 0.0 ),
			CoolingEnergy( 0.0 ),
			HeatingEnergy( 0.0 ),
			EvapEnergy( 0.0 ),
			CondEnergy( 0.0 ),
			CondInletTemp( 0.0 ),
			EvapInletTemp( 0.0 ),
			CondOutletTemp( 0.0 ),
			EvapOutletTemp( 0.0 ),
			Evapmdot( 0.0 ),
			Condmdot( 0.0 ),
			ActualCOP( 0.0 ),
			ChillerCapFT( 0.0 ),
			ChillerEIRFT( 0.0 ),
			ChillerEIRFPLR( 0.0 ),
			CondenserFanPowerUse( 0.0 ),
			CondenserFanEnergy( 0.0 ),
			CondenserFanEnergyConsumption( 0.0 ),
			ChillerPartLoadRatioSimul( 0.0 ),
			ChillerCyclingRatioSimul( 0.0 ),
			ChillerFalseLoadSimul( 0.0 ),
			ChillerFalseLoadRateSimul( 0.0 ),
			CoolingPowerSimul( 0.0 ),
			QEvapSimul( 0.0 ),
			QCondSimul( 0.0 ),
			CoolingEnergySimul( 0.0 ),
			EvapEnergySimul( 0.0 ),
			CondEnergySimul( 0.0 ),
			EvapInletTempSimul( 0.0 ),
			EvapOutletTempSimul( 0.0 ),
			EvapmdotSimul( 0.0 ),
			CondInletTempSimul( 0.0 ),
			CondOutletTempSimul( 0.0 ),
			CondmdotSimul( 0.0 ),
			ChillerCapFTSimul( 0.0 ),
			ChillerEIRFTSimul( 0.0 ),
			ChillerEIRFPLRSimul( 0.0 )
		{}
	};

	struct WrapperSpecs // This will be used for Wrapper Object. This object will decide the mode of Chiller
	{
		// Members
		std::string Name; // User identifier
		std::string AncilliaryPwSchedule; // Ancilliary Power Schedule Name
		bool VariableFlowCH; // True if all chiller heters are variable flow control
		int SchedPtr; // Schedule value for ancilliar power control
		int CHSchedPtr; // Schedule value for individual chiller heater control
		int ControlMode; // SmartMixing or FullyMixing
		int CHWInletNodeNum; // Node number on the inlet side of the plant (Chilled Water side)
		int CHWOutletNodeNum; // Node number on the outlet side of the plant (Chilled Water side)
		int HWInletNodeNum; // Node number on the inlet side of the plant (Hot Water side)
		int HWOutletNodeNum; // Node number on the outlet side of the plant (Hot Water side)
		int GLHEInletNodeNum; // Node number on the inlet side of the plant (GLHE Water side)
		int GLHEOutletNodeNum; // Node number on the outlet side of the plant (GLHE Water side)
		int NumOfComp; // Number of Components under the wrapper
		Real64 CHWMassFlowRate; // Chilled water mass flow rate
		Real64 HWMassFlowRate; // Hot water mass flow rate
		Real64 GLHEMassFlowRate; // Condenser water mass flow rate
		Real64 CHWMassFlowRateMax; // Maximum chilled water mass flow rate
		Real64 HWMassFlowRateMax; // Maximum hot water mass flow rate
		Real64 GLHEMassFlowRateMax; // Maximum condenser water mass flow rate
		Real64 WrapperCoolingLoad; // Cooling demand for the central heat pump system
		Real64 WrapperHeatingLoad; // Heating demand for the central heat pump system
		Real64 AncilliaryPower; // Wrapper Ancilliary Power
		Array1D< WrapperComponentSpecs > WrapperComp;
		Array1D< ChillerHeaterSpecs > ChillerHeater; // Dimension to number of machines
		Array1D< CHReportVars > ChillerHeaterReport; // Dimension to number of machines
		bool CoolSetPointErrDone; // true if setpoint warning issued
		bool HeatSetPointErrDone; // true if setpoint warning issued
		bool CoolSetPointSetToLoop; // True if the setpoint is missing at the outlet node
		bool HeatSetPointSetToLoop; // True if the setpoint is missing at the outlet node
		int ChillerHeaterNums; // Total number of chiller heater units
		int CWLoopNum; // Chilled water plant loop index number
		int CWLoopSideNum; // Chilled water plant loop side index
		int CWBranchNum; // Chilled water plant loop branch index
		int CWCompNum; // Chilled water plant loop component index
		int HWLoopNum; // Hot water plant loop index number
		int HWLoopSideNum; // Hot water plant loop side index
		int HWBranchNum; // Hot water plant loop branch index
		int HWCompNum; // Hot water plant loop component index
		int GLHELoopNum; // Geo-field water plant loop index number
		int GLHELoopSideNum; // Geo-field water plant loop side index
		int GLHEBranchNum; // Geo-field water plant loop branch index
		int GLHECompNum; // Geo-field water plant loop component index
		int CHWMassFlowIndex; // Chilled water flow index
		int HWMassFlowIndex; // Hot water flow index
		int GLHEMassFlowIndex; // Condenser side flow index
		Real64 SizingFactor; // Sizing factor to adjust the capacity
		Real64 CHWVolFlowRate; // Chilled water volume flow rate [kg/s]
		Real64 HWVolFlowRate; // Hot water volume flow rate [kg/s]
		Real64 GLHEVolFlowRate; // Geo-field volume flow rate [kg/s]

		// Default Constructor
		WrapperSpecs() :
			VariableFlowCH( false ),
			SchedPtr( 0 ),
			CHSchedPtr( 0 ),
			ControlMode( 0 ),
			CHWInletNodeNum( 0 ),
			CHWOutletNodeNum( 0 ),
			HWInletNodeNum( 0 ),
			HWOutletNodeNum( 0 ),
			GLHEInletNodeNum( 0 ),
			GLHEOutletNodeNum( 0 ),
			NumOfComp( 0 ),
			CHWMassFlowRate( 0.0 ),
			HWMassFlowRate( 0.0 ),
			GLHEMassFlowRate( 0.0 ),
			CHWMassFlowRateMax( 0.0 ),
			HWMassFlowRateMax( 0.0 ),
			GLHEMassFlowRateMax( 0.0 ),
			WrapperCoolingLoad( 0.0 ),
			WrapperHeatingLoad( 0.0 ),
			AncilliaryPower( 0.0 ),
			CoolSetPointErrDone( false ),
			HeatSetPointErrDone( false ),
			CoolSetPointSetToLoop( false ),
			HeatSetPointSetToLoop( false ),
			CWLoopNum( 0 ),
			CWLoopSideNum( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			HWLoopNum( 0 ),
			HWLoopSideNum( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			GLHELoopNum( 0 ),
			GLHELoopSideNum( 0 ),
			GLHEBranchNum( 0 ),
			GLHECompNum( 0 ),
			CHWMassFlowIndex( 0 ),
			HWMassFlowIndex( 0 ),
			GLHEMassFlowIndex( 0 ),
			SizingFactor( 1.0 ),
			CHWVolFlowRate( 0.0 ),
			HWVolFlowRate( 0.0 ),
			GLHEVolFlowRate( 0.0 )
		{}
	};

	struct WrapperReportVars
	{
		// Members
		Real64 Power; // Wrapper power, W
		Real64 QCHW; // Chilled water heat transfer rate [W]
		Real64 QHW; // Hot Water heat transfer rate [W]
		Real64 QGLHE; // Geo-field heat transfer rate [W]
		Real64 TotElecCooling; // Wrapper cooling electric consumption [J]
		Real64 TotElecHeating; // Wrapper heating electric consumption [J]
		Real64 CoolingEnergy; // Chilled water heat transfer energy [J]
		Real64 HeatingEnergy; // Hot Water heat transfer energy [J]
		Real64 GLHEEnergy; // Geo-field heat transfer energy [J]
		Real64 TotElecCoolingPwr; // Wrapper cooling electric consumption rate [W]
		Real64 TotElecHeatingPwr; // Wrapper heating electric consumption rate [W]
		Real64 CoolingRate; // Chilled water heat transfer rate [W]
		Real64 HeatingRate; // Hot Water heat transfer rate [W]
		Real64 GLHERate; // Geo-field heat transfer rate [W]
		Real64 CHWInletTemp; // Chilled water inlet temperature [C]
		Real64 HWInletTemp; // Hot water inlet temperature [C]
		Real64 GLHEInletTemp; // Geo-field inlet temperature [C]
		Real64 CHWOutletTemp; // Chilled water Outlet temperature [C]
		Real64 HWOutletTemp; // Hot water Outlet temperature [C]
		Real64 GLHEOutletTemp; // Geo-field Outlet temperature [C]
		Real64 CHWmdot; // Chilled water mass flow rate [kg/s]
		Real64 HWmdot; // Hot water mass flow rate [kg/s]
		Real64 GLHEmdot; // Geo-field mass flow rate [kg/s]
		Real64 TotElecCoolingSimul; // Wrapper cooling electric consumption [J]
		Real64 CoolingEnergySimul; // Chilled water heat transfer energy [J]
		Real64 TotElecCoolingPwrSimul; // Wrapper cooling electric consumption rate [W]
		Real64 CoolingRateSimul; // Chilled water heat transfer rate [W]
		Real64 CHWInletTempSimul; // Chilled water inlet temperature [C]
		Real64 GLHEInletTempSimul; // Geo-field inlet temperature [C]
		Real64 CHWOutletTempSimul; // Chilled water Outlet temperature [C]
		Real64 GLHEOutletTempSimul; // Geo-field Outlet temperature [C]
		Real64 CHWmdotSimul; // Chilled water mass flow rate [kg/s]
		Real64 GLHEmdotSimul; // Geo-field mass flow rate [kg/s]

		// Default Constructor
		WrapperReportVars() :
			Power( 0.0 ),
			QCHW( 0.0 ),
			QHW( 0.0 ),
			QGLHE( 0.0 ),
			TotElecCooling( 0.0 ),
			TotElecHeating( 0.0 ),
			CoolingEnergy( 0.0 ),
			HeatingEnergy( 0.0 ),
			GLHEEnergy( 0.0 ),
			TotElecCoolingPwr( 0.0 ),
			TotElecHeatingPwr( 0.0 ),
			CoolingRate( 0.0 ),
			HeatingRate( 0.0 ),
			GLHERate( 0.0 ),
			CHWInletTemp( 0.0 ),
			HWInletTemp( 0.0 ),
			GLHEInletTemp( 0.0 ),
			CHWOutletTemp( 0.0 ),
			HWOutletTemp( 0.0 ),
			GLHEOutletTemp( 0.0 ),
			CHWmdot( 0.0 ),
			HWmdot( 0.0 ),
			GLHEmdot( 0.0 ),
			TotElecCoolingSimul( 0.0 ),
			CoolingEnergySimul( 0.0 ),
			TotElecCoolingPwrSimul( 0.0 ),
			CoolingRateSimul( 0.0 ),
			CHWInletTempSimul( 0.0 ),
			GLHEInletTempSimul( 0.0 ),
			CHWOutletTempSimul( 0.0 ),
			GLHEOutletTempSimul( 0.0 ),
			CHWmdotSimul( 0.0 ),
			GLHEmdotSimul( 0.0 )
		{}
	};

	// Object Data
	extern Array1D< WrapperSpecs > Wrapper;
	extern Array1D< ChillerHeaterSpecs > ChillerHeater;
	extern Array1D< CHReportVars > ChillerHeaterReport;
	extern Array1D< WrapperReportVars > WrapperReport;

	// Functions

	void
	SimCentralGroundSourceHeatPump(
		std::string const & WrapperName, // User specified name of wrapper
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
		Real64 & SizingFactor // sizing factor
	);

	void
	SizeWrapper( int const WrapperNum );

	void
	GetWrapperInput();

	void
	GetChillerHeaterInput();

	void
	InitWrapper(
		int const WrapperNum, // Number of the current wrapper being simulated
		bool const RunFlag, // TRUE when chiller operating
		bool const FirstIteration, // Initialize variables when TRUE
		Real64 const MyLoad, // Demand Load
		int const LoopNum // Loop Number Index
	);

	void
	CalcChillerModel(
		int const WrapperNum, // Number of wrapper
		int const OpMode, // Operation mode
		Real64 & MyLoad, // Operating load
		bool const RunFlag, // TRUE when chiller operating
		bool const FirstIteration, // TRUE when first iteration of timestep
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int const LoopNum // Plant loop number
	);

	void
	CalcChillerHeaterModel(
		int const WrapperNum, // Wrapper number pointor
		int const OpMode, // Operation mode
		Real64 & MyLoad, // Heating load plant should meet
		bool const RunFlag, // TRUE when chiller operating
		bool const FirstIteration, // TRUE when first iteration of timestep
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int const LoopNum // Loop number
	);

	void
	CalcWrapperModel(
		int const WrapperNum,
		Real64 & MyLoad,
		bool const RunFlag,
		bool const FirstIteration,
		int const EquipFlowCtrl,
		int const LoopNum
	);

	void
	UpdateChillerRecords( int const WrapperNum ); // Wrapper number

	void
	UpdateChillerHeaterRecords( int const WrapperNum ); // Wrapper number

} // PlantCentralGSHP

} // EnergyPlus

#endif
