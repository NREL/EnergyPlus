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

#ifndef DesiccantDehumidifiers_hh_INCLUDED
#define DesiccantDehumidifiers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DesiccantDehumidifiers {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	// Desiccant dehumidifier type
	extern int const Solid; // DESICCANT DEHUMIDIFIER:SOLID = 1
	extern int const Generic; // DESICCANT DEHUMIDIFIER = 2
	//  Desiccant heat exchanger type
	extern int const BalancedHX; // HeatExchanger:Desiccant:BalancedFlow = 1
	// Desiccant control type
	extern int const FixedHumratBypass; // FIXED LEAVING HUMRAT SETPOINT:BYPASS = 1
	extern int const NodeHumratBypass; // NODE LEAVING HUMRAT SETPOINT:BYPASS  = 2
	// Preheat selection
	extern int const No; // Condenser waste heat NOT reclaimed for desiccant regeneration
	extern int const Yes; // Condenser waste heat reclaimed for desiccant regeneration
	// Performance Model
	extern int const PM_Default; // Performance Model = default
	extern int const PM_UserCurves; // Performance Model = user curve

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int NumDesicDehums; // number of desiccant dehumidifiers of all types
	extern int NumSolidDesicDehums; // number of solid desiccant dehumidifiers
	extern int NumGenericDesicDehums; // number of generic desiccant dehumidifiers
	extern Real64 TempSteamIn; // steam coil steam inlet temperature

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>

	// Name Public routines, optionally name Private routines within this module

	// Types

	struct DesiccantDehumidifierData
	{
		// Members
		// User Input data
		std::string Name; // unique name of component
		std::string Sched; // name of availability schedule
		std::string RegenCoilType; // type of regen coil
		std::string RegenCoilName; // name of regen coil
		std::string RegenFanType; // type of regen fan
		std::string RegenFanName; // name of regen fan
		int PerformanceModel_Num; // type of performance model, default or user curves
		int ProcAirInNode; // process air inlet node of dehumidifier
		int ProcAirOutNode; // process air outlet node of dehumidifier
		int RegenAirInNode; // regen air inlet node of dehumidifier
		// (initially set to conditions entering regen heating coil)
		int RegenAirOutNode; // regen air outlet node of dehumidifier
		int RegenFanInNode; // regen fan inlet node
		int ControlType; // type of controls
		Real64 HumRatSet; // humidity ratio setpoint [kg water / kg air]
		Real64 NomProcAirVolFlow; // nominal process air flow rate [m3/s]
		Real64 NomProcAirVel; // nominal process air velocity [m/s]
		Real64 NomRotorPower; // rotor power consumption at full output [W]
		int RegenCoilIndex; // Index for regen coil
		int RegenFanIndex; // Index for regen fan
		int ProcDryBulbCurvefTW; // number of process leaving dry bulb f(edb,ew) curve
		int ProcDryBulbCurvefV; // number of process leaving dry bulb f(v) curve
		int ProcHumRatCurvefTW; // number of process leaving humidity ratio f(edb,ew) curve
		int ProcHumRatCurvefV; // number of process leaving humidity ratio f(v) curve
		int RegenEnergyCurvefTW; // number of regen energy f(edb,ew) curve
		int RegenEnergyCurvefV; // number of regen energy f(v) curve
		int RegenVelCurvefTW; // number of regen velocity f(edb,ew) curve
		int RegenVelCurvefV; // number of regen velocity f(v) curve
		Real64 NomRegenTemp; // nominal regen temperature for regen energy curve [C]
		// Possible future inputs, hardwired for now depending on which performance model is in use, unit off if out of bounds
		Real64 MinProcAirInTemp; // min allowable process inlet air temperature [C]
		Real64 MaxProcAirInTemp; // max allowable process inlet air temperature [C]
		Real64 MinProcAirInHumRat; // min allowable process inlet air humidity ratio [kg water / kg air]
		Real64 MaxProcAirInHumRat; // max allowable process inlet air humidity ratio [kg water / kg air]
		// Internal Data
		int SchedPtr; // index of availability schedule
		Real64 NomProcAirMassFlow; // nominal process air mass flow rate [kg/s]
		Real64 NomRegenAirMassFlow; // nominal regeneration air mass flow rate [kg/s]
		Real64 ProcAirInTemp; // process inlet air temperature [C]
		Real64 ProcAirInHumRat; // process inlet air humidity ratio [kg water / kg air]
		Real64 ProcAirInEnthalpy; // process inlet air specific enthalpy [J/kg]
		Real64 ProcAirInMassFlowRate; // process inlet air mass flow rate [kg/s]
		Real64 ProcAirOutTemp; // process outlet air temperature [C]
		Real64 ProcAirOutHumRat; // process outlet air humidity ratio [kg water / kg air]
		Real64 ProcAirOutEnthalpy; // process outlet air specific enthalpy [J/kg]
		Real64 ProcAirOutMassFlowRate; // process outlet air mass flow rate [kg/s]
		Real64 RegenAirInTemp; // regen inlet air temperature [C]
		Real64 RegenAirInHumRat; // regen inlet air humidity ratio [kg water / kg air]
		Real64 RegenAirInEnthalpy; // regen inlet air specific enthalpy [J/kg]
		Real64 RegenAirInMassFlowRate; // regen inlet air mass flow rate [kg/s]
		Real64 RegenAirVel; // regen air velocity [m/s]
		std::string DehumType; // Type of desiccant dehumidifier
		int DehumTypeCode; // Type of desiccant dehumidifier, integer code
		Real64 WaterRemove; // water removed [kg]
		Real64 WaterRemoveRate; // water removal rate [kg/s]
		Real64 SpecRegenEnergy; // specific regen energy [J/kg of water removed]
		Real64 QRegen; // regen energy rate requested from regen coil [W]
		Real64 RegenEnergy; // regen energy requested from regen coil [J]
		Real64 ElecUseEnergy; // electricity consumption [J]
		Real64 ElecUseRate; // electricity consumption rate [W]
		Real64 PartLoad; // fraction of dehumidification capacity required to meet setpoint
		int RegenCapErrorIndex1; // recurring error message index for insufficient regen coil capacity
		int RegenCapErrorIndex2; // recurring error message index for insufficient regen coil capacity
		int RegenCapErrorIndex3; // recurring error message index for insufficient regen coil capacity
		int RegenCapErrorIndex4; // recurring error message index for insufficient regen coil capacity
		int RegenFanErrorIndex1; // recurring error message index for incorrect regen fan flow
		int RegenFanErrorIndex2; // recurring error message index for incorrect regen fan flow
		int RegenFanErrorIndex3; // recurring error message index for incorrect regen fan flow
		int RegenFanErrorIndex4; // recurring error message index for incorrect regen fan flow
		// structure elements unique to generic desiccant dehumidifier
		std::string HXType; // type of desiccant heat exchanger
		std::string HXName; // name of desiccant heat exchanger
		int HXTypeNum; // parameter number of desiccant heat exchanger
		std::string ExhaustFanCurveObject; // exhaust fan curve object
		std::string CoolingCoilType; // type of cooling coil used with desiccant heat exchanger
		std::string CoolingCoilName; // name of cooling coil used with desiccant heat exchanger
		int Preheat; // determine condenser waste heat usage for pre heating regen air
		Real64 RegenSetPointTemp; // heating set-point for regeneration air [C]
		Real64 ExhaustFanMaxVolFlowRate; // exhaust fan maximum allowable air flow rate [m3/s]
		Real64 ExhaustFanMaxMassFlowRate; // exhaust fan maximum allowable air mass flow rate [kg/s]
		Real64 ExhaustFanMaxPower; // exhaust fan maximum allowable power [W]
		Real64 ExhaustFanPower; // exhaust fan power for reporting [W]
		Real64 ExhaustFanElecConsumption; // exhaust fan electric consumption for reporting [J]
		Real64 CompanionCoilCapacity; // DX coil capacity for dehumidifier companion cooling coil [W]
		int RegenFanPlacement; // placement of the fan used for regeneration air flow
		int ControlNodeNum; // node number of control node
		int ExhaustFanCurveIndex; // exhaust fan curve object index
		int CompIndex; // index of HX component to call simheatrecovery
		int CoolingCoilOutletNode; // node number of cooling coil outlet node
		int RegenFanOutNode; // fan outlet node number mined from regen fan object
		int RegenCoilInletNode; // regen heating coil inlet node number mined from regen heater object
		int RegenCoilOutletNode; // regen heating coil outlet node number mined from regen heater object
		int HXProcInNode; // process inlet node num mined from desiccant heat exchanger object
		int HXProcOutNode; // process outlet node num mined from desiccant heat exchanger object
		int HXRegenInNode; // regen inlet node number mined from desiccant heat exchanger object
		int HXRegenOutNode; // regen outlet node number mined from desiccant heat exchanger object
		int CondenserInletNode; // regen outlet node number mined from desiccant heat exchanger object
		int DXCoilIndex; // DX Coil index mined from coil object
		int ErrCount; // error count
		int ErrIndex1; // error index
		int CoilUpstreamOfProcessSide; // used to determine if process inlet is pre-cooled
		bool RegenInletIsOutsideAirNode; // regen inlet is connected to an outside air node
		int RegenCoilType_Num; // type number of regen coil
		int CoilControlNode; // heating coil hot water or steam inlet node
		int CoilOutletNode; // outlet node for water coil
		int LoopNum; // plant loop index for water heating coil
		int LoopSide; // plant loop side  index for water heating coil
		int BranchNum; // plant loop branch index for water heating coil
		int CompNum; // plant loop component index for water heating coil
		int HotWaterCoilMaxIterIndex; // Index to recurring warning message
		int HotWaterCoilMaxIterIndex2; // Index to recurring warning message
		Real64 MaxCoilFluidFlow; // hot water or steam mass flow rate regen. heating coil [kg/s]
		Real64 RegenCoilCapacity; // hot water or steam coil operating capacity [W]

		// Default Constructor
		DesiccantDehumidifierData() :
			PerformanceModel_Num( 0 ),
			ProcAirInNode( 0 ),
			ProcAirOutNode( 0 ),
			RegenAirInNode( 0 ),
			RegenAirOutNode( 0 ),
			RegenFanInNode( 0 ),
			ControlType( 0 ),
			HumRatSet( 0.0 ),
			NomProcAirVolFlow( 0.0 ),
			NomProcAirVel( 0.0 ),
			NomRotorPower( 0.0 ),
			RegenCoilIndex( 0 ),
			RegenFanIndex( 0 ),
			ProcDryBulbCurvefTW( 0 ),
			ProcDryBulbCurvefV( 0 ),
			ProcHumRatCurvefTW( 0 ),
			ProcHumRatCurvefV( 0 ),
			RegenEnergyCurvefTW( 0 ),
			RegenEnergyCurvefV( 0 ),
			RegenVelCurvefTW( 0 ),
			RegenVelCurvefV( 0 ),
			NomRegenTemp( 121.0 ),
			MinProcAirInTemp( -73.3 ),
			MaxProcAirInTemp( 65.6 ),
			MinProcAirInHumRat( 0.0 ),
			MaxProcAirInHumRat( 0.21273 ),
			SchedPtr( 0 ),
			NomProcAirMassFlow( 0.0 ),
			NomRegenAirMassFlow( 0.0 ),
			ProcAirInTemp( 0.0 ),
			ProcAirInHumRat( 0.0 ),
			ProcAirInEnthalpy( 0.0 ),
			ProcAirInMassFlowRate( 0.0 ),
			ProcAirOutTemp( 0.0 ),
			ProcAirOutHumRat( 0.0 ),
			ProcAirOutEnthalpy( 0.0 ),
			ProcAirOutMassFlowRate( 0.0 ),
			RegenAirInTemp( 0.0 ),
			RegenAirInHumRat( 0.0 ),
			RegenAirInEnthalpy( 0.0 ),
			RegenAirInMassFlowRate( 0.0 ),
			RegenAirVel( 0.0 ),
			DehumTypeCode( 0 ),
			WaterRemove( 0.0 ),
			WaterRemoveRate( 0.0 ),
			SpecRegenEnergy( 0.0 ),
			QRegen( 0.0 ),
			RegenEnergy( 0.0 ),
			ElecUseEnergy( 0.0 ),
			ElecUseRate( 0.0 ),
			PartLoad( 0.0 ),
			RegenCapErrorIndex1( 0 ),
			RegenCapErrorIndex2( 0 ),
			RegenCapErrorIndex3( 0 ),
			RegenCapErrorIndex4( 0 ),
			RegenFanErrorIndex1( 0 ),
			RegenFanErrorIndex2( 0 ),
			RegenFanErrorIndex3( 0 ),
			RegenFanErrorIndex4( 0 ),
			HXTypeNum( 0 ),
			Preheat( 0 ),
			RegenSetPointTemp( 0.0 ),
			ExhaustFanMaxVolFlowRate( 0.0 ),
			ExhaustFanMaxMassFlowRate( 0.0 ),
			ExhaustFanMaxPower( 0.0 ),
			ExhaustFanPower( 0.0 ),
			ExhaustFanElecConsumption( 0.0 ),
			CompanionCoilCapacity( 0.0 ),
			RegenFanPlacement( 0 ),
			ControlNodeNum( 0 ),
			ExhaustFanCurveIndex( 0 ),
			CompIndex( 0 ),
			CoolingCoilOutletNode( 0 ),
			RegenFanOutNode( 0 ),
			RegenCoilInletNode( 0 ),
			RegenCoilOutletNode( 0 ),
			HXProcInNode( 0 ),
			HXProcOutNode( 0 ),
			HXRegenInNode( 0 ),
			HXRegenOutNode( 0 ),
			CondenserInletNode( 0 ),
			DXCoilIndex( 0 ),
			ErrCount( 0 ),
			ErrIndex1( 0 ),
			CoilUpstreamOfProcessSide( 0 ),
			RegenInletIsOutsideAirNode( false ),
			RegenCoilType_Num( 0 ),
			CoilControlNode( 0 ),
			CoilOutletNode( 0 ),
			LoopNum( 0 ),
			LoopSide( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			HotWaterCoilMaxIterIndex( 0 ),
			HotWaterCoilMaxIterIndex2( 0 ),
			MaxCoilFluidFlow( 0.0 ),
			RegenCoilCapacity( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< DesiccantDehumidifierData > DesicDehum;

	// Functions

	void
	SimDesiccantDehumidifier(
		std::string const & CompName, // name of the dehumidifier unit
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int & CompIndex
	);

	void
	GetDesiccantDehumidifierInput();

	void
	InitDesiccantDehumidifier(
		int const DesicDehumNum, // number of the current dehumidifier being simulated
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	);

	void
	ControlDesiccantDehumidifier(
		int const DesicDehumNum, // number of the current dehumidifier being simulated
		Real64 & HumRatNeeded, // process air leaving humidity ratio set by controller [kg water/kg air]
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep !unused1208
	);

	void
	CalcSolidDesiccantDehumidifier(
		int const DesicDehumNum, // number of the current dehumidifier being simulated
		Real64 const HumRatNeeded, // process air leaving humidity ratio set by controller [kgWater/kgDryAir]
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	);

	void
	CalcGenericDesiccantDehumidifier(
		int const DesicDehumNum, // number of the current dehumidifier being simulated
		Real64 const HumRatNeeded, // process air leaving humidity ratio set by controller [kg water/kg air]
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	);

	void
	UpdateDesiccantDehumidifier( int const DesicDehumNum ); // number of the current dehumidifier being simulated

	void
	ReportDesiccantDehumidifier( int const DesicDehumNum ); // number of the current dehumidifier being simulated

	void
	CalcNonDXHeatingCoils(
		int const DesicDehumNum, // Desiccant dehumidifier unit index
		bool const FirstHVACIteration, // flag for first HVAC iteration in the time step
		Real64 const RegenCoilLoad, // heating coil load to be met (Watts)
		Optional< Real64 > RegenCoilLoadmet = _ // heating load met
	);

	Real64
	HotWaterCoilResidual(
		Real64 const HWFlow, // hot water flow rate in kg/s
		Array1< Real64 > const & Par // Par(5) is the requested coil load
	);

	// Clears the global data in HeatingCoils.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	//        End of Reporting subroutines for the SimAir Module
	// *****************************************************************************

	//                                 COPYRIGHT NOTICE

	//     Portions Copyright (c) Gas Research Institute 2001.  All rights reserved.

	//     GRI LEGAL NOTICE
	//     Neither GRI, members of GRI nor any person or organization acting on behalf
	//     of either:

	//     A. Makes any warranty of representation, express or implied with respect to
	//        the accuracy, completness, or usefulness of the information contained in
	//        in this program, including any warranty of merchantability or fitness of
	//        any purpose with respoect to the program, or that the use of any
	//        information disclosed in this program may not infringe privately-owned
	//        rights, or

	//     B.  Assumes any liability with respoct to the use of, or for any and all
	//         damages resulting from the use of the program or any portion thereof or
	//         any information disclosed therein.

} // DesiccantDehumidifiers

} // EnergyPlus

#endif
