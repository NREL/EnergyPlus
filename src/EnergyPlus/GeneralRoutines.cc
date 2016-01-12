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

// C++ Headers
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/member.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <GeneralRoutines.hh>
#include <BaseboardRadiator.hh>
#include <ConvectionCoefficients.hh>
#include <DataAirLoop.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataSurfaces.hh>
#include <DataZoneEquipment.hh>
#include <FanCoilUnits.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HVACSingleDuctInduc.hh>
#include <HWBaseboardRadiator.hh>
#include <InputProcessor.hh>
#include <MixerComponent.hh>
#include <OutdoorAirUnit.hh>
#include <PlantUtilities.hh>
#include <PoweredInductionUnits.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <SolarCollectors.hh>
#include <SplitterComponent.hh>
#include <SteamBaseboardRadiator.hh>
#include <UnitHeater.hh>
#include <UnitVentilator.hh>
#include <UtilityRoutines.hh>
#include <VentilatedSlab.hh>
#include <WaterCoils.hh>
#include <ZonePlenum.hh>

namespace EnergyPlus {

static gio::Fmt fmtLD( "*" );

void
ControlCompOutput(
	std::string const & CompName, // the component Name
	std::string const & CompType, // Type of component
	int & CompNum, // Index of component in component array
	bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
	Real64 const QZnReq, // zone load to be met
	int const ActuatedNode, // node that controls unit output
	Real64 const MaxFlow, // maximum water flow
	Real64 const MinFlow, // minimum water flow
	Real64 const ControlOffset, // really the tolerance
	int & ControlCompTypeNum, // Internal type num for CompType
	int & CompErrIndex, // for Recurring error call
	Optional_int_const TempInNode, // inlet node for output calculation
	Optional_int_const TempOutNode, // outlet node for output calculation
	Optional< Real64 const > AirMassFlow, // air mass flow rate
	Optional_int_const Action, // 1=reverse; 2=normal
	Optional_int_const EquipIndex, // Identifier for equipment of Outdoor Air Unit "ONLY"
	Optional_int_const LoopNum, // for plant components, plant loop index
	Optional_int_const LoopSide, // for plant components, plant loop side index
	Optional_int_const BranchIndex, // for plant components, plant branch index
	Optional_int_const ControlledZoneIndex // controlled zone index for the zone containing the component
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Richard J. Liesen
	//       DATE WRITTEN   April 2000
	//       MODIFIED       Brent Griffith, Sept 2010 update plant interactions
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	//The purpose of this subroutine is to control the output of heating or cooling
	//meet the zone load.

	// METHODOLOGY EMPLOYED:
	// Currently this is using an intervasl halving scheme to a control tolerance

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataGlobals::WarmupFlag;
	using DataBranchAirLoopPlant::MassFlowTolerance;
	using InputProcessor::FindItemInSortedList;
	using WaterCoils::SimulateWaterCoilComponents;
	using FanCoilUnits::Calc4PipeFanCoil;
	using UnitVentilator::CalcUnitVentilatorComponents;
	using UnitHeater::CalcUnitHeaterComponents;
	using HWBaseboardRadiator::CalcHWBaseboard;
	using BaseboardRadiator::SimHWConvective;
	using Psychrometrics::PsyCpAirFnWTdb;
	using VentilatedSlab::CalcVentilatedSlabComps;
	using General::TrimSigDigits;
	using General::RoundSigDigits;
	using SteamBaseboardRadiator::CalcSteamBaseboard;
	using OutdoorAirUnit::CalcOAUnitCoilComps;
	using PlantUtilities::SetActuatedBranchFlowRate;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	//Iteration maximum for reheat control
	static int const MaxIter( 25 );
	static Real64 const iter_fac( 1.0 / std::pow( 2, MaxIter - 3 ) );
	int const iReverseAction( 1 );
	int const iNormalAction( 2 );

	// Note - order in routine must match order below
	//  Plus -- order in ListOfComponents array must be in sorted order.
	int const NumComponents( 11 );
	static Array1D_string const ListOfComponents( NumComponents, { "AIRTERMINAL:SINGLEDUCT:PARALLELPIU:REHEAT", "AIRTERMINAL:SINGLEDUCT:SERIESPIU:REHEAT", "COIL:HEATING:WATER", "ZONEHVAC:BASEBOARD:CONVECTIVE:WATER", "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM", "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER", "ZONEHVAC:FOURPIPEFANCOIL", "ZONEHVAC:OUTDOORAIRUNIT", "ZONEHVAC:UNITHEATER", "ZONEHVAC:UNITVENTILATOR", "ZONEHVAC:VENTILATEDSLAB" } );

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	//Interval Half Type used for Controller

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	static int Iter( 0 ); // Iteration limit for the interval halving process
	Real64 CpAir; // specific heat of air (J/kg-C)
	bool Converged;
	Real64 Denom; // the denominator of the control signal
	Real64 LoadMet; // Actual output of unit (watts)
	//INTEGER, SAVE    :: ErrCount=0  ! Number of times that the maximum iterations was exceeded
	//INTEGER, SAVE    :: ErrCount1=0 ! for recurring error
	bool WaterCoilAirFlowControl; // True if controlling air flow through water coil, water flow fixed
	int SimCompNum; // internal number for case statement
	static Real64 HalvingPrec( 0.0 ); // precision of halving algorithm

	struct IntervalHalf
	{
		// Members
		Real64 MaxFlow;
		Real64 MinFlow;
		Real64 MaxResult;
		Real64 MinResult;
		Real64 MidFlow;
		Real64 MidResult;
		bool MaxFlowCalc;
		bool MinFlowCalc;
		bool MinFlowResult;
		bool NormFlowCalc;

		// Default Constructor
		IntervalHalf()
		{}

		// Member Constructor
		IntervalHalf(
			Real64 const MaxFlow,
			Real64 const MinFlow,
			Real64 const MaxResult,
			Real64 const MinResult,
			Real64 const MidFlow,
			Real64 const MidResult,
			bool const MaxFlowCalc,
			bool const MinFlowCalc,
			bool const MinFlowResult,
			bool const NormFlowCalc
		) :
			MaxFlow( MaxFlow ),
			MinFlow( MinFlow ),
			MaxResult( MaxResult ),
			MinResult( MinResult ),
			MidFlow( MidFlow ),
			MidResult( MidResult ),
			MaxFlowCalc( MaxFlowCalc ),
			MinFlowCalc( MinFlowCalc ),
			MinFlowResult( MinFlowResult ),
			NormFlowCalc( NormFlowCalc )
		{}

	};

	struct ZoneEquipControllerProps
	{
		// Members
		Real64 SetPoint; // Desired setpoint;
		Real64 MaxSetPoint; // The maximum setpoint; either user input or reset per time step by simulation
		Real64 MinSetPoint; // The minimum setpoint; either user input or reset per time step by simulation
		Real64 SensedValue; // The sensed control variable of any type
		Real64 CalculatedSetPoint; // The Calculated SetPoint or new control actuated value

		// Default Constructor
		ZoneEquipControllerProps()
		{}

		// Member Constructor
		ZoneEquipControllerProps(
			Real64 const SetPoint, // Desired setpoint;
			Real64 const MaxSetPoint, // The maximum setpoint; either user input or reset per time step by simulation
			Real64 const MinSetPoint, // The minimum setpoint; either user input or reset per time step by simulation
			Real64 const SensedValue, // The sensed control variable of any type
			Real64 const CalculatedSetPoint // The Calculated SetPoint or new control actuated value
		) :
			SetPoint( SetPoint ),
			MaxSetPoint( MaxSetPoint ),
			MinSetPoint( MinSetPoint ),
			SensedValue( SensedValue ),
			CalculatedSetPoint( CalculatedSetPoint )
		{}

	};

	// Object Data
	static IntervalHalf ZoneInterHalf( 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, false, false, false, false );
	static ZoneEquipControllerProps ZoneController( 0.0, 0.0, 0.0, 0.0, 0.0 );

	if ( ControlCompTypeNum != 0 ) {
		SimCompNum = ControlCompTypeNum;
	} else {
		SimCompNum = FindItemInSortedList( CompType, ListOfComponents, NumComponents );
		ControlCompTypeNum = SimCompNum;
	}

	Iter = 0;
	Converged = false;
	WaterCoilAirFlowControl = false;
	LoadMet = 0.0;
	HalvingPrec = 0.0;

	// At the beginning of every time step the value is reset to the User Input
	ZoneController.SetPoint = 0.0;

	// Set to converged controller
	ZoneInterHalf.MaxFlowCalc = true;
	ZoneInterHalf.MinFlowCalc = false;
	ZoneInterHalf.NormFlowCalc = false;
	ZoneInterHalf.MinFlowResult = false;
	ZoneInterHalf.MaxResult = 1.0;
	ZoneInterHalf.MinResult = 0.0;

	// Start the Solution Iteration
	while ( ! Converged ) {

		if ( FirstHVACIteration ) {
			Node( ActuatedNode ).MassFlowRateMaxAvail = MaxFlow;
			Node( ActuatedNode ).MassFlowRateMinAvail = MinFlow;
			// Check to make sure that the Minimum Flow rate is less than the max.
			if ( MinFlow > MaxFlow ) {
				ShowSevereError( "ControlCompOutput:" + CompType + ':' + CompName + ", Min Control Flow is > Max Control Flow" );
				ShowContinueError( "Acuated Node=" + NodeID( ActuatedNode ) + " MinFlow=[" + TrimSigDigits( MinFlow, 3 ) + "], Max Flow=" + TrimSigDigits( MaxFlow, 3 ) );
				ShowContinueErrorTimeStamp( "" );
				ShowFatalError( "Program terminates due to preceding condition." );
			}
		} // End of FirstHVACIteration Conditional If
		// The interface managers can reset the Max or Min to available values during the time step
		// and these will then be the new setpoint limits for the controller to work within.
		if ( ( SimCompNum == 3 ) && ( ! present( AirMassFlow ) ) ) {
			ZoneController.MaxSetPoint = Node( ActuatedNode ).MassFlowRateMaxAvail;
			ZoneController.MinSetPoint = Node( ActuatedNode ).MassFlowRateMinAvail;
		} else {
			ZoneController.MaxSetPoint = min( Node( ActuatedNode ).MassFlowRateMaxAvail, Node( ActuatedNode ).MassFlowRateMax );
			ZoneController.MinSetPoint = max( Node( ActuatedNode ).MassFlowRateMinAvail, Node( ActuatedNode ).MassFlowRateMin );
		}
		// The first time through run at maximum flow rate and find results
		if ( ZoneInterHalf.MaxFlowCalc ) {
			ZoneController.CalculatedSetPoint = ZoneController.MaxSetPoint;
			ZoneInterHalf.MaxFlow = ZoneController.MaxSetPoint;
			ZoneInterHalf.MaxFlowCalc = false;
			ZoneInterHalf.MinFlowCalc = true;
			// Record the maximum flow rates and set the flow to the minimum and find results
		} else if ( ZoneInterHalf.MinFlowCalc ) {
			ZoneInterHalf.MaxResult = ZoneController.SensedValue;
			ZoneController.CalculatedSetPoint = ZoneController.MinSetPoint;
			ZoneInterHalf.MinFlow = ZoneController.MinSetPoint;
			ZoneInterHalf.MinFlowCalc = false;
			ZoneInterHalf.MinFlowResult = true;
			//Record the minimum results and set flow to half way between the max and min and find results
		} else if ( ZoneInterHalf.MinFlowResult ) {
			ZoneInterHalf.MinResult = ZoneController.SensedValue;
			HalvingPrec = ( ZoneInterHalf.MaxResult - ZoneInterHalf.MinResult ) * iter_fac;
			ZoneInterHalf.MidFlow = ( ZoneInterHalf.MaxFlow + ZoneInterHalf.MinFlow ) / 2.0;
			ZoneController.CalculatedSetPoint = ( ZoneInterHalf.MaxFlow + ZoneInterHalf.MinFlow ) / 2.0;
			ZoneInterHalf.MinFlowResult = false;
			ZoneInterHalf.NormFlowCalc = true;
			// Record the Mid results and check all possibilities and start interval halving procedure
		} else if ( ZoneInterHalf.NormFlowCalc ) {
			ZoneInterHalf.MidResult = ZoneController.SensedValue;

			// First check to see if the component is running; if not converge and return
			if ( ZoneInterHalf.MaxResult == ZoneInterHalf.MinResult ) {
				// Set to converged controller
				Converged = true;
				ZoneInterHalf.MaxFlowCalc = true;
				ZoneInterHalf.MinFlowCalc = false;
				ZoneInterHalf.NormFlowCalc = false;
				ZoneInterHalf.MinFlowResult = false;
				ZoneInterHalf.MaxResult = 1.0;
				ZoneInterHalf.MinResult = 0.0;
				if ( ( SimCompNum >= 4 ) && ( SimCompNum <= 6 ) ) { //hot water baseboards use min flow
					ZoneController.CalculatedSetPoint = 0.0; //CR7253
				} else {
					ZoneController.CalculatedSetPoint = ZoneInterHalf.MaxFlow; //CR7253
				}
				// Set the Actuated node MassFlowRate with zero value
				if ( present( LoopNum ) ) { // this is a plant component
					SetActuatedBranchFlowRate( ZoneController.CalculatedSetPoint, ActuatedNode, LoopNum, LoopSide, BranchIndex, false ); //Autodesk:OPTIONAL LoopSide, BranchIndex used without PRESENT check
				} else { // assume not a plant component
					Node( ActuatedNode ).MassFlowRate = ZoneController.CalculatedSetPoint;
				}
				return;
			}

			// The next series of checks is to determine what interval the current solution is in
			//   comparison to the setpoint and then respond appropriately.

			// Normal controller assumes that MaxResult will be greater than MinResult. First check
			// to make sure that this is the case
			if ( ZoneInterHalf.MaxResult <= ZoneInterHalf.MinResult ) {
				if ( WaterCoilAirFlowControl ) {
					ZoneController.CalculatedSetPoint = ZoneInterHalf.MaxFlow;
				} else {
					ZoneController.CalculatedSetPoint = ZoneInterHalf.MinFlow;
				}
				// set to converged controller
				Converged = true;
				ZoneInterHalf.MaxFlowCalc = true;
				ZoneInterHalf.MinFlowCalc = false;
				ZoneInterHalf.NormFlowCalc = false;
				ZoneInterHalf.MinFlowResult = false;
				ZoneInterHalf.MaxResult = 1.0;
				ZoneInterHalf.MinResult = 0.0;
				// MaxResult is greater than MinResult so simulation control algorithm may proceed normally
			} else if ( ZoneInterHalf.MaxResult > ZoneInterHalf.MinResult ) {
				// Now check to see if the setpoint is outside the endpoints of the control range
				// First check to see if the water is too cold and if so set to the minimum flow.
				if ( ZoneController.SetPoint <= ZoneInterHalf.MinResult ) {
					ZoneController.CalculatedSetPoint = ZoneInterHalf.MinFlow;
					// Set to Converged Controller
					Converged = true;
					ZoneInterHalf.MaxFlowCalc = true;
					ZoneInterHalf.MinFlowCalc = false;
					ZoneInterHalf.NormFlowCalc = false;
					ZoneInterHalf.MinFlowResult = false;
					ZoneInterHalf.MaxResult = 1.0;
					ZoneInterHalf.MinResult = 0.0;
					// Then check if too hot and if so set it to the maximum flow
				} else if ( ZoneController.SetPoint >= ZoneInterHalf.MaxResult ) {
					ZoneController.CalculatedSetPoint = ZoneInterHalf.MaxFlow;
					// Set to Converged Controller
					Converged = true;
					ZoneInterHalf.MaxFlowCalc = true;
					ZoneInterHalf.MinFlowCalc = false;
					ZoneInterHalf.NormFlowCalc = false;
					ZoneInterHalf.MinFlowResult = false;
					ZoneInterHalf.MaxResult = 1.0;
					ZoneInterHalf.MinResult = 0.0;
					// If between the max and mid set to new flow and raise min to mid
				} else if ( ( ZoneController.SetPoint < ZoneInterHalf.MaxResult ) && ( ZoneController.SetPoint >= ZoneInterHalf.MidResult ) ) {
					ZoneController.CalculatedSetPoint = ( ZoneInterHalf.MaxFlow + ZoneInterHalf.MidFlow ) / 2.0;
					ZoneInterHalf.MinFlow = ZoneInterHalf.MidFlow;
					ZoneInterHalf.MinResult = ZoneInterHalf.MidResult;
					ZoneInterHalf.MidFlow = ( ZoneInterHalf.MaxFlow + ZoneInterHalf.MidFlow ) / 2.0;
					// If between the min and mid set to new flow and lower Max to mid
				} else if ( ( ZoneController.SetPoint < ZoneInterHalf.MidResult ) && ( ZoneController.SetPoint > ZoneInterHalf.MinResult ) ) {
					ZoneController.CalculatedSetPoint = ( ZoneInterHalf.MinFlow + ZoneInterHalf.MidFlow ) / 2.0;
					ZoneInterHalf.MaxFlow = ZoneInterHalf.MidFlow;
					ZoneInterHalf.MaxResult = ZoneInterHalf.MidResult;
					ZoneInterHalf.MidFlow = ( ZoneInterHalf.MinFlow + ZoneInterHalf.MidFlow ) / 2.0;

				} // End of the Conditional for the actual interval halving scheme itself
			} // end of max > min check

		} // End of the Conditinal for the first 3 iterations for the interval halving

		// Make sure that the Calculated setpoint falls between the minimum and maximum allowed
		if ( ZoneController.CalculatedSetPoint > ZoneController.MaxSetPoint ) {
			ZoneController.CalculatedSetPoint = ZoneController.MaxSetPoint;
			Converged = true;
			ZoneInterHalf.MaxFlowCalc = true;
			ZoneInterHalf.MinFlowCalc = false;
			ZoneInterHalf.NormFlowCalc = false;
			ZoneInterHalf.MinFlowResult = false;
			ZoneInterHalf.MaxResult = 1.0;
			ZoneInterHalf.MinResult = 0.0;
		} else if ( ZoneController.CalculatedSetPoint < ZoneController.MinSetPoint ) {
			ZoneController.CalculatedSetPoint = ZoneController.MinSetPoint;
			Converged = true;
			ZoneInterHalf.MaxFlowCalc = true;
			ZoneInterHalf.MinFlowCalc = false;
			ZoneInterHalf.NormFlowCalc = false;
			ZoneInterHalf.MinFlowResult = false;
			ZoneInterHalf.MaxResult = 1.0;
			ZoneInterHalf.MinResult = 0.0;
		}

		// check if hunting down around the limit of a significant mass flow in systems.
		if ( ( Iter > MaxIter / 2 ) && ( ZoneController.CalculatedSetPoint < MassFlowTolerance ) ) {
			ZoneController.CalculatedSetPoint = ZoneController.MinSetPoint;
			Converged = true;
			ZoneInterHalf.MaxFlowCalc = true;
			ZoneInterHalf.MinFlowCalc = false;
			ZoneInterHalf.NormFlowCalc = false;
			ZoneInterHalf.MinFlowResult = false;
			ZoneInterHalf.MaxResult = 1.0;
			ZoneInterHalf.MinResult = 0.0;
		}

		// Set the Actuated node MassFlowRate with the new value
		if ( present( LoopNum ) ) { // this is a plant component
			SetActuatedBranchFlowRate( ZoneController.CalculatedSetPoint, ActuatedNode, LoopNum, LoopSide, BranchIndex, false ); //Autodesk:OPTIONAL LoopSide, BranchIndex used without PRESENT check
		} else { // assume not a plant component, leave alone
			Node( ActuatedNode ).MassFlowRate = ZoneController.CalculatedSetPoint;
		}

		// The denominator of the control signal should be no less than 100 watts
		Denom = sign( max( std::abs( QZnReq ), 100.0 ), QZnReq );
		if ( present( Action ) ) {
			if ( Action == iNormalAction ) {
				Denom = max( std::abs( QZnReq ), 100.0 );
			} else if ( Action == iReverseAction ) {
				Denom = -max( std::abs( QZnReq ), 100.0 );
			} else {
				ShowFatalError( "ControlCompOutput: Illegal Action argument =[" + TrimSigDigits( Action ) + ']' );
			}
		}

		switch ( SimCompNum ) { //Tuned If block changed to switch
		case 1: // 'AIRTERMINAL:SINGLEDUCT:PARALLELPIU:REHEAT'
			// simulate series piu reheat coil
			SimulateWaterCoilComponents( CompName, FirstHVACIteration, CompNum );
			// Calculate the control signal (the variable we are forcing to zero)
			CpAir = PsyCpAirFnWTdb( Node( TempOutNode ).HumRat, 0.5 * ( Node( TempOutNode ).Temp + Node( TempInNode ).Temp ) ); //Autodesk:OPTIONAL TempInNode, TempOutNode used without PRESENT check
			LoadMet = CpAir * Node( TempOutNode ).MassFlowRate * ( Node( TempOutNode ).Temp - Node( TempInNode ).Temp ); //Autodesk:OPTIONAL TempInNode, TempOutNode used without PRESENT check
			ZoneController.SensedValue = ( LoadMet - QZnReq ) / Denom;
			break;

		case 2: // 'AIRTERMINAL:SINGLEDUCT:SERIESPIU:REHEAT'
			// simulate series piu reheat coil
			SimulateWaterCoilComponents( CompName, FirstHVACIteration, CompNum );
			// Calculate the control signal (the variable we are forcing to zero)
			CpAir = PsyCpAirFnWTdb( Node( TempOutNode ).HumRat, 0.5 * ( Node( TempOutNode ).Temp + Node( TempInNode ).Temp ) ); //Autodesk:OPTIONAL TempInNode, TempOutNode used without PRESENT check
			LoadMet = CpAir * Node( TempOutNode ).MassFlowRate * ( Node( TempOutNode ).Temp - Node( TempInNode ).Temp ); //Autodesk:OPTIONAL TempInNode, TempOutNode used without PRESENT check
			ZoneController.SensedValue = ( LoadMet - QZnReq ) / Denom;
			break;

		case 3: // 'COIL:HEATING:WATER'
			// Simulate reheat coil for the VAV system
			SimulateWaterCoilComponents( CompName, FirstHVACIteration, CompNum );
			// Calculate the control signal (the variable we are forcing to zero)
			CpAir = PsyCpAirFnWTdb( Node( TempOutNode ).HumRat, Node( TempOutNode ).Temp );
			if ( present( AirMassFlow ) ) {
				LoadMet = AirMassFlow * CpAir * Node( TempOutNode ).Temp;
				ZoneController.SensedValue = ( LoadMet - QZnReq ) / Denom;
			} else {
				WaterCoilAirFlowControl = true;
				LoadMet = Node( TempOutNode ).MassFlowRate * CpAir * ( Node( TempOutNode ).Temp - Node( TempInNode ).Temp ); //Autodesk:OPTIONAL TempInNode, TempOutNode used without PRESENT check
				ZoneController.SensedValue = ( LoadMet - QZnReq ) / Denom;
			}
			break;

		case 4: // 'ZONEHVAC:BASEBOARD:CONVECTIVE:WATER'
			// Simulate baseboard
			SimHWConvective( CompNum, LoadMet );
			// Calculate the control signal (the variable we are forcing to zero)
			ZoneController.SensedValue = ( LoadMet - QZnReq ) / Denom;
			break;

		case 5: // 'ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM'
			// Simulate baseboard
			CalcSteamBaseboard( CompNum, LoadMet );
			// Calculate the control signal (the variable we are forcing to zero)
			ZoneController.SensedValue = ( LoadMet - QZnReq ) / Denom;
			break;

		case 6: // 'ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER'
			// Simulate baseboard
			CalcHWBaseboard( CompNum, LoadMet );
			// Calculate the control signal (the variable we are forcing to zero)
			ZoneController.SensedValue = ( LoadMet - QZnReq ) / Denom;
			break;

		case 7: // 'ZONEHVAC:FOURPIPEFANCOIL'
			// Simulate fancoil unit
			Calc4PipeFanCoil( CompNum, ControlledZoneIndex, FirstHVACIteration, LoadMet );
			//Calculate the control signal (the variable we are forcing to zero)
			ZoneController.SensedValue = ( LoadMet - QZnReq ) / Denom;
			break;

		case 8: //'ZONEHVAC:OUTDOORAIRUNIT'
			// Simulate outdoor air unit components
			CalcOAUnitCoilComps( CompNum, FirstHVACIteration, EquipIndex, LoadMet ); //Autodesk:OPTIONAL EquipIndex used without PRESENT check
			//Calculate the control signal (the variable we are forcing to zero)
			ZoneController.SensedValue = ( LoadMet - QZnReq ) / Denom;
			break;

		case 9: // 'ZONEHVAC:UNITHEATER'
			// Simulate unit heater components
			CalcUnitHeaterComponents( CompNum, FirstHVACIteration, LoadMet );
			//Calculate the control signal (the variable we are forcing to zero)
			ZoneController.SensedValue = ( LoadMet - QZnReq ) / Denom;
			break;

		case 10: // 'ZONEHVAC:UNITVENTILATOR'
			// Simulate unit ventilator components
			CalcUnitVentilatorComponents( CompNum, FirstHVACIteration, LoadMet );
			//Calculate the control signal (the variable we are forcing to zero)
			ZoneController.SensedValue = ( LoadMet - QZnReq ) / Denom;
			break;

		case 11: // 'ZONEHVAC:VENTILATEDSLAB'
			// Simulate unit ventilator components
			CalcVentilatedSlabComps( CompNum, FirstHVACIteration, LoadMet );
			//Calculate the control signal (the variable we are forcing to zero)
			ZoneController.SensedValue = ( LoadMet - QZnReq ) / Denom;
			break;

		default:
			ShowFatalError( "ControlCompOutput: Illegal Component Number argument =[" + TrimSigDigits( SimCompNum ) + ']' );
			break;

		}

		// Check for Controller convergence to see if within the offset
		if ( std::abs( ZoneController.SensedValue ) <= ControlOffset || std::abs( ZoneController.SensedValue ) <= HalvingPrec ) {
			//Set to converged controller
			Converged = true;
			ZoneInterHalf.MaxFlowCalc = true;
			ZoneInterHalf.MinFlowCalc = false;
			ZoneInterHalf.NormFlowCalc = false;
			ZoneInterHalf.MinFlowResult = false;
			ZoneInterHalf.MaxResult = 1.0;
			ZoneInterHalf.MinResult = 0.0;
			break;
		}

		++Iter;
		if ( ( Iter > MaxIter ) && ( ! WarmupFlag ) ) {
			//if ( CompErrIndex == 0 ) {
			ShowWarningMessage( "ControlCompOutput: Maximum iterations exceeded for " + CompType + " = " + CompName );
			ShowContinueError( "... Load met       = " + TrimSigDigits( LoadMet, 5 ) + " W." );
			ShowContinueError( "... Load requested = " + TrimSigDigits( QZnReq, 5 ) + " W." );
			ShowContinueError( "... Error          = " + TrimSigDigits( std::abs( ( LoadMet - QZnReq ) * 100.0 / Denom ), 8 ) + " %." );
			ShowContinueError( "... Tolerance      = " + TrimSigDigits( ControlOffset * 100.0, 8 ) + " %." );
			ShowContinueError( "... Error          = (Load met - Load requested) / MAXIMUM(Load requested, 100)" );
			ShowContinueError( "... Actuated Node Mass Flow Rate =" + RoundSigDigits( Node( ActuatedNode ).MassFlowRate, 9 ) + " kg/s" );
			ShowContinueErrorTimeStamp( "" );
			ShowRecurringWarningErrorAtEnd( "ControlCompOutput: Maximum iterations error for " + CompType + " = " + CompName, CompErrIndex, std::abs( ( LoadMet - QZnReq ) * 100.0 / Denom ), std::abs( ( LoadMet - QZnReq ) * 100.0 / Denom ), _, "%", "%" );
			//}
			ShowRecurringWarningErrorAtEnd( "ControlCompOutput: Maximum iterations error for " + CompType + " = " + CompName, CompErrIndex, std::abs( ( LoadMet - QZnReq ) * 100.0 / Denom ), std::abs( ( LoadMet - QZnReq ) * 100.0 / Denom ), _, "%", "%" );
			break; // It will not converge this time
		} else if ( Iter > MaxIter * 2 ) {
			break;
		}

	} // End of the Convergence Iteration

}

void
CheckSysSizing(
	std::string const & CompType, // Component Type (e.g. Chiller:Electric)
	std::string const & CompName // Component Name (e.g. Big Chiller)
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   October 2002
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This routine is called when an "autosize" input is encountered in a component
	// sizing routine to check that the system sizing calculations have been done.

	// METHODOLOGY EMPLOYED:
	// Checks SysSizingRunDone flag. If false throws a fatal error.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::DoSystemSizing;
	using DataSizing::SysSizingRunDone;
	using DataSizing::NumSysSizInput;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	if ( ! SysSizingRunDone ) {
		ShowSevereError( "For autosizing of " + CompType + ' ' + CompName + ", a system sizing run must be done." );
		if ( NumSysSizInput == 0 ) {
			ShowContinueError( "No \"Sizing:System\" objects were entered." );
		}
		if ( ! DoSystemSizing ) {
			ShowContinueError( "The \"SimulationControl\" object did not have the field \"Do System Sizing Calculation\" set to Yes." );
		}
		ShowFatalError( "Program terminates due to previously shown condition(s)." );
	}

}

void
CheckThisAirSystemForSizing(
	int const AirLoopNum,
	bool & AirLoopWasSized
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         B. Griffith
	//       DATE WRITTEN   October 2013
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// <description>

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// Using/Aliasing
	using DataSizing::SysSizingRunDone;
	using DataSizing::NumSysSizInput;
	using DataSizing::SysSizInput;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:
	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	// na
	int ThisAirSysSizineInputLoop;

	AirLoopWasSized = false;
	if ( SysSizingRunDone ) {
		for ( ThisAirSysSizineInputLoop = 1; ThisAirSysSizineInputLoop <= NumSysSizInput; ++ThisAirSysSizineInputLoop ) {
			if ( SysSizInput( ThisAirSysSizineInputLoop ).AirLoopNum == AirLoopNum ) {
				AirLoopWasSized = true;
				break;
			}
		}
	}

}

void
CheckZoneSizing(
	std::string const & CompType, // Component Type (e.g. Chiller:Electric)
	std::string const & CompName // Component Name (e.g. Big Chiller)
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   October 2002
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This routine is called when an "autosize" input is encountered in a component
	// sizing routine to check that the zone sizing calculations have been done.

	// METHODOLOGY EMPLOYED:
	// Checks ZoneSizingRunDone flag. If false throws a fatal error.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::DoZoneSizing;
	using DataSizing::ZoneSizingRunDone;
	using DataSizing::NumZoneSizingInput;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	if ( ! ZoneSizingRunDone ) {
		ShowSevereError( "For autosizing of " + CompType + ' ' + CompName + ", a zone sizing run must be done." );
		if ( NumZoneSizingInput == 0 ) {
			ShowContinueError( "No \"Sizing:Zone\" objects were entered." );
		}
		if ( ! DoZoneSizing ) {
			ShowContinueError( "The \"SimulationControl\" object did not have the field \"Do Zone Sizing Calculation\" set to Yes." );
		}
		ShowFatalError( "Program terminates due to previously shown condition(s)." );
	}

}

void
CheckThisZoneForSizing(
	int const ZoneNum, // zone index to be checked
	bool & ZoneWasSized
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         B. Griffith
	//       DATE WRITTEN   Oct 2013
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// utility routine to see if a particular zone has a Sizing:Zone object for it
	// and that sizing was done.

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// Using/Aliasing
	using DataSizing::ZoneSizingRunDone;
	using DataSizing::NumZoneSizingInput;
	using DataSizing::ZoneSizingInput;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int ThisSizingInput;

	ZoneWasSized = false;
	if ( ZoneSizingRunDone ) {
		for ( ThisSizingInput = 1; ThisSizingInput <= NumZoneSizingInput; ++ThisSizingInput ) {
			if ( ZoneSizingInput( ThisSizingInput ).ZoneNum == ZoneNum ) {
				ZoneWasSized = true;
				break;
			}
		}
	}

}

void
ValidateComponent(
	std::string const & CompType, // Component Type (e.g. Chiller:Electric)
	std::string const & CompName, // Component Name (e.g. Big Chiller)
	bool & IsNotOK, // .TRUE. if this component pair is invalid
	std::string const & CallString // Context of this pair -- for error message
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   October 2002
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine can be called to validate the component type-name pairs that
	// are so much a part of the EnergyPlus input.  The main drawback to this validation
	// has been that the "GetInput" routine may not have been called and/or exists in
	// another module from the one with the list.  This means that validation must be
	// done later, perhaps after simulation has already started or perhaps raises an
	// array bound error instead.

	// METHODOLOGY EMPLOYED:
	// Uses existing routines in InputProcessor.  GetObjectItemNum uses the "standard"
	// convention of the Name of the item/object being the first Alpha Argument.

	// REFERENCES:
	// na

	// Using/Aliasing
	using InputProcessor::GetObjectItemNum;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int ItemNum;

	IsNotOK = false;

	ItemNum = GetObjectItemNum( CompType, CompName );

	if ( ItemNum < 0 ) {
		ShowSevereError( "During " + CallString + " Input, Invalid Component Type input=" + CompType );
		ShowContinueError( "Component name=" + CompName );
		IsNotOK = true;
	} else if ( ItemNum == 0 ) {
		ShowSevereError( "During " + CallString + " Input, Invalid Component Name input=" + CompName );
		ShowContinueError( "Component type=" + CompType );
		IsNotOK = true;
	}

}

void
CalcPassiveExteriorBaffleGap(
	Array1S_int const SurfPtrARR, // Array of indexes pointing to Surface structure in DataSurfaces
	Real64 const VentArea, // Area available for venting the gap [m2]
	Real64 const Cv, // Oriface coefficient for volume-based discharge, wind-driven [--]
	Real64 const Cd, // oriface coefficient for discharge,  bouyancy-driven [--]
	Real64 const HdeltaNPL, // Height difference from neutral pressure level [m]
	Real64 const SolAbs, // solar absorptivity of baffle [--]
	Real64 const AbsExt, // thermal absorptance/emittance of baffle material [--]
	Real64 const Tilt, // Tilt of gap [Degrees]
	Real64 const AspRat, // aspect ratio of gap  Height/gap [--]
	Real64 const GapThick, // Thickness of air space between baffle and underlying heat transfer surface
	int const Roughness, // Roughness index (1-6), see DataHeatBalance parameters
	Real64 const QdotSource, // Source/sink term, e.g. electricity exported from solar cell [W]
	Real64 & TsBaffle, // Temperature of baffle (both sides) use lagged value on input [C]
	Real64 & TaGap, // Temperature of air gap (assumed mixed) use lagged value on input [C]
	Optional< Real64 > HcGapRpt,
	Optional< Real64 > HrGapRpt,
	Optional< Real64 > IscRpt,
	Optional< Real64 > MdotVentRpt,
	Optional< Real64 > VdotWindRpt,
	Optional< Real64 > VdotBouyRpt
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         B.T. Griffith
	//       DATE WRITTEN   November 2004
	//       MODIFIED       BG March 2007 outdoor conditions from surface for height-dependent conditions
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// model the effect of the a ventilated baffle covering the outside of a heat transfer surface.
	// return calculated temperatures and certain intermediate values for reporting

	// METHODOLOGY EMPLOYED:
	// Heat balances on baffle and air space.
	// Natural ventilation calculations use bouyancy and wind.

	// REFERENCES:
	// Nat. Vent. equations from ASHRAE HoF 2001 Chapt. 26

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataEnvironment::SkyTemp;
	using DataEnvironment::WindSpeedAt;
	using DataEnvironment::OutBaroPress;
	using DataEnvironment::IsRain;
	// USE DataLoopNode    , ONLY: Node
	using Psychrometrics::PsyRhoAirFnPbTdbW;
	using Psychrometrics::PsyCpAirFnWTdb;
	using Psychrometrics::PsyWFnTdbTwbPb;
	using DataSurfaces::Surface;
	using DataSurfaces::SurfaceData;
	using DataHeatBalSurface::TH;
	using DataHeatBalance::Material;
	using DataHeatBalance::Construct;
	using DataHeatBalance::QRadSWOutIncident;
	using ConvectionCoefficients::InitExteriorConvectionCoeff;
	using SolarCollectors::Collector;
	using DataGlobals::BeginEnvrnFlag;

	// Argument array dimensioning

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	Real64 const g( 9.807 ); // gravitational constant (m/s**2)
	Real64 const nu( 15.66e-6 ); // kinematic viscosity (m**2/s) for air at 300 K (Mills 1999 Heat Transfer)
	Real64 const k( 0.0267 ); // thermal conductivity (W/m K) for air at 300 K (Mills 1999 Heat Transfer)
	Real64 const Sigma( 5.6697e-08 ); // Stefan-Boltzmann constant
	Real64 const KelvinConv( 273.15 ); // Conversion from Celsius to Kelvin
	static std::string const RoutineName( "CalcPassiveExteriorBaffleGap" );
	// INTERFACE BLOCK SPECIFICATIONS:

	// DERIVED TYPE DEFINITIONS:

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	// following arrays are used to temporarily hold results from multiple underlying surfaces
	Array1D< Real64 > HSkyARR;
	Array1D< Real64 > HGroundARR;
	Array1D< Real64 > HAirARR;
	Array1D< Real64 > HPlenARR;
	Array1D< Real64 > HExtARR;
	Array1D< Real64 > LocalWindArr;

	// local working variables
	Real64 RhoAir; // density of air
	Real64 CpAir; // specific heat of air
	Real64 Tamb; // outdoor drybulb
	Real64 A; // projected area of baffle from sum of underlying surfaces
	Real64 HcPlen; // surface convection heat transfer coefficient for plenum surfaces
	int ThisSurf; // do loop counter
	int NumSurfs; // number of underlying HT surfaces associated with UTSC
	Real64 TmpTsBaf; // baffle temperature
	int SurfPtr; // index of surface in main surface structure
	Real64 HMovInsul; // dummy for call to InitExteriorConvectionCoeff
	Real64 HExt; // dummy for call to InitExteriorConvectionCoeff
	int ConstrNum; // index of construction in main construction structure
	Real64 AbsThermSurf; // thermal emmittance of underlying wall.
	Real64 TsoK; // underlying surface temperature in Kelvin
	Real64 TsBaffK; // baffle temperature in Kelvin  (lagged)
	Real64 Vwind; // localized, and area-weighted average for wind speed
	Real64 HrSky; // radiation coeff for sky, area-weighted average
	Real64 HrGround; // radiation coeff for ground, area-weighted average
	Real64 HrAtm; // radiation coeff for air (bulk atmosphere), area-weighted average
	Real64 Isc; // Incoming combined solar radiation, area-weighted average
	Real64 HrPlen; // radiation coeff for plenum surfaces, area-weighted average
	Real64 Tso; // temperature of underlying surface, area-weighted average
	Real64 TmeanK; // average of surface temps , for Beta in Grashoff no.
	Real64 Gr; // Grasshof number for natural convection calc
	Real64 VdotWind; // volume flow rate of nat. vent due to wind
	Real64 VdotThermal; // Volume flow rate of nat. vent due to bouyancy
	Real64 VdotVent; // total volume flow rate of nat vent
	Real64 MdotVent; // total mass flow rate of nat vent
	Real64 NuPlen; // Nusselt No. for plenum Gap
	Real64 LocalOutDryBulbTemp; // OutDryBulbTemp for here
	Real64 LocalWetBulbTemp; // OutWetBulbTemp for here
	Real64 LocalOutHumRat; // OutHumRat for here
	static bool ICSCollectorIsOn( false ); // ICS collector has OSCM on
	int CollectorNum; // current solar collector index
	Real64 ICSWaterTemp; // ICS solar collector water temp
	Real64 ICSULossbottom; // ICS solar collector bottom loss Conductance
	static bool MyICSEnvrnFlag( true ); // Local environment flag for ICS

	Real64 const surfaceArea( sum_sub( Surface, &SurfaceData::Area, SurfPtrARR ) );

//	LocalOutDryBulbTemp = sum( Surface( SurfPtrARR ).Area * Surface( SurfPtrARR ).OutDryBulbTemp ) / sum( Surface( SurfPtrARR ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
	LocalOutDryBulbTemp = sum_product_sub( Surface, &SurfaceData::Area, &SurfaceData::OutDryBulbTemp, SurfPtrARR ) / surfaceArea; //Autodesk:F2C++ Functions handle array subscript usage

//	LocalWetBulbTemp = sum( Surface( SurfPtrARR ).Area * Surface( SurfPtrARR ).OutWetBulbTemp ) / sum( Surface( SurfPtrARR ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
	LocalWetBulbTemp = sum_product_sub( Surface, &SurfaceData::Area, &SurfaceData::OutWetBulbTemp, SurfPtrARR ) / surfaceArea; //Autodesk:F2C++ Functions handle array subscript usage

	LocalOutHumRat = PsyWFnTdbTwbPb( LocalOutDryBulbTemp, LocalWetBulbTemp, OutBaroPress, RoutineName );

	RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, LocalOutDryBulbTemp, LocalOutHumRat, RoutineName );
	CpAir = PsyCpAirFnWTdb( LocalOutHumRat, LocalOutDryBulbTemp );
	if ( ! IsRain ) {
		Tamb = LocalOutDryBulbTemp;
	} else { // when raining we use wetbulb not drybulb
		Tamb = LocalWetBulbTemp;
	}
//	A = sum( Surface( SurfPtrARR ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
	A = surfaceArea;
	TmpTsBaf = TsBaffle;

	//loop through underlying surfaces and collect needed data
	NumSurfs = size( SurfPtrARR );
	HSkyARR.dimension( NumSurfs, 0.0 );
	HGroundARR.dimension( NumSurfs, 0.0 );
	HAirARR.dimension( NumSurfs, 0.0 );
	LocalWindArr.dimension( NumSurfs, 0.0 );
	HPlenARR.dimension( NumSurfs, 0.0 );
	HExtARR.dimension( NumSurfs, 0.0 );

	for ( ThisSurf = 1; ThisSurf <= NumSurfs; ++ThisSurf ) {
		SurfPtr = SurfPtrARR( ThisSurf );
		// Initializations for this surface
		HMovInsul = 0.0;
		LocalWindArr( ThisSurf ) = Surface( SurfPtr ).WindSpeed;
		InitExteriorConvectionCoeff( SurfPtr, HMovInsul, Roughness, AbsExt, TmpTsBaf, HExtARR( ThisSurf ), HSkyARR( ThisSurf ), HGroundARR( ThisSurf ), HAirARR( ThisSurf ) );
		ConstrNum = Surface( SurfPtr ).Construction;
		AbsThermSurf = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).AbsorpThermal;
		TsoK = TH( 1, 1, SurfPtr ) + KelvinConv;
		TsBaffK = TmpTsBaf + KelvinConv;
		if ( TsBaffK == TsoK ) { // avoid divide by zero
			HPlenARR( ThisSurf ) = 0.0; // no net heat transfer if same temperature
		} else {
			HPlenARR( ThisSurf ) = Sigma * AbsExt * AbsThermSurf * ( pow_4( TsBaffK ) - pow_4( TsoK ) ) / ( TsBaffK - TsoK );
		}
		// Added for ICS collector OSCM
		if ( Surface( SurfPtr ).IsICS ) {
			ICSCollectorIsOn = true;
			CollectorNum = Surface( SurfPtr ).ICSPtr;
		}
	}

	if ( ICSCollectorIsOn ) {
		if ( BeginEnvrnFlag && MyICSEnvrnFlag ) {
			ICSULossbottom = 0.40;
			ICSWaterTemp = 20.0;
		} else {
			if ( ! Collector.allocated() ) {
				ICSULossbottom = 0.40;
				ICSWaterTemp = 20.0;
			} else {
				ICSULossbottom = Collector( CollectorNum ).UbLoss;
				ICSWaterTemp = Collector( CollectorNum ).TempOfWater;
				MyICSEnvrnFlag = false;
			}
		}
	}
	if ( ! BeginEnvrnFlag ) {
		MyICSEnvrnFlag = true;
	}
	if ( A == 0.0 ) { // should have been caught earlier

	}
	auto Area( array_sub( Surface, &SurfaceData::Area, SurfPtrARR ) ); //Autodesk:F2C++ Copy of subscripted Area array for use below: This makes a copy so review wrt performance
	// now figure area-weighted averages from underlying surfaces.
//	Vwind = sum( LocalWindArr * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
	Vwind = sum( LocalWindArr * Area ) / A;
	LocalWindArr.deallocate();
//	HrSky = sum( HSkyARR * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
	HrSky = sum( HSkyARR * Area ) / A;
	HSkyARR.deallocate();
//	HrGround = sum( HGroundARR * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
	HrGround = sum( HGroundARR * Area ) / A;
	HGroundARR.deallocate();
//	HrAtm = sum( HAirARR * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
	HrAtm = sum( HAirARR * Area ) / A;
	HAirARR.deallocate();
//	HrPlen = sum( HPlenARR * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
	HrPlen = sum( HPlenARR * Area ) / A;
	HPlenARR.deallocate();
//	HExt = sum( HExtARR * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
	HExt = sum( HExtARR * Area ) / A;
	HExtARR.deallocate();

	if ( IsRain ) HExt = 1000.0;

//	Tso = sum( TH( 1, 1, SurfPtrARR ) * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
	Tso = sum_product_sub( TH( 1, 1, _ ), Surface, &SurfaceData::Area, SurfPtrARR ) / A; //Autodesk:F2C++ Functions handle array subscript usage
//	Isc = sum( QRadSWOutIncident( SurfPtrARR ) * Surface( SurfPtrARR ).Area ) / A; //Autodesk:F2C++ Array subscript usage: Replaced by below
	Isc = sum_product_sub( QRadSWOutIncident, Surface, &SurfaceData::Area, SurfPtrARR ) / A; //Autodesk:F2C++ Functions handle array subscript usage

	TmeanK = 0.5 * ( TmpTsBaf + Tso ) + KelvinConv;

	Gr = g * pow_3( GapThick ) * std::abs( Tso - TmpTsBaf ) * pow_2( RhoAir ) / ( TmeanK * pow_2( nu ) );

	PassiveGapNusseltNumber( AspRat, Tilt, TmpTsBaf, Tso, Gr, NuPlen ); //intentionally switch Tso to Tsi

	HcPlen = NuPlen * ( k / GapThick );

	// now model natural ventilation of plenum gap.
	VdotWind = Cv * ( VentArea / 2.0 ) * Vwind;

	if ( TaGap > Tamb ) {
		VdotThermal = Cd * ( VentArea / 2.0 ) * std::sqrt( 2.0 * g * HdeltaNPL * ( TaGap - Tamb ) / ( TaGap + KelvinConv ) );
	} else if ( TaGap == Tamb ) {
		VdotThermal = 0.0;
	} else {
		if ( ( std::abs( Tilt ) < 5.0 ) || ( std::abs( Tilt - 180.0 ) < 5.0 ) ) {
			VdotThermal = 0.0; // stable bouyancy situation
		} else {
			VdotThermal = Cd * ( VentArea / 2.0 ) * std::sqrt( 2.0 * g * HdeltaNPL * ( Tamb - TaGap ) / ( Tamb + KelvinConv ) );
		}
	}

	VdotVent = VdotWind + VdotThermal;
	MdotVent = VdotVent * RhoAir;

	//now calculate baffle temperature
	if ( ! ICSCollectorIsOn ) {
		TsBaffle = ( Isc * SolAbs + HExt * Tamb + HrAtm * Tamb + HrSky * SkyTemp + HrGround * Tamb + HrPlen * Tso + HcPlen * TaGap + QdotSource ) / ( HExt + HrAtm + HrSky + HrGround + HrPlen + HcPlen );
	} else {

		TsBaffle = ( ICSULossbottom * ICSWaterTemp + HrPlen * Tso + HcPlen * TaGap + QdotSource ) / ( ICSULossbottom + HrPlen + HcPlen );
	}
	//now calculate gap air temperature

	TaGap = ( HcPlen * A * Tso + MdotVent * CpAir * Tamb + HcPlen * A * TsBaffle ) / ( HcPlen * A + MdotVent * CpAir + HcPlen * A );

	if ( present( HcGapRpt ) ) HcGapRpt = HcPlen;
	if ( present( HrGapRpt ) ) HrGapRpt = HrPlen;
	if ( present( IscRpt ) ) IscRpt = Isc;
	if ( present( MdotVentRpt ) ) MdotVentRpt = MdotVent;
	if ( present( VdotWindRpt ) ) VdotWindRpt = VdotWind;
	if ( present( VdotBouyRpt ) ) VdotBouyRpt = VdotThermal;

}

//****************************************************************************

void
PassiveGapNusseltNumber(
	Real64 const AspRat, // Aspect Ratio of Gap height to gap width
	Real64 const Tilt, // Tilt of gap, degrees
	Real64 const Tso, // Temperature of gap surface closest to outside (K)
	Real64 const Tsi, // Temperature of gap surface closest to zone (K)
	Real64 const Gr, // Gap gas Grashof number
	Real64 & gNu // Gap gas Nusselt number
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Adapted by B. Griffith from Fred Winkelmann's from NusseltNumber in WindowManager.cc
	//       DATE WRITTEN   September 2001
	//       MODIFIED       B. Griffith November 2004  (same models but slightly different for general use)
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// Finds the Nusselt number for air-filled gaps between isothermal solid layers.

	// METHODOLOGY EMPLOYED:
	// Based on methodology in Chapter 5 of the July 18, 2001 draft of ISO 15099,
	// "Thermal Performance of Windows, Doors and Shading Devices--Detailed Calculations."
	// The equation numbers below correspond to those in the standard.

	// REFERENCES:
	// Window5 source code; ISO 15099

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::DegToRadians;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	Real64 const Pr( 0.71 ); // Prandtl number for air

	// INTERFACE BLOCK SPECIFICATIONS

	// DERIVED TYPE DEFINITIONS

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS
	Real64 Ra; // Rayleigh number
	Real64 gnu901; // Nusselt number temporary variables for
	Real64 gnu902;
	Real64 gnu90;
	Real64 gnu601;
	Real64 gnu602; // different tilt and Ra ranges
	Real64 gnu60;
	Real64 gnu601a;
	Real64 gnua;
	Real64 gnub;
	Real64 cra; // Temporary variables
	Real64 a;
	Real64 b;
	Real64 g;
	Real64 ang;
	Real64 tiltr;

	tiltr = Tilt * DegToRadians;
	Ra = Gr * Pr;

	if ( Ra > 2.0e6 ) {

		// write(*,*)' error, outside range of Rayleigh number'
	}

	if ( Ra <= 1.0e4 ) {
		gnu901 = 1.0 + 1.7596678e-10 * std::pow( Ra, 2.2984755 ); // eq. 51
	}
	if ( Ra > 1.0e4 && Ra <= 5.0e4 ) gnu901 = 0.028154 * std::pow( Ra, 0.4134 ); // eq. 50
	if ( Ra > 5.0e4 ) gnu901 = 0.0673838 * std::pow( Ra, 1.0 / 3.0 ); // eq. 49

	gnu902 = 0.242 * std::pow( Ra / AspRat, 0.272 ); // eq. 52
	gnu90 = max( gnu901, gnu902 );

	if ( Tso > Tsi ) { // window heated from above
		gNu = 1.0 + ( gnu90 - 1.0 ) * std::sin( tiltr ); // eq. 53
	} else { // window heated from below
		if ( Tilt >= 60.0 ) {
			g = 0.5 * std::pow( 1.0 + std::pow( Ra / 3160.0, 20.6 ), -0.1 ); // eq. 47
			gnu601a = 1.0 + pow_7( 0.0936 * std::pow( Ra, 0.314 ) / ( 1.0 + g ) ); // eq. 45
			gnu601 = std::pow( gnu601a, 0.142857 );

			// For any aspect ratio
			gnu602 = ( 0.104 + 0.175 / AspRat ) * std::pow( Ra, 0.283 ); // eq. 46
			gnu60 = max( gnu601, gnu602 );

			// linear interpolation for layers inclined at angles between 60 and 90 deg
			gNu = ( ( 90.0 - Tilt ) * gnu60 + ( Tilt - 60.0 ) * gnu90 ) / 30.0;
		}
		if ( Tilt < 60.0 ) { // eq. 42
			cra = Ra * std::cos( tiltr );
			a = 1.0 - 1708.0 / cra;
			b = std::pow( cra / 5830.0, 0.33333 ) - 1.0;
			gnua = ( std::abs( a ) + a ) / 2.0;
			gnub = ( std::abs( b ) + b ) / 2.0;
			ang = 1708.0 * std::pow( std::sin( 1.8 * tiltr ), 1.6 );
			gNu = 1.0 + 1.44 * gnua * ( 1.0 - ang / cra ) + gnub;
		}
	}
}

void
CalcBasinHeaterPower(
	Real64 const Capacity, // Basin heater capacity per degree C below setpoint (W/C)
	int const SchedulePtr, // Pointer to basin heater schedule
	Real64 const SetPointTemp, // setpoint temperature for basin heater operation (C)
	Real64 & Power // Basin heater power (W)
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Chandan Sharma, FSEC
	//       DATE WRITTEN   Feb 2010
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// To calculate basin heater power when the evaporative cooled equipment is not operating
	// and outdoor air dry-bulb temperature is below the set-point

	// METHODOLOGY EMPLOYED:
	// Checks to see whether schedule for basin heater exists or not. If the schedule exists,
	// the basin heater is operated for the schedule specified otherwise the heater runs
	// for the entire simulation timestep whenever the outdoor temperature is below setpoint
	// and water is not flowing through the evaporative cooled equipment.

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using ScheduleManager::GetCurrentScheduleValue;
	using DataEnvironment::OutDryBulbTemp;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	Real64 BasinHeaterSch; // Schedule for basin heater operation

	Power = 0.0;
	// Operate basin heater anytime outdoor temperature is below setpoint and water is not flowing through the equipment
	// IF schedule exists, basin heater performance can be scheduled OFF
	if ( SchedulePtr > 0 ) {
		BasinHeaterSch = GetCurrentScheduleValue( SchedulePtr );
		if ( Capacity > 0.0 && BasinHeaterSch > 0.0 ) {
			Power = max( 0.0, Capacity * ( SetPointTemp - OutDryBulbTemp ) );
		}
	} else {
		// IF schedule does not exist, basin heater operates anytime outdoor dry-bulb temp is below setpoint
		if ( Capacity > 0.0 ) {
			Power = max( 0.0, Capacity * ( SetPointTemp - OutDryBulbTemp ) );
		}
	}
}

void
TestAirPathIntegrity( bool & ErrFound )
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   March 2003
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine tests supply, return and overall air path integrity.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataHVACGlobals::NumPrimaryAirSys;
	using DataAirLoop::AirToZoneNodeInfo;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	//COMPILER-GENERATED INTERFACE MODULE: Thu Sep 29 07:54:46 2011

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int Loop;
	int Loop1;
	int Loop2;
	int Loop3;
	int Count;
	int TestNode;
	bool errFlag;
	Array2D_int ValRetAPaths;
	Array2D_int NumRAPNodes;
	Array2D_int ValSupAPaths;
	Array2D_int NumSAPNodes;

	NumSAPNodes.allocate( NumOfNodes, NumPrimaryAirSys );
	NumRAPNodes.allocate( NumOfNodes, NumPrimaryAirSys );
	ValRetAPaths.allocate( NumOfNodes, NumPrimaryAirSys );
	ValSupAPaths.allocate( NumOfNodes, NumPrimaryAirSys );
	NumSAPNodes = 0;
	NumRAPNodes = 0;
	ValRetAPaths = 0;
	ValSupAPaths = 0;

	TestSupplyAirPathIntegrity( errFlag );
	if ( errFlag ) ErrFound = true;
	TestReturnAirPathIntegrity( errFlag, ValRetAPaths );
	if ( errFlag ) ErrFound = true;

	// Final tests, look for duplicate nodes
	for ( Loop = 1; Loop <= NumPrimaryAirSys; ++Loop ) {
		if ( ValRetAPaths( 1, Loop ) != 0 ) continue;
		if ( AirToZoneNodeInfo( Loop ).NumReturnNodes <= 0 ) continue;
		ValRetAPaths( 1, Loop ) = AirToZoneNodeInfo( Loop ).ZoneEquipReturnNodeNum( 1 );
	}

	for ( Loop = 1; Loop <= NumPrimaryAirSys; ++Loop ) {
		for ( Loop1 = 1; Loop1 <= NumOfNodes; ++Loop1 ) {
			TestNode = ValRetAPaths( Loop1, Loop );
			Count = 0;
			for ( Loop2 = 1; Loop2 <= NumPrimaryAirSys; ++Loop2 ) {
				for ( Loop3 = 1; Loop3 <= NumOfNodes; ++Loop3 ) {
					if ( Loop2 == Loop && Loop1 == Loop3 ) continue; // Don't count test node
					if ( ValRetAPaths( Loop3, Loop2 ) == 0 ) break;
					if ( ValRetAPaths( Loop3, Loop2 ) == TestNode ) ++Count;
				}
			}
			if ( Count > 0 ) {
				ShowSevereError( "Duplicate Node detected in Return Air Paths" );
				ShowContinueError( "Test Node=" + NodeID( TestNode ) );
				ShowContinueError( "In Air Path=" + AirToZoneNodeInfo( Loop ).AirLoopName );
				ErrFound = true;
			}
		}
	}

	NumSAPNodes.deallocate();
	NumRAPNodes.deallocate();
	ValRetAPaths.deallocate();
	ValSupAPaths.deallocate();

}

void
TestSupplyAirPathIntegrity( bool & ErrFound )
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   March 2003
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine tests supply air path integrity and displays the loop for each branch.
	// Also, input and output nodes.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::OutputFileBNDetails;
	using namespace DataLoopNode;
	using SplitterComponent::SplitterCond;
	using SplitterComponent::NumSplitters;
	auto & GetZoneSplitterInput( SplitterComponent::GetSplitterInput );
	using namespace DataZoneEquipment;
	using namespace ZonePlenum;
	using DataAirLoop::AirToZoneNodeInfo;
	using DataHVACGlobals::NumPrimaryAirSys;
	using InputProcessor::SameString;
	using InputProcessor::MakeUPPERCase;
	using InputProcessor::GetNumObjectsFound;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int Count;
	std::string AirPathNodeName; // Air Path Inlet Node Name
	std::string PrimaryAirLoopName; // Air Loop to which this supply air path is connected
	Array1D_bool FoundSupplyPlenum;
	Array1D_bool FoundZoneSplitter;
	Array1D_string FoundNames;
	int NumErr( 0 ); // Error Counter //Autodesk:Init Initialization added
	int BCount;
	int Found;
	std::string ChrOut;
	int Count1;
	int Count2;
	int WAirLoop;

	// Formats
	static gio::Fmt Format_700( "('! <#Supply Air Paths>,<Number of Supply Air Paths>')" );
	static gio::Fmt Format_701( "(A)" );
	static gio::Fmt Format_702( "('! <Supply Air Path>,<Supply Air Path Count>,<Supply Air Path Name>,<AirLoopHVAC Name>')" );
	static gio::Fmt Format_703( "('! <#Components on Supply Air Path>,<Number of Components>')" );
	static gio::Fmt Format_704( "('! <Supply Air Path Component>,<Component Count>,<Component Type>,<Component Name>,','<AirLoopHVAC Name>')" );
	static gio::Fmt Format_705( "('! <#Nodes on Supply Air Path>,<Number of Nodes>')" );
	static gio::Fmt Format_706( "('! <Supply Air Path Node>,<Node Type>,<Node Count>,<Node Name>,<AirLoopHVAC Name>')" );
	static gio::Fmt Format_707( "('! <#Outlet Nodes on Supply Air Path Component>,<Number of Nodes>')" );
	static gio::Fmt Format_708( "('! <Supply Air Path Component Nodes>,<Node Count>,<Component Type>,<Component Name>,','<Inlet Node Name>,<Outlet Node Name>,<AirLoopHVAC Name>')" );

	// Do by Paths
	ShowMessage( "Testing Individual Supply Air Path Integrity" );
	ErrFound = false;

	gio::write( OutputFileBNDetails, Format_701 ) << "! ===============================================================";
	gio::write( OutputFileBNDetails, Format_700 );
	gio::write( ChrOut, fmtLD ) << NumSupplyAirPaths;
	gio::write( OutputFileBNDetails, Format_701 ) << " #Supply Air Paths," + stripped( ChrOut );
	gio::write( OutputFileBNDetails, Format_702 );
	gio::write( OutputFileBNDetails, Format_703 );
	gio::write( OutputFileBNDetails, Format_704 );
	gio::write( OutputFileBNDetails, Format_707 );
	gio::write( OutputFileBNDetails, Format_708 );

	for ( BCount = 1; BCount <= NumSupplyAirPaths; ++BCount ) {

		// Determine which air loop this supply air path is connected to
		Found = 0;
		for ( Count1 = 1; Count1 <= NumPrimaryAirSys; ++Count1 ) {
			PrimaryAirLoopName = AirToZoneNodeInfo( Count1 ).AirLoopName;
			Found = 0;
			for ( Count2 = 1; Count2 <= AirToZoneNodeInfo( Count1 ).NumSupplyNodes; ++Count2 ) {
				if ( SupplyAirPath( BCount ).InletNodeNum == AirToZoneNodeInfo( Count1 ).ZoneEquipSupplyNodeNum( Count2 ) ) Found = Count2;
			}
			if ( Found != 0 ) break;
		}
		if ( Found == 0 ) PrimaryAirLoopName = "**Unknown**";

		gio::write( ChrOut, fmtLD ) << BCount;
		gio::write( OutputFileBNDetails, Format_701 ) << " Supply Air Path," + stripped( ChrOut ) + ',' + SupplyAirPath( BCount ).Name + ',' + PrimaryAirLoopName;

		gio::write( ChrOut, fmtLD ) << SupplyAirPath( BCount ).NumOfComponents;
		gio::write( OutputFileBNDetails, Format_701 ) << "   #Components on Supply Air Path," + stripped( ChrOut );

		AirPathNodeName = NodeID( SupplyAirPath( BCount ).InletNodeNum );

		WAirLoop = 0;

		for ( Count = 1; Count <= SupplyAirPath( BCount ).NumOfComponents; ++Count ) {

			gio::write( ChrOut, fmtLD ) << Count;
			strip( ChrOut );
			gio::write( OutputFileBNDetails, Format_701 ) << "   Supply Air Path Component," + ChrOut + ',' + SupplyAirPath( BCount ).ComponentType( Count ) + ',' + SupplyAirPath( BCount ).ComponentName( Count ) + ',' + PrimaryAirLoopName;

			{ auto const SELECT_CASE_var( MakeUPPERCase( SupplyAirPath( BCount ).ComponentType( Count ) ) );

			if ( SELECT_CASE_var == "AIRLOOPHVAC:SUPPLYPLENUM" ) {
				for ( Count2 = 1; Count2 <= NumZoneSupplyPlenums; ++Count2 ) {
					if ( ZoneSupPlenCond( Count2 ).ZonePlenumName != SupplyAirPath( BCount ).ComponentName( Count ) ) continue;
					if ( Count == 1 && AirPathNodeName != NodeID( ZoneSupPlenCond( Count2 ).InletNode ) ) {
						ShowSevereError( "Error in AirLoopHVAC:SupplyPath=" + SupplyAirPath( BCount ).Name );
						ShowContinueError( "For AirLoopHVAC:SupplyPlenum=" + ZoneSupPlenCond( Count2 ).ZonePlenumName );
						ShowContinueError( "Expected inlet node (supply air path)=" + AirPathNodeName );
						ShowContinueError( "Encountered node name (supply plenum)=" + NodeID( ZoneSupPlenCond( Count2 ).OutletNode( 1 ) ) );
						ErrFound = true;
						++NumErr;
					}
					gio::write( ChrOut, fmtLD ) << ZoneSupPlenCond( Count2 ).NumOutletNodes;
					gio::write( OutputFileBNDetails, Format_701 ) << "     #Outlet Nodes on Supply Air Path Component," + stripped( ChrOut );
					for ( Count1 = 1; Count1 <= ZoneSupPlenCond( Count2 ).NumOutletNodes; ++Count1 ) {
						gio::write( ChrOut, fmtLD ) << Count1;
						gio::write( OutputFileBNDetails, Format_701 ) << "     Supply Air Path Component Nodes," + stripped( ChrOut ) + ',' + SupplyAirPath( BCount ).ComponentType( Count ) + ',' + SupplyAirPath( BCount ).ComponentName( Count ) + ',' + NodeID( ZoneSupPlenCond( Count2 ).InletNode ) + ',' + NodeID( ZoneSupPlenCond( Count2 ).OutletNode( Count1 ) ) + ',' + PrimaryAirLoopName;
					}
				}

			} else if ( SELECT_CASE_var == "AIRLOOPHVAC:ZONESPLITTER" ) {
				for ( Count2 = 1; Count2 <= NumSplitters; ++Count2 ) {
					if ( SplitterCond( Count2 ).SplitterName != SupplyAirPath( BCount ).ComponentName( Count ) ) continue;
					if ( Count == 1 && AirPathNodeName != NodeID( SplitterCond( Count2 ).InletNode ) ) {
						ShowSevereError( "Error in AirLoopHVAC:SupplyPath=" + SupplyAirPath( BCount ).Name );
						ShowContinueError( "For AirLoopHVAC:ZoneSplitter=" + SplitterCond( Count2 ).SplitterName );
						ShowContinueError( "Expected inlet node (supply air path)=" + AirPathNodeName );
						ShowContinueError( "Encountered node name (zone splitter)=" + NodeID( SplitterCond( Count2 ).InletNode ) );
						ErrFound = true;
						++NumErr;
					}
					gio::write( ChrOut, fmtLD ) << SplitterCond( Count2 ).NumOutletNodes;
					gio::write( OutputFileBNDetails, Format_701 ) << "     #Outlet Nodes on Supply Air Path Component," + stripped( ChrOut );
					for ( Count1 = 1; Count1 <= SplitterCond( Count2 ).NumOutletNodes; ++Count1 ) {
						gio::write( ChrOut, fmtLD ) << Count1;
						gio::write( OutputFileBNDetails, Format_701 ) << "     Supply Air Path Component Nodes," + stripped( ChrOut ) + ',' + SupplyAirPath( BCount ).ComponentType( Count ) + ',' + SupplyAirPath( BCount ).ComponentName( Count ) + ',' + NodeID( SplitterCond( Count2 ).InletNode ) + ',' + NodeID( SplitterCond( Count2 ).OutletNode( Count1 ) ) + ',' + PrimaryAirLoopName;
					}
				}

			} else {
				ShowSevereError( "Invalid Component Type in Supply Air Path=" + SupplyAirPath( BCount ).ComponentType( Count ) );
				ErrFound = true;
				++NumErr;

			}}
		}

		if ( SupplyAirPath( BCount ).NumNodes > 0 ) {
			gio::write( OutputFileBNDetails, Format_705 );
			gio::write( OutputFileBNDetails, Format_706 );
			gio::write( ChrOut, fmtLD ) << SupplyAirPath( BCount ).NumNodes;
			strip( ChrOut );
			gio::write( OutputFileBNDetails, Format_701 ) << "#Nodes on Supply Air Path," + ChrOut;
			for ( Count2 = 1; Count2 <= SupplyAirPath( BCount ).NumNodes; ++Count2 ) {
				gio::write( ChrOut, fmtLD ) << Count2;
				strip( ChrOut );
				if ( SupplyAirPath( BCount ).NodeType( Count2 ) == PathInlet ) {
					gio::write( OutputFileBNDetails, Format_701 ) << "   Supply Air Path Node,Inlet Node," + ChrOut + ',' + NodeID( SupplyAirPath( BCount ).Node( Count2 ) ) + ',' + PrimaryAirLoopName;
				} else if ( SupplyAirPath( BCount ).NodeType( Count2 ) == Intermediate ) {
					gio::write( OutputFileBNDetails, Format_701 ) << "   Supply Air Path Node,Through Node," + ChrOut + ',' + NodeID( SupplyAirPath( BCount ).Node( Count2 ) ) + ',' + PrimaryAirLoopName;
				} else if ( SupplyAirPath( BCount ).NodeType( Count2 ) == Outlet ) {
					gio::write( OutputFileBNDetails, Format_701 ) << "   Supply Air Path Node,Outlet Node," + ChrOut + ',' + NodeID( SupplyAirPath( BCount ).Node( Count2 ) ) + ',' + PrimaryAirLoopName;
				}
			}
		}
	}

	if ( NumSplitters == 0 ) {
		if ( GetNumObjectsFound( "AirLoopHVAC:ZoneSplitter" ) > 0 ) {
			GetZoneSplitterInput();
		}
	}
	if ( NumZoneSupplyPlenums == 0 && NumZoneReturnPlenums == 0 ) {
		if ( GetNumObjectsFound( "AirLoopHVAC:SupplyPlenum" ) > 0 ) {
			GetZonePlenumInput();
		}
	}

	// now the reverse.  is every zone splitter and supply plenum on supply air path
	FoundSupplyPlenum.dimension( NumZoneSupplyPlenums, false );
	FoundZoneSplitter.dimension( NumSplitters, false );
	FoundNames.allocate( NumZoneSupplyPlenums );
	for ( Count1 = 1; Count1 <= NumZoneSupplyPlenums; ++Count1 ) {
		for ( BCount = 1; BCount <= NumSupplyAirPaths; ++BCount ) {
			for ( Count = 1; Count <= SupplyAirPath( BCount ).NumOfComponents; ++Count ) {
				if ( ZoneSupPlenCond( Count1 ).ZonePlenumName != SupplyAirPath( BCount ).ComponentName( Count ) || SupplyAirPath( BCount ).ComponentType( Count ) != "AIRLOOPHVAC:SUPPLYPLENUM" ) continue;
				if ( FoundSupplyPlenum( Count1 ) ) {
					ShowSevereError( "AirLoopHVAC:SupplyPlenum=\"" + ZoneSupPlenCond( Count1 ).ZonePlenumName + "\", duplicate entry." );
					ShowContinueError( "already exists on AirLoopHVAC:SupplyPath=\"" + FoundNames( Count1 ) + "\"." );
					ErrFound = true;
				} else {
					// record use
					FoundSupplyPlenum( Count1 ) = true;
					FoundNames( Count1 ) = SupplyAirPath( BCount ).Name;
				}
			}
		}
	}
	FoundNames.deallocate();
	FoundNames.allocate( NumSplitters );
	for ( Count1 = 1; Count1 <= NumSplitters; ++Count1 ) {
		for ( BCount = 1; BCount <= NumSupplyAirPaths; ++BCount ) {
			for ( Count = 1; Count <= SupplyAirPath( BCount ).NumOfComponents; ++Count ) {
				if ( SplitterCond( Count1 ).SplitterName != SupplyAirPath( BCount ).ComponentName( Count ) || SupplyAirPath( BCount ).ComponentType( Count ) != "AIRLOOPHVAC:ZONESPLITTER" ) continue;
				if ( FoundZoneSplitter( Count1 ) ) {
					ShowSevereError( "AirLoopHVAC:ZoneSplitter=\"" + SplitterCond( Count1 ).SplitterName + "\", duplicate entry." );
					ShowContinueError( "already exists on AirLoopHVAC:SupplyPath=\"" + FoundNames( Count1 ) + "\"." );
					ErrFound = true;
				} else {
					// record use
					FoundZoneSplitter( Count1 ) = true;
					FoundNames( Count1 ) = SupplyAirPath( BCount ).Name;
				}
			}
		}
	}
	FoundNames.deallocate();

	if ( ! all( FoundSupplyPlenum ) ) {
		for ( Count1 = 1; Count1 <= NumZoneSupplyPlenums; ++Count1 ) {
			if ( FoundSupplyPlenum( Count1 ) ) continue;
			ShowSevereError( "AirLoopHVAC:SupplyPlenum=\"" + ZoneSupPlenCond( Count1 ).ZonePlenumName + "\", not found on any AirLoopHVAC:SupplyPath." );
			//      ErrFound=.TRUE.
		}
	}

	if ( ! all( FoundZoneSplitter ) ) {
		for ( Count1 = 1; Count1 <= NumSplitters; ++Count1 ) {
			if ( FoundZoneSplitter( Count1 ) ) continue;
			ShowSevereError( "AirLoopHVAC:ZoneSplitter=\"" + SplitterCond( Count1 ).SplitterName + "\", not found on any AirLoopHVAC:SupplyPath." );
			//      ErrFound=.TRUE.
		}
	}

	FoundSupplyPlenum.deallocate();
	FoundZoneSplitter.deallocate();

	if ( ErrFound ) {
		ShowSevereError( "Supply Air Path(s) did not pass integrity testing" );
	} else {
		ShowMessage( "All Supply Air Paths passed integrity testing" );
	}

}

void
TestReturnAirPathIntegrity(
	bool & ErrFound,
	Array2S_int ValRetAPaths
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   March 2003
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine tests return air path integrity and displays the loop for each branch.
	// Also, input and output nodes.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// Return Air Path Validity Rules:
	//  Last component (zone mixer or zone return plenum) must resolve to
	//  be the outlet node for the return air path.  Inlets to this component must be outlets from
	//  previous components or "controlled zone outlets"?.
	//  (though converse not true -- each outlet in previous components do not
	//  have to be inlets on this item -- though they must be inputs somewhere in the stream).
	//  If multiple components and no mixer, then a zone return plenums "outlet" must
	//  be represented as an inlet on a later plenum.  i.e. some zone return plenums are
	//  really acting as "mixers" in a sense.  These do not need to be stepwise in succession.
	//  Same caveat for inlets from previous item.
	//  If multiple components and mixer, then prior condition (nested plenums) is allowed as long as
	//  those aren't duplicated as mixer inlets.  (i.e. zone rp 1 => zone rp 2 => zone mixer but
	//  zone rp 1 outlet should not also be inlet to mixer.
	//  Can have (nzrp -- nested zone return plenum, pzrp -- parallel zone return plenum):
	//  nzrp 1 => nzrp 2 & pzrp 3 => zm (inlets from nzrp 2 and pzrp 3).  Or, likewise:
	//  pzrp 1 & pzrp 2 => zm => pzrp 3 (outlets from pzrp 1/2 are inlets to zm whose outlet is an
	//  inlet to pzrp 3 whose outlet is the outlet for the return air path.

	//  Cannot have duplicate nodes in the "inlet" stream?  (i.e. cannot have same zone feeding two independent
	//  plenums, for example).  Similarly, Same return plenum can't be in two air loops nor as two independent
	//  return plenums in one return air path.

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::OutputFileBNDetails;
	using namespace DataLoopNode;
	using namespace DataZoneEquipment;
	using DataAirLoop::AirToZoneNodeInfo;
	using namespace ZonePlenum;
	using DataHVACGlobals::NumPrimaryAirSys;
	using InputProcessor::SameString;
	using InputProcessor::MakeUPPERCase;
	using InputProcessor::GetNumObjectsFound;
	using MixerComponent::MixerCond;
	using MixerComponent::NumMixers;
	auto & GetZoneMixerInput( MixerComponent::GetMixerInput );
	using PoweredInductionUnits::PIUnitHasMixer;
	using HVACSingleDuctInduc::FourPipeInductionUnitHasMixer;

	// Argument array dimensioning

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	int Loop;
	int Count;
	std::string AirPathNodeName; // Air Path Inlet Node Name
	std::string PrimaryAirLoopName; // Air Loop to which this return air path is connected
	Array1D_bool FoundReturnPlenum;
	Array1D_bool FoundZoneMixer;
	Array1D_string FoundNames;
	int NumErr; // Error Counter
	int BCount;
	int Found;
	std::string ChrOut;
	int Count1;
	int Count2;
	bool HasMixer;
	int MixerComp;
	Array1D_int AllNodes;
	int MixerCount;
	int Count3;
	int NumComp;
	int CountNodes;
	int WAirLoop;

	// Formats
	static gio::Fmt Format_700( "('! <#Return Air Paths>,<Number of Return Air Paths>')" );
	static gio::Fmt Format_701( "(A)" );
	static gio::Fmt Format_702( "('! <Return Air Path>,<Return Air Path Count>,<Return Air Path Name>,<AirLoopHVAC Name>')" );
	static gio::Fmt Format_703( "('! <#Components on Return Air Path>,<Number of Components>')" );
	static gio::Fmt Format_704( "('! <Return Air Path Component>,<Component Count>,<Component Type>,<Component Name>,<AirLoopHVAC Name>')" );
	static gio::Fmt Format_705( "('! <#Nodes on Return Air Path>,<Number of Nodes>')" );
	static gio::Fmt Format_706( "('! <Return Air Path Node>,<Node Type>,<Node Count>,<Node Name>,<AirLoopHVAC Name>')" );
	static gio::Fmt Format_707( "('! <#Inlet Nodes on Return Air Path Component>,<Number of Nodes>')" );
	static gio::Fmt Format_708( "('! <Return Air Path Component Nodes>,<Node Count>,<Component Type>,<Component Name>,','<Inlet Node Name>,<Outlet Node Name>,<AirLoopHVAC Name>')" );

	// Do by Paths
	ShowMessage( "Testing Individual Return Air Path Integrity" );
	ErrFound = false;
	NumErr = 0;

	gio::write( OutputFileBNDetails, Format_701 ) << "! ===============================================================";
	gio::write( OutputFileBNDetails, Format_700 );
	gio::write( ChrOut, fmtLD ) << NumReturnAirPaths;
	gio::write( OutputFileBNDetails, Format_701 ) << " #Return Air Paths," + stripped( ChrOut );
	gio::write( OutputFileBNDetails, Format_702 );
	gio::write( OutputFileBNDetails, Format_703 );
	gio::write( OutputFileBNDetails, Format_704 );
	gio::write( OutputFileBNDetails, Format_707 );
	gio::write( OutputFileBNDetails, Format_708 );

	AllNodes.allocate( NumOfNodes );

	for ( BCount = 1; BCount <= NumReturnAirPaths; ++BCount ) {
		//             Determine which air loop this supply air path is connected to
		Found = 0;
		for ( Count1 = 1; Count1 <= NumPrimaryAirSys; ++Count1 ) {
			PrimaryAirLoopName = AirToZoneNodeInfo( Count1 ).AirLoopName;
			Found = 0;
			for ( Count2 = 1; Count2 <= AirToZoneNodeInfo( Count1 ).NumReturnNodes; ++Count2 ) {
				if ( ReturnAirPath( BCount ).OutletNodeNum == AirToZoneNodeInfo( Count1 ).ZoneEquipReturnNodeNum( Count2 ) ) Found = Count2;
			}
			if ( Found != 0 ) break;
		}
		if ( Found == 0 ) PrimaryAirLoopName = "**Unknown**";

		gio::write( ChrOut, fmtLD ) << BCount;
		gio::write( OutputFileBNDetails, Format_701 ) << " Return Air Path," + stripped( ChrOut ) + ',' + ReturnAirPath( BCount ).Name + ',' + PrimaryAirLoopName;

		NumComp = ReturnAirPath( BCount ).NumOfComponents;
		gio::write( ChrOut, fmtLD ) << NumComp;
		gio::write( OutputFileBNDetails, Format_701 ) << "   #Components on Return Air Path," + stripped( ChrOut );

		AirPathNodeName = NodeID( ReturnAirPath( BCount ).OutletNodeNum );

		HasMixer = false;
		MixerComp = 0;
		MixerCount = 0;
		for ( Count = 1; Count <= NumComp; ++Count ) {
			gio::write( ChrOut, fmtLD ) << Count;
			strip( ChrOut );
			gio::write( OutputFileBNDetails, Format_701 ) << "   Return Air Path Component," + ChrOut + ',' + ReturnAirPath( BCount ).ComponentType( Count ) + ',' + ReturnAirPath( BCount ).ComponentName( Count ) + ',' + PrimaryAirLoopName;

			if ( SameString( ReturnAirPath( BCount ).ComponentType( Count ), "AirLoopHVAC:ZoneMixer" ) ) {
				HasMixer = true;
				MixerComp = Count;
				++MixerCount;
			}
		}

		if ( MixerCount > 1 ) {
			ShowSevereError( "Too many zone mixers in Return Air Path=" + ReturnAirPath( BCount ).Name );
			ErrFound = true;
			++NumErr;
			continue;
		}

		AllNodes = 0;
		CountNodes = 0;
		WAirLoop = 0;

		if ( NumComp > 0 ) {

			{ auto const SELECT_CASE_var( MakeUPPERCase( ReturnAirPath( BCount ).ComponentType( NumComp ) ) );

			if ( SELECT_CASE_var == "AIRLOOPHVAC:ZONEMIXER" ) {
				for ( Count2 = 1; Count2 <= NumMixers; ++Count2 ) {
					if ( ReturnAirPath( BCount ).ComponentName( NumComp ) != MixerCond( Count2 ).MixerName ) continue;
					// Found correct Mixer (by name), check outlet node vs. return air path outlet node
					if ( AirPathNodeName != NodeID( MixerCond( Count2 ).OutletNode ) ) {
						ShowSevereError( "Error in Return Air Path=" + ReturnAirPath( BCount ).Name );
						ShowContinueError( "For Connector:Mixer=" + ReturnAirPath( BCount ).ComponentName( NumComp ) );
						ShowContinueError( "Expected outlet node (return air path)=" + AirPathNodeName );
						ShowContinueError( "Encountered node name (mixer)=" + NodeID( MixerCond( Count2 ).OutletNode ) );
						ErrFound = true;
						++NumErr;
					} else {
						++CountNodes;
						AllNodes( CountNodes ) = MixerCond( Count2 ).OutletNode;
						for ( Loop = 1; Loop <= MixerCond( Count2 ).NumInletNodes; ++Loop ) {
							++CountNodes;
							AllNodes( CountNodes ) = MixerCond( Count2 ).InletNode( Loop );
						}
					}
					gio::write( ChrOut, fmtLD ) << MixerCond( Count2 ).NumInletNodes;
					gio::write( OutputFileBNDetails, Format_701 ) << "     #Inlet Nodes on Return Air Path Component," + stripped( ChrOut );
					for ( Count1 = 1; Count1 <= MixerCond( Count2 ).NumInletNodes; ++Count1 ) {
						gio::write( ChrOut, fmtLD ) << Count1;
						gio::write( OutputFileBNDetails, Format_701 ) << "     Return Air Path Component Nodes," + stripped( ChrOut ) + ',' + ReturnAirPath( BCount ).ComponentType( NumComp ) + ',' + ReturnAirPath( BCount ).ComponentName( NumComp ) + ',' + NodeID( MixerCond( Count2 ).InletNode( Count1 ) ) + ',' + NodeID( MixerCond( Count2 ).OutletNode ) + ',' + PrimaryAirLoopName;
					}
				}

			} else if ( SELECT_CASE_var == "AIRLOOPHVAC:RETURNPLENUM" ) {
				for ( Count2 = 1; Count2 <= NumZoneReturnPlenums; ++Count2 ) {
					if ( ReturnAirPath( BCount ).ComponentName( NumComp ) != ZoneRetPlenCond( Count2 ).ZonePlenumName ) continue;
					if ( AirPathNodeName != NodeID( ZoneRetPlenCond( Count2 ).OutletNode ) ) {
						ShowSevereError( "Error in Return Air Path=" + ReturnAirPath( BCount ).Name );
						ShowContinueError( "For AirLoopHVAC:ReturnPlenum=" + ReturnAirPath( BCount ).ComponentName( NumComp ) );
						ShowContinueError( "Expected outlet node (return air path)=" + AirPathNodeName );
						ShowContinueError( "Encountered node name (zone return plenum)=" + NodeID( ZoneRetPlenCond( Count2 ).OutletNode ) );
						ErrFound = true;
						++NumErr;
					} else {
						++CountNodes;
						AllNodes( CountNodes ) = ZoneRetPlenCond( Count2 ).OutletNode;
						for ( Loop = 1; Loop <= ZoneRetPlenCond( Count2 ).NumInletNodes; ++Loop ) {
							++CountNodes;
							AllNodes( CountNodes ) = ZoneRetPlenCond( Count2 ).InletNode( Loop );
						}
					}
					gio::write( ChrOut, fmtLD ) << ZoneRetPlenCond( Count2 ).NumInletNodes;
					gio::write( OutputFileBNDetails, Format_701 ) << "     #Inlet Nodes on Return Air Path Component," + stripped( ChrOut );
					for ( Count1 = 1; Count1 <= ZoneRetPlenCond( Count2 ).NumInletNodes; ++Count1 ) {
						gio::write( ChrOut, fmtLD ) << Count1;
						gio::write( OutputFileBNDetails, Format_701 ) << "     Return Air Path Component Nodes," + stripped( ChrOut ) + ',' + ReturnAirPath( BCount ).ComponentType( NumComp ) + ',' + ReturnAirPath( BCount ).ComponentName( NumComp ) + ',' + NodeID( ZoneRetPlenCond( Count2 ).InletNode( Count1 ) ) + ',' + NodeID( ZoneRetPlenCond( Count2 ).OutletNode ) + ',' + PrimaryAirLoopName;
					}
				}

			} else {
				// This already validated in GetReturnAirPath

			}}

		}

		if ( NumComp > 1 ) {
			for ( Count3 = 1; Count3 <= NumComp - 1; ++Count3 ) {
				{ auto const SELECT_CASE_var( MakeUPPERCase( ReturnAirPath( BCount ).ComponentType( Count3 ) ) );

				if ( SELECT_CASE_var == "AIRLOOPHVAC:ZONEMIXER" ) {
					for ( Count2 = 1; Count2 <= NumMixers; ++Count2 ) {
						if ( ReturnAirPath( BCount ).ComponentName( Count3 ) != MixerCond( Count2 ).MixerName ) continue;
						for ( Loop = 1; Loop <= MixerCond( Count2 ).NumInletNodes; ++Loop ) {
							++CountNodes;
							AllNodes( CountNodes ) = MixerCond( Count2 ).InletNode( Loop );
						}
					}

				} else if ( SELECT_CASE_var == "AIRLOOPHVAC:RETURNPLENUM" ) {
					for ( Count2 = 1; Count2 <= NumZoneReturnPlenums; ++Count2 ) {
						if ( ReturnAirPath( BCount ).ComponentName( Count3 ) != ZoneRetPlenCond( Count2 ).ZonePlenumName ) continue;
						for ( Loop = 1; Loop <= ZoneRetPlenCond( Count2 ).NumInletNodes; ++Loop ) {
							++CountNodes;
							AllNodes( CountNodes ) = ZoneRetPlenCond( Count2 ).InletNode( Loop );
						}
					}

				} else {
					// This already validated in GetReturnAirPath

				}}

			}
		}
		if ( CountNodes > 0 ) {
			gio::write( OutputFileBNDetails, Format_705 );
			gio::write( OutputFileBNDetails, Format_706 );
			gio::write( ChrOut, fmtLD ) << CountNodes;
			strip( ChrOut );
			gio::write( OutputFileBNDetails, Format_701 ) << "   #Nodes on Return Air Path," + ChrOut;
			for ( Count2 = 1; Count2 <= CountNodes; ++Count2 ) {
				gio::write( ChrOut, fmtLD ) << Count2;
				strip( ChrOut );
				if ( Count2 == 1 ) {
					gio::write( OutputFileBNDetails, Format_701 ) << "   Return Air Path Node,Outlet Node," + ChrOut + ',' + NodeID( AllNodes( Count2 ) ) + ',' + PrimaryAirLoopName;
				} else {
					gio::write( OutputFileBNDetails, Format_701 ) << "   Return Air Path Node,Inlet Node," + ChrOut + ',' + NodeID( AllNodes( Count2 ) ) + ',' + PrimaryAirLoopName;
				}
			}
		}
		// Determine Air Loop this Return Air Path is on
		for ( Count2 = 1; Count2 <= NumPrimaryAirSys; ++Count2 ) {
			if ( AirToZoneNodeInfo( Count2 ).NumReturnNodes > 0 ) {
				if ( AllNodes( 1 ) == AirToZoneNodeInfo( Count2 ).ZoneEquipReturnNodeNum( 1 ) ) {
					WAirLoop = Count2;
					ValRetAPaths( _, WAirLoop ) = 0;
					ValRetAPaths( {1,CountNodes}, WAirLoop ) = AllNodes( {1,CountNodes} );
					break;
				}
			} else {
				ShowWarningError( "TestReturnAirPathIntegrity: Air Loop has no Zone Equipment Return Node=" + AirToZoneNodeInfo( Count2 ).AirLoopName );
			}
		}

	}

	AllNodes.deallocate();

	if ( NumMixers == 0 ) {
		if ( GetNumObjectsFound( "AirLoopHVAC:ZoneMixer" ) > 0 ) {
			GetZoneMixerInput();
		}
	}
	if ( NumZoneSupplyPlenums == 0 && NumZoneReturnPlenums == 0 ) {
		if ( GetNumObjectsFound( "AirLoopHVAC:ReturnPlenum" ) > 0 ) {
			GetZonePlenumInput();
		}
	}

	// now the reverse.  is every zone Mixer and Return plenum on Return air path
	FoundReturnPlenum.dimension( NumZoneReturnPlenums, false );
	FoundZoneMixer.dimension( NumMixers, false );
	FoundNames.allocate( NumZoneReturnPlenums );
	for ( Count1 = 1; Count1 <= NumZoneReturnPlenums; ++Count1 ) {
		for ( BCount = 1; BCount <= NumReturnAirPaths; ++BCount ) {
			for ( Count = 1; Count <= ReturnAirPath( BCount ).NumOfComponents; ++Count ) {
				if ( ZoneRetPlenCond( Count1 ).ZonePlenumName != ReturnAirPath( BCount ).ComponentName( Count ) || ReturnAirPath( BCount ).ComponentType( Count ) != "AIRLOOPHVAC:RETURNPLENUM" ) continue;
				if ( FoundReturnPlenum( Count1 ) ) {
					ShowSevereError( "AirLoopHVAC:ReturnPlenum=\"" + ZoneRetPlenCond( Count1 ).ZonePlenumName + "\", duplicate entry." );
					ShowContinueError( "already exists on AirLoopHVAC:ReturnPath=\"" + FoundNames( Count1 ) + "\"." );
					ErrFound = true;
				} else {
					// record use
					FoundReturnPlenum( Count1 ) = true;
					FoundNames( Count1 ) = ReturnAirPath( BCount ).Name;
				}
			}
		}
	}
	FoundNames.deallocate();
	FoundNames.allocate( NumMixers );
	for ( Count1 = 1; Count1 <= NumMixers; ++Count1 ) {
		for ( BCount = 1; BCount <= NumReturnAirPaths; ++BCount ) {
			for ( Count = 1; Count <= ReturnAirPath( BCount ).NumOfComponents; ++Count ) {
				if ( MixerCond( Count1 ).MixerName != ReturnAirPath( BCount ).ComponentName( Count ) || ReturnAirPath( BCount ).ComponentType( Count ) != "AIRLOOPHVAC:ZONEMIXER" ) continue;
				if ( FoundZoneMixer( Count1 ) ) {
					ShowSevereError( "AirLoopHVAC:ZoneMixer=\"" + MixerCond( Count1 ).MixerName + "\", duplicate entry." );
					ShowContinueError( "already exists on AirLoopHVAC:ReturnPath=\"" + FoundNames( Count1 ) + "\"." );
					ErrFound = true;
				} else {
					// record use
					FoundZoneMixer( Count1 ) = true;
					FoundNames( Count1 ) = ReturnAirPath( BCount ).Name;
				}
			}
		}
		if ( ! FoundZoneMixer( Count1 ) ) { // could be as child on other items
			// PIU Units
			if ( PIUnitHasMixer( MixerCond( Count1 ).MixerName ) ) FoundZoneMixer( Count1 ) = true;
		}
		if ( ! FoundZoneMixer( Count1 ) ) { // could be as child on other items
			// fourPipeInduction units
			if ( FourPipeInductionUnitHasMixer( MixerCond( Count1 ).MixerName ) ) FoundZoneMixer( Count1 ) = true;
		}
	}
	FoundNames.deallocate();

	if ( ! all( FoundReturnPlenum ) ) {
		for ( Count1 = 1; Count1 <= NumZoneReturnPlenums; ++Count1 ) {
			if ( FoundReturnPlenum( Count1 ) ) continue;
			ShowSevereError( "AirLoopHVAC:ReturnPlenum=\"" + ZoneRetPlenCond( Count1 ).ZonePlenumName + "\", not found on any AirLoopHVAC:ReturnPath." );
			//      ErrFound=.TRUE.
		}
	}

	if ( ! all( FoundZoneMixer ) ) {
		for ( Count1 = 1; Count1 <= NumMixers; ++Count1 ) {
			if ( FoundZoneMixer( Count1 ) ) continue;
			ShowSevereError( "AirLoopHVAC:ZoneMixer=\"" + MixerCond( Count1 ).MixerName + "\", not found on any AirLoopHVAC:ReturnPath, AirTerminal:SingleDuct:SeriesPIU:Reheat," );
			ShowContinueError( "AirTerminal:SingleDuct:ParallelPIU:Reheat or AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction." );
			//      ErrFound=.TRUE.
		}
	}

	FoundReturnPlenum.deallocate();
	FoundZoneMixer.deallocate();

	if ( ErrFound ) {
		ShowSevereError( "Return Air Path(s) did not pass integrity testing" );
	} else {
		ShowMessage( "All Return Air Paths passed integrity testing" );
	}

}


} // EnergyPlus
