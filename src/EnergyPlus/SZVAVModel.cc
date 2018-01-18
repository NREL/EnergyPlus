// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

// C++ Headers
#include <cassert>
#include <cmath>
#include <cstdlib>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <SZVAVModel.hh>
#include <General.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <HVACUnitarySystem.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>

namespace EnergyPlus {

namespace SZVAVModel {

	// Module containing routines for general use


	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataHVACGlobals::HVACSystemRootFinding;
	using DataHVACGlobals::Bisection;

	// Data
	// This module should not contain variables in the module sense as it is
	// intended strictly to provide "interfaces" to routines used by other
	// parts of the simulation.

	// MODULE PARAMETER DEFINITIONS
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS
	// na

	// INTERFACE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	// na

	//SUBROUTINE SPECIFICATIONS FOR MODULE General
	//PUBLIC  SaveCompDesWaterFlow
	//PUBLIC  ErfFunction

	// Functions

	void
	calcSZVAVModel(
		HVACUnitarySystem::UnitarySystemData & SZVAVModel,
		int const & SysIndex,
		bool const & FirstHVACIteration,
		bool const & CoolingLoad,
		bool const & HeatingLoad,
		Real64 const & ZoneLoad,
		Real64 & OnOffAirFlowRatio,
		bool const & HXUnitOn,
		int const & AirLoopNum,
		Real64 & PartLoadRatio,
		Real64 const & NoLoadSensibleOutput,
		Real64 const & NoLoadOutletTemp,
		Real64 const & FullSensibleOutput,
		Real64 const & FullLoadAirOutletTemp,
		int const & CompressorONFlag
	)
	{

		// Using/Aliasing
		using HVACUnitarySystem::CalcUnitarySystemToLoad;
		using HVACUnitarySystem::CalcUnitarySystemWaterFlowResidual;
		using PlantUtilities::SetComponentFlowRate;
		using Psychrometrics::PsyHFnTdbW;

		int const MaxIter( 100 ); // maximum number of iterations
		int SolFlag; // return flag from RegulaFalsi for sensible load

		Array1D< Real64 > Par( 17 ); // parameters passed to RegulaFalsi function
		Real64 maxCoilFluidFlow;
		Real64 maxOutletTemp;
		Real64 minAirMassFlow;
		Real64 maxAirMassFlow;
		Real64 lowSpeedFanRatio;
		int coilFluidInletNode;
		int coilFluidOutletNode;
		int coilLoopNum;
		int coilLoopSide;
		int coilBranchNum;
		int coilCompNum;
		int coilAirInletNode;
		int coilAirOutletNode;

		Real64 TempSensOutput; // iterative sensible capacity [W]
		Real64 TempLatOutput; // iterative latent capacity [W]

		// set up mode specific variables to use in common function calls
		if ( CoolingLoad ) {
			maxCoilFluidFlow = SZVAVModel.MaxCoolCoilFluidFlow;
			maxOutletTemp = SZVAVModel.DesignMinOutletTemp;
			minAirMassFlow = SZVAVModel.MaxNoCoolHeatAirMassFlow;
			maxAirMassFlow = SZVAVModel.MaxCoolAirMassFlow;
			lowSpeedFanRatio = SZVAVModel.LowSpeedCoolFanRatio;
			coilFluidInletNode = SZVAVModel.CoolCoilFluidInletNode;
			coilFluidOutletNode = SZVAVModel.CoolCoilFluidOutletNodeNum;
			coilLoopNum = SZVAVModel.CoolCoilLoopNum;
			coilLoopSide = SZVAVModel.CoolCoilLoopSide;
			coilBranchNum = SZVAVModel.CoolCoilBranchNum;
			coilCompNum = SZVAVModel.CoolCoilCompNum;
			coilAirInletNode = SZVAVModel.CoolCoilInletNodeNum;
			coilAirOutletNode = SZVAVModel.CoolCoilOutletNodeNum;
		} else if ( HeatingLoad ) {
			maxCoilFluidFlow = SZVAVModel.MaxHeatCoilFluidFlow;
			maxOutletTemp = SZVAVModel.DesignMaxOutletTemp;
			minAirMassFlow = SZVAVModel.MaxNoCoolHeatAirMassFlow;
			maxAirMassFlow = SZVAVModel.MaxHeatAirMassFlow;
			lowSpeedFanRatio = SZVAVModel.LowSpeedHeatFanRatio;
			coilFluidInletNode = SZVAVModel.HeatCoilFluidInletNode;
			coilFluidOutletNode = SZVAVModel.HeatCoilFluidOutletNodeNum;
			coilLoopNum = SZVAVModel.HeatCoilLoopNum;
			coilLoopSide = SZVAVModel.HeatCoilLoopSide;
			coilBranchNum = SZVAVModel.HeatCoilBranchNum;
			coilCompNum = SZVAVModel.HeatCoilCompNum;
			coilAirInletNode = SZVAVModel.HeatCoilInletNodeNum;
			coilAirOutletNode = SZVAVModel.HeatCoilOutletNodeNum;
		} else { // should never get here, protect against uninitialized variables
			maxCoilFluidFlow = 0.0;
			maxOutletTemp = 0.0;
			minAirMassFlow = 0.0;
			maxAirMassFlow = 0.0;
			lowSpeedFanRatio = 0.0;
			coilFluidInletNode = 0;
			coilFluidOutletNode = 0;
			coilLoopNum = 0;
			coilLoopSide = 0;
			coilBranchNum = 0;
			coilCompNum = 0;
			coilAirInletNode = 0;
			coilAirOutletNode = 0;
		}
		// set up RegulaFalsi variables
		Par( 1 ) = double( SysIndex );
		Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
		if ( FirstHVACIteration ) Par( 2 ) = 1.0;
		Par( 3 ) = double( SZVAVModel.ControlZoneNum );
		Par( 4 ) = ZoneLoad; // load to be met
		Par( 5 ) = double( SZVAVModel.AirInNode );
		Par( 6 ) = OnOffAirFlowRatio;
		Par( 7 ) = double( AirLoopNum );
		Par( 8 ) = double( coilFluidInletNode );
		// initialize other RegulaFalsi variables to most common state 
		Par( 9 ) = 0.0; // minCoilFluidFlow - low fan speed water flow rate
						//			Par( 10 ) = maxCoilFluidFlow; // max water flow rate used by RegulaFalsi
		Par( 11 ) = lowSpeedFanRatio; // ratio of low speed fan flow to max flow
		Par( 12 ) = minAirMassFlow; // operating air flow rate, minAirMassFlow indicates low speed air flow rate, maxAirMassFlow indicates full air flow
		Par( 14 ) = maxAirMassFlow; // constant, denotes system maximum air flow rate
		if ( CoolingLoad ) {
			Par( 15 ) = 1.0;
		} else {
			Par( 15 ) = 0.0;
		}
		Par( 16 ) = 1.0; // iteration method, 1 = modulate coil capacity, 2 = modulate air flow rate
		Par( 17 ) = double( CompressorONFlag ); // ** not used, gets rid of warning (unused variable) in PTUnit version of SZVAV

		int InletNode = SZVAVModel.AirInNode;
		int OutletNode = SZVAVModel.AirOutNode;
		Real64 ZoneTemp = DataLoopNode::Node( SZVAVModel.NodeNumOfControlledZone ).Temp;
		// initialize flow variables to 0
		Real64 lowWaterMdot = 0.0;

		// model attempts to control air and water flow rate in specific operating regions:
		// Region 1 (R1) - minimum air flow rate at modulated coil capacity (up to min/max temperature limits)
		// Region 2 (R2) - modultated air flow rate and coil capacity (up to max air flow rate while maintaining min/max temperature limits)
		// Region 3 (R3) - maximum air flow rate and modulated/increased coil capacity (allow increased capacity at full air flow rate to meet remaining load)
		//
		//                |    |                   |    |    ^            ^ = supply air temperature
		//                |    |                   |    | ^               * = supply air flow rate
		//                |    |                   |^^^^| <--- maximum supply air temperture
		//                |    |                ^  |    |
		//                |    |              ^    |    |
		//     ***********|    |            ^      |    |**************   <-- max unit air flow rate
		//                |*   |          ^        |   *|
		//                | *  |        ^          |  * |
		//                |  * |      ^            | *  |
		//                |   *|    ^              |*   |
		//                |    |*******************|    |                 <-- min unit air flow rate
		//          R3    | R2 | ^       R1        | R2 |    R3
		//   min SAT -->  |^^^^|                   |    |
		//               ^
		//             ^                  |
		//  (-) increasing cooling load < 0 > increasing heating load (+)
		//
		// Notes: SAT is allowed to decrease/increase once air flow rate is max'ed out otherwise load would not be met
		//        Load here is not the zone load, it's the load the system must meet to meet the Tstat set point (i.e., OA can alter required capacity)
		//        lowSpeedFanRatio = min/max unit air flow rate
		//
		// Step 1: set low fan speed and simulate full coil capacity
		// Step 2: if capacity exceeds load, check outlet air temperature (and calculate new capacity)
		// Step 3: check capacity wrt load
		//         IF load < capacity
		//           Region 1 solution
		//           Step 4: iterate on coil capacity at minimum air flow rate
		//         ELSE
		//           Step 5: set high fan speed and simulate full coil capacity
		//           Step 6: if capacity exceeds load, check outlet air temperature (and calculate new capacity)
		//                   IF load < capacity
		//                     Region 2 solution
		//                     Step 7: find operating air flow rate and coil capacity
		//                     Step 8: IF coil capacity was modulated to meet outlet air SP, load can be met, and air flow should be reduced
		//                   ELSE
		//                     Region 3 solution
		//                     Step 9: not enough cooling or heating, increase coil capacity
		//       DONE
		//
		// Step 1: set min air flow and full coil capacity
		PartLoadRatio = 1.0; // full coil capacity
		SZVAVModel.FanPartLoadRatio = 0.0; // minimum fan PLR, air flow = ( fanPartLoadRatio * maxAirMassFlow ) + ( ( 1.0 - fanPartLoadRatio ) * minAirMassFlow )
		DataLoopNode::Node( InletNode ).MassFlowRate = minAirMassFlow;
		// set max water flow rate and check to see if plant limits flow
		if ( coilLoopNum > 0 ) SetComponentFlowRate( maxCoilFluidFlow, coilFluidInletNode, coilFluidOutletNode, coilLoopNum, coilLoopSide, coilBranchNum, coilCompNum );
		Par( 10 ) = maxCoilFluidFlow; // max water flow rate limited by plant

		if ( CoolingLoad ) { // Function CalcUnitarySystemToLoad, 4th and 5th arguments are CoolPLR and HeatPLR
							 // set the water flow ratio so water coil gets proper flow
			SZVAVModel.CoolCoilWaterFlowRatio = maxCoilFluidFlow / SZVAVModel.MaxCoolCoilFluidFlow;
			CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, PartLoadRatio, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
		} else {
			SZVAVModel.HeatCoilWaterFlowRatio = maxCoilFluidFlow / SZVAVModel.MaxHeatCoilFluidFlow;
			CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, PartLoadRatio, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
		}
		bool coilActive = DataLoopNode::Node( coilAirInletNode ).Temp - DataLoopNode::Node( coilAirOutletNode ).Temp;
		Real64 outletTemp = DataLoopNode::Node( OutletNode ).Temp;

		// Step 2: if capacity exceeds load check temperature limits
		// no need to check temperature limits if load exceeds full capacity at low fan speed
		if ( ( CoolingLoad && ZoneLoad > TempSensOutput ) || ( HeatingLoad && ZoneLoad < TempSensOutput ) ) { // low speed fan can meet load prior to temp limit check
																											  // check airside temperature limits to see if coil capacity needs to be reduced (if inlet temp exceeds max do nothing?)
			if ( ( CoolingLoad && outletTemp < maxOutletTemp  && NoLoadOutletTemp > maxOutletTemp ) || ( HeatingLoad && outletTemp > maxOutletTemp && NoLoadOutletTemp < maxOutletTemp ) ) {

				// find coil capacity that meets max SA temperature
				Par( 13 ) = maxOutletTemp; // iterate on SA temperature
				General::SolveRoot( 0.001, MaxIter, SolFlag, PartLoadRatio, CalcUnitarySystemWaterFlowResidual, 0.0, 1.0, Par );

				if ( CoolingLoad ) { // 4th and 5th arguments are CoolPLR and HeatPLR
					CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, PartLoadRatio, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
				} else {
					CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, PartLoadRatio, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
				}
				if ( coilLoopNum > 0 ) lowWaterMdot = DataLoopNode::Node( coilFluidInletNode ).MassFlowRate; // save adjusted water flow rate for later use if needed

				if ( SolFlag < 0 ) {
					if ( SolFlag == -1 ) {
						if ( abs( DataLoopNode::Node( OutletNode ).Temp - maxOutletTemp ) > 0.1 ) { // oulet temperature can sometimes fluctuate around the target by 0.02+ even when PLR changes by 1E-12
							if ( SZVAVModel.MaxIterIndex == 0 ) {
								ShowWarningMessage( "Step 2: Coil control failed to converge for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
								ShowContinueError( "  Iteration limit exceeded in calculating system supply air outlet temperature." );
								ShowContinueErrorTimeStamp( "Supply air temperature target = " + General::TrimSigDigits( maxOutletTemp, 3 ) + " (C), supply air temperature = " + General::TrimSigDigits( DataLoopNode::Node( OutletNode ).Temp, 3 ) + " (C), and the simulation continues." );
							}
							ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - Iteration limit exceeded in calculating supply air temperature continues. Temperature statistics:", SZVAVModel.MaxIterIndex, DataLoopNode::Node( OutletNode ).Temp, DataLoopNode::Node( OutletNode ).Temp );
						}
					} else if ( SolFlag == -2 ) { // should not get here
						if ( SZVAVModel.RegulaFalsIFailedIndex == 0 ) {
							ShowWarningMessage( "Step 2: Coil control failed for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
							ShowContinueError( "  supply air temperature target determined to be outside the range." );
							ShowContinueErrorTimeStamp( "Supply air temperature = " + General::TrimSigDigits( maxOutletTemp, 3 ) + " (C), and the simulation continues." );
						}
						ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - supply air temperature outside of range error continues. Temperature statistics:", SZVAVModel.RegulaFalsIFailedIndex, DataLoopNode::Node( OutletNode ).Temp, DataLoopNode::Node( OutletNode ).Temp );
					}
				}
			}
		}
		// Step 3: check capacity wrt load
		if ( ( CoolingLoad && ( ZoneLoad > TempSensOutput ) ) || ( HeatingLoad && ( ZoneLoad < TempSensOutput ) ) ) { // low speed fan can meet load

																													  // Region 1: Low fan speed, modulate coil capacity
																													  // Step 4: if capacity exceeds load then modulate coil capacity
																													  //solve for the coil capacity at low speed fan
			Par( 12 ) = minAirMassFlow; // operating air flow rate, minAirMassFlow indicates low speed air flow rate, maxAirMassFlow indicates full air flow
			Par( 13 ) = 0.0; // SA Temp target, 0 means iterate on load and not SA temperature
			General::SolveRoot( 0.001, MaxIter, SolFlag, PartLoadRatio, CalcUnitarySystemWaterFlowResidual, 0.0, 1.0, Par );

			if ( SolFlag < 0 ) {

				if ( SolFlag == -1 ) {

					// get result for reporting
					if ( CoolingLoad ) { // 4th and 5th arguments are CoolPLR and HeatPLR
						CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, PartLoadRatio, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
					} else {
						CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, PartLoadRatio, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
					}

					if ( abs( TempSensOutput - ZoneLoad ) * SZVAVModel.ControlZoneMassFlowFrac > 15.0 ) { // water coil can provide same output at varying water PLR (model discontinuity?)
						if ( SZVAVModel.MaxIterIndex == 0 ) {
							ShowWarningMessage( "Step 4: Coil control failed to converge for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
							ShowContinueError( "  Iteration limit exceeded in calculating system sensible part-load ratio." );
							ShowContinueErrorTimeStamp( "Sensible load to be met = " + General::TrimSigDigits( ZoneLoad, 2 ) + " (watts), sensible output = " + General::TrimSigDigits( TempSensOutput, 2 ) + " (watts), and the simulation continues." );
						}
						ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - Iteration limit exceeded in calculating sensible part-load ratio error continues. Sensible load statistics:", SZVAVModel.MaxIterIndex, ZoneLoad, ZoneLoad );
					}
				} else if ( SolFlag == -2 ) {
					if ( SZVAVModel.RegulaFalsIFailedIndex == 0 ) {
						ShowWarningMessage( "Step 4: Coil control failed for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
						ShowContinueError( "  sensible part-load ratio determined to be outside the range of 0-1." );
						ShowContinueErrorTimeStamp( "Sensible load to be met = " + General::TrimSigDigits( ZoneLoad, 2 ) + " (watts), and the simulation continues." );
					}
					ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - sensible part-load ratio out of range error continues. Sensible load statistics:", SZVAVModel.RegulaFalsIFailedIndex, ZoneLoad, ZoneLoad );
				}
			}

		} else { // low speed fan cannot meet load (not enough cooling or heating with coil on at full capacity or capacity limited by SAT)
				 // we know here that high speed fan and full coil capacity can meet the load (i.e., FullSensibleOutput exceeds ZoneLoad)
				 // Step 5: set high speed fan and full coil capacity

			if ( coilActive ) { // if coil is not active only fan induced heat exceeds the load and iteration is not possible

				SZVAVModel.FanPartLoadRatio = 1.0;
				outletTemp = FullLoadAirOutletTemp;
				TempSensOutput = FullSensibleOutput;
				Real64 coilFluidFlow = maxCoilFluidFlow;

				// Step 6: check airside temperature limit to see if coil capacity needs to be reduced (assumes capacity exceeds load otherwise PLR = 1 and we shouldn't be here (i.e., a return was invoked previously)
				if ( ( CoolingLoad && outletTemp < maxOutletTemp ) || ( HeatingLoad && outletTemp > maxOutletTemp ) ) {

					// must check full flow outlet temp with coils off before calling regula falsi
					SZVAVModel.CoolCoilWaterFlowRatio = 0.0;
					SZVAVModel.HeatCoilWaterFlowRatio = 0.0;
					CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
					outletTemp = DataLoopNode::Node( OutletNode ).Temp;

					if ( ( CoolingLoad && outletTemp > maxOutletTemp ) || ( HeatingLoad && outletTemp < maxOutletTemp ) ) {
						// calculate capacity at max outlet air temperature limit
						// find coil PLR that meets SA temperature limit
						Par( 12 ) = maxAirMassFlow; // indicate high speed air flow rate
						Par( 13 ) = maxOutletTemp; // other than 0 means to iterate on SA temperature
						General::SolveRoot( 0.001, MaxIter, SolFlag, PartLoadRatio, CalcUnitarySystemWaterFlowResidual, 0.0, 1.0, Par );
						outletTemp = DataLoopNode::Node( OutletNode ).Temp;

						if ( coilLoopNum > 0 ) coilFluidFlow = DataLoopNode::Node( coilFluidInletNode ).MassFlowRate; // save adjusted water flow rate for later use if needed

						if ( CoolingLoad ) { // 4th and 5th arguments are CoolPLR and HeatPLR
							CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, PartLoadRatio, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
						} else {
							CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, PartLoadRatio, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
						}

						if ( SolFlag < 0 ) {
							if ( SolFlag == -1 ) {
								if ( abs( DataLoopNode::Node( OutletNode ).Temp - maxOutletTemp ) > 0.1 ) { // oulet temperature can sometimes fluctuate around the target by 0.02+ even when PLR changes by 1E-12
									if ( SZVAVModel.MaxIterIndex == 0 ) {
										ShowWarningMessage( "Step 6: Coil control failed to converge for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
										ShowContinueError( "  Iteration limit exceeded in calculating system supply air outlet temperature." );
										ShowContinueErrorTimeStamp( "Supply air temperature target = " + General::TrimSigDigits( maxOutletTemp, 3 ) + " (C), supply air temperature = " + General::TrimSigDigits( DataLoopNode::Node( OutletNode ).Temp, 3 ) + " (C), and the simulation continues." );
									}
									ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - Iteration limit exceeded in calculating supply air temperature continues. Temperature statistics:", SZVAVModel.MaxIterIndex, DataLoopNode::Node( OutletNode ).Temp, DataLoopNode::Node( OutletNode ).Temp );
								}
							} else if ( SolFlag == -2 ) { // should not get here
								if ( SZVAVModel.RegulaFalsIFailedIndex == 0 ) {
									ShowWarningMessage( "Step 6: Coil control failed for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
									ShowContinueError( "  supply air temperature target determined to be outside the range." );
									ShowContinueErrorTimeStamp( "Supply air temperature = " + General::TrimSigDigits( maxOutletTemp, 3 ) + " (C), and the simulation continues." );
								}
								ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - supply air temperature outside of range error continues. Temperature statistics:", SZVAVModel.RegulaFalsIFailedIndex, DataLoopNode::Node( OutletNode ).Temp, DataLoopNode::Node( OutletNode ).Temp );
							}
						}
						outletTemp = DataLoopNode::Node( OutletNode ).Temp;

					} else {
						TempSensOutput = FullSensibleOutput;
						outletTemp = FullLoadAirOutletTemp;
					}

				}

				// Step 7: IF capacity exceeds load then find operating air flow rate and coil capacity
				if ( ( CoolingLoad && TempSensOutput < ZoneLoad ) || ( HeatingLoad && TempSensOutput > ZoneLoad ) ) { // high speed fan and full (or limited by SAT) water flow can meet load
																													  // Region 2: air flow rate and coil capacity should modulate to meet load (within air min/max temperature limits)
																													  // Step 8: IF coil capacity was modulated to meet outlet air SP, load can be met, and air flow should be reduced
																													  // back calculate air flow rate needed to meet outlet air temperature limit
					Real64 MinHumRatio = DataLoopNode::Node( SZVAVModel.NodeNumOfControlledZone ).HumRat;
					if ( outletTemp < ZoneTemp ) MinHumRatio = DataLoopNode::Node( OutletNode ).HumRat;
					// this air flow rate should be between min and max, safer to just make sure
					Real64 AirMassFlow = min( maxAirMassFlow, ( ZoneLoad / ( PsyHFnTdbW( outletTemp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) ) ) );
					AirMassFlow = max( minAirMassFlow, AirMassFlow );
					SZVAVModel.FanPartLoadRatio = ( ( AirMassFlow - ( maxAirMassFlow * lowSpeedFanRatio ) ) / ( ( 1.0 - lowSpeedFanRatio ) * maxAirMassFlow ) );

					// can the load be met and SAT limit exceeded at this air flow rate?
					PartLoadRatio = 1.0;
					if ( CoolingLoad ) { // Function CalcUnitarySystemToLoad, 4th and 5th arguments are CoolPLR and HeatPLR
										 // set the water flow ratio so water coil gets proper flow
						SZVAVModel.CoolCoilWaterFlowRatio = maxCoilFluidFlow / SZVAVModel.MaxCoolCoilFluidFlow;
						CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, PartLoadRatio, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
					} else {
						SZVAVModel.HeatCoilWaterFlowRatio = maxCoilFluidFlow / SZVAVModel.MaxHeatCoilFluidFlow;
						CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, PartLoadRatio, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
					}
					outletTemp = DataLoopNode::Node( OutletNode ).Temp;
					if ( ( CoolingLoad && outletTemp < maxOutletTemp ) || ( HeatingLoad && outletTemp > maxOutletTemp ) ) {

						outletTemp = maxOutletTemp;
						AirMassFlow = min( maxAirMassFlow, ( ZoneLoad / ( PsyHFnTdbW( outletTemp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) ) ) );
						AirMassFlow = max( minAirMassFlow, AirMassFlow );
						SZVAVModel.FanPartLoadRatio = ( ( AirMassFlow - ( maxAirMassFlow * lowSpeedFanRatio ) ) / ( ( 1.0 - lowSpeedFanRatio ) * maxAirMassFlow ) );

						Par( 9 ) = lowWaterMdot; // minCoilFluidFlow - low fan speed water flow rate > 0 if SAT limited
						Par( 10 ) = maxCoilFluidFlow; // max water flow rate
						Par( 12 ) = AirMassFlow; // sets air flow rate used when iterating on coil capacity
						Par( 13 ) = 0.0; // other than 0 means to iterate on SA temperature

						General::SolveRoot( 0.001, MaxIter, SolFlag, PartLoadRatio, CalcUnitarySystemWaterFlowResidual, 0.0, 1.0, Par );

					} else {

						// reset the AirMassFlow rate using the max air outlet temp calculated above (should yield lower air mass flow rate and outlet air temp as close to limit as possible)
						AirMassFlow = min( maxAirMassFlow, ( ZoneLoad / ( PsyHFnTdbW( outletTemp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) ) ) );
						AirMassFlow = max( minAirMassFlow, AirMassFlow );
						SZVAVModel.FanPartLoadRatio = ( ( AirMassFlow - ( maxAirMassFlow * lowSpeedFanRatio ) ) / ( ( 1.0 - lowSpeedFanRatio ) * maxAirMassFlow ) );

						Par( 12 ) = AirMassFlow;  // sets upper limit on air flow rate
						Par( 13 ) = 0.0; // other than 0 means to iterate on SA temperature
						Par( 16 ) = 1.0; // iterate on coil capacity

						General::SolveRoot( 0.001, MaxIter, SolFlag, PartLoadRatio, CalcUnitarySystemWaterFlowResidual, 0.0, 1.0, Par );

					}

					outletTemp = DataLoopNode::Node( OutletNode ).Temp;

					if ( SolFlag < 0 ) {
						if ( SolFlag == -1 ) {
							// get capacity for warning
							if ( CoolingLoad ) { // 4th and 5th arguments are CoolPLR and HeatPLR
								CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, PartLoadRatio, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
							} else {
								CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, PartLoadRatio, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
							}
							if ( abs( DataLoopNode::Node( OutletNode ).Temp - maxOutletTemp ) > 0.1 ) { // oulet temperature can sometimes fluctuate around the target by 0.02+ even when PLR changes by 1E-12
								if ( SZVAVModel.MaxIterIndex == 0 ) {
									ShowWarningMessage( "Step 8: Coil control failed to converge for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
									ShowContinueError( "  Iteration limit exceeded in calculating system supply air outlet temperature." );
									ShowContinueErrorTimeStamp( "Supply air temperature target = " + General::TrimSigDigits( maxOutletTemp, 3 ) + " (C), supply air temperature = " + General::TrimSigDigits( DataLoopNode::Node( OutletNode ).Temp, 3 ) + " (C), and the simulation continues." );
								}
								ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - Iteration limit exceeded in calculating supply air temperature continues. Temperature statistics:", SZVAVModel.MaxIterIndex, DataLoopNode::Node( OutletNode ).Temp, DataLoopNode::Node( OutletNode ).Temp );
							}
						} else if ( SolFlag == -2 ) { // should not get here
							if ( SZVAVModel.RegulaFalsIFailedIndex == 0 ) {
								ShowWarningMessage( "Step 8: Coil control failed for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
								ShowContinueError( "  supply air temperature target determined to be outside the range." );
								ShowContinueErrorTimeStamp( "Supply air temperature = " + General::TrimSigDigits( maxOutletTemp, 3 ) + " (C), and the simulation continues." );
							}
							ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - supply air temperature outside of range error continues. Temperature statistics:", SZVAVModel.RegulaFalsIFailedIndex, DataLoopNode::Node( OutletNode ).Temp, DataLoopNode::Node( OutletNode ).Temp );
						}
					}

				} else { // Step 9: not enough cooling or heating

					if ( CoolingLoad && FullSensibleOutput < ZoneLoad || HeatingLoad && FullSensibleOutput > ZoneLoad ) {

						// Region 3: air flow rate at max while modulating coil capacity to meet load
						Par( 9 ) = lowWaterMdot; // max water flow at low speed fan
						Par( 10 ) = maxCoilFluidFlow; // max water flow rate limited by SAT or max if not limited
						Par( 12 ) = maxAirMassFlow;
						Par( 13 ) = 0.0;
						General::SolveRoot( 0.001, MaxIter, SolFlag, PartLoadRatio, CalcUnitarySystemWaterFlowResidual, 0.0, 1.0, Par );

						if ( SolFlag < 0 ) {
							if ( SolFlag == -1 ) {
								// get capacity for warning
								if ( CoolingLoad ) { // 4th and 5th arguments are CoolPLR and HeatPLR
									CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, PartLoadRatio, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
								} else {
									CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, PartLoadRatio, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
								}

								if ( abs( TempSensOutput - ZoneLoad ) * SZVAVModel.ControlZoneMassFlowFrac > 15.0 ) { // water coil can provide same output at varying water PLR (model discontinuity?)
									if ( SZVAVModel.MaxIterIndex == 0 ) {
										ShowWarningMessage( "Step 9: Coil control failed to converge for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
										ShowContinueError( "  Iteration limit exceeded in calculating system sensible part-load ratio." );
										ShowContinueErrorTimeStamp( "Sensible load to be met = " + General::TrimSigDigits( ZoneLoad, 2 ) + " (watts), sensible output = " + General::TrimSigDigits( TempSensOutput, 2 ) + " (watts), and the simulation continues." );
									}
									ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - Iteration limit exceeded in calculating sensible part-load ratio error continues. Sensible load statistics:", SZVAVModel.MaxIterIndex, ZoneLoad, ZoneLoad );
								}
							} else if ( SolFlag == -2 ) {
								if ( SZVAVModel.RegulaFalsIFailedIndex == 0 ) {
									ShowWarningMessage( "Step 9: Coil control failed for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
									ShowContinueError( "  sensible part-load ratio determined to be outside the range of 0-1." );
									ShowContinueErrorTimeStamp( "Sensible load to be met = " + General::TrimSigDigits( ZoneLoad, 2 ) + " (watts), and the simulation continues." );
								}
								ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - sensible part-load ratio out of range error continues. Sensible load statistics:", SZVAVModel.RegulaFalsIFailedIndex, ZoneLoad, ZoneLoad );
							}
						}

					}

				}

			} else { // coil can be schedule off so set fan at high speed at these times.

				PartLoadRatio = 1.0;
				SZVAVModel.FanPartLoadRatio = 1.0;
				SZVAVModel.CoolCoilWaterFlowRatio = 0.0;
				SZVAVModel.HeatCoilWaterFlowRatio = 0.0;
				if ( coilLoopNum > 0 ) DataLoopNode::Node( coilFluidInletNode ).MassFlowRate = maxCoilFluidFlow;

			}

		}

		if ( coilLoopNum > 0 ) SetComponentFlowRate( DataLoopNode::Node( coilFluidInletNode ).MassFlowRate, coilFluidInletNode, coilFluidOutletNode, coilLoopNum, coilLoopSide, coilBranchNum, coilCompNum );


	}

	void
	calcSZVAVModel(
		PackagedTerminalHeatPump::PTUnitData & SZVAVModel,
		int const & SysIndex,
		bool const & FirstHVACIteration,
		bool const & CoolingLoad,
		bool const & HeatingLoad,
		Real64 const & ZoneLoad,
		Real64 & OnOffAirFlowRatio,
		bool const & HXUnitOn,
		int const & AirLoopNum,
		Real64 & PartLoadRatio,
		Real64 const & NoLoadSensibleOutput,
		Real64 const & NoLoadOutletTemp,
		Real64 const & FullSensibleOutput,
		Real64 const & FullLoadAirOutletTemp,
		int const & CompressorONFlag
	)
	{

		// Using/Aliasing
		using PackagedTerminalHeatPump::CalcPTUnit;
		using PackagedTerminalHeatPump::CalcPTUnitWaterFlowResidual;
		using PlantUtilities::SetComponentFlowRate;
		using Psychrometrics::PsyHFnTdbW;

		int const MaxIter( 100 ); // maximum number of iterations
		int SolFlag; // return flag from RegulaFalsi for sensible load

		Array1D< Real64 > Par( 17 ); // parameters passed to RegulaFalsi function
		Real64 maxCoilFluidFlow;
		Real64 maxOutletTemp;
		Real64 minAirMassFlow;
		Real64 maxAirMassFlow;
		Real64 lowSpeedFanRatio;
		int coilFluidInletNode;
		int coilFluidOutletNode;
		int coilLoopNum;
		int coilLoopSide;
		int coilBranchNum;
		int coilCompNum;
		int coilAirInletNode;
		int coilAirOutletNode;

		Real64 TempSensOutput; // iterative sensible capacity [W]
//		Real64 TempLatOutput; // iterative latent capacity [W]

		// set up mode specific variables to use in common function calls
		if ( CoolingLoad ) {
			maxCoilFluidFlow = SZVAVModel.MaxCoolCoilFluidFlow;
			maxOutletTemp = SZVAVModel.DesignMinOutletTemp;
			minAirMassFlow = SZVAVModel.MaxNoCoolHeatAirMassFlow;
			maxAirMassFlow = SZVAVModel.MaxCoolAirMassFlow;
			lowSpeedFanRatio = SZVAVModel.LowSpeedCoolFanRatio;
			coilFluidInletNode = SZVAVModel.CoolCoilFluidInletNode;
			coilFluidOutletNode = SZVAVModel.CoolCoilFluidOutletNodeNum;
			coilLoopNum = SZVAVModel.CoolCoilLoopNum;
			coilLoopSide = SZVAVModel.CoolCoilLoopSide;
			coilBranchNum = SZVAVModel.CoolCoilBranchNum;
			coilCompNum = SZVAVModel.CoolCoilCompNum;
			coilAirInletNode = SZVAVModel.CoolCoilInletNodeNum;
			coilAirOutletNode = SZVAVModel.CoolCoilOutletNodeNum;
		} else if ( HeatingLoad ) {
			maxCoilFluidFlow = SZVAVModel.MaxHeatCoilFluidFlow;
			maxOutletTemp = SZVAVModel.DesignMaxOutletTemp;
			minAirMassFlow = SZVAVModel.MaxNoCoolHeatAirMassFlow;
			maxAirMassFlow = SZVAVModel.MaxHeatAirMassFlow;
			lowSpeedFanRatio = SZVAVModel.LowSpeedHeatFanRatio;
			coilFluidInletNode = SZVAVModel.HeatCoilFluidInletNode;
			coilFluidOutletNode = SZVAVModel.HeatCoilFluidOutletNodeNum;
			coilLoopNum = SZVAVModel.HeatCoilLoopNum;
			coilLoopSide = SZVAVModel.HeatCoilLoopSide;
			coilBranchNum = SZVAVModel.HeatCoilBranchNum;
			coilCompNum = SZVAVModel.HeatCoilCompNum;
			coilAirInletNode = SZVAVModel.HeatCoilInletNodeNum;
			coilAirOutletNode = SZVAVModel.HeatCoilOutletNodeNum;
		} else { // should never get here, protect against uninitialized variables
			maxCoilFluidFlow = 0.0;
			maxOutletTemp = 0.0;
			minAirMassFlow = 0.0;
			maxAirMassFlow = 0.0;
			lowSpeedFanRatio = 0.0;
			coilFluidInletNode = 0;
			coilFluidOutletNode = 0;
			coilLoopNum = 0;
			coilLoopSide = 0;
			coilBranchNum = 0;
			coilCompNum = 0;
			coilAirInletNode = 0;
			coilAirOutletNode = 0;
		}
		// set up RegulaFalsi variables
		Par( 1 ) = double( SysIndex );
		Par( 2 ) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
		if ( FirstHVACIteration ) Par( 2 ) = 1.0;
		Par( 3 ) = double( SZVAVModel.ControlZoneNum );
		Par( 4 ) = ZoneLoad; // load to be met
		Par( 5 ) = double( SZVAVModel.AirInNode );
		Par( 6 ) = OnOffAirFlowRatio;
		Par( 7 ) = double( AirLoopNum );
		Par( 8 ) = double( coilFluidInletNode );
		// initialize other RegulaFalsi variables to most common state 
		Par( 9 ) = 0.0; // minCoilFluidFlow - low fan speed water flow rate
						//			Par( 10 ) = maxCoilFluidFlow; // max water flow rate used by RegulaFalsi
		Par( 11 ) = lowSpeedFanRatio; // ratio of low speed fan flow to max flow
		Par( 12 ) = minAirMassFlow; // operating air flow rate, minAirMassFlow indicates low speed air flow rate, maxAirMassFlow indicates full air flow
		Par( 14 ) = maxAirMassFlow; // constant, denotes system maximum air flow rate
		if ( CoolingLoad ) {
			Par( 15 ) = 1.0;
		} else {
			Par( 15 ) = 0.0;
		}
		Par( 16 ) = 1.0; // iteration method, 1 = modulate coil capacity, 2 = modulate air flow rate
		Par( 17 ) = double( CompressorONFlag ); // ** not used, gets rid of warning (unused variable) in PTUnit version of SZVAV

		int InletNode = SZVAVModel.AirInNode;
		int OutletNode = SZVAVModel.AirOutNode;
		Real64 ZoneTemp = DataLoopNode::Node( SZVAVModel.NodeNumOfControlledZone ).Temp;
		// initialize flow variables to 0
		Real64 lowWaterMdot = 0.0;
		Real64 SupHeaterLoad = 0.0; // *** new variable used in PTHP

		// model attempts to control air and water flow rate in specific operating regions:
		// Region 1 (R1) - minimum air flow rate at modulated coil capacity (up to min/max temperature limits)
		// Region 2 (R2) - modultated air flow rate and coil capacity (up to max air flow rate while maintaining min/max temperature limits)
		// Region 3 (R3) - maximum air flow rate and modulated/increased coil capacity (allow increased capacity at full air flow rate to meet remaining load)
		//
		//                |    |                   |    |    ^            ^ = supply air temperature
		//                |    |                   |    | ^               * = supply air flow rate
		//                |    |                   |^^^^| <--- maximum supply air temperture
		//                |    |                ^  |    |
		//                |    |              ^    |    |
		//     ***********|    |            ^      |    |**************   <-- max unit air flow rate
		//                |*   |          ^        |   *|
		//                | *  |        ^          |  * |
		//                |  * |      ^            | *  |
		//                |   *|    ^              |*   |
		//                |    |*******************|    |                 <-- min unit air flow rate
		//          R3    | R2 | ^       R1        | R2 |    R3
		//   min SAT -->  |^^^^|                   |    |
		//               ^
		//             ^                  |
		//  (-) increasing cooling load < 0 > increasing heating load (+)
		//
		// Notes: SAT is allowed to decrease/increase once air flow rate is max'ed out otherwise load would not be met
		//        Load here is not the zone load, it's the load the system must meet to meet the Tstat set point (i.e., OA can alter required capacity)
		//        lowSpeedFanRatio = min/max unit air flow rate
		//
		// Step 1: set low fan speed and simulate full coil capacity
		// Step 2: if capacity exceeds load, check outlet air temperature (and calculate new capacity)
		// Step 3: check capacity wrt load
		//         IF load < capacity
		//           Region 1 solution
		//           Step 4: iterate on coil capacity at minimum air flow rate
		//         ELSE
		//           Step 5: set high fan speed and simulate full coil capacity
		//           Step 6: if capacity exceeds load, check outlet air temperature (and calculate new capacity)
		//                   IF load < capacity
		//                     Region 2 solution
		//                     Step 7: find operating air flow rate and coil capacity
		//                     Step 8: IF coil capacity was modulated to meet outlet air SP, load can be met, and air flow should be reduced
		//                   ELSE
		//                     Region 3 solution
		//                     Step 9: not enough cooling or heating, increase coil capacity
		//       DONE
		//
		// Step 1: set min air flow and full coil capacity
		PartLoadRatio = 1.0; // full coil capacity
		SZVAVModel.FanPartLoadRatio = 0.0; // minimum fan PLR, air flow = ( fanPartLoadRatio * maxAirMassFlow ) + ( ( 1.0 - fanPartLoadRatio ) * minAirMassFlow )
		DataLoopNode::Node( InletNode ).MassFlowRate = minAirMassFlow;
		// set max water flow rate and check to see if plant limits flow
		if ( coilLoopNum > 0 ) SetComponentFlowRate( maxCoilFluidFlow, coilFluidInletNode, coilFluidOutletNode, coilLoopNum, coilLoopSide, coilBranchNum, coilCompNum );
		Par( 10 ) = maxCoilFluidFlow; // max water flow rate limited by plant

		if ( CoolingLoad ) { // Function CalcUnitarySystemToLoad, 4th and 5th arguments are CoolPLR and HeatPLR
							 // set the water flow ratio so water coil gets proper flow
			SZVAVModel.CoolCoilWaterFlowRatio = maxCoilFluidFlow / SZVAVModel.MaxCoolCoilFluidFlow;
//			CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, PartLoadRatio, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
		} else {
			SZVAVModel.HeatCoilWaterFlowRatio = maxCoilFluidFlow / SZVAVModel.MaxHeatCoilFluidFlow;
//			CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, PartLoadRatio, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
		}
		CalcPTUnit( SysIndex, FirstHVACIteration, PartLoadRatio, TempSensOutput, ZoneLoad, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );

		bool coilActive = DataLoopNode::Node( coilAirInletNode ).Temp - DataLoopNode::Node( coilAirOutletNode ).Temp;
		Real64 outletTemp = DataLoopNode::Node( OutletNode ).Temp;
		Real64 LowSpeedMaxCapacity = TempSensOutput;
		Real64 LowSpeedOutletTemp = outletTemp;

		// Step 2: if capacity exceeds load check temperature limits
		// no need to check temperature limits if load exceeds full capacity at low fan speed
		if ( ( CoolingLoad && ZoneLoad > TempSensOutput ) || ( HeatingLoad && ZoneLoad < TempSensOutput ) ) { // low speed fan can meet load prior to temp limit check
																											  // check airside temperature limits to see if coil capacity needs to be reduced (if inlet temp exceeds max do nothing?)
			if ( ( CoolingLoad && outletTemp < maxOutletTemp  && NoLoadOutletTemp > maxOutletTemp ) || ( HeatingLoad && outletTemp > maxOutletTemp && NoLoadOutletTemp < maxOutletTemp ) ) {

				// find coil capacity that meets max SA temperature
				Par( 13 ) = maxOutletTemp; // iterate on SA temperature
				General::SolveRoot( 0.001, MaxIter, SolFlag, PartLoadRatio, CalcPTUnitWaterFlowResidual, 0.0, 1.0, Par );

//				if ( CoolingLoad ) { // 4th and 5th arguments are CoolPLR and HeatPLR
//					CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, PartLoadRatio, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
//				} else {
//					CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, PartLoadRatio, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
//				}
				CalcPTUnit( SysIndex, FirstHVACIteration, PartLoadRatio, TempSensOutput, ZoneLoad, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );

				if ( coilLoopNum > 0 ) lowWaterMdot = DataLoopNode::Node( coilFluidInletNode ).MassFlowRate; // save adjusted water flow rate for later use if needed

				if ( SolFlag < 0 ) {
					if ( SolFlag == -1 ) {
						if ( abs( DataLoopNode::Node( OutletNode ).Temp - maxOutletTemp ) > 0.1 ) { // oulet temperature can sometimes fluctuate around the target by 0.02+ even when PLR changes by 1E-12
							if ( SZVAVModel.MaxIterIndex == 0 ) {
								ShowWarningMessage( "Step 2: Coil control failed to converge for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
								ShowContinueError( "  Iteration limit exceeded in calculating system supply air outlet temperature." );
								ShowContinueErrorTimeStamp( "Supply air temperature target = " + General::TrimSigDigits( maxOutletTemp, 3 ) + " (C), supply air temperature = " + General::TrimSigDigits( DataLoopNode::Node( OutletNode ).Temp, 3 ) + " (C), and the simulation continues." );
							}
							ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - Iteration limit exceeded in calculating supply air temperature continues. Temperature statistics:", SZVAVModel.MaxIterIndex, DataLoopNode::Node( OutletNode ).Temp, DataLoopNode::Node( OutletNode ).Temp );
						}
					} else if ( SolFlag == -2 ) { // should not get here
						if ( SZVAVModel.RegulaFalsIFailedIndex == 0 ) {
							ShowWarningMessage( "Step 2: Coil control failed for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
							ShowContinueError( "  supply air temperature target determined to be outside the range." );
							ShowContinueErrorTimeStamp( "Supply air temperature = " + General::TrimSigDigits( maxOutletTemp, 3 ) + " (C), and the simulation continues." );
						}
						ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - supply air temperature outside of range error continues. Temperature statistics:", SZVAVModel.RegulaFalsIFailedIndex, DataLoopNode::Node( OutletNode ).Temp, DataLoopNode::Node( OutletNode ).Temp );
					}
				}
			}
		}
		// Step 3: check capacity wrt load
		if ( ( CoolingLoad && ( ZoneLoad > TempSensOutput ) ) || ( HeatingLoad && ( ZoneLoad < TempSensOutput ) ) ) { // low speed fan can meet load

			// Region 1: Low fan speed, modulate coil capacity
			// Step 4: if capacity exceeds load then modulate coil capacity
			//solve for the coil capacity at low speed fan
			Par( 12 ) = minAirMassFlow; // operating air flow rate, minAirMassFlow indicates low speed air flow rate, maxAirMassFlow indicates full air flow
			Par( 13 ) = 0.0; // SA Temp target, 0 means iterate on load and not SA temperature
			General::SolveRoot( 0.001, MaxIter, SolFlag, PartLoadRatio, CalcPTUnitWaterFlowResidual, 0.0, 1.0, Par );

			if ( SolFlag < 0 ) {

				if ( SolFlag == -1 ) {

					// get result for reporting
//					if ( CoolingLoad ) { // 4th and 5th arguments are CoolPLR and HeatPLR
//						CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, PartLoadRatio, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
//					} else {
//						CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, PartLoadRatio, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
//					}
					CalcPTUnit( SysIndex, FirstHVACIteration, PartLoadRatio, TempSensOutput, ZoneLoad, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );

					if ( abs( TempSensOutput - ZoneLoad ) * SZVAVModel.ControlZoneMassFlowFrac > 15.0 ) { // water coil can provide same output at varying water PLR (model discontinuity?)
						if ( SZVAVModel.MaxIterIndex == 0 ) {
							ShowWarningMessage( "Step 4: Coil control failed to converge for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
							ShowContinueError( "  Iteration limit exceeded in calculating system sensible part-load ratio." );
							ShowContinueErrorTimeStamp( "Sensible load to be met = " + General::TrimSigDigits( ZoneLoad, 2 ) + " (watts), sensible output = " + General::TrimSigDigits( TempSensOutput, 2 ) + " (watts), and the simulation continues." );
						}
						ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - Iteration limit exceeded in calculating sensible part-load ratio error continues. Sensible load statistics:", SZVAVModel.MaxIterIndex, ZoneLoad, ZoneLoad );
					}
				} else if ( SolFlag == -2 ) {
					if ( SZVAVModel.RegulaFalsIFailedIndex == 0 ) {
						ShowWarningMessage( "Step 4: Coil control failed for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
						ShowContinueError( "  sensible part-load ratio determined to be outside the range of 0-1." );
						ShowContinueErrorTimeStamp( "Sensible load to be met = " + General::TrimSigDigits( ZoneLoad, 2 ) + " (watts), and the simulation continues." );
					}
					ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - sensible part-load ratio out of range error continues. Sensible load statistics:", SZVAVModel.RegulaFalsIFailedIndex, ZoneLoad, ZoneLoad );
				}
			}

		} else { // low speed fan cannot meet load (not enough cooling or heating with coil on at full capacity or capacity limited by SAT)
				 // we know here that high speed fan and full coil capacity can meet the load (i.e., FullSensibleOutput exceeds ZoneLoad)
				 // Step 5: set high speed fan and full coil capacity

			if ( coilActive ) { // if coil is not active only fan induced heat exceeds the load and iteration is not possible

				SZVAVModel.FanPartLoadRatio = 1.0;
				outletTemp = FullLoadAirOutletTemp;
				TempSensOutput = FullSensibleOutput;
				Real64 coilFluidFlow = maxCoilFluidFlow;

				// Step 6: check airside temperature limit to see if coil capacity needs to be reduced (assumes capacity exceeds load otherwise PLR = 1 and we shouldn't be here (i.e., a return was invoked previously)
				if ( ( CoolingLoad && outletTemp < maxOutletTemp ) || ( HeatingLoad && outletTemp > maxOutletTemp ) ) {

					// must check full flow outlet temp with coils off before calling regula falsi
					SZVAVModel.CoolCoilWaterFlowRatio = 0.0;
					SZVAVModel.HeatCoilWaterFlowRatio = 0.0;
//					CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
					Real64 TempPLR = 0.0;
					CalcPTUnit( SysIndex, FirstHVACIteration, TempPLR, TempSensOutput, ZoneLoad, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
					outletTemp = DataLoopNode::Node( OutletNode ).Temp;

					if ( ( CoolingLoad && outletTemp > maxOutletTemp ) || ( HeatingLoad && outletTemp < maxOutletTemp ) ) {
						// calculate capacity at max outlet air temperature limit
						// find coil PLR that meets SA temperature limit
						Par( 12 ) = maxAirMassFlow; // indicate high speed air flow rate
						Par( 13 ) = maxOutletTemp; // other than 0 means to iterate on SA temperature
						General::SolveRoot( 0.001, MaxIter, SolFlag, PartLoadRatio, CalcPTUnitWaterFlowResidual, 0.0, 1.0, Par );
						outletTemp = DataLoopNode::Node( OutletNode ).Temp;

						if ( coilLoopNum > 0 ) coilFluidFlow = DataLoopNode::Node( coilFluidInletNode ).MassFlowRate; // save adjusted water flow rate for later use if needed

//						if ( CoolingLoad ) { // 4th and 5th arguments are CoolPLR and HeatPLR
//							CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, PartLoadRatio, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
//						} else {
//							CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, PartLoadRatio, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
//						}
						CalcPTUnit( SysIndex, FirstHVACIteration, PartLoadRatio, TempSensOutput, ZoneLoad, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );

						if ( SolFlag < 0 ) {
							if ( SolFlag == -1 ) {
								if ( abs( DataLoopNode::Node( OutletNode ).Temp - maxOutletTemp ) > 0.1 ) { // oulet temperature can sometimes fluctuate around the target by 0.02+ even when PLR changes by 1E-12
									if ( SZVAVModel.MaxIterIndex == 0 ) {
										ShowWarningMessage( "Step 6: Coil control failed to converge for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
										ShowContinueError( "  Iteration limit exceeded in calculating system supply air outlet temperature." );
										ShowContinueErrorTimeStamp( "Supply air temperature target = " + General::TrimSigDigits( maxOutletTemp, 3 ) + " (C), supply air temperature = " + General::TrimSigDigits( DataLoopNode::Node( OutletNode ).Temp, 3 ) + " (C), and the simulation continues." );
									}
									ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - Iteration limit exceeded in calculating supply air temperature continues. Temperature statistics:", SZVAVModel.MaxIterIndex, DataLoopNode::Node( OutletNode ).Temp, DataLoopNode::Node( OutletNode ).Temp );
								}
							} else if ( SolFlag == -2 ) { // should not get here
								if ( SZVAVModel.RegulaFalsIFailedIndex == 0 ) {
									ShowWarningMessage( "Step 6: Coil control failed for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
									ShowContinueError( "  supply air temperature target determined to be outside the range." );
									ShowContinueErrorTimeStamp( "Supply air temperature = " + General::TrimSigDigits( maxOutletTemp, 3 ) + " (C), and the simulation continues." );
								}
								ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - supply air temperature outside of range error continues. Temperature statistics:", SZVAVModel.RegulaFalsIFailedIndex, DataLoopNode::Node( OutletNode ).Temp, DataLoopNode::Node( OutletNode ).Temp );
							}
						}
						outletTemp = DataLoopNode::Node( OutletNode ).Temp;

					} else {
						TempSensOutput = FullSensibleOutput;
						outletTemp = FullLoadAirOutletTemp;
					}

				}

				// Step 7: IF capacity exceeds load then find operating air flow rate and coil capacity
				if ( ( CoolingLoad && TempSensOutput < ZoneLoad ) || ( HeatingLoad && TempSensOutput > ZoneLoad ) ) { // high speed fan and full (or limited by SAT) water flow can meet load
																													  // Region 2: air flow rate and coil capacity should modulate to meet load (within air min/max temperature limits)
																													  // Step 8: IF coil capacity was modulated to meet outlet air SP, load can be met, and air flow should be reduced
																													  // back calculate air flow rate needed to meet outlet air temperature limit
					Real64 MinHumRatio = DataLoopNode::Node( SZVAVModel.NodeNumOfControlledZone ).HumRat;
					if ( outletTemp < ZoneTemp ) MinHumRatio = DataLoopNode::Node( OutletNode ).HumRat;
					// this air flow rate should be between min and max, safer to just make sure
					Real64 AirMassFlow = min( maxAirMassFlow, ( ZoneLoad / ( PsyHFnTdbW( outletTemp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) ) ) );
					AirMassFlow = max( minAirMassFlow, AirMassFlow );
					SZVAVModel.FanPartLoadRatio = ( ( AirMassFlow - ( maxAirMassFlow * lowSpeedFanRatio ) ) / ( ( 1.0 - lowSpeedFanRatio ) * maxAirMassFlow ) );

					// can the load be met and SAT limit exceeded at this air flow rate?
					PartLoadRatio = 1.0;
					if ( CoolingLoad ) { // Function CalcUnitarySystemToLoad, 4th and 5th arguments are CoolPLR and HeatPLR
										 // set the water flow ratio so water coil gets proper flow
						SZVAVModel.CoolCoilWaterFlowRatio = maxCoilFluidFlow / SZVAVModel.MaxCoolCoilFluidFlow;
//						CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, PartLoadRatio, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
					} else {
						SZVAVModel.HeatCoilWaterFlowRatio = maxCoilFluidFlow / SZVAVModel.MaxHeatCoilFluidFlow;
//						CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, PartLoadRatio, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
					}
					CalcPTUnit( SysIndex, FirstHVACIteration, PartLoadRatio, TempSensOutput, ZoneLoad, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
					outletTemp = DataLoopNode::Node( OutletNode ).Temp;
					if ( ( CoolingLoad && outletTemp < maxOutletTemp ) || ( HeatingLoad && outletTemp > maxOutletTemp ) ) {

						outletTemp = maxOutletTemp;
						AirMassFlow = min( maxAirMassFlow, ( ZoneLoad / ( PsyHFnTdbW( outletTemp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) ) ) );
						AirMassFlow = max( minAirMassFlow, AirMassFlow );
						SZVAVModel.FanPartLoadRatio = ( ( AirMassFlow - ( maxAirMassFlow * lowSpeedFanRatio ) ) / ( ( 1.0 - lowSpeedFanRatio ) * maxAirMassFlow ) );

						Par( 9 ) = lowWaterMdot; // minCoilFluidFlow - low fan speed water flow rate > 0 if SAT limited
						Par( 10 ) = maxCoilFluidFlow; // max water flow rate
						Par( 12 ) = AirMassFlow; // sets air flow rate used when iterating on coil capacity
						Par( 13 ) = 0.0; // other than 0 means to iterate on SA temperature

						General::SolveRoot( 0.001, MaxIter, SolFlag, PartLoadRatio, CalcPTUnitWaterFlowResidual, 0.0, 1.0, Par );

					} else {

						// reset the AirMassFlow rate using the max air outlet temp calculated above (should yield lower air mass flow rate and outlet air temp as close to limit as possible)
						AirMassFlow = min( maxAirMassFlow, ( ZoneLoad / ( PsyHFnTdbW( outletTemp, MinHumRatio ) - PsyHFnTdbW( ZoneTemp, MinHumRatio ) ) ) );
						AirMassFlow = max( minAirMassFlow, AirMassFlow );
						SZVAVModel.FanPartLoadRatio = ( ( AirMassFlow - ( maxAirMassFlow * lowSpeedFanRatio ) ) / ( ( 1.0 - lowSpeedFanRatio ) * maxAirMassFlow ) );

						Par( 12 ) = AirMassFlow;  // sets upper limit on air flow rate
						Par( 13 ) = 0.0; // other than 0 means to iterate on SA temperature
						Par( 16 ) = 1.0; // iterate on coil capacity

						General::SolveRoot( 0.001, MaxIter, SolFlag, PartLoadRatio, CalcPTUnitWaterFlowResidual, 0.0, 1.0, Par );

					}

					outletTemp = DataLoopNode::Node( OutletNode ).Temp;

					if ( SolFlag < 0 ) {
						if ( SolFlag == -1 ) {
							// get capacity for warning
//							if ( CoolingLoad ) { // 4th and 5th arguments are CoolPLR and HeatPLR
//								CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, PartLoadRatio, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
//							} else {
//								CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, PartLoadRatio, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
//							}
							CalcPTUnit( SysIndex, FirstHVACIteration, PartLoadRatio, TempSensOutput, ZoneLoad, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );
							if ( abs( DataLoopNode::Node( OutletNode ).Temp - maxOutletTemp ) > 0.1 ) { // oulet temperature can sometimes fluctuate around the target by 0.02+ even when PLR changes by 1E-12
								if ( SZVAVModel.MaxIterIndex == 0 ) {
									ShowWarningMessage( "Step 8: Coil control failed to converge for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
									ShowContinueError( "  Iteration limit exceeded in calculating system supply air outlet temperature." );
									ShowContinueErrorTimeStamp( "Supply air temperature target = " + General::TrimSigDigits( maxOutletTemp, 3 ) + " (C), supply air temperature = " + General::TrimSigDigits( DataLoopNode::Node( OutletNode ).Temp, 3 ) + " (C), and the simulation continues." );
								}
								ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - Iteration limit exceeded in calculating supply air temperature continues. Temperature statistics:", SZVAVModel.MaxIterIndex, DataLoopNode::Node( OutletNode ).Temp, DataLoopNode::Node( OutletNode ).Temp );
							}
						} else if ( SolFlag == -2 ) { // should not get here
							if ( SZVAVModel.RegulaFalsIFailedIndex == 0 ) {
								ShowWarningMessage( "Step 8: Coil control failed for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
								ShowContinueError( "  supply air temperature target determined to be outside the range." );
								ShowContinueErrorTimeStamp( "Supply air temperature = " + General::TrimSigDigits( maxOutletTemp, 3 ) + " (C), and the simulation continues." );
							}
							ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - supply air temperature outside of range error continues. Temperature statistics:", SZVAVModel.RegulaFalsIFailedIndex, DataLoopNode::Node( OutletNode ).Temp, DataLoopNode::Node( OutletNode ).Temp );
						}
					}

				} else { // Step 9: not enough cooling or heating

					if ( CoolingLoad && FullSensibleOutput < ZoneLoad || HeatingLoad && FullSensibleOutput > ZoneLoad ) {

						// Region 3: air flow rate at max while modulating coil capacity to meet load
						Par( 9 ) = lowWaterMdot; // max water flow at low speed fan
						Par( 10 ) = maxCoilFluidFlow; // max water flow rate limited by SAT or max if not limited
						Par( 12 ) = maxAirMassFlow;
						Par( 13 ) = 0.0;
						General::SolveRoot( 0.001, MaxIter, SolFlag, PartLoadRatio, CalcPTUnitWaterFlowResidual, 0.0, 1.0, Par );

						if ( SolFlag < 0 ) {
							if ( SolFlag == -1 ) {
								// get capacity for warning
								//							if ( CoolingLoad ) { // 4th and 5th arguments are CoolPLR and HeatPLR
								//								CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, PartLoadRatio, 0.0, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
								//							} else {
								//								CalcUnitarySystemToLoad( SysIndex, AirLoopNum, FirstHVACIteration, 0.0, PartLoadRatio, OnOffAirFlowRatio, TempSensOutput, TempLatOutput, HXUnitOn, _, _, CompressorONFlag );
								//							}
								CalcPTUnit( SysIndex, FirstHVACIteration, PartLoadRatio, TempSensOutput, ZoneLoad, OnOffAirFlowRatio, SupHeaterLoad, HXUnitOn );

								if ( abs( TempSensOutput - ZoneLoad ) * SZVAVModel.ControlZoneMassFlowFrac > 15.0 ) { // water coil can provide same output at varying water PLR (model discontinuity?)
									if ( SZVAVModel.MaxIterIndex == 0 ) {
										ShowWarningMessage( "Step 9: Coil control failed to converge for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
										ShowContinueError( "  Iteration limit exceeded in calculating system sensible part-load ratio." );
										ShowContinueErrorTimeStamp( "Sensible load to be met = " + General::TrimSigDigits( ZoneLoad, 2 ) + " (watts), sensible output = " + General::TrimSigDigits( TempSensOutput, 2 ) + " (watts), and the simulation continues." );
									}
									ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - Iteration limit exceeded in calculating sensible part-load ratio error continues. Sensible load statistics:", SZVAVModel.MaxIterIndex, ZoneLoad, ZoneLoad );
								}
							} else if ( SolFlag == -2 ) {
								if ( SZVAVModel.RegulaFalsIFailedIndex == 0 ) {
									ShowWarningMessage( "Step 9: Coil control failed for " + SZVAVModel.UnitType + ':' + SZVAVModel.Name );
									ShowContinueError( "  sensible part-load ratio determined to be outside the range of 0-1." );
									ShowContinueErrorTimeStamp( "Sensible load to be met = " + General::TrimSigDigits( ZoneLoad, 2 ) + " (watts), and the simulation continues." );
								}
								ShowRecurringWarningErrorAtEnd( SZVAVModel.UnitType + " \"" + SZVAVModel.Name + "\" - sensible part-load ratio out of range error continues. Sensible load statistics:", SZVAVModel.RegulaFalsIFailedIndex, ZoneLoad, ZoneLoad );
							}
						}

					}

				}

			} else { // coil can be schedule off so set fan at high speed at these times.

				PartLoadRatio = 1.0;
				SZVAVModel.FanPartLoadRatio = 1.0;
				SZVAVModel.CoolCoilWaterFlowRatio = 0.0;
				SZVAVModel.HeatCoilWaterFlowRatio = 0.0;
				if ( coilLoopNum > 0 ) DataLoopNode::Node( coilFluidInletNode ).MassFlowRate = maxCoilFluidFlow;

			}

		}

		if ( coilLoopNum > 0 ) SetComponentFlowRate( DataLoopNode::Node( coilFluidInletNode ).MassFlowRate, coilFluidInletNode, coilFluidOutletNode, coilLoopNum, coilLoopSide, coilBranchNum, coilCompNum );


	}

} // General

} // EnergyPlus
