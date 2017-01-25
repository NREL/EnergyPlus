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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <IceThermalStorage.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace IceThermalStorage {

	// MODULE INFORMATION:
	//       AUTHOR         Pyeongchan Ihm
	//       DATE WRITTEN   April 2002
	//       MODIFIED       Modified Refined model, added Simple model, by Guo Zhou, Oct 2002
	//                      Remove chiller, make just a storage tank, Michael J. Witte, Sep 2005
	//                      Added detailed ice storage model, Rick Strand, Feb 2006
	//                      B. Griffith, Sept 2010, plant upgrades, fluid properties
	//                      Enhancements to detailed ice storage model, Rick Strand, Aug 2012
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module simulates the performance of Ice Thermal Storage

	// METHODOLOGY EMPLOYED:
	// Once the PlantLoopManager determines that the Ice Thermal Storage
	// is available to meet a loop cooling demand, it calls SimIceStorage
	// which in turn calls the appropriate Ice Thermal Storage model.

	// REFERENCES: Dion J. King, ASHRAE Transactions v104, pt1, 1998.

	// OTHER NOTES: na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace CurveManager;
	using namespace DataLoopNode;
	using DataGlobals::WarmupFlag;
	using DataGlobals::BeginDayFlag;
	using DataGlobals::EndDayFlag;
	using DataGlobals::TimeStepZone;
	using DataGlobals::SecInHour;
	using DataGlobals::HourOfDay;
	using DataGlobals::TimeStep;
	using DataGlobals::ScheduleAlwaysOn;
	using namespace DataHVACGlobals;
	using General::TrimSigDigits;

	// Data
	// MODULE PARAMETER DEFINITIONS
	static std::string const BlankString;

	std::string const cIceStorageSimple( "ThermalStorage:Ice:Simple" );
	std::string const cIceStorageDetailed( "ThermalStorage:Ice:Detailed" );

	int const IceStorageType_Simple( 1 );
	int const IceStorageType_Detailed( 2 );

	int const CurveQuadraticLinear( 1 );
	int const CurveCubicLinear( 2 );

	int const DetIceInsideMelt( 1 ); // Inside melt system--charge starting with bare coil
	int const DetIceOutsideMelt( 2 ); // Outside melt system--charge from existing ice layer on coil

	// ITS parameter
	Real64 const FreezTemp( 0.0 ); // Water freezing Temperature, 0[C]
	Real64 const FreezTempIP( 32.0 ); // Water freezing Temperature, 32[F]
	Real64 const TimeInterval( 3600.0 ); // Time Interval (1 hr) [s]
	int const ITSType_IceOnCoilInternal( 1 );
	int const ITSType_IceOnCoilExternal( 2 );
	// Conversion parameter
	Real64 const EpsLimitForX( 0.0 ); // 0.02  ! See Dion's code as eps1
	Real64 const EpsLimitForDisCharge( 0.0 ); // 0.20  ! See Dion's code as eps2
	Real64 const EpsLimitForCharge( 0.0 ); // 0.20  ! See Dion's code as eps3

	//variable used by simple model
	Real64 const Delta( 0.005 );
	Real64 const PLRmin( 0.1 );
	Real64 const Pa( 0.088065 );
	Real64 const Pb( 1.137742 );
	Real64 const Pc( -0.225806 );
	Real64 const Tref( 85.0 ); // F
	Real64 const Tcharge( 1.0 ); // F
	Real64 const Tdischarge( 5.0 ); // F

	// Parameter used by the Detailed Ice Storage Model
	Real64 const DeltaTofMin( 0.5 ); // Minimum allowed outlet side temperature difference [C]
	// This is (Tout - Tfreezing)
	Real64 const DeltaTifMin( 1.0 ); // Minimum allowed inlet side temperature difference [C]
	// This is (Tin - Tfreezing)

	// DERIVED TYPE DEFINITIONS
	// TYPE ITSSetCap is used for information of ITS plant in Loop, Brach, and Components.
	//  TYPE ITSSetCapData
	//    LOGICAL :: ITSFlag    = .FALSE.
	//    INTEGER :: LoopNum    =0
	//    INTEGER :: BranchNum  =0
	//    INTEGER :: CompNum    =0
	//  END TYPE ITSSetCapData

	// TYPE (ITSSetCapData), SAVE                   :: ITSSetCap=ITSSetCapData(.FALSE.,0,0,0)

	// MODULE VARIABLE DECLARATIONS:
	bool ResetXForITSFlag( false );

	// Input data
	Real64 ITSNomCap( 0.0 ); // Design nominal capacity of Ice Thermal Storage [J] (user input in GJ)
	int InletNodeNum( 0 ); // Node number on the inlet side of the plant
	int OutletNodeNum( 0 ); // Node number on the inlet side of the plant

	// ITS numbers and FoundOrNot
	int IceNum( 0 );
	int NumIceStorages( 0 );
	bool IceStorageNotFound( false );
	int NumDetIceStorages( 0 );
	int TotalIceStorages( 0 );
	// ITS UAice and HLoss
	Real64 UAIceCh( 0.0 ); // Charging Ice Thermal Storage overall heat transfer coefficient [W/C]
	Real64 UAIceDisCh( 0.0 ); // Discharging Ice Thermal Storage overall heat transfer coefficient [W/C]
	Real64 HLoss( 0.0 ); // ITS Heat Loss
	// ITS State
	Real64 XCurIceFrac( 0.0 ); // Current Fraction of Ice Thermal Storage remaining [fraction]
	Real64 U( 0.0 ); // Adjusted input U after reading U Schedule [fraction]
	Real64 Urate( 0.0 ); // Final Urate adjusted Urate based on Error protection (I) [fraction] by HOUR
	// ITS status information
	Real64 ITSMassFlowRate( 0.0 ); // ITS water mass flow rate [kg/s]
	Real64 ITSInletTemp( 0.0 ); // ITS inlet water temperature [C]
	Real64 ITSOutletTemp( 0.0 ); // ITS outlet water temperature [C]
	Real64 ITSOutletSetPointTemp( 0.0 ); // ITS outlet water temperature setpoint [C]
	Real64 ITSCoolingRate( 0.0 ); // ITS Discharge(-)/Charge(+) rate [W]
	Real64 ITSCoolingEnergy( 0.0 );
	Real64 ChillerOutletTemp( 0.0 ); // Chiller outlet brine temperature [C]
	Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE
	// General routine

	// Object Data
	Array1D< IceStorageSpecs > IceStorage; // dimension to number of machines
	Array1D< ReportVars > IceStorageReport; // dimension to number of machines
	Array1D< DetailedIceStorageData > DetIceStor; // Derived type for detailed ice storage model
	Array1D< IceStorageMapping > IceStorageTypeMap;

	//*************************************************************************

	// Functions

	void
	SimIceStorage(
		std::string const & IceStorageType,
		std::string const & IceStorageName,
		int & CompIndex,
		bool const RunFlag,
		bool const FirstIteration,
		bool const InitLoopEquip,
		Real64 & MyLoad
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataGlobals::BeginEnvrnFlag;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		Real64 DemandMdot;
		Real64 TempIn;
		Real64 TempSetPt( 0.0 );
		Real64 MyLoad2;
		Real64 MaxCap;
		Real64 MinCap;
		Real64 OptCap;
		Real64 Cp; // local plant fluid specific heat

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SimIceStorage" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool firstTime( true );
		static bool MyEnvrnFlag( true );
		int IceStorageNum;

		// FLOW

		//  Set initialization flags
		//  Allow ice to build up during warmup?
		//  IF( (BeginEnvrnFlag) .OR. (WarmupFlag) ) THEN
		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			ResetXForITSFlag = true;
			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag = true;
		}

		if ( firstTime ) {
			GetIceStorageInput();
			firstTime = false;
		}

		// Find the correct Equipment
		if ( CompIndex == 0 ) {
			IceStorageNum = FindItemInList( IceStorageName, IceStorageTypeMap, TotalIceStorages );
			if ( IceStorageNum == 0 ) {
				ShowFatalError( "SimIceStorage: Unit not found=" + IceStorageName );
			}
			CompIndex = IceStorageNum;
		} else {
			IceStorageNum = CompIndex;
			if ( IceStorageNum > TotalIceStorages || IceStorageNum < 1 ) {
				ShowFatalError( "SimIceStorage:  Invalid CompIndex passed=" + TrimSigDigits( IceStorageNum ) + ", Number of Units=" + TrimSigDigits( TotalIceStorages ) + ", Entered Unit name=" + IceStorageName );
			}
			if ( CheckEquipName( IceStorageNum ) ) {
				if ( IceStorageName != IceStorageTypeMap( IceStorageNum ).Name ) {
					ShowFatalError( "SimIceStorage: Invalid CompIndex passed=" + TrimSigDigits( IceStorageNum ) + ", Unit name=" + IceStorageName + ", stored Unit Name for that index=" + IceStorageTypeMap( IceStorageNum ).Name );
				}
				CheckEquipName( IceStorageNum ) = false;
			}
		}

		{ auto const SELECT_CASE_var( IceStorageTypeMap( IceStorageNum ).StorageType_Num );

		if ( SELECT_CASE_var == IceStorageType_Simple ) {

			//------------------------------------------------------------------------
			// READING INPUT when first calling SimIceStorage
			//------------------------------------------------------------------------
			IceNum = IceStorageTypeMap( IceStorageNum ).LocalEqNum;

			InitSimpleIceStorage();

			if ( InitLoopEquip ) {

				// Find IceStorage Number
				// Assign ice thermal storage data to Module variables by each Ice thermal storage
				ITSNomCap = IceStorage( IceNum ).ITSNomCap;
				InletNodeNum = IceStorage( IceNum ).PltInletNodeNum;
				OutletNodeNum = IceStorage( IceNum ).PltOutletNodeNum;

				return;
			} // End Of InitLoopEquip

			//------------------------------------------------------------------------
			// FIRST PROCESS (MyLoad = 0.0 as IN)
			// At this moment as first calling of ITS, ITS provide ONLY MaxCap/OptCap/MinCap.
			//------------------------------------------------------------------------
			// First process is in subroutine CalcIceStorageCapacity(MaxCap,MinCap,OptCap) shown bellow.

			//------------------------------------------------------------------------
			// SECOND PROCESS (MyLoad is provided by E+ based on MaxCap/OptCap/MinCap)
			//------------------------------------------------------------------------
			// Below routines are starting when second calling.
			// After previous return, MyLoad is calculated based on MaxCap, OptCap, and MinCap.
			// Then PlandSupplySideManager provides MyLoad to simulate Ice Thermal Storage.
			// The process will be decided based on sign(+,-,0) of input U.

			// MJW 19 Sep 2005 - New approach - calculate MyLoad locally from inlet node temp
			//                   and outlet node setpoint until MyLoad that is passed in behaves well

			//DSU? can we now use MyLoad? lets not yet to try to avoid scope creep

			TempIn = Node( InletNodeNum ).Temp;
			{ auto const SELECT_CASE_var1( PlantLoop( IceStorage( IceNum ).LoopNum ).LoopDemandCalcScheme );
			if ( SELECT_CASE_var1 == SingleSetPoint ) {
				TempSetPt = Node( OutletNodeNum ).TempSetPoint;
			} else if ( SELECT_CASE_var1 == DualSetPointDeadBand ) {
				TempSetPt = Node( OutletNodeNum ).TempSetPointHi;
			} else {
				assert( false );
			}}
			DemandMdot = IceStorage( IceNum ).DesignMassFlowRate;

			Cp = GetSpecificHeatGlycol( PlantLoop( IceStorage( IceNum ).LoopNum ).FluidName, TempIn, PlantLoop( IceStorage( IceNum ).LoopNum ).FluidIndex, RoutineName );

			MyLoad2 = ( DemandMdot * Cp * ( TempIn - TempSetPt ) );
			MyLoad = MyLoad2;

			//     Set fraction of ice remaining in storage
			XCurIceFrac = IceStorageReport( IceNum ).IceFracRemain;

			//***** Dormant Process for ITS *****************************************
			//************************************************************************
			//        IF( U .EQ. 0.0 ) THEN
			if ( ( MyLoad2 == 0.0 ) || ( DemandMdot == 0.0 ) ) {
				CalcIceStorageDormant( IceStorageType_Simple, IceNum );

				//***** Charging Process for ITS *****************************************
				//************************************************************************
				//        ELSE IF( U .GT. 0.0 ) THEN
			} else if ( MyLoad2 < 0.0 ) {

				//             Call CalcIceStorageCapacity from here - MJW - 19 Sep 2005
				CalcIceStorageCapacity( IceStorageType_Simple, MaxCap, MinCap, OptCap );

				CalcIceStorageCharge( IceStorageType_Simple, IceNum );

				//***** Discharging Process for ITS *****************************************
				//************************************************************************
				//        ELSE IF( U .LT. 0.0 ) THEN
			} else if ( MyLoad2 > 0.0 ) {
				//             Call CalcIceStorageCapacity from here - MJW - 19 Sep 2005
				CalcIceStorageCapacity( IceStorageType_Simple, MaxCap, MinCap, OptCap );

				CalcIceStorageDischarge( IceStorageType_Simple, IceNum, MyLoad, RunFlag, FirstIteration, MaxCap );
			} // Based on input of U value, deciding Dormant/Charge/Discharge process

			// Update Node properties: mdot and Temperature
			UpdateNode( MyLoad2, RunFlag, IceNum );

			// Update report variables.
			RecordOutput( IceNum, MyLoad2, RunFlag );
			//--------------------------------------------------------------------------
			//        Ali's TES modle   Itegrated by ZG  Oct. 2002
			//---------------------------------------------------------------------------

		} else if ( SELECT_CASE_var == IceStorageType_Detailed ) {

			IceNum = IceStorageTypeMap( IceStorageNum ).LocalEqNum;
			// Read input when first calling SimIceStorage
			if ( InitLoopEquip ) {
				return;
			} // End Of InitLoopEquip

			InitDetailedIceStorage(); // Initialize detailed ice storage

			SimDetailedIceStorage(); // Simulate detailed ice storage

			UpdateDetailedIceStorage(); // Update detailed ice storage

			ReportDetailedIceStorage(); // Report detailed ice storage

		} else {
			ShowFatalError( "Specified IceStorage not found in SimIceStorage" + IceStorageType );
		}}

	}

	void
	SimDetailedIceStorage()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main simulation subroutine for the detailed
		// ice storage model.

		// METHODOLOGY EMPLOYED:
		// Based on whether the unit is dormant, in charging mode, or in discharging
		// mode, the code either passes the flow through the bypass, through the tank,
		// or both.  This depends on the temperature relative to the setpoint temperature
		// and other features of the model.  The model itself is a LMTD model that uses
		// performance curve fits that are quadratic in fraction charged/discharged and
		// linear in LMTD for the calculation of Q.  The equations are actually non-
		// dimensionalized.

		// REFERENCES:
		// Ice Storage Component Model Proposal (Revised).doc by Rick Strand (Dec 2005/Jan 2006)

		// Using/Aliasing
		using CurveManager::CurveValue;
		using ScheduleManager::GetCurrentScheduleValue;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using DataPlant::CommonPipe_TwoWay;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using PlantUtilities::SetComponentFlowRate;
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataGlobals::WarmupFlag;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIterNum( 100 ); // Maximum number of internal iterations for ice storage solution
		Real64 const SmallestLoad( 0.1 ); // Smallest load to actually run the ice storage unit [Watts]
		Real64 const TankDischargeToler( 0.001 ); // Below this fraction, there is nothing left to discharge
		Real64 const TankChargeToler( 0.999 ); // Above this fraction, we don't have anything left to charge
		Real64 const TemperatureToler( 0.1 ); // Temperature difference between iterations that indicates convergence [C]
		Real64 const SIEquiv100GPMinMassFlowRate( 6.31 ); // Used to non-dimensionalize flow rate for use in CubicLinear charging equation
														// Flow rate divided by nominal 100GPM used to non-dimensionalize volume flow rate
														// Assumes approximate density of 1000 kg/m3 to get an estimate for mass flow rate
		static std::string const RoutineName( "SimDetailedIceStorage" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ActualLoad; // Actual load on the ice storage unit [W]
		Real64 AvgFracCharged; // Average fraction charged for the current time step
		Real64 ChargeFrac; // Fraction of tank to be charged in the current time step
		int IterNum; // Iteration number
		Real64 LMTDstar; // Non-dimensional log mean temperature difference of ice storage unit [non-dimensional]
		Real64 LocalLoad; // Estimated load on the ice storage unit [W]
		int NodeNumIn; // Plant loop inlet node number for component
		int NodeNumOut; // Plant loop outlet node number for component
		Real64 Qstar; // Current load on the ice storage unit [non-dimensional]
		Real64 TempIn; // Inlet temperature to component (from plant loop) [C]
		Real64 TempSetPt( 0.0 ); // Setpoint temperature defined by loop controls [C]
		Real64 ToutNew; // Updated outlet temperature from the tank [C]
		Real64 ToutOld; // Tank outlet temperature from the last iteration [C]
		Real64 Cp; // local plant fluid specific heat
		Real64 mdot; // local mass flow rate for plant connection
		Real64 MassFlowstar; //non-dimensional mass flow rate for charge curve use [non-dimensional]

		// FLOW:
		// Set local variables
		NodeNumIn = DetIceStor( IceNum ).PlantInNodeNum;
		NodeNumOut = DetIceStor( IceNum ).PlantOutNodeNum;
		TempIn = Node( NodeNumIn ).Temp;
		{ auto const SELECT_CASE_var( PlantLoop( DetIceStor( IceNum ).PlantLoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			TempSetPt = Node( NodeNumOut ).TempSetPoint;
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			TempSetPt = Node( NodeNumOut ).TempSetPointHi;
		} else {
			assert( false );
		}}

		IterNum = 0;

		// Set derived type variables
		DetIceStor( IceNum ).InletTemp = TempIn;
		DetIceStor( IceNum ).MassFlowRate = Node( NodeNumIn ).MassFlowRate;

		//if two-way common pipe and no mass flow and tank is not full, then use design flow rate
		if ( ( PlantLoop( DetIceStor( IceNum ).PlantLoopNum ).CommonPipeType == CommonPipe_TwoWay ) && ( std::abs( DetIceStor( IceNum ).MassFlowRate ) < MassFlowTolerance ) && ( DetIceStor( IceNum ).IceFracRemaining < TankChargeToler ) ) {
			DetIceStor( IceNum ).MassFlowRate = DetIceStor( IceNum ).DesignMassFlowRate;
		}

		// Calculate the current load on the ice storage unit
		Cp = GetSpecificHeatGlycol( PlantLoop( DetIceStor( IceNum ).PlantLoopNum ).FluidName, TempIn, PlantLoop( DetIceStor( IceNum ).PlantLoopNum ).FluidIndex, RoutineName );

		LocalLoad = DetIceStor( IceNum ).MassFlowRate * Cp * ( TempIn - TempSetPt );

		// Determine what the status is regarding the ice storage unit and the loop level flow
		if ( ( std::abs( LocalLoad ) <= SmallestLoad ) || ( GetCurrentScheduleValue( DetIceStor( IceNum ).ScheduleIndex ) <= 0 ) ) {
			// No real load on the ice storage device or ice storage OFF--bypass all of the flow and leave the tank alone
			DetIceStor( IceNum ).CompLoad = 0.0;
			DetIceStor( IceNum ).OutletTemp = TempIn;
			DetIceStor( IceNum ).TankOutletTemp = TempIn;
			mdot = 0.0;
			SetComponentFlowRate( mdot, DetIceStor( IceNum ).PlantInNodeNum, DetIceStor( IceNum ).PlantOutNodeNum, DetIceStor( IceNum ).PlantLoopNum, DetIceStor( IceNum ).PlantLoopSideNum, DetIceStor( IceNum ).PlantBranchNum, DetIceStor( IceNum ).PlantCompNum );

			DetIceStor( IceNum ).BypassMassFlowRate = mdot;
			DetIceStor( IceNum ).TankMassFlowRate = 0.0;
			DetIceStor( IceNum ).MassFlowRate = mdot;

		} else if ( LocalLoad < 0.0 ) {
			// The load is less than zero so we should be charging
			// Before we do anything, we should check to make sure that we will actually be charging the unit

			if ( ( TempIn > ( DetIceStor( IceNum ).FreezingTemp - DeltaTifMin ) ) || ( DetIceStor( IceNum ).IceFracRemaining >= TankChargeToler ) ) {
				// If the inlet temperature is not below the freezing temperature of the
				// device, then we cannot actually do any charging.  Bypass all of the flow.
				// Also, if the tank is already sufficiently charged, we don't need to
				// do any further charging.  So, bypass all of the flow.
				DetIceStor( IceNum ).CompLoad = 0.0;
				DetIceStor( IceNum ).OutletTemp = TempIn;
				DetIceStor( IceNum ).TankOutletTemp = TempIn;
				mdot = 0.0;
				SetComponentFlowRate( mdot, DetIceStor( IceNum ).PlantInNodeNum, DetIceStor( IceNum ).PlantOutNodeNum, DetIceStor( IceNum ).PlantLoopNum, DetIceStor( IceNum ).PlantLoopSideNum, DetIceStor( IceNum ).PlantBranchNum, DetIceStor( IceNum ).PlantCompNum );

				DetIceStor( IceNum ).BypassMassFlowRate = mdot;
				DetIceStor( IceNum ).TankMassFlowRate = 0.0;
				DetIceStor( IceNum ).MassFlowRate = mdot;

			} else {
				//make flow request so tank will get flow
				mdot = DetIceStor( IceNum ).DesignMassFlowRate;
				SetComponentFlowRate( mdot, DetIceStor( IceNum ).PlantInNodeNum, DetIceStor( IceNum ).PlantOutNodeNum, DetIceStor( IceNum ).PlantLoopNum, DetIceStor( IceNum ).PlantLoopSideNum, DetIceStor( IceNum ).PlantBranchNum, DetIceStor( IceNum ).PlantCompNum );

				// We are in charging mode, the temperatures are low enough to charge
				// the tank, and we have some charging left to do.
				// Make first guess at Qstar based on the current ice fraction remaining
				// and LMTDstar that is based on the freezing or TempSetPt temperature.
				if ( TempSetPt > ( DetIceStor( IceNum ).FreezingTemp - DeltaTofMin ) ) {
					// Outlet temperature cannot be above the freezing temperature so set
					// the outlet temperature to the freezing temperature and calculate
					// LMTDstar based on that assumption.
					TempSetPt = DetIceStor( IceNum ).FreezingTemp - DeltaTofMin;
				}

				ToutOld = TempSetPt;
				LMTDstar = CalcDetIceStorLMTDstar( TempIn, ToutOld, DetIceStor( IceNum ).FreezingTemp );

				// Find initial guess at average fraction charged during time step
				ChargeFrac = LocalLoad * TimeStepSys / DetIceStor( IceNum ).NomCapacity;
				if ( ( DetIceStor( IceNum ).IceFracRemaining + ChargeFrac ) > 1.0 ) {
					ChargeFrac = 1.0 - DetIceStor( IceNum ).IceFracRemaining;
				}
				if ( DetIceStor( IceNum ).ThawProcessIndex == DetIceInsideMelt ) {
					AvgFracCharged = DetIceStor( IceNum ).IceFracOnCoil + ( ChargeFrac / 2.0 );
				} else { // (DetIceStor(IceNum)%ThawProcessIndex == DetIceOutsideMelt)
					AvgFracCharged = DetIceStor( IceNum ).IceFracRemaining + ( ChargeFrac / 2.0 );
				}

				if ( DetIceStor( IceNum ).ChargeCurveTypeNum == CurveQuadraticLinear ) {
					Qstar = std::abs( CurveValue( DetIceStor( IceNum ).ChargeCurveNum, AvgFracCharged, LMTDstar ) );
				} else  { // ( DetIceStor( IceNum ).ChargeCurveTypeNum == CurveCubicLinear )
					MassFlowstar = DetIceStor( IceNum ).MassFlowRate/SIEquiv100GPMinMassFlowRate;
					Qstar = std::abs( CurveValue( DetIceStor( IceNum ).ChargeCurveNum, LMTDstar, MassFlowstar ) );
				}

				ActualLoad = Qstar * DetIceStor( IceNum ).NomCapacity / DetIceStor( IceNum ).CurveFitTimeStep;

				ToutNew = TempIn + ( ActualLoad / ( DetIceStor( IceNum ).MassFlowRate * Cp ) );
				// Again, the outlet temperature cannot be above the freezing temperature (factoring in the tolerance)
				if ( ToutNew > ( DetIceStor( IceNum ).FreezingTemp - DeltaTofMin ) ) ToutNew = DetIceStor( IceNum ).FreezingTemp - DeltaTofMin;

				if ( ActualLoad > std::abs( LocalLoad ) ) {
					// We have more than enough capacity to meet the load so no need to iterate to find a solution
					DetIceStor( IceNum ).OutletTemp = TempSetPt;
					DetIceStor( IceNum ).TankOutletTemp = ToutNew;
					DetIceStor( IceNum ).CompLoad = DetIceStor( IceNum ).MassFlowRate * Cp * std::abs( TempIn - TempSetPt );
					DetIceStor( IceNum ).TankMassFlowRate = DetIceStor( IceNum ).CompLoad / Cp / std::abs( TempIn - ToutNew );
					DetIceStor( IceNum ).BypassMassFlowRate = DetIceStor( IceNum ).MassFlowRate - DetIceStor( IceNum ).TankMassFlowRate;

				} else {

					while ( IterNum < MaxIterNum ) {
						if ( std::abs( ToutOld - ToutNew ) > TemperatureToler ) {
							// Not converged yet so recalculated what is needed and keep iterating
							// Calculate new values for LMTDstar and Qstar based on updated outlet temperature
							ToutOld = ToutNew;
							LMTDstar = CalcDetIceStorLMTDstar( TempIn, ToutOld, DetIceStor( IceNum ).FreezingTemp );
							if ( DetIceStor( IceNum ).ChargeCurveTypeNum == CurveQuadraticLinear ) {
								Qstar = std::abs( CurveValue( DetIceStor( IceNum ).ChargeCurveNum, AvgFracCharged, LMTDstar ) );
							} else  { // ( DetIceStor( IceNum ).ChargeCurveTypeNum == CurveCubicLinear )
								MassFlowstar = DetIceStor( IceNum ).MassFlowRate/SIEquiv100GPMinMassFlowRate;
								Qstar = std::abs( CurveValue( DetIceStor( IceNum ).ChargeCurveNum, LMTDstar, MassFlowstar ) );
							}

							// Now make sure that we don't go above 100% charged and calculate the new average fraction
							ChargeFrac = Qstar * ( TimeStepSys / DetIceStor( IceNum ).CurveFitTimeStep );
							if ( ( DetIceStor( IceNum ).IceFracRemaining + ChargeFrac ) > 1.0 ) {
								ChargeFrac = 1.0 - DetIceStor( IceNum ).IceFracRemaining;
								Qstar = ChargeFrac;
							}
							if ( DetIceStor( IceNum ).ThawProcessIndex == DetIceInsideMelt ) {
								AvgFracCharged = DetIceStor( IceNum ).IceFracOnCoil + ( ChargeFrac / 2.0 );
							} else { // (DetIceStor(IceNum)%ThawProcessIndex == DetIceOutsideMelt)
								AvgFracCharged = DetIceStor( IceNum ).IceFracRemaining + ( ChargeFrac / 2.0 );
							}

							// Finally, update the actual load and calculate the new outlet temperature; increment iteration counter
							ActualLoad = Qstar * DetIceStor( IceNum ).NomCapacity / DetIceStor( IceNum ).CurveFitTimeStep;
							ToutNew = TempIn + ( ActualLoad / ( DetIceStor( IceNum ).MassFlowRate * Cp ) );
							// Again, the outlet temperature cannot be above the freezing temperature (factoring in the tolerance)
							if ( ToutNew < ( DetIceStor( IceNum ).FreezingTemp - DeltaTofMin ) ) ToutNew = DetIceStor( IceNum ).FreezingTemp - DeltaTofMin;
							++IterNum;

						} else {
							// Converged to acceptable tolerance so set output variables and exit DO WHILE loop
							break;

						}

					} // ...loop iterating for the ice storage outlet temperature

					// Keep track of times that the iterations got excessive and report if necessary
					if ( IterNum >= MaxIterNum ) {
						++DetIceStor( IceNum ).ChargeIterErrors;
						if ( DetIceStor( IceNum ).ChargeIterErrors <= 25 ) {
							ShowWarningError( "Detailed Ice Storage model exceeded its internal charging maximum iteration limit" );
							ShowContinueError( "Detailed Ice Storage System Name = " + DetIceStor( IceNum ).Name );
							ShowContinueErrorTimeStamp( "" );
						} else {
							ShowRecurringWarningErrorAtEnd( "Detailed Ice Storage system [" + DetIceStor( IceNum ).Name + "]  charging maximum iteration limit exceeded occurrence continues.", DetIceStor( IceNum ).ChargeErrorCount );
						}
					}

					// Set the values for the key outlet parameters
					// Note that in REAL(r64)ity the tank will probably bypass some flow when it
					// gets close to full charge.  This is a simplification that assumes
					// all flow through the tank during charging and a lower delta T near
					// the full charge level.  From an energy perspective, this is a reasonable
					// approximation.
					DetIceStor( IceNum ).OutletTemp = ToutNew;
					DetIceStor( IceNum ).TankOutletTemp = ToutNew;
					DetIceStor( IceNum ).BypassMassFlowRate = 0.0;
					DetIceStor( IceNum ).TankMassFlowRate = DetIceStor( IceNum ).MassFlowRate;
					DetIceStor( IceNum ).CompLoad = DetIceStor( IceNum ).MassFlowRate * Cp * std::abs( TempIn - ToutNew );

				}

			}

		} else if ( LocalLoad > 0.0 ) {
			// The load is greater than zero so we should be discharging
			// Before we do anything, we should check to make sure that we will actually be discharging the unit

			if ( ( DetIceStor( IceNum ).InletTemp < ( DetIceStor( IceNum ).FreezingTemp + DeltaTifMin ) ) || ( DetIceStor( IceNum ).IceFracRemaining <= TankDischargeToler ) ) {
				// If the inlet temperature is below the freezing temperature of the
				// device, then we cannot actually do any discharging.  Bypass all of the flow.
				// Also, if the tank is already discharged, we can't to do any further
				// discharging.  So, bypass all of the flow.
				DetIceStor( IceNum ).CompLoad = 0.0;
				DetIceStor( IceNum ).OutletTemp = DetIceStor( IceNum ).InletTemp;
				DetIceStor( IceNum ).TankOutletTemp = DetIceStor( IceNum ).InletTemp;
				mdot = 0.0;
				SetComponentFlowRate( mdot, DetIceStor( IceNum ).PlantInNodeNum, DetIceStor( IceNum ).PlantOutNodeNum, DetIceStor( IceNum ).PlantLoopNum, DetIceStor( IceNum ).PlantLoopSideNum, DetIceStor( IceNum ).PlantBranchNum, DetIceStor( IceNum ).PlantCompNum );

				DetIceStor( IceNum ).BypassMassFlowRate = mdot;
				DetIceStor( IceNum ).TankMassFlowRate = 0.0;
				DetIceStor( IceNum ).MassFlowRate = mdot;

			} else {

				//make flow request so tank will get flow
				mdot = DetIceStor( IceNum ).DesignMassFlowRate;
				SetComponentFlowRate( mdot, DetIceStor( IceNum ).PlantInNodeNum, DetIceStor( IceNum ).PlantOutNodeNum, DetIceStor( IceNum ).PlantLoopNum, DetIceStor( IceNum ).PlantLoopSideNum, DetIceStor( IceNum ).PlantBranchNum, DetIceStor( IceNum ).PlantCompNum );

				// We are in discharging mode, the temperatures are high enough to discharge
				// the tank, and we have some discharging left to do.
				if ( TempSetPt < ( DetIceStor( IceNum ).FreezingTemp + DeltaTofMin ) ) {
					// Outlet temperature cannot be below the freezing temperature so set
					// the outlet temperature to the freezing temperature and calculate
					// LMTDstar based on that assumption.
					TempSetPt = DetIceStor( IceNum ).FreezingTemp + DeltaTofMin;
				}

				ToutOld = TempSetPt;
				LMTDstar = CalcDetIceStorLMTDstar( TempIn, ToutOld, DetIceStor( IceNum ).FreezingTemp );

				// Find initial guess at average fraction charged during time step
				ChargeFrac = LocalLoad * TimeStepSys / DetIceStor( IceNum ).NomCapacity;
				if ( ( DetIceStor( IceNum ).IceFracRemaining - ChargeFrac ) < 0.0 ) ChargeFrac = DetIceStor( IceNum ).IceFracRemaining;
				AvgFracCharged = DetIceStor( IceNum ).IceFracRemaining - ( ChargeFrac / 2.0 );

				if ( DetIceStor( IceNum ).DischargeCurveTypeNum == CurveQuadraticLinear ) {
					Qstar = std::abs( CurveValue( DetIceStor( IceNum ).DischargeCurveNum, ( 1.0 - AvgFracCharged ), LMTDstar ) );
				} else { // ( DetIceStor( IceNum ).DischargeCurveTypeNum == CurveCubicLinear )
					Qstar = std::abs( CurveValue( DetIceStor( IceNum ).DischargeCurveNum, LMTDstar, AvgFracCharged ) );
				}

				ActualLoad = Qstar * DetIceStor( IceNum ).NomCapacity / DetIceStor( IceNum ).CurveFitTimeStep;

				ToutNew = TempIn - ( ActualLoad / ( DetIceStor( IceNum ).MassFlowRate * Cp ) );
				// Again, the outlet temperature cannot be below the freezing temperature (factoring in the tolerance)
				if ( ToutNew < ( DetIceStor( IceNum ).FreezingTemp + DeltaTofMin ) ) ToutNew = DetIceStor( IceNum ).FreezingTemp + DeltaTofMin;

				if ( ActualLoad > LocalLoad ) {
					// We have more than enough storage to meet the load so no need to iterate to find a solution
					DetIceStor( IceNum ).OutletTemp = TempSetPt;
					DetIceStor( IceNum ).TankOutletTemp = ToutNew;
					DetIceStor( IceNum ).CompLoad = DetIceStor( IceNum ).MassFlowRate * Cp * std::abs( TempIn - TempSetPt );
					DetIceStor( IceNum ).TankMassFlowRate = DetIceStor( IceNum ).CompLoad / Cp / std::abs( TempIn - ToutNew );
					DetIceStor( IceNum ).BypassMassFlowRate = DetIceStor( IceNum ).MassFlowRate - DetIceStor( IceNum ).TankMassFlowRate;

				} else {

					while ( IterNum < MaxIterNum ) {
						if ( std::abs( ToutOld - ToutNew ) > TemperatureToler ) {
							// Not converged yet so recalculated what is needed and keep iterating
							// Calculate new values for LMTDstar and Qstar based on updated outlet temperature
							ToutOld = ToutNew;
							LMTDstar = CalcDetIceStorLMTDstar( TempIn, ToutOld, DetIceStor( IceNum ).FreezingTemp );

							if ( DetIceStor( IceNum ).DischargeCurveTypeNum == CurveQuadraticLinear ) {
								Qstar = std::abs( CurveValue( DetIceStor( IceNum ).DischargeCurveNum, ( 1.0 - AvgFracCharged ), LMTDstar ) );
							} else { // ( DetIceStor( IceNum ).DischargeCurveTypeNum == CurveCubicLinear )
								Qstar = std::abs( CurveValue( DetIceStor( IceNum ).DischargeCurveNum, LMTDstar, AvgFracCharged ) );
							}

							// Now make sure that we don't go below 100% discharged and calculate the new average fraction
							ChargeFrac = Qstar * ( TimeStepSys / DetIceStor( IceNum ).CurveFitTimeStep );
							if ( ( DetIceStor( IceNum ).IceFracRemaining - ChargeFrac ) < 0.0 ) {
								ChargeFrac = DetIceStor( IceNum ).IceFracRemaining;
								Qstar = ChargeFrac;
							}
							AvgFracCharged = DetIceStor( IceNum ).IceFracRemaining - ( ChargeFrac / 2.0 );

							// Finally, update the actual load and calculate the new outlet temperature; increment iteration counter
							ActualLoad = Qstar * DetIceStor( IceNum ).NomCapacity / DetIceStor( IceNum ).CurveFitTimeStep;
							ToutNew = TempIn - ( ActualLoad / ( DetIceStor( IceNum ).MassFlowRate * Cp ) );
							// Again, the outlet temperature cannot be below the freezing temperature (factoring in the tolerance)
							if ( ToutNew < ( DetIceStor( IceNum ).FreezingTemp + DeltaTofMin ) ) ToutNew = DetIceStor( IceNum ).FreezingTemp + DeltaTofMin;
							++IterNum;

						} else {
							// Converged to acceptable tolerance so set output variables and exit DO WHILE loop
							break;

						}

					} // ...loop iterating for the ice storage outlet temperature

					// Keep track of times that the iterations got excessive
					if ( IterNum >= MaxIterNum && ( !WarmupFlag ) ) {
						++DetIceStor( IceNum ).DischargeIterErrors;
						if ( DetIceStor( IceNum ).DischargeIterErrors <= 25 ) {
							ShowWarningError( "Detailed Ice Storage model exceeded its internal discharging maximum iteration limit" );
							ShowContinueError( "Detailed Ice Storage System Name = " + DetIceStor( IceNum ).Name );
							ShowContinueErrorTimeStamp( "" );
						} else {
							ShowRecurringWarningErrorAtEnd( "Detailed Ice Storage system [" + DetIceStor( IceNum ).Name + "]  discharging maximum iteration limit exceeded occurrence continues.", DetIceStor( IceNum ).DischargeErrorCount );
						}
					}

					// We are now done finding the outlet temperature of the tank.  We need
					// to compare the outlet temperature to the setpoint temperature again
					// to see where we are at and then we can set the values for the key
					// outlet parameters.  If outlet temperature is greater than or equal
					// to the setpoint temperature, then send all flow through the tank.
					// Otherwise, we have more capacity than needed so let's bypass some
					// flow and meet the setpoint temperautre.
					if ( ToutNew >= TempSetPt ) {
						DetIceStor( IceNum ).OutletTemp = ToutNew;
						DetIceStor( IceNum ).TankOutletTemp = ToutNew;
						DetIceStor( IceNum ).BypassMassFlowRate = 0.0;
						DetIceStor( IceNum ).TankMassFlowRate = DetIceStor( IceNum ).MassFlowRate;
						DetIceStor( IceNum ).CompLoad = DetIceStor( IceNum ).MassFlowRate * Cp * std::abs( TempIn - ToutNew );
					} else {
						DetIceStor( IceNum ).OutletTemp = TempSetPt;
						DetIceStor( IceNum ).TankOutletTemp = ToutNew;
						DetIceStor( IceNum ).CompLoad = DetIceStor( IceNum ).MassFlowRate * Cp * std::abs( TempIn - TempSetPt );
						DetIceStor( IceNum ).TankMassFlowRate = DetIceStor( IceNum ).CompLoad / ( Cp * std::abs( TempIn - ToutNew ) );
						DetIceStor( IceNum ).BypassMassFlowRate = DetIceStor( IceNum ).MassFlowRate - DetIceStor( IceNum ).TankMassFlowRate;
					}

				}

			}

		} else { // Shouldn't get here ever (print error if we do)

			ShowFatalError( "Detailed Ice Storage systemic code error--contact EnergyPlus support" );

		}

	}

	//******************************************************************************

	void
	GetIceStorageInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:
		//       DATE WRITTEN:

		// PURPOSE OF THIS SUBROUTINE:!This routine will get the input
		//required by the PrimaryPlantLoopManager.  As such
		//it will interact with the Input Scanner to retrieve
		//information from the input file, count the number of
		//heating and cooling loops and begin to fill the
		//arrays associated with the type PlantLoopProps.

		// METHODOLOGY EMPLOYED: to be determined...
		// REFERENCES:

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using namespace ScheduleManager;
		using BranchNodeConnections::TestCompSet;
		using NodeInputManager::GetOnlySingleNode;
		using General::RoundSigDigits;

		// Locals
		int IceNum;

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		bool ErrorsFound;
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		// FLOW:

		ErrorsFound = false; // Always need to reset this since there are multiple types of ice storage systems

		//LOAD ARRAYS WITH IceStorage DATA
		NumIceStorages = GetNumObjectsFound( cIceStorageSimple ); // by ZG
		NumDetIceStorages = GetNumObjectsFound( cIceStorageDetailed );

		IceStorageTypeMap.allocate( NumIceStorages + NumDetIceStorages );
		CheckEquipName.allocate( NumIceStorages + NumDetIceStorages );
		CheckEquipName = true;

		// Allocate IceStorage based on NumOfIceStorage
		IceStorage.allocate( NumIceStorages );
		IceStorageReport.allocate( NumIceStorages );

		cCurrentModuleObject = cIceStorageSimple;
		for ( IceNum = 1; IceNum <= NumIceStorages; ++IceNum ) {

			GetObjectItem( cCurrentModuleObject, IceNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, _, _, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), IceStorage, IceNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}

			++TotalIceStorages;
			IceStorageTypeMap( TotalIceStorages ).StorageType = cCurrentModuleObject;
			IceStorageTypeMap( TotalIceStorages ).StorageType_Num = IceStorageType_Simple;
			IceStorageTypeMap( TotalIceStorages ).Name = cAlphaArgs( 1 );
			IceStorageTypeMap( TotalIceStorages ).LocalEqNum = IceNum;
			IceStorage( IceNum ).MapNum = TotalIceStorages;

			// ITS name
			IceStorage( IceNum ).Name = cAlphaArgs( 1 );

			// Get Ice Thermal Storage Type
			IceStorage( IceNum ).ITSType = cAlphaArgs( 2 );
			if ( SameString( IceStorage( IceNum ).ITSType, "IceOnCoilInternal" ) ) {
				IceStorage( IceNum ).ITSType_Num = ITSType_IceOnCoilInternal;
			} else if ( SameString( IceStorage( IceNum ).ITSType, "IceOnCoilExternal" ) ) {
				IceStorage( IceNum ).ITSType_Num = ITSType_IceOnCoilExternal;
			} else {
				ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				ErrorsFound = true;
			}

			// Get and Verify ITS nominal Capacity (user input is in GJ, internal value in in J)
			IceStorage( IceNum ).ITSNomCap = rNumericArgs( 1 ) * 1.e+09;
			if ( rNumericArgs( 1 ) == 0.0 ) {
				ShowSevereError( cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
				ErrorsFound = true;
			}

			// Get Plant Inlet Node Num
			IceStorage( IceNum ).PltInletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			// Get Plant Outlet Node Num
			IceStorage( IceNum ).PltOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			// Test InletNode and OutletNode
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Chilled Water Nodes" );

			// Initialize Report Variables
			IceStorageReport( IceNum ).MyLoad = 0.0;
			IceStorageReport( IceNum ).U = 0.0;
			IceStorageReport( IceNum ).Urate = 0.0;
			IceStorageReport( IceNum ).IceFracRemain = 1.0;
			IceStorageReport( IceNum ).ITSCoolingRate = 0.0;
			IceStorageReport( IceNum ).ITSCoolingEnergy = 0.0;
			IceStorageReport( IceNum ).ITSChargingRate = 0.0;
			IceStorageReport( IceNum ).ITSChargingEnergy = 0.0;
			IceStorageReport( IceNum ).ITSmdot = 0.0;
			IceStorageReport( IceNum ).ITSInletTemp = 0.0;
			IceStorageReport( IceNum ).ITSOutletTemp = 0.0;

		} // IceNum

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		// Setup Output Variables to Report  CurrentModuleObject='ThermalStorage:Ice:Simple'
		//********************************************
		for ( IceNum = 1; IceNum <= NumIceStorages; ++IceNum ) {

			SetupOutputVariable( "Ice Thermal Storage Requested Load [W]", IceStorageReport( IceNum ).MyLoad, "System", "Average", IceStorage( IceNum ).Name );

			// Ice fraction
			SetupOutputVariable( "Ice Thermal Storage End Fraction []", IceStorageReport( IceNum ).IceFracRemain, "Zone", "Average", IceStorage( IceNum ).Name );

			// Discharge: ITS Information
			SetupOutputVariable( "Ice Thermal Storage Mass Flow Rate [kg/s]", IceStorageReport( IceNum ).ITSmdot, "System", "Average", IceStorage( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Inlet Temperature [C]", IceStorageReport( IceNum ).ITSInletTemp, "System", "Average", IceStorage( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Outlet Temperature [C]", IceStorageReport( IceNum ).ITSOutletTemp, "System", "Average", IceStorage( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Cooling Discharge Rate [W]", IceStorageReport( IceNum ).ITSCoolingRate, "System", "Average", IceStorage( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Cooling Discharge Energy [J]", IceStorageReport( IceNum ).ITSCoolingEnergy, "System", "Sum", IceStorage( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Cooling Charge Rate [W]", IceStorageReport( IceNum ).ITSChargingRate, "System", "Average", IceStorage( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Cooling Charge Energy [J]", IceStorageReport( IceNum ).ITSChargingEnergy, "System", "Sum", IceStorage( IceNum ).Name );

		} // IceNum

		ErrorsFound = false; // Always need to reset this since there are multiple types of ice storage systems

		// Determine the number of detailed ice storage devices are in the input file and allocate appropriately
		cCurrentModuleObject = cIceStorageDetailed;

		DetIceStor.allocate( NumDetIceStorages ); // Allocate DetIceStorage based on NumDetIceStorages

		for ( IceNum = 1; IceNum <= NumDetIceStorages; ++IceNum ) {

			GetObjectItem( cCurrentModuleObject, IceNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), DetIceStor, IceNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}

			++TotalIceStorages;
			IceStorageTypeMap( TotalIceStorages ).StorageType = cCurrentModuleObject;
			IceStorageTypeMap( TotalIceStorages ).StorageType_Num = IceStorageType_Detailed;
			IceStorageTypeMap( TotalIceStorages ).Name = cAlphaArgs( 1 );
			IceStorageTypeMap( TotalIceStorages ).LocalEqNum = IceNum;

			DetIceStor( IceNum ).MapNum = TotalIceStorages;
			DetIceStor( IceNum ).Name = cAlphaArgs( 1 ); // Detailed ice storage name

			// Get and verify availability schedule
			DetIceStor( IceNum ).ScheduleName = cAlphaArgs( 2 ); // Detailed ice storage availability schedule name
			if ( lAlphaFieldBlanks( 2 ) ) {
				DetIceStor( IceNum ).ScheduleIndex = ScheduleAlwaysOn;
			} else {
				DetIceStor( IceNum ).ScheduleIndex = GetScheduleIndex( DetIceStor( IceNum ).ScheduleName );
				if ( DetIceStor( IceNum ).ScheduleIndex == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}

			// Get and Verify ITS nominal Capacity (user input is in GJ, internal value is in W-hr)
			// Convert GJ to J by multiplying by 10^9
			// Convert J to W-hr by dividing by number of seconds in an hour (3600)
			DetIceStor( IceNum ).NomCapacity = rNumericArgs( 1 ) * ( 1.e+09 ) / ( SecInHour );

			if ( rNumericArgs( 1 ) <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 1 ) + '=' + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			// Get Plant Inlet Node Num
			DetIceStor( IceNum ).PlantInNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			// Get Plant Outlet Node Num
			DetIceStor( IceNum ).PlantOutNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

			// Test InletNode and OutletNode
			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Chilled Water Nodes" );

			// Obtain the Charging and Discharging Curve types and names
			DetIceStor( IceNum ).DischargeCurveName = cAlphaArgs( 6 );
			DetIceStor( IceNum ).DischargeCurveNum = GetCurveIndex( cAlphaArgs( 6 ) );
			if ( DetIceStor( IceNum ).DischargeCurveNum <= 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			} else {
				DetIceStor( IceNum ).DischargeCurveType = GetCurveType( DetIceStor( IceNum ).DischargeCurveNum );
				if ( DetIceStor( IceNum ).DischargeCurveType == "QUADRATICLINEAR" ) {
					DetIceStor(IceNum).DischargeCurveTypeNum = CurveQuadraticLinear;
				} else if ( DetIceStor( IceNum ).DischargeCurveType == "CUBICLINEAR" ) {
					DetIceStor(IceNum).DischargeCurveTypeNum = CurveCubicLinear;
				}
			}
			if ( ( DetIceStor( IceNum ).DischargeCurveType != cAlphaArgs( 5 ) ) || ( ( DetIceStor( IceNum ).DischargeCurveType != "QUADRATICLINEAR" ) && ( DetIceStor( IceNum ).DischargeCurveType != "CUBICLINEAR" ) ) ) {
				ShowSevereError( cCurrentModuleObject + ": Discharge curve type not valid, type=" + cAlphaArgs( 5 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Type does not match type for curve name or type does not equal QuadraticLinear or CubicLinear" );
				ErrorsFound = true;
			}

			DetIceStor( IceNum ).ChargeCurveName = cAlphaArgs( 8 );
			DetIceStor( IceNum ).ChargeCurveNum = GetCurveIndex( cAlphaArgs( 8 ) );
			if ( DetIceStor( IceNum ).ChargeCurveNum <= 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			} else {
				DetIceStor( IceNum ).ChargeCurveType = GetCurveType( DetIceStor( IceNum ).ChargeCurveNum );
				if ( DetIceStor( IceNum ).ChargeCurveType == "QUADRATICLINEAR" ) {
					DetIceStor(IceNum).ChargeCurveTypeNum = CurveQuadraticLinear;
				} else if ( DetIceStor( IceNum ).ChargeCurveType == "CUBICLINEAR" ) {
					DetIceStor(IceNum).ChargeCurveTypeNum = CurveCubicLinear;
				}
			}
			if ( ( DetIceStor( IceNum ).ChargeCurveType != cAlphaArgs( 7 ) ) || ( ( DetIceStor( IceNum ).ChargeCurveType != "QUADRATICLINEAR" ) && ( DetIceStor( IceNum ).ChargeCurveType != "CUBICLINEAR" ) ) ) {
				ShowSevereError( cCurrentModuleObject + ": Charge curve type not valid, type=" + cAlphaArgs( 7 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Type does not match type for curve name or type does not equal QuadraticLinear or CubicLinear" );
				ErrorsFound = true;
			}

			DetIceStor( IceNum ).CurveFitTimeStep = rNumericArgs( 2 );
			if ( ( DetIceStor( IceNum ).CurveFitTimeStep <= 0.0 ) || ( DetIceStor( IceNum ).CurveFitTimeStep > 1.0 ) ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 2 ) + '=' + RoundSigDigits( rNumericArgs( 2 ), 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Curve fit time step invalid, less than zero or greater than 1 for " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			DetIceStor( IceNum ).ThawProcessIndicator = cAlphaArgs( 9 );
			if ( SameString( DetIceStor( IceNum ).ThawProcessIndicator, "INSIDEMELT" ) ) {
				DetIceStor( IceNum ).ThawProcessIndex = DetIceInsideMelt;
			} else if ( ( SameString( DetIceStor( IceNum ).ThawProcessIndicator, "OUTSIDEMELT" ) ) || ( DetIceStor( IceNum ).ThawProcessIndicator.empty() ) ) {
				DetIceStor( IceNum ).ThawProcessIndex = DetIceOutsideMelt;
			} else {
				ShowSevereError( "Invalid thaw process indicator of " + cAlphaArgs( 9 ) + " was entered" );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value should either be \"InsideMelt\" or \"OutsideMelt\"" );
				DetIceStor( IceNum ).ThawProcessIndex = DetIceInsideMelt; // Severe error will end simulation, but just in case...
				ErrorsFound = true;
			}

			// Get the other ice storage parameters (electric, heat loss, freezing temperature) and stupidity check each one
			DetIceStor( IceNum ).DischargeParaElecLoad = rNumericArgs( 3 );
			DetIceStor( IceNum ).ChargeParaElecLoad = rNumericArgs( 4 );
			DetIceStor( IceNum ).TankLossCoeff = rNumericArgs( 5 );
			DetIceStor( IceNum ).FreezingTemp = rNumericArgs( 6 );

			if ( ( DetIceStor( IceNum ).DischargeParaElecLoad < 0.0 ) || ( DetIceStor( IceNum ).DischargeParaElecLoad > 1.0 ) ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 3 ) + '=' + RoundSigDigits( rNumericArgs( 3 ), 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value is either less than/equal to zero or greater than 1" );
				ErrorsFound = true;
			}

			if ( ( DetIceStor( IceNum ).ChargeParaElecLoad < 0.0 ) || ( DetIceStor( IceNum ).ChargeParaElecLoad > 1.0 ) ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 4 ) + '=' + RoundSigDigits( rNumericArgs( 4 ), 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value is either less than/equal to zero or greater than 1" );
				ErrorsFound = true;
			}

			if ( ( DetIceStor( IceNum ).TankLossCoeff < 0.0 ) || ( DetIceStor( IceNum ).TankLossCoeff > 0.1 ) ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( 5 ) + '=' + RoundSigDigits( rNumericArgs( 5 ), 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value is either less than/equal to zero or greater than 0.1 (10%)" );
				ErrorsFound = true;
			}

			if ( ( DetIceStor( IceNum ).FreezingTemp < -10.0 ) || ( DetIceStor( IceNum ).FreezingTemp > 10.0 ) ) {
				ShowWarningError( "Potentially invalid " + cNumericFieldNames( 6 ) + '=' + RoundSigDigits( rNumericArgs( 6 ), 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "Value is either less than -10.0C or greater than 10.0C" );
				ShowContinueError( "This value will be allowed but the user should verify that this temperature is correct" );
			}

			// Initialize Report Variables
			DetIceStor( IceNum ).CompLoad = 0.0;
			DetIceStor( IceNum ).IceFracChange = 0.0;
			DetIceStor( IceNum ).IceFracRemaining = 1.0;
			DetIceStor( IceNum ).IceFracOnCoil = 1.0;
			DetIceStor( IceNum ).DischargingRate = 0.0;
			DetIceStor( IceNum ).DischargingEnergy = 0.0;
			DetIceStor( IceNum ).ChargingRate = 0.0;
			DetIceStor( IceNum ).ChargingEnergy = 0.0;
			DetIceStor( IceNum ).MassFlowRate = 0.0;
			DetIceStor( IceNum ).BypassMassFlowRate = 0.0;
			DetIceStor( IceNum ).TankMassFlowRate = 0.0;
			DetIceStor( IceNum ).InletTemp = 0.0;
			DetIceStor( IceNum ).OutletTemp = 0.0;
			DetIceStor( IceNum ).TankOutletTemp = 0.0;
			DetIceStor( IceNum ).ParasiticElecRate = 0.0;
			DetIceStor( IceNum ).ParasiticElecEnergy = 0.0;

		} // ...over detailed ice storage units

		if ( ( NumIceStorages + NumDetIceStorages ) <= 0 ) {
			ShowSevereError( "No Ice Storage Equipment found in GetIceStorage" );
			ErrorsFound = true;
		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		}

		// Setup Output Variables to Report CurrentModuleObject='ThermalStorage:Ice:Detailed'
		//********************************************
		for ( IceNum = 1; IceNum <= NumDetIceStorages; ++IceNum ) {

			SetupOutputVariable( "Ice Thermal Storage Cooling Rate [W]", DetIceStor( IceNum ).CompLoad, "System", "Average", DetIceStor( IceNum ).Name );

			// Ice fraction
			SetupOutputVariable( "Ice Thermal Storage Change Fraction []", DetIceStor( IceNum ).IceFracChange, "System", "Average", DetIceStor( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage End Fraction []", DetIceStor( IceNum ).IceFracRemaining, "System", "Average", DetIceStor( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage On Coil Fraction []", DetIceStor( IceNum ).IceFracOnCoil, "System", "Average", DetIceStor( IceNum ).Name );

			// Discharge: ITS Information
			SetupOutputVariable( "Ice Thermal Storage Mass Flow Rate [kg/s]", DetIceStor( IceNum ).MassFlowRate, "System", "Average", DetIceStor( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Bypass Mass Flow Rate [kg/s]", DetIceStor( IceNum ).BypassMassFlowRate, "System", "Average", DetIceStor( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Tank Mass Flow Rate [kg/s]", DetIceStor( IceNum ).TankMassFlowRate, "System", "Average", DetIceStor( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Fluid Inlet Temperature [C]", DetIceStor( IceNum ).InletTemp, "System", "Average", DetIceStor( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Blended Outlet Temperature [C]", DetIceStor( IceNum ).OutletTemp, "System", "Average", DetIceStor( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Tank Outlet Temperature [C]", DetIceStor( IceNum ).TankOutletTemp, "System", "Average", DetIceStor( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Cooling Discharge Rate [W]", DetIceStor( IceNum ).DischargingRate, "System", "Average", DetIceStor( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Cooling Discharge Energy [J]", DetIceStor( IceNum ).DischargingEnergy, "System", "Sum", DetIceStor( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Cooling Charge Rate [W]", DetIceStor( IceNum ).ChargingRate, "System", "Average", DetIceStor( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Cooling Charge Energy [J]", DetIceStor( IceNum ).ChargingEnergy, "System", "Sum", DetIceStor( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Ancillary Electric Power [W]", DetIceStor( IceNum ).ParasiticElecRate, "System", "Average", DetIceStor( IceNum ).Name );
			SetupOutputVariable( "Ice Thermal Storage Ancillary Electric Energy [J]", DetIceStor( IceNum ).ParasiticElecEnergy, "System", "Sum", DetIceStor( IceNum ).Name, _, "ELECTRICITY", _, _, "System" );

		} // ...over detailed ice storage units

	}

	//******************************************************************************

	void
	InitDetailedIceStorage()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes variables for the detailed ice storage model.

		// METHODOLOGY EMPLOYED:
		// Initializes parameters based on current status flag values.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_TS_IceDetailed;
		using DataPlant::CommonPipe_TwoWay;
		using DataPlant::SupplySide;
		using DataPlant::LoopFlowStatus_NeedyAndTurnsLoopOn;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyPlantScanFlag;
		static Array1D_bool MyEnvrnFlag;
		int CompNum; // local do loop index
		// FLOW:

		if ( MyOneTimeFlag ) {
			MyPlantScanFlag.allocate( NumDetIceStorages );
			MyEnvrnFlag.allocate( NumDetIceStorages );
			MyPlantScanFlag = true;
			MyEnvrnFlag = true;
			MyOneTimeFlag = false;
		}

		if ( MyPlantScanFlag( IceNum ) ) {
			ScanPlantLoopsForObject( DetIceStor( IceNum ).Name, TypeOf_TS_IceDetailed, DetIceStor( IceNum ).PlantLoopNum, DetIceStor( IceNum ).PlantLoopSideNum, DetIceStor( IceNum ).PlantBranchNum, DetIceStor( IceNum ).PlantCompNum );

			MyPlantScanFlag( IceNum ) = false;
		}

		if ( BeginEnvrnFlag && MyEnvrnFlag( IceNum ) ) { // Beginning of environment initializations
			// Make sure all state variables are reset at the beginning of every environment to avoid problems.
			// The storage unit is assumed to be fully charged at the start of any environment.
			// The IceNum variable is a module level variable that is already set before this subroutine is called.
			DetIceStor( IceNum ).IceFracChange = 0.0;
			DetIceStor( IceNum ).IceFracRemaining = 1.0;
			DetIceStor( IceNum ).IceFracOnCoil = 1.0;
			DetIceStor( IceNum ).InletTemp = 0.0;
			DetIceStor( IceNum ).OutletTemp = 0.0;
			DetIceStor( IceNum ).TankOutletTemp = 0.0;
			DetIceStor( IceNum ).DischargeIterErrors = 0;
			DetIceStor( IceNum ).ChargeIterErrors = 0;
			DetIceStor( IceNum ).DesignMassFlowRate = PlantLoop( DetIceStor( IceNum ).PlantLoopNum ).MaxMassFlowRate;
			//no design flow rates for model, assume min is zero and max is plant loop's max
			InitComponentNodes( 0.0, DetIceStor( IceNum ).DesignMassFlowRate, DetIceStor( IceNum ).PlantInNodeNum, DetIceStor( IceNum ).PlantOutNodeNum, DetIceStor( IceNum ).PlantLoopNum, DetIceStor( IceNum ).PlantLoopSideNum, DetIceStor( IceNum ).PlantBranchNum, DetIceStor( IceNum ).PlantCompNum );

			if ( ( PlantLoop( DetIceStor( IceNum ).PlantLoopNum ).CommonPipeType == CommonPipe_TwoWay ) && ( DetIceStor( IceNum ).PlantLoopSideNum == SupplySide ) ) {
				// up flow priority of other components on the same branch as the Ice tank
				for ( CompNum = 1; CompNum <= PlantLoop( DetIceStor( IceNum ).PlantLoopNum ).LoopSide( SupplySide ).Branch( DetIceStor( IceNum ).PlantBranchNum ).TotalComponents; ++CompNum ) {
					PlantLoop( DetIceStor( IceNum ).PlantLoopNum ).LoopSide( SupplySide ).Branch( DetIceStor( IceNum ).PlantBranchNum ).Comp( CompNum ).FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
				}

			}

			MyEnvrnFlag( IceNum ) = false;
		}
		if ( ! BeginEnvrnFlag ) MyEnvrnFlag( IceNum ) = true;

		// Initializations that are done every iteration
		// Make sure all of the reporting variables are always reset at the start of any iteration
		DetIceStor( IceNum ).CompLoad = 0.0;
		DetIceStor( IceNum ).IceFracChange = 0.0;
		DetIceStor( IceNum ).DischargingRate = 0.0;
		DetIceStor( IceNum ).DischargingEnergy = 0.0;
		DetIceStor( IceNum ).ChargingRate = 0.0;
		DetIceStor( IceNum ).ChargingEnergy = 0.0;
		DetIceStor( IceNum ).MassFlowRate = 0.0;
		DetIceStor( IceNum ).BypassMassFlowRate = 0.0;
		DetIceStor( IceNum ).TankMassFlowRate = 0.0;
		DetIceStor( IceNum ).ParasiticElecRate = 0.0;
		DetIceStor( IceNum ).ParasiticElecEnergy = 0.0;

	}

	void
	InitSimpleIceStorage()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jan 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// do initializations for simple ice storage

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataPlant::TypeOf_TS_IceSimple;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::CommonPipe_TwoWay;
		using DataPlant::SupplySide;
		using DataPlant::LoopFlowStatus_NeedyAndTurnsLoopOn;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Array1D_bool MyPlantScanFlag;
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;
		bool errFlag;
		int CompNum; // local do loop counter

		if ( MyOneTimeFlag ) {
			MyPlantScanFlag.allocate( NumIceStorages );
			MyEnvrnFlag.allocate( NumIceStorages );
			MyOneTimeFlag = false;
			MyPlantScanFlag = true;
			MyEnvrnFlag = true;
		}

		if ( MyPlantScanFlag( IceNum ) ) {
			// Locate the storage on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( IceStorage( IceNum ).Name, TypeOf_TS_IceSimple, IceStorage( IceNum ).LoopNum, IceStorage( IceNum ).LoopSideNum, IceStorage( IceNum ).BranchNum, IceStorage( IceNum ).CompNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitSimpleIceStorage: Program terminated due to previous condition(s)." );
			}
			MyPlantScanFlag( IceNum ) = false;
		}

		if ( BeginEnvrnFlag && MyEnvrnFlag( IceNum ) ) {
			IceStorage( IceNum ).DesignMassFlowRate = PlantLoop( IceStorage( IceNum ).LoopNum ).MaxMassFlowRate;
			//no design flow rates for model, assume min is zero and max is plant loop's max
			InitComponentNodes( 0.0, IceStorage( IceNum ).DesignMassFlowRate, IceStorage( IceNum ).PltInletNodeNum, IceStorage( IceNum ).PltOutletNodeNum, IceStorage( IceNum ).LoopNum, IceStorage( IceNum ).LoopSideNum, IceStorage( IceNum ).BranchNum, IceStorage( IceNum ).CompNum );
			if ( ( PlantLoop( IceStorage( IceNum ).LoopNum ).CommonPipeType == CommonPipe_TwoWay ) && ( IceStorage( IceNum ).LoopSideNum == SupplySide ) ) {
				// up flow priority of other components on the same branch as the Ice tank
				for ( CompNum = 1; CompNum <= PlantLoop( IceStorage( IceNum ).LoopNum ).LoopSide( SupplySide ).Branch( IceStorage( IceNum ).BranchNum ).TotalComponents; ++CompNum ) {
					PlantLoop( IceStorage( IceNum ).LoopNum ).LoopSide( SupplySide ).Branch( IceStorage( IceNum ).BranchNum ).Comp( CompNum ).FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
				}

			}
			IceStorageReport( IceNum ).MyLoad = 0.0;
			IceStorageReport( IceNum ).U = 0.0;
			IceStorageReport( IceNum ).Urate = 0.0;
			IceStorageReport( IceNum ).IceFracRemain = 1.0;
			IceStorageReport( IceNum ).ITSCoolingRate = 0.0;
			IceStorageReport( IceNum ).ITSCoolingEnergy = 0.0;
			IceStorageReport( IceNum ).ITSChargingRate = 0.0;
			IceStorageReport( IceNum ).ITSChargingEnergy = 0.0;
			IceStorageReport( IceNum ).ITSmdot = 0.0;
			IceStorageReport( IceNum ).ITSInletTemp = 0.0;
			IceStorageReport( IceNum ).ITSOutletTemp = 0.0;

			MyEnvrnFlag( IceNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag( IceNum ) = true;

		ITSNomCap = IceStorage( IceNum ).ITSNomCap;
		InletNodeNum = IceStorage( IceNum ).PltInletNodeNum;
		OutletNodeNum = IceStorage( IceNum ).PltOutletNodeNum;

	}

	//******************************************************************************

	void
	CalcIceStorageCapacity(
		int const IceStorageType,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap
	)
	{
		// SUBROUTINE INFORMATION:

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Umin( 0.0 ); // Min Urate  [fraction]
		Real64 Uact( 0.0 ); // Acting between Umax and Umin [fraction]
		Real64 ITSCoolingRateMax;
		Real64 ITSCoolingRateOpt;
		Real64 ITSCoolingRateMin;
		Real64 QiceMin;
		// unused  REAL(r64)    :: Tdb

		// FLOW
		{ auto const SELECT_CASE_var( IceStorageType );
		if ( SELECT_CASE_var == IceStorageType_Simple ) {

			//------------------------------------------------------------------------
			// FIRST PROCESS (MyLoad = 0.0 as IN)
			// At this moment as first calling of ITS, ITS provide ONLY MaxCap/OptCap/MinCap.
			//------------------------------------------------------------------------

			// Initialize Capacity
			MaxCap = 0.0;
			MinCap = 0.0;
			OptCap = 0.0;

			// XCurIceFrac is reset to 1.0 when first hour of day.
			// Starting full is assumed, because most ice systems are fully charged overnight
			if ( ResetXForITSFlag ) {
				XCurIceFrac = 1.0;
				IceStorageReport( IceNum ).IceFracRemain = 1.0;
				Urate = 0.0;
				ResetXForITSFlag = false;
			}

			// Calculate UAIceDisch[W/C] and UAIceCh[W/F] based on ONLY XCurIceFrac
			CalcUAIce( IceNum, XCurIceFrac, UAIceCh, UAIceDisCh, HLoss );

			// Calculate QiceMin by UAIceDisCh*deltaTlm
			//   with UAIceDisCh(function of XCurIceFrac), ITSInletTemp and ITSOutletTemp(=Node(OutletNodeNum)%TempSetPoint by E+[C])
			// QiceMin is REAL(r64) ITS capacity.
			CalcQiceDischageMax( QiceMin );

			// At the first call of ITS model, MyLoad is 0. After that proper MyLoad will be provided by E+.
			// Therefore, Umin is decided between input U and ITS REAL(r64) capacity.
			Umin = min( max( ( -( 1.0 - EpsLimitForDisCharge ) * QiceMin * TimeInterval / ITSNomCap ), ( -XCurIceFrac + EpsLimitForX ) ), 0.0 );

			// Calculate CoolingRate with Uact to provide E+.
			Uact = Umin;
			ITSCoolingRateMax = std::abs( Uact * ITSNomCap / TimeInterval );
			ITSCoolingRateOpt = ITSCoolingRateMax;
			ITSCoolingRateMin = 0.0;

			// Define MaxCap, OptCap, and MinCap
			MaxCap = ITSCoolingRateMax;
			OptCap = ITSCoolingRateOpt;
			MinCap = ITSCoolingRateMin;

		} else {

		}}

	}

	//******************************************************************************

	void
	CalcIceStorageDormant(
		int const IceStorageType, // BY ZG
		int & IceNum
	)
	{
		// SUBROUTINE INFORMATION:

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using PlantUtilities::SetComponentFlowRate;
		using DataPlant::PlantLoop;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW
		{ auto const SELECT_CASE_var( IceStorageType ); //by ZG

		if ( SELECT_CASE_var == IceStorageType_Simple ) { //by ZG

			// Provide output results for ITS.
			ITSMassFlowRate = 0.0; //[kg/s]

			SetComponentFlowRate( ITSMassFlowRate, IceStorage( IceNum ).PltInletNodeNum, IceStorage( IceNum ).PltOutletNodeNum, IceStorage( IceNum ).LoopNum, IceStorage( IceNum ).LoopSideNum, IceStorage( IceNum ).BranchNum, IceStorage( IceNum ).CompNum );

			ITSInletTemp = Node( InletNodeNum ).Temp; //[C]
			ITSOutletTemp = ITSInletTemp; //[C]
			{ auto const SELECT_CASE_var1( PlantLoop( IceStorage( IceNum ).LoopNum ).LoopDemandCalcScheme );
			if ( SELECT_CASE_var1 == SingleSetPoint ) {
				ITSOutletSetPointTemp = Node( OutletNodeNum ).TempSetPoint;
			} else if ( SELECT_CASE_var1 == DualSetPointDeadBand ) {
				ITSOutletSetPointTemp = Node( OutletNodeNum ).TempSetPointHi;
			}}
			ITSCoolingRate = 0.0; //[W]
			ITSCoolingEnergy = 0.0; //[J]

			Urate = 0.0; //[n/a]

		} else {

		}}

	}

	//******************************************************************************

	void
	CalcIceStorageCharge(
		int const IceStorageType, // BY ZG
		int & IceNum
	)
	{
		// SUBROUTINE INFORMATION:

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using ScheduleManager::GetCurrentScheduleValue;
		using Psychrometrics::CPCW;
		using PlantUtilities::SetComponentFlowRate;
		using DataPlant::PlantLoop;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Umax( 0.0 ); // Max Urate adjusted Urate based on Error protection (I) [fraction]
		Real64 Umin( 0.0 ); // Min Urate adjusted Urate based on Error protection (I) [fraction]
		Real64 Uact( 0.0 ); // Acting between Usys and UsysLow Urate adjusted Urate based on Error protection (I) [fraction]
		Real64 QiceMax; // [W]
		Real64 QiceMaxByChiller; // [W]
		Real64 QiceMaxByITS; // [W]
		Real64 Qice; // [W]
		Real64 DeltaTemp; // [C]

		// FLOW
		{ auto const SELECT_CASE_var( IceStorageType );
		if ( SELECT_CASE_var == IceStorageType_Simple ) {

			//--------------------------------------------------------
			// Initialize
			//--------------------------------------------------------
			// Below values for ITS are reported forCharging process.
			ITSMassFlowRate = IceStorage( IceNum ).DesignMassFlowRate; //[kg/s]

			SetComponentFlowRate( ITSMassFlowRate, IceStorage( IceNum ).PltInletNodeNum, IceStorage( IceNum ).PltOutletNodeNum, IceStorage( IceNum ).LoopNum, IceStorage( IceNum ).LoopSideNum, IceStorage( IceNum ).BranchNum, IceStorage( IceNum ).CompNum );

			ITSInletTemp = Node( InletNodeNum ).Temp; //[C]
			ITSOutletTemp = ITSInletTemp; //[C]
			{ auto const SELECT_CASE_var1( PlantLoop( IceStorage( IceNum ).LoopNum ).LoopDemandCalcScheme );
			if ( SELECT_CASE_var1 == SingleSetPoint ) {
				ITSOutletSetPointTemp = Node( OutletNodeNum ).TempSetPoint;
			} else if ( SELECT_CASE_var1 == DualSetPointDeadBand ) {
				ITSOutletSetPointTemp = Node( OutletNodeNum ).TempSetPointHi;
			}}
			ITSCoolingRate = 0.0; //[W]
			ITSCoolingEnergy = 0.0; //[J]

			// Initialize processed U values
			Urate = 0.0;

			// Calculate QiceMax which is REAL(r64) ITS capacity.
			// There are three possible to calculate QiceMax
			//   with ChillerCapacity(Chiller+ITS), ITS capacity(ITS), and QchillerMax(Chiller).
			//--------------------------------------------------------
			// Calcualte QiceMax with QiceMaxByChiller, QiceMaxByITS, QchillerMax
			//--------------------------------------------------------
			// Calculate Qice charge max by Chiller with Twb and UAIceCh
			CalcQiceChargeMaxByChiller( IceNum, QiceMaxByChiller ); //[W]

			// Chiller is remote now, so chiller out is inlet node temp
			ChillerOutletTemp = Node( IceStorage( IceNum ).PltInletNodeNum ).Temp;
			// Calculate Qice charge max by ITS with ChillerOutletTemp
			CalcQiceChargeMaxByITS( IceNum, ChillerOutletTemp, QiceMaxByITS ); //[W]

			// Select minimum as QiceMax
			// Because It is uncertain that QiceMax by chiller is same as QiceMax by ITS.
			QiceMax = min( QiceMaxByChiller, QiceMaxByITS );

			//--------------------------------------------------------
			// Calculate Umin,Umax,Uact
			//--------------------------------------------------------
			// Set Umin
			Umin = 0.0;
			// Calculate Umax based on real ITS Max Capacity and remained XCurIceFrac.
			// Umax should be equal or larger than 0.02 for realistic purpose by Dion.
			Umax = max( min( ( ( 1.0 - EpsLimitForCharge ) * QiceMax * TimeInterval / ITSNomCap ), ( 1.0 - XCurIceFrac - EpsLimitForX ) ), 0.0 );

			// Cannot charge more than the fraction that is left uncharged
			Umax = min( Umax, ( 1.0 - IceStorageReport( IceNum ).IceFracRemain ) / TimeStepSys );
			// First, check input U value.
			// Based on Umax and Umin, if necessary to run E+, calculate proper Uact.
			if ( Umax == 0.0 ) { //(No Capacity of ITS), ITS is OFF.
				Uact = 0.0;

			} else { // Umax non-zero
				Uact = Umax;
			} // Check Uact for Discharging Process

			//--------------------------------------------------------
			// Calcualte possible ITSChargingRate with Uact, Then error check
			//--------------------------------------------------------
			// Calculate possible ITSChargingRate with Uact
			Qice = Uact * ITSNomCap / TimeInterval; //[W]
			// If Qice is equal or less than 0.0, no need to calculate anymore.
			if ( Qice <= 0.0 ) {
				Urate = 0.0; //[ratio]
			}

			//--------------------------------------------------------
			// Find ChillerOutlet Temperature
			//--------------------------------------------------------
			// Chiller is remote now, so chiller out is inlet node temp
			ChillerOutletTemp = Node( IceStorage( IceNum ).PltInletNodeNum ).Temp;

			// Calculate leaving water temperature
			if ( ( Qice <= 0.0 ) || ( XCurIceFrac >= 1.0 ) ) {
				ITSOutletTemp = ITSInletTemp;
				DeltaTemp = 0.0;
				Qice = 0.0;
				Uact = 0.0;
			} else {
				DeltaTemp = Qice / CPCW( ITSInletTemp ) / ITSMassFlowRate;
				ITSOutletTemp = ITSInletTemp + DeltaTemp;
				// Limit leaving temp to be no greater than setpoint or freezing temp minus 1C
				ITSOutletTemp = min( ITSOutletTemp, ITSOutletSetPointTemp, ( FreezTemp - 1 ) );
				// Limit leaving temp to be no less than inlet temp
				ITSOutletTemp = max( ITSOutletTemp, ITSInletTemp );
				DeltaTemp = ITSOutletTemp - ITSInletTemp;
				Qice = DeltaTemp * CPCW( ITSInletTemp ) * ITSMassFlowRate;
				Uact = Qice / ( ITSNomCap / TimeInterval );
			} // End of leaving temp checks

			Urate = Uact;
			ITSCoolingRate = -Qice;
			ITSCoolingEnergy = ITSCoolingRate * TimeStepSys * SecInHour;

		} else {

		}}

	}

	//******************************************************************************

	void
	CalcQiceChargeMaxByChiller(
		int & IceNum,
		Real64 & QiceMaxByChiller
	)
	{

		// SUBROUTINE INFORMATION:

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// Calculation inside is IP unit, then return QiceMaxByChiller as SI [W] unit.

		// REFERENCES:

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 TchillerOut;

		// FLOW

		// Chiller is remote now, so chiller out is inlet node temp
		TchillerOut = Node( IceStorage( IceNum ).PltInletNodeNum ).Temp;
		QiceMaxByChiller = UAIceCh * ( FreezTemp - TchillerOut ); //[W] = [W/degC]*[degC]

		// If it happened, it is occurred at the Discharing or Dormant process.
		if ( QiceMaxByChiller <= 0.0 ) {
			QiceMaxByChiller = 0.0;
		}

	}

	//******************************************************************************

	void
	CalcQiceChargeMaxByITS(
		int & EP_UNUSED( IceNum ),
		Real64 const ChillerOutletTemp, // [degC]
		Real64 & QiceMaxByITS // [W]
	)
	{

		// SUBROUTINE INFORMATION:

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Tfr;
		Real64 ChillerInletTemp;
		Real64 ChOutletTemp;
		Real64 LogTerm;

		// FLOW
		// Qice is maximized when ChillerInletTemp and ChillerOutletTemp(input data) is almost same due to LMTD method.
		// Qice is minimized(=0) when ChillerInletTemp is almost same as FreezTemp(=0).

		// Initilize
		Tfr = FreezTempIP;
		ChOutletTemp = TempSItoIP( ChillerOutletTemp ); //[degF] = ConvertSItoIP[degC]
		// Chiller outlet temp must be below freeze temp, or else no charge
		if ( ChOutletTemp >= Tfr ) {
			ChillerInletTemp = ChOutletTemp;
			QiceMaxByITS = 0.0;
		} else {
			// Make ChillerInletTemp as almost same as ChillerOutletTemp(input data)
			ChillerInletTemp = ChOutletTemp + 0.01;
			// ChillerInletTemp cannot be greater than or equal to freeze temp
			if ( ChillerInletTemp >= Tfr ) {
				ChillerInletTemp = ChOutletTemp + ( Tfr - ChOutletTemp ) / 2;
			}

			LogTerm = ( Tfr - ChOutletTemp ) / ( Tfr - ChillerInletTemp );
			// Need to protect this from LogTerm <= 0 - not sure what it should do then
			if ( LogTerm <= 0.0 ) {
				ChillerInletTemp = ChOutletTemp;
				QiceMaxByITS = 0.0;
			}
			QiceMaxByITS = UAIceCh * ( TempIPtoSI( ChillerInletTemp ) - TempIPtoSI( ChOutletTemp ) ) / std::log( LogTerm );
		}

	}

	//******************************************************************************

	void
	CalcIceStorageDischarge(
		int const IceStorageType, // by ZG
		int const IceNum, // ice storage number
		Real64 const MyLoad, // operating load
		bool const RunFlag, // TRUE when ice storage operating
		bool const EP_UNUSED( FirstIteration ), // TRUE when first iteration of timestep
		Real64 const MaxCap // Max possible discharge rate (positive value)
	)
	{

		// SUBROUTINE INFORMATION:

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using DataHVACGlobals::TimeStepSys;
		using DataPlant::PlantLoop;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;
		using FluidProperties::GetDensityGlycol;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcIceStorageDischarge" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// External function
		// Local
		static Real64 Umax( 0.0 ); // Max Urate adjusted Urate based on Error protection (I) [fraction]
		static Real64 Umin( 0.0 ); // Min Urate adjusted Urate based on Error protection (I) [fraction]
		static Real64 Uact( 0.0 ); // Acting between Usys and UsysLow Urate adjusted Urate based on Error protection (I) [fraction]
		static Real64 Umyload( 0.0 );
		// unused  REAL(r64)    :: QiceMin
		static Real64 Qice( 0.0 );
		static Real64 DeltaTemp( 0.0 );

		int LoopNum;
		int LoopSideNum;
		Real64 CpFluid; // local temporary for plant loop's fluid specific heat

		// FLOW
		{ auto const SELECT_CASE_var( IceStorageType );
		if ( SELECT_CASE_var == IceStorageType_Simple ) {

			// Initialize processed Rate and Energy
			ITSMassFlowRate = 0.0;
			ITSCoolingRate = 0.0;
			ITSCoolingEnergy = 0.0;

			{ auto const SELECT_CASE_var1( PlantLoop( IceStorage( IceNum ).LoopNum ).LoopDemandCalcScheme );
			if ( SELECT_CASE_var1 == SingleSetPoint ) {
				ITSOutletSetPointTemp = Node( OutletNodeNum ).TempSetPoint;
			} else if ( SELECT_CASE_var1 == DualSetPointDeadBand ) {
				ITSOutletSetPointTemp = Node( OutletNodeNum ).TempSetPointHi;
			}}

			// Initialize processed U values
			Umax = 0.0;
			Umin = 0.0;
			Uact = 0.0;
			Umyload = 0.0;
			Urate = 0.0;

			// If no component demand or ITS OFF, then RETURN.
			if ( MyLoad == 0 || ! RunFlag ) {
				ITSMassFlowRate = 0.0;
				ITSInletTemp = Node( InletNodeNum ).Temp;
				ITSOutletTemp = ITSInletTemp;
				ITSCoolingRate = 0.0;
				ITSCoolingEnergy = 0.0;
				return;
			}

			// If FlowLock(provided by PlantSupplyManager) is False(=0), that is, MyLoad is not changed.
			// then based on MyLoad, new ITSMassFlowRate will be calculated.

			//----------------------------
			LoopNum = IceStorage( IceNum ).LoopNum;
			LoopSideNum = IceStorage( IceNum ).LoopSideNum;

			CpFluid = GetDensityGlycol( PlantLoop( LoopNum ).FluidName, Node( InletNodeNum ).Temp, PlantLoop( LoopNum ).FluidIndex, RoutineName );

			// Calculate Umyload based on MyLoad from E+
			Umyload = -MyLoad * TimeInterval / ITSNomCap;
			// Calculate Umax and Umin
			// Cannot discharge more than the fraction that is left
			Umax = -IceStorageReport( IceNum ).IceFracRemain / TimeStepSys;
			// Calculate Umin based on returned MyLoad from E+.
			Umin = min( Umyload, 0.0 );
			// Based on Umax and Umin, if necessary to run E+, calculate proper Uact
			// U is negative here.
			Uact = max( Umin, Umax );

			// Set ITSInletTemp provided by E+
			ITSInletTemp = Node( InletNodeNum ).Temp;
			//The first thing is to set the ITSMassFlowRate
			ITSMassFlowRate = IceStorage( IceNum ).DesignMassFlowRate; //[kg/s]

			SetComponentFlowRate( ITSMassFlowRate, IceStorage( IceNum ).PltInletNodeNum, IceStorage( IceNum ).PltOutletNodeNum, IceStorage( IceNum ).LoopNum, IceStorage( IceNum ).LoopSideNum, IceStorage( IceNum ).BranchNum, IceStorage( IceNum ).CompNum );

			// Qice is calculate input U which is within boundary between Umin and Umax.
			Qice = Uact * ITSNomCap / TimeInterval;
			// Qice cannot exceed MaxCap calulated by CalcIceStorageCapacity
			// Note Qice is negative here, MaxCap is positive
			Qice = max( Qice, -MaxCap );

			// Calculate leaving water temperature
			if ( ( Qice >= 0.0 ) || ( XCurIceFrac <= 0.0 ) || ( ITSMassFlowRate < MassFlowTolerance ) ) {
				ITSOutletTemp = ITSInletTemp;
				DeltaTemp = 0.0;
				Qice = 0.0;
				Uact = 0.0;
			} else {
				DeltaTemp = Qice / CpFluid / ITSMassFlowRate;
				ITSOutletTemp = ITSInletTemp + DeltaTemp;
				// Limit leaving temp to be no less than setpoint or freezing temp plus 1C
				ITSOutletTemp = max( ITSOutletTemp, ITSOutletSetPointTemp, ( FreezTemp + 1 ) );
				// Limit leaving temp to be no greater than inlet temp
				ITSOutletTemp = min( ITSOutletTemp, ITSInletTemp );
				DeltaTemp = ITSOutletTemp - ITSInletTemp;
				Qice = DeltaTemp * CpFluid * ITSMassFlowRate;
				Uact = Qice / ( ITSNomCap / TimeInterval );
			} // End of leaving temp checks

			// Calculate reported U value
			Urate = Uact;
			// Calculate ITSCoolingEnergy [J]
			ITSCoolingRate = -Qice;
			ITSCoolingEnergy = ITSCoolingRate * TimeStepSys * SecInHour;

		} else {
		}}

	}

	//******************************************************************************

	void
	CalcQiceDischageMax( Real64 & QiceMin )
	{

		// SUBROUTINE INFORMATION:

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using DataPlant::SingleSetPoint;
		using DataPlant::DualSetPointDeadBand;

		// Locals
		Real64 ITSInletTemp;
		Real64 ITSOutletTemp( 0.0 );

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 LogTerm;

		// FLOW
		// Qice is minimized when ITSInletTemp and ITSOutletTemp is almost same due to LMTD method.
		// Qice is maximized(=0) when ITSOutletTemp is almost same as FreezTemp(=0).

		// Set ITSInletTemp from E+.
		ITSInletTemp = Node( InletNodeNum ).Temp;

		// Make ITSOutletTemp as almost same as ITSInletTemp
		{ auto const SELECT_CASE_var( PlantLoop( IceStorage( IceNum ).LoopNum ).LoopDemandCalcScheme );
		if ( SELECT_CASE_var == SingleSetPoint ) {
			ITSOutletTemp = Node( OutletNodeNum ).TempSetPoint;
		} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
			ITSOutletTemp = Node( OutletNodeNum ).TempSetPointHi;
		} else {
			assert( false );
		}}

		LogTerm = ( ITSInletTemp - FreezTemp ) / ( ITSOutletTemp - FreezTemp );

		if ( LogTerm <= 1 ) {
			QiceMin = 0.0;
		} else {
			QiceMin = UAIceDisCh * ( ITSInletTemp - ITSOutletTemp ) / std::log( LogTerm );
		}

	}

	//******************************************************************************

	void
	CalcUAIce(
		int const IceNum,
		Real64 const XCurIceFrac,
		Real64 & UAIceCh,
		Real64 & UAIceDisCh,
		Real64 & HLoss
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// This routine is funtion of XCurIceFrac, and UA vaule is based on 1 hour.

		// REFERENCES:

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 y;
		//  REAL(r64)  :: dTlmic
		//  REAL(r64)  :: Tfr     ! IP freezing temperature

		// Flow

		{ auto const SELECT_CASE_var( IceStorage( IceNum ).ITSType_Num );
		if ( SELECT_CASE_var == ITSType_IceOnCoilInternal ) {
			y = XCurIceFrac;
			UAIceCh = ( 1.3879 - 7.6333 * y + 26.3423 * pow_2( y ) - 47.6084 * pow_3( y ) + 41.8498 * pow_4( y ) - 14.2948 * pow_5( y ) ) * IceStorage( IceNum ).ITSNomCap / TimeInterval / 10.0; // [W/C]
			y = 1.0 - XCurIceFrac;
			UAIceDisCh = ( 1.3879 - 7.6333 * y + 26.3423 * pow_2( y ) - 47.6084 * pow_3( y ) + 41.8498 * pow_4( y ) - 14.2948 * pow_5( y ) ) * IceStorage( IceNum ).ITSNomCap / TimeInterval / 10.0; // [W/C]
			HLoss = 0.0;
		} else if ( SELECT_CASE_var == ITSType_IceOnCoilExternal ) {
			y = XCurIceFrac;
			UAIceCh = ( 1.3879 - 7.6333 * y + 26.3423 * pow_2( y ) - 47.6084 * pow_3( y ) + 41.8498 * pow_4( y ) - 14.2948 * pow_5( y ) ) * IceStorage( IceNum ).ITSNomCap / TimeInterval / 10.0; // [W/C]
			y = 1.0 - XCurIceFrac;
			UAIceDisCh = ( 1.1756 - 5.3689 * y + 17.3602 * pow_2( y ) - 30.1077 * pow_3( y ) + 25.6387 * pow_4( y ) - 8.5102 * pow_5( y ) ) * IceStorage( IceNum ).ITSNomCap / TimeInterval / 10.0; // [W/C]
			HLoss = 0.0;

		}}

	}

	Real64
	CalcDetIceStorLMTDstar(
		Real64 const Tin, // ice storage unit inlet temperature
		Real64 const Tout, // ice storage unit outlet (setpoint) temperature
		Real64 const Tfr // freezing temperature
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the log mean temperature difference for
		// the detailed ice storage unit.  The temperature difference is non-
		// dimensionalized using a nominal temperature difference of 10C.
		// This value must be used when obtaining the curve fit coefficients.

		// METHODOLOGY EMPLOYED:
		// Straight-forward calculation where:
		// LMTD* = LMTD/Tnom
		// LMTD = (Tin-Tout)/ln((Tin-Tfr)/(Tout-Tfr))

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 CalcDetIceStorLMTDstar;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const Tnom( 10.0 ); // Nominal temperature difference across the ice storage unit [C]

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 DeltaTio; // Inlet to outlet temperature difference
		Real64 DeltaTif; // Inlet to freezing temperature difference
		Real64 DeltaTof; // Outlet to freezing temperature difference

		// FLOW:
		// First set the temperature differences and avoid problems with the LOG
		// term by setting some reasonable minimums
		DeltaTio = std::abs( Tin - Tout );
		DeltaTif = std::abs( Tin - Tfr );
		DeltaTof = std::abs( Tout - Tfr );

		if ( DeltaTif < DeltaTifMin ) DeltaTif = DeltaTifMin;
		if ( DeltaTof < DeltaTofMin ) DeltaTof = DeltaTofMin;

		CalcDetIceStorLMTDstar = ( DeltaTio / std::log( DeltaTif / DeltaTof ) ) / Tnom;

		return CalcDetIceStorLMTDstar;

	}

	// *****************************************************************************

	Real64
	TempSItoIP( Real64 const Temp )
	{

		// Return value
		Real64 TempSItoIP;

		TempSItoIP = ( Temp * 9.0 / 5.0 ) + 32.0;
		return TempSItoIP;
	}

	// *****************************************************************************

	Real64
	TempIPtoSI( Real64 const Temp )
	{

		// Return value
		Real64 TempIPtoSI;

		TempIPtoSI = ( Temp - 32.0 ) * 5.0 / 9.0;
		return TempIPtoSI;
	}

	// *****************************************************************************

	void
	UpdateNode(
		Real64 const MyLoad,
		bool const RunFlag,
		int const EP_UNUSED( Num )
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    October 1998

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW

		// Update Node Inlet & Outlet MassFlowRat
		SafeCopyPlantNode( InletNodeNum, OutletNodeNum );
		if ( MyLoad == 0 || ! RunFlag ) {
			// Update Outlet Conditions so that same as Inlet, so component can be bypassed if necessary
			Node( OutletNodeNum ).Temp = Node( InletNodeNum ).Temp;
		} else {
			Node( OutletNodeNum ).Temp = ITSOutletTemp;
		}

		//! ??? For now, always set outletnode mass flow equal to inletnode mass flow
		//! ??? Node(InletNodeNum)%MassFlowRate   = ITSMassFlowRate
		//! ???  Node(OutletNodeNum)%MassFlowRate  = ITSMassFlowRate
		//!  IF (Node(InletNodeNum)%MassFlowRate > 0.0) THEN
		//    Node(OutletNodeNum)%MassFlowRate = Node(InletNodeNum)%MassFlowRate
		//!  ELSE
		//!    Node(InletNodeNum)%MassFlowRate  = Node(InletNodeNum)%MassFlowRateMaxAvail
		//!    Node(OutletNodeNum)%MassFlowRate = Node(InletNodeNum)%MassFlowRateMaxAvail
		//!  ENDIF
		//  Node(OutletNodeNum)%MassFlowRateMax = Node(InletNodeNum)%MassFlowRateMax
		//  Node(OutletNodeNum)%MassFlowRateMin = Node(InletNodeNum)%MassFlowRateMin
		//  Node(OutletNodeNum)%MassFlowRateMaxAvail = Node(InletNodeNum)%MassFlowRateMaxAvail
		//  Node(OutletNodeNum)%MassFlowRateMinAvail = Node(InletNodeNum)%MassFlowRateMinAvail

	}

	// *****************************************************************************

	void
	RecordOutput(
		int const IceNum,
		Real64 const MyLoad,
		bool const RunFlag
	)
	{

		// SUBROUTINE INFORMATION:

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW

		if ( MyLoad == 0 || ! RunFlag ) {
			IceStorageReport( IceNum ).MyLoad = MyLoad;
			IceStorageReport( IceNum ).U = U;
			IceStorageReport( IceNum ).Urate = Urate;
			IceStorageReport( IceNum ).ITSCoolingRate = 0.0;
			IceStorageReport( IceNum ).ITSCoolingEnergy = 0.0;
			IceStorageReport( IceNum ).ITSChargingRate = 0.0;
			IceStorageReport( IceNum ).ITSChargingEnergy = 0.0;
			IceStorageReport( IceNum ).ITSmdot = 0.0;
			IceStorageReport( IceNum ).ITSInletTemp = ITSInletTemp;
			IceStorageReport( IceNum ).ITSOutletTemp = ITSOutletTemp;

		} else {
			IceStorageReport( IceNum ).MyLoad = MyLoad;
			IceStorageReport( IceNum ).U = U;
			IceStorageReport( IceNum ).Urate = Urate;
			if ( ITSCoolingRate > 0.0 ) {
				IceStorageReport( IceNum ).ITSCoolingRate = ITSCoolingRate;
				IceStorageReport( IceNum ).ITSCoolingEnergy = ITSCoolingEnergy;
				IceStorageReport( IceNum ).ITSChargingRate = 0.0;
				IceStorageReport( IceNum ).ITSChargingEnergy = 0.0;
			} else {
				IceStorageReport( IceNum ).ITSCoolingRate = 0.0;
				IceStorageReport( IceNum ).ITSCoolingEnergy = 0.0;
				IceStorageReport( IceNum ).ITSChargingRate = -ITSCoolingRate;
				IceStorageReport( IceNum ).ITSChargingEnergy = -ITSCoolingEnergy;
			}
			IceStorageReport( IceNum ).ITSmdot = ITSMassFlowRate;
			IceStorageReport( IceNum ).ITSInletTemp = ITSInletTemp;
			IceStorageReport( IceNum ).ITSOutletTemp = ITSOutletTemp;

		}

	}

	// *****************************************************************************

	void
	UpdateIceFractions()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Mike Witte
		//       DATE WRITTEN   September 2005
		//       MODIFIED       Rick Strand (Feb 2006, for detailed ice storage model)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Update all ice fractions at end of system time step.

		// METHODOLOGY EMPLOYED:
		// This is called from HVACManager once we have actually stepped forward
		// a system time step.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IceNum2;

		// FLOW

		for ( IceNum2 = 1; IceNum2 <= NumIceStorages; ++IceNum2 ) {
			IceStorageReport( IceNum2 ).IceFracRemain += IceStorageReport( IceNum2 ).Urate * TimeStepSys;
			if ( IceStorageReport( IceNum2 ).IceFracRemain <= 0.001 ) IceStorageReport( IceNum2 ).IceFracRemain = 0.0;
			if ( IceStorageReport( IceNum2 ).IceFracRemain > 1.0 ) IceStorageReport( IceNum2 ).IceFracRemain = 1.0;
		}

		for ( IceNum2 = 1; IceNum2 <= NumDetIceStorages; ++IceNum2 ) {
			DetIceStor( IceNum2 ).IceFracRemaining += DetIceStor( IceNum2 ).IceFracChange - ( DetIceStor( IceNum2 ).TankLossCoeff * TimeStepSys );
			if ( DetIceStor( IceNum2 ).IceFracRemaining < 0.001 ) DetIceStor( IceNum2 ).IceFracRemaining = 0.0;
			if ( DetIceStor( IceNum2 ).IceFracRemaining > 1.000 ) DetIceStor( IceNum2 ).IceFracRemaining = 1.0;
			// Reset the ice on the coil to zero for inside melt whenever discharging takes place.
			// This assumes that any remaining ice floats away from the coil and resettles perfectly.
			// While this is not exactly what happens and it is possible theoretically to have multiple
			// freeze thaw cycles that are not complete, this is the best we can do.
			if ( DetIceStor( IceNum2 ).ThawProcessIndex == DetIceInsideMelt ) {
				if ( DetIceStor( IceNum2 ).IceFracChange < 0.0 ) {
					DetIceStor( IceNum2 ).IceFracOnCoil = 0.0;
				} else {
					// Assume loss term does not impact ice on the coil but what is remaining
					DetIceStor( IceNum2 ).IceFracOnCoil += DetIceStor( IceNum2 ).IceFracChange;
					// If the ice remaining has run out because of tank losses, reset ice fraction on coil so that it keeps track of losses
					if ( DetIceStor( IceNum2 ).IceFracOnCoil > DetIceStor( IceNum2 ).IceFracRemaining ) DetIceStor( IceNum2 ).IceFracOnCoil = DetIceStor( IceNum2 ).IceFracRemaining;
				}
			} else { // Outside melt system so IceFracOnCoil is always the same as IceFracRemaining (needs to be done for reporting only)
				DetIceStor( IceNum2 ).IceFracOnCoil = DetIceStor( IceNum2 ).IceFracRemaining;
			}
		}

	}

	void
	UpdateDetailedIceStorage()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine takes the necessary information from the local data
		// structure and moves it back to the loop node data structure.

		// METHODOLOGY EMPLOYED:
		// Not much mystery here--just move the data to the appropriate place
		// for the detailed ice storage system in question.

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InNodeNum; // Plant inlet node number for component
		int OutNodeNum; // Plant outlet node number for component

		// FLOW:
		// Set the temperature and flow rate for the component outlet node
		InNodeNum = DetIceStor( IceNum ).PlantInNodeNum;
		OutNodeNum = DetIceStor( IceNum ).PlantOutNodeNum;

		SafeCopyPlantNode( InNodeNum, OutNodeNum );

		Node( OutNodeNum ).Temp = DetIceStor( IceNum ).OutletTemp;

	}

	void
	ReportDetailedIceStorage()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reports all of the output necessary for the model.

		// METHODOLOGY EMPLOYED:
		// Just take what has already been calculated or calculate the appropriate
		// output value based on simulation data.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		Real64 const LowLoadLimit( 0.1 ); // Load below which device can be assumed off [W]

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:
		// Determine what is going on  based on load and the inlet and outlet temperature comparison

		if ( DetIceStor( IceNum ).CompLoad < LowLoadLimit ) { // No load condition

			DetIceStor( IceNum ).IceFracChange = 0.0;
			DetIceStor( IceNum ).DischargingRate = 0.0;
			DetIceStor( IceNum ).DischargingEnergy = 0.0;
			DetIceStor( IceNum ).ChargingRate = 0.0;
			DetIceStor( IceNum ).ChargingEnergy = 0.0;
			DetIceStor( IceNum ).ParasiticElecRate = 0.0;
			DetIceStor( IceNum ).ParasiticElecEnergy = 0.0;

		} else { // There is a load, determine whether we are charging or discharging based on inlet and outlet temperature

			if ( DetIceStor( IceNum ).InletTemp < DetIceStor( IceNum ).OutletTemp ) { // Charging Mode

				DetIceStor( IceNum ).ChargingRate = DetIceStor( IceNum ).CompLoad;
				DetIceStor( IceNum ).ChargingEnergy = DetIceStor( IceNum ).CompLoad * ( TimeStepSys * SecInHour );
				DetIceStor( IceNum ).IceFracChange = DetIceStor( IceNum ).CompLoad * TimeStepSys / DetIceStor( IceNum ).NomCapacity;
				DetIceStor( IceNum ).DischargingRate = 0.0;
				DetIceStor( IceNum ).DischargingEnergy = 0.0;
				DetIceStor( IceNum ).ParasiticElecRate = DetIceStor( IceNum ).ChargeParaElecLoad * DetIceStor( IceNum ).CompLoad;
				DetIceStor( IceNum ).ParasiticElecEnergy = DetIceStor( IceNum ).ChargeParaElecLoad * DetIceStor( IceNum ).ChargingEnergy;

			} else { // (DetIceStor(IceNum)%InletTemp < DetIceStor(IceNum)%OutletTemp) Discharging Mode

				DetIceStor( IceNum ).DischargingRate = DetIceStor( IceNum ).CompLoad;
				DetIceStor( IceNum ).DischargingEnergy = DetIceStor( IceNum ).CompLoad * ( TimeStepSys * SecInHour );
				DetIceStor( IceNum ).IceFracChange = -DetIceStor( IceNum ).CompLoad * TimeStepSys / DetIceStor( IceNum ).NomCapacity;
				DetIceStor( IceNum ).ChargingRate = 0.0;
				DetIceStor( IceNum ).ChargingEnergy = 0.0;
				DetIceStor( IceNum ).ParasiticElecRate = DetIceStor( IceNum ).DischargeParaElecLoad * DetIceStor( IceNum ).CompLoad;
				DetIceStor( IceNum ).ParasiticElecEnergy = DetIceStor( IceNum ).DischargeParaElecLoad * DetIceStor( IceNum ).ChargingEnergy;

			}

		}

	}

} // IceThermalStorage

} // EnergyPlus
