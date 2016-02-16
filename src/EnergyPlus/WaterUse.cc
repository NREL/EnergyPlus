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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <WaterUse.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataWater.hh>
#include <General.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

namespace EnergyPlus {

namespace WaterUse {

	// MODULE INFORMATION:
	//       AUTHOR         Peter Graham Ellis
	//       DATE WRITTEN   August 2006
	//       MODIFIED       Brent Griffith, plant upgrade
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:

	// METHODOLOGY EMPLOYED:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::WarmupFlag;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::InitConvTemp;
	using DataGlobals::NumOfZones;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// MODULE VARIABLE DECLARATIONS:
	//INTEGER :: MaxIterationsErrorCount =0

	// SUBROUTINE SPECIFICATIONS:

	// Types

	// Don't think this needs to inherit from PlantComponent since it doesn't "simulate"
	struct WaterEquipment_T
	{
		// Static members
		static Array1D< WaterEquipment_T > sm_instances;
		static int sm_numInstances;
		
		// Members
		std::string Name; // Name of DHW
		std::string m_EndUseSubcatName;
		int m_Connections; // Index for WATER USE CONNECTIONS object
		Real64 m_PeakVolFlowRate; // Peak volumetric flow rate, also water consumption rate (m3/s)
		int m_FlowRateFracSchedule; // Pointer to schedule object
		Real64 m_ColdVolFlowRate;
		Real64 m_HotVolFlowRate;
		Real64 m_TotalVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
		Real64 m_ColdMassFlowRate;
		Real64 m_HotMassFlowRate;
		Real64 m_TotalMassFlowRate; // Mass flow rate (kg/s)
		Real64 m_DrainMassFlowRate;
		int m_ColdTempSchedule; // Index for schedule object
		int m_HotTempSchedule; // Index for schedule object
		int m_TargetTempSchedule; // Index for schedule object
		Real64 m_ColdTemp; // Cold supply water temperature (C)
		Real64 m_HotTemp; // Hot supply water temperature (C)
		Real64 m_TargetTemp; // Target (mixed) water temperature (C)
		Real64 m_MixedTemp; // Actual outlet (mixed) water temperature (C)
		Real64 m_DrainTemp;
		int m_ZoneNum; // Index for zone object
		int m_SensibleFracSchedule; // Pointer to schedule object
		Real64 m_SensibleRate;
		Real64 m_SensibleEnergy;
		Real64 m_SensibleRateNoMultiplier;
		int m_LatentFracSchedule; // Pointer to schedule object
		Real64 m_LatentRate;
		Real64 m_LatentEnergy;
		Real64 m_LatentRateNoMultiplier;
		Real64 m_MoistureRate;
		Real64 m_MoistureMass;
		Real64 m_ColdVolume; // Water consumption (m3)
		Real64 m_HotVolume; // Water consumption (m3)
		Real64 m_TotalVolume; // Water consumption (m3)
		Real64 m_Power; // Heating rate required to meet the mixed water temperature (W)
		Real64 m_Energy; // Heating energy required to meet the mixed water temperature (J)

		// Default Constructor
		WaterEquipment_T() :
			m_Connections( 0 ),
			m_PeakVolFlowRate( 0.0 ),
			m_FlowRateFracSchedule( 0 ),
			m_ColdVolFlowRate( 0.0 ),
			m_HotVolFlowRate( 0.0 ),
			m_TotalVolFlowRate( 0.0 ),
			m_ColdMassFlowRate( 0.0 ),
			m_HotMassFlowRate( 0.0 ),
			m_TotalMassFlowRate( 0.0 ),
			m_DrainMassFlowRate( 0.0 ),
			m_ColdTempSchedule( 0 ),
			m_HotTempSchedule( 0 ),
			m_TargetTempSchedule( 0 ),
			m_ColdTemp( 0.0 ),
			m_HotTemp( 0.0 ),
			m_TargetTemp( 0.0 ),
			m_MixedTemp( 0.0 ),
			m_DrainTemp( 0.0 ),
			m_ZoneNum( 0 ),
			m_SensibleFracSchedule( 0 ),
			m_SensibleRate( 0.0 ),
			m_SensibleEnergy( 0.0 ),
			m_SensibleRateNoMultiplier( 0.0 ),
			m_LatentFracSchedule( 0 ),
			m_LatentRate( 0.0 ),
			m_LatentEnergy( 0.0 ),
			m_LatentRateNoMultiplier( 0.0 ),
			m_MoistureRate( 0.0 ),
			m_MoistureMass( 0.0 ),
			m_ColdVolume( 0.0 ),
			m_HotVolume( 0.0 ),
			m_TotalVolume( 0.0 ),
			m_Power( 0.0 ),
			m_Energy( 0.0 )
		{}

		// Reset Some Values to Zeros
		void
		reset()
		{
			m_SensibleRate = 0.0;
			m_SensibleEnergy = 0.0;
			m_LatentRate = 0.0;
			m_LatentEnergy = 0.0;
			m_MixedTemp = 0.0;
			m_TotalMassFlowRate = 0.0;
			m_DrainTemp = 0.0;
		}

		void
		CalcEquipmentFlowRates( );
		
		void
		CalcEquipmentDrainTemp( );
		
	};

	Array1D< WaterEquipment_T > WaterEquipment_T::sm_instances;
	int WaterEquipment_T::sm_numInstances = 0;

	void
	WaterEquipment_T::CalcEquipmentFlowRates( )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate desired hot and cold water flow rates

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using Psychrometrics::RhoH2O;
		using DataEnvironment::WaterMainsTemp;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WaterConnNum;

		// FLOW:
		WaterConnNum = m_Connections;

		if ( WaterConnNum > 0 ) {
			// Get water temperature conditions from the CONNECTIONS object
			m_ColdTemp = WaterConnections_T::sm_instances( WaterConnNum ).m_ColdTemp;
			m_HotTemp = WaterConnections_T::sm_instances( WaterConnNum ).m_HotTemp;

		} else {
			// Get water temperature conditions from the WATER USE EQUIPMENT schedules
			if ( m_ColdTempSchedule > 0 ) {
				m_ColdTemp = GetCurrentScheduleValue( m_ColdTempSchedule );
			} else { // If no ColdTempSchedule, use the mains temperature
				m_ColdTemp = WaterMainsTemp;
			}

			if ( m_HotTempSchedule > 0 ) {
				m_HotTemp = GetCurrentScheduleValue( m_HotTempSchedule );
			} else { // If no HotTempSchedule, use all cold water
				m_HotTemp = m_ColdTemp;
			}
		}

		if ( m_TargetTempSchedule > 0 ) {
			m_TargetTemp = GetCurrentScheduleValue( m_TargetTempSchedule );
		} else { // If no TargetTempSchedule, use all hot water
			m_TargetTemp = m_HotTemp;
		}

		// Get the requested total flow rate
		// 11-17-2006 BG Added multipliers in next block
		if ( m_ZoneNum > 0 ) {
			if ( m_FlowRateFracSchedule > 0 ) {
				m_TotalVolFlowRate = m_PeakVolFlowRate * GetCurrentScheduleValue( m_FlowRateFracSchedule ) * Zone( m_ZoneNum ).Multiplier * Zone( m_ZoneNum ).ListMultiplier;
			} else {
				m_TotalVolFlowRate = m_PeakVolFlowRate * Zone( m_ZoneNum ).Multiplier * Zone( m_ZoneNum ).ListMultiplier;
			}
		} else {
			if ( m_FlowRateFracSchedule > 0 ) {
				m_TotalVolFlowRate = m_PeakVolFlowRate * GetCurrentScheduleValue( m_FlowRateFracSchedule );
			} else {
				m_TotalVolFlowRate = m_PeakVolFlowRate;
			}
		}

		m_TotalMassFlowRate = m_TotalVolFlowRate * RhoH2O( InitConvTemp );

		// Calculate hot and cold water mixing at the tap
		if ( m_TotalMassFlowRate > 0.0 ) {
			// Calculate the flow rates needed to meet the target temperature
			if ( m_HotTemp == m_ColdTemp ) { // Avoid divide by zero
				// There is no hot water
				m_HotMassFlowRate = 0.0;

				// Need a special case for HotTemp < ColdTemp, due to bad user input  (but could happen in a plant loop accidentally)

			} else if ( m_TargetTemp > m_HotTemp ) {
				m_HotMassFlowRate = m_TotalMassFlowRate;

			} else {
				m_HotMassFlowRate = m_TotalMassFlowRate * ( m_TargetTemp - m_ColdTemp ) / ( m_HotTemp - m_ColdTemp );
			}

			if ( m_HotMassFlowRate < 0.0 ) {
				// Target temp is colder than the cold water temp; don't allow colder
				m_HotMassFlowRate = 0.0;
			}

			m_ColdMassFlowRate = m_TotalMassFlowRate - m_HotMassFlowRate;

			if ( m_ColdMassFlowRate < 0.0 ) m_ColdMassFlowRate = 0.0;

			m_MixedTemp = ( m_ColdMassFlowRate * m_ColdTemp + m_HotMassFlowRate * m_HotTemp ) / m_TotalMassFlowRate;
		} else {
			m_HotMassFlowRate = 0.0;
			m_ColdMassFlowRate = 0.0;
			m_MixedTemp = m_TargetTemp;
		}

	} // WaterEquipment_T::CalcEquipmentFlowRates( )


	void
	WaterEquipment_T::CalcEquipmentDrainTemp( )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate drainwater temperature and heat and moisture gains to zone.

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using Psychrometrics::CPHW;
		using Psychrometrics::PsyWFnTdbRhPb;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyHfgAirFnWTdb;
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZoneAirHumRat;
		using DataHeatBalance::Zone;
		using DataEnvironment::OutBaroPress;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ZoneMAT;
		Real64 ZoneHumRat;
		Real64 ZoneHumRatSat;
		Real64 RhoAirDry;
		Real64 ZoneMassMax;
		Real64 FlowMassMax;
		Real64 MoistureMassMax;

		static std::string const RoutineName( "CalcEquipmentDrainTemp" );

		// FLOW:

		m_SensibleRate = 0.0;
		m_SensibleEnergy = 0.0;
		m_LatentRate = 0.0;
		m_LatentEnergy = 0.0;

		if ( ( m_ZoneNum == 0 ) || ( m_TotalMassFlowRate == 0.0 ) ) {
			m_DrainTemp = m_MixedTemp;
			m_DrainMassFlowRate = m_TotalMassFlowRate;

		} else {
			ZoneMAT = MAT( m_ZoneNum );

			if ( m_SensibleFracSchedule == 0 ) {
				m_SensibleRate = 0.0;
				m_SensibleEnergy = 0.0;
			} else {
				m_SensibleRate = GetCurrentScheduleValue( m_SensibleFracSchedule ) * m_TotalMassFlowRate * CPHW( InitConvTemp ) * ( m_MixedTemp - ZoneMAT );
				m_SensibleEnergy = m_SensibleRate * TimeStepSys * SecInHour;
			}

			if ( m_LatentFracSchedule == 0 ) {
				m_LatentRate = 0.0;
				m_LatentEnergy = 0.0;
			} else {
				ZoneHumRat = ZoneAirHumRat( m_ZoneNum );
				ZoneHumRatSat = PsyWFnTdbRhPb( ZoneMAT, 1.0, OutBaroPress, RoutineName ); // Humidratio at 100% relative humidity
				RhoAirDry = PsyRhoAirFnPbTdbW( OutBaroPress, ZoneMAT, 0.0 );

				ZoneMassMax = ( ZoneHumRatSat - ZoneHumRat ) * RhoAirDry * Zone( m_ZoneNum ).Volume; // Max water that can be evaporated to zone
				FlowMassMax = m_TotalMassFlowRate * TimeStepSys * SecInHour; // Max water in flow
				MoistureMassMax = min( ZoneMassMax, FlowMassMax );

				m_MoistureMass = GetCurrentScheduleValue( m_LatentFracSchedule ) * MoistureMassMax;
				m_MoistureRate = m_MoistureMass / ( TimeStepSys * SecInHour );

				m_LatentRate = m_MoistureRate * PsyHfgAirFnWTdb( ZoneHumRat, ZoneMAT );
				m_LatentEnergy = m_LatentRate * TimeStepSys * SecInHour;
			}

			m_DrainMassFlowRate = m_TotalMassFlowRate - m_MoistureRate;

			if ( m_DrainMassFlowRate == 0.0 ) {
				m_DrainTemp = m_MixedTemp;
			} else {
				m_DrainTemp = ( m_TotalMassFlowRate * CPHW( InitConvTemp ) * m_MixedTemp - m_SensibleRate - m_LatentRate ) / ( m_DrainMassFlowRate * CPHW( InitConvTemp ) );
			}
		}

	} // WaterEquipment_T::CalcEquipmentDrainTemp()

	// WaterConnections_T implementation
	
	// MODULE VARIABLE DECLARATIONS:

	// Functions

	Array1D< WaterConnections_T > WaterConnections_T::sm_instances;
	int WaterConnections_T::sm_numInstances = 0;
	bool WaterConnections_T::sm_GetInputFlag = true;
	
	void
	clear_state()
	{
		WaterConnections_T::sm_GetInputFlag = true;
		WaterEquipment_T::sm_instances.deallocate();
		WaterEquipment_T::sm_numInstances = 0;
		WaterConnections_T::sm_instances.deallocate();
		WaterConnections_T::sm_numInstances = 0;
	}


	PlantComponent *
	WaterConnections_T::factory( const std::string objectName )
	{
		if ( sm_GetInputFlag ) {
			GetInput();
			sm_GetInputFlag = false;
		}

		for ( WaterConnections_T & waterConn : sm_instances ) {
			if ( waterConn.Name == objectName )
				return &waterConn;
		}
		
		ShowFatalError( "WaterConnections_T::factory: Unit not found=" + objectName );
		return nullptr;
	} // factory( )

	void
	WaterConnections_T::simulate( const PlantLocation & EP_UNUSED( calledFromLocation ),
					bool const FirstHVACIteration,
					Real64 & EP_UNUSED( CurLoad ) )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith March 2010, Demand Side Update
		//       DATE WRITTEN   August 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Plant sim call for plant loop connected water use and connections
		// (based on SimulateWaterUse by P. Ellis)

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIterations( 100 );
		Real64 const Tolerance( 0.1 ); // Make input?

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  INTEGER :: WaterEquipNum
		static int MaxIterationsErrorCount;
		static bool MyEnvrnFlag( true );

		// FLOW:

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			MaxIterationsErrorCount = 0;
			if ( WaterEquipment_T::sm_numInstances > 0 ) {
				for ( WaterEquipment_T & waterEquip : WaterEquipment_T::sm_instances )
					waterEquip.reset();
			}

			if ( sm_numInstances > 0 ) {
				for ( WaterConnections_T & waterConn : sm_instances ) waterConn.m_TotalMassFlowRate = 0.0;
			}

			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		InitConnections( );

		int NumIteration = 0;

		while ( true ) {
			++NumIteration;

			CalcConnectionsFlowRates( FirstHVACIteration );
			CalcConnectionsDrainTemp( );
			CalcConnectionsHeatRecovery( );

			if ( m_TempError < Tolerance ) {
				break;
			} else if ( NumIteration > MaxIterations ) {
				if ( ! WarmupFlag ) {
					if ( m_MaxIterationsErrorIndex == 0 ) {
						ShowWarningError( "WaterUse:Connections = " + Name + ":  Heat recovery temperature did not converge" );
						ShowContinueErrorTimeStamp( "" );
					}
					ShowRecurringWarningErrorAtEnd( "WaterUse:Connections = " + Name + ":  Heat recovery temperature did not converge", m_MaxIterationsErrorIndex );
				}
				break;
			}

		} // WHILE

		UpdateWaterConnections( );
		ReportWaterUse( );

	}

	void
	WaterConnections_T::GetInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using ScheduleManager::GetScheduleIndex;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using namespace DataLoopNode;
		using namespace DataHeatBalance;
		using WaterManager::SetupTankSupplyComponent;
		using WaterManager::SetupTankDemandComponent;
		using Psychrometrics::RhoH2O;
		using PlantUtilities::RegisterPlantCompDesignFlow;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool ErrorsFound = false; // Set to true if errors in input, fatal at end of routine

		cCurrentModuleObject = "WaterUse:Equipment";
		WaterEquipment_T::sm_numInstances = GetNumObjectsFound( cCurrentModuleObject );

		if ( WaterEquipment_T::sm_numInstances > 0 ) {
			WaterEquipment_T::sm_instances.allocate( WaterEquipment_T::sm_numInstances );

			for ( int WaterEquipNum = 1; WaterEquipNum <= WaterEquipment_T::sm_numInstances; ++WaterEquipNum ) {
				int NumAlphas, NumNumbers; // Number of Alphas and Numbers for each GetObjectItemCall
				int IOStatus;
				GetObjectItem( cCurrentModuleObject, WaterEquipNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				WaterEquipment_T & waterEquip = WaterEquipment_T::sm_instances( WaterEquipNum );

				bool IsNotOK = false, IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), WaterEquipment_T::sm_instances, WaterEquipNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				waterEquip.Name = cAlphaArgs( 1 );

				waterEquip.m_EndUseSubcatName = cAlphaArgs( 2 );

				waterEquip.m_PeakVolFlowRate = rNumericArgs( 1 );

				if ( ( NumAlphas > 2 ) && ( ! lAlphaFieldBlanks( 3 ) ) ) {
					waterEquip.m_FlowRateFracSchedule = GetScheduleIndex( cAlphaArgs( 3 ) );
					// If no FlowRateFracSchedule, fraction defaults to 1.0

					if ( waterEquip.m_FlowRateFracSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + '=' + cAlphaArgs( 3 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 3 ) && ( ! lAlphaFieldBlanks( 4 ) ) ) {
					waterEquip.m_TargetTempSchedule = GetScheduleIndex( cAlphaArgs( 4 ) );

					if ( waterEquip.m_TargetTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 4 ) && ( ! lAlphaFieldBlanks( 5 ) ) ) {
					waterEquip.m_HotTempSchedule = GetScheduleIndex( cAlphaArgs( 5 ) );
					// If no HotTempSchedule, there is no hot water.
					// HotTempSchedule is ignored if connected to a plant loop via WATER USE CONNECTIONS

					if ( waterEquip.m_HotTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 5 ) && ( ! lAlphaFieldBlanks( 6 ) ) ) {
					waterEquip.m_ColdTempSchedule = GetScheduleIndex( cAlphaArgs( 6 ) );
					// If no ColdTempSchedule, temperatures will be calculated by WATER MAINS TEMPERATURES object

					if ( waterEquip.m_ColdTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 6 ) && ( ! lAlphaFieldBlanks( 7 ) ) ) {
					waterEquip.m_ZoneNum = FindItemInList( cAlphaArgs( 7 ), Zone );

					if ( waterEquip.m_ZoneNum == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 7 ) && ( ! lAlphaFieldBlanks( 8 ) ) ) {
					waterEquip.m_SensibleFracSchedule = GetScheduleIndex( cAlphaArgs( 8 ) );

					if ( waterEquip.m_SensibleFracSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( NumAlphas > 8 ) && ( ! lAlphaFieldBlanks( 9 ) ) ) {
					waterEquip.m_LatentFracSchedule = GetScheduleIndex( cAlphaArgs( 9 ) );

					if ( waterEquip.m_LatentFracSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + cAlphaArgs( 9 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

			} // WaterEquipNum

			if ( ErrorsFound ) ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );

		}

		cCurrentModuleObject = "WaterUse:Connections";
		sm_numInstances = GetNumObjectsFound( cCurrentModuleObject );

		if ( sm_numInstances > 0 ) {
			sm_instances.allocate( sm_numInstances );

			for ( int WaterConnNum = 1; WaterConnNum <= sm_numInstances; ++WaterConnNum ) {
				int NumAlphas, NumNumbers; // Number of Alphas and Numbers for each GetObjectItemCall
				int IOStatus;
				GetObjectItem( cCurrentModuleObject, WaterConnNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				WaterConnections_T & waterConn = sm_instances( WaterConnNum );

				bool IsNotOK = false, IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), sm_instances, WaterConnNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				waterConn.Name = cAlphaArgs( 1 );

				if ( ( ! lAlphaFieldBlanks( 2 ) ) || ( ! lAlphaFieldBlanks( 3 ) ) ) {
					waterConn.m_InletNode = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
					waterConn.m_OutletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );

					// Check plant connections
					TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "DHW Nodes" );
				} else {
					// If no plant nodes are connected, simulate in stand-alone mode.
					waterConn.m_StandAlone = true;
				}

				if ( ! lAlphaFieldBlanks( 4 ) ) {
					SetupTankDemandComponent( waterConn.Name, cCurrentModuleObject, cAlphaArgs( 4 ), ErrorsFound, waterConn.m_SupplyTankNum, waterConn.m_TankDemandID );
				}

				if ( ! lAlphaFieldBlanks( 5 ) ) {
					SetupTankSupplyComponent( waterConn.Name, cCurrentModuleObject, cAlphaArgs( 5 ), ErrorsFound, waterConn.m_RecoveryTankNum, waterConn.m_TankSupplyID );
				}

				if ( ! lAlphaFieldBlanks( 6 ) ) {
					waterConn.m_HotTempSchedule = GetScheduleIndex( cAlphaArgs( 6 ) );
					// If no HotTempSchedule, there is no hot water.
					// HotTempSchedule is ignored if connected to a plant loop via WATER USE CONNECTIONS

					if ( waterConn.m_HotTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + '=' + cAlphaArgs( 6 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ! lAlphaFieldBlanks( 7 ) ) {
					waterConn.m_ColdTempSchedule = GetScheduleIndex( cAlphaArgs( 7 ) );
					// If no ColdTempSchedule, temperatures will be calculated by WATER MAINS TEMPERATURES object

					if ( waterConn.m_ColdTempSchedule == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}

				if ( ( ! lAlphaFieldBlanks( 8 ) ) && ( cAlphaArgs( 8 ) != "NONE" ) ) {
					waterConn.m_HeatRecovery = true;

					{ auto const SELECT_CASE_var( cAlphaArgs( 8 ) );
					if ( SELECT_CASE_var == "IDEAL" ) {
						waterConn.m_HeatRecoveryHX = HeatRecoveryHX_T::Ideal;
					} else if ( SELECT_CASE_var == "COUNTERFLOW" ) {
						waterConn.m_HeatRecoveryHX = HeatRecoveryHX_T::CounterFlow;
					} else if ( SELECT_CASE_var == "CROSSFLOW" ) {
						waterConn.m_HeatRecoveryHX = HeatRecoveryHX_T::CrossFlow;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + '=' + cAlphaArgs( 8 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}}

					{ auto const SELECT_CASE_var( cAlphaArgs( 9 ) );
					if ( SELECT_CASE_var == "PLANT" ) {
						waterConn.m_HeatRecoveryConfig = HeatRecoveryConfig_T::Plant;
					} else if ( SELECT_CASE_var == "EQUIPMENT" ) {
						waterConn.m_HeatRecoveryConfig = HeatRecoveryConfig_T::Equipment;
					} else if ( SELECT_CASE_var == "PLANTANDEQUIPMENT" ) {
						waterConn.m_HeatRecoveryConfig = HeatRecoveryConfig_T::PlantAndEquip;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 9 ) + '=' + cAlphaArgs( 9 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}}
				}

				waterConn.m_HXUA = rNumericArgs( 1 );

				waterConn.m_localWaterEquipment.allocate( NumAlphas - 9 );

				for ( int AlphaNum = 10; AlphaNum <= NumAlphas; ++AlphaNum ) {
					int WaterEquipNum = FindItemInList( cAlphaArgs( AlphaNum ), WaterEquipment_T::sm_instances );

					if ( WaterEquipNum == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( AlphaNum ) + '=' + cAlphaArgs( AlphaNum ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					} else {
						WaterEquipment_T & waterEquip = WaterEquipment_T::sm_instances( WaterEquipNum );
						if ( waterEquip.m_Connections > 0 ) {
							ShowSevereError( cCurrentModuleObject + " = " + cAlphaArgs( 1 ) + ":  WaterUse:Equipment = " + cAlphaArgs( AlphaNum ) + " is already referenced by another object." );
							ErrorsFound = true;
						} else {
							waterEquip.m_Connections = WaterConnNum;

							++waterConn.m_NumWaterEquipment;
							waterConn.m_localWaterEquipment( waterConn.m_NumWaterEquipment ) = WaterEquipNum;

							waterConn.m_PeakVolFlowRate += waterEquip.m_PeakVolFlowRate; // this does not include possible multipliers
						}
					}
				}

			} // for WaterConnNum

			if ( ErrorsFound ) ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
		} // if sm_numInstances > 0

		// determine connection's peak mass flow rates.
		if ( sm_numInstances > 0 ) {
			for ( WaterConnections_T & waterConn : sm_instances ) {
				waterConn.m_PeakMassFlowRate = 0.0;
				for ( int thisWaterEquipNum : waterConn.m_localWaterEquipment ) {
					WaterEquipment_T & thisWaterEquip = WaterEquipment_T::sm_instances( thisWaterEquipNum );
					if ( thisWaterEquip.m_ZoneNum > 0 ) {
						waterConn.m_PeakMassFlowRate += thisWaterEquip.m_PeakVolFlowRate * RhoH2O( InitConvTemp ) * Zone( thisWaterEquip.m_ZoneNum ).Multiplier * Zone( thisWaterEquip.m_ZoneNum ).ListMultiplier;
					} else { // can't have multipliers
						waterConn.m_PeakMassFlowRate += thisWaterEquip.m_PeakVolFlowRate * RhoH2O( InitConvTemp );
					}
				}
				RegisterPlantCompDesignFlow( waterConn.m_InletNode, waterConn.m_PeakMassFlowRate / RhoH2O( InitConvTemp ) );
			}
		}

		// Setup EQUIPMENT report variables (now that connections have been established)
		// CurrentModuleObject='WaterUse:Equipment'
		for ( WaterEquipment_T & waterEquip : WaterEquipment_T::sm_instances ) {

			SetupOutputVariable( "Water Use Equipment Hot Water Mass Flow Rate [kg/s]", waterEquip.m_HotMassFlowRate, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Cold Water Mass Flow Rate [kg/s]", waterEquip.m_ColdMassFlowRate, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Total Mass Flow Rate [kg/s]", waterEquip.m_TotalMassFlowRate, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Hot Water Volume Flow Rate [m3/s]", waterEquip.m_HotVolFlowRate, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Cold Water Volume Flow Rate [m3/s]", waterEquip.m_ColdVolFlowRate, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Total Volume Flow Rate [m3/s]", waterEquip.m_TotalVolFlowRate, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Hot Water Volume [m3]", waterEquip.m_HotVolume, "System", "Sum", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Cold Water Volume [m3]", waterEquip.m_ColdVolume, "System", "Sum", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Total Volume [m3]", waterEquip.m_TotalVolume, "System", "Sum", waterEquip.Name, _, "Water", "WATERSYSTEMS", waterEquip.m_EndUseSubcatName, "Plant" );
			SetupOutputVariable( "Water Use Equipment Mains Water Volume [m3]", waterEquip.m_TotalVolume, "System", "Sum", waterEquip.Name, _, "MainsWater", "WATERSYSTEMS", waterEquip.m_EndUseSubcatName, "Plant" );

			SetupOutputVariable( "Water Use Equipment Hot Water Temperature [C]", waterEquip.m_HotTemp, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Cold Water Temperature [C]", waterEquip.m_ColdTemp, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Target Water Temperature [C]", waterEquip.m_TargetTemp, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Mixed Water Temperature [C]", waterEquip.m_MixedTemp, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Drain Water Temperature [C]", waterEquip.m_DrainTemp, "System", "Average", waterEquip.Name );

			SetupOutputVariable( "Water Use Equipment Heating Rate [W]", waterEquip.m_Power, "System", "Average", waterEquip.Name );

			if ( waterEquip.m_Connections == 0 ) {
				SetupOutputVariable( "Water Use Equipment Heating Energy [J]", waterEquip.m_Energy, "System", "Sum", waterEquip.Name, _, "DISTRICTHEATING", "WATERSYSTEMS", waterEquip.m_EndUseSubcatName, "Plant" );

			} else if ( sm_instances( waterEquip.m_Connections ).m_StandAlone ) {
				SetupOutputVariable( "Water Use Equipment Heating Energy [J]", waterEquip.m_Energy, "System", "Sum", waterEquip.Name, _, "DISTRICTHEATING", "WATERSYSTEMS", waterEquip.m_EndUseSubcatName, "Plant" );

			} else { // The EQUIPMENT is coupled to a plant loop via a CONNECTIONS object
				SetupOutputVariable( "Water Use Equipment Heating Energy [J]", waterEquip.m_Energy, "System", "Sum", waterEquip.Name, _, "ENERGYTRANSFER", "WATERSYSTEMS", waterEquip.m_EndUseSubcatName, "Plant" );
			}

			if ( waterEquip.m_ZoneNum > 0 ) {
				SetupOutputVariable( "Water Use Equipment Zone Sensible Heat Gain Rate [W]", waterEquip.m_SensibleRate, "System", "Average", waterEquip.Name );
				SetupOutputVariable( "Water Use Equipment Zone Sensible Heat Gain Energy [J]", waterEquip.m_SensibleEnergy, "System", "Sum", waterEquip.Name );

				SetupOutputVariable( "Water Use Equipment Zone Latent Gain Rate [W]", waterEquip.m_LatentRate, "System", "Average", waterEquip.Name );
				SetupOutputVariable( "Water Use Equipment Zone Latent Gain Energy [J]", waterEquip.m_LatentEnergy, "System", "Sum", waterEquip.Name );

				SetupOutputVariable( "Water Use Equipment Zone Moisture Gain Mass Flow Rate [kg/s]", waterEquip.m_MoistureRate, "System", "Average", waterEquip.Name );
				SetupOutputVariable( "Water Use Equipment Zone Moisture Gain Mass [kg]", waterEquip.m_MoistureMass, "System", "Sum", waterEquip.Name );

				SetupZoneInternalGain( waterEquip.m_ZoneNum, "WaterUse:Equipment", waterEquip.Name, IntGainTypeOf_WaterUseEquipment, waterEquip.m_SensibleRateNoMultiplier, _, _, waterEquip.m_LatentRateNoMultiplier );

			}

		} // waterEquip

		// Setup CONNECTIONS report variables (don't put any on meters; they are metered at WATER USE EQUIPMENT level)
		// CurrentModuleObject='WaterUse:Connections'
		for ( WaterConnections_T & waterConn : sm_instances ) {

			SetupOutputVariable( "Water Use Connections Hot Water Mass Flow Rate [kg/s]", waterConn.m_HotMassFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Cold Water Mass Flow Rate [kg/s]", waterConn.m_ColdMassFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Total Mass Flow Rate [kg/s]", waterConn.m_TotalMassFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Drain Water Mass Flow Rate [kg/s]", waterConn.m_DrainMassFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Heat Recovery Mass Flow Rate [kg/s]", waterConn.m_RecoveryMassFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Hot Water Volume Flow Rate [m3/s]", waterConn.m_HotVolFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Cold Water Volume Flow Rate [m3/s]", waterConn.m_ColdVolFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Total Volume Flow Rate [m3/s]", waterConn.m_TotalVolFlowRate, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Hot Water Volume [m3]", waterConn.m_HotVolume, "System", "Sum", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Cold Water Volume [m3]", waterConn.m_ColdVolume, "System", "Sum", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Total Volume [m3]", waterConn.m_TotalVolume, "System", "Sum", waterConn.Name ); //, &
			// ResourceTypeKey='Water', EndUseKey='DHW', EndUseSubKey=EndUseSubcategoryName, GroupKey='Plant')
			// tHIS WAS double counting

			SetupOutputVariable( "Water Use Connections Hot Water Temperature [C]", waterConn.m_HotTemp, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Cold Water Temperature [C]", waterConn.m_ColdTemp, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Drain Water Temperature [C]", waterConn.m_DrainTemp, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Return Water Temperature [C]", waterConn.m_ReturnTemp, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Waste Water Temperature [C]", waterConn.m_WasteTemp, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Heat Recovery Water Temperature [C]", waterConn.m_RecoveryTemp, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Heat Recovery Effectiveness []", waterConn.m_Effectiveness, "System", "Average", waterConn.Name );

			SetupOutputVariable( "Water Use Connections Heat Recovery Rate [W]", waterConn.m_RecoveryRate, "System", "Average", waterConn.Name );
			SetupOutputVariable( "Water Use Connections Heat Recovery Energy [J]", waterConn.m_RecoveryEnergy, "System", "Sum", waterConn.Name );
			// Does this go on a meter?

			// To do:  Add report variable for starved flow when tank can't deliver?

			if ( ! waterConn.m_StandAlone ) {
				SetupOutputVariable( "Water Use Connections Plant Hot Water Energy [J]", waterConn.m_Energy, "System", "Sum", waterConn.Name, _, "PLANTLOOPHEATINGDEMAND", "WATERSYSTEMS", _, "Plant" );
			}

		} // waterConn

	} // WaterConnections_T::GetInput()

	void
	WaterConnections_T::InitConnections( )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       Brent Griffith 2010, demand side update
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using DataGlobals::DoingSizing;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataLoopNode::Node;
		using DataEnvironment::WaterMainsTemp;
		using DataWater::WaterStorage;
		using DataHeatBalance::Zone;
		using DataPlant::PlantLoop;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_WaterUseConnection;
		using PlantUtilities::InitComponentNodes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool errFlag;

		if ( m_SetLoopIndexFlag ) { // Set to true in WaterConnections_T constructor
			if ( allocated( PlantLoop ) && ! m_StandAlone ) { //DSU
				errFlag = false;
				ScanPlantLoopsForObject( Name, TypeOf_WaterUseConnection, m_PlantLoopNum, m_PlantLoopSide, m_PlantLoopBranchNum, m_PlantLoopCompNum, _, _, _, _, _, errFlag ); //DSU | DSU | DSU | DSU | DSU | DSU | DSU
				if ( errFlag ) { //DSU
					ShowFatalError( "InitConnections: Program terminated due to previous condition(s)." ); //DSU
				} //DSU
				m_SetLoopIndexFlag = false; //DSU
			} //DSU
			if ( m_StandAlone ) m_SetLoopIndexFlag = false;
		}

		// Set the cold water temperature
		if ( m_SupplyTankNum > 0 ) {
			m_ColdSupplyTemp = WaterStorage( m_SupplyTankNum ).Twater;

		} else if ( m_ColdTempSchedule > 0 ) {
			m_ColdSupplyTemp = GetCurrentScheduleValue( m_ColdTempSchedule );

		} else {
			m_ColdSupplyTemp = WaterMainsTemp;
		}

		// Initially set ColdTemp to the ColdSupplyTemp; with heat recovery, ColdTemp will change during iteration
		m_ColdTemp = m_ColdSupplyTemp;

		// Set the hot water temperature
		if ( m_StandAlone ) {
			if ( m_HotTempSchedule > 0 ) {
				m_HotTemp = GetCurrentScheduleValue( m_HotTempSchedule );
			} else {
				// If no HotTempSchedule, use all cold water
				m_HotTemp = m_ColdTemp;
			}

		} else {
			if ( BeginEnvrnFlag && m_Init ) {
				// Clear node initial conditions
				if ( m_InletNode > 0 && m_OutletNode > 0 ) {
					InitComponentNodes( 0.0, m_PeakMassFlowRate, m_InletNode, m_OutletNode, m_PlantLoopNum, m_PlantLoopSide, m_PlantLoopBranchNum, m_PlantLoopCompNum );

					m_ReturnTemp = Node( m_InletNode ).Temp;
				}

				m_Init = false;
			}

			if ( ! BeginEnvrnFlag ) m_Init = true;

			if ( m_InletNode > 0 ) {
				if ( ! DoingSizing ) {
					m_HotTemp = Node( m_InletNode ).Temp;
				} else {
					// plant loop will not be running so need a value here.
					// should change to use tank setpoint but water use connections don't have knowledge of the tank they are fed by
					m_HotTemp = 60.0;
				}
			}
		}

	} // WaterConnections_T::InitConnections()

	void
	WaterConnections_T::CalcConnectionsFlowRates(
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate summed values for WATER USE CONNECTIONS (to prepare to request flow from plant, and for reporting).

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using DataLoopNode::Node;
		using Psychrometrics::RhoH2O;
		using DataWater::WaterStorage;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FLOW:
		m_ColdMassFlowRate = 0.0;
		m_HotMassFlowRate = 0.0;

		for ( int WaterEquipNum : m_localWaterEquipment ) {
			WaterEquipment_T & waterEquip = WaterEquipment_T::sm_instances( WaterEquipNum );
			waterEquip.CalcEquipmentFlowRates( );

			m_ColdMassFlowRate += waterEquip.m_ColdMassFlowRate;
			m_HotMassFlowRate += waterEquip.m_HotMassFlowRate;
		} // Loop

		m_TotalMassFlowRate = m_ColdMassFlowRate + m_HotMassFlowRate;

		if ( ! m_StandAlone ) { // Interact with the plant loop
			if ( m_InletNode > 0 ) {
				if ( FirstHVACIteration ) {
					// Request the mass flow rate from the demand side manager
					//        Node(InletNode)%MassFlowRate = WaterConnections(WaterConnNum)%HotMassFlowRate
					//        Node(InletNode)%MassFlowRateMaxAvail = WaterConnections(WaterConnNum)%PeakMassFlowRate
					//        Node(InletNode)%MassFlowRateMinAvail = 0.0D0
					SetComponentFlowRate( m_HotMassFlowRate, m_InletNode, m_OutletNode, m_PlantLoopNum, m_PlantLoopSide, m_PlantLoopBranchNum, m_PlantLoopCompNum );

				} else {
					Real64 DesiredHotWaterMassFlow = m_HotMassFlowRate;
					SetComponentFlowRate( DesiredHotWaterMassFlow, m_InletNode, m_OutletNode, m_PlantLoopNum, m_PlantLoopSide, m_PlantLoopBranchNum, m_PlantLoopCompNum );
					//DSU3   Node(InletNode)%MassFlowRate = MIN(WaterConnections(WaterConnNum)%HotMassFlowRate, Node(InletNode)%MassFlowRateMaxAvail)
					//DSU3   Node(InletNode)%MassFlowRate = MAX(WaterConnections(WaterConnNum)%HotMassFlowRate, Node(InletNode)%MassFlowRateMinAvail)
					// readjust if more than actual available mass flow rate determined by the demand side manager
					if ( ( m_HotMassFlowRate != DesiredHotWaterMassFlow ) && ( m_HotMassFlowRate > 0.0 ) ) { // plant didn't give what was asked for

						//DSU3   Node(InletNode)%MassFlowRate = Node(InletNode)%MassFlowRateMaxAvail

						Real64 AvailableFraction = DesiredHotWaterMassFlow / m_HotMassFlowRate;

						//DSU3    WaterConnections(WaterConnNum)%HotMassFlowRate = Node(InletNode)%MassFlowRateMaxAvail
						m_ColdMassFlowRate = m_TotalMassFlowRate - m_HotMassFlowRate; // Preserve the total mass flow rate

						// Proportionally reduce hot water and increase cold water for all WATER USE EQUIPMENT
						for ( int WaterEquipNum : m_localWaterEquipment ) {
							WaterEquipment_T & waterEquip = WaterEquipment_T::sm_instances( WaterEquipNum );
							// Recalculate flow rates for water equipment within connection
							waterEquip.m_HotMassFlowRate *= AvailableFraction;
							waterEquip.m_ColdMassFlowRate = waterEquip.m_TotalMassFlowRate - waterEquip.m_HotMassFlowRate;

							// Recalculate mixed water temperature
							if ( waterEquip.m_TotalMassFlowRate > 0.0 ) {
								waterEquip.m_MixedTemp = ( waterEquip.m_ColdMassFlowRate * waterEquip.m_ColdTemp + waterEquip.m_HotMassFlowRate * waterEquip.m_HotTemp ) / waterEquip.m_TotalMassFlowRate;
							} else {
								waterEquip.m_MixedTemp = waterEquip.m_TargetTemp;
							}
						} // Loop
					}
				}
			}
		}

		if ( m_SupplyTankNum > 0 ) {
			// Set the demand request for supply water from water storage tank
			m_ColdVolFlowRate = m_ColdMassFlowRate / RhoH2O( InitConvTemp );
			WaterStorage( m_SupplyTankNum ).VdotRequestDemand( m_TankDemandID ) = m_ColdVolFlowRate;

			// Check if cold flow rate should be starved by restricted flow from tank
			// Currently, the tank flow is not really starved--water continues to flow at the tank water temperature
			// But the user can see the error by comparing report variables for TankVolFlowRate < ColdVolFlowRate
			m_TankVolFlowRate = WaterStorage( m_SupplyTankNum ).VdotAvailDemand( m_TankDemandID );
			m_TankMassFlowRate = m_TankVolFlowRate * RhoH2O( InitConvTemp );
		}

	} // WaterConnections_T::CalcConnectionsFlowRates()

	void
	WaterConnections_T::CalcConnectionsDrainTemp( )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using Psychrometrics::RhoH2O;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FLOW:
		m_DrainMassFlowRate = 0.0;
		Real64 MassFlowTempSum = 0.0;

		for ( int waterEquipNum : m_localWaterEquipment ) {
			WaterEquipment_T & waterEquip = WaterEquipment_T::sm_instances( waterEquipNum );
			waterEquip.CalcEquipmentDrainTemp( );

			m_DrainMassFlowRate += waterEquip.m_DrainMassFlowRate;
			MassFlowTempSum += waterEquip.m_DrainMassFlowRate * waterEquip.m_DrainTemp;
		} // Loop

		if ( m_DrainMassFlowRate > 0.0 ) {
			m_DrainTemp = MassFlowTempSum / m_DrainMassFlowRate;
		} else {
			m_DrainTemp = m_HotTemp;
		}

		m_DrainVolFlowRate = m_DrainMassFlowRate * RhoH2O( InitConvTemp );

	} // WaterConnections_T::CalcConnectionsDrainTemp()

	void
	WaterConnections_T::CalcConnectionsHeatRecovery( )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate drainwater heat recovery

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using Psychrometrics::CPHW;
		//unused0909  USE DataEnvironment, ONLY: WaterMainsTemp
		using DataWater::WaterStorage;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FLOW:
		if ( ! m_HeatRecovery ) {
			m_RecoveryTemp = m_ColdSupplyTemp;
			m_ReturnTemp = m_ColdSupplyTemp;
			m_WasteTemp = m_DrainTemp;

		} else if ( m_TotalMassFlowRate == 0.0 ) {
			m_Effectiveness = 0.0;
			m_RecoveryRate = 0.0;
			m_RecoveryTemp = m_ColdSupplyTemp;
			m_ReturnTemp = m_ColdSupplyTemp;
			m_WasteTemp = m_DrainTemp;

		} else { // WaterConnections(WaterConnNum)%TotalMassFlowRate > 0.0

			{ auto const SELECT_CASE_var( m_HeatRecoveryConfig );
				if ( SELECT_CASE_var == HeatRecoveryConfig_T::Plant ) {
					m_RecoveryMassFlowRate = m_HotMassFlowRate;
				} else if ( SELECT_CASE_var == HeatRecoveryConfig_T::Equipment ) {
					m_RecoveryMassFlowRate = m_ColdMassFlowRate;
				} else if ( SELECT_CASE_var == HeatRecoveryConfig_T::PlantAndEquip ) {
					m_RecoveryMassFlowRate = m_TotalMassFlowRate;
				}
			}

			Real64 HXCapacityRate = CPHW( InitConvTemp ) * m_RecoveryMassFlowRate;
			Real64 DrainCapacityRate = CPHW( InitConvTemp ) * m_DrainMassFlowRate;
			Real64 MinCapacityRate = min( DrainCapacityRate, HXCapacityRate );

			{ auto const SELECT_CASE_var( m_HeatRecoveryHX );
				if ( SELECT_CASE_var == HeatRecoveryHX_T::Ideal ) {
					m_Effectiveness = 1.0;
					
				} else if ( SELECT_CASE_var == HeatRecoveryHX_T::CounterFlow ) { // Unmixed
					Real64 CapacityRatio = MinCapacityRate / max( DrainCapacityRate, HXCapacityRate );
					Real64 NTU = m_HXUA / MinCapacityRate;
					if ( CapacityRatio == 1.0 ) {
						m_Effectiveness = NTU / ( 1.0 + NTU );
					} else {
					Real64 ExpVal = std::exp( -NTU * ( 1.0 - CapacityRatio ) );
					m_Effectiveness = ( 1.0 - ExpVal ) / ( 1.0 - CapacityRatio * ExpVal );
					}

				} else if ( SELECT_CASE_var == HeatRecoveryHX_T::CrossFlow ) { // Unmixed
					Real64 CapacityRatio = MinCapacityRate / max( DrainCapacityRate, HXCapacityRate );
					Real64 NTU = m_HXUA / MinCapacityRate;
					m_Effectiveness = 1.0 - std::exp( ( std::pow( NTU, 0.22 ) / CapacityRatio ) * ( std::exp( -CapacityRatio * std::pow( NTU, 0.78 ) ) - 1.0 ) );
				}
			}

			m_RecoveryRate = m_Effectiveness * MinCapacityRate * ( m_DrainTemp - m_ColdSupplyTemp );

			m_RecoveryTemp = m_ColdSupplyTemp + m_RecoveryRate / ( CPHW( InitConvTemp ) * m_TotalMassFlowRate );

			m_WasteTemp = m_DrainTemp - m_RecoveryRate / ( CPHW( InitConvTemp ) * m_TotalMassFlowRate );

			if ( m_RecoveryTankNum > 0 ) {
				WaterStorage( m_RecoveryTankNum ).VdotAvailSupply( m_TankSupplyID ) = m_DrainVolFlowRate;
				WaterStorage( m_RecoveryTankNum ).TwaterSupply( m_TankSupplyID ) = m_WasteTemp;
			}

			{ auto const SELECT_CASE_var( m_HeatRecoveryConfig );
				if ( SELECT_CASE_var == HeatRecoveryConfig_T::Plant ) {
					m_TempError = 0.0; // No feedback back to the cold supply
					//WaterConnections(WaterConnNum)%ColdTemp = WaterConnections(WaterConnNum)%ColdSupplyTemp
					m_ReturnTemp = m_RecoveryTemp;
					
				} else if ( SELECT_CASE_var == HeatRecoveryConfig_T::Equipment ) {
					m_TempError = std::abs( m_ColdTemp - m_RecoveryTemp );
					
					m_ColdTemp = m_RecoveryTemp;
					m_ReturnTemp = m_ColdSupplyTemp;
					
				} else if ( SELECT_CASE_var == HeatRecoveryConfig_T::PlantAndEquip ) {
					m_TempError = std::abs( m_ColdTemp - m_RecoveryTemp );
					
					m_ColdTemp = m_RecoveryTemp;
					m_ReturnTemp = m_RecoveryTemp;
				}
			} // SELECT_CASE_var( m_HeatRecoveryConfig )
		}

	} // WaterConnections_T::CalcConnectionsHeatRecovery()

	void
	WaterConnections_T::UpdateWaterConnections( )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Updates the node variables with local variables.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataLoopNode::Node;
		using PlantUtilities::SafeCopyPlantNode;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FLOW:

		if ( m_InletNode > 0 && m_OutletNode > 0 ) {
			// Pass all variables from inlet to outlet node
			SafeCopyPlantNode( m_InletNode, m_OutletNode, m_PlantLoopNum );
			// DSU3 Node(OutletNode) = Node(InletNode)

			// Set outlet node variables that are possibly changed
			Node( m_OutletNode ).Temp = m_ReturnTemp;
			// should add enthalpy update to return?
		}

	} // WaterConnections_T::UpdateWaterConnections()

	void
	WaterConnections_T::ReportStandAloneWaterUse()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith, Peter Graham Ellis
		//       DATE WRITTEN   Nov. 2011
		//       MODIFIED       Brent Griffith, March 2010 added argument
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates report variables for stand alone water use

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using Psychrometrics::RhoH2O;
		using Psychrometrics::CPHW;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// FLOW:
		for ( WaterEquipment_T & waterEquip : WaterEquipment_T::sm_instances ) {
			
			waterEquip.m_ColdVolFlowRate = waterEquip.m_ColdMassFlowRate / RhoH2O( InitConvTemp );
			waterEquip.m_HotVolFlowRate = waterEquip.m_HotMassFlowRate / RhoH2O( InitConvTemp );
			waterEquip.m_TotalVolFlowRate = waterEquip.m_ColdVolFlowRate + waterEquip.m_HotVolFlowRate;

			waterEquip.m_ColdVolume = waterEquip.m_ColdVolFlowRate * TimeStepSys * SecInHour;
			waterEquip.m_HotVolume = waterEquip.m_HotVolFlowRate * TimeStepSys * SecInHour;
			waterEquip.m_TotalVolume = waterEquip.m_TotalVolFlowRate * TimeStepSys * SecInHour;

			if ( waterEquip.m_Connections == 0 ) {
				waterEquip.m_Power = waterEquip.m_HotMassFlowRate * CPHW( InitConvTemp ) * ( waterEquip.m_HotTemp - waterEquip.m_ColdTemp );
			} else {
				waterEquip.m_Power = waterEquip.m_HotMassFlowRate * CPHW( InitConvTemp ) * ( waterEquip.m_HotTemp - sm_instances( waterEquip.m_Connections ).m_ReturnTemp );
			}

			waterEquip.m_Energy = waterEquip.m_Power * TimeStepSys * SecInHour;
		}

	}

	void
	WaterConnections_T::ReportWaterUse( )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       Brent Griffith, March 2010 added argument
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates report variables.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// Using/Aliasing
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using Psychrometrics::RhoH2O;
		using Psychrometrics::CPHW;

		// FLOW:
		for ( int WaterEquipNum : m_localWaterEquipment ) {
			WaterEquipment_T & waterEquip = WaterEquipment_T::sm_instances( WaterEquipNum );
			waterEquip.m_ColdVolFlowRate = waterEquip.m_ColdMassFlowRate / RhoH2O( InitConvTemp );
			waterEquip.m_HotVolFlowRate = waterEquip.m_HotMassFlowRate / RhoH2O( InitConvTemp );
			waterEquip.m_TotalVolFlowRate = waterEquip.m_ColdVolFlowRate + waterEquip.m_HotVolFlowRate;

			waterEquip.m_ColdVolume = waterEquip.m_ColdVolFlowRate * TimeStepSys * SecInHour;
			waterEquip.m_HotVolume = waterEquip.m_HotVolFlowRate * TimeStepSys * SecInHour;
			waterEquip.m_TotalVolume = waterEquip.m_TotalVolFlowRate * TimeStepSys * SecInHour;

			if ( waterEquip.m_Connections == 0 ) {
				waterEquip.m_Power = waterEquip.m_HotMassFlowRate * CPHW( InitConvTemp ) * ( waterEquip.m_HotTemp - waterEquip.m_ColdTemp );
			} else {
				waterEquip.m_Power = waterEquip.m_HotMassFlowRate * CPHW( InitConvTemp ) * ( waterEquip.m_HotTemp - sm_instances( waterEquip.m_Connections ).m_ReturnTemp );
			}

			waterEquip.m_Energy = waterEquip.m_Power * TimeStepSys * SecInHour;
		}

		m_ColdVolFlowRate = m_ColdMassFlowRate / RhoH2O( InitConvTemp );
		m_HotVolFlowRate = m_HotMassFlowRate / RhoH2O( InitConvTemp );
		m_TotalVolFlowRate = m_ColdVolFlowRate + m_HotVolFlowRate;

		m_ColdVolume = m_ColdVolFlowRate * TimeStepSys * SecInHour;
		m_HotVolume = m_HotVolFlowRate * TimeStepSys * SecInHour;
		m_TotalVolume = m_TotalVolFlowRate * TimeStepSys * SecInHour;

		m_Power = m_HotMassFlowRate * CPHW( InitConvTemp ) * ( m_HotTemp - m_ReturnTemp );
		m_Energy = m_Power * TimeStepSys * SecInHour;

		m_RecoveryEnergy = m_RecoveryRate * TimeStepSys * SecInHour;

	} // WaterConnections_T::ReportWaterUse()

	void
	WaterConnections_T::CalcWaterUseZoneGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the zone internal gains due to water use sensible and latent loads.

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataHeatBalance::ZoneData;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyEnvrnFlag( true );

		// FLOW:
		if ( WaterEquipment_T::sm_numInstances == 0 ) return;

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			for ( WaterEquipment_T & waterEquip : WaterEquipment_T::sm_instances ) {
				waterEquip.m_SensibleRate = 0.0;
				waterEquip.m_SensibleEnergy = 0.0;
				waterEquip.m_SensibleRateNoMultiplier = 0.0;
				waterEquip.m_LatentRate = 0.0;
				waterEquip.m_LatentEnergy = 0.0;
				waterEquip.m_LatentRateNoMultiplier = 0.0;
				waterEquip.m_MixedTemp = 0.0;
				waterEquip.m_TotalMassFlowRate = 0.0;
				waterEquip.m_DrainTemp = 0.0;
				waterEquip.m_ColdVolFlowRate = 0.0;
				waterEquip.m_HotVolFlowRate = 0.0;
				waterEquip.m_TotalVolFlowRate = 0.0;
				waterEquip.m_ColdMassFlowRate = 0.0;
				waterEquip.m_HotMassFlowRate = 0.0;
			}
			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		for ( WaterEquipment_T & waterEquip : WaterEquipment_T::sm_instances ) {
			if ( waterEquip.m_ZoneNum == 0 ) continue;
			ZoneData & z = Zone( waterEquip.m_ZoneNum );
			waterEquip.m_SensibleRateNoMultiplier = waterEquip.m_SensibleRate / ( z.Multiplier * z.ListMultiplier ); // CR7401, back out multipliers
			waterEquip.m_LatentRateNoMultiplier = waterEquip.m_LatentRate / ( z.Multiplier * z.ListMultiplier ); // CR7401, back out multipliers
		}

		//  ! this routine needs to model approx zone gains for use during sizing
		//  IF(DoingSizing)THEN
		//    DO WaterEquipNum = 1, NumWaterEquipment
		//      WaterEquipment(WaterEquipNum)%SensibleRateNoMultiplier =
		//      WaterEquipment(WaterEquipNum)%LatentRateNoMultiplier   =
		//    END DO
		//  ENDIF

	} // CalcWaterUseZoneGains()

	void
	WaterConnections_T::SimulateWaterUse( bool const FirstHVACIteration )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   August 2006
		//       MODIFIED       Brent Griffith, March 2010, seperated plant connected to different sim routine
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine is called from non zone equipment manager and serves to call
		// water use and connections that are not connected to a full plant loop

		// METHODOLOGY EMPLOYED:

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIterations( 100 );
		Real64 const Tolerance( 0.1 ); // Make input?

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int MaxIterationsErrorCount;
		static bool MyEnvrnFlag( true );

		// FLOW:
		if ( sm_GetInputFlag ) {
			GetInput();
			sm_GetInputFlag = false;
		}

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			MaxIterationsErrorCount = 0;
			if ( WaterEquipment_T::sm_numInstances > 0 ) {
				for ( WaterEquipment_T & waterEquip : WaterEquipment_T::sm_instances ) {
					waterEquip.m_SensibleRate = 0.0;
					waterEquip.m_SensibleEnergy = 0.0;
					waterEquip.m_LatentRate = 0.0;
					waterEquip.m_LatentEnergy = 0.0;
					waterEquip.m_MixedTemp = 0.0;
					waterEquip.m_TotalMassFlowRate = 0.0;
					waterEquip.m_DrainTemp = 0.0;
				}
			}

			if ( sm_numInstances > 0 ) {
				for ( WaterConnections_T & waterConn : sm_instances ) waterConn.m_TotalMassFlowRate = 0.0;
			}

			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		// Simulate all unconnected WATER USE EQUIPMENT objects
		for ( WaterEquipment_T & waterEquip : WaterEquipment_T::sm_instances ) {
			if ( waterEquip.m_Connections == 0 ) {
				waterEquip.CalcEquipmentFlowRates( );
				waterEquip.CalcEquipmentDrainTemp( );
			}
		} // WaterEquipNum

		ReportStandAloneWaterUse();

		// Simulate WATER USE CONNECTIONS objects and connected WATER USE EQUIPMENT objects
		for ( WaterConnections_T & waterConn : sm_instances ) {
			if ( ! waterConn.m_StandAlone ) continue; // only model non plant connections here

			waterConn.InitConnections( );

			int NumIteration = 0;

			while ( true ) {
				++NumIteration;

				waterConn.CalcConnectionsFlowRates( FirstHVACIteration );
				waterConn.CalcConnectionsDrainTemp( );
				waterConn.CalcConnectionsHeatRecovery( );

				if ( waterConn.m_TempError < Tolerance ) {
					break;
				} else if ( NumIteration > MaxIterations ) {
					if ( ! WarmupFlag ) {
						if ( waterConn.m_MaxIterationsErrorIndex == 0 ) {
							ShowWarningError( "WaterUse:Connections = " + waterConn.Name + ":  Heat recovery temperature did not converge" );
							ShowContinueErrorTimeStamp( "" );
						}
						ShowRecurringWarningErrorAtEnd( "WaterUse:Connections = " + waterConn.Name + ":  Heat recovery temperature did not converge", waterConn.m_MaxIterationsErrorIndex );
					}
					break;
				}

			} // while true

			waterConn.UpdateWaterConnections( );

			waterConn.ReportWaterUse( );

		} // for WaterConnNum

	} // WaterConnections_T::SimulateWaterUse()

} // namespace WaterUse

} // namespace EnergyPlus
