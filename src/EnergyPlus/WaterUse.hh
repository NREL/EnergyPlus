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

#ifndef WaterUse_hh_INCLUDED
#define WaterUse_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <PlantComponent.hh>

namespace EnergyPlus {

namespace WaterUse {

	// Using/Aliasing

	// Type Definitions
	enum class HeatRecoveryHX_T {
		Ideal = 1,
		CounterFlow = 2,
		CrossFlow = 3
	};

	enum class HeatRecoveryConfig_T {
		Plant = 1,
		Equipment = 2,
		PlantAndEquip = 3
	};

	// Data
	struct WaterConnections_T : public PlantComponent
	{

		virtual
		~WaterConnections_T()
		{}

		// Static members
		static Array1D< WaterConnections_T > sm_instances;
		static int sm_numInstances;

		static bool sm_GetInputFlag;
		
		// Members
		std::string Name; // Name of DHW
		bool m_Init; // Flag for initialization:  TRUE means do the init
		bool m_InitSizing; // Flag for initialization of plant sizing
		bool m_StandAlone; // Flag for operation with no plant connections
		int m_InletNode; // Hot water demand node
		int m_OutletNode; // Cold water supply node
		int m_SupplyTankNum;
		int m_RecoveryTankNum;
		int m_TankDemandID; // array to request flow from supply tank
		int m_TankSupplyID; // array to send flow to recovery tank
		bool m_HeatRecovery;
		HeatRecoveryHX_T m_HeatRecoveryHX;
		HeatRecoveryConfig_T m_HeatRecoveryConfig;
		Real64 m_HXUA;
		Real64 m_Effectiveness;
		Real64 m_RecoveryRate;
		Real64 m_RecoveryEnergy;
		Real64 m_MainsMassFlowRate; // Mass flow rate (kg/s)
		Real64 m_TankMassFlowRate; // Mass flow rate (kg/s)
		Real64 m_ColdMassFlowRate; // Mass flow rate (kg/s)  cold = mains + tank
		Real64 m_HotMassFlowRate; // Mass flow rate (kg/s)
		Real64 m_TotalMassFlowRate; // Mass flow rate (kg/s) total = cold + hot
		Real64 m_DrainMassFlowRate;
		Real64 m_RecoveryMassFlowRate;
		Real64 m_PeakVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
		Real64 m_MainsVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
		Real64 m_TankVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
		Real64 m_ColdVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
		Real64 m_HotVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
		Real64 m_TotalVolFlowRate; // Volumetric flow rate, also water consumption rate (m3/s)
		Real64 m_DrainVolFlowRate;
		Real64 m_PeakMassFlowRate; // Peak Mass flow rate for MassFlowRateMax
		int m_ColdTempSchedule; // Index for schedule object
		int m_HotTempSchedule; // Index for schedule object
		Real64 m_MainsTemp; // Cold supply water temperature (C)
		Real64 m_TankTemp; // Cold supply water temperature (C)
		Real64 m_ColdSupplyTemp; // cold from mains, schedule, or tank, depending
		Real64 m_ColdTemp; // Cold supply water temperature (C)  actual cold (could be reheated)
		Real64 m_HotTemp; // Hot supply water temperature (C)
		Real64 m_DrainTemp;
		Real64 m_RecoveryTemp;
		Real64 m_ReturnTemp;
		Real64 m_WasteTemp;
		Real64 m_TempError;
		Real64 m_MainsVolume; // Water consumption (m3)
		Real64 m_TankVolume; // Water consumption (m3)
		Real64 m_ColdVolume; // Water consumption (m3)
		Real64 m_HotVolume; // Water consumption (m3)
		Real64 m_TotalVolume; // Water consumption (m3)
		Real64 m_Power; // Heating rate required to raise temperature from cold to hot (W)
		Real64 m_Energy; // Heating energy required to raise temperature from cold to hot (J)
		int m_NumWaterEquipment;
		int m_MaxIterationsErrorIndex; // recurring error index

		// This shadows the namespace level array WaterEquipment
		Array1D_int m_localWaterEquipment;
		
		int m_PlantLoopNum;
		int m_PlantLoopSide;
		int m_PlantLoopBranchNum;
		int m_PlantLoopCompNum;

		bool m_SetLoopIndexFlag; // Constructed as true and set to false after connections are initialized
		
		// Static methods
	public:
		static
		PlantComponent *
		factory( const std::string objectName );

		static
		void
		CalcWaterUseZoneGains();
		
		static
		void
		SimulateWaterUse( bool const FirstHVACIteration );
		
	protected:
		static
		void
		GetInput();
		
		static
		void
		ReportStandAloneWaterUse();

		// Instance methods
	public:
		void
		simulate ( const PlantLocation & calledFromLocation, bool const FirstHVACIteration, Real64 & CurLoad, bool const RunFlag ) override ;
		
		// Default Constructor
		WaterConnections_T() :
			m_Init( true ),
			m_InitSizing( true ),
			m_StandAlone( false ),
			m_InletNode( 0 ),
			m_OutletNode( 0 ),
			m_SupplyTankNum( 0 ),
			m_RecoveryTankNum( 0 ),
			m_TankDemandID( 0 ),
			m_TankSupplyID( 0 ),
			m_HeatRecovery( false ),
			m_HeatRecoveryHX( HeatRecoveryHX_T::Ideal ),
			m_HeatRecoveryConfig( HeatRecoveryConfig_T::Plant ),
			m_HXUA( 0.0 ),
			m_Effectiveness( 0.0 ),
			m_RecoveryRate( 0.0 ),
			m_RecoveryEnergy( 0.0 ),
			m_MainsMassFlowRate( 0.0 ),
			m_TankMassFlowRate( 0.0 ),
			m_ColdMassFlowRate( 0.0 ),
			m_HotMassFlowRate( 0.0 ),
			m_TotalMassFlowRate( 0.0 ),
			m_DrainMassFlowRate( 0.0 ),
			m_RecoveryMassFlowRate( 0.0 ),
			m_PeakVolFlowRate( 0.0 ),
			m_MainsVolFlowRate( 0.0 ),
			m_TankVolFlowRate( 0.0 ),
			m_ColdVolFlowRate( 0.0 ),
			m_HotVolFlowRate( 0.0 ),
			m_TotalVolFlowRate( 0.0 ),
			m_DrainVolFlowRate( 0.0 ),
			m_PeakMassFlowRate( 0.0 ),
			m_ColdTempSchedule( 0 ),
			m_HotTempSchedule( 0 ),
			m_MainsTemp( 0.0 ),
			m_TankTemp( 0.0 ),
			m_ColdSupplyTemp( 0.0 ),
			m_ColdTemp( 0.0 ),
			m_HotTemp( 0.0 ),
			m_DrainTemp( 0.0 ),
			m_RecoveryTemp( 0.0 ),
			m_ReturnTemp( 0.0 ),
			m_WasteTemp( 0.0 ),
			m_TempError( 0.0 ),
			m_MainsVolume( 0.0 ),
			m_TankVolume( 0.0 ),
			m_ColdVolume( 0.0 ),
			m_HotVolume( 0.0 ),
			m_TotalVolume( 0.0 ),
			m_Power( 0.0 ),
			m_Energy( 0.0 ),
			m_NumWaterEquipment( 0 ),
			m_MaxIterationsErrorIndex( 0 ),
			m_PlantLoopNum( 0 ),
			m_PlantLoopSide( 0 ),
			m_PlantLoopBranchNum( 0 ),
			m_PlantLoopCompNum( 0 ),
			m_SetLoopIndexFlag( true )
		{}
	protected:
		void
		CalcConnectionsDrainTemp( );
		
		void
		CalcConnectionsHeatRecovery( );
		
		void
		CalcConnectionsFlowRates( bool const FirstHVACIteration );
		
		void
		InitConnections( );
		
		void
		UpdateWaterConnections( );
		
		void
		ReportWaterUse( );
	};
	
	void
	clear_state();

	} // WaterUse

} // EnergyPlus

#endif // WaterUse_hh_INCLUDED
