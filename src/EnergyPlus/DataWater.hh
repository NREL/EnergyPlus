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

#ifndef DataWater_hh_INCLUDED
#define DataWater_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataWater {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLI

	// MODULE PARAMETER DEFINITION

	extern int const ScheduledTankTemp; // tank water temperature is user input via schedule
	extern int const TankZoneThermalCoupled; // tank water temperature is modeled using simple UA

	extern int const RainSchedDesign; // mode of Rainfall determination is Scheduled Design
	extern int const IrrSchedDesign; // mode of Irrigation determination is Scheduled Design (DJS -PSU)
	extern int const IrrSmartSched; // mode of irrigation DJS - PSU

	extern int const ConstantRainLossFactor;
	extern int const ScheduledRainLossFactor;

	extern int const AmbientTempSchedule; // ambient temperature around tank (or HPWH inlet air) is scheduled
	extern int const AmbientTempZone; // tank is located in a zone or HPWH inlet air is zone air only
	extern int const AmbientTempExterior; // tank is located outdoors or HPWH inlet air is outdoor air only

	extern int const ConstantWaterTable;
	extern int const ScheduledWaterTable;

	extern int const NoControlLevel;
	extern int const MainsFloatValve;
	extern int const WellFloatValve;
	extern int const WellFloatMainsBackup;
	extern int const OtherTankFloatValve;
	extern int const TankMainsBackup;

	extern int const OverflowDiscarded;
	extern int const OverflowToTank;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumWaterStorageTanks; // number of water Storage tanks in model
	extern int NumRainCollectors; // number of rainfall collectors in model
	extern int NumGroundWaterWells; // number of
	extern int NumSiteRainFall;
	extern int NumIrrigation; // DJS PSU Dec 2006 number of irrigation descriptions (1 allowed)
	extern bool AnyWaterSystemsInModel; // control flag set true if any water systems
	extern bool WaterSystemGetInputCalled; // set true once input data gotten.
	extern bool AnyIrrigationInModel; // control flag set true if irrigation input for ecoroof DJS PSU Dec 2006

	// Types

	struct StorageTankDataStruct
	{
		// Members
		// user input data
		std::string Name; // name of this Storage Tank
		std::string QualitySubCategoryName; // name of water subcategory
		//   INTEGER                      :: QualitySubCategory = 0 !
		Real64 MaxCapacity; // tank capacity Limit [m3]
		int OverflowMode;
		std::string OverflowTankName;
		int OverflowTankID;
		int OverflowTankSupplyARRID;
		Real64 ValveOnCapacity; // tank capacity at lower control range [m3]
		Real64 ValveOffCapacity; // tank capacity at upper control range [m3]
		int ControlSupplyType; // mode for tank controlled resupply
		int GroundWellID; // index "pointer" to well if present
		std::string SupplyTankName;
		int SupplyTankID;
		int SupplyTankDemandARRID;
		Real64 BackupMainsCapacity;
		Real64 InitialVolume; // water in tank at start of simulation period [m3]
		Real64 MaxInFlowRate; // limit on rate of inlet [m3/s]
		Real64 MaxOutFlowRate; // limit on rate of outlet [m3/s]
		int ThermalMode;
		Real64 InitialTankTemp; // initial tank temperature [C]
		int TempSchedID; // index "pointer" to schedule
		int AmbientTempIndicator; // Indicator for ambient tank losses (SCHEDULE, ZONE, EXTERIOR)
		int AmbientTempSchedule; // Schedule index pointer
		int ZoneID; // index "pointer" to zone where tank is
		Real64 UValue; // U-value for tank [W/m2-k]
		Real64 SurfArea; // surface are of tank on Zone side... [m2]
		int InternalMassID; // index "pointer" to internal mass object for thermal coupling
		std::string SurfMaterialName; // surface properties
		// calculated data and from elsewhere
		Real64 ThisTimeStepVolume;
		Real64 LastTimeStepVolume;
		Real64 LastTimeStepTemp; // previous temperature of tank water
		int NumWaterSupplies;
		Array1D< Real64 > VdotAvailSupply; // Each supply component has its own term
		Array1D< Real64 > TwaterSupply; // Each supply component has its own term
		Array1D_string SupplyCompNames;
		Array1D_string SupplyCompTypes;
		int NumWaterDemands;
		Array1D< Real64 > VdotRequestDemand; // each demand componennt has a slot
		Array1D< Real64 > VdotAvailDemand; // each demand componennt has a slot
		Array1D_string DemandCompNames;
		Array1D_string DemandCompTypes;
		Real64 VdotFromTank;
		Real64 VdotToTank;
		Real64 VdotOverflow;
		Real64 VolOverflow;
		// report variables
		Real64 NetVdot;
		Real64 Twater;
		Real64 TouterSkin;
		Real64 TwaterOverflow;
		Real64 MainsDrawVdot;
		Real64 MainsDrawVol;
		Real64 SkinLossPower; // heat loss to surrounding zone [W]
		Real64 SkinLossEnergy; // heat loss to surround zone [J]
		Real64 SkinLossConvect; // convective heat loss to zone [W]
		Real64 SkinLossRadiat; // radiative heat loss to zone [W}

		// Default Constructor
		StorageTankDataStruct() :
			MaxCapacity( 0.0 ),
			OverflowMode( 0 ),
			OverflowTankID( 0 ),
			OverflowTankSupplyARRID( 0 ),
			ValveOnCapacity( 0.0 ),
			ValveOffCapacity( 0.0 ),
			ControlSupplyType( 0 ),
			GroundWellID( 0 ),
			SupplyTankID( 0 ),
			SupplyTankDemandARRID( 0 ),
			BackupMainsCapacity( 0.0 ),
			InitialVolume( 0.0 ),
			MaxInFlowRate( 0.0 ),
			MaxOutFlowRate( 0.0 ),
			ThermalMode( 0 ),
			InitialTankTemp( 20.0 ),
			TempSchedID( 0 ),
			AmbientTempIndicator( 0 ),
			AmbientTempSchedule( 0 ),
			ZoneID( 0 ),
			UValue( 0.0 ),
			SurfArea( 0.0 ),
			InternalMassID( 0 ),
			ThisTimeStepVolume( 0.0 ),
			LastTimeStepVolume( 0.0 ),
			LastTimeStepTemp( 0.0 ),
			NumWaterSupplies( 0 ),
			NumWaterDemands( 0 ),
			VdotFromTank( 0.0 ),
			VdotToTank( 0.0 ),
			VdotOverflow( 0.0 ),
			VolOverflow( 0.0 ),
			NetVdot( 0.0 ),
			Twater( 0.0 ),
			TouterSkin( 0.0 ),
			TwaterOverflow( 0.0 ),
			MainsDrawVdot( 0.0 ),
			MainsDrawVol( 0.0 ),
			SkinLossPower( 0.0 ),
			SkinLossEnergy( 0.0 ),
			SkinLossConvect( 0.0 ),
			SkinLossRadiat( 0.0 )
		{}

	};

	struct RainfallCollectorDataStruct
	{
		// Members
		// user input data
		std::string Name; // name of this rain collector
		std::string StorageTankName;
		int StorageTankID; // index "pointer" to storage tank array
		int StorageTankSupplyARRID;
		int LossFactorMode; // control how loss factor(s) are entered
		Real64 LossFactor; // loss factor when constant
		int LossFactorSchedID; // index "pointer" to schedule
		Real64 MaxCollectRate;
		int NumCollectSurfs; // number of surfaces used in the collector
		Array1D_string SurfName;
		Array1D_int SurfID;
		//calculated and from elsewhere
		Real64 HorizArea; // area of surfaces in the vertical normal direction
		Real64 VdotAvail;
		Real64 VolCollected;
		Real64 MeanHeight;

		// Default Constructor
		RainfallCollectorDataStruct() :
			StorageTankID( 0 ),
			StorageTankSupplyARRID( 0 ),
			LossFactorMode( 0 ),
			LossFactor( 0.0 ),
			LossFactorSchedID( 0 ),
			MaxCollectRate( 0.0 ),
			NumCollectSurfs( 0 ),
			HorizArea( 0.0 ),
			VdotAvail( 0.0 ),
			VolCollected( 0.0 ),
			MeanHeight( 0.0 )
		{}

	};

	struct GroundwaterWellDataStruct
	{
		// Members
		// user input data
		std::string Name; // name of this
		std::string StorageTankName;
		int StorageTankID; // index "pointer" to water storage tank
		int StorageTankSupplyARRID; // index "pointer" to storage supply arrays
		Real64 PumpDepth; // depth of pump  [m]
		Real64 PumpNomVolFlowRate; // nominal flow rate of pump [m3/s]
		Real64 PumpNomHead; // design nominal capacity of pump
		Real64 PumpNomPowerUse; // design nominal power of pump at nom capacity
		Real64 PumpEfficiency;
		Real64 WellRecoveryRate; // rate at which groundwater can enter well [m3/s]
		Real64 NomWellStorageVol; // water storage in well at average water table depth [m3]
		int GroundwaterTableMode; // method of determining water table depth
		Real64 WaterTableDepth;
		int WaterTableDepthSchedID;
		//calculated and from elsewhere
		Real64 VdotRequest; // rate of flow over timestep requested by tank
		Real64 VdotDelivered; // rate of flow provided [m3/s]
		Real64 VolDelivered; // water provided [m3]
		Real64 PumpPower;
		Real64 PumpEnergy;

		// Default Constructor
		GroundwaterWellDataStruct() :
			StorageTankID( 0 ),
			StorageTankSupplyARRID( 0 ),
			PumpDepth( 0.0 ),
			PumpNomVolFlowRate( 0.0 ),
			PumpNomHead( 0.0 ),
			PumpNomPowerUse( 0.0 ),
			PumpEfficiency( 0.0 ),
			WellRecoveryRate( 0.0 ),
			NomWellStorageVol( 0.0 ),
			GroundwaterTableMode( 0 ),
			WaterTableDepth( 0.0 ),
			WaterTableDepthSchedID( 0 ),
			VdotRequest( 0.0 ),
			VdotDelivered( 0.0 ),
			VolDelivered( 0.0 ),
			PumpPower( 0.0 ),
			PumpEnergy( 0.0 )
		{}

	};

	struct SiteRainFallDataStruct
	{
		// Members
		int ModeID; // type of rainfall modeling
		Real64 DesignAnnualRain;
		int RainSchedID;
		Real64 NomAnnualRain;
		//calculated and from elsewhere.
		Real64 CurrentRate;
		Real64 CurrentAmount;

		// Default Constructor
		SiteRainFallDataStruct() :
			ModeID( 0 ),
			DesignAnnualRain( 0.0 ),
			RainSchedID( 0 ),
			NomAnnualRain( 0.0 ),
			CurrentRate( 0.0 ),
			CurrentAmount( 0.0 )
		{}

	};

	struct IrrigationDataStruct
	{
		// Members
		int ModeID; // type of irrigation modeling
		int IrrSchedID;
		Real64 ScheduledAmount;
		Real64 ActualAmount;
		Real64 IrrigationThreshold; // percent at which no irrigation happens (smart schedule)

		// Default Constructor
		IrrigationDataStruct() :
			ModeID( 0 ),
			IrrSchedID( 0 ),
			ScheduledAmount( 0.0 ),
			ActualAmount( 0.0 ),
			IrrigationThreshold( 0.4 )
		{}

	};

	// Object Data
	extern SiteRainFallDataStruct RainFall; // type of rainfall modeling | design annual rain | rain sched id | nominal annual rain | current rate | current amount
	extern IrrigationDataStruct Irrigation; // type of irrigation modeling | Irrigation schedule id | scheduled amount | actual amount | irrigation threshold
	extern Array1D< StorageTankDataStruct > WaterStorage;
	extern Array1D< RainfallCollectorDataStruct > RainCollector;
	extern Array1D< GroundwaterWellDataStruct > GroundwaterWell;

} // DataWater

} // EnergyPlus

#endif
