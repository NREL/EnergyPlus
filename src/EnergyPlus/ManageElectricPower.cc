// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/MArray.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <ManageElectricPower.hh>
#include <CTElectricGenerator.hh>
#include <CurveManager.hh>
#include <DataEnvironment.hh>
#include <DataGlobalConstants.hh>
#include <DataHeatBalance.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <EMSManager.hh>
#include <FuelCellElectricGenerator.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <ICEngineElectricGenerator.hh>
#include <InputProcessor.hh>
#include <MicroCHPElectricGenerator.hh>
#include <MicroturbineElectricGenerator.hh>
#include <ManageElectricPower.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <Photovoltaics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WindTurbine.hh>

namespace EnergyPlus {

namespace ManageElectricPower {

	// MODULE INFORMATION:
	//       AUTHOR         Dan Fisher
	//       DATE WRITTEN   Sept. 2000
	//       MODIFIED       B. Griffith  Feb. 2008 add thermal following operating scheme
	//                      B. Griffith May 2009 add EMS
	//       RE-ENGINEERED  B. Griffith  June 2008 add support for DC centers and different inverter models
	//       MODIFIED       B. Brannon July 2009 Charge and Draw objects removed from line 1825
	//                      B. Brannon Nullified Charge during Drawing 2298. Nullified Draw during Charging 2305.
	//                                 to prevent simultaneous Drawing and Charging
	//                      B. Nigusse  Feb. 2010 Modifed the load center requested power calculation procedure
	//                                            used to manage the electric load center electrical storage units.
	//                      W. Wang     June-July, 2010 Add a transformer model
	//                      Y. KyungTae and W. Wang July-August, 2011 Added a battery model

	// PURPOSE OF THIS MODULE:
	// This module manages electric power generation and distribution
	// equipment.

	// METHODOLOGY EMPLOYED: na

	// REFERENCES: na

	// OTHER NOTES: na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataLoopNode;
	using DataGlobals::NumOfTimeStepInHour;
	using DataGlobals::TimeStepZoneSec;
	using DataGlobals::SecInHour;
	using DataGlobals::ScheduleAlwaysOn;
	using DataHVACGlobals::TimeStepSys;
	using DataGlobalConstants::iGeneratorICEngine;
	using DataGlobalConstants::iGeneratorCombTurbine;
	using DataGlobalConstants::iGeneratorPV;
	using DataGlobalConstants::iGeneratorFuelCell;
	using DataGlobalConstants::iGeneratorMicroCHP;
	using DataGlobalConstants::iGeneratorMicroturbine;
	using DataGlobalConstants::iGeneratorWindTurbine;

	// Data
	// MODULE PARAMETER DEFINITIONS:

	int const iOpSchemeBaseLoad( 1 ); // Electric load center dispatch mode
	int const iOpSchemeDemandLimit( 2 ); // Electric load center dispatch mode
	int const iOpSchemeTrackElectrical( 3 ); // Electric load center dispatch mode
	int const iOpSchemeTrackSchedule( 4 ); // Electric load center dispatch mode
	int const iOpSchemeTrackMeter( 5 ); // Electric load center dispatch mode
	int const iOpSchemeThermalFollow( 6 ); // Electric load center dispatch mode
	int const iOpSchemeThermalFollowLimitElectrical( 7 ); // Electric load center dispatch mode

	int const ACBuss( 100 ); // Electic load center buss and power conditioning mode
	int const ACBussStorage( 101 ); // Electic load center buss and power conditioning mode
	int const DCBussInverter( 102 ); // Electic load center buss and power conditioning mode
	int const DCBussInverterDCStorage( 103 ); // Electic load center buss and power conditioning mode
	int const DCBussInverterACStorage( 104 ); // Electic load center buss and power conditioning mode

	int const CECLookUpTableModel( 201 ); // inverter model mode
	int const CurveFuncOfPower( 202 ); // inverter model mode
	int const SimpleConstantEff( 203 ); // inverter model mode

	int const ZoneGains( 301 ); // power conditioning equipment thermal loss destination
	int const LostToOutside( 302 ); // power conditioning equipment thermal loss destination

	int const SimpleBucketStorage( 401 ); // storage model mode (1 of 2)
	int const KiBaMBattery( 402 ); // storage model mode (2 of 2)

	int const PowerInFromGrid( 501 ); // Transformer usage: power in from grid
	int const PowerOutFromBldg( 502 ); // Transformer usage: power out from onsite generation
	int const LossesMethod( 521 ); // Transformer performance input methos: RatedLosses
	int const EfficiencyMethod( 522 ); // Transformer performance input methos: NominalEfficiency

	int const Battery_LifeCalculation_Yes( 1 );
	int const Battery_LifeCalculation_No( 2 );

	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	bool GetInput( true ); // When TRUE, calls subroutine to read input file.
	int NumLoadCenters( 0 );
	int NumInverters( 0 );
	int NumElecStorageDevices( 0 );
	int NumTransformers( 0 );

	int ElecProducedCoGenIndex( 0 );
	int ElecProducedPVIndex( 0 );
	int ElecProducedWTIndex( 0 );

	int MaxRainflowArrayBounds( 100 );
	int MaxRainflowArrayInc( 100 );

	// SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops

	// Object Data
	Array1D< ElecStorageDataStruct > ElecStorage;
	Array1D< DCtoACInverterStruct > Inverter;
	Array1D< ElectricPowerLoadCenter > ElecLoadCenter; // dimension to number of machines
	Array1D< ElectricTransformer > Transformer;
	WholeBuildingElectricPowerSummary WholeBldgElectSummary;

	// MODULE SUBROUTINES:

	// Beginning of Electric Power Manager Subroutines
	//*************************************************************************

	// Functions

	void
	ManageElectricLoadCenters(
		bool const FirstHVACIteration,
		bool & SimElecCircuits, // simulation convergence flag
		bool const UpdateMetersOnly // if true then don't resimulate generators, just update meters.
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 2000
		//       RE-ENGINEERED  Richard Liesen, Feb 2003
		//       RE-ENGINEERED  Bereket Nigusse, Jan/Feb 2010   Generator dispatch is based on actual
		//                                                      power produced by the previous generator(s)
		//       MODIFIED       Weimin Wang, July 2010          Consider transformer

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the electric load centers by matching demand and
		// generator power output.

		// METHODOLOGY EMPLOYED:
		// Generators are dispatched in the sequence sepecified in the idf. The generators
		// in the first electric load center are dispatched first. Generators are called
		// right after dispatch and the remaining building load due for dispatch is updated
		// using the actual generated power output not the requested value from the
		// ElectricLoadCenter:Generators object(s).
		// REFERENCES: na

		// USE STATEMENTS:

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using General::TrimSigDigits;
		using DataGlobals::MetersHaveBeenInitialized;
		using DataGlobals::BeginEnvrnFlag;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int GenNum( 0 ); // Generator number counter
		static int LoadCenterNum( 0 ); // Load center number counter
		static int TransfNum( 0 ); // Transformer number counter
		static int MeterNum( 0 ); // A transformer's meter number counter
		static int MeterIndex( 0 ); // Meter index number from GetMeterIndex
		static int ElecFacilityIndex( 0 );
		static Real64 ElecFacilityBldg( 0.0 );
		static Real64 ElecFacilityHVAC( 0.0 );
		static Real64 ElecProducedPV( 0.0 );
		static Real64 ElecProducedWT( 0.0 );
		static Real64 RemainingLoad( 0.0 ); // Remaining electric power load to be met by a load center
		static Real64 WholeBldgRemainingLoad( 0.0 ); // Remaining electric power load for the building
		static Real64 RemainingThermalLoad( 0.0 ); // Remaining thermal load to be met
		static bool MyOneTimeFlag( true );
		static Real64 CustomMeterDemand( 0.0 ); // local variable for Custom metered elec demand
		static bool MyEnvrnFlag( true );

		static Real64 ElectricProdRate( 0.0 ); // Electric Power Production Rate of Generators
		static Real64 ThermalProdRate( 0.0 ); // Thermal Power Production Rate of Generators
		static Real64 ExcessThermalPowerRequest( 0.0 ); // Excess Thermal Power Request

		static Real64 LoadCenterElectricLoad( 0.0 ); // Load center electric load to be dispatched
		static Real64 LoadCenterThermalLoad( 0.0 ); // Load center thermal load to be dispatched
		static Real64 StorageDrawnPower( 0.0 ); // Electric Power Draw Rate from storage units
		static Real64 StorageStoredPower( 0.0 ); // Electric Power Store Rate from storage units

		// Get Generator data from input file
		if ( GetInput ) {
			GetPowerManagerInput();
			GetInput = false;
		}

		// Setting up the Internal Meters and getting their indexes is done only once
		if ( MetersHaveBeenInitialized && MyOneTimeFlag ) {
			ElecFacilityIndex = GetMeterIndex( "Electricity:Facility" );
			ElecProducedCoGenIndex = GetMeterIndex( "Cogeneration:ElectricityProduced" );
			ElecProducedPVIndex = GetMeterIndex( "Photovoltaic:ElectricityProduced" );
			ElecProducedWTIndex = GetMeterIndex( "WindTurbine:ElectricityProduced" );

			for ( LoadCenterNum = 1; LoadCenterNum <= NumLoadCenters; ++LoadCenterNum ) {
				ElecLoadCenter( LoadCenterNum ).DemandMeterPtr = GetMeterIndex( ElecLoadCenter( LoadCenterNum ).DemandMeterName );
			}

			for ( TransfNum = 1; TransfNum <= NumTransformers; ++TransfNum ) { //NumTransformers is a module variable
				if ( Transformer( TransfNum ).UsageMode == PowerInFromGrid ) {
					for ( MeterNum = 1; MeterNum <= isize( Transformer( TransfNum ).WiredMeterNames ); ++MeterNum ) {
						MeterIndex = GetMeterIndex( Transformer( TransfNum ).WiredMeterNames( MeterNum ) );
						Transformer( TransfNum ).WiredMeterPtrs( MeterNum ) = MeterIndex;

						//Check whether the meter is an electricity meter
						//Index function is used here because some resource types are not Electricity but strings containing
						// Electricity such as ElectricityPurchased and ElectricityProduced.
						//It is not proper to have this check in GetInput routine because the meter index may have not been defined
						if ( ! has( GetMeterResourceType( MeterIndex ), "Electricity" ) ) {
							ShowFatalError( "Non-electricity meter used for " + Transformer( TransfNum ).Name );
						}
					}
				}
			}

			MyOneTimeFlag = false;

		}

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			WholeBldgElectSummary.ElectricityProd = 0.0;
			WholeBldgElectSummary.ElectProdRate = 0.0;
			WholeBldgElectSummary.ElectricityPurch = 0.0;
			WholeBldgElectSummary.ElectPurchRate = 0.0;
			WholeBldgElectSummary.ElectSurplusRate = 0.0;
			WholeBldgElectSummary.ElectricitySurplus = 0.0;
			WholeBldgElectSummary.ElectricityNetRate = 0.0;
			WholeBldgElectSummary.ElectricityNet = 0.0;
			WholeBldgElectSummary.TotalBldgElecDemand = 0.0;
			WholeBldgElectSummary.TotalHVACElecDemand = 0.0;
			WholeBldgElectSummary.TotalElectricDemand = 0.0;
			WholeBldgElectSummary.ElecProducedPVRate = 0.0;
			WholeBldgElectSummary.ElecProducedWTRate = 0.0;

			if ( NumLoadCenters > 0 ) {
				ElecLoadCenter.DCElectricityProd() = 0.0;
				ElecLoadCenter.DCElectProdRate() = 0.0;
				ElecLoadCenter.DCpowerConditionLosses() = 0.0;
				ElecLoadCenter.ElectricityProd() = 0.0;
				ElecLoadCenter.ElectProdRate() = 0.0;
				ElecLoadCenter.ThermalProd() = 0.0;
				ElecLoadCenter.ThermalProdRate() = 0.0;
				ElecLoadCenter.TotalPowerRequest() = 0.0;
				ElecLoadCenter.TotalThermalPowerRequest() = 0.0;
				ElecLoadCenter.ElectDemand() = 0.0;
			}

			for ( LoadCenterNum = 1; LoadCenterNum <= NumLoadCenters; ++LoadCenterNum ) {
				if ( ElecLoadCenter( LoadCenterNum ).NumGenerators == 0 ) continue;
				ElecLoadCenter( LoadCenterNum ).ElecGen.ONThisTimestep() = false;
				ElecLoadCenter( LoadCenterNum ).ElecGen.DCElectricityProd() = 0.0;
				ElecLoadCenter( LoadCenterNum ).ElecGen.DCElectProdRate() = 0.0;
				ElecLoadCenter( LoadCenterNum ).ElecGen.ElectricityProd() = 0.0;
				ElecLoadCenter( LoadCenterNum ).ElecGen.ElectProdRate() = 0.0;
				ElecLoadCenter( LoadCenterNum ).ElecGen.ThermalProd() = 0.0;
				ElecLoadCenter( LoadCenterNum ).ElecGen.ThermalProdRate() = 0.0;
			}

			if ( NumInverters > 0 ) {
				Inverter.AncillACuseRate() = 0.0;
				Inverter.AncillACuseEnergy() = 0.0;
				Inverter.QdotConvZone() = 0.0;
				Inverter.QdotRadZone() = 0.0;
			}

			if ( NumElecStorageDevices > 0 ) {
				ElecStorage.PelNeedFromStorage() = 0.0;
				ElecStorage.PelFromStorage() = 0.0;
				ElecStorage.PelIntoStorage() = 0.0;
				ElecStorage.QdotConvZone() = 0.0;
				ElecStorage.QdotRadZone() = 0.0;
				ElecStorage.TimeElapsed() = 0.0;
				ElecStorage.ElectEnergyinStorage() = 0.0;
				ElecStorage.StoredPower() = 0.0;
				ElecStorage.StoredEnergy() = 0.0;
				ElecStorage.DecrementedEnergyStored() = 0.0;
				ElecStorage.DrawnPower() = 0.0;
				ElecStorage.DrawnEnergy() = 0.0;
				ElecStorage.ThermLossRate() = 0.0;
				ElecStorage.ThermLossEnergy() = 0.0;
			}
			MyEnvrnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		// Determine the demand from the simulation for Demand Limit and Track Electrical and Reporting
		ElecFacilityBldg = GetInstantMeterValue( ElecFacilityIndex, 1 );
		ElecFacilityHVAC = GetInstantMeterValue( ElecFacilityIndex, 2 );
		// deprecate this PV stuff?
		ElecProducedPV = GetInstantMeterValue( ElecProducedPVIndex, 2 );
		ElecProducedWT = GetInstantMeterValue( ElecProducedWTIndex, 2 );

		WholeBldgElectSummary.TotalBldgElecDemand = ElecFacilityBldg / TimeStepZoneSec;
		WholeBldgElectSummary.TotalHVACElecDemand = ElecFacilityHVAC / ( TimeStepSys * SecInHour );
		WholeBldgElectSummary.TotalElectricDemand = WholeBldgElectSummary.TotalBldgElecDemand + WholeBldgElectSummary.TotalHVACElecDemand;
		WholeBldgElectSummary.ElecProducedPVRate = ElecProducedPV / ( TimeStepSys * SecInHour );
		WholeBldgElectSummary.ElecProducedWTRate = ElecProducedWT / ( TimeStepSys * SecInHour );

		WholeBldgRemainingLoad = WholeBldgElectSummary.TotalElectricDemand; //- WholeBldgElectSummary%ElecProducedPVRate

		if ( UpdateMetersOnly ) { // just update record keeping, don't resimulate load centers
			ManageTransformers();

			UpdateWholeBuildingRecords();
			return;
		}

		// dispatch across load centers and generators keeping track of remaining whole building load.

		for ( LoadCenterNum = 1; LoadCenterNum <= NumLoadCenters; ++LoadCenterNum ) {

			if ( ( ElecLoadCenter( LoadCenterNum ).DemandMeterPtr == 0 ) && ( ElecLoadCenter( LoadCenterNum ).OperationScheme == iOpSchemeTrackMeter ) ) { // keep trying to setup
				ElecLoadCenter( LoadCenterNum ).DemandMeterPtr = GetMeterIndex( ElecLoadCenter( LoadCenterNum ).DemandMeterName );
			}

			ElecLoadCenter( LoadCenterNum ).TotalPowerRequest = 0.0;
			ElecLoadCenter( LoadCenterNum ).TotalThermalPowerRequest = 0.0;

			// Check Operation Scheme and assign power generation load
			// Both the Demand Limit and Track Electrical schemes will sequentially load the available generators.  All demand
			// not met by available generator capacity will be met by purchased electrical.
			// If a generator is needed in the simulation for a small load and it is less than the minimum part load ratio
			// the generator will operate at the minimum part load ratio and the excess will either reduce demand or
			// be available for storage or sell back to the power company.
			{ auto const SELECT_CASE_var( ElecLoadCenter( LoadCenterNum ).OperationScheme );

			if ( SELECT_CASE_var == iOpSchemeBaseLoad ) { // 'BASELOAD'

				LoadCenterElectricLoad = WholeBldgRemainingLoad;

				for ( GenNum = 1; GenNum <= ElecLoadCenter( LoadCenterNum ).NumGenerators; ++GenNum ) {

					if ( GetCurrentScheduleValue( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).AvailSchedPtr ) > 0.0 ) {
						// Set the Operation Flag
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
						// Set the electric generator load request
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut;
					} else {
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = 0.0;
					}

					// now handle EMS override
					if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
						} else {
							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
						}
					}

					// Get generator's actual electrical and thermal power outputs
					GeneratorPowerOutput( LoadCenterNum, GenNum, FirstHVACIteration, ElectricProdRate, ThermalProdRate );

					ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep;

					WholeBldgRemainingLoad -= ElectricProdRate; // Update whole building remaining load
				}

			} else if ( SELECT_CASE_var == iOpSchemeDemandLimit ) { // 'DEMAND LIMIT'
				// The Demand Limit scheme tries to have the generators meet all of the demand above the purchased Electric
				//  limit set by the user.
				RemainingLoad = WholeBldgRemainingLoad - ElecLoadCenter( LoadCenterNum ).DemandLimit;
				LoadCenterElectricLoad = RemainingLoad;

				for ( GenNum = 1; GenNum <= ElecLoadCenter( LoadCenterNum ).NumGenerators; ++GenNum ) {

					if ( GetCurrentScheduleValue( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).AvailSchedPtr ) > 0.0 && RemainingLoad > 0.0 ) {
						// Set the Operation Flag
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;

						// Set the electric generator load
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = min( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut, RemainingLoad );

						// now handle EMS override
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
							if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
							} else {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
							}
						}

					} else {
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = 0.0;

						// now handle EMS override
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
							if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
							} else {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
							}
						}
					}

					// Get generator's actual electrical and thermal power outputs
					GeneratorPowerOutput( LoadCenterNum, GenNum, FirstHVACIteration, ElectricProdRate, ThermalProdRate );

					if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
						ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
					} else {
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
							ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut;
							ElecLoadCenter( LoadCenterNum ).TotalPowerRequest = min( LoadCenterElectricLoad, ElecLoadCenter( LoadCenterNum ).TotalPowerRequest );
						}
					}
					RemainingLoad -= ElectricProdRate; // Update remaining load to be met by this load center
					WholeBldgRemainingLoad -= ElectricProdRate; // Update whole building remaining load

				}

			} else if ( SELECT_CASE_var == iOpSchemeTrackElectrical ) { // 'TRACK ELECTRICAL'
				//The Track Electrical scheme tries to have the generators meet all of the electrical demand for the building.
				RemainingLoad = WholeBldgRemainingLoad;
				LoadCenterElectricLoad = RemainingLoad;

				for ( GenNum = 1; GenNum <= ElecLoadCenter( LoadCenterNum ).NumGenerators; ++GenNum ) {

					if ( GetCurrentScheduleValue( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).AvailSchedPtr ) > 0.0 && RemainingLoad > 0.0 ) {
						// Set the Operation Flag
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;

						// Set the electric generator load
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = min( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut, RemainingLoad );

						// now handle EMS override
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
							if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
							} else {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
							}
						}

					} else {
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = 0.0;

						// now handle EMS override
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
							if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
							} else {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
							}
						}
					}

					// Get generator's actual electrical and thermal power outputs
					GeneratorPowerOutput( LoadCenterNum, GenNum, FirstHVACIteration, ElectricProdRate, ThermalProdRate );

					if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
						ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
					} else {
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
							ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut;
							ElecLoadCenter( LoadCenterNum ).TotalPowerRequest = min( LoadCenterElectricLoad, ElecLoadCenter( LoadCenterNum ).TotalPowerRequest );
						}
					}
					RemainingLoad -= ElectricProdRate; // Update remaining load to be met by this load center
					WholeBldgRemainingLoad -= ElectricProdRate; // Update whole building remaining load

				}

			} else if ( SELECT_CASE_var == iOpSchemeTrackSchedule ) { // 'TRACK SCHEDULE'
				// The Track Schedule scheme tries to have the generators meet the electrical demand determined from a schedule.
				//  Code is very similar to 'Track Electrical' except for initial RemainingLoad is replaced by SchedElecDemand
				//  and PV production is ignored.
				RemainingLoad = GetCurrentScheduleValue( ElecLoadCenter( LoadCenterNum ).TrackSchedPtr );
				LoadCenterElectricLoad = RemainingLoad;

				for ( GenNum = 1; GenNum <= ElecLoadCenter( LoadCenterNum ).NumGenerators; ++GenNum ) {

					if ( GetCurrentScheduleValue( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).AvailSchedPtr ) > 0.0 && RemainingLoad > 0.0 ) {
						// Set the Operation Flag
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;

						// Set the electric generator load
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = min( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut, RemainingLoad );

						// now handle EMS override
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
							if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
							} else {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
							}
						}
					} else {
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = 0.0;

						// now handle EMS override
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
							if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
							} else {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
							}
						}
					}

					// Get generator's actual electrical and thermal power outputs
					GeneratorPowerOutput( LoadCenterNum, GenNum, FirstHVACIteration, ElectricProdRate, ThermalProdRate );

					if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
						ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
					} else {
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
							ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut;
							ElecLoadCenter( LoadCenterNum ).TotalPowerRequest = min( LoadCenterElectricLoad, ElecLoadCenter( LoadCenterNum ).TotalPowerRequest );
						}
					}
					RemainingLoad -= ElectricProdRate; // Update remaining load to be met by this load center
					WholeBldgRemainingLoad -= ElectricProdRate; // Update whole building remaining load

				}

			} else if ( SELECT_CASE_var == iOpSchemeTrackMeter ) { // 'TRACK METER'
				// The TRACK CUSTOM METER scheme tries to have the generators meet all of the
				//   electrical demand from a meter, it can also be a user-defined Custom Meter
				//   and PV is ignored.
				CustomMeterDemand = GetInstantMeterValue( ElecLoadCenter( LoadCenterNum ).DemandMeterPtr, 1 ) / TimeStepZoneSec + GetInstantMeterValue( ElecLoadCenter( LoadCenterNum ).DemandMeterPtr, 2 ) / ( TimeStepSys * SecInHour );

				RemainingLoad = CustomMeterDemand;
				LoadCenterElectricLoad = RemainingLoad;

				for ( GenNum = 1; GenNum <= ElecLoadCenter( LoadCenterNum ).NumGenerators; ++GenNum ) {

					if ( GetCurrentScheduleValue( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).AvailSchedPtr ) > 0.0 && RemainingLoad > 0.0 ) {
						// Set the Operation Flag
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
						// Set the electric generator load
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = min( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut, RemainingLoad );

						// now handle EMS override
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
							if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
							} else {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
							}
						}

					} else {
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = 0.0;

						// now handle EMS override
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
							if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
							} else {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
							}
						}
					}

					// Get generator's actual electrical and thermal power outputs
					GeneratorPowerOutput( LoadCenterNum, GenNum, FirstHVACIteration, ElectricProdRate, ThermalProdRate );

					if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
						ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
					} else {
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
							ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut;
							ElecLoadCenter( LoadCenterNum ).TotalPowerRequest = min( LoadCenterElectricLoad, ElecLoadCenter( LoadCenterNum ).TotalPowerRequest );
						}
					}
					RemainingLoad -= ElectricProdRate; // Update remaining load to be met by this load center
					WholeBldgRemainingLoad -= ElectricProdRate; // Update whole building remaining load

				}

			} else if ( SELECT_CASE_var == iOpSchemeThermalFollow ) {
				// Turn thermal load into an electrical load for cogenerators controlled to follow heat loads
				RemainingThermalLoad = 0.0;

				CalcLoadCenterThermalLoad( FirstHVACIteration, LoadCenterNum, RemainingThermalLoad );
				LoadCenterThermalLoad = RemainingThermalLoad;

				for ( GenNum = 1; GenNum <= ElecLoadCenter( LoadCenterNum ).NumGenerators; ++GenNum ) {

					if ( GetCurrentScheduleValue( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).AvailSchedPtr ) > 0.0 && RemainingThermalLoad > 0.0 ) {

						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).NominalThermElectRatio > 0.0 ) {

							RemainingLoad = RemainingThermalLoad / ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).NominalThermElectRatio;

							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = min( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut, RemainingLoad );

							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
							// now handle EMS override
							if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
								if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
									ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
								} else {
									ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
								}
							}

						}
					} else {
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = 0.0;

						// now handle EMS override
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
							if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
							} else {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
							}
						}
					}

					// Get generator's actual electrical and thermal power outputs
					GeneratorPowerOutput( LoadCenterNum, GenNum, FirstHVACIteration, ElectricProdRate, ThermalProdRate );

					if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {

						ElecLoadCenter( LoadCenterNum ).TotalThermalPowerRequest += ( max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 ) ) * ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).NominalThermElectRatio;
						ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += ( max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 ) );
					} else {

						if ( ElecLoadCenter( LoadCenterNum ).TotalThermalPowerRequest < LoadCenterThermalLoad && ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {

							ExcessThermalPowerRequest = ElecLoadCenter( LoadCenterNum ).TotalThermalPowerRequest + ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut * ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).NominalThermElectRatio - LoadCenterThermalLoad;

							if ( ExcessThermalPowerRequest < 0.0 ) {
								ElecLoadCenter( LoadCenterNum ).TotalThermalPowerRequest += ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut * ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).NominalThermElectRatio;
								ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut;
							} else {
								ElecLoadCenter( LoadCenterNum ).TotalThermalPowerRequest = LoadCenterThermalLoad;
								if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).NominalThermElectRatio > 0.0 ) {
									ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut - ( ExcessThermalPowerRequest / ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).NominalThermElectRatio );
								}
							}

						}

					}

					RemainingThermalLoad -= ThermalProdRate; // Update remaining load to be met
					// by this load center
					WholeBldgRemainingLoad -= ElectricProdRate; // Update whole building remaining load

				}

			} else if ( SELECT_CASE_var == iOpSchemeThermalFollowLimitElectrical ) {
				//  Turn a thermal load into an electrical load for cogenerators controlled to follow heat loads.
				//  Add intitialization of RemainingThermalLoad as in the ThermalFollow operating scheme above.
				CalcLoadCenterThermalLoad( FirstHVACIteration, LoadCenterNum, RemainingThermalLoad );
				// Total current electrical demand for the building is a secondary limit.
				RemainingLoad = WholeBldgRemainingLoad;
				LoadCenterElectricLoad = WholeBldgRemainingLoad;
				LoadCenterThermalLoad = RemainingThermalLoad;

				for ( GenNum = 1; GenNum <= ElecLoadCenter( LoadCenterNum ).NumGenerators; ++GenNum ) {

					if ( ( GetCurrentScheduleValue( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).AvailSchedPtr ) > 0.0 ) && ( RemainingThermalLoad > 0.0 ) && ( RemainingLoad > 0.0 ) ) {

						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).NominalThermElectRatio > 0.0 ) {

							RemainingLoad = min( WholeBldgRemainingLoad, RemainingThermalLoad / ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).NominalThermElectRatio );

							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = min( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut, RemainingLoad );

							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
							// now handle EMS override
							if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
								if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
									ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
								} else {
									ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
								}
							}

						}

					} else {

						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
						ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = 0.0;
						// now handle EMS override
						if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {
							ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep = max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 );
							if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = true;
							} else {
								ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep = false;
							}
						}

					}

					// Get generator's actual electrical and thermal power outputs
					GeneratorPowerOutput( LoadCenterNum, GenNum, FirstHVACIteration, ElectricProdRate, ThermalProdRate );

					if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSRequestOn ) {

						ElecLoadCenter( LoadCenterNum ).TotalThermalPowerRequest += ( max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 ) ) * ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).NominalThermElectRatio;
						ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += ( max( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).EMSPowerRequest, 0.0 ) );
					} else {

						if ( ElecLoadCenter( LoadCenterNum ).TotalThermalPowerRequest < LoadCenterThermalLoad && ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep > 0.0 ) {

							ExcessThermalPowerRequest = ElecLoadCenter( LoadCenterNum ).TotalThermalPowerRequest + ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut * ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).NominalThermElectRatio - LoadCenterThermalLoad;

							if ( ExcessThermalPowerRequest < 0.0 ) {
								ElecLoadCenter( LoadCenterNum ).TotalThermalPowerRequest += ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut * ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).NominalThermElectRatio;
								ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut;
							} else {
								ElecLoadCenter( LoadCenterNum ).TotalThermalPowerRequest = LoadCenterThermalLoad;
								if ( ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).NominalThermElectRatio > 0.0 ) {
									ElecLoadCenter( LoadCenterNum ).TotalPowerRequest += ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).MaxPowerOut - ( ExcessThermalPowerRequest / ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).NominalThermElectRatio );
								}
							}

							ElecLoadCenter( LoadCenterNum ).TotalPowerRequest = min( LoadCenterElectricLoad, ElecLoadCenter( LoadCenterNum ).TotalPowerRequest );

						}

					}

					RemainingThermalLoad -= ThermalProdRate; // Update remaining thermal load to
					// be met by this load center

					WholeBldgRemainingLoad -= ElectricProdRate; // Update whole building remaining
					// electric load
				}

			} else if ( SELECT_CASE_var == 0 ) { // This case allows for the reporting to be done without generators specified.

			} else {
				ShowFatalError( "Invalid operation scheme type for Electric Load Center=" + ElecLoadCenter( LoadCenterNum ).Name );

			}} // TypeOfEquip

			ElecLoadCenter( LoadCenterNum ).ElectDemand = LoadCenterElectricLoad; //To obtain the load for transformer

			if ( ( ElecLoadCenter( LoadCenterNum ).StoragePresent ) && ( ElecLoadCenter( LoadCenterNum ).BussType == DCBussInverterDCStorage ) ) {
				ManageElectCenterStorageInteractions( LoadCenterNum, StorageDrawnPower, StorageStoredPower );
				//     Adjust whole building electric demand based on storage inputs and outputs
				WholeBldgRemainingLoad = WholeBldgRemainingLoad - StorageDrawnPower + StorageStoredPower;
			}

			if ( ElecLoadCenter( LoadCenterNum ).InverterPresent ) ManageInverter( LoadCenterNum );
			if ( ( ElecLoadCenter( LoadCenterNum ).StoragePresent ) && ( ( ElecLoadCenter( LoadCenterNum ).BussType == DCBussInverterACStorage ) || ( ElecLoadCenter( LoadCenterNum ).BussType == ACBussStorage ) ) ) {
				ManageElectCenterStorageInteractions( LoadCenterNum, StorageDrawnPower, StorageStoredPower );
				WholeBldgRemainingLoad = WholeBldgRemainingLoad - StorageDrawnPower + StorageStoredPower;
			}

			UpdateLoadCenterRecords( LoadCenterNum );

		} //End of Load Center Do Loop

		// The transformer call should be put outside of the "Load Center" loop because
		// 1) A transformer may be for utility, not for load center
		// 2) A tansformer may be shared by multiple load centers
		ManageTransformers();

		UpdateWholeBuildingRecords();

		// Need to simulate through the Elec Manager at least twice to ensure that Heat Recovery information is included.
		// recheck this, may not be needed now that load centers are called more often.
		//  Does the IF condition also need to check if any thermal following strategies have been specified?
		//  That is, if only electrical following schemes, don't need to resimulate?
		if ( FirstHVACIteration ) {
			SimElecCircuits = true;
		} else {
			SimElecCircuits = false;
		}

	}

	void
	GetPowerManagerInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   September 2000
		//       MODIFIED       B. Griffith, 2008 multiple load centers, inverter, storage
		//                      W. Wang, 2010 transformer
		//                      Y. KyungTae & W. Wang July-August, 2011 Add a battery model
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads the load center data
		// attributes from the input file

		// METHODOLOGY EMPLOYED:
		// calls the Input Processor to retrieve data from input file.
		// The format of the Energy+.idd (the EnergyPlus input data dictionary) for the
		// following keywords is reflected exactly in this subroutine:

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::MakeUPPERCase;
		using InputProcessor::GetObjectDefMaxArgs;
		using namespace DataIPShortCuts;
		using ScheduleManager::GetScheduleIndex;
		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using DataHeatBalance::Zone;
		using DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterSimple;
		using DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower;
		using DataHeatBalance::IntGainTypeOf_ElectricLoadCenterInverterLookUpTable;
		using DataHeatBalance::IntGainTypeOf_ElectricLoadCenterStorageSimple;
		using DataHeatBalance::IntGainTypeOf_ElectricLoadCenterStorageBattery;
		using DataHeatBalance::IntGainTypeOf_ElectricLoadCenterTransformer;
		using DataGlobals::NumOfZones;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using DataGlobals::ScheduleAlwaysOn;
		using General::RoundSigDigits;
		using namespace OutputReportPredefined;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetPowerManagerInput: " );
		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int NumGenLists; // Number of generator lists
		int AlphaCount; // alpha input index
		int GenCount; // generator counter index
		int Count; // loop index
		int ListNum; // List number index
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		//CHARACTER(len=MaxNameLength),DIMENSION(:), ALLOCATABLE :: Alpha !dimension to num of alpha fields in input
		//REAL(r64), DIMENSION(:), ALLOCATABLE   :: Num                   !dimension to num of numeric data fields in input
		static bool ErrorsFound( false ); // error in input
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		Array1D_string ListName;
		Array1D_string InverterNames;
		Array1D_string StorageNames;
		Array1D_string TransformerNames;
		int AnyElectricityPresent; // local test for presence of Electricty in Facility
		int NumGenerators; // local number of generators per electric load center
		bool SetupWholeBldgReports;
		//unused1208  INTEGER  :: MaxNumAlphas
		//unused1208  INTEGER  :: MaxNumArgs
		//unused1208  INTEGER  :: MaxNumNumbers
		int NumofCECinverters;
		int NumofCurveInverters;
		int NumofSimpleInverters;
		int NumofSimpleElecStorage;
		int NumofKiBaMElecStorage;
		int InvertNum;
		int StorNum;
		int TransfNum;
		int Found;
		int NumAlphaBeforeMeter; // Number of Alpha fields before the extensible meters
		//Used to derive the number of meters wired to a transformer
		int NumWiredMeters; // Number of electric meters wired to a transformer
		int LCofTransformer; // Index of load center served by a transformer
		int LoopCount; // loop counter
		static Real64 pvTotalCapacity( 0.0 ); // for LEED report
		static Real64 windTotalCapacity( 0.0 ); // for LEED report

		NumAlphaBeforeMeter = 7; //Hard coded. Changes might be needed if the transformer input structure gets changed
		LCofTransformer = 0;

		//FLOW:
		SetupWholeBldgReports = false;

		//first read in any inverters that might be associated with a load center
		NumofCECinverters = GetNumObjectsFound( "ElectricLoadCenter:Inverter:LookUpTable" );
		NumofCurveInverters = GetNumObjectsFound( "ElectricLoadCenter:Inverter:FunctionOfPower" );
		NumofSimpleInverters = GetNumObjectsFound( "ElectricLoadCenter:Inverter:Simple" );
		NumInverters = NumofCECinverters + NumofCurveInverters + NumofSimpleInverters;

		if ( NumInverters > 0 ) {
			Inverter.allocate( NumInverters );
			InverterNames.allocate( NumInverters );

			if ( NumofCECinverters > 0 ) {
				cCurrentModuleObject = "ElectricLoadCenter:Inverter:LookUpTable";
				for ( InvertNum = 1; InvertNum <= NumofCECinverters; ++InvertNum ) {
					GetObjectItem( cCurrentModuleObject, InvertNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), InverterNames, InvertNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}
					InverterNames( InvertNum ) = cAlphaArgs( 1 );
					Inverter( InvertNum ).Name = cAlphaArgs( 1 );
					Inverter( InvertNum ).ModelType = CECLookUpTableModel;

					if ( lAlphaFieldBlanks( 2 ) ) {
						Inverter( InvertNum ).AvailSchedPtr = ScheduleAlwaysOn;
					} else {
						Inverter( InvertNum ).AvailSchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
						if ( Inverter( InvertNum ).AvailSchedPtr == 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
							ErrorsFound = true;
						}
					}

					Inverter( InvertNum ).ZoneNum = FindItemInList( cAlphaArgs( 3 ), Zone.Name(), NumOfZones );
					if ( Inverter( InvertNum ).ZoneNum > 0 ) Inverter( InvertNum ).HeatLossesDestination = ZoneGains;
					if ( Inverter( InvertNum ).ZoneNum == 0 ) {
						if ( lAlphaFieldBlanks( 3 ) ) {
							Inverter( InvertNum ).HeatLossesDestination = LostToOutside;
						} else {
							Inverter( InvertNum ).HeatLossesDestination = LostToOutside;
							ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
							ShowContinueError( "Zone name not found. Inverter heat losses will not be added to a zone" );
							// continue with simulation but inverter losses not sent to a zone.
						}
					}
					Inverter( InvertNum ).ZoneRadFract = rNumericArgs( 1 );
					Inverter( InvertNum ).RatedPower = rNumericArgs( 2 );
					Inverter( InvertNum ).StandbyPower = rNumericArgs( 3 );
					Inverter( InvertNum ).LUTable.NightTareLossPower = rNumericArgs( 3 );
					Inverter( InvertNum ).LUTable.NominalVoltage = rNumericArgs( 4 );

					Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 1 ) = rNumericArgs( 5 );
					Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 2 ) = rNumericArgs( 6 );
					Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 3 ) = rNumericArgs( 7 );
					Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 4 ) = rNumericArgs( 8 );
					Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 5 ) = rNumericArgs( 9 );
					Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 6 ) = rNumericArgs( 10 );

				}
			}

			if ( NumofCurveInverters > 0 ) {
				cCurrentModuleObject = "ElectricLoadCenter:Inverter:FunctionOfPower";
				for ( InvertNum = NumofCECinverters + 1; InvertNum <= NumofCECinverters + NumofCurveInverters; ++InvertNum ) {
					GetObjectItem( cCurrentModuleObject, InvertNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), InverterNames, InvertNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
					}
					InverterNames( InvertNum ) = cAlphaArgs( 1 );
					Inverter( InvertNum ).ModelType = CurveFuncOfPower;
					Inverter( InvertNum ).Name = cAlphaArgs( 1 );

					if ( lAlphaFieldBlanks( 2 ) ) {
						Inverter( InvertNum ).AvailSchedPtr = ScheduleAlwaysOn;
					} else {
						Inverter( InvertNum ).AvailSchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
						if ( Inverter( InvertNum ).AvailSchedPtr == 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
							ErrorsFound = true;
						}
					}

					Inverter( InvertNum ).ZoneNum = FindItemInList( cAlphaArgs( 3 ), Zone.Name(), NumOfZones );
					if ( Inverter( InvertNum ).ZoneNum > 0 ) Inverter( InvertNum ).HeatLossesDestination = ZoneGains;
					if ( Inverter( InvertNum ).ZoneNum == 0 ) {
						if ( lAlphaFieldBlanks( 3 ) ) {
							Inverter( InvertNum ).HeatLossesDestination = LostToOutside;
						} else {
							Inverter( InvertNum ).HeatLossesDestination = LostToOutside;
							ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
							// continue with simulation but inverter losses not sent to a zone.
							ShowContinueError( "Zone name not found. Inverter heat losses will not be added to a zone" );
						}
					}
					Inverter( InvertNum ).CurveNum = GetCurveIndex( cAlphaArgs( 4 ) );
					if ( Inverter( InvertNum ).CurveNum == 0 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 4 ) + " = " + cAlphaArgs( 4 ) );
						ShowContinueError( "Curve was not found" );
						ErrorsFound = true;
					}
					Inverter( InvertNum ).ZoneRadFract = rNumericArgs( 1 );
					Inverter( InvertNum ).RatedPower = rNumericArgs( 2 );
					Inverter( InvertNum ).MinEfficiency = rNumericArgs( 3 );
					Inverter( InvertNum ).MaxEfficiency = rNumericArgs( 4 );
					Inverter( InvertNum ).MinPower = rNumericArgs( 5 );
					Inverter( InvertNum ).MaxPower = rNumericArgs( 6 );
					Inverter( InvertNum ).StandbyPower = rNumericArgs( 7 );

				}
			}

			if ( NumofSimpleInverters > 0 ) {
				cCurrentModuleObject = "ElectricLoadCenter:Inverter:Simple";
				for ( InvertNum = NumofCECinverters + NumofCurveInverters + 1; InvertNum <= NumInverters; ++InvertNum ) {
					GetObjectItem( cCurrentModuleObject, InvertNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), InverterNames, InvertNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxx";
					}
					InverterNames( InvertNum ) = cAlphaArgs( 1 );
					Inverter( InvertNum ).Name = cAlphaArgs( 1 );
					Inverter( InvertNum ).ModelType = SimpleConstantEff;

					if ( lAlphaFieldBlanks( 2 ) ) {
						Inverter( InvertNum ).AvailSchedPtr = ScheduleAlwaysOn;
					} else {
						Inverter( InvertNum ).AvailSchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
						if ( Inverter( InvertNum ).AvailSchedPtr == 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
							ErrorsFound = true;
						}
					}

					Inverter( InvertNum ).ZoneNum = FindItemInList( cAlphaArgs( 3 ), Zone.Name(), NumOfZones );
					if ( Inverter( InvertNum ).ZoneNum > 0 ) Inverter( InvertNum ).HeatLossesDestination = ZoneGains;
					if ( Inverter( InvertNum ).ZoneNum == 0 ) {
						if ( lAlphaFieldBlanks( 3 ) ) {
							Inverter( InvertNum ).HeatLossesDestination = LostToOutside;
						} else {
							ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
							ShowContinueError( "Zone name not found. Inverter heat losses will not be added to a zone" );
							// continue with simulation but inverter losses not sent to a zone.
						}
					}
					Inverter( InvertNum ).ZoneRadFract = rNumericArgs( 1 );
					Inverter( InvertNum ).Efficiency = rNumericArgs( 2 );
				}
			}

			//setup reports for all inverters
			for ( InvertNum = 1; InvertNum <= NumInverters; ++InvertNum ) {
				SetupOutputVariable( "Inverter DC to AC Efficiency []", Inverter( InvertNum ).Efficiency, "System", "Average", Inverter( InvertNum ).Name );
				SetupOutputVariable( "Inverter DC Input Electric Power [W]", Inverter( InvertNum ).DCPowerIn, "System", "Average", Inverter( InvertNum ).Name );
				SetupOutputVariable( "Inverter DC Input Electric Energy [J]", Inverter( InvertNum ).DCEnergyIn, "System", "Sum", Inverter( InvertNum ).Name );
				SetupOutputVariable( "Inverter AC Output Electric Power [W]", Inverter( InvertNum ).ACPowerOut, "System", "Average", Inverter( InvertNum ).Name );
				SetupOutputVariable( "Inverter AC Output Electric Energy [J]", Inverter( InvertNum ).ACEnergyOut, "System", "Sum", Inverter( InvertNum ).Name, _, "ElectricityProduced", "Photovoltaics", _, "Plant" ); // right now PV is the only DC source
				SetupOutputVariable( "Inverter Thermal Loss Rate [W]", Inverter( InvertNum ).ThermLossRate, "System", "Average", Inverter( InvertNum ).Name );
				SetupOutputVariable( "Inverter Thermal Loss Energy [J]", Inverter( InvertNum ).ThermLossEnergy, "System", "Sum", Inverter( InvertNum ).Name );
				SetupOutputVariable( "Inverter Ancillary AC Electric Power [W]", Inverter( InvertNum ).AncillACuseRate, "System", "Average", Inverter( InvertNum ).Name );
				SetupOutputVariable( "Inverter Ancillary AC Electric Energy [J]", Inverter( InvertNum ).AncillACuseEnergy, "System", "Sum", Inverter( InvertNum ).Name, _, "Electricity", "Cogeneration", _, "Plant" ); // called cogeneration for end use table
				if ( Inverter( InvertNum ).ZoneNum > 0 ) {
					{ auto const SELECT_CASE_var( Inverter( InvertNum ).ModelType );
					if ( SELECT_CASE_var == SimpleConstantEff ) {
						SetupZoneInternalGain( Inverter( InvertNum ).ZoneNum, "ElectricLoadCenter:Inverter:Simple", Inverter( InvertNum ).Name, IntGainTypeOf_ElectricLoadCenterInverterSimple, Inverter( InvertNum ).QdotConvZone, _, Inverter( InvertNum ).QdotRadZone );
					} else if ( SELECT_CASE_var == CurveFuncOfPower ) {
						SetupZoneInternalGain( Inverter( InvertNum ).ZoneNum, "ElectricLoadCenter:Inverter:FunctionOfPower", Inverter( InvertNum ).Name, IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower, Inverter( InvertNum ).QdotConvZone, _, Inverter( InvertNum ).QdotRadZone );
					} else if ( SELECT_CASE_var == CECLookUpTableModel ) {
						SetupZoneInternalGain( Inverter( InvertNum ).ZoneNum, "ElectricLoadCenter:Inverter:LookUpTable", Inverter( InvertNum ).Name, IntGainTypeOf_ElectricLoadCenterInverterLookUpTable, Inverter( InvertNum ).QdotConvZone, _, Inverter( InvertNum ).QdotRadZone );
					}}
				}

			}

		} //any inverters

		//read in any electrical storage devices that may be associated with load centers.
		NumofSimpleElecStorage = GetNumObjectsFound( "ElectricLoadCenter:Storage:Simple" );
		NumofKiBaMElecStorage = GetNumObjectsFound( "ElectricLoadCenter:Storage:Battery" );
		NumElecStorageDevices = NumofSimpleElecStorage + NumofKiBaMElecStorage;
		if ( NumElecStorageDevices > 0 ) {
			ElecStorage.allocate( NumElecStorageDevices );
			StorageNames.allocate( NumElecStorageDevices );

			if ( NumofSimpleElecStorage > 0 ) {
				cCurrentModuleObject = "ElectricLoadCenter:Storage:Simple";
				for ( StorNum = 1; StorNum <= NumofSimpleElecStorage; ++StorNum ) {
					GetObjectItem( cCurrentModuleObject, StorNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), StorageNames, StorNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxx";
					}
					StorageNames( StorNum ) = cAlphaArgs( 1 );
					ElecStorage( StorNum ).Name = cAlphaArgs( 1 );

					if ( lAlphaFieldBlanks( 2 ) ) {
						ElecStorage( StorNum ).AvailSchedPtr = ScheduleAlwaysOn;
					} else {
						ElecStorage( StorNum ).AvailSchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
						if ( ElecStorage( StorNum ).AvailSchedPtr == 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
							ErrorsFound = true;
						}
					}

					ElecStorage( StorNum ).ZoneNum = FindItemInList( cAlphaArgs( 3 ), Zone.Name(), NumOfZones );
					if ( ElecStorage( StorNum ).ZoneNum > 0 ) ElecStorage( StorNum ).HeatLossesDestination = ZoneGains;
					if ( ElecStorage( StorNum ).ZoneNum == 0 ) {
						if ( lAlphaFieldBlanks( 3 ) ) {
							ElecStorage( StorNum ).HeatLossesDestination = LostToOutside;
						} else {
							ElecStorage( StorNum ).HeatLossesDestination = LostToOutside;
							ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
							ShowContinueError( "Zone name not found. Electrical storage heat losses will not be added to a zone" );
							//continue with simulation but storage losses not sent to a zone.
						}
					}

					ElecStorage( StorNum ).StorageModelMode = SimpleBucketStorage;
					ElecStorage( StorNum ).ZoneRadFract = rNumericArgs( 1 );
					ElecStorage( StorNum ).EnergeticEfficCharge = rNumericArgs( 2 );
					ElecStorage( StorNum ).EnergeticEfficDischarge = rNumericArgs( 3 );
					ElecStorage( StorNum ).MaxEnergyCapacity = rNumericArgs( 4 );
					ElecStorage( StorNum ).MaxPowerDraw = rNumericArgs( 5 );
					ElecStorage( StorNum ).MaxPowerStore = rNumericArgs( 6 );
					ElecStorage( StorNum ).StartingEnergyStored = rNumericArgs( 7 );

					SetupOutputVariable( "Electric Storage Charge State [J]", ElecStorage( StorNum ).ElectEnergyinStorage, "System", "Average", ElecStorage( StorNum ).Name ); //? 'Sum'

				}

			} //any simple storage

			if ( NumofKiBaMElecStorage > 0 ) {
				cCurrentModuleObject = "ElectricLoadCenter:Storage:Battery";
				for ( StorNum = 1 + NumofSimpleElecStorage; StorNum <= NumofKiBaMElecStorage; ++StorNum ) {
					GetObjectItem( cCurrentModuleObject, StorNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
					IsNotOK = false;
					IsBlank = false;
					VerifyName( cAlphaArgs( 1 ), StorageNames, StorNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
					if ( IsNotOK ) {
						ErrorsFound = true;
						if ( IsBlank ) cAlphaArgs( 1 ) = "xxxx";
					}
					StorageNames( StorNum ) = cAlphaArgs( 1 );
					ElecStorage( StorNum ).Name = cAlphaArgs( 1 );

					if ( lAlphaFieldBlanks( 2 ) ) {
						ElecStorage( StorNum ).AvailSchedPtr = ScheduleAlwaysOn;
					} else {
						ElecStorage( StorNum ).AvailSchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
						if ( ElecStorage( StorNum ).AvailSchedPtr == 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
							ErrorsFound = true;
						}
					}

					ElecStorage( StorNum ).ZoneNum = FindItemInList( cAlphaArgs( 3 ), Zone.Name(), NumOfZones );
					if ( ElecStorage( StorNum ).ZoneNum > 0 ) ElecStorage( StorNum ).HeatLossesDestination = ZoneGains;
					if ( ElecStorage( StorNum ).ZoneNum == 0 ) {
						if ( lAlphaFieldBlanks( 3 ) ) {
							ElecStorage( StorNum ).HeatLossesDestination = LostToOutside;
						} else {
							ElecStorage( StorNum ).HeatLossesDestination = LostToOutside;
							ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
							ShowContinueError( "Zone name not found. Electrical storage heat losses will not be added to a zone" );
							//continue with simulation but storage losses not sent to a zone.
						}
					}

					ElecStorage( StorNum ).ChargeCurveNum = GetCurveIndex( cAlphaArgs( 4 ) ); //voltage calculation for charging
					if ( ElecStorage( StorNum ).ChargeCurveNum == 0 && ! lAlphaFieldBlanks( 4 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
						ErrorsFound = true;
					} else if ( lAlphaFieldBlanks( 4 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 4 ) + " cannot be blank. But no entry found." );
						ErrorsFound = true;
					} else if ( ! SameString( GetCurveType( ElecStorage( StorNum ).ChargeCurveNum ), "RectangularHyperbola2" ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 4 ) + '=' + cAlphaArgs( 4 ) );
						ShowContinueError( "Curve Type must be RectangularHyperbola2 but was " + GetCurveType( ElecStorage( StorNum ).ChargeCurveNum ) );
						ErrorsFound = true;
					}
					ElecStorage( StorNum ).DischargeCurveNum = GetCurveIndex( cAlphaArgs( 5 ) ); // voltage calculation for discharging
					if ( ElecStorage( StorNum ).DischargeCurveNum == 0 && ! lAlphaFieldBlanks( 5 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
						ErrorsFound = true;
					} else if ( lAlphaFieldBlanks( 5 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 5 ) + " cannot be blank. But no entry found." );
						ErrorsFound = true;
					} else if ( ! SameString( GetCurveType( ElecStorage( StorNum ).DischargeCurveNum ), "RectangularHyperbola2" ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 5 ) + '=' + cAlphaArgs( 5 ) );
						ShowContinueError( "Curve Type must be RectangularHyperbola2 but was " + GetCurveType( ElecStorage( StorNum ).DischargeCurveNum ) );
						ErrorsFound = true;
					}

					if ( SameString( cAlphaArgs( 6 ), "Yes" ) ) {
						ElecStorage( StorNum ).LifeCalculation = Battery_LifeCalculation_Yes;
					} else if ( SameString( cAlphaArgs( 6 ), "No" ) ) {
						ElecStorage( StorNum ).LifeCalculation = Battery_LifeCalculation_No;
					} else {
						ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 6 ) + " = " + cAlphaArgs( 6 ) );
						ShowContinueError( "Yes or No should be selected. Default value No is used to continue simulation" );
						ElecStorage( StorNum ).LifeCalculation = Battery_LifeCalculation_No;
					}

					if ( ElecStorage( StorNum ).LifeCalculation == Battery_LifeCalculation_Yes ) {
						ElecStorage( StorNum ).LifeCurveNum = GetCurveIndex( cAlphaArgs( 7 ) ); //Battery life calculation
						if ( ElecStorage( StorNum ).LifeCurveNum == 0 && ! lAlphaFieldBlanks( 7 ) ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
							ErrorsFound = true;
						} else if ( lAlphaFieldBlanks( 7 ) ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + " cannot be blank when " + cAlphaArgs( 6 ) + " = Yes. But no entry found." );
							ErrorsFound = true;
						} else if ( ! SameString( GetCurveType( ElecStorage( StorNum ).LifeCurveNum ), "DoubleExponentialDecay" ) ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + '=' + cAlphaArgs( 7 ) );
							ShowContinueError( "Curve Type must be DoubleExponentialDecay but was " + GetCurveType( ElecStorage( StorNum ).LifeCurveNum ) );
							ErrorsFound = true;
						}

						ElecStorage( StorNum ).CycleBinNum = rNumericArgs( 14 );

						if ( ! ErrorsFound ) { // life cycle calculation for this battery, allocate arrays for degradation calculation
							ElecStorage( StorNum ).B10.allocate( {1,MaxRainflowArrayBounds + 1} );
							ElecStorage( StorNum ).X0.allocate( {1,MaxRainflowArrayBounds + 1} );
							ElecStorage( StorNum ).Nmb0.allocate( {1,ElecStorage( StorNum ).CycleBinNum} );
							ElecStorage( StorNum ).OneNmb0.allocate( {1,ElecStorage( StorNum ).CycleBinNum} );
							ElecStorage( StorNum ).B10 = 0.0;
							ElecStorage( StorNum ).X0 = 0.0;
							ElecStorage( StorNum ).Nmb0 = 0.0;
							ElecStorage( StorNum ).OneNmb0 = 0.0;
						}
					}

					ElecStorage( StorNum ).StorageModelMode = KiBaMBattery;
					ElecStorage( StorNum ).ZoneRadFract = rNumericArgs( 1 );
					ElecStorage( StorNum ).ParallelNum = rNumericArgs( 2 );
					ElecStorage( StorNum ).SeriesNum = rNumericArgs( 3 );
					ElecStorage( StorNum ).MaxAhCapacity = rNumericArgs( 4 );
					ElecStorage( StorNum ).StartingSOC = rNumericArgs( 5 );
					ElecStorage( StorNum ).AvailableFrac = rNumericArgs( 6 );
					ElecStorage( StorNum ).ChargeConversionRate = rNumericArgs( 7 );
					ElecStorage( StorNum ).ChargedOCV = rNumericArgs( 8 );
					ElecStorage( StorNum ).DischargedOCV = rNumericArgs( 9 );
					ElecStorage( StorNum ).InternalR = rNumericArgs( 10 );
					ElecStorage( StorNum ).MaxDischargeI = rNumericArgs( 11 );
					ElecStorage( StorNum ).CutoffV = rNumericArgs( 12 );
					ElecStorage( StorNum ).MaxChargeRate = rNumericArgs( 13 );

					SetupOutputVariable( "Electric Storage Operating Mode Index []", ElecStorage( StorNum ).StorageMode, "System", "Average", ElecStorage( StorNum ).Name );
					SetupOutputVariable( "Electric Storage Charge State [Ah]", ElecStorage( StorNum ).AbsoluteSOC, "System", "Average", ElecStorage( StorNum ).Name );
					SetupOutputVariable( "Electric Storage Charge Fraction []", ElecStorage( StorNum ).FractionSOC, "System", "Average", ElecStorage( StorNum ).Name );
					SetupOutputVariable( "Electric Storage Total Current [A]", ElecStorage( StorNum ).BatteryCurrent, "System", "Average", ElecStorage( StorNum ).Name );
					SetupOutputVariable( "Electric Storage Total Voltage [V]", ElecStorage( StorNum ).BatteryVoltage, "System", "Average", ElecStorage( StorNum ).Name );

					if ( ElecStorage( StorNum ).LifeCalculation == Battery_LifeCalculation_Yes ) {
						SetupOutputVariable( "Electric Storage Degradation Fraction []", ElecStorage( StorNum ).BatteryDamage, "System", "Average", ElecStorage( StorNum ).Name );
					}

				}

			} //any kibam storage

			//For any battery
			for ( StorNum = 1; StorNum <= NumofSimpleElecStorage + NumofKiBaMElecStorage; ++StorNum ) {
				SetupOutputVariable( "Electric Storage Charge Power [W]", ElecStorage( StorNum ).StoredPower, "System", "Average", ElecStorage( StorNum ).Name );
				SetupOutputVariable( "Electric Storage Charge Energy [J]", ElecStorage( StorNum ).StoredEnergy, "System", "Sum", ElecStorage( StorNum ).Name );
				SetupOutputVariable( "Electric Storage Production Decrement Energy [J]", ElecStorage( StorNum ).DecrementedEnergyStored, "System", "Sum", ElecStorage( StorNum ).Name, _, "ElectricityProduced", "COGENERATION", _, "Plant" );
				SetupOutputVariable( "Electric Storage Discharge Power [W]", ElecStorage( StorNum ).DrawnPower, "System", "Average", ElecStorage( StorNum ).Name );
				SetupOutputVariable( "Electric Storage Discharge Energy [J]", ElecStorage( StorNum ).DrawnEnergy, "System", "Sum", ElecStorage( StorNum ).Name, _, "ElectricityProduced", "COGENERATION", _, "Plant" );
				SetupOutputVariable( "Electric Storage Thermal Loss Rate [W]", ElecStorage( StorNum ).ThermLossRate, "System", "Average", ElecStorage( StorNum ).Name );
				SetupOutputVariable( "Electric Storage Thermal Loss Energy [J]", ElecStorage( StorNum ).ThermLossEnergy, "System", "Sum", ElecStorage( StorNum ).Name );
				if ( AnyEnergyManagementSystemInModel ) {
					if ( ElecStorage( StorNum ).StorageModelMode == SimpleBucketStorage ) {
						SetupEMSInternalVariable( "Electrical Storage Maximum Capacity", ElecStorage( StorNum ).Name, "[J]", ElecStorage( StorNum ).MaxEnergyCapacity );
					} else if ( ElecStorage( StorNum ).StorageModelMode == KiBaMBattery ) {
						SetupEMSInternalVariable( "Electrical Storage Maximum Capacity", ElecStorage( StorNum ).Name, "[Ah]", ElecStorage( StorNum ).MaxAhCapacity );
					}

					SetupEMSActuator( "Electrical Storage", ElecStorage( StorNum ).Name, "Power Draw Rate", "[W]", ElecStorage( StorNum ).EMSOverridePelFromStorage, ElecStorage( StorNum ).EMSValuePelFromStorage );
					SetupEMSActuator( "Electrical Storage", ElecStorage( StorNum ).Name, "Power Charge Rate", "[W]", ElecStorage( StorNum ).EMSOverridePelIntoStorage, ElecStorage( StorNum ).EMSValuePelIntoStorage );
				}

				if ( ElecStorage( StorNum ).ZoneNum > 0 ) {
					{ auto const SELECT_CASE_var( ElecStorage( StorNum ).StorageModelMode );

					if ( SELECT_CASE_var == SimpleBucketStorage ) {
						SetupZoneInternalGain( ElecStorage( StorNum ).ZoneNum, "ElectricLoadCenter:Storage:Simple", ElecStorage( StorNum ).Name, IntGainTypeOf_ElectricLoadCenterStorageSimple, ElecStorage( StorNum ).QdotConvZone, _, ElecStorage( StorNum ).QdotRadZone );
					} else if ( SELECT_CASE_var == KiBaMBattery ) {
						SetupZoneInternalGain( ElecStorage( StorNum ).ZoneNum, "ElectricLoadCenter:Storage:Battery", ElecStorage( StorNum ).Name, IntGainTypeOf_ElectricLoadCenterStorageBattery, ElecStorage( StorNum ).QdotConvZone, _, ElecStorage( StorNum ).QdotRadZone );
					}}
				}

			}

		} //any storage at all

		//read in any electrical transformers that may be associated with load centers.
		NumTransformers = GetNumObjectsFound( "ElectricLoadCenter:Transformer" );
		if ( NumTransformers > 0 ) {
			Transformer.allocate( NumTransformers );
			TransformerNames.allocate( NumTransformers );

			cCurrentModuleObject = "ElectricLoadCenter:Transformer";
			for ( TransfNum = 1; TransfNum <= NumTransformers; ++TransfNum ) {
				GetObjectItem( cCurrentModuleObject, TransfNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), TransformerNames, TransfNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxx"; //Actually, this line is not necessary because name is a required field
				}
				TransformerNames( TransfNum ) = cAlphaArgs( 1 );
				Transformer( TransfNum ).Name = cAlphaArgs( 1 );

				if ( lAlphaFieldBlanks( 2 ) ) {
					Transformer( TransfNum ).AvailSchedPtr = ScheduleAlwaysOn;
				} else {
					Transformer( TransfNum ).AvailSchedPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
					if ( Transformer( TransfNum ).AvailSchedPtr == 0 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
						ErrorsFound = true;
					}
				}

				if ( SameString( cAlphaArgs( 3 ), "PowerInFromGrid" ) ) {
					Transformer( TransfNum ).UsageMode = PowerInFromGrid;
				} else if ( SameString( cAlphaArgs( 3 ), "PowerOutFromOnsiteGeneration" ) ) {
					Transformer( TransfNum ).UsageMode = PowerOutFromBldg;
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
					ErrorsFound = true;
				}

				Transformer( TransfNum ).ZoneNum = FindItemInList( cAlphaArgs( 4 ), Zone.Name(), NumOfZones );
				if ( Transformer( TransfNum ).ZoneNum > 0 ) Transformer( TransfNum ).HeatLossesDestination = ZoneGains;
				if ( Transformer( TransfNum ).ZoneNum == 0 ) {
					if ( lAlphaFieldBlanks( 4 ) ) {
						Transformer( TransfNum ).HeatLossesDestination = LostToOutside;
					} else {
						Transformer( TransfNum ).HeatLossesDestination = LostToOutside;
						ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 4 ) + " = " + cAlphaArgs( 4 ) );
						ShowContinueError( "Zone name not found. Transformer heat losses will not be added to a zone" );
						//continue with simulation but storage losses not sent to a zone.
					}
				}

				Transformer( TransfNum ).ZoneRadFrac = rNumericArgs( 1 );
				Transformer( TransfNum ).RatedCapacity = rNumericArgs( 2 );
				Transformer( TransfNum ).Phase = rNumericArgs( 3 );

				if ( SameString( cAlphaArgs( 5 ), "Copper" ) ) {
					Transformer( TransfNum ).FactorTempCoeff = 234.5;
				} else if ( SameString( cAlphaArgs( 5 ), "Aluminum" ) ) {
					Transformer( TransfNum ).FactorTempCoeff = 225.0;
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 5 ) + " = " + cAlphaArgs( 5 ) );
					ErrorsFound = true;
				}

				Transformer( TransfNum ).TempRise = rNumericArgs( 4 );
				Transformer( TransfNum ).EddyFrac = rNumericArgs( 5 );

				if ( SameString( cAlphaArgs( 6 ), "RatedLosses" ) ) {
					Transformer( TransfNum ).PerformanceInputMode = LossesMethod;
				} else if ( SameString( cAlphaArgs( 6 ), "NominalEfficiency" ) ) {
					Transformer( TransfNum ).PerformanceInputMode = EfficiencyMethod;
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 6 ) + " = " + cAlphaArgs( 6 ) );
					ErrorsFound = true;
				}

				Transformer( TransfNum ).RatedNL = rNumericArgs( 6 );
				Transformer( TransfNum ).RatedLL = rNumericArgs( 7 );
				Transformer( TransfNum ).RatedEfficiency = rNumericArgs( 8 );
				Transformer( TransfNum ).RatedPUL = rNumericArgs( 9 );
				Transformer( TransfNum ).RatedTemp = rNumericArgs( 10 );
				Transformer( TransfNum ).MaxPUL = rNumericArgs( 11 );

				//Check the input for MaxPUL if the performance input method is EfficiencyMethod
				//Other inputs do not need to be checked because they are handled by the IDD procedure.
				if ( Transformer( TransfNum ).PerformanceInputMode == EfficiencyMethod ) {
					if ( lNumericFieldBlanks( 11 ) ) {
						Transformer( TransfNum ).MaxPUL = Transformer( TransfNum ).RatedPUL;
					} else if ( Transformer( TransfNum ).MaxPUL <= 0 || Transformer( TransfNum ).MaxPUL > 1 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cNumericFieldNames( 11 ) + "=[" + RoundSigDigits( rNumericArgs( 11 ), 3 ) + "]." );
						ShowContinueError( "Entered value must be > 0 and <= 1." );
						ErrorsFound = true;
					}
				}

				if ( SameString( cAlphaArgs( 7 ), "Yes" ) ) {
					Transformer( TransfNum ).ConsiderLosses = true;
				} else if ( SameString( cAlphaArgs( 7 ), "No" ) ) {
					Transformer( TransfNum ).ConsiderLosses = false;
				} else {
					if ( Transformer( TransfNum ).UsageMode == PowerInFromGrid ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + " = " + cAlphaArgs( 7 ) );
						ErrorsFound = true;
					}
				}

				NumWiredMeters = NumAlphas - NumAlphaBeforeMeter;

				if ( Transformer( TransfNum ).UsageMode == PowerInFromGrid ) {

					//Provide warning if no meter is wired to a transformer used to get power from the grid
					if ( NumWiredMeters <= 0 ) {
						ShowWarningError( RoutineName + "ElectricLoadCenter:Transformer=\"" + Transformer( TransfNum ).Name + "\":" );
						ShowContinueError( "ISOLATED Transformer: No meter wired to a transformer used to input power from grid" );
					}

					Transformer( TransfNum ).WiredMeterNames.allocate( NumWiredMeters );
					Transformer( TransfNum ).WiredMeterPtrs.allocate( NumWiredMeters );
					Transformer( TransfNum ).SpecialMeter.allocate( NumWiredMeters );

					//Meter check deferred because they may have not been "loaded" yet,
					for ( LoopCount = 1; LoopCount <= NumWiredMeters; ++LoopCount ) {
						Transformer( TransfNum ).WiredMeterNames( LoopCount ) = MakeUPPERCase( cAlphaArgs( LoopCount + NumAlphaBeforeMeter ) );
						//Assign SpecialMeter as TRUE if the meter name is Electricity:Facility or Electricity:HVAC
						if ( SameString( Transformer( TransfNum ).WiredMeterNames( LoopCount ), "Electricity:Facility" ) || SameString( Transformer( TransfNum ).WiredMeterNames( LoopCount ), "Electricity:HVAC" ) ) {
							Transformer( TransfNum ).SpecialMeter( LoopCount ) = true;
						} else {
							Transformer( TransfNum ).SpecialMeter( LoopCount ) = false;
						}
					}
				}

				SetupOutputVariable( "Transformer Efficiency []", Transformer( TransfNum ).Efficiency, "System", "Average", Transformer( TransfNum ).Name );
				SetupOutputVariable( "Transformer Input Electric Power [W]", Transformer( TransfNum ).PowerIn, "System", "Average", Transformer( TransfNum ).Name );
				SetupOutputVariable( "Transformer Input Electric Energy [J]", Transformer( TransfNum ).EnergyIn, "System", "Sum", Transformer( TransfNum ).Name );
				SetupOutputVariable( "Transformer Output Electric Power [W]", Transformer( TransfNum ).PowerOut, "System", "Average", Transformer( TransfNum ).Name );
				SetupOutputVariable( "Transformer Output Electric Energy [J]", Transformer( TransfNum ).EnergyOut, "System", "Sum", Transformer( TransfNum ).Name );
				SetupOutputVariable( "Transformer No Load Loss Rate [W]", Transformer( TransfNum ).NoLoadLossRate, "System", "Average", Transformer( TransfNum ).Name );
				SetupOutputVariable( "Transformer No Load Loss Energy [J]", Transformer( TransfNum ).NoLoadLossEnergy, "System", "Sum", Transformer( TransfNum ).Name );
				SetupOutputVariable( "Transformer Load Loss Rate [W]", Transformer( TransfNum ).LoadLossRate, "System", "Average", Transformer( TransfNum ).Name );
				SetupOutputVariable( "Transformer Load Loss Energy [J]", Transformer( TransfNum ).LoadLossEnergy, "System", "Sum", Transformer( TransfNum ).Name );
				SetupOutputVariable( "Transformer Thermal Loss Rate [W]", Transformer( TransfNum ).ThermalLossRate, "System", "Average", Transformer( TransfNum ).Name );
				SetupOutputVariable( "Transformer Thermal Loss Energy [J]", Transformer( TransfNum ).ThermalLossEnergy, "System", "Sum", Transformer( TransfNum ).Name );
				SetupOutputVariable( "Transformer Distribution Electric Loss Energy [J]", Transformer( TransfNum ).ElecUseUtility, "System", "Sum", Transformer( TransfNum ).Name, _, "Electricity", "ExteriorEquipment", "Transformer", "System" );
				SetupOutputVariable( "Transformer Cogeneration Electric Loss Energy [J]", Transformer( TransfNum ).ElecProducedCoGen, "System", "Sum", Transformer( TransfNum ).Name, _, "ElectricityProduced", "COGENERATION", _, "System" );

				if ( Transformer( TransfNum ).ZoneNum > 0 ) {
					SetupZoneInternalGain( Transformer( TransfNum ).ZoneNum, "ElectricLoadCenter:Transformer", Transformer( TransfNum ).Name, IntGainTypeOf_ElectricLoadCenterTransformer, Transformer( TransfNum ).QdotConvZone, _, Transformer( TransfNum ).QdotRadZone );
				}

			} // End loop for get transformer inputs
		}

		//Get the number of electric load centers  (now allowing more than 1 per simulation)
		NumLoadCenters = GetNumObjectsFound( "ElectricLoadCenter:Distribution" );

		if ( NumLoadCenters > 0 ) {
			if ( ! allocated( ElecLoadCenter ) ) ElecLoadCenter.allocate( NumLoadCenters );
			for ( LoopCount = 1; LoopCount <= NumTransformers; ++LoopCount ) {
				if ( ! allocated( Transformer( LoopCount ).LoadCenterIndexes ) ) {
					Transformer( LoopCount ).LoadCenterIndexes.allocate( NumLoadCenters );
				}
			}
		} else {
			// set up one load center anyway for consistent access to report variables.
			if ( ! allocated( ElecLoadCenter ) ) ElecLoadCenter.allocate( 1 );
		}

		//First get the number of electric load center generator and make a list of names
		cCurrentModuleObject = "ElectricLoadCenter:Generators";
		NumGenLists = GetNumObjectsFound( cCurrentModuleObject );
		ListName.allocate( NumGenLists );
		for ( Count = 1; Count <= NumGenLists; ++Count ) {
			GetObjectItem( cCurrentModuleObject, Count, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ListName, Count - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
			}
			ListName( Count ) = cAlphaArgs( 1 );
		}

		for ( Count = 1; Count <= NumLoadCenters; ++Count ) {

			//Get the data for electric load centers
			cCurrentModuleObject = "ElectricLoadCenter:Distribution";
			GetObjectItem( cCurrentModuleObject, Count, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ElecLoadCenter.Name(), Count - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			//Load the Power Center Name.
			ElecLoadCenter( Count ).Name = cAlphaArgs( 1 );

			//Load the Power Center Generator List .
			ElecLoadCenter( Count ).GeneratorList = cAlphaArgs( 2 );

			//Load the Power Center Operation Scheme
			if ( SameString( cAlphaArgs( 3 ), "Baseload" ) ) {
				ElecLoadCenter( Count ).OperationScheme = iOpSchemeBaseLoad;
			} else if ( SameString( cAlphaArgs( 3 ), "DemandLimit" ) ) {
				ElecLoadCenter( Count ).OperationScheme = iOpSchemeDemandLimit;
			} else if ( SameString( cAlphaArgs( 3 ), "TrackElectrical" ) ) {
				ElecLoadCenter( Count ).OperationScheme = iOpSchemeTrackElectrical;
			} else if ( SameString( cAlphaArgs( 3 ), "TrackSchedule" ) ) {
				ElecLoadCenter( Count ).OperationScheme = iOpSchemeTrackSchedule;
			} else if ( SameString( cAlphaArgs( 3 ), "TrackMeter" ) ) {
				ElecLoadCenter( Count ).OperationScheme = iOpSchemeTrackMeter;
			} else if ( SameString( cAlphaArgs( 3 ), "FollowThermal" ) ) {
				ElecLoadCenter( Count ).OperationScheme = iOpSchemeThermalFollow;
			} else if ( SameString( cAlphaArgs( 3 ), "FollowThermalLimitElectrical" ) ) {
				ElecLoadCenter( Count ).OperationScheme = iOpSchemeThermalFollowLimitElectrical;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
				ErrorsFound = true;
			}

			//Load the Purchaed Electric Demand Limit for the Demand Limit scheme only. Is not used for other schemes.
			ElecLoadCenter( Count ).DemandLimit = rNumericArgs( 1 );

			ElecLoadCenter( Count ).TrackSchedPtr = GetScheduleIndex( cAlphaArgs( 4 ) );
			// test if schedule valid and 'TRACK SCHEDULE'
			if ( ( ElecLoadCenter( Count ).TrackSchedPtr == 0 ) && ( ElecLoadCenter( Count ).OperationScheme == iOpSchemeTrackSchedule ) ) { // throw error
				if ( ! lAlphaFieldBlanks( 4 ) ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 4 ) + " = " + cAlphaArgs( 4 ) );
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 4 ) + " = blank field." );
				}
				ShowContinueError( "Schedule not found; Must be entered and valid when Operation Scheme=TrackSchedule" );
				ErrorsFound = true;
			}

			ElecLoadCenter( Count ).DemandMeterName = MakeUPPERCase( cAlphaArgs( 5 ) );

			// meters may not be "loaded" yet, defered check to later subroutine
			if ( SameString( cAlphaArgs( 6 ), "AlternatingCurrent" ) ) {
				ElecLoadCenter( Count ).BussType = ACBuss;
				cAlphaArgs( 6 ) = "AlternatingCurrent";
			} else if ( SameString( cAlphaArgs( 6 ), "DirectCurrentWithInverter" ) ) {
				ElecLoadCenter( Count ).BussType = DCBussInverter;
				ElecLoadCenter( Count ).InverterPresent = true;
				cAlphaArgs( 6 ) = "DirectCurrentWithInverter";
			} else if ( SameString( cAlphaArgs( 6 ), "AlternatingCurrentWithStorage" ) ) {
				ElecLoadCenter( Count ).BussType = ACBussStorage;
				ElecLoadCenter( Count ).StoragePresent = true;
				cAlphaArgs( 6 ) = "AlternatingCurrentWithStorage";
			} else if ( SameString( cAlphaArgs( 6 ), "DirectCurrentWithInverterDCStorage" ) ) {
				ElecLoadCenter( Count ).BussType = DCBussInverterDCStorage;
				ElecLoadCenter( Count ).InverterPresent = true;
				ElecLoadCenter( Count ).StoragePresent = true;
				cAlphaArgs( 6 ) = "DirectCurrentWithInverterDCStorage";
			} else if ( SameString( cAlphaArgs( 6 ), "DirectCurrentWithInverterACStorage" ) ) {
				ElecLoadCenter( Count ).BussType = DCBussInverterACStorage;
				ElecLoadCenter( Count ).InverterPresent = true;
				ElecLoadCenter( Count ).StoragePresent = true;
				cAlphaArgs( 6 ) = "DirectCurrentWithInverterACStorage";
			} else if ( cAlphaArgs( 6 ).empty() ) {
				ElecLoadCenter( Count ).BussType = ACBuss;
				cAlphaArgs( 6 ) = "AlternatingCurrent (field was blank)";
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Invalid " + cAlphaFieldNames( 6 ) + " = " + cAlphaArgs( 6 ) );
				ErrorsFound = true;
			}

			if ( ElecLoadCenter( Count ).InverterPresent ) {
				ElecLoadCenter( Count ).InverterModelNum = FindItemInList( cAlphaArgs( 7 ), InverterNames, NumInverters );
				if ( ElecLoadCenter( Count ).InverterModelNum <= 0 ) {
					if ( ! lAlphaFieldBlanks( 7 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + " = " + cAlphaArgs( 7 ) );
					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + " = blank field." );
					}
					ShowContinueError( "Inverter object was not found; Must have and be valid when Buss Type=\"" + cAlphaArgs( 6 ) + "\"." );
					ErrorsFound = true;
				} else {
					// check if previous elec load center already uses this inverter.
					if ( Count - 1 > 0 ) {
						Found = FindItemInList( cAlphaArgs( 7 ), ElecLoadCenter.InverterName(), Count - 1 );
						if ( Found != 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + " = " + cAlphaArgs( 7 ) );
							ShowContinueError( "Inverter object has already been used by another " + cCurrentModuleObject );
							ErrorsFound = true;
						}
					}
				}
				ElecLoadCenter( Count ).InverterName = cAlphaArgs( 7 );
			}

			if ( ElecLoadCenter( Count ).StoragePresent ) {
				ElecLoadCenter( Count ).StorageModelNum = FindItemInList( cAlphaArgs( 8 ), StorageNames, NumElecStorageDevices );
				if ( ElecLoadCenter( Count ).StorageModelNum <= 0 ) {
					if ( ! lAlphaFieldBlanks( 8 ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 8 ) + " = " + cAlphaArgs( 8 ) );
					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 8 ) + " = blank field." );
					}
					ShowContinueError( "Electrical storage object was not found; Must have and be valid when Buss Type=\"" + cAlphaArgs( 6 ) + "\"." );
					ErrorsFound = true;
				} else {
					// check if previous elec load center already uses this storage.
					if ( Count - 1 > 0 ) {
						Found = FindItemInList( cAlphaArgs( 8 ), ElecLoadCenter.StorageName(), Count - 1 );
						if ( Found != 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( 8 ) + " = " + cAlphaArgs( 8 ) );
							ShowContinueError( "Storage object has already been used by another " + cCurrentModuleObject );
							ErrorsFound = true;
						}
					}
				}
				ElecLoadCenter( Count ).StorageName = cAlphaArgs( 8 );
			}

			//    If a transformer is used in an electric load center, the program needs to 1) update the number of
			//    electric load centers connected to that transformer; 2) bookkeep the load center index in the transformer
			//    data structure so that the transformer knows which load center is connected.
			if ( NumAlphas >= 9 && ( ! lAlphaFieldBlanks( 9 ) ) ) {
				ElecLoadCenter( Count ).TransformerModelNum = FindItemInList( cAlphaArgs( 9 ), TransformerNames, NumTransformers );
				if ( ElecLoadCenter( Count ).TransformerModelNum <= 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 9 ) + " = " + cAlphaArgs( 9 ) );
					ErrorsFound = true;
				} else {
					// It is allowed that a transformer can serve multiple load centers.
					// This differs from inverters and batteries (electrical storage) implemented previously
					ElecLoadCenter( Count ).TransformerName = cAlphaArgs( 9 );
					LCofTransformer = Transformer( ElecLoadCenter( Count ).TransformerModelNum ).LoadCenterNum + 1;
					Transformer( ElecLoadCenter( Count ).TransformerModelNum ).LoadCenterNum = LCofTransformer;
					Transformer( ElecLoadCenter( Count ).TransformerModelNum ).LoadCenterIndexes( LCofTransformer ) = Count;
				}
			}

			//Setup general output variables for reporting in the electric load center
			SetupWholeBldgReports = true;

			SetupOutputVariable( "Electric Load Center Produced Electric Power [W]", ElecLoadCenter( Count ).ElectProdRate, "System", "Average", ElecLoadCenter( Count ).Name );

			SetupOutputVariable( "Electric Load Center Produced Electric Energy [J]", ElecLoadCenter( Count ).ElectricityProd, "System", "Sum", ElecLoadCenter( Count ).Name );

			SetupOutputVariable( "Electric Load Center Produced Thermal Rate [W]", ElecLoadCenter( Count ).ThermalProdRate, "System", "Average", ElecLoadCenter( Count ).Name );

			SetupOutputVariable( "Electric Load Center Produced Thermal Energy [J]", ElecLoadCenter( Count ).ThermalProd, "System", "Sum", ElecLoadCenter( Count ).Name );

			if ( ElecLoadCenter( Count ).GeneratorList != "" ) {
				ListNum = FindItemInList( ElecLoadCenter( Count ).GeneratorList, ListName, NumGenLists );
				if ( ListNum == 0 ) {

					ShowSevereError( "Requested Generator List=" + ElecLoadCenter( Count ).GeneratorList + ", not found.  Load Center=" + ElecLoadCenter( Count ).Name );
					ErrorsFound = true;
					continue;
				}

				cCurrentModuleObject = "ElectricLoadCenter:Generators";
				GetObjectItem( cCurrentModuleObject, ListNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				//Calculate the number of generators in list
				NumGenerators = NumNums / 2; // note IDD needs Min Fields = 6  can this be more robust?
				if ( mod( ( NumAlphas - 1 + NumNums ), 5 ) != 0 ) ++NumGenerators;
				AlphaCount = 2;
				//Allocate the pointer array
				ElecLoadCenter( Count ).ElecGen.allocate( NumGenerators );
				ElecLoadCenter( Count ).NumGenerators = NumGenerators;
				pvTotalCapacity = 0.0; // for LEED report
				windTotalCapacity = 0.0; // for LEED report
				for ( GenCount = 1; GenCount <= ElecLoadCenter( Count ).NumGenerators; ++GenCount ) {
					//Load the Power Center Generator List Name
					ElecLoadCenter( Count ).ElecGen( GenCount ).Name = cAlphaArgs( AlphaCount );
					++AlphaCount;
					//Load the Type of Generator
					ElecLoadCenter( Count ).ElecGen( GenCount ).TypeOf = cAlphaArgs( AlphaCount );
					if ( SameString( cAlphaArgs( AlphaCount ), "Generator:InternalCombustionEngine" ) ) {
						ElecLoadCenter( Count ).ElecGen( GenCount ).CompType_Num = iGeneratorICEngine;
					} else if ( SameString( cAlphaArgs( AlphaCount ), "Generator:CombustionTurbine" ) ) {
						ElecLoadCenter( Count ).ElecGen( GenCount ).CompType_Num = iGeneratorCombTurbine;
					} else if ( SameString( cAlphaArgs( AlphaCount ), "Generator:MicroTurbine" ) ) {
						ElecLoadCenter( Count ).ElecGen( GenCount ).CompType_Num = iGeneratorMicroturbine;
					} else if ( SameString( cAlphaArgs( AlphaCount ), "Generator:Photovoltaic" ) ) {
						ElecLoadCenter( Count ).ElecGen( GenCount ).CompType_Num = iGeneratorPV;
					} else if ( SameString( cAlphaArgs( AlphaCount ), "Generator:FuelCell" ) ) {
						ElecLoadCenter( Count ).ElecGen( GenCount ).CompType_Num = iGeneratorFuelCell;
					} else if ( SameString( cAlphaArgs( AlphaCount ), "Generator:MicroCHP" ) ) {
						ElecLoadCenter( Count ).ElecGen( GenCount ).CompType_Num = iGeneratorMicroCHP;
					} else if ( SameString( cAlphaArgs( AlphaCount ), "Generator:WindTurbine" ) ) {
						ElecLoadCenter( Count ).ElecGen( GenCount ).CompType_Num = iGeneratorWindTurbine;
					} else {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( AlphaCount ) + " = " + cAlphaArgs( AlphaCount ) );
						ErrorsFound = true;
					}
					ValidateComponent( ElecLoadCenter( Count ).ElecGen( GenCount ).TypeOf, ElecLoadCenter( Count ).ElecGen( GenCount ).Name, IsNotOK, "Generator" );
					if ( IsNotOK ) {
						ShowContinueError( "In " + cCurrentModuleObject + '=' + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}

					++AlphaCount;

					ElecLoadCenter( Count ).ElecGen( GenCount ).MaxPowerOut = rNumericArgs( 2 * GenCount - 1 );
					ElecLoadCenter( Count ).ElecGen( GenCount ).NominalThermElectRatio = rNumericArgs( 2 * GenCount );

					if ( AnyEnergyManagementSystemInModel ) {
						SetupEMSInternalVariable( "Generator Nominal Maximum Power", ElecLoadCenter( Count ).ElecGen( GenCount ).Name, "[W]", ElecLoadCenter( Count ).ElecGen( GenCount ).MaxPowerOut );
						SetupEMSInternalVariable( "Generator Nominal Thermal To Electric Ratio", ElecLoadCenter( Count ).ElecGen( GenCount ).Name, "[ratio]", ElecLoadCenter( Count ).ElecGen( GenCount ).NominalThermElectRatio );
					}

					//Load the Power CenterElectric Generation Meter Name
					ElecLoadCenter( Count ).ElecGen( GenCount ).AvailSched = cAlphaArgs( AlphaCount );
					if ( lAlphaFieldBlanks( AlphaCount ) ) {
						ElecLoadCenter( Count ).ElecGen( GenCount ).AvailSchedPtr = ScheduleAlwaysOn;
					} else {
						ElecLoadCenter( Count ).ElecGen( GenCount ).AvailSchedPtr = GetScheduleIndex( cAlphaArgs( AlphaCount ) );
						if ( ElecLoadCenter( Count ).ElecGen( GenCount ).AvailSchedPtr <= 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
							ShowContinueError( "Invalid " + cAlphaFieldNames( AlphaCount ) + " = " + cAlphaArgs( AlphaCount ) );
							ShowContinueError( "Schedule was not found " );
							ErrorsFound = true;
						}
					}
					++AlphaCount;

					SetupOutputVariable( "Generator Requested Electric Power [W]", ElecLoadCenter( Count ).ElecGen( GenCount ).PowerRequestThisTimestep, "System", "Average", ElecLoadCenter( Count ).ElecGen( GenCount ).Name );
					if ( AnyEnergyManagementSystemInModel ) {
						SetupEMSActuator( "On-Site Generator Control", ElecLoadCenter( Count ).ElecGen( GenCount ).Name, "Requested Power", "[W]", ElecLoadCenter( Count ).ElecGen( GenCount ).EMSRequestOn, ElecLoadCenter( Count ).ElecGen( GenCount ).EMSPowerRequest );

					}

				} //End of the NumGenerators Loop

			}

			SetupOutputVariable( "Electric Load Center Requested Electric Power [W]", ElecLoadCenter( Count ).TotalPowerRequest, "System", "Average", ElecLoadCenter( Count ).Name );

		} // loop over number of load centers

		// LEED report
		pvTotalCapacity = 0.0;
		windTotalCapacity = 0.0;
		for ( Count = 1; Count <= NumLoadCenters; ++Count ) {
			if ( ElecLoadCenter( Count ).GeneratorList != "" ) {
				for ( GenCount = 1; GenCount <= ElecLoadCenter( Count ).NumGenerators; ++GenCount ) {
					if ( ElecLoadCenter( Count ).ElecGen( GenCount ).CompType_Num == iGeneratorPV ) {
						pvTotalCapacity += ElecLoadCenter( Count ).ElecGen( GenCount ).MaxPowerOut;
					}
					if ( ElecLoadCenter( Count ).ElecGen( GenCount ).CompType_Num == iGeneratorWindTurbine ) {
						windTotalCapacity += ElecLoadCenter( Count ).ElecGen( GenCount ).MaxPowerOut;
					}
				}
			}
		}
		//put in total capacity for PV and Wind for LEED report
		PreDefTableEntry( pdchLeedRenRatCap, "Photovoltaic", pvTotalCapacity / 1000, 2 );
		PreDefTableEntry( pdchLeedRenRatCap, "Wind", windTotalCapacity / 1000, 2 );

		if ( NumLoadCenters == 0 ) {
			// if user input did not include an Electric Load center, create a simple default one here for reporting purposes
			//   but only if there are any other electricity components set up (yet) for metering
			AnyElectricityPresent = GetMeterIndex( "ELECTRICITY:FACILITY" );

			if ( AnyElectricityPresent > 0 ) {

				NumLoadCenters = 1;
				ElecLoadCenter( 1 ).Name = "Electrical Service";
				ElecLoadCenter( 1 ).OperationScheme = iOpSchemeTrackElectrical;

				SetupWholeBldgReports = true;

			} // any electricity metered
		} // no user electric load center

		if ( SetupWholeBldgReports ) {

			SetupOutputVariable( "Facility Total Purchased Electric Power [W]", WholeBldgElectSummary.ElectPurchRate, "System", "Average", WholeBldgElectSummary.Name );
			SetupOutputVariable( "Facility Total Purchased Electric Energy [J]", WholeBldgElectSummary.ElectricityPurch, "System", "Sum", WholeBldgElectSummary.Name, _, "ElectricityPurchased", "COGENERATION", _, "Plant" );

			SetupOutputVariable( "Facility Total Surplus Electric Energy [J]", WholeBldgElectSummary.ElectricitySurplus, "System", "Sum", WholeBldgElectSummary.Name, _, "ElectricitySurplusSold", "COGENERATION", _, "Plant" );

			SetupOutputVariable( "Facility Net Purchased Electric Power [W]", WholeBldgElectSummary.ElectricityNetRate, "System", "Average", WholeBldgElectSummary.Name );
			SetupOutputVariable( "Facility Net Purchased Electric Energy [J]", WholeBldgElectSummary.ElectricityNet, "System", "Sum", WholeBldgElectSummary.Name, _, "ElectricityNet", "COGENERATION", _, "Plant" );

			SetupOutputVariable( "Facility Total Building Electric Demand Power [W]", WholeBldgElectSummary.TotalBldgElecDemand, "System", "Average", WholeBldgElectSummary.Name );
			SetupOutputVariable( "Facility Total HVAC Electric Demand Power [W]", WholeBldgElectSummary.TotalHVACElecDemand, "System", "Average", WholeBldgElectSummary.Name );
			SetupOutputVariable( "Facility Total Electric Demand Power [W]", WholeBldgElectSummary.TotalElectricDemand, "System", "Average", WholeBldgElectSummary.Name );

			SetupOutputVariable( "Facility Total Produced Electric Power [W]", WholeBldgElectSummary.ElectProdRate, "System", "Average", WholeBldgElectSummary.Name );
			SetupOutputVariable( "Facility Total Produced Electric Energy [J]", WholeBldgElectSummary.ElectricityProd, "System", "Sum", WholeBldgElectSummary.Name );
		}

		//Check whether a transformer connects to a load center if it is used to output power to the grid
		//Issue warning if no load center is connected to that transformer
		//This has to be done after reading in all load centers
		for ( TransfNum = 1; TransfNum <= NumTransformers; ++TransfNum ) {
			if ( Transformer( TransfNum ).UsageMode == PowerOutFromBldg && Transformer( TransfNum ).LoadCenterNum == 0 ) {
				ShowSevereError( RoutineName + "ElectricLoadCenter:Transformer=\"" + Transformer( TransfNum ).Name + "\", invalid entry." );
				ShowContinueError( "ISOLATED Transformer: No load center connects to a transformer used to output power" );
			}
		}

		ListName.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Preceding errors terminate program." );
		}

	}

	void
	GeneratorPowerOutput(
		int const LoadCenterNum, // Load Center number counter
		int const GenNum, // Generator number counter
		bool const FirstHVACIteration, // Unused 2010 JANUARY
		Real64 & ElectricPowerOutput, // Actual generator electric power output
		Real64 & ThermalPowerOutput // Actual generator thermal power output
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B. Nigusse
		//       DATE WRITTEN   December 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Simulates generator to get the actual electric power output based on load request

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using ICEngineElectricGenerator::SimICEngineGenerator;
		using ICEngineElectricGenerator::GetICEGeneratorResults;
		using CTElectricGenerator::SimCTGenerator;
		using CTElectricGenerator::GetCTGeneratorResults;
		using MicroturbineElectricGenerator::SimMTGenerator;
		using MicroturbineElectricGenerator::GetMTGeneratorResults;
		using Photovoltaics::SimPVGenerator;
		using Photovoltaics::GetPVGeneratorResults;
		using FuelCellElectricGenerator::SimFuelCellGenerator;
		using FuelCellElectricGenerator::GetFuelCellGeneratorResults;
		using MicroCHPElectricGenerator::SimMicroCHPGenerator;
		using MicroCHPElectricGenerator::GetMicroCHPGeneratorResults;
		using WindTurbine::SimWindTurbine;
		using WindTurbine::GetWTGeneratorResults;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		std::string GeneratorName; // User-specified name of generator
		int GeneratorType; // Type of generator
		bool RunFlag; // Simulate generator when TRUE
		Real64 MyLoad; // Generator load request (W)

		GeneratorType = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).CompType_Num;
		GeneratorName = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).Name;
		RunFlag = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ONThisTimestep;
		MyLoad = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).PowerRequestThisTimestep;

		// Select and call models and also collect results for load center power conditioning and reporting
		{ auto const SELECT_CASE_var( GeneratorType );

		if ( SELECT_CASE_var == iGeneratorICEngine ) { // 'Generator:InternalCombustionEngine'
			SimICEngineGenerator( GeneratorType, GeneratorName, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).GeneratorIndex, RunFlag, MyLoad, FirstHVACIteration );
			GetICEGeneratorResults( GeneratorType, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).GeneratorIndex, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectProdRate, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectricityProd, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProdRate, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProd );
			ElectricPowerOutput = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectProdRate;
			ThermalPowerOutput = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProdRate;

		} else if ( SELECT_CASE_var == iGeneratorCombTurbine ) { // 'Generator:CombustionTurbine'
			SimCTGenerator( GeneratorType, GeneratorName, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).GeneratorIndex, RunFlag, MyLoad, FirstHVACIteration );
			GetCTGeneratorResults( GeneratorType, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).GeneratorIndex, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectProdRate, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectricityProd, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProdRate, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProd );
			ElectricPowerOutput = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectProdRate;
			ThermalPowerOutput = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProdRate;

		} else if ( SELECT_CASE_var == iGeneratorMicroturbine ) { // 'Generator:MicroTurbine'
			SimMTGenerator( GeneratorType, GeneratorName, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).GeneratorIndex, RunFlag, MyLoad, FirstHVACIteration );
			GetMTGeneratorResults( GeneratorType, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).GeneratorIndex, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectProdRate, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectricityProd, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProdRate, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProd );
			ElectricPowerOutput = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectProdRate;
			ThermalPowerOutput = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProdRate;

		} else if ( SELECT_CASE_var == iGeneratorPV ) { // 'Generator:Photovoltaic'
			SimPVGenerator( GeneratorType, GeneratorName, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).GeneratorIndex, RunFlag, MyLoad );
			GetPVGeneratorResults( GeneratorType, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).GeneratorIndex, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).DCElectProdRate, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).DCElectricityProd, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProdRate, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProd );
			ElectricPowerOutput = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).DCElectProdRate;
			ThermalPowerOutput = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProdRate;

		} else if ( SELECT_CASE_var == iGeneratorFuelCell ) { // 'Generator:FuelCell'
			SimFuelCellGenerator( GeneratorType, GeneratorName, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).GeneratorIndex, RunFlag, MyLoad, FirstHVACIteration );
			GetFuelCellGeneratorResults( GeneratorType, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).GeneratorIndex, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectProdRate, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectricityProd, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProdRate, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProd );
			ElectricPowerOutput = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectProdRate;
			ThermalPowerOutput = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProdRate;

		} else if ( SELECT_CASE_var == iGeneratorMicroCHP ) { // 'Generator:MicroCHP'
			SimMicroCHPGenerator( GeneratorType, GeneratorName, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).GeneratorIndex, RunFlag, false, MyLoad, constant_zero, FirstHVACIteration );
			GetMicroCHPGeneratorResults( GeneratorType, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).GeneratorIndex, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectProdRate, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectricityProd, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProdRate, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProd );
			ElectricPowerOutput = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectProdRate;
			ThermalPowerOutput = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProdRate;

		} else if ( SELECT_CASE_var == iGeneratorWindTurbine ) { // 'Generator:WindTurbine'
			SimWindTurbine( GeneratorType, GeneratorName, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).GeneratorIndex, RunFlag, MyLoad );
			GetWTGeneratorResults( GeneratorType, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).GeneratorIndex, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectProdRate, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectricityProd, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProdRate, ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProd );
			ElectricPowerOutput = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ElectProdRate;
			ThermalPowerOutput = ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).ThermalProdRate;

		} else {
			ShowSevereError( "ManageElectricPower: Invalid Generator Type found= " + ElecLoadCenter( LoadCenterNum ).ElecGen( GenNum ).TypeOf );
			ShowContinueError( ".. Generator Name=" + GeneratorName );
			ShowFatalError( ".. preceding error causes termination." );

		}} // TypeOfEquip

	}

	void
	CalcLoadCenterThermalLoad(
		bool const EP_UNUSED( FirstHVACIteration ), // unused1208
		int const LoadCenterNum, // Load Center number counter
		Real64 & ThermalLoad // heat rate called for from cogenerator(watts)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Dec 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataPlant;
		using DataHVACGlobals::NumPlantLoops;
		using InputProcessor::SameString;

		using DataGlobals::MetersHaveBeenInitialized;

		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeSetupFlag( true );
		static Array1D_bool MyCoGenSetupFlag;
		int FoundCount;
		int i;
		int j;
		int k;
		int m;
		int LoopID;
		int SideID;
		int BranchID;
		int CompID;
		std::string thisName;
		//unused  INTEGER :: ThisTypeNum

		//debugstuff
		//unused    REAL(r64) :: ActualTime
		//unused    CHARACTER*300 :: OutputString
		//unused    CHARACTER*5 :: strFirstHVACIteration

		// need to do initial setups
		if ( MyOneTimeSetupFlag ) {
			MyCoGenSetupFlag.dimension( NumLoadCenters, true );
			ThermalLoad = 0.0;
			MyOneTimeSetupFlag = false;
			return;
		}

		FoundCount = 0;
		if ( MyCoGenSetupFlag( LoadCenterNum ) ) {
			if ( allocated( PlantLoop ) ) {
				// loop across generators and find match
				for ( i = 1; i <= ElecLoadCenter( LoadCenterNum ).NumGenerators; ++i ) {
					thisName = ElecLoadCenter( LoadCenterNum ).ElecGen( i ).Name;

					for ( j = 1; j <= NumPlantLoops; ++j ) {

						for ( k = 1; k <= PlantLoop( j ).LoopSide( SupplySide ).TotalBranches; ++k ) {

							for ( m = 1; m <= PlantLoop( j ).LoopSide( SupplySide ).Branch( k ).TotalComponents; ++m ) {
								if ( SameString( PlantLoop( j ).LoopSide( SupplySide ).Branch( k ).Comp( m ).Name, thisName ) ) {
									ElecLoadCenter( LoadCenterNum ).ElecGen( i ).PlantLoopNum = j;
									ElecLoadCenter( LoadCenterNum ).ElecGen( i ).LoopSideNum = SupplySide;
									ElecLoadCenter( LoadCenterNum ).ElecGen( i ).BranchNum = k;
									ElecLoadCenter( LoadCenterNum ).ElecGen( i ).CompNum = m;

									MyCoGenSetupFlag( LoadCenterNum ) = false;
									++FoundCount;
									ElecLoadCenter( LoadCenterNum ).ElecGen( i ).PlantInfoFound = true;
								}

							} // components

						} // branches

					} // plant loops

				} // generators in load center
			}

		}

		// sum up "MyLoad" for all generators on this load center from plant structure
		ThermalLoad = 0.0;
		for ( i = 1; i <= ElecLoadCenter( LoadCenterNum ).NumGenerators; ++i ) {
			if ( ElecLoadCenter( LoadCenterNum ).ElecGen( i ).PlantInfoFound ) {
				LoopID = ElecLoadCenter( LoadCenterNum ).ElecGen( i ).PlantLoopNum;
				SideID = ElecLoadCenter( LoadCenterNum ).ElecGen( i ).LoopSideNum;
				BranchID = ElecLoadCenter( LoadCenterNum ).ElecGen( i ).BranchNum;
				CompID = ElecLoadCenter( LoadCenterNum ).ElecGen( i ).CompNum;
				ThermalLoad += PlantLoop( LoopID ).LoopSide( SideID ).Branch( BranchID ).Comp( CompID ).MyLoad;

			}
		}


	}

	void
	VerifyCustomMetersElecPowerMgr()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Feb 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// check user input for custom meters
		//  need special call to use once CUSTOM METERs are "Made"

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using InputProcessor::SameString;

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
		int loop;

		for ( loop = 1; loop <= NumLoadCenters; ++loop ) {
			ElecLoadCenter( loop ).DemandMeterPtr = GetMeterIndex( ElecLoadCenter( loop ).DemandMeterName );
			if ( ( ElecLoadCenter( loop ).DemandMeterPtr == 0 ) && ( ElecLoadCenter( loop ).OperationScheme == iOpSchemeTrackMeter ) ) { // throw error
				ShowFatalError( "Did not find Meter named: " + ElecLoadCenter( loop ).DemandMeterName + " in " + ElecLoadCenter( loop ).Name );
			}
		}

	}

	void
	ManageInverter( int const LoadCenterNum ) // Load Center number counter
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   June 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// manage inveter models and fill AC variables

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 NormalizedPower;
		int InvertNum;
		int StorNum;
		Real64 tmpEffic( 0.0 );
		Real64 tempACpower( 0.0 );

		//model inverters
		InvertNum = ElecLoadCenter( LoadCenterNum ).InverterModelNum;

		if ( InvertNum <= 0 ) return;

		//first figure DC into inverter
		{ auto const SELECT_CASE_var( ElecLoadCenter( LoadCenterNum ).BussType );

		if ( ( SELECT_CASE_var == DCBussInverter ) || ( SELECT_CASE_var == DCBussInverterACStorage ) ) {
			ElecLoadCenter( LoadCenterNum ).DCElectProdRate = sum( ElecLoadCenter( LoadCenterNum ).ElecGen.DCElectProdRate() );
			Inverter( InvertNum ).DCPowerIn = ElecLoadCenter( LoadCenterNum ).DCElectProdRate;
			Inverter( InvertNum ).DCEnergyIn = Inverter( InvertNum ).DCPowerIn * ( TimeStepSys * SecInHour );
		} else if ( SELECT_CASE_var == DCBussInverterDCStorage ) {
			StorNum = ElecLoadCenter( LoadCenterNum ).StorageModelNum;
			if ( StorNum > 0 ) {
				ElecLoadCenter( LoadCenterNum ).DCElectProdRate = sum( ElecLoadCenter( LoadCenterNum ).ElecGen.DCElectProdRate() );

				Inverter( InvertNum ).DCPowerIn = ElecLoadCenter( LoadCenterNum ).DCElectProdRate;
				Inverter( InvertNum ).DCEnergyIn = Inverter( InvertNum ).DCPowerIn * ( TimeStepSys * SecInHour );
			} else { // throw error
				ShowFatalError( "Electric load center is not set up properly, check electrical storage object for " + ElecLoadCenter( LoadCenterNum ).Name );
			}

		}}

		// check availability schedule
		if ( GetCurrentScheduleValue( Inverter( InvertNum ).AvailSchedPtr ) > 0.0 ) {

			// now calculate Inverter based on model type
			{ auto const SELECT_CASE_var( Inverter( InvertNum ).ModelType );

			if ( SELECT_CASE_var == CECLookUpTableModel ) { //interpolation model from test data

				// we don't model voltage, so use nominal voltage
				NormalizedPower = Inverter( InvertNum ).DCPowerIn / Inverter( InvertNum ).RatedPower;

				// get efficiency
				if ( NormalizedPower <= 0.1 ) {
					// extrapolate or fix at 10% value? fix it for now
					tmpEffic = Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 1 );
				} else if ( ( NormalizedPower > 0.1 ) && ( NormalizedPower < 0.20 ) ) {
					tmpEffic = Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 1 ) + ( ( NormalizedPower - 0.1 ) / ( 0.2 - 0.1 ) ) * ( Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 2 ) - Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 1 ) );
				} else if ( NormalizedPower == 0.2 ) {
					tmpEffic = Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 2 );
				} else if ( ( NormalizedPower > 0.2 ) && ( NormalizedPower < 0.30 ) ) {
					tmpEffic = Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 2 ) + ( ( NormalizedPower - 0.2 ) / ( 0.3 - 0.2 ) ) * ( Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 3 ) - Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 2 ) );
				} else if ( NormalizedPower == 0.3 ) {
					tmpEffic = Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 3 );
				} else if ( ( NormalizedPower > 0.3 ) && ( NormalizedPower < 0.50 ) ) {
					tmpEffic = Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 3 ) + ( ( NormalizedPower - 0.3 ) / ( 0.5 - 0.3 ) ) * ( Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 4 ) - Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 3 ) );
				} else if ( NormalizedPower == 0.5 ) {
					tmpEffic = Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 4 );
				} else if ( ( NormalizedPower > 0.5 ) && ( NormalizedPower < 0.75 ) ) {
					tmpEffic = Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 4 ) + ( ( NormalizedPower - 0.5 ) / ( 0.75 - 0.5 ) ) * ( Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 5 ) - Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 4 ) );
				} else if ( NormalizedPower == 0.75 ) {
					tmpEffic = Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 5 );
				} else if ( ( NormalizedPower > 0.75 ) && ( NormalizedPower < 1.0 ) ) {
					tmpEffic = Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 5 ) + ( ( NormalizedPower - 0.75 ) / ( 1.0 - 0.75 ) ) * ( Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 6 ) - Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 5 ) );
				} else if ( NormalizedPower >= 1.0 ) {
					tmpEffic = Inverter( InvertNum ).LUTable.NomVoltEfficiencyARR( 6 );
				} else {
					assert( false );
				}

				Inverter( InvertNum ).Efficiency = max( tmpEffic, 0.0 );
				Inverter( InvertNum ).Efficiency = min( Inverter( InvertNum ).Efficiency, 1.0 );

				tempACpower = Inverter( InvertNum ).Efficiency * Inverter( InvertNum ).DCPowerIn;

			} else if ( SELECT_CASE_var == CurveFuncOfPower ) {
				NormalizedPower = Inverter( InvertNum ).DCPowerIn / Inverter( InvertNum ).RatedPower;

				tmpEffic = CurveValue( Inverter( InvertNum ).CurveNum, NormalizedPower );

				Inverter( InvertNum ).Efficiency = max( tmpEffic, Inverter( InvertNum ).MinEfficiency );
				Inverter( InvertNum ).Efficiency = min( Inverter( InvertNum ).Efficiency, Inverter( InvertNum ).MaxEfficiency );

				tempACpower = Inverter( InvertNum ).Efficiency * Inverter( InvertNum ).DCPowerIn;
				if ( tempACpower < Inverter( InvertNum ).MinPower ) { // not enough to produce any AC power.  all lost. also standby mode
					tempACpower = 0.0;

				} else if ( tempACpower > Inverter( InvertNum ).MaxPower ) { // too much DC for inverter to handle, excess is lost
					tempACpower = Inverter( InvertNum ).MaxPower;
					Inverter( InvertNum ).AncillACuseRate = 0.0;
					Inverter( InvertNum ).AncillACuseEnergy = 0.0;
				} else {
					Inverter( InvertNum ).AncillACuseRate = 0.0;
					Inverter( InvertNum ).AncillACuseEnergy = 0.0;
				}

			} else if ( SELECT_CASE_var == SimpleConstantEff ) {

				tempACpower = Inverter( InvertNum ).Efficiency * Inverter( InvertNum ).DCPowerIn;

			} else {
				assert( false );
			}}

			Inverter( InvertNum ).ACPowerOut = tempACpower;
			Inverter( InvertNum ).ACEnergyOut = tempACpower * ( TimeStepSys * SecInHour );

			if ( tempACpower == 0.0 ) {
				Inverter( InvertNum ).AncillACuseEnergy = Inverter( InvertNum ).StandbyPower * ( TimeStepSys * SecInHour );
				Inverter( InvertNum ).AncillACuseRate = Inverter( InvertNum ).StandbyPower;
			}
		} else { // not available per schedule, inverter is dead.
			//  assume thermal shunt for DC in, but no standby electricity
			Inverter( InvertNum ).ACPowerOut = 0.0;
			Inverter( InvertNum ).ACEnergyOut = 0.0;
			Inverter( InvertNum ).AncillACuseRate = 0.0;
			Inverter( InvertNum ).AncillACuseEnergy = 0.0;

		}

		//update report variables
		Inverter( InvertNum ).ThermLossRate = Inverter( InvertNum ).DCPowerIn - Inverter( InvertNum ).ACPowerOut + Inverter( InvertNum ).StandbyPower;
		Inverter( InvertNum ).ThermLossEnergy = Inverter( InvertNum ).ThermLossRate * ( TimeStepSys * SecInHour );
		Inverter( InvertNum ).QdotConvZone = Inverter( InvertNum ).ThermLossRate * ( 1.0 - Inverter( InvertNum ).ZoneRadFract );
		Inverter( InvertNum ).QdotRadZone = Inverter( InvertNum ).ThermLossRate * Inverter( InvertNum ).ZoneRadFract;

	}

	void
	UpdateLoadCenterRecords( int const LoadCenterNum ) // Load Center index
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          B. Griffith
		//       DATE WRITTEN:    February 2008
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// reporting

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// USE STATEMENTS: na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InvertNum; // inverter index
		int StorNum; // electrical storage index
		// Flow

		if ( ElecLoadCenter( LoadCenterNum ).NumGenerators <= 0 ) return;

		{ auto const SELECT_CASE_var( ElecLoadCenter( LoadCenterNum ).BussType );

		if ( SELECT_CASE_var == ACBuss ) {
			ElecLoadCenter( LoadCenterNum ).ElectProdRate = sum( ElecLoadCenter( LoadCenterNum ).ElecGen.ElectProdRate() );
			ElecLoadCenter( LoadCenterNum ).ElectricityProd = sum( ElecLoadCenter( LoadCenterNum ).ElecGen.ElectricityProd() );

		} else if ( SELECT_CASE_var == ACBussStorage ) {
			StorNum = ElecLoadCenter( LoadCenterNum ).StorageModelNum;
			if ( StorNum > 0 ) {
				ElecLoadCenter( LoadCenterNum ).ElectProdRate = sum( ElecLoadCenter( LoadCenterNum ).ElecGen.ElectProdRate() ) + ElecStorage( StorNum ).DrawnPower - ElecStorage( StorNum ).StoredPower;

				ElecLoadCenter( LoadCenterNum ).ElectricityProd = sum( ElecLoadCenter( LoadCenterNum ).ElecGen.ElectricityProd() ) + ElecStorage( StorNum ).DrawnEnergy - ElecStorage( StorNum ).StoredEnergy;
			}

		} else if ( ( SELECT_CASE_var == DCBussInverter ) || ( SELECT_CASE_var == DCBussInverterDCStorage ) ) {
			InvertNum = ElecLoadCenter( LoadCenterNum ).InverterModelNum;
			if ( InvertNum > 0 ) {
				ElecLoadCenter( LoadCenterNum ).ElectProdRate = Inverter( InvertNum ).ACPowerOut;
				ElecLoadCenter( LoadCenterNum ).ElectricityProd = Inverter( InvertNum ).ACEnergyOut;
			}

		} else if ( SELECT_CASE_var == DCBussInverterACStorage ) {
			StorNum = ElecLoadCenter( LoadCenterNum ).StorageModelNum;
			InvertNum = ElecLoadCenter( LoadCenterNum ).InverterModelNum;
			if ( ( InvertNum > 0 ) && ( StorNum > 0 ) ) {
				ElecLoadCenter( LoadCenterNum ).ElectProdRate = Inverter( InvertNum ).ACPowerOut + ElecStorage( StorNum ).DrawnPower - ElecStorage( StorNum ).StoredPower;
				ElecLoadCenter( LoadCenterNum ).ElectricityProd = Inverter( InvertNum ).ACEnergyOut + ElecStorage( StorNum ).DrawnEnergy - ElecStorage( StorNum ).StoredEnergy;
			}
		}}

		ElecLoadCenter( LoadCenterNum ).ThermalProdRate = sum( ElecLoadCenter( LoadCenterNum ).ElecGen.ThermalProdRate() );
		ElecLoadCenter( LoadCenterNum ).ThermalProd = sum( ElecLoadCenter( LoadCenterNum ).ElecGen.ThermalProd() );

	}

	void
	UpdateWholeBuildingRecords()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Richard Liesen
		//       DATE WRITTEN:    February 2003
		//       MODIFIED       B. Griffith  Mar. 2008 multiple load centers.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// reporting

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: formerly called UpdateRecords when there was only one load center
		//  now this reporting is for the entire model.

		// USE STATEMENTS: na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static Real64 ElecProducedCoGen( 0.0 );
		static Real64 ElecProducedFacility( 0.0 );
		static Real64 TotalPurchased( 0.0 );
		static Real64 TotalSurplus( 0.0 );
		static Real64 NetPurchased( 0.0 );

		// Flow

		ElecProducedCoGen = GetInstantMeterValue( ElecProducedCoGenIndex, 2 ); //whole building
		ElecProducedFacility = ElecProducedCoGen + WholeBldgElectSummary.ElecProducedPVRate * TimeStepSys * SecInHour + WholeBldgElectSummary.ElecProducedWTRate * TimeStepSys * SecInHour; //whole building

		WholeBldgElectSummary.ElectricityProd = ElecProducedFacility; //whole building
		WholeBldgElectSummary.ElectProdRate = ElecProducedFacility / ( TimeStepSys * SecInHour ); //whole building

		//Report the Total Electric Power Purchased [W], If negative then there is extra power to be sold or stored.
		TotalPurchased = WholeBldgElectSummary.TotalElectricDemand - WholeBldgElectSummary.ElectProdRate;
		//Check this value against a tolerance to aid in reporting.
		if ( std::abs( TotalPurchased ) < 0.0001 ) TotalPurchased = 0.0;
		if ( TotalPurchased < 0.0 ) TotalPurchased = 0.0; // don't want negative purchased...
		WholeBldgElectSummary.ElectPurchRate = TotalPurchased;
		//Report the Total Electric Energy Purchased [J]
		WholeBldgElectSummary.ElectricityPurch = WholeBldgElectSummary.ElectPurchRate * TimeStepSys * SecInHour;

		//report the total electric surplus....
		TotalSurplus = WholeBldgElectSummary.ElectProdRate - WholeBldgElectSummary.TotalElectricDemand;
		if ( std::abs( TotalSurplus ) < 0.0001 ) TotalSurplus = 0.0;
		if ( TotalSurplus < 0.0 ) TotalSurplus = 0.0; // don't want negative surplus

		WholeBldgElectSummary.ElectSurplusRate = TotalSurplus;
		WholeBldgElectSummary.ElectricitySurplus = TotalSurplus * TimeStepSys * SecInHour;

		//report the net electricity , + is purchased, - is surplus
		NetPurchased = WholeBldgElectSummary.TotalElectricDemand - WholeBldgElectSummary.ElectProdRate;
		WholeBldgElectSummary.ElectricityNetRate = NetPurchased;

		WholeBldgElectSummary.ElectricityNet = WholeBldgElectSummary.ElectricityNetRate * TimeStepSys * SecInHour;

	}

	void
	FigureInverterZoneGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Couple inverter skin losses to zone heat gains

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;

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
		static bool MyEnvrnFlag( true );

		if ( NumInverters == 0 ) return;

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			Inverter.QdotConvZone() = 0.0;
			Inverter.QdotRadZone() = 0.0;
			MyEnvrnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		//  IF(DoingSizing)THEN

		//  ENDIF

	}

	//***********************************************************************************************************************************

	void
	ManageElectCenterStorageInteractions(
		int const LoadCenterNum, // load center number, index for structure
		Real64 & StorageDrawnPower, // Electric Power Draw Rate from storage units
		Real64 & StorageStoredPower // Electric Power Store Rate from storage units
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June-August 2008
		//       MODIFIED       BG May 2009, added EMS
		//                      BN (FSEC) Feb 2010 (pass out two storage values)
		//                      Y. KyungTae & W. Wang July-August, 2011 Added a battery model
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// manage controls and calculations related to electrical storage in Electric load center

		// METHODOLOGY EMPLOYED:
		// started from fuel cell storage routine and modified for general use in load center
		// needs lots of work

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::ReallocateRealArray;
		using CurveManager::CurveValue;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataGlobals::TimeStep;
		using DataGlobals::TimeStepZone;
		using DataGlobals::SecInHour;
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::WarmupFlag;
		using DataGlobals::HourOfDay;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 tmpPdraw( 0.0 ); // power draw from storage, working var
		static Real64 tmpPcharge( 0.0 ); // power charge to storage, working var
		static bool drawing( false ); // true if drawing power
		static bool charging( false ); // true if charging
		static int ElecStorNum( 0 );
		static Real64 Pgensupply( 0.0 );
		static Real64 Pdemand( 0.0 );
		static Real64 PpcuLosses( 0.0 );
		static Real64 Pstorage( 0.0 );
		static Array1D_bool MyEnvrnFlag;
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyWarmupFlag; // flag for init after warmup complete
		Real64 TimeElapsed; // Fraction of the current hour that has elapsed (h)
		static int BinNum( 0 );
		static Real64 Input0( 0.0 );
		Real64 I0; // initial guess of current
		Real64 T0; // initial guess of T(I)
		Real64 q0; // initial charge
		Real64 qmax; // maximum capacity
		Real64 qmaxf; // maximum capacity, a function of current(I)
		Real64 k; // change rate from chemically bound charge to available charge
		Real64 c; // fraction available charge capacity to total capacity
		Real64 TotalSOC; // total charge (ThisTimeStepAvailable+ThisTimeStepBound)
		Real64 Ef; // effective internal voltage source, V
		Real64 E0c; // fully charged internal battery voltage
		Real64 Volt; // terminal voltage
		Real64 Pw; // power required
		Real64 E0d; // fully discharged internal battery voltage
		Real64 InternalR; // internal resistance
		Real64 Xf; // normalized maximum capacity at the given current
		Real64 Inew; // converged current
		Real64 Tnew; // charge of discharge time, defined by T=qmaxf/I
		Real64 Imax; // maximum current
		Real64 Numpar; // number of battery in parallel
		Real64 Numser; // number of battery in series
		Real64 Numbattery; // number of battery (Numpar*Numser)
		Real64 initialCharge; // initial charge in Ah
		Real64 dividend;
		Real64 divisor;
		Real64 newAvailable; // new available charge in Ah
		Real64 newBound; // new bound charge in Ah
		static Real64 error( 0.0 ); // error in iterative process
		static Real64 Pactual( 0.0 ); // actual Power output
		static Real64 RHS( 0.0 ); // right hand side of a equation
		Real64 DeltaSOC1; // difference of fractional SOC between this time step and last time step
		Real64 DeltaSOC2; // difference of fractional SOC between last time step and last two time step

		if ( ! ( ElecLoadCenter( LoadCenterNum ).StoragePresent ) ) return;

		ElecStorNum = ElecLoadCenter( LoadCenterNum ).StorageModelNum;

		//_____________________________________
		// begin initalizations
		// perform the one time initializations
		if ( MyOneTimeFlag ) {
			// initialize the environment and sizing flags
			MyEnvrnFlag.allocate( NumElecStorageDevices );
			MyWarmupFlag.allocate( NumElecStorageDevices );
			MyEnvrnFlag = true;
			MyOneTimeFlag = false;
			MyWarmupFlag = false;
		}

		if ( BeginEnvrnFlag && MyEnvrnFlag( ElecStorNum ) ) {
			if ( ElecStorage( ElecStorNum ).StorageModelMode == SimpleBucketStorage ) {
				ElecStorage( ElecStorNum ).LastTimeStepStateOfCharge = ElecStorage( ElecStorNum ).StartingEnergyStored;
				ElecStorage( ElecStorNum ).ThisTimeStepStateOfCharge = ElecStorage( ElecStorNum ).StartingEnergyStored;
				ElecStorage( ElecStorNum ).PelNeedFromStorage = 0.0;
				ElecStorage( ElecStorNum ).PelFromStorage = 0.0;
				ElecStorage( ElecStorNum ).PelIntoStorage = 0.0;
				ElecStorage( ElecStorNum ).QdotConvZone = 0.0;
				ElecStorage( ElecStorNum ).QdotRadZone = 0.0;
				ElecStorage( ElecStorNum ).TimeElapsed = 0.0;
				ElecStorage( ElecStorNum ).ElectEnergyinStorage = 0.0;
				ElecStorage( ElecStorNum ).StoredPower = 0.0;
				ElecStorage( ElecStorNum ).StoredEnergy = 0.0;
				ElecStorage( ElecStorNum ).DecrementedEnergyStored = 0.0;
				ElecStorage( ElecStorNum ).DrawnPower = 0.0;
				ElecStorage( ElecStorNum ).DrawnEnergy = 0.0;
				ElecStorage( ElecStorNum ).ThermLossRate = 0.0;
				ElecStorage( ElecStorNum ).ThermLossEnergy = 0.0;

			} else if ( ElecStorage( ElecStorNum ).StorageModelMode == KiBaMBattery ) {
				initialCharge = ElecStorage( ElecStorNum ).MaxAhCapacity * ElecStorage( ElecStorNum ).StartingSOC;
				ElecStorage( ElecStorNum ).LastTwoTimeStepAvailable = initialCharge * ElecStorage( ElecStorNum ).AvailableFrac;
				ElecStorage( ElecStorNum ).LastTwoTimeStepBound = initialCharge * ( 1.0 - ElecStorage( ElecStorNum ).AvailableFrac );
				ElecStorage( ElecStorNum ).LastTimeStepAvailable = initialCharge * ElecStorage( ElecStorNum ).AvailableFrac;
				ElecStorage( ElecStorNum ).LastTimeStepBound = initialCharge * ( 1.0 - ElecStorage( ElecStorNum ).AvailableFrac );
				ElecStorage( ElecStorNum ).ThisTimeStepAvailable = initialCharge * ElecStorage( ElecStorNum ).AvailableFrac;
				ElecStorage( ElecStorNum ).ThisTimeStepBound = initialCharge * ( 1.0 - ElecStorage( ElecStorNum ).AvailableFrac );
				if ( ElecStorage( ElecStorNum ).LifeCalculation == Battery_LifeCalculation_Yes ) {
					ElecStorage( ElecStorNum ).count0 = 2; // Index 1 is for initial SOC, so new input starts from index 2.
					ElecStorage( ElecStorNum ).B10( 1 ) = ElecStorage( ElecStorNum ).StartingSOC; // the initial fractional SOC is stored as the reference
					ElecStorage( ElecStorNum ).X0 = 0.0;
					ElecStorage( ElecStorNum ).OneNmb0 = 0.0;
					ElecStorage( ElecStorNum ).Nmb0 = 0.0;
					ElecStorage( ElecStorNum ).BatteryDamage = 0.0;
				}
			}
			MyEnvrnFlag( ElecStorNum ) = false;
			MyWarmupFlag( ElecStorNum ) = true;
		}
		if ( ! BeginEnvrnFlag ) MyEnvrnFlag( ElecStorNum ) = true;

		if ( MyWarmupFlag( ElecStorNum ) && ( ! WarmupFlag ) ) {
			// need to reset initial state of charge at beginning of environment but after warm up is complete
			if ( ElecStorage( ElecStorNum ).StorageModelMode == SimpleBucketStorage ) {
				ElecStorage( ElecStorNum ).LastTimeStepStateOfCharge = ElecStorage( ElecStorNum ).StartingEnergyStored;
				ElecStorage( ElecStorNum ).ThisTimeStepStateOfCharge = ElecStorage( ElecStorNum ).StartingEnergyStored;
			} else if ( ElecStorage( ElecStorNum ).StorageModelMode == KiBaMBattery ) {
				initialCharge = ElecStorage( ElecStorNum ).MaxAhCapacity * ElecStorage( ElecStorNum ).StartingSOC;
				ElecStorage( ElecStorNum ).LastTwoTimeStepAvailable = initialCharge * ElecStorage( ElecStorNum ).AvailableFrac;
				ElecStorage( ElecStorNum ).LastTwoTimeStepBound = initialCharge * ( 1.0 - ElecStorage( ElecStorNum ).AvailableFrac );
				ElecStorage( ElecStorNum ).LastTimeStepAvailable = initialCharge * ElecStorage( ElecStorNum ).AvailableFrac;
				ElecStorage( ElecStorNum ).LastTimeStepBound = initialCharge * ( 1.0 - ElecStorage( ElecStorNum ).AvailableFrac );
				ElecStorage( ElecStorNum ).ThisTimeStepAvailable = initialCharge * ElecStorage( ElecStorNum ).AvailableFrac;
				ElecStorage( ElecStorNum ).ThisTimeStepBound = initialCharge * ( 1.0 - ElecStorage( ElecStorNum ).AvailableFrac );
				if ( ElecStorage( ElecStorNum ).LifeCalculation == Battery_LifeCalculation_Yes ) {
					ElecStorage( ElecStorNum ).count0 = 2; // Index 1 is for initial SOC, so new input starts from index 2.
					ElecStorage( ElecStorNum ).B10( 1 ) = ElecStorage( ElecStorNum ).StartingSOC; // the initial fractional SOC is stored as the reference
					ElecStorage( ElecStorNum ).X0 = 0.0;
					ElecStorage( ElecStorNum ).OneNmb0 = 0.0;
					ElecStorage( ElecStorNum ).Nmb0 = 0.0;
					ElecStorage( ElecStorNum ).BatteryDamage = 0.0;
				}
			}
			MyWarmupFlag( ElecStorNum ) = false;
		}

		TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;
		if ( ElecStorage( ElecStorNum ).TimeElapsed != TimeElapsed ) { //time changed, update last with "current" result from previous time
			//   When program comes here, this means the first iteration of a new system time step. Battery life calculation needs to be considered
			//   for the Kinetic battery model.
			if ( ElecStorage( ElecStorNum ).StorageModelMode == KiBaMBattery && ElecStorage( ElecStorNum ).LifeCalculation == Battery_LifeCalculation_Yes ) {
				//    At this point, the current values, last time step values and last two time step values have not been updated, hence:
				//    "ThisTimeStep*" actually points to the previous one time step
				//    "LastTimeStep*" actually points to the previous two time steps
				//    "LastTwoTimeStep" actually points to the previous three time steps

				//      Calculate the fractional SOC change between the "current" time step and the "previous one" time step
				DeltaSOC1 = ElecStorage( ElecStorNum ).ThisTimeStepAvailable + ElecStorage( ElecStorNum ).ThisTimeStepBound - ElecStorage( ElecStorNum ).LastTimeStepAvailable - ElecStorage( ElecStorNum ).LastTimeStepBound;
				DeltaSOC1 /= ElecStorage( ElecStorNum ).MaxAhCapacity;

				//      Calculate the fractional SOC change between the "previous one" time step and the "previous two" time steps
				DeltaSOC2 = ElecStorage( ElecStorNum ).LastTimeStepAvailable + ElecStorage( ElecStorNum ).LastTimeStepBound - ElecStorage( ElecStorNum ).LastTwoTimeStepAvailable - ElecStorage( ElecStorNum ).LastTwoTimeStepBound;
				DeltaSOC2 /= ElecStorage( ElecStorNum ).MaxAhCapacity;

				//     DeltaSOC2 = 0 may occur at the begining of each simulation environment.
				//     DeltaSOC1 * DeltaSOC2 means that the SOC from "LastTimeStep" is a peak or valley. Only peak or valley needs
				//     to call the rain flow algorithm
				if ( ( DeltaSOC2 == 0 ) || ( ( DeltaSOC1 * DeltaSOC2 ) < 0 ) ) {
					//     Because we cannot determine whehter "ThisTimeStep" is a peak or valley (next time step is unknown yet), we
					//     use the "LastTimeStep" value for battery life calculation.
					Input0 = ( ElecStorage( ElecStorNum ).LastTimeStepAvailable + ElecStorage( ElecStorNum ).LastTimeStepBound ) / ElecStorage( ElecStorNum ).MaxAhCapacity;
					ElecStorage( ElecStorNum ).B10( ElecStorage( ElecStorNum ).count0 ) = Input0;

					//        The arrary size needs to be increased when count = MaxRainflowArrayBounds. Please note that (MaxRainflowArrayBounds +1)
					//        is the index used in the subroutine RainFlow. So we cannot reallocate array size until count = MaxRainflowArrayBounds +1.
					if ( ElecStorage( ElecStorNum ).count0 == MaxRainflowArrayBounds ) {
						ElecStorage( ElecStorNum ).B10.redimension( MaxRainflowArrayBounds + 1 + MaxRainflowArrayInc, 0.0 );
						ElecStorage( ElecStorNum ).X0.redimension( MaxRainflowArrayBounds + 1 + MaxRainflowArrayInc, 0.0 );
						MaxRainflowArrayBounds += MaxRainflowArrayInc;
					}

					Rainflow( ElecStorage( ElecStorNum ).CycleBinNum, Input0, ElecStorage( ElecStorNum ).B10, ElecStorage( ElecStorNum ).X0, ElecStorage( ElecStorNum ).count0, ElecStorage( ElecStorNum ).Nmb0, ElecStorage( ElecStorNum ).OneNmb0, MaxRainflowArrayBounds + 1 );

					ElecStorage( ElecStorNum ).BatteryDamage = 0.0;

					for ( BinNum = 1; BinNum <= ElecStorage( ElecStorNum ).CycleBinNum; ++BinNum ) {
						//       Battery damage is calculated by accumulating the impact from each cycle.
						ElecStorage( ElecStorNum ).BatteryDamage += ElecStorage( ElecStorNum ).OneNmb0( BinNum ) / CurveValue( ElecStorage( ElecStorNum ).LifeCurveNum, ( double( BinNum ) / double( ElecStorage( ElecStorNum ).CycleBinNum ) ) );
					}
				}
			}

			//   Updating "LastTimeStep" and "LastTwoTimeStep" should be done after calling the rain flow algorithm.
			//   Otherwise, it is not possible to determine whehter "LastTimeStep" is a peak or valley value.

			ElecStorage( ElecStorNum ).LastTimeStepStateOfCharge = ElecStorage( ElecStorNum ).ThisTimeStepStateOfCharge;
			ElecStorage( ElecStorNum ).LastTwoTimeStepAvailable = ElecStorage( ElecStorNum ).LastTimeStepAvailable;
			ElecStorage( ElecStorNum ).LastTwoTimeStepBound = ElecStorage( ElecStorNum ).LastTimeStepBound;
			ElecStorage( ElecStorNum ).LastTimeStepAvailable = ElecStorage( ElecStorNum ).ThisTimeStepAvailable;
			ElecStorage( ElecStorNum ).LastTimeStepBound = ElecStorage( ElecStorNum ).ThisTimeStepBound;
			ElecStorage( ElecStorNum ).TimeElapsed = TimeElapsed;
		}

		// end Inits

		//Begin determine Power demand request

		if ( ElecLoadCenter( LoadCenterNum ).InverterPresent ) {
			// this will be lagged from last calc.
			PpcuLosses = Inverter( ElecLoadCenter( LoadCenterNum ).InverterModelNum ).ThermLossRate;
		} else {
			PpcuLosses = 0.0;
		}

		//  Pdemand = SUM(ElecLoadCenter(LoadCenterNum)%ElecGen%PowerRequestThisTimestep) + PpcuLosses
		//  Modified when improved electric load center disptach procedure was implemented
		Pdemand = ElecLoadCenter( LoadCenterNum ).TotalPowerRequest + PpcuLosses;
		// END determine Power demand request.

		// BEGIN determine available generation supply
		{ auto const SELECT_CASE_var( ElecLoadCenter( LoadCenterNum ).BussType );

		if ( SELECT_CASE_var == ACBussStorage ) {
			Pgensupply = sum( ElecLoadCenter( LoadCenterNum ).ElecGen.ElectProdRate() );
		} else if ( SELECT_CASE_var == DCBussInverterDCStorage ) {
			ElecLoadCenter( LoadCenterNum ).DCElectProdRate = sum( ElecLoadCenter( LoadCenterNum ).ElecGen.DCElectProdRate() );
			Pgensupply = ElecLoadCenter( LoadCenterNum ).DCElectProdRate;
		} else if ( SELECT_CASE_var == DCBussInverterACStorage ) {

			Pgensupply = Inverter( ElecLoadCenter( LoadCenterNum ).InverterModelNum ).ACPowerOut;
		}}
		// End determine available generation

		//initialize locals
		tmpPdraw = 0.0;
		tmpPcharge = 0.0;
		drawing = false;
		charging = false;
		Pstorage = 0.0;

		if ( ElecStorage( ElecStorNum ).StorageModelMode == KiBaMBattery ) {
			Numpar = ElecStorage( ElecStorNum ).ParallelNum;
			Numser = ElecStorage( ElecStorNum ).SeriesNum;
			Numbattery = Numpar * Numser;
			InternalR = ElecStorage( ElecStorNum ).InternalR;
			qmax = ElecStorage( ElecStorNum ).MaxAhCapacity;
			E0c = ElecStorage( ElecStorNum ).ChargedOCV;
			E0d = ElecStorage( ElecStorNum ).DischargedOCV;
			k = ElecStorage( ElecStorNum ).ChargeConversionRate;
			c = ElecStorage( ElecStorNum ).AvailableFrac;
		}

		//*****************************************************************************************************************

		// step 1 figure out what is desired of electrical storage system

		if ( Pgensupply < ( Pdemand ) ) {
			//draw from storage
			tmpPdraw = Pdemand - Pgensupply;
			tmpPcharge = 0.0;
			drawing = true;
			charging = false;

		} else if ( Pgensupply > ( Pdemand ) ) {
			//add to storage
			tmpPcharge = Pgensupply - Pdemand;
			tmpPdraw = 0.0;
			charging = true;
			drawing = false;

		} else if ( Pgensupply == ( Pdemand ) ) {
			//do nothing
			tmpPcharge = 0.0;
			tmpPdraw = 0.0;
			charging = false;
			drawing = false;
		}

		// EMS override -- intent to draw, charge or hold?
		if ( ( ElecStorage( ElecStorNum ).EMSOverridePelFromStorage ) && ( ! ElecStorage( ElecStorNum ).EMSOverridePelIntoStorage ) ) {
			// EMS is calling for specific discharge rate
			tmpPdraw = max( ElecStorage( ElecStorNum ).EMSValuePelFromStorage, 0.0 );
			tmpPcharge = 0.0;
			drawing = true;
			charging = false;
		} else if ( ( ! ElecStorage( ElecStorNum ).EMSOverridePelFromStorage ) && ( ElecStorage( ElecStorNum ).EMSOverridePelIntoStorage ) ) {
			// EMS is calling for specific charge rate
			tmpPcharge = max( ElecStorage( ElecStorNum ).EMSValuePelIntoStorage, 0.0 );
			tmpPdraw = 0.0;
			drawing = false;
			charging = true;
		} else if ( ( ElecStorage( ElecStorNum ).EMSOverridePelFromStorage ) && ( ElecStorage( ElecStorNum ).EMSOverridePelIntoStorage ) ) {
			// EMS is asking to override both
			if ( ElecStorage( ElecStorNum ).EMSValuePelIntoStorage > ElecStorage( ElecStorNum ).EMSValuePelFromStorage ) {
				tmpPcharge = ElecStorage( ElecStorNum ).EMSValuePelIntoStorage - ElecStorage( ElecStorNum ).EMSValuePelFromStorage;
				tmpPdraw = 0.0;
				drawing = false;
				charging = true;
			} else if ( ElecStorage( ElecStorNum ).EMSValuePelIntoStorage < ElecStorage( ElecStorNum ).EMSValuePelFromStorage ) {
				tmpPdraw = ElecStorage( ElecStorNum ).EMSValuePelFromStorage - ElecStorage( ElecStorNum ).EMSValuePelIntoStorage;
				tmpPcharge = 0.0;
				drawing = true;
				charging = false;
			} else { //they equal just hold
				tmpPdraw = 0.0;
				tmpPcharge = 0.0;
				drawing = false;
				charging = false;
			}
		}

		//*****************************************************************************************************************

		//  step 2, figure out what is possible for electrical storage draws/charges

		if ( charging ) {

			if ( ElecStorage( ElecStorNum ).StorageModelMode == SimpleBucketStorage ) {

				if ( ElecStorage( ElecStorNum ).LastTimeStepStateOfCharge >= ElecStorage( ElecStorNum ).MaxEnergyCapacity ) {
					// storage full!  no more allowed!
					tmpPcharge = 0.0;
					//       Constrained = .TRUE.
					charging = false;
				}
				if ( tmpPcharge > ElecStorage( ElecStorNum ).MaxPowerStore ) {
					tmpPcharge = ElecStorage( ElecStorNum ).MaxPowerStore;
					//        Constrained = .TRUE.
				}

				//now add energy to storage from charging
				if ( ( ElecStorage( ElecStorNum ).LastTimeStepStateOfCharge + tmpPcharge * TimeStepSys * SecInHour * ElecStorage( ElecStorNum ).EnergeticEfficCharge ) < ElecStorage( ElecStorNum ).MaxEnergyCapacity ) {

					ElecStorage( ElecStorNum ).ThisTimeStepStateOfCharge = ElecStorage( ElecStorNum ).LastTimeStepStateOfCharge + tmpPcharge * TimeStepSys * SecInHour * ElecStorage( ElecStorNum ).EnergeticEfficCharge;
				} else { // would over charge this time step

					tmpPcharge = ( ElecStorage( ElecStorNum ).MaxEnergyCapacity - ElecStorage( ElecStorNum ).LastTimeStepStateOfCharge ) / ( TimeStepSys * SecInHour * ElecStorage( ElecStorNum ).EnergeticEfficCharge );
					//     Constrained = .TRUE.
					ElecStorage( ElecStorNum ).ThisTimeStepStateOfCharge = ElecStorage( ElecStorNum ).LastTimeStepStateOfCharge + tmpPcharge * TimeStepSys * SecInHour * ElecStorage( ElecStorNum ).EnergeticEfficCharge;
				}
				Pstorage = tmpPcharge;

			}

			if ( ElecStorage( ElecStorNum ).StorageModelMode == KiBaMBattery ) {
				//*************************************************
				//The sign of power and current is negative in charging
				//*************************************************
				Pw = -tmpPcharge / Numbattery;
				q0 = ElecStorage( ElecStorNum ).LastTimeStepAvailable + ElecStorage( ElecStorNum ).LastTimeStepBound;

				I0 = 1.0; // Initial assumption
				T0 = std::abs( qmax / I0 ); // Initial Assumption
				qmaxf = qmax * k * c * T0 / ( 1.0 - std::exp( -k * T0 ) + c * ( k * T0 - 1.0 + std::exp( -k * T0 ) ) ); //Initial calculation of a function qmax(I)
				Xf = q0 / qmaxf;
				Ef = E0d + CurveValue( ElecStorage( ElecStorNum ).ChargeCurveNum, Xf ); //E0d+Ac*Xf+Cc*Xf/(Dc-Xf) (use curve)
				Volt = Ef - I0 * InternalR;
				Inew = Pw / Volt;
				Tnew = qmaxf / std::abs( Inew );
				error = 1.0;

				while ( error > 0.0001 ) { //Iteration process to get converged current(I)
					I0 = Inew;
					T0 = Tnew;
					qmaxf = qmax * k * c * T0 / ( 1.0 - std::exp( -k * T0 ) + c * ( k * T0 - 1.0 + std::exp( -k * T0 ) ) );
					Xf = q0 / qmaxf;
					Ef = E0d + CurveValue( ElecStorage( ElecStorNum ).ChargeCurveNum, Xf ); //E0d+Ac*Xf+Cc*Xf/(Dc-Xf) (use curve)
					Volt = Ef - I0 * InternalR;
					Inew = Pw / Volt;
					Tnew = std::abs( qmaxf / Inew ); // ***Always positive here
					error = std::abs( Inew - I0 );
				}

				dividend = -k * c * qmax + k * ElecStorage( ElecStorNum ).LastTimeStepAvailable * std::exp( -k * TimeStepSys ) + q0 * k * c * ( 1.0 - std::exp( -k * TimeStepSys ) );
				divisor = 1.0 - std::exp( -k * TimeStepSys ) + c * ( k * TimeStepSys - 1 + std::exp( -k * TimeStepSys ) );
				Imax = dividend / divisor;
				// Below: This is the limit of charging current from Charge Rate Limit (input)
				Imax = max( Imax, - ( qmax - q0 ) * ElecStorage( ElecStorNum ).MaxChargeRate );

				if ( std::abs( I0 ) <= std::abs( Imax ) ) {
					I0 = Pw / Volt;
					Pactual = I0 * Volt;
				} else {
					I0 = Imax;
					qmaxf = 80.0; //Initial assumption to solve the equation using iterative method
					error = 10.0; //Initial assumption ...
					while ( error > 0.001 ) {
						// *** I0(current) should be positive for this calculation
						RHS = ( qmax * k * c * qmaxf / std::abs( I0 ) ) / ( 1.0 - std::exp( -k * qmaxf / std::abs( I0 ) ) + c * ( k * qmaxf / std::abs( I0 ) - 1.0 + std::exp( -k * qmaxf / std::abs( I0 ) ) ) );
						error = std::abs( qmaxf - RHS );
						qmaxf = RHS;
					}
				}

			} // end KiBaM charging

		} //charging

		if ( drawing ) {
			if ( ElecStorage( ElecStorNum ).StorageModelMode == SimpleBucketStorage ) {

				if ( ElecStorage( ElecStorNum ).LastTimeStepStateOfCharge <= 0.0 ) {
					// storage empty  no more allowed!
					tmpPdraw = 0.0;
					drawing = false;
				}
				if ( tmpPdraw > ElecStorage( ElecStorNum ).MaxPowerDraw ) {
					tmpPdraw = ElecStorage( ElecStorNum ).MaxPowerDraw;
				}

				//now take energy from storage by drawing  (amplified by energetic effic)
				if ( ( ElecStorage( ElecStorNum ).LastTimeStepStateOfCharge - tmpPdraw * TimeStepSys * SecInHour / ElecStorage( ElecStorNum ).EnergeticEfficDischarge ) > 0.0 ) {

					ElecStorage( ElecStorNum ).ThisTimeStepStateOfCharge = ElecStorage( ElecStorNum ).LastTimeStepStateOfCharge - tmpPdraw * TimeStepSys * SecInHour / ElecStorage( ElecStorNum ).EnergeticEfficDischarge;
				} else { //would over drain storage this timestep so reduce tmpPdraw
					tmpPdraw = ElecStorage( ElecStorNum ).LastTimeStepStateOfCharge * ElecStorage( ElecStorNum ).EnergeticEfficDischarge / ( TimeStepSys * SecInHour );
					ElecStorage( ElecStorNum ).ThisTimeStepStateOfCharge = ElecStorage( ElecStorNum ).LastTimeStepStateOfCharge - tmpPdraw * TimeStepSys * SecInHour / ElecStorage( ElecStorNum ).EnergeticEfficDischarge;
					ElecStorage( ElecStorNum ).ThisTimeStepStateOfCharge = max( ElecStorage( ElecStorNum ).ThisTimeStepStateOfCharge, 0.0 );
				}

				ElecStorage( ElecStorNum ).ThermLossRate = tmpPdraw * ( 1.0 / ElecStorage( ElecStorNum ).EnergeticEfficDischarge - 1.0 );
				Pstorage = -tmpPdraw;
			} // simple discharging

			if ( ElecStorage( ElecStorNum ).StorageModelMode == KiBaMBattery ) {

				//**********************************************
				//The sign of power and current is positive in discharging
				//**********************************************

				Pw = tmpPdraw / Numbattery;
				q0 = ElecStorage( ElecStorNum ).LastTimeStepAvailable + ElecStorage( ElecStorNum ).LastTimeStepBound;
				bool const ok = determineCurrentForBatteryDischarge( I0, T0, Volt, Pw, q0, ElecStorage( ElecStorNum ).DischargeCurveNum, k, c, qmax, E0c, InternalR );
				if ( !ok ) {
					ShowFatalError( "ElectricLoadCenter:Storage:Battery named=\"" + ElecStorage( ElecStorNum ).Name + "\". Battery discharge current could not be estimated due to iteration limit reached. " );
				}

				dividend = k * ElecStorage( ElecStorNum ).LastTimeStepAvailable * std::exp( -k * TimeStepSys ) + q0 * k * c * ( 1.0 - std::exp( -k * TimeStepSys ) );
				divisor = 1.0 - std::exp( -k * TimeStepSys ) + c * ( k * TimeStepSys - 1.0 + std::exp( -k * TimeStepSys ) );
				Imax = dividend / divisor;
				Imax = min( Imax, ElecStorage( ElecStorNum ).MaxDischargeI );
				if ( std::abs( I0 ) <= Imax ) {
					I0 = Pw / Volt;
					Pactual = I0 * Volt;
				} else {
					I0 = Imax;
					qmaxf = 10.0; //Initial assumption to solve the equation using iterative method
					error = 10.0; //Initial assumption ...
					while ( error > 0.001 ) {
						RHS = ( qmax * k * c * qmaxf / I0 ) / ( 1.0 - std::exp( -k * qmaxf / I0 ) + c * ( k * qmaxf / I0 - 1 + std::exp( -k * qmaxf / I0 ) ) );
						error = std::abs( qmaxf - RHS );
						qmaxf = RHS;
					}
					Xf = ( qmax - q0 ) / qmaxf;
					Ef = E0c + CurveValue( ElecStorage( ElecStorNum ).DischargeCurveNum, Xf );
					Volt = Ef - I0 * InternalR;
				}

				if ( Volt < ElecStorage( ElecStorNum ).CutoffV ) {
					I0 = 0.0;
				}
			} // end KiBaM discharging

		} //drawing

		// correct if not available.
		if ( GetCurrentScheduleValue( ElecStorage( ElecStorNum ).AvailSchedPtr ) == 0.0 ) {
			if ( ( ! ElecStorage( ElecStorNum ).EMSOverridePelFromStorage ) && ( ! ElecStorage( ElecStorNum ).EMSOverridePelIntoStorage ) ) {
				charging = false;
				drawing = false;
			}
		}

		if ( ElecStorage( ElecStorNum ).StorageModelMode == SimpleBucketStorage ) {
			if ( ( ! charging ) && ( ! drawing ) ) {

				ElecStorage( ElecStorNum ).ThisTimeStepStateOfCharge = ElecStorage( ElecStorNum ).LastTimeStepStateOfCharge;
				ElecStorage( ElecStorNum ).PelIntoStorage = 0.0;
				ElecStorage( ElecStorNum ).PelFromStorage = 0.0;
				Pstorage = 0.0;
			}

			if ( Pstorage >= 0.0 ) {

				ElecStorage( ElecStorNum ).PelIntoStorage = Pstorage;
				ElecStorage( ElecStorNum ).PelFromStorage = 0.0;
			}

			if ( Pstorage < 0.0 ) {

				ElecStorage( ElecStorNum ).PelIntoStorage = 0.0;
				ElecStorage( ElecStorNum ).PelFromStorage = -Pstorage;

			}

			ElecStorage( ElecStorNum ).ElectEnergyinStorage = ElecStorage( ElecStorNum ).ThisTimeStepStateOfCharge;
			ElecStorage( ElecStorNum ).StoredPower = ElecStorage( ElecStorNum ).PelIntoStorage;
			ElecStorage( ElecStorNum ).StoredEnergy = ElecStorage( ElecStorNum ).PelIntoStorage * TimeStepSys * SecInHour;
			ElecStorage( ElecStorNum ).DecrementedEnergyStored = -1.0 * ElecStorage( ElecStorNum ).StoredEnergy;
			ElecStorage( ElecStorNum ).DrawnPower = ElecStorage( ElecStorNum ).PelFromStorage;
			ElecStorage( ElecStorNum ).DrawnEnergy = ElecStorage( ElecStorNum ).PelFromStorage * TimeStepSys * SecInHour;
			ElecStorage( ElecStorNum ).ThermLossRate = max( ElecStorage( ElecStorNum ).StoredPower * ( 1.0 - ElecStorage( ElecStorNum ).EnergeticEfficCharge ), ElecStorage( ElecStorNum ).DrawnPower * ( 1.0 - ElecStorage( ElecStorNum ).EnergeticEfficDischarge ) );
			ElecStorage( ElecStorNum ).ThermLossEnergy = ElecStorage( ElecStorNum ).ThermLossRate * TimeStepSys * SecInHour;

			if ( ElecStorage( ElecStorNum ).ZoneNum > 0 ) { // set values for zone heat gains
				ElecStorage( ElecStorNum ).QdotConvZone = ( 1.0 - ElecStorage( ElecStorNum ).ZoneRadFract ) * ElecStorage( ElecStorNum ).ThermLossRate;
				ElecStorage( ElecStorNum ).QdotRadZone = ( ElecStorage( ElecStorNum ).ZoneRadFract ) * ElecStorage( ElecStorNum ).ThermLossRate;
			}

			StorageStoredPower = ElecStorage( ElecStorNum ).StoredPower;
			StorageDrawnPower = ElecStorage( ElecStorNum ).DrawnPower;
		} // Outputs for simple battery model

		if ( ElecStorage( ElecStorNum ).StorageModelMode == KiBaMBattery ) {

			if ( ( ! charging ) && ( ! drawing ) ) {
				ElecStorage( ElecStorNum ).ThisTimeStepAvailable = ElecStorage( ElecStorNum ).LastTimeStepAvailable;
				ElecStorage( ElecStorNum ).ThisTimeStepBound = ElecStorage( ElecStorNum ).LastTimeStepBound;
				I0 = 0.0;
				Volt = 0.0;
				q0 = ElecStorage( ElecStorNum ).LastTimeStepAvailable + ElecStorage( ElecStorNum ).LastTimeStepBound;
			} else {
				newAvailable = ElecStorage( ElecStorNum ).LastTimeStepAvailable * std::exp( -k * TimeStepSys ) + ( q0 * k * c - I0 ) * ( 1.0 - std::exp( -k * TimeStepSys ) ) / k - I0 * c * ( k * TimeStepSys - 1.0 + std::exp( -k * TimeStepSys ) ) / k;
				newBound = ElecStorage( ElecStorNum ).LastTimeStepBound * std::exp( -k * TimeStepSys ) + q0 * ( 1.0 - c ) * ( 1.0 - std::exp( -k * TimeStepSys ) ) - I0 * ( 1.0 - c ) * ( k * TimeStepSys - 1.0 + std::exp( -k * TimeStepSys ) ) / k;
				ElecStorage( ElecStorNum ).ThisTimeStepAvailable = max( 0.0, newAvailable );
				ElecStorage( ElecStorNum ).ThisTimeStepBound = max( 0.0, newBound );
			}

			Pactual = I0 * Volt;
			TotalSOC = ElecStorage( ElecStorNum ).ThisTimeStepAvailable + ElecStorage( ElecStorNum ).ThisTimeStepBound;

			//output1
			if ( TotalSOC > q0 ) {
				ElecStorage( ElecStorNum ).StorageMode = 2;
				ElecStorage( ElecStorNum ).StoredPower = Volt * I0 * Numbattery;
				ElecStorage( ElecStorNum ).StoredEnergy = Volt * I0 * Numbattery * TimeStepSys * SecInHour;
				ElecStorage( ElecStorNum ).DecrementedEnergyStored = -1.0 * ElecStorage( ElecStorNum ).StoredEnergy;
				ElecStorage( ElecStorNum ).DrawnPower = 0.0;
				ElecStorage( ElecStorNum ).DrawnEnergy = 0.0;

			} else if ( TotalSOC < q0 ) {
				ElecStorage( ElecStorNum ).StorageMode = 1;
				ElecStorage( ElecStorNum ).StoredPower = 0.0;
				ElecStorage( ElecStorNum ).StoredEnergy = 0.0;
				ElecStorage( ElecStorNum ).DecrementedEnergyStored = 0.0;
				ElecStorage( ElecStorNum ).DrawnPower = Volt * I0 * Numbattery;
				ElecStorage( ElecStorNum ).DrawnEnergy = Volt * I0 * Numbattery * TimeStepSys * SecInHour;

			} else {
				ElecStorage( ElecStorNum ).StorageMode = 0;
				ElecStorage( ElecStorNum ).StoredPower = 0.0;
				ElecStorage( ElecStorNum ).StoredEnergy = 0.0;
				ElecStorage( ElecStorNum ).DecrementedEnergyStored = 0.0;
				ElecStorage( ElecStorNum ).DrawnPower = 0.0;
				ElecStorage( ElecStorNum ).DrawnEnergy = 0.0;
			}

			ElecStorage( ElecStorNum ).AbsoluteSOC = TotalSOC * Numbattery;
			ElecStorage( ElecStorNum ).FractionSOC = TotalSOC / qmax;
			ElecStorage( ElecStorNum ).BatteryCurrent = I0 * Numpar;
			ElecStorage( ElecStorNum ).BatteryVoltage = Volt * Numser;
			ElecStorage( ElecStorNum ).ThermLossRate = InternalR * pow_2( I0 ) * Numbattery;
			ElecStorage( ElecStorNum ).ThermLossEnergy = InternalR * pow_2( I0 ) * TimeStepSys * SecInHour * Numbattery;

			if ( ElecStorage( ElecStorNum ).ZoneNum > 0 ) { // set values for zone heat gains
				ElecStorage( ElecStorNum ).QdotConvZone = ( ( 1.0 - ElecStorage( ElecStorNum ).ZoneRadFract ) * ElecStorage( ElecStorNum ).ThermLossRate ) * Numbattery;
				ElecStorage( ElecStorNum ).QdotRadZone = ( ( ElecStorage( ElecStorNum ).ZoneRadFract ) * ElecStorage( ElecStorNum ).ThermLossRate ) * Numbattery;
			}

			StorageStoredPower = ElecStorage( ElecStorNum ).StoredPower;
			StorageDrawnPower = ElecStorage( ElecStorNum ).DrawnPower;

		} // Outputs for kibam model

	}

	//*****************************************************************************************************************

	bool
	determineCurrentForBatteryDischarge(
		Real64& curI0,
		Real64& curT0,
		Real64& curVolt,
		Real64 const Pw,
		Real64 const q0,
		int const CurveNum,
		Real64 const k,
		Real64 const c,
		Real64 const qmax,
		Real64 const E0c,
		Real64 const InternalR
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June-August 2008
		//       MODIFIED       BG May 2009, added EMS
		//                      BN (FSEC) Feb 2010 (pass out two storage values)
		//                      Y. KyungTae & W. Wang July-August, 2011 Added a battery model
		//       RE-ENGINEERED  Jason Glazer, GARD Analytics, February 2015, refactor charge calculation into a function

		// PURPOSE OF THIS FUNCTION:
		// Calculate the current for battery discharge in a separate function so that it could be called from the unit tests

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using CurveManager::CurveValue;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		curI0 = 10.0; // Initial assumption
		curT0 = qmax / curI0; // Initial Assumption
		Real64 qmaxf = qmax * k * c * curT0 / ( 1.0 - std::exp( -k * curT0 ) + c * ( k * curT0 - 1.0 + std::exp( -k * curT0 ) ) ); //Initial calculation of a function qmax(I)
		Real64 Xf = ( qmax - q0 ) / qmaxf;
		Real64 Ef = E0c + CurveValue( CurveNum, Xf ); //E0d+Ac*Xf+Cc*X/(Dc-Xf)
		curVolt = Ef - curI0 * InternalR;
		Real64 Inew = Pw / curVolt;
		Real64 Tnew = qmaxf / Inew;
		Real64 error = 1.0;
		int countForIteration = 0;
		bool exceedIterationLimit = false;

		while ( error > 0.0001 ) { // Iteration process to get converged current(I)
			curI0 = Inew;
			curT0 = Tnew;
			qmaxf = qmax * k * c * curT0 / ( 1.0 - std::exp( -k * curT0 ) + c * ( k * curT0 - 1.0 + std::exp( -k * curT0 ) ) );
			Xf = ( qmax - q0 ) / qmaxf;
			Ef = E0c + CurveValue( CurveNum, Xf ); //E0c+Ad*Xf+Cd*X/(Dd-Xf)
			curVolt = Ef - curI0 * InternalR;
			Inew = Pw / curVolt;
			Tnew = qmaxf / Inew;
			error = std::abs( Inew - curI0 );
			++countForIteration;
			if ( countForIteration > 1000 ) {
				exceedIterationLimit = true;
				break;
			}
		}
		return (!exceedIterationLimit);
	}


	void
	FigureElectricalStorageZoneGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Couple electrical storage skin losses to zone heat gains

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;

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
		static bool MyEnvrnFlag( true );

		if ( NumElecStorageDevices == 0 ) return;

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			ElecStorage.QdotConvZone() = 0.0;
			ElecStorage.QdotRadZone() = 0.0;
			MyEnvrnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		//  IF(DoingSizing)THEN

		//  ENDIF

	}

	void
	ManageTransformers()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Weimin Wang
		//       DATE WRITTEN   June 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// manage transformer models

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using DataGlobals::MetersHaveBeenInitialized;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataHeatBalance::ZnAirRpt;
		using InputProcessor::SameString;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const AmbTempRef( 20.0 ); // reference ambient temperature (C)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int TransfNum; // transformer number counter
		int MeterNum; // counter for the meters wired to a transformer
		int MeterPtr; // pointer to the meters wired to a transformer
		int LCNum; // counter for the load centers served by a transformer
		int ZoneNum; // pointer to the zone where transformer is located

		Real64 FactorTempCorr; // temperature correction factor

		Real64 ResRef; // winding resistance at reference temperature (full load)
		Real64 ResSpecified; // winding resistance at specified temperature (specified load)
		Real64 ResRatio; // ratio of winding resistance = ResSpecified/ResRef
		Real64 TempChange; // winding temperature rise (C)
		Real64 AmbTemp; // ambient temperature (C)
		Real64 Capacity; // transformer nameplate capacity(VA)
		Real64 PUL; // per unit load
		Real64 SurplusPower; // surplus power for an electric load center
		Real64 ElecLoad; // transformer load which may be power in or out depending on the usage mode
		Real64 PastElecLoad; // transformer load at the previous timestep
		Real64 TotalLossRate; // the sum of no load loss rate and load loss rate

		Real64 Numerator; // intermediate variable for numerator
		Real64 Denominator; // intermediate variable for denominator

		static bool MyOneTimeFlag( true );

		if ( NumTransformers <= 0 ) return;

		if ( MyOneTimeFlag ) {
			// calculate rated no load losses and rated load losses if the performance input method is based on
			// nominal efficiency. This calculation is done only once
			for ( TransfNum = 1; TransfNum <= NumTransformers; ++TransfNum ) {
				if ( Transformer( TransfNum ).PerformanceInputMode == EfficiencyMethod ) {
					ResRef = Transformer( TransfNum ).FactorTempCoeff + Transformer( TransfNum ).TempRise + AmbTempRef;
					ResSpecified = Transformer( TransfNum ).FactorTempCoeff + Transformer( TransfNum ).RatedTemp;
					ResRatio = ResSpecified / ResRef;
					FactorTempCorr = ( 1.0 - Transformer( TransfNum ).EddyFrac ) * ResRatio + Transformer( TransfNum ).EddyFrac * ( 1.0 / ResRatio );

					Capacity = Transformer( TransfNum ).RatedCapacity;
					Numerator = Capacity * Transformer( TransfNum ).RatedPUL * ( 1.0 - Transformer( TransfNum ).RatedEfficiency );
					Denominator = Transformer( TransfNum ).RatedEfficiency * ( 1.0 + pow_2( Transformer( TransfNum ).RatedPUL / Transformer( TransfNum ).MaxPUL ) );

					Transformer( TransfNum ).RatedNL = Numerator / Denominator;
					Transformer( TransfNum ).RatedLL = Transformer( TransfNum ).RatedNL / ( FactorTempCorr * pow_2( Transformer( TransfNum ).MaxPUL ) );

				}
			}

			MyOneTimeFlag = false;

		}

		for ( TransfNum = 1; TransfNum <= NumTransformers; ++TransfNum ) {
			ElecLoad = 0.0;
			PastElecLoad = 0.0;

			if ( Transformer( TransfNum ).UsageMode == PowerInFromGrid ) {

				for ( MeterNum = 1; MeterNum <= isize( Transformer( TransfNum ).WiredMeterNames ); ++MeterNum ) {

					if ( MetersHaveBeenInitialized ) {
						MeterPtr = Transformer( TransfNum ).WiredMeterPtrs( MeterNum );
						ElecLoad += GetInstantMeterValue( MeterPtr, 1 ) / TimeStepZoneSec + GetInstantMeterValue( MeterPtr, 2 ) / ( TimeStepSys * SecInHour );
						// PastElecLoad store the metered value in the previous time step. This value will be used to check whether
						// a transformer is overloaded or not.
						PastElecLoad += GetCurrentMeterValue( MeterPtr ) / TimeStepZoneSec;
					} else {
						ElecLoad = 0.0;
						PastElecLoad = 0.0;
					}

					// Because transformer loss has been accounted for by Electricity:Facility and Electricity:HVAC, the transformer
					// loss needs to be deducted from the metered value. Otherwise, double counting (circular relationship) occurs.
					if ( Transformer( TransfNum ).SpecialMeter( MeterNum ) ) {
						ElecLoad = ElecLoad - Transformer( TransfNum ).LoadLossRate - Transformer( TransfNum ).NoLoadLossRate;

						if ( ElecLoad < 0 ) ElecLoad = 0.0; //Essential check.
					}

				}

				Transformer( TransfNum ).PowerOut = ElecLoad; //the metered value is transformer's output in PowerInFromGrid mode
			} else { //Usage mode is PowerOutFromBldg
				for ( LCNum = 1; LCNum <= Transformer( TransfNum ).LoadCenterNum; ++LCNum ) {
					SurplusPower = ElecLoadCenter( LCNum ).ElectProdRate - ElecLoadCenter( LCNum ).ElectDemand;

					if ( SurplusPower < 0 ) SurplusPower = 0.0;

					ElecLoad += SurplusPower;
					PastElecLoad = ElecLoad;
				}

				Transformer( TransfNum ).PowerIn = ElecLoad; //surplus power is transformer's input in PowerOutFromBldg mode
			}

			// check availability schedule
			if ( GetCurrentScheduleValue( Transformer( TransfNum ).AvailSchedPtr ) > 0.0 ) {
				Capacity = Transformer( TransfNum ).RatedCapacity;
				PUL = ElecLoad / Capacity;

				if ( PUL > 1.0 ) {
					PUL = 1.0;
				}

				//Originally, PUL was used to check whether a transformer is overloaded (PUL > 1.0 or not). However, it was
				//found that ElecLoad obtained from GetInstantMeterVlaue() might refer to intermideiate values before
				//convergence. The intermediate values may issue false warning. This the reason why PastElecLoad obtained
				//by GetCurrentMeterValue() is used here to check overload issue.
				if ( ( PastElecLoad / Capacity ) > 1.0 ) {
					if ( Transformer( TransfNum ).OverloadErrorIndex == 0 ) {
						ShowSevereError( "Transformer Overloaded" );
						ShowContinueError( "Entered in ElectricLoadCenter:Transformer =" + Transformer( TransfNum ).Name );
					}
					ShowRecurringSevereErrorAtEnd( "Transformer Overloaded: Entered in ElectricLoadCenter:Transformer =" + Transformer( TransfNum ).Name, Transformer( TransfNum ).OverloadErrorIndex );
				}

				TempChange = std::pow( PUL, 1.6 ) * Transformer( TransfNum ).TempRise;

				if ( Transformer( TransfNum ).HeatLossesDestination == ZoneGains ) {
					ZoneNum = Transformer( TransfNum ).ZoneNum;
					AmbTemp = ZnAirRpt( ZoneNum ).MeanAirTemp;
				} else {
					AmbTemp = 20.0;
				}

				ResRef = Transformer( TransfNum ).FactorTempCoeff + Transformer( TransfNum ).TempRise + AmbTempRef;
				ResSpecified = Transformer( TransfNum ).FactorTempCoeff + TempChange + AmbTemp;
				ResRatio = ResSpecified / ResRef;
				FactorTempCorr = ( 1.0 - Transformer( TransfNum ).EddyFrac ) * ResRatio + Transformer( TransfNum ).EddyFrac * ( 1.0 / ResRatio );

				Transformer( TransfNum ).LoadLossRate = Transformer( TransfNum ).RatedLL * pow_2( PUL ) * FactorTempCorr;
				Transformer( TransfNum ).NoLoadLossRate = Transformer( TransfNum ).RatedNL;
			} else { //Transformer is not available.
				Transformer( TransfNum ).LoadLossRate = 0.0;
				Transformer( TransfNum ).NoLoadLossRate = 0.0;
			}

			TotalLossRate = Transformer( TransfNum ).LoadLossRate + Transformer( TransfNum ).NoLoadLossRate;

			if ( Transformer( TransfNum ).UsageMode == PowerInFromGrid ) {
				Transformer( TransfNum ).PowerIn = ElecLoad + TotalLossRate;

				//Transformer losses are wired to the meter via the variable "%ElecUseUtility" only if transformer losses
				//are considered in utility cost. If transformer losses are not considered in utility cost, 0 is assigned
				//to the variable "%ElecUseUtility".
				if ( Transformer( TransfNum ).ConsiderLosses ) {
					Transformer( TransfNum ).ElecUseUtility = TotalLossRate * TimeStepSys * SecInHour;
				} else {
					Transformer( TransfNum ).ElecUseUtility = 0.0;
				}

				//Transformer has two modes.If it works in one mode, the variable for meter output in the other mode
				//is assigned 0
				Transformer( TransfNum ).ElecProducedCoGen = 0.0;
			} else { //Usage mode is PowerOutFromBldg
				Transformer( TransfNum ).PowerOut = ElecLoad - TotalLossRate;

				if ( Transformer( TransfNum ).PowerOut < 0 ) Transformer( TransfNum ).PowerOut = 0.0;

				Transformer( TransfNum ).ElecProducedCoGen = -1.0 * TotalLossRate * TimeStepSys * SecInHour;

				//Transformer has two modes.If it works in one mode, the variable for meter output in the other mode
				//is assigned 0
				Transformer( TransfNum ).ElecUseUtility = 0.0;
			}

			if ( Transformer( TransfNum ).PowerIn <= 0 ) {
				Transformer( TransfNum ).Efficiency = 0.0;
			} else {
				Transformer( TransfNum ).Efficiency = Transformer( TransfNum ).PowerOut / Transformer( TransfNum ).PowerIn;
			}

			Transformer( TransfNum ).NoLoadLossEnergy = Transformer( TransfNum ).NoLoadLossRate * TimeStepSys * SecInHour;
			Transformer( TransfNum ).LoadLossEnergy = Transformer( TransfNum ).LoadLossRate * TimeStepSys * SecInHour;

			Transformer( TransfNum ).EnergyIn = Transformer( TransfNum ).PowerIn * TimeStepSys * SecInHour;
			Transformer( TransfNum ).EnergyOut = Transformer( TransfNum ).PowerOut * TimeStepSys * SecInHour;

			//   Thermal loss rate may not be equal to Total loss rate. This is the case when surplus power is less than the
			//    calculated total loss rate for a cogeneration transformer. That is why "PowerIn - PowerOut" is used below.
			Transformer( TransfNum ).ThermalLossRate = Transformer( TransfNum ).PowerIn - Transformer( TransfNum ).PowerOut;
			Transformer( TransfNum ).ThermalLossEnergy = TotalLossRate * TimeStepSys * SecInHour;

			if ( Transformer( TransfNum ).ZoneNum > 0 ) { // set values for zone heat gains
				Transformer( TransfNum ).QdotConvZone = ( 1.0 - Transformer( TransfNum ).ZoneRadFrac ) * Transformer( TransfNum ).ThermalLossRate;
				Transformer( TransfNum ).QdotRadZone = ( Transformer( TransfNum ).ZoneRadFrac ) * Transformer( TransfNum ).ThermalLossRate;
			}
		} // End TransfNum Loop

	}

	void
	FigureTransformerZoneGains()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Weimin Wang
		//       DATE WRITTEN   July 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Couple transformer losses to zone heat gains

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;

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
		static bool MyEnvrnFlag( true );

		if ( NumTransformers == 0 ) return;

		if ( BeginEnvrnFlag && MyEnvrnFlag ) {
			Transformer.QdotConvZone() = 0.0;
			Transformer.QdotRadZone() = 0.0;
			MyEnvrnFlag = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		//  IF(DoingSizing)THEN

		//  ENDIF

	}

	void
	Rainflow(
		int const numbin, // numbin = constant value
		Real64 const input, // input = input value from other object (battery model)
		Array1A< Real64 > B1, // stores values of points, calculated here - stored for next timestep
		Array1A< Real64 > X, // stores values of two data point difference, calculated here - stored for next timestep
		int & count, // calculated here - stored for next timestep in main loop
		Array1A< Real64 > Nmb, // calculated here - stored for next timestep in main loop
		Array1A< Real64 > OneNmb, // calculated here - stored for next timestep in main loop
		int const dim // end dimension of array
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Y. KyungTae & W. Wang
		//       DATE WRITTEN   July-August, 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Rainflow cycle counting for battery life calculation

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// Ariduru S. 2004. Fatigue life calculation by rainflow cycle counting method.
		//                  Master Thesis, Middle East Technical University.

		// Argument array dimensioning
		B1.dim( {1,dim} );
		X.dim( {1,dim} );
		Nmb.dim( {1,numbin} );
		OneNmb.dim( {1,numbin} );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//Array B1 stores the value of points
		//Array X stores the value of two data points' difference.

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int num;

		X( count ) = input - B1( count - 1 ); // calculate the difference between two data (current and previous)

		// Get rid of the data if it is not peak nor valley
		// The value of count means the number of peak or valley points added to the arrary B10/B1, not including the
		// first point B10(0)/B1(0). Therefore, even if count =2, B1(count-2) is still valid.
		if ( count >= 3 ) {
			//  The following check on peak or valley may be not necessary in most times because the same check is made in the
			//  upper-level subroutine. However, it does not hurt to leave it here.
			if ( X( count ) * X( count - 1 ) >= 0 ) {
				X( count - 1 ) = B1( count ) - B1( count - 2 );
				shift( B1, count - 1, count, B1, MaxRainflowArrayBounds + 1 ); // Get rid of (count-1) row in B1
				shift( X, count, count, X, MaxRainflowArrayBounds + 1 );
				--count; // If the value keep increasing or decreasing, get rid of the middle point.
			} // Only valley and peak will be stored in the matrix, B1

			if ( ( count == 3 ) && ( std::abs( X( 2 ) ) <= std::abs( X( 3 ) ) ) ) {
				//  This means the starting point is included in X(2), a half cycle is counted according to the rain flow
				//  algorithm specified in the reference (Ariduru S. 2004)
				num = nint( ( std::abs( X( 2 ) ) * numbin * 10 + 5 ) / 10 ); // Count half cycle
				Nmb( num ) += 0.5;
				B1 = eoshift( B1, 1 ); // Once counting a half cycle, get rid of the value.
				X = eoshift( X, 1 );
				--count; // The number of matrix, B1 and X1 decrease.
			}
		} // Counting cyle end
		//*** Note: The value of "count" changes in the upper "IF LOOP"

		if ( count >= 4 ) { //count 1 cycle
			while ( std::abs( X( count ) ) > std::abs( X( count - 1 ) ) ) {
				//  This means that the starting point is not included in X(count-1). a cycle is counted according to the rain flow
				//  algorithm specified in the reference (Ariduru S. 2004)
				num = nint( ( std::abs( X( count - 1 ) ) * numbin * 10 + 5 ) / 10 );
				++Nmb( num );

				//     X(count-2) = ABS(X(count))-ABS(X(count-1))+ABS(X(count-2))
				X( count - 2 ) = B1( count ) - B1( count - 3 ); // Updating X needs to be done before shift operation below

				shift( B1, count - 1, count, B1, MaxRainflowArrayBounds + 1 ); // Get rid of two data points one by one
				shift( B1, count - 2, count, B1, MaxRainflowArrayBounds + 1 ); // Delete one point

				shift( X, count, count, X, MaxRainflowArrayBounds ); // Get rid of two data points one by one
				shift( X, count - 1, count, X, MaxRainflowArrayBounds ); // Delete one point

				count -= 2; // If one cycle is counted, two data points are deleted.
				if ( count < 4 ) break; // When only three data points exists, one cycle cannot be counted.
			}
		}

		++count;

		// Check the rest of the half cycles every time step
		OneNmb = Nmb; // Array Nmb (Bins) will be used for the next time step later.
		// OneNmb is used to show the current output only.
		// Ideally, the following clean-up counting is needed at the last system time step in each simulation environemnt.
		// Because of the difficulty in knowing the above information, the clean-up counting is skipped. Skipping this has
		// little impact on the simulation results.
		//   DO k = 1, count-1
		//     num = NINT((ABS(X(k))*numbin*10+5)/10) !Bin number
		//     OneNmb(num) = OneNmb(num)+0.5d0
		//   ENDDO
	}

	void
	shift(
		Array1A< Real64 > A,
		int const m,
		int const n,
		Array1A< Real64 > B,
		int const dim // end dimension of arrays
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Y. KyungTae & W. Wang
		//       DATE WRITTEN   July-August, 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Utility subroutine for rainflow cycle counting

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		A.dim( {1,dim} );
		B.dim( {1,dim} );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ShiftNum; // Loop variable

		for ( ShiftNum = 1; ShiftNum <= m - 1; ++ShiftNum ) {
			B( ShiftNum ) = A( ShiftNum );
		}

		for ( ShiftNum = m; ShiftNum <= n; ++ShiftNum ) {
			B( ShiftNum ) = A( ShiftNum + 1 );
		}
	}

	//******************************************************************************************************
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // ManageElectricPower

} // EnergyPlus
