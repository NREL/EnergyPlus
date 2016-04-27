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

// EnergyPlus Headers
#include <DataSizing.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataSizing {

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   December 2000
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module contains type definitions and variables
	// associated with HVAC system design flow rates, temperatures and
	// capacities. This data is available to the HVAC component modules
	// for their self sizing calculations.

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	// parameters for outside air flow method
	int const NumOAFlowMethods( 6 );

	int const OAFlowNone( 0 );
	int const OAFlowPPer( 1 );
	int const OAFlow( 2 );
	int const OAFlowPerArea( 3 );
	int const OAFlowACH( 4 );
	int const OAFlowSum( 5 );
	int const OAFlowMax( 6 );

	Array1D_string const cOAFlowMethodTypes( NumOAFlowMethods, { "Flow/Person", "Flow/Zone", "Flow/Area", "AirChanges/Hour", "Sum", "Maximum" } );

	// parameters for outside air
	int const AllOA( 1 );
	int const MinOA( 2 );

	// parameters for loop fluid type
	int const HeatingLoop( 1 );
	int const CoolingLoop( 2 );
	int const CondenserLoop( 3 );
	int const SteamLoop( 4 );

	// paramters for sizing
	int const NonCoincident( 1 );
	int const Coincident( 2 );

	// parameters for Cooling Peak Load TYpe
	int const SensibleCoolingLoad( 1 );
	int const TotalCoolingLoad( 2 );

	// parameters for Central Cooling Capacity Control Method
	int const VAV( 1 );
	int const Bypass( 2 );
	int const VT( 3 );
	int const OnOff( 4 );

	// paramters for supply air flow rate method
	int const SupplyAirTemperature( 1 );
	int const TemperatureDifference( 2 );

	// paramters for sizing
	int const FromDDCalc( 1 );
	int const InpDesAirFlow( 2 );
	int const DesAirFlowWithLim( 3 );

	int const DOANeutralSup( 1 );
	int const DOANeutralDehumSup( 2 );
	int const DOACoolSup( 3 );

	// parameters for Type of Load to Size On
	int const Sensible( 0 );
	int const Latent( 1 );
	int const Total( 2 );
	int const Ventilation( 3 );

	// parameter for autosize
	Real64 const AutoSize( -99999.0 );

	// parameter for (time-of-peak) sizing format
	gio::Fmt PeakHrMinFmt( "(I2.2,':',I2.2,':00')" );

	//Zone Outdoor Air Method
	int const ZOAM_FlowPerPerson( 1 ); // set the outdoor air flow rate based on number of people in the zone
	int const ZOAM_FlowPerZone( 2 ); // sum the outdoor air flow rate per zone based on user input
	int const ZOAM_FlowPerArea( 3 ); // sum the outdoor air flow rate based on zone area
	int const ZOAM_FlowPerACH( 4 ); // sum the outdoor air flow rate based on number of air changes for the zone
	int const ZOAM_Sum( 5 ); // sum the outdoor air flow rate of the people component and the space floor area component
	int const ZOAM_Max( 6 ); // use the maximum of the outdoor air flow rate of the people component and the space floor area component

	//System Outdoor Air Method
	int const SOAM_ZoneSum( 1 ); // Sum the outdoor air flow rates of all zones
	int const SOAM_VRP( 2 ); // Use ASHRAE Standard 62.1-2007 to calculate the system level outdoor air flow rates
	//  considering the zone air distribution effectiveness and the system ventilation efficiency
	int const SOAM_IAQP( 3 ); // Use ASHRAE Standard 62.1-2007 IAQP to calculate the system level outdoor air flow rates
	// based on the CO2 setpoint
	int const SOAM_ProportionalControlSchOcc( 4 ); // Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
	// to calculate the system level outdoor air flow rates based on scheduled occupancy
	int const SOAM_IAQPGC( 5 ); // Use ASHRAE Standard 62.1-2004 IAQP to calculate the system level outdoor air flow rates
	// based on the generic contaminant setpoint
	int const SOAM_IAQPCOM( 6 ); // Take the maximum outdoor air rate from both CO2 and generic contaminant controls
	// based on the generic contaminant setpoint
	int const SOAM_ProportionalControlDesOcc( 7 ); // Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
	// to calculate the system level outdoor air flow rates based on design occupancy

	// Zone HVAC Equipment Supply Air Sizing Option
	int const None( 1 );
	int const SupplyAirFlowRate( 2 );
	int const FlowPerFloorArea( 3 );
	int const FractionOfAutosizedCoolingAirflow( 4 );
	int const FractionOfAutosizedHeatingAirflow( 5 );
	int const FlowPerCoolingCapacity( 6 );
	int const FlowPerHeatingCapacity( 7 );

	int const CoolingDesignCapacity( 8 );
	int const HeatingDesignCapacity( 9 );
	int const CapacityPerFloorArea( 10 );
	int const FractionOfAutosizedCoolingCapacity( 11 );
	int const FractionOfAutosizedHeatingCapacity( 12 );

	int const NoSizingFactorMode( 101 );
	int const GlobalHeatingSizingFactorMode( 102 );
	int const GlobalCoolingSizingFactorMode( 103 );
	int const LoopComponentSizingFactorMode( 104 );


	// DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	//  days; includes effects of user multiplier
	//  and user set flows)
	//  of user input multiplier and flows
	//  all design days, calculated only)
	//  using user input system flow rates.
	//  before applying user input sys flow rates.

	int NumOARequirements( 0 ); // Number of OA Requirements objects
	int NumZoneAirDistribution( 0 ); // Number of zone air distribution objects
	int NumZoneSizingInput( 0 ); // Number of Zone Sizing objects
	int NumSysSizInput( 0 ); // Number of System Sizing objects
	int NumPltSizInput( 0 ); // Number of Plant Sizing objects
	int CurSysNum( 0 ); // Current Air System index (0 if not in air loop)
	int CurOASysNum( 0 ); // Current outside air system index (0 if not in OA Sys)
	int CurZoneEqNum( 0 ); // Current Zone Equipment index (0 if not simulating ZoneEq)
	int CurBranchNum( 0 ); // Index of branch being simulated (or 0 if not air loop)
	int CurDuctType( 0 ); // Duct type of current branch
	int CurLoopNum( 0 ); // the current plant loop index
	int CurCondLoopNum( 0 ); // the current condenser loop number
	int CurEnvirNumSimDay( 0 ); // current environment number for day simulated
	int CurOverallSimDay( 0 ); // current day of simulation
	int NumTimeStepsInAvg( 0 ); // number of time steps in the averaging window for the design flow and load sequences
	int SaveNumPlantComps( 0 ); // Number of components using water as an energy source or sink (e.g. water coils)
	int DataTotCapCurveIndex( 0 ); // index to total capacity as a function of temperature curve
	Real64 DataTotCapCurveValue( 0 ); // value of total capacity as a function of temperature curve for CoilVRF_FluidTCtrl_*
	int DataPltSizCoolNum( 0 ); // index to cooling plant sizing data
	int DataPltSizHeatNum( 0 ); // index to heating plant sizing data
	int DataWaterLoopNum( 0 ); // index to plant water loop
	int DataCoilNum( 0 ); // index to coil object
	int DataFanOpMode( 0 ); // fan operating mode (ContFanCycCoil or CycFanCycCoil)
	bool DataCoilIsSuppHeater( false ); // TRUE if heating coil used as supplemental heater
	bool DataIsDXCoil( false ); // TRUE if direct-expansion coil
	bool DataAutosizable( true ); // TRUE if component is autosizable
	bool DataEMSOverrideON( false ); // boolean determines if user relies on EMS to override autosizing
	bool DataScalableSizingON( false ); // boolean determines scalable flow sizing is specified
	bool DataScalableCapSizingON( false ); // boolean determines scalable capacity sizing is specified
	bool DataSysScalableFlowSizingON( false ); // boolean determines scalable system flow sizing is specified
	bool DataSysScalableCapSizingON( false ); // boolean determines scalable system capacity sizing is specified
	bool SysSizingRunDone( false ); // True if a system sizing run is successfully completed.
	bool TermUnitSingDuct( false ); // TRUE if a non-induction single duct terminal unit
	bool TermUnitPIU( false ); // TRUE if a powered induction terminal unit
	bool TermUnitIU( false ); // TRUE if an unpowered induction terminal unit
	bool ZoneEqFanCoil( false ); // TRUE if a 4 pipe fan coil unit is being simulated
	bool ZoneEqUnitHeater( false ); // TRUE if a unit heater is being simulated
	bool ZoneEqUnitVent( false ); // TRUE if a unit ventilator unit is being simulated
	bool ZoneEqVentedSlab( false ); // TRUE if a ventilated slab is being simulated
	bool ZoneEqDXCoil( false ); // TRUE if a ZoneHVAC DX coil is being simulated
	bool ZoneCoolingOnlyFan( false ); // TRUE if a ZoneHVAC DX cooling coil is only coil in parent
	bool ZoneHeatingOnlyFan( false ); // TRUE if zone unit only does heating and contains a fam (such as Unit Heater)
	bool ZoneSizingRunDone( false ); // True if a zone sizing run has been successfully completed.
	bool DataErrorsFound( false ); // used for simulation termination when errors are found
	Real64 AutoVsHardSizingThreshold( 0.1 ); // criteria threshold used to determine if user hard size and autosize disagree 10%
	Real64 AutoVsHardSizingDeltaTempThreshold( 1.5 ); // temperature criteria threshold for autosize versus hard size [C]
	Real64 DataDesInletWaterTemp( 0.0 ); // coil inlet water temperture used for warning messages
	Real64 DataDesInletAirHumRat( 0.0 ); // coil inlet air humidity ratio used for warning messages
	Real64 DataDesInletAirTemp( 0.0 ); // coil inlet air temperature used for warning messages
	Real64 DataDesOutletAirTemp( 0.0 ); // coil outlet air temperature used for sizing
	Real64 DataDesOutletAirHumRat( 0.0 ); // coil air outlet humidity ratio used in sizing calculations [kg water / kg dry air]
	Real64 DataCoolCoilCap( 0.0 ); // cooling coil capacity used for sizing with scalable inputs [W]
	Real64 DataFlowUsedForSizing( 0.0 ); // air flow rate used for sizing with scalable inputs [m3/s]
	Real64 DataAirFlowUsedForSizing( 0.0 ); // air flow rate used for sizing with scalable inputs [m3/s]
	Real64 DataWaterFlowUsedForSizing( 0.0 ); // water flow rate used for sizing with scalable inputs [m3/s]
	Real64 DataCapacityUsedForSizing( 0.0 ); //capacity used for sizing with scalable inputs [W]
	Real64 DataDesignCoilCapacity( 0.0 ); // calculated capacity of coil at end of UA calculation
	Real64 DataHeatSizeRatio( 1.0 ); // heating coil size as a ratio of cooling coil capacity
	Real64 DataEMSOverride( 0.0 ); // value of EMS variable used to override autosizing
	Real64 DataBypassFrac( 0.0 ); // value of bypass fraction for Coil:Cooling:DX:TwoStageWithHumidityControlMode coils
	Real64 DataFracOfAutosizedCoolingAirflow( 1.0 ); // fraction of design cooling supply air flow rate
	Real64 DataFracOfAutosizedHeatingAirflow( 1.0 ); // fraction of design heating supply air flow rate
	Real64 DataFlowPerCoolingCapacity( 0.0 ); // cooling supply air flow per unit cooling capacity
	Real64 DataFlowPerHeatingCapacity( 0.0 ); // heating supply air flow per unit heating capacity
	Real64 DataFracOfAutosizedCoolingCapacity( 1.0 ); // fraction of autosized cooling capacity
	Real64 DataFracOfAutosizedHeatingCapacity( 1.0 ); // fraction of autosized heating capacit
	Real64 DataAutosizedCoolingCapacity( 0.0 ); // Autosized cooling capacity used for multiplying flow per capacity to get flow rate
	Real64 DataAutosizedHeatingCapacity( 0.0 ); // Autosized heating capacit used for multiplying flow per capacity to get flow rate
	Real64 DataConstantUsedForSizing( 0.0 ); // base value used for sizing inputs that are ratios of other inputs
	Real64 DataFractionUsedForSizing( 0.0 ); // fractional value of base value used for sizing inputs that are ratios of other inputs
	Real64 DataNonZoneNonAirloopValue( 0.0 ); // used when equipment is not located in a zone or airloop
	int DataZoneNumber( 0 ); // a pointer to a served by zoneHVAC equipment
	int NumZoneHVACSizing( 0 ); // Number of zone HVAC sizing objects
	Real64 DXCoolCap( 0.0 ); // The ARI cooling capacity of a DX unit.
	Real64 GlobalHeatSizingFactor( 0.0 ); // the global heating sizing ratio
	Real64 GlobalCoolSizingFactor( 0.0 ); // the global cooling sizing ratio
	Real64 SuppHeatCap( 0.0 ); // the heating capacity of the supplemental heater in a unitary system
	Real64 UnitaryHeatCap( 0.0 ); // the heating capacity of a unitary system
	Array1D< Real64 > ZoneSizThermSetPtHi; // highest zone thermostat setpoint during zone sizing calcs
	Array1D< Real64 > ZoneSizThermSetPtLo; // lowest zone thermostat setpoint during zone sizing calcs
	Array1D_string CoolPeakDateHrMin; // date:hr:min of cooling peak
	Array1D_string HeatPeakDateHrMin; // date:hr:min of heating peak
	char SizingFileColSep; // Character to separate columns in sizing outputs
	int DataDesicDehumNum( 0 ); // index to desiccant dehumidifier
	bool DataDesicRegCoil( false ); // TRUE if heating coil desiccant regeneration coil

	// Object Data
	Array1D< OARequirementsData > OARequirements;
	Array1D< ZoneAirDistributionData > ZoneAirDistribution;
	Array1D< ZoneSizingInputData > ZoneSizingInput; // Input data for zone sizing
	Array2D< ZoneSizingData > ZoneSizing; // Data for zone sizing (all data, all design
	Array1D< ZoneSizingData > FinalZoneSizing; // Final data for zone sizing including effects
	Array2D< ZoneSizingData > CalcZoneSizing; // Data for zone sizing (all data,
	Array1D< ZoneSizingData > CalcFinalZoneSizing; // Final data for zone sizing (calculated only)
	Array1D< ZoneSizingData > TermUnitFinalZoneSizing; // Final data for sizing terminal units
	Array1D< SystemSizingInputData > SysSizInput; // Input data array for system sizing object
	Array2D< SystemSizingData > SysSizing; // Data array for system sizing (all data)
	Array1D< SystemSizingData > FinalSysSizing; // Data array for system sizing (max heat/cool)
	Array1D< SystemSizingData > CalcSysSizing; // Data array for system sizing (max heat/cool)
	Array1D< SysSizPeakDDNumData > SysSizPeakDDNum; // data array for peak des day indices
	Array1D< TermUnitSizingData > TermUnitSizing; // Data added in sizing routines
	Array1D< ZoneEqSizingData > ZoneEqSizing; // Data added in zone eq component sizing routines
	Array1D< ZoneEqSizingData > UnitarySysEqSizing; // Data added in unitary system sizing routines
	Array1D< ZoneEqSizingData > OASysEqSizing; // Data added in unitary system sizing routines
	Array1D< PlantSizingData > PlantSizData; // Input data array for plant sizing
	Array1D< DesDayWeathData > DesDayWeath; // design day weather saved at major time step
	Array1D< CompDesWaterFlowData > CompDesWaterFlow; // array to store components' design water flow
	Array1D< ZoneHVACSizingData > ZoneHVACSizing; // Input data for zone HVAC sizing

	// Clears the global data in DataSizing.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumOARequirements = 0;
		NumZoneAirDistribution = 0;
		NumZoneSizingInput = 0;
		NumSysSizInput = 0;
		NumPltSizInput = 0;
		CurSysNum = 0;
		CurOASysNum = 0;
		CurZoneEqNum = 0;
		CurBranchNum = 0;
		CurDuctType = 0;
		CurLoopNum = 0;
		CurCondLoopNum = 0;
		CurEnvirNumSimDay = 0;
		CurOverallSimDay = 0;
		NumTimeStepsInAvg = 0;
		SaveNumPlantComps = 0;
		DataTotCapCurveIndex = 0;
		DataPltSizCoolNum = 0;
		DataPltSizHeatNum = 0;
		DataWaterLoopNum = 0;
		DataCoilNum = 0;
		DataFanOpMode = 0;
		DataCoilIsSuppHeater = false;
		DataIsDXCoil = false;
		DataAutosizable = true;
		DataEMSOverrideON = false;
		DataScalableSizingON = false;
		DataScalableCapSizingON = false;
		DataSysScalableFlowSizingON = false;
		DataSysScalableCapSizingON = false;
		SysSizingRunDone = false;
		TermUnitSingDuct = false;
		TermUnitPIU = false;
		TermUnitIU = false;
		ZoneEqFanCoil = false;
		ZoneEqUnitHeater = false;
		ZoneEqUnitVent = false;
		ZoneEqVentedSlab = false;
		ZoneEqDXCoil = false;
		ZoneCoolingOnlyFan = false;
		ZoneHeatingOnlyFan = false;
		ZoneSizingRunDone = false;
		DataErrorsFound = false;
		AutoVsHardSizingThreshold = 0.1;
		AutoVsHardSizingDeltaTempThreshold = 1.5;
		DataDesInletWaterTemp = 0.0;
		DataDesInletAirHumRat = 0.0;
		DataDesInletAirTemp = 0.0;
		DataDesOutletAirTemp = 0.0;
		DataDesOutletAirHumRat = 0.0;
		DataCoolCoilCap = 0.0;
		DataFlowUsedForSizing = 0.0;
		DataAirFlowUsedForSizing = 0.0;
		DataWaterFlowUsedForSizing = 0.0;
		DataCapacityUsedForSizing = 0.0;
		DataDesignCoilCapacity = 0.0;
		DataHeatSizeRatio = 1.0;
		DataEMSOverride = 0.0;
		DataBypassFrac = 0.0;
		DataFracOfAutosizedCoolingAirflow = 1.0;
		DataFracOfAutosizedHeatingAirflow = 1.0;
		DataFlowPerCoolingCapacity = 0.0;
		DataFlowPerHeatingCapacity = 0.0;
		DataFracOfAutosizedCoolingCapacity = 1.0;
		DataFracOfAutosizedHeatingCapacity = 1.0;
		DataAutosizedCoolingCapacity = 0.0;
		DataAutosizedHeatingCapacity = 0.0;
		DataConstantUsedForSizing = 0.0;
		DataFractionUsedForSizing = 0.0;
		DataNonZoneNonAirloopValue = 0.0;
		DataZoneNumber = 0;
		NumZoneHVACSizing = 0;
		DXCoolCap = 0.0;
		GlobalHeatSizingFactor = 0.0;
		GlobalCoolSizingFactor = 0.0;
		SuppHeatCap = 0.0;
		UnitaryHeatCap = 0.0;
		ZoneSizThermSetPtHi.deallocate();
		ZoneSizThermSetPtLo.deallocate();
		CoolPeakDateHrMin.deallocate();
		HeatPeakDateHrMin.deallocate();
		SizingFileColSep = char();

		OARequirements.deallocate();
		ZoneAirDistribution.deallocate();
		ZoneSizingInput.deallocate();
		ZoneSizing.deallocate();
		FinalZoneSizing.deallocate();
		CalcZoneSizing.deallocate();
		CalcFinalZoneSizing.deallocate();
		TermUnitFinalZoneSizing.deallocate();
		SysSizInput.deallocate();
		SysSizing.deallocate();
		FinalSysSizing.deallocate();
		CalcSysSizing.deallocate();
		SysSizPeakDDNum.deallocate();
		TermUnitSizing.deallocate();
		ZoneEqSizing.deallocate();
		UnitarySysEqSizing.deallocate();
		OASysEqSizing.deallocate();
		PlantSizData.deallocate();
		DesDayWeath.deallocate();
		CompDesWaterFlow.deallocate();
		ZoneHVACSizing.deallocate();
		DataDesicDehumNum = 0;
		DataDesicRegCoil = false;
	}

} // DataSizing

} // EnergyPlus
