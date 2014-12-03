#ifndef DataSizing_hh_INCLUDED
#define DataSizing_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray2D.hh>
#include <ObjexxFCL/gio_Fmt.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataSizing {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	// parameters for outside air flow method
	extern int const NumOAFlowMethods;

	extern int const OAFlowNone;
	extern int const OAFlowPPer;
	extern int const OAFlow;
	extern int const OAFlowPerArea;
	extern int const OAFlowACH;
	extern int const OAFlowSum;
	extern int const OAFlowMax;

	extern FArray1D_string const cOAFlowMethodTypes;

	// parameters for outside air
	extern int const AllOA;
	extern int const MinOA;

	// parameters for loop fluid type
	extern int const HeatingLoop;
	extern int const CoolingLoop;
	extern int const CondenserLoop;
	extern int const SteamLoop;

	// paramters for sizing
	extern int const NonCoincident;
	extern int const Coincident;

	// paramters for supply air flow rate method
	extern int const SupplyAirTemperature;
	extern int const TemperatureDifference;

	// paramters for sizing
	extern int const FromDDCalc;
	extern int const InpDesAirFlow;
	extern int const DesAirFlowWithLim;

	// parameters for Type of Load to Size On
	extern int const Sensible;
	extern int const Latent;
	extern int const Total;
	extern int const Ventilation;

	// parameter for autosize
	extern Real64 const AutoSize;

	// parameter for (time-of-peak) sizing format
	extern gio::Fmt PeakHrMinFmt;

	//Zone Outdoor Air Method
	extern int const ZOAM_FlowPerPerson; // set the outdoor air flow rate based on number of people in the zone
	extern int const ZOAM_FlowPerZone; // sum the outdoor air flow rate per zone based on user input
	extern int const ZOAM_FlowPerArea; // sum the outdoor air flow rate based on zone area
	extern int const ZOAM_FlowPerACH; // sum the outdoor air flow rate based on number of air changes for the zone
	extern int const ZOAM_Sum; // sum the outdoor air flow rate of the people component and the space floor area component
	extern int const ZOAM_Max; // use the maximum of the outdoor air flow rate of the people component and
	// the space floor area component

	//System Outdoor Air Method
	extern int const SOAM_ZoneSum; // Sum the outdoor air flow rates of all zones
	extern int const SOAM_VRP; // Use ASHRAE Standard 62.1-2007 to calculate the system level outdoor air flow rates
	//  considering the zone air distribution effectiveness and the system ventilation efficiency
	extern int const SOAM_IAQP; // Use ASHRAE Standard 62.1-2007 IAQP to calculate the system level outdoor air flow rates
	// based on the CO2 setpoint
	extern int const SOAM_ProportionalControl; // Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
	// to calculate the system level outdoor air flow rates
	extern int const SOAM_IAQPGC; // Use ASHRAE Standard 62.1-2004 IAQP to calculate the system level outdoor air flow rates
	// based on the generic contaminant setpoint
	extern int const SOAM_IAQPCOM; // Take the maximum outdoor air rate from both CO2 and generic contaminant controls
	// based on the generic contaminant setpoint

	// Zone HVAC Equipment Supply Air Sizing Option
	extern int const None;
	extern int const SupplyAirFlowRate;
	extern int const FlowPerFloorArea;
	extern int const FractionOfAutosizedCoolingAirflow;
	extern int const FractionOfAutosizedHeatingAirflow;
	extern int const FlowPerCoolingCapacity;
	extern int const FlowPerHeatingCapacity;
	extern int const CoolingDesignCapacity;
	extern int const HeatingDesignCapacity;
	extern int const CapacityPerFloorArea;
	extern int const FractionOfAutosizedCoolingCapacity;
	extern int const FractionOfAutosizedHeatingCapacity;

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

	extern int NumOARequirements; // Number of OA Requirements objects
	extern int NumZoneAirDistribution; // Number of zone air distribution objects
	extern int NumZoneSizingInput; // Number of Zone Sizing objects
	extern int NumSysSizInput; // Number of System Sizing objects
	extern int NumPltSizInput; // Number of Plant Sizing objects
	extern int CurSysNum; // Current Air System index (0 if not in air loop)
	extern int CurOASysNum; // Current outside air system index (0 if not in OA Sys)
	extern int CurZoneEqNum; // Current Zone Equipment index (0 if not simulating ZoneEq)
	extern int CurBranchNum; // Index of branch being simulated (or 0 if not air loop)
	extern int CurDuctType; // Duct type of current branch
	extern int CurLoopNum; // the current plant loop index
	extern int CurCondLoopNum; // the current condenser loop number
	extern int CurEnvirNumSimDay; // current environment number for day simulated
	extern int CurOverallSimDay; // current day of simulation
	extern int NumTimeStepsInAvg; // number of time steps in the averaging window for the design flow and load sequences
	extern int SaveNumPlantComps; // Number of components using water as an energy source or sink (e.g. water coils)
	extern int DataTotCapCurveIndex; // index to total capacity as a function of temperature curve
	extern int DataPltSizCoolNum; // index to cooling plant sizing data
	extern int DataPltSizHeatNum; // index to heating plant sizing data
	extern int DataWaterLoopNum; // index to plant water loop
	extern int DataCoilNum; // index to coil object
	extern int DataFanOpMode; // fan operating mode (ContFanCycCoil or CycFanCycCoil)
	extern bool DataCoilIsSuppHeater; // TRUE if heating coil used as supplemental heater
	extern bool DataIsDXCoil; // TRUE if direct-expansion coil
	extern bool DataAutosizable; // TRUE if component is autosizable
	extern bool DataEMSOverrideON; // boolean determines if user relies on EMS to override autosizing
	extern bool TermUnitSingDuct; // TRUE if a non-induction single duct terminal unit
	extern bool TermUnitPIU; // TRUE if a powered induction terminal unit
	extern bool TermUnitIU; // TRUE if an unpowered induction terminal unit
	extern bool ZoneEqFanCoil; // TRUE if a 4 pipe fan coil unit is being simulated
	extern bool ZoneEqUnitHeater; // TRUE if a unit heater is being simulated
	extern bool ZoneEqUnitVent; // TRUE if a unit ventilator is being simulated
	extern bool ZoneEqVentedSlab; // TRUE if a ventilated slab is being simulated
	extern bool ZoneEqDXCoil; // TRUE if a ZoneHVAC DX coil is being simulated
	extern bool ZoneCoolingOnlyFan; // TRUE if a ZoneHVAC DX cooling coil is only coil in parent
	extern bool ZoneHeatingOnlyFan; // TRUE if zone unit only does heating and contains a fam (such as Unit Heater)
	extern bool SysSizingRunDone; // True if a system sizing run is successfully completed.
	extern bool ZoneSizingRunDone; // True if a zone sizing run has been successfully completed.
	extern bool DataErrorsFound; // used for simulation termination when errors are found
	extern bool DataAutosizable; // TRUE if component is autosizable
	extern bool DataEMSOverrideON; // boolean determines if user relies on EMS to override autosizing
	extern bool DataScalableSizingON; // boolean determines scalable zone flow sizing is specified
	extern bool DataScalableCapSizingON; // boolean determines scalable zone capacity sizing is specified
	extern bool DataSysScalableFlowSizingON; // boolean determines scalable system flow sizing is specified
	extern bool DataSysScalableCapSizingON; // boolean determines scalable system capacity sizing is specified
	extern Real64 DataDesInletWaterTemp; // coil inlet water temperture used for warning messages
	extern Real64 DataDesInletAirHumRat; // coil inlet air humidity ratio used for warning messages
	extern Real64 DataDesInletAirTemp; // coil inlet air temperature used for warning messages
	extern Real64 DataDesOutletAirTemp; // coil outlet air temperature used for sizing
	extern Real64 DataCoolCoilCap; // cooling coil capacity used for sizing with scalable inputs
	extern Real64 DataFlowUsedForSizing; // air flow rate used for sizing with scalable inputs [m3/s]
	extern Real64 DataAirFlowUsedForSizing; // air flow rate used for sizing with scalable inputs [m3/s]
	extern Real64 DataWaterFlowUsedForSizing; // water flow rate used for sizing with scalable inputs [m3/s]
	extern Real64 DataCapacityUsedForSizing; // capacity used for sizing with scalable inputs [W]
	extern Real64 DataDesignCoilCapacity; // calculated capacity of coil at end of UA calculation
	extern Real64 DataHeatSizeRatio; // heating coil size as a ratio of cooling coil capacity
	extern Real64 DataEMSOverride; // value of EMS variable used to override autosizing
	extern Real64 DataBypassFrac; // value of bypass fraction for Coil:Cooling:DX:TwoStageWithHumidityControlMode coils
	extern Real64 DataFracOfAutosizedCoolingAirflow; // fraction of design cooling supply air flow rate
	extern Real64 DataFracOfAutosizedHeatingAirflow; // fraction of design heating supply air flow rate
	extern Real64 DataFlowPerCoolingCapacity; // cooling supply air flow per unit cooling capacity
	extern Real64 DataFlowPerHeatingCapacity; // heating supply air flow per unit heating capacity
	extern Real64 DataFracOfAutosizedCoolingCapacity; // fraction of autosized cooling capacity
	extern Real64 DataFracOfAutosizedHeatingCapacity; // fraction of autosized heating capacit
	extern Real64 DataAutosizedCoolingCapacity; // Autosized cooling capacity used for multiplying flow per capacity to get flow rate
	extern Real64 DataAutosizedHeatingCapacity; // Autosized heating capacit used for multiplying flow per capacity to get flow rate
	extern Real64 DataConstantUsedForSizing; // base value used for sizing inputs that are ratios of other inputs
	extern Real64 DataFractionUsedForSizing; // fractional value of base value used for sizing inputs that are ratios of other inputs
	extern int DataZoneNumber; // a pointer to a served by zoneHVAC equipment
	extern int NumZoneHVACSizing; // Number of zone HVAC sizing objects
	extern bool TermUnitSingDuct; // TRUE if a non-induction single duct terminal unit
	extern bool TermUnitPIU; // TRUE if a powered induction terminal unit
	extern bool TermUnitIU; // TRUE if an unpowered induction terminal unit
	extern bool ZoneEqFanCoil; // TRUE if a 4 pipe fan coil unit is being simulated
	extern bool ZoneEqUnitVent; // TRUE if a unit ventilator unit is being simulated
	extern bool ZoneEqDXCoil; // TRUE if a ZoneHVAC DX coil is being simulated
	extern bool ZoneCoolingOnlyFan; // TRUE if a ZoneHVAC DX cooling coil is only coil in parent
	extern bool ZoneHeatingOnlyFan; // TRUE if zone unit only does heating and contains a fam (such as Unit Heater)
	extern bool SysSizingRunDone; // True if a system sizing run is successfully completed.
	extern bool ZoneSizingRunDone; // True if a zone sizing run has been successfully completed.
	extern Real64 AutoVsHardSizingThreshold; // criteria threshold used to determine if user hard size and autosize disagree 10%
	extern Real64 AutoVsHardSizingDeltaTempThreshold; // temperature criteria threshold for autosize versus hard size [C]
	extern Real64 DXCoolCap; // The ARI cooling capacity of a DX unit.
	extern Real64 UnitaryHeatCap; // the heating capacity of a unitary system
	extern Real64 SuppHeatCap; // the heating capacity of the supplemental heater in a unitary system
	extern Real64 GlobalHeatSizingFactor; // the global heating sizing ratio
	extern Real64 GlobalCoolSizingFactor; // the global cooling sizing ratio
	extern FArray1D< Real64 > ZoneSizThermSetPtHi; // highest zone thermostat setpoint during zone sizing calcs
	extern FArray1D< Real64 > ZoneSizThermSetPtLo; // lowest zone thermostat setpoint during zone sizing calcs
	extern FArray1D_string CoolPeakDateHrMin; // date:hr:min of cooling peak
	extern FArray1D_string HeatPeakDateHrMin; // date:hr:min of heating peak
	extern char SizingFileColSep; // Character to separate columns in sizing outputs


	// Types

	struct ZoneSizingInputData
	{
		// Members
		std::string ZoneName; // name of a zone
		int ZoneNum; // index of the zone
		int ZnCoolDgnSAMethod; // choice of how to get zone cooling design air temperature;
		//  1 = specify supply air temperature,
		//  2 = calculate from the temperature difference
		int ZnHeatDgnSAMethod; // choice of how to get zone heating design air temperature;
		//  1 = specify supply air temperature,
		//  2 = calculate from the temperature difference
		Real64 CoolDesTemp; // zone design cooling supply air temperature [C]
		Real64 HeatDesTemp; // zone design heating supply air temperature [C]
		Real64 CoolDesTempDiff; // zone design cooling supply air temperature difference [deltaC]
		Real64 HeatDesTempDiff; // zone design heating supply air temperature difference [deltaC]
		Real64 CoolDesHumRat; // zone design cooling supply air humidity ratio [kg-H2O/kg-air]
		Real64 HeatDesHumRat; // zone design heating supply air humidity ratio [kg-H2O/kg-air]
		std::string DesignSpecOAObjName; // name of the design specification outdoor air object
		int OADesMethod; // choice of how to calculate minimum outside air;
		//  1 = m3/s per person; 2 = m3/s per zone; 3 = m3/s per zone area;
		//  4 = sum of flow from 3 OA input fields;
		//  5 = max of flow from 3 OA input fields
		Real64 DesOAFlowPPer; // design outside air flow per person in zone [m3/s]
		Real64 DesOAFlowPerArea; // design outside air flow per zone area [m3/s / m2]
		Real64 DesOAFlow; // design outside air flow for the zone [m3/s]
		int CoolAirDesMethod; // choice of how to get zone cooling design air flow rates;
		//  1 = calc from des day simulation; 2 = m3/s per zone, user input
		//  3 = apply limits to air flow rate from DD calc
		Real64 DesCoolAirFlow; // design zone supply air flow rate [m3/s]
		Real64 DesCoolMinAirFlowPerArea; // design cooling minimum air flow rate per zone area [m3/s / m2]
		Real64 DesCoolMinAirFlow; // design cooling minimum air flow rate [m3/s]
		Real64 DesCoolMinAirFlowFrac; // design cooling minimum air flow rate fraction
		//  (of the cooling design air flow rate)
		int HeatAirDesMethod; // choice of how to get zone heating design air flow rates;
		//  1 = calc from des day simulation; 2 = m3/s per zone, user input
		//  3 = apply limits to air flow rate from DD calc
		Real64 DesHeatAirFlow; // design zone heating supply air flow rate [m3/s]
		Real64 DesHeatMaxAirFlowPerArea; // design heating maximum air flow rate per zone area [m3/s / m2]
		Real64 DesHeatMaxAirFlow; // design heating maximum air flow rate [m3/s]
		Real64 DesHeatMaxAirFlowFrac; // design heating maximum air flow rate fraction
		//  (of the cooling design air flow rate)
		Real64 HeatSizingFactor; // the zone heating sizing ratio
		Real64 CoolSizingFactor; // the zone cooling sizing ratio
		Real64 ZoneADEffCooling;
		Real64 ZoneADEffHeating;
		std::string ZoneAirDistEffObjName; // name of the zone air distribution effectiveness object name
		int ZoneAirDistributionIndex; // index to the zone air distribution object
		int ZoneDesignSpecOAIndex; // index to the zone design spec OA object
		Real64 ZoneSecondaryRecirculation; // the zone secondary air recirculation fraction

		// Default Constructor
		ZoneSizingInputData() :
			ZoneNum( 0 ),
			ZnCoolDgnSAMethod( 0 ),
			ZnHeatDgnSAMethod( 0 ),
			CoolDesTemp( 0.0 ),
			HeatDesTemp( 0.0 ),
			CoolDesTempDiff( 0.0 ),
			HeatDesTempDiff( 0.0 ),
			CoolDesHumRat( 0.0 ),
			HeatDesHumRat( 0.0 ),
			OADesMethod( 0 ),
			DesOAFlowPPer( 0.0 ),
			DesOAFlowPerArea( 0.0 ),
			DesOAFlow( 0.0 ),
			CoolAirDesMethod( 0 ),
			DesCoolAirFlow( 0.0 ),
			DesCoolMinAirFlowPerArea( 0.0 ),
			DesCoolMinAirFlow( 0.0 ),
			DesCoolMinAirFlowFrac( 0.0 ),
			HeatAirDesMethod( 0 ),
			DesHeatAirFlow( 0.0 ),
			DesHeatMaxAirFlowPerArea( 0.0 ),
			DesHeatMaxAirFlow( 0.0 ),
			DesHeatMaxAirFlowFrac( 0.0 ),
			HeatSizingFactor( 0.0 ),
			CoolSizingFactor( 0.0 ),
			ZoneADEffCooling( 1.0 ),
			ZoneADEffHeating( 1.0 ),
			ZoneAirDistributionIndex( 0 ),
			ZoneDesignSpecOAIndex( 0 ),
			ZoneSecondaryRecirculation( 0.0 )
		{}

		// Member Constructor
		ZoneSizingInputData(
			std::string const & ZoneName, // name of a zone
			int const ZoneNum, // index of the zone
			int const ZnCoolDgnSAMethod, // choice of how to get zone cooling design air temperature;
			int const ZnHeatDgnSAMethod, // choice of how to get zone heating design air temperature;
			Real64 const CoolDesTemp, // zone design cooling supply air temperature [C]
			Real64 const HeatDesTemp, // zone design heating supply air temperature [C]
			Real64 const CoolDesTempDiff, // zone design cooling supply air temperature difference [deltaC]
			Real64 const HeatDesTempDiff, // zone design heating supply air temperature difference [deltaC]
			Real64 const CoolDesHumRat, // zone design cooling supply air humidity ratio [kg-H2O/kg-air]
			Real64 const HeatDesHumRat, // zone design heating supply air humidity ratio [kg-H2O/kg-air]
			std::string const & DesignSpecOAObjName, // name of the design specification outdoor air object
			int const OADesMethod, // choice of how to calculate minimum outside air;
			Real64 const DesOAFlowPPer, // design outside air flow per person in zone [m3/s]
			Real64 const DesOAFlowPerArea, // design outside air flow per zone area [m3/s / m2]
			Real64 const DesOAFlow, // design outside air flow for the zone [m3/s]
			int const CoolAirDesMethod, // choice of how to get zone cooling design air flow rates;
			Real64 const DesCoolAirFlow, // design zone supply air flow rate [m3/s]
			Real64 const DesCoolMinAirFlowPerArea, // design cooling minimum air flow rate per zone area [m3/s / m2]
			Real64 const DesCoolMinAirFlow, // design cooling minimum air flow rate [m3/s]
			Real64 const DesCoolMinAirFlowFrac, // design cooling minimum air flow rate fraction
			int const HeatAirDesMethod, // choice of how to get zone heating design air flow rates;
			Real64 const DesHeatAirFlow, // design zone heating supply air flow rate [m3/s]
			Real64 const DesHeatMaxAirFlowPerArea, // design heating maximum air flow rate per zone area [m3/s / m2]
			Real64 const DesHeatMaxAirFlow, // design heating maximum air flow rate [m3/s]
			Real64 const DesHeatMaxAirFlowFrac, // design heating maximum air flow rate fraction
			Real64 const HeatSizingFactor, // the zone heating sizing ratio
			Real64 const CoolSizingFactor, // the zone cooling sizing ratio
			Real64 const ZoneADEffCooling,
			Real64 const ZoneADEffHeating,
			std::string const & ZoneAirDistEffObjName, // name of the zone air distribution effectiveness object name
			int const ZoneAirDistributionIndex, // index to the zone air distribution object
			int const ZoneDesignSpecOAIndex, // index to the zone design spec OA object
			Real64 const ZoneSecondaryRecirculation // the zone secondary air recirculation fraction
		) :
			ZoneName( ZoneName ),
			ZoneNum( ZoneNum ),
			ZnCoolDgnSAMethod( ZnCoolDgnSAMethod ),
			ZnHeatDgnSAMethod( ZnHeatDgnSAMethod ),
			CoolDesTemp( CoolDesTemp ),
			HeatDesTemp( HeatDesTemp ),
			CoolDesTempDiff( CoolDesTempDiff ),
			HeatDesTempDiff( HeatDesTempDiff ),
			CoolDesHumRat( CoolDesHumRat ),
			HeatDesHumRat( HeatDesHumRat ),
			DesignSpecOAObjName( DesignSpecOAObjName ),
			OADesMethod( OADesMethod ),
			DesOAFlowPPer( DesOAFlowPPer ),
			DesOAFlowPerArea( DesOAFlowPerArea ),
			DesOAFlow( DesOAFlow ),
			CoolAirDesMethod( CoolAirDesMethod ),
			DesCoolAirFlow( DesCoolAirFlow ),
			DesCoolMinAirFlowPerArea( DesCoolMinAirFlowPerArea ),
			DesCoolMinAirFlow( DesCoolMinAirFlow ),
			DesCoolMinAirFlowFrac( DesCoolMinAirFlowFrac ),
			HeatAirDesMethod( HeatAirDesMethod ),
			DesHeatAirFlow( DesHeatAirFlow ),
			DesHeatMaxAirFlowPerArea( DesHeatMaxAirFlowPerArea ),
			DesHeatMaxAirFlow( DesHeatMaxAirFlow ),
			DesHeatMaxAirFlowFrac( DesHeatMaxAirFlowFrac ),
			HeatSizingFactor( HeatSizingFactor ),
			CoolSizingFactor( CoolSizingFactor ),
			ZoneADEffCooling( ZoneADEffCooling ),
			ZoneADEffHeating( ZoneADEffHeating ),
			ZoneAirDistEffObjName( ZoneAirDistEffObjName ),
			ZoneAirDistributionIndex( ZoneAirDistributionIndex ),
			ZoneDesignSpecOAIndex( ZoneDesignSpecOAIndex ),
			ZoneSecondaryRecirculation( ZoneSecondaryRecirculation )
		{}

	};

	struct ZoneSizingData
	{
		// Members
		std::string ZoneName; // name of a zone
		std::string CoolDesDay; // name of a cooling design day
		std::string HeatDesDay; // name of a heating design day
		int ZnCoolDgnSAMethod; // choice of how to get zone cooling design air temperature;
		//  1 = specify supply air temperature,
		//  2 = calculate from the temperature difference
		int ZnHeatDgnSAMethod; // choice of how to get zone heating design air temperature;
		//  1 = specify supply air temperature,
		//  2 = calculate from the temperature difference
		Real64 CoolDesTemp; // zone design cooling supply air temperature [C]
		Real64 HeatDesTemp; // zone design heating supply air temperature [C]
		Real64 CoolDesTempDiff; // zone design cooling supply air temperature difference [deltaC]
		Real64 HeatDesTempDiff; // zone design heating supply air temperature difference [deltaC]
		Real64 CoolDesHumRat; // zone design cooling supply air humidity ratio [kg-H2O/kg-air]
		Real64 HeatDesHumRat; // zone design heating supply air humidity ratio [kg-H2O/kg-air]
		int ZoneDesignSpecOAIndex; // index to DesignSpecification:OutdoorAir object
		int OADesMethod; // choice of how to calculate minimum outside air;
		//  1 = m3/s per person; 2 = m3/s per zone; 3 = m3/s per zone area;
		//  4 = sum of flow from 3 OA input fields;
		//  5 = max of flow from 3 OA input fields
		Real64 DesOAFlowPPer; // design outside air flow per person in zone [m3/s]
		Real64 DesOAFlowPerArea; // design outside air flow per zone area [m3/s / m2]
		Real64 DesOAFlow; // design outside air flow for the zone [m3/s]
		int CoolAirDesMethod; // choice of how to get zone cooling design air flow rates;
		//  1 = calc from des day simulation; 2 = m3/s per zone, user input
		//  3 = apply limits to air flow rate from DD calc
		Real64 InpDesCoolAirFlow; // design zone supply air flow rate [m3/s]
		Real64 DesCoolMinAirFlowPerArea; // design cooling minimum air flow rate per zone area [m3/s / m2]
		Real64 DesCoolMinAirFlow; // design cooling minimum air flow rate [m3/s]
		Real64 DesCoolMinAirFlowFrac; // design cooling minimum air flow rate fraction
		//  (of the cooling design air flow rate)
		int HeatAirDesMethod; // choice of how to get zone heating design air flow rates;
		//  1 = calc from des day simulation; 2 = m3/s per zone, user input
		//  3 = apply limits to air flow rate from DD calc
		Real64 InpDesHeatAirFlow; // design zone heating supply air flow rate [m3/s]
		Real64 DesHeatMaxAirFlowPerArea; // design heating maximum air flow rate per zone area [m3/s / m2]
		Real64 DesHeatMaxAirFlow; // design heating maximum air flow rate [m3/s]
		Real64 DesHeatMaxAirFlowFrac; // design heating maximum air flow rate fraction
		//  (of the cooling design air flow rate)
		Real64 HeatSizingFactor; // the zone heating sizing ratio
		Real64 CoolSizingFactor; // the zone cooling sizing ratio
		int ActualZoneNum; // index into the Zone data array (in DataHeatBalance)
		int SupplyAirNode; // node number of supply air node
		Real64 DesHeatMassFlow; // zone design heating air mass flow rate [kg/s]
		Real64 DesHeatOAFlowFrac; // zone design heating OA air volume fraction [-]
		bool EMSOverrideDesHeatMassOn; // true if EMS is acting on this structure
		Real64 EMSValueDesHeatMassFlow; // Value EMS directing to use for Design Heating air mass flow [kg/s]
		Real64 DesCoolMassFlow; // zone design cooling air mass flow rate [kg/s]
		Real64 DesCoolOAFlowFrac; // zone design cooling OA air volume fraction [-]
		bool EMSOverrideDesCoolMassOn; // true if EMS is acting on this structure
		Real64 EMSValueDesCoolMassFlow; // Value EMS directing to use for Design Cooling air mass flow [kg/s]
		Real64 DesHeatLoad; // zone design heating load [W]
		bool EMSOverrideDesHeatLoadOn; // true if EMS is acting on this structure
		Real64 EMSValueDesHeatLoad; // Value EMS directing to use for zone design heating load  [W]
		Real64 DesCoolLoad; // zone design cooling load [W]
		bool EMSOverrideDesCoolLoadOn; // true if EMS is acting on this structure
		Real64 EMSValueDesCoolLoad; // Value EMS directing to use for zone design cooling load  [W]
		Real64 DesHeatDens; // zone design heating air density [kg/m3]
		Real64 DesCoolDens; // zone design cooling air density [kg/m3]
		Real64 DesHeatVolFlow; // zone design heating air volume flow rate [m3/s]
		bool EMSOverrideDesHeatVolOn; // true if EMS is acting on this structure
		Real64 EMSValueDesHeatVolFlow; // Value EMS directing to use for Design Heating air volume flow [m3/s]
		Real64 DesCoolVolFlow; // zone design cooling air volume flow rate [m3/s]
		bool EMSOverrideDesCoolVolOn; // true if EMS is acting on this structure
		Real64 EMSValueDesCoolVolFlow; // Value EMS directing to use for Design cooling air volume flow [m3/s]
		Real64 DesHeatVolFlowMax; // zone design heating maximum air volume flow rate [m3/s]
		Real64 DesCoolVolFlowMin; // zone design cooling minimum air volume flow rate [m3/s]
		Real64 DesHeatCoilInTemp; // zone heating coil design air inlet temperature [C]
		Real64 DesCoolCoilInTemp; // zone cooling coil design air inlet temperature [C]
		Real64 DesHeatCoilInHumRat; // zone heating coil design air inlet humidity ratio [kg/kg]
		Real64 DesCoolCoilInHumRat; // zone cooling coil design air inlet humidity ratio [kg/kg]
		Real64 DesHeatCoilInTempTU; // zone heating coil design air inlet temperature (supply air)([C]
		Real64 DesCoolCoilInTempTU; // zone cooling coil design air inlet temperature (supply air)[C]
		Real64 DesHeatCoilInHumRatTU; // zone heating coil design air inlet humidity ratio
		//  (supply air) [kg/kg]
		Real64 DesCoolCoilInHumRatTU; // zone cooling coil design air inlet humidity ratio
		//  (supply air) [kg/kg]
		Real64 HeatMassFlow; // current zone heating air mass flow rate (HVAC time step)
		Real64 CoolMassFlow; // current zone cooling air mass flow rate (HVAC time step)
		Real64 HeatLoad; // current zone heating load (HVAC time step)
		Real64 CoolLoad; // current zone heating load (HVAC time step)
		Real64 HeatZoneTemp; // current zone temperature (heating, time step)
		Real64 HeatOutTemp; // current outdoor temperature (heating, time step)
		Real64 HeatZoneRetTemp; // current zone return temperature (heating, time step)
		Real64 HeatTstatTemp; // current zone thermostat temperature (heating, time step)
		Real64 CoolZoneTemp; // current zone temperature (cooling, time step)
		Real64 CoolOutTemp; // current Outdoor temperature (cooling, time step)
		Real64 CoolZoneRetTemp; // current zone return temperature (cooling, time step)
		Real64 CoolTstatTemp; // current zone thermostat temperature (cooling, time step)
		Real64 HeatZoneHumRat; // current zone humidity ratio (heating, time step)
		Real64 CoolZoneHumRat; // current zone humidity ratio (cooling, time step)
		Real64 HeatOutHumRat; // current outdoor humidity ratio (heating, time step)
		Real64 CoolOutHumRat; // current outdoor humidity ratio (cooling, time step)
		Real64 ZoneTempAtHeatPeak; // zone temp at max heating [C]
		Real64 ZoneRetTempAtHeatPeak; // zone return temp at max heating [C]
		Real64 OutTempAtHeatPeak; // outdoor temperature at max heating [C]
		Real64 ZoneTempAtCoolPeak; // zone temp at max cooling [C]
		Real64 ZoneRetTempAtCoolPeak; // zone return temp at max cooling [C]
		Real64 OutTempAtCoolPeak; // outdoor temperature at max cooling [C]
		Real64 ZoneHumRatAtHeatPeak; // zone humidity ratio at max heating [kg/kg]
		Real64 ZoneHumRatAtCoolPeak; // zone humidity ratio at max cooling [kg/kg]
		Real64 OutHumRatAtHeatPeak; // outdoor humidity at max heating [kg/kg]
		Real64 OutHumRatAtCoolPeak; // outdoor humidity at max cooling [kg/kg]
		int TimeStepNumAtHeatMax; // time step number (in day) at Heating peak
		int TimeStepNumAtCoolMax; // time step number (in day) at cooling peak
		int HeatDDNum; // design day index of design day causing heating peak
		int CoolDDNum; // design day index of design day causing heating peak
		std::string cHeatDDDate; // date of design day causing heating peak
		std::string cCoolDDDate; // date of design day causing cooling peak
		Real64 MinOA; // design minimum outside air in m3/s
		Real64 DesCoolMinAirFlow2; // design cooling minimum air flow rate [m3/s] derived from
		//  DesCoolMinAirFlowPerArea
		Real64 DesHeatMaxAirFlow2; // design heating maximum air flow rate [m3/s] derived from
		//  DesHeatMaxAirFlowPerArea
		FArray1D< Real64 > HeatFlowSeq; // daily sequence of zone heating air mass flow rate
		//  (zone time step)
		FArray1D< Real64 > CoolFlowSeq; // daily sequence of zone cooling air mass flow rate
		//  (zone time step)
		FArray1D< Real64 > HeatLoadSeq; // daily sequence of zone heating load zone time step)
		FArray1D< Real64 > CoolLoadSeq; // daily sequence of zone cooling load zone time step)
		FArray1D< Real64 > HeatZoneTempSeq; // daily sequence of zone temperatures (heating, zone time step)
		FArray1D< Real64 > HeatOutTempSeq; // daily sequence of outdoor temperatures (heating, zone time step)
		FArray1D< Real64 > HeatZoneRetTempSeq; // daily sequence of zone return temperatures (heating,
		//  zone time step)
		FArray1D< Real64 > HeatTstatTempSeq; // daily sequence of zone thermostat temperatures (heating, zone time step)
		FArray1D< Real64 > DesHeatSetPtSeq; // daily sequence of indoor set point temperatures (zone time step)
		FArray1D< Real64 > CoolZoneTempSeq; // daily sequence of zone temperatures (cooling, zone time step)
		FArray1D< Real64 > CoolOutTempSeq; // daily sequence of outdoor temperatures (cooling, zone time step)
		FArray1D< Real64 > CoolZoneRetTempSeq; // daily sequence of zone return temperatures (cooling,
		//  zone time step)
		FArray1D< Real64 > CoolTstatTempSeq; // daily sequence of zone thermostat temperatures (cooling, zone time step)
		FArray1D< Real64 > DesCoolSetPtSeq; // daily sequence of indoor set point temperatures (zone time step)
		FArray1D< Real64 > HeatZoneHumRatSeq; // daily sequence of zone humidity ratios (heating, zone time step)
		FArray1D< Real64 > CoolZoneHumRatSeq; // daily sequence of zone humidity ratios (cooling, zone time step)
		FArray1D< Real64 > HeatOutHumRatSeq; // daily sequence of outdoor humidity ratios (heating, zone time step)
		FArray1D< Real64 > CoolOutHumRatSeq; // daily sequence of outdoor humidity ratios (cooling, zone time step)
		Real64 ZoneADEffCooling; // the zone air distribution effectiveness in cooling mode
		Real64 ZoneADEffHeating; // the zone air distribution effectiveness in heating mode
		Real64 ZoneSecondaryRecirculation; // the zone secondary air recirculation fraction
		Real64 ZonePrimaryAirFraction; // the zone primary air fraction for cooling based calculations
		Real64 ZonePrimaryAirFractionHtg; // the zone primary air fraction for heating based calculations
		Real64 ZoneOAFracCooling; // OA fraction in cooling mode
		Real64 ZoneOAFracHeating; // OA fraction in heating mode
		Real64 TotalOAFromPeople; // Zone OA required due to people
		Real64 TotalOAFromArea; // Zone OA required based on floor area
		Real64 TotPeopleInZone; // total number of people in the zone
		Real64 TotalZoneFloorArea; // total zone floor area
		Real64 ZonePeakOccupancy; // zone peak occupancy based on max schedule value
		Real64 SupplyAirAdjustFactor; // supply air adjustment factor for next time step if OA is capped
		Real64 ZpzClgByZone; // OA Std 62.1 required fraction in cooling mode
		Real64 ZpzHtgByZone; // OA Std 62.1 required fraction in heating mode
		Real64 VozClgByZone; // value of required cooling vent to zone, used in 62.1 tabular report
		Real64 VozHtgByZone; // value of required heating vent to zone, used in 62.1 tabular report

		// Default Constructor
		ZoneSizingData() :
			ZnCoolDgnSAMethod( 0 ),
			ZnHeatDgnSAMethod( 0 ),
			CoolDesTemp( 0.0 ),
			HeatDesTemp( 0.0 ),
			CoolDesTempDiff( 0.0 ),
			HeatDesTempDiff( 0.0 ),
			CoolDesHumRat( 0.0 ),
			HeatDesHumRat( 0.0 ),
			ZoneDesignSpecOAIndex( 0 ),
			OADesMethod( 0 ),
			DesOAFlowPPer( 0.0 ),
			DesOAFlowPerArea( 0.0 ),
			DesOAFlow( 0.0 ),
			CoolAirDesMethod( 0 ),
			InpDesCoolAirFlow( 0.0 ),
			DesCoolMinAirFlowPerArea( 0.0 ),
			DesCoolMinAirFlow( 0.0 ),
			DesCoolMinAirFlowFrac( 0.0 ),
			HeatAirDesMethod( 0 ),
			InpDesHeatAirFlow( 0.0 ),
			DesHeatMaxAirFlowPerArea( 0.0 ),
			DesHeatMaxAirFlow( 0.0 ),
			DesHeatMaxAirFlowFrac( 0.0 ),
			HeatSizingFactor( 0.0 ),
			CoolSizingFactor( 0.0 ),
			ActualZoneNum( 0 ),
			SupplyAirNode( 0 ),
			DesHeatMassFlow( 0.0 ),
			DesHeatOAFlowFrac( 0.0 ),
			EMSOverrideDesHeatMassOn( false ),
			EMSValueDesHeatMassFlow( 0.0 ),
			DesCoolMassFlow( 0.0 ),
			DesCoolOAFlowFrac( 0.0 ),
			EMSOverrideDesCoolMassOn( false ),
			EMSValueDesCoolMassFlow( 0.0 ),
			DesHeatLoad( 0.0 ),
			EMSOverrideDesHeatLoadOn( false ),
			EMSValueDesHeatLoad( 0.0 ),
			DesCoolLoad( 0.0 ),
			EMSOverrideDesCoolLoadOn( false ),
			EMSValueDesCoolLoad( 0.0 ),
			DesHeatDens( 0.0 ),
			DesCoolDens( 0.0 ),
			DesHeatVolFlow( 0.0 ),
			EMSOverrideDesHeatVolOn( false ),
			EMSValueDesHeatVolFlow( 0.0 ),
			DesCoolVolFlow( 0.0 ),
			EMSOverrideDesCoolVolOn( false ),
			EMSValueDesCoolVolFlow( 0.0 ),
			DesHeatVolFlowMax( 0.0 ),
			DesCoolVolFlowMin( 0.0 ),
			DesHeatCoilInTemp( 0.0 ),
			DesCoolCoilInTemp( 0.0 ),
			DesHeatCoilInHumRat( 0.0 ),
			DesCoolCoilInHumRat( 0.0 ),
			DesHeatCoilInTempTU( 0.0 ),
			DesCoolCoilInTempTU( 0.0 ),
			DesHeatCoilInHumRatTU( 0.0 ),
			DesCoolCoilInHumRatTU( 0.0 ),
			HeatMassFlow( 0.0 ),
			CoolMassFlow( 0.0 ),
			HeatLoad( 0.0 ),
			CoolLoad( 0.0 ),
			HeatZoneTemp( 0.0 ),
			HeatOutTemp( 0.0 ),
			HeatZoneRetTemp( 0.0 ),
			HeatTstatTemp( 0.0 ),
			CoolZoneTemp( 0.0 ),
			CoolOutTemp( 0.0 ),
			CoolZoneRetTemp( 0.0 ),
			CoolTstatTemp( 0.0 ),
			HeatZoneHumRat( 0.0 ),
			CoolZoneHumRat( 0.0 ),
			HeatOutHumRat( 0.0 ),
			CoolOutHumRat( 0.0 ),
			ZoneTempAtHeatPeak( 0.0 ),
			ZoneRetTempAtHeatPeak( 0.0 ),
			OutTempAtHeatPeak( 0.0 ),
			ZoneTempAtCoolPeak( 0.0 ),
			ZoneRetTempAtCoolPeak( 0.0 ),
			OutTempAtCoolPeak( 0.0 ),
			ZoneHumRatAtHeatPeak( 0.0 ),
			ZoneHumRatAtCoolPeak( 0.0 ),
			OutHumRatAtHeatPeak( 0.0 ),
			OutHumRatAtCoolPeak( 0.0 ),
			TimeStepNumAtHeatMax( 0 ),
			TimeStepNumAtCoolMax( 0 ),
			HeatDDNum( 0 ),
			CoolDDNum( 0 ),
			MinOA( 0.0 ),
			DesCoolMinAirFlow2( 0.0 ),
			DesHeatMaxAirFlow2( 0.0 ),
			ZoneADEffCooling( 1.0 ),
			ZoneADEffHeating( 1.0 ),
			ZoneSecondaryRecirculation( 0.0 ),
			ZonePrimaryAirFraction( 0.0 ),
			ZonePrimaryAirFractionHtg( 0.0 ),
			ZoneOAFracCooling( 0.0 ),
			ZoneOAFracHeating( 0.0 ),
			TotalOAFromPeople( 0.0 ),
			TotalOAFromArea( 0.0 ),
			TotPeopleInZone( 0.0 ),
			TotalZoneFloorArea( 0.0 ),
			ZonePeakOccupancy( 0.0 ),
			SupplyAirAdjustFactor( 1.0 ),
			ZpzClgByZone( 0.0 ),
			ZpzHtgByZone( 0.0 ),
			VozClgByZone( 0.0 ),
			VozHtgByZone( 0.0 )
		{}

		// Member Constructor
		ZoneSizingData(
			std::string const & ZoneName, // name of a zone
			std::string const & CoolDesDay, // name of a cooling design day
			std::string const & HeatDesDay, // name of a heating design day
			int const ZnCoolDgnSAMethod, // choice of how to get zone cooling design air temperature;
			int const ZnHeatDgnSAMethod, // choice of how to get zone heating design air temperature;
			Real64 const CoolDesTemp, // zone design cooling supply air temperature [C]
			Real64 const HeatDesTemp, // zone design heating supply air temperature [C]
			Real64 const CoolDesTempDiff, // zone design cooling supply air temperature difference [deltaC]
			Real64 const HeatDesTempDiff, // zone design heating supply air temperature difference [deltaC]
			Real64 const CoolDesHumRat, // zone design cooling supply air humidity ratio [kg-H2O/kg-air]
			Real64 const HeatDesHumRat, // zone design heating supply air humidity ratio [kg-H2O/kg-air]
			int const ZoneDesignSpecOAIndex, // index to DesignSpecification:OutdoorAir object
			int const OADesMethod, // choice of how to calculate minimum outside air;
			Real64 const DesOAFlowPPer, // design outside air flow per person in zone [m3/s]
			Real64 const DesOAFlowPerArea, // design outside air flow per zone area [m3/s / m2]
			Real64 const DesOAFlow, // design outside air flow for the zone [m3/s]
			int const CoolAirDesMethod, // choice of how to get zone cooling design air flow rates;
			Real64 const InpDesCoolAirFlow, // design zone supply air flow rate [m3/s]
			Real64 const DesCoolMinAirFlowPerArea, // design cooling minimum air flow rate per zone area [m3/s / m2]
			Real64 const DesCoolMinAirFlow, // design cooling minimum air flow rate [m3/s]
			Real64 const DesCoolMinAirFlowFrac, // design cooling minimum air flow rate fraction
			int const HeatAirDesMethod, // choice of how to get zone heating design air flow rates;
			Real64 const InpDesHeatAirFlow, // design zone heating supply air flow rate [m3/s]
			Real64 const DesHeatMaxAirFlowPerArea, // design heating maximum air flow rate per zone area [m3/s / m2]
			Real64 const DesHeatMaxAirFlow, // design heating maximum air flow rate [m3/s]
			Real64 const DesHeatMaxAirFlowFrac, // design heating maximum air flow rate fraction
			Real64 const HeatSizingFactor, // the zone heating sizing ratio
			Real64 const CoolSizingFactor, // the zone cooling sizing ratio
			int const ActualZoneNum, // index into the Zone data array (in DataHeatBalance)
			int const SupplyAirNode, // node number of supply air node
			Real64 const DesHeatMassFlow, // zone design heating air mass flow rate [kg/s]
			Real64 const DesHeatOAFlowFrac, // zone design heating OA air volume fraction [-]
			bool const EMSOverrideDesHeatMassOn, // true if EMS is acting on this structure
			Real64 const EMSValueDesHeatMassFlow, // Value EMS directing to use for Design Heating air mass flow [kg/s]
			Real64 const DesCoolMassFlow, // zone design cooling air mass flow rate [kg/s]
			Real64 const DesCoolOAFlowFrac, // zone design cooling OA air volume fraction [-]
			bool const EMSOverrideDesCoolMassOn, // true if EMS is acting on this structure
			Real64 const EMSValueDesCoolMassFlow, // Value EMS directing to use for Design Cooling air mass flow [kg/s]
			Real64 const DesHeatLoad, // zone design heating load [W]
			bool const EMSOverrideDesHeatLoadOn, // true if EMS is acting on this structure
			Real64 const EMSValueDesHeatLoad, // Value EMS directing to use for zone design heating load  [W]
			Real64 const DesCoolLoad, // zone design cooling load [W]
			bool const EMSOverrideDesCoolLoadOn, // true if EMS is acting on this structure
			Real64 const EMSValueDesCoolLoad, // Value EMS directing to use for zone design cooling load  [W]
			Real64 const DesHeatDens, // zone design heating air density [kg/m3]
			Real64 const DesCoolDens, // zone design cooling air density [kg/m3]
			Real64 const DesHeatVolFlow, // zone design heating air volume flow rate [m3/s]
			bool const EMSOverrideDesHeatVolOn, // true if EMS is acting on this structure
			Real64 const EMSValueDesHeatVolFlow, // Value EMS directing to use for Design Heating air volume flow [m3/s]
			Real64 const DesCoolVolFlow, // zone design cooling air volume flow rate [m3/s]
			bool const EMSOverrideDesCoolVolOn, // true if EMS is acting on this structure
			Real64 const EMSValueDesCoolVolFlow, // Value EMS directing to use for Design cooling air volume flow [m3/s]
			Real64 const DesHeatVolFlowMax, // zone design heating maximum air volume flow rate [m3/s]
			Real64 const DesCoolVolFlowMin, // zone design cooling minimum air volume flow rate [m3/s]
			Real64 const DesHeatCoilInTemp, // zone heating coil design air inlet temperature [C]
			Real64 const DesCoolCoilInTemp, // zone cooling coil design air inlet temperature [C]
			Real64 const DesHeatCoilInHumRat, // zone heating coil design air inlet humidity ratio [kg/kg]
			Real64 const DesCoolCoilInHumRat, // zone cooling coil design air inlet humidity ratio [kg/kg]
			Real64 const DesHeatCoilInTempTU, // zone heating coil design air inlet temperature (supply air)([C]
			Real64 const DesCoolCoilInTempTU, // zone cooling coil design air inlet temperature (supply air)[C]
			Real64 const DesHeatCoilInHumRatTU, // zone heating coil design air inlet humidity ratio
			Real64 const DesCoolCoilInHumRatTU, // zone cooling coil design air inlet humidity ratio
			Real64 const HeatMassFlow, // current zone heating air mass flow rate (HVAC time step)
			Real64 const CoolMassFlow, // current zone cooling air mass flow rate (HVAC time step)
			Real64 const HeatLoad, // current zone heating load (HVAC time step)
			Real64 const CoolLoad, // current zone heating load (HVAC time step)
			Real64 const HeatZoneTemp, // current zone temperature (heating, time step)
			Real64 const HeatOutTemp, // current outdoor temperature (heating, time step)
			Real64 const HeatZoneRetTemp, // current zone return temperature (heating, time step)
			Real64 const HeatTstatTemp, // current zone thermostat temperature (heating, time step)
			Real64 const CoolZoneTemp, // current zone temperature (cooling, time step)
			Real64 const CoolOutTemp, // current Outdoor temperature (cooling, time step)
			Real64 const CoolZoneRetTemp, // current zone return temperature (cooling, time step)
			Real64 const CoolTstatTemp, // current zone thermostat temperature (cooling, time step)
			Real64 const HeatZoneHumRat, // current zone humidity ratio (heating, time step)
			Real64 const CoolZoneHumRat, // current zone humidity ratio (cooling, time step)
			Real64 const HeatOutHumRat, // current outdoor humidity ratio (heating, time step)
			Real64 const CoolOutHumRat, // current outdoor humidity ratio (cooling, time step)
			Real64 const ZoneTempAtHeatPeak, // zone temp at max heating [C]
			Real64 const ZoneRetTempAtHeatPeak, // zone return temp at max heating [C]
			Real64 const OutTempAtHeatPeak, // outdoor temperature at max heating [C]
			Real64 const ZoneTempAtCoolPeak, // zone temp at max cooling [C]
			Real64 const ZoneRetTempAtCoolPeak, // zone return temp at max cooling [C]
			Real64 const OutTempAtCoolPeak, // outdoor temperature at max cooling [C]
			Real64 const ZoneHumRatAtHeatPeak, // zone humidity ratio at max heating [kg/kg]
			Real64 const ZoneHumRatAtCoolPeak, // zone humidity ratio at max cooling [kg/kg]
			Real64 const OutHumRatAtHeatPeak, // outdoor humidity at max heating [kg/kg]
			Real64 const OutHumRatAtCoolPeak, // outdoor humidity at max cooling [kg/kg]
			int const TimeStepNumAtHeatMax, // time step number (in day) at Heating peak
			int const TimeStepNumAtCoolMax, // time step number (in day) at cooling peak
			int const HeatDDNum, // design day index of design day causing heating peak
			int const CoolDDNum, // design day index of design day causing heating peak
			std::string const & cHeatDDDate, // date of design day causing heating peak
			std::string const & cCoolDDDate, // date of design day causing cooling peak
			Real64 const MinOA, // design minimum outside air in m3/s
			Real64 const DesCoolMinAirFlow2, // design cooling minimum air flow rate [m3/s] derived from
			Real64 const DesHeatMaxAirFlow2, // design heating maximum air flow rate [m3/s] derived from
			FArray1< Real64 > const & HeatFlowSeq, // daily sequence of zone heating air mass flow rate
			FArray1< Real64 > const & CoolFlowSeq, // daily sequence of zone cooling air mass flow rate
			FArray1< Real64 > const & HeatLoadSeq, // daily sequence of zone heating load zone time step)
			FArray1< Real64 > const & CoolLoadSeq, // daily sequence of zone cooling load zone time step)
			FArray1< Real64 > const & HeatZoneTempSeq, // daily sequence of zone temperatures (heating, zone time step)
			FArray1< Real64 > const & HeatOutTempSeq, // daily sequence of outdoor temperatures (heating, zone time step)
			FArray1< Real64 > const & HeatZoneRetTempSeq, // daily sequence of zone return temperatures (heating,
			FArray1< Real64 > const & HeatTstatTempSeq, // daily sequence of zone thermostat temperatures (heating, zone time step)
			FArray1< Real64 > const & DesHeatSetPtSeq, // daily sequence of indoor set point temperatures (zone time step)
			FArray1< Real64 > const & CoolZoneTempSeq, // daily sequence of zone temperatures (cooling, zone time step)
			FArray1< Real64 > const & CoolOutTempSeq, // daily sequence of outdoor temperatures (cooling, zone time step)
			FArray1< Real64 > const & CoolZoneRetTempSeq, // daily sequence of zone return temperatures (cooling,
			FArray1< Real64 > const & CoolTstatTempSeq, // daily sequence of zone thermostat temperatures (cooling, zone time step)
			FArray1< Real64 > const & DesCoolSetPtSeq, // daily sequence of indoor set point temperatures (zone time step)
			FArray1< Real64 > const & HeatZoneHumRatSeq, // daily sequence of zone humidity ratios (heating, zone time step)
			FArray1< Real64 > const & CoolZoneHumRatSeq, // daily sequence of zone humidity ratios (cooling, zone time step)
			FArray1< Real64 > const & HeatOutHumRatSeq, // daily sequence of outdoor humidity ratios (heating, zone time step)
			FArray1< Real64 > const & CoolOutHumRatSeq, // daily sequence of outdoor humidity ratios (cooling, zone time step)
			Real64 const ZoneADEffCooling, // the zone air distribution effectiveness in cooling mode
			Real64 const ZoneADEffHeating, // the zone air distribution effectiveness in heating mode
			Real64 const ZoneSecondaryRecirculation, // the zone secondary air recirculation fraction
			Real64 const ZonePrimaryAirFraction, // the zone primary air fraction for cooling based calculations
			Real64 const ZonePrimaryAirFractionHtg, // the zone primary air fraction for heating based calculations
			Real64 const ZoneOAFracCooling, // OA fraction in cooling mode
			Real64 const ZoneOAFracHeating, // OA fraction in heating mode
			Real64 const TotalOAFromPeople, // Zone OA required due to people
			Real64 const TotalOAFromArea, // Zone OA required based on floor area
			Real64 const TotPeopleInZone, // total number of people in the zone
			Real64 const TotalZoneFloorArea, // total zone floor area
			Real64 const ZonePeakOccupancy, // zone peak occupancy based on max schedule value
			Real64 const SupplyAirAdjustFactor, // supply air adjustment factor for next time step if OA is capped
			Real64 const ZpzClgByZone, // OA Std 62.1 required fraction in cooling mode
			Real64 const ZpzHtgByZone, // OA Std 62.1 required fraction in heating mode
			Real64 const VozClgByZone, // value of required cooling vent to zone, used in 62.1 tabular report
			Real64 const VozHtgByZone // value of required heating vent to zone, used in 62.1 tabular report
		) :
			ZoneName( ZoneName ),
			CoolDesDay( CoolDesDay ),
			HeatDesDay( HeatDesDay ),
			ZnCoolDgnSAMethod( ZnCoolDgnSAMethod ),
			ZnHeatDgnSAMethod( ZnHeatDgnSAMethod ),
			CoolDesTemp( CoolDesTemp ),
			HeatDesTemp( HeatDesTemp ),
			CoolDesTempDiff( CoolDesTempDiff ),
			HeatDesTempDiff( HeatDesTempDiff ),
			CoolDesHumRat( CoolDesHumRat ),
			HeatDesHumRat( HeatDesHumRat ),
			ZoneDesignSpecOAIndex( ZoneDesignSpecOAIndex ),
			OADesMethod( OADesMethod ),
			DesOAFlowPPer( DesOAFlowPPer ),
			DesOAFlowPerArea( DesOAFlowPerArea ),
			DesOAFlow( DesOAFlow ),
			CoolAirDesMethod( CoolAirDesMethod ),
			InpDesCoolAirFlow( InpDesCoolAirFlow ),
			DesCoolMinAirFlowPerArea( DesCoolMinAirFlowPerArea ),
			DesCoolMinAirFlow( DesCoolMinAirFlow ),
			DesCoolMinAirFlowFrac( DesCoolMinAirFlowFrac ),
			HeatAirDesMethod( HeatAirDesMethod ),
			InpDesHeatAirFlow( InpDesHeatAirFlow ),
			DesHeatMaxAirFlowPerArea( DesHeatMaxAirFlowPerArea ),
			DesHeatMaxAirFlow( DesHeatMaxAirFlow ),
			DesHeatMaxAirFlowFrac( DesHeatMaxAirFlowFrac ),
			HeatSizingFactor( HeatSizingFactor ),
			CoolSizingFactor( CoolSizingFactor ),
			ActualZoneNum( ActualZoneNum ),
			SupplyAirNode( SupplyAirNode ),
			DesHeatMassFlow( DesHeatMassFlow ),
			DesHeatOAFlowFrac( DesHeatOAFlowFrac ),
			EMSOverrideDesHeatMassOn( EMSOverrideDesHeatMassOn ),
			EMSValueDesHeatMassFlow( EMSValueDesHeatMassFlow ),
			DesCoolMassFlow( DesCoolMassFlow ),
			DesCoolOAFlowFrac( DesCoolOAFlowFrac ),
			EMSOverrideDesCoolMassOn( EMSOverrideDesCoolMassOn ),
			EMSValueDesCoolMassFlow( EMSValueDesCoolMassFlow ),
			DesHeatLoad( DesHeatLoad ),
			EMSOverrideDesHeatLoadOn( EMSOverrideDesHeatLoadOn ),
			EMSValueDesHeatLoad( EMSValueDesHeatLoad ),
			DesCoolLoad( DesCoolLoad ),
			EMSOverrideDesCoolLoadOn( EMSOverrideDesCoolLoadOn ),
			EMSValueDesCoolLoad( EMSValueDesCoolLoad ),
			DesHeatDens( DesHeatDens ),
			DesCoolDens( DesCoolDens ),
			DesHeatVolFlow( DesHeatVolFlow ),
			EMSOverrideDesHeatVolOn( EMSOverrideDesHeatVolOn ),
			EMSValueDesHeatVolFlow( EMSValueDesHeatVolFlow ),
			DesCoolVolFlow( DesCoolVolFlow ),
			EMSOverrideDesCoolVolOn( EMSOverrideDesCoolVolOn ),
			EMSValueDesCoolVolFlow( EMSValueDesCoolVolFlow ),
			DesHeatVolFlowMax( DesHeatVolFlowMax ),
			DesCoolVolFlowMin( DesCoolVolFlowMin ),
			DesHeatCoilInTemp( DesHeatCoilInTemp ),
			DesCoolCoilInTemp( DesCoolCoilInTemp ),
			DesHeatCoilInHumRat( DesHeatCoilInHumRat ),
			DesCoolCoilInHumRat( DesCoolCoilInHumRat ),
			DesHeatCoilInTempTU( DesHeatCoilInTempTU ),
			DesCoolCoilInTempTU( DesCoolCoilInTempTU ),
			DesHeatCoilInHumRatTU( DesHeatCoilInHumRatTU ),
			DesCoolCoilInHumRatTU( DesCoolCoilInHumRatTU ),
			HeatMassFlow( HeatMassFlow ),
			CoolMassFlow( CoolMassFlow ),
			HeatLoad( HeatLoad ),
			CoolLoad( CoolLoad ),
			HeatZoneTemp( HeatZoneTemp ),
			HeatOutTemp( HeatOutTemp ),
			HeatZoneRetTemp( HeatZoneRetTemp ),
			HeatTstatTemp( HeatTstatTemp ),
			CoolZoneTemp( CoolZoneTemp ),
			CoolOutTemp( CoolOutTemp ),
			CoolZoneRetTemp( CoolZoneRetTemp ),
			CoolTstatTemp( CoolTstatTemp ),
			HeatZoneHumRat( HeatZoneHumRat ),
			CoolZoneHumRat( CoolZoneHumRat ),
			HeatOutHumRat( HeatOutHumRat ),
			CoolOutHumRat( CoolOutHumRat ),
			ZoneTempAtHeatPeak( ZoneTempAtHeatPeak ),
			ZoneRetTempAtHeatPeak( ZoneRetTempAtHeatPeak ),
			OutTempAtHeatPeak( OutTempAtHeatPeak ),
			ZoneTempAtCoolPeak( ZoneTempAtCoolPeak ),
			ZoneRetTempAtCoolPeak( ZoneRetTempAtCoolPeak ),
			OutTempAtCoolPeak( OutTempAtCoolPeak ),
			ZoneHumRatAtHeatPeak( ZoneHumRatAtHeatPeak ),
			ZoneHumRatAtCoolPeak( ZoneHumRatAtCoolPeak ),
			OutHumRatAtHeatPeak( OutHumRatAtHeatPeak ),
			OutHumRatAtCoolPeak( OutHumRatAtCoolPeak ),
			TimeStepNumAtHeatMax( TimeStepNumAtHeatMax ),
			TimeStepNumAtCoolMax( TimeStepNumAtCoolMax ),
			HeatDDNum( HeatDDNum ),
			CoolDDNum( CoolDDNum ),
			cHeatDDDate( cHeatDDDate ),
			cCoolDDDate( cCoolDDDate ),
			MinOA( MinOA ),
			DesCoolMinAirFlow2( DesCoolMinAirFlow2 ),
			DesHeatMaxAirFlow2( DesHeatMaxAirFlow2 ),
			HeatFlowSeq( HeatFlowSeq ),
			CoolFlowSeq( CoolFlowSeq ),
			HeatLoadSeq( HeatLoadSeq ),
			CoolLoadSeq( CoolLoadSeq ),
			HeatZoneTempSeq( HeatZoneTempSeq ),
			HeatOutTempSeq( HeatOutTempSeq ),
			HeatZoneRetTempSeq( HeatZoneRetTempSeq ),
			HeatTstatTempSeq( HeatTstatTempSeq ),
			DesHeatSetPtSeq( DesHeatSetPtSeq ),
			CoolZoneTempSeq( CoolZoneTempSeq ),
			CoolOutTempSeq( CoolOutTempSeq ),
			CoolZoneRetTempSeq( CoolZoneRetTempSeq ),
			CoolTstatTempSeq( CoolTstatTempSeq ),
			DesCoolSetPtSeq( DesCoolSetPtSeq ),
			HeatZoneHumRatSeq( HeatZoneHumRatSeq ),
			CoolZoneHumRatSeq( CoolZoneHumRatSeq ),
			HeatOutHumRatSeq( HeatOutHumRatSeq ),
			CoolOutHumRatSeq( CoolOutHumRatSeq ),
			ZoneADEffCooling( ZoneADEffCooling ),
			ZoneADEffHeating( ZoneADEffHeating ),
			ZoneSecondaryRecirculation( ZoneSecondaryRecirculation ),
			ZonePrimaryAirFraction( ZonePrimaryAirFraction ),
			ZonePrimaryAirFractionHtg( ZonePrimaryAirFractionHtg ),
			ZoneOAFracCooling( ZoneOAFracCooling ),
			ZoneOAFracHeating( ZoneOAFracHeating ),
			TotalOAFromPeople( TotalOAFromPeople ),
			TotalOAFromArea( TotalOAFromArea ),
			TotPeopleInZone( TotPeopleInZone ),
			TotalZoneFloorArea( TotalZoneFloorArea ),
			ZonePeakOccupancy( ZonePeakOccupancy ),
			SupplyAirAdjustFactor( SupplyAirAdjustFactor ),
			ZpzClgByZone( ZpzClgByZone ),
			ZpzHtgByZone( ZpzHtgByZone ),
			VozClgByZone( VozClgByZone ),
			VozHtgByZone( VozHtgByZone )
		{}

	};

	struct TermUnitSizingData
	{
		// Members
		Real64 AirVolFlow; // design air vol flow rate for single duct terminal unit [m3/s]
		Real64 MaxHWVolFlow; // design Hot Water vol flow for single duct terminal unit [m3/s]
		Real64 MaxSTVolFlow; // design Steam vol flow rate for single duct terminal unit [m3/s]
		Real64 MaxCWVolFlow; // design Cold Water vol flow for single duct terminal unit [m3/s]
		Real64 MinFlowFrac; // design minimum flow fraction for a terminal unit
		Real64 InducRat; // design induction ratio for a terminal unit
		bool InducesPlenumAir; // True if secondary air comes from the plenum
		Real64 ReheatAirFlowMult; // multiplier for air flow in reheat coil UA calculation
		Real64 ReheatLoadMult; // multiplier for load in reheat coil UA calculation
		Real64 DesCoolingLoad; // design cooling load used for zone equipment [W]
		Real64 DesHeatingLoad; // design heating load used for zone equipment [W]

		// Default Constructor
		TermUnitSizingData() :
			AirVolFlow( 0.0 ),
			MaxHWVolFlow( 0.0 ),
			MaxSTVolFlow( 0.0 ),
			MaxCWVolFlow( 0.0 ),
			MinFlowFrac( 0.0 ),
			InducRat( 0.0 ),
			InducesPlenumAir( false ),
			ReheatAirFlowMult( 1.0 ),
			ReheatLoadMult( 1.0 ),
			DesCoolingLoad( 0.0 ),
			DesHeatingLoad( 0.0 )
		{}

		// Member Constructor
		TermUnitSizingData(
			Real64 const AirVolFlow, // design air vol flow rate for single duct terminal unit [m3/s]
			Real64 const MaxHWVolFlow, // design Hot Water vol flow for single duct terminal unit [m3/s]
			Real64 const MaxSTVolFlow, // design Steam vol flow rate for single duct terminal unit [m3/s]
			Real64 const MaxCWVolFlow, // design Cold Water vol flow for single duct terminal unit [m3/s]
			Real64 const MinFlowFrac, // design minimum flow fraction for a terminal unit
			Real64 const InducRat, // design induction ratio for a terminal unit
			bool const InducesPlenumAir, // True if secondary air comes from the plenum
			Real64 const ReheatAirFlowMult, // multiplier for air flow in reheat coil UA calculation
			Real64 const ReheatLoadMult, // multiplier for load in reheat coil UA calculation
			Real64 const DesCoolingLoad, // design cooling load used for zone equipment [W]
			Real64 const DesHeatingLoad // design heating load used for zone equipment [W]
		) :
			AirVolFlow( AirVolFlow ),
			MaxHWVolFlow( MaxHWVolFlow ),
			MaxSTVolFlow( MaxSTVolFlow ),
			MaxCWVolFlow( MaxCWVolFlow ),
			MinFlowFrac( MinFlowFrac ),
			InducRat( InducRat ),
			InducesPlenumAir( InducesPlenumAir ),
			ReheatAirFlowMult( ReheatAirFlowMult ),
			ReheatLoadMult( ReheatLoadMult ),
			DesCoolingLoad( DesCoolingLoad ),
			DesHeatingLoad( DesHeatingLoad )
		{}

	};

	struct ZoneEqSizingData // data saved from zone eq component sizing and passed to subcomponents
	{
		// Members
		Real64 AirVolFlow; // design air vol flow rate for zone equipment unit [m3/s]
		Real64 MaxHWVolFlow; // design Hot Water vol flow for zone equipment unit [m3/s]
		Real64 MaxCWVolFlow; // design Cold Water vol flow for zone equipment unit [m3/s]
		Real64 OAVolFlow; // design outside air flow for zone equipment unit [m3/s]
		Real64 DesCoolingLoad; // design cooling load used for zone equipment [W]
		Real64 DesHeatingLoad; // design heating load used for zone equipment [W]
		Real64 CoolingAirVolFlow; // design cooling air vol flow rate for equipment[m3/s]
		Real64 HeatingAirVolFlow; // design heating air vol flow rate for equipment[m3/s]
		Real64 SystemAirVolFlow; // design heating air vol flow rate for equipment[m3/s]
		bool AirFlow; // TRUE if AirloopHVAC system air flow rate is calcualted
		bool CoolingAirFlow; // TRUE if AirloopHVAC system cooling air flow rate is calcualted
		bool HeatingAirFlow; // TRUE if AirloopHVAC system heating air flow rate is calcualted
		bool SystemAirFlow; // TRUE if AirloopHVAC system heating air flow rate is calcualted
		bool Capacity; // TRUE if AirloopHVAC system capacity is calculated
		bool CoolingCapacity; // TRUE if AirloopHVAC system cooling capacity is calculated
		bool HeatingCapacity; // TRUE if AirloopHVAC system heating capacity is calculated
		bool SystemCapacity; // TRUE if AirloopHVAC system heating capacity is calculated
		FArray1D_int SizingMethod; // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow and FractionOfAutosizedHeatingAirflow)
		FArray1D_int CapSizingMethod; // capacity sizing methods (HeatingDesignCapacity, CoolingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity and FractionOfAutosizedHeatingCapacity )

		// Default Constructor
		ZoneEqSizingData() :
			AirVolFlow( 0.0 ),
			MaxHWVolFlow( 0.0 ),
			MaxCWVolFlow( 0.0 ),
			OAVolFlow( 0.0 ),
			DesCoolingLoad( 0.0 ), // design cooling load used for zone equipment [W]
			DesHeatingLoad( 0.0 ), // design heating load used for zone equipment [W]
			CoolingAirVolFlow( 0.0 ), // design cooling air vol flow rate for equipment[m3/s]
			HeatingAirVolFlow( 0.0 ), // design heating air vol flow rate for equipment[m3/s]
			SystemAirVolFlow( 0.0 ), // design heating air vol flow rate for equipment[m3/s]
			AirFlow( false ), // TRUE if AirloopHVAC system air flow rate is calcualted
			CoolingAirFlow( false ), // TRUE if AirloopHVAC system cooling air flow rate is calcualted
			HeatingAirFlow( false ), // TRUE if AirloopHVAC system heating air flow rate is calcualted
			SystemAirFlow( false ), // TRUE if AirloopHVAC system heating air flow rate is calcualted
			Capacity( false ), // TRUE if AirloopHVAC system capacity is calculated
			CoolingCapacity( false ), // TRUE if AirloopHVAC system cooling capacity is calculated
			HeatingCapacity( false ), // TRUE if AirloopHVAC system heating capacity is calculated
			SystemCapacity( false ) // TRUE if AirloopHVAC system heating capacity is calculated
		{}

		// Member Constructor
		ZoneEqSizingData(
			Real64 const AirVolFlow, // design air vol flow rate for zone equipment unit [m3/s]
			Real64 const MaxHWVolFlow, // design Hot Water vol flow for zone equipment unit [m3/s]
			Real64 const MaxCWVolFlow, // design Cold Water vol flow for zone equipment unit [m3/s]
			Real64 const OAVolFlow, // design outside air flow for zone equipment unit [m3/s]
			Real64 const DesCoolingLoad, // design cooling load used for zone equipment [W]
			Real64 const DesHeatingLoad, // design heating load used for zone equipment [W]
			Real64 const  CoolingAirVolFlow, // design cooling air vol flow rate for equipment[m3/s]
			Real64 const  HeatingAirVolFlow, // design heating air vol flow rate for equipment[m3/s]
			Real64 const  SystemAirVolFlow, // design heating air vol flow rate for equipment[m3/s]
			bool const  AirFlow, // TRUE if AirloopHVAC system air flow rate is calcualted
			bool const  CoolingAirFlow, // TRUE if AirloopHVAC system cooling air flow rate is calcualted
			bool const  HeatingAirFlow, // TRUE if AirloopHVAC system heating air flow rate is calcualted
			bool const  SystemAirFlow, // TRUE if AirloopHVAC system heating air flow rate is calcualted
			bool const  Capacity, // TRUE if AirloopHVAC system capacity is calculated
			bool const  CoolingCapacity, // TRUE if AirloopHVAC system cooling capacity is calculated
			bool const  HeatingCapacity, // TRUE if AirloopHVAC system heating capacity is calculated
			bool const  SystemCapacity, // TRUE if AirloopHVAC system heating capacity is calculated
			FArray1_int const & SizingMethod  // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow and FractionOfAutosizedHeatingAirflow)
		) :
			AirVolFlow( AirVolFlow ),
			MaxHWVolFlow( MaxHWVolFlow ),
			MaxCWVolFlow( MaxCWVolFlow ),
			OAVolFlow( OAVolFlow ),
			DesCoolingLoad( DesCoolingLoad ),
			DesHeatingLoad( DesHeatingLoad ),
			CoolingAirVolFlow( CoolingAirVolFlow ),
			HeatingAirVolFlow( HeatingAirVolFlow ),
			SystemAirVolFlow( SystemAirVolFlow ),
			AirFlow( AirFlow ),
			CoolingAirFlow( CoolingAirFlow ),
			HeatingAirFlow( HeatingAirFlow ),
			SystemAirFlow( SystemAirFlow ),
			Capacity( Capacity ),
			CoolingCapacity( CoolingCapacity ),
			HeatingCapacity( HeatingCapacity ),
			SystemCapacity( SystemCapacity ),
			SizingMethod( SizingMethod )
		{}

	};


	// Data Structure for Zone HVAC sizing, referenced by various ZoneHVAC Equipment
	struct ZoneHVACSizingData
	{
		// Members
		std::string Name;
		int CoolingSAFMethod; // - Method for cooling supply air flow rate sizing calculation (SupplyAirFlowRate,FlowPerFloorArea, FractionOfAutoSizedCoolingValue, FlowPerCoolingCapacity)
		int HeatingSAFMethod; // - Method for heating supply air flow rate sizing calculation (SupplyAirFlowRate,FlowPerFloorArea, FractionOfAutoSizedHeatingValue, FlowPerHeatingCapacity,
		int NoCoolHeatSAFMethod; // - Method for supply air flow sizing during no cooling and heating calculation (SupplyAirFlowRate, FractionOfAutoSizedCoolingValue, FractionOfAutoSizedHeatingValue)
		int CoolingCapMethod; // - Method for cooling capacity scaledsizing calculation (CoolingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedHeatingCapacity)
		int HeatingCapMethod; // - Method for heatiing capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 MaxCoolAirVolFlow;// - maximum cooling supply air flow rate, m3/s
		Real64 MaxHeatAirVolFlow;// - maximum heating supply air flow rate, m3/s
		Real64 MaxNoCoolHeatAirVolFlow; // - maximum supply air flow rate when no cooling or heating, m3/s
		Real64 ScaledCoolingCapacity; // - scaled maximum cooling capacity of zone HVAC equipment, W
		Real64 ScaledHeatingCapacity; // - scaled maximum heating capacity of zone HVAC equipment, W
		bool RequestAutoSize; // - true if autosizing is requested


		// Default Constructor
		ZoneHVACSizingData() :
			CoolingSAFMethod(0),
			HeatingSAFMethod(0),
			NoCoolHeatSAFMethod(0),
			CoolingCapMethod(0),
			HeatingCapMethod(0),
			MaxCoolAirVolFlow(0.0),
			MaxHeatAirVolFlow(0.0),
			MaxNoCoolHeatAirVolFlow(0.0),
			ScaledCoolingCapacity(0.0),
			ScaledHeatingCapacity(0.0),
			RequestAutoSize(false)
		{}

		// Member Constructor
		ZoneHVACSizingData(
			std::string const & Name,
			int const CoolingSAFMethod, // - Method for cooling supply air flow rate sizing calculation
			int const HeatingSAFMethod, // - Method for heating supply air flow rate sizing calculation
			int const NoCoolHeatSAFMethod, // - Method for supply air flow rate sizing during no cooling and heating calculation
			int const CoolingCapMethod, // - Method for cooling capacity scaledsizing calculation
			int const HeatingCapMethod, // - Method for heatiing capacity scaledsizing calculation
			Real64 const MaxCoolAirVolFlow, // - maximum cooling supply air flow rate, m3/s
			Real64 const MaxHeatAirVolFlow, // - maximum heating supply air flow rate, m3/s
			Real64 const MaxNoCoolHeatAirVolFlow, // - maximum supply air flow rate when no cooling or heating, m3/s
			Real64 const ScaledCoolingCapacity, // - scaled maximum cooling capacity of zone HVAC equipment, W
			Real64 const ScaledHeatingCapacity, // - scaled maximum heating capacity of zone HVAC equipment, W
			bool const RequestAutoSize, // - true if autosizing is requested
			bool const CoolCoilExists, // True if a cooling coil is specified in the ZoneHVAC equip
			bool const HeatCoilExists // True if a heating coil is specified in the ZoneHVAC equipm
			) :
			Name(Name),
			CoolingSAFMethod(CoolingSAFMethod),
			HeatingSAFMethod(HeatingSAFMethod),
			NoCoolHeatSAFMethod(NoCoolHeatSAFMethod),
			CoolingCapMethod(CoolingCapMethod),
			HeatingCapMethod(HeatingCapMethod),
			MaxCoolAirVolFlow(MaxCoolAirVolFlow),
			MaxHeatAirVolFlow(MaxHeatAirVolFlow),
			MaxNoCoolHeatAirVolFlow(MaxNoCoolHeatAirVolFlow),
			ScaledCoolingCapacity(ScaledCoolingCapacity),
			ScaledHeatingCapacity(ScaledHeatingCapacity),
			RequestAutoSize(RequestAutoSize)
		{}

	};

	struct SystemSizingInputData
	{
		// Members
		std::string AirPriLoopName; // name of an AirLoopHVAC object
		int AirLoopNum; // index number of air loop
		int LoadSizeType; // type of load to size on;
		// 0=sensible, 1=latent, 2=total, 3=ventilation
		int SizingOption; // 1 = noncoincident, 2 = coincident
		int CoolOAOption; // 1 = use 100% outside air; 2 = use min OA; for cooling sizing
		int HeatOAOption; // 1 = use 100% outside air; 2 = use min OA; for heating sizing
		Real64 DesOutAirVolFlow; // design (minimum) outside air flow rate [m3/s]
		Real64 SysAirMinFlowRat; // minimum system air flow ratio
		Real64 PreheatTemp; // preheat design set temperature [C]
		Real64 PrecoolTemp; // precool design set temperature [C]
		Real64 PreheatHumRat; // preheat design humidity ratio [kg water/kg dry air]
		Real64 PrecoolHumRat; // precool design humidity ratio [kg water/kg dry air]
		Real64 CoolSupTemp; // cooling design supply air temperature [C]
		Real64 HeatSupTemp; // heating design supply air temperature [C]
		Real64 CoolSupHumRat; // cooling design supply air humidity ratio [kg water/kg dry air]
		Real64 HeatSupHumRat; // heating design supply air humidity ratio [kg water/kg dry air]
		int CoolAirDesMethod; // choice of how to get system cooling design air flow rates;
		//  1 = calc from des day simulation; 2=m3/s per system, user input
		Real64 DesCoolAirFlow; // design system supply air flow rate for cooling[m3/s]
		int HeatAirDesMethod; // choice of how to get system heating design air flow rates;
		//  1 = calc from des day simulation; 2=m3/s per zone, user input
		Real64 DesHeatAirFlow; // design system heating supply air flow rate [m3/s]
		int ScaleCoolSAFMethod; // choice of how to get system cooling scalable air flow rates; // (FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FlowPerCoolingCapacity)
		int ScaleHeatSAFMethod; // choice of how to get system heating scalable air flow rates; // (FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow, FlowPerHeatingCapacity)
		int SystemOAMethod; // System Outdoor Air Method; 1 = SOAM_ZoneSum, 2 = SOAM_VRP
		Real64 MaxZoneOAFraction; // maximum value of min OA for zones served by system
		bool OAAutoSized; // Set to true if design OA vol flow is set to 'autosize'
		int CoolingCapMethod; // - Method for cooling capacity scaledsizing calculation (CoolingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity)
		int HeatingCapMethod; // - Method for heatiing capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 ScaledCoolingCapacity; // - scaled maximum cooling capacity of cooling coil in an air loop
		Real64 ScaledHeatingCapacity; // - scaled maximum heating capacity of cooling coil in an air loop
		Real64 FloorAreaOnAirLoopCooled; // total floor of cooled zones served by an airloop
		Real64 FloorAreaOnAirLoopHeated; // total floor of heated zones served by an airloop
		Real64 FlowPerFloorAreaCooled; // ratio of cooling supply air flow rate to total floor area of cooled zones served by an airloop
		Real64 FlowPerFloorAreaHeated; // ratio of cooling supply air flow rate to total floor area of cooled zones served by an airloop
		Real64 FractionOfAutosizedCoolingAirflow; // fraction of of cooling supply air flow rate an airloop
		Real64 FractionOfAutosizedHeatingAirflow; // fraction of of heating supply air flow rate an airloop
		Real64 FlowPerCoolingCapacity; // ratio of cooling supply air flow rate to cooling capacity of an airloop
		Real64 FlowPerHeatingCapacity; // ratio of heating supply air flow rate to heating capacity of an airloop

		// in Sizing:System

		// Default Constructor
		SystemSizingInputData() :
			AirLoopNum( 0 ),
			LoadSizeType( 0 ),
			SizingOption( 0 ),
			CoolOAOption( 0 ),
			HeatOAOption( 0 ),
			DesOutAirVolFlow( 0.0 ),
			SysAirMinFlowRat( 0.0 ),
			PreheatTemp( 0.0 ),
			PrecoolTemp( 0.0 ),
			PreheatHumRat( 0.0 ),
			PrecoolHumRat( 0.0 ),
			CoolSupTemp( 0.0 ),
			HeatSupTemp( 0.0 ),
			CoolSupHumRat( 0.0 ),
			HeatSupHumRat( 0.0 ),
			CoolAirDesMethod( 0 ),
			DesCoolAirFlow( 0.0 ),
			HeatAirDesMethod( 0 ),
			DesHeatAirFlow( 0.0 ),
			ScaleCoolSAFMethod( 0 ),
			ScaleHeatSAFMethod( 0 ),
			SystemOAMethod( 0 ),
			MaxZoneOAFraction( 0.0 ),
			OAAutoSized( false ),
			CoolingCapMethod( 0 ),
			HeatingCapMethod( 0 ),
			ScaledCoolingCapacity( 0.0 ),
			ScaledHeatingCapacity( 0.0 ),
			FloorAreaOnAirLoopCooled( 0.0 ),
			FloorAreaOnAirLoopHeated( 0.0 ),
			FlowPerFloorAreaCooled( 0.0 ),
			FlowPerFloorAreaHeated( 0.0 ),
			FractionOfAutosizedCoolingAirflow( 1.0 ),
			FractionOfAutosizedHeatingAirflow( 1.0 ),
			FlowPerCoolingCapacity( 0.0 ),
			FlowPerHeatingCapacity( 0.0 )
		{}

		// Member Constructor
		SystemSizingInputData(
			std::string const & AirPriLoopName, // name of an AirLoopHVAC object
			int const AirLoopNum, // index number of air loop
			int const LoadSizeType, // type of load to size on;
			int const SizingOption, // 1 = noncoincident, 2 = coincident
			int const CoolOAOption, // 1 = use 100% outside air; 2 = use min OA; for cooling sizing
			int const HeatOAOption, // 1 = use 100% outside air; 2 = use min OA; for heating sizing
			Real64 const DesOutAirVolFlow, // design (minimum) outside air flow rate [m3/s]
			Real64 const SysAirMinFlowRat, // minimum system air flow ratio
			Real64 const PreheatTemp, // preheat design set temperature [C]
			Real64 const PrecoolTemp, // precool design set temperature [C]
			Real64 const PreheatHumRat, // preheat design humidity ratio [kg water/kg dry air]
			Real64 const PrecoolHumRat, // precool design humidity ratio [kg water/kg dry air]
			Real64 const CoolSupTemp, // cooling design supply air temperature [C]
			Real64 const HeatSupTemp, // heating design supply air temperature [C]
			Real64 const CoolSupHumRat, // cooling design supply air humidity ratio [kg water/kg dry air]
			Real64 const HeatSupHumRat, // heating design supply air humidity ratio [kg water/kg dry air]
			int const CoolAirDesMethod, // choice of how to get system cooling design air flow rates;
			Real64 const DesCoolAirFlow, // design system supply air flow rate for cooling[m3/s]
			int const HeatAirDesMethod, // choice of how to get system heating design air flow rates;
			Real64 const DesHeatAirFlow, // design system heating supply air flow rate [m3/s]
			int const ScaleCoolSAFMethod, // choice of how to get system cooling scalable air flow rates; // (FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FlowPerCoolingCapacity)
			int const ScaleHeatSAFMethod, // choice of how to get system heating scalable air flow rates; // (FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow, FlowPerHeatingCapacity)
			int const SystemOAMethod, // System Outdoor Air Method; 1 = SOAM_ZoneSum, 2 = SOAM_VRP
			Real64 const MaxZoneOAFraction, // maximum value of min OA for zones served by system
			bool const OAAutoSized, // Set to true if design OA vol flow is set to 'autosize'
			int const CoolingCapMethod, // - Method for cooling capacity scaledsizing calculation (CoolingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity)
			int const HeatingCapMethod, // - Method for heatiing capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
			Real64 const ScaledCoolingCapacity, // - scaled maximum cooling capacity of cooling coil in an air loop
			Real64 const ScaledHeatingCapacity, // - scaled maximum heating capacity of cooling coil in an air loop
			Real64 const FloorAreaOnAirLoopCooled, // total floor of cooled zones served by an airloop
			Real64 const FloorAreaOnAirLoopHeated, // total floor of heated zones served by an airloop
			Real64 const FlowPerFloorAreaCooled, // ratio of cooling supply air flow rate to total floor area of cooled zones served by an airloop
			Real64 const FlowPerFloorAreaHeated, // ratio of cooling supply air flow rate to total floor area of cooled zones served by an airloop
			Real64 const FractionOfAutosizedCoolingAirflow, // fraction of of cooling supply air flow rate an airloop
			Real64 const FractionOfAutosizedHeatingAirflow, // fraction of of heating supply air flow rate an airloop
			Real64 const FlowPerCoolingCapacity, // ratio of cooling supply air flow rate to cooling capacity of an airloop
			Real64 const FlowPerHeatingCapacity // ratio of heating supply air flow rate to heating capacity of an airloop
		) :
			AirPriLoopName( AirPriLoopName ),
			AirLoopNum( AirLoopNum ),
			LoadSizeType( LoadSizeType ),
			SizingOption( SizingOption ),
			CoolOAOption( CoolOAOption ),
			HeatOAOption( HeatOAOption ),
			DesOutAirVolFlow( DesOutAirVolFlow ),
			SysAirMinFlowRat( SysAirMinFlowRat ),
			PreheatTemp( PreheatTemp ),
			PrecoolTemp( PrecoolTemp ),
			PreheatHumRat( PreheatHumRat ),
			PrecoolHumRat( PrecoolHumRat ),
			CoolSupTemp( CoolSupTemp ),
			HeatSupTemp( HeatSupTemp ),
			CoolSupHumRat( CoolSupHumRat ),
			HeatSupHumRat( HeatSupHumRat ),
			CoolAirDesMethod( CoolAirDesMethod ),
			DesCoolAirFlow( DesCoolAirFlow ),
			HeatAirDesMethod( HeatAirDesMethod ),
			DesHeatAirFlow( DesHeatAirFlow ),
			ScaleCoolSAFMethod( ScaleCoolSAFMethod ),
			ScaleHeatSAFMethod( ScaleHeatSAFMethod ),
			SystemOAMethod( SystemOAMethod ),
			MaxZoneOAFraction( MaxZoneOAFraction ),
			OAAutoSized( OAAutoSized ),
			CoolingCapMethod( CoolingCapMethod ),
			HeatingCapMethod( HeatingCapMethod ),
			ScaledCoolingCapacity( ScaledCoolingCapacity ),
			ScaledHeatingCapacity( ScaledHeatingCapacity ),
			FloorAreaOnAirLoopCooled( FloorAreaOnAirLoopCooled ),
			FloorAreaOnAirLoopHeated( FloorAreaOnAirLoopCooled ),
			FlowPerFloorAreaCooled( FlowPerFloorAreaCooled ),
			FlowPerFloorAreaHeated( FlowPerFloorAreaHeated ),
			FractionOfAutosizedCoolingAirflow( FractionOfAutosizedCoolingAirflow ),
			FractionOfAutosizedHeatingAirflow( FractionOfAutosizedHeatingAirflow ),
			FlowPerCoolingCapacity( FlowPerCoolingCapacity ),
			FlowPerHeatingCapacity( FlowPerHeatingCapacity )
		{}
	};

	struct SystemSizingData // Contains data for system sizing
	{
		// Members
		std::string AirPriLoopName; // name of an AirLoopHVAC object
		std::string CoolDesDay; // name of a cooling design day
		std::string HeatDesDay; // name of a heating design day
		int LoadSizeType; // type of load to size on;
		// 0=sensible, 1=latent, 2=total, 3=ventilation
		int SizingOption; // 1 = noncoincident, 2 = coincident.
		int CoolOAOption; // 1 = use 100% outside air; 2 = use min OA; for cooling sizing
		int HeatOAOption; // 1 = use 100% outside air; 2 = use min OA; for heating sizing
		Real64 DesOutAirVolFlow; // design (minimum) outside air flow rate [m3/s]
		Real64 SysAirMinFlowRat; // minimum system air flow ratio
		Real64 PreheatTemp; // preheat design set temperature
		Real64 PrecoolTemp; // precool design set temperature [C]
		Real64 PreheatHumRat; // preheat design humidity ratio [kg water/kg dry air]
		Real64 PrecoolHumRat; // precool design humidity ratio [kg water/kg dry air]
		Real64 CoolSupTemp; // cooling design supply air temperature [C]
		Real64 HeatSupTemp; // heating design supply air temperature[C]
		Real64 CoolSupHumRat; // cooling design supply air humidity ratio [kg water/kg dry air]
		Real64 HeatSupHumRat; // heating design supply air humidity ratio [kg water/kg dry air]
		int CoolAirDesMethod; // choice of how to get system design cooling air flow rates;
		//  1 = calc from des day simulation; 2=m3/s per system, user input
		int HeatAirDesMethod; // choice of how to get system design heating air flow rates;
		//  1 = calc from des day simulation; 2=m3/s per system, user input
		Real64 InpDesCoolAirFlow; // input design system supply air flow rate [m3/s]
		Real64 InpDesHeatAirFlow; // input design system heating supply air flow rate [m3/s]
		Real64 CoinCoolMassFlow; // coincident peak cooling mass flow rate [kg/s]
		bool EMSOverrideCoinCoolMassFlowOn; // If true, EMS to change coincident peak cooling mass flow rate
		Real64 EMSValueCoinCoolMassFlow; // Value EMS wants for coincident peak cooling mass flow rate [kg/s]
		Real64 CoinHeatMassFlow; // coincident peak heating mass flow rate [kg/s]
		bool EMSOverrideCoinHeatMassFlowOn; // If true, EMS to set coincident peak heating mass flow rate
		Real64 EMSValueCoinHeatMassFlow; // Value EMS wants for coincident peak heating mass flow rate [kg/s]
		Real64 NonCoinCoolMassFlow; // noncoincident peak cooling mass flow rate [kg/s]
		bool EMSOverrideNonCoinCoolMassFlowOn; // true, EMS to set noncoincident peak cooling mass flow rate
		Real64 EMSValueNonCoinCoolMassFlow; // Value EMS for noncoincident peak cooling mass flow rate [kg/s]
		Real64 NonCoinHeatMassFlow; // noncoincident peak heating mass flow rate [kg/s]
		bool EMSOverrideNonCoinHeatMassFlowOn; // true, EMS to set noncoincident peak heating mass flow rate
		Real64 EMSValueNonCoinHeatMassFlow; // Value EMS for noncoincident peak heating mass flow rate [kg/s]
		Real64 DesMainVolFlow; // design main supply duct volume flow [m3/s]
		bool EMSOverrideDesMainVolFlowOn; // If true, EMS is acting to change DesMainVolFlow
		Real64 EMSValueDesMainVolFlow; // Value EMS providing for design main supply duct volume flow [m3/s]
		Real64 DesHeatVolFlow; // design heat supply duct volume flow [m3/s]
		bool EMSOverrideDesHeatVolFlowOn; // If true, EMS is acting to change DesCoolVolFlow
		Real64 EMSValueDesHeatVolFlow; // Value EMS providing for design cool  supply duct volume flow [m3/s]
		Real64 DesCoolVolFlow; // design cool  supply duct volume flow [m3/s]
		bool EMSOverrideDesCoolVolFlowOn; // If true, EMS is acting to change DesCoolVolFlow
		Real64 EMSValueDesCoolVolFlow; // Value EMS providing for design cool  supply duct volume flow [m3/s]
		Real64 SensCoolCap; // design sensible cooling capacity [W]
		Real64 HeatCap; // design heating capacity [W]
		Real64 PreheatCap; // design preheat capacity [W]
		Real64 CoolMixTemp; // design mixed air temperature for cooling [C]
		Real64 CoolMixHumRat; // design mixed air hum ratio for cooling [kg water/kg dry air]
		Real64 CoolRetTemp; // design return air temperature for cooling [C]
		Real64 CoolRetHumRat; // design return air hum ratio for cooling [kg water/kg dry air]
		Real64 CoolOutTemp; // design outside air temperature for cooling [C]
		Real64 CoolOutHumRat; // design outside air hum ratio for cooling [kg water/kg dry air]
		Real64 HeatMixTemp; // design mixed air temperature for heating [C]
		Real64 HeatMixHumRat; // design mixed air hum ratio for heating [kg water/kg dry air]
		Real64 HeatRetTemp; // design return air temperature for heating [C]
		Real64 HeatRetHumRat; // design return air hum ratio for heating [kg water/kg dry air]
		Real64 HeatOutTemp; // design outside air temperature for heating [C]
		Real64 HeatOutHumRat; // design outside air hum ratio for Heating [kg water/kg dry air]
		Real64 DesCoolVolFlowMin; // design minimum system cooling flow rate [m3/s]
		FArray1D< Real64 > HeatFlowSeq; // daily sequence of system heating air mass flow rate
		//  (zone time step)
		FArray1D< Real64 > CoolFlowSeq; // daily sequence of system cooling air mass flow rate
		//  (zone time step)
		FArray1D< Real64 > SensCoolCapSeq; // daily sequence of system sensible cooling capacity
		//  (zone time step)
		FArray1D< Real64 > HeatCapSeq; // daily sequence of system heating capacity [zone time step]
		FArray1D< Real64 > PreheatCapSeq; // daily sequence of system preheat capacity [zone time step]
		FArray1D< Real64 > SysCoolRetTempSeq; // daily sequence of system cooling return temperatures [C]
		//  [zone time step]
		FArray1D< Real64 > SysCoolRetHumRatSeq; // daily sequence of system cooling return humidity ratios
		//  [kg water/kg dry air] [zone time step]
		FArray1D< Real64 > SysHeatRetTempSeq; // daily sequence of system heating return temperatures [C]
		//   [zone time step]
		FArray1D< Real64 > SysHeatRetHumRatSeq; // daily sequence of system heating return humidity ratios
		//  [kg water/kg dry air] [zone time step]
		FArray1D< Real64 > SysCoolOutTempSeq; // daily sequence of system cooling outside temperatures [C]
		//  [zone time step]
		FArray1D< Real64 > SysCoolOutHumRatSeq; // daily sequence of system cooling outside humidity ratios
		//  [kg water/kg dry air] [zone time step]
		FArray1D< Real64 > SysHeatOutTempSeq; // daily sequence of system heating outside temperatures [C]
		//  [zone time step]
		FArray1D< Real64 > SysHeatOutHumRatSeq; // daily sequence of system heating outside humidity ratios
		//   [kg water/kg dry air] [zone time step]
		int SystemOAMethod; // System Outdoor Air Method; 1 = SOAM_ZoneSum, 2 = SOAM_VRP
		Real64 MaxZoneOAFraction; // maximum value of min OA for zones served by system
		Real64 SysUncOA; // uncorrected system outdoor air flow based on zone people and zone area
		bool OAAutoSized; // Set to true if design OA vol flow is set to 'autosize'
		int ScaleCoolSAFMethod; // choice of how to get system cooling scalable air flow rates; (FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FlowPerCoolingCapacity)
		int ScaleHeatSAFMethod; // choice of how to get system heating scalable air flow rates; (FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow, FlowPerHeatingCapacity)
		int CoolingCapMethod; // - Method for cooling capacity scaledsizing calculation (CoolingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity)
		int HeatingCapMethod; // - Method for heatiing capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 ScaledCoolingCapacity; // - scaled maximum cooling capacity of cooling coil in an air loop
		Real64 ScaledHeatingCapacity; // - scaled maximum heating capacity of cooling coil in an air loop
		Real64 FloorAreaOnAirLoopCooled; // total floor of cooled zones served by an airloop
		Real64 FloorAreaOnAirLoopHeated; // total floor of heated zones served by an airloop
		Real64 FlowPerFloorAreaCooled; // ratio of cooling supply air flow rate to total floor area of cooled zones served by an airloop
		Real64 FlowPerFloorAreaHeated; // ratio of cooling supply air flow rate to total floor area of cooled zones served by an airloop
		Real64 FractionOfAutosizedCoolingAirflow; // fraction of of cooling supply air flow rate an airloop
		Real64 FractionOfAutosizedHeatingAirflow; // fraction of of heating supply air flow rate an airloop
		Real64 FlowPerCoolingCapacity; // ratio of cooling supply air flow rate to cooling capacity of an airloop
		Real64 FlowPerHeatingCapacity; // ratio of heating supply air flow rate to heating capacity of an airloop
		Real64 FractionOfAutosizedCoolingCapacity; // fraction of of cooling total capacity
		Real64 FractionOfAutosizedHeatingCapacity; // fraction of of heating total capacity
		Real64 CoolingTotalCapacity; // system total cooling capacity
		Real64 HeatingTotalCapacity; // system total heating capacity

		// in Sizing:System

		// Default Constructor
		SystemSizingData() :
			LoadSizeType( 0 ),
			SizingOption( 0 ),
			CoolOAOption( 0 ),
			HeatOAOption( 0 ),
			DesOutAirVolFlow( 0.0 ),
			SysAirMinFlowRat( 0.0 ),
			PreheatTemp( 0.0 ),
			PrecoolTemp( 0.0 ),
			PreheatHumRat( 0.0 ),
			PrecoolHumRat( 0.0 ),
			CoolSupTemp( 0.0 ),
			HeatSupTemp( 0.0 ),
			CoolSupHumRat( 0.0 ),
			HeatSupHumRat( 0.0 ),
			CoolAirDesMethod( 0 ),
			HeatAirDesMethod( 0 ),
			InpDesCoolAirFlow( 0.0 ),
			InpDesHeatAirFlow( 0.0 ),
			CoinCoolMassFlow( 0.0 ),
			EMSOverrideCoinCoolMassFlowOn( false ),
			EMSValueCoinCoolMassFlow( 0.0 ),
			CoinHeatMassFlow( 0.0 ),
			EMSOverrideCoinHeatMassFlowOn( false ),
			EMSValueCoinHeatMassFlow( 0.0 ),
			NonCoinCoolMassFlow( 0.0 ),
			EMSOverrideNonCoinCoolMassFlowOn( false ),
			EMSValueNonCoinCoolMassFlow( 0.0 ),
			NonCoinHeatMassFlow( 0.0 ),
			EMSOverrideNonCoinHeatMassFlowOn( false ),
			EMSValueNonCoinHeatMassFlow( 0.0 ),
			DesMainVolFlow( 0.0 ),
			EMSOverrideDesMainVolFlowOn( false ),
			EMSValueDesMainVolFlow( 0.0 ),
			DesHeatVolFlow( 0.0 ),
			EMSOverrideDesHeatVolFlowOn( false ),
			EMSValueDesHeatVolFlow( 0.0 ),
			DesCoolVolFlow( 0.0 ),
			EMSOverrideDesCoolVolFlowOn( false ),
			EMSValueDesCoolVolFlow( 0.0 ),
			SensCoolCap( 0.0 ),
			HeatCap( 0.0 ),
			PreheatCap( 0.0 ),
			CoolMixTemp( 0.0 ),
			CoolMixHumRat( 0.0 ),
			CoolRetTemp( 0.0 ),
			CoolRetHumRat( 0.0 ),
			CoolOutTemp( 0.0 ),
			CoolOutHumRat( 0.0 ),
			HeatMixTemp( 0.0 ),
			HeatMixHumRat( 0.0 ),
			HeatRetTemp( 0.0 ),
			HeatRetHumRat( 0.0 ),
			HeatOutTemp( 0.0 ),
			HeatOutHumRat( 0.0 ),
			DesCoolVolFlowMin( 0.0 ),
			SystemOAMethod( 0 ),
			MaxZoneOAFraction( 0.0 ),
			SysUncOA( 0.0 ),
			OAAutoSized( false ),
			ScaleCoolSAFMethod( 0 ),
			ScaleHeatSAFMethod( 0 ),
			CoolingCapMethod( 0 ),
			HeatingCapMethod( 0 ),
			ScaledCoolingCapacity( 0.0 ),
			ScaledHeatingCapacity( 0.0 ),
			FloorAreaOnAirLoopCooled( 0.0 ),
			FloorAreaOnAirLoopHeated( 0.0 ),
			FlowPerFloorAreaCooled( 0.0 ),
			FlowPerFloorAreaHeated( 0.0 ),
			FractionOfAutosizedCoolingAirflow( 1.0 ),
			FractionOfAutosizedHeatingAirflow( 1.0 ),
			FlowPerCoolingCapacity( 0.0 ),
			FlowPerHeatingCapacity( 0.0 ),
			FractionOfAutosizedCoolingCapacity( 1.0 ),
			FractionOfAutosizedHeatingCapacity( 1.0 ),
			CoolingTotalCapacity( 0.0 ),
			HeatingTotalCapacity( 0.0 )

		{}

		// Member Constructor
		SystemSizingData(
			std::string const & AirPriLoopName, // name of an AirLoopHVAC object
			std::string const & CoolDesDay, // name of a cooling design day
			std::string const & HeatDesDay, // name of a heating design day
			int const LoadSizeType, // type of load to size on;
			int const SizingOption, // 1 = noncoincident, 2 = coincident.
			int const CoolOAOption, // 1 = use 100% outside air; 2 = use min OA; for cooling sizing
			int const HeatOAOption, // 1 = use 100% outside air; 2 = use min OA; for heating sizing
			Real64 const DesOutAirVolFlow, // design (minimum) outside air flow rate [m3/s]
			Real64 const SysAirMinFlowRat, // minimum system air flow ratio
			Real64 const PreheatTemp, // preheat design set temperature
			Real64 const PrecoolTemp, // precool design set temperature [C]
			Real64 const PreheatHumRat, // preheat design humidity ratio [kg water/kg dry air]
			Real64 const PrecoolHumRat, // precool design humidity ratio [kg water/kg dry air]
			Real64 const CoolSupTemp, // cooling design supply air temperature [C]
			Real64 const HeatSupTemp, // heating design supply air temperature[C]
			Real64 const CoolSupHumRat, // cooling design supply air humidity ratio [kg water/kg dry air]
			Real64 const HeatSupHumRat, // heating design supply air humidity ratio [kg water/kg dry air]
			int const CoolAirDesMethod, // choice of how to get system design cooling air flow rates;
			int const HeatAirDesMethod, // choice of how to get system design heating air flow rates;
			Real64 const InpDesCoolAirFlow, // input design system supply air flow rate [m3/s]
			Real64 const InpDesHeatAirFlow, // input design system heating supply air flow rate [m3/s]
			Real64 const CoinCoolMassFlow, // coincident peak cooling mass flow rate [kg/s]
			bool const EMSOverrideCoinCoolMassFlowOn, // If true, EMS to change coincident peak cooling mass flow rate
			Real64 const EMSValueCoinCoolMassFlow, // Value EMS wants for coincident peak cooling mass flow rate [kg/s]
			Real64 const CoinHeatMassFlow, // coincident peak heating mass flow rate [kg/s]
			bool const EMSOverrideCoinHeatMassFlowOn, // If true, EMS to set coincident peak heating mass flow rate
			Real64 const EMSValueCoinHeatMassFlow, // Value EMS wants for coincident peak heating mass flow rate [kg/s]
			Real64 const NonCoinCoolMassFlow, // noncoincident peak cooling mass flow rate [kg/s]
			bool const EMSOverrideNonCoinCoolMassFlowOn, // true, EMS to set noncoincident peak cooling mass flow rate
			Real64 const EMSValueNonCoinCoolMassFlow, // Value EMS for noncoincident peak cooling mass flow rate [kg/s]
			Real64 const NonCoinHeatMassFlow, // noncoincident peak heating mass flow rate [kg/s]
			bool const EMSOverrideNonCoinHeatMassFlowOn, // true, EMS to set noncoincident peak heating mass flow rate
			Real64 const EMSValueNonCoinHeatMassFlow, // Value EMS for noncoincident peak heating mass flow rate [kg/s]
			Real64 const DesMainVolFlow, // design main supply duct volume flow [m3/s]
			bool const EMSOverrideDesMainVolFlowOn, // If true, EMS is acting to change DesMainVolFlow
			Real64 const EMSValueDesMainVolFlow, // Value EMS providing for design main supply duct volume flow [m3/s]
			Real64 const DesHeatVolFlow, // design heat supply duct volume flow [m3/s]
			bool const EMSOverrideDesHeatVolFlowOn, // If true, EMS is acting to change DesCoolVolFlow
			Real64 const EMSValueDesHeatVolFlow, // Value EMS providing for design cool  supply duct volume flow [m3/s]
			Real64 const DesCoolVolFlow, // design cool  supply duct volume flow [m3/s]
			bool const EMSOverrideDesCoolVolFlowOn, // If true, EMS is acting to change DesCoolVolFlow
			Real64 const EMSValueDesCoolVolFlow, // Value EMS providing for design cool  supply duct volume flow [m3/s]
			Real64 const SensCoolCap, // design sensible cooling capacity [W]
			Real64 const HeatCap, // design heating capacity [W]
			Real64 const PreheatCap, // design preheat capacity [W]
			Real64 const CoolMixTemp, // design mixed air temperature for cooling [C]
			Real64 const CoolMixHumRat, // design mixed air hum ratio for cooling [kg water/kg dry air]
			Real64 const CoolRetTemp, // design return air temperature for cooling [C]
			Real64 const CoolRetHumRat, // design return air hum ratio for cooling [kg water/kg dry air]
			Real64 const CoolOutTemp, // design outside air temperature for cooling [C]
			Real64 const CoolOutHumRat, // design outside air hum ratio for cooling [kg water/kg dry air]
			Real64 const HeatMixTemp, // design mixed air temperature for heating [C]
			Real64 const HeatMixHumRat, // design mixed air hum ratio for heating [kg water/kg dry air]
			Real64 const HeatRetTemp, // design return air temperature for heating [C]
			Real64 const HeatRetHumRat, // design return air hum ratio for heating [kg water/kg dry air]
			Real64 const HeatOutTemp, // design outside air temperature for heating [C]
			Real64 const HeatOutHumRat, // design outside air hum ratio for Heating [kg water/kg dry air]
			Real64 const DesCoolVolFlowMin, // design minimum system cooling flow rate [m3/s]
			FArray1< Real64 > const & HeatFlowSeq, // daily sequence of system heating air mass flow rate
			FArray1< Real64 > const & CoolFlowSeq, // daily sequence of system cooling air mass flow rate
			FArray1< Real64 > const & SensCoolCapSeq, // daily sequence of system sensible cooling capacity
			FArray1< Real64 > const & HeatCapSeq, // daily sequence of system heating capacity [zone time step]
			FArray1< Real64 > const & PreheatCapSeq, // daily sequence of system preheat capacity [zone time step]
			FArray1< Real64 > const & SysCoolRetTempSeq, // daily sequence of system cooling return temperatures [C]
			FArray1< Real64 > const & SysCoolRetHumRatSeq, // daily sequence of system cooling return humidity ratios
			FArray1< Real64 > const & SysHeatRetTempSeq, // daily sequence of system heating return temperatures [C]
			FArray1< Real64 > const & SysHeatRetHumRatSeq, // daily sequence of system heating return humidity ratios
			FArray1< Real64 > const & SysCoolOutTempSeq, // daily sequence of system cooling outside temperatures [C]
			FArray1< Real64 > const & SysCoolOutHumRatSeq, // daily sequence of system cooling outside humidity ratios
			FArray1< Real64 > const & SysHeatOutTempSeq, // daily sequence of system heating outside temperatures [C]
			FArray1< Real64 > const & SysHeatOutHumRatSeq, // daily sequence of system heating outside humidity ratios
			int const SystemOAMethod, // System Outdoor Air Method; 1 = SOAM_ZoneSum, 2 = SOAM_VRP
			Real64 const MaxZoneOAFraction, // maximum value of min OA for zones served by system
			Real64 const SysUncOA, // uncorrected system outdoor air flow based on zone people and
			bool const OAAutoSized, // Set to true if design OA vol flow is set to 'autosize'
			int const ScaleCoolSAFMethod, // choice of how to get system cooling scalable air flow rates; // (FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FlowPerCoolingCapacity)
			int const ScaleHeatSAFMethod, // choice of how to get system heating scalable air flow rates; // (FlowPerFloorArea, FractionOfAutosizedCoolingAirflow, FractionOfAutosizedHeatingAirflow, FlowPerHeatingCapacity)
			int const CoolingCapMethod, // - Method for cooling capacity scaledsizing calculation //- (CoolingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity)
			int const HeatingCapMethod, // - Method for heatiing capacity scaledsizing calculation //- (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
			Real64 const ScaledCoolingCapacity, // - scaled maximum cooling capacity of cooling coil in an air loop
			Real64 const ScaledHeatingCapacity, // - scaled maximum heating capacity of cooling coil in an air loop
			Real64 const FloorAreaOnAirLoopCooled, // total floor of cooled zones served by an airloop
			Real64 const FloorAreaOnAirLoopHeated, // total floor of heated zones served by an airloop
			Real64 const FlowPerFloorAreaCooled, // ratio of cooling supply air flow rate to total floor area of cooled zones served by an airloop
			Real64 const FlowPerFloorAreaHeated, // ratio of cooling supply air flow rate to total floor area of cooled zones served by an airloop
			Real64 const FractionOfAutosizedCoolingAirflow, // fraction of of cooling supply air flow rate an airloop
			Real64 const FractionOfAutosizedHeatingAirflow, // fraction of of heating supply air flow rate an airloop
			Real64 const FlowPerCoolingCapacity, // ratio of cooling supply air flow rate to cooling capacity of an airloop
			Real64 const FlowPerHeatingCapacity, // ratio of heating supply air flow rate to heating capacity of an airloop
			Real64 const FractionOfAutosizedCoolingCapacity, // fraction of of cooling total capacity
			Real64 const FractionOfAutosizedHeatingCapacity, // fraction of of heating total capacity
			Real64 const CoolingTotalCapacity, // system total cooling capacity
			Real64 const HeatingTotalCapacity // system total heating capacity
		) :
			AirPriLoopName( AirPriLoopName ),
			CoolDesDay( CoolDesDay ),
			HeatDesDay( HeatDesDay ),
			LoadSizeType( LoadSizeType ),
			SizingOption( SizingOption ),
			CoolOAOption( CoolOAOption ),
			HeatOAOption( HeatOAOption ),
			DesOutAirVolFlow( DesOutAirVolFlow ),
			SysAirMinFlowRat( SysAirMinFlowRat ),
			PreheatTemp( PreheatTemp ),
			PrecoolTemp( PrecoolTemp ),
			PreheatHumRat( PreheatHumRat ),
			PrecoolHumRat( PrecoolHumRat ),
			CoolSupTemp( CoolSupTemp ),
			HeatSupTemp( HeatSupTemp ),
			CoolSupHumRat( CoolSupHumRat ),
			HeatSupHumRat( HeatSupHumRat ),
			CoolAirDesMethod( CoolAirDesMethod ),
			HeatAirDesMethod( HeatAirDesMethod ),
			InpDesCoolAirFlow( InpDesCoolAirFlow ),
			InpDesHeatAirFlow( InpDesHeatAirFlow ),
			CoinCoolMassFlow( CoinCoolMassFlow ),
			EMSOverrideCoinCoolMassFlowOn( EMSOverrideCoinCoolMassFlowOn ),
			EMSValueCoinCoolMassFlow( EMSValueCoinCoolMassFlow ),
			CoinHeatMassFlow( CoinHeatMassFlow ),
			EMSOverrideCoinHeatMassFlowOn( EMSOverrideCoinHeatMassFlowOn ),
			EMSValueCoinHeatMassFlow( EMSValueCoinHeatMassFlow ),
			NonCoinCoolMassFlow( NonCoinCoolMassFlow ),
			EMSOverrideNonCoinCoolMassFlowOn( EMSOverrideNonCoinCoolMassFlowOn ),
			EMSValueNonCoinCoolMassFlow( EMSValueNonCoinCoolMassFlow ),
			NonCoinHeatMassFlow( NonCoinHeatMassFlow ),
			EMSOverrideNonCoinHeatMassFlowOn( EMSOverrideNonCoinHeatMassFlowOn ),
			EMSValueNonCoinHeatMassFlow( EMSValueNonCoinHeatMassFlow ),
			DesMainVolFlow( DesMainVolFlow ),
			EMSOverrideDesMainVolFlowOn( EMSOverrideDesMainVolFlowOn ),
			EMSValueDesMainVolFlow( EMSValueDesMainVolFlow ),
			DesHeatVolFlow( DesHeatVolFlow ),
			EMSOverrideDesHeatVolFlowOn( EMSOverrideDesHeatVolFlowOn ),
			EMSValueDesHeatVolFlow( EMSValueDesHeatVolFlow ),
			DesCoolVolFlow( DesCoolVolFlow ),
			EMSOverrideDesCoolVolFlowOn( EMSOverrideDesCoolVolFlowOn ),
			EMSValueDesCoolVolFlow( EMSValueDesCoolVolFlow ),
			SensCoolCap( SensCoolCap ),
			HeatCap( HeatCap ),
			PreheatCap( PreheatCap ),
			CoolMixTemp( CoolMixTemp ),
			CoolMixHumRat( CoolMixHumRat ),
			CoolRetTemp( CoolRetTemp ),
			CoolRetHumRat( CoolRetHumRat ),
			CoolOutTemp( CoolOutTemp ),
			CoolOutHumRat( CoolOutHumRat ),
			HeatMixTemp( HeatMixTemp ),
			HeatMixHumRat( HeatMixHumRat ),
			HeatRetTemp( HeatRetTemp ),
			HeatRetHumRat( HeatRetHumRat ),
			HeatOutTemp( HeatOutTemp ),
			HeatOutHumRat( HeatOutHumRat ),
			DesCoolVolFlowMin( DesCoolVolFlowMin ),
			HeatFlowSeq( HeatFlowSeq ),
			CoolFlowSeq( CoolFlowSeq ),
			SensCoolCapSeq( SensCoolCapSeq ),
			HeatCapSeq( HeatCapSeq ),
			PreheatCapSeq( PreheatCapSeq ),
			SysCoolRetTempSeq( SysCoolRetTempSeq ),
			SysCoolRetHumRatSeq( SysCoolRetHumRatSeq ),
			SysHeatRetTempSeq( SysHeatRetTempSeq ),
			SysHeatRetHumRatSeq( SysHeatRetHumRatSeq ),
			SysCoolOutTempSeq( SysCoolOutTempSeq ),
			SysCoolOutHumRatSeq( SysCoolOutHumRatSeq ),
			SysHeatOutTempSeq( SysHeatOutTempSeq ),
			SysHeatOutHumRatSeq( SysHeatOutHumRatSeq ),
			SystemOAMethod( SystemOAMethod ),
			MaxZoneOAFraction( MaxZoneOAFraction ),
			SysUncOA( SysUncOA ),
			OAAutoSized( OAAutoSized ),
			ScaleCoolSAFMethod( ScaleCoolSAFMethod ),
			ScaleHeatSAFMethod( ScaleHeatSAFMethod ),
			CoolingCapMethod( CoolingCapMethod ),
			HeatingCapMethod( HeatingCapMethod ),
			ScaledCoolingCapacity( ScaledCoolingCapacity ),
			ScaledHeatingCapacity( ScaledHeatingCapacity ),
			FloorAreaOnAirLoopCooled( FloorAreaOnAirLoopCooled ),
			FloorAreaOnAirLoopHeated( FloorAreaOnAirLoopHeated ),
			FlowPerFloorAreaCooled( FlowPerFloorAreaCooled ),
			FlowPerFloorAreaHeated( FlowPerFloorAreaHeated ),
			FractionOfAutosizedCoolingAirflow( FractionOfAutosizedCoolingAirflow ),
			FractionOfAutosizedHeatingAirflow( FractionOfAutosizedHeatingAirflow ),
			FlowPerCoolingCapacity( FlowPerCoolingCapacity ),
			FlowPerHeatingCapacity( FlowPerHeatingCapacity ),
			FractionOfAutosizedCoolingCapacity( FractionOfAutosizedCoolingCapacity ),
			FractionOfAutosizedHeatingCapacity( FractionOfAutosizedHeatingCapacity ),
			CoolingTotalCapacity( CoolingTotalCapacity ),
			HeatingTotalCapacity( HeatingTotalCapacity )
		{}
	};

	struct PlantSizingData
	{
		// Members
		std::string PlantLoopName; // name of PLANT LOOP or CONDENSER LOOP object
		int LoopType; // type of loop: 1=heating, 2=cooling, 3=condenser
		Real64 ExitTemp; // loop design exit (supply) temperature [C]
		Real64 DeltaT; // loop design temperature drop (or rise) [DelK]
		// Calculated
		Real64 DesVolFlowRate; // loop design flow rate in m3/s
		bool VolFlowSizingDone; // flag to indicate when this loop has finished sizing flow rate

		// Default Constructor
		PlantSizingData() :
			LoopType( 0 ),
			ExitTemp( 0.0 ),
			DeltaT( 0.0 ),
			DesVolFlowRate( 0.0 ),
			VolFlowSizingDone( false )
		{}

		// Member Constructor
		PlantSizingData(
			std::string const & PlantLoopName, // name of PLANT LOOP or CONDENSER LOOP object
			int const LoopType, // type of loop: 1=heating, 2=cooling, 3=condenser
			Real64 const ExitTemp, // loop design exit (supply) temperature [C]
			Real64 const DeltaT, // loop design temperature drop (or rise) [DelK]
			Real64 const DesVolFlowRate, // loop design flow rate in m3/s
			bool const VolFlowSizingDone // flag to indicate when this loop has finished sizing flow rate
		) :
			PlantLoopName( PlantLoopName ),
			LoopType( LoopType ),
			ExitTemp( ExitTemp ),
			DeltaT( DeltaT ),
			DesVolFlowRate( DesVolFlowRate ),
			VolFlowSizingDone( VolFlowSizingDone )
		{}

	};

	struct DesDayWeathData
	{
		// Members
		std::string DateString; // date of design day weather values
		FArray1D< Real64 > Temp; // design day temperatures at the major time step
		FArray1D< Real64 > HumRat; // design day humidity ratios at the major time step
		FArray1D< Real64 > Press; // design day braometric pressure at the major time step

		// Default Constructor
		DesDayWeathData()
		{}

		// Member Constructor
		DesDayWeathData(
			std::string const & DateString, // date of design day weather values
			FArray1< Real64 > const & Temp, // design day temperatures at the major time step
			FArray1< Real64 > const & HumRat, // design day humidity ratios at the major time step
			FArray1< Real64 > const & Press // design day braometric pressure at the major time step
		) :
			DateString( DateString ),
			Temp( Temp ),
			HumRat( HumRat ),
			Press( Press )
		{}

	};

	struct CompDesWaterFlowData // design water flow rate for components that use water as an
	{
		// Members
		//  energy source or sink
		int SupNode; // water inlet node number (condenser side for water / water)
		Real64 DesVolFlowRate; // water design flow rate [m3/s]

		// Default Constructor
		CompDesWaterFlowData() :
			SupNode( 0 ),
			DesVolFlowRate( 0.0 )
		{}

		// Member Constructor
		CompDesWaterFlowData(
			int const SupNode, // water inlet node number (condenser side for water / water)
			Real64 const DesVolFlowRate // water design flow rate [m3/s]
		) :
			SupNode( SupNode ),
			DesVolFlowRate( DesVolFlowRate )
		{}

	};

	struct OARequirementsData
	{
		// Members
		std::string Name;
		int OAFlowMethod; // - Method for OA flow calculation
		//- (Flow/Person, Flow/Zone, Flow/Area, FlowACH, Sum, Maximum)
		Real64 OAFlowPerPerson; // - OA requirement per person
		Real64 OAFlowPerArea; // - OA requirement per zone area
		Real64 OAFlowPerZone; // - OA requirement per zone
		Real64 OAFlowACH; // - OA requirement per zone per hour
		int OAFlowFracSchPtr; // - Fraction schedule applied to total OA requirement
		Real64 MaxOAFractionSchValue; // - Maximum value from OAFlow fraction schedule (used for sizing)

		// Default Constructor
		OARequirementsData() :
			OAFlowMethod( 0 ),
			OAFlowPerPerson( 0.0 ),
			OAFlowPerArea( 0.0 ),
			OAFlowPerZone( 0.0 ),
			OAFlowACH( 0.0 ),
			OAFlowFracSchPtr( 0 ),
			MaxOAFractionSchValue( 0.0 )
		{}

		// Member Constructor
		OARequirementsData(
			std::string const & Name,
			int const OAFlowMethod, // - Method for OA flow calculation
			Real64 const OAFlowPerPerson, // - OA requirement per person
			Real64 const OAFlowPerArea, // - OA requirement per zone area
			Real64 const OAFlowPerZone, // - OA requirement per zone
			Real64 const OAFlowACH, // - OA requirement per zone per hour
			int const OAFlowFracSchPtr, // - Fraction schedule applied to total OA requirement
			Real64 const MaxOAFractionSchValue // - Maximum value from OAFlow fraction schedule (used for sizing)
		) :
			Name( Name ),
			OAFlowMethod( OAFlowMethod ),
			OAFlowPerPerson( OAFlowPerPerson ),
			OAFlowPerArea( OAFlowPerArea ),
			OAFlowPerZone( OAFlowPerZone ),
			OAFlowACH( OAFlowACH ),
			OAFlowFracSchPtr( OAFlowFracSchPtr ),
			MaxOAFractionSchValue( MaxOAFractionSchValue )
		{}

	};

	struct ZoneAirDistributionData
	{
		// Members
		std::string Name;
		std::string ZoneADEffSchName; // - Zone air distribution effectiveness schedule name
		Real64 ZoneADEffCooling; // - Zone air distribution effectiveness in cooling mode
		Real64 ZoneADEffHeating; // - Zone air distribution effectiveness in heating mode
		Real64 ZoneSecondaryRecirculation; // - Zone air secondary recirculation ratio
		int ZoneADEffSchPtr; // - Zone air distribution effectiveness schedule index

		// Default Constructor
		ZoneAirDistributionData() :
			ZoneADEffCooling( 1.0 ),
			ZoneADEffHeating( 1.0 ),
			ZoneSecondaryRecirculation( 0.0 ),
			ZoneADEffSchPtr( 0 )
		{}

		// Member Constructor
		ZoneAirDistributionData(
			std::string const & Name,
			std::string const & ZoneADEffSchName, // - Zone air distribution effectiveness schedule name
			Real64 const ZoneADEffCooling, // - Zone air distribution effectiveness in cooling mode
			Real64 const ZoneADEffHeating, // - Zone air distribution effectiveness in heating mode
			Real64 const ZoneSecondaryRecirculation, // - Zone air secondary recirculation ratio
			int const ZoneADEffSchPtr // - Zone air distribution effectiveness schedule index
		) :
			Name( Name ),
			ZoneADEffSchName( ZoneADEffSchName ),
			ZoneADEffCooling( ZoneADEffCooling ),
			ZoneADEffHeating( ZoneADEffHeating ),
			ZoneSecondaryRecirculation( ZoneSecondaryRecirculation ),
			ZoneADEffSchPtr( ZoneADEffSchPtr )
		{}

	};

	// Object Data
	extern FArray1D< OARequirementsData > OARequirements;
	extern FArray1D< ZoneAirDistributionData > ZoneAirDistribution;
	extern FArray1D< ZoneSizingInputData > ZoneSizingInput; // Input data for zone sizing
	extern FArray2D< ZoneSizingData > ZoneSizing; // Data for zone sizing (all data, all design
	extern FArray1D< ZoneSizingData > FinalZoneSizing; // Final data for zone sizing including effects
	extern FArray2D< ZoneSizingData > CalcZoneSizing; // Data for zone sizing (all data,
	extern FArray1D< ZoneSizingData > CalcFinalZoneSizing; // Final data for zone sizing (calculated only)
	extern FArray1D< ZoneSizingData > TermUnitFinalZoneSizing; // Final data for sizing terminal units
	extern FArray1D< SystemSizingInputData > SysSizInput; // Input data array for system sizing object
	extern FArray2D< SystemSizingData > SysSizing; // Data array for system sizing (all data)
	extern FArray1D< SystemSizingData > FinalSysSizing; // Data array for system sizing (max heat/cool)
	extern FArray1D< SystemSizingData > CalcSysSizing; // Data array for system sizing (max heat/cool)
	extern FArray1D< TermUnitSizingData > TermUnitSizing; // Data added in sizing routines
	extern FArray1D< ZoneEqSizingData > ZoneEqSizing; // Data added in zone eq component sizing routines
	extern FArray1D< ZoneEqSizingData > UnitarySysEqSizing; // Data added in unitary system sizing routines
	extern FArray1D< ZoneEqSizingData > OASysEqSizing; // Data added in unitary system sizing routines
	extern FArray1D< PlantSizingData > PlantSizData; // Input data array for plant sizing
	extern FArray1D< DesDayWeathData > DesDayWeath; // design day weather saved at major time step
	extern FArray1D< CompDesWaterFlowData > CompDesWaterFlow; // array to store components' design water flow
	extern FArray1D< ZoneHVACSizingData > ZoneHVACSizing; // Input data for zone HVAC sizing

} // DataSizing

} // EnergyPlus

#endif
