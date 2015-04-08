#ifndef DataRoomAirModel_hh_INCLUDED
#define DataRoomAirModel_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataRoomAirModel {

	// Using/Aliasing

	// Data
	// module should be available to other modules and routines.  Thus,
	// all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS
	extern std::string const cUserDefinedControlObject;
	extern std::string const cTempPatternConstGradientObject;
	extern std::string const cTempPatternTwoGradientObject;
	extern std::string const cTempPatternNDHeightObject;
	extern std::string const cTempPatternSurfMapObject;

	// Parameters to indicate room air model selected
	extern int const RoomAirModel_UserDefined; // user defined patterns
	extern int const RoomAirModel_Mixing; // mixing air model
	extern int const RoomAirModel_Mundt; // Mundt nodal model
	extern int const RoomAirModel_UCSDDV; // UCSD Displacement Ventilation model
	extern int const RoomAirModel_UCSDCV; // UCSD-CV
	extern int const RoomAirModel_UCSDUFI; // UCSD UFAD interior zone model
	extern int const RoomAirModel_UCSDUFE; // UCSD UFAD interior zone model
	extern Array1D_string const ChAirModel;

	// Parameters to indicate air temperature coupling scheme
	extern int const DirectCoupling; // direct coupling scheme
	extern int const IndirectCoupling; // indirect coupling scheme

	// Parameters to indicate type of air node, which is dependent on air models
	extern int const InletAirNode; // air node at inlet (for Mundt and Rees&Haves Models)
	extern int const FloorAirNode; // air node at floor (for Mundt and Rees&Haves Models)
	extern int const ControlAirNode; // air node at control point (for Mundt Model)
	extern int const CeilingAirNode; // air node at ceiling (for Mundt Model)
	extern int const MundtRoomAirNode; // air node for vertical walls (for Mundt Model)
	extern int const ReturnAirNode; // air node for return (for Mundt and Rees&Haves Models)
	extern int const PlumeAirNode1; // air node for plume load (for Rees&Haves Model)
	extern int const PlumeAirNode2; // air node for plume load (for Rees&Haves Model)
	extern int const PlumeAirNode3; // air node for plume load (for Rees&Haves Model)
	extern int const PlumeAirNode4; // air node for plume load (for Rees&Haves Model)
	extern int const RoomAirNode1; // air node for vertical walls (for Rees&Haves Model)
	extern int const RoomAirNode2; // air node for vertical walls (for Rees&Haves Model)
	extern int const RoomAirNode3; // air node for vertical walls (for Rees&Haves Model)
	extern int const RoomAirNode4; // air node for vertical walls (for Rees&Haves Model)

	// user-defined pattern two gradient interplotation modes
	extern int const OutdoorDryBulbMode; // by outdoor air bulb.
	extern int const SensibleCoolingMode; // by sensible cooling load
	extern int const SensibleHeatingMode; // by sensible heating load
	extern int const ZoneAirTempMode; // by zone air temperature
	extern int const DeltaOutdoorZone; // by difference between zone and outdoor

	// user defined temperature pattern types
	extern int const ConstGradTempPattern; // constant gradient in vertical direction
	extern int const TwoGradInterpPattern; // two gradient interpolation
	extern int const NonDimenHeightPattern; // non-dimensionalized height
	extern int const SurfMapTempPattern; // arbitrary surface mappings

	// Parameters to indicate type of control for the UCSD UFAD interior zone model
	// INTEGER, PARAMETER :: ConsFlow          = 1     ! constant supply air flow
	// INTEGER, PARAMETER :: VarFlowConsPress  = 2     ! variable supply air flow, constant supply plenum pressure
	// INTEGER, PARAMETER :: VarFlowVarPress   = 3     ! variable supply air flow, variable supply plenum pressure

	// parameters to indicate diffuser type
	extern int const Swirl;
	extern int const VarArea;
	extern int const DisplVent;
	extern int const LinBarGrille;
	extern int const Custom;

	// parameters for comfort calculations
	extern int const VComfort_Invalid;
	extern int const VComfort_Jet;
	extern int const VComfort_Recirculation;

	// DERIVED TYPE DEFINITIONS

	// Air Node Data

	// UCSD

	// END UCSD

	// begin NREL RoomAir DERIVED TYPES ******************************************

	// end NREL room air derived types*********************************

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern int TotNumOfAirNodes;
	extern Array1D_int TotNumOfZoneAirNodes;
	extern Array1D< Real64 > ConvectiveFloorSplit;
	extern Array1D< Real64 > InfiltratFloorSplit;
	// UCSD
	extern Array1D< Real64 > DVHcIn;
	extern int TotUCSDDV; // Total number of UCSDDV zones
	extern Array1D_bool IsZoneDV; // Is the air model for the zone UCSDDV?
	extern Array1D< Real64 > ZTOC; // Temperature of occupied (lower) zone
	extern Array1D< Real64 > AvgTempGrad; // vertical Average Temperature Gradient in the room
	extern Array1D< Real64 > ZTMX; // Temperature of the mixing(upper) layer
	extern Array1D< Real64 > MaxTempGrad; // maximum Average Temperature Gradient in the room
	extern Array1D< Real64 > HVACAirTemp; // HVAC system temperature (DEG C)
	extern Array1D< Real64 > HVACMassFlow; // HVAC system mass flow rate (KG/S)
	extern Array1D< Real64 > ZTFloor;
	extern Array1D< Real64 > HeightTransition;
	extern Array1D< Real64 > FracMinFlow;
	extern Array1D_int ZoneDVMixedFlag;
	extern Array1D< Real64 > ZoneDVMixedFlagRep;
	extern Array1D_bool ZoneAirSystemON;
	extern Array1D< Real64 > TCMF; // comfort temperature
	extern Array1D< Real64 > ZoneCeilingHeight;
	extern Array1D< Real64 > MATFloor; // [C] floor level mean air temp
	extern Array1D< Real64 > XMATFloor; // [C] floor level mean air temp at t minus 1 zone time step
	extern Array1D< Real64 > XM2TFloor; // [C] floor level mean air temp at t minus 2 zone time step
	extern Array1D< Real64 > XM3TFloor; // [C] floor level mean air temp at t minus 3 zone time step
	extern Array1D< Real64 > XM4TFloor; // [C] floor level mean air temp at t minus 4 zone time step
	extern Array1D< Real64 > DSXMATFloor; // [C] floor level mean air temp at t minus 1 system time step
	extern Array1D< Real64 > DSXM2TFloor; // [C] floor level mean air temp at t minus 2 system time step
	extern Array1D< Real64 > DSXM3TFloor; // [C] floor level mean air temp at t minus 3 system time step
	extern Array1D< Real64 > DSXM4TFloor; // [C] floor level mean air temp at t minus 4 system time step
	extern Array1D< Real64 > MATOC; // [C] occupied mean air temp
	extern Array1D< Real64 > XMATOC; // [C] occupied mean air temp at t minus 1 zone time step
	extern Array1D< Real64 > XM2TOC; // [C] occupied mean air temp at t minus 2 zone time step
	extern Array1D< Real64 > XM3TOC; // [C] occupied mean air temp at t minus 3 zone time step
	extern Array1D< Real64 > XM4TOC; // [C] occupied mean air temp at t minus 4 zone time step
	extern Array1D< Real64 > DSXMATOC; // [C] occupied mean air temp at t minus 1 system time step
	extern Array1D< Real64 > DSXM2TOC; // [C] occupied mean air temp at t minus 2 system time step
	extern Array1D< Real64 > DSXM3TOC; // [C] occupied mean air temp at t minus 3 system time step
	extern Array1D< Real64 > DSXM4TOC; // [C] occupied mean air temp at t minus 4 system time step
	extern Array1D< Real64 > MATMX; // [C] mixed (upper) mean air temp
	extern Array1D< Real64 > XMATMX; // [C] mixed (upper) mean air temp at t minus 1 zone time step
	extern Array1D< Real64 > XM2TMX; // [C] mixed (upper) mean air temp at t minus 2 zone time step
	extern Array1D< Real64 > XM3TMX; // [C] mixed (upper) mean air temp at t minus 3 zone time step
	extern Array1D< Real64 > XM4TMX; // [C] mixed (upper) mean air temp at t minus 4 zone time step
	extern Array1D< Real64 > DSXMATMX; // [C] mixed  mean air temp at t minus 1 system time step
	extern Array1D< Real64 > DSXM2TMX; // [C] mixed  mean air temp at t minus 2 system time step
	extern Array1D< Real64 > DSXM3TMX; // [C] mixed  mean air temp at t minus 3 system time step
	extern Array1D< Real64 > DSXM4TMX; // [C] mixed  mean air temp at t minus 4 system time step
	extern Array1D< Real64 > ZTM1Floor; // [C] difference equation's Floor air temp at t minus 1
	extern Array1D< Real64 > ZTM2Floor; // [C] difference equation's Floor air temp at t minus 2
	extern Array1D< Real64 > ZTM3Floor; // [C] difference equation's Floor air temp at t minus 3
	extern Array1D< Real64 > ZTM1OC; // [C] difference equation's Occupied air temp at t minus 1
	extern Array1D< Real64 > ZTM2OC; // [C] difference equation's Occupied air temp at t minus 2
	extern Array1D< Real64 > ZTM3OC; // [C] difference equation's Occupied air temp at t minus 3
	extern Array1D< Real64 > ZTM1MX; // [C] difference equation's Mixed  air temp at t minus 1
	extern Array1D< Real64 > ZTM2MX; // [C] difference equation's Mixed  air temp at t minus 1
	extern Array1D< Real64 > ZTM3MX; // [C] difference equation's Mixed  air temp at t minus 1
	extern Array1D< Real64 > AIRRATFloor;
	extern Array1D< Real64 > AIRRATOC;
	extern Array1D< Real64 > AIRRATMX;
	// Euler and Exact solution algorithms
	extern Array1D< Real64 > Zone1Floor; // [C] difference equation's Floor air temp at previous dt
	extern Array1D< Real64 > ZoneMXFloor; // [C] difference equation's Floor air temp at t minus 1
	extern Array1D< Real64 > ZoneM2Floor; // [C] difference equation's Floor air temp at t minus 2
	extern Array1D< Real64 > Zone1OC; // [C] difference equation's Occupied air temp at previous dt
	extern Array1D< Real64 > ZoneMXOC; // [C] difference equation's Occupied air temp at t minus 1
	extern Array1D< Real64 > ZoneM2OC; // [C] difference equation's Occupied air temp at t minus 2
	extern Array1D< Real64 > Zone1MX; // [C] difference equation's Mixed  air temp at previous dt
	extern Array1D< Real64 > ZoneMXMX; // [C] difference equation's Mixed  air temp at t minus 1
	extern Array1D< Real64 > ZoneM2MX; // [C] difference equation's Mixed  air temp at t minus 2
	// UCSD-CV
	extern Array1D< Real64 > CVHcIn;
	extern int TotUCSDCV; // Total number of UCSDDV zones
	extern Array1D_bool IsZoneCV; // Is the air model for the zone UCSDDV?
	extern Array1D< Real64 > ZoneCVisMixing; // Zone set to CV is actually using a mixing model
	extern Array1D< Real64 > ZTJET; // Jet Temperatures
	extern Array1D< Real64 > ZTREC; // Recirculation Temperatures
	extern Array1D< Real64 > RoomOutflowTemp; // Temperature of air flowing out of the room
	extern Array1D< Real64 > JetRecAreaRatio;
	extern Array1D< Real64 > Urec; // Recirculation region average velocity
	extern Array1D< Real64 > Ujet; // Jet region average velocity
	extern Array1D< Real64 > Qrec; // Recirculation zone total flow rate
	extern Array1D< Real64 > Qtot; // Total volumetric inflow rate through all active aperatures [m3/s]
	extern Array1D< Real64 > RecInflowRatio; // Ratio of the recirculation volumetric flow rate to the total inflow flow rate []
	extern Array1D< Real64 > Uhc;
	extern Array1D< Real64 > Ain; // Inflow aperture area
	extern Array1D< Real64 > Droom; // CV Zone average length
	extern Array1D< Real64 > Dstar; // CV Zone average length, wind direction corrected
	extern Array1D< Real64 > Tin; // Inflow air temperature
	extern Array1D< Real64 > TotArea; // Sum of the areas of all apertures in the zone
	extern Array2D_int AirflowNetworkSurfaceUCSDCV; // table for AirflowNetwork surfaces organization
	extern int CVNumAirflowNetworkSurfaces; // total number of AirFlowNetwork surfaces.
	// Interzone surfaces counts twice.
	extern Array1D< Real64 > Rfr; // Ration between inflow and recirculation air flows
	extern Array1D< Real64 > ZoneCVhasREC; // Airflow pattern is C(0), CR(1)
	extern bool UCSDModelUsed;
	extern bool MundtModelUsed;
	// UCSD-UF
	extern int TotUCSDUI; // total number of UCSDUI zones
	extern int TotUCSDUE; // total number of UCSDUE zones
	extern Array1D_bool IsZoneUI; // controls program flow, for interior or exterior UFAD model
	extern Array1D_int ZoneUFPtr;
	extern Array1D< Real64 > UFHcIn;
	extern Array1D_int ZoneUFMixedFlag;
	extern Array1D< Real64 > ZoneUFMixedFlagRep;
	extern Array1D< Real64 > ZoneUFGamma;
	extern Array1D< Real64 > ZoneUFPowInPlumes; // [W]
	extern Array1D< Real64 > ZoneUFPowInPlumesfromWindows; // [W]
	extern Array1D< Real64 > Phi; // dimensionless measure of occupied subzone temperature

	// END UCSD

	// Begin NREL User-defined patterns

	extern int numTempDistContrldZones; // count of zones with user-defined patterns
	extern int NumAirTempPatterns; // count of all different patterns in input file
	extern int NumConstantGradient; // count of constant gradient patterns in input
	extern int NumTwoGradientInterp; // count of two gradient interp patterns in input
	extern int NumNonDimensionalHeight; // count of ND height profile patterns in input
	extern int NumSurfaceMapping; // count of generic surface map patterns in input

	extern bool UserDefinedUsed; // true if user-defined model used anywhere
	// End User-defined patterns

	// Types

	struct AirModelData
	{
		// Members
		std::string AirModelName;
		std::string ZoneName;
		int ZonePtr; // Pointer to the zone number for this statement
		int AirModelType; // 1 = Mixing, 2 = Mundt, 3 = Rees and Haves,
		// 4 = UCSDDV, 5 = UCSDCV, -1 = user defined
		// 6 = UCSDUFI
		int TempCoupleScheme; // 1 = absolute (direct),
		// 2 = relative air model temperature passing scheme (indirect)
		bool SimAirModel; // FALSE if Mixing air model is currently used and
		// TRUE if other air models are currently used

		// Default Constructor
		AirModelData() :
			ZonePtr( 0 ),
			AirModelType( RoomAirModel_Mixing ),
			TempCoupleScheme( DirectCoupling ),
			SimAirModel( false )
		{}

		// Member Constructor
		AirModelData(
			std::string const & AirModelName,
			std::string const & ZoneName,
			int const ZonePtr, // Pointer to the zone number for this statement
			int const AirModelType, // 1 = Mixing, 2 = Mundt, 3 = Rees and Haves,
			int const TempCoupleScheme, // 1 = absolute (direct),
			bool const SimAirModel // FALSE if Mixing air model is currently used and
		) :
			AirModelName( AirModelName ),
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			AirModelType( AirModelType ),
			TempCoupleScheme( TempCoupleScheme ),
			SimAirModel( SimAirModel )
		{}

	};

	struct AirNodeData
	{
		// Members
		std::string Name; // name
		std::string ZoneName;
		int ZonePtr; // Pointer to the zone number for this statement
		int ClassType; // depending on type of model
		Real64 Height; // height
		Array1D_bool SurfMask; // limit of 60 surfaces at current sizing

		// Default Constructor
		AirNodeData() :
			ZonePtr( 0 ),
			ClassType( 0 ),
			Height( 0.0 )
		{}

		// Member Constructor
		AirNodeData(
			std::string const & Name, // name
			std::string const & ZoneName,
			int const ZonePtr, // Pointer to the zone number for this statement
			int const ClassType, // depending on type of model
			Real64 const Height, // height
			Array1_bool const & SurfMask // limit of 60 surfaces at current sizing
		) :
			Name( Name ),
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			ClassType( ClassType ),
			Height( Height ),
			SurfMask( SurfMask )
		{}

	};

	struct DVData
	{
		// Members
		std::string ZoneName; // Name of zone
		int ZonePtr; // Pointer to the zone number for this statement
		int SchedGainsPtr; // Schedule for internal gain fraction to occupied zone
		std::string SchedGainsName; // Gains Schedule name
		Real64 NumPlumesPerOcc; // Effective number of plumes per occupant
		Real64 ThermostatHeight; // Height of thermostat/ temperature control sensor
		Real64 ComfortHeight; // Height at which air temperature is measured for comfort purposes
		Real64 TempTrigger; // Minimum temperature difference between TOC TMX for stratification

		// Default Constructor
		DVData() :
			ZonePtr( 0 ),
			SchedGainsPtr( -1 ),
			NumPlumesPerOcc( 1.0 ),
			ThermostatHeight( 0.0 ),
			ComfortHeight( 0.0 ),
			TempTrigger( 0.0 )
		{}

		// Member Constructor
		DVData(
			std::string const & ZoneName, // Name of zone
			int const ZonePtr, // Pointer to the zone number for this statement
			int const SchedGainsPtr, // Schedule for internal gain fraction to occupied zone
			std::string const & SchedGainsName, // Gains Schedule name
			Real64 const NumPlumesPerOcc, // Effective number of plumes per occupant
			Real64 const ThermostatHeight, // Height of thermostat/ temperature control sensor
			Real64 const ComfortHeight, // Height at which air temperature is measured for comfort purposes
			Real64 const TempTrigger // Minimum temperature difference between TOC TMX for stratification
		) :
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			SchedGainsPtr( SchedGainsPtr ),
			SchedGainsName( SchedGainsName ),
			NumPlumesPerOcc( NumPlumesPerOcc ),
			ThermostatHeight( ThermostatHeight ),
			ComfortHeight( ComfortHeight ),
			TempTrigger( TempTrigger )
		{}

	};

	struct CVData
	{
		// Members
		std::string ZoneName; // Name of zone
		int ZonePtr; // Pointer to the zone number for this statement
		int SchedGainsPtr; // Schedule for internal gain fraction to occupied zone
		std::string SchedGainsName; // Gains Schedule name
		int VforComfort; // Use Recirculation or Jet velocity and temperatures
		// for comfort models

		// Default Constructor
		CVData() :
			ZonePtr( -1 ),
			SchedGainsPtr( -1 ),
			VforComfort( VComfort_Invalid )
		{}

		// Member Constructor
		CVData(
			std::string const & ZoneName, // Name of zone
			int const ZonePtr, // Pointer to the zone number for this statement
			int const SchedGainsPtr, // Schedule for internal gain fraction to occupied zone
			std::string const & SchedGainsName, // Gains Schedule name
			int const VforComfort // Use Recirculation or Jet velocity and temperatures
		) :
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			SchedGainsPtr( SchedGainsPtr ),
			SchedGainsName( SchedGainsName ),
			VforComfort( VforComfort )
		{}

	};

	struct CVFlow
	{
		// Members
		int FlowFlag; // Equal to 1 if the opening has inflow, else equal to 0.
		Real64 Width; // Width of the opening [m]
		Real64 Area; // Area of the opening [m2]
		Real64 Fin; // Inflow volume flux through the opening [m3/s]
		Real64 Uin; // Inflow air velocity through the opening [m/s]
		Real64 Vjet; // Average maximum jet velocity for the opening [m/s]
		Real64 Yjet; // Y in "Y = aX + b" formula
		Real64 Ujet; // Volume average jet region velocity [m/s]
		Real64 Yrec; // Y in "Y = aX + b" formula
		Real64 Urec; // Area-averaged velocity in the y-z plane with maximum flow [m/s]
		Real64 YQrec; // Y in "Y = aX + b" formula
		Real64 Qrec; // Total flow rate for the recirculation regions in the plane of maximum flow [m3/s]

		// Default Constructor
		CVFlow() :
			FlowFlag( 0 ),
			Width( 0.0 ),
			Area( 0.0 ),
			Fin( 0.0 ),
			Uin( 0.0 ),
			Vjet( 0.0 ),
			Yjet( 0.0 ),
			Ujet( 0.0 ),
			Yrec( 0.0 ),
			Urec( 0.0 ),
			YQrec( 0.0 ),
			Qrec( 0.0 )
		{}

		// Member Constructor
		CVFlow(
			int const FlowFlag, // Equal to 1 if the opening has inflow, else equal to 0.
			Real64 const Width, // Width of the opening [m]
			Real64 const Area, // Area of the opening [m2]
			Real64 const Fin, // Inflow volume flux through the opening [m3/s]
			Real64 const Uin, // Inflow air velocity through the opening [m/s]
			Real64 const Vjet, // Average maximum jet velocity for the opening [m/s]
			Real64 const Yjet, // Y in "Y = aX + b" formula
			Real64 const Ujet, // Volume average jet region velocity [m/s]
			Real64 const Yrec, // Y in "Y = aX + b" formula
			Real64 const Urec, // Area-averaged velocity in the y-z plane with maximum flow [m/s]
			Real64 const YQrec, // Y in "Y = aX + b" formula
			Real64 const Qrec // Total flow rate for the recirculation regions in the plane of maximum flow [m3/s]
		) :
			FlowFlag( FlowFlag ),
			Width( Width ),
			Area( Area ),
			Fin( Fin ),
			Uin( Uin ),
			Vjet( Vjet ),
			Yjet( Yjet ),
			Ujet( Ujet ),
			Yrec( Yrec ),
			Urec( Urec ),
			YQrec( YQrec ),
			Qrec( Qrec )
		{}

	};

	struct CVDVParameters
	{
		// Members
		Real64 Width;
		Real64 Height;
		int Shadow;
		Real64 Zmin;
		Real64 Zmax;

		// Default Constructor
		CVDVParameters() :
			Width( 0.0 ),
			Height( 0.0 ),
			Shadow( 0 ),
			Zmin( 0.0 ),
			Zmax( 0.0 )
		{}

		// Member Constructor
		CVDVParameters(
			Real64 const Width,
			Real64 const Height,
			int const Shadow,
			Real64 const Zmin,
			Real64 const Zmax
		) :
			Width( Width ),
			Height( Height ),
			Shadow( Shadow ),
			Zmin( Zmin ),
			Zmax( Zmax )
		{}

	};

	struct UFIData
	{
		// Members
		std::string ZoneName; // Name of zone
		int ZonePtr; // Pointer to the zone number for this statement
		int ZoneEquipPtr; // Pointer to zone equip for this UFAD zone
		Real64 DiffusersPerZone; // Number of diffusers in this zone
		Real64 PowerPerPlume; // Power in each plume [W]
		Real64 DiffArea; // Effective area of a diffuser [m2]
		Real64 DiffAngle; // angle between diffuser slots and vertical (degrees)
		Real64 HeatSrcHeight; // height of heat source above floor [m]
		Real64 ThermostatHeight; // Height of thermostat/ temperature control sensor [m]
		Real64 ComfortHeight; // Height at which air temperature is measured for
		// comfort purposes [m]
		Real64 TempTrigger; // Minimum temperature difference between TOC TMX
		// for stratification [deltaC]
		int DiffuserType; // 1=Swirl, 2=variable area, 3=displacement, 4=linear bar grille, 5=custom
		Real64 TransHeight; // user specified transition height [m]
		bool CalcTransHeight; // flag to calc trans height or use user specified input
		Real64 A_Kc; // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
		Real64 B_Kc; // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
		Real64 C_Kc; // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
		Real64 D_Kc; // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
		Real64 E_Kc; // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2

		// Default Constructor
		UFIData() :
			ZonePtr( 0 ),
			ZoneEquipPtr( 0 ),
			DiffusersPerZone( 0.0 ),
			PowerPerPlume( 0.0 ),
			DiffArea( 0.0 ),
			DiffAngle( 0.0 ),
			HeatSrcHeight( 0.0 ),
			ThermostatHeight( 0.0 ),
			ComfortHeight( 0.0 ),
			TempTrigger( 0.0 ),
			DiffuserType( 0 ),
			TransHeight( 0.0 ),
			CalcTransHeight( false ),
			A_Kc( 0.0 ),
			B_Kc( 0.0 ),
			C_Kc( 0.0 ),
			D_Kc( 0.0 ),
			E_Kc( 0.0 )
		{}

		// Member Constructor
		UFIData(
			std::string const & ZoneName, // Name of zone
			int const ZonePtr, // Pointer to the zone number for this statement
			int const ZoneEquipPtr, // Pointer to zone equip for this UFAD zone
			Real64 const DiffusersPerZone, // Number of diffusers in this zone
			Real64 const PowerPerPlume, // Power in each plume [W]
			Real64 const DiffArea, // Effective area of a diffuser [m2]
			Real64 const DiffAngle, // angle between diffuser slots and vertical (degrees)
			Real64 const HeatSrcHeight, // height of heat source above floor [m]
			Real64 const ThermostatHeight, // Height of thermostat/ temperature control sensor [m]
			Real64 const ComfortHeight, // Height at which air temperature is measured for
			Real64 const TempTrigger, // Minimum temperature difference between TOC TMX
			int const DiffuserType, // 1=Swirl, 2=variable area, 3=displacement, 4=linear bar grille, 5=custom
			Real64 const TransHeight, // user specified transition height [m]
			bool const CalcTransHeight, // flag to calc trans height or use user specified input
			Real64 const A_Kc, // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			Real64 const B_Kc, // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			Real64 const C_Kc, // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			Real64 const D_Kc, // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			Real64 const E_Kc // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
		) :
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			ZoneEquipPtr( ZoneEquipPtr ),
			DiffusersPerZone( DiffusersPerZone ),
			PowerPerPlume( PowerPerPlume ),
			DiffArea( DiffArea ),
			DiffAngle( DiffAngle ),
			HeatSrcHeight( HeatSrcHeight ),
			ThermostatHeight( ThermostatHeight ),
			ComfortHeight( ComfortHeight ),
			TempTrigger( TempTrigger ),
			DiffuserType( DiffuserType ),
			TransHeight( TransHeight ),
			CalcTransHeight( CalcTransHeight ),
			A_Kc( A_Kc ),
			B_Kc( B_Kc ),
			C_Kc( C_Kc ),
			D_Kc( D_Kc ),
			E_Kc( E_Kc )
		{}

	};

	struct UFEData
	{
		// Members
		std::string ZoneName; // Name of zone
		int ZonePtr; // Pointer to the zone number for this statement
		int ZoneEquipPtr; // Pointer to zone equip for this UFAD zone
		Real64 DiffusersPerZone; // Number of diffusers in this zone
		Real64 PowerPerPlume; // Power in each plume [W]
		Real64 DiffArea; // Effective area of a diffuser [m2]
		Real64 DiffAngle; // angle between diffuser slots and vertical (degrees)
		Real64 HeatSrcHeight; // height of heat source above floor [m]
		Real64 ThermostatHeight; // Height of thermostat/ temperature control sensor [m]
		Real64 ComfortHeight; // Height at which air temperature is measured for
		// comfort purposes [m]
		Real64 TempTrigger; // Minimum temperature difference between TOC TMX
		// for stratification [deltaC]
		int DiffuserType; // 1=Swirl, 2=variable area, 3=displacement, 4=linear bar grille, 5=custom
		Real64 TransHeight; // user specified transition height [m]
		bool CalcTransHeight; // flag to calc trans height or use user specified input
		Real64 A_Kc; // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
		Real64 B_Kc; // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
		Real64 C_Kc; // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
		Real64 D_Kc; // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
		Real64 E_Kc; // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
		Real64 WinWidth; // sum of widths of exterior windows in zone
		Real64 NumExtWin; // number of exterior windows in the zone
		bool ShadeDown; // signals shade up or down

		// Default Constructor
		UFEData() :
			ZonePtr( 0 ),
			ZoneEquipPtr( 0 ),
			DiffusersPerZone( 0.0 ),
			PowerPerPlume( 0.0 ),
			DiffArea( 0.0 ),
			DiffAngle( 0.0 ),
			HeatSrcHeight( 0.0 ),
			ThermostatHeight( 0.0 ),
			ComfortHeight( 0.0 ),
			TempTrigger( 0.0 ),
			DiffuserType( 0 ),
			TransHeight( 0.0 ),
			CalcTransHeight( false ),
			A_Kc( 0.0 ),
			B_Kc( 0.0 ),
			C_Kc( 0.0 ),
			D_Kc( 0.0 ),
			E_Kc( 0.0 ),
			WinWidth( 0.0 ),
			NumExtWin( 0.0 ),
			ShadeDown( true )
		{}

		// Member Constructor
		UFEData(
			std::string const & ZoneName, // Name of zone
			int const ZonePtr, // Pointer to the zone number for this statement
			int const ZoneEquipPtr, // Pointer to zone equip for this UFAD zone
			Real64 const DiffusersPerZone, // Number of diffusers in this zone
			Real64 const PowerPerPlume, // Power in each plume [W]
			Real64 const DiffArea, // Effective area of a diffuser [m2]
			Real64 const DiffAngle, // angle between diffuser slots and vertical (degrees)
			Real64 const HeatSrcHeight, // height of heat source above floor [m]
			Real64 const ThermostatHeight, // Height of thermostat/ temperature control sensor [m]
			Real64 const ComfortHeight, // Height at which air temperature is measured for
			Real64 const TempTrigger, // Minimum temperature difference between TOC TMX
			int const DiffuserType, // 1=Swirl, 2=variable area, 3=displacement, 4=linear bar grille, 5=custom
			Real64 const TransHeight, // user specified transition height [m]
			bool const CalcTransHeight, // flag to calc trans height or use user specified input
			Real64 const A_Kc, // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			Real64 const B_Kc, // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			Real64 const C_Kc, // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			Real64 const D_Kc, // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			Real64 const E_Kc, // Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			Real64 const WinWidth, // sum of widths of exterior windows in zone
			Real64 const NumExtWin, // number of exterior windows in the zone
			bool const ShadeDown // signals shade up or down
		) :
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			ZoneEquipPtr( ZoneEquipPtr ),
			DiffusersPerZone( DiffusersPerZone ),
			PowerPerPlume( PowerPerPlume ),
			DiffArea( DiffArea ),
			DiffAngle( DiffAngle ),
			HeatSrcHeight( HeatSrcHeight ),
			ThermostatHeight( ThermostatHeight ),
			ComfortHeight( ComfortHeight ),
			TempTrigger( TempTrigger ),
			DiffuserType( DiffuserType ),
			TransHeight( TransHeight ),
			CalcTransHeight( CalcTransHeight ),
			A_Kc( A_Kc ),
			B_Kc( B_Kc ),
			C_Kc( C_Kc ),
			D_Kc( D_Kc ),
			E_Kc( E_Kc ),
			WinWidth( WinWidth ),
			NumExtWin( NumExtWin ),
			ShadeDown( ShadeDown )
		{}

	};

	struct SurfMapPattern // nested structure in RoomAirPattern
	{
		// Members
		// user variables
		Array1D_string SurfName; // user defined name
		Array1D< Real64 > DeltaTai; // (Tai - MAT ) offset from mean air temp
		int NumSurfs; // number of surfaces in this pattern
		//calculated and from elsewhere
		Array1D_int SurfID; // index in HB surface structure array

		// Default Constructor
		SurfMapPattern() :
			NumSurfs( 0 )
		{}

		// Member Constructor
		SurfMapPattern(
			Array1_string const & SurfName, // user defined name
			Array1< Real64 > const & DeltaTai, // (Tai - MAT ) offset from mean air temp
			int const NumSurfs, // number of surfaces in this pattern
			Array1_int const & SurfID // index in HB surface structure array
		) :
			SurfName( SurfName ),
			DeltaTai( DeltaTai ),
			NumSurfs( NumSurfs ),
			SurfID( SurfID )
		{}

	};

	struct ConstGradPattern // nested structure in RoomAirPattern
	{
		// Members
		//user variables
		std::string Name; // name
		Real64 Gradient; // value of vertical gradient [C/m]

		// Default Constructor
		ConstGradPattern() :
			Gradient( 0.0 )
		{}

		// Member Constructor
		ConstGradPattern(
			std::string const & Name, // name
			Real64 const Gradient // value of vertical gradient [C/m]
		) :
			Name( Name ),
			Gradient( Gradient )
		{}

	};

	struct TwoVertGradInterpolPattern // nested structure in RoomAirPattern
	{
		// Members
		//user variables
		std::string Name; // name
		Real64 TstatHeight; // Height of thermostat/ temperature control sensor
		Real64 TleavingHeight; // height of return air node where leaving zone
		Real64 TexhaustHeight; // height of exhaust air node where leaving zone
		Real64 LowGradient; // lower value of vertical gradient [C/m]
		Real64 HiGradient; // upper value of vertical gradient [C/m]
		int InterpolationMode; // control for interpolation mode
		Real64 UpperBoundTempScale; // temperature value for HiGradient
		Real64 LowerBoundTempScale; // temperature value for LowGradient
		Real64 UpperBoundHeatRateScale; // load value for HiGradient
		Real64 LowerBoundHeatRateScale; // load value for lowGradient

		// Default Constructor
		TwoVertGradInterpolPattern() :
			TstatHeight( 0.0 ),
			TleavingHeight( 0.0 ),
			TexhaustHeight( 0.0 ),
			LowGradient( 0.0 ),
			HiGradient( 0.0 ),
			InterpolationMode( 0 ),
			UpperBoundTempScale( 0.0 ),
			LowerBoundTempScale( 0.0 ),
			UpperBoundHeatRateScale( 0.0 ),
			LowerBoundHeatRateScale( 0.0 )
		{}

		// Member Constructor
		TwoVertGradInterpolPattern(
			std::string const & Name, // name
			Real64 const TstatHeight, // Height of thermostat/ temperature control sensor
			Real64 const TleavingHeight, // height of return air node where leaving zone
			Real64 const TexhaustHeight, // height of exhaust air node where leaving zone
			Real64 const LowGradient, // lower value of vertical gradient [C/m]
			Real64 const HiGradient, // upper value of vertical gradient [C/m]
			int const InterpolationMode, // control for interpolation mode
			Real64 const UpperBoundTempScale, // temperature value for HiGradient
			Real64 const LowerBoundTempScale, // temperature value for LowGradient
			Real64 const UpperBoundHeatRateScale, // load value for HiGradient
			Real64 const LowerBoundHeatRateScale // load value for lowGradient
		) :
			Name( Name ),
			TstatHeight( TstatHeight ),
			TleavingHeight( TleavingHeight ),
			TexhaustHeight( TexhaustHeight ),
			LowGradient( LowGradient ),
			HiGradient( HiGradient ),
			InterpolationMode( InterpolationMode ),
			UpperBoundTempScale( UpperBoundTempScale ),
			LowerBoundTempScale( LowerBoundTempScale ),
			UpperBoundHeatRateScale( UpperBoundHeatRateScale ),
			LowerBoundHeatRateScale( LowerBoundHeatRateScale )
		{}

	};

	struct TempVsHeightPattern // to be used as nested structure in RoomAirPattern
	{
		// Members
		Array1D< Real64 > ZetaPatrn; // non dimensional height from floor,
		Array1D< Real64 > DeltaTaiPatrn; // Tai- MAT (TODO, check sign)

		// Default Constructor
		TempVsHeightPattern()
		{}

		// Member Constructor
		TempVsHeightPattern(
			Array1< Real64 > const & ZetaPatrn, // non dimensional height from floor,
			Array1< Real64 > const & DeltaTaiPatrn // Tai- MAT (TODO, check sign)
		) :
			ZetaPatrn( ZetaPatrn ),
			DeltaTaiPatrn( DeltaTaiPatrn )
		{}

	};

	struct TemperaturePatternStruct // RoomAirPattern
	{
		// Members
		std::string Name; // unique identifier
		int PatrnID; // control ID for referencing in Schedules
		int PatternMode; // Control for what type of calcs in this pattern
		ConstGradPattern GradPatrn; // Constant gradient pattern
		TwoVertGradInterpolPattern TwoGradPatrn; // Two gradient interpolation pattern
		TempVsHeightPattern VertPatrn; // Vertical gradient profile pattern
		SurfMapPattern MapPatrn; // Generic Surface map pattern
		Real64 DeltaTstat; // (Tstat - MAT) offset   deg C
		Real64 DeltaTleaving; // (Tleaving - MAT) deg C
		Real64 DeltaTexhaust; // (Texhaust - MAT) deg C

		// Default Constructor
		TemperaturePatternStruct() :
			PatrnID( 0 ),
			PatternMode( 0 ),
			DeltaTstat( 0.0 ),
			DeltaTleaving( 0.0 ),
			DeltaTexhaust( 0.0 )
		{}

		// Member Constructor
		TemperaturePatternStruct(
			std::string const & Name, // unique identifier
			int const PatrnID, // control ID for referencing in Schedules
			int const PatternMode, // Control for what type of calcs in this pattern
			ConstGradPattern const & GradPatrn, // Constant gradient pattern
			TwoVertGradInterpolPattern const & TwoGradPatrn, // Two gradient interpolation pattern
			TempVsHeightPattern const & VertPatrn, // Vertical gradient profile pattern
			SurfMapPattern const & MapPatrn, // Generic Surface map pattern
			Real64 const DeltaTstat, // (Tstat - MAT) offset   deg C
			Real64 const DeltaTleaving, // (Tleaving - MAT) deg C
			Real64 const DeltaTexhaust // (Texhaust - MAT) deg C
		) :
			Name( Name ),
			PatrnID( PatrnID ),
			PatternMode( PatternMode ),
			GradPatrn( GradPatrn ),
			TwoGradPatrn( TwoGradPatrn ),
			VertPatrn( VertPatrn ),
			MapPatrn( MapPatrn ),
			DeltaTstat( DeltaTstat ),
			DeltaTleaving( DeltaTleaving ),
			DeltaTexhaust( DeltaTexhaust )
		{}

	};

	struct SurfaceAssocNestedStruct
	{
		// Members
		std::string Name; // unique identifier
		int SurfID; // id in HB surface structs
		Real64 TadjacentAir; // place to put resulting temperature value
		Real64 Zeta; // non-dimensional height in zone ot

		// Default Constructor
		SurfaceAssocNestedStruct() :
			SurfID( 0 ),
			TadjacentAir( 23.0 ),
			Zeta( 0.0 )
		{}

		// Member Constructor
		SurfaceAssocNestedStruct(
			std::string const & Name, // unique identifier
			int const SurfID, // id in HB surface structs
			Real64 const TadjacentAir, // place to put resulting temperature value
			Real64 const Zeta // non-dimensional height in zone ot
		) :
			Name( Name ),
			SurfID( SurfID ),
			TadjacentAir( TadjacentAir ),
			Zeta( Zeta )
		{}

	};

	struct AirPatternInfobyZoneStruct // becomes AirPatternZoneInfo
	{
		// Members
		// user variables
		bool IsUsed; // .TRUE. if user-defined patterns used in zone
		std::string Name; // Name
		std::string ZoneName; // Zone name in building
		int ZoneID; // Index of Zone in Heat Balance
		std::string AvailSched; // Name of availability schedule
		int AvailSchedID; // index of availability schedule
		std::string PatternCntrlSched; // name of schedule that selects pattern
		int PatternSchedID; // index of pattern selecting schedule
		//calculated and from elsewhere
		Real64 ZoneHeight; // in meters, from Zone%CeilingHeight
		int ReturnAirNodeID; // index in Node array
		int ZoneNodeID; // index in Node array for this zone
		Array1D_int ExhaustAirNodeID; // indexes in Node array
		Real64 TairMean; // comes from MAT
		Real64 Tstat; // temperature for thermostat
		Real64 Tleaving; // temperature for return air node
		Real64 Texhaust; // temperature for exhaust air node
		Array1D< SurfaceAssocNestedStruct > Surf; // nested struct w/ surface info
		int totNumSurfs; // total surfs for this zone
		int firstSurfID; // Index of first surface
		//report
		Real64 Gradient; // result for modeled gradient if using two-gradient interpolation

		// Default Constructor
		AirPatternInfobyZoneStruct() :
			IsUsed( false ),
			ZoneID( 0 ),
			AvailSchedID( 0 ),
			PatternSchedID( 0 ),
			ZoneHeight( 0.0 ),
			ReturnAirNodeID( 0 ),
			ZoneNodeID( 0 ),
			TairMean( 23.0 ),
			Tstat( 23.0 ),
			Tleaving( 23.0 ),
			Texhaust( 23.0 ),
			totNumSurfs( 0 ),
			firstSurfID( 0 ),
			Gradient( 0.0 )
		{}

		// Member Constructor
		AirPatternInfobyZoneStruct(
			bool const IsUsed, // .TRUE. if user-defined patterns used in zone
			std::string const & Name, // Name
			std::string const & ZoneName, // Zone name in building
			int const ZoneID, // Index of Zone in Heat Balance
			std::string const & AvailSched, // Name of availability schedule
			int const AvailSchedID, // index of availability schedule
			std::string const & PatternCntrlSched, // name of schedule that selects pattern
			int const PatternSchedID, // index of pattern selecting schedule
			Real64 const ZoneHeight, // in meters, from Zone%CeilingHeight
			int const ReturnAirNodeID, // index in Node array
			int const ZoneNodeID, // index in Node array for this zone
			Array1_int const & ExhaustAirNodeID, // indexes in Node array
			Real64 const TairMean, // comes from MAT
			Real64 const Tstat, // temperature for thermostat
			Real64 const Tleaving, // temperature for return air node
			Real64 const Texhaust, // temperature for exhaust air node
			Array1< SurfaceAssocNestedStruct > const & Surf, // nested struct w/ surface info
			int const totNumSurfs, // total surfs for this zone
			int const firstSurfID, // Index of first surface
			Real64 const Gradient // result for modeled gradient if using two-gradient interpolation
		) :
			IsUsed( IsUsed ),
			Name( Name ),
			ZoneName( ZoneName ),
			ZoneID( ZoneID ),
			AvailSched( AvailSched ),
			AvailSchedID( AvailSchedID ),
			PatternCntrlSched( PatternCntrlSched ),
			PatternSchedID( PatternSchedID ),
			ZoneHeight( ZoneHeight ),
			ReturnAirNodeID( ReturnAirNodeID ),
			ZoneNodeID( ZoneNodeID ),
			ExhaustAirNodeID( ExhaustAirNodeID ),
			TairMean( TairMean ),
			Tstat( Tstat ),
			Tleaving( Tleaving ),
			Texhaust( Texhaust ),
			Surf( Surf ),
			totNumSurfs( totNumSurfs ),
			firstSurfID( firstSurfID ),
			Gradient( Gradient )
		{}

	};

	// Object Data
	extern Array1D< AirModelData > AirModel;
	extern Array1D< AirNodeData > AirNode;
	extern Array1D< DVData > ZoneUCSDDV; // UCSD
	extern Array1D< CVData > ZoneUCSDCV;
	extern Array1D< UFIData > ZoneUCSDUI;
	extern Array1D< UFEData > ZoneUCSDUE;
	extern Array2D< CVFlow > CVJetRecFlows; // Jet and recirculation zone flows and properties
	extern Array1D< CVDVParameters > SurfParametersCVDV; // Surface parameters
	extern Array1D< TemperaturePatternStruct > RoomAirPattern; // user defined patterns ,various types
	extern Array1D< AirPatternInfobyZoneStruct > AirPatternZoneInfo; // added zone information for user defined patterns

} // DataRoomAirModel

} // EnergyPlus

#endif
