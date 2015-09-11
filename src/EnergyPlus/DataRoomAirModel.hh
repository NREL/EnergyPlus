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
	extern int const RoomAirModel_AirflowNetwork; // RoomAirModel_AirflowNetwork interior zone model
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
	extern int const AirflowNetworkRoomAirNode; // air node for airflow network based room air model
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
	extern int TotNumOfRoomAFNNodes;
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

	// RoomAirflowNetwork
	extern int NumOfRoomAirflowNetControl; // count of RoomAirSettings:AirflowNetwork

	// Types

	struct AirModelData
	{
		// Members
		std::string AirModelName;
		std::string ZoneName;
		int ZonePtr; // Pointer to the zone number for this statement
		int AirModelType; // 1 = Mixing, 2 = Mundt, 3 = Rees and Haves,
		// 4 = UCSDDV, 5 = UCSDCV, -1 = user defined
		// 6 = UCSDUFI, 7 = UCSDUFE, 8 = AirflowNetwork
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
		Real64 ZoneVolumeFraction; // portion of zone air volume associated with this node
		Array1D_bool SurfMask; // limit of 60 surfaces at current sizing
		bool IsZone; // TRUE if this node is zone node

		// Default Constructor
		AirNodeData() :
			ZonePtr( 0 ),
			ClassType( 0 ),
			Height( 0.0 ),
			ZoneVolumeFraction( 0 ),
			IsZone( false )
		{}

		// Member Constructor
		AirNodeData(
			std::string const & Name, // name
			std::string const & ZoneName,
			int const ZonePtr, // Pointer to the zone number for this statement
			int const ClassType, // depending on type of model
			Real64 const Height, // height
			Real64 const ZoneVolumeFraction, // portion of zone air volume associated with this node
			Array1_bool const & SurfMask, // limit of 60 surfaces at current sizing
			bool const IsZone // TRUE if this node is zone node
		) :
			Name( Name ),
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			ClassType( ClassType ),
			Height( Height ),
			ZoneVolumeFraction( ZoneVolumeFraction ),
			SurfMask( SurfMask ),
			IsZone( IsZone )
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

	struct AirflowLinkagesInfoNestedStruct // becomes link
	{
		// Members
		// user variables
		int AirflowNetworkLinkSimuID; // point to this linkage in AirflowNetworkLinkSimu structure
		int AirflowNetworkLinkageDataID; // point to this linkage in AirflowNetworkLinkageData structure
		int AirflowNetworkLinkReportID; // point to this linkage in AirflowNetworkLinkReport structure
		Real64 MdotIn; // mass flow rate of air into control volume(neg means leaving control volume) (kg / s)
		Real64 TempIn; // dryblub temperature of air into control volume
		Real64 HumRatIn; // humidity ratio of air into control volume

		// Default Constructor
		AirflowLinkagesInfoNestedStruct( ) :
			AirflowNetworkLinkSimuID( 0 ),
			AirflowNetworkLinkageDataID( 0 ),
			AirflowNetworkLinkReportID( 0 ),
			MdotIn( 0.0 ),
			TempIn( 0.0 ),
			HumRatIn( 0.0 )
		{}

		// Member Constructor
		AirflowLinkagesInfoNestedStruct(
			int const AirflowNetworkLinkSimuID, // point to this linkage in AirflowNetworkLinkSimu structure
			int const AirflowNetworkLinkageDataID, // point to this linkage in AirflowNetworkLinkageData structure
			int const AirflowNetworkLinkReportID, // point to this linkage in AirflowNetworkLinkReport structure
			Real64 const MdotIn, // mass flow rate of air into control volume(neg means leaving control volume) (kg / s)
			Real64 const TempIn, // dryblub temperature of air into control volume
			Real64 const HumRatIn // humidity ratio of air into control volume
			) :
			AirflowNetworkLinkSimuID( AirflowNetworkLinkSimuID ),
			AirflowNetworkLinkageDataID( AirflowNetworkLinkageDataID ),
			AirflowNetworkLinkReportID( AirflowNetworkLinkReportID ),
			MdotIn( MdotIn ),
			TempIn( TempIn ),
			HumRatIn( HumRatIn )
		{}

	};

	struct RoomAirflowNetworkNodeInternalGainsStruct // becomes IntGain
	{
		// Members
		// user variables
		int TypeOfNum; // Internal type
		std::string Name; // Intenral gain name
		bool UseRoomAirModelTempForGains; // TRUE if user inputs temp for gains
		bool FractionCheck; // TRUE if a fraction of internal gain for each object is checked

		// Default Constructor
		RoomAirflowNetworkNodeInternalGainsStruct( ) :
			TypeOfNum( 0 ),
			UseRoomAirModelTempForGains( false ),
			FractionCheck( false )
		{}

		// Member Constructor
		RoomAirflowNetworkNodeInternalGainsStruct(
			int const TypeOfNum, // Internal type
			std::string const & Name, // Intenral gain name
			bool const UseRoomAirModelTempForGains, // TRUE if user inputs temp for gains
			bool const FractionCheck // TRUE if a fraction of internal gain for each object is checked
			) :
			TypeOfNum( TypeOfNum ),
			Name( Name ),
			UseRoomAirModelTempForGains( UseRoomAirModelTempForGains ),
			FractionCheck( FractionCheck )
		{}

	};

	struct RoomAirflowNetworkHVACStruct // becomes HVAC
	{
		// Members
		// user variables
		std::string Name; // HVAC system name
		std::string ObjectTypeName; // HVAC object type name
		std::string SupplyNodeName; // HVAC system supply node name
		std::string ReturnNodeName; // HVAC system return node name
		int TypeOfNum; // HVAC type num
		Real64 SupplyFraction; // Supply flow fraction
		Real64 ReturnFraction; // Return flow fraction
		int EquipConfigIndex; // HVAC equipment configuration index
		int SupNodeNum; // HVAC supply node number
		int RetNodeNum; // HVAC return node number
		int CompIndex; // Component index

		// Default Constructor
		RoomAirflowNetworkHVACStruct( ) :
			TypeOfNum( 0 ), // HVAC type num
			SupplyFraction( 0 ), // Supply flow fraction
			ReturnFraction( 0 ), // Return flow fraction
			EquipConfigIndex( 0 ), // HVAC equipment configuration index
			SupNodeNum( 0 ), // HVAC supply node number
			RetNodeNum( 0 ), // HVAC return node number
			CompIndex( 0 ) // Component index
		{}

		// Member Constructor
		RoomAirflowNetworkHVACStruct(
			std::string const & Name, // HVAC system name
			std::string const & ObjectTypeName, // HVAC object type name
			std::string const & SupplyNodeName, // HVAC system supply node name
			std::string const & ReturnNodeName, // HVAC system return node name
			int const TypeOfNum, // HVAC type num
			Real64 const SupplyFraction, // Supply flow fraction
			Real64 const ReturnFraction, // Return flow fraction
			int const EquipConfigIndex, // HVAC equipment configuration index
			int const SupNodeNum, // HVAC supply node number
			int const RetNodeNum, // HVAC return node number
			int const CompIndex // Component index
			) :
			Name( Name ),
			ObjectTypeName( ObjectTypeName ),
			SupplyNodeName( SupplyNodeName ),
			ReturnNodeName( ReturnNodeName ),
			TypeOfNum( TypeOfNum ),
			SupplyFraction( SupplyFraction ),
			ReturnFraction( ReturnFraction ),
			EquipConfigIndex( EquipConfigIndex ),
			SupNodeNum( SupNodeNum ),
			RetNodeNum( RetNodeNum ),
			CompIndex( CompIndex )
		{}

	};

	struct RoomAirflowNetworkAirNodeNestedStruct // becomes Node
	{
		// Members
		// user variables
		std::string Name; // name of the node itself
		Real64 ZoneVolumeFraction; // Zone volume fraction applied to this specific node
		std::string NodeSurfListName; // name of nodes' adjacent surface list
		bool HasSurfacesAssigned; // True if this node has surfaces assigned
		Array1D< bool > SurfMask; // Sized to num of surfs in Zone, true if surface is associated with this node
		std::string NodeIntGainsListName; // name of node's internal gains list
		bool HasIntGainsAssigned; // True if this node has internal gain assigned
		int NumIntGains; // Number of internal gain objects
		Array1D< int > IntGainsDeviceIndices; // sized to NumIntGains, index pointers to internal gains struct
		Array1D< Real64 > IntGainsFractions; // sized to NumIntGains, gain fractions to this node
		Array1D< RoomAirflowNetworkNodeInternalGainsStruct > IntGain; // Internal gain struct
		std::string NodeHVACListName; // name of node's HVAC list
		bool HasHVACAssigned; // True if HVAC systems are assigned to this node
		int NumHVACs; // Number of HVAC systems
		Array1D< RoomAirflowNetworkHVACStruct > HVAC; // HVAC struct
		int AirflowNetworkNodeID; // pointer to AirflowNetworkNodeData structure
		int NumOfAirflowLinks; // Number of intra zone links
		Array1D< AirflowLinkagesInfoNestedStruct > Link; // Linkage struct
		Real64 AirVolume; // air volume in control volume associated with this node(m3 / s)
		Real64 RhoAir; // current density of air for nodal control volume
		Real64 CpAir; // current heat capacity of air for nodal control volume

		Real64 AirTemp; // node air temperature
		Real64 AirTempX1; // node air temperature at t minus 1 zone timestep
		Real64 AirTempX2; // node air temperature at t minus 2 zone timestep
		Real64 AirTempX3; // node air temperature at t minus 3 zone timestep
		Real64 AirTempX4; // node air temperature at t minus 4 zone timestep
		Real64 AirTempDSX1; // node air temperature at t minus 1 system timestep
		Real64 AirTempDSX2; // node air temperature at t minus 2 system timestep
		Real64 AirTempDSX3; // node air temperature at t minus 3 system timestep
		Real64 AirTempDSX4; // node air temperature at t minus 4 system timestep
		Real64 AirTempT1; // node air temperature at the previous time step used in Exact and Euler method
		Real64 AirTempTMX; // temporary node air temperature to test convergence used in Exact and Euler method
		Real64 AirTempTM2; // node air temperature at time step t-2 used in Exact and Euler method

		Real64 HumRat; // node air humidity ratio
		Real64 HumRatX1; // node air humidity ratio at t minus 1 zone timestep
		Real64 HumRatX2; // node air humidity ratio at t minus 2 zone timestep
		Real64 HumRatX3; // node air humidity ratio at t minus 3 zone timestep
		Real64 HumRatX4; // node air humidity ratio at t minus 4 zone timestep
		Real64 HumRatDSX1; // node air humidity ratio at t minus 1 system timestep
		Real64 HumRatDSX2; // node air humidity ratio at t minus 2 system timestep
		Real64 HumRatDSX3; // node air humidity ratio at t minus 3 system timestep
		Real64 HumRatDSX4; // node air humidity ratio at t minus 4 system timestep
		Real64 HumRatW1; // node air humidity ratio at the previous time step used in Exact and Euler method
		Real64 HumRatWMX; // temporary node air humidity ratio to test convergence used in Exact and Euler method
		Real64 HumRatWM2; // node air humidity ratio at time step t-2 used in Exact and Euler method

		Real64 RelHumidity; // node air relative humidity

		// sensible heat balance terms for node
		Real64 SumIntSensibleGain; // rate of heat gain from internal sensible gains(after fraction)
		Real64 SumHA; // sum of Hc * Area for surfaces associated with this node(surface convection sensible gain term)
		Real64 SumHATsurf; // sum of Hc * Area * Temp for surfaces associated with this node for convective heat transfer
		Real64 SumHATref; // sum of Hc * Area * Temp for surfaces associated with this node for radiation exchange
		Real64 SumLinkMCp; // sum of mdor*Cp for incoming airflows for this node derived from the AirflowNetwork model  
		Real64 SumLinkMCpT; // sum of mdor*Cp*T for incoming airflows and source temperature for this node derived from the AirflowNetwork model 
		Real64 SumSysMCp; // sum of mdor*Cp for incoming supply airflows for this node 
		Real64 SumSysMCpT; // sum of mdor*Cp*T for incoming supply airflows and temperature for this node 
		Real64 SumSysM; // sum of mdot for incoming supply airflows for this node 
		Real64 SumSysMW; // sum of mdot*W for incoming supply airflows and temperature for this node 
		Real64 NonAirSystemResponse; // sum of convective system load 
		Real64 SysDepZoneLoadsLagged; // sum of system lagged load
		Real64 SysDepZoneLoadsLaggedOld; // sum of system lagged load
		Real64 AirCap; // Air storage term for energy balalce at each node
		Real64 AirHumRat; // Air storage term for moisture balalce at each node
		// latent moisture balance terms for node
		Real64 SumIntLatentGain; // rate of heat gain form internal latent gains(after fraction)
		Real64 SumHmAW; // sum of AREA*Moist CONVECTION COEFF*INSIDE Humidity Ratio
		Real64 SumHmARa; // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air
		Real64 SumHmARaW; // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air* Inside Humidity Ratio
		Real64 SumLinkM; // sum of mdor for incoming airflows for this node derived from the AirflowNetwork model
		Real64 SumLinkMW; // sum of mdor*Cp*T for incoming airflows and source humidity ratio for this node derived from the AirflowNetwork model 

		// Default Constructor
		RoomAirflowNetworkAirNodeNestedStruct( ) :
			ZoneVolumeFraction( 0.0 ),
			HasSurfacesAssigned( false ),
			HasIntGainsAssigned( false ),
			NumIntGains( 0 ),
			HasHVACAssigned( false ),
			NumHVACs( 0 ),
			AirflowNetworkNodeID( 0 ),  // pointer to AirflowNetworkNodeData structure
			NumOfAirflowLinks( 0 ),
			AirVolume( 0.0 ),  // air volume in control volume associated with this node(m3 / s)
			RhoAir( 0.0 ),  // current density of air for nodal control volume
			CpAir( 0.0 ),  // current heat capacity of air for nodal control volume
			AirTemp( 0.0 ), // node air temperature
			AirTempX1( 0.0 ),  // node air temperature at t minus 1 zone timestep
			AirTempX2( 0.0 ),  // node air temperature at t minus 2 zone timestep
			AirTempX3( 0.0 ), // node air temperature at t minus 3 zone timestep
			AirTempX4( 0.0 ), // node air temperature at t minus 4 zone timestep
			AirTempDSX1( 0.0 ), // node air temperature at t minus 1 system timestep
			AirTempDSX2( 0.0 ), // node air temperature at t minus 2 system timestep
			AirTempDSX3( 0.0 ), // node air temperature at t minus 3 system timestep
			AirTempDSX4( 0.0 ), // node air temperature at t minus 4 system timestep
			AirTempT1( 0.0 ), // node air temperature at the previous time step used in Exact and Euler method
			AirTempTMX( 0.0 ), // temporary node air temperature to test convergence used in Exact and Euler method
			AirTempTM2( 0.0 ), // node air temperature at time step t-2 used in Exact and Euler method
			HumRat( 0.0 ), // node air humidity ratio
			HumRatX1( 0.0 ), // node air humidity ratio at t minus 1 zone timestep
			HumRatX2( 0.0 ), // node air humidity ratio at t minus 2 zone timestep
			HumRatX3( 0.0 ), // node air humidity ratio at t minus 3 zone timestep
			HumRatX4( 0.0 ), // node air humidity ratio at t minus 4 zone timestep
			HumRatDSX1( 0.0 ), // node air humidity ratio at t minus 1 system timestep
			HumRatDSX2( 0.0 ), // node air humidity ratio at t minus 2 system timestep
			HumRatDSX3( 0.0 ), // node air humidity ratio at t minus 3 system timestep
			HumRatDSX4( 0.0 ), // node air humidity ratio at t minus 4 system timestep
			HumRatW1( 0.0 ), // node air humidity ratio at the previous time step used in Exact and Euler method
			HumRatWMX( 0.0 ), // temporary node air humidity ratio to test convergence used in Exact and Euler method
			HumRatWM2( 0.0 ), // node air humidity ratio at time step t-2 used in Exact and Euler method
			RelHumidity( 0.0 ), // node air relative humidity
			//sensible heat balance terms for node
			SumIntSensibleGain( 0.0 ), // rate of heat gain from internal sensible gains(after fraction)
			SumHA( 0.0 ), // sum of Hc * Area for surfaces associated with this node(surface convection sensible gain term)
			SumHATsurf( 0.0 ),
			SumHATref( 0.0 ), // sum of Hc * Area * Temp for surfaces associated with this node
			SumLinkMCp( 0.0 ),
			SumLinkMCpT( 0.0 ),
			SumSysMCp( 0.0 ),
			SumSysMCpT( 0.0 ),
			SumSysM( 0.0 ),
			SumSysMW( 0.0 ),
			NonAirSystemResponse( 0.0 ),
			SysDepZoneLoadsLagged( 0.0 ),
			SysDepZoneLoadsLaggedOld( 0.0 ),
			AirCap( 0.0 ),
			AirHumRat( 0.0 ),
			//latent moisture balance terms for node
			SumIntLatentGain( 0.0 ), // rate of heat gain form internal latent gains(after fraction)
			SumHmAW( 0.0 ), // sum of AREA*Moist CONVECTION COEFF*INSIDE Humidity Ratio
			SumHmARa( 0.0 ), // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air
			SumHmARaW( 0.0 ), // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air* Inside Humidity Ratio
			SumLinkM( 0.0 ),
			SumLinkMW( 0.0 )
			{}

		// Member Constructor
		RoomAirflowNetworkAirNodeNestedStruct(
			std::string const & Name, // name of the node itself
			Real64 const ZoneVolumeFraction, // Zone volume fraction applied to this specific node
			std::string const & NodeSurfListName, // name of nodes' adjacent surface list
			bool const HasSurfacesAssigned, // True if this node has surfaces assigned
			Array1< bool > const & SurfMask, // Sized to num of surfs in Zone, true if surface is associated with this node
			std::string const & NodeIntGainsListName, // name of node's internal gains list
			bool const HasIntGainsAssigned, // True if this node has internal gain assigned
			int const NumIntGains, // Number of internal gain objects
			Array1< int > const & IntGainsDeviceIndices, // sized to NumIntGains, index pointers to internal gains struct
			Array1< Real64 > const & IntGainsFractions, // sized to NumIntGains, gain fractions to this node
			Array1< RoomAirflowNetworkNodeInternalGainsStruct > const & IntGain, // Internal gain struct
			std::string const & NodeHVACListName, // name of node's HVAC list
			bool const HasHVACAssigned, // True if HVAC systems are assigned to this node
			int const NumHVACs, // Number of HVAC systems
			Array1D< RoomAirflowNetworkHVACStruct > const & HVAC, // HVAC struct
			int const AirflowNetworkNodeID, // pointer to AirflowNetworkNodeData structure
			int const NumOfAirflowLinks, // Number of intra zone links
			Array1< AirflowLinkagesInfoNestedStruct > const & Link, // Linkage struct
			Real64 const AirVolume, // air volume in control volume associated with this node(m3 / s)
			Real64 const RhoAir, // current density of air for nodal control volume
			Real64 const CpAir, // current heat capacity of air for nodal control volume

			Real64 const AirTemp, // node air temperature
			Real64 const AirTempX1, // node air temperature at t minus 1 zone timestep
			Real64 const AirTempX2, // node air temperature at t minus 2 zone timestep
			Real64 const AirTempX3, // node air temperature at t minus 3 zone timestep
			Real64 const AirTempX4, // node air temperature at t minus 4 zone timestep
			Real64 const AirTempDSX1, // node air temperature at t minus 1 system timestep
			Real64 const AirTempDSX2, // node air temperature at t minus 2 system timestep
			Real64 const AirTempDSX3, // node air temperature at t minus 3 system timestep
			Real64 const AirTempDSX4, // node air temperature at t minus 4 system timestep
			Real64 const AirTempT1, // node air temperature at the previous time step used in Exact and Euler method
			Real64 const AirTempTMX, // temporary node air temperature to test convergence used in Exact and Euler method
			Real64 const AirTempTM2, // node air temperature at time step t-2 used in Exact and Euler method

			Real64 const HumRat, // node air humidity ratio
			Real64 const HumRatX1, // node air humidity ratio at t minus 1 zone timestep
			Real64 const HumRatX2, // node air humidity ratio at t minus 2 zone timestep
			Real64 const HumRatX3, // node air humidity ratio at t minus 3 zone timestep
			Real64 const HumRatX4, // node air humidity ratio at t minus 4 zone timestep
			Real64 const HumRatDSX1, // node air humidity ratio at t minus 1 system timestep
			Real64 const HumRatDSX2, // node air humidity ratio at t minus 2 system timestep
			Real64 const HumRatDSX3, // node air humidity ratio at t minus 3 system timestep
			Real64 const HumRatDSX4, // node air humidity ratio at t minus 4 system timestep
			Real64 const HumRatW1, // node air humidity ratio at the previous time step used in Exact and Euler method
			Real64 const HumRatWMX, // temporary node air humidity ratio to test convergence used in Exact and Euler method
			Real64 const HumRatWM2, // node air humidity ratio at time step t-2 used in Exact and Euler method
			Real64 const RelHumidity, // node air relative humidity
			// sensible heat balance terms for node
			Real64 const SumIntSensibleGain, // rate of heat gain from internal sensible gains(after fraction)
			Real64 const SumHA, // sum of Hc * Area for surfaces associated with this node(surface convection sensible gain term)
			Real64 const SumHATsurf, // sum of Hc * Area * Temp for surfaces associated with this node for convective heat transfer
			Real64 const SumHATref, // sum of Hc * Area * Temp for surfaces associated with this node for radiation exchange
			Real64 const SumLinkMCp, // sum of mdor*Cp for incoming airflows for this node derived from the AirflowNetwork model  
			Real64 const SumLinkMCpT, // sum of mdor*Cp*T for incoming airflows and source temperature for this node derived from the AirflowNetwork model 
			Real64 const SumSysMCp, // sum of mdor*Cp for incoming supply airflows for this node 
			Real64 const SumSysMCpT, // sum of mdor*Cp for incoming supply airflows and temperature for this node 
			Real64 const SumSysM, // sum of mdot for incoming supply airflows for this node 
			Real64 const SumSysMW, // sum of mdot*W for incoming supply airflows and temperature for this node 
			Real64 const NonAirSystemResponse, // sum of convective system load 
			Real64 const SysDepZoneLoadsLagged, // sum of system lagged load
			Real64 const SysDepZoneLoadsLaggedOld, // sum of system lagged load
			Real64 const AirCap, // Air storage term for energy balalce at each node
			Real64 const AirHumRat, // Air storage term for moisture balalce at each node
			// latent moisture balance terms for node
			Real64 const SumIntLatentGain, // rate of heat gain form internal latent gains(after fraction)
			Real64 const SumHmAW, // sum of AREA*Moist CONVECTION COEFF*INSIDE Humidity Ratio
			Real64 const SumHmARa, // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air
			Real64 const SumHmARaW, // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air* Inside Humidity Ratio
			Real64 const SumLinkM, // sum of mdor for incoming airflows for this node derived from the AirflowNetwork model
			Real64 const SumLinkMW // sum of mdor*Cp*T for incoming airflows and source humidity ratio for this node derived from the AirflowNetwork model 
			) :
		Name( Name ), // name of the node itself
			ZoneVolumeFraction( ZoneVolumeFraction ), // Zone volume fraction applied to this specific node
			NodeSurfListName( NodeSurfListName ), // name of nodes' adjacent surface list
			HasSurfacesAssigned( HasSurfacesAssigned ), // True if this node has surfaces assigned
			SurfMask( SurfMask ), // Sized to num of surfs in Zone, true if surface is associated with this node
			NodeIntGainsListName( NodeIntGainsListName ), // name of node's internal gains list
			HasIntGainsAssigned( HasIntGainsAssigned ), // True if this node has internal gain assigned
			NumIntGains( NumIntGains ), // Number of internal gain objects
			IntGainsDeviceIndices( IntGainsDeviceIndices ), // sized to NumIntGains, index pointers to internal gains struct
			IntGainsFractions( IntGainsFractions ), // sized to NumIntGains, gain fractions to this node
			IntGain( IntGain ), // Internal gain struct
			NodeHVACListName( NodeHVACListName ), // name of node's HVAC list
			HasHVACAssigned( HasHVACAssigned ), // True if HVAC systems are assigned to this node
			NumHVACs( NumHVACs ), // Number of HVAC systems
			HVAC( HVAC ), // HVAC struct
			AirflowNetworkNodeID( AirflowNetworkNodeID ), // pointer to AirflowNetworkNodeData structure
			NumOfAirflowLinks( NumOfAirflowLinks ), // Number of intra zone links
			Link( Link ), // Linkage struct
			AirVolume( AirVolume ), // air volume in control volume associated with this node(m3 / s)
			RhoAir( RhoAir ), // current density of air for nodal control volume
			CpAir( CpAir ), // current heat capacity of air for nodal control volume

			AirTemp( AirTemp ), // node air temperature
			AirTempX1( AirTempX1 ), // node air temperature at t minus 1 zone timestep
			AirTempX2( AirTempX2 ), // node air temperature at t minus 2 zone timestep
			AirTempX3( AirTempX3 ), // node air temperature at t minus 3 zone timestep
			AirTempX4( AirTempX4 ), // node air temperature at t minus 4 zone timestep
			AirTempDSX1( AirTempDSX1 ), // node air temperature at t minus 1 system timestep
			AirTempDSX2( AirTempDSX2 ), // node air temperature at t minus 2 system timestep
			AirTempDSX3( AirTempDSX3 ), // node air temperature at t minus 3 system timestep
			AirTempDSX4( AirTempDSX4 ), // node air temperature at t minus 4 system timestep
			AirTempT1( AirTempT1 ), // node air temperature at the previous time step used in Exact and Euler method
			AirTempTMX( AirTempTMX ), // temporary node air temperature to test convergence used in Exact and Euler method
			AirTempTM2( AirTempTM2 ), // node air temperature at time step t-2 used in Exact and Euler method

			HumRat( HumRat ), // node air humidity ratio
			HumRatX1( HumRatX1 ), // node air humidity ratio at t minus 1 zone timestep
			HumRatX2( HumRatX2 ), // node air humidity ratio at t minus 2 zone timestep
			HumRatX3( HumRatX3 ), // node air humidity ratio at t minus 3 zone timestep
			HumRatX4( HumRatX4 ), // node air humidity ratio at t minus 4 zone timestep
			HumRatDSX1( HumRatDSX1 ), // node air humidity ratio at t minus 1 system timestep
			HumRatDSX2( HumRatDSX2 ), // node air humidity ratio at t minus 2 system timestep
			HumRatDSX3( HumRatDSX3 ), // node air humidity ratio at t minus 3 system timestep
			HumRatDSX4( HumRatDSX4 ), // node air humidity ratio at t minus 4 system timestep
			HumRatW1( HumRatW1 ), // node air humidity ratio at the previous time step used in Exact and Euler method
			HumRatWMX( HumRatWMX ), // temporary node air humidity ratio to test convergence used in Exact and Euler method
			HumRatWM2( HumRatWM2 ), // node air humidity ratio at time step t-2 used in Exact and Euler method
			RelHumidity( RelHumidity ), // node air relative humidity

			// sensible heat balance terms for node
			SumIntSensibleGain( SumIntSensibleGain ), // rate of heat gain from internal sensible gains(after fraction)
			SumHA( SumHA ), // sum of Hc * Area for surfaces associated with this node(surface convection sensible gain term)
			SumHATsurf( SumHATsurf ), // sum of Hc * Area * Temp for surfaces associated with this node for convective heat transfer
			SumHATref( SumHATref ), // sum of Hc * Area * Temp for surfaces associated with this node for radiation exchange
			SumLinkMCp( SumLinkMCp ), // sum of mdor*Cp for incoming airflows for this node derived from the AirflowNetwork model  
			SumLinkMCpT( SumLinkMCpT ), // sum of mdor*Cp*T for incoming airflows and source temperature for this node derived from the AirflowNetwork model 
			SumSysMCp( SumSysMCp ), // sum of mdor*Cp for incoming supply airflows for this node 
			SumSysMCpT( SumSysMCpT ), // sum of mdor*Cp for incoming supply airflows and temperature for this node 
			SumSysM( SumSysM ), // sum of mdot for incoming supply airflows for this node 
			SumSysMW( SumSysMW ), // sum of mdot*W for incoming supply airflows and temperature for this node 
			NonAirSystemResponse( NonAirSystemResponse ), // sum of convective system load 
			SysDepZoneLoadsLagged( SysDepZoneLoadsLagged ), // sum of system lagged load
			SysDepZoneLoadsLaggedOld( SysDepZoneLoadsLaggedOld ), // sum of system lagged load
			AirCap( AirCap ), // Air storage term for energy balalce at each node
			AirHumRat( AirHumRat ), // Air storage term for moisture balalce at each node
			// latent moisture balance terms for node
			SumIntLatentGain( SumIntLatentGain ), // rate of heat gain form internal latent gains(after fraction)
			SumHmAW( SumHmAW ), // sum of AREA*Moist CONVECTION COEFF*INSIDE Humidity Ratio
			SumHmARa( SumHmARa ), // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air
			SumHmARaW( SumHmARaW ), // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air* Inside Humidity Ratio
			SumLinkM( SumLinkM ), // sum of mdor for incoming airflows for this node derived from the AirflowNetwork model
			SumLinkMW( SumLinkMW ) // sum of mdor*Cp*T for incoming airflows and source humidity ratio for this node derived from the AirflowNetwork model 

		{}

	};

	struct RoomAirflowNetworkInfoByZoneStruct // becomes RoomAirflowNetworkZoneInfo
	{
		// Members
		// user variables
		bool IsUsed; // true. if RoomAirflowNetwork model used in zone
		std::string Name; // Name
		std::string ZoneName; // Zone name in building
		int ZoneID; // Index of Zone in Heat Balance
		int ActualZoneID; // Index of controlled zones in ZoneCOnfigure
		std::string AvailSched; // Name of availability schedule
		int AvailSchedID; // index of availability schedule
		int ControlAirNodeID; // index of roomair node that is HVAC control sensor location
		int NumOfAirNodes; // Number of air nodes
		Array1D< RoomAirflowNetworkAirNodeNestedStruct > Node; // Node struct
		int ReturnAirNodeID; // index in system Node array
		int ZoneNodeID; // index in system Node array for this zone
		Real64 TairMean; // comes from MAT
		Real64 Tstat; // temperature for thermostat
		Real64 Tleaving; // temperature for return air node
		Real64 Texhaust; // temperature for exhaust air node
		int totNumSurfs; // total surfs for this zone
		int firstSurfID; // Index of first surface
		int RAFNNum; // RAFN number

		// Default Constructor
		RoomAirflowNetworkInfoByZoneStruct( ) :
			IsUsed( false ), // true. if RoomAirflowNetwork model used in zone
			ZoneID( 0 ), // Index of Zone in Heat Balance
			ActualZoneID( 0 ), // Index of controlled zones in ZoneCOnfigure
			AvailSchedID( 0 ), // index of availability schedule
			ControlAirNodeID( 0 ), // index of roomair node that is HVAC control sensor location
			NumOfAirNodes( 0 ), // Number of air nodes
			ReturnAirNodeID( 0 ), // index in system Node array
			ZoneNodeID( 0 ), // index in system Node array for this zone
			TairMean( 23.0 ), // comes from MAT
			Tstat( 23.0 ), // temperature for thermostat
			Tleaving( 23.0 ), // temperature for return air node
			Texhaust( 23.0 ), // temperature for exhaust air node
			totNumSurfs( 0 ), // total surfs for this zone
			firstSurfID( 0 ), // Index of first surface
			RAFNNum( 0 ) // RAFN number
		{}

		// Member Constructor
		RoomAirflowNetworkInfoByZoneStruct(
			bool const IsUsed, // true. if RoomAirflowNetwork model used in zone
			std::string const Name, // Name
			std::string const ZoneName, // Zone name in building
			int const ZoneID, // Index of Zone in Heat Balance
			int const ActualZoneID, // Index of controlled zones in ZoneCOnfigure
			std::string const AvailSched, // Name of availability schedule
			int const AvailSchedID, // index of availability schedule
			int const ControlAirNodeID, // index of roomair node that is HVAC control sensor location
			int const NumOfAirNodes, // Number of air nodes
			Array1< RoomAirflowNetworkAirNodeNestedStruct > const & Node, // Node struct
			int const ReturnAirNodeID, // index in system Node array
			int const ZoneNodeID, // index in system Node array for this zone
			Real64 const TairMean, // comes from MAT
			Real64 const Tstat, // temperature for thermostat
			Real64 const Tleaving, // temperature for return air node
			Real64 const Texhaust, // temperature for exhaust air node
			int const totNumSurfs, // total surfs for this zone
			int const firstSurfID, // Index of first surface
			int const RAFNNum // RAFN number
		) :
			IsUsed( IsUsed ),
			Name( Name ),
			ZoneName( ZoneName ),
			ZoneID( ZoneID ),
			ActualZoneID( ActualZoneID ), // Index of controlled zones in ZoneCOnfigure
			AvailSched( AvailSched ), // Name of availability schedule
			AvailSchedID( AvailSchedID ), // index of availability schedule
			ControlAirNodeID( ControlAirNodeID ), // index of roomair node that is HVAC control sensor location
			NumOfAirNodes( NumOfAirNodes ), // Number of air nodes
			Node( Node ), // Node struct
			ReturnAirNodeID( ReturnAirNodeID ), // index in system Node array
			ZoneNodeID( ZoneNodeID ), // index in system Node array for this zone
			TairMean( TairMean ), // comes from MAT
			Tstat( Tstat ), // temperature for thermostat
			Tleaving( Tleaving ), // temperature for return air node
			Texhaust( Texhaust ), // temperature for exhaust air node
			totNumSurfs( totNumSurfs ), // total surfs for this zone
			firstSurfID( firstSurfID ), // Index of first surface
			RAFNNum( RAFNNum ) // RAFN number
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
	extern Array1D< RoomAirflowNetworkInfoByZoneStruct > RoomAirflowNetworkZoneInfo; // added zone info 

} // DataRoomAirModel

} // EnergyPlus

#endif
