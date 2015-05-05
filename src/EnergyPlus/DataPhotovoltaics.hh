#ifndef DataPhotovoltaics_hh_INCLUDED
#define DataPhotovoltaics_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataPhotovoltaics {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern std::string const cPVGeneratorObjectName;
	extern std::string const cPVSimplePerfObjectName;
	extern std::string const cPVEquiv1DiodePerfObjectName;
	extern std::string const cPVSandiaPerfObjectName;

	extern int const iNotYetSetPVModel;
	extern int const iSimplePVModel;
	extern int const iTRNSYSPVModel;
	extern int const iSandiaPVModel;

	extern int const iNotYetSetCellIntegration; // cell temp method not set
	extern int const iDecoupledCellIntegration; // cell temp method based on energy balance
	extern int const iDecoupledUllebergDynamicCellIntegration; // cell temp method based on energy bal with capacity
	extern int const iSurfaceOutsideFaceCellIntegration; // cell temp method based on coupling to E+'s heat balance
	extern int const iTranspiredCollectorCellIntegration; // cell temp method based on coupling to unglazed transpired co
	extern int const iExteriorVentedCavityCellIntegration; // cell temp method based on coupling to nat vent exterior cavi
	extern int const iPVTSolarCollectorCellIntegration; // cell temp method based on coupling to PVT model

	extern int const FixedEfficiency; // simple PV, constant efficiency
	extern int const ScheduledEfficiency; // simpel PV, scheduled efficiency

	extern int const CrystallineSiPVCells;
	extern int const AmorphousSiPVCells;

	extern Real64 const MinIrradiance; // [W/m2] Assume no operation if Ic is below this number (W/m2)
	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern int NumPVs; // count of number of PV generators
	extern int Num1DiodePVModuleTypes; // count for Equivalent one-diode model
	extern int NumSimplePVModuleTypes; // count of number of input objs for simple model
	extern int NumSNLPVModuleTypes; // count of number of input objs for Sandia model

	extern Real64 ShuntResistance; // old "RSH" in common block of trnsys code

	// Types

	struct SimplePVParamsStruct
	{
		// Members
		std::string Name; // name as identified in Sandia database
		Real64 AreaCol; // effective area of solar collection
		Real64 ActiveFraction; // fraction of parent surface that has active solar cells
		int EfficencyInputMode; // to schedule or not
		int EffSchedPtr; // index pointer for efficiency schedule
		Real64 PVEfficiency; // fixed or current PV efficiency

		// Default Constructor
		SimplePVParamsStruct() :
			AreaCol( 0.0 ),
			ActiveFraction( 0.0 ),
			EfficencyInputMode( 0 ),
			EffSchedPtr( 0 ),
			PVEfficiency( 0.0 )
		{}

		// Member Constructor
		SimplePVParamsStruct(
			std::string const & Name, // name as identified in Sandia database
			Real64 const AreaCol, // effective area of solar collection
			Real64 const ActiveFraction, // fraction of parent surface that has active solar cells
			int const EfficencyInputMode, // to schedule or not
			int const EffSchedPtr, // index pointer for efficiency schedule
			Real64 const PVEfficiency // fixed or current PV efficiency
		) :
			Name( Name ),
			AreaCol( AreaCol ),
			ActiveFraction( ActiveFraction ),
			EfficencyInputMode( EfficencyInputMode ),
			EffSchedPtr( EffSchedPtr ),
			PVEfficiency( PVEfficiency )
		{}

	};

	struct TRNSYSPVModuleParamsStruct // for  GENERATOR:PV:Equivalent One-Diode Model
	{
		// Members
		std::string Name;
		int CellsInSeries; // cells in series [-]
		int CellType; // type of PV cell (crystalline, amorphous )
		Real64 Area; // module area [m2]
		Real64 TauAlpha; // tau alpha product at normal incidence [-]
		Real64 SemiConductorBandgap; // electron bandgap [eV]
		Real64 ShuntResistance; // shunt resistance [ohms]
		Real64 RefIsc; // short circuit current at reference conditions [A/K]
		Real64 RefVoc; // open circuit voltage at reference conditions [V/K]
		Real64 RefTemperature; // temperature at reference conditions
		Real64 RefInsolation; // radiation at reference conditions [W/m2]
		Real64 Imp; // current at max power [A]
		Real64 Vmp; // voltage at max power [V]
		Real64 TempCoefIsc; // temperature coefficient of short circuit current
		Real64 TempCoefVoc; // temperature coefficient of open circuit voltage
		Real64 NOCTAmbTemp; // ambient temperature at NOCT [C]
		Real64 NOCTCellTemp; // cell temperature at NOCT [C]
		Real64 NOCTInsolation; // radiation at NOCT [W/m2]
		Real64 HeatLossCoef; // heat loss coefficient [W/m2.K]
		Real64 HeatCapacity; // total heat capacity (only used in TC mode 1)

		// Default Constructor
		TRNSYSPVModuleParamsStruct() :
			CellsInSeries( 0 ),
			CellType( 0 ),
			Area( 0.0 ),
			TauAlpha( 0.0 ),
			SemiConductorBandgap( 0.0 ),
			ShuntResistance( 0.0 ),
			RefIsc( 0.0 ),
			RefVoc( 0.0 ),
			RefTemperature( 0.0 ),
			RefInsolation( 0.0 ),
			Imp( 0.0 ),
			Vmp( 0.0 ),
			TempCoefIsc( 0.0 ),
			TempCoefVoc( 0.0 ),
			NOCTAmbTemp( 0.0 ),
			NOCTCellTemp( 0.0 ),
			NOCTInsolation( 0.0 ),
			HeatLossCoef( 0.0 ),
			HeatCapacity( 0.0 )
		{}

		// Member Constructor
		TRNSYSPVModuleParamsStruct(
			std::string const & Name,
			int const CellsInSeries, // cells in series [-]
			int const CellType, // type of PV cell (crystalline, amorphous )
			Real64 const Area, // module area [m2]
			Real64 const TauAlpha, // tau alpha product at normal incidence [-]
			Real64 const SemiConductorBandgap, // electron bandgap [eV]
			Real64 const ShuntResistance, // shunt resistance [ohms]
			Real64 const RefIsc, // short circuit current at reference conditions [A/K]
			Real64 const RefVoc, // open circuit voltage at reference conditions [V/K]
			Real64 const RefTemperature, // temperature at reference conditions
			Real64 const RefInsolation, // radiation at reference conditions [W/m2]
			Real64 const Imp, // current at max power [A]
			Real64 const Vmp, // voltage at max power [V]
			Real64 const TempCoefIsc, // temperature coefficient of short circuit current
			Real64 const TempCoefVoc, // temperature coefficient of open circuit voltage
			Real64 const NOCTAmbTemp, // ambient temperature at NOCT [C]
			Real64 const NOCTCellTemp, // cell temperature at NOCT [C]
			Real64 const NOCTInsolation, // radiation at NOCT [W/m2]
			Real64 const HeatLossCoef, // heat loss coefficient [W/m2.K]
			Real64 const HeatCapacity // total heat capacity (only used in TC mode 1)
		) :
			Name( Name ),
			CellsInSeries( CellsInSeries ),
			CellType( CellType ),
			Area( Area ),
			TauAlpha( TauAlpha ),
			SemiConductorBandgap( SemiConductorBandgap ),
			ShuntResistance( ShuntResistance ),
			RefIsc( RefIsc ),
			RefVoc( RefVoc ),
			RefTemperature( RefTemperature ),
			RefInsolation( RefInsolation ),
			Imp( Imp ),
			Vmp( Vmp ),
			TempCoefIsc( TempCoefIsc ),
			TempCoefVoc( TempCoefVoc ),
			NOCTAmbTemp( NOCTAmbTemp ),
			NOCTCellTemp( NOCTCellTemp ),
			NOCTInsolation( NOCTInsolation ),
			HeatLossCoef( HeatLossCoef ),
			HeatCapacity( HeatCapacity )
		{}

	};

	struct TRNSYSPVCalcStruct
	{
		// Members
		Real64 Insolation; // radiation [W/m2]
		Real64 ArrayCurrent; // array current at current conditions [A]
		Real64 ArrayVoltage; // array voltage at current conditions [V]
		Real64 ArrayPower; // array power at current conditions [W]
		Real64 ArrayEfficiency; // array efficiency at current conditions [0..1]
		Real64 CellTemp; // array cell temperature at current conditions [C]
		Real64 CellTempK; // array cell temperature (for setting last cell temp) [K]
		Real64 TimeElapsed; // time previous update of last cell temp
		Real64 LastCellTempK; // array cell temperature at previous conditions [K]
		Real64 ArrayIsc; // array short circuit current at current conditions [A]
		Real64 ArrayVoc; // array open circuit voltage at current conditions [V]

		// Default Constructor
		TRNSYSPVCalcStruct() :
			Insolation( 0.0 ),
			ArrayCurrent( 0.0 ),
			ArrayVoltage( 0.0 ),
			ArrayPower( 0.0 ),
			ArrayEfficiency( 0.0 ),
			CellTemp( 0.0 ),
			CellTempK( 0.0 ),
			TimeElapsed( 0.0 ),
			LastCellTempK( 0.0 ),
			ArrayIsc( 0.0 ),
			ArrayVoc( 0.0 )
		{}

		// Member Constructor
		TRNSYSPVCalcStruct(
			Real64 const Insolation, // radiation [W/m2]
			Real64 const ArrayCurrent, // array current at current conditions [A]
			Real64 const ArrayVoltage, // array voltage at current conditions [V]
			Real64 const ArrayPower, // array power at current conditions [W]
			Real64 const ArrayEfficiency, // array efficiency at current conditions [0..1]
			Real64 const CellTemp, // array cell temperature at current conditions [C]
			Real64 const CellTempK, // array cell temperature (for setting last cell temp) [K]
			Real64 const TimeElapsed, // time previous update of last cell temp
			Real64 const LastCellTempK, // array cell temperature at previous conditions [K]
			Real64 const ArrayIsc, // array short circuit current at current conditions [A]
			Real64 const ArrayVoc // array open circuit voltage at current conditions [V]
		) :
			Insolation( Insolation ),
			ArrayCurrent( ArrayCurrent ),
			ArrayVoltage( ArrayVoltage ),
			ArrayPower( ArrayPower ),
			ArrayEfficiency( ArrayEfficiency ),
			CellTemp( CellTemp ),
			CellTempK( CellTempK ),
			TimeElapsed( TimeElapsed ),
			LastCellTempK( LastCellTempK ),
			ArrayIsc( ArrayIsc ),
			ArrayVoc( ArrayVoc )
		{}

	};

	struct SNLModuleParamsStuct // for PV MODULE:SANDIA PARAMETERS
	{
		// Members
		std::string name; // name as identified in Sandia database
		Real64 Acoll; // Active collector area (m2, single module)
		Real64 NcellSer; // Number of cells in series in a module's cell-string (unitless)
		Real64 NparSerCells; // Number of cell-strings in parallel in module (unitless)
		Real64 Isc0; // Short circuit current at reference conditions (Amps)
		Real64 Voc0; // Open circuit voltage at reference conditions (Volts)
		Real64 Imp0; // Max power point current at reference conditions (Amps)
		Real64 Vmp0; // Voltage at max power at reference conditions (Volts)
		Real64 aIsc; // Normalized temperature coefficient for Isc (Amps/degC) Isc temperature coeff
		Real64 aImp; // Normalized temperature coefficient for Imp (1/degC) Imp temperature coeff
		Real64 c_0; // Empirical coefficients relating Imp to Ee (unitless)
		//   coefficient relating Imp to irradiance
		Real64 c_1; // Empirical coefficients relating Imp to Ee (unitless)
		//   coefficient relating Voc to irradiance
		Real64 BVoc0; // Temperature coefficient for module open-circuit-voltage at reference conditions
		//   (Volts/degC)
		Real64 mBVoc; // Coefficient for irradiance dependence of open-circuit-voltage-temperature
		//  coefficient  (V/°C)
		Real64 BVmp0; // Temperature coefficient for module maximum-power-voltage at reference conditions
		//   (V/°C)
		Real64 mBVmp; // Cofficient for irradiance dependence of maximum-power-voltage-temperature
		//   coefficient (V/°C)
		Real64 DiodeFactor; // Empirically determined 'diode factor' for individual cells (unitless)
		Real64 c_2; // Empirical coefficients relating Vmp to Ee (unitless)
		//   (coefficient relating Vmp to irradiance)
		Real64 c_3; // Empirical coefficients relating Vmp to Ee (unitless)
		//   (coefficient relating Vmp to irradiance)
		Real64 a_0; // Empirical coefficients for f1(AMa) polynomial (unitless)
		Real64 a_1; // Empirical coefficients for f1(AMa) polynomial (unitless)
		Real64 a_2; // Empirical coefficients for f1(AMa) polynomial (unitless)
		Real64 a_3; // Empirical coefficients for f1(AMa) polynomial (unitless)
		Real64 a_4; // Empirical coefficients for f1(AMa) polynomial (unitless)
		Real64 b_0; // Empirical coefficients for f1(AOI) polynomial (unitless)
		Real64 b_1; // Empirical coefficients for f1(AOI) polynomial (unitless)
		Real64 b_2; // Empirical coefficients for f1(AOI) polynomial (unitless)
		Real64 b_3; // Empirical coefficients for f1(AOI) polynomial (unitless)
		Real64 b_4; // Empirical coefficients for f1(AOI) polynomial (unitless)
		Real64 b_5; // Empirical coefficients for f1(AOI) polynomial (unitless)
		Real64 DT0; // Temperature difference between Tc and Tm at Eo (°C),
		// (This is d(Tc) in Sandia database)
		Real64 fd; // Fraction of diffuse irradiance used by module (unitless)
		Real64 a; // Empirical coefficient for module temp.at low wind,
		// high solar irradiance (unitless)
		Real64 b; // Empirical coefficient relating module temp.
		// decrease with increasing wind speed (unitless)
		Real64 c_4; // Empirical coefficients relating Ix to Ee (unitless)
		Real64 c_5; // Empirical coefficients relating Ix to Ee (unitless)
		Real64 Ix0; // Current at V = 0.5 Voc and at reference conditions (Amps)
		Real64 Ixx0; // Current at V = 0.5 (Vmp + Voc) and at reference conditions (Amps)
		Real64 c_6; // Empirical coefficients relating Ixx to Ee (unitless)
		Real64 c_7; // Empirical coefficients relating Ixx to Ee (unitless)

		// Default Constructor
		SNLModuleParamsStuct() :
			Acoll( 0.0 ),
			NcellSer( 0.0 ),
			NparSerCells( 0.0 ),
			Isc0( 0.0 ),
			Voc0( 0.0 ),
			Imp0( 0.0 ),
			Vmp0( 0.0 ),
			aIsc( 0.0 ),
			aImp( 0.0 ),
			c_0( 0.0 ),
			c_1( 0.0 ),
			BVoc0( 0.0 ),
			mBVoc( 0.0 ),
			BVmp0( 0.0 ),
			mBVmp( 0.0 ),
			DiodeFactor( 0.0 ),
			c_2( 0.0 ),
			c_3( 0.0 ),
			a_0( 0.0 ),
			a_1( 0.0 ),
			a_2( 0.0 ),
			a_3( 0.0 ),
			a_4( 0.0 ),
			b_0( 0.0 ),
			b_1( 0.0 ),
			b_2( 0.0 ),
			b_3( 0.0 ),
			b_4( 0.0 ),
			b_5( 0.0 ),
			DT0( 0.0 ),
			fd( 0.0 ),
			a( 0.0 ),
			b( 0.0 ),
			c_4( 0.0 ),
			c_5( 0.0 ),
			Ix0( 0.0 ),
			Ixx0( 0.0 ),
			c_6( 0.0 ),
			c_7( 0.0 )
		{}

		// Member Constructor
		SNLModuleParamsStuct(
			std::string const & name, // name as identified in Sandia database
			Real64 const Acoll, // Active collector area (m2, single module)
			Real64 const NcellSer, // Number of cells in series in a module's cell-string (unitless)
			Real64 const NparSerCells, // Number of cell-strings in parallel in module (unitless)
			Real64 const Isc0, // Short circuit current at reference conditions (Amps)
			Real64 const Voc0, // Open circuit voltage at reference conditions (Volts)
			Real64 const Imp0, // Max power point current at reference conditions (Amps)
			Real64 const Vmp0, // Voltage at max power at reference conditions (Volts)
			Real64 const aIsc, // Normalized temperature coefficient for Isc (Amps/degC) Isc temperature coeff
			Real64 const aImp, // Normalized temperature coefficient for Imp (1/degC) Imp temperature coeff
			Real64 const c_0, // Empirical coefficients relating Imp to Ee (unitless)
			Real64 const c_1, // Empirical coefficients relating Imp to Ee (unitless)
			Real64 const BVoc0, // Temperature coefficient for module open-circuit-voltage at reference conditions
			Real64 const mBVoc, // Coefficient for irradiance dependence of open-circuit-voltage-temperature
			Real64 const BVmp0, // Temperature coefficient for module maximum-power-voltage at reference conditions
			Real64 const mBVmp, // Cofficient for irradiance dependence of maximum-power-voltage-temperature
			Real64 const DiodeFactor, // Empirically determined 'diode factor' for individual cells (unitless)
			Real64 const c_2, // Empirical coefficients relating Vmp to Ee (unitless)
			Real64 const c_3, // Empirical coefficients relating Vmp to Ee (unitless)
			Real64 const a_0, // Empirical coefficients for f1(AMa) polynomial (unitless)
			Real64 const a_1, // Empirical coefficients for f1(AMa) polynomial (unitless)
			Real64 const a_2, // Empirical coefficients for f1(AMa) polynomial (unitless)
			Real64 const a_3, // Empirical coefficients for f1(AMa) polynomial (unitless)
			Real64 const a_4, // Empirical coefficients for f1(AMa) polynomial (unitless)
			Real64 const b_0, // Empirical coefficients for f1(AOI) polynomial (unitless)
			Real64 const b_1, // Empirical coefficients for f1(AOI) polynomial (unitless)
			Real64 const b_2, // Empirical coefficients for f1(AOI) polynomial (unitless)
			Real64 const b_3, // Empirical coefficients for f1(AOI) polynomial (unitless)
			Real64 const b_4, // Empirical coefficients for f1(AOI) polynomial (unitless)
			Real64 const b_5, // Empirical coefficients for f1(AOI) polynomial (unitless)
			Real64 const DT0, // Temperature difference between Tc and Tm at Eo (°C),
			Real64 const fd, // Fraction of diffuse irradiance used by module (unitless)
			Real64 const a, // Empirical coefficient for module temp.at low wind,
			Real64 const b, // Empirical coefficient relating module temp.
			Real64 const c_4, // Empirical coefficients relating Ix to Ee (unitless)
			Real64 const c_5, // Empirical coefficients relating Ix to Ee (unitless)
			Real64 const Ix0, // Current at V = 0.5 Voc and at reference conditions (Amps)
			Real64 const Ixx0, // Current at V = 0.5 (Vmp + Voc) and at reference conditions (Amps)
			Real64 const c_6, // Empirical coefficients relating Ixx to Ee (unitless)
			Real64 const c_7 // Empirical coefficients relating Ixx to Ee (unitless)
		) :
			name( name ),
			Acoll( Acoll ),
			NcellSer( NcellSer ),
			NparSerCells( NparSerCells ),
			Isc0( Isc0 ),
			Voc0( Voc0 ),
			Imp0( Imp0 ),
			Vmp0( Vmp0 ),
			aIsc( aIsc ),
			aImp( aImp ),
			c_0( c_0 ),
			c_1( c_1 ),
			BVoc0( BVoc0 ),
			mBVoc( mBVoc ),
			BVmp0( BVmp0 ),
			mBVmp( mBVmp ),
			DiodeFactor( DiodeFactor ),
			c_2( c_2 ),
			c_3( c_3 ),
			a_0( a_0 ),
			a_1( a_1 ),
			a_2( a_2 ),
			a_3( a_3 ),
			a_4( a_4 ),
			b_0( b_0 ),
			b_1( b_1 ),
			b_2( b_2 ),
			b_3( b_3 ),
			b_4( b_4 ),
			b_5( b_5 ),
			DT0( DT0 ),
			fd( fd ),
			a( a ),
			b( b ),
			c_4( c_4 ),
			c_5( c_5 ),
			Ix0( Ix0 ),
			Ixx0( Ixx0 ),
			c_6( c_6 ),
			c_7( c_7 )
		{}

	};

	struct SNLPVInputStruct // for data obtained elsewhere in EnergyPlus
	{
		// Members
		Real64 IcBeam; // incident beam solar (W/m2)
		Real64 IcDiffuse; // incident diffuse solar (W/m2)
		Real64 IncidenceAngle; // angle from normal for beam (deg)
		Real64 ZenithAngle; // solar zenith angle (deg)
		Real64 Tamb; // outdoor drybulb temperature (C)
		Real64 WindSpeed; // outdoor windspeed. (m/s)
		Real64 Altitude; // elevation above sea level. (m)

		// Default Constructor
		SNLPVInputStruct() :
			IcBeam( 0.0 ),
			IcDiffuse( 0.0 ),
			IncidenceAngle( 0.0 ),
			ZenithAngle( 0.0 ),
			Tamb( 0.0 ),
			WindSpeed( 0.0 ),
			Altitude( 0.0 )
		{}

		// Member Constructor
		SNLPVInputStruct(
			Real64 const IcBeam, // incident beam solar (W/m2)
			Real64 const IcDiffuse, // incident diffuse solar (W/m2)
			Real64 const IncidenceAngle, // angle from normal for beam (deg)
			Real64 const ZenithAngle, // solar zenith angle (deg)
			Real64 const Tamb, // outdoor drybulb temperature (C)
			Real64 const WindSpeed, // outdoor windspeed. (m/s)
			Real64 const Altitude // elevation above sea level. (m)
		) :
			IcBeam( IcBeam ),
			IcDiffuse( IcDiffuse ),
			IncidenceAngle( IncidenceAngle ),
			ZenithAngle( ZenithAngle ),
			Tamb( Tamb ),
			WindSpeed( WindSpeed ),
			Altitude( Altitude )
		{}

	};

	struct SNLPVCalcStruct // hold calculated results from PV modeling.
	{
		// Members
		Real64 Vmp; // (Volts) maximum power voltage
		Real64 Imp; // (Amps) maximum power current
		Real64 Pmp; // (W) (was kJ/hr) maximum power point power
		Real64 EffMax; // (unitless) conversion efficiency at max power point
		Real64 Isc; // (Amps) short circuit current
		Real64 Voc; // (Volts) open circuit voltage
		Real64 Tcell; // (deg C) solar cell operating temperature
		Real64 Tback; // (deg C) solar module operation temp, at back of module
		Real64 AMa; // (unitless) Absolute Air mass
		Real64 F1; // (unitless) holds result of "AMa-Function" for solar spectrum influence
		Real64 F2; // (unitless) holds result of AOI-Function for angle-of-incidence
		Real64 Ix; // (Amps) Current at V = 0.5 Voc
		Real64 Vx; // (Volts) Voltage at 0.5 Voc
		Real64 Ixx; // (Amps) current at V = 0.5(Vmpp + Voc)
		Real64 Vxx; // (Volts) voltage at 0.5(Vmpp + Voc)
		Real64 SurfaceSink; // (Watts) energy balance term to account for electricity leaving

		// Default Constructor
		SNLPVCalcStruct() :
			Vmp( 0.0 ),
			Imp( 0.0 ),
			Pmp( 0.0 ),
			EffMax( 0.0 ),
			Isc( 0.0 ),
			Voc( 0.0 ),
			Tcell( 0.0 ),
			Tback( 0.0 ),
			AMa( 0.0 ),
			F1( 0.0 ),
			F2( 0.0 ),
			Ix( 0.0 ),
			Vx( 0.0 ),
			Ixx( 0.0 ),
			Vxx( 0.0 ),
			SurfaceSink( 0.0 )
		{}

		// Member Constructor
		SNLPVCalcStruct(
			Real64 const Vmp, // (Volts) maximum power voltage
			Real64 const Imp, // (Amps) maximum power current
			Real64 const Pmp, // (W) (was kJ/hr) maximum power point power
			Real64 const EffMax, // (unitless) conversion efficiency at max power point
			Real64 const Isc, // (Amps) short circuit current
			Real64 const Voc, // (Volts) open circuit voltage
			Real64 const Tcell, // (deg C) solar cell operating temperature
			Real64 const Tback, // (deg C) solar module operation temp, at back of module
			Real64 const AMa, // (unitless) Absolute Air mass
			Real64 const F1, // (unitless) holds result of "AMa-Function" for solar spectrum influence
			Real64 const F2, // (unitless) holds result of AOI-Function for angle-of-incidence
			Real64 const Ix, // (Amps) Current at V = 0.5 Voc
			Real64 const Vx, // (Volts) Voltage at 0.5 Voc
			Real64 const Ixx, // (Amps) current at V = 0.5(Vmpp + Voc)
			Real64 const Vxx, // (Volts) voltage at 0.5(Vmpp + Voc)
			Real64 const SurfaceSink // (Watts) energy balance term to account for electricity leaving
		) :
			Vmp( Vmp ),
			Imp( Imp ),
			Pmp( Pmp ),
			EffMax( EffMax ),
			Isc( Isc ),
			Voc( Voc ),
			Tcell( Tcell ),
			Tback( Tback ),
			AMa( AMa ),
			F1( F1 ),
			F2( F2 ),
			Ix( Ix ),
			Vx( Vx ),
			Ixx( Ixx ),
			Vxx( Vxx ),
			SurfaceSink( SurfaceSink )
		{}

	};

	struct PVReportVariables // for  GENERATOR:PV:EQUIVALENT ONE-DIODE MODEL
	{
		// Members
		Real64 DCPower; // Direct Current power from PV array
		Real64 DCEnergy; // Direct Current energy from PV array
		Real64 ArrayEfficiency; // array efficiency at current conditions [0..1]
		Real64 CellTemp; // array cell temperature at current conditions [C]
		Real64 ArrayIsc; // array short circuit current at current conditions [A]
		Real64 ArrayVoc; // array open circuit voltage at current conditions [V]
		Real64 ArrayCurrent;
		Real64 ArrayVoltage;

		// Default Constructor
		PVReportVariables() :
			DCPower( 0.0 ),
			DCEnergy( 0.0 ),
			ArrayEfficiency( 0.0 ),
			CellTemp( 0.0 ),
			ArrayIsc( 0.0 ),
			ArrayVoc( 0.0 ),
			ArrayCurrent( 0.0 ),
			ArrayVoltage( 0.0 )
		{}

		// Member Constructor
		PVReportVariables(
			Real64 const DCPower, // Direct Current power from PV array
			Real64 const DCEnergy, // Direct Current energy from PV array
			Real64 const ArrayEfficiency, // array efficiency at current conditions [0..1]
			Real64 const CellTemp, // array cell temperature at current conditions [C]
			Real64 const ArrayIsc, // array short circuit current at current conditions [A]
			Real64 const ArrayVoc, // array open circuit voltage at current conditions [V]
			Real64 const ArrayCurrent,
			Real64 const ArrayVoltage
		) :
			DCPower( DCPower ),
			DCEnergy( DCEnergy ),
			ArrayEfficiency( ArrayEfficiency ),
			CellTemp( CellTemp ),
			ArrayIsc( ArrayIsc ),
			ArrayVoc( ArrayVoc ),
			ArrayCurrent( ArrayCurrent ),
			ArrayVoltage( ArrayVoltage )
		{}

	};

	struct PVArrayStruct
	{
		// Members
		std::string Name;
		std::string SurfaceName; // named surface in heat balance domain
		std::string PerfObjName;
		int SurfacePtr; // index for named surface
		int PVModelType; // type of performance modeling, Simple, TRNSYS or Equivalent 1-diode, or Sandia/King model
		int CellIntegrationMode; // how are PV cells integrated with other E+ modeling
		Real64 NumModNSeries; // number of modules in series in one string
		Real64 NumSeriesNParall; // number of series strings in parallel
		int UTSCPtr; // pointer to UTSC number for INTEGRATED TRANSPIRED COLLECTOR mode
		int ExtVentCavPtr; // pointer to Exterior Vented Cavity EXTERIOR VENTED CAVITY
		int PVTPtr; // pointer to PVT model
		Real64 SurfaceSink; // PV power "sink" for integration
		PVReportVariables Report; // report variables
		// nested structs for user input parameters
		SimplePVParamsStruct SimplePVModule; // simple model input params
		TRNSYSPVModuleParamsStruct TRNSYSPVModule; // equivalent one-diode input params
		SNLModuleParamsStuct SNLPVModule; // Sandia/King model input parameter data
		//nested structs for model input from elsewhere and calculations
		TRNSYSPVCalcStruct TRNSYSPVcalc;
		SNLPVInputStruct SNLPVinto; // model input from elsewhere in EnergyPlus
		SNLPVCalcStruct SNLPVCalc; // calc'd data for GENERATOR:PV:Sandia model

		// Default Constructor
		PVArrayStruct() :
			SurfacePtr( 0 ),
			PVModelType( 0 ),
			CellIntegrationMode( 0 ),
			NumModNSeries( 1.0 ),
			NumSeriesNParall( 1.0 ),
			UTSCPtr( 0 ),
			ExtVentCavPtr( 0 ),
			PVTPtr( 0 ),
			SurfaceSink( 0.0 )
		{}

		// Member Constructor
		PVArrayStruct(
			std::string const & Name,
			std::string const & SurfaceName, // named surface in heat balance domain
			std::string const & PerfObjName,
			int const SurfacePtr, // index for named surface
			int const PVModelType, // type of performance modeling, Simple, TRNSYS or Equivalent 1-diode, or Sandia/King model
			int const CellIntegrationMode, // how are PV cells integrated with other E+ modeling
			Real64 const NumModNSeries, // number of modules in series in one string
			Real64 const NumSeriesNParall, // number of series strings in parallel
			int const UTSCPtr, // pointer to UTSC number for INTEGRATED TRANSPIRED COLLECTOR mode
			int const ExtVentCavPtr, // pointer to Exterior Vented Cavity EXTERIOR VENTED CAVITY
			int const PVTPtr, // pointer to PVT model
			Real64 const SurfaceSink, // PV power "sink" for integration
			PVReportVariables const & Report, // report variables
			SimplePVParamsStruct const & SimplePVModule, // simple model input params
			TRNSYSPVModuleParamsStruct const & TRNSYSPVModule, // equivalent one-diode input params
			SNLModuleParamsStuct const & SNLPVModule, // Sandia/King model input parameter data
			TRNSYSPVCalcStruct const & TRNSYSPVcalc,
			SNLPVInputStruct const & SNLPVinto, // model input from elsewhere in EnergyPlus
			SNLPVCalcStruct const & SNLPVCalc // calc'd data for GENERATOR:PV:Sandia model
		) :
			Name( Name ),
			SurfaceName( SurfaceName ),
			PerfObjName( PerfObjName ),
			SurfacePtr( SurfacePtr ),
			PVModelType( PVModelType ),
			CellIntegrationMode( CellIntegrationMode ),
			NumModNSeries( NumModNSeries ),
			NumSeriesNParall( NumSeriesNParall ),
			UTSCPtr( UTSCPtr ),
			ExtVentCavPtr( ExtVentCavPtr ),
			PVTPtr( PVTPtr ),
			SurfaceSink( SurfaceSink ),
			Report( Report ),
			SimplePVModule( SimplePVModule ),
			TRNSYSPVModule( TRNSYSPVModule ),
			SNLPVModule( SNLPVModule ),
			TRNSYSPVcalc( TRNSYSPVcalc ),
			SNLPVinto( SNLPVinto ),
			SNLPVCalc( SNLPVCalc )
		{}

	};

	// Object Data
	extern Array1D< PVArrayStruct > PVarray;

} // DataPhotovoltaics

} // EnergyPlus

#endif
