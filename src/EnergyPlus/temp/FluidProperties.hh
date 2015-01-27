#ifndef FluidProperties_hh_INCLUDED
#define FluidProperties_hh_INCLUDED

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/FArray2D.hh>
#include <ObjexxFCL/FArray2S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace FluidProperties {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern std::string const Refrig;
	extern std::string const Glycol;
	extern std::string const Pressure;
	extern std::string const Enthalpy;
	extern std::string const Density;
	extern std::string const SpecificHeat;
	extern std::string const Conductivity;
	extern std::string const Viscosity;
	extern std::string const Fluid;
	extern std::string const GasFluid;
	extern std::string const Water;
	extern std::string const Steam;
	extern std::string const EthyleneGlycol;
	extern std::string const PropyleneGlycol;
	extern int const EthyleneGlycolIndex;
	extern int const PropyleneGlycolIndex;
	extern int const iRefrig;
	extern int const iGlycol;

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS
	extern bool GetInput; // Used to get the input once only
	extern int NumOfRefrigerants; // Total number of refrigerants input by user
	extern int NumOfGlycols; // Total number of glycols input by user
	extern bool DebugReportGlycols;
	extern bool DebugReportRefrigerants;
	extern int GlycolErrorLimitTest; // how many times error is printed with details before recurring called
	extern int RefrigerantErrorLimitTest; // how many times error is printed with details before recurring called
	extern FArray1D_bool RefrigUsed;
	extern FArray1D_bool GlycolUsed;
	extern int FluidIndex_Water;
	extern int FluidIndex_EthyleneGlycol;
	extern int FluidIndex_PropoleneGlycol;

	// ACCESSIBLE SPECIFICATIONS OF MODULE SUBROUTINES OR FUNCTONS:

	// Types

	struct FluidPropsRefrigerantData
	{
		// Members
		std::string Name; // Name of the refrigerant
		int NumPsPoints; // Number of saturation pressure
		Real64 PsLowTempValue; // Low Temperature Value for Ps (>0.0)
		Real64 PsHighTempValue; // High Temperature Value for Ps (max in tables)
		int PsLowTempIndex; // Low Temperature Min Index for Ps (>0.0)
		int PsHighTempIndex; // High Temperature Max Index for Ps (>0.0)
		Real64 PsLowPresValue; // Low Pressure Value for Ps (>0.0)
		Real64 PsHighPresValue; // High Pressure Value for Ps (max in tables)
		int PsLowPresIndex; // Low Pressure Min Index for Ps (>0.0)
		int PsHighPresIndex; // High Pressure Max Index for Ps (>0.0)
		FArray1D< Real64 > PsTemps; // Temperatures for saturation pressures
		FArray1D< Real64 > PsValues; // Saturation pressures at PsTemps
		int NumHPoints; // Number of enthalpy points
		Real64 HfLowTempValue; // Low Temperature Value for Hf (>0.0)
		Real64 HfHighTempValue; // High Temperature Value for Hf (max in tables)
		int HfLowTempIndex; // Low Temperature Min Index for Hf (>0.0)
		int HfHighTempIndex; // High Temperature Max Index for Hf (>0.0)
		Real64 HfgLowTempValue; // Low Temperature Value for Hfg (>0.0)
		Real64 HfgHighTempValue; // High Temperature Value for Hfg (max in tables)
		int HfgLowTempIndex; // Low Temperature Min Index for Hfg (>0.0)
		int HfgHighTempIndex; // High Temperature Max Index for Hfg (>0.0)
		FArray1D< Real64 > HTemps; // Temperatures for enthalpy points
		FArray1D< Real64 > HfValues; // Enthalpy of saturated fluid at HTemps
		FArray1D< Real64 > HfgValues; // Enthalpy of saturated fluid/gas at HTemps
		int NumCpPoints; // Number of specific heat of fluid points
		Real64 CpfLowTempValue; // Low Temperature Value for Cpf (>0.0)
		Real64 CpfHighTempValue; // High Temperature Value for Cpf (max in tables)
		int CpfLowTempIndex; // Low Temperature Min Index for Cpf (>0.0)
		int CpfHighTempIndex; // High Temperature Max Index for Cpf (>0.0)
		Real64 CpfgLowTempValue; // Low Temperature Value for Cpfg (>0.0)
		Real64 CpfgHighTempValue; // High Temperature Value for Cpfg (max in tables)
		int CpfgLowTempIndex; // Low Temperature Min Index for Cpfg (>0.0)
		int CpfgHighTempIndex; // High Temperature Max Index for Cpfg (>0.0)
		FArray1D< Real64 > CpTemps; // Temperatures for specific heat points
		FArray1D< Real64 > CpfValues; // Specific heat of saturated fluid at CpTemps
		FArray1D< Real64 > CpfgValues; // Specific heat of saturated fluid/gas at CpTemps
		int NumRhoPoints; // Number of density of fluid points
		Real64 RhofLowTempValue; // Low Temperature Value for Rhof (>0.0)
		Real64 RhofHighTempValue; // High Temperature Value for Rhof (max in tables)
		int RhofLowTempIndex; // Low Temperature Min Index for Rhof (>0.0)
		int RhofHighTempIndex; // High Temperature Max Index for Rhof (>0.0)
		Real64 RhofgLowTempValue; // Low Temperature Value for Rhofg (>0.0)
		Real64 RhofgHighTempValue; // High Temperature Value for Rhofg (max in tables)
		int RhofgLowTempIndex; // Low Temperature Min Index for Rhofg (>0.0)
		int RhofgHighTempIndex; // High Temperature Max Index for Rhofg (>0.0)
		FArray1D< Real64 > RhoTemps; // Temperatures for density of fluid points
		FArray1D< Real64 > RhofValues; // Density of saturated fluid at RhoTemps
		FArray1D< Real64 > RhofgValues; // Density of saturated fluid/gas at RhoTemps
		int NumSuperTempPts; // Number of temperature points for superheated enthalpy
		int NumSuperPressPts; // Number of pressure points for superheated enthalpy
		FArray1D< Real64 > SHTemps; // Temperatures for superheated gas
		FArray1D< Real64 > SHPress; // Pressures for superheated gas
		FArray2D< Real64 > HshValues; // Enthalpy of superheated gas at HshTemps, HshPress
		FArray2D< Real64 > RhoshValues; // Density of superheated gas at HshTemps, HshPress

		// Default Constructor
		FluidPropsRefrigerantData() :
			NumPsPoints( 0 ),
			PsLowTempValue( 0.0 ),
			PsHighTempValue( 0.0 ),
			PsLowTempIndex( 0 ),
			PsHighTempIndex( 0 ),
			PsLowPresValue( 0.0 ),
			PsHighPresValue( 0.0 ),
			PsLowPresIndex( 0 ),
			PsHighPresIndex( 0 ),
			NumHPoints( 0 ),
			HfLowTempValue( 0.0 ),
			HfHighTempValue( 0.0 ),
			HfLowTempIndex( 0 ),
			HfHighTempIndex( 0 ),
			HfgLowTempValue( 0.0 ),
			HfgHighTempValue( 0.0 ),
			HfgLowTempIndex( 0 ),
			HfgHighTempIndex( 0 ),
			NumCpPoints( 0 ),
			CpfLowTempValue( 0.0 ),
			CpfHighTempValue( 0.0 ),
			CpfLowTempIndex( 0 ),
			CpfHighTempIndex( 0 ),
			CpfgLowTempValue( 0.0 ),
			CpfgHighTempValue( 0.0 ),
			CpfgLowTempIndex( 0 ),
			CpfgHighTempIndex( 0 ),
			NumRhoPoints( 0 ),
			RhofLowTempValue( 0.0 ),
			RhofHighTempValue( 0.0 ),
			RhofLowTempIndex( 0 ),
			RhofHighTempIndex( 0 ),
			RhofgLowTempValue( 0.0 ),
			RhofgHighTempValue( 0.0 ),
			RhofgLowTempIndex( 0 ),
			RhofgHighTempIndex( 0 ),
			NumSuperTempPts( 0 ),
			NumSuperPressPts( 0 )
		{}

		// Member Constructor
		FluidPropsRefrigerantData(
			std::string const & Name, // Name of the refrigerant
			int const NumPsPoints, // Number of saturation pressure
			Real64 const PsLowTempValue, // Low Temperature Value for Ps (>0.0)
			Real64 const PsHighTempValue, // High Temperature Value for Ps (max in tables)
			int const PsLowTempIndex, // Low Temperature Min Index for Ps (>0.0)
			int const PsHighTempIndex, // High Temperature Max Index for Ps (>0.0)
			Real64 const PsLowPresValue, // Low Pressure Value for Ps (>0.0)
			Real64 const PsHighPresValue, // High Pressure Value for Ps (max in tables)
			int const PsLowPresIndex, // Low Pressure Min Index for Ps (>0.0)
			int const PsHighPresIndex, // High Pressure Max Index for Ps (>0.0)
			FArray1< Real64 > const & PsTemps, // Temperatures for saturation pressures
			FArray1< Real64 > const & PsValues, // Saturation pressures at PsTemps
			int const NumHPoints, // Number of enthalpy points
			Real64 const HfLowTempValue, // Low Temperature Value for Hf (>0.0)
			Real64 const HfHighTempValue, // High Temperature Value for Hf (max in tables)
			int const HfLowTempIndex, // Low Temperature Min Index for Hf (>0.0)
			int const HfHighTempIndex, // High Temperature Max Index for Hf (>0.0)
			Real64 const HfgLowTempValue, // Low Temperature Value for Hfg (>0.0)
			Real64 const HfgHighTempValue, // High Temperature Value for Hfg (max in tables)
			int const HfgLowTempIndex, // Low Temperature Min Index for Hfg (>0.0)
			int const HfgHighTempIndex, // High Temperature Max Index for Hfg (>0.0)
			FArray1< Real64 > const & HTemps, // Temperatures for enthalpy points
			FArray1< Real64 > const & HfValues, // Enthalpy of saturated fluid at HTemps
			FArray1< Real64 > const & HfgValues, // Enthalpy of saturated fluid/gas at HTemps
			int const NumCpPoints, // Number of specific heat of fluid points
			Real64 const CpfLowTempValue, // Low Temperature Value for Cpf (>0.0)
			Real64 const CpfHighTempValue, // High Temperature Value for Cpf (max in tables)
			int const CpfLowTempIndex, // Low Temperature Min Index for Cpf (>0.0)
			int const CpfHighTempIndex, // High Temperature Max Index for Cpf (>0.0)
			Real64 const CpfgLowTempValue, // Low Temperature Value for Cpfg (>0.0)
			Real64 const CpfgHighTempValue, // High Temperature Value for Cpfg (max in tables)
			int const CpfgLowTempIndex, // Low Temperature Min Index for Cpfg (>0.0)
			int const CpfgHighTempIndex, // High Temperature Max Index for Cpfg (>0.0)
			FArray1< Real64 > const & CpTemps, // Temperatures for specific heat points
			FArray1< Real64 > const & CpfValues, // Specific heat of saturated fluid at CpTemps
			FArray1< Real64 > const & CpfgValues, // Specific heat of saturated fluid/gas at CpTemps
			int const NumRhoPoints, // Number of density of fluid points
			Real64 const RhofLowTempValue, // Low Temperature Value for Rhof (>0.0)
			Real64 const RhofHighTempValue, // High Temperature Value for Rhof (max in tables)
			int const RhofLowTempIndex, // Low Temperature Min Index for Rhof (>0.0)
			int const RhofHighTempIndex, // High Temperature Max Index for Rhof (>0.0)
			Real64 const RhofgLowTempValue, // Low Temperature Value for Rhofg (>0.0)
			Real64 const RhofgHighTempValue, // High Temperature Value for Rhofg (max in tables)
			int const RhofgLowTempIndex, // Low Temperature Min Index for Rhofg (>0.0)
			int const RhofgHighTempIndex, // High Temperature Max Index for Rhofg (>0.0)
			FArray1< Real64 > const & RhoTemps, // Temperatures for density of fluid points
			FArray1< Real64 > const & RhofValues, // Density of saturated fluid at RhoTemps
			FArray1< Real64 > const & RhofgValues, // Density of saturated fluid/gas at RhoTemps
			int const NumSuperTempPts, // Number of temperature points for superheated enthalpy
			int const NumSuperPressPts, // Number of pressure points for superheated enthalpy
			FArray1< Real64 > const & SHTemps, // Temperatures for superheated gas
			FArray1< Real64 > const & SHPress, // Pressures for superheated gas
			FArray2< Real64 > const & HshValues, // Enthalpy of superheated gas at HshTemps, HshPress
			FArray2< Real64 > const & RhoshValues // Density of superheated gas at HshTemps, HshPress
		) :
			Name( Name ),
			NumPsPoints( NumPsPoints ),
			PsLowTempValue( PsLowTempValue ),
			PsHighTempValue( PsHighTempValue ),
			PsLowTempIndex( PsLowTempIndex ),
			PsHighTempIndex( PsHighTempIndex ),
			PsLowPresValue( PsLowPresValue ),
			PsHighPresValue( PsHighPresValue ),
			PsLowPresIndex( PsLowPresIndex ),
			PsHighPresIndex( PsHighPresIndex ),
			PsTemps( PsTemps ),
			PsValues( PsValues ),
			NumHPoints( NumHPoints ),
			HfLowTempValue( HfLowTempValue ),
			HfHighTempValue( HfHighTempValue ),
			HfLowTempIndex( HfLowTempIndex ),
			HfHighTempIndex( HfHighTempIndex ),
			HfgLowTempValue( HfgLowTempValue ),
			HfgHighTempValue( HfgHighTempValue ),
			HfgLowTempIndex( HfgLowTempIndex ),
			HfgHighTempIndex( HfgHighTempIndex ),
			HTemps( HTemps ),
			HfValues( HfValues ),
			HfgValues( HfgValues ),
			NumCpPoints( NumCpPoints ),
			CpfLowTempValue( CpfLowTempValue ),
			CpfHighTempValue( CpfHighTempValue ),
			CpfLowTempIndex( CpfLowTempIndex ),
			CpfHighTempIndex( CpfHighTempIndex ),
			CpfgLowTempValue( CpfgLowTempValue ),
			CpfgHighTempValue( CpfgHighTempValue ),
			CpfgLowTempIndex( CpfgLowTempIndex ),
			CpfgHighTempIndex( CpfgHighTempIndex ),
			CpTemps( CpTemps ),
			CpfValues( CpfValues ),
			CpfgValues( CpfgValues ),
			NumRhoPoints( NumRhoPoints ),
			RhofLowTempValue( RhofLowTempValue ),
			RhofHighTempValue( RhofHighTempValue ),
			RhofLowTempIndex( RhofLowTempIndex ),
			RhofHighTempIndex( RhofHighTempIndex ),
			RhofgLowTempValue( RhofgLowTempValue ),
			RhofgHighTempValue( RhofgHighTempValue ),
			RhofgLowTempIndex( RhofgLowTempIndex ),
			RhofgHighTempIndex( RhofgHighTempIndex ),
			RhoTemps( RhoTemps ),
			RhofValues( RhofValues ),
			RhofgValues( RhofgValues ),
			NumSuperTempPts( NumSuperTempPts ),
			NumSuperPressPts( NumSuperPressPts ),
			SHTemps( SHTemps ),
			SHPress( SHPress ),
			HshValues( HshValues ),
			RhoshValues( RhoshValues )
		{}

	};

	struct FluidPropsGlycolRawData
	{
		// Members
		std::string Name; // Name of the glycol
		bool CpDataPresent; // Flag set when specific heat data is available
		int NumCpTempPts; // Number of temperature points for specific heat
		int NumCpConcPts; // Number of concentration points for specific heat
		FArray1D< Real64 > CpTemps; // Temperatures for specific heat of glycol
		FArray1D< Real64 > CpConcs; // Concentration for specific heat of glycol
		FArray2D< Real64 > CpValues; // Specific heat data values
		bool RhoDataPresent; // Flag set when density data is available
		int NumRhoTempPts; // Number of temperature points for density
		int NumRhoConcPts; // Number of concentration points for density
		FArray1D< Real64 > RhoTemps; // Temperatures for density of glycol
		FArray1D< Real64 > RhoConcs; // Concentration for density of glycol
		FArray2D< Real64 > RhoValues; // Density data values
		bool CondDataPresent; // Flag set when conductivity data is available
		int NumCondTempPts; // Number of temperature points for conductivity
		int NumCondConcPts; // Number of concentration points for conductivity
		FArray1D< Real64 > CondTemps; // Temperatures for conductivity of glycol
		FArray1D< Real64 > CondConcs; // Concentration for conductivity of glycol
		FArray2D< Real64 > CondValues; // conductivity values
		bool ViscDataPresent; // Flag set when viscosity data is available
		int NumViscTempPts; // Number of temperature points for viscosity
		int NumViscConcPts; // Number of concentration points for viscosity
		FArray1D< Real64 > ViscTemps; // Temperatures for viscosity of glycol
		FArray1D< Real64 > ViscConcs; // Concentration for viscosity of glycol
		FArray2D< Real64 > ViscValues; // viscosity values

		// Default Constructor
		FluidPropsGlycolRawData() :
			CpDataPresent( false ),
			NumCpTempPts( 0 ),
			NumCpConcPts( 0 ),
			RhoDataPresent( false ),
			NumRhoTempPts( 0 ),
			NumRhoConcPts( 0 ),
			CondDataPresent( false ),
			NumCondTempPts( 0 ),
			NumCondConcPts( 0 ),
			ViscDataPresent( false ),
			NumViscTempPts( 0 ),
			NumViscConcPts( 0 )
		{}

		// Member Constructor
		FluidPropsGlycolRawData(
			std::string const & Name, // Name of the glycol
			bool const CpDataPresent, // Flag set when specific heat data is available
			int const NumCpTempPts, // Number of temperature points for specific heat
			int const NumCpConcPts, // Number of concentration points for specific heat
			FArray1< Real64 > const & CpTemps, // Temperatures for specific heat of glycol
			FArray1< Real64 > const & CpConcs, // Concentration for specific heat of glycol
			FArray2< Real64 > const & CpValues, // Specific heat data values
			bool const RhoDataPresent, // Flag set when density data is available
			int const NumRhoTempPts, // Number of temperature points for density
			int const NumRhoConcPts, // Number of concentration points for density
			FArray1< Real64 > const & RhoTemps, // Temperatures for density of glycol
			FArray1< Real64 > const & RhoConcs, // Concentration for density of glycol
			FArray2< Real64 > const & RhoValues, // Density data values
			bool const CondDataPresent, // Flag set when conductivity data is available
			int const NumCondTempPts, // Number of temperature points for conductivity
			int const NumCondConcPts, // Number of concentration points for conductivity
			FArray1< Real64 > const & CondTemps, // Temperatures for conductivity of glycol
			FArray1< Real64 > const & CondConcs, // Concentration for conductivity of glycol
			FArray2< Real64 > const & CondValues, // conductivity values
			bool const ViscDataPresent, // Flag set when viscosity data is available
			int const NumViscTempPts, // Number of temperature points for viscosity
			int const NumViscConcPts, // Number of concentration points for viscosity
			FArray1< Real64 > const & ViscTemps, // Temperatures for viscosity of glycol
			FArray1< Real64 > const & ViscConcs, // Concentration for viscosity of glycol
			FArray2< Real64 > const & ViscValues // viscosity values
		) :
			Name( Name ),
			CpDataPresent( CpDataPresent ),
			NumCpTempPts( NumCpTempPts ),
			NumCpConcPts( NumCpConcPts ),
			CpTemps( CpTemps ),
			CpConcs( CpConcs ),
			CpValues( CpValues ),
			RhoDataPresent( RhoDataPresent ),
			NumRhoTempPts( NumRhoTempPts ),
			NumRhoConcPts( NumRhoConcPts ),
			RhoTemps( RhoTemps ),
			RhoConcs( RhoConcs ),
			RhoValues( RhoValues ),
			CondDataPresent( CondDataPresent ),
			NumCondTempPts( NumCondTempPts ),
			NumCondConcPts( NumCondConcPts ),
			CondTemps( CondTemps ),
			CondConcs( CondConcs ),
			CondValues( CondValues ),
			ViscDataPresent( ViscDataPresent ),
			NumViscTempPts( NumViscTempPts ),
			NumViscConcPts( NumViscConcPts ),
			ViscTemps( ViscTemps ),
			ViscConcs( ViscConcs ),
			ViscValues( ViscValues )
		{}

	};

	struct FluidPropsGlycolData
	{
		// Members
		std::string Name; // Name of the glycol mixture (used by other parts of code)
		std::string GlycolName; // Name of non-water fluid that is part of this mixture
		// (refers to ethylene glycol, propylene glycol, or user fluid)
		int GlycolIndex; // Index in user defined glycol data (>0 = index in raw data,
		// -1=propylene glycol, -2=ethylene glycol)
		Real64 Concentration; // Concentration (if applicable)
		bool CpDataPresent; // Flag set when specific heat data is available
		Real64 CpLowTempValue; // Low Temperature Value for Cp (>0.0)
		Real64 CpHighTempValue; // High Temperature Value for Cp (max in tables)
		int CpLowTempIndex; // Low Temperature Min Index for Cp (>0.0)
		int CpHighTempIndex; // High Temperature Max Index for Cp (>0.0)
		int NumCpTempPts; // Number of temperature points for specific heat
		FArray1D< Real64 > CpTemps; // Temperatures for specific heat of glycol
		FArray1D< Real64 > CpValues; // Specific heat data values (J/kg-K)
		bool RhoDataPresent; // Flag set when density data is available
		int NumRhoTempPts; // Number of temperature points for density
		Real64 RhoLowTempValue; // Low Temperature Value for Rho (>0.0)
		Real64 RhoHighTempValue; // High Temperature Value for Rho (max in tables)
		int RhoLowTempIndex; // Low Temperature Min Index for Rho (>0.0)
		int RhoHighTempIndex; // High Temperature Max Index for Rho (>0.0)
		FArray1D< Real64 > RhoTemps; // Temperatures for density of glycol
		FArray1D< Real64 > RhoValues; // Density data values (kg/m3)
		bool CondDataPresent; // Flag set when conductivity data is available
		int NumCondTempPts; // Number of temperature points for conductivity
		Real64 CondLowTempValue; // Low Temperature Value for Cond (>0.0)
		Real64 CondHighTempValue; // High Temperature Value for Cond (max in tables)
		int CondLowTempIndex; // Low Temperature Min Index for Cond (>0.0)
		int CondHighTempIndex; // High Temperature Max Index for Cond (>0.0)
		FArray1D< Real64 > CondTemps; // Temperatures for conductivity of glycol
		FArray1D< Real64 > CondValues; // conductivity values (W/m-K)
		bool ViscDataPresent; // Flag set when viscosity data is available
		int NumViscTempPts; // Number of temperature points for viscosity
		Real64 ViscLowTempValue; // Low Temperature Value for Visc (>0.0)
		Real64 ViscHighTempValue; // High Temperature Value for Visc (max in tables)
		int ViscLowTempIndex; // Low Temperature Min Index for Visc (>0.0)
		int ViscHighTempIndex; // High Temperature Max Index for Visc (>0.0)
		FArray1D< Real64 > ViscTemps; // Temperatures for viscosity of glycol
		FArray1D< Real64 > ViscValues; // viscosity values (mPa-s)

		// Default Constructor
		FluidPropsGlycolData() :
			GlycolIndex( 0 ),
			Concentration( 1.0 ),
			CpDataPresent( false ),
			CpLowTempValue( 0.0 ),
			CpHighTempValue( 0.0 ),
			CpLowTempIndex( 0 ),
			CpHighTempIndex( 0 ),
			NumCpTempPts( 0 ),
			RhoDataPresent( false ),
			NumRhoTempPts( 0 ),
			RhoLowTempValue( 0.0 ),
			RhoHighTempValue( 0.0 ),
			RhoLowTempIndex( 0 ),
			RhoHighTempIndex( 0 ),
			CondDataPresent( false ),
			NumCondTempPts( 0 ),
			CondLowTempValue( 0.0 ),
			CondHighTempValue( 0.0 ),
			CondLowTempIndex( 0 ),
			CondHighTempIndex( 0 ),
			ViscDataPresent( false ),
			NumViscTempPts( 0 ),
			ViscLowTempValue( 0.0 ),
			ViscHighTempValue( 0.0 ),
			ViscLowTempIndex( 0 ),
			ViscHighTempIndex( 0 )
		{}

		// Member Constructor
		FluidPropsGlycolData(
			std::string const & Name, // Name of the glycol mixture (used by other parts of code)
			std::string const & GlycolName, // Name of non-water fluid that is part of this mixture
			int const GlycolIndex, // Index in user defined glycol data (>0 = index in raw data,
			Real64 const Concentration, // Concentration (if applicable)
			bool const CpDataPresent, // Flag set when specific heat data is available
			Real64 const CpLowTempValue, // Low Temperature Value for Cp (>0.0)
			Real64 const CpHighTempValue, // High Temperature Value for Cp (max in tables)
			int const CpLowTempIndex, // Low Temperature Min Index for Cp (>0.0)
			int const CpHighTempIndex, // High Temperature Max Index for Cp (>0.0)
			int const NumCpTempPts, // Number of temperature points for specific heat
			FArray1< Real64 > const & CpTemps, // Temperatures for specific heat of glycol
			FArray1< Real64 > const & CpValues, // Specific heat data values (J/kg-K)
			bool const RhoDataPresent, // Flag set when density data is available
			int const NumRhoTempPts, // Number of temperature points for density
			Real64 const RhoLowTempValue, // Low Temperature Value for Rho (>0.0)
			Real64 const RhoHighTempValue, // High Temperature Value for Rho (max in tables)
			int const RhoLowTempIndex, // Low Temperature Min Index for Rho (>0.0)
			int const RhoHighTempIndex, // High Temperature Max Index for Rho (>0.0)
			FArray1< Real64 > const & RhoTemps, // Temperatures for density of glycol
			FArray1< Real64 > const & RhoValues, // Density data values (kg/m3)
			bool const CondDataPresent, // Flag set when conductivity data is available
			int const NumCondTempPts, // Number of temperature points for conductivity
			Real64 const CondLowTempValue, // Low Temperature Value for Cond (>0.0)
			Real64 const CondHighTempValue, // High Temperature Value for Cond (max in tables)
			int const CondLowTempIndex, // Low Temperature Min Index for Cond (>0.0)
			int const CondHighTempIndex, // High Temperature Max Index for Cond (>0.0)
			FArray1< Real64 > const & CondTemps, // Temperatures for conductivity of glycol
			FArray1< Real64 > const & CondValues, // conductivity values (W/m-K)
			bool const ViscDataPresent, // Flag set when viscosity data is available
			int const NumViscTempPts, // Number of temperature points for viscosity
			Real64 const ViscLowTempValue, // Low Temperature Value for Visc (>0.0)
			Real64 const ViscHighTempValue, // High Temperature Value for Visc (max in tables)
			int const ViscLowTempIndex, // Low Temperature Min Index for Visc (>0.0)
			int const ViscHighTempIndex, // High Temperature Max Index for Visc (>0.0)
			FArray1< Real64 > const & ViscTemps, // Temperatures for viscosity of glycol
			FArray1< Real64 > const & ViscValues // viscosity values (mPa-s)
		) :
			Name( Name ),
			GlycolName( GlycolName ),
			GlycolIndex( GlycolIndex ),
			Concentration( Concentration ),
			CpDataPresent( CpDataPresent ),
			CpLowTempValue( CpLowTempValue ),
			CpHighTempValue( CpHighTempValue ),
			CpLowTempIndex( CpLowTempIndex ),
			CpHighTempIndex( CpHighTempIndex ),
			NumCpTempPts( NumCpTempPts ),
			CpTemps( CpTemps ),
			CpValues( CpValues ),
			RhoDataPresent( RhoDataPresent ),
			NumRhoTempPts( NumRhoTempPts ),
			RhoLowTempValue( RhoLowTempValue ),
			RhoHighTempValue( RhoHighTempValue ),
			RhoLowTempIndex( RhoLowTempIndex ),
			RhoHighTempIndex( RhoHighTempIndex ),
			RhoTemps( RhoTemps ),
			RhoValues( RhoValues ),
			CondDataPresent( CondDataPresent ),
			NumCondTempPts( NumCondTempPts ),
			CondLowTempValue( CondLowTempValue ),
			CondHighTempValue( CondHighTempValue ),
			CondLowTempIndex( CondLowTempIndex ),
			CondHighTempIndex( CondHighTempIndex ),
			CondTemps( CondTemps ),
			CondValues( CondValues ),
			ViscDataPresent( ViscDataPresent ),
			NumViscTempPts( NumViscTempPts ),
			ViscLowTempValue( ViscLowTempValue ),
			ViscHighTempValue( ViscHighTempValue ),
			ViscLowTempIndex( ViscLowTempIndex ),
			ViscHighTempIndex( ViscHighTempIndex ),
			ViscTemps( ViscTemps ),
			ViscValues( ViscValues )
		{}

	};

	struct FluidPropsRefrigErrors
	{
		// Members
		std::string Name;
		int SatTempErrIndex; // Index for Sat Temperature Error (Recurring errors)
		int SatTempErrCount; // Count for Sat Temperature Error (Recurring errors)
		int SatPressErrIndex; // Index for Sat Pressure Error (Recurring errors)
		int SatPressErrCount; // Count for Sat Pressure Error (Recurring errors)
		int SatTempDensityErrIndex; // Index for Sat Temperature (Density) Error (Recurring errors)
		int SatTempDensityErrCount; // Count for Sat Temperature (Density) Error (Recurring errors)
		int SatSupEnthalpyErrIndex; // Index for Sat Temperature (Density) Error (Recurring errors)
		int SatSupEnthalpyErrCount; // Count for Sat Temperature (Density) Error (Recurring errors)
		int SatSupEnthalpyTempErrIndex; // Index for Sat Temperature (Density) Error (Recurring errors)
		int SatSupEnthalpyTempErrCount; // Count for Sat Temperature (Density) Error (Recurring errors)
		int SatSupEnthalpyPresErrIndex; // Index for Sat Temperature (Density) Error (Recurring errors)
		int SatSupEnthalpyPresErrCount; // Count for Sat Temperature (Density) Error (Recurring errors)
		int SatSupPressureErrIndex; // Index for Sat Temperature (Density) Error (Recurring errors)
		int SatSupPressureErrCount; // Count for Sat Temperature (Density) Error (Recurring errors)
		int SatSupPressureTempErrIndex; // Index for Sat Temperature (Density) Error (Recurring errors)
		int SatSupPressureTempErrCount; // Count for Sat Temperature (Density) Error (Recurring errors)
		int SatSupPressureEnthErrIndex; // Index for Sat Temperature (Density) Error (Recurring errors)
		int SatSupPressureEnthErrCount; // Count for Sat Temperature (Density) Error (Recurring errors)
		int SatSupDensityErrIndex; // Index for Sat Temperature (Density) Error (Recurring errors)
		int SatSupDensityErrCount; // Count for Sat Temperature (Density) Error (Recurring errors)
		int SatSupDensityTempErrIndex; // Index for Sat Temperature (Density) Error (Recurring errors)
		int SatSupDensityTempErrCount; // Count for Sat Temperature (Density) Error (Recurring errors)
		int SatSupDensityPresErrIndex; // Index for Sat Temperature (Density) Error (Recurring errors)
		int SatSupDensityPresErrCount; // Count for Sat Temperature (Density) Error (Recurring errors)

		// Default Constructor
		FluidPropsRefrigErrors() :
			SatTempErrIndex( 0 ),
			SatTempErrCount( 0 ),
			SatPressErrIndex( 0 ),
			SatPressErrCount( 0 ),
			SatTempDensityErrIndex( 0 ),
			SatTempDensityErrCount( 0 ),
			SatSupEnthalpyErrIndex( 0 ),
			SatSupEnthalpyErrCount( 0 ),
			SatSupEnthalpyTempErrIndex( 0 ),
			SatSupEnthalpyTempErrCount( 0 ),
			SatSupEnthalpyPresErrIndex( 0 ),
			SatSupEnthalpyPresErrCount( 0 ),
			SatSupPressureErrIndex( 0 ),
			SatSupPressureErrCount( 0 ),
			SatSupPressureTempErrIndex( 0 ),
			SatSupPressureTempErrCount( 0 ),
			SatSupPressureEnthErrIndex( 0 ),
			SatSupPressureEnthErrCount( 0 ),
			SatSupDensityErrIndex( 0 ),
			SatSupDensityErrCount( 0 ),
			SatSupDensityTempErrIndex( 0 ),
			SatSupDensityTempErrCount( 0 ),
			SatSupDensityPresErrIndex( 0 ),
			SatSupDensityPresErrCount( 0 )
		{}

		// Member Constructor
		FluidPropsRefrigErrors(
			std::string const & Name,
			int const SatTempErrIndex, // Index for Sat Temperature Error (Recurring errors)
			int const SatTempErrCount, // Count for Sat Temperature Error (Recurring errors)
			int const SatPressErrIndex, // Index for Sat Pressure Error (Recurring errors)
			int const SatPressErrCount, // Count for Sat Pressure Error (Recurring errors)
			int const SatTempDensityErrIndex, // Index for Sat Temperature (Density) Error (Recurring errors)
			int const SatTempDensityErrCount, // Count for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupEnthalpyErrIndex, // Index for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupEnthalpyErrCount, // Count for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupEnthalpyTempErrIndex, // Index for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupEnthalpyTempErrCount, // Count for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupEnthalpyPresErrIndex, // Index for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupEnthalpyPresErrCount, // Count for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupPressureErrIndex, // Index for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupPressureErrCount, // Count for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupPressureTempErrIndex, // Index for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupPressureTempErrCount, // Count for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupPressureEnthErrIndex, // Index for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupPressureEnthErrCount, // Count for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupDensityErrIndex, // Index for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupDensityErrCount, // Count for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupDensityTempErrIndex, // Index for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupDensityTempErrCount, // Count for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupDensityPresErrIndex, // Index for Sat Temperature (Density) Error (Recurring errors)
			int const SatSupDensityPresErrCount // Count for Sat Temperature (Density) Error (Recurring errors)
		) :
			Name( Name ),
			SatTempErrIndex( SatTempErrIndex ),
			SatTempErrCount( SatTempErrCount ),
			SatPressErrIndex( SatPressErrIndex ),
			SatPressErrCount( SatPressErrCount ),
			SatTempDensityErrIndex( SatTempDensityErrIndex ),
			SatTempDensityErrCount( SatTempDensityErrCount ),
			SatSupEnthalpyErrIndex( SatSupEnthalpyErrIndex ),
			SatSupEnthalpyErrCount( SatSupEnthalpyErrCount ),
			SatSupEnthalpyTempErrIndex( SatSupEnthalpyTempErrIndex ),
			SatSupEnthalpyTempErrCount( SatSupEnthalpyTempErrCount ),
			SatSupEnthalpyPresErrIndex( SatSupEnthalpyPresErrIndex ),
			SatSupEnthalpyPresErrCount( SatSupEnthalpyPresErrCount ),
			SatSupPressureErrIndex( SatSupPressureErrIndex ),
			SatSupPressureErrCount( SatSupPressureErrCount ),
			SatSupPressureTempErrIndex( SatSupPressureTempErrIndex ),
			SatSupPressureTempErrCount( SatSupPressureTempErrCount ),
			SatSupPressureEnthErrIndex( SatSupPressureEnthErrIndex ),
			SatSupPressureEnthErrCount( SatSupPressureEnthErrCount ),
			SatSupDensityErrIndex( SatSupDensityErrIndex ),
			SatSupDensityErrCount( SatSupDensityErrCount ),
			SatSupDensityTempErrIndex( SatSupDensityTempErrIndex ),
			SatSupDensityTempErrCount( SatSupDensityTempErrCount ),
			SatSupDensityPresErrIndex( SatSupDensityPresErrIndex ),
			SatSupDensityPresErrCount( SatSupDensityPresErrCount )
		{}

	};

	struct FluidPropsGlycolErrors
	{
		// Members
		std::string Name; // Which glycol this error structure is for
		int SpecHeatLowErrIndex; // Index for Specific Heat Low Error (Recurring errors)
		int SpecHeatHighErrIndex; // Index for Specific Heat High Error (Recurring errors)
		int SpecHeatLowErrCount; // Count for Specific Heat Low Error (Recurring errors)
		int SpecHeatHighErrCount; // Count for Specific Heat High Error (Recurring errors)
		int DensityHighErrCount; // Index for Density Low Error (Recurring errors)
		int DensityLowErrIndex; // Index for Density High Error (Recurring errors)
		int DensityHighErrIndex; // Count for Density Low Error (Recurring errors)
		int DensityLowErrCount; // Count for Density High Error (Recurring errors)
		int ConductivityLowErrIndex; // Index for Conductivity Low Error (Recurring errors)
		int ConductivityHighErrIndex; // Index for Conductivity High Error (Recurring errors)
		int ConductivityLowErrCount; // Count for Conductivity Low Error (Recurring errors)
		int ConductivityHighErrCount; // Count for Conductivity High Error (Recurring errors)
		int ViscosityLowErrIndex; // Index for Viscosity Low Error (Recurring errors)
		int ViscosityHighErrIndex; // Index for Viscosity High Error (Recurring errors)
		int ViscosityLowErrCount; // Count for Viscosity Low Error (Recurring errors)
		int ViscosityHighErrCount; // Count for Viscosity High Error (Recurring errors)

		// Default Constructor
		FluidPropsGlycolErrors() :
			SpecHeatLowErrIndex( 0 ),
			SpecHeatHighErrIndex( 0 ),
			SpecHeatLowErrCount( 0 ),
			SpecHeatHighErrCount( 0 ),
			DensityHighErrCount( 0 ),
			DensityLowErrIndex( 0 ),
			DensityHighErrIndex( 0 ),
			DensityLowErrCount( 0 ),
			ConductivityLowErrIndex( 0 ),
			ConductivityHighErrIndex( 0 ),
			ConductivityLowErrCount( 0 ),
			ConductivityHighErrCount( 0 ),
			ViscosityLowErrIndex( 0 ),
			ViscosityHighErrIndex( 0 ),
			ViscosityLowErrCount( 0 ),
			ViscosityHighErrCount( 0 )
		{}

		// Member Constructor
		FluidPropsGlycolErrors(
			std::string const & Name, // Which glycol this error structure is for
			int const SpecHeatLowErrIndex, // Index for Specific Heat Low Error (Recurring errors)
			int const SpecHeatHighErrIndex, // Index for Specific Heat High Error (Recurring errors)
			int const SpecHeatLowErrCount, // Count for Specific Heat Low Error (Recurring errors)
			int const SpecHeatHighErrCount, // Count for Specific Heat High Error (Recurring errors)
			int const DensityHighErrCount, // Index for Density Low Error (Recurring errors)
			int const DensityLowErrIndex, // Index for Density High Error (Recurring errors)
			int const DensityHighErrIndex, // Count for Density Low Error (Recurring errors)
			int const DensityLowErrCount, // Count for Density High Error (Recurring errors)
			int const ConductivityLowErrIndex, // Index for Conductivity Low Error (Recurring errors)
			int const ConductivityHighErrIndex, // Index for Conductivity High Error (Recurring errors)
			int const ConductivityLowErrCount, // Count for Conductivity Low Error (Recurring errors)
			int const ConductivityHighErrCount, // Count for Conductivity High Error (Recurring errors)
			int const ViscosityLowErrIndex, // Index for Viscosity Low Error (Recurring errors)
			int const ViscosityHighErrIndex, // Index for Viscosity High Error (Recurring errors)
			int const ViscosityLowErrCount, // Count for Viscosity Low Error (Recurring errors)
			int const ViscosityHighErrCount // Count for Viscosity High Error (Recurring errors)
		) :
			Name( Name ),
			SpecHeatLowErrIndex( SpecHeatLowErrIndex ),
			SpecHeatHighErrIndex( SpecHeatHighErrIndex ),
			SpecHeatLowErrCount( SpecHeatLowErrCount ),
			SpecHeatHighErrCount( SpecHeatHighErrCount ),
			DensityHighErrCount( DensityHighErrCount ),
			DensityLowErrIndex( DensityLowErrIndex ),
			DensityHighErrIndex( DensityHighErrIndex ),
			DensityLowErrCount( DensityLowErrCount ),
			ConductivityLowErrIndex( ConductivityLowErrIndex ),
			ConductivityHighErrIndex( ConductivityHighErrIndex ),
			ConductivityLowErrCount( ConductivityLowErrCount ),
			ConductivityHighErrCount( ConductivityHighErrCount ),
			ViscosityLowErrIndex( ViscosityLowErrIndex ),
			ViscosityHighErrIndex( ViscosityHighErrIndex ),
			ViscosityLowErrCount( ViscosityLowErrCount ),
			ViscosityHighErrCount( ViscosityHighErrCount )
		{}

	};

	// Object Data
	extern FArray1D< FluidPropsRefrigerantData > RefrigData;
	extern FArray1D< FluidPropsRefrigErrors > RefrigErrorTracking;
	extern FArray1D< FluidPropsGlycolRawData > GlyRawData;
	extern FArray1D< FluidPropsGlycolData > GlycolData;
	extern FArray1D< FluidPropsGlycolErrors > GlycolErrorTracking;

	// Functions

	void
	GetFluidPropertiesData();

	//*****************************************************************************

	void
	InterpDefValuesForGlycolConc(
		int const NumOfConcs, // number of concentrations (dimension of raw data)
		int const NumOfTemps, // number of temperatures (dimension of raw data)
		FArray1S< Real64 > const RawConcData, // concentrations for raw data
		FArray2S< Real64 > const RawPropData, // raw property data (concentration, temperature)
		Real64 const Concentration, // concentration of actual fluid mix
		FArray1S< Real64 > InterpData // interpolated output data at proper concentration
	);

	//*****************************************************************************

	void
	InterpValuesForGlycolConc(
		int const NumOfConcs, // number of concentrations (dimension of raw data)
		int const NumOfTemps, // number of temperatures (dimension of raw data)
		FArray1S< Real64 > const RawConcData, // concentrations for raw data
		FArray2S< Real64 > const RawPropData, // raw property data (temperature,concentration)
		Real64 const Concentration, // concentration of actual fluid mix
		FArray1S< Real64 > InterpData // interpolated output data at proper concentration
	);

	//*****************************************************************************

	void
	InitializeGlycolTempLimits( bool & ErrorsFound ); // set to true if errors found here

	//*****************************************************************************

	void
	InitializeRefrigerantLimits( bool & ErrorsFound ); // set to true if errors found here

	//*****************************************************************************

	void
	ReportAndTestGlycols();

	//*****************************************************************************

	void
	ReportAndTestRefrigerants();

	//*****************************************************************************

	Real64
	GetSatPressureRefrig(
		std::string const & Refrigerant, // carries in substance name
		Real64 const Temperature, // actual temperature given as input
		int & RefrigIndex, // Index to Refrigerant Properties
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

	//*****************************************************************************

	Real64
	GetSatTemperatureRefrig(
		std::string const & Refrigerant, // carries in substance name
		Real64 const Pressure, // actual temperature given as input
		int & RefrigIndex, // Index to Refrigerant Properties
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

	//*****************************************************************************

	Real64
	GetSatEnthalpyRefrig(
		std::string const & Refrigerant, // carries in substance name
		Real64 const Temperature, // actual temperature given as input
		Real64 const Quality, // actual quality given as input
		int & RefrigIndex, // Index to Refrigerant Properties
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

	//*****************************************************************************

	Real64
	GetSatDensityRefrig(
		std::string const & Refrigerant, // carries in substance name
		Real64 const Temperature, // actual temperature given as input
		Real64 const Quality, // actual quality given as input
		int & RefrigIndex, // Index to Refrigerant Properties
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

	//*****************************************************************************

	Real64
	GetSatSpecificHeatRefrig(
		std::string const & Refrigerant, // carries in substance name
		Real64 const Temperature, // actual temperature given as input
		Real64 const Quality, // actual quality given as input
		int & RefrigIndex, // Index to Refrigerant Properties
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

	//*****************************************************************************

	Real64
	GetSupHeatEnthalpyRefrig(
		std::string const & Refrigerant, // carries in substance name
		Real64 const Temperature, // actual temperature given as input
		Real64 const Pressure, // actual pressure given as input
		int & RefrigIndex, // Index to Refrigerant Properties
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

	//*****************************************************************************

	Real64
	GetSupHeatPressureRefrig(
		std::string const & Refrigerant, // carries in substance name
		Real64 const Temperature, // actual temperature given as input
		Real64 const Enthalpy, // actual enthalpy given as input
		int & RefrigIndex, // Index to Refrigerant Properties
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

	//*****************************************************************************

	Real64
	GetSupHeatDensityRefrig(
		std::string const & Refrigerant, // carries in substance name
		Real64 const Temperature, // actual temperature given as input
		Real64 const Pressure, // actual pressure given as input
		int & RefrigIndex, // Index to Refrigerant Properties
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

	//*****************************************************************************

	Real64
	GetSpecificHeatGlycol(
		std::string const & Glycol, // carries in substance name
		Real64 const Temperature, // actual temperature given as input
		int & GlycolIndex, // Index to Glycol Properties
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

	//*****************************************************************************

	Real64
	GetDensityGlycol(
		std::string const & Glycol, // carries in substance name
		Real64 const Temperature, // actual temperature given as input
		int & GlycolIndex, // Index to Glycol Properties
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

	//*****************************************************************************

	Real64
	GetConductivityGlycol(
		std::string const & Glycol, // carries in substance name
		Real64 const Temperature, // actual temperature given as input
		int & GlycolIndex, // Index to Glycol Properties
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

	//*****************************************************************************

	Real64
	GetViscosityGlycol(
		std::string const & Glycol, // carries in substance name
		Real64 const Temperature, // actual temperature given as input
		int & GlycolIndex, // Index to Glycol Properties
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

	//*****************************************************************************

	void
	GetInterpValue_error();

	inline
	Real64
	GetInterpValue(
		Real64 const Tact, // actual temperature at which we want the property of interest
		Real64 const Tlo, // temperature below Tact for which we have property data
		Real64 const Thi, // temperature above Tact for which we have property data
		Real64 const Xlo, // value of property at Tlo
		Real64 const Xhi // value of property at Thi
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   June 2004
		//       MODIFIED       N/A
		//       RE-ENGINEERED  N/A

		// PURPOSE OF THIS FUNCTION:
		// This subroutine does a simple linear interpolation.

		// METHODOLOGY EMPLOYED:
		// No mysteries here...just plain-old linear interpolation.

		// REFERENCES:
		// Any basic engineering mathematic text.

		// USE STATEMENTS:
		// na

		// Return value
		// na

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Real64 const TempToler( 0.001 ); // Some reasonable value for comparisons

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:
		if ( std::abs( Thi - Tlo ) > TempToler ) {
			return Xhi - ( ( ( Thi - Tact ) / ( Thi - Tlo ) ) * ( Xhi - Xlo ) );
		} else {
			GetInterpValue_error();
			return 0.0;
		}
	}

	inline
	Real64
	GetInterpValue_fast(
		Real64 const Tact, // actual temperature at which we want the property of interest
		Real64 const Tlo, // temperature below Tact for which we have property data
		Real64 const Thi, // temperature above Tact for which we have property data
		Real64 const Xlo, // value of property at Tlo
		Real64 const Xhi // value of property at Thi
	)
	{
		return Xhi - ( ( ( Thi - Tact ) / ( Thi - Tlo ) ) * ( Xhi - Xlo ) );
	}

	//*****************************************************************************

	Real64
	GetQualityRefrig(
		std::string const & Refrigerant, // carries in substance name
		Real64 const Temperature, // actual temperature given as input
		Real64 const Enthalpy, // actual enthalpy given as input
		int & RefrigIndex, // Index to Refrigerant Properties
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

	//*****************************************************************************

	int
	FindRefrigerant( std::string const & Refrigerant ); // carries in substance name

	//*****************************************************************************

	int
	FindGlycol( std::string const & Glycol ); // carries in substance name

	//*****************************************************************************

	std::string
	GetGlycolNameByIndex( int const Idx ); // carries in substance index

	//*****************************************************************************

	int
	FindArrayIndex(
		Real64 const Value, // Value to be placed/found within the array of values
		FArray1D< Real64 > const & Array, // Array of values in ascending order
		int const LowBound, // Valid values lower bound (set by calling program)
		int const UpperBound // Valid values upper bound (set by calling program)
	);

	int
	FindArrayIndex(
		Real64 const Value, // Value to be placed/found within the array of values
		FArray1D< Real64 > const & Array // Array of values in ascending order
	);

	//*****************************************************************************

	Real64
	GetInterpolatedSatProp(
		Real64 const Temperature, // Saturation Temp.
		FArray1D< Real64 > const & PropTemps, // Array of temperature at which props are available
		FArray1D< Real64 > const & LiqProp, // Array of saturated liquid properties
		FArray1D< Real64 > const & VapProp, // Array of saturatedvapour properties
		Real64 const Quality, // Quality
		std::string const & CalledFrom, // routine this function was called from (error messages)
		int const LowBound, // Valid values lower bound (set by calling program)
		int const UpperBound // Valid values upper bound (set by calling program)
	);

	//*****************************************************************************

	int
	CheckFluidPropertyName( std::string const & NameToCheck ); // Name from input(?) to be checked against valid FluidPropertyNames

	void
	ReportOrphanFluids();

	void
	ReportFatalGlycolErrors(
		int const NumGlycols, // Number of Glycols in input/data
		int const GlycolNum, // Glycol Index
		bool const DataPresent, // data is present for this fluid.
		std::string const & GlycolName, // Name being reported
		std::string const & RoutineName, // Routine name to show
		std::string const & Property, // Property being requested
		std::string const & CalledFrom // original called from (external to fluid properties)
	);

	void
	ReportFatalRefrigerantErrors(
		int const NumRefrigerants, // Number of Refrigerants in input/data
		int const RefrigerantNum, // Refrigerant Index
		bool const DataPresent, // data is present for this fluid.
		std::string const & RefrigerantName, // Name being reported
		std::string const & RoutineName, // Routine name to show
		std::string const & Property, // Property being requested
		std::string const & CalledFrom // original called from (external to fluid properties)
	);

	void
	GetFluidDensityTemperatureLimits(
		int const FluidIndex,
		Real64 & MinTempLimit,
		Real64 & MaxTempLimit
	);

	void
	GetFluidSpecificHeatTemperatureLimits(
		int const FluidIndex,
		Real64 & MinTempLimit,
		Real64 & MaxTempLimit
	);

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
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

} // FluidProperties

} // EnergyPlus

#endif
