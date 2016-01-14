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

#ifndef FluidProperties_hh_INCLUDED
#define FluidProperties_hh_INCLUDED

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array2S.hh>

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
	extern Array1D_bool RefrigUsed;
	extern Array1D_bool GlycolUsed;
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
		Array1D< Real64 > PsTemps; // Temperatures for saturation pressures
		Array1D< Real64 > PsValues; // Saturation pressures at PsTemps
		int NumHPoints; // Number of enthalpy points
		Real64 HfLowTempValue; // Low Temperature Value for Hf (>0.0)
		Real64 HfHighTempValue; // High Temperature Value for Hf (max in tables)
		int HfLowTempIndex; // Low Temperature Min Index for Hf (>0.0)
		int HfHighTempIndex; // High Temperature Max Index for Hf (>0.0)
		Real64 HfgLowTempValue; // Low Temperature Value for Hfg (>0.0)
		Real64 HfgHighTempValue; // High Temperature Value for Hfg (max in tables)
		int HfgLowTempIndex; // Low Temperature Min Index for Hfg (>0.0)
		int HfgHighTempIndex; // High Temperature Max Index for Hfg (>0.0)
		Array1D< Real64 > HTemps; // Temperatures for enthalpy points
		Array1D< Real64 > HfValues; // Enthalpy of saturated fluid at HTemps
		Array1D< Real64 > HfgValues; // Enthalpy of saturated fluid/gas at HTemps
		int NumCpPoints; // Number of specific heat of fluid points
		Real64 CpfLowTempValue; // Low Temperature Value for Cpf (>0.0)
		Real64 CpfHighTempValue; // High Temperature Value for Cpf (max in tables)
		int CpfLowTempIndex; // Low Temperature Min Index for Cpf (>0.0)
		int CpfHighTempIndex; // High Temperature Max Index for Cpf (>0.0)
		Real64 CpfgLowTempValue; // Low Temperature Value for Cpfg (>0.0)
		Real64 CpfgHighTempValue; // High Temperature Value for Cpfg (max in tables)
		int CpfgLowTempIndex; // Low Temperature Min Index for Cpfg (>0.0)
		int CpfgHighTempIndex; // High Temperature Max Index for Cpfg (>0.0)
		Array1D< Real64 > CpTemps; // Temperatures for specific heat points
		Array1D< Real64 > CpfValues; // Specific heat of saturated fluid at CpTemps
		Array1D< Real64 > CpfgValues; // Specific heat of saturated fluid/gas at CpTemps
		int NumRhoPoints; // Number of density of fluid points
		Real64 RhofLowTempValue; // Low Temperature Value for Rhof (>0.0)
		Real64 RhofHighTempValue; // High Temperature Value for Rhof (max in tables)
		int RhofLowTempIndex; // Low Temperature Min Index for Rhof (>0.0)
		int RhofHighTempIndex; // High Temperature Max Index for Rhof (>0.0)
		Real64 RhofgLowTempValue; // Low Temperature Value for Rhofg (>0.0)
		Real64 RhofgHighTempValue; // High Temperature Value for Rhofg (max in tables)
		int RhofgLowTempIndex; // Low Temperature Min Index for Rhofg (>0.0)
		int RhofgHighTempIndex; // High Temperature Max Index for Rhofg (>0.0)
		Array1D< Real64 > RhoTemps; // Temperatures for density of fluid points
		Array1D< Real64 > RhofValues; // Density of saturated fluid at RhoTemps
		Array1D< Real64 > RhofgValues; // Density of saturated fluid/gas at RhoTemps
		int NumSuperTempPts; // Number of temperature points for superheated enthalpy
		int NumSuperPressPts; // Number of pressure points for superheated enthalpy
		Array1D< Real64 > SHTemps; // Temperatures for superheated gas
		Array1D< Real64 > SHPress; // Pressures for superheated gas
		Array2D< Real64 > HshValues; // Enthalpy of superheated gas at HshTemps, HshPress
		Array2D< Real64 > RhoshValues; // Density of superheated gas at HshTemps, HshPress

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

	};

	struct FluidPropsGlycolRawData
	{
		// Members
		std::string Name; // Name of the glycol
		bool CpDataPresent; // Flag set when specific heat data is available
		int NumCpTempPts; // Number of temperature points for specific heat
		int NumCpConcPts; // Number of concentration points for specific heat
		Array1D< Real64 > CpTemps; // Temperatures for specific heat of glycol
		Array1D< Real64 > CpConcs; // Concentration for specific heat of glycol
		Array2D< Real64 > CpValues; // Specific heat data values
		bool RhoDataPresent; // Flag set when density data is available
		int NumRhoTempPts; // Number of temperature points for density
		int NumRhoConcPts; // Number of concentration points for density
		Array1D< Real64 > RhoTemps; // Temperatures for density of glycol
		Array1D< Real64 > RhoConcs; // Concentration for density of glycol
		Array2D< Real64 > RhoValues; // Density data values
		bool CondDataPresent; // Flag set when conductivity data is available
		int NumCondTempPts; // Number of temperature points for conductivity
		int NumCondConcPts; // Number of concentration points for conductivity
		Array1D< Real64 > CondTemps; // Temperatures for conductivity of glycol
		Array1D< Real64 > CondConcs; // Concentration for conductivity of glycol
		Array2D< Real64 > CondValues; // conductivity values
		bool ViscDataPresent; // Flag set when viscosity data is available
		int NumViscTempPts; // Number of temperature points for viscosity
		int NumViscConcPts; // Number of concentration points for viscosity
		Array1D< Real64 > ViscTemps; // Temperatures for viscosity of glycol
		Array1D< Real64 > ViscConcs; // Concentration for viscosity of glycol
		Array2D< Real64 > ViscValues; // viscosity values

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
		Array1D< Real64 > CpTemps; // Temperatures for specific heat of glycol
		Array1D< Real64 > CpValues; // Specific heat data values (J/kg-K)
		bool RhoDataPresent; // Flag set when density data is available
		int NumRhoTempPts; // Number of temperature points for density
		Real64 RhoLowTempValue; // Low Temperature Value for Rho (>0.0)
		Real64 RhoHighTempValue; // High Temperature Value for Rho (max in tables)
		int RhoLowTempIndex; // Low Temperature Min Index for Rho (>0.0)
		int RhoHighTempIndex; // High Temperature Max Index for Rho (>0.0)
		Array1D< Real64 > RhoTemps; // Temperatures for density of glycol
		Array1D< Real64 > RhoValues; // Density data values (kg/m3)
		bool CondDataPresent; // Flag set when conductivity data is available
		int NumCondTempPts; // Number of temperature points for conductivity
		Real64 CondLowTempValue; // Low Temperature Value for Cond (>0.0)
		Real64 CondHighTempValue; // High Temperature Value for Cond (max in tables)
		int CondLowTempIndex; // Low Temperature Min Index for Cond (>0.0)
		int CondHighTempIndex; // High Temperature Max Index for Cond (>0.0)
		Array1D< Real64 > CondTemps; // Temperatures for conductivity of glycol
		Array1D< Real64 > CondValues; // conductivity values (W/m-K)
		bool ViscDataPresent; // Flag set when viscosity data is available
		int NumViscTempPts; // Number of temperature points for viscosity
		Real64 ViscLowTempValue; // Low Temperature Value for Visc (>0.0)
		Real64 ViscHighTempValue; // High Temperature Value for Visc (max in tables)
		int ViscLowTempIndex; // Low Temperature Min Index for Visc (>0.0)
		int ViscHighTempIndex; // High Temperature Max Index for Visc (>0.0)
		Array1D< Real64 > ViscTemps; // Temperatures for viscosity of glycol
		Array1D< Real64 > ViscValues; // viscosity values (mPa-s)

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

	};

	// Object Data
	extern Array1D< FluidPropsRefrigerantData > RefrigData;
	extern Array1D< FluidPropsRefrigErrors > RefrigErrorTracking;
	extern Array1D< FluidPropsGlycolRawData > GlyRawData;
	extern Array1D< FluidPropsGlycolData > GlycolData;
	extern Array1D< FluidPropsGlycolErrors > GlycolErrorTracking;

	// Functions

	void
	clear_state();

	void
	GetFluidPropertiesData();

	//*****************************************************************************

	void
	InterpDefValuesForGlycolConc(
		int const NumOfConcs, // number of concentrations (dimension of raw data)
		int const NumOfTemps, // number of temperatures (dimension of raw data)
		Array1S< Real64 > const RawConcData, // concentrations for raw data
		Array2S< Real64 > const RawPropData, // raw property data (concentration, temperature)
		Real64 const Concentration, // concentration of actual fluid mix
		Array1S< Real64 > InterpData // interpolated output data at proper concentration
	);

	//*****************************************************************************

	void
	InterpValuesForGlycolConc(
		int const NumOfConcs, // number of concentrations (dimension of raw data)
		int const NumOfTemps, // number of temperatures (dimension of raw data)
		Array1S< Real64 > const RawConcData, // concentrations for raw data
		Array2S< Real64 > const RawPropData, // raw property data (temperature,concentration)
		Real64 const Concentration, // concentration of actual fluid mix
		Array1S< Real64 > InterpData // interpolated output data at proper concentration
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
		Array1D< Real64 > const & Array, // Array of values in ascending order
		int const LowBound, // Valid values lower bound (set by calling program)
		int const UpperBound // Valid values upper bound (set by calling program)
	);

	int
	FindArrayIndex(
		Real64 const Value, // Value to be placed/found within the array of values
		Array1D< Real64 > const & Array // Array of values in ascending order
	);

	//*****************************************************************************

	Real64
	GetInterpolatedSatProp(
		Real64 const Temperature, // Saturation Temp.
		Array1D< Real64 > const & PropTemps, // Array of temperature at which props are available
		Array1D< Real64 > const & LiqProp, // Array of saturated liquid properties
		Array1D< Real64 > const & VapProp, // Array of saturatedvapour properties
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

} // FluidProperties

} // EnergyPlus

#endif
