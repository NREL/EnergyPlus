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

#ifndef WindowManager_hh_INCLUDED
#define WindowManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2A.hh>
#include <ObjexxFCL/Array3D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace WindowManager {

	// Data
	//MODULE PARAMETER DEFINITIONS:

	extern Real64 const sigma; // Stefan-Boltzmann constant
	extern Real64 const TKelvin; // conversion from Kelvin to Celsius
	extern int const nume; // Number of wavelength values in solar spectrum
	extern int const numt3; // Number of wavelength values in the photopic response

	//               Dens  dDens/dT  Con    dCon/dT   Vis    dVis/dT Prandtl dPrandtl/dT
	extern Array1D< Real64 > const AirProps;
	// Air mass 1.5 terrestrial solar global spectral irradiance (W/m2-micron)
	// on a 37 degree tilted surface; corresponds
	// to wavelengths (microns) in following data block (ISO 9845-1 and ASTM E 892;
	// derived from Optics5 data file ISO-9845GlobalNorm.std, 10-14-99)
	extern Array1D< Real64 > wle; // Solar spectrum wavelength values (microns)

	extern Array1D< Real64 > e; // Solar spectrum values corresponding to wle

	// Phototopic response function and corresponding wavelengths (microns)
	// (CIE 1931 observer; ISO/CIE 10527, CIE Standard Calorimetric Observers;
	// derived from Optics5 data file "CIE 1931 Color Match from E308.txt", which is
	// the same as WINDOW4 file Cie31t.dat)
	extern Array1D< Real64 > wlt3; // Wavelength values for photopic response

	extern Array1D< Real64 > y30; // Photopic response corresponding to wavelengths in wlt3

	// MODULE VARIABLE DECLARATIONS:

	extern int ngllayer; // Number of glass layers
	extern int nglface; // Number of glass faces
	extern int nglfacep; // Number of glass faces, + 2 if shade layer present
	extern Real64 tout; // Outside air temperature (K)
	extern Real64 tin; // Inside air temperature (previous timestep) (K)
	extern Real64 tilt; // Window tilt (deg)
	extern Real64 tiltr; // Window tilt (radians)
	extern Real64 hcin; // Convective inside air film conductance (W/m2-K)
	extern Real64 hcout; // Convective outside air film conductance (W/m2-K)
	extern Real64 Ebout; // Sigma*(outside air temp)**4 (W/m2)
	extern Real64 Outir; // IR radiance of window's exterior surround (W/m2)
	extern Real64 Rmir; // IR radiance of window's interior surround (W/m2)
	extern Real64 Rtot; // Total thermal resistance of window (m2-K/W)
	extern Array3D< Real64 > gcon; // Gas thermal conductivity coefficients for each gap
	extern Array3D< Real64 > gvis; // Gas viscosity coefficients for each gap
	extern Array3D< Real64 > gcp; // Gas specific-heat coefficients for each gap
	extern Array2D< Real64 > gwght; // Gas molecular weights for each gap
	extern Array2D< Real64 > gfract; // Gas fractions for each gap
	extern Array1D_int gnmix; // Number of gases in gap
	extern Array1D< Real64 > gap; // Gap width (m)
	extern Array1D< Real64 > thick; // Glass layer thickness (m)
	extern Array1D< Real64 > scon; // Glass layer conductance--conductivity/thickness (W/m2-K)
	extern Array1D< Real64 > tir; // Front and back IR transmittance for each glass layer
	extern Array1D< Real64 > emis; // Front and back IR emissivity for each glass layer
	extern Array1D< Real64 > rir; // Front and back IR reflectance for each glass layer
	//  (program calculates from tir and emis)
	extern Array1D< Real64 > AbsRadGlassFace; // Solar radiation and IR radiation from internal
	//  gains absorbed by glass face
	extern Array1D< Real64 > thetas; // Glass surface temperatures (K)
	extern Array1D< Real64 > thetasPrev; // Previous-iteration glass surface temperatures (K)
	extern Array1D< Real64 > fvec; // Glass face heat balance function
	extern Array2D< Real64 > fjac; // Glass face heat balance Jacobian
	extern Array1D< Real64 > dtheta; // Glass layer temperature difference factor [K]
	extern Array2D< Real64 > zir; // IR transfer matrix
	extern Array2D< Real64 > ziri; // Inverse of IR transfer matrix
	extern Array2D< Real64 > ddeldt; // Matrix of derivatives of residuals wrt temperature
	extern Array2D< Real64 > dtddel; // Inverse of matrix of derivatives of
	//   residuals wrt temperature
	extern Array1D< Real64 > qf; // IR heat flux at each face [W/m2]
	extern Array1D< Real64 > hf; // Component of convective flux at each face
	extern Array2D< Real64 > der; // Derivative of IR sources wrt surface temperature
	extern Array2D< Real64 > dhf; // Derivative of heat flux wrt surface temperature
	extern Array1D< Real64 > sour; // IR source term at each face [W/m2]
	extern Array1D< Real64 > delta; // Residual at each glass layer [W/m2]
	extern Array1D< Real64 > hcgap; // Convective gap conductance
	extern Array1D< Real64 > hrgap; // Radiative gap conductance
	extern Array1D< Real64 > rgap; // Convective plus radiative gap resistance
	//   (inverse of hcgap + hrgap)
	extern Array1D< Real64 > rs; // Outside film convective resistance, gap resistances,
	//   inside air film convective resistance
	extern Array1D< Real64 > arhs;
	extern Real64 A23P; // Intermediate variables in glass face
	extern Real64 A32P;
	extern Real64 A45P;
	extern Real64 A54P;
	extern Real64 A67P;
	extern Real64 A76P;
	extern Real64 A23; // heat balance equations
	extern Real64 A45;
	extern Real64 A67;

	extern Array2D< Real64 > wlt; // Spectral data wavelengths for each glass layer in a glazing system
	// Following data, Spectral data for each layer for each wavelength in wlt
	extern Array2D< Real64 > t; // normal transmittance
	extern Array2D< Real64 > rff; // normal front reflectance
	extern Array2D< Real64 > rbb; // normal back reflectance
	extern Array2D< Real64 > tPhi; // transmittance at angle of incidence
	extern Array2D< Real64 > rfPhi; // front reflectance at angle of incidence
	extern Array2D< Real64 > rbPhi; // back reflectance at angle of incidence
	extern Array2D< Real64 > tadjPhi; // transmittance at angle of incidence
	extern Array2D< Real64 > rfadjPhi; // front reflectance at angle of incidence
	extern Array2D< Real64 > rbadjPhi; // back reflectance at angle of incidence

	extern Array1D_int numpt; // Number of spectral data wavelengths for each layer; =2 if no spectra data for a layer
	extern Array1D< Real64 > stPhi; // Glazing system transmittance at angle of incidence for each wavelength in wle
	extern Array1D< Real64 > srfPhi; // Glazing system front reflectance at angle of incidence for each wavelength in wle
	extern Array1D< Real64 > srbPhi; // Glazing system back reflectance at angle of incidence for each wavelenth in wle
	extern Array2D< Real64 > saPhi; // For each layer, glazing system absorptance at angle of incidence
	// for each wavelenth in wle
	extern Array2D< Real64 > top; // Transmittance matrix for subr. op
	extern Array2D< Real64 > rfop; // Front reflectance matrix for subr. op
	extern Array2D< Real64 > rbop; // Back transmittance matrix for subr. op
	extern Array1D< Real64 > IndepVarCurveFit; // Values of independent variable (cos of inc. angle) for curve fit
	extern Array1D< Real64 > DepVarCurveFit; // Values of dependent variable corresponding to IndepVarCurveFit values
	extern Array1D< Real64 > CoeffsCurveFit; // Polynomial coefficients from curve fit
	extern Array1D< Real64 > tsolPhi; // Glazing system solar transmittance for each angle of incidence
	extern Array1D< Real64 > rfsolPhi; // Glazing system solar front reflectance for each angle of incidence
	extern Array1D< Real64 > rbsolPhi; // Glazing system solar back reflectance for each angle of incidence
	extern Array2D< Real64 > solabsPhi; // Glazing system solar absorptance for each angle of incidence
	extern Array2D< Real64 > solabsBackPhi; // Glazing system back solar absorptance for each angle of incidence
	extern Array1D< Real64 > solabsShadePhi; // Glazing system interior shade solar absorptance for each angle of incidence
	extern Array1D< Real64 > tvisPhi; // Glazing system visible transmittance for each angle of incidence
	extern Array1D< Real64 > rfvisPhi; // Glazing system visible front reflectance for each angle of incidence
	extern Array1D< Real64 > rbvisPhi; // Glazing system visible back reflectance for each angle of incidence
	extern Array1D< Real64 > CosPhiIndepVar; // Cos of incidence angles at 10-deg increments for curve fits

	// SUBROUTINE SPECIFICATIONS FOR MODULE WindowManager:
	//   Optical Calculation Routines
	//   Heat Balance Routines

	// Functions

	void
	clear_state();

	void
	InitGlassOpticalCalculations();

	//*****************************************************************************************

	void
	W5InitGlassParameters();

	//****************************************************************************
	// WINDOW 5 Optical Calculation Subroutines
	//****************************************************************************

	void
	SystemSpectralPropertiesAtPhi(
		int const iquasi, // When there is no spectral data, this is the wavelength
		int const ngllayer, // Number of glass layers in construction
		Real64 const wlbot, // Lowest and highest wavelength considered
		Real64 const wltop
	);

	//************************************************************************

	void
	SystemPropertiesAtLambdaAndPhi(
		int const n, // Number of glass layers
		Real64 & tt, // System transmittance
		Real64 & rft, // System front and back reflectance
		Real64 & rbt,
		Array1A< Real64 > aft // System absorptance of each glass layer
	);

	//*************************************************************************

	void
	SolarSprectrumAverage(
		Array1A< Real64 > p, // Quantity to be weighted by solar spectrum
		Real64 & psol // Quantity p weighted by solar spectrum
	);

	//********************************************************************

	void
	VisibleSprectrumAverage(
		Array1A< Real64 > p, // Quantity to be weighted by solar spectrum
		Real64 & pvis // Quantity p weighted by solar spectrum and photopic
	);

	//**********************************************************************

	void
	Interpolate(
		Array1A< Real64 > x, // Array of data points for independent variable
		Array1A< Real64 > y, // Array of data points for dependent variable
		int const npts, // Number of data pairs
		Real64 const xin, // Given value of x
		Real64 & yout // Interpolated value of y at xin
	);

	//***********************************************************************************
	// Window Thermal Calculation Subroutines
	//***********************************************************************************

	void
	CalcWindowHeatBalance(
		int const SurfNum, // Surface number
		Real64 const HextConvCoeff, // Outside air film conductance coefficient
		Real64 & SurfInsideTemp, // Inside window surface temperature
		Real64 & SurfOutsideTemp // Outside surface temperature (C)
	);

	//****************************************************************************

	void
	WindowHeatBalanceEquations( int const SurfNum ); // Surface number

	//****************************************************************************

	void
	SolveForWindowTemperatures( int const SurfNum ); // Surface number

	//****************************************************************************

	void
	ExtOrIntShadeNaturalFlow(
		int const SurfNum, // Surface number
		int const iter, // Iteration number for glass heat balance calculation
		Real64 & VGap, // Air velocity in glass-shade/blind gap (m/s)
		Real64 & TGapNew, // Current-iteration average air temp in glass-shade/blind gap (K)
		Real64 & TGapOutlet, // Temperature of air leaving glass-shade/blind gap at top for upward
		Real64 & hcv, // Convection coefficient from gap glass or shade to gap air (W/m2-K)
		Real64 & QConvGap // Convective heat gain from glass-shade/blind gap for interior shade (W)
	);

	//****************************************************************************

	void
	BetweenGlassShadeNaturalFlow(
		int const SurfNum, // Surface number
		int const iter, // Iteration number for glass heat balance calculation
		Real64 & VGap, // Gas velocity in gaps (m/s)
		Array1A< Real64 > TGapNew, // Current-iteration average gas temp in gaps (K)
		Array1A< Real64 > hcv // Convection coefficient from gap glass or shade to gap gas (W/m2-K)
	);

	//****************************************************************************

	void
	BetweenGlassForcedFlow(
		int const SurfNum, // Surface number
		int const iter, // Iteration number for glass heat balance calculation
		Real64 & VGap, // Air velocity in airflow gap (m/s)
		Real64 & TGapNew, // Current-iteration average air temp in airflow gap (K)
		Real64 & TGapOutlet, // Temperature of air leaving glass-shade/blind gap at top for upward
		Real64 & hcv, // Convection coefficient from gap glass faces to gap air (W/m2-K)
		Real64 & QConvGap // Convective heat gain from air flow gap (W)
	);

	//****************************************************************************

	void
	BetweenGlassShadeForcedFlow(
		int const SurfNum, // Surface number
		int const iter, // Iteration number for glass heat balance calculation
		Real64 & VGap, // Air velocity in each gap (m/s)
		Array1A< Real64 > TGapNew, // Current-iteration average gas temp in gaps (K)
		Real64 & TGapOutletAve, // Average of TGapOutlet(1) and TGapOutlet(2) (K)
		Array1A< Real64 > hcv, // Convection coefficient from gap glass or shade to gap gas (W/m2-K)
		Real64 & QConvTot // Sum of convective heat flow from gaps (W)
	);

	//****************************************************************************

	void
	LUdecomposition(
		Array2< Real64 > & ajac, // As input: matrix to be decomposed;
		int const n, // Dimension of matrix
		Array1_int & indx, // Vector of row permutations
		Real64 & d // +1 if even number of row interchange is even, -1
	);

	//**************************************************************************

	void
	LUsolution(
		Array2< Real64 > const & a, // Matrix and vector in a.x = b;
		int const n, // Dimension of a and b
		Array1_int const & indx, // Vector of row permutations
		Array1< Real64 > & b // Matrix and vector in a.x = b;
	);

	//******************************************************************************

	void
	WindowGasConductance(
		Real64 const tleft, // Temperature of gap surface closest to outside (K)
		Real64 const tright, // Temperature of gap surface closest to zone (K)
		int const IGap, // Gap number
		Real64 & con, // Gap gas conductance (W/m2-K)
		Real64 & pr, // Gap gas Prandtl number
		Real64 & gr // Gap gas Grashof number
	);

	//******************************************************************************

	void
	WindowGasPropertiesAtTemp(
		Real64 const tmean, // Temperature of gas in gap (K)
		int const IGap, // Gap number
		Real64 & dens, // Gap gas density at tmean (kg/m3)
		Real64 & visc // Gap gas dynamic viscosity at tmean (g/m-s)
	);

	//********************************************************************************

	void
	StartingWindowTemps(
		int const SurfNum, // Surface number
		Array1A< Real64 > AbsRadShade // Short-wave radiation absorbed by shade/blind faces
	);

	//****************************************************************************

	void
	NusseltNumber(
		int const SurfNum, // Surface number
		Real64 const tso, // Temperature of gap surface closest to outside (K)
		Real64 const tsi, // Temperature of gap surface closest to zone (K)
		int const IGap, // Gap number
		Real64 const gr, // Gap gas Grashof number
		Real64 const pr, // Gap gas Prandtl number
		Real64 & gnu // Gap gas Nusselt number
	);

	//*******************************************************************************************************

	void
	TransAndReflAtPhi(
		Real64 const cs, // Cosine of incidence angle
		Real64 const tf0, // Transmittance at zero incidence angle
		Real64 const rf0, // Front reflectance at zero incidence angle
		Real64 const rb0, // Back reflectance at zero incidence angle
		Real64 & tfp, // Transmittance at cs
		Real64 & rfp, // Front reflectance at cs
		Real64 & rbp, // Back reflectance at cs
		bool const SimpleGlazingSystem, // .TRUE. if simple block model being used
		Real64 const SimpleGlazingSHGC, // SHGC value to use in alternate model for simple glazing system
		Real64 const SimpleGlazingU // U-factor value to use in alternate model for simple glazing system
	);

	Real64
	InterpolateBetweenTwoValues(
		Real64 const X,
		Real64 const X0,
		Real64 const X1,
		Real64 const F0,
		Real64 const F1
	);

	Real64
	InterpolateBetweenFourValues(
		Real64 const X,
		Real64 const Y,
		Real64 const X1,
		Real64 const X2,
		Real64 const Y1,
		Real64 const Y2,
		Real64 const Fx1y1,
		Real64 const Fx1y2,
		Real64 const Fx2y1,
		Real64 const Fx2y2
	);

	//**************************************************************************

	void
	W5LsqFit(
		Array1S< Real64 > const IndepVar, // Independent variables
		Array1S< Real64 > const DepVar, // Dependent variables
		int const N, // Order of polynomial
		int const N1, // First and last data points used
		int const N2,
		Array1S< Real64 > CoeffsCurve // Polynomial coeffients from fit
	);

	//********************************************************************************

	void
	W5LsqFit2(
		Array1A< Real64 > const IndepVar, // Independent variables
		Array1A< Real64 > const DepVar, // Dependent variables
		int const N, // Order of polynomial
		int const N1, // First and last data points used
		int const N2,
		Array1A< Real64 > CoeffsCurve // Polynomial coeffients from fit
	);

	//***********************************************************************

	Real64
	DiffuseAverage( Array1S< Real64 > const PropertyValue ); // Property value at angles of incidence

	//*************************************************************************************

	Real64
	DiffuseAverageProfAngGnd( Array1S< Real64 > const Property ); // Property value vs. profile angle

	//*************************************************************************************

	Real64
	DiffuseAverageProfAngSky( Array1S< Real64 > const Property ); // Property value vs. profile angle

	//*************************************************************************************

	void
	CalcWinFrameAndDividerTemps(
		int const SurfNum, // Surface number
		Real64 const tout, // Outside air temperature (K)
		Real64 const tin, // Inside air temperature (K)
		Real64 const HOutConv, // Outside convective air film conductance (W/m2-K)
		Real64 const HInConv, // Inside convective air film conductance (W/m2-K)
		Real64 const Outir, // Exterior IR irradiance from sky and ground
		int const ConstrNum // Construction number of window
	);

	//************************************************************************************

	void
	CalcNominalWindowCond(
		int const ConstrNum, // Construction number
		int const WinterSummerFlag, // 1=winter, 2=summer
		Real64 & NominalConductance, // Nominal center-of-glass conductance, including air films
		Real64 & SHGC, // Nominal center-of-glass solar heat gain coefficient for
		Real64 & TSolNorm, // Overall beam solar transmittance at normal incidence
		Real64 & TVisNorm, // Overall beam visible transmittance at normal incidence
		int & errFlag // Error flag
	);

	//****************************************************************************

	void
	WindowTempsForNominalCond(
		int const ConstrNum, // Construction number
		Array1A< Real64 > hgap // Gap gas conductive conductance (W/m2-K)
	);

	//****************************************************************************

	void
	StartingWinTempsForNominalCond();

	//****************************************************************************

	void
	ReportGlass();

	//*************************************************************************************

	void
	CalcWindowBlindProperties();

	//*************************************************************************************

	void
	CalcWindowScreenProperties();

	void
	BlindOpticsDiffuse(
		int const BlindNum, // Blind number
		int const ISolVis, // 1 = solar and IR calculation; 2 = visible calculation
		Array1A< Real64 > const c, // Slat properties
		Real64 const b_el, // Slat elevation (radians)
		Array1A< Real64 > p // Blind properties
	);

	//**********************************************************************************************

	void
	BlindOpticsBeam(
		int const BlindNum, // Blind number
		Array1A< Real64 > const c, // Slat properties (equivalent to BLD_PR)
		Real64 const b_el, // Slat elevation (radians)
		Real64 const s_el, // Solar profile angle (radians)
		Array1A< Real64 > p // Blind properties (equivalent to ST_LAY)
	);

	//********************************************************************************************

	void
	ViewFac(
		Real64 const s, // Slat width (m)
		Real64 const h, // Distance between faces of adjacent slats (m)
		Real64 const phib, // Elevation angle of normal to slat (radians)
		Real64 const phis, // Profile angle of radiation source (radians)
		Array2A< Real64 > F // View factor array
	);

	//*****************************************************************************************

	void
	InvertMatrix(
		Array2A< Real64 > a, // Matrix to be inverted
		Array2A< Real64 > y, // Inverse of matrix a
		Array1A_int indx, // Index vector for LU decomposition
		int const np, // Dimension of matrix
		int const n
	);

	//*****************************************************************************************

	void
	LUDCMP(
		Array2A< Real64 > A, // matrix
		int const N,
		int const NP,
		Array1A_int INDX,
		int & D
	);

	//*****************************************************************************************

	void
	LUBKSB(
		Array2A< Real64 > A,
		int const N,
		int const NP,
		Array1A_int INDX,
		Array1A< Real64 > B
	);

	// added for custom solar or visible spectrum

	void
	CheckAndReadCustomSprectrumData();

	//*****************************************************************************************

} // WindowManager

} // EnergyPlus

#endif
