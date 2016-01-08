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

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <CommandLineInterface.hh>
#include <WindowManager.hh>
#include <ConvectionCoefficients.hh>
#include <DataBSDFWindow.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataStringGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <DataZoneEquipment.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <Vectors.hh>
#include <WindowComplexManager.hh>
#include <WindowEquivalentLayer.hh>

namespace EnergyPlus {

namespace WindowManager {

	// MODULE INFORMATION
	//       AUTHOR         Fred Winkelmann
	//       DATE WRITTEN   September 1999
	//       MODIFIED       August 2001 (FW): add window shade thermal calculation;
	//                                        add window blind optical and thermal model.
	//                      February 2003 (FW/LKL): Name changed to WindowManager
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Manages the window optical and thermal calculations derived
	// from WINDOW 4 and WINDOW 5.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:
	// WINDOW 4:
	// D.Arasteh, M.Reilly and M.Rubin. A versative procedure for
	// calculating heat transfer through windows. ASHRAE Trans. 1989, Vol. 95, Pt. 2.

	// E.Finlayson, D.Arasteh, C.Huizenga, M.Rubin, and M.Reilly. WINDOW 4.0:
	// Documentation of calculation procedures. LBL-33943. July 1993.

	// WINDOW 5:
	// ASHRAE Standard 142P (draft 1/13/98): Standard method for determining and expressing
	// the heat transfer and total optical properties of fenestration products.

	// Shade and blind thermal model:
	// ISO/DIS 15099, Thermal Performance of Windows, Doors and Shading Devices,
	// Detailed Calculations, 1/12/00.

	// Blind optical model:
	// H. Simmler, U. Fischer and Frederick Winkelmann, Solar-Thermal Window Blind Model
	// for DOE-2, Lawrence Berkeley National Laboratory, Jan. 1996.

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataEnvironment;
	using namespace DataHeatBalance;
	using namespace DataHeatBalFanSys;
	using namespace DataGlobals;
	using namespace DataSurfaces;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	static std::string const BlankString;

	Real64 const sigma( 5.6697e-8 ); // Stefan-Boltzmann constant
	Real64 const TKelvin( KelvinConv ); // conversion from Kelvin to Celsius
	int const nume( 107 ); // Number of wavelength values in solar spectrum
	int const numt3( 81 ); // Number of wavelength values in the photopic response

	//               Dens  dDens/dT  Con    dCon/dT   Vis    dVis/dT Prandtl dPrandtl/dT
	Array1D< Real64 > const AirProps( 8, { 1.29, -0.4e-2, 2.41e-2, 7.6e-5, 1.73e-5, 1.0e-7, 0.72, 1.8e-3 } );
	// Air mass 1.5 terrestrial solar global spectral irradiance (W/m2-micron)
	// on a 37 degree tilted surface; corresponds
	// to wavelengths (microns) in following data block (ISO 9845-1 and ASTM E 892;
	// derived from Optics5 data file ISO-9845GlobalNorm.std, 10-14-99)
	Array1D< Real64 > wle( nume, { 0.3000, 0.3050, 0.3100, 0.3150, 0.3200, 0.3250, 0.3300, 0.3350, 0.3400, 0.3450, 0.3500, 0.3600, 0.3700, 0.3800, 0.3900, 0.4000, 0.4100, 0.4200, 0.4300, 0.4400, 0.4500, 0.4600, 0.4700, 0.4800, 0.4900, 0.5000, 0.5100, 0.5200, 0.5300, 0.5400, 0.5500, 0.5700, 0.5900, 0.6100, 0.6300, 0.6500, 0.6700, 0.6900, 0.7100, 0.7180, 0.7244, 0.7400, 0.7525, 0.7575, 0.7625, 0.7675, 0.7800, 0.8000, 0.8160, 0.8237, 0.8315, 0.8400, 0.8600, 0.8800, 0.9050, 0.9150, 0.9250, 0.9300, 0.9370, 0.9480, 0.9650, 0.9800, 0.9935, 1.0400, 1.0700, 1.1000, 1.1200, 1.1300, 1.1370, 1.1610, 1.1800, 1.2000, 1.2350, 1.2900, 1.3200, 1.3500, 1.3950, 1.4425, 1.4625, 1.4770, 1.4970, 1.5200, 1.5390, 1.5580, 1.5780, 1.5920, 1.6100, 1.6300, 1.6460, 1.6780, 1.7400, 1.8000, 1.8600, 1.9200, 1.9600, 1.9850, 2.0050, 2.0350, 2.0650, 2.1000, 2.1480, 2.1980, 2.2700, 2.3600, 2.4500, 2.4940, 2.5370 } ); // Solar spectrum wavelength values (microns)

	Array1D< Real64 > e( nume, { 0.0, 9.5, 42.3, 107.8, 181.0, 246.0, 395.3, 390.1, 435.3, 438.9, 483.7, 520.3, 666.2, 712.5, 720.7, 1013.1, 1158.2, 1184.0, 1071.9, 1302.0, 1526.0, 1599.6, 1581.0, 1628.3, 1539.2, 1548.7, 1586.5, 1484.9, 1572.4, 1550.7, 1561.5, 1501.5, 1395.5, 1485.3, 1434.1, 1419.9, 1392.3, 1130.0, 1316.7, 1010.3, 1043.2, 1211.2, 1193.9, 1175.5, 643.1, 1030.7, 1131.1, 1081.6, 849.2, 785.0, 916.4, 959.9, 978.9, 933.2, 748.5, 667.5, 690.3, 403.6, 258.3, 313.6, 526.8, 646.4, 746.8, 690.5, 637.5, 412.6, 108.9, 189.1, 132.2, 339.0, 460.0, 423.6, 480.5, 413.1, 250.2, 32.5, 1.6, 55.7, 105.1, 105.5, 182.1, 262.2, 274.2, 275.0, 244.6, 247.4, 228.7, 244.5, 234.8, 220.5, 171.5, 30.7, 2.0, 1.2, 21.2, 91.1, 26.8, 99.5, 60.4, 89.1, 82.2, 71.5, 70.2, 62.0, 21.2, 18.5, 3.2 } ); // Solar spectrum values corresponding to wle

	// Phototopic response function and corresponding wavelengths (microns)
	// (CIE 1931 observer; ISO/CIE 10527, CIE Standard Calorimetric Observers;
	// derived from Optics5 data file "CIE 1931 Color Match from E308.txt", which is
	// the same as WINDOW4 file Cie31t.dat)
	Array1D< Real64 > wlt3( numt3, { 0.380, 0.385, 0.390, 0.395, 0.400, 0.405, 0.410, 0.415, 0.420, 0.425, 0.430, 0.435, 0.440, 0.445, 0.450, 0.455, 0.460, 0.465, 0.470, 0.475, 0.480, 0.485, 0.490, 0.495, 0.500, 0.505, 0.510, 0.515, 0.520, 0.525, 0.530, 0.535, 0.540, 0.545, 0.550, 0.555, 0.560, 0.565, 0.570, 0.575, 0.580, 0.585, 0.590, 0.595, 0.600, 0.605, 0.610, 0.615, 0.620, 0.625, 0.630, 0.635, 0.640, 0.645, 0.650, 0.655, 0.660, 0.665, 0.670, 0.675, 0.680, 0.685, 0.690, 0.695, 0.700, 0.705, 0.710, 0.715, 0.720, 0.725, 0.730, 0.735, 0.740, 0.745, 0.750, 0.755, 0.760, 0.765, 0.770, 0.775, 0.780 } ); // Wavelength values for photopic response

	Array1D< Real64 > y30( numt3, { 0.0000, 0.0001, 0.0001, 0.0002, 0.0004, 0.0006, 0.0012, 0.0022, 0.0040, 0.0073, 0.0116, 0.0168, 0.0230, 0.0298, 0.0380, 0.0480, 0.0600, 0.0739, 0.0910, 0.1126, 0.1390, 0.1693, 0.2080, 0.2586, 0.3230, 0.4073, 0.5030, 0.6082, 0.7100, 0.7932, 0.8620, 0.9149, 0.9540, 0.9803, 0.9950, 1.0000, 0.9950, 0.9786, 0.9520, 0.9154, 0.8700, 0.8163, 0.7570, 0.6949, 0.6310, 0.5668, 0.5030, 0.4412, 0.3810, 0.3210, 0.2650, 0.2170, 0.1750, 0.1382, 0.1070, 0.0816, 0.0610, 0.0446, 0.0320, 0.0232, 0.0170, 0.0119, 0.0082, 0.0158, 0.0041, 0.0029, 0.0021, 0.0015, 0.0010, 0.0007, 0.0005, 0.0004, 0.0002, 0.0002, 0.0001, 0.0001, 0.0001, 0.0000, 0.0000, 0.0000, 0.0000 } ); // Photopic response corresponding to wavelengths in wlt3

	// MODULE VARIABLE DECLARATIONS:

	int ngllayer; // Number of glass layers
	int nglface; // Number of glass faces
	int nglfacep; // Number of glass faces, + 2 if shade layer present
	Real64 tout; // Outside air temperature (K)
	Real64 tin; // Inside air temperature (previous timestep) (K)
	Real64 tilt; // Window tilt (deg)
	Real64 tiltr; // Window tilt (radians)
	Real64 hcin; // Convective inside air film conductance (W/m2-K)
	Real64 hcout; // Convective outside air film conductance (W/m2-K)
	Real64 Ebout; // Sigma*(outside air temp)**4 (W/m2)
	Real64 Outir; // IR radiance of window's exterior surround (W/m2)
	Real64 Rmir; // IR radiance of window's interior surround (W/m2)
	Real64 Rtot; // Total thermal resistance of window (m2-K/W)
	Array3D< Real64 > gcon( 3, 5, 5, 0.0 ); // Gas thermal conductivity coefficients for each gap
	Array3D< Real64 > gvis( 3, 5, 5, 0.0 ); // Gas viscosity coefficients for each gap
	Array3D< Real64 > gcp( 3, 5, 5, 0.0 ); // Gas specific-heat coefficients for each gap
	Array2D< Real64 > gwght( 5, 5, 0.0 ); // Gas molecular weights for each gap
	Array2D< Real64 > gfract( 5, 5, 0.0 ); // Gas fractions for each gap
	Array1D_int gnmix( 5, 0 ); // Number of gases in gap
	Array1D< Real64 > gap( 5, 0.0 ); // Gap width (m)
	Array1D< Real64 > thick( 5, 0.0 ); // Glass layer thickness (m)
	Array1D< Real64 > scon( 5, 0.0 ); // Glass layer conductance--conductivity/thickness (W/m2-K)
	Array1D< Real64 > tir( 10, 0.0 ); // Front and back IR transmittance for each glass layer
	Array1D< Real64 > emis( 10, 0.0 ); // Front and back IR emissivity for each glass layer
	Array1D< Real64 > rir( 10, 0.0 ); // Front and back IR reflectance for each glass layer
	//  (program calculates from tir and emis)
	Array1D< Real64 > AbsRadGlassFace( 10, 0.0 ); // Solar radiation and IR radiation from internal
	//  gains absorbed by glass face
	Array1D< Real64 > thetas( 10, 0.0 ); // Glass surface temperatures (K)
	Array1D< Real64 > thetasPrev( 10, 0.0 ); // Previous-iteration glass surface temperatures (K)
	Array1D< Real64 > fvec( 10, 0.0 ); // Glass face heat balance function
	Array2D< Real64 > fjac( 10, 10, 0.0 ); // Glass face heat balance Jacobian
	Array1D< Real64 > dtheta( 5, 0.0 ); // Glass layer temperature difference factor [K]
	Array2D< Real64 > zir( 10, 10, 0.0 ); // IR transfer matrix
	Array2D< Real64 > ziri( 10, 10, 0.0 ); // Inverse of IR transfer matrix
	Array2D< Real64 > ddeldt( 10, 10, 0.0 ); // Matrix of derivatives of residuals wrt temperature
	Array2D< Real64 > dtddel( 10, 10, 0.0 ); // Inverse of matrix of derivatives of
	//   residuals wrt temperature
	Array1D< Real64 > qf( 10, 0.0 ); // IR heat flux at each face [W/m2]
	Array1D< Real64 > hf( 10, 0.0 ); // Component of convective flux at each face
	Array2D< Real64 > der( 5, 10, 0.0 ); // Derivative of IR sources wrt surface temperature
	Array2D< Real64 > dhf( 5, 10, 0.0 ); // Derivative of heat flux wrt surface temperature
	Array1D< Real64 > sour( 10, 0.0 ); // IR source term at each face [W/m2]
	Array1D< Real64 > delta( 5, 0.0 ); // Residual at each glass layer [W/m2]
	Array1D< Real64 > hcgap( 5, 0.0 ); // Convective gap conductance
	Array1D< Real64 > hrgap( 5, 0.0 ); // Radiative gap conductance
	Array1D< Real64 > rgap( 6, 0.0 ); // Convective plus radiative gap resistance
	//   (inverse of hcgap + hrgap)
	Array1D< Real64 > rs( 6, 0.0 ); // Outside film convective resistance, gap resistances,
	//   inside air film convective resistance
	Array1D< Real64 > arhs( 6, 0.0 );
	Real64 A23P; // Intermediate variables in glass face
	Real64 A32P;
	Real64 A45P;
	Real64 A54P;
	Real64 A67P;
	Real64 A76P;
	Real64 A23; // heat balance equations
	Real64 A45;
	Real64 A67;

	Array2D< Real64 > wlt( 5, MaxSpectralDataElements, 0.0 ); // Spectral data wavelengths for each glass layer in a glazing system
	// Following data, Spectral data for each layer for each wavelength in wlt
	Array2D< Real64 > t( 5, MaxSpectralDataElements, 0.0 ); // normal transmittance
	Array2D< Real64 > rff( 5, MaxSpectralDataElements, 0.0 ); // normal front reflectance
	Array2D< Real64 > rbb( 5, MaxSpectralDataElements, 0.0 ); // normal back reflectance
	Array2D< Real64 > tPhi( 5, MaxSpectralDataElements, 0.0 ); // transmittance at angle of incidence
	Array2D< Real64 > rfPhi( 5, MaxSpectralDataElements, 0.0 ); // front reflectance at angle of incidence
	Array2D< Real64 > rbPhi( 5, MaxSpectralDataElements, 0.0 ); // back reflectance at angle of incidence
	Array2D< Real64 > tadjPhi( 5, MaxSpectralDataElements, 0.0 ); // transmittance at angle of incidence
	Array2D< Real64 > rfadjPhi( 5, MaxSpectralDataElements, 0.0 ); // front reflectance at angle of incidence
	Array2D< Real64 > rbadjPhi( 5, MaxSpectralDataElements, 0.0 ); // back reflectance at angle of incidence

	Array1D_int numpt( 5, 0 ); // Number of spectral data wavelengths for each layer; =2 if no spectra data for a layer
	Array1D< Real64 > stPhi( nume, 0.0 ); // Glazing system transmittance at angle of incidence for each wavelength in wle
	Array1D< Real64 > srfPhi( nume, 0.0 ); // Glazing system front reflectance at angle of incidence for each wavelength in wle
	Array1D< Real64 > srbPhi( nume, 0.0 ); // Glazing system back reflectance at angle of incidence for each wavelenth in wle
	Array2D< Real64 > saPhi( 5, nume, 0.0 ); // For each layer, glazing system absorptance at angle of incidence
	// for each wavelenth in wle
	Array2D< Real64 > top( 5, 5, 0.0 ); // Transmittance matrix for subr. op
	Array2D< Real64 > rfop( 5, 5, 0.0 ); // Front reflectance matrix for subr. op
	Array2D< Real64 > rbop( 5, 5, 0.0 ); // Back transmittance matrix for subr. op
	Array1D< Real64 > IndepVarCurveFit( 10, 0.0 ); // Values of independent variable (cos of inc. angle) for curve fit
	Array1D< Real64 > DepVarCurveFit( 10, 0.0 ); // Values of dependent variable corresponding to IndepVarCurveFit values
	Array1D< Real64 > CoeffsCurveFit( 6, 0.0 ); // Polynomial coefficients from curve fit
	Array1D< Real64 > tsolPhi( 10, 0.0 ); // Glazing system solar transmittance for each angle of incidence
	Array1D< Real64 > rfsolPhi( 10, 0.0 ); // Glazing system solar front reflectance for each angle of incidence
	Array1D< Real64 > rbsolPhi( 10, 0.0 ); // Glazing system solar back reflectance for each angle of incidence
	Array2D< Real64 > solabsPhi( 5, 10, 0.0 ); // Glazing system solar absorptance for each angle of incidence
	Array2D< Real64 > solabsBackPhi( 5, 10, 0.0 ); // Glazing system back solar absorptance for each angle of incidence
	Array1D< Real64 > solabsShadePhi( 10, 0.0 ); // Glazing system interior shade solar absorptance for each angle of incidence
	Array1D< Real64 > tvisPhi( 10, 0.0 ); // Glazing system visible transmittance for each angle of incidence
	Array1D< Real64 > rfvisPhi( 10, 0.0 ); // Glazing system visible front reflectance for each angle of incidence
	Array1D< Real64 > rbvisPhi( 10, 0.0 ); // Glazing system visible back reflectance for each angle of incidence
	Array1D< Real64 > CosPhiIndepVar( 10, 0.0 ); // Cos of incidence angles at 10-deg increments for curve fits

	// SUBROUTINE SPECIFICATIONS FOR MODULE WindowManager:
	//   Optical Calculation Routines
	//   Heat Balance Routines

	// MODULE SUBROUTINES:

	// Functions

	void
	clear_state()
	{
		wle = Array1D< Real64 >( nume, { 0.3000, 0.3050, 0.3100, 0.3150, 0.3200, 0.3250, 0.3300, 0.3350, 0.3400, 0.3450, 0.3500, 0.3600, 0.3700, 0.3800, 0.3900, 0.4000, 0.4100, 0.4200, 0.4300, 0.4400, 0.4500, 0.4600, 0.4700, 0.4800, 0.4900, 0.5000, 0.5100, 0.5200, 0.5300, 0.5400, 0.5500, 0.5700, 0.5900, 0.6100, 0.6300, 0.6500, 0.6700, 0.6900, 0.7100, 0.7180, 0.7244, 0.7400, 0.7525, 0.7575, 0.7625, 0.7675, 0.7800, 0.8000, 0.8160, 0.8237, 0.8315, 0.8400, 0.8600, 0.8800, 0.9050, 0.9150, 0.9250, 0.9300, 0.9370, 0.9480, 0.9650, 0.9800, 0.9935, 1.0400, 1.0700, 1.1000, 1.1200, 1.1300, 1.1370, 1.1610, 1.1800, 1.2000, 1.2350, 1.2900, 1.3200, 1.3500, 1.3950, 1.4425, 1.4625, 1.4770, 1.4970, 1.5200, 1.5390, 1.5580, 1.5780, 1.5920, 1.6100, 1.6300, 1.6460, 1.6780, 1.7400, 1.8000, 1.8600, 1.9200, 1.9600, 1.9850, 2.0050, 2.0350, 2.0650, 2.1000, 2.1480, 2.1980, 2.2700, 2.3600, 2.4500, 2.4940, 2.5370 } );
		e = Array1D< Real64 >( nume, { 0.0, 9.5, 42.3, 107.8, 181.0, 246.0, 395.3, 390.1, 435.3, 438.9, 483.7, 520.3, 666.2, 712.5, 720.7, 1013.1, 1158.2, 1184.0, 1071.9, 1302.0, 1526.0, 1599.6, 1581.0, 1628.3, 1539.2, 1548.7, 1586.5, 1484.9, 1572.4, 1550.7, 1561.5, 1501.5, 1395.5, 1485.3, 1434.1, 1419.9, 1392.3, 1130.0, 1316.7, 1010.3, 1043.2, 1211.2, 1193.9, 1175.5, 643.1, 1030.7, 1131.1, 1081.6, 849.2, 785.0, 916.4, 959.9, 978.9, 933.2, 748.5, 667.5, 690.3, 403.6, 258.3, 313.6, 526.8, 646.4, 746.8, 690.5, 637.5, 412.6, 108.9, 189.1, 132.2, 339.0, 460.0, 423.6, 480.5, 413.1, 250.2, 32.5, 1.6, 55.7, 105.1, 105.5, 182.1, 262.2, 274.2, 275.0, 244.6, 247.4, 228.7, 244.5, 234.8, 220.5, 171.5, 30.7, 2.0, 1.2, 21.2, 91.1, 26.8, 99.5, 60.4, 89.1, 82.2, 71.5, 70.2, 62.0, 21.2, 18.5, 3.2 } );
		wlt3 = Array1D< Real64 >( numt3, { 0.380, 0.385, 0.390, 0.395, 0.400, 0.405, 0.410, 0.415, 0.420, 0.425, 0.430, 0.435, 0.440, 0.445, 0.450, 0.455, 0.460, 0.465, 0.470, 0.475, 0.480, 0.485, 0.490, 0.495, 0.500, 0.505, 0.510, 0.515, 0.520, 0.525, 0.530, 0.535, 0.540, 0.545, 0.550, 0.555, 0.560, 0.565, 0.570, 0.575, 0.580, 0.585, 0.590, 0.595, 0.600, 0.605, 0.610, 0.615, 0.620, 0.625, 0.630, 0.635, 0.640, 0.645, 0.650, 0.655, 0.660, 0.665, 0.670, 0.675, 0.680, 0.685, 0.690, 0.695, 0.700, 0.705, 0.710, 0.715, 0.720, 0.725, 0.730, 0.735, 0.740, 0.745, 0.750, 0.755, 0.760, 0.765, 0.770, 0.775, 0.780 } );
		y30 = Array1D< Real64 >( numt3, { 0.0000, 0.0001, 0.0001, 0.0002, 0.0004, 0.0006, 0.0012, 0.0022, 0.0040, 0.0073, 0.0116, 0.0168, 0.0230, 0.0298, 0.0380, 0.0480, 0.0600, 0.0739, 0.0910, 0.1126, 0.1390, 0.1693, 0.2080, 0.2586, 0.3230, 0.4073, 0.5030, 0.6082, 0.7100, 0.7932, 0.8620, 0.9149, 0.9540, 0.9803, 0.9950, 1.0000, 0.9950, 0.9786, 0.9520, 0.9154, 0.8700, 0.8163, 0.7570, 0.6949, 0.6310, 0.5668, 0.5030, 0.4412, 0.3810, 0.3210, 0.2650, 0.2170, 0.1750, 0.1382, 0.1070, 0.0816, 0.0610, 0.0446, 0.0320, 0.0232, 0.0170, 0.0119, 0.0082, 0.0158, 0.0041, 0.0029, 0.0021, 0.0015, 0.0010, 0.0007, 0.0005, 0.0004, 0.0002, 0.0002, 0.0001, 0.0001, 0.0001, 0.0000, 0.0000, 0.0000, 0.0000 } );
		ngllayer = 0;
		nglface = 0;
		nglfacep = 0;
		tout = 0.0;
		tin = 0.0;
		tilt = 0.0;
		tiltr = 0.0;
		hcin = 0.0;
		hcout = 0.0;
		Ebout = 0.0;
		Outir = 0.0;
		Rmir = 0.0;
		Rtot = 0.0;
		gcon = Array3D< Real64 >( 3, 5, 5, 0.0 );
		gvis = Array3D< Real64 >( 3, 5, 5, 0.0 );
		gcp = Array3D< Real64 >( 3, 5, 5, 0.0 );
		gwght = Array2D< Real64 >( 5, 5, 0.0 );
		gfract = Array2D< Real64 >( 5, 5, 0.0 );
		gnmix = Array1D_int( 5, 0 );
		gap = Array1D< Real64 >( 5, 0.0 );
		thick = Array1D< Real64 >( 5, 0.0 );
		scon = Array1D< Real64 >( 5, 0.0 );
		tir = Array1D< Real64 >( 10, 0.0 );
		emis = Array1D< Real64 >( 10, 0.0 );
		rir = Array1D< Real64 >( 10, 0.0 );
		AbsRadGlassFace = Array1D< Real64 >( 10, 0.0 );
		thetas = Array1D< Real64 >( 10, 0.0 );
		thetasPrev = Array1D< Real64 >( 10, 0.0 );
		fvec = Array1D< Real64 >( 10, 0.0 );
		fjac = Array2D< Real64 >( 10, 10, 0.0 );
		dtheta = Array1D< Real64 >( 5, 0.0 );
		zir = Array2D< Real64 >( 10, 10, 0.0 );
		ziri = Array2D< Real64 >( 10, 10, 0.0 );
		ddeldt = Array2D< Real64 >( 10, 10, 0.0 );
		dtddel = Array2D< Real64 >( 10, 10, 0.0 );
		qf = Array1D< Real64 >( 10, 0.0 );
		hf = Array1D< Real64 >( 10, 0.0 );
		der = Array2D< Real64 >( 5, 10, 0.0 );
		dhf = Array2D< Real64 >( 5, 10, 0.0 );
		sour = Array1D< Real64 >( 10, 0.0 );
		delta = Array1D< Real64 >( 5, 0.0 );
		hcgap = Array1D< Real64 >( 5, 0.0 );
		hrgap = Array1D< Real64 >( 5, 0.0 );
		rgap = Array1D< Real64 >( 6, 0.0 );
		rs = Array1D< Real64 >( 6, 0.0 );
		arhs = Array1D< Real64 >( 6, 0.0 );
		A23P = 0.0;
		A32P = 0.0;
		A45P = 0.0;
		A54P = 0.0;
		A67P = 0.0;
		A76P = 0.0;
		A23 = 0.0;
		A45 = 0.0;
		A67 = 0.0;
		wlt = Array2D< Real64 >( 5, MaxSpectralDataElements, 0.0 );
		t = Array2D< Real64 >( 5, MaxSpectralDataElements, 0.0 );
		rff = Array2D< Real64 >( 5, MaxSpectralDataElements, 0.0 );
		rbb = Array2D< Real64 >( 5, MaxSpectralDataElements, 0.0 );
		tPhi = Array2D< Real64 >( 5, MaxSpectralDataElements, 0.0 );
		rfPhi = Array2D< Real64 >( 5, MaxSpectralDataElements, 0.0 );
		rbPhi = Array2D< Real64 >( 5, MaxSpectralDataElements, 0.0 );
		tadjPhi = Array2D< Real64 >( 5, MaxSpectralDataElements, 0.0 );
		rfadjPhi = Array2D< Real64 >( 5, MaxSpectralDataElements, 0.0 );
		rbadjPhi = Array2D< Real64 >( 5, MaxSpectralDataElements, 0.0 );
		numpt = Array1D_int( 5, 0 );
		stPhi = Array1D< Real64 >( nume, 0.0 );
		srfPhi = Array1D< Real64 >( nume, 0.0 );
		srbPhi = Array1D< Real64 >( nume, 0.0 );
		saPhi = Array2D< Real64 >( 5, nume, 0.0 );
		top = Array2D< Real64 >( 5, 5, 0.0 );
		rfop = Array2D< Real64 >( 5, 5, 0.0 );
		rbop = Array2D< Real64 >( 5, 5, 0.0 );
		IndepVarCurveFit = Array1D< Real64 >( 10, 0.0 );
		DepVarCurveFit = Array1D< Real64 >( 10, 0.0 );
		CoeffsCurveFit = Array1D< Real64 >( 6, 0.0 );
		tsolPhi = Array1D< Real64 >( 10, 0.0 );
		rfsolPhi = Array1D< Real64 >( 10, 0.0 );
		rbsolPhi = Array1D< Real64 >( 10, 0.0 );
		solabsPhi = Array2D< Real64 >( 5, 10, 0.0 );
		solabsBackPhi = Array2D< Real64 >( 5, 10, 0.0 );
		solabsShadePhi = Array1D< Real64 >( 10, 0.0 );
		tvisPhi = Array1D< Real64 >( 10, 0.0 );
		rfvisPhi = Array1D< Real64 >( 10, 0.0 );
		rbvisPhi = Array1D< Real64 >( 10, 0.0 );
		CosPhiIndepVar = Array1D< Real64 >( 10, 0.0 );
	}

	void
	InitGlassOpticalCalculations()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   August 1999
		//       MODIFIED       May 2001 (FW): add window blinds
		//                      Jan 2002 (FW): add blinds with variable slat angle
		//                      Jan 2003 (FW): add between-glass shade/blind
		//                      May 2006 (RR): add exterior window screen
		//                      Aug 2010 (TH): allow spectral data for between-glass shade/blind
		//                      Aug 2013 (TH): allow user defined solar and visible spectrum data
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages the calculation of the solar and visible properties of a multi-layer glazing
		// system from the properties of the individual glazing and shading layers

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;
		using General::TrimSigDigits;
		using WindowEquivalentLayer::InitEquivalentLayerWindowCalculations;

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
		int CoefNum; // Polynomial coefficient number
		int j; // Wavelength counter
		int TotLay; // Total solid and gas layers in a window construction
		int ConstrNum; // Construction number
		int ConstrNumSh; // Shaded construction number
		int SurfNum; // Surface number
		int ShadeLayNum; // Layer number for shade or blind, if present
		int ShadeLayPtr; // Material number for shade or blind
		bool lquasi; // True if one or more glass layers have no spectral data
		bool AllGlassIsSpectralAverage; // True if all glazing in a construction is spectral average
		bool IntShade; // True if construction has an interior,exterior or between-glass shade
		bool ExtShade;
		bool BGShade;
		bool IntBlind; // True if construction has an interior,exterior or between-glass blind
		bool ExtBlind;
		bool BGBlind;
		bool ExtScreen; // True if construction has an exterior screen
		bool ScreenOn; // True if construction has an exterior screen
		bool BlindOn; // True if IntBlind, ExtBlind or BGBlind is true
		bool ShadeOn; // True if IntShade, ExtShade or BGShade is true
		int BlNum; // Blind number
		int ScNum; // Screen number
		Array1D< Real64 > sabsPhi( nume ); // Glazing system absorptance for a glass layer
		//  and angle of incidence, for each wavelength
		//   glass layer for an angle of incidence, for each wavelength
		Array1D< Real64 > solabsDiff( 5 ); // Glazing system layer solar absorptance for each glass layer
		Array1D< Real64 > solabsPhiLay( 10 ); // Glazing system solar absorptance for a layer at each incidence angle
		Array1D< Real64 > tsolPhiFit( 10 ); // Glazing system solar transmittance from fit at each incidence angle
		Array1D< Real64 > tvisPhiFit( 10 ); // Glazing system visible transmittance from fit at each incidence angle
		Array2D< Real64 > tBareSolPhi( 5, 10 ); // Isolated glass solar transmittance for each incidence angle
		Real64 t1; // = tBareSolPhi(,1)(,2)
		Real64 t2;
		Array2D< Real64 > tBareVisPhi( 5, 10 ); // Isolated glass visible transmittance for each incidence angle
		Real64 t1v; // = tBareVisPhi(,1)(,2)
		Real64 t2v;
		Array2D< Real64 > rfBareSolPhi( 5, 10 ); // Isolated glass front solar reflectance for each incidence angle
		Array2D< Real64 > rfBareVisPhi( 5, 10 ); // Isolated glass front visible reflectance for each incidence angle
		Array2D< Real64 > rbBareSolPhi( 5, 10 ); // Isolated glass back solar reflectance for each incidence angle
		Array2D< Real64 > rbBareVisPhi( 5, 10 ); // Isolated glass back visible reflectance for each incidence angle
		Array2D< Real64 > afBareSolPhi( 5, 10 ); // Isolated glass front solar absorptance for each incidence angle
		Real64 af1; // = afBareSolPhi(,1)(,2)
		Real64 af2;
		Real64 rbmf2; // Isolated glass #2 front beam reflectance
		Array2D< Real64 > abBareSolPhi( 5, 10 ); // Isolated glass back solar absorptance for each incidence angle
		Real64 ab1; // = abBareSolPhi(,1)(,2)
		Real64 ab2;
		Real64 td1; // Isolated glass diffuse solar transmittance
		Real64 td2;
		Real64 td3;
		Real64 td1v; // Isolated glass diffuse visible transmittance
		Real64 td2v;
		Real64 td3v;
		Real64 rf1; // Isolated glass diffuse solar front reflectance
		Real64 rf2;
		Real64 rf3;
		Real64 rf1v; // Isolated glass diffuse visible front reflectance
		Real64 rf2v;
		Real64 rf3v;
		Real64 rb1; // Isolated glass diffuse solar back reflectance
		Real64 rb2;
		Real64 rb3;
		Real64 rb1v; // Isolated glass diffuse visible back reflectance
		Real64 rb2v;
		Real64 rb3v;
		Real64 afd1; // Isolated glass diffuse solar front absorptance
		Real64 afd2;
		Real64 afd3;
		Real64 abd1; // Isolated glass diffuse solar back absorptance
		Real64 abd2;
		Real64 abd3;
		Real64 TauShIR; // IR transmittance of isolated shade
		Real64 EpsShIR; // IR absorptance of isolated shade
		Real64 RhoShIR; // IR reflectance of isolated shade
		Real64 EpsGlIR; // IR absorptance of front or back of isolated glass
		Real64 RhoGlIR; // IR reflectance of inside face of inside glass
		int NGlass; // Number of glass layers in a construction
		int IGlass; // Glass layer counter
		int LayNum; // Layer number for a glass layer
		int LayPtr; // Material number corresponding to LayNum
		int IPhi; // Incidence angle counter
		Real64 Phi; // Incidence angle (deg)
		Real64 CosPhi; // Cosine of incidence angle
		int ILam; // Wavelength counter
		Real64 tsolDiff; // Glazing system diffuse solar transmittance
		Real64 tvisDiff; // Glazing system diffuse visible transmittance
		int IGlassBack; // Glass layer number counted from back of window
		Real64 ShadeAbs; // Solar absorptance of isolated shade
		Real64 ash; // = ShadeAbs
		Real64 afsh; // Diffuse solar front absorptance of isolated blind
		Real64 afshGnd; // Ground and sky diffuse solar front absorptance of isolated blind
		Real64 afshSky;
		Real64 absh; // Diffuse solar back absorptance of isolated blind
		Real64 ShadeTrans; // Solar transmittance of isolated shade/blind
		Real64 ShadeTransGnd; // Diffuse-diffuse transmittance of isolated vertical blind with
		// horizontal slats for isotropic ground solar
		Real64 ShadeTransSky; // Diffuse-diffuse transmittance of isolated vertical blind with
		// horizontal slats for isotropic sky solar
		Real64 tsh; // = ShadeTrans
		Real64 tshGnd; // = ShadeTransGnd,ShadeTransSky
		Real64 tshSky;
		Real64 tsh2; // = tsh**2
		Real64 ShadeRefl; // Solar reflectance of isolated shade
		Real64 ShadeReflGnd; // Front blind reflectance for ground diffuse solar
		Real64 ShadeReflSky; // Front blind reflectance for sky diffuse solar
		Real64 rsh; // = ShadeRefl
		Real64 rfsh; // Diffuse solar front reflectance of isolated blind
		Real64 rfshGnd; // Ground and sky diffuse solar front reflectance of isolated blind
		Real64 rfshSky;
		Real64 rbsh; // Diffuse solar back reflectance of isolated blind
		Real64 ShadeReflFac; // Shade/blind solar reflection factor
		Real64 ShadeTransVis; // Visible transmittance of isolated shade/blind
		Real64 tshv; // = ShadeTransVis
		Real64 tshv2; // = tshv**2
		Real64 ShadeReflVis; // Visible reflectance of isolated shade
		Real64 rshv; // = ShadeReflVis
		Real64 rfshv; // Diffuse visible front reflectance of isolated blind
		Real64 rbshv; // Diffuse visible back reflectance of isolated blind
		Real64 ShadeReflFacVis; // Shade/blind visible reflection factor
		int SpecDataNum; // Spectral data set number
		int numptDAT; // Number of wavelengths in a spectral data set
		int ISlatAng; // Slat angle counter
		bool StormWinConst; // True if a construction with a storm window
		bool Triangle; // True if window is triangular
		bool Rectangle; // True if window is rectangular
		Array1D< Real64 > W1( 3 ); // Window vertices (m)
		Array1D< Real64 > W2( 3 );
		Array1D< Real64 > W3( 3 );
		Array1D< Real64 > W21( 3 ); // W1-W2, W3-W2, resp. (m)
		Array1D< Real64 > W23( 3 );
		static bool lSimpleGlazingSystem( false ); // true if using simple glazing system block model
		static Real64 SimpleGlazingSHGC( 0.0 ); // value of SHGC for simple glazing system block model
		static Real64 SimpleGlazingU( 0.0 ); // value of U-factor for simple glazing system block model
		static bool BGFlag( false ); // True if between-glass shade or blind
		static Real64 tmpTrans( 0.0 ); // solar transmittance calculated from spectral data
		static Real64 tmpTransVis( 0.0 ); // visible transmittance calculated from spectral data
		static Real64 tmpReflectSolBeamFront( 0.0 );
		static Real64 tmpReflectSolBeamBack( 0.0 );
		static Real64 tmpReflectVisBeamFront( 0.0 );
		static Real64 tmpReflectVisBeamBack( 0.0 );

		//Debug
		static Array1D< Real64 > DbgTheta( 11, { 0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 82.5, 89.5 } );
		static Array1D< Real64 > DbgTSol( 11, 0.0 );
		static Array1D< Real64 > DbgRbSol( 11, 0.0 );
		static Array1D< Real64 > DbgTVis( 11, 0.0 );
		static Array2D< Real64 > DbgFtAbs( 5, 11, 0.0 );
		static Array2D< Real64 > DbgBkAbs( 5, 11, 0.0 );
		static Array1D< Real64 > DbgFTAbsDiff( 5, 0.0 );
		static Array1D< Real64 > DbgBkAbsDiff( 5, 0.0 );

		//EndDebug

		// check and read custom solar and/or visible spectrum data if any
		CheckAndReadCustomSprectrumData();

		W5InitGlassParameters();

		// Calculate optical properties of blind-type layers entered with MATERIAL:WindowBlind
		if ( TotBlinds > 0 ) CalcWindowBlindProperties();

		// Initialize SurfaceScreen structure
		if ( NumSurfaceScreens > 0 ) CalcWindowScreenProperties();

		// Get glazing system optical properties of constructions with glass or glass plus
		//   shade, screen or blind
		// Loop over constructions and find those that are glazing constructions
		for ( ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum ) {
			if ( ! Construct( ConstrNum ).TypeIsWindow ) continue;
			if ( Construct( ConstrNum ).WindowTypeBSDF ) continue; //Skip Complex Fenestrations, they have separate
			if ( Construct( ConstrNum ).WindowTypeEQL ) continue; //skip Equivalent Layer Fenestration
			//handling of optical properties

			TotLay = Construct( ConstrNum ).TotLayers;

			// First layer must be glass, shade, screen or blind to be a glazing construction
			if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group != WindowGlass && Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group != Shade && Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group != Screen && Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group != WindowBlind && Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group != WindowSimpleGlazing ) continue;

			ShadeLayNum = 0;
			ExtShade = false;
			IntShade = false;
			BGShade = false;
			ExtBlind = false;
			IntBlind = false;
			BGBlind = false;
			ExtScreen = false;
			StormWinConst = false;
			lSimpleGlazingSystem = false;

			if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == WindowSimpleGlazing ) {
				// what if outside layer is shade, blind, or screen?
				lSimpleGlazingSystem = true;
				SimpleGlazingSHGC = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).SimpleWindowSHGC;
				SimpleGlazingU = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).SimpleWindowUfactor;
			}

			if ( has_prefix( Construct( ConstrNum ).Name, "BARECONSTRUCTIONWITHSTORMWIN" ) || has_prefix( Construct( ConstrNum ).Name, "SHADEDCONSTRUCTIONWITHSTORMWIN" ) ) StormWinConst = true;

			// Get layer number of shade/blind
			if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == Shade ) {
				ExtShade = true;
				ShadeLayNum = 1;
			} else if ( Material( Construct( ConstrNum ).LayerPoint( TotLay ) ).Group == Shade ) {
				IntShade = true;
				ShadeLayNum = TotLay;
			} else if ( Construct( ConstrNum ).TotLayers == 5 ) {
				if ( Material( Construct( ConstrNum ).LayerPoint( 3 ) ).Group == Shade ) {
					BGShade = true;
					ShadeLayNum = 3;
				}
			} else if ( Construct( ConstrNum ).TotLayers == 7 ) {
				if ( Material( Construct( ConstrNum ).LayerPoint( 5 ) ).Group == Shade ) {
					BGShade = true;
					ShadeLayNum = 5;
				}
			}

			if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == WindowBlind ) {
				ExtBlind = true;
				ShadeLayNum = 1;
				BlNum = Material( Construct( ConstrNum ).LayerPoint( ShadeLayNum ) ).BlindDataPtr;
			} else if ( Material( Construct( ConstrNum ).LayerPoint( TotLay ) ).Group == WindowBlind ) {
				IntBlind = true;
				ShadeLayNum = TotLay;
				BlNum = Material( Construct( ConstrNum ).LayerPoint( ShadeLayNum ) ).BlindDataPtr;
			} else if ( Construct( ConstrNum ).TotLayers == 5 ) {
				if ( Material( Construct( ConstrNum ).LayerPoint( 3 ) ).Group == WindowBlind ) {
					BGBlind = true;
					ShadeLayNum = 3;
					BlNum = Material( Construct( ConstrNum ).LayerPoint( ShadeLayNum ) ).BlindDataPtr;
				}
			} else if ( Construct( ConstrNum ).TotLayers == 7 ) {
				if ( Material( Construct( ConstrNum ).LayerPoint( 5 ) ).Group == WindowBlind ) {
					BGBlind = true;
					ShadeLayNum = 5;
					BlNum = Material( Construct( ConstrNum ).LayerPoint( ShadeLayNum ) ).BlindDataPtr;
				}
			}

			if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == Screen ) {
				ShadeLayNum = 1;
				ScNum = Material( Construct( ConstrNum ).LayerPoint( ShadeLayNum ) ).ScreenDataPtr;
				//   Disregard orphaned constructs with exterior screen
				if ( ScNum == 0 ) continue;
				ExtScreen = true;
			}

			ScreenOn = ExtScreen;
			BlindOn = IntBlind || ExtBlind || BGBlind;
			ShadeOn = IntShade || ExtShade || BGShade;
			BGFlag = BGBlind || BGShade;

			// For construction with interior or exterior shade, get shade thermal absorptance (emissivity)
			// (accounting for inter-reflection with glazing) and correct the inside glass InsideAbsorpThermal
			// for presence of interior shade. Assumes inner and outer glass layers have zero thermal transmittance.

			if ( IntShade || ExtShade || ExtScreen ) {
				ShadeLayPtr = Construct( ConstrNum ).LayerPoint( ShadeLayNum );
				if ( ExtScreen ) {
					TauShIR = SurfaceScreens( ScNum ).DifDifTrans;
				} else {
					TauShIR = Material( ShadeLayPtr ).TransThermal;
				}
				EpsShIR = Material( ShadeLayPtr ).AbsorpThermal;
				RhoShIR = max( 0.0, 1.0 - TauShIR - EpsShIR );
				if ( ExtShade || ExtScreen ) { // Exterior shade or screen
					EpsGlIR = Material( Construct( ConstrNum ).LayerPoint( 2 ) ).AbsorpThermalFront;
				} else { // Interior shade
					EpsGlIR = Material( Construct( ConstrNum ).LayerPoint( TotLay - 1 ) ).AbsorpThermalBack;
				}
				RhoGlIR = max( 0.0, 1.0 - EpsGlIR );
				Construct( ConstrNum ).ShadeAbsorpThermal = EpsShIR * ( 1.0 + TauShIR * RhoGlIR / ( 1.0 - RhoShIR * RhoGlIR ) );
				if ( IntShade ) Construct( ConstrNum ).InsideAbsorpThermal *= TauShIR / ( 1.0 - RhoShIR * RhoGlIR );
			}

			// From the individual glass layer properties, get the glazing system optical properties
			// for BARE GLASS (i.e., interior, exterior or between-glass shade or blind, or exterior screen, if present, not in place).
			// Get one set of system properties for solar incident on front of
			// window and a second set for solar incident on back of window. (The back-incident
			// properties are used with interior short-wave radiation striking the window from inside.)

			// After the front and back system optical properties are calculated for bare glass,
			// a correction is made for the effect of a shade, screen or blind if one of these
			// is present in the construction.

			NGlass = Construct( ConstrNum ).TotGlassLayers;

			//--------------------------------------------------------------------------------------------
			// Front calculation (solar incident from outside of room); bare glass portion of construction
			//--------------------------------------------------------------------------------------------

			lquasi = false;
			AllGlassIsSpectralAverage = true;

			// Loop over glass layers in the construction
			for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
				LayNum = 1 + 2 * ( IGlass - 1 );
				if ( ExtShade || ExtBlind || ExtScreen ) LayNum = 2 + 2 * ( IGlass - 1 );
				if ( BGShade || BGBlind ) {
					LayNum = 1;
					if ( NGlass == 2 ) {
						if ( IGlass == 2 ) LayNum = 5;
					} else { // NGlass = 3
						if ( IGlass == 2 ) LayNum = 3;
						if ( IGlass == 3 ) LayNum = 7;
					}
				}

				LayPtr = Construct( ConstrNum ).LayerPoint( LayNum );
				SpecDataNum = Material( LayPtr ).GlassSpectralDataPtr;
				if ( SpecDataNum != 0 ) {
					if ( ! BGFlag ) AllGlassIsSpectralAverage = false;

					// Get the spectral data for the transmittance, front reflectance and
					// back reflectance (all at normal incidence) for this layer.
					// In this case, "front" means incident from the outside and "back"
					// means incident from the inside.
					numptDAT = SpectralData( SpecDataNum ).NumOfWavelengths;
					numpt( IGlass ) = numptDAT;

					for ( ILam = 1; ILam <= numptDAT; ++ILam ) {
						wlt( IGlass, ILam ) = SpectralData( SpecDataNum ).WaveLength( ILam );
						t( IGlass, ILam ) = SpectralData( SpecDataNum ).Trans( ILam );
						if ( ( IGlass == 1 || ( IGlass == 2 && StormWinConst ) ) && ( ! BGFlag ) ) t( IGlass, ILam ) *= Material( LayPtr ).GlassTransDirtFactor;
						rff( IGlass, ILam ) = SpectralData( SpecDataNum ).ReflFront( ILam );
						rbb( IGlass, ILam ) = SpectralData( SpecDataNum ).ReflBack( ILam );
					}

					// TH 8/26/2010, CR 8206
					// If there is spectral data for between-glass shades or blinds, calc the average spectral properties for use.
					if ( BGFlag ) {
						// 5/16/2012 CR 8793. Add warning message for the glazing defined with full spectral data.
						ShowWarningError( "Window glazing material \"" + Material( LayPtr ).Name + "\" was defined with full spectral data and has been converted to average spectral data" );
						ShowContinueError( "due to its use with between-glass shades or blinds of the window construction \"" + Construct( ConstrNum ).Name + "\"." );
						ShowContinueError( "All occurrences of this glazing material will be modeled as SpectralAverage." );
						ShowContinueError( "If this material is also used in other window constructions  without between-glass shades or blinds," );
						ShowContinueError( "then make a duplicate material (with new name) if you want to model those windows  (and reference the new material) using the full spectral data." );
						// calc Trans, TransVis, ReflectSolBeamFront, ReflectSolBeamBack, ReflectVisBeamFront, ReflectVisBeamBack
						//  assuming wlt same as wle
						SolarSprectrumAverage( t, tmpTrans );
						SolarSprectrumAverage( rff, tmpReflectSolBeamFront );
						SolarSprectrumAverage( rbb, tmpReflectSolBeamBack );

						// visible properties
						VisibleSprectrumAverage( t, tmpTransVis );
						VisibleSprectrumAverage( rff, tmpReflectVisBeamFront );
						VisibleSprectrumAverage( rbb, tmpReflectVisBeamBack );

						// set this material to average spectral data
						Material( LayPtr ).GlassSpectralDataPtr = 0;
						Material( LayPtr ).Trans = tmpTrans;
						Material( LayPtr ).TransVis = tmpTransVis;
						Material( LayPtr ).ReflectSolBeamFront = tmpReflectSolBeamFront;
						Material( LayPtr ).ReflectSolBeamBack = tmpReflectSolBeamBack;
						Material( LayPtr ).ReflectVisBeamFront = tmpReflectVisBeamFront;
						Material( LayPtr ).ReflectVisBeamBack = tmpReflectVisBeamBack;
						SpecDataNum = 0;
					}
				}

				if ( SpecDataNum == 0 ) { // No spectral data for this layer; use spectral average values
					lquasi = true;
					numpt( IGlass ) = 2;
					t( IGlass, 1 ) = Material( LayPtr ).Trans;
					if ( IGlass == 1 || ( IGlass == 2 && StormWinConst ) ) t( IGlass, 1 ) *= Material( LayPtr ).GlassTransDirtFactor;
					t( IGlass, 2 ) = Material( LayPtr ).TransVis;
					if ( IGlass == 1 || ( IGlass == 2 && StormWinConst ) ) t( IGlass, 2 ) *= Material( LayPtr ).GlassTransDirtFactor;
					rff( IGlass, 1 ) = Material( LayPtr ).ReflectSolBeamFront;
					rbb( IGlass, 1 ) = Material( LayPtr ).ReflectSolBeamBack;
					rff( IGlass, 2 ) = Material( LayPtr ).ReflectVisBeamFront;
					rbb( IGlass, 2 ) = Material( LayPtr ).ReflectVisBeamBack;
				}
			} // End of loop over glass layers in the construction for front calculation

			// Loop over incidence angle from 0 to 90 deg in 10 deg increments.
			// Get glass layer properties, then glazing system properties (which include the
			// effect of inter-reflection among glass layers) at each incidence angle.

			for ( IPhi = 1; IPhi <= 10; ++IPhi ) {
				Phi = double( IPhi - 1 ) * 10.0;
				CosPhi = std::cos( Phi * DegToRadians );
				if ( std::abs( CosPhi ) < 0.0001 ) CosPhi = 0.0;

				// For each wavelength, get glass layer properties at this angle of incidence
				// from properties at normal incidence
				for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
					for ( ILam = 1; ILam <= numpt( IGlass ); ++ILam ) {

						TransAndReflAtPhi( CosPhi, t( IGlass, ILam ), rff( IGlass, ILam ), rbb( IGlass, ILam ), tPhi( IGlass, ILam ), rfPhi( IGlass, ILam ), rbPhi( IGlass, ILam ), lSimpleGlazingSystem, SimpleGlazingSHGC, SimpleGlazingU );
					}

					// For use with between-glass shade/blind, save angular properties of isolated glass
					// for case that all glass layers were input with spectral-average properties
					//  only used by between-glass shades or blinds
					if ( AllGlassIsSpectralAverage ) {
						tBareSolPhi( IGlass, IPhi ) = tPhi( IGlass, 1 );
						tBareVisPhi( IGlass, IPhi ) = tPhi( IGlass, 2 );
						rfBareSolPhi( IGlass, IPhi ) = rfPhi( IGlass, 1 );
						rfBareVisPhi( IGlass, IPhi ) = rfPhi( IGlass, 2 );
						rbBareSolPhi( IGlass, IPhi ) = rbPhi( IGlass, 1 );
						rbBareVisPhi( IGlass, IPhi ) = rbPhi( IGlass, 2 );
						afBareSolPhi( IGlass, IPhi ) = max( 0.0, 1.0 - ( tBareSolPhi( IGlass, IPhi ) + rfBareSolPhi( IGlass, IPhi ) ) );
						abBareSolPhi( IGlass, IPhi ) = max( 0.0, 1.0 - ( tBareSolPhi( IGlass, IPhi ) + rbBareSolPhi( IGlass, IPhi ) ) );
					}
				}

				// For each wavelength in the solar spectrum, calculate system properties
				// stPhi, srfPhi, srbPhi and saPhi at this angle of incidence.
				// In the following the argument "1" indicates that spectral average solar values
				// should be used for layers without spectral data.
				SystemSpectralPropertiesAtPhi( 1, NGlass, 0.0, 2.54 );

				// Get solar properties of system by integrating over solar irradiance spectrum.
				// For now it is assumed that the exterior and interior irradiance spectra are the same.
				SolarSprectrumAverage( stPhi, tsolPhi( IPhi ) );
				SolarSprectrumAverage( srfPhi, rfsolPhi( IPhi ) );
				SolarSprectrumAverage( srbPhi, rbsolPhi( IPhi ) );

				for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
					for ( ILam = 1; ILam <= nume; ++ILam ) {
						sabsPhi( ILam ) = saPhi( IGlass, ILam );
					}
					SolarSprectrumAverage( sabsPhi, solabsPhi( IGlass, IPhi ) );
				}

				// Get visible properties of system by integrating over solar irradiance
				// spectrum weighted by photopic response.
				// Need to redo the calculation of system spectral properties here only if
				// one or more glass layers have no spectral data (lquasi = .TRUE.); in this
				// case the spectral average visible properties will be used for the layers
				// without spectral data, as indicated by the argument "2".

				if ( lquasi ) SystemSpectralPropertiesAtPhi( 2, NGlass, 0.37, 0.78 );
				VisibleSprectrumAverage( stPhi, tvisPhi( IPhi ) );
				VisibleSprectrumAverage( srfPhi, rfvisPhi( IPhi ) );
				VisibleSprectrumAverage( srbPhi, rbvisPhi( IPhi ) );

			} // End of loop over incidence angles for front calculation

			//  only used by between-glass shades or blinds
			if ( AllGlassIsSpectralAverage ) {
				for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
					W5LsqFit( CosPhiIndepVar, tBareSolPhi( IGlass, _ ), 6, 1, 10, Construct( ConstrNum ).tBareSolCoef( _, IGlass ) );
					W5LsqFit( CosPhiIndepVar, tBareVisPhi( IGlass, _ ), 6, 1, 10, Construct( ConstrNum ).tBareVisCoef( _, IGlass ) );
					W5LsqFit( CosPhiIndepVar, rfBareSolPhi( IGlass, _ ), 6, 1, 10, Construct( ConstrNum ).rfBareSolCoef( _, IGlass ) );
					W5LsqFit( CosPhiIndepVar, rfBareVisPhi( IGlass, _ ), 6, 1, 10, Construct( ConstrNum ).rfBareVisCoef( _, IGlass ) );
					W5LsqFit( CosPhiIndepVar, rbBareSolPhi( IGlass, _ ), 6, 1, 10, Construct( ConstrNum ).rbBareSolCoef( _, IGlass ) );
					W5LsqFit( CosPhiIndepVar, rbBareVisPhi( IGlass, _ ), 6, 1, 10, Construct( ConstrNum ).rbBareVisCoef( _, IGlass ) );
					W5LsqFit( CosPhiIndepVar, afBareSolPhi( IGlass, _ ), 6, 1, 10, Construct( ConstrNum ).afBareSolCoef( _, IGlass ) );
					W5LsqFit( CosPhiIndepVar, abBareSolPhi( IGlass, _ ), 6, 1, 10, Construct( ConstrNum ).abBareSolCoef( _, IGlass ) );
				}
			}

			Construct( ConstrNum ).ReflectSolDiffFront = DiffuseAverage( rfsolPhi );
			Construct( ConstrNum ).ReflectSolDiffBack = DiffuseAverage( rbsolPhi );
			Construct( ConstrNum ).ReflectVisDiffFront = DiffuseAverage( rfvisPhi );
			Construct( ConstrNum ).ReflectVisDiffBack = DiffuseAverage( rbvisPhi );

			tsolDiff = DiffuseAverage( tsolPhi );
			tvisDiff = DiffuseAverage( tvisPhi );
			Construct( ConstrNum ).TransDiff = tsolDiff;
			Construct( ConstrNum ).TransDiffVis = tvisDiff;
			for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
				solabsPhiLay( {1,10} ) = solabsPhi( IGlass, {1,10} );
				solabsDiff( IGlass ) = DiffuseAverage( solabsPhiLay );
				Construct( ConstrNum ).AbsDiff( IGlass ) = solabsDiff( IGlass );

				// For use with between-glass shade/blind, get diffuse properties of isolated glass for case when
				// all glass layers were input with spectral-average properties
				//  only used by between-glass shades or blinds
				if ( AllGlassIsSpectralAverage ) {
					Construct( ConstrNum ).tBareSolDiff( IGlass ) = DiffuseAverage( tBareSolPhi( IGlass, {1,10} ) );
					Construct( ConstrNum ).tBareVisDiff( IGlass ) = DiffuseAverage( tBareVisPhi( IGlass, {1,10} ) );
					Construct( ConstrNum ).rfBareSolDiff( IGlass ) = DiffuseAverage( rfBareSolPhi( IGlass, {1,10} ) );
					Construct( ConstrNum ).rfBareVisDiff( IGlass ) = DiffuseAverage( rfBareVisPhi( IGlass, {1,10} ) );
					Construct( ConstrNum ).rbBareSolDiff( IGlass ) = DiffuseAverage( rbBareSolPhi( IGlass, {1,10} ) );
					Construct( ConstrNum ).rbBareVisDiff( IGlass ) = DiffuseAverage( rbBareVisPhi( IGlass, {1,10} ) );
					Construct( ConstrNum ).afBareSolDiff( IGlass ) = max( 0.0, 1.0 - ( Construct( ConstrNum ).tBareSolDiff( IGlass ) + Construct( ConstrNum ).rfBareSolDiff( IGlass ) ) );
					Construct( ConstrNum ).abBareSolDiff( IGlass ) = max( 0.0, 1.0 - ( Construct( ConstrNum ).tBareSolDiff( IGlass ) + Construct( ConstrNum ).rbBareSolDiff( IGlass ) ) );
				}
			}

			//------------------------------------------------------------------------------------------
			// Back calculation (solar incident from inside of room); bare glass portion of construction
			//------------------------------------------------------------------------------------------

			lquasi = false;

			// Loop over glass layers in the construction.
			for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
				LayNum = 1 + ( NGlass - IGlass ) * 2;
				if ( ExtShade || ExtBlind || ExtScreen ) LayNum = 2 + ( NGlass - IGlass ) * 2;
				if ( BGShade || BGBlind ) {
					if ( NGlass == 2 ) {
						if ( IGlass == 1 ) LayNum = 5;
						if ( IGlass == 2 ) LayNum = 1;
					} else { // NGlass = 3
						if ( IGlass == 1 ) LayNum = 7;
						if ( IGlass == 2 ) LayNum = 3;
						if ( IGlass == 3 ) LayNum = 1;
					}
				}
				LayPtr = Construct( ConstrNum ).LayerPoint( LayNum );

				SpecDataNum = Material( LayPtr ).GlassSpectralDataPtr;
				if ( SpecDataNum != 0 ) {

					// Get the spectral data for the transmittance, front reflectance and
					// back reflectance (all at normal incidence) for this layer.
					// In this case, "front" means incident from the inside and "back"
					// means incident from the outside.

					numptDAT = SpectralData( SpecDataNum ).NumOfWavelengths;
					numpt( IGlass ) = numptDAT;

					for ( ILam = 1; ILam <= numptDAT; ++ILam ) {
						wlt( IGlass, ILam ) = SpectralData( SpecDataNum ).WaveLength( ILam );
						t( IGlass, ILam ) = SpectralData( SpecDataNum ).Trans( ILam );
						if ( IGlass == NGlass || ( IGlass == ( NGlass - 1 ) && StormWinConst ) ) t( IGlass, ILam ) *= Material( LayPtr ).GlassTransDirtFactor;
						rff( IGlass, ILam ) = SpectralData( SpecDataNum ).ReflBack( ILam );
						rbb( IGlass, ILam ) = SpectralData( SpecDataNum ).ReflFront( ILam );
					}

				} else { // No spectral data for this layer; use spectral average values
					lquasi = true;
					numpt( IGlass ) = 2;
					t( IGlass, 1 ) = Material( LayPtr ).Trans;
					if ( IGlass == NGlass || ( IGlass == ( NGlass - 1 ) && StormWinConst ) ) t( IGlass, 1 ) *= Material( LayPtr ).GlassTransDirtFactor;
					t( IGlass, 2 ) = Material( LayPtr ).TransVis;
					if ( IGlass == NGlass || ( IGlass == ( NGlass - 1 ) && StormWinConst ) ) t( IGlass, 2 ) *= Material( LayPtr ).GlassTransDirtFactor;
					rff( IGlass, 1 ) = Material( LayPtr ).ReflectSolBeamBack;
					rbb( IGlass, 1 ) = Material( LayPtr ).ReflectSolBeamFront;
					rff( IGlass, 2 ) = Material( LayPtr ).ReflectVisBeamBack;
					rbb( IGlass, 2 ) = Material( LayPtr ).ReflectVisBeamFront;
				}
			} // End of loop over glass layers in the construction for back calculation

			// Loop over incidence angle from 0 to 90 deg in 10 deg increments.
			// Get bare glass layer properties, then glazing system properties at each incidence angle.
			// The glazing system properties include the effect of inter-reflection among glass layers,
			// but exclude the effect of a shade or blind if present in the construction.
			for ( IPhi = 1; IPhi <= 10; ++IPhi ) {
				Phi = double( IPhi - 1 ) * 10.0;
				CosPhi = std::cos( Phi * DegToRadians );
				if ( std::abs( CosPhi ) < 0.0001 ) CosPhi = 0.0;

				// For each wavelength, get glass layer properties at this angle of incidence
				// from properties at normal incidence
				for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
					for ( ILam = 1; ILam <= numpt( IGlass ); ++ILam ) {

						TransAndReflAtPhi( CosPhi, t( IGlass, ILam ), rff( IGlass, ILam ), rbb( IGlass, ILam ), tPhi( IGlass, ILam ), rfPhi( IGlass, ILam ), rbPhi( IGlass, ILam ), lSimpleGlazingSystem, SimpleGlazingSHGC, SimpleGlazingU );
					}
				}

				// For each wavelength in the solar spectrum, calculate system properties
				// stPhi, srfPhi, srbPhi and saPhi at this angle of incidence
				SystemSpectralPropertiesAtPhi( 1, NGlass, 0.0, 2.54 );

				// Get back absorptance properties of system by integrating over solar irradiance spectrum.
				// For now it is assumed that the exterior and interior irradiance spectra are the same.

				for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
					for ( j = 1; j <= nume; ++j ) {
						sabsPhi( j ) = saPhi( IGlass, j );
					}
					SolarSprectrumAverage( sabsPhi, solabsBackPhi( IGlass, IPhi ) );
				}

			} // End of loop over incidence angles for back calculation

			for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
				IGlassBack = NGlass - IGlass + 1;
				Construct( ConstrNum ).AbsDiffBack( IGlass ) = DiffuseAverage( solabsBackPhi( IGlassBack, {1,10} ) );
			}

			//-----------------------------------------------------------------------
			// Correction for effect of shade, screen or blind if present in the construction
			//-----------------------------------------------------------------------

			// For construction with shade, screen or blind, get system shading device absorptance
			// and correct the system glass layer absorptances for the effect of reflection
			// and transmission by shade, screen or blind. Get system reflectance (front and back,
			// solar and visible)

			if ( ShadeOn || BlindOn || ScreenOn ) {

				// Solar and visible properties of isolated shade or blind
				// (Note: for shades or screen we go through the following loop over slat angles only once.)

				Real64 const tsolDiff_2( pow_2( tsolDiff ) );
				Real64 const tvisDiff_2( pow_2( tvisDiff ) );
				for ( ISlatAng = 1; ISlatAng <= MaxSlatAngs; ++ISlatAng ) {

					if ( ShadeOn ) {
						ShadeAbs = Material( Construct( ConstrNum ).LayerPoint( ShadeLayNum ) ).AbsorpSolar;
						ShadeTrans = Material( Construct( ConstrNum ).LayerPoint( ShadeLayNum ) ).Trans;
						ShadeTransVis = Material( Construct( ConstrNum ).LayerPoint( ShadeLayNum ) ).TransVis;
						ShadeRefl = Material( Construct( ConstrNum ).LayerPoint( ShadeLayNum ) ).ReflectShade;
						ShadeReflVis = Material( Construct( ConstrNum ).LayerPoint( ShadeLayNum ) ).ReflectShadeVis;
						rsh = ShadeRefl;
						rshv = ShadeReflVis;
						tsh = ShadeTrans;
						tshv = ShadeTransVis;
						ash = ShadeAbs;
					} else if ( IntBlind || ExtBlind ) {
						ShadeTrans = Blind( BlNum ).SolFrontDiffDiffTrans( ISlatAng );
						ShadeTransGnd = Blind( BlNum ).SolFrontDiffDiffTransGnd( ISlatAng );
						ShadeTransSky = Blind( BlNum ).SolFrontDiffDiffTransSky( ISlatAng );
						ShadeTransVis = Blind( BlNum ).VisFrontDiffDiffTrans( ISlatAng );
						if ( IntBlind ) { // Interior blind
							ShadeAbs = Blind( BlNum ).SolFrontDiffAbs( ISlatAng );
							ShadeRefl = Blind( BlNum ).SolFrontDiffDiffRefl( ISlatAng );
							ShadeReflGnd = Blind( BlNum ).SolFrontDiffDiffReflGnd( ISlatAng );
							ShadeReflSky = Blind( BlNum ).SolFrontDiffDiffReflSky( ISlatAng );
							ShadeReflVis = Blind( BlNum ).VisFrontDiffDiffRefl( ISlatAng );
						} else { // Exterior blind
							ShadeAbs = Blind( BlNum ).SolBackDiffAbs( ISlatAng );
							ShadeRefl = Blind( BlNum ).SolBackDiffDiffRefl( ISlatAng );
							ShadeReflVis = Blind( BlNum ).VisBackDiffDiffRefl( ISlatAng );
						}
					} else if ( BGBlind ) {
						tsh = Blind( BlNum ).SolFrontDiffDiffTrans( ISlatAng );
						tshGnd = Blind( BlNum ).SolFrontDiffDiffTransGnd( ISlatAng );
						tshSky = Blind( BlNum ).SolFrontDiffDiffTransSky( ISlatAng );
						tshv = Blind( BlNum ).VisFrontDiffDiffTrans( ISlatAng );
						rfsh = Blind( BlNum ).SolFrontDiffDiffRefl( ISlatAng );
						rfshGnd = Blind( BlNum ).SolFrontDiffDiffReflGnd( ISlatAng );
						rfshSky = Blind( BlNum ).SolFrontDiffDiffReflSky( ISlatAng );
						rfshv = Blind( BlNum ).VisFrontDiffDiffRefl( ISlatAng );
						rbsh = Blind( BlNum ).SolBackDiffDiffRefl( ISlatAng );
						rbshv = Blind( BlNum ).VisBackDiffDiffRefl( ISlatAng );
						afsh = Blind( BlNum ).SolFrontDiffAbs( ISlatAng );
						afshGnd = Blind( BlNum ).SolFrontDiffAbsGnd( ISlatAng );
						afshSky = Blind( BlNum ).SolFrontDiffAbsSky( ISlatAng );
						absh = Blind( BlNum ).SolBackDiffAbs( ISlatAng );
					} else if ( ScreenOn && ScNum > 0 ) {
						//       diffuse screen properties are calculated during initialization (quarter-hemispherical integration of beam properties)
						ShadeAbs = SurfaceScreens( ScNum ).DifScreenAbsorp;
						ShadeTrans = SurfaceScreens( ScNum ).DifDifTrans;
						ShadeTransVis = SurfaceScreens( ScNum ).DifDifTransVis;
						ShadeRefl = SurfaceScreens( ScNum ).DifReflect;
						ShadeReflVis = SurfaceScreens( ScNum ).DifReflectVis;
						rsh = ShadeRefl;
						rshv = ShadeReflVis;
						tsh = ShadeTrans;
						tshv = ShadeTransVis;
						ash = ShadeAbs;
					}

					// Correction factors for inter-reflections between glass and shading device

					if ( ExtShade || ExtBlind || ExtScreen ) {
						ShadeReflFac = 1.0 / ( 1.0 - ShadeRefl * Construct( ConstrNum ).ReflectSolDiffFront );
						ShadeReflFacVis = 1.0 / ( 1.0 - ShadeReflVis * Construct( ConstrNum ).ReflectVisDiffFront );
					} else if ( IntShade || IntBlind ) {
						ShadeReflFac = 1.0 / ( 1.0 - ShadeRefl * Construct( ConstrNum ).ReflectSolDiffBack );
						ShadeReflFacVis = 1.0 / ( 1.0 - ShadeReflVis * Construct( ConstrNum ).ReflectVisDiffBack );
					}

					if ( ExtShade || ExtBlind || ExtScreen ) { // Exterior shade or blind

						// Front incident solar, beam, exterior shade, screen or blind

						if ( ExtShade ) {
							for ( IPhi = 1; IPhi <= 10; ++IPhi ) {
								for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
									solabsPhi( IGlass, IPhi ) = ShadeTrans * solabsDiff( IGlass ) * ShadeReflFac;
								}
								tsolPhi( IPhi ) = ShadeTrans * ShadeReflFac * tsolDiff;
								tvisPhi( IPhi ) = ShadeTransVis * ShadeReflFacVis * tvisDiff;
								solabsShadePhi( IPhi ) = ShadeAbs * ( 1.0 + ShadeTrans * ShadeReflFac * Construct( ConstrNum ).ReflectSolDiffFront );
							}
						}

						// Front incident solar, diffuse, exterior shade/screen/blind

						for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
							if ( ExtBlind ) {
								Construct( ConstrNum ).BlAbsDiff( ISlatAng, IGlass ) = ShadeTrans * ShadeReflFac * solabsDiff( IGlass );
								Construct( ConstrNum ).BlAbsDiffGnd( ISlatAng, IGlass ) = ShadeTransGnd * ShadeReflFac * solabsDiff( IGlass );
								Construct( ConstrNum ).BlAbsDiffSky( ISlatAng, IGlass ) = ShadeTransSky * ShadeReflFac * solabsDiff( IGlass );
							}
							if ( ExtShade || ExtScreen ) Construct( ConstrNum ).AbsDiff( IGlass ) = ShadeTrans * ShadeReflFac * solabsDiff( IGlass );
						}
						if ( ExtBlind ) {
							Construct( ConstrNum ).AbsDiffBlind( ISlatAng ) = Blind( BlNum ).SolFrontDiffAbs( ISlatAng ) + ShadeTrans * ShadeReflFac * Construct( ConstrNum ).ReflectSolDiffFront * ShadeAbs;
							Construct( ConstrNum ).AbsDiffBlindGnd( ISlatAng ) = Blind( BlNum ).SolFrontDiffAbsGnd( ISlatAng ) + ShadeTransGnd * ShadeReflFac * Construct( ConstrNum ).ReflectSolDiffFront * ShadeAbs;
							Construct( ConstrNum ).AbsDiffBlindSky( ISlatAng ) = Blind( BlNum ).SolFrontDiffAbsSky( ISlatAng ) + ShadeTransSky * ShadeReflFac * Construct( ConstrNum ).ReflectSolDiffFront * ShadeAbs;
							Construct( ConstrNum ).BlTransDiff( ISlatAng ) = tsolDiff * ShadeReflFac * ShadeTrans;
							Construct( ConstrNum ).BlTransDiffGnd( ISlatAng ) = tsolDiff * ShadeReflFac * ShadeTransGnd;
							Construct( ConstrNum ).BlTransDiffSky( ISlatAng ) = tsolDiff * ShadeReflFac * ShadeTransSky;
							Construct( ConstrNum ).BlTransDiffVis( ISlatAng ) = tvisDiff * ShadeReflFacVis * ShadeTransVis;
							Construct( ConstrNum ).BlReflectSolDiffFront( ISlatAng ) = ShadeRefl + pow_2( ShadeTrans ) * Construct( ConstrNum ).ReflectSolDiffFront * ShadeReflFac;
							Construct( ConstrNum ).BlReflectVisDiffFront( ISlatAng ) = ShadeReflVis + pow_2( ShadeTransVis ) * Construct( ConstrNum ).ReflectVisDiffFront * ShadeReflFacVis;
						}
						if ( ExtShade || ExtScreen ) {
							Construct( ConstrNum ).AbsDiffShade = ShadeAbs * ( 1.0 + ShadeTrans * ShadeReflFac * Construct( ConstrNum ).ReflectSolDiffFront );
							Construct( ConstrNum ).TransDiff = tsolDiff * ShadeReflFac * ShadeTrans;
							Construct( ConstrNum ).TransDiffVis = tvisDiff * ShadeReflFacVis * ShadeTransVis;
							Construct( ConstrNum ).ReflectSolDiffFront = ShadeRefl + pow_2( ShadeTrans ) * Construct( ConstrNum ).ReflectSolDiffFront * ShadeReflFac;
							Construct( ConstrNum ).ReflectVisDiffFront = ShadeReflVis + pow_2( ShadeTransVis ) * Construct( ConstrNum ).ReflectVisDiffFront * ShadeReflFacVis;
						}

						// Back incident solar, diffuse, exterior shade/blind

						if ( ExtBlind ) {
							for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
								Construct( ConstrNum ).BlAbsDiffBack( ISlatAng, IGlass ) = Construct( ConstrNum ).AbsDiffBack( IGlass ) + tsolDiff * ShadeRefl * ShadeReflFac * solabsDiff( IGlass );
							}
							Construct( ConstrNum ).AbsDiffBackBlind( ISlatAng ) = tsolDiff * ShadeReflFac * ShadeAbs;
							Construct( ConstrNum ).BlReflectSolDiffBack( ISlatAng ) = Construct( ConstrNum ).ReflectSolDiffBack + tsolDiff_2 * ShadeRefl * ShadeReflFac;
							Construct( ConstrNum ).BlReflectVisDiffBack( ISlatAng ) = Construct( ConstrNum ).ReflectVisDiffBack + tvisDiff_2 * ShadeReflVis * ShadeReflFacVis;
						}
						if ( ExtShade || ExtScreen ) {
							for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
								Construct( ConstrNum ).AbsDiffBack( IGlass ) += tsolDiff * ShadeRefl * ShadeReflFac * solabsDiff( IGlass );
							}
							Construct( ConstrNum ).AbsDiffBackShade = tsolDiff * ShadeReflFac * ShadeAbs;
							Construct( ConstrNum ).ReflectSolDiffBack += tsolDiff_2 * ShadeRefl * ShadeReflFac;
							Construct( ConstrNum ).ReflectVisDiffBack += tvisDiff_2 * ShadeReflVis * ShadeReflFacVis;
						}

					} // End check if exterior shade, screen or blind

					if ( IntShade || IntBlind ) { // Interior shade or blind

						// Front incident solar, beam, interior shade

						if ( IntShade ) {
							for ( IPhi = 1; IPhi <= 10; ++IPhi ) {
								for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
									solabsPhi( IGlass, IPhi ) += tsolPhi( IPhi ) * ShadeRefl * ShadeReflFac * Construct( ConstrNum ).AbsDiffBack( IGlass );
								}
								solabsShadePhi( IPhi ) = tsolPhi( IPhi ) * ShadeReflFac * ShadeAbs;
								tsolPhi( IPhi ) *= ShadeReflFac * ShadeTrans;
								tvisPhi( IPhi ) *= ShadeReflFacVis * ShadeTransVis;
							}
						} // End of check if interior shade

						// Front incident solar, diffuse, interior blind

						if ( IntBlind ) {
							for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
								Construct( ConstrNum ).BlAbsDiff( ISlatAng, IGlass ) = Construct( ConstrNum ).AbsDiff( IGlass ) + tsolDiff * ShadeRefl * ShadeReflFac * Construct( ConstrNum ).AbsDiffBack( IGlass );
								Construct( ConstrNum ).BlAbsDiffGnd( ISlatAng, IGlass ) = Construct( ConstrNum ).AbsDiff( IGlass ) + tsolDiff * ShadeReflGnd * ShadeReflFac * Construct( ConstrNum ).AbsDiffBack( IGlass );
								Construct( ConstrNum ).BlAbsDiffSky( ISlatAng, IGlass ) = Construct( ConstrNum ).AbsDiff( IGlass ) + tsolDiff * ShadeReflSky * ShadeReflFac * Construct( ConstrNum ).AbsDiffBack( IGlass );
							}

							Construct( ConstrNum ).AbsDiffBlind( ISlatAng ) = tsolDiff * ShadeReflFac * ShadeAbs;
							Construct( ConstrNum ).AbsDiffBlindGnd( ISlatAng ) = tsolDiff * ShadeReflFac * Blind( BlNum ).SolFrontDiffAbsGnd( ISlatAng );
							Construct( ConstrNum ).AbsDiffBlindSky( ISlatAng ) = tsolDiff * ShadeReflFac * Blind( BlNum ).SolFrontDiffAbsSky( ISlatAng );
							Construct( ConstrNum ).BlTransDiff( ISlatAng ) = tsolDiff * ShadeReflFac * ShadeTrans;
							Construct( ConstrNum ).BlTransDiffGnd( ISlatAng ) = tsolDiff * ShadeReflFac * ShadeTransGnd;
							Construct( ConstrNum ).BlTransDiffSky( ISlatAng ) = tsolDiff * ShadeReflFac * ShadeTransSky;
							Construct( ConstrNum ).BlTransDiffVis( ISlatAng ) = tvisDiff * ShadeReflFacVis * ShadeTransVis;
							Construct( ConstrNum ).BlReflectSolDiffFront( ISlatAng ) = Construct( ConstrNum ).ReflectSolDiffFront + tsolDiff_2 * ShadeRefl * ShadeReflFac;
							Construct( ConstrNum ).BlReflectVisDiffFront( ISlatAng ) = Construct( ConstrNum ).ReflectVisDiffFront + tvisDiff_2 * ShadeReflVis * ShadeReflFacVis;

							// Back incident solar, diffuse, interior blind

							for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
								Construct( ConstrNum ).BlAbsDiffBack( ISlatAng, IGlass ) = Construct( ConstrNum ).AbsDiffBack( IGlass ) * ShadeTrans * ShadeReflFac;
							}

							Construct( ConstrNum ).AbsDiffBackBlind( ISlatAng ) = Blind( BlNum ).SolBackDiffAbs( ISlatAng ) + ShadeTrans * ShadeReflFac * Construct( ConstrNum ).ReflectSolDiffBack * ShadeAbs;
							Construct( ConstrNum ).BlReflectSolDiffBack( ISlatAng ) = Blind( BlNum ).SolBackDiffDiffRefl( ISlatAng ) + pow_2( ShadeTrans ) * Construct( ConstrNum ).ReflectSolDiffBack * ShadeReflFac;
							Construct( ConstrNum ).BlReflectVisDiffBack( ISlatAng ) = Blind( BlNum ).VisBackDiffDiffRefl( ISlatAng ) + pow_2( ShadeTransVis ) * Construct( ConstrNum ).ReflectVisDiffBack * ShadeReflFacVis;
						} // End of check if interior blind

						// Front incident solar, diffuse, interior shade

						if ( IntShade ) {
							for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
								Construct( ConstrNum ).AbsDiff( IGlass ) += tsolDiff * ShadeRefl * ShadeReflFac * solabsDiff( IGlass );
							}

							Construct( ConstrNum ).AbsDiffShade = tsolDiff * ShadeReflFac * ShadeAbs;
							Construct( ConstrNum ).TransDiff = tsolDiff * ShadeReflFac * ShadeTrans;
							Construct( ConstrNum ).TransDiffVis = tvisDiff * ShadeReflFacVis * ShadeTransVis;
							Construct( ConstrNum ).ReflectSolDiffFront += tsolDiff_2 * ShadeRefl * ShadeReflFac;
							Construct( ConstrNum ).ReflectVisDiffFront += tvisDiff_2 * ShadeReflVis * ShadeReflFacVis;

							// Back incident solar, diffuse, interior shade

							for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
								Construct( ConstrNum ).AbsDiffBack( IGlass ) *= ShadeTrans * ShadeReflFac;
							}

							Construct( ConstrNum ).AbsDiffBackShade = ShadeAbs * ( 1 + ShadeTrans * ShadeReflFac * Construct( ConstrNum ).ReflectSolDiffBack );
							Construct( ConstrNum ).ReflectSolDiffBack = ShadeRefl + pow_2( ShadeTrans ) * Construct( ConstrNum ).ReflectSolDiffBack * ShadeReflFac;
							Construct( ConstrNum ).ReflectVisDiffBack = ShadeReflVis + pow_2( ShadeTransVis ) * Construct( ConstrNum ).ReflectVisDiffBack * ShadeReflFacVis;
						} // End of check if interior shade

					} // End check if interior shade or blind

					if ( BGShade || BGBlind ) { // Between-glass shade/blind; assumed to be between glass #2 and glass #3

						tsh2 = pow_2( tsh );
						tshv2 = pow_2( tshv );
						td1 = Construct( ConstrNum ).tBareSolDiff( 1 );
						td2 = Construct( ConstrNum ).tBareSolDiff( 2 );
						td1v = Construct( ConstrNum ).tBareVisDiff( 1 );
						td2v = Construct( ConstrNum ).tBareVisDiff( 2 );
						afd1 = Construct( ConstrNum ).afBareSolDiff( 1 );
						afd2 = Construct( ConstrNum ).afBareSolDiff( 2 );
						abd1 = Construct( ConstrNum ).abBareSolDiff( 1 );
						abd2 = Construct( ConstrNum ).abBareSolDiff( 2 );
						rb1 = Construct( ConstrNum ).rbBareSolDiff( 1 );
						rb2 = Construct( ConstrNum ).rbBareSolDiff( 2 );
						rb1v = Construct( ConstrNum ).rbBareVisDiff( 1 );
						rb2v = Construct( ConstrNum ).rbBareVisDiff( 2 );
						rf1 = Construct( ConstrNum ).rfBareSolDiff( 1 );
						rf2 = Construct( ConstrNum ).rfBareSolDiff( 2 );
						rf1v = Construct( ConstrNum ).rfBareVisDiff( 1 );
						rf2v = Construct( ConstrNum ).rfBareVisDiff( 2 );

						if ( BGShade ) {
							if ( NGlass == 2 ) {

								// Front incident solar, beam, between-glass shade, NGlass = 2

								for ( IPhi = 1; IPhi <= 10; ++IPhi ) {
									t1 = tBareSolPhi( 1, IPhi );
									t1v = tBareVisPhi( 1, IPhi );
									af1 = afBareSolPhi( 1, IPhi );
									ab1 = abBareSolPhi( 1, IPhi );
									tsolPhi( IPhi ) = t1 * ( tsh + rsh * rb1 * tsh + tsh * rf2 * rsh ) * td2;
									tvisPhi( IPhi ) = t1v * ( tshv + rshv * rb1v * tshv + tshv * rf2v * rshv ) * td2v;
									solabsShadePhi( IPhi ) = t1 * ( ash + rsh * rb1 + tsh * rf2 ) * ash;
									solabsPhi( 1, IPhi ) = af1 + t1 * ( rsh + rsh * rb1 * rsh + tsh * rf2 * tsh ) * abd1;
									solabsPhi( 2, IPhi ) = t1 * ( tsh + rsh * rb1 * tsh + tsh * rf2 * rsh ) * afd2;
								} // End of loop over incidence angles

								// Front incident solar, diffuse, between-glass shade, NGlass = 2

								Construct( ConstrNum ).TransDiff = td1 * ( tsh + rsh * rb1 * tsh + tsh * rb2 * rsh ) * td2;
								Construct( ConstrNum ).TransDiffVis = td1v * ( tshv + rshv * rb1v * tshv + tshv * rb2v * rshv ) * td2v;
								Construct( ConstrNum ).AbsDiffShade = td1 * ( ash + rsh * rb1 * ash + tsh * rf2 * ash );
								Construct( ConstrNum ).AbsDiff( 1 ) = afd1 + td1 * ( rsh + tsh * rb2 * tsh ) * abd1;
								Construct( ConstrNum ).AbsDiff( 2 ) = td1 * ( tsh + rsh * rb1 * tsh + tsh * rf2 * rsh ) * afd2;
								Construct( ConstrNum ).ReflectSolDiffFront = rf1 + td1 * ( rsh + rsh * rb1 * rsh + tsh * rf2 * tsh ) * td1;
								Construct( ConstrNum ).ReflectVisDiffFront = rf1v + td1v * ( rshv + rshv * rb1v * rshv + tshv * rf2v * tshv ) * td1v;

								// Back incident solar, diffuse, between-glass shade, NGlass = 2

								Construct( ConstrNum ).AbsDiffBackShade = td2 * ( ash + rsh * rf2 * ash + tsh * rb1 * ash );
								Construct( ConstrNum ).AbsDiffBack( 1 ) = td2 * ( tsh + rsh * rf2 * tsh + tsh * rb1 * rsh ) * abd1;
								Construct( ConstrNum ).AbsDiffBack( 2 ) = abd2 + td2 * ( rsh + rsh * rf2 * rsh + tsh * rb1 * tsh ) * afd2;
								Construct( ConstrNum ).ReflectSolDiffBack = rb2 + td2 * ( rsh + rsh * rf2 * rsh + tsh * rb1 * tsh ) * td2;
								Construct( ConstrNum ).ReflectVisDiffBack = rb2v + td2v * ( rshv + rshv * rf2v * rshv + tshv * rb1v * tshv ) * td2v;

							} // End of check if NGlass = 2

							if ( NGlass == 3 ) {

								td3 = Construct( ConstrNum ).tBareSolDiff( 3 );
								td3v = Construct( ConstrNum ).tBareVisDiff( 3 );
								afd3 = Construct( ConstrNum ).afBareSolDiff( 3 );
								abd3 = Construct( ConstrNum ).abBareSolDiff( 3 );
								rb3 = Construct( ConstrNum ).rbBareSolDiff( 3 );
								rb3v = Construct( ConstrNum ).rbBareVisDiff( 3 );
								rf3 = Construct( ConstrNum ).rfBareSolDiff( 3 );
								rf3v = Construct( ConstrNum ).rfBareVisDiff( 3 );

								// Front incident solar, beam, between-glass shade, NGlass = 3

								for ( IPhi = 1; IPhi <= 10; ++IPhi ) {
									t1 = tBareSolPhi( 1, IPhi );
									t1v = tBareVisPhi( 1, IPhi );
									t2 = tBareSolPhi( 2, IPhi );
									t2v = tBareVisPhi( 2, IPhi );
									af1 = afBareSolPhi( 1, IPhi );
									af2 = afBareSolPhi( 2, IPhi );
									ab1 = abBareSolPhi( 1, IPhi );
									ab2 = abBareSolPhi( 2, IPhi );
									rbmf2 = max( 0.0, 1.0 - ( t2 + af2 ) );

									tsolPhi( IPhi ) = t1 * t2 * ( tsh + tsh * rf3 * rsh + rsh * td2 * rb1 * td2 * tsh + rsh * rb2 * tsh ) * td3;
									tvisPhi( IPhi ) = t1v * t2v * ( tshv + tshv * rf3v * rshv + rshv * td2v * rb1v * td2v * tshv + rshv * rb2v * tshv ) * td3v;
									solabsShadePhi( IPhi ) = t1 * t2 * ( 1 + rsh * td2 * rb1 * td2 + rsh * rb2 ) * ash;
									solabsPhi( 1, IPhi ) = af1 + rbmf2 * ab1 + t1 * t2 * rsh * ( 1 + rf3 * tsh + rb2 * rsh + td2 * rb1 * td2 * rsh ) * td2 * abd1;
									solabsPhi( 2, IPhi ) = t1 * af2 + t1 * t2 * ( ( rsh + tsh * rf3 * tsh + rsh * rb2 * rsh ) * abd2 + rsh * td2 * rb1 * afd2 );
									solabsPhi( 3, IPhi ) = t1 * t2 * ( tsh + rsh * ( rb2 * tsh + td2 * rb2 * td2 * tsh + rf3 * rsh ) ) * afd3;
								} // End of loop over incidence angle

								// Front incident solar, diffuse, between-glass shade, NGlass = 3

								Construct( ConstrNum ).TransDiff = td1 * td2 * ( tsh + rsh * td2 * rb1 * td2 * tsh + rsh * rb2 * tsh + tsh * rf3 * rsh ) * td3;
								Construct( ConstrNum ).TransDiffVis = td1v * td2v * ( tshv + rshv * td2v * rb1v * td2v * tshv + rshv * rb2v * tshv + tshv * rf3v * rshv ) * td3v;
								Construct( ConstrNum ).AbsDiffShade = td1 * td2 * ( ash * ( 1 + rsh * td2 * rb1 * td2 + rsh * rb2 * ash ) + tsh * rf3 * ash );
								Construct( ConstrNum ).AbsDiff( 1 ) = afd1 + td1 * ( rf2 + td2 * ( rsh + rsh * rb2 * rsh + tsh * rf3 * tsh + rsh * td2 * rb1 * td2 * rsh ) * td2 ) * abd1;
								Construct( ConstrNum ).AbsDiff( 2 ) = td1 * ( afd2 + td2 * ( rsh + rsh * rb2 * rsh + tsh * rf3 * tsh ) * abd2 );
								Construct( ConstrNum ).AbsDiff( 3 ) = td1 * td2 * ( tsh + rsh * rb2 * tsh + rsh * td2 * rb1 * td2 * tsh + tsh * rf3 * rsh ) * afd3;
								Construct( ConstrNum ).ReflectSolDiffFront = rf1 + td1 * rf2 * td1 + td1 * td2 * ( rsh + tsh * rf3 * tsh + rsh * rb2 * rsh + rsh * td2 * rb1 * td2 * rsh ) * td2 * td1;
								Construct( ConstrNum ).ReflectVisDiffFront = rf1v + td1v * rf2v * td1v + td1v * td2v * ( rshv + tshv * rf3v * tshv + rshv * rb2v * rshv + rshv * td2v * rb1v * td2v * rshv ) * td2v * td1v;

								// Back incident solar, diffuse, between-glass shade, NGlass = 3

								Construct( ConstrNum ).AbsDiffBackShade = td3 * ( ( 1 + rsh * rf3 ) * ash + ( tsh * td2 * rb1 * td2 + tsh * rb2 ) * ash );
								Construct( ConstrNum ).AbsDiffBack( 1 ) = td3 * ( tsh + rsh * rf3 * tsh + tsh * rb2 * rsh + tsh * td2 * rb1 * td2 * rsh ) * td2 * abd1;
								Construct( ConstrNum ).AbsDiffBack( 2 ) = td3 * ( ( tsh + rsh * rf3 * tsh ) * abd2 + ( tsh * td2 * rb1 * td2 + tsh * rb2 ) * afd2 );
								Construct( ConstrNum ).AbsDiffBack( 3 ) = abd3 + td3 * ( rsh + tsh * rb2 * tsh + tsh * td2 * rb1 * td2 * tsh ) * afd3;
								Construct( ConstrNum ).ReflectSolDiffBack = rb3 + td3 * ( rsh + rsh * rf3 * rsh + tsh * rb2 * tsh + tsh * td2 * rb1 * td2 * tsh ) * td3;
								Construct( ConstrNum ).ReflectVisDiffBack = rb3v + td3v * ( rshv + rshv * rf3 * rshv + tshv * rb2v * tshv + tshv * td2v * rb1v * td2v * tshv ) * td3v;

							} // End of check if NGlass = 3

						} // End of check if between-glass shade

						if ( BGBlind ) {

							if ( NGlass == 2 ) {

								// Front incident solar, diffuse, between-glass blind, NGlass = 2

								Construct( ConstrNum ).BlAbsDiff( ISlatAng, 1 ) = afd1 + td1 * ( rfsh + rfsh * rb1 * rfsh + tsh * rb2 * tsh ) * abd1;
								Construct( ConstrNum ).BlAbsDiffGnd( ISlatAng, 1 ) = afd1 + td1 * ( rfshGnd + rfshGnd * rb1 * rfshGnd + tshGnd * rb2 * tsh ) * abd1;
								Construct( ConstrNum ).BlAbsDiffSky( ISlatAng, 1 ) = afd1 + td1 * ( rfshSky + rfshSky * rb1 * rfshSky + tshSky * rb2 * tsh ) * abd1;
								Construct( ConstrNum ).BlAbsDiff( ISlatAng, 2 ) = td1 * ( tsh + rfsh * rb1 * tsh + tsh * rf2 * rbsh ) * afd2;
								Construct( ConstrNum ).BlAbsDiffGnd( ISlatAng, 2 ) = td1 * ( tshGnd + rfshGnd * rb1 * tsh + tshGnd * rf2 * rbsh ) * afd2;
								Construct( ConstrNum ).BlAbsDiffSky( ISlatAng, 2 ) = td1 * ( tshSky + rfshSky * rb1 * tsh + tshSky * rf2 * rbsh ) * afd2;
								Construct( ConstrNum ).AbsDiffBlind( ISlatAng ) = td1 * ( afsh + rfsh * rb1 * afsh + tsh * rf2 * absh );
								Construct( ConstrNum ).AbsDiffBlindGnd( ISlatAng ) = td1 * ( afshGnd + rfsh * rb1 * afsh + tshGnd * rf2 * absh );
								Construct( ConstrNum ).AbsDiffBlindSky( ISlatAng ) = td1 * ( afshSky + rfsh * rb1 * afsh + tshSky * rf2 * absh );
								Construct( ConstrNum ).BlTransDiff( ISlatAng ) = td1 * ( tsh + rfsh * rb1 * tsh + tsh * rb2 * rbsh ) * td2;
								Construct( ConstrNum ).BlTransDiffGnd( ISlatAng ) = td1 * ( tshGnd + rfsh * rb1 * tshGnd + tshGnd * rb2 * rbsh ) * td2;
								Construct( ConstrNum ).BlTransDiffSky( ISlatAng ) = td1 * ( tshSky + rfsh * rb1 * tshSky + tshSky * rb2 * rbsh ) * td2;
								Construct( ConstrNum ).BlTransDiffVis( ISlatAng ) = td1v * ( tshv + rfshv * rb1v * tshv + tshv * rb2v * rbshv ) * td2v;
								Construct( ConstrNum ).BlReflectSolDiffFront( ISlatAng ) = rf1 + td1 * ( rfsh + rfsh * rb1 * rfsh + tsh * rf2 * tsh ) * td1;
								Construct( ConstrNum ).BlReflectVisDiffFront( ISlatAng ) = rf1v + td1v * ( rfshv + rfshv * rb1v * rfshv + tshv * rf2v * tshv ) * td1v;

								// Back incident solar, diffuse, between-glass blind, NGlass = 2

								Construct( ConstrNum ).BlAbsDiffBack( ISlatAng, 1 ) = td2 * ( tsh + rbsh * rf2 * tsh + tsh * rb1 * rfsh ) * abd1;
								Construct( ConstrNum ).BlAbsDiffBack( ISlatAng, 2 ) = abd2 + td2 * ( rbsh + rbsh * rf2 * rbsh + tsh * rb1 * tsh ) * afd2;
								Construct( ConstrNum ).AbsDiffBackBlind( ISlatAng ) = td2 * ( absh + rbsh * rf2 * absh + tsh * rb1 * afsh );
								Construct( ConstrNum ).BlReflectSolDiffBack( ISlatAng ) = rb2 + td2 * ( rbsh + rbsh * rf2 * rbsh + tsh * rb1 * tsh ) * td2;
								Construct( ConstrNum ).BlReflectVisDiffBack( ISlatAng ) = rb2v + td2v * ( rbshv + rbshv * rf2v * rbshv + tshv * rb1v * tshv ) * td2v;

							} // End of check if NGlass = 2

							if ( NGlass == 3 ) {

								td3 = Construct( ConstrNum ).tBareSolDiff( 3 );
								td3v = Construct( ConstrNum ).tBareVisDiff( 3 );
								afd3 = Construct( ConstrNum ).afBareSolDiff( 3 );
								abd3 = Construct( ConstrNum ).abBareSolDiff( 3 );
								rb3 = Construct( ConstrNum ).rbBareSolDiff( 3 );
								rb3v = Construct( ConstrNum ).rbBareVisDiff( 3 );
								rf3 = Construct( ConstrNum ).rfBareSolDiff( 3 );
								rf3v = Construct( ConstrNum ).rfBareVisDiff( 3 );

								// Front incident solar, diffuse, between-glass blind, NGlass = 3

								Construct( ConstrNum ).BlAbsDiff( ISlatAng, 1 ) = afd1 + td1 * ( rf2 + td2 * ( rfsh + rfsh * rb2 * rfsh + tsh * rf3 * tsh + rfsh * td2 * rb1 * td2 * rfsh ) * td2 ) * abd1;
								Construct( ConstrNum ).BlAbsDiffGnd( ISlatAng, 1 ) = afd1 + td1 * ( rf2 + td2 * ( rfshGnd + rfshGnd * rb2 * rfsh + tshGnd * rf3 * tsh + rfshGnd * td2 * rb1 * td2 * rfsh ) * td2 ) * abd1;
								Construct( ConstrNum ).BlAbsDiffSky( ISlatAng, 1 ) = afd1 + td1 * ( rf2 + td2 * ( rfshSky + rfshSky * rb2 * rfsh + tshSky * rf3 * tsh + rfshSky * td2 * rb1 * td2 * rfsh ) * td2 ) * abd1;
								Construct( ConstrNum ).BlAbsDiff( ISlatAng, 2 ) = td1 * ( afd2 + td2 * ( rfsh + rfsh * rb2 * rfsh + tsh * rf3 * tsh ) * abd2 );
								Construct( ConstrNum ).BlAbsDiffGnd( ISlatAng, 2 ) = td1 * ( afd2 + td2 * ( rfshGnd + rfshGnd * rb2 * rfsh + tshGnd * rf3 * tsh ) * abd2 );
								Construct( ConstrNum ).BlAbsDiffSky( ISlatAng, 2 ) = td1 * ( afd2 + td2 * ( rfshSky + rfshSky * rb2 * rfsh + tshSky * rf3 * tsh ) * abd2 );
								Construct( ConstrNum ).BlAbsDiff( ISlatAng, 3 ) = td1 * td2 * ( tsh + rfsh * rb2 * tsh + rfsh * td2 * rb1 * td2 * tsh + tsh * rf3 * rbsh ) * afd3;
								Construct( ConstrNum ).BlAbsDiffGnd( ISlatAng, 3 ) = td1 * td2 * ( tshGnd + rfshGnd * rb2 * tsh + rfshGnd * td2 * rb1 * td2 * tsh + tshGnd * rf3 * rbsh ) * afd3;
								Construct( ConstrNum ).BlAbsDiffSky( ISlatAng, 3 ) = td1 * td2 * ( tshSky + rfshSky * rb2 * tsh + rfshSky * td2 * rb1 * td2 * tsh + tshSky * rf3 * rbsh ) * afd3;
								Construct( ConstrNum ).AbsDiffBlind( ISlatAng ) = td1 * td2 * ( afsh * ( 1 + rfsh * td2 * rb1 * td2 ) + rfsh * rb2 * afsh + tsh * rf3 * absh );
								Construct( ConstrNum ).AbsDiffBlindGnd( ISlatAng ) = td1 * td2 * ( afshGnd + afsh * rfsh * ( td2 * rb1 * td2 + rb2 ) + tshGnd * rf3 * absh );
								Construct( ConstrNum ).AbsDiffBlindSky( ISlatAng ) = td1 * td2 * ( afshSky + afsh * rfsh * ( td2 * rb1 * td2 + rb2 ) + tshSky * rf3 * absh );
								Construct( ConstrNum ).BlTransDiff( ISlatAng ) = td1 * td2 * ( tsh + rfsh * td2 * rb1 * td2 * tsh + rfsh * rb2 * tsh + tsh * rf3 * rbsh ) * td3;
								Construct( ConstrNum ).BlTransDiffGnd( ISlatAng ) = td1 * td2 * ( tshGnd + rfsh * td2 * rb1 * td2 * tshGnd + rfsh * rb2 * tshGnd + tshGnd * rf3 * rbsh ) * td3;
								Construct( ConstrNum ).BlTransDiffSky( ISlatAng ) = td1 * td2 * ( tshSky + rfsh * td2 * rb1 * td2 * tshSky + rfsh * rb2 * tshSky + tshSky * rf3 * rbsh ) * td3;
								Construct( ConstrNum ).BlTransDiffVis( ISlatAng ) = td1v * td2v * ( tshv + rfshv * td2v * rb1v * td2v * tshv + rfshv * rb2v * tshv + tshv * rf3v * rbshv ) * td3v;
								Construct( ConstrNum ).BlReflectSolDiffFront( ISlatAng ) = rf1 + td1 * rf2 * td1 + td1 * td2 * ( rfsh + tsh * rf3 * tsh + rfsh * rb2 * rfsh + rfsh * td2 * rb1 * td2 * rfsh ) * td2 * td1;
								Construct( ConstrNum ).BlReflectVisDiffFront( ISlatAng ) = rf1v + td1v * rf2v * td1v + td1v * td2v * ( rfshv + tshv * rf3v * tshv + rfshv * rb2v * rfshv + rfshv * td2v * rb1v * td2v * rfshv ) * td2v * td1v;

								// Back incident solar, diffuse, between-glass blind, NGlass = 3

								Construct( ConstrNum ).BlAbsDiffBack( ISlatAng, 1 ) = td3 * ( tsh + rbsh * rf3 * tsh + tsh * rb2 * rfsh + tsh * td2 * rb1 * td2 * rfsh ) * td2 * abd1;
								Construct( ConstrNum ).BlAbsDiffBack( ISlatAng, 2 ) = td3 * ( ( tsh + rbsh * rf3 * tsh ) * abd2 + ( tsh * td2 * rb1 * td2 + tsh * rb2 ) * afd2 );
								Construct( ConstrNum ).BlAbsDiffBack( ISlatAng, 3 ) = abd3 + td3 * ( rbsh + tsh * rb2 * tsh + tsh * td2 * rb1 * td2 * tsh ) * afd3;
								Construct( ConstrNum ).AbsDiffBackBlind( ISlatAng ) = td3 * ( ( 1 + rbsh * rf3 ) * absh + ( tsh * td2 * rb1 * td2 + tsh * rb2 ) * afsh );
								Construct( ConstrNum ).BlReflectSolDiffBack( ISlatAng ) = rb3 + td3 * ( rbsh + rbsh * rf3 * rbsh + tsh * rb2 * tsh + tsh * td2 * rb1 * td2 * tsh ) * td3;
								Construct( ConstrNum ).BlReflectVisDiffBack( ISlatAng ) = rb3v + td3v * ( rbshv + rbshv * rf3v * rbshv + tshv * rb2v * tshv + tshv * td2v * rb1v * td2v * tshv ) * td3v;

							} // End of check if NGlass = 3

						} // End of check if between-glass blind

					} // End of check if between-glass shade or blind

					// Continue loop over slat angles only for blinds with variable slat angle
					if ( ShadeOn || ScreenOn ) break;
					if ( BlindOn ) {
						if ( Blind( BlNum ).SlatAngleType == FixedSlats ) break;
					}
				} // End of slat angle loop
			} // End of check if construction has a shade or blind

			// Curve fits to get solar transmittance, reflectance, layer absorptance and
			// visible transmittance as polynomials in cosine of incidence angle

			if ( ! BlindOn && ! ScreenOn ) { // Bare glass or shade on
				W5LsqFit( CosPhiIndepVar, tsolPhi, 6, 1, 10, Construct( ConstrNum ).TransSolBeamCoef );
				W5LsqFit( CosPhiIndepVar, rfsolPhi, 6, 1, 10, Construct( ConstrNum ).ReflSolBeamFrontCoef );
				W5LsqFit( CosPhiIndepVar, rbsolPhi, 6, 1, 10, Construct( ConstrNum ).ReflSolBeamBackCoef( {1,6} ) );
				W5LsqFit( CosPhiIndepVar, tvisPhi, 6, 1, 10, Construct( ConstrNum ).TransVisBeamCoef );
				for ( IGlass = 1; IGlass <= NGlass; ++IGlass ) {
					// Front absorptance coefficients for glass layers
					DepVarCurveFit( {1,10} ) = solabsPhi( IGlass, {1,10} );
					W5LsqFit( CosPhiIndepVar, DepVarCurveFit, 6, 1, 10, CoeffsCurveFit );
					Construct( ConstrNum ).AbsBeamCoef( {1,6}, IGlass ) = CoeffsCurveFit;
					// Back absorptance coefficients for glass layers
					IGlassBack = NGlass - IGlass + 1;
					DepVarCurveFit( {1,10} ) = solabsBackPhi( IGlassBack, {1,10} );
					W5LsqFit( CosPhiIndepVar, DepVarCurveFit, 6, 1, 10, CoeffsCurveFit );
					Construct( ConstrNum ).AbsBeamBackCoef( {1,6}, IGlass ) = CoeffsCurveFit;
				}

				// To check goodness of fit //Tuned
				auto const & solBeamCoef( Construct( ConstrNum ).TransSolBeamCoef );
				auto const & visBeamCoef( Construct( ConstrNum ).TransVisBeamCoef );
				for ( IPhi = 1; IPhi <= 10; ++IPhi ) {
					tsolPhiFit( IPhi ) = 0.0;
					tvisPhiFit( IPhi ) = 0.0;
					Phi = double( IPhi - 1 ) * 10.0;
					CosPhi = std::cos( Phi * DegToRadians );
					if ( std::abs( CosPhi ) < 0.0001 ) CosPhi = 0.0;
					Real64 cos_pow( 1.0 );
					for ( CoefNum = 1; CoefNum <= 6; ++CoefNum ) {
						cos_pow *= CosPhi;
						tsolPhiFit( IPhi ) += solBeamCoef( CoefNum ) * cos_pow;
						tvisPhiFit( IPhi ) += visBeamCoef( CoefNum ) * cos_pow;
					}
				}
			}

			if ( ShadeOn ) W5LsqFit( CosPhiIndepVar, solabsShadePhi, 6, 1, 10, Construct( ConstrNum ).AbsBeamShadeCoef );

		} // End of loop over constructions

		// Get effective glass and shade/blind emissivities for windows that have interior blind or
		// shade. These are used to calculate zone MRT contribution from window when
		// interior blind/shade is deployed.

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
			if ( ! Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) continue;
			if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) continue; //Irrelevant for Complex Fen
			if ( Construct( Surface( SurfNum ).Construction ).WindowTypeEQL ) continue; // not required
			ConstrNumSh = SurfaceWindow( SurfNum ).ShadedConstruction;
			if ( ConstrNumSh == 0 ) continue;
			TotLay = Construct( ConstrNumSh ).TotLayers;
			IntShade = false;
			IntBlind = false;
			if ( Material( Construct( ConstrNumSh ).LayerPoint( TotLay ) ).Group == Shade ) {
				IntShade = true;
				ShadeLayPtr = Construct( ConstrNumSh ).LayerPoint( TotLay );
			}
			if ( Material( Construct( ConstrNumSh ).LayerPoint( TotLay ) ).Group == WindowBlind ) {
				IntBlind = true;
				BlNum = Material( Construct( ConstrNumSh ).LayerPoint( TotLay ) ).BlindDataPtr;
			}

			if ( IntShade || IntBlind ) {
				for ( ISlatAng = 1; ISlatAng <= MaxSlatAngs; ++ISlatAng ) {
					if ( IntShade || IntBlind ) {
						EpsGlIR = Material( Construct( ConstrNumSh ).LayerPoint( TotLay - 1 ) ).AbsorpThermalBack;
						RhoGlIR = 1 - EpsGlIR;
					}
					if ( IntShade ) {
						TauShIR = Material( ShadeLayPtr ).TransThermal;
						EpsShIR = Material( ShadeLayPtr ).AbsorpThermal;
						RhoShIR = max( 0.0, 1.0 - TauShIR - EpsShIR );
						SurfaceWindow( SurfNum ).EffShBlindEmiss( 1 ) = EpsShIR * ( 1.0 + RhoGlIR * TauShIR / ( 1.0 - RhoGlIR * RhoShIR ) );
						SurfaceWindow( SurfNum ).EffGlassEmiss( 1 ) = EpsGlIR * TauShIR / ( 1.0 - RhoGlIR * RhoShIR );
					}
					if ( IntBlind ) {
						TauShIR = Blind( BlNum ).IRFrontTrans( ISlatAng );
						EpsShIR = Blind( BlNum ).IRBackEmiss( ISlatAng );
						RhoShIR = max( 0.0, 1.0 - TauShIR - EpsShIR );
						SurfaceWindow( SurfNum ).EffShBlindEmiss( ISlatAng ) = EpsShIR * ( 1.0 + RhoGlIR * TauShIR / ( 1.0 - RhoGlIR * RhoShIR ) );
						SurfaceWindow( SurfNum ).EffGlassEmiss( ISlatAng ) = EpsGlIR * TauShIR / ( 1.0 - RhoGlIR * RhoShIR );
					}
					// Loop over remaining slat angles only if blind with movable slats
					if ( IntShade ) break; // Loop over remaining slat angles only if blind
					if ( IntBlind ) {
						if ( Blind( BlNum ).SlatAngleType == FixedSlats ) break;
					}
				} // End of slat angle loop
			} // End of check if interior shade or interior blind
		} // End of surface loop

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( Surface( SurfNum ).Construction <= 0 ) continue;
			if ( ! Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) continue;
			ConstrNum = Surface( SurfNum ).Construction;
			// Total thickness of glazing system (used in calculation of inside reveal reflection/absorption
			SurfaceWindow( SurfNum ).TotGlazingThickness = 0.0;
			for ( LayNum = 1; LayNum <= Construct( ConstrNum ).TotLayers; ++LayNum ) {
				SurfaceWindow( SurfNum ).TotGlazingThickness += Material( Construct( ConstrNum ).LayerPoint( LayNum ) ).Thickness;
			}
			// Sine and cosine of azimuth and tilt
			//    SurfaceWindow(SurfNum)%SinAzim = Surface(SurfNum)%SinAzim
			//    SurfaceWindow(SurfNum)%CosAzim = Surface(SurfNum)%CosAzim
			//    SurfaceWindow(SurfNum)%SinTilt = Surface(SurfNum)%SinTilt
			//    SurfaceWindow(SurfNum)%CosTilt = Surface(SurfNum)%CosTilt
			//    ! Outward normal unit vector (pointing away from room)
			//    SurfaceWindow(SurfNum)%OutNormVec(1) = Surface(SurfNum)%OutNormVec(1)
			//    SurfaceWindow(SurfNum)%OutNormVec(2) = Surface(SurfNum)%OutNormVec(2)
			//    SurfaceWindow(SurfNum)%OutNormVec(3) = Surface(SurfNum)%OutNormVec(3)
			//    write(outputfiledebug,*) 'window='//TRIM(surface(SurfNum)%name)
			//    write(outputfiledebug,*) '  swindow%outnormvec=',surfacewindow(SurfNum)%outnormvec
			//    write(outputfiledebug,*) '  surface%outnormvec=',surface(SurfNum)%outnormvec
			// Window center
			Rectangle = false;
			Triangle = false;
			if ( Surface( SurfNum ).Sides == 3 ) Triangle = true;
			if ( Surface( SurfNum ).Sides == 4 ) Rectangle = true;
			if ( Rectangle ) {
				// Vertices of window (numbered counter-clockwise starting at upper left as viewed from inside of room).
				// Assumes original vertices are numbered counter-clockwise from upper left as viewed from outside.
				W3 = Surface( SurfNum ).Vertex( 2 );
				W2 = Surface( SurfNum ).Vertex( 3 );
				W1 = Surface( SurfNum ).Vertex( 4 );
			} else if ( Triangle ) {
				W3 = Surface( SurfNum ).Vertex( 2 );
				W2 = Surface( SurfNum ).Vertex( 3 );
				W1 = Surface( SurfNum ).Vertex( 1 );
			}
			W21 = W1 - W2;
			W23 = W3 - W2;
			if ( Rectangle ) {
				SurfaceWindow( SurfNum ).WinCenter = W2 + ( W23 + W21 ) / 2.0;
			} else if ( Triangle ) {
				SurfaceWindow( SurfNum ).WinCenter = W2 + ( W23 + W21 ) / 3.0;
			}
		} // End of surface loop

		ReportGlass();

	}

	//*****************************************************************************************

	void
	W5InitGlassParameters()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   October 1999
		//       MODIFIED       Aug 2001 (FW): add blinds
		//                      Oct 2002 (FW): change ConstrNumSh =
		//         WindowShadingControl(Surface(SurfNum)%WindowShadingControlPtr)%ShadedConstruction
		//         to Surface(SurfNum)%ShadedConstruction
		//                      Jul 2003 (FW): remove unneeded warning if center-of-glass area < 0
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initializes variables used in the window optical and thermal calculation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int ConstrNum; // Construction number
		int SurfNum; // Surface number
		int IPhi; // Angle of incidence counter
		int FrDivNum; // Pointer to frame/divider
		Real64 FrWidth; // Window frame width {m}
		Real64 FrEdgeWidth; // Frame edge width {m}
		Real64 DivWidth; // Window divider width {m}
		Real64 DivEdgeWidth; // Divider edge width {m}
		Real64 GlHeight; // Height of glazed part of window {m}
		Real64 GlWidth; // Width of glazed part of window {m}
		int NumHorDividers; // Number of horizontal divider elements
		int NumVertDividers; // Number of vertical divider elements
		int BaseSurfNum; // Base surface number
		int ShadingType; // Window shading type
		int MatNum; // Material number
		int DifOverrideCount; // Count the number of SolarDiffusing material overrides

		// FLOW

		for ( ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum ) {
			if ( Construct( ConstrNum ).FromWindow5DataFile ) continue;
			if ( Construct( ConstrNum ).WindowTypeBSDF ) continue;
			Construct( ConstrNum ).TransDiff = 0.0;
			Construct( ConstrNum ).TransDiffVis = 0.0;
			Construct( ConstrNum ).AbsDiffBackShade = 0.0;
			Construct( ConstrNum ).ShadeAbsorpThermal = 0.0;
			Construct( ConstrNum ).ReflectSolDiffBack = 0.0;
			Construct( ConstrNum ).ReflectSolDiffFront = 0.0;
			Construct( ConstrNum ).ReflectVisDiffFront = 0.0;
			Construct( ConstrNum ).AbsBeamShadeCoef = 0.0;
			Construct( ConstrNum ).TransSolBeamCoef = 0.0;
			Construct( ConstrNum ).ReflSolBeamFrontCoef = 0.0;
			Construct( ConstrNum ).ReflSolBeamBackCoef = 0.0;
			Construct( ConstrNum ).TransVisBeamCoef = 0.0;
			Construct( ConstrNum ).AbsBeamCoef = 0.0;
			Construct( ConstrNum ).AbsBeamBackCoef = 0.0;
			Construct( ConstrNum ).AbsDiff = 0.0;
			Construct( ConstrNum ).AbsDiffBack = 0.0;
		}

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			// For a window with shading device, get number of shaded construction and, if window
			// has a blind (interior, exterior or between glass), get blind data pointer.

			// TH 2/16/2010. CR 8010. The following code was modified and moved to GetSurfaceData
			//  in SurfaceGeometry module, because for blinds with variable slats new blinds were created and assigned
			if ( Surface( SurfNum ).WindowShadingControlPtr != 0 ) {
				//  ConstrNumSh = Surface(SurfNum)%ShadedConstruction
				ShadingType = WindowShadingControl( Surface( SurfNum ).WindowShadingControlPtr ).ShadingType;
				//  IF(ShadingType == WSC_ST_ExteriorBlind) THEN
				//    MatNum = Construct(ConstrNumSh)%LayerPoint(1)
				//    SurfaceWindow(SurfNum)%BlindNumber = Material(MatNum)%BlindDataPtr
				//  ELSE IF(ShadingType == WSC_ST_InteriorBlind) THEN
				//    MatNum = Construct(ConstrNumSh)%LayerPoint(Construct(ConstrNumSh)%TotLayers)
				//    SurfaceWindow(SurfNum)%BlindNumber = Material(MatNum)%BlindDataPtr
				// Between glass blind is layer 3 for double glazing and layer 5 for triple glazing.
				//  ELSE IF(ShadingType == WSC_ST_BetweenGlassBlind) THEN
				//    IF(Construct(ConstrNumSh)%TotGlassLayers == 2) THEN
				//      SurfaceWindow(SurfNum)%BlindNumber = Material(Construct(ConstrNumSh)%LayerPoint(3))%BlindDataPtr
				//    ELSE
				//      SurfaceWindow(SurfNum)%BlindNumber = Material(Construct(ConstrNumSh)%LayerPoint(5))%BlindDataPtr
				//    END IF
				//  ELSE IF(ShadingType == WSC_ST_ExteriorScreen) THEN
				if ( ShadingType == WSC_ST_ExteriorScreen ) {
					//     Count number of exterior window screens, initialize in InitGlassOpticalCalculations after returning
					//     from this subroutine. The blind structure is initialized first and then the screen structure is initialized.
					++NumSurfaceScreens;
				}
			}
		}

		// Set some static exterior-window frame and divider SurfaceWindow values
		// from values in FrameDivider derived type
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			FrDivNum = Surface( SurfNum ).FrameDivider;
			if ( FrDivNum > 0 ) { // Surface is a window with a frame and/or divider
				FrWidth = FrameDivider( FrDivNum ).FrameWidth;
				GlHeight = Surface( SurfNum ).Height;
				GlWidth = Surface( SurfNum ).Width;
				NumVertDividers = FrameDivider( FrDivNum ).VertDividers;
				NumHorDividers = FrameDivider( FrDivNum ).HorDividers;
				BaseSurfNum = Surface( SurfNum ).BaseSurf;
				SurfaceWindow( SurfNum ).FrameConductance = FrameDivider( FrDivNum ).FrameConductance;
				SurfaceWindow( SurfNum ).FrameSolAbsorp = FrameDivider( FrDivNum ).FrameSolAbsorp;
				SurfaceWindow( SurfNum ).FrameVisAbsorp = FrameDivider( FrDivNum ).FrameVisAbsorp;
				SurfaceWindow( SurfNum ).FrameEmis = FrameDivider( FrDivNum ).FrameEmis;
				SurfaceWindow( SurfNum ).FrEdgeToCenterGlCondRatio = FrameDivider( FrDivNum ).FrEdgeToCenterGlCondRatio;
				SurfaceWindow( SurfNum ).DividerType = DividedLite;
				if ( FrameDivider( FrDivNum ).DividerType == Suspended ) SurfaceWindow( SurfNum ).DividerType = Suspended;
				DivWidth = FrameDivider( FrDivNum ).DividerWidth;
				SurfaceWindow( SurfNum ).DividerConductance = FrameDivider( FrDivNum ).DividerConductance;
				SurfaceWindow( SurfNum ).DividerSolAbsorp = FrameDivider( FrDivNum ).DividerSolAbsorp;
				SurfaceWindow( SurfNum ).DividerVisAbsorp = FrameDivider( FrDivNum ).DividerVisAbsorp;
				SurfaceWindow( SurfNum ).DividerEmis = FrameDivider( FrDivNum ).DividerEmis;
				SurfaceWindow( SurfNum ).DivEdgeToCenterGlCondRatio = FrameDivider( FrDivNum ).DivEdgeToCenterGlCondRatio;

				SurfaceWindow( SurfNum ).OutsideRevealSolAbs = FrameDivider( FrDivNum ).OutsideRevealSolAbs;
				SurfaceWindow( SurfNum ).InsideSillDepth = FrameDivider( FrDivNum ).InsideSillDepth;
				SurfaceWindow( SurfNum ).InsideReveal = FrameDivider( FrDivNum ).InsideReveal;
				SurfaceWindow( SurfNum ).InsideSillSolAbs = FrameDivider( FrDivNum ).InsideSillSolAbs;
				SurfaceWindow( SurfNum ).InsideRevealSolAbs = FrameDivider( FrDivNum ).InsideRevealSolAbs;

				FrEdgeWidth = FrameDivider( FrDivNum ).FrameEdgeWidth;
				DivEdgeWidth = FrameDivider( FrDivNum ).DividerEdgeWidth;
				SurfaceWindow( SurfNum ).FrameEdgeArea = 2 * FrEdgeWidth * ( GlHeight - FrEdgeWidth - NumHorDividers * DivWidth + GlWidth - FrEdgeWidth - NumVertDividers * DivWidth );
				SurfaceWindow( SurfNum ).DividerEdgeArea = 2 * DivEdgeWidth * ( NumHorDividers * ( GlWidth - 2 * FrEdgeWidth ) + NumVertDividers * ( GlHeight - 2 * FrEdgeWidth ) ) - NumHorDividers * NumVertDividers * ( 4 * pow_2( DivEdgeWidth ) + 4 * FrEdgeWidth * DivWidth );
				SurfaceWindow( SurfNum ).CenterGlArea = Surface( SurfNum ).Area - SurfaceWindow( SurfNum ).FrameEdgeArea - SurfaceWindow( SurfNum ).DividerEdgeArea;
				SurfaceWindow( SurfNum ).EdgeGlCorrFac = ( SurfaceWindow( SurfNum ).FrameEdgeArea * SurfaceWindow( SurfNum ).FrEdgeToCenterGlCondRatio + SurfaceWindow( SurfNum ).DividerEdgeArea * SurfaceWindow( SurfNum ).DivEdgeToCenterGlCondRatio + SurfaceWindow( SurfNum ).CenterGlArea ) / ( SurfaceWindow( SurfNum ).FrameEdgeArea + SurfaceWindow( SurfNum ).DividerEdgeArea + SurfaceWindow( SurfNum ).CenterGlArea );
			}
		}

		// Set SolarDiffusing to true for exterior windows that have a construction with an innermost diffusing glass layer
		DifOverrideCount = 0;
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			SurfaceWindow( SurfNum ).SolarDiffusing = false;
			if ( Surface( SurfNum ).Class == SurfaceClass_Window && Surface( SurfNum ).ExtBoundCond == ExternalEnvironment && Surface( SurfNum ).StormWinConstruction == 0 ) {
				ConstrNum = Surface( SurfNum ).Construction;
				MatNum = Construct( ConstrNum ).LayerPoint( Construct( ConstrNum ).TotLayers );
				if ( Material( MatNum ).SolarDiffusing ) {
					if ( Surface( SurfNum ).WindowShadingControlPtr == 0 ) {
						SurfaceWindow( SurfNum ).SolarDiffusing = true;
					} else { // There is a shading control
						if ( WindowShadingControl( Surface( SurfNum ).WindowShadingControlPtr ).ShadingType == SwitchableGlazing ) {
							SurfaceWindow( SurfNum ).SolarDiffusing = true;
						} else {
							SurfaceWindow( SurfNum ).SolarDiffusing = false;
							++DifOverrideCount;
							if ( DisplayExtraWarnings ) {
								ShowWarningError( "W5InitGlassParameters: Window=\"" + Surface( SurfNum ).Name + "\" has interior material with Solar Diffusing=Yes, but existing Window Shading Device sets Diffusing=No." );
							}
						}
					}
				}
			}
		}

		if ( DifOverrideCount > 0 ) {
			if ( ! DisplayExtraWarnings ) {
				ShowWarningError( "W5InitGlassParameters: " + RoundSigDigits( DifOverrideCount ) + " Windows had Solar Diffusing=Yes overridden by presence of Window Shading Device." );
			} else {
				ShowMessage( "W5InitGlassParameters: " + RoundSigDigits( DifOverrideCount ) + " Windows had Solar Diffusing=Yes overridden by presence of Window Shading Device." );
			}
		}

		for ( IPhi = 1; IPhi <= 10; ++IPhi ) {
			CosPhiIndepVar( IPhi ) = std::cos( ( IPhi - 1 ) * 10.0 * DegToRadians );
		}

	}

	//****************************************************************************
	// WINDOW 5 Optical Calculation Subroutines
	//****************************************************************************

	void
	SystemSpectralPropertiesAtPhi(
		int const iquasi, // When there is no spectral data, this is the wavelength
		int const ngllayer, // Number of glass layers in construction
		Real64 const wlbot, // Lowest and highest wavelength considered
		Real64 const wltop
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Adapted by F.Winkelmann from WINDOW 5
		//                      subroutine opcalc
		//       DATE WRITTEN   August 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// For a particular angle of incidence, calculates system properties
		// for a multi-layer glazing for each wavelength in the solar spectrum.
		// Handles the special case of one or more layers that do not have spectral data.

		// Returns, for a particular angle of incidence:
		//   stPhi     transmissivity of system at each wavelength in swl
		//   srfPhi    front reflectance of system at each wavelength in swl
		//   srbPhi    back reflectance of system at each wavelength in swl
		//   sabsPhi   absorptance by layer at each wavelength in swl

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Array1D< Real64 > sabsPhi( 5 ); // System solar absorptance in each glass layer for
		//   particular angle of incidence
		int in; // Glass layer counter
		int i;
		int iwl; // Wavelength counter
		int j;
		Real64 wl; // Wavelength
		//   index to use in tPhi, rfPhi and rbPhi

		// For each glass layer find tPhi, rfPhi, and rbPhi at each wavelength

		for ( in = 1; in <= ngllayer; ++in ) {
			for ( iwl = 1; iwl <= nume; ++iwl ) {
				wl = wle( iwl );
				if ( wl < wlbot || wl > wltop ) continue;
				// In the following numpt is the number of spectral data points for each layer;
				// numpt = 2 if there is no spectral data for a layer.
				if ( numpt( in ) <= 2 ) {
					tadjPhi( in, iwl ) = tPhi( in, iquasi );
					rfadjPhi( in, iwl ) = rfPhi( in, iquasi );
					rbadjPhi( in, iwl ) = rbPhi( in, iquasi );
				} else {
					// Interpolate to get properties at the solar spectrum wavelengths
					Interpolate( wlt( in, 1 ), tPhi( in, 1 ), numpt( in ), wl, tadjPhi( in, iwl ) );
					Interpolate( wlt( in, 1 ), rfPhi( in, 1 ), numpt( in ), wl, rfadjPhi( in, iwl ) );
					Interpolate( wlt( in, 1 ), rbPhi( in, 1 ), numpt( in ), wl, rbadjPhi( in, iwl ) );
				}
			}
		}

		// Calculate system properties at each wavelength
		for ( j = 1; j <= nume; ++j ) {
			wl = wle( j );
			if ( wl < wlbot || wl > wltop ) continue;

			// Set diagonal of matrix for subroutine SystemPropertiesAtLambdaAndPhi
			for ( i = 1; i <= ngllayer; ++i ) {
				top( i, i ) = tadjPhi( i, j );
				rfop( i, i ) = rfadjPhi( i, j );
				rbop( i, i ) = rbadjPhi( i, j );
			}

			// Calculate glazing system properties
			if ( ngllayer == 1 ) { // Single-layer system
				stPhi( j ) = top( 1, 1 );
				srfPhi( j ) = rfop( 1, 1 );
				srbPhi( j ) = rbop( 1, 1 );
				sabsPhi( 1 ) = 1.0 - stPhi( j ) - srfPhi( j );
			} else { // Multilayer system
				// Get glazing system properties stPhi, etc., at this wavelength and incidence angle
				SystemPropertiesAtLambdaAndPhi( ngllayer, stPhi( j ), srfPhi( j ), srbPhi( j ), sabsPhi );
			}

			for ( i = 1; i <= ngllayer; ++i ) {
				saPhi( i, j ) = sabsPhi( i );
			}

		} // End of wavelength loop

	}

	//************************************************************************

	void
	SystemPropertiesAtLambdaAndPhi(
		int const n, // Number of glass layers
		Real64 & tt, // System transmittance
		Real64 & rft, // System front and back reflectance
		Real64 & rbt,
		Array1A< Real64 > aft // System absorptance of each glass layer
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Adapted by F. Winkelmann from WINDOW 5
		//                      subroutine op
		//       DATE WRITTEN   August 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// For a given angle of incidence, finds the overall properties of
		// of a series of layers at a particular wavelength

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Argument array dimensioning
		aft.dim( 5 );

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int i; // Glass layer counters
		int j;
		Real64 denom; // Intermediate variables
		Real64 denom1;
		Real64 denom2;
		Real64 t0; // Transmittance, back reflectance and front
		Real64 rb0;
		Real64 rf0;
		//   reflectance variables
		Real64 af; // Front and back absorptance variables
		Real64 ab;

		// FLOW

		// Calculate perimeter elements of rt matrix
		for ( i = 1; i <= n - 1; ++i ) {
			for ( j = i + 1; j <= n; ++j ) {
				denom = 1.0 - rfop( j, j ) * rbop( i, j - 1 );
				if ( denom == 0.0 ) {
					top( j, i ) = 0.0;
					rfop( j, i ) = 1.0;
					rbop( i, j ) = 1.0;
				} else {
					top( j, i ) = top( j - 1, i ) * top( j, j ) / denom;
					rfop( j, i ) = rfop( j - 1, i ) + pow_2( top( j - 1, i ) ) * rfop( j, j ) / denom;
					rbop( i, j ) = rbop( j, j ) + pow_2( top( j, j ) ) * rbop( i, j - 1 ) / denom;
				}
			}
		}
		// System properties: transmittance, front and back reflectance
		tt = top( n, 1 );
		rft = rfop( n, 1 );
		rbt = rbop( 1, n );

		// Absorptance in each layer
		for ( j = 1; j <= n; ++j ) {
			if ( j == 1 ) {
				t0 = 1.0;
				rb0 = 0.0;
			} else {
				t0 = top( j - 1, 1 );
				rb0 = rbop( 1, j - 1 );
			}

			if ( j == n ) {
				rf0 = 0.0;
			} else {
				rf0 = rfop( n, j + 1 );
			}

			af = 1.0 - top( j, j ) - rfop( j, j );
			ab = 1.0 - top( j, j ) - rbop( j, j );
			denom1 = 1.0 - rfop( n, j ) * rb0;
			denom2 = 1.0 - rbop( 1, j ) * rf0;

			if ( denom1 == 0.0 || denom2 == 0.0 ) {
				aft( j ) = 0.0;
			} else {
				aft( j ) = ( t0 * af ) / denom1 + ( top( j, 1 ) * rf0 * ab ) / denom2;
			}
		}
	}

	//*************************************************************************

	void
	SolarSprectrumAverage(
		Array1A< Real64 > p, // Quantity to be weighted by solar spectrum
		Real64 & psol // Quantity p weighted by solar spectrum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Adapted by F.Winkelmann from WINDOW 5 subroutine solar
		//       DATE WRITTEN   August 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates average of property p weighted by solar spectral irradiance, e

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Argument array dimensioning
		p.dim( nume );

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 up; // Intermediate variables
		Real64 down;
		int i; // Wavelength counter
		Real64 esol; // Solar spectrum value times delta wavelength

		// FLOW

		up = 0.0;
		down = 0.0;

		for ( i = 1; i <= nume - 1; ++i ) {
			esol = ( wle( i + 1 ) - wle( i ) ) * 0.5 * ( e( i ) + e( i + 1 ) );
			up += 0.5 * ( p( i ) + p( i + 1 ) ) * esol;
			down += esol;
		}

		psol = up / down;

	}

	//********************************************************************

	void
	VisibleSprectrumAverage(
		Array1A< Real64 > p, // Quantity to be weighted by solar spectrum
		Real64 & pvis // Quantity p weighted by solar spectrum and photopic
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Adapted by F.Winkelmann from WINDOW 5
		//                      subroutine w4vis
		//       DATE WRITTEN   August 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates visible average of property p by weighting with solar
		// spectral irradiance, e, and photopic response, y30

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Argument array dimensioning
		p.dim( nume );

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 up; // Intermediate variables
		Real64 down;
		int i; // Wavelength counter
		//   response curve
		Real64 y30ils1; // Photopic response variables
		Real64 y30new;
		Real64 evis; // Solar spectrum value times photopic response
		//   times delta wavelength
		// FLOW

		down = 0.0;
		up = 0.0;
		y30ils1 = 0.0;
		y30new = 0.0;

		for ( i = 2; i <= nume; ++i ) { //Autodesk:BoundsViolation e|wle|p(i-1) @ i=1: Changed start index from 1 to 2: wle values prevented this violation from occurring in practice
			// Restrict to visible range
			if ( wle( i ) >= 0.37 && wle( i ) <= 0.78 ) {
				Interpolate( wlt3, y30, numt3, wle( i ), y30new );
				evis = e( i - 1 ) * 0.5 * ( y30new + y30ils1 ) * ( wle( i ) - wle( i - 1 ) );
				up += 0.5 * ( p( i ) + p( i - 1 ) ) * evis;
				down += evis;
				y30ils1 = y30new;
			}
		}

		pvis = up / down;

	}

	//**********************************************************************

	void
	Interpolate(
		Array1A< Real64 > x, // Array of data points for independent variable
		Array1A< Real64 > y, // Array of data points for dependent variable
		int const npts, // Number of data pairs
		Real64 const xin, // Given value of x
		Real64 & yout // Interpolated value of y at xin
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Adapted by F.Winkelmann from WINDOW 5 subroutine interp
		//       DATE WRITTEN   August 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Linearly interpolates between data points. Outputs yout, interpolated
		// value of y corresponding to xin

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Argument array dimensioning
		x.dim( npts );
		y.dim( npts );

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int i; // Counter

		// FLOW

		for ( i = 1; i <= npts; ++i ) {
			if ( xin <= x( i ) ) {
				if ( i == 1 ) {
					yout = y( 1 );
				} else {
					yout = y( i - 1 ) + ( y( i ) - y( i - 1 ) ) * ( xin - x( i - 1 ) ) / ( x( i ) - x( i - 1 ) );
				}
				return;
			}
		}

		// Past the end of the array, so return endpoint
		yout = y( npts );
	}

	//***********************************************************************************
	// Window Thermal Calculation Subroutines
	//***********************************************************************************

	void
	CalcWindowHeatBalance(
		int const SurfNum, // Surface number
		Real64 const HextConvCoeff, // Outside air film conductance coefficient
		Real64 & SurfInsideTemp, // Inside window surface temperature
		Real64 & SurfOutsideTemp // Outside surface temperature (C)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   November 1999
		//       MODIFIED       FW, July 2000 (call better solution method)
		//                      FW, June 2001 (handle window blinds)
		//                      FW, Dec  2002 (add between-glass shades and blinds)
		//                      FW, Mar  2003 (extend condensation flag to airflow windows)
		//                      CC, Jul  2003 (set the reference temperatures for inside surface heat balance
		//                                    depending on convection algorithms and/or air models used)
		//                      FW, Sep  2003 (increment ZoneWinHeatGain only for exterior windows)
		//                      RR, May  2006 (add exterior window screen)
		//                      TH, Dec  2008 (add thermochromic windows)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Sets up information needed to calculate the window thermal behavior.
		// Calls SolveForWindowTemperatures, which calculates the inside and outside
		// face temperature of each glass layer by solving the heat balance
		// equations on each face. Also calls CalcWinFrameAndDividerTemps,
		// which calculates the outside and inside face temperatures of the
		// window frame and divider if either of these are present.
		// The resulting inside face temperature of the inner glass pane and the
		// inside surface temperatures of frame and divider are used in the zone
		// heat balance calculation. The inside face temperature of an interior shade
		// or blind, if present, and the natural convection air flow between the
		// shade/blind and inside glass face also appear in the zone heat balance calculation.
		// The logical variable NRSolution is currently set to false, which means
		// that the Newton-Raphson solution method for the glass layer heat balance
		// is not used (because it sometimes didn't converge for 3- and 4-pane
		// constructions with one or more low-emissivity layers). Instead, a more
		// robust solution method is used that successively solves linearized heat
		// balance equations until convergence is reached (see SolveForWindowTemperatures).
		// CalcWindowHeatBalance is called by CalcHeatBalanceInsideSurface once each
		// time step for each window.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataBSDFWindow;
		using General::InterpSlatAng; // Function for slat angle interpolation
		using DataZoneEquipment::ZoneEquipConfig;
		using DataLoopNode::Node;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyTdpFnWPb;
		using DataHeatBalSurface::QConvOutReport;
		using DataHeatBalSurface::QdotConvOutRep;
		using DataHeatBalSurface::QdotConvOutRepPerArea;
		using DataHeatBalSurface::QRadOutReport;
		using DataHeatBalSurface::QdotRadOutRep;
		using DataHeatBalSurface::QdotRadOutRepPerArea;
		//unused0909  USE DataEnvironment, ONLY: CurMnDyHr
		using InputProcessor::SameString;
		using WindowComplexManager::CalcComplexWindowThermal;
		using WindowEquivalentLayer::EQLWindowSurfaceHeatBalance;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// (temperature of innermost face) [C]

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int ZoneNum; // Zone number corresponding to SurfNum
		int BlNum; // Window blind number
		int SurfNumAdj; // An interzone surface's number in the adjacent zone
		int ZoneNumAdj; // An interzone surface's adjacent zone number
		int ConstrNum; // Construction number
		//unused INTEGER           :: ConstrNumSh                  ! Shaded construction number
		int IConst; // Construction number
		int TotLay; // Total number of layers in a construction
		//   (sum of solid layers and gap layers)
		int TotGlassLay; // Total number of glass layers in a construction
		int Lay; // Layer number
		int LayPtr; // Material number for a layer
		int IGlass; // glass layer number (1,2,3,...)
		int IGap; // Gap layer number (1,2,...)
		int IMix; // Gas number in a mixture of gases
		int ICoeff; // Gas property index (1,2,3)
		int ShadeFlag; // Flag indicating whether shade or blind is on, and shade/blind position
		int k; // Layer counter
		//REAL(r64) :: tsky                         ! Sky temperature [K]
		int ShadeLayPtr; // Material number corresponding to a shade layer
		Real64 dth1; // Temperature difference across glass layers [K]
		Real64 dth2;
		Real64 dth3;
		Real64 dth4;
		Real64 EffShBlEmiss; // Effective interior shade or blind emissivity
		Real64 EffGlEmiss; // Effective inside glass emissivity when interior shade or blind
		Real64 RoomHumRat; // Room air humidity ratio
		Real64 RoomDewPoint; // Room air dewpoint temperature (C)
		Real64 InsideGlassTemp; // Temperature of room side of innermost glass layer (C)
		Real64 Tleft; // For airflow windows, temperature of the glass faces adjacent
		Real64 Tright;
		//  to the airflow gap (C)
		int ZoneEquipConfigNum;
		int NodeNum;
		Real64 SumSysMCp; // Zone sum of air system MassFlowRate*Cp
		Real64 SumSysMCpT; // Zone sum of air system MassFlowRate*Cp*T
		Real64 MassFlowRate;
		Real64 NodeTemp;
		Real64 CpAir;
		Real64 RefAirTemp; // reference air temperatures

		// New variables for thermochromic windows calc
		Real64 locTCSpecTemp; // The temperature corresponding to the specified optical properties of the TC layer
		Real64 locTCLayerTemp; // TC layer temperature at each time step. C
		static bool locTCFlag( false ); // True if this surface is a TC window
		static Array1D< Real64 > deltaTemp( 100, 0.0 );
		int i;
		static Array1D_int iMinDT( 1, 0 );
		static Array1D_int IDConst( 100, 0 );
		static Real64 dT0( 0.0 );
		static Real64 dT1( 0.0 );
		Real64 SurfOutsideEmiss; // temporary for result of outside surface emissivity
		Real64 Tsout; // temporary for result of outside surface temp in Kelvin
		//integer :: CurrentThermalAlgorithm
		int temp;

		//CurrentThermalAlgorithm = -1

		if ( KickOffSizing || KickOffSimulation ) return;

		// Shorthand refernces
		auto & window( SurfaceWindow( SurfNum ) );
		auto & surface( Surface( SurfNum ) );

		if ( window.WindowModelType == WindowBSDFModel ) {

			temp = 0;

			//Simon: Complex fenestration state works only with tarcog
			CalcComplexWindowThermal( SurfNum, temp, HextConvCoeff, SurfInsideTemp, SurfOutsideTemp, SurfOutsideEmiss, noCondition );

			ConstrNum = surface.Construction;
			TotGlassLay = Construct( ConstrNum ).TotGlassLayers;
			ngllayer = Construct( ConstrNum ).TotSolidLayers; // Simon: This is necessary to keep for frame calculations
			// Simon: need to transfer surface temperatures because of frames calculation
			for ( i = 1; i <= 2 * Construct( ConstrNum ).TotSolidLayers; ++i ) {
				thetas( i ) = window.ThetaFace( i );
			}
			hcout = HextConvCoeff;

			// This is code repeating and it is necessary to calculate report variables.  Do not know
			// how to solve this in more elegant way :(
			if ( surface.ExtWind ) { // Window is exposed to wind (and possibly rain)
				if ( IsRain ) { // Raining: since wind exposed, outside window surface gets wet
					tout = surface.OutWetBulbTemp + TKelvin;
				} else { // Dry
					tout = surface.OutDryBulbTemp + TKelvin;
				}
			} else { // Window not exposed to wind
				tout = surface.OutDryBulbTemp + TKelvin;
			}

		} else if ( window.WindowModelType == WindowEQLModel ) {

			EQLWindowSurfaceHeatBalance( SurfNum, HextConvCoeff, SurfInsideTemp, SurfOutsideTemp, SurfOutsideEmiss, noCondition );
			hcout = HextConvCoeff;
			// Required for report variables calculations.
			if ( surface.ExtWind ) { // Window is exposed to wind (and possibly rain)
				if ( IsRain ) { // Raining: since wind exposed, outside window surface gets wet
					tout = surface.OutWetBulbTemp + TKelvin;
				} else { // Dry
					tout = surface.OutDryBulbTemp + TKelvin;
				}
			} else { // Window not exposed to wind
				tout = surface.OutDryBulbTemp + TKelvin;
			}

		} else { // regular window, not BSDF, not EQL Window

			ConstrNum = surface.Construction;
			if ( window.StormWinFlag > 0 ) ConstrNum = surface.StormWinConstruction;

			// Added for thermochromic windows
			locTCFlag = ( Construct( ConstrNum ).TCFlag == 1 );

			if ( locTCFlag ) {
				locTCSpecTemp = Material( Construct( ConstrNum ).TCLayer ).SpecTemp;
				window.SpecTemp = locTCSpecTemp;
				// Check to see whether needs to switch to a new TC window construction
				locTCLayerTemp = window.TCLayerTemp;
				dT0 = std::abs( locTCLayerTemp - locTCSpecTemp );
				if ( dT0 >= 1 ) {
					// Find the TC construction that is closed to the TCLayerTemp
					i = 0;
					deltaTemp = 0.0;
					IDConst = 0;
					for ( k = 1; k <= TotConstructs; ++k ) {
						if ( Construct( k ).TCMasterConst == Construct( ConstrNum ).TCMasterConst ) {
							dT1 = std::abs( locTCLayerTemp - Material( Construct( k ).TCLayer ).SpecTemp );
							if ( dT1 < dT0 ) {
								++i;
								deltaTemp( i ) = dT1;
								IDConst( i ) = k;
							}
						}
					}
					if ( i >= 1 ) {
						// Find the closest item
						iMinDT = minloc( deltaTemp, deltaTemp > 0.0 );
						// Use the new TC window construction
						ConstrNum = IDConst( iMinDT( 1 ) );
						surface.Construction = ConstrNum;
						window.SpecTemp = Material( Construct( ConstrNum ).TCLayer ).SpecTemp;
					}
				}
			}
			// end new TC code

			ZoneNum = surface.Zone;
			TotLay = Construct( ConstrNum ).TotLayers;
			TotGlassLay = Construct( ConstrNum ).TotGlassLayers;
			ngllayer = TotGlassLay;
			nglface = 2 * ngllayer;
			ShadeFlag = window.ShadingFlag;
			tilt = surface.Tilt;
			tiltr = tilt * DegToRadians;
			SurfNumAdj = surface.ExtBoundCond;
			hcin = HConvIn( SurfNum ); // Room-side surface convective film conductance

			// determine reference air temperature for this surface
			{ auto const SELECT_CASE_var( surface.TAirRef );
			if ( SELECT_CASE_var == ZoneMeanAirTemp ) {
				RefAirTemp = MAT( ZoneNum );
				TempEffBulkAir( SurfNum ) = RefAirTemp;
			} else if ( SELECT_CASE_var == AdjacentAirTemp ) {
				RefAirTemp = TempEffBulkAir( SurfNum );
			} else if ( SELECT_CASE_var == ZoneSupplyAirTemp ) {
				// determine ZoneEquipConfigNum for this zone
				//            ControlledZoneAirFlag = .FALSE.
				ZoneEquipConfigNum = ZoneNum;
				//            DO ZoneEquipConfigNum = 1, NumOfControlledZones
				//                IF (ZoneEquipConfig(ZoneEquipConfigNum)%ActualZoneNum /= ZoneNum) CYCLE
				//                ControlledZoneAirFlag = .TRUE.
				//                EXIT
				//            END DO ! ZoneEquipConfigNum
				// check whether this zone is a controlled zone or not
				if ( ! Zone( ZoneNum ).IsControlled ) {
					ShowFatalError( "Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " + Zone( ZoneNum ).Name );
					return;
				}
				// determine supply air conditions
				SumSysMCp = 0.0;
				SumSysMCpT = 0.0;
				for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ZoneEquipConfigNum ).NumInletNodes; ++NodeNum ) {
					NodeTemp = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).Temp;
					MassFlowRate = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).MassFlowRate;
					CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );
					SumSysMCp += MassFlowRate * CpAir;
					SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
				}
				// a weighted average of the inlet temperatures.
				if ( SumSysMCp > 0.0 ) {
					RefAirTemp = SumSysMCpT / SumSysMCp;
				} else {
					RefAirTemp = NodeTemp;
				}
				TempEffBulkAir( SurfNum ) = RefAirTemp;

			} else {
				// currently set to mean air temp but should add error warning here
				RefAirTemp = MAT( ZoneNum );
				TempEffBulkAir( SurfNum ) = RefAirTemp;
			}}

			tin = RefAirTemp + TKelvin; // Inside air temperature

			// Reset hcin if necessary since too small a value sometimes causes non-convergence
			// of window layer heat balance solution.
			if ( surface.IntConvCoeff == 0 ) {
				if ( hcin <= LowHConvLimit ) { // may be redundent now, check is also in HeatBalanceConvectionCoeffs.cc
					//  hcin = 3.076d0  !BG this is rather high value and abrupt change. changed to set to lower limit
					hcin = LowHConvLimit;
					HConvIn( SurfNum ) = hcin; // store for accurate reporting.
				}
			}

			// IR incident on window from zone surfaces and high-temp radiant sources
			Rmir = window.IRfromParentZone + QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum );

			// Short-wave radiation (from interior and exterior solar and zone lights)
			// absorbed at each face. Assumes equal split between faces of short-wave absorbed in glass layer.

			for ( IGlass = 1; IGlass <= TotGlassLay; ++IGlass ) {
				AbsRadGlassFace( 2 * IGlass - 1 ) = QRadSWwinAbs( IGlass, SurfNum ) / 2.0;
				AbsRadGlassFace( 2 * IGlass ) = QRadSWwinAbs( IGlass, SurfNum ) / 2.0;
			}

			// IR from zone internal gains (lights, equipment and people) absorbed on zone-side face
			// (assumes inside glass layer is opaque to IR, so no contribution to other layers)

			AbsRadGlassFace( 2 * TotGlassLay ) += QRadThermInAbs( SurfNum );

			// Fill the layer properties needed for the thermal calculation.
			// For switchable glazing it is assumed that thermal properties, such
			// as surface emissivity, are the same for the unswitched and switched state,
			// so the thermal properties of the unswitched state are used.
			// For windows with a blind or shade it is assumed
			// that the blind or shade does not affect the thermal properties of the glazing,
			// so the thermal properties of the construction without the blind or shade are used.

			// The layer and face numbering are as follows (for the triple glazing case):
			// Glass layers are 1,2 and 3, where 1 is the outside (outside environment facing)
			//   layer and 3 is the inside (room-facing) layer;
			// Faces (also called surfaces) are 1,2,3,4,5 and 6, where face 1 is the
			//   outside (front) face of glass layer 1, face 2 is the inside (back)
			//   face of glass layer 1, face 3 is the outer face of glass layer 2, face 4 is the
			//   inner face of glass layer 2, etc.
			// Gap layers are 1 and 2, where gap layer 1 is between glass layers 1 and 2
			//   and gap layer 2 is between glass layers 2 and 3.
			// If an exterior, interior or between-glass blind or shade is in place, 7 and 8
			//   are the blind/shade faces, from outside to inside. If an exterior or interior
			//   blind/shade is in place, gap layer 3 is between the blind/shade and adjacent
			//   glass layer and is assumed to be air.
			// Between-glass blind/shade is modeled only for double and triple glazing.
			//   For double glazing, gap 1 is between glass 1 and blind/shade and gap 2 is between
			//   blind/shade and glass 2.
			//   For triple glazing, the blind/shade is assumed to be between the inner two glass
			//   layers, i.e., between glass layers 2 and 3. In this case gap 1 is between glass 1
			//   and glass 2, gap 2 is between glass 2 and blind/shade, and gap 3 is between
			//   blind/shade and glass 3.

			IConst = ConstrNum;
			if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn || ShadeFlag == ExtScreenOn ) {
				IConst = surface.ShadedConstruction;
				if ( window.StormWinFlag > 0 ) IConst = surface.StormWinShadedConstruction;
			}
			TotLay = Construct( IConst ).TotLayers;
			IGlass = 0;
			IGap = 0;

			// Fill window layer properties needed for window layer heat balance calculation

			for ( Lay = 1; Lay <= TotLay; ++Lay ) {
				LayPtr = Construct( IConst ).LayerPoint( Lay );

				if ( ( Material( LayPtr ).Group == WindowGlass ) || ( Material( LayPtr ).Group == WindowSimpleGlazing ) ) {
					++IGlass;
					thick( IGlass ) = Material( LayPtr ).Thickness;
					scon( IGlass ) = Material( LayPtr ).Conductivity / Material( LayPtr ).Thickness;
					emis( 2 * IGlass - 1 ) = Material( LayPtr ).AbsorpThermalFront;
					emis( 2 * IGlass ) = Material( LayPtr ).AbsorpThermalBack;
					tir( 2 * IGlass - 1 ) = Material( LayPtr ).TransThermal;
					tir( 2 * IGlass ) = Material( LayPtr ).TransThermal;
				}

				if ( Material( LayPtr ).Group == Shade || Material( LayPtr ).Group == WindowBlind || Material( LayPtr ).Group == Screen ) {
					if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) ShadeLayPtr = Construct( IConst ).LayerPoint( Construct( IConst ).TotLayers );
					if ( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) ShadeLayPtr = Construct( IConst ).LayerPoint( 1 );
					if ( ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn ) {
						ShadeLayPtr = Construct( IConst ).LayerPoint( 3 );
						if ( TotGlassLay == 3 ) ShadeLayPtr = Construct( IConst ).LayerPoint( 5 );
					}
					if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn ) {
						// Shade or screen on
						if ( AnyEnergyManagementSystemInModel ) { // check to make sure the user hasn't messed up the shade control values
							if ( Material( ShadeLayPtr ).Group == WindowBlind ) {
								ShowSevereError( "CalcWindowHeatBalance: ShadeFlag indicates Shade but Blind=\"" + Material( ShadeLayPtr ).Name + "\" is being used." );
								ShowContinueError( "This is most likely a fault of the EMS values for shading control." );
								ShowFatalError( "Preceding condition terminates program." );
							}
						}
						thick( TotGlassLay + 1 ) = Material( ShadeLayPtr ).Thickness;
						scon( TotGlassLay + 1 ) = Material( ShadeLayPtr ).Conductivity / Material( ShadeLayPtr ).Thickness;
						if ( ShadeFlag == ExtScreenOn ) {
							emis( nglface + 1 ) = Material( ShadeLayPtr ).AbsorpThermalFront;
							tir( nglface + 1 ) = SurfaceScreens( Material( ShadeLayPtr ).ScreenDataPtr ).DifDifTrans;
							tir( nglface + 2 ) = SurfaceScreens( Material( ShadeLayPtr ).ScreenDataPtr ).DifDifTrans;
						} else {
							emis( nglface + 1 ) = Material( ShadeLayPtr ).AbsorpThermal;
							tir( nglface + 1 ) = Material( ShadeLayPtr ).TransThermal;
							tir( nglface + 2 ) = Material( ShadeLayPtr ).TransThermal;
						}
						emis( nglface + 2 ) = Material( ShadeLayPtr ).AbsorpThermal;

					} else {
						if ( AnyEnergyManagementSystemInModel ) { // check to make sure the user hasn't messed up the shade control values
							if ( Material( ShadeLayPtr ).Group == Shade || Material( ShadeLayPtr ).Group == Screen ) {
								ShowSevereError( "CalcWindowHeatBalance: ShadeFlag indicates Blind but Shade/Screen=\"" + Material( ShadeLayPtr ).Name + "\" is being used." );
								ShowContinueError( "This is most likely a fault of the EMS values for shading control." );
								ShowFatalError( "Preceding condition terminates program." );
							}
						}
						// Blind on
						BlNum = window.BlindNumber;
						thick( TotGlassLay + 1 ) = Blind( BlNum ).SlatThickness;
						scon( TotGlassLay + 1 ) = Blind( BlNum ).SlatConductivity / Blind( BlNum ).SlatThickness;
						emis( nglface + 1 ) = InterpSlatAng( window.SlatAngThisTS, window.MovableSlats, Blind( BlNum ).IRFrontEmiss );
						emis( nglface + 2 ) = InterpSlatAng( window.SlatAngThisTS, window.MovableSlats, Blind( BlNum ).IRBackEmiss );
						tir( nglface + 1 ) = InterpSlatAng( window.SlatAngThisTS, window.MovableSlats, Blind( BlNum ).IRFrontTrans );
						tir( nglface + 2 ) = InterpSlatAng( window.SlatAngThisTS, window.MovableSlats, Blind( BlNum ).IRBackTrans );
					}
				}

				if ( Material( LayPtr ).Group == WindowGas || Material( LayPtr ).Group == WindowGasMixture ) {
					++IGap;
					gap( IGap ) = Material( LayPtr ).Thickness;
					gnmix( IGap ) = Material( LayPtr ).NumberOfGasesInMixture;
					for ( IMix = 1; IMix <= gnmix( IGap ); ++IMix ) {
						gwght( IMix, IGap ) = Material( LayPtr ).GasWght( IMix );
						gfract( IMix, IGap ) = Material( LayPtr ).GasFract( IMix );
						for ( ICoeff = 1; ICoeff <= 3; ++ICoeff ) {
							gcon( ICoeff, IMix, IGap ) = Material( LayPtr ).GasCon( ICoeff, IMix );
							gvis( ICoeff, IMix, IGap ) = Material( LayPtr ).GasVis( ICoeff, IMix );
							gcp( ICoeff, IMix, IGap ) = Material( LayPtr ).GasCp( ICoeff, IMix );
						}
					}
				}

			} // End of loop over glass, gap and blind/shade layers in a window construction

			if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) {
				// Interior or exterior blind, shade or screen is on.
				// Fill gap between blind/shade and adjacent glass with air properties.
				++IGap;
				if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == ExtScreenOn ) { // Interior or exterior shade
					gap( IGap ) = Material( ShadeLayPtr ).WinShadeToGlassDist;
				} else { // Interior or exterior blind
					gap( IGap ) = Blind( window.BlindNumber ).BlindToGlassDist;
				}
				gnmix( IGap ) = 1;
				gwght( 1, IGap ) = GasWght( 1 );
				for ( ICoeff = 1; ICoeff <= 3; ++ICoeff ) {
					gcon( ICoeff, 1, IGap ) = GasCoeffsCon( ICoeff, 1 );
					gvis( ICoeff, 1, IGap ) = GasCoeffsVis( ICoeff, 1 );
					gcp( ICoeff, 1, IGap ) = GasCoeffsCp( ICoeff, 1 );
				}
			}

			// Exterior convection coefficient, exterior air temperature and IR radiance
			// of exterior surround. Depend on whether window is interzone (in an interzone
			// wall or exterior (in an exterior wall).

			hcout = HextConvCoeff; // Exterior convection coefficient is passed in from outer routine
			//tsky = SkyTemp + TKelvin

			if ( SurfNumAdj > 0 ) { // Interzone window

				ZoneNumAdj = Surface( SurfNumAdj ).Zone;

				// determine reference air temperature for this surface
				{ auto const SELECT_CASE_var( Surface( SurfNumAdj ).TAirRef );
				if ( SELECT_CASE_var == ZoneMeanAirTemp ) {
					RefAirTemp = MAT( ZoneNumAdj );
					TempEffBulkAir( SurfNumAdj ) = RefAirTemp;
				} else if ( SELECT_CASE_var == AdjacentAirTemp ) {
					RefAirTemp = TempEffBulkAir( SurfNumAdj );
				} else if ( SELECT_CASE_var == ZoneSupplyAirTemp ) {
					// determine ZoneEquipConfigNum for this zone
					ZoneEquipConfigNum = ZoneNumAdj;
					// check whether this zone is a controlled zone or not
					if ( ! Zone( ZoneNumAdj ).IsControlled ) {
						ShowFatalError( "Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone " + Zone( ZoneNum ).Name );
						return;
					}
					// determine supply air conditions
					SumSysMCp = 0.0;
					SumSysMCpT = 0.0;
					for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ZoneEquipConfigNum ).NumInletNodes; ++NodeNum ) {
						NodeTemp = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).Temp;
						MassFlowRate = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).MassFlowRate;
						CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNumAdj ), NodeTemp );
						SumSysMCp += MassFlowRate * CpAir;
						SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
					}
					if ( SumSysMCp > 0.0 ) {
						// a weighted average of the inlet temperatures.
						RefAirTemp = SumSysMCpT / SumSysMCp;
					} else {
						RefAirTemp = NodeTemp;
					}
					TempEffBulkAir( SurfNumAdj ) = RefAirTemp;
				} else {
					// currently set to mean air temp but should add error warning here
					RefAirTemp = MAT( ZoneNumAdj );
					TempEffBulkAir( SurfNumAdj ) = RefAirTemp;
				}}

				tout = RefAirTemp + TKelvin; // outside air temperature

				// Add long-wave radiation from adjacent zone absorbed by glass layer closest to the adjacent zone.

				AbsRadGlassFace( 1 ) += QRadThermInAbs( SurfNumAdj );

				// The IR radiance of this window's "exterior" surround is the IR radiance
				// from surfaces and high-temp radiant sources in the adjacent zone

				Outir = SurfaceWindow( SurfNumAdj ).IRfromParentZone + QHTRadSysSurf( SurfNumAdj ) + QHWBaseboardSurf( SurfNumAdj ) + QSteamBaseboardSurf( SurfNumAdj ) + QElecBaseboardSurf( SurfNumAdj );

			} else { // Exterior window (ExtBoundCond = 0)

				if ( surface.ExtWind ) { // Window is exposed to wind (and possibly rain)
					if ( IsRain ) { // Raining: since wind exposed, outside window surface gets wet
						tout = surface.OutWetBulbTemp + TKelvin;
					} else { // Dry
						tout = surface.OutDryBulbTemp + TKelvin;
					}
				} else { // Window not exposed to wind
					tout = surface.OutDryBulbTemp + TKelvin;
				}
				Ebout = sigma * pow_4( tout );
				Outir = surface.ViewFactorSkyIR * ( AirSkyRadSplit( SurfNum ) * sigma * pow_4( SkyTempKelvin ) + ( 1.0 - AirSkyRadSplit( SurfNum ) ) * Ebout ) + surface.ViewFactorGroundIR * Ebout;

			}

			// Factors used in window layer temperature solution
			if ( ngllayer >= 2 ) {
				A23P = -emis( 3 ) / ( 1.0 - ( 1.0 - emis( 2 ) ) * ( 1.0 - emis( 3 ) ) );
				A32P = emis( 2 ) / ( 1.0 - ( 1.0 - emis( 2 ) ) * ( 1.0 - emis( 3 ) ) );
				A23 = emis( 2 ) * sigma * A23P;
			}

			if ( ngllayer >= 3 ) {
				A45P = -emis( 5 ) / ( 1.0 - ( 1.0 - emis( 4 ) ) * ( 1.0 - emis( 5 ) ) );
				A54P = emis( 4 ) / ( 1.0 - ( 1.0 - emis( 4 ) ) * ( 1.0 - emis( 5 ) ) );
				A45 = emis( 4 ) * sigma * A45P;
			}

			if ( ngllayer == 4 ) {
				A67P = -emis( 7 ) / ( 1.0 - ( 1.0 - emis( 6 ) ) * ( 1.0 - emis( 7 ) ) );
				A76P = emis( 6 ) / ( 1.0 - ( 1.0 - emis( 6 ) ) * ( 1.0 - emis( 7 ) ) );
				A67 = emis( 6 ) * sigma * A67P;

			}

			thetas = 0.0;
			thetasPrev = 0.0;
			fvec = 0.0;
			fjac = 0.0;

			// Calculate window face temperatures

			SolveForWindowTemperatures( SurfNum );

			// Temperature difference across glass layers (for debugging)

			dth1 = thetas( 2 ) - thetas( 1 );
			dth2 = thetas( 4 ) - thetas( 3 );
			dth3 = thetas( 6 ) - thetas( 5 );
			dth4 = thetas( 8 ) - thetas( 7 );

			if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) {
				SurfInsideTemp = thetas( 2 * ngllayer + 2 ) - TKelvin;
				EffShBlEmiss = InterpSlatAng( window.SlatAngThisTS, window.MovableSlats, window.EffShBlindEmiss );
				EffGlEmiss = InterpSlatAng( window.SlatAngThisTS, window.MovableSlats, window.EffGlassEmiss );
				window.EffInsSurfTemp = ( EffShBlEmiss * SurfInsideTemp + EffGlEmiss * ( thetas( 2 * ngllayer ) - TKelvin ) ) / ( EffShBlEmiss + EffGlEmiss );
			} else {
				SurfInsideTemp = thetas( 2 * ngllayer ) - TKelvin;
			}
			if ( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) {
				SurfOutsideTemp = thetas( 2 * ngllayer + 1 ) - TKelvin; // this index looks suspicious (CR 8202)
				//SurfOutsideEmiss = emis(1)  ! this index should be coordinated with previous line
				SurfOutsideEmiss = emis( 2 * ngllayer + 1 ); // fix for CR 8202
			} else {
				SurfOutsideEmiss = emis( 1 );
				SurfOutsideTemp = thetas( 1 ) - TKelvin;
			}

			// Save temperatures for use next time step

			for ( k = 1; k <= nglfacep; ++k ) {
				window.ThetaFace( k ) = thetas( k );
			}

			// Added TH 12/23/2008 for thermochromic windows to save the current TC layer temperature
			if ( locTCFlag ) {
				window.TCLayerTemp = ( thetas( 2 * Construct( ConstrNum ).TCGlassID - 1 ) + thetas( 2 * Construct( ConstrNum ).TCGlassID ) ) / 2 - TKelvin; // degree C
			}
		} //regular window, not BSDF, not EQL

		// Set condensation flag to 1 if condensation expected to occur on the innermost glass face,
		// or, for airflow windows, on either or the two glass faces in the airflow gap
		if ( ! Construct( surface.Construction ).WindowTypeEQL ) {
			InsideGlassTemp = thetas( 2 * ngllayer ) - TKelvin;
			RoomHumRat = ZoneAirHumRat( surface.Zone );
			RoomDewPoint = PsyTdpFnWPb( RoomHumRat, OutBaroPress );
			InsideGlassCondensationFlag( SurfNum ) = 0;
			if ( InsideGlassTemp < RoomDewPoint ) InsideGlassCondensationFlag( SurfNum ) = 1;
			// If airflow window, is there condensation on either glass face of the airflow gap?
			if ( window.AirflowThisTS > 0.0 ) {
				Tleft = thetas( 2 * ngllayer - 2 ) - TKelvin;
				Tright = thetas( 2 * ngllayer - 1 ) - TKelvin;
				if ( window.AirflowSource == AirFlowWindow_Source_IndoorAir ) {
					if ( Tleft < RoomDewPoint || Tright < RoomDewPoint ) InsideGlassCondensationFlag( SurfNum ) = 1;
				} else if ( window.AirflowSource == AirFlowWindow_Source_OutdoorAir ) {
					if ( Tleft < OutDewPointTemp || Tright < OutDewPointTemp ) InsideGlassCondensationFlag( SurfNum ) = 1;
				}
			}

			// Do frame and divider calculation
			if ( window.FrameArea > 0.0 || window.DividerArea > 0.0 ) CalcWinFrameAndDividerTemps( SurfNum, tout, tin, hcout, hcin, Outir, ConstrNum );
			if ( window.FrameArea > 0.0 ) {
				InsideFrameCondensationFlag( SurfNum ) = 0;
				if ( window.FrameTempSurfIn < RoomDewPoint ) InsideFrameCondensationFlag( SurfNum ) = 1;
			}
			if ( window.DividerArea > 0.0 ) {
				InsideDividerCondensationFlag( SurfNum ) = 0;
				if ( window.DividerTempSurfIn < RoomDewPoint ) InsideDividerCondensationFlag( SurfNum ) = 1;
			}
		}
		//update exterior environment surface heat loss reporting
		Tsout = SurfOutsideTemp + TKelvin;
		QdotConvOutRep( SurfNum ) = -surface.Area * hcout * ( Tsout - tout );
		QdotConvOutRepPerArea( SurfNum ) = -hcout * ( Tsout - tout );
		QConvOutReport( SurfNum ) = QdotConvOutRep( SurfNum ) * TimeStepZoneSec;

		Real64 const Tsout_4( pow_4( Tsout ) ); //Tuned To reduce pow calls and redundancies
		Real64 const rad_out_per_area( -SurfOutsideEmiss * sigma * ( ( ( ( 1.0 - AirSkyRadSplit( SurfNum ) ) * surface.ViewFactorSkyIR + surface.ViewFactorGroundIR ) * ( Tsout_4 - pow_4( tout ) ) ) + ( AirSkyRadSplit( SurfNum ) * surface.ViewFactorSkyIR * ( Tsout_4 - pow_4( SkyTempKelvin ) ) ) ) );
		QdotRadOutRep( SurfNum ) = surface.Area * rad_out_per_area;
		QdotRadOutRepPerArea( SurfNum ) = rad_out_per_area;

		QRadOutReport( SurfNum ) = QdotRadOutRep( SurfNum ) * TimeStepZoneSec;

	}

	//****************************************************************************

	void
	WindowHeatBalanceEquations( int const SurfNum ) // Surface number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   February 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Evaluates heat balance functions at each glass face.
		// Also evaluates Jacobian.
		// Currently limited to three glass layers.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Array1D< Real64 > hgap( 5 ); // Gap gas conductance
		Real64 gr; // Gap gas Grashof number
		Real64 con; // Gap gas conductivity
		Real64 pr; // Gap gas Prandtl number
		Real64 nu; // Gap gas Nusselt number

		// FLOW

		// Have to zero fvec each time since LUdecompostion and LUsolution may
		// add values to this array in unexpected places
		fvec = 0.0;

		{ auto const SELECT_CASE_var( ngllayer );

		if ( SELECT_CASE_var == 1 ) { // single pane
			fvec( 1 ) = Outir * emis( 1 ) - emis( 1 ) * sigma * pow_4( thetas( 1 ) ) + scon( 1 ) * ( thetas( 2 ) - thetas( 1 ) ) + hcout * ( tout - thetas( 1 ) ) + AbsRadGlassFace( 1 );
			fvec( 2 ) = Rmir * emis( 2 ) - emis( 2 ) * sigma * pow_4( thetas( 2 ) ) + scon( 1 ) * ( thetas( 1 ) - thetas( 2 ) ) + hcin * ( tin - thetas( 2 ) ) + AbsRadGlassFace( 2 );

		} else if ( SELECT_CASE_var == 2 ) { // double pane
			WindowGasConductance( thetas( 2 ), thetas( 3 ), 1, con, pr, gr );
			NusseltNumber( SurfNum, thetas( 2 ), thetas( 3 ), 1, gr, pr, nu );
			hgap( 1 ) = ( con / gap( 1 ) * nu ) * SurfaceWindow( SurfNum ).EdgeGlCorrFac;

			fvec( 1 ) = Outir * emis( 1 ) - emis( 1 ) * sigma * pow_4( thetas( 1 ) ) + scon( 1 ) * ( thetas( 2 ) - thetas( 1 ) ) + hcout * ( tout - thetas( 1 ) ) + AbsRadGlassFace( 1 );
			Real64 const thetas_2_3_4( pow_4( thetas( 2 ) ) - pow_4( thetas( 3 ) ) );
			fvec( 2 ) = scon( 1 ) * ( thetas( 1 ) - thetas( 2 ) ) + hgap( 1 ) * ( thetas( 3 ) - thetas( 2 ) ) + A23 * thetas_2_3_4 + AbsRadGlassFace( 2 );
			fvec( 3 ) = hgap( 1 ) * ( thetas( 2 ) - thetas( 3 ) ) + scon( 2 ) * ( thetas( 4 ) - thetas( 3 ) ) - A23 * thetas_2_3_4 + AbsRadGlassFace( 3 );
			fvec( 4 ) = Rmir * emis( 4 ) - emis( 4 ) * sigma * pow_4( thetas( 4 ) ) + scon( 2 ) * ( thetas( 3 ) - thetas( 4 ) ) + hcin * ( tin - thetas( 4 ) ) + AbsRadGlassFace( 4 );

		} else if ( SELECT_CASE_var == 3 ) { // Triple Pane
			WindowGasConductance( thetas( 2 ), thetas( 3 ), 1, con, pr, gr );
			NusseltNumber( SurfNum, thetas( 2 ), thetas( 3 ), 1, gr, pr, nu );
			hgap( 1 ) = con / gap( 1 ) * nu * SurfaceWindow( SurfNum ).EdgeGlCorrFac;

			WindowGasConductance( thetas( 4 ), thetas( 5 ), 2, con, pr, gr );
			NusseltNumber( SurfNum, thetas( 4 ), thetas( 5 ), 2, gr, pr, nu );
			hgap( 2 ) = con / gap( 2 ) * nu * SurfaceWindow( SurfNum ).EdgeGlCorrFac;

			Real64 const thetas_2_3_4( pow_4( thetas( 2 ) ) - pow_4( thetas( 3 ) ) );
			Real64 const thetas_4_5_4( pow_4( thetas( 4 ) ) - pow_4( thetas( 5 ) ) );
			fvec( 1 ) = Outir * emis( 1 ) - emis( 1 ) * sigma * pow_4( thetas( 1 ) ) + scon( 1 ) * ( thetas( 2 ) - thetas( 1 ) ) + hcout * ( tout - thetas( 1 ) ) + AbsRadGlassFace( 1 );
			fvec( 2 ) = scon( 1 ) * ( thetas( 1 ) - thetas( 2 ) ) + hgap( 1 ) * ( thetas( 3 ) - thetas( 2 ) ) + A23 * thetas_2_3_4 + AbsRadGlassFace( 2 );
			fvec( 3 ) = hgap( 1 ) * ( thetas( 2 ) - thetas( 3 ) ) + scon( 2 ) * ( thetas( 4 ) - thetas( 3 ) ) - A23 * thetas_2_3_4 + AbsRadGlassFace( 3 );
			fvec( 4 ) = scon( 2 ) * ( thetas( 3 ) - thetas( 4 ) ) + hgap( 2 ) * ( thetas( 5 ) - thetas( 4 ) ) + A45 * thetas_4_5_4 + AbsRadGlassFace( 4 );
			fvec( 5 ) = hgap( 2 ) * ( thetas( 4 ) - thetas( 5 ) ) + scon( 3 ) * ( thetas( 6 ) - thetas( 5 ) ) - A45 * thetas_4_5_4 + AbsRadGlassFace( 5 );
			fvec( 6 ) = Rmir * emis( 6 ) - emis( 6 ) * sigma * pow_4( thetas( 6 ) ) + scon( 3 ) * ( thetas( 5 ) - thetas( 6 ) ) + hcin * ( tin - thetas( 6 ) ) + AbsRadGlassFace( 6 );

		} else if ( SELECT_CASE_var == 4 ) { // Quad Pane
			WindowGasConductance( thetas( 2 ), thetas( 3 ), 1, con, pr, gr );
			NusseltNumber( SurfNum, thetas( 2 ), thetas( 3 ), 1, gr, pr, nu );
			hgap( 1 ) = con / gap( 1 ) * nu * SurfaceWindow( SurfNum ).EdgeGlCorrFac;

			WindowGasConductance( thetas( 4 ), thetas( 5 ), 2, con, pr, gr );
			NusseltNumber( SurfNum, thetas( 4 ), thetas( 5 ), 2, gr, pr, nu );
			hgap( 2 ) = con / gap( 2 ) * nu * SurfaceWindow( SurfNum ).EdgeGlCorrFac;

			WindowGasConductance( thetas( 6 ), thetas( 7 ), 3, con, pr, gr );
			NusseltNumber( SurfNum, thetas( 6 ), thetas( 7 ), 3, gr, pr, nu );
			hgap( 3 ) = con / gap( 3 ) * nu * SurfaceWindow( SurfNum ).EdgeGlCorrFac;

			Real64 const thetas_2_3_4( pow_4( thetas( 2 ) ) - pow_4( thetas( 3 ) ) );
			Real64 const thetas_4_5_4( pow_4( thetas( 4 ) ) - pow_4( thetas( 5 ) ) );
			Real64 const thetas_6_7_4( pow_4( thetas( 6 ) ) - pow_4( thetas( 7 ) ) );
			fvec( 1 ) = Outir * emis( 1 ) - emis( 1 ) * sigma * pow_4( thetas( 1 ) ) + scon( 1 ) * ( thetas( 2 ) - thetas( 1 ) ) + hcout * ( tout - thetas( 1 ) ) + AbsRadGlassFace( 1 );
			fvec( 2 ) = scon( 1 ) * ( thetas( 1 ) - thetas( 2 ) ) + hgap( 1 ) * ( thetas( 3 ) - thetas( 2 ) ) + A23 * thetas_2_3_4 + AbsRadGlassFace( 2 );
			fvec( 3 ) = hgap( 1 ) * ( thetas( 2 ) - thetas( 3 ) ) + scon( 2 ) * ( thetas( 4 ) - thetas( 3 ) ) - A23 * thetas_2_3_4 + AbsRadGlassFace( 3 );
			fvec( 4 ) = scon( 2 ) * ( thetas( 3 ) - thetas( 4 ) ) + hgap( 2 ) * ( thetas( 5 ) - thetas( 4 ) ) + A45 * thetas_4_5_4 + AbsRadGlassFace( 4 );
			fvec( 5 ) = hgap( 2 ) * ( thetas( 4 ) - thetas( 5 ) ) + scon( 3 ) * ( thetas( 6 ) - thetas( 5 ) ) - A45 * thetas_4_5_4 + AbsRadGlassFace( 5 );
			fvec( 6 ) = scon( 3 ) * ( thetas( 5 ) - thetas( 6 ) ) + hgap( 3 ) * ( thetas( 7 ) - thetas( 6 ) ) + A67 * thetas_6_7_4 + AbsRadGlassFace( 6 );
			fvec( 7 ) = hgap( 3 ) * ( thetas( 6 ) - thetas( 7 ) ) + scon( 4 ) * ( thetas( 8 ) - thetas( 7 ) ) - A67 * thetas_6_7_4 + AbsRadGlassFace( 7 );
			fvec( 8 ) = Rmir * emis( 8 ) - emis( 8 ) * sigma * pow_4( thetas( 8 ) ) + scon( 4 ) * ( thetas( 7 ) - thetas( 8 ) ) + hcin * ( tin - thetas( 8 ) ) + AbsRadGlassFace( 8 );
		}}

	}

	//****************************************************************************

	void
	SolveForWindowTemperatures( int const SurfNum ) // Surface number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   July 2000
		//       MODIFIED       Oct 2000, FW: modify edge-of-glass correction to account
		//                       for gap radiative conductance affects
		//                      Feb 2001, FW: add interior or exterior shade to layer
		//                       heat balance calculation.
		//                      Mar 2001, FW: relax error tolerance if MaxIterations reached.
		//                      Jun 2001, FW: add interior or exterior blind
		//                      Nov 2002, FW: add relaxation on face temperatures
		//                       to improve convergence for multipane cases where outer pane
		//                       has high solar absorptance: temp --> 0.5*(temp + previous temp);
		//                       also, increase MaxIterations from 50 to 100.
		//                      Dec 2002, FW: add between-glass shade/blind for double and triple glazing.
		//                      Mar 2003, FW: remove redundant relaxation on radiative conductances
		//                      Mar 2003, FW: increase convergence tolerance from 0.01 to 0.02 to enhance
		//                                    convergence in difficult cases.
		//                      June 2003, FW: correct the expression for convective gain to zone air
		//                       from airflow windows with airflow destination = InsideAir. Previously
		//                       convective gain of air as it passed through gap was used, which is correct
		//                       for airflow source = InsideAir but incorrect for airflow source = OutsideAir.
		//                       Save SurfaceWindow%TAirflowGapOutlet for use in calculating convective heat
		//                       gain to return air when airflow source = InsideAir, destination = ReturnAir.
		//                      Dec 2003, FW: enhance converge for difficult cases by increasing relaxation
		//                       in layer surface temperatures for iterations > MaxIterations/4
		//                      May 2006, RR: add exterior window screen
		//                      January 2009, BG: inserted call to recalc inside face convection inside iteration loop
		//                        per ISO 15099 Section 8.3.2.2
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Evaluates the coefficients Aface and Bface in the system of linear
		// algebraic equations
		//     Sum [Aface(i,j)*thetas(j)] = Bface(i), i = 1,nglfacep, j=1,nglfacep
		// where
		// nglface  = number of glass faces (= 2 * number of glass layers), or, if shade or blind is present,
		// nglgacep = number of glass faces + 2
		// thetas(j) = temperature of face j
		// If an interior, exterior or between-glass shade or blind, or exterior screen is present
		// the face numbering is as follows:
		//   1 to 2*nglface are glass faces, from outside to inside;
		//   2*nglface+1 and 2*nglface+2 are the shade or blind faces, from outside to inside
		// For example, the following diagram shows the face number for an exterior shade, screen or blind
		// on double glazing:
		//     ||   ||   ||
		//    5||6 1||2 3||4
		//     ||   ||   ||
		// bl/sh/sc gl   gl

		// And for a between-glass shade/blind in triple glazing:
		//     ||   ||   ||   ||
		//    1||2 3||4 7||8 5||6
		//     ||   ||   ||   ||
		//     gl   gl  bl/sh gl

		// METHODOLOGY EMPLOYED:
		// The Aface and Bface coefficients are determined by the equations for
		// heat balance at the glass and shade/blind faces. The system of linear equations is solved
		// by LU decomposition.

		// REFERENCES:
		// na
		// Using/Aliasing
		using General::InterpSw;
		using General::InterpSlatAng;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyTdbFnHW;
		using InputProcessor::SameString;
		using ConvectionCoefficients::CalcISO15099WindowIntConvCoeff;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIterations( 100 ); // Maximum allowed number of iterations (increased 9/01 from 15 to 50,
		//   increased 11/02 from 50 to 100)
		Real64 const errtemptol( 0.02 ); // Tolerance on errtemp for convergence (increased from 0.01, 3/4/03)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int ZoneNum; // Zone number corresponding to SurfNum
		int i; // Counter
		static Array1D< Real64 > hgap( 5 ); // Gap gas conductance (W/m2-K) //Tuned Made static
		Real64 gr; // Grashof number of gas in a gap
		Real64 con; // Gap gas conductivity
		Real64 pr; // Gap gas Prandtl number
		Real64 nu; // Gap gas Nusselt number
		static Array1D< Real64 > hr( 10 ); // Radiative conductance (W/m2-K) //Tuned Made static
		Real64 d; // +1 if number of row interchanges is even,
		// -1 if odd (in LU decomposition)
		static Array1D_int indx( 10 ); // Vector of row permutations in LU decomposition //Tuned Made static
		static Array2D< Real64 > Aface( 10, 10 ); // Coefficient in equation Aface*thetas = Bface //Tuned Made static
		static Array1D< Real64 > Bface( 10 ); // Coefficient in equation Aface*thetas = Bface //Tuned Made static

		int iter; // Iteration number
		static Array1D< Real64 > hrprev( 10 ); // Value of hr from previous iteration //Tuned Made static
		Real64 errtemp; // Absolute value of sum of face temperature differences
		//   between iterations, divided by number of faces
		Real64 VGap; // Air velocity in gap between glass and shade/blind (m/s)
		Real64 VAirflowGap; // Air velocity in airflow gap between glass panes (m/s)
		Real64 VGapPrev; // Value of VGap from previous iteration
		Real64 TGapNew; // Average air temp in gap between glass and shade/blind (K)
		Real64 TAirflowGapNew; // Average air temp in airflow gap between glass panes (K)
		Real64 TGapOutlet; // Temperature of air leaving gap between glass and shade/blind (K)
		Real64 TAirflowGapOutlet; // Temperature of air leaving airflow gap between glass panes (K)
		Real64 TAirflowGapOutletC; // Temperature of air leaving airflow gap between glass panes (C)
		static Array1D< Real64 > TGapNewBG( 2 ); // For between-glass shade/blind, average gas temp in gaps on either //Tuned Made static
		//  side of shade/blind (K)
		Real64 hcv; // Convection coefficient from gap glass or shade/blind to gap air (W/m2-K)
		Real64 hcvAirflowGap; // Convection coefficient from airflow gap glass to airflow gap air (W/m2-K)
		Real64 hcvPrev; // Value of hcv from previous iteration
		static Array1D< Real64 > hcvBG( 2 ); // For between-glass shade/blind, convection coefficient from gap glass or //Tuned Made static
		//  shade/blind to gap gas on either side of shade/blind (W/m2-K)
		Real64 ConvHeatFlowNatural; // Convective heat flow from gap between glass and interior shade or blind (W)
		Real64 ConvHeatFlowForced; // Convective heat flow from forced airflow gap (W)
		Real64 ShGlReflFacIR; // Factor for long-wave inter-reflection between shade/blind and adjacent glass
		Real64 RhoGlIR1; // Long-wave reflectance of glass surface facing shade/blind; 1=exterior shade/blind,
		Real64 RhoGlIR2;
		//  2=interior shade/blind
		Real64 RhoShIR1; // Long-wave reflectance of shade/blind surface facing glass; 1=interior shade/blind,
		Real64 RhoShIR2;
		//  2=exterior shade/blind
		Real64 EpsShIR1; // Long-wave emissivity of shade/blind surface facing glass; 1=interior shade/blind,
		Real64 EpsShIR2;
		//  2=exterior shade/blind
		Real64 TauShIR; // Long-wave transmittance of isolated shade/blind
		Real64 sconsh; // shade/blind conductance (W/m2-K)
		int ShadeFlag; // Shading flag
		Real64 ShadeAbsFac1; // Fractions for apportioning absorbed radiation to shade/blind faces
		Real64 ShadeAbsFac2;
		static Array1D< Real64 > AbsRadShadeFace( 2 ); // Solar radiation, short-wave radiation from lights, and long-wave //Tuned Made static
		//  radiation from lights and zone equipment absorbed by faces of shade/blind (W/m2)
		Real64 ShadeArea; // shade/blind area (m2)
		Real64 CondHeatGainGlass; // Conduction through inner glass layer, outside to inside (W)
		Real64 CondHeatGainShade; // Conduction through shade/blind, outside to inside (W)
		Real64 NetIRHeatGainGlass; // Net IR heat gain to zone from shade/blind side of glass when interior
		//  shade/blind is present. Zero if shade/blind has zero IR transmittance (W)
		Real64 NetIRHeatGainShade; // Net IR heat gain to zone from interior shade/blind (W)
		Real64 ConvHeatGainFrZoneSideOfShade; // Convective heat gain to zone from side of interior shade facing zone (W)
		Real64 ConvHeatGainFrZoneSideOfGlass; // Convective heat gain to zone from side of glass facing zone when
		//  no interior shade/blind is present (W)
		Real64 IncidentSolar; // Solar incident on outside of window (W)
		int ConstrNum; // Construction number, bare and with shading device
		int ConstrNumSh;
		Real64 TransDiff; // Diffuse shortwave transmittance
		static Array1D< Real64 > RhoIR( 10 ); // Face IR reflectance //Tuned Made static
		Real64 FacRhoIR25; // Intermediate variable
		Real64 FacRhoIR63; // Intermediate variable
		Real64 RhoIRfp; // Intermediate variable
		Real64 RhoIRbp; // Intermediate variable
		Real64 FacRhoIR2fp; // Intermediate variable
		Real64 FacRhoIR3bp; // Intermediate variable
		Real64 FacRhoIR2fpRhoIR63; // Intermediate variable
		Real64 FacRhoIR3bpRhoIR25; // Intermediate variable
		Real64 FacRhoIR47; // Intermediate variable
		Real64 FacRhoIR85; // Intermediate variable
		Real64 FacRhoIR4fp; // Intermediate variable
		Real64 FacRhoIR5bp; // Intermediate variable
		Real64 FacRhoIR4fpRhoIR85; // Intermediate variable
		Real64 FacRhoIR5bpRhoIR47; // Intermediate variable
		Real64 ConvHeatGainToZoneAir; // Convective heat gain to zone air from window gap airflow (W)
		Real64 TotAirflowGap; // Total volumetric airflow through window gap (m3/s)
		Real64 CpAirOutlet; // Heat capacity of air from window gap (J/kg-K)
		Real64 CpAirZone; // Heat capacity of zone air (J/kg-K)
		Real64 InletAirHumRat; // Humidity ratio of air from window gap entering fan
		//unused REAL(r64)         :: RhoAir                ! Density of air from window gap entering fan (kg/m3)
		//unused REAL(r64)         :: MassFlow              ! Mass flow of air from window gap entering fan (kg/s)
		Real64 ZoneTemp; // Zone air temperature (C)
		int InsideFaceIndex; // intermediate variable for index of inside face in thetas

		iter = 0;
		ConvHeatFlowNatural = 0.0;
		ConvHeatFlowForced = 0.0;
		nglfacep = nglface;
		ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
		ZoneNum = Surface( SurfNum ).Zone;
		AbsRadShadeFace = 0.0;
		TGapNew = 0.0;

		if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn || ShadeFlag == ExtScreenOn ) {
			nglfacep = nglface + 2;
			ShadeAbsFac1 = SurfaceWindow( SurfNum ).ShadeAbsFacFace( 1 );
			ShadeAbsFac2 = SurfaceWindow( SurfNum ).ShadeAbsFacFace( 2 );
			AbsRadShadeFace( 1 ) = ( SurfaceWindow( SurfNum ).ExtBeamAbsByShade + SurfaceWindow( SurfNum ).ExtDiffAbsByShade ) * ShadeAbsFac1 + ( SurfaceWindow( SurfNum ).IntBeamAbsByShade + SurfaceWindow( SurfNum ).IntSWAbsByShade ) * ShadeAbsFac2;
			AbsRadShadeFace( 2 ) = ( SurfaceWindow( SurfNum ).ExtBeamAbsByShade + SurfaceWindow( SurfNum ).ExtDiffAbsByShade ) * ShadeAbsFac2 + ( SurfaceWindow( SurfNum ).IntBeamAbsByShade + SurfaceWindow( SurfNum ).IntSWAbsByShade ) * ShadeAbsFac1;
			if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) AbsRadShadeFace( 2 ) += SurfaceWindow( SurfNum ).IntLWAbsByShade;
			sconsh = scon( ngllayer + 1 );
			TauShIR = tir( nglface + 1 );
			EpsShIR1 = emis( nglface + 1 );
			EpsShIR2 = emis( nglface + 2 );
			RhoShIR1 = max( 0.0, 1.0 - TauShIR - EpsShIR1 );
			RhoShIR2 = max( 0.0, 1.0 - TauShIR - EpsShIR2 );
			if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) {
				RhoGlIR2 = 1.0 - emis( 2 * ngllayer );
				ShGlReflFacIR = 1.0 - RhoGlIR2 * RhoShIR1;
			} else if ( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) {
				RhoGlIR1 = 1.0 - emis( 1 );
				ShGlReflFacIR = 1.0 - RhoGlIR1 * RhoShIR2;
			}
		} // End of check if shade or blind is on

		// Initialize face temperatures.

		StartingWindowTemps( SurfNum, AbsRadShadeFace );

		hcvPrev = 0.0;
		VGapPrev = 0.0;

		// Calculate radiative conductances

		errtemp = errtemptol * 2.0;

		while ( iter < MaxIterations && errtemp > errtemptol ) {

			for ( i = 1; i <= nglfacep; ++i ) {
				hr( i ) = emis( i ) * sigma * pow_3( thetas( i ) );
				// Following line is redundant since thetas is being relaxed;
				// removed by FCW, 3/4/03
				//!fw if ( iter >= 1 ) hr(i) = 0.5*(hrprev(i)+hr(i))
				hrprev( i ) = hr( i );
			}

			// call for new interior film coeff (since it is temperature dependent) if using Detailed inside coef model
			if ( ( ( Surface( SurfNum ).IntConvCoeff == 0 ) && ( Zone( ZoneNum ).InsideConvectionAlgo == ASHRAETARP ) ) || ( Surface( SurfNum ).IntConvCoeff == -2 ) ) {
				// coef model is "detailed" and not prescribed by user
				//need to find inside face index, varies with shade/blind etc.
				if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) {
					InsideFaceIndex = nglfacep;
				} else {
					InsideFaceIndex = nglface;
				}
				CalcISO15099WindowIntConvCoeff( SurfNum, thetas( InsideFaceIndex ) - KelvinConv, tin - KelvinConv );
				hcin = HConvIn( SurfNum );
			}

			Aface = 0.0;
			Bface = 0.0;

			// If interior or exterior shade or blind is present, get heat transfer
			// coefficient from glass and shade/blind to gap between glass and shade/blind,
			// effective gap air temperature, velocity of air in gap and gap outlet temperature.

			if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) {
				ExtOrIntShadeNaturalFlow( SurfNum, iter, VGap, TGapNew, TGapOutlet, hcv, ConvHeatFlowNatural );
				if ( iter >= 1 ) {
					hcv = 0.5 * ( hcvPrev + hcv );
					VGap = 0.5 * ( VGapPrev + VGap );
				}
				hcvPrev = hcv;
				VGapPrev = VGap;
			}

			TAirflowGapOutlet = 0.0;
			// If between-glass shade or blind is not present and this is an airflow window
			// (i.e., with forced airflow in the gap for double glass or in the inner gap for triple glass)
			// get glass-to-air forced convection heat transfer coefficient, average gap air temperature, and
			// convective heat flow from gap.

			if ( ShadeFlag != BGShadeOn && ShadeFlag != BGBlindOn && SurfaceWindow( SurfNum ).AirflowThisTS > 0.0 ) {
				BetweenGlassForcedFlow( SurfNum, iter, VAirflowGap, TAirflowGapNew, TAirflowGapOutlet, hcvAirflowGap, ConvHeatFlowForced );
			}

			// If between-glass shade or blind is present, get convective heat transfer
			// coefficients from glass and shade/blind to the two gaps on either side of the shade/blind.
			// Also get average gas temperature in the two gaps, and, for airflow window, the sum of the
			// convective heat flows from the gaps.

			if ( ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn ) {
				if ( SurfaceWindow( SurfNum ).AirflowThisTS == 0.0 ) { // Natural convection in gaps
					BetweenGlassShadeNaturalFlow( SurfNum, iter, VGap, TGapNewBG, hcvBG );
				} else { // Forced convection in gaps
					BetweenGlassShadeForcedFlow( SurfNum, iter, VGap, TGapNewBG, TAirflowGapOutlet, hcvBG, ConvHeatFlowForced );
				}
			}

			++iter;
			SurfaceWindow( SurfNum ).WindowCalcIterationsRep = iter;

			// Calculations based on number of glass layers
			{ auto const SELECT_CASE_var( ngllayer );

			if ( SELECT_CASE_var == 1 ) {
				Bface( 1 ) = Outir * emis( 1 ) + hcout * tout + AbsRadGlassFace( 1 );
				Bface( 2 ) = Rmir * emis( 2 ) + hcin * tin + AbsRadGlassFace( 2 );

				Aface( 1, 1 ) = hr( 1 ) + scon( 1 ) + hcout;
				Aface( 2, 1 ) = -scon( 1 );
				Aface( 1, 2 ) = -scon( 1 );
				Aface( 2, 2 ) = hr( 2 ) + scon( 1 ) + hcin;

				if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) {
					Bface( 2 ) = Rmir * emis( 2 ) * TauShIR / ShGlReflFacIR + hcv * TGapNew + AbsRadGlassFace( 2 );
					Bface( 3 ) = Rmir * TauShIR * RhoGlIR2 * EpsShIR1 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace( 1 );
					Bface( 4 ) = Rmir * EpsShIR2 + hcin * tin + AbsRadShadeFace( 2 );

					Aface( 2, 2 ) = hr( 2 ) * ( 1 - RhoShIR1 ) / ShGlReflFacIR + scon( 1 ) + hcv;
					Aface( 3, 2 ) = -emis( 2 ) * hr( 3 ) / ShGlReflFacIR;
					Aface( 2, 3 ) = -hr( 2 ) * EpsShIR1 / ShGlReflFacIR;
					Aface( 3, 3 ) = hr( 3 ) * ( 1 - RhoGlIR2 * ( EpsShIR1 + RhoShIR1 ) ) / ShGlReflFacIR + sconsh + hcv;
					Aface( 4, 3 ) = -sconsh;
					Aface( 3, 4 ) = -sconsh;
					Aface( 4, 4 ) = hr( 4 ) + sconsh + hcin;
				}

				if ( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) {
					Bface( 1 ) = Outir * emis( 1 ) * TauShIR / ShGlReflFacIR + hcv * TGapNew + AbsRadGlassFace( 1 );
					Bface( 3 ) = Outir * EpsShIR1 + hcout * tout + AbsRadShadeFace( 1 );
					Bface( 4 ) = Outir * TauShIR * RhoGlIR1 * EpsShIR2 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace( 2 );

					Aface( 1, 1 ) = hr( 1 ) * ( 1 - RhoShIR2 ) / ShGlReflFacIR + scon( 1 ) + hcv;
					Aface( 4, 1 ) = -emis( 1 ) * hr( 4 ) / ShGlReflFacIR;
					Aface( 3, 3 ) = hr( 3 ) + sconsh + hcout;
					Aface( 4, 3 ) = -sconsh;
					Aface( 1, 4 ) = -hr( 1 ) * EpsShIR2 / ShGlReflFacIR;
					Aface( 3, 4 ) = -sconsh;
					Aface( 4, 4 ) = hr( 4 ) * ( 1 - RhoGlIR1 * ( EpsShIR2 + RhoShIR2 ) ) / ShGlReflFacIR + sconsh + hcv;
				}

			} else if ( SELECT_CASE_var == 2 ) {
				WindowGasConductance( thetas( 2 ), thetas( 3 ), 1, con, pr, gr );
				NusseltNumber( SurfNum, thetas( 2 ), thetas( 3 ), 1, gr, pr, nu );
				hgap( 1 ) = con / gap( 1 ) * nu;
				if ( SurfaceWindow( SurfNum ).EdgeGlCorrFac > 1.0 ) { // Edge of glass correction
					hrgap( 1 ) = 0.5 * std::abs( A23 ) * pow_3( thetas( 2 ) + thetas( 3 ) );
					hgap( 1 ) = hgap( 1 ) * SurfaceWindow( SurfNum ).EdgeGlCorrFac + hrgap( 1 ) * ( SurfaceWindow( SurfNum ).EdgeGlCorrFac - 1.0 );
				}

				Bface( 1 ) = Outir * emis( 1 ) + hcout * tout + AbsRadGlassFace( 1 );
				Bface( 2 ) = AbsRadGlassFace( 2 );
				Bface( 3 ) = AbsRadGlassFace( 3 );
				Bface( 4 ) = Rmir * emis( 4 ) + hcin * tin + AbsRadGlassFace( 4 );

				Aface( 1, 1 ) = hr( 1 ) + scon( 1 ) + hcout;
				Aface( 2, 1 ) = -scon( 1 );

				Aface( 1, 2 ) = -scon( 1 );
				Aface( 2, 2 ) = scon( 1 ) + hgap( 1 ) - A23P * hr( 2 );
				Aface( 3, 2 ) = -hgap( 1 ) - A32P * hr( 3 );

				Aface( 2, 3 ) = -hgap( 1 ) + A23P * hr( 2 );
				Aface( 3, 3 ) = hgap( 1 ) + scon( 2 ) + A32P * hr( 3 );
				Aface( 4, 3 ) = -scon( 2 );

				Aface( 3, 4 ) = -scon( 2 );
				Aface( 4, 4 ) = hr( 4 ) + scon( 2 ) + hcin;

				if ( ShadeFlag != BGShadeOn && ShadeFlag != BGBlindOn && SurfaceWindow( SurfNum ).AirflowThisTS > 0.0 ) {
					Bface( 2 ) = AbsRadGlassFace( 2 ) + hcvAirflowGap * TAirflowGapNew;
					Bface( 3 ) = AbsRadGlassFace( 3 ) + hcvAirflowGap * TAirflowGapNew;
					Aface( 2, 2 ) = scon( 1 ) + hcvAirflowGap - A23P * hr( 2 );
					Aface( 3, 2 ) = -A32P * hr( 3 );
					Aface( 2, 3 ) = A23P * hr( 2 );
					Aface( 3, 3 ) = hcvAirflowGap + scon( 2 ) + A32P * hr( 3 );
				}

				if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) {
					Bface( 4 ) = Rmir * emis( 4 ) * TauShIR / ShGlReflFacIR + hcv * TGapNew + AbsRadGlassFace( 4 );
					Bface( 5 ) = Rmir * TauShIR * RhoGlIR2 * EpsShIR1 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace( 1 );
					Bface( 6 ) = Rmir * EpsShIR2 + hcin * tin + AbsRadShadeFace( 2 );

					Aface( 4, 4 ) = hr( 4 ) * ( 1 - RhoShIR1 ) / ShGlReflFacIR + scon( 2 ) + hcv;
					Aface( 5, 4 ) = -emis( 4 ) * hr( 5 ) / ShGlReflFacIR;
					Aface( 4, 5 ) = -hr( 4 ) * EpsShIR1 / ShGlReflFacIR;
					Aface( 5, 5 ) = hr( 5 ) * ( 1 - RhoGlIR2 * ( EpsShIR1 + RhoShIR1 ) ) / ShGlReflFacIR + sconsh + hcv;
					Aface( 6, 5 ) = -sconsh;
					Aface( 5, 6 ) = -sconsh;
					Aface( 6, 6 ) = hr( 6 ) + sconsh + hcin;
				}

				if ( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) {
					Bface( 1 ) = Outir * emis( 1 ) * TauShIR / ShGlReflFacIR + hcv * TGapNew + AbsRadGlassFace( 1 );
					Bface( 5 ) = Outir * EpsShIR1 + hcout * tout + AbsRadShadeFace( 1 );
					Bface( 6 ) = Outir * TauShIR * RhoGlIR1 * EpsShIR2 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace( 2 );

					Aface( 1, 1 ) = hr( 1 ) * ( 1 - RhoShIR2 ) / ShGlReflFacIR + scon( 1 ) + hcv;
					Aface( 6, 1 ) = -emis( 1 ) * hr( 6 ) / ShGlReflFacIR;
					Aface( 5, 5 ) = hr( 5 ) + sconsh + hcout;
					Aface( 6, 5 ) = -sconsh;
					Aface( 1, 6 ) = -hr( 1 ) * EpsShIR2 / ShGlReflFacIR;
					Aface( 5, 6 ) = -sconsh;
					Aface( 6, 6 ) = hr( 6 ) * ( 1 - RhoGlIR1 * ( EpsShIR2 + RhoShIR2 ) ) / ShGlReflFacIR + sconsh + hcv;
				}

				if ( ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn ) {
					for ( i = 1; i <= 6; ++i ) {
						RhoIR( i ) = max( 0.0, 1.0 - tir( i ) - emis( i ) );
					}
					FacRhoIR25 = 1.0 - RhoIR( 2 ) * RhoIR( 5 );
					FacRhoIR63 = 1.0 - RhoIR( 6 ) * RhoIR( 3 );
					Real64 const tir_5_squared( pow_2( tir( 5 ) ) );
					RhoIRfp = RhoIR( 5 ) + tir_5_squared * RhoIR( 3 ) / FacRhoIR63;
					RhoIRbp = RhoIR( 6 ) + tir_5_squared * RhoIR( 2 ) / FacRhoIR25;
					FacRhoIR2fp = 1.0 - RhoIRfp * RhoIR( 2 );
					FacRhoIR3bp = 1.0 - RhoIRbp * RhoIR( 3 );
					FacRhoIR2fpRhoIR63 = FacRhoIR2fp * FacRhoIR63;
					FacRhoIR3bpRhoIR25 = FacRhoIR3bp * FacRhoIR25;
					Aface( 2, 2 ) = scon( 1 ) + hcvBG( 1 ) + hr( 2 ) * ( 1 - RhoIRfp * ( emis( 2 ) + RhoIR( 2 ) ) ) / FacRhoIR2fp;
					Aface( 3, 2 ) = -emis( 2 ) * hr( 3 ) * tir( 5 ) / FacRhoIR2fpRhoIR63;
					Aface( 5, 2 ) = -emis( 2 ) * hr( 5 ) / FacRhoIR2fp;
					Aface( 6, 2 ) = -emis( 2 ) * hr( 6 ) * RhoIR( 3 ) * tir( 5 ) / FacRhoIR2fpRhoIR63;
					Bface( 2 ) = hcvBG( 1 ) * TGapNewBG( 1 ) + AbsRadGlassFace( 2 );
					Aface( 2, 3 ) = -emis( 3 ) * hr( 2 ) * tir( 5 ) / FacRhoIR3bpRhoIR25;
					Aface( 3, 3 ) = scon( 2 ) + hcvBG( 2 ) + hr( 3 ) * ( 1 - RhoIRbp * ( emis( 3 ) + RhoIR( 3 ) ) ) / FacRhoIR3bp;
					Aface( 5, 3 ) = -emis( 3 ) * hr( 5 ) * RhoIR( 2 ) * tir( 5 ) / FacRhoIR3bpRhoIR25;
					Aface( 6, 3 ) = -emis( 3 ) * hr( 6 ) / FacRhoIR3bp;
					Bface( 3 ) = hcvBG( 2 ) * TGapNewBG( 2 ) + AbsRadGlassFace( 3 );
					Aface( 2, 5 ) = -emis( 5 ) * hr( 2 ) / FacRhoIR2fp;
					Aface( 3, 5 ) = -hr( 3 ) * tir( 5 ) * RhoIR( 2 ) * emis( 5 ) / FacRhoIR2fpRhoIR63;
					Aface( 5, 5 ) = sconsh + hcvBG( 1 ) + hr( 5 ) * ( 1 - RhoIR( 2 ) * emis( 5 ) / FacRhoIR2fp );
					Aface( 6, 5 ) = -sconsh - hr( 6 ) * RhoIR( 2 ) * tir( 5 ) * RhoIR( 3 ) * emis( 5 ) / FacRhoIR2fpRhoIR63;
					Bface( 5 ) = hcvBG( 1 ) * TGapNewBG( 1 ) + AbsRadShadeFace( 1 );
					Aface( 2, 6 ) = -hr( 2 ) * tir( 5 ) * RhoIR( 3 ) * emis( 6 ) / FacRhoIR3bpRhoIR25;
					Aface( 3, 6 ) = -emis( 6 ) * hr( 3 ) / FacRhoIR3bp;
					Aface( 5, 6 ) = -sconsh - hr( 5 ) * RhoIR( 3 ) * tir( 5 ) * RhoIR( 2 ) * emis( 6 ) / FacRhoIR3bpRhoIR25;
					Aface( 6, 6 ) = sconsh + hcvBG( 2 ) + hr( 6 ) * ( 1 - RhoIR( 3 ) * emis( 6 ) / FacRhoIR3bp );
					Bface( 6 ) = hcvBG( 2 ) * TGapNewBG( 2 ) + AbsRadShadeFace( 2 );
				}

			} else if ( SELECT_CASE_var == 3 ) {
				WindowGasConductance( thetas( 2 ), thetas( 3 ), 1, con, pr, gr );
				NusseltNumber( SurfNum, thetas( 2 ), thetas( 3 ), 1, gr, pr, nu );
				hgap( 1 ) = con / gap( 1 ) * nu;
				if ( SurfaceWindow( SurfNum ).EdgeGlCorrFac > 1.0 ) { // Edge of glass correction
					hrgap( 1 ) = 0.5 * std::abs( A23 ) * pow_3( thetas( 2 ) + thetas( 3 ) );
					hgap( 1 ) = hgap( 1 ) * SurfaceWindow( SurfNum ).EdgeGlCorrFac + hrgap( 1 ) * ( SurfaceWindow( SurfNum ).EdgeGlCorrFac - 1.0 );
				}

				WindowGasConductance( thetas( 4 ), thetas( 5 ), 2, con, pr, gr );
				NusseltNumber( SurfNum, thetas( 4 ), thetas( 5 ), 2, gr, pr, nu );
				hgap( 2 ) = con / gap( 2 ) * nu;
				if ( SurfaceWindow( SurfNum ).EdgeGlCorrFac > 1.0 ) { // Edge of glass correction
					hrgap( 2 ) = 0.5 * std::abs( A45 ) * pow_3( thetas( 4 ) + thetas( 5 ) );
					hgap( 2 ) = hgap( 2 ) * SurfaceWindow( SurfNum ).EdgeGlCorrFac + hrgap( 2 ) * ( SurfaceWindow( SurfNum ).EdgeGlCorrFac - 1.0 );
				}

				Bface( 1 ) = Outir * emis( 1 ) + hcout * tout + AbsRadGlassFace( 1 );
				Bface( 2 ) = AbsRadGlassFace( 2 );
				Bface( 3 ) = AbsRadGlassFace( 3 );
				Bface( 4 ) = AbsRadGlassFace( 4 );
				Bface( 5 ) = AbsRadGlassFace( 5 );
				Bface( 6 ) = Rmir * emis( 6 ) + hcin * tin + AbsRadGlassFace( 6 );

				Aface( 1, 1 ) = hr( 1 ) + scon( 1 ) + hcout;
				Aface( 2, 1 ) = -scon( 1 );

				Aface( 1, 2 ) = -scon( 1 );
				Aface( 2, 2 ) = scon( 1 ) + hgap( 1 ) - A23P * hr( 2 );
				Aface( 3, 2 ) = -hgap( 1 ) - A32P * hr( 3 );

				Aface( 2, 3 ) = -hgap( 1 ) + A23P * hr( 2 );
				Aface( 3, 3 ) = hgap( 1 ) + scon( 2 ) + A32P * hr( 3 );
				Aface( 4, 3 ) = -scon( 2 );

				Aface( 3, 4 ) = -scon( 2 );
				Aface( 4, 4 ) = scon( 2 ) + hgap( 2 ) - A45P * hr( 4 );
				Aface( 5, 4 ) = -hgap( 2 ) - A54P * hr( 5 );

				Aface( 4, 5 ) = -hgap( 2 ) + A45P * hr( 4 );
				Aface( 5, 5 ) = hgap( 2 ) + scon( 3 ) + A54P * hr( 5 );
				Aface( 6, 5 ) = -scon( 3 );

				Aface( 5, 6 ) = -scon( 3 );
				Aface( 6, 6 ) = hr( 6 ) + scon( 3 ) + hcin;

				if ( ShadeFlag != BGShadeOn && ShadeFlag != BGBlindOn && SurfaceWindow( SurfNum ).AirflowThisTS > 0.0 ) {
					Bface( 4 ) = AbsRadGlassFace( 4 ) + hcvAirflowGap * TAirflowGapNew;
					Bface( 5 ) = AbsRadGlassFace( 5 ) + hcvAirflowGap * TAirflowGapNew;
					Aface( 4, 4 ) = scon( 2 ) + hcvAirflowGap - A45P * hr( 4 );
					Aface( 5, 4 ) = -A54P * hr( 5 );
					Aface( 4, 5 ) = A45P * hr( 4 );
					Aface( 5, 5 ) = hcvAirflowGap + scon( 3 ) + A54P * hr( 5 );
				}

				if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) {
					Bface( 6 ) = Rmir * emis( 6 ) * TauShIR / ShGlReflFacIR + hcv * TGapNew + AbsRadGlassFace( 6 );
					Bface( 7 ) = Rmir * TauShIR * RhoGlIR2 * EpsShIR1 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace( 1 );
					Bface( 8 ) = Rmir * EpsShIR2 + hcin * tin + AbsRadShadeFace( 2 );

					Aface( 6, 6 ) = hr( 6 ) * ( 1 - RhoShIR1 ) / ShGlReflFacIR + scon( 3 ) + hcv;
					Aface( 7, 6 ) = -emis( 6 ) * hr( 7 ) / ShGlReflFacIR;
					Aface( 6, 7 ) = -hr( 6 ) * EpsShIR1 / ShGlReflFacIR;
					Aface( 7, 7 ) = hr( 7 ) * ( 1 - RhoGlIR2 * ( EpsShIR1 + RhoShIR1 ) ) / ShGlReflFacIR + sconsh + hcv;
					Aface( 8, 7 ) = -sconsh;
					Aface( 7, 8 ) = -sconsh;
					Aface( 8, 8 ) = hr( 8 ) + sconsh + hcin;
				}

				if ( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) {
					Bface( 1 ) = Outir * emis( 1 ) * TauShIR / ShGlReflFacIR + hcv * TGapNew + AbsRadGlassFace( 1 );
					Bface( 7 ) = Outir * EpsShIR1 + hcout * tout + AbsRadShadeFace( 1 );
					Bface( 8 ) = Outir * TauShIR * RhoGlIR1 * EpsShIR2 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace( 2 );

					Aface( 1, 1 ) = hr( 1 ) * ( 1 - RhoShIR2 ) / ShGlReflFacIR + scon( 1 ) + hcv;
					Aface( 8, 1 ) = -emis( 1 ) * hr( 8 ) / ShGlReflFacIR;
					Aface( 7, 7 ) = hr( 7 ) + sconsh + hcout;
					Aface( 8, 7 ) = -sconsh;
					Aface( 1, 8 ) = -hr( 1 ) * EpsShIR2 / ShGlReflFacIR;
					Aface( 7, 8 ) = -sconsh;
					Aface( 8, 8 ) = hr( 8 ) * ( 1 - RhoGlIR1 * ( EpsShIR2 + RhoShIR2 ) ) / ShGlReflFacIR + sconsh + hcv;
				}

				if ( ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn ) {
					for ( i = 1; i <= 8; ++i ) {
						RhoIR( i ) = max( 0.0, 1.0 - tir( i ) - emis( i ) );
					}
					FacRhoIR47 = 1 - RhoIR( 4 ) * RhoIR( 7 );
					FacRhoIR85 = 1 - RhoIR( 8 ) * RhoIR( 5 );
					Real64 const tir_7_squared( pow_2( tir( 7 ) ) );
					RhoIRfp = RhoIR( 7 ) + tir_7_squared * RhoIR( 5 ) / FacRhoIR85;
					RhoIRbp = RhoIR( 8 ) + tir_7_squared * RhoIR( 4 ) / FacRhoIR47;
					FacRhoIR4fp = 1 - RhoIRfp * RhoIR( 4 );
					FacRhoIR5bp = 1 - RhoIRbp * RhoIR( 5 );
					FacRhoIR4fpRhoIR85 = FacRhoIR4fp * FacRhoIR85;
					FacRhoIR5bpRhoIR47 = FacRhoIR5bp * FacRhoIR47;
					Aface( 4, 4 ) = scon( 2 ) + hcvBG( 1 ) + hr( 4 ) * ( 1 - RhoIRfp * ( emis( 4 ) + RhoIR( 4 ) ) ) / FacRhoIR4fp;
					Aface( 5, 4 ) = -emis( 4 ) * hr( 5 ) * tir( 7 ) / FacRhoIR4fpRhoIR85;
					Aface( 7, 4 ) = -emis( 4 ) * hr( 7 ) / FacRhoIR4fp;
					Aface( 8, 4 ) = -emis( 4 ) * hr( 8 ) * RhoIR( 5 ) * tir( 7 ) / FacRhoIR4fpRhoIR85;
					Bface( 4 ) = hcvBG( 1 ) * TGapNewBG( 1 ) + AbsRadGlassFace( 4 );
					Aface( 4, 5 ) = -emis( 5 ) * hr( 4 ) * tir( 7 ) / FacRhoIR5bpRhoIR47;
					Aface( 5, 5 ) = scon( 3 ) + hcvBG( 2 ) + hr( 5 ) * ( 1 - RhoIRbp * ( emis( 5 ) + RhoIR( 5 ) ) ) / FacRhoIR5bp;
					Aface( 7, 5 ) = -emis( 5 ) * hr( 7 ) * RhoIR( 4 ) * tir( 7 ) / FacRhoIR5bpRhoIR47;
					Aface( 8, 5 ) = -emis( 5 ) * hr( 8 ) / FacRhoIR5bp;
					Bface( 5 ) = hcvBG( 2 ) * TGapNewBG( 2 ) + AbsRadGlassFace( 5 );
					Aface( 4, 7 ) = -emis( 7 ) * hr( 4 ) / FacRhoIR4fp;
					Aface( 5, 7 ) = -hr( 5 ) * tir( 7 ) * RhoIR( 4 ) * emis( 7 ) / FacRhoIR4fpRhoIR85;
					Aface( 7, 7 ) = sconsh + hcvBG( 1 ) + hr( 7 ) * ( 1 - RhoIR( 4 ) * emis( 7 ) / FacRhoIR4fp );
					Aface( 8, 7 ) = -sconsh - hr( 8 ) * RhoIR( 4 ) * tir( 7 ) * RhoIR( 5 ) * emis( 7 ) / FacRhoIR4fpRhoIR85;
					Bface( 7 ) = hcvBG( 1 ) * TGapNewBG( 1 ) + AbsRadShadeFace( 1 );
					Aface( 4, 8 ) = -hr( 4 ) * tir( 7 ) * RhoIR( 5 ) * emis( 8 ) / FacRhoIR5bpRhoIR47;
					Aface( 5, 8 ) = -emis( 8 ) * hr( 5 ) / FacRhoIR5bp;
					Aface( 7, 8 ) = -sconsh - hr( 7 ) * RhoIR( 5 ) * tir( 7 ) * RhoIR( 4 ) * emis( 8 ) / FacRhoIR5bpRhoIR47;
					Aface( 8, 8 ) = sconsh + hcvBG( 2 ) + hr( 8 ) * ( 1 - RhoIR( 5 ) * emis( 8 ) / FacRhoIR5bp );
					Bface( 8 ) = hcvBG( 2 ) * TGapNewBG( 2 ) + AbsRadShadeFace( 2 );
				}

			} else if ( SELECT_CASE_var == 4 ) {
				WindowGasConductance( thetas( 2 ), thetas( 3 ), 1, con, pr, gr );
				NusseltNumber( SurfNum, thetas( 2 ), thetas( 3 ), 1, gr, pr, nu );
				hgap( 1 ) = con / gap( 1 ) * nu;
				if ( SurfaceWindow( SurfNum ).EdgeGlCorrFac > 1.0 ) { // Edge of glass correction
					hrgap( 1 ) = 0.5 * std::abs( A23 ) * pow_3( thetas( 2 ) + thetas( 3 ) );
					hgap( 1 ) = hgap( 1 ) * SurfaceWindow( SurfNum ).EdgeGlCorrFac + hrgap( 1 ) * ( SurfaceWindow( SurfNum ).EdgeGlCorrFac - 1.0 );
				}

				WindowGasConductance( thetas( 4 ), thetas( 5 ), 2, con, pr, gr );
				NusseltNumber( SurfNum, thetas( 4 ), thetas( 5 ), 2, gr, pr, nu );
				hgap( 2 ) = con / gap( 2 ) * nu;
				if ( SurfaceWindow( SurfNum ).EdgeGlCorrFac > 1.0 ) { // Edge of glass correction
					hrgap( 2 ) = 0.5 * std::abs( A45 ) * pow_3( thetas( 4 ) + thetas( 5 ) );
					hgap( 2 ) = hgap( 2 ) * SurfaceWindow( SurfNum ).EdgeGlCorrFac + hrgap( 2 ) * ( SurfaceWindow( SurfNum ).EdgeGlCorrFac - 1.0 );
				}

				WindowGasConductance( thetas( 6 ), thetas( 7 ), 3, con, pr, gr );
				NusseltNumber( SurfNum, thetas( 6 ), thetas( 7 ), 3, gr, pr, nu );
				hgap( 3 ) = con / gap( 3 ) * nu;
				if ( SurfaceWindow( SurfNum ).EdgeGlCorrFac > 1.0 ) { // Edge of glass correction
					hrgap( 3 ) = 0.5 * std::abs( A67 ) * pow_3( thetas( 6 ) + thetas( 7 ) );
					hgap( 3 ) = hgap( 3 ) * SurfaceWindow( SurfNum ).EdgeGlCorrFac + hrgap( 3 ) * ( SurfaceWindow( SurfNum ).EdgeGlCorrFac - 1.0 );
				}
				Bface( 1 ) = Outir * emis( 1 ) + hcout * tout + AbsRadGlassFace( 1 );
				Bface( 2 ) = AbsRadGlassFace( 2 );
				Bface( 3 ) = AbsRadGlassFace( 3 );
				Bface( 4 ) = AbsRadGlassFace( 4 );
				Bface( 5 ) = AbsRadGlassFace( 5 );
				Bface( 6 ) = AbsRadGlassFace( 6 );
				Bface( 7 ) = AbsRadGlassFace( 7 );
				Bface( 8 ) = Rmir * emis( 8 ) + hcin * tin + AbsRadGlassFace( 8 );

				Aface( 1, 1 ) = hr( 1 ) + scon( 1 ) + hcout;
				Aface( 2, 1 ) = -scon( 1 );

				Aface( 1, 2 ) = -scon( 1 );
				Aface( 2, 2 ) = scon( 1 ) + hgap( 1 ) - A23P * hr( 2 );
				Aface( 3, 2 ) = -hgap( 1 ) - A32P * hr( 3 );

				Aface( 2, 3 ) = -hgap( 1 ) + A23P * hr( 2 );
				Aface( 3, 3 ) = hgap( 1 ) + scon( 2 ) + A32P * hr( 3 );
				Aface( 4, 3 ) = -scon( 2 );

				Aface( 3, 4 ) = -scon( 2 );
				Aface( 4, 4 ) = scon( 2 ) + hgap( 2 ) - A45P * hr( 4 );
				Aface( 5, 4 ) = -hgap( 2 ) - A54P * hr( 5 );

				Aface( 4, 5 ) = -hgap( 2 ) + A45P * hr( 4 );
				Aface( 5, 5 ) = hgap( 2 ) + scon( 3 ) + A54P * hr( 5 );
				Aface( 6, 5 ) = -scon( 3 );

				Aface( 5, 6 ) = -scon( 3 );
				Aface( 6, 6 ) = scon( 3 ) + hgap( 3 ) - A67P * hr( 6 );
				Aface( 7, 6 ) = -hgap( 3 ) - A76P * hr( 7 );

				Aface( 6, 7 ) = -hgap( 3 ) + A67P * hr( 6 );
				Aface( 7, 7 ) = hgap( 3 ) + scon( 4 ) + A76P * hr( 7 );
				Aface( 8, 7 ) = -scon( 4 );

				Aface( 7, 8 ) = -scon( 4 );
				Aface( 8, 8 ) = hr( 8 ) + scon( 4 ) + hcin;

				if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) {
					Bface( 8 ) = Rmir * emis( 8 ) * TauShIR / ShGlReflFacIR + hcv * TGapNew + AbsRadGlassFace( 8 );
					Bface( 9 ) = Rmir * TauShIR * RhoGlIR2 * EpsShIR1 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace( 1 );
					Bface( 10 ) = Rmir * EpsShIR2 + hcin * tin + AbsRadShadeFace( 2 );

					Aface( 8, 8 ) = hr( 8 ) * ( 1 - RhoShIR1 ) / ShGlReflFacIR + scon( 4 ) + hcv;
					Aface( 9, 8 ) = -emis( 8 ) * hr( 9 ) / ShGlReflFacIR;
					Aface( 8, 9 ) = -hr( 8 ) * EpsShIR1 / ShGlReflFacIR;
					Aface( 9, 9 ) = hr( 9 ) * ( 1 - RhoGlIR2 * ( EpsShIR1 + RhoShIR1 ) ) / ShGlReflFacIR + sconsh + hcv;
					Aface( 10, 9 ) = -sconsh;
					Aface( 9, 10 ) = -sconsh;
					Aface( 10, 10 ) = hr( 10 ) + sconsh + hcin;
				}

				if ( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) {
					Bface( 1 ) = Outir * emis( 1 ) * TauShIR / ShGlReflFacIR + hcv * TGapNew + AbsRadGlassFace( 1 );
					Bface( 9 ) = Outir * EpsShIR1 + hcout * tout + AbsRadShadeFace( 1 );
					Bface( 10 ) = Outir * TauShIR * RhoGlIR1 * EpsShIR2 / ShGlReflFacIR + hcv * TGapNew + AbsRadShadeFace( 2 );

					Aface( 1, 1 ) = hr( 1 ) * ( 1 - RhoShIR2 ) / ShGlReflFacIR + scon( 1 ) + hcv;
					Aface( 10, 1 ) = -emis( 1 ) * hr( 10 ) / ShGlReflFacIR;
					Aface( 9, 9 ) = hr( 9 ) + sconsh + hcout;
					Aface( 10, 9 ) = -sconsh;
					Aface( 1, 10 ) = -hr( 1 ) * EpsShIR2 / ShGlReflFacIR;
					Aface( 9, 10 ) = -sconsh;
					Aface( 10, 10 ) = hr( 10 ) * ( 1 - RhoGlIR1 * ( EpsShIR2 + RhoShIR2 ) ) / ShGlReflFacIR + sconsh + hcv;
				}

			} else {
				ShowFatalError( "SolveForWindowTemperatures: Invalid number of Glass Layers=" + TrimSigDigits( ngllayer ) + ", up to 4 allowed." );
			}}

			LUdecomposition( Aface, nglfacep, indx, d ); // Note that these routines change Aface;
			LUsolution( Aface, nglfacep, indx, Bface ); // face temperatures are returned in Bface

			for ( i = 1; i <= nglfacep; ++i ) {
				thetasPrev( i ) = thetas( i );
				if ( iter < MaxIterations / 4 ) {
					thetas( i ) = 0.5 * thetas( i ) + 0.5 * Bface( i );
				} else {
					thetas( i ) = 0.75 * thetas( i ) + 0.25 * Bface( i );
				}
			}

			errtemp = 0.0;
			for ( i = 1; i <= nglfacep; ++i ) {
				errtemp += std::abs( thetas( i ) - thetasPrev( i ) );
			}
			errtemp /= nglfacep;

		}

		// We have reached iteration limit or we have converged. If we have reached the
		// iteration limit the following test relaxes the convergence tolerance.
		// If we have converged (errtemp <= errtemptol) the following test has not effect.

		if ( errtemp < 10 * errtemptol ) {

			// Window heat balance solution has converged.

			// For interior shade, add convective gain from glass/shade gap air flow to zone convective gain;
			// For all cases, get total window heat gain for reporting. See CalcWinFrameAndDividerTemps for
			// contribution of frame and divider.
			IncidentSolar = Surface( SurfNum ).Area * QRadSWOutIncident( SurfNum );
			if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) {
				// Interior shade or blind
				SurfaceWindow( SurfNum ).ConvHeatFlowNatural = ConvHeatFlowNatural;
				// Window heat gain from glazing and shade/blind to zone. Consists of transmitted solar, convection
				//   from air exiting gap, convection from zone-side of shade/blind, net IR to zone from shade and net IR to
				//   zone from the glass adjacent to the shade/blind (zero if shade/blind IR transmittance is zero).
				// Following assumes glazed area = window area (i.e., dividers ignored) in calculating
				//   IR to zone from glass when interior shade/blind is present.
				ShadeArea = Surface( SurfNum ).Area + SurfaceWindow( SurfNum ).DividerArea;
				CondHeatGainShade = ShadeArea * sconsh * ( thetas( nglfacep - 1 ) - thetas( nglfacep ) );
				NetIRHeatGainShade = ShadeArea * EpsShIR2 * ( sigma * pow_4( thetas( nglfacep ) ) - Rmir ) + EpsShIR1 * ( sigma * pow_4( thetas( nglfacep - 1 ) ) - Rmir ) * RhoGlIR2 * TauShIR / ShGlReflFacIR;
				NetIRHeatGainGlass = ShadeArea * ( emis( 2 * ngllayer ) * TauShIR / ShGlReflFacIR ) * ( sigma * pow_4( thetas( 2 * ngllayer ) ) - Rmir );
				ConvHeatGainFrZoneSideOfShade = ShadeArea * hcin * ( thetas( nglfacep ) - tin );
				WinHeatGain( SurfNum ) = WinTransSolar( SurfNum ) + ConvHeatFlowNatural + ConvHeatGainFrZoneSideOfShade + NetIRHeatGainGlass + NetIRHeatGainShade;
				// store components for reporting
				WinGainConvGlazShadGapToZoneRep( SurfNum ) = ConvHeatFlowNatural;
				WinGainConvShadeToZoneRep( SurfNum ) = ConvHeatGainFrZoneSideOfShade;
				WinGainIRGlazToZoneRep( SurfNum ) = NetIRHeatGainGlass;
				WinGainIRShadeToZoneRep( SurfNum ) = NetIRHeatGainShade;
			} else {
				// Interior shade or blind not present; innermost layer is glass
				CondHeatGainGlass = Surface( SurfNum ).Area * scon( ngllayer ) * ( thetas( 2 * ngllayer - 1 ) - thetas( 2 * ngllayer ) );
				NetIRHeatGainGlass = Surface( SurfNum ).Area * emis( 2 * ngllayer ) * ( sigma * pow_4( thetas( 2 * ngllayer ) ) - Rmir );
				ConvHeatGainFrZoneSideOfGlass = Surface( SurfNum ).Area * hcin * ( thetas( 2 * ngllayer ) - tin );
				WinHeatGain( SurfNum ) = WinTransSolar( SurfNum ) + ConvHeatGainFrZoneSideOfGlass + NetIRHeatGainGlass;
				// store components for reporting
				WinGainConvGlazToZoneRep( SurfNum ) = ConvHeatGainFrZoneSideOfGlass;
				WinGainIRGlazToZoneRep( SurfNum ) = NetIRHeatGainGlass;
			}

			// Add convective heat gain from airflow window
			// Note: effect of fan heat on gap outlet temperature is neglected since fan power (based
			// on pressure drop through the gap) is extremely small

			WinGapConvHtFlowRep( SurfNum ) = 0.0;
			WinGapConvHtFlowRepEnergy( SurfNum ) = 0.0;
			TotAirflowGap = SurfaceWindow( SurfNum ).AirflowThisTS * Surface( SurfNum ).Width;
			TAirflowGapOutletC = TAirflowGapOutlet - TKelvin;
			SurfaceWindow( SurfNum ).TAirflowGapOutlet = TAirflowGapOutletC;
			if ( SurfaceWindow( SurfNum ).AirflowThisTS > 0.0 ) {
				WinGapConvHtFlowRep( SurfNum ) = ConvHeatFlowForced;
				WinGapConvHtFlowRepEnergy( SurfNum ) = WinGapConvHtFlowRep( SurfNum ) * TimeStepZoneSec;
				// Add heat from gap airflow to zone air if destination is inside air; save the heat gain to return
				// air in case it needs to be sent to the zone (due to no return air determined in HVAC simulation)
				if ( SurfaceWindow( SurfNum ).AirflowDestination == AirFlowWindow_Destination_IndoorAir || SurfaceWindow( SurfNum ).AirflowDestination == AirFlowWindow_Destination_ReturnAir ) {
					if ( SurfaceWindow( SurfNum ).AirflowSource == AirFlowWindow_Source_IndoorAir ) {
						InletAirHumRat = ZoneAirHumRat( ZoneNum );
					} else { // AirflowSource = outside air
						InletAirHumRat = OutHumRat;
					}
					ZoneTemp = MAT( ZoneNum ); // this should be Tin (account for different reference temps)
					CpAirOutlet = PsyCpAirFnWTdb( InletAirHumRat, TAirflowGapOutletC );
					CpAirZone = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), ZoneTemp );
					ConvHeatGainToZoneAir = TotAirflowGap * ( CpAirOutlet * ( TAirflowGapOutletC ) - CpAirZone * ZoneTemp );
					if ( SurfaceWindow( SurfNum ).AirflowDestination == AirFlowWindow_Destination_IndoorAir ) {
						SurfaceWindow( SurfNum ).ConvHeatGainToZoneAir = ConvHeatGainToZoneAir;
						WinHeatGain( SurfNum ) += ConvHeatGainToZoneAir;
					} else {
						SurfaceWindow( SurfNum ).RetHeatGainToZoneAir = ConvHeatGainToZoneAir;
					}
				}
				// For AirflowDestination = ReturnAir in a controlled (i.e., conditioned) zone with return air, see CalcZoneLeavingConditions
				// for calculation of modification of return-air temperature due to airflow from window gaps into return air.
			}

			// Correct WinHeatGain for interior diffuse shortwave (solar and shortwave from lights) transmitted
			// back out window
			ConstrNum = Surface( SurfNum ).Construction;
			ConstrNumSh = Surface( SurfNum ).ShadedConstruction;
			if ( SurfaceWindow( SurfNum ).StormWinFlag == 1 ) {
				ConstrNum = Surface( SurfNum ).StormWinConstruction;
				ConstrNumSh = Surface( SurfNum ).StormWinShadedConstruction;
			}

			TransDiff = Construct( ConstrNum ).TransDiff; // Default value for TransDiff here
			if ( ShadeFlag <= 0 ) {
				TransDiff = Construct( ConstrNum ).TransDiff;
			} else if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn ) {
				TransDiff = Construct( ConstrNumSh ).TransDiff;
			} else if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn ) {
				TransDiff = InterpSlatAng( SurfaceWindow( SurfNum ).SlatAngThisTS, SurfaceWindow( SurfNum ).MovableSlats, Construct( ConstrNumSh ).BlTransDiff );
			} else if ( ShadeFlag == SwitchableGlazing ) {
				TransDiff = InterpSw( SurfaceWindow( SurfNum ).SwitchingFactor, Construct( ConstrNum ).TransDiff, Construct( ConstrNumSh ).TransDiff );
			}
			WinHeatGain( SurfNum ) -= QS( Surface( SurfNum ).Zone ) * Surface( SurfNum ).Area * TransDiff;
			// shouldn't this be + outward flowing fraction of absorbed SW? -- do not know whose comment this is?  LKL (9/2012)
			WinLossSWZoneToOutWinRep( SurfNum ) = QS( Surface( SurfNum ).Zone ) * Surface( SurfNum ).Area * TransDiff;

			if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn || ShadeFlag == ExtScreenOn ) {
				WinShadingAbsorbedSolar( SurfNum ) = ( SurfaceWindow( SurfNum ).ExtBeamAbsByShade + SurfaceWindow( SurfNum ).ExtDiffAbsByShade ) * ( Surface( SurfNum ).Area + SurfaceWindow( SurfNum ).DividerArea );
				WinShadingAbsorbedSolarEnergy( SurfNum ) = WinShadingAbsorbedSolar( SurfNum ) * TimeStepZoneSec;
			}
			if ( SunIsUp ) {
				WinSysSolTransmittance( SurfNum ) = WinTransSolar( SurfNum ) / ( QRadSWOutIncident( SurfNum ) * ( Surface( SurfNum ).Area + SurfaceWindow( SurfNum ).DividerArea ) + 0.0001 );
				WinSysSolAbsorptance( SurfNum ) = ( QRadSWwinAbsTot( SurfNum ) + WinShadingAbsorbedSolar( SurfNum ) ) / ( QRadSWOutIncident( SurfNum ) * ( Surface( SurfNum ).Area + SurfaceWindow( SurfNum ).DividerArea ) + 0.0001 );
				WinSysSolReflectance( SurfNum ) = 1.0 - WinSysSolTransmittance( SurfNum ) - WinSysSolAbsorptance( SurfNum );
			} else {
				WinSysSolTransmittance( SurfNum ) = 0.0;
				WinSysSolAbsorptance( SurfNum ) = 0.0;
				WinSysSolReflectance( SurfNum ) = 0.0;
			}

			// Save hcv for use in divider calc with interior or exterior shade (see CalcWinFrameAndDividerTemps)
			if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) SurfaceWindow( SurfNum ).ConvCoeffWithShade = hcv;
		} else {
			// No convergence after MaxIterations even with relaxed error tolerance
			ShowSevereError( "Convergence error in SolveForWindowTemperatures for window " + Surface( SurfNum ).Name );
			ShowContinueErrorTimeStamp( "" );

			if ( DisplayExtraWarnings ) {
				//report out temperatures
				for ( i = 1; i <= nglfacep; ++i ) {
					ShowContinueError( "Glazing face index = " + RoundSigDigits( i ) + " ; new temperature =" + RoundSigDigits( thetas( i ) - KelvinConv, 4 ) + "C  ; previous temperature = " + RoundSigDigits( thetasPrev( i ) - KelvinConv, 4 ) + 'C' );
				}

			}

			ShowFatalError( "Program halted because of convergence error in SolveForWindowTemperatures for window " + Surface( SurfNum ).Name );

		}

	}

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
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   December 2000
		//       MODIFIED       June 2001: add window blinds
		//                      May 2006 (RR): add exterior window screens
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Called by SolveForWindowTemperatures for windows that have an interior
		// or exterior blind or shade in place.
		// Solves for air flow in gap between glass and shade/blind.
		// Finds temperature of gap air and coefficient for convective heat transfer
		// from glass to gap air and shade/blind to gap air.

		// METHODOLOGY EMPLOYED:
		// Based on ISO/DIS 15099, "Thermal Performance of Windows, Doors and Shading Devices --
		// Detailed Calculations," 1/12/2000, Chapter 7, "Shading Devices."

		// REFERENCES:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//   air flow or bottom for downward air flow (K)

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int ConstrNumSh; // Shaded construction number
		int MatNumSh; // Material number of shade/blind layer
		int nglassfaces; // Number of glass faces in contruction
		Real64 TGapInlet; // Temperature of air entering glass-shade/blind gap at bottom for upward
		//   air flow or top for downward air flow (K)
		Real64 TGlassFace; // Temperature of glass surface facing glass-shade/blind gap (K)
		Real64 TShadeFace; // Temperature of shade surface facing glass-shade/blind gap (K)
		Real64 hGapStill; // Still-air glass-shade/blind gap conduction/convection coeff (W/m2-K)
		Real64 TGapOld; // Previous-iteration average air temp in glass-shade/blind gap (K)
		Real64 GapHeight; // Vertical length of glass-shade/blind gap (m)
		Real64 GapDepth; // Distance from shade to glass (m)
		Real64 RhoAir; // Density of glass-shade/blind gap air at a temperature of TGapOld (kg/m3)
		Real64 RhoTRef; // Density of glass-shade/blind air at reference temp = KelvinConv (kg/m3)
		Real64 ViscAir; // Viscosity of glass-shade/blind gap air at a temperature of TGapOld (kg/m3)
		Real64 AGap; // Cross sectional area of glass-shade/blind gap (m2); for vertical window, this
		//   is in horizontal plane normal to window.
		Real64 ATopGap; // Area of the top and bottom openings (m2)
		Real64 ABotGap;
		Real64 ALeftGap; // Area of the left and right openings (m2)
		Real64 ARightGap;
		Real64 AHolesGap; // Area of the holes in the shade (assumed homogeneously
		//   distributed) (m2)
		Real64 ATopLRH; // Intermediate variables
		Real64 ABotLRH;
		Real64 AEqInlet; // Equivalent inlet and outlet opening areas (m2)
		Real64 AEqOutlet;
		Real64 Zinlet; // Inlet and outlet pressure loss factors
		Real64 Zoutlet;
		Real64 AVGap; // Coeff. of VGap**2 term in pressure balance equation
		Real64 BVGap; // Coeff. of VGap term in pressure balance equation
		Real64 CVGap; // VGap-independent term in pressure balance equation
		Real64 GapHeightChar; // Characteristic height of the gap air temperature profile (m)
		Real64 TAve; // Average of TGlass and TShade (K)
		//REAL(r64)            :: AirProps(8)         ! Air properties
		int TotGaps; // Glass/glass gaps + glass-shade/blind gap
		Real64 con; // Gap conductivity and derivative
		Real64 gr; // glass-shade/blind gap Grashof number
		Real64 pr; // glass-shade/blind gap Prandtl number
		Real64 nu; // glass-shade/blind gap Nusselt number
		int ShadeFlag; // Shading flag
		int BlNum; // Blind number

		// Air properties
		//               Dens  dDens/dT  Con    dCon/dT   Vis    dVis/dT Prandtl dPrandtl/dT
		//DATA AirProps / 1.29, -0.4d-2, 2.41d-2, 7.6d-5, 1.73d-5, 1.0d-7, 0.72,   1.8d-3  /

		ConstrNumSh = SurfaceWindow( SurfNum ).ShadedConstruction;
		if ( SurfaceWindow( SurfNum ).StormWinFlag == 1 ) ConstrNumSh = Surface( SurfNum ).StormWinShadedConstruction;
		ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
		nglassfaces = 2 * Construct( ConstrNumSh ).TotGlassLayers;
		TotGaps = Construct( ConstrNumSh ).TotGlassLayers;

		if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) { // Interior shade or blind
			MatNumSh = Construct( ConstrNumSh ).LayerPoint( nglassfaces );
			TGapInlet = tin;
			TGlassFace = thetas( nglassfaces );
			TShadeFace = thetas( nglassfaces + 1 );
		} else { // Exterior shade, screen or blind
			MatNumSh = Construct( ConstrNumSh ).LayerPoint( 1 );
			TGapInlet = tout;
			TGlassFace = thetas( 1 );
			TShadeFace = thetas( nglassfaces + 2 );
		}
		TAve = 0.5 * ( TGlassFace + TShadeFace );

		if ( iter == 0 ) {
			TGapOld = 0.5 * ( TAve + TGapInlet );
		} else {
			TGapOld = TGapNew;
		}

		// Conductance of gap between glass and shade assuming gap is sealed
		WindowGasConductance( TGlassFace, TShadeFace, TotGaps, con, pr, gr );
		NusseltNumber( SurfNum, TGlassFace, TShadeFace, TotGaps, gr, pr, nu );
		hGapStill = con / gap( TotGaps ) * nu;

		// For near-horizontal windows (i.e., no more than 5 deg from horizontal) assume
		// there is no air flow thru gap

		if ( std::abs( Surface( SurfNum ).SinTilt ) < 0.0872 ) {
			VGap = 0.0;
			hcv = 2.0 * hGapStill;
			QConvGap = 0.0;
			TGapNew = TAve;
			TGapOutlet = TAve;
			return;
		}

		GapHeight = Surface( SurfNum ).Height;

		if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == ExtScreenOn ) {
			// Shade or Screen on
			GapDepth = Material( MatNumSh ).WinShadeToGlassDist;
			AGap = GapDepth * Surface( SurfNum ).Width;
			ATopGap = Material( MatNumSh ).WinShadeTopOpeningMult * AGap;
			ABotGap = Material( MatNumSh ).WinShadeBottomOpeningMult * AGap;
			ALeftGap = Material( MatNumSh ).WinShadeLeftOpeningMult * GapHeight * GapDepth;
			ARightGap = Material( MatNumSh ).WinShadeRightOpeningMult * GapHeight * GapDepth;
			AHolesGap = Material( MatNumSh ).WinShadeAirFlowPermeability * GapHeight * Surface( SurfNum ).Width;
		} else {
			// Blind on
			BlNum = SurfaceWindow( SurfNum ).BlindNumber;
			GapDepth = Blind( BlNum ).BlindToGlassDist;
			AGap = GapDepth * Surface( SurfNum ).Width;
			ATopGap = Blind( BlNum ).BlindTopOpeningMult * AGap;
			ABotGap = Blind( BlNum ).BlindBottomOpeningMult * AGap;
			ALeftGap = Blind( BlNum ).BlindLeftOpeningMult * GapHeight * GapDepth;
			ARightGap = Blind( BlNum ).BlindRightOpeningMult * GapHeight * GapDepth;
			AHolesGap = SurfaceWindow( SurfNum ).BlindAirFlowPermeability * GapHeight * Surface( SurfNum ).Width;
		}

		RhoAir = AirProps( 1 ) + AirProps( 2 ) * ( TGapOld - TKelvin );
		ViscAir = AirProps( 5 ) + AirProps( 6 ) * ( TGapOld - TKelvin );
		// The factor 12 in the next line is based on the solution of steady laminar flow between fixed
		// parallel plates given in Sec. 6.9.1 of Fundamentals of Fluid Mechanics, Munson/Young/Okishi, Third Edition
		// Update, John Wiley & Sons, 1998; ISO 15099 has 8 for this factor, which is for flow through a tube.
		BVGap = 12.0 * ViscAir * GapHeight / pow_2( GapDepth );
		// Adding 0.000001 and 0.000002 in the following gives ATopLRH = ABotLRH =
		// 0.25*(ALeftGap + ARightGap + AHolesGap) when ABotGap = ATopGap = 0.0 (shade/blind sealed at
		// bottom and top but possibly open at left side, right side and/or in-shade/blind)
		ATopLRH = 0.5 * ( ( ATopGap + 0.000001 ) / ( ABotGap + ATopGap + 0.000002 ) ) * ( ALeftGap + ARightGap + AHolesGap );
		ABotLRH = 0.5 * ( ( ABotGap + 0.000001 ) / ( ABotGap + ATopGap + 0.000002 ) ) * ( ALeftGap + ARightGap + AHolesGap );
		if ( TGapOld > TGapInlet ) {
			AEqInlet = ABotGap + ATopLRH;
			AEqOutlet = ATopGap + ABotLRH;
		} else {
			AEqOutlet = ABotGap + ATopLRH;
			AEqInlet = ATopGap + ABotLRH;
		}
		// Adding 0.000001 in the following gives very large value of Zinlet for AEqInlet = 0 and
		// very large value of Zoutlet for AEqInlet = 0; this gives VGap close to zero, as required
		// when there is no inlet and/or outlet for air. This then reduces to the
		// case of a completely sealed shade, in which hcv = 2*hGapStill and QConvGap = 0.
		Zinlet = pow_2( AGap / ( 0.6 * AEqInlet + 0.000001 ) - 1.0 );
		Zoutlet = pow_2( AGap / ( 0.6 * AEqOutlet + 0.000001 ) - 1.0 );
		AVGap = 0.5 * RhoAir * ( 1 + Zinlet + Zoutlet );
		RhoTRef = AirProps( 1 ) * TKelvin;
		CVGap = RhoTRef * 9.81 * GapHeight * Surface( SurfNum ).SinTilt * ( TGapOld - TGapInlet ) / ( TGapOld * TGapInlet );

		// Solution of quadratic equation in VGap
		VGap = ( std::sqrt( pow_2( BVGap ) + std::abs( 4.0 * AVGap * CVGap ) ) - BVGap ) / ( 2.0 * AVGap );
		hcv = 2.0 * hGapStill + 4.0 * VGap;
		GapHeightChar = RhoAir * 1008.0 * GapDepth * VGap / ( 2.0 * hcv );
		// The following avoids divide by zero and exponential underflow
		if ( GapHeightChar == 0.0 ) {
			TGapOutlet = TAve;
		} else if ( ( GapHeight / GapHeightChar ) > 15.0 ) {
			TGapOutlet = TAve;
		} else {
			TGapOutlet = TAve - ( TAve - TGapInlet ) * std::exp( -GapHeight / GapHeightChar );
		}
		TGapNew = TAve - ( GapHeightChar / GapHeight ) * ( TGapOutlet - TGapInlet );

		// Convective heat flow from gap to room air for interior shade or blind
		if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) {
			RhoAir = AirProps( 1 ) + AirProps( 2 ) * ( TGapNew - TKelvin );
			QConvGap = RhoAir * AGap * VGap * 1008.0 * ( TGapOutlet - TGapInlet );
			// Exclude convection to gap due to divider, if present; divider convection handled
			// separately in CalcWinFrameAndDividerTemps
			QConvGap *= 0.5 * ( 1.0 + Surface( SurfNum ).Area / ( Surface( SurfNum ).Area + SurfaceWindow( SurfNum ).DividerArea ) );
		}

	}

	//****************************************************************************

	void
	BetweenGlassShadeNaturalFlow(
		int const SurfNum, // Surface number
		int const iter, // Iteration number for glass heat balance calculation
		Real64 & VGap, // Gas velocity in gaps (m/s)
		Array1A< Real64 > TGapNew, // Current-iteration average gas temp in gaps (K)
		Array1A< Real64 > hcv // Convection coefficient from gap glass or shade to gap gas (W/m2-K)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   December 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Called by SolveForWindowTemperatures for windows that have a
		// between-glass shade or blind in place.
		// Solves for gas flow in the two gaps on either side of shade/blind.
		// Finds average temperature of gas in the two gaps, and the coefficient
		// for convective heat transfer from glass to gap gas and shade/blind to gap gas
		// for the two gaps. The two gaps are assumed to have the same depth so that the
		// gas velocity due to natural convection is the same in the two gaps.
		// The Between-glass shade/blind is between the two glass layers of double glazing
		// or between the two inner glass layers of triple glazing. The quadruple glazing
		// case is not considered.

		// METHODOLOGY EMPLOYED:
		// Based on ISO/DIS 15099, "Thermal Performance of Windows, Doors and Shading Devices --
		// Detailed Calculations," 1/12/2000, Chapter 7, "Shading Devices."

		// REFERENCES:
		// na

		// Argument array dimensioning
		TGapNew.dim( 2 );
		hcv.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ConstrNumSh; // Shaded construction number
		int MatNumSh; // Material number of shade/blind layer
		int nglassfaces; // Number of glass faces in contruction
		// In the following, "gaps" refer to the gaps on either side of the shade/blind
		Array1D< Real64 > TGlassFace( 2 ); // Temperature of glass surfaces facing gaps (K)
		Array1D< Real64 > TShadeFace( 2 ); // Temperature of shade surfaces facing gaps (K)
		Array1D< Real64 > hGapStill( 2 ); // Still-air conduction/convection coeffs for the gaps (W/m2-K)
		Array1D< Real64 > TGapOld( 2 ); // Previous-iteration average gas temp in gaps (K)
		Real64 GapHeight; // Vertical length of glass-shade/blind gap (m)
		Real64 GapDepth; // Distance from shade/blind to glass; assumed same for both gaps (m)
		Array1D< Real64 > RhoGas( 2 ); // Density of gap gas at a temperature of TGapOld (kg/m3)
		Real64 RhoTRef; // Density of gap gas at reference temp = KelvinConvK (kg/m3)
		Array1D< Real64 > ViscGas( 2 ); // Viscosity of gap gas at a temperature of TGapOld (kg/m3)
		Real64 RhoGasZero; // Gas density at KelvinConvK
		Real64 ViscGasZero; // Gas viscosity at KelvinConvK (not used)
		Real64 AGap; // Cross sectional area of gaps (m2); for vertical window, this
		//   is in horizontal plane normal to window.
		Real64 ATopGap; // Area of the top and bottom openings of shade/blind (m2)
		Real64 ABotGap;
		Real64 ALeftGap; // Area of the left and right openings of shade/blind (m2)
		Real64 ARightGap;
		Real64 AHolesGap; // Area of the holes in the shade/blind (assumed homogeneously
		//   distributed) (m2)
		Real64 ATopLRH; // Intermediate variables
		Real64 ABotLRH;
		Real64 AEqInlet; // Equivalent inlet and outlet opening areas (m2)
		Real64 AEqOutlet;
		Real64 Zinlet; // Inlet and outlet pressure loss factors
		Real64 Zoutlet;
		Real64 AVGap; // Coeff. of VGap**2 term in pressure balance equation
		Real64 BVGap; // Coeff. of VGap term in pressure balance equation
		Real64 CVGap; // VGap-independent term in pressure balance equation
		Array1D< Real64 > GapHeightChar( 2 ); // Characteristic height of the gap gas temperature profile (m)
		Array1D< Real64 > EpsChar( 2 ); // EXP(-GapHeight/GapHeightChar(IGap))
		Array1D< Real64 > TAve( 2 ); // Average of TGlass and TShade for the gaps (K)
		Real64 con; // Gap gas conductivity and derivative
		Real64 gr; // Gap gas Grashof number
		Real64 pr; // Gap gas Prandtl number
		Real64 nu; // Gap gas Nusselt number
		int ShadeFlag; // Shading flag
		int BlNum; // Blind number
		int IGap; // Gap counter; 1 = gap on outer side of shade/blind, 2 = gap on inner side.
		int IGapInc; // Gap increment (0 or 1)

		ConstrNumSh = Surface( SurfNum ).ShadedConstruction;
		ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
		nglassfaces = 2 * Construct( ConstrNumSh ).TotGlassLayers;

		if ( Construct( ConstrNumSh ).TotGlassLayers == 2 ) { // Double glazing
			MatNumSh = Construct( ConstrNumSh ).LayerPoint( 3 );
			IGapInc = 0;
			for ( IGap = 1; IGap <= 2; ++IGap ) {
				TGlassFace( IGap ) = thetas( IGap + 1 );
				TShadeFace( IGap ) = thetas( IGap + 4 );
			}
		} else { // Triple glazing
			MatNumSh = Construct( ConstrNumSh ).LayerPoint( 5 );
			IGapInc = 1;
			for ( IGap = 1; IGap <= 2; ++IGap ) {
				TGlassFace( IGap ) = thetas( IGap + 3 );
				TShadeFace( IGap ) = thetas( IGap + 6 );
			}
		}

		for ( IGap = 1; IGap <= 2; ++IGap ) {
			TAve( IGap ) = 0.5 * ( TGlassFace( IGap ) + TShadeFace( IGap ) );
			if ( iter == 0 ) {
				TGapOld( IGap ) = TAve( IGap );
			} else {
				TGapOld( IGap ) = TGapNew( IGap );
			}
			// Conductance of gaps on either side of shade/blind assuming gaps are sealed
			WindowGasConductance( TGlassFace( IGap ), TShadeFace( IGap ), IGap + IGapInc, con, pr, gr );
			NusseltNumber( SurfNum, TGlassFace( IGap ), TShadeFace( IGap ), IGap + IGapInc, gr, pr, nu );
			hGapStill( IGap ) = con / gap( IGap + IGapInc ) * nu;
		}

		// For near-horizontal windows (i.e., no more than 5 deg from horizontal) assume
		// there is no air flow thru gap

		if ( std::abs( Surface( SurfNum ).SinTilt ) < 0.0872 ) {
			VGap = 0.0;
			for ( IGap = 1; IGap <= 2; ++IGap ) {
				hcv( IGap ) = 2.0 * hGapStill( IGap );
				TGapNew( IGap ) = TAve( IGap );
			}
			return;
		}

		GapHeight = Surface( SurfNum ).Height;
		GapDepth = gap( 1 + IGapInc );
		AGap = GapDepth * Surface( SurfNum ).Width;

		if ( ShadeFlag == BGShadeOn ) {
			// Shade on
			ATopGap = Material( MatNumSh ).WinShadeTopOpeningMult * AGap;
			ABotGap = Material( MatNumSh ).WinShadeBottomOpeningMult * AGap;
			ALeftGap = Material( MatNumSh ).WinShadeLeftOpeningMult * GapHeight * GapDepth;
			ARightGap = Material( MatNumSh ).WinShadeRightOpeningMult * GapHeight * GapDepth;
			AHolesGap = Material( MatNumSh ).WinShadeAirFlowPermeability * GapHeight * Surface( SurfNum ).Width;
		} else {
			// Blind on
			BlNum = SurfaceWindow( SurfNum ).BlindNumber;
			ATopGap = Blind( BlNum ).BlindTopOpeningMult * AGap;
			ABotGap = Blind( BlNum ).BlindBottomOpeningMult * AGap;
			ALeftGap = Blind( BlNum ).BlindLeftOpeningMult * GapHeight * GapDepth;
			ARightGap = Blind( BlNum ).BlindRightOpeningMult * GapHeight * GapDepth;
			AHolesGap = SurfaceWindow( SurfNum ).BlindAirFlowPermeability * GapHeight * Surface( SurfNum ).Width;
		}

		for ( IGap = 1; IGap <= 2; ++IGap ) {
			WindowGasPropertiesAtTemp( TGapOld( IGap ), IGap + IGapInc, RhoGas( IGap ), ViscGas( IGap ) );
		}

		BVGap = 12.0 * ( ViscGas( 1 ) + ViscGas( 2 ) ) * GapHeight / pow_2( GapDepth );
		// Adding 0.000001 and 0.000002 in the following gives ATopLRH = ABotLRH =
		// 0.25*(ALeftGap + ARightGap + AHolesGap) when ABotGap = ATopGap = 0.0 (shade/blind sealed at
		// bottom and top but possibly open at left side, right side and/or in shade/blind)
		ATopLRH = 0.5 * ( ( ATopGap + 0.000001 ) / ( ABotGap + ATopGap + 0.000002 ) ) * ( ALeftGap + ARightGap + AHolesGap );
		ABotLRH = 0.5 * ( ( ABotGap + 0.000001 ) / ( ABotGap + ATopGap + 0.000002 ) ) * ( ALeftGap + ARightGap + AHolesGap );
		AEqInlet = ABotGap + ATopLRH;
		AEqOutlet = ATopGap + ABotLRH;

		// Adding 0.000001 in the following gives very large value of Zinlet for AEqInlet = 0 and
		// very large value of Zoutlet for AEqInlet = 0; this gives VGap close to zero, as required
		// when there is no inlet and/or outlet for air. This then reduces to the
		// case of a completely sealed shade, in which hcv = 2*hGapStill and QConvGap = 0.
		Zinlet = pow_2( AGap / ( 0.6 * AEqInlet + 0.000001 ) - 1.0 );
		Zoutlet = pow_2( AGap / ( 0.6 * AEqOutlet + 0.000001 ) - 1.0 );
		AVGap = 0.5 * ( RhoGas( 1 ) + RhoGas( 2 ) ) * ( 1.0 + Zinlet + Zoutlet );
		WindowGasPropertiesAtTemp( TKelvin, 1 + IGapInc, RhoGasZero, ViscGasZero );
		RhoTRef = RhoGasZero * TKelvin;
		CVGap = RhoTRef * 9.81 * GapHeight * Surface( SurfNum ).SinTilt * ( TGapOld( 1 ) - TGapOld( 2 ) ) / ( TGapOld( 1 ) * TGapOld( 2 ) );

		// Solution of quadratic equation in VGap

		VGap = ( std::sqrt( pow_2( BVGap ) + std::abs( 4 * AVGap * CVGap ) ) - BVGap ) / ( 2 * AVGap );

		for ( IGap = 1; IGap <= 2; ++IGap ) {
			hcv( IGap ) = 2.0 * hGapStill( IGap ) + 4.0 * VGap;
			GapHeightChar( IGap ) = RhoGas( IGap ) * 1008.0 * GapDepth * VGap / ( 2.0 * hcv( IGap ) );
			// The following avoids divide by zero and exponential underflow
			if ( GapHeightChar( IGap ) == 0.0 ) {
				EpsChar( IGap ) = 0.0;
			} else if ( ( GapHeight / GapHeightChar( IGap ) ) > 15.0 ) {
				EpsChar( IGap ) = 0.0;
			} else {
				EpsChar( IGap ) = std::exp( -GapHeight / GapHeightChar( IGap ) );
			}
		}

		TGapNew( 1 ) = TAve( 1 ) - ( TAve( 1 ) - TAve( 2 ) ) * ( GapHeightChar( 1 ) / GapHeight ) * ( 1 - EpsChar( 1 ) ) * ( 1 - EpsChar( 2 ) ) / ( 1 - EpsChar( 1 ) * EpsChar( 2 ) );

		TGapNew( 2 ) = TAve( 2 ) - ( TAve( 2 ) - TAve( 1 ) ) * ( GapHeightChar( 2 ) / GapHeight ) * ( 1 - EpsChar( 1 ) ) * ( 1 - EpsChar( 2 ) ) / ( 1 - EpsChar( 1 ) * EpsChar( 2 ) );

	}

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
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   February 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Called by SolveForWindowTemperatures for "airflow windows",i.e., windows
		// with forced airflow in one of the gaps between layers of glass. Based on
		// the velocity of airflow through gap, finds effective temperature of gap air,
		// convective heat transfer coefficient from glass to gap air,
		// the gap outlet temperature, and the outlet convective heat flow.

		// Called only for double and triple glazing. For triple glazing the airflow
		// is assumed to be between the inner two layers of glass (glass layers 2 and 3).

		// METHODOLOGY EMPLOYED:
		// Based on ISO/DIS 15099, "Thermal Performance of Windows, Doors and Shading Devices --
		// Detailed Calculations"

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using InputProcessor::SameString;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//   air flow or bottom for downward air flow (K)

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ConstrNum; // Construction number of surface
		int NGlass; // Number of glass layers in construction
		int GapNum; // Number of airflow gap
		Real64 TGapInlet; // Temperature of air entering glass-shade/blind gap at bottom for upward
		//   air flow or top for downward air flow (K)
		Real64 TGlassFace1; // Temperature of left-hand glass surface facing airflow gap (K)
		Real64 TGlassFace2; // Temperature of right-hand glass surface facing airflow gap (K)
		Real64 hGapStill; // Still-air gap conduction/convection coeff (W/m2-K)
		Real64 TGapOld; // Previous-iteration average air temp in airflow gap (K)
		Real64 GapHeight; // Vertical length of airflow gap (m)
		Real64 GapDepth; // Thickness of airflow gap (m)
		Real64 RhoAir; // Density of airflow gap air at a temperature of TGapOld (kg/m3)
		Real64 AGap; // Cross sectional area of airflow gap (m2); for vertical window, this
		//   is in horizontal plane normal to window.
		Real64 GapHeightChar; // Characteristic height of the airflow gap air temperature profile (m)
		Real64 TAve; // Average of TGlassFace1 and TGlassFace2 (K)
		//REAL(r64)            :: AirProps(8)         ! Air properties
		Real64 con; // Gap conductivity and derivative
		Real64 gr; // Gap air Grashof number
		Real64 pr; // Gap air Prandtl number
		Real64 nu; // Gap air Nusselt number

		// Air properties
		//               Dens  dDens/dT  Con    dCon/dT   Vis    dVis/dT Prandtl dPrandtl/dT
		//DATA AirProps / 1.29, -0.4d-2, 2.41d-2, 7.6d-5, 1.73d-5, 1.0d-7, 0.72,   1.8d-3  /

		ConstrNum = Surface( SurfNum ).Construction;
		NGlass = Construct( ConstrNum ).TotGlassLayers;
		TGlassFace1 = thetas( 2 * NGlass - 2 );
		TGlassFace2 = thetas( 2 * NGlass - 1 );
		GapNum = NGlass - 1;
		TAve = 0.5 * ( TGlassFace1 + TGlassFace2 );

		if ( SurfaceWindow( SurfNum ).AirflowSource == AirFlowWindow_Source_IndoorAir ) {
			TGapInlet = tin; // Source is inside air
		} else {
			TGapInlet = tout; // Source is outside air
		}

		if ( iter == 0 ) {
			TGapOld = 0.5 * ( TAve + TGapInlet );
		} else {
			TGapOld = TGapNew;
		}

		// Conductance of gap assuming it is sealed
		WindowGasConductance( TGlassFace1, TGlassFace2, GapNum, con, pr, gr );
		NusseltNumber( SurfNum, TGlassFace1, TGlassFace2, GapNum, gr, pr, nu );
		hGapStill = con / gap( GapNum ) * nu;
		GapHeight = Surface( SurfNum ).Height;
		GapDepth = Material( Construct( ConstrNum ).LayerPoint( 2 * NGlass - 2 ) ).Thickness;
		AGap = GapDepth * Surface( SurfNum ).Width;
		VGap = SurfaceWindow( SurfNum ).AirflowThisTS / GapDepth;
		hcv = 2.0 * hGapStill + 4.0 * VGap;
		RhoAir = AirProps( 1 ) + AirProps( 2 ) * ( TGapOld - TKelvin );
		GapHeightChar = RhoAir * 1008.0 * GapDepth * VGap / ( 2.0 * hcv );
		// The following avoids divide by zero and exponential underflow
		if ( GapHeightChar == 0.0 ) {
			TGapOutlet = TAve;
		} else if ( ( GapHeight / GapHeightChar ) > 15.0 ) {
			TGapOutlet = TAve;
		} else {
			TGapOutlet = TAve - ( TAve - TGapInlet ) * std::exp( -GapHeight / GapHeightChar );
		}
		TGapNew = TAve - ( GapHeightChar / GapHeight ) * ( TGapOutlet - TGapInlet );
		// Convective heat flow from gap [W]
		RhoAir = AirProps( 1 ) + AirProps( 2 ) * ( TGapNew - TKelvin );
		QConvGap = RhoAir * AGap * VGap * 1008.0 * ( TGapOutlet - TGapInlet );

	}

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
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   February 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Called by SolveForWindowTemperatures for airflow windows with a
		// between-glass shade or blind over which fan-forced air flows.
		// Based on the air flow velocity (which is assumed to be the same in the
		// gaps on either side of the shade/blind), finds, for each gap: the average
		// air temperature, the shade/blind or glass surface to air convective heat
		// transfer coefficient, the gap outlet temperature, and the outlet convective heat flow.

		// Called only for double and triple glazing. For triple glazing the airflow
		// is assumed to be between the inner two layers of glass (glass layers 2 and 3),
		// between which the shade/blind is located.

		// METHODOLOGY EMPLOYED:
		// Based on ISO/DIS 15099, "Thermal Performance of Windows, Doors and Shading Devices --
		// Detailed Calculations," 1/12/2000, Chapter 7, "Shading Devices."

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;

		// Argument array dimensioning
		TGapNew.dim( 2 );
		hcv.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ConstrNumSh; // Shaded construction number
		int MatNumSh; // Material number of shade/blind layer
		// In the following, "gaps" refer to the gaps on either side of the shade/blind
		Array1D< Real64 > TGlassFace( 2 ); // Temperature of glass surfaces facing gaps (K)
		Array1D< Real64 > TShadeFace( 2 ); // Temperature of shade surfaces facing gaps (K)
		Array1D< Real64 > hGapStill( 2 ); // Still-air conduction/convection coeffs for the gaps (W/m2-K)
		Array1D< Real64 > TGapOld( 2 ); // Previous-iteration average gas temp in gaps (K)
		Real64 GapHeight; // Vertical length of glass-shade/blind gap (m)
		Real64 GapDepth; // Distance from shade/blind to glass; assumed same for both gaps (m)
		Array1D< Real64 > RhoAir( 2 ); // Density of gap air (kg/m3)
		Real64 AGap; // Cross sectional area of each gap (m2); for vertical window, this
		//   is in horizontal plane normal to window.
		Real64 TGapInlet; // Gap inlet air temperature (K)
		Array1D< Real64 > TGapOutlet( 2 ); // Gap outlet air temperature (K)
		Array1D< Real64 > QConvGap( 2 ); // Convective heat flow from each gap (W)
		Array1D< Real64 > GapHeightChar( 2 ); // Characteristic height of the gap air temperature profile (m)
		Array1D< Real64 > TAve( 2 ); // Average of TGlass and TShade for the gaps (K)
		Real64 con; // Gap air conductivity and derivative
		Real64 gr; // Gap air Grashof number
		Real64 pr; // Gap air Prandtl number
		Real64 nu; // Gap air Nusselt number
		int ShadeFlag; // Shading flag
		int IGap; // Gap counter; 1 = gap on outer side of shade/blind, 2 = gap on inner side.
		int IGapInc; // Gap increment; =0, double glass, =1, triple glass
		//REAL(r64)            :: AirProps(8)         ! Air properties

		// Air properties
		//               Dens  dDens/dT  Con    dCon/dT   Vis    dVis/dT Prandtl dPrandtl/dT
		//DATA AirProps / 1.29, -0.4d-2, 2.41d-2, 7.6d-5, 1.73d-5, 1.0d-7, 0.72,   1.8d-3  /

		ConstrNumSh = Surface( SurfNum ).ShadedConstruction;
		ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;

		if ( Construct( ConstrNumSh ).TotGlassLayers == 2 ) { // Double glazing
			MatNumSh = Construct( ConstrNumSh ).LayerPoint( 3 );
			IGapInc = 0;
			for ( IGap = 1; IGap <= 2; ++IGap ) {
				TGlassFace( IGap ) = thetas( IGap + 1 );
				TShadeFace( IGap ) = thetas( IGap + 4 );
			}
		} else { // Triple glazing
			MatNumSh = Construct( ConstrNumSh ).LayerPoint( 5 );
			IGapInc = 1;
			for ( IGap = 1; IGap <= 2; ++IGap ) {
				TGlassFace( IGap ) = thetas( IGap + 3 );
				TShadeFace( IGap ) = thetas( IGap + 6 );
			}
		}

		if ( SurfaceWindow( SurfNum ).AirflowSource == AirFlowWindow_Source_IndoorAir ) {
			TGapInlet = tin;
		} else {
			TGapInlet = tout;
		}

		GapHeight = Surface( SurfNum ).Height;
		GapDepth = gap( 1 + IGapInc );
		AGap = GapDepth * Surface( SurfNum ).Width;
		// Factor of 2 below assumes gaps on either side of shade/blind have same depth
		VGap = SurfaceWindow( SurfNum ).AirflowThisTS / ( 2.0 * GapDepth );

		for ( IGap = 1; IGap <= 2; ++IGap ) {
			TAve( IGap ) = 0.5 * ( TGlassFace( IGap ) + TShadeFace( IGap ) );
			if ( iter == 0 ) {
				TGapOld( IGap ) = TAve( IGap );
			} else {
				TGapOld( IGap ) = TGapNew( IGap );
			}
			// Conductance of gaps on either side of shade/blind assuming gaps are sealed
			WindowGasConductance( TGlassFace( IGap ), TShadeFace( IGap ), IGap + IGapInc, con, pr, gr );
			NusseltNumber( SurfNum, TGlassFace( IGap ), TShadeFace( IGap ), IGap + IGapInc, gr, pr, nu );
			hGapStill( IGap ) = con / gap( IGap + IGapInc ) * nu;
			// Shade/blind or glass surface to air convection coefficient
			hcv( IGap ) = 2.0 * hGapStill( IGap ) + 4.0 * VGap;
			RhoAir( IGap ) = AirProps( 1 ) + AirProps( 2 ) * ( TGapOld( IGap ) - TKelvin );
			hcv( IGap ) = 2.0 * hGapStill( IGap ) + 4.0 * VGap;
			GapHeightChar( IGap ) = RhoAir( IGap ) * 1008.0 * GapDepth * VGap / ( 2.0 * hcv( IGap ) );
			// The following avoids divide by zero and exponential underflow
			if ( GapHeightChar( IGap ) == 0.0 ) {
				TGapOutlet( IGap ) = TAve( IGap );
			} else if ( ( GapHeight / GapHeightChar( IGap ) ) > 15.0 ) {
				TGapOutlet( IGap ) = TAve( IGap );
			} else {
				TGapOutlet( IGap ) = TAve( IGap ) - ( TAve( IGap ) - TGapInlet ) * std::exp( -GapHeight / GapHeightChar( IGap ) );
			}
			TGapNew( IGap ) = TAve( IGap ) - ( GapHeightChar( IGap ) / GapHeight ) * ( TGapOutlet( IGap ) - TGapInlet );
			// Convective heat flow from gap [W]
			RhoAir( IGap ) = AirProps( 1 ) + AirProps( 2 ) * ( TGapNew( IGap ) - TKelvin );
			QConvGap( IGap ) = RhoAir( IGap ) * AGap * VGap * 1008.0 * ( TGapOutlet( IGap ) - TGapInlet );
		}

		QConvTot = QConvGap( 1 ) + QConvGap( 2 );
		TGapOutletAve = 0.5 * ( TGapOutlet( 1 ) + TGapOutlet( 2 ) );

	}

	//****************************************************************************

	void
	LUdecomposition(
		Array2< Real64 > & ajac, // As input: matrix to be decomposed;
		int const n, // Dimension of matrix
		Array1_int & indx, // Vector of row permutations
		Real64 & d // +1 if even number of row interchange is even, -1
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann, adapted from Numerical Recipes
		//       DATE WRITTEN   February 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Performs LU decomposition of a matrix.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//   as output: decomposed matrix
		//   if odd

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i; // Counters
		int j;
		int k;
		int imax; // Temporary variable
		//   as output: decomposed matrix
		static Array1D< Real64 > vv( 10 ); // Stores the implicit scaling of each row //Tuned Made static
		Real64 aamax; // Absolute value of largest element of matrix
		Real64 dum; // Temporary variable
		Real64 sum; // Sum of products of matrix elements

		// FLOW

		assert( n <= 10 ); // vv sizing

		d = 1.0;
		for ( i = 1; i <= n; ++i ) {
			aamax = 0.0;
			for ( j = 1; j <= n; ++j ) {
				if ( std::abs( ajac( j, i ) ) > aamax ) aamax = std::abs( ajac( j, i ) );
			}
			if ( aamax == 0.0 ) ShowFatalError( "Singular matrix in LUdecomposition, window calculations" );
			vv( i ) = 1.0 / aamax;
		}
		for ( j = 1; j <= n; ++j ) {
			for ( i = 1; i <= j - 1; ++i ) {
				sum = ajac( j, i );
				for ( k = 1; k <= i - 1; ++k ) {
					sum -= ajac( k, i ) * ajac( j, k );
				}
				ajac( j, i ) = sum;
			}
			aamax = 0.0;
			for ( i = j; i <= n; ++i ) {
				sum = ajac( j, i );
				for ( k = 1; k <= j - 1; ++k ) {
					sum -= ajac( k, i ) * ajac( j, k );
				}
				ajac( j, i ) = sum;
				dum = vv( i ) * std::abs( sum );
				if ( dum >= aamax ) {
					imax = i;
					aamax = dum;
				}
			}
			if ( j != imax ) {
				for ( k = 1; k <= n; ++k ) {
					dum = ajac( k, imax );
					ajac( k, imax ) = ajac( k, j );
					ajac( k, j ) = dum;
				}
				d = -d;
				vv( imax ) = vv( j );
			}
			indx( j ) = imax;
			if ( ajac( j, j ) == 0.0 ) ajac( j, j ) = rTinyValue;
			if ( j != n ) {
				dum = 1.0 / ajac( j, j );
				for ( i = j + 1; i <= n; ++i ) {
					ajac( j, i ) *= dum;
				}
			}
		}
	}

	//**************************************************************************

	void
	LUsolution(
		Array2< Real64 > const & a, // Matrix and vector in a.x = b;
		int const n, // Dimension of a and b
		Array1_int const & indx, // Vector of row permutations
		Array1< Real64 > & b // Matrix and vector in a.x = b;
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann, adapted from Numerical Recipes
		//       DATE WRITTEN   February 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Solves set of linear equations a.x = b

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//   b is also output as the solution, x
		//   b is also output as the solution, x

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i; // Counters
		int j;
		int ii; // Intermediate variables
		int ll;
		Real64 sum; // Summation variable

		// FLOW

		ii = 0;
		for ( i = 1; i <= n; ++i ) {
			ll = indx( i );
			sum = b( ll );
			b( ll ) = b( i );
			if ( ii != 0 ) {
				for ( j = ii; j <= i - 1; ++j ) {
					sum -= a( j, i ) * b( j );
				}
			} else if ( sum != 0.0 ) {
				ii = i;
			}
			b( i ) = sum;
		}
		for ( i = n; i >= 1; --i ) {
			sum = b( i );
			for ( j = i + 1; j <= n; ++j ) {
				sum -= a( j, i ) * b( j );
			}
			b( i ) = sum / a( i, i );
		}
	}

	//******************************************************************************

	void
	WindowGasConductance(
		Real64 const tleft, // Temperature of gap surface closest to outside (K)
		Real64 const tright, // Temperature of gap surface closest to zone (K)
		int const IGap, // Gap number
		Real64 & con, // Gap gas conductance (W/m2-K)
		Real64 & pr, // Gap gas Prandtl number
		Real64 & gr // Gap gas Grashof number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Adapted by Fred Winkelmann from Window5 subroutine gasses
		//       DATE WRITTEN   September 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Find the coefficient of convective/conductive heat transfer in the gas-filled gap
		// between isothermal solid layers. The gap may be filled with a single gas or a gas mixture.

		// METHODOLOGY EMPLOYED:
		// Based on methodology in Chapter 5 of the July 18, 2001 draft of ISO 15099,
		// "Thermal Performance of Windows, Doors and Shading Devices--Detailed Calculations."
		// The equation numbers below correspond to those in the standard.

		// REFERENCES:
		// Window5 source code; ISO 15099

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const pres( 1.0e5 ); // Gap gas pressure (Pa)
		Real64 const gaslaw( 8314.51 ); // Molar gas constant (J/kMol-K)
		static Real64 const two_sqrt_2( 2.0 * std::sqrt( 2.0 ) );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//Tuned Arrays made static
		int IMix; // Counters of gases in a mixture
		int i;
		int j;
		int NMix; // Number of gases in a mixture
		Real64 molmix; // Molecular weight of mixture
		static Array1D< Real64 > kprime( 10 ); // Monotonic thermal conductivity
		static Array1D< Real64 > kdblprm( 10 ); // Conductivity term accounting for additional energy moved by
		//  the diffusional transport of internal energy in polyatomic gases.
		Real64 kpmix; // Monotonic thermal conductivity of mixture
		Real64 kdpmix;
		static Array1D< Real64 > mukpdwn( 10 ); // Denominator term
		static Array1D< Real64 > kpdown( 10 ); // Denominator terms
		static Array1D< Real64 > kdpdown( 10 );
		Real64 kmix; // For accumulating conductance of gas mixture
		Real64 mumix; // For accumulating viscosity of gas mixture
		Real64 visc( 0.0 ); // Dynamic viscosity of mixture at tmean (g/m-s)
		Real64 cp( 0.0 ); // Specific heat of mixture at tmean (J/m3-K)
		Real64 dens( 0.0 ); // Density of mixture at tmean (kg/m3)
		Real64 cpmixm; // Gives cp when divided by molmix
		Real64 phimup; // Numerator factor
		Real64 downer; // Denominator factor
		Real64 psiup; // Numerator factor
		Real64 psiterm; // Factor
		Real64 phikup; // Numerator factor
		Real64 rhomix; // Density of gas mixture (kg/m3)
		static Array1D< Real64 > frct( 10 ); // Fraction of each gas in a mixture
		static Array1D< Real64 > fvis( 10 ); // Viscosity of each gas in a mixture (g/m-s)
		static Array1D< Real64 > fcon( 10 ); // Conductance of each gas in a mixture (W/m2-K)
		static Array1D< Real64 > fdens( 10 ); // Density of each gas in a mixture (kg/m3)
		static Array1D< Real64 > fcp( 10 ); // Specific heat of each gas in a mixture (J/m3-K)

		NMix = gnmix( IGap ); //Autodesk:Logic Either assert NMix>0 or handle NMix<=0 in logic so that con and locals guar. initialized before use

		for ( IMix = 1; IMix <= NMix; ++IMix ) {
			frct( IMix ) = gfract( IMix, IGap );
		}

		Real64 const tmean( 0.5 * ( tleft + tright ) ); // Average gap gas temperature (K)
		Real64 const tmean_2( pow_2( tmean ) );

		fcon( 1 ) = gcon( 1, 1, IGap ) + gcon( 2, 1, IGap ) * tmean + gcon( 3, 1, IGap ) * tmean_2;
		fvis( 1 ) = gvis( 1, 1, IGap ) + gvis( 2, 1, IGap ) * tmean + gvis( 3, 1, IGap ) * tmean_2;
		fcp( 1 ) = gcp( 1, 1, IGap ) + gcp( 2, 1, IGap ) * tmean + gcp( 3, 1, IGap ) * tmean_2;
		fdens( 1 ) = pres * gwght( 1, IGap ) / ( gaslaw * tmean ); // Density using ideal gas law:
		//  rho=(presure*molecweight)/(gasconst*tmean)

		if ( NMix == 1 ) { // Single gas
			con = fcon( 1 );
			visc = fvis( 1 );
			cp = fcp( 1 );
			dens = fdens( 1 );
		} else if ( NMix > 1 ) { // Multiple gases; calculate mixture properties
			molmix = frct( 1 ) * gwght( 1, IGap ); // initialize eq. 56
			cpmixm = molmix * fcp( 1 ); // initialize eq. 58
			kprime( 1 ) = 3.75 * ( gaslaw / gwght( 1, IGap ) ) * fvis( 1 ); // eq. 67
			kdblprm( 1 ) = fcon( 1 ) - kprime( 1 ); // eq. 67

			// Initialize summations for eqns 60-66
			mumix = 0.0;
			kpmix = 0.0;
			kdpmix = 0.0;
			mukpdwn( 1 ) = 1.0;
			kpdown( 1 ) = 1.0;
			kdpdown( 1 ) = 1.0;

			// Calculate properties of mixture constituents
			for ( i = 2; i <= NMix; ++i ) {
				fcon( i ) = gcon( 1, i, IGap ) + gcon( 2, i, IGap ) * tmean + gcon( 3, i, IGap ) * tmean_2;
				fvis( i ) = gvis( 1, i, IGap ) + gvis( 2, i, IGap ) * tmean + gvis( 3, i, IGap ) * tmean_2;
				fcp( i ) = gcp( 1, i, IGap ) + gcp( 2, i, IGap ) * tmean + gcp( 3, i, IGap ) * tmean_2;
				fdens( i ) = pres * gwght( i, IGap ) / ( gaslaw * tmean );
				molmix += frct( i ) * gwght( i, IGap ); // eq. 56
				cpmixm += frct( i ) * fcp( i ) * gwght( i, IGap ); // eq. 58-59
				kprime( i ) = 3.75 * gaslaw / gwght( i, IGap ) * fvis( i ); // eq. 67
				kdblprm( i ) = fcon( i ) - kprime( i ); // eq. 68
				mukpdwn( i ) = 1.0; // initialize denomonator of eq. 60
				kpdown( i ) = 1.0; // initialize denomonator of eq. 63
				kdpdown( i ) = 1.0; // initialize denomonator of eq. 65
			}

			for ( i = 1; i <= NMix; ++i ) {
				for ( j = 1; j <= NMix; ++j ) {
					// numerator of equation 61
					phimup = pow_2( 1.0 + std::sqrt( fvis( i ) / fvis( j ) ) * root_4( gwght( j, IGap ) / gwght( i, IGap ) ) );
					// denomonator of eq. 61, 64 and 66
					downer = two_sqrt_2 * std::sqrt( 1 + ( gwght( i, IGap ) / gwght( j, IGap ) ) );
					// calculate the denominator of eq. 60
					if ( i != j ) mukpdwn( i ) += phimup / downer * frct( j ) / frct( i );
					// numerator of eq. 64; psiterm is the multiplied term in backets
					psiup = pow_2( 1.0 + std::sqrt( kprime( i ) / kprime( j ) ) * root_4( gwght( i, IGap ) / gwght( j, IGap ) ) );
					psiterm = 1.0 + 2.41 * ( gwght( i, IGap ) - gwght( j, IGap ) ) * ( gwght( i, IGap ) - 0.142 * gwght( j, IGap ) ) / pow_2( gwght( i, IGap ) + gwght( j, IGap ) );
					// using the common denominator, downer, calculate the denominator for eq. 63
					if ( i != j ) kpdown( i ) += psiup * ( psiterm / downer ) * ( frct( j ) / frct( i ) );
					// calculate the numerator of eq. 66
					phikup = pow_2( 1.0 + std::sqrt( kprime( i ) / kprime( j ) ) * root_4( gwght( i, IGap ) / gwght( j, IGap ) ) );
					// using the common denominator, downer, calculate the denomonator for eq. 65
					if ( i != j ) kdpdown( i ) += ( phikup / downer ) * ( frct( j ) / frct( i ) );
				}
				mumix += fvis( i ) / mukpdwn( i ); // eq. 60
				kpmix += kprime( i ) / kpdown( i ); // eq. 63
				kdpmix += kdblprm( i ) / kdpdown( i ); // eq. 65
			}

			// Calculate the density of the mixture assuming an ideal gas
			rhomix = pres * molmix / ( gaslaw * tmean ); // eq. 57
			kmix = kpmix + kdpmix; // eq. 68-a

			// Final mixture properties
			visc = mumix;
			con = kmix;
			dens = rhomix;
			cp = cpmixm / molmix;

		} else {
			assert( false );
		} // End of check if single or multiple gases in gap

		pr = cp * visc / con;
		gr = 9.807 * pow_3( gap( IGap ) ) * std::abs( tleft - tright ) * pow_2( dens ) / ( tmean * pow_2( visc ) );

	}

	//******************************************************************************

	void
	WindowGasPropertiesAtTemp(
		Real64 const tmean, // Temperature of gas in gap (K)
		int const IGap, // Gap number
		Real64 & dens, // Gap gas density at tmean (kg/m3)
		Real64 & visc // Gap gas dynamic viscosity at tmean (g/m-s)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   December 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Finds the density and viscosity of the gas in a gap at a particular temperature.
		// The gap may be filled with a single gas or a gas mixture.
		// Based on Subroutine WindowGasConductance.

		// METHODOLOGY EMPLOYED:
		// See Subr. WindowGasConductance

		// REFERENCES:
		// See Subr. WindowGasConductance

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const pres( 1.0e5 ); // Gap gas pressure (Pa)
		Real64 const gaslaw( 8314.51 ); // Molar gas constant (J/kMol-K)
		static Real64 const two_sqrt_2( 2.0 * std::sqrt( 2.0 ) );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IMix; // Counters of gases in a mixture
		int i;
		int j;
		int NMix; // Number of gases in a mixture
		Real64 molmix; // Molecular weight of mixture
		Array1D< Real64 > mukpdwn( 10 ); // Denominator term
		Real64 mumix; // For accumulating viscosity of gas mixture
		Real64 phimup; // Numerator factor
		Real64 downer; // Denominator factor
		Real64 rhomix; // Density of gas mixture (kg/m3)
		Array1D< Real64 > frct( 10 ); // Fraction of each gas in a mixture
		Array1D< Real64 > fvis( 10 ); // Viscosity of each gas in a mixture (g/m-s)
		Array1D< Real64 > fdens( 10 ); // Density of each gas in a mixture (kg/m3)

		NMix = gnmix( IGap );

		for ( IMix = 1; IMix <= NMix; ++IMix ) {
			frct( IMix ) = gfract( IMix, IGap );
		}

		Real64 const tmean_2( pow_2( tmean ) );
		fvis( 1 ) = gvis( 1, 1, IGap ) + gvis( 2, 1, IGap ) * tmean + gvis( 3, 1, IGap ) * tmean_2;
		fdens( 1 ) = pres * gwght( 1, IGap ) / ( gaslaw * tmean ); // Density using ideal gas law:
		//  rho=(presure*molecweight)/(gasconst*tmean)
		if ( NMix == 1 ) { // Single gas
			visc = fvis( 1 );
			dens = fdens( 1 );
		} else { // Multiple gases; calculate mixture properties
			molmix = frct( 1 ) * gwght( 1, IGap ); // initialize eq. 56

			// Initialize summations for eqns 60-66
			mumix = 0.0;
			mukpdwn( 1 ) = 1.0;

			// Calculate properties of mixture constituents
			for ( i = 2; i <= NMix; ++i ) {
				fvis( i ) = gvis( 1, i, IGap ) + gvis( 2, i, IGap ) * tmean + gvis( 3, i, IGap ) * tmean_2;
				fdens( i ) = pres * gwght( i, IGap ) / ( gaslaw * tmean );
				molmix += frct( i ) * gwght( i, IGap ); // eq. 56
				mukpdwn( i ) = 1.0; // initialize denomonator of eq. 60
			}

			for ( i = 1; i <= NMix; ++i ) {
				for ( j = 1; j <= NMix; ++j ) {
					// numerator of equation 61
					phimup = pow_2( 1.0 + std::sqrt( fvis( i ) / fvis( j ) ) * root_4( gwght( j, IGap ) / gwght( i, IGap ) ) );
					// denomonator of eq. 61, 64 and 66
					downer = two_sqrt_2 * std::sqrt( 1 + ( gwght( i, IGap ) / gwght( j, IGap ) ) );
					// calculate the denominator of eq. 60
					if ( i != j ) mukpdwn( i ) += phimup / downer * frct( j ) / frct( i );
				}
				mumix += fvis( i ) / mukpdwn( i ); // eq. 60
			}

			// Calculate the density of the mixture assuming an ideal gas
			rhomix = pres * molmix / ( gaslaw * tmean ); // eq. 57

			// Final mixture properties
			visc = mumix;
			dens = rhomix;

		} // End of check if single or multiple gases in gap

	}

	//********************************************************************************

	void
	StartingWindowTemps(
		int const SurfNum, // Surface number
		Array1A< Real64 > AbsRadShade // Short-wave radiation absorbed by shade/blind faces
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   January 2000
		//       MODIFIED       March 2003, FW: add rough calc of increase above ambient of
		//                        initial shade/blind temperature when shade/blind deployed
		//                        after having been off.
		//                      Jan 2004, FW: take into account whether storm window was added
		//                        or removed in the current time step.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initializes face temperature distribution prior to iteration

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Argument array dimensioning
		AbsRadShade.dim( 2 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const hrad( 5.3 ); // Typical radiative conductance (W/m2-K)
		Real64 const resgap( 0.21 ); // Typical gap resistance (m2-K/W)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int i; // Face counter
		int ShadeFlag; // Shading flag
		static Array1D< Real64 > rguess( 11 ); // Combined radiative/convective resistance (m2-K/W) of //Tuned Made static
		// inside or outside air film, or gap
		Real64 restot; // Total window resistance including outside
		//   and inside air films (m2-K/W)
		Real64 temdiff; // Inside/outside air temperature difference (K)
		Real64 ressum; // Resistance sum (m2-K/W)
		int StormWinFlagPrevDay; // Previous time step value (day) of storm window flag
		int StormWinFlagThisDay; // Current time step value (day) of storm window flag
		int nglfacePrevDay; // Previous time step value (dya) of number of glass faces (may differ
		//   current time step value, nglface, if storm window was
		//   added or removed during the current time step).

		StormWinFlagPrevDay = SurfaceWindow( SurfNum ).StormWinFlagPrevDay;
		StormWinFlagThisDay = SurfaceWindow( SurfNum ).StormWinFlag;

		if ( BeginEnvrnFlag || ( StormWinFlagThisDay != StormWinFlagPrevDay ) ) {

			// Guess values of glass face temperatures based on a simple resistance-network solution
			// that (1) ignores short- and long-wave radiation (from lights and zone equipment) absorbed
			// by the glass faces, and (2) assumes zero glass resistance. Absorbed solar is also ignored
			// since the tests on BeginEnvrnFlag and storm window transition can be true only at midnight.
			// Interaction with shade or blind, if one of these is present, is ignored. See below for
			// separate calculation of shade/blind temperature.

			rguess( 1 ) = 1.0 / ( hcout + hrad );
			rguess( nglface + 1 ) = 1.0 / ( hcin + hrad );

			for ( i = 2; i <= nglface; i += 2 ) {
				rguess( i ) = 1.0 / scon( i / 2 );
				if ( i < nglface ) rguess( i + 1 ) = resgap;
			}

			restot = 0.0;
			for ( i = 1; i <= nglface + 1; ++i ) {
				restot += rguess( i );
			}

			temdiff = tin - tout;
			if ( std::abs( temdiff ) < 0.5 ) temdiff = 2.0;

			ressum = 0.0;
			for ( i = 1; i <= nglface; ++i ) {
				ressum += rguess( i );
				thetas( i ) = ( ressum / restot ) * temdiff + tout;
			}

		} else {
			// Use previous time step values
			for ( i = 1; i <= nglface; ++i ) {
				thetas( i ) = SurfaceWindow( SurfNum ).ThetaFace( i );
			}

		}

		// Initialize face temperatures of shade or blind, if present

		ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
		if ( SurfaceWindow( SurfNum ).ExtIntShadePrevTS == IntShadeOn || SurfaceWindow( SurfNum ).ExtIntShadePrevTS == IntBlindOn || SurfaceWindow( SurfNum ).ExtIntShadePrevTS == ExtShadeOn || SurfaceWindow( SurfNum ).ExtIntShadePrevTS == ExtBlindOn || SurfaceWindow( SurfNum ).ExtIntShadePrevTS == BGShadeOn || SurfaceWindow( SurfNum ).ExtIntShadePrevTS == BGBlindOn ) {
			// Shade or blind is on during the previous TS; use previous-TS values of shade/blind face temps.
			// Note that if shade or blind is NOT on in the current TS the following two
			// temperature values, although calculated here, are not used. The shade/blind face numbers
			// during the previous time step depend on whether a storm window glass layer was added to
			// or removed from the window during the current time step.
			nglfacePrevDay = nglface;
			if ( StormWinFlagPrevDay == 0 && StormWinFlagThisDay == 1 ) nglfacePrevDay = nglface - 2;
			if ( StormWinFlagPrevDay == 1 && StormWinFlagThisDay == 0 ) nglfacePrevDay = nglface + 2;
			thetas( nglface + 1 ) = SurfaceWindow( SurfNum ).ThetaFace( nglfacePrevDay + 1 );
			thetas( nglface + 2 ) = SurfaceWindow( SurfNum ).ThetaFace( nglfacePrevDay + 2 );
		} else {
			// No shade or blind previous time step; guess starting values of shade/blind
			// taking into account short- and long-wave radiation (from solar, lights and zone equipment)
			// absorbed by shade/blind faces. Face temps are assumed to be the same and
			// equal to shade/blind temp. For interior shade/blind, air temp on either side is
			// assumed to be the same and equal to tin; for exterior blind it is assumed to be
			// equal to tout. For between-glass shade/blind it is assumed to be equal to the
			// average temperature of the adjacent glass faces.

			if ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) {
				thetas( nglface + 1 ) = tin + ( AbsRadShade( 1 ) + AbsRadShade( 2 ) ) / ( 2 * ( hcin + hrad ) );
				thetas( nglface + 2 ) = thetas( nglface + 1 );
			}
			if ( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn ) {
				thetas( nglface + 1 ) = tout + ( AbsRadShade( 1 ) + AbsRadShade( 2 ) ) / ( 2 * ( hcout + hrad ) );
				thetas( nglface + 2 ) = thetas( nglface + 1 );
			}
			if ( ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn ) {
				// Between-glass shade/blind allowed only for double and triple glazing.
				// The factor 16.0 below is based on a combined convective/radiative heat transfer
				// coefficient on either side of the shade/blind of 8.0 W/m2-K -- about 1.4 Btu/h-ft2-F.
				if ( nglface == 4 ) { // double glazing
					thetas( nglface + 1 ) = 0.5 * ( thetas( 2 ) + thetas( 3 ) ) + ( AbsRadShade( 1 ) + AbsRadShade( 2 ) ) / 16.0;
					thetas( nglface + 2 ) = thetas( nglface + 1 );
				} else { // triple glazing
					thetas( nglface + 1 ) = 0.5 * ( thetas( 4 ) + thetas( 5 ) ) + ( AbsRadShade( 1 ) + AbsRadShade( 2 ) ) / 16.0;
					thetas( nglface + 2 ) = thetas( nglface + 1 );
				}
			}
		}

	}

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
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Adapted by Fred Winkelmann from Window5 subroutine nusselt
		//       DATE WRITTEN   September 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Finds the Nusselt number for gas-filled gaps between isothermal solid layers.
		// The gap may be filled with a single gas or a gas mixture.

		// METHODOLOGY EMPLOYED:
		// Based on methodology in Chapter 5 of the July 18, 2001 draft of ISO 15099,
		// "Thermal Performance of Windows, Doors and Shading Devices--Detailed Calculations."
		// The equation numbers below correspond to those in the standard.

		// REFERENCES:
		// Window5 source code; ISO 15099

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 asp; // Aspect ratio: window height to gap width
		Real64 ra; // Rayleigh number
		Real64 gnu901; // Nusselt number temporary variables for
		Real64 gnu902;
		Real64 gnu90;
		Real64 gnu601;
		Real64 gnu602; // different tilt and Ra ranges
		Real64 gnu60;
		Real64 gnu601a;
		Real64 gnua;
		Real64 gnub;
		Real64 cra; // Temporary variables
		Real64 a;
		Real64 b;
		Real64 g;
		Real64 ang;

		if ( SurfNum > 0 ) {
			asp = Surface( SurfNum ).Height / gap( IGap );
		} else { // SurfNum = 0 when NusseltNumber is called from CalcNominalWindowCond, which applies to a
			// particular construction. So window height is not known and we assume 5 ft (1.524 m)
			asp = 1.524 / gap( IGap );
		}

		tiltr = tilt * DegToRadians;
		ra = gr * pr;
		//!fw if (ra > 2.0e6): error that outside range of Rayleigh number?

		if ( ra <= 1.0e4 ) gnu901 = 1.0 + 1.7596678e-10 * std::pow( ra, 2.2984755 ); // eq. 51
		if ( ra > 1.0e4 && ra <= 5.0e4 ) gnu901 = 0.028154 * std::pow( ra, 0.4134 ); // eq. 50
		if ( ra > 5.0e4 ) gnu901 = 0.0673838 * std::pow( ra, 1.0 / 3.0 ); // eq. 49

		gnu902 = 0.242 * std::pow( ra / asp, 0.272 ); // eq. 52
		gnu90 = max( gnu901, gnu902 );

		if ( tso > tsi ) { // window heated from above
			gnu = 1.0 + ( gnu90 - 1.0 ) * std::sin( tiltr ); // eq. 53
		} else { // window heated from below
			if ( tilt >= 60.0 ) {
				if ( ra >= 0.001 ) {
					g = 0.5 * std::pow( 1.0 + std::pow( ra / 3160.0, 20.6 ), -0.1 ); // eq. 47
				} else {
					g = 0.5;
				}
				gnu601a = 1.0 + pow_7( 0.0936 * std::pow( ra, 0.314 ) / ( 1.0 + g ) ); // eq. 45
				gnu601 = std::pow( gnu601a, 0.142857 );

				// For any aspect ratio
				gnu602 = ( 0.104 + 0.175 / asp ) * std::pow( ra, 0.283 ); // eq. 46
				gnu60 = max( gnu601, gnu602 );

				// linear interpolation for layers inclined at angles between 60 and 90 deg
				gnu = ( ( 90.0 - tilt ) * gnu60 + ( tilt - 60.0 ) * gnu90 ) / 30.0;
			}
			if ( tilt < 60.0 ) { // eq. 42
				cra = ra * std::cos( tiltr );
				a = 1.0 - 1708.0 / cra;
				b = std::pow( cra / 5830.0, 0.33333 ) - 1.0;
				gnua = ( std::abs( a ) + a ) / 2.0;
				gnub = ( std::abs( b ) + b ) / 2.0;
				ang = 1708.0 * std::pow( std::sin( 1.8 * tiltr ), 1.6 );
				gnu = 1.0 + 1.44 * gnua * ( 1.0 - ang / cra ) + gnub;
			}
		}
	}

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
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   January 2000
		//       MODIFIED       5 June 2003, FCW: modify to correspond to WINDOW 4 and WINDOW 5.
		//                      Original routine was based on the method in E.U. Finlayson et al,
		//                      "WINDOW 4.0: Documentation of Calculation Procedures," LBL-33943,
		//                      July 1993, which is not used in either WINDOW 4 or WINDOW 5.
		//                      The current routine is based on ASHRAE Handbook of Fundamentals,
		//                      2001, pp. 30.20-23, "Optical Properties of Single Glazing Layers."
		//                      Original routine underpredicted transmittance at angles of
		//                      incidence > 60 degrees.
		//                      June 2009.  Brent Griffith.  add simple window correlation
		//                                   newer model from LBNL windows group 5/15/2009
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// For a single glazing layer, calculate transmittance and reflectance at an arbitrary
		// angle of incidence given transmittance and reflectance at zero incidence angle.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// ASHRAE Handbook of Fundamentals, 2001, pp. 30.20-23,
		// "Optical Properties of Single Glazing Layers."

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 tfp1; // Transmittance at cs for each polarization
		Real64 tfp2;
		Real64 rfp1; // Front reflectance at cs for each polarization
		Real64 rfp2;
		Real64 rbp1; // Back reflectance at cs for each polarization
		Real64 rbp2;
		Real64 betaf; // Intermediate variables
		Real64 betab;
		Real64 r0f;
		Real64 r0b;
		Real64 abf;
		Real64 abb;
		Real64 ngf; // Front and back index of refraction
		Real64 ngb;
		Real64 cgf; // Intermediate variables
		Real64 cgb;
		Real64 rpf1; // Front and back air/glass interface reflectivity
		Real64 rpb1;
		Real64 tpf1;
		Real64 tpb1;
		//  and transmittivity for first polarization
		Real64 rpf2; // Front and back air/glass interface reflectivity
		Real64 rpb2;
		Real64 tpf2;
		Real64 tpb2;
		//  and transmittivity for second polarization
		Real64 tcl; // Transmittance and reflectance for clear glass
		Real64 rcl;
		Real64 tbnz; // Transmittance and reflectance for bronze glass
		Real64 rbnz;
		Real64 expmabfdivcgf;
		Real64 expm2abfdivcgf;
		Real64 expmabbdivcgb;
		Real64 TransCurveA; // result for curve A for Transmission as a function of angle
		Real64 TransCurveB; // result for curve B for Transmission as a function of angle
		Real64 TransCurveC; // result for curve C for Transmission as a function of angle
		Real64 TransCurveD; // result for curve D for Transmission as a function of angle
		Real64 TransCurveE; // result for curve E for Transmission as a function of angle
		Real64 TransCurveF; // result for curve F for Transmission as a function of angle
		Real64 TransCurveG; // result for curve G for Transmission as a function of angle
		Real64 TransCurveH; // result for curve H for Transmission as a function of angle
		Real64 TransCurveI; // result for curve I for Transmission as a function of angle
		Real64 TransCurveJ; // result for curve J for Transmission as a function of angle
		Real64 ReflectCurveA; // result for curve A for Reflectance as a function of angle
		Real64 ReflectCurveB; // result for curve B for Reflectance as a function of angle
		Real64 ReflectCurveC; // result for curve C for Reflectance as a function of angle
		Real64 ReflectCurveD; // result for curve D for Reflectance as a function of angle
		Real64 ReflectCurveE; // result for curve E for Reflectance as a function of angle
		Real64 ReflectCurveF; // result for curve F for Reflectance as a function of angle
		Real64 ReflectCurveG; // result for curve G for Reflectance as a function of angle
		Real64 ReflectCurveH; // result for curve H for Reflectance as a function of angle
		Real64 ReflectCurveI; // result for curve I for Reflectance as a function of angle
		Real64 ReflectCurveJ; // result for curve J for Reflectance as a function of angle

		Real64 TransCurveFGHI; // average of curves F, G, H, and I
		Real64 ReflectCurveFGHI; // average of curves F, G, H, and I
		Real64 TransCurveFH; // average of curves F and H
		Real64 ReflectCurveFH; // average of curves F and H
		Real64 TransCurveBDCD; // average of curves B, D, C, and D (again)
		Real64 ReflectCurveBDCD; // average of curves B, D, C, and D (again)
		Real64 TransTmp( 0.0 ); // temporary value for normalized transmission (carry out of if blocks)
		Real64 ReflectTmp( 0.0 ); // temporary value for normalized reflectance (carry out of if blocks)
		Real64 testval; // temporary value for calculations
		Real64 tmp1; // temporary value for calculations
		Real64 tmp2; // temporary value for calculations
		Real64 tmp3; // temporary value for calculations
		Real64 tmp4; // temporary value for calculations
		Real64 tmp5; // temporary value for calculations
		Real64 tmp6; // temporary value for calculations
		Real64 tmp7; // temporary value for calculations
		Real64 tmp8; // temporary value for calculations
		Real64 tmp9; // temporary value for calculations
		// FLOW

		if ( SimpleGlazingSystem ) { // use alternate angular dependence model for block model of simple glazing input

			Real64 const cs_2( pow_2( cs ) );
			Real64 const cs_3( pow_3( cs ) );
			Real64 const cs_4( pow_4( cs ) );
			TransCurveA = 1.4703E-02 * cs_4 + 1.4858 * cs_3 - 3.852 * cs_2 + 3.3549 * cs - 1.4739E-03;
			TransCurveB = 5.5455E-01 * cs_4 + 3.563E-02 * cs_3 - 2.4157 * cs_2 + 2.8305 * cs - 2.0373E-03;
			TransCurveC = 7.7087E-01 * cs_4 - 6.3831E-01 * cs_3 - 1.5755 * cs_2 + 2.4482 * cs - 2.042E-03;
			TransCurveD = 3.4624E-01 * cs_4 + 3.9626E-01 * cs_3 - 2.5819 * cs_2 + 2.845 * cs - 2.8036E-04;
			TransCurveE = 2.8825 * cs_4 - 5.8734 * cs_3 + 2.4887 * cs_2 + 1.510 * cs - 2.5766E-03;
			TransCurveF = 3.0254 * cs_4 - 6.3664 * cs_3 + 3.1371 * cs_2 + 1.213 * cs - 1.3667E-03;
			TransCurveG = 3.2292 * cs_4 - 6.844 * cs_3 + 3.5351 * cs_2 + 1.0881 * cs - 2.8905E-03;
			TransCurveH = 3.3341 * cs_4 - 7.1306 * cs_3 + 3.8287 * cs_2 + 9.7663E-01 * cs - 2.9521E-03;
			TransCurveI = 3.1464 * cs_4 - 6.8549 * cs_3 + 3.9311 * cs_2 + 7.85950E-01 * cs - 2.9344E-03;
			TransCurveJ = 3.744 * cs_4 - 8.8364 * cs_3 + 6.0178 * cs_2 + 8.4071E-02 * cs + 4.825E-04;
			TransCurveFGHI = ( TransCurveF + TransCurveG + TransCurveH + TransCurveI ) / 4.0;
			TransCurveFH = ( TransCurveF + TransCurveH ) / 2.0;
			TransCurveBDCD = ( TransCurveB + TransCurveD + TransCurveC + TransCurveD ) / 4.0;

			ReflectCurveA = 1.6322E+01 * cs_4 - 5.7819E+01 * cs_3 + 7.9244E+01 * cs_2 - 5.0081E+01 * cs + 1.3335E+01;
			ReflectCurveB = 4.0478E+01 * cs_4 - 1.1934E+02 * cs_3 + 1.3477E+02 * cs_2 - 7.0973E+01 * cs + 1.6112E+01;
			ReflectCurveC = 5.749E+01 * cs_4 - 1.6451E+02 * cs_3 + 1.780E+02 * cs_2 - 8.8748E+01 * cs + 1.8839E+01;
			ReflectCurveD = 5.7139 * cs_4 - 1.6666E+01 * cs_3 + 1.8627E+01 * cs_2 - 9.7561 * cs + 3.0743;
			ReflectCurveE = -5.4884E-01 * cs_4 - 6.4976 * cs_3 + 2.11990E+01 * cs_2 - 2.0971E+01 * cs + 7.8138;
			ReflectCurveF = 4.2902 * cs_4 - 1.2671E+01 * cs_3 + 1.4656E+01 * cs_2 - 8.1534 * cs + 2.8711;
			ReflectCurveG = 2.174E+01 * cs_4 - 6.4436E+01 * cs_3 + 7.4893E+01 * cs_2 - 4.1792E+01 * cs + 1.0624E+01;
			ReflectCurveH = 4.3405 * cs_4 - 1.280E+01 * cs_3 + 1.4777E+01 * cs_2 - 8.2034 * cs + 2.8793;
			ReflectCurveI = 4.1357E+01 * cs_4 - 1.1775E+02 * cs_3 + 1.2756E+02 * cs_2 - 6.4373E+01 * cs + 1.426E+01;
			ReflectCurveJ = 4.4901 * cs_4 - 1.2658E+01 * cs_3 + 1.3969E+01 * cs_2 - 7.501 * cs + 2.6928;
			ReflectCurveFGHI = ( ReflectCurveF + ReflectCurveG + ReflectCurveH + ReflectCurveI ) / 4.0;
			ReflectCurveFH = ( ReflectCurveF + ReflectCurveH ) / 2.0;
			ReflectCurveBDCD = ( ReflectCurveB + ReflectCurveD + ReflectCurveC + ReflectCurveD ) / 4.0;

			if ( SimpleGlazingU < 1.4195 ) { // cell 1, 2, or 3
				if ( SimpleGlazingSHGC > 0.45 ) {
					// cell # 1
					// Curve E
					TransTmp = TransCurveE;
					ReflectTmp = ReflectCurveE;

				} else if ( ( 0.35 <= SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.45 ) ) {
					// cell # 2
					// 2 way interpolation between Curve E and Curve J
					TransTmp = InterpolateBetweenTwoValues( SimpleGlazingSHGC, 0.35, 0.45, TransCurveJ, TransCurveE );
					ReflectTmp = InterpolateBetweenTwoValues( SimpleGlazingSHGC, 0.35, 0.45, ReflectCurveJ, ReflectCurveE );

				} else if ( SimpleGlazingSHGC < 0.35 ) {
					// cell # 3
					// Curve J
					TransTmp = TransCurveJ;
					ReflectTmp = ReflectCurveJ;

				}

			} else if ( ( 1.4195 <= SimpleGlazingU ) && ( SimpleGlazingU <= 1.7034 ) ) { // cell 4, 5 , 6, 7, 8, 9, or 10
				if ( SimpleGlazingSHGC > 0.55 ) {
					// cell # 4
					// Curve E
					TransTmp = TransCurveE;
					ReflectTmp = ReflectCurveE;

				} else if ( ( 0.5 < SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.55 ) ) {
					// cell # 5
					// 4 way interpolation between Curve E , Curve E, Curve E and Curve FGHI

					TransTmp = InterpolateBetweenFourValues( SimpleGlazingU, SimpleGlazingSHGC, 1.4195, 1.7034, 0.50, 0.55, TransCurveE, TransCurveE, TransCurveFGHI, TransCurveE );
					ReflectTmp = InterpolateBetweenFourValues( SimpleGlazingU, SimpleGlazingSHGC, 1.4195, 1.7034, 0.50, 0.55, ReflectCurveE, ReflectCurveE, ReflectCurveFGHI, ReflectCurveE );

				} else if ( ( 0.45 < SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.5 ) ) {
					// cell # 6
					// 2 way interpolation between Curve E and Curve FGHI
					TransTmp = InterpolateBetweenTwoValues( SimpleGlazingU, 1.4195, 1.7034, TransCurveE, TransCurveFGHI );
					ReflectTmp = InterpolateBetweenTwoValues( SimpleGlazingU, 1.4195, 1.7034, ReflectCurveE, ReflectCurveFGHI );

				} else if ( ( 0.35 < SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.45 ) ) {
					// cell # 7
					// 4 way interpolation between Curve E , Curve FGHI, Curve J and Curve FGHI
					TransTmp = InterpolateBetweenFourValues( SimpleGlazingU, SimpleGlazingSHGC, 1.4195, 1.7034, 0.35, 0.45, TransCurveJ, TransCurveE, TransCurveFGHI, TransCurveFGHI );
					ReflectTmp = InterpolateBetweenFourValues( SimpleGlazingU, SimpleGlazingSHGC, 1.4195, 1.7034, 0.35, 0.45, ReflectCurveJ, ReflectCurveE, ReflectCurveFGHI, ReflectCurveFGHI );

				} else if ( ( 0.3 < SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.35 ) ) {
					// cell # 8
					// 2 way interpolation between Curve J and Curve FGHI
					TransTmp = InterpolateBetweenTwoValues( SimpleGlazingU, 1.4195, 1.7034, TransCurveJ, TransCurveFGHI );
					ReflectTmp = InterpolateBetweenTwoValues( SimpleGlazingU, 1.4195, 1.7034, ReflectCurveJ, ReflectCurveFGHI );

				} else if ( ( 0.25 < SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.3 ) ) {
					// cell # 9
					// 4 way interpolation between Curve J, Curve FGHI, Curve J and Curve FH
					TransTmp = InterpolateBetweenFourValues( SimpleGlazingU, SimpleGlazingSHGC, 1.4195, 1.7034, 0.25, 0.3, TransCurveJ, TransCurveJ, TransCurveFH, TransCurveFGHI );
					ReflectTmp = InterpolateBetweenFourValues( SimpleGlazingU, SimpleGlazingSHGC, 1.4195, 1.7034, 0.25, 0.3, ReflectCurveJ, ReflectCurveJ, ReflectCurveFH, ReflectCurveFGHI );

				} else if ( SimpleGlazingSHGC <= 0.25 ) {
					// cell # 10
					// 2 way interpolation between Curve J and Curve FH
					TransTmp = InterpolateBetweenTwoValues( SimpleGlazingU, 1.4195, 1.7034, TransCurveJ, TransCurveFH );
					ReflectTmp = InterpolateBetweenTwoValues( SimpleGlazingU, 1.4195, 1.7034, ReflectCurveJ, ReflectCurveFH );

				}
			} else if ( ( 1.7034 < SimpleGlazingU ) && ( SimpleGlazingU < 3.4068 ) ) { // cell 11, 12, 13, 14, or 15
				if ( SimpleGlazingSHGC > 0.55 ) {
					// cell # 11
					// Curve E
					TransTmp = TransCurveE;
					ReflectTmp = ReflectCurveE;

				} else if ( ( 0.5 <= SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.55 ) ) {
					// cell # 12
					// 2 way interpolation between Curve E and Curve FGHI
					TransTmp = InterpolateBetweenTwoValues( SimpleGlazingSHGC, 0.5, 0.55, TransCurveFGHI, TransCurveE );
					ReflectTmp = InterpolateBetweenTwoValues( SimpleGlazingSHGC, 0.5, 0.55, ReflectCurveFGHI, ReflectCurveE );

				} else if ( ( 0.3 < SimpleGlazingSHGC ) && ( SimpleGlazingSHGC < 0.5 ) ) {
					// cell # 13
					// Curve FGHI
					TransTmp = TransCurveFGHI;
					ReflectTmp = ReflectCurveFGHI;

				} else if ( ( 0.25 <= SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.3 ) ) {
					// cell # 14
					// 2 way interpolation between Curve FGHI and Curve FH
					TransTmp = InterpolateBetweenTwoValues( SimpleGlazingSHGC, 0.25, 0.30, TransCurveFH, TransCurveFGHI );
					ReflectTmp = InterpolateBetweenTwoValues( SimpleGlazingSHGC, 0.25, 0.30, ReflectCurveFH, ReflectCurveFGHI );

				} else if ( SimpleGlazingSHGC < 0.25 ) {
					// cell # 15
					//Curve FH
					TransTmp = TransCurveFH;
					ReflectTmp = ReflectCurveFH;

				}

			} else if ( ( 3.4068 <= SimpleGlazingU ) && ( SimpleGlazingU <= 4.5424 ) ) { // cell 16, 17, 18, 19, 20, 21, 22, or 23
				if ( SimpleGlazingSHGC > 0.65 ) {
					// cell # 16
					// 2 way interpolation between Curve E and Curve A
					TransTmp = InterpolateBetweenTwoValues( SimpleGlazingU, 3.4068, 4.5424, TransCurveE, TransCurveA );
					ReflectTmp = InterpolateBetweenTwoValues( SimpleGlazingU, 3.4068, 4.5424, ReflectCurveE, ReflectCurveA );

				} else if ( ( 0.6 < SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.65 ) ) {
					// cell # 17
					// 4 way interpolation between Curve E , Curve E, Curve A, and Curve BDCD
					TransTmp = InterpolateBetweenFourValues( SimpleGlazingU, SimpleGlazingSHGC, 3.4068, 4.5424, 0.6, 0.65, TransCurveE, TransCurveE, TransCurveBDCD, TransCurveA );
					ReflectTmp = InterpolateBetweenFourValues( SimpleGlazingU, SimpleGlazingSHGC, 3.4068, 4.5424, 0.6, 0.65, ReflectCurveE, ReflectCurveE, ReflectCurveBDCD, ReflectCurveA );

				} else if ( ( 0.55 < SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.6 ) ) {
					// cell # 18
					// 2 way interpolation between Curve E and Curve BDCD
					TransTmp = InterpolateBetweenTwoValues( SimpleGlazingU, 3.4068, 4.5424, TransCurveE, TransCurveBDCD );
					ReflectTmp = InterpolateBetweenTwoValues( SimpleGlazingU, 3.4068, 4.5424, ReflectCurveE, ReflectCurveBDCD );

				} else if ( ( 0.5 < SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.55 ) ) {
					// cell # 19
					// 4 way interpolation between Curve E , Curve FGHI, Curve BDCD and Curve BDCD
					TransTmp = InterpolateBetweenFourValues( SimpleGlazingU, SimpleGlazingSHGC, 3.4068, 4.5424, 0.5, 0.55, TransCurveFGHI, TransCurveE, TransCurveBDCD, TransCurveBDCD );
					ReflectTmp = InterpolateBetweenFourValues( SimpleGlazingU, SimpleGlazingSHGC, 3.4068, 4.5424, 0.5, 0.55, ReflectCurveFGHI, ReflectCurveE, ReflectCurveBDCD, ReflectCurveBDCD );

				} else if ( ( 0.45 < SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.5 ) ) {
					// cell # 20
					// 2 way interpolation between Curve FGHI and Curve BDCD
					TransTmp = InterpolateBetweenTwoValues( SimpleGlazingU, 3.4068, 4.5424, TransCurveFGHI, TransCurveBDCD );
					ReflectTmp = InterpolateBetweenTwoValues( SimpleGlazingU, 3.4068, 4.5424, ReflectCurveFGHI, ReflectCurveBDCD );

				} else if ( ( 0.3 < SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.45 ) ) {
					// cell # 21
					// 4 way interpolation between Curve FGHI, Curve FGHI, Curve BDCD, and Curve D
					TransTmp = InterpolateBetweenFourValues( SimpleGlazingU, SimpleGlazingSHGC, 3.4068, 4.5424, 0.3, 0.45, TransCurveFGHI, TransCurveFGHI, TransCurveD, TransCurveBDCD );
					ReflectTmp = InterpolateBetweenFourValues( SimpleGlazingU, SimpleGlazingSHGC, 3.4068, 4.5424, 0.3, 0.45, ReflectCurveFGHI, ReflectCurveFGHI, ReflectCurveD, ReflectCurveBDCD );

				} else if ( ( 0.25 < SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.3 ) ) {
					// cell # 22
					// 4 way interpolation between Curve FGHI, Curve FH, Curve D, and Curve D
					TransTmp = InterpolateBetweenFourValues( SimpleGlazingU, SimpleGlazingSHGC, 3.4068, 4.5424, 0.25, 0.3, TransCurveFH, TransCurveFGHI, TransCurveD, TransCurveD );
					ReflectTmp = InterpolateBetweenFourValues( SimpleGlazingU, SimpleGlazingSHGC, 3.4068, 4.5424, 0.25, 0.3, ReflectCurveFH, ReflectCurveFGHI, ReflectCurveD, ReflectCurveD );

				} else if ( SimpleGlazingSHGC <= 0.25 ) {
					// cell # 23
					// 2 way interpolation between Curve FH and Curve D
					TransTmp = InterpolateBetweenTwoValues( SimpleGlazingU, 3.4068, 4.5424, TransCurveFH, TransCurveD );
					ReflectTmp = InterpolateBetweenTwoValues( SimpleGlazingU, 3.4068, 4.5424, ReflectCurveFH, ReflectCurveD );

				}
			} else if ( SimpleGlazingU > 4.5424 ) { // cell 24, 25, 26, 27, or 28
				if ( SimpleGlazingSHGC > 0.65 ) {
					// cell # 24
					// Curve A
					TransTmp = TransCurveA;
					ReflectTmp = ReflectCurveA;
				} else if ( ( 0.6 <= SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.65 ) ) {
					// cell # 25
					// 2 way interpolation between Curve A and Curve BDCD
					TransTmp = InterpolateBetweenTwoValues( SimpleGlazingSHGC, 0.6, 0.65, TransCurveBDCD, TransCurveA );
					ReflectTmp = InterpolateBetweenTwoValues( SimpleGlazingSHGC, 0.6, 0.65, ReflectCurveBDCD, ReflectCurveA );

				} else if ( ( 0.45 < SimpleGlazingSHGC ) && ( SimpleGlazingSHGC < 0.6 ) ) {
					// cell # 26
					// Curve BDCD
					TransTmp = TransCurveBDCD;
					ReflectTmp = ReflectCurveBDCD;

				} else if ( ( 0.3 <= SimpleGlazingSHGC ) && ( SimpleGlazingSHGC <= 0.45 ) ) {
					// cell # 27
					// 2 way interpolation between Curve BDCD and Curve D
					TransTmp = InterpolateBetweenTwoValues( SimpleGlazingSHGC, 0.3, 0.45, TransCurveD, TransCurveBDCD );
					ReflectTmp = InterpolateBetweenTwoValues( SimpleGlazingSHGC, 0.3, 0.45, ReflectCurveD, ReflectCurveBDCD );

				} else if ( SimpleGlazingSHGC < 0.3 ) {
					// cell # 28
					// Curve D
					TransTmp = TransCurveD;
					ReflectTmp = ReflectCurveD;

				} else {
					assert( false );
				}
			} else {
				assert( false );
			}

			if ( cs == 1.0 ) { // at 0 deg incident, TransTmp should be 1.0
				TransTmp = 1.0;
			}

			// now apply normalization factors to zero incidence angle properties
			tfp = tf0 * TransTmp;
			if ( tfp < 0.0 ) tfp = 0.0;
			if ( tfp > 1.0 ) tfp = 1.0;
			rfp = rf0 * ReflectTmp;

			if ( rfp < 0.0 ) rfp = 0.0;
			if ( rfp > 1.0 ) rfp = 1.0;

			rbp = rb0 * ReflectTmp;
			if ( rbp < 0.0 ) rbp = 0.0;
			if ( rbp > 1.0 ) rbp = 1.0;

			if ( cs == 0.0 ) { // at 90 degree incident, reflectance should be 1.0
				rfp = 1.0;
				rbp = 1.0;
			}

			//   older model, was in Version 3.1
			//    IncidenceAngle = ACOS(cs)
			//    CoefFuncSHGC   = 0.768d0 +0.817d0*SimpleGlazingSHGC**4
			//    tfp = tf0 * cs * (1 + CoefFuncSHGC*(Sin(IncidenceAngle)**3))
			//    f1     = (((2.403d0*cs - 6.192d0)*cs + 5.625d0)*cs - 2.095d0) * cs + 1
			//    f2     = (((-1.188d0* cs + 2.022d0)* cs + 0.137d0) * cs - 1.71d0) * cs
			//    Rfit_o = 0.7413d0 - (0.7396d0 * SQRT(SimpleGlazingSHGC))
			//    rfp = rf0 * (f1 + f2*SQRT(SimpleGlazingSHGC))/Rfit_o
			//    rbp = rfp  ! uncoated assumption, back equal front

			return;
		}

		if ( tf0 <= 0.0 ) {
			// This is an opaque window.  For all angles, set transmittance to 0; set reflectance to that at zero incidence angle.
			tfp = 0.0;
			rfp = rf0;
			rbp = rb0;
		} else {

			betaf = pow_2( tf0 ) - pow_2( rf0 ) + 2.0 * rf0 + 1.0;
			betab = pow_2( tf0 ) - pow_2( rb0 ) + 2.0 * rb0 + 1.0;
			r0f = ( betaf - std::sqrt( pow_2( betaf ) - 4.0 * ( 2.0 - rf0 ) * rf0 ) ) / ( 2.0 * ( 2.0 - rf0 ) );
			r0b = ( betab - std::sqrt( pow_2( betab ) - 4.0 * ( 2.0 - rb0 ) * rb0 ) ) / ( 2.0 * ( 2.0 - rb0 ) );

			tmp1 = std::abs( r0f - r0b );
			if ( tmp1 != 0.0 ) {
				testval = std::abs( r0f - r0b ) / ( r0f + r0b );
			} else {
				testval = 0.0;
			}

			if ( testval < 0.001 ) { // CR8830, CR8942, implications of relaxation of glazing properties CR8413
				// UNCOATED GLASS
				tmp1 = r0f * tf0;
				if ( tmp1 != 0.0 ) {
					abf = std::log( tmp1 / ( rf0 - r0f ) );
				} else {
					abf = 0.0;
				}
				tmp2 = r0b * tf0;
				if ( tmp2 != 0.0 ) {
					abb = std::log( tmp2 / ( rb0 - r0b ) );
				} else {
					abb = 0.0;
				}
				ngf = ( 1.0 + std::sqrt( r0f ) ) / ( 1.0 - std::sqrt( r0f ) );
				ngb = ( 1.0 + std::sqrt( r0b ) ) / ( 1.0 - std::sqrt( r0b ) );
				cgf = std::sqrt( 1.0 - ( 1.0 - cs * cs ) / pow_2( ngf ) );
				cgb = std::sqrt( 1.0 - ( 1.0 - cs * cs ) / pow_2( ngb ) );
				tmp3 = ngf * cs - cgf;
				if ( tmp3 != 0.0 ) {
					rpf1 = pow_2( tmp3 / ( ngf * cs + cgf ) );
				} else {
					rpf1 = 0.0;
				}
				tmp4 = ngf * cgf - cs;
				if ( tmp4 != 0.0 ) {
					rpf2 = pow_2( tmp4 / ( ngf * cgf + cs ) );
				} else {
					rpf2 = 0.0;
				}
				tpf1 = 1 - rpf1;
				tpf2 = 1 - rpf2;
				tmp5 = ngb * cs - cgb;
				if ( tmp5 != 0.0 ) {
					rpb1 = pow_2( tmp5 / ( ngb * cs + cgb ) );
				} else {
					rpb1 = 0.0;
				}
				tmp6 = ngb * cgb - cs;
				if ( tmp6 != 0.0 ) {
					rpb2 = pow_2( tmp6 / ( ngb * cgb + cs ) );
				} else {
					rpb2 = 0.0;
				}
				tpb1 = 1 - rpf1;
				tpb2 = 1 - rpf2;
				tmp7 = -abf;
				if ( tmp7 != 0.0 ) {
					expmabfdivcgf = std::exp( tmp7 / cgf );
				} else {
					expmabfdivcgf = 0.0;
				}
				tmp8 = -2.0 * abf;
				if ( tmp8 != 0.0 ) {
					expm2abfdivcgf = std::exp( tmp8 / cgf );
				} else {
					expm2abfdivcgf = 0.0;
				}
				if ( tpf1 != 0.0 ) {
					tfp1 = pow_2( tpf1 ) * expmabfdivcgf / ( 1.0 - pow_2( rpf1 ) * expm2abfdivcgf );
				} else {
					tfp1 = 0.0;
				}
				rfp1 = rpf1 * ( 1.0 + tfp1 * expmabfdivcgf );
				if ( tpf2 != 0.0 ) {
					tfp2 = pow_2( tpf2 ) * expmabfdivcgf / ( 1.0 - pow_2( rpf2 ) * expm2abfdivcgf );
				} else {
					tfp2 = 0.0;
				}
				rfp2 = rpf2 * ( 1.0 + tfp2 * expmabfdivcgf );
				tfp = 0.5 * ( tfp1 + tfp2 );
				rfp = 0.5 * ( rfp1 + rfp2 );
				tmp9 = -abb;
				if ( tmp9 != 0.0 ) {
					expmabbdivcgb = std::exp( tmp9 / cgb );
				} else {
					expmabbdivcgb = 0.0;
				}
				rbp1 = rpb1 * ( 1.0 + tfp1 * expmabbdivcgb );
				rbp2 = rpb2 * ( 1.0 + tfp2 * expmabbdivcgb );
				rbp = 0.5 * ( rbp1 + rbp2 );
			} else {
				// COATED GLASS
				if ( tf0 > 0.645 ) {
					// Use clear glass angular distribution.
					// Normalized clear glass transmittance and reflectance distribution
					if ( cs > 0.999 ) { // Angle of incidence = 0 deg
						tcl = 1.0;
						rcl = 0.0;
					} else if ( cs < 0.001 ) { // Angle of incidence = 90 deg
						tcl = 0.0;
						rcl = 1.0;
					} else {
						tcl = -0.0015 + ( 3.355 + ( -3.840 + ( 1.460 + 0.0288 * cs ) * cs ) * cs ) * cs;
						rcl = 0.999 + ( -0.563 + ( 2.043 + ( -2.532 + 1.054 * cs ) * cs ) * cs ) * cs - tcl;
					}
					tfp = tf0 * tcl;
					rfp = rf0 * ( 1.0 - rcl ) + rcl;
					rbp = rb0 * ( 1.0 - rcl ) + rcl;
				} else {
					// Use bronze glass angular distribution.
					// Normalized bronze tinted glass transmittance and reflectance distribution
					if ( cs > 0.999 ) { // Angle of incidence = 0 deg
						tbnz = 1.0;
						rbnz = 0.0;
					} else if ( cs < 0.001 ) { // Angle of incidence = 90 deg
						tbnz = 0.0;
						rbnz = 1.0;
					} else {
						tbnz = -0.002 + ( 2.813 + ( -2.341 + ( -0.05725 + 0.599 * cs ) * cs ) * cs ) * cs;
						rbnz = 0.997 + ( -1.868 + ( 6.513 + ( -7.862 + 3.225 * cs ) * cs ) * cs ) * cs - tbnz;
					}
					tfp = tf0 * tbnz;
					rfp = rf0 * ( 1.0 - rbnz ) + rbnz;
					rbp = rb0 * ( 1.0 - rbnz ) + rbnz;
				}
			}

		}

	}

	Real64
	InterpolateBetweenTwoValues(
		Real64 const X,
		Real64 const X0,
		Real64 const X1,
		Real64 const F0,
		Real64 const F1
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   June 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Interpolate between two results

		// METHODOLOGY EMPLOYED:
		// linear interpolation

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 InterpResult;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		InterpResult = F0 + ( ( X - X0 ) / ( X1 - X0 ) ) * ( F1 - F0 );
		return InterpResult;

	}

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
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   June 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Interpolate between four results.

		// METHODOLOGY EMPLOYED:
		// bilinear interpolation (approximate)

		// REFERENCES:
		// http://en.wikipedia.org/wiki/Bilinear_interpolation

		// USE STATEMENTS:
		// na

		// Return value
		Real64 InterpResult;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		InterpResult = ( Fx1y1 / ( ( X2 - X1 ) * ( Y2 - Y1 ) ) ) * ( X2 - X ) * ( Y2 - Y ) + ( Fx2y1 / ( ( X2 - X1 ) * ( Y2 - Y1 ) ) ) * ( X - X1 ) * ( Y2 - Y ) + ( Fx1y2 / ( ( X2 - X1 ) * ( Y2 - Y1 ) ) ) * ( X2 - X ) * ( Y - Y1 ) + ( Fx2y2 / ( ( X2 - X1 ) * ( Y2 - Y1 ) ) ) * ( X - X1 ) * ( Y - Y1 );
		return InterpResult;

	}

	//**************************************************************************

	void
	W5LsqFit(
		Array1S< Real64 > const IndepVar, // Independent variables
		Array1S< Real64 > const DepVar, // Dependent variables
		int const N, // Order of polynomial
		int const N1, // First and last data points used
		int const N2,
		Array1S< Real64 > CoeffsCurve // Polynomial coeffients from fit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   April 1976
		//       MODIFIED       November 1999 F.Winkelmann
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Does least squares fit for coefficients of a polynomial
		// that gives a window property, such as transmittance, as a function of
		// the cosine of the angle of incidence. The polynomial is of the
		// form C1*X + C2*X**2 + C3*X**3 + ... +CN*X**N, where N <= 6.
		// Adapted from BLAST subroutine LSQFIT.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Array2D< Real64 > A( 6, 6 ); // Least squares derivative matrix
		Array1D< Real64 > B( 6 ); // Least squares derivative vector
		Array2D< Real64 > D( 6, 16 ); // Powers of independent variable
		Real64 ACON; // Intermediate variables
		Real64 SUM;
		int i; // Loop parameters
		int j;
		int K;
		int L;
		int M;
		int KP1;
		int LP1;
		int NM1;

		// FLOW

		// Set up least squares matrix
		for ( M = N1; M <= N2; ++M ) {
			D( 1, M ) = IndepVar( M );
		}

		for ( i = 2; i <= N; ++i ) {
			for ( M = N1; M <= N2; ++M ) {
				D( i, M ) = D( i - 1, M ) * IndepVar( M );
			}
		}

		for ( i = 1; i <= N; ++i ) {
			SUM = 0.0;
			for ( M = N1; M <= N2; ++M ) {
				SUM += DepVar( M ) * D( i, M );
			}
			B( i ) = SUM;
			for ( j = 1; j <= N; ++j ) {
				SUM = 0.0;
				for ( M = N1; M <= N2; ++M ) {
					SUM += D( i, M ) * D( j, M );
				}
				A( j, i ) = SUM;
				A( i, j ) = SUM;
			}
		}

		// Solve the simultaneous equations using Gauss elimination
		NM1 = N - 1;
		for ( K = 1; K <= NM1; ++K ) {
			KP1 = K + 1;
			for ( i = KP1; i <= N; ++i ) {
				ACON = A( K, i ) / A( K, K );
				B( i ) -= B( K ) * ACON;
				for ( j = K; j <= N; ++j ) {
					A( j, i ) -= A( j, K ) * ACON;
				}
			}
		}

		// Perform back substituion
		CoeffsCurve( N ) = B( N ) / A( N, N );
		LP1 = N;
		L = N - 1;

		while ( L > 0 ) {
			SUM = 0.0;
			for ( j = LP1; j <= N; ++j ) {
				SUM += A( j, L ) * CoeffsCurve( j );
			}
			CoeffsCurve( L ) = ( B( L ) - SUM ) / A( L, L );
			LP1 = L;
			--L;
		}

	}

	//********************************************************************************

	void
	W5LsqFit2(
		Array1A< Real64 > const IndepVar, // Independent variables
		Array1A< Real64 > const DepVar, // Dependent variables
		int const N, // Order of polynomial
		int const N1, // First and last data points used
		int const N2,
		Array1A< Real64 > CoeffsCurve // Polynomial coeffients from fit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   April 1976
		//       MODIFIED       November 1999 F.Winkelmann
		//                      May 2001 F. Winkelmann, to do 19 indep. variables
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Does least squares fit for coefficients of a polynomial
		// that gives a window property, such as transmittance, as a function of
		// the cosine of the angle of incidence. The polynomial is of the
		// form C1*X + C2*X**2 + C3*X**3 + ... +CN*X**N, where N <= 6.
		// Adapted from BLAST subroutine LSQFIT.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Argument array dimensioning
		IndepVar.dim( 19 );
		DepVar.dim( 19 );
		CoeffsCurve.dim( 6 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array2D< Real64 > A( 6, 6 ); // Least squares derivative matrix
		Array1D< Real64 > B( 6 ); // Least squares derivative vector
		Array2D< Real64 > D( 6, 16 ); // Powers of independent variable
		Real64 ACON; // Intermediate variables
		Real64 SUM;
		int i; // Loop parameters
		int j;
		int K;
		int L;
		int M;
		int KP1;
		int LP1;
		int NM1;

		// Set up least squares matrix
		for ( M = N1; M <= N2; ++M ) {
			D( 1, M ) = IndepVar( M );
		}

		for ( i = 2; i <= N; ++i ) {
			for ( M = N1; M <= N2; ++M ) {
				D( i, M ) = D( i - 1, M ) * IndepVar( M );
			}
		}

		for ( i = 1; i <= N; ++i ) {
			SUM = 0.0;
			for ( M = N1; M <= N2; ++M ) {
				SUM += DepVar( M ) * D( i, M );
			}
			B( i ) = SUM;
			for ( j = 1; j <= N; ++j ) {
				SUM = 0.0;
				for ( M = N1; M <= N2; ++M ) {
					SUM += D( i, M ) * D( j, M );
				}
				A( j, i ) = SUM;
				A( i, j ) = SUM;
			}
		}

		// Solve the simultaneous equations using Gauss elimination
		NM1 = N - 1;
		for ( K = 1; K <= NM1; ++K ) {
			KP1 = K + 1;
			for ( i = KP1; i <= N; ++i ) {
				ACON = A( K, i ) / A( K, K );
				B( i ) -= B( K ) * ACON;
				for ( j = K; j <= N; ++j ) {
					A( j, i ) -= A( j, K ) * ACON;
				}
			}
		}

		// Perform back substituion
		CoeffsCurve( N ) = B( N ) / A( N, N );
		LP1 = N;
		L = N - 1;

		while ( L > 0 ) {
			SUM = 0.0;
			for ( j = LP1; j <= N; ++j ) {
				SUM += A( j, L ) * CoeffsCurve( j );
			}
			CoeffsCurve( L ) = ( B( L ) - SUM ) / A( L, L );
			LP1 = L;
			--L;
		}

	}

	//***********************************************************************

	Real64
	DiffuseAverage( Array1S< Real64 > const PropertyValue ) // Property value at angles of incidence
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   November 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculate value of property, such as transmittance, for hemispherical
		// diffuse radiation from property values at angles of incidence from
		// 0 to 90 degrees in 10 degree increments.

		// METHODOLOGY EMPLOYED:
		// By Simpson's rule, evaluates the integral from 0 to 90 deg of
		// 2*PropertyValue(phi)*cos(phi)*sin(phi)*dphi (which is same as
		// PropertyValue(phi)*sin(2*phi)*dphi)

		// Return value
		Real64 DiffuseAverage;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// 0,10,20,...,80,90 degress

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 const DPhiR( 10.0 * DegToRadians ); // Half of 10-deg incidence angle increment (radians)
		int IPhi; // Incidence angle counter

		// FLOW

		DiffuseAverage = 0.0;
		for ( IPhi = 1; IPhi <= 9; ++IPhi ) {
			DiffuseAverage += 0.5 * DPhiR * ( PropertyValue( IPhi ) * std::sin( 2.0 * ( IPhi - 1 ) * DPhiR ) + PropertyValue( IPhi + 1 ) * std::sin( 2.0 * IPhi * DPhiR ) );
		}
		if ( DiffuseAverage < 0.0 ) DiffuseAverage = 0.0;

		return DiffuseAverage;
	}

	//*************************************************************************************

	Real64
	DiffuseAverageProfAngGnd( Array1S< Real64 > const Property ) // Property value vs. profile angle
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculates diffuse average of Property, such as blind transmittance, over profile angles
		// corresponding to (upgoing) radiation from the ground.

		// METHODOLOGY EMPLOYED:
		// Integration by Simpson's rule assuming uniform radiance distribution.

		// Using/Aliasing
		using General::InterpProfAng;

		// Return value
		Real64 DiffuseAverageProfAngGnd;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Phi; // Profile angle (radians)
		Real64 DPhi; // Phi increment
		int IPhi; // Phi index
		Real64 Sum; // Sums
		Real64 SumDenom;

		Sum = 0.0;
		SumDenom = 0.0;
		DPhi = 5.0 * DegToRadians;

		// Integrate from -90 to 0 deg
		for ( IPhi = 1; IPhi <= 18; ++IPhi ) {
			Phi = -PiOvr2 + ( IPhi - 0.5 ) * DPhi;
			Sum += std::cos( Phi ) * DPhi * InterpProfAng( Phi, Property );
			SumDenom += std::cos( Phi ) * DPhi;
		}

		DiffuseAverageProfAngGnd = Sum / SumDenom;
		if ( DiffuseAverageProfAngGnd < 0.0 ) DiffuseAverageProfAngGnd = 0.0;

		return DiffuseAverageProfAngGnd;
	}

	//*************************************************************************************

	Real64
	DiffuseAverageProfAngSky( Array1S< Real64 > const Property ) // Property value vs. profile angle
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Calculates diffuse average of Property, such as blind transmittance, over profile angles
		// corresponding to (downgoing) radiation from the sky.

		// METHODOLOGY EMPLOYED:
		// Integration by Simpson's rule assuming uniform radiance distribution.

		// Using/Aliasing
		using General::InterpProfAng;

		// Return value
		Real64 DiffuseAverageProfAngSky;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Phi; // Profile angle (radians)
		Real64 DPhi; // Phi increment
		int IPhi; // Phi index
		Real64 Sum; // Sums
		Real64 SumDenom;

		Sum = 0.0;
		SumDenom = 0.0;
		DPhi = 5.0 * DegToRadians;

		// Integrate from 0 to 90 deg
		for ( IPhi = 19; IPhi <= 36; ++IPhi ) {
			Phi = -PiOvr2 + ( IPhi - 0.5 ) * DPhi;
			Sum += std::cos( Phi ) * DPhi * InterpProfAng( Phi, Property );
			SumDenom += std::cos( Phi ) * DPhi;
		}

		DiffuseAverageProfAngSky = Sum / SumDenom;
		if ( DiffuseAverageProfAngSky < 0.0 ) DiffuseAverageProfAngSky = 0.0;

		return DiffuseAverageProfAngSky;
	}

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
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Aug 2000, FCW: Add effect of frame and divider projections
		//                      Jun 2001, FCW: Add frame/divider contribution to WinHeatGain
		//                      Aug 2003, FCW: Fix calculation of divider outside temp: frame
		//                       inside temp was being used instead of divider inside temp
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates window frame divider face temperatures from a linearized
		// heat balance on the inside and outside faces

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 HInRad; // Inside radiative conductance (W/m2-K)
		Real64 HOutRad; // Outside radiative conductance (W/m2-K)
		int FrDivNum; // Frame/divider number
		Real64 TInRad; // Inside radiative temperature (K)
		Real64 TInRadFr; // Effective inside radiative temperature for frame (K)
		Real64 TInRadDiv; // Effective inside radiative temperature for divider (K)
		Real64 TOutRad; // Outside radiative temperature (K)
		Real64 TOutRadFr; // Effective outside radiative temperature for frame (K)
		Real64 TOutRadDiv; // Effective outside radiative temperature for divider (K)
		int ShadeFlag; // Window shading flag
		Real64 FrameCon; // Frame conductance (W/m2-K)

		Real64 Afac; // Intermediate calculation variables
		Real64 Bfac;
		Real64 Dfac;
		Real64 Efac;
		int DivType; // Divider type
		Real64 DivCon; // Divider conductance (W/m2-K)
		Real64 DivEmisIn; // Inside divider emissivity
		Real64 DivEmisOut; // Outside divider emissivity

		Real64 ProjCorrFrOut; // Outside correction factor for absorbed radiation
		//   for frame with outside projection
		Real64 ProjCorrFrIn; // Inside correction factor for absorbed radiation
		//   for frame with inside projection
		Real64 HOutConvFr; // Effective outside convective coeff for frame
		//   with outside projection (W/m2-K)
		Real64 HOutConvDiv; // Effective outside convective coeff for divider
		//   with outside projection (W/m2-K)
		Real64 HInConvFr; // Effective inside convective coeff for frame
		//   with inside projection (W/m2-K)
		Real64 HInConvDiv; // Effective inside convective coeff for divider
		//   with inside projection (W/m2-K)
		Real64 EmisGlassOut; // Outside surface emissivity of window glazing
		Real64 EmisGlassIn; // Inside surface emissivity of window glazing
		int TotGlassLayers; // Total number of glass layers
		int TotLayers; // Total number of layers in unshaded construction
		Real64 DivTempOut; // Outside surface divider temperature (K)
		Real64 FrameHeatGain; // Heat gain to zone from frame (W)
		Real64 ProjCorrWinHeatGain; // Inside projection correction to IR from divider to zone
		//   for window heat gain calculation
		Real64 DividerConduction; // Conduction through divider from outside to inside face (W)
		Real64 DividerConvHeatGain; // Convective heat gain from divider to zone (W)
		Real64 DividerRadHeatGain; // Convective IR radiative gain from divider to zone (W)
		Real64 DividerHeatGain; // Heat gain from divider to zone (W)

		TInRad = root_4( SurfaceWindow( SurfNum ).IRfromParentZone / sigma );
		TOutRad = root_4( Outir / sigma );
		ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
		FrDivNum = Surface( SurfNum ).FrameDivider;
		TotLayers = Construct( ConstrNum ).TotLayers;
		TotGlassLayers = Construct( ConstrNum ).TotSolidLayers;
		EmisGlassOut = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).AbsorpThermalFront;
		EmisGlassIn = Material( Construct( ConstrNum ).LayerPoint( TotLayers ) ).AbsorpThermalBack;
		FrameHeatGain = 0.0;
		DividerConduction = 0.0;
		SurfaceWindow( SurfNum ).FrameHeatGain = 0.0;
		SurfaceWindow( SurfNum ).FrameHeatLoss = 0.0;
		SurfaceWindow( SurfNum ).DividerHeatGain = 0.0;
		SurfaceWindow( SurfNum ).DividerHeatLoss = 0.0;

		if ( SurfaceWindow( SurfNum ).FrameArea > 0.0 ) {
			// Window has a frame. Note that if a shade, screen or blind is present it covers only the glazed part of the
			// window and is assumed not to shadow long- or short-wave radiation incident on the frame elements.
			ProjCorrFrOut = SurfaceWindow( SurfNum ).ProjCorrFrOut;
			ProjCorrFrIn = SurfaceWindow( SurfNum ).ProjCorrFrIn;
			TOutRadFr = TOutRad * root_4( ( 1.0 + 0.5 * ProjCorrFrOut ) / ( 1.0 + ProjCorrFrOut ) );
			TInRadFr = TInRad * root_4( ( 1.0 + 0.5 * ProjCorrFrIn ) / ( 1.0 + ProjCorrFrIn ) );
			FrameCon = SurfaceWindow( SurfNum ).FrameConductance;
			HInRad = 0.5 * SurfaceWindow( SurfNum ).FrameEmis * sigma * pow_3( TInRadFr + SurfaceWindow( SurfNum ).FrameTempSurfIn + TKelvin );
			HInConvFr = 0.0;
			HOutRad = 0.5 * SurfaceWindow( SurfNum ).FrameEmis * sigma * pow_3( TOutRadFr + SurfaceWindow( SurfNum ).FrameTempSurfOut + TKelvin );
			HOutConvFr = HOutConv;
			if ( FrameDivider( FrDivNum ).FrameProjectionOut > 0.0 ) {
				HOutRad *= ( 1.0 + ProjCorrFrOut );
				HOutConvFr = HOutConv * ( 1.0 + ProjCorrFrOut );
				// Add long-wave from outside window surface absorbed by frame outside projection
				SurfaceWindow( SurfNum ).FrameQRadOutAbs += 0.5 * SurfaceWindow( SurfNum ).ProjCorrFrOut * FrameDivider( FrDivNum ).FrameEmis * EmisGlassOut * sigma * pow_4( SurfaceWindow( SurfNum ).ThetaFace( 1 ) );
			}
			if ( FrameDivider( FrDivNum ).FrameProjectionIn > 0.0 ) {
				HInRad *= ( 1.0 + ProjCorrFrIn );
				HInConvFr = HInConv * ( 1.0 + ProjCorrFrIn );
				// Add long-wave from inside window surface absorbed by frame inside projection
				SurfaceWindow( SurfNum ).FrameQRadInAbs += 0.5 * SurfaceWindow( SurfNum ).ProjCorrFrIn * FrameDivider( FrDivNum ).FrameEmis * EmisGlassIn * sigma * pow_4( SurfaceWindow( SurfNum ).ThetaFace( 2 * TotGlassLayers ) );
			}
			Afac = ( HOutRad * TOutRadFr + HOutConvFr * tout + SurfaceWindow( SurfNum ).FrameQRadOutAbs ) / ( HOutRad + FrameCon + HOutConvFr );
			Bfac = FrameCon / ( HOutRad + FrameCon + HOutConvFr );
			Dfac = ( HInRad * TInRadFr + HInConvFr * tin + SurfaceWindow( SurfNum ).FrameQRadInAbs ) / ( HInRad + FrameCon + HInConvFr );
			Efac = FrameCon / ( HInRad + FrameCon + HInConvFr );
			SurfaceWindow( SurfNum ).FrameTempSurfIn = ( Dfac + Efac * Afac ) / ( 1.0 - Efac * Bfac ) - TKelvin;
			SurfaceWindow( SurfNum ).FrameTempSurfOut = Afac + Bfac * ( SurfaceWindow( SurfNum ).FrameTempSurfIn + TKelvin ) - TKelvin;
			// Heat gain to zone from frame

			//  FrameHeatGain = SurfaceWindow(SurfNum)%FrameArea * (1.0d0+SurfaceWindow(SurfNum)%ProjCorrFrIn) * &
			//  ( SurfaceWindow(SurfNum)%FrameEmis*(sigma*(SurfaceWindow(SurfNum)%FrameTempSurfIn+TKelvin)**4 - rmir) + &
			//    hcin*(SurfaceWindow(SurfNum)%FrameTempSurfIn+TKelvin - tin) )

			FrameHeatGain = SurfaceWindow( SurfNum ).FrameArea * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrFrIn ) * ( hcin * ( SurfaceWindow( SurfNum ).FrameTempSurfIn + TKelvin - tin ) );

			if ( FrameHeatGain > 0.0 ) {
				SurfaceWindow( SurfNum ).FrameHeatGain = FrameHeatGain;
			} else {
				SurfaceWindow( SurfNum ).FrameHeatLoss = std::abs( FrameHeatGain );
			}

			WinHeatGain( SurfNum ) += FrameHeatGain;
			WinGainFrameDividerToZoneRep( SurfNum ) = FrameHeatGain;
		} // End of check if window has a frame

		if ( SurfaceWindow( SurfNum ).DividerArea > 0.0 && SurfaceWindow( SurfNum ).StormWinFlag < 1 ) {
			// Window has divider. Note that if the window has a storm window layer in place (StormWinFlag = 1)
			// the divider heat transfer calculation is not done.

			DivType = SurfaceWindow( SurfNum ).DividerType;
			DivCon = SurfaceWindow( SurfNum ).DividerConductance;

			if ( DivType == DividedLite ) { // Divided lite
				DivEmisIn = SurfaceWindow( SurfNum ).DividerEmis;
				DivEmisOut = DivEmisIn;
			} else { // Suspended (between-glass) divider
				DivEmisOut = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).AbsorpThermalFront;
				DivEmisIn = Material( Construct( ConstrNum ).LayerPoint( Construct( ConstrNum ).TotLayers ) ).AbsorpThermalBack;

			}

			TOutRadDiv = TOutRad * root_4( ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrDivOut ) / ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivOut ) );
			TInRadDiv = TInRad * root_4( ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrDivIn ) / ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivIn ) );
			HInRad = 0.5 * DivEmisIn * sigma * pow_3( TInRadDiv + SurfaceWindow( SurfNum ).DividerTempSurfIn + TKelvin );
			HOutRad = 0.5 * DivEmisOut * sigma * pow_3( TOutRadDiv + SurfaceWindow( SurfNum ).DividerTempSurfOut + TKelvin );
			HOutConvDiv = HOutConv;

			if ( FrameDivider( FrDivNum ).DividerProjectionOut > 0.0 ) {
				HOutRad *= ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivOut );
				if ( SurfaceWindow( SurfNum ).ShadingFlag == ExtShadeOn ) HOutConvDiv = SurfaceWindow( SurfNum ).ConvCoeffWithShade;
				HOutConvDiv *= ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivOut );
				// Add long-wave from outside window surface absorbed by divider outside projection
				SurfaceWindow( SurfNum ).DividerQRadOutAbs += SurfaceWindow( SurfNum ).ProjCorrDivOut * FrameDivider( FrDivNum ).DividerEmis * EmisGlassOut * sigma * pow_4( SurfaceWindow( SurfNum ).ThetaFace( 1 ) );
			}

			HInConvDiv = HInConv;

			if ( FrameDivider( FrDivNum ).DividerProjectionIn > 0.0 ) {
				HInRad *= ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivIn );
				if ( SurfaceWindow( SurfNum ).ShadingFlag == IntShadeOn ) HInConvDiv = SurfaceWindow( SurfNum ).ConvCoeffWithShade;
				HInConvDiv *= ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivIn );
				// Add long-wave from inside window surface absorbed by divider inside projection
				SurfaceWindow( SurfNum ).DividerQRadInAbs += SurfaceWindow( SurfNum ).ProjCorrDivIn * FrameDivider( FrDivNum ).DividerEmis * EmisGlassIn * sigma * pow_4( SurfaceWindow( SurfNum ).ThetaFace( 2 * TotGlassLayers ) );
			}
			Afac = ( HOutRad * TOutRadDiv + HOutConvDiv * tout + SurfaceWindow( SurfNum ).DividerQRadOutAbs ) / ( HOutRad + DivCon + HOutConvDiv );
			Bfac = DivCon / ( HOutRad + DivCon + HOutConvDiv );
			Dfac = ( HInRad * TInRadDiv + HInConvDiv * tin + SurfaceWindow( SurfNum ).DividerQRadInAbs ) / ( HInRad + DivCon + HInConvDiv );
			Efac = DivCon / ( HInRad + DivCon + HInConvDiv );
			SurfaceWindow( SurfNum ).DividerTempSurfIn = ( Dfac + Efac * Afac ) / ( 1 - Efac * Bfac ) - TKelvin;
			SurfaceWindow( SurfNum ).DividerTempSurfOut = Afac + Bfac * ( SurfaceWindow( SurfNum ).DividerTempSurfIn + TKelvin ) - TKelvin;
			// Contribution of divider to window heat gain
			ProjCorrWinHeatGain = 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivIn;
			DividerConduction = SurfaceWindow( SurfNum ).DividerArea * DivCon * ( SurfaceWindow( SurfNum ).DividerTempSurfOut - SurfaceWindow( SurfNum ).DividerTempSurfIn );
			if ( DividerConduction > 0.0 ) {
				SurfaceWindow( SurfNum ).DividerHeatGain = DividerConduction;
			} else {
				SurfaceWindow( SurfNum ).DividerHeatLoss = std::abs( DividerConduction );
			}
			WinHeatGain( SurfNum ) += DividerConduction;
			WinGainFrameDividerToZoneRep( SurfNum ) += DividerConduction;
			// The following three statements are for debugging purposes only
			DividerConvHeatGain = SurfaceWindow( SurfNum ).DividerArea * HInConvDiv * ( SurfaceWindow( SurfNum ).DividerTempSurfIn + TKelvin - tin );
			DividerRadHeatGain = SurfaceWindow( SurfNum ).DividerArea * HInRad * ( SurfaceWindow( SurfNum ).DividerTempSurfIn + TKelvin - TInRadDiv ) - SurfaceWindow( SurfNum ).DividerArea * SurfaceWindow( SurfNum ).DividerQRadInAbs;
			DividerHeatGain = DividerConvHeatGain + DividerRadHeatGain;
			// If interior shade is present it is assumed that both the convective and IR radiative gain
			// from the inside surface of the divider goes directly into the zone air -- i.e., the IR radiative
			// interaction between divider and shade is ignored due to the difficulty of calculating this interaction
			// at the same time that the interaction between glass and shade is calculated.
			if ( SurfaceWindow( SurfNum ).ShadingFlag == IntShadeOn || SurfaceWindow( SurfNum ).ShadingFlag == IntBlindOn ) SurfaceWindow( SurfNum ).DividerConduction = DividerConduction;
			DivTempOut = SurfaceWindow( SurfNum ).DividerTempSurfOut + TKelvin;
		} // End of check if window has dividers

	}

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
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   September 2000
		//       MODIFIED       Oct 2000, FW: add solar heat gain coefficient
		//                      June 2001, FW: account for blinds; change summer outside air
		//                       temp from 35.0C to 31.7C to correspond to ASHRAE value
		//                      Feb 2003, FW: add comments that this routine is not called for
		//                       between-glass shade/blind constructions.
		//                      May 2006, RR: account for screens
		//                      Oct 2007, LKL: change temps to match Window 5 values
		//                      Feb 2009, BG: Changes for CR7682 (SHGC)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates nominal center-of-glass U-value and solar heat gain coefficient
		// (SHGC) of a window construction for ASHRAE winter and summer conditions.
		// Winter:
		// Inside air temperature = 21.C (69.80F)
		// Outside air temperature = -18C (-.4F)
		// Windspeed = 5.5 m/s (12.3 mph)
		// No solar radiation
		// Replaced Winter:
		// Inside air temperature = 21.1C (70F)
		// Outside air temperature = -17.8C (0F)
		// Windspeed = 6.71 m/s (15 mph)
		// No solar radiation
		// Summer:
		// Inside air temperature = 24C (75.2F)
		// Outside air temperature = 32C (89.6F)
		// Windspeed = 2.8 m/s (6.2 mph)
		// 783 W/m2 (248 Btu/h-ft2) incident beam solar radiation normal to glazing
		// Replaced Summer:
		// Inside air temperature = 24C (75.2F) ! BG changed again Feb. 2009 by 0.1 (per Window5 team)
		// Outside air temperature = 31.7C (89F)
		// Windspeed = 3.35 m/s (7.5 mph)
		// 783 W/m2 (248 Btu/h-ft2) incident beam solar radiation normal to glazing
		// The window's inside surround is assumed to be a black body at the inside air temp.
		// The window's outside surround is assumed t be a black body at the outside air temp.
		// Note that in this routine we use a value of 26 W/m2 for the outside convective
		// air film conductance for 5.5 m/s (12.3 mph) wind speed.
		// This is the value used in Window 5 and is also the value for which the center-of-glass
		// conductances in the EnergyPlus window construction reference data set were calculated.
		// However, in the time step loop we will have different values of outside film
		// conductance depending on that time step's wind speed, wind direction, surface-to-air temp difference,
		// etc.(see subroutine InitExteriorConvectionCoeff).
		// This routine will return an error and exit for window constructions with between-glass shade or blind
		// until a method is worked out to determine the nominal conductance and SHGC in this case.
		// If an interior or exterior shade or blind is present in the construction,
		// the conductance calculation does not include the effect of the shade or blind.
		// This is because in this case the conductance depends on the natural convective
		// air flow in the shade/glass, screen/glass or blind/glass channel, which in turn is highly dependent
		// on window height and other parameters that are not part of the construction definition.
		// Therefore, the reported conductance value will be too high for windows with a tightly fitting
		// shade, screen or blind with a relatively high thermal resistance.
		// For SHGC calculation, all solar absorbed by interior blind or shade is assumed
		// to go into zone air. (This is not true in general; the fraction of this absorbed solar that
		// is conducted back out is accounted for in the time-step glazing calculation.)
		// For CR 7682, the SHGC calculations were changed to model the absorbed solar arriving at the middle of the layer
		// rather than at the outer face of the layer.  The resistances changed by one half the glazing layer, or 0.5/scon(n).
		// (CR 7682 also changed WindowTempsForNominalCond to include absorbed solar, a bigger change)
		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::POLYF;
		using General::InterpBlind;
		using General::InterpSlatAng;
		using General::InterpProfSlatAng;
		using General::BlindBeamBeamTrans;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//   normal incidence beam solar radiation

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int TotLay; // Total number of layers in a construction
		//   (sum of solid layers and gap layers)
		int TotGlassLay; // Total number of glass layers in a construction
		int Lay; // Layer number
		int LayPtr; // Material number for a layer
		int IGlass; // glass layer number (1,2,3,...)
		int IGap; // Gap layer number (1,2,...)
		int IMix; // Gas component loop index for gap gas mixture
		int ICoeff; // Gas property coefficient index

		Real64 BeamSolarInc; // Incident beam radiation at zero angle of incidence (W/m2)
		Real64 hOutRad; // Radiative conductance of outside and inside airfilm [W/m2-K]
		Real64 hInRad;
		Real64 rOut; // Combined radiative and conductive outside and inside film
		Real64 rIn;
		//   resistance [m2-K/W]
		Array1D< Real64 > hgap( 5 ); // Conductive gap conductance [W/m2-K]
		Array1D< Real64 > hGapTot( 5 ); // Combined radiative and conductive gap conductance [W/m2-K]
		Real64 Rbare; // Nominal center-of-glass resistance without air films [m2-K/W]
		int ShadeFlag; // Shading flag
		Real64 ShadeRes; // Thermal resistance of shade
		int MatOutside; // Material number of outside layer of construction
		int MatInside; // Material number of inside layer of construction
		int MatShade; // Material number of shade layer
		Array1D< Real64 > AbsBeamNorm( 5 ); // Beam absorptance at normal incidence for each glass layer
		Real64 AbsBeamShadeNorm; // Shade solar absorptance at normal incidence
		int ConstrNum1; // Construction counter
		int ConstrNumBare; // Construction without shading device
		int BlNum; // Blind number
		int ScNum; // Screen number
		bool VarSlats; // True if slats in blind are variable angle
		Real64 SlatAng; // Slat angle (rad)
		int LayPtrSh; // Layer pointer of blind
		Real64 TBmBm; // Bare glass normal incidence beam-beam transmittance
		Real64 TBmBmVis;
		Real64 TBlBmBm; // Normal incidence blind beam-beam transmittance
		Real64 TScBmBm; // Screen incident beam-beam transmittance
		Real64 TScBmBmVis;
		Real64 TBmBmBl; // TBmBm * TBlBmBm, TBmBmVis * TBlBmBm
		Real64 TBmBmBlVis;
		Real64 RGlDiffBack; // Bare glass back sol/vis reflectance
		Real64 RGlDiffBackVis;
		Real64 RGlDiffFront; // Bare glass front sol/vis reflectance
		Real64 RGlDiffFrontVis;
		Real64 RhoBlFront; // Blind normal front beam-diffuse sol/vis reflectance
		Real64 RhoBlFrontVis;
		Real64 RhoBlBack; // Blind normal back beam-diffuse sol/vis reflectance
		Real64 RhoBlBackVis;
		Real64 RScBack; // Screen back beam-diffuse sol/vis reflectance (same as front)
		Real64 RScBackVis;
		Real64 AbsBlFront; // Blind normal front beam solar absorptance
		Real64 AbsBlBack; // Blind normal back beam solar absorptance
		Real64 RhoBlDiffFront; // Blind front diffuse-diffuse sol/vis reflectance
		Real64 RhoBlDiffFrontVis;
		Real64 AbsBlDiffFront; // Blind front diffuse solar absorptance
		Real64 AbsBlDiffBack; // Blind back diffuse solar absorptance
		Real64 RGlFront; // Bare glass normal front beam sol/vis reflectance
		Real64 RGlFrontVis;
		Real64 RhoBlDiffBack; // Blind back diffuse-diffuse sol/vis reflectance
		Real64 RhoBlDiffBackVis;
		Real64 RScDifBack; // Screen back diffuse-diffuse sol/vis reflectance (doesn't change with sun angle)
		Real64 RScDifBackVis;
		Real64 TBlBmDif; // Blind front normal beam-diffuse sol/vis transmittance
		Real64 TBlBmDifVis;
		Real64 TBlDifDif; // Blind front diffuse-diffuse sol/vis transmittance
		Real64 TBlDifDifVis;
		Real64 TScBmDif; // Screen front beam-diffuse sol/vis transmittance
		Real64 TScBmDifVis;
		Real64 TDif; // Bare glass diffuse sol/vis transmittance
		Real64 TDifVis;
		Real64 AGlDiffBack; // Back diffuse solar absorptance of a glass layer

		//Autodesk:Uninit Initialize variables used uninitialized
		Rbare = 0.0; //Autodesk:Uninit Force default initialization

		errFlag = 0;
		TotLay = Construct( ConstrNum ).TotLayers;
		TotGlassLay = Construct( ConstrNum ).TotGlassLayers;
		ngllayer = TotGlassLay; //Autodesk:Uninit This routine needs to check/enforce 1<=ngllayer<=4
		//EPTeam - believe that is done on input.
		nglface = 2 * ngllayer;
		tilt = 90.0; // Assume vertical window

		if ( WinterSummerFlag == 1 ) { // Winter
			// LKL Oct 2007:  According to Window5, Winter environmental conditions are:
			tin = 294.15; // Inside air temperature (69.8F, 21.C)
			tout = 255.15; // Outside air temperature (-.4F, -18C)
			hcout = 26.0; // Outside convective film conductance for 5.5 m/s (12.3 mph) wind speed
			// (the value used in Window 5)
			//  tin = 294.26   ! Inside air temperature (70F, 21.1C)
			//  tout = 255.35  ! Outside air temperature (0F, -17.8C)
			//  hcout = 25.47  ! Outside convective film conductance for 6.71 m/s (15 mph) wind speed
			//                 ! (the value used in Window 4)
			BeamSolarInc = 0.0;
		} else { // Summer
			// LKL Oct 2007: According to Window5, Summer environmental conditions are:
			//tin = 297.05d0   ! Inside air temperature (75.2F, 24C)
			// BG Feb. 2009: According to Window5 Expert Christian Kohler, it is exactly 24C or 297.15
			tin = 297.15;
			tout = 305.15; // Outside air temperature (89.6F, 32C)
			hcout = 15.0; // Outside convective film conductance for 2.8 m/s (6.2 mph) wind speed
			// (the value used in Window 5)
			//  tin = 297.05   ! Inside air temperature (75F, 23.9C)
			//  !tout = 308.15  ! Outside air temperature (95F, 35.0C)
			//  ! Changed 6/20/01 by FCW to make consistent with Window 4 and 5.
			//  tout = 304.82  ! Outside air temperature (89F, 31.7C)
			//  hcout = 18.86  ! Outside convective film conductance for 3.35 m/s (7.5 mph) wind speed
			//                 ! (average of Window 4 0 m/s and 6.71 m/s values)
			BeamSolarInc = 783.0;
		}

		// IR incident on inside of glazing (inside surround assumed to be
		// a black body at inside air temperature)
		Rmir = sigma * pow_4( tin );

		// IR incident on outside of glazing
		// (outside surround is assumed to be a black body at outside air temperature)
		Outir = sigma * pow_4( tout );

		// Determine whether construction has an exterior or interior shade or blind
		ShadeFlag = NoShade;
		ShadeRes = 0.0;
		MatOutside = Construct( ConstrNum ).LayerPoint( 1 );
		MatInside = Construct( ConstrNum ).LayerPoint( TotLay );
		if ( Material( MatOutside ).Group == 2 ) { // Exterior shade present
			MatShade = MatOutside;
			ShadeFlag = ExtShadeOn;
			// Set glazing outside convection coefficient to Window 4 still-air value
			hcout = 12.25;
		} else if ( Material( MatOutside ).Group == 7 ) { // Exterior screen present
			MatShade = MatOutside;
			ScNum = Material( MatShade ).ScreenDataPtr;
			// Orphaned constructs with exterior screen are ignored
			if ( ScNum > 0 ) ShadeFlag = ExtScreenOn;
			hcout = 12.25;
		} else if ( Material( MatOutside ).Group == 5 ) { // Exterior blind present
			MatShade = MatOutside;
			ShadeFlag = ExtBlindOn;
			BlNum = Material( MatShade ).BlindDataPtr;
			hcout = 12.25;
		} else if ( Material( MatInside ).Group == 2 ) { // Interior shade present
			MatShade = MatInside;
			ShadeFlag = IntShadeOn;
		} else if ( Material( MatInside ).Group == 5 ) { // Interior blind present
			MatShade = MatInside;
			BlNum = Material( MatShade ).BlindDataPtr;
			ShadeFlag = IntBlindOn;
		} else if ( TotGlassLay == 2 ) {
			if ( Material( Construct( ConstrNum ).LayerPoint( 3 ) ).Group == 2 ) ShadeFlag = BGShadeOn;
			if ( Material( Construct( ConstrNum ).LayerPoint( 3 ) ).Group == 5 ) ShadeFlag = BGBlindOn;
		} else if ( TotGlassLay == 3 ) {
			if ( Material( Construct( ConstrNum ).LayerPoint( 5 ) ).Group == 2 ) ShadeFlag = BGShadeOn;
			if ( Material( Construct( ConstrNum ).LayerPoint( 5 ) ).Group == 5 ) ShadeFlag = BGBlindOn;
		}

		if ( ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn ) {
			errFlag = 2;
			return;
		}

		TSolNorm = POLYF( 1.0, Construct( ConstrNum ).TransSolBeamCoef );
		TVisNorm = POLYF( 1.0, Construct( ConstrNum ).TransVisBeamCoef );
		AbsBeamShadeNorm = 0.0;
		if ( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn ) { // Exterior or interior shade on
			AbsBeamShadeNorm = POLYF( 1.0, Construct( ConstrNum ).AbsBeamShadeCoef );
			// Exterior blind or screen or interior blind on
		} else if ( ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) {
			// Find unshaded construction that goes with this construction w/blind or screen
			ConstrNumBare = 0;
			for ( ConstrNum1 = 1; ConstrNum1 <= TotConstructs; ++ConstrNum1 ) {
				if ( ConstrNum1 != ConstrNum && Construct( ConstrNum1 ).TypeIsWindow && Construct( ConstrNum1 ).TotGlassLayers == Construct( ConstrNum1 ).TotSolidLayers && Construct( ConstrNum1 ).TotGlassLayers == Construct( ConstrNum ).TotGlassLayers ) {
					// We have an unshaded window construction with the same number of glass layers as ConstrNum;
					// see if the glass and gas layers match
					ConstrNumBare = ConstrNum1;
					for ( Lay = 1; Lay <= Construct( ConstrNum1 ).TotLayers; ++Lay ) {
						LayPtr = Construct( ConstrNum1 ).LayerPoint( Lay );
						if ( ShadeFlag == IntBlindOn ) { // The shaded construction has an interior blind
							LayPtrSh = Construct( ConstrNum ).LayerPoint( Lay );
						} else { // The shaded construction has an exterior blind or screen
							LayPtrSh = Construct( ConstrNum ).LayerPoint( Lay + 1 );
						}
						if ( LayPtrSh != LayPtr ) ConstrNumBare = 0;
					}
					if ( ConstrNumBare != 0 ) break;
				}
			}
			if ( ConstrNumBare == 0 ) {
				// No matching bare construction found for this construction with blind or screen
				errFlag = 1;
				return;
			}

			TBmBm = POLYF( 1.0, Construct( ConstrNumBare ).TransSolBeamCoef );
			TBmBmVis = POLYF( 1.0, Construct( ConstrNumBare ).TransVisBeamCoef );
			if ( ShadeFlag == ExtScreenOn ) {
				//   Don't need to call subroutine, use normal incident properties (SUBROUTINE CalcNominalWindowCond)
				//   Last call to CalcScreenTransmittance(ISurf) was done at direct normal angle (0,0) in CalcWindowScreenProperties
				TScBmBm = SurfaceScreens( ScNum ).BmBmTrans;
				TScBmBmVis = SurfaceScreens( ScNum ).BmBmTransVis;
				TScBmDif = SurfaceScreens( ScNum ).BmDifTrans;
				TScBmDifVis = SurfaceScreens( ScNum ).BmDifTransVis;
				TDif = Construct( ConstrNumBare ).TransDiff;
				TDifVis = Construct( ConstrNumBare ).TransDiffVis;
				RScBack = SurfaceScreens( ScNum ).ReflectScreen;
				RScBackVis = SurfaceScreens( ScNum ).ReflectScreenVis;
				RScDifBack = SurfaceScreens( ScNum ).DifReflect;
				RScDifBackVis = SurfaceScreens( ScNum ).DifReflectVis;
				RGlFront = POLYF( 1.0, Construct( ConstrNumBare ).ReflSolBeamFrontCoef );
				RGlFrontVis = POLYF( 1.0, Construct( ConstrNumBare ).ReflSolBeamFrontCoef );
				RGlDiffFront = Construct( ConstrNumBare ).ReflectSolDiffFront;
				RGlDiffFrontVis = Construct( ConstrNumBare ).ReflectVisDiffFront;
				TSolNorm = TScBmBm * ( TBmBm + TDif * RGlFront * RScBack / ( 1 - RGlDiffFront * RScDifBack ) ) + TScBmDif * TDif / ( 1 - RGlDiffFront * RScDifBack );
				TVisNorm = TScBmBmVis * ( TBmBmVis + TDifVis * RGlFrontVis * RScBackVis / ( 1 - RGlDiffFrontVis * RScDifBackVis ) ) + TScBmDifVis * TDifVis / ( 1 - RGlDiffFrontVis * RScDifBackVis );
			} else {
				VarSlats = false;
				if ( Blind( BlNum ).SlatAngleType == VariableSlats ) VarSlats = true;
				SlatAng = Blind( BlNum ).SlatAngle * DegToRadians;
				TBlBmBm = BlindBeamBeamTrans( 0.0, SlatAng, Blind( BlNum ).SlatWidth, Blind( BlNum ).SlatSeparation, Blind( BlNum ).SlatThickness );
				TBmBmBl = TBmBm * TBlBmBm;
				TBmBmBlVis = TBmBmVis * TBlBmBm;
				TBlBmDif = InterpProfSlatAng( 0.0, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamDiffTrans );
				TBlBmDifVis = InterpProfSlatAng( 0.0, SlatAng, VarSlats, Blind( BlNum ).VisFrontBeamDiffTrans );
				TDif = Construct( ConstrNumBare ).TransDiff;
				TDifVis = Construct( ConstrNumBare ).TransDiffVis;
				if ( ShadeFlag == IntBlindOn ) {
					RGlDiffBack = Construct( ConstrNumBare ).ReflectSolDiffBack;
					RGlDiffBackVis = Construct( ConstrNumBare ).ReflectVisDiffBack;
					RhoBlFront = InterpProfSlatAng( 0.0, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamDiffRefl );
					RhoBlFrontVis = InterpProfSlatAng( 0.0, SlatAng, VarSlats, Blind( BlNum ).VisFrontBeamDiffRefl );
					AbsBlFront = InterpProfSlatAng( 0.0, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamAbs );
					RhoBlDiffFront = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolFrontDiffDiffRefl );
					RhoBlDiffFrontVis = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).VisFrontDiffDiffRefl );
					AbsBlDiffFront = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolFrontDiffAbs );
					AbsBeamShadeNorm = TBmBm * ( AbsBlFront + RhoBlFront * RGlDiffBack * AbsBlDiffFront / ( 1.0 - RhoBlDiffFront * RGlDiffBack ) );
					TBlDifDif = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolFrontDiffDiffTrans );
					TBlDifDifVis = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).VisFrontDiffDiffTrans );
					TSolNorm = TBmBm * ( TBlBmBm + TBlBmDif + TBlDifDif * RhoBlFront * RGlDiffBack / ( 1.0 - RhoBlDiffFront * RGlDiffBack ) );
					//     use of TBlBmBm here is correct, visible and IR transmittance are the same (reference deleted CR6925 on 3/20/2006)
					TVisNorm = TBmBmVis * ( TBlBmBm + TBlBmDifVis + TBlDifDifVis * RhoBlFrontVis * RGlDiffBackVis / ( 1.0 - RhoBlDiffFrontVis * RGlDiffBackVis ) );
				} // (IntBlind)
				if ( ShadeFlag == ExtBlindOn ) {
					TBlBmBm = BlindBeamBeamTrans( 0.0, SlatAng, Blind( BlNum ).SlatWidth, Blind( BlNum ).SlatSeparation, Blind( BlNum ).SlatThickness );
					RGlFront = POLYF( 1.0, Construct( ConstrNumBare ).ReflSolBeamFrontCoef );
					RGlFrontVis = POLYF( 1.0, Construct( ConstrNumBare ).ReflSolBeamFrontCoef );
					AbsBlFront = InterpProfSlatAng( 0.0, SlatAng, VarSlats, Blind( BlNum ).SolFrontBeamAbs );
					AbsBlBack = InterpProfSlatAng( 0.0, SlatAng, VarSlats, Blind( BlNum ).SolBackBeamAbs );
					AbsBlDiffBack = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolBackDiffAbs );
					RGlDiffFront = Construct( ConstrNumBare ).ReflectSolDiffFront;
					RGlDiffFrontVis = Construct( ConstrNumBare ).ReflectVisDiffFront;
					RhoBlDiffBack = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).SolBackDiffDiffRefl );
					RhoBlDiffBackVis = InterpSlatAng( SlatAng, VarSlats, Blind( BlNum ).VisBackDiffDiffRefl );
					RhoBlBack = InterpProfSlatAng( 0.0, SlatAng, VarSlats, Blind( BlNum ).SolBackBeamDiffRefl );
					RhoBlBackVis = InterpProfSlatAng( 0.0, SlatAng, VarSlats, Blind( BlNum ).SolBackBeamDiffRefl );
					AbsBeamShadeNorm = AbsBlFront + AbsBlBack * RGlFront * TBlBmBm + ( AbsBlDiffBack * RGlDiffFront / ( 1.0 - RhoBlDiffBack * RGlDiffFront ) ) * ( RGlFront * TBlBmBm * RhoBlBack + TBlBmDif );
					RGlDiffFront = Construct( ConstrNumBare ).ReflectSolDiffFront;
					TSolNorm = TBlBmBm * ( TBmBm + TDif * RGlFront * RhoBlBack / ( 1 - RGlDiffFront * RhoBlDiffBack ) ) + TBlBmDif * TDif / ( 1.0 - RGlDiffFront * RhoBlDiffBack );
					TVisNorm = TBlBmBm * ( TBmBmVis + TDifVis * RGlFrontVis * RhoBlBackVis / ( 1 - RGlDiffFrontVis * RhoBlDiffBackVis ) ) + TBlBmDifVis * TDifVis / ( 1.0 - RGlDiffFrontVis * RhoBlDiffBackVis );
				} // (ExtBlind)
			} // (Screen or Blind)
		} // (Shade, Blind, or Screen)

		// Fill the layer properties needed for the thermal calculation.

		// The layer and face numbering are as follows (for the triple glazing case):
		// Glass layers are 1,2 and 3, where 1 is the outside (outside environment facing)
		//   layer and 3 is the inside (room-facing) layer;
		// Faces (also called surfaces) are 1,2,3,4,5 and 6, where face 1 is the
		//   outside (front) face of glass layer 1, face 2 is the inside (back)
		//   face of glass layer 1, face 3 is the outer face of glass layer 2, face 4 is the
		//   inner face of glass layer 2, etc.
		// Gap layers are 1 and 2, where gap layer 1 is between glass layers 1 and 2
		//   and gap layer 2 is between glass layers 2 and 3.

		IGlass = 0;
		IGap = 0;

		for ( Lay = 1; Lay <= TotLay; ++Lay ) {
			LayPtr = Construct( ConstrNum ).LayerPoint( Lay );
			if ( ( Material( LayPtr ).Group == WindowGlass ) || ( Material( LayPtr ).Group == WindowSimpleGlazing ) ) {
				++IGlass;
				thick( IGlass ) = Material( LayPtr ).Thickness;
				scon( IGlass ) = Material( LayPtr ).Conductivity / Material( LayPtr ).Thickness;
				emis( 2 * IGlass - 1 ) = Material( LayPtr ).AbsorpThermalFront;
				emis( 2 * IGlass ) = Material( LayPtr ).AbsorpThermalBack;
				tir( 2 * IGlass - 1 ) = Material( LayPtr ).TransThermal;
				tir( 2 * IGlass ) = Material( LayPtr ).TransThermal;
				AbsBeamNorm( IGlass ) = POLYF( 1.0, Construct( ConstrNum ).AbsBeamCoef( {1,6}, IGlass ) );
				if ( ShadeFlag == IntBlindOn ) { // Interior blind on
					AbsBeamNorm( IGlass ) = POLYF( 1.0, Construct( ConstrNumBare ).AbsBeamCoef( {1,6}, IGlass ) );
					AGlDiffBack = Construct( ConstrNumBare ).AbsDiffBack( IGlass );
					AbsBeamNorm( IGlass ) += TBmBm * AGlDiffBack * RhoBlFront / ( 1.0 - RhoBlFront * RGlDiffBack );
				} else if ( ShadeFlag == ExtBlindOn ) { // Exterior blind on
					AbsBeamNorm( IGlass ) = POLYF( 1.0, Construct( ConstrNumBare ).AbsBeamCoef( {1,6}, IGlass ) );
					AbsBeamNorm( IGlass ) = TBlBmBm * AbsBeamNorm( IGlass ) + ( TBlBmBm * RGlFront * RhoBlBack + TBlBmDif ) * Construct( ConstrNumBare ).AbsDiff( IGlass ) / ( 1.0 - RGlDiffFront * RhoBlDiffBack );
				} else if ( ShadeFlag == ExtScreenOn ) { // Exterior screen on
					AbsBeamNorm( IGlass ) = POLYF( 1.0, Construct( ConstrNumBare ).AbsBeamCoef( {1,6}, IGlass ) );
					AbsBeamNorm( IGlass ) = TScBmBm * AbsBeamNorm( IGlass ) + ( TScBmBm * RGlFront * RScBack + TScBmDif ) * Construct( ConstrNumBare ).AbsDiff( IGlass ) / ( 1.0 - RGlDiffFront * RScDifBack );
				}
				AbsRadGlassFace( 2 * IGlass - 1 ) = 0.5 * BeamSolarInc * AbsBeamNorm( IGlass );
				AbsRadGlassFace( 2 * IGlass ) = 0.5 * BeamSolarInc * AbsBeamNorm( IGlass );
			}
			if ( Material( LayPtr ).Group == WindowGas || Material( LayPtr ).Group == WindowGasMixture || Material( LayPtr ).Group == ComplexWindowGap ) { // Gap layer
				++IGap;
				//Simon: Need to re-reference gas data in casee of complex fenestration gap
				if ( Material( LayPtr ).Group == ComplexWindowGap ) {
					LayPtr = Material( LayPtr ).GasPointer;
				}
				gap( IGap ) = Material( LayPtr ).Thickness;
				gnmix( IGap ) = Material( LayPtr ).NumberOfGasesInMixture;
				for ( IMix = 1; IMix <= gnmix( IGap ); ++IMix ) {
					gwght( IMix, IGap ) = Material( LayPtr ).GasWght( IMix );
					gfract( IMix, IGap ) = Material( LayPtr ).GasFract( IMix );
					for ( ICoeff = 1; ICoeff <= 3; ++ICoeff ) {
						gcon( ICoeff, IMix, IGap ) = Material( LayPtr ).GasCon( ICoeff, IMix );
						gvis( ICoeff, IMix, IGap ) = Material( LayPtr ).GasVis( ICoeff, IMix );
						gcp( ICoeff, IMix, IGap ) = Material( LayPtr ).GasCp( ICoeff, IMix );
					}
				}
			}
		}

		// Factors used in glass temperature solution
		if ( ngllayer >= 2 ) {
			A23P = -emis( 3 ) / ( 1.0 - ( 1.0 - emis( 2 ) ) * ( 1.0 - emis( 3 ) ) );
			A32P = emis( 2 ) / ( 1.0 - ( 1.0 - emis( 2 ) ) * ( 1.0 - emis( 3 ) ) );
			A23 = emis( 2 ) * sigma * A23P;
		}

		if ( ngllayer >= 3 ) {
			A45P = -emis( 5 ) / ( 1.0 - ( 1.0 - emis( 4 ) ) * ( 1.0 - emis( 5 ) ) );
			A54P = emis( 4 ) / ( 1.0 - ( 1.0 - emis( 4 ) ) * ( 1.0 - emis( 5 ) ) );
			A45 = emis( 4 ) * sigma * A45P;
		}

		if ( ngllayer == 4 ) {
			A67P = -emis( 7 ) / ( 1.0 - ( 1.0 - emis( 6 ) ) * ( 1.0 - emis( 7 ) ) );
			A76P = emis( 6 ) / ( 1.0 - ( 1.0 - emis( 6 ) ) * ( 1.0 - emis( 7 ) ) );
			A67 = emis( 6 ) * sigma * A67P;
		}

		thetas = 0.0;

		WindowTempsForNominalCond( ConstrNum, hgap );

		// Get center-of-glass conductance and solar heat gain coefficient
		// including inside and outside air films

		hOutRad = emis( 1 ) * sigma * 0.5 * pow_3( tout + thetas( 1 ) );
		rOut = 1.0 / ( hOutRad + hcout );
		hInRad = emis( nglface ) * sigma * 0.5 * pow_3( tin + thetas( nglface ) );
		rIn = 1.0 / ( hInRad + hcin );

		if ( ! ( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) ) AbsBeamShadeNorm = 0.0;

		{ auto const SELECT_CASE_var( ngllayer );

		if ( SELECT_CASE_var == 1 ) {
			Rbare = 1.0 / scon( 1 );
			Rtot = rOut + Rbare + rIn;
			SHGC = AbsBeamNorm( 1 ) * ( rOut + ( 0.5 / scon( 1 ) ) ) / Rtot; // BG changed for CR7682 (solar absorbed in middle of layer)
			SHGC += AbsBeamShadeNorm;
			SHGC += TSolNorm;

		} else if ( SELECT_CASE_var == 2 ) {
			hGapTot( 1 ) = hgap( 1 ) + std::abs( A23 ) * 0.5 * pow_3( thetas( 2 ) + thetas( 3 ) );
			Rbare = 1.0 / scon( 1 ) + 1.0 / hGapTot( 1 ) + 1.0 / scon( 2 );
			Rtot = rOut + Rbare + rIn;
			SHGC = AbsBeamNorm( 1 ) * ( rOut + 0.5 / scon( 1 ) ) / Rtot + AbsBeamNorm( 2 ) * ( rOut + 1.0 / scon( 1 ) + 1.0 / hGapTot( 1 ) + 0.5 / scon( 2 ) ) / Rtot; //CR7682
			SHGC += AbsBeamShadeNorm;
			SHGC += TSolNorm;

		} else if ( SELECT_CASE_var == 3 ) {
			hGapTot( 1 ) = hgap( 1 ) + std::abs( A23 ) * 0.5 * pow_3( thetas( 2 ) + thetas( 3 ) );
			hGapTot( 2 ) = hgap( 2 ) + std::abs( A45 ) * 0.5 * pow_3( thetas( 4 ) + thetas( 5 ) );
			Rbare = 1.0 / scon( 1 ) + 1.0 / hGapTot( 1 ) + 1.0 / scon( 2 ) + 1.0 / hGapTot( 2 ) + 1.0 / scon( 3 );
			Rtot = rOut + Rbare + rIn;
			SHGC = AbsBeamNorm( 1 ) * ( rOut + 0.5 / scon( 1 ) ) / Rtot + AbsBeamNorm( 2 ) * ( rOut + 1.0 / scon( 1 ) + 1.0 / hGapTot( 1 ) + 0.5 / scon( 2 ) ) / Rtot + AbsBeamNorm( 3 ) * ( rOut + 1.0 / scon( 1 ) + 1.0 / hGapTot( 1 ) + 1.0 / scon( 2 ) + 1.0 / hGapTot( 2 ) + 0.5 / scon( 3 ) ) / Rtot;
			SHGC += AbsBeamShadeNorm;
			SHGC += TSolNorm;

		} else if ( SELECT_CASE_var == 4 ) {
			hGapTot( 1 ) = hgap( 1 ) + std::abs( A23 ) * 0.5 * pow_3( thetas( 2 ) + thetas( 3 ) );
			hGapTot( 2 ) = hgap( 2 ) + std::abs( A45 ) * 0.5 * pow_3( thetas( 4 ) + thetas( 5 ) );
			hGapTot( 3 ) = hgap( 3 ) + std::abs( A67 ) * 0.5 * pow_3( thetas( 6 ) + thetas( 7 ) );
			Rbare = 1.0 / scon( 1 ) + 1.0 / hGapTot( 1 ) + 1.0 / scon( 2 ) + 1.0 / hGapTot( 2 ) + 1.0 / scon( 3 ) + 1.0 / hGapTot( 3 ) + 1.0 / scon( 4 );
			Rtot = rOut + Rbare + rIn;
			SHGC = AbsBeamNorm( 1 ) * ( rOut + 0.5 / scon( 1 ) ) / Rtot + AbsBeamNorm( 2 ) * ( rOut + 1.0 / scon( 1 ) + 1.0 / hGapTot( 1 ) + 0.5 / scon( 2 ) ) / Rtot + AbsBeamNorm( 3 ) * ( rOut + 1.0 / scon( 1 ) + 1.0 / hGapTot( 1 ) + 1.0 / scon( 2 ) + 1.0 / hGapTot( 2 ) + 0.5 / scon( 3 ) ) / Rtot + AbsBeamNorm( 4 ) * ( rOut + 1.0 / scon( 1 ) + 1.0 / hGapTot( 1 ) + 1.0 / scon( 2 ) + 1.0 / hGapTot( 2 ) + 1.0 / scon( 3 ) + 1.0 / hGapTot( 3 ) + 0.5 / scon( 4 ) ) / Rtot; //CR7682
			SHGC += AbsBeamShadeNorm;
			SHGC += TSolNorm;

		}}

		NominalConductance = 1.0 / ( rOut + Rbare + rIn ); //Autodesk:Uninit Rbare was uninitialized if ngllayer > 4
		//EPTeam - again -- believe that is enforced in input //Autodesk But this routine is not self-protecting: Add as an assert

	}

	//****************************************************************************

	void
	WindowTempsForNominalCond(
		int const ConstrNum, // Construction number
		Array1A< Real64 > hgap // Gap gas conductive conductance (W/m2-K)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   September 2000
		//       MODIFIED       Nov 2002, FW: increase MaxIterations from 15 to 100, add face
		//                       temperature relaxation, and increase convergence tolerance by
		//                       a factor of 10 if no convergence after MaxIterations,
		//                       all for consistency with SolveForWindowTemperatures.
		//                      Mar 2003, FW: increase convergence tolerance from 0.01 to 0.02;
		//                       remove redundant relaxation on radiative conductances (both of
		//                       these were also done in SolveForWindowTemperatures).
		//                      Jan 2009, BG: changed interior convection coefficient correlation to match
		//                       ISO 15099.
		//                      Feb 2009, BG: extended coefficient to include absorbed radiation
		//                       to cover summer conditions for SHGC determination.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This is a shortened form of SolveForWindowTemperatures tailored
		// for calculation of the nominal center-of-glass U-value for a window
		// construction at ASHRAE winter conditions and for determining conditions at
		// summer conditions for calculationg SHGC.
		// Evaluates the coefficients Aface and Bface in the system of linear
		// algebraic equations
		//     Sum    [Aface(i,j)*thetas(j)] = Bface(i), i = 1,nglface
		//  j=1,nglface
		// where
		// nglface = number of glass faces (= 2 * number of layers) and
		// thetas(j) = temperature of face j

		// METHODOLOGY EMPLOYED:
		// The Aface and Bface coefficients are determined by the equations for
		// heat balance at the glass faces. The system of linear equations is solved
		// by LU decomposition.

		// REFERENCES:
		// na
		// Using/Aliasing
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		//unused0909  USE DataEnvironment, ONLY: StdBaroPress

		// Argument array dimensioning
		hgap.dim( 5 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIterations( 100 ); // Maximum allowed number of iterations
		Real64 const errtemptol( 0.02 ); // Tolerance on errtemp for convergence
		static std::string const RoutineName( "WindowTempsForNominalCond" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i; // Counter
		Real64 gr; // Grashof number of gas in a gap
		Real64 con; // Gap gas conductivity
		Real64 pr; // Gap gas Prandtl number
		Real64 nu; // Gap gas Nusselt number
		Array1D< Real64 > hr( 10 ); // Radiative conductance (W/m2-K)
		Array1D< Real64 > hrprev( 10 ); // Value of hr from previous iteration
		Real64 hcinprev; // Value of hcin from previous iteration
		Real64 d; // +1 if number of row interchanges is even,
		// -1 if odd (in LU decomposition)
		Array1D_int indx( 10 ); // Vector of row permutations in LU decomposition
		Array2D< Real64 > Aface( 10, 10 ); // Coefficient in equation Aface*thetas = Bface
		Array1D< Real64 > Bface( 10 ); // Coefficient in equation Aface*thetas = Bface
		int iter; // Iteration number
		Real64 errtemp; // Absolute value of sum of face temperature differences
		//   between iterations, divided by number of faces
		Real64 TmeanFilm; // mean film temperature
		Real64 TmeanFilmKelvin; // mean film temperature for property evaluation
		Real64 rho; // density of (apparently dry) air [kg/m3]
		Real64 g; // acceleration due to gravity [m/s2]
		Real64 Height; // window cavity height [m]
		Real64 Cp; // specific heat of air [J/kg-K]
		Real64 lambda; // thermal conductivity of air [W/m-K]
		Real64 mu; // dynamic viscosity of air [kg/m-s]
		Real64 RaH; // Rayleigh number for cavity height [ Non dim]
		Real64 TiltDeg; // glazing tilt in degrees
		Real64 sineTilt; // sine of glazing tilt
		Real64 Nuint; // Nusselt number for interior surface convection

		iter = 0;

		// Initialize face temperatures
		StartingWinTempsForNominalCond();

		// Calculate radiative conductance
		errtemp = errtemptol * 2.0;

		TiltDeg = 90.0;

		sineTilt = std::sin( TiltDeg * DegToRadians ); //degrees as arg

		while ( iter < MaxIterations && errtemp > errtemptol ) {
			for ( i = 1; i <= nglface; ++i ) {
				hr( i ) = emis( i ) * sigma * pow_3( thetas( i ) );
				//!fw 3/4/03 if ( iter >= 1 ) hr(i) = 0.5*(hrprev(i)+hr(i))
				hrprev( i ) = hr( i );
			}

			Aface = 0.0;
			Bface = 0.0;

			// Inside convective film conductance for vertical window
			if ( iter >= 1 ) {
				hcinprev = hcin;
			}
			// CR7670 BG this next correlation was used for hcin but is not "standard" for windows
			//  hcin = 1.31d0*((ABS(thetas(nglface)-tin))**0.3333d0)
			// Begin calculating for ISO 15099 method.
			// mean film temperature
			TmeanFilmKelvin = tin + 0.25 * ( thetas( nglface ) - tin ); // eq. 133 in ISO 15099
			TmeanFilm = TmeanFilmKelvin - 273.15;
			// the following properties are constants or linear relations for "standard" type reporting
			rho = PsyRhoAirFnPbTdbW( 101325.0, TmeanFilm, 0.0, RoutineName ); // dry air assumption
			g = 9.81;
			Height = 1.0; // standard window rating practice is to use 1 meter (rather than actual)

			lambda = 2.873E-3 + 7.76E-5 * TmeanFilmKelvin; // Table B.1 in ISO 15099
			mu = 3.723E-6 + 4.94E-8 * TmeanFilmKelvin; // Table B.2 in ISO 15099
			Cp = 1002.737 + 1.2324E-2 * TmeanFilmKelvin; // Table B.3 in ISO 15099

			RaH = ( pow_2( rho ) * pow_3( Height ) * g * Cp * ( std::abs( thetas( nglface ) - tin ) ) ) / ( TmeanFilmKelvin * mu * lambda ); // eq 132 in ISO 15099

			Nuint = 0.56 * root_4( RaH * sineTilt ); // eq. 135 in ISO 15099 (only need this one because tilt is 90 deg

			hcin = Nuint * lambda / Height;

			// End calculations for ISO 15099 method.

			if ( iter >= 1 ) hcin = 0.5 * ( hcinprev + hcin );

			++iter;

			{ auto const SELECT_CASE_var( ngllayer );

			if ( SELECT_CASE_var == 1 ) {
				Bface( 1 ) = Outir * emis( 1 ) + hcout * tout + AbsRadGlassFace( 1 );
				Bface( 2 ) = Rmir * emis( 2 ) + hcin * tin + AbsRadGlassFace( 2 );

				Aface( 1, 1 ) = hr( 1 ) + scon( 1 ) + hcout;
				Aface( 2, 1 ) = -scon( 1 );
				Aface( 1, 2 ) = -scon( 1 );
				Aface( 2, 2 ) = hr( 2 ) + scon( 1 ) + hcin;

			} else if ( SELECT_CASE_var == 2 ) {
				WindowGasConductance( thetas( 2 ), thetas( 3 ), 1, con, pr, gr );
				NusseltNumber( 0, thetas( 2 ), thetas( 3 ), 1, gr, pr, nu );
				hgap( 1 ) = con / gap( 1 ) * nu;

				Bface( 1 ) = Outir * emis( 1 ) + hcout * tout + AbsRadGlassFace( 1 );
				Bface( 2 ) = AbsRadGlassFace( 2 );
				Bface( 3 ) = AbsRadGlassFace( 3 );
				Bface( 4 ) = Rmir * emis( 4 ) + hcin * tin + AbsRadGlassFace( 4 );

				Aface( 1, 1 ) = hr( 1 ) + scon( 1 ) + hcout;
				Aface( 2, 1 ) = -scon( 1 );

				Aface( 1, 2 ) = -scon( 1 );
				Aface( 2, 2 ) = scon( 1 ) + hgap( 1 ) - A23P * hr( 2 );
				Aface( 3, 2 ) = -hgap( 1 ) - A32P * hr( 3 );

				Aface( 2, 3 ) = -hgap( 1 ) + A23P * hr( 2 );
				Aface( 3, 3 ) = hgap( 1 ) + scon( 2 ) + A32P * hr( 3 );
				Aface( 4, 3 ) = -scon( 2 );

				Aface( 3, 4 ) = -scon( 2 );
				Aface( 4, 4 ) = hr( 4 ) + scon( 2 ) + hcin;

			} else if ( SELECT_CASE_var == 3 ) {
				WindowGasConductance( thetas( 2 ), thetas( 3 ), 1, con, pr, gr );
				NusseltNumber( 0, thetas( 2 ), thetas( 3 ), 1, gr, pr, nu );
				hgap( 1 ) = con / gap( 1 ) * nu;

				WindowGasConductance( thetas( 4 ), thetas( 5 ), 2, con, pr, gr );
				NusseltNumber( 0, thetas( 4 ), thetas( 5 ), 2, gr, pr, nu );
				hgap( 2 ) = con / gap( 2 ) * nu;

				Bface( 1 ) = Outir * emis( 1 ) + hcout * tout + AbsRadGlassFace( 1 );
				Bface( 2 ) = AbsRadGlassFace( 2 );
				Bface( 3 ) = AbsRadGlassFace( 3 );
				Bface( 4 ) = AbsRadGlassFace( 4 );
				Bface( 5 ) = AbsRadGlassFace( 5 );
				Bface( 6 ) = Rmir * emis( 6 ) + hcin * tin + AbsRadGlassFace( 6 );

				Aface( 1, 1 ) = hr( 1 ) + scon( 1 ) + hcout;
				Aface( 2, 1 ) = -scon( 1 );

				Aface( 1, 2 ) = -scon( 1 );
				Aface( 2, 2 ) = scon( 1 ) + hgap( 1 ) - A23P * hr( 2 );
				Aface( 3, 2 ) = -hgap( 1 ) - A32P * hr( 3 );

				Aface( 2, 3 ) = -hgap( 1 ) + A23P * hr( 2 );
				Aface( 3, 3 ) = hgap( 1 ) + scon( 2 ) + A32P * hr( 3 );
				Aface( 4, 3 ) = -scon( 2 );

				Aface( 3, 4 ) = -scon( 2 );
				Aface( 4, 4 ) = scon( 2 ) + hgap( 2 ) - A45P * hr( 4 );
				Aface( 5, 4 ) = -hgap( 2 ) - A54P * hr( 5 );

				Aface( 4, 5 ) = -hgap( 2 ) + A45P * hr( 4 );
				Aface( 5, 5 ) = hgap( 2 ) + scon( 3 ) + A54P * hr( 5 );
				Aface( 6, 5 ) = -scon( 3 );

				Aface( 5, 6 ) = -scon( 3 );
				Aface( 6, 6 ) = hr( 6 ) + scon( 3 ) + hcin;

			} else if ( SELECT_CASE_var == 4 ) {
				WindowGasConductance( thetas( 2 ), thetas( 3 ), 1, con, pr, gr );
				NusseltNumber( 0, thetas( 2 ), thetas( 3 ), 1, gr, pr, nu );
				hgap( 1 ) = con / gap( 1 ) * nu;

				WindowGasConductance( thetas( 4 ), thetas( 5 ), 2, con, pr, gr );
				NusseltNumber( 0, thetas( 4 ), thetas( 5 ), 2, gr, pr, nu );
				hgap( 2 ) = con / gap( 2 ) * nu;

				WindowGasConductance( thetas( 6 ), thetas( 7 ), 3, con, pr, gr );
				NusseltNumber( 0, thetas( 6 ), thetas( 7 ), 3, gr, pr, nu );
				hgap( 3 ) = con / gap( 3 ) * nu;

				Bface( 1 ) = Outir * emis( 1 ) + hcout * tout + AbsRadGlassFace( 1 );
				Bface( 2 ) = AbsRadGlassFace( 2 );
				Bface( 3 ) = AbsRadGlassFace( 3 );
				Bface( 4 ) = AbsRadGlassFace( 4 );
				Bface( 5 ) = AbsRadGlassFace( 5 );
				Bface( 6 ) = AbsRadGlassFace( 6 );
				Bface( 7 ) = AbsRadGlassFace( 7 );
				Bface( 8 ) = Rmir * emis( 8 ) + hcin * tin + AbsRadGlassFace( 8 );

				Aface( 1, 1 ) = hr( 1 ) + scon( 1 ) + hcout;
				Aface( 2, 1 ) = -scon( 1 );

				Aface( 1, 2 ) = -scon( 1 );
				Aface( 2, 2 ) = scon( 1 ) + hgap( 1 ) - A23P * hr( 2 );
				Aface( 3, 2 ) = -hgap( 1 ) - A32P * hr( 3 );

				Aface( 2, 3 ) = -hgap( 1 ) + A23P * hr( 2 );
				Aface( 3, 3 ) = hgap( 1 ) + scon( 2 ) + A32P * hr( 3 );
				Aface( 4, 3 ) = -scon( 2 );

				Aface( 3, 4 ) = -scon( 2 );
				Aface( 4, 4 ) = scon( 2 ) + hgap( 2 ) - A45P * hr( 4 );
				Aface( 5, 4 ) = -hgap( 2 ) - A54P * hr( 5 );

				Aface( 4, 5 ) = -hgap( 2 ) + A45P * hr( 4 );
				Aface( 5, 5 ) = hgap( 2 ) + scon( 3 ) + A54P * hr( 5 );
				Aface( 6, 5 ) = -scon( 3 );

				Aface( 5, 6 ) = -scon( 3 );
				Aface( 6, 6 ) = scon( 3 ) + hgap( 3 ) - A67P * hr( 6 );
				Aface( 7, 6 ) = -hgap( 3 ) - A76P * hr( 7 );

				Aface( 6, 7 ) = -hgap( 3 ) + A67P * hr( 6 );
				Aface( 7, 7 ) = hgap( 3 ) + scon( 4 ) + A76P * hr( 7 );
				Aface( 8, 7 ) = -scon( 4 );

				Aface( 7, 8 ) = -scon( 4 );
				Aface( 8, 8 ) = hr( 8 ) + scon( 4 ) + hcin;

			}}

			LUdecomposition( Aface, nglface, indx, d ); // Note that these routines change Aface;
			LUsolution( Aface, nglface, indx, Bface ); // face temperatures are returned in Bface

			errtemp = 0.0;
			for ( i = 1; i <= nglface; ++i ) {
				errtemp += std::abs( thetas( i ) - Bface( i ) ) / nglface;
			}

			for ( i = 1; i <= nglface; ++i ) {
				thetas( i ) = 0.5 * ( thetas( i ) + Bface( i ) );
			}

		}

		// No convergence after MaxIterations; and/or error tolerance
		if ( errtemp >= 10 * errtemptol ) {
			// Fatal error: didn't converge
			ShowFatalError( "Convergence error in WindowTempsForNominalCond for construction " + Construct( ConstrNum ).Name );
		}

	}

	//****************************************************************************

	void
	StartingWinTempsForNominalCond()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         F. Winkelmann
		//       DATE WRITTEN   September 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initializes face temperature distribution prior to iteration.
		// This is a shortened form of StartingWindowTemps for use in calculating
		// the nominal center-of-glass U-value.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const hrad( 5.3 ); // Typical radiative conductance (W/m2-K)
		Real64 const hcinStartValue( 3.2 ); // Starting value for inside air film convective
		//   conductance (estimated for typical double glazing
		//   using 1.31(dT**0.333), where dT =
		//   room air temp - inside surface temp = 14.2K)
		Real64 const resgap( 0.21 ); // Typical gap resistance (m2-K/W)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int i; // Face counter
		Array1D< Real64 > rguess( 11 ); // Combined radiative/convective resistance (m2-K/W) of
		// inside or outside air film, or gap
		Real64 restot; // Total window resistance including outside
		//   and inside air films (m2-K/W)
		Real64 temdiff; // Inside/outside air temperature difference (K)
		Real64 ressum; // Resistance sum (m2-K/W)

		rguess( 1 ) = 1.0 / ( hcout + hrad );
		rguess( nglface + 1 ) = 1.0 / ( hcinStartValue + hrad );

		for ( i = 2; i <= nglface; i += 2 ) {
			rguess( i ) = 1.0 / scon( i / 2 );
			if ( i < nglface ) rguess( i + 1 ) = resgap;
		}
		restot = 0.0;

		for ( i = 1; i <= nglface + 1; ++i ) {
			restot += rguess( i );
		}

		temdiff = tin - tout;
		if ( std::abs( temdiff ) < 0.5 ) temdiff = 2.0;
		ressum = 0.0;

		for ( i = 1; i <= nglface; ++i ) {
			ressum += rguess( i );
			thetas( i ) = ( ressum / restot ) * temdiff + tout;
		}

	}

	//****************************************************************************

	void
	ReportGlass()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   March 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gives a detailed report to the user about
		// the calculation parameters for windows and their associated
		// materials.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataBSDFWindow::winterCondition;
		using DataBSDFWindow::summerCondition;
		using General::POLYF;
		using General::ScanForReports;
		using General::RoundSigDigits;
		// InterpBlind ! Blind profile angle interpolation function
		using WindowComplexManager::CalcComplexWindowThermal;
		using WindowComplexManager::UpdateComplexWindows;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const Roughness( 6, { "VeryRough", "Rough", "MediumRough", "MediumSmooth", "Smooth", "VerySmooth" } );
		static Array1D_string const GasTypeName( {0,4}, { "Custom", "Air", "Argon", "Krypton", "Xenon" } );
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool DoReport( false );
		static bool HasWindows( false );
		static bool HasComplexWindows( false );
		static bool HasEQLWindows( false ); // equivalent layer window defined
		static Real64 TempVar( 0.0 ); // just temporary usage for complex fenestration

		int ThisNum;
		int Layer;
		int BlNum; // Blind number
		int i;
		Real64 NominalConductanceWinter; // Nominal center-of-glass conductance of a window construction
		// for ASHRAE winter conditions (W/m2-K):
		// Inside air temperature = 21.1C (70F)
		// Outside air temperature = -17.8C (0F)
		// Windspeed = 6.71 m/s (15 mph)
		// No solar radiation
		Real64 NominalConductanceSummer; // Nominal center-of-glass conductance of a window construction
		// for ASHRAE summer conditions (W/m2-K):
		// Inside air temperature = 23.9C (75F)
		// Outside air temperature = 35.0C (95F)
		// Windspeed = 3.35 m/s (7.5 mph)
		// 783 W/m2 (248 Btu/h-ft2) incident beam solar radiation normal to glazing
		Real64 SHGCWinter; // Center-of-glass solar heat gain coefficient for ASHRAE
		Real64 SHGCSummer;
		// winter and summer conditions
		Real64 TransSolNorm; // Window construction solar transmittance at normal incidence
		Real64 TransVisNorm; // Window construction visible transmittance at normal incidence
		int errFlag; // Error flag
		std::string SolarDiffusing; // 'Yes' if glass is solar diffusing; otherwise 'No' (clear glass)
		std::string SpectralDataName;
		std::string OpticalDataType;
		std::string SlateOrientation;
		std::string GapVentType;

		// Formats
		static gio::Fmt Format_700( "(' WindowConstruction',8(',',A))" );
		static gio::Fmt Format_702( "(' WindowMaterial:Gas',3(',',A))" );
		static gio::Fmt Format_703( "(' WindowMaterial:Shade,',7(',',A))" );
		static gio::Fmt Format_704( "(' WindowMaterial:Blind',8(',',A))" );
		static gio::Fmt Format_706( "(' WindowMaterial:Screen',11(',',A))" );
		static gio::Fmt Format_707( "(' WindowMaterial:Glazing',16(',',A))" );
		static gio::Fmt Format_708( "(' WindowMaterial:Glazing:EquivalentLayer',17(',',A))" );
		static gio::Fmt Format_709( "(' WindowMaterial:Shade:EquivalentLayer',10(',',A))" );
		static gio::Fmt Format_710( "(' WindowMaterial:Drape:EquivalentLayer',11(',',A))" );
		static gio::Fmt Format_711( "(' WindowMaterial:Screen:EquivalentLayer',11(',',A))" );
		static gio::Fmt Format_712( "(' WindowMaterial:Blind:EquivalentLayer',16(',',A))" );
		static gio::Fmt Format_713( "(' WindowMaterial:Gap:EquivalentLayer',4(',',A))" );
		static gio::Fmt Format_799( "(' Construction:WindowEquivalentLayer',6(',',A))" );
		static gio::Fmt Format_800( "(' WindowConstruction:Complex',5(',',A))" );

		ScanForReports( "Constructions", DoReport, "Constructions" );

		//  DO ThisNum=1,TotConstructs
		//    IF (.not. Construct(ThisNum)%TypeIsWindow) CYCLE
		//    HasWindows=.TRUE.
		//    EXIT
		//  ENDDO

		if ( std::any_of( Construct.begin(), Construct.end(), []( ConstructionData const & e ){ return e.TypeIsWindow; } ) ) HasWindows = true;
		if ( std::any_of( Construct.begin(), Construct.end(), []( ConstructionData const & e ){ return e.WindowTypeBSDF; } ) ) HasComplexWindows = true; // Yes, this is a bit different than actually using them.
		if ( std::any_of( Construct.begin(), Construct.end(), []( ConstructionData const & e ){ return e.WindowTypeEQL; } ) ) HasEQLWindows = true; // for reporting purpose only

		//  DO ThisNum=1,TotSurfaces
		//    SurfConstr = Surface(ThisNum)%Construction
		//    IF (SurfConstr /= 0) THEN
		//      IF (Construct(SurfConstr)%WindowTypeBSDF) THEN
		//        HasComplexWindows=.TRUE.
		//        EXIT
		//      END IF
		//    END IF
		//  ENDDO

		if ( DoReport && ( HasWindows || HasComplexWindows || HasEQLWindows ) ) {
			//                                      Write Descriptions

			gio::write( OutputFileInits, fmtA ) << "! <WindowConstruction>,Construction Name,Index,#Layers,Roughness,Conductance {W/m2-K},SHGC,Solar Transmittance at Normal Incidence,Visible Transmittance at Normal Incidence";
			if ( ( TotSimpleWindow > 0 ) || ( W5GlsMat > 0 ) || ( W5GlsMatAlt > 0 ) ) gio::write( OutputFileInits, fmtA ) << "! <WindowMaterial:Glazing>, Material Name, Optical Data Type, Spectral Data Set Name, Thickness {m}, Solar Transmittance,Front Solar Reflectance, Back Solar Reflectance, Visible Transmittance, Front Visible Reflectance,Back Visible Reflectance,Infrared Transmittance, Front Thermal Emissivity, Back Thermal Emissivity,Conductivity {W/m-K},Dirt Factor,Solar Diffusing";
			if ( ( W5GasMat > 0 ) || ( W5GasMatMixture > 0 ) ) gio::write( OutputFileInits, fmtA ) << "! <WindowMaterial:Gas>,Material Name,GasType,Thickness {m}";
			if ( TotShades > 0 ) gio::write( OutputFileInits, fmtA ) << "! <WindowMaterial:Shade>,Material Name,Thickness {m},Conductivity {W/m-K},Thermal Absorptance,Transmittance,Visible Transmittance,Shade Reflectance";
			if ( TotScreens > 0 ) gio::write( OutputFileInits, fmtA ) << "! <WindowMaterial:Screen>,Material Name,Thickness {m},Conductivity {W/m-K},Thermal Absorptance,Transmittance,Reflectance,Visible Reflectance,Diffuse Reflectance,Diffuse Visible Reflectance,Screen Material Diameter To Spacing Ratio,Screen To GlassDistance {m}";
			if ( TotBlinds > 0 ) gio::write( OutputFileInits, fmtA ) << "! <WindowMaterial:Blind>,Material Name,Slat Width {m},Slat Separation {m},Slat Thickness {m},Slat Angle {deg},Slat Beam Solar Transmittance,Slat Beam Solar Front Reflectance,Blind To Glass Distance {m}";

			if ( HasComplexWindows ) gio::write( OutputFileInits, fmtA ) << "! <WindowConstruction:Complex>,Construction Name,Index,#Layers,U-factor {W/m2-K},SHGC";

			if ( HasEQLWindows ) gio::write( OutputFileInits, fmtA ) << "! <Construction:WindowEquivalentLayer>,Construction Name,Index,#Layers,U-factor {W/m2-K},SHGC, Solar Transmittance at Normal Incidence";
			if ( W5GlsMatEQL > 0 ) gio::write( OutputFileInits, fmtA ) << "! <WindowMaterial:Glazing:EquivalentLayer>, Material Name, Optical Data Type, Spectral Data Set Name, Front Side Beam-Beam Solar Transmittance, Back Side Beam-Beam Solar Transmittance, Front Side Beam-Beam Solar Reflectance, Back Side Beam-Beam Solar Reflectance, Front Side Beam-Diffuse Solar Transmittance, Back Side Beam-Diffuse Solar Transmittance, , Front Side Beam-Diffuse Solar Reflectance, Back Side Beam-Diffuse Solar Reflectance, Diffuse-Diffuse Solar Transmittance, Front Side Diffuse-Diffuse Solar Reflectance, Back Side Diffuse-Diffuse Solar Reflectance, Infrared Transmittance, Front Side Infrared Emissivity, Back Side Infrared Emissivity";
			if ( TotShadesEQL > 0 ) gio::write( OutputFileInits, fmtA ) << "! <WindowMaterial:Shade:EquivalentLayer>, Material Name, Front Side Beam-Beam Solar Transmittance, Back Side Beam-Beam Solar Transmittance, Front Side Beam-Diffuse Solar Transmittance, Back Side Beam-Diffuse Solar Transmittance, , Front Side Beam-Diffuse Solar Reflectance, Back Side Beam-Diffuse Solar Reflectance, Infrared Transmittance, Front Side Infrared Emissivity, Back Side Infrared Emissivity";

			if ( TotDrapesEQL > 0 ) gio::write( OutputFileInits, fmtA ) << "! <WindowMaterial:Drape:EquivalentLayer>, Material Name, Front Side Beam-Beam Solar Transmittance, Back Side Beam-Beam Solar Transmittance, Front Side Beam-Diffuse Solar Transmittance, Back Side Beam-Diffuse Solar Transmittance, , Front Side Beam-Diffuse Solar Reflectance, Back Side Beam-Diffuse Solar Reflectance, Infrared Transmittance, Front Side Infrared Emissivity, Back Side Infrared Emissivity, Width of Pleated Fabric, Length of Pleated Fabric";

			if ( TotBlindsEQL > 0 ) gio::write( OutputFileInits, fmtA ) << "! <WindowMaterial:Blind:EquivalentLayer>, Material Name, Slat Orientation, Slat Width, Slat Separation, Slat Crown, Slat Angle, Front Side Slate Beam-Diffuse Solar Transmittance, Back Side Slate Beam-Diffuse Solar Transmittance, Front Side Slate Beam-Diffuse Solar Reflectance, Back Side Slate Beam-Diffuse Solar Reflectance, Slat Diffuse-Diffuse Solar Transmittance, Front Side Slat Diffuse-Diffuse Solar Reflectance, Back Side Slat Diffuse-Diffuse Solar Reflectance, Infrared Transmittance, Front Side Infrared Emissivity, Back Side Infrared Emissivity, Slat Angle Control";
			if ( TotScreensEQL > 0 ) gio::write( OutputFileInits, fmtA ) << "! <WindowMaterial:Screen:EquivalentLayer>, Material Name, Screen Beam-Beam Solar Transmittance, Screen Beam-Diffuse Solar Transmittance, Screen Beam-Diffuse Solar Reflectance, Screen Infrared Transmittance, Screen Infrared Emissivity, Screen Wire Spacing, Screen Wire Diameter";
			if ( W5GapMatEQL > 0 ) gio::write( OutputFileInits, fmtA ) << "! <WindowMaterial:Gap:EquivalentLayer>, Material Name, GasType, Gap Thickness {m}, Gap Vent Type";

			for ( ThisNum = 1; ThisNum <= TotConstructs; ++ThisNum ) {

				if ( Construct( ThisNum ).WindowTypeBSDF ) {

					i = ThisNum;
					CalcComplexWindowThermal( 0, i, TempVar, TempVar, TempVar, TempVar, winterCondition );
					CalcComplexWindowThermal( 0, i, TempVar, TempVar, TempVar, TempVar, summerCondition );

					gio::write( OutputFileInits, Format_800 ) << Construct( ThisNum ).Name << RoundSigDigits( ThisNum ) << RoundSigDigits( Construct( ThisNum ).TotSolidLayers ) << RoundSigDigits( NominalU( ThisNum ), 3 ) << RoundSigDigits( Construct( ThisNum ).SummerSHGC, 3 );

				} else if ( Construct( ThisNum ).TypeIsWindow ) {
					// Calculate for ASHRAE winter and summer conditions:
					// (1) nominal center-of-glass conductance, including inside and outside air films,
					// (2) solar heat gain coefficient (SHGC),
					// (3) solar transmittance at normal incidence, and (4) visible transmittance at normal incidence.

					if ( Construct( ThisNum ).WindowTypeEQL ) {
						// for equivalent layer Window already calculated
						// NominalU(ThisNum)=NominalConductanceWinter
						// Save the SHGC for later use in tabular report IVRS
						// Construct(ThisNum)%SummerSHGC = SHGCSummer
						Construct( ThisNum ).VisTransNorm = 0.0; // TODO list

						gio::write( OutputFileInits, Format_799 ) << Construct( ThisNum ).Name << RoundSigDigits( ThisNum ) << RoundSigDigits( Construct( ThisNum ).TotSolidLayers ) << RoundSigDigits( NominalU( ThisNum ), 3 ) << RoundSigDigits( Construct( ThisNum ).SummerSHGC, 3 ) << RoundSigDigits( Construct( ThisNum ).SolTransNorm, 3 );

					} else {

						CalcNominalWindowCond( ThisNum, 1, NominalConductanceWinter, SHGCWinter, TransSolNorm, TransVisNorm, errFlag );

						if ( errFlag == 1 ) {
							ShowWarningError( "Window construction " + Construct( ThisNum ).Name + " has an interior or exterior blind" );
							ShowContinueError( "but the corresponding construction without the blind cannot be found." );
							ShowContinueError( "The ReportGlass entry for this construction will not be printed in eplusout.eio." );
							continue;
						}

						// Skip constructions with between-glass shade/blind until method is worked out to determine
						// nominal conductance and SHGC.

						if ( errFlag == 2 ) {
							ShowWarningError( "Window construction " + Construct( ThisNum ).Name + " has a between-glass shade or blind" );
							ShowContinueError( "The ReportGlass entry for this construction will not be printed in eplusout.eio." );
							continue;
						}

						NominalU( ThisNum ) = NominalConductanceWinter;
						if ( ! Construct( ThisNum ).WindowTypeEQL ) {
							CalcNominalWindowCond( ThisNum, 2, NominalConductanceSummer, SHGCSummer, TransSolNorm, TransVisNorm, errFlag );
						}
						// Save the SHGC for later use in tabular report IVRS
						Construct( ThisNum ).SummerSHGC = SHGCSummer;
						Construct( ThisNum ).VisTransNorm = TransVisNorm;

						gio::write( OutputFileInits, Format_700 ) << Construct( ThisNum ).Name << RoundSigDigits( ThisNum ) << RoundSigDigits( Construct( ThisNum ).TotLayers ) << Roughness( Construct( ThisNum ).OutsideRoughness ) << RoundSigDigits( NominalConductanceWinter, 3 ) << RoundSigDigits( SHGCSummer, 3 ) << RoundSigDigits( TransSolNorm, 3 ) << RoundSigDigits( TransVisNorm, 3 );
					}
					//    Write(OutputFileConstrainParams, 705)  TRIM(Construct(ThisNum)%Name), SHGCSummer ,TransVisNorm

					for ( i = 1; i <= Construct( ThisNum ).TotLayers; ++i ) {
						Layer = Construct( ThisNum ).LayerPoint( i );
						{ auto const SELECT_CASE_var( Material( Layer ).Group );
						if ( SELECT_CASE_var == WindowGas ) {
							gio::write( OutputFileInits, Format_702 ) << Material( Layer ).Name << GasTypeName( Material( Layer ).GasType( 1 ) ) << RoundSigDigits( Material( Layer ).Thickness, 3 );

							//!fw CASE(WindowGasMixture)

						} else if ( SELECT_CASE_var == Shade ) {
							gio::write( OutputFileInits, Format_703 ) << Material( Layer ).Name << RoundSigDigits( Material( Layer ).Thickness, 3 ) << RoundSigDigits( Material( Layer ).Conductivity, 3 ) << RoundSigDigits( Material( Layer ).AbsorpThermal, 3 ) << RoundSigDigits( Material( Layer ).Trans, 3 ) << RoundSigDigits( Material( Layer ).TransVis, 3 ) << RoundSigDigits( Material( Layer ).ReflectShade, 3 );

						} else if ( SELECT_CASE_var == WindowBlind ) {
							BlNum = Material( Layer ).BlindDataPtr;
							gio::write( OutputFileInits, Format_704 ) << Material( Layer ).Name << RoundSigDigits( Blind( BlNum ).SlatWidth, 4 ) << RoundSigDigits( Blind( BlNum ).SlatSeparation, 4 ) << RoundSigDigits( Blind( BlNum ).SlatThickness, 4 ) << RoundSigDigits( Blind( BlNum ).SlatAngle, 3 ) << RoundSigDigits( Blind( BlNum ).SlatTransSolBeamDiff, 3 ) << RoundSigDigits( Blind( BlNum ).SlatFrontReflSolBeamDiff, 3 ) << RoundSigDigits( Blind( BlNum ).BlindToGlassDist, 3 );
						} else if ( SELECT_CASE_var == Screen ) {
							if ( Material( Layer ).ScreenDataPtr > 0 ) gio::write( OutputFileInits, Format_706 ) << Material( Layer ).Name << RoundSigDigits( Material( Layer ).Thickness, 5 ) << RoundSigDigits( Material( Layer ).Conductivity, 3 ) << RoundSigDigits( Material( Layer ).AbsorpThermal, 3 ) << RoundSigDigits( SurfaceScreens( Material( Layer ).ScreenDataPtr ).BmBmTrans, 3 ) << RoundSigDigits( SurfaceScreens( Material( Layer ).ScreenDataPtr ).ReflectSolBeamFront, 3 ) << RoundSigDigits( SurfaceScreens( Material( Layer ).ScreenDataPtr ).ReflectVisBeamFront, 3 ) << RoundSigDigits( SurfaceScreens( Material( Layer ).ScreenDataPtr ).DifReflect, 3 ) << RoundSigDigits( SurfaceScreens( Material( Layer ).ScreenDataPtr ).DifReflectVis, 3 ) << RoundSigDigits( SurfaceScreens( Material( Layer ).ScreenDataPtr ).ScreenDiameterToSpacingRatio, 3 ) << RoundSigDigits( Material( Layer ).WinShadeToGlassDist, 3 );

						} else if ( ( SELECT_CASE_var == WindowGlass ) || ( SELECT_CASE_var == WindowSimpleGlazing ) ) {
							SolarDiffusing = "No";
							if ( Material( Layer ).SolarDiffusing ) SolarDiffusing = "Yes";
							OpticalDataType = "SpectralAverage";
							SpectralDataName = "";
							if ( Material( Layer ).GlassSpectralDataPtr > 0 ) {
								OpticalDataType = "Spectral";
								SpectralDataName = SpectralData( Material( Layer ).GlassSpectralDataPtr ).Name;
							}
							gio::write( OutputFileInits, Format_707 ) << Material( Layer ).Name << OpticalDataType << SpectralDataName << RoundSigDigits( Material( Layer ).Thickness, 5 ) << RoundSigDigits( Material( Layer ).Trans, 5 ) << RoundSigDigits( Material( Layer ).ReflectSolBeamFront, 5 ) << RoundSigDigits( Material( Layer ).ReflectSolBeamBack, 5 ) << RoundSigDigits( Material( Layer ).TransVis, 5 ) << RoundSigDigits( Material( Layer ).ReflectVisBeamFront, 5 ) << RoundSigDigits( Material( Layer ).ReflectVisBeamBack, 5 ) << RoundSigDigits( Material( Layer ).TransThermal, 5 ) << RoundSigDigits( Material( Layer ).AbsorpThermalFront, 5 ) << RoundSigDigits( Material( Layer ).AbsorpThermalBack, 5 ) << RoundSigDigits( Material( Layer ).Conductivity, 5 ) << RoundSigDigits( Material( Layer ).GlassTransDirtFactor, 5 ) << SolarDiffusing;

						} else if ( SELECT_CASE_var == GlassEquivalentLayer ) {
							OpticalDataType = "SpectralAverage";
							SpectralDataName = "";
							gio::write( OutputFileInits, Format_708 ) << Material( Layer ).Name << OpticalDataType << SpectralDataName << RoundSigDigits( Material( Layer ).TausFrontBeamBeam, 5 ) << RoundSigDigits( Material( Layer ).TausBackBeamBeam, 5 ) << RoundSigDigits( Material( Layer ).ReflFrontBeamBeam, 5 ) << RoundSigDigits( Material( Layer ).ReflBackBeamBeam, 5 ) << RoundSigDigits( Material( Layer ).TausFrontBeamDiff, 5 ) << RoundSigDigits( Material( Layer ).TausBackBeamDiff, 5 ) << RoundSigDigits( Material( Layer ).ReflFrontBeamDiff, 5 ) << RoundSigDigits( Material( Layer ).ReflBackBeamDiff, 5 ) << RoundSigDigits( Material( Layer ).TausDiffDiff, 5 ) << RoundSigDigits( Material( Layer ).ReflFrontDiffDiff, 5 ) << RoundSigDigits( Material( Layer ).ReflBackDiffDiff, 5 ) << RoundSigDigits( Material( Layer ).TausThermal, 5 ) << RoundSigDigits( Material( Layer ).EmissThermalFront, 5 ) << RoundSigDigits( Material( Layer ).EmissThermalBack, 5 );

						} else if ( SELECT_CASE_var == ShadeEquivalentLayer ) {
							gio::write( OutputFileInits, Format_709 ) << Material( Layer ).Name << RoundSigDigits( Material( Layer ).TausFrontBeamBeam, 4 ) << RoundSigDigits( Material( Layer ).TausBackBeamBeam, 4 ) << RoundSigDigits( Material( Layer ).TausFrontBeamDiff, 4 ) << RoundSigDigits( Material( Layer ).TausBackBeamDiff, 4 ) << RoundSigDigits( Material( Layer ).ReflFrontBeamDiff, 4 ) << RoundSigDigits( Material( Layer ).ReflBackBeamDiff, 4 ) << RoundSigDigits( Material( Layer ).TausThermal, 4 ) << RoundSigDigits( Material( Layer ).EmissThermalFront, 4 ) << RoundSigDigits( Material( Layer ).EmissThermalBack, 4 );

						} else if ( SELECT_CASE_var == DrapeEquivalentLayer ) {
							gio::write( OutputFileInits, Format_710 ) << Material( Layer ).Name << RoundSigDigits( Material( Layer ).TausFrontBeamBeam, 4 ) << RoundSigDigits( Material( Layer ).TausFrontBeamDiff, 4 ) << RoundSigDigits( Material( Layer ).TausBackBeamDiff, 4 ) << RoundSigDigits( Material( Layer ).ReflFrontBeamDiff, 4 ) << RoundSigDigits( Material( Layer ).ReflBackBeamDiff, 4 ) << RoundSigDigits( Material( Layer ).TausThermal, 4 ) << RoundSigDigits( Material( Layer ).EmissThermalFront, 4 ) << RoundSigDigits( Material( Layer ).EmissThermalBack, 4 ) << RoundSigDigits( Material( Layer ).PleatedDrapeWidth, 5 ) << RoundSigDigits( Material( Layer ).PleatedDrapeLength, 5 );

						} else if ( SELECT_CASE_var == ScreenEquivalentLayer ) {
							gio::write( OutputFileInits, Format_711 ) << Material( Layer ).Name << RoundSigDigits( Material( Layer ).TausFrontBeamBeam, 4 ) << RoundSigDigits( Material( Layer ).TausFrontBeamDiff, 4 ) << RoundSigDigits( Material( Layer ).TausBackBeamDiff, 4 ) << RoundSigDigits( Material( Layer ).ReflFrontBeamDiff, 4 ) << RoundSigDigits( Material( Layer ).ReflBackBeamDiff, 4 ) << RoundSigDigits( Material( Layer ).TausThermal, 4 ) << RoundSigDigits( Material( Layer ).EmissThermalFront, 4 ) << RoundSigDigits( Material( Layer ).EmissThermalBack, 4 ) << RoundSigDigits( Material( Layer ).ScreenWireSpacing, 5 ) << RoundSigDigits( Material( Layer ).ScreenWireDiameter, 5 );

						} else if ( SELECT_CASE_var == BlindEquivalentLayer ) {
							SlateOrientation = "Horizontal";
							if ( Material( Layer ).SlatOrientation == Vertical ) {
								SlateOrientation = "Vertical";
							}
							gio::write( OutputFileInits, Format_712 ) << Material( Layer ).Name << SlateOrientation << RoundSigDigits( Material( Layer ).SlatWidth, 5 ) << RoundSigDigits( Material( Layer ).SlatSeparation, 5 ) << RoundSigDigits( Material( Layer ).SlatCrown, 5 ) << RoundSigDigits( Material( Layer ).SlatAngle, 5 ) << RoundSigDigits( Material( Layer ).TausFrontBeamDiff, 5 ) << RoundSigDigits( Material( Layer ).TausBackBeamDiff, 5 ) << RoundSigDigits( Material( Layer ).ReflFrontBeamDiff, 5 ) << RoundSigDigits( Material( Layer ).ReflBackBeamDiff, 5 ) << RoundSigDigits( Material( Layer ).TausDiffDiff, 5 ) << RoundSigDigits( Material( Layer ).ReflFrontDiffDiff, 5 ) << RoundSigDigits( Material( Layer ).ReflBackDiffDiff, 5 ) << RoundSigDigits( Material( Layer ).TausThermal, 5 ) << RoundSigDigits( Material( Layer ).EmissThermalFront, 5 ) << RoundSigDigits( Material( Layer ).EmissThermalBack, 5 );

						} else if ( SELECT_CASE_var == GapEquivalentLayer ) {
							GapVentType = "Sealed";
							if ( Material( Layer ).GapVentType == 2 ) {
								GapVentType = "VentedIndoor";
							} else if ( Material( Layer ).GapVentType == 3 ) {
								GapVentType = "VentedOutdoor";
							}
							gio::write( OutputFileInits, Format_713 ) << Material( Layer ).Name << GasTypeName( Material( Layer ).GasType( 1 ) ) << RoundSigDigits( Material( Layer ).Thickness, 3 ) << GapVentType;
						}}
					}

				}

			}

		} else if ( HasWindows ) {

			for ( ThisNum = 1; ThisNum <= TotConstructs; ++ThisNum ) {

				if ( ! Construct( ThisNum ).TypeIsWindow ) continue;
				if ( Construct( ThisNum ).WindowTypeEQL ) continue; // skip if equivalent layer window

				// Calculate for ASHRAE winter and summer conditions: (1)nominal center-of-glass conductance,
				// (2) solar heat gain coefficient (SHGC), including inside and outside air films,
				// (3) solar transmittance at normal incidence, and (4) visible transmittance at normal incidence.

				CalcNominalWindowCond( ThisNum, 1, NominalConductanceWinter, SHGCWinter, TransSolNorm, TransVisNorm, errFlag );

				if ( errFlag == 1 || errFlag == 2 ) continue;
				NominalU( ThisNum ) = NominalConductanceWinter;

			}

		}

		//IF (HasComplexWindows) THEN
		//DO ThisNum=1,TotSurfaces
		//  SurfConstr = Surface(ThisNum)%Construction
		//  IF (SurfConstr /= 0) THEN
		//    IF (Construct(SurfConstr)%WindowTypeBSDF) THEN
		//      Write(OutputFileInits,'(A)') '! <WindowConstructionComplex>,Construction Name,Index,#Layers,'//  &
		//                         'U-factor {W/m2-K},SHGC'
		//      CALL CalcComplexWindowThermal(ThisNum, TempVar, TempVar, TempVar, TempVar, winterCondition)
		//      CALL CalcComplexWindowThermal(ThisNum, TempVar, TempVar, TempVar, TempVar, summerCondition)
		//      Write(OutputFileInits,800) TRIM(Construct(SurfConstr)%Name), TRIM(RoundSigDigits(SurfConstr)), &
		//                                 TRIM(RoundSigDigits(Construct(SurfConstr)%TotSolidLayers)), &
		//                                 TRIM(RoundSigDigits(NominalU(SurfConstr),3)), &
		//                                 TRIM(RoundSigDigits(Construct(SurfConstr)%SummerSHGC,3))
		//    END IF
		//  END IF
		//ENDDO

		//END IF

	}

	//*************************************************************************************

	void
	CalcWindowBlindProperties()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Hans Simmler
		//       DATE WRITTEN   July-Aug 1995
		//       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
		//                      Dec 2001 (FCW): add variable slat angle
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates solar-optical properties of a window blind
		// from slat properties and solar profile angle. Assumes flat slats.

		// METHODOLOGY EMPLOYED:
		// The solar profile angle is varied from -90 to +90 deg and slat angle is varied from 0 to 180deg,
		// covering the full range of possible profile angles and slat angles.
		// (The profile angle is defined as the angle of incidence when the radiation
		// source is located in a plane that (1)is perpendicular to the  plane of the blinds [which is
		// the same as the window plane] and (2) contains the slat normal vector.)

		// In the time-step calculation,the blind properties vs. profile angle and slat angle
		// that are calculated here will be applicable to windows and slats
		// of arbitrary orientation, and to arbitrary sun positions, as long as the appropiate
		// profile angle is used. The slat angle for a particular window with blinds is determined
		// each time step in subroutine WindowShadingManager on the basis of user-specified
		// slat control options.

		// REFERENCES:
		// "Solar-Thermal Window Blind Model for DOE-2," H. Simmler, U. Fischer and
		// F. Winkelmann, Lawrence Berkeley National Laboratory, Jan. 1996.

		// USE STATEMENTS:na
		// Using/Aliasing
		using InputProcessor::SameString;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:na
		// SUBROUTINE PARAMETER DEFINITIONS:na
		// INTERFACE BLOCK SPECIFICATIONS:na
		// DERIVED TYPE DEFINITIONS:na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Array1D< Real64 > bld_pr( 15 ); // Slat properties
		Array1D< Real64 > st_lay( 16 ); // Solar-optical blind/glazing system properties
		Real64 sun_el; // Solar profile angle (radians)
		Array1D< Real64 > sun_el_deg( 37 ); // Solar profile angle (deg) corresponding to sun_el values
		Real64 bld_el; // Slat angle (elevation of slat normal vector in plane
		//  perpendicular to window and containing the slat normal vector) (radians)
		int ISolVis; // 1 = do solar and IR calculation; 2 = do visible calculation
		int IProfAng; // Profile angle index
		int BlindNum; // Blind number
		int ISlatAng; // Slat angle index

		for ( BlindNum = 1; BlindNum <= TotBlinds; ++BlindNum ) {

			bld_pr( 2 ) = Blind( BlindNum ).SlatWidth;
			bld_pr( 3 ) = Blind( BlindNum ).SlatSeparation;

			for ( ISolVis = 1; ISolVis <= 2; ++ISolVis ) {
				if ( ISolVis == 1 ) { // For solar calculation
					bld_pr( 4 ) = 0.0;
					bld_pr( 5 ) = 0.0;
					bld_pr( 6 ) = 0.0;
					bld_pr( 7 ) = Blind( BlindNum ).SlatTransSolBeamDiff;
					bld_pr( 8 ) = Blind( BlindNum ).SlatFrontReflSolBeamDiff;
					bld_pr( 9 ) = Blind( BlindNum ).SlatBackReflSolBeamDiff;
					bld_pr( 10 ) = Blind( BlindNum ).SlatTransSolDiffDiff;
					bld_pr( 11 ) = Blind( BlindNum ).SlatFrontReflSolDiffDiff;
					bld_pr( 12 ) = Blind( BlindNum ).SlatBackReflSolDiffDiff;
				} else { // For visible calculation
					bld_pr( 4 ) = 0.0;
					bld_pr( 5 ) = 0.0;
					bld_pr( 6 ) = 0.0;
					bld_pr( 7 ) = Blind( BlindNum ).SlatTransVisBeamDiff;
					bld_pr( 8 ) = Blind( BlindNum ).SlatFrontReflVisBeamDiff;
					bld_pr( 9 ) = Blind( BlindNum ).SlatBackReflVisBeamDiff;
					bld_pr( 10 ) = Blind( BlindNum ).SlatTransVisDiffDiff;
					bld_pr( 11 ) = Blind( BlindNum ).SlatFrontReflVisDiffDiff;
					bld_pr( 12 ) = Blind( BlindNum ).SlatBackReflVisDiffDiff;
				}
				// For IR calculation
				bld_pr( 13 ) = Blind( BlindNum ).SlatTransIR;
				bld_pr( 14 ) = Blind( BlindNum ).SlatFrontEmissIR;
				bld_pr( 15 ) = Blind( BlindNum ).SlatBackEmissIR;

				// Calculate diffuse properties of blind. If blind has variable slat angle, &
				// vary slat angle from 0 to 180 deg in 10-deg steps (for MaxSlatAngs = 19).
				// If blind has fixed slat angle, calculate properties at that angle only.

				for ( ISlatAng = 1; ISlatAng <= MaxSlatAngs; ++ISlatAng ) {

					st_lay = 0.0;
					if ( Blind( BlindNum ).SlatAngleType == FixedSlats ) {
						bld_el = Blind( BlindNum ).SlatAngle * DegToRadians;
					} else { // Variable slat angle
						bld_el = ( Pi / ( MaxSlatAngs - 1 ) ) * ( ISlatAng - 1 ); // 0 <= bld_el <= 180 deg
					}
					BlindOpticsDiffuse( BlindNum, ISolVis, bld_pr, bld_el, st_lay );

					if ( ISolVis == 1 ) { // Fill blind diffuse solar and IR properties
						Blind( BlindNum ).SolFrontDiffDiffTrans( ISlatAng ) = st_lay( 9 );
						Blind( BlindNum ).SolFrontDiffDiffRefl( ISlatAng ) = st_lay( 10 );
						Blind( BlindNum ).SolBackDiffDiffTrans( ISlatAng ) = st_lay( 11 );
						Blind( BlindNum ).SolBackDiffDiffRefl( ISlatAng ) = st_lay( 12 );
						Blind( BlindNum ).SolFrontDiffAbs( ISlatAng ) = max( 0.0, 1.0 - st_lay( 9 ) - st_lay( 10 ) );
						Blind( BlindNum ).SolBackDiffAbs( ISlatAng ) = max( 0.0, 1.0 - st_lay( 11 ) - st_lay( 12 ) );
						Blind( BlindNum ).IRFrontTrans( ISlatAng ) = st_lay( 13 );
						Blind( BlindNum ).IRFrontEmiss( ISlatAng ) = st_lay( 14 );
						//Blind(BlindNum)%IRBackTrans(ISlatAng)           = st_lay(15)
						//Blind(BlindNum)%IRBackEmiss(ISlatAng)           = st_lay(16)
						//  Above two lines are incorrect; replaced by (FCW, 2/10/03)
						Blind( BlindNum ).IRBackTrans( ISlatAng ) = st_lay( 13 );
						Blind( BlindNum ).IRBackEmiss( ISlatAng ) = st_lay( 15 );
					} else { // Fill blind diffuse visible properties
						Blind( BlindNum ).VisFrontDiffDiffTrans( ISlatAng ) = st_lay( 9 );
						Blind( BlindNum ).VisFrontDiffDiffRefl( ISlatAng ) = st_lay( 10 );
						Blind( BlindNum ).VisBackDiffDiffTrans( ISlatAng ) = st_lay( 11 );
						Blind( BlindNum ).VisBackDiffDiffRefl( ISlatAng ) = st_lay( 12 );
					}

					if ( Blind( BlindNum ).SlatAngleType == FixedSlats ) break;
				} // End of slat angle loop

				// Calculate beam properties of blind. Vary profile angle from -90 to +90 deg in 5-deg steps.
				// If blind has variable slat angle, vary slat angle from 0 to 180 deg in 10-deg steps
				// (for MaxSlatAngs = 19). If blind has fixed slat angle, calculate properties at that angle only.

				for ( IProfAng = 1; IProfAng <= 37; ++IProfAng ) {
					sun_el = -Pi / 2.0 + ( Pi / 36.0 ) * ( IProfAng - 1 );
					sun_el_deg( IProfAng ) = 57.2958 * sun_el;

					for ( ISlatAng = 1; ISlatAng <= MaxSlatAngs; ++ISlatAng ) {
						st_lay = 0.0;
						if ( Blind( BlindNum ).SlatAngleType == FixedSlats ) {
							bld_el = Blind( BlindNum ).SlatAngle * DegToRadians;
						} else { // Variable slat angle
							bld_el = ( Pi / ( MaxSlatAngs - 1 ) ) * ( ISlatAng - 1 ); // 0 <= bld_el <= 180 deg
						}

						// Beam solar-optical properties of blind for given profile angle and slat angle

						BlindOpticsBeam( BlindNum, bld_pr, bld_el, sun_el, st_lay );

						if ( ISolVis == 1 ) { // Fill blind beam solar properties
							Blind( BlindNum ).SolFrontBeamBeamTrans( ISlatAng, IProfAng ) = st_lay( 1 );
							Blind( BlindNum ).SolFrontBeamBeamRefl( ISlatAng, IProfAng ) = st_lay( 2 );
							Blind( BlindNum ).SolBackBeamBeamTrans( ISlatAng, IProfAng ) = st_lay( 3 );
							Blind( BlindNum ).SolBackBeamBeamRefl( ISlatAng, IProfAng ) = st_lay( 4 );
							Blind( BlindNum ).SolFrontBeamDiffTrans( ISlatAng, IProfAng ) = st_lay( 5 );
							Blind( BlindNum ).SolFrontBeamDiffRefl( ISlatAng, IProfAng ) = st_lay( 6 );
							Blind( BlindNum ).SolBackBeamDiffTrans( ISlatAng, IProfAng ) = st_lay( 7 );
							Blind( BlindNum ).SolBackBeamDiffRefl( ISlatAng, IProfAng ) = st_lay( 8 );
							Blind( BlindNum ).SolFrontBeamAbs( ISlatAng, IProfAng ) = max( 0.0, 1.0 - st_lay( 6 ) - st_lay( 1 ) - st_lay( 5 ) );
							Blind( BlindNum ).SolBackBeamAbs( ISlatAng, IProfAng ) = max( 0.0, 1.0 - st_lay( 7 ) - st_lay( 3 ) - st_lay( 8 ) );

						} else { // Fill blind beam visible properties
							Blind( BlindNum ).VisFrontBeamBeamTrans( ISlatAng, IProfAng ) = st_lay( 1 );
							Blind( BlindNum ).VisFrontBeamBeamRefl( ISlatAng, IProfAng ) = st_lay( 2 );
							Blind( BlindNum ).VisBackBeamBeamTrans( ISlatAng, IProfAng ) = st_lay( 3 );
							Blind( BlindNum ).VisBackBeamBeamRefl( ISlatAng, IProfAng ) = st_lay( 4 );
							Blind( BlindNum ).VisFrontBeamDiffTrans( ISlatAng, IProfAng ) = st_lay( 5 );
							Blind( BlindNum ).VisFrontBeamDiffRefl( ISlatAng, IProfAng ) = st_lay( 6 );
							Blind( BlindNum ).VisBackBeamDiffTrans( ISlatAng, IProfAng ) = st_lay( 7 );
							Blind( BlindNum ).VisBackBeamDiffRefl( ISlatAng, IProfAng ) = st_lay( 8 );
						}

						if ( Blind( BlindNum ).SlatAngleType == FixedSlats ) break;
					} // End of loop over slat angles
				} // End of loop over profile angles

				if ( ISolVis == 1 ) {
					for ( ISlatAng = 1; ISlatAng <= MaxSlatAngs; ++ISlatAng ) {
						Blind( BlindNum ).SolFrontDiffDiffTransGnd( ISlatAng ) = DiffuseAverageProfAngGnd( Blind( BlindNum ).SolFrontBeamBeamTrans( ISlatAng, {1,37} ) ) + DiffuseAverageProfAngGnd( Blind( BlindNum ).SolFrontBeamDiffTrans( ISlatAng, {1,37} ) );
						Blind( BlindNum ).SolFrontDiffDiffTransSky( ISlatAng ) = DiffuseAverageProfAngSky( Blind( BlindNum ).SolFrontBeamBeamTrans( ISlatAng, {1,37} ) ) + DiffuseAverageProfAngSky( Blind( BlindNum ).SolFrontBeamDiffTrans( ISlatAng, {1,37} ) );
						Blind( BlindNum ).SolFrontDiffAbsGnd( ISlatAng ) = DiffuseAverageProfAngGnd( Blind( BlindNum ).SolFrontBeamAbs( ISlatAng, {1,37} ) );
						Blind( BlindNum ).SolFrontDiffAbsSky( ISlatAng ) = DiffuseAverageProfAngSky( Blind( BlindNum ).SolFrontBeamAbs( ISlatAng, {1,37} ) );
						Blind( BlindNum ).SolFrontDiffDiffReflGnd( ISlatAng ) = DiffuseAverageProfAngGnd( Blind( BlindNum ).SolFrontBeamDiffRefl( ISlatAng, {1,37} ) );
						Blind( BlindNum ).SolFrontDiffDiffReflSky( ISlatAng ) = DiffuseAverageProfAngSky( Blind( BlindNum ).SolFrontBeamDiffRefl( ISlatAng, {1,37} ) );

						// TH 2/17/2010. Added. Loop only for movable slat blinds
						if ( Blind( BlindNum ).SlatAngleType == FixedSlats ) break;
					}
				}

			} // End of loop over solar vs. visible properties

		} // End of loop over blinds

	}

	//*************************************************************************************

	void
	CalcWindowScreenProperties()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   April 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initialize static properties of window screens.

		// METHODOLOGY EMPLOYED:
		// Loop through all surfaces to determine which window has an exterior screen. Static
		// variables are defined here, dynamic variables are calculated in CalcScreenTransmittance.

		// REFERENCES: na

		// Using/Aliasing
		using InputProcessor::SameString;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		int const M( 18 );
		int const N( 18 );
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // Index to surface number
		int ScreenNum; // Index to each screen used on exterior of window
		int ConstrNumSh; // Index to shaded constuction
		int MatNum; // Index to material number
		int i; // Integration loop counters
		int j;
		Real64 SumTrans; // Integration variable for transmittance
		Real64 SumTransVis; // Integration variable for visible transmittance
		Real64 SumReflect; // Integration variable for reflectance
		Real64 SumReflectVis; // Integration variable for visible reflectance
		Real64 SumArea; // Integration variable for area of quarter hemisphere
		Real64 SkyArea; // Area of integration
		Real64 SunAzimuth; // Azimuth angle of sun during integration
		Real64 SunAltitude; // Altitude angle of sun during integration
		Real64 RelativeAzimuth; // Relative azimuth angle of sun with respect to surface outward normal
		Real64 RelativeAltitude; // Relative altitude angle of sun with respect to surface outward normal
		int ShadingType; // Type of shading device
		int ScreenTransUnitNo; // Unit number of screen transmittance data file
		bool FoundMaterial; // Flag to avoid printing screen transmittance data multiple times when Material:WindowScreen
		// is used on multiple surfaces
		bool PrintTransMap; // Flag used to print transmittance map

		SurfaceScreens.allocate( NumSurfaceScreens );
		ScreenTrans.allocate( NumSurfaceScreens );
		ScreenNum = 0;

		PrintTransMap = false;
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {

			if ( Surface( SurfNum ).WindowShadingControlPtr != 0 ) {
				ConstrNumSh = Surface( SurfNum ).ShadedConstruction;
				MatNum = Construct( ConstrNumSh ).LayerPoint( 1 );
				ShadingType = WindowShadingControl( Surface( SurfNum ).WindowShadingControlPtr ).ShadingType;
				if ( ShadingType == WSC_ST_ExteriorScreen ) {

					if ( Material( MatNum ).ScreenMapResolution > 0 ) PrintTransMap = true;
					++ScreenNum;
					SurfaceWindow( SurfNum ).ScreenNumber = ScreenNum;
					//     If a screen material is used more than once, the Material structure's screen data pointer holds the screen number
					//     of the last window surface. Use this method to access the screen parameter's only for static variables such as
					//     diffuse properties (InitGlassOpticalCalculations). For all cases where the screen properties are a function of
					//     sun azimuth and altitude angles, use the SurfaceScreens structure.
					Material( MatNum ).ScreenDataPtr = ScreenNum;
					SurfaceScreens( ScreenNum ).MaterialNumber = MatNum;
					//     Invert calculation done in GetMaterialInput to find Diameter to Spacing ratio (Props(7)/Props(6))
					//     Material(MaterNum)%Trans = (1 - MaterialProps(7)/MaterialProps(6))**2.0
					SurfaceScreens( ScreenNum ).ScreenDiameterToSpacingRatio = 1.0 - std::sqrt( Material( MatNum ).Trans );

					if ( SameString( Material( MatNum ).ReflectanceModeling, "DoNotModel" ) ) {
						SurfaceScreens( ScreenNum ).ScreenBeamReflectanceAccounting = DoNotModel;
					} else if ( SameString( Material( MatNum ).ReflectanceModeling, "ModelAsDirectBeam" ) ) {
						SurfaceScreens( ScreenNum ).ScreenBeamReflectanceAccounting = ModelAsDirectBeam;
					} else if ( SameString( Material( MatNum ).ReflectanceModeling, "ModelAsDiffuse" ) ) {
						SurfaceScreens( ScreenNum ).ScreenBeamReflectanceAccounting = ModelAsDiffuse;
					}

					// Reflectance of screen material only
					SurfaceScreens( ScreenNum ).ReflectCylinder = Material( MatNum ).ReflectShade / ( 1 - Material( MatNum ).Trans );
					SurfaceScreens( ScreenNum ).ReflectCylinderVis = Material( MatNum ).ReflectShadeVis / ( 1 - Material( MatNum ).Trans );

					//     Integrate the transmittance over a quarter hemisphere for use in diffuse calculations
					SumTrans = 0.0;
					SumTransVis = 0.0;
					SumReflect = 0.0;
					SumReflectVis = 0.0;
					SumArea = 0.0;
					//     Integration over quarter hemisphere in polar coordinates and converting to rectangular to call screen model.
					//     Proceed in reverse order such that the last calculation yields zero sun angle to window screen normal (angles=0,0).
					//     The properties calculated at zero sun angle are then used elsewhere prior to the start of the actual simulation.
					for ( j = N; j >= 1; --j ) {
						for ( i = M; i >= 1; --i ) {
							SunAzimuth = ( 90.0 / N ) * ( j - 1 ) * ( Pi / 180.0 );
							SunAltitude = ( 90.0 / M ) * ( i - 1 ) * ( Pi / 180.0 );
							SkyArea = std::sin( SunAltitude ) * std::cos( SunAltitude );
							//         Integrate transmittance using coordiante transform
							RelativeAzimuth = std::asin( std::sin( SunAltitude ) * std::cos( SunAzimuth ) ); // phi prime
							RelativeAltitude = std::atan( std::tan( SunAltitude ) * std::sin( SunAzimuth ) ); // alpha
							CalcScreenTransmittance( 0, RelativeAltitude, RelativeAzimuth, ScreenNum );
							SumTrans += ( SurfaceScreens( ScreenNum ).BmBmTrans + SurfaceScreens( ScreenNum ).BmDifTrans ) * SkyArea;
							SumTransVis += ( SurfaceScreens( ScreenNum ).BmBmTransVis + SurfaceScreens( ScreenNum ).BmDifTransVis ) * SkyArea;
							SumReflect += SurfaceScreens( ScreenNum ).ReflectSolBeamFront * SkyArea;
							SumReflectVis += SurfaceScreens( ScreenNum ).ReflectVisBeamFront * SkyArea;
							SumArea += SkyArea;
						}
					}

					// Reflectance of overall screen including openings and scattered transmittance
					SurfaceScreens( ScreenNum ).ReflectScreen = SurfaceScreens( ScreenNum ).ReflectCylinder * ( 1.0 - ( SurfaceScreens( ScreenNum ).BmBmTrans + SurfaceScreens( ScreenNum ).BmDifTrans ) );
					SurfaceScreens( ScreenNum ).ReflectScreenVis = SurfaceScreens( ScreenNum ).ReflectCylinderVis * ( 1.0 - ( SurfaceScreens( ScreenNum ).BmBmTransVis + SurfaceScreens( ScreenNum ).BmDifTransVis ) );

					if ( SumArea != 0 ) {
						SurfaceScreens( ScreenNum ).DifDifTrans = SumTrans / SumArea;
						SurfaceScreens( ScreenNum ).DifDifTransVis = SumTransVis / SumArea;
						SurfaceScreens( ScreenNum ).DifReflect = SumReflect / SumArea;
						SurfaceScreens( ScreenNum ).DifReflectVis = SumReflectVis / SumArea;
					}
					SurfaceScreens( ScreenNum ).DifScreenAbsorp = max( 0.0, ( 1.0 - SurfaceScreens( ScreenNum ).DifDifTrans - SurfaceScreens( ScreenNum ).DifReflect ) );

					Material( MatNum ).AbsorpThermalBack = SurfaceScreens( ScreenNum ).DifScreenAbsorp;
					Material( MatNum ).AbsorpThermalFront = SurfaceScreens( ScreenNum ).DifScreenAbsorp;
					Material( MatNum ).ReflectSolBeamFront = SurfaceScreens( ScreenNum ).DifReflect;
					Material( MatNum ).ReflectSolBeamBack = SurfaceScreens( ScreenNum ).DifReflect;

				} // (ShadingType == 'EXTERIORSCREEN')
			} //(Surface(SurfNum)%WindowShadingControlPtr /= 0)

		} // End of screen surface initialization

		// Write transmittance versus direct normal angle to csv file

		if ( PrintTransMap ) {
			ScreenTransUnitNo = GetNewUnitNumber();
			{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "unknown" ); gio::open( ScreenTransUnitNo, DataStringGlobals::outputScreenCsvFileName, flags ); if ( flags.err() ) goto Label99999; }
			//  WRITE(ScreenTransUnitNo,*)' '
			for ( ScreenNum = 1; ScreenNum <= NumSurfaceScreens; ++ScreenNum ) {
				MatNum = SurfaceScreens( ScreenNum ).MaterialNumber;
				//   Do not print transmittance map if angle increment is equal to 0
				if ( Material( MatNum ).ScreenMapResolution == 0 ) continue;
				FoundMaterial = false;
				for ( i = ScreenNum + 1; i <= NumSurfaceScreens; ++i ) {
					//     Write out transmittance data once for each Material:WindowScreen object
					if ( MatNum == SurfaceScreens( i ).MaterialNumber ) FoundMaterial = true;
				}
				if ( FoundMaterial ) continue;
				//   Store transmittance at direct normal angle
				if ( Material( MatNum ).ScreenMapResolution != 0 ) {
					ScreenTrans( ScreenNum ).Trans.allocate( 90 / Material( MatNum ).ScreenMapResolution + 1, 90 / Material( MatNum ).ScreenMapResolution + 1 );
					ScreenTrans( ScreenNum ).Scatt.allocate( 90 / Material( MatNum ).ScreenMapResolution + 1, 90 / Material( MatNum ).ScreenMapResolution + 1 );
					ScreenTrans( ScreenNum ).Trans = 0.0;
					ScreenTrans( ScreenNum ).Scatt = 0.0;
					for ( j = 90 / Material( MatNum ).ScreenMapResolution + 1; j >= 1; --j ) {
						for ( i = 90 / Material( MatNum ).ScreenMapResolution + 1; i >= 1; --i ) {
							SunAzimuth = Material( MatNum ).ScreenMapResolution * ( j - 1 ) * ( Pi / 180.0 );
							SunAltitude = Material( MatNum ).ScreenMapResolution * ( i - 1 ) * ( Pi / 180.0 );
							CalcScreenTransmittance( 0, SunAltitude, SunAzimuth, ScreenNum );
							ScreenTrans( ScreenNum ).Trans( i, j ) = SurfaceScreens( ScreenNum ).BmBmTrans;
							ScreenTrans( ScreenNum ).Scatt( i, j ) = SurfaceScreens( ScreenNum ).BmDifTrans;
						}
					}

					gio::write( ScreenTransUnitNo, fmtA ) << "MATERIAL:WINDOWSCREEN:" + Material( SurfaceScreens( ScreenNum ).MaterialNumber ).Name;
					gio::write( ScreenTransUnitNo, fmtA ) << "Tabular data for beam solar transmittance at varying \"relative\" azimuth (row) and altitude (column) angles (deg) [relative to surface normal].";
					{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( ScreenTransUnitNo, fmtA, flags ) << ",90"; }
					for ( i = 90 / Material( MatNum ).ScreenMapResolution; i >= 2; --i ) {
						{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( ScreenTransUnitNo, fmtA, flags ) << "," + RoundSigDigits( ( ( i - 1 ) * Material( MatNum ).ScreenMapResolution ) ); }
					}
					gio::write( ScreenTransUnitNo, fmtA ) << ",0";

					for ( j = 1; j <= 90 / Material( MatNum ).ScreenMapResolution + 1; ++j ) {
						{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( ScreenTransUnitNo, fmtA, flags ) << RoundSigDigits( ( ( j - 1 ) * Material( MatNum ).ScreenMapResolution ) ); }
						for ( i = 90 / Material( MatNum ).ScreenMapResolution + 1; i >= 2; --i ) {
							{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( ScreenTransUnitNo, fmtA, flags ) << "," + RoundSigDigits( ScreenTrans( ScreenNum ).Trans( i, j ), 6 ); }
						}
						gio::write( ScreenTransUnitNo, fmtA ) << "," + RoundSigDigits( ScreenTrans( ScreenNum ).Trans( i, j ), 6 );
					}
					gio::write( ScreenTransUnitNo );
					gio::write( ScreenTransUnitNo );

					gio::write( ScreenTransUnitNo, fmtA ) << "MATERIAL:WINDOWSCREEN:" + Material( SurfaceScreens( ScreenNum ).MaterialNumber ).Name;
					gio::write( ScreenTransUnitNo, fmtA ) << "Tabular data for scattered solar transmittance at varying \"relative\" azimuth (row) and altitude (column) angles (deg) [relative to surface normal].";
					for ( i = 1; i <= 90 / Material( MatNum ).ScreenMapResolution; ++i ) {
						{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( ScreenTransUnitNo, fmtA, flags ) << "," + RoundSigDigits( ( ( i - 1 ) * Material( MatNum ).ScreenMapResolution ) ); }
					}
					gio::write( ScreenTransUnitNo, fmtA ) << "," + RoundSigDigits( ( ( i - 1 ) * Material( MatNum ).ScreenMapResolution ) );

					for ( j = 1; j <= 90 / Material( MatNum ).ScreenMapResolution + 1; ++j ) {
						{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( ScreenTransUnitNo, fmtA, flags ) << RoundSigDigits( ( ( j - 1 ) * Material( MatNum ).ScreenMapResolution ) ); }
						for ( i = 1; i <= 90 / Material( MatNum ).ScreenMapResolution; ++i ) {
							{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( ScreenTransUnitNo, fmtA, flags ) << "," + RoundSigDigits( ScreenTrans( ScreenNum ).Scatt( i, j ), 6 ); }
						}
						gio::write( ScreenTransUnitNo, fmtA ) << "," + RoundSigDigits( ScreenTrans( ScreenNum ).Scatt( 90 / Material( MatNum ).ScreenMapResolution + 1, j ), 6 );
					}
					gio::write( ScreenTransUnitNo );
					gio::write( ScreenTransUnitNo );
				}
			}
Label99999: ;
			gio::close( ScreenTransUnitNo );
		}
		ScreenTrans.deallocate();
	}

	void
	BlindOpticsDiffuse(
		int const BlindNum, // Blind number
		int const ISolVis, // 1 = solar and IR calculation; 2 = visible calculation
		Array1A< Real64 > const c, // Slat properties
		Real64 const b_el, // Slat elevation (radians)
		Array1A< Real64 > p // Blind properties
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Hans Simmler
		//       DATE WRITTEN   July-Aug 1995
		//       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
		//                      Aug 2002 (FCW): make corrections so that calculations are consistent with
		//                       G(i) = Sum over j of J(j)*F(j,i). Previously, i,j was
		//                      interchanged in F, so that
		//                       G(i) = Sum over j of J(j)*F(i,j), which is wrong.
		//                      This change was made to resolve discrepancies between EnergyPlus results
		//                      and blind transmittance measurements made at Oklahoma State Univ.
		//                      Feb 2004 (FCW): modify slat edge correction calc to avoid possible divide by zero
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// From the slat properties, calculates the diffuse solar, diffuse visible and IR
		// transmission and reflection properties of a window blind.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// "Solar-Thermal Window Blind Model for DOE-2," H. Simmler, U. Fischer and
		// F. Winkelmann, Lawrence Berkeley National Laboratory, Jan. 1996.

		// USE STATEMENTS:na

		// Argument array dimensioning
		c.dim( 15 );
		p.dim( 16 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:na
		// INTERFACE BLOCK SPECIFICATIONS:na
		// DERIVED TYPE DEFINITIONS:na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 ri; // Front and back IR slat reflectance
		Real64 rib;
		Real64 phib; // Elevation of slat normal vector (radians)
		Real64 phis; // Source elevation (radians)
		Real64 delphis; // Angle increment for integration over source distribution (radians)
		Array1D< Real64 > fEdgeSource( 10 ); // Slat edge correction factor vs source elevation
		Array1D< Real64 > fEdgeA( 2 ); // Average slat edge correction factor for upper and lower quadrants
		//  seen by window blind
		Real64 gamma; // phib - phis
		int Iphis; // Source elevation counter
		int IUpDown; // =1 for source in upper quadrant, =2 for source in lower quadrant
		Real64 fEdge; // Slat edge correction factor
		Real64 fEdge1;
		Array1D< Real64 > j( 6 ); // Slat section radiosity vector
		Array1D< Real64 > G( 6 ); // Slat section irradiance vector
		Array1D< Real64 > Q( 6 ); // Slat section radiance vector
		Array2D< Real64 > F( 6, 6 ); // View factor array
		Array2D< Real64 > X( 4, 4 ); // Exchange matrix
		Array2D< Real64 > Xinv( 4, 4 ); // Inverse of exchange matrix
		int k; // Array indices
		int m;
		Array1D_int indx( 4 ); // LU decomposition indices
		Real64 BlindIRreflFront; // Blind front IR reflectance
		Real64 BlindIRreflBack; // Blind back IR reflectance

		// The slat input properties are:
		// c(1)    0. (unused)
		// c(2)    Slat width (m)
		// c(3)    Slat separation (m)
		// c(4)    0. (unused)
		// c(5)    0. (unused)
		// c(6)    0. (unused)
		//      The following are solar or visible properties
		// c(7)    trans beam-diff
		// c(8)    refl front beam-diff
		// c(9)    refl back beam-diff
		// c(10)   trans diff-diff
		// c(11)   refl front diff-diff
		// c(12)   refl back diff-diff
		//      The following are hemispherical thermal IR properties
		// c(13)   trans diff-diff
		// c(14)   emiss front diff
		// c(15)   emiss back diff

		// The calculated blind properties are:
		//      The following are solar or visible properties
		// p(1)    trans front beam-beam
		// p(2)    refl front beam-beam
		// p(3)    trans back beam-beam
		// p(4)    refl back beam-beam
		// p(5)    trans front beam-diff
		// p(6)    refl front beam-diff
		// p(7)    trans back beam-diff
		// p(8)    refl back beam-diff
		// p(9)    trans front diff-diff
		// p(10)   refl front diff-diff
		// p(11)   trans back diff-diff
		// p(12)   refl back diff-diff
		//      The following are IR properties
		// p(13)   IR trans front (same as IR trans back)
		// p(14)   IR emissivity front
		// p(15)   IR emissivity back
		// p(16)   0.0 (unused)

		//     Calculate view factors between slat sections (slat is divided longitudinally into two equal parts)

		ViewFac( c( 2 ), c( 3 ), b_el, PiOvr2, F );

		//     Set up exchange matrix X for diffuse properties

		for ( k = 3; k <= 5; k += 2 ) {
			for ( m = 3; m <= 6; ++m ) {
				X( m - 2, k - 2 ) = -c( 12 ) * F( k, m ) - c( 10 ) * F( k + 1, m );
				X( m - 2, k - 1 ) = -c( 10 ) * F( k, m ) - c( 11 ) * F( k + 1, m );
			}
		}

		for ( k = 1; k <= 4; ++k ) {
			++X( k, k );
		}

		indx = 0;
		InvertMatrix( X, Xinv, indx, 4, 4 ); //Autodesk:Note X modified by this call

		//---------Calculate diffuse short-wave properties for the front side of the blind

		//     Sources

		Q( 3 ) = c( 12 ) * F( 3, 1 ) + c( 10 ) * F( 4, 1 );
		Q( 4 ) = c( 10 ) * F( 3, 1 ) + c( 11 ) * F( 4, 1 );
		Q( 5 ) = c( 12 ) * F( 5, 1 ) + c( 10 ) * F( 6, 1 );
		Q( 6 ) = c( 10 ) * F( 5, 1 ) + c( 11 ) * F( 6, 1 );

		//     Radiosities

		j( 1 ) = 1.0;
		j( 2 ) = 0.0;
		for ( k = 3; k <= 6; ++k ) {
			j( k ) = 0.0;
			for ( m = 3; m <= 6; ++m ) {
				j( k ) += Xinv( m - 2, k - 2 ) * Q( m );
			}
		}

		//     Irradiances

		for ( k = 1; k <= 6; ++k ) {
			G( k ) = 0.0;
			for ( m = 1; m <= 6; ++m ) {
				//G(k)=G(k)+F(k,m)*J(m)
				G( k ) += j( m ) * F( k, m );
			}
		}

		//     Slat edge correction factor
		phib = b_el;
		delphis = PiOvr2 / 10.0;
		for ( IUpDown = 1; IUpDown <= 2; ++IUpDown ) {
			for ( Iphis = 1; Iphis <= 10; ++Iphis ) {
				phis = -( Iphis - 0.5 ) * delphis;
				if ( IUpDown == 2 ) phis = ( Iphis - 0.5 ) * delphis;
				fEdgeSource( Iphis ) = 0.0;
				fEdge1 = 0.0;
				gamma = phib - phis;
				if ( std::abs( std::sin( gamma ) ) > 0.01 ) {
					if ( ( phib > 0.0 && phib <= PiOvr2 && phis <= phib ) || ( phib > PiOvr2 && phib <= Pi && phis > -( Pi - phib ) ) ) fEdge1 = Blind( BlindNum ).SlatThickness * std::abs( std::sin( gamma ) ) / ( ( Blind( BlindNum ).SlatSeparation + Blind( BlindNum ).SlatThickness / std::abs( std::sin( phib ) ) ) * std::cos( phis ) );
					fEdgeSource( Iphis ) = min( 1.0, std::abs( fEdge1 ) );
				}
			}
			fEdgeA( IUpDown ) = DiffuseAverage( fEdgeSource );
		}
		fEdge = 0.5 * ( fEdgeA( 1 ) + fEdgeA( 2 ) );

		//     Front diffuse-diffuse transmittance (transmittance of slat edge assumed zero)
		p( 9 ) = G( 2 ) * ( 1.0 - fEdge );

		//     Front diffuse-diffuse reflectance (edge of slat is assumed to have same diffuse
		//     reflectance as front side of slat, c(11))
		p( 10 ) = G( 1 ) * ( 1.0 - fEdge ) + fEdge * c( 11 );

		//-----------Calculate diffuse short-wave properties for the back side of the blind

		//     Sources

		Q( 3 ) = c( 12 ) * F( 3, 2 ) + c( 10 ) * F( 4, 2 );
		Q( 4 ) = c( 10 ) * F( 3, 2 ) + c( 11 ) * F( 4, 2 );
		Q( 5 ) = c( 12 ) * F( 5, 2 ) + c( 10 ) * F( 6, 2 );
		Q( 6 ) = c( 10 ) * F( 5, 2 ) + c( 11 ) * F( 6, 2 );

		//     Radiosities

		j( 1 ) = 0.0;
		j( 2 ) = 1.0;
		for ( k = 3; k <= 6; ++k ) {
			j( k ) = 0.0;
			for ( m = 3; m <= 6; ++m ) {
				j( k ) += Xinv( m - 2, k - 2 ) * Q( m );
			}
		}

		//     Irradiances

		for ( k = 1; k <= 6; ++k ) {
			G( k ) = 0.0;
			for ( m = 1; m <= 6; ++m ) {
				//G(k)=G(k)+F(k,m)*J(m)
				G( k ) += j( m ) * F( k, m );
			}
		}

		//     Back diffuse-diffuse transmittance
		p( 11 ) = G( 1 ) * ( 1.0 - fEdge );

		//     Back hemi-hemi reflectance
		p( 12 ) = G( 2 ) * ( 1.0 - fEdge ) + fEdge * c( 11 );

		if ( ISolVis == 1 ) {

			//-----------Calculate IR properties of the blind
			//           (use same set of view factors as for diffuse short-wave properties)

			//     Front and back slat IR reflectances
			ri = 1 - c( 13 ) - c( 14 );
			rib = 1 - c( 13 ) - c( 15 );

			//     Set up exchange matrix X for diffuse properties

			for ( k = 3; k <= 5; k += 2 ) {
				for ( m = 3; m <= 6; ++m ) {
					X( m - 2, k - 2 ) = -rib * F( k, m ) - c( 13 ) * F( k + 1, m );
					X( m - 2, k - 1 ) = -c( 13 ) * F( k, m ) - ri * F( k + 1, m );
				}
			}

			for ( k = 1; k <= 4; ++k ) {
				++X( k, k );
			}

			indx = 0;
			InvertMatrix( X, Xinv, indx, 4, 4 ); //Autodesk:Note X modified by this call

			//---------Calculate diffuse IR properties for the FRONT side of the blind

			//     Sources

			Q( 3 ) = rib * F( 3, 1 ) + c( 13 ) * F( 4, 1 );
			Q( 4 ) = c( 13 ) * F( 3, 1 ) + ri * F( 4, 1 );
			Q( 5 ) = rib * F( 5, 1 ) + c( 13 ) * F( 6, 1 );
			Q( 6 ) = c( 13 ) * F( 5, 1 ) + ri * F( 6, 1 );

			//     Radiosities

			j( 1 ) = 1.0;
			j( 2 ) = 0.0;
			for ( k = 3; k <= 6; ++k ) {
				j( k ) = 0.0;
				for ( m = 3; m <= 6; ++m ) {
					j( k ) += Xinv( m - 2, k - 2 ) * Q( m );
				}
			}

			//     Irradiances
			for ( k = 1; k <= 6; ++k ) {
				G( k ) = 0.0;
				for ( m = 1; m <= 6; ++m ) {
					//G(k)=G(k)+F(k,m)*J(m)
					G( k ) += j( m ) * F( k, m );
				}
			}

			//     Front diffuse-diffuse IR transmittance (transmittance of slat edge assumed zero)
			p( 13 ) = G( 2 ) * ( 1.0 - fEdge );

			//     Front diffuse-diffuse IR reflectance (edge of slat is assumed to have same IR
			//     reflectance as front side of slat, ri)
			BlindIRreflFront = G( 1 ) * ( 1.0 - fEdge ) + fEdge * ri;

			//     Front IR emissivity
			p( 14 ) = max( 0.0001, 1.0 - p( 13 ) - BlindIRreflFront );

			//-----------Calculate diffuse IR properties for the BACK side of the blind

			//     Sources

			Q( 3 ) = rib * F( 3, 2 ) + c( 13 ) * F( 4, 2 );
			Q( 4 ) = c( 13 ) * F( 3, 2 ) + ri * F( 4, 2 );
			Q( 5 ) = rib * F( 5, 2 ) + c( 13 ) * F( 6, 2 );
			Q( 6 ) = c( 13 ) * F( 5, 2 ) + ri * F( 6, 2 );

			//     Radiosities

			j( 1 ) = 0.0;
			j( 2 ) = 1.0;
			for ( k = 3; k <= 6; ++k ) {
				j( k ) = 0.0;
				for ( m = 3; m <= 6; ++m ) {
					j( k ) += Xinv( m - 2, k - 2 ) * Q( m );
				}
			}

			//     Irradiances

			for ( k = 1; k <= 6; ++k ) {
				G( k ) = 0.0;
				for ( m = 1; m <= 6; ++m ) {
					//G(k)=G(k)+F(k,m)*J(m)
					G( k ) += j( m ) * F( k, m );
				}
			}

			//     Back diffuse-diffuse IR reflectance
			BlindIRreflBack = G( 2 ) * ( 1.0 - fEdge ) + fEdge * ri;

			//     Back IR emissivity
			p( 15 ) = max( 0.0001, 1.0 - p( 13 ) - BlindIRreflBack );

		} // End of IR properties calculation

	}

	//**********************************************************************************************

	void
	BlindOpticsBeam(
		int const BlindNum, // Blind number
		Array1A< Real64 > const c, // Slat properties (equivalent to BLD_PR)
		Real64 const b_el, // Slat elevation (radians)
		Real64 const s_el, // Solar profile angle (radians)
		Array1A< Real64 > p // Blind properties (equivalent to ST_LAY)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Hans Simmler
		//       DATE WRITTEN   July-Aug 1995
		//       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
		//                      Aug 2002 (FCW): make corrections so that calculations are consistent with
		//                       G(i) = Sum over j of J(j)*F(j,i). Previously, i,j was
		//                      interchanged in F, so that
		//                       G(i) = Sum over j of J(j)*F(i,j), which is wrong.
		//                      This change was made to resolve discrepancies between EnergyPlus results
		//                      and blind transmittance measurements made at Oklahoma State Univ.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//     Calculates the beam radiation properties of a
		//     window blind consisting of flat slats with known material properties.
		//     The calculation for the reverse direction is done with the radiation source
		//     reflected at the window plane.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// "Solar-Thermal Window Blind Model for DOE-2," H. Simmler, U. Fischer and
		// F. Winkelmann, Lawrence Berkeley National Laboratory, Jan. 1996.

		// USE STATEMENTS:

		// Using/Aliasing
		using General::BlindBeamBeamTrans; // Blind beam-to-beam transmittance function

		// Argument array dimensioning
		c.dim( 15 );
		p.dim( 16 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// The slat input properties are:
		// c(1)    0. (unused)
		// c(2)    Slat width (m)
		// c(3)    Slat separation (m)
		// c(4)    0. (unused)
		// c(5)    0. (unused)
		// c(6)    0. (unused)
		//      The following are solar or visible properties
		// c(7)    trans beam-diff
		// c(8)    refl front beam-diff
		// c(9)    refl back beam-diff
		// c(10)   trans diff-diff
		// c(11)   refl front diff-diff
		// c(12)   refl back diff-diff
		//      The following are hemispherical thermal IR properties
		// c(13)   trans diff-diff
		// c(14)   emiss front diff
		// c(15)   emiss back diff

		// The calculated blind properties are:
		//      The following are solar or visible properties
		// p(1)    trans front beam-beam
		// p(2)    refl front beam-beam
		// p(3)    trans back beam-beam
		// p(4)    refl back beam-beam
		// p(5)    trans front beam-diff
		// p(6)    refl front beam-diff
		// p(7)    trans back beam-diff
		// p(8)    refl back beam-diff
		// p(9)    trans front diff-diff
		// p(10)   refl front diff-diff
		// p(11)   trans back diff-diff
		// p(12)   refl back diff-diff
		//      The following are IR properties
		// p(13)   IR trans front (same as IR trans back)
		// p(14)   IR emissivity front
		// p(15)   IR emissivity back
		// p(16)   0.0 (unused)

		// SUBROUTINE PARAMETER DEFINITIONS:na
		// INTERFACE BLOCK SPECIFICATIONS:na
		// DERIVED TYPE DEFINITIONS:na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 phib; // Elevation angle of normal vector to front of slat (0 to pi radians)
		Real64 phis; // Elevation angle of source vector; same as "profile angle" (-pi/2 to pi/2 radians)
		Real64 gamma; // phib - phis (radians)
		Array1D< Real64 > j( 6 ); // Slat surface section radiosity vector
		Array1D< Real64 > G( 6 ); // Slat surface section irradiance vector
		Array1D< Real64 > Q( 6 ); // Slat surface section source vector
		Array2D< Real64 > F( 6, 6 ); // View factor array
		Array2D< Real64 > X( 4, 4 ); // X*J = Q
		Array2D< Real64 > Xinv( 4, 4 ); // J = Xinv*Q
		Real64 fEdge; // Slat edge correction factor
		Real64 fEdge1;
		int i; // Array indices
		int k;
		int m;
		Array1D_int indx( 4 ); // Indices for LU decomposition

		p = 0.0;

		//     Elevation of radiation source; source is assumed to be in a plane that
		//     (1) contains the slat outward normal and (2) is perpendicular to plane of the blinds.
		phis = s_el;

		//     Elevation of slat outward normal
		phib = b_el;

		//     Loop twice for front and back side properties of blind
		for ( i = 0; i <= 2; i += 2 ) {

			//       For back-side properties, reflect the source position so that it is the mirror
			//       image of the original source position, where the "mirror" is in the plane of the
			//       blinds. This is equivalent to keeping the original source position but rotating
			//       the slats so that the original slat angle (e.g., 45 deg) becomes 180 - original slat
			//       angle (135 deg).

			if ( i == 2 ) {
				phib = Pi - phib;
			}

			//       Correction factor that accounts for finite thickness of slats. It is used to modify the
			//       blind transmittance and reflectance to account for reflection and absorption by the
			//       edge of the slat. fEdge is ratio of area subtended by edge of slat
			//       to area between tops of adjacent slats.

			fEdge = 0.0;
			fEdge1 = 0.0;
			gamma = phib - phis;
			if ( std::abs( std::sin( gamma ) ) > 0.01 ) {
				if ( ( phib > 0.0 && phib <= PiOvr2 && phis <= phib ) || ( phib > PiOvr2 && phib <= Pi && phis > -( Pi - phib ) ) ) fEdge1 = Blind( BlindNum ).SlatThickness * std::abs( std::sin( gamma ) ) / ( ( Blind( BlindNum ).SlatSeparation + Blind( BlindNum ).SlatThickness / std::abs( std::sin( phib ) ) ) * std::cos( phis ) );
				fEdge = min( 1.0, std::abs( fEdge1 ) );
			}

			//       Direct-to-direct transmittance (portion of beam that passes between slats without
			//       without touching them

			p( 1 + i ) = BlindBeamBeamTrans( phis, phib, Blind( BlindNum ).SlatWidth, Blind( BlindNum ).SlatSeparation, Blind( BlindNum ).SlatThickness );
			//       Direct-to-direct reflectance; this is zero for now since all reflection is assumed to be diffuse.
			p( 2 + i ) = 0.0;

			//       View factors between slat sections for calculating direct-to-diffuse transmittance and reflectance
			ViewFac( c( 2 ), c( 3 ), phib, phis, F );

			//       Set up exchange matrix X for calculating direct-to-diffuse properties

			for ( k = 3; k <= 5; k += 2 ) {
				for ( m = 3; m <= 6; ++m ) {
					X( m - 2, k - 2 ) = -c( 12 ) * F( k, m ) - c( 10 ) * F( k + 1, m );
					X( m - 2, k - 1 ) = -c( 10 ) * F( k, m ) - c( 11 ) * F( k + 1, m );
				}
			}

			for ( k = 1; k <= 4; ++k ) {
				++X( k, k );
			}

			indx = 0;
			// In the following, note that InvertMatrix changes X
			InvertMatrix( X, Xinv, indx, 4, 4 );

			//       Set up sources for direct-diffuse slat properties
			if ( std::abs( phis - phib ) <= PiOvr2 ) { // Beam hits front of slat
				Q( 3 ) = c( 4 ) + c( 7 ); // beam-beam trans of slat + beam-diff trans of slat
				Q( 4 ) = c( 5 ) + c( 8 ); // front beam-beam refl of slat + front beam-diff refl of slat
			} else { // Beam hits back of slat
				Q( 3 ) = c( 6 ) + c( 9 ); // back beam-beam refl of slat  + back beam-diff refl of slat
				Q( 4 ) = c( 4 ) + c( 7 ); // beam-beam trans of slat + beam-diff trans of slat
			}

			//       Correct for fraction of beam that is not directly transmitted; 1 - this fraction is
			//       the fraction of the incoming beam that is incident on the front or back surfaces of the slats.
			Q( 3 ) *= ( 1.0 - p( 1 + i ) );
			Q( 4 ) *= ( 1.0 - p( 1 + i ) );

			//       Radiosities (radiance of slat sections)
			j( 1 ) = 0.0;
			j( 2 ) = 0.0;
			for ( k = 3; k <= 6; ++k ) {
				j( k ) = 0.0;
				for ( m = 3; m <= 4; ++m ) {
					j( k ) += Xinv( m - 2, k - 2 ) * Q( m );
				}
			}

			//       Irradiance on slat sections
			for ( k = 1; k <= 6; ++k ) {
				G( k ) = 0.0;
				for ( m = 3; m <= 6; ++m ) {
					G( k ) += j( m ) * F( k, m );
				}
			}

			//       Direct-to-diffuse transmittance
			p( 5 + i ) = G( 2 ) * ( 1.0 - fEdge );

			//       Direct-to-diffuse reflectance (assuming the edge reflectance is the same as the
			//       reflectance of the front side of the slat, C(8))
			p( 6 + i ) = G( 1 ) * ( 1.0 - fEdge ) + fEdge * c( 8 );

		} // End of loop over front and back side properties of blind

	}

	//********************************************************************************************

	void
	ViewFac(
		Real64 const s, // Slat width (m)
		Real64 const h, // Distance between faces of adjacent slats (m)
		Real64 const phib, // Elevation angle of normal to slat (radians)
		Real64 const phis, // Profile angle of radiation source (radians)
		Array2A< Real64 > F // View factor array
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Hans Simmler
		//       DATE WRITTEN   July-Aug 1995
		//       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
		//                      Apr 2002 (FCW): prevent sqrt of small negative argument
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//     Calculates the view factors between sections of adjacent slats,
		//     where each slat is divided longitudinally into two equal sections whose
		//     dimensions depend on source profile angle and slat geometry. The view
		//     factors are used in BlindOpticsBeam and BlindOpticsDiffuse to determine blind
		//     transmittance and reflectance for short-wave and long-wave radiation.

		// METHODOLOGY EMPLOYED:
		//     Uses expressions for view factor between flat strips with a common edge
		//     and flat strips displaced from one another. See engineering documentation.

		// REFERENCES:
		// "Solar-Thermal Window Blind Model for DOE-2," H. Simmler, U. Fischer and
		// F. Winkelmann, Lawrence Berkeley National Laboratory, Jan. 1996.

		// USE STATEMENTS:na

		// Argument array dimensioning
		F.dim( 6, 6 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D< Real64 > L( 6 ); // Length of slat sections: L1 = L2 = h; L3, L5 = length
		Real64 L3;
		Real64 L5;
		//  of upper slat sections; L4, L6 = length of lower slat
		//  slat sections (m)
		Real64 d1; // Slat geometry variables (m)
		Real64 d2;
		Real64 d3;
		Real64 d4;
		Real64 d5;
		Real64 d6;
		Real64 h2; // h**2
		Real64 ht; // 2*h
		Real64 w; // Slat geometry variable (m)
		Real64 a; // Intermediate variable (m)
		Real64 co; // Cosine of source profile angle
		int i; // View factor array indices
		int j;

		h2 = pow_2( h );
		ht = 2.0 * h;
		co = std::cos( phis );
		if ( std::abs( co ) < 0.001 ) co = 0.0;
		w = ht;
		if ( co != 0.0 ) w = s * std::cos( phib - phis ) / co;
		L3 = s * h / std::abs( w );
		if ( L3 > s ) L3 = s;
		L5 = s - L3;
		a = ht * std::cos( phib );
		// MAX(0.,...) in the following prevents small negative argument for sqrt
		d1 = std::sqrt( max( 0.0, s * s + h2 + a * s ) );
		d2 = std::sqrt( max( 0.0, s * s + h2 - a * s ) );
		d3 = std::sqrt( max( 0.0, L3 * L3 + h2 + a * L3 ) );
		d4 = std::sqrt( max( 0.0, L3 * L3 + h2 - a * L3 ) );
		d5 = std::sqrt( max( 0.0, L5 * L5 + h2 - a * L5 ) );
		d6 = std::sqrt( max( 0.0, L5 * L5 + h2 + a * L5 ) );
		for ( i = 1; i <= 6; ++i ) {
			F( i, i ) = 0.0;
		}
		F( 1, 1 ) = 0.0;
		F( 2, 1 ) = ( d1 + d2 - 2.0 * s ) / ht;
		F( 3, 1 ) = ( h + L3 - d3 ) / ht;
		F( 4, 1 ) = ( h + L3 - d4 ) / ht;
		F( 5, 1 ) = ( L5 + d3 - d1 ) / ht;
		F( 6, 1 ) = ( L5 + d4 - d2 ) / ht;
		F( 3, 2 ) = ( L3 + d5 - d2 ) / ht;
		F( 4, 2 ) = ( L3 + d6 - d1 ) / ht;
		F( 5, 2 ) = ( h + L5 - d5 ) / ht;
		F( 6, 2 ) = ( h + L5 - d6 ) / ht;
		F( 4, 3 ) = ( d3 + d4 - ht ) / ( 2.0 * L3 );
		F( 5, 3 ) = 0.0;
		F( 6, 3 ) = ( d2 + h - d4 - d5 ) / ( 2.0 * L3 );
		F( 5, 4 ) = ( d1 + h - d3 - d6 ) / ( 2.0 * L3 );
		F( 6, 4 ) = 0.0;
		F( 6, 5 ) = 0.0;
		if ( L5 > 0.0 ) F( 6, 5 ) = ( d5 + d6 - ht ) / ( 2.0 * L5 );
		L( 1 ) = h;
		L( 2 ) = h;
		L( 3 ) = L3;
		L( 4 ) = L3;
		L( 5 ) = L5;
		L( 6 ) = L5;
		for ( i = 2; i <= 6; ++i ) {
			for ( j = 1; j <= i - 1; ++j ) {
				F( j, i ) = 0.0;
				if ( L( i ) > 0.0 ) F( j, i ) = F( i, j ) * L( j ) / L( i );
			}
		}
	}

	//*****************************************************************************************

	void
	InvertMatrix(
		Array2A< Real64 > a, // Matrix to be inverted
		Array2A< Real64 > y, // Inverse of matrix a
		Array1A_int indx, // Index vector for LU decomposition
		int const np, // Dimension of matrix
		int const n
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Hans Simmler
		//       DATE WRITTEN   July-Aug 1995
		//       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//     Inverts a matrix.

		// METHODOLOGY EMPLOYED:
		//     Uses LU decomposition.

		// REFERENCES:na

		// USE STATEMENTS:na

		// Argument array dimensioning
		a.dim( np, np );
		y.dim( np, np );
		indx.dim( np );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DEFINITIONS:
		int i; // Array indices
		int j;
		int d;

		y = 0.0;
		for ( i = 1; i <= n; ++i ) {
			y( i, i ) = 1.0;
		}
		indx = 0;

		LUDCMP( a, n, np, indx, d );

		for ( j = 1; j <= n; ++j ) {
			LUBKSB( a, n, np, indx, y( j, 1 ) );
		}

	}

	//*****************************************************************************************

	void
	LUDCMP(
		Array2A< Real64 > A, // matrix
		int const N,
		int const NP,
		Array1A_int INDX,
		int & D
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann?
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine performs a LU decompostion of given matrix.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		A.dim( NP, NP );
		INDX.dim( N );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D< Real64 > VV( 100 );
		Real64 sum;
		Real64 aamax;
		Real64 dum;
		int i;
		int j;
		int K;
		int imax;

		D = 1;
		for ( i = 1; i <= N; ++i ) {
			aamax = 0.0;
			for ( j = 1; j <= N; ++j ) {
				if ( std::abs( A( j, i ) ) > aamax ) aamax = std::abs( A( j, i ) );
			}

			if ( aamax == 0.0 ) {
				ShowFatalError( "Singular matrix in LUDCMP, window calculations" );
			}
			VV( i ) = 1.0 / aamax; // Was commented out prior to 10/5/01, which caused overflows
			// in this routine in rare cases

		}

		for ( j = 1; j <= N; ++j ) {
			if ( j > 1 ) {
				for ( i = 1; i <= j - 1; ++i ) {
					sum = A( j, i );
					if ( i > 1 ) {
						for ( K = 1; K <= i - 1; ++K ) {
							sum -= A( K, i ) * A( j, K );
						}

						A( j, i ) = sum;
					}
				}
			}
			aamax = 0.0;
			for ( i = j; i <= N; ++i ) {
				sum = A( j, i );
				if ( j > 1 ) {
					for ( K = 1; K <= j - 1; ++K ) {
						sum -= A( K, i ) * A( j, K );
					}
					A( j, i ) = sum;
				}

				dum = VV( i ) * std::abs( sum );
				if ( dum >= aamax ) {
					imax = i;
					aamax = dum;
				}
			}

			if ( j != imax ) {
				for ( K = 1; K <= N; ++K ) {
					dum = A( K, imax );
					A( K, imax ) = A( K, j );
					A( K, j ) = dum;
				}

				D = -D;
				VV( imax ) = VV( j );
			}

			INDX( j ) = imax;
			if ( j != N ) {
				if ( A( j, j ) == 0.0 ) A( j, j ) = rTinyValue;

				dum = 1.0 / A( j, j );
				for ( i = j + 1; i <= N; ++i ) {
					A( j, i ) *= dum;
				}

			}
		}

		if ( A( N, N ) == 0.0 ) A( N, N ) = rTinyValue;
	}

	//*****************************************************************************************

	void
	LUBKSB(
		Array2A< Real64 > A,
		int const N,
		int const NP,
		Array1A_int INDX,
		Array1A< Real64 > B
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine performs back substitution of a LU matrix.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		A.dim( NP, NP );
		INDX.dim( N );
		B.dim( N );

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
		// na

		int j;
		int i;
		int ii;
		int LL;
		Real64 sum;

		ii = 0;

		for ( i = 1; i <= N; ++i ) {
			LL = INDX( i );
			sum = B( LL );
			B( LL ) = B( i );
			if ( ii != 0 ) {
				for ( j = ii; j <= i - 1; ++j ) {
					sum -= A( j, i ) * B( j );
				}
			} else if ( sum != 0.0 ) {
				ii = i;
			}

			B( i ) = sum;
		}

		for ( i = N; i >= 1; --i ) {
			sum = B( i );
			if ( i < N ) {
				for ( j = i + 1; j <= N; ++j ) {
					sum -= A( j, i ) * B( j );
				}
			}

			B( i ) = sum / A( i, i );
		}
	}

	// added for custom solar or visible spectrum

	void
	CheckAndReadCustomSprectrumData()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         T. Hong
		//       DATE WRITTEN   August 2013
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Check, read, and assign the custom solar or visible spectrum to:
		//  solar: nume, wle(nume), e(nume). nume = 107
		//  visible: numt3, wlt3(numt3), y30(numt3). numt3 = 81
		// Three related IDD objects:
		//  EnergyManagementSystem:ConstructionIndexVariable
		//  Site:SolarAndVisibleSpectrum, Site:SpectrumData

		// METHODOLOGY EMPLOYED:
		// Overwriting the default values

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using namespace InputProcessor;
		//USE DataGlobals ,    ONLY: AnyEnergyManagementSystemInModel

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
		static bool ErrorsFound( false ); // If errors detected in input
		int NumAlphas; // Number of Alphas for each GetobjectItem call
		int NumNumbers; // Number of Numbers for each GetobjectItem call
		int NumArgs;
		int IOStatus;
		Array1D_string cAlphaArgs; // Alpha input items for object
		Array1D< Real64 > rNumericArgs; // Numeric input items for object

		static bool RunMeOnceFlag( false ); // This subroutine only needs to be run once
		std::string cCurrentModuleObject;
		std::string cSolarSpectrum;
		std::string cVisibleSpectrum;
		static int iSolarSpectrum( 0 );
		static int iVisibleSpectrum( 0 );
		static int NumSiteSpectrum( 0 );
		int Loop;
		int iTmp;

		if ( RunMeOnceFlag ) return;

		// Step 1 - check whether there is custom solar or visible spectrum
		cCurrentModuleObject = "Site:SolarAndVisibleSpectrum";
		NumSiteSpectrum = GetNumObjectsFound( cCurrentModuleObject );

		// no custom spectrum data, done!
		if ( NumSiteSpectrum == 0 ) {
			RunMeOnceFlag = true;
			return;
		}

		// read custom spectrum data from Site:SolarAndVisibleSpectrum
		if ( NumSiteSpectrum > 1 ) { // throw error
			ShowSevereError( "Only one " + cCurrentModuleObject + " object is allowed" );
			ErrorsFound = true;
		}

		GetObjectDefMaxArgs( cCurrentModuleObject, NumArgs, NumAlphas, NumNumbers );
		cAlphaArgs.allocate( NumAlphas );
		rNumericArgs.dimension( NumNumbers, 0.0 );

		if ( NumSiteSpectrum == 1 ) {
			GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus );

			// use default spectrum data, done!
			if ( SameString( cAlphaArgs( 2 ), "Default" ) ) {
				RunMeOnceFlag = true;
				return;
			}

			// now read custom solar and visible spectrum data
			cSolarSpectrum = cAlphaArgs( 3 );
			cVisibleSpectrum = cAlphaArgs( 4 );

			cCurrentModuleObject = "Site:SpectrumData";
			NumSiteSpectrum = GetNumObjectsFound( cCurrentModuleObject );
			if ( NumSiteSpectrum == 0 ) { // throw error
				ShowSevereError( "No " + cCurrentModuleObject + " object is found" );
				ErrorsFound = true;
			}

			cAlphaArgs.deallocate();
			rNumericArgs.deallocate();

			GetObjectDefMaxArgs( cCurrentModuleObject, NumArgs, NumAlphas, NumNumbers );
			cAlphaArgs.allocate( NumAlphas );
			rNumericArgs.dimension( NumNumbers, 0.0 );

			iSolarSpectrum = 0;
			iVisibleSpectrum = 0;
			for ( Loop = 1; Loop <= NumSiteSpectrum; ++Loop ) {
				// Step 2 - read user-defined spectrum data
				GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus );
				if ( SameString( cAlphaArgs( 1 ), cSolarSpectrum ) ) {
					iSolarSpectrum = Loop;
					// overwrite the default solar spectrum
					if ( NumNumbers > 2 * nume ) {
						ShowSevereError( "Solar spectrum data pair is more than 107 - " + cCurrentModuleObject + " - " + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					} else {
						// Step 3 - overwrite default solar spectrum data
						for ( iTmp = 1; iTmp <= nume; ++iTmp ) {
							if ( iTmp <= NumNumbers / 2 ) {
								wle( iTmp ) = rNumericArgs( 2 * iTmp - 1 );
								e( iTmp ) = rNumericArgs( 2 * iTmp );
							} else {
								wle( iTmp ) = 0.0;
								e( iTmp ) = 0.0;
							}
						}
					}
				}
				if ( SameString( cAlphaArgs( 1 ), cVisibleSpectrum ) ) {
					iVisibleSpectrum = Loop;
					// overwrite the default solar spectrum
					if ( NumNumbers > 2 * numt3 ) {
						ShowSevereError( "Visible spectrum data pair is more than 81 - " + cCurrentModuleObject + " - " + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					} else {
						// Step 3 - overwrite default visible spectrum data
						for ( iTmp = 1; iTmp <= numt3; ++iTmp ) {
							if ( iTmp <= NumNumbers / 2 ) {
								wlt3( iTmp ) = rNumericArgs( 2 * iTmp - 1 );
								y30( iTmp ) = rNumericArgs( 2 * iTmp );
							} else {
								wlt3( iTmp ) = 0.0;
								y30( iTmp ) = 0.0;
							}
						}
					}
				}
				if ( ( iSolarSpectrum > 0 ) && ( iVisibleSpectrum > 0 ) ) break;
			}
		}

		cAlphaArgs.deallocate();
		rNumericArgs.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in processing input for user-defined solar/visible spectrum" );
		}

		RunMeOnceFlag = true;

	}

	//*****************************************************************************************

} // WindowManager

} // EnergyPlus
