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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <TARCOGMain.hh>
#include <TARCOGArgs.hh>
#include <TARCOGDeflection.hh>
#include <TARCOGGassesParams.hh>
#include <TARCOGParams.hh>
#include <TARCOGOutput.hh>
#include <TarcogShading.hh>
#include <ThermalEN673Calc.hh>
#include <ThermalISO15099Calc.hh>

namespace EnergyPlus {

namespace TARCOGMain {
	// TARCOG: Thermal Analysis Routine for Center of Glazing

	// MODULE INFORMATION:
	//       AUTHOR         D. Charlie Curcija
	//       DATE WRITTEN   July 2000
	//       MODIFIED       na
	//       RE-ENGINEERED  (see information bellow)
	//  Revision: 7.0.13  (March/27/2012), Simon Vidanovic
	//   - feature: New set of equaitons is set instead of hhat coefficents and new approach to solution which improves
	//               speed and stability.  Note that this solution does not include laminates
	//  Revision: 7.0.12  (March/06/2012), Simon Vidanovic
	//   - feature: Additional state for isky introduced.  Tarcog now can accept IR radiance from external source.
	//  Revision: 7.0.11  (January/04/2012), Simon Vidanovic
	//   - imrovements/bug fixes: Several items merged from Windows tarcog version into this one:
	//      - bug fix: Correct foramtting for VacuumMaxGapThickness when program writes input file
	//      - improvement: Gamma coefficient is now written in scientific notation (needed for correct output file generation)
	//      - imporvement: Gap data are now saved with higer precision to wincog input file (test purposes, debug mode only)
	//      - bug fix: Gap temperatures are recalculated within iterations (for thermally driven and forced ventilation)
	//  Revision: 7.0.10  (December/15/2011), Simon Vidanovic
	//   - imrovement: Flag for performing SHGC calculations
	//  Revision: 7.0.09  (November/15/2011), Simon Vidanovic
	//   - imrovement: Added error message tolerance (This is necessary to handle error messages in correct way)
	//  Revision: 7.0.08  (November/15/2011), Simon Vidanovic
	//   - bug fix: Fixed program crashing when warrning message 1007 occured (output could not fit in string)
	//   - feature: relaxation parameter changed
	//  Revision: 7.0.07  (November/08/2011), Simon Vidanovic
	//   - feature: Error message (as string) is now return from tarcog
	//  Revision: 7.0.06  (November/07/2011), Simon Vidanovic
	//   - bug fix: Error report now actually use passed VacuumMaxGapThickness value
	//  Revision: 7.0.05  (November/07/2011), Simon Vidanovic
	//   - bug fix: Troom and Tamb are not passed out of hhat routine after recalculation is performed.
	//              This will cause differences in calculation of U-factor
	//   - feature: VacuumMaxGapThickness is added to list of input paramters
	//  Revision: 7.0.04  (November/03/2011), Simon Vidanovic
	//   - bug fix: one of debug files did not update properly
	//  Revision: 7.0.03  (November/01/2011), Simon Vidanovic
	//   - tarcog will now exit if error code is in range from 2000 to 3000
	//   - tarcog now accepts file name which is template for debug output files creation
	//   - temperature correction added in case temperatures on the layers are equal
	//     which in case of energy calculation will give division with zero
	//   - iteration results now can be saved in the file (just for debugging purposes)
	//  Revision: 7.0.02  (October/10/2011), Simon Vidanovic
	//   - Deflection calculations implemented.
	//  Revision: 7.0.01  (September/23/2011), Simon Vidanovic
	//   - Support pillars implemented.
	//  Revision: 7.0.00  (August/23/2011), Simon Vidanovic
	//   - Added comments to input arguments; Fixed bug in Nu calculation for angle between 60 and 90 degrees.
	//  Revision: 6.0.36  (June/22/2010)
	//   - Converted to F95; refactoring.
	//  Revision: 6.0.35  (June/4/2010)
	//   - Fixed a few potential bugs
	//  Revision: 6.0.34  (May/26/2009)
	//   - Updated creation of W6 debug file (added ClosedBlind flag)
	//  Revision: 6.0.33  (March/03/2008)
	//   - Fixed a bug in arguments checking.
	//   - Added creation of WINCOG input file in debug mode.
	//  Revision: 6.0.32  (February/29/2008)
	//   - Applied CSM thermal model to Woven Shades layers.
	//   - Introduced a new error message (#39)
	//  Revision: 6.0.31  (February/15/2008)
	//   - EN673 Design and Declared standards added/fixed (it used to be EN673 and ISO 10292).
	//  Revision: 6.0.30  (May/05/2007)
	//   - Scalar model (CSM) fixed.
	//  Revision: 6.0.29  (September/14/2006)
	//   - Fixed a bug in CSM calculation that affected some cases.
	//  Revision: 6.0.28  (September/05/2006)
	//   - Woven Shade layer type introduced. These layers will be treated
	//    the same way as Venetian Blind layers.
	//  Revision: 6.0.27  (August/24/2006)
	//   - Implemented new thermal model (thermal model 2 - Convection Scalar Model)
	//   - Added new input argument - SDScalar:
	//      0.0  - No SD (in terms of convection)
	//      1.0  - Closed SD (SD treated as a 'regular' specular layer)
	//      between 0 and 1 - cobination of No SD and Closed SD cases
	//   - A bug in unshaded run (remapping gas properties in removal of indoor SD layer) was fixed
	//   - TARCOG error codes have been updated (error codes 30 and 37 added)
	//  Revision: 6.0.26  (May/31/2006)
	//   - Hard-coded Xenon gas properties for EN673 were updated.
	//   - hrin equation in EN673 routine was updated.
	//  Revision: 6.0.25  (March/29/2006)
	//   - Bug fixes in EN673/ISO10292 procedure:
	//    .values of gas properties for EN673 and ISO10292 procedure can now be passed to TARCOG
	//     via A coefficients in gvis, gcon and gcp matrices and wght array
	//    .gas mixture buid-up limited to number of gasses used in the mix
	//    .dT array is now updated correctly after each iteration
	//    .hrin formula fixed
	//   - Bug fix (checking of slat tilt angle - negative values are now allowed)
	//  Revision: 6.0.24  (November/25/2005)
	//   - Code responsible for ETR calculation (in U factor calculation)
	//    has been redesigned and cleared of all bugs.
	//  Revision: 6.0.23  (November/24/2005)
	//   - Bug fix (wrong U factor results for IGUs with one glass + indoor SD)
	//   - Bug fix (wrong results for Hc modification ratios)
	//    when fixed H or fixed Hc model is used.
	//   - Allowed DiffuseShade as layer type (does not affect calculation algorithm).
	//   - hflux value has been updated - includes contribution from freely ventilated air
	//    around indoor SD layer (this affects SHGC value)
	//   - Debug file has been updated:
	//      . qr is shown in each gap,
	//      . hcgas values have been updated to include contribution from freely ventilated air
	//        around SD layers (this affects output argument as well),
	//      . hrgas values have been introduced in debug file.
	//  Revision: 6.0.22  (November/04/2005)
	//   - Added an internal "unshaded" calculation run for glazing systems with
	//     outdoor and/or indoor SD layer(s). This run is needed for proper calculation
	//     of Hc modification ratios.
	//   - Added two new output arguments needed for connection with T6:
	//      HcUnshadedOut,
	//      HcUnshadeIn.
	//   - Changed the way ShadeHcRatioOut and ShadeHcRatioIn are caculated.
	//  Revision: 6.0.21  (October/28/2005)
	//   - Fixed another serious bug in new routine for calculation of U factor.
	//  Revision: 6.0.20  (October/20/2005)
	//   - Fixed a bug in new calculation of U factor.
	//   - Fixed a bug in formulas for calculation of SD thickness.
	//   - Forced ventilation calc has been disabled (since v6.0.19)
	//  Revision: 6.0.19  (October/19/2005)
	//   - New input arguments added:
	//      SlatThick, SlatWidth, SlatAngle, SlatCond, SlatSpacing, SlatCurve, ThermalMod.
	//   - Argument gltype renamed to LayerType.
	//   - Thermal_model_1 implemented
	//   - U factor calculation has been updated.
	//   - Description of arguments has been updated.
	//   - Forced ventilation calc has been disabled.
	//  Revision: 6.0.18  (September/16/2005)
	//   - Changed Tvent for outdoor and indoor SD layers to Tout and Tin, respectivelly.
	//   - Keff is now calculated for each gap.
	//  Revision: 6.0.17  (September/08/2005)
	//   - Fixed a bug involving Al, Ar & Ah values (patch for a case of Al+Ar+Ah = 0).
	//  Revision: 6.0.16  (September/07/2005)
	//   - Added new output arguments needed for connection with T6:
	//      ShadeEmisRatioOut,
	//      ShadeEmisRatioIn,
	//      ShadeHcRatioOut,
	//      ShadeHcRatioIn,
	//      Keff,
	//      ShadeGapKeffConv
	//  Revision: 6.0.15  (August/30/2005)
	//   - Removed CHDIR call; used filepath//filename instead.
	//  Revision: 6.0.14  (August/26/2005)
	//   - New arguments introduced:
	//      Debug_dir  - character array: target directory for debug output
	//      Window_ID  - integer: window ID (from W6)
	//      IGU_ID  - integer: IGU ID (from W6)
	//  Revision: 6.0.13  (August/19/2005)
	//   - Bug fix #9 - allows calculations for Atop=Abot=0, by setting both values to 1e-6
	//  Revision: 6.0.12  (August/12/2005)
	//   - Minor change in Picard coefficients for vented gaps - MaxIter cannot
	//     be less than 800. This solves issue with theta buffer in Therm1d.
	//   - Implemented creation of TARCOG debug file w/ i/o arguments/results (Tarcog.dbg)
	//     (fetaure #8); debug file will be created depending on Debug_mode switch:
	//      Debug_mode = 0 : debug file will not be created
	//      Debug_mode = 1 : debug file will be appended
	//      Debug_mode = 2 : new debug file will be created
	//   - Bug fix #7- qin result corrected in SHGC run.
	//  Revision: 6.0.11  (July/15/2005)
	//   - Bug fix #4: an issue in original formulas that causes Therm1d
	//     to fail to converge for in certain cases. Changes were made in Therm1d
	//     that allow regular calculation outcome in these cases.
	//  Revision: 6.0.10  (June/3/2005)
	//   - Bug fix #00005: added IGU Height in term A in ShadingIn and ShadingEdge
	//     procedures. This term is used in calculations of free vent air velocity
	//  Revision: 6.0.09  (May/16/2005)
	//   - Bug fix #00001: for fixed combined coef. BC -> Trmout = Tout, Trmin = Tind
	//  Revision: 6.0.08  (May/12/2005)
	//   - Debug flag introduced in Tarcog, Therm1d and dtemp procedures:
	//     debug info is stored in two files: tarcog.dbg and temps.out
	//  Revision: 6.0.07  (April/12/2005)
	//   - Fixed a bug (#00002) in calculation of
	//     Picard method coefficients, in Therm1d procedure:
	//     fixes SD ventilation velocity related calcs.
	//  Revision: 6.0.06  (October/22/2004)
	//   - Fixed a bug in velocity calculations
	//  Revision: 6.0.05  (October/01/2004)
	//   - Changed name from w5cog to TARCOG
	//   - Changed version numbering
	//---------------------------------------------
	//  Revision: 5.3.105  (September/07/04)
	//   - Changed argument 'standard' from string to integer
	//  Revision: 5.3.02.03  (January/16/04)
	//   - Slice temperatures calculated
	//   - Write out Nusselt and Rayleigh (in debug mode)
	//   - sol converted to asol in input format
	//  Revision: 5.3.02.02  (11/14/03)
	//   - Implemented CEN Standard
	//  Revision: 5.3.02.01  (11/06/03)
	//   - implemented Laminate procedure
	//  Revision: 5.3.01  (09/22/03)
	//   - fixed bug in Tgap initial calculation
	//   - reduce dimension of Tgap
	//  Revision: 5.3.00  (08/25/03)
	//   - make code more readable and rename version
	//  Revision: 6.0.07  (08/10/03)
	//   - implemented arrays for ventilation temperature and speed
	//   - repaired equation 121 in ISO (instead of cos(tilt) now is ABS(cos(tilt)))
	//   - implemented input field for forced ventilation velocity (for all cases)
	//   - fixed bug in shading edge: coefficients B1, C1 and D1 repaired to calcluate value for gap temperature (not enviroment)
	//   - fixed bug in routine shading edge: characteristic Height is calculated for gap (not for enviroment)
	//   - fixed bug in proccesing error messages (this was produced slightly wrong in some cases results)
	//  Revision: 6.0.06 rv  (04/22/03)
	//   - write out heat flux calculation for shading device panes
	//   - qv smoothing: make vented heat flux same in adjacent gaps (gaps are divided by shading pane)
	//   - fixed bug in routine "shadingin": make that vented heat flow direction is calculated correct, according
	//     to temeratures in adjecent gaps
	//  Revision: 6.0.05  (04/14/03)
	//   - implemented "Picard" method in iteration loop
	//   - number of iterations is increased to 1000
	//   - alpha coefficient is set to 0.01 (Picard method)
	//  Revision: 6.0.04  (03/03/03)
	//   - implemented forced ventilation for indoor and outdoor shading devices
	//   - maximum number of iterations is increased to 100
	//  Revision: 6.0.03 (02/25/03)
	//   - fixed bug in error message for "nlayer<1"
	//   - error messages update
	//   - repaired U-value calculation and Tgap1, Tgap2 calculation for shading inside
	//   - shading devices implemented
	//   - error messages and warnings updated and tolerance is decreased to 1e-5
	//   - write out heat fluxes
	//   - fixed bug in hrin and hrout calculation
	//   - fixed bug in nusselt number for gap calculation for angle > 90 degrees
	//    and new error messages are added
	//pr  Revision: 5.13 (01/16/02)
	//pr
	//pr   - pi value updated
	//pr
	//  Revision: 5.12 (10/27/01)
	//   - deleted extra lines in hatter (just cleanup)
	//  Revision: 5.11 (10/23/01)
	//   - fixed bug in reporting SHGC
	//pr  Revision: 5.10 (10/15/01) [Not included here - under testing]
	//pr
	//pr  - updated gas properties for Xenon (for EN673 standard)
	//pr  Revision: 5.09 (10/12/01) [Not included here - under testing]
	//pr
	//pr  - implemented EN673 standard
	//  Revision: 5.08 (10/11/01)
	//   - corrected reporting for solar conditions
	//   - corrected array declaration for nmix in filmg
	//  Revision: 5.07 (9/21/01) [not included here - under testing]
	//   - implemented Laminate procedure
	//pr  Revision: 5.06(09/18/00) [not included here - under testing]
	//pr
	//pr  - implemented CEN algorithms
	// Revised: June, 2001
	//   - further implementation of ISO 15099
	//     * new def. of shgc
	//     * revised gas constants
	//     * implemented mean radiant temperature on indoor side
	//     *
	//   - cleaned bugs
	//     * fixed film coefficient didn't work correctly
	//   - removed condensation calculations (part of main w5 code now)
	//   - implemented environmental temperatures
	// Revised: January, 2001
	//   - revised input format
	//   - streamlined code
	//   - implemented dll's
	// Revised: July 2000 (Major revision of the new code)
	//   - checked for accuracy, cleaned bunch of bugs
	//   - implemented gas mixtures
	// Initial update of the old "therm" (WINDOW 4.1) code: Around 1998/99

	// PURPOSE OF THIS MODULE:
	//   Module For Calculation of Thermal Performance Indices For Center
	//     of Glass According to ISO 15099/ASHRAE SPC142, ISO10292, and EN673

	// METHODOLOGY EMPLOYED:
	//  Standard ISO 15099/ASHRAE SPC142, ISO10292 and EN673

	// REFERENCES:
	// ISO 15099/ASHRAE SPC142, ISO10292, EN673, Tarcog technical documentation

	// OTHER NOTES:
	// na

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace TARCOGOutput;
	using namespace TARCOGGassesParams;
	using namespace TARCOGParams;
	using namespace TarcogShading;
	using namespace TARCOGArgs;
	using namespace ThermalISO15099Calc;
	using namespace ThermalEN673Calc;
	using namespace TARCOGDeflection;

	// Functions

	void
	TARCOG90(
		int const nlayer, // Number of layers (glass + SD)
		int const iwd, // Wind direction:
		Real64 & tout, // Outdoor temperature [K]
		Real64 & tind, // Indoor temperature [K]
		Real64 & trmin, // Indoor mean radiant temperature [K]
		Real64 const wso, // Outdoor wind speed [m/s]
		Real64 const wsi, // Inside forced air speed [m/s]
		Real64 const dir, // Direct solar radiation [W/m2]
		Real64 const outir, // IR radiance of window's exterior surround [W/m2]
		int const isky, // Flag for sky temperature(Tsky) and sky emittance(esky)
		Real64 const tsky, // Night sky temperature [K]
		Real64 & esky, // Effective night sky emittance
		Real64 const fclr, // Fraction of sky that is clear
		Real64 const VacuumPressure, // maximal pressure for gas to be considered as vacuum
		Real64 & VacuumMaxGapThickness, // maximum allowed thickness without producing warning message
		int const CalcDeflection, // Deflection calculation flag:
		Real64 const Pa, // Atmospheric (outside/inside) pressure (used onlu if CalcDeflection = 1)
		Real64 const Pini, // Initial presssure at time of fabrication (used only if CalcDeflection = 1)
		Real64 const Tini, // Initial temperature at time of fabrication (used only if CalcDeflection = 1)
		Array1A< Real64 > gap, // Vector of gap widths [m]
		Array1A< Real64 > GapDefMax, // Vector of gap widths in deflected state. It will be used as input
		Array1A< Real64 > thick, // Vector of glazing layer thicknesses [m]
		Array1A< Real64 > scon, // Vector of conductivities of each glazing layer  [W/mK]
		Array1A< Real64 > const YoungsMod, // Youngs Modulus coefficients used in deflection calculations
		Array1A< Real64 > const PoissonsRat, // Poissons Ratio coefficients used in deflection calculations
		Array1A< Real64 > const tir, // Vector of IR transmittances of each surface
		Array1A< Real64 > const emis, // Vector of IR emittances of each surface
		Real64 const totsol, // Total solar transmittance of the IGU
		Real64 const tilt, // Window tilt [degrees]
		Array1A< Real64 > const asol, // Vector of Absorbed solar energy fractions for each layer
		Real64 const height, // IGU cavity height
		Real64 const heightt, // Window height
		Real64 const width, // Window width
		Array1A< Real64 > const presure, // Vector of gas pressures in gaps [N/m2]
		Array2A_int const iprop, // Matrix of gas codes - see mgas definition
		Array2A< Real64 > const frct, // Matrix of mass percentages in gap mixtures
		Array2A< Real64 > const xgcon, // Matrix of constants for gas conductivity calc
		Array2A< Real64 > const xgvis, // Matrix of constants for gas dynamic viscosity calc
		Array2A< Real64 > const xgcp, // Matrix of constants for gas specific heat calc at constant pressure
		Array1A< Real64 > const xwght, // Vector of Molecular weights for gasses
		Array1A< Real64 > const gama, // Vector of spefic heat ration for low pressure calc
		Array1A_int const nmix, // Vector of number of gasses in gas mixture of each gap
		Array1A_int const SupportPillar, // Shows whether or not gap have support pillar
		Array1A< Real64 > const PillarSpacing, // Pillar spacing for each gap (used in case there is support pillar)
		Array1A< Real64 > const PillarRadius, // Pillar radius for each gap (used in case there is support pillar)
		Array1A< Real64 > theta, // Vector of average temperatures of glazing surfaces [K]
		Array1A< Real64 > LayerDef, // Vector of layers deflection. [m]
		Array1A< Real64 > q, // Vector of various heat fluxes [W/m2]
		Array1A< Real64 > qv, // Vector of heat fluxes to each gap by ventillation [W/m2]
		Real64 & ufactor, // Center of glass U-value [W/m2 K]
		Real64 & sc, // Shading Coefficient
		Real64 & hflux, // Net heat flux between room and window [W/m2]
		Real64 & hcin, // Indoor convective surface heat transfer coefficient  [W/m2 K]
		Real64 & hcout, // Outdoor convective surface heat transfer coefficient [W/m2 K]
		Real64 & hrin, // Indoor radiative surface heat transfer coefficient [W/m2 K]
		Real64 & hrout, // Outdoor radiative surface heat transfer coefficient [W/m2 K]
		Real64 & hin, // Indoor combined film coefficient (if non-zero) [W/m2K]
		Real64 & hout, // Outdoor combined film coefficient (if non-zero) [W/m2K]
		Array1A< Real64 > hcgas, // Convective part of gap effective conductivity (including in and out)
		Array1A< Real64 > hrgas, // Radiative part of gap effective conductivity (including in and out)
		Real64 & shgc, // Solar heat gain coefficient - per ISO 15099
		int & nperr, // Error code
		std::string & ErrorMessage, // To store error message from tarcog execution
		Real64 & shgct, // Solar heat gain coefficient - per old procedure
		Real64 & tamb, // Outdoor environmental temperature [K]
		Real64 & troom, // Indoor environmental temperature [K]
		Array1A_int const ibc, // Vector of boundary condition flags (ibc(1) - outdoor, ibc(2) - indoor
		Array1A< Real64 > const Atop, // Vector with areas of top openings - between SD layers and top of
		Array1A< Real64 > const Abot, // Vector with areas of bottom openings - between SD layers and
		Array1A< Real64 > const Al, // Vector with areas of left-hand side openings - between SD layers and
		Array1A< Real64 > const Ar, // Vector of areas of right-hand side openings - between SD layers and
		Array1A< Real64 > const Ah, // Vector of total areas of holes for each SD [m2]
		Array1A< Real64 > const SlatThick, // Thickness of the slat material [m]
		Array1A< Real64 > const SlatWidth, // Slat width [m]
		Array1A< Real64 > const SlatAngle, // Slat tilt angle [deg]
		Array1A< Real64 > const SlatCond, // Conductivity of the slat material [W/m.K]
		Array1A< Real64 > const SlatSpacing, // Distance between slats [m]
		Array1A< Real64 > const SlatCurve, // Curvature radius of the slat [m]
		Array1A< Real64 > const vvent, // Vector of velocities for forced ventilation, for each gap, and for
		Array1A< Real64 > const tvent, // Vector of temperatures of ventilation gas for forced ventilation,
		Array1A_int const LayerType, // Glazing layer type flag
		Array1A_int const nslice, // Vector of numbers of slices in a laminated glazing layers
		Array1A< Real64 > const LaminateA, // Left-hand side array for creating slice equations
		Array1A< Real64 > const LaminateB, // Right-hand side array for creating slice equations
		Array1A< Real64 > const sumsol, // Array of absorbed solar energy fractions for each laminated
		Array1A< Real64 > hg, // Gas conductance of the glazing cavity [W/m2 K]
		Array1A< Real64 > hr, // Radiation conductance of the glazing cavity [W/m2 K]
		Array1A< Real64 > hs, // Thermal conductance of the glazing cavity [W/m2 K]
		Real64 & he, // External heat transfer coefficient [W/m2 K] - EN673 and ISO 10292 procedure
		Real64 & hi, // Internal heat transfer coefficient [W/m2 K] - EN673 and ISO 10292 procedure
		Array1A< Real64 > Ra, // Vector of Rayleigh numbers, for each gap
		Array1A< Real64 > Nu, // Vector of Nusselt numbers, for each gap
		int const standard, // Calculation standard switch:
		int const ThermalMod, // Thermal model:
		int const Debug_mode, // Switch for debug output files:
		std::string const & Debug_dir, // Target directory for debug files
		std::string const & Debug_file, // File name template for debug files
		int const win_ID, // ID of window (passed by W6)
		int const igu_ID, // ID of the IGU (passed by W6)
		Real64 & ShadeEmisRatioOut, // Ratio of modified to glass emissivity at the outermost glazing surface
		Real64 & ShadeEmisRatioIn, // Ratio of modified to glass emissivity at the innermost glazing surface
		Real64 & ShadeHcRatioOut, // Ratio of modified to unshaded Hc at the outermost glazing surface
		Real64 & ShadeHcRatioIn, // Ratio of modified to unshaded Hc at the innermost glazing surface
		Real64 & HcUnshadedOut, // Hc value at outermost glazing surface of an unshaded subsystem [W/m2 K]
		Real64 & HcUnshadedIn, // Hc value at innermost glazing surface of an unshaded subsystem [W/m2 K]
		Array1A< Real64 > Keff, // Vector of keff values for gaps [W/m.K]
		Array1A< Real64 > ShadeGapKeffConv, // Vector of convective keff values for areas above/below
		Real64 const SDScalar, // Factor of Venetian SD layer contribution to convection
		int const SHGCCalc, // SHGC calculation switch:
		int & NumOfIterations // Number of iterations for reacing solution
	)
	{

		/// function attributes:

		/// INPUTS:

		/// General:

		// Argument array dimensioning
		gap.dim( maxlay );
		GapDefMax.dim( MaxGap );
		thick.dim( maxlay );
		scon.dim( maxlay );
		YoungsMod.dim( maxlay );
		PoissonsRat.dim( maxlay );
		tir.dim( maxlay2 );
		emis.dim( maxlay2 );
		asol.dim( maxlay );
		presure.dim( maxlay1 );
		iprop.dim( maxgas, maxlay1 );
		frct.dim( maxgas, maxlay1 );
		xgcon.dim( 3, maxgas );
		xgvis.dim( 3, maxgas );
		xgcp.dim( 3, maxgas );
		xwght.dim( maxgas );
		gama.dim( maxgas );
		nmix.dim( maxlay1 );
		SupportPillar.dim( maxlay );
		PillarSpacing.dim( maxlay );
		PillarRadius.dim( maxlay );
		theta.dim( maxlay2 );
		LayerDef.dim( maxlay );
		q.dim( maxlay3 );
		qv.dim( maxlay1 );
		hcgas.dim( maxlay1 );
		hrgas.dim( maxlay1 );
		ibc.dim( 2 );
		Atop.dim( maxlay );
		Abot.dim( maxlay );
		Al.dim( maxlay );
		Ar.dim( maxlay );
		Ah.dim( maxlay );
		SlatThick.dim( maxlay );
		SlatWidth.dim( maxlay );
		SlatAngle.dim( maxlay );
		SlatCond.dim( maxlay );
		SlatSpacing.dim( maxlay );
		SlatCurve.dim( maxlay );
		vvent.dim( maxlay1 );
		tvent.dim( maxlay1 );
		LayerType.dim( maxlay );
		nslice.dim( maxlay );
		LaminateA.dim( maxlay );
		LaminateB.dim( maxlay );
		sumsol.dim( maxlay );
		hg.dim( maxlay );
		hr.dim( maxlay );
		hs.dim( maxlay );
		Ra.dim( maxlay );
		Nu.dim( maxlay );
		Keff.dim( maxlay );
		ShadeGapKeffConv.dim( MaxGap );

		// Locals
		//    1 - ISO15099
		//    2 - ISO10292
		//    3 - EN673
		//    0 - ISO15099
		//    1 - Thermal model 1
		//    2 - Thermal model 2 (not implemented)
		//    0 - don't create debug output files
		//    1 - append results to existing debug output file
		//    2 - store results in new debug output file
		//   3 - save in-between results (in all iterations) to existing debug file
		//    0 - do not perform SHGC calculations
		//    1 - perform SHGC calculations

		/// Environment related:
		//    0 - windward
		//    1 - leeward
		//    0 - both Tsky and Esky are specified
		//    1 - Tsky specified; esky = 1
		//    2 - Swinbank model for effective sky emittance
		//    3 - IR radiance is provided from external source
		//integer, intent(in) :: mgas  ! Flag for gas property constants:
		//    0 - gas constants supplied (through gcon, gvis and gcp arrays)
		//    1 - use internal constants; when internal then first index
		//        in gcon, gciv and gcp is:
		//          1 - Air
		//          2 - Argon
		//          3 - Krypton
		//          4 - Xenon
		//    0 - h to be calculated
		//    1 - combined film coefficient h prescribed
		//    2 - convective film coefficient (hc) prescibed
		// Also used in old algorithms for calculating h, accessible through negative
		// values for flags:
		//    -1 - old SPC142 correlation
		//    -2 - Klems-Yazdanian correlation (applicable to outdoor only)
		//    -3 - Kimura correlation (applicable to outdoor only)

		/// Layers:
		//    0 - Specular layer
		//    1 - Venetian blind (SD)
		//    2 - Woven shade (SD) (not implemented)
		//    3 - Diffuse shade

		/// Venetians:
		// glazing cavity [m2]
		// bottom of glazing cavity [m2]
		// left end of glazing cavity [m2]
		// right end of glazing cavity [m2]
		// outdoor and indoor environment [m/s]
		// for each gap, and for outdoor and indoor environment

		/// Laminates:
		// (0 - monolithic layer)
		// glazing layer [W/m2]

		/// Gaps:
		//  (A, B, C for max of 10 gasses)
		//  (A, B, C for max of 10 gasses)
		//  (A, B, C for max of 10 gasses)

		//   0 - does not have support pillar
		//   1 - have support pillar

		// (used in conjunction with Thermal Model 2; otherwise, this value is ignored by TARCOG)
		// - REAL(r64) value between 0 (SD contribution to convection is neglected) and
		//  1 (SD treated as "closed" - as if it is a glass layer with thermal
		//  properties of SD slat material)

		//Deflection
		//    0 - no deflection calculations
		//    1 - perform deflection calculation (input is Pressure/Temp)
		//    2 - perform deflection calculation (input is measured deflection)
		// if CalcDeflection = 2. In case CalcDeflection = 1 it will return
		// recalculated gap widths. [m]

		//// INPUTS/OUTPUTS:

		/// OUTPUTS:
		/// Overall:

		/// Layers:
		// depending on element index:
		//    1 - qout (heat flux from outer-most glazing surface to outdoor space)
		//    2*i = qpane(i) (heat flux through i-th glazing layer
		//    2*i-1 = qgap(i) (heat flux from i-th glazing cavity to indoor-faced
		//                      surface of the adjacent glazing layer)
		//    2*nlayer + 1 = qin (heat flux from indoor space to inner-most glazing
		//                        surface)

		/// Gaps:
		//      - EN673 and ISO 10292 procedure
		//      - EN673 and ISO 10292 procedure
		//      - EN673 and ISO 10292 procedure

		/// Shading related:
		//  SD layers [W/m.K]

		// temporary variables stored between deflection iterations because tarcog need to produce result with exact same
		// input variables
		Real64 eskyTemp;
		Real64 trminTemp;
		Real64 hinTemp;
		Real64 houtTemp;
		static Array1D< Real64 > sconTemp( maxlay );
		static Array1D< Real64 > thickTemp( maxlay );

		//REAL(r64), dimension(maxlay) ::  sol ! Vector of Absorbed solar energy for each layer [W/m2] = dir*asol

		//Internaly used
		static bool converged( false ); // used for convergence check in case of deflection calculations
		static Array1D< Real64 > told( maxlay2 );
		static Array1D< Real64 > CurGap( MaxGap );
		static Array1D< Real64 > GapDefMean( MaxGap );
		Real64 dtmax;
		int i;
		int counter;

		//!! Body of TARCOG90

		he = 0.0;
		hi = 0.0;
		hcin = 0.0;
		hrin = 0.0;
		hcout = 0.0;
		hrout = 0.0;
		LayerDef = 0.0;
		dtmax = 0.0;
		i = 0;
		counter = 0;
		eskyTemp = 0.0;
		trminTemp = 0.0;
		hinTemp = 0.0;
		houtTemp = 0.0;
		ErrorMessage = "Normal Termination";

		//sol = 0.0d0
		//if (dir.ne.0) then
		//  do i= 1, nlayer
		//    sol(i) = dir * asol(i)
		//  end do
		//end if

		for ( i = 1; i <= nlayer - 1; ++i ) {
			CurGap( i ) = gap( i );
		}

		//  Prepare common debug variables:
		PrepDebugFilesAndVariables( Debug_dir, Debug_file, Debug_mode, win_ID, igu_ID, nperr );

		// Check input arguments:
		nperr = ArgCheck( nlayer, iwd, tout, tind, trmin, wso, wsi, dir, outir, isky, tsky, esky, fclr, VacuumPressure, VacuumMaxGapThickness, CalcDeflection, Pa, Pini, Tini, gap, GapDefMax, thick, scon, YoungsMod, PoissonsRat, tir, emis, totsol, tilt, asol, height, heightt, width, presure, iprop, frct, xgcon, xgvis, xgcp, xwght, gama, nmix, SupportPillar, PillarSpacing, PillarRadius, hin, hout, ibc, Atop, Abot, Al, Ar, Ah, SlatThick, SlatWidth, SlatAngle, SlatCond, SlatSpacing, SlatCurve, vvent, tvent, LayerType, nslice, LaminateA, LaminateB, sumsol, standard, ThermalMod, SDScalar, ErrorMessage );

		// in case of provided deflected gap widths just store deflected widhts before temperatures calculation
		// deflections in this case do not depend of temperatures and it should be calculated before to avoid
		// one extra call of temperatures calculations
		if ( CalcDeflection == DEFLECTION_CALC_GAP_WIDTHS ) {
			PanesDeflection( CalcDeflection, width, height, nlayer, Pa, Pini, Tini, thick, gap, GapDefMax, GapDefMean, theta, YoungsMod, PoissonsRat, LayerDef, nperr, ErrorMessage );
			for ( i = 1; i <= nlayer - 1; ++i ) {
				CurGap( i ) = GapDefMean( i );
			} //do i = 1, nlayer - 1
		}

		// in case of deflection calculation for temperature & pressure input some variables needs to be stored because
		// Calc_ISO15099 and EN673 routines will change them and for deflection recalculation everything needs to be
		// called in same way except for changed gap widths
		if ( CalcDeflection == DEFLECTION_CALC_TEMPERATURE ) {
			eskyTemp = esky;
			trminTemp = trmin;
			hinTemp = hin;
			houtTemp = hout;
			sconTemp = scon;
			thickTemp = thick;
		}

		if ( GoAhead( nperr ) ) {

			if ( standard == ISO15099 ) {
				Calc_ISO15099( nlayer, iwd, tout, tind, trmin, wso, wsi, dir, outir, isky, tsky, esky, fclr, VacuumPressure, VacuumMaxGapThickness, CurGap, thick, scon, tir, emis, totsol, tilt, asol, height, heightt, width, presure, iprop, frct, xgcon, xgvis, xgcp, xwght, gama, nmix, SupportPillar, PillarSpacing, PillarRadius, theta, q, qv, ufactor, sc, hflux, hcin, hcout, hrin, hrout, hin, hout, hcgas, hrgas, shgc, nperr, ErrorMessage, shgct, tamb, troom, ibc, Atop, Abot, Al, Ar, Ah, SlatThick, SlatWidth, SlatAngle, SlatCond, SlatSpacing, SlatCurve, vvent, tvent, LayerType, nslice, LaminateA, LaminateB, sumsol, Ra, Nu, ThermalMod, Debug_mode, ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcRatioOut, ShadeHcRatioIn, HcUnshadedOut, HcUnshadedIn, Keff, ShadeGapKeffConv, SDScalar, SHGCCalc, NumOfIterations );
			} else if ( ( standard == EN673 ) || ( standard == EN673Design ) ) {
				Calc_EN673( standard, nlayer, tout, tind, CurGap, thick, scon, emis, totsol, tilt, dir, asol, presure, iprop, frct, nmix, xgcon, xgvis, xgcp, xwght, theta, ufactor, hcin, hin, hout, shgc, nperr, ErrorMessage, ibc, hg, hr, hs, Ra, Nu );
			} else {
			}

		}

		//Deflection calculations in case of temperature & pressure inputs
		if ( GoAhead( nperr ) ) {
			if ( ! ( GoAhead( nperr ) ) ) {
				return;
			}

			if ( CalcDeflection == DEFLECTION_CALC_TEMPERATURE ) {
				converged = false;
				while ( ! ( converged ) ) {
					PanesDeflection( CalcDeflection, width, height, nlayer, Pa, Pini, Tini, thick, gap, GapDefMax, GapDefMean, theta, YoungsMod, PoissonsRat, LayerDef, nperr, ErrorMessage );

					if ( ! ( GoAhead( nperr ) ) ) {
						return;
					}

					//store temperatures before new calculations are performed. This is necessary in order to determine
					for ( i = 1; i <= 2 * nlayer; ++i ) {
						told( i ) = theta( i );
					} //do i=1, 2*nlayer

					//before calling thermal calculations, return back old variables
					esky = eskyTemp;
					trmin = trminTemp;
					hin = hinTemp;
					hout = houtTemp;
					scon = sconTemp;
					thick = thickTemp;

					//after performed deflection recalculate temperatures with new gap widths
					if ( standard == ISO15099 ) {
						Calc_ISO15099( nlayer, iwd, tout, tind, trmin, wso, wsi, dir, outir, isky, tsky, esky, fclr, VacuumPressure, VacuumMaxGapThickness, GapDefMean, thick, scon, tir, emis, totsol, tilt, asol, height, heightt, width, presure, iprop, frct, xgcon, xgvis, xgcp, xwght, gama, nmix, SupportPillar, PillarSpacing, PillarRadius, theta, q, qv, ufactor, sc, hflux, hcin, hcout, hrin, hrout, hin, hout, hcgas, hrgas, shgc, nperr, ErrorMessage, shgct, tamb, troom, ibc, Atop, Abot, Al, Ar, Ah, SlatThick, SlatWidth, SlatAngle, SlatCond, SlatSpacing, SlatCurve, vvent, tvent, LayerType, nslice, LaminateA, LaminateB, sumsol, Ra, Nu, ThermalMod, Debug_mode, ShadeEmisRatioOut, ShadeEmisRatioIn, ShadeHcRatioOut, ShadeHcRatioIn, HcUnshadedOut, HcUnshadedIn, Keff, ShadeGapKeffConv, SDScalar, SHGCCalc, NumOfIterations );
					} else if ( ( standard == EN673 ) || ( standard == EN673Design ) ) {
						Calc_EN673( standard, nlayer, tout, tind, GapDefMean, thick, scon, emis, totsol, tilt, dir, asol, presure, iprop, frct, nmix, xgcon, xgvis, xgcp, xwght, theta, ufactor, hcin, hin, hout, shgc, nperr, ErrorMessage, ibc, hg, hr, hs, Ra, Nu );
					} else {
					} //select case (standard)

					if ( ! ( GoAhead( nperr ) ) ) {
						return;
					}

					//calc error
					dtmax = 0.0;
					for ( i = 1; i <= 2 * nlayer; ++i ) {
						dtmax = std::abs( told( i ) - theta( i ) );
					} //do i=1, 2*nlayer

					if ( dtmax < DeflectionErrorMargin ) {
						converged = true;
					}
					++counter;

					if ( counter > DeflectionMaxIterations ) {
						converged = true;
						nperr = 41; // Deflection calculations failed to converge
						ErrorMessage = "Deflection calculations failed to converge";
					}
				} //do while (.not.(converged))
			} // if ((CalcDeflection.eq.DEFLECTION_CALC_TEMPERATURE).or.(CalcDeflection.eq.DEFLECTION_CALC_GAP_WIDTHS)) then
		} // if (GoAhead(nperr)) then

		FinishDebugOutputFiles( nperr );

	}

} // TARCOGMain

} // EnergyPlus
