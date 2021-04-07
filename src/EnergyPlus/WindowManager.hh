// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

#ifndef WindowManager_hh_INCLUDED
#define WindowManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2A.hh>
#include <ObjexxFCL/Array3D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/WindowEquivalentLayer.hh>
#include <EnergyPlus/WindowManagerExteriorData.hh>
#include <EnergyPlus/WindowModel.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace WindowManager {

    class CWindowModel;
    class CWindowOpticalModel;
    class CWindowConstructionsSimplified;

    // SUBROUTINE SPECIFICATIONS FOR MODULE WindowManager:
    //   Optical Calculation Routines
    //   Heat Balance Routines

    void InitWindowOpticalCalculations(EnergyPlusData &state);

    void InitGlassOpticalCalculations(EnergyPlusData &state);

    //*****************************************************************************************

    void W5InitGlassParameters(EnergyPlusData &state);

    //****************************************************************************
    // WINDOW 5 Optical Calculation Subroutines
    //****************************************************************************

    void SystemSpectralPropertiesAtPhi(EnergyPlusData &state,
                                       int const iquasi,   // When there is no spectral data, this is the wavelength
                                       int const ngllayer, // Number of glass layers in construction
                                       Real64 const wlbot, // Lowest and highest wavelength considered
                                       Real64 const wltop);

    //************************************************************************

    void SystemPropertiesAtLambdaAndPhi(EnergyPlusData &state,
                                        int const n, // Number of glass layers
                                        Real64 &tt,  // System transmittance
                                        Real64 &rft, // System front and back reflectance
                                        Real64 &rbt,
                                        Array1A<Real64> aft // System absorptance of each glass layer
    );

    //*************************************************************************

    void SolarSprectrumAverage(EnergyPlusData &state,
                               Array1A<Real64> p, // Quantity to be weighted by solar spectrum
                               Real64 &psol       // Quantity p weighted by solar spectrum
    );

    //********************************************************************

    void VisibleSprectrumAverage(EnergyPlusData &state,
                                 Array1A<Real64> p, // Quantity to be weighted by solar spectrum
                                 Real64 &pvis       // Quantity p weighted by solar spectrum and photopic
    );

    //**********************************************************************

    void Interpolate(Array1A<Real64> x, // Array of data points for independent variable
                     Array1A<Real64> y, // Array of data points for dependent variable
                     int const npts,    // Number of data pairs
                     Real64 const xin,  // Given value of x
                     Real64 &yout       // Interpolated value of y at xin
    );

    //***********************************************************************************
    // Window Thermal Calculation Subroutines
    //***********************************************************************************

    void CalcWindowHeatBalance(EnergyPlusData &state,
                               int const SurfNum,          // Surface number
                               Real64 const HextConvCoeff, // Outside air film conductance coefficient
                               Real64 &SurfInsideTemp,     // Inside window surface temperature
                               Real64 &SurfOutsideTemp     // Outside surface temperature (C)
    );

    void CalcWindowHeatBalanceInternalRoutines(EnergyPlusData &state,
                                               int const SurfNum,          // Surface number
                                               Real64 const HextConvCoeff, // Outside air film conductance coefficient
                                               Real64 &SurfInsideTemp,     // Inside window surface temperature
                                               Real64 &SurfOutsideTemp     // Outside surface temperature (C)
    );

    //****************************************************************************

    void WindowHeatBalanceEquations(EnergyPlusData &state, int const SurfNum); // Surface number

    //****************************************************************************

    void SolveForWindowTemperatures(EnergyPlusData &state, int const SurfNum); // Surface number

    //****************************************************************************

    void ExtOrIntShadeNaturalFlow(EnergyPlusData &state,
                                  int const SurfNum,  // Surface number
                                  int const iter,     // Iteration number for glass heat balance calculation
                                  Real64 &VGap,       // Air velocity in glass-shade/blind gap (m/s)
                                  Real64 &TGapNew,    // Current-iteration average air temp in glass-shade/blind gap (K)
                                  Real64 &TGapOutlet, // Temperature of air leaving glass-shade/blind gap at top for upward
                                  Real64 &hcv,        // Convection coefficient from gap glass or shade to gap air (W/m2-K)
                                  Real64 &QConvGap    // Convective heat gain from glass-shade/blind gap for interior shade (W)
    );

    //****************************************************************************

    void BetweenGlassShadeNaturalFlow(EnergyPlusData &state,
                                      int const SurfNum,       // Surface number
                                      int const iter,          // Iteration number for glass heat balance calculation
                                      Real64 &VGap,            // Gas velocity in gaps (m/s)
                                      Array1A<Real64> TGapNew, // Current-iteration average gas temp in gaps (K)
                                      Array1A<Real64> hcv      // Convection coefficient from gap glass or shade to gap gas (W/m2-K)
    );

    //****************************************************************************

    void BetweenGlassForcedFlow(EnergyPlusData &state,
                                int const SurfNum,  // Surface number
                                int const iter,     // Iteration number for glass heat balance calculation
                                Real64 &VGap,       // Air velocity in airflow gap (m/s)
                                Real64 &TGapNew,    // Current-iteration average air temp in airflow gap (K)
                                Real64 &TGapOutlet, // Temperature of air leaving glass-shade/blind gap at top for upward
                                Real64 &hcv,        // Convection coefficient from gap glass faces to gap air (W/m2-K)
                                Real64 &QConvGap    // Convective heat gain from air flow gap (W)
    );

    //****************************************************************************

    void BetweenGlassShadeForcedFlow(EnergyPlusData &state,
                                     int const SurfNum,       // Surface number
                                     int const iter,          // Iteration number for glass heat balance calculation
                                     Real64 &VGap,            // Air velocity in each gap (m/s)
                                     Array1A<Real64> TGapNew, // Current-iteration average gas temp in gaps (K)
                                     Real64 &TGapOutletAve,   // Average of TGapOutlet(1) and TGapOutlet(2) (K)
                                     Array1A<Real64> hcv,     // Convection coefficient from gap glass or shade to gap gas (W/m2-K)
                                     Real64 &QConvTot         // Sum of convective heat flow from gaps (W)
    );

    //****************************************************************************

    void LUdecomposition(EnergyPlusData &state,
                         Array2<Real64> &ajac, // As input: matrix to be decomposed;
                         int const n,          // Dimension of matrix
                         Array1D_int &indx,    // Vector of row permutations
                         Real64 &d             // +1 if even number of row interchange is even, -1
    );

    //**************************************************************************

    void LUsolution(Array2<Real64> const &a, // Matrix and vector in a.x = b;
                    int const n,             // Dimension of a and b
                    Array1D_int const &indx, // Vector of row permutations
                    Array1D<Real64> &b       // Matrix and vector in a.x = b;
    );

    //******************************************************************************

    void WindowGasConductance(EnergyPlusData &state,
                              Real64 const tleft,  // Temperature of gap surface closest to outside (K)
                              Real64 const tright, // Temperature of gap surface closest to zone (K)
                              int const IGap,      // Gap number
                              Real64 &con,         // Gap gas conductance (W/m2-K)
                              Real64 &pr,          // Gap gas Prandtl number
                              Real64 &gr           // Gap gas Grashof number
    );

    //******************************************************************************

    void WindowGasPropertiesAtTemp(EnergyPlusData &state,
                                   Real64 const tmean, // Temperature of gas in gap (K)
                                   int const IGap,     // Gap number
                                   Real64 &dens,       // Gap gas density at tmean (kg/m3)
                                   Real64 &visc        // Gap gas dynamic viscosity at tmean (g/m-s)
    );

    //********************************************************************************

    void StartingWindowTemps(EnergyPlusData &state,
                             int const SurfNum,          // Surface number
                             Array1A<Real64> AbsRadShade // Short-wave radiation absorbed by shade/blind faces
    );

    //****************************************************************************

    void NusseltNumber(EnergyPlusData &state,
                       int const SurfNum, // Surface number
                       Real64 const tso,  // Temperature of gap surface closest to outside (K)
                       Real64 const tsi,  // Temperature of gap surface closest to zone (K)
                       int const IGap,    // Gap number
                       Real64 const gr,   // Gap gas Grashof number
                       Real64 const pr,   // Gap gas Prandtl number
                       Real64 &gnu        // Gap gas Nusselt number
    );

    //*******************************************************************************************************

    void TransAndReflAtPhi(Real64 const cs,                // Cosine of incidence angle
                           Real64 const tf0,               // Transmittance at zero incidence angle
                           Real64 const rf0,               // Front reflectance at zero incidence angle
                           Real64 const rb0,               // Back reflectance at zero incidence angle
                           Real64 &tfp,                    // Transmittance at cs
                           Real64 &rfp,                    // Front reflectance at cs
                           Real64 &rbp,                    // Back reflectance at cs
                           bool const SimpleGlazingSystem, // .TRUE. if simple block model being used
                           Real64 const SimpleGlazingSHGC, // SHGC value to use in alternate model for simple glazing system
                           Real64 const SimpleGlazingU     // U-factor value to use in alternate model for simple glazing system
    );

    Real64 InterpolateBetweenTwoValues(Real64 const X, Real64 const X0, Real64 const X1, Real64 const F0, Real64 const F1);

    Real64 InterpolateBetweenFourValues(Real64 const X,
                                        Real64 const Y,
                                        Real64 const X1,
                                        Real64 const X2,
                                        Real64 const Y1,
                                        Real64 const Y2,
                                        Real64 const Fx1y1,
                                        Real64 const Fx1y2,
                                        Real64 const Fx2y1,
                                        Real64 const Fx2y2);

    //**************************************************************************

    void W5LsqFit(Array1S<Real64> const IndepVar, // Independent variables
                  Array1S<Real64> const DepVar,   // Dependent variables
                  int const N,                    // Order of polynomial
                  int const N1,                   // First and last data points used
                  int const N2,
                  Array1S<Real64> CoeffsCurve // Polynomial coeffients from fit
    );

    //********************************************************************************

    void W5LsqFit2(Array1A<Real64> const IndepVar, // Independent variables
                   Array1A<Real64> const DepVar,   // Dependent variables
                   int const N,                    // Order of polynomial
                   int const N1,                   // First and last data points used
                   int const N2,
                   Array1A<Real64> CoeffsCurve // Polynomial coeffients from fit
    );

    //***********************************************************************

    Real64 DiffuseAverage(Array1S<Real64> const PropertyValue); // Property value at angles of incidence

    //*************************************************************************************

    Real64 DiffuseAverageProfAngGnd(Array1S<Real64> const Property); // Property value vs. profile angle

    //*************************************************************************************

    Real64 DiffuseAverageProfAngSky(Array1S<Real64> const Property); // Property value vs. profile angle

    //*************************************************************************************

    void CalcWinFrameAndDividerTemps(EnergyPlusData &state,
                                     int const SurfNum,     // Surface number
                                     Real64 const tout,     // Outside air temperature (K)
                                     Real64 const tin,      // Inside air temperature (K)
                                     Real64 const HOutConv, // Outside convective air film conductance (W/m2-K)
                                     Real64 const HInConv,  // Inside convective air film conductance (W/m2-K)
                                     Real64 const Outir,    // Exterior IR irradiance from sky and ground
                                     int const ConstrNum    // Construction number of window
    );

    //************************************************************************************

    void CalcNominalWindowCond(EnergyPlusData &state,
                               int const ConstrNum,        // Construction number
                               int const WinterSummerFlag, // 1=winter, 2=summer
                               Real64 &NominalConductance, // Nominal center-of-glass conductance, including air films
                               Real64 &SHGC,               // Nominal center-of-glass solar heat gain coefficient for
                               Real64 &TSolNorm,           // Overall beam solar transmittance at normal incidence
                               Real64 &TVisNorm,           // Overall beam visible transmittance at normal incidence
                               int &errFlag                // Error flag
    );

    //****************************************************************************

    void WindowTempsForNominalCond(EnergyPlusData &state,
                                   int const ConstrNum, // Construction number
                                   Array1A<Real64> hgap // Gap gas conductive conductance (W/m2-K)
    );

    //****************************************************************************

    void StartingWinTempsForNominalCond(EnergyPlusData &state);

    //****************************************************************************

    void ReportGlass(EnergyPlusData &state);

    //*************************************************************************************

    void CalcWindowBlindProperties(EnergyPlusData &state);

    //*************************************************************************************

    void CalcWindowScreenProperties(EnergyPlusData &state);

    void BlindOpticsDiffuse(EnergyPlusData &state,
                            int const BlindNum,      // Blind number
                            int const ISolVis,       // 1 = solar and IR calculation; 2 = visible calculation
                            Array1A<Real64> const c, // Slat properties
                            Real64 const b_el,       // Slat elevation (radians)
                            Array1A<Real64> p        // Blind properties
    );

    //**********************************************************************************************

    void BlindOpticsBeam(EnergyPlusData &state,
                         int const BlindNum,      // Blind number
                         Array1A<Real64> const c, // Slat properties (equivalent to BLD_PR)
                         Real64 const b_el,       // Slat elevation (radians)
                         Real64 const s_el,       // Solar profile angle (radians)
                         Array1A<Real64> p        // Blind properties (equivalent to ST_LAY)
    );

    //********************************************************************************************

    void ViewFac(Real64 const s,    // Slat width (m)
                 Real64 const h,    // Distance between faces of adjacent slats (m)
                 Real64 const phib, // Elevation angle of normal to slat (radians)
                 Real64 const phis, // Profile angle of radiation source (radians)
                 Array2A<Real64> F  // View factor array
    );

    //*****************************************************************************************

    void InvertMatrix(EnergyPlusData &state,
                      Array2A<Real64> a, // Matrix to be inverted
                      Array2A<Real64> y, // Inverse of matrix a
                      Array1A_int indx,  // Index vector for LU decomposition
                      int const np,      // Dimension of matrix
                      int const n);

    //*****************************************************************************************

    void LUDCMP(EnergyPlusData &state,
                Array2A<Real64> A, // matrix
                int const N,
                int const NP,
                Array1A_int INDX,
                int &D);

    //*****************************************************************************************

    void LUBKSB(Array2A<Real64> A, int const N, int const NP, Array1A_int INDX, Array1A<Real64> B);

    // added for custom solar or visible spectrum

    void CheckAndReadCustomSprectrumData(EnergyPlusData &state);

    //*****************************************************************************************

    void initWindowModel(EnergyPlusData &state);

    //*****************************************************************************************

} // namespace WindowManager

struct WindowManagerData : BaseGlobalStruct
{

    Real64 sigma;   // Stefan-Boltzmann constant
    Real64 TKelvin; // conversion from Kelvin to Celsius
    int nume;       // Number of wavelength values in solar spectrum
    int numt3;      // Number of wavelength values in the photopic response

    //               Dens  dDens/dT  Con    dCon/dT   Vis    dVis/dT Prandtl dPrandtl/dT
    Array1D<Real64> AirProps;
    // Air mass 1.5 terrestrial solar global spectral irradiance (W/m2-micron)
    // on a 37 degree tilted surface; corresponds
    // to wavelengths (microns) in following data block (ISO 9845-1 and ASTM E 892;
    // derived from Optics5 data file ISO-9845GlobalNorm.std, 10-14-99)
    Array1D<Real64> wle; // Solar spectrum wavelength values (microns)

    Array1D<Real64> e; // Solar spectrum values corresponding to wle

    // Phototopic response function and corresponding wavelengths (microns)
    // (CIE 1931 observer; ISO/CIE 10527, CIE Standard Calorimetric Observers;
    // derived from Optics5 data file "CIE 1931 Color Match from E308.txt", which is
    // the same as WINDOW4 file Cie31t.dat)
    Array1D<Real64> wlt3; // Wavelength values for photopic response

    Array1D<Real64> y30; // Photopic response
                         // corresponding to
                         // wavelengths in wlt3

    int ngllayer;                    // Number of glass layers
    int nglface;                     // Number of glass faces
    int nglfacep;                    // Number of glass faces, + 2 if shade layer present
    Real64 tout;                     // Outside air temperature (K)
    Real64 tin;                      // Inside air temperature (previous timestep) (K)
    Real64 tilt;                     // Window tilt (deg)
    Real64 tiltr;                    // Window tilt (radians)
    Real64 hcin;                     // Convective inside air film conductance (W/m2-K)
    Real64 hcout;                    // Convective outside air film conductance (W/m2-K)
    Real64 Ebout;                    // Sigma*(outside air temp)**4 (W/m2)
    Real64 Outir;                    // IR radiance of window's exterior surround (W/m2)
    Real64 Rmir;                     // IR radiance of window's interior surround (W/m2)
    Real64 Rtot;                     // Total thermal resistance of window (m2-K/W)
    Array3D<Real64> gcon;            // Gas thermal conductivity coefficients for each gap
    Array3D<Real64> gvis;            // Gas viscosity coefficients for each gap
    Array3D<Real64> gcp;             // Gas specific-heat coefficients for each gap
    Array2D<Real64> gwght;           // Gas molecular weights for each gap
    Array2D<Real64> gfract;          // Gas fractions for each gap
    Array1D_int gnmix;               // Number of gases in gap
    Array1D<Real64> gap;             // Gap width (m)
    Array1D<Real64> thick;           // Glass layer thickness (m)
    Array1D<Real64> scon;            // Glass layer conductance--conductivity/thickness (W/m2-K)
    Array1D<Real64> tir;             // Front and back IR transmittance for each glass layer
    Array1D<Real64> emis;            // Front and back IR emissivity for each glass layer
    Array1D<Real64> rir;             // Front and back IR reflectance for each glass layer
                                     //  (program calculates from tir and emis)
    Array1D<Real64> AbsRadGlassFace; // Solar radiation and IR radiation from internal
                                     //  gains absorbed by glass face
    Array1D<Real64> thetas;          // Glass surface temperatures (K)
    Array1D<Real64> thetasPrev;      // Previous-iteration glass surface temperatures (K)
    Array1D<Real64> fvec;            // Glass face heat balance function
    Array2D<Real64> fjac;            // Glass face heat balance Jacobian
    Array1D<Real64> dtheta;          // Glass layer temperature difference factor [K]
    Array2D<Real64> zir;             // IR transfer matrix
    Array2D<Real64> ziri;            // Inverse of IR transfer matrix
    Array2D<Real64> ddeldt;          // Matrix of derivatives of residuals wrt temperature
    Array2D<Real64> dtddel;          // Inverse of matrix of derivatives of
                                     //   residuals wrt temperature
    Array1D<Real64> qf;              // IR heat flux at each face [W/m2]
    Array1D<Real64> hf;              // Component of convective flux at each face
    Array2D<Real64> der;             // Derivative of IR sources wrt surface temperature
    Array2D<Real64> dhf;             // Derivative of heat flux wrt surface temperature
    Array1D<Real64> sour;            // IR source term at each face [W/m2]
    Array1D<Real64> delta;           // Residual at each glass layer [W/m2]
    Array1D<Real64> hcgap;           // Convective gap conductance
    Array1D<Real64> hrgap;           // Radiative gap conductance
    Array1D<Real64> rgap;            // Convective plus radiative gap resistance
                                     //   (inverse of hcgap + hrgap)
    Array1D<Real64> rs;              // Outside film convective resistance, gap resistances,
                                     //   inside air film convective resistance
    Array1D<Real64> arhs;
    Real64 A23P; // Intermediate variables in glass face
    Real64 A32P;
    Real64 A45P;
    Real64 A54P;
    Real64 A67P;
    Real64 A76P;
    Real64 A23; // heat balance equations
    Real64 A45;
    Real64 A67;

    int MaxNumOfIncidentAngles;
    int const MaxSpectralDataElements; // Maximum number in Spectral Data arrays.
    // TEMP MOVED FROM DataHeatBalance.hh -BLB

    Array2D<Real64> wlt;      // Spectral data wavelengths for each glass layer in a glazing system
                              // Following data, Spectral data for each layer for each wavelength in wlt
    Array2D<Real64> t;        // normal transmittance
    Array2D<Real64> rff;      // normal front reflectance
    Array2D<Real64> rbb;      // normal back reflectance
    Array2D<Real64> tPhi;     // transmittance at angle of incidence
    Array2D<Real64> rfPhi;    // front reflectance at angle of incidence
    Array2D<Real64> rbPhi;    // back reflectance at angle of incidence
    Array2D<Real64> tadjPhi;  // transmittance at angle of incidence
    Array2D<Real64> rfadjPhi; // front reflectance at angle of incidence
    Array2D<Real64> rbadjPhi; // back reflectance at angle of incidence

    Array1D_int numpt;              // Number of spectral data wavelengths for each layer; =2 if no spectra data for a layer
    Array1D<Real64> stPhi;          // Glazing system transmittance at angle of incidence for each wavelength in wle
    Array1D<Real64> srfPhi;         // Glazing system front reflectance at angle of incidence for each wavelength in wle
    Array1D<Real64> srbPhi;         // Glazing system back reflectance at angle of incidence for each wavelenth in wle
    Array2D<Real64> saPhi;          // For each layer, glazing system absorptance at angle of incidence
                                    // for each wavelenth in wle
    Array2D<Real64> top;            // Transmittance matrix for subr. op
    Array2D<Real64> rfop;           // Front reflectance matrix for subr. op
    Array2D<Real64> rbop;           // Back transmittance matrix for subr. op
    Array1D<Real64> DepVarCurveFit; // Values of dependent variable corresponding to IndepVarCurveFit values
    Array1D<Real64> CoeffsCurveFit; // Polynomial coefficients from curve fit
    Array1D<Real64> tsolPhi;        // Glazing system solar transmittance for each angle of incidence
    Array1D<Real64> rfsolPhi;       // Glazing system solar front reflectance for each angle of incidence
    Array1D<Real64> rbsolPhi;       // Glazing system solar back reflectance for each angle of incidence
    Array2D<Real64> solabsPhi;      // Glazing system solar absorptance for each angle of incidence
    Array2D<Real64> solabsBackPhi;  // Glazing system back solar absorptance for each angle of incidence
    Array1D<Real64> solabsShadePhi; // Glazing system interior shade solar absorptance for each angle of incidence
    Array1D<Real64> tvisPhi;        // Glazing system visible transmittance for each angle of incidence
    Array1D<Real64> rfvisPhi;       // Glazing system visible front reflectance for each angle of incidence
    Array1D<Real64> rbvisPhi;       // Glazing system visible back reflectance for each angle of incidence
    Array1D<Real64> CosPhiIndepVar; // Cos of incidence angles at 10-deg increments for curve fits

    Array1D<int> LayerNum; // Glass layer number

    std::unique_ptr<WindowManager::CWindowModel> inExtWindowModel;       // Information about windows model (interior or exterior)
    std::unique_ptr<WindowManager::CWindowOpticalModel> winOpticalModel; // Information about windows optical model (Simplified or BSDF)

    bool RunMeOnceFlag = false;
    bool lSimpleGlazingSystem = false; // true if using simple glazing system block model
    bool BGFlag = false;               // True if between-glass shade or blind
    bool locTCFlag = false;            // True if this surface is a TC window
    bool DoReport = false;
    bool HasWindows = false;
    bool HasComplexWindows = false;
    bool HasEQLWindows = false;     // equivalent layer window defined
    Real64 SimpleGlazingSHGC = 0.0; // value of SHGC for simple glazing system block model
    Real64 SimpleGlazingU = 0.0;    // value of U-factor for simple glazing system block model
    Real64 tmpTrans = 0.0;          // solar transmittance calculated from spectral data
    Real64 tmpTransVis = 0.0;       // visible transmittance calculated from spectral data
    Real64 tmpReflectSolBeamFront = 0.0;
    Real64 tmpReflectSolBeamBack = 0.0;
    Real64 tmpReflectVisBeamFront = 0.0;
    Real64 tmpReflectVisBeamBack = 0.0;

    // Debug
    //        Array1D<Real64> DbgTheta = Array1D<Real64>(11, {0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 82.5, 89.5});
    //        Array1D<Real64> DbgTSol = Array1D<Real64>(11, 0.0);
    //        Array1D<Real64> DbgRbSol = Array1D<Real64>(11, 0.0);
    //        Array1D<Real64> DbgTVis = Array1D<Real64>(11, 0.0);
    //        Array2D<Real64> DbgFtAbs = Array2D<Real64>(5, 11, 0.0);
    //        Array2D<Real64> DbgBkAbs = Array2D<Real64>(5, 11, 0.0);
    //        Array1D<Real64> DbgFTAbsDiff = Array1D<Real64>(5, 0.0);
    //        Array1D<Real64> DbgBkAbsDiff = Array1D<Real64>(5, 0.0);
    // EndDebug

    Array1D<Real64> deltaTemp = Array1D<Real64>(100, 0.0);
    Array1D_int iMinDT = Array1D_int(1, 0);
    Array1D_int IDConst = Array1D_int(100, 0);

    Array1D<Real64> hgap = Array1D<Real64>(5);       // Gap gas conductance (W/m2-K)
    Array1D<Real64> hr = Array1D<Real64>(10);        // Radiative conductance (W/m2-K)
    Array1D_int indx = Array1D_int(10);              // Vector of row permutations in LU decomposition
    Array2D<Real64> Aface = Array2D<Real64>(10, 10); // Coefficient in equation Aface*thetas = Bface
    Array1D<Real64> Bface = Array1D<Real64>(10);     // Coefficient in equation Aface*thetas = Bface
    Array1D<Real64> hrprev = Array1D<Real64>(10);    // Value of hr from previous iteration
    Array1D<Real64> TGapNewBG = Array1D<Real64>(2);  // For between-glass shade/blind, average gas temp in gaps on either
    //  side of shade/blind (K)
    Array1D<Real64> hcvBG = Array1D<Real64>(2); // For between-glass shade/blind, convection coefficient from gap glass or
    //  shade/blind to gap gas on either side of shade/blind (W/m2-K)
    Array1D<Real64> AbsRadShadeFace = Array1D<Real64>(2); // Solar radiation, short-wave radiation from lights, and long-wave
    Array1D<Real64> RhoIR = Array1D<Real64>(10);          // Face IR reflectance

    Array1D<Real64> vv = Array1D<Real64>(10); // Stores the implicit scaling of each row

    Array1D<Real64> kprime = Array1D<Real64>(10);  // Monotonic thermal conductivity
    Array1D<Real64> kdblprm = Array1D<Real64>(10); // Conductivity term accounting for additional energy moved by
    //  the diffusional transport of internal energy in polyatomic gases.
    Array1D<Real64> mukpdwn = Array1D<Real64>(10); // Denominator term
    Array1D<Real64> kpdown = Array1D<Real64>(10);  // Denominator terms
    Array1D<Real64> kdpdown = Array1D<Real64>(10);
    Array1D<Real64> frct = Array1D<Real64>(10);  // Fraction of each gas in a mixture
    Array1D<Real64> fvis = Array1D<Real64>(10);  // Viscosity of each gas in a mixture (g/m-s)
    Array1D<Real64> fcon = Array1D<Real64>(10);  // Conductance of each gas in a mixture (W/m2-K)
    Array1D<Real64> fdens = Array1D<Real64>(10); // Density of each gas in a mixture (kg/m3)
    Array1D<Real64> fcp = Array1D<Real64>(10);   // Specific heat of each gas in a mixture (J/m3-K)

    void clear_state() override
    {
        this->wle = Array1D<Real64>(
            nume, {0.3000, 0.3050, 0.3100, 0.3150, 0.3200, 0.3250, 0.3300, 0.3350, 0.3400, 0.3450, 0.3500, 0.3600, 0.3700, 0.3800, 0.3900, 0.4000,
                   0.4100, 0.4200, 0.4300, 0.4400, 0.4500, 0.4600, 0.4700, 0.4800, 0.4900, 0.5000, 0.5100, 0.5200, 0.5300, 0.5400, 0.5500, 0.5700,
                   0.5900, 0.6100, 0.6300, 0.6500, 0.6700, 0.6900, 0.7100, 0.7180, 0.7244, 0.7400, 0.7525, 0.7575, 0.7625, 0.7675, 0.7800, 0.8000,
                   0.8160, 0.8237, 0.8315, 0.8400, 0.8600, 0.8800, 0.9050, 0.9150, 0.9250, 0.9300, 0.9370, 0.9480, 0.9650, 0.9800, 0.9935, 1.0400,
                   1.0700, 1.1000, 1.1200, 1.1300, 1.1370, 1.1610, 1.1800, 1.2000, 1.2350, 1.2900, 1.3200, 1.3500, 1.3950, 1.4425, 1.4625, 1.4770,
                   1.4970, 1.5200, 1.5390, 1.5580, 1.5780, 1.5920, 1.6100, 1.6300, 1.6460, 1.6780, 1.7400, 1.8000, 1.8600, 1.9200, 1.9600, 1.9850,
                   2.0050, 2.0350, 2.0650, 2.1000, 2.1480, 2.1980, 2.2700, 2.3600, 2.4500, 2.4940, 2.5370});
        this->e = Array1D<Real64>(
            nume, {0.0,    9.5,    42.3,   107.8,  181.0,  246.0,  395.3,  390.1,  435.3,  438.9,  483.7,  520.3,  666.2,  712.5,  720.7,  1013.1,
                   1158.2, 1184.0, 1071.9, 1302.0, 1526.0, 1599.6, 1581.0, 1628.3, 1539.2, 1548.7, 1586.5, 1484.9, 1572.4, 1550.7, 1561.5, 1501.5,
                   1395.5, 1485.3, 1434.1, 1419.9, 1392.3, 1130.0, 1316.7, 1010.3, 1043.2, 1211.2, 1193.9, 1175.5, 643.1,  1030.7, 1131.1, 1081.6,
                   849.2,  785.0,  916.4,  959.9,  978.9,  933.2,  748.5,  667.5,  690.3,  403.6,  258.3,  313.6,  526.8,  646.4,  746.8,  690.5,
                   637.5,  412.6,  108.9,  189.1,  132.2,  339.0,  460.0,  423.6,  480.5,  413.1,  250.2,  32.5,   1.6,    55.7,   105.1,  105.5,
                   182.1,  262.2,  274.2,  275.0,  244.6,  247.4,  228.7,  244.5,  234.8,  220.5,  171.5,  30.7,   2.0,    1.2,    21.2,   91.1,
                   26.8,   99.5,   60.4,   89.1,   82.2,   71.5,   70.2,   62.0,   21.2,   18.5,   3.2});
        this->wlt3 = Array1D<Real64>(numt3, {0.380, 0.385, 0.390, 0.395, 0.400, 0.405, 0.410, 0.415, 0.420, 0.425, 0.430, 0.435, 0.440, 0.445,
                                             0.450, 0.455, 0.460, 0.465, 0.470, 0.475, 0.480, 0.485, 0.490, 0.495, 0.500, 0.505, 0.510, 0.515,
                                             0.520, 0.525, 0.530, 0.535, 0.540, 0.545, 0.550, 0.555, 0.560, 0.565, 0.570, 0.575, 0.580, 0.585,
                                             0.590, 0.595, 0.600, 0.605, 0.610, 0.615, 0.620, 0.625, 0.630, 0.635, 0.640, 0.645, 0.650, 0.655,
                                             0.660, 0.665, 0.670, 0.675, 0.680, 0.685, 0.690, 0.695, 0.700, 0.705, 0.710, 0.715, 0.720, 0.725,
                                             0.730, 0.735, 0.740, 0.745, 0.750, 0.755, 0.760, 0.765, 0.770, 0.775, 0.780});
        this->y30 =
            Array1D<Real64>(numt3, {0.0000, 0.0001, 0.0001, 0.0002, 0.0004, 0.0006, 0.0012, 0.0022, 0.0040, 0.0073, 0.0116, 0.0168, 0.0230, 0.0298,
                                    0.0380, 0.0480, 0.0600, 0.0739, 0.0910, 0.1126, 0.1390, 0.1693, 0.2080, 0.2586, 0.3230, 0.4073, 0.5030, 0.6082,
                                    0.7100, 0.7932, 0.8620, 0.9149, 0.9540, 0.9803, 0.9950, 1.0000, 0.9950, 0.9786, 0.9520, 0.9154, 0.8700, 0.8163,
                                    0.7570, 0.6949, 0.6310, 0.5668, 0.5030, 0.4412, 0.3810, 0.3210, 0.2650, 0.2170, 0.1750, 0.1382, 0.1070, 0.0816,
                                    0.0610, 0.0446, 0.0320, 0.0232, 0.0170, 0.0119, 0.0082, 0.0158, 0.0041, 0.0029, 0.0021, 0.0015, 0.0010, 0.0007,
                                    0.0005, 0.0004, 0.0002, 0.0002, 0.0001, 0.0001, 0.0001, 0.0000, 0.0000, 0.0000, 0.0000});
        this->ngllayer = 0;
        this->nglface = 0;
        this->nglfacep = 0;
        this->tout = 0.0;
        this->tin = 0.0;
        this->tilt = 0.0;
        this->tiltr = 0.0;
        this->hcin = 0.0;
        this->hcout = 0.0;
        this->Ebout = 0.0;
        this->Outir = 0.0;
        this->Rmir = 0.0;
        this->Rtot = 0.0;
        this->gcon = Array3D<Real64>(3, 5, 5, 0.0);
        this->gvis = Array3D<Real64>(3, 5, 5, 0.0);
        this->gcp = Array3D<Real64>(3, 5, 5, 0.0);
        this->gwght = Array2D<Real64>(5, 5, 0.0);
        this->gfract = Array2D<Real64>(5, 5, 0.0);
        this->gnmix = Array1D_int(5, 0);
        this->gap = Array1D<Real64>(5, 0.0);
        this->thick = Array1D<Real64>(5, 0.0);
        this->scon = Array1D<Real64>(5, 0.0);
        this->tir = Array1D<Real64>(10, 0.0);
        this->emis = Array1D<Real64>(10, 0.0);
        this->rir = Array1D<Real64>(10, 0.0);
        this->AbsRadGlassFace = Array1D<Real64>(10, 0.0);
        this->thetas = Array1D<Real64>(10, 0.0);
        this->thetasPrev = Array1D<Real64>(10, 0.0);
        this->fvec = Array1D<Real64>(10, 0.0);
        this->fjac = Array2D<Real64>(10, 10, 0.0);
        this->dtheta = Array1D<Real64>(5, 0.0);
        this->zir = Array2D<Real64>(10, 10, 0.0);
        this->ziri = Array2D<Real64>(10, 10, 0.0);
        this->ddeldt = Array2D<Real64>(10, 10, 0.0);
        this->dtddel = Array2D<Real64>(10, 10, 0.0);
        this->qf = Array1D<Real64>(10, 0.0);
        this->hf = Array1D<Real64>(10, 0.0);
        this->der = Array2D<Real64>(5, 10, 0.0);
        this->dhf = Array2D<Real64>(5, 10, 0.0);
        this->sour = Array1D<Real64>(10, 0.0);
        this->delta = Array1D<Real64>(5, 0.0);
        this->hcgap = Array1D<Real64>(5, 0.0);
        this->hrgap = Array1D<Real64>(5, 0.0);
        this->rgap = Array1D<Real64>(6, 0.0);
        this->rs = Array1D<Real64>(6, 0.0);
        this->arhs = Array1D<Real64>(6, 0.0);
        this->A23P = 0.0;
        this->A32P = 0.0;
        this->A45P = 0.0;
        this->A54P = 0.0;
        this->A67P = 0.0;
        this->A76P = 0.0;
        this->A23 = 0.0;
        this->A45 = 0.0;
        this->A67 = 0.0;
        this->wlt = Array2D<Real64>(5, MaxSpectralDataElements, 0.0);
        this->t = Array2D<Real64>(5, MaxSpectralDataElements, 0.0);
        this->rff = Array2D<Real64>(5, MaxSpectralDataElements, 0.0);
        this->rbb = Array2D<Real64>(5, MaxSpectralDataElements, 0.0);
        this->tPhi = Array2D<Real64>(5, MaxSpectralDataElements, 0.0);
        this->rfPhi = Array2D<Real64>(5, MaxSpectralDataElements, 0.0);
        this->rbPhi = Array2D<Real64>(5, MaxSpectralDataElements, 0.0);
        this->tadjPhi = Array2D<Real64>(5, MaxSpectralDataElements, 0.0);
        this->rfadjPhi = Array2D<Real64>(5, MaxSpectralDataElements, 0.0);
        this->rbadjPhi = Array2D<Real64>(5, MaxSpectralDataElements, 0.0);
        this->numpt = Array1D_int(5, 0);
        this->stPhi = Array1D<Real64>(nume, 0.0);
        this->srfPhi = Array1D<Real64>(nume, 0.0);
        this->srbPhi = Array1D<Real64>(nume, 0.0);
        this->saPhi = Array2D<Real64>(5, nume, 0.0);
        this->top = Array2D<Real64>(5, 5, 0.0);
        this->rfop = Array2D<Real64>(5, 5, 0.0);
        this->rbop = Array2D<Real64>(5, 5, 0.0);
        this->DepVarCurveFit = Array1D<Real64>(10, 0.0);
        this->CoeffsCurveFit = Array1D<Real64>(6, 0.0);
        this->tsolPhi = Array1D<Real64>(MaxNumOfIncidentAngles, 0.0);
        this->rfsolPhi = Array1D<Real64>(MaxNumOfIncidentAngles, 0.0);
        this->rbsolPhi = Array1D<Real64>(MaxNumOfIncidentAngles, 0.0);
        this->solabsPhi = Array2D<Real64>(5, MaxNumOfIncidentAngles, 0.0);
        this->solabsBackPhi = Array2D<Real64>(5, MaxNumOfIncidentAngles, 0.0);
        this->solabsShadePhi = Array1D<Real64>(MaxNumOfIncidentAngles, 0.0);
        this->tvisPhi = Array1D<Real64>(MaxNumOfIncidentAngles, 0.0);
        this->rfvisPhi = Array1D<Real64>(MaxNumOfIncidentAngles, 0.0);
        this->rbvisPhi = Array1D<Real64>(MaxNumOfIncidentAngles, 0.0);
        this->CosPhiIndepVar = Array1D<Real64>(MaxNumOfIncidentAngles, 0.0);
        WindowManager::CWindowConstructionsSimplified::clearState();
        this->RunMeOnceFlag = false;
        this->lSimpleGlazingSystem = false; // true if using simple glazing system block model
        this->BGFlag = false;               // True if between-glass shade or blind
        this->locTCFlag = false;            // True if this surface is a TC window
        this->DoReport = false;
        this->HasWindows = false;
        this->HasComplexWindows = false;
        this->HasEQLWindows = false; // equivalent layer window defined
        this->SimpleGlazingSHGC = 0.0;
        this->SimpleGlazingU = 0.0;
        this->tmpTrans = 0.0;    // solar transmittance calculated from spectral data
        this->tmpTransVis = 0.0; // visible transmittance calculated from spectral data
        this->tmpReflectSolBeamFront = 0.0;
        this->tmpReflectSolBeamBack = 0.0;
        this->tmpReflectVisBeamFront = 0.0;
        this->tmpReflectVisBeamBack = 0.0;
        //            this->DbgTheta = Array1D<Real64>(11, {0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 82.5, 89.5});
        //            this->DbgTSol = Array1D<Real64>(11, 0.0);
        //            this->DbgRbSol = Array1D<Real64>(11, 0.0);
        //            this->DbgTVis = Array1D<Real64>(11, 0.0);
        //            this->DbgFtAbs = Array2D<Real64>(5, 11, 0.0);
        //            this->DbgBkAbs = Array2D<Real64>(5, 11, 0.0);
        //            this->DbgFTAbsDiff = Array1D<Real64>(5, 0.0);
        //            this->DbgBkAbsDiff = Array1D<Real64>(5, 0.0);
    }

    // Default Constructor
    WindowManagerData()
        : sigma(5.6697e-8), TKelvin(DataGlobalConstants::KelvinConv), nume(107), numt3(81), gcon(3, 5, 5, 0.0), gvis(3, 5, 5, 0.0), gcp(3, 5, 5, 0.0),
          gwght(5, 5, 0.0), gfract(5, 5, 0.0), gnmix(5, 0), gap(5, 0.0), thick(5, 0.0), scon(5, 0.0), tir(10, 0.0), emis(10, 0.0), rir(10, 0.0),
          AbsRadGlassFace(10, 0.0), thetas(10, 0.0), thetasPrev(10, 0.0), fvec(10, 0.0), fjac(10, 10, 0.0), dtheta(5, 0.0), zir(10, 10, 0.0),
          ziri(10, 10, 0.0), ddeldt(10, 10, 0.0), dtddel(10, 10, 0.0), qf(10, 0.0), hf(10, 0.0), der(5, 10, 0.0), dhf(5, 10, 0.0), sour(10, 0.0),
          delta(5, 0.0), hcgap(5, 0.0), hrgap(5, 0.0), rgap(6, 0.0), rs(6, 0.0), arhs(6, 0.0), MaxNumOfIncidentAngles(20),
          MaxSpectralDataElements(800)
    {
        AirProps.allocate(8);
        AirProps = {1.29, -0.4e-2, 2.41e-2, 7.6e-5, 1.73e-5, 1.0e-7, 0.72, 1.8e-3};
        wle.allocate(nume);
        wle = {0.3000, 0.3050, 0.3100, 0.3150, 0.3200, 0.3250, 0.3300, 0.3350, 0.3400, 0.3450, 0.3500, 0.3600, 0.3700, 0.3800, 0.3900, 0.4000,
               0.4100, 0.4200, 0.4300, 0.4400, 0.4500, 0.4600, 0.4700, 0.4800, 0.4900, 0.5000, 0.5100, 0.5200, 0.5300, 0.5400, 0.5500, 0.5700,
               0.5900, 0.6100, 0.6300, 0.6500, 0.6700, 0.6900, 0.7100, 0.7180, 0.7244, 0.7400, 0.7525, 0.7575, 0.7625, 0.7675, 0.7800, 0.8000,
               0.8160, 0.8237, 0.8315, 0.8400, 0.8600, 0.8800, 0.9050, 0.9150, 0.9250, 0.9300, 0.9370, 0.9480, 0.9650, 0.9800, 0.9935, 1.0400,
               1.0700, 1.1000, 1.1200, 1.1300, 1.1370, 1.1610, 1.1800, 1.2000, 1.2350, 1.2900, 1.3200, 1.3500, 1.3950, 1.4425, 1.4625, 1.4770,
               1.4970, 1.5200, 1.5390, 1.5580, 1.5780, 1.5920, 1.6100, 1.6300, 1.6460, 1.6780, 1.7400, 1.8000, 1.8600, 1.9200, 1.9600, 1.9850,
               2.0050, 2.0350, 2.0650, 2.1000, 2.1480, 2.1980, 2.2700, 2.3600, 2.4500, 2.4940, 2.5370};
        e.allocate(nume);
        e = {0.0,    9.5,    42.3,   107.8,  181.0,  246.0,  395.3,  390.1,  435.3,  438.9,  483.7,  520.3,  666.2,  712.5,  720.7,  1013.1,
             1158.2, 1184.0, 1071.9, 1302.0, 1526.0, 1599.6, 1581.0, 1628.3, 1539.2, 1548.7, 1586.5, 1484.9, 1572.4, 1550.7, 1561.5, 1501.5,
             1395.5, 1485.3, 1434.1, 1419.9, 1392.3, 1130.0, 1316.7, 1010.3, 1043.2, 1211.2, 1193.9, 1175.5, 643.1,  1030.7, 1131.1, 1081.6,
             849.2,  785.0,  916.4,  959.9,  978.9,  933.2,  748.5,  667.5,  690.3,  403.6,  258.3,  313.6,  526.8,  646.4,  746.8,  690.5,
             637.5,  412.6,  108.9,  189.1,  132.2,  339.0,  460.0,  423.6,  480.5,  413.1,  250.2,  32.5,   1.6,    55.7,   105.1,  105.5,
             182.1,  262.2,  274.2,  275.0,  244.6,  247.4,  228.7,  244.5,  234.8,  220.5,  171.5,  30.7,   2.0,    1.2,    21.2,   91.1,
             26.8,   99.5,   60.4,   89.1,   82.2,   71.5,   70.2,   62.0,   21.2,   18.5,   3.2};
        wlt3.allocate(numt3);
        wlt3 = {0.380, 0.385, 0.390, 0.395, 0.400, 0.405, 0.410, 0.415, 0.420, 0.425, 0.430, 0.435, 0.440, 0.445, 0.450, 0.455, 0.460,
                0.465, 0.470, 0.475, 0.480, 0.485, 0.490, 0.495, 0.500, 0.505, 0.510, 0.515, 0.520, 0.525, 0.530, 0.535, 0.540, 0.545,
                0.550, 0.555, 0.560, 0.565, 0.570, 0.575, 0.580, 0.585, 0.590, 0.595, 0.600, 0.605, 0.610, 0.615, 0.620, 0.625, 0.630,
                0.635, 0.640, 0.645, 0.650, 0.655, 0.660, 0.665, 0.670, 0.675, 0.680, 0.685, 0.690, 0.695, 0.700, 0.705, 0.710, 0.715,
                0.720, 0.725, 0.730, 0.735, 0.740, 0.745, 0.750, 0.755, 0.760, 0.765, 0.770, 0.775, 0.780};
        y30.allocate(numt3);
        y30 = {0.0000, 0.0001, 0.0001, 0.0002, 0.0004, 0.0006, 0.0012, 0.0022, 0.0040, 0.0073, 0.0116, 0.0168, 0.0230, 0.0298, 0.0380, 0.0480, 0.0600,
               0.0739, 0.0910, 0.1126, 0.1390, 0.1693, 0.2080, 0.2586, 0.3230, 0.4073, 0.5030, 0.6082, 0.7100, 0.7932, 0.8620, 0.9149, 0.9540, 0.9803,
               0.9950, 1.0000, 0.9950, 0.9786, 0.9520, 0.9154, 0.8700, 0.8163, 0.7570, 0.6949, 0.6310, 0.5668, 0.5030, 0.4412, 0.3810, 0.3210, 0.2650,
               0.2170, 0.1750, 0.1382, 0.1070, 0.0816, 0.0610, 0.0446, 0.0320, 0.0232, 0.0170, 0.0119, 0.0082, 0.0158, 0.0041, 0.0029, 0.0021, 0.0015,
               0.0010, 0.0007, 0.0005, 0.0004, 0.0002, 0.0002, 0.0001, 0.0001, 0.0001, 0.0000, 0.0000, 0.0000, 0.0000};

        wlt.allocate(5, MaxSpectralDataElements);
        wlt = 0.0;

        t.allocate(5, MaxSpectralDataElements);
        t = (0.0); // normal transmittance
        rff.allocate(5, MaxSpectralDataElements);
        rff = (0.0); // normal front reflectance
        rbb.allocate(5, MaxSpectralDataElements);
        rbb = (0.0); // normal back reflectance
        tPhi.allocate(5, MaxSpectralDataElements);
        tPhi = (0.0); // transmittance at angle of incidence
        rfPhi.allocate(5, MaxSpectralDataElements);
        rfPhi = (0.0); // front reflectance at angle of incidence
        rbPhi.allocate(5, MaxSpectralDataElements);
        rbPhi = (0.0); // back reflectance at angle of incidence
        tadjPhi.allocate(5, MaxSpectralDataElements);
        tadjPhi = (0.0); // transmittance at angle of incidence
        rfadjPhi.allocate(5, MaxSpectralDataElements);
        rfadjPhi = (0.0); // front reflectance at angle of incidence
        rbadjPhi.allocate(5, MaxSpectralDataElements);
        rbadjPhi = (0.0); // back reflectance at angle of incidence

        numpt.allocate(5); // Number of spectral data wavelengths for each layer; =2 if no spectra data for a layer
        numpt = 0;
        stPhi.allocate(nume);
        stPhi = 0.0; // Glazing system transmittance at angle of incidence for each wavelength in wle
        srfPhi.allocate(nume);
        srfPhi = 0.0; // Glazing system front reflectance at angle of incidence for each wavelength in wle
        srbPhi.allocate(nume);
        srbPhi = 0.0;            // Glazing system back reflectance at angle of incidence for each wavelenth in wle
        saPhi.allocate(5, nume); // For each layer, glazing system absorptance at angle of incidence
        saPhi = 0.0;             // for each wavelenth in wle
        top.allocate(5, 5);      // Transmittance matrix for subr. op
        top = 0.0;
        rfop.allocate(5, 5); // Front reflectance matrix for subr. op
        rfop = 0.0;
        rbop.allocate(5, 5); // Back transmittance matrix for subr. op
        rbop = 0.0;
        DepVarCurveFit.allocate(MaxNumOfIncidentAngles); // Values of dependent variable corresponding to IndepVarCurveFit values
        DepVarCurveFit = 0.0;
        CoeffsCurveFit.allocate(6); // Polynomial coefficients from curve fit
        CoeffsCurveFit = 0.0;
        tsolPhi.allocate(MaxNumOfIncidentAngles); // Glazing system solar transmittance for each angle of incidence
        tsolPhi = 0.0;
        rfsolPhi.allocate(MaxNumOfIncidentAngles); // Glazing system solar front reflectance for each angle of incidence
        rfsolPhi = 0.0;
        rbsolPhi.allocate(MaxNumOfIncidentAngles); // Glazing system solar back reflectance for each angle of incidence
        rbsolPhi = 0.0;
        solabsPhi.allocate(5, MaxNumOfIncidentAngles); // Glazing system solar absorptance for each angle of incidence
        solabsPhi = 0.0;
        solabsBackPhi.allocate(5, MaxNumOfIncidentAngles); // Glazing system back solar absorptance for each angle of incidence
        solabsBackPhi = 0.0;
        solabsShadePhi.allocate(MaxNumOfIncidentAngles); // Glazing system interior shade solar absorptance for each angle of incidence
        solabsShadePhi = 0.0;
        tvisPhi.allocate(MaxNumOfIncidentAngles); // Glazing system visible transmittance for each angle of incidence
        tvisPhi = 0.0;
        rfvisPhi.allocate(MaxNumOfIncidentAngles); // Glazing system visible front reflectance for each angle of incidence
        rfvisPhi = 0.0;
        rbvisPhi.allocate(MaxNumOfIncidentAngles); // Glazing system visible back reflectance for each angle of incidence
        rbvisPhi = 0.0;
        CosPhiIndepVar.allocate(MaxNumOfIncidentAngles); // Cos of incidence angles at 10-deg increments for curve fits
        CosPhiIndepVar = 0.0;
        LayerNum.allocate(5); // Glass layer number
        LayerNum = 0;
        SimpleGlazingSHGC = 0.0;
        SimpleGlazingU = 0.0;
        tmpReflectSolBeamFront = 0.0;
        tmpReflectSolBeamBack = 0.0;
        tmpReflectVisBeamFront = 0.0;
        tmpReflectVisBeamBack = 0.0;
    }
};

} // namespace EnergyPlus

#endif
