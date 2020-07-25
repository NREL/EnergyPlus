// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

#ifndef WindowComplexManager_hh_INCLUDED
#define WindowComplexManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataBSDFWindow.hh>
#include <EnergyPlus/DataVectorTypes.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

    //forward declaration
    struct EnergyPlusData;
    struct WindowComplexManagerData;

namespace WindowComplexManager {

    // Using/Aliasing
    using DataBSDFWindow::BasisElemDescr;
    using DataBSDFWindow::BasisStruct;
    using DataBSDFWindow::BSDFDaylghtPosition;
    using DataBSDFWindow::BSDFGeomDescr;
    using DataBSDFWindow::BSDFStateDescr;
    using DataBSDFWindow::BSDFWindowGeomDescr;
    using DataBSDFWindow::BSDFWindowInputStruct;
    using DataVectorTypes::Vector;

    struct WindowIndex
    {
        // Members
        int NumStates; // No States for this window
        int SurfNo;    // Surface number of window
        // Real64 Azimuth; // Window surface azimuth
        // Real64 Tilt; // Window surface tilt

        // Default Constructor
        WindowIndex() : NumStates(0)
        {
        }
    };

    struct WindowStateIndex
    {
        // Members
        int InitInc;      // Flag indicating initialization needed on Incoming basis
        int IncBasisIndx; // Index of basis list entry for Incoming basis
        int CopyIncState; // Pointer to state from which geometry can be copied (Incident)
        int InitTrn;      // Flag indicating initialization needed on Outgoing basis
        int TrnBasisIndx; // Index of basis list entry for Outgoing basis
        int CopyTrnState; // Pointer to state from which geometry can be copied (Outgoing)
        int Konst;        // Index of state descript in Construct array
        // INTEGER  ::  ThermConst  !Index of state thermal description in Construct array

        // Default Constructor
        WindowStateIndex()
        {
        }
    };

    // Functions

    //void clear_state();

    void InitBSDFWindows(WindowComplexManagerData &dataWindowComplexManager);

    void AllocateCFSStateHourlyData(int const iSurf, // Surface number
                                    int const iState // Complex fenestration state number
    );

    void ExpandComplexState(WindowComplexManagerData &dataWindowComplexManager, int const iSurf, // Surface number
                            int const iConst // Construction number
    );

    void CheckCFSStates(WindowComplexManagerData &dataWindowComplexManager, int const iSurf); // Surface number

    void InitComplexWindows(WindowComplexManagerData &dataWindowComplexManager);

    void UpdateComplexWindows(WindowComplexManagerData &dataWindowComplexManager);

    void CFSShadeAndBeamInitialization(WindowComplexManagerData &dataWindowComplexManager, int const iSurf, // Window surface number
                                       int const iState // Window state number
    );

    void CalculateWindowBeamProperties(WindowComplexManagerData &dataWindowComplexManager, int const ISurf,                   // Window surface number
                                       int const IState,                  // Window state number
                                       BSDFWindowGeomDescr const &Window, // Window Geometry
                                       BSDFGeomDescr const &Geom,         // State Geometry
                                       BSDFStateDescr &State,             // State Description
                                       int const Hour,                    // Hour number
                                       int const TS                       // Timestep number
    );

    void CalcStaticProperties(WindowComplexManagerData &dataWindowComplexManager);

    void CalculateBasisLength(BSDFWindowInputStruct const &Input, // BSDF data input struct for this construction
                              int const IConst,                   // Construction number of input
                              int &NBasis                         // Calculated Basis length
    );

    void DetermineMaxBackSurfaces();

    void ConstructBasis(int const IConst, // Index for accessing Construct array
                        BasisStruct &Basis);

    void FillBasisElement(Real64 const Theta, // Central polar angle of element
                          Real64 const Phi,   // Central azimuthal angle of element
                          int const Elem,     // Index number of element in basis
                          BasisElemDescr &BasisElem,
                          Real64 const LowerTheta, // Lower edge of element (polar angle)
                          Real64 const UpperTheta, // Upper edge of element (polar angle)
                          Real64 const DPhi,       // Width of element (azimuthal angle)
                          int const InputType      // Basis type
    );

    void SetupComplexWindowStateGeometry(WindowComplexManagerData &dataWindowComplexManager, int const ISurf,             // Surface number of the complex fenestration
                                         int const IState,            // State number of the complex fenestration state
                                         int const IConst,            // Pointer to construction for this state
                                         BSDFWindowGeomDescr &Window, // Window Geometry
                                         BSDFGeomDescr &Geom,         // State Geometry
                                         BSDFStateDescr &State        // State Description
    );

    void CalcWindowStaticProperties(int const ISurf,             // Surface number of the complex fenestration
                                    int const IState,            // State number of the complex fenestration state
                                    BSDFWindowGeomDescr &Window, // Window Geometry
                                    BSDFGeomDescr &Geom,         // State Geometry
                                    BSDFStateDescr &State        // State Description
    );

    Real64 SkyWeight(Vector const &DirVec); // Direction of the element to be weighted

    Real64 SkyGndWeight(Vector const &PosVec); // x,y,z(=0) of ground intersection pt

    BSDFDaylghtPosition DaylghtAltAndAzimuth(Vector const &UnitVect); // vector which needs to be converted

    Vector WorldVectFromW6(WindowComplexManagerData &dataWindowComplexManager, Real64 const Theta, // Polar angle in W6 Coords
                           Real64 const Phi,   // Azimuthal angle in W6 Coords
                           int const RadType,  // Type of radiation: Front_Incident, etc.
                           Real64 const Gamma, // Surface tilt angle, radians, world coordinate system
                           Real64 const Alpha  // Surface azimuth, radians, world coordinate system
    );

    int FindInBasis(WindowComplexManagerData &dataWindowComplexManager, Vector const &RayToFind,  // Ray vector direction in world CS
                    int const RadType,        // Type of radiation: Front_Incident, etc.
                    int const ISurf,          // Window Surface number
                    int const IState,         // Complex Fenestration state number
                    BasisStruct const &Basis, // Complex Fenestration basis root
                    Real64 &Theta,            // Theta value for ray
                    Real64 &Phi               // Phi value for ray
    );

    void W6CoordsFromWorldVect(WindowComplexManagerData &dataWindowComplexManager, Vector const &RayVect, // Ray vector direction in world CS
                               int const RadType,     // Type of radiation: Front_Incident, etc.
                               Real64 const Gamma,    // Surface tilt angle, world coordinate system
                               Real64 const Alpha,    // Surface azimuth, world coordinate system
                               Real64 &Theta,         // Polar angle in W6 Coords
                               Real64 &Phi            // Azimuthal angle in W6 Coords
    );

    void CalcComplexWindowThermal(WindowComplexManagerData &dataWindowComplexManager, int const SurfNum,          // Surface number
                                  int &ConstrNum,             // Construction number
                                  Real64 const HextConvCoeff, // Outside air film conductance coefficient
                                  Real64 &SurfInsideTemp,     // Inside window surface temperature
                                  Real64 &SurfOutsideTemp,    // Outside surface temperature (C)
                                  Real64 &SurfOutsideEmiss,
                                  int const CalcCondition // Calucation condition (summer, winter or no condition)
    );

    // This function check if gas with molecular weight has already been feed into coefficients and
    // feed arrays

    void CheckGasCoefs(Real64 const currentWeight, int &indexNumber, Array1D<Real64> &wght, bool &feedData);

    int SearchAscTable(Real64 const y,            // Value to be found in the table
                       int const n,               // Number of values in the table
                       Array1S<Real64> const ytab // Table of values, monotonic, ascending order
    );

    //=================================================================================================

} // namespace WindowComplexManager

    struct WindowComplexManagerData : BaseGlobalStruct {

        Real64 const sigma; // Stefan-Boltzmann constant
        Real64 const PressureDefault;

        int const Calculate_Geometry;
        int const Copy_Geometry;

        int const TmpLen; // Length increment of temporary arrays

        int const Front_Incident; // Ray identification types
        int const Front_Transmitted;
        int const Front_Reflected;
        int const Back_Incident;
        int const Back_Transmitted;
        int const Back_Reflected;

        int NumComplexWind; // Total number of complex windows

        Array1D<DataBSDFWindow::BasisStruct> BasisList;
        Array1D<WindowComplexManager::WindowIndex> WindowList;
        Array2D<WindowComplexManager::WindowStateIndex> WindowStateList;

        bool InitComplexWindowsOnce = true; // Flag for insuring things happen once
        bool InitBSDFWindowsOnce = true;
        bool resetAbunchOfStuff = true;

        void clear_state() //override
        {
            NumComplexWind = 0;
            BasisList.deallocate();
            WindowList.deallocate();
            WindowStateList.deallocate();
            InitComplexWindowsOnce = true;
            InitBSDFWindowsOnce = true;
            resetAbunchOfStuff = true;
        }

        // Default Constructor
        WindowComplexManagerData()
            : sigma(5.6697e-8), PressureDefault(101325.0), Calculate_Geometry(1), Copy_Geometry(2),
            TmpLen(20), Front_Incident(1), Front_Transmitted(2), Front_Reflected(3), Back_Incident(4),
            Back_Transmitted(5), Back_Reflected(6), NumComplexWind(0)
        {
        }

    };

} // namespace EnergyPlus

#endif
