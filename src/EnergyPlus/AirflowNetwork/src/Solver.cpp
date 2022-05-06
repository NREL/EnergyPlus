// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

#include "AirflowNetwork/Elements.hpp"
#include "AirflowNetwork/Properties.hpp"
#include "AirflowNetwork/Solver.hpp"

#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>

#include "../../Data/EnergyPlusData.hh"

namespace EnergyPlus {

// define this variable to get new code, commenting should yield original
#define SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS

namespace AirflowNetwork {

    // MODULE INFORMATION:
    //       AUTHOR         Lixing Gu, Don Shirey, and Muthusamy V. Swami
    //       DATE WRITTEN   Jul. 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module is used to simulate airflows and pressures. The module is modified to
    // meet requirements of EnergyPLus based on AIRNET, developed by
    // National Institute of Standards and Technology (NIST).

    // METHODOLOGY EMPLOYED:
    // An airflow network approach is used. It consists of nodes connected by airflow elements.
    // The Newton's method is applied to solve a sparse matrix. When a new solver is available, this
    // module will be replaced or updated.

    // REFERENCES:
    // Walton, G. N., 1989, "AIRNET - A Computer Program for Building Airflow Network Modeling,"
    // NISTIR 89-4072, National Institute of Standards and Technology, Gaithersburg, Maryland

    // Functions

    void DetailedOpeningSolver::presprofile(EnergyPlusData &state,
                     int const il,                  // Linkage number
                     int const Pprof,               // Opening number
                     Real64 const G,                // gravitation field strength [N/kg]
                     const Array1D<Real64> &DpF,    // Stack pressures at start heights of Layers
                     const Array1D<Real64> &DpT,    // Stack pressures at start heights of Layers
                     const Array1D<Real64> &BetaF,  // Density gradients in the FROM zone (starting at linkheight) [Kg/m3/m]
                     const Array1D<Real64> &BetaT,  // Density gradients in the TO zone (starting at linkheight) [Kg/m3/m]
                     const Array1D<Real64> &RhoStF, // Density at the start heights of Layers in the FROM zone
                     const Array1D<Real64> &RhoStT, // Density at the start heights of Layers in the TO zone
                     int const From,                // Number of FROM zone
                     int const To,                  // Number of To zone
                     Real64 const ActLh,            // Actual height of opening [m]
                     Real64 const OwnHeightFactor   // Cosine of deviation angle of the opening plane from the vertical direction
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  This subroutine is revised based on PresProfile subroutine from COMIS

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates for a large opening profiles of stack pressure difference and
        // densities in the zones linked by the a detailed opening cmponent.

        // METHODOLOGY EMPLOYED:
        // The profiles are obtained in the following
        // way:    - the opening is divided into NrInt vertical intervals
        //         - the stack pressure difference and densities in From-
        //           and To-zone are calculated at the centre of each
        //           interval aswell as at the top and bottom of the LO
        //          - these values are stored in the (NrInt+2)-dimensional
        //             arrays DpProf, RhoProfF, RhoProfT.
        // The calculation of stack pressure and density in the two zones
        // is based on the arrays DpF/T, RhoStF/T, BetaF/T. These arrays
        // are calculated in the COMIS routine Lclimb. They contain the
        // values of stack pressure and density at the startheight of the
        // opening and at startheights of all layers lying inside the
        // opening, and the density gradients across the layers.
        // The effective startheight zl(1/2) in the From/To zone and the
        // effective length actLh of the LO take into account the
        // startheightfactor, heightfactor and ownheightfactor. Thus for
        // slanted windows the range of the profiles is the vertical
        // projection of the actual opening.

        // REFERENCES:
        // Helmut E. Feustel and Alison Rayner-Hooson, "COMIS Fundamentals," LBL-28560,
        // Lawrence Berkeley National Laboratory, Berkeley, CA, May 1990

        // USE STATEMENTS:
        // na

        // Argument array dimensioning
        EP_SIZE_CHECK(DpF, 2);
        EP_SIZE_CHECK(DpT, 2);
        EP_SIZE_CHECK(BetaF, 2);
        EP_SIZE_CHECK(BetaT, 2);
        EP_SIZE_CHECK(RhoStF, 2);
        EP_SIZE_CHECK(RhoStT, 2);

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // in the FROM zone (starting at linkheight) [Pa]
        // (starting at linkheight) [Kg/m3]
        // (starting at linkheight) [Kg/m3]

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D<Real64> zF(2); // Startheights of layers in FROM-, TO-zone
        Array1D<Real64> zT(2);
        Array1D<Real64> zStF(2); // Startheights of layers within the LO, starting with the actual startheight of the LO.
        Array1D<Real64> zStT(2);
        // The values in the arrays DpF, DpT, BetaF, BetaT, RhoStF, RhoStT are calculated at these heights.
        Real64 hghtsFR;
        Real64 hghtsTR;
        Array1D<Real64> hghtsF(NrInt + 2); // Heights of evaluation points for pressure and density profiles
        Array1D<Real64> hghtsT(NrInt + 2);
        Real64 Interval; // Distance between two evaluation points
        Real64 delzF;    // Interval between actual evaluation point and startheight of actual layer in FROM-, TO-zone
        Real64 delzT;
        int AnzLayF; // Number of layers in FROM-, TO-zone
        int AnzLayT;
        int lF; // Actual index for DpF/T, BetaF/T, RhoStF/T, zStF/T
        int lT;
        int n;
        int i;
        int k;

        // Initialization
        delzF = 0.0;
        delzT = 0.0;
        Interval = ActLh * OwnHeightFactor / NrInt;

        for (n = 1; n <= NrInt; ++n) {
            hghtsF(n + 1) = state.dataAirflowNetwork->AirflowNetworkLinkageData(il).NodeHeights[0] + Interval * (n - 0.5);
            hghtsT(n + 1) = state.dataAirflowNetwork->AirflowNetworkLinkageData(il).NodeHeights[1] + Interval * (n - 0.5);
        }
        hghtsF(1) = state.dataAirflowNetwork->AirflowNetworkLinkageData(il).NodeHeights[0];
        hghtsT(1) = state.dataAirflowNetwork->AirflowNetworkLinkageData(il).NodeHeights[1];
        hghtsF(NrInt + 2) = state.dataAirflowNetwork->AirflowNetworkLinkageData(il).NodeHeights[0] + ActLh * OwnHeightFactor;
        hghtsT(NrInt + 2) = state.dataAirflowNetwork->AirflowNetworkLinkageData(il).NodeHeights[1] + ActLh * OwnHeightFactor;

        lF = 1;
        lT = 1;
        if (From == 0) {
            AnzLayF = 1;
        } else {
            AnzLayF = 0;
        }
        if (To == 0) {
            AnzLayT = 1;
        } else {
            AnzLayT = 0;
        }

        if (AnzLayF > 0) {
            for (n = 1; n <= AnzLayF; ++n) {
                zF(n) = 0.0;
                if (hghtsF(1) < 0.0) zF(n) = hghtsF(1);
            }
        }

        if (AnzLayT > 0) {
            for (n = 1; n <= AnzLayT; ++n) {
                zT(n) = 0.0;
                if (hghtsT(1) < 0.0) zT(n) = hghtsT(1);
            }
        }

        zStF(1) = state.dataAirflowNetwork->AirflowNetworkLinkageData(il).NodeHeights[0];
        i = 2;
        k = 1;

        while (k <= AnzLayF) {
            if (zF(k) > zStF(1)) break;
            ++k;
        }

        while (k <= AnzLayF) {
            if (zF(k) > hghtsF(NrInt)) break;
            zStF(i) = zF(k); // Autodesk:BoundsViolation zStF(i) @ i>2 and zF(k) @ k>2
            ++i;
            ++k;
        }

        zStF(i) = state.dataAirflowNetwork->AirflowNetworkLinkageData(il).NodeHeights[0] +
                  ActLh * OwnHeightFactor; // Autodesk:BoundsViolation zStF(i) @ i>2
        zStT(1) = state.dataAirflowNetwork->AirflowNetworkLinkageData(il).NodeHeights[1];
        i = 2;
        k = 1;

        while (k <= AnzLayT) {
            if (zT(k) > zStT(1)) break;
            ++k;
        }

        while (k <= AnzLayT) {
            if (zT(k) > hghtsT(NrInt)) break; // Autodesk:BoundsViolation zT(k) @ k>2
            zStT(i) = zT(k);                  // Autodesk:BoundsViolation zStF(i) @ i>2 and zT(k) @ k>2
            ++i;
            ++k;
        }

        zStT(i) = state.dataAirflowNetwork->AirflowNetworkLinkageData(il).NodeHeights[1] +
                  ActLh * OwnHeightFactor; // Autodesk:BoundsViolation zStT(i) @ i>2

        // Calculation of DpProf, RhoProfF, RhoProfT
        for (i = 1; i <= NrInt + 2; ++i) {
            hghtsFR = hghtsF(i);
            hghtsTR = hghtsT(i);

            while (true) {
                if (hghtsFR > zStF(lF + 1)) {
                    if (lF > 2) break;
                    ++lF;
                }
                if (hghtsFR <= zStF(lF + 1)) break;
            }

            while (true) {
                if (hghtsTR > zStT(lT + 1)) {
                    ++lT;
                }
                if (hghtsTR <= zStT(lT + 1)) break;
            }

            delzF = hghtsF(i) - zStF(lF);
            delzT = hghtsT(i) - zStT(lT);

            RhoProfF(i + Pprof) = RhoStF(lF) + BetaF(lF) * delzF;
            RhoProfT(i + Pprof) = RhoStT(lT) + BetaT(lT) * delzT;

            DpProf(i + Pprof) = DpF(lF) - DpT(lT) - G * (RhoStF(lF) * delzF + BetaF(lF) * pow_2(delzF) / 2.0) +
                                G * (RhoStT(lT) * delzT + BetaT(lT) * pow_2(delzT) / 2.0);
        }
    }

    void DetailedOpeningSolver::pstack(EnergyPlusData &state,
                                       std::vector<AirflowNetwork::AirProperties> &props,
                                       Array1D<Real64> &pz)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  This subroutine is revised based on PresProfile subroutine from COMIS

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the stack pressures for a link between two zones

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // Helmut E. Feustel and Alison Rayner-Hooson, "COMIS Fundamentals," LBL-28560,
        // Lawrence Berkeley National Laboratory, Berkeley, CA, May 1990

        // USE STATEMENTS:
        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 constexpr PSea(101325.0);

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        //      REAL(r64) RhoOut ! air density outside [kg/m3]
        Real64 G;     // gravity field strength [N/kg]
        Real64 RhoL1; // Air density [kg/m3]
        Real64 RhoL2;
        Real64 Pbz;                                                               // Pbarom at entrance level [Pa]
        Array2D<Real64> RhoDrL(state.dataAirflowNetwork->NumOfLinksMultiZone, 2); // dry air density on both sides of the link [kg/m3]
        Real64 TempL1;                                                            // Temp in From and To zone at link level [C]
        Real64 TempL2;
        //      REAL(r64) Tout ! outside temperature [C]
        Real64 Xhl1; // Humidity in From and To zone at link level [kg/kg]
        Real64 Xhl2;
        //      REAL(r64) Xhout ! outside humidity [kg/kg]
        Array1D<Real64> Hfl(state.dataAirflowNetwork->NumOfLinksMultiZone); // Own height factor for large (slanted) openings
        int Nl;                                                             // number of links

        Array1D<Real64> DpF(2);
        Real64 DpP;
        Array1D<Real64> DpT(2);
        Real64 H;
        Array1D<Real64> RhoStF(2);
        Array1D<Real64> RhoStT(2);
        Real64 RhoDrDummi;
        Array1D<Real64> BetaStF(2);
        Array1D<Real64> BetaStT(2);
        Real64 T;
        Real64 X;
        Array1D<Real64> HSt(2);
        Real64 TzFrom;
        Real64 XhzFrom;
        Real64 TzTo;
        Real64 XhzTo;
        Real64 ActLh;
        Real64 ActLOwnh;
        Real64 Pref;
        Real64 PzFrom;
        Real64 PzTo;
        Array1D<Real64> RhoLd(2);
        Real64 RhoStd;
        int From;
        int To;
        int Fromz;
        int Toz;
        iComponentTypeNum Ltyp;
        int i;
        int ll;
        int j;
        int k;
        int Pprof;
        int ilayptr;
        int OpenNum;

        Real64 RhoREF;
        Real64 CONV;

        RhoREF = AIRDENSITY(state, PSea, state.dataEnvrn->OutDryBulbTemp, state.dataEnvrn->OutHumRat);

        CONV = state.dataEnvrn->Latitude * 2.0 * DataGlobalConstants::Pi / 360.0;
        G = 9.780373 * (1.0 + 0.0052891 * pow_2(std::sin(CONV)) - 0.0000059 * pow_2(std::sin(2.0 * CONV)));

        Hfl = 1.0;
        Pbz = state.dataEnvrn->OutBaroPress;
        Nl = state.dataAirflowNetwork->NumOfLinksMultiZone;
        OpenNum = 0;
        RhoLd(1) = 1.2;
        RhoLd(2) = 1.2;
        RhoStd = 1.2;

        for (i = 1; i <= Nl; ++i) {
            // Check surface tilt
            if (i <= Nl - state.dataAirflowNetwork->NumOfLinksIntraZone) { // Revised by L.Gu, on 9 / 29 / 10
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).DetOpenNum > 0 &&
                    state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Tilt < 90) {
                    Hfl(i) = state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).SinTilt;
                }
            }
            // Initialisation
            From = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
            To = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(From).EPlusZoneNum > 0 &&
                state.dataAirflowNetwork->AirflowNetworkNodeData(To).EPlusZoneNum > 0) {
                ll = 0;
            } else if (state.dataAirflowNetwork->AirflowNetworkNodeData(From).EPlusZoneNum == 0 &&
                       state.dataAirflowNetwork->AirflowNetworkNodeData(To).EPlusZoneNum > 0) {
                ll = 1;
            } else {
                ll = 3;
            }

            Ltyp = state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum;
            if (Ltyp == iComponentTypeNum::DOP) {
                ActLh = state.dataAirflowNetwork->MultizoneSurfaceData(i).Height;
                ActLOwnh = ActLh * 1.0;
            } else {
                ActLh = 0.0;
                ActLOwnh = 0.0;
            }

            TempL1 = props[From].temperature;
            Xhl1 = props[From].humidity_ratio;
            TzFrom = props[From].temperature;
            XhzFrom = props[From].humidity_ratio;
            RhoL1 = props[From].density;
            if (ll == 0 || ll == 3) {
                PzFrom = pz(From);
            } else {
                PzFrom = 0.0;
                From = 0;
            }

            ilayptr = 0;
            if (From == 0) ilayptr = 1;
            if (ilayptr == 0) {
                Fromz = 0;
            } else {
                Fromz = From;
            }

            TempL2 = props[To].temperature;
            Xhl2 = props[To].humidity_ratio;
            TzTo = props[To].temperature;
            XhzTo = props[To].humidity_ratio;
            RhoL2 = props[To].density;

            if (ll < 3) {
                PzTo = pz(To);
            } else {
                PzTo = 0.0;
                To = 0;
            }
            ilayptr = 0;
            if (To == 0) ilayptr = 1;
            if (ilayptr == 0) {
                Toz = 0;
            } else {
                Toz = To;
            }

            // RhoDrL is Rho at link level without pollutant but with humidity
            RhoDrL(i, 1) = AIRDENSITY(state, state.dataEnvrn->OutBaroPress + PzFrom, TempL1, Xhl1);
            RhoDrL(i, 2) = AIRDENSITY(state, state.dataEnvrn->OutBaroPress + PzTo, TempL2, Xhl2);

            // End initialisation

            // calculate DpF the difference between Pz and P at Node 1 height
            ilayptr = 0;
            if (Fromz == 0) ilayptr = 1;
            j = ilayptr;
            k = 1;
            lclimb(state,
                   G,
                   RhoLd(1),
                   state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[0],
                   TempL1,
                   Xhl1,
                   DpF(k),
                   Toz,
                   PzTo,
                   Pbz,
                   RhoDrL(i, 1));
            RhoL1 = RhoLd(1);
            // For large openings calculate the stack pressure difference profile and the
            // density profile within the the top- and the bottom- height of the large opening
            if (ActLOwnh > 0.0) {
                HSt(k) = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[0];
                RhoStF(k) = RhoL1;
                ++k;
                HSt(k) = 0.0;
                if (HSt(k - 1) < 0.0) HSt(k) = HSt(k - 1);

                // Search for the first startheight of a layer which is within the top- and the
                // bottom- height of the large opening.
                while (true) {
                    ilayptr = 0;
                    if (Fromz == 0) ilayptr = 9;
                    if ((j > ilayptr) || (HSt(k) > state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[0])) break;
                    j += 9;
                    HSt(k) = 0.0;
                    if (HSt(k - 1) < 0.0) HSt(k) = HSt(k - 1);
                }

                // Calculate Rho and stack pressure for every StartHeight of a layer which is
                // within the top- and the bottom-height of the  large opening.
                while (true) {
                    ilayptr = 0;
                    if (Fromz == 0) ilayptr = 9;
                    if ((j > ilayptr) || (HSt(k) >= (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[0] + ActLOwnh)))
                        break; // Autodesk:BoundsViolation HSt(k) @ k>2
                    T = TzFrom;
                    X = XhzFrom;
                    lclimb(
                        state, G, RhoStd, HSt(k), T, X, DpF(k), Fromz, PzFrom, Pbz, RhoDrDummi); // Autodesk:BoundsViolation HSt(k) and DpF(k) @ k>2
                    RhoStF(k) = RhoStd;                                                          // Autodesk:BoundsViolation RhoStF(k) @ k>2
                    j += 9;
                    ++k;                                       // Autodesk:Note k>2 now
                    HSt(k) = 0.0;                              // Autodesk:BoundsViolation @ k>2
                    if (HSt(k - 1) < 0.0) HSt(k) = HSt(k - 1); // Autodesk:BoundsViolation @ k>2
                }
                // Stack pressure difference and rho for top-height of the large opening
                HSt(k) = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[0] + ActLOwnh; // Autodesk:BoundsViolation k>2 poss
                T = TzFrom;
                X = XhzFrom;
                lclimb(state, G, RhoStd, HSt(k), T, X, DpF(k), Fromz, PzFrom, Pbz, RhoDrDummi); // Autodesk:BoundsViolation k>2 poss
                RhoStF(k) = RhoStd;                                                             // Autodesk:BoundsViolation k >= 3 poss

                for (j = 1; j <= (k - 1); ++j) {
                    BetaStF(j) = (RhoStF(j + 1) - RhoStF(j)) / (HSt(j + 1) - HSt(j));
                }
            }

            // repeat procedure for the "To" node, DpT
            ilayptr = 0;
            if (Toz == 0) ilayptr = 1;
            j = ilayptr;
            // Calculate Rho at link height only if we have large openings or layered zones.
            k = 1;
            lclimb(state,
                   G,
                   RhoLd(2),
                   state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[1],
                   TempL2,
                   Xhl2,
                   DpT(k),
                   Toz,
                   PzTo,
                   Pbz,
                   RhoDrL(i, 2));
            RhoL2 = RhoLd(2);

            // For large openings calculate the stack pressure difference profile and the
            // density profile within the the top- and the bottom- height of the large opening
            if (ActLOwnh > 0.0) {
                HSt(k) = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[1];
                RhoStT(k) = RhoL2;
                ++k;
                HSt(k) = 0.0;
                if (HSt(k - 1) < 0.0) HSt(k) = HSt(k - 1);
                while (true) {
                    ilayptr = 0;
                    if (Toz == 0) ilayptr = 9;
                    if ((j > ilayptr) || (HSt(k) > state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[1])) break;
                    j += 9;
                    HSt(k) = 0.0;
                    if (HSt(k - 1) < 0.0) HSt(k) = HSt(k - 1);
                }
                // Calculate Rho and stack pressure for every StartHeight of a layer which is
                // within the top- and the bottom-height of the  large opening.
                while (true) {
                    ilayptr = 0;
                    if (Toz == 0) ilayptr = 9;
                    if ((j > ilayptr) || (HSt(k) >= (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[1] + ActLOwnh)))
                        break; // Autodesk:BoundsViolation Hst(k) @ k>2
                    T = TzTo;
                    X = XhzTo;
                    lclimb(state, G, RhoStd, HSt(k), T, X, DpT(k), Toz, PzTo, Pbz, RhoDrDummi); // Autodesk:BoundsViolation HSt(k) and DpT(k) @ k>2
                    RhoStT(k) = RhoStd;                                                         // Autodesk:BoundsViolation RhoStT(k) @ k>2
                    j += 9;
                    ++k;                                       // Autodesk:Note k>2 now
                    HSt(k) = 0.0;                              // Autodesk:BoundsViolation @ k>2
                    if (HSt(k - 1) < 0.0) HSt(k) = HSt(k - 1); // Autodesk:BoundsViolation @ k>2
                }
                // Stack pressure difference and rho for top-height of the large opening
                HSt(k) = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[1] + ActLOwnh; // Autodesk:BoundsViolation k>2 poss
                T = TzTo;
                X = XhzTo;
                lclimb(state, G, RhoStd, HSt(k), T, X, DpT(k), Toz, PzTo, Pbz, RhoDrDummi); // Autodesk:BoundsViolation k>2 poss
                RhoStT(k) = RhoStd;                                                         // Autodesk:BoundsViolation k>2 poss

                for (j = 1; j <= (k - 1); ++j) {
                    BetaStT(j) = (RhoStT(j + 1) - RhoStT(j)) / (HSt(j + 1) - HSt(j));
                }
            }

            // CALCULATE STACK PRESSURE FOR THE PATH ITSELF for different flow directions
            H = double(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[1]) -
                double(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[0]);
            if (ll == 0 || ll == 3 || ll == 6) {
                H -= state.dataAirflowNetwork->AirflowNetworkNodeData(From).NodeHeight;
            }
            if (ll < 3) {
                H += state.dataAirflowNetwork->AirflowNetworkNodeData(To).NodeHeight;
            }

            // IF AIR FLOWS from "From" to "To"
            Pref = Pbz + PzFrom + DpF(1);
            DpP = psz(Pref, RhoLd(1), 0.0, 0.0, H, G);
            DpL(i, 1) = (DpF(1) - DpT(1) + DpP);

            // IF AIR FLOWS from "To" to "From"
            Pref = Pbz + PzTo + DpT(1);
            DpP = -psz(Pref, RhoLd(2), 0.0, 0.0, -H, G);
            DpL(i, 2) = (DpF(1) - DpT(1) + DpP);

            if (Ltyp == iComponentTypeNum::DOP) {
                Pprof = OpenNum * (NrInt + 2);
                presprofile(state, i, Pprof, G, DpF, DpT, BetaStF, BetaStT, RhoStF, RhoStT, From, To, ActLh, Hfl(i));
                ++OpenNum;
            }
        }
    }

    Real64 DetailedOpeningSolver::psz(Real64 const Pz0,  // Pressure at altitude z0 [Pa]
                                      Real64 const Rho0, // density at altitude z0 [kg/m3]
                                      Real64 const beta, // density gradient [kg/m4]
                                      Real64 const z0,   // reference altitude [m]
                                      Real64 const z,    // altitude[m]
                                      Real64 const g     // gravity field strength [N/kg]
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  This subroutine is revised based on psz function from COMIS

        // PURPOSE OF THIS SUBROUTINE:
        // This function determines the pressure due to buoyancy in a stratified density environment

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 psz;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 dz;
        Real64 rho;

        dz = z - z0;
        rho = (Rho0 + beta * dz / 2.0);
        psz = -Pz0 * (1.0 - std::exp(-dz * rho * g / Pz0)); // Differential pressure from z to z0 [Pa]

        return psz;
    }

    void DetailedOpeningSolver::lclimb(EnergyPlusData &state,
                                       Real64 const G,   // gravity field strength [N/kg]
                                       Real64 &Rho,      // Density link level (initialized with rho zone) [kg/m3]
                                       Real64 const Z,   // Height of the link above the zone reference [m]
                                       Real64 &T,        // temperature at link level [C]
                                       Real64 &X,        // absolute humidity at link level [kg/kg]
                                       Real64 &Dp,       // Stackpressure to the linklevel [Pa]
                                       int const zone,   // Zone number
                                       Real64 const PZ,  // Zone Pressure (reflevel) [Pa]
                                       Real64 const Pbz, // Barometric pressure at entrance level [Pa]
                                       Real64 &RhoDr     // Air density of dry air on the link level used
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  This subroutine is revised based on subroutine IClimb from COMIS

        // PURPOSE OF THIS SUBROUTINE:
        // This function the differential pressure from the reflevel in a zone To Z, the level of a link

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // for the concentration routine [kg/m3]

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 H;        // Start Height of the layer
        Real64 BetaT;    // Temperature gradient of this layer
        Real64 BetaXfct; // Humidity gradient factor of this layer
        Real64 BetaCfct; // Concentration 1 gradient factor of this layer
        Real64 X0;
        Real64 P;
        Real64 Htop;
        Real64 Hbot;
        Real64 Rho0;
        Real64 Rho1;
        Real64 BetaRho;
        static int L(0);
        static int ilayptr(0);

        Dp = 0.0;
        Rho0 = Rho;
        X0 = X;
        if (Z > 0.0) {
            // initialize start values
            H = 0.0;
            BetaT = 0.0;
            BetaXfct = 0.0;
            BetaCfct = 0.0;
            BetaRho = 0.0;
            Hbot = 0.0;

            while (H < 0.0) {
                // loop until H>0 ; The start of the layer is above 0
                BetaT = 0.0;
                BetaXfct = 0.0;
                BetaCfct = 0.0;
                L += 9;
                ilayptr = 0;
                if (zone == 0) ilayptr = 9;
                if (L >= ilayptr) {
                    H = Z + 1.0;
                } else {
                    H = 0.0;
                }
            }

            // The link is in this layer also if it is on the top of it.

            while (true) {
                if (H >= Z) {
                    // the link ends in this layer , we reached the final Dp and BetaRho
                    Htop = Z;
                    P = PZ + Dp;
                    if (Htop != Hbot) {
                        Rho0 = AIRDENSITY(state, Pbz + P, T, X);
                        T += (Htop - Hbot) * BetaT;
                        X += (Htop - Hbot) * BetaXfct * X0;
                        Rho1 = AIRDENSITY(state, Pbz + P, T, X);
                        BetaRho = (Rho1 - Rho0) / (Htop - Hbot);
                        Dp += psz(Pbz + P, Rho0, BetaRho, Hbot, Htop, G);
                    }
                    RhoDr = AIRDENSITY(state, Pbz + PZ + Dp, T, X);
                    Rho = AIRDENSITY(state, Pbz + PZ + Dp, T, X);
                    return;

                } else {
                    // bottom of the layer is below Z  (Z above ref)
                    Htop = H;
                    // P is the pressure up to the start height of the layer we just reached
                    P = PZ + Dp;
                    if (Htop != Hbot) {
                        Rho0 = AIRDENSITY(state, Pbz + P, T, X);
                        T += (Htop - Hbot) * BetaT;
                        X += (Htop - Hbot) * BetaXfct * X0;
                        Rho1 = AIRDENSITY(state, Pbz + P, T, X);
                        BetaRho = (Rho1 - Rho0) / (Htop - Hbot);
                        Dp += psz(Pbz + P, Rho0, BetaRho, Hbot, Htop, G);
                    }

                    RhoDr = AIRDENSITY(state, Pbz + PZ + Dp, T, X);
                    Rho = AIRDENSITY(state, Pbz + PZ + Dp, T, X);

                    // place current values Hbot and Beta's
                    Hbot = H;
                    BetaT = 0.0;
                    BetaXfct = 0.0;
                    BetaCfct = 0.0;
                    L += 9;
                    ilayptr = 0;
                    if (zone == 0) ilayptr = 9;
                    if (L >= ilayptr) {
                        H = Z + 1.0;
                    } else {
                        H = 0.0;
                    }
                }
            }

        } else {
            // This is the ELSE for negative linkheights Z below the refplane
            H = 0.0;
            BetaT = 0.0;
            BetaXfct = 0.0;
            BetaCfct = 0.0;
            BetaRho = 0.0;
            Htop = 0.0;
            while (H > 0.0) {
                // loop until H<0 ; The start of the layer is below the zone refplane
                L -= 9;
                ilayptr = 0;
                if (zone == 0) ilayptr = 1;
                if (L < ilayptr) {
                    // with H=Z (negative) this loop will exit, no data for interval Z-refplane
                    H = Z;
                    BetaT = 0.0;
                    BetaXfct = 0.0;
                    BetaCfct = 0.0;
                    BetaRho = 0.0;
                } else {
                    H = 0.0;
                    BetaT = 0.0;
                    BetaXfct = 0.0;
                    BetaCfct = 0.0;
                }
            }

            // The link is in this layer also if it is on the bottom of it.
            while (true) {
                if (H <= Z) {
                    Hbot = Z;
                    P = PZ + Dp;
                    if (Htop != Hbot) {
                        Rho1 = AIRDENSITY(state, Pbz + P, T, X);
                        T += (Hbot - Htop) * BetaT;
                        X += (Hbot - Htop) * BetaXfct * X0;
                        Rho0 = AIRDENSITY(state, Pbz + P, T, X);
                        BetaRho = (Rho1 - Rho0) / (Htop - Hbot);
                        Dp -= psz(Pbz + P, Rho0, BetaRho, Hbot, Htop, G);
                    }
                    RhoDr = AIRDENSITY(state, Pbz + PZ + Dp, T, X);
                    Rho = AIRDENSITY(state, Pbz + PZ + Dp, T, X);
                    return;
                } else {
                    // bottom of the layer is below Z  (Z below ref)
                    Hbot = H;
                    P = PZ + Dp;
                    if (Htop != Hbot) {
                        Rho1 = AIRDENSITY(state, Pbz + P, T, X);
                        // T,X,C calculated for the lower height
                        T += (Hbot - Htop) * BetaT;
                        X += (Hbot - Htop) * BetaXfct * X0;
                        Rho0 = AIRDENSITY(state, Pbz + P, T, X);
                        BetaRho = (Rho1 - Rho0) / (Htop - Hbot);
                        Dp -= psz(Pbz + P, Rho0, BetaRho, Hbot, Htop, G);
                    }
                    RhoDr = AIRDENSITY(state, Pbz + PZ + Dp, T, X);
                    Rho = AIRDENSITY(state, Pbz + PZ + Dp, T, X);

                    // place current values Hbot and Beta's
                    Htop = H;
                    L -= 9;
                    ilayptr = 0;
                    if (zone == 0) ilayptr = 1;
                    if (L < ilayptr) {
                        H = Z - 1.0;
                        BetaT = 0.0;
                        BetaXfct = 0.0;
                        BetaCfct = 0.0;
                    } else {
                        H = 0.0;
                        BetaT = 0.0;
                        BetaXfct = 0.0;
                        BetaCfct = 0.0;
                    }
                }
                // ENDIF H<Z
            }
        }
    }

    //*****************************************************************************************

} // namespace AirflowNetwork

} // namespace EnergyPlus
