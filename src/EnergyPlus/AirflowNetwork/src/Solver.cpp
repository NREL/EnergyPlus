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

    AirProperties::AirProperties(double const airDensity)
    {
        this->density = airDensity;
        this->sqrtDensity = sqrt(airDensity);
    }

    void Solver::allocate(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Aug. 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine allocates dynamic arrays for AirflowNetworkSolver.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        iComponentTypeNum j;
        int n;

        // Formats

        // Assume a network to simulate multizone airflow is a subset of the network to simulate air distribution system.
        // Network array size is allocated based on the network of air distribution system.
        // If multizone airflow is simulated only, the array size is allocated based on the multizone network.

        auto &NetworkNumOfLinks = state.dataAFNSolver->NetworkNumOfLinks;
        auto &NetworkNumOfNodes = state.dataAFNSolver->NetworkNumOfNodes;
        auto &DpProf = state.dataAFNSolver->DpProf;
        auto &RhoProfF = state.dataAFNSolver->RhoProfF;
        auto &RhoProfT = state.dataAFNSolver->RhoProfT;
        auto &DpL = state.dataAFNSolver->DpL;

        NetworkNumOfLinks = state.dataAirflowNetwork->AirflowNetworkNumOfLinks;
        NetworkNumOfNodes = state.dataAirflowNetwork->AirflowNetworkNumOfNodes;

        AFECTL.allocate(NetworkNumOfLinks);
        AFLOW2.allocate(NetworkNumOfLinks);
        AFLOW.allocate(NetworkNumOfLinks);
        PW.allocate(NetworkNumOfLinks);
        PS.allocate(NetworkNumOfLinks);

        // TZ.allocate(NetworkNumOfNodes);
        // WZ.allocate(NetworkNumOfNodes);
        PZ.allocate(NetworkNumOfNodes);
        // RHOZ.allocate(NetworkNumOfNodes);
        // SQRTDZ.allocate(NetworkNumOfNodes);
        // VISCZ.allocate(NetworkNumOfNodes);
        SUMAF.allocate(NetworkNumOfNodes);

        for (int it = 0; it <= NetworkNumOfNodes + 1; ++it)
            properties.emplace_back(AIRDENSITY(state, 20.0, 101325.0, 0.0));

        ID.allocate(NetworkNumOfNodes);
        IK.allocate(NetworkNumOfNodes + 1);
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
        newIK.allocate(NetworkNumOfNodes + 1);
#endif
        AD.allocate(NetworkNumOfNodes);
        SUMF.allocate(NetworkNumOfNodes);

        n = 0;
        for (i = 1; i <= state.dataAirflowNetwork->AirflowNetworkNumOfLinks; ++i) {
            j = state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum;
            if (j == iComponentTypeNum::DOP) {
                ++n;
            }
        }

        DpProf.allocate(n * (NrInt + 2));
        RhoProfF.allocate(n * (NrInt + 2));
        RhoProfT.allocate(n * (NrInt + 2));
        DpL.allocate(state.dataAirflowNetwork->AirflowNetworkNumOfLinks, 2);

        PB = 101325.0;
        //   LIST = 5
        // LIST = 0;

        for (n = 1; n <= NetworkNumOfNodes; ++n) {
            ID(n) = n;
        }
        for (i = 1; i <= NetworkNumOfLinks; ++i) {
            AFECTL(i) = 1.0;
            AFLOW(i) = 0.0;
            AFLOW2(i) = 0.0;
        }

        for (i = 1; i <= NetworkNumOfNodes; ++i) {
            // TZ(i) = AirflowNetworkNodeSimu(i).TZ;
            // WZ(i) = AirflowNetworkNodeSimu(i).WZ;
            PZ(i) = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).PZ;
            properties[i].temperature = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZ;
            properties[i].humidityRatio = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZ;
            // properties[i].pressure = AirflowNetworkNodeSimu(i).PZ;
        }

        // Assign linkage values
        for (i = 1; i <= NetworkNumOfLinks; ++i) {
            PW(i) = 0.0;
        }
        // Write an ouput file used for AIRNET input
        /*
        if (LIST >= 5) {
            Unit11 = GetNewUnitNumber();
            ObjexxFCL::gio::open(Unit11, DataStringGlobals::eplusADSFileName);
            for (i = 1; i <= NetworkNumOfNodes; ++i) {
                ObjexxFCL::gio::write(Unit11, Format_901) << i << state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeTypeNum <<
        state.dataAirflowNetwork->AirflowNetworkNodeData(i).NodeHeight << TZ(i)
                                               << PZ(i);
            }
            ObjexxFCL::gio::write(Unit11, Format_900) << 0;
            for (i = 1; i <= AirflowNetworkNumOfComps; ++i) {
                j = state.dataAirflowNetwork->AirflowNetworkCompData(i).TypeNum;
                {
                    auto const SELECT_CASE_var(state.dataAirflowNetwork->AirflowNetworkCompData(i).CompTypeNum);
                    if (SELECT_CASE_var == CompTypeNum_PLR) { //'PLR'  Power law component
                        //              WRITE(Unit11,902)
        state.dataAirflowNetwork->AirflowNetworkCompData(i)%CompNum,1,DisSysCompLeakData(j)%FlowCoef, &
                        //                  DisSysCompLeakData(j)%FlowCoef,DisSysCompLeakData(j)%FlowCoef,DisSysCompLeakData(j)%FlowExpo
                    } else if (SELECT_CASE_var == CompTypeNum_SCR) { //'SCR'  Surface crack component
                        ObjexxFCL::gio::write(Unit11, Format_902) << state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum << 1 <<
        MultizoneSurfaceCrackData(j).FlowCoef
                                                       << MultizoneSurfaceCrackData(j).FlowCoef << MultizoneSurfaceCrackData(j).FlowCoef
                                                       << MultizoneSurfaceCrackData(j).FlowExpo;
                    } else if (SELECT_CASE_var == CompTypeNum_DWC) { //'DWC' Duct component
                        //              WRITE(Unit11,902)
        state.dataAirflowNetwork->AirflowNetworkCompData(i)%CompNum,2,DisSysCompDuctData(j)%L,DisSysCompDuctData(j)%D, &
                        //                               DisSysCompDuctData(j)%A,DisSysCompDuctData(j)%Rough
                        //              WRITE(Unit11,903) DisSysCompDuctData(i)%TurDynCoef,DisSysCompDuctData(j)%LamFriCoef, &
                        //                               DisSysCompDuctData(j)%LamFriCoef,DisSysCompDuctData(j)%InitLamCoef
                        //           CASE (CompTypeNum_CVF) ! 'CVF' Constant volume fan component
                        //              WRITE(Unit11,904) state.dataAirflowNetwork->AirflowNetworkCompData(i)%CompNum,4,DisSysCompCVFData(j)%FlowRate
                    } else if (SELECT_CASE_var == CompTypeNum_EXF) { // 'EXF' Zone exhaust fan
                        ObjexxFCL::gio::write(Unit11, Format_904) << state.dataAirflowNetwork->AirflowNetworkCompData(i).CompNum << 4 <<
        MultizoneCompExhaustFanData(j).FlowRate; } else {
                    }
                }
            }
            ObjexxFCL::gio::write(Unit11, Format_900) << 0;
            for (i = 1; i <= NetworkNumOfLinks; ++i) {
                gio::write(Unit11, Format_910) << i << state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0] <<
        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[0]
                                               << state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1] <<
        state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[1]
                                               << state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum << 0 << 0;
            }
            ObjexxFCL::gio::write(Unit11, Format_900) << 0;
        }
        */
        auto &solver = state.dataAFNSolver->solver;
        solver.setsky(state);

        // SETSKY figures out the IK stuff -- which is why E+ doesn't allocate AU until here
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
        //   ! only printing to screen, can be commented
        //   print*, "SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS is defined"
        //   write(*,'(2(a,i8))') "AllocateAirflowNetworkData: after SETSKY, allocating AU.  NetworkNumOfNodes=", &
        //        NetworkNumOfNodes, " IK(NetworkNumOfNodes+1)= NNZE=", IK(NetworkNumOfNodes+1)
        //   print*, " NetworkNumOfLinks=", NetworkNumOfLinks
        // allocate same size as others -- this will be maximum  !noel
        newAU.allocate(IK(NetworkNumOfNodes + 1));
#endif

        // noel, GNU says the AU is indexed above its upper bound
        // ALLOCATE(AU(IK(NetworkNumOfNodes+1)-1))
        AU.allocate(IK(NetworkNumOfNodes + 1));
    }

    void Solver::initialize(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Aug. 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes variables for AirflowNetworkSolver.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        auto &NetworkNumOfLinks = state.dataAFNSolver->NetworkNumOfLinks;
        auto &NetworkNumOfNodes = state.dataAFNSolver->NetworkNumOfNodes;

        for (int i = 1; i <= NetworkNumOfNodes; ++i) {
            ID(i) = i;
        }
        for (int i = 1; i <= NetworkNumOfLinks; ++i) {
            AFECTL(i) = 1.0;
            AFLOW(i) = 0.0;
            AFLOW2(i) = 0.0;
        }

        for (int i = 1; i <= NetworkNumOfNodes; ++i) {
            // TZ(i) = AirflowNetworkNodeSimu(i).TZ;
            // WZ(i) = AirflowNetworkNodeSimu(i).WZ;
            PZ(i) = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).PZ;
            properties[i].temperature = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).TZ;
            properties[i].humidityRatio = state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).WZ;
            // properties[i].pressure = AirflowNetworkNodeSimu(i).PZ;
        }
    }

    void Solver::setsky(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   1998
        //       MODIFIED       Feb. 2006 (L. Gu) to meet requirements of AirflowNetwork
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine sets up the "IK" array describing the sparse matrix [A] in skyline
        //     form by using the location matrix.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // AIRNET

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // IK(K) - pointer to the top of column/row "K".
        int i;
        int j;
        int k;
        int L;
        int M;
        int N1;
        int N2;
        auto &NetworkNumOfLinks = state.dataAFNSolver->NetworkNumOfLinks;
        auto &NetworkNumOfNodes = state.dataAFNSolver->NetworkNumOfNodes;

        // Initialize "IK".
        for (i = 1; i <= NetworkNumOfNodes + 1; ++i) {
            IK(i) = 0;
        }
        // Determine column heights.
        for (M = 1; M <= NetworkNumOfLinks; ++M) {
            j = state.dataAirflowNetwork->AirflowNetworkLinkageData(M).NodeNums[1];
            if (j == 0) continue;
            L = ID(j);
            i = state.dataAirflowNetwork->AirflowNetworkLinkageData(M).NodeNums[0];
            k = ID(i);
            N1 = std::abs(L - k);
            N2 = max(k, L);
            IK(N2) = max(IK(N2), N1);
        }
        // Convert heights to column addresses.
        j = IK(1);
        IK(1) = 1;
        for (k = 1; k <= NetworkNumOfNodes; ++k) {
            i = IK(k + 1);
            IK(k + 1) = IK(k) + j;
            j = i;
        }
    }

    void Solver::airmov(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   Extracted from AIRNETf
        //       MODIFIED       Lixing Gu, 2/1/04
        //                      Revised the subroutine to meet E+ needs
        //       MODIFIED       Lixing Gu, 6/8/05
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is a driver for AIRNET to calculate nodal pressures and linkage airflows

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;
        int m;
        int n;
        int ITER;

        // Formats

        // static ObjexxFCL::gio::Fmt Format_900("(,/,11X,'i    n    m       DP',12x,'F1',12X,'F2')");
        // static ObjexxFCL::gio::Fmt Format_901("(1X,A6,3I5,3F14.6)");
        // static ObjexxFCL::gio::Fmt Format_902("(,/,11X,'n       P',12x,'sumF')");
        // static ObjexxFCL::gio::Fmt Format_903("(1X,A6,I5,3F14.6)");
        // static ObjexxFCL::gio::Fmt Format_907("(,/,' CPU seconds for ',A,F12.3)");

        auto &NetworkNumOfLinks = state.dataAFNSolver->NetworkNumOfLinks;
        auto &NetworkNumOfNodes = state.dataAFNSolver->NetworkNumOfNodes;

        // Initialize pressure for pressure control and for Initialization Type = LinearInitializationMethod
        if ((state.dataAirflowNetwork->AirflowNetworkSimu.InitFlag == 0) ||
            (state.dataAirflowNetwork->PressureSetFlag > 0 && state.dataAirflowNetwork->AirflowNetworkFanActivated)) {
            for (n = 1; n <= NetworkNumOfNodes; ++n) {
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(n).NodeTypeNum == 0) PZ(n) = 0.0;
            }
        }
        // Compute zone air properties.
        for (n = 1; n <= NetworkNumOfNodes; ++n) {
            properties[n].density = AIRDENSITY(state, state.dataEnvrn->StdBaroPress + PZ(n), properties[n].temperature, properties[n].humidityRatio);
            // RHOZ(n) = PsyRhoAirFnPbTdbW(StdBaroPress + PZ(n), TZ(n), WZ(n));
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(n).ExtNodeNum > 0) {
                properties[n].density =
                    AIRDENSITY(state, state.dataEnvrn->StdBaroPress + PZ(n), state.dataEnvrn->OutDryBulbTemp, state.dataEnvrn->OutHumRat);
                properties[n].temperature = state.dataEnvrn->OutDryBulbTemp;
                properties[n].humidityRatio = state.dataEnvrn->OutHumRat;
            }
            properties[n].sqrtDensity = std::sqrt(properties[n].density);
            properties[n].viscosity = 1.71432e-5 + 4.828e-8 * properties[n].temperature;
            // if (LIST >= 2) ObjexxFCL::gio::write(outputFile, Format_903) << "D,V:" << n << properties[n].density << properties[n].viscosity;
        }
        // Compute stack pressures.
        for (i = 1; i <= NetworkNumOfLinks; ++i) {
            n = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
            m = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
            if (AFLOW(i) > 0.0) {
                PS(i) =
                    9.80 * (properties[n].density * (state.dataAirflowNetwork->AirflowNetworkNodeData(n).NodeHeight -
                                                     state.dataAirflowNetwork->AirflowNetworkNodeData(m).NodeHeight) +
                            state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[1] * (properties[m].density - properties[n].density));
            } else if (AFLOW(i) < 0.0) {
                PS(i) =
                    9.80 * (properties[m].density * (state.dataAirflowNetwork->AirflowNetworkNodeData(n).NodeHeight -
                                                     state.dataAirflowNetwork->AirflowNetworkNodeData(m).NodeHeight) +
                            state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[0] * (properties[m].density - properties[n].density));
            } else {
                PS(i) = 4.90 * ((properties[n].density + properties[m].density) * (state.dataAirflowNetwork->AirflowNetworkNodeData(n).NodeHeight -
                                                                                   state.dataAirflowNetwork->AirflowNetworkNodeData(m).NodeHeight) +
                                (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[0] +
                                 state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[1]) *
                                    (properties[m].density - properties[n].density));
            }
        }

        // Calculate pressure field in a large opening
        PStack(state);
        auto &solver = state.dataAFNSolver->solver;
        solver.solvzp(state, ITER);

        // Report element flows and zone pressures.
        for (n = 1; n <= NetworkNumOfNodes; ++n) {
            SUMAF(n) = 0.0;
        }
        // if (LIST >= 1) ObjexxFCL::gio::write(outputFile, Format_900);
        for (i = 1; i <= NetworkNumOfLinks; ++i) {
            n = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
            m = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
            // if (LIST >= 1) {
            //    gio::write(outputFile, Format_901) << "Flow: " << i << n << m << AirflowNetworkLinkSimu(i).DP << AFLOW(i) << AFLOW2(i);
            //}
            if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum ==
                iComponentTypeNum::HOP) {
                SUMAF(n) = SUMAF(n) - AFLOW(i);
                SUMAF(m) += AFLOW(i);
            } else {
                SUMAF(n) = SUMAF(n) - AFLOW(i) - AFLOW2(i);
                SUMAF(m) += AFLOW(i) + AFLOW2(i);
            }
        }
        // for (n = 1; n <= NetworkNumOfNodes; ++n) {
        //    if (LIST >= 1) gio::write(outputFile, Format_903) << "Room: " << n << PZ(n) << SUMAF(n) << properties[n].temperature;
        //}

        for (i = 1; i <= NetworkNumOfLinks; ++i) {
            if (AFLOW2(i) != 0.0) {
            }
            if (AFLOW(i) > 0.0) {
                state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW = AFLOW(i);
                state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 = 0.0;
            } else {
                state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW = 0.0;
                state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 = -AFLOW(i);
            }
            if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum ==
                iComponentTypeNum::HOP) {
                if (AFLOW(i) > 0.0) {
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW = AFLOW(i) + AFLOW2(i);
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 = AFLOW2(i);
                } else {
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW = AFLOW2(i);
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 = -AFLOW(i) + AFLOW2(i);
                }
            }
            if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).DetOpenNum > 0) {
                if (AFLOW2(i) != 0.0) {
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW = AFLOW(i) + AFLOW2(i);
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 = AFLOW2(i);
                }
            }
            if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum).CompTypeNum ==
                    iComponentTypeNum::SOP &&
                AFLOW2(i) != 0.0) {
                if (AFLOW(i) >= 0.0) {
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW = AFLOW(i);
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 = std::abs(AFLOW2(i));
                } else {
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW = std::abs(AFLOW2(i));
                    state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).FLOW2 = -AFLOW(i);
                }
            }
        }

        for (i = 1; i <= NetworkNumOfNodes; ++i) {
            state.dataAirflowNetwork->AirflowNetworkNodeSimu(i).PZ = PZ(i);
        }
    }

    void Solver::solvzp(EnergyPlusData &state, int &ITER) // number of iterations
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   Extracted from AIRNET
        //       MODIFIED       Lixing Gu, 2/1/04
        //                      Revised the subroutine to meet E+ needs
        //       MODIFIED       Lixing Gu, 6/8/05
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine solves zone pressures by modified Newton-Raphson iteration

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        auto &NetworkNumOfLinks = state.dataAFNSolver->NetworkNumOfLinks;
        auto &NetworkNumOfNodes = state.dataAFNSolver->NetworkNumOfNodes;

        // Argument array dimensioning (these used to be arguments, need to also test newAU and newIK)
        EP_SIZE_CHECK(IK, NetworkNumOfNodes + 1);
        EP_SIZE_CHECK(AD, NetworkNumOfNodes);
        EP_SIZE_CHECK(AU, IK(NetworkNumOfNodes + 1));

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // noel GNU says AU is being indexed beyound bounds
        // REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)-1) ! the upper triangle of [A] before and after factoring

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        //     NNZE   - number of nonzero entries in the "AU" array.
        //     LFLAG   - if = 1, use laminar relationship (initialization).
        //     I       - element number.
        //     N       - number of node/zone 1.
        //     M       - number of node/zone 2.
        //     F       - flows through the element (kg/s).
        //     DF      - partial derivatives:  DF/DP.
        //     NF      - number of flows, 1 or 2.
        //     SUMF    - sum of flows into node/zone.
        //     CCF     - current pressure correction (Pa).
        //     PCF     - previous pressure correction (Pa).
        //     CEF     - convergence enhancement factor.
        int n;
        int NNZE;
        int NSYM;
        bool LFLAG;
        int CONVG;
        int ACCEL;
        Array1D<Real64> PCF(NetworkNumOfNodes);
        Array1D<Real64> CEF(NetworkNumOfNodes);
        Real64 C;
        Real64 SSUMF;
        Real64 SSUMAF;
        Real64 ACC0;
        Real64 ACC1;
        Array1D<Real64> CCF(NetworkNumOfNodes);

        // auto &outputFile = std::cout;

        ACC1 = 0.0;
        ACCEL = 0;
        NSYM = 0;
        NNZE = IK(NetworkNumOfNodes + 1) - 1;
        // if (LIST >= 2) print(outputFile, "Initialization{:16}{:16}{:16}\n", NetworkNumOfNodes, NetworkNumOfLinks, NNZE);
        ITER = 0;

        for (n = 1; n <= NetworkNumOfNodes; ++n) {
            PCF(n) = 0.0;
            CEF(n) = 0.0;
        }

        if (state.dataAirflowNetwork->AirflowNetworkSimu.InitFlag != 1) {
            // Initialize node/zone pressure values by assuming only linear relationship between
            // airflows and pressure drops.
            LFLAG = true;
            auto &solver = state.dataAFNSolver->solver;
            solver.filjac(state, NNZE, LFLAG);
            for (n = 1; n <= NetworkNumOfNodes; ++n) {
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(n).NodeTypeNum == 0) PZ(n) = SUMF(n);
            }
            // Data dump.
//            if (LIST >= 3) {
//                DUMPVD("AD:", AD, NetworkNumOfNodes, outputFile);
//                DUMPVD("AU:", AU, NNZE, outputFile);
//                DUMPVR("AF:", SUMF, NetworkNumOfNodes, outputFile);
//            }
// Solve linear system for approximate PZ.
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
            FACSKY(state, newAU, AD, newAU, newIK, NetworkNumOfNodes, NSYM);     // noel
            SLVSKY(state, newAU, AD, newAU, PZ, newIK, NetworkNumOfNodes, NSYM); // noel
#else
            FACSKY(AU, AD, AU, IK, NetworkNumOfNodes, NSYM);
            SLVSKY(AU, AD, AU, PZ, IK, NetworkNumOfNodes, NSYM);
#endif
            // if (LIST >= 2) DUMPVD("PZ:", PZ, NetworkNumOfNodes, outputFile);
        }
        // Solve nonlinear airflow network equations by modified Newton's method.
        auto &solver = state.dataAFNSolver->solver;
        while (ITER < state.dataAirflowNetwork->AirflowNetworkSimu.MaxIteration) {
            LFLAG = false;
            ++ITER;
            //            if (LIST >= 2) {
            //                print(outputFile, "Begin iteration {}\n", ITER);
            //            }
            // Set up the Jacobian matrix.
            solver.filjac(state, NNZE, LFLAG);
            // Data dump.
            //            if (LIST >= 3) {
            //                DUMPVR("SUMF:", SUMF, NetworkNumOfNodes, outputFile);
            //                DUMPVR("SUMAF:", SUMAF, NetworkNumOfNodes, outputFile);
            //            }
            // Check convergence.
            CONVG = 1;
            SSUMF = 0.0;
            SSUMAF = 0.0;
            for (n = 1; n <= NetworkNumOfNodes; ++n) {
                SSUMF += std::abs(SUMF(n));
                SSUMAF += SUMAF(n);
                if (CONVG == 1) {
                    if (std::abs(SUMF(n)) <= state.dataAirflowNetwork->AirflowNetworkSimu.AbsTol) continue;
                    if (std::abs(SUMF(n) / SUMAF(n)) > state.dataAirflowNetwork->AirflowNetworkSimu.RelTol) CONVG = 0;
                }
            }
            ACC0 = ACC1;
            if (SSUMAF > 0.0) ACC1 = SSUMF / SSUMAF;
            if (CONVG == 1 && ITER > 1) return;
            if (ITER >= state.dataAirflowNetwork->AirflowNetworkSimu.MaxIteration) break;
            // Data dump.
            //            if (LIST >= 3) {
            //                DUMPVD("AD:", AD, NetworkNumOfNodes, outputFile);
            //                DUMPVD("AU:", AU, NNZE, outputFile);
            //            }
            // Solve AA * CCF = SUMF.
            for (n = 1; n <= NetworkNumOfNodes; ++n) {
                CCF(n) = SUMF(n);
            }
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
            FACSKY(state, newAU, AD, newAU, newIK, NetworkNumOfNodes, NSYM);      // noel
            SLVSKY(state, newAU, AD, newAU, CCF, newIK, NetworkNumOfNodes, NSYM); // noel
#else
            FACSKY(AU, AD, AU, IK, NetworkNumOfNodes, NSYM);
            SLVSKY(AU, AD, AU, CCF, IK, NetworkNumOfNodes, NSYM);
#endif
            // Revise PZ (Steffensen iteration on the N-R correction factors to handle oscillating corrections).
            if (ACCEL == 1) {
                ACCEL = 0;
            } else {
                if (ITER > 2 && ACC1 > 0.5 * ACC0) ACCEL = 1;
            }
            for (n = 1; n <= NetworkNumOfNodes; ++n) {
                if (state.dataAirflowNetwork->AirflowNetworkNodeData(n).NodeTypeNum == 1) continue;
                CEF(n) = 1.0;
                if (ACCEL == 1) {
                    C = CCF(n) / PCF(n);
                    if (C < state.dataAirflowNetwork->AirflowNetworkSimu.ConvLimit) CEF(n) = 1.0 / (1.0 - C);
                    C = CCF(n) * CEF(n);
                } else {
                    //            IF (CCF(N) .EQ. 0.0d0) CCF(N)=TINY(CCF(N))  ! 1.0E-40
                    if (CCF(n) == 0.0) CCF(n) = DataGlobalConstants::rTinyValue; // 1.0E-40 (Epsilon)
                    PCF(n) = CCF(n);
                    C = CCF(n);
                }
                if (std::abs(C) > state.dataAirflowNetwork->AirflowNetworkSimu.MaxPressure) {
                    CEF(n) *= state.dataAirflowNetwork->AirflowNetworkSimu.MaxPressure / std::abs(C);
                    PZ(n) -= CCF(n) * CEF(n);
                } else {
                    PZ(n) -= C;
                }
            }
            // Data revision dump.
            //            if (LIST >= 2) {
            //                for (n = 1; n <= NetworkNumOfNodes; ++n) {
            //                    if (state.dataAirflowNetwork->AirflowNetworkNodeData(n).NodeTypeNum == 0) {
            //                        print(outputFile, "Rev: {:5}{:3}{:14.6E} {:8.4F}{:24.14F}", n, SUMF(n), CCF(n), CEF(n), PZ(n));
            //                    }
            //                }
            //            }
        }

        // Error termination.
        ShowSevereError(state, "Too many iterations (SOLVZP) in Airflow Network simulation");
        ++state.dataAirflowNetwork->AirflowNetworkSimu.ExtLargeOpeningErrCount;
        if (state.dataAirflowNetwork->AirflowNetworkSimu.ExtLargeOpeningErrCount < 2) {
            ShowWarningError(state,
                             "AirflowNetwork: SOLVER, Changing values for initialization flag, Relative airflow convergence, Absolute airflow "
                             "convergence, Convergence acceleration limit or Maximum Iteration Number may solve the problem.");
            ShowContinueErrorTimeStamp(state, "");
            ShowContinueError(state,
                              "..Iterations=" + std::to_string(ITER) +
                                  ", Max allowed=" + std::to_string(state.dataAirflowNetwork->AirflowNetworkSimu.MaxIteration));
            ShowFatalError(state, "AirflowNetwork: SOLVER, The previous error causes termination.");
        } else {
            ShowRecurringWarningErrorAtEnd(state,
                                           "AirFlowNetwork: Too many iterations (SOLVZP) in AirflowNetwork simulation continues.",
                                           state.dataAirflowNetwork->AirflowNetworkSimu.ExtLargeOpeningErrIndex);
        }
    }

    void Solver::filjac(EnergyPlusData &state,
                        int const NNZE,  // number of nonzero entries in the "AU" array.
                        bool const LFLAG // if = 1, use laminar relationship (initialization).
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   Extracted from AIRNET
        //       MODIFIED       Lixing Gu, 2/1/04
        //                      Revised the subroutine to meet E+ needs
        //       MODIFIED       Lixing Gu, 6/8/05
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine creates matrices for solution of flows

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // I       - component number.
        // N       - number of node/zone 1.
        // M       - number of node/zone 2.
        // F       - flows through the element (kg/s).
        // DF      - partial derivatives:  DF/DP.
        // NF      - number of flows, 1 or 2.
        int i;
        int j;
        int n;
        int FLAG;
        int NF;
#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS
        int LHK; // noel
        int JHK;
        int JHK1;
        int newsum;
        int newh;
        int ispan;
        int thisIK;
        bool allZero; // noel
#endif
        Array1D<Real64> X(4);
        Real64 DP;
        std::array<Real64, 2> F{{0.0, 0.0}};
        std::array<Real64, 2> DF{{0.0, 0.0}};

        auto &NetworkNumOfLinks = state.dataAFNSolver->NetworkNumOfLinks;
        auto &NetworkNumOfNodes = state.dataAFNSolver->NetworkNumOfNodes;
        auto &DpL = state.dataAFNSolver->DpL;

        for (n = 1; n <= NetworkNumOfNodes; ++n) {
            SUMF(n) = 0.0;
            SUMAF(n) = 0.0;
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(n).NodeTypeNum == 1) {
                AD(n) = 1.0;
            } else {
                AD(n) = 0.0;
            }
        }
        for (n = 1; n <= NNZE; ++n) {
            AU(n) = 0.0;
        }
        //                              Set up the Jacobian matrix.
        for (i = 1; i <= NetworkNumOfLinks; ++i) {
            if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).element == nullptr) {
                continue;
            }
            n = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
            int m = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
            //!!! Check array of DP. DpL is used for multizone air flow calculation only
            //!!! and is not for forced air calculation
            if (i > state.dataAirflowNetwork->NumOfLinksMultiZone) {
                DP = PZ(n) - PZ(m) + PS(i) + PW(i);
            } else {
                DP = PZ(n) - PZ(m) + DpL(i, 1) + PW(i);
            }
            Real64 multiplier = 1.0;
            Real64 control = 1.0;
            // if (LIST >= 4) ObjexxFCL::gio::write(outputFile, Format_901) << "PS:" << i << n << M << PS(i) << PW(i) << AirflowNetworkLinkSimu(i).DP;
            j = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).CompNum;

            NF = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).element->calculate(
                state, LFLAG, DP, i, multiplier, control, properties[n], properties[m], F, DF);
            if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).element->type() == ComponentType::CPD && DP != 0.0) {
                DP = state.dataAirflowNetwork->DisSysCompCPDData(state.dataAirflowNetwork->AirflowNetworkCompData(j).TypeNum).DP;
            }

            state.dataAirflowNetwork->AirflowNetworkLinkSimu(i).DP = DP;
            AFLOW(i) = F[0];
            AFLOW2(i) = 0.0;
            if (state.dataAirflowNetwork->AirflowNetworkCompData(j).CompTypeNum == iComponentTypeNum::DOP) {
                AFLOW2(i) = F[1];
            }
            // if (LIST >= 3) ObjexxFCL::gio::write(outputFile, Format_901) << " NRi:" << i << n << M << AirflowNetworkLinkSimu(i).DP << F[0] <<
            // DF[0];
            FLAG = 1;
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(n).NodeTypeNum == 0) {
                ++FLAG;
                X(1) = DF[0];
                X(2) = -DF[0];
                SUMF(n) += F[0];
                SUMAF(n) += std::abs(F[0]);
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(m).NodeTypeNum == 0) {
                FLAG += 2;
                X(4) = DF[0];
                X(3) = -DF[0];
                SUMF(m) -= F[0];
                SUMAF(m) += std::abs(F[0]);
            }
            if (FLAG != 1) FILSKY(state, X, state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums, IK, AU, AD, FLAG);
            if (NF == 1) continue;
            AFLOW2(i) = F[1];
            // if (LIST >= 3) ObjexxFCL::gio::write(outputFile, Format_901) << " NRj:" << i << n << m << AirflowNetworkLinkSimu(i).DP << F[1] <<
            // DF[1];
            FLAG = 1;
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(n).NodeTypeNum == 0) {
                ++FLAG;
                X(1) = DF[1];
                X(2) = -DF[1];
                SUMF(n) += F[1];
                SUMAF(n) += std::abs(F[1]);
            }
            if (state.dataAirflowNetwork->AirflowNetworkNodeData(m).NodeTypeNum == 0) {
                FLAG += 2;
                X(4) = DF[1];
                X(3) = -DF[1];
                SUMF(m) -= F[1];
                SUMAF(m) += std::abs(F[1]);
            }
            if (FLAG != 1) FILSKY(state, X, state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums, IK, AU, AD, FLAG);
        }

#ifdef SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS

        // After the matrix values have been set, we can look at them and see if any columns are filled with zeros.
        // If they are, let's remove them from the matrix -- but only for the purposes of doing the solve.
        // They way I do this is building a separate IK array (newIK) that simply changes the column heights.
        // So the affected SOLVEs would use this newIK and nothing else changes.
        for (n = 1; n <= NetworkNumOfNodes + 1; ++n) {
            newIK(n) = IK(n);
            // print*, " NetworkNumOfNodes  n=", n, " IK(n)=", IK(n)
        }

        newsum = IK(2) - IK(1); // always 0?

        JHK = 1;
        for (n = 2; n <= NetworkNumOfNodes; ++n) {
            JHK1 = IK(n + 1); // starts at IK(3)-IK(2)
            LHK = JHK1 - JHK;
            if (LHK <= 0) {
                newIK(n + 1) = newIK(n);
                continue;
            }
            // write(*,'(4(a,i8))') "n=", n, " ik=", ik(n), " JHK=", JHK, " LHK=", LHK

            // is the entire column zero?  noel
            allZero = true;
            for (i = 0; i <= LHK - 1; ++i) {
                if (AU(JHK + i) != 0.0) {
                    allZero = false;
                    break;
                }
            }

            newh = LHK;
            if (allZero) {
                // print*, "allzero n=", n
                newh = 0;
            } else {
                // DO i=0,LHK-1
                //   write(*, '(2(a,i8),a, f15.3)') "  n=", n, " i=", i, " AU(JHK+i)=", AU(JHK+i)
                // enddo
            }
            newIK(n + 1) = newIK(n) + newh;
            newsum += newh;

            // do i = LHK-1,0, -1
            //   write(*, '(2(a,i8),a, f15.3)') "  n=", n, " i=", i, " AU(JHK+i)=", AU(JHK+i)
            // enddo
            JHK = JHK1;
        }

        // this is just a print to screen, is not necessary
        //     if (firstTime) then
        //        write(*, '(2(a,i8))') " After SKYLINE_MATRIX_REMOVE_ZERO_COLUMNS: newsum=", newsum, " oldsum=", IK(NetworkNumOfNodes+1)
        //        firstTime=.FALSE.
        //     endif

        // Now fill newAU from AU, using newIK
        thisIK = 1;
        for (n = 2; n <= NetworkNumOfNodes; ++n) {
            thisIK = newIK(n);
            ispan = newIK(n + 1) - thisIK;

            if (ispan <= 0) continue;
            for (i = 0; i <= ispan - 1; ++i) {
                newAU(thisIK + i) = AU(IK(n) + i);
            }
        }
#endif
    }

    int GenericCrack(EnergyPlusData &state,
                     Real64 &coef,               // Flow coefficient
                     Real64 const expn,          // Flow exponent
                     bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                     Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                     const AirProperties &propN, // Node 1 properties
                     const AirProperties &propM, // Node 2 properties
                     std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                     std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   Extracted from AIRNET
        //       MODIFIED       Lixing Gu, 2/1/04
        //                      Revised the subroutine to meet E+ needs
        //       MODIFIED       Lixing Gu, 6/8/05
        //       RE-ENGINEERED  This subroutine is revised from AFEPLR developed by George Walton, NIST

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine solves airflow for a power law component

        // METHODOLOGY EMPLOYED:
        // Using Q=C(dP)^n

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CDM;
        Real64 FL;
        Real64 FT;
        Real64 RhozNorm;
        Real64 VisczNorm;
        Real64 Ctl;
        Real64 VisAve;
        Real64 Tave;
        Real64 RhoCor;

        // Calculate normal density and viscocity at Crack standard condition: T=20C, p=101325 Pa and 0 g/kg
        RhozNorm = AIRDENSITY(state, 101325.0, 20.0, 0.0);
        VisczNorm = 1.71432e-5 + 4.828e-8 * 20.0;
        VisAve = (propN.viscosity + propM.viscosity) / 2.0;
        Tave = (propN.temperature + propM.temperature) / 2.0;
        if (PDROP >= 0.0) {
            coef /= propN.sqrtDensity;
        } else {
            coef /= propM.sqrtDensity;
        }

        if (LFLAG) {
            // Initialization by linear relation.
            if (PDROP >= 0.0) {
                RhoCor = (propN.temperature + DataGlobalConstants::KelvinConv) / (Tave + DataGlobalConstants::KelvinConv);
                Ctl = std::pow(RhozNorm / propN.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
                DF[0] = coef * propN.density / propN.viscosity * Ctl;
            } else {
                RhoCor = (propM.temperature + DataGlobalConstants::KelvinConv) / (Tave + DataGlobalConstants::KelvinConv);
                Ctl = std::pow(RhozNorm / propM.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
                DF[0] = coef * propM.density / propM.viscosity * Ctl;
            }
            F[0] = -DF[0] * PDROP;
        } else {
            // Standard calculation.
            if (PDROP >= 0.0) {
                // Flow in positive direction.
                // Laminar flow.
                RhoCor = (propN.temperature + DataGlobalConstants::KelvinConv) / (Tave + DataGlobalConstants::KelvinConv);
                Ctl = std::pow(RhozNorm / propN.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
                CDM = coef * propN.density / propN.viscosity * Ctl;
                FL = CDM * PDROP;
                // Turbulent flow.
                if (expn == 0.5) {
                    FT = coef * propN.sqrtDensity * std::sqrt(PDROP) * Ctl;
                } else {
                    FT = coef * propN.sqrtDensity * std::pow(PDROP, expn) * Ctl;
                }
            } else {
                // Flow in negative direction.
                // Laminar flow.
                RhoCor = (propM.temperature + DataGlobalConstants::KelvinConv) / (Tave + DataGlobalConstants::KelvinConv);
                Ctl = std::pow(RhozNorm / propM.density / RhoCor, 2.0 * expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
                CDM = coef * propM.density / propM.viscosity * Ctl;
                FL = CDM * PDROP;
                // Turbulent flow.
                if (expn == 0.5) {
                    FT = -coef * propM.sqrtDensity * std::sqrt(-PDROP) * Ctl;
                } else {
                    FT = -coef * propM.sqrtDensity * std::pow(-PDROP, expn) * Ctl;
                }
            }
            // Select laminar or turbulent flow.
            //            if (LIST >= 4) {
            //                static ObjexxFCL::gio::Fmt Format_901("(A5,6X,4E16.7)");
            //                print(std::cout, " generic crack: {:5}      {:16.7E} {16.7E}\n", PDROP, FL, FT);
            //            }
            if (std::abs(FL) <= std::abs(FT)) {
                F[0] = FL;
                DF[0] = CDM;
            } else {
                F[0] = FT;
                DF[0] = FT * expn / PDROP;
            }
        }
        return 1;
    }

    int GenericDuct(Real64 const Length,        // Duct length
                    Real64 const Diameter,      // Duct diameter
                    bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                    Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                    const AirProperties &propN, // Node 1 properties
                    const AirProperties &propM, // Node 2 properties
                    std::array<Real64, 2> &F,   // Airflow through the component [kg/s]
                    std::array<Real64, 2> &DF   // Partial derivative:  DF/DP
    )
    {

        // This subroutine solve air flow as a duct if fan has zero flow rate

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const C(0.868589);
        Real64 const EPS(0.001);
        Real64 const Rough(0.0001);
        Real64 const InitLamCoef(128.0);
        Real64 const LamDynCoef(64.0);
        Real64 const LamFriCoef(0.0001);
        Real64 const TurDynCoef(0.0001);

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 A0;
        Real64 A1;
        Real64 A2;
        Real64 B;
        Real64 D;
        Real64 S2;
        Real64 CDM;
        Real64 FL;
        Real64 FT;
        Real64 FTT;
        Real64 RE;

        // Get component properties
        Real64 ed = Rough / Diameter;
        Real64 area = Diameter * Diameter * DataGlobalConstants::Pi / 4.0;
        Real64 ld = Length / Diameter;
        Real64 g = 1.14 - 0.868589 * std::log(ed);
        Real64 AA1 = g;

        if (LFLAG) {
            // Initialization by linear relation.
            if (PDROP >= 0.0) {
                DF[0] = (2.0 * propN.density * area * Diameter) / (propN.viscosity * InitLamCoef * ld);
            } else {
                DF[0] = (2.0 * propM.density * area * Diameter) / (propM.viscosity * InitLamCoef * ld);
            }
            F[0] = -DF[0] * PDROP;
        } else {
            // Standard calculation.
            if (PDROP >= 0.0) {
                // Flow in positive direction.
                // Laminar flow coefficient !=0
                if (LamFriCoef >= 0.001) {
                    A2 = LamFriCoef / (2.0 * propN.density * area * area);
                    A1 = (propN.viscosity * LamDynCoef * ld) / (2.0 * propN.density * area * Diameter);
                    A0 = -PDROP;
                    CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                    FL = (CDM - A1) / (2.0 * A2);
                    CDM = 1.0 / CDM;
                } else {
                    CDM = (2.0 * propN.density * area * Diameter) / (propN.viscosity * LamDynCoef * ld);
                    FL = CDM * PDROP;
                }
                RE = FL * Diameter / (propN.viscosity * area);
                // Turbulent flow; test when Re>10.
                if (RE >= 10.0) {
                    S2 = std::sqrt(2.0 * propN.density * PDROP) * area;
                    FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                    while (true) {
                        FT = FTT;
                        B = (9.3 * propN.viscosity * area) / (FT * Rough);
                        D = 1.0 + g * B;
                        g -= (g - AA1 + C * std::log(D)) / (1.0 + C * B / D);
                        FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                        if (std::abs(FTT - FT) / FTT < EPS) break;
                    }
                    FT = FTT;
                } else {
                    FT = FL;
                }
            } else {
                // Flow in negative direction.
                // Laminar flow coefficient !=0
                if (LamFriCoef >= 0.001) {
                    A2 = LamFriCoef / (2.0 * propM.density * area * area);
                    A1 = (propM.viscosity * LamDynCoef * ld) / (2.0 * propM.density * area * Diameter);
                    A0 = PDROP;
                    CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                    FL = -(CDM - A1) / (2.0 * A2);
                    CDM = 1.0 / CDM;
                } else {
                    CDM = (2.0 * propM.density * area * Diameter) / (propM.viscosity * LamDynCoef * ld);
                    FL = CDM * PDROP;
                }
                RE = -FL * Diameter / (propM.viscosity * area);
                // Turbulent flow; test when Re>10.
                if (RE >= 10.0) {
                    S2 = std::sqrt(-2.0 * propM.density * PDROP) * area;
                    FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                    while (true) {
                        FT = FTT;
                        B = (9.3 * propM.viscosity * area) / (FT * Rough);
                        D = 1.0 + g * B;
                        g -= (g - AA1 + C * std::log(D)) / (1.0 + C * B / D);
                        FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                        if (std::abs(FTT - FT) / FTT < EPS) break;
                    }
                    FT = -FTT;
                } else {
                    FT = FL;
                }
            }
            // Select laminar or turbulent flow.
            if (std::abs(FL) <= std::abs(FT)) {
                F[0] = FL;
                DF[0] = CDM;
            } else {
                F[0] = FT;
                DF[0] = 0.5 * FT / PDROP;
            }
        }
        return 1;
    }

    void FACSKY(EnergyPlusData &state,
                Array1D<Real64> &AU,   // the upper triangle of [A] before and after factoring
                Array1D<Real64> &AD,   // the main diagonal of [A] before and after factoring
                Array1D<Real64> &AL,   // the lower triangle of [A] before and after factoring
                const Array1D_int &IK, // pointer to the top of column/row "K"
                int const NEQ,         // number of equations
                int const NSYM         // symmetry:  0 = symmetric matrix, 1 = non-symmetric
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   Extracted from AIRNET
        //       MODIFIED       Lixing Gu, 2/1/04
        //                      Revised the subroutine to meet E+ needs
        //       MODIFIED       Lixing Gu, 6/8/05
        //       RE-ENGINEERED  This subroutine is revised from FACSKY developed by George Walton, NIST

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine performs L-U factorization of a skyline ordered matrix, [A]
        // The algorithm has been restructured for clarity.
        // Note dependence on compiler for optimizing the inner do loops.

        // METHODOLOGY EMPLOYED:
        //     L-U factorization of a skyline ordered matrix, [A], used for
        //     solution of simultaneous linear algebraic equations [A] * X = B.
        //     No pivoting!  No scaling!  No warnings!!!
        //     Related routines:  SLVSKY, SETSKY, FILSKY.

        // REFERENCES:
        //     Algorithm is described in "The Finite Element Method Displayed",
        //     by G. Dhatt and G. Touzot, John Wiley & Sons, New York, 1984.

        // USE STATEMENTS:
        // na

        auto &NetworkNumOfLinks = state.dataAFNSolver->NetworkNumOfLinks;
        auto &NetworkNumOfNodes = state.dataAFNSolver->NetworkNumOfNodes;

        // Argument array dimensioning
        EP_SIZE_CHECK(IK, NetworkNumOfNodes + 1);
        EP_SIZE_CHECK(AU, IK(NetworkNumOfNodes + 1));
        EP_SIZE_CHECK(AD, NetworkNumOfNodes);
        EP_SIZE_CHECK(AL, IK(NetworkNumOfNodes + 1) - 1);

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // noel, GNU says the AU is indexed above its upper bound
        // REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)-1) ! the upper triangle of [A] before and after factoring

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int JHK;
        int JHK1;
        int LHK;
        int LHK1;
        int IMIN;
        int IMIN1;
        int JHJ;
        int JHJ1;
        int IC;
        int i;
        int j;
        int k;
        Real64 T1;
        Real64 T2;
        Real64 SDOT;
        Real64 SUMD;

        AD(1) = 1.0 / AD(1);
        JHK = 1;
        for (k = 2; k <= NEQ; ++k) {
            SUMD = 0.0;
            JHK1 = IK(k + 1);
            LHK = JHK1 - JHK;
            if (LHK > 0) {
                LHK1 = LHK - 1;
                IMIN = k - LHK1;
                IMIN1 = IMIN - 1;
                if (NSYM == 1) AL(JHK) *= AD(IMIN1);
                if (LHK1 != 0) {
                    JHJ = IK(IMIN);
                    if (NSYM == 0) {
                        for (j = 1; j <= LHK1; ++j) {
                            JHJ1 = IK(IMIN + j);
                            IC = min(j, JHJ1 - JHJ);
                            if (IC > 0) {
                                SDOT = 0.0;
                                for (i = 0; i <= IC - 1; ++i) {
                                    SDOT += AU(JHJ1 - IC + i) * AU(JHK + j - IC + i);
                                }
                                AU(JHK + j) -= SDOT;
                            }
                            JHJ = JHJ1;
                        }
                    } else {
                        for (j = 1; j <= LHK1; ++j) {
                            JHJ1 = IK(IMIN + j);
                            IC = min(j, JHJ1 - JHJ);
                            SDOT = 0.0;
                            if (IC > 0) {
                                for (i = 0; i <= IC - 1; ++i) {
                                    SDOT += AL(JHJ1 - IC + i) * AU(JHK + j - IC + i);
                                }
                                AU(JHK + j) -= SDOT;
                                SDOT = 0.0;
                                for (i = 0; i <= IC - 1; ++i) {
                                    SDOT += AU(JHJ1 - IC + i) * AL(JHK + j - IC + i);
                                }
                            }
                            AL(JHK + j) = (AL(JHK + j) - SDOT) * AD(IMIN1 + j);
                            JHJ = JHJ1;
                        }
                    }
                }
                if (NSYM == 0) {
                    for (i = 0; i <= LHK1; ++i) {
                        T1 = AU(JHK + i);
                        T2 = T1 * AD(IMIN1 + i);
                        AU(JHK + i) = T2;
                        SUMD += T1 * T2;
                    }
                } else {
                    for (i = 0; i <= LHK1; ++i) {
                        SUMD += AU(JHK + i) * AL(JHK + i);
                    }
                }
            }
            if (AD(k) - SUMD == 0.0) {
                ShowSevereError(state, "AirflowNetworkSolver: L-U factorization in Subroutine FACSKY.");
                ShowContinueError(state,
                                  "The denominator used in L-U factorizationis equal to 0.0 at node = " +
                                      state.dataAirflowNetwork->AirflowNetworkNodeData(k).Name + '.');
                ShowContinueError(
                    state, "One possible cause is that this node may not be connected directly, or indirectly via airflow network connections ");
                ShowContinueError(
                    state, "(e.g., AirflowNetwork:Multizone:SurfaceCrack, AirflowNetwork:Multizone:Component:SimpleOpening, etc.), to an external");
                ShowContinueError(state, "node (AirflowNetwork:MultiZone:Surface).");
                ShowContinueError(state,
                                  "Please send your input file and weather file to EnergyPlus support/development team for further investigation.");
                ShowFatalError(state, "Preceding condition causes termination.");
            }
            AD(k) = 1.0 / (AD(k) - SUMD);
            JHK = JHK1;
        }
    }

    void SLVSKY(EnergyPlusData &state,
                const Array1D<Real64> &AU, // the upper triangle of [A] before and after factoring
                const Array1D<Real64> &AD, // the main diagonal of [A] before and after factoring
                const Array1D<Real64> &AL, // the lower triangle of [A] before and after factoring
                Array1D<Real64> &B,        // "B" vector (input); "X" vector (output).
                const Array1D_int &IK,     // pointer to the top of column/row "K"
                int const NEQ,             // number of equations
                int const NSYM             // symmetry:  0 = symmetric matrix, 1 = non-symmetric
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   Extracted from AIRNET
        //       MODIFIED       Lixing Gu, 2/1/04
        //                      Revised the subroutine to meet E+ needs
        //       MODIFIED       Lixing Gu, 6/8/05
        //       RE-ENGINEERED  This subroutine is revised from CLVSKY developed by George Walton, NIST

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine solves simultaneous linear algebraic equations [A] * X = B
        // using L-U factored skyline form of [A] from "FACSKY"

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        auto &NetworkNumOfLinks = state.dataAFNSolver->NetworkNumOfLinks;
        auto &NetworkNumOfNodes = state.dataAFNSolver->NetworkNumOfNodes;

        // Argument array dimensioning
        EP_SIZE_CHECK(IK, NetworkNumOfNodes + 1);
        EP_SIZE_CHECK(AU, IK(NetworkNumOfNodes + 1));
        EP_SIZE_CHECK(AD, NetworkNumOfNodes);
        EP_SIZE_CHECK(AL, IK(NetworkNumOfNodes + 1) - 1);
        EP_SIZE_CHECK(B, NetworkNumOfNodes);

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // noel, GNU says the AU is indexed above its upper bound
        // REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)-1) ! the upper triangle of [A] before and after factoring

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int i;
        int JHK;
        int JHK1;
        int k;
        int LHK;
        Real64 SDOT;
        Real64 T1;

        JHK = 1;
        for (k = 2; k <= NEQ; ++k) {
            JHK1 = IK(k + 1);
            LHK = JHK1 - JHK;
            if (LHK <= 0) continue;
            SDOT = 0.0;
            if (NSYM == 0) {
                for (i = 0; i <= LHK - 1; ++i) {
                    SDOT += AU(JHK + i) * B(k - LHK + i);
                }
            } else {
                for (i = 0; i <= LHK - 1; ++i) {
                    SDOT += AL(JHK + i) * B(k - LHK + i);
                }
            }
            B(k) -= SDOT;
            JHK = JHK1;
        }
        if (NSYM == 0) {
            for (k = 1; k <= NEQ; ++k) {
                B(k) *= AD(k);
            }
        }
        k = NEQ + 1;
        JHK1 = IK(k);
        while (k != 1) {
            --k;
            if (NSYM == 1) B(k) *= AD(k);
            if (k == 1) break;
            //        IF(K.EQ.1) RETURN
            JHK = IK(k);
            T1 = B(k);
            for (i = 0; i <= JHK1 - JHK - 1; ++i) {
                B(k - JHK1 + JHK + i) -= AU(JHK + i) * T1;
            }
            JHK1 = JHK;
        }
    }

    void FILSKY(EnergyPlusData &state,
                const Array1D<Real64> &X,    // element array (row-wise sequence)
                std::array<int, 2> const LM, // location matrix
                const Array1D_int &IK,       // pointer to the top of column/row "K"
                Array1D<Real64> &AU,         // the upper triangle of [A] before and after factoring
                Array1D<Real64> &AD,         // the main diagonal of [A] before and after factoring
                int const FLAG               // mode of operation
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   Extracted from AIRNET
        //       MODIFIED       Lixing Gu, 2/1/04
        //                      Revised the subroutine to meet E+ needs
        //       MODIFIED       Lixing Gu, 6/8/05
        //       RE-ENGINEERED  This subroutine is revised from FILSKY developed by George Walton, NIST

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine adds element array "X" to the sparse skyline matrix [A]

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        auto &NetworkNumOfLinks = state.dataAFNSolver->NetworkNumOfLinks;
        auto &NetworkNumOfNodes = state.dataAFNSolver->NetworkNumOfNodes;

        // Argument array dimensioning
        EP_SIZE_CHECK(X, 4);
        EP_SIZE_CHECK(IK, NetworkNumOfNodes + 1);
        EP_SIZE_CHECK(AU, IK(NetworkNumOfNodes + 1));
        EP_SIZE_CHECK(AD, NetworkNumOfNodes);

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // noel, GNU says the AU is indexed above its upper bound
        // REAL(r64), INTENT(INOUT) :: AU(IK(NetworkNumOfNodes+1)-1) ! the upper triangle of [A] before and after factoring

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int j;
        int k;
        int L;

        // K = row number, L = column number.
        if (FLAG > 1) {
            k = LM[0];
            L = LM[1];
            if (FLAG == 4) {
                AD(k) += X(1);
                if (k < L) {
                    j = IK(L + 1) - L + k;
                    AU(j) += X(2);
                } else {
                    j = IK(k + 1) - k + L;
                    AU(j) += X(3);
                }
                AD(L) += X(4);
            } else if (FLAG == 3) {
                AD(L) += X(4);
            } else if (FLAG == 2) {
                AD(k) += X(1);
            }
        }
    }

    void DUMPVD(std::string const &S,     // Description
                const Array1D<Real64> &V, // Output values
                int const n,              // Array size
                std::ostream &UOUT        // Output file unit
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   Extracted from AIRNET
        //       MODIFIED       Lixing Gu, 2/1/04
        //                      Revised the subroutine to meet E+ needs
        //       MODIFIED       Lixing Gu, 6/8/05
        //       RE-ENGINEERED  This subroutine is revised from DUMPVD developed by George Walton, NIST

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine prints the contents of the REAL(r64) "V" vector

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Argument array dimensioning
        // V.dim(_);

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;

        // Formats

        // Write values for debug
        print(UOUT, " {}", S);
        for (i = 1; i <= n; ++i) {
            print(UOUT, " {:15.7E}", V(i));
        }
        print(UOUT, "\n");
    }

    void DUMPVR(std::string const &S,     // Description
                const Array1D<Real64> &V, // Output values
                int const n,              // Array size
                std::ostream &UOUT        // Output file unit
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   Extracted from AIRNET
        //       MODIFIED       Lixing Gu, 2/1/04
        //                      Revised the subroutine to meet E+ needs
        //       MODIFIED       Lixing Gu, 6/8/05
        //       RE-ENGINEERED  This subroutine is revised from DUMPVR developed by George Walton, NIST

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine prints the contents of the REAL(r64) "V" vector

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Argument array dimensioning
        // V.dim(_);

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int i;

        print(UOUT, " {}", S);
        for (i = 1; i <= n; ++i) {
            print(UOUT, " {:15.7E}", V(i));
        }
        print(UOUT, "\n");
    }

    void PresProfile(EnergyPlusData &state,
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

        auto &RhoProfF = state.dataAFNSolver->RhoProfF;
        auto &RhoProfT = state.dataAFNSolver->RhoProfT;
        auto &DpProf = state.dataAFNSolver->DpProf;

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

    void PStack(EnergyPlusData &state)
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
        Real64 const PSea(101325.0);

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

        auto &DpL = state.dataAFNSolver->DpL;

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

            auto &solver = state.dataAFNSolver->solver;

            TempL1 = solver.properties[From].temperature;
            Xhl1 = solver.properties[From].humidityRatio;
            TzFrom = solver.properties[From].temperature;
            XhzFrom = solver.properties[From].humidityRatio;
            RhoL1 = solver.properties[From].density;
            if (ll == 0 || ll == 3) {
                PzFrom = solver.PZ(From);
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

            TempL2 = solver.properties[To].temperature;
            Xhl2 = solver.properties[To].humidityRatio;
            TzTo = solver.properties[To].temperature;
            XhzTo = solver.properties[To].humidityRatio;
            RhoL2 = solver.properties[To].density;

            if (ll < 3) {
                PzTo = solver.PZ(To);
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
            LClimb(state,
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
                    LClimb(
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
                LClimb(state, G, RhoStd, HSt(k), T, X, DpF(k), Fromz, PzFrom, Pbz, RhoDrDummi); // Autodesk:BoundsViolation k>2 poss
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
            LClimb(state,
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
                    LClimb(state, G, RhoStd, HSt(k), T, X, DpT(k), Toz, PzTo, Pbz, RhoDrDummi); // Autodesk:BoundsViolation HSt(k) and DpT(k) @ k>2
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
                LClimb(state, G, RhoStd, HSt(k), T, X, DpT(k), Toz, PzTo, Pbz, RhoDrDummi); // Autodesk:BoundsViolation k>2 poss
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
                PresProfile(state, i, Pprof, G, DpF, DpT, BetaStF, BetaStT, RhoStF, RhoStT, From, To, ActLh, Hfl(i));
                ++OpenNum;
            }
        }
    }

    Real64 psz(Real64 const Pz0,  // Pressure at altitude z0 [Pa]
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

    void LClimb(EnergyPlusData &state,
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
