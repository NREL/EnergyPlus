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

#include "AirflowNetwork/Elements.hpp"
#include "AirflowNetwork/Properties.hpp"

#include "../../Data/EnergyPlusData.hh"
#include "../../DataAirLoop.hh"
#include "../../DataHVACGlobals.hh"
#include "../../DataLoopNode.hh"
#include "../../DataSurfaces.hh"

namespace EnergyPlus {

namespace AirflowNetwork {

    // MODULE INFORMATION:
    //       AUTHOR         Lixing Gu, Don Shirey, and Muthusamy V. Swami
    //       DATE WRITTEN   Aug. 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module should contain the information that is needed to simulate
    // performance of air distribution system, including pressure, temperature
    // and moisture levels at each node, and airflow and sensible and latent energy losses
    // at each element

    // Data
    // module should be available to other modules and routines.  Thus,
    // all variables in this module must be PUBLIC.

    // MODULE PARAMETER DEFINITIONS:

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE DECLARATIONS:
    // Node simulation variable in air distribution system
    // Link simulation variable in air distribution system
    // Sensible and latent exchange variable in air distribution system

    // Object Data
    // Array1D<AirflowNetworkExchangeProp> AirflowNetworkExchangeData;
    // Array1D<AirflowNetworkExchangeProp> AirflowNetworkMultiExchangeData;
    // Array1D<AirflowNetworkLinkReportData> AirflowNetworkLinkReport;
    // Array1D<AirflowNetworkNodeReportData> AirflowNetworkNodeReport;
    // Array1D<AirflowNetworkLinkReportData> AirflowNetworkLinkReport1;
    // Array1D<ReferenceConditions> MultizoneSurfaceStdConditionsCrackData;

    static Real64 square(Real64 x)
    {
        return x * x;
    }

    int Duct::calculate([[maybe_unused]] EnergyPlusData &state,
                        bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                        Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                        [[maybe_unused]] int const i,             // Linkage number
                        [[maybe_unused]] const Real64 multiplier, // Element multiplier
                        [[maybe_unused]] const Real64 control,    // Element control signal
                        const AirProperties &propN,               // Node 1 properties
                        const AirProperties &propM,               // Node 2 properties
                        std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                        std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a duct/pipe component using Colebrook equation for the
        // turbulent friction factor

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const C(0.868589);
        Real64 const EPS(0.001);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 A0;
        Real64 A1;
        Real64 A2;
        Real64 B;
        Real64 D;
        Real64 S2;
        Real64 CDM;
        Real64 FL; // friction factor for laminar flow.
        Real64 FT; // friction factor for turbulent flow.
        Real64 FTT;
        Real64 RE; // Reynolds number.
        Real64 ed;
        Real64 ld;
        Real64 g;
        Real64 AA1;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        // CompNum = state.dataAirflowNetwork->AirflowNetworkCompData(j).TypeNum;
        ed = roughness / hydraulicDiameter;
        ld = L / hydraulicDiameter;
        g = 1.14 - 0.868589 * std::log(ed);
        AA1 = g;

        if (LFLAG) {
            // Initialization by linear relation.
            if (PDROP >= 0.0) {
                DF[0] = (2.0 * propN.density * A * hydraulicDiameter) / (propN.viscosity * InitLamCoef * ld);
            } else {
                DF[0] = (2.0 * propM.density * A * hydraulicDiameter) / (propM.viscosity * InitLamCoef * ld);
            }
            F[0] = -DF[0] * PDROP;
            // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwi:" << i << InitLamCoef << F[0] << DF[0];
        } else {
            // Standard calculation.
            if (PDROP >= 0.0) {
                // Flow in positive direction.
                // Laminar flow coefficient !=0
                if (LamFriCoef >= 0.001) {
                    A2 = LamFriCoef / (2.0 * propN.density * A * A);
                    A1 = (propN.viscosity * LamDynCoef * ld) / (2.0 * propN.density * A * hydraulicDiameter);
                    A0 = -PDROP;
                    CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                    FL = (CDM - A1) / (2.0 * A2);
                    CDM = 1.0 / CDM;
                } else {
                    CDM = (2.0 * propN.density * A * hydraulicDiameter) / (propN.viscosity * LamDynCoef * ld);
                    FL = CDM * PDROP;
                }
                RE = FL * hydraulicDiameter / (propN.viscosity * A);
                // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwl:" << i << PDROP << FL << CDM << RE;
                // Turbulent flow; test when Re>10.
                if (RE >= 10.0) {
                    S2 = std::sqrt(2.0 * propN.density * PDROP) * A;
                    FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                    // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << S2 << FTT << g;
                    while (true) {
                        FT = FTT;
                        B = (9.3 * propN.viscosity * A) / (FT * roughness);
                        D = 1.0 + g * B;
                        g -= (g - AA1 + C * std::log(D)) / (1.0 + C * B / D);
                        FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                        // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << B << FTT << g;
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
                    A2 = LamFriCoef / (2.0 * propM.density * A * A);
                    A1 = (propM.viscosity * LamDynCoef * ld) / (2.0 * propM.density * A * hydraulicDiameter);
                    A0 = PDROP;
                    CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                    FL = -(CDM - A1) / (2.0 * A2);
                    CDM = 1.0 / CDM;
                } else {
                    CDM = (2.0 * propM.density * A * hydraulicDiameter) / (propM.viscosity * LamDynCoef * ld);
                    FL = CDM * PDROP;
                }
                RE = -FL * hydraulicDiameter / (propM.viscosity * A);
                // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwl:" << i << PDROP << FL << CDM << RE;
                // Turbulent flow; test when Re>10.
                if (RE >= 10.0) {
                    S2 = std::sqrt(-2.0 * propM.density * PDROP) * A;
                    FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                    // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << S2 << FTT << g;
                    while (true) {
                        FT = FTT;
                        B = (9.3 * propM.viscosity * A) / (FT * roughness);
                        D = 1.0 + g * B;
                        g -= (g - AA1 + C * std::log(D)) / (1.0 + C * B / D);
                        FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                        // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << B << FTT << g;
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

    int Duct::calculate([[maybe_unused]] EnergyPlusData &state,
                        Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                        [[maybe_unused]] const Real64 multiplier, // Element multiplier
                        [[maybe_unused]] const Real64 control,    // Element control signal
                        const AirProperties &propN,               // Node 1 properties
                        const AirProperties &propM,               // Node 2 properties
                        std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                        std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a duct/pipe component using Colebrook equation for the
        // turbulent friction factor

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const C(0.868589);
        Real64 const EPS(0.001);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 A0;
        Real64 A1;
        Real64 A2;
        Real64 B;
        Real64 D;
        Real64 S2;
        Real64 CDM;
        Real64 FL; // friction factor for laminar flow.
        Real64 FT; // friction factor for turbulent flow.
        Real64 FTT;
        Real64 RE; // Reynolds number.
        Real64 ed;
        Real64 ld;
        Real64 g;
        Real64 AA1;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        // CompNum = state.dataAirflowNetwork->AirflowNetworkCompData(j).TypeNum;
        ed = roughness / hydraulicDiameter;
        ld = L / hydraulicDiameter;
        g = 1.14 - 0.868589 * std::log(ed);
        AA1 = g;

        // Standard calculation.
        if (PDROP >= 0.0) {
            // Flow in positive direction.
            // Laminar flow coefficient !=0
            if (LamFriCoef >= 0.001) {
                A2 = LamFriCoef / (2.0 * propN.density * A * A);
                A1 = (propN.viscosity * LamDynCoef * ld) / (2.0 * propN.density * A * hydraulicDiameter);
                A0 = -PDROP;
                CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                FL = (CDM - A1) / (2.0 * A2);
                CDM = 1.0 / CDM;
            } else {
                CDM = (2.0 * propN.density * A * hydraulicDiameter) / (propN.viscosity * LamDynCoef * ld);
                FL = CDM * PDROP;
            }
            RE = FL * hydraulicDiameter / (propN.viscosity * A);
            // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwl:" << i << PDROP << FL << CDM << RE;
            // Turbulent flow; test when Re>10.
            if (RE >= 10.0) {
                S2 = std::sqrt(2.0 * propN.density * PDROP) * A;
                FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << S2 << FTT << g;
                while (true) {
                    FT = FTT;
                    B = (9.3 * propN.viscosity * A) / (FT * roughness);
                    D = 1.0 + g * B;
                    g -= (g - AA1 + C * std::log(D)) / (1.0 + C * B / D);
                    FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                    // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << B << FTT << g;
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
                A2 = LamFriCoef / (2.0 * propM.density * A * A);
                A1 = (propM.viscosity * LamDynCoef * ld) / (2.0 * propM.density * A * hydraulicDiameter);
                A0 = PDROP;
                CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                FL = -(CDM - A1) / (2.0 * A2);
                CDM = 1.0 / CDM;
            } else {
                CDM = (2.0 * propM.density * A * hydraulicDiameter) / (propM.viscosity * LamDynCoef * ld);
                FL = CDM * PDROP;
            }
            RE = -FL * hydraulicDiameter / (propM.viscosity * A);
            // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwl:" << i << PDROP << FL << CDM << RE;
            // Turbulent flow; test when Re>10.
            if (RE >= 10.0) {
                S2 = std::sqrt(-2.0 * propM.density * PDROP) * A;
                FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << S2 << FTT << g;
                while (true) {
                    FT = FTT;
                    B = (9.3 * propM.viscosity * A) / (FT * roughness);
                    D = 1.0 + g * B;
                    g -= (g - AA1 + C * std::log(D)) / (1.0 + C * B / D);
                    FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                    // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << B << FTT << g;
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
        return 1;
    }

    int SurfaceCrack::calculate(EnergyPlusData &state,
                                bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                                Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                int const i,                              // Linkage number
                                [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                [[maybe_unused]] const Real64 control,    // Element control signal
                                const AirProperties &propN,               // Node 1 properties
                                const AirProperties &propM,               // Node 2 properties
                                std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a surface crack component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CDM;
        Real64 FL;
        Real64 FT;
        Real64 RhozNorm;
        Real64 VisczNorm;
        Real64 expn;
        Real64 Ctl;
        Real64 coef;
        Real64 Corr;
        Real64 VisAve;
        Real64 Tave;
        Real64 RhoCor;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        // Crack standard condition from given inputs
        if (i > state.dataAFNSolver->NetworkNumOfLinks - state.dataAirflowNetwork->NumOfLinksIntraZone) {
            Corr = 1.0;
        } else {
            Corr = state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor;
        }
        // CompNum = state.dataAirflowNetwork->AirflowNetworkCompData(j).TypeNum;
        RhozNorm = AIRDENSITY(state, StandardP, StandardT, StandardW);
        VisczNorm = 1.71432e-5 + 4.828e-8 * StandardT;

        expn = FlowExpo;
        VisAve = (propN.viscosity + propM.viscosity) / 2.0;
        Tave = (propN.temperature + propM.temperature) / 2.0;
        if (PDROP >= 0.0) {
            coef = FlowCoef / propN.sqrtDensity * Corr;
        } else {
            coef = FlowCoef / propM.sqrtDensity * Corr;
        }

        if (LFLAG) {
            // Initialization by linear relation.
            if (PDROP >= 0.0) {
                RhoCor = TOKELVIN(propN.temperature) / TOKELVIN(Tave);
                Ctl = std::pow(RhozNorm / propN.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
                DF[0] = coef * propN.density / propN.viscosity * Ctl;
            } else {
                RhoCor = TOKELVIN(propM.temperature) / TOKELVIN(Tave);
                Ctl = std::pow(RhozNorm / propM.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
                DF[0] = coef * propM.density / propM.viscosity * Ctl;
            }
            F[0] = -DF[0] * PDROP;
        } else {
            // Standard calculation.
            if (PDROP >= 0.0) {
                // Flow in positive direction.
                // Laminar flow.
                RhoCor = TOKELVIN(propN.temperature) / TOKELVIN(Tave);
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
                RhoCor = TOKELVIN(propM.temperature) / TOKELVIN(Tave);
                Ctl = std::pow(RhozNorm / propM.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
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
            // if (LIST >= 4) gio::write(Unit21, Format_901) << " scr: " << i << PDROP << FL << FT;
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

    int SurfaceCrack::calculate(EnergyPlusData &state,
                                Real64 const PDROP,         // Total pressure drop across a component (P1 - P2) [Pa]
                                const Real64 multiplier,    // Element multiplier
                                const Real64 control,       // Element control signal
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
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine solves airflow for a surface crack component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CDM;
        Real64 FL;
        Real64 FT;
        Real64 RhozNorm;
        Real64 VisczNorm;
        Real64 expn;
        Real64 Ctl;
        Real64 coef;
        // Real64 Corr;
        Real64 VisAve;
        Real64 Tave;
        Real64 RhoCor;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        // Crack standard condition from given inputs
        // if (i > NetworkNumOfLinks - state.dataAirflowNetwork->NumOfLinksIntraZone) {
        //    Corr = 1.0;
        //} else {
        //    Corr = state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor;
        //}
        // CompNum = state.dataAirflowNetwork->AirflowNetworkCompData(j).TypeNum;
        RhozNorm = AIRDENSITY(state, StandardP, StandardT, StandardW);
        VisczNorm = 1.71432e-5 + 4.828e-8 * StandardT;

        expn = FlowExpo;
        VisAve = (propN.viscosity + propM.viscosity) / 2.0;
        Tave = (propN.temperature + propM.temperature) / 2.0;
        if (PDROP >= 0.0) {
            coef = multiplier * control * FlowCoef / propN.sqrtDensity;
        } else {
            coef = multiplier * control * FlowCoef / propM.sqrtDensity;
        }

        // Standard calculation.
        if (PDROP >= 0.0) {
            // Flow in positive direction.
            // Laminar flow.
            RhoCor = TOKELVIN(propN.temperature) / TOKELVIN(Tave);
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
            RhoCor = TOKELVIN(propM.temperature) / TOKELVIN(Tave);
            Ctl = std::pow(RhozNorm / propM.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
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
        // if (LIST >= 4) gio::write(Unit21, Format_901) << " scr: " << i << PDROP << FL << FT;
        if (std::abs(FL) <= std::abs(FT)) {
            F[0] = FL;
            DF[0] = CDM;
        } else {
            F[0] = FT;
            DF[0] = FT * expn / PDROP;
        }
        return 1;
    }

    int DuctLeak::calculate(EnergyPlusData &state,
                            bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                            Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                            [[maybe_unused]] int const i,             // Linkage number
                            [[maybe_unused]] const Real64 multiplier, // Element multiplier
                            [[maybe_unused]] const Real64 control,    // Element control signal
                            const AirProperties &propN,               // Node 1 properties
                            const AirProperties &propM,               // Node 2 properties
                            std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                            std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a power law resistance airflow component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CDM;
        Real64 FL;
        Real64 FT;
        Real64 Ctl;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        // Crack standard condition: T=20C, p=101325 Pa and 0 g/kg
        Real64 RhozNorm = AIRDENSITY(state, 101325.0, 20.0, 0.0);
        Real64 VisczNorm = 1.71432e-5 + 4.828e-8 * 20.0;
        Real64 coef = FlowCoef;

        if (PDROP >= 0.0) {
            coef /= propN.sqrtDensity;
        } else {
            coef /= propM.sqrtDensity;
        }

        if (LFLAG) {
            // Initialization by linear relation.
            if (PDROP >= 0.0) {
                Ctl = std::pow(RhozNorm / propN.density, FlowExpo - 1.0) * std::pow(VisczNorm / propN.viscosity, 2.0 * FlowExpo - 1.0);
                DF[0] = coef * propN.density / propN.viscosity * Ctl;
            } else {
                Ctl = std::pow(RhozNorm / propM.density, FlowExpo - 1.0) * std::pow(VisczNorm / propM.viscosity, 2.0 * FlowExpo - 1.0);
                DF[0] = coef * propM.density / propM.viscosity * Ctl;
            }
            F[0] = -DF[0] * PDROP;
        } else {
            // Standard calculation.
            if (PDROP >= 0.0) {
                // Flow in positive direction for laminar flow.
                Ctl = std::pow(RhozNorm / propN.density, FlowExpo - 1.0) * std::pow(VisczNorm / propN.viscosity, 2.0 * FlowExpo - 1.0);
                CDM = coef * propN.density / propN.viscosity * Ctl;
                FL = CDM * PDROP;
                // Flow in positive direction for turbulent flow.
                if (FlowExpo == 0.5) {
                    FT = coef * propN.sqrtDensity * std::sqrt(PDROP);
                } else {
                    FT = coef * propN.sqrtDensity * std::pow(PDROP, FlowExpo);
                }
            } else {
                // Flow in negative direction for laminar flow
                CDM = coef * propM.density / propM.viscosity;
                FL = CDM * PDROP;
                // Flow in negative direction for turbulent flow
                if (FlowExpo == 0.5) {
                    FT = -coef * propM.sqrtDensity * std::sqrt(-PDROP);
                } else {
                    FT = -coef * propM.sqrtDensity * std::pow(-PDROP, FlowExpo);
                }
            }
            // Select laminar or turbulent flow.
            // if (LIST >= 4) gio::write(Unit21, Format_901) << " plr: " << i << PDROP << FL << FT;
            if (std::abs(FL) <= std::abs(FT)) {
                F[0] = FL;
                DF[0] = CDM;
            } else {
                F[0] = FT;
                DF[0] = FT * FlowExpo / PDROP;
            }
        }
        return 1;
    }

    int DuctLeak::calculate(EnergyPlusData &state,
                            Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                            [[maybe_unused]] const Real64 multiplier, // Element multiplier
                            [[maybe_unused]] const Real64 control,    // Element control signal
                            const AirProperties &propN,               // Node 1 properties
                            const AirProperties &propM,               // Node 2 properties
                            std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                            std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a power law resistance airflow component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CDM;
        Real64 FL;
        Real64 FT;
        Real64 Ctl;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        // Crack standard condition: T=20C, p=101325 Pa and 0 g/kg
        Real64 RhozNorm = AIRDENSITY(state, 101325.0, 20.0, 0.0);
        Real64 VisczNorm = 1.71432e-5 + 4.828e-8 * 20.0;
        Real64 coef = FlowCoef;

        if (PDROP >= 0.0) {
            coef /= propN.sqrtDensity;
        } else {
            coef /= propM.sqrtDensity;
        }

        // Standard calculation.
        if (PDROP >= 0.0) {
            // Flow in positive direction for laminar flow.
            Ctl = std::pow(RhozNorm / propN.density, FlowExpo - 1.0) * std::pow(VisczNorm / propN.viscosity, 2.0 * FlowExpo - 1.0);
            CDM = coef * propN.density / propN.viscosity * Ctl;
            FL = CDM * PDROP;
            // Flow in positive direction for turbulent flow.
            if (FlowExpo == 0.5) {
                FT = coef * propN.sqrtDensity * std::sqrt(PDROP);
            } else {
                FT = coef * propN.sqrtDensity * std::pow(PDROP, FlowExpo);
            }
        } else {
            // Flow in negative direction for laminar flow
            CDM = coef * propM.density / propM.viscosity;
            FL = CDM * PDROP;
            // Flow in negative direction for turbulent flow
            if (FlowExpo == 0.5) {
                FT = -coef * propM.sqrtDensity * std::sqrt(-PDROP);
            } else {
                FT = -coef * propM.sqrtDensity * std::pow(-PDROP, FlowExpo);
            }
        }
        // Select laminar or turbulent flow.
        // if (LIST >= 4) gio::write(Unit21, Format_901) << " plr: " << i << PDROP << FL << FT;
        if (std::abs(FL) <= std::abs(FT)) {
            F[0] = FL;
            DF[0] = CDM;
        } else {
            F[0] = FT;
            DF[0] = FT * FlowExpo / PDROP;
        }
        return 1;
    }

    int ConstantVolumeFan::calculate(EnergyPlusData &state,
                                     bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                                     Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                     int const i,                              // Linkage number
                                     [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                     [[maybe_unused]] const Real64 control,    // Element control signal
                                     const AirProperties &propN,               // Node 1 properties
                                     const AirProperties &propM,               // Node 2 properties
                                     std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                     std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a constant flow rate airflow component -- using standard interface.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataHVACGlobals::FanType_SimpleConstVolume;
        using DataHVACGlobals::FanType_SimpleOnOff;
        using DataHVACGlobals::FanType_SimpleVAV;
        auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const CycFanCycComp(1); // fan cycles with compressor operation

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int k;
        int k1;
        Real64 SumTermFlow;     // Sum of all Terminal flows [kg/s]
        Real64 SumFracSuppLeak; // Sum of all supply leaks as a fraction of supply fan flow rate
        int Node1;
        int Node2;

        int NF(1);

        int AirLoopNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum;

        if (FanTypeNum == FanType_SimpleOnOff) {
            if (state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode == CycFanCycComp &&
                state.dataLoopNodes->Node(InletNode).MassFlowRate == 0.0) {
                NF = GenericDuct(0.1, 0.001, LFLAG, PDROP, propN, propM, F, DF);
            } else if (state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode == CycFanCycComp &&
                       state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopSystemOnMassFlowrate > 0.0) {
                F[0] = state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopSystemOnMassFlowrate;
            } else {
                F[0] = state.dataLoopNodes->Node(InletNode).MassFlowRate * Ctrl;
                if (state.dataAirflowNetwork->MultiSpeedHPIndicator == 2) {
                    F[0] = state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopSystemOnMassFlowrate *
                               state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopCompCycRatio +
                           state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopSystemOffMassFlowrate *
                               (1.0 - state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopCompCycRatio);
                }
            }
        } else if (FanTypeNum == FanType_SimpleConstVolume) {
            if (state.dataLoopNodes->Node(InletNode).MassFlowRate > 0.0) {
                F[0] = FlowRate * Ctrl;
            } else if (NumPrimaryAirSys > 1 && state.dataLoopNodes->Node(InletNode).MassFlowRate <= 0.0) {
                NF = GenericDuct(0.1, 0.001, LFLAG, PDROP, propN, propM, F, DF);
            }

            if (state.dataAirflowNetwork->MultiSpeedHPIndicator == 2) {
                F[0] = state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopSystemOnMassFlowrate;
            }
        } else if (FanTypeNum == FanType_SimpleVAV) {
            // Check VAV termals with a damper
            SumTermFlow = 0.0;
            SumFracSuppLeak = 0.0;
            for (k = 1; k <= state.dataAFNSolver->NetworkNumOfLinks; ++k) {
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(k).VAVTermDamper &&
                    state.dataAirflowNetwork->AirflowNetworkLinkageData(k).AirLoopNum == AirLoopNum) {
                    k1 = state.dataAirflowNetwork->AirflowNetworkNodeData(state.dataAirflowNetwork->AirflowNetworkLinkageData(k).NodeNums[0])
                             .EPlusNodeNum;
                    if (state.dataLoopNodes->Node(k1).MassFlowRate > 0.0) {
                        SumTermFlow += state.dataLoopNodes->Node(k1).MassFlowRate;
                    }
                }
                if (state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(k).CompNum).CompTypeNum ==
                    iComponentTypeNum::ELR) {
                    // Calculate supply leak sensible losses
                    Node1 = state.dataAirflowNetwork->AirflowNetworkLinkageData(k).NodeNums[0];
                    Node2 = state.dataAirflowNetwork->AirflowNetworkLinkageData(k).NodeNums[1];
                    if ((state.dataAirflowNetwork->AirflowNetworkNodeData(Node2).EPlusZoneNum > 0) &&
                        (state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).EPlusNodeNum == 0) &&
                        (state.dataAirflowNetwork->AirflowNetworkNodeData(Node1).AirLoopNum == AirLoopNum)) {
                        SumFracSuppLeak +=
                            state.dataAirflowNetwork
                                ->DisSysCompELRData(
                                    state.dataAirflowNetwork->AirflowNetworkCompData(state.dataAirflowNetwork->AirflowNetworkLinkageData(k).CompNum)
                                        .TypeNum)
                                .ELR;
                    }
                }
            }
            F[0] = SumTermFlow / (1.0 - SumFracSuppLeak);
            state.dataAirflowNetwork->VAVTerminalRatio = 0.0;
            if (F[0] > MaxAirMassFlowRate) {
                state.dataAirflowNetwork->VAVTerminalRatio = MaxAirMassFlowRate / F[0];
                F[0] = MaxAirMassFlowRate;
            }
        }
        DF[0] = 0.0;
        return NF;
    }

    int DetailedFan::calculate(EnergyPlusData &state,
                               bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                               Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                               int const i,                              // Linkage number
                               [[maybe_unused]] const Real64 multiplier, // Element multiplier
                               [[maybe_unused]] const Real64 control,    // Element control signal
                               const AirProperties &propN,               // Node 1 properties
                               const AirProperties &propM,               // Node 2 properties
                               std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                               std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a detailed fan component -- using standard interface.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const TOL(0.00001);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int j;
        int k;
        int L;
        Real64 DPDF;
        Real64 PRISE; // pressure rise (negative of pressure drop) (Pa).
        Real64 BX;
        Real64 BY;
        Real64 CX;
        Real64 CY;
        Real64 CCY;
        Real64 DX;
        Real64 DY;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,5E14.6)");

        int NumCur = n;
        auto &solver = state.dataAFNSolver->solver;
        if (solver.AFECTL(i) <= 0.0) {
            // Speed = 0; treat fan as resistance.
            return GenericCrack(state, FlowCoef, FlowExpo, LFLAG, PDROP, propN, propM, F, DF);
        }
        // Pressure rise at reference fan speed.
        if (solver.AFECTL(i) >= TranRat) {
            PRISE = -PDROP * (RhoAir / propN.density) / pow_2(solver.AFECTL(i));
        } else {
            PRISE = -PDROP * (RhoAir / propN.density) / (TranRat * solver.AFECTL(i));
        }
        // if (LIST >= 4) gio::write(Unit21, Format_901) << " fan:" << i << PDROP << PRISE << AFECTL(i) << DisSysCompDetFanData(CompNum).TranRat;
        if (LFLAG) {
            // Initialization by linear approximation.
            F[0] = -Qfree * solver.AFECTL(i) * (1.0 - PRISE / Pshut);
            DPDF = -Pshut / Qfree;
            // if (LIST >= 4)
            //    gio::write(Unit21, Format_901) << " fni:" << JA << DisSysCompDetFanData(CompNum).Qfree << DisSysCompDetFanData(CompNum).Pshut;
        } else {
            // Solution of the fan performance curve.
            // Determine curve fit range.
            j = 1;
            k = 5 * (j - 1) + 1;
            BX = Coeff(k);
            BY = Coeff(k + 1) + BX * (Coeff(k + 2) + BX * (Coeff(k + 3) + BX * Coeff(k + 4))) - PRISE;
            if (BY < 0.0) ShowFatalError(state, "Out of range, too low in an AirflowNetwork detailed Fan");

            while (true) {
                DX = Coeff(k + 5);
                DY = Coeff(k + 1) + DX * (Coeff(k + 2) + DX * (Coeff(k + 3) + DX * Coeff(k + 5))) - PRISE;
                // if (LIST >= 4) gio::write(Unit21, Format_901) << " fp0:" << j << BX << BY << DX << DY;
                if (BY * DY <= 0.0) break;
                ++j;
                if (j > NumCur) ShowFatalError(state, "Out of range, too high (FAN) in ADS simulation");
                k += 5;
                BX = DX;
                BY = DY;
            }
            // Determine reference mass flow rate by false position method.
            L = 0;
            CY = 0.0;
        Label40:;
            ++L;
            if (L > 100) ShowFatalError(state, "Too many iterations (FAN) in AirflowNtework simulation");
            CCY = CY;
            CX = BX - BY * ((DX - BX) / (DY - BY));
            CY = Coeff(k + 1) + CX * (Coeff(k + 2) + CX * (Coeff(k + 3) + CX * Coeff(k + 4))) - PRISE;
            if (BY * CY == 0.0) goto Label90;
            if (BY * CY > 0.0) goto Label60;
            DX = CX;
            DY = CY;
            if (CY * CCY > 0.0) BY *= 0.5;
            goto Label70;
        Label60:;
            BX = CX;
            BY = CY;
            if (CY * CCY > 0.0) DY *= 0.5;
        Label70:;
            // if (LIST >= 4) gio::write(Unit21, Format_901) << " fpi:" << j << BX << CX << DX << BY << DY;
            if (DX - BX < TOL * CX) goto Label80;
            if (DX - BX < TOL) goto Label80;
            goto Label40;
        Label80:;
            CX = 0.5 * (BX + DX);
        Label90:;
            F[0] = CX;
            DPDF = Coeff(k + 2) + CX * (2.0 * Coeff(k + 3) + CX * 3.0 * Coeff(k + 4));
        }
        // Convert to flow at given speed.
        F[0] *= (propN.density / RhoAir) * solver.AFECTL(i);
        // Set derivative w/r pressure drop (-).
        if (solver.AFECTL(i) >= TranRat) {
            DF[0] = -solver.AFECTL(i) / DPDF;
        } else {
            DF[0] = -1.0 / DPDF;
        }
        return 1;
    }

    int DetailedFan::calculate(EnergyPlusData &state,
                               Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                               [[maybe_unused]] const Real64 multiplier, // Element multiplier
                               const Real64 control,                     // Element control signal
                               const AirProperties &propN,               // Node 1 properties
                               const AirProperties &propM,               // Node 2 properties
                               std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                               std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a detailed fan component -- using standard interface.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const TOL(0.00001);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int j;
        int k;
        int L;
        Real64 DPDF;
        Real64 PRISE; // pressure rise (negative of pressure drop) (Pa).
        Real64 BX;
        Real64 BY;
        Real64 CX;
        Real64 CY;
        Real64 CCY;
        Real64 DX;
        Real64 DY;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,5E14.6)");

        int NumCur = n;

        if (control <= 0.0) {
            // Speed = 0; treat fan as resistance.
            return GenericCrack(state, FlowCoef, FlowExpo, false, PDROP, propN, propM, F, DF);
        }
        // Pressure rise at reference fan speed.
        if (control >= TranRat) {
            PRISE = -PDROP * (RhoAir / propN.density) / pow_2(control);
        } else {
            PRISE = -PDROP * (RhoAir / propN.density) / (TranRat * control);
        }
        // if (LIST >= 4) gio::write(Unit21, Format_901) << " fan:" << i << PDROP << PRISE << AFECTL(i) << DisSysCompDetFanData(CompNum).TranRat;
        // if (LFLAG) {
        //    // Initialization by linear approximation.
        //    F[0] = -Qfree * control * (1.0 - PRISE / Pshut);
        //    DPDF = -Pshut / Qfree;
        //    // if (LIST >= 4)
        //    //    gio::write(Unit21, Format_901) << " fni:" << JA << DisSysCompDetFanData(CompNum).Qfree << DisSysCompDetFanData(CompNum).Pshut;
        //} else {
        // Solution of the fan performance curve.
        // Determine curve fit range.
        j = 1;
        k = 5 * (j - 1) + 1;
        BX = Coeff(k);
        BY = Coeff(k + 1) + BX * (Coeff(k + 2) + BX * (Coeff(k + 3) + BX * Coeff(k + 4))) - PRISE;
        if (BY < 0.0) ShowFatalError(state, "Out of range, too low in an AirflowNetwork detailed Fan");

        while (true) {
            DX = Coeff(k + 5);
            DY = Coeff(k + 1) + DX * (Coeff(k + 2) + DX * (Coeff(k + 3) + DX * Coeff(k + 5))) - PRISE;
            // if (LIST >= 4) gio::write(Unit21, Format_901) << " fp0:" << j << BX << BY << DX << DY;
            if (BY * DY <= 0.0) break;
            ++j;
            if (j > NumCur) ShowFatalError(state, "Out of range, too high (FAN) in ADS simulation");
            k += 5;
            BX = DX;
            BY = DY;
        }
        // Determine reference mass flow rate by false position method.
        L = 0;
        CY = 0.0;
    Label40:;
        ++L;
        if (L > 100) ShowFatalError(state, "Too many iterations (FAN) in AirflowNtework simulation");
        CCY = CY;
        CX = BX - BY * ((DX - BX) / (DY - BY));
        CY = Coeff(k + 1) + CX * (Coeff(k + 2) + CX * (Coeff(k + 3) + CX * Coeff(k + 4))) - PRISE;
        if (BY * CY == 0.0) goto Label90;
        if (BY * CY > 0.0) goto Label60;
        DX = CX;
        DY = CY;
        if (CY * CCY > 0.0) BY *= 0.5;
        goto Label70;
    Label60:;
        BX = CX;
        BY = CY;
        if (CY * CCY > 0.0) DY *= 0.5;
    Label70:;
        // if (LIST >= 4) gio::write(Unit21, Format_901) << " fpi:" << j << BX << CX << DX << BY << DY;
        if (DX - BX < TOL * CX) goto Label80;
        if (DX - BX < TOL) goto Label80;
        goto Label40;
    Label80:;
        CX = 0.5 * (BX + DX);
    Label90:;
        F[0] = CX;
        DPDF = Coeff(k + 2) + CX * (2.0 * Coeff(k + 3) + CX * 3.0 * Coeff(k + 4));

        // Convert to flow at given speed.
        F[0] *= (propN.density / RhoAir) * control;
        // Set derivative w/r pressure drop (-).
        if (control >= TranRat) {
            DF[0] = -control / DPDF;
        } else {
            DF[0] = -1.0 / DPDF;
        }
        return 1;
    }

    int Damper::calculate([[maybe_unused]] EnergyPlusData &state,
                          bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                          Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                          int const i,                              // Linkage number
                          [[maybe_unused]] const Real64 multiplier, // Element multiplier
                          [[maybe_unused]] const Real64 control,    // Element control signal
                          const AirProperties &propN,               // Node 1 properties
                          const AirProperties &propM,               // Node 2 properties
                          std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                          std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a Controlled power law resistance airflow component (damper)

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 C;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        auto &solver = state.dataAFNSolver->solver;
        C = solver.AFECTL(i);
        if (C < FlowMin) C = FlowMin;
        if (C > FlowMax) C = FlowMax;
        C = A0 + C * (A1 + C * (A2 + C * A3));
        // if (LIST >= 4)
        //    gio::write(Unit21, Format_901) << " Dmp:" << i << AFECTL(i) << DisSysCompDamperData(CompNum).FlowMin
        //                                   << DisSysCompDamperData(CompNum).FlowMax << C;
        if (LFLAG || std::abs(PDROP) <= LTP) {
            //                              Laminar flow.
            if (PDROP >= 0.0) {
                DF[0] = C * LamFlow * propN.density / propN.viscosity;
            } else {
                DF[0] = C * LamFlow * propM.density / propM.viscosity;
            }
            F[0] = DF[0] * PDROP;
        } else {
            //                              Turbulent flow.
            if (PDROP >= 0.0) {
                F[0] = C * TurFlow * propN.sqrtDensity * std::pow(PDROP, FlowExpo);
            } else {
                F[0] = -C * TurFlow * propM.sqrtDensity * std::pow(-PDROP, FlowExpo);
            }
            DF[0] = F[0] * FlowExpo / PDROP;
        }
        return 1;
    }

    int Damper::calculate([[maybe_unused]] EnergyPlusData &state,
                          const Real64 PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                          [[maybe_unused]] const Real64 multiplier, // Element multiplier
                          const Real64 control,                     // Element control signal
                          const AirProperties &propN,               // Node 1 properties
                          const AirProperties &propM,               // Node 2 properties
                          std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                          std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a Controlled power law resistance airflow component (damper)

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 C;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        C = control;
        if (C < FlowMin) C = FlowMin;
        if (C > FlowMax) C = FlowMax;
        C = A0 + C * (A1 + C * (A2 + C * A3));
        // if (LIST >= 4)
        //    gio::write(Unit21, Format_901) << " Dmp:" << i << AFECTL(i) << DisSysCompDamperData(CompNum).FlowMin
        //                                   << DisSysCompDamperData(CompNum).FlowMax << C;
        if (std::abs(PDROP) <= LTP) {
            //                              Laminar flow.
            if (PDROP >= 0.0) {
                DF[0] = C * LamFlow * propN.density / propN.viscosity;
            } else {
                DF[0] = C * LamFlow * propM.density / propM.viscosity;
            }
            F[0] = DF[0] * PDROP;
        } else {
            //                              Turbulent flow.
            if (PDROP >= 0.0) {
                F[0] = C * TurFlow * propN.sqrtDensity * std::pow(PDROP, FlowExpo);
            } else {
                F[0] = -C * TurFlow * propM.sqrtDensity * std::pow(-PDROP, FlowExpo);
            }
            DF[0] = F[0] * FlowExpo / PDROP;
        }
        return 1;
    }

    int EffectiveLeakageRatio::calculate([[maybe_unused]] EnergyPlusData &state,
                                         bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                                         Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                         [[maybe_unused]] int const i,             // Linkage number
                                         [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                         [[maybe_unused]] const Real64 control,    // Element control signal
                                         const AirProperties &propN,               // Node 1 properties
                                         const AirProperties &propM,               // Node 2 properties
                                         std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                         std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a Effective leakage ratio component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CDM;
        Real64 FL;
        Real64 FT;
        Real64 FlowCoef;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        // Get component properties
        FlowCoef = ELR * FlowRate / propN.density * std::pow(RefPres, -FlowExpo);

        if (LFLAG) {
            // Initialization by linear relation.
            if (PDROP >= 0.0) {
                DF[0] = FlowCoef * propN.density / propN.viscosity;
            } else {
                DF[0] = FlowCoef * propM.density / propM.viscosity;
            }
            F[0] = -DF[0] * PDROP;
        } else {
            // Standard calculation.
            if (PDROP >= 0.0) {
                // Flow in positive direction.
                // Laminar flow.
                CDM = FlowCoef * propN.density / propN.viscosity;
                FL = CDM * PDROP;
                // Turbulent flow.
                if (FlowExpo == 0.5) {
                    FT = FlowCoef * propN.sqrtDensity * std::sqrt(PDROP);
                } else {
                    FT = FlowCoef * propN.sqrtDensity * std::pow(PDROP, FlowExpo);
                }
            } else {
                // Flow in negative direction.
                // Laminar flow.
                CDM = FlowCoef * propM.density / propM.viscosity;
                FL = CDM * PDROP;
                // Turbulent flow.
                if (FlowExpo == 0.5) {
                    FT = -FlowCoef * propM.sqrtDensity * std::sqrt(-PDROP);
                } else {
                    FT = -FlowCoef * propM.sqrtDensity * std::pow(-PDROP, FlowExpo);
                }
            }
            // Select laminar or turbulent flow.
            // if (LIST >= 4) gio::write(Unit21, Format_901) << " plr: " << i << PDROP << FL << FT;
            if (std::abs(FL) <= std::abs(FT)) {
                F[0] = FL;
                DF[0] = CDM;
            } else {
                F[0] = FT;
                DF[0] = FT * FlowExpo / PDROP;
            }
        }
        return 1;
    }

    int EffectiveLeakageRatio::calculate([[maybe_unused]] EnergyPlusData &state,
                                         Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                         [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                         [[maybe_unused]] const Real64 control,    // Element control signal
                                         const AirProperties &propN,               // Node 1 properties
                                         const AirProperties &propM,               // Node 2 properties
                                         std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                         std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a Effective leakage ratio component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CDM;
        Real64 FL;
        Real64 FT;
        Real64 FlowCoef;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        // Get component properties
        FlowCoef = ELR * FlowRate / propN.density * std::pow(RefPres, -FlowExpo);

        // Standard calculation.
        if (PDROP >= 0.0) {
            // Flow in positive direction.
            // Laminar flow.
            CDM = FlowCoef * propN.density / propN.viscosity;
            FL = CDM * PDROP;
            // Turbulent flow.
            if (FlowExpo == 0.5) {
                FT = FlowCoef * propN.sqrtDensity * std::sqrt(PDROP);
            } else {
                FT = FlowCoef * propN.sqrtDensity * std::pow(PDROP, FlowExpo);
            }
        } else {
            // Flow in negative direction.
            // Laminar flow.
            CDM = FlowCoef * propM.density / propM.viscosity;
            FL = CDM * PDROP;
            // Turbulent flow.
            if (FlowExpo == 0.5) {
                FT = -FlowCoef * propM.sqrtDensity * std::sqrt(-PDROP);
            } else {
                FT = -FlowCoef * propM.sqrtDensity * std::pow(-PDROP, FlowExpo);
            }
        }
        // Select laminar or turbulent flow.
        // if (LIST >= 4) gio::write(Unit21, Format_901) << " plr: " << i << PDROP << FL << FT;
        if (std::abs(FL) <= std::abs(FT)) {
            F[0] = FL;
            DF[0] = CDM;
        } else {
            F[0] = FT;
            DF[0] = FT * FlowExpo / PDROP;
        }
        return 1;
    }

    int DetailedOpening::calculate(EnergyPlusData &state,
                                   [[maybe_unused]] bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                                   Real64 const PDROP,                          // Total pressure drop across a component (P1 - P2) [Pa]
                                   int const IL,                                // Linkage number
                                   [[maybe_unused]] const Real64 multiplier,    // Element multiplier
                                   [[maybe_unused]] const Real64 control,       // Element control signal
                                   [[maybe_unused]] const AirProperties &propN, // Node 1 properties
                                   [[maybe_unused]] const AirProperties &propM, // Node 2 properties
                                   std::array<Real64, 2> &F,                    // Airflow through the component [kg/s]
                                   std::array<Real64, 2> &DF                    // Partial derivative:  DF/DP
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Oct. 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  This subroutine is revised based on a vertical large opening subroutine from COMIS

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates airflow and pressure of a detailed large opening component.

        // METHODOLOGY EMPLOYED:
        // Purpose:  This routine calculates the massflow and its derivative
        //       through a large opening in both flow directions. As input
        //       the density profiles RhoProfF/T are required aswell as the
        //       effective pressure difference profile DpProfNew, which is the
        //       sum of the stack pressure difference profile DpProf and the
        //       difference of the actual pressures at reference height. The
        //       profiles are calculated in the routine PresProfile.
        //       The massflow and its derivative are calculated for each
        //       interval representing a step of the pressure difference
        //       profile. The total flow and derivative are obtained by
        //       summation over the whole opening.
        //       The calculation is split into different cases representing
        //       different situations of the opening:
        //       - closed opening (opening factor = 0): summation of top and
        //         bottom crack (crack length = lwmax) plus "integration" over
        //         a vertically distributed crack of length (2*lhmax+lextra).
        //       - type 1: normal rectangular opening: "integration" over NrInt
        //         openings with width actlw and height actlh/NrInt
        //       - type 2: horizontally pivoted window: flow direction assumed
        //         strictly perpendicular to the plane of the opening
        //         -> "integration" over normal rectangular openings at top
        //         and bottom of LO plus a rectangular opening in series with two
        //         triangular openings in the middle of the LO (most general
        //         situation). The geometry is defined by the input parameters
        //         actlw(=lwmax), actlh, axisheight, opening angle.
        //       Assuming the massflow perpendicular to the opening plane in all
        //       cases the ownheightfactor has no influence on the massflow.

        // REFERENCES:
        // Helmut E. Feustel and Alison Rayner-Hooson, "COMIS Fundamentals," LBL-28560,
        // Lawrence Berkeley National Laboratory, Berkeley, CA, May 1990

        // USE STATEMENTS:
        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const RealMin(1e-37);
        static Real64 const sqrt_1_2(std::sqrt(1.2));

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static Real64 const sqrt_2(std::sqrt(2.0));

        Real64 Width;
        Real64 Height;

        Real64 fma12;                         // massflow in direction "from-to" [kg/s]
        Real64 fma21;                         // massflow in direction "to-from" [kg/s]
        Real64 dp1fma12;                      // derivative d fma12 / d Dp [kg/s/Pa]
        Real64 dp1fma21;                      // derivative d fma21 / d Dp [kg/s/Pa]
        Array1D<Real64> DpProfNew(NrInt + 2); // Differential pressure profile for Large Openings, taking into account fixed
        // pressures and actual zone pressures at reference height
        Real64 Fact;   // Actual opening factor
        Real64 DifLim; // Limit for the pressure difference where laminarization takes place [Pa]
        Real64 Cfact;
        Real64 FvVeloc;

        Real64 ActLh;
        Real64 ActLw;
        Real64 Lextra;
        Real64 Axishght;
        Real64 ActCD;
        Real64 Cs;
        Real64 expn;
        Real64 Type;
        Real64 Interval;
        Real64 fmasum;
        Real64 dfmasum;
        Real64 Prefact;
        Array1D<Real64> EvalHghts(NrInt + 2);
        Real64 h2;
        Real64 h4;
        Real64 alpha;
        Real64 rholink;
        Real64 c1;
        Real64 c2;
        Real64 DpZeroOffset;
        Real64 area;
        Real64 WFact;
        Real64 HFact;
        int i;
        int Loc;
        int iNum;

        // Get component properties
        DifLim = 1.0e-4;
        Width = state.dataAirflowNetwork->MultizoneSurfaceData(IL).Width;
        Height = state.dataAirflowNetwork->MultizoneSurfaceData(IL).Height;
        Fact = state.dataAirflowNetwork->MultizoneSurfaceData(IL).OpenFactor;
        Loc = (state.dataAirflowNetwork->AirflowNetworkLinkageData(IL).DetOpenNum - 1) * (NrInt + 2);
        iNum = NumFac;
        ActCD = 0.0;

        if (iNum == 2) {
            if (Fact <= OpenFac2) {
                WFact = WidthFac1 + (Fact - OpenFac1) / (OpenFac2 - OpenFac1) * (WidthFac2 - WidthFac1);
                HFact = HeightFac1 + (Fact - OpenFac1) / (OpenFac2 - OpenFac1) * (HeightFac2 - HeightFac1);
                Cfact = DischCoeff1 + (Fact - OpenFac1) / (OpenFac2 - OpenFac1) * (DischCoeff2 - DischCoeff1);
            } else {
                ShowFatalError(
                    state,
                    "Open Factor is above the maximum input range for opening factors in AirflowNetwork:MultiZone:Component:DetailedOpening = " +
                        name);
            }
        }

        if (iNum == 3) {
            if (Fact <= OpenFac2) {
                WFact = WidthFac1 + (Fact - OpenFac1) / (OpenFac2 - OpenFac1) * (WidthFac2 - WidthFac1);
                HFact = HeightFac1 + (Fact - OpenFac1) / (OpenFac2 - OpenFac1) * (HeightFac2 - HeightFac1);
                Cfact = DischCoeff1 + (Fact - OpenFac1) / (OpenFac2 - OpenFac1) * (DischCoeff2 - DischCoeff1);
            } else if (Fact <= OpenFac3) {
                WFact = WidthFac2 + (Fact - OpenFac2) / (OpenFac3 - OpenFac2) * (WidthFac3 - WidthFac2);
                HFact = HeightFac2 + (Fact - OpenFac2) / (OpenFac3 - OpenFac2) * (HeightFac3 - HeightFac2);
                Cfact = DischCoeff2 + (Fact - OpenFac2) / (OpenFac3 - OpenFac2) * (DischCoeff3 - DischCoeff2);
            } else {
                ShowFatalError(
                    state,
                    "Open Factor is above the maximum input range for opening factors in AirflowNetwork:MultiZone:Component:DetailedOpening = " +
                        name);
            }
        }

        if (iNum == 4) {
            if (Fact <= OpenFac2) {
                WFact = WidthFac1 + (Fact - OpenFac1) / (OpenFac2 - OpenFac1) * (WidthFac2 - WidthFac1);
                HFact = HeightFac1 + (Fact - OpenFac1) / (OpenFac2 - OpenFac1) * (HeightFac2 - HeightFac1);
                Cfact = DischCoeff1 + (Fact - OpenFac1) / (OpenFac2 - OpenFac1) * (DischCoeff2 - DischCoeff1);
            } else if (Fact <= OpenFac3) {
                WFact = WidthFac2 + (Fact - OpenFac2) / (OpenFac3 - OpenFac2) * (WidthFac3 - WidthFac2);
                HFact = HeightFac2 + (Fact - OpenFac2) / (OpenFac3 - OpenFac2) * (HeightFac3 - HeightFac2);
                Cfact = DischCoeff2 + (Fact - OpenFac2) / (OpenFac3 - OpenFac2) * (DischCoeff3 - DischCoeff2);
            } else if (Fact <= OpenFac4) {
                WFact = WidthFac3 + (Fact - OpenFac3) / (OpenFac4 - OpenFac3) * (WidthFac4 - WidthFac3);
                HFact = HeightFac3 + (Fact - OpenFac3) / (OpenFac4 - OpenFac3) * (HeightFac4 - HeightFac3);
                Cfact = DischCoeff3 + (Fact - OpenFac3) / (OpenFac4 - OpenFac3) * (DischCoeff4 - DischCoeff3);
            } else {
                ShowFatalError(
                    state,
                    "Open Factor is above the maximum input range for opening factors in AirflowNetwork:MultiZone:Component:DetailedOpening = " +
                        name);
            }
        }

        // calculate DpProfNew
        for (i = 1; i <= NrInt + 2; ++i) {
            DpProfNew(i) = PDROP + state.dataAFNSolver->DpProf(Loc + i) - state.dataAFNSolver->DpL(IL, 1);
        }

        // Get opening data based on the opening factor
        if (Fact == 0) {
            ActLw = state.dataAirflowNetwork->MultizoneSurfaceData(IL).Width;
            ActLh = state.dataAirflowNetwork->MultizoneSurfaceData(IL).Height;
            Cfact = 0.0;
        } else {
            ActLw = state.dataAirflowNetwork->MultizoneSurfaceData(IL).Width * WFact;
            ActLh = state.dataAirflowNetwork->MultizoneSurfaceData(IL).Height * HFact;
            ActCD = Cfact;
        }

        Cs = FlowCoef;
        expn = FlowExpo;
        Type = LVOType;
        if (Type == 1) {
            Lextra = LVOValue;
            Axishght = 0.0;
        } else if (Type == 2) {
            Lextra = 0.0;
            Axishght = LVOValue;
            ActLw = state.dataAirflowNetwork->MultizoneSurfaceData(IL).Width;
            ActLh = state.dataAirflowNetwork->MultizoneSurfaceData(IL).Height;
        }

        // Add window multiplier with window close
        if (state.dataAirflowNetwork->MultizoneSurfaceData(IL).Multiplier > 1.0) Cs *= state.dataAirflowNetwork->MultizoneSurfaceData(IL).Multiplier;
        // Add window multiplier with window open
        if (Fact > 0.0) {
            if (state.dataAirflowNetwork->MultizoneSurfaceData(IL).Multiplier > 1.0)
                ActLw *= state.dataAirflowNetwork->MultizoneSurfaceData(IL).Multiplier;
        }

        // Add recurring warnings
        if (Fact > 0.0) {
            if (ActLw == 0.0) {
                ++WidthErrCount;
                if (WidthErrCount < 2) {
                    ShowWarningError(state, "The actual width of the AirflowNetwork:MultiZone:Component:DetailedOpening of " + name + " is 0.");
                    ShowContinueError(state, "The actual width is set to 1.0E-6 m.");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "The actual width of the AirflowNetwork:MultiZone:Component:DetailedOpening of " + name +
                                                       " is 0 error continues.",
                                                   WidthErrIndex,
                                                   ActLw,
                                                   ActLw);
                }
                ActLw = 1.0e-6;
            }
            if (ActLh == 0.0) {
                ++HeightErrCount;
                if (HeightErrCount < 2) {
                    ShowWarningError(state, "The actual height of the AirflowNetwork:MultiZone:Component:DetailedOpening of " + name + " is 0.");
                    ShowContinueError(state, "The actual height is set to 1.0E-6 m.");
                    ShowContinueErrorTimeStamp(state, "Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "The actual width of the AirflowNetwork:MultiZone:Component:DetailedOpening of " + name +
                                                       " is 0 error continues.",
                                                   HeightErrIndex,
                                                   ActLh,
                                                   ActLh);
                }
                ActLh = 1.0e-6;
            }
        }
        // Initialization:
        int NF(1);
        Interval = ActLh / NrInt;
        fma12 = 0.0;
        fma21 = 0.0;
        dp1fma12 = 0.0;
        dp1fma21 = 0.0;

        // Closed LO
        if (Cfact == 0) {
            DpZeroOffset = DifLim;
            // bottom crack
            if (DpProfNew(1) > 0) {
                if (std::abs(DpProfNew(1)) <= DpZeroOffset) {
                    dfmasum = Cs * ActLw * std::pow(DpZeroOffset, expn) / DpZeroOffset;
                    fmasum = DpProfNew(1) * dfmasum;
                } else {
                    fmasum = Cs * ActLw * std::pow(DpProfNew(1), expn);
                    dfmasum = fmasum * expn / DpProfNew(1);
                }
                fma12 += fmasum;
                dp1fma12 += dfmasum;
            } else {
                if (std::abs(DpProfNew(1)) <= DpZeroOffset) {
                    dfmasum = -Cs * ActLw * std::pow(DpZeroOffset, expn) / DpZeroOffset;
                    fmasum = DpProfNew(1) * dfmasum;
                } else {
                    fmasum = Cs * ActLw * std::pow(-DpProfNew(1), expn);
                    dfmasum = fmasum * expn / DpProfNew(1);
                }
                fma21 += fmasum;
                dp1fma21 += dfmasum;
            }
            // top crack
            if (DpProfNew(NrInt + 2) > 0) {
                if (std::abs(DpProfNew(NrInt + 2)) <= DpZeroOffset) {
                    dfmasum = Cs * ActLw * std::pow(DpZeroOffset, expn) / DpZeroOffset;
                    fmasum = DpProfNew(NrInt + 2) * dfmasum;
                } else {
                    fmasum = Cs * ActLw * std::pow(DpProfNew(NrInt + 2), expn);
                    dfmasum = fmasum * expn / DpProfNew(NrInt + 2);
                }
                fma12 += fmasum;
                dp1fma12 += dfmasum;
            } else {
                if (std::abs(DpProfNew(NrInt + 2)) <= DpZeroOffset) {
                    dfmasum = -Cs * ActLw * std::pow(DpZeroOffset, expn) / DpZeroOffset;
                    fmasum = DpProfNew(NrInt + 2) * dfmasum;
                } else {
                    fmasum = Cs * ActLw * std::pow(-DpProfNew(NrInt + 2), expn);
                    dfmasum = fmasum * expn / DpProfNew(NrInt + 2);
                }
                fma21 += fmasum;
                dp1fma21 += dfmasum;
            }
            // side and extra cracks
            Prefact = Interval * (2 + Lextra / ActLh) * Cs;
            for (i = 2; i <= NrInt + 1; ++i) {
                if (DpProfNew(i) > 0) {
                    if (std::abs(DpProfNew(i)) <= DpZeroOffset) {
                        dfmasum = Prefact * std::pow(DpZeroOffset, expn) / DpZeroOffset;
                        fmasum = DpProfNew(i) * dfmasum;
                    } else {
                        fmasum = Prefact * std::pow(DpProfNew(i), expn);
                        dfmasum = fmasum * expn / DpProfNew(i);
                    }
                    fma12 += fmasum;
                    dp1fma12 += dfmasum;
                } else {
                    if (std::abs(DpProfNew(i)) <= DpZeroOffset) {
                        dfmasum = -Prefact * std::pow(DpZeroOffset, expn) / DpZeroOffset;
                        fmasum = DpProfNew(i) * dfmasum;
                    } else {
                        fmasum = Prefact * std::pow(-DpProfNew(i), expn);
                        dfmasum = fmasum * expn / DpProfNew(i);
                    }
                    fma21 += fmasum;
                    dp1fma21 += dfmasum;
                }
            }
        }

        // Open LO, type 1
        if ((Cfact != 0) && (Type == 1)) {
            DpZeroOffset = DifLim * 1e-3;
            Prefact = ActLw * ActCD * Interval * sqrt_2;
            for (i = 2; i <= NrInt + 1; ++i) {
                if (DpProfNew(i) > 0) {
                    if (std::abs(DpProfNew(i)) <= DpZeroOffset) {
                        dfmasum = std::sqrt(state.dataAFNSolver->RhoProfF(Loc + i) * DpZeroOffset) / DpZeroOffset;
                        fmasum = DpProfNew(i) * dfmasum;
                    } else {
                        fmasum = std::sqrt(state.dataAFNSolver->RhoProfF(Loc + i) * DpProfNew(i));
                        dfmasum = 0.5 * fmasum / DpProfNew(i);
                    }
                    fma12 += fmasum;
                    dp1fma12 += dfmasum;
                } else {
                    if (std::abs(DpProfNew(i)) <= DpZeroOffset) {
                        dfmasum = -std::sqrt(state.dataAFNSolver->RhoProfT(Loc + i) * DpZeroOffset) / DpZeroOffset;
                        fmasum = DpProfNew(i) * dfmasum;
                    } else {
                        fmasum = std::sqrt(-state.dataAFNSolver->RhoProfT(Loc + i) * DpProfNew(i));
                        dfmasum = 0.5 * fmasum / DpProfNew(i);
                    }
                    fma21 += fmasum;
                    dp1fma21 += dfmasum;
                }
            }

            fma12 *= Prefact;
            fma21 *= Prefact;
            dp1fma12 *= Prefact;
            dp1fma21 *= Prefact;
        }

        // Open LO, type 2
        if ((Cfact != 0) && (Type == 2)) {
            // Initialization
            DpZeroOffset = DifLim * 1e-3;
            // New definition for opening factors for LVO type 2: opening angle = 90 degrees --> opening factor = 1.0
            // should be PIOvr2 in below?
            alpha = Fact * DataGlobalConstants::PiOvr2;
            Real64 const cos_alpha(std::cos(alpha));
            Real64 const tan_alpha(std::tan(alpha));
            h2 = Axishght * (1.0 - cos_alpha);
            h4 = Axishght + (ActLh - Axishght) * cos_alpha;
            EvalHghts(1) = 0.0;
            EvalHghts(NrInt + 2) = ActLh;
            // New definition for opening factors for LVO type 2: pening angle = 90 degrees --> opening factor = 1.0
            if (Fact == 1.0) {
                h2 = Axishght;
                h4 = Axishght;
            }

            for (i = 2; i <= NrInt + 1; ++i) {
                EvalHghts(i) = Interval * (i - 1.5);
            }

            // Calculation of massflow and its derivative
            for (i = 2; i <= NrInt + 1; ++i) {
                if (DpProfNew(i) > 0) {
                    rholink = state.dataAFNSolver->RhoProfF(Loc + i);
                } else {
                    rholink = state.dataAFNSolver->RhoProfT(Loc + i);
                }

                if ((EvalHghts(i) <= h2) || (EvalHghts(i) >= h4)) {
                    if (std::abs(DpProfNew(i)) <= DpZeroOffset) {
                        dfmasum = ActCD * ActLw * Interval * std::sqrt(2.0 * rholink * DpZeroOffset) / DpZeroOffset * sign(1, DpProfNew(i));
                        fmasum = DpProfNew(i) * dfmasum;
                    } else {
                        fmasum = ActCD * ActLw * Interval * std::sqrt(2.0 * rholink * std::abs(DpProfNew(i)));
                        dfmasum = 0.5 * fmasum / DpProfNew(i);
                    }
                } else {
                    // triangular opening at the side of LO
                    c1 = ActCD * ActLw * Interval * std::sqrt(2.0 * rholink);
                    c2 = 2 * ActCD * std::abs(Axishght - EvalHghts(i)) * tan_alpha * Interval * std::sqrt(2.0 * rholink);
                    if ((c1 != 0) && (c2 != 0)) {
                        if (std::abs(DpProfNew(i)) <= DpZeroOffset) {
                            dfmasum = std::sqrt(DpZeroOffset / (1 / c1 / c1 + 1 / c2 / c2)) / DpZeroOffset * sign(1, DpProfNew(i));
                            fmasum = DpProfNew(i) * dfmasum;
                        } else {
                            fmasum = std::sqrt(std::abs(DpProfNew(i)) / (1 / c1 / c1 + 1 / c2 / c2));
                            dfmasum = 0.5 * fmasum / DpProfNew(i);
                        }
                    } else {
                        fmasum = 0.0;
                        dfmasum = 0.0;
                    }
                }

                if (DpProfNew(i) > 0) {
                    fma12 += fmasum;
                    dp1fma12 += dfmasum;
                } else {
                    fma21 += fmasum;
                    dp1fma21 += dfmasum;
                }
            }
        }

        // Calculate some velocity in the large opening
        area = ActLh * ActLw * ActCD;
        if (area > (Cs + RealMin)) {
            if (area > RealMin) {
                FvVeloc = (fma21 + fma12) / area;
            } else {
                FvVeloc = 0.0;
            }
        } else {
            // here the average velocity over the full area, may blow half in half out.
            // velocity= Fva/Nett area=Fma/Rho/(Cm/( (2**N)* SQRT(1.2) ) )
            if (Cs > 0.0) {
                // get the average Rho for this closed window
                for (i = 2; i <= NrInt + 1; ++i) {
                    rholink = 0.0;
                    if (DpProfNew(i) > 0) {
                        rholink = state.dataAFNSolver->RhoProfF(Loc + i);
                    } else {
                        rholink = state.dataAFNSolver->RhoProfT(Loc + i);
                    }
                    rholink /= NrInt;
                    rholink = 1.2;
                }
                FvVeloc = (fma21 + fma12) * std::pow(2.0, expn) * sqrt_1_2 / (rholink * Cs);
            } else {
                FvVeloc = 0.0;
            }
        }

        // Output mass flow rates and associated derivatives
        F[0] = fma12 - fma21;
        DF[0] = dp1fma12 - dp1fma21;
        F[1] = 0.0;
        if (fma12 != 0.0 && fma21 != 0.0) {
            F[1] = fma21;
        }
        DF[1] = 0.0;
        return NF;
    }

    int SimpleOpening::calculate(EnergyPlusData &state,
                                 bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                                 Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                 int const i,                              // Linkage number
                                 [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                 [[maybe_unused]] const Real64 control,    // Element control signal
                                 const AirProperties &propN,               // Node 1 properties
                                 const AirProperties &propM,               // Node 2 properties
                                 std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                 std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a Doorway airflow component using standard interface.
        // A doorway may have two-way airflows. Heights measured relative to the bottom of the door.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const SQRT2(1.414213562373095);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 DPMID; // pressure drop at mid-height of doorway.
        Real64 C;
        Real64 DF0;  // derivative factor at the bottom of the door.
        Real64 DFH;  // derivative factor at the top of the door.
        Real64 DRHO; // difference in air densities between rooms.
        Real64 GDRHO;
        Real64 F0; // flow factor at the bottom of the door.
        Real64 FH; // flow factor at the top of the door.
        Real64 Y;  // height of neutral plane rel. to bottom of door (m).
        Real64 coeff;
        Real64 Width;
        Real64 Height;
        Real64 OpenFactor;

        int NF(1);

        // Formats
        // static gio::Fmt Format_900("(A5,9X,4E16.7)");
        // static gio::Fmt Format_903("(A5,3I3,4E16.7)");

        Width = state.dataAirflowNetwork->MultizoneSurfaceData(i).Width;
        Height = state.dataAirflowNetwork->MultizoneSurfaceData(i).Height;
        coeff = FlowCoef * 2.0 * (Width + Height);
        OpenFactor = state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor;
        if (OpenFactor > 0.0) {
            Width *= OpenFactor;
            if (state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).Tilt < 90.0) {
                Height *= state.dataSurface->Surface(state.dataAirflowNetwork->MultizoneSurfaceData(i).SurfNum).SinTilt;
            }
        }

        if (PDROP >= 0.0) {
            coeff /= propN.sqrtDensity;
        } else {
            coeff /= propM.sqrtDensity;
        }

        // Add window multiplier with window close
        if (state.dataAirflowNetwork->MultizoneSurfaceData(i).Multiplier > 1.0) coeff *= state.dataAirflowNetwork->MultizoneSurfaceData(i).Multiplier;
        // Add window multiplier with window open
        if (OpenFactor > 0.0) {
            if (state.dataAirflowNetwork->MultizoneSurfaceData(i).Multiplier > 1.0)
                Width *= state.dataAirflowNetwork->MultizoneSurfaceData(i).Multiplier;
        }

        DRHO = propN.density - propM.density;
        GDRHO = 9.8 * DRHO;
        // if (LIST >= 4) gio::write(Unit21, Format_903) << " DOR:" << i << n << m << PDROP << std::abs(DRHO) << MinRhoDiff;
        if (OpenFactor == 0.0) {
            return GenericCrack(state, coeff, FlowExpo, LFLAG, PDROP, propN, propM, F, DF);
        }
        if (std::abs(DRHO) < MinRhoDiff || LFLAG) {
            DPMID = PDROP - 0.5 * Height * GDRHO;
            // Initialization or identical temps: treat as one-way flow.
            NF = GenericCrack(state, coeff, FlowExpo, LFLAG, DPMID, propN, propM, F, DF);
            // if (LIST >= 4) gio::write(Unit21, Format_900) << " Drs:" << DPMID << F[0] << DF[0];
        } else {
            // Possible two-way flow:
            Y = PDROP / GDRHO;
            // if (LIST >= 4) gio::write(Unit21, Format_900) << " DrY:" << PDROP << GDRHO << Y;
            // F0 = lower flow, FH = upper flow.
            C = SQRT2 * Width * DischCoeff;
            DF0 = C * std::sqrt(std::abs(PDROP)) / std::abs(GDRHO);
            //        F0 = 0.666667d0*C*SQRT(ABS(GDRHO*Y))*ABS(Y)
            F0 = (2.0 / 3.0) * C * std::sqrt(std::abs(GDRHO * Y)) * std::abs(Y);
            DFH = C * std::sqrt(std::abs((Height - Y) / GDRHO));
            //        FH = 0.666667d0*DFH*ABS(GDRHO*(Height-Y))
            FH = (2.0 / 3.0) * DFH * std::abs(GDRHO * (Height - Y));
            // if (LIST >= 4) gio::write(Unit21, Format_900) << " DrF:" << F0 << DF0 << FH << DFH;
            if (Y <= 0.0) {
                // One-way flow (negative).
                if (DRHO >= 0.0) {
                    F[0] = -propM.sqrtDensity * std::abs(FH - F0);
                    DF[0] = propM.sqrtDensity * std::abs(DFH - DF0);
                } else {
                    F[0] = propN.sqrtDensity * std::abs(FH - F0);
                    DF[0] = propN.sqrtDensity * std::abs(DFH - DF0);
                }
                // if (LIST >= 4) gio::write(Unit21, Format_900) << " Dr1:" << C << F[0] << DF[0];
            } else if (Y >= Height) {
                // One-way flow (positive).
                if (DRHO >= 0.0) {
                    F[0] = propN.sqrtDensity * std::abs(FH - F0);
                    DF[0] = propN.sqrtDensity * std::abs(DFH - DF0);
                } else {
                    F[0] = -propM.sqrtDensity * std::abs(FH - F0);
                    DF[0] = propM.sqrtDensity * std::abs(DFH - DF0);
                }
                // if (LIST >= 4) gio::write(Unit21, Format_900) << " Dr2:" << C << F[0] << DF[0];
            } else {
                // Two-way flow.
                NF = 2;
                if (DRHO >= 0.0) {
                    F[0] = -propM.sqrtDensity * FH;
                    DF[0] = propM.sqrtDensity * DFH;
                    F[1] = propN.sqrtDensity * F0;
                    DF[1] = propN.sqrtDensity * DF0;
                } else {
                    F[0] = propN.sqrtDensity * FH;
                    DF[0] = propN.sqrtDensity * DFH;
                    F[1] = -propM.sqrtDensity * F0;
                    DF[1] = propM.sqrtDensity * DF0;
                }
                // if (LIST >= 4) gio::write(Unit21, Format_900) << " Dr3:" << C << F[0] << DF[0];
                // if (LIST >= 4) gio::write(Unit21, Format_900) << " Dr4:" << C << F[1] << DF[1];
            }
        }
        return NF;
    }

    int ConstantPressureDrop::calculate([[maybe_unused]] EnergyPlusData &state,
                                        [[maybe_unused]] bool const LFLAG,           // Initialization flag.If = 1, use laminar relationship
                                        const Real64 PDROP,                          // Total pressure drop across a component (P1 - P2) [Pa]
                                        int const i,                                 // Linkage number
                                        [[maybe_unused]] const Real64 multiplier,    // Element multiplier
                                        [[maybe_unused]] const Real64 control,       // Element control signal
                                        const AirProperties &propN,                  // Node 1 properties
                                        [[maybe_unused]] const AirProperties &propM, // Node 2 properties
                                        std::array<Real64, 2> &F,                    // Airflow through the component [kg/s]
                                        std::array<Real64, 2> &DF                    // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a Constant pressure drop component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 Co;
        int k;

        int n = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[0];
        int m = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeNums[1];
        auto &solver = state.dataAFNSolver->solver;

        // Get component properties
        // A  = Cross section area [m2]
        // DP = Pressure difference across the element [Pa]

        if (PDROP == 0.0) {
            F[0] = std::sqrt(2.0 * propN.density) * A * std::sqrt(DP);
            DF[0] = 0.5 * F[0] / DP;
        } else {
            for (k = 1; k <= state.dataAFNSolver->NetworkNumOfLinks; ++k) {
                if (state.dataAirflowNetwork->AirflowNetworkLinkageData(k).NodeNums[1] == n) {
                    F[0] = solver.AFLOW(k);
                    break;
                }
            }
            solver.PZ(m) = solver.PZ(n) - DP;
            Co = F[0] / DP;
            DF[0] = 10.e10;
        }
        return 1;
    }

    int EffectiveLeakageArea::calculate([[maybe_unused]] EnergyPlusData &state,
                                        bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                                        Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                        [[maybe_unused]] int const i,             // Linkage number
                                        [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                        [[maybe_unused]] const Real64 control,    // Element control signal
                                        const AirProperties &propN,               // Node 1 properties
                                        const AirProperties &propM,               // Node 2 properties
                                        std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                        std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a Surface effective leakage area component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Real64 const sqrt_2(std::sqrt(2.0));

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CDM;
        Real64 FL;
        Real64 FT;
        Real64 FlowCoef;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        // Get component properties
        FlowCoef = ELA * DischCoeff * sqrt_2 * std::pow(RefDeltaP, 0.5 - FlowExpo);

        if (LFLAG) {
            // Initialization by linear relation.
            if (PDROP >= 0.0) {
                DF[0] = FlowCoef * propN.density / propN.viscosity;
            } else {
                DF[0] = FlowCoef * propM.density / propM.viscosity;
            }
            F[0] = -DF[0] * PDROP;
        } else {
            // Standard calculation.
            if (PDROP >= 0.0) {
                // Flow in positive direction.
                // Laminar flow.
                CDM = FlowCoef * propN.density / propN.viscosity;
                FL = CDM * PDROP;
                // Turbulent flow.
                if (FlowExpo == 0.5) {
                    FT = FlowCoef * propN.sqrtDensity * std::sqrt(PDROP);
                } else {
                    FT = FlowCoef * propN.sqrtDensity * std::pow(PDROP, FlowExpo);
                }
            } else {
                // Flow in negative direction.
                // Laminar flow.
                CDM = FlowCoef * propM.density / propM.viscosity;
                FL = CDM * PDROP;
                // Turbulent flow.
                if (FlowExpo == 0.5) {
                    FT = -FlowCoef * propM.sqrtDensity * std::sqrt(-PDROP);
                } else {
                    FT = -FlowCoef * propM.sqrtDensity * std::pow(-PDROP, FlowExpo);
                }
            }
            // Select laminar or turbulent flow.
            // if (LIST >= 4) gio::write(Unit21, Format_901) << " plr: " << i << PDROP << FL << FT;
            if (std::abs(FL) <= std::abs(FT)) {
                F[0] = FL;
                DF[0] = CDM;
            } else {
                F[0] = FT;
                DF[0] = FT * FlowExpo / PDROP;
            }
        }
        return 1;
    }

    int EffectiveLeakageArea::calculate([[maybe_unused]] EnergyPlusData &state,
                                        Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                        [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                        [[maybe_unused]] const Real64 control,    // Element control signal
                                        const AirProperties &propN,               // Node 1 properties
                                        const AirProperties &propM,               // Node 2 properties
                                        std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                        std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a Surface effective leakage area component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Real64 const sqrt_2(std::sqrt(2.0));

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CDM;
        Real64 FL;
        Real64 FT;
        Real64 FlowCoef;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        // Get component properties
        FlowCoef = ELA * DischCoeff * sqrt_2 * std::pow(RefDeltaP, 0.5 - FlowExpo);

        // Standard calculation.
        if (PDROP >= 0.0) {
            // Flow in positive direction.
            // Laminar flow.
            CDM = FlowCoef * propN.density / propN.viscosity;
            FL = CDM * PDROP;
            // Turbulent flow.
            if (FlowExpo == 0.5) {
                FT = FlowCoef * propN.sqrtDensity * std::sqrt(PDROP);
            } else {
                FT = FlowCoef * propN.sqrtDensity * std::pow(PDROP, FlowExpo);
            }
        } else {
            // Flow in negative direction.
            // Laminar flow.
            CDM = FlowCoef * propM.density / propM.viscosity;
            FL = CDM * PDROP;
            // Turbulent flow.
            if (FlowExpo == 0.5) {
                FT = -FlowCoef * propM.sqrtDensity * std::sqrt(-PDROP);
            } else {
                FT = -FlowCoef * propM.sqrtDensity * std::pow(-PDROP, FlowExpo);
            }
        }
        // Select laminar or turbulent flow.
        // if (LIST >= 4) gio::write(Unit21, Format_901) << " plr: " << i << PDROP << FL << FT;
        if (std::abs(FL) <= std::abs(FT)) {
            F[0] = FL;
            DF[0] = CDM;
        } else {
            F[0] = FT;
            DF[0] = FT * FlowExpo / PDROP;
        }

        return 1;
    }

    int DisSysCompCoilProp::calculate([[maybe_unused]] EnergyPlusData &state,
                                      bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                                      Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                      [[maybe_unused]] int const i,             // Linkage number
                                      [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                      [[maybe_unused]] const Real64 control,    // Element control signal
                                      const AirProperties &propN,               // Node 1 properties
                                      const AirProperties &propM,               // Node 2 properties
                                      std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                      std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a coil component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const C(0.868589);
        Real64 const EPS(0.001);
        Real64 const Rough(0.0001);
        Real64 const InitLamCoef(128.0);
        Real64 const LamDynCoef(64.0);
        Real64 const LamFriCoef(0.0001);
        Real64 const TurDynCoef(0.0001);

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
        Real64 ed;
        Real64 ld;
        Real64 g;
        Real64 AA1;
        Real64 area;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        // Get component properties
        // ed = Rough / DisSysCompCoilData(CompNum).hydraulicDiameter;
        ed = Rough / hydraulicDiameter;

        area = square(hydraulicDiameter) * DataGlobalConstants::Pi;
        ld = L / hydraulicDiameter;
        g = 1.14 - 0.868589 * std::log(ed);
        AA1 = g;

        if (LFLAG) {
            // Initialization by linear relation.
            if (PDROP >= 0.0) {
                DF[0] = (2.0 * propN.density * area * hydraulicDiameter) / (propN.viscosity * InitLamCoef * ld);
            } else {
                DF[0] = (2.0 * propM.density * area * hydraulicDiameter) / (propM.viscosity * InitLamCoef * ld);
            }
            F[0] = -DF[0] * PDROP;
            // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwi:" << i << InitLamCoef << F[0] << DF[0];
        } else {
            // Standard calculation.
            if (PDROP >= 0.0) {
                // Flow in positive direction.
                // Laminar flow coefficient !=0
                if (LamFriCoef >= 0.001) {
                    A2 = LamFriCoef / (2.0 * propN.density * area * area);
                    A1 = (propN.viscosity * LamDynCoef * ld) / (2.0 * propN.density * area * hydraulicDiameter);
                    A0 = -PDROP;
                    CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                    FL = (CDM - A1) / (2.0 * A2);
                    CDM = 1.0 / CDM;
                } else {
                    CDM = (2.0 * propN.density * area * hydraulicDiameter) / (propN.viscosity * LamDynCoef * ld);
                    FL = CDM * PDROP;
                }
                RE = FL * hydraulicDiameter / (propN.viscosity * area);
                // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwl:" << i << PDROP << FL << CDM << RE;
                // Turbulent flow; test when Re>10.
                if (RE >= 10.0) {
                    S2 = std::sqrt(2.0 * propN.density * PDROP) * area;
                    FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                    // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << S2 << FTT << g;
                    while (true) {
                        FT = FTT;
                        B = (9.3 * propN.viscosity * area) / (FT * Rough);
                        D = 1.0 + g * B;
                        g -= (g - AA1 + C * std::log(D)) / (1.0 + C * B / D);
                        FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                        // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << B << FTT << g;
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
                    A1 = (propM.viscosity * LamDynCoef * ld) / (2.0 * propM.density * area * hydraulicDiameter);
                    A0 = PDROP;
                    CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                    FL = -(CDM - A1) / (2.0 * A2);
                    CDM = 1.0 / CDM;
                } else {
                    CDM = (2.0 * propM.density * area * hydraulicDiameter) / (propM.viscosity * LamDynCoef * ld);
                    FL = CDM * PDROP;
                }
                RE = -FL * hydraulicDiameter / (propM.viscosity * area);
                // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwl:" << i << PDROP << FL << CDM << RE;
                // Turbulent flow; test when Re>10.
                if (RE >= 10.0) {
                    S2 = std::sqrt(-2.0 * propM.density * PDROP) * area;
                    FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                    // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << S2 << FTT << g;
                    while (true) {
                        FT = FTT;
                        B = (9.3 * propM.viscosity * area) / (FT * Rough);
                        D = 1.0 + g * B;
                        g -= (g - AA1 + C * std::log(D)) / (1.0 + C * B / D);
                        FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                        // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << B << FTT << g;
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

    int DisSysCompCoilProp::calculate([[maybe_unused]] EnergyPlusData &state,
                                      Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                      [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                      [[maybe_unused]] const Real64 control,    // Element control signal
                                      const AirProperties &propN,               // Node 1 properties
                                      const AirProperties &propM,               // Node 2 properties
                                      std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                      std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a coil component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const C(0.868589);
        Real64 const EPS(0.001);
        Real64 const Rough(0.0001);
        Real64 const LamDynCoef(64.0);
        Real64 const LamFriCoef(0.0001);
        Real64 const TurDynCoef(0.0001);

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
        Real64 ed;
        Real64 ld;
        Real64 g;
        Real64 AA1;
        Real64 area;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        // Get component properties
        // ed = Rough / DisSysCompCoilData(CompNum).hydraulicDiameter;
        ed = Rough / hydraulicDiameter;

        area = square(hydraulicDiameter) * DataGlobalConstants::Pi;
        ld = L / hydraulicDiameter;
        g = 1.14 - 0.868589 * std::log(ed);
        AA1 = g;

        // Standard calculation.
        if (PDROP >= 0.0) {
            // Flow in positive direction.
            // Laminar flow coefficient !=0
            if (LamFriCoef >= 0.001) {
                A2 = LamFriCoef / (2.0 * propN.density * area * area);
                A1 = (propN.viscosity * LamDynCoef * ld) / (2.0 * propN.density * area * hydraulicDiameter);
                A0 = -PDROP;
                CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                FL = (CDM - A1) / (2.0 * A2);
                CDM = 1.0 / CDM;
            } else {
                CDM = (2.0 * propN.density * area * hydraulicDiameter) / (propN.viscosity * LamDynCoef * ld);
                FL = CDM * PDROP;
            }
            RE = FL * hydraulicDiameter / (propN.viscosity * area);
            // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwl:" << i << PDROP << FL << CDM << RE;
            // Turbulent flow; test when Re>10.
            if (RE >= 10.0) {
                S2 = std::sqrt(2.0 * propN.density * PDROP) * area;
                FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << S2 << FTT << g;
                while (true) {
                    FT = FTT;
                    B = (9.3 * propN.viscosity * area) / (FT * Rough);
                    D = 1.0 + g * B;
                    g -= (g - AA1 + C * std::log(D)) / (1.0 + C * B / D);
                    FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                    // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << B << FTT << g;
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
                A1 = (propM.viscosity * LamDynCoef * ld) / (2.0 * propM.density * area * hydraulicDiameter);
                A0 = PDROP;
                CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                FL = -(CDM - A1) / (2.0 * A2);
                CDM = 1.0 / CDM;
            } else {
                CDM = (2.0 * propM.density * area * hydraulicDiameter) / (propM.viscosity * LamDynCoef * ld);
                FL = CDM * PDROP;
            }
            RE = -FL * hydraulicDiameter / (propM.viscosity * area);
            // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwl:" << i << PDROP << FL << CDM << RE;
            // Turbulent flow; test when Re>10.
            if (RE >= 10.0) {
                S2 = std::sqrt(-2.0 * propM.density * PDROP) * area;
                FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << S2 << FTT << g;
                while (true) {
                    FT = FTT;
                    B = (9.3 * propM.viscosity * area) / (FT * Rough);
                    D = 1.0 + g * B;
                    g -= (g - AA1 + C * std::log(D)) / (1.0 + C * B / D);
                    FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                    // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << B << FTT << g;
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
        return 1;
    }

    int DisSysCompTermUnitProp::calculate([[maybe_unused]] EnergyPlusData &state,
                                          bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                                          Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                          int const i,                              // Linkage number
                                          [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                          [[maybe_unused]] const Real64 control,    // Element control signal
                                          const AirProperties &propN,               // Node 1 properties
                                          const AirProperties &propM,               // Node 2 properties
                                          std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                          std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
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
        // This subroutine solves airflow for a terminal unit component

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const C(0.868589);
        Real64 const EPS(0.001);
        Real64 const Rough(0.0001);
        Real64 const InitLamCoef(128.0);
        Real64 const LamDynCoef(64.0);
        Real64 const LamFriCoef(0.0001);
        Real64 const TurDynCoef(0.0001);

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
        Real64 ed;
        Real64 ld;
        Real64 g;
        Real64 AA1;
        Real64 area;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        // Get component properties
        ed = Rough / hydraulicDiameter;
        area = pow_2(hydraulicDiameter) * DataGlobalConstants::Pi;
        ld = L / hydraulicDiameter;
        g = 1.14 - 0.868589 * std::log(ed);
        AA1 = g;

        if (LFLAG) {
            // Initialization by linear relation.
            if (PDROP >= 0.0) {
                DF[0] = (2.0 * propN.density * area * hydraulicDiameter) / (propN.viscosity * InitLamCoef * ld);
            } else {
                DF[0] = (2.0 * propM.density * area * hydraulicDiameter) / (propM.viscosity * InitLamCoef * ld);
            }
            F[0] = -DF[0] * PDROP;
            // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwi:" << i << InitLamCoef << F[0] << DF[0];
        } else {
            // Standard calculation.
            if (PDROP >= 0.0) {
                // Flow in positive direction.
                // Laminar flow coefficient !=0
                if (LamFriCoef >= 0.001) {
                    A2 = LamFriCoef / (2.0 * propN.density * area * area);
                    A1 = (propN.viscosity * LamDynCoef * ld) / (2.0 * propN.density * area * hydraulicDiameter);
                    A0 = -PDROP;
                    CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                    FL = (CDM - A1) / (2.0 * A2);
                    CDM = 1.0 / CDM;
                } else {
                    CDM = (2.0 * propN.density * area * hydraulicDiameter) / (propN.viscosity * LamDynCoef * ld);
                    FL = CDM * PDROP;
                }
                RE = FL * hydraulicDiameter / (propN.viscosity * area);
                // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwl:" << i << PDROP << FL << CDM << RE;
                // Turbulent flow; test when Re>10.
                if (RE >= 10.0) {
                    S2 = std::sqrt(2.0 * propN.density * PDROP) * area;
                    FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                    // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << S2 << FTT << g;
                    while (true) {
                        FT = FTT;
                        B = (9.3 * propN.viscosity * area) / (FT * Rough);
                        D = 1.0 + g * B;
                        g -= (g - AA1 + C * std::log(D)) / (1.0 + C * B / D);
                        FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                        // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << B << FTT << g;
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
                    A1 = (propM.viscosity * LamDynCoef * ld) / (2.0 * propM.density * area * hydraulicDiameter);
                    A0 = PDROP;
                    CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                    FL = -(CDM - A1) / (2.0 * A2);
                    CDM = 1.0 / CDM;
                } else {
                    CDM = (2.0 * propM.density * area * hydraulicDiameter) / (propM.viscosity * LamDynCoef * ld);
                    FL = CDM * PDROP;
                }
                RE = -FL * hydraulicDiameter / (propM.viscosity * area);
                // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwl:" << i << PDROP << FL << CDM << RE;
                // Turbulent flow; test when Re>10.
                if (RE >= 10.0) {
                    S2 = std::sqrt(-2.0 * propM.density * PDROP) * area;
                    FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                    // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << S2 << FTT << g;
                    while (true) {
                        FT = FTT;
                        B = (9.3 * propM.viscosity * area) / (FT * Rough);
                        D = 1.0 + g * B;
                        g -= (g - AA1 + C * std::log(D)) / (1.0 + C * B / D);
                        FTT = S2 / std::sqrt(ld / pow_2(g) + TurDynCoef);
                        // if (LIST >= 4) gio::write(Unit21, Format_901) << " dwt:" << i << B << FTT << g;
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
        // If damper, setup the airflows from nodal values calculated from terminal
        if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).VAVTermDamper) {
            F[0] = state.dataLoopNodes->Node(DamperInletNode).MassFlowRate;
            if (state.dataAirflowNetwork->VAVTerminalRatio > 0.0) {
                F[0] *= state.dataAirflowNetwork->VAVTerminalRatio;
            }
            DF[0] = 0.0;
        }
        return 1;
    }

    int DisSysCompHXProp::calculate([[maybe_unused]] EnergyPlusData &state,
                                    bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                                    Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                    [[maybe_unused]] int const i,             // Linkage number
                                    [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                    [[maybe_unused]] const Real64 control,    // Element control signal
                                    const AirProperties &propN,               // Node 1 properties
                                    const AirProperties &propM,               // Node 2 properties
                                    std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                    std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   Extracted from AIRNET
        //       MODIFIED       Lixing Gu, 2/1/04
        //                      Revised the subroutine to meet E+ needs
        //       MODIFIED       Lixing Gu, 1/18/09
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine solves airflow for a heat exchanger component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const C(0.868589);
        Real64 const EPS(0.001);
        Real64 const Rough(0.0001);
        Real64 const InitLamCoef(128.0);
        Real64 const LamDynCoef(64.0);
        Real64 const LamFriCoef(0.0001);
        Real64 const TurDynCoef(0.0001);

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
        Real64 ed;
        Real64 ld;
        Real64 g;
        Real64 AA1;
        Real64 area;

        // Get component properties
        ed = Rough / hydraulicDiameter;
        area = pow_2(hydraulicDiameter) * DataGlobalConstants::Pi;
        ld = L / hydraulicDiameter;
        g = 1.14 - 0.868589 * std::log(ed);
        AA1 = g;

        if (LFLAG) {
            // Initialization by linear relation.
            if (PDROP >= 0.0) {
                DF[0] = (2.0 * propN.density * area * hydraulicDiameter) / (propN.viscosity * InitLamCoef * ld);
            } else {
                DF[0] = (2.0 * propM.density * area * hydraulicDiameter) / (propM.viscosity * InitLamCoef * ld);
            }
            F[0] = -DF[0] * PDROP;
        } else {
            // Standard calculation.
            if (PDROP >= 0.0) {
                // Flow in positive direction.
                // Laminar flow coefficient !=0
                if (LamFriCoef >= 0.001) {
                    A2 = LamFriCoef / (2.0 * propN.density * area * area);
                    A1 = (propN.viscosity * LamDynCoef * ld) / (2.0 * propN.density * area * hydraulicDiameter);
                    A0 = -PDROP;
                    CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                    FL = (CDM - A1) / (2.0 * A2);
                    CDM = 1.0 / CDM;
                } else {
                    CDM = (2.0 * propN.density * area * hydraulicDiameter) / (propN.viscosity * LamDynCoef * ld);
                    FL = CDM * PDROP;
                }
                RE = FL * hydraulicDiameter / (propN.viscosity * area);
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
                    A1 = (propM.viscosity * LamDynCoef * ld) / (2.0 * propM.density * area * hydraulicDiameter);
                    A0 = PDROP;
                    CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                    FL = -(CDM - A1) / (2.0 * A2);
                    CDM = 1.0 / CDM;
                } else {
                    CDM = (2.0 * propM.density * area * hydraulicDiameter) / (propM.viscosity * LamDynCoef * ld);
                    FL = CDM * PDROP;
                }
                RE = -FL * hydraulicDiameter / (propM.viscosity * area);
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

    int DisSysCompHXProp::calculate([[maybe_unused]] EnergyPlusData &state,
                                    Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                    [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                    [[maybe_unused]] const Real64 control,    // Element control signal
                                    const AirProperties &propN,               // Node 1 properties
                                    const AirProperties &propM,               // Node 2 properties
                                    std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                    std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   Extracted from AIRNET
        //       MODIFIED       Lixing Gu, 2/1/04
        //                      Revised the subroutine to meet E+ needs
        //       MODIFIED       Lixing Gu, 1/18/09
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine solves airflow for a heat exchanger component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const C(0.868589);
        Real64 const EPS(0.001);
        Real64 const Rough(0.0001);
        Real64 const LamDynCoef(64.0);
        Real64 const LamFriCoef(0.0001);
        Real64 const TurDynCoef(0.0001);

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
        Real64 ed;
        Real64 ld;
        Real64 g;
        Real64 AA1;
        Real64 area;

        // Get component properties
        ed = Rough / hydraulicDiameter;
        area = pow_2(hydraulicDiameter) * DataGlobalConstants::Pi;
        ld = L / hydraulicDiameter;
        g = 1.14 - 0.868589 * std::log(ed);
        AA1 = g;

        // Standard calculation.
        if (PDROP >= 0.0) {
            // Flow in positive direction.
            // Laminar flow coefficient !=0
            if (LamFriCoef >= 0.001) {
                A2 = LamFriCoef / (2.0 * propN.density * area * area);
                A1 = (propN.viscosity * LamDynCoef * ld) / (2.0 * propN.density * area * hydraulicDiameter);
                A0 = -PDROP;
                CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                FL = (CDM - A1) / (2.0 * A2);
                CDM = 1.0 / CDM;
            } else {
                CDM = (2.0 * propN.density * area * hydraulicDiameter) / (propN.viscosity * LamDynCoef * ld);
                FL = CDM * PDROP;
            }
            RE = FL * hydraulicDiameter / (propN.viscosity * area);
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
                A1 = (propM.viscosity * LamDynCoef * ld) / (2.0 * propM.density * area * hydraulicDiameter);
                A0 = PDROP;
                CDM = std::sqrt(A1 * A1 - 4.0 * A2 * A0);
                FL = -(CDM - A1) / (2.0 * A2);
                CDM = 1.0 / CDM;
            } else {
                CDM = (2.0 * propM.density * area * hydraulicDiameter) / (propM.viscosity * LamDynCoef * ld);
                FL = CDM * PDROP;
            }
            RE = -FL * hydraulicDiameter / (propM.viscosity * area);
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
        return 1;
    }

    int ZoneExhaustFan::calculate(EnergyPlusData &state,
                                  bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                                  Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                  int const i,                              // Linkage number
                                  [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                  [[maybe_unused]] const Real64 control,    // Element control signal
                                  const AirProperties &propN,               // Node 1 properties
                                  const AirProperties &propM,               // Node 2 properties
                                  std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                  std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   Extracted from AIRNET
        //       MODIFIED       Lixing Gu, 12/17/06
        //                      Revised for zone exhaust fan
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine solves airflow for a surface crack component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataHVACGlobals::VerySmallMassFlow;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CDM;
        Real64 FL;
        Real64 FT;
        Real64 RhozNorm;
        Real64 VisczNorm;
        Real64 expn;
        Real64 Ctl;
        Real64 coef;
        Real64 Corr;
        Real64 VisAve;
        Real64 Tave;
        Real64 RhoCor;
        // int InletNode;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        if (state.dataLoopNodes->Node(InletNode).MassFlowRate > VerySmallMassFlow) {
            // Treat the component as an exhaust fan
            if (state.dataAirflowNetwork->PressureSetFlag == PressureCtrlExhaust) {
                F[0] = state.dataAirflowNetwork->ExhaustFanMassFlowRate;
            } else {
                F[0] = state.dataLoopNodes->Node(InletNode).MassFlowRate;
            }
            DF[0] = 0.0;
            return 1;
        } else {
            // Treat the component as a surface crack
            // Crack standard condition from given inputs
            Corr = state.dataAirflowNetwork->MultizoneSurfaceData(i).Factor;
            RhozNorm = AIRDENSITY(state, StandardP, StandardT, StandardW);
            VisczNorm = 1.71432e-5 + 4.828e-8 * StandardT;

            expn = FlowExpo;
            VisAve = (propN.viscosity + propM.viscosity) / 2.0;
            Tave = (propN.temperature + propM.temperature) / 2.0;
            if (PDROP >= 0.0) {
                coef = FlowCoef / propN.sqrtDensity * Corr;
            } else {
                coef = FlowCoef / propM.sqrtDensity * Corr;
            }

            if (LFLAG) {
                // Initialization by linear relation.
                if (PDROP >= 0.0) {
                    RhoCor = TOKELVIN(propN.temperature) / TOKELVIN(Tave);
                    Ctl = std::pow(RhozNorm / propN.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
                    DF[0] = coef * propN.density / propN.viscosity * Ctl;
                } else {
                    RhoCor = TOKELVIN(propM.temperature) / TOKELVIN(Tave);
                    Ctl = std::pow(RhozNorm / propM.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
                    DF[0] = coef * propM.density / propM.viscosity * Ctl;
                }
                F[0] = -DF[0] * PDROP;
            } else {
                // Standard calculation.
                if (PDROP >= 0.0) {
                    // Flow in positive direction.
                    // Laminar flow.
                    RhoCor = TOKELVIN(propN.temperature) / TOKELVIN(Tave);
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
                    RhoCor = TOKELVIN(propM.temperature) / TOKELVIN(Tave);
                    Ctl = std::pow(RhozNorm / propM.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
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
                // if (LIST >= 4) gio::write(Unit21, Format_901) << " scr: " << i << PDROP << FL << FT;
                if (std::abs(FL) <= std::abs(FT)) {
                    F[0] = FL;
                    DF[0] = CDM;
                } else {
                    F[0] = FT;
                    DF[0] = FT * expn / PDROP;
                }
            }
        }
        return 1;
    }

    int ZoneExhaustFan::calculate(EnergyPlusData &state,
                                  Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                  [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                  const Real64 control,                     // Element control signal
                                  const AirProperties &propN,               // Node 1 properties
                                  const AirProperties &propM,               // Node 2 properties
                                  std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                  std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   Extracted from AIRNET
        //       MODIFIED       Lixing Gu, 12/17/06
        //                      Revised for zone exhaust fan
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine solves airflow for a surface crack component

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataHVACGlobals::VerySmallMassFlow;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 CDM;
        Real64 FL;
        Real64 FT;
        Real64 RhozNorm;
        Real64 VisczNorm;
        Real64 expn;
        Real64 Ctl;
        Real64 coef;
        Real64 VisAve;
        Real64 Tave;
        Real64 RhoCor;
        // int InletNode;

        // Formats
        // static gio::Fmt Format_901("(A5,I3,6X,4E16.7)");

        if (state.dataLoopNodes->Node(InletNode).MassFlowRate > VerySmallMassFlow) {
            // Treat the component as an exhaust fan
            if (state.dataAirflowNetwork->PressureSetFlag == PressureCtrlExhaust) {
                F[0] = state.dataAirflowNetwork->ExhaustFanMassFlowRate;
            } else {
                F[0] = state.dataLoopNodes->Node(InletNode).MassFlowRate;
            }
            DF[0] = 0.0;
            return 1;
        } else {
            // Treat the component as a surface crack
            // Crack standard condition from given inputs
            RhozNorm = AIRDENSITY(state, StandardP, StandardT, StandardW);
            VisczNorm = 1.71432e-5 + 4.828e-8 * StandardT;

            expn = FlowExpo;
            VisAve = (propN.viscosity + propM.viscosity) / 2.0;
            Tave = (propN.temperature + propM.temperature) / 2.0;
            if (PDROP >= 0.0) {
                coef = control * FlowCoef / propN.sqrtDensity;
            } else {
                coef = control * FlowCoef / propM.sqrtDensity;
            }

            // Standard calculation.
            if (PDROP >= 0.0) {
                // Flow in positive direction.
                // Laminar flow.
                RhoCor = TOKELVIN(propN.temperature) / TOKELVIN(Tave);
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
                RhoCor = TOKELVIN(propM.temperature) / TOKELVIN(Tave);
                Ctl = std::pow(RhozNorm / propM.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
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
            // if (LIST >= 4) gio::write(Unit21, Format_901) << " scr: " << i << PDROP << FL << FT;
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

    int HorizontalOpening::calculate(EnergyPlusData &state,
                                     bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                                     Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                     int const i,                              // Linkage number
                                     [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                     [[maybe_unused]] const Real64 control,    // Element control signal
                                     const AirProperties &propN,               // Node 1 properties
                                     const AirProperties &propM,               // Node 2 properties
                                     std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                     std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   Apr. 2009
        //       MODIFIED       na
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine solves airflow for a horizontal opening component. The subroutine was
        // developed based on the subroutine AFEPLR of AIRNET.

        // METHODOLOGY EMPLOYED:
        // Combine forced and buyancy airflows together with a cap

        // REFERENCES:
        // Cooper, L., 1989, "Calculation of the Flow Through a Horizontal Ceiling/Floor Vent,"
        // NISTIR 89-4052, National Institute of Standards and Technology, Gaithersburg, MD

        // USE STATEMENTS:
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 RhozAver;
        Real64 expn;
        Real64 coef;
        Real64 Width;  // Opening width
        Real64 Height; // Opening height
        Real64 Fact;   // Opening factor
        // Real64 Slope;      // Opening slope
        // Real64 DischCoeff; // Discharge coefficient
        Real64 fma12;      // massflow in direction "from-to" [kg/s]
        Real64 fma21;      // massflow in direction "to-from" [kg/s]
        Real64 dp1fma12;   // derivative d fma12 / d Dp [kg/s/Pa]
        Real64 dp1fma21;   // derivative d fma21 / d Dp [kg/s/Pa]
        Real64 PurgedP;    // Purge pressure [Pa]
        Real64 BuoFlow;    // Buoyancy flow rate [Pa]
        Real64 BuoFlowMax; // Maximum buoyancy flow rate [Pa]
        Real64 dPBuoFlow;  // Derivative of buoyancy flow rate [kg/s/Pa]
        Real64 DH;         // Hydraulic diameter [m]
        Real64 Cshape;     // Shape factor [dimensionless]
        Real64 OpenArea;   // Opening area [m2]

        // Get information on the horizontal opening
        RhozAver = (propN.density + propM.density) / 2.0;
        Width = state.dataAirflowNetwork->MultizoneSurfaceData(i).Width;
        Height = state.dataAirflowNetwork->MultizoneSurfaceData(i).Height;
        Fact = state.dataAirflowNetwork->MultizoneSurfaceData(i).OpenFactor;
        expn = FlowExpo;
        coef = FlowCoef;
        // Slope = MultizoneCompHorOpeningData(CompNum).Slope;
        // DischCoeff = MultizoneCompHorOpeningData(CompNum).DischCoeff;
        Cshape = 0.942 * Width / Height;
        OpenArea =
            Width * Height * Fact * std::sin(Slope * DataGlobalConstants::Pi / 180.0) * (1.0 + std::cos(Slope * DataGlobalConstants::Pi / 180.0));
        DH = 4.0 * (Width * Height) / 2.0 / (Width + Height) * Fact;

        // Check which zone is higher
        if (Fact == 0.0) {
            return GenericCrack(state, coef, expn, LFLAG, PDROP, propN, propM, F, DF);
        }

        fma12 = 0.0;
        fma21 = 0.0;
        dp1fma12 = 0.0;
        dp1fma21 = 0.0;
        BuoFlow = 0.0;
        dPBuoFlow = 0.0;

        if (state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[0] >
            state.dataAirflowNetwork->AirflowNetworkLinkageData(i).NodeHeights[1]) {
            // Node N is upper zone
            if (propN.density > propM.density) {
                BuoFlowMax = RhozAver * 0.055 * std::sqrt(9.81 * std::abs(propN.density - propM.density) * pow_5(DH) / RhozAver);
                PurgedP = Cshape * Cshape * 9.81 * std::abs(propN.density - propM.density) * pow_5(DH) / (2.0 * pow_2(OpenArea));
                if (std::abs(PDROP) <= PurgedP) {
                    BuoFlow = BuoFlowMax * (1.0 - std::abs(PDROP) / PurgedP);
                    dPBuoFlow = BuoFlowMax / PurgedP;
                }
            }
        } else {
            // Node M is upper zone
            if (propN.density < propM.density) {
                BuoFlowMax = RhozAver * 0.055 * std::sqrt(9.81 * std::abs(propN.density - propM.density) * pow_5(DH) / RhozAver);
                PurgedP = Cshape * Cshape * 9.81 * std::abs(propN.density - propM.density) * pow_5(DH) / (2.0 * pow_2(OpenArea));
                if (std::abs(PDROP) <= PurgedP) {
                    BuoFlow = BuoFlowMax * (1.0 - std::abs(PDROP) / PurgedP);
                    dPBuoFlow = BuoFlowMax / PurgedP;
                }
            }
        }

        if (PDROP == 0.0) {
            fma12 = BuoFlow;
            fma21 = BuoFlow;
            dp1fma12 = 0.0;
            dp1fma21 = 0.0;
        } else if (PDROP > 0.0) {
            fma12 = propN.density * OpenArea * Fact * DischCoeff * std::sqrt(2.0 * PDROP / RhozAver) + BuoFlow;
            dp1fma12 = propN.density * OpenArea * DischCoeff / std::sqrt(2.0 * PDROP * RhozAver) + dPBuoFlow;
            if (BuoFlow > 0.0) {
                fma21 = BuoFlow;
                dp1fma21 = dPBuoFlow;
            }
        } else { // PDROP.LT.0.0
            fma21 = propM.density * OpenArea * Fact * DischCoeff * std::sqrt(2.0 * std::abs(PDROP) / RhozAver) + BuoFlow;
            dp1fma21 = -propM.density * OpenArea * DischCoeff / std::sqrt(2.0 * std::abs(PDROP) * RhozAver) + dPBuoFlow;
            if (BuoFlow > 0.0) {
                fma12 = BuoFlow;
                dp1fma12 = dPBuoFlow;
            }
        }

        F[0] = fma12 - fma21;
        DF[0] = dp1fma12 - dp1fma21;
        F[1] = 0.0;
        if (fma12 != 0.0 && fma21 != 0.0) {
            F[1] = BuoFlow;
        }
        DF[1] = 0.0;

        return 1;
    }

    int OutdoorAirFan::calculate(EnergyPlusData &state,
                                 bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                                 Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                                 int const i,                              // Linkage number
                                 [[maybe_unused]] const Real64 multiplier, // Element multiplier
                                 [[maybe_unused]] const Real64 control,    // Element control signal
                                 const AirProperties &propN,               // Node 1 properties
                                 const AirProperties &propM,               // Node 2 properties
                                 std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                                 std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
    )
    {

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine solves airflow for a constant flow rate airflow component -- using standard interface.

        // Using/Aliasing
        using DataHVACGlobals::FanType_SimpleOnOff;
        using DataHVACGlobals::VerySmallMassFlow;

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const CycFanCycComp(1); // fan cycles with compressor operation

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 expn;
        Real64 Ctl;
        Real64 coef;
        Real64 Corr;
        Real64 VisAve;
        Real64 Tave;
        Real64 RhoCor;
        // int InletNode;
        Real64 RhozNorm;
        Real64 VisczNorm;
        Real64 CDM;
        Real64 FL;
        Real64 FT;

        int AirLoopNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum;

        if (state.dataLoopNodes->Node(InletNode).MassFlowRate > VerySmallMassFlow) {
            // Treat the component as an exhaust fan
            F[0] = state.dataLoopNodes->Node(InletNode).MassFlowRate;
            DF[0] = 0.0;
            if (state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode == CycFanCycComp &&
                state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio > 0.0) {
                F[0] = F[0] / state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio;
            }
            return 1;
        } else {
            // Treat the component as a surface crack
            // Crack standard condition from given inputs
            Corr = 1.0;
            RhozNorm = AIRDENSITY(state, StandardP, StandardT, StandardW);
            VisczNorm = 1.71432e-5 + 4.828e-8 * StandardT;

            expn = FlowExpo;
            VisAve = (propN.viscosity + propM.viscosity) / 2.0;
            Tave = (propN.temperature + propM.temperature) / 2.0;
            if (PDROP >= 0.0) {
                coef = FlowCoef / propN.sqrtDensity * Corr;
            } else {
                coef = FlowCoef / propM.sqrtDensity * Corr;
            }

            if (LFLAG) {
                // Initialization by linear relation.
                if (PDROP >= 0.0) {
                    RhoCor = TOKELVIN(propN.temperature) / TOKELVIN(Tave);
                    Ctl = std::pow(RhozNorm / propN.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
                    DF[0] = coef * propN.density / propN.viscosity * Ctl;
                } else {
                    RhoCor = TOKELVIN(propM.temperature) / TOKELVIN(Tave);
                    Ctl = std::pow(RhozNorm / propM.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
                    DF[0] = coef * propM.density / propM.viscosity * Ctl;
                }
                F[0] = -DF[0] * PDROP;
            } else {
                // Standard calculation.
                if (PDROP >= 0.0) {
                    // Flow in positive direction.
                    // Laminar flow.
                    RhoCor = TOKELVIN(propN.temperature) / TOKELVIN(Tave);
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
                    RhoCor = TOKELVIN(propM.temperature) / TOKELVIN(Tave);
                    Ctl = std::pow(RhozNorm / propM.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
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
                if (std::abs(FL) <= std::abs(FT)) {
                    F[0] = FL;
                    DF[0] = CDM;
                } else {
                    F[0] = FT;
                    DF[0] = FT * expn / PDROP;
                }
            }
        }
        return 1;
    }

    int ReliefFlow::calculate(EnergyPlusData &state,
                              bool const LFLAG,                         // Initialization flag.If = 1, use laminar relationship
                              Real64 const PDROP,                       // Total pressure drop across a component (P1 - P2) [Pa]
                              int const i,                              // Linkage number
                              [[maybe_unused]] const Real64 multiplier, // Element multiplier
                              [[maybe_unused]] const Real64 control,    // Element control signal
                              const AirProperties &propN,               // Node 1 properties
                              const AirProperties &propM,               // Node 2 properties
                              std::array<Real64, 2> &F,                 // Airflow through the component [kg/s]
                              std::array<Real64, 2> &DF                 // Partial derivative:  DF/DP
    )
    {

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine solves airflow for a constant flow rate airflow component -- using standard interface.

        // Using/Aliasing
        using DataHVACGlobals::VerySmallMassFlow;

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const CycFanCycComp(1); // fan cycles with compressor operation

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 expn;
        Real64 Ctl;
        Real64 coef;
        Real64 Corr;
        Real64 VisAve;
        Real64 Tave;
        Real64 RhoCor;
        Real64 RhozNorm;
        Real64 VisczNorm;
        Real64 CDM;
        Real64 FL;
        Real64 FT;

        int AirLoopNum = state.dataAirflowNetwork->AirflowNetworkLinkageData(i).AirLoopNum;

        if (state.dataLoopNodes->Node(OutletNode).MassFlowRate > VerySmallMassFlow) {
            // Treat the component as an exhaust fan
            DF[0] = 0.0;
            if (state.dataAirflowNetwork->PressureSetFlag == PressureCtrlRelief) {
                F[0] = state.dataAirflowNetwork->ReliefMassFlowRate;
            } else {
                F[0] = state.dataLoopNodes->Node(OutletNode).MassFlowRate;
                if (state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode == CycFanCycComp &&
                    state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio > 0.0) {
                    F[0] = F[0] / state.dataAirLoop->AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio;
                }
            }
            return 1;
        } else {
            // Treat the component as a surface crack
            // Crack standard condition from given inputs
            Corr = 1.0;
            RhozNorm = AIRDENSITY(state, StandardP, StandardT, StandardW);
            VisczNorm = 1.71432e-5 + 4.828e-8 * StandardT;

            expn = FlowExpo;
            VisAve = (propN.viscosity + propM.viscosity) / 2.0;
            Tave = (propN.temperature + propM.temperature) / 2.0;
            if (PDROP >= 0.0) {
                coef = FlowCoef / propN.sqrtDensity * Corr;
            } else {
                coef = FlowCoef / propM.sqrtDensity * Corr;
            }

            if (LFLAG) {
                // Initialization by linear relation.
                if (PDROP >= 0.0) {
                    RhoCor = TOKELVIN(propN.temperature) / TOKELVIN(Tave);
                    Ctl = std::pow(RhozNorm / propN.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
                    DF[0] = coef * propN.density / propN.viscosity * Ctl;
                } else {
                    RhoCor = TOKELVIN(propM.temperature) / TOKELVIN(Tave);
                    Ctl = std::pow(RhozNorm / propM.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
                    DF[0] = coef * propM.density / propM.viscosity * Ctl;
                }
                F[0] = -DF[0] * PDROP;
            } else {
                // Standard calculation.
                if (PDROP >= 0.0) {
                    // Flow in positive direction.
                    // Laminar flow.
                    RhoCor = TOKELVIN(propN.temperature) / TOKELVIN(Tave);
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
                    RhoCor = TOKELVIN(propM.temperature) / TOKELVIN(Tave);
                    Ctl = std::pow(RhozNorm / propM.density / RhoCor, expn - 1.0) * std::pow(VisczNorm / VisAve, 2.0 * expn - 1.0);
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
                if (std::abs(FL) <= std::abs(FT)) {
                    F[0] = FL;
                    DF[0] = CDM;
                } else {
                    F[0] = FT;
                    DF[0] = FT * expn / PDROP;
                }
            }
        }
        return 1;
    }

} // namespace AirflowNetwork

} // namespace EnergyPlus
