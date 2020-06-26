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

// EnergyPlus Headers
#include <EnergyPlus/ConductionTransferFunctionCalc.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include "OutputFiles.hh"

namespace EnergyPlus {

namespace ConductionTransferFunctionCalc {

    // MODULE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   January 1997
    //       MODIFIED       June-July 2000, RKS
    //       RE-ENGINEERED  June 1996, February 1997, August 1997, RKS
    //       RE-ENGINEERED  November 1999, LKL

    // PURPOSE OF THIS MODULE:
    // This module calculates the conduction transfer functions (CTFs) for
    // all building constructions.

    // METHODOLOGY EMPLOYED:
    // This subroutine uses the state space method of calculating CTFs.
    // The state space method involves imposing a finite difference grid
    // to a solution space (i.e., a building construction, inside to
    // outside surface).  The finite difference grid is only used to
    // derive a system of differential equations.  This first order
    // system can then be solved using matrix algebra.  In this
    // implementation of the state space method, a conversion from the
    // internal units of EnergyPlus (SI) to English units is used, This
    // is done due to observations made by Russ Taylor that the solution
    // method was not as stable numerically when SI units were used.

    // REFERENCES:
    // While there are several important references on the state space
    // method, the most definitive reference which includes details on
    // implementing the state space method is:
    // Seem, J.E.  1987.  Modeling of Heat Transfer in Buildings, Ph.D.
    // Dissertation, Department of Mechanical Engineering, University of
    // Wisconsin-Madison.

    // Using/Aliasing
    using namespace DataGlobals;
    using namespace DataHeatBalance; // This is the Heat balance super block data-only module

    void InitConductionTransferFunctions(OutputFiles &outputFiles)
    {
        static bool ErrorsFound(false); // Flag for input error condition
        bool DoCTFErrorReport(false);
        for (auto & construction : dataConstruction.Construct) {
            construction.calculateTransferFunction(ErrorsFound, DoCTFErrorReport);
        }
        ReportCTFs(outputFiles, DoCTFErrorReport);
        if (ErrorsFound) {
            ShowFatalError("Program terminated for reasons listed (InitConductionTransferFunctions)");
        }
    }

    void ReportCTFs(OutputFiles &outputFiles, bool const DoReportBecauseError)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   July 1999
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine gives a detailed report to the user about
        // the conduction transfer functions and other thermal data
        // of each construction.

        bool DoReport;
        General::ScanForReports("Constructions", DoReport, "Constructions");

        if (DoReport || DoReportBecauseError) {
            //                                      Write Descriptions
            print(outputFiles.eio,
                  "! <Construction CTF>,Construction Name,Index,#Layers,#CTFs,Time Step {{hours}},ThermalConductance "
                  "{{w/m2-K}},OuterThermalAbsorptance,InnerThermalAbsorptance,OuterSolarAbsorptance,InnerSolarAbsorptance,Roughness\n");
            print(outputFiles.eio,
                  "! <Material CTF Summary>,Material Name,Thickness {{m}},Conductivity {{w/m-K}},Density {{kg/m3}},Specific Heat "
                  "{{J/kg-K}},ThermalResistance {{m2-K/w}}\n");
            print(outputFiles.eio, "! <Material:Air>,Material Name,ThermalResistance {{m2-K/w}}\n");
            print(outputFiles.eio, "! <CTF>,Time,Outside,Cross,Inside,Flux (except final one)\n");

            for (int ThisNum = 1; ThisNum <= TotConstructs; ++ThisNum) {

                if (!dataConstruction.Construct(ThisNum).IsUsedCTF) continue;

                static constexpr auto Format_700{" Construction CTF,{},{:4},{:4},{:4},{:8.3F},{:15.4N},{:8.3F},{:8.3F},{:8.3F},{:8.3F},{}\n"};
                print(outputFiles.eio,
                      Format_700,
                      dataConstruction.Construct(ThisNum).Name,
                      ThisNum,
                      dataConstruction.Construct(ThisNum).TotLayers,
                      dataConstruction.Construct(ThisNum).NumCTFTerms,
                      dataConstruction.Construct(ThisNum).CTFTimeStep,
                      dataConstruction.Construct(ThisNum).UValue,
                      dataConstruction.Construct(ThisNum).OutsideAbsorpThermal,
                      dataConstruction.Construct(ThisNum).InsideAbsorpThermal,
                      dataConstruction.Construct(ThisNum).OutsideAbsorpSolar,
                      dataConstruction.Construct(ThisNum).InsideAbsorpSolar,
                      DisplayMaterialRoughness(dataConstruction.Construct(ThisNum).OutsideRoughness));

                for (int I = 1; I <= dataConstruction.Construct(ThisNum).TotLayers; ++I) {
                    int Layer = dataConstruction.Construct(ThisNum).LayerPoint(I);
                    {
                        auto const SELECT_CASE_var(dataMaterial.Material(Layer).Group);
                        if (SELECT_CASE_var == Air) {
                            static constexpr auto Format_702(" Material:Air,{},{:12.4N}\n");
                            print(outputFiles.eio, Format_702, dataMaterial.Material(Layer).Name, dataMaterial.Material(Layer).Resistance);
                        } else {
                            static constexpr auto Format_701(" Material CTF Summary,{},{:8.4F},{:14.3F},{:11.3F},{:13.3F},{:12.4N}\n");
                            print(outputFiles.eio,
                                  Format_701,
                                  dataMaterial.Material(Layer).Name,
                                  dataMaterial.Material(Layer).Thickness,
                                  dataMaterial.Material(Layer).Conductivity,
                                  dataMaterial.Material(Layer).Density,
                                  dataMaterial.Material(Layer).SpecHeat,
                                  dataMaterial.Material(Layer).Resistance);
                        }
                    }
                }

                for (int I = dataConstruction.Construct(ThisNum).NumCTFTerms; I >= 0; --I) {
                    if (I != 0) {
                        static constexpr auto Format_703(" CTF,{:4},{:20.8N},{:20.8N},{:20.8N},{:20.8N}\n");
                        print(outputFiles.eio,
                              Format_703,
                              I,
                              dataConstruction.Construct(ThisNum).CTFOutside(I),
                              dataConstruction.Construct(ThisNum).CTFCross(I),
                              dataConstruction.Construct(ThisNum).CTFInside(I),
                              dataConstruction.Construct(ThisNum).CTFFlux(I));
                    } else {
                        static constexpr auto Format_704(" CTF,{:4},{:20.8N},{:20.8N},{:20.8N}\n");
                        print(outputFiles.eio,
                              Format_704,
                              I,
                              dataConstruction.Construct(ThisNum).CTFOutside(I),
                              dataConstruction.Construct(ThisNum).CTFCross(I),
                              dataConstruction.Construct(ThisNum).CTFInside(I));
                    }
                }

                if (dataConstruction.Construct(ThisNum).SourceSinkPresent) {
                    // QTFs...
                    for (int I = dataConstruction.Construct(ThisNum).NumCTFTerms; I >= 0; --I) {
                        static constexpr auto Format_705(" QTF,{:4},{:20.8N},{:20.8N}\n");
                        print(outputFiles.eio, Format_705, I, dataConstruction.Construct(ThisNum).CTFSourceOut(I), dataConstruction.Construct(ThisNum).CTFSourceIn(I));
                    }
                    // QTFs for source/sink location temperature calculation...
                    for (int I = dataConstruction.Construct(ThisNum).NumCTFTerms; I >= 0; --I) {
                        static constexpr auto Format_706(" Source/Sink Loc Internal Temp QTF,{:4},{:20.8N},{:20.8N},{:20.8N}\n");
                        print(outputFiles.eio,
                              Format_706,
                              I,
                              dataConstruction.Construct(ThisNum).CTFTSourceOut(I),
                              dataConstruction.Construct(ThisNum).CTFTSourceIn(I),
                              dataConstruction.Construct(ThisNum).CTFTSourceQ(I));
                    }
                    if (dataConstruction.Construct(ThisNum).TempAfterLayer != 0) {
                        // QTFs for user specified interior temperature calculation...
                        for (int I = dataConstruction.Construct(ThisNum).NumCTFTerms; I >= 0; --I) {
                            static constexpr auto Format_707(" User Loc Internal Temp QTF,{:4},{:20.8N},{:20.8N},{:20.8N}\n");
                            print(outputFiles.eio,
                                  Format_707,
                                  I,
                                  dataConstruction.Construct(ThisNum).CTFTUserOut(I),
                                  dataConstruction.Construct(ThisNum).CTFTUserIn(I),
                                  dataConstruction.Construct(ThisNum).CTFTUserSource(I));
                        }
                    }
                }
            }
        }
    }

} // namespace ConductionTransferFunctionCalc

} // namespace EnergyPlus
