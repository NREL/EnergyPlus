// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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

#ifndef FuelCellElectricGenerator_hh_INCLUDED
#define FuelCellElectricGenerator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace FuelCellElectricGenerator {

    // Data
    // MODULE PARAMETER DEFINITIONS

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:
    extern bool GetFuelCellInput; // When TRUE, calls subroutine to read input file.
    extern Array1D_bool CheckEquipName;

    // SUBROUTINE SPECIFICATIONS FOR MODULE FuelCell ElectricGenerator

    // PRIVATE    SetupFuelAndAirConstituentData ! hardwired data for gas phase thermochemistry calcs

    // Functions

    void SimFuelCellGenerator(int GeneratorType,          // type of Generator
                              std::string const &GeneratorName, // user specified name of Generator
                              int &GeneratorIndex,
                              bool RunFlag,  // simulate Generator when TRUE
                              Real64 MyLoad, // demand on electric generator
                              bool FirstHVACIteration);

    // End FuelCell Generator Module Driver Subroutines
    //******************************************************************************

    // Beginning of FuelCell Generator Module Get Input subroutines
    //******************************************************************************

    void GetFuelCellGeneratorInput();

    // End of Get Input subroutines for the FuelCell Generator Module

    // Beginning of Generator model Subroutines
    // *****************************************************************************

    void CalcFuelCellGeneratorModel(int GeneratorNum, // Generator number
                                    bool RunFlag,     // TRUE when Generator operating
                                    Real64 MyLoad,    // Generator demand
                                    bool FirstHVACIteration);

    void ManageElectStorInteractions(int Num, // Generator number, index for structure
                                     Real64 Pdemand,
                                     Real64 PpcuLosses,
                                     bool &Constrained,  // TODO: This one is never used anywhere in the code
                                     Real64 &Pstorage,
                                     Real64 &PgridOverage // electricity that can't be stored and needs to go out
    );

    Real64 FuelCellProductGasEnthResidual(Real64 TprodGas,    // temperature, this is "x" being searched
                                          Array1<Real64> const &Par // par(1) = Generator Number
    );

    void FigureAirHeatCap(int GeneratorNum, // ID of generator FuelCell data structure
                          Real64 FluidTemp, // degree C
                          Real64 &Cp              // (J/mol*K)
    );

    void FigureAirEnthalpy(int GeneratorNum, // ID of generator FuelCell data structure
                           Real64 FluidTemp, // degree C
                           Real64 &Hair            // (kJ/mol)
    );

    void FigureFuelHeatCap(int GeneratorNum, // ID of generator FuelCell data structure
                           Real64 FluidTemp, // degree C
                           Real64 &Cp              // (J/mol*K)
    );

    void FigureFuelEnthalpy(int GeneratorNum, // ID of generator FuelCell data structure
                            Real64 FluidTemp, // degree C
                            Real64 &Hfuel           // kJ/mol
    );

    void FigureProductGasesEnthalpy(int GeneratorNum, // ID of generator FuelCell data structure
                                    Real64 FluidTemp, // degree C
                                    Real64 &HProdGases      // kJ/mol
    );

    void FigureProductGasHeatCap(int GeneratorNum, // ID of generator FuelCell data structure
                                 Real64 FluidTemp, // degree C
                                 Real64 &Cp              // (J/mol*K)
    );

    void FigureAuxilHeatGasHeatCap(int GeneratorNum, // ID of generator FuelCell data structure
                                   Real64 FluidTemp, // degree C
                                   Real64 &Cp              // (J/mol*K)
    );

    void FigureGaseousWaterEnthalpy(Real64 FluidTemp, // degree C
                                    Real64 &HGasWater       // kJ/mol
    );

    void FigureLiquidWaterEnthalpy(Real64 FluidTemp, // degree C
                                   Real64 &HLiqWater       // kJ/mol
    );

    void FigureLiquidWaterHeatCap(Real64 FluidTemp, // degree C
                                  Real64 &Cp              // (J/mol*K)
    );

    void FigureACAncillaries(int GeneratorNum, Real64 &PacAncill);

    void FigurePowerConditioningLosses(int GeneratorNum, Real64 Pdemand, Real64 &PpcuLosses);

    void FigureTransientConstraints(int GeneratorNum, // index number for accessing correct generator
                                    Real64 &Pel,            // DC power control setting for power module
                                    bool &Constrained,      // true if transient constraints kick in (TODO: never used anywhere)
                                    Real64 &PelDiff         // if constrained then this is the difference, positive
    );

    void CalcFuelCellAuxHeater(int Num); // Generator number

    void CalcFuelCellGenHeatRecovery(int Num); // Generator number

    void SimFuelCellPlantHeatRecovery(std::string const &CompType,
                                      std::string const &CompName,
                                      int CompTypeNum,
                                      int &CompNum,
                                      bool RunFlag,
                                      bool &InitLoopEquip,
                                      Real64 &MyLoad, // unused1208
                                      Real64 &MaxCap,
                                      Real64 &MinCap,
                                      Real64 &OptCap,
                                      bool FirstHVACIteration // TRUE if First iteration of simulation
    );

    void InitFuelCellGenerators(int FCnum); // index to specific fuel cell generator

    void getFuelCellGeneratorHeatRecoveryInfo(std::string const &GeneratorName, // user specified name of Generator
                                              std::string &heatRecoveryCompName);

    void FigureFuelCellZoneGains();

    void CalcUpdateHeatRecovery(int Num, // Generator number
                                bool FirstHVACIteration);

    void UpdateFuelCellGeneratorRecords(bool RunFlag, // TRUE if Generator operating
                                        int Num       // Generator number
    );

    void GetFuelCellGeneratorResults(int GeneratorType, // type of Generator
                                     int GeneratorIndex,
                                     Real64 &GeneratorPower,  // electrical power
                                     Real64 &GeneratorEnergy, // electrical energy
                                     Real64 &ThermalPower,    // heat power
                                     Real64 &ThermalEnergy    // heat energy
    );

} // namespace FuelCellElectricGenerator

} // namespace EnergyPlus

#endif
