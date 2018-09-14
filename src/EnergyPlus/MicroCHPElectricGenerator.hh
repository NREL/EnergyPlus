// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

#ifndef MicroCHPElectricGenerator_hh_INCLUDED
#define MicroCHPElectricGenerator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace MicroCHPElectricGenerator {

    // Data
    // MODULE PARAMETER DEFINITIONS

    // DERIVED TYPE DEFINITIONS
    extern bool GetMicroCHPInput; // When TRUE, calls subroutine to read input file.
    extern Array1D_bool CheckEquipName;
    extern Array1D_bool MySizeFlag;

    // SUBROUTINE SPECIFICATIONS FOR MODULE Combustion ElectricGenerator

    // Functions

    void SimMicroCHPGenerator(int const GeneratorType,          // type of Generator
                              std::string const &GeneratorName, // user specified name of Generator
                              int &GeneratorIndex,
                              bool const RunFlagElectCenter, // simulate Generator when TRUE
                              bool const RunFlagPlant,       // simulate generator when true.
                              Real64 const MyElectricLoad,   // demand on electric generator
                              Real64 const MyThermalLoad,    // thermal demand on cogenerator
                              bool const FirstHVACIteration);

    // End MicroCHPNoNormalize Generator Module Driver Subroutines
    //******************************************************************************

    // Beginning of Combustion Generator Module Get Input subroutines
    //******************************************************************************

    void GetMicroCHPGeneratorInput();

    // PARAMETERS

    void InitMicroCHPNoNormalizeGenerators(int const GeneratorNum, // Generator number
                                           bool const FirstHVACIteration);

    void CalcMicroCHPNoNormalizeGeneratorModel(int const GeneratorNum,        // Generator number
                                               bool const RunFlagElectCenter, // TRUE when Generator operating
                                               bool const RunFlagPlant,
                                               Real64 const MyElectricLoad, // Generator demand
                                               Real64 const MyThermalLoad,
                                               bool const FirstHVACIteration);

    Real64 FuncDetermineEngineTemp(Real64 const TcwOut,   // hot water leaving temp
                                   Real64 const MCeng,    // Fictitious mass and heat capacity of engine
                                   Real64 const UAHX,     // Heat exchanger UA
                                   Real64 const UAskin,   // Skin losses UA
                                   Real64 const Troom,    // surrounding zone temperature C
                                   Real64 const Qgenss,   // steady state generator heat generation
                                   Real64 const TengLast, // engine temp at previous time step
                                   Real64 const time      // elapsed time since previous evaluation
    );

    Real64 FuncDetermineCoolantWaterExitTemp(Real64 const TcwIn,      // hot water inlet temp
                                             Real64 const MCcw,       // Fictitious mass and heat capacity of coolant hx
                                             Real64 const UAHX,       // Heat exchanger UA
                                             Real64 const MdotCpcw,   // mass flow and specific heat of coolant water
                                             Real64 const Teng,       // engine mass temperature C
                                             Real64 const TcwoutLast, // coolant water leaving temp at previous time step
                                             Real64 const time        // elapsed time since previous evaluation
    );

    bool CheckMicroCHPThermalBalance(Real64 const NomHeatGen, // nominal heat generation rate for scaling
                                     Real64 const TcwIn,      // hot water inlet temp
                                     Real64 const TcwOut,     // hot water leaving temp
                                     Real64 const Teng,       // engine mass temperature C
                                     Real64 const Troom,      // surrounding zone temperature C
                                     Real64 const UAHX,       // Heat exchanger UA
                                     Real64 const UAskin,     // Skin losses UA
                                     Real64 const Qgenss,     // steady state generator heat generation
                                     Real64 const MCeng,      // Fictitious mass and heat capacity of engine
                                     Real64 const MCcw,       // Fictitious mass and heat capacity of coolant hx
                                     Real64 const MdotCpcw    // mass flow and specific heat of coolant water
    );

    void FigureMicroCHPZoneGains();

    void CalcUpdateHeatRecovery(int const Num, // Generator number
                                bool const FirstHVACIteration);

    void SimMicroCHPPlantHeatRecovery(std::string const &CompType,
                                      std::string const &CompName,
                                      int &CompNum,
                                      bool const RunFlag,
                                      bool &InitLoopEquip,
                                      Real64 &MyThermalLoad,
                                      Real64 &MaxCap,
                                      Real64 &MinCap,
                                      Real64 &OptCap,
                                      bool const FirstHVACIteration // TRUE if First iteration of simulation
    );

    void UpdateMicroCHPGeneratorRecords(int const Num); // Generator number

    void GetMicroCHPGeneratorResults(int const GeneratorType, // type of Generator
                                     int const GeneratorIndex,
                                     Real64 &GeneratorPower,  // electrical power
                                     Real64 &GeneratorEnergy, // electrical energy
                                     Real64 &ThermalPower,    // heat power
                                     Real64 &ThermalEnergy    // heat energy
    );

} // namespace MicroCHPElectricGenerator

} // namespace EnergyPlus

#endif
