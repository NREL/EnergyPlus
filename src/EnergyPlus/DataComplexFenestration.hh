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

#ifndef DataComplexFenestration_hh_INCLUDED
#define DataComplexFenestration_hh_INCLUDED

// ObjexxFCL Headers

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/TARCOGGassesParams.hh>
#include <EnergyPlus/TARCOGParams.hh>

namespace EnergyPlus {

namespace DataComplexFenestration {

    // Parameters for gas definitions
    enum class GasCoeffs
    {
        Unassigned = -1,
        Custom,
        Air,
        Argon,
        Krypton,
        Xenon
    };

    // Parameters for thermal model
    enum class ThermalModel
    {
        Unassigned = -1,
        ISO15099,
        ScaledCavityWidth,
        ConvectiveScalarModel_NoSDThickness,
        ConvectiveScalarModel_WithSDThickness
    };

    struct GapSupportPillar
    {
        // Members
        std::string Name; // Name of support pillar
        Real64 Spacing;   // Spacing between centers of support pillars (m)
        Real64 Radius;    // Support pillar radius (m)

        // Default Constructor
        GapSupportPillar() : Spacing(0.0), Radius(0.0)
        {
        }
    };

    struct GapDeflectionState
    {
        // Members
        std::string Name; // Name of deflection state
        Real64 DeflectedThickness;

        // Default Constructor
        GapDeflectionState() : DeflectedThickness(0.0)
        {
        }
    };

    struct WindowComplexShade
    {
        // Members
        std::string Name;                        // Name for complex shade
        TARCOGParams::TARCOGLayerType LayerType; // Layer type (OtherShadingType, Venetian, Woven, Perforated)
        Real64 Thickness;                        // Layer thickness (m)
        Real64 Conductivity;                     // Layer conductivity (W/m2K)
        Real64 IRTransmittance;                  // IR Transmittance
        Real64 FrontEmissivity;                  // Emissivity of front suraface
        Real64 BackEmissivity;                   // Emissivity of back surface
        Real64 TopOpeningMultiplier;             // Coverage percent for top opening (%)
        Real64 BottomOpeningMultiplier;          // Coverage percent for bottom opening (%)
        Real64 LeftOpeningMultiplier;            // Coverage percent for left opening (%)
        Real64 RightOpeningMultiplier;           // Coverage percent for right opening (%)
        Real64 FrontOpeningMultiplier;           // Coverage percent for front opening (%)
        Real64 SlatWidth;                        // Slat width (m)
        Real64 SlatSpacing;                      // Slat spacing (m)
        Real64 SlatThickness;                    // Slat thickness (m)
        Real64 SlatAngle;                        // Slat angle (deg)
        Real64 SlatConductivity;                 // Slat conductivity (W/m2K)
        Real64 SlatCurve;                        // Curvature radius of slat (if =0 then flat) (m)

        // Default Constructor
        WindowComplexShade()
            : LayerType(TARCOGParams::TARCOGLayerType::UNASSSIGNED), Thickness(0.0), Conductivity(0.0), IRTransmittance(0.0), FrontEmissivity(0.0),
              BackEmissivity(0.0), TopOpeningMultiplier(0.0), BottomOpeningMultiplier(0.0), LeftOpeningMultiplier(0.0), RightOpeningMultiplier(0.0),
              FrontOpeningMultiplier(0.0), SlatWidth(0.0), SlatSpacing(0.0), SlatThickness(0.0), SlatAngle(0.0), SlatConductivity(0.0), SlatCurve(0.0)
        {
        }
    };

    struct WindowThermalModelParams
    {
        // Members
        std::string Name;                                    // Window thermal model name
        TARCOGGassesParams::Stdrd CalculationStandard;       // Tarcog calculation standard
        TARCOGParams::TARCOGThermalModel ThermalModel;       // Tarcog thermal model
        Real64 SDScalar;                                     // SDScalar coefficient
        TARCOGParams::DeflectionCalculation DeflectionModel; // Deflection model
        Real64 VacuumPressureLimit;                          // Pressure limit at which it will be considered vacuum gas state
        Real64 InitialTemperature;                           // Window(s) temperature in time of fabrication
        Real64 InitialPressure;                              // Window(s) pressure in time of fabrication

        // Default Constructor
        WindowThermalModelParams()
            : CalculationStandard(TARCOGGassesParams::Stdrd::Unassigned), ThermalModel(TARCOGParams::TARCOGThermalModel::UNASSIGNED), SDScalar(0.0),
              DeflectionModel(TARCOGParams::DeflectionCalculation::UNASSIGNED), VacuumPressureLimit(0.0), InitialTemperature(0.0),
              InitialPressure(0.0)
        {
        }
    };

} // namespace DataComplexFenestration

} // namespace EnergyPlus

#endif
