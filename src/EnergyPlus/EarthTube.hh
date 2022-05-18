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

#ifndef EarthTube_hh_INCLUDED
#define EarthTube_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace EarthTube {

    // Parameters for Ventilation
    enum class Ventilation
    {
        Invalid = -1,
        Natural,
        Intake,
        Exhaust,
        Num
    };

    enum class SoilType
    {
        Invalid = -1,
        HeavyAndSat,
        HeavyAndDamp,
        HeavyAndDry,
        LightAndDry,
        Num
    };

    struct EarthTubeData
    {
        int ZonePtr = 0;
        int SchedPtr = 0;
        Real64 DesignLevel = 0.0;
        Real64 MinTemperature = 0.0;
        Real64 MaxTemperature = 0.0;
        Real64 DelTemperature = 0.0;
        Ventilation FanType = Ventilation::Invalid;
        Real64 FanPressure = 0.0;
        Real64 FanEfficiency = 0.0;
        Real64 FanPower = 0.0;
        Real64 GroundTempz1z2t = 0.0; // ground temp between z1 and z2 at time t
        Real64 InsideAirTemp = 0.0;
        Real64 AirTemp = 0.0;
        Real64 HumRat = 0.0;          // Humidity ratio of air leaving EarthTube and entering zone
        Real64 WetBulbTemp = 0.0;     // Humidity ratio of air leaving EarthTube and entering zone
        Real64 r1 = 0.0;              // Inner Pipe Radius (m)
        Real64 r2 = 0.0;              // Pipe Thickness (m)
        Real64 r3 = 0.0;              // Distance between Pipe Outer Surface and Undistubed Soil (m)
        Real64 PipeLength = 0.0;      // Entire Pipe Length
        Real64 PipeThermCond = 0.0;   // Pipe Thermal Conductivity
        Real64 z = 0.0;               // Depth under the Ground Surface (m)
        Real64 SoilThermDiff = 0.0;   // Soil Thermal Diffusivity
        Real64 SoilThermCond = 0.0;   // Soil Thermal Conductivity
        Real64 AverSoilSurTemp = 0.0; // Average Soil Surface Temperature
        Real64 ApmlSoilSurTemp = 0.0; // Amplitude of Soil Surface Temperature
        int SoilSurPhaseConst = 0;    // Phase constant of Soil Surface
        Real64 ConstantTermCoef = 0.0;
        Real64 TemperatureTermCoef = 0.0;
        Real64 VelocityTermCoef = 0.0;
        Real64 VelocitySQTermCoef = 0.0;
        void CalcEarthTubeHumRat(EnergyPlusData &state, int NZ); // Zone number (index)
    };

    struct EarthTubeZoneReportVars
    {
        // Members
        Real64 EarthTubeHeatLoss = 0.0;          // [J] Heat loss or cooling to zone from air delivered by earth tube
        Real64 EarthTubeHeatLossRate = 0.0;      // [W] Heat loss or cooling rate to zone from air delivered by earth tube
        Real64 EarthTubeHeatGain = 0.0;          // [J] Heat Gain to zone from air delivered by earth tube
        Real64 EarthTubeHeatGainRate = 0.0;      // [W] Heat Gain rate to zone from air delivered by earth tube
        Real64 EarthTubeOATreatmentPower = 0.0;  // [W] rate of heat transfer to/from air.  positive is heating OA to higher temp
        Real64 EarthTubeVolume = 0.0;            // Volume of Air {m3} due to EarthTube
        Real64 EarthTubeVolFlowRate = 0.0;       // Volume flow rate of air (m3/s) due to EarthTube
        Real64 EarthTubeVolFlowRateStd = 0.0;    // Volume flow rate of air (m3/s) due to EarthTube at standard air conditions
        Real64 EarthTubeMass = 0.0;              // Mass of Air {kg} due to EarthTube
        Real64 EarthTubeMassFlowRate = 0.0;      // Mass flow rate of air (kg/s) due to EarthTube
        Real64 EarthTubeWaterMassFlowRate = 0.0; // Mass flow rate of water vapor (kg/s) due to EarthTube
        Real64 EarthTubeFanElec = 0.0;           // [J] Fan Electricity consumed by EarthTube
        Real64 EarthTubeFanElecPower = 0.0;      // [W] Fan Electric power for EarthTube
        Real64 EarthTubeAirTemp = 0.0;           // Air Temp {C} of EarthTube, air leaving tube and entering zone
        Real64 EarthTubeWetBulbTemp = 0.0;       // Wet Bulb Temperature {C} of EarthTube, air leaving tube and entering zone
        Real64 EarthTubeHumRat = 0.0;            // Humidity Ratio {kg/kg} of EarthTube, air leaving tube and entering zone
    };

    void ManageEarthTube(EnergyPlusData &state);

    void GetEarthTube(EnergyPlusData &state, bool &ErrorsFound); // If errors found in input

    void CheckEarthTubesInZones(EnergyPlusData &state,
                                std::string const &ZoneName, // name of zone for error reporting
                                std::string_view FieldName,  // name of earth tube in input
                                bool &ErrorsFound            // Found a problem
    );

    void CalcEarthTube(EnergyPlusData &state);

    void ReportEarthTube(EnergyPlusData &state);

} // namespace EarthTube

struct EarthTubeData : BaseGlobalStruct
{
    bool GetInputFlag = true;
    EPVector<EarthTube::EarthTubeData> EarthTubeSys;
    EPVector<EarthTube::EarthTubeZoneReportVars> ZnRptET;

    void clear_state() override
    {
        *this = EarthTubeData();
    }
};

} // namespace EnergyPlus

#endif
