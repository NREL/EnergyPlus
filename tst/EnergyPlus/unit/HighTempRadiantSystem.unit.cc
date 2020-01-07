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

// EnergyPlus::Low Temperature Radiant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/HighTempRadiantSystem.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::HighTempRadiantSystem;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHeatBalance;
using namespace DataGlobals;
using namespace DataHVACGlobals;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataSizing;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, HighTempRadiantSystemTest_GetHighTempRadiantSystem)
{

    bool ErrorsFound;

    std::string const idf_objects = delimited_string({
        "  ZoneHVAC:HighTemperatureRadiant,",
        "    ZONERADHEATER,           !- Name",
        "    ,                        !- Availability Schedule Name",
        "	 ZONE1,                   !- Zone Name",
        "	 HeatingDesignCapacity,   !- Heating Design Capacity Method",
        "	 10000,                   !- Heating Design Capacity {W}",
        "	 ,                        !- Heating Design Capacity Per Floor Area {W/m2}",
        "	 ,                        !- Fraction of Autosized Heating Design Capacity",
        "	 Electricity,             !- Fuel Type",
        "	 1.0,                     !- Combustion Efficiency",
        "	 0.80,                    !- Fraction of Input Converted to Radiant Energy",
        "	 0.00,                    !- Fraction of Input Converted to Latent Energy",
        "	 0.00,                    !- Fraction of Input that Is Lost",
        "	 MeanAirTemperature,      !- Temperature Control Type",
        "	 2.0,                     !- Heating Throttling Range {deltaC}",
        "	 Radiant Heating Setpoints, !- Heating Setpoint Temperature Schedule Name",
        "	 0.04,                    !- Fraction of Radiant Energy Incident on People",
        "	 WALL1,                   !- Surface 1 Name",
        "	 0.80;                    !- Fraction of Radiant Energy to Surface 1",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    Zone.allocate(1);
    Zone(1).Name = "ZONE1";
    Surface.allocate(1);
    Surface(1).Name = "WALL1";
    Surface(1).Zone = 1;

    ErrorsFound = false;

    GetHighTempRadiantSystem(ErrorsFound);

    std::string const error_string01 =
        delimited_string({"   ** Severe  ** Heating Setpoint Temperature Schedule Name not found: RADIANT HEATING SETPOINTS",
                          "   **   ~~~   ** Occurs for ZoneHVAC:HighTemperatureRadiant = ZONERADHEATER",
                          "   ** Severe  ** GetRadiantSystemSurface: Somehow the radiant system enclosure number is zero "
                          "forZoneHVAC:HighTemperatureRadiant = ZONERADHEATER",
                          "   ** Severe  ** Fraction of radiation distributed to surfaces and people sums up to less than 1 for ZONERADHEATER",
                          "   **   ~~~   ** This would result in some of the radiant energy delivered by the high temp radiant heater being lost.",
                          "   **   ~~~   ** The sum of all radiation fractions to surfaces = 0.80000",
                          "   **   ~~~   ** The radiant fraction to people = 4.00000E-002",
                          "   **   ~~~   ** So, all radiant fractions including surfaces and people = 0.84000",
                          "   **   ~~~   ** This means that the fraction of radiant energy that would be lost from the high temperature radiant "
                          "heater would be = 0.16000",
                          "   **   ~~~   ** Please check and correct this so that all radiant energy is accounted for in "
                          "ZoneHVAC:HighTemperatureRadiant = ZONERADHEATER"});

    EXPECT_TRUE(compare_err_stream(error_string01, true));
    EXPECT_TRUE(ErrorsFound);
}

TEST_F(EnergyPlusFixture, HighTempRadiantSystemTest_SizeHighTempRadiantSystemScalableFlagSetTest)
{
    int RadSysNum;
    int SizingTypesNum;

    DataSizing::DataScalableCapSizingON = false;
    DataSizing::CurZoneEqNum = 1;

    RadSysNum = 1;
    HighTempRadSys.allocate(RadSysNum);
    HighTempRadSysNumericFields.allocate(RadSysNum);
    HighTempRadSysNumericFields(RadSysNum).FieldNames.allocate(1);
    HighTempRadSys(RadSysNum).Name = "TESTSCALABLEFLAG";
    HighTempRadSys(RadSysNum).ZonePtr = 1;
    HighTempRadSys(RadSysNum).HeatingCapMethod = DataSizing::CapacityPerFloorArea;
    HighTempRadSys(RadSysNum).ScaledHeatingCapacity = 100.0;
    DataSizing::ZoneEqSizing.allocate(1);
    DataHeatBalance::Zone.allocate(1);
    Zone(1).FloorArea = 10.0;
    SizingTypesNum = DataHVACGlobals::NumOfSizingTypes;
    if (SizingTypesNum < 1) SizingTypesNum = 1;
    ZoneEqSizing(CurZoneEqNum).SizingMethod.allocate(DataHVACGlobals::NumOfSizingTypes);

    SizeHighTempRadiantSystem(RadSysNum);
    EXPECT_FALSE(DataSizing::DataScalableSizingON);
}

} // namespace EnergyPlus
