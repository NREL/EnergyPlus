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

// EnergyPlus::Low Temperature Radiant Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
//#include <EnergyPlus/DataHeatBalance.hh>
//#include <EnergyPlus/DataPlant.hh>
//#include <EnergyPlus/DataSizing.hh>
//#include <EnergyPlus/DataZoneEquipment.hh>
//#include <EnergyPlus/FluidProperties.hh>
//#include <EnergyPlus/LowTempRadiantSystem.hh>
//#include <EnergyPlus/UtilityRoutines.hh>
//
//#include <EnergyPlus/DataHVACGlobals.hh>
//#include <EnergyPlus/DataHeatBalFanSys.hh>
//#include <EnergyPlus/DataPlant.hh>
//#include <EnergyPlus/DataSurfaceLists.hh>
//#include <EnergyPlus/DataSurfaces.hh>
//#include <EnergyPlus/DataZoneEnergyDemands.hh>
//#include <EnergyPlus/General.hh>
//#include <EnergyPlus/HeatBalanceManager.hh>
//#include <EnergyPlus/Plant/PlantManager.hh>
//#include <EnergyPlus/PlantUtilities.hh>
//#include <EnergyPlus/Psychrometrics.hh>
//#include <EnergyPlus/ScheduleManager.hh>
//#include <EnergyPlus/SizingManager.hh>
//#include <EnergyPlus/SurfaceGeometry.hh>
//
//using namespace EnergyPlus;
//using namespace EnergyPlus::LowTempRadiantSystem;
//using namespace ObjexxFCL;
//using namespace EnergyPlus::DataHeatBalance;
//using namespace DataGlobals;
//using namespace EnergyPlus::DataPlant;
//using namespace EnergyPlus::DataZoneEquipment;
//using namespace EnergyPlus::DataSizing;
//using namespace EnergyPlus::FluidProperties;
//
//using namespace EnergyPlus::DataHVACGlobals;
//using namespace EnergyPlus::DataPlant;
//using namespace EnergyPlus::DataSurfaces;
//using namespace EnergyPlus::DataSurfaceLists;
//using namespace EnergyPlus::DataZoneEnergyDemands;
//using namespace EnergyPlus::HeatBalanceManager;
//using namespace EnergyPlus::PlantManager;
//using namespace EnergyPlus::Psychrometrics;
//using namespace EnergyPlus::ScheduleManager;
//using namespace EnergyPlus::SizingManager;
//using namespace EnergyPlus::SurfaceGeometry;
//using namespace EnergyPlus::General;

class CoilCoolingDXTest : public EnergyPlus::EnergyPlusFixture
{
public:

protected:
	void SetUp() override
	{
		EnergyPlus::EnergyPlusFixture::SetUp(); // Sets up the base fixture first.
	}

	std::string getSpeedObjectStrings(const std::string &speedObjectName) {
		std::string const idf_objects = delimited_string( {
		        "Coil:Cooling:DX:CurveFit:Speed, ",
				" " + speedObjectName + ",       ",
				" 0.8,                           ",
				" 0.745,                         ",
				" 3.1415926,                     ",
				" 0.9,                           ",
				" 0.9,                           ",
				" 0.5,                           ",
				" 300,                           ",
				" 6.9,                           ",
				" 0.8,                           ",
				" " + speedObjectName + "CapFT,  ",
				" " + speedObjectName + "CapFF,  ",
				" " + speedObjectName + "EIRFT,  ",
				" " + speedObjectName + "EIRFF,  ",
				" " + speedObjectName + "PLFCurveName, ",
				" 0.6,                           ",
				" " + speedObjectName + "WasteHeatFunctionCurve, ",
				" " + speedObjectName + "SHRFT,  ",
				" " + speedObjectName + "SHRFF;  ",
				"Curve:Biquadratic,              ",
				" " + speedObjectName + "CapFT,  ",
				" 1, 0, 0, 0, 0, 0,              ",
				" 0, 1, 0, 1;                    ",
				"Curve:Linear,                   ",
				" " + speedObjectName + "CapFF,  ",
				" 1, 0,                          ",
				" 0, 1;                          ",
				"Curve:Biquadratic,              ",
				" " + speedObjectName + "EIRFT,  ",
				" 1, 0, 0, 0, 0, 0,              ",
				" 0, 1, 0, 1;                    ",
				"Curve:Linear,                   ",
				" " + speedObjectName + "EIRFF,  ",
				" 1, 0,                          ",
				" 0, 1;                          ",
				"Curve:Linear,                   ",
				" " + speedObjectName + "PLFCurveName, ",
				" 0.85, 0.15,                    ",
				" 0, 1;                          ",
				"Curve:Biquadratic,              ",
				" " + speedObjectName + "WasteHeatFunctionCurve, ",
				" 1, 0, 0, 0, 0, 0,              ",
				" 0, 1, 0, 1;                    ",
				"Curve:Biquadratic,              ",
				" " + speedObjectName + "SHRFT,  ",
				" 1, 0, 0, 0, 0, 0,              ",
				" 0, 1, 0, 1;                    ",
				"Curve:Linear,                   ",
				" " + speedObjectName + "SHRFF,  ",
				" 1, 0,                          ",
				" 0, 1;                          ",
				" "});
		return idf_objects + '\n';
	}

	std::string getModeObjectString(std::string modeName, int numSpeeds) {
		std::vector<std::string> mode_object_lines =
				{
						"Coil:Cooling:DX:CurveFit:OperatingMode, ",
						" " + modeName + ",                      ", // name
						" 12000,                                 ", // rated gross total cooling capacity
						" 100,                                   ", // rated evap air flow rate
						" 200,                                   ", // rated condenser air flow rate
						" 2.5,                                   ", // maximum cycling rate
						" 0.5,                                   ", // ratio for latent cycling
						" 100,                                   ", // latent time constant
						" 300,                                   ", // latent time for removal to begin
						" Yes,                                   ", // apply latent in higher speeds than 1
						" EvaporativelyCooled,                   ", // condenser type
						" 200,                                   ", // evap condenser pump power
						" 5,                                     " // nominal speed num
				};
		std::vector<std::string> speedObjects;
		for (int speedNum = 1; speedNum <= numSpeeds; speedNum++) {
			if (speedNum < numSpeeds) {
				mode_object_lines.push_back(" " + modeName + "Speed" + std::to_string(speedNum) + ",");
			} else {
				mode_object_lines.push_back(" " + modeName + "Speed" + std::to_string(speedNum) + ";");
			}
			speedObjects.push_back(this->getSpeedObjectStrings(modeName + "Speed" + std::to_string(speedNum)));
		}
		std::string const mode_object = delimited_string(mode_object_lines);

		std::string fullObject;
		fullObject = mode_object;
		for (auto & speedObj : speedObjects) {
			fullObject += speedObj;
		}
		return fullObject;
	}

	std::string getPerformanceObjectString(std::string const performanceName, bool addAlternateMode, int numSpeedsPerMode) {
		std::vector<std::string> performance_object_lines =
				{
						"Coil:Cooling:DX:CurveFit:Performance, ",
						" " + performanceName + ",             ",  // name
						" 100,                                 ",  // crankcase heater capacity
						" 0,                                   ",  // min OAT for compressor
						" 1,                                   ",  // max OAT for basin heater
						" 100,                                 ",  // static pressure
						" VariableSpeed,                       ",  // capacity control method
						" 100,                                 ",  // basin heater capacity
						" 400,                                 ",  // basin heater setpoint temp
						" BasinHeaterOpSchedule,               ",  // basin heater operating schedule name
						" Electricity,                         ",  // compressor fuel type
						" BaseOperatingMode,                   "   // base operating mode name
				};

		std::vector<std::string> modeObjects;
		modeObjects.push_back(this->getModeObjectString("BaseOperatingMode", numSpeedsPerMode));
		if (addAlternateMode) {
			performance_object_lines.emplace_back(" AlternateOperatingMode;");
			modeObjects.push_back(this->getModeObjectString("AlternateOperatingMode", numSpeedsPerMode));
		} else {
			performance_object_lines.emplace_back(";");
		}
		std::string const performanceObject = delimited_string(performance_object_lines);

		std::string fullObject = performanceObject;
		for (auto & modeObject : modeObjects) {
			fullObject += modeObject;
		}
		return fullObject;
	}

	std::string getCoilObjectString(std::string const coilName, bool addAlternateMode, int numSpeedsPerMode) {
		std::string coilObject = delimited_string(
				{
						"Coil:Cooling:DX,                     ",
						" " + coilName + ",                   ",  // name
						" EvapInletNode,                      ",  // evap inlet node
						" EvapOutletNode,                     ",  // evap outlet node
						" AvailSchedule,                      ",  // availability schedule name
						" ZoneNameForCondenser,               ",  // condenser zone name
						" CondenserInletNode,                 ",  // condenser inlet node
						" CondenserOutletNode,                ",  // condenser outlet node
						" PerformanceObjectName,              ",  // performance object name
						" CondensateCollectionTankName,       ",  // condensate storage tank name
						" EvaporativeCondenserSupplyTankName; "   // evaporative condenser supply tank name
				});
		std::string performanceObject = this->getPerformanceObjectString("PerformanceObjectName", addAlternateMode, numSpeedsPerMode);
		std::string fullObject = coilObject + performanceObject;
		return fullObject;
	}

	void TearDown() override
	{
		EnergyPlus::EnergyPlusFixture::TearDown(); // Remember to tear down the base fixture after cleaning up derived fixture!
	}
};