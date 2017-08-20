
#include <HybridEvapCoolingModel.hh>
#include <CurveManager.hh>


#include <EnergyPlus/General.hh>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/HybridUnitaryAirConditioners.hh>
#include <FileSystem.hh>
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <ConfiguredFunctions.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/Psychrometrics.hh>

using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::DataContaminantBalance;
using namespace EnergyPlus::DataAirLoop;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataZoneControls;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::Humidifiers;
using namespace EnergyPlus::SizingManager;

using namespace EnergyPlus::DataContaminantBalance;
using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus;
using namespace EnergyPlus::SizingManager;
using EnergyPlus::CurveManager::CurveValue;
using EnergyPlus::CurveManager::GetNormalPoint;
using EnergyPlus::CurveManager::GetCurveName;
using EnergyPlus::Psychrometrics::PsyHFnTdbRhPb;
using EnergyPlus::Psychrometrics::PsyRhFnTdbWPb;
using namespace EnergyPlus::ScheduleManager;
using EnergyPlus::HybridEvapCoolingModel::CMode;
using EnergyPlus::HybridEvapCoolingModel::CSetting;
using EnergyPlus::HybridEvapCoolingModel::Model;
using namespace EnergyPlus::HybridUnitaryAirConditioners;

namespace EnergyPlus {
	std::vector<std::string> getAllLinesInFile2(std::string filePath) {
		std::ifstream infile(filePath);
		std::vector<std::string> lines;
		std::string line;
		while (std::getline(infile, line))
		{
			lines.push_back(line);
		}
		return lines;
	}

	std::vector<std::string> parseLine(std::string line)
	{
		std::vector<std::string> vect;
		std::stringstream ss(line);
		std::string token;

		while (std::getline(ss, token, ',')) {
			vect.push_back(token);
		}
		return vect;
	}


	TEST_F(EnergyPlusFixture, Test_UnitaryHybridAirConditioner_Calculation) {


		using namespace InputProcessor;
		std::vector<std::string> snippet = getAllLinesInFile2(configured_source_directory() + "/datasets/Example_1Zone_1Hybrid_MuntersEPX5000_rev0806_nolimits.idf");
		std::string string = delimited_string(snippet);
		ASSERT_FALSE(process_idf(string));

		#define  TEMP_CURVE 0 
		#define  W_CURVE 1
		#define  POWER_CURVE 2

		bool ErrorsFound(false);
		GetZoneData(ErrorsFound);
		EXPECT_FALSE(ErrorsFound);
		// Initialize schedule values
		DataGlobals::TimeStep = 1;
		DataHVACGlobals::TimeStepSys = 1;
		DataGlobals::NumOfTimeStepInHour = 1;
		DataGlobals::MinutesPerTimeStep = 60;
		DataEnvironment::Month = 1;
		DataEnvironment::DayOfMonth = 21;
		DataGlobals::HourOfDay = 1;
		DataEnvironment::DSTIndicator = 0;
		DataEnvironment::DayOfWeek = 2;
		DataEnvironment::HolidayIndex = 0;
		using General::JulianDay;
		DataGlobals::WarmupFlag = false;
		//StdRhoAir
		DataEnvironment::DayOfYear_Schedule = JulianDay(Month, DayOfMonth, 1);

		ScheduleManager::UpdateScheduleValues();
		Real64 SysMassFlow(0.0); // System supply mass flow rate [kg/s]
		Real64 OAMassFlow(0.0); // OA mass flow rate [kg/s]
								// Initialize zone areas and volumes - too many other things need to be set up to do these in the normal routines

		DataHeatBalance::Zone(1).FloorArea = 232.26;
		DataEnvironment::StdRhoAir = 1.225;
		DataHeatBalance::ZoneIntGain.allocate(1);
		//DataHeatBalance::ZoneIntGain(1).NOFOCC = 10;

		SizingManager::GetOARequirements();
		GetOAControllerInputs();
		using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;
		bool UseOccSchFlag = true;
		bool UseMinOASchFlag = true;
		int OARequirementsPtr = 1;

		//other stuff
		CurveManager::GetCurveInputData(ErrorsFound);
		EXPECT_FALSE(ErrorsFound);
		using namespace EnergyPlus::DataEnvironment;

		ProcessScheduleInput(); // read schedules

		UpdateScheduleValues();
		GetInputZoneHybridUnitaryAirConditioners(ErrorsFound);

		GetOARequirements(); // get the OA requirements object
							 //MixedAir::GetOutsideAirSysInputs();
							 //MixedAir::GetOAControllerInputs();
		EXPECT_FALSE(ErrorsFound);
		InitZoneHybridUnitaryAirConditioners(1, 1);
		Model * pZoneHybridUnitaryAirConditioner = HandelToHybridUnitaryAirConditioner(1);

		double  Tosa, Tra, Wra, Wosa, RHosa, RHra, RequestedLoad, DesignMinVR, Requestedheating, RequestedCooling,Requested_Humidification, Requested_Dehumidification;
		RHosa = 0;
		std::string TimeDate;
		int modenumber = 0;
		double MsaRatio, OSAF;
		MsaRatio = OSAF = 1;
		int lineN = 0;

		Requestedheating = RequestedCooling= Requested_Humidification = Requested_Dehumidification = 0;

		std::vector<std::string> lines = getAllLinesInFile2(configured_source_directory() + "/datasets/InputData_mixed.csv"); // configured_source_directory()																									 //line input format: time, Outside Air Temperature(C),	Outside Air Relative Humidity(1 - 100 % ),	Outside Air Humidity Ratio(-)
		double NormalizationReference = 3.0176;
		int Msa = NormalizationReference* MsaRatio;
		int scaler = pZoneHybridUnitaryAirConditioner->ScalingFactor;
		ofstream myfile;
		myfile.open(configured_source_directory() + "/datasets/OutputData.csv");

		std::list<CMode*>::const_iterator iterator_mode;
		std::vector<std::string>::const_iterator iterator_line;
		iterator_line = lines.begin();
		iterator_line++;

		myfile << "Date" << ", Inputs, " << "Requested Sensible Heat Transfer Rate (W)," << "Tosa (C)" << ", " << "Wosa (kgW/kgDryAir)" << ", " << "Minimum OA requested" << ", Outputs" << ",Primary Mode ID" << ", " << "Primary Mode RF" << ", " << "Error code" << ", " << "Msa" << ", " << "Tsa" << ", " << "Wsa" << ", " << "Power" << "," << "Delivered Unit Sensible Cooling (W)" << "," << "Delivered Unit Sensible Heatinging (W)" << "," << "Expected Outputs:,ExpectedSupplyAirTemperature" << "," << "ExpectedSupplyAirHumidityRatio" << "," << "ExpectedTotalElectricPower" << "," << "ExpectedMode" << "," << "ExpectedSupplyAirMassFlowRate" << ", " << "ExpectedSupplyAirVolumeFlowRate" << ", " << "ExpectedSensibleSystemCoolingCapacity" << ", " << "ExpectedLatentSystemCoolingCapacity" << ", " << "ExpectedTotalSystemCoolingCapacity" << ", " << ",\n";


		for (; iterator_line != lines.end(); ++iterator_line) // iterate though the modes.
		{
			std::ostringstream s;
			lineN++;
			if (lineN == 10000) break;
			std::string str = *iterator_line;
			std::vector<std::string> linesplit = parseLine(str);
			TimeDate = linesplit.at(0);
			Tosa = atof((linesplit.at(1)).c_str());
			double RHosa = atof((linesplit.at(2)).c_str());
			Wosa = atof((linesplit.at(3)).c_str());//humidity ratio
			RequestedLoad = 1000 * atof((linesplit.at(4)).c_str());
			Requestedheating = RequestedCooling = 0;
			if (RequestedLoad > 0) Requestedheating = RequestedLoad; //load in W
			if (RequestedLoad<0) RequestedCooling= RequestedLoad;

			double ExpectedSupplyAirTemperature, ExpectedSupplyAirHumidityRatio, ExpectedTotalElectricPower, ExpectedFanPower;
			ExpectedSupplyAirTemperature = atof((linesplit.at(5)).c_str());
			ExpectedSupplyAirHumidityRatio = atof((linesplit.at(7)).c_str());
			ExpectedTotalElectricPower = atof((linesplit.at(9)).c_str());
			std::string ModeName = (linesplit.at(10)).c_str();
			//Supply Air Mass Flow Rate(kg / s)	Supply Air Volume Flow Rate(m3 / s)	Sensible System Cooling Capacity(kW)	Latent System Cooling Capacity(kW)	Total System Cooling Capacity(kW)
			double ExpectedSupplyAirMassFlowRate = atof((linesplit.at(11)).c_str());
			double ExpectedSupplyAirVolumeFlowRate = atof((linesplit.at(12)).c_str());
			double ExpectedSensibleSystemCoolingCapacity = atof((linesplit.at(13)).c_str());
			double ExpectedLatentSystemCoolingCapacity = atof((linesplit.at(14)).c_str());
			double ExpectedTotalSystemCoolingCapacity = atof((linesplit.at(14)).c_str());
			DesignMinVR = ExpectedSupplyAirMassFlowRate;
			Tra = 24;
			Wra = Wosa; //Humidity Ratio kgw/kga)
			RHra = 100 * PsyRhFnTdbWPb(Tra, Wra, 101325);
			double newRHosa = PsyRhFnTdbWPb(Tosa, Wosa, 101325);


			s << TimeDate << ", Inputs," << RequestedLoad << ", " << Tosa << ", " << Wosa << "," << DesignMinVR << ",";

			pZoneHybridUnitaryAirConditioner->InletTemp = Tra;

			pZoneHybridUnitaryAirConditioner->InletHumRat = Wra;
			pZoneHybridUnitaryAirConditioner->InletEnthalpy = PsyHFnTdbRhPb(Tra, RHra / 100, 101325, "test");
			pZoneHybridUnitaryAirConditioner->InletPressure = 101325;
			pZoneHybridUnitaryAirConditioner->InletRH = RHra;// PsyRhFnTdbWPb(pZoneHybridUnitaryAirConditioner->InletTemp, pZoneHybridUnitaryAirConditioner->InletHumRat, pZoneHybridUnitaryAirConditioner->OutletPressure, "InitZoneHybridUnitaryAirConditioners");

			pZoneHybridUnitaryAirConditioner->SecInletTemp = Tosa;
			pZoneHybridUnitaryAirConditioner->SecInletHumRat = Wosa;
			pZoneHybridUnitaryAirConditioner->SecInletEnthalpy = PsyHFnTdbRhPb(Tosa, RHosa / 100, 101325, "test");
			pZoneHybridUnitaryAirConditioner->SecInletPressure = 101325;
			pZoneHybridUnitaryAirConditioner->SecInletRH = RHosa; //PsyRhFnTdbWPb(pZoneHybridUnitaryAirConditioner->SecInletTemp, pZoneHybridUnitaryAirConditioner->SecInletHumRat, pZoneHybridUnitaryAirConditioner->SecInletPressure, "InitZoneHybridUnitaryAirConditioners");
			pZoneHybridUnitaryAirConditioner->Initialize(1);

			// Main simulation step
			pZoneHybridUnitaryAirConditioner->doStep(Tosa, Tra, RHosa / 100, RHra / 100, RequestedCooling, Requestedheating, Requested_Humidification, Requested_Dehumidification, DesignMinVR);
			// output results
			modenumber = pZoneHybridUnitaryAirConditioner->PrimaryMode;
			double primaryRuntime = pZoneHybridUnitaryAirConditioner->PrimaryModeRuntimeFraction;
			double Tsa = pZoneHybridUnitaryAirConditioner->OutletTemp;
			double Wsa = pZoneHybridUnitaryAirConditioner->OutletHumRat;
			double Msa = pZoneHybridUnitaryAirConditioner->OutletMassFlowRate;
			double Y_val = pZoneHybridUnitaryAirConditioner->FinalElectricalPower / 1000;
			double ErrorCode = pZoneHybridUnitaryAirConditioner->ErrorCode;
			double deliveredSC = pZoneHybridUnitaryAirConditioner->UnitSensibleCoolingRate;
			double deliveredSH = pZoneHybridUnitaryAirConditioner->UnitSensibleHeatingRate;
			s << "Outputs, mode:" << modenumber << ", " << primaryRuntime << ", " << ErrorCode << ", " << Msa << ", " << Tsa << ", " << Wsa << "," << Y_val << "," << deliveredSC << "," << deliveredSH << ",";
			s << "Expected Outputs: ," << ExpectedSupplyAirTemperature << "," << ExpectedSupplyAirHumidityRatio << "," << ExpectedTotalElectricPower << "," << ModeName << "," << ExpectedSupplyAirMassFlowRate << "," << ExpectedSupplyAirVolumeFlowRate << "," << ExpectedSensibleSystemCoolingCapacity << "," << ExpectedLatentSystemCoolingCapacity << "," << ExpectedTotalSystemCoolingCapacity;

			s << "\n";
			myfile << s.str();
		}
		myfile.close();


	}

}