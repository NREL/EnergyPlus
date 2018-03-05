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


// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <HybridUnitaryAirConditioners.hh>
// EnergyPlus Headers
#include <HybridEvapCoolingModel.hh>
#include <DataEnvironment.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DataAirSystems.hh>
#include <DataContaminantBalance.hh>
#include <DataEnvironment.hh>
#include <DataGlobalConstants.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataWater.hh>
#include <DataZoneEnergyDemands.hh>
#include <EMSManager.hh>
#include <Fans.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>


#define  TEMP_CURVE 0
#define  W_CURVE 1
#define  POWER_CURVE 2

namespace EnergyPlus {

	namespace HybridUnitaryAirConditioners {
		// Using/Aliasing
		using General::TrimSigDigits;
		using HybridEvapCoolingModel::Model;

		using CurveManager::GetCurveIndex;
		using CurveManager::GetCurveType;
		using CurveManager::GetCurveMinMaxValues;
		using CurveManager::CurveValue;
		using HybridEvapCoolingModel::CMode;

		Array1D< Model > ZoneHybridUnitaryAirConditioner;
		int NumZoneHybridEvap(0);
		Array1D_bool CheckZoneHybridEvapName;
		bool GetInputZoneHybridEvap(true);
		//Begin routines for zone HVAC Hybrid Evaporative cooler unit
		//_______________________________________________________________________________________________________________________
		//***************
		void
		SimZoneHybridUnitaryAirConditioners(
				std::string const & CompName, // name of the packaged terminal heat pump
				int const ZoneNum, // number of zone being served
				Real64 & SensibleOutputProvided, // sensible capacity delivered to zone cooling is negitive
				Real64 & LatentOutputProvided, // Latent add/removal  (kg/s), dehumid = negative
				int & CompIndex // index to zone hvac unit
				)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017
			//       MODIFIED
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			//

			// METHODOLOGY EMPLOYED:
			//

			// REFERENCES:
			// na

			// Using/Aliasing
			using General::TrimSigDigits;
			// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
			int CompNum;
			bool errorsfound = false;
			if (GetInputZoneHybridEvap) {
				GetInputZoneHybridUnitaryAirConditioners(errorsfound);
				GetInputZoneHybridEvap = false;
			}

			if (CompIndex == 0) {
				CompNum = UtilityRoutines::FindItemInList(CompName, ZoneHybridUnitaryAirConditioner);
				if (CompNum == 0) {
					ShowFatalError("SimZoneHybridUnitaryAirConditioners: Zone evaporative cooler unit not found.");
				}
				CompIndex = CompNum;
			}
			else {
				CompNum = CompIndex;
				if (CompNum < 1 || CompNum > NumZoneHybridEvap) {
					ShowFatalError("SimZoneHybridUnitaryAirConditioners: Invalid CompIndex passed=" + TrimSigDigits(CompNum) + ", Number of units =" + TrimSigDigits(NumZoneHybridEvap) + ", Entered Unit name = " + CompName);
				}
				if (CheckZoneHybridEvapName(CompNum)) {
					if (CompName != ZoneHybridUnitaryAirConditioner(CompNum).Name) {
						ShowFatalError("SimZoneHybridUnitaryAirConditioners: Invalid CompIndex passed=" + TrimSigDigits(CompNum) + ", Unit name=" + CompName + ", stored unit name for that index=" + ZoneHybridUnitaryAirConditioner(CompNum).Name);
					}
					CheckZoneHybridEvapName(CompNum) = false;
				}
			}
			try
			{
				InitZoneHybridUnitaryAirConditioners(CompNum, ZoneNum);
			}
			catch (int e)
			{
				ShowFatalError("An exception occurred in InitZoneHybridUnitaryAirConditioners" + TrimSigDigits(CompNum) + ", Unit name=" + CompName + ", stored unit name for that index=" + ZoneHybridUnitaryAirConditioner(CompNum).Name+ ". Please check idf.");
				return;
			}
			try
			{
				CalcZoneHybridUnitaryAirConditioners(CompNum, ZoneNum, SensibleOutputProvided, LatentOutputProvided);
			}
			catch (int e)
			{
				ShowFatalError("An exception occurred in CalcZoneHybridUnitaryAirConditioners" + TrimSigDigits(CompNum) + ", Unit name=" + CompName + ", stored unit name for that index=" + ZoneHybridUnitaryAirConditioner(CompNum).Name + ". Please check idf.");
				return;
			}
			try
			{
				ReportZoneHybridUnitaryAirConditioners(CompNum);
			}
			catch (int e)
			{
				ShowFatalError("An exception occurred in ReportZoneHybridUnitaryAirConditioners" + TrimSigDigits(CompNum) + ", Unit name=" + CompName + ", stored unit name for that index=" + ZoneHybridUnitaryAirConditioner(CompNum).Name + ". Please check idf.");
				return;
			}
		}


		void
		InitZoneHybridUnitaryAirConditioners(
				int const UnitNum, // unit number
				int const ZoneNum // number of zone being served
				)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017
			//       MODIFIED
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			//

			// METHODOLOGY EMPLOYED:
			//

			// REFERENCES:
			// na

			// Using/Aliasing
			using namespace DataLoopNode;
			using namespace Psychrometrics;
			using Fans::GetFanVolFlow;

			// Locals
			static Array1D_bool MySizeFlag;

			static bool HybridCoolOneTimeFlag(true); // one time flag
			static Array1D_bool MyEnvrnFlag;
			static Array1D_bool MyFanFlag;
			static Array1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers


			int InletNode;


			if (HybridCoolOneTimeFlag) {
				MySizeFlag.dimension(NumZoneHybridEvap, true);
				MyEnvrnFlag.dimension(NumZoneHybridEvap, true);
				MyFanFlag.dimension(NumZoneHybridEvap, true);
				MyZoneEqFlag.allocate(NumZoneHybridEvap);
				MyZoneEqFlag = true;
				HybridCoolOneTimeFlag = false;
			}
			if(!ZoneHybridUnitaryAirConditioner(UnitNum).Initialized)
			{
				ZoneHybridUnitaryAirConditioner(UnitNum).Initialize(ZoneNum);
			}
			ZoneHybridUnitaryAirConditioner(UnitNum).RequestedLoadToHeatingSetpoint = 0;
			ZoneHybridUnitaryAirConditioner(UnitNum).RequestedLoadToCoolingSetpoint = 0;
			ZoneHybridUnitaryAirConditioner(UnitNum).RequestedHumdificationMass = 0;
			ZoneHybridUnitaryAirConditioner(UnitNum).RequestedHumdificationLoad = 0;
			ZoneHybridUnitaryAirConditioner(UnitNum).RequestedHumdificationEnergy = 0;
			ZoneHybridUnitaryAirConditioner(UnitNum).RequestedDeHumdificationMass = 0;
			ZoneHybridUnitaryAirConditioner(UnitNum).RequestedDeHumdificationLoad = 0;
			ZoneHybridUnitaryAirConditioner(UnitNum).RequestedDeHumdificationEnergy = 0;

			ZoneHybridUnitaryAirConditioner(UnitNum).UnitTotalCoolingRate = 0.0;
			ZoneHybridUnitaryAirConditioner(UnitNum).UnitTotalCoolingEnergy = 0.0;

			ZoneHybridUnitaryAirConditioner(UnitNum).UnitSensibleCoolingRate = 0.0;
			ZoneHybridUnitaryAirConditioner(UnitNum).UnitSensibleCoolingEnergy = 0.0;
			ZoneHybridUnitaryAirConditioner(UnitNum).UnitLatentCoolingRate = 0.0;
			ZoneHybridUnitaryAirConditioner(UnitNum).UnitLatentCoolingEnergy = 0.0;

			ZoneHybridUnitaryAirConditioner(UnitNum).InitializeModelParams();
			// Do the following initializations (every time step): This should be the info from
			// the previous components outlets or the node data in this section.

			//Transfer the node data to EvapCond data structure
			InletNode = ZoneHybridUnitaryAirConditioner(UnitNum).InletNode;
			ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate = Node(InletNode).MassFlowRate;

			//Set all of the inlet state variables from the inlet nodes
			ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp = Node(InletNode).Temp;
			ZoneHybridUnitaryAirConditioner(UnitNum).InletHumRat = Node(InletNode).HumRat;
			ZoneHybridUnitaryAirConditioner(UnitNum).InletEnthalpy = Node(InletNode).Enthalpy;
			ZoneHybridUnitaryAirConditioner(UnitNum).InletPressure = Node(InletNode).Press;
			ZoneHybridUnitaryAirConditioner(UnitNum).InletRH = PsyRhFnTdbWPb(ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp, ZoneHybridUnitaryAirConditioner(UnitNum).InletHumRat, ZoneHybridUnitaryAirConditioner(UnitNum).InletPressure, "InitZoneHybridUnitaryAirConditioners");

			//Set default outlet state to inlet states, just to be safe
			ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp = ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp;
			ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat = ZoneHybridUnitaryAirConditioner(UnitNum).InletHumRat;
			ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy = ZoneHybridUnitaryAirConditioner(UnitNum).InletEnthalpy;
			ZoneHybridUnitaryAirConditioner(UnitNum).OutletPressure = ZoneHybridUnitaryAirConditioner(UnitNum).InletPressure;
			ZoneHybridUnitaryAirConditioner(UnitNum).OutletRH = PsyRhFnTdbWPb(ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp, ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat, ZoneHybridUnitaryAirConditioner(UnitNum).OutletPressure, "InitZoneHybridUnitaryAirConditioners");
			ZoneHybridUnitaryAirConditioner(UnitNum).OutletMassFlowRate = ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate;

			ZoneHybridUnitaryAirConditioner(UnitNum).SecInletTemp = Node(ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).Temp;
			ZoneHybridUnitaryAirConditioner(UnitNum).SecInletHumRat = Node(ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).HumRat;
			ZoneHybridUnitaryAirConditioner(UnitNum).SecInletEnthalpy = Node(ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).Enthalpy;
			ZoneHybridUnitaryAirConditioner(UnitNum).SecInletPressure = Node(ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).Press;
			ZoneHybridUnitaryAirConditioner(UnitNum).SecInletRH = PsyRhFnTdbWPb(ZoneHybridUnitaryAirConditioner(UnitNum).SecInletTemp, ZoneHybridUnitaryAirConditioner(UnitNum).SecInletHumRat, ZoneHybridUnitaryAirConditioner(UnitNum).SecInletPressure, "InitZoneHybridUnitaryAirConditioners");
		}


		void
		CalcZoneHybridUnitaryAirConditioners(
				int const UnitNum, // unit number
				int const ZoneNum, // number of zone being served
				Real64 & SensibleOutputProvided, // sensible capacity delivered to zone cooling negitive
				Real64 & LatentOutputProvided // Latent add/removal  (kg/s), dehumid = negative
				)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017
			//       MODIFIED
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			//

			// METHODOLOGY EMPLOYED:
			//

			// REFERENCES:
			// na

			// Using/Aliasing
			using DataZoneEnergyDemands::ZoneSysEnergyDemand;
			using DataZoneEnergyDemands::ZoneSysMoistureDemand;
			using namespace DataLoopNode;
			using namespace Psychrometrics;
			using DataEnvironment::StdRhoAir;

			Real64 EnvDryBulbT, AirTempRoom, EnvRelHumm, RoomRelHum, DesignMinVR;

			Real64 ZoneCoolingLoad = ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP; // Remaining load required to meet cooling setpoint (<0 is a cooling load)
			Real64 ZoneHeatingLoad = ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP; // Remaining load required to meet heating setpoint (>0 is a heating load)
			Real64 OutputRequiredToHumidify= ZoneSysMoistureDemand(ZoneNum).OutputRequiredToHumidifyingSP; // Load required to meet humidifying setpoint (>0 = a humidify load) [kgWater/s]

			Real64 OutputRequiredToDehumidify =ZoneSysMoistureDemand(ZoneNum).OutputRequiredToDehumidifyingSP; // Load required to meet dehumidifying setpoint (<0 = a dehumidify load)  [kgWater/s]

			SensibleOutputProvided = 0;
			LatentOutputProvided = 0;
			// taking class members out of the object and then using them in the calcualtion is odd but its for clarity with unit testing.
			EnvDryBulbT = ZoneHybridUnitaryAirConditioner(UnitNum).SecInletTemp;// degrees C
			AirTempRoom = ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp;//degrees C
			EnvRelHumm = ZoneHybridUnitaryAirConditioner(UnitNum).SecInletRH;//RH
			RoomRelHum = ZoneHybridUnitaryAirConditioner(UnitNum).InletRH;//RH

			bool UseOccSchFlag = 1;
			bool UseMinOASchFlag = 1;

			using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;
			DesignMinVR = CalcDesignSpecificationOutdoorAir(ZoneHybridUnitaryAirConditioner(UnitNum).OARequirementsPtr, ZoneNum, UseOccSchFlag, UseMinOASchFlag); //[m3/s]
			Real64 DesignMinVRMassFlow = 0;
			if (StdRhoAir > 1)
			{
				DesignMinVRMassFlow = DesignMinVR * StdRhoAir;
			}
			else
			{
				DesignMinVRMassFlow = DesignMinVR *  1.225;
			}
			ZoneHybridUnitaryAirConditioner(UnitNum).doStep( ZoneCoolingLoad, ZoneHeatingLoad, OutputRequiredToHumidify, OutputRequiredToDehumidify, DesignMinVRMassFlow);
			SensibleOutputProvided = -(ZoneHybridUnitaryAirConditioner(UnitNum).QSensZoneOut); // cooling negitive
			
			LatentOutputProvided = -(ZoneHybridUnitaryAirConditioner(UnitNum).QLatentZoneOutMass);// dehumidification negitive kg/s
		}

		void
		ReportZoneHybridUnitaryAirConditioners(int const UnitNum)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017
			//       MODIFIED
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			//

			// METHODOLOGY EMPLOYED:
			//

			// REFERENCES:
			// na

			// Using/Aliasing
			using namespace DataLoopNode;
			using namespace Psychrometrics;
			ZoneHybridUnitaryAirConditioner(UnitNum).PrimaryMode = ZoneHybridUnitaryAirConditioner(UnitNum).PrimaryMode;
			Node(ZoneHybridUnitaryAirConditioner(UnitNum).InletNode).MassFlowRate =  ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate;
			Node(ZoneHybridUnitaryAirConditioner(UnitNum).InletNode).MassFlowRate = ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate;
			Node(ZoneHybridUnitaryAirConditioner(UnitNum).OutletNode).Temp =         ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp;
			Node(ZoneHybridUnitaryAirConditioner(UnitNum).OutletNode).HumRat =       ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat;
			Node(ZoneHybridUnitaryAirConditioner(UnitNum).OutletNode).MassFlowRate = ZoneHybridUnitaryAirConditioner(UnitNum).OutletMassFlowRate;
			Node(ZoneHybridUnitaryAirConditioner(UnitNum).OutletNode).Enthalpy = ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy;
		}

		void
		GetInputZoneHybridUnitaryAirConditioners(bool & Errors)
		{
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Spencer Maxwell Dutton
			//       DATE WRITTEN   October 2017
			//       MODIFIED
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			//

			// METHODOLOGY EMPLOYED:
			//

			// REFERENCES:
			// na

			// Using/Aliasing
			using BranchNodeConnections::TestCompSet;
			using namespace ScheduleManager;
			using DataGlobals::ScheduleAlwaysOn;
			using NodeInputManager::GetOnlySingleNode;
			using BranchNodeConnections::SetUpCompSets;
			using namespace DataIPShortCuts; // Data for field names, blank numerics
			using namespace DataLoopNode;
			using DataSizing::OARequirements; // to find DesignSpecification:OutdoorAir pointer
			std::string CurrentModuleObject; // Object type for getting and error messages
			Array1D_string Alphas; // Alpha items for object
			Array1D< Real64 > Numbers; // Numeric items for object
			Array1D_string cAlphaFields; // Alpha field names
			Array1D_string cNumericFields; // Numeric field names
			Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
			Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
			int NumAlphas; // Number of Alphas for each GetObjectItem call
			int NumNumbers; // Number of Numbers for each GetObjectItem call
			int MaxAlphas; // Maximum number of alpha fields in all objects
			int MaxNumbers; // Maximum number of numeric fields in all objects
			int NumFields; // Total number of fields in object
			int IOStatus; // Used in GetObjectItem
			static bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
			bool IsNotOK; // Flag to verify name
			bool IsBlank; // Flag for blank name
			int UnitLoop;

						 // SUBROUTINE PARAMETER DEFINITIONS:
			static std::string const RoutineName("GetInputZoneEvaporativeCoolerUnit: ");
			MaxNumbers = 0;
			MaxAlphas = 0;
			CurrentModuleObject = "ZoneHVAC:HybridUnitaryHVAC";
			NumZoneHybridEvap = inputProcessor->getNumObjectsFound(CurrentModuleObject);
			inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumFields, NumAlphas, NumNumbers);
			MaxNumbers = max(MaxNumbers, NumNumbers);
			MaxAlphas = max(MaxAlphas, NumAlphas);
			Alphas.allocate(MaxAlphas);
			Numbers.dimension(MaxNumbers, 0.0);
			cAlphaFields.allocate(MaxAlphas);
			cNumericFields.allocate(MaxNumbers);
			lAlphaBlanks.dimension(MaxAlphas, true);
			lNumericBlanks.dimension(MaxNumbers, true);
			std::vector<std::string> test;
			std::vector<bool> blanks;

			if (NumZoneHybridEvap > 0) {
				CheckZoneHybridEvapName.dimension(NumZoneHybridEvap, true);
				ZoneHybridUnitaryAirConditioner.allocate(NumZoneHybridEvap);

				for (UnitLoop = 1; UnitLoop <= NumZoneHybridEvap; ++UnitLoop) {
					inputProcessor->getObjectItem(CurrentModuleObject, UnitLoop, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields);

					IsNotOK = false;
					IsBlank = false;
					UtilityRoutines::VerifyName(Alphas(1), ZoneHybridUnitaryAirConditioner, UnitLoop - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name");

					//A1, \field Name
					ZoneHybridUnitaryAirConditioner(UnitLoop).Name = Alphas(1);
					//A2, \field Availability Schedule Name
					ZoneHybridUnitaryAirConditioner(UnitLoop).Schedule = Alphas(2);
					if (lAlphaFieldBlanks(2)) {
						ZoneHybridUnitaryAirConditioner(UnitLoop).SchedPtr = ScheduleAlwaysOn;
					}
					else {
						ZoneHybridUnitaryAirConditioner(UnitLoop).SchedPtr = GetScheduleIndex(Alphas(2));
						if (ZoneHybridUnitaryAirConditioner(UnitLoop).SchedPtr == 0) {
							ShowSevereError("Invalid " + cAlphaFieldNames(2) + '=' + Alphas(2));
							ShowContinueError("Entered in " + cCurrentModuleObject + '=' + Alphas(1));
							ErrorsFound = true;
						}
					}
					//A3, \field Availability Manager List Name

					//A4, \field Minimum Supply Air Temperature Schedule Named
					ZoneHybridUnitaryAirConditioner(UnitLoop).TsaMin_schedule_pointer = GetScheduleIndex(Alphas(4));
					if (ZoneHybridUnitaryAirConditioner(UnitLoop).TsaMin_schedule_pointer == 0) {
						ShowSevereError("Invalid " + cAlphaFields(4) + '=' + Alphas(4));
						ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
						ErrorsFound = true;
					}
					//A5, \field Maximum Supply Air Temperature Schedule Name
					ZoneHybridUnitaryAirConditioner(UnitLoop).TsaMax_schedule_pointer = GetScheduleIndex(Alphas(5));
					if (ZoneHybridUnitaryAirConditioner(UnitLoop).TsaMax_schedule_pointer == 0) {
						ShowSevereError("Invalid " + cAlphaFields(5) + '=' + Alphas(5));
						ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
						ErrorsFound = true;
					}
					//A6, \field Minimum Supply Air Humidity Ratio Schedule Name
					ZoneHybridUnitaryAirConditioner(UnitLoop).RHsaMin_schedule_pointer = GetScheduleIndex(Alphas(6));
					if (ZoneHybridUnitaryAirConditioner(UnitLoop).RHsaMin_schedule_pointer == 0) {
						ShowSevereError("Invalid " + cAlphaFields(6) + '=' + Alphas(6));
						ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
						ErrorsFound = true;
					}
					//A7, \field Maximum Supply Air Humidity Ratio Schedule Name
					ZoneHybridUnitaryAirConditioner(UnitLoop).RHsaMax_schedule_pointer = GetScheduleIndex(Alphas(7));
					if (ZoneHybridUnitaryAirConditioner(UnitLoop).RHsaMax_schedule_pointer == 0) {
						ShowSevereError("Invalid " + cAlphaFields(7) + '=' + Alphas(7));
						ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
						ErrorsFound = true;
					}

					//A8, \field Method to Choose Value of Controlled Inputs

					//A9, \field Return Air Node Name
					//A10, \field Outdoor Air Node Name
					//A11, \field Supply Air Node Name
					//A12, \field Relief Node Name
					ZoneHybridUnitaryAirConditioner(UnitLoop).InletNode = GetOnlySingleNode(Alphas(9), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent);
					ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryInletNode = GetOnlySingleNode(Alphas(10), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent);
					ZoneHybridUnitaryAirConditioner(UnitLoop).OutletNode = GetOnlySingleNode(Alphas(11), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent);
					ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryOutletNode = GetOnlySingleNode(Alphas(12), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_ReliefAir, 1, ObjectIsNotParent);
					TestCompSet(CurrentModuleObject, Alphas(1), Alphas(9), Alphas(11), "Hybrid Evap Air Zone Nodes");
					TestCompSet(CurrentModuleObject, Alphas(1), Alphas(10), Alphas(12), "Hybrid Evap Air Zone Secondary Nodes");

					//N1, \field System Maximum Supply AirFlow Rate
					ZoneHybridUnitaryAirConditioner(UnitLoop).SystemMaximumSupplyAirFlowRate = Numbers(1);

					//N2, \field External Static Pressure at System Maximum Supply Air Flow Rate

					//In each time step, the result for system power, fan power, gas use, water user, or supply airflow rate will be determined as :
					//TableValue * SysMaxSupply * ScalingFactor
					//N3, \field Scaling Factor
					ZoneHybridUnitaryAirConditioner(UnitLoop).ScalingFactor = Numbers(3);
					// the two numbers above are used to generate a overal scaling factor
					if (DataEnvironment::StdRhoAir > 1)
					{
						// SystemMaximumSupplyAirFlowRate*ScalingFactor*AirDensity;
						ZoneHybridUnitaryAirConditioner(UnitLoop).ScaledSystemMaximumSupplyAirMassFlowRate = Numbers(1)*Numbers(3)*DataEnvironment::StdRhoAir;
					}
					else
					{
						ZoneHybridUnitaryAirConditioner(UnitLoop).ScaledSystemMaximumSupplyAirMassFlowRate = Numbers(1)*Numbers(3)*1.225;
					}

						//N4, \field Number of Operating Modes
					int Numberofoperatingmodes = 0;
					if (lNumericBlanks(4)) {
						ShowSevereError("Invalid number of operating modes" + cNumericFields(5)  );
						ShowFatalError(RoutineName + "Errors found in getting input.");
						ShowContinueError("... Preceding condition causes terminascaler*1.2041*pZoneHybridUnitaryAirConditioner->SystemMaximumSupplyAirFlowRatetion.");
					}
					else {
						Numberofoperatingmodes= Numbers(4)-1; //zero based count
					}
						//N5, \field Minimum Time Between Mode Change
						//A13, \field First fuel type
						//A14, \field Second fuel type
						//A15, \field Third fuel type
						//A16, \field Objective Function Minimizes

						//A17, \ OA requiremnt pointer
					ZoneHybridUnitaryAirConditioner(UnitLoop).OARequirementsPtr = UtilityRoutines::FindItemInList(Alphas(17), OARequirements);
					if (ZoneHybridUnitaryAirConditioner(UnitLoop).OARequirementsPtr == 0) {
						ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + Alphas(1) + " invalid data");
						ShowContinueError("Invalid-not found" + cAlphaFieldNames(17) + "=\"" + Alphas(17) + "\".");
						ErrorsFound = true;
					}
					else {
						ZoneHybridUnitaryAirConditioner(UnitLoop).OutdoorAir = true;
					}

					for (int modeIter = 0; modeIter <= Numberofoperatingmodes; ++modeIter) {
						ErrorsFound = ZoneHybridUnitaryAirConditioner(UnitLoop).ParseMode(Alphas, cAlphaFields, Numbers, cNumericFields, lAlphaBlanks, cCurrentModuleObject);
						if (ErrorsFound) {
							ShowFatalError(RoutineName + "Errors found parsing modes");
							ShowContinueError("... Preceding condition causes termination.");
							break;
						}
					}

				}
			}
			// setup output variables
			for (UnitLoop = 1; UnitLoop <= NumZoneHybridEvap; ++UnitLoop) {

				SetUpCompSets(CurrentModuleObject, ZoneHybridUnitaryAirConditioner(UnitLoop).Name, CurrentModuleObject, ZoneHybridUnitaryAirConditioner(UnitLoop).Name, NodeID(ZoneHybridUnitaryAirConditioner(UnitLoop).InletNode), NodeID(ZoneHybridUnitaryAirConditioner(UnitLoop).OutletNode));
				SetUpCompSets(CurrentModuleObject, ZoneHybridUnitaryAirConditioner(UnitLoop).Name, CurrentModuleObject, ZoneHybridUnitaryAirConditioner(UnitLoop).Name, NodeID(ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryInletNode), NodeID(ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryOutletNode));

				SetUpCompSets(CurrentModuleObject, ZoneHybridUnitaryAirConditioner(UnitLoop).Name, CurrentModuleObject, ZoneHybridUnitaryAirConditioner(UnitLoop).Name, NodeID(ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryInletNode), NodeID(ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryOutletNode));

				SetupOutputVariable("Zone Hybrid Unitary HVAC System Total Cooling Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).SystemTotalCoolingRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC System Total Cooling Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).SystemTotalCoolingEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System");
				SetupOutputVariable("Zone Hybrid Unitary HVAC System Sensible Cooling Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).SystemSensibleCoolingRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC System Sensible Cooling Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).SystemSensibleCoolingEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System");
				SetupOutputVariable("Zone Hybrid Unitary HVAC System Latent Cooling Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).SystemLatentCoolingRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC System Latent Cooling Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).SystemLatentCoolingEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System");

				SetupOutputVariable("Zone Hybrid Unitary HVAC Zone Total Cooling Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).UnitTotalCoolingRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Zone Total Cooling Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).UnitTotalCoolingEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System");
				SetupOutputVariable("Zone Hybrid Unitary HVAC Zone Sensible Cooling Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).UnitSensibleCoolingRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Zone Sensible Cooling Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).UnitSensibleCoolingEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System");
				SetupOutputVariable("Zone Hybrid Unitary HVAC Zone Latent Cooling Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).UnitLatentCoolingRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Zone Latent Cooling Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).UnitLatentCoolingEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System");

				SetupOutputVariable("Zone Hybrid Unitary HVAC System Total Heating Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).SystemTotalHeatingRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC System Total Heating Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).SystemTotalHeatingEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "ENERGYTRANSFER", "HeatingCOILS", _, "System");
				SetupOutputVariable("Zone Hybrid Unitary HVAC System Sensible Heating Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).SystemSensibleHeatingRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC System Sensible Heating Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).SystemSensibleHeatingEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "ENERGYTRANSFER", "HeatingCOILS", _, "System");
				SetupOutputVariable("Zone Hybrid Unitary HVAC System Latent Heating Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).SystemLatentHeatingRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC System Latent Heating Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).SystemLatentHeatingEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "ENERGYTRANSFER", "HeatingCOILS", _, "System");

				SetupOutputVariable("Zone Hybrid Unitary HVAC Zone Total Heating Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).UnitTotalHeatingRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Zone Total Heating Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).UnitTotalHeatingEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "ENERGYTRANSFER", "HeatingCOILS", _, "System");
				SetupOutputVariable("Zone Hybrid Unitary HVAC Zone Sensible Heating Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).UnitSensibleHeatingRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Zone Sensible Heating Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).UnitSensibleHeatingEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "ENERGYTRANSFER", "HeatingCOILS", _, "System");
				SetupOutputVariable("Zone Hybrid Unitary HVAC Zone Latent Heating Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).UnitLatentHeatingRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Zone Latent Heating Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).UnitLatentHeatingEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System");

				SetupOutputVariable("Zone Hybrid Unitary HVAC Predicted Sensible Load to Setpoint Heat Transfer Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedLoadToCoolingSetpoint, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Error Code", OutputProcessor::Unit::None, ZoneHybridUnitaryAirConditioner(UnitLoop).ErrorCode, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

				SetupOutputVariable("Zone Hybrid Unitary HVAC Supply Air Temperature", OutputProcessor::Unit::C, ZoneHybridUnitaryAirConditioner(UnitLoop).OutletTemp, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Return Air Temperature", OutputProcessor::Unit::C, ZoneHybridUnitaryAirConditioner(UnitLoop).InletTemp, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Outdoor Air Temperature", OutputProcessor::Unit::C, ZoneHybridUnitaryAirConditioner(UnitLoop).SecInletTemp, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

				SetupOutputVariable("Zone Hybrid Unitary HVAC Supply Air Humidity Ratio", OutputProcessor::Unit::kgWater_kgDryAir, ZoneHybridUnitaryAirConditioner(UnitLoop).OutletHumRat, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Return Air Humidity Ratio", OutputProcessor::Unit::kgWater_kgDryAir, ZoneHybridUnitaryAirConditioner(UnitLoop).InletHumRat, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Outdoor Air Humidity Ratio", OutputProcessor::Unit::kgWater_kgDryAir, ZoneHybridUnitaryAirConditioner(UnitLoop).SecInletHumRat, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

				SetupOutputVariable("Zone Hybrid Unitary HVAC Supply Air Relative Humidity", OutputProcessor::Unit::Perc, ZoneHybridUnitaryAirConditioner(UnitLoop).OutletRH, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Return Air Relative Humidity", OutputProcessor::Unit::Perc, ZoneHybridUnitaryAirConditioner(UnitLoop).InletRH, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Outdoor Air Relative Humidity", OutputProcessor::Unit::Perc, ZoneHybridUnitaryAirConditioner(UnitLoop).SecInletRH, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

				SetupOutputVariable("Zone Hybrid Unitary HVAC Supply Air Mass Flow Rate", OutputProcessor::Unit::kg_s, ZoneHybridUnitaryAirConditioner(UnitLoop).OutletMassFlowRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Supply Air Standard Density Volume Flow Rate", OutputProcessor::Unit::m3_s, ZoneHybridUnitaryAirConditioner(UnitLoop).OutletVolumetricFlowRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Ventilation Air Standard Density Volume Flow Rate", OutputProcessor::Unit::m3_s, ZoneHybridUnitaryAirConditioner(UnitLoop).SupplyVentilationVolume, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Electric Power", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).FinalElectricalPower, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Electric Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).FinalElectricalEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "Electric", "Cooling", "Hybrid HVAC Cooling", "System");

				SetupOutputVariable("Zone Hybrid Unitary HVAC Requested Outdoor Air Ventilation Mass Flow Rate", OutputProcessor::Unit::kg_s, ZoneHybridUnitaryAirConditioner(UnitLoop).MinOA_Msa, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Ventilation Air Mass Flow Rate", OutputProcessor::Unit::kg_s, ZoneHybridUnitaryAirConditioner(UnitLoop).SupplyVentilationAir, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Availability Status", OutputProcessor::Unit::None, ZoneHybridUnitaryAirConditioner(UnitLoop).UnitOn, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Outdoor Air Fraction", OutputProcessor::Unit::None, ZoneHybridUnitaryAirConditioner(UnitLoop).averageOSAF, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

				SetupOutputVariable("Zone Hybrid Unitary HVAC Dehumidification Load to Humidistat Setpoint Moisture Transfer Rate", OutputProcessor::Unit::kg_s, ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedDeHumdificationMass, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Dehumidification Load to Humidistat Setpoint Heat Transfer Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedDeHumdificationLoad, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC DehumidificationLoad to Humidistat Setpoint Heat Tansfer Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedDeHumdificationEnergy, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

				SetupOutputVariable("Zone Hybrid Unitary HVAC Humidification Load to Humidistat Setpoint Moisture Transfer Rate", OutputProcessor::Unit::kg_s, ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedHumdificationMass, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Humidification Load to Humidistat Setpoint Heat Transfer Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedHumdificationLoad, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Humidification Load to Humidistat Setpoint Heat Tansfer Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedHumdificationEnergy, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);


				SetupOutputVariable("Zone Hybrid Unitary HVAC Supply Fan Electric Power", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).SupplyFanElectricPower, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Supply Fan Electric Energy", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).SupplyFanElectricEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "Electric", "Fans", "Hybrid HVAC Fans", "System");
				SetupOutputVariable("Zone Hybrid Unitary HVAC Secondary Fuel Consumption Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryFuelConsumptionRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Secondary Fuel Consumption", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryFuelConsumption, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Third Fuel Consumption Rate", OutputProcessor::Unit::W, ZoneHybridUnitaryAirConditioner(UnitLoop).ThirdFuelConsumptionRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Third Fuel Consumption", OutputProcessor::Unit::J, ZoneHybridUnitaryAirConditioner(UnitLoop).ThirdFuelConsumption, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC Water Consumption Rate", OutputProcessor::Unit::kgWater_s, ZoneHybridUnitaryAirConditioner(UnitLoop).WaterConsumptionRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				//make sure units above are right.
				SetupOutputVariable("Zone Hybrid Unitary HVAC Water Consumption", OutputProcessor::Unit::m3, ZoneHybridUnitaryAirConditioner(UnitLoop).WaterConsumption, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Hybrid Unitary HVAC External Static Pressure", OutputProcessor::Unit::Pa, ZoneHybridUnitaryAirConditioner(UnitLoop).ExternalStaticPressure, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

				int index=0;

				for (auto & thisSetting : ZoneHybridUnitaryAirConditioner(UnitLoop).CurrentOperatingSettings) {
					SetupOutputVariable("Zone Hybrid Unitary HVAC Runtime Fraction in Setting " + std::to_string(index), OutputProcessor::Unit::None, thisSetting.Runtime_Fraction,"Zone", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
					SetupOutputVariable("Zone Hybrid Unitary HVAC Mode in Setting " + std::to_string(index), OutputProcessor::Unit::None,thisSetting.Mode,"Zone", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
					SetupOutputVariable("Zone Hybrid Unitary HVAC Outdoor Air Fraction in Setting " + std::to_string(index),OutputProcessor::Unit::kg_s, thisSetting.Outdoor_Air_Fraction,"Zone", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
					SetupOutputVariable("Zone Hybrid Unitary HVAC Supply Air Mass Flow Rate in Setting " + std::to_string(index), OutputProcessor::Unit::kg_s, thisSetting.Unscaled_Supply_Air_Mass_Flow_Rate,"Zone", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
					SetupOutputVariable("Zone Hybrid Unitary HVAC Supply Air Mass Flow Rate Ratio in Setting " + std::to_string(index), OutputProcessor::Unit::None, thisSetting.Supply_Air_Mass_Flow_Rate_Ratio,"Zone", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
					index++;
				}
			}
			Errors = ErrorsFound;
			if (ErrorsFound) {
				ShowFatalError(RoutineName + "Errors found in getting input.");
				ShowContinueError("... Preceding condition causes termination.");
			}
		}


	}
}

