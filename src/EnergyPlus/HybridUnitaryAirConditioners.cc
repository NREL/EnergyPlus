// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <HybridUnitaryAirConditioners.hh>

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
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <WaterManager.hh>

#include <HybridModelConfigFile.hh>



namespace EnergyPlus {//***************

	namespace HybridUnitaryAirConditioners {
		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using HybridEvapCoolingModel::Model;
//		using HybridEvapCoolingModel::ZoneHybridUnitaryACSystem;
		//using HybridEvapCoolingModel::ZoneEvapCoolerHybridStruct2;
		//using HybridEvapCoolingModel::Model;
		using HybridModelConfigFile::ConfigFile;
		
		//Array1D<ZoneEvapCoolerHybridStruct2> ZoneHybridUnitaryAirConditioner;
		//Array1D< ZoneHybridUnitaryACSystem > ZoneHybridUnitaryAirConditioner;
		Array1D< Model > ZoneHybridUnitaryAirConditioner;
		int NumZoneHybridEvap(0);
		Array1D_bool CheckZoneHybridEvapName;
		bool GetInputZoneHybridEvap(true);
		ConfigFile* pConfig = new ConfigFile;
		//Begin routines for zone HVAC Hybrid Evaporative cooler unit
		//_______________________________________________________________________________________________________________________
		//***************
		void
			SimZoneHybridUnitaryAirConditioners(
				std::string const & CompName, // name of the packaged terminal heat pump
				int const ZoneNum, // number of zone being served
				Real64 & SensibleOutputProvided, // sensible capacity delivered to zone
				Real64 & LatentOutputProvided, // Latent add/removal  (kg/s), dehumid = negative
				int & CompIndex // index to zone hvac unit
				)
		{
			using InputProcessor::FindItemInList;
			using General::TrimSigDigits;
			// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
			int CompNum;

			if (GetInputZoneHybridEvap) {
				GetInputZoneHybridUnitaryAirConditioners();
				GetInputZoneHybridEvap = false;
			}

			if (CompIndex == 0) {
				CompNum = FindItemInList(CompName, ZoneHybridUnitaryAirConditioner);
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
				cout << "An exception occurred in InitZoneHybridUnitaryAirConditioners. Exception Nr. " << e << '\n';
				return;
			}
			try
			{
				CalcZoneHybridUnitaryAirConditioners(CompNum, ZoneNum, SensibleOutputProvided, LatentOutputProvided);
			}
			catch (int e)
			{
				cout << "An exception occurred in CalcZoneHybridUnitaryAirConditioners. Exception Nr. " << e << '\n';
				return;
			}
			try
			{
				ReportZoneHybridUnitaryAirConditioners(CompNum);
			}
			catch (int e)
			{
				cout << "An exception occurred in ReportZoneHybridUnitaryAirConditioners. Exception Nr. " << e << '\n';
				return;
			}
		}



		void
			InitZoneHybridUnitaryAirConditioners(
				int const UnitNum, // unit number
				int const ZoneNum // number of zone being served
				)
		{

			// Using/Aliasing
			using namespace DataLoopNode;
			using namespace Psychrometrics;
			using DataGlobals::TimeStep;
			using DataGlobals::TimeStepZone;
			using DataGlobals::WarmupFlag;
			using DataGlobals::HourOfDay;
			using DataZoneEquipment::ZoneEquipInputsFilled;
			using DataZoneEquipment::CheckZoneEquipmentList;
			using DataZoneEquipment::ZoneHybridEvaporativeCooler_Num;
			using DataZoneEquipment::ZoneEquipConfig;
			using DataHVACGlobals::ZoneComp;
			using DataHVACGlobals::SysTimeElapsed;
			using DataSizing::AutoSize;
			using DataEnvironment::StdRhoAir;
			using Fans::GetFanVolFlow;

			// Locals
			static Array1D_bool MySizeFlag;

			static bool HybridCoolOneTimeFlag(true); // one time flag
			static Array1D_bool MyEnvrnFlag;
			static Array1D_bool MyFanFlag;
			static Array1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers
			int Loop;
			static bool ZoneEquipmentListChecked(false); // True after the Zone Equipment List has been checked for items
			Real64 TimeElapsed;

			int InletNode;
			int SecInletNode; // local index for secondary inlet node.
			Real64 RhoAir; // Air Density
			int ControlNode;
			int OutNode;
			int EvapUnitNum;

			if (HybridCoolOneTimeFlag) {
				MySizeFlag.dimension(NumZoneHybridEvap, true);
				MyEnvrnFlag.dimension(NumZoneHybridEvap, true);
				MyFanFlag.dimension(NumZoneHybridEvap, true);
				MyZoneEqFlag.allocate(NumZoneHybridEvap);
				MyZoneEqFlag = true;
				HybridCoolOneTimeFlag = false;
			//	ZoneHybridUnitaryAirConditioner(UnitNum).Hybrid_Model = new Model;
				
				
				//ZoneHybridUnitaryAirConditioner(UnitNum).Hybrid_Model->Initialize(ZoneHybridUnitaryAirConditioner(UnitNum).Path, pConfig);// X:\\LBNL_WCEC\\FMUDev\\HybridEvapModel\\HybridEvapCooling   Z:\Dropbox\LBNL_WCEC\FMUDev\HybridEvapModel\HybridEvapCooling\resources\HybridModelConfig
				// X:\\LBNL_WCEC\\FMUDev\\HybridEvapModel\\HybridEvapCooling   Z:\Dropbox\LBNL_WCEC\FMUDev\HybridEvapModel\HybridEvapCooling\resources\HybridModelConfig

			}
			if(!ZoneHybridUnitaryAirConditioner(UnitNum).Initialized)
			{
				ZoneHybridUnitaryAirConditioner(UnitNum).Initialize(ZoneHybridUnitaryAirConditioner(UnitNum).Path);//, pConfig);
			}
			
			ZoneHybridUnitaryAirConditioner(UnitNum).RequestedLoadToCoolingSetpoint = 0;
			ZoneHybridUnitaryAirConditioner(UnitNum).UnitTotalCoolingRate = 0.0;
			ZoneHybridUnitaryAirConditioner(UnitNum).UnitTotalCoolingEnergy = 0.0;

			ZoneHybridUnitaryAirConditioner(UnitNum).UnitSensibleCoolingRate = 0.0;
			ZoneHybridUnitaryAirConditioner(UnitNum).UnitSensibleCoolingEnergy = 0.0;
		
			// Do the following initializations (every time step): This should be the info from
			// the previous components outlets or the node data in this section.


			//*** But why?*** 


			//Transfer the node data to EvapCond data structure
			InletNode = ZoneHybridUnitaryAirConditioner(UnitNum).InletNode;

		//	RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, Node(InletNode).Temp, Node(InletNode).HumRat);

			// set the volume flow rates from the input mass flow rates
		//	ZoneHybridUnitaryAirConditioner(UnitNum).VolFlowRate = Node(InletNode).MassFlowRate / RhoAir;

			// Calculate the entering wet bulb temperature for inlet conditions
//			ZoneHybridUnitaryAirConditioner(UnitNum).InletWetBulbTemp = PsyTwbFnTdbWPb(Node(InletNode).Temp, Node(InletNode).HumRat, OutBaroPress);

			//Set all of the inlet mass flow variables from the nodes
			ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate = Node(InletNode).MassFlowRate;
//			ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRateMaxAvail = Node(InletNode).MassFlowRateMaxAvail;
//			ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRateMinAvail = Node(InletNode).MassFlowRateMinAvail;
			//Set all of the inlet state variables from the inlet nodes
			ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp = Node(InletNode).Temp;
			ZoneHybridUnitaryAirConditioner(UnitNum).InletHumRat = Node(InletNode).HumRat;
			ZoneHybridUnitaryAirConditioner(UnitNum).InletEnthalpy = Node(InletNode).Enthalpy;
			ZoneHybridUnitaryAirConditioner(UnitNum).InletPressure = Node(InletNode).Press;
			ZoneHybridUnitaryAirConditioner(UnitNum).InletRH = PsyRhFnTdbWPb(ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp, ZoneHybridUnitaryAirConditioner(UnitNum).InletHumRat, ZoneHybridUnitaryAirConditioner(UnitNum).OutletPressure, "InitZoneHybridUnitaryAirConditioners");

			//Set default outlet state to inlet states(?)
			ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp = ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp;
			ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat = ZoneHybridUnitaryAirConditioner(UnitNum).InletHumRat;
			ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy = ZoneHybridUnitaryAirConditioner(UnitNum).InletEnthalpy;
			ZoneHybridUnitaryAirConditioner(UnitNum).OutletPressure = ZoneHybridUnitaryAirConditioner(UnitNum).InletPressure;
			ZoneHybridUnitaryAirConditioner(UnitNum).OutletRH = PsyRhFnTdbWPb(ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp, ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat, ZoneHybridUnitaryAirConditioner(UnitNum).OutletPressure, "InitZoneHybridUnitaryAirConditioners");

			ZoneHybridUnitaryAirConditioner(UnitNum).OutletMassFlowRate = ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate;
//			ZoneHybridUnitaryAirConditioner(UnitNum).OutletMassFlowRateMaxAvail = ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRateMaxAvail;
//			ZoneHybridUnitaryAirConditioner(UnitNum).OutletMassFlowRateMinAvail = ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRateMinAvail;

			ZoneHybridUnitaryAirConditioner(UnitNum).SecInletTemp = Node(ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).Temp;
			ZoneHybridUnitaryAirConditioner(UnitNum).SecInletHumRat = Node(ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).HumRat;
			ZoneHybridUnitaryAirConditioner(UnitNum).SecInletEnthalpy = Node(ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).Enthalpy;
			ZoneHybridUnitaryAirConditioner(UnitNum).SecInletPressure = Node(ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).Press;
			double RHosa = Part_press(101.325, ZoneHybridUnitaryAirConditioner(UnitNum).SecInletHumRat) / Sat_press(ZoneHybridUnitaryAirConditioner(UnitNum).SecInletTemp);
			ZoneHybridUnitaryAirConditioner(UnitNum).SecInletRH = PsyRhFnTdbWPb(ZoneHybridUnitaryAirConditioner(UnitNum).SecInletTemp, ZoneHybridUnitaryAirConditioner(UnitNum).SecInletHumRat, ZoneHybridUnitaryAirConditioner(UnitNum).SecInletPressure, "InitZoneHybridUnitaryAirConditioners");

		}

		double Part_press(double P, double W)
		{
			// Function to compute partial vapor pressure in [kPa]
			// From page 6.9 equation 38 in ASHRAE Fundamentals handbook (2005)
			//   P = ambient pressure [kPa]
			//   W = humidity ratio [kg/kg dry air]

			return (P * W / (0.62198 + W));
		}

		double Sat_press(double Tdb)
		{
			// Function to compute saturation vapor pressure in [kPa]
			//ASHRAE Fundamentals handbood (2005) p 6.2, equation 5 and 6
			//   Tdb = Dry bulb temperature [degC]
			// Valid from -100C to 200 C

			double  C1 = -5674.5359;
			double  C2 = 6.3925247;
			double  C3 = -0.009677843;
			double  C4 = 0.00000062215701;
			double  C5 = 2.0747825E-09;
			double  C6 = -9.484024E-13;
			double  C7 = 4.1635019;
			double  C8 = -5800.2206;
			double  C9 = 1.3914993;
			double  C10 = -0.048640239;
			double  C11 = 0.000041764768;
			double  C12 = -0.000000014452093;
			double  C13 = 6.5459673;
			double  Sat_press_val = 0;

			double   TK = Tdb + 273.15;         //Converts from degC to degK

			if (TK <= 273.15)
			{
				Sat_press_val = exp(C1 / TK + C2 + C3 * TK + C4 * pow(TK, 2) + C5 * pow(TK, 3) + C6 * pow(TK, 4) + C7 * log(TK)) / 1000;
			}
			else
			{
				Sat_press_val = exp(C8 / TK + C9 + C10 * TK + C11 * pow(TK, 2) + C12 * pow(TK, 3) + C13 * log(TK)) / 1000;
			}
			return Sat_press_val;

		}

		void
			CalcZoneHybridUnitaryAirConditioners(
				int const UnitNum, // unit number
				int const ZoneNum, // number of zone being served
				Real64 & SensibleOutputProvided, // sensible capacity delivered to zone
				Real64 & LatentOutputProvided // Latent add/removal  (kg/s), dehumid = negative
				)
		{
			using DataZoneEnergyDemands::ZoneSysEnergyDemand;
			using DataHVACGlobals::TimeStepSys;
			using DataGlobals::SecInHour;
			using namespace DataLoopNode;
			using namespace Psychrometrics;
			Real64 ZoneCoolingLoad;
			Real64 AirMassFlow;
			Real64 MinHumRat, MinRH;
			Real64 QTotUnitOut;
			Real64 QSensUnitOut;
			Real64 EnvDryBulbT, AirTempRoom, EnvRelHumm, RoomRelHum, RemainQ, MsaCapacityRatedCond, DesignMinVR, rTestFlag, returnQSensible, returnQLatent, returnSupplyAirMassFlow, returnSupplyAirTemp, returnSupplyAirRelHum, returnVentilationAir, ElectricalPowerUse, communicationStepSize;
		
			int CapacityFlag, FMUmode,ErrorCode;
			bool error;
			error = false;
			ZoneCoolingLoad = ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;
			SensibleOutputProvided = ZoneCoolingLoad;
			LatentOutputProvided = 0;
			ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate = 1;
			//ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat = 0.5;
			ZoneHybridUnitaryAirConditioner(UnitNum).OutletMassFlowRate = 1;
			FMUmode = -2;
			AirMassFlow = ZoneHybridUnitaryAirConditioner(UnitNum).OutletMassFlowRate;
			MinRH = 0.5;
			double pressure_pascals = 101325;
			MinHumRat = ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat;// should do some sort of minium I gues but dont know why. min(Node(ZoneNodeNum).HumRat, Node(UnitOutletNodeNum).HumRat);
			Real64 InletEnthalpy = PsyHFnTdbW(ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp, MinHumRat);
			if (ZoneCoolingLoad>0)
			{//heating mode do nothing
				ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy = InletEnthalpy;
				ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp = ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp;
				QTotUnitOut = 0;
				QSensUnitOut = 0;
				FMUmode = -1;
			}
			else
			{
			//	Real64 OutletTempCalculated = PsyTdbFnHW(ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy, MinHumRat);
			//	if (OutletTempCalculated < 10)
			//	{
			//		ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy =  PsyHFnTdbW(10, MinHumRat); //PsyHFnTdbRhPb(
			//		ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp = 10;
			//	}
				
				EnvDryBulbT = ZoneHybridUnitaryAirConditioner(UnitNum).SecInletTemp;// 34.95;
				AirTempRoom = ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp;//23.94039067;
				EnvRelHumm = ZoneHybridUnitaryAirConditioner(UnitNum).SecInletRH;//RH 78; Check thats the right humidity metric
				RoomRelHum = ZoneHybridUnitaryAirConditioner(UnitNum).InletRH;//RH 38;
				RemainQ = ZoneCoolingLoad;
				MsaCapacityRatedCond = ZoneHybridUnitaryAirConditioner(UnitNum).MsaCapacityRatedCond; //m3/s
				CapacityFlag = 1; // boolean
				DesignMinVR = 0.5;   //m3/s
				rTestFlag = 0;
				communicationStepSize = 60 * 10; //s
				// slight issue here that the multplication of the max values of the "Fraction of peak Msa" and the OSAF as specified in the config must be greater than the ratio of MinVR/MsaCapacityRatedCond otherwise it will never reach minVR
				ZoneHybridUnitaryAirConditioner(UnitNum).RequestedLoadToCoolingSetpoint = RemainQ;
				ZoneHybridUnitaryAirConditioner(UnitNum).doStep(EnvDryBulbT, AirTempRoom, EnvRelHumm, RoomRelHum, RemainQ, MsaCapacityRatedCond, CapacityFlag, DesignMinVR, rTestFlag, &returnQSensible, &returnQLatent, &returnSupplyAirMassFlow, &returnSupplyAirTemp, &returnSupplyAirRelHum, &returnVentilationAir, &FMUmode, &ElectricalPowerUse, communicationStepSize, &ErrorCode);

				//ZoneHybridUnitaryAirConditioner(UnitNum).Hybrid_Model->doStep(EnvDryBulbT, AirTempRoom, EnvRelHumm, RoomRelHum, RemainQ, MsaCapacityRatedCond, CapacityFlag, DesignMinVR, rTestFlag, &returnQSensible, &returnQLatent, &returnSupplyAirMassFlow, &returnSupplyAirTemp, &returnSupplyAirRelHum, &returnVentilationAir, &FMUmode, &ElectricalPowerUse, communicationStepSize,&ErrorCode);
			/*	if (returnSupplyAirTemp < 10)
				{
					//ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy = PsyHFnTdbW(10, ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat); //PsyHFnTdbRhPb(
					//ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp = 10;
				}*/
				ZoneHybridUnitaryAirConditioner(UnitNum).ErrorCode = ErrorCode;
				if (ErrorCode > 0) error = true;
				if (error==true)
				{ 
					ZoneHybridUnitaryAirConditioner(UnitNum).OutletRH = ZoneHybridUnitaryAirConditioner(UnitNum).InletRH;
					ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat = ZoneHybridUnitaryAirConditioner(UnitNum).InletHumRat;
					ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy = ZoneHybridUnitaryAirConditioner(UnitNum).InletEnthalpy;
					ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp = ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp;
					ZoneHybridUnitaryAirConditioner(UnitNum).OutletMassFlowRate = ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate;
					ZoneHybridUnitaryAirConditioner(UnitNum).Mode = FMUmode;
					
					QTotUnitOut = 0;
					QSensUnitOut = 0;
				
				}
				else
				{ 
					ZoneHybridUnitaryAirConditioner(UnitNum).OutletRH = returnSupplyAirRelHum;
					ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat = PsyWFnTdbRhPb(returnSupplyAirTemp, returnSupplyAirRelHum, ZoneHybridUnitaryAirConditioner(UnitNum).InletPressure);
					ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy = PsyHFnTdbRhPb(returnSupplyAirTemp, returnSupplyAirRelHum, ZoneHybridUnitaryAirConditioner(UnitNum).InletPressure); // is the outlet presure going to be different? //InletEnthalpy - (ZoneCoolingLoad / AirMassFlow);
					ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp = returnSupplyAirTemp;//PsyTdbFnHW(ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy, MinHumRat);
					ZoneHybridUnitaryAirConditioner(UnitNum).OutletMassFlowRate = returnSupplyAirMassFlow;
					ZoneHybridUnitaryAirConditioner(UnitNum).Mode = FMUmode;
					if (ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy < InletEnthalpy)
					{
						QTotUnitOut = returnSupplyAirMassFlow * (ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy - InletEnthalpy);
						QSensUnitOut = returnSupplyAirMassFlow * (PsyHFnTdbW(ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp, MinHumRat) - PsyHFnTdbW(ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp, MinHumRat));
					}
					else 
					{
						QTotUnitOut =  0;
						QSensUnitOut = 0;
					}

				}

				// check power out using enthalpy
	
			}
				
			//ZoneHybridUnitaryAirConditioner(UnitNum).Mode = 1;
			ZoneHybridUnitaryAirConditioner(UnitNum).UnitTotalCoolingRate = std::abs(min(0.0, QTotUnitOut));
			ZoneHybridUnitaryAirConditioner(UnitNum).UnitTotalCoolingEnergy = ZoneHybridUnitaryAirConditioner(UnitNum).UnitTotalCoolingRate * TimeStepSys * SecInHour;
			ZoneHybridUnitaryAirConditioner(UnitNum).UnitSensibleCoolingRate = std::abs(min(0.0, QSensUnitOut));
			ZoneHybridUnitaryAirConditioner(UnitNum).UnitSensibleCoolingEnergy = ZoneHybridUnitaryAirConditioner(UnitNum).UnitSensibleCoolingRate * TimeStepSys * SecInHour;

		}

		void
			ReportZoneHybridUnitaryAirConditioners(int const UnitNum) // unit number
		{
			using namespace DataLoopNode;
			using namespace Psychrometrics;
			ZoneHybridUnitaryAirConditioner(UnitNum).Mode = ZoneHybridUnitaryAirConditioner(UnitNum).Mode;
			Node(ZoneHybridUnitaryAirConditioner(UnitNum).InletNode).MassFlowRate =  ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate;
			Node(ZoneHybridUnitaryAirConditioner(UnitNum).InletNode).MassFlowRate = ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate;
			Node(ZoneHybridUnitaryAirConditioner(UnitNum).OutletNode).Temp =         ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp;
			Node(ZoneHybridUnitaryAirConditioner(UnitNum).OutletNode).HumRat =       ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat;
			Node(ZoneHybridUnitaryAirConditioner(UnitNum).OutletNode).MassFlowRate = ZoneHybridUnitaryAirConditioner(UnitNum).OutletMassFlowRate;
			Node(ZoneHybridUnitaryAirConditioner(UnitNum).OutletNode).Enthalpy = ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy;//PsyHFnTdbW(ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp, ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat);
		

		}

		void
			GetInputZoneHybridUnitaryAirConditioners()
		{
			using BranchNodeConnections::TestCompSet;
			using namespace ScheduleManager;
			using DataGlobals::ScheduleAlwaysOn;
			using NodeInputManager::GetOnlySingleNode;
			using BranchNodeConnections::SetUpCompSets;
			using namespace DataIPShortCuts; // Data for field names, blank numerics
			using namespace DataLoopNode;
			using InputProcessor::GetNumObjectsFound;
			using InputProcessor::GetObjectDefMaxArgs;
			using InputProcessor::GetObjectItem;
			using InputProcessor::FindItemInList;
			using InputProcessor::VerifyName;

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
			bool errFlag;
			Real64 FanVolFlow;
			int UnitLoop;
			int CtrlZone; // index to loop counter
			int NodeNum; // index to loop counter

						 // SUBROUTINE PARAMETER DEFINITIONS:
			static std::string const RoutineName("GetInputZoneEvaporativeCoolerUnit: ");
			MaxNumbers = 0;
			MaxAlphas = 0;

			CurrentModuleObject = "ZoneHVAC:HybridUnitaryAC";
			NumZoneHybridEvap = GetNumObjectsFound(CurrentModuleObject);
			GetObjectDefMaxArgs(CurrentModuleObject, NumFields, NumAlphas, NumNumbers);
			MaxNumbers = max(MaxNumbers, NumNumbers);
			MaxAlphas = max(MaxAlphas, NumAlphas);
			Alphas.allocate(MaxAlphas);
			Numbers.dimension(MaxNumbers, 0.0);
			cAlphaFields.allocate(MaxAlphas);
			cNumericFields.allocate(MaxNumbers);
			lAlphaBlanks.dimension(MaxAlphas, true);
			lNumericBlanks.dimension(MaxNumbers, true); 


			if (NumZoneHybridEvap > 0) {
				CheckZoneHybridEvapName.dimension(NumZoneHybridEvap, true);
				ZoneHybridUnitaryAirConditioner.allocate(NumZoneHybridEvap);
				//ZoneEvapCoolerUnitFields.allocate(NumZoneEvapUnits);

				for (UnitLoop = 1; UnitLoop <= NumZoneHybridEvap; ++UnitLoop) {
					GetObjectItem(CurrentModuleObject, UnitLoop, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields);

					//ZoneEvapCoolerUnitFields(UnitLoop).FieldNames.allocate(NumNumbers);
					//ZoneEvapCoolerUnitFields(UnitLoop).FieldNames = "";
					//ZoneEvapCoolerUnitFields(UnitLoop).FieldNames = cNumericFields;

					IsNotOK = false;
					IsBlank = false;
					VerifyName(Alphas(1), ZoneHybridUnitaryAirConditioner, UnitLoop - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name");

					ZoneHybridUnitaryAirConditioner(UnitLoop).Name = Alphas(1);

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
					
					ZoneHybridUnitaryAirConditioner(UnitLoop).InletNode = GetOnlySingleNode(Alphas(3), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent);
					//ZoneEvapUnit(UnitLoop).UnitOutletNodeNum = GetOnlySingleNode(Alphas(5), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent);
					ZoneHybridUnitaryAirConditioner(UnitLoop).OutletNode = GetOnlySingleNode(Alphas(4), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsNotParent);
					//ZoneHybridUnitaryAirConditioner(UnitLoop).OAInletNodeNum = GetOnlySingleNode(Alphas(4), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_OutsideAir, 1, ObjectIsParent);
					ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryInletNode= GetOnlySingleNode(Alphas(5), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent);
					ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryOutletNode = GetOnlySingleNode(Alphas(6), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_ReliefAir, 1, ObjectIsNotParent);

					TestCompSet(CurrentModuleObject, Alphas(1), Alphas(3), Alphas(4), "Hybrid Evap Air Zone Nodes");
					TestCompSet(CurrentModuleObject, Alphas(1), Alphas(5), Alphas(6), "Hybrid Evap Air Zone Secondary Nodes");
					ZoneHybridUnitaryAirConditioner(UnitLoop).Path = Alphas(7);
						ZoneHybridUnitaryAirConditioner(UnitLoop).Tsa_Lookup_Name = Alphas(8);
					
				ZoneHybridUnitaryAirConditioner(UnitLoop).Tsa_schedule_pointer = GetScheduleIndex(Alphas(8));
					if (ZoneHybridUnitaryAirConditioner(UnitLoop).Tsa_schedule_pointer == 0) {
						ShowSevereError("Invalid " + cAlphaFields(8) + '=' + Alphas(8));
						ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
						ErrorsFound = true;
					}
					
					
					//A8, \Number of modes
					//A9, \Mode1_Tsa_Lookup_Name
					//A10, \Mode1_Hsa_Lookup_Name
					//A11, \Mode1_Power_Lookup_Name
					ZoneHybridUnitaryAirConditioner(UnitLoop).MsaCapacityRatedCond = Numbers(2);
					//A13, \Outdoor air min temperature
					//A14, \Outdoor air max temperature
					//A15, \ Outdoor air RH temperature
					//A16, \ Outdoor air RH temperature
					//A17, \ operating conditions min OSAF
					//A18, \ operating conditions max OSAF
					//A19, \ operating conditions min MsaRatio
					//A20;		\ operating conditions max MsaRatio

				}
			}
			// setup output variables
			for (UnitLoop = 1; UnitLoop <= NumZoneHybridEvap; ++UnitLoop) {

				SetUpCompSets(CurrentModuleObject, ZoneHybridUnitaryAirConditioner(UnitLoop).Name, CurrentModuleObject, ZoneHybridUnitaryAirConditioner(UnitLoop).Name, NodeID(ZoneHybridUnitaryAirConditioner(UnitLoop).InletNode), NodeID(ZoneHybridUnitaryAirConditioner(UnitLoop).OutletNode));
				SetUpCompSets(CurrentModuleObject, ZoneHybridUnitaryAirConditioner(UnitLoop).Name, CurrentModuleObject, ZoneHybridUnitaryAirConditioner(UnitLoop).Name, NodeID(ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryInletNode), NodeID(ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryOutletNode));

				SetupOutputVariable("Zone Hybrid Evaporative Cooler Total Cooling Rate [W]", ZoneHybridUnitaryAirConditioner(UnitLoop).UnitTotalCoolingRate, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Zone Evaporative Cooler Unit Total Cooling Energy [J]", ZoneHybridUnitaryAirConditioner(UnitLoop).UnitTotalCoolingEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System");
				SetupOutputVariable("CoolingLoad []", ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedLoadToCoolingSetpoint,"System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("Mode []", ZoneHybridUnitaryAirConditioner(UnitLoop).Mode, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("ErrorCode []", ZoneHybridUnitaryAirConditioner(UnitLoop).ErrorCode, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
				SetupOutputVariable("System Supply Air RH []", ZoneHybridUnitaryAirConditioner(UnitLoop).Wsa, "System", "Average", ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

				//int count_SAHR_OC_MetOnce;
				//int count_SAT_OC_MetOnce;
				//int count_DidWeMeetLoad;


				//SetupOutputVariable("Zone Evaporative Cooler Unit Total Cooling Energy [J]", ZoneHybridUnitaryAirConditioner(UnitLoop).UnitTotalCoolingEnergy, "System", "Sum", ZoneHybridUnitaryAirConditioner(UnitLoop).Name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System");


				/*	SetupOutputVariable("Zone Evaporative Cooler Unit Sensible Cooling Rate [W]", ZoneEvapUnit(UnitLoop).UnitSensibleCoolingRate, "System", "Average", ZoneEvapUnit(UnitLoop).Name);
				SetupOutputVariable("Zone Evaporative Cooler Unit Sensible Cooling Energy [J]", ZoneEvapUnit(UnitLoop).UnitSensibleCoolingEnergy, "System", "Sum", ZoneEvapUnit(UnitLoop).Name);
				SetupOutputVariable("Zone Evaporative Cooler Unit Latent Heating Rate [W]", ZoneEvapUnit(UnitLoop).UnitLatentHeatingRate, "System", "Average", ZoneEvapUnit(UnitLoop).Name);
				SetupOutputVariable("Zone Evaporative Cooler Unit Latent Heating Energy [J]", ZoneEvapUnit(UnitLoop).UnitLatentHeatingEnergy, "System", "Sum", ZoneEvapUnit(UnitLoop).Name);
				SetupOutputVariable("Zone Evaporative Cooler Unit Latent Cooling Rate [W]", ZoneEvapUnit(UnitLoop).UnitLatentCoolingRate, "System", "Average", ZoneEvapUnit(UnitLoop).Name);
				SetupOutputVariable("Zone Evaporative Cooler Unit Latent Cooling Energy [J]", ZoneEvapUnit(UnitLoop).UnitLatentCoolingEnergy, "System", "Sum", ZoneEvapUnit(UnitLoop).Name);
				SetupOutputVariable("Zone Evaporative Cooler Unit Fan Speed Ratio []", ZoneEvapUnit(UnitLoop).UnitFanSpeedRatio, "System", "Average", ZoneEvapUnit(UnitLoop).Name);
				SetupOutputVariable("Zone Evaporative Cooler Unit Fan Availability Status []", ZoneEvapUnit(UnitLoop).FanAvailStatus, "System", "Average", ZoneEvapUnit(UnitLoop).Name);*/
			}

		}

	}
}