// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
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
#ifndef HybridEvapCoolingModel_hh_INCLUDED
#define HybridEvapCoolingModel_hh_INCLUDED
#include <iostream>  

using namespace std;
#include <string>
#include <list>
#include <map>
#include <vector>


// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

using namespace std;

const int MODE_BLOCK_OFFSET_Alpha = 9;
const int BLOCK_HEADER_OFFSET_Alpha =  19;

const int MODE1_BLOCK_OFFSET_Number = 2;
const int MODE_BLOCK_OFFSET_Number = 16;
const int BLOCK_HEADER_OFFSET_Number = 6;
#define MINIMUM_LOAD_TO_ACTIVATE 0.5 // (kw)_ should think about if this needs to be user defined!!!!!
#define IMPLAUSIBLE_POWER 10000000
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

	namespace HybridEvapCoolingModel {

		enum class SYSTEMOUTPUTS { VENTILATION_AIR_V, SUPPLY_MASS_FLOW, SUPPLY_AIR_HUMIDITY, SYSTEM_FUEL_USE, SUPPLY_AIR_TEMP, MIXED_AIR_TEMP, SUPPLY_AIR_RH, SUPPLY_AIR_HR, MIXED_AIR_HR};
		
		class CModeSolutionSpace
		{
		public:
			vector<Real64> PointX;
			vector<Real64> PointY;
			vector<Real64> PointMeta;
			void AddItem(Real64 X, Real64 Y, Real64 M)
			{
				PointX.push_back(X);
				PointY.push_back(Y);
				PointMeta.push_back(M);
			}
		};	 

		class CMode
		{
			public:
			CMode() : ModeID(0.0), Max_Msa(0.0), Min_Msa(0.0),Min_OAF(0.0), Max_OAF(0.0), Minimum_Outside_Air_Temperature(0.0), Maximum_Outside_Air_Temperature(0.0)
			,Minimum_Outside_Air_Humidity_Ratio(0.0), Maximum_Outside_Air_Humidity_Ratio(0.0), NormalizationReference(0.0), Correction (0.0) {}
			
			//finish init above
			~CMode();                  // destructor
			int ModeID;
			CModeSolutionSpace sol;
			string ModeName;
			int Tsa_curve_pointer;
			int HRsa_curve_pointer;
			int Psa_curve_pointer;
			Real64 Max_Msa;
			Real64 Min_Msa;
			Real64 Min_OAF;
			Real64 Max_OAF;
			Real64 Minimum_Outside_Air_Temperature;
			Real64 Maximum_Outside_Air_Temperature;
			Real64 Minimum_Outside_Air_Humidity_Ratio;
			Real64 Maximum_Outside_Air_Humidity_Ratio;
			Real64 Minimum_Outside_Air_Relative_Humidity;
			Real64 Maximum_Outside_Air_Relative_Humidity;
			Real64 Minimum_Return_Air_Temperature;
			Real64 Maximum_Return_Air_Temperature;
			Real64 Minimum_Return_Air_Humidity_Ratio;
			Real64 Maximum_Return_Air_Humidity_Ratio;
			Real64 Minimum_Return_Air_Relative_Humidity;
			Real64 Maximum_Return_Air_Relative_Humidity;
			Real64 NormalizationReference;
			Real64 Correction;
			bool CMode::ValidPointer(int curve_pointer);
			bool CMode::ValidateArrays(Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields, std::string cCurrentModuleObject);
			bool CMode::ParseMode(Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields, Array1D<bool>  lAlphaBlanks, std::string cCurrentModuleObject);
			bool CMode::ParseMode0(Array1D_string Alphas, Array1D_string cAlphaFields, Array1D< Real64 > Numbers, Array1D_string cNumericFields, Array1D<bool>  lAlphaBlanks, std::string cCurrentModuleObject);
		
			void CMode::InitializeCurve( int curveType, int CurveID);
			Real64 CMode::CalculateCurveVal(Real64 X_0, Real64 X_1, Real64 X_2, Real64 X_3, Real64 X_4, Real64 X_5, Real64 X_6, int mode_number, int curve_ID);
			bool CMode::InitializeOSAFConstraints(Real64 minOSAF, Real64 maxOSAF);
			bool CMode::InitializeMsaRatioConstraints(Real64 minMsa, Real64 maxMsa);
			bool CMode::InitializeOutsideAirTemperatureConstraints(Real64 min, Real64 max);
			bool CMode::InitializeOutsideAirHumidityRatioConstraints(Real64 min, Real64 max);
			bool CMode::InitializeOutsideAirRelativeHumidityConstraints(Real64 min, Real64 max);
			bool CMode::InitializeReturnAirTemperatureConstraints(Real64 min, Real64 max);
			bool CMode::InitializeReturnAirHumidityRatioConstraints(Real64 min, Real64 max);
			bool CMode::InitializeReturnAirRelativeHumidityConstraints(Real64 min, Real64 max); 
			bool CMode::GenerateSolutionSpace(Real64 ResolutionMsa, Real64 ResolutionOSA);
			bool CMode::MeetsOAEnvConstraints(Real64 Tosa, Real64 Wosa, Real64 RHos);
		private:
		
		};
		
		class CSetting
		{
		public:
			CSetting() :Runtime_Fraction(0), Mode(0), Outside_Air_Fraction(0), Supply_Air_Mass_Flow_Rate(0), Supply_Air_Ventilation_Volume(0), Supply_Air_Mass_Flow_Rate_Ratio(0),
				SupplyAirTemperature(0), Mixed_Air_Temperature(0), SupplyAirW(0), Mixed_Air_W(0), TotalSystem(0), SensibleSystem(0), LatentSystem(0), TotalZone(0), SensibleZone(0), LatentZone(0), ElectricalPower(IMPLAUSIBLE_POWER), pMode(NULL){}
			Real64 Runtime_Fraction;
			Real64 Mode;
			Real64 Outside_Air_Fraction;
			Real64 Supply_Air_Mass_Flow_Rate;
			Real64 Supply_Air_Ventilation_Volume;
			Real64 Supply_Air_Mass_Flow_Rate_Ratio;
			Real64 SupplyAirTemperature;
			Real64 Mixed_Air_Temperature;
			Real64 SupplyAirW;
			Real64 Mixed_Air_W;
			Real64 TotalSystem;
			Real64 SensibleSystem;
			Real64 LatentSystem;
			Real64 TotalZone;
			Real64 SensibleZone;
			Real64 LatentZone;
			Real64 ElectricalPower;
			// add other fuels, or change name to be fuel
			CMode* pMode;
			CSetting& operator=(CSetting other)
			{
				using std::swap;
				swap(Runtime_Fraction, other.Runtime_Fraction);
				swap(Mode, other.Mode);
				swap(Outside_Air_Fraction, other.Outside_Air_Fraction);
				swap(Supply_Air_Mass_Flow_Rate, other.Supply_Air_Mass_Flow_Rate);
				swap(Supply_Air_Ventilation_Volume, other.Supply_Air_Ventilation_Volume);
				swap(Supply_Air_Mass_Flow_Rate_Ratio, other.Supply_Air_Mass_Flow_Rate_Ratio);
				swap(SupplyAirTemperature, other.SupplyAirTemperature);
				swap(Mixed_Air_Temperature, other.Mixed_Air_Temperature);
				swap(SupplyAirW, other.SupplyAirW);
				swap(Mixed_Air_W, other.Mixed_Air_W);
				swap(TotalSystem, other.TotalSystem);
				swap(SensibleSystem, other.SensibleSystem);
				swap(LatentSystem, other.LatentSystem);
				swap(TotalZone, other.TotalZone);
				swap(SensibleZone, other.SensibleZone);
				swap(LatentZone, other.LatentZone);
				swap(ElectricalPower, other.ElectricalPower);
				swap(pMode, other.pMode);
				
				// repeat for other member variables;
				return *this;
			}
		};

		class CStepInputs
		{
			public:
			CStepInputs() : Tosa(0), Tra(0), RHosa(0), RHra(0), RequestedCoolingLoad(0), RequestedHeatingLoad(0), ZoneMoistureLoad(0), ZoneDehumidificationLoad(0), MinimumOA(0) {}
			Real64 Tosa; Real64 Tra; Real64 RHosa; Real64 RHra; Real64 RequestedCoolingLoad; Real64 RequestedHeatingLoad; Real64 ZoneMoistureLoad; Real64 ZoneDehumidificationLoad; Real64 MinimumOA ;
		};

		class Model                   // begin declaration of the class
		{
		public:                    // begin public section
			Model();
			~Model();                  // destructor
	
			// Default Constructor
			std::string Name; // user identifier
			bool Initialized; // initialization flag ensures the system object is initialized only once.
			int ZoneNum; //stores the current zone associated with the system, this is currently not used but is expected to be used in the next set of functionality additions. 
			std::string Schedule; // Availability Schedule Name
			int SchedPtr; // Pointer to the correct schedule
	
			Real64 SystemMaximumSupplyAirFlowRate; //taken from IDF N1, the system max supply flow rate in m3/s.
			Real64 ScalingFactor;  //taken from IDF N3, linear scaling factor.
			Real64 ScaledSystemMaximumSupplyAirMassFlowRate; // the actual scaling factor used to multiply the 
			
			int UnitOn; //feels like it should be a bool but its an output and I couldn't get it to work as a bool 
			Real64 UnitTotalCoolingRate;       // unit output to zone, total cooling rate [W]
			Real64 UnitTotalCoolingEnergy;	   // unit output to zone, total cooling energy [J]
			Real64 UnitSensibleCoolingRate;    // unit sensible cooling rate [W]
			Real64 UnitSensibleCoolingEnergy;  // unit sensible cooling energy [J]
			Real64 UnitLatentCoolingRate;      // unit latent cooling rate [W]
			Real64 UnitLatentCoolingEnergy;    // unit latent cooling energy [J]
			Real64 SystemTotalCoolingRate;	   // system output to zone, total cooling rate [W]
			Real64 SystemTotalCoolingEnergy;   // system output to zone, total cooling energy [J]
			Real64 SystemSensibleCoolingRate;  // system sensible cooling rate [W]
			Real64 SystemSensibleCoolingEnergy;// system sensible cooling energy [J]
			Real64 SystemLatentCoolingRate;	   // system latent cooling rate [W]
			Real64 SystemLatentCoolingEnergy;  // system latent cooling energy [J]
			Real64 UnitTotalHeatingRate;       // unit output to zone, total heating rate [W]
			Real64 UnitTotalHeatingEnergy;     // unit output to zone, total heating energy [J]
			Real64 UnitSensibleHeatingRate;	   // unit sensible heating rate [W]
			Real64 UnitSensibleHeatingEnergy;  // unit sensible heating energy [J]
			Real64 UnitLatentHeatingRate;	   // unit latent heating rate [W]
			Real64 UnitLatentHeatingEnergy;	   // unit latent heating energy [J]
			Real64 SystemTotalHeatingRate;     // system output to zone, total heating rate [W]
			Real64 SystemTotalHeatingEnergy;   // system output to zone, total heating energy [J] 
			Real64 SystemSensibleHeatingRate;  // system sensible heating rate [W]
			Real64 SystemSensibleHeatingEnergy;// system sensible heating energy [J]
			Real64 SystemLatentHeatingRate;	   // system latent heating rate [W]
			Real64 SystemLatentHeatingEnergy;  // system latent heating energy [J]
			Real64 RequestedLoadToHeatingSetpoint;
			Real64 RequestedLoadToCoolingSetpoint;
			Real64  RequestedHumdificationMass;
			Real64  RequestedHumdificationLoad;
			Real64  RequestedHumdificationEnergy;
			Real64  RequestedDeHumdificationMass;
			Real64  RequestedDeHumdificationLoad;
			Real64  RequestedDeHumdificationEnergy;
			int TsaMin_schedule_pointer;
			int TsaMax_schedule_pointer;
			int RHsaMin_schedule_pointer;
			int RHsaMax_schedule_pointer;
			int PrimaryMode;
			Real64 PrimaryModeRuntimeFraction;
			Real64 averageOSAF;
			int CurrentPrimaryMode();
			Real64 CurrentPrimaryRuntimeFraction();
			Real64 Model::CalculatePartRuntimeFraction(Real64 MinOA_Msa, Real64 Mvent, Real64 RequestedCoolingLoad, Real64 RequestedHeatingLoad, Real64 SensibleRoomORZone, Real64 RequestedDehumidificationLoad, Real64 RequestedMoistureLoad, Real64 LatentRoomORZone);
			int ErrorCode;
			int InletNode;
			int OutletNode;
			int SecondaryInletNode; // This is usually OA node feeding into the purge/secondary side
			int SecondaryOutletNode; // This outlet node of the secondary side and ilet to the secondary fan
			vector<int> Tsa_curve_pointer;
			vector<int>  HRsa_curve_pointer;
			vector<int>  Psa_curve_pointer;
			list<CMode*> OperatingModes; 
			list<CSetting*> CurrentOperatingSettings;
			
			CSetting *pOptimal;
			CSetting *pStandBy;

			list<CSetting*> Settings;
			Real64 FinalElectricalPower; // Output fuel use in W
			Real64 FinalElectricalEnergy; // Output fuel energy use in J
			Real64 InletMassFlowRate; // Inlet is primary process air node at inlet to cooler
			Real64 InletTemp;
			Real64 InletWetBulbTemp;
			Real64 InletHumRat;
			Real64 InletEnthalpy;
			Real64 InletPressure;
			Real64 InletRH;
			Real64 OutletVolumetricFlowRate;
			Real64 OutletMassFlowRate; // Inlet is primary process air node at inlet to cooler
			Real64 OutletTemp;
			Real64 OutletWetBulbTemp;
			Real64 OutletHumRat;
			Real64 OutletEnthalpy;
			Real64 OutletPressure;
			Real64 OutletRH;
			Real64 SecInletMassFlowRate; // Inlet is primary process air node at inlet to cooler
			Real64 SecInletTemp;
			Real64 SecInletWetBulbTemp;
			Real64 SecInletHumRat;
			Real64 SecInletEnthalpy;
			Real64 SecInletPressure;
			Real64 SecInletRH;
			Real64 SecOutletMassFlowRate; // Inlet is primary process air node at inlet to cooler
			Real64 SecOutletTemp;
			Real64 SecOutletWetBulbTemp;
			Real64 SecOutletHumRat;
			Real64 SecOutletEnthalpy;
			Real64 SecOutletPressure;
			Real64 SecOutletRH;
			
			int OARequirementsPtr; // Index to DesignSpecification:OutdoorAir object
			bool OutdoorAir;
			Real64 MinOA_Msa;

			int Model::GetID();            // accessor function
			void Model::SetID(int vID) { ID = vID; };    // accessor function
			void Model::doStep(Real64 Tosa, Real64 Tra, Real64 RHosa, Real64 RHra, Real64 RequestedLoad, Real64 ZoneHeatingLoad, Real64 OutputRequiredToHumidify, Real64 OutputRequiredToDehumidify, Real64 DesignMinVR);
			void Model::Initialize(int ZoneNumber);
			CMode* Model::AddNewOperatingMode(Real64 correction);
			void Model::InitializeModelParams();
			void Model::ResetOutputs();
			void Model::ModelLog(std::string fmuLocation);
			Real64 Model::CalcHum_ratio_W(Real64 Tdb, Real64 RH, Real64 P);
			bool Model::MeetsSupplyAirTOC(Real64 Tosa);
			bool Model::MeetsSupplyAirRHOC(Real64 Wosa);
			Real64 Model::CheckVal_T(Real64 T);
			Real64 Model::CheckVal_W(Real64 W);
			bool Model::SetStandByMode(CMode* pMode0, CSetting *pStandBy, Real64 Tosa, Real64 Wosa, Real64 Tra, Real64 Wra );
			Real64 Model::CalculateTimeStepAverage(SYSTEMOUTPUTS val);
			int Model::SetOperatingSetting(CStepInputs StepIns);
			Real64 Model::Sat_press(Real64 Tdb);
			Real64 Model::Part_press(Real64 P, Real64 W);
			
			Real64 Tsa;
			Real64 Wsa;
			Real64 SupplyVentilationAir;
			Real64 SupplyVentilationVolume; 
			int ModeCounter;
			bool CoolingRequested ;
			bool HeatingRequested ;
			bool VentilationRequested;
			bool DehumidificationRequested;
			bool HumidificationRequested;
		
		private:                   // begin private section
			int ID;              // member variable
			char * string;
			//number of times in a day it failed to 
			vector<int> SAT_OC_MetinMode_v;
			vector<int> SAHR_OC_MetinMode_v;
			bool WarnOnceFlag;
			//holds the X and Y points of the possible sollutions within the operating conditions. Int is the mode number
			list<CModeSolutionSpace*> SolutionSpaces;
			
			Real64 ResolutionMsa;
			Real64 ResolutionOSA;
			int count_EnvironmentConditionsNotMet;
			int count_EnvironmentConditionsMetOnce;
			int count_SAHR_OC_MetOnce;
			int count_SAT_OC_MetOnce;
			int count_DidWeMeetLoad;
			int count_DidWeNotMeetLoad;

			Real64 MsaRated;
			Real64 RatedH;
		
			Real64 cp;
			Real64 Lambna;
			bool optimal_EnvCondMet;
			bool RunningPeakCapacity_EnvCondMet;
		
			Real64 Minimum_Supply_Air_Temp;
			Real64 RunningPeakCapacity_Wsa;
			Real64 RunningPeakCapacity_Tsa;
			Real64 RunningPeakCapacity_Point;
			vector<Real64> PolygonXs;
			vector<Real64> PolygonYs;

			int NumberOfModes;
		};

	
	}
}

#endif