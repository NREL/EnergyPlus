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

#ifndef HVACFan_hh_INCLUDED
#define HVACFan_hh_INCLUDED

// C++ Headers
#include <string>
#include <vector>
#include <memory>

#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>

namespace EnergyPlus {

namespace HVACFan {

	int
	getFanObjectVectorIndex( std::string const objectName );

	bool
	checkIfFanNameIsAFanSystem( std::string const objectName );

class FanSystem
{

public: // Methods

	// Constructor
	FanSystem( std::string const objectName );

	//Destructor
	~FanSystem()
	{}

	// Copy Constructor
	FanSystem( FanSystem const & ) = default;

	void
	simulate(
//		bool const firstHVACIteration,
		Optional< Real64 const > flowFraction,
		Optional_bool_const zoneCompTurnFansOn, // Turn fans ON signal from ZoneHVAC component
		Optional_bool_const zoneCompTurnFansOff, // Turn Fans OFF signal from ZoneHVAC component
		Optional< Real64 const > pressureRise // Pressure difference to use for DeltaPress
	);

	std::string const &
	name() const;

	Real64
	fanPower() const;

	Real64
	designAirVolFlowRate() const;

	int
	inletNodeNum() const;

	int
	outletNodeNum() const;

	int
	availSchedIndex() const;

	int
	getFanPowerCurveIndex() const;

	Real64
	getFanDesignTemperatureRise() const;

	Real64 
	getFanDesignHeatGain( Real64 const FanVolFlow );

	bool
	getIfContinuousSpeedControl() const;

private: //methods

	void
	init();

	void
	set_size();

	void
	calcSimpleSystemFan(
		Optional< Real64 const > flowFraction,
		Optional< Real64 const > PressureRise
	);

	void
	update() const; 

	void
	report();

private: // data

	enum class SpeedControlMethod : int {
		notSet = 0,
		discrete,
		continuous
	};
	enum class PowerSizingMethod : int {
		powerSizingMethodNotSet = 0,
		powerPerFlow,
		powerPerFlowPerPressure,
		totalEfficiencyAndPressure
	};
	enum class ThermalLossDestination : int {
		heatLossNotDetermined = 0,
		zoneGains,
		lostToOutside
	};

	std::string name_; // user identifier
	std::string fanType_; // Type of Fan ie. Simple, Vane axial, Centrifugal, etc.
	int fanType_Num_; // DataHVACGlobals fan type
	int availSchedIndex_; // Pointer to the availability schedule
	int inletNodeNum_; // system air node at fan inlet
	int outletNodeNum_; // system air node at fan outlet
	Real64 designAirVolFlowRate_; // Max Specified Volume Flow Rate of Fan [m3/sec]
	bool designAirVolFlowRateWasAutosized_; // true if design max volume flow rate was autosize on input
	SpeedControlMethod speedControl_; // Discrete or Continuous speed control method
	Real64 minPowerFlowFrac_; // Minimum fan air flow fraction for power calculation
	Real64 deltaPress_; // Delta Pressure Across the Fan [N/m2]
	Real64 motorEff_; // Fan motor efficiency
	Real64 motorInAirFrac_; // Fraction of motor heat entering air stream
	Real64 designElecPower_; // design electric power consumption [W]
	bool designElecPowerWasAutosized_;
	PowerSizingMethod powerSizingMethod_; // sizing method for design electric power, three options
	Real64 elecPowerPerFlowRate_; // scaling factor for powerPerFlow method
	Real64 elecPowerPerFlowRatePerPressure_; // scaling factor for powerPerFlowPerPressure
	Real64 fanTotalEff_; // Fan total system efficiency (fan*belt*motor*VFD)
	int powerModFuncFlowFractionCurveIndex_; // pointer to performance curve or table
	Real64 nightVentPressureDelta_; // fan pressure rise during night ventilation mode
	Real64 nightVentFlowFraction_; // fan flow fraction during night ventilation mode
	int zoneNum_; // zone index for motor heat losses as internal gains
	Real64 zoneRadFract_; // thermal radiation split for motor losses
	ThermalLossDestination heatLossesDestination_; //enum for where motor loss go
	std::string endUseSubcategoryName_;
	int numSpeeds_; // input for how many speed levels for discrete fan
	std::vector< Real64 > flowFractionAtSpeed_; //array of flow fractions for speed levels
	std::vector< Real64 > powerFractionAtSpeed_; // array of power fractions for speed levels
	std::vector< bool > powerFractionInputAtSpeed_;
	//calculation variables
	std::vector< Real64 > massFlowAtSpeed_;
	std::vector< Real64 > totEfficAtSpeed_;
	Real64 inletAirMassFlowRate_; // MassFlow through the Fan being Simulated [kg/Sec]
	Real64 outletAirMassFlowRate_;
	Real64 minAirFlowRate_; // Min Specified Volume Flow Rate of Fan [m3/sec]
	Real64 maxAirMassFlowRate_; // Max flow rate of fan in kg/sec
	Real64 minAirMassFlowRate_; // Min flow rate of fan in kg/sec
//	int fanMinAirFracMethod; // parameter for what method is used for min flow fraction
//	Real64 fanFixedMin; // Absolute minimum fan air flow [m3/s]
	Real64 inletAirTemp_;
	Real64 outletAirTemp_;
	Real64 inletAirHumRat_;
	Real64 outletAirHumRat_;
	Real64 inletAirEnthalpy_;
	Real64 outletAirEnthalpy_;
	bool objTurnFansOn_;
	bool objTurnFansOff_;
	bool objEnvrnFlag_; // initialize to true
	bool objSizingFlag_; //initialize to true, set to false after sizing routine

	//report variables
	Real64 fanPower_; // Power of the Fan being Simulated [kW]
	Real64 fanEnergy_; // Fan energy in [kJ]
//	Real64 fanRuntimeFraction; // Fraction of the timestep that the fan operates
	Real64 deltaTemp_; // Temp Rise across the Fan [C]
	std::vector< Real64 > fanRunTimeFractionAtSpeed_;
	//EMS related variables
	bool maxAirFlowRateEMSOverrideOn_; // if true, EMS wants to override fan size for Max Volume Flow Rate
	Real64 maxAirFlowRateEMSOverrideValue_; // EMS value to use for override of  Max Volume Flow Rate
	bool eMSFanPressureOverrideOn_; // if true, then EMS is calling to override
	Real64 eMSFanPressureValue_; // EMS value for Delta Pressure Across the Fan [Pa]
	bool eMSFanEffOverrideOn_; // if true, then EMS is calling to override
	Real64 eMSFanEffValue_; // EMS value for total efficiency of the Fan, fraction on 0..1
	bool eMSMaxMassFlowOverrideOn_; // if true, then EMS is calling to override mass flow
	Real64 eMSAirMassFlowValue_; // value EMS is directing to use [kg/s]

	bool faultyFilterFlag_; // Indicate whether there is a fouling air filter corresponding to the fan
	int faultyFilterIndex_;  // Index of the fouling air filter corresponding to the fan
	// Mass Flow Rate Control Variables
	Real64 massFlowRateMaxAvail_;
	Real64 massFlowRateMinAvail_;
	Real64 rhoAirStdInit_;
	bool oneTimePowerCurveCheck_; // one time flag used for error message

}; //class FanSystem 

extern std::vector< std::unique_ptr< FanSystem > > fanObjs;

} // Fan namespace

} // EnergyPlus namespace
#endif //HVACFan_hh_INCLUDED_hh_INCLUDED
