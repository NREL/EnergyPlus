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

#ifndef HVACFan_hh_INCLUDED
#define HVACFan_hh_INCLUDED

// C++ Headers
#include <memory>
#include <string>
#include <vector>

#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace HVACFan {

    int getFanObjectVectorIndex(std::string const &objectName);

    bool checkIfFanNameIsAFanSystem(std::string const &objectName);

    class FanSystem
    {

    public: // Methods
        // Constructor
        FanSystem(std::string const &objectName);

        // Destructor
        ~FanSystem()
        {
        }

        // Copy Constructor
        FanSystem(FanSystem const &) = default;

        void simulate(
            //		bool const firstHVACIteration,
            Optional<Real64 const> flowFraction = _,     // Flow fraction in operating mode 1
            Optional_bool_const zoneCompTurnFansOn = _,  // Turn fans ON signal from ZoneHVAC component
            Optional_bool_const zoneCompTurnFansOff = _, // Turn Fans OFF signal from ZoneHVAC component
            Optional<Real64 const> pressureRise = _,     // Pressure difference to use for DeltaPress
            Optional<Real64 const> massFlowRate1 = _,    // Mass flow rate in operating mode 1 [kg/s]
            Optional<Real64 const> runTimeFraction1 = _, // Run time fraction in operating mode 1
            Optional<Real64 const> massFlowRate2 = _,    // Mass flow rate in operating mode 2 [kg/s]
            Optional<Real64 const> runTimeFraction2 = _, // Run time fraction in operating mode 2
            Optional<Real64 const> pressureRise2 = _     // Pressure difference to use for operating mode 2
        );

        Real64 fanPower() const;

        Real64 powerLossToAir() const;

        Real64 maxAirMassFlowRate() const;

        Real64 getFanDesignTemperatureRise() const;

        Real64 getFanDesignHeatGain(Real64 const FanVolFlow);

        // void
        // fanIsSecondaryDriver();

        // void
        // setFaultyFilterOn();

        // void
        // setFaultyFilterIndex( int const faultyAirFilterIndex );

        enum class SpeedControlMethod : int
        {
            NotSet = 0,
            Discrete,
            Continuous
        };

        // data
        std::string name;                       // user identifier
        int availSchedIndex;                    // Pointer to the availability schedule
        int inletNodeNum;                       // system air node at fan inlet
        int outletNodeNum;                      // system air node at fan outlet
        Real64 designAirVolFlowRate;            // Max Specified Volume Flow Rate of Fan [m3/sec]
        SpeedControlMethod speedControl;        // Discrete or Continuous speed control method
        Real64 deltaPress;                      // Delta Pressure Across the Fan [N/m2]
        Real64 designElecPower;                 // design electric power consumption [W]
        int powerModFuncFlowFractionCurveIndex; // pointer to performance curve or table

        // Mass Flow Rate Control Variables
        bool fanIsSecondaryDriver; // true if this fan is used to augment flow and may pass air when off.

        // FEI
        static Real64 report_fei(Real64 const designFlowRate, Real64 const designElecPower, Real64 const designDeltaPress, Real64 inletRhoAir);

    private: // methods
        void init();

        void set_size();

        void calcSimpleSystemFan(Optional<Real64 const> flowFraction, // Flow fraction for entire timestep (not used if flow ratios are present)
                                 Optional<Real64 const> pressureRise, // Pressure difference to use for DeltaPress
                                 Optional<Real64 const> flowRatio1,   // Flow ratio in operating mode 1
                                 Optional<Real64 const> runTimeFrac1, // Run time fraction in operating mode 1
                                 Optional<Real64 const> flowRatio2,   // Flow ratio in operating mode 2
                                 Optional<Real64 const> runTimeFrac2, // Run time fraction in operating mode 2
                                 Optional<Real64 const> pressureRise2 // Pressure difference to use for operating mode 2
        );

        void update() const;

        void report();

        // data

        enum class PowerSizingMethod : int
        {
            powerSizingMethodNotSet = 0,
            powerPerFlow,
            powerPerFlowPerPressure,
            totalEfficiencyAndPressure
        };
        enum class ThermalLossDestination : int
        {
            heatLossNotDetermined = 0,
            zoneGains,
            lostToOutside
        };

        std::string m_fanType;                   // Type of Fan ie. Simple, Vane axial, Centrifugal, etc.
        int m_fanType_Num;                       // DataHVACGlobals fan type
        bool m_designAirVolFlowRateWasAutosized; // true if design max volume flow rate was autosize on input
        Real64 m_minPowerFlowFrac;               // Minimum fan air flow fraction for power calculation
        Real64 m_motorEff;                       // Fan motor efficiency
        Real64 m_motorInAirFrac;                 // Fraction of motor heat entering air stream
        bool m_designElecPowerWasAutosized;
        PowerSizingMethod m_powerSizingMethod;          // sizing method for design electric power, three options
        Real64 m_elecPowerPerFlowRate;                  // scaling factor for powerPerFlow method
        Real64 m_elecPowerPerFlowRatePerPressure;       // scaling factor for powerPerFlowPerPressure
        Real64 m_fanTotalEff;                           // Fan total system efficiency (fan*belt*motor*VFD)
        Real64 m_nightVentPressureDelta;                // fan pressure rise during night ventilation mode
        Real64 m_nightVentFlowFraction;                 // fan's flow fraction during night ventilation mode, not used
        int m_zoneNum;                                  // zone index for motor heat losses as internal gains
        Real64 m_zoneRadFract;                          // thermal radiation split for motor losses
        ThermalLossDestination m_heatLossesDestination; // enum for where motor loss go
        Real64 m_qdotConvZone;                          // fan power lost to surrounding zone by convection to air (W)
        Real64 m_qdotRadZone;                           // fan power lost to surrounding zone by radiation to zone surfaces(W)
        std::string m_endUseSubcategoryName;
        int m_numSpeeds;                            // input for how many speed levels for discrete fan
        std::vector<Real64> m_flowFractionAtSpeed;  // array of flow fractions for speed levels
        std::vector<Real64> m_powerFractionAtSpeed; // array of power fractions for speed levels
        std::vector<bool> m_powerFractionInputAtSpeed;
        // calculation variables
        std::vector<Real64> m_massFlowAtSpeed;
        std::vector<Real64> m_totEfficAtSpeed;
        Real64 m_inletAirMassFlowRate; // MassFlow through the Fan being Simulated [kg/Sec]
        Real64 m_outletAirMassFlowRate;
        //	Real64 m_minAirFlowRate; // Min Specified Volume Flow Rate of Fan [m3/sec]
        Real64 m_maxAirMassFlowRate; // Max flow rate of fan in kg/sec
                                     //	Real64 m_minAirMassFlowRate; // Min flow rate of fan in kg/sec
                                     //	int fanMinAirFracMethod; // parameter for what method is used for min flow fraction
                                     //	Real64 fanFixedMin; // Absolute minimum fan air flow [m3/s]
        Real64 m_inletAirTemp;
        Real64 m_outletAirTemp;
        Real64 m_inletAirHumRat;
        Real64 m_outletAirHumRat;
        Real64 m_inletAirEnthalpy;
        Real64 m_outletAirEnthalpy;
        bool m_objTurnFansOn;
        bool m_objTurnFansOff;
        bool m_objEnvrnFlag;  // initialize to true
        bool m_objSizingFlag; // initialize to true, set to false after sizing routine

        // report variables
        Real64 m_fanPower;       // Power of the Fan being Simulated [W]
        Real64 m_fanEnergy;      // Fan energy in [J]
                                 //	Real64 fanRuntimeFraction; // Fraction of the timestep that the fan operates
        Real64 m_deltaTemp;      // Temp Rise across the Fan [C]
        Real64 m_powerLossToAir; // fan heat gain into process air [W]
        std::vector<Real64> m_fanRunTimeFractionAtSpeed;
        // EMS related variables
        bool m_maxAirFlowRateEMSOverrideOn;      // if true, EMS wants to override fan size for Max Volume Flow Rate
        Real64 m_maxAirFlowRateEMSOverrideValue; // EMS value to use for override of  Max Volume Flow Rate
        bool m_eMSFanPressureOverrideOn;         // if true, then EMS is calling to override
        Real64 m_eMSFanPressureValue;            // EMS value for Delta Pressure Across the Fan [Pa]
        bool m_eMSFanEffOverrideOn;              // if true, then EMS is calling to override
        Real64 m_eMSFanEffValue;                 // EMS value for total efficiency of the Fan, fraction on 0..1
        bool m_eMSMaxMassFlowOverrideOn;         // if true, then EMS is calling to override mass flow
        Real64 m_eMSAirMassFlowValue;            // value EMS is directing to use [kg/s]

        bool m_faultyFilterFlag; // Indicate whether there is a fouling air filter corresponding to the fan
        int m_faultyFilterIndex; // Index of the fouling air filter corresponding to the fan
        // Mass Flow Rate Control Variables
        Real64 m_massFlowRateMaxAvail;
        Real64 m_massFlowRateMinAvail;
        Real64 m_rhoAirStdInit;
        //	bool oneTimePowerCurveCheck_; // one time flag used for error message
        Real64 m_designPointFEI; // Fan Energy Index for the fan at the design operating point

    }; // class FanSystem

    extern std::vector<std::unique_ptr<FanSystem>> fanObjs;

    void clearHVACFanObjects();

} // namespace HVACFan

} // namespace EnergyPlus
#endif // HVACFan_hh_INCLUDED_hh_INCLUDED
