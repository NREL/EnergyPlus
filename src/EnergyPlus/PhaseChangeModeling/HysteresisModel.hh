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

#ifndef HYSTERESISPHASECHANGE_HH_INCLUDED
#define HYSTERESISPHASECHANGE_HH_INCLUDED

#include <vector>

#include <EnergyPlus.hh>

namespace EnergyPlus {

	namespace HysteresisPhaseChange {

		struct PhaseChangeStates {
			// keeping these as ints to allow output variable reporting; could refine later into enum class
			static const int LIQUID = -2;
			static const int MELTING = -1;
			static const int TRANSITION = 0;
			static const int FREEZING = 1;
			static const int CRYSTALLIZED = 2;
		};

		extern int numHysteresisModels;

		class HysteresisPhaseChange {

			Real64 getEnthalpy( Real64 T, Real64 Tc, Real64 tau1, Real64 tau2 );

			Real64 specHeat( Real64 temperaturePrev, Real64 temperatureCurrent, Real64 criticalTemperature, Real64 tau1,
							 Real64 tau2, Real64 EnthalpyOld, Real64 EnthalpyNew );

		public:

			// members are pretty much all accessed outside of the class in one way or another (by the static factory, etc.)
			std::string name;
			Real64 enthalpyM;
			Real64 enthalpyF;

			// input parameters
			Real64 totalLatentHeat;
			Real64 specificHeatLiquid;
			Real64 deltaTempMeltingHigh;
			Real64 peakTempMelting;
			Real64 deltaTempMeltingLow;
			Real64 specificHeatSolid;
			Real64 deltaTempFreezingHigh;
			Real64 peakTempFreezing;
			Real64 deltaTempFreezingLow;

			// additional thermal propreties
			Real64 fullySolidThermalConductivity;
			Real64 fullyLiquidThermalConductivity;
			Real64 fullySolidDensity;
			Real64 fullyLiquidDensity;

			// history and state terms
			bool phaseChangeTransition;
			Real64 enthOld;
			Real64 enthNew;
			Real64 enthRev;
			Real64 CpOld;
			Real64 specHeatTransition;

			// the factory for this class
			static HysteresisPhaseChange *factory( const std::string &objectName );

			// the Cp calculation function for this class
			Real64 getCurrentSpecificHeat( Real64 prevTempTD, Real64 updatedTempTDT, Real64 phaseChangeTempReverse,
										   int prevPhaseChangeState, int &phaseChangeState );

			// the conductivity calculation function for this class
			Real64 getConductivity( Real64 T );

			// the density calculation function for this class
			Real64 getDensity( Real64 T );

			// and the destructor
			virtual
			~HysteresisPhaseChange() {}

		};

		extern std::vector<HysteresisPhaseChange> hysteresisPhaseChangeModels;

		void readAllHysteresisModels();

		void clear_state();

	} // namespace HysteresisPhaseChange

} // namespace EnergyPlus

#endif

