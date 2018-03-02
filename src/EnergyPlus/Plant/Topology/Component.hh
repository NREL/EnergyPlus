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

#ifndef PlantTopologyComponent_hh_INCLUDED
#define PlantTopologyComponent_hh_INCLUDED

#include <PlantComponent.hh>
#include <Plant/Operation/Enums.hh>
#include <Plant/Operation/EquipAndOperations.hh>

namespace EnergyPlus {
	namespace DataPlant {

		struct CompData {
			// Members
			std::string TypeOf; // The 'keyWord' identifying  component type
			int TypeOf_Num; // Reference the "TypeOf" parameters in DataPlant
			int GeneralEquipType; // General Equipment Type (e.g. Chillers, Pumps, etc)
			std::string Name; // Component name
			int CompNum; // Component ID number
			int FlowCtrl; // flow control for splitter/mixer (ACTIVE/PASSIVE/BYPASS)
			int FlowPriority; // status for overall loop flow determination
			bool ON; // TRUE = designated component or operation scheme available
			bool Available; // TRUE = designated component or operation scheme available
			std::string NodeNameIn; // Component inlet node name
			std::string NodeNameOut; // Component outlet node name
			int NodeNumIn; // Component inlet node number
			int NodeNumOut; // Component outlet node number
			Real64 MyLoad; // Distributed Load
			Real64 MaxLoad; // Maximum load
			Real64 MinLoad; // Minimum Load
			Real64 OptLoad; // Optimal Load
			Real64 SizFac; // Sizing Fraction
			int CurOpSchemeType; // updated pointer to
			// Plant()%OpScheme(CurOpSchemeType)...
			int NumOpSchemes; // number of schemes held in the pointer array
			int CurCompLevelOpNum; // pointer to the OpScheme array defined next
			// PlantLoop()%LoopSide()%Branch()%Comp()%OpScheme(curOpSchemePtr)
			Array1D <OpSchemePtrData> OpScheme; // Pointers to component on lists
			Real64 EquipDemand; // Component load request based on inlet temp and outlet SP
			bool EMSLoadOverrideOn; // EMS is calling to override load dispatched to component
			Real64 EMSLoadOverrideValue; // EMS value to use for load when overridden [W] always positive.
			int HowLoadServed; // nature of component in terms of how it can meet load
			Real64 MinOutletTemp; // Component exit lower limit temperature
			Real64 MaxOutletTemp; // Component exit upper limit temperature
			bool FreeCoolCntrlShutDown; // true if component was shut down because of free cooling
			Real64 FreeCoolCntrlMinCntrlTemp; // current control temp value for free cooling controls
			int FreeCoolCntrlMode; // type of sensor used for free cooling controls
			int FreeCoolCntrlNodeNum; // chiller condenser inlet node number for free cooling controls
			int IndexInLoopSidePumps; // If I'm a pump, this tells my index in PL(:)%LS(:)%Pumps
			Real64 TempDesCondIn;
			Real64 TempDesEvapOut;
			PlantComponent *compPtr;

			// Default Constructor
			CompData() :
					TypeOf_Num(0),
					GeneralEquipType(0),
					CompNum(0),
					FlowCtrl(0),
					FlowPriority(LoopFlowStatus_Unknown),
					ON(false),
					Available(false),
					NodeNumIn(0),
					NodeNumOut(0),
					MyLoad(0.0),
					MaxLoad(0.0),
					MinLoad(0.0),
					OptLoad(0.0),
					SizFac(0.0),
					CurOpSchemeType(UnknownStatusOpSchemeType),
					NumOpSchemes(0),
					CurCompLevelOpNum(0),
					EquipDemand(0.0),
					EMSLoadOverrideOn(false),
					EMSLoadOverrideValue(0.0),
					HowLoadServed(HowMet_Unknown),
					MinOutletTemp(0.0),
					MaxOutletTemp(0.0),
					FreeCoolCntrlShutDown(false),
					FreeCoolCntrlMinCntrlTemp(0.0),
					FreeCoolCntrlMode(0),
					FreeCoolCntrlNodeNum(0),
					IndexInLoopSidePumps(0),
					TempDesCondIn(0.0),
					TempDesEvapOut(0.0),
					compPtr(nullptr) {}

		};
	}
}

#endif
