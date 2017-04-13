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

#ifndef HybridEvaporativeCooler_hh_INCLUDED
#define HybridEvaporativeCooler_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataZoneEquipment.hh>
#include <HybridEvapCoolingModel.hh>
namespace EnergyPlus {

	namespace HybridEvaporativeCooler {

		// Using/Aliasing

		using HybridEvapCoolingModel::Model;
		struct ZoneEvapCoolerHybridStruct
		{
			std::string EvapCoolerHybridName; // Name of the EvapCoolerHybrid
			std::string Name; // user identifier
			//std::unique_ptr <Model> pHybrid_Model;
			Model* Hybrid_Model;
			int ZoneNodeNum;
			std::string Path;//X:\\LBNL_WCEC\\FMUDev\\HybridEvapModel\\HybridEvapCooling
			std::string Schedule; // HeatingCoil Operation Schedule
			std::string Mode1_Tsa_Lookup_Name;   
			std::string Mode1_Hsa_Lookup_Name;
			std::string Mode1_Power_Lookup_Name;
			Real64 MsaCapacityRatedCond;
			int SchedPtr; // Pointer to the correct schedule
			Real64 UnitTotalCoolingRate; // unit output to zone, total cooling rate [W]
			Real64 UnitTotalCoolingEnergy; // unit output to zone, total cooling energy [J]
			Real64 UnitSensibleCoolingRate;
			Real64 UnitSensibleCoolingEnergy;
			Real64 RequestedLoadToCoolingSetpoint;
			int Mode;
			int ErrorCode;
			int InletNode;
			int OutletNode;
			int SecondaryInletNode; // This is usually OA node feeding into the purge/secondary side
			int SecondaryOutletNode; // This outlet node of the secondary side and ilet to the secondary fan
			Real64 InletMassFlowRate; // Inlet is primary process air node at inlet to cooler
			Real64 InletTemp;
			Real64 InletWetBulbTemp;
			Real64 InletHumRat;
			Real64 InletEnthalpy;
			Real64 InletPressure;
			Real64 InletRH;
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
										   // Default Constructor
			ZoneEvapCoolerHybridStruct() :
				ZoneNodeNum(0),
				MsaCapacityRatedCond(0),
				SchedPtr(0),
				UnitTotalCoolingRate(0.0),
				UnitTotalCoolingEnergy(0.0),
				UnitSensibleCoolingRate(0.0),
				UnitSensibleCoolingEnergy(0.0),
				RequestedLoadToCoolingSetpoint(0.0),
				Mode(0),
				ErrorCode(0),
				InletNode(0),
				OutletNode(0),
				SecondaryInletNode(0),
				SecondaryOutletNode(0),
				InletMassFlowRate(0.0),
				InletTemp(0.0),
				InletWetBulbTemp(0.0),
				InletHumRat(0.0),
				InletEnthalpy(0.0),
				InletPressure(0.0),
				InletRH(0.0),
				OutletMassFlowRate(0.0),
				OutletTemp(0.0),
				OutletWetBulbTemp(0.0),
				OutletHumRat(0.0),
				OutletEnthalpy(0.0),
				OutletPressure(0.0),
				OutletRH(0.0),
				SecInletMassFlowRate(0.0),
				SecInletTemp(0.0),
				SecInletWetBulbTemp(0.0),
				SecInletHumRat(0.0),
				SecInletEnthalpy(0.0),
				SecInletPressure(0.0),
				SecInletRH(0.0),
				SecOutletMassFlowRate(0.0),
				SecOutletTemp(0.0),
				SecOutletWetBulbTemp(0.0),
				SecOutletHumRat(0.0),
				SecOutletEnthalpy(0.0),
				SecOutletPressure(0.0),
				SecOutletRH(0.0)
			{}

		};
		// Data
		// MODULE PARAMETER DEFINITIONS

		void
			SimZoneHybridEvaporativeCooler(
				std::string const & CompName, // name of the packaged terminal heat pump
				int const ZoneNum, // number of zone being served
				Real64 & SensibleOutputProvided, // sensible capacity delivered to zone
				Real64 & LatentOutputProvided, // Latent add/removal  (kg/s), dehumid = negative
				int & CompIndex // index to zone hvac unit
				);

		void
			GetInputZoneHybridEvaporativeCooler();

		void
			InitZoneHybridEvaporativeCooler(
				int const UnitNum, // unit number
				int const ZoneNum // number of zone being served
				);

		void
			CalcZoneHybridEvaporativeCooler(
				int const UnitNum, // unit number
				int const ZoneNum, // number of zone being served
				Real64 & SensibleOutputProvided, // sensible capacity delivered to zone
				Real64 & LatentOutputProvided // Latent add/removal  (kg/s), dehumid = negative
				);

		void
			ReportZoneHybridEvaporativeCooler(int const UnitNum); // unit number
		double Sat_press(double Tdb);
		double Part_press(double P, double W);

	}
}
#endif