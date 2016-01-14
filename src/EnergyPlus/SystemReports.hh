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

#ifndef SystemReports_hh_INCLUDED
#define SystemReports_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace SystemReports {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const NoHeatNoCool;
	extern int const CoolingOnly;
	extern int const HeatingOnly;
	extern int const HeatAndCool;
	extern int const MaxSetBackCount;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	//Ventilation Report Variables
	extern Array1D< Real64 > MaxCoolingLoadMetByVent;
	extern Array1D< Real64 > MaxCoolingLoadAddedByVent;
	extern Array1D< Real64 > MaxOvercoolingByVent;
	extern Array1D< Real64 > MaxHeatingLoadMetByVent;
	extern Array1D< Real64 > MaxHeatingLoadAddedByVent;
	extern Array1D< Real64 > MaxOverheatingByVent;
	extern Array1D< Real64 > MaxNoLoadHeatingByVent;
	extern Array1D< Real64 > MaxNoLoadCoolingByVent;

	extern Array1D< Real64 > RemMaxCoolingLoadMetByVent;
	extern Array1D< Real64 > RemMaxCoolingLoadAddedByVent;
	extern Array1D< Real64 > RemMaxOvercoolingByVent;
	extern Array1D< Real64 > RemMaxHeatingLoadMetByVent;
	extern Array1D< Real64 > RemMaxHeatingLoadAddedByVent;
	extern Array1D< Real64 > RemMaxOverheatingByVent;
	extern Array1D< Real64 > RemMaxNoLoadHeatingByVent;
	extern Array1D< Real64 > RemMaxNoLoadCoolingByVent;

	extern Array1D< Real64 > LastMaxCoolingLoadMetByVent;
	extern Array1D< Real64 > LastMaxCoolingLoadAddedByVent;
	extern Array1D< Real64 > LastMaxOvercoolingByVent;
	extern Array1D< Real64 > LastMaxHeatingLoadMetByVent;
	extern Array1D< Real64 > LastMaxHeatingLoadAddedByVent;
	extern Array1D< Real64 > LastMaxOverheatingByVent;
	extern Array1D< Real64 > LastMaxNoLoadHeatingByVent;
	extern Array1D< Real64 > LastMaxNoLoadCoolingByVent;

	extern Array1D< Real64 > SysTotZoneLoadHTNG;
	extern Array1D< Real64 > SysTotZoneLoadCLNG;
	extern Array1D< Real64 > SysOALoadHTNG;
	extern Array1D< Real64 > SysOALoadCLNG;
	extern Array1D< Real64 > SysTotHTNG;
	extern Array1D< Real64 > SysTotCLNG;

	extern Array1D< Real64 > SysTotH2OHOT;
	extern Array1D< Real64 > SysTotH2OCOLD;
	extern Array1D< Real64 > SysTotElec;
	extern Array1D< Real64 > SysTotGas;
	extern Array1D< Real64 > SysTotSteam;

	extern Array1D< Real64 > SysHumidHTNG;
	extern Array1D< Real64 > SysHumidElec;
	extern Array1D< Real64 > SysHumidGas;
	extern Array1D< Real64 > SysEvapCLNG;
	extern Array1D< Real64 > SysEvapElec;
	extern Array1D< Real64 > SysHeatExHTNG;
	extern Array1D< Real64 > SysHeatExCLNG;
	extern Array1D< Real64 > DesDehumidCLNG;
	extern Array1D< Real64 > DesDehumidElec;
	extern Array1D< Real64 > SysSolarCollectHeating;
	extern Array1D< Real64 > SysSolarCollectCooling;
	extern Array1D< Real64 > SysUserDefinedTerminalHeating;
	extern Array1D< Real64 > SysUserDefinedTerminalCooling;

	extern Array1D< Real64 > SysFANCompHTNG;
	extern Array1D< Real64 > SysFANCompElec;
	extern Array1D< Real64 > SysCCCompCLNG;
	extern Array1D< Real64 > SysCCCompH2OCOLD;
	extern Array1D< Real64 > SysCCCompElec;
	extern Array1D< Real64 > SysHCCompH2OHOT;
	extern Array1D< Real64 > SysHCCompElec;
	extern Array1D< Real64 > SysHCCompElecRes;
	extern Array1D< Real64 > SysHCCompHTNG;
	extern Array1D< Real64 > SysHCCompGas;
	extern Array1D< Real64 > SysHCCompSteam;
	extern Array1D< Real64 > SysDomesticH20;

	extern Array1D< Real64 > ZoneOAMassFlow; // zone mech vent mass flow rate {kg/s}
	extern Array1D< Real64 > ZoneOAMass; // zone mech vent total mass for time {kg}
	extern Array1D< Real64 > ZoneOAVolFlowStdRho; // zone mech vent volume flow rate at standard density {m3/s}
	extern Array1D< Real64 > ZoneOAVolStdRho; // zone mech vent total volume OA at standard density {m3/s}
	extern Array1D< Real64 > ZoneOAVolFlowCrntRho; // zone mech vent volume flow rate at current density {m3/s}
	extern Array1D< Real64 > ZoneOAVolCrntRho; // zone mech vent total volume OA at current density {m3/s}
	extern Array1D< Real64 > ZoneMechACH; // zone mech vent air changes per hour {ACH}

	extern bool AirLoopLoadsReportEnabled;
	extern bool VentLoadsReportEnabled;
	extern bool VentEnergyReportEnabled;
	extern bool VentReportStructureCreated;
	extern int TotalLoopConnects; // Total number of loop connections
	extern int MaxLoopArraySize;
	extern int MaxCompArraySize;
	extern int DBFlag;

	extern Array1D_int SetBackCounter;
	extern Array1D_int HeatCoolFlag;
	extern Array1D_int FirstHeatCoolFlag;
	extern Array1D_int FirstHeatCoolHour;
	extern Array1D_int LastHeatCoolFlag;
	extern Array1D_int LastHeatCoolHour;
	extern Array1D_bool AirLoopCalcDone;
	extern Array1D_bool NoLoadFlag;
	extern Array1D_bool UnmetLoadFlag;

	// SUBROUTINE SPECIFICATIONS FOR MODULE SystemReports

	//Reporting Initialization

	// Reporting routines for module

	// Types

	struct Energy
	{
		// Members
		Real64 TotDemand;
		Real64 Elec;
		Real64 Gas;
		Real64 Purch;
		Real64 Other;

		// Default Constructor
		Energy() :
			TotDemand( 0.0 ),
			Elec( 0.0 ),
			Gas( 0.0 ),
			Purch( 0.0 ),
			Other( 0.0 )
		{}

	};

	struct CoilType
	{
		// Members
		Energy DecreasedCC; // LoadMetByVent
		Energy DecreasedHC; // LoadMetByVent
		Energy IncreasedCC; // LoadIncreasedVent
		Energy IncreasedHC; // LoadAddedByVent
		Energy ReducedByCC; // LoadAddedByVent
		Energy ReducedByHC; // LoadAddedByVent

		// Default Constructor
		CoilType()
		{}

	};

	struct SummarizeLoads
	{
		// Members
		CoilType Load; // LoadMetByVent
		CoilType NoLoad; // LoadMetByVentNoLoad
		CoilType ExcessLoad; // LoadAddedByVentOvercool
		CoilType PotentialSavings; // LoadAddedByVentCoolLost
		CoilType PotentialCost; // LoadAddedByVentHeatLost

		// Default Constructor
		SummarizeLoads()
		{}

	};

	// Object Data
	extern Array1D< SummarizeLoads > Vent;

	// Functions

	void
	InitEnergyReports();

	void
	FindFirstLastPtr(
		int & LoopType,
		int & LoopNum,
		int & ArrayCount,
		int & LoopCount,
		bool & ConnectionFlag
	);

	void
	UpdateZoneCompPtrArray(
		int & Idx,
		int const ListNum,
		int const AirDistUnitNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	);

	void
	UpdateZoneSubCompPtrArray(
		int & Idx,
		int const ListNum,
		int const AirDistUnitNum,
		int const SubCompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	);

	void
	UpdateZoneSubSubCompPtrArray(
		int & Idx,
		int const ListNum,
		int const AirDistUnitNum,
		int const SubCompNum,
		int const SubSubCompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	);

	void
	UpdateAirSysCompPtrArray(
		int & Idx,
		int const AirLoopNum,
		int const BranchNum,
		int const CompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	);

	void
	UpdateAirSysSubCompPtrArray(
		int & Idx,
		int const AirLoopNum,
		int const BranchNum,
		int const CompNum,
		int const SubCompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	);

	void
	UpdateAirSysSubSubCompPtrArray(
		int & Idx,
		int const AirLoopNum,
		int const BranchNum,
		int const CompNum,
		int const SubCompNum,
		int const SubSubCompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	);

	void
	AllocateAndSetUpVentReports();

	void
	CreateEnergyReportStructure();

	// End Initialization Section of the Module
	//******************************************************************************

	// Beginning of Reporting subroutines for the SimAir Module
	// *****************************************************************************

	void
	ReportSystemEnergyUse();

	void
	CalcSystemEnergyUse(
		bool const CompLoadFlag,
		int const AirLoopNum,
		std::string const & CompType,
		int const EnergyType,
		Real64 const CompLoad,
		Real64 const CompEnergy
	);

	void
	ReportMaxVentilationLoads();

	void
	MatchPlantSys(
		int const AirLoopNum, // counter for zone air distribution inlets
		int const BranchNum // counter for zone air distribution inlets
	);

	void
	FindDemandSideMatch(
		std::string const & CompType, // Inlet node of the component to find the match of
		std::string const & CompName, // Outlet node of the component to find the match of
		bool & MatchFound, // Set to .TRUE. when a match is found
		int & MatchLoopType, // Loop number of the match
		int & MatchLoop, // Loop number of the match
		int & MatchBranch, // Branch number of the match
		int & MatchComp // Component number of the match
	);

	void
	ReportAirLoopConnections();

	//        End of Reporting subroutines for the SimAir Module
	// *****************************************************************************

} // SystemReports

} // EnergyPlus

#endif
