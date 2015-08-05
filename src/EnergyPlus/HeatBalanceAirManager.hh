#ifndef HeatBalanceAirManager_hh_INCLUDED
#define HeatBalanceAirManager_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace HeatBalanceAirManager {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	//         Subroutine Specifications for the Heat Balance Module
	// Driver Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module
	// Reporting routines for module

	// Functions

	void
	ManageAirHeatBalance();

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetAirHeatBalanceInput();

	void
	GetAirFlowFlag( bool & ErrorsFound ); // Set to true if errors found

	void
	SetZoneMassConservationFlag();  // sets the zone air mass flow variables

	void
	GetSimpleAirModelInputs( bool & ErrorsFound ); // IF errors found in input

	//*****************************************************************************************
	// This subroutine was moved from 'RoomAirManager' Module

	void
	GetRoomAirModelParameters( bool & errFlag ); // True if errors found during this input routine

	// END of Get Input subroutines for the HBAir Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitAirHeatBalance();

	void
	AllocateAirHeatBalArrays();

	void
	InitSimpleMixingConvectiveHeatGains();

	// END Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcHeatBalanceAir();

	// END Algorithm Section of the Module

	void
	ReportZoneMeanAirTemp();

	// *****************************************************************************

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // HeatBalanceAirManager

} // EnergyPlus

#endif
