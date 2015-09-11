#ifndef RoomAirModelManager_hh_INCLUDED
#define RoomAirModelManager_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace RoomAirModelManager {

	// Data
	// MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern bool GetUCSDDVDataFlag; // UCSD
	extern bool GetAirModelData;  // Used to "get" all air model data

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Functions

	void
	ManageAirModel( int & ZoneNum );

	//*****************************************************************************************

	void
	GetAirModelDatas();

	void
	GetUserDefinedPatternData( bool & ErrorsFound ); // True if errors found during this get input routine

	void
	GetAirNodeData( bool & ErrorsFound ); // True if errors found during this get input routine

	//*****************************************************************************************

	void
	GetMundtData( bool & ErrorsFound ); // True if errors found during this get input routine

	void
	GetDisplacementVentData( bool & ErrorsFound ); // True if errors found during this get input routine

	void
	GetCrossVentData( bool & ErrorsFound ); // True if errors found during this get input routine

	void
	GetUFADZoneData( bool & ErrorsFound ); // True if errors found during this get input routine

	void
	SharedDVCVUFDataInit( int & ZoneNum );

	void
	GetRoomAirflowNetworkData( bool & ErrorsFound ); // True if errors found during this get input routine

	void
	GetRAFNNodeNum( std::string const & RAFNNodeName, int & ZoneNum, int & RAFNNodeNum, bool & Errorfound ); // find zone number and node number based on the node name

	bool
	CheckEquipName( int ZoneNum, std::string const & EquipType, std::string const & EquipName, std::string & SupplyNodeName, std::string & ReturnNodeName, int TotNumEquip, int TypeNum ); // Ensure valid equipment name

	//*****************************************************************************************
	//     NOTICE

	//     Copyright (c) 1996-2015 The Board of Trustees of the University of Illinois
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

} // RoomAirModelManager

} // EnergyPlus

#endif
