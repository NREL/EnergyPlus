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

} // RoomAirModelManager

} // EnergyPlus

#endif
