#ifndef MundtSimMgr_hh_INCLUDED
#define MundtSimMgr_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace MundtSimMgr {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern Real64 const CpAir; // Specific heat of air
	extern Real64 const MinSlope; // Bound on result from Mundt model
	extern Real64 const MaxSlope; // Bound on result from Mundt Model

	// MODULE DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D_int FloorSurfSetIDs; // fixed variable for floors
	extern Array1D_int TheseSurfIDs; // temporary working variable
	extern int MundtCeilAirID; // air node index in AirDataManager
	extern int MundtFootAirID; // air node index in AirDataManager
	extern int SupplyNodeID; // air node index in AirDataManager
	extern int TstatNodeID; // air node index in AirDataManager
	extern int ReturnNodeID; // air node index in AirDataManager
	extern int NumRoomNodes; // number of nodes connected to walls
	extern int NumFloorSurfs; // total number of surfaces for floor
	extern Array1D_int RoomNodeIDs; // ids of the first NumRoomNode Air Nodes
	extern Array1D_int ID1dSurf; // numbers used to identify surfaces
	extern int MundtZoneNum; // index of zones using Mundt model
	extern Real64 ZoneHeight; // zone height
	extern Real64 ZoneFloorArea; // zone floor area
	extern Real64 QventCool; // heat gain due to ventilation
	extern Real64 ConvIntGain; // heat gain due to internal gains
	extern Real64 SupplyAirTemp; // supply air temperature
	extern Real64 SupplyAirVolumeRate; // supply air volume flowrate
	extern Real64 ZoneAirDensity; // zone air density
	extern Real64 QsysCoolTot; // zone sensible cooling load

	// SUBROUTINE SPECIFICATIONS FOR MODULE MundtSimMgr

	// main subsroutine

	// Routines for transferring data between surface and air domains

	// Routines for actual calculations in Mundt model

	// Types

	struct DefineLinearModelNode
	{
		// Members
		std::string AirNodeName; // Name of air nodes
		int ClassType; // Type of air nodes
		Real64 Height; // Z coordinates [m] node's Control Vol. center
		Real64 Temp; // Surface temperature BC
		Array1D_bool SurfMask; // Limit of 60 surfaces at current sizing

		// Default Constructor
		DefineLinearModelNode() :
			ClassType( 0 ),
			Height( 0.0 ),
			Temp( 0.0 )
		{}

		// Member Constructor
		DefineLinearModelNode(
			std::string const & AirNodeName, // Name of air nodes
			int const ClassType, // Type of air nodes
			Real64 const Height, // Z coordinates [m] node's Control Vol. center
			Real64 const Temp, // Surface temperature BC
			Array1_bool const & SurfMask // Limit of 60 surfaces at current sizing
		) :
			AirNodeName( AirNodeName ),
			ClassType( ClassType ),
			Height( Height ),
			Temp( Temp ),
			SurfMask( SurfMask )
		{}

	};

	struct DefineSurfaceSettings
	{
		// Members
		Real64 Area; // m2
		Real64 Temp; // surface temperature BC
		Real64 Hc; // convective film coeff BC
		Real64 TMeanAir; // effective near-surface air temp from air model solution

		// Default Constructor
		DefineSurfaceSettings() :
			Area( 0.0 ),
			Temp( 0.0 ),
			Hc( 0.0 ),
			TMeanAir( 0.0 )
		{}

		// Member Constructor
		DefineSurfaceSettings(
			Real64 const Area, // m2
			Real64 const Temp, // surface temperature BC
			Real64 const Hc, // convective film coeff BC
			Real64 const TMeanAir // effective near-surface air temp from air model solution
		) :
			Area( Area ),
			Temp( Temp ),
			Hc( Hc ),
			TMeanAir( TMeanAir )
		{}

	};

	struct DefineZoneData
	{
		// Members
		int SurfFirst; // index for first surface of the zone
		int NumOfSurfs; // number of surfaces in the zone
		int MundtZoneIndex; // index for zones using Mundt model

		// Default Constructor
		DefineZoneData() :
			SurfFirst( 0 ),
			NumOfSurfs( 0 ),
			MundtZoneIndex( 0 )
		{}

		// Member Constructor
		DefineZoneData(
			int const SurfFirst, // index for first surface of the zone
			int const NumOfSurfs, // number of surfaces in the zone
			int const MundtZoneIndex // index for zones using Mundt model
		) :
			SurfFirst( SurfFirst ),
			NumOfSurfs( NumOfSurfs ),
			MundtZoneIndex( MundtZoneIndex )
		{}

	};

	// Object Data
	extern Array1D< DefineZoneData > ZoneData; // zone data
	extern Array2D< DefineLinearModelNode > LineNode; // air nodes
	extern Array2D< DefineSurfaceSettings > MundtAirSurf; // surfaces
	extern Array1D< DefineSurfaceSettings > FloorSurf; // floor

	// Functions

	void
	ManageMundtModel( int const ZoneNum ); // index number for the specified zone

	//*****************************************************************************************

	void
	InitMundtModel();

	//*****************************************************************************************

	void
	GetSurfHBDataForMundtModel( int const ZoneNum ); // index number for the specified zone

	//*****************************************************************************************

	void
	SetupMundtModel(
		int const ZoneNum, // index number for the specified zone
		bool & ErrorsFound // true if problems setting up model
	);

	//*****************************************************************************************

	void
	CalcMundtModel( int const ZoneNum ); // index number for the specified zone

	//*****************************************************************************************

	void
	SetNodeResult(
		int const NodeID, // node ID
		Real64 const TempResult // temperature for the specified air node
	);

	//*****************************************************************************************

	void
	SetSurfTmeanAir(
		int const SurfID, // surface ID
		Real64 const TeffAir // temperature of air node adjacent to the specified surface
	);

	//*****************************************************************************************

	void
	SetSurfHBDataForMundtModel( int const ZoneNum ); // index number for the specified zone

	//*****************************************************************************************

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // MundtSimMgr

} // EnergyPlus

#endif
