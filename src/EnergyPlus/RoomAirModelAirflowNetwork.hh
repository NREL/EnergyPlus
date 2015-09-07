#ifndef RoomAirModelAirflowNetwork_hh_INCLUDED
#define RoomAirModelAirflowNetwork_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace RoomAirModelAirflowNetwork {

	// Data
	class RAFNData {

	private:

	public:

		int ZoneNum;
		int RoomAirNode;

		// constructor
		RAFNData( ):
			ZoneNum( 0 ),
			RoomAirNode( 0 )
		{}

		// functions

		//*****************************************************************************************
		void
		InitRoomAirModelAirflowNetwork( int const RoomAirNode ); // index number for the specified zone and room air node

		//*****************************************************************************************
		void
		CalcRoomAirModelAirflowNetwork( int const ThisRoomAirNode ); // index number for the specified zone and room air node

		//*****************************************************************************************
		void
		UpdateRoomAirModelAirflowNetwork(  ); // index number for the specified zone

		//*****************************************************************************************
		void
		CalcNodeSums( int const RoomAirNode ); // index number for the specified zone and room air node

		//*****************************************************************************************
		void
		SumNonAirSystemResponseForNode( int const RoomAirNode ); // index number for the specified zone and room air node
		//*****************************************************************************************
		void
		SumSystemDepResponseForNode(  ); // index number for the specified zone and room air node

		//*****************************************************************************************

		void
		CalcSurfaceMoistureSums(
			int const RoomAirNode,
			Real64 & SumHmAW,
			Real64 & SumHmARa,
			Real64 & SumHmARaW,
			Array1 < bool > const & SurfMask
			);
	};

	// Object data
	extern Array1D< RAFNData > RAFN;

	void
	SimRoomAirModelAirflowNetwork( int const ZoneNum ); // index number for the specified zone

	void
	LoadPredictionRoomAirModelAirflowNetwork( int const ZoneNum, int const RoomAirNode ); // index number for the specified zone and node


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

} // RoomAirModelAirflowNetwork

} // EnergyPlus

#endif
