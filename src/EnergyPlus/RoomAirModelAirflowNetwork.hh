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

} // RoomAirModelAirflowNetwork

} // EnergyPlus

#endif
