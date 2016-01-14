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

} // MundtSimMgr

} // EnergyPlus

#endif
