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

#ifndef TranspiredCollector_hh_INCLUDED
#define TranspiredCollector_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataVectorTypes.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace TranspiredCollector {

	// Using/Aliasing
	using DataVectorTypes::Vector;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const Layout_Square;
	extern int const Layout_Triangle;
	extern int const Correlation_Kutscher1994;
	extern int const Correlation_VanDeckerHollandsBrunger2001;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumUTSC; // number of transpired collectors in model
	extern Array1D_bool CheckEquipName;
	extern bool GetInputFlag; // First time, input is gotten

	// SUBROUTINE SPECIFICATIONS FOR MODULE TranspiredCollector:

	// Types

	struct UTSCDataStruct
	{
		// Members
		// from input data
		std::string Name;
		std::string OSCMName; // OtherSideConditionsModel
		int OSCMPtr; // OtherSideConditionsModel index
		int SchedPtr; // Availablity schedule
		Array1D_int InletNode; // Air system node "pointer", should be set to outdoor air
		Array1D_int OutletNode; // Air system node "pointer", outlet from UTSC
		Array1D_int ControlNode; // Air system node "pointer", should have mixed air setpoint
		Array1D_int ZoneNode; // Air system node "pointer", should have zone node
		int Layout; // 'Square' or 'Triangle'
		int Correlation; // which heat exchanger effectiveness model
		Real64 HoleDia; // Diameter of Perforations in Collector [m]
		Real64 Pitch; // Distance between Perforations in Collector [m]
		Real64 LWEmitt; // Thermal Emissivity of Collector Surface [dimensionless]
		Real64 SolAbsorp; // Solar Absorbtivity of Collector Surface [dimensionless]
		int CollRoughness; // surface roughness for exterior convection calcs.
		Real64 PlenGapThick; // Depth of Plenum Behind Collector [m]
		Real64 PlenCrossArea; // cross section area of plenum behind collector [m2]
		int NumSurfs; // a single collector can have multiple surfaces underneath it
		Array1D_int SurfPtrs; // = 0  ! array of pointers for participating underlying surfaces
		Real64 Height; // Overall Height of Collector  [m]
		Real64 AreaRatio; // Ratio of actual surface are to projected surface area [dimensionless]
		Real64 CollectThick; // Thickness of collector absorber plate material.  [m]
		Real64 Cv; // volume-based effectiveness of openings for wind-driven vent when Passive
		Real64 Cd; // discharge coefficient of openings for bouyancy-driven vent when Passive
		int NumOASysAttached; // =1 if no splitter, other wise set by Splitter object
		int FreeHeatSetPointSchedPtr; // used for controlling seperately from usual setpoint managers.
		int VsucErrIndex;
		// data from elswhere and calculated
		Real64 ActualArea; // Overall Area of Collect with surface corrugations.
		Real64 ProjArea; // Overall Area of Collector projected, as if flat [m2]
		Vector Centroid; // computed centroid
		Real64 Porosity; // fraction of absorber plate [--]
		bool IsOn; // .TRUE. means "on" or "ACTIVE" , .false means "off" or "PASSIVE
		Real64 Tplen; // modeled drybulb temperature for air between collector and wall [C]
		Real64 Tcoll; // modeled surface temperature for collector [C]
		Real64 TplenLast; // Old Value for modeled drybulb temp if air between collector and wall [C]
		Real64 TcollLast; // Old value for modeled surface temperature for collector [C]
		Real64 HrPlen; // Modeled radiation coef for OSCM [W/m2-C]
		Real64 HcPlen; // Modeled Convection coef for OSCM [W/m2-C]
		Real64 MdotVent; // air mass flow exchanging with ambient when passive.
		Real64 HdeltaNPL; // lenth scale for bouyancy-driven vent when Passive [m]
		Real64 TairHX; // air drybulb of air leaving collector when Active [C]
		Real64 InletMDot; // flow rate from outdoor mixer controller
		Real64 InletTempDB;
		Real64 Tilt; // Tilt from area weighted average of underlying surfaces
		Real64 Azimuth; // Azimuth from area weighted average of underlying surfaces
		Real64 QdotSource; // Source/sink term
		// reporting data
		Real64 Isc; // total incident solar on collector [W]
		Real64 HXeff; // heat exchanger effectiveness [--]
		Real64 Vsuction; // Average suction face velocity [m/s]
		Real64 PassiveACH; // air changes per hour when passive [1/hr]
		Real64 PassiveMdotVent; // Total Nat Vent air change rate  [kg/s]
		Real64 PassiveMdotWind; // Nat Vent air change rate from Wind-driven [kg/s]
		Real64 PassiveMdotTherm; // Nat. Vent air change rate from bouyancy-driven flow [kg/s]
		Real64 PlenumVelocity; // effective velocity inside plenum [m/s]
		Real64 SupOutTemp; // supply air outlet temperature [C]
		Real64 SupOutHumRat; // supply air outlet humidity ratio [kg water/kg dry air]
		Real64 SupOutEnth; // supply air outlet enthalpy [J/kg]
		Real64 SupOutMassFlow; // supply air outlet mass flow rate [kg/s]
		Real64 SensHeatingRate; // rate of sensible heat being added to the supply (primary) air [W]
		Real64 SensHeatingEnergy; // sensible heat added to the supply (primary) air [J]
		Real64 SensCoolingRate; // rate of sensible heat being removed from the supply (primary) air [W]
		Real64 SensCoolingEnergy; // sensible heat removed from the supply (primary) air [J]
		Real64 UTSCEfficiency; // Total Efficiency (with wall) SensHeatingRate/IncidentRadiation[--]
		Real64 UTSCCollEff; // Collector-only Efficiency [--]

		// Default Constructor
		UTSCDataStruct() :
			OSCMPtr( 0 ),
			SchedPtr( 0 ),
			Layout( 0 ),
			Correlation( 0 ),
			HoleDia( 0.0 ),
			Pitch( 0.0 ),
			LWEmitt( 0.0 ),
			SolAbsorp( 0.0 ),
			CollRoughness( 1 ),
			PlenGapThick( 0.0 ),
			PlenCrossArea( 0.0 ),
			NumSurfs( 0 ),
			Height( 0.0 ),
			AreaRatio( 0.0 ),
			CollectThick( 0.0 ),
			Cv( 0.0 ),
			Cd( 0.0 ),
			NumOASysAttached( 0 ),
			FreeHeatSetPointSchedPtr( 0 ),
			VsucErrIndex( 0 ),
			ActualArea( 0.0 ),
			ProjArea( 0.0 ),
			Centroid( 0.0, 0.0, 0.0 ),
			Porosity( 0.0 ),
			IsOn( false ),
			Tplen( 0.0 ),
			Tcoll( 0.0 ),
			TplenLast( 22.5 ),
			TcollLast( 22.0 ),
			HrPlen( 0.0 ),
			HcPlen( 0.0 ),
			MdotVent( 0.0 ),
			HdeltaNPL( 0.0 ),
			TairHX( 0.0 ),
			InletMDot( 0.0 ),
			InletTempDB( 0.0 ),
			Tilt( 0.0 ),
			Azimuth( 0.0 ),
			QdotSource( 0.0 ),
			Isc( 0.0 ),
			HXeff( 0.0 ),
			Vsuction( 0.0 ),
			PassiveACH( 0.0 ),
			PassiveMdotVent( 0.0 ),
			PassiveMdotWind( 0.0 ),
			PassiveMdotTherm( 0.0 ),
			PlenumVelocity( 0.0 ),
			SupOutTemp( 0.0 ),
			SupOutHumRat( 0.0 ),
			SupOutEnth( 0.0 ),
			SupOutMassFlow( 0.0 ),
			SensHeatingRate( 0.0 ),
			SensHeatingEnergy( 0.0 ),
			SensCoolingRate( 0.0 ),
			SensCoolingEnergy( 0.0 ),
			UTSCEfficiency( 0.0 ),
			UTSCCollEff( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< UTSCDataStruct > UTSC;

	// Functions

	void
	SimTranspiredCollector(
		std::string const & CompName, // component name
		int & CompIndex // component index (to reduce string compares during simulation)
	);

	void
	GetTranspiredCollectorInput();

	void
	InitTranspiredCollector( int const UTSCNum ); // compindex already checked in calling routine

	void
	CalcActiveTranspiredCollector( int const UTSCNum );

	void
	CalcPassiveTranspiredCollector( int const UTSCNum );

	void
	UpdateTranspiredCollector( int const UTSCNum );

	void
	SetUTSCQdotSource(
		int const UTSCNum,
		Real64 const QSource // source term in Watts
	);

	void
	GetTranspiredCollectorIndex(
		int const SurfacePtr,
		int & UTSCIndex
	);

	void
	GetUTSCTsColl(
		int const UTSCNum,
		Real64 & TsColl
	);

} // TranspiredCollector

} // EnergyPlus

#endif
