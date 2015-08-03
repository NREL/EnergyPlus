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

		// Member Constructor
		UTSCDataStruct(
			std::string const & Name,
			std::string const & OSCMName, // OtherSideConditionsModel
			int const OSCMPtr, // OtherSideConditionsModel index
			int const SchedPtr, // Availablity schedule
			Array1_int const & InletNode, // Air system node "pointer", should be set to outdoor air
			Array1_int const & OutletNode, // Air system node "pointer", outlet from UTSC
			Array1_int const & ControlNode, // Air system node "pointer", should have mixed air setpoint
			Array1_int const & ZoneNode, // Air system node "pointer", should have zone node
			int const Layout, // 'Square' or 'Triangle'
			int const Correlation, // which heat exchanger effectiveness model
			Real64 const HoleDia, // Diameter of Perforations in Collector [m]
			Real64 const Pitch, // Distance between Perforations in Collector [m]
			Real64 const LWEmitt, // Thermal Emissivity of Collector Surface [dimensionless]
			Real64 const SolAbsorp, // Solar Absorbtivity of Collector Surface [dimensionless]
			int const CollRoughness, // surface roughness for exterior convection calcs.
			Real64 const PlenGapThick, // Depth of Plenum Behind Collector [m]
			Real64 const PlenCrossArea, // cross section area of plenum behind collector [m2]
			int const NumSurfs, // a single collector can have multiple surfaces underneath it
			Array1_int const & SurfPtrs, // = 0  ! array of pointers for participating underlying surfaces
			Real64 const Height, // Overall Height of Collector  [m]
			Real64 const AreaRatio, // Ratio of actual surface are to projected surface area [dimensionless]
			Real64 const CollectThick, // Thickness of collector absorber plate material.  [m]
			Real64 const Cv, // volume-based effectiveness of openings for wind-driven vent when Passive
			Real64 const Cd, // discharge coefficient of openings for bouyancy-driven vent when Passive
			int const NumOASysAttached, // =1 if no splitter, other wise set by Splitter object
			int const FreeHeatSetPointSchedPtr, // used for controlling seperately from usual setpoint managers.
			int const VsucErrIndex,
			Real64 const ActualArea, // Overall Area of Collect with surface corrugations.
			Real64 const ProjArea, // Overall Area of Collector projected, as if flat [m2]
			Vector const & Centroid, // computed centroid
			Real64 const Porosity, // fraction of absorber plate [--]
			bool const IsOn, // .TRUE. means "on" or "ACTIVE" , .false means "off" or "PASSIVE
			Real64 const Tplen, // modeled drybulb temperature for air between collector and wall [C]
			Real64 const Tcoll, // modeled surface temperature for collector [C]
			Real64 const TplenLast, // Old Value for modeled drybulb temp if air between collector and wall [C]
			Real64 const TcollLast, // Old value for modeled surface temperature for collector [C]
			Real64 const HrPlen, // Modeled radiation coef for OSCM [W/m2-C]
			Real64 const HcPlen, // Modeled Convection coef for OSCM [W/m2-C]
			Real64 const MdotVent, // air mass flow exchanging with ambient when passive.
			Real64 const HdeltaNPL, // lenth scale for bouyancy-driven vent when Passive [m]
			Real64 const TairHX, // air drybulb of air leaving collector when Active [C]
			Real64 const InletMDot, // flow rate from outdoor mixer controller
			Real64 const InletTempDB,
			Real64 const Tilt, // Tilt from area weighted average of underlying surfaces
			Real64 const Azimuth, // Azimuth from area weighted average of underlying surfaces
			Real64 const QdotSource, // Source/sink term
			Real64 const Isc, // total incident solar on collector [W]
			Real64 const HXeff, // heat exchanger effectiveness [--]
			Real64 const Vsuction, // Average suction face velocity [m/s]
			Real64 const PassiveACH, // air changes per hour when passive [1/hr]
			Real64 const PassiveMdotVent, // Total Nat Vent air change rate  [kg/s]
			Real64 const PassiveMdotWind, // Nat Vent air change rate from Wind-driven [kg/s]
			Real64 const PassiveMdotTherm, // Nat. Vent air change rate from bouyancy-driven flow [kg/s]
			Real64 const PlenumVelocity, // effective velocity inside plenum [m/s]
			Real64 const SupOutTemp, // supply air outlet temperature [C]
			Real64 const SupOutHumRat, // supply air outlet humidity ratio [kg water/kg dry air]
			Real64 const SupOutEnth, // supply air outlet enthalpy [J/kg]
			Real64 const SupOutMassFlow, // supply air outlet mass flow rate [kg/s]
			Real64 const SensHeatingRate, // rate of sensible heat being added to the supply (primary) air [W]
			Real64 const SensHeatingEnergy, // sensible heat added to the supply (primary) air [J]
			Real64 const SensCoolingRate, // rate of sensible heat being removed from the supply (primary) air [W]
			Real64 const SensCoolingEnergy, // sensible heat removed from the supply (primary) air [J]
			Real64 const UTSCEfficiency, // Total Efficiency (with wall) SensHeatingRate/IncidentRadiation[--]
			Real64 const UTSCCollEff // Collector-only Efficiency [--]
		) :
			Name( Name ),
			OSCMName( OSCMName ),
			OSCMPtr( OSCMPtr ),
			SchedPtr( SchedPtr ),
			InletNode( InletNode ),
			OutletNode( OutletNode ),
			ControlNode( ControlNode ),
			ZoneNode( ZoneNode ),
			Layout( Layout ),
			Correlation( Correlation ),
			HoleDia( HoleDia ),
			Pitch( Pitch ),
			LWEmitt( LWEmitt ),
			SolAbsorp( SolAbsorp ),
			CollRoughness( CollRoughness ),
			PlenGapThick( PlenGapThick ),
			PlenCrossArea( PlenCrossArea ),
			NumSurfs( NumSurfs ),
			SurfPtrs( SurfPtrs ),
			Height( Height ),
			AreaRatio( AreaRatio ),
			CollectThick( CollectThick ),
			Cv( Cv ),
			Cd( Cd ),
			NumOASysAttached( NumOASysAttached ),
			FreeHeatSetPointSchedPtr( FreeHeatSetPointSchedPtr ),
			VsucErrIndex( VsucErrIndex ),
			ActualArea( ActualArea ),
			ProjArea( ProjArea ),
			Centroid( Centroid ),
			Porosity( Porosity ),
			IsOn( IsOn ),
			Tplen( Tplen ),
			Tcoll( Tcoll ),
			TplenLast( TplenLast ),
			TcollLast( TcollLast ),
			HrPlen( HrPlen ),
			HcPlen( HcPlen ),
			MdotVent( MdotVent ),
			HdeltaNPL( HdeltaNPL ),
			TairHX( TairHX ),
			InletMDot( InletMDot ),
			InletTempDB( InletTempDB ),
			Tilt( Tilt ),
			Azimuth( Azimuth ),
			QdotSource( QdotSource ),
			Isc( Isc ),
			HXeff( HXeff ),
			Vsuction( Vsuction ),
			PassiveACH( PassiveACH ),
			PassiveMdotVent( PassiveMdotVent ),
			PassiveMdotWind( PassiveMdotWind ),
			PassiveMdotTherm( PassiveMdotTherm ),
			PlenumVelocity( PlenumVelocity ),
			SupOutTemp( SupOutTemp ),
			SupOutHumRat( SupOutHumRat ),
			SupOutEnth( SupOutEnth ),
			SupOutMassFlow( SupOutMassFlow ),
			SensHeatingRate( SensHeatingRate ),
			SensHeatingEnergy( SensHeatingEnergy ),
			SensCoolingRate( SensCoolingRate ),
			SensCoolingEnergy( SensCoolingEnergy ),
			UTSCEfficiency( UTSCEfficiency ),
			UTSCCollEff( UTSCCollEff )
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

} // TranspiredCollector

} // EnergyPlus

#endif
