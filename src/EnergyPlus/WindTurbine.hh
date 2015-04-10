#ifndef WindTurbine_hh_INCLUDED
#define WindTurbine_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

// (ref: Object: Generator:WindTurbine)

namespace WindTurbine {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern int const HAWT; // 'HorizontalAxisWindTurbine'
	extern int const VAWT; // 'VerticalAxisWindTurbine'

	extern int const FSFP; // 'FixedSpeedFixedPitch'
	extern int const FSVP; // 'FixedSpeedVariablePitch'
	extern int const VSFP; // 'VariableSpeedFixedPitch'
	extern int const VSVP; // 'VariableSpeedVariablePitch'

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLES DECLARATIONS:
	extern int NumWindTurbines; // Total wind turbine statements in inputs

	// Subroutine Specifications for the Heat Balance Module

	// Types

	struct WindTurbineParams
	{
		// Members
		std::string Name; // The component name
		std::string Schedule; // Available schedule
		int RotorType; // Rotor type (HAWT or VAWT)
		int ControlType; // Control type
		int SchedPtr; // Schedule
		int NumOfBlade; // Blade number
		Real64 RatedRotorSpeed; // Rated rotor speed in m/s
		Real64 RotorDiameter; // Diameter of rotor in m
		Real64 RotorHeight; // Overall height of the rotor in m
		Real64 RatedPower; // Nominal average power outpout at the rated wind speed in Watts
		Real64 RatedWindSpeed; // Rated wind speed showing maximum power output in Watts
		Real64 CutInSpeed; // Minimum wind speed for system operation in m/s
		Real64 CutOutSpeed; // Maximum wind speed for system operation in m/s
		Real64 SysEfficiency; // Overall system efficiency including subsystems and losses
		Real64 MaxTipSpeedRatio; // Maximum tip speed ratio
		Real64 MaxPowerCoeff; // Maximum power coefficient
		Real64 LocalAnnualAvgWS; // Annual average wind speed locally measured in m/s
		Real64 AnnualTMYWS; // Annual average wind speed from stat file in m/s
		Real64 HeightForLocalWS; // Height of the local station in m
		Real64 ChordArea; // Chord area of a single blade for VAWTs in m2
		Real64 DragCoeff; // Empirical blade drag coefficient for VAWTs
		Real64 LiftCoeff; // Empirical blade lift coefficient for VAWTs
		Real64 PowerCoeffC1; // Empirical power coefficient 1 for analytical calculation
		Real64 PowerCoeffC2; // Empirical power coefficient 2 for analytical calculation
		Real64 PowerCoeffC3; // Empirical power coefficient 3 for analytical calculation
		Real64 PowerCoeffC4; // Empirical power coefficient 4 for analytical calculation
		Real64 PowerCoeffC5; // Empirical power coefficient 5 for analytical calculation
		Real64 PowerCoeffC6; // Empirical power coefficient 6 for analytical calculation
		Real64 TotPower; // Maximum power produced from the wind in Watts
		Real64 Power; // Actual power wind turbine supplies to the building in Watts
		Real64 TotEnergy; // Maximum energy produced from the wind in Joules
		Real64 Energy; // Actual energy wind turbine supplies to the building in Joules
		Real64 LocalWindSpeed; // Local wind speed estimated at the particular height in m/s
		Real64 LocalAirDensity; // Local air density estimated at the particular height kg/m3
		Real64 PowerCoeff; // Power coefficient determined
		Real64 ChordalVel; // Chordal velocity for VAWTs in m/s
		Real64 NormalVel; // Normal velocity for VAWTs in m/s
		Real64 RelFlowVel; // Relative flow velocity for VAWTs in m/s
		Real64 TipSpeedRatio; // Relative flow velocity for VAWTs in m/s
		Real64 WSFactor; // Relative flow velocity for VAWTs in m/s
		Real64 AngOfAttack; // Angle of attack in degree
		Real64 IntRelFlowVel; // Integral of relative flow velocity
		Real64 TanForce; // Tnagential force
		Real64 NorForce; // Normal force in N.m
		Real64 TotTorque; // Total torque in N.m
		Real64 AzimuthAng; // Azimuth angle between blades

		// Default Constructor
		WindTurbineParams() :
			RotorType( 0 ),
			ControlType( 0 ),
			SchedPtr( 0 ),
			NumOfBlade( 0 ),
			RatedRotorSpeed( 0.0 ),
			RotorDiameter( 0.0 ),
			RotorHeight( 0.0 ),
			RatedPower( 0.0 ),
			RatedWindSpeed( 0.0 ),
			CutInSpeed( 0.0 ),
			CutOutSpeed( 0.0 ),
			SysEfficiency( 0.0 ),
			MaxTipSpeedRatio( 0.0 ),
			MaxPowerCoeff( 0.0 ),
			LocalAnnualAvgWS( 0.0 ),
			AnnualTMYWS( 0.0 ),
			HeightForLocalWS( 0.0 ),
			ChordArea( 0.0 ),
			DragCoeff( 0.0 ),
			LiftCoeff( 0.0 ),
			PowerCoeffC1( 0.0 ),
			PowerCoeffC2( 0.0 ),
			PowerCoeffC3( 0.0 ),
			PowerCoeffC4( 0.0 ),
			PowerCoeffC5( 0.0 ),
			PowerCoeffC6( 0.0 ),
			TotPower( 0.0 ),
			Power( 0.0 ),
			TotEnergy( 0.0 ),
			Energy( 0.0 ),
			LocalWindSpeed( 0.0 ),
			LocalAirDensity( 0.0 ),
			PowerCoeff( 0.0 ),
			ChordalVel( 0.0 ),
			NormalVel( 0.0 ),
			RelFlowVel( 0.0 ),
			TipSpeedRatio( 0.0 ),
			WSFactor( 0.0 ),
			AngOfAttack( 0.0 ),
			IntRelFlowVel( 0.0 ),
			TanForce( 0.0 ),
			NorForce( 0.0 ),
			TotTorque( 0.0 ),
			AzimuthAng( 0.0 )
		{}

		// Member Constructor
		WindTurbineParams(
			std::string const & Name, // The component name
			std::string const & Schedule, // Available schedule
			int const RotorType, // Rotor type (HAWT or VAWT)
			int const ControlType, // Control type
			int const SchedPtr, // Schedule
			int const NumOfBlade, // Blade number
			Real64 const RatedRotorSpeed, // Rated rotor speed in m/s
			Real64 const RotorDiameter, // Diameter of rotor in m
			Real64 const RotorHeight, // Overall height of the rotor in m
			Real64 const RatedPower, // Nominal average power outpout at the rated wind speed in Watts
			Real64 const RatedWindSpeed, // Rated wind speed showing maximum power output in Watts
			Real64 const CutInSpeed, // Minimum wind speed for system operation in m/s
			Real64 const CutOutSpeed, // Maximum wind speed for system operation in m/s
			Real64 const SysEfficiency, // Overall system efficiency including subsystems and losses
			Real64 const MaxTipSpeedRatio, // Maximum tip speed ratio
			Real64 const MaxPowerCoeff, // Maximum power coefficient
			Real64 const LocalAnnualAvgWS, // Annual average wind speed locally measured in m/s
			Real64 const AnnualTMYWS, // Annual average wind speed from stat file in m/s
			Real64 const HeightForLocalWS, // Height of the local station in m
			Real64 const ChordArea, // Chord area of a single blade for VAWTs in m2
			Real64 const DragCoeff, // Empirical blade drag coefficient for VAWTs
			Real64 const LiftCoeff, // Empirical blade lift coefficient for VAWTs
			Real64 const PowerCoeffC1, // Empirical power coefficient 1 for analytical calculation
			Real64 const PowerCoeffC2, // Empirical power coefficient 2 for analytical calculation
			Real64 const PowerCoeffC3, // Empirical power coefficient 3 for analytical calculation
			Real64 const PowerCoeffC4, // Empirical power coefficient 4 for analytical calculation
			Real64 const PowerCoeffC5, // Empirical power coefficient 5 for analytical calculation
			Real64 const PowerCoeffC6, // Empirical power coefficient 6 for analytical calculation
			Real64 const TotPower, // Maximum power produced from the wind in Watts
			Real64 const Power, // Actual power wind turbine supplies to the building in Watts
			Real64 const TotEnergy, // Maximum energy produced from the wind in Joules
			Real64 const Energy, // Actual energy wind turbine supplies to the building in Joules
			Real64 const LocalWindSpeed, // Local wind speed estimated at the particular height in m/s
			Real64 const LocalAirDensity, // Local air density estimated at the particular height kg/m3
			Real64 const PowerCoeff, // Power coefficient determined
			Real64 const ChordalVel, // Chordal velocity for VAWTs in m/s
			Real64 const NormalVel, // Normal velocity for VAWTs in m/s
			Real64 const RelFlowVel, // Relative flow velocity for VAWTs in m/s
			Real64 const TipSpeedRatio, // Relative flow velocity for VAWTs in m/s
			Real64 const WSFactor, // Relative flow velocity for VAWTs in m/s
			Real64 const AngOfAttack, // Angle of attack in degree
			Real64 const IntRelFlowVel, // Integral of relative flow velocity
			Real64 const TanForce, // Tnagential force
			Real64 const NorForce, // Normal force in N.m
			Real64 const TotTorque, // Total torque in N.m
			Real64 const AzimuthAng // Azimuth angle between blades
		) :
			Name( Name ),
			Schedule( Schedule ),
			RotorType( RotorType ),
			ControlType( ControlType ),
			SchedPtr( SchedPtr ),
			NumOfBlade( NumOfBlade ),
			RatedRotorSpeed( RatedRotorSpeed ),
			RotorDiameter( RotorDiameter ),
			RotorHeight( RotorHeight ),
			RatedPower( RatedPower ),
			RatedWindSpeed( RatedWindSpeed ),
			CutInSpeed( CutInSpeed ),
			CutOutSpeed( CutOutSpeed ),
			SysEfficiency( SysEfficiency ),
			MaxTipSpeedRatio( MaxTipSpeedRatio ),
			MaxPowerCoeff( MaxPowerCoeff ),
			LocalAnnualAvgWS( LocalAnnualAvgWS ),
			AnnualTMYWS( AnnualTMYWS ),
			HeightForLocalWS( HeightForLocalWS ),
			ChordArea( ChordArea ),
			DragCoeff( DragCoeff ),
			LiftCoeff( LiftCoeff ),
			PowerCoeffC1( PowerCoeffC1 ),
			PowerCoeffC2( PowerCoeffC2 ),
			PowerCoeffC3( PowerCoeffC3 ),
			PowerCoeffC4( PowerCoeffC4 ),
			PowerCoeffC5( PowerCoeffC5 ),
			PowerCoeffC6( PowerCoeffC6 ),
			TotPower( TotPower ),
			Power( Power ),
			TotEnergy( TotEnergy ),
			Energy( Energy ),
			LocalWindSpeed( LocalWindSpeed ),
			LocalAirDensity( LocalAirDensity ),
			PowerCoeff( PowerCoeff ),
			ChordalVel( ChordalVel ),
			NormalVel( NormalVel ),
			RelFlowVel( RelFlowVel ),
			TipSpeedRatio( TipSpeedRatio ),
			WSFactor( WSFactor ),
			AngOfAttack( AngOfAttack ),
			IntRelFlowVel( IntRelFlowVel ),
			TanForce( TanForce ),
			NorForce( NorForce ),
			TotTorque( TotTorque ),
			AzimuthAng( AzimuthAng )
		{}

	};

	// Object Data
	extern Array1D< WindTurbineParams > WindTurbineSys;

	// Functions

	void
	SimWindTurbine(
		int const GeneratorType, // Type of Generator
		std::string const & GeneratorName, // User specified name of Generator
		int & GeneratorIndex, // Generator index
		bool const RunFlag, // ON or OFF
		Real64 const WTLoad // Electrical load on WT (not used)
	);

	void
	GetWTGeneratorResults(
		int const GeneratorType, // Type of Generator
		int const GeneratorIndex, // Generator number
		Real64 & GeneratorPower, // Electrical power
		Real64 & GeneratorEnergy, // Electrical energy
		Real64 & ThermalPower,
		Real64 & ThermalEnergy
	);

	void
	GetWindTurbineInput();

	void
	InitWindTurbine( int const WindTurbineNum );

	void
	CalcWindTurbine(
		int const WindTurbineNum, // System is on
		bool const RunFlag // System is on
	);

	void
	ReportWindTurbine( int const WindTurbineNum );

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

} // WindTurbine

} // EnergyPlus

#endif
