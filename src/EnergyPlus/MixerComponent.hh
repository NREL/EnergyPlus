#ifndef MixerComponent_hh_INCLUDED
#define MixerComponent_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace MixerComponent {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern Real64 const MassFlowTol;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int NumMixers; // The Number of Mixers found in the Input
	extern int LoopInletNode;
	extern int LoopOutletNode;
	extern bool GetInputFlag;
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE Mixers

	// Types

	struct MixerConditions
	{
		// Members
		std::string MixerName; // Name of the Mixer
		Real64 OutletTemp;
		Real64 OutletHumRat;
		Real64 OutletEnthalpy;
		Real64 OutletPressure;
		int OutletNode;
		Real64 OutletMassFlowRate; // MassFlow through the Mixer being Simulated [kg/Sec]
		Real64 OutletMassFlowRateMaxAvail; // [kg/Sec]
		Real64 OutletMassFlowRateMinAvail; // [kg/Sec]
		bool InitFlag;
		int NumInletNodes;
		Array1D_int InletNode;
		Array1D< Real64 > InletMassFlowRate;
		Array1D< Real64 > InletMassFlowRateMaxAvail;
		Array1D< Real64 > InletMassFlowRateMinAvail;
		Array1D< Real64 > InletTemp;
		Array1D< Real64 > InletHumRat;
		Array1D< Real64 > InletEnthalpy;
		Array1D< Real64 > InletPressure;

		// Default Constructor
		MixerConditions() :
			OutletTemp( 0.0 ),
			OutletHumRat( 0.0 ),
			OutletEnthalpy( 0.0 ),
			OutletPressure( 0.0 ),
			OutletNode( 0 ),
			OutletMassFlowRate( 0.0 ),
			OutletMassFlowRateMaxAvail( 0.0 ),
			OutletMassFlowRateMinAvail( 0.0 ),
			InitFlag( false ),
			NumInletNodes( 0 )
		{}

		// Member Constructor
		MixerConditions(
			std::string const & MixerName, // Name of the Mixer
			Real64 const OutletTemp,
			Real64 const OutletHumRat,
			Real64 const OutletEnthalpy,
			Real64 const OutletPressure,
			int const OutletNode,
			Real64 const OutletMassFlowRate, // MassFlow through the Mixer being Simulated [kg/Sec]
			Real64 const OutletMassFlowRateMaxAvail, // [kg/Sec]
			Real64 const OutletMassFlowRateMinAvail, // [kg/Sec]
			bool const InitFlag,
			int const NumInletNodes,
			Array1_int const & InletNode,
			Array1< Real64 > const & InletMassFlowRate,
			Array1< Real64 > const & InletMassFlowRateMaxAvail,
			Array1< Real64 > const & InletMassFlowRateMinAvail,
			Array1< Real64 > const & InletTemp,
			Array1< Real64 > const & InletHumRat,
			Array1< Real64 > const & InletEnthalpy,
			Array1< Real64 > const & InletPressure
		) :
			MixerName( MixerName ),
			OutletTemp( OutletTemp ),
			OutletHumRat( OutletHumRat ),
			OutletEnthalpy( OutletEnthalpy ),
			OutletPressure( OutletPressure ),
			OutletNode( OutletNode ),
			OutletMassFlowRate( OutletMassFlowRate ),
			OutletMassFlowRateMaxAvail( OutletMassFlowRateMaxAvail ),
			OutletMassFlowRateMinAvail( OutletMassFlowRateMinAvail ),
			InitFlag( InitFlag ),
			NumInletNodes( NumInletNodes ),
			InletNode( InletNode ),
			InletMassFlowRate( InletMassFlowRate ),
			InletMassFlowRateMaxAvail( InletMassFlowRateMaxAvail ),
			InletMassFlowRateMinAvail( InletMassFlowRateMinAvail ),
			InletTemp( InletTemp ),
			InletHumRat( InletHumRat ),
			InletEnthalpy( InletEnthalpy ),
			InletPressure( InletPressure )
		{}

	};

	// Object Data
	extern Array1D< MixerConditions > MixerCond;

	// Functions

	void
	SimAirMixer(
		std::string const & CompName,
		int & CompIndex
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetMixerInput();

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitAirMixer( int const MixerNum );

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcAirMixer( int & MixerNum );

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the Mixer Module
	// *****************************************************************************

	void
	UpdateAirMixer( int const MixerNum );

	//        End of Update subroutines for the Mixer Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Mixer Module
	// *****************************************************************************

	void
	ReportMixer( int const MixerNum );

	//        End of Reporting subroutines for the Mixer Module
	// *****************************************************************************

	// Beginning of Utility subroutines for the Mixer Component
	// *****************************************************************************
	void
	GetZoneMixerIndex(
		std::string const & MixerName,
		int & MixerIndex,
		bool & ErrorsFound,
		std::string const & ThisObjectType = std::string()
	);

	// End of Utility subroutines for the Mixer Component
	// *****************************************************************************

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

} // MixerComponent

} // EnergyPlus

#endif
