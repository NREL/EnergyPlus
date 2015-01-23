#ifndef GroundHeatExchangers_hh_INCLUDED
#define GroundHeatExchangers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace GroundHeatExchangers {

	// Using/Aliasing

	// Data
	// DERIVED TYPE DEFINITIONS

	// MODULE PARAMETER DEFINITIONS
	extern Real64 const HrsPerDay; // Number of hours in a day
	extern Real64 const HrsPerMonth; // Number of hours in month
	extern int const MaxTSinHr; // Max number of time step in a hour

	// MODULE VARIABLE DECLARATIONS:
	//extern int NumVerticalGLHEs;
	//extern int N; // COUNTER OF TIME STEP
	//extern Real64 CurrentSimTime; // Current simulation time in hours
	//extern Real64 GLHEOutletTemp; // Outlet temperature of the fluid  [°C]
	//extern Real64 GLHEInletTemp; // Inlet temperature of the fluid   [°C]
	//extern Real64 GLHEMassFlowRate; // Mass flowrate of the fluid       [Kg/s]
	//extern Real64 QGLHE; // The normalized heat transfer rate[W/m]
	////extern Real64 GLHERB; // [K per W/m] Just for Analyis will be removed later
	//extern Real64 GLHEAveFluidTemp; // The average fluid temperature    [°C]
	//extern Real64 GLHEBoreholeTemp; // The average borehole tempreature [°C]
	//extern int LocHourOfDay;
	//extern int LocDayOfSim;
	//extern FArray1D< Real64 > LastQnSubHr; // Previous time step Qn subhourly value
	//extern Real64 MDotActual;

	//extern FArray1D< Real64 > PrevTimeSteps; // This is used to store only the Last Few time step's time
	// to enable the calculation of the subhouly contribution..
	// Recommended size, the product of Minimum subhourly history required and
	// the maximum no of system time steps in an hour

	//extern FArray1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE CondenserTowers

	// Types

	struct GLHEBase
	{
		// Members
		bool Available; // need an array of logicals--load identifiers of available equipment
		bool ON; // simulate the machine at it's operating part load ratio
		std::string Name; // user identifier
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;
		int GLHEInletNodeNum; // Node number on the inlet side of the plant
		int GLHEOutletNodeNum; // Node number on the outlet side of the plant
		Real64 KGround; // Thermal conductivity of the ground		[W/(mK)]
		Real64 CpRhoGround; // Specific heat capacity of ground		[J/Kg/K]
		Real64 KPipe; // Thermal Conductivity of the U tube			[W/(mK)]
		Real64 CpPipe; // Specific heat of the U tube				[J/kg-K]
		Real64 RhoPipe; // Density of the U tube					[kg/m3]
		Real64 PipeOutDia; // Outer diameter of the Pipe			[m]
		Real64 PipeThick; // Thickness of the pipe wall				[m]
		Real64 DesignFlow; // Design volumetric flow rate			[m3/s]
		Real64 DesignMassFlow; // Design mass flow rate				[kg/s]
		Real64 TempGround; // The far feild temperature of the ground   [°C]
		FArray1D< Real64 > QnMonthlyAgg; // Monthly aggregated normalized heat extraction/rejection rate [W/m]
		FArray1D< Real64 > QnHr; // Hourly aggregated normalized heat extraction/rejection rate [W/m]
		FArray1D< Real64 > QnSubHr; // Contains the subhourly heat extraction/rejection rate normalized
		// by the total active length of bore holes  [W/m]
		int PrevHour;
		Real64 gReferenceRatio; // Reference ratio for developing g-functions [-]
		int NPairs; // Number of pairs of Lntts and Gfunc
		FArray1D< Real64 > LNTTS; // natural log of Non Dimensional Time Ln(t/ts)
		FArray1D< Real64 > GFNC; // G-function ( Non Dimensional temperature response factors)
		int AGG; // Minimum Hourly History required
		int SubAGG; // Minimum subhourly History
		FArray1D_int LastHourN; // Stores the Previous hour's N for past hours
		// until the minimum subhourly history
		//loop topology variables
		Real64 GLHEBoreholeTemp; // [°C]
		Real64 GLHEMassFlowRate; // [kg/s]
		Real64 GLHEOutletTemp; // [°C]
		Real64 GLHEInletTemp; // [°C]
		Real64 GLHEAveFluidTemp; // [°C]
		Real64 QGLHE; // [W] heat transfer rate
		bool MyFlag;
		bool MyEnvrnFlag;

		// Default Constructor
		GLHEBase() :
			Available( false ),
			ON( false ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			GLHEInletNodeNum( 0 ),
			GLHEOutletNodeNum( 0 ),
			KGround( 0.0 ),
			CpRhoGround( 0.0 ),
			KPipe( 0.0 ),
			CpPipe( 0.0 ),
			RhoPipe( 0.0 ),
			PipeOutDia( 0.0 ),
			PipeThick( 0.0 ),
			DesignFlow( 0.0 ),
			DesignMassFlow( 0.0 ),
			TempGround( 0.0 ),
			PrevHour( 1 ),
			gReferenceRatio( 0.0 ),
			NPairs( 0 ),
			AGG( 0 ),
			SubAGG( 0 ),
			GLHEBoreholeTemp( 0.0 ),
			GLHEMassFlowRate( 0.0 ),
			GLHEOutletTemp( 0.0 ),
			GLHEInletTemp( 0.0 ),
			GLHEAveFluidTemp( 0.0 ),
			QGLHE( 0.0 ),
			MyFlag( true ),
			MyEnvrnFlag( true )

		{}

		void
		CalcAggregateLoad();

		virtual void
		CalcGroundHeatExchanger()=0;

		virtual void
		UpdateGroundHeatExchanger()=0;

		virtual void
		InitGLHESimVars()=0;

		virtual Real64
		InterpGFunc( Real64 )=0;

	};

	struct GLHEVert:GLHEBase
	{
		// Members
		Real64 MaxGLHEFlowRate; // design nominal capacity of Pump
		int MaxSimYears; // maximum length of simulation (years)
		int NumBoreholes;
		Real64 BoreholeLength;
		Real64 BoreholeRadius;
		Real64 KGrout; // Grout thermal conductivity                [W/(mK)]
		Real64 UtubeDist; // Distance between the legs of the Utube    [m]
		Real64 ResistanceBhole; // The thermal resistance of the borehole, (K per W/m)
		bool RunFlag;
		Real64 LastQnSubHr;


		// Default Constructor
		GLHEVert() :
			MaxGLHEFlowRate( 0.0 ),
			MaxSimYears( 0 ),
			NumBoreholes( 0 ),
			BoreholeLength( 0.0 ),
			BoreholeRadius( 0.0 ),
			KGrout( 0.0 ),
			UtubeDist( 0.0 ),
			ResistanceBhole( 0.0 ),
			RunFlag( false ),
			LastQnSubHr( 0.0 )

		{}

		void
		UpdateGroundHeatExchanger();

		void
		CalcGroundHeatExchanger();

		void
		BoreholeResistance();

		void
		InitGLHESimVars();

		Real64
		InterpGFunc(
		Real64 const LnTTsVal // The value of LN(t/TimeSS) that a g-function
		);

	};

	struct GLHESlinky:GLHEBase
	{
		// Members
		bool VerticalHX;		// Vertical HX Configuration Flag
		bool HorizontalHX;		// Horizontal HX Configuration Flag
		Real64 CoilDiameter;	// Diameter of the slinky coils [m]
		Real64 CoilPitch;		// Center-to-center slinky coil spacing [m]
		Real64 TrenchDepth;		// Trench depth from ground surface to trench bottom [m]
		Real64 TrenchLength;	// Length of single trench [m]
		int NumTrenches;		// Number of parallel trenches [m]
		Real64 TrenchSpacing;	// Spacing between parallel trenches [m]

		// Default Constructor
		GLHESlinky() :
			VerticalHX( false ),
			HorizontalHX( false ),
			CoilDiameter( 0.0 ),
			CoilPitch( 0.0 ),
			TrenchDepth( 0.0 ),
			TrenchLength( 0.0 ),
			NumTrenches( 0 ),
			TrenchSpacing( 0.0 )

		{}

		void
		CalcGroundHeatExchanger();

		void
		CalcGFunctions();

		void
		InitGLHESimVars();

		Real64
		InterpGFunc(
		Real64 const LnTTsVal // The value of LN(t/TimeSS) that a g-function
		);

	};


	// Object Data
	extern FArray1D< GLHEVert > VerticalGLHE; // dimension to number of machines

	// Functions

	void
	SimGroundHeatExchangers(
		std::string const & GLHEType,
		std::string const & GLHEName,
		int & CompIndex,
		bool const RunFlag,
		bool const FirstIteration,
		bool const InitLoopEquip
	);

	//******************************************************************************

	void
	GetGroundHeatExchangerInput();


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

} // GroundHeatExchangers

} // EnergyPlus

#endif
