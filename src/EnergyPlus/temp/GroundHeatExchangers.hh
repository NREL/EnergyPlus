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
	extern int NumVerticalGlhes;
	extern int N; // COUNTER OF TIME STEP
	extern Real64 CurrentSimTime; // Current simulation time in hours
	extern Real64 GlheOutletTemp; // Outlet temperature of the fluid  [°C]
	extern Real64 GlheInletTemp; // Inlet temperature of the fluid   [°C]
	extern Real64 GlheMassFlowRate; // Mass flowrate of the fluid       [Kg/s]
	extern Real64 QGlhe; // The normalised heat transfer rate[W/m]
	extern Real64 GlheRB; // [K per W/m] Just for Analyis will be removed later
	extern Real64 GlheAveFluidTemp; // The average fluid temperature    [°C]
	extern Real64 GlheBoreholeTemp; // The average borehole tempreature [°C]
	extern int LocHourOfDay;
	extern int LocDayOfSim;
	extern FArray1D< Real64 > LastQnSubHr; // Previous time step Qn subhourly value
	extern Real64 MDotActual;

	extern FArray1D< Real64 > PrevTimeSteps; // This is used to store only the Last Few time step's time
	// to enable the calculation of the subhouly contribution..
	// Recommended size, the product of Minimum subhourly history required and
	// the maximum no of system time steps in an hour

	extern FArray1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE CondenserTowers

	// Types

	struct GlheSpecs
	{
		// Members
		std::string Name; // user identifier
		bool Available; // need an array of logicals--load identifiers of available equipment
		bool ON; // simulate the machine at it's operating part load ratio
		Real64 MaxGlheFlowRate; // design nominal capacity of Pump
		int MaxSimYears; // maximum length of simulation (years)
		int GlheInletNodeNum; // Node number on the inlet side of the plant
		int GlheOutletNodeNum; // Node number on the outlet side of the plant
		int NumBoreholes;
		Real64 BoreholeLength;
		Real64 BoreholeRadius;
		Real64 KGround; // Thermal conductivity of the ground        [W/(mK)]
		Real64 CpRhoGround; // Specific heat capacity of ground      [J/Kg/K]
		Real64 TempGround; // The far feild temperature of the ground   [°C]
		Real64 DesignFlow; // Design volumetric flow rate               [m3/S]
		Real64 DesignMassFlow; // Design mass flow rate                    [kg/S]
		Real64 KGrout; // Grout thermal conductivity                [W/(mK)]
		Real64 KPipe; // Thermal Conductivity of the U tube        [W/(mK)]
		Real64 PipeOutDia; // Outer diameter of the Pipe                [m]
		Real64 UtubeDist; // Distance between the legs of the Utube    [m]
		Real64 PipeThick; // Thickness of the pipe wall
		Real64 gReferenceRatio; // Reference ratio for developing g-functions [-]
		int NPairs; // Number of pairs of Lntts and Gfunc
		FArray1D< Real64 > QnMonthlyAgg; // Monthly aggregated normalised heat extraction/rejection rate [W/m]
		FArray1D< Real64 > QnHr; // Hourly aggregated normalised heat extraction/rejection rate [W/m]
		FArray1D< Real64 > QnSubHr; // Contains the subhourly heat extraction/rejection rate normalised
		// by the total active length of bore holes  [W/m]
		FArray1D< Real64 > LNTTS; // natural log of Non Dimensional Time Ln(t/ts)
		FArray1D< Real64 > GFNC; // G-function ( Non Dimensional temperature response factors)
		int AGG; // Minimum Hourly Histroy required
		int SubAGG; // Minimum subhourly History
		FArray1D_int LastHourN; // Stores the Previous hour's N for past hours
		// until the minimum subhourly history
		//loop topology variables
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;

		// Default Constructor
		GlheSpecs() :
			Available( false ),
			ON( false ),
			MaxGlheFlowRate( 0.0 ),
			MaxSimYears( 0 ),
			GlheInletNodeNum( 0 ),
			GlheOutletNodeNum( 0 ),
			NumBoreholes( 0 ),
			BoreholeLength( 0.0 ),
			BoreholeRadius( 0.0 ),
			KGround( 0.0 ),
			CpRhoGround( 0.0 ),
			TempGround( 0.0 ),
			DesignFlow( 0.0 ),
			DesignMassFlow( 0.0 ),
			KGrout( 0.0 ),
			KPipe( 0.0 ),
			PipeOutDia( 0.0 ),
			UtubeDist( 0.0 ),
			PipeThick( 0.0 ),
			gReferenceRatio( 0.0 ),
			NPairs( 0 ),
			AGG( 0 ),
			SubAGG( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 )
		{}

		// Member Constructor
		GlheSpecs(
			std::string const & Name, // user identifier
			bool const Available, // need an array of logicals--load identifiers of available equipment
			bool const ON, // simulate the machine at it's operating part load ratio
			Real64 const MaxGlheFlowRate, // design nominal capacity of Pump
			int const MaxSimYears, // maximum length of simulation (years)
			int const GlheInletNodeNum, // Node number on the inlet side of the plant
			int const GlheOutletNodeNum, // Node number on the outlet side of the plant
			int const NumBoreholes,
			Real64 const BoreholeLength,
			Real64 const BoreholeRadius,
			Real64 const KGround, // Thermal conductivity of the ground        [W/(mK)]
			Real64 const CpRhoGround, // Specific heat capacity of ground      [J/Kg/K]
			Real64 const TempGround, // The far feild temperature of the ground   [°C]
			Real64 const DesignFlow, // Design volumetric flow rate               [m3/S]
			Real64 const DesignMassFlow, // Design mass flow rate                    [kg/S]
			Real64 const KGrout, // Grout thermal conductivity                [W/(mK)]
			Real64 const KPipe, // Thermal Conductivity of the U tube        [W/(mK)]
			Real64 const PipeOutDia, // Outer diameter of the Pipe                [m]
			Real64 const UtubeDist, // Distance between the legs of the Utube    [m]
			Real64 const PipeThick, // Thickness of the pipe wall
			Real64 const gReferenceRatio, // Reference ratio for developing g-functions [-]
			int const NPairs, // Number of pairs of Lntts and Gfunc
			FArray1< Real64 > const & QnMonthlyAgg, // Monthly aggregated normalised heat extraction/rejection rate [W/m]
			FArray1< Real64 > const & QnHr, // Hourly aggregated normalised heat extraction/rejection rate [W/m]
			FArray1< Real64 > const & QnSubHr, // Contains the subhourly heat extraction/rejection rate normalised
			FArray1< Real64 > const & LNTTS, // natural log of Non Dimensional Time Ln(t/ts)
			FArray1< Real64 > const & GFNC, // G-function ( Non Dimensional temperature response factors)
			int const AGG, // Minimum Hourly Histroy required
			int const SubAGG, // Minimum subhourly History
			FArray1_int const & LastHourN, // Stores the Previous hour's N for past hours
			int const LoopNum,
			int const LoopSideNum,
			int const BranchNum,
			int const CompNum
		) :
			Name( Name ),
			Available( Available ),
			ON( ON ),
			MaxGlheFlowRate( MaxGlheFlowRate ),
			MaxSimYears( MaxSimYears ),
			GlheInletNodeNum( GlheInletNodeNum ),
			GlheOutletNodeNum( GlheOutletNodeNum ),
			NumBoreholes( NumBoreholes ),
			BoreholeLength( BoreholeLength ),
			BoreholeRadius( BoreholeRadius ),
			KGround( KGround ),
			CpRhoGround( CpRhoGround ),
			TempGround( TempGround ),
			DesignFlow( DesignFlow ),
			DesignMassFlow( DesignMassFlow ),
			KGrout( KGrout ),
			KPipe( KPipe ),
			PipeOutDia( PipeOutDia ),
			UtubeDist( UtubeDist ),
			PipeThick( PipeThick ),
			gReferenceRatio( gReferenceRatio ),
			NPairs( NPairs ),
			QnMonthlyAgg( QnMonthlyAgg ),
			QnHr( QnHr ),
			QnSubHr( QnSubHr ),
			LNTTS( LNTTS ),
			GFNC( GFNC ),
			AGG( AGG ),
			SubAGG( SubAGG ),
			LastHourN( LastHourN ),
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum )
		{}

	};

	struct ReportVars
	{
		// Members
		Real64 GlheBoreholeTemp; // [°C]
		Real64 GlheMassFlowRate; // [kg/s]
		Real64 GlheOutletTemp; // [°C]
		Real64 GlheInletTemp; // [°C]
		Real64 GlheAveFluidTemp; // [°C]
		Real64 QGlhe; // [W] heat transfer rate

		// Default Constructor
		ReportVars() :
			GlheBoreholeTemp( 0.0 ),
			GlheMassFlowRate( 0.0 ),
			GlheOutletTemp( 0.0 ),
			GlheInletTemp( 0.0 ),
			GlheAveFluidTemp( 0.0 ),
			QGlhe( 0.0 )
		{}

		// Member Constructor
		ReportVars(
			Real64 const GlheBoreholeTemp, // [°C]
			Real64 const GlheMassFlowRate, // [kg/s]
			Real64 const GlheOutletTemp, // [°C]
			Real64 const GlheInletTemp, // [°C]
			Real64 const GlheAveFluidTemp, // [°C]
			Real64 const QGlhe // [W] heat transfer rate
		) :
			GlheBoreholeTemp( GlheBoreholeTemp ),
			GlheMassFlowRate( GlheMassFlowRate ),
			GlheOutletTemp( GlheOutletTemp ),
			GlheInletTemp( GlheInletTemp ),
			GlheAveFluidTemp( GlheAveFluidTemp ),
			QGlhe( QGlhe )
		{}

	};

	// Object Data
	extern FArray1D< GlheSpecs > VerticalGlhe; // dimension to number of machines
	extern FArray1D< ReportVars > VerticalGlheReport;

	// Functions

	void
	SimGroundHeatExchangers(
		std::string const & GlheType,
		std::string const & GlheName,
		int & CompIndex,
		bool const RunFlag,
		bool const FirstIteration,
		bool const InitLoopEquip
	);

	//******************************************************************************

	void
	CalcVerticalGroundHeatExchanger( int & GlheNum );

	//******************************************************************************

	void
	CalcAggregateLoad( int const GlheNum );

	//******************************************************************************

	void
	GetGroundheatExchangerInput();

	//******************************************************************************

	void
	BoreholeResistance(
		int const GlheNum,
		Real64 & ResistanceBhole
	);

	//******************************************************************************

	void
	INTERP(
		int const GlheNum, // Ground loop heat exchanger ID number
		Real64 const LnTTsVal, // The value of LN(t/TimeSS) that a g-function
		Real64 & GfuncVal // The value of the g-function at LnTTsVal; found by
	);

	//******************************************************************************

	void
	InitBoreholeHXSimVars(
		int const GlheNum,
		bool const RunFlag
	);

	//******************************************************************************

	void
	UpdateVerticalGroundHeatExchanger(
		bool const RunFlag,
		int const Num
	);

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
