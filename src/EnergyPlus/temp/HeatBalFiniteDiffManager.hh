#ifndef HeatBalFiniteDiffManager_hh_INCLUDED
#define HeatBalFiniteDiffManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray2D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HeatBalFiniteDiffManager {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern Real64 const Lambda;
	extern Real64 const smalldiff; // Used in places where "equality" tests should not be used.

	extern int const CrankNicholsonSecondOrder; // original CondFD scheme.  semi implicit, second order in time
	extern int const FullyImplicitFirstOrder; // fully implicit scheme, first order in time.
	extern FArray1D_string const cCondFDSchemeType;

	extern Real64 const TempInitValue; // Initialization value for Temperature
	extern Real64 const RhovInitValue; // Initialization value for Rhov
	extern Real64 const EnthInitValue; // Initialization value for Enthalpy

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	//REAL(r64) :: TFDout   =0.0d0
	//REAL(r64) :: TFDin    =0.0d0
	//REAL(r64) :: rhovFDout=0.0d0
	//REAL(r64) :: rhovFDin =0.0d0
	//REAL(r64) :: TDryout  =0.0d0
	//REAL(r64) :: Tdryin   =0.0d0
	//REAL(r64) :: RHOut    =0.0d0
	//REAL(r64) :: RHIn     =0.0d0
	extern FArray1D< Real64 > SigmaR; // Total Resistance of construction layers
	extern FArray1D< Real64 > SigmaC; // Total Capacitance of construction layers

	//REAL(r64), ALLOCATABLE, DIMENSION(:)   :: WSurfIn         !Humidity Ratio of the inside surface for reporting
	//REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QMassInFlux     !MassFlux on Surface for reporting
	//REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QMassOutFlux    !MassFlux on Surface for reporting
	extern FArray1D< Real64 > QHeatInFlux; // HeatFlux on Surface for reporting
	extern FArray1D< Real64 > QHeatOutFlux; // HeatFlux on Surface for reporting
	//REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QFluxZoneToInSurf !sum of Heat flows at the surface to air interface,
	//                                 ! zone-side boundary conditions W/m2 before CR 8280 was not reported, but was calculated.
	//REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QFluxOutsideToOutSurf !sum of Heat flows at the surface to air interface, Out-side boundary conditions W/m2
	//                                                           ! before CR 8280 was
	//REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QFluxInArrivSurfCond !conduction between surface node and first node into the surface (sensible)
	//                                                           ! before CR 8280 was -- Qdryin    !HeatFlux on Surface for reporting for Sensible only
	//REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QFluxOutArrivSurfCond  !HeatFlux on Surface for reporting for Sensible only
	//                                                                 ! before CR 8280 -- Qdryout         !HeatFlux on Surface for reporting for Sensible only

	extern int CondFDSchemeType; // solution scheme for CondFD - default
	extern Real64 SpaceDescritConstant; // spatial descritization constant,
	extern Real64 MinTempLimit; // lower limit check, degree C
	extern Real64 MaxTempLimit; // upper limit check, degree C
	//feb2012 INTEGER   :: MaxGSiter = 200  ! maximum number of Gauss Seidel iterations
	extern int MaxGSiter; // maximum number of Gauss Seidel iterations
	extern Real64 fracTimeStepZone_Hour;
	extern bool GetHBFiniteDiffInputFlag;
	extern int WarmupSurfTemp;
	// Subroutine Specifications for the Heat Balance Module
	// Driver Routines

	// Initialization routines for module

	// Algorithms for the module

	// Reporting routines for module

	// Update Data Routine

	// Types

	struct ConstructionDataFD
	{
		// Members
		FArray1D_string Name; // Name of construction
		FArray1D< Real64 > DelX;
		FArray1D< Real64 > TempStability;
		FArray1D< Real64 > MoistStability;
		FArray1D_int NodeNumPoint;
		//  INTEGER, ALLOCATABLE, DIMENSION(:) :: InterfaceNodeNums   ! Layer interfaces occur at these nodes
		FArray1D< Real64 > Thickness;
		FArray1D< Real64 > NodeXlocation; // sized to TotNode, contains X distance in m from outside face
		int TotNodes;
		int DeltaTime;

		// Default Constructor
		ConstructionDataFD() :
			TotNodes( 0 ),
			DeltaTime( 0 )
		{}

		// Member Constructor
		ConstructionDataFD(
			FArray1_string const & Name, // Name of construction
			FArray1< Real64 > const & DelX,
			FArray1< Real64 > const & TempStability,
			FArray1< Real64 > const & MoistStability,
			FArray1_int const & NodeNumPoint,
			FArray1< Real64 > const & Thickness,
			FArray1< Real64 > const & NodeXlocation, // sized to TotNode, contains X distance in m from outside face
			int const TotNodes,
			int const DeltaTime
		) :
			Name( Name ),
			DelX( DelX ),
			TempStability( TempStability ),
			MoistStability( MoistStability ),
			NodeNumPoint( NodeNumPoint ),
			Thickness( Thickness ),
			NodeXlocation( NodeXlocation ),
			TotNodes( TotNodes ),
			DeltaTime( DeltaTime )
		{}

	};

	struct SurfaceDataFD
	{
		// Members
		FArray1D< Real64 > T;
		FArray1D< Real64 > TOld;
		FArray1D< Real64 > TT;
		FArray1D< Real64 > Rhov;
		FArray1D< Real64 > RhovOld;
		FArray1D< Real64 > RhoT;
		FArray1D< Real64 > TD;
		FArray1D< Real64 > TDT;
		FArray1D< Real64 > TDTLast;
		FArray1D< Real64 > TDOld;
		FArray1D< Real64 > TDreport;
		FArray1D< Real64 > RH;
		FArray1D< Real64 > RHreport;
		FArray1D< Real64 > EnthOld; // Current node enthalpy
		FArray1D< Real64 > EnthNew; // Node enthalpy at new time
		FArray1D< Real64 > EnthLast;
		int GSloopCounter; // count of inner loop iterations
		int GSloopErrorCount; // recurring error counter
		Real64 MaxNodeDelTemp; // largest change in node temps after calc

		// Default Constructor
		SurfaceDataFD() :
			GSloopCounter( 0 ),
			GSloopErrorCount( 0 ),
			MaxNodeDelTemp( 0.0 )
		{}

		// Member Constructor
		SurfaceDataFD(
			FArray1< Real64 > const & T,
			FArray1< Real64 > const & TOld,
			FArray1< Real64 > const & TT,
			FArray1< Real64 > const & Rhov,
			FArray1< Real64 > const & RhovOld,
			FArray1< Real64 > const & RhoT,
			FArray1< Real64 > const & TD,
			FArray1< Real64 > const & TDT,
			FArray1< Real64 > const & TDTLast,
			FArray1< Real64 > const & TDOld,
			FArray1< Real64 > const & TDreport,
			FArray1< Real64 > const & RH,
			FArray1< Real64 > const & RHreport,
			FArray1< Real64 > const & EnthOld, // Current node enthalpy
			FArray1< Real64 > const & EnthNew, // Node enthalpy at new time
			FArray1< Real64 > const & EnthLast,
			int const GSloopCounter, // count of inner loop iterations
			int const GSloopErrorCount, // recurring error counter
			Real64 const MaxNodeDelTemp // largest change in node temps after calc
		) :
			T( T ),
			TOld( TOld ),
			TT( TT ),
			Rhov( Rhov ),
			RhovOld( RhovOld ),
			RhoT( RhoT ),
			TD( TD ),
			TDT( TDT ),
			TDTLast( TDTLast ),
			TDOld( TDOld ),
			TDreport( TDreport ),
			RH( RH ),
			RHreport( RHreport ),
			EnthOld( EnthOld ),
			EnthNew( EnthNew ),
			EnthLast( EnthLast ),
			GSloopCounter( GSloopCounter ),
			GSloopErrorCount( GSloopErrorCount ),
			MaxNodeDelTemp( MaxNodeDelTemp )
		{}

		inline
		void
		UpdateMoistureBalance()
		{
			// Based on UpdateMoistureBalanceFD by Richard Liesen
			// Brought into class for performance
			TOld = T;
			RhovOld = Rhov;
			TDOld = TDreport;
		}

	};

	struct MaterialDataFD
	{
		// Members
		Real64 tk1; // Temperature coefficient for thermal conductivity
		int numTempEnth; // number of Temperature/Enthalpy pairs
		int numTempCond; // number of Temperature/Conductivity pairs
		FArray2D< Real64 > TempEnth; // Temperature enthalpy Function Pairs,
		//  TempEnth(1,1)= first Temp, TempEnth(1,2) = First Enthalpy,
		//  TempEnth(2,1) = secomd Temp, etc.
		FArray2D< Real64 > TempCond; // Temperature thermal conductivity Function Pairs,
		//  TempCond(1,1)= first Temp, Tempcond(1,2) = First conductivity,
		//  TempEnth(2,1) = secomd Temp, etc.

		// Default Constructor
		MaterialDataFD() :
			tk1( 0.0 ),
			numTempEnth( 0 ),
			numTempCond( 0 )
		{}

		// Member Constructor
		MaterialDataFD(
			Real64 const tk1, // Temperature coefficient for thermal conductivity
			int const numTempEnth, // number of Temperature/Enthalpy pairs
			int const numTempCond, // number of Temperature/Conductivity pairs
			FArray2< Real64 > const & TempEnth, // Temperature enthalpy Function Pairs,
			FArray2< Real64 > const & TempCond // Temperature thermal conductivity Function Pairs,
		) :
			tk1( tk1 ),
			numTempEnth( numTempEnth ),
			numTempCond( numTempCond ),
			TempEnth( TempEnth ),
			TempCond( TempCond )
		{}

	};

	// Object Data
	extern FArray1D< ConstructionDataFD > ConstructFD;
	extern FArray1D< SurfaceDataFD > SurfaceFD;
	extern FArray1D< MaterialDataFD > MaterialFD;

	// Functions

	void
	ManageHeatBalFiniteDiff(
		int const SurfNum,
		Real64 & TempSurfInTmp, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Real64 & TempSurfOutTmp // Outside Surface Temperature of each Heat Transfer Surface
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetCondFDInput();

	void
	InitHeatBalFiniteDiff();

	void
	InitialInitHeatBalFiniteDiff();

	void
	CalcHeatBalFiniteDiff(
		int const Surf,
		Real64 & TempSurfInTmp, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		Real64 & TempSurfOutTmp // Outside Surface Temperature of each Heat Transfer Surface
	);

	// Beginning of Reporting subroutines
	// *****************************************************************************

	void
	ReportFiniteDiffInits();

	// Utility Interpolation Function for the Module
	//******************************************************************************

	Real64
	terpld(
		FArray2< Real64 > const & a,
		Real64 const x1,
		int const nind,
		int const ndep
	);

	// Equation Types of the Module
	//******************************************************************************

	void
	ExteriorBCEqns(
		int const Delt, // Time Increment
		int const i, // Node Index
		int const Lay, // Layer Number for Construction
		int const Surf, // Surface number
		FArray1< Real64 > const & T, // Old node Temperature in MFD finite difference solution
		FArray1< Real64 > & TT, // New node Temperature in MFD finite difference solution.
		FArray1< Real64 > const & Rhov, // MFD Nodal Vapor Density[kg/m3] and is the old or last time step result.
		FArray1< Real64 > & RhoT, // MFD vapor density for the new time step.
		FArray1< Real64 > & RH, // Nodal relative humidity
		FArray1< Real64 > const & TD, // The old dry Temperature at each node for the CondFD algorithm..
		FArray1< Real64 > & TDT, // The current or new Temperature at each node location for the CondFD solution..
		FArray1< Real64 > & EnthOld, // Old Nodal enthalpy
		FArray1< Real64 > & EnthNew, // New Nodal enthalpy
		int const TotNodes, // Total nodes in layer
		Real64 const HMovInsul // Conductance of movable(transparent) insulation.
	);

	void
	InteriorNodeEqns(
		int const Delt, // Time Increment
		int const i, // Node Index
		int const Lay, // Layer Number for Construction
		int const Surf, // Surface number
		FArray1< Real64 > const & T, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > & TT, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > const & Rhov, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > & RhoT, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > & RH, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > const & TD, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > & TDT, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > & EnthOld, // Old Nodal enthalpy
		FArray1< Real64 > & EnthNew // New Nodal enthalpy
	);

	void
	IntInterfaceNodeEqns(
		int const Delt, // Time Increment
		int const i, // Node Index
		int const Lay, // Layer Number for Construction
		int const Surf, // Surface number
		FArray1< Real64 > const & T, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > & TT, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > const & Rhov, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > & RhoT, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > & RH, // RELATIVE HUMIDITY.
		FArray1< Real64 > const & TD, // OLD NODE TEMPERATURES OF EACH HEAT TRANSFER SURF IN CONDFD.
		FArray1< Real64 > & TDT, // NEW NODE TEMPERATURES OF EACH HEAT TRANSFER SURF IN CONDFD.
		FArray1< Real64 > const & EnthOld, // Old Nodal enthalpy
		FArray1< Real64 > & EnthNew, // New Nodal enthalpy
		int const GSiter // Iteration number of Gauss Seidell iteration
	);

	void
	InteriorBCEqns(
		int const Delt, // Time Increment
		int const i, // Node Index
		int const Lay, // Layer Number for Construction
		int const Surf, // Surface number
		FArray1< Real64 > const & T, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF (Old).
		FArray1< Real64 > & TT, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF (New).
		FArray1< Real64 > const & Rhov, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > & RhoT, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > & RH, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > const & TD, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > & TDT, // INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
		FArray1< Real64 > & EnthOld, // Old Nodal enthalpy
		FArray1< Real64 > & EnthNew, // New Nodal enthalpy
		FArray1< Real64 > & TDreport // Temperature value from previous HeatSurfaceHeatManager titeration's value
	);

	void
	CheckFDSurfaceTempLimits(
		int const SurfNum, // surface number
		Real64 const CheckTemperature // calculated temperature, not reset
	);

	// *****************************************************************************

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
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

} // HeatBalFiniteDiffManager

} // EnergyPlus

#endif
