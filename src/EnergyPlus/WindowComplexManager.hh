#ifndef WindowComplexManager_hh_INCLUDED
#define WindowComplexManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataBSDFWindow.hh>
#include <DataVectorTypes.hh>

namespace EnergyPlus {

namespace WindowComplexManager {

	// Using/Aliasing
	using DataBSDFWindow::BSDFDaylghtPosition;
	using DataBSDFWindow::BSDFGeomDescr;
	using DataBSDFWindow::BSDFStateDescr;
	using DataBSDFWindow::BSDFWindowGeomDescr;
	using DataBSDFWindow::BSDFWindowInputStruct;
	using DataBSDFWindow::BasisElemDescr;
	using DataBSDFWindow::BasisStruct;
	using DataVectorTypes::Vector;

	// Data
	// MODULE PARAMETER DEFINITIONS:

	extern Real64 const sigma; // Stefan-Boltzmann constant
	extern Real64 const PressureDefault;

	extern int const Calculate_Geometry;
	extern int const Copy_Geometry;

	extern int const TmpLen; // Length increment of temporary arrays

	extern int const Front_Incident; // Ray identification types
	extern int const Front_Transmitted;
	extern int const Front_Reflected;
	extern int const Back_Incident;
	extern int const Back_Transmitted;
	extern int const Back_Reflected;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	extern int NumComplexWind; // Total number of complex windows
	//Debug
	extern Array2D_int DbgIBm;
	extern Array2D< Real64 > DbgTheta;
	extern Array2D< Real64 > DbgPhi;
	extern Real64 DdbgTheta;
	extern Real64 DdbgPhi;
	//EndDebug

	// SUBROUTINE SPECIFICATIONS FOR MODULE WindowComplexManager:

	// Types

	struct WindowIndex
	{
		// Members
		int NumStates; // No States for this window
		int SurfNo; // Surface number of window
		//Real64 Azimuth; // Window surface azimuth
		//Real64 Tilt; // Window surface tilt

		// Default Constructor
		WindowIndex() :
			NumStates( 0 )
		{}

		// Member Constructor
		WindowIndex(
			int const NumStates, // No States for this window
			int const SurfNo // Surface number of window
		) :
			NumStates( NumStates ),
			SurfNo( SurfNo )
		{}

	};

	struct WindowStateIndex
	{
		// Members
		int InitInc; // Flag indicating initialization needed on Incoming basis
		int IncBasisIndx; // Index of basis list entry for Incoming basis
		int CopyIncState; // Pointer to state from which geometry can be copied (Incident)
		int InitTrn; // Flag indicating initialization needed on Outgoing basis
		int TrnBasisIndx; // Index of basis list entry for Outgoing basis
		int CopyTrnState; // Pointer to state from which geometry can be copied (Outgoing)
		int Konst; // Index of state descript in Construct array
		//INTEGER  ::  ThermConst  !Index of state thermal description in Construct array

		// Default Constructor
		WindowStateIndex()
		{}

		// Member Constructor
		WindowStateIndex(
			int const InitInc, // Flag indicating initialization needed on Incoming basis
			int const IncBasisIndx, // Index of basis list entry for Incoming basis
			int const CopyIncState, // Pointer to state from which geometry can be copied (Incident)
			int const InitTrn, // Flag indicating initialization needed on Outgoing basis
			int const TrnBasisIndx, // Index of basis list entry for Outgoing basis
			int const CopyTrnState, // Pointer to state from which geometry can be copied (Outgoing)
			int const Konst // Index of state descript in Construct array
		) :
			InitInc( InitInc ),
			IncBasisIndx( IncBasisIndx ),
			CopyIncState( CopyIncState ),
			InitTrn( InitTrn ),
			TrnBasisIndx( TrnBasisIndx ),
			CopyTrnState( CopyTrnState ),
			Konst( Konst )
		{}

	};

	// Object Data
	extern Array1D< BasisStruct > BasisList;
	extern Array1D< WindowIndex > WindowList;
	extern Array2D< WindowStateIndex > WindowStateList;

	// Functions

	void
	InitBSDFWindows();

	void
	AllocateCFSStateHourlyData(
		int const iSurf, // Surface number
		int const iState // Complex fenestration state number
	);

	void
	ExpandComplexState(
		int const iSurf, // Surface number
		int const iConst // Construction number
	);

	void
	CheckCFSStates( int const iSurf ); // Surface number

	void
	InitComplexWindows();

	void
	UpdateComplexWindows();

	void
	CFSShadeAndBeamInitialization(
		int const iSurf, // Window surface number
		int const iState, // Window state number
		BSDFWindowGeomDescr & Window, // Window Geometry
		BSDFGeomDescr & Geom, // State Geometry
		BSDFStateDescr & State // State Description
	);

	void
	CalculateWindowBeamProperties(
		int const ISurf, // Window surface number
		int const IState, // Window state number
		BSDFWindowGeomDescr & Window, // Window Geometry
		BSDFGeomDescr & Geom, // State Geometry
		BSDFStateDescr & State, // State Description
		int const Hour, // Hour number
		int const TS // Timestep number
	);

	void
	CalcStaticProperties();

	void
	CalculateBasisLength(
		BSDFWindowInputStruct const & Input, // BSDF data input struct for this construction
		int const IConst, // Construction number of input
		int & NBasis // Calculated Basis length
	);

	void
	DetermineMaxBackSurfaces();

	void
	ConstructBasis(
		int const IConst, // Index for accessing Construct array
		BasisStruct & Basis
	);

	void
	FillBasisElement(
		Real64 const Theta, // Central polar angle of element
		Real64 const Phi, // Central azimuthal angle of element
		int const Elem, // Index number of element in basis
		BasisElemDescr & BasisElem,
		Real64 const LowerTheta, // Lower edge of element (polar angle)
		Real64 const UpperTheta, // Upper edge of element (polar angle)
		Real64 const DPhi, // Width of element (azimuthal angle)
		int const InputType // Basis type
	);

	void
	SetupComplexWindowStateGeometry(
		int const ISurf, // Surface number of the complex fenestration
		int const IState, // State number of the complex fenestration state
		int const IConst, // Pointer to construction for this state
		BSDFWindowGeomDescr & Window, // Window Geometry
		BSDFGeomDescr & Geom, // State Geometry
		BSDFStateDescr & State // State Description
	);

	void
	CalcWindowStaticProperties(
		int const ISurf, // Surface number of the complex fenestration
		int const IState, // State number of the complex fenestration state
		BSDFWindowGeomDescr & Window, // Window Geometry
		BSDFGeomDescr & Geom, // State Geometry
		BSDFStateDescr & State // State Description
	);

	Real64
	SkyWeight( Vector const & DirVec ); // Direction of the element to be weighted

	Real64
	SkyGndWeight( Vector const & PosVec ); // x,y,z(=0) of ground intersection pt

	BSDFDaylghtPosition
	DaylghtAltAndAzimuth( Vector const & UnitVect ); // vector which needs to be converted

	Vector
	WorldVectFromW6(
		Real64 const Theta, // Polar angle in W6 Coords
		Real64 const Phi, // Azimuthal angle in W6 Coords
		int const RadType, // Type of radiation: Front_Incident, etc.
		Real64 const Gamma, // Surface tilt angle, radians, world coordinate system
		Real64 const Alpha // Surface azimuth, radians, world coordinate system
	);

	int
	FindInBasis(
		Vector const & RayToFind, // Ray vector direction in world CS
		int const RadType, // Type of radiation: Front_Incident, etc.
		int const ISurf, // Window Surface number
		int const IState, // Complex Fenestration state number
		BasisStruct const & Basis, // Complex Fenestration basis root
		Real64 & Theta, // Theta value for ray
		Real64 & Phi // Phi value for ray
	);

	void
	W6CoordsFromWorldVect(
		Vector const & RayVect, // Ray vector direction in world CS
		int const RadType, // Type of radiation: Front_Incident, etc.
		Real64 const Gamma, // Surface tilt angle, world coordinate system
		Real64 const Alpha, // Surface azimuth, world coordinate system
		Real64 & Theta, // Polar angle in W6 Coords
		Real64 & Phi // Azimuthal angle in W6 Coords
	);

	void
	CalcComplexWindowThermal(
		int const SurfNum, // Surface number
		int & ConstrNum, // Construction number
		Real64 const HextConvCoeff, // Outside air film conductance coefficient
		Real64 & SurfInsideTemp, // Inside window surface temperature
		Real64 & SurfOutsideTemp, // Outside surface temperature (C)
		Real64 & SurfOutsideEmiss,
		int const CalcCondition // Calucation condition (summer, winter or no condition)
	);

	// This function check if gas with molecular weight has already been feed into coefficients and
	// feed arrays

	void
	CheckGasCoefs(
		Real64 const currentWeight,
		int & indexNumber,
		Array1A< Real64 > wght,
		bool & feedData
	);

	int
	SearchAscTable(
		Real64 const y, // Value to be found in the table
		int const n, // Number of values in the table
		Array1S< Real64 > const ytab // Table of values, monotonic, ascending order
	);

	//=================================================================================================

	void
	CrossProduct(
		Array1A< Real64 > A, // Vector components: C = A X B
		Array1A< Real64 > B,
		Array1A< Real64 > C
	);

	void
	PierceSurfaceVector(
		int const ISurf, // Surface index
		Vector const & Orig, // Point from which ray originates
		Vector const & Dir, // Unit vector along in direction of ray whose
		int & IPIERC, // =1 if line through point R1 in direction of unit vector
		Vector & HitPt // Point that ray along RN intersects plane of surface
	);

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

} // WindowComplexManager

} // EnergyPlus

#endif
