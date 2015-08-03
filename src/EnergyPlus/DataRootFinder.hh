#ifndef DataRootFinder_hh_INCLUDED
#define DataRootFinder_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataRootFinder {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS
	extern int const iSlopeNone; // Undefined slope specification
	extern int const iSlopeIncreasing; // For overall increasing function F(X) between min and max points
	extern int const iSlopeDecreasing; // For overall decreasing function F(X) between min and max points

	// Error because the overall slope appears to be flat between the min and max points,
	// implying that the function might be singular over the interval:
	// F(XMin) == F(XMax)
	extern int const iStatusErrorSingular;
	// Error because the overall slope assumption is not observed at the min and max points:
	// - for an increasing function F(X), we expect F(XMin) < F(XMax)  otherwise error
	// - for a decreasing function F(X),  we expect F(XMin) > F(XMax)  otherwise error
	// Note that this error status does not detect strict monotonicity at points
	// between the min and max points.
	extern int const iStatusErrorSlope;
	// Error because the current candidate X does not lie within the current lower an upper points:
	// X < XLower or X > XUpper
	extern int const iStatusErrorBracket;
	// Error because the current candidate X does not lie within the min and max points:
	// X < XMin or X > XMax
	extern int const iStatusErrorRange;

	extern int const iStatusNone; // Indeterminate error state (not converged), also default state
	extern int const iStatusOK; // Unconstrained convergence achieved with root solution so that:
	// XMin < XRoot < XMax
	extern int const iStatusOKMin; // Constrained convergence achieved with solution XRoot==XMin
	extern int const iStatusOKMax; // Constrained convergence achieved with solution XRoot==XMax
	extern int const iStatusOKRoundOff; // Reached requested tolerance in X variables although Y=F(X) does not
	// satisfy unconstrained convergence check

	extern int const iStatusWarningNonMonotonic; // Error because F(X) is not strictly monotonic between the
	// lower and upper points
	extern int const iStatusWarningSingular; // Error because F(X) == YLower or F(X) == YUpper

	extern int const iMethodNone; // No solution method (used internally only when root finder is reset)
	extern int const iMethodBracket; // Bracketting mode (used internally only to bracket root)
	extern int const iMethodBisection; // Step performed using bisection method (aka interval halving)
	extern int const iMethodFalsePosition; // Step performed using false position method (aka regula falsi)
	extern int const iMethodSecant; // Step performed using secant method
	extern int const iMethodBrent; // Step performed using Brent's method
	// Names for each solution method type
	extern Array1D_string const SolutionMethodTypes;

	// DERIVED TYPE DEFINITIONS
	// Type declaration for the numerical controls.

	// Type declaration for iterate tracking.

	// Type declaration for the root finder solution technique.

	// Types

	struct ControlsType
	{
		// Members
		int SlopeType; // Set to any of the iSlope<...> codes
		int MethodType; // Desired solution method.
		// Set to any of the iMethod<...> codes except for iMethodNone and iMethodBracket
		Real64 TolX; // Relative tolerance for variable X
		Real64 ATolX; // Absolute tolerance for variable X
		Real64 ATolY; // Absolute tolerance for variable Y

		// Default Constructor
		ControlsType() :
			SlopeType( iSlopeNone ),
			MethodType( iMethodNone ),
			TolX( 1.0e-3 ),
			ATolX( 1.0e-3 ),
			ATolY( 1.0e-3 )
		{}

		// Member Constructor
		ControlsType(
			int const SlopeType, // Set to any of the iSlope<...> codes
			int const MethodType, // Desired solution method.
			Real64 const TolX, // Relative tolerance for variable X
			Real64 const ATolX, // Absolute tolerance for variable X
			Real64 const ATolY // Absolute tolerance for variable Y
		) :
			SlopeType( SlopeType ),
			MethodType( MethodType ),
			TolX( TolX ),
			ATolX( ATolX ),
			ATolY( ATolY )
		{}

	};

	struct PointType
	{
		// Members
		bool DefinedFlag; // Set to true if point has been set; false otherwise
		Real64 X; // X value
		Real64 Y; // Y value = F(X)

		// Default Constructor
		PointType() :
			DefinedFlag( false ),
			X( 0.0 ),
			Y( 0.0 )
		{}

		// Member Constructor
		PointType(
			bool const DefinedFlag, // Set to true if point has been set; false otherwise
			Real64 const X, // X value
			Real64 const Y // Y value = F(X)
		) :
			DefinedFlag( DefinedFlag ),
			X( X ),
			Y( Y )
		{}

	};

	struct RootFinderDataType
	{
		// Members
		ControlsType Controls;
		int StatusFlag; // Current status of root finder
		// Valid values are any of the STATUS_<code> constants
		int CurrentMethodType; // Solution method used to perform current step
		Real64 XCandidate; // Candidate X value to use next when evaluating F(X)
		Real64 ConvergenceRate; // Convergence rate achieved over the last 2 successive iterations
		PointType Increment; // Increment between last 2 iterations
		PointType MinPoint; // Point { XMin, F(XMin) }
		PointType MaxPoint; // Point { XMax, F(XMax) }
		PointType LowerPoint; // Point { XLower, F(XLower) } so that XLower <= XRoot
		PointType UpperPoint; // Point { XUpper, F(XUpper) } so that XRoot <= YUpper
		PointType CurrentPoint; // Last evaluated point { X, F(X) }
		int NumHistory; // Number of points stored in History
		Array1D< PointType > History; // Vector containing last 3 best iterates

		// Default Constructor
		RootFinderDataType() :
			StatusFlag( iStatusNone ),
			CurrentMethodType( iMethodNone ),
			XCandidate( 0.0 ),
			ConvergenceRate( 0.0 ),
			NumHistory( 0 ),
			History( 3 )
		{}

		// Member Constructor
		RootFinderDataType(
			ControlsType const & Controls,
			int const StatusFlag, // Current status of root finder
			int const CurrentMethodType, // Solution method used to perform current step
			Real64 const XCandidate, // Candidate X value to use next when evaluating F(X)
			Real64 const ConvergenceRate, // Convergence rate achieved over the last 2 successive iterations
			PointType const & Increment, // Increment between last 2 iterations
			PointType const & MinPoint, // Point { XMin, F(XMin) }
			PointType const & MaxPoint, // Point { XMax, F(XMax) }
			PointType const & LowerPoint, // Point { XLower, F(XLower) } so that XLower <= XRoot
			PointType const & UpperPoint, // Point { XUpper, F(XUpper) } so that XRoot <= YUpper
			PointType const & CurrentPoint, // Last evaluated point { X, F(X) }
			int const NumHistory, // Number of points stored in History
			Array1< PointType > const & History // Vector containing last 3 best iterates
		) :
			Controls( Controls ),
			StatusFlag( StatusFlag ),
			CurrentMethodType( CurrentMethodType ),
			XCandidate( XCandidate ),
			ConvergenceRate( ConvergenceRate ),
			Increment( Increment ),
			MinPoint( MinPoint ),
			MaxPoint( MaxPoint ),
			LowerPoint( LowerPoint ),
			UpperPoint( UpperPoint ),
			CurrentPoint( CurrentPoint ),
			NumHistory( NumHistory ),
			History( 3, History )
		{}

	};

} // DataRootFinder

} // EnergyPlus

#endif
