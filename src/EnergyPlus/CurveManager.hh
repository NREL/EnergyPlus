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

#ifndef CurveManager_hh_INCLUDED
#define CurveManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Array5D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace CurveManager {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern std::string const Blank;

	// Curve Type parameters, these can differ from object types (e.g. a CurveType_TableOneIV can be linear, quadratic, etc)
	extern int const Linear;
	extern int const BiLinear;
	extern int const Quadratic;
	extern int const BiQuadratic;
	extern int const Cubic;
	extern int const QuadraticLinear;
	extern int const BiCubic;
	extern int const TriQuadratic;
	extern int const Exponent;
	extern int const Quartic;
	extern int const FuncPressDrop;
	extern int const MultiVariableLookup;
	extern int const FanPressureRise;
	extern int const ExponentialSkewNormal;
	extern int const Sigmoid;
	extern int const RectangularHyperbola1;
	extern int const RectangularHyperbola2;
	extern int const ExponentialDecay;
	extern int const DoubleExponentialDecay;
	extern int const QuadLinear;
	extern int const CubicLinear;
	extern int const ChillerPartLoadCustom;

	// Interpolation Types
	extern int const LinearInterpolationOfTable;
	extern int const LagrangeInterpolationLinearExtrapolation;
	extern int const EvaluateCurveToLimits;

	// Data Format
	extern int const SINGLELINEINDEPENDENTVARIABLEWITHMATRIX;

	// Sort Order
	extern int const ASCENDING;
	extern int const DESCENDING;

	// parameters describing curve object/table types
	extern int const NumAllCurveTypes;

	// curve object/table types (used for warning messages)
	extern int const CurveType_Linear;
	extern int const CurveType_Quadratic;
	extern int const CurveType_Cubic;
	extern int const CurveType_Quartic;
	extern int const CurveType_Exponent;
	extern int const CurveType_BiCubic;
	extern int const CurveType_BiQuadratic;
	extern int const CurveType_QuadraticLinear;
	extern int const CurveType_TriQuadratic;
	extern int const CurveType_FuncPressDrop;
	extern int const CurveType_TableOneIV;
	extern int const CurveType_TableTwoIV;
	extern int const CurveType_TableMultiIV;
	extern int const CurveType_FanPressureRise;
	extern int const CurveType_ExponentialSkewNormal;
	extern int const CurveType_Sigmoid;
	extern int const CurveType_RectangularHyperbola1;
	extern int const CurveType_RectangularHyperbola2;
	extern int const CurveType_ExponentialDecay;
	extern int const CurveType_DoubleExponentialDecay;
	extern int const CurveType_QuadLinear;
	extern int const CurveType_CubicLinear;
	extern int const CurveType_ChillerPartLoadCustom;

	extern Array1D_string const cCurveTypes;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	extern int NumCurves;
	extern bool GetCurvesInputFlag; // First time, input is "gotten"

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Types

	struct TriQuadraticCurveDataStruct
	{
		// Members
		// this structure is for 27 coefficient full triquadratic (!)
		Real64 CoeffA0;
		Real64 CoeffA1;
		Real64 CoeffA2;
		Real64 CoeffA3;
		Real64 CoeffA4;
		Real64 CoeffA5;
		Real64 CoeffA6;
		Real64 CoeffA7;
		Real64 CoeffA8;
		Real64 CoeffA9;
		Real64 CoeffA10;
		Real64 CoeffA11;
		Real64 CoeffA12;
		Real64 CoeffA13;
		Real64 CoeffA14;
		Real64 CoeffA15;
		Real64 CoeffA16;
		Real64 CoeffA17;
		Real64 CoeffA18;
		Real64 CoeffA19;
		Real64 CoeffA20;
		Real64 CoeffA21;
		Real64 CoeffA22;
		Real64 CoeffA23;
		Real64 CoeffA24;
		Real64 CoeffA25;
		Real64 CoeffA26;

		// Default Constructor
		TriQuadraticCurveDataStruct() :
			CoeffA0( 0.0 ),
			CoeffA1( 0.0 ),
			CoeffA2( 0.0 ),
			CoeffA3( 0.0 ),
			CoeffA4( 0.0 ),
			CoeffA5( 0.0 ),
			CoeffA6( 0.0 ),
			CoeffA7( 0.0 ),
			CoeffA8( 0.0 ),
			CoeffA9( 0.0 ),
			CoeffA10( 0.0 ),
			CoeffA11( 0.0 ),
			CoeffA12( 0.0 ),
			CoeffA13( 0.0 ),
			CoeffA14( 0.0 ),
			CoeffA15( 0.0 ),
			CoeffA16( 0.0 ),
			CoeffA17( 0.0 ),
			CoeffA18( 0.0 ),
			CoeffA19( 0.0 ),
			CoeffA20( 0.0 ),
			CoeffA21( 0.0 ),
			CoeffA22( 0.0 ),
			CoeffA23( 0.0 ),
			CoeffA24( 0.0 ),
			CoeffA25( 0.0 ),
			CoeffA26( 0.0 )
		{}

	};

	struct TableDataStruct
	{
		// Members
		Real64 NormalPoint;
		Array1D< Real64 > X1;
		Array1D< Real64 > X2;
		Array1D< Real64 > Y;

		// Default Constructor
		TableDataStruct() :
			NormalPoint( 1.0 )
		{}

	};

	struct PerfCurveTableDataStruct
	{
		// Members
		Array1D< Real64 > X1;
		Array1D< Real64 > X2;
		Array2D< Real64 > Y;

		// Default Constructor
		PerfCurveTableDataStruct()
		{}

	};

	struct PerfomanceCurveData
	{
		// Members
		std::string Name; // Curve Name
		int ObjectType; // Curve object type (e.g., integer for Curve:Linear above)
		int CurveType; // Curve type (see parameter definitions above)
		int InterpolationType; // table interpolation method
		int DataFormat; // format of tabular data
		int TableIndex; // Index to tablular data (0 if a standard curve object)
		int TableVariables; // Number of independent variables (0 if a standard curve object)
		int NumIVLowErrorIndex; // Index to table object error message for too few IV's
		int NumIVHighErrorIndex; // Index to table object error message for too many IV's
		int X1SortOrder; // sort order for table data for X1
		int X2SortOrder; // sort order for table data for X2
		Real64 Coeff1; // constant coefficient
		Real64 Coeff2; // linear coeff (1st independent variable)
		Real64 Coeff3; // quadratic coeff (1st independent variable)
		Real64 Coeff4; // linear coeff (2nd ind var) or cubic coeff
		Real64 Coeff5; // quadratic coeff (2nd independent variable)
		Real64 Coeff6; // cross coeff (1st & 2nd ind var)
		Real64 Coeff7; // cubic coeff for bicubic (1st ind var)
		Real64 Coeff8; // cubic coeff for bicubic (2nd ind var)
		Real64 Coeff9; // cross coeff for bicubic (1st quadratic & 2nd linear)
		Real64 Coeff10; // cross coeff for bicubic (1st linear & 2nd quadratic)
		Real64 Coeff11; // cross coeff
		Real64 Coeff12; // cross coeff
		Real64 Var1Max; // maximum of 1st independent variable
		Real64 Var1Min; // minimum of 1st independent variable
		Real64 Var2Max; // maximum of 2nd independent variable
		Real64 Var2Min; // minimum of 2nd independent variable
		Real64 Var3Max; // maximum of 3rd independent variable
		Real64 Var3Min; // minimum of 3rd independent variable
		Real64 Var4Max; // maximum of 4th independent variable
		Real64 Var4Min; // minimum of 4th independent variable
		Real64 Var5Max; // maximum of 5th independent variable
		Real64 Var5Min; // minimum of 5th independent variable
		Real64 CurveMin; // minimum value of curve output
		Real64 CurveMax; // maximum value of curve output
		bool CurveMinPresent; // If TRUE, then cap minimum curve output
		bool CurveMaxPresent; // if TRUE, then cap maximum curve output
		Array1D< TriQuadraticCurveDataStruct > Tri2ndOrder; // structure for triquadratic curve data
		bool EMSOverrideOn; // if TRUE, then EMS is calling to override curve value
		Real64 EMSOverrideCurveValue; // Value of curve result EMS is directing to use
		// report variables
		Real64 CurveOutput; // curve output or result
		Real64 CurveInput1; // curve input #1 (e.g., x or X1 variable)
		Real64 CurveInput2; // curve input #1 (e.g., y or X2 variable)
		Real64 CurveInput3; // curve input #1 (e.g., z or X3 variable)
		Real64 CurveInput4; // curve input #1 (e.g., X4 variable)
		Real64 CurveInput5; // curve input #1 (e.g., X5 variable)

		// Default Constructor
		PerfomanceCurveData() :
			ObjectType( 0 ),
			CurveType( 0 ),
			InterpolationType( 0 ),
			DataFormat( 0 ),
			TableIndex( 0 ),
			TableVariables( 0 ),
			NumIVLowErrorIndex( 0 ),
			NumIVHighErrorIndex( 0 ),
			X1SortOrder( 1 ),
			X2SortOrder( 1 ),
			Coeff1( 0.0 ),
			Coeff2( 0.0 ),
			Coeff3( 0.0 ),
			Coeff4( 0.0 ),
			Coeff5( 0.0 ),
			Coeff6( 0.0 ),
			Coeff7( 0.0 ),
			Coeff8( 0.0 ),
			Coeff9( 0.0 ),
			Coeff10( 0.0 ),
			Coeff11( 0.0 ),
			Coeff12( 0.0 ),
			Var1Max( 0.0 ),
			Var1Min( 0.0 ),
			Var2Max( 0.0 ),
			Var2Min( 0.0 ),
			Var3Max( 0.0 ),
			Var3Min( 0.0 ),
			Var4Max( 0.0 ),
			Var4Min( 0.0 ),
			Var5Max( 0.0 ),
			Var5Min( 0.0 ),
			CurveMin( 0.0 ),
			CurveMax( 0.0 ),
			CurveMinPresent( false ),
			CurveMaxPresent( false ),
			EMSOverrideOn( false ),
			EMSOverrideCurveValue( 0.0 ),
			CurveOutput( 0.0 ),
			CurveInput1( 0.0 ),
			CurveInput2( 0.0 ),
			CurveInput3( 0.0 ),
			CurveInput4( 0.0 ),
			CurveInput5( 0.0 )
		{}

	};

	struct TableLookupData
	{
		// Members
		int NumIndependentVars; // Curve type (see parameter definitions above)
		int InterpolationOrder; // number of points to interpolate (table data only)
		int NumX1Vars; // Number of variables for independent variable #1
		Array1D< Real64 > X1Var;
		int NumX2Vars; // Number of variables for independent variable #2
		Array1D< Real64 > X2Var;
		int NumX3Vars; // Number of variables for independent variable #3
		Array1D< Real64 > X3Var;
		int NumX4Vars; // Number of variables for independent variable #4
		Array1D< Real64 > X4Var;
		int NumX5Vars; // Number of variables for independent variable #5
		Array1D< Real64 > X5Var;
		Array5D< Real64 > TableLookupZData;

		// Default Constructor
		TableLookupData() :
			NumIndependentVars( 0 ),
			InterpolationOrder( 0 ),
			NumX1Vars( 0 ),
			NumX2Vars( 0 ),
			NumX3Vars( 0 ),
			NumX4Vars( 0 ),
			NumX5Vars( 0 )
		{}

	};

	// Object Data
	extern Array1D< PerfomanceCurveData > PerfCurve;
	extern Array1D< PerfCurveTableDataStruct > PerfCurveTableData;
	extern Array1D< TableDataStruct > TableData;
	extern Array1D< TableDataStruct > TempTableData;
	extern Array1D< TableDataStruct > Temp2TableData;
	extern Array1D< TableLookupData > TableLookup;

	// Functions

	// Clears the global data in CurveManager.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	void
	ResetPerformanceCurveOutput();

	Real64
	CurveValue(
		int const CurveIndex, // index of curve in curve array
		Real64 const Var1, // 1st independent variable
		Optional< Real64 const > Var2 = _, // 2nd independent variable
		Optional< Real64 const > Var3 = _, // 3rd independent variable
		Optional< Real64 const > Var4 = _, // 4th independent variable
		Optional< Real64 const > Var5 = _ // 5th independent variable
	);

	void
	GetCurveInput();

	void
	InitCurveReporting();

	void
	ReadTableData(
		int const CurveNum,
		std::string & CurrentModuleObject,
		bool const ReadFromFile,
		std::string & FileName,
		Array1S_string Alphas,
		Array1S< Real64 > Numbers,
		int const NumNumbers,
		bool & ErrorsFound
	);

	Real64
	DLAG(
		Real64 const XX,
		Real64 const YY,
		Array1S< Real64 > X,
		Array1S< Real64 > Y,
		Array2S< Real64 > Z,
		int const NX,
		int const NY,
		int const M,
		int & IEXTX,
		int & IEXTY
	);

	Real64
	PerformanceCurveObject(
		int const CurveIndex, // index of curve in curve array
		Real64 const Var1, // 1st independent variable
		Optional< Real64 const > Var2 = _, // 2nd independent variable
		Optional< Real64 const > Var3 = _, // 3rd independent variable
		Optional< Real64 const > Var4 = _ // 4th independent variable
	);

	Real64
	PerformanceTableObject(
		int const CurveIndex, // index of curve in curve array
		Real64 const Var1, // 1st independent variable
		Optional< Real64 const > Var2 = _, // 2nd independent variable
		Optional< Real64 const > Var3 = _ // 3rd independent variable
	);

	Real64
	TableLookupObject(
		int const CurveIndex, // index of curve in curve array
		Real64 const Var1, // 1st independent variable
		Optional< Real64 const > Var2 = _, // 2nd independent variable
		Optional< Real64 const > Var3 = _, // 3rd independent variable
		Optional< Real64 const > Var4 = _, // 4th independent variable
		Optional< Real64 const > Var5 = _ // 5th independent variable
	);

	void
	SolveRegression(
		int & CurveNum, // index to performance curve
		std::string & TableType, // tabular data object type
		std::string & CurveName, // performance curve name
		Array1S< Real64 > RawDataX, // table data X values (1st independent variable)
		Array1S< Real64 > RawDataY, // table data Y values (dependent variables)
		Optional< Array1S< Real64 > > RawDataX2 = _ // table data X2 values (2nd independent variable)
	);

	void
	Interpolate_Lagrange(
		Real64 const DataPoint, // point used for interpolating output (x)
		Array1S< Real64 > FunctionArray, // array of output data (Y's)
		Array1S< Real64 > Ordinate, // array of input data (X's)
		int const ISPT, // the starting point in the interpolated array
		int const IEPT, // the ending point in the interpolated array
		Real64 & ALAG // the interpolated output (y or F(x) in equation above)
	);

	bool
	IsCurveInputTypeValid( std::string const & InInputType ); // index of curve in curve array

	bool
	IsCurveOutputTypeValid( std::string const & InOutputType ); // index of curve in curve array

	std::string
	GetCurveType( int const CurveIndex ); // index of curve in curve array

	std::string
	GetCurveName( int const CurveIndex ); // index of curve in curve array

	int
	GetCurveIndex( std::string const & CurveName ); // name of the curve

	// This utility function grabs a curve index and performs the
	// error checking

	int
	GetCurveCheck(
		std::string const & alph, // curve name
		bool & errFlag,
		std::string const & ObjName // parent object of curve
	);

	void
	GetCurveMinMaxValues(
		int const CurveIndex, // index of curve in curve array
		Real64 & Var1Min, // Minimum values of 1st independent variable
		Real64 & Var1Max, // Maximum values of 1st independent variable
		Optional< Real64 > Var2Min = _, // Minimum values of 2nd independent variable
		Optional< Real64 > Var2Max = _, // Maximum values of 2nd independent variable
		Optional< Real64 > Var3Min = _, // Minimum values of 2nd independent variable
		Optional< Real64 > Var3Max = _ // Maximum values of 2nd independent variable
	);

	void
	SetCurveOutputMinMaxValues(
		int const CurveIndex, // index of curve in curve array
		bool & ErrorsFound, // TRUE when errors occur
		Optional< Real64 const > CurveMin = _, // Minimum value of curve output
		Optional< Real64 const > CurveMax = _ // Maximum values of curve output
	);

	void
	GetPressureSystemInput();

	void
	GetPressureCurveTypeAndIndex(
		std::string const & PressureCurveName, // name of the curve
		int & PressureCurveType,
		int & PressureCurveIndex
	);

	Real64
	PressureCurveValue(
		int const PressureCurveIndex,
		Real64 const MassFlow,
		Real64 const Density,
		Real64 const Viscosity
	);

	Real64
	CalculateMoodyFrictionFactor(
		Real64 const ReynoldsNumber,
		Real64 const RoughnessRatio
	);

	int
	GetCurveObjectTypeNum( int const CurveIndex ); // index of curve in curve array

	//=================================================================================================!

} // CurveManager

} // EnergyPlus

#endif
