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

// C++ Headers
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <CurveManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSystemVariables.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace CurveManager {
	// Module containing the Curve Manager routines

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   May 2000
	//       MODIFIED       January 2006, Rick Strand, added a curve type (quadratic-linear)
	//                      July 2006, L. Gu, added a new curve type (bicubic)

	//                      July 2006, Brent Griffith, added triquadratic curve
	//                       RR added exponential curve
	//                      May 2009 Brent griffith add EMS actuator registry and override (for custom equations)
	//                      August 2010, Richard Raustad, FSEC, added Table:* objects
	//                      August 2014, Rick Strand, added a curve type (cubic-linear)
	//                      Future Improvements:
	//                       1) Merge TableData and TableLookup arrays. Care is needed here since the
	//                          Table:OneIndependentVariable (and Two) use different data patterns.
	//                          For Table:One - a one-to-one correspondence between X and Z
	//                          For Table:Multi - not a one-to-one correspondence between X and Z
	//                          Code does show examples of the translation so each Table object can use
	//                          either interpolation technique.
	//                       2) Subroutine PerformanceTableObject is not really needed (and is probably slower)
	//                          since Subroutine TableLookupObject can do the same thing. The difference
	//                          is that Sub PerformanceTableObject does a linear interpolation without extrapolation.
	//                          More math is also involved. Sub TableLookupObject can also do this if a) the limits
	//                          of the input data use the boundaries of the tabular data, b) the arrays are corrected
	//                          to use this other subroutine, and c) the Number of Interpolation Points is set to 2.
	//                      22Aug2010 Craig Wray, added new curves for fan component model:
	//                          FanPressureRise, ExponentialSkewNormal, Sigmoid, RectangularHyperbola1,
	//                          RectangularHyperbola2, ExponentialDecay
	//                      March 2012, Atefe Makhmalbaf and Heejin Cho, added a new curve type (QuadLinear)
	//                      Aug.  2014, Rongpeng Zhang, added a new curve type (ChillerPartLoadWithLift)
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To provide the capabilities of getting the curve data from the input,
	// validating it, and storing it in such a manner that the curve manager
	// can provide the simulation with performance curve output.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::AnyEnergyManagementSystemInModel;
	using namespace DataBranchAirLoopPlant;

	// Use statements for access to subroutines in other modules

	// Data
	//MODULE PARAMETER DEFINITIONS
	static std::string const BlankString;

	// Curve Type parameters, these can differ from object types (e.g. a CurveType_TableOneIV can be linear, quadratic, etc)
	int const Linear( 1 );
	int const BiLinear( 2 );
	int const Quadratic( 3 );
	int const BiQuadratic( 4 );
	int const Cubic( 5 );
	int const QuadraticLinear( 6 );
	int const BiCubic( 7 );
	int const TriQuadratic( 8 );
	int const Exponent( 9 );
	int const Quartic( 10 );
	int const FuncPressDrop( 11 );
	int const MultiVariableLookup( 12 );
	int const FanPressureRise( 13 );
	int const ExponentialSkewNormal( 14 );
	int const Sigmoid( 15 );
	int const RectangularHyperbola1( 16 );
	int const RectangularHyperbola2( 17 );
	int const ExponentialDecay( 18 );
	int const DoubleExponentialDecay( 19 );
	int const QuadLinear( 20 );
	int const CubicLinear( 21 );
	int const ChillerPartLoadWithLift( 22 );

	// Interpolation Types
	int const LinearInterpolationOfTable( 1 );
	int const LagrangeInterpolationLinearExtrapolation( 2 );
	int const EvaluateCurveToLimits( 3 );

	// Data Format
	int const SINGLELINEINDEPENDENTVARIABLEWITHMATRIX( 1 );

	// Sort Order
	int const ASCENDING( 1 );
	int const DESCENDING( 2 );

	// parameters describing curve object/table types
	int const NumAllCurveTypes( 23 );

	// curve object/table types (used for warning messages)
	int const CurveType_Linear( 1 );
	int const CurveType_Quadratic( 2 );
	int const CurveType_Cubic( 3 );
	int const CurveType_Quartic( 4 );
	int const CurveType_Exponent( 5 );
	int const CurveType_BiCubic( 6 );
	int const CurveType_BiQuadratic( 7 );
	int const CurveType_QuadraticLinear( 8 );
	int const CurveType_TriQuadratic( 9 );
	int const CurveType_FuncPressDrop( 10 );
	int const CurveType_TableOneIV( 11 );
	int const CurveType_TableTwoIV( 12 );
	int const CurveType_TableMultiIV( 13 );
	int const CurveType_FanPressureRise( 14 );
	int const CurveType_ExponentialSkewNormal( 15 );
	int const CurveType_Sigmoid( 16 );
	int const CurveType_RectangularHyperbola1( 17 );
	int const CurveType_RectangularHyperbola2( 18 );
	int const CurveType_ExponentialDecay( 19 );
	int const CurveType_DoubleExponentialDecay( 20 );
	int const CurveType_QuadLinear( 21 );
	int const CurveType_CubicLinear( 22 );
	int const CurveType_ChillerPartLoadWithLift( 23 );

	Array1D_string const cCurveTypes( NumAllCurveTypes, { "Curve:Linear", "Curve:Quadratic", "Curve:Cubic", "Curve:Quartic", "Curve:Exponent", "Curve:BiCubic", "Curve:BiQuadratic", "Curve:QuadraitcLinear", "Curve:TriQuadratic", "Curve:Functional:PressureDrop", "Table:OneIndependentVariable", "Table:TwoIndependentVariables", "Table:MultiVariableLookup", "Curve:FanPressureRise", "Curve:ExponentialSkewNormal", "Curve:Sigmoid", "Curve:RectangularHyperbola1", "Curve:RectangularHyperbola2", "Curve:ExponentialDecay", "Curve:DoubleExponentialDecay", "Curve:QuadLinear", "Curve:CubicLinear", "Curve:ChillerPartLoadWithLift" } );

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	int NumCurves( 0 ); //Autodesk Was used unintialized in InitCurveReporting
	bool GetCurvesInputFlag( true ); // First time, input is "gotten"

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Object Data
	Array1D< PerfomanceCurveData > PerfCurve;
	Array1D< PerfCurveTableDataStruct > PerfCurveTableData;
	Array1D< TableDataStruct > TableData;
	Array1D< TableDataStruct > TempTableData;
	Array1D< TableDataStruct > Temp2TableData;
	Array1D< TableLookupData > TableLookup;

	// Functions

	// Clears the global data in CurveManager.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumCurves = 0;
		GetCurvesInputFlag = true;
		PerfCurve.deallocate();
		PerfCurveTableData.deallocate();
		TableData.deallocate();
		TempTableData.deallocate();
		Temp2TableData.deallocate();
		TableLookup.deallocate();
	}

	void
	ResetPerformanceCurveOutput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// Reset curve outputs prior to simulating air loops, plant loops, etc.
		// This allows the report variable for curve/table objects to show an inactive state.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataLoopNode::SensedNodeFlagValue;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		int CurveIndex;

		for ( CurveIndex = 1; CurveIndex <= NumCurves; ++CurveIndex ) {
			PerfCurve( CurveIndex ).CurveOutput = SensedNodeFlagValue;
			PerfCurve( CurveIndex ).CurveInput1 = SensedNodeFlagValue;
			PerfCurve( CurveIndex ).CurveInput2 = SensedNodeFlagValue;
			PerfCurve( CurveIndex ).CurveInput3 = SensedNodeFlagValue;
			PerfCurve( CurveIndex ).CurveInput4 = SensedNodeFlagValue;
			PerfCurve( CurveIndex ).CurveInput5 = SensedNodeFlagValue;
		}

	}

	Real64
	CurveValue(
		int const CurveIndex, // index of curve in curve array
		Real64 const Var1, // 1st independent variable
		Optional< Real64 const > Var2, // 2nd independent variable
		Optional< Real64 const > Var3, // 3rd independent variable
		Optional< Real64 const > Var4, // 4th independent variable
		Optional< Real64 const > Var5 // 5th independent variable
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   May 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Given the curve index and the values of 1 or 2 independent variables,
		// calls the curve or table routine to return the value of an equipment performance curve or table.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;

		// Return value
		Real64 CurveValue( 0.0 );

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static bool MyBeginTimeStepFlag;

		// need to be careful on where and how resetting curve outputs to some "iactive value" is done
		// EMS can intercept curves and modify output
		if ( BeginEnvrnFlag && MyBeginTimeStepFlag ) {
			ResetPerformanceCurveOutput();
			MyBeginTimeStepFlag = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyBeginTimeStepFlag = true;
		}

		if ( ( CurveIndex <= 0 ) || ( CurveIndex > NumCurves ) ) {
			ShowFatalError( "CurveValue: Invalid curve passed." );
		}

		{ auto const SELECT_CASE_var( PerfCurve( CurveIndex ).InterpolationType );
		if ( SELECT_CASE_var == EvaluateCurveToLimits ) {
			CurveValue = PerformanceCurveObject( CurveIndex, Var1, Var2, Var3 );
		} else if ( SELECT_CASE_var == LinearInterpolationOfTable ) {
			CurveValue = PerformanceTableObject( CurveIndex, Var1, Var2, Var3 );
		} else if ( SELECT_CASE_var == LagrangeInterpolationLinearExtrapolation ) {
			CurveValue = TableLookupObject( CurveIndex, Var1, Var2, Var3, Var4, Var5 );
		} else {
			ShowFatalError( "CurveValue: Invalid Interpolation Type" );
		}}

		if ( PerfCurve( CurveIndex ).EMSOverrideOn ) CurveValue = PerfCurve( CurveIndex ).EMSOverrideCurveValue;

		PerfCurve( CurveIndex ).CurveOutput = CurveValue;
		PerfCurve( CurveIndex ).CurveInput1 = Var1;
		if ( present( Var2 ) ) PerfCurve( CurveIndex ).CurveInput2 = Var2;
		if ( present( Var3 ) ) PerfCurve( CurveIndex ).CurveInput3 = Var3;
		if ( present( Var4 ) ) PerfCurve( CurveIndex ).CurveInput4 = Var4;
		if ( present( Var5 ) ) PerfCurve( CurveIndex ).CurveInput5 = Var5;

		return CurveValue;

	}

	void
	GetCurveInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       January 2006, Rick Strand, added a curve type (quadratic-linear)
		//                      July 2006, L. Gu, added a curve type (bicubic)
		//                      July 2006, BG added triquadratic.
		//                      April 2008, LL Added Linear Curve; July 2008, restructure for easier renaming
		//                      Feb 2009, R. Raustad - FSEC, added exponent curve
		//                      22Aug2010 Craig Wray, added new curves for fan component model:
		//                          FanPressureRise, ExponentialSkewNormal, Sigmoid, RectangularHyperbola1,
		//                          RectangularHyperbola2, ExponentialDecay
		//                      Aug.  2014, Rongpeng Zhang, added a new curve type (ChillerPartLoadWithLift)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtains input data for EnergyPlus equipment performance curves

		// METHODOLOGY EMPLOYED:
		// Uses "Get" routines to read in data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::FindItemInList;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using General::RoundSigDigits;
		//  USE DataGlobals, ONLY: DisplayExtraWarnings, OutputFileInits

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumBiQuad; // Number of biquadratic curve objects in the input data file
		int NumCubic; // Number of cubic curve objects in the input data file
		int NumQuartic; // Number of quartic (4th order polynomial) objects in the input data file
		int NumQuad; // Number of quadratic curve objects in the input data file
		int NumQuadLinear; // Number of quadratic linear curve objects in the input data file
		int NumCubicLinear; // Number of cubic linear curve objects in the input file
		int NumQLinear; // Number of quad linear curve objects in the input data file
		int NumLinear; // Number of linear curve objects in the input data file
		int NumBicubic; // Number of bicubic curve objects in the input data file
		int NumTriQuad; // Number of triquadratic curve objects in the input file
		int NumExponent; // Number of exponent curve objects in the input file
		int NumOneVarTab; // Number of one variable table objects in the input file
		int NumTwoVarTab; // Number of two variable table objects in the input file
		int NumChillerPartLoadWithLift; // Number of ChillerPartLoadWithLift curve objects in the input data file
		int NumMultVarLookup; // Number of multivariable tables
		int NumLookupTables; // total number of one, two, and multivariable tables
		int NumFanPressRise; // cpw22Aug2010 Number of fan pressure rise curve objects in the input file
		int NumExpSkewNorm; // cpw22Aug2010 Number of exponential skew normal curve objects in the input file
		int NumSigmoid; // cpw22Aug2010 Number of sigmoid curve objects in the input file
		int NumRectHyper1; // cpw22Aug2010 Number of rectangular hyperbola Type 1 curve objects in the input file
		int NumRectHyper2; // cpw22Aug2010 Number of rectangular hyperbola Type 2 curve objects in the input file
		int NumExpDecay; // cpw22Aug2010 Number of exponential decay curve objects in the input file
		int NumDoubleExpDecay; // ykt July 2011
		int NumTables; // Total tables in the input file
		int CurveIndex; // do loop index
		int CurveNum; // current curve number
		Array1D_string Alphas( 13 ); // Alpha items for object
		Array1D< Real64 > Numbers( 10000 ); // Numeric items for object
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		std::string CurrentModuleObject; // for ease in renaming.
		static int MaxTableNums( 0 ); // Maximum number of numeric input fields in Tables
		static int MaxTableData( 0 ); // Maximum number of numeric input field pairs in Tables
		static int TotalArgs( 0 ); // Total number of alpha and numeric arguments (max) for a
		//   certain object in the input file
		static int TableNum( 0 ); // Index to TableData structure
		static int TableDataIndex( 0 ); // Loop counter for table data
		static int NumTableEntries( 0 ); // Number of data pairs in table data
		int NumXVar;
		int NumX2Var;
		Array1D< Real64 > XVar;
		Array1D< Real64 > X2Var;
		int VarIndex;
		int TempVarIndex;
		int TempVarIndex1;
		Real64 MinTableData( 999999.0 );
		Real64 MaxTableDataValue;
		int NextXVar;
		bool FoundNewData;
		Array1D< Real64 > TempArray1;
		Array1D< Real64 > TempArray2;
		Array1D< Real64 > TempArray3;

		std::string FileName; // name of external table data file
		bool ReadFromFile; // True if external data file exists
		int CurveFound;

		// Find the number of each type of curve (note: Current Module object not used here, must rename manually)

		NumBiQuad = GetNumObjectsFound( "Curve:Biquadratic" );
		NumCubic = GetNumObjectsFound( "Curve:Cubic" );
		NumQuartic = GetNumObjectsFound( "Curve:Quartic" );
		NumQuad = GetNumObjectsFound( "Curve:Quadratic" );
		NumQLinear = GetNumObjectsFound( "Curve:QuadLinear" );
		NumQuadLinear = GetNumObjectsFound( "Curve:QuadraticLinear" );
		NumCubicLinear = GetNumObjectsFound( "Curve:CubicLinear" );
		NumLinear = GetNumObjectsFound( "Curve:Linear" );
		NumBicubic = GetNumObjectsFound( "Curve:Bicubic" );
		NumTriQuad = GetNumObjectsFound( "Curve:Triquadratic" );
		NumExponent = GetNumObjectsFound( "Curve:Exponent" );
		NumMultVarLookup = GetNumObjectsFound( "Table:MultiVariableLookup" );
		NumFanPressRise = GetNumObjectsFound( "Curve:FanPressureRise" ); //cpw22Aug2010
		NumExpSkewNorm = GetNumObjectsFound( "Curve:ExponentialSkewNormal" ); //cpw22Aug2010
		NumSigmoid = GetNumObjectsFound( "Curve:Sigmoid" ); //cpw22Aug2010
		NumRectHyper1 = GetNumObjectsFound( "Curve:RectangularHyperbola1" ); //cpw22Aug2010
		NumRectHyper2 = GetNumObjectsFound( "Curve:RectangularHyperbola2" ); //cpw22Aug2010
		NumExpDecay = GetNumObjectsFound( "Curve:ExponentialDecay" ); //cpw22Aug2010
		NumDoubleExpDecay = GetNumObjectsFound( "Curve:DoubleExponentialDecay" ); //ykt July 2011
		NumChillerPartLoadWithLift = GetNumObjectsFound( "Curve:ChillerPartLoadWithLift" ); // zrp_Aug2014

		NumOneVarTab = GetNumObjectsFound( "Table:OneIndependentVariable" );
		NumTwoVarTab = GetNumObjectsFound( "Table:TwoIndependentVariables" );

		NumCurves = NumBiQuad + NumCubic + NumQuad + NumQuadLinear + NumCubicLinear + NumLinear + NumBicubic + NumTriQuad + NumExponent + NumQuartic +
					NumOneVarTab + NumTwoVarTab + NumMultVarLookup + NumFanPressRise + NumExpSkewNorm + NumSigmoid + NumRectHyper1 + NumRectHyper2 +
					NumExpDecay + NumDoubleExpDecay + NumQLinear + NumChillerPartLoadWithLift;

		// intermediate count for one and two variable performance tables
		NumTables = NumOneVarTab + NumTwoVarTab;
		// final count for all tables
		NumLookupTables = NumOneVarTab + NumTwoVarTab + NumMultVarLookup;
		if ( NumLookupTables > 0 ) TableLookup.allocate( NumLookupTables );

		if ( NumOneVarTab > 0 ) {
			GetObjectDefMaxArgs( "Table:OneIndependentVariable", TotalArgs, NumAlphas, NumNumbers );
			MaxTableNums = max( MaxTableNums, NumNumbers );
			MaxTableData = max( MaxTableData, MaxTableNums );
		}
		if ( NumTwoVarTab > 0 ) {
			GetObjectDefMaxArgs( "Table:TwoIndependentVariables", TotalArgs, NumAlphas, NumNumbers );
			MaxTableNums = max( MaxTableNums, NumNumbers );
			MaxTableData = max( MaxTableData, MaxTableNums );
		}

		// allocate the data structure
		PerfCurve.allocate( NumCurves );
		PerfCurveTableData.allocate( NumLookupTables );
		TableData.allocate( NumLookupTables );
		TempTableData.allocate( NumTables );
		Temp2TableData.allocate( NumTables );
		// initialize the array

		CurveNum = 0;
		// Loop over biquadratic curves and load data
		CurrentModuleObject = "Curve:Biquadratic";
		for ( CurveIndex = 1; CurveIndex <= NumBiQuad; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			// could add checks for blank numeric fields, and use field names for errors.
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = BiQuadratic;
			PerfCurve( CurveNum ).ObjectType = CurveType_BiQuadratic;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Coeff4 = Numbers( 4 );
			PerfCurve( CurveNum ).Coeff5 = Numbers( 5 );
			PerfCurve( CurveNum ).Coeff6 = Numbers( 6 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 7 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 8 );
			PerfCurve( CurveNum ).Var2Min = Numbers( 9 );
			PerfCurve( CurveNum ).Var2Max = Numbers( 10 );
			if ( NumNumbers > 10 && ! lNumericFieldBlanks( 11 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 11 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 11 && ! lNumericFieldBlanks( 12 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 12 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 7 ) > Numbers( 8 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 7 ) + " [" + RoundSigDigits( Numbers( 7 ), 2 ) + "] > " + cNumericFieldNames( 8 ) + " [" + RoundSigDigits( Numbers( 8 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 9 ) > Numbers( 10 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 9 ) + " [" + RoundSigDigits( Numbers( 9 ), 2 ) + "] > " + cNumericFieldNames( 10 ) + " [" + RoundSigDigits( Numbers( 10 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for Y is invalid." );
				}
			}
			if ( NumAlphas >= 4 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 4 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}

		}

		// Loop over ChillerPartLoadWithLift curves and load data //zrp_Aug2014
		CurrentModuleObject = "Curve:ChillerPartLoadWithLift";
		for ( CurveIndex = 1; CurveIndex <= NumChillerPartLoadWithLift; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );

			PerfCurve( CurveNum ).CurveType = ChillerPartLoadWithLift;
			PerfCurve( CurveNum ).ObjectType = CurveType_ChillerPartLoadWithLift;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;

			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Coeff4 = Numbers( 4 );
			PerfCurve( CurveNum ).Coeff5 = Numbers( 5 );
			PerfCurve( CurveNum ).Coeff6 = Numbers( 6 );
			PerfCurve( CurveNum ).Coeff7 = Numbers( 7 );
			PerfCurve( CurveNum ).Coeff8 = Numbers( 8 );
			PerfCurve( CurveNum ).Coeff9 = Numbers( 9 );
			PerfCurve( CurveNum ).Coeff10 = Numbers( 10 );
			PerfCurve( CurveNum ).Coeff11 = Numbers( 11 );
			PerfCurve( CurveNum ).Coeff12 = Numbers( 12 );

			PerfCurve( CurveNum ).Var1Min = Numbers( 13 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 14 );
			PerfCurve( CurveNum ).Var2Min = Numbers( 15 );
			PerfCurve( CurveNum ).Var2Max = Numbers( 16 );
			PerfCurve( CurveNum ).Var3Min = Numbers( 17 );
			PerfCurve( CurveNum ).Var3Max = Numbers( 18 );

			if ( NumNumbers > 18 && ! lNumericFieldBlanks( 19 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 19 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 19 && ! lNumericFieldBlanks( 20 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 20 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for Y is invalid." );
				}
			}
			if ( NumAlphas >= 4 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 4 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the OInput Unit Type for Z is invalid." );
				}
			}
			if ( NumAlphas >= 5 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 5 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}

		}

		// Loop over cubic curves and load data
		CurrentModuleObject = "Curve:Cubic";
		for ( CurveIndex = 1; CurveIndex <= NumCubic; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = Cubic;
			PerfCurve( CurveNum ).ObjectType = CurveType_Cubic;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Coeff4 = Numbers( 4 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 5 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 6 );
			if ( NumNumbers > 6 && ! lNumericFieldBlanks( 7 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 7 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 7 && ! lNumericFieldBlanks( 8 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 8 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 5 ) > Numbers( 6 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 5 ) + '[' + RoundSigDigits( Numbers( 5 ), 2 ) + "] > " + cNumericFieldNames( 6 ) + " [" + RoundSigDigits( Numbers( 6 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}

		}

		// Loop over quadrinomial curves and load data
		CurrentModuleObject = "Curve:Quartic";
		for ( CurveIndex = 1; CurveIndex <= NumQuartic; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = Quartic;
			PerfCurve( CurveNum ).ObjectType = CurveType_Quartic;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Coeff4 = Numbers( 4 );
			PerfCurve( CurveNum ).Coeff5 = Numbers( 5 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 6 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 7 );
			if ( NumNumbers > 7 && ! lNumericFieldBlanks( 8 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 8 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 8 && ! lNumericFieldBlanks( 9 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 9 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 6 ) > Numbers( 7 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 6 ) + '[' + RoundSigDigits( Numbers( 6 ), 2 ) + "] > " + cNumericFieldNames( 7 ) + " [" + RoundSigDigits( Numbers( 7 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}

		}

		// Loop over quadratic curves and load data
		CurrentModuleObject = "Curve:Quadratic";
		for ( CurveIndex = 1; CurveIndex <= NumQuad; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = Quadratic;
			PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 4 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 5 );
			if ( NumNumbers > 5 && ! lNumericFieldBlanks( 6 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 6 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 6 && ! lNumericFieldBlanks( 7 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 7 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 4 ) > Numbers( 5 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 4 ) + " [" + RoundSigDigits( Numbers( 4 ), 2 ) + "] > " + cNumericFieldNames( 5 ) + " [" + RoundSigDigits( Numbers( 5 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}

		}

		// Loop over quadratic-linear curves and load data
		CurrentModuleObject = "Curve:QuadraticLinear";
		for ( CurveIndex = 1; CurveIndex <= NumQuadLinear; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = QuadraticLinear;
			PerfCurve( CurveNum ).ObjectType = CurveType_QuadraticLinear;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Coeff4 = Numbers( 4 );
			PerfCurve( CurveNum ).Coeff5 = Numbers( 5 );
			PerfCurve( CurveNum ).Coeff6 = Numbers( 6 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 7 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 8 );
			PerfCurve( CurveNum ).Var2Min = Numbers( 9 );
			PerfCurve( CurveNum ).Var2Max = Numbers( 10 );
			if ( NumNumbers > 10 && ! lNumericFieldBlanks( 11 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 11 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 11 && ! lNumericFieldBlanks( 12 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 12 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 7 ) > Numbers( 8 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 7 ) + " [" + RoundSigDigits( Numbers( 7 ), 2 ) + "] > " + cNumericFieldNames( 8 ) + " [" + RoundSigDigits( Numbers( 8 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 9 ) > Numbers( 10 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 9 ) + " [" + RoundSigDigits( Numbers( 9 ), 2 ) + "] > " + cNumericFieldNames( 10 ) + " [" + RoundSigDigits( Numbers( 10 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for Y is invalid." );
				}
			}
			if ( NumAlphas >= 4 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 4 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}
		}

		// Loop over cubic-linear curves and load data
		CurrentModuleObject = "Curve:CubicLinear";
		for ( CurveIndex = 1; CurveIndex <= NumCubicLinear; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = CubicLinear;
			PerfCurve( CurveNum ).ObjectType = CurveType_CubicLinear;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Coeff4 = Numbers( 4 );
			PerfCurve( CurveNum ).Coeff5 = Numbers( 5 );
			PerfCurve( CurveNum ).Coeff6 = Numbers( 6 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 7 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 8 );
			PerfCurve( CurveNum ).Var2Min = Numbers( 9 );
			PerfCurve( CurveNum ).Var2Max = Numbers( 10 );
			if ( NumNumbers > 10 && ! lNumericFieldBlanks( 11 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 11 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 11 && ! lNumericFieldBlanks( 12 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 12 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 7 ) > Numbers( 8 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 7 ) + " [" + RoundSigDigits( Numbers( 7 ), 2 ) + "] > " + cNumericFieldNames( 8 ) + " [" + RoundSigDigits( Numbers( 8 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 9 ) > Numbers( 10 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 9 ) + " [" + RoundSigDigits( Numbers( 9 ), 2 ) + "] > " + cNumericFieldNames( 10 ) + " [" + RoundSigDigits( Numbers( 10 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for Y is invalid." );
				}
			}
			if ( NumAlphas >= 4 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 4 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}
		}

		// Loop over linear curves and load data
		CurrentModuleObject = "Curve:Linear";
		for ( CurveIndex = 1; CurveIndex <= NumLinear; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = Linear;
			PerfCurve( CurveNum ).ObjectType = CurveType_Linear;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 3 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 4 );
			if ( NumNumbers > 4 && ! lNumericFieldBlanks( 5 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 5 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 5 && ! lNumericFieldBlanks( 6 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 6 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 3 ) > Numbers( 4 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 3 ) + " [" + RoundSigDigits( Numbers( 3 ), 2 ) + "] > " + cNumericFieldNames( 4 ) + " [" + RoundSigDigits( Numbers( 4 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}
		}

		// Loop over bicubic curves and load data
		CurrentModuleObject = "Curve:Bicubic";
		for ( CurveIndex = 1; CurveIndex <= NumBicubic; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = BiCubic;
			PerfCurve( CurveNum ).ObjectType = CurveType_BiCubic;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Coeff4 = Numbers( 4 );
			PerfCurve( CurveNum ).Coeff5 = Numbers( 5 );
			PerfCurve( CurveNum ).Coeff6 = Numbers( 6 );
			PerfCurve( CurveNum ).Coeff7 = Numbers( 7 );
			PerfCurve( CurveNum ).Coeff8 = Numbers( 8 );
			PerfCurve( CurveNum ).Coeff9 = Numbers( 9 );
			PerfCurve( CurveNum ).Coeff10 = Numbers( 10 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 11 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 12 );
			PerfCurve( CurveNum ).Var2Min = Numbers( 13 );
			PerfCurve( CurveNum ).Var2Max = Numbers( 14 );
			if ( NumNumbers > 14 && ! lNumericFieldBlanks( 15 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 15 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 15 && ! lNumericFieldBlanks( 16 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 16 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 11 ) > Numbers( 12 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 11 ) + " [" + RoundSigDigits( Numbers( 11 ), 2 ) + "] > " + cNumericFieldNames( 12 ) + " [" + RoundSigDigits( Numbers( 12 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 13 ) > Numbers( 14 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 13 ) + " [" + RoundSigDigits( Numbers( 13 ), 2 ) + "] > " + cNumericFieldNames( 14 ) + " [" + RoundSigDigits( Numbers( 14 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for Y is invalid." );
				}
			}
			if ( NumAlphas >= 4 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 4 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}
		}

		// Loop over Triquadratic curves and load data
		CurrentModuleObject = "Curve:Triquadratic";
		for ( CurveIndex = 1; CurveIndex <= NumTriQuad; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = TriQuadratic;
			PerfCurve( CurveNum ).ObjectType = CurveType_TriQuadratic;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Tri2ndOrder.allocate( 1 );
			for ( auto & e : PerfCurve( CurveNum ).Tri2ndOrder ) {
				e.CoeffA0 = Numbers( 1 );
				e.CoeffA1 = Numbers( 2 );
				e.CoeffA2 = Numbers( 3 );
				e.CoeffA3 = Numbers( 4 );
				e.CoeffA4 = Numbers( 5 );
				e.CoeffA5 = Numbers( 6 );
				e.CoeffA6 = Numbers( 7 );
				e.CoeffA7 = Numbers( 8 );
				e.CoeffA8 = Numbers( 9 );
				e.CoeffA9 = Numbers( 10 );
				e.CoeffA10 = Numbers( 11 );
				e.CoeffA11 = Numbers( 12 );
				e.CoeffA12 = Numbers( 13 );
				e.CoeffA13 = Numbers( 14 );
				e.CoeffA14 = Numbers( 15 );
				e.CoeffA15 = Numbers( 16 );
				e.CoeffA16 = Numbers( 17 );
				e.CoeffA17 = Numbers( 18 );
				e.CoeffA18 = Numbers( 19 );
				e.CoeffA19 = Numbers( 20 );
				e.CoeffA20 = Numbers( 21 );
				e.CoeffA21 = Numbers( 22 );
				e.CoeffA22 = Numbers( 23 );
				e.CoeffA23 = Numbers( 24 );
				e.CoeffA24 = Numbers( 25 );
				e.CoeffA25 = Numbers( 26 );
				e.CoeffA26 = Numbers( 27 );
			}
			PerfCurve( CurveNum ).Var1Min = Numbers( 28 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 29 );
			PerfCurve( CurveNum ).Var2Min = Numbers( 30 );
			PerfCurve( CurveNum ).Var2Max = Numbers( 31 );
			PerfCurve( CurveNum ).Var3Min = Numbers( 32 );
			PerfCurve( CurveNum ).Var3Max = Numbers( 33 );
			if ( NumNumbers > 33 && ! lNumericFieldBlanks( 34 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 34 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 34 && ! lNumericFieldBlanks( 35 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 35 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 28 ) > Numbers( 29 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 28 ) + " [" + RoundSigDigits( Numbers( 28 ), 2 ) + "] > " + cNumericFieldNames( 29 ) + " [" + RoundSigDigits( Numbers( 29 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 30 ) > Numbers( 31 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 30 ) + " [" + RoundSigDigits( Numbers( 30 ), 2 ) + "] > " + cNumericFieldNames( 31 ) + " [" + RoundSigDigits( Numbers( 31 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 32 ) > Numbers( 33 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 32 ) + " [" + RoundSigDigits( Numbers( 32 ), 2 ) + "] > " + cNumericFieldNames( 33 ) + " [" + RoundSigDigits( Numbers( 33 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for Y is invalid." );
				}
			}
			if ( NumAlphas >= 4 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 4 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for Z is invalid." );
				}
			}
			if ( NumAlphas >= 5 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 5 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}

		}

		// Loop over quad linear curves and load data
		CurrentModuleObject = "Curve:QuadLinear";
		for ( CurveIndex = 1; CurveIndex <= NumQLinear; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = QuadLinear;
			PerfCurve( CurveNum ).ObjectType = CurveType_QuadLinear;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Coeff4 = Numbers( 4 );
			PerfCurve( CurveNum ).Coeff5 = Numbers( 5 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 6 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 7 );
			PerfCurve( CurveNum ).Var2Min = Numbers( 8 );
			PerfCurve( CurveNum ).Var2Max = Numbers( 9 );
			PerfCurve( CurveNum ).Var3Min = Numbers( 10 );
			PerfCurve( CurveNum ).Var3Max = Numbers( 11 );
			PerfCurve( CurveNum ).Var4Min = Numbers( 12 );
			PerfCurve( CurveNum ).Var4Max = Numbers( 13 );

			if ( NumNumbers > 13 && ! lNumericFieldBlanks( 14 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 14 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 14 && ! lNumericFieldBlanks( 15 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 15 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 6 ) > Numbers( 7 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 6 ) + " [" + RoundSigDigits( Numbers( 6 ), 2 ) + "] > " + cNumericFieldNames( 7 ) + " [" + RoundSigDigits( Numbers( 7 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 8 ) > Numbers( 9 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 8 ) + " [" + RoundSigDigits( Numbers( 8 ), 2 ) + "] > " + cNumericFieldNames( 9 ) + " [" + RoundSigDigits( Numbers( 9 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 10 ) > Numbers( 11 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 10 ) + " [" + RoundSigDigits( Numbers( 10 ), 2 ) + "] > " + cNumericFieldNames( 11 ) + " [" + RoundSigDigits( Numbers( 11 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 12 ) > Numbers( 13 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 12 ) + " [" + RoundSigDigits( Numbers( 12 ), 2 ) + "] > " + cNumericFieldNames( 13 ) + " [" + RoundSigDigits( Numbers( 13 ), 2 ) + ']' );
				ErrorsFound = true;
			}

			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for W is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 4 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 4 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for Y is invalid." );
				}
			}
			if ( NumAlphas >= 5 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 5 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for Z is invalid." );
				}
			}
			if ( NumAlphas >= 6 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 6 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}

		}
		// Loop over Exponent curves and load data
		CurrentModuleObject = "Curve:Exponent";
		for ( CurveIndex = 1; CurveIndex <= NumExponent; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = Exponent;
			PerfCurve( CurveNum ).ObjectType = CurveType_Exponent;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 4 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 5 );
			if ( NumNumbers > 5 && ! lNumericFieldBlanks( 6 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 6 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 6 && ! lNumericFieldBlanks( 7 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 7 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}
			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}

		}

		// cpw22Aug2010 Loop over Fan Pressure Rise curves and load data - udated 15Sep2010 for unit types
		CurrentModuleObject = "Curve:FanPressureRise";
		for ( CurveIndex = 1; CurveIndex <= NumFanPressRise; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = FanPressureRise;
			PerfCurve( CurveNum ).ObjectType = CurveType_FanPressureRise;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Coeff4 = Numbers( 4 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 5 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 6 );
			PerfCurve( CurveNum ).Var2Min = Numbers( 7 );
			PerfCurve( CurveNum ).Var2Max = Numbers( 8 );

			if ( NumNumbers > 8 && ! lNumericFieldBlanks( 9 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 9 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 9 && ! lNumericFieldBlanks( 10 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 10 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 5 ) > Numbers( 6 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 5 ) + '[' + RoundSigDigits( Numbers( 5 ), 2 ) + "] > " + cNumericFieldNames( 6 ) + " [" + RoundSigDigits( Numbers( 6 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 7 ) > Numbers( 8 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 7 ) + '[' + RoundSigDigits( Numbers( 7 ), 2 ) + "] > " + cNumericFieldNames( 8 ) + " [" + RoundSigDigits( Numbers( 8 ), 2 ) + ']' );
				ErrorsFound = true;
			}

		} //Fan Pressure Rise

		// cpw22Aug2010 Loop over Exponential Skew Normal curves and load data
		CurrentModuleObject = "Curve:ExponentialSkewNormal";
		for ( CurveIndex = 1; CurveIndex <= NumExpSkewNorm; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = ExponentialSkewNormal;
			PerfCurve( CurveNum ).ObjectType = CurveType_ExponentialSkewNormal;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Coeff4 = Numbers( 4 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 5 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 6 );

			if ( NumNumbers > 6 && ! lNumericFieldBlanks( 7 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 7 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 7 && ! lNumericFieldBlanks( 9 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 9 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 5 ) > Numbers( 6 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 5 ) + '[' + RoundSigDigits( Numbers( 5 ), 2 ) + "] > " + cNumericFieldNames( 6 ) + " [" + RoundSigDigits( Numbers( 6 ), 2 ) + ']' );
				ErrorsFound = true;
			}

			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}
		} //Exponential Skew Normal

		// cpw22Aug2010 Loop over Sigmoid curves and load data
		CurrentModuleObject = "Curve:Sigmoid";
		for ( CurveIndex = 1; CurveIndex <= NumSigmoid; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = Sigmoid;
			PerfCurve( CurveNum ).ObjectType = CurveType_Sigmoid;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Coeff4 = Numbers( 4 );
			PerfCurve( CurveNum ).Coeff5 = Numbers( 5 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 6 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 7 );

			if ( NumNumbers > 7 && ! lNumericFieldBlanks( 8 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 8 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 8 && ! lNumericFieldBlanks( 9 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 9 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 6 ) > Numbers( 7 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 6 ) + '[' + RoundSigDigits( Numbers( 6 ), 2 ) + "] > " + cNumericFieldNames( 7 ) + " [" + RoundSigDigits( Numbers( 7 ), 2 ) + ']' );
				ErrorsFound = true;
			}

			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}
		} //Sigmoid

		// cpw22Aug2010 Loop over Rectangular Hyperbola Type 1 curves and load data
		CurrentModuleObject = "Curve:RectangularHyperbola1";
		for ( CurveIndex = 1; CurveIndex <= NumRectHyper1; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = RectangularHyperbola1;
			PerfCurve( CurveNum ).ObjectType = CurveType_RectangularHyperbola1;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 4 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 5 );

			if ( NumNumbers > 5 && ! lNumericFieldBlanks( 6 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 6 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 6 && ! lNumericFieldBlanks( 7 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 7 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 4 ) > Numbers( 5 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 4 ) + '[' + RoundSigDigits( Numbers( 4 ), 2 ) + "] > " + cNumericFieldNames( 5 ) + " [" + RoundSigDigits( Numbers( 5 ), 2 ) + ']' );
				ErrorsFound = true;
			}

			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}
		} //Rectangular Hyperbola Type 1

		// cpw22Aug2010 Loop over Rectangular Hyperbola Type 2 curves and load data
		CurrentModuleObject = "Curve:RectangularHyperbola2";
		for ( CurveIndex = 1; CurveIndex <= NumRectHyper2; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = RectangularHyperbola2;
			PerfCurve( CurveNum ).ObjectType = CurveType_RectangularHyperbola2;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 4 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 5 );

			if ( NumNumbers > 5 && ! lNumericFieldBlanks( 6 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 6 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 6 && ! lNumericFieldBlanks( 7 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 7 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 4 ) > Numbers( 5 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 4 ) + '[' + RoundSigDigits( Numbers( 4 ), 2 ) + "] > " + cNumericFieldNames( 5 ) + " [" + RoundSigDigits( Numbers( 5 ), 2 ) + ']' );
				ErrorsFound = true;
			}

			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}
		} //Rectangular Hyperbola Type 2

		// cpw22Aug2010 Loop over Exponential Decay curves and load data
		CurrentModuleObject = "Curve:ExponentialDecay";
		for ( CurveIndex = 1; CurveIndex <= NumExpDecay; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = ExponentialDecay;
			PerfCurve( CurveNum ).ObjectType = CurveType_ExponentialDecay;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 4 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 5 );

			if ( NumNumbers > 5 && ! lNumericFieldBlanks( 6 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 6 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 6 && ! lNumericFieldBlanks( 7 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 7 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Numbers( 4 ) > Numbers( 5 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 4 ) + '[' + RoundSigDigits( Numbers( 4 ), 2 ) + "] > " + cNumericFieldNames( 5 ) + " [" + RoundSigDigits( Numbers( 5 ), 2 ) + ']' );
				ErrorsFound = true;
			}

			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}
		} //Exponential Decay

		// ykt July,2011 Loop over DoubleExponential Decay curves and load data
		CurrentModuleObject = "Curve:DoubleExponentialDecay";
		for ( CurveIndex = 1; CurveIndex <= NumDoubleExpDecay; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).CurveType = DoubleExponentialDecay;
			PerfCurve( CurveNum ).ObjectType = CurveType_DoubleExponentialDecay;
			PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			PerfCurve( CurveNum ).Coeff1 = Numbers( 1 );
			PerfCurve( CurveNum ).Coeff2 = Numbers( 2 );
			PerfCurve( CurveNum ).Coeff3 = Numbers( 3 );
			PerfCurve( CurveNum ).Coeff4 = Numbers( 4 );
			PerfCurve( CurveNum ).Coeff5 = Numbers( 5 );
			PerfCurve( CurveNum ).Var1Min = Numbers( 6 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 7 );

			if ( NumNumbers > 7 && ! lNumericFieldBlanks( 8 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 8 );
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( NumNumbers > 8 && ! lNumericFieldBlanks( 9 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 9 );
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			//  IF (Numbers(4) > Numbers(5)) THEN  ! error
			//    CALL ShowSevereError('GetCurveInput: For '//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1)))
			//    CALL ShowContinueError(TRIM(cNumericFieldNames(4))//'['//TRIM(RoundSigDigits(Numbers(4),2))//'] > '//  &
			//       TRIM(cNumericFieldNames(5))//' ['//TRIM(RoundSigDigits(Numbers(5),2))//']')
			//    ErrorsFound=.TRUE.
			//  ENDIF

			if ( NumAlphas >= 2 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 2 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Input Unit Type for X is invalid." );
				}
			}
			if ( NumAlphas >= 3 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 3 ) ) ) {
					ShowWarningError( "In " + CurrentModuleObject + " named " + Alphas( 1 ) + " the Output Unit Type is invalid." );
				}
			}
		} //Exponential Decay
		TableNum = 0;

		// Loop over one variable tables and load data
		CurrentModuleObject = "Table:OneIndependentVariable";
		for ( CurveIndex = 1; CurveIndex <= NumOneVarTab; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			++TableNum;
			NumTableEntries = ( NumNumbers - 5 ) / 2;
			TableData( TableNum ).X1.allocate( NumTableEntries );
			TableData( TableNum ).Y.allocate( NumTableEntries );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).ObjectType = CurveType_TableOneIV;
			PerfCurve( CurveNum ).TableIndex = TableNum;
			{ auto const SELECT_CASE_var( Alphas( 2 ) );
			if ( SELECT_CASE_var == "LINEAR" ) {
				PerfCurve( CurveNum ).CurveType = Linear;
				TableLookup( TableNum ).InterpolationOrder = 2;
			} else if ( SELECT_CASE_var == "QUADRATIC" ) {
				PerfCurve( CurveNum ).CurveType = Quadratic;
				TableLookup( TableNum ).InterpolationOrder = 3;
			} else if ( SELECT_CASE_var == "CUBIC" ) {
				PerfCurve( CurveNum ).CurveType = Cubic;
				TableLookup( TableNum ).InterpolationOrder = 4;
			} else if ( SELECT_CASE_var == "QUARTIC" ) {
				PerfCurve( CurveNum ).CurveType = Quartic;
				TableLookup( TableNum ).InterpolationOrder = 5;
			} else if ( SELECT_CASE_var == "EXPONENT" ) {
				PerfCurve( CurveNum ).CurveType = Exponent;
				TableLookup( TableNum ).InterpolationOrder = 4;
			} else {
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cAlphaFieldNames( 2 ) + " [" + Alphas( 2 ) + "] is not a valid choice. " );
				ErrorsFound = true;
			}}

			{ auto const SELECT_CASE_var( Alphas( 3 ) );
			if ( SELECT_CASE_var == "LINEARINTERPOLATIONOFTABLE" ) {
				PerfCurve( CurveNum ).InterpolationType = LinearInterpolationOfTable;
			} else if ( SELECT_CASE_var == "LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION" ) {
				PerfCurve( CurveNum ).InterpolationType = LagrangeInterpolationLinearExtrapolation;
			} else if ( SELECT_CASE_var == "EVALUATECURVETOLIMITS" ) {
				PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			} else {
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cAlphaFieldNames( 2 ) + " [" + Alphas( 2 ) + "] is not a valid choice. " );
				ErrorsFound = true;
			}}

			if ( lNumericFieldBlanks( 1 ) ) {
				PerfCurve( CurveNum ).Var1Min = 99999999999.0;
			} else {
				PerfCurve( CurveNum ).Var1Min = Numbers( 1 );
			}
			if ( lNumericFieldBlanks( 2 ) ) {
				PerfCurve( CurveNum ).Var1Max = -99999999999.0;
			} else {
				PerfCurve( CurveNum ).Var1Max = Numbers( 2 );
			}

			if ( ! lNumericFieldBlanks( 1 ) && ! lNumericFieldBlanks( 2 ) ) {
				if ( Numbers( 1 ) > Numbers( 2 ) ) { // error
					ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( cNumericFieldNames( 1 ) + " [" + RoundSigDigits( Numbers( 1 ), 2 ) + "] > " + cNumericFieldNames( 2 ) + " [" + RoundSigDigits( Numbers( 2 ), 2 ) + ']' );
					ErrorsFound = true;
				}
			}
			if ( NumAlphas >= 4 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 4 ) ) ) {
					ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( cAlphaFieldNames( 4 ) + " [" + Alphas( 4 ) + "] is invalid" );
				}
			}
			if ( NumAlphas >= 5 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 5 ) ) ) {
					ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( cAlphaFieldNames( 5 ) + " [" + Alphas( 5 ) + "] is invlaid" );
				}
			}

			// read this value first to allow normalization of min/max table output fields
			if ( ! lNumericFieldBlanks( 5 ) ) {
				TableData( TableNum ).NormalPoint = Numbers( 5 );
				if ( Numbers( 5 ) == 0.0 ) {
					ShowSevereError( "GetTableInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( "..." + cNumericFieldNames( 5 ) + " [" + RoundSigDigits( Numbers( 5 ), 6 ) + "] is not a valid choice." );
					ShowContinueError( "...Setting Normalization Reference to 1 and the simulation continues." );
					TableData( TableNum ).NormalPoint = 1.0;
				}
			} else {
				TableData( TableNum ).NormalPoint = 1.0;
			}

			if ( ! lNumericFieldBlanks( 3 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 3 ) / TableData( TableNum ).NormalPoint;
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( ! lNumericFieldBlanks( 4 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 4 ) / TableData( TableNum ).NormalPoint;
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			MaxTableNums = ( NumNumbers - 5 ) / 2;
			if ( mod( ( NumNumbers - 5 ), 2 ) != 0 ) {
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( "The number of data entries must be evenly divisable by 2. Number of data entries = " + RoundSigDigits( NumNumbers - 5 ) );
				ErrorsFound = true;
			} else {
				for ( TableDataIndex = 1; TableDataIndex <= MaxTableNums; ++TableDataIndex ) {
					TableData( TableNum ).X1( TableDataIndex ) = Numbers( ( TableDataIndex - 1 ) * 2 + 5 + 1 );
					TableData( TableNum ).Y( TableDataIndex ) = Numbers( ( TableDataIndex - 1 ) * 2 + 5 + 2 ) / TableData( TableNum ).NormalPoint;
				}
			}

			// convert raw table data to multidimensional array
			// find number of x variables
			NumXVar = 1;
			NextXVar = 1;
			TempTableData = TableData;
			while ( NumXVar <= MaxTableNums ) {

				MinTableData = minval( TempTableData( TableNum ).X1 );
				for ( VarIndex = 1; VarIndex <= MaxTableNums; ++VarIndex ) {
					if ( TempTableData( TableNum ).X1( VarIndex ) == MinTableData ) {
						TableData( TableNum ).X1( NumXVar ) = TempTableData( TableNum ).X1( VarIndex );
						TableData( TableNum ).Y( NumXVar ) = TempTableData( TableNum ).Y( VarIndex );
						TempTableData( TableNum ).X1( VarIndex ) = 999999.0;
						++NumXVar;
					}
				}

				NextXVar = NumXVar;

			}

			// move table data to performance curve table data structure
			PerfCurveTableData( TableNum ).X1.allocate( NumXVar - 1 );
			PerfCurveTableData( TableNum ).Y.allocate( 1, NumXVar - 1 );
			PerfCurveTableData( TableNum ).X1 = TableData( TableNum ).X1;
			for ( VarIndex = 1; VarIndex <= NumXVar - 1; ++VarIndex ) {
				PerfCurveTableData( TableNum ).Y( 1, VarIndex ) = TableData( TableNum ).Y( VarIndex );
			}

			// create curve objects when regression analysis is required
			if ( PerfCurve( CurveNum ).InterpolationType == EvaluateCurveToLimits ) {
				{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
				if ( ( SELECT_CASE_var == Linear ) || ( SELECT_CASE_var == Quadratic ) || ( SELECT_CASE_var == Cubic ) || ( SELECT_CASE_var == Quartic ) || ( SELECT_CASE_var == Exponent ) ) {
					TempArray1 = PerfCurveTableData( TableNum ).X1;
					TempArray2.allocate( size( PerfCurveTableData( TableNum ).Y ) );
					for ( VarIndex = 1; VarIndex <= isize( PerfCurveTableData( TableNum ).Y ); ++VarIndex ) {
						TempArray2( VarIndex ) = PerfCurveTableData( TableNum ).Y( 1, VarIndex );
					}
					SolveRegression( CurveNum, CurrentModuleObject, PerfCurve( CurveNum ).Name, TempArray1, TempArray2 );
					TempArray1.deallocate();
					TempArray2.deallocate();
				} else {
					ShowWarningError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( "The requested regression analysis is not available at this time. Curve type = " + Alphas( 2 ) );
					PerfCurve( CurveIndex ).InterpolationType = LinearInterpolationOfTable;
				}}
			}
			// move table data to more compact array to allow interpolation using multivariable lookup table method
			TableLookup( TableNum ).NumIndependentVars = 1;
			TableLookup( TableNum ).NumX1Vars = size( PerfCurveTableData( TableNum ).X1 );
			TableLookup( TableNum ).X1Var.allocate( TableLookup( TableNum ).NumX1Vars );
			TableLookup( TableNum ).TableLookupZData.allocate( 1, 1, 1, 1, size( PerfCurveTableData( TableNum ).Y ) );
			TableLookup( TableNum ).X1Var = PerfCurveTableData( TableNum ).X1;
			TableLookup( TableNum ).TableLookupZData( 1, 1, 1, 1, _ ) = PerfCurveTableData( TableNum ).Y( 1, _ );
		}

		// Loop over two variable tables and load data
		CurrentModuleObject = "Table:TwoIndependentVariables";
		for ( CurveIndex = 1; CurveIndex <= NumTwoVarTab; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			++TableNum;
			NumTableEntries = ( NumNumbers - 7 ) / 3;
			TableData( TableNum ).X1.allocate( NumTableEntries );
			TableData( TableNum ).X2.allocate( NumTableEntries );
			TableData( TableNum ).Y.allocate( NumTableEntries );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).ObjectType = CurveType_TableTwoIV;
			PerfCurve( CurveNum ).TableIndex = TableNum;
			{ auto const SELECT_CASE_var( Alphas( 2 ) );
			if ( SELECT_CASE_var == "BICUBIC" ) {
				PerfCurve( CurveNum ).CurveType = BiCubic;
				TableLookup( TableNum ).InterpolationOrder = 4;
			} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
				PerfCurve( CurveNum ).CurveType = BiQuadratic;
				TableLookup( TableNum ).InterpolationOrder = 3;
			} else if ( SELECT_CASE_var == "QUADRATICLINEAR" ) {
				PerfCurve( CurveNum ).CurveType = QuadraticLinear;
				TableLookup( TableNum ).InterpolationOrder = 3;
			} else if ( SELECT_CASE_var == "TRIQUADRATIC" ) {
				PerfCurve( CurveNum ).CurveType = TriQuadratic;
				TableLookup( TableNum ).InterpolationOrder = 3;
			} else {
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cAlphaFieldNames( 2 ) + " [" + Alphas( 2 ) + "] is not a valid choice. " );
				ErrorsFound = true;
			}}
			{ auto const SELECT_CASE_var( Alphas( 3 ) );
			if ( SELECT_CASE_var == "LINEARINTERPOLATIONOFTABLE" ) {
				PerfCurve( CurveNum ).InterpolationType = LinearInterpolationOfTable;
			} else if ( SELECT_CASE_var == "LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION" ) {
				PerfCurve( CurveNum ).InterpolationType = LagrangeInterpolationLinearExtrapolation;
			} else if ( SELECT_CASE_var == "EVALUATECURVETOLIMITS" ) {
				PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			} else {
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cAlphaFieldNames( 2 ) + " [" + Alphas( 2 ) + "] is not a valid choice. " );
				ErrorsFound = true;
			}}

			PerfCurve( CurveNum ).Var1Min = Numbers( 1 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 2 );
			PerfCurve( CurveNum ).Var2Min = Numbers( 3 );
			PerfCurve( CurveNum ).Var2Max = Numbers( 4 );

			if ( Numbers( 1 ) > Numbers( 2 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 1 ) + " [" + RoundSigDigits( Numbers( 1 ), 2 ) + "] > " + cNumericFieldNames( 2 ) + " [" + RoundSigDigits( Numbers( 2 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 3 ) > Numbers( 4 ) ) { // error
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 3 ) + " [" + RoundSigDigits( Numbers( 3 ), 2 ) + "] > " + cNumericFieldNames( 4 ) + " [" + RoundSigDigits( Numbers( 4 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( NumAlphas >= 4 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 4 ) ) ) {
					ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( cAlphaFieldNames( 4 ) + " [" + Alphas( 4 ) + "] is invlaid" );
				}
			}
			if ( NumAlphas >= 5 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 5 ) ) ) {
					ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( cAlphaFieldNames( 5 ) + " [" + Alphas( 5 ) + "] is invlaid" );
				}
			}
			if ( NumAlphas >= 6 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 6 ) ) ) {
					ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( cAlphaFieldNames( 6 ) + " [" + Alphas( 6 ) + "] is invlaid" );
				}
			}

			if ( ! lNumericFieldBlanks( 7 ) ) {
				TableData( TableNum ).NormalPoint = Numbers( 7 );
				if ( Numbers( 7 ) == 0.0 ) {
					ShowSevereError( "GetTableInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( "..." + cNumericFieldNames( 7 ) + " [" + RoundSigDigits( Numbers( 7 ), 6 ) + "] is not a valid choice." );
					ShowContinueError( "...Setting Normalization Reference to 1 and the simulation continues." );
					TableData( TableNum ).NormalPoint = 1.0;
				}
			} else {
				TableData( TableNum ).NormalPoint = 1.0;
			}

			if ( ! lNumericFieldBlanks( 5 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 5 ) / TableData( TableNum ).NormalPoint;
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( ! lNumericFieldBlanks( 6 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 6 ) / TableData( TableNum ).NormalPoint;
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			MaxTableNums = ( NumNumbers - 7 ) / 3;
			if ( mod( ( NumNumbers - 7 ), 3 ) != 0 ) {
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( "The number of data entries must be evenly divisable by 3. Number of data entries = " + RoundSigDigits( NumNumbers - 7 ) );
				ErrorsFound = true;
			} else {
				for ( TableDataIndex = 1; TableDataIndex <= MaxTableNums; ++TableDataIndex ) {
					TableData( TableNum ).X1( TableDataIndex ) = Numbers( ( TableDataIndex - 1 ) * 3 + 7 + 1 );
					TableData( TableNum ).X2( TableDataIndex ) = Numbers( ( TableDataIndex - 1 ) * 3 + 7 + 2 );
					TableData( TableNum ).Y( TableDataIndex ) = Numbers( ( TableDataIndex - 1 ) * 3 + 7 + 3 ) / TableData( TableNum ).NormalPoint;
				}
			}

			//  convert raw table data to multidimensional array
			// find number of x variables
			XVar.allocate( MaxTableNums );
			X2Var.allocate( MaxTableNums );
			NumXVar = 1;
			NextXVar = 1;
			XVar( 1 ) = 1.0;
			TempTableData = TableData;
			Temp2TableData = TableData;
			while ( NumXVar <= MaxTableNums ) {

				MinTableData = minval( TempTableData( TableNum ).X1 );
				for ( VarIndex = 1; VarIndex <= MaxTableNums; ++VarIndex ) {
					if ( TempTableData( TableNum ).X1( VarIndex ) == MinTableData ) {
						TableData( TableNum ).X1( NumXVar ) = TempTableData( TableNum ).X1( VarIndex );
						TableData( TableNum ).X2( NumXVar ) = TempTableData( TableNum ).X2( VarIndex );
						TableData( TableNum ).Y( NumXVar ) = TempTableData( TableNum ).Y( VarIndex );
						TempTableData( TableNum ).X1( VarIndex ) = 999999.0;
						++NumXVar;
					}
				}
				Temp2TableData( TableNum ).X2( {NextXVar,NumXVar - 1} ) = TableData( TableNum ).X2( {NextXVar,NumXVar - 1} );
				Temp2TableData( TableNum ).Y( {NextXVar,NumXVar - 1} ) = TableData( TableNum ).Y( {NextXVar,NumXVar - 1} );

				for ( TempVarIndex = NumXVar - 1; TempVarIndex >= NextXVar; --TempVarIndex ) {
					MaxTableDataValue = -999999.0;
					for ( TempVarIndex1 = NextXVar; TempVarIndex1 <= NumXVar - 1; ++TempVarIndex1 ) {
						if ( Temp2TableData( TableNum ).X2( TempVarIndex1 ) > MaxTableDataValue ) {
							MaxTableDataValue = Temp2TableData( TableNum ).X2( TempVarIndex1 );
						}
					}

					for ( TempVarIndex1 = NextXVar; TempVarIndex1 <= NumXVar - 1; ++TempVarIndex1 ) {
						if ( Temp2TableData( TableNum ).X2( TempVarIndex1 ) != MaxTableDataValue ) continue;
						TableData( TableNum ).X2( TempVarIndex ) = Temp2TableData( TableNum ).X2( TempVarIndex1 );
						TableData( TableNum ).Y( TempVarIndex ) = Temp2TableData( TableNum ).Y( TempVarIndex1 );
						Temp2TableData( TableNum ).X2( TempVarIndex1 ) = -999999.0;
						break;
					}
				}

				NextXVar = NumXVar;

			}
			// reorganize table data
			NumXVar = 1;
			NumX2Var = 1;
			XVar( 1 ) = TableData( TableNum ).X1( 1 );
			for ( VarIndex = 2; VarIndex <= MaxTableNums; ++VarIndex ) {
				if ( TableData( TableNum ).X1( VarIndex ) != TableData( TableNum ).X1( VarIndex - 1 ) ) {
					++NumXVar;
					XVar( NumXVar ) = TableData( TableNum ).X1( VarIndex );
				}
			}
			X2Var( 1 ) = TableData( TableNum ).X2( 1 );
			for ( VarIndex = 2; VarIndex <= MaxTableNums; ++VarIndex ) {
				FoundNewData = true;
				for ( TempVarIndex = 1; TempVarIndex <= NumX2Var; ++TempVarIndex ) {
					if ( TableData( TableNum ).X2( VarIndex ) == X2Var( TempVarIndex ) ) {
						FoundNewData = false;
					}
				}
				if ( FoundNewData ) {
					++NumX2Var;
					X2Var( NumX2Var ) = TableData( TableNum ).X2( VarIndex );
				}
			}

			// move table data to performance curve table data structure
			PerfCurveTableData( TableNum ).X1.allocate( NumXVar );
			PerfCurveTableData( TableNum ).X2.allocate( NumX2Var );
			PerfCurveTableData( TableNum ).Y.allocate( NumX2Var, NumXVar );
			PerfCurveTableData( TableNum ).X1 = -9999999.0;
			PerfCurveTableData( TableNum ).X2 = -9999999.0;
			PerfCurveTableData( TableNum ).Y = -9999999.0;
			for ( VarIndex = 1; VarIndex <= NumXVar; ++VarIndex ) {
				PerfCurveTableData( TableNum ).X1( VarIndex ) = XVar( VarIndex );
				for ( TempVarIndex = 1; TempVarIndex <= NumX2Var; ++TempVarIndex ) {
					PerfCurveTableData( TableNum ).X2( TempVarIndex ) = X2Var( TempVarIndex );
					for ( TempVarIndex1 = 1; TempVarIndex1 <= MaxTableNums; ++TempVarIndex1 ) {
						if ( ( TableData( TableNum ).X1( TempVarIndex1 ) == PerfCurveTableData( TableNum ).X1( VarIndex ) ) && ( TableData( TableNum ).X2( TempVarIndex1 ) == PerfCurveTableData( TableNum ).X2( TempVarIndex ) ) ) {
							PerfCurveTableData( TableNum ).Y( TempVarIndex, VarIndex ) = TableData( TableNum ).Y( TempVarIndex1 );
						}
					}
				}
			}
			XVar.deallocate();
			X2Var.deallocate();

			// create curve objects when regression analysis is required
			if ( PerfCurve( CurveNum ).InterpolationType == EvaluateCurveToLimits ) {
				{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
				if ( ( SELECT_CASE_var == BiQuadratic ) || ( SELECT_CASE_var == QuadraticLinear ) ) {
					TempArray1 = TableData( TableNum ).X1;
					TempArray3 = TableData( TableNum ).X2;
					TempArray2.allocate( size( TableData( TableNum ).Y ) );
					for ( VarIndex = 1; VarIndex <= isize( TableData( TableNum ).Y ); ++VarIndex ) {
						TempArray2( VarIndex ) = TableData( TableNum ).Y( VarIndex );
					}
					SolveRegression( CurveNum, CurrentModuleObject, PerfCurve( CurveNum ).Name, TempArray1, TempArray2, TempArray3 );
					TempArray1.deallocate();
					TempArray2.deallocate();
					TempArray3.deallocate();
				} else {
					ShowWarningError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( "The requested regression analysis is not available at this time. Curve type = " + Alphas( 2 ) );
					PerfCurve( CurveIndex ).InterpolationType = LinearInterpolationOfTable;
				}}
			}

			// move table data to more compact array to allow interpolation using multivariable lookup table method
			TableLookup( TableNum ).NumIndependentVars = 2;
			TableLookup( TableNum ).NumX1Vars = size( PerfCurveTableData( TableNum ).X1 );
			TableLookup( TableNum ).NumX2Vars = size( PerfCurveTableData( TableNum ).X2 );
			TableLookup( TableNum ).X1Var.allocate( TableLookup( TableNum ).NumX1Vars );
			TableLookup( TableNum ).X2Var.allocate( TableLookup( TableNum ).NumX2Vars );
			TableLookup( TableNum ).TableLookupZData.allocate( 1, 1, 1, size( PerfCurveTableData( TableNum ).Y( _, 1 ) ), size( PerfCurveTableData( TableNum ).Y( 1, _ ) ) );
			TableLookup( TableNum ).X1Var = PerfCurveTableData( TableNum ).X1;
			TableLookup( TableNum ).X2Var = PerfCurveTableData( TableNum ).X2;
			TableLookup( TableNum ).TableLookupZData( 1, 1, 1, _, _ ) = PerfCurveTableData( TableNum ).Y( _, _ );

		}

		// Loop over multiple variable tables and load data (strict lookup only - no curve creation
		CurrentModuleObject = "Table:MultiVariableLookup";
		TableNum = NumTables;
		for ( CurveIndex = 1; CurveIndex <= NumMultVarLookup; ++CurveIndex ) {
			GetObjectItem( CurrentModuleObject, CurveIndex, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			++CurveNum;
			++TableNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PerfCurve, CurveNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Pressure Curves as well.
			if ( NumPressureCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PressureCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetCurveInput: " + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Pressure Curves. Names must be unique across all curves." );
					ErrorsFound = true;
				}
			}
			PerfCurve( CurveNum ).Name = Alphas( 1 );
			PerfCurve( CurveNum ).ObjectType = CurveType_TableMultiIV;
			PerfCurve( CurveNum ).TableIndex = TableNum;
			{ auto const SELECT_CASE_var( Alphas( 2 ) );
			if ( SELECT_CASE_var == "LINEARINTERPOLATIONOFTABLE" ) {
				PerfCurve( CurveNum ).InterpolationType = LinearInterpolationOfTable;
			} else if ( SELECT_CASE_var == "LAGRANGEINTERPOLATIONLINEAREXTRAPOLATION" ) {
				PerfCurve( CurveNum ).InterpolationType = LagrangeInterpolationLinearExtrapolation;
			} else if ( SELECT_CASE_var == "EVALUATECURVETOLIMITS" ) {
				PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
			} else {
				ShowSevereError( "GetTableInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cAlphaFieldNames( 2 ) + " [" + Alphas( 2 ) + "] is not a valid choice. " );
				ErrorsFound = true;
			}}

			{ auto const SELECT_CASE_var( Alphas( 3 ) );
			if ( SELECT_CASE_var == "LINEAR" ) {
				PerfCurve( CurveNum ).CurveType = Linear;
			} else if ( SELECT_CASE_var == "QUADRATIC" ) {
				PerfCurve( CurveNum ).CurveType = Quadratic;
			} else if ( SELECT_CASE_var == "CUBIC" ) {
				PerfCurve( CurveNum ).CurveType = Cubic;
			} else if ( SELECT_CASE_var == "QUARTIC" ) {
				PerfCurve( CurveNum ).CurveType = Quartic;
			} else if ( SELECT_CASE_var == "EXPONENT" ) {
				PerfCurve( CurveNum ).CurveType = Exponent;
			} else if ( SELECT_CASE_var == "BIQUADRATIC" ) {
				PerfCurve( CurveNum ).CurveType = BiQuadratic;
			} else if ( SELECT_CASE_var == "QUADRATICINEAR" ) {
				PerfCurve( CurveNum ).CurveType = QuadraticLinear;
			} else if ( SELECT_CASE_var == "BICUBIC" ) {
				PerfCurve( CurveNum ).CurveType = BiCubic;
			} else if ( SELECT_CASE_var == "TRIQUADRATIC" ) {
				PerfCurve( CurveNum ).CurveType = TriQuadratic;
			} else {
				ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cAlphaFieldNames( 3 ) + " [" + Alphas( 3 ) + "] is not a valid choice. " );
				ErrorsFound = true;
			}}

			{ auto const SELECT_CASE_var( Alphas( 4 ) );
			if ( SELECT_CASE_var == "SINGLELINEINDEPENDENTVARIABLEWITHMATRIX" ) {
				PerfCurve( CurveNum ).DataFormat = SINGLELINEINDEPENDENTVARIABLEWITHMATRIX;
			} else {
				ShowSevereError( "GetTableInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cAlphaFieldNames( 4 ) + " [" + Alphas( 4 ) + "] is not a valid choice. " );
				ErrorsFound = true;
			}}

			TableLookup( TableNum ).InterpolationOrder = Numbers( 1 );

			if ( ! lNumericFieldBlanks( 2 ) ) {
				TableData( TableNum ).NormalPoint = Numbers( 2 );
				if ( Numbers( 2 ) == 0.0 ) {
					ShowSevereError( "GetTableInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( "..." + cNumericFieldNames( 2 ) + " [" + RoundSigDigits( Numbers( 2 ), 6 ) + "] is not a valid choice." );
					ShowContinueError( "...Setting Normalization Reference to 1 and the simulation continues." );
					TableData( TableNum ).NormalPoint = 1.0;
				}
			} else {
				TableData( TableNum ).NormalPoint = 1.0;
			}
			PerfCurve( CurveNum ).Var1Min = Numbers( 3 );
			PerfCurve( CurveNum ).Var1Max = Numbers( 4 );
			PerfCurve( CurveNum ).Var2Min = Numbers( 5 );
			PerfCurve( CurveNum ).Var2Max = Numbers( 6 );
			PerfCurve( CurveNum ).Var3Min = Numbers( 7 );
			PerfCurve( CurveNum ).Var3Max = Numbers( 8 );
			PerfCurve( CurveNum ).Var4Min = Numbers( 9 );
			PerfCurve( CurveNum ).Var4Max = Numbers( 10 );
			PerfCurve( CurveNum ).Var5Min = Numbers( 11 );
			PerfCurve( CurveNum ).Var5Max = Numbers( 12 );

			if ( Numbers( 3 ) > Numbers( 4 ) ) { // error
				ShowSevereError( "GetTableInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 3 ) + " [" + RoundSigDigits( Numbers( 3 ), 2 ) + "] > " + cNumericFieldNames( 4 ) + " [" + RoundSigDigits( Numbers( 4 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 5 ) > Numbers( 6 ) ) { // error
				ShowSevereError( "GetTableInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 5 ) + " [" + RoundSigDigits( Numbers( 5 ), 2 ) + "] > " + cNumericFieldNames( 6 ) + " [" + RoundSigDigits( Numbers( 6 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 7 ) > Numbers( 8 ) ) { // error
				ShowSevereError( "GetTableInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 7 ) + " [" + RoundSigDigits( Numbers( 7 ), 2 ) + "] > " + cNumericFieldNames( 8 ) + " [" + RoundSigDigits( Numbers( 8 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 9 ) > Numbers( 10 ) ) { // error
				ShowSevereError( "GetTableInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 9 ) + " [" + RoundSigDigits( Numbers( 9 ), 2 ) + "] > " + cNumericFieldNames( 10 ) + " [" + RoundSigDigits( Numbers( 10 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( Numbers( 11 ) > Numbers( 12 ) ) { // error
				ShowSevereError( "GetTableInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( cNumericFieldNames( 11 ) + " [" + RoundSigDigits( Numbers( 11 ), 2 ) + "] > " + cNumericFieldNames( 12 ) + " [" + RoundSigDigits( Numbers( 12 ), 2 ) + ']' );
				ErrorsFound = true;
			}
			if ( NumAlphas >= 8 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 8 ) ) ) {
					ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( cAlphaFieldNames( 8 ) + " [" + Alphas( 8 ) + "] is invlaid" );
				}
			}
			if ( NumAlphas >= 9 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 9 ) ) ) {
					ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( cAlphaFieldNames( 9 ) + " [" + Alphas( 9 ) + "] is invlaid" );
				}
			}
			if ( NumAlphas >= 10 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 10 ) ) ) {
					ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( cAlphaFieldNames( 10 ) + " [" + Alphas( 10 ) + "] is invlaid" );
				}
			}
			if ( NumAlphas >= 11 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 11 ) ) ) {
					ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( cAlphaFieldNames( 11 ) + " [" + Alphas( 11 ) + "] is invlaid" );
				}
			}
			if ( NumAlphas >= 12 ) {
				if ( ! IsCurveInputTypeValid( Alphas( 12 ) ) ) {
					ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( cAlphaFieldNames( 12 ) + " [" + Alphas( 12 ) + "] is invlaid" );
				}
			}
			if ( NumAlphas >= 13 ) {
				if ( ! IsCurveOutputTypeValid( Alphas( 13 ) ) ) {
					ShowSevereError( "GetCurveInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( cAlphaFieldNames( 13 ) + " [" + Alphas( 13 ) + "] is invlaid" );
				}
			}

			if ( ! lNumericFieldBlanks( 13 ) ) {
				PerfCurve( CurveNum ).CurveMin = Numbers( 13 ) / TableData( TableNum ).NormalPoint;
				PerfCurve( CurveNum ).CurveMinPresent = true;
			}
			if ( ! lNumericFieldBlanks( 14 ) ) {
				PerfCurve( CurveNum ).CurveMax = Numbers( 14 ) / TableData( TableNum ).NormalPoint;
				PerfCurve( CurveNum ).CurveMaxPresent = true;
			}

			if ( Alphas( 6 ) == "ASCENDING" ) {
				PerfCurve( CurveNum ).X1SortOrder = ASCENDING;
			} else if ( Alphas( 6 ) == "DESCENDING" ) {
				PerfCurve( CurveNum ).X1SortOrder = DESCENDING;
			} else {
				ShowSevereError( "GetTableInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( "...Invalid " + cAlphaFieldNames( 6 ) + " = " + Alphas( 6 ) );
				ErrorsFound = true;
			}
			if ( Alphas( 7 ) == "ASCENDING" ) {
				PerfCurve( CurveNum ).X2SortOrder = ASCENDING;
			} else if ( Alphas( 7 ) == "DESCENDING" ) {
				PerfCurve( CurveNum ).X2SortOrder = DESCENDING;
			} else {
				ShowSevereError( "GetTableInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( "...Invalid " + cAlphaFieldNames( 7 ) + " = " + Alphas( 7 ) );
				ErrorsFound = true;
			}

			if ( ! lAlphaFieldBlanks( 5 ) ) {
				ReadFromFile = true;
				FileName = Alphas( 5 );
			} else {
				ReadFromFile = false;
				FileName = "";
			}

			ReadTableData( CurveNum, CurrentModuleObject, ReadFromFile, FileName, Alphas, Numbers, NumNumbers, ErrorsFound );

			if ( PerfCurve( CurveNum ).InterpolationType == EvaluateCurveToLimits ) {
				{ auto const SELECT_CASE_var( TableLookup( TableNum ).NumIndependentVars );
				if ( SELECT_CASE_var == 1 ) {
					TempArray1.allocate( size( TableLookup( TableNum ).TableLookupZData( 1, 1, 1, 1, _ ) ) );
					TempArray2.allocate( size( TempArray1 ) );
					TempArray1 = TableLookup( TableNum ).X1Var;
					TempArray2 = TableLookup( TableNum ).TableLookupZData( 1, 1, 1, 1, _ );
					SolveRegression( CurveNum, CurrentModuleObject, PerfCurve( CurveNum ).Name, TempArray1, TempArray2 );
					TempArray1.deallocate();
					TempArray2.deallocate();

					// Save array info in performance table arrays in case the performance table routine is selected in regression routine
					PerfCurveTableData( TableNum ).X1.allocate( size( TableLookup( TableNum ).X1Var ) );
					PerfCurveTableData( TableNum ).Y.allocate( 1, size( TableLookup( TableNum ).X1Var ) );
					PerfCurveTableData( TableNum ).X1 = TableLookup( TableNum ).X1Var;
					PerfCurveTableData( TableNum ).Y( 1, _ ) = TableLookup( TableNum ).TableLookupZData( 1, 1, 1, 1, _ );

				} else if ( SELECT_CASE_var == 2 ) {
					TempArray1.allocate( size( TableLookup( TableNum ).TableLookupZData( 1, 1, 1, _, _ ) ) );
					TempArray2.allocate( size( TempArray1 ) );
					TempArray3.allocate( size( TempArray1 ) );
					TableDataIndex = 0;
					for ( VarIndex = 1; VarIndex <= TableLookup( TableNum ).NumX1Vars; ++VarIndex ) {
						for ( TempVarIndex = 1; TempVarIndex <= TableLookup( TableNum ).NumX2Vars; ++TempVarIndex ) {
							++TableDataIndex;
							TempArray1( TableDataIndex ) = TableLookup( TableNum ).X1Var( VarIndex );
							TempArray2( TableDataIndex ) = TableLookup( TableNum ).X2Var( TempVarIndex );
							TempArray3( TableDataIndex ) = TableLookup( TableNum ).TableLookupZData( 1, 1, 1, TempVarIndex, VarIndex );
						}
					}
					SolveRegression( CurveNum, CurrentModuleObject, PerfCurve( CurveNum ).Name, TempArray1, TempArray3, TempArray2 );
					TempArray1.deallocate();
					TempArray2.deallocate();
					TempArray3.deallocate();
					// Save array info in performance table arrays in case the performance table routine is selected in regression routine
					PerfCurveTableData( TableNum ).X1.allocate( size( TableLookup( TableNum ).X1Var ) );
					PerfCurveTableData( TableNum ).X2.allocate( size( TableLookup( TableNum ).X2Var ) );
					PerfCurveTableData( TableNum ).Y.allocate( size( TableLookup( TableNum ).X2Var ), size( TableLookup( TableNum ).X1Var ) );
					PerfCurveTableData( TableNum ).X1 = TableLookup( TableNum ).X1Var;
					PerfCurveTableData( TableNum ).X2 = TableLookup( TableNum ).X2Var;
					PerfCurveTableData( TableNum ).Y( _, _ ) = TableLookup( TableNum ).TableLookupZData( 1, 1, 1, _, _ );
				} else {
					ShowSevereError( "GetTableInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( "...Invalid " + cAlphaFieldNames( 2 ) + " = " + Alphas( 2 ) );
					ShowContinueError( "...Choice not allowed with more than 2 indpendent variables." );
					ErrorsFound = true;
				}}
			} else {
				{ auto const SELECT_CASE_var( TableLookup( TableNum ).NumIndependentVars );
				if ( SELECT_CASE_var == 1 ) {
					// Save array info in performance table arrays in case the performance table routine is selected in regression routine
					PerfCurveTableData( TableNum ).X1.allocate( size( TableLookup( TableNum ).X1Var ) );
					PerfCurveTableData( TableNum ).Y.allocate( 1, size( TableLookup( TableNum ).X1Var ) );
					PerfCurveTableData( TableNum ).X1 = TableLookup( TableNum ).X1Var;
					PerfCurveTableData( TableNum ).Y( 1, _ ) = TableLookup( TableNum ).TableLookupZData( 1, 1, 1, 1, _ );
					// if linear interpolation of table is selected, switch interpolation type
				} else if ( SELECT_CASE_var == 2 ) {
					// Save array info in performance table arrays in case the performance table routine is selected in regression routine
					PerfCurveTableData( TableNum ).X1.allocate( size( TableLookup( TableNum ).X1Var ) );
					PerfCurveTableData( TableNum ).X2.allocate( size( TableLookup( TableNum ).X2Var ) );
					PerfCurveTableData( TableNum ).Y.allocate( size( TableLookup( TableNum ).X2Var ), size( TableLookup( TableNum ).X1Var ) );
					PerfCurveTableData( TableNum ).X1 = TableLookup( TableNum ).X1Var;
					PerfCurveTableData( TableNum ).X2 = TableLookup( TableNum ).X2Var;
					PerfCurveTableData( TableNum ).Y( _, _ ) = TableLookup( TableNum ).TableLookupZData( 1, 1, 1, _, _ );
					// if linear interpolation of table is selected, switch interpolation type
				} else {
					// if linear interpolation of table is selected, fatal if more than 2 independent variables
					if ( PerfCurve( CurveNum ).InterpolationType == LinearInterpolationOfTable ) {
						ShowSevereError( "GetTableInput: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
						ShowContinueError( "...Invalid " + cAlphaFieldNames( 2 ) + " = " + Alphas( 2 ) );
						ShowContinueError( "...Choice not allowed with more than 2 indpendent variables." );
						ErrorsFound = true;
					}
				}}
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "GetCurveInput: Errors found in getting Curve Objects.  Preceding condition(s) cause termination." );
		}

	}

	void
	InitCurveReporting()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Setting up of curve output variables caused errors in some files. Thus, separating the setup
		// from the getinput.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CurveIndex;

		for ( CurveIndex = 1; CurveIndex <= NumCurves; ++CurveIndex ) {
			{ auto const SELECT_CASE_var( PerfCurve( CurveIndex ).ObjectType );
			// CurrentModuleObject='Table:MultiVariableLookup'
			if ( SELECT_CASE_var == CurveType_TableMultiIV ) {
				{ auto const SELECT_CASE_var1( TableLookup( PerfCurve( CurveIndex ).TableIndex ).NumIndependentVars );
				if ( SELECT_CASE_var1 == 1 ) { //- 1 independent variable
					SetupOutputVariable( "Performance Curve Input Variable 1 Value []", PerfCurve( CurveIndex ).CurveInput1, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
				} else if ( SELECT_CASE_var1 == 2 ) { //- 2 independent variables
					SetupOutputVariable( "Performance Curve Input Variable 1 Value []", PerfCurve( CurveIndex ).CurveInput1, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 2 Value []", PerfCurve( CurveIndex ).CurveInput2, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
				} else if ( SELECT_CASE_var1 == 3 ) { //- 3 independent variables
					SetupOutputVariable( "Performance Curve Input Variable 1 Value []", PerfCurve( CurveIndex ).CurveInput1, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 2 Value []", PerfCurve( CurveIndex ).CurveInput2, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 3 Value []", PerfCurve( CurveIndex ).CurveInput3, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
				} else if ( SELECT_CASE_var1 == 4 ) { //- 4 independent variables
					SetupOutputVariable( "Performance Curve Input Variable 1 Value []", PerfCurve( CurveIndex ).CurveInput1, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 2 Value []", PerfCurve( CurveIndex ).CurveInput2, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 3 Value []", PerfCurve( CurveIndex ).CurveInput3, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 4 Value []", PerfCurve( CurveIndex ).CurveInput4, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
				} else if ( SELECT_CASE_var1 == 5 ) { //- 5 independent variables
					SetupOutputVariable( "Performance Curve Input Variable 1 Value []", PerfCurve( CurveIndex ).CurveInput1, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 2 Value []", PerfCurve( CurveIndex ).CurveInput2, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 3 Value []", PerfCurve( CurveIndex ).CurveInput3, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 4 Value []", PerfCurve( CurveIndex ).CurveInput4, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 5 Value []", PerfCurve( CurveIndex ).CurveInput5, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
				} else {
				}}
			} else if ( SELECT_CASE_var == CurveType_TableOneIV ) {
				// CurrentModuleObject='Table:OneIndependentVariable'
				SetupOutputVariable( "Performance Curve Input Variable 1 Value []", PerfCurve( CurveIndex ).CurveInput1, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
			} else if ( SELECT_CASE_var == CurveType_TableTwoIV ) {
				// CurrentModuleObject='Table:TwoIndependentVariables'
				SetupOutputVariable( "Performance Curve Input Variable 1 Value []", PerfCurve( CurveIndex ).CurveInput1, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
				SetupOutputVariable( "Performance Curve Input Variable 2 Value []", PerfCurve( CurveIndex ).CurveInput2, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
			} else {
				{ auto const SELECT_CASE_var1( PerfCurve( CurveIndex ).CurveType );
				if ( ( SELECT_CASE_var1 == Linear ) || ( SELECT_CASE_var1 == Quadratic ) || ( SELECT_CASE_var1 == Cubic ) || ( SELECT_CASE_var1 == Quartic ) || ( SELECT_CASE_var1 == Exponent ) || ( SELECT_CASE_var1 == FuncPressDrop ) ) {
					// CurrentModuleObject='Curve:Linear/Quadratic/Cubic/Quartic/Exponent/Functional:PressureDrop'
					SetupOutputVariable( "Performance Curve Input Variable 1 Value []", PerfCurve( CurveIndex ).CurveInput1, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
				} else if ( ( SELECT_CASE_var1 == BiQuadratic ) || ( SELECT_CASE_var1 == QuadraticLinear ) || ( SELECT_CASE_var1 == BiCubic ) || ( SELECT_CASE_var1 == CubicLinear ) ) {
					// CurrentModuleObject='Curve:BiQuadratic/QuadraticLinear/BiCubic/CubicLinear'
					SetupOutputVariable( "Performance Curve Input Variable 1 Value []", PerfCurve( CurveIndex ).CurveInput1, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 2 Value []", PerfCurve( CurveIndex ).CurveInput2, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
				} else if ( ( SELECT_CASE_var1 == TriQuadratic ) || ( SELECT_CASE_var1 == ChillerPartLoadWithLift ) ) {
					// CurrentModuleObject='Curve:TriQuadratic'
					SetupOutputVariable( "Performance Curve Input Variable 1 Value []", PerfCurve( CurveIndex ).CurveInput1, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 2 Value []", PerfCurve( CurveIndex ).CurveInput2, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 3 Value []", PerfCurve( CurveIndex ).CurveInput3, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
				} else if ( SELECT_CASE_var1 == QuadLinear ) {
					// CurrentModuleObject='Curve:QuadLinear'
					SetupOutputVariable( "Performance Curve Input Variable 1 Value []", PerfCurve( CurveIndex ).CurveInput1, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 2 Value []", PerfCurve( CurveIndex ).CurveInput2, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 3 Value []", PerfCurve( CurveIndex ).CurveInput3, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
					SetupOutputVariable( "Performance Curve Input Variable 4 Value []", PerfCurve( CurveIndex ).CurveInput4, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
				}}
			}}
			// set the output up last so it shows up after the input in the csv file
			SetupOutputVariable( "Performance Curve Output Value []", PerfCurve( CurveIndex ).CurveOutput, "HVAC", "Average", PerfCurve( CurveIndex ).Name );
		}

		for ( CurveIndex = 1; CurveIndex <= NumPressureCurves; ++CurveIndex ) {
			SetupOutputVariable( "Performance Curve Input Variable 1 Value []", PressureCurve( CurveIndex ).CurveInput1, "HVAC", "Average", PressureCurve( CurveIndex ).Name );
			SetupOutputVariable( "Performance Curve Input Variable 2 Value []", PressureCurve( CurveIndex ).CurveInput2, "HVAC", "Average", PressureCurve( CurveIndex ).Name );
			SetupOutputVariable( "Performance Curve Input Variable 3 Value []", PressureCurve( CurveIndex ).CurveInput3, "HVAC", "Average", PressureCurve( CurveIndex ).Name );
			SetupOutputVariable( "Performance Curve Output Value []", PressureCurve( CurveIndex ).CurveOutput, "HVAC", "Average", PressureCurve( CurveIndex ).Name );
		}

		if ( AnyEnergyManagementSystemInModel ) { // provide hook for possible EMS control
			for ( CurveIndex = 1; CurveIndex <= NumCurves; ++CurveIndex ) {
				SetupEMSActuator( "Curve", PerfCurve( CurveIndex ).Name, "Curve Result", "[unknown]", PerfCurve( CurveIndex ).EMSOverrideOn, PerfCurve( CurveIndex ).EMSOverrideCurveValue );
			} // All performance curves
		}
		if ( AnyEnergyManagementSystemInModel ) { // provide hook for possible EMS control
			for ( CurveIndex = 1; CurveIndex <= NumPressureCurves; ++CurveIndex ) {
				SetupEMSActuator( "Curve", PressureCurve( CurveIndex ).Name, "Curve Result", "[unknown]", PressureCurve( CurveIndex ).EMSOverrideOn, PressureCurve( CurveIndex ).EMSOverrideCurveValue );
			} // All pressure curves
		}

	}

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
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Given the curve index, read the table data.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataGlobals::DisplayAdvancedReportVariables;
		using DataGlobals::OutputFileInits;
		using DataSystemVariables::iUnicode_end;
		using DataSystemVariables::GoodIOStatValue;
		using DataSystemVariables::TempFullFileName;
		using DataSystemVariables::CheckForActualFileName;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// Argument array dimensioning

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int FileNum( 0 );
		int endcol;
		int TableNum;
		std::string NextLine; // Line of data
		int DataSetCount( 0 ); // counter for number of lines read (used in some error messages)
		int ReadStat; // File read status
		bool EOFonFile; // True if EOF during file read
		int I; // Do loop indexes and data set counter
		int J;
		int NumDataSets;
		Real64 Var3; // Temp variables for processing table lookup data
		Real64 Var4;
		Real64 Var5;
		int Var3Index;
		int Var4Index;
		int Var5Index;
		int NumIVars;
		int TotalDataSets;
		int NumbersOffset;
		int BaseOffset;
		static bool WriteHeaderOnce( true ); // eio header file write flag
		std::string CharTableData; // used to echo each line of table data read in to eio file
		bool EchoTableDataToEio; // logical set equal to global and used to report to eio file
		bool FileExists;
		IOFlags non_adv; non_adv.na_on(); // For non-advancing list-directed output

		// Formats
		static gio::Fmt Format_140( "('! Reading external file tabular data for ',A,' \"',A,'\"')" );
		static gio::Fmt Format_150( "('! Reading tabular data for ',A,' \"',A,'\"')" );
		static gio::Fmt Format_110( "('! <READING LOOKUP TABLE DATA>')" );
		static gio::Fmt Format_130( "('READING LOOKUP TABLE DATA')" );
		static gio::Fmt Format_131( "('END READING LOOKUP TABLE DATA')" );
		static gio::Fmt Format_160( "(1X,10(I2,:,2X))" );

		//Autodesk:Uninit Initialize variables used uninitialized
		TotalDataSets = 0; //Autodesk:Uninit Force default initialization

		EchoTableDataToEio = DisplayAdvancedReportVariables;
		TableNum = PerfCurve( CurveNum ).TableIndex;

		if ( ReadFromFile ) {
			CheckForActualFileName( FileName, FileExists, TempFullFileName );
			if ( ! FileExists ) goto Label999;
			FileNum = GetNewUnitNumber();
			{ IOFlags flags; flags.ACTION( "read" ); gio::open( FileNum, TempFullFileName, flags ); if ( flags.err() ) goto Label999; }
			gio::read( FileNum, fmtA ) >> NextLine; trim( NextLine );
			endcol = len( NextLine );
			if ( endcol == 0 ) {
				ShowWarningError( "ReadTableData: Blank line found in external file = " + FileName );
				ShowContinueError( "...Blank lines are not allowed. Will try to read next line." );
				gio::read( FileNum, fmtA ) >> NextLine; trim( NextLine );
				endcol = len( NextLine );
				if ( endcol == 0 ) {
					ShowWarningError( "ReadTableData: Data not found on second line in external file = " + FileName );
					ShowContinueError( "...Check that file is ASCII text and that file is not locked by other applications." );
					ShowFatalError( "External table data not found. Simulation will terminate." );
				} else {
					ShowWarningError( "Second read attempt found data in external file = " + FileName );
					ShowFatalError( "...Blank lines are not allowed. Simulation will terminate." );
				}
			}
			if ( endcol > 0 ) {
				if ( int( NextLine[ endcol - 1 ] ) == iUnicode_end ) {
					ShowSevereError( "ReadTableData: For Table:MultiVariableLookup \"" + PerfCurve( CurveNum ).Name + "\" external file, appears to be a Unicode or binary file." );
					ShowContinueError( "...This file cannot be read by this program. Please save as PC or Unix file and try again" );
					ShowFatalError( "Program terminates due to previous condition." );
				}
			}

			gio::rewind( FileNum );

			{ IOFlags flags; gio::read( FileNum, fmtLD, flags ) >> NumIVars; ReadStat = flags.ios(); }
			if ( NumIVars > 5 || NumIVars < 1 ) {
				ShowSevereError( "ReadTableData: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( "...Invalid number of independent variables found in external file = " + FileName );
				ShowFatalError( "...Only 1 to 5 independent variables are allowed." );
			}

			gio::rewind( FileNum );

			if ( NumIVars == 1 ) { IOFlags flags; gio::read( FileNum, fmtLD, flags ) >> NumIVars >> TableLookup( TableNum ).NumX1Vars; ReadStat = flags.ios(); };
			if ( NumIVars == 2 ) { IOFlags flags; gio::read( FileNum, fmtLD, flags ) >> NumIVars >> TableLookup( TableNum ).NumX1Vars >> TableLookup( TableNum ).NumX2Vars; ReadStat = flags.ios(); };
			if ( NumIVars == 3 ) { IOFlags flags; gio::read( FileNum, fmtLD, flags ) >> NumIVars >> TableLookup( TableNum ).NumX1Vars >> TableLookup( TableNum ).NumX2Vars >> TableLookup( TableNum ).NumX3Vars; ReadStat = flags.ios(); };
			if ( NumIVars == 4 ) { IOFlags flags; gio::read( FileNum, fmtLD, flags ) >> NumIVars >> TableLookup( TableNum ).NumX1Vars >> TableLookup( TableNum ).NumX2Vars >> TableLookup( TableNum ).NumX3Vars >> TableLookup( TableNum ).NumX4Vars; ReadStat = flags.ios(); };
			if ( NumIVars == 5 ) { IOFlags flags; gio::read( FileNum, fmtLD, flags ) >> NumIVars >> TableLookup( TableNum ).NumX1Vars >> TableLookup( TableNum ).NumX2Vars >> TableLookup( TableNum ).NumX3Vars >> TableLookup( TableNum ).NumX4Vars >> TableLookup( TableNum ).NumX5Vars; ReadStat = flags.ios(); };

			if ( ReadStat < GoodIOStatValue ) goto Label1000; //Autodesk:Uninit TotalDataSets was uninitialized after goto jump

			TableLookup( TableNum ).NumIndependentVars = NumIVars;
			// Echo table data for user verification
			if ( EchoTableDataToEio ) {
				if ( WriteHeaderOnce ) {
					gio::write( OutputFileInits, Format_110 );
					WriteHeaderOnce = false;
				}
				gio::write( OutputFileInits, Format_130 );
				gio::write( OutputFileInits, Format_140 ) << CurrentModuleObject << Alphas( 1 );
			}
		} else {
			if ( EchoTableDataToEio ) {
				if ( WriteHeaderOnce ) {
					gio::write( OutputFileInits, Format_110 );
					WriteHeaderOnce = false;
				}
				gio::write( OutputFileInits, Format_130 );
				gio::write( OutputFileInits, Format_150 ) << CurrentModuleObject << Alphas( 1 );
			}
			NumIVars = Numbers( 15 );
			if ( NumIVars > 5 || NumIVars < 1 ) {
				ShowSevereError( "ReadTableData: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
				ShowContinueError( "...Invalid number of independent variables." );
				ShowFatalError( "...Only 1 to 5 independent variables are allowed." );
			}

			BaseOffset = 15;
			TableLookup( TableNum ).NumIndependentVars = NumIVars;
			TableLookup( TableNum ).NumX1Vars = Numbers( 16 );
			if ( NumIVars > 1 ) TableLookup( TableNum ).NumX2Vars = Numbers( 17 );
			if ( NumIVars > 2 ) TableLookup( TableNum ).NumX3Vars = Numbers( 18 );
			if ( NumIVars > 3 ) TableLookup( TableNum ).NumX4Vars = Numbers( 19 );
			if ( NumIVars > 4 ) TableLookup( TableNum ).NumX5Vars = Numbers( 20 );
		}

		if ( EchoTableDataToEio ) {
			if ( NumIVars == 1 ) {
				gio::write( CharTableData, Format_160 ) << NumIVars << TableLookup( TableNum ).NumX1Vars;
			} else if ( NumIVars == 2 ) {
				gio::write( CharTableData, Format_160 ) << NumIVars << TableLookup( TableNum ).NumX1Vars << TableLookup( TableNum ).NumX2Vars;
			} else if ( NumIVars == 3 ) {
				gio::write( CharTableData, Format_160 ) << NumIVars << TableLookup( TableNum ).NumX1Vars << TableLookup( TableNum ).NumX2Vars << TableLookup( TableNum ).NumX3Vars;
			} else if ( NumIVars == 4 ) {
				gio::write( CharTableData, Format_160 ) << NumIVars << TableLookup( TableNum ).NumX1Vars << TableLookup( TableNum ).NumX2Vars << TableLookup( TableNum ).NumX3Vars << TableLookup( TableNum ).NumX4Vars;
			} else {
				gio::write( CharTableData, Format_160 ) << NumIVars << TableLookup( TableNum ).NumX1Vars << TableLookup( TableNum ).NumX2Vars << TableLookup( TableNum ).NumX3Vars << TableLookup( TableNum ).NumX4Vars << TableLookup( TableNum ).NumX5Vars;
			}

			gio::write( OutputFileInits, fmtA ) << trim( CharTableData );
		}

		TableLookup( TableNum ).X1Var.allocate( TableLookup( TableNum ).NumX1Vars );
		TableLookup( TableNum ).X2Var.allocate( TableLookup( TableNum ).NumX2Vars );
		TableLookup( TableNum ).X3Var.allocate( TableLookup( TableNum ).NumX3Vars );
		TableLookup( TableNum ).X4Var.allocate( TableLookup( TableNum ).NumX4Vars );
		TableLookup( TableNum ).X5Var.allocate( TableLookup( TableNum ).NumX5Vars );

		if ( NumIVars > 0 ) {
			if ( ReadFromFile ) {

				{
					IOFlags flags;
					for ( I = 1; I <= TableLookup( TableNum ).NumX1Vars; ++I ) {
						gio::read( FileNum, fmtLD, flags ) >> TableLookup( TableNum ).X1Var( I );
					}
					ReadStat = flags.ios();
				}

				if ( EchoTableDataToEio ) {
					for ( I = 1; I <= TableLookup( TableNum ).NumX1Vars; ++I ) {
						gio::write( OutputFileInits, fmtLD, non_adv ) << TableLookup( TableNum ).X1Var( I );
					} gio::write( OutputFileInits );
				}

				if ( ReadStat < GoodIOStatValue ) goto Label1000;

			} else {

				if ( NumNumbers >= BaseOffset + NumIVars + TableLookup( TableNum ).NumX1Vars ) {
					for ( I = 1; I <= TableLookup( TableNum ).NumX1Vars; ++I ) {
						TableLookup( TableNum ).X1Var( I ) = Numbers( BaseOffset + NumIVars + I );
					}

					if ( EchoTableDataToEio ) {
						for ( I = 1; I <= TableLookup( TableNum ).NumX1Vars; ++I ) {
							gio::write( OutputFileInits, fmtLD, non_adv ) << TableLookup( TableNum ).X1Var( I );
						} gio::write( OutputFileInits );
					}

				} else {
					ShowSevereError( "ReadTableData: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( "...The number of numeric inputs is less than expected." );
					ErrorsFound = true;
				}
			}

			// check to make sure XVars are in increaseing (ascending) order
			for ( I = 1; I <= TableLookup( TableNum ).NumX1Vars; ++I ) {
				if ( I == 1 ) continue;
				if ( TableLookup( TableNum ).X1Var( I ) <= TableLookup( TableNum ).X1Var( I - 1 ) ) {
					ShowSevereError( "ReadTableData: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
					ShowContinueError( "...The values for the independent variable X1 must be in increasing (ascending) order." );
					ErrorsFound = true;
					break;
				}
			}

			if ( NumIVars > 1 ) {
				if ( ReadFromFile ) {
					{
						IOFlags flags;
						for ( I = 1; I <= TableLookup( TableNum ).NumX2Vars; ++I ) {
							gio::read( FileNum, fmtLD, flags ) >> TableLookup( TableNum ).X2Var( I );
						}
						ReadStat = flags.ios();
					}

					if ( EchoTableDataToEio ) {
						for ( I = 1; I <= TableLookup( TableNum ).NumX2Vars; ++I ) {
							gio::write( OutputFileInits, fmtLD, non_adv ) << TableLookup( TableNum ).X2Var( I );
						} gio::write( OutputFileInits );
					}

					if ( ReadStat < GoodIOStatValue ) goto Label1000;

				} else {
					if ( NumNumbers >= BaseOffset + NumIVars + TableLookup( TableNum ).NumX1Vars + TableLookup( TableNum ).NumX2Vars ) {
						for ( I = 1; I <= TableLookup( TableNum ).NumX2Vars; ++I ) {
							TableLookup( TableNum ).X2Var( I ) = Numbers( BaseOffset + NumIVars + TableLookup( TableNum ).NumX1Vars + I );
						}

						if ( EchoTableDataToEio ) {
							for ( I = 1; I <= TableLookup( TableNum ).NumX2Vars; ++I ) {
								gio::write( OutputFileInits, fmtLD, non_adv ) << TableLookup( TableNum ).X2Var( I );
							} gio::write( OutputFileInits );
						}

					} else {
						ShowSevereError( "ReadTableData: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
						ShowContinueError( "...The number of numeric inputs is less than expected." );
						ErrorsFound = true;
					}
				}

				// check to make sure XVars are in increaseing (ascending) order
				for ( I = 1; I <= TableLookup( TableNum ).NumX2Vars; ++I ) {
					if ( I == 1 ) continue;
					if ( TableLookup( TableNum ).X2Var( I ) <= TableLookup( TableNum ).X2Var( I - 1 ) ) {
						ShowSevereError( "ReadTableData: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
						ShowContinueError( "...The values for the independent variable X2 must be in increasing (ascending) order." );
						ErrorsFound = true;
						break;
					}
				}

				if ( NumIVars > 2 ) {
					if ( ReadFromFile ) {
						{
							IOFlags flags;
							for ( I = 1; I <= TableLookup( TableNum ).NumX3Vars; ++I ) {
								gio::read( FileNum, fmtLD, flags ) >> TableLookup( TableNum ).X3Var( I );
							}
							ReadStat = flags.ios();
						}

						if ( EchoTableDataToEio ) {
							for ( I = 1; I <= TableLookup( TableNum ).NumX3Vars; ++I ) {
								gio::write( OutputFileInits, fmtLD, non_adv ) << TableLookup( TableNum ).X3Var( I );
							} gio::write( OutputFileInits );
						}

						if ( ReadStat < GoodIOStatValue ) goto Label1000;

					} else {
						if ( NumNumbers >= BaseOffset + NumIVars + TableLookup( TableNum ).NumX1Vars + TableLookup( TableNum ).NumX2Vars + TableLookup( TableNum ).NumX3Vars ) {

							for ( I = 1; I <= TableLookup( TableNum ).NumX3Vars; ++I ) {
								TableLookup( TableNum ).X3Var( I ) = Numbers( BaseOffset + NumIVars + TableLookup( TableNum ).NumX1Vars + TableLookup( TableNum ).NumX2Vars + I );
							}

							if ( EchoTableDataToEio ) {
								for ( I = 1; I <= TableLookup( TableNum ).NumX3Vars; ++I ) {
									gio::write( OutputFileInits, fmtLD, non_adv ) << TableLookup( TableNum ).X3Var( I );
								} gio::write( OutputFileInits );
							}

						} else {
							ShowSevereError( "ReadTableData: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
							ShowContinueError( "...The number of numeric inputs is less than expected." );
							ErrorsFound = true;
						}
					}

					// check to make sure XVars are in increasing (ascending) order
					for ( I = 1; I <= TableLookup( TableNum ).NumX3Vars; ++I ) {
						if ( I == 1 ) continue;
						if ( TableLookup( TableNum ).X3Var( I ) <= TableLookup( TableNum ).X3Var( I - 1 ) ) {
							ShowSevereError( "ReadTableData: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
							ShowContinueError( "...The values for the independent variable X3 must be in increasing (ascending) order." );
							ErrorsFound = true;
							break;
						}
					}

					if ( NumIVars > 3 ) {
						if ( ReadFromFile ) {

							{
								IOFlags flags;

								for ( I = 1; I <= TableLookup( TableNum ).NumX4Vars; ++I ) {
									gio::read( FileNum, fmtLD, flags ) >> TableLookup( TableNum ).X4Var( I );
								}
								ReadStat = flags.ios();
							}

							if ( EchoTableDataToEio ) {
								TableLookup( TableNum ).X4Var( I );
								for ( I = 1; I <= TableLookup( TableNum ).NumX4Vars; ++I ) {
									gio::write( OutputFileInits, fmtLD, non_adv ) << TableLookup( TableNum ).X4Var( I );
								} gio::write( OutputFileInits );
							}

							if ( ReadStat < GoodIOStatValue ) goto Label1000;

						} else {
							if ( NumNumbers >= BaseOffset + NumIVars + TableLookup( TableNum ).NumX1Vars + TableLookup( TableNum ).NumX2Vars + TableLookup( TableNum ).NumX3Vars + TableLookup( TableNum ).NumX4Vars ) {

								for ( I = 1; I <= TableLookup( TableNum ).NumX4Vars; ++I ) {
									TableLookup( TableNum ).X4Var( I ) = Numbers( BaseOffset + NumIVars + TableLookup( TableNum ).NumX1Vars + TableLookup( TableNum ).NumX2Vars + TableLookup( TableNum ).NumX3Vars + I );
								}

								if ( EchoTableDataToEio ) {
									for ( I = 1; I <= TableLookup( TableNum ).NumX4Vars; ++I ) {
										gio::write( OutputFileInits, fmtLD, non_adv ) << TableLookup( TableNum ).X4Var( I );
									} gio::write( OutputFileInits );
								}

							} else {
								ShowSevereError( "ReadTableData: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
								ShowContinueError( "...The number of numeric inputs is less than expected." );
								ErrorsFound = true;
							}
						}

						// check to make sure XVars are in increaseing (ascending) order
						for ( I = 1; I <= TableLookup( TableNum ).NumX4Vars; ++I ) {
							if ( I == 1 ) continue;
							if ( TableLookup( TableNum ).X4Var( I ) <= TableLookup( TableNum ).X4Var( I - 1 ) ) {
								ShowSevereError( "ReadTableData: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
								ShowContinueError( "...The values for the independent variable X4 must be in increasing (ascending) order." );
								ErrorsFound = true;
								break;
							}
						}

						if ( NumIVars > 4 ) {
							if ( ReadFromFile ) {
								{
									IOFlags flags;
									for ( I = 1; I <= TableLookup( TableNum ).NumX5Vars; ++I ) {
										gio::read( FileNum, fmtLD, flags ) >> TableLookup( TableNum ).X5Var( I );
									}
									ReadStat = flags.ios();
								}

								if ( EchoTableDataToEio ) {
									for ( I = 1; I <= TableLookup( TableNum ).NumX5Vars; ++I ) {
										gio::write( OutputFileInits, fmtLD, non_adv ) << TableLookup( TableNum ).X5Var( I );
									} gio::write( OutputFileInits );
								}

								if ( ReadStat < GoodIOStatValue ) goto Label1000;

							} else {
								if ( NumNumbers >= BaseOffset + NumIVars + TableLookup( TableNum ).NumX1Vars + TableLookup( TableNum ).NumX2Vars + TableLookup( TableNum ).NumX3Vars + TableLookup( TableNum ).NumX4Vars + TableLookup( TableNum ).NumX5Vars ) {

									for ( I = 1; I <= TableLookup( TableNum ).NumX5Vars; ++I ) {
										TableLookup( TableNum ).X5Var( I ) = Numbers( BaseOffset + NumIVars + TableLookup( TableNum ).NumX1Vars + TableLookup( TableNum ).NumX2Vars + TableLookup( TableNum ).NumX3Vars + TableLookup( TableNum ).NumX4Vars + I );
									}

									if ( EchoTableDataToEio ) {
										for ( I = 1; I <= TableLookup( TableNum ).NumX5Vars; ++I ) {
											gio::write( OutputFileInits, fmtLD, non_adv ) << TableLookup( TableNum ).X5Var( I );
										} gio::write( OutputFileInits );
									}

								} else {
									ShowSevereError( "ReadTableData: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
									ShowContinueError( "...The number of numeric inputs is less than expected." );
									ErrorsFound = true;
								}
							}
							// check to make sure XVars are in increaseing (ascending) order
							for ( I = 1; I <= TableLookup( TableNum ).NumX5Vars; ++I ) {
								if ( I == 1 ) continue;
								if ( TableLookup( TableNum ).X5Var( I ) <= TableLookup( TableNum ).X5Var( I - 1 ) ) {
									ShowSevereError( "ReadTableData: For " + CurrentModuleObject + ": " + Alphas( 1 ) );
									ShowContinueError( "...The values for the independent variable X5 must be in increasing (ascending) order." );
									ErrorsFound = true;
									break;
								}
							}

							TableLookup( TableNum ).TableLookupZData.allocate( TableLookup( TableNum ).NumX5Vars, TableLookup( TableNum ).NumX4Vars, TableLookup( TableNum ).NumX3Vars, TableLookup( TableNum ).NumX2Vars, TableLookup( TableNum ).NumX1Vars );
						} else {
							TableLookup( TableNum ).TableLookupZData.allocate( 1, TableLookup( TableNum ).NumX4Vars, TableLookup( TableNum ).NumX3Vars, TableLookup( TableNum ).NumX2Vars, TableLookup( TableNum ).NumX1Vars );
						}
					} else {
						TableLookup( TableNum ).TableLookupZData.allocate( 1, 1, TableLookup( TableNum ).NumX3Vars, TableLookup( TableNum ).NumX2Vars, TableLookup( TableNum ).NumX1Vars );
					}
				} else {
					TableLookup( TableNum ).TableLookupZData.allocate( 1, 1, 1, TableLookup( TableNum ).NumX2Vars, TableLookup( TableNum ).NumX1Vars );
				}
			} else {
				TableLookup( TableNum ).TableLookupZData.allocate( 1, 1, 1, 1, TableLookup( TableNum ).NumX1Vars );
			}
		}

		TotalDataSets = 1;
		DataSetCount = 0;
		if ( NumIVars == 3 ) TotalDataSets = TableLookup( TableNum ).NumX3Vars;
		if ( NumIVars == 4 ) TotalDataSets = TableLookup( TableNum ).NumX3Vars * TableLookup( TableNum ).NumX4Vars;
		if ( NumIVars == 5 ) TotalDataSets = TableLookup( TableNum ).NumX3Vars * TableLookup( TableNum ).NumX4Vars * TableLookup( TableNum ).NumX5Vars;

		NumbersOffset = 15 + NumIVars + TableLookup( TableNum ).NumX1Vars + TableLookup( TableNum ).NumX2Vars + TableLookup( TableNum ).NumX3Vars + TableLookup( TableNum ).NumX4Vars + TableLookup( TableNum ).NumX5Vars + 1;

		// initialize NumX2Vars to 1 so the DO loops work correctly
		if ( NumIVars == 1 ) TableLookup( TableNum ).NumX2Vars = 1;

		for ( NumDataSets = 1; NumDataSets <= TotalDataSets; ++NumDataSets ) {

			if ( NumIVars == 3 ) {
				if ( ReadFromFile ) {

					{ IOFlags flags; gio::read( FileNum, fmtLD, flags ) >> Var3; ReadStat = flags.ios(); }

					if ( EchoTableDataToEio ) {
						gio::write( CharTableData, fmtLD ) << Var3;
						gio::write( OutputFileInits, fmtA ) << trim( CharTableData );
					}

					if ( ReadStat < GoodIOStatValue ) goto Label1000;

				} else {
					Var3 = Numbers( NumbersOffset );
					++NumbersOffset;

					if ( EchoTableDataToEio ) {
						gio::write( CharTableData, fmtLD ) << Var3;
						gio::write( OutputFileInits, fmtA ) << trim( CharTableData );
					}

				}
			} else if ( NumIVars == 4 ) {
				if ( ReadFromFile ) {

					{ IOFlags flags; gio::read( FileNum, fmtLD, flags ) >> Var3 >> Var4; ReadStat = flags.ios(); }

					if ( EchoTableDataToEio ) {
						gio::write( CharTableData, fmtLD ) << Var3 << "  " << Var4;
						gio::write( OutputFileInits, fmtA ) << trim( CharTableData );
					}

					if ( ReadStat < GoodIOStatValue ) goto Label1000;

				} else {
					Var3 = Numbers( NumbersOffset );
					Var4 = Numbers( NumbersOffset + 1 );
					NumbersOffset += 2;

					if ( EchoTableDataToEio ) {
						gio::write( CharTableData, fmtLD ) << Var3 << "  " << Var4;
						gio::write( OutputFileInits, fmtA ) << trim( CharTableData );
					}

				}
			} else if ( NumIVars == 5 ) {
				if ( ReadFromFile ) {

					{ IOFlags flags; gio::read( FileNum, fmtLD, flags ) >> Var3 >> Var4 >> Var5; ReadStat = flags.ios(); }

					if ( EchoTableDataToEio ) {
						gio::write( CharTableData, fmtLD ) << Var3 << "  " << Var4 << "  " << Var5;
						gio::write( OutputFileInits, fmtA ) << trim( CharTableData );
					}

					if ( ReadStat < GoodIOStatValue ) goto Label1000;

				} else {
					Var3 = Numbers( NumbersOffset );
					Var4 = Numbers( NumbersOffset + 1 );
					Var5 = Numbers( NumbersOffset + 2 );
					NumbersOffset += 3;

					if ( EchoTableDataToEio ) {
						gio::write( CharTableData, fmtLD ) << Var3 << "  " << Var4 << "  " << Var5;
						gio::write( OutputFileInits, fmtA ) << trim( CharTableData );
					}

				}
			}

			if ( NumIVars > 2 ) {
				Var3Index = 0;
				// match the independent variable values to the allowed values to find the index. Input must match allowed values.
				for ( I = 1; I <= TableLookup( TableNum ).NumX3Vars; ++I ) {
					if ( Var3 != TableLookup( TableNum ).X3Var( I ) ) continue;
					Var3Index = I;
					break;
				}
				if ( Var3Index == 0 ) {
					ShowSevereError( "GetTableDataFile: For Table:MultiVariableLookup \"" + PerfCurve( CurveNum ).Name + "\"" );
					ShowContinueError( "...The value of the 3rd independent variable (" + RoundSigDigits( Var3, 9 ) + ") does not match the values listed as valid entries for this independent variable." );
					ShowContinueError( "...Valid entries are: " );
					if ( TableLookup( TableNum ).NumX3Vars >= 1 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X3Var( 1 ), 9 ) );
					if ( TableLookup( TableNum ).NumX3Vars >= 2 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X3Var( 2 ), 9 ) );
					if ( TableLookup( TableNum ).NumX3Vars >= 3 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X3Var( 3 ), 9 ) );
					if ( TableLookup( TableNum ).NumX3Vars >= 4 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X3Var( 4 ), 9 ) );
					if ( TableLookup( TableNum ).NumX3Vars >= 5 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X3Var( 5 ), 9 ) );
					if ( TableLookup( TableNum ).NumX3Vars >= 6 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X3Var( 6 ), 9 ) );
					if ( TableLookup( TableNum ).NumX3Vars >= 7 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X3Var( 7 ), 9 ) );
					if ( TableLookup( TableNum ).NumX3Vars >= 8 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X3Var( 8 ), 9 ) );
					if ( TableLookup( TableNum ).NumX3Vars >= 9 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X3Var( 9 ), 9 ) );
					if ( TableLookup( TableNum ).NumX3Vars >= 10 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X3Var( 10 ), 9 ) );
					ShowContinueError( "...This occurs for data set = " + RoundSigDigits( DataSetCount + 1 ) + " in data file = " + FileName );
					ErrorsFound = true;
					Var3Index = 1;
				}
			} else {
				Var3Index = 1;
			}

			if ( NumIVars > 3 ) {
				Var4Index = 0;
				// match the independent variable values to the allowed values to find the index. Input must match allowed values.
				for ( I = 1; I <= TableLookup( TableNum ).NumX4Vars; ++I ) {
					if ( Var4 != TableLookup( TableNum ).X4Var( I ) ) continue;
					Var4Index = I;
					break;
				}
				if ( Var4Index == 0 ) {
					ShowSevereError( "GetTableDataFile: For Table:MultiVariableLookup \"" + PerfCurve( CurveNum ).Name + "\"" );
					ShowContinueError( "...The value of the 4th independent variable (" + RoundSigDigits( Var4, 9 ) + ") does not match the values listed as valid entries for this independent variable." );
					ShowContinueError( "...Valid entries are: " );
					if ( TableLookup( TableNum ).NumX4Vars >= 1 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X4Var( 1 ), 9 ) );
					if ( TableLookup( TableNum ).NumX4Vars >= 2 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X4Var( 2 ), 9 ) );
					if ( TableLookup( TableNum ).NumX4Vars >= 3 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X4Var( 3 ), 9 ) );
					if ( TableLookup( TableNum ).NumX4Vars >= 4 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X4Var( 4 ), 9 ) );
					if ( TableLookup( TableNum ).NumX4Vars >= 5 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X4Var( 5 ), 9 ) );
					if ( TableLookup( TableNum ).NumX4Vars >= 6 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X4Var( 6 ), 9 ) );
					if ( TableLookup( TableNum ).NumX4Vars >= 7 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X4Var( 7 ), 9 ) );
					if ( TableLookup( TableNum ).NumX4Vars >= 8 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X4Var( 8 ), 9 ) );
					if ( TableLookup( TableNum ).NumX4Vars >= 9 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X4Var( 9 ), 9 ) );
					if ( TableLookup( TableNum ).NumX4Vars >= 10 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X4Var( 10 ), 9 ) );
					ShowContinueError( "...This occurs for data set = " + RoundSigDigits( DataSetCount + 1 ) + " in data file = " + FileName );
					ErrorsFound = true;
					Var4Index = 1;
				}
			} else {
				Var4Index = 1;
			}

			if ( NumIVars > 4 ) {
				Var5Index = 0;
				// match the independent variable values to the allowed values to find the index. Input must match allowed values.
				for ( I = 1; I <= TableLookup( TableNum ).NumX5Vars; ++I ) {
					if ( Var5 != TableLookup( TableNum ).X5Var( I ) ) continue;
					Var5Index = I;
					break;
				}
				if ( Var5Index == 0 && NumIVars > 4 ) {
					ShowSevereError( "GetTableDataFile: For Table:MultiVariableLookup \"" + PerfCurve( CurveNum ).Name + "\"" );
					ShowContinueError( "...The value of the 5th independent variable (" + RoundSigDigits( Var5, 9 ) + ") does not match the values listed as valid entries for this independent variable." );
					ShowContinueError( "...Valid entries are: " );
					if ( TableLookup( TableNum ).NumX5Vars >= 1 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X5Var( 1 ), 9 ) );
					if ( TableLookup( TableNum ).NumX5Vars >= 2 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X5Var( 2 ), 9 ) );
					if ( TableLookup( TableNum ).NumX5Vars >= 3 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X5Var( 3 ), 9 ) );
					if ( TableLookup( TableNum ).NumX5Vars >= 4 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X5Var( 4 ), 9 ) );
					if ( TableLookup( TableNum ).NumX5Vars >= 5 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X5Var( 5 ), 9 ) );
					if ( TableLookup( TableNum ).NumX5Vars >= 6 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X5Var( 6 ), 9 ) );
					if ( TableLookup( TableNum ).NumX5Vars >= 7 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X5Var( 7 ), 9 ) );
					if ( TableLookup( TableNum ).NumX5Vars >= 8 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X5Var( 8 ), 9 ) );
					if ( TableLookup( TableNum ).NumX5Vars >= 9 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X5Var( 9 ), 9 ) );
					if ( TableLookup( TableNum ).NumX5Vars >= 10 ) ShowContinueError( "..." + RoundSigDigits( TableLookup( TableNum ).X5Var( 10 ), 9 ) );
					ShowContinueError( "...This occurs for data set = " + RoundSigDigits( DataSetCount + 1 ) + " in data file = " + FileName );
					ErrorsFound = true;
					Var5Index = 1;
				}
			} else {
				Var5Index = 1;
			}

			// now read in X1 | X2 matrix data set
			if ( PerfCurve( CurveNum ).X1SortOrder == ASCENDING ) {
				if ( PerfCurve( CurveNum ).X2SortOrder == ASCENDING ) {
					for ( J = 1; J <= TableLookup( TableNum ).NumX2Vars; ++J ) {
						if ( ReadFromFile ) {

							{
								IOFlags flags;
								for ( I = 1; I <= TableLookup( TableNum ).NumX1Vars; ++I ) {
									gio::read( FileNum, fmtLD, flags ) >> TableLookup( TableNum ).TableLookupZData( Var5Index, Var4Index, Var3Index, J, I );
								}
								ReadStat = flags.ios();
							}

							if ( ReadStat < GoodIOStatValue ) goto Label1000;

						} else {

							for ( I = 1; I <= TableLookup( TableNum ).NumX1Vars; ++I ) {
								TableLookup( TableNum ).TableLookupZData( Var5Index, Var4Index, Var3Index, J, I ) = Numbers( NumbersOffset );
								++NumbersOffset;
							}

						}
					}
				} else { // PerfCurve(CurveNum)%X2SortOrder == DESCENDING

					for ( J = TableLookup( TableNum ).NumX2Vars; J >= 1; --J ) {

						if ( ReadFromFile ) {

							{
								IOFlags flags;
								for ( I = 1; I <= TableLookup( TableNum ).NumX1Vars; ++I ) {
									gio::read( FileNum, fmtLD, flags ) >> TableLookup( TableNum ).TableLookupZData( Var5Index, Var4Index, Var3Index, J, I );
								}
								ReadStat = flags.ios();
							}

							if ( ReadStat < GoodIOStatValue ) goto Label1000;

						} else {

							for ( I = 1; I <= TableLookup( TableNum ).NumX1Vars; ++I ) {
								TableLookup( TableNum ).TableLookupZData( Var5Index, Var4Index, Var3Index, J, I ) = Numbers( NumbersOffset ) / TableData( TableNum ).NormalPoint;
								++NumbersOffset;
							}

						}
					}
				}
			} else { // PerfCurve(CurveNum)%X1SortOrder == DESCENDING
				if ( PerfCurve( CurveNum ).X2SortOrder == ASCENDING ) {

					for ( J = 1; J <= TableLookup( TableNum ).NumX2Vars; ++J ) {

						if ( ReadFromFile ) {

							{
								IOFlags flags;
								for ( I = TableLookup( TableNum ).NumX1Vars; I >= 1; --I ) {
									gio::read( FileNum, fmtLD, flags ) >> TableLookup( TableNum ).TableLookupZData( Var5Index, Var4Index, Var3Index, J, I );
								}
								ReadStat = flags.ios();
							}

							if ( ReadStat < GoodIOStatValue ) goto Label1000;

						} else {

							for ( I = TableLookup( TableNum ).NumX1Vars; I >= 1; --I ) {
								TableLookup( TableNum ).TableLookupZData( Var5Index, Var4Index, Var3Index, J, I ) = Numbers( NumbersOffset );
								++NumbersOffset;
							}

						}
					}
				} else {
					for ( J = TableLookup( TableNum ).NumX2Vars; J >= 1; --J ) {

						if ( ReadFromFile ) {

							{
								IOFlags flags;
								for ( I = TableLookup( TableNum ).NumX1Vars; I >= 1; --I ) {
									gio::read( FileNum, fmtLD, flags ) >> TableLookup( TableNum ).TableLookupZData( Var5Index, Var4Index, Var3Index, J, I );
								}
								ReadStat = flags.ios();
							}

							if ( ReadStat < GoodIOStatValue ) goto Label1000;

						} else {

							for ( I = TableLookup( TableNum ).NumX1Vars; I >= 1; --I ) {
								TableLookup( TableNum ).TableLookupZData( Var5Index, Var4Index, Var3Index, J, I ) = Numbers( NumbersOffset );
								++NumbersOffset;
							}

						}
					}
				}
			}

			for ( J = 1; J <= TableLookup( TableNum ).NumX2Vars; ++J ) {

				// write data to eio file in ascending order
				if ( EchoTableDataToEio ) {
					for ( I = 1; I <= TableLookup( TableNum ).NumX1Vars; ++I ) {
						gio::write( OutputFileInits, fmtLD, non_adv ) << TableLookup( TableNum ).TableLookupZData( Var5Index, Var4Index, Var3Index, J, I );
					} gio::write( OutputFileInits );
				}

				// normalize the data according to the user entered normal point
				for ( I = 1; I <= TableLookup( TableNum ).NumX1Vars; ++I ) {
					TableLookup( TableNum ).TableLookupZData( Var5Index, Var4Index, Var3Index, J, I ) /= TableData( TableNum ).NormalPoint;
				}

			}

			++DataSetCount;

		}

		if ( EchoTableDataToEio ) {
			gio::write( OutputFileInits, Format_131 );
		}

Label1000: ;
		EOFonFile = true;
		if ( ReadFromFile ) gio::close( FileNum );

		if ( TotalDataSets < DataSetCount ) {
			ShowSevereError( "GetTableDataFile: For Table:MultiVariableLookup \"" + PerfCurve( CurveNum ).Name + "\"" );
			ShowContinueError( "...The required number of data sets (" + RoundSigDigits( TotalDataSets ) + ") is less than the number determined by the number and count of independent variables (" + RoundSigDigits( DataSetCount ) + ")." );
		}

		return;

Label999: ;
		ShowSevereError( "CurveManager: SearchTableDataFile: Could not open Table Data File, expecting it as file name = " + FileName );
		ShowContinueError( "Certain run environments require a full path to be included with the file name in the input field." );
		ShowContinueError( "Try again with putting full path and file name in the field." );
		ShowFatalError( "Program terminates due to these conditions." );

	}

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
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         AUTHOR: F.D.HAMMERLING, COMPUTING TECHNOLOGY CENTER, ORNL
		//       DATE WRITTEN   2010
		//       MODIFIED       Richard Raustad, FSEC
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//         DLAG IS A DOUBLE-PRECISION, TWO-DIMENSIONAL,
		//         LAGRANGIAN INTERPOLATION
		//           INPUT VARIABLES:
		//             XX        X-COORDINATE OF THE DESIRED INTERPOLATED POINT
		//             YY        Y-COORDINATE OF THE DESIRED INTERPOLATED POINT
		//             X         SINGLY DIMENSIONED ARRAY OF X-COORDINATES
		//             Y         SINGLY DIMENSIONED ARRAY OF Y-COORDINATES
		//             Z         DOUBLY DIMENSIONED ARRAY OF FUNCTION VALUES,
		//                       I.E. Z(I,J) = F( X(I), Y(J) )
		//             NX        NUMBER OF ELEMENTS IN THE X-ARRAY
		//             NY        NUMBER OF ELEMENTS IN THE Y-ARRAY
		//             M         THE SQUARE ROOT OF THE NUMBER OF POINTS TO BE
		//                       CONSIDERED IN THE INTERPOLATION - NUMBER OF
		//                       POINTS IN EACH DIRECTION
		//             ID        THE FIRST DIMENSION OF THE Z-ARRAY (AT LEAST NX)
		//           OUTPUT VARIABLES:
		//             IEXTX     =1, IF EXTRAPOLATION OCCURED ABOVE THE X-ARRAY
		//                       =0, IF INTERPOLATION OCCURED
		//                       =-1, IF EXTRAPOLATION OCCURED BELOW THE X-ARRAY
		//             IEXTY     SAME FOR THE Y-ARRAY AS IEXTX IS FOR THE X-ARRAY
		//      THIS PROGRAM WAS MODIFIED AUGUST 1984 BY CJ EMERSON TO INSURE
		//      THAT ITERATIVE CALLS TO DLAG USE THE SAME POINTS FOR INTERPOLATION.
		//      ISXPT(ISYPT) ARE CHOSEN FROM THE UPPER END OF THE INTERVAL.
		//      ALSO THE PROGRAM WAS MODIFIED SO THAT EXTRAPOLATION ALWAYS USES
		//      AT MOST TWO POINTS.
		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 DLAG;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//DIMENSION Z(ID,NY),X(NX),Y(NY),XLAG(100)
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:!
		int I;
		int ISXPT( 0 );
		int IEXPT( 0 );
		int J;
		int ISYPT( 0 );
		int IEYPT( 0 );
		int K;
		int M1;
		Real64 MIDX;
		Real64 MIDY;

		//       INITIALIZE
		bool QUITX = false;
		bool QUITY = false;
		IEXTX = 0;
		IEXTY = 0;
		//       The following code has been upgraded to current Fortran standards
		//       See Starteam Revision 33, August 17, 2010 for legacy code if comparison is needed
		//       FIND THE RANGE OF INTERPOLATION ALONG X
		M1 = M; // number of points to be interpolated (X direction is first)
		if ( M1 > NX ) M1 = NX; // limit to number of X points if necessary

		//       loop through X data and find the first x-coordinate less than the interpolated point
		//       if the interpolation point is less than or greater than the X data then linearly extrapolate
		//       linear extrapolation uses only 2 points (M1=2)
		for ( I = 1; I <= NX; ++I ) {
			if ( XX - X( I ) < 0.0 ) {
				MIDX = I; // found X point just greater than interpolation point
				if ( MIDX == 1 ) {
					IEXTX = -1; // extrapolating at the lower bound of x
					if ( M1 > 2 ) M1 = 2; // limit to linear extrapolation
				}
				ISXPT = MIDX - ( ( M1 + 1 ) / 2 ); // calculate starting point in X array
				if ( ISXPT <= 0 ) ISXPT = 1; // limit to first element in X array
				IEXPT = ISXPT + M1 - 1; // calculate ending point in X array
				if ( IEXPT > NX ) {
					ISXPT = NX - M1 + 1; // if upper X array boundary exceeded, recalculate starting point
					IEXPT = NX; // limit ending point to upper boundary of X array
				}
				break;
			} else if ( XX - X( I ) == 0.0 ) { // interpolation point is equal to element in X array
				QUITX = true; // exact interpolation point found in X array, do not interpolate
				break;
			} else if ( I == NX ) { // interpolation point is greater than max X value
				IEXTX = 1; // extrapolating at the upper bound of X
				if ( M1 > 2 ) M1 = 2; // limit to linear extrapolation
				ISXPT = NX - M1 + 1; // calculate starting point in X array
				IEXPT = NX; // ending point equals upper bound of X array
				break;
			}
		}

		M1 = M; // number of points to be interpolated (Y direction is second)
		if ( M1 > NY ) M1 = NY; // limit to number of Y points if necessary

		for ( J = 1; J <= NY; ++J ) {
			if ( YY - Y( J ) < 0.0 ) {
				MIDY = J; // found Y point just greater than interpolation point
				if ( MIDY <= 1 ) {
					IEXTY = -1; // extrapolating at the lower bound of y
					if ( M1 > 2 ) M1 = 2; // limit to linear extrapolation
				}
				ISYPT = MIDY - ( ( M1 + 1 ) / 2 ); // calculate starting point in Y array
				if ( ISYPT <= 0 ) ISYPT = 1; // limit to first element in array
				IEYPT = ISYPT + M1 - 1; // calculate ending point in X array
				if ( IEYPT > NY ) {
					ISYPT = NY - M1 + 1; // if upper Y array boundary exceeded, recalculate starting point
					IEYPT = NY; // limit ending point to upper boundary of Y array
				}
				break;
			} else if ( YY - Y( J ) == 0.0 ) { // interpolation point is equal to element in Y array
				QUITY = true; // exact interpolation point found in Y array, do not interpolate
				break;
			} else if ( J == NY ) { // interpolation point is greater than max Y value
				IEXTY = 1; // extrapolating at the upper bound of Y
				if ( M1 > 2 ) M1 = 2; // limit to linear extrapolation
				ISYPT = NY - M1 + 1; // calculate starting point in Y array
				IEYPT = NY; // ending point equals upper bound of Y array
				break;
			}
		}

		if ( QUITX && QUITY ) {
			DLAG = Z( J, I ); // found exact X and Y point in Z array
		} else if ( QUITX && ! QUITY ) { // only interpolate in Y direction
			Array1D< Real64 > XLAG( IEYPT );
			for ( int l = ISYPT; l <= IEYPT; ++l ) {
				XLAG( l ) = Z( l, I ); // store X's at each Y (I = midpoint of array from above)
			}
			Interpolate_Lagrange( YY, XLAG, Y, ISYPT, IEYPT, DLAG ); // now interpolate these X's
		} else if ( ! QUITX && QUITY ) { // only interpolate in X direction
			Interpolate_Lagrange( XX, Z( J, _ ), X, ISXPT, IEXPT, DLAG ); // (:,J) interpolate X array at fixed Y (J here)
		} else { // else interpolate in X and Y directions
			Array1D< Real64 > XLAG( IEYPT );
			for ( K = ISYPT; K <= IEYPT; ++K ) {
				Interpolate_Lagrange( XX, Z( K, _ ), X, ISXPT, IEXPT, XLAG( K ) ); // (:,K) interpolate X array at all Y's (K here)
			}
			Interpolate_Lagrange( YY, XLAG, Y, ISYPT, IEYPT, DLAG ); // final interpolation of X array
		}

		return DLAG;
	}

	Real64
	PerformanceCurveObject(
		int const CurveIndex, // index of curve in curve array
		Real64 const Var1, // 1st independent variable
		Optional< Real64 const > Var2, // 2nd independent variable
		Optional< Real64 const > Var3, // 3rd independent variable
		Optional< Real64 const > Var4 // 4th independent variable
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Lixing Gu, July 2006; B. Griffith July 2006
		//                      22Aug2010 Craig Wray, added new curves for fan component model:
		//                          FanPressureRise, ExponentialSkewNormal, Sigmoid, RectangularHyperbola1,
		//                          RectangularHyperbola2, ExponentialDecay

		//       RE-ENGINEERED  Autodesk: Performance tuning

		// PURPOSE OF THIS FUNCTION:
		// Given the curve index and the values of 1 or 2 independent variables,
		// returns the value of an equipment performance curve.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 CurveValue;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		static Real64 const sqrt_2_inv( 1.0 / std::sqrt( 2.0 ) );

		Real64 CoeffZ1; // cpw22Aug2010 Coefficient Z1 in exponential skew normal curve
		Real64 CoeffZ2; // cpw22Aug2010 Coefficient Z2 in exponential skew normal curve
		Real64 CoeffZ3; // cpw22Aug2010 Coefficient Z3 in exponential skew normal curve
		Real64 CurveValueNumer; // cpw22Aug2010 Numerator in in exponential skew normal curve
		Real64 CurveValueDenom; // cpw22Aug2010 Numerator in in exponential skew normal curve
		Real64 CurveValueExp; // cpw22Aug2010 Exponential term in sigmoid curve
		auto const & Curve( PerfCurve( CurveIndex ) );

		Real64 const V1( max( min( Var1, Curve.Var1Max ), Curve.Var1Min ) ); // 1st independent variable after limits imposed
		Real64 const V2( Var2.present() ? max( min( Var2, Curve.Var2Max ), Curve.Var2Min ) : 0.0 ); // 2nd independent variable after limits imposed
		Real64 const V3( Var3.present() ? max( min( Var3, Curve.Var3Max ), Curve.Var3Min ) : 0.0 ); // 3rd independent variable after limits imposed
		Real64 const V4( Var4.present() ? max( min( Var4, Curve.Var4Max ), Curve.Var4Min ) : 0.0 ); // 4th independent variable after limits imposed

		{ auto const SELECT_CASE_var( Curve.CurveType );
		if ( SELECT_CASE_var == Linear ) {
			CurveValue = Curve.Coeff1 + V1 * Curve.Coeff2;
		} else if ( SELECT_CASE_var == Quadratic ) {
			CurveValue = Curve.Coeff1 + V1 * ( Curve.Coeff2 + V1 * Curve.Coeff3 );
		} else if ( SELECT_CASE_var == QuadLinear ) {
			CurveValue = Curve.Coeff1 + V1 * Curve.Coeff2 + V2 * Curve.Coeff3 + V3 * Curve.Coeff4 + V4 * Curve.Coeff5;
		} else if ( SELECT_CASE_var == Cubic ) {
			CurveValue = Curve.Coeff1 + V1 * ( Curve.Coeff2 + V1 * ( Curve.Coeff3 + V1 * Curve.Coeff4 ) );
		} else if ( SELECT_CASE_var == Quartic ) {
			CurveValue = Curve.Coeff1 + V1 * ( Curve.Coeff2 + V1 * ( Curve.Coeff3 + V1 * ( Curve.Coeff4 + V1 * Curve.Coeff5 ) ) );
		} else if ( SELECT_CASE_var == BiQuadratic ) {
			CurveValue = Curve.Coeff1 + V1 * ( Curve.Coeff2 + V1 * Curve.Coeff3 ) + V2 * ( Curve.Coeff4 + V2 * Curve.Coeff5 ) + V1 * V2 * Curve.Coeff6;
		} else if ( SELECT_CASE_var == QuadraticLinear ) {
			CurveValue = ( Curve.Coeff1 + V1 * ( Curve.Coeff2 + V1 * Curve.Coeff3 ) ) + ( Curve.Coeff4 + V1 * ( Curve.Coeff5 + V1 * Curve.Coeff6 ) ) * V2;
		} else if ( SELECT_CASE_var == CubicLinear ) {
			CurveValue = ( Curve.Coeff1 + V1 * ( Curve.Coeff2 + V1 * ( Curve.Coeff3 + V1 * Curve.Coeff4 ) ) ) + ( Curve.Coeff5 + V1 * Curve.Coeff6 ) * V2;
		} else if ( SELECT_CASE_var == BiCubic ) {
			CurveValue = Curve.Coeff1 + V1 * Curve.Coeff2 + V1 * V1 * Curve.Coeff3 + V2 * Curve.Coeff4 + V2 * V2 * Curve.Coeff5 + V1 * V2 * Curve.Coeff6 + V1 * V1 * V1 * Curve.Coeff7 + V2 * V2 * V2 * Curve.Coeff8 + V1 * V1 * V2 * Curve.Coeff9 + V1 * V2 * V2 * Curve.Coeff10;
		} else if ( SELECT_CASE_var == ChillerPartLoadWithLift ) {
			CurveValue = Curve.Coeff1 + Curve.Coeff2*V1 + Curve.Coeff3*V1*V1 + Curve.Coeff4*V2 + Curve.Coeff5*V2*V2 + Curve.Coeff6*V1*V2  + Curve.Coeff7*V1*V1*V1 + Curve.Coeff8*V2*V2*V2 + Curve.Coeff9*V1*V1*V2 + Curve.Coeff10*V1*V2*V2 + Curve.Coeff11*V1*V1*V2*V2 + Curve.Coeff12*V3*V2*V2*V2;
		} else if ( SELECT_CASE_var == TriQuadratic ) {
			auto const & Tri2ndOrder( Curve.Tri2ndOrder( 1 ) );
			auto const V1s( V1 * V1 );
			auto const V2s( V2 * V2 );
			auto const V3s( V3 * V3 );
			CurveValue = Tri2ndOrder.CoeffA0 + Tri2ndOrder.CoeffA1 * V1s + Tri2ndOrder.CoeffA2 * V1 + Tri2ndOrder.CoeffA3 * V2s + Tri2ndOrder.CoeffA4 * V2 + Tri2ndOrder.CoeffA5 * V3s + Tri2ndOrder.CoeffA6 * V3 + Tri2ndOrder.CoeffA7 * V1s * V2s + Tri2ndOrder.CoeffA8 * V1 * V2 + Tri2ndOrder.CoeffA9 * V1 * V2s + Tri2ndOrder.CoeffA10 * V1s * V2 + Tri2ndOrder.CoeffA11 * V1s * V3s + Tri2ndOrder.CoeffA12 * V1 * V3 + Tri2ndOrder.CoeffA13 * V1 * V3s + Tri2ndOrder.CoeffA14 * V1s * V3 + Tri2ndOrder.CoeffA15 * V2s * V3s + Tri2ndOrder.CoeffA16 * V2 * V3 + Tri2ndOrder.CoeffA17 * V2 * V3s + Tri2ndOrder.CoeffA18 * V2s * V3 + Tri2ndOrder.CoeffA19 * V1s * V2s * V3s + Tri2ndOrder.CoeffA20 * V1s * V2s * V3 + Tri2ndOrder.CoeffA21 * V1s * V2 * V3s + Tri2ndOrder.CoeffA22 * V1 * V2s * V3s + Tri2ndOrder.CoeffA23 * V1s * V2 * V3 + Tri2ndOrder.CoeffA24 * V1 * V2s * V3 + Tri2ndOrder.CoeffA25 * V1 * V2 * V3s + Tri2ndOrder.CoeffA26 * V1 * V2 * V3;
		} else if ( SELECT_CASE_var == Exponent ) {
			CurveValue = Curve.Coeff1 + Curve.Coeff2 * std::pow( V1, Curve.Coeff3 );
		} else if ( SELECT_CASE_var == FanPressureRise ) { //cpw22Aug2010 Added Fan Pressure Rise curve
			CurveValue = V1 * ( Curve.Coeff1 * V1 + Curve.Coeff2 + Curve.Coeff3 * std::sqrt( V2 ) ) + Curve.Coeff4 * V2;
		} else if ( SELECT_CASE_var == ExponentialSkewNormal ) { //cpw22Aug2010 Added Exponential Skew Normal curve
			CoeffZ1 = ( V1 - Curve.Coeff1 ) / Curve.Coeff2;
			CoeffZ2 = ( Curve.Coeff4 * V1 * std::exp( Curve.Coeff3 * V1 ) - Curve.Coeff1 ) / Curve.Coeff2;
			CoeffZ3 = -Curve.Coeff1 / Curve.Coeff2;
			//    CurveValueNumer = EXP(-0.5d0 * CoeffZ1**2) * (1.0d0 + SIGN(1.0d0,CoeffZ2) * ErfFunction(ABS(CoeffZ2)/SQRT(2.0d0)))
			//    CurveValueDenom = EXP(-0.5d0 * CoeffZ3**2) * (1.0d0 + SIGN(1.0d0,CoeffZ3) * ErfFunction(ABS(CoeffZ3)/SQRT(2.0d0)))
			CurveValueNumer = std::exp( -0.5 * ( CoeffZ1 * CoeffZ1 ) ) * ( 1.0 + sign( 1.0, CoeffZ2 ) * std::erf( std::abs( CoeffZ2 ) * sqrt_2_inv ) );
			CurveValueDenom = std::exp( -0.5 * ( CoeffZ3 * CoeffZ3 ) ) * ( 1.0 + sign( 1.0, CoeffZ3 ) * std::erf( std::abs( CoeffZ3 ) * sqrt_2_inv ) );
			CurveValue = CurveValueNumer / CurveValueDenom;
		} else if ( SELECT_CASE_var == Sigmoid ) { //cpw22Aug2010 Added Sigmoid curve
			CurveValueExp = std::exp( ( Curve.Coeff3 - V1 ) / Curve.Coeff4 );
			CurveValue = Curve.Coeff1 + Curve.Coeff2 / std::pow( 1.0 + CurveValueExp, Curve.Coeff5 );
		} else if ( SELECT_CASE_var == RectangularHyperbola1 ) { //cpw22Aug2010 Added Rectangular Hyperbola Type 1 curve
			CurveValueNumer = Curve.Coeff1 * V1;
			CurveValueDenom = Curve.Coeff2 + V1;
			CurveValue = ( CurveValueNumer / CurveValueDenom ) + Curve.Coeff3;
		} else if ( SELECT_CASE_var == RectangularHyperbola2 ) { //cpw22Aug2010 Added Rectangular Hyperbola Type 2 curve
			CurveValueNumer = Curve.Coeff1 * V1;
			CurveValueDenom = Curve.Coeff2 + V1;
			CurveValue = ( CurveValueNumer / CurveValueDenom ) + ( Curve.Coeff3 * V1 );
		} else if ( SELECT_CASE_var == ExponentialDecay ) { //cpw22Aug2010 Added Exponential Decay curve
			CurveValue = Curve.Coeff1 + Curve.Coeff2 * std::exp( Curve.Coeff3 * V1 );
		} else if ( SELECT_CASE_var == DoubleExponentialDecay ) { //ykt Jul 2011 Added Double Exponential Decay curve
			CurveValue = Curve.Coeff1 + Curve.Coeff2 * std::exp( Curve.Coeff3 * V1 ) + Curve.Coeff4 * std::exp( Curve.Coeff5 * V1 );
		} else {
			CurveValue = 0.0;
		}}

		if ( Curve.CurveMinPresent ) CurveValue = max( CurveValue, Curve.CurveMin );
		if ( Curve.CurveMaxPresent ) CurveValue = min( CurveValue, Curve.CurveMax );

		return CurveValue;
	}

	Real64
	PerformanceTableObject(
		int const CurveIndex, // index of curve in curve array
		Real64 const Var1, // 1st independent variable
		Optional< Real64 const > Var2, // 2nd independent variable
		Optional< Real64 const > Var3 // 3rd independent variable
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   May 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Given the curve index and the values of 1 or 2 independent variables,
		// returns the value of an equipment performance table lookup.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// Return value
		Real64 TableValue;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Real64 V1; // 1st independent variable after limits imposed
		Real64 V2; // 2nd independent variable after limits imposed
		Real64 V3; // 3rd independent variable after limits imposed
		Real64 TempX1Low;
		Real64 TempX1High;
		Real64 TempX2Low;
		Real64 TempX2High;
		//INTEGER   :: ATempX1LowPtr(1)
		//INTEGER   :: ATempX1HighPtr(1)
		//INTEGER   :: ATempX2LowPtr(1)
		//INTEGER   :: ATempX2HighPtr(1)
		int TempX1LowPtr( 0 );
		int TempX1HighPtr( 0 );
		int TempX2LowPtr( 0 );
		int TempX2HighPtr( 0 );
		Real64 X1Frac;
		Real64 X2Frac;
		Real64 X1ValLow;
		Real64 X1ValHigh;
		//INTEGER   :: MaxSizeArray
		int X1Val;
		int X2Val;
		int TableIndex;

		TableIndex = PerfCurve( CurveIndex ).TableIndex;

		V1 = max( min( Var1, PerfCurve( CurveIndex ).Var1Max ), PerfCurve( CurveIndex ).Var1Min );

		if ( present( Var2 ) ) {
			V2 = max( min( Var2, PerfCurve( CurveIndex ).Var2Max ), PerfCurve( CurveIndex ).Var2Min );
		} else {
			V2 = 0.0;
		}

		if ( present( Var3 ) ) {
			V3 = max( min( Var3, PerfCurve( CurveIndex ).Var3Max ), PerfCurve( CurveIndex ).Var3Min );
		} else {
			V3 = 0.0;
		}

		{ auto const SELECT_CASE_var( TableLookup( TableIndex ).NumIndependentVars );
		if ( SELECT_CASE_var == 1 ) {

			TempX1Low = minval( PerfCurveTableData( TableIndex ).X1 );
			TempX1High = maxval( PerfCurveTableData( TableIndex ).X1 );
			if ( V1 <= TempX1Low ) {
				TempX1LowPtr = 1;
				TempX1HighPtr = 1;
			} else if ( V1 >= TempX1High ) {
				TempX1LowPtr = size( PerfCurveTableData( TableIndex ).X1 );
				TempX1HighPtr = TempX1LowPtr;
			} else {
				for ( X1Val = 1; X1Val <= isize( PerfCurveTableData( TableIndex ).X1 ); ++X1Val ) {
					if ( V1 >= PerfCurveTableData( TableIndex ).X1( X1Val ) ) TempX1LowPtr = X1Val;
				}
				if ( V1 == PerfCurveTableData( TableIndex ).X1( TempX1LowPtr ) ) {
					TempX1HighPtr = TempX1LowPtr;
				} else {
					TempX1HighPtr = TempX1LowPtr + 1;
				}
			}
			if ( TempX1LowPtr == TempX1HighPtr ) {
				TableValue = PerfCurveTableData( TableIndex ).Y( 1, TempX1LowPtr );
			} else {
				X1Frac = ( V1 - PerfCurveTableData( TableIndex ).X1( TempX1LowPtr ) ) / ( PerfCurveTableData( TableIndex ).X1( TempX1HighPtr ) - PerfCurveTableData( TableIndex ).X1( TempX1LowPtr ) );
				TableValue = X1Frac * PerfCurveTableData( TableIndex ).Y( 1, TempX1HighPtr ) + ( 1 - X1Frac ) * PerfCurveTableData( TableIndex ).Y( 1, TempX1LowPtr );
			}

		} else if ( SELECT_CASE_var == 2 ) {

			TempX1Low = minval( PerfCurveTableData( TableIndex ).X1 );
			TempX1High = maxval( PerfCurveTableData( TableIndex ).X1 );
			if ( V1 <= TempX1Low ) {
				TempX1LowPtr = 1;
				TempX1HighPtr = 1;
			} else if ( V1 >= TempX1High ) {
				TempX1LowPtr = size( PerfCurveTableData( TableIndex ).X1 );
				TempX1HighPtr = TempX1LowPtr;
			} else {
				for ( X1Val = 1; X1Val <= isize( PerfCurveTableData( TableIndex ).X1 ); ++X1Val ) {
					if ( V1 >= PerfCurveTableData( TableIndex ).X1( X1Val ) ) TempX1LowPtr = X1Val;
				}
				if ( V1 == PerfCurveTableData( TableIndex ).X1( TempX1LowPtr ) ) {
					TempX1HighPtr = TempX1LowPtr;
				} else {
					TempX1HighPtr = TempX1LowPtr + 1;
				}
			}
			TempX2Low = minval( PerfCurveTableData( TableIndex ).X2 );
			TempX2High = maxval( PerfCurveTableData( TableIndex ).X2 );

			if ( V2 <= TempX2Low ) {
				TempX2LowPtr = 1;
				TempX2HighPtr = 1;
			} else if ( V2 >= TempX2High ) {
				TempX2LowPtr = size( PerfCurveTableData( TableIndex ).X2 );
				TempX2HighPtr = TempX2LowPtr;
			} else {
				for ( X2Val = 1; X2Val <= isize( PerfCurveTableData( TableIndex ).X2 ); ++X2Val ) {
					if ( V2 >= PerfCurveTableData( TableIndex ).X2( X2Val ) ) TempX2LowPtr = X2Val;
				}
				if ( V2 == PerfCurveTableData( TableIndex ).X2( TempX2LowPtr ) ) {
					TempX2HighPtr = TempX2LowPtr;
				} else {
					TempX2HighPtr = TempX2LowPtr + 1;
				}
			}

			if ( TempX1LowPtr == TempX1HighPtr ) {
				if ( TempX2LowPtr == TempX2HighPtr ) {
					TableValue = PerfCurveTableData( TableIndex ).Y( TempX2LowPtr, TempX1LowPtr );
				} else {
					X2Frac = ( V2 - PerfCurveTableData( TableIndex ).X2( TempX2LowPtr ) ) / ( PerfCurveTableData( TableIndex ).X2( TempX2HighPtr ) - PerfCurveTableData( TableIndex ).X2( TempX2LowPtr ) );
					TableValue = X2Frac * PerfCurveTableData( TableIndex ).Y( TempX2HighPtr, TempX1LowPtr ) + ( 1 - X2Frac ) * PerfCurveTableData( TableIndex ).Y( TempX2LowPtr, TempX1LowPtr );
				}
			} else {
				X1Frac = ( V1 - PerfCurveTableData( TableIndex ).X1( TempX1LowPtr ) ) / ( PerfCurveTableData( TableIndex ).X1( TempX1HighPtr ) - PerfCurveTableData( TableIndex ).X1( TempX1LowPtr ) );
				if ( TempX2LowPtr == TempX2HighPtr ) {
					TableValue = X1Frac * PerfCurveTableData( TableIndex ).Y( TempX2LowPtr, TempX1HighPtr ) + ( 1 - X1Frac ) * PerfCurveTableData( TableIndex ).Y( TempX2LowPtr, TempX1LowPtr );
				} else {
					X1ValLow = X1Frac * PerfCurveTableData( TableIndex ).Y( TempX2LowPtr, TempX1HighPtr ) + ( 1 - X1Frac ) * PerfCurveTableData( TableIndex ).Y( TempX2LowPtr, TempX1LowPtr );
					X1ValHigh = X1Frac * PerfCurveTableData( TableIndex ).Y( TempX2HighPtr, TempX1HighPtr ) + ( 1 - X1Frac ) * PerfCurveTableData( TableIndex ).Y( TempX2HighPtr, TempX1LowPtr );
					X2Frac = ( V2 - PerfCurveTableData( TableIndex ).X2( TempX2LowPtr ) ) / ( PerfCurveTableData( TableIndex ).X2( TempX2HighPtr ) - PerfCurveTableData( TableIndex ).X2( TempX2LowPtr ) );
					TableValue = X2Frac * X1ValHigh + ( 1 - X2Frac ) * X1ValLow;
				}
			}

		} else {
			TableValue = 0.0;
			ShowSevereError( "Errors found in table output calculation for " + PerfCurve( CurveIndex ).Name );
			ShowContinueError( "...Possible causes are selection of Interpolation Method or Type or Number of Independent Variables or Points." );
			ShowFatalError( "PerformanceTableObject: Previous error causes program termination." );
		}}

		if ( PerfCurve( CurveIndex ).CurveMinPresent ) TableValue = max( TableValue, PerfCurve( CurveIndex ).CurveMin );
		if ( PerfCurve( CurveIndex ).CurveMaxPresent ) TableValue = min( TableValue, PerfCurve( CurveIndex ).CurveMax );

		return TableValue;

	}

	Real64
	TableLookupObject(
		int const CurveIndex, // index of curve in curve array
		Real64 const Var1, // 1st independent variable
		Optional< Real64 const > Var2, // 2nd independent variable
		Optional< Real64 const > Var3, // 3rd independent variable
		Optional< Real64 const > Var4, // 4th independent variable
		Optional< Real64 const > Var5 // 5th independent variable
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   May 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Given the curve index and the values of 1 or 2 independent variables,
		// returns the value of an equipment performance table lookup.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// Return value
		Real64 TableValue;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Real64 V1; // 1st independent variable after limits imposed
		Real64 V2; // 2nd independent variable after limits imposed
		Real64 V3; // 3rd independent variable after limits imposed
		Real64 V4; // 4th independent variable after limits imposed
		Real64 V5; // 5th independent variable after limits imposed

		int NX;
		int NY;
		int NV3;
		int NV4;
		int NV5;
		int TableIndex;
		//REAL(r64), ALLOCATABLE, DIMENSION(:)     :: ONEDVALS
		Array2D< Real64 > TWODVALS;
		Array3D< Real64 > THREEDVALS;
		Array1D< Real64 > VALSX;
		Array1D< Real64 > VALSY;
		Array1D< Real64 > VALSV3;
		Array1D< Real64 > VALSV4;
		Array1D< Real64 > VALSV5;
		//REAL(r64), ALLOCATABLE, DIMENSION(:,:,:) :: HPVAL
		//REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: HPVALS
		//REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: DVLTRN
		//REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: FiveDArray
		//REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: FourDArray
		//REAL(r64), ALLOCATABLE, DIMENSION(:,:,:) :: ThreeDArray
		//REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: TwoDArray
		//REAL(r64), ALLOCATABLE, DIMENSION(:) :: OneDArray
		int IV3;
		int IV4;
		int IV5;
		int IEXTX;
		int IEXTY;
		int IEXTV3;
		int IEXTV4;
		int IEXTV5;
		int NUMPT;

		TableIndex = PerfCurve( CurveIndex ).TableIndex;

		V1 = max( min( Var1, PerfCurve( CurveIndex ).Var1Max ), PerfCurve( CurveIndex ).Var1Min );

		if ( present( Var2 ) ) {
			V2 = max( min( Var2, PerfCurve( CurveIndex ).Var2Max ), PerfCurve( CurveIndex ).Var2Min );
			if ( TableLookup( TableIndex ).NumIndependentVars < 2 ) {
				if ( PerfCurve( CurveIndex ).NumIVHighErrorIndex == 0 ) {
					ShowSevereError( "TableLookupObject: " + cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + "\"" + PerfCurve( CurveIndex ).Name + "\"" );
					ShowContinueError( "...Excess number of independent variables (2) passed to subroutine when only 1 is required." );
				}
				ShowRecurringWarningErrorAtEnd( cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + " \"" + PerfCurve( CurveIndex ).Name + "\": Excess number of independent variables warning continues...", PerfCurve( CurveIndex ).NumIVHighErrorIndex, 2.0, 2.0 );
			}
		} else {
			if ( TableLookup( TableIndex ).NumIndependentVars > 1 ) {
				if ( PerfCurve( CurveIndex ).NumIVLowErrorIndex == 0 ) {
					ShowSevereError( "TableLookupObject: " + cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + "\"" + PerfCurve( CurveIndex ).Name + "\"" );
					ShowContinueError( "...Insufficient number of independent variables (1) passed to subroutine when at least 2 are required." );
				}
				ShowRecurringWarningErrorAtEnd( cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + " \"" + PerfCurve( CurveIndex ).Name + "\": Insufficient number of independent variables warning continues...", PerfCurve( CurveIndex ).NumIVLowErrorIndex, 1.0, 1.0 );
			}
			V2 = 0.0;
		}

		if ( present( Var3 ) ) {
			V3 = max( min( Var3, PerfCurve( CurveIndex ).Var3Max ), PerfCurve( CurveIndex ).Var3Min );
			if ( TableLookup( TableIndex ).NumIndependentVars < 3 ) {
				if ( PerfCurve( CurveIndex ).NumIVHighErrorIndex == 0 ) {
					ShowSevereError( "TableLookupObject: " + cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + "\"" + PerfCurve( CurveIndex ).Name + "\"" );
					ShowContinueError( "...Excess number of independent variables (3) passed to subroutine when 2 or less are required." );
				}
				ShowRecurringWarningErrorAtEnd( cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + " \"" + PerfCurve( CurveIndex ).Name + "\": Excess number of independent variables warning continues...", PerfCurve( CurveIndex ).NumIVHighErrorIndex, 3.0, 3.0 );
			}
		} else {
			if ( TableLookup( TableIndex ).NumIndependentVars > 2 ) {
				if ( PerfCurve( CurveIndex ).NumIVLowErrorIndex == 0 ) {
					ShowSevereError( "TableLookupObject: " + cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + "\"" + PerfCurve( CurveIndex ).Name + "\"" );
					ShowContinueError( "...Insufficient number of independent variables (2) passed to subroutine when at least 3 are required." );
				}
				ShowRecurringWarningErrorAtEnd( cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + " \"" + PerfCurve( CurveIndex ).Name + "\": Insufficient number of independent variables warning continues...", PerfCurve( CurveIndex ).NumIVLowErrorIndex, 2.0, 2.0 );
			}
			V3 = 0.0;
		}

		if ( present( Var4 ) ) {
			V4 = max( min( Var4, PerfCurve( CurveIndex ).Var4Max ), PerfCurve( CurveIndex ).Var4Min );
			if ( TableLookup( TableIndex ).NumIndependentVars < 4 ) {
				if ( PerfCurve( CurveIndex ).NumIVHighErrorIndex == 0 ) {
					ShowSevereError( "TableLookupObject: " + cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + "\"" + PerfCurve( CurveIndex ).Name + "\"" );
					ShowContinueError( "...Excess number of independent variables (4) passed to subroutine when 3 or less are required." );
				}
				ShowRecurringWarningErrorAtEnd( cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + " \"" + PerfCurve( CurveIndex ).Name + "\": Excess number of independent variables warning continues...", PerfCurve( CurveIndex ).NumIVHighErrorIndex, 4.0, 4.0 );
			}
		} else {
			if ( TableLookup( TableIndex ).NumIndependentVars > 3 ) {
				if ( PerfCurve( CurveIndex ).NumIVLowErrorIndex == 0 ) {
					ShowSevereError( "TableLookupObject: " + cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + "\"" + PerfCurve( CurveIndex ).Name + "\"" );
					ShowContinueError( "...Insufficient number of independent variables (3) passed to subroutine when at least 4 are required." );
				}
				ShowRecurringWarningErrorAtEnd( cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + " \"" + PerfCurve( CurveIndex ).Name + "\": Insufficient number of independent variables warning continues...", PerfCurve( CurveIndex ).NumIVLowErrorIndex, 3.0, 3.0 );
			}
			V4 = 0.0;
		}

		if ( present( Var5 ) ) {
			V5 = max( min( Var5, PerfCurve( CurveIndex ).Var5Max ), PerfCurve( CurveIndex ).Var5Min );
			if ( TableLookup( TableIndex ).NumIndependentVars < 5 ) {
				if ( PerfCurve( CurveIndex ).NumIVHighErrorIndex == 0 ) {
					ShowSevereError( "TableLookupObject: " + cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + "\"" + PerfCurve( CurveIndex ).Name + "\"" );
					ShowContinueError( "...Excess number of independent variables (5) passed to subroutine when 4 or less are required." );
				}
				ShowRecurringWarningErrorAtEnd( cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + " \"" + PerfCurve( CurveIndex ).Name + "\": Excess number of independent variables warning continues...", PerfCurve( CurveIndex ).NumIVHighErrorIndex, 5.0, 5.0 );
			}
		} else {
			if ( TableLookup( TableIndex ).NumIndependentVars > 4 ) {
				if ( PerfCurve( CurveIndex ).NumIVLowErrorIndex == 0 ) {
					ShowSevereError( "TableLookupObject: " + cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + "\"" + PerfCurve( CurveIndex ).Name + "\"" );
					ShowContinueError( "...Insufficient number of independent variables (4) passed to subroutine when at least 5 are required." );
				}
				ShowRecurringWarningErrorAtEnd( cCurveTypes( PerfCurve( CurveIndex ).ObjectType ) + " \"" + PerfCurve( CurveIndex ).Name + "\": Insufficient number of independent variables warning continues...", PerfCurve( CurveIndex ).NumIVLowErrorIndex, 4.0, 4.0 );
			}
			V5 = 0.0;
		}

		{ auto const SELECT_CASE_var( TableLookup( TableIndex ).NumIndependentVars );
		if ( SELECT_CASE_var == 1 ) {
			NX = TableLookup( TableIndex ).NumX1Vars;
			NY = 1;
			NUMPT = TableLookup( TableIndex ).InterpolationOrder;
			VALSX.allocate( NX );
			VALSX = TableLookup( TableIndex ).X1Var;
			TableValue = DLAG( V1, VALSX( 1 ), VALSX, VALSX, TableLookup( TableIndex ).TableLookupZData( 1, 1, 1, _, _ ), NX, NY, NUMPT, IEXTX, IEXTY );
			VALSX.deallocate();
		} else if ( SELECT_CASE_var == 2 ) {
			NX = TableLookup( TableIndex ).NumX1Vars;
			NY = TableLookup( TableIndex ).NumX2Vars;
			NUMPT = TableLookup( TableIndex ).InterpolationOrder;
			VALSX.allocate( NX );
			VALSX = TableLookup( TableIndex ).X1Var;
			VALSY.allocate( NY );
			VALSY = TableLookup( TableIndex ).X2Var;
			TableValue = DLAG( V1, V2, VALSX, VALSY, TableLookup( TableIndex ).TableLookupZData( 1, 1, 1, _, _ ), NX, NY, NUMPT, IEXTX, IEXTY );
			VALSX.deallocate();
			VALSY.deallocate();
		} else if ( SELECT_CASE_var == 3 ) {
			NX = TableLookup( TableIndex ).NumX1Vars;
			NY = TableLookup( TableIndex ).NumX2Vars;
			NV3 = TableLookup( TableIndex ).NumX3Vars;
			NUMPT = TableLookup( TableIndex ).InterpolationOrder;
			VALSX.allocate( NX );
			VALSX = TableLookup( TableIndex ).X1Var;
			VALSY.allocate( NY );
			VALSY = TableLookup( TableIndex ).X2Var;
			VALSV3.allocate( NV3 );
			VALSV3 = TableLookup( TableIndex ).X3Var;
			TWODVALS.allocate( 1, NV3 );
			// perform 2-D interpolation of X (V1) and Y (V2) and save in 2-D array
			for ( IV3 = 1; IV3 <= NV3; ++IV3 ) {
				TWODVALS( 1, IV3 ) = DLAG( V1, V2, VALSX, VALSY, TableLookup( TableIndex ).TableLookupZData( 1, 1, IV3, _, _ ), NX, NY, NUMPT, IEXTX, IEXTY );
			}
			if ( NV3 == 1 ) {
				TableValue = TWODVALS( 1, 1 );
			} else {
				TableValue = DLAG( V3, 1.0, VALSV3, VALSV3, TWODVALS, NV3, 1, NUMPT, IEXTV3, IEXTV4 );
			}
			TWODVALS.deallocate();
			VALSX.deallocate();
			VALSY.deallocate();
			VALSV3.deallocate();
		} else if ( SELECT_CASE_var == 4 ) {
			NX = TableLookup( TableIndex ).NumX1Vars;
			NY = TableLookup( TableIndex ).NumX2Vars;
			NV3 = TableLookup( TableIndex ).NumX3Vars;
			NV4 = TableLookup( TableIndex ).NumX4Vars;
			NUMPT = TableLookup( TableIndex ).InterpolationOrder;
			VALSX.allocate( NX );
			VALSX = TableLookup( TableIndex ).X1Var;
			VALSY.allocate( NY );
			VALSY = TableLookup( TableIndex ).X2Var;
			VALSV3.allocate( NV3 );
			VALSV3 = TableLookup( TableIndex ).X3Var;
			VALSV4.allocate( NV4 );
			VALSV4 = TableLookup( TableIndex ).X4Var;
			TWODVALS.allocate( NV4, NV3 );
			// perform 2-D interpolation of X (V1) and Y (V2) and save in 2-D array
			for ( IV4 = 1; IV4 <= NV4; ++IV4 ) {
				for ( IV3 = 1; IV3 <= NV3; ++IV3 ) {
					TWODVALS( IV4, IV3 ) = DLAG( V1, V2, VALSX, VALSY, TableLookup( TableIndex ).TableLookupZData( 1, IV4, IV3, _, _ ), NX, NY, NUMPT, IEXTX, IEXTY );
				}
			}
			// final interpolation of 2-D array in V3 and V4
			TableValue = DLAG( V3, V4, VALSV3, VALSV4, TWODVALS, NV3, NV4, NUMPT, IEXTV3, IEXTV4 );
			TWODVALS.deallocate();
			VALSX.deallocate();
			VALSY.deallocate();
			VALSV3.deallocate();
			VALSV4.deallocate();
		} else if ( SELECT_CASE_var == 5 ) {
			NX = TableLookup( TableIndex ).NumX1Vars;
			NY = TableLookup( TableIndex ).NumX2Vars;
			NV3 = TableLookup( TableIndex ).NumX3Vars;
			NV4 = TableLookup( TableIndex ).NumX4Vars;
			NV5 = TableLookup( TableIndex ).NumX5Vars;
			NUMPT = TableLookup( TableIndex ).InterpolationOrder;
			VALSX.allocate( NX );
			VALSX = TableLookup( TableIndex ).X1Var;
			VALSY.allocate( NY );
			VALSY = TableLookup( TableIndex ).X2Var;
			VALSV3.allocate( NV3 );
			VALSV3 = TableLookup( TableIndex ).X3Var;
			VALSV4.allocate( NV4 );
			VALSV4 = TableLookup( TableIndex ).X4Var;
			VALSV5.allocate( NV5 );
			VALSV5 = TableLookup( TableIndex ).X5Var;
			THREEDVALS.allocate( NV5, NV4, NV3 );
			for ( IV5 = 1; IV5 <= NV5; ++IV5 ) {
				for ( IV4 = 1; IV4 <= NV4; ++IV4 ) {
					for ( IV3 = 1; IV3 <= NV3; ++IV3 ) {
						THREEDVALS( IV5, IV4, IV3 ) = DLAG( V1, V2, VALSX, VALSY, TableLookup( TableIndex ).TableLookupZData( IV5, IV4, IV3, _, _ ), NX, NY, NUMPT, IEXTX, IEXTY );
					}
				}
			}
			TWODVALS.allocate( 1, NV5 );
			for ( IV5 = 1; IV5 <= NV5; ++IV5 ) {
				TWODVALS( 1, IV5 ) = DLAG( V3, V4, VALSV3, VALSV4, THREEDVALS( IV5, _, _ ), NV3, NV4, NUMPT, IEXTX, IEXTY );
			}
			if ( NV5 == 1 ) {
				TableValue = TWODVALS( 1, 1 );
			} else {
				TableValue = DLAG( V5, 1.0, VALSV5, VALSV5, TWODVALS, NV5, 1, NUMPT, IEXTV5, IEXTV4 );
			}
			TWODVALS.deallocate();
			THREEDVALS.deallocate();
			VALSX.deallocate();
			VALSY.deallocate();
			VALSV3.deallocate();
			VALSV4.deallocate();
			VALSV5.deallocate();
		} else {
			TableValue = 0.0;
			ShowSevereError( "Errors found in table output calculation for " + PerfCurve( CurveIndex ).Name );
			ShowContinueError( "...Possible causes are selection of Interpolation Method or Type or Number of Independent Variables or Points." );
			ShowFatalError( "PerformanceTableObject: Previous error causes program termination." );
		}}

		if ( PerfCurve( CurveIndex ).CurveMinPresent ) TableValue = max( TableValue, PerfCurve( CurveIndex ).CurveMin );
		if ( PerfCurve( CurveIndex ).CurveMaxPresent ) TableValue = min( TableValue, PerfCurve( CurveIndex ).CurveMax );

		return TableValue;

	}

	void
	SolveRegression(
		int & CurveNum, // index to performance curve
		std::string & TableType, // tabular data object type
		std::string & CurveName, // performance curve name
		Array1S< Real64 > RawDataX, // table data X values (1st independent variable)
		Array1S< Real64 > RawDataY, // table data Y values (dependent variables)
		Optional< Array1S< Real64 > > RawDataX2 // table data X2 values (2nd independent variable)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   June 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Given the curve index and the values of 1 or 2 independent variables,
		// calls the curve or table routine to return the value of an equipment performance curve or table.
		// The solution requires use of linear algebra and forms a matrix of sums of the data set.
		// For a linear equation of the form Z = a + bX + cX^2, the general solution is as follows.
		// Phi = SUM1ToN[Zi - f(Xi)]^2 = SUM1ToN[Zi - (a+bX+cX^2)]^2 = minimum
		// substitue Y = X^2 in the equations above.
		// then set up the partials of Phi with respect to a, the partial of Phi with respect to b, etc.
		// PartialPhiRespectToa = 2 * SUM1ToN[1*(Zi-(a+bXi+cYi))] = 0
		// PartialPhiRespectTob = 2 * SUM1ToN[Xi(Zi-(a+bXi+cYi))] = 0
		// PartialPhiRespectTob = 2 * SUM1ToN[Yi(Zi-(a+bXi+cYi))] = 0
		// then set up the square matrix by solving the above partials.
		// SUM1ToN(Zi)   = a * SUM1ToN(1)  + b * SUM1ToN(Xi)   + c * SUM1ToN(Yi)
		// SUM1ToN(ZiXi) = a * SUM1ToN(Xi) + b * SUM1ToN(Xi)^2 + c * SUM1ToN(XiYi)
		// SUM1ToN(ZiYi) = a * SUM1ToN(Yi) + b * SUM1ToN(XiYi) + c * SUM1ToN(Yi)^2
		// the matirx (A) is then the 3x3 matrix on the right, with a solution of the 1x3 matrix on the left
		// Note symmetry about the diagonal.
		// (i.e., A(1,2)=A(2,1), A(1,3)=A(3,1), A(3,2)=A(2,3), and diagonal are all squared terms)
		//      _                                          _              _              _
		//     |  SUM1ToN(1)   SUM1ToN(Xi)   SUM1ToN(Yi)    |            |  SUM1ToN(Zi)   |
		// A = |  SUM1ToN(Xi)  SUM1ToN(Xi)^2 SUM1ToN(XiYi)  |  Results = |  SUM1ToN(ZiXi) |
		//     |_ SUM1ToN(Yi)  SUM1ToN(XiYi) SUM1ToN(Yi)^2 _|            |_ SUM1ToN(ZiYi)_|
		// The linear algebra equation is then solved using foward elimination and reverse substitution
		// This solution (Results) provides the coefficients of the associated performance curve (a,b,and c in the eq. above).
		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using General::TrimSigDigits;
		using DataGlobals::DisplayAdvancedReportVariables;
		using DataGlobals::OutputFileInits;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 X; // linear algebra equation coefficients
		Real64 X2;
		Real64 Y;
		Real64 V;
		Real64 U;
		Real64 T;
		Real64 Z;
		int MatrixSize; // square matrix array size (MatrixSize,MatrixSize)
		int LoopCount; // loop counter
		int N; // loop variables
		int i;
		int j;
		int k;
		Real64 C; // intermediate calculation of a constant in matrix solution
		Real64 sX; // sum of the X
		Real64 sX2; // sum of the X^2
		Real64 sX3; // sum of the X^3
		Real64 sY; // sum of the Y
		Real64 sY2; // sum of the Y^2
		Real64 sV; // sum of the V
		Real64 sV2; // sum of the V^2
		Real64 sU; // sum of the U
		Real64 sU2; // sum of the U^2
		Real64 sT; // sum of the T
		Real64 sT2; // sum of the T^2
		Real64 sXY; // sum of the XY
		Real64 sXV; // sum of the XV
		Real64 sXU; // sum of the XU
		Real64 sXT; // sum of the XT
		Real64 sYV; // sum of the TV
		Real64 sYU; // sum of the YU
		Real64 sYT; // sum of the YT
		Real64 sVU; // sum of the VU
		Real64 sVT; // sum of the VT
		Real64 sUT; // sum of the UT
		Real64 Results1; // regression coefficient #1
		Real64 Results2; // regression coefficient #2
		Real64 Results3; // regression coefficient #3
		Real64 Results4; // regression coefficient #4
		Real64 Results5; // regression coefficient #5
		Real64 Results6; // regression coefficient #6
		Real64 MinX; // equation variable min/max statistics
		Real64 MaxX;
		Real64 MinX2;
		Real64 MaxX2;
		Real64 MinY;
		Real64 MaxY;
		Real64 Mean; // statistical parameters
		Real64 RSquared;
		Real64 StandardError;
		Real64 Est( 0.0 );
		Array1D< Real64 > Results; // performance curve coefficients
		Array2D< Real64 > A; // linear algebra matrix
		std::string StrCurve; // string representation of curve type
		static bool WriteHeaderOnce( true );
		bool EchoTableDataToEio; // logical set equal to global and used to report to eio file

		// Formats
		static gio::Fmt Format_110( "('! <CREATING NEW CURVE OBJECT>')" );
		static gio::Fmt Format_130( "('CREATING NEW CURVE OBJECT')" );
		static gio::Fmt Format_140( "('! Input as ',A,' \"',A,'\"')" );
		static gio::Fmt Format_150( "('! RSquared       = ',A)" );
		static gio::Fmt Format_160( "('! Standard Error = ',A)" );
		static gio::Fmt Format_170( "('! Sample Size    = ',A)" );
		static gio::Fmt Format_180( "('Curve:',A,',')" );
		static gio::Fmt Format_190( "('FromTable_',A,',  !- Name')" );
		static gio::Fmt Format_200( "('  ',A,',  !- Coefficient1 Constant')" );
		static gio::Fmt Format_210( "('  ',A,',  !- Coefficient2 x')" );
		static gio::Fmt Format_300( "('  ',A,',  !- Minimum Value of x')" );
		static gio::Fmt Format_310( "('  ',A,',  !- Maximum Value of x')" );
		static gio::Fmt Format_340( "('  ',A,',  !- Minimum Curve Output')" );
		static gio::Fmt Format_350( "('  ',A,';  !- Maximum Curve Output')" );
		static gio::Fmt Format_360( "('END CREATING NEW CURVE OBJECT')" );
		static gio::Fmt Format_220( "('  ',A,',  !- Coefficient3 x**2')" );
		static gio::Fmt Format_230( "('  ',A,',  !- !- Coefficient4 x**3')" );
		static gio::Fmt Format_240( "('  ',A,',  !- Coefficient4 y')" );
		static gio::Fmt Format_250( "('  ',A,',  !- !- Coefficient5 x**4')" );
		static gio::Fmt Format_260( "('  ',A,',  !- Coefficient5 y**2')" );
		static gio::Fmt Format_270( "('  ',A,',  !- Coefficient5 xy')" );
		static gio::Fmt Format_280( "('  ',A,',  !- Coefficient6 x*y')" );
		static gio::Fmt Format_290( "('  ',A,',  !- Coefficient6 x**2y')" );
		static gio::Fmt Format_320( "('  ',A,',  !- Minimum Value of y')" );
		static gio::Fmt Format_330( "('  ',A,',  !- Maximum Value of y')" );

		EchoTableDataToEio = DisplayAdvancedReportVariables;

		{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
		if ( SELECT_CASE_var == Linear ) {
			MatrixSize = 2;
			StrCurve = "Linear";
		} else if ( SELECT_CASE_var == Quadratic ) {
			MatrixSize = 3;
			StrCurve = "Quadratic";
		} else if ( SELECT_CASE_var == Cubic ) {
			MatrixSize = 4;
			StrCurve = "Cubic";
		} else if ( SELECT_CASE_var == Quartic ) {
			MatrixSize = 5;
			StrCurve = "Quartic";
		} else if ( SELECT_CASE_var == BiQuadratic ) {
			MatrixSize = 6;
			StrCurve = "BiQuadratic";
		} else if ( SELECT_CASE_var == QuadraticLinear ) {
			MatrixSize = 6;
			StrCurve = "QuadraticLinear";
		} else {
			return;
		}}

		if ( isize( RawDataX ) < MatrixSize ) {
			{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).ObjectType );
			if ( SELECT_CASE_var == CurveType_TableOneIV ) {
				ShowSevereError( "TABLE:ONEINDEPENDENTVARIABLE: \"" + PerfCurve( CurveNum ).Name + "\"" );
			} else if ( SELECT_CASE_var == CurveType_TableTwoIV ) {
				ShowSevereError( "TABLE:TWOINDEPENDENTVARIABLES: \"" + PerfCurve( CurveNum ).Name + "\"" );
			} else if ( SELECT_CASE_var == CurveType_TableMultiIV ) {
				ShowSevereError( "TABLE:MULTIVARIABLELOOKUP: \"" + PerfCurve( CurveNum ).Name + "\"" );
			} else {
				ShowSevereError( "SOLVEREGRESSION: Incorrect object type with name = " + PerfCurve( CurveNum ).Name + "\"" );
			}}
			ShowContinueError( "Insufficient data to calculate regression coefficients." );
			ShowContinueError( "Required data pairs = " + RoundSigDigits( MatrixSize ) );
			ShowContinueError( "Entered data pairs  = " + RoundSigDigits( size( RawDataX ) ) );
			ShowContinueError( "Setting interpolation type equal to LinearInterpolationOfTable and simulation continues." );
			PerfCurve( CurveNum ).InterpolationType = LinearInterpolationOfTable;
			return;
		}

		Results.dimension( MatrixSize, 0.0 );
		A.allocate( MatrixSize, MatrixSize );
		//   ' Sum data
		N = 0;
		sX = 0.0;
		sX2 = 0.0;
		sY = 0.0;
		sY2 = 0.0;
		sV = 0.0;
		sV2 = 0.0;
		sU = 0.0;
		sU2 = 0.0;
		sT = 0.0;
		sT2 = 0.0;
		sXY = 0.0;
		sXV = 0.0;
		sXU = 0.0;
		sXT = 0.0;
		sYV = 0.0;
		sYU = 0.0;
		sYT = 0.0;
		sVU = 0.0;
		sVT = 0.0;
		sUT = 0.0;
		Results = 0.0;
		Results1 = 0.0;
		Results2 = 0.0;
		Results3 = 0.0;
		Results4 = 0.0;
		Results5 = 0.0;
		Results6 = 0.0;
		X2 = 0.0;
		Y = 0.0;
		V = 0.0;
		U = 0.0;
		T = 0.0;
		for ( LoopCount = 1; LoopCount <= isize( RawDataX ); ++LoopCount ) {
			X = RawDataX( LoopCount );
			if ( present( RawDataX2 ) ) X2 = RawDataX2()( LoopCount );
			{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
			if ( ( SELECT_CASE_var == Linear ) || ( SELECT_CASE_var == Quadratic ) || ( SELECT_CASE_var == Cubic ) || ( SELECT_CASE_var == Quartic ) ) {
				Y = X * X;
				V = X * Y;
				U = X * V;
			} else if ( SELECT_CASE_var == BiQuadratic ) {
				Y = X * X;
				V = X2;
				U = V * V;
				T = X * X2;
			} else if ( SELECT_CASE_var == QuadraticLinear ) {
				Y = X * X;
				V = X2;
				U = X * V;
				T = Y * X2;
			} else {
			}}
			Z = RawDataY( LoopCount );
			++N; // Count
			sX += X; // Sum X
			sX2 += X * X; // Sum X*X
			sY += Y; // Sum Y
			sY2 += Y * Y; // Sum Y*Y
			sV += V; // Sum V
			sV2 += V * V; // Sum V*V
			sU += U; // Sum U
			sU2 += U * U; // Sum U*U
			sT += T; // Sum T
			sT2 += T * T; // Sum T*T
			sXY += X * Y; // Sum XY
			sXV += X * V; // Sum XV
			sXU += X * U; // Sum XU
			sXT += X * T; // Sum XT
			sYV += Y * V; // Sum YV
			sYU += Y * U; // Sum YU
			sYT += Y * T; // Sum YT
			sVU += V * U; // Sum VU
			sVT += V * T; // Sum VT
			sUT += U * T; // Sum UT
			Results1 += Z; // Sum Z
			Results2 += Z * X; // Sum ZX
			Results3 += Z * Y; // Sum ZY
			Results4 += Z * V; // Sum ZV
			Results5 += Z * U; // Sum ZU
			Results6 += Z * T; // Sum ZT
		}

		Results( 1 ) = Results1;
		Results( 2 ) = Results2;
		{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
		if ( SELECT_CASE_var == Linear ) {
		} else if ( SELECT_CASE_var == Quadratic ) {
			Results( 3 ) = Results3;
		} else if ( SELECT_CASE_var == Cubic ) {
			Results( 3 ) = Results3;
			Results( 4 ) = Results4;
		} else if ( SELECT_CASE_var == Quartic ) {
			Results( 3 ) = Results3;
			Results( 4 ) = Results4;
			Results( 5 ) = Results5;
		} else if ( ( SELECT_CASE_var == BiQuadratic ) || ( SELECT_CASE_var == QuadraticLinear ) ) {
			Results( 3 ) = Results3;
			Results( 4 ) = Results4;
			Results( 5 ) = Results5;
			Results( 6 ) = Results6;
		}}

		Mean = Results( 1 ) / N;

		//    ' Form "A" Matrix
		A( 1, 1 ) = double( N );
		A( 2, 1 ) = sX;
		A( 2, 2 ) = sX2;
		{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
		if ( SELECT_CASE_var == Linear ) {
		} else if ( SELECT_CASE_var == Quadratic ) {
			A( 3, 1 ) = sY;
			A( 3, 2 ) = sXY;
			A( 3, 3 ) = sY2;
		} else if ( SELECT_CASE_var == Cubic ) {
			A( 3, 1 ) = sY;
			A( 4, 1 ) = sV;
			A( 3, 2 ) = sXY;
			A( 4, 2 ) = sXV;
			A( 3, 3 ) = sY2;
			A( 4, 3 ) = sYV;
			A( 4, 4 ) = sV2;
		} else if ( SELECT_CASE_var == Quartic ) {
			A( 3, 1 ) = sY;
			A( 4, 1 ) = sV;
			A( 5, 1 ) = sU;
			A( 3, 2 ) = sXY;
			A( 4, 2 ) = sXV;
			A( 5, 2 ) = sXU;
			A( 3, 3 ) = sY2;
			A( 4, 3 ) = sYV;
			A( 5, 3 ) = sYU;
			A( 4, 4 ) = sV2;
			A( 5, 4 ) = sVU;
			A( 5, 5 ) = sU2;
		} else if ( ( SELECT_CASE_var == BiQuadratic ) || ( SELECT_CASE_var == QuadraticLinear ) ) {
			A( 3, 1 ) = sY;
			A( 4, 1 ) = sV;
			A( 5, 1 ) = sU;
			A( 6, 1 ) = sT;
			A( 3, 2 ) = sXY;
			A( 4, 2 ) = sXV;
			A( 5, 2 ) = sXU;
			A( 6, 2 ) = sXT;
			A( 3, 3 ) = sY2;
			A( 4, 3 ) = sYV;
			A( 5, 3 ) = sYU;
			A( 6, 3 ) = sYT;
			A( 4, 4 ) = sV2;
			A( 5, 4 ) = sVU;
			A( 6, 4 ) = sVT;
			A( 5, 5 ) = sU2;
			A( 6, 5 ) = sUT;
			A( 6, 6 ) = sT2;
		} else {
		}}

		//  copy elements to bottom half of symmetrical square matrix
		for ( i = 1; i <= MatrixSize - 1; ++i ) {
			for ( j = i + 1; j <= MatrixSize; ++j ) {
				A( i, j ) = A( j, i );
			}
		}

		//   Forward Eliminiation
		for ( i = 1; i <= MatrixSize - 1; ++i ) {
			if ( A( i, i ) == 0.0 ) {
				ShowSevereError( "SolveRegression: Zero value on the diagonal." );
				ShowContinueError( "Setting interpolation type equal to LinearInterpolationOfTable and simulation continues." );
				PerfCurve( CurveNum ).InterpolationType = LinearInterpolationOfTable;
				return;
			}
			for ( j = i + 1; j <= MatrixSize; ++j ) {
				//      find the ratio of the element to the one above it
				C = A( i, j ) / A( i, i );
				//      replace the element by reducing it by the ratio multiplied by the element above it
				//      this makes the bottom half of symmetrical square matix 0's
				for ( k = i; k <= MatrixSize; ++k ) {
					A( k, j ) -= C * A( k, i );
				}
				Results( j ) -= C * Results( i );
			}
		}

		//    ' Back Substitution
		if ( A( MatrixSize, MatrixSize ) == 0.0 ) {
			ShowSevereError( "SolveRegression: Zero value on the diagonal end point." );
			ShowContinueError( "Setting interpolation type equal to LinearInterpolationOfTable and simulation continues." );
			PerfCurve( CurveNum ).InterpolationType = LinearInterpolationOfTable;
			return;
		}
		//  now starting at the lower right corner of the matrix solve for the last coefficient
		Results( MatrixSize ) /= A( MatrixSize, MatrixSize );
		//  substitute that coefficient back into the equation above it and solve for the 2nd to last coefficient
		//  proceed until all coefficients are found
		for ( i = MatrixSize - 1; i >= 1; --i ) {
			C = Results( i );
			for ( j = 1; j <= MatrixSize - i; ++j ) {
				C -= A( i + j, i ) * Results( i + j );
			}
			Results( i ) = C / A( i, i );
		}

		//  calculate the regression statistics
		sX = 0.0;
		sX2 = 0.0;
		sX3 = 0.0;
		MinX = 9999999.0;
		MaxX = -9999999.0;
		MinX2 = 9999999.0;
		MaxX2 = -9999999.0;
		MinY = 9999999.0;
		MaxY = -9999999.0;
		for ( LoopCount = 1; LoopCount <= N; ++LoopCount ) {
			X = RawDataX( LoopCount );
			if ( present( RawDataX2 ) ) X2 = RawDataX2()( LoopCount );
			{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
			if ( ( SELECT_CASE_var == Linear ) || ( SELECT_CASE_var == Quadratic ) || ( SELECT_CASE_var == Cubic ) || ( SELECT_CASE_var == Quartic ) ) {
				Y = X * X;
				V = X * Y;
				U = X * V;
			} else if ( SELECT_CASE_var == BiQuadratic ) {
				Y = X * X;
				V = X2;
				U = V * V;
				T = X * X2;
			} else if ( SELECT_CASE_var == QuadraticLinear ) {
				Y = X * X;
				V = X2;
				U = X * V;
				T = Y * X2;
			} else {
			}}
			Z = RawDataY( LoopCount );
			if ( MinX > X ) MinX = X;
			if ( MaxX < X ) MaxX = X;
			if ( MinX2 > X2 ) MinX2 = X2;
			if ( MaxX2 < X2 ) MaxX2 = X2;
			if ( MinY > Z ) MinY = Z;
			if ( MaxY < Z ) MaxY = Z;

			{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
			if ( SELECT_CASE_var == Linear ) {
				Est = Results( 1 ) + X * Results( 2 );
			} else if ( SELECT_CASE_var == Quadratic ) {
				Est = Results( 1 ) + X * Results( 2 ) + Y * Results( 3 );
			} else if ( SELECT_CASE_var == Cubic ) {
				Est = Results( 1 ) + X * Results( 2 ) + Y * Results( 3 ) + V * Results( 4 );
			} else if ( SELECT_CASE_var == Quartic ) {
				Est = Results( 1 ) + X * Results( 2 ) + Y * Results( 3 ) + V * Results( 4 ) + U * Results( 5 );
			} else if ( ( SELECT_CASE_var == BiQuadratic ) || ( SELECT_CASE_var == QuadraticLinear ) ) {
				Est = Results( 1 ) + X * Results( 2 ) + Y * Results( 3 ) + V * Results( 4 ) + U * Results( 5 ) + T * Results( 6 );
			} else {
			}}
			sX += ( Est - Mean ) * ( Est - Mean );
			sX2 += ( Z - Mean ) * ( Z - Mean );
			sX3 += ( Z - Est ) * ( Z - Est );
		}
		if ( sX2 != 0.0 ) {
			RSquared = sX / sX2;
		} else {
			RSquared = 0.0;
		}
		if ( N > MatrixSize ) {
			StandardError = std::sqrt( sX3 / ( N - MatrixSize ) );
		} else {
			StandardError = 0.0;
		}

		{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).InterpolationType );
		if ( SELECT_CASE_var == LinearInterpolationOfTable ) {
		} else if ( SELECT_CASE_var == EvaluateCurveToLimits ) {
			MinX = min( MinX, PerfCurve( CurveNum ).Var1Min );
			MaxX = max( MaxX, PerfCurve( CurveNum ).Var1Max );
			MinX2 = min( MinX2, PerfCurve( CurveNum ).Var2Min );
			MaxX2 = max( MaxX2, PerfCurve( CurveNum ).Var2Max );
			MinY = min( MinY, PerfCurve( CurveNum ).CurveMin );
			MaxY = max( MaxY, PerfCurve( CurveNum ).CurveMax );
		} else {
		}}

		// echo new curve object to eio file
		if ( EchoTableDataToEio ) {
			if ( WriteHeaderOnce ) {
				gio::write( OutputFileInits, Format_110 );
				WriteHeaderOnce = false;
			}

			gio::write( OutputFileInits, Format_130 );
			gio::write( OutputFileInits, Format_140 ) << TableType << CurveName;
			gio::write( OutputFileInits, Format_150 ) << RoundSigDigits( RSquared, 10 );
			gio::write( OutputFileInits, Format_160 ) << RoundSigDigits( StandardError, 10 );
			gio::write( OutputFileInits, Format_170 ) << TrimSigDigits( N );
			gio::write( OutputFileInits, Format_180 ) << StrCurve;
			gio::write( OutputFileInits, Format_190 ) << CurveName;
			gio::write( OutputFileInits, Format_200 ) << RoundSigDigits( Results( 1 ), 10 );
			{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
			if ( ( SELECT_CASE_var == Linear ) || ( SELECT_CASE_var == Quadratic ) || ( SELECT_CASE_var == Cubic ) || ( SELECT_CASE_var == Quartic ) || ( SELECT_CASE_var == BiQuadratic ) || ( SELECT_CASE_var == QuadraticLinear ) ) {
				gio::write( OutputFileInits, Format_210 ) << RoundSigDigits( Results( 2 ), 10 );
			} else {
			}}
			{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
			if ( ( SELECT_CASE_var == Quadratic ) || ( SELECT_CASE_var == Cubic ) || ( SELECT_CASE_var == Quartic ) || ( SELECT_CASE_var == BiQuadratic ) || ( SELECT_CASE_var == QuadraticLinear ) ) {
				gio::write( OutputFileInits, Format_220 ) << RoundSigDigits( Results( 3 ), 10 );
			} else {
			}}
			{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
			if ( ( SELECT_CASE_var == Cubic ) || ( SELECT_CASE_var == Quartic ) ) {
				gio::write( OutputFileInits, Format_230 ) << RoundSigDigits( Results( 4 ), 10 );
			} else if ( ( SELECT_CASE_var == BiQuadratic ) || ( SELECT_CASE_var == QuadraticLinear ) ) {
				gio::write( OutputFileInits, Format_240 ) << RoundSigDigits( Results( 4 ), 10 );
			} else {
			}}
			{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
			if ( SELECT_CASE_var == Quartic ) {
				gio::write( OutputFileInits, Format_250 ) << RoundSigDigits( Results( 5 ), 10 );
			} else if ( SELECT_CASE_var == BiQuadratic ) {
				gio::write( OutputFileInits, Format_260 ) << RoundSigDigits( Results( 5 ), 10 );
			} else if ( SELECT_CASE_var == QuadraticLinear ) {
				gio::write( OutputFileInits, Format_270 ) << RoundSigDigits( Results( 5 ), 10 );
			} else {
			}}
			{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
			if ( SELECT_CASE_var == BiQuadratic ) {
				gio::write( OutputFileInits, Format_280 ) << RoundSigDigits( Results( 6 ), 10 );
			} else if ( SELECT_CASE_var == QuadraticLinear ) {
				gio::write( OutputFileInits, Format_290 ) << RoundSigDigits( Results( 6 ), 10 );
			} else {
			}}
			gio::write( OutputFileInits, Format_300 ) << RoundSigDigits( MinX, 10 );
			gio::write( OutputFileInits, Format_310 ) << RoundSigDigits( MaxX, 10 );
			{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
			if ( SELECT_CASE_var == Quartic ) {
			} else if ( ( SELECT_CASE_var == BiQuadratic ) || ( SELECT_CASE_var == QuadraticLinear ) ) {
				gio::write( OutputFileInits, Format_320 ) << RoundSigDigits( MinX2, 10 );
				gio::write( OutputFileInits, Format_330 ) << RoundSigDigits( MaxX2, 10 );
			} else {
			}}
			gio::write( OutputFileInits, Format_340 ) << RoundSigDigits( MinY, 10 );
			gio::write( OutputFileInits, Format_350 ) << RoundSigDigits( MaxY, 10 );
			gio::write( OutputFileInits, Format_360 );
		}

		// save results in performance curve structure
		{ auto const SELECT_CASE_var( PerfCurve( CurveNum ).CurveType );
		if ( SELECT_CASE_var == Linear ) {
			PerfCurve( CurveNum ).Coeff1 = Results( 1 );
			PerfCurve( CurveNum ).Coeff2 = Results( 2 );
		} else if ( SELECT_CASE_var == Quadratic ) {
			PerfCurve( CurveNum ).Coeff1 = Results( 1 );
			PerfCurve( CurveNum ).Coeff2 = Results( 2 );
			PerfCurve( CurveNum ).Coeff3 = Results( 3 );
		} else if ( SELECT_CASE_var == Cubic ) {
			PerfCurve( CurveNum ).Coeff1 = Results( 1 );
			PerfCurve( CurveNum ).Coeff2 = Results( 2 );
			PerfCurve( CurveNum ).Coeff3 = Results( 3 );
			PerfCurve( CurveNum ).Coeff4 = Results( 4 );
		} else if ( SELECT_CASE_var == Quartic ) {
			PerfCurve( CurveNum ).Coeff1 = Results( 1 );
			PerfCurve( CurveNum ).Coeff2 = Results( 2 );
			PerfCurve( CurveNum ).Coeff3 = Results( 3 );
			PerfCurve( CurveNum ).Coeff4 = Results( 4 );
			PerfCurve( CurveNum ).Coeff5 = Results( 5 );
		} else if ( ( SELECT_CASE_var == BiQuadratic ) || ( SELECT_CASE_var == QuadraticLinear ) ) {
			PerfCurve( CurveNum ).Coeff1 = Results( 1 );
			PerfCurve( CurveNum ).Coeff2 = Results( 2 );
			PerfCurve( CurveNum ).Coeff3 = Results( 3 );
			PerfCurve( CurveNum ).Coeff4 = Results( 4 );
			PerfCurve( CurveNum ).Coeff5 = Results( 5 );
			PerfCurve( CurveNum ).Coeff6 = Results( 6 );
		} else {
		}}

		PerfCurve( CurveNum ).Var1Min = MinX;
		PerfCurve( CurveNum ).Var1Max = MaxX;
		PerfCurve( CurveNum ).Var2Min = MinX2;
		PerfCurve( CurveNum ).Var2Max = MaxX2;
		PerfCurve( CurveNum ).CurveMin = MinY;
		PerfCurve( CurveNum ).CurveMax = MaxY;
		PerfCurve( CurveNum ).CurveMinPresent = true;
		PerfCurve( CurveNum ).CurveMaxPresent = true;

		A.deallocate();
		Results.deallocate();

	}

	void
	Interpolate_Lagrange(
		Real64 const DataPoint, // point used for interpolating output (x)
		Array1S< Real64 > FunctionArray, // array of output data (Y's)
		Array1S< Real64 > Ordinate, // array of input data (X's)
		int const ISPT, // the starting point in the interpolated array
		int const IEPT, // the ending point in the interpolated array
		Real64 & ALAG // the interpolated output (y or F(x) in equation above)
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Raustad, FSEC
		//       DATE WRITTEN   July 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Solves the lagrange polynomial equation for interpolation. For second-order:
		// F(x) = y1 * ((x-x2)(x-x3) / (x1-x2)(x1-x3)) +
		//        y2 * ((x-x1)(x-x3) / (x2-x1)(x2-x3)) +
		//        y3 * ((x-x1)(x-x2) / (x3-x1)(x3-x2))
		// where xn, yn represent data points 1-n, and x represents the interpolation point.
		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//        DIMENSION FunctionAry(IEPT),Ordinate(IEPT)
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Lagrange; // intermediate variable

		ALAG = 0.0;
		for ( int J = ISPT; J <= IEPT; ++J ) {
			Lagrange = 1.0;
			Real64 const Ordinate_J( Ordinate( J ) );
			for ( int K = ISPT; K <= IEPT; ++K ) {
				if ( K != J ) {
					Lagrange *= ( ( DataPoint - Ordinate( K ) ) / ( Ordinate_J - Ordinate( K ) ) );
				}
			}
			ALAG += Lagrange * FunctionArray( J );
		}
	}

	bool
	IsCurveInputTypeValid( std::string const & InInputType ) // index of curve in curve array
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   Oct 2009
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Returns true if the input unit type is valid

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;

		// Return value
		bool IsCurveInputTypeValid;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		if ( len( InInputType ) > 0 ) {
			if ( SameString( InInputType, "DIMENSIONLESS" ) ) {
				IsCurveInputTypeValid = true;
			} else if ( SameString( InInputType, "TEMPERATURE" ) ) {
				IsCurveInputTypeValid = true;
			} else if ( SameString( InInputType, "PRESSURE" ) ) { //cpw22Aug2010
				IsCurveInputTypeValid = true; //cpw22Aug2010
				// CR8124 Glazer - Need to use volumetricflow and massflow not just flow
				//  ELSEIF (SameString(InInputType,'FLOW')) THEN
				//    IsCurveInputTypeValid = .TRUE.
			} else if ( SameString( InInputType, "VOLUMETRICFLOW" ) ) {
				IsCurveInputTypeValid = true;
			} else if ( SameString( InInputType, "MASSFLOW" ) ) {
				IsCurveInputTypeValid = true;
			} else if ( SameString( InInputType, "POWER" ) ) {
				IsCurveInputTypeValid = true;
			} else if ( SameString( InInputType, "DISTANCE" ) ) {
				IsCurveInputTypeValid = true;
			} else {
				IsCurveInputTypeValid = false;
			}
		} else {
			IsCurveInputTypeValid = true; //if not used it is valid
		}
		return IsCurveInputTypeValid;
	}

	bool
	IsCurveOutputTypeValid( std::string const & InOutputType ) // index of curve in curve array
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   Oct 2009
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Returns true if the output unit type is valid

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::SameString;

		// Return value
		bool IsCurveOutputTypeValid;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( SameString( InOutputType, "DIMENSIONLESS" ) ) {
			IsCurveOutputTypeValid = true;
		} else if ( SameString( InOutputType, "PRESSURE" ) ) { //cpw22Aug2010
			IsCurveOutputTypeValid = true; //cpw22Aug2010
		} else if ( SameString( InOutputType, "TEMPERATURE" ) ) {
			IsCurveOutputTypeValid = true;
		} else if ( SameString( InOutputType, "CAPACITY" ) ) {
			IsCurveOutputTypeValid = true;
		} else if ( SameString( InOutputType, "POWER" ) ) {
			IsCurveOutputTypeValid = true;
		} else {
			IsCurveOutputTypeValid = false;
		}
		return IsCurveOutputTypeValid;
	}

	std::string
	GetCurveType( int const CurveIndex ) // index of curve in curve array
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Kenneth Tang
		//       DATE WRITTEN   Oct 2004
		//       MODIFIED       January 2006, Rick Strand; July 2006, Lixing Gu; Aug. 2014, Rongpeng Zhang
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Given a curve index, returns the curve type

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		std::string GetCurveType;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( CurveIndex > 0 ) {
			{ auto const SELECT_CASE_var( PerfCurve( CurveIndex ).CurveType );
			if ( SELECT_CASE_var == Linear ) {
				GetCurveType = "LINEAR";
			} else if ( SELECT_CASE_var == BiLinear ) {
				GetCurveType = "BILINEAR";
			} else if ( SELECT_CASE_var == QuadLinear ) {
				GetCurveType = "QUADLINEAR";
			} else if ( SELECT_CASE_var == Quadratic ) {
				GetCurveType = "QUADRATIC";
			} else if ( SELECT_CASE_var == Cubic ) {
				GetCurveType = "CUBIC";
			} else if ( SELECT_CASE_var == BiQuadratic ) {
				GetCurveType = "BIQUADRATIC";
			} else if ( SELECT_CASE_var == QuadraticLinear ) {
				GetCurveType = "QUADRATICLINEAR";
			} else if ( SELECT_CASE_var == CubicLinear ) {
				GetCurveType = "CUBICLINEAR";
			} else if ( SELECT_CASE_var == BiCubic ) {
				GetCurveType = "BICUBIC";
			} else if ( SELECT_CASE_var == TriQuadratic ) {
				GetCurveType = "TRIQUADRATIC";
			} else if ( SELECT_CASE_var == Exponent ) {
				GetCurveType = "EXPONENT";
			} else if ( SELECT_CASE_var == Quartic ) {
				GetCurveType = "QUARTIC";
			} else if ( SELECT_CASE_var == FanPressureRise ) {
				GetCurveType = "FANPRESSURERISE";
			} else if ( SELECT_CASE_var == ExponentialSkewNormal ) {
				GetCurveType = "EXPONENTIALSKEWNORMAL";
			} else if ( SELECT_CASE_var == Sigmoid ) {
				GetCurveType = "SIGMOID";
			} else if ( SELECT_CASE_var == RectangularHyperbola1 ) {
				GetCurveType = "RECTANGULARHYPERBOLA1";
			} else if ( SELECT_CASE_var == RectangularHyperbola2 ) {
				GetCurveType = "RECTANGULARHYPERBOLA2";
			} else if ( SELECT_CASE_var == ExponentialDecay ) {
				GetCurveType = "EXPONENTIALDECAY";
			} else if ( SELECT_CASE_var == DoubleExponentialDecay ) {
				GetCurveType = "DOUBLEEXPONENTIALDECAY";
			} else if ( SELECT_CASE_var == ChillerPartLoadWithLift ) {
				GetCurveType = "CHILLERPARTLOADWITHLIFT";
			}}
		} else {
			GetCurveType = "";
		}
		return GetCurveType;
	}

	std::string
	GetCurveName( int const CurveIndex ) // index of curve in curve array
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Bereket Nigusse
		//       DATE WRITTEN   May 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Given a curve index, returns the curve name

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		std::string GetCurveName;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( CurveIndex > 0 ) {
			GetCurveName = PerfCurve( CurveIndex ).Name;
		} else {
			GetCurveName = "";
		}
		return GetCurveName;
	}

	int
	GetCurveIndex( std::string const & CurveName ) // name of the curve
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Given a curve name, returns the curve index

		// METHODOLOGY EMPLOYED:
		// uses FindItemInList to search the curve array for the curve name

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Return value
		int GetCurveIndex;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// First time GetCurveIndex is called, get the input for all the performance curves
		if ( GetCurvesInputFlag ) {
			GetCurveInput();
			GetPressureSystemInput();
			GetCurvesInputFlag = false;
		}

		if ( NumCurves > 0 ) {
			GetCurveIndex = FindItemInList( CurveName, PerfCurve );
		} else {
			GetCurveIndex = 0;
		}

		return GetCurveIndex;

	}

	// This utility function grabs a curve index and performs the
	// error checking

	int
	GetCurveCheck(
		std::string const & alph, // curve name
		bool & errFlag,
		std::string const & ObjName // parent object of curve
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   March 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides a simple call to both return a curve index as well
		// as check for validity and produce an error message.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int GetCurveCheckOut;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		GetCurveCheckOut = GetCurveIndex( alph ); // convert curve name to pointer
		if ( GetCurveCheckOut == 0 ) {
			ShowSevereError( "Curve Not Found for Object=\"" + ObjName + "\" :: " + alph );
			errFlag = true;
		}
		return GetCurveCheckOut;

	}

	void
	GetCurveMinMaxValues(
		int const CurveIndex, // index of curve in curve array
		Real64 & Var1Min, // Minimum values of 1st independent variable
		Real64 & Var1Max, // Maximum values of 1st independent variable
		Optional< Real64 > Var2Min, // Minimum values of 2nd independent variable
		Optional< Real64 > Var2Max, // Maximum values of 2nd independent variable
		Optional< Real64 > Var3Min, // Minimum values of 2nd independent variable
		Optional< Real64 > Var3Max // Maximum values of 2nd independent variable
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   July 2006
		//       MODIFIED       B. Griffith Aug 2006 add third independent variable
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Given the curve index, returns the minimum and maximum values specified in the input
		// for the independent variables of the performance curve.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Var1Min = PerfCurve( CurveIndex ).Var1Min;
		Var1Max = PerfCurve( CurveIndex ).Var1Max;
		if ( present( Var2Min ) ) Var2Min = PerfCurve( CurveIndex ).Var2Min;
		if ( present( Var2Max ) ) Var2Max = PerfCurve( CurveIndex ).Var2Max;
		if ( present( Var3Min ) ) Var3Min = PerfCurve( CurveIndex ).Var3Min;
		if ( present( Var3Max ) ) Var3Max = PerfCurve( CurveIndex ).Var3Max;

	}

	void
	SetCurveOutputMinMaxValues(
		int const CurveIndex, // index of curve in curve array
		bool & ErrorsFound, // TRUE when errors occur
		Optional< Real64 const > CurveMin, // Minimum value of curve output
		Optional< Real64 const > CurveMax // Maximum values of curve output
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   Feb 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Given the curve index, sets the minimum and maximum possible value for this curve.
		// Certain curve types have set limits (e.g., PLF curve should not be greater than 1 or less than 0.7).

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		if ( CurveIndex > 0 && CurveIndex <= NumCurves ) {

			if ( present( CurveMin ) ) {
				PerfCurve( CurveIndex ).CurveMin = CurveMin;
				PerfCurve( CurveIndex ).CurveMinPresent = true;
			}

			if ( present( CurveMax ) ) {
				PerfCurve( CurveIndex ).CurveMax = CurveMax;
				PerfCurve( CurveIndex ).CurveMaxPresent = true;
			}

		} else {

			ShowSevereError( "SetCurveOutputMinMaxValues: CurveIndex=[" + TrimSigDigits( CurveIndex ) + "] not in range of curves=[1:" + TrimSigDigits( NumCurves ) + "]." );
			ErrorsFound = true;

		}

	}

	void
	GetPressureSystemInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Currently it just reads the input for pressure curve objects

		// METHODOLOGY EMPLOYED:
		// General EnergyPlus Methodology

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using namespace DataIPShortCuts;
		//  USE PlantPressureSystem, ONLY: PlantPressureCurveData, PressureCurve

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurveObjectName( "Curve:Functional:PressureDrop" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumPressure;
		Array1D_string Alphas( 1 ); // Alpha items for object
		Array1D< Real64 > Numbers( 5 ); // Numeric items for object
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		static bool ErrsFound( false ); // Set to true if errors in input, fatal at end of routine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int CurveNum;
		int CurveFound;

		NumPressure = GetNumObjectsFound( CurveObjectName );
		PressureCurve.allocate( NumPressure );
		for ( CurveNum = 1; CurveNum <= NumPressure; ++CurveNum ) {
			GetObjectItem( CurveObjectName, CurveNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), PressureCurve, CurveNum - 1, IsNotOK, IsBlank, CurveObjectName + " Name" );
			if ( IsNotOK ) {
				ErrsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			// Need to verify that this name isn't used in Performance Curves as well.
			if ( NumCurves > 0 ) {
				CurveFound = FindItemInList( Alphas( 1 ), PerfCurve );
				if ( CurveFound != 0 ) {
					ShowSevereError( "GetPressureCurveInput: " + CurveObjectName + "=\"" + Alphas( 1 ) + "\", duplicate curve name." );
					ShowContinueError( "...Curve name duplicates one of the Performance Curves. Names must be unique across all curves." );
					ErrsFound = true;
				}
			}
			PressureCurve( CurveNum ).Name = Alphas( 1 );
			PressureCurve( CurveNum ).EquivDiameter = Numbers( 1 );
			PressureCurve( CurveNum ).MinorLossCoeff = Numbers( 2 );
			PressureCurve( CurveNum ).EquivLength = Numbers( 3 );
			PressureCurve( CurveNum ).EquivRoughness = Numbers( 4 );
			if ( NumNumbers > 4 && ! lNumericFieldBlanks( 5 ) ) {
				if ( Numbers( 5 ) != 0.0 ) {
					PressureCurve( CurveNum ).ConstantFPresent = true;
					PressureCurve( CurveNum ).ConstantF = Numbers( 5 );
				}
			}
		}

		NumPressureCurves = NumPressure;

		if ( ErrsFound ) {
			ShowFatalError( "GetPressureCurveInput: Errors found in Curve Objects.  Preceding condition(s) cause termination." );
		}

	}

	void
	GetPressureCurveTypeAndIndex(
		std::string const & PressureCurveName, // name of the curve
		int & PressureCurveType,
		int & PressureCurveIndex
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Given a curve name, returns the curve type and index

		// METHODOLOGY EMPLOYED:
		// Curve types are:
		//  PressureCurve_Error       = pressure name was given, but curve is not available
		//  PressureCurve_None        = no pressure curve for this branch
		//  PressureCurve_Pressure    = pressure curve based on friction/minor loss
		//  PressureCurve_Generic     = curvemanager held curve which is function of flow rate

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		//  USE CurveManager,   ONLY : GetCurveIndex, GetCurveType
		using DataBranchAirLoopPlant::PressureCurve_None;
		using DataBranchAirLoopPlant::PressureCurve_Pressure;
		using DataBranchAirLoopPlant::PressureCurve_Generic;
		using DataBranchAirLoopPlant::PressureCurve_Error;
		//  USE PlantPressureSystem, ONLY: PlantPressureCurveData, PressureCurve

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int TempCurveIndex;
		bool FoundCurve;
		std::string GenericCurveType;

		//If input is not gotten, go ahead and get it now
		if ( GetCurvesInputFlag ) {
			GetCurveInput();
			GetPressureSystemInput();
			GetCurvesInputFlag = false;
		}

		//Initialize
		FoundCurve = false;
		PressureCurveType = PressureCurve_None;
		PressureCurveIndex = 0;

		//Try to retrieve a curve manager object
		TempCurveIndex = GetCurveIndex( PressureCurveName );

		//See if it is valid
		if ( TempCurveIndex > 0 ) {
			//We have to check the type of curve to make sure it is single independent variable type
			GenericCurveType = GetCurveType( TempCurveIndex );
			{ auto const SELECT_CASE_var( GenericCurveType );
			if ( ( SELECT_CASE_var == "LINEAR" ) || ( SELECT_CASE_var == "QUADRATIC" ) || ( SELECT_CASE_var == "CUBIC" ) || ( SELECT_CASE_var == "QUARTIC" ) || ( SELECT_CASE_var == "EXPONENT" ) ) {
				PressureCurveType = PressureCurve_Generic;
				PressureCurveIndex = TempCurveIndex;
			} else {
				ShowSevereError( "Plant Pressure Simulation: Found error for curve: " + PressureCurveName );
				ShowContinueError( "Curve type detected: " + GenericCurveType );
				ShowContinueError( "Generic curves should be single independent variable such that DeltaP = f(mdot)" );
				ShowContinueError( " Therefore they should be of type: Linear, Quadratic, Cubic, Quartic, or Exponent" );
				ShowFatalError( "Errors in pressure simulation input cause program termination" );
			}}
			return;
		}

		//Then try to retrieve a pressure curve object
		if ( allocated( PressureCurve ) ) {
			if ( size( PressureCurve ) > 0 ) {
				TempCurveIndex = FindItemInList( PressureCurveName, PressureCurve );
			} else {
				TempCurveIndex = 0;
			}
		}

		//See if it is valid
		if ( TempCurveIndex > 0 ) {
			PressureCurveType = PressureCurve_Pressure;
			PressureCurveIndex = TempCurveIndex;
			return;
		}

		//If we made it here, we didn't find either type of match

		//Last check, see if it is blank:
		if ( PressureCurveName == "" ) {
			PressureCurveType = PressureCurve_None;
			return;
		}

		//At this point, we had a non-blank user entry with no match
		PressureCurveType = PressureCurve_Error;
		return;

	}

	Real64
	PressureCurveValue(
		int const PressureCurveIndex,
		Real64 const MassFlow,
		Real64 const Density,
		Real64 const Viscosity
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This will evaluate the pressure drop for components which use pressure information

		// METHODOLOGY EMPLOYED:
		// Friction factor pressure drop equation:
		// DP = [f*(L/D) + K] * (rho * V^2) / 2

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::Pi;

		// Return value
		Real64 PressureCurveValue;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 Diameter;
		Real64 MinorLossCoeff;
		Real64 Length;
		Real64 Roughness;
		bool IsConstFPresent;
		Real64 ConstantF;
		Real64 FrictionFactor;
		Real64 CrossSectArea;
		Real64 Velocity;
		Real64 ReynoldsNumber;
		Real64 RoughnessRatio;

		//Retrieve data from structure
		Diameter = PressureCurve( PressureCurveIndex ).EquivDiameter;
		MinorLossCoeff = PressureCurve( PressureCurveIndex ).MinorLossCoeff;
		Length = PressureCurve( PressureCurveIndex ).EquivLength;
		Roughness = PressureCurve( PressureCurveIndex ).EquivRoughness;
		IsConstFPresent = PressureCurve( PressureCurveIndex ).ConstantFPresent;
		ConstantF = PressureCurve( PressureCurveIndex ).ConstantF;

		//Intermediate calculations
		CrossSectArea = ( Pi / 4.0 ) * pow_2( Diameter );
		Velocity = MassFlow / ( Density * CrossSectArea );
		ReynoldsNumber = Density * Diameter * Velocity / Viscosity; //assuming mu here
		RoughnessRatio = Roughness / Diameter;

		//If we don't have any flow then exit out
		if ( MassFlow < MassFlowTolerance ) {
			PressureCurveValue = 0.0;
			PressureCurve( PressureCurveIndex ).CurveInput1 = MassFlow;
			PressureCurve( PressureCurveIndex ).CurveInput2 = Density;
			PressureCurve( PressureCurveIndex ).CurveInput3 = Velocity;
			PressureCurve( PressureCurveIndex ).CurveOutput = 0.0;
			return PressureCurveValue;
		}

		//Calculate the friction factor
		if ( IsConstFPresent ) { //use the constant value
			FrictionFactor = ConstantF;
		} else { // must calculate f
			FrictionFactor = CalculateMoodyFrictionFactor( ReynoldsNumber, RoughnessRatio );
		}

		//Pressure drop calculation
		PressureCurveValue = ( FrictionFactor * ( Length / Diameter ) + MinorLossCoeff ) * ( Density * pow_2( Velocity ) ) / 2.0;

		if ( PressureCurve( PressureCurveIndex ).EMSOverrideOn ) PressureCurveValue = PressureCurve( PressureCurveIndex ).EMSOverrideCurveValue;

		PressureCurve( PressureCurveIndex ).CurveInput1 = MassFlow;
		PressureCurve( PressureCurveIndex ).CurveInput2 = Density;
		PressureCurve( PressureCurveIndex ).CurveInput3 = Velocity;
		PressureCurve( PressureCurveIndex ).CurveOutput = PressureCurveValue;

		return PressureCurveValue;
	}

	Real64
	CalculateMoodyFrictionFactor(
		Real64 const ReynoldsNumber,
		Real64 const RoughnessRatio
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   August 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This will evaluate the moody friction factor based on Reynolds number and roughness ratio

		// METHODOLOGY EMPLOYED:
		// General empirical correlations for friction factor based on Moody Chart data

		// REFERENCES:
		// Haaland, SE (1983). "Simple and Explicit Formulas for the Friction Factor in Turbulent Flow".
		//   Trans. ASIVIE, J. of Fluids Engineering 103: 89-90.

		// Using/Aliasing
		using General::RoundSigDigits;

		// Return value
		Real64 CalculateMoodyFrictionFactor;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 Term1;
		Real64 Term2;
		Real64 Term3;
		std::string RR;
		std::string Re;
		static bool FrictionFactorErrorHasOccurred( false );

		//Check for no flow before calculating values
		if ( ReynoldsNumber == 0.0 ) {
			CalculateMoodyFrictionFactor = 0.0;
			return CalculateMoodyFrictionFactor;
		}

		//Check for no roughness also here
		if ( RoughnessRatio == 0.0 ) {
			CalculateMoodyFrictionFactor = 0.0;
			return CalculateMoodyFrictionFactor;
		}

		//Calculate the friction factor
		Term1 = std::pow( RoughnessRatio / 3.7, 1.11 );
		Term2 = 6.9 / ReynoldsNumber;
		Term3 = -1.8 * std::log10( Term1 + Term2 );
		if ( Term3 != 0.0 ) {
			CalculateMoodyFrictionFactor = std::pow( Term3, -2.0 );
		} else {
			if ( ! FrictionFactorErrorHasOccurred ) {
				RR = RoundSigDigits( RoughnessRatio, 7 );
				Re = RoundSigDigits( ReynoldsNumber, 1 );
				ShowSevereError( "Plant Pressure System: Error in moody friction factor calculation" );
				ShowContinueError( "Current Conditions: Roughness Ratio=" + RR + "; Reynolds Number=" + Re );
				ShowContinueError( "These conditions resulted in an unhandled numeric issue." );
				ShowContinueError( "Please contact EnergyPlus support/development team to raise an alert about this issue" );
				ShowContinueError( "This issue will occur only one time.  The friction factor has been reset to 0.04 for calculations" );
				FrictionFactorErrorHasOccurred = true;
			}
			CalculateMoodyFrictionFactor = 0.04;
		}

		return CalculateMoodyFrictionFactor;

	}

	int
	GetCurveObjectTypeNum( int const CurveIndex ) // index of curve in curve array
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   April 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// get the object type integer identifier for curves and tables

		// METHODOLOGY EMPLOYED:
		// retrieve from data structure.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int CurveOrTableObjectTypeNum;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		if ( CurveIndex > 0 ) {
			CurveOrTableObjectTypeNum = PerfCurve( CurveIndex ).ObjectType;
		} else {
			CurveOrTableObjectTypeNum = 0;
		}

		return CurveOrTableObjectTypeNum;
	}

	//=================================================================================================!

} // CurveManager

} // EnergyPlus
