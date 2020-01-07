// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MatrixDataManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace MatrixDataManager {

    // Module containing the routines dealing with Matrix input objects and services

    // MODULE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   June 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Process user input for Matrix: input data objects
    // Provide central services for other routines to access
    // matrix input data.

    // METHODOLOGY EMPLOYED:
    // Basic calls to InputProcessor, series of simple get and set routines

    // REFERENCES:
    // none

    // OTHER NOTES:
    // first implemented for complex fenestration

    // Using/Aliasing
    using namespace DataPrecisionGlobals;

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // INTEGER, PARAMETER :: OneDimensional = 1
    int const TwoDimensional(2);
    // INTEGER, PARAMETER :: ThreeDimensional = 3
    static std::string const BlankString;
    // DERIVED TYPE DEFINITIONS:
    // na

    // MODULE VARIABLE DECLARATIONS:

    int NumMats; // number of matracies in input file

    // SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

    // todo, flush out the following routines, see CurveManager for patterns
    // PUBLIC GetMatrixValue
    // PUBLIC GetMatrixCheck
    // PUBLIC GetMatrixType
    // PUBLIC GetMatrixMinMaxValues
    // PUBLIC SetMatrixOutputMinMaxValues
    // PUBLIC GetMatrixName

    // Object Data
    Array1D<MatrixDataStruct> MatData;

    // Functions

    void GetMatrixInput()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // get input for Matrix objects

        // Using/Aliasing
        using namespace DataIPShortCuts; // Data for field names, blank numerics
        using General::RoundSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumTwoDimMatrix;            // count of Matrix:TwoDimension objects
        int MatIndex;                   // do loop counter
        int MatNum;                     // index management
        int NumAlphas;                  // Number of Alphas for each GetObjectItem call
        int NumNumbers;                 // Number of Numbers for each GetObjectItem call
        int IOStatus;                   // Used in GetObjectItem
        static bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        int NumRows;
        int NumCols;
        int NumElements;

        cCurrentModuleObject = "Matrix:TwoDimension";
        NumTwoDimMatrix = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        NumMats = NumTwoDimMatrix;

        MatData.allocate(NumMats);

        MatNum = 0;
        for (MatIndex = 1; MatIndex <= NumTwoDimMatrix; ++MatIndex) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          MatIndex,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          _,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            ++MatNum;
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            MatData(MatNum).Name = cAlphaArgs(1);
            NumRows = std::floor(rNumericArgs(1));
            NumCols = std::floor(rNumericArgs(2));
            NumElements = NumRows * NumCols;

            // test
            if (NumElements < 1) {
                ShowSevereError("GetMatrixInput: for " + cCurrentModuleObject + ": " + cAlphaArgs(1));
                ShowContinueError("Check " + cNumericFieldNames(1) + " and " + cNumericFieldNames(2) +
                                  " total number of elements in matrix must be 1 or more");
                ErrorsFound = true;
            }
            if ((NumNumbers - 2) < NumElements) {
                ShowSevereError("GetMatrixInput: for " + cCurrentModuleObject + ": " + cAlphaArgs(1));
                ShowContinueError("Check input, total number of elements does not agree with " + cNumericFieldNames(1) + " and " +
                                  cNumericFieldNames(2));
                ErrorsFound = true;
            }
            MatData(MatNum).MatrixType = TwoDimensional;
            // Note With change to row-major arrays the "row" and "col" usage here is transposed
            auto &matrix(MatData(MatNum).Mat2D);
            matrix.allocate(NumCols, NumRows); // This is standard order for a NumRows X NumCols matrix
            Array2<Real64>::size_type l(0);
            for (int ElementNum = 1; ElementNum <= NumElements; ++ElementNum, l += matrix.size()) {
                int const RowIndex = (ElementNum - 1) / NumCols + 1;
                int const ColIndex = mod((ElementNum - 1), NumCols) + 1;
                matrix(ColIndex, RowIndex) = rNumericArgs(ElementNum + 2); // Matrix is read in row-by-row
            }
        }

        if (ErrorsFound) {
            ShowFatalError("GetMatrixInput: Errors found in Matrix objects. Preceding condition(s) cause termination.");
        }
    }

    int MatrixIndex(std::string const &MatrixName)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Return integer index or pointer to MatData structure array

        // METHODOLOGY EMPLOYED:
        // inputs name of matrix and returns integer index
        // currently uses UtilityRoutines::FindItemInList( which is case sensitive

        // Return value
        int MatrixIndexPtr; // Function result

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        static bool GetInputFlag(true); // First time, input is "gotten"

        if (GetInputFlag) {
            GetMatrixInput();
            GetInputFlag = false;
        }

        if (NumMats > 0) {
            MatrixIndexPtr = UtilityRoutines::FindItemInList(MatrixName, MatData);
        } else {
            MatrixIndexPtr = 0;
        }

        return MatrixIndexPtr;
    }

    void Get2DMatrix(int const Idx, // pointer index to location in MatData
                     Array2S<Real64> Mat2D)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // pass matrix to calling routine

        if (Idx > 0) { // protect hard crash
            Mat2D = MatData(Idx).Mat2D;
        } else {
            // do nothing (?) throw dev error
        }
    }

    void Get2DMatrixDimensions(int const Idx, // pointer index to location in MatData
                               int &NumRows,
                               int &NumCols)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   June 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        if (Idx > 0) {
            NumRows = MatData(Idx).Mat2D.isize(2);
            NumCols = MatData(Idx).Mat2D.isize(1);
        } else {
            // do nothing (?) throw dev error?
        }
    }

} // namespace MatrixDataManager

} // namespace EnergyPlus
