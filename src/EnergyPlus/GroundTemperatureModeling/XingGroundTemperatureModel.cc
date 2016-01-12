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
#include <memory>

// EnergyPlus headers
#include <DataGlobals.hh>
#include <DataIPShortCuts.hh>
#include <GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <GroundTemperatureModeling/XingGroundTemperatureModel.hh>
#include <InputProcessor.hh>
#include <WeatherManager.hh>

namespace EnergyPlus {

	//******************************************************************************

	// Xing model factory
	std::shared_ptr< XingGroundTempsModel > 
	XingGroundTempsModel::XingGTMFactory( 
		int objectType, 
		std::string objectName
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reads input and creates instance of Xing ground temps model

		// USE STATEMENTS:
		using namespace DataIPShortCuts;
		using namespace GroundTemperatureManager;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool found = false;
		int NumNums;
		int NumAlphas;
		int IOStat;
		bool ErrorsFound = false;

		// New shared pointer for this model object
		std::shared_ptr< XingGroundTempsModel > thisModel( new XingGroundTempsModel() );

		std::string const cCurrentModuleObject = CurrentModuleObjects( objectType_XingGroundTemp );
		int numCurrModels = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );

		for ( int modelNum = 1; modelNum <= numCurrModels; ++modelNum ) {

			InputProcessor::GetObjectItem( cCurrentModuleObject, modelNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

			if ( objectName == cAlphaArgs( 1 ) ) {
				// Read input into object here

				thisModel->objectName = cAlphaArgs( 1 );
				thisModel->objectType = objectType;
				thisModel->groundThermalDiffisivity = rNumericArgs( 1 ) / ( rNumericArgs( 2 ) * rNumericArgs( 3 ) );
				thisModel->aveGroundTemp = rNumericArgs( 4 );
				thisModel->surfTempAmplitude_1 = rNumericArgs( 5 );
				thisModel->surfTempAmplitude_2 = rNumericArgs( 6 );
				thisModel->phaseShift_1 = rNumericArgs( 7 );
				thisModel->phaseShift_2 = rNumericArgs( 8 );
				
				found = true;
				break;
			}
		}

		if ( found && !ErrorsFound ) {
			groundTempModels.push_back( thisModel );
			return thisModel;
		} else {
			ShowFatalError( "Site:GroundTemperature:Undisturbed:Xing--Errors getting input for ground temperature model");
			return nullptr;
		}
	}

	//******************************************************************************

	Real64 XingGroundTempsModel::getGroundTemp()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Returns the ground temperature for the Site:GroundTemperature:Undisturbed:Xing

		// USE STATEMENTS:
		using DataGlobals::Pi;
		using WeatherManager::NumDaysInYear;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int n;
		Real64 static tp( NumDaysInYear ); // Period of soil temperature cycle
		Real64 Ts_1; // Amplitude of surface temperature
		Real64 Ts_2; // Amplitude of surface temperature
		Real64 PL_1; // Phase shift of surface temperature
		Real64 PL_2; // Phase shift of surface temperature

		Real64 term1;
		Real64 term2;
		Real64 term3;
		Real64 term4;

		Real64 retVal;
		Real64 summation;

		// Inits
		Ts_1 = surfTempAmplitude_1;
		PL_1 = phaseShift_1;
		Ts_2 = surfTempAmplitude_2;
		PL_2 = phaseShift_2;

		n = 1;
		term1 = -depth * std::sqrt( ( n * Pi ) / ( groundThermalDiffisivity * tp ) );
		term2 = ( 2 * Pi * n ) / tp * ( simTimeInDays - PL_1 ) - depth * std::sqrt( ( n * Pi ) / ( groundThermalDiffisivity * tp ) );

		n = 2;
		term3 = -depth * std::sqrt( ( n * Pi ) / ( groundThermalDiffisivity * tp ) );
		term4 = ( 2 * Pi * n ) / tp * ( simTimeInDays - PL_2 ) - depth * std::sqrt( ( n * Pi ) / ( groundThermalDiffisivity * tp ) );

		summation = std::exp( term1 ) * Ts_1 * std::cos( term2 ) + std::exp( term3 ) * Ts_2 * std::cos( term4 );

		retVal = aveGroundTemp - summation;

		return retVal;
	}

	//******************************************************************************

	Real64 XingGroundTempsModel::getGroundTempAtTimeInMonths(
		Real64 _depth,
		int _month
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Returns ground temperature when input time is in months

		// USE STATEMENTS:
		using WeatherManager::NumDaysInYear;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 const aveDaysInMonth = NumDaysInYear / 12;

		depth = _depth;
	
		// Set month
		if ( _month >= 1 && _month <= 12 ) {
			simTimeInDays = aveDaysInMonth * ( ( _month - 1 ) + 0.5 );
		} else {
			int monthIndex = remainder( _month, 12 );
			simTimeInDays = aveDaysInMonth * ( ( monthIndex - 1 ) + 0.5 );
		}

		// Get and return ground temp
		return getGroundTemp();
	}

	//******************************************************************************

	Real64 XingGroundTempsModel::getGroundTempAtTimeInSeconds(
		Real64 _depth,
		Real64 seconds
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Returns ground temperature when time is in seconds

		// USE STATEMENTS:
		using DataGlobals::SecsInDay;
		using WeatherManager::NumDaysInYear;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		depth = _depth;

		simTimeInDays = seconds / SecsInDay;

		if ( simTimeInDays >  NumDaysInYear ) {
			simTimeInDays = remainder( simTimeInDays, NumDaysInYear );
		}

		return getGroundTemp();
	}

	//******************************************************************************

}	// EnergyPlus namespace
