// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois,
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
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// JSON Headers
#include <jsoncpp/json/json.h>

// EnergyPlus Headers
#include <GroundHeatExchangers.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DisplayRoutines.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace GroundHeatExchangers {
	// MODULE INFORMATION:
	//       AUTHOR         Arun Murugappan, Dan Fisher
	//       DATE WRITTEN   September 2000
	//       MODIFIED       B. Griffith, Sept 2010,plant upgrades
	//                      Matt Mitchell, February 2015. Added Slinky GHX.
	//                                                    Moved models to object-oriented design.
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The module contains the data structures and routines to simulate the
	// operation of vertical closed-loop ground heat exchangers (GLHE) typically
	// used in low temperature geothermal heat pump systems.

	// METHODOLOGY EMPLOYED:
	// The borehole and fluid temperatures are calculated from the response to
	// the current heat transfer rate and the response to the history of past
	// applied heat pulses. The response to each pulse is calculated from a non-
	// dimensionalized response function, or G-function, that is specific to the
	// given borehole field arrangement, depth and spacing. The data defining
	// this function is read from input.
	// The heat pulse histories need to be recorded over an extended period (months).
	// To aid computational efficiency past pulses are continuously aggregated into
	// equivalent heat pulses of longer duration, as each pulse becomes less recent.

	// REFERENCES:
	// Eskilson, P. 'Thermal Analysis of Heat Extraction Boreholes' Ph.D. Thesis:
	//   Dept. of Mathematical Physics, University of Lund, Sweden, June 1987.
	// Yavuzturk, C., J.D. Spitler. 1999. 'A Short Time Step Response Factor Model
	//   for Vertical Ground Loop Heat Exchangers. ASHRAE Transactions. 105(2): 475-485.
	// Xiong, Z., D.E. Fisher, J.D. Spitler. 2015. 'Development and Validation of a Slinky
	//   Ground Heat Exchanger.' Applied Energy. Vol 114, 57-69.

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginSimFlag;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginTimeStepFlag;
	using DataGlobals::BeginHourFlag;
	using DataGlobals::HourOfDay;
	using DataGlobals::TimeStep;
	using DataGlobals::TimeStepZone;
	using DataGlobals::DayOfSim;
	using DataGlobals::Pi;
	using DataGlobals::WarmupFlag;
	using DataGlobals::SecInHour;
	using DataHVACGlobals::TimeStepSys;
	using DataHVACGlobals::SysTimeElapsed;
	using namespace DataLoopNode;
	using General::TrimSigDigits;
	using namespace GroundTemperatureManager;

	// MODULE PARAMETER DEFINITIONS
	Real64 const hrsPerDay( 24.0 ); // Number of hours in a day
	Real64 const hrsPerMonth( 730.0 ); // Number of hours in month
	int const maxTSinHr( 60 ); // Max number of time step in a hour

	// MODULE VARIABLE DECLARATIONS:
	int numVerticalGLHEs( 0 );
	int numSlinkyGLHEs( 0 );
	int numVertArray( 0 );
	int numVertProps( 0 );
	int numResponseFactors( 0 );
	int numSingleBorehole( 0 );

	int N( 1 ); // COUNTER OF TIME STEP
	Real64 currentSimTime( 0.0 ); // Current simulation time in hours
	int locHourOfDay( 0 );
	int locDayOfSim( 0 );
	bool GetInput( true );
	bool errorsFound( false );

	static int numAutoGeneratedResponseFactors( 0 );

	Array1D< Real64 > prevTimeSteps; // This is used to store only the Last Few time step's time
	// to enable the calculation of the sub-hourly contribution..
	// Recommended size, the product of Minimum sub-hourly history required and
	// the maximum no of system time steps in an hour

	Array1D_bool checkEquipName;

	// Object Data
	std::vector < GLHEVert > verticalGLHE;
	std::vector < GLHESlinky > slinkyGLHE;
	std::vector < std::shared_ptr< GLHEVertArrayStruct > > vertArraysVector;
	std::vector < std::shared_ptr < GLHEVertPropsStruct > > vertPropsVector;
	std::vector < std::shared_ptr < GLHEResponseFactorsStruct > > responseFactorsVector;
	std::vector < std::shared_ptr < GLHEVertSingleStruct > > singleBoreholesVector;

	//******************************************************************************

	void
	clear_state(){
		numVerticalGLHEs = 0;
		numSlinkyGLHEs = 0;
		numVertArray = 0;
		numVertProps = 0;
		numResponseFactors = 0;
		numSingleBorehole = 0;
		N = 1;
		currentSimTime = 0.0 ;
		locHourOfDay = 0;
		locDayOfSim = 0;
		GetInput = true;
		errorsFound = false;
		prevTimeSteps.deallocate();
		checkEquipName.deallocate();
		verticalGLHE.clear();
		slinkyGLHE.clear();
		vertArraysVector.clear();
		vertPropsVector.clear();
		responseFactorsVector.clear();
		singleBoreholesVector.clear();
	}

	//******************************************************************************

	std::shared_ptr < GLHEVertPropsStruct >
	GetVertProps(
		std::string const & objectName
	)
	{

		int numVertProps = vertPropsVector.size();

		// Check if this instance of this model has already been retrieved
		for ( int i = 0; i < numVertProps; ++i ) {
			auto thisProp( vertPropsVector[i] );
			// Check if the type and name match
			if ( objectName == thisProp->name ) {
				return vertPropsVector[i];
			}
		}

		return nullptr;
	}

	//******************************************************************************

	std::shared_ptr < GLHEVertSingleStruct >
	GetSingleBH(
		std::string const & objectName
	)
	{

		int numBH = singleBoreholesVector.size();

		// Check if this instance of this model has already been retrieved
		for ( int i = 0; i < numBH; ++i ) {
			auto thisBH( singleBoreholesVector[i] );
			// Check if the type and name match
			if ( objectName == thisBH->name ) {
				return singleBoreholesVector[i];
			}
		}

		return nullptr;
	}

	//******************************************************************************

	std::shared_ptr < GLHEVertArrayStruct >
	GetVertArray(
		std::string const & objectName
	)
	{

		int numVertArrays = vertArraysVector.size();

		// Check if this instance of this model has already been retrieved
		for ( int i = 0; i < numVertArrays; ++i ) {
			auto thisProp( vertArraysVector[i] );
			// Check if the type and name match
			if( objectName == thisProp->name ) {
				return vertArraysVector[i];
			}
		}

		return nullptr;
	}

	//******************************************************************************

	std::shared_ptr < GLHEResponseFactorsStruct >
	GetResponseFactor(
		std::string const & objectName
	)
	{

		int numRF = responseFactorsVector.size();

		// Check if this instance of this model has already been retrieved
		for ( int i = 0; i < numRF; ++i ) {
			auto thisRF( responseFactorsVector[i] );
			// Check if the type and name match
			if ( objectName == thisRF->name ) {
				return responseFactorsVector[i];
			}
		}

		return nullptr;
	}

	//******************************************************************************

	std::shared_ptr < GLHEResponseFactorsStruct >
	BuildAndGetResponseFactorObjectFromArray(
		std::shared_ptr < GLHEVertArrayStruct > const & arrayObjectPtr
	)
	{
		// Make new response factor object and store it for later use
		std::shared_ptr < GLHEResponseFactorsStruct > thisRF( new GLHEResponseFactorsStruct );
		thisRF->name = arrayObjectPtr->name;
		thisRF->props = arrayObjectPtr->props;

		// Build out new instances of the vertical BH objects which correspond to this object
		int xLoc = 0;
		int bhCounter = 0;
		for ( int xBH = 1; xBH <= arrayObjectPtr->numBHinXDirection; ++xBH ) {
			int yLoc = 0;
			for ( int yBH = 1; yBH <= arrayObjectPtr->numBHinYDirection; ++yBH ) {
				bhCounter += 1;
				std::shared_ptr< GLHEVertSingleStruct > thisBH( new GLHEVertSingleStruct );
				thisBH->name = thisRF->name + " BH " + std::to_string( bhCounter ) + " loc: (" + std::to_string( xLoc ) + ", " + std::to_string( yLoc ) + ")";
				thisBH->props = GetVertProps( arrayObjectPtr->props->name );
				thisBH->xLoc = xLoc;
				thisBH->yLoc = yLoc;
				thisRF->myBorholes.push_back( thisBH );
				singleBoreholesVector.push_back( thisBH );
				yLoc += arrayObjectPtr->bhSpacing;
				thisRF->numBoreholes += 1;
			}
			xLoc += arrayObjectPtr->bhSpacing;
		}

		SetupBHPointsForResponseFactorsObject( thisRF );
		responseFactorsVector.push_back( thisRF );
		return thisRF;
	}

	//******************************************************************************

	std::shared_ptr < GLHEResponseFactorsStruct >
	BuildAndGetResponseFactorsObjectFromSingleBHs(
		std::vector < std::shared_ptr < GLHEVertSingleStruct > > const & singleBHsForRFVect
	)
	{
		// Make new response factor object and store it for later use
		std::shared_ptr < GLHEResponseFactorsStruct > thisRF( new GLHEResponseFactorsStruct );
		thisRF->name = "Response Factor Object Auto Generated No: " + std::to_string( numAutoGeneratedResponseFactors + 1 );

		// Make new props object which has the mean values of the other props objects referenced by the individual BH objects
		std::shared_ptr < GLHEVertPropsStruct > thisProps( new GLHEVertPropsStruct );
		thisProps->name = "Response Factor Auto Generated Mean Props No: " + std::to_string( numAutoGeneratedResponseFactors + 1 );
		int numBH = singleBHsForRFVect.size();
		for ( auto & thisBH : singleBoreholesVector ) {
			thisProps->bhDiameter += thisBH->props->bhDiameter / numBH;
			thisProps->bhLength += thisBH->props->bhLength / numBH;
			thisProps->bhTopDepth += thisBH->props->bhTopDepth / numBH;
			thisProps->bhUTubeDist += thisBH->props->bhUTubeDist / numBH;

			thisProps->grout.cp += thisBH->props->grout.cp / numBH;
			thisProps->grout.diffusivity += thisBH->props->grout.diffusivity / numBH;
			thisProps->grout.k += thisBH->props->grout.diffusivity / numBH;
			thisProps->grout.rho += thisBH->props->grout.k / numBH;
			thisProps->grout.rhoCp += thisBH->props->grout.rhoCp / numBH;

			thisProps->pipe.cp += thisBH->props->pipe.cp / numBH;
			thisProps->pipe.diffusivity += thisBH->props->pipe.diffusivity / numBH;
			thisProps->pipe.k += thisBH->props->pipe.diffusivity / numBH;
			thisProps->pipe.rho += thisBH->props->pipe.k / numBH;
			thisProps->pipe.rhoCp += thisBH->props->pipe.rhoCp / numBH;

			thisProps->pipe.outDia += thisBH->props->pipe.outDia / numBH;
			thisProps->pipe.thickness += thisBH->props->pipe.thickness / numBH;

			thisRF->myBorholes.push_back( thisBH );
		}

		thisRF->props = thisProps;
		thisRF->numBoreholes = thisRF->myBorholes.size();
		vertPropsVector.push_back( thisProps );

		SetupBHPointsForResponseFactorsObject( thisRF );

		responseFactorsVector.push_back( thisRF );

		numAutoGeneratedResponseFactors += 1;

		return thisRF;
	}

	//******************************************************************************

	void
	SetupBHPointsForResponseFactorsObject(
		std::shared_ptr < GLHEResponseFactorsStruct > & thisRF
	)
	{
		for ( auto & thisBH : thisRF->myBorholes ) {

			// Using Simpson's rule the number of points (n+1) must be odd, therefore an even number of panels is required
			// Starting from i = 0 to i <= NumPanels produces an odd number of points
			int numPanels_i = 50;
			int numPanels_ii = 50;
			int numPanels_j = 560;

			thisBH->dl_i = thisBH->props->bhLength / numPanels_i;
			for ( int i = 0; i <= numPanels_i; ++i ) {
				MyCartesian newPoint;
				newPoint.x = thisBH->xLoc;
				newPoint.y = thisBH->yLoc;
				newPoint.z = thisBH->props->bhTopDepth + ( i * thisBH->dl_i );
				thisBH->pointLocations_i.push_back( newPoint );
			}

			thisBH->dl_ii = thisBH->props->bhLength / numPanels_ii;
			for ( int i = 0; i <= numPanels_ii; ++i ) {
				MyCartesian newPoint;
				// For case when bh is being compared to itself, shift points by 1 radius in the horizontal plane
				newPoint.x = thisBH->xLoc + ( thisBH->props->bhDiameter / 2.0 ) / sqrt( 2.0 );
				newPoint.y = thisBH->yLoc + ( thisBH->props->bhDiameter / 2.0 ) / ( -sqrt( 2.0 ) );
				newPoint.z = thisBH->props->bhTopDepth + ( i * thisBH->dl_ii );
				thisBH->pointLocations_ii.push_back( newPoint );
			}

			thisBH->dl_j = thisBH->props->bhLength / numPanels_j;
			for ( int i = 0; i <= numPanels_j; ++i ) {
				MyCartesian newPoint;
				newPoint.x = thisBH->xLoc;
				newPoint.y = thisBH->yLoc;
				newPoint.z = thisBH->props->bhTopDepth + ( i * thisBH->dl_j );
				thisBH->pointLocations_j.push_back( newPoint );
			}
		}
	}


	//******************************************************************************

	void GLHEBase::onInitLoopEquip( const PlantLocation & EP_UNUSED( calledFromLocation ) ) {
		this->initGLHESimVars();
	}

	//******************************************************************************

	void GLHEBase::simulate( const PlantLocation & EP_UNUSED( calledFromLocation ), bool const EP_UNUSED( FirstHVACIteration ), Real64 & EP_UNUSED( CurLoad ), bool const EP_UNUSED( RunFlag ) ) {

		using DataGlobals::KickOffSimulation;

		if ( KickOffSimulation ) {
			this->initGLHESimVars();
		} else {
			this->initGLHESimVars();
			this->calcGroundHeatExchanger();
			this->updateGHX();
		}
	}

	//******************************************************************************

	PlantComponent * GLHEBase::factory( int const objectType, std::string objectName ) {
		if ( GetInput ) {
			GetGroundHeatExchangerInput();
			GetInput = false;
		}
		if ( objectType == DataPlant::TypeOf_GrndHtExchgSystem ) {
			for ( auto & ghx : verticalGLHE ) {
				if ( ghx.name == objectName ) {
					return &ghx;
				}
			}
		} else if ( objectType == DataPlant::TypeOf_GrndHtExchgSlinky ) {
			for ( auto & ghx : slinkyGLHE ) {
				if ( ghx.name == objectName ) {
					return &ghx;
				}
			}
		}
		// If we didn't find it, fatal
		ShowFatalError( "Ground Heat Exchanger Factory: Error getting inputs for GHX named: " + objectName );
		// Shut up the compiler
		return nullptr;
	}

	//******************************************************************************

	std::vector< Real64 >
	GLHEVert::distances(
		MyCartesian const & point_i,
		MyCartesian const & point_j
	)
	{
		std::vector< Real64 > sumVals;

		// Calculate the distance between points
		sumVals.push_back( pow_2( point_i.x - point_j.x ) );
		sumVals.push_back( pow_2( point_i.y - point_j.y ) );
		sumVals.push_back( pow_2( point_i.z - point_j.z ) );

		Real64 sumTot = 0;
		std::vector< Real64 > retVals;
		std::for_each( sumVals.begin(), sumVals.end(), [&] ( Real64 n ) { sumTot += n; } );
		retVals.push_back( std::sqrt( sumTot ) );

		// Calculate distance to mirror point
		sumVals.pop_back();
		sumVals.push_back( pow_2( point_i.z - ( -point_j.z ) ) );

		sumTot = 0;
		std::for_each( sumVals.begin(), sumVals.end(), [&] ( Real64 n ) { sumTot += n; } );
		retVals.push_back( std::sqrt( sumTot ) );

		return retVals;
	}

	//******************************************************************************

	Real64
	GLHEVert::calcResponse(
		std::vector< Real64 > const & dists,
		Real64 const & currTime
	)
	{
		Real64 pointToPointResponse = erfc( dists[0] / ( 2 * sqrt( soil.diffusivity * currTime ) ) ) / dists[0];
		Real64 pointToReflectedResponse = erfc( dists[1] / ( 2 * sqrt( soil.diffusivity * currTime ) ) ) / dists[1];

		return pointToPointResponse - pointToReflectedResponse;

	}

	//******************************************************************************

	Real64
	GLHEVert::integral(
		MyCartesian const & point_i,
		std::shared_ptr< GLHEVertSingleStruct > const & bh_i,
		std::shared_ptr< GLHEVertSingleStruct > const & bh_j,
		Real64 const & currTime
	)
	{

		Real64 sum_f = 0;
		int index = 0;
		int const lastIndex_j = bh_j->pointLocations_j.size() - 1;
		for ( auto & point_j : bh_j->pointLocations_j ) {
			std::vector< Real64 > dists = distances( point_i, point_j );
			Real64 f = calcResponse( dists, currTime );

			// Integrate using Simpson's
			if ( index == 0 || index == lastIndex_j ) {
				sum_f += f;
			} else if ( isEven( index ) ) {
				sum_f += 2 * f;
			} else {
				sum_f += 4 * f;
			}

			++index;
		}

		return ( bh_j->dl_j / 3.0 ) * sum_f;
	}

	//******************************************************************************

	Real64
	GLHEVert::doubleIntegral(
		std::shared_ptr< GLHEVertSingleStruct > const & bh_i,
		std::shared_ptr< GLHEVertSingleStruct > const & bh_j,
		Real64 const & currTime
	)
	{

		if ( bh_i == bh_j ) {

			Real64 sum_f = 0;
			int index = 0;
			int const lastIndex = bh_i->pointLocations_ii.size() - 1;
			for( auto & thisPoint : bh_i->pointLocations_ii ) {

				Real64 f = integral( thisPoint, bh_i, bh_j, currTime );

				// Integrate using Simpson's
				if( index == 0 || index == lastIndex ) {
					sum_f += f;
				} else if( isEven( index ) ) {
					sum_f += 2 * f;
				} else {
					sum_f += 4 * f;
				}

				++index;
			}

			return ( bh_i->dl_ii / 3.0 ) * sum_f;

		} else {

			Real64 sum_f = 0;
			int index = 0;
			int const lastIndex = bh_i->pointLocations_i.size() - 1;
			for( auto & thisPoint : bh_i->pointLocations_i ) {

				Real64 f = integral( thisPoint, bh_i, bh_j, currTime );

				// Integrate using Simpson's
				if( index == 0 || index == lastIndex ) {
					sum_f += f;
				} else if( isEven( index ) ) {
					sum_f += 2 * f;
				} else {
					sum_f += 4 * f;
				}

				++index;
			}

			return ( bh_i->dl_i / 3.0 ) * sum_f;
		}
	}

	//******************************************************************************

	void
	GLHEVert::calcGFunctions()
	{

		int const numDaysInYear( 365 );
		using DataGlobals::HoursInDay;
		using DataGlobals::SecInHour;

		DisplayString( "Initializing GroundHeatExchanger:System: " + name );

		// Minimum simulation time for witch finite line source method is applicable
		Real64 const minTimeForgFunctions = 5 * pow_2( bhRadius ) / soil.diffusivity;

		// Time scale constant
		Real64 const ts = pow_2( bhLength ) / ( 9 * soil.diffusivity ) ;

		// Temporary vector for holding the LNTTS vals
		std::vector < Real64 > tempLNTTS;
		Real64 const lnttsStepSize = 0.5;

		tempLNTTS.push_back( log( minTimeForgFunctions / ts ) );

		// Determine how many g-function pairs to generate based on user defined maximum simulation time
		while( true ) {
			Real64 maxPossibleSimTime = exp( tempLNTTS.back() ) * ts;
			if( maxPossibleSimTime < myRespFactors->maxSimYears * numDaysInYear * HoursInDay * SecInHour ) {
				tempLNTTS.push_back( tempLNTTS.back() + lnttsStepSize );
			} else {
				break;
			}
		}

		// Setup the arrays
		myRespFactors->time.dimension ( tempLNTTS.size(), 0.0 );
		myRespFactors->LNTTS.dimension( tempLNTTS.size(), 0.0 );
		myRespFactors->GFNC.dimension( tempLNTTS.size(), 0.0 );

		int index = 1;
		for ( auto & thisLNTTS : tempLNTTS ) {
			myRespFactors->time( index ) = exp( thisLNTTS ) * ts;
			myRespFactors->LNTTS( index ) = thisLNTTS;
			++index;
		}

		// Calculate the g-functions
		for( int lntts_index = 1; lntts_index <= myRespFactors->LNTTS.size(); ++lntts_index ) {
			for( auto & bh_i : myRespFactors->myBorholes ) {
				Real64 sum_T_ji = 0;
				for( auto & bh_j : myRespFactors->myBorholes ) {
					sum_T_ji += doubleIntegral( bh_i, bh_j, myRespFactors->time( lntts_index ) );
				}
				myRespFactors->GFNC( lntts_index ) += sum_T_ji;
			}
			myRespFactors->GFNC( lntts_index ) /= ( 2 * totalTubeLength );
		}
	}

	//******************************************************************************

	void
	GLHESlinky::calcGFunctions()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Matt Mitchell
		//       DATE WRITTEN:    February, 2015
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// calculates g-functions for the slinky ground heat exchanger model

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 tLg_max( 0.0 );
		Real64 tLg_min( -2 );
		Real64 tLg_grid( 0.25 );
		Real64 ts( 3600 );
		Real64 tLg;
		Real64 t;
		Real64 convertYearsToSeconds( 356 * 24 * 60 * 60 );
		int NT;
		int numLC;
		int numRC;
		int coil;
		int trench;
		Real64 fraction;
		Array2D< Real64 > valStored( {0, numTrenches}, {0, numCoils}, -1.0 );
		Real64 gFunc;
		Real64 gFuncin;
		int m1;
		int n1;
		int m;
		int n;
		int mm1;
		int nn1;
		int i;
		Real64 disRing;
		int I0;
		int J0;
		Real64 doubleIntegralVal;
		Real64 midFieldVal;

		DisplayString( "Initializing GroundHeatExchanger:Slinky: " + name );

		X0.allocate( numCoils );
		Y0.allocate( numTrenches );

		// Calculate the number of g-functions required
		tLg_max = std::log10( maxSimYears * convertYearsToSeconds / ts );
		int NPairs = ( tLg_max - tLg_min ) / ( tLg_grid ) + 1;

		// Allocate and setup g-function arrays
		myRespFactors->GFNC.allocate( NPairs );
		myRespFactors->LNTTS.allocate( NPairs );
		QnMonthlyAgg.allocate( maxSimYears * 12 );
		QnHr.allocate( 730 + AGG + SubAGG );
		QnSubHr.allocate( ( SubAGG + 1 ) * maxTSinHr + 1 );
		LastHourN.allocate( SubAGG + 1 );

		for ( i = 1; i <= NPairs; ++i ) {
			myRespFactors->GFNC( i ) = 0.0;
			myRespFactors->LNTTS( i ) = 0.0;
		}

		// Calculate the number of loops (per trench) and number of trenches to be involved
			// Due to the symmetry of a slinky GHX field, we need only calculate about
			// on quarter of the rings' tube wall temperature perturbation to get the
			// mean wall temperature perturbation of the entire slinky GHX field.
		numLC = std::ceil( numCoils / 2.0 );
		numRC = std::ceil( numTrenches / 2.0 );

		// Calculate coordinates (X0, Y0, Z0) of a ring's center
		for ( coil = 1; coil <= numCoils; ++coil ) {
			X0( coil ) = coilPitch * ( coil - 1 );
		}
		for ( trench = 1; trench <= numTrenches; ++trench ) {
			Y0( trench ) = ( trench - 1 ) * trenchSpacing;
		}
		Z0 = coilDepth;

		// If number of trenches is greater than 1, one quarter of the rings are involved.
		// If number of trenches is 1, one half of the rings are involved.
		if ( numTrenches > 1 ) {
			fraction = 0.25;
		} else {
			fraction = 0.5;
		}

		// Calculate the corresponding time of each temperature response factor
		for ( NT = 1; NT <= NPairs; ++NT ) {
			tLg = tLg_min + tLg_grid * ( NT - 1 );
			t = std::pow( 10, tLg ) * ts;

			// Set the average temperature response of the whole field to zero
			gFunc = 0;

			valStored = -1.0;

			for ( m1 = 1; m1 <= numRC; ++m1 ) {
				for ( n1 = 1; n1 <= numLC; ++n1 ) {
					for ( m = 1; m <= numTrenches; ++m ) {
						for ( n = 1; n <= numCoils; ++n ) {

							// Zero out val after each iteration
							doubleIntegralVal = 0.0;
							midFieldVal = 0.0;

							// Calculate the distance between ring centers
							disRing = distToCenter( m, n, m1, n1 );

							// Save mm1 and nn1
							mm1 = std::abs( m - m1 );
							nn1 = std::abs( n - n1 );

							// If we're calculating a ring's temperature response to itself as a ring source,
							// then we need some extra effort in calculating the double integral
							if ( m1 == m && n1 == n ) {
								I0 = 33;
								J0 = 1089;
							} else {
								I0 = 33;
								J0 = 561;
							}

							// if the ring(n1, m1) is the near-field ring of the ring(n,m)
							if ( disRing <= 2.5 + coilDiameter ) {
								// if no calculated value has been stored
								if ( valStored( mm1, nn1 ) < 0 ) {
									doubleIntegralVal = doubleIntegral( m, n, m1, n1, t, I0, J0 );
									valStored( mm1, nn1 ) = doubleIntegralVal;
								// else: if a stored value is found for the combination of (m, n, m1, n1)
								} else {
									doubleIntegralVal = valStored( mm1, nn1 );
								}

								// due to symmetry, the temperature response of ring(n1, m1) should be 0.25, 0.5, or 1 times its calculated value
								if ( ! isEven( numTrenches ) && ! isEven( numCoils ) && m1 == numRC && n1 == numLC && numTrenches > 1.5 ) {
									gFuncin = 0.25 * doubleIntegralVal;
								} else if ( ! isEven( numTrenches ) && m1 == numRC && numTrenches > 1.5 ) {
									gFuncin = 0.5 * doubleIntegralVal;
								} else if ( ! isEven( numCoils ) && n1 == numLC ) {
									gFuncin = 0.5  * doubleIntegralVal;
								} else {
									gFuncin = doubleIntegralVal;
								}

							// if the ring(n1, m1) is in the far-field or the ring(n,m)
							} else if ( disRing > ( 10 + coilDiameter ) ) {
								gFuncin = 0;

							// else the ring(n1, m1) is in the middle-field of the ring(n,m)
							} else {
								// if no calculated value have been stored
								if ( valStored( mm1, nn1 ) < 0.0 ) {
									midFieldVal = midFieldResponseFunction( m, n, m1, n1, t );
									valStored( mm1, nn1 ) = midFieldVal;
								// if a stored value is found for the combination of (m, n, m1, n1), then
								} else {
									midFieldVal = valStored( mm1, nn1 );
								}

								// due to symmetry, the temperature response of ring(n1, m1) should be 0.25, 0.5, or 1 times its calculated value
								if ( ! isEven( numTrenches ) && ! isEven( numCoils ) && m1 == numRC && n1 == numLC && numTrenches > 1.5 ) {
									gFuncin = 0.25 * midFieldVal;
								} else if ( ! isEven( numTrenches ) && m1 == numRC && numTrenches > 1.5 ) {
									gFuncin = 0.5 * midFieldVal;
								} else if ( ! isEven( numCoils ) && n1 == numLC ) {
									gFuncin = 0.5  * midFieldVal;
								} else {
									gFuncin = midFieldVal;
								}

							}

							gFunc += gFuncin;

						} // n
					} // m
				} // n1
			} // m1

			myRespFactors->GFNC( NT ) = ( gFunc * ( coilDiameter / 2.0 ) ) / ( 4 * Pi	* fraction * numTrenches * numCoils );
			myRespFactors->LNTTS( NT ) = tLg;

		} // NT time
	}

	//******************************************************************************

	Real64
	GLHESlinky::nearFieldResponseFunction(
		int const m,
		int const n,
		int const m1,
		int const n1,
		Real64 const eta,
		Real64 const theta,
		Real64 const t
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Matt Mitchell
		//       DATE WRITTEN:    February, 2015
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the temperature response of from one near-field point to another

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 distance1;
		Real64 distance2;
		Real64 errFunc1;
		Real64 errFunc2;
		Real64 sqrtAlphaT;
		Real64 sqrtDistDepth;

		distance1 = distance( m, n, m1, n1, eta, theta );

		sqrtAlphaT = std::sqrt( soil.diffusivity * t );

		if ( ! verticalConfig ) {

			sqrtDistDepth = std::sqrt( pow_2( distance1 ) + 4 * pow_2( coilDepth ) );
			errFunc1 = std::erfc( 0.5 * distance1 / sqrtAlphaT );
			errFunc2 = std::erfc( 0.5 * sqrtDistDepth / sqrtAlphaT );

			return errFunc1 / distance1 - errFunc2 / sqrtDistDepth;

		} else {

			distance2 = distanceToFictRing( m, n, m1, n1, eta, theta );

			errFunc1 = std::erfc( 0.5 * distance1 / sqrtAlphaT );
			errFunc2 = std::erfc( 0.5 * distance2 / sqrtAlphaT );

			return errFunc1 / distance1 - errFunc2 / distance2;
		}
	}

	//******************************************************************************

	Real64
	GLHESlinky::midFieldResponseFunction(
		int const m,
		int const n,
		int const m1,
		int const n1,
		Real64 const t
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Matt Mitchell
		//       DATE WRITTEN:    February, 2015
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the temperature response of from one mid-field point to another

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 errFunc1;
		Real64 errFunc2;
		Real64 sqrtAlphaT;
		Real64 sqrtDistDepth;
		Real64 distance;

		sqrtAlphaT = std::sqrt( soil.diffusivity * t );

		distance = distToCenter( m, n, m1, n1 );
		sqrtDistDepth = std::sqrt( pow_2( distance ) + 4 * pow_2( coilDepth ) );

		errFunc1 = std::erfc( 0.5 * distance / sqrtAlphaT );
		errFunc2 = std::erfc( 0.5 * sqrtDistDepth / sqrtAlphaT );

		return 4 * pow_2( Pi ) * ( errFunc1 / distance - errFunc2 / sqrtDistDepth );
	}

	//******************************************************************************

	Real64
	GLHESlinky::distance(
		int const m,
		int const n,
		int const m1,
		int const n1,
		Real64 const eta,
		Real64 const theta
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Matt Mitchell
		//       DATE WRITTEN:    February, 2015
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the distance between any two points on any two loops

		Real64 pipeOuterRadius = pipe.outDia / 2.0;

		Real64 const cos_theta = std::cos( theta );
		Real64 const sin_theta = std::sin( theta );
		Real64 const cos_eta = std::cos( eta );
		Real64 const sin_eta = std::sin( eta );

		Real64 x = X0( n ) + cos_theta * ( coilDiameter / 2.0 );
		Real64 y = Y0( m ) + sin_theta * ( coilDiameter / 2.0 );

		Real64 xIn = X0( n1 ) + cos_eta * ( coilDiameter / 2.0 - pipeOuterRadius );
		Real64 yIn = Y0( m1 ) + sin_eta * ( coilDiameter / 2.0 - pipeOuterRadius );

		Real64 xOut = X0( n1 ) + cos_eta * ( coilDiameter / 2.0 + pipeOuterRadius );
		Real64 yOut = Y0( m1 ) + sin_eta * ( coilDiameter / 2.0 + pipeOuterRadius );

		if ( ! verticalConfig ) {

			return 0.5 * std::sqrt( pow_2( x - xIn ) + pow_2( y - yIn ) )
				+ 0.5 * std::sqrt( pow_2( x - xOut ) + pow_2( y - yOut ) );

		} else {

			Real64 z = Z0 + sin_theta * ( coilDiameter / 2.0 );

			Real64 zIn = Z0 + sin_eta * ( coilDiameter / 2.0 - pipeOuterRadius );
			Real64 zOut = Z0 + sin_eta * ( coilDiameter / 2.0 + pipeOuterRadius );

			return 0.5 * std::sqrt( pow_2( x - xIn ) + pow_2( Y0( m1 ) - Y0( m ) ) + pow_2( z - zIn ) )
				+ 0.5 * std::sqrt( pow_2( x - xOut ) + pow_2( Y0( m1 ) - Y0( m ) ) + pow_2( z - zOut ) );
		}
	}

	//******************************************************************************

	Real64
	GLHESlinky::distanceToFictRing(
		int const m,
		int const n,
		int const m1,
		int const n1,
		Real64 const eta,
		Real64 const theta
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Matt Mitchell
		//       DATE WRITTEN:    February, 2015
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the distance between any two points between real and fictitious rings

		Real64 pipeOuterRadius = pipe.outDia / 2.0;

		Real64 const sin_theta = std::sin( theta );
		Real64 const cos_theta = std::cos( theta );
		Real64 const sin_eta = std::sin( eta );
		Real64 const cos_eta = std::cos( eta );

		Real64 x = X0( n ) + cos_theta * ( coilDiameter / 2.0 );
		// Real64 y = Y0( m ) + sin_theta * ( coilDiameter / 2.0 );
		Real64 z = Z0 + sin_theta * ( coilDiameter / 2.0 ) + 2 * coilDepth;

		Real64 xIn = X0( n1 ) + cos_eta * ( coilDiameter / 2.0 - pipeOuterRadius );
		//Real64 yIn = Y0( m1 ) + sin_eta * ( coilDiameter / 2.0 - pipeOuterRadius );
		Real64 zIn = Z0 + sin_eta * ( coilDiameter / 2.0 - pipeOuterRadius );

		Real64 xOut = X0( n1 ) + cos_eta * ( coilDiameter / 2.0 + pipeOuterRadius );
		//Real64 yOut = Y0( m1 ) + sin_eta * ( coilDiameter / 2.0 + pipeOuterRadius );
		Real64 zOut = Z0 + sin_eta * ( coilDiameter / 2.0 + pipeOuterRadius );

		return 0.5 * std::sqrt( pow_2( x - xIn ) + pow_2( Y0( m1 ) - Y0( m ) ) + pow_2( z - zIn ) )
				+ 0.5 * std::sqrt( pow_2( x - xOut ) + pow_2( Y0( m1 ) - Y0( m ) ) + pow_2( z - zOut ) );

	}

	//******************************************************************************

	Real64
	GLHESlinky::distToCenter(
		int const m,
		int const n,
		int const m1,
		int const n1
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Matt Mitchell
		//       DATE WRITTEN:    February, 2015
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the center-to-center distance between rings

		return std::sqrt( pow_2( X0( n ) - X0( n1 ) ) + pow_2( Y0( m ) - Y0( m1 ) ) );
	}

	//******************************************************************************

	inline bool
	GLHEBase::isEven(
		int const val
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Matt Mitchell
		//       DATE WRITTEN:    February, 2015
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// Determines if an integer is even

		if ( val % 2 == 0 ) {
			return true;
		} else {
			return false;
		}
	}

	//******************************************************************************

	Real64
	GLHESlinky::integral(
		int const m,
		int const n,
		int const m1,
		int const n1,
		Real64 const t,
		Real64 const eta,
		Real64 const J0
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Matt Mitchell
		//       DATE WRITTEN:    February, 2015
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// Integrates the temperature response at one point based on
		// input from other points

		// METHODOLOGY EMPLOYED:
		// Simpson's 1/3 rule of integration

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 sumIntF( 0.0 );
		Real64 theta( 0.0 );
		Real64 theta1( 0.0 );
		Real64 theta2( 2 * Pi );
		Real64 h;
		int j;
		Array1D< Real64 > f( J0, 0.0 );

		h = ( theta2 - theta1 ) / ( J0 - 1 );

		// Calculate the function at various equally spaced x values
		for ( j = 1; j <= J0; ++j ) {

			theta = theta1 + ( j - 1 ) * h;

			f( j ) = nearFieldResponseFunction( m, n, m1, n1, eta, theta, t );

			if ( j == 1 || j == J0 ) {
				f( j ) = f( j );
			} else if ( isEven( j ) ) {
				f( j ) = 4 * f( j );
			} else {
				f( j ) = 2 * f( j );
			}

			sumIntF += f( j );
		}

		return ( h / 3 ) * sumIntF;
	}

	//******************************************************************************

	Real64
	GLHESlinky::doubleIntegral(
		int const m,
		int const n,
		int const m1,
		int const n1,
		Real64 const t,
		int const I0,
		int const J0
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Matt Mitchell
		//       DATE WRITTEN:    February, 2015
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// Integrates the temperature response at one point based on
		// input from other points

		// METHODOLOGY EMPLOYED:
		// Simpson's 1/3 rule of integration

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 sumIntF( 0.0 );
		Real64 eta( 0.0 );
		Real64 eta1( 0.0 );
		Real64 eta2( 2 * Pi );
		Real64 h;
		int i;
		Array1D< Real64 > g( I0, 0.0 );

		h = ( eta2 - eta1 ) / ( I0 - 1 );

		// Calculates the value of the function at various equally spaced values
		for ( i = 1; i <= I0; ++i ) {

			eta = eta1 + ( i - 1 ) * h;
			g( i ) = integral( m, n, m1, n1, t, eta, J0 );

			if ( i == 1 || i == I0 ) {
				g( i ) = g( i );
			} else if ( isEven( i ) ) {
				g( i ) = 4 * g( i );
			} else {
				g( i ) = 2 * g( i );
			}

			sumIntF += g( i );
		}

		return ( h / 3 ) * sumIntF;
	}

	//******************************************************************************

	void
	GLHEVert::getAnnualTimeConstant()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Matt Mitchell
		//       DATE WRITTEN:    February, 2015
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate annual time constant for ground conduction

		timeSS = ( pow_2( bhLength ) / ( 9.0 * soil.diffusivity ) ) / SecInHour / 8760.0;
		timeSSFactor = timeSS * 8760.0;
	}

	//******************************************************************************

	void
	GLHESlinky::getAnnualTimeConstant()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Matt Mitchell
		//       DATE WRITTEN:    February, 2015
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate annual time constant for ground conduction

		timeSSFactor = 1.0;
	}

	//******************************************************************************

	void
	GLHEBase::calcGroundHeatExchanger()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    August, 2000
		//       MODIFIED         Arun Murugappan
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// This is the main routine to simulate the operation of vertical
		// closed-loop ground heat exchangers (GLHE).

		// METHODOLOGY EMPLOYED:
		// The borehole and fluid temperatures are calculated from the response to
		// the current heat transfer rate and the response to the history of past
		// applied heat pulses. The response to each pulse is calculated from a non-
		// dimensionalized response function, or G-function, that is specific to the
		// given borehole field arrangement, depth and spacing. The data defining
		// this function is read from input.
		// The heat pulse histories need to be recorded over an extended period (months).
		// To aid computational efficiency past pulses are continuously aggregated into
		// equivalent heat pulses of longer duration, as each pulse becomes less recent.

		// REFERENCES:
		// Eskilson, P. 'Thermal Analysis of Heat Extraction Boreholes' Ph.D. Thesis:
		//   Dept. of Mathematical Physics, University of Lund, Sweden, June 1987.
		// Yavuzturk, C., J.D. Spitler. 1999. 'A Short Time Step Response Factor Model
		//   for Vertical Ground Loop Heat Exchangers.' ASHRAE Transactions. 105(2): 475-485.

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS
		static std::string const RoutineName( "CalcGroundHeatExchanger" );

		//LOCAL PARAMETERS
		Real64 fluidDensity;
		Real64 kGroundFactor;
		Real64 cpFluid;
		Real64 gFuncVal; // Interpolated G function value at a sub-hour
		static Real64 ToutNew( 19.375 );
		Real64 fluidAveTemp;
		Real64 C_1;
		int numOfMonths; // the number of months of simulation elapsed
		int currentMonth; // The Month up to which the monthly blocks are superposed
		Real64 sumQnMonthly; // tmp variable which holds the sum of the Temperature difference due to Aggregated heat extraction/rejection step
		Real64 sumQnHourly; // same as above for hourly
		Real64 sumQnSubHourly; // same as above for sub-hourly( with no aggregation]
		Real64 RQMonth;
		Real64 RQHour;
		Real64 RQSubHr;
		int I;
		Real64 tmpQnSubHourly; // current Qn sub-hourly value
		int hourlyLimit; // number of hours to be taken into account in superposition
		int subHourlyLimit; // number of sub-hourly to be taken into account in sub-hourly superposition
		Real64 sumTotal( 0.0 ); // sum of all the Qn (load) blocks
		Real64 C0; // **Intermediate constants used
		Real64 C1; // **Intermediate constants used
		Real64 C2; // **in explicit  calculation of the
		Real64 C3; // **temperature at the U tube outlet.
		static int PrevN( 1 ); // The saved value of N at previous time step
		int IndexN; // Used to index the LastHourN array
		static bool updateCurSimTime( true ); // Used to reset the CurSimTime to reset after WarmupFlag
		static bool triggerDesignDayReset( false );
		static bool firstTime( true );

		// Calculate G-Functions
		if ( firstTime ) {
			calcGFunctions();
			firstTime = false;
		}

		inletTemp = Node( inletNodeNum ).Temp;

		cpFluid = GetSpecificHeatGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
		fluidDensity = GetDensityGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );

		kGroundFactor = 2.0 * Pi * soil.k;

		// Get time constants
		getAnnualTimeConstant();

		if ( triggerDesignDayReset && WarmupFlag ) updateCurSimTime = true;
		if ( DayOfSim == 1 && updateCurSimTime ) {
			currentSimTime = 0.0;
			prevTimeSteps = 0.0;
			QnHr = 0.0;
			QnMonthlyAgg = 0.0;
			QnSubHr = 0.0;
			LastHourN = 1;
			N = 1;
			updateCurSimTime = false;
			triggerDesignDayReset = false;
		}

		currentSimTime = ( DayOfSim - 1 ) * 24 + HourOfDay - 1 + ( TimeStep - 1 ) * TimeStepZone + SysTimeElapsed; //+ TimeStepsys
		locHourOfDay = mod( currentSimTime, hrsPerDay ) + 1;
		locDayOfSim = currentSimTime / 24 + 1;

		if ( DayOfSim > 1 ) {
			updateCurSimTime = true;
		}

		if ( !WarmupFlag ) {
			triggerDesignDayReset = true;
		}

		if ( currentSimTime <= 0.0 ) {
			prevTimeSteps = 0.0; // This resets history when rounding 24:00 hours during warmup avoids hard crash later
			calcAggregateLoad(); // Just allocates and initializes prevHour array
			return;
		}

		// Store currentSimTime in prevTimeSteps only if a time step occurs

		if ( prevTimeSteps( 1 ) != currentSimTime ) {
			prevTimeSteps = eoshift( prevTimeSteps, -1, currentSimTime );
			++N;
		}

		if ( N != PrevN ) {
			PrevN = N;
			QnSubHr = eoshift( QnSubHr, -1, lastQnSubHr );
		}

		calcAggregateLoad();

		// Update the heat exchanger resistance each time
		calcHXResistance();

		if ( N == 1 ) {
			if ( massFlowRate <= 0.0 ) {
				tmpQnSubHourly = 0.0;
				fluidAveTemp = tempGround;
				ToutNew = inletTemp;
			} else {
				gFuncVal = getGFunc( currentSimTime / ( timeSSFactor ) );

				C_1 = ( totalTubeLength ) / ( 2.0 * massFlowRate * cpFluid );
				tmpQnSubHourly = ( tempGround - inletTemp ) / ( gFuncVal / ( kGroundFactor ) + HXResistance + C_1 );
				fluidAveTemp = tempGround - tmpQnSubHourly * HXResistance;
				ToutNew = tempGround - tmpQnSubHourly * ( gFuncVal / ( kGroundFactor ) + HXResistance - C_1 );
			}
		} else {
			// no monthly super position
			if ( currentSimTime < ( hrsPerMonth + AGG + SubAGG ) ) {

				// Calculate the Sub Hourly Superposition

				sumQnSubHourly = 0.0;
				if ( int( currentSimTime ) < SubAGG ) {
					IndexN = int( currentSimTime ) + 1;
				} else {
					IndexN = SubAGG + 1;
				}
				subHourlyLimit = N - LastHourN( IndexN ); //Check this when running simulation

				for ( I = 1; I <= subHourlyLimit; ++I ) {
					if ( I == subHourlyLimit ) {
						if ( int( currentSimTime ) >= SubAGG ) {
							gFuncVal = getGFunc( ( currentSimTime - prevTimeSteps( I + 1 ) ) / ( timeSSFactor ) );
							RQSubHr = gFuncVal / ( kGroundFactor );
							sumQnSubHourly += ( QnSubHr( I ) - QnHr( IndexN ) ) * RQSubHr;
						} else {
							gFuncVal = getGFunc( ( currentSimTime - prevTimeSteps( I + 1 ) ) / ( timeSSFactor ) );
							RQSubHr = gFuncVal / ( kGroundFactor );
							sumQnSubHourly += QnSubHr( I ) * RQSubHr;
						}
						break;
					}
					//prevTimeSteps(I+1) This is "I+1" because prevTimeSteps(1) = CurrentTimestep
					gFuncVal = getGFunc( ( currentSimTime - prevTimeSteps( I + 1 ) ) / ( timeSSFactor ) );
					RQSubHr = gFuncVal / ( kGroundFactor );
					sumQnSubHourly += ( QnSubHr( I ) - QnSubHr( I + 1 ) ) * RQSubHr;
				}

				// Calculate the Hourly Superposition

				hourlyLimit = int( currentSimTime );
				sumQnHourly = 0.0;
				for ( I = SubAGG + 1; I <= hourlyLimit; ++I ) {
					if ( I == hourlyLimit ) {
						gFuncVal = getGFunc( currentSimTime / ( timeSSFactor ) );
						RQHour = gFuncVal / ( kGroundFactor );
						sumQnHourly += QnHr( I ) * RQHour;
						break;
					}
					gFuncVal = getGFunc( ( currentSimTime - int( currentSimTime ) + I ) / ( timeSSFactor ) );
					RQHour = gFuncVal / ( kGroundFactor );
					sumQnHourly += ( QnHr( I ) - QnHr( I + 1 ) ) * RQHour;
				}

				// Find the total Sum of the Temperature difference due to all load blocks
				sumTotal = sumQnSubHourly + sumQnHourly;

				//Calculate the sub-hourly temperature due the Last Time steps Load
				gFuncVal = getGFunc( ( currentSimTime - prevTimeSteps( 2 ) ) / ( timeSSFactor ) );
				RQSubHr = gFuncVal / ( kGroundFactor );

				if ( massFlowRate <= 0.0 ) {
					tmpQnSubHourly = 0.0;
					fluidAveTemp = tempGround - sumTotal; // Q(N)*RB = 0
					ToutNew = inletTemp;
				} else {
					// Dr.Spitler's Explicit set of equations to calculate the New Outlet Temperature of the U-Tube
					C0 = RQSubHr;
					C1 = tempGround - ( sumTotal - QnSubHr( 1 ) * RQSubHr );
					C2 = totalTubeLength / ( 2.0 * massFlowRate * cpFluid );
					C3 = massFlowRate * cpFluid / ( totalTubeLength );
					tmpQnSubHourly = ( C1 - inletTemp ) / ( HXResistance + C0 - C2 + ( 1 / C3 ) );
					fluidAveTemp = C1 - ( C0 + HXResistance ) * tmpQnSubHourly;
					ToutNew = C1 + ( C2 - C0 - HXResistance ) * tmpQnSubHourly;
				}

			} else { // Monthly Aggregation and super position

				numOfMonths = ( currentSimTime + 1 ) / hrsPerMonth;

				if ( currentSimTime < ( ( numOfMonths ) * hrsPerMonth ) + AGG + SubAGG ) {
					currentMonth = numOfMonths - 1;
				} else {
					currentMonth = numOfMonths;
				}

				// Monthly superposition
				sumQnMonthly = 0.0;
				for ( I = 1; I <= currentMonth; ++I ) {
					if ( I == 1 ) {
						gFuncVal = getGFunc( currentSimTime / ( timeSSFactor ) );
						RQMonth = gFuncVal / ( kGroundFactor );
						sumQnMonthly += QnMonthlyAgg( I ) * RQMonth;
						continue;
					}
					gFuncVal = getGFunc( ( currentSimTime - ( I - 1 ) * hrsPerMonth ) / ( timeSSFactor ) );
					RQMonth = gFuncVal / ( kGroundFactor );
					sumQnMonthly += ( QnMonthlyAgg( I ) - QnMonthlyAgg( I - 1 ) ) * RQMonth;
				}

				// Hourly Superposition
				hourlyLimit = int( currentSimTime - currentMonth * hrsPerMonth );
				sumQnHourly = 0.0;
				for ( I = 1 + SubAGG; I <= hourlyLimit; ++I ) {
					if ( I == hourlyLimit ) {
						gFuncVal = getGFunc( ( currentSimTime - int( currentSimTime ) + I ) / ( timeSSFactor ) );
						RQHour = gFuncVal / ( kGroundFactor );
						sumQnHourly += ( QnHr( I ) - QnMonthlyAgg( currentMonth ) ) * RQHour;
						break;
					}
					gFuncVal = getGFunc( ( currentSimTime - int( currentSimTime ) + I ) / ( timeSSFactor ) );
					RQHour = gFuncVal / ( kGroundFactor );
					sumQnHourly += ( QnHr( I ) - QnHr( I + 1 ) ) * RQHour;
				}

				// sub-hourly Superposition
				subHourlyLimit = N - LastHourN( SubAGG + 1 );
				sumQnSubHourly = 0.0;
				for ( I = 1; I <= subHourlyLimit; ++I ) {
					if ( I == subHourlyLimit ) {
						gFuncVal = getGFunc( ( currentSimTime - prevTimeSteps( I + 1 ) ) / ( timeSSFactor ) );
						RQSubHr = gFuncVal / ( kGroundFactor );
						sumQnSubHourly += ( QnSubHr( I ) - QnHr( SubAGG + 1 ) ) * RQSubHr;
						break;
					}
					gFuncVal = getGFunc( ( currentSimTime - prevTimeSteps( I + 1 ) ) / ( timeSSFactor ) );
					RQSubHr = gFuncVal / ( kGroundFactor );
					sumQnSubHourly += ( QnSubHr( I ) - QnSubHr( I + 1 ) ) * RQSubHr;
				}

				sumTotal = sumQnMonthly + sumQnHourly + sumQnSubHourly;

				// Calculate the sub-hourly temperature due the Last Time steps Load

				gFuncVal = getGFunc( ( currentSimTime - prevTimeSteps( 2 ) ) / ( timeSSFactor ) );
				RQSubHr = gFuncVal / ( kGroundFactor );

				if ( massFlowRate <= 0.0 ) {
					tmpQnSubHourly = 0.0;
					fluidAveTemp = tempGround - sumTotal; // Q(N)*RB = 0
					ToutNew = inletTemp;
				} else {
					// Explicit set of equations to calculate the New Outlet Temperature of the U-Tube
					C0 = RQSubHr;
					C1 = tempGround - ( sumTotal - QnSubHr( 1 ) * RQSubHr );
					C2 = totalTubeLength / ( 2 * massFlowRate * cpFluid );
					C3 = massFlowRate * cpFluid / ( totalTubeLength );
					tmpQnSubHourly = ( C1 - inletTemp ) / ( HXResistance + C0 - C2 + ( 1 / C3 ) );
					fluidAveTemp = C1 - ( C0 + HXResistance ) * tmpQnSubHourly;
					ToutNew = C1 + ( C2 - C0 - HXResistance ) * tmpQnSubHourly;
				}
			} //  end of AGG OR NO AGG
		} // end of N  = 1 branch
		bhTemp = tempGround - sumTotal;
		// Load the QnSubHourly Array with a new value at end of every timestep

		lastQnSubHr = tmpQnSubHourly;
		outletTemp = ToutNew;
		QGLHE = tmpQnSubHourly * totalTubeLength;
		aveFluidTemp = fluidAveTemp;

	}

	//******************************************************************************

	void
	GLHEBase::updateGHX()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Matt Mitchell
		//       DATE WRITTEN:    February, 2015
		//       MODIFIED:        na
		//       RE-ENGINEERED:   na

		// PURPOSE OF THIS SUBROUTINE:
		// Updates the outlet node and check for out of bounds temperatures

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using General::TrimSigDigits;
		using PlantUtilities::SafeCopyPlantNode;

		// SUBROUTINE ARGUMENT DEFINITIONS
		static std::string const RoutineName( "UpdateGroundHeatExchanger" );

		Real64 fluidDensity;
		Real64 const deltaTempLimit( 100.0 ); // temp limit for warnings
		Real64 GLHEdeltaTemp; // ABS(Outlet temp -inlet temp)
		static int numErrorCalls( 0 );

		SafeCopyPlantNode( inletNodeNum, outletNodeNum );

		Node( outletNodeNum ).Temp = outletTemp;
		Node( outletNodeNum ).Enthalpy = outletTemp * GetSpecificHeatGlycol( PlantLoop( loopNum ).FluidName, outletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );

		GLHEdeltaTemp = std::abs( outletTemp - inletTemp );

		if ( GLHEdeltaTemp > deltaTempLimit && numErrorCalls < numVerticalGLHEs && ! WarmupFlag ) {
			fluidDensity = GetDensityGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
			designMassFlow = designFlow * fluidDensity;
			ShowWarningError( "Check GLHE design inputs & g-functions for consistency" );
			ShowContinueError( "For GroundHeatExchanger: " + name + "GLHE delta Temp > 100C." );
			ShowContinueError( "This can be encountered in cases where the GLHE mass flow rate is either significantly" );
			ShowContinueError( " lower than the design value, or cases where the mass flow rate rapidly changes." );
			ShowContinueError( "GLHE Current Flow Rate=" + TrimSigDigits( massFlowRate, 3 ) + "; GLHE Design Flow Rate=" + TrimSigDigits( designMassFlow, 3 ) );
			++numErrorCalls;
		}
	}

	//******************************************************************************

	void
	GLHEBase::calcAggregateLoad()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Arun Murugappan
		//       DATE WRITTEN:    August, 2000
		//       MODIFIED:        na
		//       RE-ENGINEERED:   na

		// PURPOSE OF THIS SUBROUTINE:
		// Manages the heat transfer history.

		// METHODOLOGY EMPLOYED:
		// The heat pulse histories need to be recorded over an extended period (months).
		// To aid computational efficiency past pulses are continuously aggregated into
		// equivalent heat pulses of longer duration, as each pulse becomes less recent.
		// Past sub-hourly loads are re-aggregated into equivalent hourly and monthly loads.

		// REFERENCES:
		// Eskilson, P. 'Thermal Analysis of Heat Extraction Boreholes' Ph.D. Thesis:
		//   Dept. of Mathematical Physics, University of Lund, Sweden, June 1987.
		// Yavuzturk, C., J.D. Spitler. 1999. 'A Short Time Step Response Factor Model
		//   for Vertical Ground Loop Heat Exchangers. ASHRAE Transactions. 105(2): 475-485.

		// USE STATEMENTS:
		// na

		// Locals
		//LOCAL VARIABLES
		Real64 SumQnMonth; // intermediate variable to store the monthly heat rejection/
		Real64 SumQnHr;
		int MonthNum;
		int J; // Loop counter

		if ( currentSimTime <= 0.0 ) return;

		//FOR EVERY HOUR UPDATE THE HOURLY QN QnHr(J)
		//THIS IS DONE BY AGGREGATING THE sub-hourly QN FROM THE PREVIOUS HOUR TO UNTIL THE CURRENT HOUR
		//AND STORING IT IN  verticalGLHE(GLHENum)%QnHr(J)

		//sub-hourly Qn IS NOT AGGREGATED . IT IS THE BASIC LOAD
		if ( prevHour != locHourOfDay ) {
			SumQnHr = 0.0;
			for ( J = 1; J <= ( N - LastHourN( 1 ) ); ++J ) {
				SumQnHr += QnSubHr( J ) * std::abs( prevTimeSteps( J ) - prevTimeSteps( J + 1 ) );
			}
			if ( prevTimeSteps( 1 ) != prevTimeSteps( J ) ){
				SumQnHr /= std::abs( prevTimeSteps( 1 ) - prevTimeSteps( J ) );
			} else {
				SumQnHr /= 0.05; // estimated small timestep
			}
			QnHr = eoshift( QnHr, -1, SumQnHr );
			LastHourN = eoshift( LastHourN, -1, N );
		}

		//CHECK IF A MONTH PASSES...
		if ( mod( ( ( locDayOfSim - 1 ) * hrsPerDay + ( locHourOfDay ) ), hrsPerMonth ) == 0 && prevHour != locHourOfDay ) {
			MonthNum = ( locDayOfSim * hrsPerDay + locHourOfDay ) / hrsPerMonth;
			SumQnMonth = 0.0;
			for ( J = 1; J <= int( hrsPerMonth ); ++J ) {
				SumQnMonth += QnHr( J );
			}
			SumQnMonth /= hrsPerMonth;
			QnMonthlyAgg( MonthNum ) = SumQnMonth;
		}
		if ( prevHour != locHourOfDay ) {
			prevHour = locHourOfDay;
		}
	}

	//******************************************************************************

	void
	GetGroundHeatExchangerInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    August, 2000
		//       MODIFIED         Arun Murugappan
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::SameString;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using DataEnvironment::MaxNumberSimYears;
		using PlantUtilities::RegisterPlantCompDesignFlow;

		//GET NUMBER OF ALL EQUIPMENT TYPES

		numVerticalGLHEs = GetNumObjectsFound( "GroundHeatExchanger:System" );
		numSlinkyGLHEs = GetNumObjectsFound( "GroundHeatExchanger:Slinky" );
		numVertArray = GetNumObjectsFound ( "GroundHeatExchanger:Vertical:Array" );
		numVertProps = GetNumObjectsFound ( "GroundHeatExchanger:Vertical:Properties" );
		numResponseFactors = GetNumObjectsFound( "GroundHeatExchanger:ResponseFactors" );
		numSingleBorehole = GetNumObjectsFound ( "GroundHeatExchanger:Vertical:Single" );

		if ( numVerticalGLHEs <= 0 && numSlinkyGLHEs <= 0 ) {
			ShowSevereError( "Error processing inputs for GLHE objects" );
			ShowContinueError( "Simulation indicated these objects were found, but input processor doesn't find any" );
			ShowContinueError( "Check inputs for GroundHeatExchanger:System and GroundHeatExchanger:Slinky" );
			ShowContinueError( "Also check plant/branch inputs for references to invalid/deleted objects" );
			errorsFound = true;
		}

		if ( numVertProps > 0 ) {

			DataIPShortCuts::cCurrentModuleObject = "GroundHeatExchanger:Vertical:Properties";

			for ( int propNum = 1; propNum <= numVertProps; ++ propNum ) {

				// just a few vars to pass in and out to GetObjectItem
				int ioStatus;
				int numAlphas;
				int numNumbers;

				// get the input data and store it in the Shortcuts structures
				InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, propNum,
											   DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs,
											   numNumbers, ioStatus, DataIPShortCuts::lNumericFieldBlanks,
											   DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames,
											   DataIPShortCuts::cNumericFieldNames );

				// the input processor validates the numeric inputs based on the IDD definition
				// still validate the name to make sure there aren't any duplicates or blanks
				// blanks are easy: fatal if blank
				if ( DataIPShortCuts::lAlphaFieldBlanks( 1 ) ) {
					ShowFatalError( "Invalid input for " + DataIPShortCuts::cCurrentModuleObject +
						" object: Name cannot be blank" );
				}

				// we just need to loop over the existing vector elements to check for duplicates since we haven't add this one yet
				for ( auto &existingVertProp : vertPropsVector ) {
					if ( DataIPShortCuts::cAlphaArgs( 1 ) == existingVertProp->name ) {
						ShowFatalError( "Invalid input for " + DataIPShortCuts::cCurrentModuleObject +
							" object: Duplicate name found: " + existingVertProp->name );
					}
				}

				// Build out new instance and add it to the vector
				std::shared_ptr < GLHEVertPropsStruct > thisProp( new GLHEVertPropsStruct );
				thisProp->name = DataIPShortCuts::cAlphaArgs( 1 );
				thisProp->bhTopDepth = DataIPShortCuts::rNumericArgs( 1 );
				thisProp->bhLength = DataIPShortCuts::rNumericArgs( 2 );
				thisProp->bhDiameter = DataIPShortCuts::rNumericArgs( 3 );
				thisProp->grout.k = DataIPShortCuts::rNumericArgs( 4 );
				thisProp->grout.rhoCp = DataIPShortCuts::rNumericArgs( 5 );
				thisProp->pipe.k = DataIPShortCuts::rNumericArgs( 6 );
				thisProp->pipe.outDia = DataIPShortCuts::rNumericArgs( 7 );
				thisProp->pipe.thickness = DataIPShortCuts::rNumericArgs( 8 );
				thisProp->bhUTubeDist = DataIPShortCuts::rNumericArgs( 9 );
				vertPropsVector.push_back( thisProp );
			}
		}

		if ( numResponseFactors > 0 ) {

			DataIPShortCuts::cCurrentModuleObject = "GroundHeatExchanger:ResponseFactors";

			for ( int rfNum = 1; rfNum <= numResponseFactors; ++ rfNum ) {

				// just a few vars to pass in and out to GetObjectItem
				int ioStatus;
				int numAlphas;
				int numNumbers;

				// get the input data and store it in the Shortcuts structures
				InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, rfNum,
											   DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs,
											   numNumbers, ioStatus, DataIPShortCuts::lNumericFieldBlanks,
											   DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames,
											   DataIPShortCuts::cNumericFieldNames );

				// the input processor validates the numeric inputs based on the IDD definition
				// still validate the name to make sure there aren't any duplicates or blanks
				// blanks are easy: fatal if blank
				if ( DataIPShortCuts::lAlphaFieldBlanks( 1 ) ) {
					ShowFatalError( "Invalid input for " + DataIPShortCuts::cCurrentModuleObject +
						" object: Name cannot be blank" );
				}

				// we just need to loop over the existing vector elements to check for duplicates since we haven't add this one yet
				for ( auto & existingVertProp : vertPropsVector ) {
					if ( DataIPShortCuts::cAlphaArgs( 1 ) == existingVertProp->name ) {
						ShowFatalError( "Invalid input for " + DataIPShortCuts::cCurrentModuleObject +
							" object: Duplicate name found: " + existingVertProp->name );
					}
				}

				// Build out new instance and add it to the vector
				std::shared_ptr< GLHEResponseFactorsStruct > thisRF( new GLHEResponseFactorsStruct );
				thisRF->name = DataIPShortCuts::cAlphaArgs( 1 );
				thisRF->props = GetVertProps( DataIPShortCuts::cAlphaArgs( 2 ) );
				thisRF->numBoreholes = DataIPShortCuts::rNumericArgs( 1 );
				thisRF->gRefRatio = DataIPShortCuts::rNumericArgs( 2 );

				thisRF->maxSimYears = MaxNumberSimYears;

				int numPreviousFields = 2;
				int numFields = 0;
				for ( auto & isFieldBlank : DataIPShortCuts::lNumericFieldBlanks ) {
					if ( !isFieldBlank ) {
						numFields += 1;
					} else if ( isFieldBlank ) {
						break;
					}
				}

				if ( ( numFields - numPreviousFields ) % 2 == 0 ) {
					thisRF->numGFuncPairs = ( numFields - numPreviousFields ) / 2;
				} else {
					errorsFound = true;
					ShowSevereError( "Errors found processing response factor input for Response Factor= " + thisRF->name );
					ShowSevereError( "Uneven number of g-function pairs" );
				}

				thisRF->LNTTS.dimension( thisRF->numGFuncPairs, 0.0 );
				thisRF->GFNC.dimension( thisRF->numGFuncPairs, 0.0 );

				int indexNum = 3;
				for ( int pairNum = 1; pairNum <= thisRF->numGFuncPairs; ++pairNum ) {
					thisRF->LNTTS( pairNum ) = DataIPShortCuts::rNumericArgs( indexNum );
					thisRF->GFNC( pairNum ) = DataIPShortCuts::rNumericArgs( indexNum + 1 );
					indexNum += 2;
				}

				responseFactorsVector.push_back( thisRF );
			}
		}

		if ( numVertArray > 0 ) {

			DataIPShortCuts::cCurrentModuleObject = "GroundHeatExchanger:Vertical:Array";

			for ( int arrayNum = 1; arrayNum <= numVertArray; ++arrayNum ) {

				// just a few vars to pass in and out to GetObjectItem
				int ioStatus;
				int numAlphas;
				int numNumbers;

				// get the input data and store it in the Shortcuts structures
				InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, arrayNum,
											   DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs,
											   numNumbers, ioStatus, DataIPShortCuts::lNumericFieldBlanks,
											   DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames,
											   DataIPShortCuts::cNumericFieldNames );

				// the input processor validates the numeric inputs based on the IDD definition
				// still validate the name to make sure there aren't any duplicates or blanks
				// blanks are easy: fatal if blank
				if ( DataIPShortCuts::lAlphaFieldBlanks( 1 ) ) {
					ShowFatalError( "Invalid input for " + DataIPShortCuts::cCurrentModuleObject +
						" object: Name cannot be blank" );
				}

				// we just need to loop over the existing vector elements to check for duplicates since we haven't add this one yet
				for ( auto & existingVerticalArray : vertArraysVector ) {
					if ( DataIPShortCuts::cAlphaArgs( 1 ) == existingVerticalArray->name ) {
						ShowFatalError( "Invalid input for " + DataIPShortCuts::cCurrentModuleObject +
							" object: Duplicate name found: " + existingVerticalArray->name );
					}
				}

				// Build out new instance and add it to the vector
				std::shared_ptr< GLHEVertArrayStruct > thisArray( new GLHEVertArrayStruct );
				thisArray->name = DataIPShortCuts::cAlphaArgs( 1 );
				thisArray->props = GetVertProps( DataIPShortCuts::cAlphaArgs( 2 ) );
				thisArray->numBHinXDirection = DataIPShortCuts::rNumericArgs( 1 );
				thisArray->numBHinYDirection = DataIPShortCuts::rNumericArgs( 2 );
				thisArray->bhSpacing = DataIPShortCuts::rNumericArgs( 3 );
				vertArraysVector.push_back( thisArray );
			}
		}

		if ( numSingleBorehole > 0 ) {

			DataIPShortCuts::cCurrentModuleObject = "GroundHeatExchanger:Vertical:Single";

			for ( int bhNum = 1; bhNum <= numSingleBorehole; ++bhNum ) {

				// just a few vars to pass in and out to GetObjectItem
				int ioStatus;
				int numAlphas;
				int numNumbers;

				// get the input data and store it in the Shortcuts structures
				InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, bhNum,
											   DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs,
											   numNumbers, ioStatus, DataIPShortCuts::lNumericFieldBlanks,
											   DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames,
											   DataIPShortCuts::cNumericFieldNames );

				// the input processor validates the numeric inputs based on the IDD definition
				// still validate the name to make sure there aren't any duplicates or blanks
				// blanks are easy: fatal if blank
				if ( DataIPShortCuts::lAlphaFieldBlanks( 1 ) ) {
					ShowFatalError( "Invalid input for " + DataIPShortCuts::cCurrentModuleObject +
						" object: Name cannot be blank" );
				}

				// we just need to loop over the existing vector elements to check for duplicates since we haven't add this one yet
				for ( auto & existingSingleBH : singleBoreholesVector ) {
					if ( DataIPShortCuts::cAlphaArgs( 1 ) == existingSingleBH->name ) {
						ShowFatalError( "Invalid input for " + DataIPShortCuts::cCurrentModuleObject +
							" object: Duplicate name found: " + existingSingleBH->name );
					}
				}

				// Build out new instance and add it to the vector
				std::shared_ptr < GLHEVertSingleStruct > thisArray( new GLHEVertSingleStruct );
				thisArray->name = DataIPShortCuts::cAlphaArgs( 1 );
				thisArray->props = GetVertProps( DataIPShortCuts::cAlphaArgs( 2 ) );
				thisArray->xLoc = DataIPShortCuts::rNumericArgs( 1 );
				thisArray->yLoc = DataIPShortCuts::rNumericArgs( 2 );

				singleBoreholesVector.push_back( thisArray );
			}
		}

		if ( numVerticalGLHEs > 0 ) {

			DataIPShortCuts::cCurrentModuleObject = "GroundHeatExchanger:System";

			for ( int GLHENum = 1; GLHENum <= numVerticalGLHEs; ++GLHENum ) {

				// just a few vars to pass in and out to GetObjectItem
				int ioStatus;
				int numAlphas;
				int numNumbers;

				// get the input data and store it in the Shortcuts structures
				InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, GLHENum,
											   DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs,
											   numNumbers, ioStatus, DataIPShortCuts::lNumericFieldBlanks,
											   DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames,
											   DataIPShortCuts::cNumericFieldNames );

				// the input processor validates the numeric inputs based on the IDD definition
				// still validate the name to make sure there aren't any duplicates or blanks
				// blanks are easy: fatal if blank
				if ( DataIPShortCuts::lAlphaFieldBlanks( 1 ) ) {
					ShowFatalError( "Invalid input for " + DataIPShortCuts::cCurrentModuleObject +
						" object: Name cannot be blank" );
				}

				// we just need to loop over the existing vector elements to check for duplicates since we haven't add this one yet
				for ( auto & existingVerticalGLHE : verticalGLHE ) {
					if ( DataIPShortCuts::cAlphaArgs( 1 ) == existingVerticalGLHE.name ) {
						ShowFatalError( "Invalid input for " + DataIPShortCuts::cCurrentModuleObject +
							" object: Duplicate name found: " + existingVerticalGLHE.name );
					}
				}

				// Build out new instance
				GLHEVert thisGLHE;
				thisGLHE.name = DataIPShortCuts::cAlphaArgs( 1 );

				// get inlet node num
				thisGLHE.inletNodeNum = GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 2 ), errorsFound, DataIPShortCuts::cCurrentModuleObject,
														   DataIPShortCuts::cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

				// get outlet node num
				thisGLHE.outletNodeNum = GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 3 ), errorsFound, DataIPShortCuts::cCurrentModuleObject,
															DataIPShortCuts::cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				thisGLHE.available = true;
				thisGLHE.on = true;

				TestCompSet( DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs( 1 ), DataIPShortCuts::cAlphaArgs( 2 ),
							 DataIPShortCuts::cAlphaArgs( 3 ), "Condenser Water Nodes" );

				thisGLHE.designFlow = DataIPShortCuts::rNumericArgs( 1 );
				RegisterPlantCompDesignFlow( thisGLHE.inletNodeNum, thisGLHE.designFlow );

				thisGLHE.soil.k = DataIPShortCuts::rNumericArgs( 2 );
				thisGLHE.soil.rhoCp = DataIPShortCuts::rNumericArgs( 3 );

				if ( !DataIPShortCuts::lAlphaFieldBlanks( 6 ) ) {
					// Response factors come from IDF object
					thisGLHE.myRespFactors = GetResponseFactor( DataIPShortCuts::cAlphaArgs( 6 ) );

					if ( !thisGLHE.myRespFactors ) {
						errorsFound = true;
						ShowSevereError( "GroundHeatExchanger:ResponseFactors object not found." );
					}

				} else if ( !DataIPShortCuts::lAlphaFieldBlanks( 7 ) ) {
					// Response factors come from eplusout.glhe or use array object to calculate them
					thisGLHE.myRespFactors = BuildAndGetResponseFactorObjectFromArray( GetVertArray( DataIPShortCuts::cAlphaArgs( 7 ) ) );

					if ( !thisGLHE.myRespFactors ) {
						errorsFound = true;
						ShowSevereError( "GroundHeatExchanger:Vertical:Array object not found." );
					}
				} else {
					// Calculate response factors from individual boreholes
					std::vector < std::shared_ptr < GLHEVertSingleStruct > > tempVectOfBHObjects;

					for ( int index = 8; index < DataIPShortCuts::cAlphaArgs.u1(); ++index ) {
						if ( !DataIPShortCuts::lAlphaFieldBlanks( index ) ) {
							std::shared_ptr < GLHEVertSingleStruct > tempBHptr = GetSingleBH( DataIPShortCuts::cAlphaArgs( index ) );
							if ( tempBHptr ) {
								tempVectOfBHObjects.push_back( tempBHptr );
							} else {
								errorsFound = true;
								ShowSevereError( "Borehole= " + DataIPShortCuts::cAlphaArgs( index ) + " not found." );
								break;
							}
						} else {
							break;
						}
					}

					thisGLHE.myRespFactors = BuildAndGetResponseFactorsObjectFromSingleBHs( tempVectOfBHObjects );

					if ( !thisGLHE.myRespFactors ) {
						errorsFound = true;
						ShowSevereError( "GroundHeatExchanger:Vertical:Single objects not found." );
					}
				}

				thisGLHE.bhDiameter = thisGLHE.myRespFactors->props->bhDiameter;
				thisGLHE.bhRadius = thisGLHE.bhDiameter / 2.0;
				thisGLHE.bhLength = thisGLHE.myRespFactors->props->bhLength;
				thisGLHE.bhUTubeDist = thisGLHE.myRespFactors->props->bhUTubeDist;
				thisGLHE.pipe.outDia = thisGLHE.myRespFactors->props->pipe.outDia;
				thisGLHE.pipe.thickness = thisGLHE.myRespFactors->props->pipe.thickness;
				thisGLHE.pipe.k = thisGLHE.myRespFactors->props->pipe.k;
				thisGLHE.grout.k = thisGLHE.myRespFactors->props->grout.k;

				// Number of simulation years from RunPeriod
				thisGLHE.myRespFactors->maxSimYears = MaxNumberSimYears;

				// total tube length
				thisGLHE.totalTubeLength = thisGLHE.myRespFactors->numBoreholes * thisGLHE.myRespFactors->props->bhLength;

				// ground thermal diffusivity
				thisGLHE.soil.diffusivity = thisGLHE.soil.k / thisGLHE.soil.rhoCp;

				thisGLHE.SubAGG = 15;
				thisGLHE.AGG = 192;

				// Allocation of all the dynamic arrays
				thisGLHE.QnMonthlyAgg.dimension( thisGLHE.myRespFactors->maxSimYears * 12, 0.0 );
				thisGLHE.QnHr.dimension( 730 + thisGLHE.AGG + thisGLHE.SubAGG, 0.0 );
				thisGLHE.QnSubHr.dimension( ( thisGLHE.SubAGG + 1 ) * maxTSinHr + 1, 0.0 );
				thisGLHE.LastHourN.dimension( thisGLHE.SubAGG + 1, 0 );

				prevTimeSteps.allocate( ( thisGLHE.SubAGG + 1 ) * maxTSinHr + 1 );
				prevTimeSteps = 0.0;

				// Initialize ground temperature model and get pointer reference
				thisGLHE.groundTempModel = GetGroundTempModelAndInit( DataIPShortCuts::cAlphaArgs( 4 ), DataIPShortCuts::cAlphaArgs( 5 ) );
				if ( thisGLHE.groundTempModel ) {
					errorsFound = thisGLHE.groundTempModel->errorsFound;
				}

				// Check for Errors
				if ( errorsFound ) {
					ShowFatalError( "Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject );
				}

				verticalGLHE.push_back( thisGLHE );
			}

			// Set up report variables
			for( int GLHENum = 0; GLHENum < numVerticalGLHEs; ++GLHENum ) {
				auto & thisGLHE( verticalGLHE[ GLHENum ] );
				SetupOutputVariable( "Ground Heat Exchanger Average Borehole Temperature", OutputProcessor::Unit::C, thisGLHE.bhTemp, "System", "Average", thisGLHE.name );
				SetupOutputVariable( "Ground Heat Exchanger Heat Transfer Rate", OutputProcessor::Unit::W, thisGLHE.QGLHE, "System", "Average", thisGLHE.name );
				SetupOutputVariable( "Ground Heat Exchanger Inlet Temperature", OutputProcessor::Unit::C, thisGLHE.inletTemp, "System", "Average", thisGLHE.name );
				SetupOutputVariable( "Ground Heat Exchanger Outlet Temperature", OutputProcessor::Unit::C, thisGLHE.outletTemp, "System", "Average", thisGLHE.name );
				SetupOutputVariable( "Ground Heat Exchanger Mass Flow Rate", OutputProcessor::Unit::kg_s, thisGLHE.massFlowRate, "System", "Average", thisGLHE.name );
				SetupOutputVariable( "Ground Heat Exchanger Average Fluid Temperature", OutputProcessor::Unit::C, thisGLHE.aveFluidTemp, "System", "Average", thisGLHE.name );
			}
		}

		// SLINKY GLHE

		if ( numSlinkyGLHEs > 0 ) {

			DataIPShortCuts::cCurrentModuleObject = "GroundHeatExchanger:Slinky";

			for ( int GLHENum = 1; GLHENum <= numSlinkyGLHEs; ++GLHENum ) {

				// just a few vars to pass in and out to GetObjectItem
				int ioStatus;
				int numAlphas;
				int numNumbers;

				// get the input data and store it in the Shortcuts structures
				InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, GLHENum,
											   DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs,
											   numNumbers, ioStatus, DataIPShortCuts::lNumericFieldBlanks,
											   DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames,
											   DataIPShortCuts::cNumericFieldNames );

				// the input processor validates the numeric inputs based on the IDD definition
				// still validate the name to make sure there aren't any duplicates or blanks
				// blanks are easy: fatal if blank
				if ( DataIPShortCuts::lAlphaFieldBlanks[0] ) {
					ShowFatalError( "Invalid input for " + DataIPShortCuts::cCurrentModuleObject +
						" object: Name cannot be blank" );
				}

				// we just need to loop over the existing vector elements to check for duplicates since we haven't add this one yet
				for ( auto &existingSlinkyGLHE : slinkyGLHE ) {
					if ( DataIPShortCuts::cAlphaArgs( 1 ) == existingSlinkyGLHE.name ) {
						ShowFatalError( "Invalid input for " + DataIPShortCuts::cCurrentModuleObject +
							" object: Duplicate name found: " + existingSlinkyGLHE.name );
					}
				}

				// Build out new instance
				GLHESlinky thisGLHE;
				thisGLHE.name = DataIPShortCuts::cAlphaArgs( 1 );

				// get inlet node num
				thisGLHE.inletNodeNum = GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 2 ), errorsFound, DataIPShortCuts::cCurrentModuleObject,
														   DataIPShortCuts::cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

				// get outlet node num
				thisGLHE.outletNodeNum = GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 3 ), errorsFound, DataIPShortCuts::cCurrentModuleObject,
															DataIPShortCuts::cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				thisGLHE.available = true;
				thisGLHE.on = true;

				TestCompSet( DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs( 1 ), DataIPShortCuts::cAlphaArgs( 2 ),
							 DataIPShortCuts::cAlphaArgs( 3 ), "Condenser Water Nodes" );

				// load data
				thisGLHE.designFlow = DataIPShortCuts::rNumericArgs( 1 );
				RegisterPlantCompDesignFlow( thisGLHE.inletNodeNum, thisGLHE.designFlow );

				thisGLHE.soil.k = DataIPShortCuts::rNumericArgs( 2 );
				thisGLHE.soil.rhoCp = DataIPShortCuts::rNumericArgs( 3 ) * DataIPShortCuts::rNumericArgs( 4 );
				thisGLHE.pipe.k = DataIPShortCuts::rNumericArgs( 5 );
				thisGLHE.pipe.rho = DataIPShortCuts::rNumericArgs( 6 );
				thisGLHE.pipe.cp = DataIPShortCuts::rNumericArgs( 7 );
				thisGLHE.pipe.outDia = DataIPShortCuts::rNumericArgs( 8 );
				thisGLHE.pipe.thickness = DataIPShortCuts::rNumericArgs( 9 );

				if ( SameString( DataIPShortCuts::cAlphaArgs( 4 ), "VERTICAL" ) ) {
					thisGLHE.verticalConfig = true;
				} else if ( SameString( DataIPShortCuts::cAlphaArgs( 4 ), "HORIZONTAL" ) ) {
					thisGLHE.verticalConfig = false;
				}

				thisGLHE.coilDiameter = DataIPShortCuts::rNumericArgs( 10 );
				thisGLHE.coilPitch = DataIPShortCuts::rNumericArgs( 11 );
				thisGLHE.trenchDepth = DataIPShortCuts::rNumericArgs( 12 );
				thisGLHE.trenchLength = DataIPShortCuts::rNumericArgs( 13 );
				thisGLHE.numTrenches = DataIPShortCuts::rNumericArgs( 14 );
				thisGLHE.trenchSpacing = DataIPShortCuts::rNumericArgs( 15 );
				thisGLHE.maxSimYears = DataIPShortCuts::rNumericArgs( 16 );

				// Need to add a response factor object for the slinky model
				std::shared_ptr < GLHEResponseFactorsStruct > thisRF( new GLHEResponseFactorsStruct );
				thisRF->name = "Response Factor Object Auto Generated No: " + std::to_string( numAutoGeneratedResponseFactors + 1 );
				thisGLHE.myRespFactors = thisRF;
				responseFactorsVector.push_back( thisRF );

				// Number of coils
				thisGLHE.numCoils = thisGLHE.trenchLength / thisGLHE.coilPitch;

				// Total tube length
				thisGLHE.totalTubeLength = Pi * thisGLHE.coilDiameter * thisGLHE.trenchLength * thisGLHE. numTrenches / thisGLHE. coilPitch;

				// Get g function data
				thisGLHE.SubAGG = 15;
				thisGLHE.AGG = 192;

				// Average coil depth
				if ( thisGLHE.verticalConfig ) {
					// Vertical configuration
					if ( thisGLHE.trenchDepth - thisGLHE.coilDiameter < 0.0 ) {
						// Error: part of the coil is above ground
						ShowSevereError( DataIPShortCuts::cCurrentModuleObject + "=\"" + thisGLHE.name + "\", invalid value in field." );
						ShowContinueError( "..." + DataIPShortCuts::cNumericFieldNames( 13 ) + "=[" + RoundSigDigits( thisGLHE.trenchDepth, 3 ) + "]." );
						ShowContinueError( "..." + DataIPShortCuts::cNumericFieldNames( 10 ) + "=[" + RoundSigDigits( thisGLHE.coilDepth, 3 ) + "]." );
						ShowContinueError( "...Average coil depth will be <=0." );
						errorsFound = true;

					} else {
						// Entire coil is below ground
						thisGLHE.coilDepth = thisGLHE.trenchDepth - ( thisGLHE.coilDiameter / 2.0 );
					}

				} else {
					// Horizontal configuration
					thisGLHE. coilDepth = thisGLHE.trenchDepth;
				}

				// Thermal diffusivity of the ground
				thisGLHE.soil.diffusivity = thisGLHE.soil.k / thisGLHE.soil.rhoCp;

				prevTimeSteps.allocate( ( thisGLHE.SubAGG + 1 ) * maxTSinHr + 1 );
				prevTimeSteps = 0.0;

				if ( thisGLHE.pipe.thickness >= thisGLHE.pipe.outDia / 2.0 ) {
					ShowSevereError( DataIPShortCuts::cCurrentModuleObject + "=\"" + thisGLHE.name + "\", invalid value in field." );
					ShowContinueError( "..." + DataIPShortCuts::cNumericFieldNames( 12 ) + "=[" + RoundSigDigits( thisGLHE.pipe.thickness, 3 ) + "]." );
					ShowContinueError( "..." + DataIPShortCuts::cNumericFieldNames( 10 ) + "=[" + RoundSigDigits( thisGLHE.pipe.outDia, 3 ) + "]." );
					ShowContinueError( "...Radius will be <=0." );
					errorsFound = true;
				}

				// Initialize ground temperature model and get pointer reference
				thisGLHE.groundTempModel = GetGroundTempModelAndInit( DataIPShortCuts::cAlphaArgs( 5 ) , DataIPShortCuts::cAlphaArgs( 6 ) );
				if ( thisGLHE.groundTempModel ) {
					errorsFound = thisGLHE.groundTempModel->errorsFound;
				}

				// Check for Errors
				if ( errorsFound ) {
					ShowFatalError( "Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject );
				}

				slinkyGLHE.push_back( thisGLHE );

			}

			// Set up report variables
			for ( int GLHENum = 0; GLHENum < numSlinkyGLHEs; ++GLHENum ) {
				auto & thisGLHE( slinkyGLHE[ GLHENum ] );
				SetupOutputVariable( "Ground Heat Exchanger Average Borehole Temperature", OutputProcessor::Unit::C, thisGLHE.bhTemp, "System", "Average", thisGLHE.name );
				SetupOutputVariable( "Ground Heat Exchanger Heat Transfer Rate", OutputProcessor::Unit::W, thisGLHE.QGLHE, "System", "Average", thisGLHE.name );
				SetupOutputVariable( "Ground Heat Exchanger Inlet Temperature", OutputProcessor::Unit::C, thisGLHE.inletTemp, "System", "Average", thisGLHE.name );
				SetupOutputVariable( "Ground Heat Exchanger Outlet Temperature", OutputProcessor::Unit::C, thisGLHE.outletTemp, "System", "Average", thisGLHE.name );
				SetupOutputVariable( "Ground Heat Exchanger Mass Flow Rate", OutputProcessor::Unit::kg_s, thisGLHE.massFlowRate, "System", "Average", thisGLHE.name );
				SetupOutputVariable( "Ground Heat Exchanger Average Fluid Temperature", OutputProcessor::Unit::C, thisGLHE.aveFluidTemp, "System", "Average", thisGLHE.name );
			}
		}

	}

	//******************************************************************************

	void
	GLHEVert::calcHXResistance()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Cenk Yavuzturk
		//       DATE WRITTEN   1998
		//       MODIFIED       August, 2000
		//       RE-ENGINEERED Dan Fisher

		// PURPOSE OF THIS SUBROUTINE:
		//    Calculates the resistance of a vertical borehole
		//    with a U-tube inserted into it.

		// METHODOLOGY EMPLOYED:

		//  REFERENCE:          Thermal Analysis of Heat Extraction
		//                      Boreholes.  Per Eskilson, Dept. of
		//                      Mathematical Physics, University of
		//                      Lund, Sweden, June 1987.
		// USE STATEMENTS: na
		// Using/Aliasing
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetViscosityGlycol;
		using FluidProperties::GetConductivityGlycol;
		using DataPlant::PlantLoop;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcVerticalGroundHeatExchanger" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 cpFluid;
		Real64 kFluid;
		Real64 fluidDensity;
		Real64 fluidViscosity;
		Real64 pipeInnerDia;
		Real64 BholeMdot;
		Real64 pipeOuterRad;
		Real64 pipeInnerRad;
		Real64 nusseltNum;
		Real64 reynoldsNum;
		Real64 prandtlNum;
		Real64 hci;
		Real64 Rcond;
		Real64 Rconv;
		Real64 Rgrout;
		Real64 B0; // grout resistance curve fit coefficients
		Real64 B1;
		Real64 maxDistance;
		Real64 distanceRatio;
		Real64 smoothingFunction;
		Real64 A( 3150 );
		Real64 B( 350 );
		Real64 laminarNusseltNo( 4.364 );
		Real64 turbulentNusseltNo;

		cpFluid = GetSpecificHeatGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
		kFluid = GetConductivityGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
		fluidDensity = GetDensityGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
		fluidViscosity = GetViscosityGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );

		//calculate mass flow rate
		BholeMdot = massFlowRate / myRespFactors->numBoreholes;

		pipeOuterRad = pipe.outDia / 2.0;
		pipeInnerRad = pipeOuterRad - pipe.thickness;
		pipeInnerDia = 2.0 * pipeInnerRad;

		if ( BholeMdot == 0.0 ) {
			Rconv = 0.0;
		} else {
			//Re=Rho*V*D/Mu
			reynoldsNum = fluidDensity * pipeInnerDia * ( BholeMdot / fluidDensity / ( Pi * pow_2( pipeInnerRad ) ) ) / fluidViscosity;
			prandtlNum = ( cpFluid * fluidViscosity ) / ( kFluid );
			//   Convection Resistance
			if ( reynoldsNum <= 2300 ) {
				nusseltNum = laminarNusseltNo;
			} else if ( reynoldsNum > 2300 && reynoldsNum <= 4000 ) {
				smoothingFunction = 0.5 + 0.5 * std::tanh( ( reynoldsNum - A ) / B );
				turbulentNusseltNo = 0.023 * std::pow( reynoldsNum, 0.8 ) * std::pow( prandtlNum, 0.35 );
				nusseltNum = laminarNusseltNo * ( 1 - smoothingFunction ) + turbulentNusseltNo * smoothingFunction;
			} else {
				nusseltNum = 0.023 * std::pow( reynoldsNum, 0.8 ) * std::pow( prandtlNum, 0.35 );
			}
			hci = nusseltNum * kFluid / pipeInnerDia;
			Rconv = 1.0 / ( 2.0 * Pi * pipeInnerDia * hci );
		}

		//   Conduction Resistance
		Rcond = std::log( pipeOuterRad / pipeInnerRad ) / ( 2.0 * Pi * pipe.k ) / 2.0; // pipe in parallel so /2

		//   Resistance Due to the grout.
		maxDistance = 2.0 * bhRadius - ( 2.0 * pipe.outDia );
		distanceRatio = bhUTubeDist / maxDistance;

		if ( distanceRatio >= 0.0 && distanceRatio <= 0.25 ) {
			B0 = 14.450872;
			B1 = -0.8176;
		} else if ( distanceRatio > 0.25 && distanceRatio < 0.5 ) {
			B0 = 20.100377;
			B1 = -0.94467;
		} else if ( distanceRatio >= 0.5 && distanceRatio <= 0.75 ) {
			B0 = 17.44268;
			B1 = -0.605154;
		} else {
			B0 = 21.90587;
			B1 = -0.3796;
		}

		Rgrout = 1.0 / ( grout.k * ( B0 * std::pow( bhRadius / pipeOuterRad, B1 ) ) );
		HXResistance = Rcond + Rconv + Rgrout;
	}

	//******************************************************************************

	void
	GLHESlinky::calcHXResistance()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   February, 2015
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		//    Calculates the resistance of the slinky HX from the fluid to the
		//	  outer tube wall.

		// Using/Aliasing
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetViscosityGlycol;
		using FluidProperties::GetConductivityGlycol;
		using DataPlant::PlantLoop;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcSlinkyGroundHeatExchanger" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 cpFluid;
		Real64 kFluid;
		Real64 fluidDensity;
		Real64 fluidViscosity;
		Real64 pipeInnerDia;
		Real64 singleSlinkyMassFlowRate;
		Real64 pipeOuterRad;
		Real64 pipeInnerRad;
		Real64 nusseltNum;
		Real64 reynoldsNum;
		Real64 prandtlNum;
		Real64 hci;
		Real64 Rcond;
		Real64 Rconv;
		Real64 smoothingFunction;
		Real64 A( 3150 );
		Real64 B( 350 );
		Real64 laminarNusseltNo( 4.364 );
		Real64 turbulentNusseltNo;

		cpFluid = GetSpecificHeatGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
		kFluid = GetConductivityGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
		fluidDensity = GetDensityGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
		fluidViscosity = GetViscosityGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );

		//calculate mass flow rate
		singleSlinkyMassFlowRate = massFlowRate / numTrenches;

		pipeOuterRad = pipe.outDia / 2.0;
		pipeInnerRad = pipeOuterRad - pipe.thickness;
		pipeInnerDia = 2.0 * pipeInnerRad;

		if ( singleSlinkyMassFlowRate == 0.0 ) {
			Rconv = 0.0;
		} else {
			//Re=Rho*V*D/Mu
			reynoldsNum = fluidDensity * pipeInnerDia * ( singleSlinkyMassFlowRate / fluidDensity / ( Pi * pow_2( pipeInnerRad ) ) ) / fluidViscosity;
			prandtlNum = ( cpFluid * fluidViscosity ) / ( kFluid );
			//   Convection Resistance
			if ( reynoldsNum <= 2300 ) {
				nusseltNum = laminarNusseltNo;
			} else if ( reynoldsNum > 2300 && reynoldsNum <= 4000 ) {
				smoothingFunction = 0.5 + 0.5 * std::tanh( ( reynoldsNum - A ) / B );
				turbulentNusseltNo = 0.023 * std::pow( reynoldsNum, 0.8 ) * std::pow( prandtlNum, 0.35 );
				nusseltNum = laminarNusseltNo * ( 1 - smoothingFunction ) + turbulentNusseltNo * smoothingFunction;
			} else {
				nusseltNum = 0.023 * std::pow( reynoldsNum, 0.8 ) * std::pow( prandtlNum, 0.35 );
			}
			hci = nusseltNum * kFluid / pipeInnerDia;
			Rconv = 1.0 / ( 2.0 * Pi * pipeInnerDia * hci );
		}

		//   Conduction Resistance
		Rcond = std::log( pipeOuterRad / pipeInnerRad ) / ( 2.0 * Pi * pipe.k ) / 2.0; // pipe in parallel so /2

		HXResistance = Rcond + Rconv;
	}

	//******************************************************************************

	Real64
	GLHEBase::interpGFunc(
		Real64 const LnTTsVal // The value of LN(t/TimeSS) that a g-function
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Chris L. Marshall, Jeffrey D. Spitler
		//       DATE WRITTEN   1993
		//       MODIFIED       August, 2000
		//       RE-ENGINEERED Dan Fisher

		// PURPOSE OF THIS SUBROUTINE:
		//    To interpolate or extrapolate data in GFILE
		//    to find the correct g-function value for a
		//    known value of the natural log of (T/Ts)

		// METHODOLOGY EMPLOYED:

		//  REFERENCE:          Thermal Analysis of Heat Extraction
		//                      Boreholes.  Per Eskilson, Dept. of
		//                      Mathematical Physics, University of
		//                      Lund, Sweden, June 1987.
		// USE STATEMENTS: na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//          needs to be found for.
		//          either extrapolation or interpolation
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 gFuncVal;

		//Binary Search Algorithms Variables
		// REFERENCE      :  DATA STRUCTURES AND ALGORITHM ANALYSIS IN C BY MARK ALLEN WEISS
		int Mid;
		int Low;
		int High;
		bool Found;

		// The following IF loop determines the g-function for the case
		// when LnTTsVal is less than the first element of the LnTTs array.
		// In this case, the g-function must be found by extrapolation.

		if ( LnTTsVal <= myRespFactors->LNTTS( 1 ) ) {
			gFuncVal = ( ( LnTTsVal - myRespFactors->LNTTS( 1 ) ) / ( myRespFactors->LNTTS( 2 ) - myRespFactors->LNTTS( 1 ) ) ) * ( myRespFactors->GFNC( 2 ) - myRespFactors->GFNC( 1 ) ) + myRespFactors->GFNC( 1 );
			return gFuncVal;
		}

		// The following IF loop determines the g-function for the case
		// when LnTTsVal is greater than the last element of the LnTTs array.
		// In this case, the g-function must be found by extrapolation.

		int NPairs = myRespFactors->LNTTS.u1();

		if ( LnTTsVal > myRespFactors->LNTTS( NPairs ) ) {
			gFuncVal = ( ( LnTTsVal - myRespFactors->LNTTS( NPairs ) ) / ( myRespFactors->LNTTS( NPairs - 1 ) - myRespFactors->LNTTS( NPairs ) ) ) * ( myRespFactors->GFNC( NPairs - 1 ) - myRespFactors->GFNC( NPairs ) ) + myRespFactors->GFNC( NPairs );
			return gFuncVal;
		}

		// The following DO loop is for the case when LnTTsVal falls within
		// the first and last elements of the LnTTs array, or is identically
		// equal to one of the LnTTs elements.  In this case the g-function
		// must be found by interpolation.
		// USING BINARY SEARCH TO FIND THE ELEMENET
		Found = false;
		Low = 1;
		High = NPairs;
		while ( Low <= High ) {
			Mid = ( Low + High ) / 2;
			if ( myRespFactors->LNTTS( Mid ) < LnTTsVal ) {
				Low = Mid + 1;
			} else {
				if ( myRespFactors->LNTTS( Mid ) > LnTTsVal ) {
					High = Mid - 1;
				} else {
					Found = true;
					break;
				}
			}
		}
		//LnTTsVal is identical to one of the LnTTS array elements return gFuncVal
		//the gFuncVal after applying the correction
		if ( Found ) {
			gFuncVal = myRespFactors->GFNC( Mid );
			return gFuncVal;
		}

		//LnTTsVal is in between any of the two LnTTS array elements find the
		// g-function value by interpolation and apply the correction and return gFuncVal
		else {
			if ( myRespFactors->LNTTS( Mid ) < LnTTsVal ) ++Mid;

			gFuncVal = ( ( LnTTsVal - myRespFactors->LNTTS( Mid ) ) / ( myRespFactors->LNTTS( Mid - 1 ) - myRespFactors->LNTTS( Mid ) ) ) * ( myRespFactors->GFNC( Mid - 1 ) - myRespFactors->GFNC( Mid ) ) + myRespFactors->GFNC( Mid );

			return gFuncVal;
		}
	}

	//******************************************************************************

	Real64
	GLHESlinky::getGFunc(
		Real64 const time
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Matt Mitchell
		//       DATE WRITTEN:    February, 2015
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the g-function for slinky GHXs
		// Note: Base 10 here.

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 LNTTS;

		LNTTS = std::log10( time );

		return interpGFunc( LNTTS );
	}

	//******************************************************************************

	Real64
	GLHEVert::getGFunc(
		Real64 const time
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Matt Mitchell
		//       DATE WRITTEN:    February, 2015
		//       MODIFIED         na
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the g-function for vertical GHXs
		// Note: Base e here.

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 RATIO;
		Real64 gFuncVal;
		Real64 LNTTS;

		LNTTS = std::log( time );

		gFuncVal = interpGFunc( LNTTS );

		RATIO = bhRadius / bhLength;

		if ( RATIO != myRespFactors->gRefRatio ) {
			gFuncVal -= std::log( bhRadius / ( bhLength * myRespFactors->gRefRatio ) );
		}

		return gFuncVal;
	}

	//******************************************************************************

	void
	GLHEVert::initGLHESimVars()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    August, 2000
		//       MODIFIED         Arun Murugappan
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::RegulateCondenserCompFlowReqOp;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_GrndHtExchgSystem;
		using DataPlant::ScanPlantLoopsForObject;
		using FluidProperties::GetDensityGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "initGLHESimVars" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 fluidDensity;
		bool errFlag;

		Real64 currTime = ( ( DayOfSim - 1 ) * 24 + ( HourOfDay - 1 ) + ( TimeStep - 1 ) * TimeStepZone + SysTimeElapsed ) * SecInHour;

		// Init more variables
		if ( myFlag ) {
			// Locate the hx on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( name, TypeOf_GrndHtExchgSystem, loopNum, loopSideNum, branchNum, compNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "initGLHESimVars: Program terminated due to previous condition(s)." );
			}
			myFlag = false;
		}

		if ( myEnvrnFlag && BeginEnvrnFlag ) {

			myEnvrnFlag = false;

			fluidDensity = GetDensityGlycol( PlantLoop( loopNum ).FluidName, 20.0, PlantLoop( loopNum ).FluidIndex, RoutineName );
			designMassFlow = designFlow * fluidDensity;
			InitComponentNodes( 0.0, designMassFlow, inletNodeNum, outletNodeNum, loopNum, loopSideNum, branchNum, compNum );

			lastQnSubHr = 0.0;
			Node( inletNodeNum ).Temp = tempGround;
			Node( outletNodeNum ).Temp = tempGround;

			// zero out all history arrays

			QnHr = 0.0;
			QnMonthlyAgg = 0.0;
			QnSubHr = 0.0;
			LastHourN = 0;
			prevTimeSteps = 0.0;
			currentSimTime = 0.0;
			QGLHE = 0.0;
			prevHour = 1;
		}

		// Calculate the average ground temperature over the depth of the borehole

		Real64 minDepth = myRespFactors->props->bhTopDepth;
		Real64 maxDepth = myRespFactors->props->bhLength + minDepth;
		Real64 oneQuarterDepth = minDepth + ( maxDepth - minDepth ) * 0.25;
		Real64 halfDepth = minDepth + ( maxDepth - minDepth ) * 0.5;
		Real64 threeQuarterDepth = minDepth + ( maxDepth - minDepth ) * 0.75;

		tempGround = 0;

		tempGround += this->groundTempModel->getGroundTempAtTimeInSeconds( minDepth, currTime );
		tempGround += this->groundTempModel->getGroundTempAtTimeInSeconds( maxDepth, currTime );
		tempGround += this->groundTempModel->getGroundTempAtTimeInSeconds( oneQuarterDepth, currTime );
		tempGround += this->groundTempModel->getGroundTempAtTimeInSeconds( halfDepth, currTime );
		tempGround += this->groundTempModel->getGroundTempAtTimeInSeconds( threeQuarterDepth, currTime );

		tempGround /= 5;

		massFlowRate = RegulateCondenserCompFlowReqOp( loopNum, loopSideNum, branchNum, compNum, designMassFlow );

		SetComponentFlowRate( massFlowRate, inletNodeNum, outletNodeNum, loopNum, loopSideNum, branchNum, compNum );

		// Reset local environment init flag
		if ( ! BeginEnvrnFlag ) myEnvrnFlag = true;
	}

	//******************************************************************************

	void
	GLHESlinky::initGLHESimVars()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    August, 2000
		//       MODIFIED         Arun Murugappan
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine needs a description.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::RegulateCondenserCompFlowReqOp;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_GrndHtExchgSlinky;
		using DataPlant::ScanPlantLoopsForObject;
		using FluidProperties::GetDensityGlycol;
		using namespace GroundTemperatureManager;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "initGLHESimVars" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 fluidDensity;
		bool errFlag;
		Real64 CurTime;

		CurTime = ( ( DayOfSim - 1 ) * 24 + ( HourOfDay - 1 ) + ( TimeStep - 1 ) * TimeStepZone + SysTimeElapsed ) * SecInHour;

		// Init more variables
		if ( myFlag ) {
			// Locate the hx on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( name, TypeOf_GrndHtExchgSlinky, loopNum, loopSideNum, branchNum, compNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "initGLHESimVars: Program terminated due to previous condition(s)." );
			}
			myFlag = false;
		}

		if ( myEnvrnFlag && BeginEnvrnFlag ) {

			myEnvrnFlag = false;

			fluidDensity = GetDensityGlycol( PlantLoop( loopNum ).FluidName, 20.0, PlantLoop( loopNum ).FluidIndex, RoutineName );
			designMassFlow = designFlow * fluidDensity;
			InitComponentNodes( 0.0, designMassFlow, inletNodeNum, outletNodeNum, loopNum, loopSideNum, branchNum, compNum );

			lastQnSubHr = 0.0;
			Node( inletNodeNum ).Temp = this->groundTempModel->getGroundTempAtTimeInSeconds( coilDepth, CurTime );
			Node( outletNodeNum ).Temp = this->groundTempModel->getGroundTempAtTimeInSeconds( coilDepth, CurTime );

			// zero out all history arrays

			QnHr = 0.0;
			QnMonthlyAgg = 0.0;
			QnSubHr = 0.0;
			LastHourN = 0;
			prevTimeSteps = 0.0;
			currentSimTime = 0.0;
			QGLHE = 0.0;
			prevHour = 1;
		}

		tempGround = this->groundTempModel->getGroundTempAtTimeInSeconds( coilDepth, CurTime );

		massFlowRate = RegulateCondenserCompFlowReqOp( loopNum, loopSideNum, branchNum, compNum, designMassFlow );

		SetComponentFlowRate( massFlowRate, inletNodeNum, outletNodeNum, loopNum, loopSideNum, branchNum, compNum );

		// Reset local environment init flag
		if ( ! BeginEnvrnFlag ) myEnvrnFlag = true;

	}

	//******************************************************************************

} // GroundHeatExchangers

} // EnergyPlus
