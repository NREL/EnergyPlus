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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

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
	// To aid computational efficiency past pulses are continuously agregated into
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
	using DataGlobals::InitConvTemp;
	using DataGlobals::WarmupFlag;
	using DataGlobals::SecInHour;
	using DataHVACGlobals::TimeStepSys;
	using DataHVACGlobals::SysTimeElapsed;
	using namespace DataLoopNode;
	using General::TrimSigDigits;
	using namespace GroundTemperatureManager;

	// Data
	// DERIVED TYPE DEFINITIONS

	// MODULE PARAMETER DEFINITIONS
	Real64 const hrsPerDay( 24.0 ); // Number of hours in a day
	Real64 const hrsPerMonth( 730.0 ); // Number of hours in month
	int const maxTSinHr( 60 ); // Max number of time step in a hour

	// MODULE VARIABLE DECLARATIONS:
	int numVerticalGLHEs( 0 );
	int numSlinkyGLHEs( 0 );
	int N( 1 ); // COUNTER OF TIME STEP
	Real64 currentSimTime( 0.0 ); // Current simulation time in hours
	int locHourOfDay( 0 );
	int locDayOfSim( 0 );
	namespace {
		bool GetInput( true );
		bool errorsFound( false );
	}

	Array1D< Real64 > prevTimeSteps; // This is used to store only the Last Few time step's time
	// to enable the calculation of the subhouly contribution..
	// Recommended size, the product of Minimum subhourly history required and
	// the maximum no of system time steps in an hour

	Array1D_bool checkEquipName;

	// Object Data
	Array1D< GLHEVert > verticalGLHE;
	Array1D< GLHESlinky > slinkyGLHE;

	// MODULE SUBROUTINES:

	//******************************************************************************

	// Functions
	void
	clear_state(){
		numVerticalGLHEs = 0;
		numSlinkyGLHEs = 0;
		N = 1; // COUNTER OF TIME STEP
		currentSimTime = 0.0 ; // Current simulation time in hours
		locHourOfDay = 0;
		locDayOfSim = 0;
		GetInput = true;
		errorsFound = false;
		prevTimeSteps.deallocate();
		checkEquipName.deallocate();
		verticalGLHE.deallocate();
		slinkyGLHE.deallocate();
	}


	void GLHEBase::onInitLoopEquip( const PlantLocation & EP_UNUSED( calledFromLocation ) ) {
		this->initGLHESimVars();
	}

	void GLHEBase::simulate( const PlantLocation & EP_UNUSED(calledFromLocation), bool const EP_UNUSED(FirstHVACIteration), Real64 & EP_UNUSED( CurLoad ), bool const EP_UNUSED( RunFlag ) ) {
		this->initGLHESimVars();
		this->calcGroundHeatExchanger();
		this->updateGHX();
	}

	PlantComponent * GLHEBase::factory( int const objectType, std::string objectName ) {
		if ( GetInput ) {
			GetGroundHeatExchangerInput();
			GetInput = false;
		}
		if ( objectType == DataPlant::TypeOf_GrndHtExchgVertical ) {
			for ( auto & ghx : verticalGLHE ) {
				if ( ghx.Name == objectName ) {
					return &ghx;
				}
			}
		} else if ( objectType == DataPlant::TypeOf_GrndHtExchgSlinky ) {
			for ( auto & ghx : slinkyGLHE ) {
				if ( ghx.Name == objectName ) {
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

	void
	GLHEVert::calcGFunctions()
	{
		// Nothing to see here. Move along.
		// Just a stub out for future work.
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

		X0.allocate( numCoils );
		Y0.allocate( numTrenches );

		// Calculate the number of g-functions required
		tLg_max = std::log10( maxSimYears * convertYearsToSeconds / ts );
		NPairs = ( tLg_max - tLg_min ) / ( tLg_grid ) + 1;

		// Allocate and setup g-function arrays
		GFNC.allocate( NPairs );
		LNTTS.allocate( NPairs );
		QnMonthlyAgg.allocate( maxSimYears * 12 );
		QnHr.allocate( 730 + AGG + SubAGG );
		QnSubHr.allocate( ( SubAGG + 1 ) * maxTSinHr + 1 );
		LastHourN.allocate( SubAGG + 1 );

		for ( i = 1; i <= NPairs; ++i ) {
			GFNC( i ) = 0.0;
			LNTTS( i ) = 0.0;
		}

		// Calculate the number of loops (per trench) and number of trenchs to be involved
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

			// Set the average temperature resonse of the whole field to zero
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
							// then we nee some extra effort in calculating the double integral
							if ( m1 == m && n1 == n) {
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
							} else if ( disRing > (10 + coilDiameter ) ) {
								gFuncin = 0;

							// else the ring(n1, m1) is in the middle-field of the ring(n,m)
							} else {
								// if no calculated value have been stored
								if ( valStored( mm1, nn1 ) < 0.0 ) {
									midFieldVal = midFieldResponseFunction( m, n, m1, n1, t );
									valStored( mm1, nn1 ) = midFieldVal;
								// if a stored value is found for the comination of (m, n, m1, n1), then
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

			GFNC( NT ) = ( gFunc * ( coilDiameter / 2.0 ) ) / ( 4 * Pi	* fraction * numTrenches * numCoils );
			LNTTS( NT ) = tLg;

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

		distance1 = distance( m, n, m1, n1, eta, theta);

		sqrtAlphaT = std::sqrt( diffusivityGround * t );

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

		sqrtAlphaT = std::sqrt( diffusivityGround * t );

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

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 x;
		Real64 y;
		Real64 z;
		Real64 xIn;
		Real64 yIn;
		Real64 zIn;
		Real64 xOut;
		Real64 yOut;
		Real64 zOut;
		Real64 pipeOuterRadius;

		pipeOuterRadius = pipeOutDia / 2.0;

		x = X0( n ) + std::cos( theta ) * ( coilDiameter / 2.0 );
		y = Y0( m ) + std::sin( theta ) * ( coilDiameter / 2.0 );

		xIn = X0( n1 ) + std::cos( eta ) * ( coilDiameter / 2.0 - pipeOuterRadius );
		yIn = Y0( m1 ) + std::sin( eta ) * ( coilDiameter / 2.0 - pipeOuterRadius );

		xOut = X0( n1 ) + std::cos( eta ) * ( coilDiameter / 2.0 + pipeOuterRadius );
		yOut = Y0( m1 ) + std::sin( eta ) * ( coilDiameter / 2.0 + pipeOuterRadius );

		if ( ! verticalConfig ) {

			return 0.5 * std::sqrt( pow_2( x - xIn ) + pow_2( y - yIn ) )
				+ 0.5 * std::sqrt( pow_2( x - xOut ) + pow_2( y - yOut ) );

		} else {

			z = Z0 + std::sin( theta ) * ( coilDiameter / 2.0 );

			zIn = Z0 + std::sin( eta ) * ( coilDiameter / 2.0 - pipeOuterRadius );
			zOut = Z0 + std::sin( eta ) * ( coilDiameter / 2.0 + pipeOuterRadius );

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

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 x;
		Real64 y;
		Real64 z;
		Real64 xIn;
		Real64 yIn;
		Real64 zIn;
		Real64 xOut;
		Real64 yOut;
		Real64 zOut;
		Real64 pipeOuterRadius;

		pipeOuterRadius = pipeOutDia / 2.0;

		x = X0( n ) + std::cos( theta ) * ( coilDiameter / 2.0 );
		y = Y0( m ) + std::sin( theta ) * ( coilDiameter / 2.0 );
		z = Z0 + std::sin( theta ) * ( coilDiameter / 2.0 ) + 2 * coilDepth;

		xIn = X0( n1 ) + std::cos( eta ) * ( coilDiameter / 2.0 - pipeOuterRadius );
		yIn = Y0( m1 ) + std::sin( eta ) * ( coilDiameter / 2.0 - pipeOuterRadius );
		zIn = Z0 + std::sin( eta ) * ( coilDiameter / 2.0 - pipeOuterRadius );

		xOut = X0( n1 ) + std::cos( eta ) * ( coilDiameter / 2.0 + pipeOuterRadius );
		yOut = Y0( m1 ) + std::sin( eta ) * ( coilDiameter / 2.0 + pipeOuterRadius );
		zOut = Z0 + std::sin( eta ) * ( coilDiameter / 2.0 + pipeOuterRadius );

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

	bool
	GLHESlinky::isEven(
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

		timeSS = ( pow_2( boreholeLength ) / ( 9.0 * diffusivityGround ) ) / SecInHour / 8760.0;
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
		// To aid computational efficiency past pulses are continuously agregated into
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
		int currentMonth; // The Month upto which the Montly blocks are superposed
		Real64 sumQnMonthly; // tmp variable which holds the sum of the Temperature diffrence
		// due to Aggregated heat extraction/rejection step
		Real64 sumQnHourly; // same as above for hourly
		Real64 sumQnSubHourly; // same as above for subhourly( with no aggreation]
		Real64 RQMonth;
		Real64 RQHour;
		Real64 RQSubHr;
		int I;
		Real64 tmpQnSubHourly; // current Qn subhourly value
		int hourlyLimit; // number of hours to be taken into account in superposition
		int subHourlyLimit; // number of subhourlys to be taken into account in subhourly superposition
		Real64 sumTotal( 0.0 ); // sum of all the Qn (load) blocks
		Real64 C0; // **Intermediate constants used
		Real64 C1; // **Intermediate constants used
		Real64 C2; // **in explicit  calcualtion of the
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

		kGroundFactor = 2.0 * Pi * kGround;

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

		if ( ! WarmupFlag ) {
			triggerDesignDayReset = true;
		}

		if ( currentSimTime <= 0.0 ) {
			prevTimeSteps = 0.0; // this resets history when rounding 24:00 hours during warmup avoids hard crash later
			calcAggregateLoad(); //Just allocates and initializes prevHour array
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

				//Calulate the subhourly temperature due the Last Time steps Load
				gFuncVal = getGFunc( ( currentSimTime - prevTimeSteps( 2 ) ) / ( timeSSFactor ) );
				RQSubHr = gFuncVal / ( kGroundFactor );

				if ( massFlowRate <= 0.0 ) {
					tmpQnSubHourly = 0.0;
					fluidAveTemp = tempGround - sumTotal; // Q(N)*RB = 0
					ToutNew = inletTemp;
				} else {
					//Dr.Spitler's Explicit set of equations to calculate the New Outlet Temperature of the U-Tube
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

				// Subhourly Superposition
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

				// Calulate the subhourly temperature due the Last Time steps Load

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
		boreholeTemp = tempGround - sumTotal;
		//Load the QnSubHourly Array with a new value at end of every timestep

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
			ShowContinueError( "For GroundHeatExchanger: " + Name + "GLHE delta Temp > 100C." );
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
		// To aid computational efficiency past pulses are continuously agregated into
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
		Real64 SumQnMonth; // intermediate variable to store the Montly heat rejection/
		Real64 SumQnHr;
		int MonthNum;
		int J; // Loop counter

		if ( currentSimTime <= 0.0 ) return;

		//FOR EVERY HOUR UPDATE THE HOURLY QN QnHr(J)
		//THIS IS DONE BY AGGREGATING THE SUBHOURLY QN FROM THE PREVIOUS HOUR TO UNTIL THE CURRNET HOUR
		//AND STORING IT IN  verticalGLHE(GLHENum)%QnHr(J)

		//SUBHOURLY Qn IS NOT AGGREGATED . IT IS THE BASIC LOAD
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
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using DataEnvironment::MaxNumberSimYears;
		using PlantUtilities::RegisterPlantCompDesignFlow;

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
		int GLHENum;
		int numAlphas; // Number of elements in the alpha array
		int numNums; // Number of elements in the numeric array. "numNums" :)
		int IOStat; // IO Status when calling get input subroutine
		bool isNotOK; // Flag to verify name
		bool isBlank; // Flag for blank name
		int indexNum;
		int pairNum;
		bool allocated;

		// VERTICAL GLHE

		//GET NUMBER OF ALL EQUIPMENT TYPES

		numVerticalGLHEs = GetNumObjectsFound( "GroundHeatExchanger:Vertical" );
		numSlinkyGLHEs = GetNumObjectsFound( "GroundHeatExchanger:Slinky" );

		allocated = false;

		if ( numVerticalGLHEs <= 0 && numSlinkyGLHEs <= 0 ) {
			ShowSevereError( "Error processing inputs for slinky and vertical GLHE objects" );
			ShowContinueError( "Simulation indicated these objects were found, but input processor doesn't find any" );
			ShowContinueError( "Check inputs for GroundHeatExchanger:Vertical and GroundHeatExchanger:Slinky" );
			ShowContinueError( "Also check plant/branch inputs for references to invalid/deleted objects" );
			errorsFound = true;
		}

		if ( numVerticalGLHEs > 0 ) {

			cCurrentModuleObject = "GroundHeatExchanger:Vertical";

			verticalGLHE.allocate( numVerticalGLHEs );

			checkEquipName.dimension( numVerticalGLHEs, true );

			for ( GLHENum = 1; GLHENum <= numVerticalGLHEs; ++GLHENum ) {
				GetObjectItem( cCurrentModuleObject, GLHENum, cAlphaArgs, numAlphas, rNumericArgs, numNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				isNotOK = false;
				isBlank = false;

				// Create temporary array of previous names to pass to VerifyName
				Array1D <std::string> tmpNames;
				tmpNames.allocate( numVerticalGLHEs - 1 );

				// Populate temporary array with previous entrys
				for (int i = 1; i < numVerticalGLHEs - 1; ++i ) {
					tmpNames( i ) = verticalGLHE( i ).Name;
				}

				//get object name
				VerifyName( cAlphaArgs( 1 ), tmpNames, GLHENum - 1, isNotOK, isBlank, cCurrentModuleObject + " name" );

				// Deallocate temporary array when no longer needed
				tmpNames.deallocate();

				if ( isNotOK ) {
					errorsFound = true;
					if ( isBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				verticalGLHE( GLHENum ).Name = cAlphaArgs( 1 );

				//get inlet node num
				verticalGLHE( GLHENum ).inletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), errorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

				//get outlet node num
				verticalGLHE( GLHENum ).outletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), errorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				verticalGLHE( GLHENum ).available = true;
				verticalGLHE( GLHENum ).on = true;

				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Condenser Water Nodes" );

				//load borehole data
				verticalGLHE( GLHENum ).designFlow = rNumericArgs( 1 );
				RegisterPlantCompDesignFlow( verticalGLHE( GLHENum ).inletNodeNum, verticalGLHE( GLHENum ).designFlow );

				verticalGLHE( GLHENum ).numBoreholes = rNumericArgs( 2 );
				verticalGLHE( GLHENum ).boreholeLength = rNumericArgs( 3 );
				verticalGLHE( GLHENum ).boreholeRadius = rNumericArgs( 4 );
				verticalGLHE( GLHENum ).kGround = rNumericArgs( 5 );
				verticalGLHE( GLHENum ).cpRhoGround = rNumericArgs( 6 );
				verticalGLHE( GLHENum ).tempGround = rNumericArgs( 7 );
				verticalGLHE( GLHENum ).kGrout = rNumericArgs( 8 );
				verticalGLHE( GLHENum ).kPipe = rNumericArgs( 9 );
				verticalGLHE( GLHENum ).pipeOutDia = rNumericArgs( 10 );
				verticalGLHE( GLHENum ).UtubeDist = rNumericArgs( 11 );
				verticalGLHE( GLHENum ).pipeThick = rNumericArgs( 12 );
				verticalGLHE( GLHENum ).maxSimYears = rNumericArgs( 13 );
				verticalGLHE( GLHENum ).gReferenceRatio = rNumericArgs( 14 );

				// total tube length
				verticalGLHE( GLHENum ).totalTubeLength = verticalGLHE( GLHENum ).numBoreholes * verticalGLHE( GLHENum ).boreholeLength;

				// ground thermal diffusivity
				verticalGLHE( GLHENum ).diffusivityGround = verticalGLHE( GLHENum ).kGround / verticalGLHE( GLHENum ).cpRhoGround;

				//   Not many checks

				if ( verticalGLHE( GLHENum ).pipeThick >= verticalGLHE( GLHENum ).pipeOutDia / 2.0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + verticalGLHE( GLHENum ).Name + "\", invalid value in field." );
					ShowContinueError( "..." + cNumericFieldNames( 12 ) + "=[" + RoundSigDigits( verticalGLHE( GLHENum ).pipeThick, 3 ) + "]." );
					ShowContinueError( "..." + cNumericFieldNames( 10 ) + "=[" + RoundSigDigits( verticalGLHE( GLHENum ).pipeOutDia, 3 ) + "]." );
					ShowContinueError( "...Radius will be <=0." );
					errorsFound = true;
				}

				if ( verticalGLHE( GLHENum ).maxSimYears < MaxNumberSimYears ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + verticalGLHE( GLHENum ).Name + "\", invalid value in field." );
					ShowContinueError( "..." + cNumericFieldNames( 13 ) + " less than RunPeriod Request" );
					ShowContinueError( "Requested input=" + TrimSigDigits( verticalGLHE( GLHENum ).maxSimYears ) + " will be set to " + TrimSigDigits( MaxNumberSimYears ) );
					verticalGLHE( GLHENum ).maxSimYears = MaxNumberSimYears;
				}

				// Get Gfunction data
				indexNum = 15;
				verticalGLHE( GLHENum ).NPairs = rNumericArgs( indexNum );

				// Get Gfunc error handling
				if ( verticalGLHE( GLHENum ).NPairs < 1 ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + verticalGLHE( GLHENum ).Name + "\", invalid value in field." );
					ShowContinueError( "..." + cNumericFieldNames( indexNum ) + " is less than 1." );
					errorsFound = true;
				} else if ( numNums != ( indexNum + ( verticalGLHE( GLHENum ).NPairs * 2 ) ) ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + verticalGLHE( GLHENum ).Name + "\", invalid number of input fields." );
					errorsFound = true;
				}

				verticalGLHE( GLHENum ).SubAGG = 15;
				verticalGLHE( GLHENum ).AGG = 192;

				// Allocation of all the dynamic arrays
				verticalGLHE( GLHENum ).LNTTS.dimension( verticalGLHE( GLHENum ).NPairs, 0.0 );
				verticalGLHE( GLHENum ).GFNC.dimension( verticalGLHE( GLHENum ).NPairs, 0.0 );
				verticalGLHE( GLHENum ).QnMonthlyAgg.dimension( verticalGLHE( GLHENum ).maxSimYears * 12, 0.0 );
				verticalGLHE( GLHENum ).QnHr.dimension( 730 + verticalGLHE( GLHENum ).AGG + verticalGLHE( GLHENum ).SubAGG, 0.0 );
				verticalGLHE( GLHENum ).QnSubHr.dimension( ( verticalGLHE( GLHENum ).SubAGG + 1 ) * maxTSinHr + 1, 0.0 );
				verticalGLHE( GLHENum ).LastHourN.dimension( verticalGLHE( GLHENum ).SubAGG + 1, 0 );

				if ( ! allocated ) {
					prevTimeSteps.allocate( ( verticalGLHE( GLHENum ).SubAGG + 1 ) * maxTSinHr + 1 );
					prevTimeSteps = 0.0;
					allocated = true;
				}

				indexNum = 16;
				for ( pairNum = 1; pairNum <= verticalGLHE( GLHENum ).NPairs; ++pairNum ) {
					verticalGLHE( GLHENum ).LNTTS( pairNum ) = rNumericArgs( indexNum );
					verticalGLHE( GLHENum ).GFNC( pairNum ) = rNumericArgs( indexNum + 1 );
					indexNum += 2;
				}
				//Check for Errors
				if ( errorsFound ) {
					ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
				}
			}

			//Set up report variables
			for ( GLHENum = 1; GLHENum <= numVerticalGLHEs; ++GLHENum ) {
				SetupOutputVariable( "Ground Heat Exchanger Average Borehole Temperature [C]", verticalGLHE( GLHENum ).boreholeTemp, "System", "Average", verticalGLHE( GLHENum ).Name );
				SetupOutputVariable( "Ground Heat Exchanger Heat Transfer Rate [W]", verticalGLHE( GLHENum ).QGLHE, "System", "Average", verticalGLHE( GLHENum ).Name );
				SetupOutputVariable( "Ground Heat Exchanger Inlet Temperature [C]", verticalGLHE( GLHENum ).inletTemp, "System", "Average", verticalGLHE( GLHENum ).Name );
				SetupOutputVariable( "Ground Heat Exchanger Outlet Temperature [C]", verticalGLHE( GLHENum ).outletTemp, "System", "Average", verticalGLHE( GLHENum ).Name );
				SetupOutputVariable( "Ground Heat Exchanger Mass Flow Rate [kg/s]", verticalGLHE( GLHENum ).massFlowRate, "System", "Average", verticalGLHE( GLHENum ).Name );
				SetupOutputVariable( "Ground Heat Exchanger Average Fluid Temperature [C]", verticalGLHE( GLHENum ).aveFluidTemp, "System", "Average", verticalGLHE( GLHENum ).Name );
			}

		}

		// SLINKY GLHE

		allocated = false;

		if ( numSlinkyGLHEs > 0 ) {

			cCurrentModuleObject = "GroundHeatExchanger:Slinky";

			slinkyGLHE.allocate( numSlinkyGLHEs );

			checkEquipName.dimension( numSlinkyGLHEs, true );

			for ( GLHENum = 1; GLHENum <= numSlinkyGLHEs; ++GLHENum ) {
				GetObjectItem( cCurrentModuleObject, GLHENum, cAlphaArgs, numAlphas, rNumericArgs, numNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				isNotOK = false;
				isBlank = false;

				// Create temporary array of previous names to pass to VerifyName
				Array1D <std::string> tmpNames;
				tmpNames.allocate( numSlinkyGLHEs - 1 );

				// Populate temporary array with previous entrys
				for (int i = 1; i < numSlinkyGLHEs - 1; ++i ) {
					tmpNames( i ) = slinkyGLHE( i ).Name;
				}

				//get object name
				VerifyName( cAlphaArgs( 1 ), tmpNames, GLHENum - 1, isNotOK, isBlank, cCurrentModuleObject + " name" );

				// Deallocate temporary array when no longer needed
				tmpNames.deallocate();

				if ( isNotOK ) {
					errorsFound = true;
					if ( isBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				slinkyGLHE( GLHENum ).Name = cAlphaArgs( 1 );

				//get inlet node num
				slinkyGLHE( GLHENum ).inletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), errorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

				//get outlet node num
				slinkyGLHE( GLHENum ).outletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), errorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				slinkyGLHE( GLHENum ).available = true;
				slinkyGLHE( GLHENum ).on = true;

				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Condenser Water Nodes" );

				//load data
				slinkyGLHE( GLHENum ).designFlow = rNumericArgs( 1 );
				RegisterPlantCompDesignFlow( slinkyGLHE( GLHENum ).inletNodeNum, slinkyGLHE( GLHENum ).designFlow );

				slinkyGLHE( GLHENum ).kGround = rNumericArgs( 2 );
				slinkyGLHE( GLHENum ).cpRhoGround = rNumericArgs( 3 ) * rNumericArgs( 4 );
				slinkyGLHE( GLHENum ).kPipe = rNumericArgs( 5 );
				slinkyGLHE( GLHENum ).rhoPipe = rNumericArgs( 6 );
				slinkyGLHE( GLHENum ).cpPipe = rNumericArgs( 7 );
				slinkyGLHE( GLHENum ).pipeOutDia = rNumericArgs( 8 );
				slinkyGLHE( GLHENum ).pipeThick = rNumericArgs( 9 );

				if ( SameString( cAlphaArgs( 4 ), "VERTICAL" ) ) {
					slinkyGLHE( GLHENum ).verticalConfig = true;
				} else if ( SameString( cAlphaArgs( 4 ), "HORIZONTAL" ) ) {
					slinkyGLHE( GLHENum ).verticalConfig = false;
				}

				slinkyGLHE( GLHENum ).coilDiameter = rNumericArgs( 10 );
				slinkyGLHE( GLHENum ).coilPitch = rNumericArgs( 11 );
				slinkyGLHE( GLHENum ).trenchDepth = rNumericArgs( 12 );
				slinkyGLHE( GLHENum ).trenchLength = rNumericArgs( 13 );
				slinkyGLHE( GLHENum ).numTrenches = rNumericArgs( 14 );
				slinkyGLHE( GLHENum ).trenchSpacing = rNumericArgs( 15 );
				slinkyGLHE( GLHENum ).maxSimYears = rNumericArgs( 16 );

				// Number of coils
				slinkyGLHE( GLHENum ).numCoils = slinkyGLHE( GLHENum ).trenchLength / slinkyGLHE( GLHENum ).coilPitch;

				// Total tube length
				slinkyGLHE( GLHENum ).totalTubeLength = Pi * slinkyGLHE( GLHENum ).coilDiameter * slinkyGLHE( GLHENum ).trenchLength
													* slinkyGLHE( GLHENum ). numTrenches / slinkyGLHE( GLHENum ). coilPitch;

				// Get Gfunction data
				slinkyGLHE( GLHENum ).SubAGG = 15;
				slinkyGLHE( GLHENum ).AGG = 192;

				// Average coil depth
				if ( slinkyGLHE( GLHENum ).verticalConfig ) {
					// Vertical configuration
					if ( slinkyGLHE( GLHENum ).trenchDepth - slinkyGLHE(GLHENum).coilDiameter < 0.0 ) {
						// Error: part of the coil is above ground
						ShowSevereError( cCurrentModuleObject + "=\"" + slinkyGLHE( GLHENum ).Name + "\", invalid value in field." );
						ShowContinueError( "..." + cNumericFieldNames( 13 ) + "=[" + RoundSigDigits( slinkyGLHE( GLHENum ).trenchDepth, 3 ) + "]." );
						ShowContinueError( "..." + cNumericFieldNames( 10 ) + "=[" + RoundSigDigits( slinkyGLHE( GLHENum ).coilDepth, 3 ) + "]." );
						ShowContinueError( "...Average coil depth will be <=0." );
						errorsFound = true;

					} else {
						// Entire coil is below ground
						slinkyGLHE( GLHENum ).coilDepth = slinkyGLHE( GLHENum ).trenchDepth - ( slinkyGLHE( GLHENum ).coilDiameter / 2.0 );
					}

				} else {
					// Horizontal configuration
					slinkyGLHE( GLHENum ). coilDepth = slinkyGLHE( GLHENum ).trenchDepth;
				}

				// Thermal diffusivity of the ground
				slinkyGLHE( GLHENum ).diffusivityGround = slinkyGLHE( GLHENum ).kGround / slinkyGLHE( GLHENum ).cpRhoGround;

				if ( ! allocated ) {
					prevTimeSteps.allocate( ( slinkyGLHE( GLHENum ).SubAGG + 1 ) * maxTSinHr + 1 );
					prevTimeSteps = 0.0;
					allocated = true;
				}

				//   Not many checks

				if ( slinkyGLHE( GLHENum ).pipeThick >= slinkyGLHE( GLHENum ).pipeOutDia / 2.0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + slinkyGLHE( GLHENum ).Name + "\", invalid value in field." );
					ShowContinueError( "..." + cNumericFieldNames( 12 ) + "=[" + RoundSigDigits( slinkyGLHE( GLHENum ).pipeThick, 3 ) + "]." );
					ShowContinueError( "..." + cNumericFieldNames( 10 ) + "=[" + RoundSigDigits( slinkyGLHE( GLHENum ).pipeOutDia, 3 ) + "]." );
					ShowContinueError( "...Radius will be <=0." );
					errorsFound = true;
				}

				// Initialize ground temperature model and get pointer reference
				slinkyGLHE( GLHENum ).groundTempModel = GetGroundTempModelAndInit( cAlphaArgs( 5 ) , cAlphaArgs( 6 ) );
				if ( slinkyGLHE ( GLHENum ).groundTempModel ) {
					errorsFound = slinkyGLHE ( GLHENum ).groundTempModel->errorsFound;
				}

				//Check for Errors
				if ( errorsFound ) {
					ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
				}
			}

			//Set up report variables
			for ( GLHENum = 1; GLHENum <= numSlinkyGLHEs; ++GLHENum ) {
				SetupOutputVariable( "Ground Heat Exchanger Average Borehole Temperature [C]", slinkyGLHE( GLHENum ).boreholeTemp, "System", "Average", slinkyGLHE( GLHENum ).Name );
				SetupOutputVariable( "Ground Heat Exchanger Heat Transfer Rate [W]", slinkyGLHE( GLHENum ).QGLHE, "System", "Average", slinkyGLHE( GLHENum ).Name );
				SetupOutputVariable( "Ground Heat Exchanger Inlet Temperature [C]", slinkyGLHE( GLHENum ).inletTemp, "System", "Average", slinkyGLHE( GLHENum ).Name );
				SetupOutputVariable( "Ground Heat Exchanger Outlet Temperature [C]", slinkyGLHE( GLHENum ).outletTemp, "System", "Average", slinkyGLHE( GLHENum ).Name );
				SetupOutputVariable( "Ground Heat Exchanger Mass Flow Rate [kg/s]", slinkyGLHE( GLHENum ).massFlowRate, "System", "Average", slinkyGLHE( GLHENum ).Name );
				SetupOutputVariable( "Ground Heat Exchanger Average Fluid Temperature [C]", slinkyGLHE( GLHENum ).aveFluidTemp, "System", "Average", slinkyGLHE( GLHENum ).Name );
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
		BholeMdot = massFlowRate / numBoreholes; //verticalGLHE(GLHENum)%designFlow*fluidDensity /numBoreholes

		pipeOuterRad = pipeOutDia / 2.0;
		pipeInnerRad = pipeOuterRad - pipeThick;
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
		Rcond = std::log( pipeOuterRad / pipeInnerRad ) / ( 2.0 * Pi * kPipe ) / 2.0; // pipe in parallel so /2

		//   Resistance Due to the grout.
		maxDistance = 2.0 * boreholeRadius - ( 2.0 * pipeOutDia );
		distanceRatio = UtubeDist / maxDistance;

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

		Rgrout = 1.0 / ( kGrout * ( B0 * std::pow( boreholeRadius / pipeOuterRad, B1 ) ) );
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

		pipeOuterRad = pipeOutDia / 2.0;
		pipeInnerRad = pipeOuterRad - pipeThick;
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
		Rcond = std::log( pipeOuterRad / pipeInnerRad ) / ( 2.0 * Pi * kPipe ) / 2.0; // pipe in parallel so /2

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

		if ( LnTTsVal <= LNTTS( 1 ) ) {
			gFuncVal = ( ( LnTTsVal - LNTTS( 1 ) ) / ( LNTTS( 2 ) - LNTTS( 1 ) ) ) * ( GFNC( 2 ) - GFNC( 1 ) ) + GFNC( 1 );
			return gFuncVal;
		}

		// The following IF loop determines the g-function for the case
		// when LnTTsVal is greater than the last element of the LnTTs array.
		// In this case, the g-function must be found by extrapolation.

		if ( LnTTsVal > LNTTS( NPairs ) ) {
			gFuncVal = ( ( LnTTsVal - LNTTS( NPairs ) ) / ( LNTTS( NPairs - 1 ) - LNTTS( NPairs ) ) ) * ( GFNC( NPairs - 1 ) - GFNC( NPairs ) ) + GFNC( NPairs );
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
			if ( LNTTS( Mid ) < LnTTsVal ) {
				Low = Mid + 1;
			} else {
				if ( LNTTS( Mid ) > LnTTsVal ) {
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
			gFuncVal = GFNC( Mid );
			return gFuncVal;
		}

		//LnTTsVal is in between any of the two LnTTS array elements find the
		// gfunction value by interplation and apply the correction and return gFuncVal
		else {
			if ( LNTTS( Mid ) < LnTTsVal ) ++Mid;

			gFuncVal = ( ( LnTTsVal - LNTTS( Mid ) ) / ( LNTTS( Mid - 1 ) - LNTTS( Mid ) ) ) * ( GFNC( Mid - 1 ) - GFNC( Mid ) ) + GFNC( Mid );

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

		RATIO = boreholeRadius / boreholeLength;

		if ( RATIO != gReferenceRatio ) {
			gFuncVal -= std::log( boreholeRadius / ( boreholeLength * gReferenceRatio ) );
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
		using DataPlant::TypeOf_GrndHtExchgVertical;
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

		// Init more variables
		if ( myFlag ) {
			// Locate the hx on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( Name, TypeOf_GrndHtExchgVertical, loopNum, loopSideNum, branchNum, compNum, _, _, _, _, _, errFlag );
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
			ScanPlantLoopsForObject( Name, TypeOf_GrndHtExchgSlinky, loopNum, loopSideNum, branchNum, compNum, _, _, _, _, _, errFlag );
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
