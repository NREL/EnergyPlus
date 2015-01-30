// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
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
#include <FluidProperties.hh>
#include <General.hh>
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

	// Data
	// DERIVED TYPE DEFINITIONS

	// MODULE PARAMETER DEFINITIONS
	Real64 const hrsPerDay( 24.0 ); // Number of hours in a day
	Real64 const hrsPerMonth( 730.0 ); // Number of hours in month
	int const maxTSinHr( 60 ); // Max number of time step in a hour

	// MODULE VARIABLE DECLARATIONS:
	int numVerticalGLHEs( 0 );
	int N( 1 ); // COUNTER OF TIME STEP
	Real64 currentSimTime( 0.0 ); // Current simulation time in hours
	int locHourOfDay( 0 );
	int locDayOfSim( 0 );

	FArray1D< Real64 > prevTimeSteps; // This is used to store only the Last Few time step's time
	// to enable the calculation of the subhouly contribution..
	// Recommended size, the product of Minimum subhourly history required and
	// the maximum no of system time steps in an hour

	FArray1D_bool checkEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE CondenserTowers

	// Object Data
	FArray1D< GLHEVert > verticalGLHE; // dimension to number of machines

	// MODULE SUBROUTINES:

	//******************************************************************************

	// Functions

	void
	SimGroundHeatExchangers(
		std::string const & type,
		std::string const & name,
		int & compIndex,
		bool const runFlag,
		bool const firstIteration,
		bool const initLoopEquip
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    August, 2000
		//       MODIFIED         Arun Murugappan
		//       RE-ENGINEERED    na

		// PURPOSE OF THIS SUBROUTINE:
		// mananges the simulation of the vertical closed-loop ground heat
		// exchangers (GLHE) model

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// Eskilson, P. 'Thermal Analysis of Heat Extraction Boreholes' Ph.D. Thesis:
		//   Dept. of Mathematical Physics, University of Lund, Sweden, June 1987.
		// Yavuzturk, C., J.D. Spitler. 1999. 'A Short Time Step Response Factor Model
		//   for Vertical Ground Loop Heat Exchangers. ASHRAE Transactions. 105(2): 475-485.

		// USE STATEMENTS:

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInput( true );
		int GLHENum;

		//GET INPUT
		if ( GetInput ) {
			GetGroundHeatExchangerInput();
			GetInput = false;
		}

		// Find the correct Furnace
		if ( compIndex == 0 ) {
			GLHENum = FindItemInList( name, verticalGLHE.name(), numVerticalGLHEs );
			if ( GLHENum == 0 ) {
				ShowFatalError( "SimGroundHeatExchangers: Unit not found=" + name );
			}
			compIndex = GLHENum;
		} else {
			GLHENum = compIndex;
			if ( GLHENum > numVerticalGLHEs || GLHENum < 1 ) {
				ShowFatalError( "SimGroundHeatExchangers:  Invalid compIndex passed=" + TrimSigDigits( GLHENum ) + ", Number of Units=" + TrimSigDigits( numVerticalGLHEs ) + ", Entered Unit name=" + name );
			}
			if ( checkEquipName( GLHENum ) ) {
				if ( name != verticalGLHE( GLHENum ).name ) {
					ShowFatalError( "SimGroundHeatExchangers: Invalid compIndex passed=" + TrimSigDigits( numVerticalGLHEs ) + ", Unit name=" + name + ", stored Unit name for that index=" + verticalGLHE( GLHENum ).name );
				}
				checkEquipName( GLHENum ) = false;
			}
		}

		auto & thisGLHE( verticalGLHE( GLHENum ) );

		if ( initLoopEquip ) {
			thisGLHE.initGLHESimVars();
			return;
		}

		//INITIALIZE
		thisGLHE.initGLHESimVars();

		//SIMULATE HEAT EXCHANGER
		thisGLHE.calcGroundHeatExchanger();
		thisGLHE.updateGroundHeatExchanger();

	}

	//******************************************************************************

	void
	GLHESlinky::calcGroundHeatExchanger(){};

	void
	GLHEVert::calcGroundHeatExchanger()
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
		//   for Vertical Ground Loop Heat Exchangers. ASHRAE Transactions. 105(2): 475-485.

		// Using/Aliasing
		using DataPlant::PlantLoop;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS
		static std::string const RoutineName( "CalcVerticalGroundHeatExchanger" );

		//LOCAL BHORE HOLE PARAMETERS
		Real64 fluidDensity;
		Real64 kGroundFactor;
		Real64 cpFluid;
		Real64 gFuncVal; // Interpolated G function value at a sub-hour
		static Real64 ToutNew( 19.375 );
		Real64 fluidAveTemp;
		Real64 groundDiffusivity;
		Real64 timeSS; // Steady state time
		Real64 timeSSFactor; // Steady state time factor for calculation
		Real64 XI;
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
		Real64 sumTotal; // sum of all the Qn (load) blocks
		Real64 C0; // **Intermediate constants used
		Real64 C1; // **Intermediate constants used
		Real64 C2; // **in explicit  calcualtion of the
		Real64 C3; // **temperature at the U tube outlet.
		static int PrevN( 1 ); // The saved value of N at previous time step
		int IndexN; // Used to index the LastHourN array
		static bool updateCurSimTime( true ); // Used to reset the CurSimTime to reset after WarmupFlag
		static bool triggerDesignDayReset( false );

		//Autodesk:Uninit Initialize variables used uninitialized
		sumTotal = 0.0; //Autodesk:Uninit Force default initialization

		inletTemp = Node( inletNodeNum ).Temp;

		cpFluid = GetSpecificHeatGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
		fluidDensity = GetDensityGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );

		kGroundFactor = 2.0 * Pi * kGround;
		groundDiffusivity = kGround / cpRhoGround;

		// calculate annual time constant for ground conduction
		timeSS = ( pow_2( boreholeLength ) / ( 9.0 * groundDiffusivity ) ) / SecInHour / 8760.0;
		timeSSFactor = timeSS * 8760.0;

		if ( triggerDesignDayReset && WarmupFlag ) updateCurSimTime = true;
		if ( DayOfSim == 1 && updateCurSimTime ) {
			currentSimTime = 0.0;
			prevTimeSteps = 0.0;
			for ( I = 1; I <= numVerticalGLHEs; ++I ) {
				verticalGLHE( I ).QnHr = 0.0;
				verticalGLHE( I ).QnMonthlyAgg = 0.0;
				verticalGLHE( I ).QnSubHr = 0.0;
				verticalGLHE( I ).LastHourN = 1;
			}
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
			outletTemp = inletTemp;
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
			for ( I = 1; I <= numVerticalGLHEs; ++I ) {
				verticalGLHE( I ).QnSubHr = eoshift( verticalGLHE( I ).QnSubHr, -1, lastQnSubHr );
			}
		}

		calcAggregateLoad();

		// Update the borehole resistance each time
		boreholeResistance();

		if ( N == 1 ) {
			if ( massFlowRate <= 0.0 ) {
				tmpQnSubHourly = 0.0;
				fluidAveTemp = tempGround;
				ToutNew = inletTemp;
			} else {
				XI = std::log( currentSimTime / ( timeSSFactor ) );
				gFuncVal = interpGFunc( XI );

				C_1 = ( boreholeLength * numBoreholes ) / ( 2.0 * massFlowRate * cpFluid );
				tmpQnSubHourly = ( tempGround - inletTemp ) / ( gFuncVal / ( kGroundFactor ) + resistanceBhole + C_1 );
				fluidAveTemp = tempGround - tmpQnSubHourly * resistanceBhole;
				ToutNew = tempGround - tmpQnSubHourly * ( gFuncVal / ( kGroundFactor ) + resistanceBhole - C_1 );
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

				SUBHRLY_LOOP: for ( I = 1; I <= subHourlyLimit; ++I ) {
					if ( I == subHourlyLimit ) {
						if ( int( currentSimTime ) >= SubAGG ) {
							XI = std::log( ( currentSimTime - prevTimeSteps( I + 1 ) ) / ( timeSSFactor ) );
							gFuncVal = interpGFunc( XI );
							RQSubHr = gFuncVal / ( kGroundFactor );
							sumQnSubHourly += ( QnSubHr( I ) - QnHr( IndexN ) ) * RQSubHr;
						} else {
							XI = std::log( ( currentSimTime - prevTimeSteps( I + 1 ) ) / ( timeSSFactor ) );
							gFuncVal = interpGFunc( XI );
							RQSubHr = gFuncVal / ( kGroundFactor );
							sumQnSubHourly += QnSubHr( I ) * RQSubHr;
						}
						goto SUBHRLY_LOOP_exit;
					}
					//prevTimeSteps(I+1) This is "I+1" because prevTimeSteps(1) = CurrentTimestep
					XI = std::log( ( currentSimTime - prevTimeSteps( I + 1 ) ) / ( timeSSFactor ) );
					gFuncVal = interpGFunc( XI );
					RQSubHr = gFuncVal / ( kGroundFactor );
					sumQnSubHourly += ( QnSubHr( I ) - QnSubHr( I + 1 ) ) * RQSubHr;
					SUBHRLY_LOOP_loop: ;
				}
				SUBHRLY_LOOP_exit: ;

				// Calculate the Hourly Superposition

				hourlyLimit = int( currentSimTime );
				sumQnHourly = 0.0;
				HOURLY_LOOP: for ( I = SubAGG + 1; I <= hourlyLimit; ++I ) {
					if ( I == hourlyLimit ) {
						XI = std::log( currentSimTime / ( timeSSFactor ) );
						gFuncVal = interpGFunc( XI );
						RQHour = gFuncVal / ( kGroundFactor );
						sumQnHourly += QnHr( I ) * RQHour;
						goto HOURLY_LOOP_exit;
					}
					XI = std::log( ( currentSimTime - int( currentSimTime ) + I ) / ( timeSSFactor ) );
					gFuncVal = interpGFunc( XI );
					RQHour = gFuncVal / ( kGroundFactor );
					sumQnHourly += ( QnHr( I ) - QnHr( I + 1 ) ) * RQHour;
					HOURLY_LOOP_loop: ;
				}
				HOURLY_LOOP_exit: ;

				// Find the total Sum of the Temperature difference due to all load blocks
				sumTotal = sumQnSubHourly + sumQnHourly;

				//Calulate the subhourly temperature due the Last Time steps Load
				XI = std::log( ( currentSimTime - prevTimeSteps( 2 ) ) / ( timeSSFactor ) );
				gFuncVal = interpGFunc( XI );
				RQSubHr = gFuncVal / ( kGroundFactor );

				if ( massFlowRate <= 0.0 ) {
					tmpQnSubHourly = 0.0;
					fluidAveTemp = tempGround - sumTotal; // Q(N)*RB = 0
					ToutNew = inletTemp;
				} else {
					//Dr.Spitler's Explicit set of equations to calculate the New Outlet Temperature of the U-Tube
					C0 = RQSubHr;
					C1 = tempGround - ( sumTotal - QnSubHr( 1 ) * RQSubHr );
					C2 = boreholeLength * numBoreholes / ( 2.0 * massFlowRate * cpFluid );
					C3 = massFlowRate * cpFluid / ( boreholeLength * numBoreholes );
					tmpQnSubHourly = ( C1 - inletTemp ) / ( resistanceBhole + C0 - C2 + ( 1 / C3 ) );
					fluidAveTemp = C1 - ( C0 + resistanceBhole ) * tmpQnSubHourly;
					ToutNew = C1 + ( C2 - C0 - resistanceBhole ) * tmpQnSubHourly;
				}

			} else { // Monthly Aggregation and super position

				numOfMonths = ( currentSimTime + 1 ) / hrsPerMonth;

				if ( currentSimTime < ( ( numOfMonths ) * hrsPerMonth ) + AGG + SubAGG ) {
					currentMonth = numOfMonths - 1;
				} else {
					currentMonth = numOfMonths;
				}

				//monthly superposition
				sumQnMonthly = 0.0;
				SUMMONTHLY: for ( I = 1; I <= currentMonth; ++I ) {
					if ( I == 1 ) {
						XI = std::log( currentSimTime / ( timeSSFactor ) );
						gFuncVal = interpGFunc( XI );
						RQMonth = gFuncVal / ( kGroundFactor );
						sumQnMonthly += QnMonthlyAgg( I ) * RQMonth;
						goto SUMMONTHLY_loop;
					}
					XI = std::log( ( currentSimTime - ( I - 1 ) * hrsPerMonth ) / ( timeSSFactor ) );
					gFuncVal = interpGFunc( XI );
					RQMonth = gFuncVal / ( kGroundFactor );
					sumQnMonthly += ( QnMonthlyAgg( I ) - QnMonthlyAgg( I - 1 ) ) * RQMonth;
					SUMMONTHLY_loop: ;
				}
				SUMMONTHLY_exit: ;

				// Hourly Supr position
				hourlyLimit = int( currentSimTime - currentMonth * hrsPerMonth );
				sumQnHourly = 0.0;
				HOURLYLOOP: for ( I = 1 + SubAGG; I <= hourlyLimit; ++I ) {
					if ( I == hourlyLimit ) {
						XI = std::log( ( currentSimTime - int( currentSimTime ) + I ) / ( timeSSFactor ) );
						gFuncVal = interpGFunc( XI );
						RQHour = gFuncVal / ( kGroundFactor );
						sumQnHourly += ( QnHr( I ) - QnMonthlyAgg( currentMonth ) ) * RQHour;
						goto HOURLYLOOP_exit;
					}
					XI = std::log( ( currentSimTime - int( currentSimTime ) + I ) / ( timeSSFactor ) );
					gFuncVal = interpGFunc( XI );
					RQHour = gFuncVal / ( kGroundFactor );
					sumQnHourly += ( QnHr( I ) - QnHr( I + 1 ) ) * RQHour;
					HOURLYLOOP_loop: ;
				}
				HOURLYLOOP_exit: ;

				// Subhourly Superposition
				subHourlyLimit = N - LastHourN( SubAGG + 1 );
				sumQnSubHourly = 0.0;
				SUBHRLOOP: for ( I = 1; I <= subHourlyLimit; ++I ) {
					if ( I == subHourlyLimit ) {
						XI = std::log( ( currentSimTime - prevTimeSteps( I + 1 ) ) / ( timeSSFactor ) );
						gFuncVal = interpGFunc( XI );
						RQSubHr = gFuncVal / ( kGroundFactor );
						sumQnSubHourly += ( QnSubHr( I ) - QnHr( SubAGG + 1 ) ) * RQSubHr;
						goto SUBHRLOOP_exit;
					}
					XI = std::log( ( currentSimTime - prevTimeSteps( I + 1 ) ) / ( timeSSFactor ) );
					gFuncVal = interpGFunc( XI );
					RQSubHr = gFuncVal / ( kGroundFactor );
					sumQnSubHourly += ( QnSubHr( I ) - QnSubHr( I + 1 ) ) * RQSubHr;
					SUBHRLOOP_loop: ;
				}
				SUBHRLOOP_exit: ;

				sumTotal = sumQnMonthly + sumQnHourly + sumQnSubHourly;

				//Calulate the subhourly temperature due the Last Time steps Load

				XI = std::log( ( currentSimTime - prevTimeSteps( 2 ) ) / ( timeSSFactor ) );
				gFuncVal = interpGFunc( XI );
				RQSubHr = gFuncVal / ( kGroundFactor );

				if ( massFlowRate <= 0.0 ) {
					tmpQnSubHourly = 0.0;
					fluidAveTemp = tempGround - sumTotal; // Q(N)*RB = 0
					ToutNew = inletTemp;
				} else {
					// Explicit set of equations to calculate the New Outlet Temperature of the U-Tube
					C0 = RQSubHr;
					C1 = tempGround - ( sumTotal - QnSubHr( 1 ) * RQSubHr );
					C2 = boreholeLength * numBoreholes / ( 2 * massFlowRate * cpFluid );
					C3 = massFlowRate * cpFluid / ( boreholeLength * numBoreholes );
					tmpQnSubHourly = ( C1 - inletTemp ) / ( resistanceBhole + C0 - C2 + ( 1 / C3 ) );
					fluidAveTemp = C1 - ( C0 + resistanceBhole ) * tmpQnSubHourly;
					ToutNew = C1 + ( C2 - C0 - resistanceBhole ) * tmpQnSubHourly;
				}
			} //  end of AGG OR NO AGG
		} // end of N  = 1 branch
		boreholeTemp = tempGround - sumTotal; //Autodesk:Uninit sumTotal could have been uninitialized here
		//Load the QnSubHourly Array with a new value at end of every timestep

		//Load the report vars
		lastQnSubHr = tmpQnSubHourly;
		outletTemp = ToutNew;
		QGLHE = tmpQnSubHourly;
		aveFluidTemp = fluidAveTemp;

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
			for ( J = 1; J <= ( N - LastHourN( 1 ) ); ++J ) { // Check during debugging if we need a +1
				SumQnHr += QnSubHr( J ) * std::abs( prevTimeSteps( J ) - prevTimeSteps( J + 1 ) );
			}
			SumQnHr /= std::abs( prevTimeSteps( 1 ) - prevTimeSteps( J ) );
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
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int IndexNum;
		int PairNum;
		bool Allocated;

		//GET NUMBER OF ALL EQUIPMENT TYPES
		cCurrentModuleObject = "GroundHeatExchanger:Vertical";
		numVerticalGLHEs = GetNumObjectsFound( cCurrentModuleObject );

		Allocated = false;

		if ( numVerticalGLHEs <= 0 ) {
			ShowSevereError( "No " + cCurrentModuleObject + " equipment found in input file" );
			ErrorsFound = true;
		}

		verticalGLHE.allocate( numVerticalGLHEs );

		checkEquipName.dimension( numVerticalGLHEs, true );

		for ( GLHENum = 1; GLHENum <= numVerticalGLHEs; ++GLHENum ) {
			GetObjectItem( cCurrentModuleObject, GLHENum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;

			//get object name
			VerifyName( cAlphaArgs( 1 ), verticalGLHE.name(), GLHENum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			verticalGLHE( GLHENum ).name = cAlphaArgs( 1 );

			//get inlet node num
			verticalGLHE( GLHENum ).inletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			//get outlet node num
			verticalGLHE( GLHENum ).outletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
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

			//   Not many checks
			if ( verticalGLHE( GLHENum ).pipeThick >= verticalGLHE( GLHENum ).pipeOutDia / 2.0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + verticalGLHE( GLHENum ).name + "\", invalid value in field." );
				ShowContinueError( "..." + cNumericFieldNames( 13 ) + "=[" + RoundSigDigits( verticalGLHE( GLHENum ).pipeThick, 3 ) + "]." );
				ShowContinueError( "..." + cNumericFieldNames( 11 ) + "=[" + RoundSigDigits( verticalGLHE( GLHENum ).pipeOutDia, 3 ) + "]." );
				ShowContinueError( "...Radius will be <=0." );
				ErrorsFound = true;
			}

			if ( verticalGLHE( GLHENum ).maxSimYears < MaxNumberSimYears ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + verticalGLHE( GLHENum ).name + "\", invalid value in field." );
				ShowContinueError( "..." + cNumericFieldNames( 14 ) + " less than RunPeriod Request" );
				ShowContinueError( "Requested input=" + TrimSigDigits( verticalGLHE( GLHENum ).maxSimYears ) + " will be set to " + TrimSigDigits( MaxNumberSimYears ) );
				verticalGLHE( GLHENum ).maxSimYears = MaxNumberSimYears;
			}

			// Get Gfunction data
			verticalGLHE( GLHENum ).NPairs = rNumericArgs( 15 );
			verticalGLHE( GLHENum ).SubAGG = 15;
			verticalGLHE( GLHENum ).AGG = 192;

			// Allocation of all the dynamic arrays
			verticalGLHE( GLHENum ).LNTTS.allocate( verticalGLHE( GLHENum ).NPairs );
			verticalGLHE( GLHENum ).LNTTS = 0.0;
			verticalGLHE( GLHENum ).GFNC.allocate( verticalGLHE( GLHENum ).NPairs );
			verticalGLHE( GLHENum ).GFNC = 0.0;
			verticalGLHE( GLHENum ).QnMonthlyAgg.allocate( verticalGLHE( GLHENum ).maxSimYears * 12 );
			verticalGLHE( GLHENum ).QnMonthlyAgg = 0.0;
			verticalGLHE( GLHENum ).QnHr.allocate( 730 + verticalGLHE( GLHENum ).AGG + verticalGLHE( GLHENum ).SubAGG );
			verticalGLHE( GLHENum ).QnHr = 0.0;
			verticalGLHE( GLHENum ).QnSubHr.allocate( ( verticalGLHE( GLHENum ).SubAGG + 1 ) * maxTSinHr + 1 );
			verticalGLHE( GLHENum ).QnSubHr = 0.0;
			verticalGLHE( GLHENum ).LastHourN.allocate( verticalGLHE( GLHENum ).SubAGG + 1 );
			verticalGLHE( GLHENum ).LastHourN = 0;

			if ( ! Allocated ) {
				prevTimeSteps.allocate( ( verticalGLHE( GLHENum ).SubAGG + 1 ) * maxTSinHr + 1 );
				prevTimeSteps = 0.0;
				Allocated = true;
			}

			IndexNum = 16;
			for ( PairNum = 1; PairNum <= verticalGLHE( GLHENum ).NPairs; ++PairNum ) {
				verticalGLHE( GLHENum ).LNTTS( PairNum ) = rNumericArgs( IndexNum );
				verticalGLHE( GLHENum ).GFNC( PairNum ) = rNumericArgs( IndexNum + 1 );
				IndexNum += 2;
			}
			//Check for Errors
			if ( ErrorsFound ) {
				ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
			}
		}

		//Set up report variables
		for ( GLHENum = 1; GLHENum <= numVerticalGLHEs; ++GLHENum ) {
			SetupOutputVariable( "Ground Heat Exchanger Average Borehole Temperature [C]", verticalGLHE( GLHENum ).boreholeTemp, "System", "Average", verticalGLHE( GLHENum ).name );
			SetupOutputVariable( "Ground Heat Exchanger Heat Transfer Rate [W]", verticalGLHE( GLHENum ).QGLHE, "System", "Average", verticalGLHE( GLHENum ).name );
			SetupOutputVariable( "Ground Heat Exchanger Inlet Temperature [C]", verticalGLHE( GLHENum ).inletTemp, "System", "Average", verticalGLHE( GLHENum ).name );
			SetupOutputVariable( "Ground Heat Exchanger Outlet Temperature [C]", verticalGLHE( GLHENum ).outletTemp, "System", "Average", verticalGLHE( GLHENum ).name );
			SetupOutputVariable( "Ground Heat Exchanger Mass Flow Rate [kg/s]", verticalGLHE( GLHENum ).massFlowRate, "System", "Average", verticalGLHE( GLHENum ).name );
			SetupOutputVariable( "Ground Heat Exchanger Average Fluid Temperature [C]", verticalGLHE( GLHENum ).aveFluidTemp, "System", "Average", verticalGLHE( GLHENum ).name );
		}

	}

	//******************************************************************************

	void
	GLHEVert::boreholeResistance()
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

		cpFluid = GetSpecificHeatGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
		kFluid = GetConductivityGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
		fluidDensity = GetDensityGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
		fluidViscosity = GetViscosityGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );

		//calculate mass flow rate
		BholeMdot = massFlowRate / numBoreholes; //verticalGLHE(GLHENum)%designFlow*fluidDensity /numBoreholes

		pipeOuterRad = pipeOutDia / 2.0;
		pipeInnerRad = pipeOuterRad - pipeThick;
		pipeInnerDia = 2.0 * pipeInnerRad;
		//Re=Rho*V*D/Mu
		reynoldsNum = fluidDensity * pipeInnerDia * ( BholeMdot / fluidDensity / ( Pi * pow_2( pipeInnerRad ) ) ) / fluidViscosity;
		prandtlNum = ( cpFluid * fluidViscosity ) / ( kFluid );
		//   Convection Resistance
		nusseltNum = 0.023 * std::pow( reynoldsNum, 0.8 ) * std::pow( prandtlNum, 0.35 );
		hci = nusseltNum * kFluid / pipeInnerDia;
		if ( BholeMdot == 0.0 ) {
			Rconv = 0.0;
		} else {
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
		resistanceBhole = Rcond + Rconv + Rgrout;
	}

	//******************************************************************************

	Real64
	GLHESlinky::interpGFunc(
		Real64 const LnTTsVal // The value of LN(t/TimeSS) that a g-function
		)
	{
		return 0;
	}

	Real64
	GLHEVert::interpGFunc(
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
		Real64 RATIO;
		Real64 gFuncVal;

		//Binary Search Algorithms Variables
		// REFERENCE      :  DATA STRUCTURES AND ALGORITHM ANALYSIS IN C BY MARK ALLEN WEISS
		int Mid;
		int Low;
		int High;
		bool Found;

		//NumPairs = this->NPairs;
		RATIO = boreholeRadius / boreholeLength;

		// The following IF loop determines the g-function for the case
		// when LnTTsVal is less than the first element of the LnTTs array.
		// In this case, the g-function must be found by extrapolation.

		if ( LnTTsVal <= LNTTS( 1 ) ) {
			gFuncVal = ( ( LnTTsVal - LNTTS( 1 ) ) / ( LNTTS( 2 ) - LNTTS( 1 ) ) ) * ( GFNC( 2 ) - GFNC( 1 ) ) + GFNC( 1 );

			// The following IF statement determines the condition of the ratio
			// between the borehole radius and the active borehole length.
			// If RATIO does not equal 0.0005 then a correction factor for
			// the g-function must be used.

			if ( RATIO != gReferenceRatio ) {
				gFuncVal -= std::log( boreholeRadius / ( boreholeLength * gReferenceRatio ) );
			}

			return gFuncVal;
		}

		// The following IF loop determines the g-function for the case
		// when LnTTsVal is greater than the last element of the LnTTs array.
		// In this case, the g-function must be found by extrapolation.

		if ( LnTTsVal > LNTTS( NPairs ) ) {
			gFuncVal = ( ( LnTTsVal - LNTTS( NPairs ) ) / ( LNTTS( NPairs - 1 ) - LNTTS( NPairs ) ) ) * ( GFNC( NPairs - 1 ) - GFNC( NPairs ) ) + GFNC( NPairs );

			// Apply correction factor if necessary
			if ( RATIO != gReferenceRatio ) {
				gFuncVal -= std::log( boreholeRadius / ( boreholeLength * gReferenceRatio ) );
			}

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
		LOOP: while ( Low <= High ) {
			Mid = ( Low + High ) / 2;
			if ( LNTTS( Mid ) < LnTTsVal ) {
				Low = Mid + 1;
			} else {
				if ( LNTTS( Mid ) > LnTTsVal ) {
					High = Mid - 1;
				} else {
					Found = true;
					goto LOOP_exit;
				}
			}
			LOOP_loop: ;
		}
		LOOP_exit: ;
		//LnTTsVal is identical to one of the LnTTS array elements return gFuncVal
		//the gFuncVal after applying the correction
		if ( Found ) {
			gFuncVal = GFNC( Mid );
			// Apply correction factor if necessary
			if ( RATIO != gReferenceRatio ) {
				gFuncVal -= std::log( boreholeRadius / ( boreholeLength * gReferenceRatio ) );
			}
			return gFuncVal;
		}

		//LnTTsVal is in between any of the two LnTTS array elements find the
		// gfunction value by interplation and apply the correction and return gFuncVal
		else {
			if ( LNTTS( Mid ) < LnTTsVal ) ++Mid;

			gFuncVal = ( ( LnTTsVal - LNTTS( Mid ) ) / ( LNTTS( Mid - 1 ) - LNTTS( Mid ) ) ) * ( GFNC( Mid - 1 ) - GFNC( Mid ) ) + GFNC( Mid );

			// Apply correction factor if necessary
			if ( RATIO != gReferenceRatio ) {
				gFuncVal -= std::log( boreholeRadius / ( boreholeLength * gReferenceRatio ) );
			}
			return gFuncVal;
		}
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
			ScanPlantLoopsForObject( name, TypeOf_GrndHtExchgVertical, loopNum, loopSideNum, branchNum, compNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "initGLHESimVars: Program terminated due to previous condition(s)." );
			}
			myFlag = false;
		}

		if ( myEnvrnFlag && BeginEnvrnFlag ) {
			std::cout << "BegEnvrnFlag = true\n" ;
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
			QGLHE = 0.0;
			LastHourN = 0;
			prevTimeSteps = 0.0;
			currentSimTime = 0.0;
		}

		massFlowRate = RegulateCondenserCompFlowReqOp( loopNum, loopSideNum, branchNum, compNum, designMassFlow );

		SetComponentFlowRate( massFlowRate, inletNodeNum, outletNodeNum, loopNum, loopSideNum, branchNum, compNum );

		// Reset local environment init flag
		if ( ! BeginEnvrnFlag ) myEnvrnFlag = true;

	}

	//******************************************************************************

	void
	GLHEVert::updateGroundHeatExchanger()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Dan Fisher
		//       DATE WRITTEN:    August, 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Updates the GLHE report variable data structure

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using PlantUtilities::SafeCopyPlantNode;
		using DataPlant::PlantLoop;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const DeltaTempLimit( 100.0 ); // temp limit for warnings
		static std::string const RoutineName( "updateVerticalGroundHeatExchanger" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static int numErrorCalls( 0 );
		Real64 GLHEdeltaTemp; // ABS(Outlet temp -inlet temp)
		Real64 fluidDensity;

		SafeCopyPlantNode( inletNodeNum, outletNodeNum );

		Node( outletNodeNum ).Temp = outletTemp;
		Node( outletNodeNum ).Enthalpy = outletTemp * GetSpecificHeatGlycol( PlantLoop( loopNum ).FluidName, outletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );

		GLHEdeltaTemp = std::abs( outletTemp - inletTemp );
		QGLHE = QGLHE * boreholeLength * numBoreholes;

		if ( GLHEdeltaTemp > DeltaTempLimit && numErrorCalls < numVerticalGLHEs && ! WarmupFlag ) {
			fluidDensity = GetDensityGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
			designMassFlow = designFlow * fluidDensity;
			ShowWarningError( "Check GLHE design inputs & g-functions for consistency" );
			ShowContinueError( "For GroundHeatExchanger:Vertical " + name + "GLHE delta Temp > 100C." );
			ShowContinueError( "This can be encountered in cases where the GLHE mass flow rate is either significantly" );
			ShowContinueError( " lower than the design value, or cases where the mass flow rate rapidly changes." );
			ShowContinueError( "GLHE Current Flow Rate=" + TrimSigDigits( massFlowRate, 3 ) + "; GLHE Design Flow Rate=" + TrimSigDigits( designMassFlow, 3 ) );
			++numErrorCalls;
		}

	}

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
