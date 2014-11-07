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
	Real64 const HrsPerDay( 24.0 ); // Number of hours in a day
	Real64 const HrsPerMonth( 730.0 ); // Number of hours in month
	int const MaxTSinHr( 60 ); // Max number of time step in a hour

	// MODULE VARIABLE DECLARATIONS:
	int NumVerticalGlhes( 0 );
	int N( 1 ); // COUNTER OF TIME STEP
	Real64 CurrentSimTime( 0.0 ); // Current simulation time in hours
	Real64 GlheOutletTemp( 0.0 ); // Outlet temperature of the fluid  [°C]
	Real64 GlheInletTemp( 0.0 ); // Inlet temperature of the fluid   [°C]
	Real64 GlheMassFlowRate( 0.0 ); // Mass flowrate of the fluid       [Kg/s]
	Real64 QGlhe( 0.0 ); // The normalised heat transfer rate[W/m]
	Real64 GlheRB( 0.0 ); // [K per W/m] Just for Analyis will be removed later
	Real64 GlheAveFluidTemp( 0.0 ); // The average fluid temperature    [°C]
	Real64 GlheBoreholeTemp( 0.0 ); // The average borehole tempreature [°C]
	int LocHourOfDay( 0 );
	int LocDayOfSim( 0 );
	FArray1D< Real64 > LastQnSubHr; // Previous time step Qn subhourly value
	Real64 MDotActual;

	FArray1D< Real64 > PrevTimeSteps; // This is used to store only the Last Few time step's time
	// to enable the calculation of the subhouly contribution..
	// Recommended size, the product of Minimum subhourly history required and
	// the maximum no of system time steps in an hour

	FArray1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE CondenserTowers

	// Object Data
	FArray1D< GlheSpecs > VerticalGlhe; // dimension to number of machines
	FArray1D< ReportVars > VerticalGlheReport;

	// MODULE SUBROUTINES:

	//******************************************************************************

	// Functions

	void
	SimGroundHeatExchangers(
		std::string const & GlheType,
		std::string const & GlheName,
		int & CompIndex,
		bool const RunFlag,
		bool const FirstIteration,
		bool const InitLoopEquip
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
		int GlheNum;

		//GET INPUT
		if ( GetInput ) {
			GetGroundheatExchangerInput();
			GetInput = false;
		}

		// Find the correct Furnace
		if ( CompIndex == 0 ) {
			GlheNum = FindItemInList( GlheName, VerticalGlhe.Name(), NumVerticalGlhes );
			if ( GlheNum == 0 ) {
				ShowFatalError( "SimGroundHeatExchangers: Unit not found=" + GlheName );
			}
			CompIndex = GlheNum;
		} else {
			GlheNum = CompIndex;
			if ( GlheNum > NumVerticalGlhes || GlheNum < 1 ) {
				ShowFatalError( "SimGroundHeatExchangers:  Invalid CompIndex passed=" + TrimSigDigits( GlheNum ) + ", Number of Units=" + TrimSigDigits( NumVerticalGlhes ) + ", Entered Unit name=" + GlheName );
			}
			if ( CheckEquipName( GlheNum ) ) {
				if ( GlheName != VerticalGlhe( GlheNum ).Name ) {
					ShowFatalError( "SimGroundHeatExchangers: Invalid CompIndex passed=" + TrimSigDigits( NumVerticalGlhes ) + ", Unit name=" + GlheName + ", stored Unit Name for that index=" + VerticalGlhe( GlheNum ).Name );
				}
				CheckEquipName( GlheNum ) = false;
			}
		}

		if ( InitLoopEquip ) {
			InitBoreholeHXSimVars( GlheNum, RunFlag );
			return;
		}

		//INITIALIZE
		InitBoreholeHXSimVars( GlheNum, RunFlag );

		//SIMULATE HEAT EXCHANGER
		CalcVerticalGroundHeatExchanger( GlheNum );
		UpdateVerticalGroundHeatExchanger( RunFlag, GlheNum );

	}

	//******************************************************************************

	void
	CalcVerticalGroundHeatExchanger( int & GlheNum )
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
		int NumBholes;
		Real64 FluidDensity;
		Real64 BholeLength;
		Real64 K_Ground;
		Real64 K_Ground_Factor;
		Real64 Cp_Fluid;
		Real64 Tground;
		Real64 ResistanceBhole; // The thermal resistance of the borehole, (K per W/m]
		Real64 GfuncVal; // Interpolated G function value at a sub-hour
		static Real64 ToutNew( 19.375 );
		Real64 FluidAveTemp;
		Real64 GroundDiffusivity;
		Real64 TimeSS; // Steady state time
		Real64 TimeSS_Factor; // Steady state time factor for calculation
		Real64 XI;
		Real64 C_1;
		int NumOfMonths; // the number of months of simulation elapsed
		int CurrentMonth; // The Month upto which the Montly blocks are superposed
		Real64 SumQnMonthly; // tmp variable which holds the sum of the Temperature diffrence
		// due to Aggregated heat extraction/rejection step
		Real64 SumQnHourly; // same as above for hourly
		Real64 SumQnSubHourly; // same as above for subhourly( with no aggreation]
		Real64 RQMonth;
		Real64 RQHour;
		Real64 RQSubHr;
		int I;
		Real64 tmpQnSubHourly; // current Qn subhourly value
		int HourlyLimit; // number of hours to be taken into account in superposition
		int SubHourlyLimit; // number of subhourlys to be taken into account in subhourly superposition
		Real64 SumTotal; // sum of all the Qn (load) blocks
		Real64 C0; // **Intermediate constants used
		Real64 C1; // **Intermediate constants used
		Real64 C2; // **in explicit  calcualtion of the
		Real64 C3; // **temperature at the U tube outlet.
		static int PrevN( 1 ); // The saved value of N at previous time step
		int IndexN; // Used to index the LastHourN array
		static bool UpdateCurSimTime( true ); // Used to reset the CurSimTime to reset after WarmupFlag
		static bool TriggerDesignDayReset( false );
		int GlheInletNode; // Inlet node number of the Glhe
		int GlheOutletNode; // Outlet node number of the Glhe
		//    LOGICAL, SAVE      :: Allocated = .FALSE.
		int AGG;
		int SubAGG;
		int LoopNum;
		int LoopSideNum;

		//Autodesk:Uninit Initialize variables used uninitialized
		SumTotal = 0.0; //Autodesk:Uninit Force default initialization

		//set local glhe parameters

		NumBholes = VerticalGlhe( GlheNum ).NumBoreholes;
		BholeLength = VerticalGlhe( GlheNum ).BoreholeLength;
		GlheInletNode = VerticalGlhe( GlheNum ).GlheInletNodeNum;
		GlheInletTemp = Node( GlheInletNode ).Temp;
		Cp_Fluid = GetSpecificHeatGlycol( PlantLoop( VerticalGlhe( GlheNum ).LoopNum ).FluidName, GlheInletTemp, PlantLoop( VerticalGlhe( GlheNum ).LoopNum ).FluidIndex, RoutineName );

		Tground = VerticalGlhe( GlheNum ).TempGround;
		FluidDensity = GetDensityGlycol( PlantLoop( VerticalGlhe( GlheNum ).LoopNum ).FluidName, GlheInletTemp, PlantLoop( VerticalGlhe( GlheNum ).LoopNum ).FluidIndex, RoutineName );
		K_Ground = VerticalGlhe( GlheNum ).KGround;
		K_Ground_Factor = 2.0 * Pi * K_Ground;
		AGG = VerticalGlhe( GlheNum ).AGG;
		SubAGG = VerticalGlhe( GlheNum ).SubAGG;
		GroundDiffusivity = VerticalGlhe( GlheNum ).KGround / VerticalGlhe( GlheNum ).CpRhoGround;

		// calculate annual time constant for ground conduction
		TimeSS = ( pow_2( VerticalGlhe( GlheNum ).BoreholeLength ) / ( 9.0 * GroundDiffusivity ) ) / SecInHour / 8760.0;
		TimeSS_Factor = TimeSS * 8760.0;

		GlheOutletNode = VerticalGlhe( GlheNum ).GlheOutletNodeNum;
		LoopNum = VerticalGlhe( GlheNum ).LoopNum;
		LoopSideNum = VerticalGlhe( GlheNum ).LoopSideNum;

		GlheMassFlowRate = MDotActual;

		if ( TriggerDesignDayReset && WarmupFlag ) UpdateCurSimTime = true;
		if ( DayOfSim == 1 && UpdateCurSimTime ) {
			CurrentSimTime = 0.0;
			PrevTimeSteps = 0.0;
			for ( I = 1; I <= NumVerticalGlhes; ++I ) {
				VerticalGlhe( I ).QnHr = 0.0;
				VerticalGlhe( I ).QnMonthlyAgg = 0.0;
				VerticalGlhe( I ).QnSubHr = 0.0;
				VerticalGlhe( I ).LastHourN = 1;
			}
			N = 1;
			UpdateCurSimTime = false;
			TriggerDesignDayReset = false;
		}

		CurrentSimTime = ( DayOfSim - 1 ) * 24 + HourOfDay - 1 + ( TimeStep - 1 ) * TimeStepZone + SysTimeElapsed; //+ TimeStepsys
		LocHourOfDay = mod( CurrentSimTime, HrsPerDay ) + 1;
		LocDayOfSim = CurrentSimTime / 24 + 1;

		if ( DayOfSim > 1 ) {
			UpdateCurSimTime = true;
		}

		if ( ! WarmupFlag ) {
			TriggerDesignDayReset = true;
		}

		if ( CurrentSimTime <= 0.0 ) {
			PrevTimeSteps = 0.0; // this resets history when rounding 24:00 hours during warmup avoids hard crash later
			GlheOutletTemp = GlheInletTemp;
			GlheMassFlowRate = MDotActual;
			CalcAggregateLoad( GlheNum ); //Just allocates and initializes PrevHour array
			return;
		}

		// Store currentsimtime in PrevTimeSteps only if a time step occurs

		if ( PrevTimeSteps( 1 ) != CurrentSimTime ) {
			PrevTimeSteps = eoshift( PrevTimeSteps, -1, CurrentSimTime );
			++N;
		}
		if ( N != PrevN ) {
			PrevN = N;
			for ( I = 1; I <= NumVerticalGlhes; ++I ) {
				VerticalGlhe( I ).QnSubHr = eoshift( VerticalGlhe( I ).QnSubHr, -1, LastQnSubHr( I ) );
			}
		}

		CalcAggregateLoad( GlheNum );

		// Update the borehole resistance each time
		BoreholeResistance( GlheNum, ResistanceBhole );

		if ( N == 1 ) {
			if ( MDotActual <= 0.0 ) {
				tmpQnSubHourly = 0.0;
				FluidAveTemp = Tground;
				ToutNew = GlheInletTemp;
			} else {
				XI = std::log( CurrentSimTime / ( TimeSS_Factor ) );
				INTERP( GlheNum, XI, GfuncVal );

				C_1 = ( BholeLength * NumBholes ) / ( 2.0 * MDotActual * Cp_Fluid );
				tmpQnSubHourly = ( Tground - GlheInletTemp ) / ( GfuncVal / ( K_Ground_Factor ) + ResistanceBhole + C_1 );
				FluidAveTemp = Tground - tmpQnSubHourly * ResistanceBhole;
				ToutNew = Tground - tmpQnSubHourly * ( GfuncVal / ( K_Ground_Factor ) + ResistanceBhole - C_1 );
			}
		} else {
			// no monthly super position
			if ( CurrentSimTime < ( HrsPerMonth + AGG + SubAGG ) ) {

				// Calculate the Sub Hourly Superposition
				SumQnSubHourly = 0.0;
				if ( int( CurrentSimTime ) < SubAGG ) {
					IndexN = int( CurrentSimTime ) + 1;
				} else {
					IndexN = SubAGG + 1;
				}
				SubHourlyLimit = N - VerticalGlhe( GlheNum ).LastHourN( IndexN ); //Check this when running simulation

				SUBHRLY_LOOP: for ( I = 1; I <= SubHourlyLimit; ++I ) {
					if ( I == SubHourlyLimit ) {
						if ( int( CurrentSimTime ) >= SubAGG ) {
							XI = std::log( ( CurrentSimTime - PrevTimeSteps( I + 1 ) ) / ( TimeSS_Factor ) );
							INTERP( GlheNum, XI, GfuncVal );
							RQSubHr = GfuncVal / ( K_Ground_Factor );
							SumQnSubHourly += ( VerticalGlhe( GlheNum ).QnSubHr( I ) - VerticalGlhe( GlheNum ).QnHr( IndexN ) ) * RQSubHr;
						} else {
							XI = std::log( ( CurrentSimTime - PrevTimeSteps( I + 1 ) ) / ( TimeSS_Factor ) );
							INTERP( GlheNum, XI, GfuncVal );
							RQSubHr = GfuncVal / ( K_Ground_Factor );
							SumQnSubHourly += VerticalGlhe( GlheNum ).QnSubHr( I ) * RQSubHr;
						}
						goto SUBHRLY_LOOP_exit;
					}
					//PrevTimeSteps(I+1) This is "I+1" because PrevTimeSteps(1) = CurrentTimestep
					XI = std::log( ( CurrentSimTime - PrevTimeSteps( I + 1 ) ) / ( TimeSS_Factor ) );
					INTERP( GlheNum, XI, GfuncVal );
					RQSubHr = GfuncVal / ( K_Ground_Factor );
					SumQnSubHourly += ( VerticalGlhe( GlheNum ).QnSubHr( I ) - VerticalGlhe( GlheNum ).QnSubHr( I + 1 ) ) * RQSubHr;
					SUBHRLY_LOOP_loop: ;
				}
				SUBHRLY_LOOP_exit: ;

				// Calculate the Hourly Superposition

				HourlyLimit = int( CurrentSimTime );
				SumQnHourly = 0.0;
				HOURLY_LOOP: for ( I = SubAGG + 1; I <= HourlyLimit; ++I ) {
					if ( I == HourlyLimit ) {
						XI = std::log( CurrentSimTime / ( TimeSS_Factor ) );
						INTERP( GlheNum, XI, GfuncVal );
						RQHour = GfuncVal / ( K_Ground_Factor );
						SumQnHourly += VerticalGlhe( GlheNum ).QnHr( I ) * RQHour;
						goto HOURLY_LOOP_exit;
					}
					XI = std::log( ( CurrentSimTime - int( CurrentSimTime ) + I ) / ( TimeSS_Factor ) );
					INTERP( GlheNum, XI, GfuncVal );
					RQHour = GfuncVal / ( K_Ground_Factor );
					SumQnHourly += ( VerticalGlhe( GlheNum ).QnHr( I ) - VerticalGlhe( GlheNum ).QnHr( I + 1 ) ) * RQHour;
					HOURLY_LOOP_loop: ;
				}
				HOURLY_LOOP_exit: ;

				// Find the total Sum of the Temperature difference due to all load blocks
				SumTotal = SumQnSubHourly + SumQnHourly;

				//Calulate the subhourly temperature due the Last Time steps Load
				XI = std::log( ( CurrentSimTime - PrevTimeSteps( 2 ) ) / ( TimeSS_Factor ) );
				INTERP( GlheNum, XI, GfuncVal );
				RQSubHr = GfuncVal / ( K_Ground_Factor );

				if ( MDotActual <= 0.0 ) {
					tmpQnSubHourly = 0.0;
					FluidAveTemp = Tground - SumTotal; // Q(N)*RB = 0
					ToutNew = GlheInletTemp;
				} else {
					//Dr.Spitler's Explicit set of equations to calculate the New Outlet Temperature of the U-Tube
					C0 = RQSubHr;
					C1 = Tground - ( SumTotal - VerticalGlhe( GlheNum ).QnSubHr( 1 ) * RQSubHr );
					C2 = BholeLength * NumBholes / ( 2.0 * MDotActual * Cp_Fluid );
					C3 = MDotActual * Cp_Fluid / ( BholeLength * NumBholes );
					tmpQnSubHourly = ( C1 - GlheInletTemp ) / ( ResistanceBhole + C0 - C2 + ( 1 / C3 ) );
					FluidAveTemp = C1 - ( C0 + ResistanceBhole ) * tmpQnSubHourly;
					ToutNew = C1 + ( C2 - C0 - ResistanceBhole ) * tmpQnSubHourly;
				}

			} else { // Monthly Aggregation and super position

				NumOfMonths = ( CurrentSimTime + 1 ) / HrsPerMonth;

				if ( CurrentSimTime < ( ( NumOfMonths ) * HrsPerMonth ) + AGG + SubAGG ) {
					CurrentMonth = NumOfMonths - 1;
				} else {
					CurrentMonth = NumOfMonths;
				}

				//monthly superposition
				SumQnMonthly = 0.0;
				SUMMONTHLY: for ( I = 1; I <= CurrentMonth; ++I ) {
					if ( I == 1 ) {
						XI = std::log( CurrentSimTime / ( TimeSS_Factor ) );
						INTERP( GlheNum, XI, GfuncVal );
						RQMonth = GfuncVal / ( K_Ground_Factor );
						SumQnMonthly += VerticalGlhe( GlheNum ).QnMonthlyAgg( I ) * RQMonth;
						goto SUMMONTHLY_loop;
					}
					XI = std::log( ( CurrentSimTime - ( I - 1 ) * HrsPerMonth ) / ( TimeSS_Factor ) );
					INTERP( GlheNum, XI, GfuncVal );
					RQMonth = GfuncVal / ( K_Ground_Factor );
					SumQnMonthly += ( VerticalGlhe( GlheNum ).QnMonthlyAgg( I ) - VerticalGlhe( GlheNum ).QnMonthlyAgg( I - 1 ) ) * RQMonth;
					SUMMONTHLY_loop: ;
				}
				SUMMONTHLY_exit: ;

				// Hourly Supr position
				HourlyLimit = int( CurrentSimTime - CurrentMonth * HrsPerMonth );
				SumQnHourly = 0.0;
				HOURLYLOOP: for ( I = 1 + SubAGG; I <= HourlyLimit; ++I ) {
					if ( I == HourlyLimit ) {
						XI = std::log( ( CurrentSimTime - int( CurrentSimTime ) + I ) / ( TimeSS_Factor ) );
						INTERP( GlheNum, XI, GfuncVal );
						RQHour = GfuncVal / ( K_Ground_Factor );
						SumQnHourly += ( VerticalGlhe( GlheNum ).QnHr( I ) - VerticalGlhe( GlheNum ).QnMonthlyAgg( CurrentMonth ) ) * RQHour;
						goto HOURLYLOOP_exit;
					}
					XI = std::log( ( CurrentSimTime - int( CurrentSimTime ) + I ) / ( TimeSS_Factor ) );
					INTERP( GlheNum, XI, GfuncVal );
					RQHour = GfuncVal / ( K_Ground_Factor );
					SumQnHourly += ( VerticalGlhe( GlheNum ).QnHr( I ) - VerticalGlhe( GlheNum ).QnHr( I + 1 ) ) * RQHour;
					HOURLYLOOP_loop: ;
				}
				HOURLYLOOP_exit: ;

				// Subhourly Superposition
				SubHourlyLimit = N - VerticalGlhe( GlheNum ).LastHourN( SubAGG + 1 );
				SumQnSubHourly = 0.0;
				SUBHRLOOP: for ( I = 1; I <= SubHourlyLimit; ++I ) {
					if ( I == SubHourlyLimit ) {
						XI = std::log( ( CurrentSimTime - PrevTimeSteps( I + 1 ) ) / ( TimeSS_Factor ) );
						INTERP( GlheNum, XI, GfuncVal );
						RQSubHr = GfuncVal / ( K_Ground_Factor );
						SumQnSubHourly += ( VerticalGlhe( GlheNum ).QnSubHr( I ) - VerticalGlhe( GlheNum ).QnHr( SubAGG + 1 ) ) * RQSubHr;
						goto SUBHRLOOP_exit;
					}
					XI = std::log( ( CurrentSimTime - PrevTimeSteps( I + 1 ) ) / ( TimeSS_Factor ) );
					INTERP( GlheNum, XI, GfuncVal );
					RQSubHr = GfuncVal / ( K_Ground_Factor );
					SumQnSubHourly += ( VerticalGlhe( GlheNum ).QnSubHr( I ) - VerticalGlhe( GlheNum ).QnSubHr( I + 1 ) ) * RQSubHr;
					SUBHRLOOP_loop: ;
				}
				SUBHRLOOP_exit: ;

				SumTotal = SumQnMonthly + SumQnHourly + SumQnSubHourly;

				//Calulate the subhourly temperature due the Last Time steps Load

				XI = std::log( ( CurrentSimTime - PrevTimeSteps( 2 ) ) / ( TimeSS_Factor ) );
				INTERP( GlheNum, XI, GfuncVal );
				RQSubHr = GfuncVal / ( K_Ground_Factor );

				if ( MDotActual <= 0.0 ) {
					tmpQnSubHourly = 0.0;
					FluidAveTemp = Tground - SumTotal; // Q(N)*RB = 0
					ToutNew = GlheInletTemp;
				} else {
					// Explicit set of equations to calculate the New Outlet Temperature of the U-Tube
					C0 = RQSubHr;
					C1 = Tground - ( SumTotal - VerticalGlhe( GlheNum ).QnSubHr( 1 ) * RQSubHr );
					C2 = BholeLength * NumBholes / ( 2 * MDotActual * Cp_Fluid );
					C3 = MDotActual * Cp_Fluid / ( BholeLength * NumBholes );
					tmpQnSubHourly = ( C1 - GlheInletTemp ) / ( ResistanceBhole + C0 - C2 + ( 1 / C3 ) );
					FluidAveTemp = C1 - ( C0 + ResistanceBhole ) * tmpQnSubHourly;
					ToutNew = C1 + ( C2 - C0 - ResistanceBhole ) * tmpQnSubHourly;
				}
			} //  end of AGG OR NO AGG
		} // end of N  = 1 branch
		GlheBoreholeTemp = Tground - SumTotal; //Autodesk:Uninit SumTotal could have been uninitialized here
		//Load the QnSubHourly Array with a new value at end of every timestep

		//Load the report vars
		LastQnSubHr( GlheNum ) = tmpQnSubHourly;
		GlheOutletTemp = ToutNew;
		QGlhe = tmpQnSubHourly;
		GlheAveFluidTemp = FluidAveTemp;
		GlheRB = ResistanceBhole;
		GlheMassFlowRate = MDotActual;

	}

	//******************************************************************************

	void
	CalcAggregateLoad( int const GlheNum )
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
		static FArray1D_int PrevHour; // Saved Var to store the previous hour
		//    LOGICAL,SAVE :: Allocated = .FALSE.
		static bool MyEnvrnFlag( true );

		if ( MyEnvrnFlag && BeginEnvrnFlag ) {
			//    IF(.not.Allocated)THEN
			//      ALLOCATE(PrevHour(NumVerticalGlhes))
			if ( ! allocated( PrevHour ) ) PrevHour.allocate( NumVerticalGlhes );
			//      Allocated = .TRUE.
			MyEnvrnFlag = false;
			PrevHour = 1;
		}
		if ( CurrentSimTime <= 0.0 ) return;

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		//FOR EVERY HOUR UPDATE THE HOURLY QN QnHr(J)
		//THIS IS DONE BY AGGREGATING THE SUBHOURLY QN FROM THE PREVIOUS HOUR TO UNTIL THE CURRNET HOUR
		//AND STORING IT IN  VerticalGlhe(GlheNum)%QnHr(J)

		//SUBHOURLY Qn IS NOT AGGREGATED . IT IS THE BASIC LOAD
		if ( PrevHour( GlheNum ) != LocHourOfDay ) {
			SumQnHr = 0.0;
			for ( J = 1; J <= ( N - VerticalGlhe( GlheNum ).LastHourN( 1 ) ); ++J ) { // Check during debugging if we need a +1
				SumQnHr += VerticalGlhe( GlheNum ).QnSubHr( J ) * std::abs( PrevTimeSteps( J ) - PrevTimeSteps( J + 1 ) );
			}
			SumQnHr /= std::abs( PrevTimeSteps( 1 ) - PrevTimeSteps( J ) );
			VerticalGlhe( GlheNum ).QnHr = eoshift( VerticalGlhe( GlheNum ).QnHr, -1, SumQnHr );
			VerticalGlhe( GlheNum ).LastHourN = eoshift( VerticalGlhe( GlheNum ).LastHourN, -1, N );
		}

		//CHECK IF A MONTH PASSES...
		if ( mod( ( ( LocDayOfSim - 1 ) * HrsPerDay + ( LocHourOfDay ) ), HrsPerMonth ) == 0 && PrevHour( GlheNum ) != LocHourOfDay ) {
			MonthNum = ( LocDayOfSim * HrsPerDay + LocHourOfDay ) / HrsPerMonth;
			SumQnMonth = 0.0;
			for ( J = 1; J <= int( HrsPerMonth ); ++J ) {
				SumQnMonth += VerticalGlhe( GlheNum ).QnHr( J );
			}
			SumQnMonth /= HrsPerMonth;
			VerticalGlhe( GlheNum ).QnMonthlyAgg( MonthNum ) = SumQnMonth;
		}
		if ( PrevHour( GlheNum ) != LocHourOfDay ) {
			PrevHour( GlheNum ) = LocHourOfDay;
		}

	}

	//******************************************************************************

	void
	GetGroundheatExchangerInput()
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
		int GlheNum;
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
		NumVerticalGlhes = GetNumObjectsFound( cCurrentModuleObject );

		Allocated = false;

		if ( NumVerticalGlhes <= 0 ) {
			ShowSevereError( "No " + cCurrentModuleObject + " equipment found in input file" );
			ErrorsFound = true;
		}

		VerticalGlhe.allocate( NumVerticalGlhes );

		VerticalGlheReport.allocate( NumVerticalGlhes );
		CheckEquipName.allocate( NumVerticalGlhes );
		CheckEquipName = true;

		for ( GlheNum = 1; GlheNum <= NumVerticalGlhes; ++GlheNum ) {
			GetObjectItem( cCurrentModuleObject, GlheNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;

			//get object name
			VerifyName( cAlphaArgs( 1 ), VerticalGlhe.Name(), GlheNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VerticalGlhe( GlheNum ).Name = cAlphaArgs( 1 );

			//get inlet node num
			VerticalGlhe( GlheNum ).GlheInletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			//get outlet node num
			VerticalGlhe( GlheNum ).GlheOutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			VerticalGlhe( GlheNum ).Available = true;
			VerticalGlhe( GlheNum ).ON = true;

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Condenser Water Nodes" );

			//load borehole data
			VerticalGlhe( GlheNum ).DesignFlow = rNumericArgs( 1 );
			RegisterPlantCompDesignFlow( VerticalGlhe( GlheNum ).GlheInletNodeNum, VerticalGlhe( GlheNum ).DesignFlow );

			VerticalGlhe( GlheNum ).NumBoreholes = rNumericArgs( 2 );
			VerticalGlhe( GlheNum ).BoreholeLength = rNumericArgs( 3 );
			VerticalGlhe( GlheNum ).BoreholeRadius = rNumericArgs( 4 );
			VerticalGlhe( GlheNum ).KGround = rNumericArgs( 5 );
			VerticalGlhe( GlheNum ).CpRhoGround = rNumericArgs( 6 );
			VerticalGlhe( GlheNum ).TempGround = rNumericArgs( 7 );
			VerticalGlhe( GlheNum ).MaxGlheFlowRate = rNumericArgs( 8 );
			VerticalGlhe( GlheNum ).KGrout = rNumericArgs( 9 );
			VerticalGlhe( GlheNum ).KPipe = rNumericArgs( 10 );
			VerticalGlhe( GlheNum ).PipeOutDia = rNumericArgs( 11 );
			VerticalGlhe( GlheNum ).UtubeDist = rNumericArgs( 12 );
			VerticalGlhe( GlheNum ).PipeThick = rNumericArgs( 13 );
			VerticalGlhe( GlheNum ).MaxSimYears = rNumericArgs( 14 );
			VerticalGlhe( GlheNum ).gReferenceRatio = rNumericArgs( 15 );

			//   Not many checks
			if ( VerticalGlhe( GlheNum ).PipeThick >= VerticalGlhe( GlheNum ).PipeOutDia / 2.0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + VerticalGlhe( GlheNum ).Name + "\", invalid value in field." );
				ShowContinueError( "..." + cNumericFieldNames( 13 ) + "=[" + RoundSigDigits( VerticalGlhe( GlheNum ).PipeThick, 3 ) + "]." );
				ShowContinueError( "..." + cNumericFieldNames( 11 ) + "=[" + RoundSigDigits( VerticalGlhe( GlheNum ).PipeOutDia, 3 ) + "]." );
				ShowContinueError( "...Radius will be <=0." );
				ErrorsFound = true;
			}

			if ( VerticalGlhe( GlheNum ).MaxSimYears < MaxNumberSimYears ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + VerticalGlhe( GlheNum ).Name + "\", invalid value in field." );
				ShowContinueError( "..." + cNumericFieldNames( 14 ) + " less than RunPeriod Request" );
				ShowContinueError( "Requested input=" + TrimSigDigits( VerticalGlhe( GlheNum ).MaxSimYears ) + " will be set to " + TrimSigDigits( MaxNumberSimYears ) );
				VerticalGlhe( GlheNum ).MaxSimYears = MaxNumberSimYears;
			}

			// Get Gfunction data
			VerticalGlhe( GlheNum ).NPairs = rNumericArgs( 16 );
			VerticalGlhe( GlheNum ).SubAGG = 15;
			VerticalGlhe( GlheNum ).AGG = 192;

			// Allocation of all the dynamic arrays
			VerticalGlhe( GlheNum ).LNTTS.allocate( VerticalGlhe( GlheNum ).NPairs );
			VerticalGlhe( GlheNum ).LNTTS = 0.0;
			VerticalGlhe( GlheNum ).GFNC.allocate( VerticalGlhe( GlheNum ).NPairs );
			VerticalGlhe( GlheNum ).GFNC = 0.0;
			VerticalGlhe( GlheNum ).QnMonthlyAgg.allocate( VerticalGlhe( GlheNum ).MaxSimYears * 12 );
			VerticalGlhe( GlheNum ).QnMonthlyAgg = 0.0;
			VerticalGlhe( GlheNum ).QnHr.allocate( 730 + VerticalGlhe( GlheNum ).AGG + VerticalGlhe( GlheNum ).SubAGG );
			VerticalGlhe( GlheNum ).QnHr = 0.0;
			VerticalGlhe( GlheNum ).QnSubHr.allocate( ( VerticalGlhe( GlheNum ).SubAGG + 1 ) * MaxTSinHr + 1 );
			VerticalGlhe( GlheNum ).QnSubHr = 0.0;
			VerticalGlhe( GlheNum ).LastHourN.allocate( VerticalGlhe( GlheNum ).SubAGG + 1 );
			VerticalGlhe( GlheNum ).LastHourN = 0;

			if ( ! Allocated ) {
				PrevTimeSteps.allocate( ( VerticalGlhe( GlheNum ).SubAGG + 1 ) * MaxTSinHr + 1 );
				PrevTimeSteps = 0.0;
				Allocated = true;
			}

			IndexNum = 17;
			for ( PairNum = 1; PairNum <= VerticalGlhe( GlheNum ).NPairs; ++PairNum ) {
				VerticalGlhe( GlheNum ).LNTTS( PairNum ) = rNumericArgs( IndexNum );
				VerticalGlhe( GlheNum ).GFNC( PairNum ) = rNumericArgs( IndexNum + 1 );
				IndexNum += 2;
			}
			//Check for Errors
			if ( ErrorsFound ) {
				ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
			}
		}

		//Set up report variables
		for ( GlheNum = 1; GlheNum <= NumVerticalGlhes; ++GlheNum ) {
			SetupOutputVariable( "Ground Heat Exchanger Average Borehole Temperature [C]", VerticalGlheReport( GlheNum ).GlheBoreholeTemp, "System", "Average", VerticalGlhe( GlheNum ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Heat Transfer Rate [W]", VerticalGlheReport( GlheNum ).QGlhe, "System", "Average", VerticalGlhe( GlheNum ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Inlet Temperature [C]", VerticalGlheReport( GlheNum ).GlheInletTemp, "System", "Average", VerticalGlhe( GlheNum ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Outlet Temperature [C]", VerticalGlheReport( GlheNum ).GlheOutletTemp, "System", "Average", VerticalGlhe( GlheNum ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Mass Flow Rate [kg/s]", VerticalGlheReport( GlheNum ).GlheMassFlowRate, "System", "Average", VerticalGlhe( GlheNum ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Average Fluid Temperature [C]", VerticalGlheReport( GlheNum ).GlheAveFluidTemp, "System", "Average", VerticalGlhe( GlheNum ).Name );
		}

	}

	//******************************************************************************

	void
	BoreholeResistance(
		int const GlheNum,
		Real64 & ResistanceBhole
	)
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
		int NumBholes; // number of boreholes
		Real64 BholeLength;
		Real64 BholeRadius;
		Real64 K_Ground;
		Real64 Cp_Ground;
		Real64 Cp_Fluid;
		Real64 Tground;
		Real64 K_Grout;
		Real64 K_Fluid;
		Real64 K_Pipe;
		Real64 FluidDensity;
		Real64 FluidViscosity;
		Real64 PipeOuterDia;
		Real64 PipeInnerDia;
		Real64 DistUtube;
		Real64 ThickPipe;
		Real64 BholeMdot;
		Real64 PipeOuterRad;
		Real64 PipeInnerRad;
		Real64 NusseltNum;
		Real64 ReynoldsNum;
		Real64 PrandlNum;
		Real64 hci;
		Real64 Rcond;
		Real64 Rconv;
		Real64 Rgrout;
		Real64 B0; // grout resistance curve fit coefficients
		Real64 B1;
		Real64 MaxDistance;
		Real64 DistanceRatio;

		//assign local variables
		NumBholes = VerticalGlhe( GlheNum ).NumBoreholes;
		BholeLength = VerticalGlhe( GlheNum ).BoreholeLength;
		BholeRadius = VerticalGlhe( GlheNum ).BoreholeRadius;
		K_Ground = VerticalGlhe( GlheNum ).KGround;
		Cp_Ground = VerticalGlhe( GlheNum ).CpRhoGround;

		Cp_Fluid = GetSpecificHeatGlycol( PlantLoop( VerticalGlhe( GlheNum ).LoopNum ).FluidName, GlheInletTemp, PlantLoop( VerticalGlhe( GlheNum ).LoopNum ).FluidIndex, RoutineName );

		Tground = VerticalGlhe( GlheNum ).TempGround;
		K_Grout = VerticalGlhe( GlheNum ).KGrout;
		K_Pipe = VerticalGlhe( GlheNum ).KPipe;
		K_Fluid = GetConductivityGlycol( PlantLoop( VerticalGlhe( GlheNum ).LoopNum ).FluidName, GlheInletTemp, PlantLoop( VerticalGlhe( GlheNum ).LoopNum ).FluidIndex, RoutineName );
		FluidDensity = GetDensityGlycol( PlantLoop( VerticalGlhe( GlheNum ).LoopNum ).FluidName, GlheInletTemp, PlantLoop( VerticalGlhe( GlheNum ).LoopNum ).FluidIndex, RoutineName );

		FluidViscosity = GetViscosityGlycol( PlantLoop( VerticalGlhe( GlheNum ).LoopNum ).FluidName, GlheInletTemp, PlantLoop( VerticalGlhe( GlheNum ).LoopNum ).FluidIndex, RoutineName );

		PipeOuterDia = VerticalGlhe( GlheNum ).PipeOutDia;
		DistUtube = VerticalGlhe( GlheNum ).UtubeDist;
		ThickPipe = VerticalGlhe( GlheNum ).PipeThick;

		//calculate mass flow rate
		BholeMdot = GlheMassFlowRate / NumBholes; //VerticalGlhe(GlheNum)%DesignFlow*FluidDensity /NumBholes

		PipeOuterRad = PipeOuterDia / 2.0;
		PipeInnerRad = PipeOuterRad - ThickPipe;
		PipeInnerDia = 2.0 * PipeInnerRad;
		//Re=Rho*V*D/Mu
		ReynoldsNum = FluidDensity * PipeInnerDia * ( BholeMdot / FluidDensity / ( Pi * pow_2( PipeInnerRad ) ) ) / FluidViscosity;
		PrandlNum = ( Cp_Fluid * FluidViscosity ) / ( K_Fluid );
		//   Convection Resistance
		NusseltNum = 0.023 * std::pow( ReynoldsNum, 0.8 ) * std::pow( PrandlNum, 0.35 );
		hci = NusseltNum * K_Fluid / PipeInnerDia;
		if ( BholeMdot == 0.0 ) {
			Rconv = 0.0;
		} else {
			Rconv = 1.0 / ( 2.0 * Pi * PipeInnerDia * hci );
		}

		//   Conduction Resistance
		Rcond = std::log( PipeOuterRad / PipeInnerRad ) / ( 2.0 * Pi * K_Pipe ) / 2.0; // pipe in parallel so /2

		//   Resistance Due to the grout.
		MaxDistance = 2.0 * BholeRadius - ( 2.0 * PipeOuterDia );
		DistanceRatio = DistUtube / MaxDistance;

		if ( DistanceRatio >= 0.0 && DistanceRatio <= 0.25 ) {
			B0 = 14.450872;
			B1 = -0.8176;
		} else if ( DistanceRatio > 0.25 && DistanceRatio < 0.5 ) {
			B0 = 20.100377;
			B1 = -0.94467;
		} else if ( DistanceRatio >= 0.5 && DistanceRatio <= 0.75 ) {
			B0 = 17.44268;
			B1 = -0.605154;
		} else {
			B0 = 21.90587;
			B1 = -0.3796;
		}

		Rgrout = 1.0 / ( K_Grout * ( B0 * std::pow( BholeRadius / PipeOuterRad, B1 ) ) );
		ResistanceBhole = Rcond + Rconv + Rgrout;
	}

	//******************************************************************************

	void
	INTERP(
		int const GlheNum, // Ground loop heat exchanger ID number
		Real64 const LnTTsVal, // The value of LN(t/TimeSS) that a g-function
		Real64 & GfuncVal // The value of the g-function at LnTTsVal; found by
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
		int NumPairs;
		Real64 RATIO;
		Real64 ReferenceRatio;

		//Binary Search Algorithms Variables
		// REFERENCE      :  DATA STRUCTURES AND ALGORITHM ANALYSIS IN C BY MARK ALLEN WEISS
		int Mid;
		int Low;
		int High;
		bool Found;

		NumPairs = VerticalGlhe( GlheNum ).NPairs;
		RATIO = VerticalGlhe( GlheNum ).BoreholeRadius / VerticalGlhe( GlheNum ).BoreholeLength;
		ReferenceRatio = VerticalGlhe( GlheNum ).gReferenceRatio;

		// The following IF loop determines the g-function for the case
		// when LnTTsVal is less than the first element of the LnTTs array.
		// In this case, the g-function must be found by extrapolation.

		if ( LnTTsVal <= VerticalGlhe( GlheNum ).LNTTS( 1 ) ) {
			GfuncVal = ( ( LnTTsVal - VerticalGlhe( GlheNum ).LNTTS( 1 ) ) / ( VerticalGlhe( GlheNum ).LNTTS( 2 ) - VerticalGlhe( GlheNum ).LNTTS( 1 ) ) ) * ( VerticalGlhe( GlheNum ).GFNC( 2 ) - VerticalGlhe( GlheNum ).GFNC( 1 ) ) + VerticalGlhe( GlheNum ).GFNC( 1 );

			// The following IF statement determines the condition of the ratio
			// between the borehole radius and the active borehole length.
			// If RATIO does not equal 0.0005 then a correction factor for
			// the g-function must be used.

			if ( RATIO != ReferenceRatio ) {
				GfuncVal -= std::log( VerticalGlhe( GlheNum ).BoreholeRadius / ( VerticalGlhe( GlheNum ).BoreholeLength * ReferenceRatio ) );
			}

			return;
		}

		// The following IF loop determines the g-function for the case
		// when LnTTsVal is greater than the last element of the LnTTs array.
		// In this case, the g-function must be found by extrapolation.

		if ( LnTTsVal > VerticalGlhe( GlheNum ).LNTTS( NumPairs ) ) {
			GfuncVal = ( ( LnTTsVal - VerticalGlhe( GlheNum ).LNTTS( NumPairs ) ) / ( VerticalGlhe( GlheNum ).LNTTS( NumPairs - 1 ) - VerticalGlhe( GlheNum ).LNTTS( NumPairs ) ) ) * ( VerticalGlhe( GlheNum ).GFNC( NumPairs - 1 ) - VerticalGlhe( GlheNum ).GFNC( NumPairs ) ) + VerticalGlhe( GlheNum ).GFNC( NumPairs );

			// Apply correction factor if necessary
			if ( RATIO != ReferenceRatio ) {
				GfuncVal -= std::log( VerticalGlhe( GlheNum ).BoreholeRadius / ( VerticalGlhe( GlheNum ).BoreholeLength * ReferenceRatio ) );
			}

			return;
		}

		// The following DO loop is for the case when LnTTsVal falls within
		// the first and last elements of the LnTTs array, or is identically
		// equal to one of the LnTTs elements.  In this case the g-function
		// must be found by interpolation.
		// USING BINARY SEARCH TO FIND THE ELEMENET
		Found = false;
		Low = 1;
		High = NumPairs;
		LOOP: while ( Low <= High ) {
			Mid = ( Low + High ) / 2;
			if ( VerticalGlhe( GlheNum ).LNTTS( Mid ) < LnTTsVal ) {
				Low = Mid + 1;
			} else {
				if ( VerticalGlhe( GlheNum ).LNTTS( Mid ) > LnTTsVal ) {
					High = Mid - 1;
				} else {
					Found = true;
					goto LOOP_exit;
				}
			}
			LOOP_loop: ;
		}
		LOOP_exit: ;
		//LnTTsVal is identical to one of the LnTTS array elements return
		//the GfuncVal after applying the correction
		if ( Found ) {
			GfuncVal = VerticalGlhe( GlheNum ).GFNC( Mid );
			// Apply correction factor if necessary
			if ( RATIO != ReferenceRatio ) {
				GfuncVal -= std::log( VerticalGlhe( GlheNum ).BoreholeRadius / ( VerticalGlhe( GlheNum ).BoreholeLength * ReferenceRatio ) );
			}
			return;
		}

		//LnTTsVal is in between any of the two LnTTS array elements find the
		// gfunction value by interplation and apply the correction and return
		if ( ! Found ) {
			if ( VerticalGlhe( GlheNum ).LNTTS( Mid ) < LnTTsVal ) ++Mid;

			GfuncVal = ( ( LnTTsVal - VerticalGlhe( GlheNum ).LNTTS( Mid ) ) / ( VerticalGlhe( GlheNum ).LNTTS( Mid - 1 ) - VerticalGlhe( GlheNum ).LNTTS( Mid ) ) ) * ( VerticalGlhe( GlheNum ).GFNC( Mid - 1 ) - VerticalGlhe( GlheNum ).GFNC( Mid ) ) + VerticalGlhe( GlheNum ).GFNC( Mid );

			// Apply correction factor if necessary
			if ( RATIO != ReferenceRatio ) {
				GfuncVal -= std::log( VerticalGlhe( GlheNum ).BoreholeRadius / ( VerticalGlhe( GlheNum ).BoreholeLength * ReferenceRatio ) );
			}
			return;
		}
	}

	//******************************************************************************

	void
	InitBoreholeHXSimVars(
		int const GlheNum,
		bool const RunFlag
	)
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
		static std::string const RoutineName( "InitBoreholeHXSimVars" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyEnvironFlag( true );
		Real64 FluidDensity;
		static FArray1D_bool MyFlag;
		static bool MyOneTimeFlag( true );
		static FArray1D_bool MyEnvrnFlag;
		bool errFlag;

		if ( MyOneTimeFlag ) {
			MyEnvrnFlag.allocate( NumVerticalGlhes );
			MyFlag.allocate( NumVerticalGlhes );
			MyOneTimeFlag = false;
			MyEnvrnFlag = true;
			MyFlag = true;
		}

		// Init more variables
		if ( MyFlag( GlheNum ) ) {
			// Locate the hx on the plant loops for later usage
			errFlag = false;
			ScanPlantLoopsForObject( VerticalGlhe( GlheNum ).Name, TypeOf_GrndHtExchgVertical, VerticalGlhe( GlheNum ).LoopNum, VerticalGlhe( GlheNum ).LoopSideNum, VerticalGlhe( GlheNum ).BranchNum, VerticalGlhe( GlheNum ).CompNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "InitBoreholeHXSimVars: Program terminated due to previous condition(s)." );
			}
			MyFlag( GlheNum ) = false;
		}

		if ( MyEnvrnFlag( GlheNum ) && BeginEnvrnFlag ) {
			MyEnvrnFlag( GlheNum ) = false;

			if ( ! allocated( LastQnSubHr ) ) LastQnSubHr.allocate( NumVerticalGlhes );
			FluidDensity = GetDensityGlycol( PlantLoop( VerticalGlhe( GlheNum ).LoopNum ).FluidName, 20.0, PlantLoop( VerticalGlhe( GlheNum ).LoopNum ).FluidIndex, RoutineName );
			VerticalGlhe( GlheNum ).DesignMassFlow = VerticalGlhe( GlheNum ).DesignFlow * FluidDensity;
			InitComponentNodes( 0.0, VerticalGlhe( GlheNum ).DesignMassFlow, VerticalGlhe( GlheNum ).GlheInletNodeNum, VerticalGlhe( GlheNum ).GlheOutletNodeNum, VerticalGlhe( GlheNum ).LoopNum, VerticalGlhe( GlheNum ).LoopSideNum, VerticalGlhe( GlheNum ).BranchNum, VerticalGlhe( GlheNum ).CompNum );

			LastQnSubHr = 0.0;
			Node( VerticalGlhe( GlheNum ).GlheInletNodeNum ).Temp = VerticalGlhe( GlheNum ).TempGround;
			Node( VerticalGlhe( GlheNum ).GlheOutletNodeNum ).Temp = VerticalGlhe( GlheNum ).TempGround;

			// zero out all history arrays

			VerticalGlhe( GlheNum ).QnHr = 0.0;
			VerticalGlhe( GlheNum ).QnMonthlyAgg = 0.0;
			VerticalGlhe( GlheNum ).QnSubHr = 0.0;
			VerticalGlhe( GlheNum ).LastHourN = 0;
			PrevTimeSteps = 0.0;
			CurrentSimTime = 0.0;
		}

		MDotActual = RegulateCondenserCompFlowReqOp( VerticalGlhe( GlheNum ).LoopNum, VerticalGlhe( GlheNum ).LoopSideNum, VerticalGlhe( GlheNum ).BranchNum, VerticalGlhe( GlheNum ).CompNum, VerticalGlhe( GlheNum ).DesignMassFlow );

		SetComponentFlowRate( MDotActual, VerticalGlhe( GlheNum ).GlheInletNodeNum, VerticalGlhe( GlheNum ).GlheOutletNodeNum, VerticalGlhe( GlheNum ).LoopNum, VerticalGlhe( GlheNum ).LoopSideNum, VerticalGlhe( GlheNum ).BranchNum, VerticalGlhe( GlheNum ).CompNum );

		// Resent local environment init flag
		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

	}

	//******************************************************************************

	void
	UpdateVerticalGroundHeatExchanger(
		bool const RunFlag,
		int const Num
	)
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
		static std::string const RoutineName( "UpdateVerticalGroundHeatExchanger" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		int GlheInletNode; // Inlet node number of the Glhe
		int GlheOutletNode; // Outlet node number of the Glhe
		Real64 GlhedeltaTemp; // ABS(Outlet temp -inlet temp)
		static int NumErrorCalls( 0 );
		Real64 DesignMassFlow;
		Real64 FluidDensity;

		//set node temperatures
		GlheInletNode = VerticalGlhe( Num ).GlheInletNodeNum;
		GlheOutletNode = VerticalGlhe( Num ).GlheOutletNodeNum;

		SafeCopyPlantNode( GlheInletNode, GlheOutletNode );

		Node( GlheOutletNode ).Temp = GlheOutletTemp;
		Node( GlheOutletNode ).Enthalpy = GlheOutletTemp * GetSpecificHeatGlycol( PlantLoop( VerticalGlhe( Num ).LoopNum ).FluidName, GlheOutletTemp, PlantLoop( VerticalGlhe( Num ).LoopNum ).FluidIndex, RoutineName );
		GlhedeltaTemp = std::abs( GlheOutletTemp - GlheInletTemp );
		VerticalGlheReport( Num ).GlheBoreholeTemp = GlheBoreholeTemp;
		VerticalGlheReport( Num ).GlheOutletTemp = GlheOutletTemp;
		// calc load from load per unit length.
		VerticalGlheReport( Num ).QGlhe = QGlhe * VerticalGlhe( Num ).BoreholeLength * VerticalGlhe( Num ).NumBoreholes;
		VerticalGlheReport( Num ).GlheInletTemp = GlheInletTemp;
		VerticalGlheReport( Num ).GlheMassFlowRate = GlheMassFlowRate;
		VerticalGlheReport( Num ).GlheAveFluidTemp = GlheAveFluidTemp;

		if ( GlhedeltaTemp > DeltaTempLimit && NumErrorCalls < NumVerticalGlhes && ! WarmupFlag ) {
			FluidDensity = GetDensityGlycol( PlantLoop( VerticalGlhe( Num ).LoopNum ).FluidName, GlheInletTemp, PlantLoop( VerticalGlhe( Num ).LoopNum ).FluidIndex, RoutineName );
			DesignMassFlow = VerticalGlhe( Num ).DesignFlow * FluidDensity;
			ShowWarningError( "Check GLHE design inputs & g-functions for consistency" );
			ShowContinueError( "For GroundHeatExchanger:Vertical " + VerticalGlhe( Num ).Name + "GLHE delta Temp > 100C." );
			ShowContinueError( "This can be encountered in cases where the GLHE mass flow rate is either significantly" );
			ShowContinueError( " lower than the design value, or cases where the mass flow rate rapidly changes." );
			ShowContinueError( "Glhe Current Flow Rate=" + TrimSigDigits( GlheMassFlowRate, 3 ) + "; Glhe Design Flow Rate=" + TrimSigDigits( DesignMassFlow, 3 ) );
			++NumErrorCalls;
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
