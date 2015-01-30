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
	//Real64 outletTemp( 0.0 ); // Outlet temperature of the fluid  [°C]
	//Real64 inletTemp( 0.0 ); // Inlet temperature of the fluid   [°C]
	//Real64 massFlowRate( 0.0 ); // Mass flowrate of the fluid       [Kg/s]
	//Real64 QGLHE( 0.0 ); // The normalized heat transfer rate[W/m]
	//Real64 GLHERB( 0.0 ); // [K per W/m] Just for Analyis will be removed later
	//Real64 aveFluidTemp( 0.0 ); // The average fluid temperature    [°C]
	//Real64 boreholeTemp( 0.0 ); // The average borehole tempreature [°C]
	int LocHourOfDay( 0 );
	int LocDayOfSim( 0 );
	Real64 mdotActual;

	FArray1D< Real64 > PrevTimeSteps; // This is used to store only the Last Few time step's time
	// to enable the calculation of the subhouly contribution..
	// Recommended size, the product of Minimum subhourly history required and
	// the maximum no of system time steps in an hour

	FArray1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE CondenserTowers

	// Object Data
	FArray1D< GLHEVert > VerticalGLHE; // dimension to number of machines

	// MODULE SUBROUTINES:

	//******************************************************************************

	// Functions

	void
	SimGroundHeatExchangers(
		std::string const & type,
		std::string const & Name,
		int & CompIndex,
		bool const runFlag,
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
		int GLHENum;

		//GET INPUT
		if ( GetInput ) {
			GetGroundHeatExchangerInput();
			GetInput = false;
		}

		// Find the correct Furnace
		if ( CompIndex == 0 ) {
			GLHENum = FindItemInList( Name, VerticalGLHE.Name(), numVerticalGLHEs );
			if ( GLHENum == 0 ) {
				ShowFatalError( "SimGroundHeatExchangers: Unit not found=" + Name );
			}
			CompIndex = GLHENum;
		} else {
			GLHENum = CompIndex;
			if ( GLHENum > numVerticalGLHEs || GLHENum < 1 ) {
				ShowFatalError( "SimGroundHeatExchangers:  Invalid CompIndex passed=" + TrimSigDigits( GLHENum ) + ", Number of Units=" + TrimSigDigits( numVerticalGLHEs ) + ", Entered Unit name=" + Name );
			}
			if ( CheckEquipName( GLHENum ) ) {
				if ( Name != VerticalGLHE( GLHENum ).Name ) {
					ShowFatalError( "SimGroundHeatExchangers: Invalid CompIndex passed=" + TrimSigDigits( numVerticalGLHEs ) + ", Unit name=" + Name + ", stored Unit Name for that index=" + VerticalGLHE( GLHENum ).Name );
				}
				CheckEquipName( GLHENum ) = false;
			}
		}

		auto & thisGLHE( VerticalGLHE( GLHENum ) );

		if ( InitLoopEquip ) {
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
		int NumBholes;
		Real64 FluidDensity;
		Real64 BholeLength;
		Real64 K_Ground;
		Real64 K_Ground_Factor;
		Real64 Cp_Fluid;
		Real64 Tground;
		Real64 resistanceBhole; // The thermal resistance of the borehole, (K per W/m]
		Real64 GfuncVal; // Interpolated G function value at a sub-hour
		static Real64 ToutNew( 19.375 );
		Real64 FluidAveTemp;
		Real64 groundDiffusivity;
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
		int inletNode; // Inlet node number of the GLHE
		int outletNode; // Outlet node number of the GLHE
		//    LOGICAL, SAVE      :: Allocated = .FALSE.
		int AGG;
		int SubAGG;
		int loopNum;
		int loopSideNum;

		//auto & thisGLHE( VerticalGLHE( GLHENum ) );

		//Autodesk:Uninit Initialize variables used uninitialized
		SumTotal = 0.0; //Autodesk:Uninit Force default initialization

		//set local glhe parameters

		NumBholes = this->numBoreholes;
		BholeLength = this->boreholeLength;
		inletNode = this->inletNodeNum;
		inletTemp = Node( inletNode ).Temp;
		Cp_Fluid = GetSpecificHeatGlycol( PlantLoop( this->loopNum ).FluidName, inletTemp, PlantLoop( this->loopNum ).FluidIndex, RoutineName );

		Tground = this->tempGround;
		FluidDensity = GetDensityGlycol( PlantLoop( this->loopNum ).FluidName, inletTemp, PlantLoop( this->loopNum ).FluidIndex, RoutineName );
		K_Ground = this->kGround;
		K_Ground_Factor = 2.0 * Pi * K_Ground;
		AGG = this->AGG;
		SubAGG = this->SubAGG;
		groundDiffusivity = this->kGround / this->cpRhoGround;

		// calculate annual time constant for ground conduction
		TimeSS = ( pow_2( this->boreholeLength ) / ( 9.0 * groundDiffusivity ) ) / SecInHour / 8760.0;
		TimeSS_Factor = TimeSS * 8760.0;

		outletNode = this->outletNodeNum;
		loopNum = this->loopNum;
		loopSideNum = this->loopSideNum;

		massFlowRate = mdotActual;

		if ( TriggerDesignDayReset && WarmupFlag ) UpdateCurSimTime = true;
		if ( DayOfSim == 1 && UpdateCurSimTime ) {
			currentSimTime = 0.0;
			PrevTimeSteps = 0.0;
			for ( I = 1; I <= numVerticalGLHEs; ++I ) {
				VerticalGLHE( I ).QnHr = 0.0;
				VerticalGLHE( I ).QnMonthlyAgg = 0.0;
				VerticalGLHE( I ).QnSubHr = 0.0;
				VerticalGLHE( I ).LastHourN = 1;
			}
			N = 1;
			UpdateCurSimTime = false;
			TriggerDesignDayReset = false;
		}

		currentSimTime = ( DayOfSim - 1 ) * 24 + HourOfDay - 1 + ( TimeStep - 1 ) * TimeStepZone + SysTimeElapsed; //+ TimeStepsys
		LocHourOfDay = mod( currentSimTime, hrsPerDay ) + 1;
		LocDayOfSim = currentSimTime / 24 + 1;

		if ( DayOfSim > 1 ) {
			UpdateCurSimTime = true;
		}

		if ( ! WarmupFlag ) {
			TriggerDesignDayReset = true;
		}

		if ( currentSimTime <= 0.0 ) {
			PrevTimeSteps = 0.0; // this resets history when rounding 24:00 hours during warmup avoids hard crash later
			outletTemp = inletTemp;
			massFlowRate = mdotActual;
			this->calcAggregateLoad(); //Just allocates and initializes prevHour array
			return;
		}

		// Store currentSimTime in PrevTimeSteps only if a time step occurs

		if ( PrevTimeSteps( 1 ) != currentSimTime ) {
			PrevTimeSteps = eoshift( PrevTimeSteps, -1, currentSimTime );
			++N;
		}

		if ( N != PrevN ) {
			PrevN = N;
			for ( I = 1; I <= numVerticalGLHEs; ++I ) {
				VerticalGLHE( I ).QnSubHr = eoshift( VerticalGLHE( I ).QnSubHr, -1, this->LastQnSubHr );
			}
		}

		this->calcAggregateLoad();

		// Update the borehole resistance each time
		this->BoreholeResistance();

		if ( N == 1 ) {
			if ( mdotActual <= 0.0 ) {
				tmpQnSubHourly = 0.0;
				FluidAveTemp = Tground;
				ToutNew = inletTemp;
			} else {
				XI = std::log( currentSimTime / ( TimeSS_Factor ) );
				GfuncVal = this->interpGFunc( XI );

				C_1 = ( BholeLength * NumBholes ) / ( 2.0 * mdotActual * Cp_Fluid );
				tmpQnSubHourly = ( Tground - inletTemp ) / ( GfuncVal / ( K_Ground_Factor ) + this->resistanceBhole + C_1 );
				FluidAveTemp = Tground - tmpQnSubHourly * this->resistanceBhole;
				ToutNew = Tground - tmpQnSubHourly * ( GfuncVal / ( K_Ground_Factor ) + this->resistanceBhole - C_1 );
			}
		} else {
			// no monthly super position
			if ( currentSimTime < ( hrsPerMonth + AGG + SubAGG ) ) {

				// Calculate the Sub Hourly Superposition
				SumQnSubHourly = 0.0;
				if ( int( currentSimTime ) < SubAGG ) {
					IndexN = int( currentSimTime ) + 1;
				} else {
					IndexN = SubAGG + 1;
				}
				SubHourlyLimit = N - this->LastHourN( IndexN ); //Check this when running simulation

				SUBHRLY_LOOP: for ( I = 1; I <= SubHourlyLimit; ++I ) {
					if ( I == SubHourlyLimit ) {
						if ( int( currentSimTime ) >= SubAGG ) {
							XI = std::log( ( currentSimTime - PrevTimeSteps( I + 1 ) ) / ( TimeSS_Factor ) );
							GfuncVal = this->interpGFunc( XI );
							RQSubHr = GfuncVal / ( K_Ground_Factor );
							SumQnSubHourly += ( this->QnSubHr( I ) - this->QnHr( IndexN ) ) * RQSubHr;
						} else {
							XI = std::log( ( currentSimTime - PrevTimeSteps( I + 1 ) ) / ( TimeSS_Factor ) );
							GfuncVal = this->interpGFunc( XI );
							RQSubHr = GfuncVal / ( K_Ground_Factor );
							SumQnSubHourly += this->QnSubHr( I ) * RQSubHr;
						}
						goto SUBHRLY_LOOP_exit;
					}
					//PrevTimeSteps(I+1) This is "I+1" because PrevTimeSteps(1) = CurrentTimestep
					XI = std::log( ( currentSimTime - PrevTimeSteps( I + 1 ) ) / ( TimeSS_Factor ) );
					GfuncVal = this->interpGFunc( XI );
					RQSubHr = GfuncVal / ( K_Ground_Factor );
					SumQnSubHourly += ( this->QnSubHr( I ) - this->QnSubHr( I + 1 ) ) * RQSubHr;
					SUBHRLY_LOOP_loop: ;
				}
				SUBHRLY_LOOP_exit: ;

				// Calculate the Hourly Superposition

				HourlyLimit = int( currentSimTime );
				SumQnHourly = 0.0;
				HOURLY_LOOP: for ( I = SubAGG + 1; I <= HourlyLimit; ++I ) {
					if ( I == HourlyLimit ) {
						XI = std::log( currentSimTime / ( TimeSS_Factor ) );
						GfuncVal = this->interpGFunc( XI );
						RQHour = GfuncVal / ( K_Ground_Factor );
						SumQnHourly += this->QnHr( I ) * RQHour;
						goto HOURLY_LOOP_exit;
					}
					XI = std::log( ( currentSimTime - int( currentSimTime ) + I ) / ( TimeSS_Factor ) );
					GfuncVal = this->interpGFunc( XI );
					RQHour = GfuncVal / ( K_Ground_Factor );
					SumQnHourly += ( this->QnHr( I ) - this->QnHr( I + 1 ) ) * RQHour;
					HOURLY_LOOP_loop: ;
				}
				HOURLY_LOOP_exit: ;

				// Find the total Sum of the Temperature difference due to all load blocks
				SumTotal = SumQnSubHourly + SumQnHourly;

				//Calulate the subhourly temperature due the Last Time steps Load
				XI = std::log( ( currentSimTime - PrevTimeSteps( 2 ) ) / ( TimeSS_Factor ) );
				GfuncVal = this->interpGFunc( XI );
				RQSubHr = GfuncVal / ( K_Ground_Factor );

				if ( mdotActual <= 0.0 ) {
					tmpQnSubHourly = 0.0;
					FluidAveTemp = Tground - SumTotal; // Q(N)*RB = 0
					ToutNew = inletTemp;
				} else {
					//Dr.Spitler's Explicit set of equations to calculate the New Outlet Temperature of the U-Tube
					C0 = RQSubHr;
					C1 = Tground - ( SumTotal - this->QnSubHr( 1 ) * RQSubHr );
					C2 = BholeLength * NumBholes / ( 2.0 * mdotActual * Cp_Fluid );
					C3 = mdotActual * Cp_Fluid / ( BholeLength * NumBholes );
					tmpQnSubHourly = ( C1 - inletTemp ) / ( this->resistanceBhole + C0 - C2 + ( 1 / C3 ) );
					FluidAveTemp = C1 - ( C0 + this->resistanceBhole ) * tmpQnSubHourly;
					ToutNew = C1 + ( C2 - C0 - this->resistanceBhole ) * tmpQnSubHourly;
				}

			} else { // Monthly Aggregation and super position

				NumOfMonths = ( currentSimTime + 1 ) / hrsPerMonth;

				if ( currentSimTime < ( ( NumOfMonths ) * hrsPerMonth ) + AGG + SubAGG ) {
					CurrentMonth = NumOfMonths - 1;
				} else {
					CurrentMonth = NumOfMonths;
				}

				//monthly superposition
				SumQnMonthly = 0.0;
				SUMMONTHLY: for ( I = 1; I <= CurrentMonth; ++I ) {
					if ( I == 1 ) {
						XI = std::log( currentSimTime / ( TimeSS_Factor ) );
						GfuncVal = this->interpGFunc( XI );
						RQMonth = GfuncVal / ( K_Ground_Factor );
						SumQnMonthly += this->QnMonthlyAgg( I ) * RQMonth;
						goto SUMMONTHLY_loop;
					}
					XI = std::log( ( currentSimTime - ( I - 1 ) * hrsPerMonth ) / ( TimeSS_Factor ) );
					GfuncVal = this->interpGFunc( XI );
					RQMonth = GfuncVal / ( K_Ground_Factor );
					SumQnMonthly += ( this->QnMonthlyAgg( I ) - this->QnMonthlyAgg( I - 1 ) ) * RQMonth;
					SUMMONTHLY_loop: ;
				}
				SUMMONTHLY_exit: ;

				// Hourly Supr position
				HourlyLimit = int( currentSimTime - CurrentMonth * hrsPerMonth );
				SumQnHourly = 0.0;
				HOURLYLOOP: for ( I = 1 + SubAGG; I <= HourlyLimit; ++I ) {
					if ( I == HourlyLimit ) {
						XI = std::log( ( currentSimTime - int( currentSimTime ) + I ) / ( TimeSS_Factor ) );
						GfuncVal = this->interpGFunc( XI );
						RQHour = GfuncVal / ( K_Ground_Factor );
						SumQnHourly += ( this->QnHr( I ) - this->QnMonthlyAgg( CurrentMonth ) ) * RQHour;
						goto HOURLYLOOP_exit;
					}
					XI = std::log( ( currentSimTime - int( currentSimTime ) + I ) / ( TimeSS_Factor ) );
					GfuncVal = this->interpGFunc( XI );
					RQHour = GfuncVal / ( K_Ground_Factor );
					SumQnHourly += ( this->QnHr( I ) - this->QnHr( I + 1 ) ) * RQHour;
					HOURLYLOOP_loop: ;
				}
				HOURLYLOOP_exit: ;

				// Subhourly Superposition
				SubHourlyLimit = N - this->LastHourN( SubAGG + 1 );
				SumQnSubHourly = 0.0;
				SUBHRLOOP: for ( I = 1; I <= SubHourlyLimit; ++I ) {
					if ( I == SubHourlyLimit ) {
						XI = std::log( ( currentSimTime - PrevTimeSteps( I + 1 ) ) / ( TimeSS_Factor ) );
						GfuncVal = this->interpGFunc( XI );
						RQSubHr = GfuncVal / ( K_Ground_Factor );
						SumQnSubHourly += ( this->QnSubHr( I ) - this->QnHr( SubAGG + 1 ) ) * RQSubHr;
						goto SUBHRLOOP_exit;
					}
					XI = std::log( ( currentSimTime - PrevTimeSteps( I + 1 ) ) / ( TimeSS_Factor ) );
					GfuncVal = this->interpGFunc( XI );
					RQSubHr = GfuncVal / ( K_Ground_Factor );
					SumQnSubHourly += ( this->QnSubHr( I ) - this->QnSubHr( I + 1 ) ) * RQSubHr;
					SUBHRLOOP_loop: ;
				}
				SUBHRLOOP_exit: ;

				SumTotal = SumQnMonthly + SumQnHourly + SumQnSubHourly;

				//Calulate the subhourly temperature due the Last Time steps Load

				XI = std::log( ( currentSimTime - PrevTimeSteps( 2 ) ) / ( TimeSS_Factor ) );
				GfuncVal = this->interpGFunc( XI );
				RQSubHr = GfuncVal / ( K_Ground_Factor );

				if ( mdotActual <= 0.0 ) {
					tmpQnSubHourly = 0.0;
					FluidAveTemp = Tground - SumTotal; // Q(N)*RB = 0
					ToutNew = inletTemp;
				} else {
					// Explicit set of equations to calculate the New Outlet Temperature of the U-Tube
					C0 = RQSubHr;
					C1 = Tground - ( SumTotal - this->QnSubHr( 1 ) * RQSubHr );
					C2 = BholeLength * NumBholes / ( 2 * mdotActual * Cp_Fluid );
					C3 = mdotActual * Cp_Fluid / ( BholeLength * NumBholes );
					tmpQnSubHourly = ( C1 - inletTemp ) / ( this->resistanceBhole + C0 - C2 + ( 1 / C3 ) );
					FluidAveTemp = C1 - ( C0 + this->resistanceBhole ) * tmpQnSubHourly;
					ToutNew = C1 + ( C2 - C0 - this->resistanceBhole ) * tmpQnSubHourly;
				}
			} //  end of AGG OR NO AGG
		} // end of N  = 1 branch
		boreholeTemp = Tground - SumTotal; //Autodesk:Uninit SumTotal could have been uninitialized here
		//Load the QnSubHourly Array with a new value at end of every timestep

		//Load the report vars
		this->LastQnSubHr = tmpQnSubHourly;
		this->outletTemp = ToutNew;
		this->QGLHE = tmpQnSubHourly;
		this->aveFluidTemp = FluidAveTemp;
		//this->GLHERB = resistanceBhole;
		this->massFlowRate = mdotActual;

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
		//AND STORING IT IN  VerticalGLHE(GLHENum)%QnHr(J)

		//SUBHOURLY Qn IS NOT AGGREGATED . IT IS THE BASIC LOAD
		if ( this->prevHour != LocHourOfDay ) {
			SumQnHr = 0.0;
			for ( J = 1; J <= ( N - this->LastHourN( 1 ) ); ++J ) { // Check during debugging if we need a +1
				SumQnHr += this->QnSubHr( J ) * std::abs( PrevTimeSteps( J ) - PrevTimeSteps( J + 1 ) );
			}
			SumQnHr /= std::abs( PrevTimeSteps( 1 ) - PrevTimeSteps( J ) );
			this->QnHr = eoshift( this->QnHr, -1, SumQnHr );
			this->LastHourN = eoshift( this->LastHourN, -1, N );
		}

		//CHECK IF A MONTH PASSES...
		if ( mod( ( ( LocDayOfSim - 1 ) * hrsPerDay + ( LocHourOfDay ) ), hrsPerMonth ) == 0 && this->prevHour != LocHourOfDay ) {
			MonthNum = ( LocDayOfSim * hrsPerDay + LocHourOfDay ) / hrsPerMonth;
			SumQnMonth = 0.0;
			for ( J = 1; J <= int( hrsPerMonth ); ++J ) {
				SumQnMonth += this->QnHr( J );
			}
			SumQnMonth /= hrsPerMonth;
			this->QnMonthlyAgg( MonthNum ) = SumQnMonth;
		}
		if ( this->prevHour != LocHourOfDay ) {
			this->prevHour = LocHourOfDay;
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

		VerticalGLHE.allocate( numVerticalGLHEs );

		CheckEquipName.dimension( numVerticalGLHEs, true );

		for ( GLHENum = 1; GLHENum <= numVerticalGLHEs; ++GLHENum ) {
			GetObjectItem( cCurrentModuleObject, GLHENum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;

			//get object name
			VerifyName( cAlphaArgs( 1 ), VerticalGLHE.Name(), GLHENum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			VerticalGLHE( GLHENum ).Name = cAlphaArgs( 1 );

			//get inlet node num
			VerticalGLHE( GLHENum ).inletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );

			//get outlet node num
			VerticalGLHE( GLHENum ).outletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			VerticalGLHE( GLHENum ).available = true;
			VerticalGLHE( GLHENum ).on = true;

			TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Condenser Water Nodes" );

			//load borehole data
			VerticalGLHE( GLHENum ).designFlow = rNumericArgs( 1 );
			RegisterPlantCompDesignFlow( VerticalGLHE( GLHENum ).inletNodeNum, VerticalGLHE( GLHENum ).designFlow );

			VerticalGLHE( GLHENum ).numBoreholes = rNumericArgs( 2 );
			VerticalGLHE( GLHENum ).boreholeLength = rNumericArgs( 3 );
			VerticalGLHE( GLHENum ).boreholeRadius = rNumericArgs( 4 );
			VerticalGLHE( GLHENum ).kGround = rNumericArgs( 5 );
			VerticalGLHE( GLHENum ).cpRhoGround = rNumericArgs( 6 );
			VerticalGLHE( GLHENum ).tempGround = rNumericArgs( 7 );
			VerticalGLHE( GLHENum ).kGrout = rNumericArgs( 8 );
			VerticalGLHE( GLHENum ).kPipe = rNumericArgs( 9 );
			VerticalGLHE( GLHENum ).pipeOutDia = rNumericArgs( 10 );
			VerticalGLHE( GLHENum ).UtubeDist = rNumericArgs( 11 );
			VerticalGLHE( GLHENum ).pipeThick = rNumericArgs( 12 );
			VerticalGLHE( GLHENum ).maxSimYears = rNumericArgs( 13 );
			VerticalGLHE( GLHENum ).gReferenceRatio = rNumericArgs( 14 );

			//   Not many checks
			if ( VerticalGLHE( GLHENum ).pipeThick >= VerticalGLHE( GLHENum ).pipeOutDia / 2.0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + VerticalGLHE( GLHENum ).Name + "\", invalid value in field." );
				ShowContinueError( "..." + cNumericFieldNames( 13 ) + "=[" + RoundSigDigits( VerticalGLHE( GLHENum ).pipeThick, 3 ) + "]." );
				ShowContinueError( "..." + cNumericFieldNames( 11 ) + "=[" + RoundSigDigits( VerticalGLHE( GLHENum ).pipeOutDia, 3 ) + "]." );
				ShowContinueError( "...Radius will be <=0." );
				ErrorsFound = true;
			}

			if ( VerticalGLHE( GLHENum ).maxSimYears < MaxNumberSimYears ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + VerticalGLHE( GLHENum ).Name + "\", invalid value in field." );
				ShowContinueError( "..." + cNumericFieldNames( 14 ) + " less than RunPeriod Request" );
				ShowContinueError( "Requested input=" + TrimSigDigits( VerticalGLHE( GLHENum ).maxSimYears ) + " will be set to " + TrimSigDigits( MaxNumberSimYears ) );
				VerticalGLHE( GLHENum ).maxSimYears = MaxNumberSimYears;
			}

			// Get Gfunction data
			VerticalGLHE( GLHENum ).NPairs = rNumericArgs( 15 );
			VerticalGLHE( GLHENum ).SubAGG = 15;
			VerticalGLHE( GLHENum ).AGG = 192;

			// Allocation of all the dynamic arrays
			VerticalGLHE( GLHENum ).LNTTS.allocate( VerticalGLHE( GLHENum ).NPairs );
			VerticalGLHE( GLHENum ).LNTTS = 0.0;
			VerticalGLHE( GLHENum ).GFNC.allocate( VerticalGLHE( GLHENum ).NPairs );
			VerticalGLHE( GLHENum ).GFNC = 0.0;
			VerticalGLHE( GLHENum ).QnMonthlyAgg.allocate( VerticalGLHE( GLHENum ).maxSimYears * 12 );
			VerticalGLHE( GLHENum ).QnMonthlyAgg = 0.0;
			VerticalGLHE( GLHENum ).QnHr.allocate( 730 + VerticalGLHE( GLHENum ).AGG + VerticalGLHE( GLHENum ).SubAGG );
			VerticalGLHE( GLHENum ).QnHr = 0.0;
			VerticalGLHE( GLHENum ).QnSubHr.allocate( ( VerticalGLHE( GLHENum ).SubAGG + 1 ) * maxTSinHr + 1 );
			VerticalGLHE( GLHENum ).QnSubHr = 0.0;
			VerticalGLHE( GLHENum ).LastHourN.allocate( VerticalGLHE( GLHENum ).SubAGG + 1 );
			VerticalGLHE( GLHENum ).LastHourN = 0;

			if ( ! Allocated ) {
				PrevTimeSteps.allocate( ( VerticalGLHE( GLHENum ).SubAGG + 1 ) * maxTSinHr + 1 );
				PrevTimeSteps = 0.0;
				Allocated = true;
			}

			IndexNum = 16;
			for ( PairNum = 1; PairNum <= VerticalGLHE( GLHENum ).NPairs; ++PairNum ) {
				VerticalGLHE( GLHENum ).LNTTS( PairNum ) = rNumericArgs( IndexNum );
				VerticalGLHE( GLHENum ).GFNC( PairNum ) = rNumericArgs( IndexNum + 1 );
				IndexNum += 2;
			}
			//Check for Errors
			if ( ErrorsFound ) {
				ShowFatalError( "Errors found in processing input for " + cCurrentModuleObject );
			}
		}

		//Set up report variables
		for ( GLHENum = 1; GLHENum <= numVerticalGLHEs; ++GLHENum ) {
			SetupOutputVariable( "Ground Heat Exchanger Average Borehole Temperature [C]", VerticalGLHE( GLHENum ).boreholeTemp, "System", "Average", VerticalGLHE( GLHENum ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Heat Transfer Rate [W]", VerticalGLHE( GLHENum ).QGLHE, "System", "Average", VerticalGLHE( GLHENum ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Inlet Temperature [C]", VerticalGLHE( GLHENum ).inletTemp, "System", "Average", VerticalGLHE( GLHENum ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Outlet Temperature [C]", VerticalGLHE( GLHENum ).outletTemp, "System", "Average", VerticalGLHE( GLHENum ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Mass Flow Rate [kg/s]", VerticalGLHE( GLHENum ).massFlowRate, "System", "Average", VerticalGLHE( GLHENum ).Name );
			SetupOutputVariable( "Ground Heat Exchanger Average Fluid Temperature [C]", VerticalGLHE( GLHENum ).aveFluidTemp, "System", "Average", VerticalGLHE( GLHENum ).Name );
		}

	}

	//******************************************************************************

	void
	GLHEVert::BoreholeResistance()
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
		BholeMdot = massFlowRate / numBoreholes; //VerticalGLHE(GLHENum)%designFlow*fluidDensity /numBoreholes

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
		//int NumPairs;
		Real64 RATIO;
		//Real64 referenceRatio;
		Real64 GfuncVal;

		//Binary Search Algorithms Variables
		// REFERENCE      :  DATA STRUCTURES AND ALGORITHM ANALYSIS IN C BY MARK ALLEN WEISS
		int Mid;
		int Low;
		int High;
		bool Found;

		//NumPairs = this->NPairs;
		RATIO = boreholeRadius / boreholeLength;
		//referenceRatio = this->gReferenceRatio;

		// The following IF loop determines the g-function for the case
		// when LnTTsVal is less than the first element of the LnTTs array.
		// In this case, the g-function must be found by extrapolation.

		if ( LnTTsVal <= LNTTS( 1 ) ) {
			GfuncVal = ( ( LnTTsVal - LNTTS( 1 ) ) / ( LNTTS( 2 ) - LNTTS( 1 ) ) ) * ( GFNC( 2 ) - GFNC( 1 ) ) + GFNC( 1 );

			// The following IF statement determines the condition of the ratio
			// between the borehole radius and the active borehole length.
			// If RATIO does not equal 0.0005 then a correction factor for
			// the g-function must be used.

			if ( RATIO != gReferenceRatio ) {
				GfuncVal -= std::log( boreholeRadius / ( boreholeLength * gReferenceRatio ) );
			}

			return GfuncVal;
		}

		// The following IF loop determines the g-function for the case
		// when LnTTsVal is greater than the last element of the LnTTs array.
		// In this case, the g-function must be found by extrapolation.

		if ( LnTTsVal > LNTTS( NPairs ) ) {
			GfuncVal = ( ( LnTTsVal - LNTTS( NPairs ) ) / ( LNTTS( NPairs - 1 ) - LNTTS( NPairs ) ) ) * ( GFNC( NPairs - 1 ) - GFNC( NPairs ) ) + GFNC( NPairs );

			// Apply correction factor if necessary
			if ( RATIO != gReferenceRatio ) {
				GfuncVal -= std::log( boreholeRadius / ( boreholeLength * gReferenceRatio ) );
			}

			return GfuncVal;
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
		//LnTTsVal is identical to one of the LnTTS array elements return GfuncVal
		//the GfuncVal after applying the correction
		if ( Found ) {
			GfuncVal = GFNC( Mid );
			// Apply correction factor if necessary
			if ( RATIO != gReferenceRatio ) {
				GfuncVal -= std::log( boreholeRadius / ( boreholeLength * gReferenceRatio ) );
			}
			return GfuncVal;
		}

		//LnTTsVal is in between any of the two LnTTS array elements find the
		// gfunction value by interplation and apply the correction and return GfuncVal
		else {
			if ( LNTTS( Mid ) < LnTTsVal ) ++Mid;

			GfuncVal = ( ( LnTTsVal - LNTTS( Mid ) ) / ( LNTTS( Mid - 1 ) - LNTTS( Mid ) ) ) * ( GFNC( Mid - 1 ) - GFNC( Mid ) ) + GFNC( Mid );

			// Apply correction factor if necessary
			if ( RATIO != gReferenceRatio ) {
				GfuncVal -= std::log( boreholeRadius / ( boreholeLength * gReferenceRatio ) );
			}
			return GfuncVal;
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
		Real64 FluidDensity;
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

			FluidDensity = GetDensityGlycol( PlantLoop( loopNum ).FluidName, 20.0, PlantLoop( loopNum ).FluidIndex, RoutineName );
			designMassFlow = designFlow * FluidDensity;
			InitComponentNodes( 0.0, designMassFlow, inletNodeNum, outletNodeNum, loopNum, loopSideNum, branchNum, compNum );

			LastQnSubHr = 0.0;
			Node( inletNodeNum ).Temp = tempGround;
			Node( outletNodeNum ).Temp = tempGround;

			// zero out all history arrays

			QnHr = 0.0;
			QnMonthlyAgg = 0.0;
			QnSubHr = 0.0;
			LastHourN = 0;
			PrevTimeSteps = 0.0;
			currentSimTime = 0.0;
		}

		mdotActual = RegulateCondenserCompFlowReqOp( loopNum, loopSideNum, branchNum, compNum, designMassFlow );

		SetComponentFlowRate( mdotActual, inletNodeNum, outletNodeNum, loopNum, loopSideNum, branchNum, compNum );

		// Resent local environment init flag
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
		static std::string const RoutineName( "UpdateVerticalGroundHeatExchanger" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		//int inletNode; // Inlet node number of the GLHE
		//int outletNode; // Outlet node number of the GLHE
		Real64 GLHEdeltaTemp; // ABS(Outlet temp -inlet temp)
		static int numErrorCalls( 0 );
		Real64 designMassFlow;
		Real64 FluidDensity;

		//set node temperatures
		//inletNode = this->inletNodeNum;
		//outletNode = this->outletNodeNum;

		SafeCopyPlantNode( inletNodeNum, outletNodeNum );

		Node( outletNodeNum ).Temp = outletTemp;
		Node( outletNodeNum ).Enthalpy = outletTemp * GetSpecificHeatGlycol( PlantLoop( loopNum ).FluidName, outletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
		GLHEdeltaTemp = std::abs( outletTemp - inletTemp );
		this->boreholeTemp = boreholeTemp;
		this->outletTemp = outletTemp;
		// calc load from load per unit length.
		this->QGLHE = this->QGLHE * this->boreholeLength * this->numBoreholes;
		this->inletTemp = inletTemp;
		this->massFlowRate = massFlowRate;
		this->aveFluidTemp = aveFluidTemp;

		if ( GLHEdeltaTemp > DeltaTempLimit && numErrorCalls < numVerticalGLHEs && ! WarmupFlag ) {
			FluidDensity = GetDensityGlycol( PlantLoop( loopNum ).FluidName, inletTemp, PlantLoop( loopNum ).FluidIndex, RoutineName );
			designMassFlow = this->designFlow * FluidDensity;
			ShowWarningError( "Check GLHE design inputs & g-functions for consistency" );
			ShowContinueError( "For GroundHeatExchanger:Vertical " + this->Name + "GLHE delta Temp > 100C." );
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
