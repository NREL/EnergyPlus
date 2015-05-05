#ifndef EconomicTariff_hh_INCLUDED
#define EconomicTariff_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace EconomicTariff {

	// Using/Aliasing

	// Data
	//ECONOMCIS:TARIFF enumerated lists

	extern int const kindUnknown;
	extern int const kindTariff;
	extern int const kindQualify;
	extern int const kindChargeSimple;
	extern int const kindChargeBlock;
	extern int const kindRatchet;
	extern int const kindVariable;
	extern int const kindComputation;
	extern int const kindCategory;
	extern int const kindNative;
	extern int const kindAssignCompute;

	extern int const conversionUSERDEF;
	extern int const conversionKWH;
	extern int const conversionTHERM;
	extern int const conversionMMBTU; // million btu
	extern int const conversionMJ;
	extern int const conversionKBTU;
	extern int const conversionMCF; // thousand cubic feet
	extern int const conversionCCF; // hundred cubic feet

	extern Array1D_string const convEneStrings;
	extern Array1D_string const convDemStrings;

	extern int const demandWindowQuarter;
	extern int const demandWindowHalf;
	extern int const demandWindowHour;
	extern int const demandWindowDay;
	extern int const demandWindowWeek;

	extern Array1D_string const demWindowStrings;

	extern int const buyFromUtility;
	extern int const sellToUtility;
	extern int const netMetering;

	//For several different objects that reference seasons
	extern int const seasonWinter;
	extern int const seasonSpring;
	extern int const seasonSummer;
	extern int const seasonFall;
	extern int const seasonAnnual;
	extern int const seasonMonthly;

	//For AssignVariablePt
	extern int const varIsArgument; // if used as a value or on right side of expression
	extern int const varIsAssigned; // if variable is assigned to or on left side of expression

	//For ComputeSteps
	// All are negative because all variables are positive
	extern int const opSUM;
	extern int const opMULTIPLY;
	extern int const opSUBTRACT;
	extern int const opDIVIDE;
	extern int const opABSOLUTE;
	extern int const opINTEGER;
	extern int const opSIGN;
	extern int const opROUND;
	extern int const opMAXIMUM;
	extern int const opMINIMUM;
	extern int const opEXCEEDS;
	extern int const opANNUALMINIMUM;
	extern int const opANNUALMAXIMUM;
	extern int const opANNUALSUM;
	extern int const opANNUALAVERAGE;
	extern int const opANNUALOR;
	extern int const opANNUALAND;
	extern int const opANNUALMAXIMUMZERO;
	extern int const opANNUALMINIMUMZERO;
	extern int const opIF;
	extern int const opGREATERTHAN;
	extern int const opGREATEREQUAL;
	extern int const opLESSTHAN;
	extern int const opLESSEQUAL;
	extern int const opEQUAL;
	extern int const opNOTEQUAL;
	extern int const opAND;
	extern int const opOR;
	extern int const opNOT;
	extern int const opADD;
	extern int const opNOOP; // no operation - just list the operand variables - shown as FROM

	//not predefined variable (user defined name - many variables and all objects)
	// used in econvar%specific
	extern int const varUserDefined;
	extern int const varNotYetDefined;

	//category variables (used in econvar%specific)
	extern int const catEnergyCharges;
	extern int const catDemandCharges;
	extern int const catServiceCharges;
	extern int const catBasis;
	extern int const catAdjustment;
	extern int const catSurcharge;
	extern int const catSubtotal;
	extern int const catTaxes;
	extern int const catTotal;
	extern int const catNotIncluded;

	//native variables (based on energy and demands from the simulation) used in econvar%specific
	extern int const nativeTotalEnergy;
	extern int const nativeTotalDemand;
	extern int const nativePeakEnergy;
	extern int const nativePeakDemand;
	extern int const nativeShoulderEnergy;
	extern int const nativeShoulderDemand;
	extern int const nativeOffPeakEnergy;
	extern int const nativeOffPeakDemand;
	extern int const nativeMidPeakEnergy;
	extern int const nativeMidPeakDemand;
	extern int const nativePeakExceedsOffPeak;
	extern int const nativeOffPeakExceedsPeak;
	extern int const nativePeakExceedsMidPeak;
	extern int const nativeMidPeakExceedsPeak;
	extern int const nativePeakExceedsShoulder;
	extern int const nativeShoulderExceedsPeak;
	extern int const nativeIsWinter;
	extern int const nativeIsNotWinter;
	extern int const nativeIsSpring;
	extern int const nativeIsNotSpring;
	extern int const nativeIsSummer;
	extern int const nativeIsNotSummer;
	extern int const nativeIsAutumn;
	extern int const nativeIsNotAutumn;

	extern int const nativePeakAndShoulderEnergy;
	extern int const nativePeakAndShoulderDemand;
	extern int const nativePeakAndMidPeakEnergy;
	extern int const nativePeakAndMidPeakDemand;
	extern int const nativeShoulderAndOffPeakEnergy;
	extern int const nativeShoulderAndOffPeakDemand;
	extern int const nativePeakAndOffPeakEnergy;
	extern int const nativePeakAndOffPeakDemand;

	extern int const nativeRealTimePriceCosts;
	extern int const nativeAboveCustomerBaseCosts;
	extern int const nativeBelowCustomerBaseCosts;
	extern int const nativeAboveCustomerBaseEnergy;
	extern int const nativeBelowCustomerBaseEnergy;

	extern int const countPeriod;
	extern int const MaxNumMonths;
	extern int const maxNumBlk;

	extern int const periodPeak;
	extern int const periodShoulder;
	extern int const periodOffPeak;
	extern int const periodMidPeak;

	extern int const kindMeterNotElectric; // must be zero because testing of >0 done later.
	extern int const kindMeterElecSimple;
	extern int const kindMeterElecProduced;
	extern int const kindMeterElecPurchased;
	extern int const kindMeterElecSurplusSold;
	extern int const kindMeterElecNet;

	extern int const varUnitTypeEnergy;
	extern int const varUnitTypeDemand;
	extern int const varUnitTypeDimensionless;
	extern int const varUnitTypeCurrency;

	//MODULE PARAMETER DEFINITIONS:

	extern int numEconVar;
	extern int sizeEconVar;

	// holds the outbound connections for each variable
	extern Array1D_int operand; // sized to sizeOperand
	extern int numOperand;
	extern int sizeOperand;

	extern int numTariff;

	extern int numQualify;

	extern int numChargeSimple;

	extern int numChargeBlock;

	extern int numRatchet;

	extern int numComputation;

	//list of pointers to variable, 0 end of line, negative indicate operations
	extern Array1D_int steps;
	extern Array1D_int stepsCopy;
	extern int numSteps;
	extern int sizeSteps;

	extern int topOfStack;
	extern int sizeStack;

	//MODULE VARIABLE DECLARATIONS:

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Types

	struct EconVarType
	{
		// Members
		std::string name; // name of the economics object or variable
		int tariffIndx; // index of the tariff name in the tariff array
		int kindOfObj; // enumerated list for the kind of economics object
		int index; // pointer to item in specific array
		Array1D< Real64 > values; // values
		// the following items are not part of the object description
		bool isArgument; // flag if the variable is ever used as an argument (value needed)
		bool isAssigned; // flag if the variable is ever assigned to
		int specific; // the specific type of variable - see enumerated lists
		// the following items are used in determinging the dependency relationship of variables
		// and consist of an operator and a list of variables.
		int cntMeDependOn; // count of items in depend this line depends upon
		int Operator; // operator used in equation (usually opSUM or opNOOP)
		int firstOperand; // first item in the operand array
		int lastOperand; // last item in the operand array
		bool activeNow; // flag if the econVar is used in the current tariff
		bool isEvaluated; // flag if the economics object that results in this variable
		//has already been evaulated
		bool isReported; // flag if the econVar has been reported in the output file
		int varUnitType; // variable unit type: energy, demand, dimensionless, currency

		// Default Constructor
		EconVarType() :
			tariffIndx( 0 ),
			kindOfObj( 0 ),
			index( 0 ),
			values( MaxNumMonths, 0.0 ),
			isArgument( false ),
			isAssigned( false ),
			specific( 0 ),
			cntMeDependOn( 0 ),
			Operator( 0 ),
			firstOperand( 0 ),
			lastOperand( 0 ),
			activeNow( false ),
			isEvaluated( false ),
			isReported( false ),
			varUnitType( 0 )
		{}

		// Member Constructor
		EconVarType(
			std::string const & name, // name of the economics object or variable
			int const tariffIndx, // index of the tariff name in the tariff array
			int const kindOfObj, // enumerated list for the kind of economics object
			int const index, // pointer to item in specific array
			Array1< Real64 > const & values, // values
			bool const isArgument, // flag if the variable is ever used as an argument (value needed)
			bool const isAssigned, // flag if the variable is ever assigned to
			int const specific, // the specific type of variable - see enumerated lists
			int const cntMeDependOn, // count of items in depend this line depends upon
			int const Operator, // operator used in equation (usually opSUM or opNOOP)
			int const firstOperand, // first item in the operand array
			int const lastOperand, // last item in the operand array
			bool const activeNow, // flag if the econVar is used in the current tariff
			bool const isEvaluated, // flag if the economics object that results in this variable
			bool const isReported, // flag if the econVar has been reported in the output file
			int const varUnitType // variable unit type: energy, demand, dimensionless, currency
		) :
			name( name ),
			tariffIndx( tariffIndx ),
			kindOfObj( kindOfObj ),
			index( index ),
			values( MaxNumMonths, values ),
			isArgument( isArgument ),
			isAssigned( isAssigned ),
			specific( specific ),
			cntMeDependOn( cntMeDependOn ),
			Operator( Operator ),
			firstOperand( firstOperand ),
			lastOperand( lastOperand ),
			activeNow( activeNow ),
			isEvaluated( isEvaluated ),
			isReported( isReported ),
			varUnitType( varUnitType )
		{}

	};

	struct TariffType
	{
		// Members
		std::string tariffName; // name of the tariff
		std::string reportMeter; // name of the report meter
		int reportMeterIndx; // index of the report meter
		int kindElectricMtr; // kind of electric meter - see enumerated list above, 0 is not electric
		int resourceNum; // based on list of DataGlobalConstants
		int convChoice; // enumerated choice index of the conversion factor
		Real64 energyConv; // energy conversion factor
		Real64 demandConv; // demand conversion factor
		std::string periodSchedule; // name of the period schedule (time of day)
		int periodSchIndex; // index to the period schedule
		std::string seasonSchedule; // name of the season schedule (winter/summer)
		int seasonSchIndex; // index to the season schedule
		std::string monthSchedule; // name of month schedule (when months end)
		int monthSchIndex; // index to the month schedule
		int demandWindow; // enumerated list of the kind of demand window
		Real64 demWinTime; // length of time for the demand window
		Real64 monthChgVal; // monthly charge value
		int monthChgPt; // pointer to a variable that contains the monthly charge
		//if 0 then use monthChgVal
		Real64 minMonthChgVal; // minimum monthly charge value
		int minMonthChgPt; // pointer to a variable that contains the minimum monthly charge
		//if 0 then use minMonthChgVal
		std::string chargeSchedule; // name of the charge schedule (for real time pricing)
		int chargeSchIndex; // index to the charge schedule
		std::string baseUseSchedule; // name of the baseline use schedule (for real time pricing)
		int baseUseSchIndex; // index to the baseline use schedule
		std::string groupName; // name of the group
		std::string monetaryUnit; // text string representing monetary unit, usually $
		int buyOrSell; // enumerated choice index of the buy or sell options
		// index to the first and last category variables
		int firstCategory; // first category referenced
		int lastCategory; // last category referenced
		// pointers to econ variables for categories
		int ptEnergyCharges;
		int ptDemandCharges;
		int ptServiceCharges;
		int ptBasis;
		int ptAdjustment;
		int ptSurcharge;
		int ptSubtotal;
		int ptTaxes;
		int ptTotal;
		int ptNotIncluded;
		// index to the first and last native variables (energies and demands from the simulation)
		int firstNative;
		int lastNative;
		//native variables (based on energy and demands from the simulation)
		int nativeTotalEnergy;
		int nativeTotalDemand;
		int nativePeakEnergy;
		int nativePeakDemand;
		int nativeShoulderEnergy;
		int nativeShoulderDemand;
		int nativeOffPeakEnergy;
		int nativeOffPeakDemand;
		int nativeMidPeakEnergy;
		int nativeMidPeakDemand;
		int nativePeakExceedsOffPeak;
		int nativeOffPeakExceedsPeak;
		int nativePeakExceedsMidPeak;
		int nativeMidPeakExceedsPeak;
		int nativePeakExceedsShoulder;
		int nativeShoulderExceedsPeak;
		int nativeIsWinter;
		int nativeIsNotWinter;
		int nativeIsSpring;
		int nativeIsNotSpring;
		int nativeIsSummer;
		int nativeIsNotSummer;
		int nativeIsAutumn;
		int nativeIsNotAutumn;
		int nativePeakAndShoulderEnergy;
		int nativePeakAndShoulderDemand;
		int nativePeakAndMidPeakEnergy;
		int nativePeakAndMidPeakDemand;
		int nativeShoulderAndOffPeakEnergy;
		int nativeShoulderAndOffPeakDemand;
		int nativePeakAndOffPeakEnergy;
		int nativePeakAndOffPeakDemand;
		//real time pricing native variable pointers
		int nativeRealTimePriceCosts;
		int nativeAboveCustomerBaseCosts;
		int nativeBelowCustomerBaseCosts;
		int nativeAboveCustomerBaseEnergy;
		int nativeBelowCustomerBaseEnergy;
		//arrays for holding gathered values
		Array2D< Real64 > gatherEnergy;
		Array2D< Real64 > gatherDemand;
		Real64 collectTime;
		Real64 collectEnergy;
		//arryas for holding real time pricing gathered values
		Array1D< Real64 > RTPcost;
		Array1D< Real64 > RTPaboveBaseCost;
		Array1D< Real64 > RTPbelowBaseCost;
		Array1D< Real64 > RTPaboveBaseEnergy;
		Array1D< Real64 > RTPbelowBaseEnergy;
		Array1D_int seasonForMonth;
		//overall qualification of the rate
		bool isQualified;
		int ptDisqualifier;
		//overall selection and annual cost
		bool isSelected;
		Real64 totalAnnualCost;
		Real64 totalAnnualEnergy;

		// Default Constructor
		TariffType() :
			reportMeterIndx( 0 ),
			kindElectricMtr( 0 ),
			resourceNum( 0 ),
			convChoice( 0 ),
			energyConv( 0.0 ),
			demandConv( 0.0 ),
			periodSchIndex( 0 ),
			seasonSchIndex( 0 ),
			monthSchIndex( 0 ),
			demandWindow( 0 ),
			demWinTime( 0.0 ),
			monthChgVal( 0.0 ),
			monthChgPt( 0 ),
			minMonthChgVal( 0.0 ),
			minMonthChgPt( 0 ),
			chargeSchIndex( 0 ),
			baseUseSchIndex( 0 ),
			buyOrSell( 0 ),
			firstCategory( 0 ),
			lastCategory( 0 ),
			ptEnergyCharges( 0 ),
			ptDemandCharges( 0 ),
			ptServiceCharges( 0 ),
			ptBasis( 0 ),
			ptAdjustment( 0 ),
			ptSurcharge( 0 ),
			ptSubtotal( 0 ),
			ptTaxes( 0 ),
			ptTotal( 0 ),
			ptNotIncluded( 0 ),
			firstNative( 0 ),
			lastNative( 0 ),
			nativeTotalEnergy( 0 ),
			nativeTotalDemand( 0 ),
			nativePeakEnergy( 0 ),
			nativePeakDemand( 0 ),
			nativeShoulderEnergy( 0 ),
			nativeShoulderDemand( 0 ),
			nativeOffPeakEnergy( 0 ),
			nativeOffPeakDemand( 0 ),
			nativeMidPeakEnergy( 0 ),
			nativeMidPeakDemand( 0 ),
			nativePeakExceedsOffPeak( 0 ),
			nativeOffPeakExceedsPeak( 0 ),
			nativePeakExceedsMidPeak( 0 ),
			nativeMidPeakExceedsPeak( 0 ),
			nativePeakExceedsShoulder( 0 ),
			nativeShoulderExceedsPeak( 0 ),
			nativeIsWinter( 0 ),
			nativeIsNotWinter( 0 ),
			nativeIsSpring( 0 ),
			nativeIsNotSpring( 0 ),
			nativeIsSummer( 0 ),
			nativeIsNotSummer( 0 ),
			nativeIsAutumn( 0 ),
			nativeIsNotAutumn( 0 ),
			nativePeakAndShoulderEnergy( 0 ),
			nativePeakAndShoulderDemand( 0 ),
			nativePeakAndMidPeakEnergy( 0 ),
			nativePeakAndMidPeakDemand( 0 ),
			nativeShoulderAndOffPeakEnergy( 0 ),
			nativeShoulderAndOffPeakDemand( 0 ),
			nativePeakAndOffPeakEnergy( 0 ),
			nativePeakAndOffPeakDemand( 0 ),
			nativeRealTimePriceCosts( 0 ),
			nativeAboveCustomerBaseCosts( 0 ),
			nativeBelowCustomerBaseCosts( 0 ),
			nativeAboveCustomerBaseEnergy( 0 ),
			nativeBelowCustomerBaseEnergy( 0 ),
			gatherEnergy( MaxNumMonths, countPeriod, 0.0 ),
			gatherDemand( MaxNumMonths, countPeriod, 0.0 ),
			collectTime( 0.0 ),
			collectEnergy( 0.0 ),
			RTPcost( MaxNumMonths, 0.0 ),
			RTPaboveBaseCost( MaxNumMonths, 0.0 ),
			RTPbelowBaseCost( MaxNumMonths, 0.0 ),
			RTPaboveBaseEnergy( MaxNumMonths, 0.0 ),
			RTPbelowBaseEnergy( MaxNumMonths, 0.0 ),
			seasonForMonth( MaxNumMonths, 0 ),
			isQualified( false ),
			ptDisqualifier( 0 ),
			isSelected( false ),
			totalAnnualCost( 0.0 ),
			totalAnnualEnergy( 0.0 )
		{}

		// Member Constructor
		TariffType(
			std::string const & tariffName, // name of the tariff
			std::string const & reportMeter, // name of the report meter
			int const reportMeterIndx, // index of the report meter
			int const kindElectricMtr, // kind of electric meter - see enumerated list above, 0 is not electric
			int const resourceNum, // based on list of DataGlobalConstants
			int const convChoice, // enumerated choice index of the conversion factor
			Real64 const energyConv, // energy conversion factor
			Real64 const demandConv, // demand conversion factor
			std::string const & periodSchedule, // name of the period schedule (time of day)
			int const periodSchIndex, // index to the period schedule
			std::string const & seasonSchedule, // name of the season schedule (winter/summer)
			int const seasonSchIndex, // index to the season schedule
			std::string const & monthSchedule, // name of month schedule (when months end)
			int const monthSchIndex, // index to the month schedule
			int const demandWindow, // enumerated list of the kind of demand window
			Real64 const demWinTime, // length of time for the demand window
			Real64 const monthChgVal, // monthly charge value
			int const monthChgPt, // pointer to a variable that contains the monthly charge
			Real64 const minMonthChgVal, // minimum monthly charge value
			int const minMonthChgPt, // pointer to a variable that contains the minimum monthly charge
			std::string const & chargeSchedule, // name of the charge schedule (for real time pricing)
			int const chargeSchIndex, // index to the charge schedule
			std::string const & baseUseSchedule, // name of the baseline use schedule (for real time pricing)
			int const baseUseSchIndex, // index to the baseline use schedule
			std::string const & groupName, // name of the group
			std::string const & monetaryUnit, // text string representing monetary unit, usually $
			int const buyOrSell, // enumerated choice index of the buy or sell options
			int const firstCategory, // first category referenced
			int const lastCategory, // last category referenced
			int const ptEnergyCharges,
			int const ptDemandCharges,
			int const ptServiceCharges,
			int const ptBasis,
			int const ptAdjustment,
			int const ptSurcharge,
			int const ptSubtotal,
			int const ptTaxes,
			int const ptTotal,
			int const ptNotIncluded,
			int const firstNative,
			int const lastNative,
			int const nativeTotalEnergy,
			int const nativeTotalDemand,
			int const nativePeakEnergy,
			int const nativePeakDemand,
			int const nativeShoulderEnergy,
			int const nativeShoulderDemand,
			int const nativeOffPeakEnergy,
			int const nativeOffPeakDemand,
			int const nativeMidPeakEnergy,
			int const nativeMidPeakDemand,
			int const nativePeakExceedsOffPeak,
			int const nativeOffPeakExceedsPeak,
			int const nativePeakExceedsMidPeak,
			int const nativeMidPeakExceedsPeak,
			int const nativePeakExceedsShoulder,
			int const nativeShoulderExceedsPeak,
			int const nativeIsWinter,
			int const nativeIsNotWinter,
			int const nativeIsSpring,
			int const nativeIsNotSpring,
			int const nativeIsSummer,
			int const nativeIsNotSummer,
			int const nativeIsAutumn,
			int const nativeIsNotAutumn,
			int const nativePeakAndShoulderEnergy,
			int const nativePeakAndShoulderDemand,
			int const nativePeakAndMidPeakEnergy,
			int const nativePeakAndMidPeakDemand,
			int const nativeShoulderAndOffPeakEnergy,
			int const nativeShoulderAndOffPeakDemand,
			int const nativePeakAndOffPeakEnergy,
			int const nativePeakAndOffPeakDemand,
			int const nativeRealTimePriceCosts,
			int const nativeAboveCustomerBaseCosts,
			int const nativeBelowCustomerBaseCosts,
			int const nativeAboveCustomerBaseEnergy,
			int const nativeBelowCustomerBaseEnergy,
			Array2< Real64 > const & gatherEnergy,
			Array2< Real64 > const & gatherDemand,
			Real64 const collectTime,
			Real64 const collectEnergy,
			Array1< Real64 > const & RTPcost,
			Array1< Real64 > const & RTPaboveBaseCost,
			Array1< Real64 > const & RTPbelowBaseCost,
			Array1< Real64 > const & RTPaboveBaseEnergy,
			Array1< Real64 > const & RTPbelowBaseEnergy,
			Array1_int const & seasonForMonth,
			bool const isQualified,
			int const ptDisqualifier,
			bool const isSelected,
			Real64 const totalAnnualCost,
			Real64 const totalAnnualEnergy
		) :
			tariffName( tariffName ),
			reportMeter( reportMeter ),
			reportMeterIndx( reportMeterIndx ),
			kindElectricMtr( kindElectricMtr ),
			resourceNum( resourceNum ),
			convChoice( convChoice ),
			energyConv( energyConv ),
			demandConv( demandConv ),
			periodSchedule( periodSchedule ),
			periodSchIndex( periodSchIndex ),
			seasonSchedule( seasonSchedule ),
			seasonSchIndex( seasonSchIndex ),
			monthSchedule( monthSchedule ),
			monthSchIndex( monthSchIndex ),
			demandWindow( demandWindow ),
			demWinTime( demWinTime ),
			monthChgVal( monthChgVal ),
			monthChgPt( monthChgPt ),
			minMonthChgVal( minMonthChgVal ),
			minMonthChgPt( minMonthChgPt ),
			chargeSchedule( chargeSchedule ),
			chargeSchIndex( chargeSchIndex ),
			baseUseSchedule( baseUseSchedule ),
			baseUseSchIndex( baseUseSchIndex ),
			groupName( groupName ),
			monetaryUnit( monetaryUnit ),
			buyOrSell( buyOrSell ),
			firstCategory( firstCategory ),
			lastCategory( lastCategory ),
			ptEnergyCharges( ptEnergyCharges ),
			ptDemandCharges( ptDemandCharges ),
			ptServiceCharges( ptServiceCharges ),
			ptBasis( ptBasis ),
			ptAdjustment( ptAdjustment ),
			ptSurcharge( ptSurcharge ),
			ptSubtotal( ptSubtotal ),
			ptTaxes( ptTaxes ),
			ptTotal( ptTotal ),
			ptNotIncluded( ptNotIncluded ),
			firstNative( firstNative ),
			lastNative( lastNative ),
			nativeTotalEnergy( nativeTotalEnergy ),
			nativeTotalDemand( nativeTotalDemand ),
			nativePeakEnergy( nativePeakEnergy ),
			nativePeakDemand( nativePeakDemand ),
			nativeShoulderEnergy( nativeShoulderEnergy ),
			nativeShoulderDemand( nativeShoulderDemand ),
			nativeOffPeakEnergy( nativeOffPeakEnergy ),
			nativeOffPeakDemand( nativeOffPeakDemand ),
			nativeMidPeakEnergy( nativeMidPeakEnergy ),
			nativeMidPeakDemand( nativeMidPeakDemand ),
			nativePeakExceedsOffPeak( nativePeakExceedsOffPeak ),
			nativeOffPeakExceedsPeak( nativeOffPeakExceedsPeak ),
			nativePeakExceedsMidPeak( nativePeakExceedsMidPeak ),
			nativeMidPeakExceedsPeak( nativeMidPeakExceedsPeak ),
			nativePeakExceedsShoulder( nativePeakExceedsShoulder ),
			nativeShoulderExceedsPeak( nativeShoulderExceedsPeak ),
			nativeIsWinter( nativeIsWinter ),
			nativeIsNotWinter( nativeIsNotWinter ),
			nativeIsSpring( nativeIsSpring ),
			nativeIsNotSpring( nativeIsNotSpring ),
			nativeIsSummer( nativeIsSummer ),
			nativeIsNotSummer( nativeIsNotSummer ),
			nativeIsAutumn( nativeIsAutumn ),
			nativeIsNotAutumn( nativeIsNotAutumn ),
			nativePeakAndShoulderEnergy( nativePeakAndShoulderEnergy ),
			nativePeakAndShoulderDemand( nativePeakAndShoulderDemand ),
			nativePeakAndMidPeakEnergy( nativePeakAndMidPeakEnergy ),
			nativePeakAndMidPeakDemand( nativePeakAndMidPeakDemand ),
			nativeShoulderAndOffPeakEnergy( nativeShoulderAndOffPeakEnergy ),
			nativeShoulderAndOffPeakDemand( nativeShoulderAndOffPeakDemand ),
			nativePeakAndOffPeakEnergy( nativePeakAndOffPeakEnergy ),
			nativePeakAndOffPeakDemand( nativePeakAndOffPeakDemand ),
			nativeRealTimePriceCosts( nativeRealTimePriceCosts ),
			nativeAboveCustomerBaseCosts( nativeAboveCustomerBaseCosts ),
			nativeBelowCustomerBaseCosts( nativeBelowCustomerBaseCosts ),
			nativeAboveCustomerBaseEnergy( nativeAboveCustomerBaseEnergy ),
			nativeBelowCustomerBaseEnergy( nativeBelowCustomerBaseEnergy ),
			gatherEnergy( MaxNumMonths, countPeriod, gatherEnergy ),
			gatherDemand( MaxNumMonths, countPeriod, gatherDemand ),
			collectTime( collectTime ),
			collectEnergy( collectEnergy ),
			RTPcost( MaxNumMonths, RTPcost ),
			RTPaboveBaseCost( MaxNumMonths, RTPaboveBaseCost ),
			RTPbelowBaseCost( MaxNumMonths, RTPbelowBaseCost ),
			RTPaboveBaseEnergy( MaxNumMonths, RTPaboveBaseEnergy ),
			RTPbelowBaseEnergy( MaxNumMonths, RTPbelowBaseEnergy ),
			seasonForMonth( MaxNumMonths, seasonForMonth ),
			isQualified( isQualified ),
			ptDisqualifier( ptDisqualifier ),
			isSelected( isSelected ),
			totalAnnualCost( totalAnnualCost ),
			totalAnnualEnergy( totalAnnualEnergy )
		{}

	};

	struct QualifyType
	{
		// Members
		int namePt; // index of the name and variable in the variable array
		int tariffIndx; // index of the tariff name in the tariff array
		int sourcePt; // index of the variable in the variable array
		bool isMaximum; // indicator if maximum test otherwise minimum
		Real64 thresholdVal; // value of the threshold
		int thresholdPt; // pointer to the variable holding the values
		int season; // enumerated list of the kind of season
		bool isConsecutive; // indicator if consecutive months otherwise count
		int numberOfMonths; // number of months the test must be good for

		// Default Constructor
		QualifyType() :
			namePt( 0 ),
			tariffIndx( 0 ),
			sourcePt( 0 ),
			isMaximum( false ),
			thresholdVal( 0.0 ),
			thresholdPt( 0 ),
			season( 0 ),
			isConsecutive( false ),
			numberOfMonths( 0 )
		{}

		// Member Constructor
		QualifyType(
			int const namePt, // index of the name and variable in the variable array
			int const tariffIndx, // index of the tariff name in the tariff array
			int const sourcePt, // index of the variable in the variable array
			bool const isMaximum, // indicator if maximum test otherwise minimum
			Real64 const thresholdVal, // value of the threshold
			int const thresholdPt, // pointer to the variable holding the values
			int const season, // enumerated list of the kind of season
			bool const isConsecutive, // indicator if consecutive months otherwise count
			int const numberOfMonths // number of months the test must be good for
		) :
			namePt( namePt ),
			tariffIndx( tariffIndx ),
			sourcePt( sourcePt ),
			isMaximum( isMaximum ),
			thresholdVal( thresholdVal ),
			thresholdPt( thresholdPt ),
			season( season ),
			isConsecutive( isConsecutive ),
			numberOfMonths( numberOfMonths )
		{}

	};

	struct ChargeSimpleType
	{
		// Members
		int namePt; // index of the name and variable in the variable array
		int tariffIndx; // index of the tariff name in the tariff array
		int sourcePt; // index of the variable in the variable array
		int season; // enumerated list of the kind of season
		int categoryPt; // index of the category in the variable array
		Real64 costPerVal; // cost per unit value
		int costPerPt; // cost per unit index in the variable array (0 is flag for no variable)

		// Default Constructor
		ChargeSimpleType() :
			namePt( 0 ),
			tariffIndx( 0 ),
			sourcePt( 0 ),
			season( 0 ),
			categoryPt( 0 ),
			costPerVal( 0.0 ),
			costPerPt( 0 )
		{}

		// Member Constructor
		ChargeSimpleType(
			int const namePt, // index of the name and variable in the variable array
			int const tariffIndx, // index of the tariff name in the tariff array
			int const sourcePt, // index of the variable in the variable array
			int const season, // enumerated list of the kind of season
			int const categoryPt, // index of the category in the variable array
			Real64 const costPerVal, // cost per unit value
			int const costPerPt // cost per unit index in the variable array (0 is flag for no variable)
		) :
			namePt( namePt ),
			tariffIndx( tariffIndx ),
			sourcePt( sourcePt ),
			season( season ),
			categoryPt( categoryPt ),
			costPerVal( costPerVal ),
			costPerPt( costPerPt )
		{}

	};

	struct ChargeBlockType
	{
		// Members
		int namePt; // index of the name and variable in the variable array
		int tariffIndx; // index of the tariff name in the tariff array
		int sourcePt; // index of the variable in the variable array
		int season; // enumerated list of the kind of season
		int categoryPt; // index of the category in the variable array
		int remainingPt; // index of the remaining into variable in the variable array
		Real64 blkSzMultVal; // block size multiplier value
		int blkSzMultPt; // block size variable in the variable array (0 is flag for no variable)
		int numBlk; // number of blocks used
		Array1D< Real64 > blkSzVal; // array of block size values
		Array1D_int blkSzPt; // block size variables index to the variable array (0 is no variable)
		Array1D< Real64 > blkCostVal; // array of block cost values
		Array1D_int blkCostPt; // block cost variables index to the variable array (0 is no variable)

		// Default Constructor
		ChargeBlockType() :
			namePt( 0 ),
			tariffIndx( 0 ),
			sourcePt( 0 ),
			season( 0 ),
			categoryPt( 0 ),
			remainingPt( 0 ),
			blkSzMultVal( 0.0 ),
			blkSzMultPt( 0 ),
			numBlk( 0 ),
			blkSzVal( maxNumBlk, 0.0 ),
			blkSzPt( maxNumBlk, 0 ),
			blkCostVal( maxNumBlk, 0.0 ),
			blkCostPt( maxNumBlk, 0 )
		{}

		// Member Constructor
		ChargeBlockType(
			int const namePt, // index of the name and variable in the variable array
			int const tariffIndx, // index of the tariff name in the tariff array
			int const sourcePt, // index of the variable in the variable array
			int const season, // enumerated list of the kind of season
			int const categoryPt, // index of the category in the variable array
			int const remainingPt, // index of the remaining into variable in the variable array
			Real64 const blkSzMultVal, // block size multiplier value
			int const blkSzMultPt, // block size variable in the variable array (0 is flag for no variable)
			int const numBlk, // number of blocks used
			Array1< Real64 > const & blkSzVal, // array of block size values
			Array1_int const & blkSzPt, // block size variables index to the variable array (0 is no variable)
			Array1< Real64 > const & blkCostVal, // array of block cost values
			Array1_int const & blkCostPt // block cost variables index to the variable array (0 is no variable)
		) :
			namePt( namePt ),
			tariffIndx( tariffIndx ),
			sourcePt( sourcePt ),
			season( season ),
			categoryPt( categoryPt ),
			remainingPt( remainingPt ),
			blkSzMultVal( blkSzMultVal ),
			blkSzMultPt( blkSzMultPt ),
			numBlk( numBlk ),
			blkSzVal( maxNumBlk, blkSzVal ),
			blkSzPt( maxNumBlk, blkSzPt ),
			blkCostVal( maxNumBlk, blkCostVal ),
			blkCostPt( maxNumBlk, blkCostPt )
		{}

	};

	struct RatchetType
	{
		// Members
		int namePt; // index of the name and variable in the variable array
		int tariffIndx; // index of the tariff name in the tariff array
		int baselinePt; // index of the baseline variable in the variable array
		int adjustmentPt; // index fo the adjustment variable in the variable array
		int seasonFrom; // enumerated list of the kind of season
		int seasonTo; // enumerated list of the kind of season
		Real64 multiplierVal; // value of the ratchet multiplier
		int multiplierPt; // multiplier variable in the variable array (0 for no variable)
		Real64 offsetVal; // value of the ratchet offset
		int offsetPt; // offset variable in the variable array (0 for no variable)

		// Default Constructor
		RatchetType() :
			namePt( 0 ),
			tariffIndx( 0 ),
			baselinePt( 0 ),
			adjustmentPt( 0 ),
			seasonFrom( 0 ),
			seasonTo( 0 ),
			multiplierVal( 0.0 ),
			multiplierPt( 0 ),
			offsetVal( 0.0 ),
			offsetPt( 0 )
		{}

		// Member Constructor
		RatchetType(
			int const namePt, // index of the name and variable in the variable array
			int const tariffIndx, // index of the tariff name in the tariff array
			int const baselinePt, // index of the baseline variable in the variable array
			int const adjustmentPt, // index fo the adjustment variable in the variable array
			int const seasonFrom, // enumerated list of the kind of season
			int const seasonTo, // enumerated list of the kind of season
			Real64 const multiplierVal, // value of the ratchet multiplier
			int const multiplierPt, // multiplier variable in the variable array (0 for no variable)
			Real64 const offsetVal, // value of the ratchet offset
			int const offsetPt // offset variable in the variable array (0 for no variable)
		) :
			namePt( namePt ),
			tariffIndx( tariffIndx ),
			baselinePt( baselinePt ),
			adjustmentPt( adjustmentPt ),
			seasonFrom( seasonFrom ),
			seasonTo( seasonTo ),
			multiplierVal( multiplierVal ),
			multiplierPt( multiplierPt ),
			offsetVal( offsetVal ),
			offsetPt( offsetPt )
		{}

	};

	struct ComputationType
	{
		// Members
		std::string computeName; // name of the compute
		int firstStep; // index to steps array for the first step in this compute steps
		int lastStep; // index to steps array for the last step in this compute steps
		bool isUserDef; // if the computation steps were user defined

		// Default Constructor
		ComputationType() :
			firstStep( 0 ),
			lastStep( 0 ),
			isUserDef( false )
		{}

		// Member Constructor
		ComputationType(
			std::string const & computeName, // name of the compute
			int const firstStep, // index to steps array for the first step in this compute steps
			int const lastStep, // index to steps array for the last step in this compute steps
			bool const isUserDef // if the computation steps were user defined
		) :
			computeName( computeName ),
			firstStep( firstStep ),
			lastStep( lastStep ),
			isUserDef( isUserDef )
		{}

	};

	struct StackType
	{
		// Members
		int varPt; // pointer to item in specific array
		Array1D< Real64 > values; // values

		// Default Constructor
		StackType() :
			varPt( 0 ),
			values( MaxNumMonths, 0.0 )
		{}

		// Member Constructor
		StackType(
			int const varPt, // pointer to item in specific array
			Array1< Real64 > const & values // values
		) :
			varPt( varPt ),
			values( MaxNumMonths, values )
		{}

	};

	// Object Data
	extern Array1D< EconVarType > econVar;
	extern Array1D< TariffType > tariff;
	extern Array1D< QualifyType > qualify;
	extern Array1D< ChargeSimpleType > chargeSimple;
	extern Array1D< ChargeBlockType > chargeBlock;
	extern Array1D< RatchetType > ratchet;
	extern Array1D< ComputationType > computation;
	extern Array1D< StackType > stack;

	// Functions

	void
	UpdateUtilityBills();

	//======================================================================================================================
	//======================================================================================================================

	//    GET INPUT ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	GetInputEconomicsTariff( bool & ErrorsFound ); // true if errors found during getting input objects.

	void
	GetInputEconomicsQualify( bool & ErrorsFound ); // true if errors found during getting input objects.

	void
	GetInputEconomicsChargeSimple( bool & ErrorsFound ); // true if errors found during getting input objects.

	void
	GetInputEconomicsChargeBlock( bool & ErrorsFound ); // true if errors found during getting input objects.

	void
	GetInputEconomicsRatchet( bool & ErrorsFound ); // true if errors found during getting input objects.

	void
	GetInputEconomicsVariable( bool & ErrorsFound ); // true if errors found during getting input objects.

	void
	GetInputEconomicsComputation( bool & ErrorsFound ); // true if errors found during getting input objects.

	void
	GetInputEconomicsCurrencyType( bool & ErrorsFound ); // true if errors found during getting input objects.

	void
	parseComputeLine(
		std::string const & lineOfCompute,
		int const fromTariff
	);

	void
	GetLastWord(
		std::string const & lineOfText,
		std::string::size_type & endOfScan,
		std::string & aWord
	);

	void
	initializeMonetaryUnit();

	int
	LookUpSeason(
		std::string const & nameOfSeason,
		std::string const & nameOfReferingObj
	);

	int
	FindTariffIndex(
		std::string const & nameOfTariff,
		std::string const & nameOfReferingObj,
		bool & ErrorsFound,
		std::string const & nameOfCurObj
	);

	void
	warnIfNativeVarname(
		std::string const & objName,
		int const curTariffIndex,
		bool & ErrorsFound,
		std::string const & curobjName
	);

	int
	AssignVariablePt(
		std::string const & stringIn,
		bool const flagIfNotNumeric,
		int const useOfVar,
		int const varSpecific,
		int const econObjKind,
		int const objIndex,
		int const tariffPt
	);

	void
	incrementEconVar();

	void
	incrementSteps();

	std::string
	RemoveSpaces( std::string const & StringIn );

	void
	CreateCategoryNativeVariables();

	int
	lookupOperator( std::string const & opString );

	//======================================================================================================================
	//======================================================================================================================

	//    DEFAULT COMPUTATION RELATED ROUTINES

	//======================================================================================================================
	//======================================================================================================================

	void
	CreateDefaultComputation();

	void
	addOperand(
		int const varMe,
		int const varOperand
	);

	void
	addChargesToOperand(
		int const curTariff,
		int const curPointer
	);

	//======================================================================================================================
	//======================================================================================================================

	//    GATHER TIMESTEP VALUES ROUTINE

	//======================================================================================================================
	//======================================================================================================================

	void
	GatherForEconomics();

	bool
	isWithinRange(
		int const testVal,
		int const minThreshold,
		int const maxThreshold
	);

	//======================================================================================================================
	//======================================================================================================================

	//    COMPUTE THE UTILITY BILLS AND CREATE REPORTS

	//======================================================================================================================
	//======================================================================================================================

	void
	ComputeTariff();

	void
	pushStack(
		Array1A< Real64 > const monthlyArray,
		int const variablePointer
	);

	void
	popStack(
		Array1A< Real64 > monthlyArray,
		int & variablePointer
	);

	void
	evaluateChargeSimple( int const usingVariable );

	void
	evaluateChargeBlock( int const usingVariable );

	void
	evaluateRatchet( int const usingVariable );

	void
	evaluateQualify( int const usingVariable );

	void
	addMonthlyCharge( int const usingVariable );

	void
	checkMinimumMonthlyCharge( int const curTariff );

	void
	setNativeVariables();

	void
	LEEDtariffReporting();

	void
	WriteTabularTariffReports();

	void
	showWarningsBasedOnTotal();

	void
	getMaxAndSum(
		int const varPointer,
		Real64 & sumResult,
		Real64 & maxResult
	);

	void
	ReportEconomicVariable(
		std::string const & titleString,
		bool const includeCategory,
		bool const showCurrencySymbol,
		std::string const & forString
	);

	void
	selectTariff();

	void
	GetMonthlyCostForResource(
		int const inResourceNumber,
		Array1A< Real64 > outMonthlyCosts
	);

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

} // EconomicTariff

} // EnergyPlus

#endif
