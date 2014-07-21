#ifndef ChillerIndirectAbsorption_hh_INCLUDED
#define ChillerIndirectAbsorption_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ChillerIndirectAbsorption {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	//chiller flow modes
	extern int const FlowModeNotSet;
	extern int const ConstantFlow;
	extern int const NotModulated;
	extern int const LeavingSetPointModulated;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumIndirectAbsorbers; // number of Absorption Chillers specified in input
	extern Real64 CondMassFlowRate; // Kg/s - condenser mass flow rate, water side
	extern Real64 EvapMassFlowRate; // Kg/s - evaporator mass flow rate, water side
	extern Real64 GenMassFlowRate; // Kg/s - steam mass flow rate, water side
	extern Real64 CondOutletTemp; // C - condenser outlet temperature, water side
	extern Real64 EvapOutletTemp; // C - evaporator outlet temperature, water side
	extern Real64 GenOutletTemp; // C - generator fluid outlet temperature
	extern Real64 SteamOutletEnthalpy; // J/kg - generator fluid outlet enthalpy
	extern Real64 PumpingPower; // W - rate of Absorber energy use
	extern Real64 PumpingEnergy; // J - Absorber energy use
	extern Real64 QGenerator; // W - rate of Absorber steam use
	extern Real64 GeneratorEnergy; // J - Absorber steam use
	extern Real64 QEvaporator; // W - rate of heat transfer to the evaporator coil
	extern Real64 EvaporatorEnergy; // J - heat transfer to the evaporator coil
	extern Real64 QCondenser; // W - rate of heat transfer to the condenser coil
	extern Real64 CondenserEnergy; // J - heat transfer to the condenser coil
	extern Real64 EnergyLossToEnvironment; // J - piping energy loss from generator outlet to pump inlet
	extern Real64 ChillerONOFFCyclingFrac; // fraction of time chiller is on

	// SUBROUTINE SPECIFICATIONS FOR MODULE:

	// Types

	struct IndirectAbsorberSpecs
	{
		// Members
		std::string Name; // user identifier
		Real64 NomCap; // W - design nominal capacity of Absorber
		Real64 NomPumpPower; // W - design nominal capacity of Absorber
		Real64 EvapVolFlowRate; // m3/s - design nominal water volumetric flow rate through the evaporator
		Real64 CondVolFlowRate; // m3/s - design nominal water volumetric flow rate through the condenser
		Real64 EvapMassFlowRateMax; // kg/s - Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
		Real64 CondMassFlowRateMax; // Max Design Condeneser Mass Flow Rate [kg/s]
		Real64 GenMassFlowRateMax; // kg/s - Max Design Generator Mass Flow Rate converted from Volume Flow Rate
		Real64 MinPartLoadRat; // (BLAST MIN) min allowed operating frac full load
		Real64 MaxPartLoadRat; // (BLAST MAX) max allowed operating frac full load
		Real64 OptPartLoadRat; // (BLAST BEST) optimal operating frac full load
		Real64 TempDesCondIn; // C - (BLAST ADJTC(1)The design secondary loop fluid
		// temperature at the Absorber condenser side inlet
		Real64 MinCondInletTemp; // C - minimum condenser inlet temperature for chiller operation
		Real64 MinGeneratorInletTemp; // C - minimum generator inlet temperature for chiller operation
		Real64 TempLowLimitEvapOut; // C - low temperature shut off
		Real64 GeneratorVolFlowRate; // m3/s - hot water volumetric flow rate through generator
		Real64 GeneratorSubcool; // C - amount of subcooling in steam generator
		Real64 LoopSubcool; // C - amount of subcooling in steam generator
		Real64 GeneratorDeltaTemp; // C - generator fluid temperature difference (water only)
		Real64 SizFac; // Sizing factor
		int EvapInletNodeNum; // Node number on the inlet side of the plant
		int EvapOutletNodeNum; // Node number on the outlet side of the plant
		int CondInletNodeNum; // Node number on the inlet side of the condenser
		int CondOutletNodeNum; // Node number on the outlet side of the condenser
		int GeneratorInletNodeNum; // Generator inlet node number, steam/water side
		int GeneratorOutletNodeNum; // Generator outlet node number, steam/water side
		int GeneratorInputCurvePtr; // Index to steam use curve as a function of PLR
		int PumpPowerCurvePtr; // Index to pump power curve as a function of PLR
		int CapFCondenserTempPtr; // Index to capacity as a function of absorber temp curve
		int CapFEvaporatorTempPtr; // Index to capacity as a function of evaporator temp curve
		int CapFGeneratorTempPtr; // Index to capacity as a function of generator temp curve
		int HeatInputFCondTempPtr; // Index to generator heat input as a function of absorber temp
		int HeatInputFEvapTempPtr; // Index to generator heat input as a function of absorber temp
		int ErrCount2; // error counter
		int GenHeatSourceType; // Generator heat source type, NodeType_Steam=3 or NodeType_Water=2
		int SteamFluidIndex; // index to generator fluid type
		bool Available; // need an array of logicals--load identifiers of available equipment
		bool ON; // simulate the machine at it's operating part load ratio
		int FlowMode; // one of 3 modes for componet flow during operation
		bool ModulatedFlowSetToLoop; // True if the setpoint is missing at the outlet node
		bool ModulatedFlowErrDone; // true if setpoint warning issued
		int MinCondInletTempCtr; // Low condenser temp warning message counter
		int MinCondInletTempIndex; // Low condenser temp warning message index
		int MinGenInletTempCtr; // Low generator temp warning message counter
		int MinGenInletTempIndex; // Low generator temp warning message index
		int CWLoopNum; // chilled water plant loop index number
		int CWLoopSideNum; // chilled water plant loop side index
		int CWBranchNum; // chilled water plant loop branch index
		int CWCompNum; // chilled water plant loop component index
		int CDLoopNum; // condenser water plant loop index number
		int CDLoopSideNum; // condenser water plant loop side index
		int CDBranchNum; // condenser water plant loop branch index
		int CDCompNum; // condenser water plant loop component index
		int GenLoopNum; // generator plant loop index number
		int GenLoopSideNum; // generator plant loop side index
		int GenBranchNum; // generator plant loop branch index
		int GenCompNum; // generator plant loop component index
		bool PossibleSubcooling; // flag to indicate chiller is doing less cooling that requested
		bool IsThisSized; // TRUE if sizing is done

		// Default Constructor
		IndirectAbsorberSpecs() :
			NomCap( 0.0 ),
			NomPumpPower( 0.0 ),
			EvapVolFlowRate( 0.0 ),
			CondVolFlowRate( 0.0 ),
			EvapMassFlowRateMax( 0.0 ),
			CondMassFlowRateMax( 0.0 ),
			GenMassFlowRateMax( 0.0 ),
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			OptPartLoadRat( 0.0 ),
			TempDesCondIn( 0.0 ),
			MinCondInletTemp( 0.0 ),
			MinGeneratorInletTemp( 0.0 ),
			TempLowLimitEvapOut( 0.0 ),
			GeneratorVolFlowRate( 0.0 ),
			GeneratorSubcool( 0.0 ),
			LoopSubcool( 0.0 ),
			GeneratorDeltaTemp( -99999.0 ),
			SizFac( 0.0 ),
			EvapInletNodeNum( 0 ),
			EvapOutletNodeNum( 0 ),
			CondInletNodeNum( 0 ),
			CondOutletNodeNum( 0 ),
			GeneratorInletNodeNum( 0 ),
			GeneratorOutletNodeNum( 0 ),
			GeneratorInputCurvePtr( 0 ),
			PumpPowerCurvePtr( 0 ),
			CapFCondenserTempPtr( 0 ),
			CapFEvaporatorTempPtr( 0 ),
			CapFGeneratorTempPtr( 0 ),
			HeatInputFCondTempPtr( 0 ),
			HeatInputFEvapTempPtr( 0 ),
			ErrCount2( 0 ),
			GenHeatSourceType( 0 ),
			SteamFluidIndex( 0 ),
			Available( false ),
			ON( false ),
			FlowMode( FlowModeNotSet ),
			ModulatedFlowSetToLoop( false ),
			ModulatedFlowErrDone( false ),
			MinCondInletTempCtr( 0 ),
			MinCondInletTempIndex( 0 ),
			MinGenInletTempCtr( 0 ),
			MinGenInletTempIndex( 0 ),
			CWLoopNum( 0 ),
			CWLoopSideNum( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			CDLoopNum( 0 ),
			CDLoopSideNum( 0 ),
			CDBranchNum( 0 ),
			CDCompNum( 0 ),
			GenLoopNum( 0 ),
			GenLoopSideNum( 0 ),
			GenBranchNum( 0 ),
			GenCompNum( 0 ),
			PossibleSubcooling( false ),
			IsThisSized( false )
		{}

		// Member Constructor
		IndirectAbsorberSpecs(
			std::string const & Name, // user identifier
			Real64 const NomCap, // W - design nominal capacity of Absorber
			Real64 const NomPumpPower, // W - design nominal capacity of Absorber
			Real64 const EvapVolFlowRate, // m3/s - design nominal water volumetric flow rate through the evaporator
			Real64 const CondVolFlowRate, // m3/s - design nominal water volumetric flow rate through the condenser
			Real64 const EvapMassFlowRateMax, // kg/s - Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
			Real64 const CondMassFlowRateMax, // Max Design Condeneser Mass Flow Rate [kg/s]
			Real64 const GenMassFlowRateMax, // kg/s - Max Design Generator Mass Flow Rate converted from Volume Flow Rate
			Real64 const MinPartLoadRat, // (BLAST MIN) min allowed operating frac full load
			Real64 const MaxPartLoadRat, // (BLAST MAX) max allowed operating frac full load
			Real64 const OptPartLoadRat, // (BLAST BEST) optimal operating frac full load
			Real64 const TempDesCondIn, // C - (BLAST ADJTC(1)The design secondary loop fluid
			Real64 const MinCondInletTemp, // C - minimum condenser inlet temperature for chiller operation
			Real64 const MinGeneratorInletTemp, // C - minimum generator inlet temperature for chiller operation
			Real64 const TempLowLimitEvapOut, // C - low temperature shut off
			Real64 const GeneratorVolFlowRate, // m3/s - hot water volumetric flow rate through generator
			Real64 const GeneratorSubcool, // C - amount of subcooling in steam generator
			Real64 const LoopSubcool, // C - amount of subcooling in steam generator
			Real64 const GeneratorDeltaTemp, // C - generator fluid temperature difference (water only)
			Real64 const SizFac, // Sizing factor
			int const EvapInletNodeNum, // Node number on the inlet side of the plant
			int const EvapOutletNodeNum, // Node number on the outlet side of the plant
			int const CondInletNodeNum, // Node number on the inlet side of the condenser
			int const CondOutletNodeNum, // Node number on the outlet side of the condenser
			int const GeneratorInletNodeNum, // Generator inlet node number, steam/water side
			int const GeneratorOutletNodeNum, // Generator outlet node number, steam/water side
			int const GeneratorInputCurvePtr, // Index to steam use curve as a function of PLR
			int const PumpPowerCurvePtr, // Index to pump power curve as a function of PLR
			int const CapFCondenserTempPtr, // Index to capacity as a function of absorber temp curve
			int const CapFEvaporatorTempPtr, // Index to capacity as a function of evaporator temp curve
			int const CapFGeneratorTempPtr, // Index to capacity as a function of generator temp curve
			int const HeatInputFCondTempPtr, // Index to generator heat input as a function of absorber temp
			int const HeatInputFEvapTempPtr, // Index to generator heat input as a function of absorber temp
			int const ErrCount2, // error counter
			int const GenHeatSourceType, // Generator heat source type, NodeType_Steam=3 or NodeType_Water=2
			int const SteamFluidIndex, // index to generator fluid type
			bool const Available, // need an array of logicals--load identifiers of available equipment
			bool const ON, // simulate the machine at it's operating part load ratio
			int const FlowMode, // one of 3 modes for componet flow during operation
			bool const ModulatedFlowSetToLoop, // True if the setpoint is missing at the outlet node
			bool const ModulatedFlowErrDone, // true if setpoint warning issued
			int const MinCondInletTempCtr, // Low condenser temp warning message counter
			int const MinCondInletTempIndex, // Low condenser temp warning message index
			int const MinGenInletTempCtr, // Low generator temp warning message counter
			int const MinGenInletTempIndex, // Low generator temp warning message index
			int const CWLoopNum, // chilled water plant loop index number
			int const CWLoopSideNum, // chilled water plant loop side index
			int const CWBranchNum, // chilled water plant loop branch index
			int const CWCompNum, // chilled water plant loop component index
			int const CDLoopNum, // condenser water plant loop index number
			int const CDLoopSideNum, // condenser water plant loop side index
			int const CDBranchNum, // condenser water plant loop branch index
			int const CDCompNum, // condenser water plant loop component index
			int const GenLoopNum, // generator plant loop index number
			int const GenLoopSideNum, // generator plant loop side index
			int const GenBranchNum, // generator plant loop branch index
			int const GenCompNum, // generator plant loop component index
			bool const PossibleSubcooling, // flag to indicate chiller is doing less cooling that requested
			bool const IsThisSized // TRUE if sizing is done
		) :
			Name( Name ),
			NomCap( NomCap ),
			NomPumpPower( NomPumpPower ),
			EvapVolFlowRate( EvapVolFlowRate ),
			CondVolFlowRate( CondVolFlowRate ),
			EvapMassFlowRateMax( EvapMassFlowRateMax ),
			CondMassFlowRateMax( CondMassFlowRateMax ),
			GenMassFlowRateMax( GenMassFlowRateMax ),
			MinPartLoadRat( MinPartLoadRat ),
			MaxPartLoadRat( MaxPartLoadRat ),
			OptPartLoadRat( OptPartLoadRat ),
			TempDesCondIn( TempDesCondIn ),
			MinCondInletTemp( MinCondInletTemp ),
			MinGeneratorInletTemp( MinGeneratorInletTemp ),
			TempLowLimitEvapOut( TempLowLimitEvapOut ),
			GeneratorVolFlowRate( GeneratorVolFlowRate ),
			GeneratorSubcool( GeneratorSubcool ),
			LoopSubcool( LoopSubcool ),
			GeneratorDeltaTemp( GeneratorDeltaTemp ),
			SizFac( SizFac ),
			EvapInletNodeNum( EvapInletNodeNum ),
			EvapOutletNodeNum( EvapOutletNodeNum ),
			CondInletNodeNum( CondInletNodeNum ),
			CondOutletNodeNum( CondOutletNodeNum ),
			GeneratorInletNodeNum( GeneratorInletNodeNum ),
			GeneratorOutletNodeNum( GeneratorOutletNodeNum ),
			GeneratorInputCurvePtr( GeneratorInputCurvePtr ),
			PumpPowerCurvePtr( PumpPowerCurvePtr ),
			CapFCondenserTempPtr( CapFCondenserTempPtr ),
			CapFEvaporatorTempPtr( CapFEvaporatorTempPtr ),
			CapFGeneratorTempPtr( CapFGeneratorTempPtr ),
			HeatInputFCondTempPtr( HeatInputFCondTempPtr ),
			HeatInputFEvapTempPtr( HeatInputFEvapTempPtr ),
			ErrCount2( ErrCount2 ),
			GenHeatSourceType( GenHeatSourceType ),
			SteamFluidIndex( SteamFluidIndex ),
			Available( Available ),
			ON( ON ),
			FlowMode( FlowMode ),
			ModulatedFlowSetToLoop( ModulatedFlowSetToLoop ),
			ModulatedFlowErrDone( ModulatedFlowErrDone ),
			MinCondInletTempCtr( MinCondInletTempCtr ),
			MinCondInletTempIndex( MinCondInletTempIndex ),
			MinGenInletTempCtr( MinGenInletTempCtr ),
			MinGenInletTempIndex( MinGenInletTempIndex ),
			CWLoopNum( CWLoopNum ),
			CWLoopSideNum( CWLoopSideNum ),
			CWBranchNum( CWBranchNum ),
			CWCompNum( CWCompNum ),
			CDLoopNum( CDLoopNum ),
			CDLoopSideNum( CDLoopSideNum ),
			CDBranchNum( CDBranchNum ),
			CDCompNum( CDCompNum ),
			GenLoopNum( GenLoopNum ),
			GenLoopSideNum( GenLoopSideNum ),
			GenBranchNum( GenBranchNum ),
			GenCompNum( GenCompNum ),
			PossibleSubcooling( PossibleSubcooling ),
			IsThisSized( IsThisSized )
		{}

	};

	struct ReportVars
	{
		// Members
		Real64 PumpingPower; // reporting: W - electric pumping power
		Real64 QGenerator; // reporting: W - steam heat transfer rate
		Real64 QEvap; // reporting: W - evaporator heat transfer rate
		Real64 QCond; // reporting: W - condensor heat transfer rate
		Real64 PumpingEnergy; // reporting: J - electric pumping power
		Real64 GeneratorEnergy; // reporting: J - steam heat transfer rate
		Real64 EvapEnergy; // reporting: J - evaporator heat transfer rate
		Real64 CondEnergy; // reporting: J - condensor heat transfer rate
		Real64 CondInletTemp; // reporting: C - condenser inlet temperature
		Real64 EvapInletTemp; // reporting: C - evaporator inlet temperature
		Real64 CondOutletTemp; // reporting: C - condenser outlet temperature
		Real64 EvapOutletTemp; // reporting: C - evaporator outlet temperature
		Real64 Evapmdot; // reporting: kg/ - evaporator mass flow rate
		Real64 Condmdot; // reporting: kg/ - condenser mass flow rate
		Real64 Genmdot; // reporting: generatore mass flow rate when connected to plant
		Real64 SteamMdot; // reporting: kg/s - steam mass flow rate
		Real64 ActualCOP; // reporting: coefficient of performance = QEvap/QGenerator
		Real64 ChillerPartLoadRatio; // reporting: part-load ratio
		Real64 ChillerCyclingFrac; // reporting: chiller on/off cycling fraction
		Real64 LoopLoss; // reporting: W - loop loss from absorber outlet to condensate pump inlet

		// Default Constructor
		ReportVars() :
			PumpingPower( 0.0 ),
			QGenerator( 0.0 ),
			QEvap( 0.0 ),
			QCond( 0.0 ),
			PumpingEnergy( 0.0 ),
			GeneratorEnergy( 0.0 ),
			EvapEnergy( 0.0 ),
			CondEnergy( 0.0 ),
			CondInletTemp( 0.0 ),
			EvapInletTemp( 0.0 ),
			CondOutletTemp( 0.0 ),
			EvapOutletTemp( 0.0 ),
			Evapmdot( 0.0 ),
			Condmdot( 0.0 ),
			Genmdot( 0.0 ),
			SteamMdot( 0.0 ),
			ActualCOP( 0.0 ),
			ChillerPartLoadRatio( 0.0 ),
			ChillerCyclingFrac( 0.0 ),
			LoopLoss( 0.0 )
		{}

		// Member Constructor
		ReportVars(
			Real64 const PumpingPower, // reporting: W - electric pumping power
			Real64 const QGenerator, // reporting: W - steam heat transfer rate
			Real64 const QEvap, // reporting: W - evaporator heat transfer rate
			Real64 const QCond, // reporting: W - condensor heat transfer rate
			Real64 const PumpingEnergy, // reporting: J - electric pumping power
			Real64 const GeneratorEnergy, // reporting: J - steam heat transfer rate
			Real64 const EvapEnergy, // reporting: J - evaporator heat transfer rate
			Real64 const CondEnergy, // reporting: J - condensor heat transfer rate
			Real64 const CondInletTemp, // reporting: C - condenser inlet temperature
			Real64 const EvapInletTemp, // reporting: C - evaporator inlet temperature
			Real64 const CondOutletTemp, // reporting: C - condenser outlet temperature
			Real64 const EvapOutletTemp, // reporting: C - evaporator outlet temperature
			Real64 const Evapmdot, // reporting: kg/ - evaporator mass flow rate
			Real64 const Condmdot, // reporting: kg/ - condenser mass flow rate
			Real64 const Genmdot, // reporting: generatore mass flow rate when connected to plant
			Real64 const SteamMdot, // reporting: kg/s - steam mass flow rate
			Real64 const ActualCOP, // reporting: coefficient of performance = QEvap/QGenerator
			Real64 const ChillerPartLoadRatio, // reporting: part-load ratio
			Real64 const ChillerCyclingFrac, // reporting: chiller on/off cycling fraction
			Real64 const LoopLoss // reporting: W - loop loss from absorber outlet to condensate pump inlet
		) :
			PumpingPower( PumpingPower ),
			QGenerator( QGenerator ),
			QEvap( QEvap ),
			QCond( QCond ),
			PumpingEnergy( PumpingEnergy ),
			GeneratorEnergy( GeneratorEnergy ),
			EvapEnergy( EvapEnergy ),
			CondEnergy( CondEnergy ),
			CondInletTemp( CondInletTemp ),
			EvapInletTemp( EvapInletTemp ),
			CondOutletTemp( CondOutletTemp ),
			EvapOutletTemp( EvapOutletTemp ),
			Evapmdot( Evapmdot ),
			Condmdot( Condmdot ),
			Genmdot( Genmdot ),
			SteamMdot( SteamMdot ),
			ActualCOP( ActualCOP ),
			ChillerPartLoadRatio( ChillerPartLoadRatio ),
			ChillerCyclingFrac( ChillerCyclingFrac ),
			LoopLoss( LoopLoss )
		{}

	};

	// Object Data
	extern FArray1D< IndirectAbsorberSpecs > IndirectAbsorber; // dimension to number of machines
	extern FArray1D< ReportVars > IndirectAbsorberReport;

	// Functions

	void
	SimIndirectAbsorber(
		std::string const & AbsorberType, // type of Absorber
		std::string const & AbsorberName, // user specified name of Absorber
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int const LoopNum, // Plant loop index for where called from
		int const LoopSide, // Plant loop side index for where called from
		int & CompIndex, // Chiller number pointer
		bool const RunFlag, // simulate Absorber when TRUE
		bool const FirstIteration, // initialize variables when TRUE
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 & MyLoad, // loop demand component will meet
		Real64 & MaxCap, // W - maximum operating capacity of Absorber
		Real64 & MinCap, // W - minimum operating capacity of Absorber
		Real64 & OptCap, // W - optimal operating capacity of Absorber
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor, // sizing factor
		Real64 & TempCondInDesign
	);

	// End Absorption Chiller Module Driver Subroutines
	//******************************************************************************

	// Beginning of Absorption Chiller Module Get Input subroutines
	//******************************************************************************

	void
	GetIndirectAbsorberInput();

	// End of Get Input subroutines for the Absorption Chiller Module
	//******************************************************************************

	void
	InitIndirectAbsorpChiller(
		int const ChillNum, // number of the current electric chiller being simulated
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad // requested load
	);

	void
	SizeIndirectAbsorpChiller( int const ChillNum );

	// Beginning of Absorber model Subroutines
	// *****************************************************************************

	void
	CalcIndirectAbsorberModel(
		int const ChillNum, // Absorber number
		Real64 const MyLoad, // operating load
		bool const RunFlag, // TRUE when Absorber operating
		bool const FirstIteration, // TRUE when first iteration of timestep !unused1208
		int const EquipFlowCtrl // Flow control mode for the equipment
	);

	// End of Absorption Chiller Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

	void
	UpdateIndirectAbsorberRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if Absorber operating
		int const Num // Absorber number
	);

	// End of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

} // ChillerIndirectAbsorption

} // EnergyPlus

#endif
