#ifndef ChillerAbsorption_hh_INCLUDED
#define ChillerAbsorption_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ChillerAbsorption {

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
	extern int NumBLASTAbsorbers; // number of Absorption Chillers specified in input

	extern Real64 CondMassFlowRate; // Kg/s - condenser mass flow rate, water side
	extern Real64 EvapMassFlowRate; // Kg/s - evaporator mass flow rate, water side
	extern Real64 SteamMassFlowRate; // Kg/s - steam mass flow rate, water side
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

	extern FArray1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE:

	// Types

	struct BLASTAbsorberSpecs
	{
		// Members
		std::string Name; // user identifier
		bool Available; // need an array of logicals--load identifiers of available equipment
		bool ON; // simulate the machine at it's operating part load ratio
		Real64 NomCap; // W - design nominal capacity of Absorber
		Real64 NomPumpPower; // W - design nominal capacity of Absorber
		int FlowMode; // one of 3 modes for componet flow during operation
		bool ModulatedFlowSetToLoop; // True if the setpoint is missing at the outlet node
		bool ModulatedFlowErrDone; // true if setpoint warning issued
		Real64 EvapVolFlowRate; // m3/s - design water volumetric flow rate through the evaporator
		Real64 CondVolFlowRate; // m3/s - design water volumetric flow rate through the condenser
		Real64 EvapMassFlowRateMax; // Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
		Real64 CondMassFlowRateMax; // Max Design Condeneser Mass Flow Rate [kg/s]
		Real64 GenMassFlowRateMax; // Max Design Generator Mass Flow Rate converted from Volume Flow Rate
		Real64 SizFac; // Sizing factor
		int EvapInletNodeNum; // Node number on the inlet side of the plant
		int EvapOutletNodeNum; // Node number on the outlet side of the plant
		int CondInletNodeNum; // Node number on the inlet side of the condenser
		int CondOutletNodeNum; // Node number on the outlet side of the condenser
		int GeneratorInletNodeNum; // absorber steam inlet node number, water side
		int GeneratorOutletNodeNum; // absorber steam outlet node number, water side
		Real64 MinPartLoadRat; // (BLAST MIN) min allowed operating frac full load
		Real64 MaxPartLoadRat; // (BLAST MAX) max allowed operating frac full load
		Real64 OptPartLoadRat; // (BLAST BEST) optimal operating frac full load
		Real64 TempDesCondIn; // C - (BLAST ADJTC(1)The design secondary loop fluid
		// temperature at the Absorber condenser side inlet
		FArray1D< Real64 > SteamLoadCoef; // (BLAST RPWRC() ) coeff of full load poly. fit
		FArray1D< Real64 > PumpPowerCoef; // coeff of pumping power poly. fit
		Real64 TempLowLimitEvapOut; // C - low temperature shut off
		int ErrCount2; // error counter
		int GenHeatSourceType; // Generator heat source type, NodeType_Steam=3 or NodeType_Water=2
		Real64 GeneratorVolFlowRate; // m3/s - hot water volumetric flow rate through generator
		Real64 GeneratorSubcool; // amount of subcooling in steam generator
		int SteamFluidIndex; // index to generator fluid type
		Real64 GeneratorDeltaTemp; // C - generator fluid temperature difference (water only)
		int CWLoopNum; // chilled water plant loop index number
		int CWLoopSideNum; // chilled water plant loop side index
		int CWBranchNum; // chilled water plant loop branch index
		int CWCompNum; // chilled water plant loop component index
		int CDLoopNum; // condenser water plant loop index number
		int CDLoopSideNum; // condenser water plant loop side index
		int CDBranchNum; // condenser water plant loop branch index
		int CDCompNum; // condenser water plant loop component index
		int GenLoopNum; // generator water plant loop index number
		int GenLoopSideNum; // generator water plant loop side index
		int GenBranchNum; // generator water plant loop branch index
		int GenCompNum; // generator water plant loop component index
		bool PossibleSubcooling; // flag to indicate chiller is doing less cooling that requested
		bool IsThisSized; // TRUE if sizing is done

		// Default Constructor
		BLASTAbsorberSpecs() :
			Available( false ),
			ON( false ),
			NomCap( 0.0 ),
			NomPumpPower( 0.0 ),
			FlowMode( FlowModeNotSet ),
			ModulatedFlowSetToLoop( false ),
			ModulatedFlowErrDone( false ),
			EvapVolFlowRate( 0.0 ),
			CondVolFlowRate( 0.0 ),
			EvapMassFlowRateMax( 0.0 ),
			CondMassFlowRateMax( 0.0 ),
			GenMassFlowRateMax( 0.0 ),
			SizFac( 0.0 ),
			EvapInletNodeNum( 0 ),
			EvapOutletNodeNum( 0 ),
			CondInletNodeNum( 0 ),
			CondOutletNodeNum( 0 ),
			GeneratorInletNodeNum( 0 ),
			GeneratorOutletNodeNum( 0 ),
			MinPartLoadRat( 0.0 ),
			MaxPartLoadRat( 0.0 ),
			OptPartLoadRat( 0.0 ),
			TempDesCondIn( 0.0 ),
			SteamLoadCoef( 3, 0.0 ),
			PumpPowerCoef( 3, 0.0 ),
			TempLowLimitEvapOut( 0.0 ),
			ErrCount2( 0 ),
			GenHeatSourceType( 0 ),
			GeneratorVolFlowRate( 0.0 ),
			GeneratorSubcool( 0.0 ),
			SteamFluidIndex( 0 ),
			GeneratorDeltaTemp( -99999.0 ),
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
		BLASTAbsorberSpecs(
			std::string const & Name, // user identifier
			bool const Available, // need an array of logicals--load identifiers of available equipment
			bool const ON, // simulate the machine at it's operating part load ratio
			Real64 const NomCap, // W - design nominal capacity of Absorber
			Real64 const NomPumpPower, // W - design nominal capacity of Absorber
			int const FlowMode, // one of 3 modes for componet flow during operation
			bool const ModulatedFlowSetToLoop, // True if the setpoint is missing at the outlet node
			bool const ModulatedFlowErrDone, // true if setpoint warning issued
			Real64 const EvapVolFlowRate, // m3/s - design water volumetric flow rate through the evaporator
			Real64 const CondVolFlowRate, // m3/s - design water volumetric flow rate through the condenser
			Real64 const EvapMassFlowRateMax, // Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
			Real64 const CondMassFlowRateMax, // Max Design Condeneser Mass Flow Rate [kg/s]
			Real64 const GenMassFlowRateMax, // Max Design Generator Mass Flow Rate converted from Volume Flow Rate
			Real64 const SizFac, // Sizing factor
			int const EvapInletNodeNum, // Node number on the inlet side of the plant
			int const EvapOutletNodeNum, // Node number on the outlet side of the plant
			int const CondInletNodeNum, // Node number on the inlet side of the condenser
			int const CondOutletNodeNum, // Node number on the outlet side of the condenser
			int const GeneratorInletNodeNum, // absorber steam inlet node number, water side
			int const GeneratorOutletNodeNum, // absorber steam outlet node number, water side
			Real64 const MinPartLoadRat, // (BLAST MIN) min allowed operating frac full load
			Real64 const MaxPartLoadRat, // (BLAST MAX) max allowed operating frac full load
			Real64 const OptPartLoadRat, // (BLAST BEST) optimal operating frac full load
			Real64 const TempDesCondIn, // C - (BLAST ADJTC(1)The design secondary loop fluid
			FArray1< Real64 > const & SteamLoadCoef, // (BLAST RPWRC() ) coeff of full load poly. fit
			FArray1< Real64 > const & PumpPowerCoef, // coeff of pumping power poly. fit
			Real64 const TempLowLimitEvapOut, // C - low temperature shut off
			int const ErrCount2, // error counter
			int const GenHeatSourceType, // Generator heat source type, NodeType_Steam=3 or NodeType_Water=2
			Real64 const GeneratorVolFlowRate, // m3/s - hot water volumetric flow rate through generator
			Real64 const GeneratorSubcool, // amount of subcooling in steam generator
			int const SteamFluidIndex, // index to generator fluid type
			Real64 const GeneratorDeltaTemp, // C - generator fluid temperature difference (water only)
			int const CWLoopNum, // chilled water plant loop index number
			int const CWLoopSideNum, // chilled water plant loop side index
			int const CWBranchNum, // chilled water plant loop branch index
			int const CWCompNum, // chilled water plant loop component index
			int const CDLoopNum, // condenser water plant loop index number
			int const CDLoopSideNum, // condenser water plant loop side index
			int const CDBranchNum, // condenser water plant loop branch index
			int const CDCompNum, // condenser water plant loop component index
			int const GenLoopNum, // generator water plant loop index number
			int const GenLoopSideNum, // generator water plant loop side index
			int const GenBranchNum, // generator water plant loop branch index
			int const GenCompNum, // generator water plant loop component index
			bool const PossibleSubcooling, // flag to indicate chiller is doing less cooling that requested
			bool const IsThisSized // TRUE if sizing is done
		) :
			Name( Name ),
			Available( Available ),
			ON( ON ),
			NomCap( NomCap ),
			NomPumpPower( NomPumpPower ),
			FlowMode( FlowMode ),
			ModulatedFlowSetToLoop( ModulatedFlowSetToLoop ),
			ModulatedFlowErrDone( ModulatedFlowErrDone ),
			EvapVolFlowRate( EvapVolFlowRate ),
			CondVolFlowRate( CondVolFlowRate ),
			EvapMassFlowRateMax( EvapMassFlowRateMax ),
			CondMassFlowRateMax( CondMassFlowRateMax ),
			GenMassFlowRateMax( GenMassFlowRateMax ),
			SizFac( SizFac ),
			EvapInletNodeNum( EvapInletNodeNum ),
			EvapOutletNodeNum( EvapOutletNodeNum ),
			CondInletNodeNum( CondInletNodeNum ),
			CondOutletNodeNum( CondOutletNodeNum ),
			GeneratorInletNodeNum( GeneratorInletNodeNum ),
			GeneratorOutletNodeNum( GeneratorOutletNodeNum ),
			MinPartLoadRat( MinPartLoadRat ),
			MaxPartLoadRat( MaxPartLoadRat ),
			OptPartLoadRat( OptPartLoadRat ),
			TempDesCondIn( TempDesCondIn ),
			SteamLoadCoef( 3, SteamLoadCoef ),
			PumpPowerCoef( 3, PumpPowerCoef ),
			TempLowLimitEvapOut( TempLowLimitEvapOut ),
			ErrCount2( ErrCount2 ),
			GenHeatSourceType( GenHeatSourceType ),
			GeneratorVolFlowRate( GeneratorVolFlowRate ),
			GeneratorSubcool( GeneratorSubcool ),
			SteamFluidIndex( SteamFluidIndex ),
			GeneratorDeltaTemp( GeneratorDeltaTemp ),
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
		Real64 PumpingPower; // reporting: electric pumping power
		Real64 QGenerator; // reporting: steam heat transfer rate
		Real64 QEvap; // reporting: evaporator heat transfer rate
		Real64 QCond; // reporting: condensor heat transfer rate
		Real64 PumpingEnergy; // reporting: electric pumping power
		Real64 GeneratorEnergy; // reporting: steam heat transfer rate
		Real64 EvapEnergy; // reporting: evaporator heat transfer rate
		Real64 CondEnergy; // reporting: condensor heat transfer rate
		Real64 CondInletTemp; // reporting: condenser inlet temperature
		Real64 EvapInletTemp; // reporting: evaporator inlet temperature
		Real64 CondOutletTemp; // reporting: condenser outlet temperature
		Real64 EvapOutletTemp; // reporting: evaporator outlet temperature
		Real64 Evapmdot; // reporting: evaporator mass flow rate
		Real64 Condmdot; // reporting: condenser mass flow rate
		Real64 Genmdot; // reporting: generatore mass flow rate when connected to plant
		Real64 SteamMdot; // reporting: steam mass flow rate
		Real64 ActualCOP; // reporting: coefficient of performance = QEvap/QGenerator

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
			ActualCOP( 0.0 )
		{}

		// Member Constructor
		ReportVars(
			Real64 const PumpingPower, // reporting: electric pumping power
			Real64 const QGenerator, // reporting: steam heat transfer rate
			Real64 const QEvap, // reporting: evaporator heat transfer rate
			Real64 const QCond, // reporting: condensor heat transfer rate
			Real64 const PumpingEnergy, // reporting: electric pumping power
			Real64 const GeneratorEnergy, // reporting: steam heat transfer rate
			Real64 const EvapEnergy, // reporting: evaporator heat transfer rate
			Real64 const CondEnergy, // reporting: condensor heat transfer rate
			Real64 const CondInletTemp, // reporting: condenser inlet temperature
			Real64 const EvapInletTemp, // reporting: evaporator inlet temperature
			Real64 const CondOutletTemp, // reporting: condenser outlet temperature
			Real64 const EvapOutletTemp, // reporting: evaporator outlet temperature
			Real64 const Evapmdot, // reporting: evaporator mass flow rate
			Real64 const Condmdot, // reporting: condenser mass flow rate
			Real64 const Genmdot, // reporting: generatore mass flow rate when connected to plant
			Real64 const SteamMdot, // reporting: steam mass flow rate
			Real64 const ActualCOP // reporting: coefficient of performance = QEvap/QGenerator
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
			ActualCOP( ActualCOP )
		{}

	};

	// Object Data
	extern FArray1D< BLASTAbsorberSpecs > BLASTAbsorber; // dimension to number of machines
	extern FArray1D< ReportVars > BLASTAbsorberReport;

	// Functions

	void
	SimBLASTAbsorber(
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
		Real64 & MaxCap, // Maximum operating capacity of chiller [W]
		Real64 & MinCap, // Minimum operating capacity of chiller [W]
		Real64 & OptCap, // Optimal operating capacity of chiller [W]
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor, // sizing factor
		Real64 & TempCondInDesign
	);

	// End Absorption Chiller Module Driver Subroutines
	//******************************************************************************

	// Beginning of Absorption Chiller Module Get Input subroutines
	//******************************************************************************

	void
	GetBLASTAbsorberInput();

	// End of Get Input subroutines for the Absorption Chiller Module
	//******************************************************************************

	void
	InitBLASTAbsorberModel(
		int const ChillNum, // number of the current electric chiller being simulated
		bool const RunFlag, // TRUE when chiller operating
		Real64 const MyLoad
	);

	void
	SizeAbsorpChiller( int const ChillNum );

	// Beginning of Absorber model Subroutines
	// *****************************************************************************

	void
	CalcBLASTAbsorberModel(
		int & ChillNum, // Absorber number
		Real64 & MyLoad, // operating load
		bool const RunFlag, // TRUE when Absorber operating
		bool const FirstIteration, // TRUE when first iteration of timestep !unused1208
		int const EquipFlowCtrl // Flow control mode for the equipment
	);

	// End of Absorption Chiller Module Utility Subroutines
	// *****************************************************************************

	// Beginning of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

	void
	UpdateBLASTAbsorberRecords(
		Real64 const MyLoad, // current load
		bool const RunFlag, // TRUE if Absorber operating
		int const Num // Absorber number
	);

	// End of Record Keeping subroutines for the Absorption Chiller Module
	// *****************************************************************************

} // ChillerAbsorption

} // EnergyPlus

#endif
