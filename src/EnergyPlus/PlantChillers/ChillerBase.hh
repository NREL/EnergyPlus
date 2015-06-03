#ifndef CHILLERBASE_HH_INCLUDED
#define CHILLERBASE_HH_INCLUDED

#include <DataPlant.hh>
#include <PlantComponent.hh>
#include <PlantLocation.hh>

namespace EnergyPlus {

// condenser types
enum ChillerCondenserType { AIRCOOLED, WATERCOOLED, EVAPCOOLED };

// chiller flow modes
enum ChillerFlowMode { NOTSET, CONSTANTFLOW, NOTMODULATED, LEAVINGSETPOINTMODULATED };

// base class for all chillers
class ChillerBase : public PlantComponent
{
	// Members
	ChillerCondenserType condenserType; // Type of Condenser - Air or Water Cooled
	Real64 NomCap; // design nominal capacity of chiller
	bool NomCapWasAutoSized; // true if NomCap was autosize on input
	Real64 COP; // COP
	ChillerFlowMode flowMode; // one of 3 modes for componet flow during operation
	bool ModulatedFlowSetToLoop; // True if the setpoint is missing at the outlet node
	bool ModulatedFlowErrDone; // true if setpoint warning issued
	bool HRSPErrDone; // TRUE if set point warning issued for heat recovery loop
	int EvapInletNodeNum; // Node number on the inlet side of the plant
	int EvapOutletNodeNum; // Node number on the outlet side of the plant
	int CondInletNodeNum; // Node number on the inlet side of the condenser
	int CondOutletNodeNum; // Node number on the outlet side of the condenser
	Real64 EvapVolFlowRate; // m**3/s - design nominal water volumetric flow rate through the evaporator
	bool EvapVolFlowRateWasAutoSized; //true if autosized design evap flow rate on input
	Real64 EvapMassFlowRateMax; // kg/s - design water mass flow rate through evaporator
	Real64 CondVolFlowRate; // m**3/s - design nominal water volumetric flow rate through the condenser
	bool CondVolFlowRateWasAutoSized; // true if previous was autosized
	Real64 CondMassFlowRateMax; // kg/s - design water mass flow rate through condenser
	PlantLocation chwLocation; // chilled water loop location
	PlantLocation condLocation; // condenser loop location
	Real64 SizFac; // sizing factor
	Real64 BasinHeaterPowerFTempDiff; // Basin heater capacity per degree C below setpoint (W/C)
	Real64 BasinHeaterSetPointTemp; // Setpoint temperature for basin heater operation (C)
	int BasinHeaterSchedulePtr; // Pointer to basin heater schedule
	int ErrCount1; // for recurring error messages
	int ErrCount2; // for recurring error messages
	std::string MsgBuffer1; // - buffer to print warning messages on following time step
	std::string MsgBuffer2; // - buffer to print warning messages on following time step
	Real64 MsgDataLast; // value of data when warning occurred (passed to Recurring Warn)
	bool PrintMessage; // logical to determine if message is valid
	int MsgErrorCount; // number of occurrences of warning
	bool CheckEquipName;
	bool PossibleSubcooling; // flag to indicate chiller is doing less cooling that requested
	int CondMassFlowIndex;

	// Default Constructor
	ChillerBase() :
		condenserType( AIRCOOLED ),
		NomCap( 0.0 ),
		NomCapWasAutoSized( false ),
		COP( 0.0 ),
		flowMode( NOTSET ),
		ModulatedFlowSetToLoop( false ),
		ModulatedFlowErrDone( false ),
		HRSPErrDone( false ),
		EvapInletNodeNum( 0 ),
		EvapOutletNodeNum( 0 ),
		CondInletNodeNum( 0 ),
		CondOutletNodeNum( 0 ),
		EvapVolFlowRate( 0.0 ),
		EvapVolFlowRateWasAutoSized( false ),
		EvapMassFlowRateMax( 0.0 ),
		CondVolFlowRate( 0.0 ),
		CondVolFlowRateWasAutoSized( false ),
		CondMassFlowRateMax( 0.0 ),
		chwLocation( PlantLocation( 0, 0, 0, 0 ) ),
		condLocation( PlantLocation( 0, 0, 0, 0 ) ),
		SizFac( 0.0 ),
		BasinHeaterPowerFTempDiff( 0.0 ),
		BasinHeaterSetPointTemp( 0.0 ),
		BasinHeaterSchedulePtr( 0 ),
		ErrCount1( 0 ),
		ErrCount2( 0 ),
		MsgDataLast( 0.0 ),
		PrintMessage( false ),
		MsgErrorCount( 0 ),
		CheckEquipName( true ),
		PossibleSubcooling( false ),
		CondMassFlowIndex( 0 )
	{}
};



class ChillerBaseReportVars
	{
		public:
		// Members
		Real64 Power;
		Real64 QEvap;
		Real64 QCond;
		Real64 Energy;
		Real64 EvapEnergy;
		Real64 CondEnergy;
		Real64 CondInletTemp;
		Real64 EvapInletTemp;
		Real64 CondOutletTemp;
		Real64 EvapOutletTemp;
		Real64 Evapmdot;
		Real64 Condmdot;
		Real64 BasinHeaterPower; // Basin heater power (W)
		Real64 BasinHeaterConsumption; // Basin heater energy consumption (J)

		// Default Constructor
		ChillerBaseReportVars() :
			Power( 0.0 ),
			QEvap( 0.0 ),
			QCond( 0.0 ),
			Energy( 0.0 ),
			EvapEnergy( 0.0 ),
			CondEnergy( 0.0 ),
			CondInletTemp( 0.0 ),
			EvapInletTemp( 0.0 ),
			CondOutletTemp( 0.0 ),
			EvapOutletTemp( 0.0 ),
			Evapmdot( 0.0 ),
			Condmdot( 0.0 ),
			BasinHeaterPower( 0.0 ),
			BasinHeaterConsumption( 0.0 )
		{}
	};

}









#endif
