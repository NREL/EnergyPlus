#ifndef PlantComponentTemperatureSources_hh_INCLUDED
#define PlantComponentTemperatureSources_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataPlant.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace PlantComponentTemperatureSources {

	// Using/Aliasing
	using DataPlant::PlantLocation;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	extern int const TempSpecType_Constant;
	extern int const TempSpecType_Schedule;

	//MODULE DERIVED TYPE DEFINITIONS:

	//MODULE VARIABLES
	extern int NumSources;
	extern bool GetInput; // then TRUE, calls subroutine to read input file.

	//MODULE ROUTINES

	// Types

	struct WaterSourceSpecs
	{
		// Members
		std::string Name; // user identifier
		int InletNodeNum; // Node number on the inlet side of the plant
		int OutletNodeNum; // Node number on the outlet side of the plant
		Real64 DesVolFlowRate; // m**3/s - design nominal volumetric flow rate
		bool DesVolFlowRateWasAutoSized; // true if desing flow rate was autosized on input
		Real64 MassFlowRateMax; // kg/s - design mass flow rate
		bool EMSOverrideOnMassFlowRateMax; // if true EMS is calling to override maximum mass flow
		Real64 EMSOverrideValueMassFlowRateMax; // value to use if EMS is overriding max mass flow
		Real64 MassFlowRate;
		int TempSpecType; // temperature specification type
		std::string TempSpecScheduleName;
		int TempSpecScheduleNum;
		Real64 BoundaryTemp;
		Real64 OutletTemp; // may be different if the flow is off
		Real64 InletTemp;
		Real64 HeatRate;
		Real64 HeatEnergy;
		PlantLocation Location;
		Real64 SizFac; // sizing factor
		bool CheckEquipName;
		bool MyFlag;
		bool MyEnvironFlag;
		bool IsThisSized; // TRUE if sizing is done

		// Default Constructor
		WaterSourceSpecs() :
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			DesVolFlowRate( 0.0 ),
			DesVolFlowRateWasAutoSized( false ),
			MassFlowRateMax( 0.0 ),
			EMSOverrideOnMassFlowRateMax( false ),
			EMSOverrideValueMassFlowRateMax( 0.0 ),
			MassFlowRate( 0.0 ),
			TempSpecType( 0 ),
			TempSpecScheduleNum( 0 ),
			BoundaryTemp( 0.0 ),
			OutletTemp( 0.0 ),
			InletTemp( 0.0 ),
			HeatRate( 0.0 ),
			HeatEnergy( 0.0 ),
			Location( 0, 0, 0, 0 ),
			SizFac( 0.0 ),
			CheckEquipName( true ),
			MyFlag( true ),
			MyEnvironFlag( true ),
			IsThisSized( false )
		{}

	};

	// Object Data
	extern Array1D< WaterSourceSpecs > WaterSource; // dimension to number of machines

	// Functions

	void
	SimWaterSource(
		std::string const & SourceName, // user-specified name for this component
		int const EquipFlowCtrl, // Flow control mode for the equipment
		int & CompIndex, // HX number pointer
		bool const RunFlag, // simulate HX when TRUE
		bool const FirstHVACIteration, // initialize variables when TRUE
		bool & InitLoopEquip, // If not zero, calculate the max load for operating conditions
		Real64 & MyLoad, // loop demand component will meet
		Real64 & MaxLoad,
		Real64 & MinLoad,
		Real64 & OptLoad,
		bool const GetSizingFactor, // TRUE when just the sizing factor is requested
		Real64 & SizingFactor // sizing factor
	);

	void
	GetWaterSource();

	void
	InitWaterSource(
		int const SourceNum, // number of the current component being simulated
		bool const RunFlag, // TRUE when component operating
		Real64 const MyLoad,
		bool const FirstHVACIteration // initialize variables when TRUE
	);

	void
	SizeWaterSource( int const SourceNum );

	void
	CalcWaterSource(
		int const SourceNum,
		Real64 const MyLoad,
		bool const RunFlag,
		int const EquipFlowCtrl // Flow control mode for the equipment
	);

	void
	UpdateWaterSource( int const SourceNum );

	// End of Record Keeping subroutines for the Const COP Chiller Module
	// *****************************************************************************

} // PlantComponentTemperatureSources

} // EnergyPlus

#endif
