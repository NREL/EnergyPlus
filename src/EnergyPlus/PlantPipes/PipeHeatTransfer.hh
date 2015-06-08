#ifndef PipeHeatTransfer_hh_INCLUDED
#define PipeHeatTransfer_hh_INCLUDED

// C++ Headers
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array4D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <DataPlant.hh>
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <PlantLocation.hh>
#include <PlantComponent.hh>

namespace EnergyPlus {

namespace PipeHeatTransfer {

	extern int const None;
	extern int const ZoneEnv;
	extern int const ScheduleEnv;
	extern int const OutsideAirEnv;
	extern int const GroundEnv;

	extern int const PreviousTimeIndex;
	extern int const CurrentTimeIndex;
	extern int const TentativeTimeIndex;

	extern Real64 const InnerDeltaTime; // one minute time step in seconds

	class PipeHTData : public PlantComponent
	{

		// Members
		// Input data
		std::string Name; // name of the component
		std::string Construction; // construction object name
		std::string Environment; // keyword:  'Schedule', 'OutdoorAir', 'Zone'
		std::string EnvrSchedule; // temperature schedule for environmental temp
		std::string EnvrVelSchedule; // temperature schedule for environmental temp
		std::string EnvrZone; // zone providing environmental temp
		std::string EnvrAirNode; // outside air node providing environmental temp
		Real64 Length; // total pipe length [m]
		Real64 PipeID; // pipe inside diameter [m]
		std::string InletNode; // inlet node name
		std::string OutletNode; // outlet node name
		int InletNodeNum; // inlet node number
		int OutletNodeNum; // outlet node number
		// derived data
		int ConstructionNum; // construction ref number
		int EnvironmentPtr;
		int EnvrSchedPtr; // pointer to schedule used to set environmental temp
		int EnvrVelSchedPtr; // pointer to schedule used to set environmental temp
		int EnvrZonePtr; // pointer to zone number used to set environmental temp
		int EnvrAirNodeNum; // pointer to outside air node used to set environmental temp
		int NumSections; // total number of nodes along pipe length
		Real64 FluidSpecHeat; // fluid Cp [J/kg.K]
		Real64 FluidDensity; // density [kg/m3]
		Real64 MaxFlowRate; // max flow rate (from loop/node data)
		Real64 FluidSectionVol; // volume of each pipe section (node) [m^3]
		Real64 InsideArea; // pipe section inside surface area [m^2]
		Real64 OutsideArea; // pipe section outside surface area [m^2]
		Real64 SectionArea; // cross sectional area [m^2]
		Real64 PipeHeatCapacity; // heat capacity of pipe section [J/m.K]
		Real64 PipeOD; // pipe outside diameter [m]
		Real64 PipeCp; // pipe materail Cp [J/kg.K]
		Real64 PipeDensity; // pipe material density [kg/m3]
		Real64 PipeConductivity; // pipe material thermal conductivity [W/m.K]
		Real64 InsulationOD; // insulation outside diameter [m]
		Real64 InsulationCp; // insulation  specific heat [J/kg.K]
		Real64 InsulationDensity; // insulation density [kg/m3]
		Real64 InsulationConductivity; // insulation conductivity [W/m.K]
		Real64 InsulationThickness; // insulation thickness [m]
		Real64 InsulationResistance; // Insulation thermal resistance [m2.K/W]
		Real64 CurrentSimTime; // Current simulation time [hr]
		Real64 PreviousSimTime; // simulation time the report data was last updated
		Real64 DeltaTime; // system time step in seconds
		int NumInnerTimeSteps; // number of time steps to perform per hvac time step
		Array1D< Real64 > TentativeFluidTemp;
		Array1D< Real64 > FluidTemp; // arrays for fluid and pipe temperatures at each node
		Array1D< Real64 > PreviousFluidTemp;
		Array1D< Real64 > TentativePipeTemp;
		Array1D< Real64 > PipeTemp;
		Array1D< Real64 > PreviousPipeTemp;
		int NumDepthNodes; // number of soil grid points in the depth direction
		int PipeNodeDepth; // soil depth grid point where pipe is located
		int PipeNodeWidth; // soil width grid point where pipe is located
		Real64 PipeDepth; // pipe burial depth [m]
		Real64 DomainDepth; // soil grid depth [m]
		Real64 dSregular; // grid spacing in cartesian domain [m]
		Real64 OutdoorConvCoef; // soil to air convection coefficient [W/m2.K]
		std::string SoilMaterial; // name of soil material:regular object
		int SoilMaterialNum; // soil material index in material data structure
		int MonthOfMinSurfTemp; // month of minimum ground surface temperature
		Real64 AvgGroundTemp; // annual average ground temperature [C]
		Real64 AvgGndTempAmp; // annual average amplitude of gnd temp [C]
		int PhaseShiftDays; // shift of minimum gnd surf temp from 1/1  [days]
		Real64 MinSurfTemp; // minimum annual surface temperature [C]
		Real64 SoilDensity; // density of soil [kg/m3]
		Real64 SoilDepth; // thickness of soil [m]
		Real64 SoilCp; // specific heat of soil [J/kg.K]
		Real64 SoilConductivity; // thermal conductivity of soil [W/m.K]
		Real64 SoilRoughness; // ground surface roughness
		Real64 SoilThermAbs; // ground surface thermal absorptivity
		Real64 SoilSolarAbs; // ground surface solar absorptivity
		Real64 CoefS1; // soil surface finite difference coefficient
		Real64 CoefS2; // soil surface finite difference coefficient
		Real64 CoefA1; // soil finite difference coefficient
		Real64 CoefA2; // soil finite difference coefficient
		Real64 FourierDS; // soil Fourier number based on grid spacing
		Real64 SoilDiffusivity; // soil thermal diffusivity [m2/s]
		Real64 SoilDiffusivityPerDay; // soil thermal diffusivity [m2/day]
		int AvgAnnualManualInput; // flag for method of bringing in annual avg data yes-1 no-0
		Array4D< Real64 > T; // soil temperature array
		bool SolarExposed; // Flag to determine if solar is included at ground surface
		Real64 SumTK; // Sum of thickness/conductivity over all material layers
		Real64 ZoneHeatGainRate; // Lagged energy summation for zone heat gain {W}
		int LoopNum; // PlantLoop index where this pipe lies
		int LoopSideNum; // PlantLoop%LoopSide index where this pipe lies
		int BranchNum; // ..LoopSide%Branch index where this pipe lies
		int CompNum; // ..Branch%Comp index where this pipe lies
		Real64 EnvironmentTemp; // environmental temperature (surrounding pipe)
		// Report data
		Real64 FluidInletTemp; // inlet temperature [C]
		Real64 FluidOutletTemp; // outlet temperature [C]
		Real64 MassFlowRate; // mass flow rate [kg/s]
		Real64 FluidHeatLossRate; // overall heat transfer rate from fluid to pipe [W]
		Real64 FluidHeatLossEnergy; // energy transferred from fluid to pipe [J]
		Real64 PipeInletTemp; // pipe temperature at inlet [C]
		Real64 PipeOutletTemp; // pipe temperature at Oulet [C]
		Real64 EnvironmentHeatLossRate; // overall heat transfer rate from pipe to environment [W]
		Real64 EnvHeatLossEnergy; // energy transferred from pipe to environment [J]
		Real64 EnvHeatLossRate; // energy rate transferred from pipe to environment [W]
		Real64 VolumeFlowRate;
		
		// Default Constructor
		PipeHTData() :
			Length( 0.0 ),
			PipeID( 0.0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			ConstructionNum( 0 ),
			EnvironmentPtr( 0 ),
			EnvrSchedPtr( 0 ),
			EnvrVelSchedPtr( 0 ),
			EnvrZonePtr( 0 ),
			EnvrAirNodeNum( 0 ),
			NumSections( 0 ),
			FluidSpecHeat( 0.0 ),
			FluidDensity( 0.0 ),
			MaxFlowRate( 0.0 ),
			FluidSectionVol( 0.0 ),
			InsideArea( 0.0 ),
			OutsideArea( 0.0 ),
			SectionArea( 0.0 ),
			PipeHeatCapacity( 0.0 ),
			PipeOD( 0.0 ),
			PipeCp( 0.0 ),
			PipeDensity( 0.0 ),
			PipeConductivity( 0.0 ),
			InsulationOD( 0.0 ),
			InsulationCp( 0.0 ),
			InsulationDensity( 0.0 ),
			InsulationConductivity( 0.0 ),
			InsulationThickness( 0.0 ),
			InsulationResistance( 0.0 ),
			CurrentSimTime( 0.0 ),
			PreviousSimTime( 0.0 ),
			DeltaTime( 0.0 ),
			NumInnerTimeSteps( 0.0 ),
			NumDepthNodes( 0 ),
			PipeNodeDepth( 0 ),
			PipeNodeWidth( 0 ),
			PipeDepth( 0.0 ),
			DomainDepth( 0.0 ),
			dSregular( 0.0 ),
			OutdoorConvCoef( 0.0 ),
			SoilMaterialNum( 0 ),
			MonthOfMinSurfTemp( 0 ),
			AvgGroundTemp( 0.0 ),
			AvgGndTempAmp( 0.0 ),
			PhaseShiftDays( 0 ),
			MinSurfTemp( 0.0 ),
			SoilDensity( 0.0 ),
			SoilDepth( 0.0 ),
			SoilCp( 0.0 ),
			SoilConductivity( 0.0 ),
			SoilRoughness( 0.0 ),
			SoilThermAbs( 0.0 ),
			SoilSolarAbs( 0.0 ),
			CoefS1( 0.0 ),
			CoefS2( 0.0 ),
			CoefA1( 0.0 ),
			CoefA2( 0.0 ),
			FourierDS( 0.0 ),
			SoilDiffusivity( 0.0 ),
			SoilDiffusivityPerDay( 0.0 ),
			AvgAnnualManualInput( 0 ),
			SolarExposed( true ),
			SumTK( 0.0 ),
			ZoneHeatGainRate( 0.0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			EnvironmentTemp( 0.0 ),
			// report variables
			FluidInletTemp( 0.0 ),
			FluidOutletTemp( 0.0 ),
			MassFlowRate( 0.0 ),
			FluidHeatLossRate( 0.0 ),
			FluidHeatLossEnergy( 0.0 ),
			PipeInletTemp( 0.0 ),
			PipeOutletTemp( 0.0 ),
			EnvironmentHeatLossRate( 0.0 ),
			EnvHeatLossEnergy( 0.0 ),
			EnvHeatLossRate( 0.0 ),
			VolumeFlowRate( 0.0 )
		{}

	public:
		static std::shared_ptr< PlantComponent >
		pipeHTFactory(
			int objectType,
			std::string objectName
		);
	
	private: 

		// methods implemented from base class

		int
		performEveryTimeInit( const PlantLocation & calledFromLocation );

		int
		performOneTimeInit( const PlantLocation & calledFromLocation );

		int
		performBeginEnvrnInit( const PlantLocation & calledFromLocation );

		int
		performFirstHVACInit( const PlantLocation & calledFromLocation );

		int
		performInitLoopEquip( const PlantLocation & calledFromLocation );

		int
		simulate( const PlantLocation & calledFromLocation, bool const & FirstHVACIteration );

		// additional worker methods 

		void
		pushInnerTimeStepArrays();

		int
		validatePipeConstruction();

		void
		initPipesHeatTransfer();
		
		void
		initializeHeatTransferPipes();
		
		void
		calcPipesHeatTransfer(int LengthIndex = -1);

		void
		calcBuriedPipeSoil();

		void
		updatePipesHeatTransfer();

		void
		reportPipesHeatTransfer();

		void
		calcZonePipesHeatGain();

		Real64
		calcPipeHeatTransCoef();

		Real64
		outsidePipeHeatTransCoef(); 

		Real64
		farfieldTemperature(
			Real64 const z, // Current Depth
			Real64 const DayOfSim // Current Simulation Day
		);

	};

	// Object Data
	extern Array1D< std::shared_ptr< PipeHTData > > PipeHT;


	//==============================================================================



	//===============================================================================

	//===============================================================================

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

} // PipeHeatTransfer

} // EnergyPlus

#endif
