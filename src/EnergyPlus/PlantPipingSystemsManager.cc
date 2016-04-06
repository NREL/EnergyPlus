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
#include <cassert>
#include <cmath>
#include <memory>
#include <set>
#include <utility>
#include <fstream>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/floops.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <PlantPipingSystemsManager.hh>
#include <BranchNodeConnections.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalSurface.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace PlantPipingSystemsManager {

	// Module containing the routines dealing with the PipingSystems

	// MODULE INFORMATION:
	//       AUTHOR         Edwin Lee
	//       DATE WRITTEN   Summer 2011
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Simulate all cases of plant "piping systems"
	//      PipingSystem:Underground
	//      PipingSystem:Generalized

	// METHODOLOGY EMPLOYED:
	// A 3D mesh is established, with full 3D conduction being employed
	// For ground simulation, moisture content and soil freezing is included
	// The mesh can include any number of pipe circuits placed within the domain
	// The mesh can interact with basement walls also

	// Using/Aliasing
	using DataGlobals::Pi;

	// MODULE PARAMETER DEFINITIONS:
	std::string const ObjName_ug_GeneralDomain( "PipingSystem:Underground:Domain" );
	std::string const ObjName_Circuit( "PipingSystem:Underground:PipeCircuit" );
	std::string const ObjName_Segment( "PipingSystem:Underground:PipeSegment" );
	std::string const ObjName_HorizTrench( "GroundHeatExchanger:HorizontalTrench" );
	std::string const ObjName_ZoneCoupled_Slab( "Site:GroundDomain:Slab" );
	std::string const ObjName_ZoneCoupled_Basement( "Site:GroundDomain:Basement" );
	std::string const ObjName_BESTEST_SurfaceConditions( "Site:GroundDomain:BESTEST:GroundSurfaceConditions" );

	// MODULE VARIABLE DECLARATIONS:
	Array1D< Direction > NeighborFieldCells( 6 );
	Array1D< Direction > NeighborBoundaryCells( 6 );
	Array1D< FullDomainStructureInfo > PipingSystemDomains;
	Array1D< PipeCircuitInfo > PipingSystemCircuits;
	Array1D< PipeSegmentInfo > PipingSystemSegments;

	void
	clear_state()
	{
		PipingSystemDomains.deallocate();
		PipingSystemCircuits.deallocate();
		PipingSystemSegments.deallocate();
		NeighborFieldCells.deallocate();
		NeighborBoundaryCells.deallocate();
	}

	void
	CheckIfAnySlabs()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   May 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		int numSlabsCheck( InputProcessor::GetNumObjectsFound( ObjName_ZoneCoupled_Slab ) );
		DataGlobals::AnySlabsInModel = ( numSlabsCheck > 0 );
	}

	void
	CheckIfAnyBasements()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   May 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		int const numBasementsCheck( InputProcessor::GetNumObjectsFound( ObjName_ZoneCoupled_Basement ) );
		DataGlobals::AnyBasementsInModel = ( numBasementsCheck > 0 );
	}

	void
	SimPipingSystemCircuit(
		std::string const & EquipName, // name of the Pipe Heat Transfer.
		int & EqNum, // index in local derived types for external calling
		bool const EP_UNUSED( FirstHVACIteration ), // component number
		bool const InitLoopEquip
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SimPipingSystems" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true ); // First time, input is "gotten"
		int CircuitNum;
		int DomainNum;

		//Autodesk:Uninit Initialize variables used uninitialized
		DomainNum = 0; //Autodesk:Uninit Force default initialization

		// Read input if necessary
		if ( GetInputFlag ) {
			GetPipingSystemsAndGroundDomainsInput();
			GetInputFlag = false;
		}

		// Look for circuit index
		if ( EqNum == 0 ) {
			CircuitNum = FindItemInList( EquipName, PipingSystemCircuits );
			if ( CircuitNum == 0 ) {
				// Catch any bad names before crashing
				ShowFatalError( RoutineName + ": Piping circuit requested not found=" + EquipName );
			}
			EqNum = CircuitNum;
		} else {
			CircuitNum = EqNum;
			int const NumOfPipeCircuits = isize( PipingSystemCircuits );
			if ( CircuitNum > NumOfPipeCircuits || CircuitNum < 1 ) {
				ShowFatalError( RoutineName + ":  Invalid component index passed=" + TrimSigDigits( DomainNum ) + ", Number of Units=" + TrimSigDigits( NumOfPipeCircuits ) + ", Entered Unit name=" + EquipName ); //Autodesk:Uninit DomainNum was uninitialized
			}
			if ( PipingSystemCircuits( CircuitNum ).CheckEquipName ) {
				if ( EquipName != PipingSystemCircuits( CircuitNum ).Name ) {
					ShowFatalError( RoutineName + ": Invalid component name passed=" + TrimSigDigits( CircuitNum ) + ", Unit name=" + EquipName + ", stored Unit Name for that index=" + PipingSystemCircuits( CircuitNum ).Name );
				}
				PipingSystemCircuits( CircuitNum ).CheckEquipName = false;
			}
		}

		// If we are just initializing data structures, then return
		if ( InitLoopEquip ) return;

		// Retrieve the parent domain index for this pipe circuit
		DomainNum = PipingSystemCircuits( CircuitNum ).ParentDomainIndex;

		// Do any initialization here
		InitPipingSystems( DomainNum, CircuitNum );

		// Update the temperature field
		PerformIterationLoop( DomainNum, CircuitNum );

		// Update outlet nodes, etc.
		UpdatePipingSystems( DomainNum, CircuitNum );

	}

	void
	SimulateGroundDomains(
		bool initOnly
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Spring 2014
		//       MODIFIED       by Sushobhit Acharya, March 2015
		//       RE-ENGINEERED  na

		// Using/Aliasing
		using DataHVACGlobals::SysTimeElapsed;
		using DataGlobals::BeginSimFlag;
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::DayOfSim;
		using DataGlobals::HourOfDay;
		using DataGlobals::TimeStep;
		using DataGlobals::TimeStepZone;
		using DataGlobals::TimeStepZoneSec;
		using DataGlobals::SecInHour;
		using DataGlobals::AnyBasementsInModel;
		using DataGlobals::OutputFileInits;
		using DataHeatBalFanSys::ZTAV;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitAndSimGroundDomain" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true ); // First time, input is "gotten"
		static bool WriteEIOFlag( true ); // Set to false once eio information is written

		static gio::Fmt DomainCellsToEIOHeader( "('! <Domain Name>, Total Number of Domain Cells, Total Number of Ground Surface Cells, Total Number of Insulation Cells')" );
		static gio::Fmt DomainCellsToEIO( "(A,',',I5',',I5',',I5)" );

		//int ZoneNum( 0 );
		int SurfCtr( 0 );

		// Read input if necessary
		if ( GetInputFlag ) {
			GetPipingSystemsAndGroundDomainsInput();
			GetInputFlag = false;
		}

		for ( int DomainNum = 1; DomainNum <= isize( PipingSystemDomains ); ++DomainNum ) {
			if ( PipingSystemDomains( DomainNum ).DomainNeedsToBeMeshed ) {
				PipingSystemDomains( DomainNum ).developMesh();
			}

			PipingSystemDomains( DomainNum ).DomainNeedsToBeMeshed = false;

			// The time init should be done here before we DoOneTimeInits because the DoOneTimeInits
			// includes a ground temperature initialization, which is based on the Cur%CurSimTimeSeconds variable
			// which would be carried over from the previous environment
			PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize = TimeStepZone * SecInHour;
			PipingSystemDomains( DomainNum ).Cur.CurSimTimeSeconds = ( ( DayOfSim - 1 ) * 24 + ( HourOfDay - 1 ) + ( TimeStep - 1 ) * TimeStepZone + SysTimeElapsed ) * SecInHour;

			// There are also some inits that are "close to one time" inits...( one-time in standalone, each envrn in E+ )
			if ( ( BeginSimFlag && PipingSystemDomains( DomainNum ).BeginSimInit ) || ( BeginEnvrnFlag && PipingSystemDomains( DomainNum ).BeginSimEnvrn ) ) {

				DoOneTimeInitializations( DomainNum, _ );

				if ( PipingSystemDomains( DomainNum ).HasZoneCoupledSlab ) {
					int Xmax = ubound( PipingSystemDomains( DomainNum ).Cells, 1 );
					//int Ymax = ubound( PipingSystemDomains( DomainNum ).Cells, 2 );
					int Zmax = ubound( PipingSystemDomains( DomainNum ).Cells, 3 );

					PipingSystemDomains( DomainNum ).WeightingFactor.allocate( { 0, Xmax }, { 0, Zmax } );
					PipingSystemDomains( DomainNum ).WeightedHeatFlux.allocate( { 0, Xmax }, { 0, Zmax } );
				}

				PipingSystemDomains( DomainNum ).BeginSimInit = false;
				PipingSystemDomains( DomainNum ).BeginSimEnvrn = false;
			}

			// Reset the heat fluxes if domain update has been completed
			if ( PipingSystemDomains( DomainNum ).ResetHeatFluxFlag ) {
				PipingSystemDomains( DomainNum ).AggregateHeatFlux = 0;
				PipingSystemDomains( DomainNum ).AggregateWallHeatFlux = 0;
				PipingSystemDomains( DomainNum ).AggregateFloorHeatFlux = 0;
				PipingSystemDomains( DomainNum ).NumHeatFlux = 0;
				PipingSystemDomains( DomainNum ).ResetHeatFluxFlag = false;
			}

			if ( !initOnly ) {
				// Aggregate the heat flux
				// Zone-coupled slab
				if ( PipingSystemDomains( DomainNum ).HasZoneCoupledSlab ) {
					PipingSystemDomains( DomainNum ).AggregateHeatFlux += PipingSystemDomains( DomainNum ).GetZoneInterfaceHeatFlux();
					PipingSystemDomains( DomainNum ).NumHeatFlux += 1;
					PipingSystemDomains( DomainNum ).HeatFlux = PipingSystemDomains( DomainNum ).AggregateHeatFlux / PipingSystemDomains( DomainNum ).NumHeatFlux;
				} else { // Coupled basement

					// basement walls
					PipingSystemDomains( DomainNum ).AggregateWallHeatFlux += PipingSystemDomains( DomainNum ).GetBasementWallHeatFlux();
					// basement floor
					PipingSystemDomains( DomainNum ).AggregateFloorHeatFlux += PipingSystemDomains( DomainNum ).GetBasementFloorHeatFlux();

					PipingSystemDomains( DomainNum ).NumHeatFlux += 1;
					PipingSystemDomains( DomainNum ).WallHeatFlux = PipingSystemDomains( DomainNum ).AggregateWallHeatFlux / PipingSystemDomains( DomainNum ).NumHeatFlux;
					PipingSystemDomains( DomainNum ).FloorHeatFlux = PipingSystemDomains( DomainNum ).AggregateFloorHeatFlux / PipingSystemDomains( DomainNum ).NumHeatFlux;
				}

				// Aggregate the heat flux
				// Zone-coupled slab
				if ( PipingSystemDomains( DomainNum ).HasZoneCoupledSlab ) {
					PipingSystemDomains( DomainNum ).AggregateHeatFlux += PipingSystemDomains( DomainNum ).GetZoneInterfaceHeatFlux();
					PipingSystemDomains( DomainNum ).NumHeatFlux += 1;
					PipingSystemDomains( DomainNum ).HeatFlux = PipingSystemDomains( DomainNum ).AggregateHeatFlux / PipingSystemDomains( DomainNum ).NumHeatFlux;
				} else if ( PipingSystemDomains( DomainNum ).HasZoneCoupledBasement ) { // Coupled basement
					// basement walls
					PipingSystemDomains( DomainNum ).AggregateWallHeatFlux += PipingSystemDomains( DomainNum ).GetBasementWallHeatFlux();
					// basement floor
					PipingSystemDomains( DomainNum ).AggregateFloorHeatFlux += PipingSystemDomains( DomainNum ).GetBasementFloorHeatFlux();

					PipingSystemDomains( DomainNum ).NumHeatFlux += 1;
					PipingSystemDomains( DomainNum ).WallHeatFlux = PipingSystemDomains( DomainNum ).AggregateWallHeatFlux / PipingSystemDomains( DomainNum ).NumHeatFlux;
					PipingSystemDomains( DomainNum ).FloorHeatFlux = PipingSystemDomains( DomainNum ).AggregateFloorHeatFlux / PipingSystemDomains( DomainNum ).NumHeatFlux;
				}

				// Zone-coupled slab
				if ( PipingSystemDomains( DomainNum ).HasZoneCoupledSlab ) {

					PipingSystemDomains( DomainNum ).HeatFlux = PipingSystemDomains( DomainNum ).AggregateHeatFlux / PipingSystemDomains( DomainNum ).NumHeatFlux;

					Real64 ZoneTemp = 0.0;

					//Set ZoneTemp equal to the average air temperature of the zones the coupled surfaces are part of.
					for ( SurfCtr = 1; SurfCtr <= isize( PipingSystemDomains( DomainNum ).ZoneCoupledSurfaces ); ++SurfCtr ) {
						int ZoneNum = PipingSystemDomains( DomainNum ).ZoneCoupledSurfaces( SurfCtr ).Zone;
						ZoneTemp += ZTAV( ZoneNum );
					}

					ZoneTemp = ZoneTemp / ( SurfCtr - 1 );
					Real64 AvgSlabTemp = PipingSystemDomains( DomainNum ).GetAverageTempByType( CellType::ZoneGroundInterface );

					int Ymax = ubound( PipingSystemDomains( DomainNum ).Cells, 2 );

					for ( int Z = lbound( PipingSystemDomains( DomainNum ).Cells, 3 ); Z <= ubound( PipingSystemDomains( DomainNum ).Cells, 3 ); ++Z ) {
						for ( int X = lbound( PipingSystemDomains( DomainNum ).Cells, 1 ); X <= ubound( PipingSystemDomains( DomainNum ).Cells, 1 ); ++X ) {
							// Zone interface cells
							if ( PipingSystemDomains( DomainNum ).Cells( X, Ymax, Z ).cellType == CellType::ZoneGroundInterface ){
								PipingSystemDomains( DomainNum ).WeightingFactor( X, Z ) = abs ( ( ZoneTemp - PipingSystemDomains( DomainNum ).Cells( X, Ymax, Z ).MyBase.Temperature_PrevTimeStep ) / ( ZoneTemp - AvgSlabTemp ) );
							}
						}
					}

					// Set initial weighted heat flux
					for ( int Z = lbound( PipingSystemDomains( DomainNum ).Cells, 3 ); Z <= ubound( PipingSystemDomains( DomainNum ).Cells, 3 ); ++Z ) {
						for ( int X = lbound( PipingSystemDomains( DomainNum ).Cells, 1 ); X <= ubound( PipingSystemDomains( DomainNum ).Cells, 1 ); ++X ) {
							// Zone interface cells
							if ( PipingSystemDomains( DomainNum ).Cells( X, Ymax, Z ).cellType == CellType::ZoneGroundInterface ){
								PipingSystemDomains( DomainNum ).WeightedHeatFlux( X, Z ) = PipingSystemDomains( DomainNum ).WeightingFactor( X, Z ) * PipingSystemDomains( DomainNum ).HeatFlux;
							}
						}
					}

					// Weighted heat flux and uniform heat flux balance energy may not balance exactly
					// Calculate difference and adjust
					PipingSystemDomains( DomainNum ).TotalEnergyUniformHeatFlux = PipingSystemDomains( DomainNum ).HeatFlux * PipingSystemDomains( DomainNum ).SlabArea * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
					PipingSystemDomains( DomainNum ).TotalEnergyWeightedHeatFlux = 0.0;

					for ( int Z = lbound( PipingSystemDomains( DomainNum ).Cells, 3 ); Z <= ubound( PipingSystemDomains( DomainNum ).Cells, 3 ); ++Z ) {
						for ( int X = lbound( PipingSystemDomains( DomainNum ).Cells, 1 ); X <= ubound( PipingSystemDomains( DomainNum ).Cells, 1 ); ++X ) {
							// Zone interface cells
							if ( PipingSystemDomains( DomainNum ).Cells( X, Ymax, Z ).cellType == CellType::ZoneGroundInterface ){
								auto & cell( PipingSystemDomains( DomainNum ).Cells( X, Ymax, Z ) );
								PipingSystemDomains( DomainNum ).TotalEnergyWeightedHeatFlux += PipingSystemDomains( DomainNum ).WeightedHeatFlux( X, Z ) * cell.width() * cell.depth() * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
							}
						}
					}

					PipingSystemDomains( DomainNum ).HeatFluxWeightingFactor = PipingSystemDomains( DomainNum ).TotalEnergyWeightedHeatFlux / PipingSystemDomains( DomainNum ).TotalEnergyUniformHeatFlux;
					PipingSystemDomains( DomainNum ).TotalEnergyWeightedHeatFlux = 0.0;

					// Finally, adjust the weighted heat flux so that energy balances
					for ( int Z = lbound( PipingSystemDomains( DomainNum ).Cells, 3 ); Z <= ubound( PipingSystemDomains( DomainNum ).Cells, 3 ); ++Z ) {
						for ( int X = lbound( PipingSystemDomains( DomainNum ).Cells, 1 ); X <= ubound( PipingSystemDomains( DomainNum ).Cells, 1 ); ++X ) {
							// Zone interface cells
							if ( PipingSystemDomains( DomainNum ).Cells( X, Ymax, Z ).cellType == CellType::ZoneGroundInterface ) {
								auto & cell( PipingSystemDomains( DomainNum ).Cells( X, Ymax, Z ) );
								PipingSystemDomains( DomainNum ).WeightedHeatFlux( X, Z ) = PipingSystemDomains( DomainNum ).WeightedHeatFlux( X, Z ) / PipingSystemDomains( DomainNum ).HeatFluxWeightingFactor;
								PipingSystemDomains( DomainNum ).TotalEnergyWeightedHeatFlux += PipingSystemDomains( DomainNum ).WeightedHeatFlux( X, Z ) * cell.width() * cell.depth() * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
							}
						}
					}

				} else { // Coupled basement
					PipingSystemDomains( DomainNum ).WallHeatFlux = PipingSystemDomains( DomainNum ).AggregateWallHeatFlux / PipingSystemDomains( DomainNum ).NumHeatFlux;
					PipingSystemDomains( DomainNum ).FloorHeatFlux = PipingSystemDomains( DomainNum ).AggregateFloorHeatFlux / PipingSystemDomains( DomainNum ).NumHeatFlux;
				}

				// Shift history arrays only if necessary
				if ( std::abs( PipingSystemDomains( DomainNum ).Cur.CurSimTimeSeconds - PipingSystemDomains( DomainNum ).Cur.PrevSimTimeSeconds ) > 1.0e-6 ) {
					PipingSystemDomains( DomainNum ).Cur.PrevSimTimeSeconds = PipingSystemDomains( DomainNum ).Cur.CurSimTimeSeconds;
					ShiftTemperaturesForNewTimeStep( DomainNum );
					PipingSystemDomains( DomainNum ).DomainNeedsSimulation = true;
				}
				PerformIterationLoop( DomainNum, _ );
			}
		}

		if ( WriteEIOFlag ) {
			// Write eio header
			gio::write( OutputFileInits, DomainCellsToEIOHeader );

			// Write eio data
			for ( int DomainNum = 1; DomainNum <= isize( PipingSystemDomains ); ++DomainNum ) {
				gio::write( OutputFileInits, DomainCellsToEIO ) << PipingSystemDomains( DomainNum ).Name << PipingSystemDomains( DomainNum ).NumDomainCells
					<< PipingSystemDomains( DomainNum ).NumGroundSurfCells << PipingSystemDomains( DomainNum ).NumInsulationCells;
			}
			WriteEIOFlag = false;
		}
	}

	void
	GetPipingSystemsAndGroundDomainsInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using General::TrimSigDigits;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetPipingSystemsAndGroundDomainsInput" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // Set to true if errors in input, fatal at end of routine
		int NumGeneralizedDomains;
		int NumZoneCoupledDomains;
		int NumBasements;
		int PipeCtr;
		int CircuitCtr;
		int CircuitIndex;
		int ThisSegmentIndex;
		int NumPipeCircuits;
		int NumPipeSegmentsInInput;
		int NumCircuitsInThisDomain;
		int NumHorizontalTrenches;
		int NumSegmentsInHorizontalTrenches;
		int DomainNum;
		int TotalNumDomains;
		int TotalNumCircuits;
		int TotalNumSegments;
		int ThisCircuitPipeSegmentCounter;
		std::string ThisSegmentName;

		// Read number of objects and allocate main data structures - first domains
		NumGeneralizedDomains = GetNumObjectsFound( ObjName_ug_GeneralDomain );
		NumHorizontalTrenches = GetNumObjectsFound( ObjName_HorizTrench );
		NumZoneCoupledDomains = GetNumObjectsFound( ObjName_ZoneCoupled_Slab );
		NumBasements = GetNumObjectsFound( ObjName_ZoneCoupled_Basement );
		TotalNumDomains = NumGeneralizedDomains + NumHorizontalTrenches + NumZoneCoupledDomains + NumBasements;
		PipingSystemDomains.allocate( TotalNumDomains );

		// then circuits
		NumPipeCircuits = GetNumObjectsFound( ObjName_Circuit );
		TotalNumCircuits = NumPipeCircuits + NumHorizontalTrenches;
		PipingSystemCircuits.allocate( TotalNumCircuits );

		// then segments
		NumPipeSegmentsInInput = GetNumObjectsFound( ObjName_Segment );
		NumSegmentsInHorizontalTrenches = GetNumSegmentsForHorizontalTrenches( NumHorizontalTrenches );
		TotalNumSegments = NumPipeSegmentsInInput + NumSegmentsInHorizontalTrenches;
		PipingSystemSegments.allocate( TotalNumSegments );

		// Read in raw inputs, don't try to interpret dependencies yet
		ReadGeneralDomainInputs( 1, NumGeneralizedDomains, ErrorsFound );
		ReadPipeCircuitInputs( NumPipeCircuits, ErrorsFound );
		ReadPipeSegmentInputs( NumPipeSegmentsInInput, ErrorsFound );
		ReadHorizontalTrenchInputs( NumGeneralizedDomains + 1, NumPipeCircuits + 1, NumPipeSegmentsInInput + 1, NumHorizontalTrenches, ErrorsFound );

		// This is heavily dependent on the order of the domains in the main array.
		ReadZoneCoupledDomainInputs( NumGeneralizedDomains + NumHorizontalTrenches + 1, NumZoneCoupledDomains, ErrorsFound );

		// This is heavily dependent on the order of the domains in the main array.
		ReadBasementInputs( NumGeneralizedDomains + NumHorizontalTrenches + NumZoneCoupledDomains + 1, NumBasements, ErrorsFound );

		// Check for BESTEST
		ReadBESTESTInputs( NumGeneralizedDomains + NumHorizontalTrenches + NumZoneCoupledDomains + NumBasements, ErrorsFound );

		// Report errors that are purely input problems
		if ( ErrorsFound ) ShowFatalError( RoutineName + ": Preceding input errors cause program termination." );

		// Setup output variables
		SetupPipingSystemOutputVariables( TotalNumSegments, TotalNumCircuits );

		// Validate CIRCUIT-SEGMENT cross references
		for ( CircuitCtr = PipingSystemCircuits.l1(); CircuitCtr <= PipingSystemCircuits.u1(); ++CircuitCtr ) {

			// validate circuit-segment name-to-index references
			for ( ThisCircuitPipeSegmentCounter = PipingSystemCircuits( CircuitCtr ).PipeSegmentNames.l1(); ThisCircuitPipeSegmentCounter <= PipingSystemCircuits( CircuitCtr ).PipeSegmentNames.u1(); ++ThisCircuitPipeSegmentCounter ) {

				ThisSegmentName = PipingSystemCircuits( CircuitCtr ).PipeSegmentNames( ThisCircuitPipeSegmentCounter );
				ThisSegmentIndex = FindItemInList( ThisSegmentName, PipingSystemSegments );
				if ( ThisSegmentIndex > 0 ) {
					PipingSystemCircuits( CircuitCtr ).PipeSegmentIndeces( ThisCircuitPipeSegmentCounter ) = ThisSegmentIndex;
					PipingSystemSegments( ThisSegmentIndex ).ParentCircuitIndex = CircuitCtr;
				} else {
					ShowSevereError( RoutineName + ": Could not match a pipe segment for: " + ObjName_Circuit + '=' + PipingSystemCircuits( CircuitCtr ).Name );
					ShowContinueError( RoutineName + ": Looking for: " + ObjName_Segment + '=' + ThisSegmentName );
					ErrorsFound = true;
				}

			} // Segment loop

		} // Circuit loop

		// Validate DOMAIN-CIRCUIT cross references
		for ( DomainNum = 1; DomainNum <= TotalNumDomains; ++DomainNum ) {

			// Convenience
			NumCircuitsInThisDomain = size( PipingSystemDomains( DomainNum ).CircuitNames );

			// validate pipe domain-circuit name-to-index references
			for ( CircuitCtr = 1; CircuitCtr <= NumCircuitsInThisDomain; ++CircuitCtr ) {
				CircuitIndex = FindItemInList( PipingSystemDomains( DomainNum ).CircuitNames( CircuitCtr ), PipingSystemCircuits );
				PipingSystemDomains( DomainNum ).CircuitIndeces( CircuitCtr ) = CircuitIndex;
				PipingSystemCircuits( CircuitIndex ).ParentDomainIndex = DomainNum;
			}

			// correct segment locations for: INTERNAL DATA STRUCTURE Y VALUE MEASURED FROM BOTTOM OF DOMAIN,
			//                                INPUT WAS MEASURED FROM GROUND SURFACE
			for ( CircuitCtr = 1; CircuitCtr <= NumCircuitsInThisDomain; ++CircuitCtr ) {
				CircuitIndex = PipingSystemDomains( DomainNum ).CircuitIndeces( CircuitCtr );
				for ( PipeCtr = PipingSystemCircuits( CircuitIndex ).PipeSegmentIndeces.l1(); PipeCtr <= PipingSystemCircuits( CircuitIndex ).PipeSegmentIndeces.u1(); ++PipeCtr ) {
					ThisSegmentIndex = PipingSystemCircuits( CircuitCtr ).PipeSegmentIndeces( PipeCtr );
					PipingSystemSegments( ThisSegmentIndex ).PipeLocation.Y = PipingSystemDomains( DomainNum ).Extents.Ymax - PipingSystemSegments( ThisSegmentIndex ).PipeLocation.Y;
				} // segment loop
			} // circuit loop

			// correct segment locations for: BASEMENT X SHIFT
			if ( PipingSystemDomains( DomainNum ).HasBasement && PipingSystemDomains( DomainNum ).BasementZone.ShiftPipesByWidth ) {
				for ( CircuitCtr = 1; CircuitCtr <= NumCircuitsInThisDomain; ++CircuitCtr ) {
					CircuitIndex = PipingSystemDomains( DomainNum ).CircuitIndeces( CircuitCtr );
					for ( PipeCtr = PipingSystemCircuits( CircuitIndex ).PipeSegmentIndeces.l1(); PipeCtr <= PipingSystemCircuits( CircuitIndex ).PipeSegmentIndeces.u1(); ++PipeCtr ) {
						ThisSegmentIndex = PipingSystemCircuits( CircuitCtr ).PipeSegmentIndeces( PipeCtr );
						PipingSystemSegments( ThisSegmentIndex ).PipeLocation.X += PipingSystemDomains( DomainNum ).BasementZone.Width;
					} // segment loop
				} // circuit loop
			}

			// now we will have good values of pipe segment locations, we can validate them
			for ( CircuitCtr = 1; CircuitCtr <= NumCircuitsInThisDomain; ++CircuitCtr ) {

				// retrieve the index
				CircuitIndex = PipingSystemDomains( DomainNum ).CircuitIndeces( CircuitCtr );

				// check to make sure it isn't outside the domain
				for ( PipeCtr = PipingSystemCircuits( CircuitIndex ).PipeSegmentIndeces.l1(); PipeCtr <= PipingSystemCircuits( CircuitIndex ).PipeSegmentIndeces.u1(); ++PipeCtr ) {
					ThisSegmentIndex = PipingSystemCircuits( CircuitCtr ).PipeSegmentIndeces( PipeCtr );
					if ( ( PipingSystemSegments( ThisSegmentIndex ).PipeLocation.X > PipingSystemDomains( DomainNum ).Extents.Xmax ) || ( PipingSystemSegments( ThisSegmentIndex ).PipeLocation.X < 0.0 ) || ( PipingSystemSegments( ThisSegmentIndex ).PipeLocation.Y > PipingSystemDomains( DomainNum ).Extents.Ymax ) || ( PipingSystemSegments( ThisSegmentIndex ).PipeLocation.Y < 0.0 ) ) {
						ShowSevereError( "PipingSystems::" + RoutineName + ":A pipe was found to be outside of the domain extents after performing any corrections for basement or burial depth." );
						ShowContinueError( "Pipe segment name:" + PipingSystemSegments( ThisSegmentIndex ).Name );
						ShowContinueError( "Corrected pipe location: ( x,y )=( " + TrimSigDigits( PipingSystemSegments( ThisSegmentIndex ).PipeLocation.X, 2 ) + ',' + TrimSigDigits( PipingSystemSegments( ThisSegmentIndex ).PipeLocation.Y, 2 ) + " )" );
					}
				} // segment loop

			} // circuit loop

		} // domain loop

		// If we encountered any other errors that we couldn't handle separately than stop now
		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + ':' + ObjName_ug_GeneralDomain + ": Errors found in input." );
		}

	}

	int
	GetNumSegmentsForHorizontalTrenches( int const NumHorizontalTrenches )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   September 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Using/Aliasing
		using InputProcessor::GetObjectItem;
		using namespace DataIPShortCuts;

		// Return value
		int Total;

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int HorizontalCtr;
		int NumPipesInThisHorizontal;
		int NumAlphas;
		int NumNumbers;
		int IOStatus;

		Total = 0;

		for ( HorizontalCtr = 1; HorizontalCtr <= NumHorizontalTrenches; ++HorizontalCtr ) {

			// Set up all the inputs for this domain object
			GetObjectItem( ObjName_HorizTrench, HorizontalCtr, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			NumPipesInThisHorizontal = rNumericArgs( 3 );

			Total += NumPipesInThisHorizontal;

		}

		return Total;

	}

	void
	ReadGeneralDomainInputs(
		int const IndexStart,
		int const NumGeneralizedDomains,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Using/Aliasing
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts;
		using DataSurfaces::OSCM;
		using General::TrimSigDigits;
		using namespace GroundTemperatureManager;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ReadGeneralDomainInputs" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int DomainNum; // Item to be "gotten"
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		int NumCircuitsInThisDomain;
		int CircuitCtr;
		int NumSurfacesWithThisOSCM;
		int NumAlphasBeforePipeCircOne;
		int CurIndex;
		bool IsBlank;
		bool IsNotOK;

		for ( DomainNum = IndexStart; DomainNum <= NumGeneralizedDomains; ++DomainNum ) {

			// Set up all the inputs for this domain object
			GetObjectItem( ObjName_ug_GeneralDomain, DomainNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			// Get the name, validate
			PipingSystemDomains( DomainNum ).Name = cAlphaArgs( 1 );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PipingSystemDomains, DomainNum - 1, IsNotOK, IsBlank, ObjName_ug_GeneralDomain + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				cAlphaArgs( 1 ) = "Duplicate name encountered";
			} else if ( IsBlank ) {
				ErrorsFound = true;
				cAlphaArgs( 1 ) = "Blank name encountered";
			}

			// Mesh extents, validated by IP
			PipingSystemDomains( DomainNum ).Extents.Xmax = rNumericArgs( 1 );
			PipingSystemDomains( DomainNum ).Extents.Ymax = rNumericArgs( 2 );
			PipingSystemDomains( DomainNum ).Extents.Zmax = rNumericArgs( 3 );

			// X direction mesh inputs, validated by IP
			PipingSystemDomains( DomainNum ).Mesh.X.RegionMeshCount = rNumericArgs( 4 );
			{ auto const meshDistribution( uppercased( cAlphaArgs( 2 ) ) );
			if ( meshDistribution == "UNIFORM" ) {
				PipingSystemDomains( DomainNum ).Mesh.X.thisMeshDistribution = MeshDistribution::Uniform;
			} else if ( meshDistribution == "SYMMETRICGEOMETRIC" ) {
				PipingSystemDomains( DomainNum ).Mesh.X.thisMeshDistribution = MeshDistribution::SymmetricGeometric;
				if ( mod( PipingSystemDomains( DomainNum ).Mesh.X.RegionMeshCount, 2 ) != 0 ) {
					ShowWarningError( "PipingSystems:" + RoutineName + ": Invalid mesh type-count combination." );
					ShowContinueError( "Instance:" + ObjName_ug_GeneralDomain + '=' + PipingSystemDomains( DomainNum ).Name );
					ShowContinueError( "An ODD-valued X mesh count was found in the input for symmetric geometric configuration." );
					ShowContinueError( "This is invalid, mesh count incremented UP by one to next EVEN value." );
					++PipingSystemDomains( DomainNum ).Mesh.X.RegionMeshCount;
					PipingSystemDomains( DomainNum ).Mesh.X.GeometricSeriesCoefficient = rNumericArgs( 5 );
				} else {
					PipingSystemDomains( DomainNum ).Mesh.X.GeometricSeriesCoefficient = 1.0;
				}
			} else {
				IssueSevereInputFieldError( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( 2 ), cAlphaArgs( 2 ), "Use a choice from the available mesh type keys.", ErrorsFound );
			}}

			// Y direction mesh inputs, validated by IP
			PipingSystemDomains( DomainNum ).Mesh.Y.RegionMeshCount = rNumericArgs( 6 );
			{ auto const meshDistribution( stripped( cAlphaArgs( 3 ) ) );
			if ( meshDistribution == "UNIFORM" ) {
				PipingSystemDomains( DomainNum ).Mesh.Y.thisMeshDistribution = MeshDistribution::Uniform;
			} else if ( meshDistribution == "SYMMETRICGEOMETRIC" ) {
				PipingSystemDomains( DomainNum ).Mesh.Y.thisMeshDistribution = MeshDistribution::SymmetricGeometric;
				if ( mod( PipingSystemDomains( DomainNum ).Mesh.Y.RegionMeshCount, 2 ) != 0 ) {
					ShowWarningError( "PipingSystems:" + RoutineName + ": Invalid mesh type-count combination." );
					ShowContinueError( "Instance:" + ObjName_ug_GeneralDomain + '=' + PipingSystemDomains( DomainNum ).Name );
					ShowContinueError( "An ODD-valued Y mesh count was found in the input for symmetric geometric configuration." );
					ShowContinueError( "This is invalid, mesh count incremented UP by one to next EVEN value." );
					++PipingSystemDomains( DomainNum ).Mesh.Y.RegionMeshCount;
					PipingSystemDomains( DomainNum ).Mesh.Y.GeometricSeriesCoefficient = rNumericArgs( 7 );
				} else {
					PipingSystemDomains( DomainNum ).Mesh.Y.GeometricSeriesCoefficient = 1.0;
				}
			} else {
				IssueSevereInputFieldError( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( 3 ), cAlphaArgs( 3 ), "Use a choice from the available mesh type keys.", ErrorsFound );
			}}

			// Z direction mesh inputs, validated by IP
			PipingSystemDomains( DomainNum ).Mesh.Z.RegionMeshCount = rNumericArgs( 8 );
			{ auto const meshDistribution( stripped( cAlphaArgs( 4 ) ) );
			if ( meshDistribution == "UNIFORM" ) {
				PipingSystemDomains( DomainNum ).Mesh.Z.thisMeshDistribution = MeshDistribution::Uniform;
			} else if ( meshDistribution == "SYMMETRICGEOMETRIC" ) {
				PipingSystemDomains( DomainNum ).Mesh.Z.thisMeshDistribution = MeshDistribution::SymmetricGeometric;
				if ( mod( PipingSystemDomains( DomainNum ).Mesh.Z.RegionMeshCount, 2 ) != 0 ) {
					ShowWarningError( "PipingSystems:" + RoutineName + ": Invalid mesh type-count combination." );
					ShowContinueError( "Instance:" + ObjName_ug_GeneralDomain + '=' + PipingSystemDomains( DomainNum ).Name );
					ShowContinueError( "An ODD-valued Z mesh count was found in the input for symmetric geometric configuration." );
					ShowContinueError( "This is invalid, mesh count incremented UP by one to next EVEN value." );
					++PipingSystemDomains( DomainNum ).Mesh.Z.RegionMeshCount;
					PipingSystemDomains( DomainNum ).Mesh.Z.GeometricSeriesCoefficient = rNumericArgs( 9 );
				} else {
					PipingSystemDomains( DomainNum ).Mesh.Z.GeometricSeriesCoefficient = 1.0;
				}
			} else {
				IssueSevereInputFieldError( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( 4 ), cAlphaArgs( 4 ), "Use a choice from the available mesh type keys.", ErrorsFound );
			}}

			// Soil properties, validated min/max by IP
			PipingSystemDomains( DomainNum ).GroundProperties.Conductivity = rNumericArgs( 10 );
			PipingSystemDomains( DomainNum ).GroundProperties.Density = rNumericArgs( 11 );
			PipingSystemDomains( DomainNum ).GroundProperties.SpecificHeat = rNumericArgs( 12 );

			// Moisture properties, validated min/max by IP, and converted to a fraction for computation here
			PipingSystemDomains( DomainNum ).Moisture.Theta_liq = rNumericArgs( 13 ) / 100.0;
			PipingSystemDomains( DomainNum ).Moisture.Theta_sat = rNumericArgs( 14 ) / 100.0;

			// check if there is a basement
			if ( SameString( cAlphaArgs( 7 ), "YES" ) ) {
				PipingSystemDomains( DomainNum ).HasBasement = true;
			} else if ( SameString( cAlphaArgs( 7 ), "NO" ) ) {
				PipingSystemDomains( DomainNum ).HasBasement = false;
			} else {
				IssueSevereInputFieldError( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( 7 ), cAlphaArgs( 7 ), "Must enter either yes or no.", ErrorsFound );
			}

			// more work to do if there is a basement
			if ( PipingSystemDomains( DomainNum ).HasBasement ) {

				// check if there are blank inputs related to the basement,
				// IP can't catch this because they are inherently optional if there ISN'T a basement
				if ( lNumericFieldBlanks( 15 ) || lNumericFieldBlanks( 16 ) || lAlphaFieldBlanks( 8 ) || lAlphaFieldBlanks( 9 ) || lAlphaFieldBlanks( 10 ) ) {
					ShowSevereError( "Erroneous basement inputs for " + ObjName_ug_GeneralDomain + '=' + cAlphaArgs( 1 ) );
					ShowContinueError( "Object specified to have a basement, while at least one basement input was left blank." );
					ErrorsFound = true;
				}

				// get dimensions for meshing
				CurIndex = 15;
				PipingSystemDomains( DomainNum ).BasementZone.Width = rNumericArgs( CurIndex );
				if ( PipingSystemDomains( DomainNum ).BasementZone.Width <= 0.0 ) {
					IssueSevereInputFieldError( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cNumericFieldNames( CurIndex ), rNumericArgs( CurIndex ), "Basement width must be a positive nonzero value.", ErrorsFound );
				}

				CurIndex = 16;
				PipingSystemDomains( DomainNum ).BasementZone.Depth = rNumericArgs( CurIndex );
				if ( PipingSystemDomains( DomainNum ).BasementZone.Depth <= 0.0 ) {
					IssueSevereInputFieldError( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cNumericFieldNames( CurIndex ), rNumericArgs( CurIndex ), "Basement depth must be a positive nonzero value.", ErrorsFound );
				}

				// check for dimension shift
				CurIndex = 8;
				if ( SameString( cAlphaArgs( CurIndex ), "YES" ) ) {
					PipingSystemDomains( DomainNum ).BasementZone.ShiftPipesByWidth = true;
				} else if ( SameString( cAlphaArgs( CurIndex ), "NO" ) ) {
					PipingSystemDomains( DomainNum ).BasementZone.ShiftPipesByWidth = false;
				} else {
					IssueSevereInputFieldError( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Must enter either yes or no.", ErrorsFound );
				}

				// get boundary condition model names and indices --error check
				CurIndex = 9;
				PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMName = cAlphaArgs( CurIndex );
				PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex = FindItemInList( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMName, OSCM );
				if ( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex <= 0 ) {
					IssueSevereInputFieldError( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Could not match with an Other Side Conditions Model input object.", ErrorsFound );
				} else {
					NumSurfacesWithThisOSCM = GetSurfaceCountForOSCM( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex );
					if ( NumSurfacesWithThisOSCM <= 0 ) {
						IssueSevereInputFieldError( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Entry matched an Other Side Conditions Model, but no surfaces were found to be using this Other Side Conditions Model.", ErrorsFound );
					} else {
						PipingSystemDomains( DomainNum ).BasementZone.WallSurfacePointers.allocate( NumSurfacesWithThisOSCM );
						PipingSystemDomains( DomainNum ).BasementZone.WallSurfacePointers = GetSurfaceIndecesForOSCM( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex, NumSurfacesWithThisOSCM );
					}
				}

				CurIndex = 10;
				PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMName = cAlphaArgs( CurIndex );
				PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex = FindItemInList( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMName, OSCM );
				if ( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex <= 0 ) {
					IssueSevereInputFieldError( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Could not match with an Other Side Conditions Model input object.", ErrorsFound );
				} else {
					NumSurfacesWithThisOSCM = GetSurfaceCountForOSCM( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex );
					if ( NumSurfacesWithThisOSCM <= 0 ) {
						IssueSevereInputFieldError( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Entry matched an Other Side Conditions Model, but no surfaces were found to be using this Other Side Conditions Model.", ErrorsFound );
					} else {
						PipingSystemDomains( DomainNum ).BasementZone.FloorSurfacePointers.allocate( NumSurfacesWithThisOSCM );
						PipingSystemDomains( DomainNum ).BasementZone.FloorSurfacePointers = GetSurfaceIndecesForOSCM( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex, NumSurfacesWithThisOSCM );
					}
				}

			}

			// get some convergence tolerances, minimum/maximum are enforced by the IP, along with default values if user left them blank
			PipingSystemDomains( DomainNum ).SimControls.Convergence_CurrentToPrevIteration = rNumericArgs( 17 );
			PipingSystemDomains( DomainNum ).SimControls.MaxIterationsPerTS = rNumericArgs( 18 );

			// additional evapotranspiration parameter, min/max validated by IP
			PipingSystemDomains( DomainNum ).Moisture.GroundCoverCoefficient = rNumericArgs( 19 );

			// Allocate the circuit placeholder arrays
			NumCircuitsInThisDomain = int( rNumericArgs( 20 ) );
			PipingSystemDomains( DomainNum ).CircuitNames.allocate( NumCircuitsInThisDomain );
			PipingSystemDomains( DomainNum ).CircuitIndeces.allocate( NumCircuitsInThisDomain );

			// Check for blank or missing or mismatched number...
			NumAlphasBeforePipeCircOne = 10;
			for ( CircuitCtr = 1; CircuitCtr <= NumCircuitsInThisDomain; ++CircuitCtr ) {
				PipingSystemDomains( DomainNum ).CircuitNames( CircuitCtr ) = cAlphaArgs( CircuitCtr + NumAlphasBeforePipeCircOne );
			}

			// Initialize ground temperature model and get pointer reference
			PipingSystemDomains( DomainNum ).Farfield.groundTempModel = GetGroundTempModelAndInit( cAlphaArgs( 5 ), cAlphaArgs( 6 ) );

		}

	}

	void
	ReadZoneCoupledDomainInputs(
		int const StartingDomainNumForZone,
		int const NumZoneCoupledDomains,
		bool & ErrorsFound
	)
	{

			// SUBROUTINE INFORMATION:
			//       AUTHOR         Edwin Lee
			//       DATE WRITTEN   Summer 2011
			//       MODIFIED       Spring 2014 by Matt Mitchell and Sushobhit Acharya to accommodate ground coupled calculations
			//       RE-ENGINEERED  na

			// Using/Aliasing
			using InputProcessor::GetObjectItem;
			using InputProcessor::FindItemInList;
			using InputProcessor::SameString;
			using InputProcessor::VerifyName;
			using namespace DataIPShortCuts;
			using DataSurfaces::OSCM;
			using General::TrimSigDigits;
			using DataHeatBalance::Material;
			using DataHeatBalance::TotMaterials;
			using namespace GroundTemperatureManager;

			// SUBROUTINE PARAMETER DEFINITIONS:
			static std::string const RoutineName( "ReadZoneCoupledDomainInputs" );

			// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
			int ZoneCoupledDomainCtr;
			int DomainCtr;
			int NumAlphas; // Number of Alphas for each GetObjectItem call
			int NumNumbers; // Number of Numbers for each GetObjectItem call
			int IOStatus; // Used in GetObjectItem
			int NumSurfacesWithThisOSCM;
			int SurfCtr;
			bool IsBlank;
			bool IsNotOK;
			Real64 ThisArea;

			struct GroundDomainData
			{
				// Members
				std::string ObjName;
				Real64 Depth;
				Real64 AspectRatio;
				Real64 PerimeterOffset;
				Real64 SoilConductivity;
				Real64 SoilDensity;
				Real64 SoilSpecificHeat;
				Real64 MoistureContent;
				Real64 SaturationMoistureContent;
				Real64 EvapotranspirationCoeff;
				Real64 MinSurfTemp;
				int MonthOfMinSurfTemp;
				Real64 HorizInsWidth;
				Real64 VertInsDepth;
				int OSCMIndex;
				std::string OSCMName;
				std::string SlabMaterial;
				std::string HorizInsMaterial;
				std::string VertInsMaterial;

				// Default Constructor
				GroundDomainData() :
					Depth( 0.0 ),
					AspectRatio( 0.0 ),
					PerimeterOffset( 0.0 ),
					SoilConductivity( 0.0 ),
					SoilDensity( 0.0 ),
					SoilSpecificHeat( 0.0 ),
					MoistureContent( 0.0 ),
					SaturationMoistureContent( 0.0 ),
					EvapotranspirationCoeff( 0.0 ),
					MinSurfTemp( 0.0 ),
					MonthOfMinSurfTemp( 0 ),
					HorizInsWidth( 0.0 ),
					VertInsDepth( 0.0 ),
					OSCMIndex( 0 )
				{}
			};

			// Object Data
			Array1D< GroundDomainData > Domain( NumZoneCoupledDomains );

			// initialize these counters properly so they can be incremented within the DO loop
			DomainCtr = StartingDomainNumForZone - 1;

			// For each domain, we need to process the inputs into a local array of derived type, then resolve each one, creating definitions for a zone coupled domain.
			// This way, the outer get input routines can handle it as though they were generalized routines

			for ( ZoneCoupledDomainCtr = 1; ZoneCoupledDomainCtr <= NumZoneCoupledDomains; ++ZoneCoupledDomainCtr ) {

				// Increment the domain counters here
				++DomainCtr;

				// Read all the inputs for this domain object
				GetObjectItem( ObjName_ZoneCoupled_Slab, ZoneCoupledDomainCtr, cAlphaArgs,
					NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				// Get the name, validate
				Domain( ZoneCoupledDomainCtr ).ObjName = cAlphaArgs( 1 );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), Domain, &GroundDomainData::ObjName, ZoneCoupledDomainCtr - 1, IsNotOK, IsBlank, ObjName_ZoneCoupled_Slab + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					cAlphaArgs( 1 ) = "Duplicate name encountered";
				} else if ( IsBlank ) {
					ErrorsFound = true;
					cAlphaArgs( 1 ) = "Blank name encountered";
				}

				// Read in the rest of the inputs into the local type for clarity during transition
				Domain( ZoneCoupledDomainCtr ).OSCMName = cAlphaArgs( 4 );
				Domain( ZoneCoupledDomainCtr ).Depth = rNumericArgs( 1 );
				Domain( ZoneCoupledDomainCtr ).AspectRatio = rNumericArgs( 2 );
				Domain( ZoneCoupledDomainCtr ).PerimeterOffset = rNumericArgs( 3 );
				Domain( ZoneCoupledDomainCtr ).SoilConductivity = rNumericArgs( 4 );
				Domain( ZoneCoupledDomainCtr ).SoilDensity = rNumericArgs( 5 );
				Domain( ZoneCoupledDomainCtr ).SoilSpecificHeat = rNumericArgs( 6 );
				Domain( ZoneCoupledDomainCtr ).MoistureContent = rNumericArgs( 7 );
				Domain( ZoneCoupledDomainCtr ).SaturationMoistureContent = rNumericArgs( 8 );
				Domain( ZoneCoupledDomainCtr ).EvapotranspirationCoeff = rNumericArgs( 9 );
				Domain( ZoneCoupledDomainCtr ).HorizInsWidth = rNumericArgs( 10 );
				Domain( ZoneCoupledDomainCtr ).VertInsDepth = rNumericArgs( 11 );

				// Set flag for slab in-grade or slab on-grade
				if ( SameString( cAlphaArgs( 5 ), "INGRADE" ) ) {
					PipingSystemDomains( DomainCtr ).SlabInGradeFlag = true;
				} else if ( SameString( cAlphaArgs( 5 ), "ONGRADE" ) ) {
					PipingSystemDomains( DomainCtr ).SlabInGradeFlag = false;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + "=" + cAlphaArgs( 5 ) );
					ShowContinueError( "Found in: " + Domain( ZoneCoupledDomainCtr ).ObjName );
					ErrorsFound = true;
				}

				// Get slab material properties
				if ( PipingSystemDomains( DomainCtr ).SlabInGradeFlag ) {
					Domain( ZoneCoupledDomainCtr ).SlabMaterial = cAlphaArgs( 6 );
					PipingSystemDomains( DomainCtr ).SlabMaterialNum = FindItemInList( cAlphaArgs( 6 ), Material, TotMaterials );
					if ( PipingSystemDomains( DomainCtr ).SlabMaterialNum == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + "=" + cAlphaArgs( 6 ) );
						ShowContinueError( "Found in: " + Domain( ZoneCoupledDomainCtr ).ObjName );
						ErrorsFound = true;
					} else {
						PipingSystemDomains( DomainCtr ).SlabThickness = Material( PipingSystemDomains( DomainCtr ).SlabMaterialNum ).Thickness;
						PipingSystemDomains( DomainCtr ).SlabProperties.Density = Material( PipingSystemDomains( DomainCtr ).SlabMaterialNum ).Density;
						PipingSystemDomains( DomainCtr ).SlabProperties.SpecificHeat = Material( PipingSystemDomains( DomainCtr ).SlabMaterialNum ).SpecHeat;
						PipingSystemDomains( DomainCtr ).SlabProperties.Conductivity = Material( PipingSystemDomains( DomainCtr ).SlabMaterialNum ).Conductivity;
					}
				}

				// set flag for horizontal insulation
				if ( PipingSystemDomains( DomainCtr ).SlabInGradeFlag ) {
					if ( SameString( cAlphaArgs( 7 ), "NO" ) ) {
						PipingSystemDomains( DomainCtr ).HorizInsPresentFlag = false;
					} else if ( SameString( cAlphaArgs( 7 ), "YES" ) ) {
						PipingSystemDomains( DomainCtr ).HorizInsPresentFlag = true;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + "=" + cAlphaArgs( 7 ) );
						ShowContinueError( "Found in: " + Domain( ZoneCoupledDomainCtr ).ObjName );
						ErrorsFound = true;
					}
				}

				// Get horizontal insulation material properties
				if ( PipingSystemDomains( DomainCtr ).HorizInsPresentFlag ) {
					Domain( ZoneCoupledDomainCtr ).HorizInsMaterial = cAlphaArgs( 8 );
					PipingSystemDomains( DomainCtr ).HorizInsMaterialNum = FindItemInList( cAlphaArgs( 8 ), Material, TotMaterials );
					if ( PipingSystemDomains( DomainCtr ).HorizInsMaterialNum == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 8 ) + "=" + cAlphaArgs( 8 ) );
						ShowContinueError( "Found in: " + Domain( ZoneCoupledDomainCtr ).ObjName );
						ErrorsFound = true;
					} else {
						PipingSystemDomains( DomainCtr ).HorizInsThickness = Material( PipingSystemDomains( DomainCtr ).HorizInsMaterialNum ).Thickness;
						PipingSystemDomains( DomainCtr ).HorizInsProperties.Density = Material( PipingSystemDomains( DomainCtr ).HorizInsMaterialNum ).Density;
						PipingSystemDomains( DomainCtr ).HorizInsProperties.SpecificHeat = Material( PipingSystemDomains( DomainCtr ).HorizInsMaterialNum ).SpecHeat;
						PipingSystemDomains( DomainCtr ).HorizInsProperties.Conductivity = Material( PipingSystemDomains( DomainCtr ).HorizInsMaterialNum ).Conductivity;
					}

					// Set flag for horizontal insulation extents
					if ( SameString( cAlphaArgs( 9 ), "PERIMETER" ) ) {
						PipingSystemDomains( DomainCtr ).FullHorizInsPresent = false;
						// Horizontal insulation perimeter width
						if ( Domain( ZoneCoupledDomainCtr ).HorizInsWidth > 0.0 ) {
							PipingSystemDomains( DomainCtr ).HorizInsWidth = Domain( ZoneCoupledDomainCtr ).HorizInsWidth;
						} else {
							ShowSevereError( "Invalid " + cNumericFieldNames( 10 ) );
							ShowContinueError( "Found in: " + Domain( ZoneCoupledDomainCtr ).ObjName );
							ErrorsFound = true;
						}
					} else if ( SameString( cAlphaArgs( 9 ), "FULL" ) ) {
						PipingSystemDomains( DomainCtr ).FullHorizInsPresent = true;
					} else {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 9 ) + "=" + cAlphaArgs( 9 ) );
						ShowContinueError( "Found in: " + Domain( ZoneCoupledDomainCtr ).ObjName );
						ErrorsFound = true;
					}

					// Horizontal insulation perimeter width
					PipingSystemDomains( DomainCtr ).HorizInsWidth = Domain( ZoneCoupledDomainCtr ).HorizInsWidth;

				}

				// set flag for vertical insulation
				if ( SameString( cAlphaArgs( 10 ), "NO" ) ) {
					PipingSystemDomains( DomainCtr ).VertInsPresentFlag = false;
				} else if ( SameString( cAlphaArgs( 10 ), "YES" ) ) {
					PipingSystemDomains( DomainCtr ).VertInsPresentFlag = true;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 10 ) + "=" + cAlphaArgs( 10 ) );
					ShowContinueError( "Found in: " + Domain( ZoneCoupledDomainCtr ).ObjName );
					ErrorsFound = true;
				}

				// Get vertical insulation material properties
				if ( PipingSystemDomains( DomainCtr ).VertInsPresentFlag ) {
					Domain( ZoneCoupledDomainCtr ).VertInsMaterial = cAlphaArgs( 11 );
					PipingSystemDomains( DomainCtr ).VertInsMaterialNum = FindItemInList( cAlphaArgs( 11 ), Material, TotMaterials );
					if ( PipingSystemDomains( DomainCtr ).VertInsMaterialNum == 0 ) {
						ShowSevereError( "Invalid " + cAlphaFieldNames( 11 ) + "=" + cAlphaArgs( 11 ) );
						ShowContinueError( "Found in: " + Domain( ZoneCoupledDomainCtr ).ObjName );
						ErrorsFound = true;
					} else {
						PipingSystemDomains( DomainCtr ).VertInsThickness = Material( PipingSystemDomains( DomainCtr ).VertInsMaterialNum ).Thickness;
						PipingSystemDomains( DomainCtr ).VertInsProperties.Density = Material( PipingSystemDomains( DomainCtr ).VertInsMaterialNum ).Density;
						PipingSystemDomains( DomainCtr ).VertInsProperties.SpecificHeat = Material( PipingSystemDomains( DomainCtr ).VertInsMaterialNum ).SpecHeat;
						PipingSystemDomains( DomainCtr ).VertInsProperties.Conductivity = Material( PipingSystemDomains( DomainCtr ).VertInsMaterialNum ).Conductivity;
					}

					// vertical insulation depth
					if ( Domain( ZoneCoupledDomainCtr ).VertInsDepth > Domain( ZoneCoupledDomainCtr ).Depth || Domain( ZoneCoupledDomainCtr ).VertInsDepth <= 0.0 ) {
						ShowSevereError( "Invalid " + cNumericFieldNames( 11 ) );
						ShowContinueError( "Found in: " + Domain( ZoneCoupledDomainCtr ).ObjName );
						ErrorsFound = true;
					} else {
						PipingSystemDomains( DomainCtr ).VertInsDepth = Domain( ZoneCoupledDomainCtr ).VertInsDepth;
					}
				}

				// Domain perimeter offset
				PipingSystemDomains( DomainCtr ).PerimeterOffset = Domain( ZoneCoupledDomainCtr ).PerimeterOffset;

				// Set simulation interval flag
				if ( SameString( cAlphaArgs( 12 ), "TIMESTEP" ) ) {
					PipingSystemDomains( DomainCtr ).SimTimestepFlag = true;
				} else if ( SameString( cAlphaArgs( 12 ), "HOURLY" ) ) {
					PipingSystemDomains( DomainCtr ).SimHourlyFlag = true;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 12 ) + "=" + cAlphaArgs( 12 ) );
					ShowContinueError( "Found in: " + Domain( ZoneCoupledDomainCtr ).ObjName );
					ErrorsFound = true;
				}

				//******* We'll first set up the domain ********
				PipingSystemDomains( DomainCtr ).IsActuallyPartOfAHorizontalTrench = false;
				PipingSystemDomains( DomainCtr ).HasAPipeCircuit = false;
				PipingSystemDomains( DomainCtr ).HasZoneCoupledSlab = true;

				// Domain name
				PipingSystemDomains( DomainCtr ).Name = Domain( ZoneCoupledDomainCtr ).ObjName;

				// get boundary condition model names and indices -- error check
				PipingSystemDomains( DomainCtr ).ZoneCoupledOSCMIndex = FindItemInList( Domain( ZoneCoupledDomainCtr ).OSCMName, OSCM );
				if ( PipingSystemDomains( DomainCtr ).ZoneCoupledOSCMIndex <= 0 ) {
					IssueSevereInputFieldError( RoutineName, ObjName_ZoneCoupled_Slab, cAlphaArgs( 1 ), cAlphaFieldNames( 4 ), cAlphaArgs( 4 ), "Could not match with an Other Side Conditions Model input object.", ErrorsFound );
					ErrorsFound = true;
				} else {
					NumSurfacesWithThisOSCM = GetSurfaceCountForOSCM( PipingSystemDomains( DomainCtr ).ZoneCoupledOSCMIndex );
					if ( NumSurfacesWithThisOSCM <= 0 ) {
						IssueSevereInputFieldError( RoutineName, ObjName_ZoneCoupled_Slab, cAlphaArgs( 1 ), cAlphaFieldNames( 4 ), cAlphaArgs( 4 ), "Entry matched an Other Side Conditions Model, but no surfaces were found to be using this Other Side Conditions Model.", ErrorsFound );
						ErrorsFound = true;
					} else {
						PipingSystemDomains( DomainCtr ).ZoneCoupledSurfaces.allocate( NumSurfacesWithThisOSCM );
						// Create GetSurfaceDataForOSCM function
						PipingSystemDomains( DomainCtr ).ZoneCoupledSurfaces = GetSurfaceDataForOSCM( PipingSystemDomains( DomainCtr ).ZoneCoupledOSCMIndex, NumSurfacesWithThisOSCM );
					}
				}

				// Total surface area
				ThisArea = 0.0;

				for ( SurfCtr = 1; SurfCtr <= isize( PipingSystemDomains( DomainCtr ).ZoneCoupledSurfaces ); ++SurfCtr ) {
					ThisArea += PipingSystemDomains( DomainCtr ).ZoneCoupledSurfaces( SurfCtr ).SurfaceArea;
				}

				PipingSystemDomains( DomainCtr ).SlabArea = ThisArea / 4; // We are only interested in 1/4 of total area due to symmetry

				// Surface dimensions
				PipingSystemDomains( DomainCtr ).SlabWidth = std::sqrt( ThisArea / Domain( ZoneCoupledDomainCtr ).AspectRatio );
				PipingSystemDomains( DomainCtr ).SlabLength = PipingSystemDomains( DomainCtr ).SlabWidth * Domain( ZoneCoupledDomainCtr ).AspectRatio;

				// Check horizontal insulation width so as to prevent overlapping insulation. VertInsThickness is used here since it is used for vertical partition thickness.
				if ( !PipingSystemDomains( DomainCtr ).FullHorizInsPresent && ThisArea > 0.0 ) {
					if ( 2 * ( PipingSystemDomains( DomainCtr ).HorizInsWidth + PipingSystemDomains( DomainCtr ).VertInsThickness ) > PipingSystemDomains( DomainCtr ).SlabWidth ||
						2 * ( PipingSystemDomains( DomainCtr ).HorizInsWidth + PipingSystemDomains( DomainCtr ).VertInsThickness ) > PipingSystemDomains( DomainCtr ).SlabLength ) {
						ShowContinueError( RoutineName + ": Perimeter insulation width is too large." );
						ShowContinueError( "This would cause overlapping insulation. Check inputs." );
						ShowContinueError( "Defaulting to full horizontal insulation." );
						ShowContinueError( "Found in: " + Domain( ZoneCoupledDomainCtr ).ObjName );
						PipingSystemDomains( DomainCtr ).FullHorizInsPresent = true;
					}
				}

				// Set ground domain dimensions
				PipingSystemDomains( DomainCtr ).Extents.Xmax = Domain( ZoneCoupledDomainCtr ).PerimeterOffset + PipingSystemDomains( DomainCtr ).SlabWidth / 2;
				PipingSystemDomains( DomainCtr ).Extents.Ymax = Domain( ZoneCoupledDomainCtr ).Depth;
				PipingSystemDomains( DomainCtr ).Extents.Zmax = Domain( ZoneCoupledDomainCtr ).PerimeterOffset + PipingSystemDomains( DomainCtr ).SlabLength / 2;

				// Get mesh parameters

				// Mesh inputs
				PipingSystemDomains( DomainCtr ).Mesh.X.thisMeshDistribution = MeshDistribution::Geometric;
				PipingSystemDomains( DomainCtr ).Mesh.Y.thisMeshDistribution = MeshDistribution::Geometric;
				PipingSystemDomains( DomainCtr ).Mesh.Z.thisMeshDistribution = MeshDistribution::Geometric;

				Real64 MeshCoefficient = rNumericArgs( 12 );
				PipingSystemDomains( DomainCtr ).Mesh.X.GeometricSeriesCoefficient = MeshCoefficient;
				PipingSystemDomains( DomainCtr ).Mesh.Y.GeometricSeriesCoefficient = MeshCoefficient;
				PipingSystemDomains( DomainCtr ).Mesh.Z.GeometricSeriesCoefficient = MeshCoefficient;

				int MeshCount = rNumericArgs( 13 );
				PipingSystemDomains( DomainCtr ).Mesh.X.RegionMeshCount = MeshCount;
				PipingSystemDomains( DomainCtr ).Mesh.Y.RegionMeshCount = MeshCount;
				PipingSystemDomains( DomainCtr ).Mesh.Z.RegionMeshCount = MeshCount;

				// Soil properties
				PipingSystemDomains( DomainCtr ).GroundProperties.Conductivity = Domain( ZoneCoupledDomainCtr ).SoilConductivity;
				PipingSystemDomains( DomainCtr ).GroundProperties.Density = Domain( ZoneCoupledDomainCtr ).SoilDensity;
				PipingSystemDomains( DomainCtr ).GroundProperties.SpecificHeat = Domain( ZoneCoupledDomainCtr ).SoilSpecificHeat;

				// Moisture properties
				PipingSystemDomains( DomainCtr ).Moisture.Theta_liq = Domain( ZoneCoupledDomainCtr ).MoistureContent / 100.0;
				PipingSystemDomains( DomainCtr ).Moisture.Theta_sat = Domain( ZoneCoupledDomainCtr ).SaturationMoistureContent / 100.0;

				PipingSystemDomains( DomainCtr ).NumSlabCells = PipingSystemDomains( DomainCtr ).Mesh.Y.RegionMeshCount;

				// Farfield model
				PipingSystemDomains( DomainCtr ).Farfield.groundTempModel = GetGroundTempModelAndInit( cAlphaArgs( 2 ), cAlphaArgs( 3 ) );

				// Other parameters
				PipingSystemDomains( DomainCtr ).SimControls.Convergence_CurrentToPrevIteration = 0.001;
				PipingSystemDomains( DomainCtr ).SimControls.MaxIterationsPerTS = 250;

				// additional evapotranspiration parameter, min/max validated by IP
				PipingSystemDomains( DomainCtr ).Moisture.GroundCoverCoefficient = Domain( ZoneCoupledDomainCtr ).EvapotranspirationCoeff;

				// setup output variables
				SetupZoneCoupledOutputVariables( ZoneCoupledDomainCtr );

			}

		}

	void
	ReadBasementInputs(
		int const StartingDomainNumForBasement,
		int const NumBasements,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       Summer 2014  Sushobhit Acharya to accommodate basement calculations
		//       RE-ENGINEERED  na

		// Using/Aliasing
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::VerifyName;
		using InputProcessor::MakeUPPERCase;
		using namespace DataIPShortCuts;
		using DataSurfaces::OSCM;
		using General::TrimSigDigits;
		using DataHeatBalance::Material;
		using DataHeatBalance::TotMaterials;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ReadBasementInputs" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int BasementCtr;
		int DomainNum;
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		int CurIndex;
		int NumSurfacesWithThisOSCM;
		bool IsBlank;
		bool IsNotOK;
		Real64 ThisArea;

		struct GroundDomainData
		{
			// Members
			std::string ObjName;
			Real64 Depth;
			Real64 AspectRatio;
			Real64 PerimeterOffset;
			Real64 MinSurfTemp;
			int MonthOfMinSurfTemp;
			Real64 HorizInsWidth;
			Real64 VertInsDepth;
			std::string HorizInsMaterial;
			std::string VertInsMaterial;

			// Default Constructor
			GroundDomainData() :
				Depth( 0.0 ),
				AspectRatio( 0.0 ),
				PerimeterOffset( 0.0 ),
				MinSurfTemp( 0.0 ),
				MonthOfMinSurfTemp( 0 ),
				HorizInsWidth( 0.0 ),
				VertInsDepth( 0.0 )
			{}

		};

		// Object Data
		Array1D< GroundDomainData > Domain( NumBasements );

		// initialize these counters properly so they can be incremented within the DO loop
		DomainNum = StartingDomainNumForBasement - 1;

		// For each domain, we need to process the inputs into a local array of derived type, then resolve each one, creating definitions for a zone coupled domain.
		// This way, the outer get input routines can handle it as though they were generalized routines

		for ( BasementCtr = 1; BasementCtr <= NumBasements; ++BasementCtr ) {

			// Increment the domain counters here
			++DomainNum;

			// Read all the inputs for this domain object
			GetObjectItem( ObjName_ZoneCoupled_Basement, BasementCtr, cAlphaArgs,
				NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			// Get the name, validate
			Domain( BasementCtr ).ObjName = cAlphaArgs( 1 );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), Domain, &GroundDomainData::ObjName, BasementCtr - 1, IsNotOK, IsBlank, ObjName_ZoneCoupled_Basement + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				cAlphaArgs( 1 ) = "Duplicate name encountered";
			} else if ( IsBlank ) {
				ErrorsFound = true;
				cAlphaArgs( 1 ) = "Blank name encountered";
			}

			// Read in the some of the inputs into the local type for clarity during transition
			Domain( BasementCtr ).Depth = rNumericArgs( 1 );
			Domain( BasementCtr ).AspectRatio = rNumericArgs( 2 );
			Domain( BasementCtr ).PerimeterOffset = rNumericArgs( 3 );
			Domain( BasementCtr ).HorizInsWidth = rNumericArgs( 10 );
			Domain( BasementCtr ).VertInsDepth = rNumericArgs( 12 );

			// Other inputs
			PipingSystemDomains( DomainNum ).Name = cAlphaArgs( 1 );

			// Soil properties, validated min/max by IP
			PipingSystemDomains( DomainNum ).GroundProperties.Conductivity = rNumericArgs( 4 );
			PipingSystemDomains( DomainNum ).GroundProperties.Density = rNumericArgs( 5 );
			PipingSystemDomains( DomainNum ).GroundProperties.SpecificHeat = rNumericArgs( 6 );

			// Moisture properties, validated min/max by IP, and converted to a fraction for computation here
			PipingSystemDomains( DomainNum ).Moisture.Theta_liq = rNumericArgs( 7 ) / 100.0;
			PipingSystemDomains( DomainNum ).Moisture.Theta_sat = rNumericArgs( 8 ) / 100.0;

			// check if there are blank inputs related to the basement,
			if ( lNumericFieldBlanks( 11 ) || lAlphaFieldBlanks( 5 ) || lAlphaFieldBlanks( 10 ) ) {
				ShowSevereError( "Erroneous basement inputs for " + ObjName_ZoneCoupled_Basement + '=' + cAlphaArgs( 1 ) );
				ShowContinueError( "At least one basement input was left blank." );
				ErrorsFound = true;
			}

			// Basement zone depth
			CurIndex = 11;
			PipingSystemDomains( DomainNum ).BasementZone.Depth = rNumericArgs( CurIndex );
			if ( PipingSystemDomains( DomainNum ).BasementZone.Depth >= Domain( BasementCtr ).Depth || PipingSystemDomains( DomainNum ).BasementZone.Depth <= 0.0 ) {
				ShowSevereError( "Invalid " + cNumericFieldNames( CurIndex ) );
				ShowContinueError( "Found in: " + PipingSystemDomains( DomainNum ).Name );
				ErrorsFound = true;
			}

			// get boundary condition model names and indices --error check
			CurIndex = 4;
			PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMName = cAlphaArgs( CurIndex );
			PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex = FindItemInList( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMName, OSCM );
			if ( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex <= 0 ) {
				IssueSevereInputFieldError( RoutineName, ObjName_ZoneCoupled_Basement, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Could not match with an Other Side Conditions Model input object.", ErrorsFound );
			} else {
				NumSurfacesWithThisOSCM = GetSurfaceCountForOSCM( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex );
				if ( NumSurfacesWithThisOSCM <= 0 ) {
					IssueSevereInputFieldError( RoutineName, ObjName_ZoneCoupled_Basement, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Entry matched an Other Side Conditions Model, but no surfaces were found to be using this Other Side Conditions Model.", ErrorsFound );
				} else {
					PipingSystemDomains( DomainNum ).BasementZone.FloorSurfacePointers.allocate( NumSurfacesWithThisOSCM );
					PipingSystemDomains( DomainNum ).BasementZone.FloorSurfacePointers = GetSurfaceIndecesForOSCM( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex, NumSurfacesWithThisOSCM );
					PipingSystemDomains( DomainNum ).ZoneCoupledSurfaces.allocate( NumSurfacesWithThisOSCM );
					// Create GetSurfaceDataForOSCM function
					PipingSystemDomains( DomainNum ).ZoneCoupledSurfaces = GetSurfaceDataForOSCM( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex, NumSurfacesWithThisOSCM );
				}
			}

			CurIndex = 8;
			PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMName = cAlphaArgs( CurIndex );
			PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex = FindItemInList( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMName, OSCM );
			if ( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex <= 0 ) {
				IssueSevereInputFieldError( RoutineName, ObjName_ZoneCoupled_Basement, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Could not match with an Other Side Conditions Model input object.", ErrorsFound );
				ErrorsFound = true;
			} else {
				NumSurfacesWithThisOSCM = GetSurfaceCountForOSCM( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex );
				if ( NumSurfacesWithThisOSCM <= 0 ) {
					IssueSevereInputFieldError( RoutineName, ObjName_ZoneCoupled_Basement, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Entry matched an Other Side Conditions Model, but no surfaces were found to be using this Other Side Conditions Model.", ErrorsFound );
					ErrorsFound = true;
				} else {
					PipingSystemDomains( DomainNum ).BasementZone.WallSurfacePointers.allocate( NumSurfacesWithThisOSCM );
					PipingSystemDomains( DomainNum ).BasementZone.WallSurfacePointers = GetSurfaceIndecesForOSCM( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex, NumSurfacesWithThisOSCM );
				}
			}

			// get some convergence tolerances, minimum/maximum are enforced by the IP, along with default values if user left them blank
			PipingSystemDomains( DomainNum ).SimControls.Convergence_CurrentToPrevIteration = 0.01;
			PipingSystemDomains( DomainNum ).SimControls.MaxIterationsPerTS = 250;

			// additional evapotranspiration parameter, min/max validated by IP
			PipingSystemDomains( DomainNum ).Moisture.GroundCoverCoefficient = rNumericArgs( 9 );

			// assign the mesh count
			int meshCount = 4;
			PipingSystemDomains( DomainNum ).Mesh.X.RegionMeshCount = meshCount;
			PipingSystemDomains( DomainNum ).Mesh.Y.RegionMeshCount = meshCount;
			PipingSystemDomains( DomainNum ).Mesh.Z.RegionMeshCount = meshCount;

			PipingSystemDomains( DomainNum ).Mesh.X.thisMeshDistribution = MeshDistribution::Uniform;
			PipingSystemDomains( DomainNum ).Mesh.Y.thisMeshDistribution = MeshDistribution::Uniform;
			PipingSystemDomains( DomainNum ).Mesh.Z.thisMeshDistribution = MeshDistribution::Uniform;

			// Initialize properties for basement interface cells
			PipingSystemDomains( DomainNum ).BasementInterfaceProperties.Conductivity = 500.0;
			PipingSystemDomains( DomainNum ).BasementInterfaceProperties.SpecificHeat = 1.0;
			PipingSystemDomains( DomainNum ).BasementInterfaceProperties.Density = 1.0;

			// set flag for horizontal insulation
			// Check cAlphaArgs value
			if ( SameString( cAlphaArgs( 5 ), "NO" ) ) {
				PipingSystemDomains( DomainNum ).HorizInsPresentFlag = false;
			} else if ( SameString( cAlphaArgs( 5 ), "YES" ) ) {
				PipingSystemDomains( DomainNum ).HorizInsPresentFlag = true;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + "=" + cAlphaArgs( 5 ) );
				ShowContinueError( "Found in: " + PipingSystemDomains( DomainNum ).Name );
				ErrorsFound = true;
			}

			// Get horizontal insulation material properties
			if ( PipingSystemDomains( DomainNum ).HorizInsPresentFlag ) {
				Domain( BasementCtr ).HorizInsMaterial = cAlphaArgs( 6 );
				PipingSystemDomains( DomainNum ).HorizInsMaterialNum = FindItemInList( cAlphaArgs( 6 ), Material, TotMaterials );
				if ( PipingSystemDomains( DomainNum ).HorizInsMaterialNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 6 ) + "=" + cAlphaArgs( 6 ) );
					ShowContinueError( "Found in: " + PipingSystemDomains( DomainNum ).Name );
					ErrorsFound = true;
				} else {
					PipingSystemDomains( DomainNum ).HorizInsThickness = Material( PipingSystemDomains( DomainNum ).HorizInsMaterialNum ).Thickness;
					PipingSystemDomains( DomainNum ).HorizInsProperties.Density = Material( PipingSystemDomains( DomainNum ).HorizInsMaterialNum ).Density;
					PipingSystemDomains( DomainNum ).HorizInsProperties.SpecificHeat = Material( PipingSystemDomains( DomainNum ).HorizInsMaterialNum ).SpecHeat;
					PipingSystemDomains( DomainNum ).HorizInsProperties.Conductivity = Material( PipingSystemDomains( DomainNum ).HorizInsMaterialNum ).Conductivity;
				}

				// Set flag for horizontal insulation extents
				if ( SameString( cAlphaArgs( 7 ), "PERIMETER" ) ) {
					PipingSystemDomains( DomainNum ).FullHorizInsPresent = false;
					// Horizontal insulation perimeter width
					if ( Domain( BasementCtr ).HorizInsWidth > 0.0 ) {
						PipingSystemDomains( DomainNum ).HorizInsWidth = Domain( BasementCtr ).HorizInsWidth;
					} else {
						ShowSevereError( "Invalid " + cNumericFieldNames( 10 ) );
						ShowContinueError( "Found in: " + PipingSystemDomains( DomainNum ).Name );
						ErrorsFound = true;
					}
				} else if ( SameString( cAlphaArgs( 7 ), "FULL" ) ) {
					PipingSystemDomains( DomainNum ).FullHorizInsPresent = true;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 7 ) + "=" + cAlphaArgs( 7 ) );
					ShowContinueError( "Found in: " + PipingSystemDomains( DomainNum ).Name );
					ErrorsFound = true;
				}

				// Horizontal insulation perimeter width
				PipingSystemDomains( DomainNum ).HorizInsWidth = Domain( BasementCtr ).HorizInsWidth;

			}

			// set flag for vertical insulation
			if ( SameString( cAlphaArgs( 9 ), "NO" ) ) {
				PipingSystemDomains( DomainNum ).VertInsPresentFlag = false;
			} else if ( SameString( cAlphaArgs( 9 ), "YES" ) ) {
				PipingSystemDomains( DomainNum ).VertInsPresentFlag = true;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 9 ) + "=" + cAlphaArgs( 9 ) );
				ShowContinueError( "Found in: " + PipingSystemDomains( DomainNum ).Name );
				ErrorsFound = true;
			}

			// Get vertical insulation material properties
			if ( PipingSystemDomains( DomainNum ).VertInsPresentFlag ) {
				// Check if vertical insulation is in domain
				if ( Domain( BasementCtr ).VertInsDepth >= Domain( BasementCtr ).Depth || Domain( BasementCtr ).VertInsDepth <= 0.0 ) {
					ShowSevereError( "Invalid " + cNumericFieldNames( 12 ) );
					ShowContinueError( "Found in: " + PipingSystemDomains( DomainNum ).Name );
					ErrorsFound = true;
				} else {
					// Set insulation depth
					PipingSystemDomains( DomainNum ).VertInsDepth = Domain( BasementCtr ).VertInsDepth;
				}

				Domain( BasementCtr ).VertInsMaterial = cAlphaArgs( 10 );
				PipingSystemDomains( DomainNum ).VertInsMaterialNum = FindItemInList( cAlphaArgs( 10 ), Material, TotMaterials );
				if ( PipingSystemDomains( DomainNum ).VertInsMaterialNum == 0 ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 10 ) + "=" + cAlphaArgs( 10 ) );
					ShowContinueError( "Found in: " + PipingSystemDomains( DomainNum ).Name );
					ErrorsFound = true;
				} else {
					PipingSystemDomains( DomainNum ).VertInsThickness = Material( PipingSystemDomains( DomainNum ).VertInsMaterialNum ).Thickness;
					PipingSystemDomains( DomainNum ).VertInsProperties.Density = Material( PipingSystemDomains( DomainNum ).VertInsMaterialNum ).Density;
					PipingSystemDomains( DomainNum ).VertInsProperties.SpecificHeat = Material( PipingSystemDomains( DomainNum ).VertInsMaterialNum ).SpecHeat;
					PipingSystemDomains( DomainNum ).VertInsProperties.Conductivity = Material( PipingSystemDomains( DomainNum ).VertInsMaterialNum ).Conductivity;
				}
			}

			// Set simulation interval flag
			if ( SameString( cAlphaArgs( 11 ), "TIMESTEP" ) ) {
				PipingSystemDomains( DomainNum ).SimTimestepFlag = true;
			} else if ( SameString( cAlphaArgs( 11 ), "HOURLY" ) ) {
				PipingSystemDomains( DomainNum ).SimHourlyFlag = true;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 11 ) + "=" + cAlphaArgs( 11 ) );
				ShowContinueError( "Found in: " + PipingSystemDomains( DomainNum ).Name );
				ErrorsFound = true;
			}

			// Farfield ground temperature model
			PipingSystemDomains( DomainNum ).Farfield.groundTempModel = GetGroundTempModelAndInit( cAlphaArgs( 2 ), cAlphaArgs( 3 ) );

			// Domain perimeter offset
			PipingSystemDomains( DomainNum ).PerimeterOffset = Domain( BasementCtr ).PerimeterOffset;

			// Total surface area
			ThisArea = 0.0;

			for ( int SurfCtr = 1, SurfCtr_end = isize( PipingSystemDomains( DomainNum ).ZoneCoupledSurfaces ); SurfCtr <= SurfCtr_end; ++SurfCtr ) {
				ThisArea += PipingSystemDomains( DomainNum ).ZoneCoupledSurfaces( SurfCtr ).SurfaceArea;
			}

			// Surface dimensions
			PipingSystemDomains( DomainNum ).BasementZone.Width = sqrt( ThisArea / Domain( BasementCtr ).AspectRatio );
			PipingSystemDomains( DomainNum ).BasementZone.Length = PipingSystemDomains( DomainNum ).BasementZone.Width * Domain( BasementCtr ).AspectRatio;

			// Set ground domain dimensions
			// get width and length from aspect ratio later
			PipingSystemDomains( DomainNum ).Extents.Xmax = Domain( BasementCtr ).PerimeterOffset + PipingSystemDomains( DomainNum ).BasementZone.Width / 2;
			PipingSystemDomains( DomainNum ).Extents.Ymax = Domain( BasementCtr ).Depth;
			PipingSystemDomains( DomainNum ).Extents.Zmax = Domain( BasementCtr ).PerimeterOffset + PipingSystemDomains( DomainNum ).BasementZone.Length / 2;

			// Check horizontal insulation width so as to prevent overlapping insulation. VertInsThickness is used here since it is used for vertical partition thickness.
			if ( !PipingSystemDomains( DomainNum ).FullHorizInsPresent && ThisArea > 0.0 ) {
				if ( ( PipingSystemDomains( DomainNum ).HorizInsWidth + PipingSystemDomains( DomainNum ).VertInsThickness ) > PipingSystemDomains( DomainNum ).BasementZone.Width / 2.0 ||
					( PipingSystemDomains( DomainNum ).HorizInsWidth + PipingSystemDomains( DomainNum ).VertInsThickness ) > PipingSystemDomains( DomainNum ).BasementZone.Length / 2.0 ) {
					ShowContinueError( RoutineName + ": Perimeter insulation width is too large." );
					ShowContinueError( "This would cause overlapping insulation. Check inputs." );
					ShowContinueError( "Defaulting to full horizontal insulation." );
					ShowContinueError( "Found in: " + PipingSystemDomains( DomainNum ).Name );
					PipingSystemDomains( DomainNum ).FullHorizInsPresent = true;
				}
			}

			//******* We'll first set up the domain ********
			PipingSystemDomains( DomainNum ).IsActuallyPartOfAHorizontalTrench = false;
			PipingSystemDomains( DomainNum ).HasAPipeCircuit = false;
			PipingSystemDomains( DomainNum ).HasZoneCoupledSlab = false;
			PipingSystemDomains( DomainNum ).HasBasement = false;
			PipingSystemDomains( DomainNum ).HasZoneCoupledBasement = true;

			// Domain name
			PipingSystemDomains( DomainNum ).Name = Domain( BasementCtr ).ObjName;

			// setup output variables
			SetupZoneCoupledOutputVariables( BasementCtr );

		}

	}

	void
	ReadBESTESTInputs(
		int const TotalNumDomains,
		bool & ErrorsFound
	)
	{
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts;

		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem

		int NumBESTESTObjects = GetNumObjectsFound( ObjName_BESTEST_SurfaceConditions );

		if ( !( NumBESTESTObjects > 0 ) ) return;

		for ( int DomainCtr = 1; DomainCtr <= TotalNumDomains; ++DomainCtr ) {

			for ( int BESTESTCtr = 1; BESTESTCtr <= NumBESTESTObjects; ++BESTESTCtr ) {
				GetObjectItem( ObjName_BESTEST_SurfaceConditions, BESTESTCtr, cAlphaArgs,
					NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				if ( PipingSystemDomains( DomainCtr ).Name == cAlphaArgs( 2 ) ) {

					if ( !PipingSystemDomains( DomainCtr ).BESTESTFlag ) {
						if ( SameString( cAlphaArgs( 3 ), "WEATHERFILE" ) ) {
							PipingSystemDomains( DomainCtr ).BESTESTFlag = true;
							PipingSystemDomains( DomainCtr ).BESTESTConstConvCoeff = false;
						} else if ( SameString( cAlphaArgs( 3 ), "CONSTCONVCOEFFICIENT" ) ) {
							PipingSystemDomains( DomainCtr ).BESTESTFlag = true;
							PipingSystemDomains( DomainCtr ).BESTESTConstConvCoeff = true;
							PipingSystemDomains( DomainCtr ).BESTESTSurfaceConvCoefficient = rNumericArgs( 1 );
							PipingSystemDomains( DomainCtr ).BESTESTGroundSurfTemp = rNumericArgs( 2 );
						} else {
							// Bad input
							ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + "=" + cAlphaArgs( 3 ) );
 							ShowContinueError( "Found in: " + cAlphaArgs( 1 ) );
							ErrorsFound = true;
						}
					} else { //
						// Error--this has already been found
						ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + "=" + cAlphaArgs( 2 ) );
 						ShowContinueError( "Found in: " + cAlphaArgs( 1 ) );
						ErrorsFound = true;
					}
				}
				break;
			}

		}
	}

	void
	ReadPipeCircuitInputs(
		int const NumPipeCircuits,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Using/Aliasing
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts;
		using namespace DataLoopNode;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ReadPipeCircuitInputs" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumPipeSegments;
		int NumAlphas;
		int NumNumbers;
		int IOStatus;
		int PipeCircuitCounter;
		int ThisCircuitPipeSegmentCounter;
		bool IsNotOK;
		bool IsBlank;
		int CurIndex;
		int NumAlphasBeforeSegmentOne;

		for ( PipeCircuitCounter = 1; PipeCircuitCounter <= NumPipeCircuits; ++PipeCircuitCounter ) {

			// Read all the inputs for this pipe circuit
			GetObjectItem( ObjName_Circuit, PipeCircuitCounter, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			// Get the name, validate
			PipingSystemCircuits( PipeCircuitCounter ).Name = cAlphaArgs( 1 );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PipingSystemCircuits, PipeCircuitCounter - 1, IsNotOK, IsBlank, ObjName_Circuit + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				cAlphaArgs( 1 ) = "Duplicate name encountered";
			} else if ( IsBlank ) {
				ErrorsFound = true;
				cAlphaArgs( 1 ) = "Blank name encountered";
			}

			// Read pipe thermal properties, validated by IP
			PipingSystemCircuits( PipeCircuitCounter ).PipeProperties.Conductivity = rNumericArgs( 1 );
			PipingSystemCircuits( PipeCircuitCounter ).PipeProperties.Density = rNumericArgs( 2 );
			PipingSystemCircuits( PipeCircuitCounter ).PipeProperties.SpecificHeat = rNumericArgs( 3 );

			// Read pipe sizing, validated individually by IP, validated comparison here
			PipingSystemCircuits( PipeCircuitCounter ).PipeSize.InnerDia = rNumericArgs( 4 );
			PipingSystemCircuits( PipeCircuitCounter ).PipeSize.OuterDia = rNumericArgs( 5 );
			if ( PipingSystemCircuits( PipeCircuitCounter ).PipeSize.InnerDia >= PipingSystemCircuits( PipeCircuitCounter ).PipeSize.OuterDia ) {
				CurIndex = 5;
				IssueSevereInputFieldError( RoutineName, ObjName_Circuit, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Outer diameter must be greater than inner diameter.", ErrorsFound );
			}

			// Read design flow rate, validated positive by IP
			PipingSystemCircuits( PipeCircuitCounter ).DesignVolumeFlowRate = rNumericArgs( 6 );

			// Read inlet and outlet node names and validate them
			PipingSystemCircuits( PipeCircuitCounter ).InletNodeName = cAlphaArgs( 2 );
			PipingSystemCircuits( PipeCircuitCounter ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, ObjName_Circuit, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			if ( PipingSystemCircuits( PipeCircuitCounter ).InletNodeNum == 0 ) {
				CurIndex = 2;
				IssueSevereInputFieldError( RoutineName, ObjName_Circuit, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Bad node name.", ErrorsFound );
			}
			PipingSystemCircuits( PipeCircuitCounter ).OutletNodeName = cAlphaArgs( 3 );
			PipingSystemCircuits( PipeCircuitCounter ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, ObjName_Circuit, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			if ( PipingSystemCircuits( PipeCircuitCounter ).OutletNodeNum == 0 ) {
				CurIndex = 3;
				IssueSevereInputFieldError( RoutineName, ObjName_Circuit, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Bad node name.", ErrorsFound );
			}
			TestCompSet( ObjName_Circuit, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), "Piping System Circuit Nodes" );

			// Convergence tolerance values, validated by IP
			PipingSystemCircuits( PipeCircuitCounter ).Convergence_CurrentToPrevIteration = rNumericArgs( 7 );
			PipingSystemCircuits( PipeCircuitCounter ).MaxIterationsPerTS = rNumericArgs( 8 );

			// Radial mesh inputs, validated by IP
			// -- mesh thickness should be considered slightly dangerous until mesh dev engine can trap erroneous values
			PipingSystemCircuits( PipeCircuitCounter ).NumRadialCells = rNumericArgs( 9 );
			PipingSystemCircuits( PipeCircuitCounter ).RadialMeshThickness = rNumericArgs( 10 );

			// Read number of pipe segments for this circuit, allocate arrays
			NumPipeSegments = rNumericArgs( 11 );
			PipingSystemCircuits( PipeCircuitCounter ).PipeSegmentIndeces.allocate( NumPipeSegments );
			PipingSystemCircuits( PipeCircuitCounter ).PipeSegmentNames.allocate( NumPipeSegments );

			// Check for blank or missing or mismatched number...
			NumAlphasBeforeSegmentOne = 3;
			for ( ThisCircuitPipeSegmentCounter = 1; ThisCircuitPipeSegmentCounter <= NumPipeSegments; ++ThisCircuitPipeSegmentCounter ) {
				CurIndex = ThisCircuitPipeSegmentCounter + NumAlphasBeforeSegmentOne;
				if ( lAlphaFieldBlanks( CurIndex ) ) {
					IssueSevereInputFieldError( RoutineName, ObjName_Circuit, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Expected a pipe segment name, check pipe segment count input field.", ErrorsFound );
				}
				PipingSystemCircuits( PipeCircuitCounter ).PipeSegmentNames( ThisCircuitPipeSegmentCounter ) = cAlphaArgs( CurIndex );
			}

		} // All pipe circuits in input

	}

	void
	ReadPipeSegmentInputs(
		int const NumPipeSegmentsInInput,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Using/Aliasing
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ReadPipeSegmentInputs" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SegmentCtr;
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		int CurIndex;
		bool IsNotOK;
		bool IsBlank;

		// Read in all pipe segments
		for ( SegmentCtr = 1; SegmentCtr <= NumPipeSegmentsInInput; ++SegmentCtr ) {

			// Read all inputs for this pipe segment
			GetObjectItem( ObjName_Segment, SegmentCtr, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			// Get the name, validate
			PipingSystemSegments( SegmentCtr ).Name = cAlphaArgs( 1 );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), PipingSystemSegments, SegmentCtr - 1, IsNotOK, IsBlank, ObjName_Segment + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				cAlphaArgs( 1 ) = "Duplicate name encountered";
			} else if ( IsBlank ) {
				ErrorsFound = true;
				cAlphaArgs( 1 ) = "Blank name encountered";
			}

			// Read in the pipe location, validated as positive by IP
			// -- note that these values will be altered by the main GetInput routine in two ways:
			//   1) shift for basement wall if selected
			//   2) invert y direction to be measured from domain bottom surface for calculations
			PipingSystemSegments( SegmentCtr ).PipeLocation = PointF( rNumericArgs( 1 ), rNumericArgs( 2 ) );

			// Read in the flow direction
			{ auto const SELECT_CASE_var( stripped( cAlphaArgs( 2 ) ) );
			if ( SELECT_CASE_var == "INCREASINGZ" ) {
				PipingSystemSegments( SegmentCtr ).FlowDirection = SegmentFlow::IncreasingZ;
			} else if ( SELECT_CASE_var == "DECREASINGZ" ) {
				PipingSystemSegments( SegmentCtr ).FlowDirection = SegmentFlow::DecreasingZ;
			} else {
				CurIndex = 2;
				IssueSevereInputFieldError( RoutineName, ObjName_Segment, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Invalid flow direction, use one of the available keys.", ErrorsFound );
			}}

		}

	}

	void
	ReadHorizontalTrenchInputs(
		int const StartingDomainNumForHorizontal,
		int const StartingCircuitNumForHorizontal,
		int const StartingSegmentNumForHorizontal,
		int const NumHorizontalTrenchesInInput,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   September 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Using/Aliasing
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts;
		using namespace DataLoopNode;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using namespace GroundTemperatureManager;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ReadHorizontalTrenchInputs" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HorizontalGHXCtr;
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		int CurIndex;
		bool IsNotOK;
		bool IsBlank;
		int DomainCtr;
		int CircuitCtr;
		int SegmentCtr;
		int NumPipeSegments;
		int ThisCircuitPipeSegmentCounter;

		struct HorizontalTrenchData
		{
			// Members
			std::string ObjName;
			std::string InletNodeName;
			std::string OutletNodeName;
			Real64 AxialLength;
			Real64 PipeID;
			Real64 PipeOD;
			int NumPipes;
			Real64 BurialDepth;
			Real64 DesignFlowRate;
			Real64 SoilConductivity;
			Real64 SoilDensity;
			Real64 SoilSpecificHeat;
			Real64 PipeConductivity;
			Real64 PipeDensity;
			Real64 PipeSpecificHeat;
			Real64 InterPipeSpacing;
			Real64 MoistureContent;
			Real64 SaturationMoistureContent;
			Real64 EvapotranspirationCoeff;
			Real64 MinSurfTemp;
			int MonthOfMinSurfTemp;

			// Default Constructor
			HorizontalTrenchData() :
				AxialLength( 0.0 ),
				PipeID( 0.0 ),
				PipeOD( 0.0 ),
				NumPipes( 0 ),
				BurialDepth( 0.0 ),
				DesignFlowRate( 0.0 ),
				SoilConductivity( 0.0 ),
				SoilDensity( 0.0 ),
				SoilSpecificHeat( 0.0 ),
				PipeConductivity( 0.0 ),
				PipeDensity( 0.0 ),
				PipeSpecificHeat( 0.0 ),
				InterPipeSpacing( 0.0 ),
				MoistureContent( 0.0 ),
				SaturationMoistureContent( 0.0 ),
				EvapotranspirationCoeff( 0.0 ),
				MinSurfTemp( 0.0 ),
				MonthOfMinSurfTemp( 0 )
			{}

		};

		// Object Data
		Array1D< HorizontalTrenchData > HGHX( NumHorizontalTrenchesInInput );

		// initialize these counters properly so they can be incremented within the DO loop
		DomainCtr = StartingDomainNumForHorizontal - 1;
		CircuitCtr = StartingCircuitNumForHorizontal - 1;
		SegmentCtr = StartingSegmentNumForHorizontal - 1;

		// For each horizontal, we need to process the inputs into a local array of derived type,
		//  then resolve each one, creating definitions for a pipe domain, pipe circuit, and series of pipe segments
		// This way, the outer get input routines can handle it as though they were generalized routines

		// Read in all pipe segments
		for ( HorizontalGHXCtr = 1; HorizontalGHXCtr <= NumHorizontalTrenchesInInput; ++HorizontalGHXCtr ) {

			// Increment the domain and circuit counters here
			++DomainCtr;
			++CircuitCtr;

			// Read all inputs for this pipe segment
			GetObjectItem( ObjName_HorizTrench, HorizontalGHXCtr, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			// Get the name, validate
			HGHX( HorizontalGHXCtr ).ObjName = cAlphaArgs( 1 );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), HGHX, &HorizontalTrenchData::ObjName, HorizontalGHXCtr - 1, IsNotOK, IsBlank, ObjName_HorizTrench + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				cAlphaArgs( 1 ) = "Duplicate name encountered";
			} else if ( IsBlank ) {
				ErrorsFound = true;
				cAlphaArgs( 1 ) = "Blank name encountered";
			}

			// Read in the rest of the inputs into the local type for clarity during transition
			HGHX( HorizontalGHXCtr ).InletNodeName = cAlphaArgs( 2 );
			HGHX( HorizontalGHXCtr ).OutletNodeName = cAlphaArgs( 3 );
			HGHX( HorizontalGHXCtr ).DesignFlowRate = rNumericArgs( 1 );
			HGHX( HorizontalGHXCtr ).AxialLength = rNumericArgs( 2 );
			HGHX( HorizontalGHXCtr ).NumPipes = rNumericArgs( 3 );
			HGHX( HorizontalGHXCtr ).InterPipeSpacing = rNumericArgs( 4 );
			HGHX( HorizontalGHXCtr ).PipeID = rNumericArgs( 5 );
			HGHX( HorizontalGHXCtr ).PipeOD = rNumericArgs( 6 );
			HGHX( HorizontalGHXCtr ).BurialDepth = rNumericArgs( 7 );
			HGHX( HorizontalGHXCtr ).SoilConductivity = rNumericArgs( 8 );
			HGHX( HorizontalGHXCtr ).SoilDensity = rNumericArgs( 9 );
			HGHX( HorizontalGHXCtr ).SoilSpecificHeat = rNumericArgs( 10 );
			HGHX( HorizontalGHXCtr ).PipeConductivity = rNumericArgs( 11 );
			HGHX( HorizontalGHXCtr ).PipeDensity = rNumericArgs( 12 );
			HGHX( HorizontalGHXCtr ).PipeSpecificHeat = rNumericArgs( 13 );
			HGHX( HorizontalGHXCtr ).MoistureContent = rNumericArgs( 14 );
			HGHX( HorizontalGHXCtr ).SaturationMoistureContent = rNumericArgs( 15 );
			HGHX( HorizontalGHXCtr ).EvapotranspirationCoeff = rNumericArgs( 16 );

			//******* We'll first set up the domain ********
			// the extents will be: Zmax = axial length; Ymax = burial depth*2; Xmax = ( NumPipes+1 )*HorizontalPipeSpacing
			PipingSystemDomains( DomainCtr ).IsActuallyPartOfAHorizontalTrench = true;
			gio::write( PipingSystemDomains( DomainCtr ).Name, "( 'HorizontalTrenchDomain',I4 )" ) << HorizontalGHXCtr;
			PipingSystemDomains( DomainCtr ).Extents.Xmax = ( double( HGHX( HorizontalGHXCtr ).NumPipes ) + 1.0 ) * HGHX( HorizontalGHXCtr ).InterPipeSpacing;
			PipingSystemDomains( DomainCtr ).Extents.Ymax = 2.0 * HGHX( HorizontalGHXCtr ).BurialDepth;
			PipingSystemDomains( DomainCtr ).Extents.Zmax = HGHX( HorizontalGHXCtr ).AxialLength;

			// set up the mesh with some default parameters
			PipingSystemDomains( DomainCtr ).Mesh.X.RegionMeshCount = 4;
			PipingSystemDomains( DomainCtr ).Mesh.X.thisMeshDistribution = MeshDistribution::Uniform;
			PipingSystemDomains( DomainCtr ).Mesh.Y.RegionMeshCount = 4;
			PipingSystemDomains( DomainCtr ).Mesh.Y.thisMeshDistribution = MeshDistribution::Uniform;
			PipingSystemDomains( DomainCtr ).Mesh.Z.RegionMeshCount = 4;
			PipingSystemDomains( DomainCtr ).Mesh.Z.thisMeshDistribution = MeshDistribution::Uniform;

			// Soil properties
			PipingSystemDomains( DomainCtr ).GroundProperties.Conductivity = HGHX( HorizontalGHXCtr ).SoilConductivity;
			PipingSystemDomains( DomainCtr ).GroundProperties.Density = HGHX( HorizontalGHXCtr ).SoilDensity;
			PipingSystemDomains( DomainCtr ).GroundProperties.SpecificHeat = HGHX( HorizontalGHXCtr ).SoilSpecificHeat;

			// Moisture properties
			PipingSystemDomains( DomainCtr ).Moisture.Theta_liq = HGHX( HorizontalGHXCtr ).MoistureContent / 100.0;
			PipingSystemDomains( DomainCtr ).Moisture.Theta_sat = HGHX( HorizontalGHXCtr ).SaturationMoistureContent / 100.0;

			// Farfield model parameters
			PipingSystemDomains( DomainCtr ).Farfield.groundTempModel = GetGroundTempModelAndInit( cAlphaArgs( 4 ), cAlphaArgs( 5 ) );

			// Other parameters
			PipingSystemDomains( DomainCtr ).SimControls.Convergence_CurrentToPrevIteration = 0.001;
			PipingSystemDomains( DomainCtr ).SimControls.MaxIterationsPerTS = 250;

			// additional evapotranspiration parameter, min/max validated by IP
			PipingSystemDomains( DomainCtr ).Moisture.GroundCoverCoefficient = HGHX( HorizontalGHXCtr ).EvapotranspirationCoeff;

			// Allocate the circuit placeholder arrays
			PipingSystemDomains( DomainCtr ).CircuitNames.allocate( 1 );
			PipingSystemDomains( DomainCtr ).CircuitIndeces.allocate( 1 );
			PipingSystemDomains( DomainCtr ).CircuitNames( 1 ) = HGHX( HorizontalGHXCtr ).ObjName;

			//******* We'll next set up the circuit ********
			PipingSystemCircuits( CircuitCtr ).IsActuallyPartOfAHorizontalTrench = true;
			PipingSystemCircuits( CircuitCtr ).Name = HGHX( HorizontalGHXCtr ).ObjName;

			// Read pipe thermal properties
			PipingSystemCircuits( CircuitCtr ).PipeProperties.Conductivity = HGHX( HorizontalGHXCtr ).PipeConductivity;
			PipingSystemCircuits( CircuitCtr ).PipeProperties.Density = HGHX( HorizontalGHXCtr ).PipeDensity;
			PipingSystemCircuits( CircuitCtr ).PipeProperties.SpecificHeat = HGHX( HorizontalGHXCtr ).PipeSpecificHeat;

			// Pipe sizing
			PipingSystemCircuits( CircuitCtr ).PipeSize.InnerDia = HGHX( HorizontalGHXCtr ).PipeID;
			PipingSystemCircuits( CircuitCtr ).PipeSize.OuterDia = HGHX( HorizontalGHXCtr ).PipeOD;
			if ( PipingSystemCircuits( CircuitCtr ).PipeSize.InnerDia >= PipingSystemCircuits( CircuitCtr ).PipeSize.OuterDia ) {
				// CurIndex = 5
				// CALL IssueSevereInputFieldError( RoutineName, ObjName_Circuit, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), &
				//                            cAlphaArgs( CurIndex ), 'Outer diameter must be greater than inner diameter.', ErrorsFound )
			}

			// Read design flow rate, validated positive by IP
			PipingSystemCircuits( CircuitCtr ).DesignVolumeFlowRate = HGHX( HorizontalGHXCtr ).DesignFlowRate;

			// Read inlet and outlet node names and validate them
			PipingSystemCircuits( CircuitCtr ).InletNodeName = HGHX( HorizontalGHXCtr ).InletNodeName;
			PipingSystemCircuits( CircuitCtr ).InletNodeNum = GetOnlySingleNode( PipingSystemCircuits( CircuitCtr ).InletNodeName, ErrorsFound, ObjName_HorizTrench, HGHX( HorizontalGHXCtr ).ObjName, NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			if ( PipingSystemCircuits( CircuitCtr ).InletNodeNum == 0 ) {
				CurIndex = 2;
				// CALL IssueSevereInputFieldError( RoutineName, ObjName_Circuit, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), &
				//                                cAlphaArgs( CurIndex ), 'Bad node name.', ErrorsFound )
			}
			PipingSystemCircuits( CircuitCtr ).OutletNodeName = HGHX( HorizontalGHXCtr ).OutletNodeName;
			PipingSystemCircuits( CircuitCtr ).OutletNodeNum = GetOnlySingleNode( PipingSystemCircuits( CircuitCtr ).OutletNodeName, ErrorsFound, ObjName_HorizTrench, HGHX( HorizontalGHXCtr ).ObjName, NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			if ( PipingSystemCircuits( CircuitCtr ).OutletNodeNum == 0 ) {
				CurIndex = 3;
				// CALL IssueSevereInputFieldError( RoutineName, ObjName_Circuit, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), &
				//                                cAlphaArgs( CurIndex ), 'Bad node name.', ErrorsFound )
			}
			TestCompSet( ObjName_HorizTrench, HGHX( HorizontalGHXCtr ).ObjName, PipingSystemCircuits( CircuitCtr ).InletNodeName, PipingSystemCircuits( CircuitCtr ).OutletNodeName, "Piping System Circuit Nodes" );

			// Convergence tolerance values, validated by IP
			PipingSystemCircuits( CircuitCtr ).Convergence_CurrentToPrevIteration = 0.001;
			PipingSystemCircuits( CircuitCtr ).MaxIterationsPerTS = 100;

			// Radial mesh inputs, validated by IP
			// -- mesh thickness should be considered slightly dangerous until mesh dev engine can trap erroneous values
			PipingSystemCircuits( CircuitCtr ).NumRadialCells = 4;
			PipingSystemCircuits( CircuitCtr ).RadialMeshThickness = PipingSystemCircuits( CircuitCtr ).PipeSize.InnerDia / 2.0;

			// Read number of pipe segments for this circuit, allocate arrays
			NumPipeSegments = HGHX( HorizontalGHXCtr ).NumPipes;
			PipingSystemCircuits( CircuitCtr ).PipeSegmentIndeces.allocate( NumPipeSegments );
			PipingSystemCircuits( CircuitCtr ).PipeSegmentNames.allocate( NumPipeSegments );

			// Hard-code the segments
			for ( ThisCircuitPipeSegmentCounter = 1; ThisCircuitPipeSegmentCounter <= NumPipeSegments; ++ThisCircuitPipeSegmentCounter ) {
				gio::write( PipingSystemCircuits( CircuitCtr ).PipeSegmentNames( ThisCircuitPipeSegmentCounter ), "( 'HorizontalTrenchCircuit',I4,'Segment',I4 )" ) << HorizontalGHXCtr << ThisCircuitPipeSegmentCounter;
			}

			//******* Then we'll do the segments *******!
			for ( ThisCircuitPipeSegmentCounter = 1; ThisCircuitPipeSegmentCounter <= NumPipeSegments; ++ThisCircuitPipeSegmentCounter ) {
				++SegmentCtr;
				gio::write( PipingSystemSegments( SegmentCtr ).Name, "( 'HorizontalTrenchCircuit',I4,'Segment',I4 )" ) << HorizontalGHXCtr << ThisCircuitPipeSegmentCounter;

				PipingSystemSegments( SegmentCtr ).IsActuallyPartOfAHorizontalTrench = true;
				PipingSystemSegments( SegmentCtr ).PipeLocation = PointF( ThisCircuitPipeSegmentCounter * HGHX( HorizontalGHXCtr ).InterPipeSpacing, HGHX( HorizontalGHXCtr ).BurialDepth );

				if ( mod( ThisCircuitPipeSegmentCounter, 2 ) != 0 ) {
					PipingSystemSegments( SegmentCtr ).FlowDirection = SegmentFlow::IncreasingZ;
				} else {
					PipingSystemSegments( SegmentCtr ).FlowDirection = SegmentFlow::DecreasingZ;
				}

			}

		}

	}

	void
	SetupPipingSystemOutputVariables(
		int const TotalNumSegments,
		int const TotalNumCircuits
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   September 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PipeCircuitCounter;
		int SegmentCtr;

		for ( SegmentCtr = 1; SegmentCtr <= TotalNumSegments; ++SegmentCtr ) {

			if ( ! PipingSystemSegments( SegmentCtr ).IsActuallyPartOfAHorizontalTrench ) {

				SetupOutputVariable( "Pipe Segment Inlet Temperature [C]", PipingSystemSegments( SegmentCtr ).InletTemperature, "Plant", "Average", PipingSystemSegments( SegmentCtr ).Name );
				SetupOutputVariable( "Pipe Segment Outlet Temperature [C]", PipingSystemSegments( SegmentCtr ).OutletTemperature, "Plant", "Average", PipingSystemSegments( SegmentCtr ).Name );

				SetupOutputVariable( "Pipe Segment Fluid Heat Transfer Rate [W]", PipingSystemSegments( SegmentCtr ).FluidHeatLoss, "Plant", "Average", PipingSystemSegments( SegmentCtr ).Name );

			}

		}

		for ( PipeCircuitCounter = 1; PipeCircuitCounter <= TotalNumCircuits; ++PipeCircuitCounter ) {

			if ( ! PipingSystemCircuits( PipeCircuitCounter ).IsActuallyPartOfAHorizontalTrench ) {

				SetupOutputVariable( "Pipe Circuit Mass Flow Rate [kg/s]", PipingSystemCircuits( PipeCircuitCounter ).CurCircuitFlowRate, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );

				SetupOutputVariable( "Pipe Circuit Inlet Temperature [C]", PipingSystemCircuits( PipeCircuitCounter ).InletTemperature, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );
				SetupOutputVariable( "Pipe Circuit Outlet Temperature [C]", PipingSystemCircuits( PipeCircuitCounter ).OutletTemperature, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );

				SetupOutputVariable( "Pipe Circuit Fluid Heat Transfer Rate [W]", PipingSystemCircuits( PipeCircuitCounter ).FluidHeatLoss, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );

			} else { // it is a horizontal trench

				SetupOutputVariable( "Ground Heat Exchanger Mass Flow Rate [kg/s]", PipingSystemCircuits( PipeCircuitCounter ).CurCircuitFlowRate, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );

				SetupOutputVariable( "Ground Heat Exchanger Inlet Temperature [C]", PipingSystemCircuits( PipeCircuitCounter ).InletTemperature, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );
				SetupOutputVariable( "Ground Heat Exchanger Outlet Temperature [C]", PipingSystemCircuits( PipeCircuitCounter ).OutletTemperature, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );

				SetupOutputVariable( "Ground Heat Exchanger Fluid Heat Transfer Rate [W]", PipingSystemCircuits( PipeCircuitCounter ).FluidHeatLoss, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );

			}

		}

	}

	void
	SetupZoneCoupledOutputVariables(
		int const DomainNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   August 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		if ( PipingSystemDomains( DomainNum ).HasZoneCoupledSlab ) {
			// Zone-coupled slab outputs
			SetupOutputVariable( "GroundDomain Slab Zone Coupled Surface Heat Flux [W/m2]", PipingSystemDomains( DomainNum ).HeatFlux, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
			SetupOutputVariable( "GroundDomain Slab Zone Coupled Surface Temperature [C]", PipingSystemDomains( DomainNum ).ZoneCoupledSurfaceTemp, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		} else if ( PipingSystemDomains( DomainNum ).HasZoneCoupledBasement ) {
			// Zone-coupled basement wall outputs
			SetupOutputVariable( "GroundDomain Basement Wall Interface Heat Flux [W/m2]", PipingSystemDomains( DomainNum ).WallHeatFlux, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
			SetupOutputVariable( "GroundDomain Basement Wall Interface Temperature [C]", PipingSystemDomains( DomainNum ).BasementWallTemp, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
			// Zone-coupled basement floor outputs
			SetupOutputVariable( "GroundDomain Basement Floor Interface Heat Flux [W/m2]", PipingSystemDomains( DomainNum ).FloorHeatFlux, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
			SetupOutputVariable( "GroundDomain Basement Floor Interface Temperature [C]", PipingSystemDomains( DomainNum ).BasementFloorTemp, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		}

#ifdef CalcEnergyBalance
		SetupOutputVariable( "Maximum Domain Energy Imbalance [J]", PipingSystemDomains( DomainNum ).MaxEnergyImbalance, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		SetupOutputVariable( "Maximum Domain Energy Imbalance X Location []", PipingSystemDomains( DomainNum ).MaxEnergyImbalance_XLocation, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		SetupOutputVariable( "Maximum Domain Energy Imbalance Y Location []", PipingSystemDomains( DomainNum ).MaxEnergyImbalance_YLocation, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		SetupOutputVariable( "Maximum Domain Energy Imbalance Z Location []", PipingSystemDomains( DomainNum ).MaxEnergyImbalance_ZLocation, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );

		SetupOutputVariable( "Total Heat Flux Energy Uniform Heat Flux []", PipingSystemDomains( DomainNum ).TotalEnergyUniformHeatFlux, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		SetupOutputVariable( "Total Heat Flux Energy Weighted Heat Flux []", PipingSystemDomains( DomainNum ).TotalEnergyWeightedHeatFlux, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		SetupOutputVariable( "Heat Flux Weighting Factor []", PipingSystemDomains( DomainNum ).HeatFluxWeightingFactor, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );

		SetupOutputVariable( "Cell with Minimum Number of Sides Calculated []", PipingSystemDomains( DomainNum ).minNumberOfSidesCalculated, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		SetupOutputVariable( "Cell with Minimum Number of Sides Calculated X Location []", PipingSystemDomains( DomainNum ).minNumberOfSidesCalculated_XLocation, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		SetupOutputVariable( "Cell with Minimum Number of Sides Calculated Y Location []", PipingSystemDomains( DomainNum ).minNumberOfSidesCalculated_YLocation, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		SetupOutputVariable( "Cell with Minimum Number of Sides Calculated Z Location []", PipingSystemDomains( DomainNum ).minNumberOfSidesCalculated_ZLocation, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );

		SetupOutputVariable( "Cell with Maximum Number of Sides Calculated []", PipingSystemDomains( DomainNum ).maxNumberOfSidesCalculated, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		SetupOutputVariable( "Cell with Maximum Number of Sides Calculated X Location []", PipingSystemDomains( DomainNum ).maxNumberOfSidesCalculated_XLocation, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		SetupOutputVariable( "Cell with Maximum Number of Sides Calculated Y Location []", PipingSystemDomains( DomainNum ).maxNumberOfSidesCalculated_YLocation, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		SetupOutputVariable( "Cell with Maximum Number of Sides Calculated Z Location []", PipingSystemDomains( DomainNum ).maxNumberOfSidesCalculated_ZLocation, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );

		SetupOutputVariable( "Unweighted GroundDomain Slab Zone Coupled Surface Temperature []", PipingSystemDomains( DomainNum ).AvgUnweightedSurfTemp, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );

		SetupOutputVariable( "Max Temperature Difference Due To Energy Imbalance []", PipingSystemDomains( DomainNum ).maxTempDiffDueToImbalance, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		SetupOutputVariable( "Max Temperature Difference Due To Energy Imbalance X Location []", PipingSystemDomains( DomainNum ).maxTempDiffDueToImbalance_XLocation, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		SetupOutputVariable( "Max Temperature Difference Due To Energy Imbalance Y Location []", PipingSystemDomains( DomainNum ).maxTempDiffDueToImbalance_YLocation, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		SetupOutputVariable( "Max Temperature Difference Due To Energy Imbalance Z Location []", PipingSystemDomains( DomainNum ).maxTempDiffDueToImbalance_ZLocation, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
#endif

	}

	void
	InitPipingSystems(
		int const DomainNum,
		int const CircuitNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::TypeOf_PipingSystemPipeCircuit;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_GrndHtExchgHorizTrench;
		using DataGlobals::BeginSimFlag;
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::DayOfSim;
		using DataGlobals::HourOfDay;
		using DataGlobals::TimeStep;
		using DataGlobals::TimeStepZone;
		using DataGlobals::SecInHour;
		using DataGlobals::InitConvTemp;
		using DataLoopNode::Node;
		using PlantUtilities::SetComponentFlowRate;
		using FluidProperties::GetDensityGlycol;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitPipingSystems" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool errFlag;
		int InletNodeNum;
		int OutletNodeNum;
		int CircCtr;
		int SegCtr;
		int SegmentIndex;
		Real64 rho;
		int TypeToLookFor;

		// Do any one-time initializations
		if ( PipingSystemCircuits( CircuitNum ).NeedToFindOnPlantLoop ) {

			errFlag = false;

			if ( PipingSystemCircuits( CircuitNum ).IsActuallyPartOfAHorizontalTrench ) {
				TypeToLookFor = TypeOf_GrndHtExchgHorizTrench;
			} else {
				TypeToLookFor = TypeOf_PipingSystemPipeCircuit;
			}

			ScanPlantLoopsForObject( PipingSystemCircuits( CircuitNum ).Name, TypeToLookFor, PipingSystemCircuits( CircuitNum ).LoopNum, PipingSystemCircuits( CircuitNum ).LoopSideNum, PipingSystemCircuits( CircuitNum ).BranchNum, PipingSystemCircuits( CircuitNum ).CompNum, _, _, _, _, _, errFlag );

			if ( errFlag ) {
				ShowFatalError( "PipingSystems:" + RoutineName + ": Program terminated due to previous condition(s)." );
			}

			// Once we find ourselves on the plant loop, we can do other things
			rho = GetDensityGlycol( PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidName, InitConvTemp, PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidIndex, RoutineName );
			PipingSystemCircuits( CircuitNum ).DesignMassFlowRate = PipingSystemCircuits( CircuitNum ).DesignVolumeFlowRate * rho;

			PipingSystemCircuits( CircuitNum ).NeedToFindOnPlantLoop = false;

		}

		if ( PipingSystemDomains( DomainNum ).DomainNeedsToBeMeshed ) {

			PipingSystemDomains( DomainNum ).developMesh();

			// would be OK to do some post-mesh error handling here I think
			for ( CircCtr = 1; CircCtr <= isize( PipingSystemDomains( DomainNum ).CircuitIndeces ); ++CircCtr ) {
				for ( SegCtr = 1; SegCtr <= isize( PipingSystemCircuits( PipingSystemDomains( DomainNum ).CircuitIndeces( CircCtr ) ).PipeSegmentIndeces ); ++SegCtr ) {
					SegmentIndex = PipingSystemCircuits( PipingSystemDomains( DomainNum ).CircuitIndeces( CircCtr ) ).PipeSegmentIndeces( SegCtr );
					if ( ! PipingSystemSegments( SegmentIndex ).PipeCellCoordinatesSet ) {
						ShowSevereError( "PipingSystems:" + RoutineName + ":Pipe segment index not set." );
						ShowContinueError( "...Possibly because pipe segment was placed outside of the domain." );
						ShowContinueError( "...Verify piping system domain inputs, circuits, and segments." );
						ShowFatalError( "Preceding error causes program termination" );
					}
				}
			}

			PipingSystemDomains( DomainNum ).DomainNeedsToBeMeshed = false;

		}

		// The time init should be done here before we DoOneTimeInits because the DoOneTimeInits
		// includes a ground temperature initialization, which is based on the Cur%CurSimTimeSeconds variable
		// which would be carried over from the previous environment
		PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize = TimeStepSys * SecInHour;
		PipingSystemDomains( DomainNum ).Cur.CurSimTimeSeconds = ( DayOfSim - 1 ) * 24 + ( HourOfDay - 1 ) + ( TimeStep - 1 ) * TimeStepZone + SysTimeElapsed;

		// There are also some inits that are "close to one time" inits...(one-time in standalone, each envrn in E+)
		if ( ( BeginSimFlag && PipingSystemDomains( DomainNum ).BeginSimInit ) || ( BeginEnvrnFlag && PipingSystemDomains( DomainNum ).BeginSimEnvrn ) ) {

			// this seemed to clean up a lot of reverse DD stuff because fluid thermal properties were
			// being based on the inlet temperature, which wasn't updated until later
			InletNodeNum = PipingSystemCircuits( CircuitNum ).InletNodeNum;
			PipingSystemCircuits( CircuitNum ).CurCircuitInletTemp = Node( InletNodeNum ).Temp;
			PipingSystemCircuits( CircuitNum ).InletTemperature = PipingSystemCircuits( CircuitNum ).CurCircuitInletTemp;

			DoOneTimeInitializations( DomainNum, CircuitNum );

			PipingSystemDomains( DomainNum ).BeginSimInit = false;
			PipingSystemDomains( DomainNum ).BeginSimEnvrn = false;

		}

		// Shift history arrays only if necessary
		if ( std::abs( PipingSystemDomains( DomainNum ).Cur.CurSimTimeSeconds - PipingSystemDomains( DomainNum ).Cur.PrevSimTimeSeconds ) > 1.0e-6 ) {
			PipingSystemDomains( DomainNum ).Cur.PrevSimTimeSeconds = PipingSystemDomains( DomainNum ).Cur.CurSimTimeSeconds;
			ShiftTemperaturesForNewTimeStep( DomainNum );
			PipingSystemDomains( DomainNum ).DomainNeedsSimulation = true;
		}

		// Get the mass flow and inlet temperature to use for this time step
		InletNodeNum = PipingSystemCircuits( CircuitNum ).InletNodeNum;
		OutletNodeNum = PipingSystemCircuits( CircuitNum ).OutletNodeNum;
		PipingSystemCircuits( CircuitNum ).CurCircuitInletTemp = Node( InletNodeNum ).Temp;

		// request design, set component flow will decide what to give us based on restrictions and flow lock status
		PipingSystemCircuits( CircuitNum ).CurCircuitFlowRate = PipingSystemCircuits( CircuitNum ).DesignMassFlowRate;
		SetComponentFlowRate( PipingSystemCircuits( CircuitNum ).CurCircuitFlowRate, InletNodeNum, OutletNodeNum, PipingSystemCircuits( CircuitNum ).LoopNum, PipingSystemCircuits( CircuitNum ).LoopSideNum, PipingSystemCircuits( CircuitNum ).BranchNum, PipingSystemCircuits( CircuitNum ).CompNum );

	}

	void
	UpdatePipingSystems(
		int const DomainNum,
		int const CircuitNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Using/Aliasing
		using DataLoopNode::Node;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OutletNodeNum;

		OutletNodeNum = PipingSystemCircuits( CircuitNum ).OutletNodeNum;
		auto const & out_cell( PipingSystemCircuits( CircuitNum ).CircuitOutletCell );
		Node( OutletNodeNum ).Temp = PipingSystemDomains( DomainNum ).Cells( out_cell.X, out_cell.Y, out_cell.Z ).PipeCellData.Fluid.MyBase.Temperature;

	}

	void
	IssueSevereInputFieldError(
		std::string const & RoutineName,
		std::string const & ObjectName,
		std::string const & InstanceName,
		std::string const & FieldName,
		std::string const & FieldEntry,
		std::string const & Condition,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		ShowSevereError( RoutineName + ':' + ObjectName + "=\"" + InstanceName + "\", invalid " + FieldName + "=\"" + FieldEntry + "\", Condition: " + Condition );
		ErrorsFound = true;

	}

	void
	IssueSevereInputFieldError(
		std::string const & RoutineName,
		std::string const & ObjectName,
		std::string const & InstanceName,
		std::string const & FieldName,
		Real64 const FieldEntry,
		std::string const & Condition,
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		ShowSevereError( RoutineName + ':' + ObjectName + "=\"" + InstanceName + "\", invalid " + FieldName + "=\"" + General::TrimSigDigits( FieldEntry, 3 ) + "\", Condition: " + Condition );
		ErrorsFound = true;

	}

	int
	GetSurfaceCountForOSCM( int const OSCMIndex )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		int RetVal;
		RetVal = 0;
		for ( int SurfCtr = 1; SurfCtr <= isize( DataSurfaces::Surface ); ++SurfCtr ) {
			if ( DataSurfaces::Surface( SurfCtr ).OSCMPtr == OSCMIndex ) ++RetVal;
		}
		return RetVal;

	}

	Array1D_int
	GetSurfaceIndecesForOSCM(
		int const OSCMIndex,
		int const SurfCount
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		Array1D_int RetVal( {1,SurfCount} );
		int FoundSurfIndexCtr( 0 );
		for ( int SurfCtr = 1; SurfCtr <= isize( DataSurfaces::Surface ); ++SurfCtr ) {
			if ( DataSurfaces::Surface( SurfCtr ).OSCMPtr == OSCMIndex ) {
				++FoundSurfIndexCtr;
				RetVal( FoundSurfIndexCtr ) = SurfCtr;
			}
		}
		return RetVal;

	}

	Array1D <ZoneCoupledSurfaceData>
		GetSurfaceDataForOSCM(
		int const OSCMIndex,
		int const SurfCount
		)
	{

			// FUNCTION INFORMATION:
			//       AUTHOR         Edwin Lee
			//       DATE WRITTEN   Summer 2011
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			Array1D <ZoneCoupledSurfaceData> RetVal( { 1, SurfCount } );
			int FoundSurfIndexCtr( 0 );
			for ( int SurfCtr = 1; SurfCtr <= isize( DataSurfaces::Surface ); ++SurfCtr ) {
				if ( DataSurfaces::Surface( SurfCtr ).OSCMPtr == OSCMIndex ) {
					++FoundSurfIndexCtr;
					RetVal( FoundSurfIndexCtr ).IndexInSurfaceArray = SurfCtr;
					RetVal( FoundSurfIndexCtr ).SurfaceArea = DataSurfaces::Surface( SurfCtr ).Area;
					RetVal( FoundSurfIndexCtr ).Zone = DataSurfaces::Surface( SurfCtr ).Zone;
				}
			}
			return RetVal;

		}

	bool
	CellType_IsFieldCell( CellType const cellType )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		return ( ( cellType == CellType::GeneralField ) || ( cellType == CellType::BasementCorner ) || ( cellType == CellType::BasementWall ) || ( cellType == CellType::BasementFloor ) );
	}

	bool
	MeshPartitionArray_Contains(
		Array1D< MeshPartition > const & meshes,
		Real64 const value
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// METHODOLOGY EMPLOYED:
		// <description>

		// Return value

		// Argument array dimensioning

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		for ( int meshnum = meshes.l1(), meshnum_end = meshes.u1(); meshnum <= meshnum_end; ++meshnum ) {
			if ( meshes( meshnum ).rDimension == value ) return true;
		}

		return false;

	}

	void
	MeshPartition_SelectionSort( Array1< MeshPartition > & X )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE OF THIS FUNCTION:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		using std::swap;

		for ( int I = X.l1(), I_end = X.u1() - 1; I <= I_end; ++I ) {
			int loc( 1 ), l( 1 );
			Real64 r_min( std::numeric_limits< Real64 >::max() );
			for ( int j = I, j_end = X.u1(); j <= j_end; ++j, ++l ) {
				if ( X( j ).rDimension < r_min ) {
					r_min = X( j ).rDimension;
					loc = l;
				}
			}
			int const ISWAP1( loc + I - 1 );
			if ( ISWAP1 != I ) swap( X( I ), X( ISWAP1 ) );
		}

	}

	void
	PipeSegmentInfo::initPipeCells(
		int const x,
		int const y
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		Point TempPt;
		TempPt.X = x;
		TempPt.Y = y;

		this->PipeCellCoordinates = TempPt;
		this->PipeCellCoordinatesSet = true;

	}

	void
	PipeCircuitInfo::initInOutCells(
		CartesianCell const & in,
		CartesianCell const & out
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		this->CircuitInletCell = Point3DInteger( in.X_index, in.Y_index, in.Z_index );
		this->CircuitOutletCell = Point3DInteger( out.X_index, out.Y_index, out.Z_index );

	}

	bool
	IsConverged_CurrentToPrevIteration( int const DomainNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Real64 LocalMax( 0.0 );
		auto const & cells( PipingSystemDomains( DomainNum ).Cells );
		for ( int X = cells.l1(), X_end = cells.u1(); X <= X_end; ++X ) {
			for ( int Y = cells.l2(), Y_end = cells.u2(); Y <= Y_end; ++Y ) {
				for ( int Z = cells.l3(), Z_end = cells.u3(); Z <= Z_end; ++Z ) {
					LocalMax = max( LocalMax, std::abs( cells( X, Y, Z ).MyBase.Temperature - cells( X, Y, Z ).MyBase.Temperature_PrevIteration ) );
				}
			}
		}

		return ( LocalMax < PipingSystemDomains( DomainNum ).SimControls.Convergence_CurrentToPrevIteration );
	}

	bool
	IsConverged_PipeCurrentToPrevIteration(
		int const CircuitNum,
		CartesianCell const & CellToCheck,
		Real64 & MaxDivAmount
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// METHODOLOGY EMPLOYED:
		// <description>

		// Return value
		bool RetVal;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int RadialCtr;
		Real64 ThisCellMax;

		// Object Data
		RadialCellInformation radCell;

		MaxDivAmount = 0.0;
		for ( RadialCtr = CellToCheck.PipeCellData.Soil.l1(); RadialCtr <= CellToCheck.PipeCellData.Soil.u1(); ++RadialCtr ) {
			radCell = CellToCheck.PipeCellData.Soil( RadialCtr );
			ThisCellMax = std::abs( radCell.MyBase.Temperature - radCell.MyBase.Temperature_PrevIteration );
			if ( ThisCellMax > MaxDivAmount ) {
				MaxDivAmount = ThisCellMax;
			}
		}
		//'also do the pipe cell
		ThisCellMax = std::abs( CellToCheck.PipeCellData.Pipe.MyBase.Temperature - CellToCheck.PipeCellData.Pipe.MyBase.Temperature_PrevIteration );
		if ( ThisCellMax > MaxDivAmount ) {
			MaxDivAmount = ThisCellMax;
		}
		//'also do the water cell
		ThisCellMax = std::abs( CellToCheck.PipeCellData.Fluid.MyBase.Temperature - CellToCheck.PipeCellData.Fluid.MyBase.Temperature_PrevIteration );
		if ( ThisCellMax > MaxDivAmount ) {
			MaxDivAmount = ThisCellMax;
		}
		//'also do insulation if it exists
		if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
			ThisCellMax = std::abs( CellToCheck.PipeCellData.Insulation.MyBase.Temperature - CellToCheck.PipeCellData.Insulation.MyBase.Temperature_PrevIteration );
			if ( ThisCellMax > MaxDivAmount ) {
				MaxDivAmount = ThisCellMax;
			}
		}

		RetVal = ( MaxDivAmount < PipingSystemCircuits( CircuitNum ).Convergence_CurrentToPrevIteration );

		return RetVal;

	}

	void
	ShiftTemperaturesForNewTimeStep( int const DomainNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// METHODOLOGY EMPLOYED:
		// <description>

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		auto & cells( PipingSystemDomains( DomainNum ).Cells );
		for ( int X = cells.l1(), X_end = cells.u1(); X <= X_end; ++X ) {
			for ( int Y = cells.l2(), Y_end = cells.u2(); Y <= Y_end; ++Y ) {
				for ( int Z = cells.l3(), Z_end = cells.u3(); Z <= Z_end; ++Z ) {
					auto & cell( cells( X, Y, Z ) );

					cell.MyBase.Temperature_PrevTimeStep = cell.MyBase.Temperature;

					if ( cell.cellType == CellType::Pipe ) {

						for ( int RadCtr = cell.PipeCellData.Soil.l1(); RadCtr <= cell.PipeCellData.Soil.u1(); ++RadCtr ) {

							cell.PipeCellData.Soil( RadCtr ).MyBase.Temperature_PrevTimeStep = cell.PipeCellData.Soil( RadCtr ).MyBase.Temperature;

						}

						cell.PipeCellData.Fluid.MyBase.Temperature_PrevTimeStep = cell.PipeCellData.Fluid.MyBase.Temperature;

						cell.PipeCellData.Pipe.MyBase.Temperature_PrevTimeStep = cell.PipeCellData.Pipe.MyBase.Temperature;

						cell.PipeCellData.Insulation.MyBase.Temperature_PrevTimeStep = cell.PipeCellData.Insulation.MyBase.Temperature;

					}

				}
			}
		}

	}

	void
	ShiftTemperaturesForNewIteration( int const DomainNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// METHODOLOGY EMPLOYED:
		// <description>

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		auto & cells( PipingSystemDomains( DomainNum ).Cells );
		for ( int X = cells.l1(), X_end = cells.u1(); X <= X_end; ++X ) {
			for ( int Y = cells.l2(), Y_end = cells.u2(); Y <= Y_end; ++Y ) {
				for ( int Z = cells.l3(), Z_end = cells.u3(); Z <= Z_end; ++Z ) {
					auto & cell( cells( X, Y, Z ) );

					cell.MyBase.Temperature_PrevIteration = cell.MyBase.Temperature;

					if ( cell.cellType == CellType::Pipe ) {

						for ( int RadCtr = cell.PipeCellData.Soil.l1(); RadCtr <= cell.PipeCellData.Soil.u1(); ++RadCtr ) {

							cell.PipeCellData.Soil( RadCtr ).MyBase.Temperature_PrevIteration = cell.PipeCellData.Soil( RadCtr ).MyBase.Temperature;

						}

						cell.PipeCellData.Fluid.MyBase.Temperature_PrevIteration = cell.PipeCellData.Fluid.MyBase.Temperature;

						cell.PipeCellData.Pipe.MyBase.Temperature_PrevIteration = cell.PipeCellData.Pipe.MyBase.Temperature;

						cell.PipeCellData.Insulation.MyBase.Temperature_PrevIteration = cell.PipeCellData.Insulation.MyBase.Temperature;

					}

				}
			}
		}

	}

	void
	ShiftPipeTemperaturesForNewIteration( CartesianCell & ThisPipeCell )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// METHODOLOGY EMPLOYED:
		// <description>

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int RadCtr;

		if ( ThisPipeCell.cellType == CellType::Pipe ) { // It better be!

			for ( RadCtr = ThisPipeCell.PipeCellData.Soil.l1(); RadCtr <= ThisPipeCell.PipeCellData.Soil.u1(); ++RadCtr ) {
				ThisPipeCell.PipeCellData.Soil( RadCtr ).MyBase.Temperature_PrevIteration = ThisPipeCell.PipeCellData.Soil( RadCtr ).MyBase.Temperature;
			}

			ThisPipeCell.PipeCellData.Fluid.MyBase.Temperature_PrevIteration = ThisPipeCell.PipeCellData.Fluid.MyBase.Temperature;

			ThisPipeCell.PipeCellData.Pipe.MyBase.Temperature_PrevIteration = ThisPipeCell.PipeCellData.Pipe.MyBase.Temperature;

			ThisPipeCell.PipeCellData.Insulation.MyBase.Temperature_PrevIteration = ThisPipeCell.PipeCellData.Insulation.MyBase.Temperature;

		}

	}

	bool
	CheckForOutOfRangeTemps( int const DomainNum )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// METHODOLOGY EMPLOYED:
		// <description>

		Real64 const MaxLimit = PipingSystemDomains( DomainNum ).SimControls.MaximumTemperatureLimit;
		Real64 const MinLimit = PipingSystemDomains( DomainNum ).SimControls.MinimumTemperatureLimit;

		auto const & Cells( PipingSystemDomains( DomainNum ).Cells );
		for ( std::size_t i = 0, e = Cells.size(); i < e; ++i ) {
			double const Temperature( Cells[ i ].MyBase.Temperature );
			if ( ( Temperature > MaxLimit ) || ( Temperature < MinLimit ) )  return true;
		}
		return false;
	}

	Real64
	CartesianCell::normalArea(
		Direction const direction
	) const
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		if ( ( direction == Direction::PositiveY ) || ( direction == Direction::NegativeY ) ) {
			return this->YNormalArea();
		} else if ( ( direction == Direction::PositiveX ) || ( direction == Direction::NegativeX ) ) {
			return this->XNormalArea();
		} else if ( ( direction == Direction::PositiveZ ) || ( direction == Direction::NegativeZ ) ) {
			return this->ZNormalArea();
		} else {
			throw "Invalid direction passed to PlantPipingSystemsManager::CartesianCell::normalArea";
		}

	}

	NeighborInformation
	NeighborInformationArray_Value(
		Array1D< DirectionNeighbor_Dictionary > const & dict,
		Direction const direction
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Return value
		NeighborInformation RetVal;

		for ( int Index = dict.l1(), Index_end = dict.u1(); Index <= Index_end; ++Index ) {
			if ( dict( Index ).direction == direction ) {
				RetVal = dict( Index ).Value;
				break;
			}
		}
		//Autodesk:Return Check/enforce that return value is set

		return RetVal;

	}

	void
	CartesianPipeCellInformation::ctor(
		CartesianPipeCellInformation & c,
		Real64 const GridCellWidth,
		RadialSizing const & PipeSizes,
		int const NumRadialNodes,
		Real64 const CellDepth,
		Real64 const InsulationThickness,
		Real64 const RadialGridExtent,
		bool const SimHasInsulation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		using DataGlobals::Pi;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 InsulationInnerRadius;
		Real64 InsulationOuterRadius;
		Real64 InsulationCentroid;
		Real64 PipeOuterRadius;
		Real64 PipeInnerRadius;
		Real64 MinimumSoilRadius;
		Real64 ThisSliceInnerRadius;
		Real64 Rval;
		int RadialCellCtr;

		//'calculate pipe radius
		PipeOuterRadius = PipeSizes.OuterDia / 2.0;
		PipeInnerRadius = PipeSizes.InnerDia / 2.0;

		//'--we will work from inside out, calculating dimensions and instantiating variables--
		//'first instantiate the water cell
		FluidCellInformation::ctor( c.Fluid, PipeInnerRadius, CellDepth );

		//'then the pipe cell
		RadialCellInformation::ctor( c.Pipe, ( PipeOuterRadius + PipeInnerRadius ) / 2.0, PipeInnerRadius, PipeOuterRadius );

		//'then the insulation if we have it
		if ( InsulationThickness > 0.0 ) {
			InsulationInnerRadius = PipeOuterRadius;
			InsulationOuterRadius = InsulationInnerRadius + InsulationThickness;
			InsulationCentroid = ( InsulationInnerRadius + InsulationOuterRadius ) / 2.0;
			RadialCellInformation::ctor( c.Insulation, InsulationCentroid, InsulationInnerRadius, InsulationOuterRadius );
		}

		//'determine where to start applying the radial soil cells based on whether we have insulation or not
		if ( ! SimHasInsulation ) {
			MinimumSoilRadius = PipeOuterRadius;
		} else {
			MinimumSoilRadius = c.Insulation.OuterRadius;
		}

		//'the radial cells are distributed evenly throughout this region
		c.RadialSliceWidth = RadialGridExtent / NumRadialNodes;

		// allocate the array of radial soil nodes
		c.Soil.allocate( {0,NumRadialNodes - 1} );

		// first set Rval to the minimum soil radius plus half a slice thickness for the innermost radial node
		Rval = MinimumSoilRadius + ( c.RadialSliceWidth / 2.0 );
		ThisSliceInnerRadius = MinimumSoilRadius;
		RadialCellInformation::ctor( c.Soil( 0 ), Rval, ThisSliceInnerRadius, ThisSliceInnerRadius + c.RadialSliceWidth );

		//'then loop through the rest and assign them, each radius is simply one more slice thickness
		for ( RadialCellCtr = 1; RadialCellCtr <= c.Soil.u1(); ++RadialCellCtr ) {
			Rval += c.RadialSliceWidth;
			ThisSliceInnerRadius += c.RadialSliceWidth;
			RadialCellInformation::ctor( c.Soil( RadialCellCtr ), Rval, ThisSliceInnerRadius, ThisSliceInnerRadius + c.RadialSliceWidth );
		}

		//'also assign the interface cell surrounding the radial system
		c.InterfaceVolume = ( 1.0 - ( Pi / 4.0 ) ) * pow_2( GridCellWidth ) * CellDepth;

	}

	void
	RadialCellInformation::ctor(
		RadialCellInformation & c,
		Real64 const m_RadialCentroid,
		Real64 const m_MinRadius,
		Real64 const m_MaxRadius
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		c.RadialCentroid = m_RadialCentroid;
		c.InnerRadius = m_MinRadius;
		c.OuterRadius = m_MaxRadius;

	}

	void
	FluidCellInformation::ctor(
		FluidCellInformation & c,
		Real64 const m_PipeInnerRadius,
		Real64 const m_CellDepth
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		c.PipeInnerRadius = m_PipeInnerRadius;
		c.Volume = DataGlobals::Pi * pow_2( m_PipeInnerRadius ) * m_CellDepth;

	}

	void
	FullDomainStructureInfo::developMesh()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D< Real64 > XBoundaryPoints;
		Array1D< Real64 > YBoundaryPoints;
		Array1D< Real64 > ZBoundaryPoints;
		int RegionListCount;
		int BoundaryListCount;
		bool XPartitionsExist;
		bool YPartitionsExist;
		bool ZPartitionsExist;

		// Object Data
		Array1D< GridRegion > XPartitionRegions;
		Array1D< GridRegion > YPartitionRegions;
		Array1D< GridRegion > ZPartitionRegions;
		Array1D< GridRegion > XRegions;
		Array1D< GridRegion > YRegions;
		Array1D< GridRegion > ZRegions;

		//'****** LAYOUT PARTITIONS ******'
		this->createPartitionCenterList();

		if ( allocated( this->Partitions.X ) ) {
			XPartitionRegions.allocate( {0,this->Partitions.X.u1()} );
			XPartitionsExist = true;
		} else {
			XPartitionRegions.allocate( {0,0} );
			this->Partitions.X.allocate( {0,0} );
			XPartitionsExist = false;
		}

		XPartitionRegions = this->createPartitionRegionList( this->Partitions.X, XPartitionsExist, this->Extents.Xmax, this->Partitions.X.u1() );

		if ( allocated( this->Partitions.Y ) ) {
			YPartitionRegions.allocate( {0,this->Partitions.Y.u1()} );
			YPartitionsExist = true;
		} else {
			YPartitionRegions.allocate( {0,0} );
			this->Partitions.Y.allocate( {0,0} );
			YPartitionsExist = false;
		}

		YPartitionRegions = this->createPartitionRegionList( this->Partitions.Y, YPartitionsExist, this->Extents.Ymax, this->Partitions.Y.u1() );

		if ( allocated( this->Partitions.Z ) ) {
			ZPartitionRegions.allocate( { 0, this->Partitions.Z.u1() } );
			ZPartitionsExist = true;
		} else {
			ZPartitionRegions.allocate( { 0, 0 } );
			this->Partitions.Z.allocate( { 0, 0 } );
			ZPartitionsExist = false;
		}

		ZPartitionRegions = this->createPartitionRegionList( this->Partitions.Z, ZPartitionsExist, this->Extents.Zmax, this->Partitions.Z.u1() );


		//'***** LAYOUT MESH REGIONS *****'
		// Zone-coupled slab  models
		if ( this->HasZoneCoupledBasement ) {
			RegionListCount = CreateRegionListCount( XPartitionRegions, this->Extents.Xmax, XPartitionsExist );
			XRegions.allocate( { 0, RegionListCount - 1 } );
			XRegions = this->createRegionList( XPartitionRegions, this->Extents.Xmax, RegionType::XDirection, RegionListCount - 1, XPartitionsExist, _, _, this->XIndex, this->XWallIndex, this->InsulationXIndex );

			RegionListCount = CreateRegionListCount( YPartitionRegions, this->Extents.Ymax, YPartitionsExist );
			YRegions.allocate( { 0, RegionListCount - 1 } );
			YRegions = this->createRegionList( YPartitionRegions, this->Extents.Ymax, RegionType::YDirection, RegionListCount - 1, YPartitionsExist, _, _, _, _, _, this->YIndex, this->YFloorIndex, this->InsulationYIndex );

			RegionListCount = CreateRegionListCount( ZPartitionRegions, this->Extents.Zmax, ZPartitionsExist );
			ZRegions.allocate( { 0, RegionListCount - 1 } );
			ZRegions = this->createRegionList( ZPartitionRegions, this->Extents.Zmax, RegionType::ZDirection, RegionListCount - 1, ZPartitionsExist, _, _, _, _, _, _, _, _, this->ZIndex, this->ZWallIndex, this->InsulationZIndex );
		} else if ( this->HasZoneCoupledSlab ) {
			RegionListCount = CreateRegionListCount( XPartitionRegions, this->Extents.Xmax, XPartitionsExist );
			XRegions.allocate( { 0, RegionListCount - 1 } );
			XRegions = this->createRegionList( XPartitionRegions, this->Extents.Xmax, RegionType::XDirection, RegionListCount - 1, XPartitionsExist, _, _, this->XIndex, _, this->InsulationXIndex );

			RegionListCount = CreateRegionListCount( YPartitionRegions, this->Extents.Ymax, YPartitionsExist );
			YRegions.allocate( { 0, RegionListCount - 1 } );
			YRegions = this->createRegionList( YPartitionRegions, this->Extents.Ymax, RegionType::YDirection, RegionListCount - 1, YPartitionsExist, _, _, _, _, _, this->YIndex, _, this->InsulationYIndex );

			RegionListCount = CreateRegionListCount( ZPartitionRegions, this->Extents.Zmax, ZPartitionsExist );
			ZRegions.allocate( { 0, RegionListCount - 1 } );
			ZRegions = this->createRegionList( ZPartitionRegions, this->Extents.Zmax, RegionType::ZDirection, RegionListCount - 1, ZPartitionsExist, _, _, _, _, _, _, _, _, this->ZIndex, _, this->InsulationZIndex );
		} else {
			RegionListCount = CreateRegionListCount( XPartitionRegions, this->Extents.Xmax, XPartitionsExist );
			XRegions.allocate( {0,RegionListCount - 1} );

			XRegions = this->createRegionList( XPartitionRegions, this->Extents.Xmax, RegionType::XDirection, RegionListCount - 1, XPartitionsExist, this->BasementZone.BasementWallXIndex );

			RegionListCount = CreateRegionListCount( YPartitionRegions, this->Extents.Ymax, YPartitionsExist );
			YRegions.allocate( {0,RegionListCount - 1} );
			YRegions = this->createRegionList( YPartitionRegions, this->Extents.Ymax, RegionType::YDirection, RegionListCount - 1, YPartitionsExist, _, this->BasementZone.BasementFloorYIndex );

			RegionListCount = CreateRegionListCount( ZPartitionRegions, this->Extents.Zmax, ZPartitionsExist );
			ZRegions.allocate( {0,RegionListCount - 1} );
			ZRegions = this->createRegionList( ZPartitionRegions, this->Extents.Zmax, RegionType::ZDirection, RegionListCount - 1, ZPartitionsExist );
		}

		//'** MAKE REGIONS > BOUNDARIES **'
		BoundaryListCount = CreateBoundaryListCount( XRegions, RegionType::XDirection );
		XBoundaryPoints.allocate( {0,BoundaryListCount - 1} );
		XBoundaryPoints = CreateBoundaryList( XRegions, this->Extents.Xmax, RegionType::XDirection, 0, BoundaryListCount - 1 );

		BoundaryListCount = CreateBoundaryListCount( YRegions, RegionType::YDirection );
		YBoundaryPoints.allocate( {0,BoundaryListCount - 1} );
		YBoundaryPoints = CreateBoundaryList( YRegions, this->Extents.Ymax, RegionType::YDirection, 0, BoundaryListCount - 1 );

		BoundaryListCount = CreateBoundaryListCount( ZRegions, RegionType::ZDirection );
		ZBoundaryPoints.allocate( {0,BoundaryListCount - 1} );
		ZBoundaryPoints = CreateBoundaryList( ZRegions, this->Extents.Zmax, RegionType::ZDirection, 0, BoundaryListCount - 1 );

		//'****** DEVELOP CELL ARRAY *****'
		this->createCellArray( XBoundaryPoints, YBoundaryPoints, ZBoundaryPoints );

		//'***** SETUP CELL NEIGHBORS ****'
		this->setupCellNeighbors();

		//'** SET UP PIPE CIRCUIT CELLS **'
		this->setupPipeCircuitInOutCells();

		if ( allocated( XPartitionRegions ) ) XPartitionRegions.deallocate();
		if ( allocated( YPartitionRegions ) ) YPartitionRegions.deallocate();
		if ( allocated( ZPartitionRegions ) ) ZPartitionRegions.deallocate();
		if ( allocated( XRegions ) ) XRegions.deallocate();
		if ( allocated( YRegions ) ) YRegions.deallocate();
		if ( allocated( ZRegions ) ) ZRegions.deallocate();
		if ( allocated( XBoundaryPoints ) ) XBoundaryPoints.deallocate();
		if ( allocated( YBoundaryPoints ) ) YBoundaryPoints.deallocate();
		if ( allocated( ZBoundaryPoints ) ) ZBoundaryPoints.deallocate();

	}

	void
	FullDomainStructureInfo::createPartitionCenterList()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Real64 BasementCellFraction( 0.001 ); // the fraction of domain extent to use for the basement cells
		// actual dimension shouldn't matter for calculation purposes

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 BasementDistFromBottom;
		Real64 FloorLocation;
		Real64 UnderFloorLocation;
		int CircuitCtr;
		int CircuitIndex;
		int PipeCtr;
		int PreviousUbound;
		Real64 PipeCellWidth;
		Real64 SurfCellWidth; // Basement surface...
		Real64 SideXLocation;
		Real64 SideXWallLocation;
		Real64 SideXInsulationLocation;
		Real64 SideZLocation;
		Real64 SideZWallLocation;
		Real64 SideZInsulationLocation;
		Real64 SlabDistFromBottom;
		Real64 YInsulationLocation;
		Real64 CellWidth( 0.0 );
		Real64 InterfaceCellWidth( 0.008 );

		// Object Data
		Array1D< MeshPartition > PreviousEntries;
		PipeSegmentInfo ThisSegment;

		//'NOTE: pipe location y values have already been corrected to be measured from the bottom surface
		//'in input they are measured by depth, but internally they are referred to by distance from y = 0, or the bottom boundary
		for ( CircuitCtr = this->CircuitIndeces.l1(); CircuitCtr <= this->CircuitIndeces.u1(); ++CircuitCtr ) {
			CircuitIndex = this->CircuitIndeces( CircuitCtr );

			// set up a convenience variable here
			//'account for the pipe and insulation if necessary
			if ( ! PipingSystemCircuits( CircuitIndex ).HasInsulation ) {
				PipeCellWidth = PipingSystemCircuits( CircuitIndex ).PipeSize.OuterDia;
			} else {
				PipeCellWidth = PipingSystemCircuits( CircuitIndex ).InsulationSize.OuterDia;
			}

			//'then add the radial mesh thickness on both sides of the pipe/insulation construct
			PipeCellWidth += 2 * PipingSystemCircuits( CircuitIndex ).RadialMeshThickness;

			for ( PipeCtr = PipingSystemCircuits( CircuitIndex ).PipeSegmentIndeces.l1(); PipeCtr <= PipingSystemCircuits( CircuitIndex ).PipeSegmentIndeces.u1(); ++PipeCtr ) {

				ThisSegment = PipingSystemSegments( PipingSystemCircuits( CircuitIndex ).PipeSegmentIndeces( PipeCtr ) );
				if ( ! allocated( this->Partitions.X ) ) {
					this->Partitions.X.allocate( {0,0} );
					this->Partitions.X( 0 ) = MeshPartition( ThisSegment.PipeLocation.X, PartitionType::Pipe, PipeCellWidth );
				} else if ( ! MeshPartitionArray_Contains( this->Partitions.X, ThisSegment.PipeLocation.X ) ) {
					PreviousUbound = this->Partitions.X.u1();
					if ( allocated( PreviousEntries ) ) PreviousEntries.deallocate();
					PreviousEntries.allocate( {0,PreviousUbound} );
					PreviousEntries = this->Partitions.X;
					this->Partitions.X.deallocate();
					this->Partitions.X.allocate( {0,PreviousUbound + 1} );
					this->Partitions.X( {0,PreviousUbound} ) = PreviousEntries;
					this->Partitions.X( PreviousUbound + 1 ) = MeshPartition( ThisSegment.PipeLocation.X, PartitionType::Pipe, PipeCellWidth );
				}

				if ( ! allocated( this->Partitions.Y ) ) {
					this->Partitions.Y.allocate( {0,0} );
					this->Partitions.Y( 0 ) = MeshPartition( ThisSegment.PipeLocation.Y, PartitionType::Pipe, PipeCellWidth );
				} else if ( ! MeshPartitionArray_Contains( this->Partitions.Y, ThisSegment.PipeLocation.Y ) ) {
					PreviousUbound = this->Partitions.Y.u1();
					if ( allocated( PreviousEntries ) ) PreviousEntries.deallocate();
					PreviousEntries.allocate( {0,PreviousUbound} );
					PreviousEntries = this->Partitions.Y;
					this->Partitions.Y.deallocate();
					this->Partitions.Y.allocate( {0,PreviousUbound + 1} );
					this->Partitions.Y( {0,PreviousUbound} ) = PreviousEntries;
					this->Partitions.Y( PreviousUbound + 1 ) = MeshPartition( ThisSegment.PipeLocation.Y, PartitionType::Pipe, PipeCellWidth );
				}

			}

		}

		// Underground Piping Systems Ground domain with basement interaction
		if ( !this->HasZoneCoupledBasement ) {
			if ( this->HasBasement ) { // FHX model
				//'NOTE: the basement depth is still a depth from the ground surface, need to correct for this here
				if ( this->BasementZone.Width > 0 ) {
					SurfCellWidth = this->Extents.Xmax * BasementCellFraction;
					if ( !allocated( this->Partitions.X ) ) {
						this->Partitions.X.allocate( { 0, 0 } );
						this->Partitions.X( 0 ) = MeshPartition( this->BasementZone.Width, PartitionType::BasementWall, SurfCellWidth );
					} else if ( !MeshPartitionArray_Contains( this->Partitions.X, this->BasementZone.Width ) ) {
						PreviousUbound = this->Partitions.X.u1();
						if ( allocated( PreviousEntries ) ) PreviousEntries.deallocate();
						PreviousEntries.allocate( { 0, PreviousUbound } );
						PreviousEntries = this->Partitions.X;
						this->Partitions.X.deallocate();
						this->Partitions.X.allocate( { 0, PreviousUbound + 1 } );
						this->Partitions.X( { 0, PreviousUbound } ) = PreviousEntries;
						this->Partitions.X( PreviousUbound + 1 ) = MeshPartition( this->BasementZone.Width, PartitionType::BasementWall, SurfCellWidth );
					}
				}

				if ( this->BasementZone.Depth > 0 ) {
					SurfCellWidth = this->Extents.Ymax * BasementCellFraction;
					BasementDistFromBottom = this->Extents.Ymax - this->BasementZone.Depth;
					if ( !allocated( this->Partitions.Y ) ) {
						this->Partitions.Y.allocate( { 0, 0 } );
						this->Partitions.Y( 0 ) = MeshPartition( BasementDistFromBottom, PartitionType::BasementFloor, SurfCellWidth );
					} else if ( !MeshPartitionArray_Contains( this->Partitions.Y, BasementDistFromBottom ) ) {
						PreviousUbound = this->Partitions.Y.u1();
						if ( allocated( PreviousEntries ) ) PreviousEntries.deallocate();
						PreviousEntries.allocate( { 0, PreviousUbound } );
						PreviousEntries = this->Partitions.Y;
						this->Partitions.Y.deallocate();
						this->Partitions.Y.allocate( { 0, PreviousUbound + 1 } );
						this->Partitions.Y( { 0, PreviousUbound } ) = PreviousEntries;
						this->Partitions.Y( PreviousUbound + 1 ) = MeshPartition( BasementDistFromBottom, PartitionType::BasementFloor, SurfCellWidth );
					}
				}
			}
		} else { // Zone-coupled basement model
			//'NOTE: the basement depth is still a depth from the ground surface, need to correct for this here
			if ( this->BasementZone.Width > 0 ) {
				// Create partitions at basement walls and horizontal insulation edges
				CellWidth = this->VertInsThickness;
				// Side X direction - Insulation layer
				SideXLocation = this->PerimeterOffset - InterfaceCellWidth - CellWidth / 2.0;
				// Side X direction - Basement Wall Interface
				SideXWallLocation = this->PerimeterOffset - InterfaceCellWidth / 2.0;
				if ( this->HorizInsPresentFlag && !this->FullHorizInsPresent ) {
					// Insulation Edge in X direction
					SideXInsulationLocation = this->PerimeterOffset + this->HorizInsWidth + InterfaceCellWidth / 2.0;
				}
				if ( !allocated( this->Partitions.X ) ) {
					if ( this->HorizInsPresentFlag ) {
						if ( !this->FullHorizInsPresent ) {
							this->Partitions.X.allocate( { 0, 2 } );
							// Side X direction - Insulation layer
							this->Partitions.X( 0 ) = MeshPartition( SideXLocation, PartitionType::XSide, CellWidth );
							// Side X direction - Basement Wall Interface
							this->Partitions.X( 1 ) = MeshPartition( SideXWallLocation, PartitionType::XSideWall, InterfaceCellWidth );
							// Insulation Edge X direction
							this->Partitions.X( 2 ) = MeshPartition( SideXInsulationLocation, PartitionType::HorizInsXSide, InterfaceCellWidth );

						} else {
							this->Partitions.X.allocate( { 0, 1 } );
							// Side X direction - Insulation layer
							this->Partitions.X( 0 ) = MeshPartition( SideXLocation, PartitionType::XSide, CellWidth );
							// Side X direction - Basement Wall Interface
							this->Partitions.X( 1 ) = MeshPartition( SideXWallLocation, PartitionType::XSideWall, InterfaceCellWidth );
						}
					} else {
						this->Partitions.X.allocate( { 0, 1 } );
						// Side X direction - Insulation layer
						this->Partitions.X( 0 ) = MeshPartition( SideXLocation, PartitionType::XSide, CellWidth );
						// Side X direction - Basement Wall interface
						this->Partitions.X( 1 ) = MeshPartition( SideXWallLocation, PartitionType::XSideWall, InterfaceCellWidth );
					}
				} else if ( !MeshPartitionArray_Contains( this->Partitions.X, this->BasementZone.Width ) ) {
					PreviousUbound = this->Partitions.X.u1();
					if ( allocated( PreviousEntries ) ) PreviousEntries.deallocate();
					PreviousEntries.allocate( { 0, PreviousUbound } );
					PreviousEntries = this->Partitions.X;
					this->Partitions.X.deallocate();
					// Partition at insulation edges in the X direction, if horizontal insulation present
					if ( this->HorizInsPresentFlag ) {
						if ( !this->FullHorizInsPresent ) {
							this->Partitions.X.allocate( { 0, PreviousUbound + 3 } );
							this->Partitions.X( { 0, PreviousUbound } ) = PreviousEntries;
							// Side X direction - Insulation layer
							this->Partitions.X( PreviousUbound + 1 ) = MeshPartition( SideXLocation, PartitionType::XSide, CellWidth );
							// Side X direction - Basement Wall interface
							this->Partitions.X( PreviousUbound + 2 ) = MeshPartition( SideXWallLocation, PartitionType::XSideWall, InterfaceCellWidth );
							// Insulation Edge X direction
							this->Partitions.X( PreviousUbound + 3 ) = MeshPartition( SideXInsulationLocation, PartitionType::HorizInsXSide, InterfaceCellWidth );
						} else {
							this->Partitions.X.allocate( { 0, PreviousUbound + 2 } );
							this->Partitions.X( { 0, PreviousUbound } ) = PreviousEntries;
							// Side X direction - Insulation layer
							this->Partitions.X( PreviousUbound + 1 ) = MeshPartition( SideXLocation, PartitionType::XSide, CellWidth );
							// Side X direction -Basement Wall interface
							this->Partitions.X( PreviousUbound + 2 ) = MeshPartition( SideXWallLocation, PartitionType::XSideWall, InterfaceCellWidth );
						}
					} else {
						this->Partitions.X.allocate( { 0, PreviousUbound + 2 } );
						this->Partitions.X( { 0, PreviousUbound } ) = PreviousEntries;
						// Side X direction - Insulation layer
						this->Partitions.X( PreviousUbound + 1 ) = MeshPartition( SideXLocation, PartitionType::XSide, CellWidth );
						// Side X direction - Basement Wall interface
						this->Partitions.X( PreviousUbound + 2 ) = MeshPartition( SideXWallLocation, PartitionType::XSideWall, InterfaceCellWidth );
					}
				}
			}
			// Zone coupled basement model
			if ( this->BasementZone.Depth > 0 ) {
				CellWidth = this->HorizInsThickness;
				// Distance of basement floor interface from domain bottom
				FloorLocation = this->Extents.Ymax - this->BasementZone.Depth - InterfaceCellWidth / 2.0;
				// Distance of basement floor insulation layer from domain bottom
				UnderFloorLocation = this->Extents.Ymax - this->BasementZone.Depth -InterfaceCellWidth- CellWidth / 2.0;
				if ( this->VertInsPresentFlag ) {
					YInsulationLocation = this->Extents.Ymax - this->VertInsDepth - InterfaceCellWidth / 2.0;
				}
				if ( !allocated( this->Partitions.Y ) ) {
					// Partition at bottom edge of vertical insulation, if vertical insulation is present. Must be careful not to have floor and vertical insulation-edge partitions overlap.
					if ( this->VertInsPresentFlag && YInsulationLocation > FloorLocation + CellWidth ) {
						this->Partitions.Y.allocate( { 0, 2 } );
						// Partition at basement floor interface
						this->Partitions.Y( 0 ) = MeshPartition( FloorLocation, PartitionType::FloorInside, InterfaceCellWidth );
						// Partition under the basement floor for insulation layer
						this->Partitions.Y( 1 ) = MeshPartition( UnderFloorLocation, PartitionType::UnderFloor, CellWidth );
						// Vertical-Insulation edge partition
						this->Partitions.Y( 2 ) = MeshPartition( YInsulationLocation, PartitionType::VertInsLowerEdge, InterfaceCellWidth );
					} else {
						this->Partitions.Y.allocate( { 0, 1 } );
						this->Partitions.Y( 0 ) = MeshPartition( FloorLocation, PartitionType::FloorInside, InterfaceCellWidth );
						this->Partitions.Y( 1 ) = MeshPartition( UnderFloorLocation, PartitionType::UnderFloor, CellWidth );
					}
				} else if ( !MeshPartitionArray_Contains( this->Partitions.Y, FloorLocation ) ) {
					PreviousUbound = this->Partitions.Y.u1();
					if ( allocated( PreviousEntries ) ) PreviousEntries.deallocate();
					PreviousEntries.allocate( {0,PreviousUbound} );
					PreviousEntries = this->Partitions.Y;
					this->Partitions.Y.deallocate();
					// Partition at bottom edge of vertical insulation, if vertical insulation is present
					if ( this->VertInsPresentFlag && YInsulationLocation > FloorLocation + CellWidth ) {
						this->Partitions.Y.allocate( { 0, PreviousUbound + 3 } );
						this->Partitions.Y( { 0, PreviousUbound } ) = PreviousEntries;
						// Partition at basement floor interface
						this->Partitions.Y( PreviousUbound + 1 ) = MeshPartition( FloorLocation, PartitionType::FloorInside, InterfaceCellWidth );
						// Partition under the basement floor for insulation layer
						this->Partitions.Y( PreviousUbound + 2 ) = MeshPartition( UnderFloorLocation, PartitionType::UnderFloor, CellWidth );
						// Vertical-Insulation edge partition
						this->Partitions.Y( PreviousUbound + 3 ) = MeshPartition( YInsulationLocation, PartitionType::VertInsLowerEdge, InterfaceCellWidth );
					} else {
						this->Partitions.Y.allocate( { 0, PreviousUbound + 2 } );
						this->Partitions.Y( { 0, PreviousUbound } ) = PreviousEntries;
						this->Partitions.Y( PreviousUbound + 1 ) = MeshPartition( FloorLocation, PartitionType::FloorInside, InterfaceCellWidth );
						this->Partitions.Y( PreviousUbound + 2 ) = MeshPartition( UnderFloorLocation, PartitionType::UnderFloor, CellWidth );
					}
				}
			}
			if ( this->BasementZone.Width > 0 ) {
				// Create partitions at basement walls and horizontal insulation edges
				CellWidth = this->VertInsThickness;
				// Side Z direction - Insulation layer
				SideZLocation = this->PerimeterOffset - InterfaceCellWidth - CellWidth / 2.0;
				// Side Z direction - Basement Wall Interface
				SideZWallLocation = this->PerimeterOffset - InterfaceCellWidth / 2.0;
				if ( this->HorizInsPresentFlag && !this->FullHorizInsPresent ) {
					// Insulation Edge Z direction
					SideZInsulationLocation = this->PerimeterOffset + this->HorizInsWidth + InterfaceCellWidth / 2.0;
				}
				if ( !allocated( this->Partitions.Z ) ) {
					if ( this->HorizInsPresentFlag ) {
						if ( !this->FullHorizInsPresent ) {
							this->Partitions.Z.allocate( { 0, 2 } );
							// Side Z direction - Insulation layer
							this->Partitions.Z( 0 ) = MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth );
							// Side Z direction - Basement Wall Interface
							this->Partitions.Z( 1 ) = MeshPartition( SideZWallLocation, PartitionType::ZSideWall, InterfaceCellWidth );
							// Insulation Edge Z direction
							this->Partitions.Z( 2 ) = MeshPartition( SideZInsulationLocation, PartitionType::HorizInsZSide, InterfaceCellWidth );

						} else {
							this->Partitions.Z.allocate( { 0, 1 } );
							// Side Z direction - Insulation layer
							this->Partitions.Z( 0 ) = MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth );
							// Side Z direction - Basement Wall Interface
							this->Partitions.Z( 1 ) = MeshPartition( SideZWallLocation, PartitionType::ZSideWall, InterfaceCellWidth );
						}
					} else {
						this->Partitions.Z.allocate( { 0, 1 } );
						// Side Z direction - Insulation layer
						this->Partitions.Z( 0 ) = MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth );
						// Side Z direction -Basement Wall interface
						this->Partitions.Z( 1 ) = MeshPartition( SideZWallLocation, PartitionType::ZSideWall, InterfaceCellWidth );
					}
				} else if ( !MeshPartitionArray_Contains( this->Partitions.Z, this->BasementZone.Width ) ) {
					PreviousUbound = this->Partitions.Z.u1();
					if ( allocated( PreviousEntries ) ) PreviousEntries.deallocate();
					PreviousEntries.allocate( { 0, PreviousUbound } );
					PreviousEntries = this->Partitions.Z;
					this->Partitions.Z.deallocate();
					// Partition at insulation edges in the Z direction, if horizontal insulation present
					if ( this->HorizInsPresentFlag ) {
						if ( !this->FullHorizInsPresent ) {
							this->Partitions.Z.allocate( { 0, PreviousUbound + 3 } );
							this->Partitions.Z( { 0, PreviousUbound } ) = PreviousEntries;
							// Side Z direction - Insulation layer
							this->Partitions.Z( PreviousUbound + 1 ) = MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth );
							// Side Z direction - Basement Wall interface
							this->Partitions.Z( PreviousUbound + 2 ) = MeshPartition( SideZWallLocation, PartitionType::ZSideWall, InterfaceCellWidth );
							// Insulation Edge Z direction
							this->Partitions.Z( PreviousUbound + 3 ) = MeshPartition( SideZInsulationLocation, PartitionType::HorizInsZSide, InterfaceCellWidth );
						} else {
							this->Partitions.Z.allocate( { 0, PreviousUbound + 2 } );
							this->Partitions.Z( { 0, PreviousUbound } ) = PreviousEntries;
							// Side Z direction - Insulation layer
							this->Partitions.Z( PreviousUbound + 1 ) = MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth );
							// Side Z direction -Basement Wall interface
							this->Partitions.Z( PreviousUbound + 2 ) = MeshPartition( SideZWallLocation, PartitionType::ZSideWall, InterfaceCellWidth );
						}
					} else {
						this->Partitions.Z.allocate( { 0, PreviousUbound + 2 } );
						this->Partitions.Z( { 0, PreviousUbound } ) = PreviousEntries;
						// Side Z direction - Insulation layer
						this->Partitions.Z( PreviousUbound + 1 ) = MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth );
						// Side Z direction -Basement Wall interface
						this->Partitions.Z( PreviousUbound + 2 ) = MeshPartition( SideZWallLocation, PartitionType::ZSideWall, InterfaceCellWidth );
					}
				}
			}
		}

		// Zone-coupled slab
		if ( this->HasZoneCoupledSlab ) {
			// NOTE: the slab depth is still a depth from the ground surface, need to correct for this here.
			// Create partition at slab edges in the X direction
			if ( this->SlabWidth > 0 ) {
				CellWidth = this->VertInsThickness;
				// Side X direction
				SideXLocation = this->PerimeterOffset - CellWidth / 2.0;
				// Insulation Edge X direction
				if ( this->HorizInsPresentFlag && !this->FullHorizInsPresent ) {
					SideXInsulationLocation = SideXLocation + this->HorizInsWidth;
				}
				if ( !allocated( this->Partitions.X ) ) {
					// Partition at insulation edges in the X direction, if horizontal insulation present
					if ( this->HorizInsPresentFlag ) {
						if ( !this->FullHorizInsPresent ) {
							this->Partitions.X.allocate( { 0, 1 } );
							// Side X direction
							this->Partitions.X( 0 ) = MeshPartition( SideXLocation, PartitionType::XSide, CellWidth );
							// Insulation Edge X direction
							this->Partitions.X( 1 ) = MeshPartition( SideXInsulationLocation, PartitionType::HorizInsXSide, CellWidth );
						} else {
							this->Partitions.X.allocate( { 0, 0 } );
							// Side X direction
							this->Partitions.X( 0 ) = MeshPartition( SideXLocation, PartitionType::XSide, CellWidth );
						}
					} else {
						this->Partitions.X.allocate( { 0, 0 } );
						// Side X direction
						this->Partitions.X( 0 ) = MeshPartition( SideXLocation, PartitionType::XSide, CellWidth );
					}
				} else if ( !MeshPartitionArray_Contains( this->Partitions.X, this->SlabWidth ) ) {
					PreviousUbound = this->Partitions.X.u1();
					if ( allocated( PreviousEntries ) ) PreviousEntries.deallocate();
					PreviousEntries.allocate( { 0, PreviousUbound } );
					PreviousEntries = this->Partitions.X;
					this->Partitions.X.deallocate();

					// Partition at insulation edges in the X direction, if horizontal insulation present
					if ( this->HorizInsPresentFlag ) {
						if ( !this->FullHorizInsPresent ) {
							this->Partitions.X.allocate( { 0, PreviousUbound + 4 } );
							this->Partitions.X( { 0, PreviousUbound } ) = PreviousEntries;
							// Side X direction
							this->Partitions.X( PreviousUbound + 1 ) = MeshPartition( SideXLocation, PartitionType::XSide, CellWidth );
							// Insulation Edge X direction
							this->Partitions.X( PreviousUbound + 2 ) = MeshPartition( SideXInsulationLocation, PartitionType::HorizInsXSide, CellWidth );
						} else {
							this->Partitions.X.allocate( { 0, PreviousUbound + 1 } );
							this->Partitions.X( { 0, PreviousUbound } ) = PreviousEntries;
							// Side X direction
							this->Partitions.X( PreviousUbound + 1 ) = MeshPartition( SideXLocation, PartitionType::XSide, CellWidth );
						}
					} else {
						this->Partitions.X.allocate( { 0, PreviousUbound + 1 } );
						this->Partitions.X( { 0, PreviousUbound } ) = PreviousEntries;
						// Side X direction
						this->Partitions.X( PreviousUbound + 1 ) = MeshPartition( SideXLocation, PartitionType::XSide, CellWidth );
					}
				}
			}

			if ( this->SlabThickness > 0 ) {
				CellWidth = this->HorizInsThickness;

				SlabDistFromBottom = this->Extents.Ymax - this->SlabThickness - CellWidth / 2.0;

				// Partition at bottom edge of vertical insulation, if vertical insulation present
				if ( this->VertInsPresentFlag ) {
					YInsulationLocation = this->Extents.Ymax - this->VertInsDepth + CellWidth / 2.0;
				}
				if ( !allocated( this->Partitions.Y ) ) {
					if ( this->VertInsPresentFlag ) {
						this->Partitions.Y.allocate( { 0, 1 } );
						// Underslab partition
						this->Partitions.Y( 0 ) = MeshPartition( SlabDistFromBottom, PartitionType::UnderFloor, CellWidth );
						// Vertical-Insulation edge partition
						this->Partitions.Y( 1 ) = MeshPartition( YInsulationLocation, PartitionType::VertInsLowerEdge, CellWidth );
					} else {
						this->Partitions.Y.allocate( { 0, 0 } );
						// Underslab partition
						this->Partitions.Y( 0 ) = MeshPartition( SlabDistFromBottom, PartitionType::UnderFloor, CellWidth );
					}
				} else if ( !MeshPartitionArray_Contains( this->Partitions.Y, SlabDistFromBottom ) ) {
					PreviousUbound = this->Partitions.Y.u1();
					if ( allocated( PreviousEntries ) ) PreviousEntries.deallocate();
					PreviousEntries.allocate( { 0, PreviousUbound } );
					PreviousEntries = this->Partitions.Y;
					this->Partitions.Y.deallocate();

					// Partition at bottom edge of vertical insulation, if vertical insulation present
					if ( this->VertInsPresentFlag ) {
						this->Partitions.Y.allocate( { 0, PreviousUbound + 2 } );
						this->Partitions.Y( { 0, PreviousUbound } ) = PreviousEntries;

						// Underslab partition
						this->Partitions.Y( PreviousUbound + 1 ) = MeshPartition( SlabDistFromBottom, PartitionType::UnderFloor, CellWidth );
						// Vertical-Insulation edge partition
						this->Partitions.Y( PreviousUbound + 2 ) = MeshPartition( YInsulationLocation, PartitionType::VertInsLowerEdge, CellWidth );
					} else {
						this->Partitions.Y.allocate( { 0, PreviousUbound + 1 } );
						this->Partitions.Y( { 0, PreviousUbound } ) = PreviousEntries;
						// Underslab partition
						this->Partitions.Y( PreviousUbound + 1 ) = MeshPartition( SlabDistFromBottom, PartitionType::UnderFloor, CellWidth );
					}
				}
			}

			if ( this->SlabWidth > 0 ) {
				CellWidth = this->VertInsThickness;
				// Side Z direction
				SideZLocation = this->PerimeterOffset - CellWidth / 2.0;
				// Insulation Edge Z direction
				if ( this->HorizInsPresentFlag && !this->FullHorizInsPresent ) {
					SideZInsulationLocation = SideZLocation + this->HorizInsWidth;
				}
				if ( !allocated( this->Partitions.Z ) ) {
					// Partition at insulation edges in the Z direction, if horizontal insulation present
					if ( this->HorizInsPresentFlag ) {
						if ( !this->FullHorizInsPresent ) {
							this->Partitions.Z.allocate( { 0, 1 } );
							// Side Z direction
							this->Partitions.Z( 0 ) = MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth );
							// Insulation Edge Z direction
							this->Partitions.Z( 1 ) = MeshPartition( SideZInsulationLocation, PartitionType::HorizInsZSide, CellWidth );
						} else {
							this->Partitions.Z.allocate( { 0, 0 } );
							// Side Z direction
							this->Partitions.Z( 0 ) = MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth );
						}
					} else {
						this->Partitions.Z.allocate( { 0, 0 } );
						// Side Z direction
						this->Partitions.Z( 0 ) = MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth );
					}
				} else if ( !MeshPartitionArray_Contains( this->Partitions.Z, this->SlabWidth ) ) {
					PreviousUbound = this->Partitions.Z.u1();
					if ( allocated( PreviousEntries ) ) PreviousEntries.deallocate();
					PreviousEntries.allocate( { 0, PreviousUbound } );
					PreviousEntries = this->Partitions.Z;
					this->Partitions.Z.deallocate();

					// Partition at insulation edges in the Z direction, if horizontal insulation present
					if ( this->HorizInsPresentFlag ) {
						if ( !this->FullHorizInsPresent ) {
							this->Partitions.Z.allocate( { 0, PreviousUbound + 2 } );
							this->Partitions.Z( { 0, PreviousUbound } ) = PreviousEntries;
							// Side Z direction
							this->Partitions.Z( PreviousUbound + 1 ) = MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth );
							// Insulation Edge Z direction
							this->Partitions.Z( PreviousUbound + 2 ) = MeshPartition( SideZInsulationLocation, PartitionType::HorizInsZSide, CellWidth );
						} else {
							this->Partitions.Z.allocate( { 0, PreviousUbound + 1 } );
							this->Partitions.Z( { 0, PreviousUbound } ) = PreviousEntries;
							// Side Z direction
							this->Partitions.Z( PreviousUbound + 1 ) = MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth );
						}
					} else {
						this->Partitions.Z.allocate( { 0, PreviousUbound + 1 } );
						this->Partitions.Z( { 0, PreviousUbound } ) = PreviousEntries;
						// Side Z direction
						this->Partitions.Z( PreviousUbound + 1 ) = MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth );
					}
				}
			}
		}


		MeshPartition_SelectionSort( this->Partitions.X );
		MeshPartition_SelectionSort( this->Partitions.Y );
		MeshPartition_SelectionSort( this->Partitions.Z );


	}

	Array1D< GridRegion >
	FullDomainStructureInfo::createPartitionRegionList(
		Array1D< MeshPartition > const & ThesePartitionCenters,
		bool const PartitionsExist,
		Real64 const DirExtentMax,
		int const PartitionsUBound
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Return value
		Array1D< GridRegion > ThesePartitionRegions( {0,PartitionsUBound} );

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CreatePartitionRegionList" );

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Index;
		Real64 ThisCellWidthBy2;
		PartitionType ThisPartitionType;
		Real64 CellLeft;
		Real64 CellRight;
		int SubIndex;

		if ( ! PartitionsExist ) {
			return ThesePartitionRegions;
		}

		//'loop across all partitions
		for ( Index = ThesePartitionCenters.l1(); Index <= ThesePartitionCenters.u1(); ++Index ) {

			ThisCellWidthBy2 = ThesePartitionCenters( Index ).TotalWidth / 2.0;
			ThisPartitionType = ThesePartitionCenters( Index ).partitionType;

			//'use this half width to validate the region and add it to the collection
			CellLeft = ThesePartitionCenters( Index ).rDimension - ThisCellWidthBy2;
			CellRight = ThesePartitionCenters( Index ).rDimension + ThisCellWidthBy2;

			// check to make sure this location is valid
			if ( CellLeft < 0.0 || CellRight > DirExtentMax ) {
				ShowSevereError( "PlantPipingSystems::" + RoutineName + ": Invalid partition location in domain." );
				ShowContinueError( "Occurs during mesh development for domain=" + this->Name );
				ShowContinueError( "A pipe or basement is located outside of the domain extents." );
				ShowFatalError( "Preceding error causes program termination." );
			}

			// Scan all grid regions to make sure this range doesn't fall within an already entered range
			for ( SubIndex = 0; SubIndex <= Index - 1; ++SubIndex ) {
				// Coupled-basement model has adjacent partitions: ThesePartitionRegions( 0 ) and ThesePartitionRegions( 1 ) - SA
				if ( this->HasZoneCoupledBasement && Index ==1 ) {
					if ( IsInRange_BasementModel( CellLeft, ThesePartitionRegions( SubIndex ).Min, ThesePartitionRegions( SubIndex ).Max ) || IsInRange( CellRight, ThesePartitionRegions( SubIndex ).Min, ThesePartitionRegions( SubIndex ).Max ) ) {

						ShowSevereError( "PlantPipingSystems::" + RoutineName + ": Invalid partition location in domain." );
						ShowContinueError( "Occurs during mesh development for domain=" + this->Name );
						ShowContinueError( "A mesh conflict was encountered where partitions were overlapping." );
						ShowContinueError( "Ensure that all pipes exactly line up or are separated to allow meshing in between them" );
						ShowContinueError( "Also verify the pipe and basement dimensions to avoid conflicts there." );
						ShowFatalError( "Preceding error causes program termination" );

					}

				} else {

					if ( IsInRange( CellLeft, ThesePartitionRegions( SubIndex ).Min, ThesePartitionRegions( SubIndex ).Max ) || IsInRange( CellRight, ThesePartitionRegions( SubIndex ).Min, ThesePartitionRegions( SubIndex ).Max ) ) {

						ShowSevereError( "PlantPipingSystems::" + RoutineName + ": Invalid partition location in domain." );
						ShowContinueError( "Occurs during mesh development for domain=" + this->Name );
						ShowContinueError( "A mesh conflict was encountered where partitions were overlapping." );
						ShowContinueError( "Ensure that all pipes exactly line up or are separated to allow meshing in between them" );
						ShowContinueError( "Also verify the pipe and basement dimensions to avoid conflicts there." );
						ShowFatalError( "Preceding error causes program termination" );

					}
				}
			}

			ThesePartitionRegions( Index ).Min = CellLeft;
			ThesePartitionRegions( Index ).Max = CellRight;

			// Need to map partition type into region type parameters, since they are different enumerations
			if ( ThisPartitionType == PartitionType::BasementWall ) {
				ThesePartitionRegions( Index ).thisRegionType = RegionType::BasementWall;
			} else if ( ThisPartitionType == PartitionType::BasementFloor ) {
				ThesePartitionRegions( Index ).thisRegionType = RegionType::BasementFloor;
			} else if ( ThisPartitionType == PartitionType::Pipe ) {
				ThesePartitionRegions( Index ).thisRegionType = RegionType::Pipe;
			} else if ( ThisPartitionType == PartitionType::XSide ) {
				ThesePartitionRegions( Index ).thisRegionType = RegionType::XSide;
			} else if ( ThisPartitionType == PartitionType::XSideWall ) {
				ThesePartitionRegions( Index ).thisRegionType = RegionType::XSideWall;
			} else if ( ThisPartitionType == PartitionType::HorizInsXSide ) {
				ThesePartitionRegions( Index ).thisRegionType = RegionType::HorizInsXSide;
			} else if ( ThisPartitionType == PartitionType::ZSide ) {
				ThesePartitionRegions( Index ).thisRegionType = RegionType::ZSide;
			} else if ( ThisPartitionType == PartitionType::ZSideWall ) {
				ThesePartitionRegions( Index ).thisRegionType = RegionType::ZSideWall;
			} else if ( ThisPartitionType == PartitionType::HorizInsZSide ) {
				ThesePartitionRegions( Index ).thisRegionType = RegionType::HorizInsZSide;
			} else if ( ThisPartitionType == PartitionType::FloorInside ) {
				ThesePartitionRegions( Index ).thisRegionType = RegionType::FloorInside;
			} else if ( ThisPartitionType == PartitionType::UnderFloor ) {
				ThesePartitionRegions( Index ).thisRegionType = RegionType::UnderFloor;
			} else if ( ThisPartitionType == PartitionType::VertInsLowerEdge ) {
				ThesePartitionRegions( Index ).thisRegionType = RegionType::VertInsLowerEdge;
			} else {
				// diagnostic error
			}

		}

		return ThesePartitionRegions;

	}

	int
	CreateRegionListCount(
		Array1D< GridRegion > const & ThesePartitionRegions,
		Real64 const DirExtentMax,
		bool const PartitionsExist
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// USE STATEMENTS:
		using DataGlobals::AnyBasementsInModel;

		// Return value
		int RetVal;

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Index;

		RetVal = 0;
		if ( PartitionsExist ) {
			for ( Index = ThesePartitionRegions.l1(); Index <= ThesePartitionRegions.u1(); ++Index ) {
				// Coupled-basement model has adjacent partitions: ThesePartitionRegions( 0 ) and ThesePartitionRegions( 1 ). Do not add a region to the left of ThesePartitionRegions( 1 ).-SA
				if ( !AnyBasementsInModel || ( AnyBasementsInModel && ( Index == 0 || Index == 2 ) ) ) {
					//'add a mesh region to the "left" of the partition
					++RetVal;
				}
				//'then add the pipe node itself
				++RetVal;
				// some cleanup based on where we are
				if ( ( Index == 0 && size( ThesePartitionRegions ) == 1 ) || ( Index == ThesePartitionRegions.u1() && ThesePartitionRegions( Index ).Max < DirExtentMax ) ) {
					//'if there is only one partition, add a mesh region to the "right" before we leave
					//'or if we are on the last partition, and we have room on the "right" side then add a mesh region
					++RetVal;
				}
			}
		} else { // Input partitions were not allocate
			//'if we don't have a region, we still need to make a single mesh region
			++RetVal;
		}

		return RetVal;

	}

	Array1D< GridRegion >
	FullDomainStructureInfo::createRegionList(
		Array1D< GridRegion > const & ThesePartitionRegions,
		Real64 const DirExtentMax,
		RegionType const DirDirection,
		int const RetValUBound,
		bool const PartitionsExist,
		Optional_int BasementWallXIndex,
		Optional_int BasementFloorYIndex,
		Optional_int XIndex,
		Optional_int XWallIndex,
		Optional_int InsulationXIndex,
		Optional_int YIndex,
		Optional_int YFloorIndex,
		Optional_int InsulationYIndex,
		Optional_int ZIndex,
		Optional_int ZWallIndex,
		Optional_int InsulationZIndex
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Return value
		Array1D< GridRegion > RetVal( {0,RetValUBound} );

		Real64 LeftRegionExtent;
		int PreviousUbound;
		int Index;
		int SubIndex;
		int CellCountUpToNow;
		int NumCellWidths;

		// Object Data
		Array1D< TempGridRegionData > TempRegions( {0,RetValUBound} );
		GridRegion ThisRegion;
		TempGridRegionData PreviousRegion;

		PreviousUbound = -1;
		if ( PartitionsExist ) {
			for ( Index = ThesePartitionRegions.l1(); Index <= ThesePartitionRegions.u1(); ++Index ) {

				ThisRegion = ThesePartitionRegions( Index );

				if ( Index == 0 ) {
					LeftRegionExtent = 0.0;
				} else {
					LeftRegionExtent = ThesePartitionRegions( Index - 1 ).Max;
				}
				// Coupled-basement model has adjacent partitions: ThesePartitionRegions( 0 ) and ThesePartitionRegions( 1 ). Do not add a mesh region to the left of ThesePartitionRegions( 1 ).-SA
				if ( ! this->HasZoneCoupledBasement || ( Index == 0 || Index == 2 ) ) {
					//'add a mesh region to the "left" of the partition
					++PreviousUbound;
					TempRegions( PreviousUbound ) = TempGridRegionData( LeftRegionExtent, ThisRegion.Min, DirDirection );

					//'alert calling routines to the location of the basement cells within the domain
					CellCountUpToNow = 0;

					for ( SubIndex = TempRegions.l1(); SubIndex <= PreviousUbound; ++SubIndex ) {
						PreviousRegion = TempRegions( SubIndex );
						if ( std::set< RegionType >( { RegionType::Pipe, RegionType::BasementFloor, RegionType::BasementWall, RegionType::XSide, RegionType::XSideWall, RegionType::ZSide, RegionType::ZSideWall,
										RegionType::HorizInsXSide, RegionType::HorizInsZSide, RegionType::FloorInside, RegionType::UnderFloor, RegionType::VertInsLowerEdge } ).count( PreviousRegion.thisRegionType ) != 0 ) {
							++CellCountUpToNow;
						} else {
							CellCountUpToNow += this->getCellWidthsCount( DirDirection, SubIndex );
						}
					}
				} else {
					// alert calling routines to the location of the cell for ThesePartitionRegions( 1 ) in the coupled-basement model. - SA
					++CellCountUpToNow;
				}
				if ( ThisRegion.thisRegionType == RegionType::BasementWall ) {
					if ( present( BasementWallXIndex ) ) BasementWallXIndex = CellCountUpToNow;
				} else if ( ThisRegion.thisRegionType == RegionType::BasementFloor ) {
					if ( present( BasementFloorYIndex ) ) BasementFloorYIndex = CellCountUpToNow;
				} else if ( ThisRegion.thisRegionType == RegionType::XSide ) {
					if ( present( XIndex ) ) XIndex = CellCountUpToNow;
					this->XIndex = XIndex;
				} else if ( ThisRegion.thisRegionType == RegionType::XSideWall ) {
					if ( present( XWallIndex ) ) XWallIndex = CellCountUpToNow;
					this->XWallIndex = XWallIndex;
				} else if ( ThisRegion.thisRegionType == RegionType::ZSide ) {
					if ( present( ZIndex ) ) ZIndex = CellCountUpToNow;
					this->ZIndex = ZIndex;
				} else if ( ThisRegion.thisRegionType == RegionType::ZSideWall ) {
					if ( present( ZWallIndex ) ) ZWallIndex = CellCountUpToNow;
					this->ZWallIndex = ZWallIndex;
				} else if ( ThisRegion.thisRegionType == RegionType::HorizInsXSide ) {
					if ( present( InsulationXIndex ) ) InsulationXIndex = CellCountUpToNow;
					this->InsulationXIndex = InsulationXIndex;
				} else if ( ThisRegion.thisRegionType == RegionType::HorizInsZSide ) {
					if ( present( InsulationZIndex ) ) InsulationZIndex = CellCountUpToNow;
					this->InsulationZIndex = InsulationZIndex;
				} else if ( ThisRegion.thisRegionType == RegionType::FloorInside ) {
					if ( present( YFloorIndex ) ) YFloorIndex = CellCountUpToNow;
					this->YFloorIndex = YFloorIndex;
				} else if ( ThisRegion.thisRegionType == RegionType::UnderFloor ) {
					if ( present( YIndex ) ) YIndex = CellCountUpToNow;
					this->YIndex = YIndex;
				} else if ( ThisRegion.thisRegionType == RegionType::VertInsLowerEdge ) {
					if ( present( InsulationYIndex ) ) InsulationYIndex = CellCountUpToNow;
					this->InsulationYIndex = InsulationYIndex;
				}

				//'then add the pipe node itself
				++PreviousUbound;
				TempRegions( PreviousUbound ) = TempGridRegionData( ThisRegion.Min, ThisRegion.Max, ThisRegion.thisRegionType );

				// some cleanup based on where we are
				if ( ( Index == 0 && size( ThesePartitionRegions ) == 1 ) || ( Index == ThesePartitionRegions.u1() && ThisRegion.Max < DirExtentMax ) ) {
					//'if there is only one partition, add a mesh region to the "right" before we leave
					//'or if we are on the last partition, and we have room on the "right" side then add a mesh region
					++PreviousUbound;
					TempRegions( PreviousUbound ) = TempGridRegionData( ThisRegion.Max, DirExtentMax, DirDirection );
				}

			}
		} else { // Input partitions were not allocate
			//'if we don't have a region, we still need to make a single mesh region
			TempRegions( 0 ) = TempGridRegionData( 0.0, DirExtentMax, DirDirection );
		}

		//'finally repackage the grid regions into the final class form with cell counts included
		for ( Index = TempRegions.l1(); Index <= TempRegions.u1(); ++Index ) {
			RetVal( Index ).Min = TempRegions( Index ).Min;
			RetVal( Index ).Max = TempRegions( Index ).Max;
			RetVal( Index ).thisRegionType = TempRegions( Index ).thisRegionType;
			NumCellWidths = this->getCellWidthsCount( DirDirection, Index );
			if ( allocated( RetVal( Index ).CellWidths ) ) RetVal( Index ).CellWidths.deallocate();
			RetVal( Index ).CellWidths.allocate( {0,NumCellWidths - 1} );

			this->getCellWidths( RetVal( Index ), DirDirection );
		}

		return RetVal;

	}

	int
	CreateBoundaryListCount(
		Array1D< GridRegion > const & RegionList,
		RegionType const DirDirection
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Return value
		int RetVal;

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int Index;
		int CellWidthCtr;

		RetVal = 0;

		for ( Index = RegionList.l1(); Index <= RegionList.u1(); ++Index ) {
			if ( std::set< RegionType >( { RegionType::Pipe, RegionType::BasementFloor, RegionType::BasementWall, RegionType::XSide, RegionType::XSideWall, RegionType::ZSide, RegionType::ZSideWall,
							RegionType::HorizInsXSide, RegionType::HorizInsZSide, RegionType::FloorInside, RegionType::UnderFloor, RegionType::VertInsLowerEdge } ).count( RegionList( Index ).thisRegionType ) != 0 ) {
				++RetVal;
			} else {
				if ( RegionList( Index ).thisRegionType == DirDirection ) {
					for ( CellWidthCtr = RegionList( Index ).CellWidths.l1(); CellWidthCtr <= RegionList( Index ).CellWidths.u1(); ++CellWidthCtr ) {
						++RetVal;
					}
				}
			}
		}
		++RetVal;

		return RetVal;

	}

	Array1D< Real64 >
	CreateBoundaryList(
		Array1D< GridRegion > const & RegionList,
		Real64 const DirExtentMax,
		RegionType const DirDirection,
		int const RetValLbound,
		int const RetValUBound
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		Array1D< Real64 > RetVal( {RetValLbound,RetValUBound} );
		int Counter = -1;
		for ( int Index = RegionList.l1(); Index <= RegionList.u1(); ++Index ) {
			if ( std::set< RegionType >( { RegionType::Pipe, RegionType::BasementFloor, RegionType::BasementWall, RegionType::XSide, RegionType::XSideWall, RegionType::ZSide, RegionType::ZSideWall,
							RegionType::HorizInsXSide, RegionType::HorizInsZSide, RegionType::FloorInside, RegionType::UnderFloor, RegionType::VertInsLowerEdge } ).count( RegionList( Index ).thisRegionType ) != 0 ) {
				++Counter;
				RetVal( Counter ) = RegionList( Index ).Min;
			} else {
				if ( RegionList( Index ).thisRegionType == DirDirection ) {
					Real64 StartingPointCounter = RegionList( Index ).Min;
					for ( int CellWidthCtr = RegionList( Index ).CellWidths.l1(); CellWidthCtr <= RegionList( Index ).CellWidths.u1(); ++CellWidthCtr ) {
						++Counter;
						RetVal( Counter ) = StartingPointCounter;
						StartingPointCounter += RegionList( Index ).CellWidths( CellWidthCtr );
					}
				}
			}
		}
		RetVal( RetVal.u1() ) = DirExtentMax;
		return RetVal;

	}

	void
	FullDomainStructureInfo::createCellArray(
		Array1D< Real64 > const & XBoundaryPoints,
		Array1D< Real64 > const & YBoundaryPoints,
		Array1D< Real64 > const & ZBoundaryPoints
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int YIndexMax;
		CellType cellType;
		CellType ZWallCellType;
		CellType UnderBasementBoundary;
		int PipeCounter;

		int MaxBasementXNodeIndex( -1 );
		int MinBasementYNodeIndex( -1 );
		int MinXIndex( -1 );
		int YIndex( -1 );
		int MinZIndex( -1 );
		int XWallIndex( -1 );
		int ZWallIndex( -1 );
		int YFloorIndex( -1 );
		int InsulationXIndex( -1 );
		int InsulationYIndex( -1 );
		int InsulationZIndex( -1 );

		int CellXIndex;
		Real64 CellXMinValue;
		Real64 CellXMaxValue;
		Real64 CellXCenter;
		Real64 CellWidth;

		int CellYIndex;
		Real64 CellYMinValue;
		Real64 CellYMaxValue;
		Real64 CellYCenter;
		Real64 CellHeight;

		int CellZIndex;
		Real64 CellZMinValue;
		Real64 CellZMaxValue;
		Real64 CellZCenter;
		Real64 CellDepth;

		int PipeIndex;
		int NumRadialCells;
		Real64 InsulationThickness( 0.0 );
		int CircuitCtr;
		int CircuitIndex;
		int FoundOnCircuitIndex;

		Real64 RadialMeshThickness;
		bool HasInsulation;
		int TotNumCells = 0;
		int NumInsulationCells = 0;
		int NumGroundSurfaceCells = 0;

		//std::ofstream static outFile( "Cells.csv", std::ofstream::out );

		struct tCellExtents
		{
			// Members
			MeshExtents MyBase;
			Real64 Xmin;
			Real64 Ymin;
			Real64 Zmin;

			// Default Constructor
			tCellExtents()
			{}

			// Member Constructor
			tCellExtents(
				MeshExtents const & MyBase,
				Real64 const Xmin,
				Real64 const Ymin,
				Real64 const Zmin
			) :
				MyBase( MyBase ),
				Xmin( Xmin ),
				Ymin( Ymin ),
				Zmin( Zmin )
			{}

		};

		// Object Data
		DomainRectangle BasementRectangle;
		tCellExtents CellExtents;
		Point3DReal Centroid;
		Point3DInteger CellIndeces;
		RectangleF XYRectangle;
		RadialSizing PipeSizing;
		PipeSegmentInfo ThisSegment;

		//'subtract 2 in each dimension:
		//'     one for zero based array
		//'     one because the boundary points contain one entry more than the number of cells WITHIN the domain
		this->Cells.allocate( {0,isize( XBoundaryPoints )-2}, {0,isize( YBoundaryPoints )-2}, {0,isize( ZBoundaryPoints )-2} );

		YIndexMax = this->Cells.u2();
		MaxBasementXNodeIndex = this->BasementZone.BasementWallXIndex;
		MinBasementYNodeIndex = this->BasementZone.BasementFloorYIndex;
		BasementRectangle = DomainRectangle( 0, MaxBasementXNodeIndex, MinBasementYNodeIndex, YIndexMax );
		MinXIndex = this->XIndex;
		YIndex = this->YIndex;
		MinZIndex = this->ZIndex;
		XWallIndex = this->XWallIndex;
		YFloorIndex = this->YFloorIndex;
		ZWallIndex = this->ZWallIndex;
		InsulationXIndex = this->InsulationXIndex;
		InsulationYIndex = this->InsulationYIndex;
		InsulationZIndex = this->InsulationZIndex;

		auto & cells( this->Cells );
		for ( int X = cells.l1(), X_end = cells.u1(); X <= X_end; ++X ) {
			for ( int Y = cells.l2(), Y_end = cells.u2(); Y <= Y_end; ++Y ) {
				for ( int Z = cells.l3(), Z_end = cells.u3(); Z <= Z_end; ++Z ) {
					auto & cell( cells( X, Y, Z ) );

					//'set up x-direction variables
					CellXIndex = X; //'zero based index
					CellXMinValue = XBoundaryPoints( X ); //'left wall x-value
					CellXMaxValue = XBoundaryPoints( X + 1 ); //'right wall x-value
					CellXCenter = ( CellXMinValue + CellXMaxValue ) / 2;
					CellWidth = CellXMaxValue - CellXMinValue;

					//'set up y-direction variables
					CellYIndex = Y; //'zero based index
					CellYMinValue = YBoundaryPoints( Y ); //'bottom wall y-value
					CellYMaxValue = YBoundaryPoints( Y + 1 ); //'top wall y-value
					CellYCenter = ( CellYMinValue + CellYMaxValue ) / 2;
					CellHeight = CellYMaxValue - CellYMinValue;

					//'set up z-direction variables
					CellZIndex = Z; //'zero based index
					CellZMinValue = ZBoundaryPoints( Z ); //'lower z value
					CellZMaxValue = ZBoundaryPoints( Z + 1 ); //'higher z value
					CellZCenter = ( CellZMinValue + CellZMaxValue ) / 2;
					CellDepth = CellZMaxValue - CellZMinValue;

					//'set up an extent class for this cell
					CellExtents = tCellExtents( MeshExtents( CellXMaxValue, CellYMaxValue, CellZMaxValue ), CellXMinValue, CellYMinValue, CellZMinValue );

					//'set up centroid, index, and overall size
					Centroid = Point3DReal( CellXCenter, CellYCenter, CellZCenter );
					CellIndeces = Point3DInteger( CellXIndex, CellYIndex, CellZIndex );
					XYRectangle = RectangleF( CellXMinValue, CellYMinValue, CellWidth, CellHeight );

					//'determine cell type
					cellType = CellType::Unknown;

					//'if this is a pipe node, some flags are needed
					PipeIndex = -1;
					NumRadialCells = -1;
					CircuitIndex = -1;

					// Adiabatic behavior is now achieved in the SetupCellNeighbors routine, these are simply farfield for now.
					ZWallCellType = CellType::FarfieldBoundary;
					UnderBasementBoundary = CellType::FarfieldBoundary;

					//'apply boundary conditions

					// For zone-coupled ground domain
					if ( this->HasZoneCoupledSlab ) {
						if ( CellYIndex == cells.l2() ) { // Farfield cells
							cellType = CellType::FarfieldBoundary;
							++TotNumCells;
						} else if ( CellXIndex > MinXIndex && CellZIndex > MinZIndex ) { // Slab cells
							if ( CellYIndex < cells.u2() && CellYIndex > YIndex ) { // General slab cells
								cellType = CellType::Slab;
								++TotNumCells;
							} else if ( CellYIndex == cells.u2() ) { // Surface cells
								cellType = CellType::ZoneGroundInterface;
								++TotNumCells;
							} else if ( CellYIndex == YIndex ) { // Underslab insulation cells
								// Check if horizontal insulation present
								if ( this->HorizInsPresentFlag ) {
									if ( this->FullHorizInsPresent ) { // Entire underslab insulation
										cellType = CellType::HorizInsulation;
										++TotNumCells;
										++NumInsulationCells;
									} else { // Perimeter insulation
										if ( CellXIndex <= InsulationXIndex || CellZIndex <= InsulationZIndex ) {
											cellType = CellType::HorizInsulation;
											++TotNumCells;
											++NumInsulationCells;
										}
									}
								}
							}
						} else if ( CellXIndex == MinXIndex &&  CellZIndex > MinZIndex ) { // X side interface
							// Check if vertical insulation present
							if ( this->VertInsPresentFlag ) {
								if ( CellYIndex <= cells.u2() && CellYIndex >= InsulationYIndex ) { // Check depth of vertical insulation
									cellType = CellType::VertInsulation;
									++TotNumCells;
									++NumInsulationCells;
								}
							} else if ( CellYIndex == cells.u2() ) {
								cellType = CellType::GroundSurface;
								++TotNumCells;
								++NumGroundSurfaceCells;
							}
							if ( !this->SlabInGradeFlag ) {//Apply insulation to sides of slab in slab-on-grade configuration
								if ( CellYIndex <= cells.u2( ) && CellYIndex > YIndex ) {// Check depth of slab
									cellType = CellType::SlabOnGradeEdgeInsu;
									++TotNumCells;
								}
							}
						} else if ( CellZIndex == MinZIndex  &&  CellXIndex > MinXIndex ) { // Z side interface
							if ( this->VertInsPresentFlag ) { // Check if vertical insulation present
								if ( CellYIndex <= cells.u2() && CellYIndex >= InsulationYIndex ) { // Check depth of vertical insulation
									cellType = CellType::VertInsulation;
									++TotNumCells;
									++NumInsulationCells;
								}
							} else if ( CellYIndex == cells.u2() ) {
								cellType = CellType::GroundSurface;
								++TotNumCells;
								++NumGroundSurfaceCells;
							}
							if ( !this->SlabInGradeFlag ) { //Apply insulation to sides of slab in slab-on-grade configuration
								if ( CellYIndex <= cells.u2( ) && CellYIndex > YIndex ) { // Check depth of slab
									cellType = CellType::SlabOnGradeEdgeInsu;
									++TotNumCells;
								}
							}
						} else if ( CellYIndex == cells.u2() ) { // Surface cells
							cellType = CellType::GroundSurface;
							++TotNumCells;
							++NumGroundSurfaceCells;
						} else if ( CellYIndex == cells.l2() || CellXIndex == cells.l1() || CellZIndex == cells.l3() ) { // Farfield boundary
							cellType = CellType::FarfieldBoundary;
							++TotNumCells;
						}
					} else if ( this->HasZoneCoupledBasement ) { // basement model, zone-coupled
						// Set the appropriate cell type
						if ( CellYIndex == cells.l2() ) { // Farfield cells
							cellType = CellType::FarfieldBoundary;
							++TotNumCells;
						} else if ( CellXIndex > XWallIndex && CellZIndex > ZWallIndex ) { // Basement cutaway
							if ( CellYIndex <= cells.u2() && CellYIndex > YFloorIndex ) { // General basement cells
								cellType = CellType::BasementCutaway;
								// Not counting basement cutaway cells.
							} else if ( CellYIndex == YFloorIndex ) { //Basement Floor cells
								cellType = CellType::BasementFloor;
								++TotNumCells;
							} else if ( CellYIndex == YIndex ) {
								// Check if horizontal insulation present
								if ( this->HorizInsPresentFlag ) {
									if ( this->FullHorizInsPresent ) { // Entire underfloor insulated
										cellType = CellType::HorizInsulation;
										++TotNumCells;
										++NumInsulationCells;
									} else { //Perimeter insulation
										if ( CellXIndex < InsulationXIndex || CellZIndex < InsulationZIndex ) {
											cellType = CellType::HorizInsulation;
											++TotNumCells;
											++NumInsulationCells;
										}
									}
								}
							}
						} else if ( ( CellXIndex == XWallIndex && CellZIndex > ZWallIndex ) || ( CellZIndex == ZWallIndex && CellXIndex > XWallIndex ) ) { // Basement Walls
							if ( CellYIndex <= cells.u2() && CellYIndex > YFloorIndex ) {
								cellType = CellType::BasementWall;
								++TotNumCells;
							}
						} else if ( ( CellXIndex == MinXIndex && CellZIndex > ZWallIndex ) || ( CellZIndex == MinZIndex && CellXIndex > XWallIndex ) ) { // Insulation cells
							if ( CellYIndex <= cells.u2() && CellYIndex > YFloorIndex ) {
								// Check if vertical insulation present
								if ( this->VertInsPresentFlag ) {
									if ( InsulationYIndex != 0 ) { // Partial vertical insulation
										if ( CellYIndex <= cells.u2() && CellYIndex > InsulationYIndex ) {
											cellType = CellType::VertInsulation;
											++TotNumCells;
											++NumInsulationCells;
										}
									} else { //Vertical insulation extends to depth of basement floor
										if ( CellYIndex <= cells.u2() && CellYIndex > YFloorIndex ) {
											cellType = CellType::VertInsulation;
											++TotNumCells;
											++NumInsulationCells;
										}
									}
								}
							}
						} else if ( CellYIndex == cells.u2() ) { // Surface cells
							cellType = CellType::GroundSurface;
							++TotNumCells;
							++NumGroundSurfaceCells;
						} else if ( CellYIndex == cells.l2() || CellXIndex == cells.l1() || CellZIndex == cells.l3() ) { // Farfield boundary
							cellType = CellType::FarfieldBoundary;
							++TotNumCells;
						}
					} else if ( CellXIndex == MaxBasementXNodeIndex && CellYIndex == MinBasementYNodeIndex ) {
						cellType = CellType::BasementCorner;
						++TotNumCells;
					} else if ( CellXIndex == MaxBasementXNodeIndex && CellYIndex > MinBasementYNodeIndex ) {
						cellType = CellType::BasementWall;
						++TotNumCells;
					} else if ( CellXIndex < MaxBasementXNodeIndex && CellYIndex == MinBasementYNodeIndex ) {
						cellType = CellType::BasementFloor;
						++TotNumCells;
					} else if ( CellXIndex < MaxBasementXNodeIndex && CellYIndex > MinBasementYNodeIndex ) {
						cellType = CellType::BasementCutaway;
						//Not counting basement cutaway cells
					} else if ( CellYIndex == cells.u2() ) {
						cellType = CellType::GroundSurface;
						++TotNumCells;
						++NumGroundSurfaceCells;
					} else if ( CellXIndex == 0 ) {
						if ( this->HasBasement && Y > 0 ) {
							cellType = UnderBasementBoundary; //'this must come after the basement cutaway ELSEIF branch
							++TotNumCells;
						} else {
							cellType = CellType::FarfieldBoundary;
							++TotNumCells;
						}
					} else if ( CellXIndex == cells.u1() || CellYIndex == 0 ) {
						cellType = CellType::FarfieldBoundary;
						++TotNumCells;
					} else if ( CellZIndex == 0 || CellZIndex == cells.u3() ) {
						cellType = ZWallCellType;
						++TotNumCells;
					}

					//'check to see if this is a pipe node...
					for ( CircuitCtr = this->CircuitIndeces.l1(); CircuitCtr <= this->CircuitIndeces.u1(); ++CircuitCtr ) {

						FoundOnCircuitIndex = this->CircuitIndeces( CircuitCtr );
						for ( PipeCounter = PipingSystemCircuits( FoundOnCircuitIndex ).PipeSegmentIndeces.l1(); PipeCounter <= PipingSystemCircuits( FoundOnCircuitIndex ).PipeSegmentIndeces.u1(); ++PipeCounter ) {

							ThisSegment = PipingSystemSegments( PipingSystemCircuits( FoundOnCircuitIndex ).PipeSegmentIndeces( PipeCounter ) );
							if ( XYRectangle.contains( ThisSegment.PipeLocation ) ) {
								//'inform the cell that it is a pipe node
								cellType = CellType::Pipe;
								//'inform the cell of which pipe it contains
								PipeIndex = PipeCounter;
								//'inform the cell of which pipe circuit contains it
								CircuitIndex = FoundOnCircuitIndex;
								//'inform the pipe of what cell it is inside
								PipingSystemSegments( PipingSystemCircuits( FoundOnCircuitIndex ).PipeSegmentIndeces( PipeCounter ) ).initPipeCells( CellXIndex, CellYIndex );
								//'set the number of cells to be generated in this near-pipe region
								NumRadialCells = PipingSystemCircuits( FoundOnCircuitIndex ).NumRadialCells;
								//'exit the pipe counter loop
								goto CircuitLoop_exit;
							}

						}
					}
					CircuitLoop_exit: ;

					//'if it still isn't anything, then it is just an interior node
					if ( cellType == CellType::Unknown ) {
						cellType = CellType::GeneralField;
						++TotNumCells;
					}

					// if we were found on a pipe circuit, get some things for convenience
					if ( CircuitIndex != -1 ) {
						if ( PipingSystemCircuits( CircuitIndex ).HasInsulation ) {
							InsulationThickness = PipingSystemCircuits( CircuitIndex ).InsulationSize.thickness();
						}
						PipeSizing = PipingSystemCircuits( CircuitIndex ).PipeSize;
						RadialMeshThickness = PipingSystemCircuits( CircuitIndex ).RadialMeshThickness;
						HasInsulation = PipingSystemCircuits( CircuitIndex ).HasInsulation;
					}

					//'instantiate the cell class
					cell.X_min = CellExtents.Xmin;
					cell.X_max = CellExtents.MyBase.Xmax;
					cell.Y_min = CellExtents.Ymin;
					cell.Y_max = CellExtents.MyBase.Ymax;
					cell.Z_min = CellExtents.Zmin;
					cell.Z_max = CellExtents.MyBase.Zmax;
					cell.X_index = CellIndeces.X;
					cell.Y_index = CellIndeces.Y;
					cell.Z_index = CellIndeces.Z;
					cell.Centroid = Centroid;
					cell.cellType = cellType;

					if ( PipeIndex != -1 ) {
						cell.PipeIndex = PipeIndex;
						CartesianPipeCellInformation::ctor( cell.PipeCellData, cell.X_max - cell.X_min, PipeSizing, NumRadialCells, cell.depth(), InsulationThickness, RadialMeshThickness, HasInsulation );
					}

//#ifdef CalcEnergyBalance
//					outFile << cell.CellType << "," << cell.X_index << "," << cell.Y_index << "," << cell.Z_index << "," << cell.X_max - cell.X_min << "," << cell.Y_max - cell.Y_min << "," << cell.Z_max - cell.Z_min << std::endl;
//#endif

				} //'z
			} //'y
		} //'x

		this->NumDomainCells = TotNumCells;
		this->NumGroundSurfCells = NumGroundSurfaceCells;
		this->NumInsulationCells = NumInsulationCells;
	}

	void
	FullDomainStructureInfo::setupCellNeighbors()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ThisCellCentroidX;
		Real64 ThisCellCentroidY;
		Real64 ThisCellCentroidZ;
		Real64 CellRightCentroidX;
		Real64 CellRightLeftWallX;
		Real64 CellLeftCentroidX;
		Real64 CellLeftRightWallX;
		Real64 LeftCellCentroidX;
		Real64 LeftCellRightWallX;
		Real64 RightCellCentroidX;
		Real64 RightCellLeftWallX;
		Real64 UpperCellCentroidY;
		Real64 UpperCellLowerWallY;
		Real64 LowerCellCentroidY;
		Real64 LowerCellUpperWallY;
		Real64 UpperZCellCentroidZ;
		Real64 UpperZCellLowerWallZ;
		Real64 LowerZCellCentroidZ;
		Real64 LowerZCellUpperWallZ;

		bool DoingBESTEST = true;

		auto const & cells( this->Cells );
		for ( int X = cells.l1(), X_end = cells.u1(); X <= X_end; ++X ) {
			for ( int Y = cells.l2(), Y_end = cells.u2(); Y <= Y_end; ++Y ) {
				for ( int Z = cells.l3(), Z_end = cells.u3(); Z <= Z_end; ++Z ) {
					auto const & cell( cells( X, Y, Z ) );

					//'for convenience
					ThisCellCentroidX = cell.Centroid.X;
					ThisCellCentroidY = cell.Centroid.Y;
					ThisCellCentroidZ = cell.Centroid.Z;
					Real64 ThisAdiabaticMultiplier = 1.0;
					Real64 ThisAdiabaticMultiplierMirror = 1.0;

					//'setup east/west cell neighbors
					if ( X == 0 ) {
						CellRightCentroidX = cells( X + 1, Y, Z ).Centroid.X;
						CellRightLeftWallX = cells( X + 1, Y, Z ).X_min;
						// on the X=0 face, the only adiabatic cases are:
						//   1) For a non-zone-coupled basement simulation, for example for simple FHX simulation, in which case only if y > 0
						//   2) If we are doing BESTEST build
						if ( ( (!this->HasZoneCoupledSlab) && (!this->HasZoneCoupledBasement) && (this->HasBasement) && (Y>0) ) || DoingBESTEST ) {
							ThisAdiabaticMultiplier = 2.0;
							ThisAdiabaticMultiplierMirror = 0.0;
						}
						this->addNeighborInformation( X, Y, Z, Direction::PositiveX, CellRightCentroidX - ThisCellCentroidX, CellRightLeftWallX - ThisCellCentroidX, CellRightCentroidX - CellRightLeftWallX, ThisAdiabaticMultiplier );
						this->addNeighborInformation( X, Y, Z, Direction::NegativeX, 0.0, 0.0, 0.0, ThisAdiabaticMultiplierMirror );
					} else if ( X == cells.u1() ) {
						// on the X=XMAX face, the adiabatic cases are:
						//   1) if we are doing a zone coupled slab/basement simulation where we quartered the domain
						//   2) if we are doing a special BESTEST build where the farfield is actually adiabatic
						if ( this->HasZoneCoupledSlab || this->HasZoneCoupledBasement || DoingBESTEST ) {
							ThisAdiabaticMultiplier = 2.0;
							ThisAdiabaticMultiplierMirror = 0.0;
						}
						CellLeftCentroidX = cells( X - 1, Y, Z ).Centroid.X;
						CellLeftRightWallX = cells( X - 1, Y, Z ).X_max;
						this->addNeighborInformation( X, Y, Z, Direction::NegativeX, ThisCellCentroidX - CellLeftCentroidX, ThisCellCentroidX - CellLeftRightWallX, CellLeftRightWallX - CellLeftCentroidX, ThisAdiabaticMultiplier );
						this->addNeighborInformation( X, Y, Z, Direction::PositiveX, 0.0, 0.0, 0.0, ThisAdiabaticMultiplierMirror );
					} else {
						LeftCellCentroidX = cells( X - 1, Y, Z ).Centroid.X;
						LeftCellRightWallX = cells( X - 1, Y, Z ).X_max;
						RightCellCentroidX = cells( X + 1, Y, Z ).Centroid.X;
						RightCellLeftWallX = cells( X + 1, Y, Z ).X_min;
						this->addNeighborInformation( X, Y, Z, Direction::NegativeX, ThisCellCentroidX - LeftCellCentroidX, ThisCellCentroidX - LeftCellRightWallX, LeftCellRightWallX - LeftCellCentroidX, ThisAdiabaticMultiplier );
						this->addNeighborInformation( X, Y, Z, Direction::PositiveX, RightCellCentroidX - ThisCellCentroidX, RightCellLeftWallX - ThisCellCentroidX, RightCellCentroidX - RightCellLeftWallX, ThisAdiabaticMultiplier );
					}

					// Reset for the Y direction assignments
					ThisAdiabaticMultiplier = 1.0;
					ThisAdiabaticMultiplierMirror = 1.0;

					//'setup north/south cell neighbors
					if ( Y == 0 ) {
						UpperCellCentroidY = cells( X, Y + 1, Z ).Centroid.Y;
						UpperCellLowerWallY = cells( X, Y + 1, Z ).Y_min;
						// on the Y=0 face, the only adiabatic cases are:
						//   1) NONE
						this->addNeighborInformation( X, Y, Z, Direction::PositiveY, UpperCellCentroidY - ThisCellCentroidY, UpperCellLowerWallY - ThisCellCentroidY, UpperCellCentroidY - UpperCellLowerWallY, ThisAdiabaticMultiplier );
						this->addNeighborInformation( X, Y, Z, Direction::NegativeY, 0.0, 0.0, 0.0, ThisAdiabaticMultiplierMirror );
					} else if ( Y == cells.u2() ) {
						LowerCellCentroidY = cells( X, Y - 1, Z ).Centroid.Y;
						LowerCellUpperWallY = cells( X, Y - 1, Z ).Y_max;
						// on the Y=YMAX face, the only adiabatic cases are:
						//   1) NONE
						this->addNeighborInformation( X, Y, Z, Direction::NegativeY, ThisCellCentroidY - LowerCellCentroidY, ThisCellCentroidY - LowerCellUpperWallY, LowerCellUpperWallY - LowerCellCentroidY, ThisAdiabaticMultiplier );
						this->addNeighborInformation( X, Y, Z, Direction::PositiveY, 0.0, 0.0, 0.0, ThisAdiabaticMultiplierMirror );
					} else {
						UpperCellCentroidY = cells( X, Y + 1, Z ).Centroid.Y;
						LowerCellCentroidY = cells( X, Y - 1, Z ).Centroid.Y;
						UpperCellLowerWallY = cells( X, Y + 1, Z ).Y_min;
						LowerCellUpperWallY = cells( X, Y - 1, Z ).Y_max;
						this->addNeighborInformation( X, Y, Z, Direction::NegativeY, ThisCellCentroidY - LowerCellCentroidY, ThisCellCentroidY - LowerCellUpperWallY, LowerCellUpperWallY - LowerCellCentroidY, ThisAdiabaticMultiplier );
						this->addNeighborInformation( X, Y, Z, Direction::PositiveY, UpperCellCentroidY - ThisCellCentroidY, UpperCellLowerWallY - ThisCellCentroidY, UpperCellCentroidY - UpperCellLowerWallY, ThisAdiabaticMultiplier );
					}

					// Reset for the Z direction assignments
					ThisAdiabaticMultiplier = 1.0;
					ThisAdiabaticMultiplierMirror = 1.0;

					//'setup forward/backward cell neighbors
					if ( Z == 0 ) {
						UpperZCellCentroidZ = cells( X, Y, Z + 1 ).Centroid.Z;
						UpperZCellLowerWallZ = cells( X, Y, Z + 1 ).Z_min;
						// on the Z=0 face, the only adiabatic cases are:
						//   1) for a non-zone-related simulation, such as for a standalone ground HX, or if we have the regular HasBasement simulation
						//   2) if we are doing BESTEST build
						if ( ( (!this->HasZoneCoupledSlab) && (!this->HasZoneCoupledBasement) ) || DoingBESTEST ) {
							ThisAdiabaticMultiplier = 2.0;
							ThisAdiabaticMultiplierMirror = 0.0;
						}
						this->addNeighborInformation( X, Y, Z, Direction::PositiveZ, UpperZCellCentroidZ - ThisCellCentroidZ, UpperZCellLowerWallZ - ThisCellCentroidZ, UpperZCellCentroidZ - UpperZCellLowerWallZ, ThisAdiabaticMultiplier );
						this->addNeighborInformation( X, Y, Z, Direction::NegativeZ, 0.0, 0.0, 0.0, ThisAdiabaticMultiplierMirror );
					} else if ( Z == cells.u3() ) {
						LowerZCellCentroidZ = cells( X, Y, Z - 1 ).Centroid.Z;
						LowerZCellUpperWallZ = cells( X, Y, Z - 1 ).Z_max;
						// on the Z=ZMAX face, the only adiabatic cases are:
						//   1) this face is always adiabatic?
						//if (  ) {
							ThisAdiabaticMultiplier = 2.0;
							ThisAdiabaticMultiplierMirror = 0.0;
						//}
						this->addNeighborInformation( X, Y, Z, Direction::NegativeZ, ThisCellCentroidZ - LowerZCellCentroidZ, ThisCellCentroidZ - LowerZCellUpperWallZ, LowerZCellUpperWallZ - LowerZCellCentroidZ, ThisAdiabaticMultiplier );
						this->addNeighborInformation( X, Y, Z, Direction::PositiveZ, 0.0, 0.0, 0.0, ThisAdiabaticMultiplierMirror );
					} else {
						LowerZCellCentroidZ = cells( X, Y, Z - 1 ).Centroid.Z;
						UpperZCellCentroidZ = cells( X, Y, Z + 1 ).Centroid.Z;
						UpperZCellLowerWallZ = cells( X, Y, Z + 1 ).Z_min;
						LowerZCellUpperWallZ = cells( X, Y, Z - 1 ).Z_max;
						this->addNeighborInformation( X, Y, Z, Direction::NegativeZ, ThisCellCentroidZ - LowerZCellCentroidZ, ThisCellCentroidZ - LowerZCellUpperWallZ, LowerZCellUpperWallZ - LowerZCellCentroidZ, ThisAdiabaticMultiplier );
						this->addNeighborInformation( X, Y, Z, Direction::PositiveZ, UpperZCellCentroidZ - ThisCellCentroidZ, UpperZCellLowerWallZ - ThisCellCentroidZ, UpperZCellCentroidZ - UpperZCellLowerWallZ, ThisAdiabaticMultiplier );
					}

				}
			}
		}

	}

	void
	FullDomainStructureInfo::addNeighborInformation(
		int const X,
		int const Y,
		int const Z,
		Direction const direction,
		Real64 const ThisCentroidToNeighborCentroid,
		Real64 const ThisCentroidToNeighborWall,
		Real64 const ThisWallToNeighborCentroid,
		Real64 const ThisAdiabaticMultiplier
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PrevUBound;

		auto & cell( this->Cells( X, Y, Z ) );
		if ( ! allocated( cell.NeighborInformation ) ) {
			cell.NeighborInformation.allocate( {0,0} );
			PrevUBound = -1;
		} else {
			PrevUBound = cell.NeighborInformation.u1();
			cell.NeighborInformation.redimension( {0,PrevUBound + 1} );
		}

		cell.NeighborInformation( PrevUBound + 1 ).direction = direction;
		cell.NeighborInformation( PrevUBound + 1 ).Value.ThisCentroidToNeighborCentroid = ThisCentroidToNeighborCentroid;
		cell.NeighborInformation( PrevUBound + 1 ).Value.ThisCentroidToNeighborWall = ThisCentroidToNeighborWall;
		cell.NeighborInformation( PrevUBound + 1 ).Value.ThisWallToNeighborCentroid = ThisWallToNeighborCentroid;
		cell.NeighborInformation( PrevUBound + 1 ).Value.adiabaticMultiplier = ThisAdiabaticMultiplier;
	}

	void
	FullDomainStructureInfo::setupPipeCircuitInOutCells()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SegmentInletCellX;
		int SegmentInletCellY;
		int SegmentInletCellZ;
		int SegmentOutletCellX;
		int SegmentOutletCellY;
		int SegmentOutletCellZ;
		int CircuitInletCellX;
		int CircuitInletCellY;
		int CircuitInletCellZ;
		int CircuitOutletCellX;
		int CircuitOutletCellY;
		int CircuitOutletCellZ;

		auto const & cells( this->Cells );
		for ( int CircuitNum = this->CircuitIndeces.l1(); CircuitNum <= this->CircuitIndeces.u1(); ++CircuitNum ) {

			int CircuitIndex = this->CircuitIndeces( CircuitNum );
			bool CircuitInletCellSet = false;

			for ( int SegmentCtr = PipingSystemCircuits( CircuitIndex ).PipeSegmentIndeces.l1(); SegmentCtr <= PipingSystemCircuits( CircuitIndex ).PipeSegmentIndeces.u1(); ++SegmentCtr ) {

				auto & Segment = PipingSystemSegments( PipingSystemCircuits( CircuitIndex ).PipeSegmentIndeces( SegmentCtr ) );
				switch ( Segment.FlowDirection ) {
				case SegmentFlow::IncreasingZ:
					SegmentInletCellX = Segment.PipeCellCoordinates.X;
					SegmentInletCellY = Segment.PipeCellCoordinates.Y;
					SegmentInletCellZ = 0;
					SegmentOutletCellX = Segment.PipeCellCoordinates.X;
					SegmentOutletCellY = Segment.PipeCellCoordinates.Y;
					SegmentOutletCellZ = cells.u3();
					break;
				case SegmentFlow::DecreasingZ:
					SegmentInletCellX = Segment.PipeCellCoordinates.X;
					SegmentInletCellY = Segment.PipeCellCoordinates.Y;
					SegmentInletCellZ = cells.u3();
					SegmentOutletCellX = Segment.PipeCellCoordinates.X;
					SegmentOutletCellY = Segment.PipeCellCoordinates.Y;
					SegmentOutletCellZ = 0;
					break;
				}
				if ( ! CircuitInletCellSet ) {
					CircuitInletCellX = SegmentInletCellX;
					CircuitInletCellY = SegmentInletCellY;
					CircuitInletCellZ = SegmentInletCellZ;
					CircuitInletCellSet = true;
				}
				CircuitOutletCellX = SegmentOutletCellX;
				CircuitOutletCellY = SegmentOutletCellY;
				CircuitOutletCellZ = SegmentOutletCellZ;

			}

			PipingSystemCircuits( CircuitIndex ).initInOutCells( cells( CircuitInletCellX, CircuitInletCellY, CircuitInletCellZ ), cells( CircuitOutletCellX, CircuitOutletCellY, CircuitOutletCellZ ) );

		}

	}

	int
	FullDomainStructureInfo::getCellWidthsCount(
		RegionType const dir,
		int const n
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Return value
		int RetVal( 0 );

		if ( dir == RegionType::XDirection ) {
			RetVal = this->Mesh.X.RegionMeshCount;
		} else if ( dir == RegionType::YDirection ) {
			// Set slab cell count
			if ( this->HasZoneCoupledSlab ) {//Slab model
				if ( ( this->VertInsPresentFlag && n == 4 ) || ( !this->VertInsPresentFlag && n == 2 ) ) {//Slab region
					RetVal = this->NumSlabCells;
				} else {//Other regions
					RetVal = this->Mesh.Y.RegionMeshCount;
				}
			} else {//basement models
				RetVal = this->Mesh.Y.RegionMeshCount;
			}
		} else if ( dir == RegionType::ZDirection ) {
			RetVal = this->Mesh.Z.RegionMeshCount;
		} else {
			assert( false );
		}
		//Autodesk:Return Check/enforce that one of these CASEs holds to assure return value is set

		return RetVal;

	}

	void
	FullDomainStructureInfo::getCellWidths(
		GridRegion & g,
		RegionType const direction
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int NumCellsOnEachSide;
		int NumCells;
		Real64 SummationTerm;
		int I;
		Real64 CellWidth;
		int SubIndex;
		Array1D< Real64 > RetVal;
		int RetMaxIndex;

		// Object Data
		DistributionStructure ThisMesh;
		ThisMesh.RegionMeshCount = 0;
		ThisMesh.GeometricSeriesCoefficient = 0.0;

		switch ( direction ) {
		case RegionType::XDirection:
			ThisMesh = this->Mesh.X;
			break;
		case RegionType::YDirection:
			ThisMesh = this->Mesh.Y;
			break;
		case RegionType::ZDirection:
			ThisMesh = this->Mesh.Z;
			break;
		default:
			ShowSevereError( "Invalid RegionType passed to PlantPipingSystems::FullDomainStructureInfo::getCellWidths; should be x, y, or z direction only." );
			ShowContinueError( "This is a developer problem, as the code should never reach this point." );
			ShowFatalError( "EnergyPlus aborts due to the previous severe error" );
		}

		if ( ThisMesh.RegionMeshCount > 0 ) {
			RetVal.allocate( {0,ThisMesh.RegionMeshCount - 1} );
			RetMaxIndex = ThisMesh.RegionMeshCount - 1;
		} else {
			RetVal.allocate( {0,0} );
			RetMaxIndex = 0;
		}

		Real64 GridWidth = g.Max - g.Min;

		if ( ThisMesh.thisMeshDistribution == MeshDistribution::Uniform ) {
			if ( this->HasZoneCoupledSlab && g.thisRegionType == RegionType::YDirection && g.Max == this->Extents.Ymax ) {//Slab region
				NumCells = this->NumSlabCells;
				if ( allocated( RetVal ) ) RetVal.deallocate( );
				RetVal.allocate( { 0, NumCells - 1 } );
				RetMaxIndex = NumCells - 1;
				CellWidth = GridWidth / NumCells;

				for ( I = 0; I <= NumCells - 1; ++I ) {
					RetVal( I ) = CellWidth;
				}
			} else {
				// we have it quite simple
				assert( ThisMesh.RegionMeshCount > 0 );
				CellWidth = GridWidth / ThisMesh.RegionMeshCount;

				for ( I = 0; I <= ThisMesh.RegionMeshCount - 1; ++I ) {
					RetVal( I ) = CellWidth;
				}
			}
		} else if ( ThisMesh.thisMeshDistribution == MeshDistribution::SymmetricGeometric ) {

			//'then apply this "direction"'s conditions to generate a cell width array
			//'first get the total number of cells on this half of the region
			NumCellsOnEachSide = ThisMesh.RegionMeshCount / 2; // Already validated to be an even #

			//'calculate geometric series
			SummationTerm = 0.0;
			for ( I = 1; I <= NumCellsOnEachSide; ++I ) {
				SummationTerm += std::pow( ThisMesh.GeometricSeriesCoefficient, I - 1 );
			}

			//'set up a list of cell widths for this region
			CellWidth = ( GridWidth / 2 ) / SummationTerm;
			RetVal( 0 ) = CellWidth;
			for ( I = 1; I <= NumCellsOnEachSide - 1; ++I ) {
				CellWidth *= ThisMesh.GeometricSeriesCoefficient;
				RetVal( I ) = CellWidth;
			}
			SubIndex = NumCellsOnEachSide;
			for ( I = NumCellsOnEachSide - 1; I >= 0; --I ) {
				RetVal( SubIndex ) = RetVal( I );
				++SubIndex;  // SubIndex should be incremented here - After RetVal (SubIndex) is assigned a value. -SA
			}

		} else if ( ThisMesh.thisMeshDistribution == MeshDistribution::Geometric ) {

			NumCells = ThisMesh.RegionMeshCount;

			if ( g.thisRegionType == RegionType::XDirection || g.thisRegionType == RegionType::ZDirection ) {
				//'calculate geometric series
				SummationTerm = 0.0;
				for ( I = 1; I <= NumCells; ++I ) {
					SummationTerm += std::pow( ThisMesh.GeometricSeriesCoefficient, I - 1 );
				}
				CellWidth = GridWidth / SummationTerm;
				if ( g.Min == 0 ) {
					//Ground region to the left of the slab will have cells expanding to the left
					RetVal( NumCells - 1 ) = CellWidth;
					for ( I = NumCells - 2; I >= 0; --I ) {
						CellWidth *= ThisMesh.GeometricSeriesCoefficient;
						RetVal( I ) = CellWidth;
					}
				} else {
					//Slab region will have cells expanding to the right
					RetVal( 0 ) = CellWidth;
					for ( I = 1; I <= NumCells - 1; ++I ) {
						CellWidth *= ThisMesh.GeometricSeriesCoefficient;
						RetVal( I ) = CellWidth;
					}
				}
			} else if ( g.thisRegionType == RegionType::YDirection ) {
				//Assign uniform cell thickness to the slab cells.
				if ( g.Max == this->Extents.Ymax ) {
					NumCells = this->NumSlabCells;
					if ( allocated( RetVal ) ) RetVal.deallocate( );
					RetVal.allocate( { 0, NumCells - 1 } );
					RetMaxIndex = NumCells - 1;

					CellWidth = GridWidth / NumCells;

					for ( I = 0; I <= NumCells - 1; ++I ) {
						RetVal( I ) = CellWidth;
					}
				} else {
					//'calculate geometric series
					SummationTerm = 0.0;
					for ( I = 1; I <= NumCells; ++I ) {
						SummationTerm += std::pow( ThisMesh.GeometricSeriesCoefficient, I - 1 );
					}
					CellWidth = GridWidth / SummationTerm;

					//Ground region under the slab will have cells expanding as we go down
					RetVal( NumCells - 1 ) = CellWidth;
					for ( I = NumCells - 2; I >= 0; --I ) {
						CellWidth *= ThisMesh.GeometricSeriesCoefficient;
						RetVal( I ) = CellWidth;
					}
				}
			}
		}

		g.CellWidths( {0,RetMaxIndex} ) = RetVal( {0,RetMaxIndex} );
		RetVal.deallocate();
	}

	void
	PerformIterationLoop(
		int const DomainNum,
		Optional < int const > CircuitNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Always do start of time step inits
		DoStartOfTimeStepInitializations( DomainNum, CircuitNum );

		// Prepare the pipe circuit for calculations, but we'll actually do calcs at the iteration level
		if ( PipingSystemDomains( DomainNum ).HasAPipeCircuit ) {
			PreparePipeCircuitSimulation( DomainNum, CircuitNum );
		}

		// Begin iterating for this time step
		for ( int IterationIndex = 1; IterationIndex <= PipingSystemDomains( DomainNum ).SimControls.MaxIterationsPerTS; ++IterationIndex ) {

			ShiftTemperaturesForNewIteration( DomainNum );

			if ( PipingSystemDomains( DomainNum ).HasAPipeCircuit ) {
				PerformPipeCircuitSimulation( DomainNum, CircuitNum );
			}

			if ( PipingSystemDomains( DomainNum ).DomainNeedsSimulation ) PerformTemperatureFieldUpdate( DomainNum );
			bool FinishedIterationLoop = false;
			DoEndOfIterationOperations( DomainNum, FinishedIterationLoop );

#ifdef CalcEnergyBalance
			if( FinishedIterationLoop ) {
				UpdateMaxEnergyBalance( DomainNum );
			}
#endif
			if ( FinishedIterationLoop ) break;
		}

		// Update the basement surface temperatures, if any
		if ( PipingSystemDomains( DomainNum ).HasBasement || PipingSystemDomains( DomainNum ).HasZoneCoupledBasement ) {
			PipingSystemDomains( DomainNum ).UpdateBasementSurfaceTemperatures();
		}

		// Update the slab surface temperatures, if any
		if ( PipingSystemDomains( DomainNum ).HasZoneCoupledSlab ) {
			PipingSystemDomains( DomainNum ).UpdateZoneSurfaceTemperatures();
		}
	}

	void
	PerformTemperatureFieldUpdate( int const DomainNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		auto & cells( PipingSystemDomains( DomainNum ).Cells );
		for ( int X = cells.l1(), X_end = cells.u1(); X <= X_end; ++X ) {
			for ( int Y = cells.l2(), Y_end = cells.u2(); Y <= Y_end; ++Y ) {
				for ( int Z = cells.l3(), Z_end = cells.u3(); Z <= Z_end; ++Z ) {
					auto & cell( cells( X, Y, Z ) );

					//if ( X==0 && Y==10 && Z==10 ) {
					//	int j = 23;
					//}

					{ auto const SELECT_CASE_var( cell.cellType );
					if ( SELECT_CASE_var == CellType::Pipe ) {
						//'pipes are simulated separately
					} else if ( ( SELECT_CASE_var == CellType::GeneralField ) || ( SELECT_CASE_var == CellType::Slab ) || ( SELECT_CASE_var == CellType::HorizInsulation ) || ( SELECT_CASE_var == CellType::VertInsulation ) || ( SELECT_CASE_var == CellType::SlabOnGradeEdgeInsu ) ) {
						cell.MyBase.Temperature = EvaluateFieldCellTemperature( DomainNum, cell );
					} else if ( SELECT_CASE_var == CellType::GroundSurface ) {
						cell.MyBase.Temperature = EvaluateGroundSurfaceTemperature( DomainNum, cell );
					} else if ( SELECT_CASE_var == CellType::FarfieldBoundary ) {
						cell.MyBase.Temperature = EvaluateFarfieldBoundaryTemperature( DomainNum, cell );
					} else if ( ( SELECT_CASE_var == CellType::BasementWall ) || ( SELECT_CASE_var == CellType::BasementCorner ) || ( SELECT_CASE_var == CellType::BasementFloor ) ) {
						// basement model, zone-coupled. Call EvaluateZoneInterfaceTemperature routine to handle timestep/hourly simulation.
						if ( PipingSystemDomains( DomainNum ).HasZoneCoupledBasement ) {
							cell.MyBase.Temperature = EvaluateZoneInterfaceTemperature( DomainNum, cell );
						} else { // FHX model
							cell.MyBase.Temperature = EvaluateBasementCellTemperature( DomainNum, cell );
						}
					} else if ( SELECT_CASE_var == CellType::ZoneGroundInterface ) {
						cell.MyBase.Temperature = EvaluateZoneInterfaceTemperature( DomainNum, cell );
					}

#ifdef CalcEnergyBalance
					if ( PipingSystemDomains( DomainNum ).finalIteration ) {
						Real64 massCp = cell.MyBase.Properties.Density * cell.volume() * cell.MyBase.Properties.SpecificHeat;
						cell.MyBase.totalEnergyChange = massCp * ( cell.MyBase.Temperature_PrevIteration - cell.MyBase.Temperature_PrevTimeStep );
						cell.MyBase.energyImbalance = std::abs( cell.MyBase.totalEnergyChange - cell.MyBase.sumEnergyFromAllSides );
						cell.MyBase.tempDiffDueToImbalance = cell.MyBase.energyImbalance / massCp;
					}
#endif
					}
				}
			}
		}

	}

	Real64
	EvaluateFieldCellTemperature(
		int const DomainNum,
		CartesianCell & cell
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 Numerator = 0.0;
		Real64 Denominator = 0.0;
		Real64 AdiabaticMultiplier = 1.0;
		Real64 Beta = cell.MyBase.Beta;

		// add effect from cell history
		Numerator += cell.MyBase.Temperature_PrevTimeStep;
		++Denominator;

		// determine the neighbor types based on cell location
		int NumFieldCells = 0, NumBoundaryCells = 0;
		EvaluateCellNeighborDirections( DomainNum, cell, NumFieldCells, NumBoundaryCells );

#ifdef CalcEnergyBalance
		Real64 energyFromThisSide = 0.0;
		cell.MyBase.sumEnergyFromAllSides = 0.0;
		cell.MyBase.numberOfSidesCalculated = 0;
#endif

		// loop across each direction in the simulation
		for ( int DirectionCounter = 1; DirectionCounter <= NumFieldCells; ++DirectionCounter ) {

			Real64 NeighborTemp = 0.0;
			Real64 Resistance = 0.0;
			Direction CurDirection = NeighborFieldCells( DirectionCounter );

			//'evaluate the transient expression terms
			EvaluateNeighborCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );

			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

#ifdef CalcEnergyBalance
			if ( PipingSystemDomains( DomainNum ).finalIteration ) {
				energyFromThisSide = AdiabaticMultiplier * ( NeighborTemp - cell.MyBase.Temperature ) / ( Resistance ) * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
				cell.MyBase.sumEnergyFromAllSides += energyFromThisSide;
				cell.MyBase.numberOfSidesCalculated += int( AdiabaticMultiplier );
			}
#endif
		}

		//'now that we have passed all directions, update the temperature
		return Numerator / Denominator;

	}

	Real64
	EvaluateGroundSurfaceTemperature(
		int const DomainNum,
		CartesianCell & cell
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const AirDensity( 1.22521 ); // '[kg/m3]
		Real64 const AirSpecificHeat( 1003 ); // '[J/kg-K]
		Real64 const Absor_Corrected( 0.77 );
		Real64 const Convert_Wm2_To_MJhrmin( 3600.0 / 1000000.0 );
		Real64 const Convert_MJhrmin_To_Wm2( 1.0 / Convert_Wm2_To_MJhrmin );
		Real64 const Rho_water( 998.0 ); // [kg/m3]

		// retrieve any information from input data structure
		Real64 GroundCoverCoefficient = PipingSystemDomains( DomainNum ).Moisture.GroundCoverCoefficient;

		// initialize values
		Real64 Numerator = 0.0;
		Real64 Denominator = 0.0;
		Real64 Resistance = 0.0;
		Real64 AdiabaticMultiplier = 1.0;
		Real64 Beta = cell.MyBase.Beta;
		Real64 ThisNormalArea = cell.normalArea( Direction::PositiveY );

#ifdef CalcEnergyBalance
		Real64 energyFromThisSide = 0.0;
		cell.MyBase.sumEnergyFromAllSides = 0.0;
		cell.MyBase.numberOfSidesCalculated = 0;
#endif

		//'add effect from previous time step
		Numerator += cell.MyBase.Temperature_PrevTimeStep;
		++Denominator;

		// now that we aren't infinitesimal, we need to determine the neighbor types based on cell location
		int NumFieldCells = 0, NumBoundaryCells = 0;
		EvaluateCellNeighborDirections( DomainNum, cell, NumFieldCells, NumBoundaryCells );

		// loop over all regular neighbor cells, check if we have adiabatic on opposite surface
		for ( int DirectionCounter = 1; DirectionCounter <= NumFieldCells; ++DirectionCounter ) {
			Real64 NeighborTemp = 0.0;
			Direction CurDirection = NeighborFieldCells( DirectionCounter );
			EvaluateNeighborCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );
			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

#ifdef CalcEnergyBalance
			if ( PipingSystemDomains( DomainNum ).finalIteration ) {
				energyFromThisSide = AdiabaticMultiplier * ( NeighborTemp - cell.MyBase.Temperature ) / ( Resistance ) * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
				cell.MyBase.sumEnergyFromAllSides += energyFromThisSide;
				cell.MyBase.numberOfSidesCalculated += int( AdiabaticMultiplier );
			}
#endif

		}

		// do all non-adiabatic boundary types here
		for ( int DirectionCounter = 1; DirectionCounter <= NumBoundaryCells; ++DirectionCounter ) {
			Real64 NeighborTemp = 0.0;
			Direction CurDirection = NeighborBoundaryCells( DirectionCounter );

			// reset the adiabatic multiplier for each direction, it will be 1 unless overridden somehow
			AdiabaticMultiplier = 1.0;
			// For Zone-coupled slab or basement configuration
			if ( PipingSystemDomains( DomainNum ).HasZoneCoupledSlab || PipingSystemDomains( DomainNum ).HasZoneCoupledBasement ) {
				//-x-direction will always be a farfield boundary
				//-z will also be a farfield boundary
				//+x and +z will be handled above
				//-y will always be a neighbor cell, so it is handled above
				//+y will always be the outdoor air
				switch ( CurDirection ) {
				case Direction::NegativeX:
				case Direction::NegativeZ:
					// always farfield
					EvaluateFarfieldCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );
					break;
				case Direction::PositiveY:
					// convection at the surface
					if ( DataEnvironment::WindSpeed > 0.1 ) {
						if ( PipingSystemDomains( DomainNum ).BESTESTConstConvCoeff ) {
							Resistance = 1.0 / ( PipingSystemDomains( DomainNum ).BESTESTSurfaceConvCoefficient *  ThisNormalArea );
							NeighborTemp = PipingSystemDomains( DomainNum ).BESTESTGroundSurfTemp;
						} else {
							Resistance = 208.0 / ( AirDensity * AirSpecificHeat * DataEnvironment::WindSpeed * ThisNormalArea );
							NeighborTemp = PipingSystemDomains( DomainNum ).Cur.CurAirTemp;
						}
					} else {
						// Need to incorporate natural convection effects here
						if ( PipingSystemDomains( DomainNum ).BESTESTConstConvCoeff ) {
							Resistance = 1.0 / ( PipingSystemDomains( DomainNum ).BESTESTSurfaceConvCoefficient *  ThisNormalArea );
							NeighborTemp = PipingSystemDomains( DomainNum ).BESTESTGroundSurfTemp;
						}
					}
					break;
				case Direction::PositiveZ:
				case Direction::PositiveX:
					AdiabaticMultiplier = 0.0;
					break;
				case Direction::NegativeY:
					assert( false ); // debug error, can't get here!
				}
			} else { // FHX model
				//x-direction will always be a farfield boundary, z boundaries will be adiabatic, but can be included here
				//-y we don't handle here because -y will always be a neighbor cell, so handled above
				//+y will always be the outdoor air
				switch ( CurDirection ) {
				case Direction::PositiveX:
				case Direction::NegativeX:
				case Direction::PositiveZ:
				case Direction::NegativeZ:
					// always farfield
					EvaluateFarfieldCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );
					break;
				case Direction::PositiveY:
					// convection at the surface
					if ( DataEnvironment::WindSpeed > 0.1 ) {
						Resistance = 208.0 / ( AirDensity * AirSpecificHeat * DataEnvironment::WindSpeed * ThisNormalArea );
						NeighborTemp = PipingSystemDomains( DomainNum ).Cur.CurAirTemp;
					} else {
						// Future development should include additional natural convection effects here
					}
					break;
				case Direction::NegativeY:
					assert( false ); // debug error, can't get here!
				}
			}

			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

#ifdef CalcEnergyBalance
			if ( PipingSystemDomains( DomainNum ).finalIteration ) {
				energyFromThisSide = AdiabaticMultiplier * ( NeighborTemp - cell.MyBase.Temperature ) / ( Resistance ) * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
				cell.MyBase.sumEnergyFromAllSides += energyFromThisSide;
				cell.MyBase.numberOfSidesCalculated += int( AdiabaticMultiplier );
			}
#endif
		}

		// Initialize, this variable is used for both evapotranspiration and non-ET cases, [W]
		Real64 IncidentHeatGain = 0.0;

		// For convenience convert to Kelvin once
		Real64 CurAirTempK = PipingSystemDomains( DomainNum ).Cur.CurAirTemp + 273.15;

		// Convert input solar radiation [w/m2] into units for ET model, [MJ/hr-min]
		Real64 IncidentSolar_MJhrmin = PipingSystemDomains( DomainNum ).Cur.CurIncidentSolar * Convert_Wm2_To_MJhrmin;

		// Calculate another Q term, [MJ/hr-min]
		Real64 AbsorbedIncidentSolar_MJhrmin = Absor_Corrected * IncidentSolar_MJhrmin;

		// Calculate saturated vapor pressure, [kPa]
		Real64 VaporPressureSaturated_kPa = 0.6108 * std::exp( 17.27 * PipingSystemDomains( DomainNum ).Cur.CurAirTemp / ( PipingSystemDomains( DomainNum ).Cur.CurAirTemp + 237.3 ) );

		// Calculate actual vapor pressure, [kPa]
		Real64 VaporPressureActual_kPa = VaporPressureSaturated_kPa * PipingSystemDomains( DomainNum ).Cur.CurRelativeHumidity / 100.0;

		// Calculate another Q term, [MJ/m2-hr]
		Real64 QRAD_NL = 2.042E-10 * pow_4( CurAirTempK ) * ( 0.34 - 0.14 * std::sqrt( VaporPressureActual_kPa ) );

		// Calculate another Q term, [MJ/hr]
		Real64 NetIncidentRadiation_MJhr = AbsorbedIncidentSolar_MJhrmin - QRAD_NL;

		// ?
		Real64 const CN = 37.0;

		// Check whether there was sun
		Real64 G_hr;
		Real64 Cd;
		if ( NetIncidentRadiation_MJhr < 0.0 ) {
			G_hr = 0.5 * NetIncidentRadiation_MJhr;
			Cd = 0.96;
		} else {
			G_hr = 0.1 * NetIncidentRadiation_MJhr;
			Cd = 0.24;
		}

		Real64 Slope_S = 2503.0 * std::exp( 17.27 * PipingSystemDomains( DomainNum ).Cur.CurAirTemp / ( PipingSystemDomains( DomainNum ).Cur.CurAirTemp + 237.3 ) ) / pow_2( PipingSystemDomains( DomainNum ).Cur.CurAirTemp + 237.3 );
		Real64 Pressure = 98.0;
		Real64 PsychrometricConstant = 0.665e-3 * Pressure;

		// Evapotranspiration constant, [mm/hr]
		Real64 EvapotransFluidLoss_mmhr = ( GroundCoverCoefficient * Slope_S * ( NetIncidentRadiation_MJhr - G_hr ) + PsychrometricConstant * ( CN / CurAirTempK ) * PipingSystemDomains( DomainNum ).Cur.CurWindSpeed * ( VaporPressureSaturated_kPa - VaporPressureActual_kPa ) ) / ( Slope_S + PsychrometricConstant * ( 1 + Cd * PipingSystemDomains( DomainNum ).Cur.CurWindSpeed ) );

		// Convert units, [m/hr]
		Real64 EvapotransFluidLoss_mhr = EvapotransFluidLoss_mmhr / 1000.0;

		// Calculate latent heat, [MJ/kg]
		// Full formulation is cubic: L(T) = -0.0000614342 * T**3 + 0.00158927 * T**2 - 2.36418 * T + 2500.79[5]
		// In: Cubic fit to Table 2.1,p.16, Textbook: R.R.Rogers & M.K. Yau, A Short Course in Cloud Physics, 3e,(1989), Pergamon press
		// But a linear relation should suffice;
		// note-for now using the previous time step temperature as an approximation to help ensure stability
		Real64 LatentHeatVaporization = 2.501 - 2.361e-3 * cell.MyBase.Temperature_PrevTimeStep;

		// Calculate evapotranspiration heat loss, [MJ/m2-hr]
		Real64 EvapotransHeatLoss_MJhrmin = Rho_water * EvapotransFluidLoss_mhr * LatentHeatVaporization;

		// Convert net incident solar units, [W/m2]
		Real64 NetIncidentRadiation_Wm2 = NetIncidentRadiation_MJhr * Convert_MJhrmin_To_Wm2;

		// Convert evapotranspiration units, [W/m2]
		Real64 EvapotransHeatLoss_Wm2 = EvapotransHeatLoss_MJhrmin * Convert_MJhrmin_To_Wm2;

		// Calculate overall net heat ?gain? into the cell, [W]
		if ( PipingSystemDomains( DomainNum ).BESTESTConstConvCoeff ) {
			IncidentHeatGain = 0.0;
		} else {
			IncidentHeatGain = ( NetIncidentRadiation_Wm2 - EvapotransHeatLoss_Wm2 ) * ThisNormalArea;
		}

		// Add any solar/evapotranspiration heat gain here
		Numerator += Beta * IncidentHeatGain;

#ifdef CalcEnergyBalance
		if ( PipingSystemDomains( DomainNum ).finalIteration ) {
			energyFromThisSide = IncidentHeatGain * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
			cell.MyBase.sumEnergyFromAllSides += energyFromThisSide;
			// Don't add one here because we already accounted for this side
			// when we calculated the surface convective resistance above
			cell.MyBase.numberOfSidesCalculated += 0;
		}
#endif

		// Calculate the return temperature and leave
		return Numerator / Denominator;

	}

	Real64
	EvaluateBasementCellTemperature(
		int const DomainNum,
		CartesianCell & cell
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 Beta = 0.0;
		Real64 NeighborTemp = 0.0;
		Real64 HeatFlux = 0.0;
		Real64 Numerator = 0.0;
		Real64 Denominator = 0.0;
		Real64 Resistance = 0.0;
		Real64 AdiabaticMultiplier = 1.0;

#ifdef CalcEnergyBalance
		Real64 energyFromThisSide = 0.0;
		cell.MyBase.sumEnergyFromAllSides = 0.0;
		cell.MyBase.numberOfSidesCalculated = 0;
#endif
		{ auto const SELECT_CASE_var( cell.cellType );
		if ( ( SELECT_CASE_var == CellType::BasementWall ) || ( SELECT_CASE_var == CellType::BasementFloor ) ) {
			// This is actually only a half-cell since the basement wall slices right through the middle in one direction
			Beta = cell.MyBase.Beta / 2.0;
		} else if ( SELECT_CASE_var == CellType::BasementCorner ) {
			// This is actually only a three-quarter-cell since the basement wall slices right through the middle in both directions
			Beta = cell.MyBase.Beta * 3.0 / 4.0;
		}}

		// add effect from previous time step
		Numerator += cell.MyBase.Temperature_PrevTimeStep;
		++Denominator;

		{ auto const SELECT_CASE_var( cell.cellType );
		if ( SELECT_CASE_var == CellType::BasementWall ) {

			// we will only have heat flux from the basement wall and heat conduction to the +x cell

			// get the average basement wall heat flux and add it to the tally
			HeatFlux = PipingSystemDomains( DomainNum ).GetBasementWallHeatFlux();
			Numerator += Beta * HeatFlux * cell.height();

#ifdef CalcEnergyBalance
			if ( PipingSystemDomains( DomainNum ).finalIteration ) {
				energyFromThisSide = HeatFlux * cell.height() * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
				cell.MyBase.sumEnergyFromAllSides += energyFromThisSide;
				cell.MyBase.numberOfSidesCalculated += 1;
			}
#endif
			// then get the +x conduction to continue the heat balance
			EvaluateNeighborCharacteristics( DomainNum, cell, Direction::PositiveX, NeighborTemp, Resistance, AdiabaticMultiplier );
			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

#ifdef CalcEnergyBalance
			if ( PipingSystemDomains( DomainNum ).finalIteration ) {
				energyFromThisSide = ( NeighborTemp - cell.MyBase.Temperature ) / ( Resistance ) * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
				cell.MyBase.sumEnergyFromAllSides += energyFromThisSide;
				cell.MyBase.numberOfSidesCalculated += 1;
			}
#endif
		} else if ( SELECT_CASE_var == CellType::BasementFloor ) {

			// we will only have heat flux from the basement floor and heat conduction to the lower cell

			// get the average basement floor heat flux and add it to the tally
			HeatFlux = PipingSystemDomains( DomainNum ).GetBasementFloorHeatFlux();
			Numerator += Beta * HeatFlux * cell.width();

#ifdef CalcEnergyBalance
			if ( PipingSystemDomains( DomainNum ).finalIteration ) {
				energyFromThisSide = HeatFlux * cell.width() * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
				cell.MyBase.sumEnergyFromAllSides += energyFromThisSide;
				cell.MyBase.numberOfSidesCalculated += 1;
			}
#endif
			// then get the -y conduction to continue the heat balance
			EvaluateNeighborCharacteristics( DomainNum, cell, Direction::NegativeY, NeighborTemp, Resistance, AdiabaticMultiplier );
			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

#ifdef CalcEnergyBalance
			if ( PipingSystemDomains( DomainNum ).finalIteration ) {
				energyFromThisSide = ( NeighborTemp - cell.MyBase.Temperature ) / ( Resistance ) * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
				cell.MyBase.sumEnergyFromAllSides += energyFromThisSide;
				cell.MyBase.numberOfSidesCalculated += 1;
			}
#endif
		} else if ( SELECT_CASE_var == CellType::BasementCorner ) {

			// we will only have heat conduction to the +x and -y cells
			EvaluateNeighborCharacteristics( DomainNum, cell, Direction::PositiveX, NeighborTemp, Resistance, AdiabaticMultiplier );
			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

#ifdef CalcEnergyBalance
			if ( PipingSystemDomains( DomainNum ).finalIteration ) {
				energyFromThisSide = ( NeighborTemp - cell.MyBase.Temperature ) / ( Resistance ) * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
				cell.MyBase.sumEnergyFromAllSides += energyFromThisSide;
				cell.MyBase.numberOfSidesCalculated += 1;
			}
#endif
			EvaluateNeighborCharacteristics( DomainNum, cell, Direction::NegativeY, NeighborTemp, Resistance, AdiabaticMultiplier );
			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

#ifdef CalcEnergyBalance
			if ( PipingSystemDomains( DomainNum ).finalIteration ) {
				energyFromThisSide = ( NeighborTemp - cell.MyBase.Temperature ) / ( Resistance ) * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
				cell.MyBase.sumEnergyFromAllSides += energyFromThisSide;
				cell.MyBase.numberOfSidesCalculated += 1;
			}
#endif
		}}

		return Numerator / Denominator;

	}

	Real64
	EvaluateFarfieldBoundaryTemperature(
		int const DomainNum,
		CartesianCell & cell
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 Numerator = 0.0;
		Real64 Denominator = 0.0;
		Real64 Resistance = 0.0;
		Real64 AdiabaticMultiplier = 1.0;
		Real64 Beta = cell.MyBase.Beta;

#ifdef CalcEnergyBalance
		Real64 energyFromThisSide = 0.0;
		cell.MyBase.sumEnergyFromAllSides = 0.0;
		cell.MyBase.numberOfSidesCalculated = 0;
#endif

		// add effect from previous time step
		Numerator += cell.MyBase.Temperature_PrevTimeStep;
		++Denominator;

		// now that we aren't infinitesimal, we need to determine the neighbor types based on cell location
		int NumFieldCells = 0, NumBoundaryCells = 0;
		EvaluateCellNeighborDirections( DomainNum, cell, NumFieldCells, NumBoundaryCells );

		// Do all neighbor cells
		for ( int DirectionCounter = 1; DirectionCounter <= NumFieldCells; ++DirectionCounter ) {
			Direction CurDirection = NeighborFieldCells( DirectionCounter );

			Real64 NeighborTemp = 0.0;
			EvaluateNeighborCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );

			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

#ifdef CalcEnergyBalance
			if ( PipingSystemDomains( DomainNum ).finalIteration ) {
				energyFromThisSide = AdiabaticMultiplier * ( NeighborTemp - cell.MyBase.Temperature ) / ( Resistance  ) * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
				cell.MyBase.sumEnergyFromAllSides += energyFromThisSide;
				cell.MyBase.numberOfSidesCalculated += int( AdiabaticMultiplier );
			}
#endif
		}

		// Then all farfield boundaries
		for ( int DirectionCounter = 1; DirectionCounter <= NumBoundaryCells; ++DirectionCounter ) {
			Direction CurDirection = NeighborBoundaryCells( DirectionCounter );

			Real64 NeighborTemp = 0.0;
			EvaluateFarfieldCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );

			//if ( PipingSystemDomains( DomainNum ).HasZoneCoupledSlab || PipingSystemDomains( DomainNum ).HasZoneCoupledBasement ) {
				//if ( CurDirection == Direction::PositiveX || CurDirection == Direction::PositiveZ ) {
					//AdiabaticMultiplier = 0.0; // Do nothing. This should only apply to lower corner cell at Xmax, Ymin, Zmax
				//}
			//}

			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

#ifdef CalcEnergyBalance
			if ( PipingSystemDomains( DomainNum ).finalIteration ) {
				energyFromThisSide = AdiabaticMultiplier * ( NeighborTemp - cell.MyBase.Temperature ) / ( Resistance ) * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
				cell.MyBase.sumEnergyFromAllSides += energyFromThisSide;
				cell.MyBase.numberOfSidesCalculated += int( AdiabaticMultiplier );
			}
#endif
		}

		return Numerator / Denominator;

	}

	Real64
	EvaluateZoneInterfaceTemperature(
		int const DomainNum,
		CartesianCell & cell
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 HeatFlux = 0.0;
		Real64 ConductionArea = 0.0;
		Real64 Numerator = 0.0;
		Real64 Denominator = 0.0;
		Real64 Resistance = 0.0;
		Real64 AdiabaticMultiplier = 1.0;
		Real64 Beta = cell.MyBase.Beta;

#ifdef CalcEnergyBalance
		Real64 energyFromThisSide = 0.0;
		cell.MyBase.sumEnergyFromAllSides = 0.0;
		cell.MyBase.numberOfSidesCalculated = 0;
#endif

		// add effect from previous time step
		Numerator += cell.MyBase.Temperature_PrevTimeStep;
		++Denominator;

		// catch invalid types
		assert( std::set< CellType >( { CellType::BasementWall, CellType::BasementFloor, CellType::ZoneGroundInterface, CellType::BasementCorner } ).count( cell.cellType ) != 0 );

		if ( cell.cellType == CellType::BasementWall ) {
			// Get the average basement wall heat flux and add it to the tally
			HeatFlux = PipingSystemDomains( DomainNum ).WallHeatFlux;
			if ( cell.X_index == PipingSystemDomains( DomainNum ).XWallIndex ) {
				ConductionArea = cell.depth() * cell.height();
				Numerator += Beta * HeatFlux * ConductionArea;
			} else if ( cell.Z_index == PipingSystemDomains( DomainNum ).ZWallIndex ) {
				ConductionArea = cell.width() * cell.height();
				Numerator += Beta * HeatFlux * ConductionArea;
			}
		} else if ( cell.cellType == CellType::BasementFloor ) {
			// Get the average basement floor heat flux and add it to the tally
			HeatFlux = PipingSystemDomains( DomainNum ).FloorHeatFlux;
			ConductionArea = cell.width() * cell.depth();
			Numerator += Beta * HeatFlux * ConductionArea;
		} else if ( cell.cellType ==  CellType::ZoneGroundInterface ) {
			// Get the average slab heat flux and add it to the tally
			HeatFlux = PipingSystemDomains( DomainNum ).WeightedHeatFlux( cell.X_index, cell.Z_index );
			ConductionArea = cell.width() * cell.depth();
			Numerator += Beta * HeatFlux * ConductionArea;
		}

#ifdef CalcEnergyBalance
		if ( PipingSystemDomains( DomainNum ).finalIteration ) {
			energyFromThisSide = HeatFlux * ConductionArea * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
			cell.MyBase.sumEnergyFromAllSides += energyFromThisSide;
			cell.MyBase.numberOfSidesCalculated += 1;
		}
#endif
		//determine the neighbor types based on cell location
		int NumFieldCells = 0, NumBoundaryCells = 0;
		EvaluateCellNeighborDirections( DomainNum, cell, NumFieldCells, NumBoundaryCells );

		//loop across each direction in the simulation
		for ( int DirectionCounter = 1; DirectionCounter <= NumFieldCells; ++DirectionCounter ) {

			Real64 NeighborTemp = 0.0;
			Direction CurDirection = NeighborFieldCells( DirectionCounter );

			// Have to be careful here to make sure heat conduction happens only in the appropriate directions
			if ( cell.cellType == CellType::BasementWall ) {
				// No heat conduction from the X-side basement wall cell to the +x cell ( basement cutaway )
				if ( cell.X_index == PipingSystemDomains( DomainNum ).XWallIndex && CurDirection != Direction::PositiveX ) {
					// Evaluate the transient expression terms
					EvaluateNeighborCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );
					Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
					Denominator += AdiabaticMultiplier * ( Beta / Resistance );
				}
				// No heat conduction from the Z-side basement wall cell to the +z cell ( basement cutaway )
				if ( cell.Z_index == PipingSystemDomains( DomainNum ).ZWallIndex && CurDirection != Direction::PositiveZ ) {
					// Evaluate the transient expression terms
					EvaluateNeighborCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );
					Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
					Denominator += AdiabaticMultiplier * ( Beta / Resistance );
				}
			} else if ( cell.cellType == CellType::BasementFloor ) {
				// No heat conduction from the basement floor cell to the +y cell ( basement cutaway )
				if ( CurDirection != Direction::PositiveY ) {
					// Evaluate the transient expression terms
					EvaluateNeighborCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );
					Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
					Denominator += AdiabaticMultiplier * ( Beta / Resistance );
				}
			} else if ( cell.cellType == CellType::ZoneGroundInterface || cell.cellType == CellType::BasementCorner ) {
				// Heat conduction in all directions
				// Evaluate the transient expression terms
				EvaluateNeighborCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );
				Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
				Denominator += AdiabaticMultiplier * ( Beta / Resistance );
			}

#ifdef CalcEnergyBalance
			if ( PipingSystemDomains( DomainNum ).finalIteration ) {
				energyFromThisSide = AdiabaticMultiplier * ( NeighborTemp - cell.MyBase.Temperature ) / ( Resistance ) * PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize;
				cell.MyBase.sumEnergyFromAllSides += energyFromThisSide;
				cell.MyBase.numberOfSidesCalculated += int( AdiabaticMultiplier );
			}
#endif
		}

		return Numerator / Denominator;

	}

	Real64
	FullDomainStructureInfo::GetBasementWallHeatFlux()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		Real64 RunningSummation = 0.0;
		int NumSurfaces = size( this->BasementZone.WallSurfacePointers );
		for ( int SurfaceCounter = 1; SurfaceCounter <= NumSurfaces; ++SurfaceCounter ) {
			int SurfacePointer = this->BasementZone.WallSurfacePointers( SurfaceCounter );
			RunningSummation += DataHeatBalSurface::QdotConvOutRepPerArea( SurfacePointer );
		}
		return -RunningSummation / NumSurfaces; // heat flux is negative here

	}

	Real64
	FullDomainStructureInfo::GetBasementFloorHeatFlux()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		Real64 RunningSummation = 0.0;
		int NumSurfaces = size( this->BasementZone.FloorSurfacePointers );
		for ( int SurfaceCounter = 1; SurfaceCounter <= NumSurfaces; ++SurfaceCounter ) {
			int SurfacePointer = this->BasementZone.FloorSurfacePointers( SurfaceCounter );
			RunningSummation += DataHeatBalSurface::QdotConvOutRepPerArea( SurfacePointer );
		}
		return -RunningSummation / NumSurfaces; // heat flux is negative here

	}

	void
	FullDomainStructureInfo::UpdateBasementSurfaceTemperatures()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const BigNumber( 10000.0 );

		// First the wall
		this->BasementWallTemp = this->GetAverageTempByType( CellType::BasementWall );
		int OSCMIndex = this->BasementZone.WallBoundaryOSCMIndex;
		DataSurfaces::OSCM( OSCMIndex ).TConv = this->BasementWallTemp;
		DataSurfaces::OSCM( OSCMIndex ).HConv = BigNumber;
		DataSurfaces::OSCM( OSCMIndex ).TRad = this->BasementWallTemp;
		DataSurfaces::OSCM( OSCMIndex ).HRad = 0.0;

		// Then the floor
		this->BasementFloorTemp = this->GetAverageTempByType( CellType::BasementFloor );
		OSCMIndex = this->BasementZone.FloorBoundaryOSCMIndex;
		DataSurfaces::OSCM( OSCMIndex ).TConv = this->BasementFloorTemp;
		DataSurfaces::OSCM( OSCMIndex ).HConv = BigNumber;
		DataSurfaces::OSCM( OSCMIndex ).TRad = this->BasementFloorTemp;
		DataSurfaces::OSCM( OSCMIndex ).HRad = 0.0;

	}

	Real64
	FullDomainStructureInfo::GetZoneInterfaceHeatFlux()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		Real64 RunningSummation = 0.0;
		int NumSurfaces = size( this->ZoneCoupledSurfaces );
		for ( int SurfaceCounter = 1; SurfaceCounter <= NumSurfaces; ++SurfaceCounter ) {
			int SurfacePointer = this->ZoneCoupledSurfaces( SurfaceCounter ).IndexInSurfaceArray;
			RunningSummation += DataHeatBalSurface::QdotConvOutRepPerArea( SurfacePointer );
		}
		return -RunningSummation / NumSurfaces; // heat flux is negative here

	}

	void
	FullDomainStructureInfo::UpdateZoneSurfaceTemperatures()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR
		//       DATE WRITTEN
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const BigNumber( 10000.0 );

		this->ZoneCoupledSurfaceTemp = this->GetAverageTempByType( CellType::ZoneGroundInterface );
		int OSCMIndex = this->ZoneCoupledOSCMIndex;
		DataSurfaces::OSCM( OSCMIndex ).TConv = this->ZoneCoupledSurfaceTemp;
		DataSurfaces::OSCM( OSCMIndex ).HConv = BigNumber;
		DataSurfaces::OSCM( OSCMIndex ).TRad = this->ZoneCoupledSurfaceTemp;
		DataSurfaces::OSCM( OSCMIndex ).HRad = 0.0;

		// Reset the interface heat flux after iteration
		this->ResetHeatFluxFlag = true;

	}

	Real64
	FullDomainStructureInfo::GetAverageTempByType(
		CellType const cellType
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Return value
		Real64 RetVal;

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 RunningSummation = 0.0;
		Real64 CellVolume;
		Real64 RunningVolume = 0.0;
		Real64 AvgTemp = 0.0;
		int NumCells = 0;

		auto const & cells( Cells );
		for ( int X = cells.l1(), X_end = cells.u1(); X <= X_end; ++X ) {
			for ( int Y = cells.l2(), Y_end = cells.u2(); Y <= Y_end; ++Y ) {
				for ( int Z = cells.l3(), Z_end = cells.u3(); Z <= Z_end; ++Z ) {
					auto const & cell( cells( X, Y, Z ) );
					if ( cell.cellType == cellType ) {
						CellVolume = cell.volume();
						RunningVolume += CellVolume;
						RunningSummation += CellVolume * cell.MyBase.Temperature;
						AvgTemp += cell.MyBase.Temperature;
						NumCells += 1;
					}
				}
			}
		}

#ifdef CalcEnergyBalance
		this->AvgUnweightedSurfTemp = AvgTemp / NumCells;
#endif

		if ( RunningVolume > 0.0 ) {
			RetVal = RunningSummation / RunningVolume;
		} else {
			// ERROR!!!
			RetVal = 0.0; //Autodesk:Return Line added to assure return value is set: Proper error handling needed here!
		}

		return RetVal;

	}

	void
	EvaluateFarfieldCharacteristics(
		int const DomainNum,
		CartesianCell const & cell,
		Direction const direction,
		Real64 & neighbortemp,
		Real64 & resistance,
		Real64 & adiabaticMultiplier
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 distance( 0.0 );

		if ( ( direction == Direction::NegativeX ) || ( direction == Direction::PositiveX ) ) {
			distance = ( cell.width() / 2.0 );
		} else if ( ( direction == Direction::NegativeY ) || ( direction == Direction::PositiveY ) ) {
			distance = ( cell.height() / 2.0 );
		} else if ( ( direction == Direction::NegativeZ ) || ( direction == Direction::PositiveZ ) ) {
			distance = ( cell.depth() / 2.0 );
		} else {
			assert( false );
		}

		resistance = ( distance / 2.0 ) / ( cell.MyBase.Properties.Conductivity * cell.normalArea( direction ) );
		neighbortemp = GetFarfieldTemp( DomainNum, cell );

		auto const & TempNeighborInfo = NeighborInformationArray_Value( cell.NeighborInformation, direction );
		adiabaticMultiplier = TempNeighborInfo.adiabaticMultiplier;
	}

	Real64
	GetFarfieldTemp(
		int const DomainNum,
		CartesianCell const & cell
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		Real64 CurTime = PipingSystemDomains( DomainNum ).Cur.CurSimTimeSeconds;
		Real64 z = PipingSystemDomains( DomainNum ).Extents.Ymax - cell.Centroid.Y;
		return PipingSystemDomains( DomainNum ).Farfield.groundTempModel->getGroundTempAtTimeInSeconds( z, CurTime );

	}

	void
	PreparePipeCircuitSimulation(
		int const DomainNum,
		int const CircuitNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		Real64 const StagnantFluidConvCoeff( 200.0 );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Density;
		Real64 Viscosity;
		Real64 Conductivity;
		Real64 Prandtl;
		Real64 Area_c;
		Real64 Velocity;
		Real64 ConvCoefficient;
		Real64 Reynolds;
		Real64 ExponentTerm;
		Real64 Nusselt;
		Real64 SpecificHeat;
		int CellX;
		int CellY;
		int CellZ;

		// Setup circuit flow conditions -- convection coefficient
		CellX = PipingSystemCircuits( CircuitNum ).CircuitInletCell.X;
		CellY = PipingSystemCircuits( CircuitNum ).CircuitInletCell.Y;
		CellZ = PipingSystemCircuits( CircuitNum ).CircuitInletCell.Z;

		// Look up current fluid properties
		Density = PipingSystemCircuits( CircuitNum ).CurFluidPropertySet.MyBase.Density;
		Viscosity = PipingSystemCircuits( CircuitNum ).CurFluidPropertySet.Viscosity;
		Conductivity = PipingSystemCircuits( CircuitNum ).CurFluidPropertySet.MyBase.Conductivity;
		Prandtl = PipingSystemCircuits( CircuitNum ).CurFluidPropertySet.Prandtl;
		SpecificHeat = PipingSystemCircuits( CircuitNum ).CurFluidPropertySet.MyBase.SpecificHeat;

		// Flow calculations
		Area_c = ( Pi / 4.0 ) * pow_2( PipingSystemCircuits( CircuitNum ).PipeSize.InnerDia );
		Velocity = PipingSystemCircuits( CircuitNum ).CurCircuitFlowRate / ( Density * Area_c );

		// Determine convection coefficient based on flow conditions
		if ( Velocity > 0 ) {
			Reynolds = Density * PipingSystemCircuits( CircuitNum ).PipeSize.InnerDia * Velocity / Viscosity;
			if ( PipingSystemDomains( DomainNum ).Cells( CellX, CellY, CellZ ).PipeCellData.Fluid.MyBase.Temperature > PipingSystemDomains( DomainNum ).Cells( CellX, CellY, CellZ ).PipeCellData.Pipe.MyBase.Temperature ) {
				ExponentTerm = 0.3;
			} else {
				ExponentTerm = 0.4;
			}
			Nusselt = 0.023 * std::pow( Reynolds, 4.0 / 5.0 ) * std::pow( Prandtl, ExponentTerm );
			ConvCoefficient = Nusselt * Conductivity / PipingSystemCircuits( DomainNum ).PipeSize.InnerDia;
		} else {
			ConvCoefficient = StagnantFluidConvCoeff;
		}

		// Assign the convection coefficient
		PipingSystemCircuits( CircuitNum ).CurCircuitConvectionCoefficient = ConvCoefficient;

	}

	void
	PerformPipeCircuitSimulation(
		int const DomainNum,
		int const CircuitNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 CircuitCrossTemp;
		Real64 FlowRate;
		Real64 EnteringTemp;
		int SegmentCtr;
		int SegmentCellCtr;
		int StartingSegment;
		int EndingSegment;
		int StartingZ;
		int EndingZ;
		int Increment;
		int PipeX;
		int PipeY;
		int Zindex;
		int SegmentIndex;

		// retrieve initial conditions from the data structure
		// these have been set either by the init routine or by the heat pump routine
		FlowRate = PipingSystemCircuits( CircuitNum ).CurCircuitFlowRate;
		EnteringTemp = PipingSystemCircuits( CircuitNum ).CurCircuitInletTemp;

		// initialize
		SegmentCellCtr = 0;
		StartingSegment = PipingSystemCircuits( CircuitNum ).PipeSegmentIndeces.l1();
		EndingSegment = PipingSystemCircuits( CircuitNum ).PipeSegmentIndeces.u1();

		//'loop across all segments (pipes) of the circuit
		auto & cells( PipingSystemDomains( DomainNum ).Cells );
		for ( SegmentCtr = StartingSegment; SegmentCtr <= EndingSegment; ++SegmentCtr ) {

			SegmentIndex = PipingSystemCircuits( CircuitNum ).PipeSegmentIndeces( SegmentCtr );

			//'set simulation flow direction
			switch ( PipingSystemSegments( SegmentIndex ).FlowDirection ) {
			case SegmentFlow::IncreasingZ:
				StartingZ = 0;
				EndingZ = cells.u3();
				Increment = 1;
				break;
			case SegmentFlow::DecreasingZ:
				StartingZ = cells.u3();
				EndingZ = 0;
				Increment = -1;
				break;
			default:
				ShowFatalError( "Debug error: invalid flow direction on piping system segment" );
			}

			//'find the cell we are working on in order to retrieve cell and neighbor information
			PipeX = PipingSystemSegments( SegmentIndex ).PipeCellCoordinates.X;
			PipeY = PipingSystemSegments( SegmentIndex ).PipeCellCoordinates.Y;

			//'loop across all z-direction indeces
			int const Zindex_stop( floop_end( StartingZ, EndingZ, Increment ) );
			for ( Zindex = StartingZ; Zindex != Zindex_stop; Zindex += Increment ) {

				//'overall cell segment counter
				++SegmentCellCtr;

				if ( SegmentCellCtr == 1 ) {
					//'we have the very first cell, need to pass in circuiting entering temperature
					PerformPipeCellSimulation( DomainNum, CircuitNum, cells( PipeX, PipeY, Zindex ), FlowRate, EnteringTemp );
				} else {
					//'we don't have the first cell so just normal simulation
					if ( Zindex == EndingZ ) {
						// simulate current cell using upstream as entering conditions
						PerformPipeCellSimulation( DomainNum, CircuitNum, cells( PipeX, PipeY, Zindex ), FlowRate, cells( PipeX, PipeY, Zindex - Increment ).PipeCellData.Fluid.MyBase.Temperature );
						// store this outlet condition to be passed to the next segment
						CircuitCrossTemp = cells( PipeX, PipeY, Zindex ).PipeCellData.Fluid.MyBase.Temperature;
					} else if ( Zindex == StartingZ ) {
						// we are starting another segment, use the previous cross temperature
						PerformPipeCellSimulation( DomainNum, CircuitNum, cells( PipeX, PipeY, Zindex ), FlowRate, CircuitCrossTemp );
					} else {
						// we are in an interior node, so just get the upstream cell and use the main simulation
						PerformPipeCellSimulation( DomainNum, CircuitNum, cells( PipeX, PipeY, Zindex ), FlowRate, cells( PipeX, PipeY, Zindex - Increment ).PipeCellData.Fluid.MyBase.Temperature );
					}
				}

				// Bookkeeping: segment fluid temperature updates
				if ( Zindex == StartingZ ) {
					if ( SegmentCtr == StartingSegment ) {
						PipingSystemSegments( SegmentIndex ).InletTemperature = EnteringTemp;
					} else {
						PipingSystemSegments( SegmentIndex ).InletTemperature = CircuitCrossTemp;
					}
				} else if ( Zindex == EndingZ ) {
					PipingSystemSegments( SegmentIndex ).OutletTemperature = cells( PipeX, PipeY, Zindex ).PipeCellData.Fluid.MyBase.Temperature;
					PipingSystemSegments( SegmentIndex ).FluidHeatLoss = FlowRate * PipingSystemCircuits( CircuitNum ).CurFluidPropertySet.MyBase.SpecificHeat * ( PipingSystemSegments( SegmentIndex ).InletTemperature - PipingSystemSegments( SegmentIndex ).OutletTemperature );
				}

				// Bookkeeping: circuit fluid temperature updates
				if ( ( SegmentCtr == StartingSegment ) && ( Zindex == StartingZ ) ) {
					PipingSystemCircuits( CircuitNum ).InletTemperature = EnteringTemp;
				} else if ( ( SegmentCtr == EndingSegment ) && ( Zindex == EndingZ ) ) {
					PipingSystemCircuits( CircuitNum ).OutletTemperature = cells( PipeX, PipeY, Zindex ).PipeCellData.Fluid.MyBase.Temperature;
					PipingSystemCircuits( CircuitNum ).FluidHeatLoss = FlowRate * PipingSystemCircuits( CircuitNum ).CurFluidPropertySet.MyBase.SpecificHeat * ( PipingSystemCircuits( CircuitNum ).InletTemperature - PipingSystemCircuits( CircuitNum ).OutletTemperature );
				}

			}

		}

	}

	void
	PerformPipeCellSimulation(
		int const DomainNum,
		int const CircuitNum,
		CartesianCell & ThisCell,
		Real64 const FlowRate,
		Real64 const EnteringTemp
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Iter;
		Real64 MaxDeviationAmount;

		for ( Iter = 1; Iter <= PipingSystemCircuits( CircuitNum ).MaxIterationsPerTS; ++Iter ) {

			//'shift all the pipe related temperatures for the next internal pipe iteration
			ShiftPipeTemperaturesForNewIteration( ThisCell );

			//'simulate the funny interface soil cell between the radial and cartesian systems
			SimulateRadialToCartesianInterface( DomainNum, ThisCell );

			//'simulate the outermost radial slice
			SimulateOuterMostRadialSoilSlice( CircuitNum, ThisCell );

			//'we only need to simulate these if they actually exist!
			if ( size( ThisCell.PipeCellData.Soil ) > 1 ) {

				//'simulate all interior radial slices
				SimulateAllInteriorRadialSoilSlices( ThisCell );

				//'simulate the innermost radial soil slice
				SimulateInnerMostRadialSoilSlice( CircuitNum, ThisCell );

			}

			if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
				SimulateRadialInsulationCell( ThisCell );
			}

			//'simulate the pipe cell
			SimulateRadialPipeCell( CircuitNum, ThisCell, PipingSystemCircuits( CircuitNum ).CurCircuitConvectionCoefficient );

			//'simulate the water cell
			SimulateFluidCell( ThisCell, FlowRate, PipingSystemCircuits( CircuitNum ).CurCircuitConvectionCoefficient, EnteringTemp );

			//'check convergence
			if ( IsConverged_PipeCurrentToPrevIteration( DomainNum, ThisCell, MaxDeviationAmount ) ) break;

		}

	}

	void
	SimulateRadialToCartesianInterface(
		int const DomainNum,
		CartesianCell & cell
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D< Direction > const Directions( 4, { Direction::NegativeX, Direction::NegativeY, Direction::PositiveX, Direction::PositiveY } );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Numerator;
		Real64 Denominator;
		Real64 Resistance;
		Real64 Beta;
		int DirectionCounter;
		Direction CurDirection;
		Real64 NeighborTemp;
		Real64 OutermostRadialCellOuterRadius;
		Real64 OutermostRadialCellRadialCentroid;
		Real64 OutermostRadialCellTemperature;
		Real64 AdiabaticMultiplier;

		Numerator = 0.0;
		Denominator = 0.0;
		AdiabaticMultiplier = 1.0;

		//'retrieve beta
		Beta = cell.MyBase.Beta;

		//'add effects from this cell history
		Numerator += cell.MyBase.Temperature_PrevTimeStep;
		++Denominator;

		//'add effects from outermost radial cell
		OutermostRadialCellOuterRadius = cell.PipeCellData.Soil( cell.PipeCellData.Soil.u1() ).OuterRadius;
		OutermostRadialCellRadialCentroid = cell.PipeCellData.Soil( cell.PipeCellData.Soil.u1() ).RadialCentroid;
		OutermostRadialCellTemperature = cell.PipeCellData.Soil( cell.PipeCellData.Soil.u1() ).MyBase.Temperature;
		Resistance = std::log( OutermostRadialCellOuterRadius / OutermostRadialCellRadialCentroid ) / ( 2.0 * Pi * cell.depth() * cell.MyBase.Properties.Conductivity );
		Numerator += ( Beta / Resistance ) * OutermostRadialCellTemperature;
		Denominator += ( Beta / Resistance );

		//'add effects from neighboring Cartesian cells
		for ( DirectionCounter = Directions.l1(); DirectionCounter <= Directions.u1(); ++DirectionCounter ) {
			CurDirection = Directions( DirectionCounter );

			//'get info about cartesian neighbors
			EvaluateNeighborCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );

			//'add to the numerator and denominator expressions
			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

		}

		//'calculate the new temperature
		cell.MyBase.Temperature = Numerator / Denominator;

	}

	void
	SimulateOuterMostRadialSoilSlice(
		int const CircuitNum,
		CartesianCell & cell
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Numerator;
		Real64 Denominator;
		Real64 Resistance;
		Real64 Beta;
		int MaxRadialIndex;
		Real64 ThisRadialCellOuterRadius;
		Real64 ThisRadialCellRadialCentroid;
		Real64 ThisRadialCellConductivity;
		Real64 ThisRadialCellInnerRadius;
		Real64 ThisRadialCellTemperature_PrevTimeStep;
		Real64 ThisRadialCellTemperature;
		Real64 NextOuterRadialCellOuterRadius;
		Real64 NextOuterRadialCellRadialCentroid;
		Real64 NextOuterRadialCellConductivity;
		Real64 NextOuterRadialCellInnerRadius;
		Real64 NextOuterRadialCellTemperature;

		Numerator = 0.0;
		Denominator = 0.0;
		Resistance = 0.0;

		//'convenience variables
		MaxRadialIndex = cell.PipeCellData.Soil.u1();
		ThisRadialCellOuterRadius = cell.PipeCellData.Soil( MaxRadialIndex ).OuterRadius;
		ThisRadialCellRadialCentroid = cell.PipeCellData.Soil( MaxRadialIndex ).RadialCentroid;
		ThisRadialCellConductivity = cell.PipeCellData.Soil( MaxRadialIndex ).MyBase.Properties.Conductivity;
		ThisRadialCellInnerRadius = cell.PipeCellData.Soil( MaxRadialIndex ).InnerRadius;
		ThisRadialCellTemperature_PrevTimeStep = cell.PipeCellData.Soil( MaxRadialIndex ).MyBase.Temperature_PrevTimeStep;
		ThisRadialCellTemperature = cell.PipeCellData.Soil( MaxRadialIndex ).MyBase.Temperature;
		if ( size( cell.PipeCellData.Soil ) == 1 ) {
			if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
				NextOuterRadialCellOuterRadius = cell.PipeCellData.Insulation.OuterRadius;
				NextOuterRadialCellRadialCentroid = cell.PipeCellData.Insulation.RadialCentroid;
				NextOuterRadialCellConductivity = cell.PipeCellData.Insulation.MyBase.Properties.Conductivity;
				NextOuterRadialCellInnerRadius = cell.PipeCellData.Insulation.InnerRadius;
				NextOuterRadialCellTemperature = cell.PipeCellData.Insulation.MyBase.Temperature;
			} else {
				NextOuterRadialCellOuterRadius = cell.PipeCellData.Pipe.OuterRadius;
				NextOuterRadialCellRadialCentroid = cell.PipeCellData.Pipe.RadialCentroid;
				NextOuterRadialCellConductivity = cell.PipeCellData.Pipe.MyBase.Properties.Conductivity;
				NextOuterRadialCellInnerRadius = cell.PipeCellData.Pipe.InnerRadius;
				NextOuterRadialCellTemperature = cell.PipeCellData.Pipe.MyBase.Temperature;
			}
		} else {
			NextOuterRadialCellOuterRadius = cell.PipeCellData.Soil( MaxRadialIndex - 1 ).OuterRadius;
			NextOuterRadialCellRadialCentroid = cell.PipeCellData.Soil( MaxRadialIndex - 1 ).RadialCentroid;
			NextOuterRadialCellConductivity = cell.PipeCellData.Soil( MaxRadialIndex - 1 ).MyBase.Properties.Conductivity;
			NextOuterRadialCellInnerRadius = cell.PipeCellData.Soil( MaxRadialIndex - 1 ).InnerRadius;
			NextOuterRadialCellTemperature = cell.PipeCellData.Soil( MaxRadialIndex - 1 ).MyBase.Temperature;
		}

		//'any broadly defined variables
		Beta = cell.PipeCellData.Soil( MaxRadialIndex ).MyBase.Beta;

		//'add effects from this cell history
		Numerator += ThisRadialCellTemperature_PrevTimeStep;
		++Denominator;

		//'add effects from interface cell
		Resistance = std::log( ThisRadialCellOuterRadius / ThisRadialCellRadialCentroid ) / ( 2 * Pi * cell.depth() * ThisRadialCellConductivity );
		Numerator += ( Beta / Resistance ) * cell.MyBase.Temperature;
		Denominator += ( Beta / Resistance );

		//'add effects from inner radial cell
		Resistance = ( std::log( ThisRadialCellRadialCentroid / ThisRadialCellInnerRadius ) / ( 2 * Pi * cell.depth() * ThisRadialCellConductivity ) ) + ( std::log( NextOuterRadialCellOuterRadius / NextOuterRadialCellRadialCentroid ) / ( 2 * Pi * cell.depth() * NextOuterRadialCellConductivity ) );
		Numerator += ( Beta / Resistance ) * NextOuterRadialCellTemperature;
		Denominator += ( Beta / Resistance );

		//'calculate the new temperature
		cell.PipeCellData.Soil( MaxRadialIndex ).MyBase.Temperature = Numerator / Denominator;

	}

	void
	SimulateAllInteriorRadialSoilSlices(
		CartesianCell & cell
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Numerator;
		Real64 Denominator;
		Real64 Resistance;
		Real64 Beta;
		int rCtr;

		Real64 ThisRadialCellOuterRadius;
		Real64 ThisRadialCellRadialCentroid;
		Real64 ThisRadialCellConductivity;
		Real64 ThisRadialCellInnerRadius;
		Real64 ThisRadialCellTemperature_PrevTimeStep;
		Real64 ThisRadialCellTemperature;

		Real64 InnerRadialCellOuterRadius;
		Real64 InnerRadialCellRadialCentroid;
		Real64 InnerRadialCellConductivity;
		Real64 InnerRadialCellInnerRadius;
		Real64 InnerRadialCellTemperature;

		Real64 OuterRadialCellOuterRadius;
		Real64 OuterRadialCellRadialCentroid;
		Real64 OuterRadialCellConductivity;
		Real64 OuterRadialCellInnerRadius;
		Real64 OuterRadialCellTemperature;

		Numerator = 0.0;
		Denominator = 0.0;

		for ( rCtr = cell.PipeCellData.Soil.u1() - 1; rCtr >= 1; --rCtr ) {

			Numerator = 0.0;
			Denominator = 0.0;
			Resistance = 0.0;

			//'convenience variables
			ThisRadialCellOuterRadius = cell.PipeCellData.Soil( rCtr ).OuterRadius;
			ThisRadialCellRadialCentroid = cell.PipeCellData.Soil( rCtr ).RadialCentroid;
			ThisRadialCellConductivity = cell.PipeCellData.Soil( rCtr ).MyBase.Properties.Conductivity;
			ThisRadialCellInnerRadius = cell.PipeCellData.Soil( rCtr ).InnerRadius;
			ThisRadialCellTemperature_PrevTimeStep = cell.PipeCellData.Soil( rCtr ).MyBase.Temperature_PrevTimeStep;
			ThisRadialCellTemperature = cell.PipeCellData.Soil( rCtr ).MyBase.Temperature;

			InnerRadialCellOuterRadius = cell.PipeCellData.Soil( rCtr - 1 ).OuterRadius;
			InnerRadialCellRadialCentroid = cell.PipeCellData.Soil( rCtr - 1 ).RadialCentroid;
			InnerRadialCellConductivity = cell.PipeCellData.Soil( rCtr - 1 ).MyBase.Properties.Conductivity;
			InnerRadialCellInnerRadius = cell.PipeCellData.Soil( rCtr - 1 ).InnerRadius;
			InnerRadialCellTemperature = cell.PipeCellData.Soil( rCtr - 1 ).MyBase.Temperature;

			OuterRadialCellOuterRadius = cell.PipeCellData.Soil( rCtr + 1 ).OuterRadius;
			OuterRadialCellRadialCentroid = cell.PipeCellData.Soil( rCtr + 1 ).RadialCentroid;
			OuterRadialCellConductivity = cell.PipeCellData.Soil( rCtr + 1 ).MyBase.Properties.Conductivity;
			OuterRadialCellInnerRadius = cell.PipeCellData.Soil( rCtr + 1 ).InnerRadius;
			OuterRadialCellTemperature = cell.PipeCellData.Soil( rCtr + 1 ).MyBase.Temperature;

			//'any broadly defined variables
			Beta = cell.PipeCellData.Soil( rCtr ).MyBase.Beta;

			//'add effects from this cell history
			Numerator += ThisRadialCellTemperature_PrevTimeStep;
			++Denominator;

			//'add effects from outer cell
			Resistance = ( std::log( OuterRadialCellRadialCentroid / OuterRadialCellInnerRadius ) / ( 2 * Pi * cell.depth() * OuterRadialCellConductivity ) ) + ( std::log( ThisRadialCellOuterRadius / ThisRadialCellRadialCentroid ) / ( 2 * Pi * cell.depth() * ThisRadialCellConductivity ) );
			Numerator += ( Beta / Resistance ) * OuterRadialCellTemperature;
			Denominator += ( Beta / Resistance );

			//'add effects from inner cell
			Resistance = ( std::log( ThisRadialCellRadialCentroid / ThisRadialCellInnerRadius ) / ( 2 * Pi * cell.depth() * ThisRadialCellConductivity ) ) + ( std::log( InnerRadialCellOuterRadius / InnerRadialCellRadialCentroid ) / ( 2 * Pi * cell.depth() * InnerRadialCellConductivity ) );
			Numerator += ( Beta / Resistance ) * InnerRadialCellTemperature;
			Denominator += ( Beta / Resistance );

			//'calculate the new temperature
			cell.PipeCellData.Soil( rCtr ).MyBase.Temperature = Numerator / Denominator;

		}

	}

	void
	SimulateInnerMostRadialSoilSlice(
		int const CircuitNum,
		CartesianCell & cell
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Numerator;
		Real64 Denominator;
		Real64 Resistance;
		Real64 Beta;

		Real64 ThisRadialCellOuterRadius;
		Real64 ThisRadialCellRadialCentroid;
		Real64 ThisRadialCellConductivity;
		Real64 ThisRadialCellInnerRadius;
		Real64 ThisRadialCellTemperature_PrevTimeStep;
		Real64 ThisRadialCellTemperature;

		Real64 InnerNeighborRadialCellOuterRadius;
		Real64 InnerNeighborRadialCellRadialCentroid;
		Real64 InnerNeighborRadialCellConductivity;
		Real64 InnerNeighborRadialCellInnerRadius;
		Real64 InnerNeighborRadialCellTemperature;

		Real64 OuterNeighborRadialCellOuterRadius;
		Real64 OuterNeighborRadialCellRadialCentroid;
		Real64 OuterNeighborRadialCellConductivity;
		Real64 OuterNeighborRadialCellInnerRadius;
		Real64 OuterNeighborRadialCellTemperature;

		Numerator = 0.0;
		Denominator = 0.0;

		//'convenience variables
		if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
			InnerNeighborRadialCellOuterRadius = cell.PipeCellData.Insulation.OuterRadius;
			InnerNeighborRadialCellRadialCentroid = cell.PipeCellData.Insulation.RadialCentroid;
			InnerNeighborRadialCellConductivity = cell.PipeCellData.Insulation.MyBase.Properties.Conductivity;
			InnerNeighborRadialCellInnerRadius = cell.PipeCellData.Insulation.InnerRadius;
			InnerNeighborRadialCellTemperature = cell.PipeCellData.Insulation.MyBase.Temperature;
		} else {
			InnerNeighborRadialCellOuterRadius = cell.PipeCellData.Pipe.OuterRadius;
			InnerNeighborRadialCellRadialCentroid = cell.PipeCellData.Pipe.RadialCentroid;
			InnerNeighborRadialCellConductivity = cell.PipeCellData.Pipe.MyBase.Properties.Conductivity;
			InnerNeighborRadialCellInnerRadius = cell.PipeCellData.Pipe.InnerRadius;
			InnerNeighborRadialCellTemperature = cell.PipeCellData.Pipe.MyBase.Temperature;
		}

		ThisRadialCellOuterRadius = cell.PipeCellData.Soil( 0 ).OuterRadius;
		ThisRadialCellRadialCentroid = cell.PipeCellData.Soil( 0 ).RadialCentroid;
		ThisRadialCellConductivity = cell.PipeCellData.Soil( 0 ).MyBase.Properties.Conductivity;
		ThisRadialCellInnerRadius = cell.PipeCellData.Soil( 0 ).InnerRadius;
		ThisRadialCellTemperature_PrevTimeStep = cell.PipeCellData.Soil( 0 ).MyBase.Temperature_PrevTimeStep;
		ThisRadialCellTemperature = cell.PipeCellData.Soil( 0 ).MyBase.Temperature;

		OuterNeighborRadialCellOuterRadius = cell.PipeCellData.Soil( 1 ).OuterRadius;
		OuterNeighborRadialCellRadialCentroid = cell.PipeCellData.Soil( 1 ).RadialCentroid;
		OuterNeighborRadialCellConductivity = cell.PipeCellData.Soil( 1 ).MyBase.Properties.Conductivity;
		OuterNeighborRadialCellInnerRadius = cell.PipeCellData.Soil( 1 ).InnerRadius;
		OuterNeighborRadialCellTemperature = cell.PipeCellData.Soil( 1 ).MyBase.Temperature;

		//'any broadly defined variables
		Beta = cell.PipeCellData.Soil( 0 ).MyBase.Beta;

		//'add effects from this cell history
		Numerator += ThisRadialCellTemperature_PrevTimeStep;
		++Denominator;

		//'add effects from outer radial cell
		Resistance = ( std::log( OuterNeighborRadialCellRadialCentroid / OuterNeighborRadialCellInnerRadius ) / ( 2 * Pi * cell.depth() * OuterNeighborRadialCellConductivity ) ) + ( std::log( ThisRadialCellOuterRadius / ThisRadialCellRadialCentroid ) / ( 2 * Pi * cell.depth() * ThisRadialCellConductivity ) );
		Numerator += ( Beta / Resistance ) * OuterNeighborRadialCellTemperature;
		Denominator += ( Beta / Resistance );

		//'add effects from pipe cell
		Resistance = ( std::log( ThisRadialCellRadialCentroid / ThisRadialCellInnerRadius ) / ( 2 * Pi * cell.depth() * ThisRadialCellConductivity ) ) + ( std::log( InnerNeighborRadialCellOuterRadius / InnerNeighborRadialCellRadialCentroid ) / ( 2 * Pi * cell.depth() * InnerNeighborRadialCellConductivity ) );
		Numerator += ( Beta / Resistance ) * InnerNeighborRadialCellTemperature;
		Denominator += ( Beta / Resistance );

		//'calculate the new temperature
		cell.PipeCellData.Soil( 0 ).MyBase.Temperature = Numerator / Denominator;

	}

	void
	SimulateRadialInsulationCell(
		CartesianCell & cell
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Numerator;
		Real64 Denominator;
		Real64 Resistance;
		Real64 Beta;

		// Object Data
		RadialCellInformation PipeCell;
		RadialCellInformation ThisInsulationCell;
		RadialCellInformation NextInnerRadialCell;

		Numerator = 0.0;
		Denominator = 0.0;

		//'convenience variables
		PipeCell = cell.PipeCellData.Pipe;
		ThisInsulationCell = cell.PipeCellData.Insulation;
		NextInnerRadialCell = cell.PipeCellData.Soil( 0 );

		//'any broadly defined variables
		Beta = ThisInsulationCell.MyBase.Beta;

		//'add effects from this cell history
		Numerator += ThisInsulationCell.MyBase.Temperature_PrevTimeStep;
		++Denominator;

		//'add effects from outer radial cell
		Resistance = ( std::log( NextInnerRadialCell.RadialCentroid / NextInnerRadialCell.InnerRadius ) / ( 2 * Pi * cell.depth() * NextInnerRadialCell.MyBase.Properties.Conductivity ) ) + ( std::log( ThisInsulationCell.OuterRadius / ThisInsulationCell.RadialCentroid ) / ( 2 * Pi * cell.depth() * ThisInsulationCell.MyBase.Properties.Conductivity ) );
		Numerator += ( Beta / Resistance ) * NextInnerRadialCell.MyBase.Temperature;
		Denominator += ( Beta / Resistance );

		//'add effects from pipe cell
		Resistance = ( std::log( ThisInsulationCell.RadialCentroid / ThisInsulationCell.InnerRadius ) / ( 2 * Pi * cell.depth() * ThisInsulationCell.MyBase.Properties.Conductivity ) ) + ( std::log( PipeCell.OuterRadius / PipeCell.RadialCentroid ) / ( 2 * Pi * cell.depth() * PipeCell.MyBase.Properties.Conductivity ) );
		Numerator += ( Beta / Resistance ) * PipeCell.MyBase.Temperature;
		Denominator += ( Beta / Resistance );

		//'calculate the new temperature
		cell.PipeCellData.Insulation.MyBase.Temperature = Numerator / Denominator;

	}

	void
	SimulateRadialPipeCell(
		int const CircuitNum,
		CartesianCell & cell,
		Real64 const ConvectionCoefficient
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Numerator;
		Real64 Denominator;
		Real64 Resistance;
		Real64 Beta;
		Real64 PipeConductionResistance;
		Real64 ConvectiveResistance;

		Real64 ThisPipeCellOuterRadius;
		Real64 ThisPipeCellRadialCentroid;
		Real64 ThisPipeCellConductivity;
		Real64 ThisPipeCellInnerRadius;
		Real64 ThisPipeCellTemperature_PrevTimeStep;
		Real64 ThisPipeCellTemperature;

		Real64 FluidCellTemperature;

		Real64 OuterNeighborRadialCellOuterRadius;
		Real64 OuterNeighborRadialCellRadialCentroid;
		Real64 OuterNeighborRadialCellConductivity;
		Real64 OuterNeighborRadialCellInnerRadius;
		Real64 OuterNeighborRadialCellTemperature;

		Numerator = 0.0;
		Denominator = 0.0;
		Resistance = 0.0;

		//'convenience variables
		if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
			OuterNeighborRadialCellOuterRadius = cell.PipeCellData.Insulation.OuterRadius;
			OuterNeighborRadialCellRadialCentroid = cell.PipeCellData.Insulation.RadialCentroid;
			OuterNeighborRadialCellConductivity = cell.PipeCellData.Insulation.MyBase.Properties.Conductivity;
			OuterNeighborRadialCellInnerRadius = cell.PipeCellData.Insulation.InnerRadius;
			OuterNeighborRadialCellTemperature = cell.PipeCellData.Insulation.MyBase.Temperature;
		} else {
			OuterNeighborRadialCellOuterRadius = cell.PipeCellData.Soil( 0 ).OuterRadius;
			OuterNeighborRadialCellRadialCentroid = cell.PipeCellData.Soil( 0 ).RadialCentroid;
			OuterNeighborRadialCellConductivity = cell.PipeCellData.Soil( 0 ).MyBase.Properties.Conductivity;
			OuterNeighborRadialCellInnerRadius = cell.PipeCellData.Soil( 0 ).InnerRadius;
			OuterNeighborRadialCellTemperature = cell.PipeCellData.Soil( 0 ).MyBase.Temperature;
		}

		ThisPipeCellOuterRadius = cell.PipeCellData.Pipe.OuterRadius;
		ThisPipeCellRadialCentroid = cell.PipeCellData.Pipe.RadialCentroid;
		ThisPipeCellConductivity = cell.PipeCellData.Pipe.MyBase.Properties.Conductivity;
		ThisPipeCellInnerRadius = cell.PipeCellData.Pipe.InnerRadius;
		ThisPipeCellTemperature_PrevTimeStep = cell.PipeCellData.Pipe.MyBase.Temperature_PrevTimeStep;
		ThisPipeCellTemperature = cell.PipeCellData.Pipe.MyBase.Temperature;

		FluidCellTemperature = cell.PipeCellData.Fluid.MyBase.Temperature;

		//'any broadly defined variables
		Beta = cell.PipeCellData.Pipe.MyBase.Beta;

		//'add effects from this cell history
		Numerator += ThisPipeCellTemperature_PrevTimeStep;
		++Denominator;

		//'add effects from outer radial cell
		Resistance = ( std::log( OuterNeighborRadialCellRadialCentroid / OuterNeighborRadialCellInnerRadius ) / ( 2 * Pi * cell.depth() * OuterNeighborRadialCellConductivity ) ) + ( std::log( ThisPipeCellOuterRadius / ThisPipeCellRadialCentroid ) / ( 2 * Pi * cell.depth() * ThisPipeCellConductivity ) );
		Numerator += ( Beta / Resistance ) * OuterNeighborRadialCellTemperature;
		Denominator += ( Beta / Resistance );

		//'add effects from water cell
		PipeConductionResistance = std::log( ThisPipeCellRadialCentroid / ThisPipeCellInnerRadius ) / ( 2 * Pi * cell.depth() * ThisPipeCellConductivity );
		ConvectiveResistance = 1.0 / ( ConvectionCoefficient * 2 * Pi * ThisPipeCellInnerRadius * cell.depth() );
		Resistance = PipeConductionResistance + ConvectiveResistance;
		Numerator += ( Beta / Resistance ) * FluidCellTemperature;
		Denominator += ( Beta / Resistance );

		//'calculate new temperature
		cell.PipeCellData.Pipe.MyBase.Temperature = Numerator / Denominator;

	}

	void
	SimulateFluidCell(
		CartesianCell & cell,
		Real64 const FlowRate,
		Real64 const ConvectionCoefficient,
		Real64 const EnteringFluidTemp
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Beta;
		Real64 Numerator;
		Real64 Denominator;
		Real64 TotalPipeResistance;
		Real64 PipeConductionResistance;
		Real64 ConvectiveResistance;
		Real64 UpstreamResistance;
		Real64 EnteringFluidConductance;

		Real64 FluidCellTemperature_PrevTimeStep;
		Real64 FluidCellSpecificHeat;
		Real64 PipeCellOuterRadius;
		Real64 PipeCellRadialCentroid;
		Real64 PipeCellConductivity;
		Real64 PipeCellInnerRadius;
		Real64 PipeCellTemperature;

		Numerator = 0.0;
		Denominator = 0.0;

		//'convenience variables
		FluidCellTemperature_PrevTimeStep = cell.PipeCellData.Fluid.MyBase.Temperature_PrevTimeStep;
		FluidCellSpecificHeat = cell.PipeCellData.Fluid.Properties.MyBase.SpecificHeat;

		PipeCellOuterRadius = cell.PipeCellData.Pipe.OuterRadius;
		PipeCellRadialCentroid = cell.PipeCellData.Pipe.RadialCentroid;
		PipeCellConductivity = cell.PipeCellData.Pipe.MyBase.Properties.Conductivity;
		PipeCellInnerRadius = cell.PipeCellData.Pipe.InnerRadius;
		PipeCellTemperature = cell.PipeCellData.Pipe.MyBase.Temperature;

		Beta = cell.PipeCellData.Fluid.MyBase.Beta;

		//'add effects from this cell history
		Numerator += FluidCellTemperature_PrevTimeStep;
		++Denominator;

		//'add effects from outer pipe cell
		PipeConductionResistance = std::log( PipeCellRadialCentroid / PipeCellInnerRadius ) / ( 2 * Pi * cell.depth() * PipeCellConductivity );
		ConvectiveResistance = 1.0 / ( ConvectionCoefficient * 2 * Pi * PipeCellInnerRadius * cell.depth() );
		TotalPipeResistance = PipeConductionResistance + ConvectiveResistance;
		Numerator += ( Beta / TotalPipeResistance ) * PipeCellTemperature;
		Denominator += ( Beta / TotalPipeResistance );

		//'add effects from upstream flow
		EnteringFluidConductance = 0.0;
		if ( FlowRate > 0.0 ) {
			UpstreamResistance = 1 / ( FlowRate * FluidCellSpecificHeat );
			Numerator += ( Beta / UpstreamResistance ) * EnteringFluidTemp;
			Denominator += ( Beta / UpstreamResistance );
		}

		//'calculate new temperature
		cell.PipeCellData.Fluid.MyBase.Temperature = Numerator / Denominator;

	}

	void
	DoOneTimeInitializations(
		int const DomainNum,
		Optional < int const > CircuitNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		//'initialize cell properties
		auto & cells( PipingSystemDomains( DomainNum ).Cells );
		for ( int X = cells.l1(), X_end = cells.u1(); X <= X_end; ++X ) {
			for ( int Y = cells.l2(), Y_end = cells.u2(); Y <= Y_end; ++Y ) {
				for ( int Z = cells.l3(), Z_end = cells.u3(); Z <= Z_end; ++Z ) {
					auto & cell( cells( X, Y, Z ) );

					{ auto const SELECT_CASE_var( cell.cellType );
					if ( SELECT_CASE_var == CellType::Pipe ) {
						cell.MyBase.Properties = PipingSystemDomains( DomainNum ).GroundProperties;
						for ( int rCtr = 0; rCtr <= cell.PipeCellData.Soil.u1(); ++rCtr ) {
							cell.PipeCellData.Soil( rCtr ).MyBase.Properties = PipingSystemDomains( DomainNum ).GroundProperties;
						}
						cell.PipeCellData.Pipe.MyBase.Properties = PipingSystemCircuits( CircuitNum ).PipeProperties;
						if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
							cell.PipeCellData.Insulation.MyBase.Properties = PipingSystemCircuits( CircuitNum ).InsulationProperties;
						}
					} else if ( ( SELECT_CASE_var == CellType::GeneralField ) || ( SELECT_CASE_var == CellType::GroundSurface ) || ( SELECT_CASE_var == CellType::FarfieldBoundary ) ) {
						cell.MyBase.Properties = PipingSystemDomains( DomainNum ).GroundProperties;
					} else if ( ( SELECT_CASE_var == CellType::BasementWall ) || ( SELECT_CASE_var == CellType::BasementFloor ) || ( SELECT_CASE_var == CellType::BasementCorner ) ) {
						if ( PipingSystemDomains( DomainNum ).HasZoneCoupledBasement ) { // Basement interface layer
							cell.MyBase.Properties = PipingSystemDomains( DomainNum ).BasementInterfaceProperties;
						} else { // Basement cells are partially ground, give them some props
							cell.MyBase.Properties = PipingSystemDomains( DomainNum ).GroundProperties;
						}
					} else if ( SELECT_CASE_var == CellType::Slab ) {
						cell.MyBase.Properties = PipingSystemDomains( DomainNum ).SlabProperties;
					} else if ( SELECT_CASE_var == CellType::HorizInsulation ) {
						cell.MyBase.Properties = PipingSystemDomains( DomainNum ).HorizInsProperties;
					} else if ( SELECT_CASE_var == CellType::VertInsulation ) {
						cell.MyBase.Properties = PipingSystemDomains( DomainNum ).VertInsProperties;
					} else if ( SELECT_CASE_var == CellType::SlabOnGradeEdgeInsu ) {//These cells insulate the slab sides. Give them some properties
						cell.MyBase.Properties = PipingSystemDomains( DomainNum ).GroundProperties;
						cell.MyBase.Properties.Conductivity = 0.000001; //Assign low conductivity
					} else if ( SELECT_CASE_var == CellType::ZoneGroundInterface ) {
							cell.MyBase.Properties = PipingSystemDomains( DomainNum ).SlabProperties;
					}}
				}
			}
		}

		//'calculate one-time resistance terms for cartesian cells
		for ( int X = cells.l1(), X_end = cells.u1(); X <= X_end; ++X ) {
			for ( int Y = cells.l2(), Y_end = cells.u2(); Y <= Y_end; ++Y ) {
				for ( int Z = cells.l3(), Z_end = cells.u3(); Z <= Z_end; ++Z ) {
					auto const & cell( cells( X, Y, Z ) );
					int NumFieldCells = 0, NumBoundaryCells = 0;
					EvaluateCellNeighborDirections( DomainNum, cell, NumFieldCells, NumBoundaryCells );
					for ( int DirectionCtr = 1; DirectionCtr <= NumFieldCells; ++DirectionCtr ) {
						Direction CurDirection = NeighborFieldCells( DirectionCtr );
						Real64 AdiabaticMultiplier = 1.0, NeighborTemp = 0.0, Resistance = 0.0;
						EvaluateNeighborCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );
						int NX = 0, NY = 0, NZ = 0;
						EvaluateNeighborCoordinates( cell, CurDirection, NX, NY, NZ );
						SetAdditionalNeighborData( DomainNum, X, Y, Z, CurDirection, Resistance, cells( NX, NY, NZ ) );
					}
				}
			}
		}

		//'create circuit array for convenience

		if ( present ( CircuitNum ) ) {
			if ( !allocated( PipingSystemCircuits( CircuitNum ).ListOfCircuitPoints ) ) {

				int SegCtr2 = -1;

				int TotalSegments = size( cells, 3 ) * size( PipingSystemCircuits( CircuitNum ).PipeSegmentIndeces );
				PipingSystemCircuits( CircuitNum ).ListOfCircuitPoints.allocate( { 0, TotalSegments - 1 } );

				for ( int SegIndex = PipingSystemCircuits( CircuitNum ).PipeSegmentIndeces.l1(); SegIndex <= PipingSystemCircuits( CircuitNum ).PipeSegmentIndeces.u1(); ++SegIndex ) {

					//'set simulation flow direction
					int StartingZ = 0;
					int EndingZ = 0;
					int Increment = 0;
					switch ( PipingSystemSegments( PipingSystemCircuits( CircuitNum ).PipeSegmentIndeces( SegIndex ) ).FlowDirection ) {
					case SegmentFlow::IncreasingZ:
						StartingZ = 0;
						EndingZ = cells.u3();
						Increment = 1;
						break;
					case SegmentFlow::DecreasingZ:
						StartingZ = cells.u3();
						EndingZ = 0;
						Increment = -1;
						break;
					}

					int PipeX = PipingSystemSegments( PipingSystemCircuits( CircuitNum ).PipeSegmentIndeces( SegIndex ) ).PipeCellCoordinates.X;
					int PipeY = PipingSystemSegments( PipingSystemCircuits( CircuitNum ).PipeSegmentIndeces( SegIndex ) ).PipeCellCoordinates.Y;

					//'loop across all z-direction indeces
					int const Zindex_stop( floop_end( StartingZ, EndingZ, Increment ) );
					for ( int Zindex = StartingZ; Zindex != Zindex_stop; Zindex += Increment ) {
						++SegCtr2;
						PipingSystemCircuits( CircuitNum ).ListOfCircuitPoints( SegCtr2 ) = Point3DInteger( PipeX, PipeY, Zindex );
					}
				}
			}
		}

		//'initialize freezing calculation variables
		PipingSystemDomains( DomainNum ).InitializeSoilMoistureCalcs();

		//'we can also initialize the domain based on the farfield temperature here
		for ( int X = cells.l1(), X_end = cells.u1(); X <= X_end; ++X ) {
			for ( int Y = cells.l2(), Y_end = cells.u2(); Y <= Y_end; ++Y ) {
				for ( int Z = cells.l3(), Z_end = cells.u3(); Z <= Z_end; ++Z ) {
					auto & cell( cells( X, Y, Z ) );

					// On OneTimeInit, the cur sim time should be zero, so this will be OK
					Real64 ThisCellTemp = GetFarfieldTemp( DomainNum, cell );
					cell.MyBase.Temperature = ThisCellTemp;
					cell.MyBase.Temperature_PrevIteration = ThisCellTemp;
					cell.MyBase.Temperature_PrevTimeStep = ThisCellTemp;


					if ( cell.cellType == CellType::Pipe ) {

						for ( int rCtr = 0; rCtr <= cell.PipeCellData.Soil.u1(); ++rCtr ) {
							cell.PipeCellData.Soil( rCtr ).MyBase.Temperature = ThisCellTemp;
							cell.PipeCellData.Soil( rCtr ).MyBase.Temperature_PrevIteration = ThisCellTemp;
							cell.PipeCellData.Soil( rCtr ).MyBase.Temperature_PrevTimeStep = ThisCellTemp;
						}
						cell.PipeCellData.Pipe.MyBase.Temperature = ThisCellTemp;
						cell.PipeCellData.Pipe.MyBase.Temperature_PrevIteration = ThisCellTemp;
						cell.PipeCellData.Pipe.MyBase.Temperature_PrevTimeStep = ThisCellTemp;
						if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
							cell.PipeCellData.Insulation.MyBase.Temperature = ThisCellTemp;
							cell.PipeCellData.Insulation.MyBase.Temperature_PrevIteration = ThisCellTemp;
							cell.PipeCellData.Insulation.MyBase.Temperature_PrevTimeStep = ThisCellTemp;
						}
						cell.PipeCellData.Fluid.MyBase.Temperature = ThisCellTemp;
						cell.PipeCellData.Fluid.MyBase.Temperature_PrevIteration = ThisCellTemp;
						cell.PipeCellData.Fluid.MyBase.Temperature_PrevTimeStep = ThisCellTemp;

					}
				}
			}
		}

	}

	void
	DoStartOfTimeStepInitializations(
		int const DomainNum,
		Optional < int const > CircuitNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// Using/Aliasing
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutRelHum;
		using DataEnvironment::WindSpeed;
		using DataEnvironment::BeamSolarRad;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetConductivityGlycol;
		using FluidProperties::GetViscosityGlycol;
		using DataPlant::PlantLoop; // only for fluid name/index

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static std::string const RoutineName( "PipingSystemCircuit::DoStartOfTimeStepInitializations" );
		Real64 Beta;
		Real64 CellTemp;
		Real64 CellRhoCp;
		int radialctr;
		int rCtr;
		Real64 FluidCp;
		Real64 FluidDensity;
		Real64 FluidConductivity;
		Real64 FluidViscosity;
		Real64 FluidPrandtl;

		// Update environmental conditions
		PipingSystemDomains( DomainNum ).Cur.CurAirTemp = OutDryBulbTemp;
		PipingSystemDomains( DomainNum ).Cur.CurWindSpeed = WindSpeed;
		PipingSystemDomains( DomainNum ).Cur.CurRelativeHumidity = OutRelHum;
		PipingSystemDomains( DomainNum ).Cur.CurIncidentSolar = BeamSolarRad;

		// If pipe circuit present
		if ( present( CircuitNum ) ) {
			// retrieve fluid properties based on the circuit inlet temperature -- which varies during the simulation
			// but need to verify the value of inlet temperature during warm up, etc.
			FluidCp = GetSpecificHeatGlycol( PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidName, PipingSystemCircuits( CircuitNum ).InletTemperature, PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidIndex, RoutineName );
			FluidDensity = GetDensityGlycol( PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidName, PipingSystemCircuits( CircuitNum ).InletTemperature, PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidIndex, RoutineName );
			FluidConductivity = GetConductivityGlycol( PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidName, PipingSystemCircuits( CircuitNum ).InletTemperature, PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidIndex, RoutineName );
			FluidViscosity = GetViscosityGlycol( PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidName, PipingSystemCircuits( CircuitNum ).InletTemperature, PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidIndex, RoutineName );

			// Doesn't anyone care about poor Ludwig Prandtl?
			FluidPrandtl = 3.0;

			// then assign these fluid properties to the current fluid property set for easy lookup as needed
			PipingSystemCircuits( CircuitNum ).CurFluidPropertySet = ExtendedFluidProperties( BaseThermalPropertySet( FluidConductivity, FluidDensity, FluidCp ), FluidViscosity, FluidPrandtl );
		}

		//'now update cell properties
		auto & cells( PipingSystemDomains( DomainNum ).Cells );
		for ( int X = cells.l1(), X_end = cells.u1(); X <= X_end; ++X ) {
			for ( int Y = cells.l2(), Y_end = cells.u2(); Y <= Y_end; ++Y ) {
				for ( int Z = cells.l3(), Z_end = cells.u3(); Z <= Z_end; ++Z ) {
					auto & cell( cells( X, Y, Z ) );

					{ auto const SELECT_CASE_var( cell.cellType );
					if ( ( SELECT_CASE_var == CellType::GeneralField ) || ( SELECT_CASE_var == CellType::FarfieldBoundary ) || ( SELECT_CASE_var == CellType::GroundSurface ) || ( SELECT_CASE_var == CellType::BasementCorner ) || ( SELECT_CASE_var == CellType::BasementFloor ) || ( SELECT_CASE_var == CellType::BasementWall ) ) {
							// UPDATE CELL PROPERTY SETS
							//'main ground cells, update with soil properties
							CellTemp = cell.MyBase.Temperature;
							PipingSystemDomains( DomainNum ).EvaluateSoilRhoCp( CellTemp, CellRhoCp );
							cell.MyBase.Properties.SpecificHeat = CellRhoCp / cell.MyBase.Properties.Density;

							// UPDATE BETA VALUE
							//'these are basic cartesian calculation cells
							Beta = PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize / ( cell.MyBase.Properties.Density * cell.volume() * cell.MyBase.Properties.SpecificHeat );
							cell.MyBase.Beta = Beta;

					} else if ( ( SELECT_CASE_var == CellType::HorizInsulation ) || ( SELECT_CASE_var == CellType::VertInsulation ) || ( SELECT_CASE_var == CellType::Slab ) || ( SELECT_CASE_var == CellType::ZoneGroundInterface ) || ( SELECT_CASE_var == CellType::SlabOnGradeEdgeInsu ) ) {

						Beta = PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize / ( cell.MyBase.Properties.Density * cell.volume() * cell.MyBase.Properties.SpecificHeat );
						PipingSystemDomains ( DomainNum ).Cells ( X, Y, Z ).MyBase.Beta = Beta;

					} else if ( SELECT_CASE_var == CellType::Pipe ) {
						// If pipe circuit present
						if ( present( CircuitNum ) ) {
							// UPDATE CELL PROPERTY SETS
							//'first update the outer cell itself
							CellTemp = cell.MyBase.Temperature;
							PipingSystemDomains( DomainNum ).EvaluateSoilRhoCp( CellTemp, CellRhoCp );
							cell.MyBase.Properties.SpecificHeat = CellRhoCp / cell.MyBase.Properties.Density;
							//'then update all the soil radial cells
							for ( radialctr = cell.PipeCellData.Soil.l1(); radialctr <= cell.PipeCellData.Soil.u1(); ++radialctr ) {
								CellTemp = cell.PipeCellData.Soil( radialctr ).MyBase.Temperature;
								PipingSystemDomains( DomainNum ).EvaluateSoilRhoCp( CellTemp, CellRhoCp );
								cell.PipeCellData.Soil( radialctr ).MyBase.Properties.SpecificHeat = CellRhoCp / cell.PipeCellData.Soil( radialctr ).MyBase.Properties.Density;
							}

							// UPDATE BETA VALUES
							//'set the interface cell
							Beta = PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize / ( cell.MyBase.Properties.Density * cell.PipeCellData.InterfaceVolume * cell.MyBase.Properties.SpecificHeat );
							cell.MyBase.Beta = Beta;

							//'set the radial soil cells
							for ( rCtr = 0; rCtr <= cell.PipeCellData.Soil.u1(); ++rCtr ) {
								Beta = PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize / ( cell.PipeCellData.Soil( rCtr ).MyBase.Properties.Density * cell.PipeCellData.Soil( rCtr ).XY_CrossSectArea() * cell.depth() * cell.PipeCellData.Soil( rCtr ).MyBase.Properties.SpecificHeat );
								cell.PipeCellData.Soil( rCtr ).MyBase.Beta = Beta;
							}

							//'then insulation if it exists
							if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
								Beta = PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize / ( cell.PipeCellData.Insulation.MyBase.Properties.Density * cell.PipeCellData.Insulation.XY_CrossSectArea() * cell.depth() * cell.PipeCellData.Insulation.MyBase.Properties.SpecificHeat );
								cell.PipeCellData.Insulation.MyBase.Beta = Beta;
							}

							//'set the pipe cell
							Beta = PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize / ( cell.PipeCellData.Pipe.MyBase.Properties.Density * cell.PipeCellData.Pipe.XY_CrossSectArea() * cell.depth() * cell.PipeCellData.Pipe.MyBase.Properties.SpecificHeat );
							cell.PipeCellData.Pipe.MyBase.Beta = Beta;

							// now the fluid cell also
							cell.PipeCellData.Fluid.Properties = PipingSystemCircuits( CircuitNum ).CurFluidPropertySet;
							cell.PipeCellData.Fluid.MyBase.Beta = PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize / ( cell.PipeCellData.Fluid.Properties.MyBase.Density * cell.PipeCellData.Fluid.Volume * cell.PipeCellData.Fluid.Properties.MyBase.SpecificHeat );
						}
					}}
				}
			}
		}

#ifdef CalcEnergyBalance
		PipingSystemDomains( DomainNum ).finalIteration = false;
#endif

	}

	void
	DoEndOfIterationOperations(
		int const DomainNum,
		bool & Finished
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "DoEndOfIterationOperations" );

		//'check if we have converged for this iteration
		Finished = IsConverged_CurrentToPrevIteration( DomainNum );

#ifdef CalcEnergyBalance
		if ( Finished && !PipingSystemDomains( DomainNum ).finalIteration ) {
			PipingSystemDomains( DomainNum ).finalIteration = true;
			Finished = false;
		}
#endif

		//'check for out of range temperatures here so they aren't plotted
		//'this routine should be *much* more restrictive than the exceptions, so we should be safe with this location
		bool OutOfRange = CheckForOutOfRangeTemps( DomainNum );
		if ( OutOfRange ) {
			if ( PipingSystemDomains( DomainNum ).HasZoneCoupledSlab ) {
				ShowSevereError( "Site:GroundDomain:Slab" + RoutineName + ": Out of range temperatures detected in the ground domain." );
				ShowContinueError( "This could be due to the size of the loads on the domain." );
				ShowContinueError( "Verify inputs are correct. If problem persists, notify EnergyPlus support." );
				ShowFatalError( "Preceding error(s) cause program termination" );
			} else if ( PipingSystemDomains( DomainNum ).HasZoneCoupledBasement ) {
				ShowSevereError( "Site:GroundDomain:Basement" + RoutineName + ": Out of range temperatures detected in the ground domain." );
				ShowContinueError( "This could be due to the size of the loads on the domain." );
				ShowContinueError( "Verify inputs are correct. If problem persists, notify EnergyPlus support." );
				ShowFatalError( "Preceding error(s) cause program termination" );
			} else {
				ShowSevereError( "PipingSystems:" + RoutineName + ": Out of range temperatures detected in piping system simulation." );
				ShowContinueError( "This could be due to the size of the pipe circuit in relation to the loads being imposed." );
				ShowContinueError( "Try increasing the size of the pipe circuit and investigate sizing effects." );
				ShowFatalError( "Preceding error(s) cause program termination" );
			}
		}

	}

	void
	FullDomainStructureInfo::InitializeSoilMoistureCalcs()
	{

		// These vary by domain now, so we must be careful to retrieve them every time
		Real64 Theta_liq = this->Moisture.Theta_liq;
		Real64 Theta_sat = this->Moisture.Theta_sat;

		// Assumption
		Real64 Theta_ice = Theta_liq;

		//'Cp (freezing) calculations
		Real64 const rho_ice = 917.0; //'Kg / m3
		Real64 const rho_liq = 1000.0; //'kg / m3

		//'from( " An improved model for predicting soil thermal conductivity from water content at room temperature, Fig 4" )
		Real64 const CP_liq = 4180.0; //'J / KgK
		Real64 const CP_ice = 2066.0; //'J / KgK
		Real64 const Lat_fus = 334000.0; //'J / Kg
		Real64 const Cp_transient = Lat_fus / 0.4 + ( 0.5 * CP_ice - ( CP_liq + CP_ice ) / 2.0 * 0.1 ) / 0.4;

		//'from( " Numerical and experimental investigation of melting and freezing processes in phase change material storage" )
		this->Moisture.rhoCp_soil_liq_1 = 1225000.0 / ( 1.0 - Theta_sat ); //'J/m3K
		this->Moisture.rhoCP_soil_liq = this->Moisture.rhoCp_soil_liq_1 * ( 1.0 - Theta_sat ) + rho_liq * CP_liq * Theta_liq;
		this->Moisture.rhoCP_soil_transient = this->Moisture.rhoCp_soil_liq_1 * ( 1.0 - Theta_sat ) + ( ( rho_liq + rho_ice ) / 2.0 ) * Cp_transient * Theta_ice;
		this->Moisture.rhoCP_soil_ice = this->Moisture.rhoCp_soil_liq_1 * ( 1.0 - Theta_sat ) + rho_ice * CP_ice * Theta_ice; //'!J / m3K


	}

	void
	FullDomainStructureInfo::EvaluateSoilRhoCp(
		Real64 const CellTemp,
		Real64 & rhoCp
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		//'set some temperatures here for generalization -- these could be set in the input file
		Real64 const frzAllIce = -0.5;
		Real64 const frzIceTrans = -0.4;
		Real64 const frzLiqTrans = -0.1;
		Real64 const frzAllLiq = 0.0;

		//'calculate this cell's new Cp value based on the cell temperature
		if ( CellTemp <= frzAllIce ) { // totally frozen
			rhoCp = this->Moisture.rhoCP_soil_ice;
		} else if ( ( CellTemp > frzAllIce ) && ( CellTemp < frzIceTrans ) ) { // in between totally frozen and ice transition
			rhoCp = this->Moisture.rhoCP_soil_ice + ( this->Moisture.rhoCP_soil_transient - this->Moisture.rhoCP_soil_ice ) / ( frzIceTrans - frzAllIce ) * ( CellTemp - frzAllIce );
		} else if ( ( CellTemp >= frzIceTrans ) && ( CellTemp <= frzLiqTrans ) ) { // in between ice transition and liquid transition
			rhoCp = this->Moisture.rhoCP_soil_transient;
		} else if ( ( CellTemp > frzLiqTrans ) && ( CellTemp < frzAllLiq ) ) { // in between liquid transition and all liquid
			rhoCp = this->Moisture.rhoCp_soil_liq_1 + ( this->Moisture.rhoCP_soil_transient - this->Moisture.rhoCP_soil_liq ) / ( frzAllLiq - frzLiqTrans ) * ( frzAllLiq - CellTemp );
		} else { // ( CellTemp >= frzAllLiq ) --- greater than or equal to all liquid
			rhoCp = this->Moisture.rhoCp_soil_liq_1;
		}

	}

	void
	SetAdditionalNeighborData(
		int const DomainNum,
		int const X,
		int const Y,
		int const Z,
		Direction const direction,
		Real64 const Resistance,
		CartesianCell const & NeighborCell
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		auto & cell( PipingSystemDomains( DomainNum ).Cells( X, Y, Z ) );
		for ( int NeighborIndex = 0; NeighborIndex <= cell.NeighborInformation.u1(); ++NeighborIndex ) {
			if ( cell.NeighborInformation( NeighborIndex ).direction == direction ) {
				cell.NeighborInformation( NeighborIndex ).Value.ConductionResistance = Resistance;
				cell.NeighborInformation( NeighborIndex ).Value.NeighborCellIndeces = Point3DInteger( NeighborCell.X_index, NeighborCell.Y_index, NeighborCell.Z_index );
			}
		}

	}


	void
	EvaluateNeighborCoordinates(
		CartesianCell const & ThisCell,
		Direction const CurDirection,
		int & NX,
		int & NY,
		int & NZ
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		int const X = ThisCell.X_index;
		int const Y = ThisCell.Y_index;
		int const Z = ThisCell.Z_index;

		switch ( CurDirection ) {
		case Direction::PositiveY:
			NX = X;
			NY = Y + 1;
			NZ = Z;
			break;
		case Direction::NegativeY:
			NX = X;
			NY = Y - 1;
			NZ = Z;
			break;
		case Direction::PositiveX:
			NX = X + 1;
			NY = Y;
			NZ = Z;
			break;
		case Direction::NegativeX:
			NX = X - 1;
			NY = Y;
			NZ = Z;
			break;
		case Direction::PositiveZ:
			NX = X;
			NY = Y;
			NZ = Z + 1;
			break;
		case Direction::NegativeZ:
			NX = X;
			NY = Y;
			NZ = Z - 1;
			break;
		default:
			assert( false );
		}

	}

	void
	EvaluateNeighborCharacteristics(
		int const DomainNum,
		CartesianCell const & ThisCell,
		Direction const CurDirection,
		Real64 & NeighborTemp,
		Real64 & Resistance,
		Real64 & AdiabaticMultiplier
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		int NX = 0, NY = 0, NZ = 0;
		EvaluateNeighborCoordinates( ThisCell, CurDirection, NX, NY, NZ );

		NeighborInformation TempNeighborInfo;

		//'split effects between the two cells so we can carefully calculate resistance values
		Real64 ThisCellLength = 0.0;
		Real64 NeighborCellLength = 0.0;
		Real64 ThisCellConductivity = 10000.0;
		if ( ThisCell.MyBase.Properties.Conductivity > 0.0 ) ThisCellConductivity = ThisCell.MyBase.Properties.Conductivity;
		Real64 NeighborConductivity = 10000.0;
		auto const & cell( PipingSystemDomains( DomainNum ).Cells( NX, NY, NZ ) );
		if ( cell.MyBase.Properties.Conductivity > 0.0 ) NeighborConductivity = cell.MyBase.Properties.Conductivity;

		//'calculate normal surface area
		Real64 ThisNormalArea = ThisCell.normalArea( CurDirection );

		//'set distance based on cell types
		TempNeighborInfo = NeighborInformationArray_Value( ThisCell.NeighborInformation, CurDirection );
		if ( ThisCell.cellType == CellType::Pipe ) {
			//'we need to be a bit careful with pipes, as they are full centroid to centroid in the z direction,
			//' but only centroid to wall in the x and y directions
			if ( CurDirection == Direction::NegativeZ || CurDirection == Direction::PositiveZ ) {
				ThisCellLength = TempNeighborInfo.ThisCentroidToNeighborWall;
				NeighborCellLength = TempNeighborInfo.ThisWallToNeighborCentroid;
			} else {
				ThisCellLength = 0.0;
				NeighborCellLength = TempNeighborInfo.ThisWallToNeighborCentroid;
			}
		} else if ( cell.cellType == CellType::Pipe ) {
			ThisCellLength = TempNeighborInfo.ThisCentroidToNeighborWall;
			NeighborCellLength = 0.0;
		} else {
			ThisCellLength = TempNeighborInfo.ThisCentroidToNeighborWall;
			NeighborCellLength = TempNeighborInfo.ThisWallToNeighborCentroid;
		}

		//'calculate resistance based on different conductivities between the two cells
		Resistance = ( ThisCellLength / ( ThisNormalArea * ThisCellConductivity ) ) + ( NeighborCellLength / ( ThisNormalArea * NeighborConductivity ) );

		//'return proper temperature for the given simulation type
		NeighborTemp = cell.MyBase.Temperature;

		//'return the adiabatic multiplier
		AdiabaticMultiplier = TempNeighborInfo.adiabaticMultiplier;

	}

	void
	EvaluateCellNeighborDirections(
		int const DomainNum,
		CartesianCell const & cell,
		int & NumFieldCells,
		int & NumBoundaryCells
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int const TotalNumDimensions( 6 );

		auto const & cells( PipingSystemDomains( DomainNum ).Cells );
		int Xmax = cells.u1();
		int Ymax = cells.u2();
		int Zmax = cells.u3();
		int Xindex = cell.X_index;
		int Yindex = cell.Y_index;
		int Zindex = cell.Z_index;
		// Initialize the counters

		NumFieldCells = 0;
		NumBoundaryCells = 0;

		// First get the count for each array
		if ( Xindex < Xmax ) ++NumFieldCells;
		if ( Xindex > 0 ) ++NumFieldCells;
		if ( Yindex < Ymax ) ++NumFieldCells;
		if ( Yindex > 0 ) ++NumFieldCells;
		if ( Zindex < Zmax ) ++NumFieldCells;
		if ( Zindex > 0 ) ++NumFieldCells;
		NumBoundaryCells = TotalNumDimensions - NumFieldCells;

		// Then add to each array appropriately
		int FieldCellCtr = 0;
		int BoundaryCellCtr = 0;
		if ( Xindex < Xmax ) {
			++FieldCellCtr;
			NeighborFieldCells( FieldCellCtr ) = Direction::PositiveX;
		} else {
			++BoundaryCellCtr;
			NeighborBoundaryCells( BoundaryCellCtr ) = Direction::PositiveX;
		}

		if ( Xindex > 0 ) {
			++FieldCellCtr;
			NeighborFieldCells( FieldCellCtr ) = Direction::NegativeX;
		} else {
			++BoundaryCellCtr;
			NeighborBoundaryCells( BoundaryCellCtr ) = Direction::NegativeX;
		}

		if ( Yindex < Ymax ) {
			++FieldCellCtr;
			NeighborFieldCells( FieldCellCtr ) = Direction::PositiveY;
		} else {
			++BoundaryCellCtr;
			NeighborBoundaryCells( BoundaryCellCtr ) = Direction::PositiveY;
		}

		if ( Yindex > 0 ) {
			++FieldCellCtr;
			NeighborFieldCells( FieldCellCtr ) = Direction::NegativeY;
		} else {
			++BoundaryCellCtr;
			NeighborBoundaryCells( BoundaryCellCtr ) = Direction::NegativeY;
		}

		if ( Zindex < Zmax ) {
			++FieldCellCtr;
			NeighborFieldCells( FieldCellCtr ) = Direction::PositiveZ;
		} else {
			++BoundaryCellCtr;
			NeighborBoundaryCells( BoundaryCellCtr ) = Direction::PositiveZ;
		}

		if ( Zindex > 0 ) {
			++FieldCellCtr;
			NeighborFieldCells( FieldCellCtr ) = Direction::NegativeZ;
		} else {
			++BoundaryCellCtr;
			NeighborBoundaryCells( BoundaryCellCtr ) = Direction::NegativeZ;
		}

	}

#ifdef CalcEnergyBalance
	void
	UpdateMaxEnergyBalance(
		int const DomainNum
	)
	{
		PipingSystemDomains( DomainNum ).MaxEnergyImbalance = 0.0;
		PipingSystemDomains( DomainNum ).maxTempDiffDueToImbalance = 0.0;

		auto & cells( PipingSystemDomains( DomainNum ).Cells );
		for ( int X = cells.l1(), X_end = cells.u1(); X <= X_end; ++X ) {
			for ( int Y = cells.l2(), Y_end = cells.u2(); Y <= Y_end; ++Y ) {
				for ( int Z = cells.l3(), Z_end = cells.u3(); Z <= Z_end; ++Z ) {

					auto & cell( cells( X, Y, Z ) );

					// Max energy imbalance
					if ( cell.MyBase.energyImbalance > PipingSystemDomains( DomainNum ).MaxEnergyImbalance ){
						PipingSystemDomains( DomainNum ).MaxEnergyImbalance = cell.MyBase.energyImbalance;
						PipingSystemDomains( DomainNum ).MaxEnergyImbalance_XLocation = X;
						PipingSystemDomains( DomainNum ).MaxEnergyImbalance_YLocation = Y;
						PipingSystemDomains( DomainNum ).MaxEnergyImbalance_ZLocation = Z;
					}

					// Cell with min num sides calculated
					if ( cell.MyBase.numberOfSidesCalculated < PipingSystemDomains( DomainNum ).minNumberOfSidesCalculated ) {
						PipingSystemDomains( DomainNum ).minNumberOfSidesCalculated = cell.MyBase.numberOfSidesCalculated;
						PipingSystemDomains( DomainNum ).minNumberOfSidesCalculated_XLocation = X;
						PipingSystemDomains( DomainNum ).minNumberOfSidesCalculated_YLocation = Y;
						PipingSystemDomains( DomainNum ).minNumberOfSidesCalculated_ZLocation = Z;
					}

					// Cell with max num sides calculated
					if ( cell.MyBase.numberOfSidesCalculated > PipingSystemDomains( DomainNum ).maxNumberOfSidesCalculated ) {
						PipingSystemDomains( DomainNum ).maxNumberOfSidesCalculated = cell.MyBase.numberOfSidesCalculated;
						PipingSystemDomains( DomainNum ).maxNumberOfSidesCalculated_XLocation = X;
						PipingSystemDomains( DomainNum ).maxNumberOfSidesCalculated_YLocation = Y;
						PipingSystemDomains( DomainNum ).maxNumberOfSidesCalculated_ZLocation = Z;
					}

					// Cell with max temp difference do to energy imbalance
					if ( cell.MyBase.tempDiffDueToImbalance > PipingSystemDomains( DomainNum ).maxTempDiffDueToImbalance ) {
						PipingSystemDomains( DomainNum ).maxTempDiffDueToImbalance = cell.MyBase.tempDiffDueToImbalance;
						PipingSystemDomains( DomainNum ).maxTempDiffDueToImbalance_XLocation = X;
						PipingSystemDomains( DomainNum ).maxTempDiffDueToImbalance_YLocation = Y;
						PipingSystemDomains( DomainNum ).maxTempDiffDueToImbalance_ZLocation = Z;
					}
				}
			}
		}
	}
#endif

} // PlantPipingSystemsManager

} // EnergyPlus
