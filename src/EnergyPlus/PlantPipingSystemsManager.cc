// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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
#include <GlobalNames.hh>
#include <GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <InputProcessing/InputProcessor.hh>
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

	// MODULE VARIABLE DECLARATIONS:
	Array1D< Direction > NeighborFieldCells( 6 );
	Array1D< Direction > NeighborBoundaryCells( 6 );
	Array1D< FullDomainStructureInfo > PipingSystemDomains;
	Array1D< PipeCircuitInfo > PipingSystemCircuits;
	Array1D< PipeSegmentInfo > PipingSystemSegments;
	std::unordered_map< std::string, std::string > GroundDomainUniqueNames;

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
		int numSlabsCheck( inputProcessor->getNumObjectsFound( ObjName_ZoneCoupled_Slab ) );
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
		int const numBasementsCheck( inputProcessor->getNumObjectsFound( ObjName_ZoneCoupled_Basement ) );
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
		using General::TrimSigDigits;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SimPipingSystems" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true ); // First time, input is "gotten"
		int CircuitNum;

		// Read input if necessary
		if ( GetInputFlag ) {
			GetPipingSystemsAndGroundDomainsInput();
			GetInputFlag = false;
		}

		// Look for circuit index
		if ( EqNum == 0 ) {
			CircuitNum = UtilityRoutines::FindItemInList( EquipName, PipingSystemCircuits );
			if ( CircuitNum == 0 ) {
				// Catch any bad names before crashing
				ShowFatalError( RoutineName + ": Piping circuit requested not found=" + EquipName );
			}
			EqNum = CircuitNum;
		} else {
			CircuitNum = EqNum;
			int const NumOfPipeCircuits = isize( PipingSystemCircuits );
			if ( CircuitNum > NumOfPipeCircuits || CircuitNum < 1 ) {
				ShowFatalError( RoutineName + ":  Invalid component index passed=" + TrimSigDigits( CircuitNum ) + ", Number of Units=" + TrimSigDigits( NumOfPipeCircuits ) + ", Entered Unit name=" + EquipName ); //Autodesk:Uninit DomainNum was uninitialized
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
		int DomainNum = PipingSystemCircuits( CircuitNum ).ParentDomainIndex;

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
			auto & thisDomain( PipingSystemDomains( DomainNum ) );
			if ( thisDomain.DomainNeedsToBeMeshed ) {
				thisDomain.developMesh();
			}

			thisDomain.DomainNeedsToBeMeshed = false;

			// The time init should be done here before we DoOneTimeInits because the DoOneTimeInits
			// includes a ground temperature initialization, which is based on the Cur%CurSimTimeSeconds variable
			// which would be carried over from the previous environment
			thisDomain.Cur.CurSimTimeStepSize = TimeStepZone * SecInHour;
			thisDomain.Cur.CurSimTimeSeconds = ( ( DayOfSim - 1 ) * 24 + ( HourOfDay - 1 ) + ( TimeStep - 1 ) * TimeStepZone + SysTimeElapsed ) * SecInHour;

			// There are also some inits that are "close to one time" inits...( one-time in standalone, each envrn in E+ )
			if ( ( BeginSimFlag && thisDomain.BeginSimInit ) || ( BeginEnvrnFlag && thisDomain.BeginSimEnvrn ) ) {

				DoOneTimeInitializations( DomainNum, _ );

				if ( thisDomain.HasZoneCoupledSlab ) {
					int Xmax = ubound( thisDomain.Cells, 1 );
					//int Ymax = ubound( thisDomain.Cells, 2 );
					int Zmax = ubound( thisDomain.Cells, 3 );

					thisDomain.WeightingFactor.allocate( { 0, Xmax }, { 0, Zmax } );
					thisDomain.WeightedHeatFlux.allocate( { 0, Xmax }, { 0, Zmax } );
				}

				thisDomain.BeginSimInit = false;
				thisDomain.BeginSimEnvrn = false;
			}
			if ( ! BeginSimFlag ) thisDomain.BeginSimInit = true;
			if ( ! BeginEnvrnFlag ) thisDomain.BeginSimEnvrn = true;


			// Reset the heat fluxes if domain update has been completed
			if ( thisDomain.ResetHeatFluxFlag ) {
				thisDomain.AggregateHeatFlux = 0;
				thisDomain.AggregateWallHeatFlux = 0;
				thisDomain.AggregateFloorHeatFlux = 0;
				thisDomain.NumHeatFlux = 0;
				thisDomain.ResetHeatFluxFlag = false;
			}

			if ( !initOnly ) {
				// Aggregate the heat flux
				// Zone-coupled slab
				if ( thisDomain.HasZoneCoupledSlab ) {
					thisDomain.AggregateHeatFlux += thisDomain.GetZoneInterfaceHeatFlux();
					thisDomain.NumHeatFlux += 1;
					thisDomain.HeatFlux = thisDomain.AggregateHeatFlux / thisDomain.NumHeatFlux;
				} else { // Coupled basement

					// basement walls
					thisDomain.AggregateWallHeatFlux += thisDomain.GetBasementWallHeatFlux();
					// basement floor
					thisDomain.AggregateFloorHeatFlux += thisDomain.GetBasementFloorHeatFlux();

					thisDomain.NumHeatFlux += 1;
					thisDomain.WallHeatFlux = thisDomain.AggregateWallHeatFlux / thisDomain.NumHeatFlux;
					thisDomain.FloorHeatFlux = thisDomain.AggregateFloorHeatFlux / thisDomain.NumHeatFlux;
				}

				// Aggregate the heat flux
				// Zone-coupled slab
				if ( thisDomain.HasZoneCoupledSlab ) {
					thisDomain.AggregateHeatFlux += thisDomain.GetZoneInterfaceHeatFlux();
					thisDomain.NumHeatFlux += 1;
					thisDomain.HeatFlux = thisDomain.AggregateHeatFlux / thisDomain.NumHeatFlux;
				} else if ( thisDomain.HasZoneCoupledBasement ) { // Coupled basement
					// basement walls
					thisDomain.AggregateWallHeatFlux += thisDomain.GetBasementWallHeatFlux();
					// basement floor
					thisDomain.AggregateFloorHeatFlux += thisDomain.GetBasementFloorHeatFlux();

					thisDomain.NumHeatFlux += 1;
					thisDomain.WallHeatFlux = thisDomain.AggregateWallHeatFlux / thisDomain.NumHeatFlux;
					thisDomain.FloorHeatFlux = thisDomain.AggregateFloorHeatFlux / thisDomain.NumHeatFlux;
				}

				// Zone-coupled slab
				if ( thisDomain.HasZoneCoupledSlab ) {

					thisDomain.HeatFlux = thisDomain.AggregateHeatFlux / thisDomain.NumHeatFlux;

					Real64 ZoneTemp = 0.0;

					//Set ZoneTemp equal to the average air temperature of the zones the coupled surfaces are part of.
					for ( SurfCtr = 1; SurfCtr <= isize( thisDomain.ZoneCoupledSurfaces ); ++SurfCtr ) {
						int ZoneNum = thisDomain.ZoneCoupledSurfaces( SurfCtr ).Zone;
						ZoneTemp += ZTAV( ZoneNum );
					}

					ZoneTemp = ZoneTemp / ( SurfCtr - 1 );
					Real64 AvgSlabTemp = thisDomain.GetAverageTempByType( CellType::ZoneGroundInterface );

					int Ymax = ubound( thisDomain.Cells, 2 );

					for ( int Z = lbound( thisDomain.Cells, 3 ); Z <= ubound( thisDomain.Cells, 3 ); ++Z ) {
						for ( int X = lbound( thisDomain.Cells, 1 ); X <= ubound( thisDomain.Cells, 1 ); ++X ) {
							// Zone interface cells
							if ( thisDomain.Cells( X, Ymax, Z ).cellType == CellType::ZoneGroundInterface ){
								thisDomain.WeightingFactor( X, Z ) = abs ( ( ZoneTemp - thisDomain.Cells( X, Ymax, Z ).Temperature_PrevTimeStep ) / ( ZoneTemp - AvgSlabTemp ) );
							}
						}
					}

					// Set initial weighted heat flux
					for ( int Z = lbound( thisDomain.Cells, 3 ); Z <= ubound( thisDomain.Cells, 3 ); ++Z ) {
						for ( int X = lbound( thisDomain.Cells, 1 ); X <= ubound( thisDomain.Cells, 1 ); ++X ) {
							// Zone interface cells
							if ( thisDomain.Cells( X, Ymax, Z ).cellType == CellType::ZoneGroundInterface ){
								thisDomain.WeightedHeatFlux( X, Z ) = thisDomain.WeightingFactor( X, Z ) * thisDomain.HeatFlux;
							}
						}
					}

					// Weighted heat flux and uniform heat flux balance energy may not balance exactly
					// Calculate difference and adjust
					thisDomain.TotalEnergyUniformHeatFlux = thisDomain.HeatFlux * thisDomain.SlabArea * thisDomain.Cur.CurSimTimeStepSize;
					thisDomain.TotalEnergyWeightedHeatFlux = 0.0;

					for ( int Z = lbound( thisDomain.Cells, 3 ); Z <= ubound( thisDomain.Cells, 3 ); ++Z ) {
						for ( int X = lbound( thisDomain.Cells, 1 ); X <= ubound( thisDomain.Cells, 1 ); ++X ) {
							// Zone interface cells
							if ( thisDomain.Cells( X, Ymax, Z ).cellType == CellType::ZoneGroundInterface ){
								auto & cell( thisDomain.Cells( X, Ymax, Z ) );
								thisDomain.TotalEnergyWeightedHeatFlux += thisDomain.WeightedHeatFlux( X, Z ) * cell.width() * cell.depth() * thisDomain.Cur.CurSimTimeStepSize;
							}
						}
					}

					thisDomain.HeatFluxWeightingFactor = thisDomain.TotalEnergyWeightedHeatFlux / thisDomain.TotalEnergyUniformHeatFlux;
					thisDomain.TotalEnergyWeightedHeatFlux = 0.0;

					// Finally, adjust the weighted heat flux so that energy balances
					for ( int Z = lbound( thisDomain.Cells, 3 ); Z <= ubound( thisDomain.Cells, 3 ); ++Z ) {
						for ( int X = lbound( thisDomain.Cells, 1 ); X <= ubound( thisDomain.Cells, 1 ); ++X ) {
							// Zone interface cells
							if ( thisDomain.Cells( X, Ymax, Z ).cellType == CellType::ZoneGroundInterface ) {
								auto & cell( thisDomain.Cells( X, Ymax, Z ) );
								thisDomain.WeightedHeatFlux( X, Z ) = thisDomain.WeightedHeatFlux( X, Z ) / thisDomain.HeatFluxWeightingFactor;
								thisDomain.TotalEnergyWeightedHeatFlux += thisDomain.WeightedHeatFlux( X, Z ) * cell.width() * cell.depth() * thisDomain.Cur.CurSimTimeStepSize;
							}
						}
					}

				} else { // Coupled basement
					thisDomain.WallHeatFlux = thisDomain.AggregateWallHeatFlux / thisDomain.NumHeatFlux;
					thisDomain.FloorHeatFlux = thisDomain.AggregateFloorHeatFlux / thisDomain.NumHeatFlux;
				}

				// Shift history arrays only if necessary
				if ( std::abs( thisDomain.Cur.CurSimTimeSeconds - thisDomain.Cur.PrevSimTimeSeconds ) > 1.0e-6 ) {
					thisDomain.Cur.PrevSimTimeSeconds = thisDomain.Cur.CurSimTimeSeconds;
					thisDomain.ShiftTemperaturesForNewTimeStep();
					thisDomain.DomainNeedsSimulation = true;
				}
				PerformIterationLoop( DomainNum, _ );
			}
		}

		if ( WriteEIOFlag ) {
			// Write eio header
			gio::write( OutputFileInits, DomainCellsToEIOHeader );

			// Write eio data
			for ( int DomainNum = 1; DomainNum <= isize( PipingSystemDomains ); ++DomainNum ) {
				auto & thisDomain( PipingSystemDomains( DomainNum ) );
				gio::write( OutputFileInits, DomainCellsToEIO ) << thisDomain.Name << thisDomain.NumDomainCells
					<< thisDomain.NumGroundSurfCells << thisDomain.NumInsulationCells;
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
		NumGeneralizedDomains = inputProcessor->getNumObjectsFound( ObjName_ug_GeneralDomain );
		NumHorizontalTrenches = inputProcessor->getNumObjectsFound( ObjName_HorizTrench );
		NumZoneCoupledDomains = inputProcessor->getNumObjectsFound( ObjName_ZoneCoupled_Slab );
		NumBasements = inputProcessor->getNumObjectsFound( ObjName_ZoneCoupled_Basement );
		TotalNumDomains = NumGeneralizedDomains + NumHorizontalTrenches + NumZoneCoupledDomains + NumBasements;
		PipingSystemDomains.allocate( TotalNumDomains );

		// then circuits
		NumPipeCircuits = inputProcessor->getNumObjectsFound( ObjName_Circuit );
		TotalNumCircuits = NumPipeCircuits + NumHorizontalTrenches;
		PipingSystemCircuits.allocate( TotalNumCircuits );

		// then segments
		NumPipeSegmentsInInput = inputProcessor->getNumObjectsFound( ObjName_Segment );
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

		// Report errors that are purely input problems
		if ( ErrorsFound ) ShowFatalError( RoutineName + ": Preceding input errors cause program termination." );

		// Setup output variables
		SetupPipingSystemOutputVariables( TotalNumSegments, TotalNumCircuits );

		// Validate CIRCUIT-SEGMENT cross references
		for ( CircuitCtr = PipingSystemCircuits.l1(); CircuitCtr <= PipingSystemCircuits.u1(); ++CircuitCtr ) {

			// validate circuit-segment name-to-index references
			for ( ThisCircuitPipeSegmentCounter = PipingSystemCircuits( CircuitCtr ).PipeSegmentNames.l1(); ThisCircuitPipeSegmentCounter <= PipingSystemCircuits( CircuitCtr ).PipeSegmentNames.u1(); ++ThisCircuitPipeSegmentCounter ) {

				ThisSegmentName = PipingSystemCircuits( CircuitCtr ).PipeSegmentNames( ThisCircuitPipeSegmentCounter );
				ThisSegmentIndex = UtilityRoutines::FindItemInList( ThisSegmentName, PipingSystemSegments );
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
				CircuitIndex = UtilityRoutines::FindItemInList( PipingSystemDomains( DomainNum ).CircuitNames( CircuitCtr ), PipingSystemCircuits );
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
			inputProcessor->getObjectItem( ObjName_HorizTrench, HorizontalCtr, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

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

		for ( DomainNum = IndexStart; DomainNum <= NumGeneralizedDomains; ++DomainNum ) {

			// Set up all the inputs for this domain object
			inputProcessor->getObjectItem( ObjName_ug_GeneralDomain, DomainNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			// Get the name, validate
			PipingSystemDomains( DomainNum ).Name = cAlphaArgs( 1 );
			UtilityRoutines::IsNameEmpty(cAlphaArgs( 1 ), cCurrentModuleObject, ErrorsFound);
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
				IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( 2 ), cAlphaArgs( 2 ), "Use a choice from the available mesh type keys.", ErrorsFound );
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
				IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( 3 ), cAlphaArgs( 3 ), "Use a choice from the available mesh type keys.", ErrorsFound );
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
				IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( 4 ), cAlphaArgs( 4 ), "Use a choice from the available mesh type keys.", ErrorsFound );
			}}

			// Soil properties, validated min/max by IP
			PipingSystemDomains( DomainNum ).GroundProperties.Conductivity = rNumericArgs( 10 );
			PipingSystemDomains( DomainNum ).GroundProperties.Density = rNumericArgs( 11 );
			PipingSystemDomains( DomainNum ).GroundProperties.SpecificHeat = rNumericArgs( 12 );

			// Moisture properties, validated min/max by IP, and converted to a fraction for computation here
			PipingSystemDomains( DomainNum ).Moisture.Theta_liq = rNumericArgs( 13 ) / 100.0;
			PipingSystemDomains( DomainNum ).Moisture.Theta_sat = rNumericArgs( 14 ) / 100.0;

			// check if there is a basement
			if ( UtilityRoutines::SameString( cAlphaArgs( 7 ), "YES" ) ) {
				PipingSystemDomains( DomainNum ).HasBasement = true;
			} else if ( UtilityRoutines::SameString( cAlphaArgs( 7 ), "NO" ) ) {
				PipingSystemDomains( DomainNum ).HasBasement = false;
			} else {
				IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( 7 ), cAlphaArgs( 7 ), "Must enter either yes or no.", ErrorsFound );
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
					IssueSevereInputFieldErrorRealEntry( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cNumericFieldNames( CurIndex ), rNumericArgs( CurIndex ), "Basement width must be a positive nonzero value.", ErrorsFound );
				}

				CurIndex = 16;
				PipingSystemDomains( DomainNum ).BasementZone.Depth = rNumericArgs( CurIndex );
				if ( PipingSystemDomains( DomainNum ).BasementZone.Depth <= 0.0 ) {
					IssueSevereInputFieldErrorRealEntry( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cNumericFieldNames( CurIndex ), rNumericArgs( CurIndex ), "Basement depth must be a positive nonzero value.", ErrorsFound );
				}

				// check for dimension shift
				CurIndex = 8;
				if ( UtilityRoutines::SameString( cAlphaArgs( CurIndex ), "YES" ) ) {
					PipingSystemDomains( DomainNum ).BasementZone.ShiftPipesByWidth = true;
				} else if ( UtilityRoutines::SameString( cAlphaArgs( CurIndex ), "NO" ) ) {
					PipingSystemDomains( DomainNum ).BasementZone.ShiftPipesByWidth = false;
				} else {
					IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Must enter either yes or no.", ErrorsFound );
				}

				// get boundary condition model names and indices --error check
				CurIndex = 9;
				PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMName = cAlphaArgs( CurIndex );
				PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex = UtilityRoutines::FindItemInList( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMName, OSCM );
				if ( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex <= 0 ) {
					IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Could not match with an Other Side Conditions Model input object.", ErrorsFound );
				} else {
					NumSurfacesWithThisOSCM = GetSurfaceCountForOSCM( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex );
					if ( NumSurfacesWithThisOSCM <= 0 ) {
						IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Entry matched an Other Side Conditions Model, but no surfaces were found to be using this Other Side Conditions Model.", ErrorsFound );
					} else {
						PipingSystemDomains( DomainNum ).BasementZone.WallSurfacePointers.allocate( NumSurfacesWithThisOSCM );
						PipingSystemDomains( DomainNum ).BasementZone.WallSurfacePointers = GetSurfaceIndecesForOSCM( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex, NumSurfacesWithThisOSCM );
					}
				}

				CurIndex = 10;
				PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMName = cAlphaArgs( CurIndex );
				PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex = UtilityRoutines::FindItemInList( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMName, OSCM );
				if ( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex <= 0 ) {
					IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Could not match with an Other Side Conditions Model input object.", ErrorsFound );
				} else {
					NumSurfacesWithThisOSCM = GetSurfaceCountForOSCM( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex );
					if ( NumSurfacesWithThisOSCM <= 0 ) {
						IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ug_GeneralDomain, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Entry matched an Other Side Conditions Model, but no surfaces were found to be using this Other Side Conditions Model.", ErrorsFound );
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
				inputProcessor->getObjectItem( ObjName_ZoneCoupled_Slab, ZoneCoupledDomainCtr, cAlphaArgs,
					NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				// Get the name, validate
				Domain( ZoneCoupledDomainCtr ).ObjName = cAlphaArgs( 1 );
				GlobalNames::VerifyUniqueInterObjectName( GroundDomainUniqueNames, cAlphaArgs( 1 ), ObjName_ZoneCoupled_Slab, cAlphaFieldNames( 1 ), ErrorsFound );

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
				if ( UtilityRoutines::SameString( cAlphaArgs( 5 ), "INGRADE" ) ) {
					PipingSystemDomains( DomainCtr ).SlabInGradeFlag = true;
				} else if ( UtilityRoutines::SameString( cAlphaArgs( 5 ), "ONGRADE" ) ) {
					PipingSystemDomains( DomainCtr ).SlabInGradeFlag = false;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + "=" + cAlphaArgs( 5 ) );
					ShowContinueError( "Found in: " + Domain( ZoneCoupledDomainCtr ).ObjName );
					ErrorsFound = true;
				}

				// Get slab material properties
				if ( PipingSystemDomains( DomainCtr ).SlabInGradeFlag ) {
					Domain( ZoneCoupledDomainCtr ).SlabMaterial = cAlphaArgs( 6 );
					PipingSystemDomains( DomainCtr ).SlabMaterialNum = UtilityRoutines::FindItemInList( cAlphaArgs( 6 ), Material, TotMaterials );
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
					if ( UtilityRoutines::SameString( cAlphaArgs( 7 ), "NO" ) ) {
						PipingSystemDomains( DomainCtr ).HorizInsPresentFlag = false;
					} else if ( UtilityRoutines::SameString( cAlphaArgs( 7 ), "YES" ) ) {
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
					PipingSystemDomains( DomainCtr ).HorizInsMaterialNum = UtilityRoutines::FindItemInList( cAlphaArgs( 8 ), Material, TotMaterials );
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
					if ( UtilityRoutines::SameString( cAlphaArgs( 9 ), "PERIMETER" ) ) {
						PipingSystemDomains( DomainCtr ).FullHorizInsPresent = false;
						// Horizontal insulation perimeter width
						if ( Domain( ZoneCoupledDomainCtr ).HorizInsWidth > 0.0 ) {
							PipingSystemDomains( DomainCtr ).HorizInsWidth = Domain( ZoneCoupledDomainCtr ).HorizInsWidth;
						} else {
							ShowSevereError( "Invalid " + cNumericFieldNames( 10 ) );
							ShowContinueError( "Found in: " + Domain( ZoneCoupledDomainCtr ).ObjName );
							ErrorsFound = true;
						}
					} else if ( UtilityRoutines::SameString( cAlphaArgs( 9 ), "FULL" ) ) {
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
				if ( UtilityRoutines::SameString( cAlphaArgs( 10 ), "NO" ) ) {
					PipingSystemDomains( DomainCtr ).VertInsPresentFlag = false;
				} else if ( UtilityRoutines::SameString( cAlphaArgs( 10 ), "YES" ) ) {
					PipingSystemDomains( DomainCtr ).VertInsPresentFlag = true;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 10 ) + "=" + cAlphaArgs( 10 ) );
					ShowContinueError( "Found in: " + Domain( ZoneCoupledDomainCtr ).ObjName );
					ErrorsFound = true;
				}

				// Get vertical insulation material properties
				if ( PipingSystemDomains( DomainCtr ).VertInsPresentFlag ) {
					Domain( ZoneCoupledDomainCtr ).VertInsMaterial = cAlphaArgs( 11 );
					PipingSystemDomains( DomainCtr ).VertInsMaterialNum = UtilityRoutines::FindItemInList( cAlphaArgs( 11 ), Material, TotMaterials );
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
				if ( UtilityRoutines::SameString( cAlphaArgs( 12 ), "TIMESTEP" ) ) {
					PipingSystemDomains( DomainCtr ).SimTimestepFlag = true;
				} else if ( UtilityRoutines::SameString( cAlphaArgs( 12 ), "HOURLY" ) ) {
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
				PipingSystemDomains( DomainCtr ).ZoneCoupledOSCMIndex = UtilityRoutines::FindItemInList( Domain( ZoneCoupledDomainCtr ).OSCMName, OSCM );
				if ( PipingSystemDomains( DomainCtr ).ZoneCoupledOSCMIndex <= 0 ) {
					IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ZoneCoupled_Slab, cAlphaArgs( 1 ), cAlphaFieldNames( 4 ), cAlphaArgs( 4 ), "Could not match with an Other Side Conditions Model input object.", ErrorsFound );
					ErrorsFound = true;
				} else {
					NumSurfacesWithThisOSCM = GetSurfaceCountForOSCM( PipingSystemDomains( DomainCtr ).ZoneCoupledOSCMIndex );
					if ( NumSurfacesWithThisOSCM <= 0 ) {
						IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ZoneCoupled_Slab, cAlphaArgs( 1 ), cAlphaFieldNames( 4 ), cAlphaArgs( 4 ), "Entry matched an Other Side Conditions Model, but no surfaces were found to be using this Other Side Conditions Model.", ErrorsFound );
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
				PipingSystemDomains( DomainCtr ).Mesh.X.thisMeshDistribution = MeshDistribution::SymmetricGeometric;
				PipingSystemDomains( DomainCtr ).Mesh.Y.thisMeshDistribution = MeshDistribution::SymmetricGeometric;
				PipingSystemDomains( DomainCtr ).Mesh.Z.thisMeshDistribution = MeshDistribution::SymmetricGeometric;

				Real64 MeshCoefficient = rNumericArgs( 12 );
				if ( MeshCoefficient == 0.0 ) MeshCoefficient = 1.6;
				PipingSystemDomains( DomainCtr ).Mesh.X.GeometricSeriesCoefficient = MeshCoefficient;
				PipingSystemDomains( DomainCtr ).Mesh.Y.GeometricSeriesCoefficient = MeshCoefficient;
				PipingSystemDomains( DomainCtr ).Mesh.Z.GeometricSeriesCoefficient = MeshCoefficient;

				int MeshCount = rNumericArgs( 13 );
				if ( MeshCount == 0.0 ) MeshCount = 6;
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

				PipingSystemDomains( DomainCtr ).NumSlabCells = PipingSystemDomains( DomainCtr ).Mesh.Y.RegionMeshCount; // Need to clean this out at some point

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
			inputProcessor->getObjectItem( ObjName_ZoneCoupled_Basement, BasementCtr, cAlphaArgs,
				NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			// Get the name, validate
			Domain( BasementCtr ).ObjName = cAlphaArgs( 1 );
			GlobalNames::VerifyUniqueInterObjectName( GroundDomainUniqueNames, cAlphaArgs( 1 ), ObjName_ZoneCoupled_Basement, cAlphaFieldNames( 1 ), ErrorsFound );

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
			PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex = UtilityRoutines::FindItemInList( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMName, OSCM );
			if ( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex <= 0 ) {
				IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ZoneCoupled_Basement, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Could not match with an Other Side Conditions Model input object.", ErrorsFound );
			} else {
				NumSurfacesWithThisOSCM = GetSurfaceCountForOSCM( PipingSystemDomains( DomainNum ).BasementZone.FloorBoundaryOSCMIndex );
				if ( NumSurfacesWithThisOSCM <= 0 ) {
					IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ZoneCoupled_Basement, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Entry matched an Other Side Conditions Model, but no surfaces were found to be using this Other Side Conditions Model.", ErrorsFound );
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
			PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex = UtilityRoutines::FindItemInList( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMName, OSCM );
			if ( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex <= 0 ) {
				IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ZoneCoupled_Basement, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Could not match with an Other Side Conditions Model input object.", ErrorsFound );
				ErrorsFound = true;
			} else {
				NumSurfacesWithThisOSCM = GetSurfaceCountForOSCM( PipingSystemDomains( DomainNum ).BasementZone.WallBoundaryOSCMIndex );
				if ( NumSurfacesWithThisOSCM <= 0 ) {
					IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_ZoneCoupled_Basement, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Entry matched an Other Side Conditions Model, but no surfaces were found to be using this Other Side Conditions Model.", ErrorsFound );
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
			int meshCount;
		        if ( lNumericFieldBlanks( 13 ) ) {
				meshCount = 4;
			} else {
				meshCount = rNumericArgs( 13 );
			}
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
			if ( UtilityRoutines::SameString( cAlphaArgs( 5 ), "NO" ) ) {
				PipingSystemDomains( DomainNum ).HorizInsPresentFlag = false;
			} else if ( UtilityRoutines::SameString( cAlphaArgs( 5 ), "YES" ) ) {
				PipingSystemDomains( DomainNum ).HorizInsPresentFlag = true;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 5 ) + "=" + cAlphaArgs( 5 ) );
				ShowContinueError( "Found in: " + PipingSystemDomains( DomainNum ).Name );
				ErrorsFound = true;
			}

			// Get horizontal insulation material properties
			if ( PipingSystemDomains( DomainNum ).HorizInsPresentFlag ) {
				Domain( BasementCtr ).HorizInsMaterial = cAlphaArgs( 6 );
				PipingSystemDomains( DomainNum ).HorizInsMaterialNum = UtilityRoutines::FindItemInList( cAlphaArgs( 6 ), Material, TotMaterials );
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
				if ( UtilityRoutines::SameString( cAlphaArgs( 7 ), "PERIMETER" ) ) {
					PipingSystemDomains( DomainNum ).FullHorizInsPresent = false;
					// Horizontal insulation perimeter width
					if ( Domain( BasementCtr ).HorizInsWidth > 0.0 ) {
						PipingSystemDomains( DomainNum ).HorizInsWidth = Domain( BasementCtr ).HorizInsWidth;
					} else {
						ShowSevereError( "Invalid " + cNumericFieldNames( 10 ) );
						ShowContinueError( "Found in: " + PipingSystemDomains( DomainNum ).Name );
						ErrorsFound = true;
					}
				} else if ( UtilityRoutines::SameString( cAlphaArgs( 7 ), "FULL" ) ) {
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
			if ( UtilityRoutines::SameString( cAlphaArgs( 9 ), "NO" ) ) {
				PipingSystemDomains( DomainNum ).VertInsPresentFlag = false;
			} else if ( UtilityRoutines::SameString( cAlphaArgs( 9 ), "YES" ) ) {
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
				PipingSystemDomains( DomainNum ).VertInsMaterialNum = UtilityRoutines::FindItemInList( cAlphaArgs( 10 ), Material, TotMaterials );
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
			if ( UtilityRoutines::SameString( cAlphaArgs( 11 ), "TIMESTEP" ) ) {
				PipingSystemDomains( DomainNum ).SimTimestepFlag = true;
			} else if ( UtilityRoutines::SameString( cAlphaArgs( 11 ), "HOURLY" ) ) {
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
		int CurIndex;
		int NumAlphasBeforeSegmentOne;

		for ( PipeCircuitCounter = 1; PipeCircuitCounter <= NumPipeCircuits; ++PipeCircuitCounter ) {

			// Read all the inputs for this pipe circuit
			inputProcessor->getObjectItem( ObjName_Circuit, PipeCircuitCounter, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			// Get the name, validate
			PipingSystemCircuits( PipeCircuitCounter ).Name = cAlphaArgs( 1 );
			UtilityRoutines::IsNameEmpty(cAlphaArgs( 1 ), cCurrentModuleObject, ErrorsFound);

			// Read pipe thermal properties, validated by IP
			PipingSystemCircuits( PipeCircuitCounter ).PipeProperties.Conductivity = rNumericArgs( 1 );
			PipingSystemCircuits( PipeCircuitCounter ).PipeProperties.Density = rNumericArgs( 2 );
			PipingSystemCircuits( PipeCircuitCounter ).PipeProperties.SpecificHeat = rNumericArgs( 3 );

			// Read pipe sizing, validated individually by IP, validated comparison here
			PipingSystemCircuits( PipeCircuitCounter ).PipeSize.InnerDia = rNumericArgs( 4 );
			PipingSystemCircuits( PipeCircuitCounter ).PipeSize.OuterDia = rNumericArgs( 5 );
			if ( PipingSystemCircuits( PipeCircuitCounter ).PipeSize.InnerDia >= PipingSystemCircuits( PipeCircuitCounter ).PipeSize.OuterDia ) {
				CurIndex = 5;
				IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_Circuit, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Outer diameter must be greater than inner diameter.", ErrorsFound );
			}

			// Read design flow rate, validated positive by IP
			PipingSystemCircuits( PipeCircuitCounter ).DesignVolumeFlowRate = rNumericArgs( 6 );

			// Read inlet and outlet node names and validate them
			PipingSystemCircuits( PipeCircuitCounter ).InletNodeName = cAlphaArgs( 2 );
			PipingSystemCircuits( PipeCircuitCounter ).InletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, ObjName_Circuit, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			if ( PipingSystemCircuits( PipeCircuitCounter ).InletNodeNum == 0 ) {
				CurIndex = 2;
				IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_Circuit, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Bad node name.", ErrorsFound );
			}
			PipingSystemCircuits( PipeCircuitCounter ).OutletNodeName = cAlphaArgs( 3 );
			PipingSystemCircuits( PipeCircuitCounter ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, ObjName_Circuit, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			if ( PipingSystemCircuits( PipeCircuitCounter ).OutletNodeNum == 0 ) {
				CurIndex = 3;
				IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_Circuit, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Bad node name.", ErrorsFound );
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
					IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_Circuit, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Expected a pipe segment name, check pipe segment count input field.", ErrorsFound );
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
		using namespace DataIPShortCuts;

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ReadPipeSegmentInputs" );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SegmentCtr;
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		int CurIndex;

		// Read in all pipe segments
		for ( SegmentCtr = 1; SegmentCtr <= NumPipeSegmentsInInput; ++SegmentCtr ) {

			// Read all inputs for this pipe segment
			inputProcessor->getObjectItem( ObjName_Segment, SegmentCtr, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			// Get the name, validate
			PipingSystemSegments( SegmentCtr ).Name = cAlphaArgs( 1 );
			UtilityRoutines::IsNameEmpty(cAlphaArgs( 1 ), cCurrentModuleObject, ErrorsFound);
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
				IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_Segment, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), cAlphaArgs( CurIndex ), "Invalid flow direction, use one of the available keys.", ErrorsFound );
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
			inputProcessor->getObjectItem( ObjName_HorizTrench, HorizontalGHXCtr, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			// Get the name, validate
			HGHX( HorizontalGHXCtr ).ObjName = cAlphaArgs( 1 );
			UtilityRoutines::IsNameEmpty(cAlphaArgs( 1 ), cCurrentModuleObject, ErrorsFound);

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
				// CALL IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_Circuit, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), &
				//                            cAlphaArgs( CurIndex ), 'Outer diameter must be greater than inner diameter.', ErrorsFound )
			}

			// Read design flow rate, validated positive by IP
			PipingSystemCircuits( CircuitCtr ).DesignVolumeFlowRate = HGHX( HorizontalGHXCtr ).DesignFlowRate;

			// Read inlet and outlet node names and validate them
			PipingSystemCircuits( CircuitCtr ).InletNodeName = HGHX( HorizontalGHXCtr ).InletNodeName;
			PipingSystemCircuits( CircuitCtr ).InletNodeNum = GetOnlySingleNode( PipingSystemCircuits( CircuitCtr ).InletNodeName, ErrorsFound, ObjName_HorizTrench, HGHX( HorizontalGHXCtr ).ObjName, NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			if ( PipingSystemCircuits( CircuitCtr ).InletNodeNum == 0 ) {
				CurIndex = 2;
				// CALL IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_Circuit, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), &
				//                                cAlphaArgs( CurIndex ), 'Bad node name.', ErrorsFound )
			}
			PipingSystemCircuits( CircuitCtr ).OutletNodeName = HGHX( HorizontalGHXCtr ).OutletNodeName;
			PipingSystemCircuits( CircuitCtr ).OutletNodeNum = GetOnlySingleNode( PipingSystemCircuits( CircuitCtr ).OutletNodeName, ErrorsFound, ObjName_HorizTrench, HGHX( HorizontalGHXCtr ).ObjName, NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			if ( PipingSystemCircuits( CircuitCtr ).OutletNodeNum == 0 ) {
				CurIndex = 3;
				// CALL IssueSevereInputFieldErrorStringEntry( RoutineName, ObjName_Circuit, cAlphaArgs( 1 ), cAlphaFieldNames( CurIndex ), &
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

		for ( int SegmentCtr = 1; SegmentCtr <= TotalNumSegments; ++SegmentCtr ) {

			if ( ! PipingSystemSegments( SegmentCtr ).IsActuallyPartOfAHorizontalTrench ) {

				SetupOutputVariable( "Pipe Segment Inlet Temperature", OutputProcessor::Unit::C, PipingSystemSegments( SegmentCtr ).InletTemperature, "Plant", "Average", PipingSystemSegments( SegmentCtr ).Name );
				SetupOutputVariable( "Pipe Segment Outlet Temperature", OutputProcessor::Unit::C, PipingSystemSegments( SegmentCtr ).OutletTemperature, "Plant", "Average", PipingSystemSegments( SegmentCtr ).Name );

				SetupOutputVariable( "Pipe Segment Fluid Heat Transfer Rate", OutputProcessor::Unit::W, PipingSystemSegments( SegmentCtr ).FluidHeatLoss, "Plant", "Average", PipingSystemSegments( SegmentCtr ).Name );

			}

		}

		for ( int PipeCircuitCounter = 1; PipeCircuitCounter <= TotalNumCircuits; ++PipeCircuitCounter ) {

			if ( ! PipingSystemCircuits( PipeCircuitCounter ).IsActuallyPartOfAHorizontalTrench ) {

				SetupOutputVariable( "Pipe Circuit Mass Flow Rate", OutputProcessor::Unit::kg_s, PipingSystemCircuits( PipeCircuitCounter ).CurCircuitFlowRate, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );

				SetupOutputVariable( "Pipe Circuit Inlet Temperature", OutputProcessor::Unit::C, PipingSystemCircuits( PipeCircuitCounter ).InletTemperature, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );
				SetupOutputVariable( "Pipe Circuit Outlet Temperature", OutputProcessor::Unit::C, PipingSystemCircuits( PipeCircuitCounter ).OutletTemperature, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );

				SetupOutputVariable( "Pipe Circuit Fluid Heat Transfer Rate", OutputProcessor::Unit::W, PipingSystemCircuits( PipeCircuitCounter ).FluidHeatLoss, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );

			} else { // it is a horizontal trench

				SetupOutputVariable( "Ground Heat Exchanger Mass Flow Rate", OutputProcessor::Unit::kg_s, PipingSystemCircuits( PipeCircuitCounter ).CurCircuitFlowRate, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );

				SetupOutputVariable( "Ground Heat Exchanger Inlet Temperature", OutputProcessor::Unit::C, PipingSystemCircuits( PipeCircuitCounter ).InletTemperature, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );
				SetupOutputVariable( "Ground Heat Exchanger Outlet Temperature", OutputProcessor::Unit::C, PipingSystemCircuits( PipeCircuitCounter ).OutletTemperature, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );

				SetupOutputVariable( "Ground Heat Exchanger Fluid Heat Transfer Rate", OutputProcessor::Unit::W, PipingSystemCircuits( PipeCircuitCounter ).FluidHeatLoss, "Plant", "Average", PipingSystemCircuits( PipeCircuitCounter ).Name );

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
			SetupOutputVariable( "GroundDomain Slab Zone Coupled Surface Heat Flux", OutputProcessor::Unit::W_m2, PipingSystemDomains( DomainNum ).HeatFlux, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
			SetupOutputVariable( "GroundDomain Slab Zone Coupled Surface Temperature", OutputProcessor::Unit::C, PipingSystemDomains( DomainNum ).ZoneCoupledSurfaceTemp, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		} else if ( PipingSystemDomains( DomainNum ).HasZoneCoupledBasement ) {
			// Zone-coupled basement wall outputs
			SetupOutputVariable( "GroundDomain Basement Wall Interface Heat Flux", OutputProcessor::Unit::W_m2, PipingSystemDomains( DomainNum ).WallHeatFlux, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
			SetupOutputVariable( "GroundDomain Basement Wall Interface Temperature", OutputProcessor::Unit::C, PipingSystemDomains( DomainNum ).BasementWallTemp, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
			// Zone-coupled basement floor outputs
			SetupOutputVariable( "GroundDomain Basement Floor Interface Heat Flux", OutputProcessor::Unit::W_m2, PipingSystemDomains( DomainNum ).FloorHeatFlux, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
			SetupOutputVariable( "GroundDomain Basement Floor Interface Temperature", OutputProcessor::Unit::C, PipingSystemDomains( DomainNum ).BasementFloorTemp, "Zone", "Average", PipingSystemDomains( DomainNum ).Name );
		}

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

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitPipingSystems" );

		auto & thisDomain = PipingSystemDomains( DomainNum );
		auto & thisCircuit = PipingSystemCircuits( CircuitNum );

		// Do any one-time initializations
		if ( thisCircuit.NeedToFindOnPlantLoop ) {

			int TypeToLookFor;
			if ( thisCircuit.IsActuallyPartOfAHorizontalTrench ) {
				TypeToLookFor = DataPlant::TypeOf_GrndHtExchgHorizTrench;
			} else {
				TypeToLookFor = DataPlant::TypeOf_PipingSystemPipeCircuit;
			}

			bool errFlag = false;
			DataPlant::ScanPlantLoopsForObject( thisCircuit.Name, TypeToLookFor, thisCircuit.LoopNum, thisCircuit.LoopSideNum, thisCircuit.BranchNum, thisCircuit.CompNum, _, _, _, _, _, errFlag );
			if ( errFlag ) {
				ShowFatalError( "PipingSystems:" + RoutineName + ": Program terminated due to previous condition(s)." );
			}

			// Once we find ourselves on the plant loop, we can do other things
			Real64 rho = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( thisCircuit.LoopNum ).FluidName, DataGlobals::InitConvTemp, DataPlant::PlantLoop( thisCircuit.LoopNum ).FluidIndex, RoutineName );
			thisCircuit.DesignMassFlowRate = thisCircuit.DesignVolumeFlowRate * rho;
			thisCircuit.NeedToFindOnPlantLoop = false;

		}

		if ( thisDomain.DomainNeedsToBeMeshed ) {

			thisDomain.developMesh();

			// would be OK to do some post-mesh error handling here I think
			for ( int CircCtr = 1; CircCtr <= isize( thisDomain.CircuitIndeces ); ++CircCtr ) {
				for ( int SegCtr = 1; SegCtr <= isize( PipingSystemCircuits( thisDomain.CircuitIndeces( CircCtr ) ).PipeSegmentIndeces ); ++SegCtr ) {
					int SegmentIndex = PipingSystemCircuits( thisDomain.CircuitIndeces( CircCtr ) ).PipeSegmentIndeces( SegCtr );
					if ( ! PipingSystemSegments( SegmentIndex ).PipeCellCoordinatesSet ) {
						ShowSevereError( "PipingSystems:" + RoutineName + ":Pipe segment index not set." );
						ShowContinueError( "...Possibly because pipe segment was placed outside of the domain." );
						ShowContinueError( "...Verify piping system domain inputs, circuits, and segments." );
						ShowFatalError( "Preceding error causes program termination" );
					}
				}
			}

			thisDomain.DomainNeedsToBeMeshed = false;

		}

		// The time init should be done here before we DoOneTimeInits because the DoOneTimeInits
		// includes a ground temperature initialization, which is based on the Cur%CurSimTimeSeconds variable
		// which would be carried over from the previous environment
		thisDomain.Cur.CurSimTimeStepSize = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
		thisDomain.Cur.CurSimTimeSeconds = ( DataGlobals::DayOfSim - 1 ) * 24 + ( DataGlobals::HourOfDay - 1 ) + ( DataGlobals::TimeStep - 1 ) * DataGlobals::TimeStepZone + DataHVACGlobals::SysTimeElapsed;

		// There are also some inits that are "close to one time" inits...(one-time in standalone, each envrn in E+)
		if ( ( DataGlobals::BeginSimFlag && thisDomain.BeginSimInit ) || ( DataGlobals::BeginEnvrnFlag && thisDomain.BeginSimEnvrn ) ) {

			// this seemed to clean up a lot of reverse DD stuff because fluid thermal properties were
			// being based on the inlet temperature, which wasn't updated until later
			thisCircuit.CurCircuitInletTemp = DataLoopNode::Node( thisCircuit.InletNodeNum ).Temp;
			thisCircuit.InletTemperature = thisCircuit.CurCircuitInletTemp;

			DoOneTimeInitializations( DomainNum, CircuitNum );

			thisDomain.BeginSimInit = false;
			thisDomain.BeginSimEnvrn = false;

		}
		if ( ! DataGlobals::BeginSimFlag ) thisDomain.BeginSimInit = true;
		if ( ! DataGlobals::BeginEnvrnFlag ) thisDomain.BeginSimEnvrn = true;

		// Shift history arrays only if necessary
		if ( std::abs( thisDomain.Cur.CurSimTimeSeconds - thisDomain.Cur.PrevSimTimeSeconds ) > 1.0e-6 ) {
			thisDomain.Cur.PrevSimTimeSeconds = thisDomain.Cur.CurSimTimeSeconds;
			thisDomain.ShiftTemperaturesForNewTimeStep();
			thisDomain.DomainNeedsSimulation = true;
		}

		// Get the mass flow and inlet temperature to use for this time step
		int InletNodeNum = thisCircuit.InletNodeNum;
		int OutletNodeNum = thisCircuit.OutletNodeNum;
		thisCircuit.CurCircuitInletTemp = DataLoopNode::Node( InletNodeNum ).Temp;

		// request design, set component flow will decide what to give us based on restrictions and flow lock status
		thisCircuit.CurCircuitFlowRate = thisCircuit.DesignMassFlowRate;
		PlantUtilities::SetComponentFlowRate( thisCircuit.CurCircuitFlowRate, InletNodeNum, OutletNodeNum, thisCircuit.LoopNum, thisCircuit.LoopSideNum, thisCircuit.BranchNum, thisCircuit.CompNum );

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

		int OutletNodeNum = PipingSystemCircuits( CircuitNum ).OutletNodeNum;
		auto const & out_cell( PipingSystemCircuits( CircuitNum ).CircuitOutletCell );
		DataLoopNode::Node( OutletNodeNum ).Temp = PipingSystemDomains( DomainNum ).Cells( out_cell.X, out_cell.Y, out_cell.Z ).PipeCellData.Fluid.Temperature;

	}

	void
	IssueSevereInputFieldErrorStringEntry(
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
	IssueSevereInputFieldErrorRealEntry(
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

		int RetVal = 0;
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
			if ( ISWAP1 != I ) std::swap( X( I ), X( ISWAP1 ) );
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
	FullDomainStructureInfo::IsConverged_CurrentToPrevIteration()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		Real64 LocalMax( 0.0 );
		for ( int X = 0, X_end = this->x_max_index; X <= X_end; ++X ) {
			for ( int Y = 0, Y_end = this->y_max_index; Y <= Y_end; ++Y ) {
				for ( int Z = 0, Z_end = this->z_max_index; Z <= Z_end; ++Z ) {
					auto const & cell( this->Cells( X, Y, Z ) );
					LocalMax = max( LocalMax, std::abs( cell.Temperature - cell.Temperature_PrevIteration ) );
				}
			}
		}

		return ( LocalMax < this->SimControls.Convergence_CurrentToPrevIteration );
	}

	bool
	IsConverged_PipeCurrentToPrevIteration(
		int const CircuitNum,
		CartesianCell const & CellToCheck
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		Real64 ThisCellMax;

		Real64 MaxDivAmount = 0.0;
		for ( int RadialCtr = CellToCheck.PipeCellData.Soil.l1(); RadialCtr <= CellToCheck.PipeCellData.Soil.u1(); ++RadialCtr ) {
			auto const & radCell = CellToCheck.PipeCellData.Soil( RadialCtr );
			ThisCellMax = std::abs( radCell.Temperature - radCell.Temperature_PrevIteration );
			if ( ThisCellMax > MaxDivAmount ) {
				MaxDivAmount = ThisCellMax;
			}
		}
		//'also do the pipe cell
		ThisCellMax = std::abs( CellToCheck.PipeCellData.Pipe.Temperature - CellToCheck.PipeCellData.Pipe.Temperature_PrevIteration );
		if ( ThisCellMax > MaxDivAmount ) {
			MaxDivAmount = ThisCellMax;
		}
		//'also do the water cell
		ThisCellMax = std::abs( CellToCheck.PipeCellData.Fluid.Temperature - CellToCheck.PipeCellData.Fluid.Temperature_PrevIteration );
		if ( ThisCellMax > MaxDivAmount ) {
			MaxDivAmount = ThisCellMax;
		}
		//'also do insulation if it exists
		if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
			ThisCellMax = std::abs( CellToCheck.PipeCellData.Insulation.Temperature - CellToCheck.PipeCellData.Insulation.Temperature_PrevIteration );
			if ( ThisCellMax > MaxDivAmount ) {
				MaxDivAmount = ThisCellMax;
			}
		}

		return ( MaxDivAmount < PipingSystemCircuits( CircuitNum ).Convergence_CurrentToPrevIteration );

	}

	void
	FullDomainStructureInfo::ShiftTemperaturesForNewTimeStep()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		for ( int X = 0, X_end = this->x_max_index; X <= X_end; ++X ) {
			for ( int Y = 0, Y_end = this->y_max_index; Y <= Y_end; ++Y ) {
				for ( int Z = 0, Z_end = this->z_max_index; Z <= Z_end; ++Z ) {
					auto & cell( this->Cells( X, Y, Z ) );

					cell.Temperature_PrevTimeStep = cell.Temperature;

					if ( cell.cellType == CellType::Pipe ) {

						for ( int RadCtr = cell.PipeCellData.Soil.l1(); RadCtr <= cell.PipeCellData.Soil.u1(); ++RadCtr ) {

							cell.PipeCellData.Soil( RadCtr ).Temperature_PrevTimeStep = cell.PipeCellData.Soil( RadCtr ).Temperature;

						}

						cell.PipeCellData.Fluid.Temperature_PrevTimeStep = cell.PipeCellData.Fluid.Temperature;

						cell.PipeCellData.Pipe.Temperature_PrevTimeStep = cell.PipeCellData.Pipe.Temperature;

						cell.PipeCellData.Insulation.Temperature_PrevTimeStep = cell.PipeCellData.Insulation.Temperature;

					}

				}
			}
		}

	}

	void
	FullDomainStructureInfo::ShiftTemperaturesForNewIteration()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na


		for ( int X = 0, X_end = this->x_max_index; X <= X_end; ++X ) {
			for ( int Y = 0, Y_end = this->y_max_index; Y <= Y_end; ++Y ) {
				for ( int Z = 0, Z_end = this->z_max_index; Z <= Z_end; ++Z ) {
					auto & cell( this->Cells( X, Y, Z ) );

					cell.Temperature_PrevIteration = cell.Temperature;

					if ( cell.cellType == CellType::Pipe ) {

						for ( int RadCtr = cell.PipeCellData.Soil.l1(); RadCtr <= cell.PipeCellData.Soil.u1(); ++RadCtr ) {

							cell.PipeCellData.Soil( RadCtr ).Temperature_PrevIteration = cell.PipeCellData.Soil( RadCtr ).Temperature;

						}

						cell.PipeCellData.Fluid.Temperature_PrevIteration = cell.PipeCellData.Fluid.Temperature;

						cell.PipeCellData.Pipe.Temperature_PrevIteration = cell.PipeCellData.Pipe.Temperature;

						cell.PipeCellData.Insulation.Temperature_PrevIteration = cell.PipeCellData.Insulation.Temperature;

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

		if ( ThisPipeCell.cellType == CellType::Pipe ) { // It better be!

			for ( int RadCtr = ThisPipeCell.PipeCellData.Soil.l1(); RadCtr <= ThisPipeCell.PipeCellData.Soil.u1(); ++RadCtr ) {
				ThisPipeCell.PipeCellData.Soil( RadCtr ).Temperature_PrevIteration = ThisPipeCell.PipeCellData.Soil( RadCtr ).Temperature;
			}

			ThisPipeCell.PipeCellData.Fluid.Temperature_PrevIteration = ThisPipeCell.PipeCellData.Fluid.Temperature;

			ThisPipeCell.PipeCellData.Pipe.Temperature_PrevIteration = ThisPipeCell.PipeCellData.Pipe.Temperature;

			ThisPipeCell.PipeCellData.Insulation.Temperature_PrevIteration = ThisPipeCell.PipeCellData.Insulation.Temperature;

		}

	}

	bool
	FullDomainStructureInfo::CheckForOutOfRangeTemps()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		Real64 const MaxLimit = this->SimControls.MaximumTemperatureLimit;
		Real64 const MinLimit = this->SimControls.MinimumTemperatureLimit;

		auto const & Cells( this->Cells );
		for ( std::size_t i = 0, e = Cells.size(); i < e; ++i ) {
			double const Temperature( Cells[ i ].Temperature );
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

		switch ( direction ) {
		case Direction::PositiveY:
			return this->YNormalArea();
		case Direction::NegativeY:
			return this->YNormalArea();
		case Direction::PositiveX:
			return this->XNormalArea();
		case Direction::NegativeX:
			return this->XNormalArea();
		case Direction::PositiveZ:
			return this->ZNormalArea();
		case Direction::NegativeZ:
			return this->ZNormalArea();
		default:
			assert( false );
		}

		return 0;
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
		Real64 MinimumSoilRadius;

		//'calculate pipe radius
		Real64 PipeOuterRadius = PipeSizes.OuterDia / 2.0;
		Real64 PipeInnerRadius = PipeSizes.InnerDia / 2.0;

		//'--we will work from inside out, calculating dimensions and instantiating variables--
		//'first instantiate the water cell
		c.Fluid = FluidCellInformation( PipeInnerRadius, CellDepth );

		//'then the pipe cell
		c.Pipe = RadialCellInformation( ( PipeOuterRadius + PipeInnerRadius ) / 2.0, PipeInnerRadius, PipeOuterRadius );

		//'then the insulation if we have it
		if ( InsulationThickness > 0.0 ) {
			InsulationInnerRadius = PipeOuterRadius;
			InsulationOuterRadius = InsulationInnerRadius + InsulationThickness;
			InsulationCentroid = ( InsulationInnerRadius + InsulationOuterRadius ) / 2.0;
			c.Insulation = RadialCellInformation( InsulationCentroid, InsulationInnerRadius, InsulationOuterRadius );
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
		Real64 Rval = MinimumSoilRadius + ( c.RadialSliceWidth / 2.0 );
		Real64 ThisSliceInnerRadius = MinimumSoilRadius;
		c.Soil( 0 ) = RadialCellInformation( Rval, ThisSliceInnerRadius, ThisSliceInnerRadius + c.RadialSliceWidth );

		//'then loop through the rest and assign them, each radius is simply one more slice thickness
		for ( int RadialCellCtr = 1; RadialCellCtr <= c.Soil.u1(); ++RadialCellCtr ) {
			Rval += c.RadialSliceWidth;
			ThisSliceInnerRadius += c.RadialSliceWidth;
			c.Soil( RadialCellCtr ) = RadialCellInformation( Rval, ThisSliceInnerRadius, ThisSliceInnerRadius + c.RadialSliceWidth );
		}

		//'also assign the interface cell surrounding the radial system
		c.InterfaceVolume = ( 1.0 - ( Pi / 4.0 ) ) * pow_2( GridCellWidth ) * CellDepth;

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
		int BoundaryListCount;
		bool XPartitionsExist;
		bool YPartitionsExist;
		bool ZPartitionsExist;

		// Object Data
		Array1D< GridRegion > XPartitionRegions;
		Array1D< GridRegion > YPartitionRegions;
		Array1D< GridRegion > ZPartitionRegions;

		//'****** LAYOUT PARTITIONS ******'
		this->createPartitionCenterList();

		if ( allocated( this->Partitions.X ) ) {
			XPartitionRegions.allocate( { 0, this->Partitions.X.u1() } );
			XPartitionsExist = true;
		} else {
			XPartitionRegions.allocate( { 0, 0 } );
			this->Partitions.X.allocate( { 0, 0 } );
			XPartitionsExist = false;
		}

		XPartitionRegions = this->createPartitionRegionList( this->Partitions.X, XPartitionsExist, this->Extents.Xmax, this->Partitions.X.u1() );

		if ( allocated( this->Partitions.Y ) ) {
			YPartitionRegions.allocate( { 0,this->Partitions.Y.u1() } );
			YPartitionsExist = true;
		} else {
			YPartitionRegions.allocate( { 0, 0 } );
			this->Partitions.Y.allocate( { 0, 0 } );
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
			this->createRegionList( XRegions, XPartitionRegions, this->Extents.Xmax, RegionType::XDirection, XPartitionsExist, _, _, this->XIndex, this->XWallIndex, this->InsulationXIndex );

			this->createRegionList( YRegions, YPartitionRegions, this->Extents.Ymax, RegionType::YDirection, YPartitionsExist, _, _, _, _, _, this->YIndex, this->YFloorIndex, this->InsulationYIndex );

			this->createRegionList( ZRegions, ZPartitionRegions, this->Extents.Zmax, RegionType::ZDirection, ZPartitionsExist, _, _, _, _, _, _, _, _, this->ZIndex, this->ZWallIndex, this->InsulationZIndex );
		} else if ( this->HasZoneCoupledSlab ) {
			this->createRegionList( XRegions, XPartitionRegions, this->Extents.Xmax, RegionType::XDirection, XPartitionsExist, _, _, this->XIndex, _, this->InsulationXIndex );

			this->createRegionList( YRegions, YPartitionRegions, this->Extents.Ymax, RegionType::YDirection, YPartitionsExist, _, _, _, _, _, this->YIndex, _, this->InsulationYIndex );

			this->createRegionList( ZRegions, ZPartitionRegions, this->Extents.Zmax, RegionType::ZDirection, ZPartitionsExist, _, _, _, _, _, _, _, _, this->ZIndex, _, this->InsulationZIndex );
		} else {
			this->createRegionList( XRegions, XPartitionRegions, this->Extents.Xmax, RegionType::XDirection, XPartitionsExist, this->BasementZone.BasementWallXIndex );

			this->createRegionList( YRegions, YPartitionRegions, this->Extents.Ymax, RegionType::YDirection, YPartitionsExist, _, this->BasementZone.BasementFloorYIndex );

			this->createRegionList( ZRegions, ZPartitionRegions, this->Extents.Zmax, RegionType::ZDirection, ZPartitionsExist );
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
					this->Partitions.X.push_back( MeshPartition( ThisSegment.PipeLocation.X, PartitionType::Pipe, PipeCellWidth ) );
				}

				if ( ! allocated( this->Partitions.Y ) ) {
					this->Partitions.Y.allocate( {0,0} );
					this->Partitions.Y( 0 ) = MeshPartition( ThisSegment.PipeLocation.Y, PartitionType::Pipe, PipeCellWidth );
				} else if ( ! MeshPartitionArray_Contains( this->Partitions.Y, ThisSegment.PipeLocation.Y ) ) {
					this->Partitions.Y.push_back( MeshPartition( ThisSegment.PipeLocation.Y, PartitionType::Pipe, PipeCellWidth ) );
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
						this->Partitions.X.push_back( MeshPartition( this->BasementZone.Width, PartitionType::BasementWall, SurfCellWidth ) );
					}
				}

				if ( this->BasementZone.Depth > 0 ) {
					SurfCellWidth = this->Extents.Ymax * BasementCellFraction;
					BasementDistFromBottom = this->Extents.Ymax - this->BasementZone.Depth;
					if ( !allocated( this->Partitions.Y ) ) {
						this->Partitions.Y.allocate( { 0, 0 } );
						this->Partitions.Y( 0 ) = MeshPartition( BasementDistFromBottom, PartitionType::BasementFloor, SurfCellWidth );
					} else if ( !MeshPartitionArray_Contains( this->Partitions.Y, BasementDistFromBottom ) ) {
						this->Partitions.Y.push_back( MeshPartition( BasementDistFromBottom, PartitionType::BasementFloor, SurfCellWidth ) );
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
					// Partition at insulation edges in the X direction, if horizontal insulation present
					if ( this->HorizInsPresentFlag ) {
						if ( !this->FullHorizInsPresent ) {
							// Side X direction - Insulation layer
							this->Partitions.X.push_back( MeshPartition( SideXLocation, PartitionType::XSide, CellWidth ) );
							// Side X direction - Basement Wall interface
							this->Partitions.X.push_back( MeshPartition( SideXWallLocation, PartitionType::XSideWall, InterfaceCellWidth ) );
							// Insulation Edge X direction
							this->Partitions.X.push_back( MeshPartition( SideXInsulationLocation, PartitionType::HorizInsXSide, InterfaceCellWidth ) );
						} else {
							// Side X direction - Insulation layer
							this->Partitions.X.push_back( MeshPartition( SideXLocation, PartitionType::XSide, CellWidth ) );
							// Side X direction -Basement Wall interface
							this->Partitions.X.push_back( MeshPartition( SideXWallLocation, PartitionType::XSideWall, InterfaceCellWidth ) );
						}
					} else {
						// Side X direction - Insulation layer
						this->Partitions.X.push_back( MeshPartition( SideXLocation, PartitionType::XSide, CellWidth ) );
						// Side X direction - Basement Wall interface
						this->Partitions.X.push_back( MeshPartition( SideXWallLocation, PartitionType::XSideWall, InterfaceCellWidth ) );
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
					// Partition at bottom edge of vertical insulation, if vertical insulation is present
					if ( this->VertInsPresentFlag && YInsulationLocation > FloorLocation + CellWidth ) {
						// Partition at basement floor interface
						this->Partitions.Y.push_back( MeshPartition( FloorLocation, PartitionType::FloorInside, InterfaceCellWidth ) );
						// Partition under the basement floor for insulation layer
						this->Partitions.Y.push_back( MeshPartition( UnderFloorLocation, PartitionType::UnderFloor, CellWidth ) );
						// Vertical-Insulation edge partition
						this->Partitions.Y.push_back( MeshPartition( YInsulationLocation, PartitionType::VertInsLowerEdge, InterfaceCellWidth ) );
					} else {
						this->Partitions.Y.push_back( MeshPartition( FloorLocation, PartitionType::FloorInside, InterfaceCellWidth ) );
						this->Partitions.Y.push_back( MeshPartition( UnderFloorLocation, PartitionType::UnderFloor, CellWidth ) );
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
					// Partition at insulation edges in the Z direction, if horizontal insulation present
					if ( this->HorizInsPresentFlag ) {
						if ( !this->FullHorizInsPresent ) {
							// Side Z direction - Insulation layer
							this->Partitions.Z.push_back( MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth ) );
							// Side Z direction - Basement Wall interface
							this->Partitions.Z.push_back( MeshPartition( SideZWallLocation, PartitionType::ZSideWall, InterfaceCellWidth ) );
							// Insulation Edge Z direction
							this->Partitions.Z.push_back( MeshPartition( SideZInsulationLocation, PartitionType::HorizInsZSide, InterfaceCellWidth ) );
						} else {
							// Side Z direction - Insulation layer
							this->Partitions.Z.push_back( MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth ) );
							// Side Z direction -Basement Wall interface
							this->Partitions.Z.push_back( MeshPartition( SideZWallLocation, PartitionType::ZSideWall, InterfaceCellWidth ) );
						}
					} else {
						// Side Z direction - Insulation layer
						this->Partitions.Z.push_back( MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth ) );
						// Side Z direction -Basement Wall interface
						this->Partitions.Z.push_back( MeshPartition( SideZWallLocation, PartitionType::ZSideWall, InterfaceCellWidth ) );
					}
				}
			}
		}

		// Zone-coupled slab
		if ( this->HasZoneCoupledSlab ) {
			// NOTE: the slab depth is still a depth from the ground surface, need to correct for this here.

			// Create X-direction partitions

			// Create partition at slab edges in the X direction
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

				// Partition at insulation edges in the X direction, if horizontal insulation present
				if ( this->HorizInsPresentFlag ) {
					if ( !this->FullHorizInsPresent ) {
						// Side X direction
						this->Partitions.X.push_back( MeshPartition( SideXLocation, PartitionType::XSide, CellWidth ) );
						// Insulation Edge X direction
						this->Partitions.X.push_back( MeshPartition( SideXInsulationLocation, PartitionType::HorizInsXSide, CellWidth ) );
					} else {
						// Side X direction
						this->Partitions.X.push_back( MeshPartition( SideXLocation, PartitionType::XSide, CellWidth ) );
					}
				} else {
					// Side X direction
					this->Partitions.X.push_back( MeshPartition( SideXLocation, PartitionType::XSide, CellWidth ) );
				}
			}

			// Create Y-direction partitions

			CellWidth = this->VertInsThickness;

			// Partition at bottom edge of vertical insulation, if vertical insulation present
			if ( this->VertInsPresentFlag ) {
				YInsulationLocation = this->Extents.Ymax - this->VertInsDepth + CellWidth / 2.0;
			}

			if ( this->SlabInGradeFlag) { // Slab in-grade case

				SlabDistFromBottom = this->Extents.Ymax - this->SlabThickness - CellWidth / 2.0;

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

					// Partition at bottom edge of vertical insulation, if vertical insulation present
					if ( this->VertInsPresentFlag ) {
						// Underslab partition
						this->Partitions.Y.push_back( MeshPartition( SlabDistFromBottom, PartitionType::UnderFloor, CellWidth ) );
						// Vertical-Insulation edge partition
						this->Partitions.Y.push_back( MeshPartition( YInsulationLocation, PartitionType::VertInsLowerEdge, CellWidth ) );
					} else {
						// Underslab partition
						this->Partitions.Y.push_back( MeshPartition( SlabDistFromBottom, PartitionType::UnderFloor, CellWidth ) );
					}
				}
			} else { // Slab on-grade case

				if ( !allocated( this->Partitions.Y ) ) {
					if ( this->VertInsPresentFlag ) {
						this->Partitions.Y.allocate( { 0, 0 } );
						// Vertical-Insulation edge partition
						this->Partitions.Y( 0 ) = MeshPartition( YInsulationLocation, PartitionType::VertInsLowerEdge, CellWidth );
					}
				} else if ( !MeshPartitionArray_Contains( this->Partitions.Y, YInsulationLocation ) ) {

					// Partition at bottom edge of vertical insulation, if vertical insulation present
					if ( this->VertInsPresentFlag ) {
						// Vertical-Insulation edge partition
						this->Partitions.Y.push_back( MeshPartition( YInsulationLocation, PartitionType::VertInsLowerEdge, CellWidth ) );
					}
				}
			}

			// Create Z-direction partitions

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

				// Partition at insulation edges in the Z direction, if horizontal insulation present
				if ( this->HorizInsPresentFlag ) {
					if ( !this->FullHorizInsPresent ) {
						// Side Z direction
						this->Partitions.Z.push_back( MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth ) );
						// Insulation Edge Z direction
						this->Partitions.Z.push_back( MeshPartition( SideZInsulationLocation, PartitionType::HorizInsZSide, CellWidth ) );
					} else {
						// Side Z direction
						this->Partitions.Z.push_back( MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth ) );
					}
				} else {
					// Side Z direction
					this->Partitions.Z.push_back( MeshPartition( SideZLocation, PartitionType::ZSide, CellWidth ) );
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

		if ( ! PartitionsExist ) {
			return ThesePartitionRegions;
		}

		//'loop across all partitions
		for ( int Index = ThesePartitionCenters.l1(); Index <= ThesePartitionCenters.u1(); ++Index ) {

			Real64 ThisCellWidthBy2 = ThesePartitionCenters( Index ).TotalWidth / 2.0;
			PartitionType ThisPartitionType = ThesePartitionCenters( Index ).partitionType;

			//'use this half width to validate the region and add it to the collection
			Real64 CellLeft = ThesePartitionCenters( Index ).rDimension - ThisCellWidthBy2;
			Real64 CellRight = ThesePartitionCenters( Index ).rDimension + ThisCellWidthBy2;

			// check to make sure this location is valid
			if ( CellLeft < 0.0 || CellRight > DirExtentMax ) {
				ShowSevereError( "PlantPipingSystems::" + RoutineName + ": Invalid partition location in domain." );
				ShowContinueError( "Occurs during mesh development for domain=" + this->Name );
				ShowContinueError( "A pipe or basement is located outside of the domain extents." );
				ShowFatalError( "Preceding error causes program termination." );
			}

			// Scan all grid regions to make sure this range doesn't fall within an already entered range
			for ( int SubIndex = 0; SubIndex <= Index - 1; ++SubIndex ) {
				// Coupled-basement model has adjacent partitions: ThesePartitionRegions( 0 ) and ThesePartitionRegions( 1 ) - SA
				if ( this->HasZoneCoupledBasement && Index ==1 ) {
					if ( IsInRange_BasementModel( CellLeft, ThesePartitionRegions( SubIndex ).Min, ThesePartitionRegions( SubIndex ).Max ) || IsInRangeReal( CellRight, ThesePartitionRegions( SubIndex ).Min, ThesePartitionRegions( SubIndex ).Max ) ) {

						ShowSevereError( "PlantPipingSystems::" + RoutineName + ": Invalid partition location in domain." );
						ShowContinueError( "Occurs during mesh development for domain=" + this->Name );
						ShowContinueError( "A mesh conflict was encountered where partitions were overlapping." );
						ShowContinueError( "Ensure that all pipes exactly line up or are separated to allow meshing in between them" );
						ShowContinueError( "Also verify the pipe and basement dimensions to avoid conflicts there." );
						ShowFatalError( "Preceding error causes program termination" );

					}

				} else {

					if ( IsInRangeReal( CellLeft, ThesePartitionRegions( SubIndex ).Min, ThesePartitionRegions( SubIndex ).Max ) || IsInRangeReal( CellRight, ThesePartitionRegions( SubIndex ).Min, ThesePartitionRegions( SubIndex ).Max ) ) {

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

		// Return value
		int RetVal;

		RetVal = 0;
		if ( PartitionsExist ) {
			for ( int Index = ThesePartitionRegions.l1(); Index <= ThesePartitionRegions.u1(); ++Index ) {
				// Coupled-basement model has adjacent partitions: ThesePartitionRegions( 0 ) and ThesePartitionRegions( 1 ). Do not add a region to the left of ThesePartitionRegions( 1 ).-SA
				if ( !DataGlobals::AnyBasementsInModel || ( DataGlobals::AnyBasementsInModel && ( Index == 0 || Index == 2 ) ) ) {
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

	void
	FullDomainStructureInfo::createRegionList(
		Array1D< GridRegion > & Regions,
		Array1D< GridRegion > const & ThesePartitionRegions,
		Real64 const DirExtentMax,
		RegionType const DirDirection,
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

		int cellCountUpToNow = 0.0;
		int regionIndex = 0;
		Array1D< Real64 > tempCellWidths( {0,-1} );

		if ( PartitionsExist ) {

			for ( int i = ThesePartitionRegions.l1(); i <= ThesePartitionRegions.u1(); ++i ) {
				auto & thisPartition( ThesePartitionRegions( i ) );

				if ( i == ThesePartitionRegions.l1() ) { // First partition
					// Create region to left of partition
					auto tempRegion( GridRegion( 0.0, thisPartition.Min, DirDirection, tempCellWidths ) );
					int potentialCellWidthsCount = this->getCellWidthsCount( DirDirection );
					if ( ( thisPartition.Min - 0.0 ) < 0.00001 ) {
						cellCountUpToNow += 1; // just one cell for extremely tight regions
					} else {
						cellCountUpToNow += potentialCellWidthsCount;
					}
					++regionIndex;
					this->getCellWidths( tempRegion, tempRegion.thisRegionType );
					Regions.push_back( tempRegion );
				} else if ( i == 1 && this->HasZoneCoupledBasement ) {
					cellCountUpToNow += 1; // don't add a left partition for partition index 1 of coupled basements
				} else { // All other partitions
					// Because of the way the index block below is structured, we need to update cellCount
					//  **after** we pass that block.  We could include logic below to do this, but this block
					//  already fits within the structure properly, so increment it here to account for the
					//  single cell partition layer that was applied at the **end** of the previous partition index
					++cellCountUpToNow;
					// Create region to left of partition
					auto & leftPartition( ThesePartitionRegions( i - 1 ) );
					auto tempRegion( GridRegion( leftPartition.Max, thisPartition.Min, DirDirection, tempCellWidths ) );
					int potentialCellWidthsCount = this->getCellWidthsCount( DirDirection );
					if ( ( thisPartition.Min - leftPartition.Max ) < 0.00001 ) {
						cellCountUpToNow += 1; // just one cell for extremely tight regions
					} else {
						cellCountUpToNow += potentialCellWidthsCount;
					}
					++regionIndex;
					this->getCellWidths( tempRegion, tempRegion.thisRegionType );
					Regions.push_back( tempRegion );
				}

				if ( thisPartition.thisRegionType == RegionType::BasementWall ) {
					if ( present( BasementWallXIndex ) ) BasementWallXIndex = cellCountUpToNow;
				} else if ( thisPartition.thisRegionType == RegionType::BasementFloor ) {
					if ( present( BasementFloorYIndex ) ) BasementFloorYIndex = cellCountUpToNow;
				} else if ( thisPartition.thisRegionType == RegionType::XSide ) {
					if ( present( XIndex ) ) XIndex = cellCountUpToNow;
					this->XIndex = XIndex;
				} else if ( thisPartition.thisRegionType == RegionType::XSideWall ) {
					if ( present( XWallIndex ) ) XWallIndex = cellCountUpToNow;
					this->XWallIndex = XWallIndex;
				} else if ( thisPartition.thisRegionType == RegionType::ZSide ) {
					if ( present( ZIndex ) ) ZIndex = cellCountUpToNow;
					this->ZIndex = ZIndex;
				} else if ( thisPartition.thisRegionType == RegionType::ZSideWall ) {
					if ( present( ZWallIndex ) ) ZWallIndex = cellCountUpToNow;
					this->ZWallIndex = ZWallIndex;
				} else if ( thisPartition.thisRegionType == RegionType::HorizInsXSide ) {
					if ( present( InsulationXIndex ) ) InsulationXIndex = cellCountUpToNow;
					this->InsulationXIndex = InsulationXIndex;
				} else if ( thisPartition.thisRegionType == RegionType::HorizInsZSide ) {
					if ( present( InsulationZIndex ) ) InsulationZIndex = cellCountUpToNow;
					this->InsulationZIndex = InsulationZIndex;
				} else if ( thisPartition.thisRegionType == RegionType::FloorInside ) {
					if ( present( YFloorIndex ) ) YFloorIndex = cellCountUpToNow;
					this->YFloorIndex = YFloorIndex;
				} else if ( thisPartition.thisRegionType == RegionType::UnderFloor ) {
					if ( present( YIndex ) ) YIndex = cellCountUpToNow;
					this->YIndex = YIndex;
				} else if ( thisPartition.thisRegionType == RegionType::VertInsLowerEdge ) {
					if ( present( InsulationYIndex ) ) InsulationYIndex = cellCountUpToNow;
					this->InsulationYIndex = InsulationYIndex;
				}

				// Create region for this partition
				auto tempRegion( GridRegion( thisPartition.Min, thisPartition.Max, thisPartition.thisRegionType, tempCellWidths ) );
				//++cellCountUpToNow;
				++regionIndex;
				this->getCellWidths( tempRegion, tempRegion.thisRegionType );
				Regions.push_back( tempRegion );

			}

			// Create final region
			auto & thisPartition( ThesePartitionRegions( ThesePartitionRegions.u1() ) );
			auto tempRegion( GridRegion( thisPartition.Max, DirExtentMax, DirDirection, tempCellWidths ) );
			cellCountUpToNow += this->getCellWidthsCount( DirDirection );
			++regionIndex;
			this->getCellWidths( tempRegion, tempRegion.thisRegionType );
			Regions.push_back( tempRegion );

		} else {
			// Need to create a region anyway if no partitions exist
			auto tempRegion( GridRegion( 0.0, DirExtentMax, DirDirection, tempCellWidths ) );
			cellCountUpToNow += this->getCellWidthsCount( DirDirection );
			++regionIndex;
			this->getCellWidths( tempRegion, tempRegion.thisRegionType );
			Regions.push_back( tempRegion );
		}

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

		int RetVal = 0;
		for ( int Index = RegionList.l1(); Index <= RegionList.u1(); ++Index ) {
			switch ( RegionList( Index ).thisRegionType ) {
			case RegionType::Pipe:
			case RegionType::BasementFloor:
			case RegionType::BasementWall:
			case RegionType::XSide:
			case RegionType::XSideWall:
			case RegionType::ZSide:
			case RegionType::ZSideWall:
			case RegionType::HorizInsXSide:
			case RegionType::HorizInsZSide:
			case RegionType::FloorInside:
			case RegionType::UnderFloor:
			case RegionType::VertInsLowerEdge:
				++RetVal;
				break;
			default:
				if ( RegionList( Index ).thisRegionType == DirDirection ) {
					for ( int CellWidthCtr = RegionList( Index ).CellWidths.l1(); CellWidthCtr <= RegionList( Index ).CellWidths.u1(); ++CellWidthCtr ) {
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
			switch ( RegionList( Index ).thisRegionType ) {
			case RegionType::Pipe:
			case RegionType::BasementFloor:
			case RegionType::BasementWall:
			case RegionType::XSide:
			case RegionType::XSideWall:
			case RegionType::ZSide:
			case RegionType::ZSideWall:
			case RegionType::HorizInsXSide:
			case RegionType::HorizInsZSide:
			case RegionType::FloorInside:
			case RegionType::UnderFloor:
			case RegionType::VertInsLowerEdge:
				++Counter;
				RetVal( Counter ) = RegionList( Index ).Min;
				break;
			default:
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

		int TotNumCells = 0;
		int NumCutawayBasementCells = 0;
		int NumInsulationCells = 0;
		int NumGroundSurfaceCells = 0;

		struct tCellExtents : MeshExtents
		{
			// Members
			Real64 Xmin;
			Real64 Ymin;
			Real64 Zmin;

			// Member Constructor
			tCellExtents(
				Real64 const Xmax,
				Real64 const Ymax,
				Real64 const Zmax,
				Real64 const Xmin,
				Real64 const Ymin,
				Real64 const Zmin
			) :
				MeshExtents( Xmax, Ymax, Zmax ),
				Xmin( Xmin ),
				Ymin( Ymin ),
				Zmin( Zmin )
			{}

		};

		//'subtract 2 in each dimension:
		//'     one for zero based array
		//'     one because the boundary points contain one entry more than the number of cells WITHIN the domain
		this->x_max_index = isize( XBoundaryPoints )-2;
		this->y_max_index = isize( YBoundaryPoints )-2;
		this->z_max_index = isize( ZBoundaryPoints )-2;
		this->Cells.allocate( {0,this->x_max_index}, {0,this->y_max_index}, {0,this->z_max_index} );

		int MaxBasementXNodeIndex = this->BasementZone.BasementWallXIndex;
		int MinBasementYNodeIndex = this->BasementZone.BasementFloorYIndex;
		int MinXIndex = this->XIndex;
		int YIndex = this->YIndex;
		int MinZIndex = this->ZIndex;
		int XWallIndex = this->XWallIndex;
		int YFloorIndex = this->YFloorIndex;
		int ZWallIndex = this->ZWallIndex;
		int InsulationXIndex = this->InsulationXIndex;
		int InsulationYIndex = this->InsulationYIndex;
		int InsulationZIndex = this->InsulationZIndex;

		auto & cells( this->Cells );
		for ( int X = 0, X_end = this->x_max_index; X <= X_end; ++X ) {
			for ( int Y = 0, Y_end = this->y_max_index; Y <= Y_end; ++Y ) {
				for ( int Z = 0, Z_end = this->z_max_index; Z <= Z_end; ++Z ) {
					auto & cell( cells( X, Y, Z ) );

					//'set up x-direction variables
					int CellXIndex = X; //'zero based index
					Real64 CellXMinValue = XBoundaryPoints( X ); //'left wall x-value
					Real64 CellXMaxValue = XBoundaryPoints( X + 1 ); //'right wall x-value
					Real64 CellXCenter = ( CellXMinValue + CellXMaxValue ) / 2;
					Real64 CellWidth = CellXMaxValue - CellXMinValue;

					//'set up y-direction variables
					int CellYIndex = Y; //'zero based index
					Real64 CellYMinValue = YBoundaryPoints( Y ); //'bottom wall y-value
					Real64 CellYMaxValue = YBoundaryPoints( Y + 1 ); //'top wall y-value
					Real64 CellYCenter = ( CellYMinValue + CellYMaxValue ) / 2;
					Real64 CellHeight = CellYMaxValue - CellYMinValue;

					//'set up z-direction variables
					int CellZIndex = Z; //'zero based index
					Real64 CellZMinValue = ZBoundaryPoints( Z ); //'lower z value
					Real64 CellZMaxValue = ZBoundaryPoints( Z + 1 ); //'higher z value
					Real64 CellZCenter = ( CellZMinValue + CellZMaxValue ) / 2;

					//'set up an extent class for this cell
					tCellExtents CellExtents = tCellExtents( CellXMaxValue, CellYMaxValue, CellZMaxValue, CellXMinValue, CellYMinValue, CellZMinValue );

					//'set up centroid, index, and overall size
					Point3DReal Centroid = Point3DReal( CellXCenter, CellYCenter, CellZCenter );
					Point3DInteger CellIndeces = Point3DInteger( CellXIndex, CellYIndex, CellZIndex );
					RectangleF XYRectangle = RectangleF( CellXMinValue, CellYMinValue, CellWidth, CellHeight );

					//'determine cell type
					CellType cellType = CellType::Unknown;

					//'if this is a pipe node, some flags are needed
					int PipeIndex = -1;
					int NumRadialCells = -1;
					int CircuitIndex = -1;

					// Adiabatic behavior is now achieved in the SetupCellNeighbors routine, these are simply farfield for now.
					CellType const ZWallCellType = CellType::FarfieldBoundary;
					CellType const UnderBasementBoundary = CellType::FarfieldBoundary;

					//'apply boundary conditions

					// For zone-coupled ground domain
					if ( this->HasZoneCoupledSlab ) {

						// Assign all cells common between on-grade and in-grade cases first
						// This should be all X/Z-side interface, vertical insulation, far-field,
						// ground surface, and ground/zone interface cells
						if ( CellXIndex == MinXIndex  &&  CellZIndex >= MinZIndex ) { // Z side interface
							// Check if vertical insulation present
							if ( this->VertInsPresentFlag ) {
								if ( CellYIndex <= this->y_max_index && CellYIndex >= InsulationYIndex ) { // Check depth of vertical insulation
									cellType = CellType::VertInsulation;
									++NumInsulationCells;
								}
							} else if ( CellYIndex == this->y_max_index ) {
								cellType = CellType::GroundSurface;
								++NumGroundSurfaceCells;
							}
						} else if ( CellZIndex == MinZIndex  &&  CellXIndex >= MinXIndex ) { // X side interface
							if ( this->VertInsPresentFlag ) { // Check if vertical insulation present
								if ( CellYIndex <= this->y_max_index && CellYIndex >= InsulationYIndex ) { // Check depth of vertical insulation
									cellType = CellType::VertInsulation;
									++NumInsulationCells;
								}
							} else if ( CellYIndex == this->y_max_index ) {
								cellType = CellType::GroundSurface;
								++NumGroundSurfaceCells;
							}
						} else if ( CellYIndex == y_max_index ) {
							if ( CellXIndex <= MinXIndex || CellZIndex <= MinZIndex ) { // Ground surface
								cellType = CellType::GroundSurface;
								++NumGroundSurfaceCells;
							} else if ( CellXIndex >= MinXIndex || CellZIndex >= MinZIndex ) { // Zone-ground interface
								cellType = CellType::ZoneGroundInterface;
							}
						}

						if ( CellYIndex == 0 || CellXIndex == 0 || CellZIndex == 0 ) { // Farfield boundary
							cellType = CellType::FarfieldBoundary;
						}

						// Assign different cells between in-grade and on-grade cases
						if ( this->SlabInGradeFlag ) { // In-grade case
							// This will assign the slab cells and horizontal insulation

							if ( CellZIndex > MinZIndex && CellXIndex > MinXIndex ) { // Cells inside bounds of slab
								if ( CellYIndex >= YIndex && CellYIndex < y_max_index ) { // Slab cells
									cellType = CellType::Slab;
								} else if ( CellYIndex == ( YIndex - 1 ) ) {
									if ( this->HorizInsPresentFlag && this->FullHorizInsPresent ) { // Full under-slab insulation
										cellType = CellType::HorizInsulation;
									} else if ( this->HorizInsPresentFlag && !this->FullHorizInsPresent ) { // Perimeter only under-slab insulation
										if ( CellZIndex < InsulationZIndex || CellXIndex < InsulationXIndex ) {
											cellType = CellType::HorizInsulation;
										}
									}
								}

							}

						} else { // Slab-on grade
							// Nothing should happen. Interface cells should already be set.
							// Under that are 'General' field cells that should be caught later.
						}

					} else if ( this->HasZoneCoupledBasement ) { // basement model, zone-coupled
						// Set the appropriate cell type
						if ( CellYIndex == 0 ) { // Farfield cells
							cellType = CellType::FarfieldBoundary;
						} else if ( CellXIndex > XWallIndex && CellZIndex > ZWallIndex ) { // Basement cutaway
							if ( CellYIndex <= this->y_max_index && CellYIndex > YFloorIndex ) { // General basement cells
								cellType = CellType::BasementCutaway;
								// Not counting basement cutaway cells.
							} else if ( CellYIndex == YFloorIndex ) { //Basement Floor cells
								cellType = CellType::BasementFloor;
							} else if ( CellYIndex == YIndex ) {
								// Check if horizontal insulation present
								if ( this->HorizInsPresentFlag ) {
									if ( this->FullHorizInsPresent ) { // Entire underfloor insulated
										cellType = CellType::HorizInsulation;
										++NumInsulationCells;
									} else { //Perimeter insulation
										if ( CellXIndex < InsulationXIndex || CellZIndex < InsulationZIndex ) {
											cellType = CellType::HorizInsulation;
											++NumInsulationCells;
										}
									}
								}
							}
						} else if ( ( CellXIndex == XWallIndex && CellZIndex > ZWallIndex ) || ( CellZIndex == ZWallIndex && CellXIndex > XWallIndex ) ) { // Basement Walls
							if ( CellYIndex <= this->y_max_index && CellYIndex > YFloorIndex ) {
								cellType = CellType::BasementWall;
							}
						} else if ( ( CellXIndex == MinXIndex && CellZIndex > ZWallIndex ) || ( CellZIndex == MinZIndex && CellXIndex > XWallIndex ) ) { // Insulation cells
							if ( CellYIndex <= this->y_max_index && CellYIndex > YFloorIndex ) {
								// Check if vertical insulation present
								if ( this->VertInsPresentFlag ) {
									if ( InsulationYIndex != 0 ) { // Partial vertical insulation
										if ( CellYIndex <= this->y_max_index && CellYIndex > InsulationYIndex ) {
											cellType = CellType::VertInsulation;
											++NumInsulationCells;
										}
									} else { //Vertical insulation extends to depth of basement floor
										if ( CellYIndex <= this->y_max_index && CellYIndex > YFloorIndex ) {
											cellType = CellType::VertInsulation;
											++NumInsulationCells;
										}
									}
								}
							}
						} else if ( CellYIndex == this->y_max_index ) { // Surface cells
							cellType = CellType::GroundSurface;
							++NumGroundSurfaceCells;
						} else if ( CellYIndex == 0 || CellXIndex == 0 || CellZIndex == 0 ) { // Farfield boundary
							cellType = CellType::FarfieldBoundary;
						}
					} else if ( CellXIndex == MaxBasementXNodeIndex && CellYIndex == MinBasementYNodeIndex ) {
						cellType = CellType::BasementCorner;
					} else if ( CellXIndex == MaxBasementXNodeIndex && CellYIndex > MinBasementYNodeIndex ) {
						cellType = CellType::BasementWall;
					} else if ( CellXIndex < MaxBasementXNodeIndex && CellYIndex == MinBasementYNodeIndex ) {
						cellType = CellType::BasementFloor;
					} else if ( CellXIndex < MaxBasementXNodeIndex && CellYIndex > MinBasementYNodeIndex ) {
						cellType = CellType::BasementCutaway;
						//Not counting basement cutaway cells
					} else if ( CellYIndex == Y_end ) {
						cellType = CellType::GroundSurface;
						++NumGroundSurfaceCells;
					} else if ( CellXIndex == 0 ) {
						if ( this->HasBasement && Y > 0 ) {
							cellType = UnderBasementBoundary; //'this must come after the basement cutaway ELSEIF branch
						} else {
							cellType = CellType::FarfieldBoundary;
						}
					} else if ( CellXIndex == X_end || CellYIndex == 0 ) {
						cellType = CellType::FarfieldBoundary;
					} else if ( CellZIndex == 0 || CellZIndex == Z_end ) {
						cellType = ZWallCellType;
					}

					//'check to see if this is a pipe node...
					Real64 InsulationThickness( 0.0 );
					int FoundOnCircuitIndex( 0 );
					Real64 RadialMeshThickness( 0.0 );
					bool HasInsulation( false );
					RadialSizing PipeSizing;
					for ( int CircuitCtr = this->CircuitIndeces.l1(), NumCircuits = this->CircuitIndeces.u1(); CircuitCtr <= NumCircuits; ++CircuitCtr ) {

						FoundOnCircuitIndex = this->CircuitIndeces( CircuitCtr );
						for ( int PipeCounter = PipingSystemCircuits( FoundOnCircuitIndex ).PipeSegmentIndeces.l1(), NumSegments = PipingSystemCircuits( FoundOnCircuitIndex ).PipeSegmentIndeces.u1(); PipeCounter <= NumSegments; ++PipeCounter ) {

							PipeSegmentInfo const & ThisSegment = PipingSystemSegments( PipingSystemCircuits( FoundOnCircuitIndex ).PipeSegmentIndeces( PipeCounter ) );
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
					switch ( cellType ) {
					case CellType::BasementCutaway:
						++NumCutawayBasementCells;
						break;
					case CellType::Unknown:
						cellType = CellType::GeneralField;
						ATTR_FALLTHROUGH;
					default:
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
					cell.X_max = CellExtents.Xmax;
					cell.Y_min = CellExtents.Ymin;
					cell.Y_max = CellExtents.Ymax;
					cell.Z_min = CellExtents.Zmin;
					cell.Z_max = CellExtents.Zmax;
					cell.X_index = CellIndeces.X;
					cell.Y_index = CellIndeces.Y;
					cell.Z_index = CellIndeces.Z;
					cell.Centroid = Centroid;
					cell.cellType = cellType;

					if ( PipeIndex != -1 ) {
						cell.PipeIndex = PipeIndex;
						CartesianPipeCellInformation::ctor( cell.PipeCellData, cell.X_max - cell.X_min, PipeSizing, NumRadialCells, cell.depth(), InsulationThickness, RadialMeshThickness, HasInsulation );
					}

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

		auto const & cells( this->Cells );
		for ( int X = 0, X_end = this->x_max_index; X <= X_end; ++X ) {
			for ( int Y = 0, Y_end = this->y_max_index; Y <= Y_end; ++Y ) {
				for ( int Z = 0, Z_end = this->z_max_index; Z <= Z_end; ++Z ) {
					auto const & cell( cells( X, Y, Z ) );

					//'for convenience
					Real64 const & ThisCellCentroidX = cell.Centroid.X;
					Real64 const & ThisCellCentroidY = cell.Centroid.Y;
					Real64 const & ThisCellCentroidZ = cell.Centroid.Z;
					Real64 ThisAdiabaticMultiplier = 1.0;
					Real64 ThisAdiabaticMultiplierMirror = 1.0;

					//'setup east/west cell neighbors
					if ( X == 0 ) {
						CellRightCentroidX = cells( X + 1, Y, Z ).Centroid.X;
						CellRightLeftWallX = cells( X + 1, Y, Z ).X_min;
						// on the X=0 face, the only adiabatic cases are:
						//   1) For a non-zone-coupled basement simulation, where the under basement X=0 cells are adiabatic -- cutaways will also get adiabatic, but who cares?
						if ( ( (!this->HasZoneCoupledSlab) && (!this->HasZoneCoupledBasement) && (this->HasBasement) ) ) {
							ThisAdiabaticMultiplier = 2.0;
							ThisAdiabaticMultiplierMirror = 0.0;
						}
						this->addNeighborInformation( X, Y, Z, Direction::PositiveX, CellRightCentroidX - ThisCellCentroidX, CellRightLeftWallX - ThisCellCentroidX, CellRightCentroidX - CellRightLeftWallX, ThisAdiabaticMultiplier );
						this->addNeighborInformation( X, Y, Z, Direction::NegativeX, 0.0, 0.0, 0.0, ThisAdiabaticMultiplierMirror );
					} else if ( X == this->x_max_index ) {
						// on the X=XMAX face, the adiabatic cases are:
						//   1) if we are doing a zone coupled slab/basement simulation where we quartered the domain
						if ( this->HasZoneCoupledSlab || this->HasZoneCoupledBasement ) {
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
					} else if ( Y == this->y_max_index ) {
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
						if ( ( (!this->HasZoneCoupledSlab) && (!this->HasZoneCoupledBasement) ) ) {
							ThisAdiabaticMultiplier = 2.0;
							ThisAdiabaticMultiplierMirror = 0.0;
						}
						this->addNeighborInformation( X, Y, Z, Direction::PositiveZ, UpperZCellCentroidZ - ThisCellCentroidZ, UpperZCellLowerWallZ - ThisCellCentroidZ, UpperZCellCentroidZ - UpperZCellLowerWallZ, ThisAdiabaticMultiplier );
						this->addNeighborInformation( X, Y, Z, Direction::NegativeZ, 0.0, 0.0, 0.0, ThisAdiabaticMultiplierMirror );
					} else if ( Z == this->z_max_index ) {
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
		NeighborInformation newItem;
		newItem.direction = direction;
		newItem.ThisCentroidToNeighborCentroid = ThisCentroidToNeighborCentroid;
		newItem.ThisCentroidToNeighborWall = ThisCentroidToNeighborWall;
		newItem.ThisWallToNeighborCentroid = ThisWallToNeighborCentroid;
		newItem.adiabaticMultiplier = ThisAdiabaticMultiplier;
		this->Cells( X, Y, Z ).NeighborInfo[ direction ] = newItem;
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
					SegmentOutletCellZ = this->z_max_index;
					break;
				case SegmentFlow::DecreasingZ:
					SegmentInletCellX = Segment.PipeCellCoordinates.X;
					SegmentInletCellY = Segment.PipeCellCoordinates.Y;
					SegmentInletCellZ = this->z_max_index;
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
		RegionType const dir
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		if ( dir == RegionType::XDirection ) {
			return this->Mesh.X.RegionMeshCount;
		} else if ( dir == RegionType::YDirection ) {
			return this->Mesh.Y.RegionMeshCount;
		} else if ( dir == RegionType::ZDirection ) {
			return this->Mesh.Z.RegionMeshCount;
		} else {
			return 1;  // it's either a mesh region (X,Y,ZDirection), or it is some form of partition -- so 1
		}

		assert( false );

		return 0;

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
		//Array1D< Real64 > RetVal;
		int maxIndex;

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
			ThisMesh.RegionMeshCount = 1; // it must be a partition type or something
			ThisMesh.thisMeshDistribution = MeshDistribution::Uniform;
			//ShowSevereError( "Invalid RegionType passed to PlantPipingSystems::FullDomainStructureInfo::getCellWidths; should be x, y, or z direction only." );
			//ShowContinueError( "This is a developer problem, as the code should never reach this point." );
			//ShowFatalError( "EnergyPlus aborts due to the previous severe error" );
		}

		// just one cell for extremely tight regions
		if ( ( g.Max - g.Min ) < 0.00001 ) {
			ThisMesh.RegionMeshCount = 1;
			ThisMesh.thisMeshDistribution = MeshDistribution::Uniform;
		}
		assert(g.Max>g.Min);

		if ( ThisMesh.RegionMeshCount > 0 ) {
			g.CellWidths.allocate( {0,ThisMesh.RegionMeshCount - 1} );
			maxIndex = ThisMesh.RegionMeshCount - 1;
		} else {
			g.CellWidths.allocate( {0,0} );
			maxIndex = 0;
		}

		Real64 GridWidth = g.Max - g.Min;

		if ( ThisMesh.thisMeshDistribution == MeshDistribution::Uniform ) {
			if ( this->HasZoneCoupledSlab && g.thisRegionType == RegionType::YDirection && g.Max == this->Extents.Ymax ) {//Slab region
				int NumCells = this->NumSlabCells;
				if ( allocated( g.CellWidths ) ) g.CellWidths.deallocate( );
				g.CellWidths.allocate( { 0, NumCells - 1 } );
				maxIndex = NumCells - 1;
				Real64 CellWidth = GridWidth / NumCells;

				for ( int I = 0; I <= NumCells - 1; ++I ) {
					g.CellWidths( I ) = CellWidth;
				}
			} else {
				// we have it quite simple
				assert( ThisMesh.RegionMeshCount > 0 );
				Real64 CellWidth = GridWidth / ThisMesh.RegionMeshCount;

				for ( int I = 0; I <= ThisMesh.RegionMeshCount - 1; ++I ) {
					g.CellWidths( I ) = CellWidth;
				}
			}
		} else if ( ThisMesh.thisMeshDistribution == MeshDistribution::SymmetricGeometric ) {

			//'then apply this "direction"'s conditions to generate a cell width array
			//'first get the total number of cells on this half of the region
			int NumCellsOnEachSide = ThisMesh.RegionMeshCount / 2; // Already validated to be an even #

			//'calculate geometric series
			Real64 SummationTerm = 0.0;
			for ( int I = 1; I <= NumCellsOnEachSide; ++I ) {
				SummationTerm += std::pow( ThisMesh.GeometricSeriesCoefficient, I - 1 );
			}

			//'set up a list of cell widths for this region
			Real64 CellWidth = ( GridWidth / 2 ) / SummationTerm;
			g.CellWidths( 0 ) = CellWidth;
			for ( int I = 1; I <= NumCellsOnEachSide - 1; ++I ) {
				CellWidth *= ThisMesh.GeometricSeriesCoefficient;
				g.CellWidths( I ) = CellWidth;
			}
			int SubIndex = NumCellsOnEachSide;
			for ( int I = NumCellsOnEachSide - 1; I >= 0; --I ) {
				g.CellWidths( SubIndex ) = g.CellWidths( I );
				++SubIndex;  // SubIndex should be incremented here - After RetVal (SubIndex) is assigned a value. -SA
			}

		} else if ( ThisMesh.thisMeshDistribution == MeshDistribution::Geometric ) {

			int NumCells = ThisMesh.RegionMeshCount;

			if ( g.thisRegionType == RegionType::XDirection || g.thisRegionType == RegionType::ZDirection ) {
				//'calculate geometric series
				Real64 SummationTerm = 0.0;
				for ( int I = 1; I <= NumCells; ++I ) {
					SummationTerm += std::pow( ThisMesh.GeometricSeriesCoefficient, I - 1 );
				}
				Real64 CellWidth = GridWidth / SummationTerm;
				if ( g.Min == 0 ) {
					//Ground region to the left of the slab will have cells expanding to the left
					g.CellWidths( NumCells - 1 ) = CellWidth;
					for ( int I = NumCells - 2; I >= 0; --I ) {
						CellWidth *= ThisMesh.GeometricSeriesCoefficient;
						g.CellWidths( I ) = CellWidth;
					}
				} else {
					//Slab region will have cells expanding to the right
					g.CellWidths( 0 ) = CellWidth;
					for ( int I = 1; I <= NumCells - 1; ++I ) {
						CellWidth *= ThisMesh.GeometricSeriesCoefficient;
						g.CellWidths( I ) = CellWidth;
					}
				}
			} else if ( g.thisRegionType == RegionType::YDirection ) {
				//Assign uniform cell thickness to the slab cells.
				if ( g.Max == this->Extents.Ymax ) {
					NumCells = this->NumSlabCells;
					if ( allocated( g.CellWidths ) ) g.CellWidths.deallocate( );
					g.CellWidths.allocate( { 0, NumCells - 1 } );
					maxIndex = NumCells - 1;

					Real64 CellWidth = GridWidth / NumCells;

					for ( int I = 0; I <= NumCells - 1; ++I ) {
						g.CellWidths( I ) = CellWidth;
					}
				} else {
					//'calculate geometric series
					Real64 SummationTerm = 0.0;
					for ( int I = 1; I <= NumCells; ++I ) {
						SummationTerm += std::pow( ThisMesh.GeometricSeriesCoefficient, I - 1 );
					}
					Real64 CellWidth = GridWidth / SummationTerm;

					//Ground region under the slab will have cells expanding as we go down
					g.CellWidths( NumCells - 1 ) = CellWidth;
					for ( int I = NumCells - 2; I >= 0; --I ) {
						CellWidth *= ThisMesh.GeometricSeriesCoefficient;
						g.CellWidths( I ) = CellWidth;
					}
				}
			}
		}

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

		auto & thisDomain( PipingSystemDomains( DomainNum ) );

		// Always do start of time step inits
		DoStartOfTimeStepInitializations( DomainNum, CircuitNum );

		// Prepare the pipe circuit for calculations, but we'll actually do calcs at the iteration level
		if ( thisDomain.HasAPipeCircuit ) {
			PreparePipeCircuitSimulation( DomainNum, CircuitNum );
		}

		// Begin iterating for this time step
		for ( int IterationIndex = 1; IterationIndex <= thisDomain.SimControls.MaxIterationsPerTS; ++IterationIndex ) {

			thisDomain.ShiftTemperaturesForNewIteration();

			if ( thisDomain.HasAPipeCircuit ) {
				PerformPipeCircuitSimulation( DomainNum, CircuitNum );
			}

			if ( thisDomain.DomainNeedsSimulation ) PerformTemperatureFieldUpdate( DomainNum );
			bool FinishedIterationLoop = false;
			DoEndOfIterationOperations( DomainNum, FinishedIterationLoop );

			if ( FinishedIterationLoop ) break;
		}

		// Update the basement surface temperatures, if any
		if ( thisDomain.HasBasement || thisDomain.HasZoneCoupledBasement ) {
			thisDomain.UpdateBasementSurfaceTemperatures();
		}

		// Update the slab surface temperatures, if any
		if ( thisDomain.HasZoneCoupledSlab ) {
			thisDomain.UpdateZoneSurfaceTemperatures();
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

		auto & dom( PipingSystemDomains( DomainNum ) );
		for ( int X = 0, X_end = dom.x_max_index; X <= X_end; ++X ) {
			for ( int Y = 0, Y_end = dom.y_max_index; Y <= Y_end; ++Y ) {
				for ( int Z = 0, Z_end = dom.z_max_index; Z <= Z_end; ++Z ) {
					auto & cell( dom.Cells( X, Y, Z ) );

					switch ( cell.cellType ) {
					case CellType::Pipe:
						//'pipes are simulated separately
						break;
					case CellType::GeneralField:
					case CellType::Slab:
					case CellType::HorizInsulation:
					case CellType::VertInsulation:
						cell.Temperature = EvaluateFieldCellTemperature( DomainNum, cell );
						break;
					case CellType::GroundSurface:
						cell.Temperature = EvaluateGroundSurfaceTemperature( DomainNum, cell );
						break;
					case CellType::FarfieldBoundary:
						cell.Temperature = EvaluateFarfieldBoundaryTemperature( DomainNum, cell );
						break;
					case CellType::BasementWall:
					case CellType::BasementCorner:
					case CellType::BasementFloor:
						// basement model, zone-coupled. Call EvaluateZoneInterfaceTemperature routine to handle timestep/hourly simulation.
						if ( PipingSystemDomains( DomainNum ).HasZoneCoupledBasement ) {
							cell.Temperature = EvaluateZoneInterfaceTemperature( DomainNum, cell );
						} else { // FHX model
							cell.Temperature = EvaluateBasementCellTemperature( DomainNum, cell );
						}
						break;
					case CellType::ZoneGroundInterface:
						cell.Temperature = EvaluateZoneInterfaceTemperature( DomainNum, cell );
						break;
					case CellType::BasementCutaway:
						// it's ok to not simulate this one
						break;
					case CellType::Unknown:
						assert(false);
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
		Real64 Beta = cell.Beta;

		// add effect from cell history
		Numerator += cell.Temperature_PrevTimeStep;
		++Denominator;

		// determine the neighbor types based on cell location
		int NumFieldCells = 0, NumBoundaryCells = 0;
		EvaluateCellNeighborDirections( DomainNum, cell, NumFieldCells, NumBoundaryCells );

		// loop across each direction in the simulation
		for ( int DirectionCounter = 1; DirectionCounter <= NumFieldCells; ++DirectionCounter ) {

			Real64 NeighborTemp = 0.0;
			Real64 Resistance = 0.0;
			Direction CurDirection = NeighborFieldCells( DirectionCounter );

			//'evaluate the transient expression terms
			EvaluateNeighborCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );

			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

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
		// evapotranspiration parameters
		Real64 const MeanSolarConstant( 0.08196 ); // 1367 [W/m2], entered in [MJ/m2-minute]
		Real64 const A_s( 0.25 ); // ?
		Real64 const B_s( 0.5 ); // ?
		Real64 const Absor_Corrected( 0.77 );
		Real64 const Convert_Wm2_To_MJhrmin( 3600.0 / 1000000.0 );
		Real64 const Convert_MJhrmin_To_Wm2( 1.0 / Convert_Wm2_To_MJhrmin );
		Real64 const Rho_water( 998.0 ); // [kg/m3]

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 NeighborTemp;
		Real64 IncidentHeatGain;
		Real64 Latitude_Degrees; // Latitude, degrees N
		Real64 StMeridian_Degrees; // Standard meridian, degrees W -- note it is degrees E in DataEnvironment
		Real64 Longitude_Degrees; // Longitude, degrees W -- note it is degrees E in DataEnvironment
		// evapotranspiration calculated values
		Real64 Latitude_Radians;
		Real64 DayOfYear;
		Real64 HourOfDay;
		Real64 CurSecondsIntoToday;
		Real64 dr;
		Real64 Declination;
		Real64 b_SC;
		Real64 Sc;
		Real64 Hour_Angle;
		Real64 X_sunset;
		Real64 Sunset_Angle;
		Real64 Altitude_Angle;
		Real64 Solar_Angle_1;
		Real64 Solar_Angle_2;
		Real64 QRAD_A;
		Real64 QRAD_SO;
		Real64 Ratio_SO;
		Real64 IncidentSolar_MJhrmin;
		Real64 AbsorbedIncidentSolar_MJhrmin;
		Real64 VaporPressureSaturated_kPa;
		Real64 VaporPressureActual_kPa;
		Real64 QRAD_NL;
		Real64 NetIncidentRadiation_MJhr; // [MJ/hr]
		Real64 NetIncidentRadiation_Wm2; // [W/m2]
		Real64 CN;
		Real64 G_hr;
		Real64 Cd;
		Real64 Slope_S;
		Real64 Pressure;
		Real64 PsychrometricConstant;
		Real64 EvapotransFluidLoss_mmhr;
		Real64 EvapotransFluidLoss_mhr;
		Real64 LatentHeatVaporization;
		Real64 EvapotransHeatLoss_MJhrmin; // [MJ/m2-hr]
		Real64 EvapotransHeatLoss_Wm2; // [W/m2]
		Real64 CurAirTempK;
		Real64 GroundCoverCoefficient;

		// retrieve information from E+ globals
		Latitude_Degrees = DataEnvironment::Latitude;
		StMeridian_Degrees = -DataEnvironment::TimeZoneMeridian; // Standard meridian, degrees W
		Longitude_Degrees = -DataEnvironment::Longitude; // Longitude, degrees W

		// retrieve any information from input data structure
		GroundCoverCoefficient = PipingSystemDomains( DomainNum ).Moisture.GroundCoverCoefficient;

		// initialize values
		Real64 AdiabaticMultiplier = 1.0;
		Real64 Numerator = 0.0;
		Real64 Denominator = 0.0;
		Real64 Resistance = 0.0;
		Real64 Beta = cell.Beta;
		Real64 ThisNormalArea = cell.normalArea( Direction::PositiveY );

		//'add effect from previous time step
		Numerator += cell.Temperature_PrevTimeStep;
		++Denominator;

		// now that we aren't infinitesimal, we need to determine the neighbor types based on cell location
		int NumFieldCells = 0, NumBoundaryCells = 0;
		EvaluateCellNeighborDirections( DomainNum, cell, NumFieldCells, NumBoundaryCells );

		// loop over all regular neighbor cells, check if we have adiabatic on opposite surface
		for ( int DirectionCounter = 1; DirectionCounter <= NumFieldCells; ++DirectionCounter ) {
			Direction CurDirection = NeighborFieldCells( DirectionCounter );

			// Use the multiplier ( either 1 or 2 ) to calculate the neighbor cell effects
			EvaluateNeighborCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );
			Numerator = AdiabaticMultiplier * Numerator + ( Beta / Resistance ) * NeighborTemp;
			Denominator = AdiabaticMultiplier * Denominator + ( Beta / Resistance );

		}

		// do all non-adiabatic boundary types here
		for ( int DirectionCounter = 1; DirectionCounter <= NumBoundaryCells; ++DirectionCounter ) {
			Direction CurDirection = NeighborBoundaryCells( DirectionCounter );

			// For Zone-coupled slab or basement configuration
			if ( PipingSystemDomains( DomainNum ).HasZoneCoupledSlab || PipingSystemDomains( DomainNum ).HasZoneCoupledBasement ) {
				//-x-direction will always be a farfield boundary
				//-z will also be a farfield boundary
				//+x and +z will be handled above
				//-y will always be a neighbor cell, so it is handled above
				//+y will always be the outdoor air
				if ( CurDirection == Direction::NegativeX || CurDirection == Direction::NegativeZ ) {
					// always farfield
					EvaluateFarfieldCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );
					Numerator += ( Beta / Resistance ) * NeighborTemp;
					Denominator += ( Beta / Resistance );
				} else if ( CurDirection == Direction::PositiveY ) {
					// convection at the surface
					if ( DataEnvironment::WindSpeed > 0.1 ) {
						Resistance = 208.0 / ( AirDensity * AirSpecificHeat * DataEnvironment::WindSpeed * ThisNormalArea );
						Numerator += ( Beta / Resistance ) * PipingSystemDomains( DomainNum ).Cur.CurAirTemp;
						Denominator += ( Beta / Resistance );
					}
				} else if ( CurDirection == Direction::NegativeY ) {
					assert( false ); // debug error, can't get here!
				}
			} else { // FHX model
				//x-direction will always be a farfield boundary
				//z-direction will be handled above -- adiabatic
				//-y we don't handle here because -y will always be a neighbor cell, so handled above
				//+y will always be the outdoor air
				if ( ( CurDirection == Direction::PositiveX ) || ( CurDirection == Direction::NegativeX ) ) {
					// always farfield
					EvaluateFarfieldCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );
					Numerator += ( Beta / Resistance ) * NeighborTemp;
					Denominator += ( Beta / Resistance );
				} else if ( ( CurDirection == Direction::PositiveZ ) || ( CurDirection == Direction::NegativeZ ) ) {
					// debug error, can't get here
				} else if ( CurDirection == Direction::PositiveY ) {
					// convection at the surface
					if ( DataEnvironment::WindSpeed > 0.1 ) {
						Resistance = 208.0 / ( AirDensity * AirSpecificHeat * DataEnvironment::WindSpeed * ThisNormalArea );
						Numerator += ( Beta / Resistance ) * PipingSystemDomains( DomainNum ).Cur.CurAirTemp;
						Denominator += ( Beta / Resistance );
					} else {
						// Future development should include additional natural convection effects here
					}
				} else if ( CurDirection == Direction::NegativeY ) {
					assert( false ); // debug error, can't get here!
				}
			}

		}

		// Initialize, this variable is used for both evapotranspiration and non-ET cases, [W]
		IncidentHeatGain = 0.0;

		// Latitude, converted to radians...positive for northern hemisphere, [radians]
		Latitude_Radians = Pi / 180.0 * Latitude_Degrees;

		// The day of year at this point in the simulation
		DayOfYear = int( PipingSystemDomains( DomainNum ).Cur.CurSimTimeSeconds / DataGlobals::SecsInDay );

		// The number of seconds into the current day
		CurSecondsIntoToday = int( mod( PipingSystemDomains( DomainNum ).Cur.CurSimTimeSeconds, DataGlobals::SecsInDay ) );

		// The number of hours into today
		HourOfDay = int( CurSecondsIntoToday / DataGlobals::SecInHour );

		// For convenience convert to Kelvin once
		CurAirTempK = PipingSystemDomains( DomainNum ).Cur.CurAirTemp + 273.15;

		// Calculate some angles
		dr = 1.0 + 0.033 * std::cos( 2.0 * Pi * DayOfYear / 365.0 );
		Declination = 0.409 * std::sin( 2.0 * Pi / 365.0 * DayOfYear - 1.39 );
		b_SC = 2.0 * Pi * ( DayOfYear - 81.0 ) / 364.0;
		Sc = 0.1645 * std::sin( 2.0 * b_SC ) - 0.1255 * std::cos( b_SC ) - 0.025 * std::sin( b_SC );
		Hour_Angle = Pi / 12.0 * ( ( ( HourOfDay - 0.5 ) + 0.06667 * ( StMeridian_Degrees - Longitude_Degrees ) + Sc ) - 12.0 );

		// Calculate sunset something, and constrain to a minimum of 0.000001
		X_sunset = 1.0 - pow_2( std::tan( Latitude_Radians ) ) * pow_2( std::tan( Declination ) );
		X_sunset = max( X_sunset, 0.000001 );

		// Find sunset angle
		Sunset_Angle = Pi / 2.0 - std::atan( -std::tan( Latitude_Radians ) * std::tan( Declination ) / std::sqrt( X_sunset ) );

		// Find the current sun angle
		Altitude_Angle = std::asin( std::sin( Latitude_Radians ) * std::sin( Declination ) + std::cos( Latitude_Radians ) * std::cos( Declination ) * std::cos( Hour_Angle ) );

		// Find solar angles
		Solar_Angle_1 = Hour_Angle - Pi / 24.0;
		Solar_Angle_2 = Hour_Angle + Pi / 24.0;

		// Constrain solar angles
		if ( Solar_Angle_1 < -Sunset_Angle ) Solar_Angle_1 = -Sunset_Angle;
		if ( Solar_Angle_2 < -Sunset_Angle ) Solar_Angle_2 = -Sunset_Angle;
		if ( Solar_Angle_1 > Sunset_Angle ) Solar_Angle_1 = Sunset_Angle;
		if ( Solar_Angle_2 > Sunset_Angle ) Solar_Angle_2 = Sunset_Angle;
		if ( Solar_Angle_1 > Solar_Angle_2 ) Solar_Angle_1 = Solar_Angle_2;

		// Convert input solar radiation [w/m2] into units for ET model, [MJ/hr-min]
		IncidentSolar_MJhrmin = PipingSystemDomains( DomainNum ).Cur.CurIncidentSolar * Convert_Wm2_To_MJhrmin;

		// Calculate another Q term...
		QRAD_A = 12.0 * 60.0 / Pi * MeanSolarConstant * dr * ( ( Solar_Angle_2 - Solar_Angle_1 ) * std::sin( Latitude_Radians ) * std::sin( Declination ) + std::cos( Latitude_Radians ) * std::cos( Declination ) * ( std::sin( Solar_Angle_2 ) - std::sin( Solar_Angle_1 ) ) );

		// Calculate another Q term...
		QRAD_SO = ( A_s + B_s + 0.00002 * DataEnvironment::Elevation ) * QRAD_A;

		// Correct the Qrad term ... better way??
		if ( IncidentSolar_MJhrmin < 0.01 ) {
			Ratio_SO = 0.0;
		} else {
			if ( QRAD_SO != 0.0 ) {
				Ratio_SO = IncidentSolar_MJhrmin / QRAD_SO;
			} else {
				// I used logic below to choose value, divide by 0 = infinity, so value = 1, not sure if correct...
				Ratio_SO = 1.0;
			}

		}

		// Constrain Ratio_SO
		Ratio_SO = min( Ratio_SO, 1.0 );
		Ratio_SO = max( Ratio_SO, 0.3 );

		// Calculate another Q term, [MJ/hr-min]
		AbsorbedIncidentSolar_MJhrmin = Absor_Corrected * IncidentSolar_MJhrmin;

		// Calculate saturated vapor pressure, [kPa]
		VaporPressureSaturated_kPa = 0.6108 * std::exp( 17.27 * PipingSystemDomains( DomainNum ).Cur.CurAirTemp / ( PipingSystemDomains( DomainNum ).Cur.CurAirTemp + 237.3 ) );

		// Calculate actual vapor pressure, [kPa]
		VaporPressureActual_kPa = VaporPressureSaturated_kPa * PipingSystemDomains( DomainNum ).Cur.CurRelativeHumidity / 100.0;

		// Calculate another Q term, [MJ/m2-hr]
		QRAD_NL = 2.042E-10 * pow_4( CurAirTempK ) * ( 0.34 - 0.14 * std::sqrt( VaporPressureActual_kPa ) ) * ( 1.35 * Ratio_SO - 0.35 );

		// Calculate another Q term, [MJ/hr]
		NetIncidentRadiation_MJhr = AbsorbedIncidentSolar_MJhrmin - QRAD_NL;

		// ?
		CN = 37.0;

		// Check whether there was sun
		if ( NetIncidentRadiation_MJhr < 0.0 ) {
			G_hr = 0.5 * NetIncidentRadiation_MJhr;
			Cd = 0.96;
		} else {
			G_hr = 0.1 * NetIncidentRadiation_MJhr;
			Cd = 0.24;
		}

		// Just For Check
		// Lu Xing Sep 22 2009

		Slope_S = 2503.0 * std::exp( 17.27 * PipingSystemDomains( DomainNum ).Cur.CurAirTemp / ( PipingSystemDomains( DomainNum ).Cur.CurAirTemp + 237.3 ) ) / pow_2( PipingSystemDomains( DomainNum ).Cur.CurAirTemp + 237.3 );
		Pressure = 98.0;
		PsychrometricConstant = 0.665e-3 * Pressure;

		// Evapotranspiration constant, [mm/hr]
		EvapotransFluidLoss_mmhr = ( GroundCoverCoefficient * Slope_S * ( NetIncidentRadiation_MJhr - G_hr ) + PsychrometricConstant * ( CN / CurAirTempK ) * PipingSystemDomains( DomainNum ).Cur.CurWindSpeed * ( VaporPressureSaturated_kPa - VaporPressureActual_kPa ) ) / ( Slope_S + PsychrometricConstant * ( 1 + Cd * PipingSystemDomains( DomainNum ).Cur.CurWindSpeed ) );

		// Convert units, [m/hr]
		EvapotransFluidLoss_mhr = EvapotransFluidLoss_mmhr / 1000.0;

		// Calculate latent heat, [MJ/kg]
		// Full formulation is cubic: L(T) = -0.0000614342 * T**3 + 0.00158927 * T**2 - 2.36418 * T + 2500.79[5]
		// In: Cubic fit to Table 2.1,p.16, Textbook: R.R.Rogers & M.K. Yau, A Short Course in Cloud Physics, 3e,(1989), Pergamon press
		// But a linear relation should suffice;
		// note-for now using the previous time step temperature as an approximation to help ensure stability
		LatentHeatVaporization = 2.501 - 2.361e-3 * cell.Temperature_PrevTimeStep;

		// Calculate evapotranspiration heat loss, [MJ/m2-hr]
		EvapotransHeatLoss_MJhrmin = Rho_water * EvapotransFluidLoss_mhr * LatentHeatVaporization;

		// Convert net incident solar units, [W/m2]
		NetIncidentRadiation_Wm2 = NetIncidentRadiation_MJhr * Convert_MJhrmin_To_Wm2;

		// Convert evapotranspiration units, [W/m2]
		EvapotransHeatLoss_Wm2 = EvapotransHeatLoss_MJhrmin * Convert_MJhrmin_To_Wm2;

		// Calculate overall net heat ?gain? into the cell, [W]
		IncidentHeatGain = ( NetIncidentRadiation_Wm2 - EvapotransHeatLoss_Wm2 ) * ThisNormalArea;

		// Add any solar/evapotranspiration heat gain here
		Numerator += Beta * IncidentHeatGain;

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

		{ auto const SELECT_CASE_var( cell.cellType );
		if ( ( SELECT_CASE_var == CellType::BasementWall ) || ( SELECT_CASE_var == CellType::BasementFloor ) ) {
			// This is actually only a half-cell since the basement wall slices right through the middle in one direction
			Beta = cell.Beta / 2.0;
		} else if ( SELECT_CASE_var == CellType::BasementCorner ) {
			// This is actually only a three-quarter-cell since the basement wall slices right through the middle in both directions
			Beta = cell.Beta * 3.0 / 4.0;
		}}

		// add effect from previous time step
		Numerator += cell.Temperature_PrevTimeStep;
		++Denominator;

		{ auto const SELECT_CASE_var( cell.cellType );
		if ( SELECT_CASE_var == CellType::BasementWall ) {

			// we will only have heat flux from the basement wall and heat conduction to the +x cell

			// get the average basement wall heat flux and add it to the tally
			HeatFlux = PipingSystemDomains( DomainNum ).GetBasementWallHeatFlux();
			Numerator += Beta * HeatFlux * cell.height();

			// then get the +x conduction to continue the heat balance
			EvaluateNeighborCharacteristics( DomainNum, cell, Direction::PositiveX, NeighborTemp, Resistance, AdiabaticMultiplier );
			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

		} else if ( SELECT_CASE_var == CellType::BasementFloor ) {

			// we will only have heat flux from the basement floor and heat conduction to the lower cell

			// get the average basement floor heat flux and add it to the tally
			HeatFlux = PipingSystemDomains( DomainNum ).GetBasementFloorHeatFlux();
			Numerator += Beta * HeatFlux * cell.width();

			// then get the -y conduction to continue the heat balance
			EvaluateNeighborCharacteristics( DomainNum, cell, Direction::NegativeY, NeighborTemp, Resistance, AdiabaticMultiplier );
			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

		} else if ( SELECT_CASE_var == CellType::BasementCorner ) {

			// we will only have heat conduction to the +x and -y cells
			EvaluateNeighborCharacteristics( DomainNum, cell, Direction::PositiveX, NeighborTemp, Resistance, AdiabaticMultiplier );
			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

			EvaluateNeighborCharacteristics( DomainNum, cell, Direction::NegativeY, NeighborTemp, Resistance, AdiabaticMultiplier );
			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );

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
		Real64 Beta = cell.Beta;

		// add effect from previous time step
		Numerator += cell.Temperature_PrevTimeStep;
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
		Real64 Beta = cell.Beta;

		// add effect from previous time step
		Numerator += cell.Temperature_PrevTimeStep;
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

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 RunningSummation = 0.0;
		Real64 RunningVolume = 0.0;

		auto const & cells( Cells );
		for ( int X = 0, X_end = this->x_max_index; X <= X_end; ++X ) {
			for ( int Y = 0, Y_end = this->y_max_index; Y <= Y_end; ++Y ) {
				for ( int Z = 0, Z_end = this->z_max_index; Z <= Z_end; ++Z ) {
					auto const & cell( cells( X, Y, Z ) );
					if ( cell.cellType == cellType ) {
						Real64 CellVolume = cell.volume();
						RunningVolume += CellVolume;
						RunningSummation += CellVolume * cell.Temperature;
					}
				}
			}
		}

		if ( RunningVolume <= 0.0 ) {
			ShowFatalError( "FullDomainStructureInfo::GetAverageTempByType calculated zero volume, program aborts" );
		}

		return RunningSummation / RunningVolume;

	}

	void
	EvaluateFarfieldCharacteristics(
		int const DomainNum,
		CartesianCell & cell,
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

		switch ( direction ) {
		case Direction::NegativeX:
		case Direction::PositiveX:
			distance = ( cell.width() / 2.0 );
			break;
		case Direction::NegativeY:
		case Direction::PositiveY:
			distance = ( cell.height() / 2.0 );
			break;
		case Direction::NegativeZ:
		case Direction::PositiveZ:
			distance = ( cell.depth() / 2.0 );
			break;
		}

		resistance = ( distance / 2.0 ) / ( cell.Properties.Conductivity * cell.normalArea( direction ) );
		neighbortemp = GetFarfieldTemp( DomainNum, cell );

		adiabaticMultiplier = cell.NeighborInfo[ direction ].adiabaticMultiplier;
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

		// Setup circuit flow conditions -- convection coefficient
		int CellX = PipingSystemCircuits( CircuitNum ).CircuitInletCell.X;
		int CellY = PipingSystemCircuits( CircuitNum ).CircuitInletCell.Y;
		int CellZ = PipingSystemCircuits( CircuitNum ).CircuitInletCell.Z;

		// Look up current fluid properties
		Real64 Density = PipingSystemCircuits( CircuitNum ).CurFluidPropertySet.Density;
		Real64 Viscosity = PipingSystemCircuits( CircuitNum ).CurFluidPropertySet.Viscosity;
		Real64 Conductivity = PipingSystemCircuits( CircuitNum ).CurFluidPropertySet.Conductivity;
		Real64 Prandtl = PipingSystemCircuits( CircuitNum ).CurFluidPropertySet.Prandtl;

		// Flow calculations
		Real64 Area_c = ( Pi / 4.0 ) * pow_2( PipingSystemCircuits( CircuitNum ).PipeSize.InnerDia );
		Real64 Velocity = PipingSystemCircuits( CircuitNum ).CurCircuitFlowRate / ( Density * Area_c );

		// Determine convection coefficient based on flow conditions
		Real64 ConvCoefficient = 0.0;
		if ( Velocity > 0 ) {
			Real64 Reynolds = Density * PipingSystemCircuits( CircuitNum ).PipeSize.InnerDia * Velocity / Viscosity;
			Real64 ExponentTerm = 0.0;
			if ( PipingSystemDomains( DomainNum ).Cells( CellX, CellY, CellZ ).PipeCellData.Fluid.Temperature > PipingSystemDomains( DomainNum ).Cells( CellX, CellY, CellZ ).PipeCellData.Pipe.Temperature ) {
				ExponentTerm = 0.3;
			} else {
				ExponentTerm = 0.4;
			}
			Real64 Nusselt = 0.023 * std::pow( Reynolds, 4.0 / 5.0 ) * std::pow( Prandtl, ExponentTerm );
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

		// retrieve initial conditions from the data structure
		// these have been set either by the init routine or by the heat pump routine
		Real64 FlowRate = PipingSystemCircuits( CircuitNum ).CurCircuitFlowRate;
		Real64 EnteringTemp = PipingSystemCircuits( CircuitNum ).CurCircuitInletTemp;

		// initialize
		int SegmentCellCtr = 0;
		int StartingSegment = PipingSystemCircuits( CircuitNum ).PipeSegmentIndeces.l1();
		int EndingSegment = PipingSystemCircuits( CircuitNum ).PipeSegmentIndeces.u1();

		//'loop across all segments (pipes) of the circuit
		auto & dom( PipingSystemDomains( DomainNum ) );
		auto & cells( dom.Cells );
		for ( int SegmentCtr = StartingSegment; SegmentCtr <= EndingSegment; ++SegmentCtr ) {

			int SegmentIndex = PipingSystemCircuits( CircuitNum ).PipeSegmentIndeces( SegmentCtr );
			int StartingZ = 0;
			int EndingZ = 0;
			int Increment = 0;

			//'set simulation flow direction
			switch ( PipingSystemSegments( SegmentIndex ).FlowDirection ) {
			case SegmentFlow::IncreasingZ:
				StartingZ = 0;
				EndingZ = dom.z_max_index;
				Increment = 1;
				break;
			case SegmentFlow::DecreasingZ:
				StartingZ = dom.z_max_index;
				EndingZ = 0;
				Increment = -1;
				break;
			default:
				ShowFatalError( "Debug error: invalid flow direction on piping system segment" );
			}

			//'find the cell we are working on in order to retrieve cell and neighbor information
			int PipeX = PipingSystemSegments( SegmentIndex ).PipeCellCoordinates.X;
			int PipeY = PipingSystemSegments( SegmentIndex ).PipeCellCoordinates.Y;

			//'loop across all z-direction indeces
			int const Zindex_stop( floop_end( StartingZ, EndingZ, Increment ) );
			for ( int Zindex = StartingZ; Zindex != Zindex_stop; Zindex += Increment ) {

				//'overall cell segment counter
				++SegmentCellCtr;

				if ( SegmentCellCtr == 1 ) {
					//'we have the very first cell, need to pass in circuiting entering temperature
					PerformPipeCellSimulation( DomainNum, CircuitNum, cells( PipeX, PipeY, Zindex ), FlowRate, EnteringTemp );
				} else {
					//'we don't have the first cell so just normal simulation
					if ( Zindex == EndingZ ) {
						// simulate current cell using upstream as entering conditions
						PerformPipeCellSimulation( DomainNum, CircuitNum, cells( PipeX, PipeY, Zindex ), FlowRate, cells( PipeX, PipeY, Zindex - Increment ).PipeCellData.Fluid.Temperature );
						// store this outlet condition to be passed to the next segment
						CircuitCrossTemp = cells( PipeX, PipeY, Zindex ).PipeCellData.Fluid.Temperature;
					} else if ( Zindex == StartingZ ) {
						// we are starting another segment, use the previous cross temperature
						PerformPipeCellSimulation( DomainNum, CircuitNum, cells( PipeX, PipeY, Zindex ), FlowRate, CircuitCrossTemp );
					} else {
						// we are in an interior node, so just get the upstream cell and use the main simulation
						PerformPipeCellSimulation( DomainNum, CircuitNum, cells( PipeX, PipeY, Zindex ), FlowRate, cells( PipeX, PipeY, Zindex - Increment ).PipeCellData.Fluid.Temperature );
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
					PipingSystemSegments( SegmentIndex ).OutletTemperature = cells( PipeX, PipeY, Zindex ).PipeCellData.Fluid.Temperature;
					PipingSystemSegments( SegmentIndex ).FluidHeatLoss = FlowRate * PipingSystemCircuits( CircuitNum ).CurFluidPropertySet.SpecificHeat * ( PipingSystemSegments( SegmentIndex ).InletTemperature - PipingSystemSegments( SegmentIndex ).OutletTemperature );
				}

				// Bookkeeping: circuit fluid temperature updates
				if ( ( SegmentCtr == StartingSegment ) && ( Zindex == StartingZ ) ) {
					PipingSystemCircuits( CircuitNum ).InletTemperature = EnteringTemp;
				} else if ( ( SegmentCtr == EndingSegment ) && ( Zindex == EndingZ ) ) {
					PipingSystemCircuits( CircuitNum ).OutletTemperature = cells( PipeX, PipeY, Zindex ).PipeCellData.Fluid.Temperature;
					PipingSystemCircuits( CircuitNum ).FluidHeatLoss = FlowRate * PipingSystemCircuits( CircuitNum ).CurFluidPropertySet.SpecificHeat * ( PipingSystemCircuits( CircuitNum ).InletTemperature - PipingSystemCircuits( CircuitNum ).OutletTemperature );
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

		for ( int Iter = 1; Iter <= PipingSystemCircuits( CircuitNum ).MaxIterationsPerTS; ++Iter ) {

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
			if ( IsConverged_PipeCurrentToPrevIteration( DomainNum, ThisCell ) ) break;

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
		Real64 Resistance = 0.0;
		Real64 Numerator = 0.0;
		Real64 Denominator = 0.0;
		Real64 Beta = cell.Beta;

		//'add effects from this cell history
		Numerator += cell.Temperature_PrevTimeStep;
		++Denominator;

		//'add effects from outermost radial cell
		Real64 OutermostRadialCellOuterRadius = cell.PipeCellData.Soil( cell.PipeCellData.Soil.u1() ).OuterRadius;
		Real64 OutermostRadialCellRadialCentroid = cell.PipeCellData.Soil( cell.PipeCellData.Soil.u1() ).RadialCentroid;
		Real64 OutermostRadialCellTemperature = cell.PipeCellData.Soil( cell.PipeCellData.Soil.u1() ).Temperature;
		Resistance = std::log( OutermostRadialCellOuterRadius / OutermostRadialCellRadialCentroid ) / ( 2.0 * Pi * cell.depth() * cell.Properties.Conductivity );
		Numerator += ( Beta / Resistance ) * OutermostRadialCellTemperature;
		Denominator += ( Beta / Resistance );

		//'add effects from neighboring Cartesian cells
		for ( int DirectionCounter = Directions.l1(); DirectionCounter <= Directions.u1(); ++DirectionCounter ) {
			Direction CurDirection = Directions( DirectionCounter );
			Real64 AdiabaticMultiplier = 1.0;
			Real64 NeighborTemp = 0.0;
			EvaluateNeighborCharacteristics( DomainNum, cell, CurDirection, NeighborTemp, Resistance, AdiabaticMultiplier );
			Numerator += AdiabaticMultiplier * ( Beta / Resistance ) * NeighborTemp;
			Denominator += AdiabaticMultiplier * ( Beta / Resistance );
		}

		//'calculate the new temperature
		cell.Temperature = Numerator / Denominator;

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
		Real64 NextOuterRadialCellOuterRadius;
		Real64 NextOuterRadialCellRadialCentroid;
		Real64 NextOuterRadialCellConductivity;
		Real64 NextOuterRadialCellInnerRadius;
		Real64 NextOuterRadialCellTemperature;

		Real64 Numerator = 0.0;
		Real64 Denominator = 0.0;
		Real64 Resistance = 0.0;

		//'convenience variables
		int MaxRadialIndex = cell.PipeCellData.Soil.u1();
		Real64 ThisRadialCellOuterRadius = cell.PipeCellData.Soil( MaxRadialIndex ).OuterRadius;
		Real64 ThisRadialCellRadialCentroid = cell.PipeCellData.Soil( MaxRadialIndex ).RadialCentroid;
		Real64 ThisRadialCellConductivity = cell.PipeCellData.Soil( MaxRadialIndex ).Properties.Conductivity;
		Real64 ThisRadialCellInnerRadius = cell.PipeCellData.Soil( MaxRadialIndex ).InnerRadius;
		Real64 ThisRadialCellTemperature_PrevTimeStep = cell.PipeCellData.Soil( MaxRadialIndex ).Temperature_PrevTimeStep;
		if ( size( cell.PipeCellData.Soil ) == 1 ) {
			if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
				NextOuterRadialCellOuterRadius = cell.PipeCellData.Insulation.OuterRadius;
				NextOuterRadialCellRadialCentroid = cell.PipeCellData.Insulation.RadialCentroid;
				NextOuterRadialCellConductivity = cell.PipeCellData.Insulation.Properties.Conductivity;
				NextOuterRadialCellInnerRadius = cell.PipeCellData.Insulation.InnerRadius;
				NextOuterRadialCellTemperature = cell.PipeCellData.Insulation.Temperature;
			} else {
				NextOuterRadialCellOuterRadius = cell.PipeCellData.Pipe.OuterRadius;
				NextOuterRadialCellRadialCentroid = cell.PipeCellData.Pipe.RadialCentroid;
				NextOuterRadialCellConductivity = cell.PipeCellData.Pipe.Properties.Conductivity;
				NextOuterRadialCellInnerRadius = cell.PipeCellData.Pipe.InnerRadius;
				NextOuterRadialCellTemperature = cell.PipeCellData.Pipe.Temperature;
			}
		} else {
			NextOuterRadialCellOuterRadius = cell.PipeCellData.Soil( MaxRadialIndex - 1 ).OuterRadius;
			NextOuterRadialCellRadialCentroid = cell.PipeCellData.Soil( MaxRadialIndex - 1 ).RadialCentroid;
			NextOuterRadialCellConductivity = cell.PipeCellData.Soil( MaxRadialIndex - 1 ).Properties.Conductivity;
			NextOuterRadialCellInnerRadius = cell.PipeCellData.Soil( MaxRadialIndex - 1 ).InnerRadius;
			NextOuterRadialCellTemperature = cell.PipeCellData.Soil( MaxRadialIndex - 1 ).Temperature;
		}

		//'any broadly defined variables
		Real64 Beta = cell.PipeCellData.Soil( MaxRadialIndex ).Beta;

		//'add effects from this cell history
		Numerator += ThisRadialCellTemperature_PrevTimeStep;
		++Denominator;

		//'add effects from interface cell
		Resistance = std::log( ThisRadialCellOuterRadius / ThisRadialCellRadialCentroid ) / ( 2 * Pi * cell.depth() * ThisRadialCellConductivity );
		Numerator += ( Beta / Resistance ) * cell.Temperature;
		Denominator += ( Beta / Resistance );

		//'add effects from inner radial cell
		Resistance = ( std::log( ThisRadialCellRadialCentroid / ThisRadialCellInnerRadius ) / ( 2 * Pi * cell.depth() * ThisRadialCellConductivity ) ) + ( std::log( NextOuterRadialCellOuterRadius / NextOuterRadialCellRadialCentroid ) / ( 2 * Pi * cell.depth() * NextOuterRadialCellConductivity ) );
		Numerator += ( Beta / Resistance ) * NextOuterRadialCellTemperature;
		Denominator += ( Beta / Resistance );

		//'calculate the new temperature
		cell.PipeCellData.Soil( MaxRadialIndex ).Temperature = Numerator / Denominator;

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

		for ( int rCtr = cell.PipeCellData.Soil.u1() - 1; rCtr >= 1; --rCtr ) {

			Real64 Numerator = 0.0;
			Real64 Denominator = 0.0;
			Real64 Resistance = 0.0;

			//'convenience variables
			Real64 ThisRadialCellOuterRadius = cell.PipeCellData.Soil( rCtr ).OuterRadius;
			Real64 ThisRadialCellRadialCentroid = cell.PipeCellData.Soil( rCtr ).RadialCentroid;
			Real64 ThisRadialCellConductivity = cell.PipeCellData.Soil( rCtr ).Properties.Conductivity;
			Real64 ThisRadialCellInnerRadius = cell.PipeCellData.Soil( rCtr ).InnerRadius;
			Real64 ThisRadialCellTemperature_PrevTimeStep = cell.PipeCellData.Soil( rCtr ).Temperature_PrevTimeStep;

			Real64 InnerRadialCellOuterRadius = cell.PipeCellData.Soil( rCtr - 1 ).OuterRadius;
			Real64 InnerRadialCellRadialCentroid = cell.PipeCellData.Soil( rCtr - 1 ).RadialCentroid;
			Real64 InnerRadialCellConductivity = cell.PipeCellData.Soil( rCtr - 1 ).Properties.Conductivity;
			Real64 InnerRadialCellTemperature = cell.PipeCellData.Soil( rCtr - 1 ).Temperature;

			Real64 OuterRadialCellRadialCentroid = cell.PipeCellData.Soil( rCtr + 1 ).RadialCentroid;
			Real64 OuterRadialCellConductivity = cell.PipeCellData.Soil( rCtr + 1 ).Properties.Conductivity;
			Real64 OuterRadialCellInnerRadius = cell.PipeCellData.Soil( rCtr + 1 ).InnerRadius;
			Real64 OuterRadialCellTemperature = cell.PipeCellData.Soil( rCtr + 1 ).Temperature;

			//'any broadly defined variables
			Real64 Beta = cell.PipeCellData.Soil( rCtr ).Beta;

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
			cell.PipeCellData.Soil( rCtr ).Temperature = Numerator / Denominator;

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
		Real64 Resistance;
		Real64 InnerNeighborRadialCellOuterRadius;
		Real64 InnerNeighborRadialCellRadialCentroid;
		Real64 InnerNeighborRadialCellConductivity;
		Real64 InnerNeighborRadialCellInnerRadius;
		Real64 InnerNeighborRadialCellTemperature;

		Real64 Numerator = 0.0;
		Real64 Denominator = 0.0;

		//'convenience variables
		if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
			InnerNeighborRadialCellOuterRadius = cell.PipeCellData.Insulation.OuterRadius;
			InnerNeighborRadialCellRadialCentroid = cell.PipeCellData.Insulation.RadialCentroid;
			InnerNeighborRadialCellConductivity = cell.PipeCellData.Insulation.Properties.Conductivity;
			InnerNeighborRadialCellInnerRadius = cell.PipeCellData.Insulation.InnerRadius;
			InnerNeighborRadialCellTemperature = cell.PipeCellData.Insulation.Temperature;
		} else {
			InnerNeighborRadialCellOuterRadius = cell.PipeCellData.Pipe.OuterRadius;
			InnerNeighborRadialCellRadialCentroid = cell.PipeCellData.Pipe.RadialCentroid;
			InnerNeighborRadialCellConductivity = cell.PipeCellData.Pipe.Properties.Conductivity;
			InnerNeighborRadialCellInnerRadius = cell.PipeCellData.Pipe.InnerRadius;
			InnerNeighborRadialCellTemperature = cell.PipeCellData.Pipe.Temperature;
		}

		Real64 ThisRadialCellOuterRadius = cell.PipeCellData.Soil( 0 ).OuterRadius;
		Real64 ThisRadialCellRadialCentroid = cell.PipeCellData.Soil( 0 ).RadialCentroid;
		Real64 ThisRadialCellConductivity = cell.PipeCellData.Soil( 0 ).Properties.Conductivity;
		Real64 ThisRadialCellInnerRadius = cell.PipeCellData.Soil( 0 ).InnerRadius;
		Real64 ThisRadialCellTemperature_PrevTimeStep = cell.PipeCellData.Soil( 0 ).Temperature_PrevTimeStep;

		Real64 OuterNeighborRadialCellRadialCentroid = cell.PipeCellData.Soil( 1 ).RadialCentroid;
		Real64 OuterNeighborRadialCellConductivity = cell.PipeCellData.Soil( 1 ).Properties.Conductivity;
		Real64 OuterNeighborRadialCellInnerRadius = cell.PipeCellData.Soil( 1 ).InnerRadius;
		Real64 OuterNeighborRadialCellTemperature = cell.PipeCellData.Soil( 1 ).Temperature;

		//'any broadly defined variables
		Real64 Beta = cell.PipeCellData.Soil( 0 ).Beta;

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
		cell.PipeCellData.Soil( 0 ).Temperature = Numerator / Denominator;

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
		Real64 Resistance = 0.0;
		Real64 Numerator = 0.0;
		Real64 Denominator = 0.0;

		//'convenience variables
		auto const & PipeCell = cell.PipeCellData.Pipe;
		auto const & ThisInsulationCell = cell.PipeCellData.Insulation;
		auto const & NextInnerRadialCell = cell.PipeCellData.Soil( 0 );

		//'any broadly defined variables
		Real64 Beta = ThisInsulationCell.Beta;

		//'add effects from this cell history
		Numerator += ThisInsulationCell.Temperature_PrevTimeStep;
		++Denominator;

		//'add effects from outer radial cell
		Resistance = ( std::log( NextInnerRadialCell.RadialCentroid / NextInnerRadialCell.InnerRadius ) / ( 2 * Pi * cell.depth() * NextInnerRadialCell.Properties.Conductivity ) ) + ( std::log( ThisInsulationCell.OuterRadius / ThisInsulationCell.RadialCentroid ) / ( 2 * Pi * cell.depth() * ThisInsulationCell.Properties.Conductivity ) );
		Numerator += ( Beta / Resistance ) * NextInnerRadialCell.Temperature;
		Denominator += ( Beta / Resistance );

		//'add effects from pipe cell
		Resistance = ( std::log( ThisInsulationCell.RadialCentroid / ThisInsulationCell.InnerRadius ) / ( 2 * Pi * cell.depth() * ThisInsulationCell.Properties.Conductivity ) ) + ( std::log( PipeCell.OuterRadius / PipeCell.RadialCentroid ) / ( 2 * Pi * cell.depth() * PipeCell.Properties.Conductivity ) );
		Numerator += ( Beta / Resistance ) * PipeCell.Temperature;
		Denominator += ( Beta / Resistance );

		//'calculate the new temperature
		cell.PipeCellData.Insulation.Temperature = Numerator / Denominator;

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
		Real64 OuterNeighborRadialCellOuterRadius;
		Real64 OuterNeighborRadialCellRadialCentroid;
		Real64 OuterNeighborRadialCellConductivity;
		Real64 OuterNeighborRadialCellInnerRadius;
		Real64 OuterNeighborRadialCellTemperature;

		Real64 Numerator = 0.0;
		Real64 Denominator = 0.0;
		Real64 Resistance = 0.0;

		//'convenience variables
		if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
			OuterNeighborRadialCellOuterRadius = cell.PipeCellData.Insulation.OuterRadius;
			OuterNeighborRadialCellRadialCentroid = cell.PipeCellData.Insulation.RadialCentroid;
			OuterNeighborRadialCellConductivity = cell.PipeCellData.Insulation.Properties.Conductivity;
			OuterNeighborRadialCellInnerRadius = cell.PipeCellData.Insulation.InnerRadius;
			OuterNeighborRadialCellTemperature = cell.PipeCellData.Insulation.Temperature;
		} else {
			OuterNeighborRadialCellOuterRadius = cell.PipeCellData.Soil( 0 ).OuterRadius;
			OuterNeighborRadialCellRadialCentroid = cell.PipeCellData.Soil( 0 ).RadialCentroid;
			OuterNeighborRadialCellConductivity = cell.PipeCellData.Soil( 0 ).Properties.Conductivity;
			OuterNeighborRadialCellInnerRadius = cell.PipeCellData.Soil( 0 ).InnerRadius;
			OuterNeighborRadialCellTemperature = cell.PipeCellData.Soil( 0 ).Temperature;
		}

		Real64 ThisPipeCellOuterRadius = cell.PipeCellData.Pipe.OuterRadius;
		Real64 ThisPipeCellRadialCentroid = cell.PipeCellData.Pipe.RadialCentroid;
		Real64 ThisPipeCellConductivity = cell.PipeCellData.Pipe.Properties.Conductivity;
		Real64 ThisPipeCellInnerRadius = cell.PipeCellData.Pipe.InnerRadius;
		Real64 ThisPipeCellTemperature_PrevTimeStep = cell.PipeCellData.Pipe.Temperature_PrevTimeStep;

		Real64 FluidCellTemperature = cell.PipeCellData.Fluid.Temperature;

		//'any broadly defined variables
		Real64 Beta = cell.PipeCellData.Pipe.Beta;

		//'add effects from this cell history
		Numerator += ThisPipeCellTemperature_PrevTimeStep;
		++Denominator;

		//'add effects from outer radial cell
		Resistance = ( std::log( OuterNeighborRadialCellRadialCentroid / OuterNeighborRadialCellInnerRadius ) / ( 2 * Pi * cell.depth() * OuterNeighborRadialCellConductivity ) ) + ( std::log( ThisPipeCellOuterRadius / ThisPipeCellRadialCentroid ) / ( 2 * Pi * cell.depth() * ThisPipeCellConductivity ) );
		Numerator += ( Beta / Resistance ) * OuterNeighborRadialCellTemperature;
		Denominator += ( Beta / Resistance );

		//'add effects from water cell
		Real64 PipeConductionResistance = std::log( ThisPipeCellRadialCentroid / ThisPipeCellInnerRadius ) / ( 2 * Pi * cell.depth() * ThisPipeCellConductivity );
		Real64 ConvectiveResistance = 1.0 / ( ConvectionCoefficient * 2 * Pi * ThisPipeCellInnerRadius * cell.depth() );
		Resistance = PipeConductionResistance + ConvectiveResistance;
		Numerator += ( Beta / Resistance ) * FluidCellTemperature;
		Denominator += ( Beta / Resistance );

		//'calculate new temperature
		cell.PipeCellData.Pipe.Temperature = Numerator / Denominator;

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
		Real64 Numerator = 0.0;
		Real64 Denominator = 0.0;

		//'convenience variables
		Real64 FluidCellTemperature_PrevTimeStep = cell.PipeCellData.Fluid.Temperature_PrevTimeStep;
		Real64 FluidCellSpecificHeat = cell.PipeCellData.Fluid.Properties.SpecificHeat;

		Real64 PipeCellRadialCentroid = cell.PipeCellData.Pipe.RadialCentroid;
		Real64 PipeCellConductivity = cell.PipeCellData.Pipe.Properties.Conductivity;
		Real64 PipeCellInnerRadius = cell.PipeCellData.Pipe.InnerRadius;
		Real64 PipeCellTemperature = cell.PipeCellData.Pipe.Temperature;

		Real64 Beta = cell.PipeCellData.Fluid.Beta;

		//'add effects from this cell history
		Numerator += FluidCellTemperature_PrevTimeStep;
		++Denominator;

		//'add effects from outer pipe cell
		Real64 PipeConductionResistance = std::log( PipeCellRadialCentroid / PipeCellInnerRadius ) / ( 2 * Pi * cell.depth() * PipeCellConductivity );
		Real64 ConvectiveResistance = 1.0 / ( ConvectionCoefficient * 2 * Pi * PipeCellInnerRadius * cell.depth() );
		Real64 TotalPipeResistance = PipeConductionResistance + ConvectiveResistance;
		Numerator += ( Beta / TotalPipeResistance ) * PipeCellTemperature;
		Denominator += ( Beta / TotalPipeResistance );

		//'add effects from upstream flow
		if ( FlowRate > 0.0 ) {
			Real64 UpstreamResistance = 1 / ( FlowRate * FluidCellSpecificHeat );
			Numerator += ( Beta / UpstreamResistance ) * EnteringFluidTemp;
			Denominator += ( Beta / UpstreamResistance );
		}

		//'calculate new temperature
		cell.PipeCellData.Fluid.Temperature = Numerator / Denominator;

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
		auto & thisDomain( PipingSystemDomains( DomainNum ) );
		auto & cells( thisDomain.Cells );
		for ( int X = 0, X_end = thisDomain.x_max_index; X <= X_end; ++X ) {
			for ( int Y = 0, Y_end = thisDomain.y_max_index; Y <= Y_end; ++Y ) {
				for ( int Z = 0, Z_end = thisDomain.z_max_index; Z <= Z_end; ++Z ) {
					auto & cell( cells( X, Y, Z ) );
					switch ( cell.cellType ) {
					case CellType::Pipe:
						cell.Properties = PipingSystemDomains( DomainNum ).GroundProperties;
						for ( int rCtr = 0; rCtr <= cell.PipeCellData.Soil.u1(); ++rCtr ) {
							cell.PipeCellData.Soil( rCtr ).Properties = PipingSystemDomains( DomainNum ).GroundProperties;
						}
						cell.PipeCellData.Pipe.Properties = PipingSystemCircuits( CircuitNum ).PipeProperties;
						if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
							cell.PipeCellData.Insulation.Properties = PipingSystemCircuits( CircuitNum ).InsulationProperties;
						}
						break;
					case CellType::GeneralField:
					case CellType::GroundSurface:
					case CellType::FarfieldBoundary:
						cell.Properties = thisDomain.GroundProperties;
						break;
					case CellType::BasementWall:
					case CellType::BasementFloor:
					case CellType::BasementCorner:
						if ( thisDomain.HasZoneCoupledBasement ) { // Basement interface layer
							cell.Properties = thisDomain.BasementInterfaceProperties;
						} else { // Basement cells are partially ground, give them some props
							cell.Properties = thisDomain.GroundProperties;
						}
						break;
					case CellType::Slab:
						cell.Properties = thisDomain.SlabProperties;
						break;
					case CellType::HorizInsulation:
						cell.Properties = thisDomain.HorizInsProperties;
						break;
					case CellType::VertInsulation:
						cell.Properties = thisDomain.VertInsProperties;
						break;
					case CellType::ZoneGroundInterface:
						if ( thisDomain.SlabInGradeFlag ) {
							cell.Properties = thisDomain.SlabProperties;
						} else {
							cell.Properties = thisDomain.GroundProperties;
						}
						break;
					case CellType::BasementCutaway:
						break;
					case CellType::Unknown:
						assert( false );
					}
				}
			}
		}

		//'calculate one-time resistance terms for cartesian cells
		for ( int X = 0, X_end = thisDomain.x_max_index; X <= X_end; ++X ) {
			for ( int Y = 0, Y_end = thisDomain.y_max_index; Y <= Y_end; ++Y ) {
				for ( int Z = 0, Z_end = thisDomain.z_max_index; Z <= Z_end; ++Z ) {
					auto & cell( cells( X, Y, Z ) );
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
						EndingZ = thisDomain.z_max_index;
						Increment = 1;
						break;
					case SegmentFlow::DecreasingZ:
						StartingZ = thisDomain.z_max_index;
						EndingZ = 0;
						Increment = -1;
						break;
					}

					int PipeX = PipingSystemSegments( PipingSystemCircuits( CircuitNum ).PipeSegmentIndeces( SegIndex ) ).PipeCellCoordinates.X;
					int PipeY = PipingSystemSegments( PipingSystemCircuits( CircuitNum ).PipeSegmentIndeces( SegIndex ) ).PipeCellCoordinates.Y;

					//'loop across all z-direction indices
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
		for ( int X = 0, X_end = thisDomain.x_max_index; X <= X_end; ++X ) {
			for ( int Y = 0, Y_end = thisDomain.y_max_index; Y <= Y_end; ++Y ) {
				for ( int Z = 0, Z_end = thisDomain.z_max_index; Z <= Z_end; ++Z ) {
					auto & cell( cells( X, Y, Z ) );

					// On OneTimeInit, the cur sim time should be zero, so this will be OK
					Real64 ThisCellTemp = GetFarfieldTemp( DomainNum, cell );
					cell.Temperature = ThisCellTemp;
					cell.Temperature_PrevIteration = ThisCellTemp;
					cell.Temperature_PrevTimeStep = ThisCellTemp;


					if ( cell.cellType == CellType::Pipe ) {

						for ( int rCtr = 0; rCtr <= cell.PipeCellData.Soil.u1(); ++rCtr ) {
							cell.PipeCellData.Soil( rCtr ).Temperature = ThisCellTemp;
							cell.PipeCellData.Soil( rCtr ).Temperature_PrevIteration = ThisCellTemp;
							cell.PipeCellData.Soil( rCtr ).Temperature_PrevTimeStep = ThisCellTemp;
						}
						cell.PipeCellData.Pipe.Temperature = ThisCellTemp;
						cell.PipeCellData.Pipe.Temperature_PrevIteration = ThisCellTemp;
						cell.PipeCellData.Pipe.Temperature_PrevTimeStep = ThisCellTemp;
						if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
							cell.PipeCellData.Insulation.Temperature = ThisCellTemp;
							cell.PipeCellData.Insulation.Temperature_PrevIteration = ThisCellTemp;
							cell.PipeCellData.Insulation.Temperature_PrevTimeStep = ThisCellTemp;
						}
						cell.PipeCellData.Fluid.Temperature = ThisCellTemp;
						cell.PipeCellData.Fluid.Temperature_PrevIteration = ThisCellTemp;
						cell.PipeCellData.Fluid.Temperature_PrevTimeStep = ThisCellTemp;

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
		PipingSystemDomains( DomainNum ).Cur.CurAirTemp = DataEnvironment::OutDryBulbTemp;
		PipingSystemDomains( DomainNum ).Cur.CurWindSpeed = DataEnvironment::WindSpeed;
		PipingSystemDomains( DomainNum ).Cur.CurRelativeHumidity = DataEnvironment::OutRelHum;
		PipingSystemDomains( DomainNum ).Cur.CurIncidentSolar = DataEnvironment::BeamSolarRad;

		// If pipe circuit present
		if ( present( CircuitNum ) ) {
			// retrieve fluid properties based on the circuit inlet temperature -- which varies during the simulation
			// but need to verify the value of inlet temperature during warm up, etc.
			FluidCp = FluidProperties::GetSpecificHeatGlycol( DataPlant::PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidName, PipingSystemCircuits( CircuitNum ).InletTemperature, DataPlant::PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidIndex, RoutineName );
			FluidDensity = FluidProperties::GetDensityGlycol( DataPlant::PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidName, PipingSystemCircuits( CircuitNum ).InletTemperature, DataPlant::PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidIndex, RoutineName );
			FluidConductivity = FluidProperties::GetConductivityGlycol( DataPlant::PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidName, PipingSystemCircuits( CircuitNum ).InletTemperature, DataPlant::PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidIndex, RoutineName );
			FluidViscosity = FluidProperties::GetViscosityGlycol( DataPlant::PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidName, PipingSystemCircuits( CircuitNum ).InletTemperature, DataPlant::PlantLoop( PipingSystemCircuits( CircuitNum ).LoopNum ).FluidIndex, RoutineName );

			// Doesn't anyone care about poor Ludwig Prandtl?
			FluidPrandtl = 3.0;

			// then assign these fluid properties to the current fluid property set for easy lookup as needed
			PipingSystemCircuits( CircuitNum ).CurFluidPropertySet = ExtendedFluidProperties( FluidConductivity, FluidDensity, FluidCp, FluidViscosity, FluidPrandtl );
		}

		//'now update cell properties
		auto & dom( PipingSystemDomains( DomainNum ) );
		auto & cells( dom.Cells );
		for ( int X = 0, X_end = dom.x_max_index; X <= X_end; ++X ) {
			for ( int Y = 0, Y_end = dom.y_max_index; Y <= Y_end; ++Y ) {
				for ( int Z = 0, Z_end = dom.z_max_index; Z <= Z_end; ++Z ) {
					auto & cell( cells( X, Y, Z ) );
					switch ( cell.cellType ) {
					case CellType::GeneralField:
					case CellType::FarfieldBoundary:
					case CellType::GroundSurface:
					case CellType::BasementCorner:
					case CellType::BasementFloor:
					case CellType::BasementWall:
						// UPDATE CELL PROPERTY SETS
						//'main ground cells, update with soil properties
						CellTemp = cell.Temperature;
						PipingSystemDomains( DomainNum ).EvaluateSoilRhoCp( CellTemp, CellRhoCp );
						cell.Properties.SpecificHeat = CellRhoCp / cell.Properties.Density;
						// UPDATE BETA VALUE
						//'these are basic cartesian calculation cells
						Beta = PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize / ( cell.Properties.Density * cell.volume() * cell.Properties.SpecificHeat );
						cell.Beta = Beta;
						break;
					case CellType::HorizInsulation:
					case CellType::VertInsulation:
					case CellType::Slab:
					case CellType::ZoneGroundInterface:
						Beta = PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize / ( cell.Properties.Density * cell.volume() * cell.Properties.SpecificHeat );
						PipingSystemDomains ( DomainNum ).Cells ( X, Y, Z ).Beta = Beta;
						break;
					case CellType::Pipe:
						// If pipe circuit present
						if ( present( CircuitNum ) ) {
							// UPDATE CELL PROPERTY SETS
							//'first update the outer cell itself
							CellTemp = cell.Temperature;
							PipingSystemDomains( DomainNum ).EvaluateSoilRhoCp( CellTemp, CellRhoCp );
							cell.Properties.SpecificHeat = CellRhoCp / cell.Properties.Density;
							//'then update all the soil radial cells
							for ( radialctr = cell.PipeCellData.Soil.l1(); radialctr <= cell.PipeCellData.Soil.u1(); ++radialctr ) {
								CellTemp = cell.PipeCellData.Soil( radialctr ).Temperature;
								PipingSystemDomains( DomainNum ).EvaluateSoilRhoCp( CellTemp, CellRhoCp );
								cell.PipeCellData.Soil( radialctr ).Properties.SpecificHeat = CellRhoCp / cell.PipeCellData.Soil( radialctr ).Properties.Density;
							}

							// UPDATE BETA VALUES
							//'set the interface cell
							Beta = PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize / ( cell.Properties.Density * cell.PipeCellData.InterfaceVolume * cell.Properties.SpecificHeat );
							cell.Beta = Beta;

							//'set the radial soil cells
							for ( rCtr = 0; rCtr <= cell.PipeCellData.Soil.u1(); ++rCtr ) {
								Beta = PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize / ( cell.PipeCellData.Soil( rCtr ).Properties.Density * cell.PipeCellData.Soil( rCtr ).XY_CrossSectArea() * cell.depth() * cell.PipeCellData.Soil( rCtr ).Properties.SpecificHeat );
								cell.PipeCellData.Soil( rCtr ).Beta = Beta;
							}

							//'then insulation if it exists
							if ( PipingSystemCircuits( CircuitNum ).HasInsulation ) {
								Beta = PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize / ( cell.PipeCellData.Insulation.Properties.Density * cell.PipeCellData.Insulation.XY_CrossSectArea() * cell.depth() * cell.PipeCellData.Insulation.Properties.SpecificHeat );
								cell.PipeCellData.Insulation.Beta = Beta;
							}

							//'set the pipe cell
							Beta = PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize / ( cell.PipeCellData.Pipe.Properties.Density * cell.PipeCellData.Pipe.XY_CrossSectArea() * cell.depth() * cell.PipeCellData.Pipe.Properties.SpecificHeat );
							cell.PipeCellData.Pipe.Beta = Beta;

							// now the fluid cell also
							cell.PipeCellData.Fluid.Properties = PipingSystemCircuits( CircuitNum ).CurFluidPropertySet;
							cell.PipeCellData.Fluid.Beta = PipingSystemDomains( DomainNum ).Cur.CurSimTimeStepSize / ( cell.PipeCellData.Fluid.Properties.Density * cell.PipeCellData.Fluid.Volume * cell.PipeCellData.Fluid.Properties.SpecificHeat );
						}
						break;
					case CellType::BasementCutaway:
						break;
					case CellType::Unknown:
						assert( false );
					}
				}
			}
		}

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

		auto & thisDomain( PipingSystemDomains( DomainNum ) );

		//'check if we have converged for this iteration
		Finished = thisDomain.IsConverged_CurrentToPrevIteration();

		//'check for out of range temperatures here so they aren't plotted
		//'this routine should be *much* more restrictive than the exceptions, so we should be safe with this location
		bool OutOfRange = thisDomain.CheckForOutOfRangeTemps();
		if ( OutOfRange ) {
			if ( thisDomain.HasZoneCoupledSlab ) {
				ShowSevereError( "Site:GroundDomain:Slab" + RoutineName + ": Out of range temperatures detected in the ground domain." );
				ShowContinueError( "This could be due to the size of the loads on the domain." );
				ShowContinueError( "Verify inputs are correct. If problem persists, notify EnergyPlus support." );
				ShowFatalError( "Preceding error(s) cause program termination" );
			} else if ( thisDomain.HasZoneCoupledBasement ) {
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
		for( auto & neighborInfo : cell.NeighborInfo ) {
			if ( neighborInfo.first == direction ) {
				neighborInfo.second.ConductionResistance = Resistance;
				neighborInfo.second.NeighborCellIndeces = Point3DInteger( NeighborCell.X_index, NeighborCell.Y_index, NeighborCell.Z_index );
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
		CartesianCell & ThisCell,
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

		//'split effects between the two cells so we can carefully calculate resistance values
		Real64 ThisCellLength = 0.0;
		Real64 NeighborCellLength = 0.0;
		Real64 ThisCellConductivity = 10000.0;
		if ( ThisCell.Properties.Conductivity > 0.0 ) ThisCellConductivity = ThisCell.Properties.Conductivity;
		Real64 NeighborConductivity = 10000.0;
		auto const & cell( PipingSystemDomains( DomainNum ).Cells( NX, NY, NZ ) );
		if ( cell.Properties.Conductivity > 0.0 ) NeighborConductivity = cell.Properties.Conductivity;

		//'calculate normal surface area
		Real64 ThisNormalArea = ThisCell.normalArea( CurDirection );

		//'set distance based on cell types
		auto & TempNeighborInfo = ThisCell.NeighborInfo[ CurDirection ];
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
		NeighborTemp = cell.Temperature;

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
		auto const & dom( PipingSystemDomains( DomainNum ) );
		int Xmax = dom.x_max_index;
		int Ymax = dom.y_max_index;
		int Zmax = dom.z_max_index;
		int Xindex = cell.X_index;
		int Yindex = cell.Y_index;
		int Zindex = cell.Z_index;

		NumFieldCells = 0;
		NumBoundaryCells = 0;

		if ( Xindex < Xmax ) {
			++NumFieldCells;
			NeighborFieldCells( NumFieldCells ) = Direction::PositiveX;
		} else {
			++NumBoundaryCells;
			NeighborBoundaryCells( NumBoundaryCells ) = Direction::PositiveX;
		}

		if ( Xindex > 0 ) {
			++NumFieldCells;
			NeighborFieldCells( NumFieldCells ) = Direction::NegativeX;
		} else {
			++NumBoundaryCells;
			NeighborBoundaryCells( NumBoundaryCells ) = Direction::NegativeX;
		}

		if ( Yindex < Ymax ) {
			++NumFieldCells;
			NeighborFieldCells( NumFieldCells ) = Direction::PositiveY;
		} else {
			++NumBoundaryCells;
			NeighborBoundaryCells( NumBoundaryCells ) = Direction::PositiveY;
		}

		if ( Yindex > 0 ) {
			++NumFieldCells;
			NeighborFieldCells( NumFieldCells ) = Direction::NegativeY;
		} else {
			++NumBoundaryCells;
			NeighborBoundaryCells( NumBoundaryCells ) = Direction::NegativeY;
		}

		if ( Zindex < Zmax ) {
			++NumFieldCells;
			NeighborFieldCells( NumFieldCells ) = Direction::PositiveZ;
		} else {
			++NumBoundaryCells;
			NeighborBoundaryCells( NumBoundaryCells ) = Direction::PositiveZ;
		}

		if ( Zindex > 0 ) {
			++NumFieldCells;
			NeighborFieldCells( NumFieldCells ) = Direction::NegativeZ;
		} else {
			++NumBoundaryCells;
			NeighborBoundaryCells( NumBoundaryCells ) = Direction::NegativeZ;
		}

	}

} // PlantPipingSystemsManager

} // EnergyPlus
