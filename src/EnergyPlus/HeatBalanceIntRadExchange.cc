// C++ Headers
#include <cassert>
#include <cmath>
#include <cassert>
#include <numeric>
#include <functional>
#include <vector>
#include <forward_list>
#include <thread>
#ifdef DEBUG_CI
#include <iostream>
#endif

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <HeatBalanceIntRadExchange.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalSurface.hh>
#include <DataIPShortCuts.hh> 
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <DataSystemVariables.hh>
#include <DataTimings.hh>
#include <DataViewFactorInformation.hh>
#include <DisplayRoutines.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>
#include <WindowEquivalentLayer.hh>
#include <Timer.h>

// SpeedupHelpers
#include <timers.hh>

// Linear LU Solver
extern "C"{
#include <clapack.h>
}

namespace EnergyPlus {

#define EP_HBIRE_SEQ
#define DEBUG_SH
namespace HeatBalanceIntRadExchange {
	// Module containing the routines dealing with the interior radiant exchange
	// between surfaces.

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   September 2000
	//       MODIFIED       Aug 2001, FW: recalculate ScriptF for a zone if window interior
	//                       shade/blind status is different from previous time step. This is
	//                       because ScriptF, which is used to calculate interior LW
	//                       exchange between surfaces, depends on inside surface emissivities,
	//                       which, for a window, depends on whether or not an interior
	//                       shade or blind is in place.
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Part of the heat balance modularization/re-engineering.  Purpose of this
	// module is to replace the MRT with RBAL method of modeling radiant exchange
	// between interior surfaces.

	// METHODOLOGY EMPLOYED:
	// Standard EnergyPlus methodology

	// REFERENCES:
	// ASHRAE Loads Toolkit "Script F" routines by Curt Pedersen
	// Hottel, H.C., and A.F. Sarofim. "Radiative Transfer" (mainly chapter 3),
	//  McGraw-Hill, Inc., New York, 1967.

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataEnvironment;
	using namespace DataHeatBalance;
	using namespace DataSurfaces;
	using namespace DataSystemVariables;
	using namespace DataViewFactorInformation;
	using namespace DataTimings;

	// Data
	// MODULE PARAMETER DEFINITIONS
	static gio::Fmt const fmtLD( "*" );
	static gio::Fmt const fmtA( "(A)" );
	static gio::Fmt const fmtx( "(A,I4,1x,A,1x,6f16.8)" );
	static gio::Fmt const fmty( "(A,1x,6f16.8)" );

	// DERIVED TYPE DEFINITIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	int MaxNumOfZoneSurfaces; // Max saved to get large enough space for user input view factors

	std::vector<size_t> LoadBalanceVector;

	std::forward_list<EppPerformance::genPerTArray*> WriteVectors;

	std::vector<std::pair<std::vector< ReSurface >::iterator,
			      std::vector< ReSurface >::iterator>> threadSurfIterators;	// SUBROUTINE SPECIFICATIONS FOR MODULE HeatBalanceIntRadExchange

  int count = 0;
	void
	CalcInteriorRadExchange(const int SurfIterations,
													const int ZoneToResimulate){
		using EppPerformance::Timer;
    
		static Timer timer(__PRETTY_FUNCTION__);
		timer.startTimer();

		static bool firstTime( true );
		if(firstTime){
			InitInteriorRadExchange();
			firstTime = false;
		}
		if( KickOffSimulation || KickOffSizing) return;
		if(ZoneToResimulate != -1){
			DoCalcInteriorRadExchange(SurfIterations, ZoneToResimulate);
		}else{
			std::forward_list<std::thread> threads;
			for(int x = 0; x < EppPerformance::Perf_Thread_Count; ++x){
				threads.push_front(std::thread(DoCalcInteriorRadExchange,
							       SurfIterations,
							       ZoneToResimulate,
							       x));
				     
			}
			for(auto& t: threads){
				t.join();
			}
			for(auto& z: ZoneInfo){z.ready = false;}
		}
		timer.stopTimer();
		++count;
// #ifdef DEBUG_SH
// 		EppPerformance::Utility::doDataDump();
// #endif
	}

	void
	DoCalcInteriorRadExchange(const int SurfIterations, 
														const int ZoneToResimulate,
														const int tid){
		std::vector<bool> ZoneChecked(NumOfZones, false);
#ifdef DEBUG_CI
			static int ranCount = 0;
			const int MAX_RUNS(1000);
#endif    
		//tid will be -1 when ZoneToResimulate != -1
		for(auto s = surfBegin(tid, ZoneToResimulate); s != surfEnd(tid, ZoneToResimulate); s++){
			ReSurface& recv = (*s);
			//if(!s.isHeatTransSurf) continue; //I think this is superfluous, as it should have been checked before adding the surface
			ZoneViewFactorInformation& zone = ZoneInfo[ (*s).zone ];
			if(ZoneToResimulate == -1){
				if(!ZoneChecked[zone()]){
					if(zone.ready == false){
						if(zone.owner == tid){
							if(SurfIterations == 0 && (zone.shadeChanged || BeginEnvrnFlag)){
								CalcScriptF(zone); //calls CalcSurfaceEmiss
								zone.shadeChanged = false;
							}
							CalcSurfaceTemp(zone, SurfIterations);
							zone.ready = true;
						}else{
							while(zone.ready == false){}
						}
					}
				}
			}else{
				CalcSurfaceTemp(zone, SurfIterations);
				CalcSurfaceEmiss(zone);
			}
	
			ZoneChecked[zone()] == true;
			Real64 tIR, tLWR;
			tIR = tLWR = 0;
			for(auto send : zone.surfaces){
				//delme
				// std::cout << "count " << count << " rec surf: " << recv() <<
				// 	" temp: " << recv.temperature
				// 					<< std::endl;
				//end delme
				if (recv.isWindow){
					tIR += zone.ScriptF(recv(false) + 1, send(false) + 1) * 
						send.temperature / recv.emissivity;
					// std::cout << "scriptF@ " << recv(false) << "," 
					// 	  << send(false) << ": " << 
					//   zone.ScriptF(recv(false), send(false)) <<
					//   std::endl;
				}
				if(recv() != send()){
					tLWR += zone.ScriptF(recv(false) + 1, send(false) + 1) * 
						(send.temperature - recv.temperature);
				}
			}
			DataSurfaces::IRfromParentZone[ recv() ] = tIR;

			DataHeatBalSurface::NetLWRadToSurf[ recv() ] = tLWR;
#ifdef DEBUG_CI
			if (ranCount < MAX_RUNS ){
			std::cout << "cire, z:" << zone() << " s:" << recv(false) 
								<< " ir:" << tIR << " lwr:"
								<< tLWR << std::endl;
			}
#endif
			tIR = tLWR = 0;
		}
#ifdef DEBUG_CI
		++ranCount;
#endif
	}

	void
	CalcSurfaceEmiss(ZoneViewFactorInformation& Zone){
		for(auto& surf: Zone.surfaces){
			if( !ConstrWin[ Construction[ surf() ] - 1].TypeIsWindow ||
					(SurfaceRadiantWin[ surf() ].getShadingFlag() != IntShadeOn &&
					 SurfaceRadiantWin[ surf() ].getShadingFlag() != IntBlindOn)){
				surf.emissivity = ConstrWin[ Construction[ surf() ] - 1].InsideAbsorpThermal;
			}else{
				surf.emissivity = General::InterpSlatAng( SurfaceRadiantWin[ surf() ].SlatAngThisTS,
																									SurfaceRadiantWin[ surf() ].MovableSlats,
																									SurfaceRadiantWin[ surf() ].EffGlassEmiss );
			}
		}
	}

	void
	CalcSurfaceTemp(ZoneViewFactorInformation& Zone, int SurfIterations){
	  for(auto& surf: Zone.surfaces){
	    if(!ConstrWin[ Construction[ surf() ] - 1 ].TypeIsWindow ||
	       SurfaceRadiantWin[ surf() ].OriginalClass == SurfaceClass_TDD_Diffuser){
	      surf.temperature = std::pow(DataHeatBalSurface::TH(surf()+1, 1, 2) + KelvinConv, 4);
	    }else{
	      int shadeFlag = SurfaceRadiantWin[ surf() ].getShadingFlag();
	      if(SurfIterations == 0 && 
		 shadeFlag <= 0){
		surf.temperature = std::pow(SurfaceRadiantWin[ surf() ].ThetaFace( 2 * ConstrWin[ Construction[ surf() ] - 1 ].
										   TotGlassLayers ), 4); //ThetaFace already Kelvin
	      }else if(shadeFlag == IntShadeOn || shadeFlag == IntBlindOn){
		surf.temperature = std::pow(SurfaceRadiantWin[ surf() ].EffInsSurfTemp + KelvinConv, 4);
	      }else{
		surf.temperature = std::pow(DataHeatBalSurface::TH(surf()+1, 1, 2) + KelvinConv, 4);
	      }
	    }
	  }
	}

	void
	CalcScriptF(ZoneViewFactorInformation& Zone){
		using EppPerformance::Timer;
		// This thread_local static isn't working out -- actually,
		// when you think about it, it doesn't make sense
		//		thread_local static Timer timer(__PRETTY_FUNCTION__);
		//timer.startTimer();

		CalcSurfaceEmiss(Zone);
		Real64 const StefanBoltzmannConst( 5.6697e-8 ); // Stefan-Boltzmann constant in W/(m2*K4)
		Real64 const MAX_EMISS( 0.9999 );
		Real64 *jMatrix, *cMatrix;
		int surfCount = Zone.NumOfSurfaces;
		cMatrix = new Real64[surfCount * surfCount];
		jMatrix = new Real64[surfCount * surfCount];



		if(cMatrix == nullptr || jMatrix == nullptr){
			throw noMoreMemCalcSF();
		}

		//calculate and load cMatrix and jMatrix
		for(auto i: Zone.surfaces){
			for(auto j: Zone.surfaces){
				Real64 cmTemp = Zone.F(i(false) + 1, j(false) + 1) * Zone.Area(i(false) +1);
				if(i(false) == j(false)){
					Real64 area = Zone.Area(i(false) + 1);
					Real64 emiss = Zone.Emissivity( i(false) + 1 ); //i.emissivity;
					emiss = emiss >  MAX_EMISS ? MAX_EMISS : emiss;
					jMatrix[i(false) * surfCount + i(false)] = -area * emiss
						/ (1.0 - emiss);
					cmTemp -= area / (1.0 - emiss);
				}else{
					jMatrix[i(false) * surfCount + j(false)] = 0;
				}
				cMatrix[i(false) * surfCount + j(false)] = cmTemp;
			}
		}

		int *ipiv = new int[surfCount];
// #ifdef DEBUG_CI
// 		std::cout << "Dumping prelims in CalcScriptF Zone:" << Zone() << std::endl;
// 		std::cout << "jMatrix first." << std::endl;
// 		for(int x = 0; x < Zone.NumOfSurfaces; ++x){
// 			for(int y = 0; y < Zone.NumOfSurfaces; ++y){
// 				std::cout << jMatrix[ x * surfCount + y] << " ";
// 			}
// 			std::cout << std::endl;
// 		}
// 		std::cout << "Now cMatrix." << std::endl;
// 		for(int x = 0; x < Zone.NumOfSurfaces; ++x){
// 			for(int y = 0; y < Zone.NumOfSurfaces; ++y){
// 				std::cout << cMatrix[ x * surfCount + y] << " ";
// 			}
// 			std::cout << std::endl;
// 		}

// #endif
		int result = clapack_dgesv(CblasRowMajor, surfCount, 
															 surfCount, cMatrix, surfCount, 
															 ipiv, jMatrix, surfCount);
		delete[] cMatrix; //made this as early as possible -- it appears that having 8 threads allocate NxN all at once for large zones 
		//is having an acute impact on system memory
		delete[] ipiv;

		if( result == 0){ //success
// #ifdef DEBUG_CI
// 			std::cout << "Finished calculating linear system.  Here's the result: " << 
// 				std::endl;
// 			for(int x = 0; x < Zone.NumOfSurfaces; ++x){
// 				for(int y = 0; y < Zone.NumOfSurfaces; ++y){
// 					std::cout << jMatrix[ y * surfCount + x] << " ";
// 				}
// 				std::cout << std::endl;
// 			}

// #endif
			for(auto i: Zone.surfaces){
				Real64 emiss = Zone.Emissivity( i(false) + 1 );
				emiss = emiss > MAX_EMISS ? MAX_EMISS : emiss;
				for(auto j: Zone.surfaces){
					Real64 temp;
					if(i(false) != j(false)){
						//dgesv seems to return transposed matrices!  s/b i * sc + j 
						temp = emiss / (1.0 - emiss) * jMatrix[j(false) * surfCount + i(false)];
					}else{
						temp = emiss / (1.0 - emiss) * (jMatrix[j(false) * surfCount + i(false)]
																						- emiss);
					}
					Zone.ScriptF(i(false) + 1, j(false) + 1) = temp * StefanBoltzmannConst;
				}
			}
		}else{
			throw badLU();
		}
		delete[] jMatrix;
// #ifdef DEBUG_CI
// 		std::cout << "Finished an iteration of CSF.  Here's ScriptF in zone " <<
// 			Zone() << ":" << std::endl;
// 		for(int x = 1; x <= Zone.NumOfSurfaces; ++x){
// 			for(int y = 1; y <= Zone.NumOfSurfaces; ++y){
// 				std::cout << Zone.ScriptF( x, y ) << " ";
// 			}
// 			std::cout << std::endl; 
// 		}
// #endif
//		timer.stopTimer();
	}
	void
	InitInteriorRadExchange()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   September 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initializes the various parameters for Hottel's ScriptF method for
		// the grey interchange between surfaces in an enclosure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectDefMaxArgs;
		using General::RoundSigDigits;
		using General::ScanForReports;
		using EppPerformance::Perf_Thread_Count;

		// Locals
		// SUBROUTINE ARGUMENTS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt const AFormat( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumOfZoneSurfaces; // total number of surfaces in the zone.
		int SurfNum; // Counter within DO loop (refers to main surface derived type index)
		int ZoneNum; // DO loop counter for zones
		int ZoneSurfNum; // DO loop counter for surfaces within a zone (refers to local derived type arrays)
		int Findex; // index to print view factors
		int Vindex; // index for vertices
		int NumZonesWithUserFbyS; // Zones with user input,  used for flag here
		bool NoUserInputF; // Logical flag signifying no input F's for zone
		static bool ViewFactorReport; // Flag to output view factor report in eio file
		static bool ErrorsFound( false );
		Real64 CheckValue1;
		Real64 CheckValue2;
		Real64 FinalCheckValue;
		FArray2D< Real64 > SaveApproximateViewFactors; // Save for View Factor reporting
		Real64 RowSum;
		Real64 FixedRowSum;
		int NumIterations;
		std::string Option1; //Fstring Option1( MaxNameLength ); // view factor report option

		using EppPerformance::Timer;
    
		static Timer timer(__PRETTY_FUNCTION__);
		timer.startTimer();

		// FLOW:
// #ifdef DEBUG_SH
// 		//		EppPerformance::Utility::doDataDump();
// #endif


		ZoneInfo.resize( NumOfZones ); // Allocate the entire derived type

		ScanForReports( "ViewFactorInfo", ViewFactorReport, _, Option1 );

		if ( ViewFactorReport ) { // Print heading
			gio::write( OutputFileInits, fmtA ) << "! <Surface View Factor and Grey Interchange Information>";
			gio::write( OutputFileInits, fmtA ) << "! <View Factor - Zone Information>,Zone Name,Number of Surfaces";
			gio::write( OutputFileInits, fmtA ) << "! <View Factor - Surface Information>,Surface Name,Surface Class,Area {m2},Azimuth," "Tilt,Thermal Emissivity,#Sides,Vertices";
			gio::write( OutputFileInits, fmtA ) << "! <View Factor / Grey Interchange Type>,Surface Name(s)";
			gio::write( OutputFileInits, fmtA ) << "! <View Factor>,Surface Name,Surface Class,Row Sum,View Factors for each Surface";
		}

		cCurrentModuleObject = "ZoneProperty:UserViewFactors:bySurfaceName";
		NumZonesWithUserFbyS = GetNumObjectsFound( cCurrentModuleObject );

		MaxNumOfZoneSurfaces = 0;
		LoadBalanceVector.resize(Perf_Thread_Count);
		int totalSurfZoneDensity = 0;
		int totalHTSurfaces = 0;
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			if ( ZoneNum == 1 ) {
				if ( DisplayAdvancedReportVariables ) gio::write( OutputFileInits, fmtA ) << "! <Surface View Factor Check Values>,Zone Name,Original Check Value," "Calculated Fixed Check Value,Final Check Value,Number of Iterations,Fixed RowSum Convergence," "Used RowSum Convergence";
			}

			ZoneInfo[ ZoneNum - 1 ].Name = Zone( ZoneNum ).Name;

			NumOfZoneSurfaces = 0;


			for ( SurfNum = ZoneSpecs[ ZoneNum - 1 ].SurfaceFirst; SurfNum <= ZoneSpecs[ ZoneNum - 1 ].SurfaceLast; ++SurfNum ) {
				if ( Surface( SurfNum ).HeatTransSurf ){
					++NumOfZoneSurfaces;
				}
			}
			totalHTSurfaces += NumOfZoneSurfaces;
			totalSurfZoneDensity += NumOfZoneSurfaces * NumOfZoneSurfaces;
			ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces = NumOfZoneSurfaces;
			MaxNumOfZoneSurfaces = max( MaxNumOfZoneSurfaces, NumOfZoneSurfaces );
			if ( ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces < 1 ) ShowFatalError( "No surfaces in a zone in InitInteriorRadExchange" );

			// Allocate the parts of the derived type
			ZoneInfo[ ZoneNum - 1 ].F.allocate( ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces, ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces );
			ZoneInfo[ ZoneNum - 1 ].F = 0.0;
			ZoneInfo[ ZoneNum - 1 ].ScriptF.allocate( ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces, ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces );
			ZoneInfo[ ZoneNum - 1 ].ScriptF = 0.0;
			ZoneInfo[ ZoneNum - 1 ].Area.allocate( ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces );
			ZoneInfo[ ZoneNum - 1 ].Area = 0.0;
			ZoneInfo[ ZoneNum - 1 ].Emissivity.allocate( ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces );
			ZoneInfo[ ZoneNum - 1 ].Emissivity = 0.0;
			ZoneInfo[ ZoneNum - 1 ].Azimuth.allocate( ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces );
			ZoneInfo[ ZoneNum - 1 ].Azimuth = 0.0;
			ZoneInfo[ ZoneNum - 1 ].Tilt.allocate( ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces );
			ZoneInfo[ ZoneNum - 1 ].Tilt = 0.0;
			ZoneInfo[ ZoneNum - 1 ].SurfacePtr.allocate( ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces );
			ZoneInfo[ ZoneNum - 1 ].SurfacePtr = 0;//TODO is this redundant?

			// Initialize the surface pointer array
			ZoneSurfNum = 0;
			for ( SurfNum = ZoneSpecs[ ZoneNum - 1 ].SurfaceFirst; SurfNum <= ZoneSpecs[ ZoneNum - 1 ].SurfaceLast; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				++ZoneSurfNum;
				ZoneInfo[ ZoneNum - 1 ].SurfacePtr( ZoneSurfNum ) = SurfNum;
			}
			// Initialize the area and emissivity arrays
			for ( ZoneSurfNum = 1; ZoneSurfNum <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++ZoneSurfNum ) {
				SurfNum = ZoneInfo[ ZoneNum - 1 ].SurfacePtr( ZoneSurfNum );

				//************************************************
				if ( ! Construct( Construction[ SurfNum  - 1] ).TypeIsIRT ) {
					ZoneInfo[ ZoneNum - 1 ].Area( ZoneSurfNum ) = Surface( SurfNum ).Area;
				} else {
					// Double area for infrared transparent (IRT) surfaces
					ZoneInfo[ ZoneNum - 1 ].Area( ZoneSurfNum ) = 2.0 * Surface( SurfNum ).Area;
				}
				//***********************************************

				ZoneInfo[ ZoneNum - 1 ].Emissivity( ZoneSurfNum ) = ConstrWin[ Construction[ SurfNum  - 1]  - 1 ].InsideAbsorpThermal;
				ZoneInfo[ ZoneNum - 1 ].Azimuth( ZoneSurfNum ) = Surface( SurfNum ).Azimuth;
				ZoneInfo[ ZoneNum - 1 ].Tilt( ZoneSurfNum ) = Surface( SurfNum ).Tilt;
			}

			if ( ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces == 1 ) {
				// If there is only one surface in a zone, then there is no radiant exchange
				ZoneInfo[ ZoneNum - 1 ].F = 0.0;
				ZoneInfo[ ZoneNum - 1 ].ScriptF = 0.0;
				if ( DisplayAdvancedReportVariables ) gio::write( OutputFileInits, fmtA ) << "Surface View Factor Check Values," + Zone( ZoneNum ).Name + ",0,0,0,-1,0,0";
				continue; // Go to the next zone in the  ZoneNum DO loop
			}

			//  Get user supplied view factors if available in idf.

			NoUserInputF = true;

			if ( NumZonesWithUserFbyS > 0 ) {

				GetInputViewFactorsbyName( ZoneInfo[ ZoneNum - 1 ].Name, ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces, ZoneInfo[ ZoneNum - 1 ].F, ZoneInfo[ ZoneNum - 1 ].SurfacePtr, NoUserInputF, ErrorsFound ); // Obtains user input view factors from input file
			}

			if ( NoUserInputF ) {

				// Calculate the view factors and make sure they satisfy reciprocity
				CalcApproximateViewFactors( ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces, ZoneInfo[ ZoneNum - 1 ].Area, ZoneInfo[ ZoneNum - 1 ].Azimuth, ZoneInfo[ ZoneNum - 1 ].Tilt, ZoneInfo[ ZoneNum - 1 ].F, ZoneInfo[ ZoneNum - 1 ].SurfacePtr );
			}

			if ( ViewFactorReport ) { // Allocate and save user or approximate view factors for reporting.
				SaveApproximateViewFactors.allocate( ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces, ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces );
				SaveApproximateViewFactors = ZoneInfo[ ZoneNum - 1 ].F;
			}

			FixViewFactors( ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces, ZoneInfo[ ZoneNum - 1 ].Area, ZoneInfo[ ZoneNum - 1 ].F, ZoneNum, CheckValue1, CheckValue2, FinalCheckValue, NumIterations, FixedRowSum );

			// Calculate the script F factors
			//CalcScriptF( ZoneInfo[ ZoneNum - 1 ]);//.NumOfSurfaces, ZoneInfo[ ZoneNum - 1 ].Area, ZoneInfo[ ZoneNum - 1 ].F, ZoneInfo[ ZoneNum - 1 ].Emissivity, ZoneInfo[ ZoneNum - 1 ].ScriptF );

			if ( ViewFactorReport ) { // Write to SurfInfo File
				// Zone Surface Information Output
				gio::write( OutputFileInits, fmtA ) << "Surface View Factor - Zone Information," + trim( ZoneInfo[ ZoneNum - 1 ].Name ) + "," + trim( RoundSigDigits( ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces ) );

				for ( SurfNum = 1; SurfNum <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++SurfNum ) {
					gio::write( OutputFileInits, "(A,',',A,$)" )
						<< "Surface View Factor - Surface Information,"
						+  Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( SurfNum ) ).Name + ','
						+  cSurfaceClass( Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( SurfNum ) ).Class)
						<<  RoundSigDigits( ZoneInfo[ ZoneNum - 1 ].Area( SurfNum ), 4 )+ ','
						+  RoundSigDigits( ZoneInfo[ ZoneNum - 1 ].Azimuth( SurfNum ), 4 ) + ','
						+  RoundSigDigits( ZoneInfo[ ZoneNum - 1 ].Tilt( SurfNum ), 4 ) + ','
						+  RoundSigDigits( ZoneInfo[ ZoneNum - 1 ].Emissivity( SurfNum ), 4 ) + ','
						+  RoundSigDigits( Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( SurfNum ) ).Sides );
					for ( Vindex = 1; Vindex <= Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( SurfNum ) ).Sides; ++Vindex ) {
						auto & Vertex = Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( SurfNum ) ).Vertex( Vindex );
						gio::write( OutputFileInits, "(3(',',A),$)" )
							<< RoundSigDigits( Vertex.x, 4 )
							<< RoundSigDigits( Vertex.y, 4 )
							<< RoundSigDigits( Vertex.z, 4 );
					} gio::write( OutputFileInits );
				}

				gio::write( OutputFileInits, "(A,A,$)" )
					<< "Approximate or User Input ViewFactors"
					<< ",To Surface,Surface Class,RowSum";
				for ( SurfNum = 1; SurfNum <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++SurfNum ) {
					gio::write( OutputFileInits, "(',',A,$)" )
						<< Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( SurfNum ) ).Name;
				} gio::write( OutputFileInits );

				for ( Findex = 1; Findex <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++Findex ) {
					RowSum = sum( SaveApproximateViewFactors( Findex, _ ) );
					gio::write( OutputFileInits, "(A,3(',',A),$)" )
						<< "View Factor"
						<< Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( Findex ) ).Name
						<< cSurfaceClass( Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( Findex ) ).Class )
						<< RoundSigDigits( RowSum, 4 );
					for ( SurfNum = 1; SurfNum <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++SurfNum ) {
						gio::write( OutputFileInits, "(',',A,$)" )
							<< RoundSigDigits( SaveApproximateViewFactors( Findex, SurfNum ), 4 );
					} gio::write( OutputFileInits );
				}
			}

			if ( ViewFactorReport ) {
				gio::write( OutputFileInits, "(A,A,$)" ) << "Final ViewFactors" << ",To Surface,Surface Class,RowSum";
				for ( SurfNum = 1; SurfNum <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++SurfNum ) {
					gio::write( OutputFileInits, "(',',A,$)" ) << Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( SurfNum ) ).Name;
				} gio::write( OutputFileInits );

				for ( Findex = 1; Findex <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++Findex ) {
					RowSum = sum( ZoneInfo[ ZoneNum - 1 ].F( Findex, _ ) );
					gio::write( OutputFileInits, "(A,3(',',A),$)" )
						<< "View Factor"
						<< Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( Findex ) ).Name
						<< cSurfaceClass( Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( Findex ) ).Class )
						<< RoundSigDigits( RowSum, 4 );
					for ( SurfNum = 1; SurfNum <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++SurfNum ) {
						gio::write( OutputFileInits, "(',',A,$)" ) << RoundSigDigits( ZoneInfo[ ZoneNum - 1 ].F( Findex, SurfNum ), 4 );
					} gio::write( OutputFileInits );
				}

				if ( Option1 == "IDF" ) {
					gio::write( OutputFileDebug, fmtA ) << "!======== original input factors ===========================";
					gio::write( OutputFileDebug, fmtA ) << "ZoneProperty:UserViewFactors:bySurfaceName," + ZoneInfo[ ZoneNum - 1 ].Name + ',';
					for ( SurfNum = 1; SurfNum <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++SurfNum ) {
						for ( Findex = 1; Findex <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++Findex ) {
							if ( ! ( SurfNum == ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces && Findex == ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces ) ) {
								gio::write( OutputFileDebug, fmtA ) << '  ' + Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( SurfNum ) ).Name + ',' + 
									Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( Findex ) ).Name + ',' + 
									RoundSigDigits( ZoneInfo[ ZoneNum - 1 ].F( SurfNum, Findex ), 6 ) + ',';
							} else {
								gio::write( OutputFileDebug, fmtA ) << "  " + Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( SurfNum ) ).Name + ',' + 
									Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( Findex ) ).Name + ',' + RoundSigDigits( ZoneInfo[ ZoneNum - 1 ].F( SurfNum, Findex ), 6 ) + ';';
							}
						}
					}
					gio::write( OutputFileDebug, fmtA ) << "!============= end of data ======================";

					gio::write( OutputFileDebug, fmtA ) << "!============ final view factors =======================";
					gio::write( OutputFileDebug, fmtA ) << "ZoneProperty:UserViewFactors:bySurfaceName," + ZoneInfo[ ZoneNum - 1 ].Name + ',';
					for ( SurfNum = 1; SurfNum <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++SurfNum ) {
						for ( Findex = 1; Findex <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++Findex ) {
							if ( ! ( SurfNum == ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces && Findex == ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces ) ) {
								gio::write( OutputFileDebug, fmtA ) << "  " + Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( SurfNum ) ).Name + ',' + 
									Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( Findex ) ).Name + ',' + RoundSigDigits( ZoneInfo[ ZoneNum - 1 ].F( SurfNum, Findex ), 6 ) + ',';
							} else {
								gio::write( OutputFileDebug, fmtA ) << "  " + Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( SurfNum ) ).Name + ',' + 
									Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( Findex ) ).Name + ',' + RoundSigDigits( ZoneInfo[ ZoneNum - 1 ].F( SurfNum, Findex ), 6 ) + ';';
							}
						}
					}
					gio::write( OutputFileDebug, fmtA ) << "!============= end of data ======================";
				}

			}

			if ( ViewFactorReport ) {
				gio::write( OutputFileInits, "(A,A,$)" )
					<< "Script F Factors"
					<< ",X Surface";
				for ( SurfNum = 1; SurfNum <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++SurfNum ) {
					gio::write( OutputFileInits, "(',',A,$)" ) <<
						Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( SurfNum ) ).Name;
				} gio::write( OutputFileInits );
				for ( Findex = 1; Findex <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++Findex ) {
					gio::write( OutputFileInits, "(A,',',A,$)" )
						<< "Script F Factor"
						<< Surface( ZoneInfo[ ZoneNum - 1 ].SurfacePtr( Findex ) ).Name;
					for ( SurfNum = 1; SurfNum <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++SurfNum ) { //TODO: This may need to be reinstated?  It causes an unneccessary call to CalcScriptF -- expensive
						gio::write( OutputFileInits, "(',',A,$)" ) //TODO: PLUS I don't know if it's initialized now 
							<< RoundSigDigits( ZoneInfo[ ZoneNum - 1 ].ScriptF( Findex, SurfNum ), 4 );
					}
					gio::write( OutputFileInits );
				}
			}

			if ( ViewFactorReport ) { // Deallocate saved approximate/user view factors
				SaveApproximateViewFactors.deallocate();
			}

			RowSum = 0.0;
			for ( Findex = 1; Findex <= ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces; ++Findex ) {
				RowSum += sum( ZoneInfo[ ZoneNum - 1 ].F( Findex, _ ) );
			}
			RowSum = std::abs( RowSum - ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces );
			FixedRowSum = std::abs( FixedRowSum - ZoneInfo[ ZoneNum - 1 ].NumOfSurfaces );
			if ( DisplayAdvancedReportVariables ) {
				gio::write( OutputFileInits, "(8A)" )
					<< "Surface View Factor Check Values,"
					 + Zone( ZoneNum ).Name + ','
					 + RoundSigDigits( CheckValue1, 6 ) + ','
					 + RoundSigDigits( CheckValue2, 6 ) + ','
					 + RoundSigDigits( FinalCheckValue, 6 ) + ','
					 + RoundSigDigits( NumIterations ) + ','
					 + RoundSigDigits( FixedRowSum, 6 ) + ','
					 + RoundSigDigits( RowSum, 6 );
			}

		}

		//thread load balancing calcs
		int calcsPerT = totalSurfZoneDensity / Perf_Thread_Count;
		int curThread = 0;
		int curDensity = 0;
		int surfCount = 0;
		int zoneIndex = 0;
		int tSurfIndexEnd = 0;
		int tSurfIndexBeg = 0;
		int zSurfIndex = 0;
		threadSurfIterators.resize(Perf_Thread_Count);
		VfSurfaces.resize(totalHTSurfaces);
		//so we're setting the thread surface iterators,
		//the thread zone owners, and the surface
		//data for HBIRE
		for(int z = 1; z <= NumOfZones; ++ z){
			ZoneInfo[ z - 1 ].surfBegin = VfSurfaces.begin() + 
				zSurfIndex;
			ZoneInfo[ z - 1 ].setIndex(z - 1);
			ZoneInfo[ z - 1 ].owner = curThread; //may move this to end of loop for latest thread
			for(int s = ZoneSpecs[z - 1].SurfaceFirst; 
					s <= ZoneSpecs[z - 1].SurfaceLast; 
					++s){
				if(Surface( s ).HeatTransSurf){
					curDensity += ZoneInfo[ z - 1].NumOfSurfaces; 
					++surfCount;
					++tSurfIndexEnd;
					if(curDensity > calcsPerT && curThread < Perf_Thread_Count - 1){
						LoadBalanceVector[ curThread ] = surfCount;
						threadSurfIterators[ curThread ].first = VfSurfaces.begin() + 
							tSurfIndexBeg;
						threadSurfIterators[ curThread ].second = VfSurfaces.begin() + 
							tSurfIndexEnd;
						++curThread;
						surfCount = curDensity = 0;
						tSurfIndexBeg = tSurfIndexEnd;
					}
					VfSurfaces[ tSurfIndexEnd - 1 ].zone =  z - 1;
					VfSurfaces[ tSurfIndexEnd - 1 ].globalIndex = s - 1;
					// ZoneInfo[ zoneIndex ].index = zoneIndex;
					VfSurfaces[ tSurfIndexEnd - 1 ].zoneIndex = zoneIndex++;
					VfSurfaces[ tSurfIndexEnd - 1 ].isWindow = ConstrWin[ Construction[ s - 1 ] - 1].TypeIsWindow;
					SurfaceRadiantWin[ s - 1 ].shadeChangedCallback = //is it better to store zone and call directly??
						//but then that would introduce a source code dependency from DataSurfaces -> DataViewFactorInformation
						//ZoneInfo[ z - 1 ].setShadeChanged( s );
						std::bind(&ZoneViewFactorInformation::setShadeChanged, 
											std::ref( ZoneInfo [ z - 1 ] ),
											s );
					++zSurfIndex;
				}
			}
			ZoneInfo[ z - 1 ].surfEnd = VfSurfaces.begin() + zSurfIndex;
			zoneIndex = 0;
			// ZoneInfo[ z - 1 ].surfaces.zone = ZoneInfo[ z - 1 ];
		}
		LoadBalanceVector[curThread] = surfCount;
		threadSurfIterators[curThread].first = VfSurfaces.begin() + 
			tSurfIndexBeg;
		threadSurfIterators[curThread].second = VfSurfaces.begin() + 
			tSurfIndexEnd;
		
		//this is an optimizing step -- its best for the thread that hits the zone 
		//first to do the pre-calcs (i.e. surface temp ^4 + calcScriptF) -- this 
		//way for the crossover zones (where more than one thread is involved) one
		//that hits first will calc (there can be multiple but only one will be the owner)
		for(int t = 0; t < Perf_Thread_Count; ++t){
			ZoneInfo[ (*threadSurfIterators[ t ].first).zone ].owner = t;
		}

		for(auto vect: WriteVectors){
			if(!vect->isOptimized()){
				vect->optimize(LoadBalanceVector);
			}
		}
		// int thread = 0;
		// int firstSurf = 0;
		// int zone = 0;
		// std::cout << "looking at surface distribution and zone ownership (are they optimal?)" << std::endl;
		// for(auto ts: LoadBalanceVector){
		// 	std::cout << "thread: " << thread++ << " surfaces: " << ts <<
		// 		" first surf: " << firstSurf << " last surf: " << (firstSurf + ts) - 1 << std::endl;
		// 	firstSurf += ts;
		// }
		// for(auto zone: ZoneInfo){
		// 	std::cout << "zone : " << zone() << " owner: " << zone.owner
		// 						<< " firstSurf: " << (*zone.surfBegin)()
		// 						<< " lastSurf: " << (*zone.surfEnd)() << std::endl;
		// }
		// for ( ZoneNum = 1; ZoneNum <= Numofzones; ++ZoneNum ) {
		// 	CalcScriptF( ZoneInfo[ ZoneNum - 1 ]); //this is from the commented out version above in this function -- it was only used for a report (also commented out above in this func)
		// }

		if ( ErrorsFound ) {
			ShowFatalError( "InitInteriorRadExchange: Errors found during initialization of radiant exchange.  Program terminated." );
		}
		timer.stopTimer();
	}

	void
	GetInputViewFactors(
		std::string const & ZoneName, // Needed to check for user input view factors.
		int const N, // NUMBER OF SURFACES
		FArray2A< Real64 > F, // USER INPUT DIRECT VIEW FACTOR MATRIX (N X N)
		FArray1A_int const SPtr, // pointer to actual surface number
		bool & NoUserInputF, // Flag signifying no input F's for this
		bool & ErrorsFound // True when errors are found in number of fields vs max args
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Curt Pedersen
		//       DATE WRITTEN   September 2005
		//       MODIFIED       Linda Lawrie;September 2010
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gets the user view factor info.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectItemNum;
		using General::TrimSigDigits;

		// Argument array dimensioning
		F.dim( N, N );
		SPtr.dim( N );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  INTEGER   :: NumZonesWithUserF
		int UserFZoneIndex;
		int NumAlphas;
		int NumNums;
		int IOStat;
		int index;
		int inx1;
		int inx2;
		//unused  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ZoneSurfaceNames

		NoUserInputF = true;
		UserFZoneIndex = GetObjectItemNum( "ZoneProperty:UserViewFactors", ZoneName );

		if ( UserFZoneIndex > 0 ) {
			NoUserInputF = false;

			GetObjectItem( "ZoneProperty:UserViewFactors", UserFZoneIndex, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			if ( NumNums < 3 * pow_2( N ) ) {
				ShowSevereError( "GetInputViewFactors: " + cCurrentModuleObject + "=\"" + ZoneName + "\", not enough values." );
				ShowContinueError( "...Number of input values [" + TrimSigDigits( NumNums ) + "] is less than the required number=[" + TrimSigDigits( 3 * pow_2( N ) ) + "]." );
				ErrorsFound = true;
				NumNums = 0;
			}
			F = 0.0;
			for ( index = 1; index <= NumNums; index += 3 ) {
				inx1 = rNumericArgs( index );
				inx2 = rNumericArgs( index + 1 );
				F( inx1, inx2 ) = rNumericArgs( index + 2 );
			}
		}

	}

	void
	GetInputViewFactorsbyName(
		std::string const & ZoneName, // Needed to check for user input view factors.
		int const N, // NUMBER OF SURFACES
		FArray2A< Real64 > F, // USER INPUT DIRECT VIEW FACTOR MATRIX (N X N)
		FArray1A_int const SPtr, // pointer to actual surface number
		bool & NoUserInputF, // Flag signifying no input F's for this
		bool & ErrorsFound // True when errors are found in number of fields vs max args
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Curt Pedersen
		//       DATE WRITTEN   September 2005
		//       MODIFIED       Linda Lawrie;September 2010
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gets the user view factor info.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectItemNum;
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Argument array dimensioning
		F.dim( N, N );
		SPtr.dim( N );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int UserFZoneIndex;
		int NumAlphas;
		int NumNums;
		int IOStat;
		int index;
		int numinx1;
		int inx1;
		int inx2;
		FArray1D_string ZoneSurfaceNames;

		NoUserInputF = true;
		UserFZoneIndex = GetObjectItemNum( "ZoneProperty:UserViewFactors:bySurfaceName", ZoneName );

		if ( UserFZoneIndex > 0 ) {
			ZoneSurfaceNames.allocate( N );
			for ( index = 1; index <= N; ++index ) {
				ZoneSurfaceNames( index ) = Surface( SPtr( index ) ).Name;
			}
			NoUserInputF = false;

			GetObjectItem( "ZoneProperty:UserViewFactors:bySurfaceName", UserFZoneIndex, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			if ( NumNums < pow_2( N ) ) {
				ShowSevereError( "GetInputViewFactors: " + cCurrentModuleObject + "=\"" + ZoneName + "\", not enough values." );
				ShowContinueError( "...Number of input values [" + TrimSigDigits( NumNums ) + "] is less than the required number=[" + TrimSigDigits( pow_2( N ) ) + "]." );
				ErrorsFound = true;
				NumNums = 0; // cancel getting any coordinates
			}
			F = 0.0;
			numinx1 = 0;

			for ( index = 2; index <= NumAlphas; index += 2 ) {
				inx1 = FindItemInList( cAlphaArgs( index ), ZoneSurfaceNames, N );
				inx2 = FindItemInList( cAlphaArgs( index + 1 ), ZoneSurfaceNames, N );
				if ( inx1 == 0 ) {
					ShowSevereError( "GetInputViewFactors: " + cCurrentModuleObject + "=\"" + ZoneName + "\", invalid surface name." );
					ShowContinueError( "...Surface name=\"" + cAlphaArgs( index ) + "\", not in this zone." );
					ErrorsFound = true;
				}
				if ( inx2 == 0 ) {
					ShowSevereError( "GetInputViewFactors: " + cCurrentModuleObject + "=\"" + ZoneName + "\", invalid surface name." );
					ShowContinueError( "...Surface name=\"" + cAlphaArgs( index + 2 ) + "\", not in this zone." );
					ErrorsFound = true;
				}
				++numinx1;
				if ( inx1 > 0 && inx2 > 0 ) F( inx1, inx2 ) = rNumericArgs( numinx1 );
			}
			ZoneSurfaceNames.deallocate();
		}

	}

	void
	CalcApproximateViewFactors(
		int const N, // NUMBER OF SURFACES
		FArray1A< Real64 > const A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		FArray1A< Real64 > const Azimuth, // Facing angle of the surface (in degrees)
		FArray1A< Real64 > const Tilt, // Tilt angle of the surface (in degrees)
		FArray2A< Real64 > F, // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
		FArray1A_int const SPtr // pointer to REAL(r64) surface number (for error message)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Curt Pedersen
		//       DATE WRITTEN   July 2000
		//       MODIFIED       March 2001 (RKS) to disallow surfaces facing the same direction to interact radiatively
		//                      May 2002 (COP) to include INTMASS, FLOOR, ROOF and CEILING.
		//       RE-ENGINEERED  September 2000 (RKS for EnergyPlus)

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine approximates view factors using an area weighting.
		// This is improved by one degree by not allowing surfaces facing the same
		// direction to "see" each other.

		// METHODOLOGY EMPLOYED:
		// Each surface sees some area of other surfaces within the zone.  The view
		// factors from the surface to the other seen surfaces are defined by their
		// area over the summed area of seen surfaces.  Surfaces facing the same angle
		// are assumed to not be able to see each other.
		//  Modified May 2002 to cover poorly defined surface orientation.  Now all thermal masses, roofs and
		//  ceilings are "seen" by other surfaces. Floors are seen by all other surfaces, but
		//  not by other floors.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		A.dim( N );
		Azimuth.dim( N );
		Tilt.dim( N );
		F.dim( N, N );
		SPtr.dim( N );

		// Locals
		// SUBROUTINE ARGUMENTS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SameAngleLimit( 10.0 ); // If the difference in the azimuth angles are above this value (degrees),
		// then the surfaces are assumed to be facing different directions.

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int i; // DO loop counters for surfaces in the zone
		int j;
		FArray1D< Real64 > ZoneArea; // Sum of the area of all zone surfaces seen

		// FLOW:
		// Calculate the sum of the areas seen by all zone surfaces
		ZoneArea.allocate( N );
		ZoneArea = 0.0;
		for ( i = 1; i <= N; ++i ) {
			for ( j = 1; j <= N; ++j ) {
				// Assumption is that a surface cannot see itself or any other surface
				// that is facing the same direction (has the same azimuth)
				//  Modified to use Class of surface to permit INTMASS to be seen by all surfaces,
				//  FLOOR to be seen by all except other floors, and ROOF and CEILING by all.
				//  Skip same surface
				if ( i == j ) continue;
				//  Include INTMASS, FLOOR(for others), CEILING, ROOF  and different facing surfaces.
				//  Roofs/ceilings always see floors
				if ( ( Surface( SPtr( j ) ).Class == SurfaceClass_IntMass ) || ( Surface( SPtr( j ) ).Class == SurfaceClass_Floor ) || ( Surface( SPtr( j ) ).Class == SurfaceClass_Roof && Surface( SPtr( i ) ).Class == SurfaceClass_Floor ) || ( ( std::abs( Azimuth( i ) - Azimuth( j ) ) > SameAngleLimit ) || ( std::abs( Tilt( i ) - Tilt( j ) ) > SameAngleLimit ) ) ) { // Everything sees internal mass surfaces | Everything except other floors sees floors

					ZoneArea( i ) += A( j );

				}
			}
			if ( ZoneArea( i ) <= 0.0 ) {
				ShowWarningError( "CalcApproximateViewFactors: Zero area for all other zone surfaces." );
				ShowContinueError( "Happens for Surface=\"" + Surface( SPtr( i ) ).Name + "\" in Zone=" + Zone( Surface( SPtr( i ) ).Zone ).Name );
			}
		}

		// Set up the approximate view factors.  First these are initialized to all zero.
		// This will clear out any junk leftover from whenever.  Then, for each zone
		// surface, set the view factor from that surface to other surfaces as the
		// area of the other surface divided by the sum of the area of all zone surfaces
		// that the original surface can actually see (calculated above).  This will
		// allow that the sum of all view factors from the original surface to all other
		// surfaces will equal unity.  F(I,J)=0 if I=J or if the surfaces face the same
		// direction.
		//  Modified to use Class of surface to permit INTMASS to be seen by all surfaces,
		//  FLOOR to be seen by all except other floors, and ROOF and CEILING by all.
		// The second IF statement is intended to avoid a divide by zero if
		// there are no other surfaces in the zone that can be seen.
		F = 0.0;
		for ( i = 1; i <= N; ++i ) {
			for ( j = 1; j <= N; ++j ) {

				//  Skip same surface

				if ( i == j ) continue;
				//  Include INTMASS, FLOOR(for others), CEILING/ROOF  and different facing surfaces.
				if ( ( Surface( SPtr( j ) ).Class == SurfaceClass_IntMass ) || ( Surface( SPtr( j ) ).Class == SurfaceClass_Floor ) || ( Surface( SPtr( j ) ).Class == SurfaceClass_Roof ) || ( ( std::abs( Azimuth( i ) - Azimuth( j ) ) > SameAngleLimit ) || ( std::abs( Tilt( i ) - Tilt( j ) ) > SameAngleLimit ) ) ) {
					if ( ZoneArea( i ) > 0.0 ) F( i, j ) = A( j ) / ( ZoneArea( i ) );
				}

			}
		}

		ZoneArea.deallocate();

	}

	void
	FixViewFactors(
		int const N, // NUMBER OF SURFACES
		FArray1A< Real64 > const A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		FArray2A< Real64 > F, // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
		int const ZoneNum, // Zone number being fixe
		Real64 & OriginalCheckValue, // check of SUM(F) - N
		Real64 & FixedCheckValue, // check after fixed of SUM(F) - N
		Real64 & FinalCheckValue, // the one to go with
		int & NumIterations, // number of iterations to fixed
		Real64 & RowSum // RowSum of Fixed
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Curt Pedersen
		//       DATE WRITTEN   July 2000
		//       MODIFIED       September 2000 (RKS for EnergyPlus)
		//                      April 2005,COP added capability to handle a
		//                      surface larger than sum of all others (nonenclosure)
		//                      by using a Fii view factor for that surface. Process is
		//                      now much more robust and stable.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine fixes approximate view factors and enforces reciprocity
		// and completeness.

		// METHODOLOGY EMPLOYED:
		// A(i)*F(i,j)=A(j)*F(j,i); F(i,i)=0.; SUM(F(i,j)=1.0, j=1,N)
		// Subroutine takes approximate view factors and enforces reciprocity by
		// averaging AiFij and AjFji.  Then it determines a set of row coefficients
		// which can be multipled by each AF product to force the sum of AiFij for
		// each row to equal Ai, and applies them. Completeness is checked, and if
		// not satisfied, the AF averaging and row modifications are repeated until
		// completeness is within a preselected small deviation from 1.0
		// The routine also checks the number of surfaces and if N<=3, just enforces reciprocity.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Argument array dimensioning
		A.dim( N );
		F.dim( N, N );

		// Locals
		// SUBROUTINE ARGUMENTS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const PrimaryConvergence( 0.001 );
		Real64 const DifferenceConvergence( 0.00001 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		FArray2D< Real64 > AF; // = (AREA * DIRECT VIEW FACTOR) MATRIX
		FArray2D< Real64 > AFTranspose;
		FArray2D< Real64 > AFAverage;
		FArray2D< Real64 > FixedAF;
		FArray2D< Real64 > FixedF; // CORRECTED MATRIX OF VIEW FACTORS (N X N)
		FArray2D< Real64 > FixedAFTranspose;
		FArray1D< Real64 > RowCoefficient;
		Real64 LargestArea;
		Real64 ConvrgNew;
		Real64 ConvrgOld;
		Real64 Accelerator; // RowCoefficient multipler to accelerate convergence
		Real64 CheckConvergeTolerance; // check value for actual warning

		bool Converged;
		int i;
		int j;
		static int LargestSurf( 0 );

		// FLOW:
		OriginalCheckValue = std::abs( sum( F ) - N );

		//  Allocate and zero arrays
		AF.allocate( N, N );
		AFTranspose.allocate( N, N );
		AFAverage.allocate( N, N );
		FixedAF.allocate( N, N );
		FixedAFTranspose.allocate( N, N );

		AF = 0.0;
		AFTranspose = 0.0;
		FixedAF = 0.0;
		Accelerator = 1.0;
		ConvrgOld = 10.0;
		LargestArea = maxval( A );

		FixedAF = F; // store for largest area check

		//  Check for Strange Geometry
		if ( LargestArea > ( sum( A ) - LargestArea ) ) {
			for ( i = 1; i <= N; ++i ) {
				if ( LargestArea != A( i ) ) continue;
				LargestSurf = i;
				break;
			}
			FixedAF( LargestSurf, LargestSurf ) = min( 0.9, 1.2 * LargestArea / sum( A ) ); // Give self view to big surface
		}

		//  Set up AF matrix.
		for ( i = 1; i <= N; ++i ) {
			for ( j = 1; j <= N; ++j ) {
				AF( i, j ) = FixedAF( i, j ) * A( i );
			}
		}

		//  Enforce reciprocity by averaging AiFij and AjFji
		AFTranspose = transpose( AF );
		AFAverage = 0.5 * ( AF + AFTranspose );

		FixedAF = AFAverage; //Initialize Fixed Matrix

		AF.deallocate();
		AFTranspose.deallocate();
		AFAverage.deallocate();

		FixedF.allocate( N, N );
		RowCoefficient.allocate( N );
		FixedF = 0.0;
		RowCoefficient = 1.0;

		NumIterations = 0;
		RowSum = 0.0;
		//  Check for physically unreasonable enclosures.

		if ( N <= 3 ) {
			for ( i = 1; i <= N; ++i ) {
				for ( j = 1; j <= N; ++j ) {
					FixedF( i, j ) = FixedAF( i, j ) / A( i );
				}
			}

			ShowWarningError( "Surfaces in Zone=\"" + Zone( ZoneNum ).Name + "\" do not define an enclosure." );
			ShowContinueError( "Number of surfaces <= 3, view factors are set to force reciprocity." );

			F = FixedF;
			FixedCheckValue = std::abs( sum( FixedF ) - N );
			FinalCheckValue = FixedCheckValue;
			RowSum = 0.0;
			for ( i = 1; i <= N; ++i ) {
				RowSum += sum( FixedF( i, _ ) );
			}
			Zone( ZoneNum ).EnforcedReciprocity = true;
			FixedAF.deallocate();
			FixedF.deallocate();
			FixedAFTranspose.deallocate();
			RowCoefficient.deallocate();
			return; // Do not iterate, stop with reciprocity satisfied.

		} //  N <= 3 Case

		//  Regular fix cases
		Converged = false;
		while ( ! Converged ) {
			++NumIterations;
			for ( i = 1; i <= N; ++i ) {
				// Determine row coefficients which will enforce closure.
				if ( std::abs( sum( FixedAF( i, {1,N} ) ) ) > 1.0e-10 ) {
					RowCoefficient( i ) = A( i ) / sum( FixedAF( i, {1,N} ) );
				} else {
					RowCoefficient( i ) = 1.0;
				}
				FixedAF( i, {1,N} ) *= RowCoefficient( i );
			}

			//  Enforce reciprocity by averaging AiFij and AjFji
			FixedAFTranspose = transpose( FixedAF );
			FixedAF = 0.5 * ( FixedAFTranspose + FixedAF );

			//  Form FixedF matrix
			for ( i = 1; i <= N; ++i ) {
				for ( j = 1; j <= N; ++j ) {
					FixedF( i, j ) = FixedAF( i, j ) / A( i );
					if ( std::abs( FixedF( i, j ) ) < 1.e-10 ) {
						FixedF( i, j ) = 0.0;
						FixedAF( i, j ) = 0.0;
					}
				}
			}

			ConvrgNew = std::abs( sum( FixedF ) - N );
			if ( std::abs( ConvrgOld - ConvrgNew ) < DifferenceConvergence || ConvrgNew <= PrimaryConvergence ) { //  Change in sum of Fs must be small.
				Converged = true;
			}
			ConvrgOld = ConvrgNew;
			if ( NumIterations > 400 ) { //  If everything goes bad,enforce reciprocity and go home.
				//  Enforce reciprocity by averaging AiFij and AjFji
				FixedAFTranspose = transpose( FixedAF );
				FixedAF = 0.5 * ( FixedAFTranspose + FixedAF );

				//  Form FixedF matrix
				for ( i = 1; i <= N; ++i ) {
					for ( j = 1; j <= N; ++j ) {
						FixedF( i, j ) = FixedAF( i, j ) / A( i );
					}
				}
				CheckConvergeTolerance = std::abs( sum( FixedF ) - N );
				if ( CheckConvergeTolerance > 0.005 ) {
					ShowWarningError( "FixViewFactors: View factors not complete. Check for " "bad surface descriptions or unenclosed zone=\"" + Zone( ZoneNum ).Name + "\"." );
					ShowContinueError( "Enforced reciprocity has tolerance (ideal is 0)=[" + RoundSigDigits( CheckConvergeTolerance, 6 ) + "], Row Sum (ideal is " + RoundSigDigits( N ) + ")=[" + RoundSigDigits( RowSum, 2 ) + "]." );
					ShowContinueError( "If zone is unusual, or tolerance is on the order of 0.001, view factors are probably OK." );
				}
				FixedCheckValue = std::abs( sum( FixedF ) - N );
				FinalCheckValue = FixedCheckValue;
				if ( std::abs( FixedCheckValue ) < std::abs( OriginalCheckValue ) ) {
					F = FixedF;
					FinalCheckValue = FixedCheckValue;
				}
				RowSum = 0.0;
				for ( i = 1; i <= N; ++i ) {
					RowSum += sum( FixedF( i, _ ) );
				}
				FixedAF.deallocate();
				FixedF.deallocate();
				FixedAFTranspose.deallocate();
				RowCoefficient.deallocate();
				return;
			}
		}
		FixedCheckValue = ConvrgNew;
		if ( FixedCheckValue < OriginalCheckValue ) {
			F = FixedF;
			FinalCheckValue = FixedCheckValue;
		} else {
			FinalCheckValue = OriginalCheckValue;
			RowSum = 0.0;
			for ( i = 1; i <= N; ++i ) {
				RowSum += sum( FixedF( i, _ ) );
			}
			if ( std::abs( RowSum - N ) < PrimaryConvergence ) {
				F = FixedF;
				FinalCheckValue = FixedCheckValue;
			} else {
				ShowWarningError( "FixViewFactors: View factors not complete. Check for " "bad surface descriptions or unenclosed zone=\"" + Zone( ZoneNum ).Name + "\"." );
			}
		}

		FixedAF.deallocate();
		FixedF.deallocate();
		FixedAFTranspose.deallocate();
		RowCoefficient.deallocate();

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

} // HeatBalanceIntRadExchange

} // EnergyPlus
