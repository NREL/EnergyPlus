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

#ifndef SurfaceGroundHeatExchanger_hh_INCLUDED
#define SurfaceGroundHeatExchanger_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <PlantComponent.hh>

namespace EnergyPlus {

namespace SurfaceGroundHeatExchanger {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern Real64 const SmallNum; // Very small number to avoid div0 errors
	extern Real64 const StefBoltzmann; // Stefan-Boltzmann constant
	extern Real64 const SurfaceHXHeight; // Surface Height above ground -- used in height dependent calcs.

	extern int const SurfCond_Ground;
	extern int const SurfCond_Exposed;

namespace loc {
	extern int const MaxCTFTerms; // Maximum number of CTF terms allowed to still allow stability //Note Duplicate of DataHeatBalance::MaxCTFTerms to avoid static initialization order bug: Keep them in sync
} // loc

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	// utility variables initialized once
	//extern int NumOfSurfaceGHEs; // Number of surface GHE ground heat exchangers
	//extern bool NoSurfaceGroundTempObjWarning; // This will cause a warning to be issued if no "surface" ground
	//// temperature object was input.
	//// Utility variables - initialized for each instance of a surface GHE
	//extern int InletNodeNum; // inlet node number
	//extern int OutletNodeNum; // oulet node number
	//extern int ConstructionNum; // construction index number
	//extern int TopRoughness; // roughness of top layer
	//extern int BtmRoughness; // roughness of bottom layer
	extern Real64 nsvInletTemp; // water inlet temperature
	extern Real64 nsvOutletTemp; // water outlet temperature
	extern Real64 FlowRate; // water mass flow rate
	extern Real64 TopSurfTemp; // Top  surface temperature
	extern Real64 BtmSurfTemp; // Bottom  surface temperature
	extern Real64 TopSurfFlux; // Top  surface heat flux
	extern Real64 BtmSurfFlux; // Bottom  surface heat flux
	extern Real64 SourceFlux; // total heat transfer rate, Watts
	extern Real64 SourceTemp; // total heat transfer rate, Watts
	extern Real64 TopThermAbs; // Thermal absortivity of top layer
	extern Real64 BtmThermAbs; // Thermal absortivity of bottom layer
	extern Real64 TopSolarAbs; // Solar absortivity of top layer
	extern Array1D_bool CheckEquipName;

	// weather data records updated every zone time step
	extern Real64 PastBeamSolarRad; // Previous beam normal solar irradiance
	extern Real64 PastSolarDirCosVert; // Previous vertical component of solar normal
	extern Real64 PastDifSolarRad; // Previous sky diffuse solar horizontal irradiance
	extern Real64 PastGroundTemp; // Previous ground temperature
	extern bool PastIsRain; // Previous Surfaces are wet for this time interval
	extern bool PastIsSnow; // Previous Snow on the ground for this time interval
	extern Real64 PastOutBaroPress; // Previous outdoor air barometric pressure
	extern Real64 PastOutDryBulbTemp; // Previous outdoor air dry bulb temperature
	extern Real64 PastOutHumRat; // Previous outdoor air humidity ratio
	extern Real64 PastOutAirDensity; // Previous outdoor air density
	extern Real64 PastOutWetBulbTemp; // Previous outdoor air wet bulb temperature
	extern Real64 PastOutDewPointTemp; // Previous outdoor dewpoint temperature
	extern Real64 PastSkyTemp; // Previous sky temperature
	extern Real64 PastWindSpeed; // Previous outdoor air wind speed
	extern Real64 PastCloudFraction; // Previous Fraction of sky covered by clouds

	// get input flag
	extern bool GetInputFlag;

	// time keeping variables used for keeping track of average flux over each time step
	extern Array1D< Real64 > QRadSysSrcAvg; // Average source over the time step
	extern Array1D< Real64 > LastSysTimeElapsed; // record of system time
	extern Array1D< Real64 > LastTimeStepSys; // previous time step size

	// SUBROUTINE SPECIFICATIONS FOR MODULE PlantSurfaceGroundHeatExchangers

	// Types

	struct SurfaceGroundHeatExchangerData : PlantComponent
	{

		virtual
		~SurfaceGroundHeatExchangerData()
		{}

		// Members
		// Input data
		std::string Name; // name of surface GHE
		std::string ConstructionName; // name of the associated construction
		std::string InletNode; // surface GHE inlet fluid node
		std::string OutletNode; // surface GHE outlet fluid node
		Real64 DesignMassFlowRate;
		Real64 TubeDiameter; // hydronic tube inside diameter
		Real64 TubeSpacing; // tube spacing
		Real64 SurfaceLength; // active length of surface GHE
		Real64 SurfaceWidth; // active width of surface GHE
		Real64 TopThermAbs; // Thermal absortivity of top layer
		Real64 TopSolarAbs; // solar absortivity of top layer
		Real64 BtmThermAbs; // Thermal absortivity of bottom layer
		int LowerSurfCond; // Type of lower surf. boundary condition
		int TubeCircuits; // number of circuits in total
		int ConstructionNum; // construction index number
		int InletNodeNum; // inlet node number
		int OutletNodeNum; // oulet node number
		int TopRoughness; // roughness of top layer
		int BtmRoughness; // roughness of bottom layer
		int FrozenErrIndex1; // recurring error index
		int FrozenErrIndex2; // recurring error index
		int ConvErrIndex1; // recurring error index
		int ConvErrIndex2; // recurring error index
		int ConvErrIndex3; // recurring error index
		//loop topology variables
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;

		// QTF Constants
		Real64 TsrcConstCoef;
		Real64 TsrcVarCoef;
		Real64 QbtmConstCoef;
		Real64 QbtmVarCoef;
		Real64 QtopConstCoef;
		Real64 QtopVarCoef;
		// conventional CTF terms
		int NumCTFTerms; // number of terms for surface
		// could be allocated rather than hard dimensioning.
		Array1D< Real64 > CTFin; // surf flux in ctf - X
		Array1D< Real64 > CTFout; // surf flux in ctf - Z
		Array1D< Real64 > CTFcross; // surf flux in ctf - Y
		Array1D< Real64 > CTFflux; // surf flux in ctf - F
		// QTF coefficients
		Array1D< Real64 > CTFSourceIn; // surf flux in ctf - Wi
		Array1D< Real64 > CTFSourceOut; // surf flux out ctf - Wo
		Array1D< Real64 > CTFTSourceOut; // surf flux in qtf - x
		Array1D< Real64 > CTFTSourceIn; // surf flux in qtf - y
		Array1D< Real64 > CTFTSourceQ; // surf flux in qtf - f
		// History data
		Array1D< Real64 > TbtmHistory;
		Array1D< Real64 > TtopHistory;
		Array1D< Real64 > TsrcHistory;
		Array1D< Real64 > QbtmHistory;
		Array1D< Real64 > QtopHistory;
		Array1D< Real64 > QsrcHistory;
		Real64 QSrc;
		Real64 QSrcAvg;
		Real64 LastQSrc;
		Real64 LastSysTimeElapsed;
		Real64 LastTimeStepSys;

		// Report data
		Real64 InletTemp; // water inlet temperature
		Real64 OutletTemp; // water outlet temperature
		Real64 MassFlowRate; // water mass flow rate
		Real64 TopSurfaceTemp; // Top surface temperature
		Real64 BtmSurfaceTemp; // Bottom  surface temperature
		Real64 TopSurfaceFlux; // Top  surface heat flux
		Real64 BtmSurfaceFlux; // Bottom  surface heat flux
		Real64 HeatTransferRate; // total fluid heat transfer rate, Watts
		Real64 SurfHeatTransferRate; // total surface heat transfer rate, Watts
		Real64 Energy; // cumulative energy, Joules
		Real64 SurfEnergy; // cumulative energy, Joules
		Real64 SourceTemp; // Source temperature

		bool MyFlag;
		bool InitQTF;
		bool MyEnvrnFlag;
		Real64 SurfaceArea; // surface GHE surface area

		// Default Constructor
		SurfaceGroundHeatExchangerData() :
			DesignMassFlowRate( 0.0 ),
			TubeDiameter( 0.0 ),
			TubeSpacing( 0.0 ),
			SurfaceLength( 0.0 ),
			SurfaceWidth( 0.0 ),
			TopThermAbs( 0.0 ),
			TopSolarAbs( 0.0 ),
			BtmThermAbs( 0.0 ),
			LowerSurfCond( 0 ),
			TubeCircuits( 0 ),
			ConstructionNum( 0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			TopRoughness( 0 ),
			BtmRoughness( 0 ),
			FrozenErrIndex1( 0 ),
			FrozenErrIndex2( 0 ),
			ConvErrIndex1( 0 ),
			ConvErrIndex2( 0 ),
			ConvErrIndex3( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),

			TsrcConstCoef( 0.0 ),
			TsrcVarCoef( 0.0 ),
			QbtmConstCoef( 0.0 ),
			QbtmVarCoef( 0.0 ),
			QtopConstCoef( 0.0 ),
			QtopVarCoef( 0.0 ),
			NumCTFTerms( 0 ),
			CTFin( {0,loc::MaxCTFTerms-1}, 0.0 ),
			CTFout( {0,loc::MaxCTFTerms-1}, 0.0 ),
			CTFcross( {0,loc::MaxCTFTerms-1}, 0.0 ),
			CTFflux( {0,loc::MaxCTFTerms-1}, 0.0 ),
			CTFSourceIn( {0,loc::MaxCTFTerms-1}, 0.0 ),
			CTFSourceOut( {0,loc::MaxCTFTerms-1}, 0.0 ),
			CTFTSourceOut( {0,loc::MaxCTFTerms-1}, 0.0 ),
			CTFTSourceIn( {0,loc::MaxCTFTerms-1}, 0.0 ),
			CTFTSourceQ( {0,loc::MaxCTFTerms-1}, 0.0 ),
			TbtmHistory( {0,loc::MaxCTFTerms-1}, 0.0 ),
			TtopHistory( {0,loc::MaxCTFTerms-1}, 0.0 ),
			TsrcHistory( {0,loc::MaxCTFTerms-1}, 0.0 ),
			QbtmHistory( {0,loc::MaxCTFTerms-1}, 0.0 ),
			QtopHistory( {0,loc::MaxCTFTerms-1}, 0.0 ),
			QsrcHistory( {0,loc::MaxCTFTerms-1}, 0.0 ),
			QSrc( 0.0 ),
			QSrcAvg( 0.0 ),
			LastQSrc( 0.0 ),
			LastSysTimeElapsed( 0.0 ),
			LastTimeStepSys( 0.0 ),

			InletTemp( 0.0 ),
			OutletTemp( 0.0 ),
			MassFlowRate( 0.0 ),
			TopSurfaceTemp( 0.0 ),
			BtmSurfaceTemp( 0.0 ),
			TopSurfaceFlux( 0.0 ),
			BtmSurfaceFlux( 0.0 ),
			HeatTransferRate( 0.0 ),
			SurfHeatTransferRate( 0.0 ),
			Energy( 0.0 ),
			SurfEnergy( 0.0 ),
			SourceTemp( 0.0 ),

			MyFlag( true ),
			InitQTF( true ),
			MyEnvrnFlag( true ),
			SurfaceArea( 0.0 )
		{}

		void simulate( const PlantLocation & calledFromLocation, bool const FirstHVACIteration, Real64 & CurLoad, bool const RunFlag ) override;

		static PlantComponent * factory( int const objectType, std::string const objectName );


		void
		InitSurfaceGroundHeatExchanger();

		//==============================================================================

		void
		CalcSurfaceGroundHeatExchanger(
			bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
		);

		//==============================================================================

		void
		CalcBottomFluxCoefficents(
			Real64 const Tbottom, // current bottom (lower) surface temperature
			Real64 const Ttop // current top (upper) surface temperature
		);

		//==============================================================================

		void
		CalcTopFluxCoefficents(
			Real64 const Tbottom, // current bottom (lower) surface temperature
			Real64 const Ttop // current top (upper) surface temperature
		);

		//==============================================================================

		void
		CalcSourceTempCoefficents(
			Real64 const Tbottom, // current bottom (lower) surface temperature
			Real64 const Ttop // current top (upper) surface temperature
		);

		//==============================================================================

		Real64
		CalcSourceFlux(); // component number

		//==============================================================================

		void
		UpdateHistories(
			Real64 const TopFlux, // current top (top) surface flux
			Real64 const BottomFlux, // current bottom (bottom) surface flux
			Real64 const SourceFlux, // current source surface flux
			Real64 const SourceTemp // current source temperature
		);

		//==============================================================================

		Real64
		CalcHXEffectTerm(
			Real64 const Temperature, // Temperature of water entering the surface, in C
			Real64 const WaterMassFlow // Mass flow rate, in kg/s
		);

		//==============================================================================

		void
		CalcTopSurfTemp(
			Real64 const FluxTop, // top surface flux
			Real64 & TempTop, // top surface temperature
			Real64 const ThisDryBulb, // dry bulb temperature
			Real64 const ThisWetBulb, // wet bulb temperature
			Real64 const ThisSkyTemp, // sky temperature
			Real64 const ThisBeamSolarRad, // beam solar radiation
			Real64 const ThisDifSolarRad, // diffuse solar radiation
			Real64 const ThisSolarDirCosVert, // vertical component of solar normal
			Real64 const ThisWindSpeed, // wind speed
			bool const ThisIsRain, // rain flag
			bool const ThisIsSnow // snow flag
		);

		//==============================================================================

		void
		CalcBottomSurfTemp(
			Real64 const FluxBtm, // bottom surface flux
			Real64 & TempBtm, // bottom surface temperature
			Real64 const ThisDryBulb, // dry bulb temperature
			Real64 const ThisWindSpeed, // wind speed
			Real64 const ThisGroundTemp // ground temperature
		);

		//==============================================================================

		void
		UpdateSurfaceGroundHeatExchngr(); // Index for the surface

		//==============================================================================

		void
		ReportSurfaceGroundHeatExchngr(); // Index for the surface under consideration


	};

	// Object Data
	extern Array1D< SurfaceGroundHeatExchangerData > SurfaceGHE;

	void
	GetSurfaceGroundHeatExchanger();

	//==============================================================================

} // SurfaceGroundHeatExchanger

} // EnergyPlus

#endif
