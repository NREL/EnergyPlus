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

// EnergyPlus Headers
#include <DataRoomAirModel.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataRoomAirModel {

	// MODULE INFORMATION:
	//       AUTHOR         Weixiu Kong
	//       DATE WRITTEN   March 2003
	//       MODIFIED       July 2003, CC
	//                      Jan 2004, CC
	//                      Aug 2005, BG -- added structures for user-defined patterns
	//                      June 2008, BG -- revised for system time step history terms
	//                      Aug 2013, Sam Brunswick -- added structures for improved RoomAirModelCrossVent
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module contain global variables needed in air models

	// USE STATEMENTS:                       ! UCSD
	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// module should be available to other modules and routines.  Thus,
	// all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS
	std::string const cUserDefinedControlObject( "RoomAir:TemperaturePattern:UserDefined" );
	std::string const cTempPatternConstGradientObject( "RoomAir:TemperaturePattern:ConstantGradient" );
	std::string const cTempPatternTwoGradientObject( "RoomAir:TemperaturePattern:TwoGradient" );
	std::string const cTempPatternNDHeightObject( "RoomAir:TemperaturePattern:NondimensionalHeight" );
	std::string const cTempPatternSurfMapObject( "RoomAir:TemperaturePattern:SurfaceMapping" );

	// Parameters to indicate room air model selected
	int const RoomAirModel_UserDefined( 1 ); // user defined patterns
	int const RoomAirModel_Mixing( 2 ); // mixing air model
	int const RoomAirModel_Mundt( 3 ); // Mundt nodal model
	int const RoomAirModel_UCSDDV( 4 ); // UCSD Displacement Ventilation model
	int const RoomAirModel_UCSDCV( 5 ); // UCSD-CV
	int const RoomAirModel_UCSDUFI( 6 ); // UCSD UFAD interior zone model
	int const RoomAirModel_UCSDUFE( 7 ); // UCSD UFAD interior zone model
	int const RoomAirModel_AirflowNetwork( 8 ); // Room Air model using AirflowNetwork
	Array1D_string const ChAirModel( {0,8}, { "*Invalid*", "UserDefined", "Mixing", "Mundt", "UCSD_DV", "UCSD_CV", "UCSD_UFI", "UCSD_UFE", "AirflowNetwork" } );

	// Parameters to indicate air temperature coupling scheme
	int const DirectCoupling( 1 ); // direct coupling scheme
	int const IndirectCoupling( 2 ); // indirect coupling scheme

	// Parameters to indicate type of air node, which is dependent on air models
	int const InletAirNode( 0 ); // air node at inlet (for Mundt and Rees&Haves Models)
	int const FloorAirNode( 1 ); // air node at floor (for Mundt and Rees&Haves Models)
	int const ControlAirNode( 2 ); // air node at control point (for Mundt Model)
	int const CeilingAirNode( 3 ); // air node at ceiling (for Mundt Model)
	int const MundtRoomAirNode( 4 ); // air node for vertical walls (for Mundt Model)
	int const ReturnAirNode( 10 ); // air node for return (for Mundt and Rees&Haves Models)
	int const AirflowNetworkRoomAirNode( 11 ); // air node for airflow network based room air model
	int const PlumeAirNode1( 2 ); // air node for plume load (for Rees&Haves Model)
	int const PlumeAirNode2( 3 ); // air node for plume load (for Rees&Haves Model)
	int const PlumeAirNode3( 4 ); // air node for plume load (for Rees&Haves Model)
	int const PlumeAirNode4( 5 ); // air node for plume load (for Rees&Haves Model)
	int const RoomAirNode1( 6 ); // air node for vertical walls (for Rees&Haves Model)
	int const RoomAirNode2( 7 ); // air node for vertical walls (for Rees&Haves Model)
	int const RoomAirNode3( 8 ); // air node for vertical walls (for Rees&Haves Model)
	int const RoomAirNode4( 9 ); // air node for vertical walls (for Rees&Haves Model)

	// user-defined pattern two gradient interplotation modes
	int const OutdoorDryBulbMode( 21 ); // by outdoor air bulb.
	int const SensibleCoolingMode( 22 ); // by sensible cooling load
	int const SensibleHeatingMode( 23 ); // by sensible heating load
	int const ZoneAirTempMode( 24 ); // by zone air temperature
	int const DeltaOutdoorZone( 25 ); // by difference between zone and outdoor

	// user defined temperature pattern types
	int const ConstGradTempPattern( 31 ); // constant gradient in vertical direction
	int const TwoGradInterpPattern( 32 ); // two gradient interpolation
	int const NonDimenHeightPattern( 33 ); // non-dimensionalized height
	int const SurfMapTempPattern( 34 ); // arbitrary surface mappings

	// Parameters to indicate type of control for the UCSD UFAD interior zone model
	// INTEGER, PARAMETER :: ConsFlow          = 1     ! constant supply air flow
	// INTEGER, PARAMETER :: VarFlowConsPress  = 2     ! variable supply air flow, constant supply plenum pressure
	// INTEGER, PARAMETER :: VarFlowVarPress   = 3     ! variable supply air flow, variable supply plenum pressure

	// parameters to indicate diffuser type
	int const Swirl( 1 );
	int const VarArea( 2 );
	int const DisplVent( 3 );
	int const LinBarGrille( 4 );
	int const Custom( 5 );

	// parameters for comfort calculations
	int const VComfort_Invalid( -1 );
	int const VComfort_Jet( 1 );
	int const VComfort_Recirculation( 2 );

	// DERIVED TYPE DEFINITIONS

	// Air Node Data

	// UCSD

	// END UCSD

	// begin NREL RoomAir DERIVED TYPES ******************************************

	// end NREL room air derived types*********************************

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	int TotNumOfAirNodes( 0 );
	int TotNumOfRoomAFNNodes( 0 );
	Array1D_int TotNumOfZoneAirNodes;
	Array1D< Real64 > ConvectiveFloorSplit;
	Array1D< Real64 > InfiltratFloorSplit;
	// UCSD
	Array1D< Real64 > DVHcIn;
	int TotUCSDDV( 0 ); // Total number of UCSDDV zones
	Array1D_bool IsZoneDV; // Is the air model for the zone UCSDDV?
	Array1D< Real64 > ZTOC; // Temperature of occupied (lower) zone
	Array1D< Real64 > AvgTempGrad; // vertical Average Temperature Gradient in the room
	Array1D< Real64 > ZTMX; // Temperature of the mixing(upper) layer
	Array1D< Real64 > MaxTempGrad; // maximum Average Temperature Gradient in the room
	Array1D< Real64 > HVACAirTemp; // HVAC system temperature (DEG C)
	Array1D< Real64 > HVACMassFlow; // HVAC system mass flow rate (KG/S)
	Array1D< Real64 > ZTFloor;
	Array1D< Real64 > HeightTransition;
	Array1D< Real64 > FracMinFlow;
	Array1D_int ZoneDVMixedFlag;
	Array1D< Real64 > ZoneDVMixedFlagRep;
	Array1D_bool ZoneAirSystemON;
	Array1D< Real64 > TCMF; // comfort temperature
	Array1D< Real64 > ZoneCeilingHeight;
	Array1D< Real64 > MATFloor; // [C] floor level mean air temp
	Array1D< Real64 > XMATFloor; // [C] floor level mean air temp at t minus 1 zone time step
	Array1D< Real64 > XM2TFloor; // [C] floor level mean air temp at t minus 2 zone time step
	Array1D< Real64 > XM3TFloor; // [C] floor level mean air temp at t minus 3 zone time step
	Array1D< Real64 > XM4TFloor; // [C] floor level mean air temp at t minus 4 zone time step
	Array1D< Real64 > DSXMATFloor; // [C] floor level mean air temp at t minus 1 system time step
	Array1D< Real64 > DSXM2TFloor; // [C] floor level mean air temp at t minus 2 system time step
	Array1D< Real64 > DSXM3TFloor; // [C] floor level mean air temp at t minus 3 system time step
	Array1D< Real64 > DSXM4TFloor; // [C] floor level mean air temp at t minus 4 system time step
	Array1D< Real64 > MATOC; // [C] occupied mean air temp
	Array1D< Real64 > XMATOC; // [C] occupied mean air temp at t minus 1 zone time step
	Array1D< Real64 > XM2TOC; // [C] occupied mean air temp at t minus 2 zone time step
	Array1D< Real64 > XM3TOC; // [C] occupied mean air temp at t minus 3 zone time step
	Array1D< Real64 > XM4TOC; // [C] occupied mean air temp at t minus 4 zone time step
	Array1D< Real64 > DSXMATOC; // [C] occupied mean air temp at t minus 1 system time step
	Array1D< Real64 > DSXM2TOC; // [C] occupied mean air temp at t minus 2 system time step
	Array1D< Real64 > DSXM3TOC; // [C] occupied mean air temp at t minus 3 system time step
	Array1D< Real64 > DSXM4TOC; // [C] occupied mean air temp at t minus 4 system time step
	Array1D< Real64 > MATMX; // [C] mixed (upper) mean air temp
	Array1D< Real64 > XMATMX; // [C] mixed (upper) mean air temp at t minus 1 zone time step
	Array1D< Real64 > XM2TMX; // [C] mixed (upper) mean air temp at t minus 2 zone time step
	Array1D< Real64 > XM3TMX; // [C] mixed (upper) mean air temp at t minus 3 zone time step
	Array1D< Real64 > XM4TMX; // [C] mixed (upper) mean air temp at t minus 4 zone time step
	Array1D< Real64 > DSXMATMX; // [C] mixed  mean air temp at t minus 1 system time step
	Array1D< Real64 > DSXM2TMX; // [C] mixed  mean air temp at t minus 2 system time step
	Array1D< Real64 > DSXM3TMX; // [C] mixed  mean air temp at t minus 3 system time step
	Array1D< Real64 > DSXM4TMX; // [C] mixed  mean air temp at t minus 4 system time step
	Array1D< Real64 > ZTM1Floor; // [C] difference equation's Floor air temp at t minus 1
	Array1D< Real64 > ZTM2Floor; // [C] difference equation's Floor air temp at t minus 2
	Array1D< Real64 > ZTM3Floor; // [C] difference equation's Floor air temp at t minus 3
	Array1D< Real64 > ZTM1OC; // [C] difference equation's Occupied air temp at t minus 1
	Array1D< Real64 > ZTM2OC; // [C] difference equation's Occupied air temp at t minus 2
	Array1D< Real64 > ZTM3OC; // [C] difference equation's Occupied air temp at t minus 3
	Array1D< Real64 > ZTM1MX; // [C] difference equation's Mixed  air temp at t minus 1
	Array1D< Real64 > ZTM2MX; // [C] difference equation's Mixed  air temp at t minus 1
	Array1D< Real64 > ZTM3MX; // [C] difference equation's Mixed  air temp at t minus 1
	Array1D< Real64 > AIRRATFloor;
	Array1D< Real64 > AIRRATOC;
	Array1D< Real64 > AIRRATMX;
	// Euler and Exact solution algorithms
	Array1D< Real64 > Zone1Floor; // [C] difference equation's Floor air temp at previous dt
	Array1D< Real64 > ZoneMXFloor; // [C] difference equation's Floor air temp at t minus 1
	Array1D< Real64 > ZoneM2Floor; // [C] difference equation's Floor air temp at t minus 2
	Array1D< Real64 > Zone1OC; // [C] difference equation's Occupied air temp at previous dt
	Array1D< Real64 > ZoneMXOC; // [C] difference equation's Occupied air temp at t minus 1
	Array1D< Real64 > ZoneM2OC; // [C] difference equation's Occupied air temp at t minus 2
	Array1D< Real64 > Zone1MX; // [C] difference equation's Mixed  air temp at previous dt
	Array1D< Real64 > ZoneMXMX; // [C] difference equation's Mixed  air temp at t minus 1
	Array1D< Real64 > ZoneM2MX; // [C] difference equation's Mixed  air temp at t minus 2
	// UCSD-CV
	Array1D< Real64 > CVHcIn;
	int TotUCSDCV( 0 ); // Total number of UCSDDV zones
	Array1D_bool IsZoneCV; // Is the air model for the zone UCSDDV?
	Array1D< Real64 > ZoneCVisMixing; // Zone set to CV is actually using a mixing model
	Array1D< Real64 > ZTJET; // Jet Temperatures
	Array1D< Real64 > ZTREC; // Recirculation Temperatures
	Array1D< Real64 > RoomOutflowTemp; // Temperature of air flowing out of the room
	Array1D< Real64 > JetRecAreaRatio;
	Array1D< Real64 > Urec; // Recirculation region average velocity
	Array1D< Real64 > Ujet; // Jet region average velocity
	Array1D< Real64 > Qrec; // Recirculation zone total flow rate
	Array1D< Real64 > Qtot; // Total volumetric inflow rate through all active aperatures [m3/s]
	Array1D< Real64 > RecInflowRatio; // Ratio of the recirculation volumetric flow rate to the total inflow flow rate []
	Array1D< Real64 > Uhc;
	Array1D< Real64 > Ain; // Inflow aperture area
	Array1D< Real64 > Droom; // CV Zone average length
	Array1D< Real64 > Dstar; // CV Zone average length, wind direction corrected
	Array1D< Real64 > Tin; // Inflow air temperature
	Array1D< Real64 > TotArea; // Sum of the areas of all apertures in the zone
	Array2D_int AirflowNetworkSurfaceUCSDCV; // table for AirflowNetwork surfaces organization
	int CVNumAirflowNetworkSurfaces( 0 ); // total number of AirFlowNetwork surfaces.
	// Interzone surfaces counts twice.
	Array1D< Real64 > Rfr; // Ration between inflow and recirculation air flows
	Array1D< Real64 > ZoneCVhasREC; // Airflow pattern is C(0), CR(1)
	bool UCSDModelUsed( false );
	bool MundtModelUsed( false );
	// UCSD-UF
	int TotUCSDUI( 0 ); // total number of UCSDUI zones
	int TotUCSDUE( 0 ); // total number of UCSDUE zones
	Array1D_bool IsZoneUI; // controls program flow, for interior or exterior UFAD model
	Array1D_int ZoneUFPtr;
	Array1D< Real64 > UFHcIn;
	Array1D_int ZoneUFMixedFlag;
	Array1D< Real64 > ZoneUFMixedFlagRep;
	Array1D< Real64 > ZoneUFGamma;
	Array1D< Real64 > ZoneUFPowInPlumes; // [W]
	Array1D< Real64 > ZoneUFPowInPlumesfromWindows; // [W]
	Array1D< Real64 > Phi; // dimensionless measure of occupied subzone temperature

	// END UCSD

	// Begin NREL User-defined patterns

	int numTempDistContrldZones( 0 ); // count of zones with user-defined patterns
	int NumAirTempPatterns( 0 ); // count of all different patterns in input file
	int NumConstantGradient( 0 ); // count of constant gradient patterns in input
	int NumTwoGradientInterp( 0 ); // count of two gradient interp patterns in input
	int NumNonDimensionalHeight( 0 ); // count of ND height profile patterns in input
	int NumSurfaceMapping( 0 ); // count of generic surface map patterns in input

	bool UserDefinedUsed( false ); // true if user-defined model used anywhere
	// End User-defined patterns

	// RoomAirflowNetwork
	int NumOfRoomAirflowNetControl( 0 ); // count of RoomAirSettings:AirflowNetwork

	// Object Data
	Array1D< AirModelData > AirModel;
	Array1D< AirNodeData > AirNode;
	Array1D< DVData > ZoneUCSDDV; // UCSD
	Array1D< CVData > ZoneUCSDCV;
	Array1D< UFIData > ZoneUCSDUI;
	Array1D< UFEData > ZoneUCSDUE;
	Array2D< CVFlow > CVJetRecFlows; // Jet and recirculation zone flows and properties
	Array1D< CVDVParameters > SurfParametersCVDV; // Surface parameters
	Array1D< TemperaturePatternStruct > RoomAirPattern; // user defined patterns ,various types
	Array1D< AirPatternInfobyZoneStruct > AirPatternZoneInfo; // added zone information for user defined patterns
	Array1D< RoomAirflowNetworkInfoByZoneStruct > RoomAirflowNetworkZoneInfo; // added zone info

	void
	clear_state()
	{
		TotNumOfAirNodes = 0;
		TotNumOfRoomAFNNodes = 0;
		TotNumOfZoneAirNodes.deallocate();
		ConvectiveFloorSplit.deallocate();
		InfiltratFloorSplit.deallocate();
		DVHcIn.deallocate();
		TotUCSDDV = 0;
		IsZoneDV.deallocate();
		ZTOC.deallocate();
		AvgTempGrad.deallocate();
		ZTMX.deallocate();
		MaxTempGrad.deallocate();
		HVACAirTemp.deallocate();
		HVACMassFlow.deallocate();
		ZTFloor.deallocate();
		HeightTransition.deallocate();
		FracMinFlow.deallocate();
		ZoneDVMixedFlag.deallocate();
		ZoneDVMixedFlagRep.deallocate();
		ZoneAirSystemON.deallocate();
		TCMF.deallocate();
		ZoneCeilingHeight.deallocate();
		MATFloor.deallocate();
		XMATFloor.deallocate();
		XM2TFloor.deallocate();
		XM3TFloor.deallocate();
		XM4TFloor.deallocate();
		DSXMATFloor.deallocate();
		DSXM2TFloor.deallocate();
		DSXM3TFloor.deallocate();
		DSXM4TFloor.deallocate();
		MATOC.deallocate();
		XMATOC.deallocate();
		XM2TOC.deallocate();
		XM3TOC.deallocate();
		XM4TOC.deallocate();
		DSXMATOC.deallocate();
		DSXM2TOC.deallocate();
		DSXM3TOC.deallocate();
		DSXM4TOC.deallocate();
		MATMX.deallocate();
		XMATMX.deallocate();
		XM2TMX.deallocate();
		XM3TMX.deallocate();
		XM4TMX.deallocate();
		DSXMATMX.deallocate();
		DSXM2TMX.deallocate();
		DSXM3TMX.deallocate();
		DSXM4TMX.deallocate();
		ZTM1Floor.deallocate();
		ZTM2Floor.deallocate();
		ZTM3Floor.deallocate();
		ZTM1OC.deallocate();
		ZTM2OC.deallocate();
		ZTM3OC.deallocate();
		ZTM1MX.deallocate();
		ZTM2MX.deallocate();
		ZTM3MX.deallocate();
		AIRRATFloor.deallocate();
		AIRRATOC.deallocate();
		AIRRATMX.deallocate();
		Zone1Floor.deallocate();
		ZoneMXFloor.deallocate();
		ZoneM2Floor.deallocate();
		Zone1OC.deallocate();
		ZoneMXOC.deallocate();
		ZoneM2OC.deallocate();
		Zone1MX.deallocate();
		ZoneMXMX.deallocate();
		ZoneM2MX.deallocate();
		CVHcIn.deallocate();
		TotUCSDCV = 0;
		IsZoneCV.deallocate();
		ZoneCVisMixing.deallocate();
		ZTJET.deallocate();
		ZTREC.deallocate();
		RoomOutflowTemp.deallocate();
		JetRecAreaRatio.deallocate();
		Urec.deallocate();
		Ujet.deallocate();
		Qrec.deallocate();
		Qtot.deallocate();
		RecInflowRatio.deallocate();
		Uhc.deallocate();
		Ain.deallocate();
		Droom.deallocate();
		Dstar.deallocate();
		Tin.deallocate();
		TotArea.deallocate();
		AirflowNetworkSurfaceUCSDCV.deallocate();
		CVNumAirflowNetworkSurfaces = 0;
		Rfr.deallocate();
		ZoneCVhasREC.deallocate();
		UCSDModelUsed = false;
		MundtModelUsed = false;
		TotUCSDUI = 0;
		TotUCSDUE = 0;
		IsZoneUI.deallocate();
		ZoneUFPtr.deallocate();
		UFHcIn.deallocate();
		ZoneUFMixedFlag.deallocate();
		ZoneUFMixedFlagRep.deallocate();
		ZoneUFGamma.deallocate();
		ZoneUFPowInPlumes.deallocate();
		ZoneUFPowInPlumesfromWindows.deallocate();
		Phi.deallocate();
		numTempDistContrldZones = 0;
		NumAirTempPatterns = 0;
		NumConstantGradient = 0;
		NumTwoGradientInterp = 0;
		NumNonDimensionalHeight = 0;
		NumSurfaceMapping = 0;
		UserDefinedUsed = false;
		NumOfRoomAirflowNetControl = 0;
		AirModel.deallocate();
		AirNode.deallocate();
		ZoneUCSDDV.deallocate();
		ZoneUCSDCV.deallocate();
		ZoneUCSDUI.deallocate();
		ZoneUCSDUE.deallocate();
		CVJetRecFlows.deallocate();
		SurfParametersCVDV.deallocate();
		RoomAirPattern.deallocate();
		AirPatternZoneInfo.deallocate();
		RoomAirflowNetworkZoneInfo.deallocate();
	}

	//**********************************************************************************************

} // DataRoomAirModel

} // EnergyPlus
