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

// EnergyPlus::DXCoils unit tests
// DX heating coil defrost capacity with electric resistance

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <DXCoils.hh>
#include <CurveManager.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataSizing.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <OutputReportPredefined.hh>
#include <ScheduleManager.hh>
#include <Psychrometrics.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>

using namespace EnergyPlus;
using namespace DXCoils;
using namespace DataAirLoop;
using namespace DataAirSystems;
using namespace DataHVACGlobals;
using namespace DataSizing;
using namespace CurveManager;
using namespace OutputReportPredefined;
using namespace ScheduleManager;
using namespace DataEnvironment;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, DXCoils_Test1 ) {
		using CurveManager::Quadratic;
		using CurveManager::BiQuadratic;
		using CurveManager::NumCurves;
		//	int NumDXCoils( 0 ); // Total number of DX coils
		//	Array1D< DXCoilData > DXCoil;
		int DXCoilNum;
		int CurveNum;

		NumDXCoils = 2;
		DXCoilNum = 2;
		DXCoil.allocate( NumDXCoils );
		DXCoil( 1 ).DXCoilType_Num = CoilDX_MultiSpeedCooling;
		DXCoil( 1 ).DXCoilType = "Coil:Cooling:DX:MultiSpeed";
		DXCoil( 2 ).DXCoilType_Num = CoilDX_MultiSpeedHeating;
		DXCoil( 2 ).DXCoilType = "Coil:Heating:DX:MultiSpeed";
		DXCoil( 1 ).MSRatedTotCap.allocate( 2 );
		DXCoil( 2 ).MSRatedTotCap.allocate( 2 );
		DXCoil( 2 ).CompanionUpstreamDXCoil = 1;

		DXCoilNumericFields.allocate( NumDXCoils );
		DXCoilNumericFields( 2 ).PerfMode.allocate( 1 );
		DXCoilNumericFields( 2 ).PerfMode( 1 ).FieldNames.allocate( 15 );
		DXCoil( 2 ).DefrostStrategy = Resistive;
		DXCoil( 2 ).DefrostCapacity = 5000.0;
		DXCoil( 2 ).Name = "DX Heating coil";
		DXCoil( 1 ).NumOfSpeeds = 2;
		DXCoil( 2 ).NumOfSpeeds = 2;

		DXCoil( DXCoilNum ).MSRatedTotCap.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSRatedSHR.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSRatedCOP.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSRatedAirVolFlowRate.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSRatedAirMassFlowRate.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSCCapFTemp.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSCCapFFlow.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSEIRFTemp.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSEIRFFlow.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSWasteHeat.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSEvapCondEffect.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSEvapCondAirFlow.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSEvapCondPumpElecNomPower.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSRatedCBF.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSWasteHeatFrac.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSPLFFPLR.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSTwet_Rated.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSGamma_Rated.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSMaxONOFFCyclesperHour.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSLatentCapacityTimeConstant.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );
		DXCoil( DXCoilNum ).MSFanPowerPerEvapAirFlowRate.allocate( DXCoil( DXCoilNum ).NumOfSpeeds );

		DXCoil( 1 ).MSRatedTotCap( 1 ) = 4455.507579219055;
		DXCoil( 1 ).MSRatedTotCap( 2 ) = 6188.507579219055;

		DXCoil( DXCoilNum ).MSRatedTotCap( 1 ) = 4455.507579219055;
		DXCoil( DXCoilNum ).MSRatedTotCap( 2 ) = 6188.204971137576;
		DXCoil( DXCoilNum ).MSRatedCOP( 1 ) = 4.03;
		DXCoil( DXCoilNum ).MSRatedCOP( 2 ) = 3.53;

		DXCoil( DXCoilNum ).MSCCapFFlow = 1;
		DXCoil( DXCoilNum ).MSCCapFTemp = 3;
		DXCoil( DXCoilNum ).MSEIRFFlow = 1;
		DXCoil( DXCoilNum ).MSEIRFTemp = 3;
		DXCoil( DXCoilNum ).MSPLFFPLR = 2;
		DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( 1 ) = 0.2339;
		DXCoil( DXCoilNum ).MSRatedAirVolFlowRate( 2 ) = 0.2924;
		DXCoil( DXCoilNum ).MSFanPowerPerEvapAirFlowRate = 0.0;
		DXCoil( DXCoilNum ).RegionNum = 4;
		DXCoil( DXCoilNum ).MinOATCompressor = -17.78;


		NumCurves = 3;
		PerfCurve.allocate( NumCurves );

		CurveNum = 1;
		PerfCurve( CurveNum ).CurveType = Quadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 1;
		PerfCurve( CurveNum ).Coeff2 = 0.0;
		PerfCurve( CurveNum ).Coeff3 = 0.0;
		PerfCurve( CurveNum ).Coeff4 = 0.0;
		PerfCurve( CurveNum ).Coeff5 = 0.0;
		PerfCurve( CurveNum ).Coeff6 = 0.0;
		PerfCurve( CurveNum ).Var1Min = 0.0;
		PerfCurve( CurveNum ).Var1Max = 2.0;
		PerfCurve( CurveNum ).Var2Min = 0.0;
		PerfCurve( CurveNum ).Var2Max = 2.0;

		CurveNum = 2;
		PerfCurve( CurveNum ).CurveType = Quadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 1;
		PerfCurve( CurveNum ).Coeff2 = 0.0;
		PerfCurve( CurveNum ).Coeff3 = 0.0;
		PerfCurve( CurveNum ).Coeff4 = 0.0;
		PerfCurve( CurveNum ).Coeff5 = 0.0;
		PerfCurve( CurveNum ).Coeff6 = 0.0;
		PerfCurve( CurveNum ).Var1Min = 0.0;
		PerfCurve( CurveNum ).Var1Max = 1.0;
		PerfCurve( CurveNum ).Var2Min = 0.7;
		PerfCurve( CurveNum ).Var2Max = 1.0;

		CurveNum = 3;
		PerfCurve( CurveNum ).CurveType = BiQuadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_BiQuadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 1;
		PerfCurve( CurveNum ).Coeff2 = 0.0;
		PerfCurve( CurveNum ).Coeff3 = 0.0;
		PerfCurve( CurveNum ).Coeff4 = 0.0;
		PerfCurve( CurveNum ).Coeff5 = 0.0;
		PerfCurve( CurveNum ).Coeff6 = 0.0;
		PerfCurve( CurveNum ).Var1Min = -100.0;
		PerfCurve( CurveNum ).Var1Max = 100.0;
		PerfCurve( CurveNum ).Var2Min = -100.0;
		PerfCurve( CurveNum ).Var2Max = 100.0;

		SetPredefinedTables();
		SizeDXCoil( 2 );
		EXPECT_DOUBLE_EQ( 5000.0, DXCoil( 2 ).DefrostCapacity );

		EXPECT_TRUE( has_cerr_output() );

		// fails on windows due to endline issue... this outputs /r/n on Windows but it is outputting /n on Windows for some reason...
		// EXPECT_TRUE( compare_cerr_stream( delimited_string( {
		// 	"! <DX Heating Coil Standard Rating Information>, Component Type, Component Name, High Temperature Heating (net) Rating Capacity {W}, Low Temperature Heating (net) Rating Capacity {W}, HSPF {Btu/W-h}, Region Number",
		// 	" DX Heating Coil Standard Rating Information, , DX Heating coil, 6414.3, 6414.3, 6.58, 4" } ) ) );

		// Clean up
		DXCoil.deallocate();
		DXCoilNumericFields.deallocate();
		PerfCurve.deallocate();

	}
	TEST_F( EnergyPlusFixture, DXCoils_Test2 ) {
		using CurveManager::Quadratic;
		using CurveManager::BiQuadratic;
		using CurveManager::NumCurves;
		int DXCoilNum;
		int CurveNum;

		DataGlobals::DisplayExtraWarnings = true;
		SysSizingRunDone = true;
		FinalSysSizing.allocate( 1 );
		PrimaryAirSystem.allocate( 1 );
		AirLoopControlInfo.allocate( 1 );
		CurSysNum = 1;
		NumDXCoils = 2;
		DXCoilNum = 2;
		UnitarySysEqSizing.allocate( 1 );
		DXCoil.allocate( NumDXCoils );
		DXCoil( 1 ).DXCoilType_Num = CoilDX_CoolingSingleSpeed;
		DXCoil( 2 ).DXCoilType_Num = CoilDX_HeatingEmpirical;
		DXCoil( DXCoilNum ).DXCoilType = "Coil:Heating:DX:SingleSpeed";
		DXCoil( 2 ).CompanionUpstreamDXCoil = 1;

		DXCoilNumericFields.allocate( NumDXCoils );
		DXCoilNumericFields( 2 ).PerfMode.allocate( 1 );
		DXCoilNumericFields( 2 ).PerfMode( 1 ).FieldNames.allocate( 20 );
		DXCoil( 2 ).DefrostStrategy = Resistive;
		DXCoil( 2 ).DefrostCapacity = 5000.0;
		DXCoil( 2 ).Name = "DX Heating coil";

		DXCoil( 1 ).RatedTotCap( 1 ) = AutoSize;
		DXCoil( 1 ).RatedTotCap( 2 ) = AutoSize;
		DXCoil( 2 ).RatedTotCap( 1 ) = AutoSize;
		DXCoil( DXCoilNum ).RegionNum = 4;
		DXCoil( DXCoilNum ).MinOATCompressor = -17.78;
		DXCoil( DXCoilNum ).CCapFFlow( 1 ) = 1;
		DXCoil( DXCoilNum ).CCapFTemp( 1 ) = 1;
		DXCoil( DXCoilNum ).EIRFFlow( 1 ) = 1;
		DXCoil( DXCoilNum ).EIRFTemp( 1 ) = 1;
		DXCoil( DXCoilNum ).PLFFPLR( 1 ) = 1;
		NumCurves = 3;
		PerfCurve.allocate( NumCurves );

		CurveNum = 1;
		PerfCurve( CurveNum ).CurveType = Quadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 1;
		PerfCurve( CurveNum ).Coeff2 = 0.0;
		PerfCurve( CurveNum ).Coeff3 = 0.0;
		PerfCurve( CurveNum ).Coeff4 = 0.0;
		PerfCurve( CurveNum ).Coeff5 = 0.0;
		PerfCurve( CurveNum ).Coeff6 = 0.0;
		PerfCurve( CurveNum ).Var1Min = 0.0;
		PerfCurve( CurveNum ).Var1Max = 2.0;
		PerfCurve( CurveNum ).Var2Min = 0.0;
		PerfCurve( CurveNum ).Var2Max = 2.0;

		CurveNum = 2;
		PerfCurve( CurveNum ).CurveType = Quadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_Quadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 1;
		PerfCurve( CurveNum ).Coeff2 = 0.0;
		PerfCurve( CurveNum ).Coeff3 = 0.0;
		PerfCurve( CurveNum ).Coeff4 = 0.0;
		PerfCurve( CurveNum ).Coeff5 = 0.0;
		PerfCurve( CurveNum ).Coeff6 = 0.0;
		PerfCurve( CurveNum ).Var1Min = 0.0;
		PerfCurve( CurveNum ).Var1Max = 1.0;
		PerfCurve( CurveNum ).Var2Min = 0.7;
		PerfCurve( CurveNum ).Var2Max = 1.0;

		CurveNum = 3;
		PerfCurve( CurveNum ).CurveType = BiQuadratic;
		PerfCurve( CurveNum ).ObjectType = CurveType_BiQuadratic;
		PerfCurve( CurveNum ).InterpolationType = EvaluateCurveToLimits;
		PerfCurve( CurveNum ).Coeff1 = 1;
		PerfCurve( CurveNum ).Coeff2 = 0.0;
		PerfCurve( CurveNum ).Coeff3 = 0.0;
		PerfCurve( CurveNum ).Coeff4 = 0.0;
		PerfCurve( CurveNum ).Coeff5 = 0.0;
		PerfCurve( CurveNum ).Coeff6 = 0.0;
		PerfCurve( CurveNum ).Var1Min = -100.0;
		PerfCurve( CurveNum ).Var1Max = 100.0;
		PerfCurve( CurveNum ).Var2Min = -100.0;
		PerfCurve( CurveNum ).Var2Max = 100.0;

		SetPredefinedTables();
		SizeDXCoil( 2 );
		EXPECT_DOUBLE_EQ( 0.0, DXCoil( 2 ).RatedTotCap( 1 ) );

		EXPECT_TRUE( has_cerr_output() );

		// EXPECT_TRUE( compare_cerr_stream( delimited_string( {
		// 	"! <Component Sizing Information>, Component Type, Component Name, Input Field Description, Value",
		// 	" Component Sizing Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, Design Size  [W], 0.00000",
		// 	" Component Sizing Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, User-Specified  [W], 5000.00000",
		// 	" DX Heating Coil Standard Rating Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, 0.0, 0.0, 3.51, 4"} ) ) );

		// Output from CI, I don't know why it is different than above...

		// "! , Component Type, Component Name, Input Field Description, Value",
		// " Component Sizing Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, Design Size [W], 0.00000",
		// " Component Sizing Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, User-Specified [W], 5000.00000",
		// "! , Component Type, Component Name, High Temperature Heating (net) Rating Capacity {W}, Low Temperature Heating (net) Rating Capacity {W}, HSPF {Btu/W-h}, Region Number",
		// " DX Heating Coil Standard Rating Information, Coil:Heating:DX:SingleSpeed, DX Heating coil, 0.0, 0.0, 3.51, 4"

		// Clean up
		DXCoil.deallocate();
		DXCoilNumericFields.deallocate();
		UnitarySysEqSizing.deallocate();
		PerfCurve.deallocate();
		FinalSysSizing.deallocate();
		PrimaryAirSystem.deallocate();
		AirLoopControlInfo.deallocate();

	}

	TEST_F( EnergyPlusFixture, TestMultiSpeedDefrostCOP ) {
		// Test that the COP calculation is correct when the defrost is on. #4973

		using CurveManager::Quadratic;
		using CurveManager::BiQuadratic;
		using CurveManager::NumCurves;
		using DataEnvironment::OutHumRat;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutBaroPress;
		using DXCoils::CalcMultiSpeedDXCoilHeating;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyHFnTdbW;
		int DXCoilNum;

		// Set up heating coil and curves.

		NumDXCoils = 1;
		DXCoilNum = 1;
		DXCoil.allocate( NumDXCoils );
		DXCoilData & Coil = DXCoil( DXCoilNum );

		Coil.DXCoilType = "Coil:Heating:DX:MultiSpeed";
		Coil.DXCoilType_Num = CoilDX_MultiSpeedHeating;
		Coil.SchedPtr = DataGlobals::ScheduleAlwaysOn;

		DXCoilNumericFields.allocate( NumDXCoils );
		DataHeatBalance::HeatReclaimDXCoil.allocate( NumDXCoils );
		DXCoilOutletTemp.allocate( NumDXCoils );
		DXCoilOutletHumRat.allocate( NumDXCoils );
		DXCoilFanOpMode.allocate( NumDXCoils );
		DXCoilPartLoadRatio.allocate( NumDXCoils );
		DXCoilNumericFields( DXCoilNum ).PerfMode.allocate( 1 );
		DXCoilNumericFields( DXCoilNum ).PerfMode( 1 ).FieldNames.allocate( 15 );
		Coil.DefrostStrategy = Resistive;
		Coil.Name = "DX Heating coil";
		Coil.NumOfSpeeds = 2;

		Coil.MSRatedTotCap.allocate( Coil.NumOfSpeeds );
		Coil.MSRatedSHR.allocate( Coil.NumOfSpeeds );
		Coil.MSRatedCOP.allocate( Coil.NumOfSpeeds );
		Coil.MSRatedAirVolFlowRate.allocate( Coil.NumOfSpeeds );
		Coil.MSRatedAirMassFlowRate.allocate( Coil.NumOfSpeeds );
		Coil.MSCCapFTemp.allocate( Coil.NumOfSpeeds );
		Coil.MSCCapFFlow.allocate( Coil.NumOfSpeeds );
		Coil.MSEIRFTemp.allocate( Coil.NumOfSpeeds );
		Coil.MSEIRFFlow.allocate( Coil.NumOfSpeeds );
		Coil.MSWasteHeat.allocate( Coil.NumOfSpeeds );
		Coil.MSEvapCondEffect.allocate( Coil.NumOfSpeeds );
		Coil.MSEvapCondAirFlow.allocate( Coil.NumOfSpeeds );
		Coil.MSEvapCondPumpElecNomPower.allocate( Coil.NumOfSpeeds );
		Coil.MSRatedCBF.allocate( Coil.NumOfSpeeds );
		Coil.MSWasteHeatFrac.allocate( Coil.NumOfSpeeds );
		Coil.MSPLFFPLR.allocate( Coil.NumOfSpeeds );
		Coil.MSTwet_Rated.allocate( Coil.NumOfSpeeds );
		Coil.MSGamma_Rated.allocate( Coil.NumOfSpeeds );
		Coil.MSMaxONOFFCyclesperHour.allocate( Coil.NumOfSpeeds );
		Coil.MSLatentCapacityTimeConstant.allocate( Coil.NumOfSpeeds );
		Coil.MSFanPowerPerEvapAirFlowRate.allocate( Coil.NumOfSpeeds );
		Coil.MSTotCapTempModFacCurveType.allocate( Coil.NumOfSpeeds );
		Coil.MSEIRTempModFacCurveType.allocate( Coil.NumOfSpeeds );

		Coil.MinOATCompressor = -73.27777777777779;
		Coil.CrankcaseHeaterCapacity = 0.0;
		Coil.MaxOATDefrost = 0.0;
		Coil.DefrostStrategy = Resistive;
		Coil.DefrostControl = Timed;
		Coil.DefrostTime = 0.058333;
		Coil.DefrostCapacity = 1000;
		Coil.PLRImpact = false;
		Coil.FuelType = FuelTypeElectricity;
		Coil.RegionNum = 4;
		Coil.MSRatedTotCap( 1 ) = 2202.5268975202675;
		Coil.MSRatedCOP( 1 ) = 4.200635910578916;
		Coil.MSRatedAirVolFlowRate( 1 ) = 0.087746133503702;
		Coil.MSFanPowerPerEvapAirFlowRate( 1 ) = 773.3;
		Coil.MSWasteHeatFrac( 1 ) = 0.2;
		Coil.MSRatedTotCap( 2 ) = 11012.634487601337;
		Coil.MSRatedCOP( 2 ) = 4.200635910578916;
		Coil.MSRatedAirVolFlowRate( 2 ) = 0.43873066751851;
		Coil.MSFanPowerPerEvapAirFlowRate( 2 ) = 773.3;
		Coil.MSWasteHeatFrac( 2 ) = 0.2;
		Coil.RatedSHR( 1 ) = 1.0;

		for ( int mode = 1; mode <= Coil.NumOfSpeeds; ++mode ) {
			Coil.MSRatedAirMassFlowRate( mode ) = Coil.MSRatedAirVolFlowRate( mode ) * PsyRhoAirFnPbTdbW( EnergyPlus::DataEnvironment::StdBaroPress, 21.11, 0.00881, "InitDXCoil" );
		}

		NumCurves = 11;
		PerfCurve.allocate( NumCurves );

		PerfomanceCurveData * pCurve;

		int const nCapfT1 = 1;
		pCurve = &PerfCurve( nCapfT1 );
		pCurve->CurveType = BiQuadratic;
		pCurve->Name = "HP_Heat-Cap-fT1";
		pCurve->Coeff1 = 0.95624428;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Coeff4 = 0.005999544;
		pCurve->Coeff5 = -0.0000900072;
		pCurve->Coeff6 = 0;
		pCurve->Var1Min = -100;
		pCurve->Var1Max = 100;
		pCurve->Var2Min = -100;
		pCurve->Var2Max = 100;

		Coil.MSCCapFTemp( 1 ) = nCapfT1;
		Coil.MSTotCapTempModFacCurveType( 1 ) = pCurve->CurveType;

		int const nCapfFF1 = 2;
		pCurve = &PerfCurve( nCapfFF1 );
		pCurve->CurveType = Quadratic;
		pCurve->Name = "HP_Heat-Cap-fFF1";
		pCurve->Coeff1 = 1;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Var1Min = 0;
		pCurve->Var1Max = 2;
		pCurve->CurveMin = 0;
		pCurve->CurveMax = 2;

		Coil.MSCCapFFlow( 1 ) = nCapfFF1;

		int const nEIRfT1 = 3;
		pCurve = &PerfCurve( nEIRfT1 );
		pCurve->CurveType = BiQuadratic;
		pCurve->Name = "HP_Heat-EIR-fT1";
		pCurve->Coeff1 = 1.065476178;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Coeff4 = -0.0085714308;
		pCurve->Coeff5 = 0.0000857142;
		pCurve->Coeff6 = 0;
		pCurve->Var1Min = -100;
		pCurve->Var1Max = 100;
		pCurve->Var2Min = -100;
		pCurve->Var2Max = 100;

		Coil.MSEIRFTemp( 1 ) = nEIRfT1;
		Coil.MSEIRTempModFacCurveType( 1 ) = pCurve->CurveType;

		int const nEIRfFF1 = 4;
		pCurve = &PerfCurve( nEIRfFF1 );
		pCurve->CurveType = Quadratic;
		pCurve->Name = "HP_Heat-EIR-fFF1";
		pCurve->Coeff1 = 1;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Var1Min = 0;
		pCurve->Var1Max = 2;
		pCurve->CurveMin = 0;
		pCurve->CurveMax = 2;

		Coil.MSEIRFFlow( 1 ) = nEIRfFF1;

		int const nPLFfPLR1 = 5;
		pCurve = &PerfCurve( nPLFfPLR1 );
		pCurve->CurveType = Quadratic;
		pCurve->Name = "HP_Heat-PLF-fPLR1";
		pCurve->Coeff1 = 1;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Var1Min = 0;
		pCurve->Var1Max = 1;
		pCurve->CurveMin = 0.7;
		pCurve->CurveMax = 1;

		Coil.MSPLFFPLR( 1 ) = nPLFfPLR1;

		int const nConstantBiquadratic = 6;
		pCurve = &PerfCurve( nConstantBiquadratic );
		pCurve->CurveType = BiQuadratic;
		pCurve->Name = "ConstantBiquadratic";
		pCurve->Coeff1 = 1;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Coeff4 = 0;
		pCurve->Coeff5 = 0;
		pCurve->Coeff6 = 0;
		pCurve->Var1Min = -100;
		pCurve->Var1Max = 100;
		pCurve->Var2Min = -100;
		pCurve->Var2Max = 100;

		Coil.MSWasteHeat( 1 ) = nConstantBiquadratic;
		Coil.MSWasteHeat( 2 ) = nConstantBiquadratic;

		int const nCapfT2 = 7;
		pCurve = &PerfCurve( nCapfT2 );
		pCurve->CurveType = BiQuadratic;
		pCurve->Name = "HP_Heat-Cap-fT2";
		pCurve->Coeff1 = 0.95624428;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Coeff4 = 0.005999544;
		pCurve->Coeff5 = -0.0000900072;
		pCurve->Coeff6 = 0;
		pCurve->Var1Min = -100;
		pCurve->Var1Max = 100;
		pCurve->Var2Min = -100;
		pCurve->Var2Max = 100;

		Coil.MSCCapFTemp( 2 ) = nCapfT2;
		Coil.MSTotCapTempModFacCurveType( 2 ) = pCurve->CurveType;

		int const nCapfFF2 = 8;
		pCurve = &PerfCurve( nCapfFF2 );
		pCurve->CurveType = Quadratic;
		pCurve->Name = "HP_Heat-Cap-fFF2";
		pCurve->Coeff1 = 1;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Var1Min = 0;
		pCurve->Var1Max = 2;
		pCurve->CurveMin = 0;
		pCurve->CurveMax = 2;

		Coil.MSCCapFFlow( 2 ) = nCapfFF2;

		int const nEIRfT2 = 9;
		pCurve = &PerfCurve( nEIRfT2 );
		pCurve->CurveType = BiQuadratic;
		pCurve->Name = "HP_Heat-EIR-fT2";
		pCurve->Coeff1 = 1.065476178;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Coeff4 = -0.0085714308;
		pCurve->Coeff5 = 0.0000857142;
		pCurve->Coeff6 = 0;
		pCurve->Var1Min = -100;
		pCurve->Var1Max = 100;
		pCurve->Var2Min = -100;
		pCurve->Var2Max = 100;

		Coil.MSEIRFTemp( 2 ) = nEIRfT2;
		Coil.MSEIRTempModFacCurveType( 2 ) = pCurve->CurveType;

		int const nEIRfFF2 = 10;
		pCurve = &PerfCurve( nEIRfFF2 );
		pCurve->CurveType = Quadratic;
		pCurve->Name = "HP_Heat-EIR-fFF2";
		pCurve->Coeff1 = 1;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Var1Min = 0;
		pCurve->Var1Max = 2;
		pCurve->CurveMin = 0;
		pCurve->CurveMax = 2;

		Coil.MSEIRFFlow( 2 ) = nEIRfFF2;

		int const nPLFfPLR2 = 11;
		pCurve = &PerfCurve( nPLFfPLR2 );
		pCurve->CurveType = Quadratic;
		pCurve->Name = "HP_Heat-PLF-fPLR2";
		pCurve->Coeff1 = 1;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Var1Min = 0;
		pCurve->Var1Max = 1;
		pCurve->CurveMin = 0.7;
		pCurve->CurveMax = 1;

		Coil.MSPLFFPLR( 2 ) = nPLFfPLR2;

		for ( int CurveNum = 1; CurveNum <= NumCurves; ++CurveNum ) {
			PerfomanceCurveData & rCurve = PerfCurve( CurveNum );
			if ( rCurve.CurveType == BiQuadratic ) {
				rCurve.ObjectType = CurveType_BiQuadratic;
				rCurve.InterpolationType = EvaluateCurveToLimits;
			} else if ( rCurve.CurveType == Quadratic ) {
				rCurve.ObjectType = CurveType_Quadratic;
				rCurve.InterpolationType = EvaluateCurveToLimits;
			}
		}

		// Set up inlet air conditions.
		Coil.InletAirMassFlowRate = Coil.MSRatedAirMassFlowRate( 1 );
		MSHPMassFlowRateLow = Coil.MSRatedAirMassFlowRate( 1 );
		MSHPMassFlowRateHigh = Coil.MSRatedAirMassFlowRate( 2 );
		OutHumRat = 0.002;
		OutBaroPress = 101325; // sea level
		Coil.InletAirTemp = 20;
		Coil.InletAirHumRat = 0.008;
		Coil.InletAirEnthalpy = PsyHFnTdbW( Coil.InletAirTemp, Coil.InletAirHumRat );

		// Test high speed
		Real64 SpeedRatio = 1.0;
		Real64 CycRatio = 1.0;
		int SpeedNum = 2;
		int const FanOpMode = ContFanCycCoil;

		// Defroster on
		OutDryBulbTemp = -5.0; // cold
		CalcMultiSpeedDXCoilHeating( DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0 );
		Real64 COPwoDefrost = Coil.MSRatedCOP( SpeedNum ) / ( CurveValue( nEIRfT2, Coil.InletAirTemp, OutDryBulbTemp ) * CurveValue( nEIRfFF2, 1 ) );
		Real64 COPwDefrost = Coil.TotalHeatingEnergyRate / Coil.ElecHeatingPower;
		EXPECT_LT( COPwDefrost, COPwoDefrost );

		// Defroster off
		OutDryBulbTemp = 5.0; // not cold enough for defroster
		CalcMultiSpeedDXCoilHeating( DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0 );
		COPwoDefrost = Coil.MSRatedCOP( SpeedNum ) / ( CurveValue( nEIRfT2, Coil.InletAirTemp, OutDryBulbTemp ) * CurveValue( nEIRfFF2, 1 ) );
		COPwDefrost = Coil.TotalHeatingEnergyRate / Coil.ElecHeatingPower;
		EXPECT_DOUBLE_EQ( COPwoDefrost, COPwDefrost );

		// Test low speed
		SpeedNum = 1;

		// Defroster on
		OutDryBulbTemp = -5.0; // cold
		CalcMultiSpeedDXCoilHeating( DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0 );
		COPwoDefrost = Coil.MSRatedCOP( SpeedNum ) / ( CurveValue( nEIRfT1, Coil.InletAirTemp, OutDryBulbTemp ) * CurveValue( nEIRfFF1, 1 ) );
		COPwDefrost = Coil.TotalHeatingEnergyRate / Coil.ElecHeatingPower;
		EXPECT_LT( COPwDefrost, COPwoDefrost );

		// Defroster off
		OutDryBulbTemp = 5.0; // not cold enough for defroster
		CalcMultiSpeedDXCoilHeating( DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0 );
		COPwoDefrost = Coil.MSRatedCOP( SpeedNum ) / ( CurveValue( nEIRfT1, Coil.InletAirTemp, OutDryBulbTemp ) * CurveValue( nEIRfFF1, 1 ) );
		COPwDefrost = Coil.TotalHeatingEnergyRate / Coil.ElecHeatingPower;
		EXPECT_DOUBLE_EQ( COPwoDefrost, COPwDefrost );

		// Now test that coil output at Speed = 1, CyclingRatio = 1 is the same as Speed = 2 and SpeedRatio = 0
		Real64 DXCoilOutletNodeTemp = Coil.OutletAirTemp;
		Real64 DXCoilOutletNodeHumRat = Coil.OutletAirHumRat;
		Real64 DXCoilOutletNodeEnthalpy = Coil.OutletAirEnthalpy;
		Real64 DXCoilHeatingCapacity = Coil.TotalHeatingEnergyRate;

		SpeedRatio = 0.0;
		CycRatio = 1.0;
		SpeedNum = 2;

		CalcMultiSpeedDXCoilHeating( DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0 );

		Real64 DXCoilOutletNodeTemp2 = Coil.OutletAirTemp;
		Real64 DXCoilOutletNodeHumRat2 = Coil.OutletAirHumRat;
		Real64 DXCoilOutletNodeEnthalpy2 = Coil.OutletAirEnthalpy;
		Real64 DXCoilHeatingCapacity2 = Coil.TotalHeatingEnergyRate;

		EXPECT_DOUBLE_EQ( DXCoilOutletNodeTemp, DXCoilOutletNodeTemp2 );
		EXPECT_DOUBLE_EQ( DXCoilOutletNodeHumRat, DXCoilOutletNodeHumRat2 );
		EXPECT_DOUBLE_EQ( DXCoilOutletNodeEnthalpy, DXCoilOutletNodeEnthalpy2 );
		EXPECT_DOUBLE_EQ( DXCoilHeatingCapacity, DXCoilHeatingCapacity2 );

		// Defroster on
		OutDryBulbTemp = -5.0; // cold

		SpeedRatio = 0.0;
		CycRatio = 1.0;
		SpeedNum = 1;

		CalcMultiSpeedDXCoilHeating( DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0 );

		DXCoilOutletNodeTemp = Coil.OutletAirTemp;
		DXCoilOutletNodeHumRat = Coil.OutletAirHumRat;
		DXCoilOutletNodeEnthalpy = Coil.OutletAirEnthalpy;
		DXCoilHeatingCapacity = Coil.TotalHeatingEnergyRate;

		SpeedRatio = 0.0;
		CycRatio = 1.0;
		SpeedNum = 2;

		CalcMultiSpeedDXCoilHeating( DXCoilNum, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0 );

		DXCoilOutletNodeTemp2 = Coil.OutletAirTemp;
		DXCoilOutletNodeHumRat2 = Coil.OutletAirHumRat;
		DXCoilOutletNodeEnthalpy2 = Coil.OutletAirEnthalpy;
		DXCoilHeatingCapacity2 = Coil.TotalHeatingEnergyRate;

		EXPECT_DOUBLE_EQ( DXCoilOutletNodeTemp, DXCoilOutletNodeTemp2 );
		EXPECT_DOUBLE_EQ( DXCoilOutletNodeHumRat, DXCoilOutletNodeHumRat2 );
		EXPECT_DOUBLE_EQ( DXCoilOutletNodeEnthalpy, DXCoilOutletNodeEnthalpy2 );
		EXPECT_DOUBLE_EQ( DXCoilHeatingCapacity, DXCoilHeatingCapacity2 );

	}

	TEST_F( EnergyPlusFixture, TestSingleSpeedDefrostCOP ) {
		// Test that the COP calculation is correct when the defrost is on. #4973

		using CurveManager::Quadratic;
		using CurveManager::BiQuadratic;
		using CurveManager::NumCurves;
		using EnergyPlus::DataEnvironment::OutHumRat;
		using EnergyPlus::DataEnvironment::OutDryBulbTemp;
		using EnergyPlus::DataEnvironment::OutBaroPress;
		using DXCoils::CalcMultiSpeedDXCoilHeating;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyHFnTdbW;
		int DXCoilNum;

		// Set up heating coil and curves.

		NumDXCoils = 1;
		DXCoilNum = 1;
		DXCoil.allocate( NumDXCoils );
		DXCoilNumericFields.allocate( 1 );
		DXCoilOutletTemp.allocate( NumDXCoils );
		DXCoilOutletHumRat.allocate( NumDXCoils );
		DXCoilFanOpMode.allocate( NumDXCoils );
		DXCoilPartLoadRatio.allocate( NumDXCoils );
		DXCoilTotalHeating.allocate( NumDXCoils );
		DXCoilHeatInletAirDBTemp.allocate( NumDXCoils );
		DXCoilHeatInletAirWBTemp.allocate( NumDXCoils );
		DXCoilData & Coil = DXCoil( DXCoilNum );

		Coil.Name = "DX Single Speed Heating Coil";
		Coil.DXCoilType = "Coil:Heating:DX:SingleSpeed";
		Coil.DXCoilType_Num = CoilDX_HeatingEmpirical;
		Coil.SchedPtr = DataGlobals::ScheduleAlwaysOn;

		Coil.RatedSHR( 1 ) = 1.0;
		Coil.RatedTotCap( 1 ) = 11012.634487601337;
		Coil.RatedCOP( 1 ) = 4.200635910578916;
		Coil.RatedEIR( 1 ) = 1 / Coil.RatedCOP( 1 );
		Coil.RatedAirVolFlowRate( 1 ) = 0.43873066751851;
		Coil.RatedAirMassFlowRate( 1 ) = Coil.RatedAirVolFlowRate( 1 ) * PsyRhoAirFnPbTdbW( EnergyPlus::DataEnvironment::StdBaroPress, 21.11, 0.00881, "InitDXCoil" );
		Coil.FanPowerPerEvapAirFlowRate( 1 ) = 773.3;
		Coil.MinOATCompressor = -73.27777777777779;
		Coil.CrankcaseHeaterCapacity = 0.0;
		Coil.MaxOATDefrost = 0.0;
		Coil.DefrostStrategy = Resistive;
		Coil.DefrostControl = Timed;
		Coil.DefrostTime = 0.058333;
		Coil.DefrostCapacity = 1000;
		Coil.PLRImpact = false;
		Coil.FuelType = FuelTypeElectricity;
		Coil.RegionNum = 4;

		NumCurves = 5;
		PerfCurve.allocate( NumCurves );

		PerfomanceCurveData * pCurve;

		int const nCapfT2 = 1;
		pCurve = &PerfCurve( nCapfT2 );
		pCurve->CurveType = BiQuadratic;
		pCurve->Name = "HP_Heat-Cap-fT2";
		pCurve->Coeff1 = 0.95624428;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Coeff4 = 0.005999544;
		pCurve->Coeff5 = -0.0000900072;
		pCurve->Coeff6 = 0;
		pCurve->Var1Min = -100;
		pCurve->Var1Max = 100;
		pCurve->Var2Min = -100;
		pCurve->Var2Max = 100;

		Coil.CCapFTemp( 1 ) = nCapfT2;
		Coil.TotCapTempModFacCurveType( 1 ) = pCurve->CurveType;

		int const nCapfFF2 = 2;
		pCurve = &PerfCurve( nCapfFF2 );
		pCurve->CurveType = Quadratic;
		pCurve->Name = "HP_Heat-Cap-fFF2";
		pCurve->Coeff1 = 1;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Var1Min = 0;
		pCurve->Var1Max = 2;
		pCurve->CurveMin = 0;
		pCurve->CurveMax = 2;

		Coil.CCapFFlow( 1 ) = nCapfFF2;

		int const nEIRfT2 = 3;
		pCurve = &PerfCurve( nEIRfT2 );
		pCurve->CurveType = BiQuadratic;
		pCurve->Name = "HP_Heat-EIR-fT2";
		pCurve->Coeff1 = 1.065476178;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Coeff4 = -0.0085714308;
		pCurve->Coeff5 = 0.0000857142;
		pCurve->Coeff6 = 0;
		pCurve->Var1Min = -100;
		pCurve->Var1Max = 100;
		pCurve->Var2Min = -100;
		pCurve->Var2Max = 100;

		Coil.EIRFTemp( 1 ) = nEIRfT2;
		Coil.EIRTempModFacCurveType( 1 ) = pCurve->CurveType;

		int const nEIRfFF2 = 4;
		pCurve = &PerfCurve( nEIRfFF2 );
		pCurve->CurveType = Quadratic;
		pCurve->Name = "HP_Heat-EIR-fFF2";
		pCurve->Coeff1 = 1;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Var1Min = 0;
		pCurve->Var1Max = 2;
		pCurve->CurveMin = 0;
		pCurve->CurveMax = 2;

		Coil.EIRFFlow( 1 ) = nEIRfFF2;

		int const nPLFfPLR2 = 5;
		pCurve = &PerfCurve( nPLFfPLR2 );
		pCurve->CurveType = Quadratic;
		pCurve->Name = "HP_Heat-PLF-fPLR2";
		pCurve->Coeff1 = 1;
		pCurve->Coeff2 = 0;
		pCurve->Coeff3 = 0;
		pCurve->Var1Min = 0;
		pCurve->Var1Max = 1;
		pCurve->CurveMin = 0.7;
		pCurve->CurveMax = 1;

		Coil.PLFFPLR( 1 ) = nPLFfPLR2;

		for ( int CurveNum = 1; CurveNum <= NumCurves; ++CurveNum ) {
			PerfomanceCurveData & rCurve = PerfCurve( CurveNum );
			if ( rCurve.CurveType == BiQuadratic ) {
				rCurve.ObjectType = CurveType_BiQuadratic;
				rCurve.InterpolationType = EvaluateCurveToLimits;
			} else if ( rCurve.CurveType == Quadratic ) {
				rCurve.ObjectType = CurveType_Quadratic;
				rCurve.InterpolationType = EvaluateCurveToLimits;
			}
		}

		// Set up inlet air conditions.
		Coil.InletAirMassFlowRate = Coil.RatedAirMassFlowRate( 1 );
		OutHumRat = 0.002;
		OutBaroPress = 101325; // sea level
		Coil.InletAirTemp = 20;
		Coil.InletAirHumRat = 0.008;
		Coil.InletAirEnthalpy = PsyHFnTdbW( Coil.InletAirTemp, Coil.InletAirHumRat );

		int const FanOpMode = ContFanCycCoil;
		Real64 const PLR = 1.0;

		// Defrost Off
		OutDryBulbTemp = -5.0; // cold
		CalcDXHeatingCoil( DXCoilNum, PLR, FanOpMode );
		Real64 COPwoDefrost = Coil.RatedCOP( 1 ) / ( CurveValue( nEIRfT2, Coil.InletAirTemp, OutDryBulbTemp ) * CurveValue( nEIRfFF2, 1 ) );
		Real64 COPwDefrost = Coil.TotalHeatingEnergyRate / Coil.ElecHeatingPower;
		EXPECT_LT( COPwDefrost, COPwoDefrost );

		// Defrost On
		OutDryBulbTemp = 5.0; // not as cold
		CalcDXHeatingCoil( DXCoilNum, PLR, FanOpMode );
		COPwoDefrost = Coil.RatedCOP( 1 ) / ( CurveValue( nEIRfT2, Coil.InletAirTemp, OutDryBulbTemp ) * CurveValue( nEIRfFF2, 1 ) );
		COPwDefrost = Coil.TotalHeatingEnergyRate / Coil.ElecHeatingPower;
		EXPECT_DOUBLE_EQ( COPwoDefrost, COPwDefrost );

	}

	TEST_F( EnergyPlusFixture, TestCalcCBF ) {
		using DataEnvironment::StdPressureSeaLevel;
		const std::string CoilType( "Coil:WaterHeating:AirToWaterHeatPump:Wrapped" );
		const std::string CoilName( "The Coil" );
		const Real64 InletDBTemp( 19.722222222222221 );
		const Real64 InletWBTemp( 13.078173565729553 );
		Real64 InletAirHumRat;
		const Real64 TotalCap( 1303.5987246916557 );
		const Real64 AirVolFlowRate( 0.085422486640000003 );
		Real64 AirMassFlowRate;
		const Real64 SHR( 0.88 );
		Real64 AirPressure;
		Real64 CBF_expected;
		Real64 CBF_calculated;

		AirPressure = StdPressureSeaLevel;
		InletAirHumRat = Psychrometrics::PsyWFnTdbTwbPb(InletDBTemp, InletWBTemp, AirPressure );
		AirMassFlowRate = AirVolFlowRate * Psychrometrics::PsyRhoAirFnPbTdbW( AirPressure, InletDBTemp, InletAirHumRat );
		CBF_calculated = CalcCBF( CoilType, CoilName, InletDBTemp, InletAirHumRat, TotalCap, AirMassFlowRate, SHR, true, AirPressure );
		CBF_expected = 0.17268167698750708;
		EXPECT_DOUBLE_EQ( CBF_calculated, CBF_expected );
	}

	TEST_F( EnergyPlusFixture, DXCoilEvapCondPumpSizingTest ) {

		// tests autosizing evaporatively cooled condenser pump #4802

		std::string const idf_objects = delimited_string( {
			"Version,8.3;",
			"	Schedule:Compact,",
			"	FanAndCoilAvailSched, !- Name",
			"	Fraction,             !- Schedule Type Limits Name",
			"	Through: 12/31,       !- Field 1",
			"	For: AllDays,         !- Field 2",
			"	Until: 24:00, 1.0;    !- Field 3",
			"Curve:Biquadratic,",
			"	WindACCoolCapFT, !- Name",
			"	0.942587793,     !- Coefficient1 Constant",
			"	0.009543347,     !- Coefficient2 x",
			"	0.000683770,     !- Coefficient3 x**2",
			"	-0.011042676,    !- Coefficient4 y",
			"	0.000005249,     !- Coefficient5 y**2",
			"	-0.000009720,    !- Coefficient6 x*y",
			"	12.77778,        !- Minimum Value of x",
			"	23.88889,        !- Maximum Value of x",
			"	18.0,            !- Minimum Value of y",
			"	46.11111,        !- Maximum Value of y",
			"	,                !- Minimum Curve Output",
			"	,                !- Maximum Curve Output",
			"	Temperature,     !- Input Unit Type for X",
			"	Temperature,     !- Input Unit Type for Y",
			"	Dimensionless;   !- Output Unit Type",
			"Curve:Biquadratic,",
			"	WindACEIRFT,   !- Name",
			"	0.342414409,   !- Coefficient1 Constant",
			"	0.034885008,   !- Coefficient2 x",
			"	-0.000623700,  !- Coefficient3 x**2",
			"	0.004977216,   !- Coefficient4 y",
			"	0.000437951,   !- Coefficient5 y**2",
			"	-0.000728028,  !- Coefficient6 x*y",
			"	12.77778,      !- Minimum Value of x",
			"	23.88889,      !- Maximum Value of x",
			"	18.0,          !- Minimum Value of y",
			"	46.11111,      !- Maximum Value of y",
			"	,              !- Minimum Curve Output",
			"	,              !- Maximum Curve Output",
			"	Temperature,   !- Input Unit Type for X",
			"	Temperature,   !- Input Unit Type for Y",
			"	Dimensionless; !- Output Unit Type",
			"Curve:Quadratic,",
			"	WindACCoolCapFFF, !- Name",
			"	0.8,              !- Coefficient1 Constant",
			"	0.2,              !- Coefficient2 x",
			"	0.0,              !- Coefficient3 x**2",
			"	0.5,              !- Minimum Value of x",
			"	1.5;              !- Maximum Value of x",
			"Curve:Quadratic,",
			"	WindACEIRFFF, !- Name",
			"	1.1552,       !- Coefficient1 Constant",
			"  -0.1808,       !- Coefficient2 x",
			"	0.0256,       !- Coefficient3 x**2",
			"	0.5,          !- Minimum Value of x",
			"	1.5;          !- Maximum Value of x",
			"Curve:Quadratic,",
			"	WindACPLFFPLR, !- Name",
			"	0.85,          !- Coefficient1 Constant",
			"	0.15,          !- Coefficient2 x",
			"	0.0,           !- Coefficient3 x**2",
			"	0.0,           !- Minimum Value of x",
			"	1.0;           !- Maximum Value of x",
			"Coil:Cooling:DX:SingleSpeed,",
			"	Furnace ACDXCoil 1,   !- Name",
			" 	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	25000.0,              !- Gross Rated Total Cooling Capacity { W }",
			"	0.75,                 !- Gross Rated Sensible Heat Ratio",
			"	4.40,                 !- Gross Rated Cooling COP { W / W }",
			"	1.30,                 !- Rated Air Flow Rate { m3 / s }",
			"	,                     !- Rated Evaporator Fan Power Per Volume Flow Rate { W / ( m3 / s ) }",
			"	DX Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
			"	Heating Coil Air Inlet Node,    !- Air Outlet Node Name",
			"	WindACCoolCapFT,      !- Total Cooling Capacity Function of Temperature Curve Name",
			"	WindACCoolCapFFF,     !- Total Cooling Capacity Function of Flow Fraction Curve Name",
			"	WindACEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name",
			"	WindACEIRFFF,         !- Energy Input Ratio Function of Flow Fraction Curve Name",
			"	WindACPLFFPLR,        !- Part Load Fraction Correlation Curve Name",
			"	0.0,                  !- Nominal Time for Condensate Removal to Begin",
			"	0.0,                  !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity",
			"	0.0,                  !- Maximum Cycling Rate",
			"	0.0,                  !- Latent Capacity Time Constant",
			"	Split TSW Cooling Coil Condenser Inlet, !- Condenser Air Inlet Node Name",
			"	EvaporativelyCooled,  !- Condenser Type",
			"	0.0,                  !- Evaporative Condenser Effectiveness",
			"	,                     !- Evaporative Condenser Air Flow Rate",
			"	autosize,             !- Evaporative Condenser Pump Rated Power Consumption",
			"	0.0,                  !- Crankcase Heater Capacity",
			"	10.0;                 !- Maximum Outdoor DryBulb Temperature for Crankcase Heater Operation",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		ProcessScheduleInput();
		GetCurveInput();
		GetDXCoils();

		ASSERT_EQ( 1, NumDXCoils );
		EXPECT_EQ( DataSizing::AutoSize, DXCoil( 1 ).EvapCondPumpElecNomPower( 1 ) );

		SetPredefinedTables();

		SizeDXCoil( 1 );
		EXPECT_EQ( 25000.0, DXCoil( 1 ).RatedTotCap( 1 ) );
		EXPECT_EQ( DXCoil( 1 ).RatedTotCap( 1 ) * 0.004266, DXCoil( 1 ).EvapCondPumpElecNomPower( 1 ) );

		// clear
		DXCoil.deallocate();
	}

	TEST_F( EnergyPlusFixture, TestDXCoilIndoorOrOutdoor ) {

		//Test whether the coil is placed indoor or outdoor, by checking the air inlet node location

		using namespace DXCoils;
		using NodeInputManager::GetOnlySingleNode;
		using OutAirNodeManager::CheckOutAirNodeNumber;

		// Common Inputs
		int NumCoils; // total number of coils
		int DXCoilNum; // index to the current coil

		// Allocate
		NumCoils = 3;
		DXCoil.allocate( NumCoils );

		// IDF snippets
		std::string const idf_objects = delimited_string({
			"Version,8.3;                                          ",
			"OutdoorAir:Node,                                      ",
			"   Outside Air Inlet Node 1; !- Name                  ",
			"OutdoorAir:NodeList,                                  ",
			"   OutsideAirInletNodes;    !- Node or NodeList Name 1",
			"NodeList,                                             ",
			"   OutsideAirInletNodes,    !- Name                   ",
			"   Outside Air Inlet Node 2;!- Node 1 Name            ",
		});

		ASSERT_FALSE( process_idf( idf_objects ) );

		// Run
		DXCoilNum = 1;
		DXCoil(DXCoilNum).AirInNode = 1; // "Outside Air Inlet Node 1"
		DXCoil( DXCoilNum ).IsDXCoilInZone = ! CheckOutAirNodeNumber( DXCoil( DXCoilNum ).AirInNode );

		DXCoilNum = 2;
		DXCoil(DXCoilNum).AirInNode = 2; // "Outside Air Inlet Node 2"
		DXCoil( DXCoilNum ).IsDXCoilInZone = ! CheckOutAirNodeNumber( DXCoil( DXCoilNum ).AirInNode );

		DXCoilNum = 3; // "Inside Air Inlet Node"
		DXCoil( DXCoilNum ).IsDXCoilInZone = ! CheckOutAirNodeNumber( DXCoil( DXCoilNum ).AirInNode );

		// Check
		EXPECT_FALSE( DXCoil( 1 ).IsDXCoilInZone );
		EXPECT_FALSE( DXCoil( 2 ).IsDXCoilInZone );
		EXPECT_TRUE( DXCoil( 3 ).IsDXCoilInZone );

		// Clean up
		DXCoil.deallocate();
	}

	TEST_F( EnergyPlusFixture, TestMultiSpeedWasteHeat )
	{
		// Test the waste heat function #4536

		using Psychrometrics::PsyTwbFnTdbWPb;
		using Psychrometrics::PsyHFnTdbW;

		std::string const idf_objects = delimited_string( {
			"Version,8.3;",
			" Schedule:Compact,",
			"	FanAndCoilAvailSched, !- Name",
			"	Fraction,             !- Schedule Type Limits Name",
			"	Through: 12/31,       !- Field 1",
			"	For: AllDays,         !- Field 2",
			"	Until: 24:00, 1.0;    !- Field 3",
			" OutdoorAir:Node,",
			"	Outdoor Condenser Air Node, !- Name",
			"	1.0;                     !- Height Above Ground{ m }",
			" Coil:Cooling:DX:MultiSpeed,",
			"  Heat Pump ACDXCoil 1, !- Name",
			"  FanAndCoilAvailSched, !- Availability Schedule Name",
			"  DX Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
			"  Heating Coil Air Inlet Node, !- Air Outlet Node Name",
			"  Outdoor Condenser Air Node, !- Condenser Air Inlet Node Name",
			"  AirCooled, !- Condenser Type",
			"  , !- Supply Water Storage Tank Name",
			"  , !- Condensate Collection Water Storage Tank Name",
			"  No, !- Apply Part Load Fraction to Speeds Greater than 1",
			"  No, !- Apply Latent Degradation to Speeds Greater than 1",
			"  200.0, !- Crankcase Heater Capacity{ W }",
			"  10.0, !- Maximum Outdoor Dry - Bulb Temperature for Crankcase Heater Operation{ C }",
			"  , !- Basin Heater Capacity{ W / K }",
			"  , !- Basin Heater Setpoint Temperature{ C }",
			"  , !- Basin Heater Operating Schedule Name",
			"  Electricity, !- Fuel Type",
			"  4, !- Number of Speeds",
			"  7500, !- Speed 1 Gross Rated Total Cooling Capacity{ W }",
			"  0.75, !- Speed 1 Gross Rated Sensible Heat Ratio",
			"  3.0, !- Speed 1 Gross Rated Cooling COP{ W / W }",
			"  0.40, !- Speed 1 Rated Air Flow Rate{ m3 / s }",
			"  453.3, !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
			"  HPACCoolCapFT Speed, !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
			"  HPACCoolCapFF Speed, !- Speed 1 Total Cooling Capacity Function of Flow Fraction Curve Name",
			"  HPACCOOLEIRFT Speed, !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
			"  HPACCOOLEIRFF Speed, !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
			"  HPACCOOLPLFFPLR Speed, !- Speed 1 Part Load Fraction Correlation Curve Name",
			"  1000.0, !- Speed 1 Nominal Time for Condensate Removal to Begin{ s }",
			"  1.5, !- Speed 1 Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity{ dimensionless }",
			"  3.0, !- Speed 1 Maximum Cycling Rate{ cycles / hr }",
			"  45.0, !- Speed 1 Latent Capacity Time Constant{ s }",
			"  0.2, !- Speed 1 Rated Waste Heat Fraction of Power Input{ dimensionless }",
			"  , !- Speed 1 Waste Heat Function of Temperature Curve Name",
			"  0.9, !- Speed 1 Evaporative Condenser Effectiveness{ dimensionless }",
			"  0.05, !- Speed 1 Evaporative Condenser Air Flow Rate{ m3 / s }",
			"  50, !- Speed 1 Rated Evaporative Condenser Pump Power Consumption{ W }",
			"  17500, !- Speed 2 Gross Rated Total Cooling Capacity{ W }",
			"  0.75, !- Speed 2 Gross Rated Sensible Heat Ratio",
			"  3.0, !- Speed 2 Gross Rated Cooling COP{ W / W }",
			"  0.85, !- Speed 2 Rated Air Flow Rate{ m3 / s }",
			"  523.3, !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
			"  HPACCoolCapFT Speed, !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
			"  HPACCoolCapFF Speed, !- Speed 2 Total Cooling Capacity Function of Flow Fraction Curve Name",
			"  HPACCOOLEIRFT Speed, !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
			"  HPACCOOLEIRFF Speed, !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
			"  HPACCOOLPLFFPLR Speed, !- Speed 2 Part Load Fraction Correlation Curve Name",
			"  1000.0, !- Speed 2 Nominal Time for Condensate Removal to Begin{ s }",
			"  1.5, !- Speed 2 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
			"  3.0, !- Speed 2 Maximum Cycling Rate{ cycles / hr }",
			"  45.0, !- Speed 2 Latent Capacity Time Constant{ s }",
			"  0.2, !- Speed 2 Rated Waste Heat Fraction of Power Input{ dimensionless }",
			"  , !- Speed 2 Waste Heat Function of Temperature Curve Name",
			"  0.9, !- Speed 2 Evaporative Condenser Effectiveness{ dimensionless }",
			"  0.1, !- Speed 2 Evaporative Condenser Air Flow Rate{ m3 / s }",
			"  60, !- Speed 2 Rated Evaporative Condenser Pump Power Consumption{ W }",
			"  25500, !- Speed 3 Gross Rated Total Cooling Capacity{ W }",
			"  0.75, !- Speed 3 Gross Rated Sensible Heat Ratio",
			"  3.0, !- Speed 3 Gross Rated Cooling COP{ W / W }",
			"  1.25, !- Speed 3 Rated Air Flow Rate{ m3 / s }",
			"  573.3, !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
			"  HPACCoolCapFT Speed, !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
			"  HPACCoolCapFF Speed, !- Speed 3 Total Cooling Capacity Function of Flow Fraction Curve Name",
			"  HPACCOOLEIRFT Speed, !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
			"  HPACCOOLEIRFF Speed, !- Speed 3 Energy Input Ratio Function of Flow Fraction Curve Name",
			"  HPACCOOLPLFFPLR Speed, !- Speed 3 Part Load Fraction Correlation Curve Name",
			"  1000.0, !- Speed 3 Nominal Time for Condensate Removal to Begin{ s }",
			"  1.5, !- Speed 3 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
			"  3.0, !- Speed 3 Maximum Cycling Rate{ cycles / hr }",
			"  45.0, !- Speed 3 Latent Capacity Time Constant{ s }",
			"  0.2, !- Speed 3 Rated Waste Heat Fraction of Power Input{ dimensionless }",
			"  , !- Speed 3 Waste Heat Function of Temperature Curve Name",
			"  0.9, !- Speed 3 Evaporative Condenser Effectiveness{ dimensionless }",
			"  0.2, !- Speed 3 Evaporative Condenser Air Flow Rate{ m3 / s }",
			"  80, !- Speed 3 Rated Evaporative Condenser Pump Power Consumption{ W }",
			"  35500, !- Speed 4 Gross Rated Total Cooling Capacity{ W }",
			"  0.75, !- Speed 4 Gross Rated Sensible Heat Ratio",
			"  3.0, !- Speed 4 Gross Rated Cooling COP{ W / W }",
			"  1.75, !- Speed 4 Rated Air Flow Rate{ m3 / s }",
			"  673.3, !- Rated Evaporator Fan Power Per Volume Flow Rate{ W / ( m3 / s ) }",
			"  HPACCoolCapFT Speed, !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
			"  HPACCoolCapFF Speed, !- Speed 4 Total Cooling Capacity Function of Flow Fraction Curve Name",
			"  HPACCOOLEIRFT Speed, !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
			"  HPACCOOLEIRFF Speed, !- Speed 4 Energy Input Ratio Function of Flow Fraction Curve Name",
			"  HPACCOOLPLFFPLR Speed, !- Speed 4 Part Load Fraction Correlation Curve Name",
			"  1000.0, !- Speed 4 Nominal Time for Condensate Removal to Begin{ s }",
			"  1.5, !- Speed 4 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity{ dimensionless }",
			"  3.0, !- Speed 4 Maximum Cycling Rate{ cycles / hr }",
			"  45.0, !- Speed 4 Latent Capacity Time Constant{ s }",
			"  0.2, !- Speed 4 Rated Waste Heat Fraction of Power Input{ dimensionless }",
			"  , !- Speed 4 Waste Heat Function of Temperature Curve Name",
			"  0.9, !- Speed 4 Evaporative Condenser Effectiveness{ dimensionless }",
			"  0.3, !- Speed 4 Evaporative Condenser Air Flow Rate{ m3 / s }",
			" 100;                     !- Speed 4 Rated Evaporative Condenser Pump Power Consumption{ W }",
			" Curve:Biquadratic,",
			"  HPACCoolCapFT Speed, !- Name",
			"  1.0, !- Coefficient1 Constant",
			"  0.0, !- Coefficient2 x",
			"  0.0, !- Coefficient3 x**2",
			"  0.0, !- Coefficient4 y",
			"  0.0, !- Coefficient5 y**2",
			"  0.0, !- Coefficient6 x*y",
			"  12.77778, !- Minimum Value of x",
			"  23.88889, !- Maximum Value of x",
			"  23.88889, !- Minimum Value of y",
			"  46.11111, !- Maximum Value of y",
			"  , !- Minimum Curve Output",
			"  , !- Maximum Curve Output",
			"  Temperature, !- Input Unit Type for X",
			"  Temperature, !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			" Curve:Cubic,",
			"  HPACCoolCapFF Speed, !- Name",
			"  .47278589, !- Coefficient1 Constant",
			"  1.2433415, !- Coefficient2 x",
			"  -1.0387055, !- Coefficient3 x**2",
			"  .32257813, !- Coefficient4 x**3",
			"  0.5, !- Minimum Value of x",
			"  1.5;                   !- Maximum Value of x",
			" Curve:Biquadratic,",
			"  HPACCOOLEIRFT Speed, !- Name",
			"  0.632475E+00, !- Coefficient1 Constant",
			"  -0.121321E-01, !- Coefficient2 x",
			"  0.507773E-03, !- Coefficient3 x**2",
			"  0.155377E-01, !- Coefficient4 y",
			"  0.272840E-03, !- Coefficient5 y**2",
			"  -0.679201E-03, !- Coefficient6 x*y",
			"  12.77778, !- Minimum Value of x",
			"  23.88889, !- Maximum Value of x",
			"  23.88889, !- Minimum Value of y",
			"  46.11111, !- Maximum Value of y",
			"  , !- Minimum Curve Output",
			"  , !- Maximum Curve Output",
			"  Temperature, !- Input Unit Type for X",
			"  Temperature, !- Input Unit Type for Y",
			"  Dimensionless;           !- Output Unit Type",
			"Curve:Cubic,",
			"  HPACCOOLEIRFF Speed, !- Name",
			"  .47278589, !- Coefficient1 Constant",
			"  1.2433415, !- Coefficient2 x",
			"  -1.0387055, !- Coefficient3 x**2",
			"  .32257813, !- Coefficient4 x**3",
			"  0.5, !- Minimum Value of x",
			"  1.5;                     !- Maximum Value of x",
			"Curve:Quadratic,",
			"  HPACCOOLPLFFPLR Speed, !- Name",
			"  0.85, !- Coefficient1 Constant",
			"  0.15, !- Coefficient2 x",
			"  0.0, !- Coefficient3 x**2",
			"  0.0, !- Minimum Value of x",
			"  1.0;                     !- Maximum Value of x",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		// Case 1 test
		GetDXCoils();

		EXPECT_EQ( FuelTypeElectricity, DXCoil( 1 ).FuelType );
		EXPECT_EQ( 0, DXCoil( 1 ).MSWasteHeat( 2 ) );

		// Test calculations of the waste heat function #5162

		// Case 2 test waste heat is zero when the parent has not heat recovery inputs
		DXCoil( 1 ).FuelType = FuelTypeNaturalGas;
		DXCoil( 1 ).MSHPHeatRecActive = false;

		OutDryBulbTemp = 35;
		OutHumRat = 0.0128;
		OutBaroPress = 101325;
		OutWetBulbTemp = PsyTwbFnTdbWPb( OutDryBulbTemp, OutHumRat, OutBaroPress);

		DXCoil( 1 ).MSRatedAirMassFlowRate( 1 ) = DXCoil( 1 ).MSRatedAirVolFlowRate( 1 ) * 1.2;
		DXCoil( 1 ).MSRatedAirMassFlowRate( 2 ) = DXCoil( 1 ).MSRatedAirVolFlowRate( 2 ) * 1.2;
		DXCoil( 1 ).InletAirMassFlowRate = DXCoil( 1 ).MSRatedAirMassFlowRate( 2 );
		MSHPMassFlowRateLow = DXCoil( 1 ).MSRatedAirMassFlowRate( 1 );
		MSHPMassFlowRateHigh = DXCoil( 1 ).MSRatedAirMassFlowRate( 2 );

		DXCoil( 1 ).InletAirTemp = 25.0;
		DXCoil( 1 ).InletAirHumRat = 0.005;
		DXCoil( 1 ).InletAirEnthalpy = PsyHFnTdbW( 25.0, 0.005 );

		DXCoil( 1 ).SchedPtr = 1;
		Schedule( DXCoil( 1 ).SchedPtr ).CurrentValue = 1.0; // enable the VRF condenser
		DXCoil( 1 ).MSRatedCBF( 1 ) = 0.1262;
		DXCoil( 1 ).MSRatedCBF( 2 ) = 0.0408;

		CalcMultiSpeedDXCoilCooling( 1, 1, 1, 2, 1, 1, 0 );

		EXPECT_EQ( 0, MSHPWasteHeat );

		// Case 3 heat recovery is true and no waste heat function cuvre
		DXCoil( 1 ).MSWasteHeat( 1 ) = 0;
		DXCoil( 1 ).MSWasteHeat( 2 ) = 0;
		DXCoil( 1 ).MSHPHeatRecActive = true;

		CalcMultiSpeedDXCoilCooling( 1, 1, 1, 2, 1, 1, 0 );

		EXPECT_NEAR( 1303.4304, MSHPWasteHeat, 0.001 );

		// clear
		DXCoil.deallocate();

	}

	TEST_F( EnergyPlusFixture, DXCoil_ValidateADPFunction ) {

		using Psychrometrics::PsyRhoAirFnPbTdbW;

		// tests autosizing DX coil SHR #4853

		std::string const idf_objects = delimited_string( {
			"	Schedule:Compact,",
			"	FanAndCoilAvailSched, !- Name",
			"	Fraction,             !- Schedule Type Limits Name",
			"	Through: 12/31,       !- Field 1",
			"	For: AllDays,         !- Field 2",
			"	Until: 24:00, 1.0;    !- Field 3",
			"Curve:Biquadratic,",
			"	WindACCoolCapFT, !- Name",
			"	0.942587793,     !- Coefficient1 Constant",
			"	0.009543347,     !- Coefficient2 x",
			"	0.000683770,     !- Coefficient3 x**2",
			"	-0.011042676,    !- Coefficient4 y",
			"	0.000005249,     !- Coefficient5 y**2",
			"	-0.000009720,    !- Coefficient6 x*y",
			"	12.77778,        !- Minimum Value of x",
			"	23.88889,        !- Maximum Value of x",
			"	18.0,            !- Minimum Value of y",
			"	46.11111,        !- Maximum Value of y",
			"	,                !- Minimum Curve Output",
			"	,                !- Maximum Curve Output",
			"	Temperature,     !- Input Unit Type for X",
			"	Temperature,     !- Input Unit Type for Y",
			"	Dimensionless;   !- Output Unit Type",
			"Curve:Biquadratic,",
			"	WindACEIRFT,   !- Name",
			"	0.342414409,   !- Coefficient1 Constant",
			"	0.034885008,   !- Coefficient2 x",
			"	-0.000623700,  !- Coefficient3 x**2",
			"	0.004977216,   !- Coefficient4 y",
			"	0.000437951,   !- Coefficient5 y**2",
			"	-0.000728028,  !- Coefficient6 x*y",
			"	12.77778,      !- Minimum Value of x",
			"	23.88889,      !- Maximum Value of x",
			"	18.0,          !- Minimum Value of y",
			"	46.11111,      !- Maximum Value of y",
			"	,              !- Minimum Curve Output",
			"	,              !- Maximum Curve Output",
			"	Temperature,   !- Input Unit Type for X",
			"	Temperature,   !- Input Unit Type for Y",
			"	Dimensionless; !- Output Unit Type",
			"Curve:Quadratic,",
			"	WindACCoolCapFFF, !- Name",
			"	0.8,              !- Coefficient1 Constant",
			"	0.2,              !- Coefficient2 x",
			"	0.0,              !- Coefficient3 x**2",
			"	0.5,              !- Minimum Value of x",
			"	1.5;              !- Maximum Value of x",
			"Curve:Quadratic,",
			"	WindACEIRFFF, !- Name",
			"	1.1552,       !- Coefficient1 Constant",
			"  -0.1808,       !- Coefficient2 x",
			"	0.0256,       !- Coefficient3 x**2",
			"	0.5,          !- Minimum Value of x",
			"	1.5;          !- Maximum Value of x",
			"Curve:Quadratic,",
			"	WindACPLFFPLR, !- Name",
			"	0.85,          !- Coefficient1 Constant",
			"	0.15,          !- Coefficient2 x",
			"	0.0,           !- Coefficient3 x**2",
			"	0.0,           !- Minimum Value of x",
			"	1.0;           !- Maximum Value of x",
			"Coil:Cooling:DX:SingleSpeed,",
			"	Furnace ACDXCoil 1,   !- Name",
			" 	FanAndCoilAvailSched, !- Availability Schedule Name",
			"	25000.0,              !- Gross Rated Total Cooling Capacity { W }",
			"	autosize,             !- Gross Rated Sensible Heat Ratio",
			"	4.40,                 !- Gross Rated Cooling COP { W / W }",
			"	1.30,                 !- Rated Air Flow Rate { m3 / s }",
			"	,                     !- Rated Evaporator Fan Power Per Volume Flow Rate { W / ( m3 / s ) }",
			"	DX Cooling Coil Air Inlet Node, !- Air Inlet Node Name",
			"	Heating Coil Air Inlet Node,    !- Air Outlet Node Name",
			"	WindACCoolCapFT,      !- Total Cooling Capacity Function of Temperature Curve Name",
			"	WindACCoolCapFFF,     !- Total Cooling Capacity Function of Flow Fraction Curve Name",
			"	WindACEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name",
			"	WindACEIRFFF,         !- Energy Input Ratio Function of Flow Fraction Curve Name",
			"	WindACPLFFPLR,        !- Part Load Fraction Correlation Curve Name",
			"	0.0,                  !- Nominal Time for Condensate Removal to Begin",
			"	0.0,                  !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity",
			"	0.0,                  !- Maximum Cycling Rate",
			"	0.0,                  !- Latent Capacity Time Constant",
			"	Split TSW Cooling Coil Condenser Inlet, !- Condenser Air Inlet Node Name",
			"	EvaporativelyCooled,  !- Condenser Type",
			"	0.0,                  !- Evaporative Condenser Effectiveness",
			"	,                     !- Evaporative Condenser Air Flow Rate",
			"	autosize,             !- Evaporative Condenser Pump Rated Power Consumption",
			"	0.0,                  !- Crankcase Heater Capacity",
			"	10.0;                 !- Maximum Outdoor DryBulb Temperature for Crankcase Heater Operation",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		ProcessScheduleInput();
		GetCurveInput();
		GetDXCoils();
		SetPredefinedTables();
		CurZoneEqNum = 1;

		// Need this to prevent crash in RequestSizing
		FinalZoneSizing.allocate( 1 );
		FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 0.1;
		FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 0.1;
		DataFlowUsedForSizing = 0.1;
		ZoneEqSizing.allocate( 1 );
		ZoneEqSizing( CurZoneEqNum ).CoolingCapacity = true;
		ZoneEqSizing( CurZoneEqNum ).DesCoolingLoad = DXCoil( 1 ).RatedTotCap( 1 );
		ZoneEqSizing( CurZoneEqNum ).DesignSizeFromParent = false;
		ZoneEqSizing( CurZoneEqNum ).SizingMethod.allocate( 25 );
		ZoneEqSizing( CurZoneEqNum ).SizingMethod( DataHVACGlobals::SystemAirflowSizing ) = DataSizing::SupplyAirFlowRate;
		ZoneSizingInput.allocate( 1 );
		ZoneSizingInput( 1 ).ZoneNum = 1;
		DataSizing::NumZoneSizingInput = 1;
		ZoneSizingRunDone = true;
		StdBaroPress = 101325.0;

		SizeDXCoil( 1 ); // normal sizing

		Real64 const RatedInletAirTemp( 26.6667 ); // 26.6667C or 80F
		Real64 const RatedInletAirHumRat( 0.01125 ); // Humidity ratio corresponding to 80F dry bulb/67F wet bulb
		std::string const CallingRoutine( "DXCoil_ValidateADPFunction" );

		Real64 DesMassFlow = DXCoil( 1 ).RatedAirVolFlowRate( 1 ) * PsyRhoAirFnPbTdbW( StdBaroPress, RatedInletAirTemp, RatedInletAirHumRat, CallingRoutine );
		Real64 CBF_calculated = CalcCBF( DXCoil( 1 ).DXCoilType, DXCoil( 1 ).Name, RatedInletAirTemp, RatedInletAirHumRat, DXCoil( 1 ).RatedTotCap( 1 ), DesMassFlow, DXCoil( 1 ).RatedSHR( 1 ), true );

		EXPECT_NEAR( 0.747472, DXCoil( 1 ).RatedSHR( 1 ), 0.0000001 );
		EXPECT_NEAR( 0.1012203, CBF_calculated, 0.0000001 );

		DXCoil( 1 ).RatedTotCap( 1 ) = 35000.0; // run right at the saturation curve
		DXCoil( 1 ).RatedSHR( 1 ) = AutoSize;

		SizeDXCoil( 1 );
		CBF_calculated = CalcCBF( DXCoil( 1 ).DXCoilType, DXCoil( 1 ).Name, RatedInletAirTemp, RatedInletAirHumRat, DXCoil( 1 ).RatedTotCap( 1 ), DesMassFlow, DXCoil( 1 ).RatedSHR( 1 ), true );

		EXPECT_NEAR( 0.67608322, DXCoil( 1 ).RatedSHR( 1 ), 0.0000001 );
		EXPECT_NEAR( 0.0003243, CBF_calculated, 0.0000001 );

		DXCoil( 1 ).RatedTotCap( 1 ) = 40000.0; // reverse perturb SHR (i.e., decrease SHR), CalcCBF would have failed with RH >= 1.0
		DXCoil( 1 ).RatedSHR( 1 ) = AutoSize;

		SizeDXCoil( 1 );
		CBF_calculated = CalcCBF( DXCoil( 1 ).DXCoilType, DXCoil( 1 ).Name, RatedInletAirTemp, RatedInletAirHumRat, DXCoil( 1 ).RatedTotCap( 1 ), DesMassFlow, DXCoil( 1 ).RatedSHR( 1 ), true );

		EXPECT_NEAR( 0.64408322, DXCoil( 1 ).RatedSHR( 1 ), 0.0000001 );
		EXPECT_NEAR( 0.0028271, CBF_calculated, 0.0000001 );

	}

}
