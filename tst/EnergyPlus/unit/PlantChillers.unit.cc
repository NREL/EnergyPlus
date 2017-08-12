// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
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

// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/PlantChillers.hh>
#include <DataPlant.hh>
#include <DataSizing.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;

TEST_F( EnergyPlusFixture, GTChiller_HeatRecoveryAutosizeTest )
{
	// unit test for autosizing heat recovery in Chiller:CombustionTurbine
	PlantChillers::GTChiller.allocate( 1 );

	PlantChillers::GTChiller( 1 ).Base.SizFac = 1.0;
	PlantChillers::GTChiller( 1 ).DesignHeatRecVolFlowRateWasAutoSized = true;
	PlantChillers::GTChiller( 1 ).HeatRecCapacityFraction = 0.5;
	PlantChillers::GTChiller( 1 ).HeatRecActive = true;
	PlantChillers::GTChiller( 1 ).Base.CondenserType = PlantChillers::WaterCooled;
	PlantChillers::GTChiller( 1 ).Base.CWLoopNum = 1;
	PlantChillers::GTChiller( 1 ).Base.CDLoopNum = 2;
	PlantChillers::GTChiller( 1 ).Base.EvapVolFlowRate = 1.0;
	PlantChillers::GTChiller( 1 ).Base.CondVolFlowRate = 1.0;
	PlantChillers::GTChiller( 1 ).Base.NomCap = 10000;
	PlantChillers::GTChiller( 1 ).Base.COP = 3.0;
	PlantChillers::GTChiller( 1 ).engineCapacityScalar = 1.0;

	DataPlant::PlantLoop.allocate( 2 );
	DataSizing::PlantSizData.allocate( 1 );
	DataPlant::PlantLoop( 1 ).PlantSizNum = 1;
	DataPlant::PlantLoop( 1 ).FluidIndex = 1;
	DataPlant::PlantLoop( 1 ).FluidName = "WATER";
	DataSizing::PlantSizData( 1 ).DesVolFlowRate = 1.0;
	DataSizing::PlantSizData( 1 ).DeltaT = 5.0;
	DataPlant::PlantFirstSizesOkayToFinalize = true;

	//now call sizing routine
	PlantChillers::SizeGTChiller( 1 );
	// see if heat recovery flow rate is as expected
	EXPECT_NEAR( PlantChillers::GTChiller( 1 ).DesignHeatRecVolFlowRate, 0.5, 0.00001 );

	PlantChillers::GTChiller.deallocate();
	DataSizing::PlantSizData.deallocate();
	DataPlant::PlantLoop.deallocate();

}

TEST_F( EnergyPlusFixture, EngineDrivenChiller_HeatRecoveryAutosizeTest )
{
	// unit test for autosizing heat recovery in Chiller:EngineDriven
	PlantChillers::EngineDrivenChiller.allocate( 1 );

	PlantChillers::EngineDrivenChiller( 1 ).Base.SizFac = 1.0;
	PlantChillers::EngineDrivenChiller( 1 ).DesignHeatRecVolFlowRateWasAutoSized = true;
	PlantChillers::EngineDrivenChiller( 1 ).HeatRecCapacityFraction = 0.5;
	PlantChillers::EngineDrivenChiller( 1 ).HeatRecActive = true;
	PlantChillers::EngineDrivenChiller( 1 ).Base.CondenserType = PlantChillers::WaterCooled;
	PlantChillers::EngineDrivenChiller( 1 ).Base.CWLoopNum = 1;
	PlantChillers::EngineDrivenChiller( 1 ).Base.CDLoopNum = 2;
	PlantChillers::EngineDrivenChiller( 1 ).Base.EvapVolFlowRate = 1.0;
	PlantChillers::EngineDrivenChiller( 1 ).Base.CondVolFlowRate = 1.0;
	PlantChillers::EngineDrivenChiller( 1 ).Base.NomCap = 10000;
	PlantChillers::EngineDrivenChiller( 1 ).Base.COP = 3.0;
		
	DataPlant::PlantLoop.allocate( 2 );
	DataSizing::PlantSizData.allocate( 1 );
	DataPlant::PlantLoop( 1 ).PlantSizNum = 1;
	DataPlant::PlantLoop( 1 ).FluidIndex = 1;
	DataPlant::PlantLoop( 1 ).FluidName = "WATER";
	DataSizing::PlantSizData( 1 ).DesVolFlowRate = 1.0;
	DataSizing::PlantSizData( 1 ).DeltaT = 5.0;
	DataPlant::PlantFirstSizesOkayToFinalize = true;

	//now call sizing routine
	PlantChillers::SizeEngineDrivenChiller( 1 );
	// see if heat recovery flow rate is as expected
	EXPECT_NEAR( PlantChillers::EngineDrivenChiller( 1 ).DesignHeatRecVolFlowRate, 0.5, 0.00001 );

	PlantChillers::EngineDrivenChiller.deallocate();
	DataSizing::PlantSizData.deallocate();
	DataPlant::PlantLoop.deallocate();

}

