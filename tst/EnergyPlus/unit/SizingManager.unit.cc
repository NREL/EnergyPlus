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

// EnergyPlus::ExteriorEnergyUse Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::SizingManager;
using namespace EnergyPlus::DataSizing;
using namespace ObjexxFCL;

TEST_F( EnergyPlusFixture, GetOARequirementsTest_DSOA1 )
{
	bool ErrorsFound( false ); // If errors detected in input
	int OAIndex( 0 ); // Zone number
	int NumAlphas( 2 );
	int NumNumbers( 4 );

	std::string CurrentModuleObject = "DesignSpecification:OutdoorAir";
	int NumOARequirements = 6;
	OARequirements.allocate( NumOARequirements );

	Array1D_string Alphas; // Alpha input items for object
	Array1D_string cAlphaFields; // Alpha field names
	Array1D_string cNumericFields; // Numeric field names
	Array1D< Real64 > Numbers; // Numeric input items for object
	Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
	Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.

	Alphas.allocate( NumAlphas );
	cAlphaFields.allocate( NumAlphas );
	cNumericFields.allocate( NumNumbers );
	Numbers.dimension( NumNumbers, 0.0 );
	lAlphaBlanks.dimension( NumAlphas, true );
	lNumericBlanks.dimension( NumNumbers, true );

	// Flow/Area
	OAIndex = 1;
	Alphas( 1 ) = "Test DSOA 1"; // Name
	Alphas( 2 ) = "Flow/Area";   // Outdoor Air Method
	Numbers( 1 ) = 0.1;          // Outdoor Air Flow per Person{ m3 / s - person }
	Numbers( 2 ) = 0.2;          // Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }
	Numbers( 3 ) = 0.3;          // Outdoor Air Flow per Zone{ m3 / s }
	Numbers( 4 ) = 0.4;        	 //Outdoor Air Flow Air Changes per Hour

	ErrorsFound = false;
	ProcessInputOARequirements( CurrentModuleObject, OAIndex, Alphas, NumAlphas, Numbers, NumNumbers, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields, ErrorsFound );

	EXPECT_FALSE( ErrorsFound );

	EXPECT_EQ( OAFlowPerArea, OARequirements( OAIndex ).OAFlowMethod );
	EXPECT_EQ( 0.0,           OARequirements( OAIndex ).OAFlowPerPerson );
	EXPECT_EQ( 0.2,           OARequirements( OAIndex ).OAFlowPerArea );
	EXPECT_EQ( 0.0,           OARequirements( OAIndex ).OAFlowPerZone );
	EXPECT_EQ( 0.0,           OARequirements( OAIndex ).OAFlowACH );

	// Flow/Person
	OAIndex = 2;
	Alphas( 1 ) = "Test DSOA 2"; // Name
	Alphas( 2 ) = "Flow/Person";   // Outdoor Air Method
	Numbers( 1 ) = 0.1;          // Outdoor Air Flow per Person{ m3 / s - person }
	Numbers( 2 ) = 0.2;          // Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }
	Numbers( 3 ) = 0.3;          // Outdoor Air Flow per Zone{ m3 / s }
	Numbers( 4 ) = 0.4;        	 //Outdoor Air Flow Air Changes per Hour

	ErrorsFound = false;
	ProcessInputOARequirements( CurrentModuleObject, OAIndex, Alphas, NumAlphas, Numbers, NumNumbers, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields, ErrorsFound );

	EXPECT_FALSE( ErrorsFound );

	EXPECT_EQ( OAFlowPPer, OARequirements( OAIndex ).OAFlowMethod );
	EXPECT_EQ( 0.1, OARequirements( OAIndex ).OAFlowPerPerson );
	EXPECT_EQ( 0.0, OARequirements( OAIndex ).OAFlowPerArea );
	EXPECT_EQ( 0.0, OARequirements( OAIndex ).OAFlowPerZone );
	EXPECT_EQ( 0.0, OARequirements( OAIndex ).OAFlowACH );

	// Flow/Zone
	OAIndex = 3;
	Alphas( 1 ) = "Test DSOA 3"; // Name
	Alphas( 2 ) = "Flow/Zone";   // Outdoor Air Method
	Numbers( 1 ) = 0.1;          // Outdoor Air Flow per Person{ m3 / s - person }
	Numbers( 2 ) = 0.2;          // Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }
	Numbers( 3 ) = 0.3;          // Outdoor Air Flow per Zone{ m3 / s }
	Numbers( 4 ) = 0.4;        	 //Outdoor Air Flow Air Changes per Hour

	ErrorsFound = false;
	ProcessInputOARequirements( CurrentModuleObject, OAIndex, Alphas, NumAlphas, Numbers, NumNumbers, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields, ErrorsFound );

	EXPECT_FALSE( ErrorsFound );

	EXPECT_EQ( OAFlow, OARequirements( OAIndex ).OAFlowMethod );
	EXPECT_EQ( 0.0, OARequirements( OAIndex ).OAFlowPerPerson );
	EXPECT_EQ( 0.0, OARequirements( OAIndex ).OAFlowPerArea );
	EXPECT_EQ( 0.3, OARequirements( OAIndex ).OAFlowPerZone );
	EXPECT_EQ( 0.0, OARequirements( OAIndex ).OAFlowACH );

	// Flow/Zone
	OAIndex = 4;
	Alphas( 1 ) = "Test DSOA 4"; // Name
	Alphas( 2 ) = "AirChanges/Hour";   // Outdoor Air Method
	Numbers( 1 ) = 0.1;          // Outdoor Air Flow per Person{ m3 / s - person }
	Numbers( 2 ) = 0.2;          // Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }
	Numbers( 3 ) = 0.3;          // Outdoor Air Flow per Zone{ m3 / s }
	Numbers( 4 ) = 0.4;        	 //Outdoor Air Flow Air Changes per Hour

	ErrorsFound = false;
	ProcessInputOARequirements( CurrentModuleObject, OAIndex, Alphas, NumAlphas, Numbers, NumNumbers, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields, ErrorsFound );

	EXPECT_FALSE( ErrorsFound );

	EXPECT_EQ( OAFlowACH, OARequirements( OAIndex ).OAFlowMethod );
	EXPECT_EQ( 0.0, OARequirements( OAIndex ).OAFlowPerPerson );
	EXPECT_EQ( 0.0, OARequirements( OAIndex ).OAFlowPerArea );
	EXPECT_EQ( 0.0, OARequirements( OAIndex ).OAFlowPerZone );
	EXPECT_EQ( 0.4, OARequirements( OAIndex ).OAFlowACH );

	// Sum
	OAIndex = 5;
	Alphas( 1 ) = "Test DSOA 5"; // Name
	Alphas( 2 ) = "Sum";   // Outdoor Air Method
	Numbers( 1 ) = 0.1;          // Outdoor Air Flow per Person{ m3 / s - person }
	Numbers( 2 ) = 0.2;          // Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }
	Numbers( 3 ) = 0.3;          // Outdoor Air Flow per Zone{ m3 / s }
	Numbers( 4 ) = 0.4;        	 //Outdoor Air Flow Air Changes per Hour

	ErrorsFound = false;
	ProcessInputOARequirements( CurrentModuleObject, OAIndex, Alphas, NumAlphas, Numbers, NumNumbers, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields, ErrorsFound );

	EXPECT_FALSE( ErrorsFound );

	EXPECT_EQ( OAFlowSum, OARequirements( OAIndex ).OAFlowMethod );
	EXPECT_EQ( 0.1, OARequirements( OAIndex ).OAFlowPerPerson );
	EXPECT_EQ( 0.2, OARequirements( OAIndex ).OAFlowPerArea );
	EXPECT_EQ( 0.3, OARequirements( OAIndex ).OAFlowPerZone );
	EXPECT_EQ( 0.4, OARequirements( OAIndex ).OAFlowACH );

	// Maximum
	OAIndex = 6;
	Alphas( 1 ) = "Test DSOA 6"; // Name
	Alphas( 2 ) = "Maximum";   // Outdoor Air Method
	Numbers( 1 ) = 0.1;          // Outdoor Air Flow per Person{ m3 / s - person }
	Numbers( 2 ) = 0.2;          // Outdoor Air Flow per Zone Floor Area{ m3 / s - m2 }
	Numbers( 3 ) = 0.3;          // Outdoor Air Flow per Zone{ m3 / s }
	Numbers( 4 ) = 0.4;        	 //Outdoor Air Flow Air Changes per Hour

	ErrorsFound = false;
	ProcessInputOARequirements( CurrentModuleObject, OAIndex, Alphas, NumAlphas, Numbers, NumNumbers, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields, ErrorsFound );

	EXPECT_FALSE( ErrorsFound );

	EXPECT_EQ( OAFlowMax, OARequirements( OAIndex ).OAFlowMethod );
	EXPECT_EQ( 0.1, OARequirements( OAIndex ).OAFlowPerPerson );
	EXPECT_EQ( 0.2, OARequirements( OAIndex ).OAFlowPerArea );
	EXPECT_EQ( 0.3, OARequirements( OAIndex ).OAFlowPerZone );
	EXPECT_EQ( 0.4, OARequirements( OAIndex ).OAFlowACH );

	// Clean up
	OARequirements.deallocate();
	Alphas.deallocate();
	cAlphaFields.deallocate();
	cNumericFields.deallocate();

}
