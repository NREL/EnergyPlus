// EnergyPlus::ExteriorEnergyUse Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::SizingManager;
using namespace EnergyPlus::DataSizing;
using namespace ObjexxFCL;

TEST( GetOARequirementsTest, DSOA1 )
{
	ShowMessage( "Begin Test: GetOARequirementsTest, DSOA1" );

	static bool ErrorsFound( false ); // If errors detected in input
	static int OAIndex( 0 ); // Zone number
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
