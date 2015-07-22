// EnergyPlus::EMSManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EMSManager.hh>
#include <DataRuntimeLanguage.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::EMSManager;
using namespace EnergyPlus::DataRuntimeLanguage;
using namespace ObjexxFCL;

TEST( EMSManager, TestForUniqueEMSActuators )
{

	ShowMessage( "Begin Test: EMSManager, TestForUniqueEMSActuators" );
	EMSActuatorAvailable.allocate(100);


	std::string componentTypeName1( "Chiller1" );
	std::string componentTypeName2( "Chiller2" );
	std::string uniqueIDName1( "Plant Component Chiller:Electric:ReformulatedEIR" );
	std::string controlTypeName1( "On/Off Supervisory" );
	std::string units1( "None" );
	bool EMSActuated1( true );
	bool testBoolean1( true );
	bool testBoolean2( true );
	bool testBoolean3( true );

	//calling three times but twice with same names should still result in only two item in the resulting list
	SetupEMSActuator( componentTypeName1, uniqueIDName1, controlTypeName1, units1, EMSActuated1, testBoolean1 );
	SetupEMSActuator( componentTypeName1, uniqueIDName1, controlTypeName1, units1, EMSActuated1, testBoolean2 );
	SetupEMSActuator( componentTypeName2, uniqueIDName1, controlTypeName1, units1, EMSActuated1, testBoolean3 );
	EXPECT_EQ( 2, numEMSActuatorsAvailable );


	// repeat with integers
	std::string controlTypeName2( "ModeOfSomething" );
	int testInt1( 7 );
	int testInt2( 9 );
	int testInt3( 11 );
	SetupEMSActuator( componentTypeName1, uniqueIDName1, controlTypeName2, units1, EMSActuated1, testInt1 );
	SetupEMSActuator( componentTypeName1, uniqueIDName1, controlTypeName2, units1, EMSActuated1, testInt2 );
	SetupEMSActuator( componentTypeName2, uniqueIDName1, controlTypeName2, units1, EMSActuated1, testInt3 );
	EXPECT_EQ( 4, numEMSActuatorsAvailable );

	// repeat with reals
	std::string controlTypeName3( "ValueOfResults" );
	Real64 testReal1( 0.123 );
	Real64 testReal2( 0.456 );
	Real64 testReal3( 0.789 );
	SetupEMSActuator( componentTypeName1, uniqueIDName1, controlTypeName3, units1, EMSActuated1, testReal1 );
	SetupEMSActuator( componentTypeName1, uniqueIDName1, controlTypeName3, units1, EMSActuated1, testReal2 );
	SetupEMSActuator( componentTypeName2, uniqueIDName1, controlTypeName3, units1, EMSActuated1, testReal3 );
	EXPECT_EQ( 6, numEMSActuatorsAvailable );

	EMSActuatorAvailable.deallocate();

}
