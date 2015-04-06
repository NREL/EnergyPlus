// EnergyPlus::OutputProcessor Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/UtilityRoutines.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::OutputProcessor;
using namespace ObjexxFCL;
using namespace DataGlobals;

TEST( OutputProcessor, TestGetMeteredVariables )
{
	ShowMessage( "Begin Test: OutputProcessor, TestGetMeteredVariables" );

	int const NumVariables = 2;
	Array1D_int VarIndexes( NumVariables ); // Variable Numbers
	Array1D_int VarTypes( NumVariables ); // Variable Types (1=integer, 2=real, 3=meter)
	Array1D_int IndexTypes( NumVariables ); // Variable Index Types (1=Zone,2=HVAC)
	Array1D_string UnitsStrings( NumVariables ); // UnitsStrings for each variable
	Array1D_int ResourceTypes( NumVariables ); // ResourceTypes for each variable
	Array1D_string EndUses( NumVariables ); // EndUses for each variable
	Array1D_string Groups( NumVariables ); // Groups for each variable
	Array1D_string Names( NumVariables ); // Variable Names for each variable
	Reference< RealVariables > RVar;

	std::string TypeOfComp = "ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW";
	std::string NameOfComp = "FC-5-1B";

	int NumFound;

	GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );

	EXPECT_EQ( 0, NumFound );

	NumOfRVariable = 2;
	RVariableTypes.allocate( NumOfRVariable );
	NameOfComp = "OUTSIDELIGHTS";
	RVar.allocate();

	RVar().MeterArrayPtr = 1;
	RVariableTypes( 1 ).KeyNameOnlyUC = NameOfComp;
	RVariableTypes( 1 ).VarPtr = RVar;
	VarMeterArrays.allocate( 1 );

	VarMeterArrays( 1 ).NumOnMeters = 1;
	VarMeterArrays( 1 ).OnMeters( 1 ) = 1;

	EnergyMeters.allocate( 10 );
	EnergyMeters( 1 ).ResourceType = NameOfComp;

	GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );
	EXPECT_EQ( 1 , NumFound );

	// Clean up
	RVariableTypes.deallocate();
	RVar.deallocate();
	VarMeterArrays.deallocate();
	EnergyMeters.deallocate();
}
