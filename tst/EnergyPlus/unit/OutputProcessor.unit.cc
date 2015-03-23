// EnergyPlus::OutputProcessor Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/OutputProcessor.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::OutputProcessor;
using namespace ObjexxFCL;
using namespace DataGlobals;

TEST( OutputProcessor, TestGetMeteredVariables )
{
	FArray1D_int VarIndexes; // Variable Numbers
	FArray1D_int VarTypes; // Variable Types (1=integer, 2=real, 3=meter)
	FArray1D_int IndexTypes; // Variable Index Types (1=Zone,2=HVAC)
	FArray1D_string UnitsStrings; // UnitsStrings for each variable
	FArray1D_int ResourceTypes; // ResourceTypes for each variable
	FArray1D_string EndUses; // EndUses for each variable
	FArray1D_string Groups; // Groups for each variable
	FArray1D_string Names; // Variable Names for each variable
	int NumVariables = 2;
	Real64 ZoneElectricPower = 0.2;
	Reference< RealVariables > RVar;


	std::string TypeOfComp = "ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW";
	std::string NameOfComp = "FC-5-1B";

	VarIndexes.allocate( NumVariables );
	VarTypes.allocate( NumVariables );
	IndexTypes.allocate( NumVariables );
	UnitsStrings.allocate( NumVariables );
	ResourceTypes.allocate( NumVariables );
	EndUses.allocate( NumVariables );
	Groups.allocate( NumVariables );
	Names.allocate( NumVariables );

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
	VarIndexes.deallocate();
	VarTypes.deallocate();
	IndexTypes.deallocate();
	UnitsStrings.deallocate();
	ResourceTypes.deallocate();
	EndUses.deallocate();
	Groups.deallocate();
	Names.deallocate();
	RVariableTypes.deallocate();
	// Won't compile RVar.deallocate();
	VarMeterArrays.deallocate();
	EnergyMeters.deallocate();
}
