// EnergyPlus::BranchNodeConnections unit tests

// Google test headers
#include <gtest/gtest.h>

#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include "Fixtures/SQLiteFixture.hh"
#include <BranchNodeConnections.hh>
#include <DataBranchNodeConnections.hh>
#include <DataLoopNode.hh>

using namespace EnergyPlus;
using namespace BranchNodeConnections;
using namespace DataBranchNodeConnections;
using namespace DataLoopNode;

namespace EnergyPlus {

	TEST_F(SQLiteFixture, BranchNodeErrorCheck11Test) {
		bool errFlag = false; 
		RegisterNodeConnection(1, "BadNode", "Type1", "Object1", "ZoneNode", 0, true, errFlag);
		RegisterNodeConnection(2, "BadNode", "Type2", "Object3", "Actuator", 0, true, errFlag);
		RegisterNodeConnection(3, "BadNode", "Type2", "Object3", "Inlet", 0, true , errFlag);
		bool ErrorsFound = false;

		sqlite_test->createSQLiteSimulationsRecord(1, "EnergyPlus Version", "Current Time");

		EnergyPlus::sqlite = std::move(sqlite_test);
		CheckNodeConnections(ErrorsFound);
		sqlite_test = std::move(EnergyPlus::sqlite);

		auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

		ASSERT_EQ(3, errorData.size());
		std::vector<std::string> errorData2{ "3", "1", "1", "Node Connection Error, Node Name=\"BadNode\", The same zone node appears more than once.  Reference Object=TYPE1, Object Name=Object1  Reference Object=TYPE2, Object Name=Object3", "1" };
		EXPECT_EQ(errorData2, errorData[2]);

	}
}
