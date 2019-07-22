#ifndef EPLUS6_SCHEDULINGFIXTURE_HH
#define EPLUS6_SCHEDULINGFIXTURE_HH

#include <Fixtures/EnergyPlusFixture.hh>

namespace EnergyPlus {

class SchedulingTestFixture : public EnergyPlusFixture
{
public:
    // nothing for now

protected:
    void SetUp() override
    {
        EnergyPlusFixture::SetUp(); // Sets up the base fixture first.
        std::string const idf_objects =
            delimited_string({"Schedule:Constant,Always On,,1.0;", "Schedule:Constant,Always Off,,0.0;", "Schedule:Constant,Always Pi,,3.14;"});
        ASSERT_TRUE(process_idf(idf_objects));
    }
    void TearDown() override
    {
        EnergyPlusFixture::TearDown();
    } // Remember to tear down the base fixture after cleaning up derived fixture
};

}

#endif //EPLUS6_SCHEDULINGFIXTURE_HH
