#include <memory>
#include <stdexcept>
#include <gtest/gtest.h>

#include "WCETarcog.hpp"
#include "WCECommon.hpp"

// Example of double clear window with inital guess for solution
class TestTripleClear_EN673 : public testing::Test
{
private:
    std::unique_ptr<Tarcog::EN673::IGU> m_IGU;

protected:
    void SetUp() override
    {
        /////////////////////////////////////////////////////////
        /// Outdoor
        /////////////////////////////////////////////////////////
        auto airTemperature = 273.15;   // Kelvins
        auto filmCoefficient = 23;      // [W/m2K]
        const auto outdoor = Tarcog::EN673::Environment(airTemperature, filmCoefficient);

        /////////////////////////////////////////////////////////
        /// Indoor
        /////////////////////////////////////////////////////////

        airTemperature = 293.15;   // Kelvins
        filmCoefficient = 8;       // [W/m2K]
        const auto indoor = Tarcog::EN673::Environment(airTemperature, filmCoefficient);

        //////////////////////////////////////////////////////////
        /// First layer
        //////////////////////////////////////////////////////////
        const auto thickness = 0.003;    // [m]
        const auto conductivity = 1.0;   // [W/m2K]
        const auto emissFront = 0.84;
        const auto emissBack = 0.84;
        auto layerAbsorptance = 0.099839858711;

        const auto layer1 =
          Tarcog::EN673::Glass(conductivity, thickness, emissFront, emissBack, layerAbsorptance);

        /////////////////////////////////////////////////////////
        /// IGU
        /////////////////////////////////////////////////////////
        m_IGU = Tarcog::EN673::IGU::create(indoor, outdoor);
        m_IGU->addGlass(layer1);

        /////////////////////////////////////////////////////////
        /// gap and layer
        /////////////////////////////////////////////////////////

        const auto gapThickness = 0.0127;   // [mm]
        const auto gap1 = Tarcog::EN673::Gap(gapThickness);

        m_IGU->addGap(gap1);

        layerAbsorptance = 0.076627746224;
        const auto layer2 =
          Tarcog::EN673::Glass(conductivity, thickness, emissFront, emissBack, layerAbsorptance);
        m_IGU->addGlass(layer2);

        const auto gap2 = Tarcog::EN673::Gap(gapThickness);
        m_IGU->addGap(gap2);

        layerAbsorptance = 0.058234799653;
        const auto layer3 =
          Tarcog::EN673::Glass(conductivity, thickness, emissFront, emissBack, layerAbsorptance);

        m_IGU->addGlass(layer3);
    }

public:
    Tarcog::EN673::IGU * GetIGU() const
    {
        return m_IGU.get();
    };
};

TEST_F(TestTripleClear_EN673, Test1)
{
    SCOPED_TRACE("Begin Test: Uvalue");

    auto igu = GetIGU();

    const auto Uvalue = igu->Uvalue();

    EXPECT_NEAR(1.874193, Uvalue, 1e-4);

    const auto SHGC = igu->shgc(0.5984);

    EXPECT_NEAR(0.7084, SHGC, 1e-4);
}
