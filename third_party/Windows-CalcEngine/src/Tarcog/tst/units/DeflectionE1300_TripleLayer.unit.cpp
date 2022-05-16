#include <gtest/gtest.h>

#include <vector>

#include <WCETarcog.hpp>

class TestDeflectionE1300_TripleLayer : public testing::Test
{
protected:
    void SetUp() override
    {}
};

TEST_F(TestDeflectionE1300_TripleLayer, ZeroDeflection)
{
    const auto width{1.0};
    const auto height{1.0};
    const std::vector<Deflection::LayerData> layer{{0.00556}, {0.00742}, {0.00556}};
    const std::vector<Deflection::GapData> gap{{0.0127, 273.15 + 21}, {0.0127, 273.15 + 21}};
    Deflection::DeflectionE1300 def(width, height, layer, gap);

    const std::vector<double> loadTemperatures{21 + 273.15, 21 + 273.15};
    def.setLoadTemperatures(loadTemperatures);

    const auto res{def.results()};
    const auto deflection{res.deflection};
    const auto panesLoad{res.paneLoad};

    const std::vector<double> correctDeflection{0, 0, 0};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-9);
    }

    const std::vector<double> correctPanesLoad{0, 0, 0};
    for(size_t i = 0u; i < correctPanesLoad.size(); ++i)
    {
        EXPECT_NEAR(correctPanesLoad[i], panesLoad[i], 1e-6);
    }
}

TEST_F(TestDeflectionE1300_TripleLayer, DeflectionSquaredWindow)
{
    const auto width{1.0};
    const auto height{1.0};
    const std::vector<Deflection::LayerData> layer{{0.00556}, {0.00742}, {0.00556}};
    const std::vector<Deflection::GapData> gap{{0.0127, 273.15 + 21}, {0.0127, 273.15 + 21}};
    Deflection::DeflectionE1300 def(width, height, layer, gap);

    const std::vector<double> loadTemperatures{22 + 273.15, 21 + 273.15};
    def.setLoadTemperatures(loadTemperatures);

    const auto res{def.results()};
    const auto deflection{res.deflection};
    const auto panesLoad{res.paneLoad};

    const std::vector<double> correctDeflection{0.07465884e-03, -0.022613683e-03, -0.02094219e-03};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-9);
    }

    const std::vector<double> correctPanesLoad{19.883320, -14.308254, -5.575066};
    for(size_t i = 0u; i < correctPanesLoad.size(); ++i)
    {
        EXPECT_NEAR(correctPanesLoad[i], panesLoad[i], 1e-6);
    }
}

TEST_F(TestDeflectionE1300_TripleLayer, DeflectionDifferentWidthAndHeight)
{
    const auto width{1.0};
    const auto height{2.5};
    const std::vector<Deflection::LayerData> layer{{0.00556}, {0.00742}, {0.00556}};
    const std::vector<Deflection::GapData> gap{{0.0127, 273.15 + 21}, {0.0127, 273.15 + 21}};
    Deflection::DeflectionE1300 def(width, height, layer, gap);

    const std::vector<double> loadTemperatures{22 + 273.15, 21 + 273.15};
    def.setLoadTemperatures(loadTemperatures);

    const auto res{def.results()};
    const auto error{res.error};
    const auto deflection{res.deflection};
    const auto panesLoad{res.paneLoad};

    const std::vector<double> correctDeflection{0.072674289e-03, -0.02168655e-03, -0.02113032e-03};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-9);
    }

    const std::vector<double> correctPanesLoad{6.844653, -4.854545, -1.990108};
    for(size_t i = 0u; i < correctPanesLoad.size(); ++i)
    {
        EXPECT_NEAR(correctPanesLoad[i], panesLoad[i], 1e-6);
    }
}

TEST_F(TestDeflectionE1300_TripleLayer, DeflectionDifferentInteriorAndExteriorPressure)
{
    const auto width{1.0};
    const auto height{2.5};
    const std::vector<Deflection::LayerData> layer{{0.00556}, {0.00742}, {0.00556}};
    const std::vector<Deflection::GapData> gap{{0.0127, 273.15 + 21}, {0.0127, 273.15 + 21}};
    Deflection::DeflectionE1300 def(width, height, layer, gap);

    const double exteriorPressure{102325};
    def.setExteriorPressure(exteriorPressure);

    const std::vector<double> loadTemperatures{22 + 273.15, 21 + 273.15};
    def.setLoadTemperatures(loadTemperatures);

    const auto res{def.results()};
    const auto deflection{res.deflection};
    const auto panesLoad{res.paneLoad};

    const std::vector<double> correctDeflection{-2.408286e-3, -2.317540e-3, -2.242010e-3};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-9);
    }

    const std::vector<double> correctPanesLoad{-238.541713, -539.835780, -221.622507};
    for(size_t i = 0u; i < correctPanesLoad.size(); ++i)
    {
        EXPECT_NEAR(correctPanesLoad[i], panesLoad[i], 1e-6);
    }
}

TEST_F(TestDeflectionE1300_TripleLayer, DeflectionWithTiltAngle)
{
    const auto width{1.0};
    const auto height{2.5};
    const std::vector<Deflection::LayerData> layer{{0.00556}, {0.00742}, {0.00556}};
    const std::vector<Deflection::GapData> gap{{0.0127, 273.15 + 21}, {0.0127, 273.15 + 21}};
    Deflection::DeflectionE1300 def(width, height, layer, gap);

    const auto tiltAngle{45.0};
    def.setIGUTilt(tiltAngle);

    const std::vector<double> loadTemperatures{22 + 273.15, 21 + 273.15};
    def.setLoadTemperatures(loadTemperatures);

    const auto res{def.results()};
    const auto deflection{res.deflection};
    const auto panesLoad{res.paneLoad};

    const std::vector<double> correctDeflection{-0.261122e-3, -0.337247e-3, -0.302394e-3};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-9);
    }

    const std::vector<double> correctPanesLoad{-24.606516, -75.533516, -28.496727};
    for(size_t i = 0u; i < correctPanesLoad.size(); ++i)
    {
        EXPECT_NEAR(correctPanesLoad[i], panesLoad[i], 1e-6);
    }
}

TEST_F(TestDeflectionE1300_TripleLayer, DeflectionWithAppliedLoad)
{
    const auto width{1.0};
    const auto height{2.5};
    const std::vector<Deflection::LayerData> layer{{0.00556}, {0.00742}, {0.00556}};
    const std::vector<Deflection::GapData> gap{{0.0127, 273.15 + 21}, {0.0127, 273.15 + 21}};
    Deflection::DeflectionE1300 def(width, height, layer, gap);

    const std::vector<double> appliedLoad{1500, 0, 0};
    def.setAppliedLoad(appliedLoad);

    const std::vector<double> loadTemperatures{22 + 273.15, 21 + 273.15};
    def.setLoadTemperatures(loadTemperatures);

    const auto res{def.results()};
    const auto deflection{res.deflection};
    const auto panesLoad{res.paneLoad};

    const std::vector<double> correctDeflection{-3.619900e-3, -3.420178e-3, -3.323497e-3};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-9);
    }

    const std::vector<double> correctPanesLoad{-361.828081, -806.503990, -331.667929};
    for(size_t i = 0u; i < correctPanesLoad.size(); ++i)
    {
        EXPECT_NEAR(correctPanesLoad[i], panesLoad[i], 1e-6);
    }
}

TEST_F(TestDeflectionE1300_TripleLayer, DeflectionTestTripleClearNoLoad)
{
    const auto width{1.0};
    const auto height{1.0};
    const std::vector<Deflection::LayerData> layer{{0.003048}, {0.003048}, {0.003048}};
    const std::vector<Deflection::GapData> gap{{0.006, 273}, {0.025, 273}};
    Deflection::DeflectionE1300 def(width, height, layer, gap);

    //const std::vector<double> appliedLoad{0, 0, 0};
    //def.setAppliedLoad(appliedLoad);

    const std::vector<double> loadTemperatures{259.44977388, 273.46099867};
    def.setLoadTemperatures(loadTemperatures);

    const auto res{def.results()};
    const auto error{res.error};
    const auto deflection{res.deflection};
    const auto panesLoad{res.paneLoad};

    const std::vector<double> correctDeflection{-0.421354e-3, 0.265433e-3, 0.166646e-3};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-9);
    }

    const std::vector<double> correctPanesLoad{-19.254700278326521, 11.941581502135044, 7.3131187761958927};
    for(size_t i = 0u; i < correctPanesLoad.size(); ++i)
    {
        EXPECT_NEAR(correctPanesLoad[i], panesLoad[i], 1e-6);
    }
}

TEST_F(TestDeflectionE1300_TripleLayer, DeflectionTestTripleClearWithLoad)
{
    const auto width{1.0};
    const auto height{1.0};
    const std::vector<Deflection::LayerData> layer{{0.003048}, {0.003048}, {0.003048}};
    const std::vector<Deflection::GapData> gap{{0.006, 273}, {0.025, 273}};
    Deflection::DeflectionE1300 def(width, height, layer, gap);

    const std::vector<double> appliedLoad{0, 0, 100000};
    def.setAppliedLoad(appliedLoad);

    const std::vector<double> loadTemperatures{259.4496024, 273.460723};
    def.setLoadTemperatures(loadTemperatures);

    const auto res{def.results()};
    const auto deflection{res.deflection};
    const auto panesLoad{res.paneLoad};

    const std::vector<double> correctDeflection{22.795550e-3, 24.486071e-3, 63.329275e-3};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-9);
    }

    const std::vector<double> correctPanesLoad{7726.904240, 8555.365110, 83717.730650};
    for(size_t i = 0u; i < correctPanesLoad.size(); ++i)
    {
        EXPECT_NEAR(correctPanesLoad[i], panesLoad[i], 1e-6);
    }
}

TEST_F(TestDeflectionE1300_TripleLayer, DeflectionTestTripleDifferentInOutPressureLoad)
{
    const auto width{1.0};
    const auto height{1.0};
    const std::vector<Deflection::LayerData> layer{{0.003048}, {0.003048}, {0.003048}};
    const std::vector<Deflection::GapData> gap{{0.006, 330, 102000}, {0.0127, 330, 102000}};
    Deflection::DeflectionE1300 def(width, height, layer, gap);

    const auto interiorPressure{101000};
    const auto exteriorPressure{104000};

    def.setInteriorPressure(interiorPressure);
    def.setExteriorPressure(exteriorPressure);

    const std::vector<double> loadTemperatures{316.3393916, 317.7188435};
    def.setLoadTemperatures(loadTemperatures);

    const auto res{def.results()};
    const auto deflection{res.deflection};
    const auto panesLoad{res.paneLoad};

    const std::vector<double> correctDeflection{-9.338437e-3, -9.100851e-3, -8.466587e-3};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-9);
    }

    const std::vector<double> correctPanesLoad{-1132.076073, -1015.644143, -852.279784};
    for(size_t i = 0u; i < correctPanesLoad.size(); ++i)
    {
        EXPECT_NEAR(correctPanesLoad[i], panesLoad[i], 1e-6);
    }
}