#include <gtest/gtest.h>

#include <vector>

#include <WCETarcog.hpp>

class TestDeflectionE1300_DoubleLayer : public testing::Test
{
protected:
    void SetUp() override
    {}

};

TEST_F(TestDeflectionE1300_DoubleLayer, DeflectionSquaredWindow)
{
    const double width{1.0};
    const double height{1.0};
    std::vector<Deflection::LayerData> layer{{0.003048}, {0.00742}};
    std::vector<Deflection::GapData> gap{{0.0127, 273.15 + 30}};
    Deflection::DeflectionE1300 def(width, height, layer, gap);

    const std::vector<double> loadTemperatures{268};

    def.setLoadTemperatures(loadTemperatures);

    const auto res{def.results()};
    const auto error{res.error};
    const auto deflection{res.deflection};
    const auto pressureDifference{res.paneLoad};

    ASSERT_EQ(error.has_value(), true);

    const auto correctError{9.924406e-08};
    EXPECT_NEAR(error.value(), correctError, 1e-9);

    const std::vector<double> correctDeflection{-2.469101e-3, 0.259526e-3};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-9);
    }

    const std::vector<double> correctPressureDifference{-164.301506, 164.301506};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctPressureDifference[i], pressureDifference[i], 1e-6);
    }
}

TEST_F(TestDeflectionE1300_DoubleLayer, DeflectionDifferentWidthAndHeight)
{
    const double width{1.0};
    const double height{2.5};
    std::vector<Deflection::LayerData> layer{{0.003048}, {0.00742}};
    std::vector<Deflection::GapData> gap{{0.0127, 273.15 + 30}};
    Deflection::DeflectionE1300 def(width, height, layer, gap);

    const std::vector<double> loadTemperatures{268};

    def.setLoadTemperatures(loadTemperatures);

    const auto res{def.results()};
    const auto error{res.error};
    const auto deflection{res.deflection};
    const auto pressureDifference{res.paneLoad};

    ASSERT_EQ(error.has_value(), true);

    const auto correctError{5.43917994946e-05};
    EXPECT_NEAR(error.value(), correctError, 1e-9);

    const std::vector<double> correctDeflection{-2.922637e-3, 0.216158e-3};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-9);
    }

    const std::vector<double> correctPressureDifference{-48.406364, 48.406364};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctPressureDifference[i], pressureDifference[i], 1e-6);
    }
}

TEST_F(TestDeflectionE1300_DoubleLayer, DeflectionDifferentInteriorAndExteriorPressure)
{
    const double width{1.0};
    const double height{2.5};
    std::vector<Deflection::LayerData> layer{{0.003048}, {0.00742}};
    std::vector<Deflection::GapData> gap{{0.0127, 273.15 + 30}};
    Deflection::DeflectionE1300 def(width, height, layer, gap);

    const double exteriorPressure{102325};
    def.setExteriorPressure(exteriorPressure);

    const std::vector<double> loadTemperatures{268};

    def.setLoadTemperatures(loadTemperatures);

    const auto res{def.results()};
    const auto error{res.error};
    const auto deflection{res.deflection};
    const auto pressureDifference{res.paneLoad};

    ASSERT_EQ(error.has_value(), true);

    const auto correctError{-4.656613e-08};
    EXPECT_NEAR(error.value(), correctError, 1e-9);

    const std::vector<double> correctDeflection{-6.029248e-3, -3.687419e-3};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-9);
    }

    const std::vector<double> correctPressureDifference{-128.864957, -871.135043};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctPressureDifference[i], pressureDifference[i], 1e-6);
    }
}

TEST_F(TestDeflectionE1300_DoubleLayer, DeflectionWithTiltAngle)
{
    const double width{1.0};
    const double height{2.5};
    std::vector<Deflection::LayerData> layer{{0.003048}, {0.00742}};
    std::vector<Deflection::GapData> gap{{0.0127, 273.15 + 30}};
    Deflection::DeflectionE1300 def(width, height, layer, gap);

    const auto tiltAngle{45.0};
    def.setIGUTilt(tiltAngle);

    const std::vector<double> loadTemperatures{268};

    def.setLoadTemperatures(loadTemperatures);

    const auto res{def.results()};
    const auto error{res.error};
    const auto deflection{res.deflection};
    const auto pressureDifference{res.paneLoad};

    ASSERT_EQ(error.has_value(), true);

    const auto correctError{3.521785e-05};
    EXPECT_NEAR(error.value(), correctError, 1e-9);

    const std::vector<double> correctDeflection{-2.626985e-3, 0.532367e-3};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-9);
    }

    const std::vector<double> correctPressureDifference{-43.450134, 119.245270};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctPressureDifference[i], pressureDifference[i], 1e-6);
    }
}

TEST_F(TestDeflectionE1300_DoubleLayer, DeflectionWithAppliedLoad)
{
    const double width{1.0};
    const double height{2.5};
    std::vector<Deflection::LayerData> layer{{0.003048}, {0.00742}};
    std::vector<Deflection::GapData> gap{{0.0127, 273.15 + 30}};
    Deflection::DeflectionE1300 def(width, height, layer, gap);

    const std::vector<double> loadTemperatures{268};
    def.setLoadTemperatures(loadTemperatures);

    const std::vector<double> appliedLoad{1500, 0};
    def.setAppliedLoad(appliedLoad);

    const auto res{def.results()};
    const auto error{res.error};
    const auto deflection{res.deflection};
    const auto panesLoad{res.paneLoad};

    ASSERT_EQ(error.has_value(), true);

    const auto correctError{-5.215406e-08};
    EXPECT_NEAR(error.value(), correctError, 1e-9);

    const std::vector<double> correctDeflection{-7.190758e-3, -5.554584e-3};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctDeflection[i], deflection[i], 1e-9);
    }

    const std::vector<double> correctPanesLoad{-177.299229, -1322.700771};
    for(size_t i = 0u; i < correctDeflection.size(); ++i)
    {
        EXPECT_NEAR(correctPanesLoad[i], panesLoad[i], 1e-6);
    }
}