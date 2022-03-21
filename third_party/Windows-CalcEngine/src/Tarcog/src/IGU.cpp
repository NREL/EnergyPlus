
#include <cmath>
#include <vector>
#include <memory>
#include <algorithm>
#include <cassert>
#include <stdexcept>
#include <iostream>

#include "IGU.hpp"
#include "BaseIGULayer.hpp"
#include "IGUSolidLayer.hpp"
#include "IGUGapLayer.hpp"
#include "Surface.hpp"
#include "IGUSolidDeflection.hpp"
#include "IGUGapDeflection.hpp"
#include "IGUVentilatedGapLayer.hpp"
#include "BaseShade.hpp"
#include "Environment.hpp"
#include "WCECommon.hpp"

using FenestrationCommon::Side;

namespace Tarcog
{
    namespace ISO15099
    {
        CIGU::CIGU(double const t_Width, double const t_Height, double const t_Tilt) :
            m_Width(t_Width), m_Height(t_Height), m_Tilt(t_Tilt)
        {}

        CIGU::CIGU(CIGU const & t_IGU)
        {
            operator=(t_IGU);
        }

        CIGU & CIGU::operator=(CIGU const & t_IGU)
        {
            m_Width = t_IGU.m_Width;
            m_Height = t_IGU.m_Height;
            m_Tilt = t_IGU.m_Tilt;
            m_Layers.clear();
            for(auto & layer : t_IGU.m_Layers)
            {
                const auto aLayer{layer->clone()};
                addLayer(aLayer);
            }

            if(t_IGU.m_DeflectionFromE1300Curves != nullptr)
            {
                m_DeflectionFromE1300Curves =
                  std::make_unique<Deflection::DeflectionE1300>(*t_IGU.m_DeflectionFromE1300Curves);
            }

            return *this;
        }

        CIGU::~CIGU()
        {
            for(std::shared_ptr<CBaseIGULayer> layer : getSolidLayers())
            {
                layer->tearDownConnections();
            }
        }

        void CIGU::addLayer(const std::shared_ptr<CBaseLayer> & t_Layer)
        {
            // pushes only solid layers to array. Gap layers are connected via linked list
            // In case this is first layer then it must be a solid layer in order to create IGU
            if(getNumOfLayers() == 0)
            {
                if(std::dynamic_pointer_cast<CIGUSolidLayer>(t_Layer) != nullptr)
                {
                    m_Layers.push_back(t_Layer);
                }
                else
                {
                    throw std::runtime_error("First inserted layer must be a solid layer.");
                }
            }
            else
            {
                auto lastLayer = m_Layers.back();
                if(std::dynamic_pointer_cast<CIGUSolidLayer>(t_Layer)
                   != std::dynamic_pointer_cast<CIGUSolidLayer>(lastLayer))
                {
                    m_Layers.push_back(t_Layer);
                    lastLayer->connectToBackSide(t_Layer);
                }
                else
                {
                    throw std::runtime_error(
                      "Two adjecent layers in IGU cannot be of same type. "
                      "IGU must be constructed of array of solid and gap layers.");
                }
            }

            checkForLayerUpgrades(t_Layer);

            t_Layer->setTilt(m_Tilt);
            t_Layer->setWidth(m_Width);
            t_Layer->setHeight(m_Height);
        }

        void CIGU::addLayers(const std::initializer_list<std::shared_ptr<CBaseIGULayer>> & layers)
        {
            for(const auto & layer : layers)
            {
                addLayer(layer);
            }
        }

        void CIGU::setTilt(double const t_Tilt)
        {
            for(auto & layer : m_Layers)
            {
                layer->setTilt(t_Tilt);
            }
            m_Tilt = t_Tilt;

            if(m_DeflectionFromE1300Curves != nullptr)
            {
                m_DeflectionFromE1300Curves->setIGUTilt(t_Tilt);
            }
        }

        void CIGU::setWidth(double const t_Width)
        {
            for(auto & layer : m_Layers)
            {
                layer->setWidth(t_Width);
            }
            m_Width = t_Width;

            if(m_DeflectionFromE1300Curves != nullptr)
            {
                m_DeflectionFromE1300Curves->setDimensions(m_Width, m_Height);
            }
        }

        void CIGU::setHeight(double const t_Height)
        {
            for(auto & layer : m_Layers)
            {
                layer->setHeight(t_Height);
            }
            m_Height = t_Height;

            if(m_DeflectionFromE1300Curves != nullptr)
            {
                m_DeflectionFromE1300Curves->setDimensions(m_Width, m_Height);
            }
        }

        void CIGU::setSolarRadiation(double const t_SolarRadiation) const
        {
            for(auto & layer : getSolidLayers())
            {
                layer->setSolarRadiation(t_SolarRadiation);
            }
        }

        std::shared_ptr<CBaseLayer> CIGU::getEnvironment(Environment t_Environment) const
        {
            std::shared_ptr<CBaseLayer> aLayer = nullptr;
            switch(t_Environment)
            {
                case Environment::Indoor:
                    aLayer = m_Layers.back();
                    break;
                case Environment::Outdoor:
                    aLayer = m_Layers.front();
                    break;
            }
            return aLayer;
        }

        std::vector<double> CIGU::getState() const
        {
            std::vector<double> aState;

            for(auto & layer : getSolidLayers())
            {
                // State must be filled in this exact order.
                aState.push_back(layer->getTemperature(Side::Front));
                aState.push_back(layer->J(Side::Front));
                aState.push_back(layer->J(Side::Back));
                aState.push_back(layer->getTemperature(Side::Back));
            }

            return aState;
        }

        void CIGU::setState(const std::vector<double> & t_State) const
        {
            size_t i = 0;
            for(const auto & aLayer : getSolidLayers())
            {
                const auto Tf = t_State[4 * i];
                const auto Jf = t_State[4 * i + 1];
                const auto Jb = t_State[4 * i + 2];
                const auto Tb = t_State[4 * i + 3];
                aLayer->setLayerState(Tf, Tb, Jf, Jb);
                ++i;
            }
        }

        std::vector<double> CIGU::getTemperatures() const
        {
            std::vector<double> aTemperatures;

            for(auto const & layer : getSolidLayers())
            {
                for(auto aSide : FenestrationCommon::EnumSide())
                {
                    aTemperatures.push_back(layer->getTemperature(aSide));
                }
            }

            return aTemperatures;
        }

        std::vector<double> CIGU::getRadiosities() const
        {
            std::vector<double> aRadiosities;

            for(auto const & layer : getSolidLayers())
            {
                for(auto aSide : FenestrationCommon::EnumSide())
                {
                    aRadiosities.push_back(layer->J(aSide));
                }
            }

            return aRadiosities;
        }

        std::vector<double> CIGU::getMaxDeflections() const
        {
            std::vector<double> aMaxDeflections;

            for(auto const & layer : getSolidLayers())
            {
                aMaxDeflections.push_back(layer->getMaxDeflection());
            }

            return aMaxDeflections;
        }

        std::vector<double> CIGU::getMeanDeflections() const
        {
            std::vector<double> aMeanDeflections;

            for(auto const & layer : getSolidLayers())
            {
                aMeanDeflections.push_back(layer->getMeanDeflection());
            }

            return aMeanDeflections;
        }

        std::vector<double> CIGU::getPanesLoad() const
        {
            std::vector<double> paneLoad(getSolidLayers().size());

            if(m_DeflectionFromE1300Curves != nullptr)
            {
                paneLoad = m_DeflectionFromE1300Curves->results().paneLoad;
            }

            return paneLoad;
        }

        double CIGU::getThickness() const
        {
            auto totalWidth = 0.0;

            for(auto & layer : m_Layers)
            {
                totalWidth += layer->getThickness();
            }

            return totalWidth;
        }

        double CIGU::getTilt() const
        {
            return m_Tilt;
        }

        double CIGU::getWidth() const
        {
            return m_Width;
        }

        double CIGU::getHeight() const
        {
            return m_Height;
        }

        size_t CIGU::getNumOfLayers() const
        {
            return (m_Layers.size() + 1) / 2;
        }

        double CIGU::getVentilationFlow(const Environment t_Environment) const
        {
            // This is asking flow from the gap that is connected to the one of the environments.
            const auto size = m_Layers.size();
            auto result{0.0};
            if(size > 1u)
            {
                // This is important in order to get correct gap numbering. It will return the gap
                // that is connected with the environment.
                const std::map<Environment, size_t> envLayer = {{Environment::Indoor, size - 2},
                                                                {Environment::Outdoor, 1}};

                // Need to make sure that solid layer is actually permeable as well
                const std::map<Environment, size_t> solidLayerIndex = {{Environment::Indoor, size - 1},
                                                                  {Environment::Outdoor, 0}};

                if(m_Layers[solidLayerIndex.at(t_Environment)]->isPermeable())
                {
                    result = m_Layers[envLayer.at(t_Environment)]->getGainFlow();
                }
            }
            return result;
        }

        void CIGU::setInitialGuess(std::vector<double> const & t_Guess) const
        {
            if(2 * getNumOfLayers() != t_Guess.size())
            {
                std::cout << "Number of temperatures in initial guess cannot fit number of layers."
                             "Program will use initial guess instead"
                          << std::endl;
            }
            else
            {
                size_t Index = 0;
                for(auto & aLayer : getSolidLayers())
                {
                    for(auto aSide : FenestrationCommon::EnumSide())
                    {
                        auto aSurface = aLayer->getSurface(aSide);
                        aSurface->initializeStart(t_Guess[Index]);
                        ++Index;
                    }
                }
            }
        }

        void CIGU::setDeflectionProperties(const double t_Tini,
                                           const double t_Pini,
                                           const double t_InsidePressure,
                                           const double t_OutsidePressure)
        {
            std::vector<Deflection::LayerData> layerData;
            for(const auto & layer : getSolidLayers())
            {
                layerData.emplace_back(
                  layer->getThickness(), layer->density(), layer->youngsModulus());
            }

            std::vector<Deflection::GapData> gapData;
            for(const auto & gap : getGapLayers())
            {
                gapData.emplace_back(gap->getThickness(), t_Tini, t_Pini);
            }

            m_DeflectionFromE1300Curves =
              std::make_unique<Deflection::DeflectionE1300>(m_Width, m_Height, layerData, gapData);

            m_DeflectionFromE1300Curves->setIGUTilt(m_Tilt);
            m_DeflectionFromE1300Curves->setInteriorPressure(t_InsidePressure);
            m_DeflectionFromE1300Curves->setExteriorPressure(t_OutsidePressure);

            if(m_DeflectionAppliedLoad.size() == layerData.size())
            {
                m_DeflectionFromE1300Curves->setAppliedLoad(m_DeflectionAppliedLoad);
            }
        }

        //! The old deflection routine that did not work because program failed to converge. Will
        //! disable it for now since results are incorrect (Simon)
        // void CIGU::setDeflectionProperties(double const t_Tini, double const t_Pini)
        //{
        //    // Simply decorating layers in a list with new behavior
        //    auto aVector = getSolidLayers();
        //    // deflection properties of the IGU
        //    auto Lmean = Ldmean();
        //    auto Lmax = Ldmax();
        //
        //    for(auto & aLayer : getSolidLayers())
        //    {
        //        // Deflection could also be decorated (created) outside in which case program
        //        // already have a layer as deflection layer. If that is not done then layer must
        //        be
        //        // decorated with default deflection properties
        //        std::shared_ptr<CIGUSolidLayerDeflection> aDeflectionLayer = nullptr;
        //        if(!aLayer->isDeflected())
        //        {
        //            aDeflectionLayer = std::make_shared<CIGUSolidLayerDeflection>(*aLayer);
        //        }
        //        else
        //        {
        //            aDeflectionLayer =
        //            std::dynamic_pointer_cast<CIGUSolidLayerDeflection>(aLayer);
        //        }
        //        replaceLayer(
        //          aLayer,
        //          std::make_shared<CIGUDeflectionTempAndPressure>(aDeflectionLayer, Lmax, Lmean));
        //    }
        //
        //    for(std::shared_ptr<CIGUGapLayer> & aLayer : getGapLayers())
        //    {
        //        replaceLayer(aLayer,
        //                     std::make_shared<CIGUGapLayerDeflection>(*aLayer, t_Tini, t_Pini));
        //    }
        //}

        void CIGU::setDeflectionProperties(std::vector<double> const & t_MeasuredDeflections)
        {
            if(t_MeasuredDeflections.size() != getNumOfLayers() - 1)
            {
                throw std::runtime_error(
                  "Number of measured deflection values must be equal to number of gaps.");
            }

            auto nominator = 0.0;
            for(size_t i = 0; i < t_MeasuredDeflections.size(); ++i)
            {
                auto SumL = 0.0;
                for(auto j = i; j < t_MeasuredDeflections.size(); ++j)
                {
                    SumL += getGapLayers()[j]->getThickness() - t_MeasuredDeflections[j];
                }
                auto aDefLayer = CIGUSolidLayerDeflection(*getSolidLayers()[i]);
                nominator += SumL * aDefLayer.flexuralRigidity();
            }

            auto denominator = 0.0;
            for(auto i = 0u; i < getSolidLayers().size(); ++i)
            {
                auto aDefLayer = CIGUSolidLayerDeflection(*getSolidLayers()[i]);
                denominator += aDefLayer.flexuralRigidity();
            }

            // First need to calculate new deflections before applying them. Applying them right
            // away will cause that next gap width calculation will already have included one
            // surface makeDeflectable
            auto LDefNMax = nominator / denominator;
            auto deflectionRatio = Ldmean() / Ldmax();

            std::vector<double> LDefMax;
            LDefMax.push_back(LDefNMax);
            for(auto i = getNumOfLayers() - 1; i > 0; --i)
            {
                LDefNMax =
                  t_MeasuredDeflections[i - 1] - getGapLayers()[i - 1]->getThickness() + LDefNMax;
                LDefMax.insert(LDefMax.begin(), LDefNMax);
            }

            for(auto i = 0u; i < getNumOfLayers(); ++i)
            {
                LDefNMax = LDefMax[i];
                auto LDefNMean = deflectionRatio * LDefNMax;
                auto aLayer = getSolidLayers()[i];
                auto aDefLayer = std::make_shared<CIGUSolidLayerDeflection>(*aLayer);
                aDefLayer =
                  std::make_shared<CIGUDeflectionMeasuread>(aDefLayer, LDefNMean, LDefNMax);
                replaceLayer(aLayer, aDefLayer);
            }
        }

        void CIGU::updateDeflectionState()
        {
            if(m_DeflectionFromE1300Curves != nullptr)
            {
                const auto gapLayers{getGapLayers()};
                std::vector<double> gapTemperatures(gapLayers.size());
                for(size_t i = 0u; i < gapTemperatures.size(); ++i)
                {
                    gapTemperatures[i] = gapLayers[i]->averageTemperature();
                }
                m_DeflectionFromE1300Curves->setLoadTemperatures(gapTemperatures);

                auto deflectionResults{m_DeflectionFromE1300Curves->results()};

                // This is borrowed from Timschenko. It will be used till E1300 calculations are
                // actually doing this.
                const auto deflectionRatio = Ldmean() / Ldmax();

                auto solidLayers{getSolidLayers()};

                assert(deflectionResults.deflection.size() == solidLayers.size());

                for(size_t i = 0u; i < deflectionResults.deflection.size(); ++i)
                {
                    auto def{deflectionResults.deflection[i]};
                    solidLayers[i]->applyDeflection(deflectionRatio * def, def);
                }
            }
        }

        void CIGU::replaceLayer(std::shared_ptr<CBaseIGULayer> const & t_Original,
                                std::shared_ptr<CBaseIGULayer> const & t_Replacement)
        {
            size_t index = static_cast<size_t>(find(m_Layers.begin(), m_Layers.end(), t_Original)
                                               - m_Layers.begin());
            m_Layers[index] = t_Replacement;
            if(index > 0)
            {
                m_Layers[index - 1]->connectToBackSide(t_Replacement);
            }
            if(index < m_Layers.size() - 1)
            {
                t_Replacement->connectToBackSide(m_Layers[index + 1]);
            }
        }

        void CIGU::checkForLayerUpgrades(const std::shared_ptr<CBaseLayer> & t_Layer)
        {
            if(std::dynamic_pointer_cast<CIGUShadeLayer>(t_Layer) != nullptr)
            {
                if(std::dynamic_pointer_cast<CIGUGapLayer>(t_Layer->getPreviousLayer()) != nullptr)
                {
                    auto newLayer = std::make_shared<CIGUVentilatedGapLayer>(
                      std::dynamic_pointer_cast<CIGUGapLayer>(t_Layer->getPreviousLayer()));
                    replaceLayer(
                      std::dynamic_pointer_cast<CIGUGapLayer>(t_Layer->getPreviousLayer()),
                      newLayer);
                }
            }
            if(std::dynamic_pointer_cast<CIGUGapLayer>(t_Layer) != nullptr)
            {
                if(std::dynamic_pointer_cast<CIGUShadeLayer>(t_Layer->getPreviousLayer())
                   != nullptr)
                {
                    auto newLayer = std::make_shared<CIGUVentilatedGapLayer>(
                      std::dynamic_pointer_cast<CIGUGapLayer>(t_Layer));
                    replaceLayer(std::dynamic_pointer_cast<CIGUGapLayer>(t_Layer), newLayer);
                }
            }
        }

        double CIGU::Ldmean() const
        {
            using ConstantsData::WCE_PI;

            auto coeff = 16 / (pow(WCE_PI, 6));
            auto totalSum = 0.0;
            for(auto m = 1; m <= 5; m += 2)
            {
                for(auto n = 1; n <= 5; n += 2)
                {
                    auto nomin = 4.0;
                    auto denom = m * m * n * n * WCE_PI * WCE_PI
                                 * pow(pow(m / m_Width, 2) + pow(n / m_Height, 2), 2);
                    totalSum += nomin / denom;
                }
            }
            return coeff * totalSum;
        }

        double CIGU::Ldmax() const
        {
            using ConstantsData::WCE_PI;

            auto coeff = 16 / (pow(WCE_PI, 6));
            auto totalSum = 0.0;
            for(auto m = 1; m <= 5; m += 2)
            {
                for(auto n = 1; n <= 5; n += 2)
                {
                    auto nomin = sin(m * WCE_PI / 2) * sin(n * WCE_PI / 2);
                    auto denom = m * n * pow(pow(m / m_Width, 2) + pow(n / m_Height, 2), 2);
                    totalSum += nomin / denom;
                }
            }
            return coeff * totalSum;
        }

        std::vector<std::shared_ptr<CIGUSolidLayer>> CIGU::getSolidLayers() const
        {
            std::vector<std::shared_ptr<CIGUSolidLayer>> aVect;
            for(auto const & aLayer : m_Layers)
            {
                if(std::dynamic_pointer_cast<CIGUSolidLayer>(aLayer) != nullptr)
                {
                    aVect.push_back(std::dynamic_pointer_cast<CIGUSolidLayer>(aLayer));
                }
            }
            return aVect;
        }

        std::vector<std::shared_ptr<CIGUGapLayer>> CIGU::getGapLayers() const
        {
            std::vector<std::shared_ptr<CIGUGapLayer>> aVect;
            for(auto const & aLayer : m_Layers)
            {
                if(std::dynamic_pointer_cast<CIGUGapLayer>(aLayer) != nullptr)
                {
                    aVect.push_back(std::dynamic_pointer_cast<CIGUGapLayer>(aLayer));
                }
            }
            return aVect;
        }

        std::vector<std::shared_ptr<CBaseLayer>> CIGU::getLayers() const
        {
            return m_Layers;
        }

        void CIGU::setAbsorptances(const std::vector<double> & absorptances, double solarRadiation)
        {
            auto solidLayers = getSolidLayers();
            if(solidLayers.size() != absorptances.size())
            {
                throw std::runtime_error(
                  "Number of absorptances does not match number of solid layers.");
            }
            for(size_t i = 0; i < solidLayers.size(); ++i)
            {
                solidLayers[i]->setSolarAbsorptance(absorptances[i], solarRadiation);
            }
        }

        void CIGU::clearDeflection()
        {
            m_DeflectionFromE1300Curves = nullptr;
        }

        void CIGU::setAppliedLoad(std::vector<double> t_AppliedLoad)
        {
            m_DeflectionAppliedLoad = t_AppliedLoad;
            if(m_DeflectionFromE1300Curves != nullptr)
            {
                m_DeflectionFromE1300Curves->setAppliedLoad(std::move(t_AppliedLoad));
            }
        }

    }   // namespace ISO15099

}   // namespace Tarcog
