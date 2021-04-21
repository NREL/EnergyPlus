#include <numeric>
#include "IGUEN673.hpp"
#include "WCECommon.hpp"

namespace Tarcog
{
    namespace EN673
    {
        Glass::Glass(const double Conductivity,
                     const double Thickness,
                     const double emissFront,
                     const double emissBack,
                     const double aSol) :
            Thickness(Thickness),
            Conductivity(Conductivity),
            EmissFront(emissFront),
            EmissBack(emissBack),
            SolarAbsorptance(aSol)
        {}

        Gap::Gap(const double Thickness, const double Pressure, const Gases::CGas & tGas) :
            Thickness(Thickness),
            Pressure(Pressure),
            Gas(tGas)
        {}

        IGU::BaseLayer::BaseLayer(const double thickness, const double t1, const double t2) :
            m_Thickness(thickness),
            T1(t1),
            T2(t2),
            EmissivityFront(0.84),
            EmissivityBack(0.84)
        {}

        double IGU::BaseLayer::getEmissivityFront() const
        {
            return EmissivityFront;
        }

        void IGU::BaseLayer::setEmissivityFront(double tEmissivityFront)
        {
            EmissivityFront = tEmissivityFront;
        }

        double IGU::BaseLayer::getEmissivityBack() const
        {
            return EmissivityBack;
        }

        void IGU::BaseLayer::setEmissivityBack(double tEmissivityBack)
        {
            EmissivityBack = tEmissivityBack;
        }

        void IGU::BaseLayer::updateTemperatures(double t1, double t2)
        {
            T1 = t1;
            T2 = t2;
        }

        IGU::GapLayer::GapLayer(const Gap & gap, double & t1, double & t2) :
            BaseLayer(gap.Thickness, t1, t2),
            pressure(gap.Pressure),
            m_Gas(gap.Gas)
        {}

        double IGU::GapLayer::thermalConductance()
        {
            using ConstantsData::STEFANBOLTZMANN;

            const double Tm = (T1 + T2) / 2.0;

            m_Gas.setTemperatureAndPressure(Tm, pressure);
            const auto prop = m_Gas.getGasProperties();
            const auto convection = prop.m_ThermalConductivity / m_Thickness;
            const auto radiation = 4 * STEFANBOLTZMANN * 1
                                   / (1 / EmissivityFront + 1 / EmissivityBack - 1)
                                   * std::pow(Tm, 3);
            return convection + radiation;
        }

        IGU::SolidLayer::SolidLayer(const Glass & glass, double & t1, double & t2) :
            BaseLayer(glass.Thickness, t1, t2),
            m_Conductivity(glass.Conductivity)
        {}

        double IGU::SolidLayer::thermalConductance()
        {
            return m_Conductivity / m_Thickness;
        }

        Environment::Environment(double Temperature, double filmCoefficient) :
            Temperature(Temperature),
            filmCoefficient(filmCoefficient)
        {}

        IGU::IGU(const Environment & interior, const Environment & exterior) :
            interior(interior),
            exterior(exterior),
            numOfSolidLayers(0)
        {
            // temperature.push_back(exterior.Temperature + 3);
            // temperature.push_back(exterior.Temperature + 6);
            // layers.emplace_back(new SolidLayer(glass, temperature[0], temperature[1]));
        }

        void IGU::addGlass(const Glass & glass)
        {
            if(temperature.size() > 0)
            {
                temperature.push_back(temperature[temperature.size() - 1] + 3);
            }
            else
            {
                temperature.push_back(3);
                temperature.push_back(exterior.Temperature + 6);
                layers.emplace_back(new SolidLayer(glass, temperature[0], temperature[1]));
            }

            abs.push_back(glass.SolarAbsorptance);
            ++numOfSolidLayers;

            if(layers.size() > 1)
            {
                auto gap = layers[layers.size() - 1].get();
                if(dynamic_cast<GapLayer *>(gap))
                {
                    gap->setEmissivityBack(glass.EmissFront);
                    layers.emplace_back(new SolidLayer(glass,
                                                       temperature[temperature.size() - 2],
                                                       temperature[temperature.size() - 1]));
                }
                else
                {
                    throw std::runtime_error("Cannot put two consecutive glass layers to IGU.");
                }
            }
        }

        void IGU::addGap(const Gap & gap)
        {
            temperature.push_back(temperature[temperature.size() - 1] + 3);
            auto solid = layers[layers.size() - 1].get();
            if(dynamic_cast<SolidLayer *>(solid))
            {
                layers.emplace_back(new GapLayer(
                  gap, temperature[temperature.size() - 2], temperature[temperature.size() - 1]));
                layers[layers.size() - 1]->setEmissivityFront(solid->getEmissivityBack());
            }
            else
            {
                throw std::runtime_error("Cannot put two consecutive gap layers to IGU.");
            }
        }

        double IGU::Uvalue()
        {
            double condSum = conductanceSums();
            double condSumNew = 0;
            double ug = 0;

            while(std::abs(condSum - condSumNew) > 1e-4)
            {
                condSum = condSumNew;
                double intExt = 1 / exterior.filmCoefficient + 1 / interior.filmCoefficient;
                auto accumulateFunc = [](double accumulator,
                                         const std::unique_ptr<BaseLayer> & layer) {
                    return accumulator + 1 / layer->thermalConductance();
                };

                ug = 1 / std::accumulate(layers.begin(), layers.end(), intExt, accumulateFunc);
                calculateNewTemperatures(ug * (interior.Temperature - exterior.Temperature));
                updateThermalResistances();
                condSumNew = conductanceSums();
            }

            return ug;
        }

        double IGU::conductanceSums() const
        {
            auto accumulateFunc = [](double accumulator, const std::unique_ptr<BaseLayer> & layer) {
                return accumulator + layer->thermalConductance();
            };
            return std::accumulate(layers.begin(), layers.end(), 0.0, accumulateFunc);
        }

        void IGU::calculateNewTemperatures(double scaleFactor)
        {
            temperature[0] = scaleFactor / exterior.filmCoefficient + exterior.Temperature;
            temperature[temperature.size() - 1] =
              interior.Temperature - scaleFactor / interior.filmCoefficient;
            for(size_t i = 0u; i < layers.size() - 1; ++i)
            {
                temperature[i + 1] = scaleFactor / layers[i]->thermalConductance() + temperature[i];
            }
            updateLayerTemperatures();
        }

        void IGU::updateThermalResistances()
        {
            thermalResistance.clear();
            thermalResistance.push_back(1 / exterior.filmCoefficient);
            for(auto & layer : layers)
            {
                thermalResistance.push_back(1 / layer->thermalConductance());
            }
            thermalResistance.push_back(1 / interior.filmCoefficient);
        }

        double IGU::shgc(const double totSol)
        {
            std::vector<double> lambdaCoeff(numOfSolidLayers - 1, 0);
            double cNom{0};
            double cDen{0};

            double cAbs{0};

            // Need to set correct thermal resistances and for that Uvalue calculation is needed.
            Uvalue();

            for(size_t i = numOfSolidLayers - 1; i-- > 0;)
            {
                auto j = 2u * (i + 1u);
                double k1{0.5};
                double k2{0.5};

                cAbs += abs[i];

                if(i == 0)
                {
                    k1 = 1;
                }

                if(i == (numOfSolidLayers - 2))
                {
                    k2 = 1;
                }

                lambdaCoeff[i] = 1
                                 / (k1 * thermalResistance[j - 1] + thermalResistance[j]
                                    + k2 * thermalResistance[j + 1]);

                cNom += cAbs / lambdaCoeff[i];
                cDen += 1 / lambdaCoeff[i];
            }

            cAbs += abs[0];
            double flowin =
              (cAbs * thermalResistance[0] + cNom)
              / (thermalResistance[0] + thermalResistance[2 * numOfSolidLayers] + cDen);

            return flowin + totSol;
        }

        void IGU::updateLayerTemperatures()
        {
            for(size_t i = 0u; i < layers.size(); ++i)
            {
                layers[i]->updateTemperatures(temperature[i], temperature[i + 1]);
            }
        }

        std::unique_ptr<IGU> IGU::create(const Environment & interior, const Environment & exterior)
        {
            return std::unique_ptr<IGU>(new IGU(interior, exterior));
        }
    }   // namespace EN673
}   // namespace Tarcog
