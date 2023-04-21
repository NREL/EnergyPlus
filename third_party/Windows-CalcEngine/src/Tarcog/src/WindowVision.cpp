#include "WindowVision.hpp"

#include "EnvironmentConfigurations.hpp"

namespace Tarcog::ISO15099
{
    WindowVision::WindowVision(double width,
                               double height,
                               double tvis,
                               double tsol,
                               std::shared_ptr<IIGUSystem> iguSystem) :
        m_IGUSystem(std::move(iguSystem)),
        m_Width(width),
        m_Height(height),
        m_VT(tvis),
        m_Tsol(tsol),
        m_ExteriorSurfaceHeight(height),
        m_Frame({{FramePosition::Top, {width}},
                 {FramePosition::Bottom, {width}},
                 {FramePosition::Left, {height}},
                 {FramePosition::Right, {height}}})
    {
        m_IGUSystem->setWidthAndHeight(width, height);
        m_IGUSystem->setInteriorAndExteriorSurfacesHeight(m_ExteriorSurfaceHeight);
        m_IGUUvalue = m_IGUSystem->getUValue();
        m_HExterior = m_IGUSystem->getH(System::SHGC, Environment::Outdoor);
    }

    double WindowVision::uValue() const
    {
        auto frameWeightedUValue{0.0};
        auto edgeOfGlassWeightedUValue{0.0};

        for(const auto & [key, frame] : m_Frame)
        {
            std::ignore = key;
            frameWeightedUValue += frame.projectedArea() * frame.frameData().UValue;
            edgeOfGlassWeightedUValue += frame.edgeOfGlassArea() * frame.frameData().EdgeUValue;
        }

        const auto COGWeightedUValue{m_IGUUvalue
                                     * (area() - frameProjectedArea() - edgeOfGlassArea()
                                        - dividerArea() - dividerEdgeArea())};

        auto dividerWeightedUValue{0.0};
        auto dividerWeightedEdgeUValue{0.0};
        if(m_Divider.has_value())
        {
            dividerWeightedUValue += dividerArea() * m_Divider->UValue;
            dividerWeightedEdgeUValue += dividerEdgeArea() * m_Divider->EdgeUValue;
        }

        return (COGWeightedUValue + frameWeightedUValue + edgeOfGlassWeightedUValue
                + dividerWeightedUValue + dividerWeightedEdgeUValue)
               / area();
    }

    double WindowVision::shgc() const
    {
        return shgc(m_Tsol);
    }

    double WindowVision::shgc(const double tSol) const
    {
        auto frameWeightedSHGC{0.0};

        for(const auto & [key, frame] : m_Frame)
        {
            std::ignore = key;
            frameWeightedSHGC += frame.projectedArea() * frame.frameData().shgc(m_HExterior);
        }

        const auto COGWeightedSHGC{m_IGUSystem->getSHGC(tSol)
                                   * (area() - frameProjectedArea() - dividerArea())};

        auto dividerWeightedSHGC{0.0};
        if(m_Divider.has_value())
        {
            dividerWeightedSHGC += dividerArea() * m_Divider->shgc(m_HExterior);
        }

        return (COGWeightedSHGC + frameWeightedSHGC + dividerWeightedSHGC) / area();
    }

    double WindowVision::area() const
    {
        return m_Width * m_Height;
    }

    double WindowVision::vt() const
    {
        return vt(m_VT);
    }

    double WindowVision::vt(const double tVis) const
    {
        return visionPercentage() * tVis;
    }

    double WindowVision::visionPercentage() const
    {
        return (area() - frameProjectedArea() - dividerArea()) / area();
    }

    double WindowVision::hc() const
    {
        return m_HExterior;
    }

    double WindowVision::uValueCOG() const
    {
        return m_IGUUvalue;
    }

    double WindowVision::shgcCOG() const
    {
        return m_IGUSystem->getSHGC(m_Tsol);
    }

    void WindowVision::setHc(double hc)
    {
        m_HExterior = hc;
    }

    void WindowVision::setFrameData(FramePosition position, FrameData frameData)
    {
        m_Frame.at(position).setFrameData(frameData);

        connectFrames();
        resizeIGU();
    }

    void WindowVision::setFrameTypes(std::map<FramePosition, FrameType> frameTypes)
    {
        for(const auto & [position, type] : frameTypes)
        {
            if(m_Frame.count(position) > 0u)
            {
                m_Frame.at(position).setFrameType(type);
            }
        }

        connectFrames();
    }

    void WindowVision::setDividers(FrameData divider, size_t nHorizontal, size_t nVertical)
    {
        m_Divider = divider;
        m_NumOfHorizontalDividers = nHorizontal;
        m_NumOfVerticalDividers = nVertical;
        std::map<FramePosition, size_t> numOfDivs{
          {FramePosition::Top, m_NumOfVerticalDividers},
          {FramePosition::Bottom, m_NumOfVerticalDividers},
          {FramePosition::Left, m_NumOfHorizontalDividers},
          {FramePosition::Right, m_NumOfHorizontalDividers}};

        for(auto & [key, frame] : m_Frame)
        {
            frame.assignDividerArea(m_Divider->ProjectedFrameDimension * ConstantsData::EOGHeight,
                                    numOfDivs.at(key));
        }
    }

    void WindowVision::setInteriorAndExteriorSurfaceHeight(const double height)
    {
        m_ExteriorSurfaceHeight = height;

        m_IGUSystem->setInteriorAndExteriorSurfacesHeight(m_ExteriorSurfaceHeight);

        m_IGUUvalue = m_IGUSystem->getUValue();
        m_HExterior = m_IGUSystem->getH(System::SHGC, Environment::Outdoor);
    }

    double WindowVision::getIGUWidth() const
    {
        return m_Width - m_Frame.at(FramePosition::Left).projectedFrameDimension()
               - m_Frame.at(FramePosition::Right).projectedFrameDimension();
    }

    double WindowVision::getIGUHeight() const
    {
        return m_Height - m_Frame.at(FramePosition::Top).projectedFrameDimension()
               - m_Frame.at(FramePosition::Bottom).projectedFrameDimension();
    }

    void WindowVision::connectFrames()
    {
        m_Frame.at(FramePosition::Top)
          .assignFrame(m_Frame.at(FramePosition::Right), FrameSide::Left);
        m_Frame.at(FramePosition::Top)
          .assignFrame(m_Frame.at(FramePosition::Left), FrameSide::Right);

        m_Frame.at(FramePosition::Bottom)
          .assignFrame(m_Frame.at(FramePosition::Right), FrameSide::Right);
        m_Frame.at(FramePosition::Bottom)
          .assignFrame(m_Frame.at(FramePosition::Left), FrameSide::Left);

        m_Frame.at(FramePosition::Left)
          .assignFrame(m_Frame.at(FramePosition::Top), FrameSide::Left);
        m_Frame.at(FramePosition::Left)
          .assignFrame(m_Frame.at(FramePosition::Bottom), FrameSide::Right);

        m_Frame.at(FramePosition::Right)
          .assignFrame(m_Frame.at(FramePosition::Bottom), FrameSide::Left);
        m_Frame.at(FramePosition::Right)
          .assignFrame(m_Frame.at(FramePosition::Top), FrameSide::Right);
    }

    void WindowVision::resizeIGU()
    {
        const auto width{m_Width - m_Frame.at(FramePosition::Left).projectedFrameDimension()
                         - m_Frame.at(FramePosition::Right).projectedFrameDimension()};
        const auto height{m_Height - m_Frame.at(FramePosition::Top).projectedFrameDimension()
                          - m_Frame.at(FramePosition::Bottom).projectedFrameDimension()};
        m_IGUSystem->setWidthAndHeight(width, height);
        m_IGUSystem->setInteriorAndExteriorSurfacesHeight(m_ExteriorSurfaceHeight);
        m_IGUUvalue = m_IGUSystem->getUValue();
        m_HExterior = m_IGUSystem->getH(System::SHGC, Environment::Outdoor);
    }

    double WindowVision::dividerArea() const
    {
        auto result{0.0};

        if(m_Divider.has_value())
        {
            const auto dividersWidth{m_Width
                                     - m_Frame.at(FramePosition::Left).projectedFrameDimension()
                                     - m_Frame.at(FramePosition::Right).projectedFrameDimension()};
            const auto dividersHeight{
              m_Height - m_Frame.at(FramePosition::Top).projectedFrameDimension()
              - m_Frame.at(FramePosition::Bottom).projectedFrameDimension()};
            const auto areaVertical{m_NumOfVerticalDividers * dividersHeight
                                    * m_Divider->ProjectedFrameDimension};
            const auto areaHorizontal{m_NumOfHorizontalDividers * dividersWidth
                                      * m_Divider->ProjectedFrameDimension};
            const auto areaDoubleCounted{m_NumOfHorizontalDividers * m_NumOfVerticalDividers
                                         * std::pow(m_Divider->ProjectedFrameDimension, 2)};
            result = areaVertical + areaHorizontal - areaDoubleCounted;
        }

        return result;
    }

    double WindowVision::dividerEdgeArea() const
    {
        auto result{0.0};

        if(m_Divider.has_value())
        {
            const auto eogWidth{m_Width - m_Frame.at(FramePosition::Left).projectedFrameDimension()
                                - m_Frame.at(FramePosition::Right).projectedFrameDimension()
                                - 2 * ConstantsData::EOGHeight};
            const auto eogHeight{m_Height - m_Frame.at(FramePosition::Top).projectedFrameDimension()
                                 - m_Frame.at(FramePosition::Bottom).projectedFrameDimension()
                                 - 2 * ConstantsData::EOGHeight};
            const auto areaVertical{m_NumOfVerticalDividers * 2 * ConstantsData::EOGHeight
                                    * eogHeight};
            const auto areaHorizontal{m_NumOfHorizontalDividers * 2 * ConstantsData::EOGHeight
                                      * eogWidth};
            const auto dividerAreaSubtract{4 * m_NumOfVerticalDividers * m_NumOfHorizontalDividers
                                           * m_Divider->ProjectedFrameDimension
                                           * ConstantsData::EOGHeight};
            const auto eogAreaSubtract{4 * m_NumOfVerticalDividers * m_NumOfHorizontalDividers
                                       * ConstantsData::EOGHeight * ConstantsData::EOGHeight};

            result = areaVertical + areaHorizontal - dividerAreaSubtract - eogAreaSubtract;
        }

        return result;
    }

    double WindowVision::frameProjectedArea() const
    {
        auto area{0.0};

        for(const auto & system : m_Frame)
        {
            area += system.second.projectedArea();
        }

        return area;
    }

    double WindowVision::edgeOfGlassArea() const
    {
        auto area{0.0};

        for(const auto & [key, frame] : m_Frame)
        {
            std::ignore = key;
            area += frame.edgeOfGlassArea();
        }

        return area;
    }
}   // namespace Tarcog::ISO15099
