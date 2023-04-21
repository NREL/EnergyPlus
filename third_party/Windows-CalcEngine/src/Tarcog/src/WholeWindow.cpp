#include "WholeWindow.hpp"

#include <utility>

namespace Tarcog::ISO15099
{
    ////////////////////////////////////////////////
    /// WindowSingleVision
    ////////////////////////////////////////////////
    WindowSingleVision::WindowSingleVision(double width,
                                           double height,
                                           double tvis,
                                           double tsol,
                                           std::shared_ptr<IIGUSystem> iguSystem) :
        vision(width, height, tvis, tsol, std::move(iguSystem))
    {}

    double WindowSingleVision::area() const
    {
        return vision.area();
    }

    double WindowSingleVision::uValue() const
    {
        return vision.uValue();
    }

    double WindowSingleVision::shgc() const
    {
        return vision.shgc();
    }

    double WindowSingleVision::shgc(const double tSol) const
    {
        return vision.shgc(tSol);
    }

    double WindowSingleVision::vt() const
    {
        return vision.vt();
    }

    double WindowSingleVision::vt(const double tVis) const
    {
        return vision.vt(tVis);
    }

    double WindowSingleVision::uValueCOG() const
    {
        return vision.uValueCOG();
    }

    double WindowSingleVision::shgcCOG() const
    {
        return vision.shgcCOG();
    }

    double WindowSingleVision::uValueCOGAverage() const
    {
        return vision.uValueCOG();
    }

    double WindowSingleVision::shgcCOGAverage() const
    {
        return vision.shgcCOG();
    }

    double WindowSingleVision::visionPercentage() const
    {
        return vision.visionPercentage();
    }

    void WindowSingleVision::setFrameTop(FrameData frameData)
    {
        vision.setFrameData(FramePosition::Top, frameData);
    }

    void WindowSingleVision::setFrameBottom(FrameData frameData)
    {
        vision.setFrameData(FramePosition::Bottom, frameData);
    }

    void WindowSingleVision::setFrameLeft(FrameData frameData)
    {
        vision.setFrameData(FramePosition::Left, frameData);
    }

    void WindowSingleVision::setFrameRight(FrameData frameData)
    {
        vision.setFrameData(FramePosition::Right, frameData);
    }

    void WindowSingleVision::setDividers(FrameData frameData, size_t nHorizontal, size_t nVertical)
    {
        vision.setDividers(frameData, nHorizontal, nVertical);
    }

    IGUDimensions WindowSingleVision::getIGUDimensions() const
    {
        return {vision.getIGUWidth(), vision.getIGUHeight()};
    }

    ////////////////////////////////////////////////
    /// WindowDualVision
    ////////////////////////////////////////////////

    WindowDualVision::WindowDualVision(double width,
                                       double height,
                                       double tvis1,
                                       double tsol1,
                                       std::shared_ptr<IIGUSystem> iguSystem1,
                                       double tvis2,
                                       double tsol2,
                                       std::shared_ptr<IIGUSystem> iguSystem2) :
        m_Vision1(width, height, tvis1, tsol1, std::move(iguSystem1)),
        m_Vision2(width, height, tvis2, tsol2, std::move(iguSystem2))
    {
        averageHc();
    }

    double WindowDualVision::area() const
    {
        return m_Vision1.area() + m_Vision2.area();
    }

    double WindowDualVision::uValue() const
    {
        return (m_Vision1.uValue() * m_Vision1.area() + m_Vision2.uValue() * m_Vision2.area())
               / area();
    }

    double WindowDualVision::shgc() const
    {
        return (m_Vision1.shgc() * m_Vision1.area() + m_Vision2.shgc() * m_Vision2.area()) / area();
    }

    double WindowDualVision::shgc(const double tSol1, const double tSol2) const
    {
        return (m_Vision1.shgc(tSol1) * m_Vision1.area() + m_Vision2.shgc(tSol2) * m_Vision2.area())
               / area();
    }

    double WindowDualVision::shgc(const double tSol) const
    {
        return shgc(tSol, tSol);
    }

    double WindowDualVision::vt() const
    {
        return (m_Vision1.vt() * m_Vision1.area() + m_Vision2.vt() * m_Vision2.area()) / area();
    }

    double WindowDualVision::vt(double tVis1, double tVis2) const
    {
        return (m_Vision1.vt(tVis1) * m_Vision1.area() + m_Vision2.vt(tVis2) * m_Vision2.area())
               / area();
    }

    double WindowDualVision::vt(const double tVis) const
    {
        return vt(tVis, tVis);
    }

    double WindowDualVision::uValueCOGAverage() const
    {
        return (m_Vision1.uValueCOG() * m_Vision1.area() + m_Vision2.uValueCOG() * m_Vision2.area())
               / area();
    }

    double WindowDualVision::shgcCOGAverage() const
    {
        return (m_Vision1.shgcCOG() * m_Vision1.area() + m_Vision2.shgcCOG() * m_Vision2.area())
               / area();
    }

    IGUDimensions WindowDualVision::getIGUDimensions() const
    {
        return {m_Vision1.getIGUWidth(), m_Vision1.getIGUHeight()};
    }

    double WindowDualVision::visionPercentage() const
    {
        return (m_Vision1.visionPercentage() * m_Vision1.area()
                + m_Vision2.visionPercentage() * m_Vision2.area())
               / area();
    }

    double WindowDualVision::uValueCOG1() const
    {
        return m_Vision1.uValueCOG();
    }

    double WindowDualVision::uValueCOG2() const
    {
        return m_Vision2.uValueCOG();
    }

    double WindowDualVision::shgcCOG1() const
    {
        return m_Vision1.shgcCOG();
    }

    double WindowDualVision::shgcCOG2() const
    {
        return m_Vision2.shgcCOG();
    }

    void WindowDualVision::averageHc()
    {
        const auto hc1{m_Vision1.hc()};
        const auto hc2{m_Vision2.hc()};
        const auto hcavg{(hc1 + hc2) / 2};
        m_Vision1.setHc(hcavg);
        m_Vision2.setHc(hcavg);
    }

    ////////////////////////////////////////////////
    /// DualVisionHorizontal
    ////////////////////////////////////////////////

    DualVisionHorizontal::DualVisionHorizontal(double width,
                                               double height,
                                               double tvis1,
                                               double tsol1,
                                               const std::shared_ptr<IIGUSystem> & iguSystem1,
                                               double tvis2,
                                               double tsol2,
                                               const std::shared_ptr<IIGUSystem> & iguSystem2) :
        WindowDualVision(width / 2, height, tvis1, tsol1, iguSystem1, tvis2, tsol2, iguSystem2)
    {
        const std::map<FramePosition, FrameType> leftVisionFrameTypes{
          {FramePosition::Top, FrameType::Exterior},
          {FramePosition::Bottom, FrameType::Exterior},
          {FramePosition::Left, FrameType::Exterior},
          {FramePosition::Right, FrameType::Interior}};
        m_Vision1.setFrameTypes(leftVisionFrameTypes);

        const std::map<FramePosition, FrameType> rightVisionFrameTypes{
          {FramePosition::Top, FrameType::Exterior},
          {FramePosition::Bottom, FrameType::Exterior},
          {FramePosition::Left, FrameType::Interior},
          {FramePosition::Right, FrameType::Exterior}};
        m_Vision2.setFrameTypes(rightVisionFrameTypes);
    }

    double DualVisionHorizontal::uValueCOGLeft() const
    {
        return uValueCOG1();
    }

    double DualVisionHorizontal::uValueCOGRight() const
    {
        return uValueCOG2();
    }

    double DualVisionHorizontal::shgcCOGLeft() const
    {
        return shgcCOG1();
    }

    double DualVisionHorizontal::shgcCOGRight() const
    {
        return shgcCOG2();
    }

    void DualVisionHorizontal::setFrameTopLeft(FrameData frameData)
    {
        m_Vision1.setFrameData(FramePosition::Top, frameData);
    }

    void DualVisionHorizontal::setFrameTopRight(FrameData frameData)
    {
        m_Vision2.setFrameData(FramePosition::Top, frameData);
    }

    void DualVisionHorizontal::setFrameBottomLeft(FrameData frameData)
    {
        m_Vision1.setFrameData(FramePosition::Bottom, frameData);
    }

    void DualVisionHorizontal::setFrameBottomRight(FrameData frameData)
    {
        m_Vision2.setFrameData(FramePosition::Bottom, frameData);
    }

    void DualVisionHorizontal::setFrameLeft(FrameData frameData)
    {
        m_Vision1.setFrameData(FramePosition::Left, frameData);
    }

    void DualVisionHorizontal::setFrameRight(FrameData frameData)
    {
        m_Vision2.setFrameData(FramePosition::Right, frameData);
    }

    void DualVisionHorizontal::setFrameMeetingRail(FrameData frameData)
    {
        frameData.splitFrameWidth();
        m_Vision1.setFrameData(FramePosition::Right, frameData);
        m_Vision2.setFrameData(FramePosition::Left, frameData);
    }

    void
      DualVisionHorizontal::setDividers(FrameData frameData, size_t nHorizontal, size_t nVertical)
    {
        m_Vision1.setDividers(frameData, nHorizontal, nVertical);
        m_Vision2.setDividers(frameData, nHorizontal, nVertical);
    }

    void DualVisionHorizontal::setDividersLeftVision(FrameData frameData,
                                                     size_t nHorizontal,
                                                     size_t nVertical)
    {
        m_Vision1.setDividers(frameData, nHorizontal, nVertical);
    }

    void DualVisionHorizontal::setDividersRightVision(FrameData frameData,
                                                      size_t nHorizontal,
                                                      size_t nVertical)
    {
        m_Vision2.setDividers(frameData, nHorizontal, nVertical);
    }

    ////////////////////////////////////////////////
    /// DualVisionVertical
    ////////////////////////////////////////////////

    DualVisionVertical::DualVisionVertical(double width,
                                           double height,
                                           double tvis1,
                                           double tsol1,
                                           const std::shared_ptr<IIGUSystem> & iguSystem1,
                                           double tvis2,
                                           double tsol2,
                                           const std::shared_ptr<IIGUSystem> & iguSystem2) :
        WindowDualVision(width, height / 2, tvis1, tsol1, iguSystem1, tvis2, tsol2, iguSystem2)
    {
        const std::map<FramePosition, FrameType> topVisionFrameTypes{
          {FramePosition::Top, FrameType::Exterior},
          {FramePosition::Bottom, FrameType::Interior},
          {FramePosition::Left, FrameType::Exterior},
          {FramePosition::Right, FrameType::Exterior}};
        m_Vision1.setFrameTypes(topVisionFrameTypes);

        const std::map<FramePosition, FrameType> bottomVisionFrameTypes{
          {FramePosition::Top, FrameType::Interior},
          {FramePosition::Bottom, FrameType::Exterior},
          {FramePosition::Left, FrameType::Exterior},
          {FramePosition::Right, FrameType::Exterior}};
        m_Vision2.setFrameTypes(bottomVisionFrameTypes);

        // In case of vertical vision we must adjust exterior surface height for film coefficient
        // calculations since visions will scale that coefficient only to their heights.
        m_Vision1.setInteriorAndExteriorSurfaceHeight(height);
        m_Vision2.setInteriorAndExteriorSurfaceHeight(height);
    }

    double DualVisionVertical::uValueCOGTop() const
    {
        return uValueCOG1();
    }

    double DualVisionVertical::uValueCOGBottom() const
    {
        return uValueCOG2();
    }

    double DualVisionVertical::shgcCOGTop() const
    {
        return shgcCOG1();
    }

    double DualVisionVertical::shgcCOGBottom() const
    {
        return shgcCOG2();
    }

    void DualVisionVertical::setFrameMeetingRail(FrameData frameData)
    {
        frameData.splitFrameWidth();
        m_Vision1.setFrameData(FramePosition::Bottom, frameData);
        m_Vision2.setFrameData(FramePosition::Top, frameData);
    }

    void DualVisionVertical::setDividers(FrameData frameData, size_t nHorizontal, size_t nVertical)
    {
        m_Vision1.setDividers(frameData, nHorizontal, nVertical);
        m_Vision2.setDividers(frameData, nHorizontal, nVertical);
    }

    void DualVisionVertical::setDividersTopVision(FrameData frameData,
                                                  size_t nHorizontal,
                                                  size_t nVertical)
    {
        m_Vision1.setDividers(frameData, nHorizontal, nVertical);
    }

    void DualVisionVertical::setDividersBottomVision(FrameData frameData,
                                                     size_t nHorizontal,
                                                     size_t nVertical)
    {
        m_Vision2.setDividers(frameData, nHorizontal, nVertical);
    }

    void DualVisionVertical::setFrameBottomRight(FrameData frameData)
    {
        m_Vision2.setFrameData(FramePosition::Right, frameData);
    }

    void DualVisionVertical::setFrameBottomLeft(FrameData frameData)
    {
        m_Vision2.setFrameData(FramePosition::Left, frameData);
    }

    void DualVisionVertical::setFrameTopRight(FrameData frameData)
    {
        m_Vision1.setFrameData(FramePosition::Right, frameData);
    }

    void DualVisionVertical::setFrameTopLeft(FrameData frameData)
    {
        m_Vision1.setFrameData(FramePosition::Left, frameData);
    }

    void DualVisionVertical::setFrameBottom(FrameData frameData)
    {
        m_Vision2.setFrameData(FramePosition::Bottom, frameData);
    }

    void DualVisionVertical::setFrameTop(FrameData frameData)
    {
        m_Vision1.setFrameData(FramePosition::Top, frameData);
    }
}   // namespace Tarcog::ISO15099
