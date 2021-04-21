#pragma once

#include "WholeWindowConfigurations.hpp"
#include "WindowVision.hpp"

namespace Tarcog
{
    namespace ISO15099
    {
        ////////////////////////////////////////////////
        /// WindowSingleVision
        ////////////////////////////////////////////////

        class WindowSingleVision : public IWindow
        {
        public:
            WindowSingleVision() = default;

            WindowSingleVision(double width,
                               double height,
                               double tvis,
                               double tsol,
                               std::shared_ptr<IIGUSystem> iguSystem);

            [[nodiscard]] double area() const override;
            [[nodiscard]] double uValue() const override;
            [[nodiscard]] double shgc() const override;
            [[nodiscard]] double shgc(double tSol) const override;
            [[nodiscard]] double vt() const override;
            [[nodiscard]] double vt(double tVis) const override;
            [[nodiscard]] double uValueCOG() const;
            [[nodiscard]] double shgcCOG() const;

            [[nodiscard]] double uValueCOGAverage() const override;
            [[nodiscard]] double shgcCOGAverage() const override;

            void setFrameTop(FrameData frameData);
            void setFrameBottom(FrameData frameData);
            void setFrameLeft(FrameData frameData);
            void setFrameRight(FrameData frameData);
            void setDividers(FrameData frameData, size_t nHorizontal, size_t nVertical);


            [[nodiscard]] IGUDimensions getIGUDimensions() const override;

        protected:
            [[nodiscard]] double visionPercentage() const override;

        private:            

            WindowVision vision;
        };

        ////////////////////////////////////////////////
        /// WindowDualVision
        ////////////////////////////////////////////////

        class WindowDualVision : public IWindow
        {
        public:
            WindowDualVision() = default;

            [[nodiscard]] double area() const override;
            [[nodiscard]] double uValue() const override;
            [[nodiscard]] double shgc() const override;
            [[nodiscard]] double shgc(double tSol1, double tSol2) const;
            //! Both vision areas have same solar transmittance
            [[nodiscard]] double shgc(double tSol) const override;
            [[nodiscard]] double vt() const override;
            [[nodiscard]] double vt(double tVis1, double tVis2) const;
            //! Both vision areas have same visible transmittance
            [[nodiscard]] double vt(double tVis) const override;

            [[nodiscard]] double uValueCOGAverage() const override;
            [[nodiscard]] double shgcCOGAverage() const override;

            [[nodiscard]] IGUDimensions getIGUDimensions() const override;

        protected:
            WindowDualVision(double width,
                             double height,
                             double tvis1,
                             double tsol1,
                             std::shared_ptr<IIGUSystem> iguSystem1,
                             double tvis2,
                             double tsol2,
                             std::shared_ptr<IIGUSystem> iguSystem2);

            [[nodiscard]] double visionPercentage() const override;

            [[nodiscard]] double uValueCOG1() const;
            [[nodiscard]] double uValueCOG2() const;
            [[nodiscard]] double shgcCOG1() const;
            [[nodiscard]] double shgcCOG2() const;

            WindowVision m_Vision1;
            WindowVision m_Vision2;

        private:
            //! Makes single convective film coefficient averaged over the both visions
            void averageHc();
        };

        ////////////////////////////////////////////////
        /// DualVisionHorizontal
        ////////////////////////////////////////////////

        //! Concrete implementation of dual vision horizontal slider
        //! Vision1 is the vision on the left side and Vision2 is the vision on the right side.
        class DualVisionHorizontal : public WindowDualVision
        {
        public:
            DualVisionHorizontal() = default;

            DualVisionHorizontal(double width,
                                 double height,
                                 double tvis1,
                                 double tsol1,
                                 const std::shared_ptr<IIGUSystem> & iguSystem1,
                                 double tvis2,
                                 double tsol2,
                                 const std::shared_ptr<IIGUSystem> & iguSystem2);

            [[nodiscard]] double uValueCOGLeft() const;
            [[nodiscard]] double uValueCOGRight() const;
            [[nodiscard]] double shgcCOGLeft() const;
            [[nodiscard]] double shgcCOGRight() const;

            void setFrameTopLeft(FrameData frameData);
            void setFrameTopRight(FrameData frameData);
            void setFrameBottomLeft(FrameData frameData);
            void setFrameBottomRight(FrameData frameData);
            void setFrameLeft(FrameData frameData);
            void setFrameRight(FrameData frameData);
            void setFrameMeetingRail(FrameData frameData);

            void setDividers(FrameData frameData, size_t nHorizontal, size_t nVertical);

            void setDividersLeftVision(FrameData frameData, size_t nHorizontal, size_t nVertical);
            void setDividersRightVision(FrameData frameData, size_t nHorizontal, size_t nVertical);
        };

        ////////////////////////////////////////////////
        /// DualVisionVertical
        ////////////////////////////////////////////////

        //! Concrete implementation of dual vision vertical slider
        //! Vision1 is the vision on the top and Vision2 is the vision on the bottom.
        class DualVisionVertical : public WindowDualVision
        {
        public:
            DualVisionVertical() = default;

            DualVisionVertical(double width,
                               double height,
                               double tvis1,
                               double tsol1,
                               const std::shared_ptr<IIGUSystem> & iguSystem1,
                               double tvis2,
                               double tsol2,
                               const std::shared_ptr<IIGUSystem> & iguSystem2);

            [[nodiscard]] double uValueCOGTop() const;
            [[nodiscard]] double uValueCOGBottom() const;
            [[nodiscard]] double shgcCOGTop() const;
            [[nodiscard]] double shgcCOGBottom() const;

            void setFrameTop(FrameData frameData);
            void setFrameBottom(FrameData frameData);
            void setFrameTopLeft(FrameData frameData);
            void setFrameTopRight(FrameData frameData);
            void setFrameBottomLeft(FrameData frameData);
            void setFrameBottomRight(FrameData frameData);
            void setFrameMeetingRail(FrameData frameData);

            void setDividers(FrameData frameData, size_t nHorizontal, size_t nVertical);

            void setDividersTopVision(FrameData frameData, size_t nHorizontal, size_t nVertical);
            void setDividersBottomVision(FrameData frameData, size_t nHorizontal, size_t nVertical);
        };
    }   // namespace ISO15099
}   // namespace Tarcog
