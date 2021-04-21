#pragma once

#include <map>
#include <optional>

#include "IGUConfigurations.hpp"
#include "WholeWindowConfigurations.hpp"
#include "Frame.hpp"

namespace Tarcog
{
    namespace ISO15099
    {
        class WindowVision : public IVision
        {
        public:
            WindowVision() = default;
            WindowVision(const WindowVision & vision) = default;
            WindowVision(double width, double height, double tvis, double tsol, std::shared_ptr<IIGUSystem> iguSystem);
            [[nodiscard]] double area() const override;
            [[nodiscard]] double uValue() const override;
            //! Returns solar transmittance for the default IGU solar transmittance.
            [[nodiscard]] double shgc() const override;
            [[nodiscard]] double shgc(double tSol) const override;
            //! Returns visible transmittance for the default IGU visible transmittance
            [[nodiscard]] double vt() const override;
            [[nodiscard]] double vt(double tVis) const override;
            [[nodiscard]] double visionPercentage() const override;
            [[nodiscard]] double hc() const override;
            [[nodiscard]] double uValueCOG() const;
            [[nodiscard]] double shgcCOG() const;
            void setHc(double hc) override;
            
            void setFrameData(FramePosition position, FrameData frameData);

            void setFrameTypes(std::map<FramePosition, FrameType> frameTypes);

            void setDividers(FrameData divider, size_t nHorizontal, size_t nVertical);

            void setInteriorAndExteriorSurfaceHeight(double height);

            [[nodiscard]] double getIGUWidth() const;
            [[nodiscard]] double getIGUHeight() const;

        private:
            //! Makes connection between frames for correct area calculations.
            void connectFrames();

            //! Resizes IGU according to frames that are currently set in the vision
            void resizeIGU();

            //! Returns total area of dividers assigned to this window vision area
            [[nodiscard]] double dividerArea() const;
            [[nodiscard]] double dividerEdgeArea() const;

            [[nodiscard]] double frameProjectedArea() const;
            [[nodiscard]] double edgeOfGlassArea() const;

            std::shared_ptr<IIGUSystem> m_IGUSystem;

            double m_Width{0};
            double m_Height{0};
            double m_IGUUvalue{0};
            double m_VT{1};
            double m_Tsol{0};
            double m_HcExterior{0};

            //! Exterior surface height is used to calculate exterior film coefficient.
            //! Since vision can be part of any construction, this variable is kept internally.
            double m_ExteriorSurfaceHeight{0};

            std::map<FramePosition, Frame> m_Frame;

            size_t m_NumOfVerticalDividers{0u};
            size_t m_NumOfHorizontalDividers{0u}; 
            std::optional<FrameData> m_Divider;

        };
    }   // namespace ISO15099
}   // namespace Tarcog
