#pragma once

#include <memory>

namespace SingleLayerOptics
{
    // Hold spherical point data for beam. Phi angle is measured in clockwise direction starting
    // from positive direction of x-axis. Theta angle is measured starting from indicent direction
    // (z-axis) towards x0y plane. Limits of Phi are 0 to 360 degrees and limits of Theta are 0 to
    // 90 degrees
    class CBeamDirection
    {
    public:
        CBeamDirection();
        CBeamDirection(CBeamDirection const & t_BeamDirection);
        CBeamDirection(double t_Theta, double t_Phi);

        [[nodiscard]] double theta() const;
        [[nodiscard]] double phi() const;
        [[nodiscard]] double profileAngle() const;

        CBeamDirection & operator=(const CBeamDirection & t_SphericalPoint);
        bool operator==(const CBeamDirection & t_SphericalPoint) const;
        bool operator!=(const CBeamDirection & t_SphericalPoint) const;

        [[nodiscard]] double distance(double t_Theta, double t_Phi) const;

        [[nodiscard]] double Altitude() const;
        [[nodiscard]] double Azimuth() const;

        [[nodiscard]] CBeamDirection rotate(double angle) const;

    private:
        void updateProfileAngle(double t_Theta, double t_Phi);
        double m_Theta;
        double m_Phi;
        double m_ProfileAngle;
    };

}   // namespace SingleLayerOptics