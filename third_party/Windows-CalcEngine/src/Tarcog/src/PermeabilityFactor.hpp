#pragma once

namespace ThermalPermeability
{
    namespace Venetian
    {
        double maxAngle(double t_SlatSpacing, double t_MatThickness);

        double calculateRise(double t_Curvature, double t_SlatWidth);

        double openness(double t_TiltAngle,
                        double t_SlatSpacing,
                        double t_MatThickness,
                        double t_SlatCurvature,
                        double t_SlatWidth);
    }   // namespace Venetian

    namespace Perforated
    {
        enum class Geometry
        {
            Circular,
            Square,
            Rectangular
        };

        struct XYDimension
        {
            XYDimension(double x, double y);
            double x;
            double y;
        };

        XYDimension diameterToXYDimension(double diameter);

        double openness(Geometry t_Geometry,
                        double t_SpacingX,
                        double t_SpacingY,
                        double t_DimensionX,
                        double t_DimensionY);
    }   // namespace Perforated

    namespace Woven
    {
        double openness(double t_Diameter, double t_Spacing);
    }
}   // namespace ThermalPermeability