#pragma once

#include "EnumerationTemplate.hpp"

namespace FenestrationCommon
{

    template<typename T>
    int sgn(T val)
    {
        return (T(0) < val) - (val < T(0));
    }

    //////////////////////////////////////////////////////////////////////////
    // Side
    //////////////////////////////////////////////////////////////////////////

    enum class Side
    {
        Front,
        Back
    };

    class EnumSide : public Enum<Side>
    {};

    inline EnumSide::Iterator begin(EnumSide)
    {
        return EnumSide::Iterator(static_cast<int>(Side::Front));
    }

    inline EnumSide::Iterator end(EnumSide)
    {
        return EnumSide::Iterator(static_cast<int>(Side::Back) + 1);
    }

    //////////////////////////////////////////////////////////////////////////
    // Property
    //////////////////////////////////////////////////////////////////////////

    enum class Property
    {
        T,
        R,
        Abs
    };

    class EnumProperty : public Enum<Property>
    {};

    inline EnumProperty::Iterator begin(EnumProperty)
    {
        return EnumProperty::Iterator(static_cast<int>(Property::T));
    }

    inline EnumProperty::Iterator end(EnumProperty)
    {
        return EnumProperty::Iterator(static_cast<int>(Property::Abs) + 1);
    }

    inline Side oppositeSide(const Side t_Side)
    {
        auto aSide = Side::Front;
        if(t_Side == Side::Front)
        {
            aSide = Side::Back;
        }
        return aSide;
    }

    inline Side getSide(const Side side, const bool flipped)
    {
        return flipped ? oppositeSide(side) : side;
    }

    //////////////////////////////////////////////////////////////////////////
    // WavelengthRange
    //////////////////////////////////////////////////////////////////////////

    enum class WavelengthRange
    {
        Solar,
        Visible,
        IR
    };

    class EnumWavelengthRange : public Enum<WavelengthRange>
    {};

    inline EnumWavelengthRange::Iterator begin(EnumWavelengthRange)
    {
        return EnumWavelengthRange::Iterator(static_cast<int>(WavelengthRange::Solar));
    }

    inline EnumWavelengthRange::Iterator end(EnumWavelengthRange)
    {
        return EnumWavelengthRange::Iterator(static_cast<int>(WavelengthRange::IR) + 1);
    }

    //////////////////////////////////////////////////////////////////////////
    // PropertySimple
    //////////////////////////////////////////////////////////////////////////

    // Short version of enum class Property is necessary because in optical routines it is quite
    // often the case when calculations are performed only over transmittance and reflectance. It is
    // also often the case when Transmittance and Reflectance have different structure from
    // absorptances.
    enum class PropertySimple
    {
        T,
        R
    };

    inline PropertySimple toPropertySimple(const Property prop)
    {
        PropertySimple result{PropertySimple::T};
        if(prop == Property::R)
        {
            result = PropertySimple::R;
        }
        return result;
    }

    inline Property toProperty(const PropertySimple prop)
    {
        Property result{Property::T};
        if(prop == PropertySimple::R)
        {
            result = Property::R;
        }
        return result;
    }

    class EnumPropertySimple : public Enum<PropertySimple>
    {};

    inline EnumPropertySimple::Iterator begin(EnumPropertySimple)
    {
        return EnumPropertySimple::Iterator(static_cast<int>(PropertySimple::T));
    }

    inline EnumPropertySimple::Iterator end(EnumPropertySimple)
    {
        return EnumPropertySimple::Iterator(static_cast<int>(PropertySimple::R) + 1);
    }

    enum class Scattering
    {
        DirectDirect,
        DirectDiffuse,
        DiffuseDiffuse,
        DirectHemispherical
    };

    class EnumScattering : public Enum<Scattering>
    {};

    inline EnumScattering::Iterator begin(EnumScattering)
    {
        return EnumScattering::Iterator(static_cast<int>(Scattering::DirectDirect));
    }

    inline EnumScattering::Iterator end(EnumScattering)
    {
        return EnumScattering::Iterator(static_cast<int>(Scattering::DiffuseDiffuse) + 1);
    }

    //////////////////////////////////////////////////////////////////////////
    // ScatteringSimple
    //////////////////////////////////////////////////////////////////////////

    enum class ScatteringSimple
    {
        Direct,
        Diffuse
    };

    class EnumScatteringSimple : public Enum<ScatteringSimple>
    {};

    inline EnumScatteringSimple::Iterator begin(EnumScatteringSimple)
    {
        return EnumScatteringSimple::Iterator(static_cast<int>(ScatteringSimple::Direct));
    }

    inline EnumScatteringSimple::Iterator end(EnumScatteringSimple)
    {
        return EnumScatteringSimple::Iterator(static_cast<int>(ScatteringSimple::Diffuse) + 1);
    }

    //////////////////////////////////////////////////////////////////////////
    // EnergyFlow
    //////////////////////////////////////////////////////////////////////////

    enum class EnergyFlow
    {
        Forward,
        Backward
    };

    class EnumEnergyFlow : public Enum<EnergyFlow>
    {};

    inline EnumEnergyFlow::Iterator begin(EnumEnergyFlow)
    {
        return EnumEnergyFlow::Iterator(static_cast<int>(EnergyFlow::Forward));
    }

    inline EnumEnergyFlow::Iterator end(EnumEnergyFlow)
    {
        return EnumEnergyFlow::Iterator(static_cast<int>(EnergyFlow::Backward) + 1);
    }

    inline EnergyFlow getFlowFromSide(const Side t_Side)
    {
        auto aResult = EnergyFlow::Forward;
        if(t_Side == Side::Back)
        {
            aResult = EnergyFlow::Backward;
        }

        return aResult;
    }

    inline Side getSideFromFlow(const EnergyFlow t_EnergyFlow)
    {
        auto aResult = Side::Front;
        if(t_EnergyFlow == EnergyFlow::Backward)
        {
            aResult = Side::Back;
        }

        return aResult;
    }

}   // namespace FenestrationCommon

