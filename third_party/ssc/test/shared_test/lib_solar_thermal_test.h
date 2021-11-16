#ifndef __LIB_CSP_TEST_H__
#define __LIB_CSP_TEST_H__

#include <gtest/gtest.h>
#include "../tcs/flat_plate_solar_collector.h"

namespace solar_thermal
{
    const double kErrorToleranceLo = 0.001;    // 0.1%
    const double kErrorToleranceHi = 0.01;     // 1.0%

    class FpcFactory {
    public:
        FpcFactory() {};

        virtual std::unique_ptr<FlatPlateArray> MakeFpcArray() const = 0;
        std::unique_ptr<FlatPlateArray> MakeFpcArray(FlatPlateCollector* flat_plate_collector,
            CollectorLocation* collector_location,
            CollectorOrientation* collector_orientation,
            ArrayDimensions* array_dimensions,
            Pipe* inlet_pipe,
            Pipe* outlet_pipe) const;
        virtual std::unique_ptr<FlatPlateCollector> MakeCollector() const = 0;
        std::unique_ptr<FlatPlateCollector> MakeCollector(CollectorTestSpecifications* collector_test_specifications) const;
        std::unique_ptr<TimeAndPosition> MakeTimeAndPosition() const;

        virtual std::unique_ptr<CollectorTestSpecifications> MakeTestSpecifications() const = 0;
        virtual CollectorLocation MakeLocation() const = 0;
        virtual CollectorOrientation MakeOrientation() const = 0;
        virtual std::unique_ptr<Pipe> MakePipe() const = 0;
        virtual std::unique_ptr<ExternalConditions> MakeExternalConditions() const = 0;
        virtual tm MakeTime() const = 0;
        virtual ArrayDimensions MakeArrayDimensions() const = 0;
    };

    class DefaultFpcFactory : public FpcFactory {
    public:
        DefaultFpcFactory() {};

        virtual std::unique_ptr<FlatPlateArray> MakeFpcArray() const;
        virtual std::unique_ptr<FlatPlateCollector> MakeCollector() const;

        virtual std::unique_ptr<CollectorTestSpecifications> MakeTestSpecifications() const;
        virtual CollectorLocation MakeLocation() const;
        virtual CollectorOrientation MakeOrientation() const;
        virtual std::unique_ptr<Pipe> MakePipe() const;
        virtual std::unique_ptr<ExternalConditions> MakeExternalConditions() const;
        virtual tm MakeTime() const;
        virtual ArrayDimensions MakeArrayDimensions() const;
    };
}

#endif
