#ifndef GRID_VARIABLES_BASE_H_
#define GRID_VARIABLES_BASE_H_

#include <memory>
#include <vector>
#include <iostream>
#include <sstream>

#include <performance_map_base.h>
#include <error_handling_tk205.h>

// ------------------------------------------------------------------------------------------------
/// @class GridVariablesBase grid_variables_base.h

class GridVariablesBase {

public:
    GridVariablesBase() = default;
    virtual ~GridVariablesBase() = default;
    GridVariablesBase(const GridVariablesBase& other) = default;
    GridVariablesBase& operator=(const GridVariablesBase& other) = default;

    virtual void populate_performance_map(PerformanceMapBase* performance_map) = 0;

    inline void add_grid_axis(PerformanceMapBase* performance_map, std::vector<double>& axis)
    {
       performance_map->add_grid_axis(axis);
       std::ostringstream oss;
       oss << "Adding grid axis with size " << axis.size();
       tk205::show_message(tk205::MsgSeverity::INFO_205, oss.str());
    }
    inline void add_grid_axis(PerformanceMapBase* performance_map, std::vector<int>& axis)
    {
       performance_map->add_grid_axis(axis);
       std::ostringstream oss;
       oss << "Adding (int) grid axis with size " << axis.size();
       tk205::show_message(tk205::MsgSeverity::INFO_205, oss.str());
    }
};

#endif