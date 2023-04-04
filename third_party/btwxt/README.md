[![Build and Test](https://github.com/bigladder/btwxt/actions/workflows/build-and-test.yml/badge.svg)](https://github.com/bigladder/btwxt/actions/workflows/build-and-test.yml)
[![codecov](https://codecov.io/gh/bigladder/btwxt/branch/master/graph/badge.svg)](https://codecov.io/gh/bigladder/btwxt)

# Btwxt

## General-purpose, N-dimensional interpolation library...

Btwxt is a free and open source c++ library to perform numerical interpolation on a gridded data set. The general architecture is that it creates a RegularGridInterpolator object from a) a collection of grid axes and b) a collection of values that fit that axis system. That RegularGridInterpolator object can then be queried repeatedly for values that fall inside its grid.

Btwxt supports linear and cubic spline (Catmull-Rom) interpolation methods, and those methods can be specified independently for each axis. The API also allows specification of preferred extrapolation methods (constant or linear)--again independently for each axis--and extrapolation limits.

The input grid is required to be regular (rectilinear) but the axes are not required to have uniform spacing. Each axis need only be strictly increasing.

The values are imported as a long vector: a flattened array. It is your responsibility to ensure that you load the grid vectors in the same order as they are defined for the values. Btwxt ensures that the number of values aligns with the grid space, but it does not attempt to verify that the grid axis order is consistent with the values.

Btwxt accepts:
*   a collection of N vectors representing the input variables,
*   attributes of each of the input vectors describing preferred interpolation and extrapolation methods, and
*   an array of (or collection of arrays of) values that map onto the grid defined by the input vectors.

## How to Use

### How to install/include
Need to complete this section

### API
An individual axis can be instantiated with
```c++
std::vector<double> one_axis{6, 10, 15};
GridAxis(one_axis);
GridAxis(one_axis, interpolation_method = Method::CUBIC);
// interpolation method defaults to LINEAR
GridAxis(one_axis, extrapolation_method = Method::LINEAR);
// extrapolation method defaults to CONSTANT
std::pair<double, double> extrap_bounds{0, 20};
GridAxis(one_axis, Method::LINEAR, Method::CUBIC, extrap_bounds);
// extrapolation bounds default to {-DBL_MAX, DBL_MAX}
```

A collection of axes can be tied together into a GriddedData object
```c++
std::vector<GridAxis> my_grid{first_axis, second_axis, third_axis};
std::vector< std::vector<double> > values = {...};
GriddedData(my_grid, values);
```

Or you can feed a vector of vectors straight into GriddedData and let it construct the GridAxis objects:
```c++
std::vector<std::vector<double> > vector_grid = {{6, 10, 15},
                                                 {4, 6, 9}};
std::vector< std::vector<double> > values = {...};
GriddedData gridded_data(vector_grid, values);
gridded_data.set_axis_interp_method(0, Method::CUBIC);
gridded_data.set_axis_extrap_method(1, Method::LINEAR);
gridded_data.set_axis_extrap_limits(1, {2, 10});
```

RegularGridInterpolator holds a GriddedData object and adds capabilities for interpolation. If specified straight from vectors, it will build the GriddedData and GridAxis objects appropriately.
```c++
RegularGridInterpolator my_interpolator(gridded_data);
// or
std::vector<std::vector<double> > vector_grid = {{6, 10, 15},
                                                 {4, 6, 9}};
std::vector< std::vector<double> > values = {...};
my_interpolator(vector_grid, values);
```

Once you have an RGI object, you can set a point to interpolate to:
```c++
std::vector<double> target{12.5, 5.1};
my_interpolator.set_new_grid_point(target);
std::vector<double> result = my_interpolator.calculate_all_values_at_target();
my_interpolator.clear_current_grid_point();
// or set the point within the calculate:
std::vector<double> new_target{11.7, 6.1};
result = my_interpolator.calculate_all_values_at_target(new_target);
```

We have overloaded the () operator on the RGI to perform the interpolation, enabling the following:
```c++
std::vector<double> target{12.5, 5.1};
my_interpolator.set_new_grid_point(target);
std::vector<double> result = my_interpolator();
std::vector<double> new_target{11.7, 6.1};
std::vector<double> result = my_interpolator(new_target);
```


## Dependencies
Btwxt uses
*  [Googletest](https://github.com/google/googletest), both gtest and gmock for unittests.
