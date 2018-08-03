# Refactoring of Performance Tables

## Btwxt Overview

This document describes the planned design of the interpolation library, Task #1 in the statement of work. Design for tasks #2 and #3---which relate to refactoring  EnergyPlus `table` procedures---will be described in a separate document.

The proposal is to write a general-purpose N-dimensional interpolation library, Btwxt, for gridded data. After reviewing existing open-source libraries, we believe it is sensible to write a new library.

The library will accept:
*   a collection of N vectors representing the input variables,
*   attributes of each of the input vectors describing preferred interpolation and extrapolation methods, and
*   an array of (or collection of arrays of) values that map onto the grid defined by the input vectors.

Together, the input vectors and array(s) of output values will define a performance space. The library will allow querying of a defined performance space for a well-defined point within the grid.

*   A point fully inside the grid boundaries will receive an interpolated value.
    *   The library will support (a) linear and (b) cubic interpolation.
    *   The library structure will allow for alternative interpolation methods to be added later.
*   A point outside the grid boundaries will receive an extrapolated value.
    *   The library will support (a) linear and (b) constant extrapolation
    *   The library will allow each input dimension to define its extrapolation method and an extrapolation limit.
    *   Each input dimension will have the option of specifying outer boundaries beyond which the library will not interpolate.
    *   Each input dimension will have the option of disallowing any extrapolation.

## Glossary

*   *GridAxis* - The collection of values that define points along a single grid dimension (often thought of as an independent variable)
*   *GridSpace* - The collection of multiple GridAxis objects that fully define the performance space.
*   *ValueTables* - A collection of result/output values that all map to the same GridSpace.
*   *GriddedData* - A unification of a GridSpace with its associated ValueTables.
*   *GridPoint* - A coordinate set (one value along each axis) that defines a single point within a GridSpace.
*   *ValuePoint* - the set of result/output values at a single coordinate location. For example, the output values for a GridPoint.
*   *Floor* - along one GridAxis, the index of the grid-defined point less-than-or-equal-to a specified GridPoint. For example, for `GridAxis = {5, 10, 15, 20}`, `Floor(12, GridAxis) = 1`. (1 being the index of the value "10" in the array.)
*   *Weight* - the fraction defined by how far between the floor and the next point on a GridAxis a GridPoint falls. Referenced from the floor. For example, for `GridAxis = {5, 10, 15, 20}`, `Weight(12, GridAxis) = 0.4`.


## Approach

The GriddedData constructor is being designed to work smoothly with the PerformanceData FlatBuffer file format being defined for ASHRAE Section 205. It will also be easy to instantiate a GriddedData object from a well-defined set of input vectors and value arrays.

#### Summaries of important classes:
**GridAxis**

```c++
class GridAxis{
  GridAxis(std::vector<double> grid_vector);

  // attributes
  std::vector<double> grid;
  int interpolation_method;
  int extrapolation_method;
  std::pair<double, double> extrapolation_limits;
  // bool is_regular;     (to add later)

  // methods
  std::size_t get_length();
  void set_interp_method();
  void set_extrap_method();
  void set_extrap_limits();
}
```
The `GridAxis` constructor ensures that the `grid_vector` is sorted strictly ascending.

Each axis (input vector) is able to specify its own preferred:
*   interpolation method
*   extrapolation method
*   extrapolation limit, outside of which Btwxt will return a warning.

The idea behind a future `is_regular` attribute is that there are steps in the interpolation math that become much quicker if a grid axis is regularly spaced.

**GridSpace**
```c++
class GridSpace{
public:
  GridSpace(std::vector<GridAxis> grid_axes);

  std::vector<GridAxis> axes;
};
```
The `GridSpace` is a container for the set of N GridAxis objects that delineate the performance space.


**GriddedData**
```c++
class GriddedData{
  GriddedData(
    std::vector< std::vector<double> > grid,
    std::vector< std::vector<double> > values
  );

  // attributes
  GridSpace grid_axes;
  Eigen::ArrayXXd value_tables;
  std::size_t num_values;
  std::vector<std::size_t> dimension_lengths;

  // methods
  void check_inputs();
  std::vector<double> get_grid_vector(const std::size_t& grid_index);
  std::vector<double> get_values(std::vector<std::size_t> coords);
  Eigen::ArrayXd get_column(const std::vector<std::size_t>& coords);
}
```
`GriddedData` holds both a `GridSpace` object and an array of the provided gridded values. The constructor ensures the size of the value array matches with the dimensions of the input grid.

The `value_tables` array comprises a single long row for each output. That is, if this `GriddedData` object has N output variables, the `value_tables` array will be NxM where M is the number of points defined by the grid.

The `GriddedData.get_column()` method returns an Mx1 column array of the output values for a provided grid index (i.e., by index, not by value). `GriddedData.get_values()` returns the same output values in a std::vector rather than an Eigen Array.


**GridPoint**
```c++
class GridPoint{
  GridPoint(std::vector<double> &target_vector);

  // attributes
  std::vector<double> target;
}
```

A `GridPoint` describes the coordinates (by value on each GridAxis) of a performance point we are about to interpolate about.



**WhereInTheGridIsThisPoint**
```c++
class WhereInTheGridIsThisPoint{
public:
  WhereInTheGridIsThisPoint(GridPoint&, GriddedData&);

  // attributes
  std::vector<std::size_t> point_floor;
  std::vector<double> weights;
  std::vector<bool> is_inbounds;

  // methods
  std::vector<std::size_t> get_floor();
  std::vector<double> get_weights();
  std::vector<bool> get_is_inbounds();
  void find_floor(GridPoint&, GriddedData&);
  void calculate_weights(GridPoint&, GriddedData&);
};
```
This elegantly named class is the locator that describes how a specific `GridPoint` fits into the `GriddedData` performance space.
*   `point_floor` is the defined grid point less than or equal to `GridPoint` in each dimension.
*   `weights` define the where GridPoint falls between two GridAxis elements, for each dimension.
*   `is_inbounds` is used to decide interpolation or extrapolation for each GridAxis.


**RegularGridInterpolator**
```c++
class RegularGridInterpolator{
public:
  RegularGridInterpolator(GriddedData &the_blob);
  RegularGridInterpolator(
    std::vector< std::vector<double> > grid,
    std::vector< std::vector<double> > values
  );

  double calculate_value_at_target(std::vector<double> target, std::size_t table_index);
  double operator()(std::vector<double> target, std::size_t table_index)
  {
    return calculate_value_at_target(target, table_index);
  }

  double calculate_value_at_target(std::size_t table_index);
  double operator()(std::size_t table_index)
  {
    return calculate_value_at_target(table_index);
  }

  std::vector<double> calculate_all_values_at_target(std::vector<double> target);
  std::vector<double> operator()(std::vector<double> target)
  {
    return calculate_all_values_at_target(target);
  }

  std::vector<double> calculate_all_values_at_target();
  std::vector<double> operator()()
  {
    return calculate_all_values_at_target();
  }

  void set_new_grid_point(std::vector<double> target);
  std::vector<double> get_current_grid_point();
  void clear_current_grid_point();
  std::size_t get_ndims();

private:
  GriddedData the_blob;
  GridPoint current_grid_point;

  void check_target_dimensions(std::vector<double> target);
  std::vector<double> the_calculator();

```


Note that the "()" operator is overloaded, allowing a function-like shortcut to interpolation. `my_interpolator.calculate_all_values_at_target(target)` can be shortened to `my_interpolator(target)`.

The library will store a `current_grid_point`, meaning that `my_interpolator()` implies interpolating to that saved `GridPoint`. The floors and weights are saved as attributes of `current_grid_point`, so reusing a point will save those search and algebraic steps.

Accordingly, the `RegularGridInterpolator` API has methods to set, get, and clear `current_grid_point`. If the developer attempts to interpolate to `current_grid_point` before one has been specified, the library will return a warning.

the private `the_calculator` method is wrapped by the various public `calculate...` methods and houses the interpolation algorithm itself. The algorithm will deal with the possible interpolation and extrapolation methods on the fly, for each axis.


## Algorithmic Overview

The interpolation algorithm is designed around the N-dimensional grid cell containing the target point. Btwxt refers to that grid cell as the hypercube and bases the interpolation math around the hypercube's values.

**Linear Interpolation:**

Linear interpolation in 1 dimension: Given known values of f at x<sub>0</sub> and x<sub>1</sub>, and a point x between x<sub>0</sub> and x<sub>1</sub>, the value at x is estimated by f(x) = (1-&mu;) &times; f(x<sub>0</sub>) + &mu; &times; f(x<sub>1</sub>) where &mu; is the location of x expressed as a fraction of the distance between x<sub>0 </sub> and x<sub>1</sub>: &mu; = (x-x<sub>0</sub>)/(x<sub>1</sub>-x<sub>0</sub>).

Generalizing to N-dimensions: For each dimension d of the hypercube containing a target GridPoint, let &mu;<sub>d</sub> be the fraction of the distance along that hypercube edge where the GridPoint lies. The vertex on low side on that edge gets a weight of (1-&mu;<sub>d</sub>) and the high side gets a &mu;<sub>d</sub> multiplier.

So for a 2-D linear interpolation, for a hypercube of (x<sub>0</sub>,y<sub>0</sub>), (x<sub>0</sub>,y<sub>1</sub>), (x<sub>1</sub>,y<sub>0</sub>), (x<sub>1</sub>,y<sub>1</sub>), and a GridPoint of (x<sub>0</sub>+&mu;<sub>x</sub>, y<sub>0</sub>+&mu;<sub>y</sub>):

f(x<sub>0</sub>+&mu;<sub>x</sub>, y<sub>0</sub>+&mu;<sub>y</sub>) = f(x<sub>0</sub>, y<sub>0</sub>) (1-&mu;<sub>x</sub>) (1-&mu;<sub>y</sub>) <br>
&nbsp;&nbsp;&plus; f(x<sub>0</sub>, y<sub>1</sub>) (1-&mu;<sub>x</sub>) &mu;<sub>y</sub> <br>
&nbsp;&nbsp;&plus; f(x<sub>1</sub>, y<sub>0</sub>) &mu;<sub>x</sub> (1-&mu;<sub>y</sub>) <br>
&nbsp;&nbsp;&plus; f(x<sub>1</sub>, y<sub>1</sub>) &mu;<sub>x</sub> &mu;<sub>y</sub>

This extends to arbitrary dimensions. For N dimensions, a linear interpolation will have 2<sup>N</sup> terms, each with N multiplicative factors.


**Cubic Interpolation:**

The general formula for a 1-dimensional piecewise cubic spline (for x between known values x<sub>0</sub> and x<sub>1</sub>) is:

f(x) = (2&mu;3 - 3&mu;2 + 1) f(x<sub>0</sub>) + (-2&mu;3 + 3&mu;2) f(x<sub>1</sub>) <br>
&nbsp;&nbsp;&plus; (&mu;3 - 2&mu;2 + &mu;) f’(x<sub>0</sub>)
&plus; (&mu;3 - &mu;2) f’(x<sub>1</sub>)

We need to define what we will use as the derivative at x<sub>0</sub> and x<sub>1</sub>. Btwxt implements a Catmull-Rom cubic spline, which defines the slope at x as the slope between the point before and the point after x:

f’(x<sub>0</sub>) = ( f(x<sub>1</sub>) - f(x<sub>-1</sub>) ) / ( x<sub>1</sub> - x<sub>-1</sub>) <br>
f’(x<sub>1</sub>) = ( f(x<sub>2</sub>) - f(x<sub>0</sub>) ) / ( x<sub>2</sub> - x<sub>0</sub>)

When the hypercube is at the edge of the grid, Catmull-Rom simply extends the slope of the final segment.

As with the linear interpolation, the cubic can be rewritten as a sum of weights on each vertex. Because of the slope calculations, there are four points involved on each cubic axis. For one dimension, reorganized, the formula looks like this:

f(x) = f(x<sub>0</sub>) (2&mu;3 - 3&mu;2 + 1)  <br>
&nbsp;&nbsp;&plus; f(x<sub>1</sub>) (-2&mu;3 + 3&mu;2) <br>
&nbsp;&nbsp;&plus;  f(x<sub>1</sub>) (&mu;3 - 2&mu;2 + &mu;) / ( x<sub>1</sub> - x<sub>-1</sub>)  <br>
&nbsp;&nbsp;&minus; f(x<sub>-1</sub>) (&mu;3 - 2&mu;2 + &mu;) / ( x<sub>1</sub> - x<sub>-1</sub>) <br>
&nbsp;&nbsp;&plus; f(x<sub>2</sub>) (&mu;3 - &mu;2) / ( x<sub>2</sub> - x<sub>0</sub>) <br>
&nbsp;&nbsp;&minus; f(x<sub>0</sub>) (&mu;3 - &mu;2) / ( x<sub>2</sub> - x<sub>0</sub>)


The cubic formula extends to multiple dimensions. More points are involved (4<sup>N</sup> for N dimensions, rather than 2<sup>N</sup> for linear), but the formula remains a sum of products of independent weights on each point in the hypercube. The algorithm can also mix and match interpolation methods from one axis to another. The cubic interpolation, being a more complex calculation, takes substantially longer than linear, especially as the number of cubic axes increases.

## Performance Benchmarking

The Btwxt library will include a CPU/memory performance benchmark mechanism.
Six-variable function, ten grid steps on each axis.
Select 1000 GridPoints in the defined region. Interpolate and time.
