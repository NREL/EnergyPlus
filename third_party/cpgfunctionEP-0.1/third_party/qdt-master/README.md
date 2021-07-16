# qdt
A C++11 header-based library for quadrature integrators.

This library provides a set of quadrature methods for numerically integrating one-dimensional functions (that return real or vector values). This code is part of my research on rendering participating media, which has been published as the paper [Higher Order Ray Marching](http://giga.cps.unizar.es/~amunoz/projects/CGF2014_higherorder/). 

It supports the following quadrature methods:

Method                                                          | Code
----------------------------------------------------------------|-----------------------------------------------------
[Rectangle rule](http://en.wikipedia.org/wiki/Rectangle_method) | `qdt::rectangle()`
[Trapezoid rule](http://en.wikipedia.org/wiki/Trapezoidal_rule) | `qdt::trapezoid()`
[Simpson rule](http://en.wikipedia.org/wiki/Simpson%27s_rule)   | `qdt::simpson()`
[Monte Carlo](http://en.wikipedia.org/wiki/Monte_Carlo_integration) | `qdt::monte_carlo(/*Number of samples*/)`
[Romberg](http://en.wikipedia.org/wiki/Romberg%27s_method)                             | `qdt::romberg(/*Order*/)`
[Gauss-Kronrod](http://en.wikipedia.org/wiki/Gauss%E2%80%93Kronrod_quadrature_formula) (order 7-15) | `qdt::gauss_kronrod()`

Each of those quadrature rules can be applied in different ways:

Usage                                                           | Code
----------------------------------------------------------------|-----------------------------------------------------
As is | None
Subdividing the integration range into steps | `qdt::constant_step(/*Number of steps*/, /*Base method*/)`
Adaptatively with a specified tolerance parameter | `qdt::adaptive(/*Tolerance*/, /*Base method*/)`

Note that either the stepping or the adaptativeness of methods is reasonably smart. For instance, the stepping of the Trapezoid rule does not calculate twice the same points for two consecutive steps. For adaptive methods similar rules are followed: adaptive Simpson´s rule does not re-evaluate the function in previously evaluated points. Furthermore, nested methods (Gauss-Kronrod) take advantage of such (do not reevaluate points for calculating the error) and multi-order methods (Romberg) increase their order for getting more accurate (instead of subdividing the step).


## Compiling the example
There is an [example](https://github.com/adolfomunoz/qdt/blob/master/examples/qdt-pi.cc) that computes number pi as the arc tangent integral over the infinite range using different methods. It can be compiled with any C++11 compiler. We also provide a very simple `CMakeLists.txt` for CMake lovers. 

You can clone the repository and compile the example (git + gcc) as follows:
```
git clone https://github.com/adolfomunoz/qdt.git
g++ --std=c++11 qdt/examples/qdt-pi.cc -o qdt-pi
```

## Usage
You could first get inspired by the [example](https://github.com/adolfomunoz/qdt/blob/master/examples/qdt-pi.cc). Foryour code, you only need to include [qdt.h](https://github.com/adolfomunoz/qdt/blob/master/qdt.h). I pesonally have this in a `qdt` directory, so I just type `#include <qdt/qdt.h>` and that's it. There is no linkage, this is a header-only library.

All quadrature algorithms have a method `integrate` that integrates the provided function along the specified range. For instance: 

```
auto method = qdt::constant_step(10, qdt::trapezoid());
std::cout << method.integrate([] (float x) { return x*x; }, -1.0f, 1.0f)<<std::endl;
```
would numerically integrate the square function in the range [-1,1] using 10 steps of the Trapezoid rule.

The `integrate` method also works on an infinite range. For instance the following line:
```
std::cout << qdt::adaptive(1.e-6, qdt::gauss_kronrod()).
      integrate([] (double t) { return 1.0/(t*t + 1.0); }, qdt::MINUS_INF, qdt::INF) << std::endl;
```
calculates the integral of a function along the infinte range, with an adaptive nested Gauss-Kronrod method with 1.e-6 tolerance. The result should be close to pi. Internally, this perform a change of variable. It is recommended to avoid any method that evaluates the boundaries of the range (such as Trapezoid or Simpsons rule).

Quadrature rules can be combined in strange ways:
```
auto stratified_monte_carlo = qdt::constant_step(100, qdt::monte_carlo(100));
auto stratified_adaptive    = qdt::constant_step(10, qdt::adaptive(1.e-6, qdt::simpson()));
```

## Disclaimer

This code is released under the [GNU Public License V3 licence](http://www.gnu.org/licenses/gpl-3.0-standalone.html). In practice, if you use this code I'd like the following to happen:
* Let me know that you are using it.
* Let me know how can it be improved.
* If it makes sense, cite my [paper](http://giga.cps.unizar.es/~amunoz/projects/CGF2014_higherorder/).

