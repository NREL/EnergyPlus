/* Copyright (c) 2012-2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#ifndef GEOMETRY_H_
#define GEOMETRY_H_

#include <boost/geometry.hpp>
#include <boost/geometry/geometries/geometries.hpp>
#include <boost/geometry/multi/geometries/multi_point.hpp>
#include <boost/geometry/multi/geometries/multi_polygon.hpp>

#include "Functions.hpp"
#include "libkiva_export.h"

namespace Kiva {

typedef boost::geometry::model::point<double, 2, boost::geometry::cs::cartesian> Point;
typedef boost::geometry::model::polygon<Point, true, false> Polygon;
typedef boost::geometry::model::ring<Point, true, false> Ring;
typedef boost::geometry::model::multi_polygon<Polygon> MultiPolygon;
typedef boost::geometry::model::multi_point<Point> MultiPoint;
typedef boost::geometry::model::linestring<Point> Line;
typedef boost::geometry::model::box<Point> Box;
typedef boost::geometry::model::point<double, 3, boost::geometry::cs::cartesian> Point3;
typedef boost::geometry::model::polygon<Point3, true, false> Polygon3;

namespace geom {
enum Direction { X_NEG, X_POS, Y_NEG, Y_POS };

enum Turn { LEFT, RIGHT };
} // namespace geom

bool isRectilinear(Polygon poly);
Polygon offset(Polygon poly, double dist);
geom::Direction getDirectionIn(Polygon poly, std::size_t vertex);
geom::Direction getDirectionOut(Polygon poly, std::size_t vertex);
geom::Turn getTurn(Polygon poly, std::size_t vertex);
MultiPolygon mirrorX(MultiPolygon poly, double x);
MultiPolygon mirrorY(MultiPolygon poly, double y);
Polygon symmetricUnit(Polygon poly);
bool isXSymmetric(Polygon poly);
bool isYSymmetric(Polygon poly);
double getXmin(Polygon poly, std::size_t vertex);
double getYmin(Polygon poly, std::size_t vertex);
double getXmax(Polygon poly, std::size_t vertex);
double getYmax(Polygon poly, std::size_t vertex);

bool LIBKIVA_EXPORT comparePointsX(Point first, Point second);
bool LIBKIVA_EXPORT comparePointsY(Point first, Point second);
bool LIBKIVA_EXPORT pointOnPoly(Point point, Polygon poly);
bool LIBKIVA_EXPORT isConvex(Polygon poly);

double LIBKIVA_EXPORT getDistance(Point a, Point b);

double getAngle(Point a, Point b, Point c);

} // namespace Kiva

#endif /* GEOMETRY_H_ */
