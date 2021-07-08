#ifndef _QDT_TRAPEZOID_H_
#define _QDT_TRAPEZOID_H_

#include "../common/method.h"

namespace qdt
{

class Trapezoid: public MethodWithBoundaries<Trapezoid>
{
public:
	Trapezoid()  { }

	template<typename real, typename YType, typename Function>
	YType integrate_with_boundaries(const Function& f, real a, const YType& fa, real b, const YType& fb) const
	{
		return (b-a)*0.5*(fa + fb);
	}
};

Trapezoid trapezoid()
{	return Trapezoid();	}

};

#endif
