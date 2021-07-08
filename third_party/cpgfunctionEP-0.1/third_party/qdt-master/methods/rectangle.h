#ifndef _QDT_RECTANGLE_H_
#define _QDT_RECTANGLE_H_

#include "../common/method.h"

namespace qdt
{

class Rectangle : public Method<Rectangle>
{
public:
	Rectangle() { }

	template<typename real, typename Function>
	auto integrate_real(const Function& f, real a, real b) const -> decltype(f(a))
	{
		return (b-a)*f(0.5*(b+a));
	}
};

Rectangle rectangle()
{	return Rectangle();	}

};

#endif
