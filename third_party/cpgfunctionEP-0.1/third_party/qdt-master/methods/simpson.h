#ifndef _QDT_SIMPSON_H_
#define _QDT_SIMPSON_H_

#include "../common/method.h"

namespace qdt
{

class Simpson: public MethodWithBoundariesMiddle<Simpson>
{
public:
	Simpson()  { }

	template<typename real, typename YType, typename Function>
	YType integrate_with_boundaries_and_middle(const Function& f, 
		real a, const YType& fa, real b, const YType& fb, const YType& fmiddle) const
	{
		return (b-a)*(fa + fb + 4.0*fmiddle)/6.0;
	}
};

Simpson simpson()
{	return Simpson();	}

};

#endif
