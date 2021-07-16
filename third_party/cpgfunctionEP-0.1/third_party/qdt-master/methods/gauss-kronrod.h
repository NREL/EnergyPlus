#ifndef _QDT_GAUSS_KRONROD_H_
#define _QDT_GAUSS_KRONROD_H_

#include <cmath>
#include <vector>
#include "../common/method.h"

namespace qdt
{

class GaussKronrod : public MethodNested<GaussKronrod>
{
private:
	template<typename YType, typename Function>
	YType integrate_gauss_kronrod(const Function& f, YType& gauss) const
	{
		YType kronrod(0.0); gauss = YType(0.0);
		auto fi = f(0.0); 
			gauss += 0.417959183673469*fi; kronrod += 0.209482141084728*fi;
		fi = f(0.405845151377397) + f(-0.405845151377397);
			gauss += 0.381830050505119*fi; kronrod += 0.190350578064785*fi;
		fi = f(0.741531185599394) + f(-0.741531185599394); 
			gauss += 0.279705391489277*fi; kronrod += 0.140653259715525*fi;
		fi = f(0.949107912342759) + f(-0.949107912342759); 
			gauss += 0.129484966168870*fi; kronrod += 0.063092092629979*fi;

		kronrod += 0.204432940075298*( f(0.207784955007898) + f(-0.207784955007898) )
				+ 0.169004726639267*( f(0.586087235467691) + f(-0.586087235467691) )
				+ 0.104790010322250*( f(0.864864423359769) + f(-0.864864423359769) )
				+ 0.022935322010529*( f(0.991455371120813) + f(-0.991455371120813) );

		return kronrod;
	}

public:
	template<typename YType, typename real, typename Function>
	YType integrate_nested(const Function& f, real a, real b, YType& prev) const 
	{
		return integrate_gauss_kronrod(
			[&](real t) {return real(0.5)*(b-a)*f( (t + real(1.0))*real(0.5)*(b-a) + a); }, prev); 
	}

};

class GaussKronrodML : public MethodMultiLevel<GaussKronrodML>
{
public:
	GaussKronrodML() { }

private:
	
	template<typename YType, typename Function>
	YType integrate_gauss_kronrod(const Function& f, std::vector<YType>& data) const
	{
		//If we are in the gauss pass
		if (data.size()==1) 
		{
			data.resize(4);
			data[0] = f(0.0);
			data[1] = f(0.405845151377397) + f(-0.405845151377397);
			data[2] = f(0.741531185599394) + f(-0.741531185599394);
			data[3] = f(0.949107912342759) + f(-0.949107912342759);
			return 0.417959183673469*data[0]
			     + 0.381830050505119*data[1]
			     + 0.279705391489277*data[2]
			     + 0.129484966168870*data[3];

		} //We have the date from the gauss pass, compute Kronrod
		else if (data.size()==4) 
		{
			YType kronrod = 0.204432940075298*( f(0.207784955007898) + f(-0.207784955007898) )
				+ 0.169004726639267*( f(0.586087235467691) + f(-0.586087235467691) )
				+ 0.104790010322250*( f(0.864864423359769) + f(-0.864864423359769) )
				+ 0.022935322010529*( f(0.991455371120813) + f(-0.991455371120813) )
				+ 0.209482141084728*data[0]
			        + 0.190350578064785*data[1]
			        + 0.140653259715525*data[2]
			        + 0.063092092629979*data[3];
			data.clear();
			return kronrod;
		} else return YType(0.0);
	}

public:
	template<typename real, typename Function>
	auto first_level_data(const Function& f, real a, real b) const -> std::vector<decltype(f(a))>
	{
		//We mark the start (gauss pass) with a vector of size 1 (could be anything else, but this makes sense and is easier to do
		return std::vector<decltype(f(a))>(1);
	}

	template<typename T>
	bool max_level(const std::vector<T>& data) const { return data.empty(); }


	//This nested method just has two levels
	template<typename YType, typename real, typename Function>
	YType integrate_level(const Function& f, real a, real b, std::vector<YType>& data) const 
	{
		return integrate_gauss_kronrod(
			[&](real t) {return real(0.5)*(b-a)*f( (t + real(1.0))*real(0.5)*(b-a) + a); }, data); 
	}
};

GaussKronrod gauss_kronrod()
{	return GaussKronrod();	}

};

#endif
