#ifndef _QDT_ROMBERG_H_
#define _QDT_ROMBERG_H_

#include <cmath>
#include <vector>
#include "../common/method.h"

namespace qdt
{

class Romberg : public MethodMultiLevelWithBoundariesMiddle<Romberg>
{
	unsigned int max_levels;
public:
	/**
         * \brief Constructs the Romberg numerical method.
         *
         * \param _max_levels is the maximum level of subdivision of the method, which corresponds to the maximum order of the method.
	 * This parameter should be smaller than 16 in order to avoid extreme subdivision and numerical errors.  
         */
	Romberg(unsigned int _max_levels = 16) : max_levels(_max_levels) { }

	//We reserve memory in a vector for all the levels, but we include a number which indicates in which level we are.
	template<typename YType, typename real>
	class LevelData
	{
		friend class Romberg;
		unsigned int level;
		real hn; unsigned int twonminus1;
		std::vector<YType> Rm;

		LevelData(unsigned int max_levels, real a, real b) : level(0), hn(b-a), twonminus1(1),Rm(max_levels) { }
		void add_level()     { level++; hn*=0.5; if (level>1) twonminus1*=2; }
		YType return_value() { return Rm[level-1]; } 
	};

public:
	template<typename YType, typename real, typename Function>
	LevelData<YType,real> first_level_data(const Function& f, 
		real a, const YType& fa, real b, const YType& fb, const YType& fmiddle) const 
	{
		return LevelData<decltype(f(a)),real>(this->max_levels, a, b);
	}

	template<typename T, typename real>
	bool max_level(const LevelData<T,real>& data) const { return data.level >= this->max_levels; }

	unsigned int levels() const { return max_levels; }

	//This nested method just has two levels
	template<typename YType, typename real, typename Function>
	YType integrate_level(const Function& f, 
		real a, const YType& fa, real b, const YType& fb, const YType& fmiddle, LevelData<YType, real>& data) const 
	{
		if (data.level == 0)
			data.Rm[0] = 0.5*(b-a)*(fa + fb);
		else
		{
			YType prev = data.Rm[0];
			if (data.level == 1)
			    	data.Rm[0] = 0.5*data.Rm[0] + data.hn*fmiddle;
			else
			{
				YType sum(0.0);
				for (unsigned int k = 1; k<=data.twonminus1; k++) sum+=f(a + real(2*k - 1)*data.hn);
				data.Rm[0] = 0.5*data.Rm[0] + data.hn*sum;
			}
			for (unsigned int m = 1, fourm = 4; m<=data.level; m++, fourm*=4)
			{
				YType actual = data.Rm[m];
				data.Rm[m] = (real(1.0)/real(fourm - 1))*(real(fourm)*data.Rm[m-1] - prev);
				prev = actual;
			}  
		}
		
		data.add_level();	
		return data.return_value();
	}
};


Romberg romberg(unsigned int order)
{	return Romberg(order);	}

};

#endif
