#ifndef _QDT_MONTE_CARLO_H_
#define _QDT_MONTE_CARLO_H_

#include <cmath>
#include <random>
#include "../common/method.h"

namespace qdt
{

template<typename RNG = std::default_random_engine>
class MonteCarlo : public Method<MonteCarlo<RNG> >
{
	unsigned int nsamples_;
	mutable RNG rng;
public:
	MonteCarlo(unsigned int nsamples = 1, 
		typename RNG::result_type seed = std::random_device()()): nsamples_(nsamples), rng(seed)  { }
	
	unsigned int nsamples() const { return nsamples_; }	

	template<typename real, typename Function>
	auto integrate_real(const Function& f, real a, real b) const -> decltype(f(a))
	{
		std::uniform_real_distribution<real> dist(a, b);
		decltype(f(a)) sol = f(dist(rng));
		for (unsigned int i = 1;i<nsamples();i++) sol+=f(dist(rng));
		return (b - a)*sol / (decltype(f(a))(nsamples()));
	}
};

MonteCarlo<> monte_carlo(unsigned int nsamples = 1)
{
	return MonteCarlo<>(nsamples);
}



};

#endif
