#ifndef _QDT_CONSTANT_STEP_H_
#define _QDT_CONSTANT_STEP_H_

#include "../common/method.h"

namespace qdt
{

template<typename BaseMethod, bool boundaries = BaseMethod::uses_boundaries>
class ConstantStep : public Method<ConstantStep<BaseMethod, boundaries> >
{
	float st;
	unsigned int nsteps;
	BaseMethod m;
public:
	ConstantStep(const BaseMethod& _m = BaseMethod()): st(1.0f), nsteps(1), m(_m) { }
	ConstantStep(float s, const BaseMethod& _m) :   st(s),nsteps(0), m(_m) { }
	ConstantStep(int ns, const BaseMethod& _m) : st(1.0f),nsteps(ns), m(_m) { }

	template<typename... Params>
	ConstantStep(float s, Params... constructor_parameters) :   st(s),nsteps(0), m(constructor_parameters...) { }
	template<typename... Params>
	ConstantStep(int ns, Params... constructor_parameters) : st(1.0f),nsteps(ns), m(constructor_parameters...) { }

	const BaseMethod& base_method() const { return m; }
	int steps()                     const { return nsteps>0?nsteps:int(1.0f/st); }

	template<typename real>	
	real step(const real& total) const { return (nsteps>0?(total/real(nsteps)):real(st)); }

	template<typename real>
	static bool stop(real x, real b, real h) { return h>0?x<(b-0.5*h):x>(b+0.5*h); }

	template<typename real, typename Function>
	auto integrate_real(const Function& f, real a, real b) const -> decltype(f(a))
	{
		real h = step(b-a);
		real x_prev = a; 
		real x = a+h;
		decltype(f(a)) sol(0.0);

		for (;stop(x,b,h);x+=h, x_prev+=h) sol+=m.integrate(f,x_prev,x);
		sol+=m.integrate(f,x_prev,b);

		return sol;
	}
};


template<typename BaseMethod>
class ConstantStep<BaseMethod,true> : public MethodWithBoundaries<ConstantStep<BaseMethod, true> >
{
	float st;
	unsigned int nsteps;
	BaseMethod m;
public:
	ConstantStep(const BaseMethod& _m = BaseMethod()): st(-1.0), nsteps(1), m(_m) { }
	ConstantStep(float s, const BaseMethod& _m = BaseMethod()) :   st(s),nsteps(0), m(_m) { }
	ConstantStep(int ns, const BaseMethod& _m = BaseMethod()) : st(-1.0),nsteps(ns), m(_m) { }

	template<typename... Params>
	ConstantStep(float s, Params... constructor_parameters) :   st(s),nsteps(0), m(constructor_parameters...) { }
	template<typename... Params>
	ConstantStep(int ns, Params... constructor_parameters) : st(1.0f),nsteps(ns), m(constructor_parameters...) { }

	const BaseMethod& base_method() const { return m; }
	int steps()                     const { return nsteps>0?nsteps:int(1.0f/st); }

	template<typename real>	
	real step(const real& total) const { return (nsteps>0?(total/real(nsteps)):real(st)); }

	template<typename real>
	static bool stop(real x, real b, real h) { return h>0?x<(b-0.5*h):x>(b+0.5*h); }

	template<typename real, typename YType, typename Function>
	YType integrate_with_boundaries(const Function& f, real a, const YType& fa, real b, const YType& fb) const
	{
		real h = step(b-a);
		real x_prev = a; 
		YType fx_prev = fa;
		real x = a+h;
		YType sol(0.0);

		for (;stop(x,b,h);x+=h, x_prev+=h) 
		{	
			YType fx = f(x);
			sol+=m.integrate_with_boundaries(f,x_prev,fx_prev,x,fx);
			fx_prev=fx;
		}
		sol+=m.integrate_with_boundaries(f,x_prev,fx_prev,b,fb);
		return sol;
	}
};

template<typename BM>
ConstantStep<BM> constant_step(int nsteps, const BM& bm)
{	return ConstantStep<BM>(nsteps,bm);	}


};

#endif
