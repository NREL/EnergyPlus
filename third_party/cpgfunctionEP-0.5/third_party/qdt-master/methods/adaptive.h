#ifndef _QDT_ADAPTIVE_H_
#define _QDT_ADAPTIVE_H_

#include "../common/method.h"
#include <cmath>
#include <type_traits>

namespace qdt 
{

class ErrorEstimator
{
public:
	template<typename T>
	T estimate_error(const T& e1, const T& e2);

private:
	template<typename T, typename U>
	typename T::value_type estimate_error_vector(const T& e1, const U& e2) const
	{
//		typename std::remove_const<typename std::remove_reference<decltype(*(e1.begin()))>::type>::type sol 
//										= decltype(*(e1.begin()))(0.0);
		auto sol = (typename T::value_type)(0.0);
		auto i1 = e1.begin();
		auto i2 = e2.begin();
		for(; (i1 != e1.end()) && (i2!=e2.end());i1++, i2++)
		{
			auto err = estimate_error(*i1, *i2);
			if (err>sol) sol = err;
		} 
		return sol;
	}

	template<typename T>
	T estimate_error_scalar(const T& e1, const T& e2) const 
	{
		if (std::max(std::abs(e1),std::abs(e2)) < 1.e-6) return std::abs(e1 - e2);
		else return std::abs(e1-e2)/std::max(std::abs(e1),std::abs(e2));
	}

public:
	template<typename T, typename U>
	auto estimate_error(const T& e1, const U& e2) const -> decltype(estimate_error_vector(e1,e2))
	{ return estimate_error_vector(e1,e2);	};

	int estimate_error(int e1, int e2) const { return estimate_error_scalar(e1,e2); }
	float estimate_error(float e1, float e2) const { return estimate_error_scalar(e1,e2); }
	double estimate_error(double e1, double e2) const { return estimate_error_scalar(e1,e2); }
};

template<typename BaseMethod, typename Estimator = ErrorEstimator, 
		bool boundaries = BaseMethod::uses_boundaries, bool middle = BaseMethod::uses_middle, 
		bool nested = BaseMethod::is_nested, bool multi_level = BaseMethod::is_multi_level>
class Adaptive : public Method<Adaptive<BaseMethod,Estimator,boundaries,middle,nested> >
{
	BaseMethod _base_method;
	Estimator estimator;
	float _tolerance;
	float min_step;
public:
	Adaptive(float tol = 1.e-5, float _min_step = 1.e-5): _tolerance(tol), min_step(_min_step) { }
	Adaptive(const BaseMethod& base_method, float tol = 1.e-5, float _min_step = 1.e-5): 
		_base_method(base_method), _tolerance(tol), min_step(_min_step) { }
	Adaptive(const BaseMethod& base_method, const Estimator& _estimator, float tol = 1.e-5, float _min_step = 1.e-5): 
		_base_method(base_method), estimator(_estimator), _tolerance(tol), min_step(_min_step) { }

	const BaseMethod& base_method() const { return _base_method; }
	float tolerance()               const { return _tolerance;   }

private:
	template<typename YType, typename real, typename Function>
	YType integrate_real_tol(const Function& f, const real& a, const real& b, const YType& whole, const real& tol) const 
	{
		if (std::abs(a-b)<min_step) return whole;
		real half   = 0.5*(a+b);
		YType left  = base_method().integrate(f,a,half);
		YType right = base_method().integrate(f,half,b);
		real error  = estimator.estimate_error(left+right, whole);
		
		if (error<tol) return left+right;
		else return integrate_real_tol(f,a,half,left,real(0.5*tol)) + integrate_real_tol(f,half,b,right,real(0.5*tol));
	}

public:
	template<typename real, typename Function>
	auto integrate_real(const Function& f, real a, real b) const -> decltype(f(a))
	{
		return integrate_real_tol(f,a,b,base_method().integrate_real(f,a,b),real(tolerance()));
	}
};

template<typename BaseMethod, typename Estimator>
class Adaptive<BaseMethod,Estimator,true,false,false,false> : 
		public MethodWithBoundaries<Adaptive<BaseMethod,Estimator,true,false,false,false> >
{
	BaseMethod _base_method;
	Estimator estimator;
	float _tolerance;
	float min_step;
public:
	Adaptive(float tol = 1.e-5, float _min_step = 1.e-5): _tolerance(tol), min_step(_min_step) { }
	Adaptive(const BaseMethod& base_method, float tol = 1.e-5, float _min_step = 1.e-5): 
		_base_method(base_method), _tolerance(tol), min_step(_min_step) { }
	Adaptive(const BaseMethod& base_method, const Estimator& _estimator, float tol = 1.e-5, float _min_step = 1.e-5): 
		_base_method(base_method), estimator(_estimator), _tolerance(tol), min_step(_min_step) { }

	const BaseMethod& base_method() const { return _base_method; }
	float tolerance()               const { return _tolerance;   }

private:
	template<typename real, typename YType, typename Function>
	YType integrate_with_boundaries_tol(const Function& f, real a, const YType& fa, real b, const YType& fb, 
			const YType& whole, const real& tol) const
	{
		if (std::abs(a-b)<min_step) return whole;
		real half   = 0.5*(a+b);
		YType f_half = f(half);
		YType left  = base_method().integrate_with_boundaries(f,a,fa,half,f_half);
		YType right = base_method().integrate_with_boundaries(f,half,f_half,b,fb);
		real error  = estimator.estimate_error(left+right, whole);

		if (error<tol) return left+right;
		else return integrate_with_boundaries_tol(f,a,fa,half,f_half,left,real(0.5*tol)) + 
				integrate_with_boundaries_tol(f,half,f_half,b,fb,right,real(0.5*tol));

	}

public:
	template<typename real, typename YType, typename Function>
	YType integrate_with_boundaries(const Function& f, real a, const YType& fa, real b, const YType& fb) const
	{
		return integrate_with_boundaries_tol(f,a,fa,b,fb,base_method().integrate_with_boundaries(f,a,fa,b,fb),real(tolerance()));
	}
};

template<typename BaseMethod, typename Estimator>
class Adaptive<BaseMethod,Estimator,true,true,false,false> : 
	public MethodWithBoundariesMiddle<Adaptive<BaseMethod,Estimator,true,true,false,false> >
{
	BaseMethod _base_method;
	Estimator estimator;
	float _tolerance;
	float min_step;
public:
	Adaptive(float tol = 1.e-5, float _min_step = 1.e-5): _tolerance(tol), min_step(_min_step) { }
	Adaptive(const BaseMethod& base_method, float tol = 1.e-5, float _min_step = 1.e-5): 
		_base_method(base_method), _tolerance(tol), min_step(_min_step) { }
	Adaptive(const BaseMethod& base_method, const Estimator& _estimator, float tol = 1.e-5, float _min_step = 1.e-5): 
		_base_method(base_method), estimator(_estimator), _tolerance(tol), min_step(_min_step) { }

	const BaseMethod& base_method() const { return _base_method; }
	float tolerance()               const { return _tolerance;   }

private:
	template<typename real, typename YType, typename Function>
	YType integrate_with_boundaries_and_middle_tol(const Function& f, 
		real a, const YType& fa, real b, const YType& fb, const YType& f_half,
			const YType& whole, const real& tol) const
	{
		if (std::abs(a-b)<min_step) return whole;
		real half = 0.5*(a+b);
		real left_half   = 0.5*(a+half); real right_half = 0.5*(half+b);
		YType f_left_half = f(left_half); YType f_right_half = f(right_half);
		YType left  = base_method().integrate_with_boundaries_and_middle(f,a,fa,half,f_half,f_left_half);
		YType right = base_method().integrate_with_boundaries_and_middle(f,half,f_half,b,fb,f_right_half);
		real error  = estimator.estimate_error(left+right, whole);

		if (error<tol) return left+right;
		else return integrate_with_boundaries_and_middle_tol(f,a,fa,half,f_half,f_left_half,left,real(0.5*tol)) + 
				integrate_with_boundaries_and_middle_tol(f,half,f_half,b,fb,f_right_half,right,real(0.5*tol));

	}

public:
	template<typename real, typename YType, typename Function>
	YType integrate_with_boundaries_and_middle(const Function& f, 
		real a, const YType& fa, real b, const YType& fb, const YType& f_half) const
	{
		return integrate_with_boundaries_and_middle_tol(f,a,fa,b,fb,f_half,
			base_method().integrate_with_boundaries_and_middle(f,a,fa,b,fb,f_half),real(tolerance()));
	}
};


template<typename BaseMethod, typename Estimator, bool b1, bool b2>
class Adaptive<BaseMethod,Estimator,b1,b2,true,false> : public Method<Adaptive<BaseMethod,Estimator,b1,b2,true,false> >
{
	BaseMethod _base_method;
	Estimator estimator;
	float _tolerance;
	float min_step;
public:
	Adaptive(float tol = 1.e-5, float _min_step = 1.e-5): _tolerance(tol), min_step(_min_step) { }
	Adaptive(const BaseMethod& base_method, float tol = 1.e-5, float _min_step = 1.e-5): 
		_base_method(base_method), _tolerance(tol), min_step(_min_step) { }
	Adaptive(const BaseMethod& base_method, const Estimator& _estimator, float tol = 1.e-5, float _min_step = 1.e-5): 
		_base_method(base_method), estimator(_estimator), _tolerance(tol), min_step(_min_step) { }

	const BaseMethod& base_method() const { return _base_method; }
	float tolerance()               const { return _tolerance;   }

private:
	template<typename real, typename Function>
	auto integrate_real_tol(const Function& f, const real& a, const real& b, const real& tol) const -> decltype(f(a))
	{
		decltype(f(a)) prev;
		decltype(f(a)) sol = base_method().integrate_nested(f,a,b,prev);
		real error  = estimator.estimate_error(sol, prev);

		if ((std::abs(a-b)<min_step)||(error<tol)) return sol;
		else
		{
			real half   = 0.5*(a+b);
			return integrate_real_tol(f,a,half,real(0.5*tol)) + integrate_real_tol(f,half,b,real(0.5*tol));
		}		
	}

public:
	template<typename real, typename Function>
	auto integrate_real(const Function& f, real a, real b) const -> decltype(f(a))
	{
		return integrate_real_tol(f,a,b,real(tolerance()));
	}
};

template<typename BaseMethod, typename Estimator>
class Adaptive<BaseMethod,Estimator,false,false,false,true> : public Method<Adaptive<BaseMethod,Estimator,false,false,false,true> >
{
	BaseMethod _base_method;
	Estimator estimator;
	float _tolerance;
	float min_step;
public:
	Adaptive(float tol = 1.e-5, float _min_step = 1.e-5): _tolerance(tol), min_step(_min_step) { }
	Adaptive(const BaseMethod& base_method, float tol = 1.e-5, float _min_step = 1.e-5): 
		_base_method(base_method), _tolerance(tol), min_step(_min_step) { }
	Adaptive(const BaseMethod& base_method, const Estimator& _estimator, float tol = 1.e-5, float _min_step = 1.e-5): 
		_base_method(base_method), estimator(_estimator), _tolerance(tol), min_step(_min_step) { }

	const BaseMethod& base_method() const { return _base_method; }
	float tolerance()               const { return _tolerance;   }

private:
	template<typename real, typename Function>
	auto integrate_real_tol(const Function& f, const real& a, const real& b, const real& tol) const -> decltype(f(a))
	{
		auto data = base_method().first_level_data(f,a,b);
		decltype(f(a)) sol = base_method().integrate_level(f,a,b,data);
		real error = tol + real(1.0);
		while ( (!base_method().max_level(data)) && (error >= tol) )
		{
			decltype(f(a)) prev = sol;
			sol = base_method().integrate_level(f,a,b,data);
			error  = estimator.estimate_error(sol, prev);
		}

		if ((std::abs(a-b)<min_step)||(error<tol)) return sol;
		else
		{
			real half   = 0.5*(a+b);
			return integrate_real_tol(f,a,half,real(0.5*tol)) + integrate_real_tol(f,half,b,real(0.5*tol));
		}		
	}

public:
	template<typename real, typename Function>
	auto integrate_real(const Function& f, real a, real b) const -> decltype(f(a))
	{
		return integrate_real_tol(f,a,b,real(tolerance()));
	}
};

template<typename BaseMethod, typename Estimator>
class Adaptive<BaseMethod,Estimator,true,false,false,true> : 
		public MethodWithBoundaries<Adaptive<BaseMethod,Estimator,true,false,false,true> >
{
	BaseMethod _base_method;
	Estimator estimator;
	float _tolerance;
	float min_step;
public:
	Adaptive(float tol = 1.e-5, float _min_step = 1.e-5): _tolerance(tol), min_step(_min_step) { }
	Adaptive(const BaseMethod& base_method, float tol = 1.e-5, float _min_step = 1.e-5): 
		_base_method(base_method), _tolerance(tol), min_step(_min_step) { }
	Adaptive(const BaseMethod& base_method, const Estimator& _estimator, float tol = 1.e-5, float _min_step = 1.e-5): 
		_base_method(base_method), estimator(_estimator), _tolerance(tol), min_step(_min_step) { }

	const BaseMethod& base_method() const { return _base_method; }
	float tolerance()               const { return _tolerance;   }

private:
	template<typename real, typename YType, typename Function>
	YType integrate_with_boundaries_tol(const Function& f, 
		const real& a, const YType& fa, const real& b, const YType& fb, const real& tol) const
	{
		auto data = base_method().first_level_data(f,a,fa,b,fb);
		decltype(f(a)) sol = base_method().integrate_level(f,a,fa,b,fb,data);
		real error = tol + real(1.0);
		while ( (!base_method().max_level(data)) && (error >= tol) )
		{
			decltype(f(a)) prev = sol;
			sol = base_method().integrate_level(f,a,fa,b,fb,data);
			error  = estimator.estimate_error(sol, prev);
		}

		if ((std::abs(a-b)<min_step)||(error<tol)) return sol;
		else
		{
			real half   = 0.5*(a+b);  YType f_half = f(half);
			return integrate_with_boundaries_tol(f,a,fa,half,f_half,real(0.5*tol)) 
				+ integrate_with_boundaries_tol(f,half,f_half,b,fb,real(0.5*tol));
		}		
	}

public:
	template<typename real, typename YType, typename Function>
	YType integrate_with_boundaries(const Function& f, real a, const YType& fa, real b, const YType& fb) const
	{
		return integrate_with_boundaries_tol(f,a,fa,b,fb,real(tolerance()));
	}
};

template<typename BaseMethod, typename Estimator>
class Adaptive<BaseMethod,Estimator,true,true,false,true> : 
		public MethodWithBoundariesMiddle<Adaptive<BaseMethod,Estimator,true,true,false,true> >
{
	BaseMethod _base_method;
	Estimator estimator;
	float _tolerance;
	float min_step;
public:
	Adaptive(float tol = 1.e-5, float _min_step = 1.e-5): _tolerance(tol), min_step(_min_step) { }
	Adaptive(const BaseMethod& base_method, float tol = 1.e-5, float _min_step = 1.e-5): 
		_base_method(base_method), _tolerance(tol), min_step(_min_step) { }
	Adaptive(const BaseMethod& base_method, const Estimator& _estimator, float tol = 1.e-5, float _min_step = 1.e-5): 
		_base_method(base_method), estimator(_estimator), _tolerance(tol), min_step(_min_step) { }

	const BaseMethod& base_method() const { return _base_method; }
	float tolerance()               const { return _tolerance;   }

private:
	template<typename real, typename YType, typename Function>
	YType integrate_with_boundaries_and_middle_tol(const Function& f, 
		const real& a, const YType& fa, const real& b, const YType& fb, const YType& fmiddle, const real& tol) const
	{
		auto data = base_method().first_level_data(f,a,fa,b,fb,fmiddle);
		decltype(f(a)) sol = base_method().integrate_level(f,a,fa,b,fb,fmiddle,data);
		real error = tol + real(1.0);
		while ( (!base_method().max_level(data)) && (error >= tol) )
		{
			decltype(f(a)) prev = sol;
			sol = base_method().integrate_level(f,a,fa,b,fb,fmiddle,data);
			error  = estimator.estimate_error(sol, prev);
		}

		if ((std::abs(a-b)<min_step)||(error<tol)) return sol;
		else
		{
			real half = 0.5*(a+b);
			real left_half   = 0.5*(a+half); real right_half = 0.5*(half+b);
			YType f_left_half = f(left_half); YType f_right_half = f(right_half);
			return integrate_with_boundaries_and_middle_tol(f,a,fa,half,fmiddle,f_left_half,real(0.5*tol)) 
				+ integrate_with_boundaries_and_middle_tol(f,half,fmiddle,b,fb,f_right_half,real(0.5*tol));
		}		
	}

public:
	template<typename real, typename YType, typename Function>
	YType integrate_with_boundaries_and_middle(const Function& f, 
		real a, const YType& fa, real b, const YType& fb, const YType& fmiddle) const
	{
		return integrate_with_boundaries_and_middle_tol(f,a,fa,b,fb,fmiddle,real(tolerance()));
	}
};

template<typename BM>
Adaptive<BM> adaptive(const BM& bm)
{
	return Adaptive<BM>(bm, 1.e-5, 1.e-10);
}	

template<typename BM>
Adaptive<BM> adaptive(float tol, const BM& bm)
{
	return Adaptive<BM>(bm, tol, 1.e-10);
}	






};

#endif
