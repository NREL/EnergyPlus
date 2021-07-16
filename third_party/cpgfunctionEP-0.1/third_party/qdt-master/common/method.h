#ifndef _QDT_METHOD_H_
#define _QDT_METHOD_H_


namespace qdt {

enum Infinity { INF };
enum MinusInfinity { MINUS_INF };


//No dynamic polymorphism ergo long compilation times
template<typename M>
class Method
{
public:
	static const bool uses_middle     = false;
	static const bool uses_boundaries = false;
	static const bool is_nested       = false;
	static const bool is_multi_level  = false;

	

	template<typename Function, typename real>
	auto integrate(const Function& f, real a, real b) const -> decltype(f(a))
	{
		return static_cast<const M&>(*this).integrate_real(f,a,b);
	}

	template<typename Function, typename real>
	auto integrate_from_minus_inf(const Function& f, real b) const -> decltype(f(b))
	{
		return static_cast<const M&>(*this).integrate_real
			([&](real t){ return (real(1.0)/(t*t))*f(b - (real(1.0) - t)/t); },
				real(0.0), real(1.0));
	}

	template<typename Function, typename real>
	auto integrate_to_inf(const Function& f, real a) const -> decltype(f(a))
	{
		return static_cast<const M&>(*this).integrate_real
			([&](real t){return (real(1.0)/((real(1.0) - t)*(real(1.0) - t)))*f(a + (t/(real(1.0) - t)));},
				real(0.0), real(1.0));
	}

	template<typename Function, typename real = float>
	auto integrate_from_minus_inf_to_inf(const Function& f) const -> decltype(f(real(0.0)))
	{
		return static_cast<const M&>(*this).integrate_real
			([&](real t){real t2=t*t; return ((real(1.0)+t2)/((real(1.0) - t2)*(real(1.0) - t2)))*f(t/(real(1.0) - t2));},
				real(-1.0), real(1.0));
	}

	template<typename Function, typename real>
	auto integrate(const Function& f, const MinusInfinity& a, real b) const -> decltype(f(b))
	{
		return static_cast<const M&>(*this).integrate_from_minus_inf(f,b);
	}

	template<typename Function, typename real>
	auto integrate(const Function& f, real a, const Infinity& b) const -> decltype(f(a))
	{
		return static_cast<const M&>(*this).integrate_to_inf(f,a);
	}

	template<typename Function, typename real = float>
	auto integrate(const Function& f, const MinusInfinity& a, const Infinity& b) const -> decltype(f(real(0.0)))
	{
		return static_cast<const M&>(*this).integrate_from_minus_inf_to_inf(f);
	}

	template<typename Function, typename real, typename VChange, typename InvVChange, typename DVChange>
	auto integrate_change_of_variable(const Function& f, real a, real b, 
		const VChange& vc, const InvVChange& vc_inv, const DVChange& dvc) const -> decltype(f(vc(a)))
	{
		return integrate(
			[&] (real t) { return f(vc(t))*dvc(t); },
			vc_inv(a),vc_inv(b));
	}
};

template<typename M>
class MethodWithBoundaries : public Method<MethodWithBoundaries<M> >
{
public:
	static const bool uses_boundaries = true;

	template<typename real, typename Function>
	auto integrate_real(const Function& f, real a, real b) const -> decltype(f(a))
	{
		return static_cast<const M&>(*this).integrate_with_boundaries(f,a,f(a),b,f(b));
	}

	template<typename Function, typename real>
	auto integrate_from_minus_inf(const Function& f, real b) const -> decltype(f(b))
	{
		return static_cast<const M&>(*this).integrate_with_boundaries
			([&](real t){ return (real(1.0)/(t*t))*f(b - (real(1.0) - t)/t); },
				real(0.0), f(b - real(1.e5))*real(1.e10), 
				real(1.0), f(b));
	}

	template<typename Function, typename real>
	auto integrate_to_inf(const Function& f, real a) const -> decltype(f(a))
	{
		return static_cast<const M&>(*this).integrate_with_boundaries
			([&](real t){return (real(1.0)/((real(1.0) - t)*(real(1.0) - t)))*f(a + (t/(real(1.0) - t)));},
				real(0.0), f(a), 
				real(1.0), f(a + real(1.e5))*real(1.e10));
	}

	template<typename Function, typename real = float>
	auto integrate_from_minus_inf_to_inf(const Function& f) const -> decltype(f(real(0.0)))
	{
		return static_cast<const M&>(*this).integrate_with_boundaries
			([&](real t){ real t2=t*t; return ((real(1.0)+t2)/((real(1.0) - t2)*(real(1.0) - t2)))*f(t/(real(1.0) - t2));},
				real(-1.0), real(0.5*1.e10)*f(-1.e5), 
				real(1.0),  real(0.5*1.e10)*f(1.e5));
	}
};

template<typename M>
class MethodWithBoundariesMiddle : public MethodWithBoundaries<MethodWithBoundariesMiddle<M> >
{
public:
	static const bool uses_middle = true;

	template<typename real, typename YType, typename Function>
	YType integrate_with_boundaries(const Function& f, real a, const YType& fa, real b, const YType& fb) const
	{
		return static_cast<const M&>(*this).integrate_with_boundaries_and_middle(f,a,fa,b,fb,f(0.5*(a+b)));
	}
};

template<typename M>
class MethodNested : public Method<MethodNested<M> >
{
public:
	static const bool is_nested = true;

	template<typename real, typename Function>
	auto integrate_real(const Function& f, real a, real b) const -> decltype(f(a))
	{
		decltype(f(a)) that;
		return static_cast<const M&>(*this).integrate_nested(f,a,b,that);
	}
};

template<typename M>
class MethodMultiLevel : public Method<MethodMultiLevel<M> >
{
public:
	static const bool is_multi_level = true;

	template<typename real, typename Function>
	auto integrate_real(const Function& f, real a, real b) const -> decltype(f(a))
	{
		auto data = static_cast<const M&>(*this).first_level_data(f,a,b);
		decltype(f(a)) sol = static_cast<const M&>(*this).integrate_level(f,a,b,data);
		while(!static_cast<const M&>(*this).max_level(data))
		{
			sol = static_cast<const M&>(*this).integrate_level(f,a,b,data);
		} 
		return sol;
	}
};

template<typename M>
class MethodMultiLevelWithBoundaries : public MethodWithBoundaries<MethodMultiLevelWithBoundaries<M> >
{
public:
	static const bool is_multi_level = true;

	template<typename real, typename YType, typename Function>
	YType integrate_with_boundaries(const Function& f, real a, const YType& fa, real b, const YType& fb) const
	{
		auto data = static_cast<const M&>(*this).first_level_data(f,a,fa,b,fb);
		decltype(f(a)) sol = static_cast<const M&>(*this).integrate_level(f,a,fa,b,fb,data);
		while(!static_cast<const M&>(*this).max_level(data))
		{
			sol = static_cast<const M&>(*this).integrate_level(f,a,fa,b,fb,data);
		} 
		return sol;
	}
};

template<typename M>
class MethodMultiLevelWithBoundariesMiddle : public MethodWithBoundariesMiddle<MethodMultiLevelWithBoundariesMiddle<M> >
{
public:
	static const bool is_multi_level = true;

	template<typename real, typename YType, typename Function>
	YType integrate_with_boundaries_and_middle(const Function& f, 
		real a, const YType& fa, real b, const YType& fb, const YType& fmiddle) const
	{
		auto data = static_cast<const M&>(*this).first_level_data(f,a,fa,b,fb,fmiddle);
		decltype(f(a)) sol = static_cast<const M&>(*this).integrate_level(f,a,fa,b,fb,fmiddle,data);
		while(!static_cast<const M&>(*this).max_level(data))
		{
			sol = static_cast<const M&>(*this).integrate_level(f,a,fa,b,fb,fmiddle,data);
		} 
		return sol;
	}
};



};

#endif

