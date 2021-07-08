#ifndef _QDT_TEST_H_
#define _QDT_TEST_H_

#include <iostream>
#include <sstream>
#include <cmath>
#include <list>
#include <chrono>
#include <qdt/qdt.h>

namespace qdt {

template<typename T>
std::ostream& operator<<(std::ostream& os, const MonteCarlo<T>& method)
{   os<<"MonteCarlo-"<<std::setfill('0')<<std::setw(7)<<method.nsamples(); return os; }

std::ostream& operator<<(std::ostream& os, const Trapezoid& method)
{   os<<"Trapezoid"; return os; }

std::ostream& operator<<(std::ostream& os, const Simpson& method)
{   os<<"Simpson"; return os; }

std::ostream& operator<<(std::ostream& os, const Rectangle& method)
{   os<<"Rectangle"; return os; }

std::ostream& operator<<(std::ostream& os, const GaussKronrod& method)
{   os<<"GaussKronrod"; return os; }

std::ostream& operator<<(std::ostream& os, const Romberg& method)
{   os<<"Romberg"<<method.levels(); return os; }

template<typename T>
std::ostream& operator<<(std::ostream& os, const Adaptive<T>& method)
{   os<<"Adapt-"<<method.base_method()<<"-"<<std::setfill('0')<<std::setw(5)<<method.tolerance(); return os; }

template<typename T>
std::ostream& operator<<(std::ostream& os, const ConstantStep<T>& method)
{   os<<"Steps-"<<method.base_method()<<"-"<<std::setfill('0')<<std::setw(5)<<method.steps(); return os; }

template<typename M>
std::string traits_string(const M& method)
{
    std::stringstream ss;
    ss<<(M::uses_boundaries?"[B]":"   ");
    ss<<(M::uses_middle?"[M]":"   ");
    ss<<(M::is_nested?"[N]":"   ");
    ss<<(M::is_multi_level?"[L]":"   ");
    return ss.str();
}

template<typename T>
class TestResult
{
	float time;
	unsigned int nevals;
	T sol, approx;
public:
	   TestResult(float _time, unsigned int _nevals, const T& _sol, const T& _approx) :
		time(_time), nevals(_nevals), sol(_sol), approx(_approx) { }

	float absolute_error()     const { return fabs(sol - approx); }
	float relative_error()     const { return fabs(sol - approx)/std::max(fabs(sol),fabs(approx)); }
	float seconds()            const { return time;   } 
	unsigned int evaluations() const { return nevals; }
	T solution()               const { return sol;    }
        T approximation()          const { return approx; }
};

template<typename T>
std::ostream& operator<<(std::ostream& os, const TestResult<T>& tr)
{
   os<<std::fixed<<std::setw(9)<<std::setprecision(3)<<1.e6*tr.seconds()<<"us\t"<<std::setw(6)<<tr.evaluations()<<"ev\t";
   os<<std::fixed<<std::setw(9)<<std::setprecision(3)<<1.e9*tr.seconds()/float(tr.evaluations())<<"ns\t";
   os<<"Error = "<<std::scientific<<std::setprecision(2)<<tr.absolute_error();
   os<<" ("<<std::fixed<<std::setw(6)<<std::setprecision(2)<<100.0*tr.relative_error()<<"%)";
   return os;
}

template<typename F>
class EvaluationCounter
{
	F f;
	mutable long int n;
public:
	EvaluationCounter(const F& _f) : f(_f), n(0) { }
	float operator()(float t) const { n++; return f(t); }
	long int count() const { return n; }
};

std::ostream& operator<<(std::ostream& os,const MinusInfinity& t) { os<<"-inf"; return os; }
std::ostream& operator<<(std::ostream& os,const Infinity& t)      { os<<"inf";  return os; }

template<typename Function, typename R, typename Method, typename A, typename B>
TestResult<R> test(const Function& f, const R& sol, const Method& m, const A& a, const B& b)
{
	EvaluationCounter<Function> fc(f);
	unsigned int n;
	R approx = m.integrate(fc,a,b);

        std::chrono::time_point<std::chrono::system_clock> start = std::chrono::system_clock::now();
	m.integrate(f,a,b); n=1;
	std::chrono::duration<double> elapsed_seconds = std::chrono::system_clock::now() - start;
	while (elapsed_seconds.count()<=0.01)
	{	 
		m.integrate(f,a,b); n++;
		elapsed_seconds = std::chrono::system_clock::now() - start;
	}
	return TestResult<R>(elapsed_seconds.count()/float(n),fc.count(),sol,approx);
}

template<typename F, typename real>
class EvaluationSamples
{
	F f;
	mutable std::list<real> _samples;
public:
	EvaluationSamples(const F& _f) : f(_f) { }
	float operator()(real t) const { _samples.push_back(t); return f(t); }
	std::list<real> samples() const { return _samples; }
};

template<typename Function, typename Method, typename A, typename B>
std::list<A> sampled_values(const Function& f, const Method& m, const A& a, const B& b)
{
	EvaluationSamples<Function, A> fs(f);
	m.integrate(fs,a,b);
	std::list<A> samples = fs.samples();
	return samples;
}


}

#endif

