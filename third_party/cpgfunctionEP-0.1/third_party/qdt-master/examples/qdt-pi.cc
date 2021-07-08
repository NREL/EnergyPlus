#include <iostream>
#include <iomanip>
#include "../qdt.h"

int main(int argc, char** argv) {
	std::cout<<"Calculating PI as an integral over an infinite range"<<std::endl;

	auto f = [] (double t) { return 1.0/(t*t + 1.0); };
	std::cout<<"    Steps\tMonteCarlo\tRectangle\tTrapezoid\tSimpson"<<std::endl;
	for (unsigned int i = 100;i<=2000;i+=100)
	{
		auto rectangle = qdt::constant_step(i, qdt::rectangle());
		auto trapezoid = qdt::constant_step(i, qdt::trapezoid());
		auto simpsons  = qdt::constant_step(i, qdt::simpson());
		auto mc	       = qdt::monte_carlo(i);
		std::cout<<"      "<<std::setw(5)<<i<<"\t"<<std::setprecision(10)
			<<mc.integrate(f, qdt::MINUS_INF, qdt::INF)<<"\t" 
			<<rectangle.integrate(f, qdt::MINUS_INF, qdt::INF)<<"\t" 
			<<trapezoid.integrate(f, qdt::MINUS_INF, qdt::INF)<<"\t" 
			<<simpsons.integrate(f, qdt::MINUS_INF, qdt::INF)<<std::endl; 
	}
	std::cout<<"    Gauss-Kronrod"<<std::endl;
	{
		auto method = qdt::adaptive(qdt::gauss_kronrod());
		std::cout<<"      "<<std::setprecision(10)
			<<method.integrate(f, qdt::MINUS_INF, qdt::INF)<<std::endl; 
	}
}
