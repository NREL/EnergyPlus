#ifndef TARNONLINEARSOLVER_H
#define TARNONLINEARSOLVER_H

#include <memory>
#include <vector>

namespace FenestrationCommon {

	class CLinearSolver;

}

namespace Tarcog {

	class CHeatFlowBalance;
	class CIGU;

	class CNonLinearSolver {
	public:
		explicit CNonLinearSolver( std::shared_ptr< CIGU > const& t_IGU );

		// sets tolerance for solution
		void setTolerance( double const t_Tolerance );

		// returns number of iterations for current solution.
		size_t getNumOfIterations() const;

		void solve();

		double solutionTolerance() const;
		bool isToleranceAchieved() const;

	private:
		double calculateTolerance( std::vector< double > const& t_Solution ) const;
		void estimateNewState( std::vector< double > const& t_Solution ) const;

		std::shared_ptr< CIGU > m_IGU;
		std::shared_ptr< FenestrationCommon::CLinearSolver > m_LinearSolver;
		std::shared_ptr< CHeatFlowBalance > m_QBalance;
		std::shared_ptr< std::vector< double > > m_IGUState;
		double m_Tolerance;
		size_t m_Iterations;
		double m_RelaxParam;
		double m_SolutionTolerance;
	};

}


#endif
