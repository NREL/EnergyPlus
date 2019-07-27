#ifndef TARCOGSINGLESYSTEM_H
#define TARCOGSINGLESYSTEM_H

#include <memory>
#include <map>
#include <vector>

namespace Tarcog {

	enum class Environment;
	class CIGU;
	class CBaseIGULayer;
	class CIGUSolidLayer;
	class CIGUGapLayer;
	class CEnvironment;
	class CNonLinearSolver;

	class CSingleSystem {
	public:
		CSingleSystem( std::shared_ptr< CIGU > const& t_IGU,
		               std::shared_ptr< CEnvironment > const& t_Indoor,
		               std::shared_ptr< CEnvironment > const& t_Outdoor );

		CSingleSystem( CSingleSystem const & t_SingleSystem );
		CSingleSystem & operator=( CSingleSystem const & t_SingleSystem );

		std::vector< std::shared_ptr< CIGUSolidLayer > > getSolidLayers() const;
		std::vector< std::shared_ptr< CIGUGapLayer > > getGapLayers() const;

		std::shared_ptr< std::vector< double > > getTemperatures() const;
		std::shared_ptr< std::vector< double > > getRadiosities() const;

		std::shared_ptr< std::vector< double > > getMaxDeflections() const;
		std::shared_ptr< std::vector< double > > getMeanDeflections() const;

		std::shared_ptr< CSingleSystem > clone() const;

		double getHeatFlow( Environment const t_Environment ) const;
		double getConvectiveHeatFlow( Environment const t_Environment ) const;
		double getRadiationHeatFlow( Environment const t_Environment ) const;
		double getHc( Environment const t_Environment ) const;
		double getAirTemperature( Environment const t_Environment ) const;

		// If interior layer have openings, this will return heat flow from airflow
		double getVentilationFlow( Environment const t_Environment ) const;
		double getUValue() const;
		size_t getNumberOfIterations() const;
		double solutionTolarance() const;
		bool isToleranceAchieved() const;

		// Set solution tolerance
		void setTolerance( double const t_Tolerance ) const;
		// Set intial guess for solution.
		void setInitialGuess( std::vector< double > const& t_Temperatures ) const;

		void setSolarRadiation( double const t_SolarRadiation );
		double getSolarRadiation() const;

		void solve() const;

	private:
		std::shared_ptr< CIGU > m_IGU;
		std::map< Environment, std::shared_ptr< CEnvironment > > m_Environment;
		std::shared_ptr< CNonLinearSolver > m_NonLinearSolver;
		void initializeStartValues();
	};

}

#endif
