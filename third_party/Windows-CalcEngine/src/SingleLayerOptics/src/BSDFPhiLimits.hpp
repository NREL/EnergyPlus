#ifndef BSDFPHILIMITS_H
#define BSDFPHILIMITS_H

#include <vector>
#include <memory>

namespace SingleLayerOptics {

	class CPhiLimits {
	public:
		explicit CPhiLimits( const size_t t_NumOfPhis );

		std::shared_ptr< std::vector< double > > getPhiLimits() const;

	private:
		void createLimits( const std::vector< double >& t_PhiAngles );
		std::shared_ptr< std::vector< double > > m_PhiLimits;
	};

}

#endif
