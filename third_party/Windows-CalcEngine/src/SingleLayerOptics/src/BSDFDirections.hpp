#ifndef BSDFDIRECTIONS_H
#define BSDFDIRECTIONS_H

#include <vector>
#include <memory>

namespace FenestrationCommon {

	class CSquareMatrix;

}

namespace SingleLayerOptics {

	class CBSDFPatch;
	class CBeamDirection;

	class CBSDFDefinition {
	public:
		CBSDFDefinition( const double t_Theta, const size_t t_NumOfPhis );
		double theta() const;
		size_t numOfPhis() const;

	private:
		double m_Theta;
		size_t m_NumOfPhis;
	};

	enum class BSDFHemisphere { Incoming, Outgoing };

	class CBSDFDirections {
	public:
		CBSDFDirections( std::vector< CBSDFDefinition >& t_Definitions, const BSDFHemisphere t_Side );
		size_t size() const;
		std::shared_ptr< const CBSDFPatch > operator[]( const size_t Index ) const;
		std::vector< std::shared_ptr< CBSDFPatch > >::iterator begin();
		std::vector< std::shared_ptr< CBSDFPatch > >::iterator end();

		std::shared_ptr< std::vector< double > > lambdaVector() const;
		std::shared_ptr< FenestrationCommon::CSquareMatrix > lambdaMatrix() const;

		// returns index of element that is closest to given Theta and Phi angles
		size_t getNearestBeamIndex( const double t_Theta, const double t_Phi ) const;

	private:
		std::vector< std::shared_ptr< CBSDFPatch > > m_Patches;
		std::shared_ptr< std::vector< double > > m_LambdaVector;
		std::shared_ptr< FenestrationCommon::CSquareMatrix > m_LambdaMatrix;

	};

	enum class BSDFBasis { Small, Quarter, Half, Full };

	class CBSDFHemisphere {
	public:
		// Construction for pre-defined basis
		explicit CBSDFHemisphere( const BSDFBasis t_Basis );
		// Construction for custom basis
		explicit CBSDFHemisphere( std::vector< CBSDFDefinition >& t_Definitions );
		std::shared_ptr< const CBSDFDirections > getDirections( const BSDFHemisphere t_Side ) const;

	private:
		std::shared_ptr< CBSDFDirections > m_IncomingDirections;
		std::shared_ptr< CBSDFDirections > m_OutgoingDirections;
	};

}

#endif
