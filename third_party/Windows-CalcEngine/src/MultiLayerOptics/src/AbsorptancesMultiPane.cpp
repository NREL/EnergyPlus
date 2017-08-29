#include "AbsorptancesMultiPane.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace FenestrationCommon;

namespace MultiLayerOptics {

	CAbsorptancesMultiPane::CAbsorptancesMultiPane( const std::shared_ptr< const CSeries >& t_T,
	                                                const std::shared_ptr< const CSeries >& t_Rf,
	                                                const std::shared_ptr< const CSeries >& t_Rb ) : m_StateCalculated( false ) {
		m_T.push_back( t_T );
		m_Rf.push_back( t_Rf );
		m_Rb.push_back( t_Rb );
	}

	void CAbsorptancesMultiPane::addLayer( const std::shared_ptr< const CSeries >& t_T,
	                                       const std::shared_ptr< const CSeries >& t_Rf,
	                                       const std::shared_ptr< const CSeries >& t_Rb ) {
		m_T.push_back( t_T );
		m_Rf.push_back( t_Rf );
		m_Rb.push_back( t_Rb );
		m_StateCalculated = false;
	}

	std::shared_ptr< CSeries > CAbsorptancesMultiPane::Abs( size_t const Index ) {
		calculateState();
		std::shared_ptr< CSeries > aAbs = nullptr;
		if ( Index < m_Abs.size() ) {
			aAbs = m_Abs[ Index ];
		}
		return aAbs;
	}

	size_t CAbsorptancesMultiPane::numOfLayers() {
		calculateState();
		return m_Abs.size();
	}

	void CAbsorptancesMultiPane::calculateState() {
		if ( !m_StateCalculated ) {
			size_t size = m_T.size();

			// Calculate r and t coefficients
			std::shared_ptr< CSeries > r = make_shared< CSeries >();
			std::shared_ptr< CSeries > t = make_shared< CSeries >();
			vector< double > wv = m_T[ size - 1 ]->getXArray();
			r->setConstantValues( wv, 0 );
			t->setConstantValues( wv, 0 );
			m_rCoeffs.clear();
			m_tCoeffs.clear();

			// layers loop
			for ( int i = int( size ) - 1; i >= 0; --i ) {
				t = tCoeffs( *m_T[ i ], *m_Rb[ i ], *r );
				r = rCoeffs( *m_T[ i ], *m_Rf[ i ], *m_Rb[ i ], *r );

				m_rCoeffs.insert( m_rCoeffs.begin(), r );
				m_tCoeffs.insert( m_tCoeffs.begin(), t );
			}

			// Calculate normalized radiances
			size = m_rCoeffs.size();
			vector< std::shared_ptr< CSeries > > Iplus;
			vector< std::shared_ptr< CSeries > > Iminus;

			std::shared_ptr< CSeries > Im = make_shared< CSeries >();
			std::shared_ptr< CSeries > Ip = nullptr;
			Im->setConstantValues( wv, 1 );
			Iminus.push_back( Im );

			for ( size_t i = 0; i < size; ++i ) {
				Ip = m_rCoeffs[ i ]->mMult( *Im );
				Im = m_tCoeffs[ i ]->mMult( *Im );
				Iplus.push_back( Ip );
				Iminus.push_back( Im );
			}
			Ip = make_shared< CSeries >();
			Ip->setConstantValues( wv, 0 );
			Iplus.push_back( Ip );

			// Calculate absorptances
			m_Abs.clear();
			size = Iminus.size();
			for ( size_t i = 0; i < size - 1; ++i ) {
				std::shared_ptr< CSeries > Iincoming = Iminus[ i ]->mSub( *Iplus[ i ] );
				std::shared_ptr< CSeries > Ioutgoing = Iminus[ i + 1 ]->mSub( *Iplus[ i + 1 ] );
				std::shared_ptr< CSeries > layerAbs = Iincoming->mSub( *Ioutgoing );
				m_Abs.push_back( layerAbs );
			}
		}
	}

	std::shared_ptr< CSeries > CAbsorptancesMultiPane::rCoeffs(
		const CSeries& t_T,
		const CSeries& t_Rf,
		const CSeries& t_Rb,
		const CSeries& t_RCoeffs ) {

		std::shared_ptr< CSeries > rCoeffs = make_shared< CSeries >();
		size_t size = t_T.size();

		for ( size_t i = 0; i < size; ++i ) {
			double wl = t_T[ i ].x();
			double rValue = t_Rf[ i ].value() + t_T[ i ].value() * t_T[ i ].value() * t_RCoeffs[ i ].value() /
				( 1 - t_Rb[ i ].value() * t_RCoeffs[ i ].value() );
			rCoeffs->addProperty( wl, rValue );
		}

		return rCoeffs;
	}

	std::shared_ptr< CSeries > CAbsorptancesMultiPane::tCoeffs(
		const CSeries& t_T,
		const CSeries& t_Rb,
		const CSeries& t_RCoeffs ) {

		std::shared_ptr< CSeries > tCoeffs = make_shared< CSeries >();
		size_t size = t_T.size();

		for ( size_t i = 0; i < size; ++i ) {
			double wl = t_T[ i ].x();
			double tValue = t_T[ i ].value() / ( 1 - t_Rb[ i ].value() * t_RCoeffs[ i ].value() );
			tCoeffs->addProperty( wl, tValue );
		}

		return tCoeffs;
	}

}
