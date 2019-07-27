#include <memory>
#include <gtest/gtest.h>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"


using namespace SingleLayerOptics;
using namespace FenestrationCommon;

// Simple scattering surface tests
class TestScatteringSurface : public testing::Test {

private:
	std::shared_ptr< CScatteringSurface > m_Surface;

protected:
	virtual void SetUp() {

		const double T_dir_dir = 0.08;
		const double R_dir_dir = 0.05;

		const double T_dir_dif = 0.46;
		const double R_dir_dif = 0.23;

		const double T_dif_dif = 0.46;
		const double R_dif_dif = 0.52;

		m_Surface = std::make_shared< CScatteringSurface >( T_dir_dir, R_dir_dir,
		                                               T_dir_dif, R_dir_dif,
		                                               T_dif_dif, R_dif_dif );

	}

public:
	std::shared_ptr< CScatteringSurface > getSurface() {
		return m_Surface;
	};

};

TEST_F( TestScatteringSurface, ScatteringSurface1 ) {
	SCOPED_TRACE( "Begin Test: Simple scattering surface." );

	std::shared_ptr< CScatteringSurface > surf = getSurface();

	double A_dir = surf->getAbsorptance( ScatteringSimple::Direct );

	EXPECT_NEAR( 0.18, A_dir, 1e-6 );

	double A_dif = surf->getAbsorptance( ScatteringSimple::Diffuse );

	EXPECT_NEAR( 0.02, A_dif, 1e-6 );

}
