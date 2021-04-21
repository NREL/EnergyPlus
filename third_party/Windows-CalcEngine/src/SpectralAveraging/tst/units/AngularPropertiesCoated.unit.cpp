#include <memory>
#include <gtest/gtest.h>

#include "WCESpectralAveraging.hpp"
#include "WCECommon.hpp"


using namespace SpectralAveraging;
using namespace FenestrationCommon;

class TestAngularPropertiesCoated : public testing::Test {

protected:
	void SetUp() override {
	}

};

TEST_F( TestAngularPropertiesCoated, Test1 ) {
	SCOPED_TRACE( "Begin Test: Coated properties - various angles." );

	auto T0 = 0.722;
	auto R0 = 0.066;
	auto angle = 0.0;

	auto aAngularFactory = CAngularPropertiesFactory( T0, R0, 0, T0 );
	auto aProperties = aAngularFactory.getAngularProperties( SurfaceType::Coated );

	EXPECT_NEAR( 0.7236606, aProperties->transmittance( angle ), 1e-6 );
	EXPECT_NEAR( 0.0647858, aProperties->reflectance( angle ), 1e-6 );

	angle = 30;
	EXPECT_NEAR( 0.71370641981902272, aProperties->transmittance( angle ), 1e-6 );
	EXPECT_NEAR( 0.06922922004519097, aProperties->reflectance( angle ), 1e-6 );

	angle = 60;
	EXPECT_NEAR( 0.6500166, aProperties->transmittance( angle ), 1e-6 );
	EXPECT_NEAR( 0.13822155, aProperties->reflectance( angle ), 1e-6 );

	angle = 90;
	EXPECT_NEAR( 0, aProperties->transmittance( angle ), 1e-6 );
	EXPECT_NEAR( 1, aProperties->reflectance( angle ), 1e-6 );
}

TEST_F( TestAngularPropertiesCoated, Test2 ) {
	SCOPED_TRACE( "Begin Test: Coated properties - NFRC Sample ID=1042." );

	auto T0 = 0.4517085;
	auto R0 = 0.3592343;
	auto angle = 0.0;

	auto aAngularFactory = CAngularPropertiesFactory( T0, R0, 0, T0 );
	auto aProperties = aAngularFactory.getAngularProperties( SurfaceType::Coated );

	EXPECT_NEAR( 0.457016074875, aProperties->transmittance( angle ), 1e-6 );
	EXPECT_NEAR( 0.354909131525, aProperties->reflectance( angle ), 1e-6 );

	angle = 10;
	EXPECT_NEAR( 0.45468722065434630, aProperties->transmittance( angle ), 1e-6 );
	EXPECT_NEAR( 0.35394347494903849, aProperties->reflectance( angle ), 1e-6 );

	angle = 20;
	EXPECT_NEAR( 0.44888678437700730, aProperties->transmittance( angle ), 1e-6 );
	EXPECT_NEAR( 0.35282710650693727, aProperties->reflectance( angle ), 1e-6 );

	angle = 30;
	EXPECT_NEAR( 0.44183053629536256, aProperties->transmittance( angle ), 1e-6 );
	EXPECT_NEAR( 0.35502207112203277, aProperties->reflectance( angle ), 1e-6 );

	angle = 40;
	EXPECT_NEAR( 0.43348892234243364, aProperties->transmittance( angle ), 1e-6 );
	EXPECT_NEAR( 0.36223764779145762, aProperties->reflectance( angle ), 1e-6 );

	angle = 50;
	EXPECT_NEAR( 0.41826848334431255, aProperties->transmittance( angle ), 1e-6 );
	EXPECT_NEAR( 0.37451467264543725, aProperties->reflectance( angle ), 1e-6 );

	angle = 60;
	EXPECT_NEAR( 0.38374048664062504, aProperties->transmittance( angle ), 1e-6 );
	EXPECT_NEAR( 0.39802064877812487, aProperties->reflectance( angle ), 1e-6 );

	angle = 70;
	EXPECT_NEAR( 0.31265626242160510, aProperties->transmittance( angle ), 1e-6 );
	EXPECT_NEAR( 0.46009099882567134, aProperties->reflectance( angle ), 1e-6 );

	angle = 80;
	EXPECT_NEAR( 0.18796828616155317, aProperties->transmittance( angle ), 1e-6 );
	EXPECT_NEAR( 0.62493075431787948, aProperties->reflectance( angle ), 1e-6 );

}
