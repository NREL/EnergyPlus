#include <cassert>

#include "EquivalentScatteringLayer.hpp"
#include "EquivalentLayerSingleComponent.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace SingleLayerOptics;
using namespace FenestrationCommon;

namespace MultiLayerOptics {

	CEquivalentScatteringLayer::CEquivalentScatteringLayer( const double Tf_dir_dir, const double Rf_dir_dir,
	                                                        const double Tb_dir_dir, const double Rb_dir_dir,
	                                                        const double Tf_dir_dif, const double Rf_dir_dif,
	                                                        const double Tb_dir_dif, const double Rb_dir_dif,
	                                                        const double Tf_dif_dif, const double Rf_dif_dif,
	                                                        const double Tb_dif_dif, const double Rb_dif_dif ) :
		m_Layer( std::make_shared< CScatteringLayer >( 
			std::make_shared< CScatteringSurface >( Tf_dir_dir, Rf_dir_dir, Tf_dir_dif, Rf_dir_dif, Tf_dif_dif, Rf_dif_dif ), 
			std::make_shared< CScatteringSurface >( Tb_dir_dir, Rb_dir_dir, Tb_dir_dif, Rb_dir_dif, Tb_dif_dif, Rb_dif_dif ) ) ),
		m_DiffuseLayer( std::make_shared< CEquivalentLayerSingleComponent >( Tf_dif_dif, Rf_dif_dif, Tb_dif_dif, Rb_dif_dif ) ),
		m_BeamLayer( std::make_shared< CEquivalentLayerSingleComponent >( Tf_dir_dir, Rf_dir_dir, Tb_dir_dir, Rb_dir_dir ) )
	{

		
	}

	CEquivalentScatteringLayer::CEquivalentScatteringLayer( CScatteringLayer& t_Layer,
	                                                        const double t_Theta, const double t_Phi ) {
		m_Layer = std::make_shared< CScatteringLayer >( t_Layer );

		double Tf = t_Layer.getPropertySimple( PropertySimple::T, Side::Front, Scattering::DirectDirect, t_Theta, t_Phi );
		double Rf = t_Layer.getPropertySimple( PropertySimple::R, Side::Front, Scattering::DirectDirect, t_Theta, t_Phi );
		double Tb = t_Layer.getPropertySimple( PropertySimple::T, Side::Back, Scattering::DirectDirect, t_Theta, t_Phi );
		double Rb = t_Layer.getPropertySimple( PropertySimple::R, Side::Back, Scattering::DirectDirect, t_Theta, t_Phi );

		m_BeamLayer = std::make_shared< CEquivalentLayerSingleComponent >( Tf, Rf, Tb, Rb );

		Tf = t_Layer.getPropertySimple( PropertySimple::T, Side::Front, Scattering::DiffuseDiffuse, t_Theta, t_Phi );
		Rf = t_Layer.getPropertySimple( PropertySimple::R, Side::Front, Scattering::DiffuseDiffuse, t_Theta, t_Phi );
		Tb = t_Layer.getPropertySimple( PropertySimple::T, Side::Back, Scattering::DiffuseDiffuse, t_Theta, t_Phi );
		Rb = t_Layer.getPropertySimple( PropertySimple::R, Side::Back, Scattering::DiffuseDiffuse, t_Theta, t_Phi );

		m_DiffuseLayer = std::make_shared< CEquivalentLayerSingleComponent >( Tf, Rf, Tb, Rb );
	}

	void CEquivalentScatteringLayer::addLayer( const double Tf_dir_dir, const double Rf_dir_dir,
	                                           const double Tb_dir_dir, const double Rb_dir_dir,
	                                           const double Tf_dir_dif, const double Rf_dir_dif,
	                                           const double Tb_dir_dif, const double Rb_dir_dif,
	                                           const double Tf_dif_dif, const double Rf_dif_dif,
	                                           const double Tb_dif_dif, const double Rb_dif_dif,
	                                           const Side t_Side ) {

		std::shared_ptr< CScatteringSurface > aFrontSurface =
			std::make_shared< CScatteringSurface >( Tf_dir_dir, Rf_dir_dir,
			                                   Tf_dir_dif, Rf_dir_dif,
			                                   Tf_dif_dif, Rf_dif_dif );

		std::shared_ptr< CScatteringSurface > aBackSurface =
			std::make_shared< CScatteringSurface >( Tb_dir_dir, Rb_dir_dir,
			                                   Tb_dir_dif, Rb_dir_dif,
			                                   Tb_dif_dif, Rb_dif_dif );

		CScatteringLayer aLayer = CScatteringLayer( aFrontSurface, aBackSurface );
		addLayer( aLayer, t_Side );
	}

	void CEquivalentScatteringLayer::addLayer( CScatteringLayer& t_Layer, const Side t_Side,
	                                           const double t_Theta, const double t_Phi ) {
		addLayerComponents( t_Layer, t_Side, t_Theta, t_Phi );
		switch ( t_Side ) {
		case Side::Front:
			calcEquivalentProperties( t_Layer, *m_Layer );
			break;
		case Side::Back:
			calcEquivalentProperties( *m_Layer, t_Layer );
			break;
		default:
			assert("Impossible side selection.");
			break;
		}
	}

	double CEquivalentScatteringLayer::getPropertySimple( const PropertySimple t_Property, const Side t_Side,
	                                                      const Scattering t_Scattering, const double t_Theta, const double t_Phi ) const {
		return m_Layer->getPropertySimple( t_Property, t_Side, t_Scattering, t_Theta, t_Phi );
	}

	std::shared_ptr< CScatteringLayer > CEquivalentScatteringLayer::getLayer() const {
		return m_Layer;
	}

	void CEquivalentScatteringLayer::calcEquivalentProperties(
		CScatteringLayer& t_First, CScatteringLayer& t_Second ) {
		// Direct to diffuse componet calculation
		const CScatteringSurface f1 = *t_First.getSurface( Side::Front );
		const CScatteringSurface b1 = *t_First.getSurface( Side::Back );
		const CScatteringSurface f2 = *t_Second.getSurface( Side::Front );
		const CScatteringSurface b2 = *t_Second.getSurface( Side::Back );
		SimpleResults frontSide = *calcDirectDiffuseTransAndRefl( f1, b1, f2 );
		SimpleResults backSide = *calcDirectDiffuseTransAndRefl( b2, f2, b1 );

		double Tf_dir_dif = frontSide.T;
		double Rf_dir_dif = frontSide.R;
		double Tb_dir_dif = backSide.T;
		double Rb_dir_dif = backSide.R;

		// Direct component
		double Tf_dir_dir = m_BeamLayer->getProperty( Property::T, Side::Front );
		double Rf_dir_dir = m_BeamLayer->getProperty( Property::R, Side::Front );
		double Tb_dir_dir = m_BeamLayer->getProperty( Property::T, Side::Back );
		double Rb_dir_dir = m_BeamLayer->getProperty( Property::R, Side::Back );

		// Diffuse component
		double Tf_dif_dif = m_DiffuseLayer->getProperty( Property::T, Side::Front );
		double Rf_dif_dif = m_DiffuseLayer->getProperty( Property::R, Side::Front );
		double Tb_dif_dif = m_DiffuseLayer->getProperty( Property::T, Side::Back );
		double Rb_dif_dif = m_DiffuseLayer->getProperty( Property::R, Side::Back );

		std::shared_ptr< CScatteringSurface > aFrontSurface =
			std::make_shared< CScatteringSurface >( Tf_dir_dir, Rf_dir_dir, Tf_dir_dif,
			                                   Rf_dir_dif, Tf_dif_dif, Rf_dif_dif );
		std::shared_ptr< CScatteringSurface >
			aBackSurface = std::make_shared< CScatteringSurface >( Tb_dir_dir, Rb_dir_dir, Tb_dir_dif,
			                                                  Rb_dir_dif, Tb_dif_dif, Rb_dif_dif );

		m_Layer = std::make_shared< CScatteringLayer >( aFrontSurface, aBackSurface );
	}

	double CEquivalentScatteringLayer::getInterreflectance(
		const CScatteringSurface& t_First,
		const CScatteringSurface& t_Second,
		const Scattering t_Scattering ) {
		return 1 - t_First.getPropertySimple( PropertySimple::R, t_Scattering ) *
			t_Second.getPropertySimple( PropertySimple::R, t_Scattering );
	}

	std::shared_ptr< SimpleResults > CEquivalentScatteringLayer::calcDirectDiffuseTransAndRefl(
		const CScatteringSurface& f1,
		const CScatteringSurface& b1,
		const CScatteringSurface& f2 ) const {

		auto aResult = std::make_shared< SimpleResults >();

		// Diffuse from direct beam component on the outside
		// Direct to direct interreflectance component
		double dirInterrefl = getInterreflectance( b1, f2, Scattering::DirectDirect );


		double If1_dif_ray = f1.getPropertySimple( PropertySimple::R, Scattering::DirectDiffuse );
		double Ib1_dif_ray = f1.getPropertySimple( PropertySimple::T, Scattering::DirectDiffuse );

		// Diffuse on surface from gap beam interreflections
		// First calculate direct beam that is incoming to surfaces b1 and f2
		double Incoming_f2_dir = f1.getPropertySimple( PropertySimple::T,
		                                               Scattering::DirectDirect ) / dirInterrefl;
		double Incoming_b1_dir = Incoming_f2_dir * f2.getPropertySimple( PropertySimple::R,
		                                                                 Scattering::DirectDirect );

		// Each component is calculated by simple multiplication of incoming beam with direct to diffuse property
		double If1_dif_inbm = Incoming_b1_dir * b1.getPropertySimple( PropertySimple::T, Scattering::DirectDiffuse );
		double Ib1_dif_inbm = Incoming_b1_dir * b1.getPropertySimple( PropertySimple::R, Scattering::DirectDiffuse );

		double If2_dif_inbm = Incoming_f2_dir * f2.getPropertySimple( PropertySimple::R, Scattering::DirectDiffuse );
		double Ib2_dif_inbm = Incoming_f2_dir * f2.getPropertySimple( PropertySimple::T, Scattering::DirectDiffuse );

		// Diffuse on surfaces from gap diffuse interreflections
		// First calculate diffuse components that are leaving surfaces in the gap
		double I_b1_dif = Ib1_dif_ray + Ib1_dif_inbm;
		double I_f2_dif = If2_dif_inbm;

		// Diffuse interreflectance component
		double difInterrefl = getInterreflectance( b1, f2, Scattering::DiffuseDiffuse );

		double I_fwd = ( I_b1_dif + I_f2_dif *
			b1.getPropertySimple( PropertySimple::R, Scattering::DiffuseDiffuse ) ) / difInterrefl;
		double I_bck = ( I_b1_dif * f2.getPropertySimple( PropertySimple::R, Scattering::DiffuseDiffuse )
			+ I_f2_dif ) / difInterrefl;

		double If1_dif_dif = I_bck * b1.getPropertySimple( PropertySimple::T, Scattering::DiffuseDiffuse );
		double Ib2_dif_dif = I_fwd * f2.getPropertySimple( PropertySimple::T, Scattering::DiffuseDiffuse );

		aResult->T = Ib2_dif_inbm + Ib2_dif_dif;
		aResult->R = If1_dif_ray + If1_dif_inbm + If1_dif_dif;

		return aResult;
	}

	void CEquivalentScatteringLayer::addLayerComponents( CScatteringLayer& t_Layer, const Side t_Side,
	                                                     const double t_Theta, const double t_Phi ) const {
		double Tf = t_Layer.getPropertySimple( PropertySimple::T, Side::Front, Scattering::DirectDirect, t_Theta, t_Phi );
		double Rf = t_Layer.getPropertySimple( PropertySimple::R, Side::Front, Scattering::DirectDirect, t_Theta, t_Phi );
		double Tb = t_Layer.getPropertySimple( PropertySimple::T, Side::Back, Scattering::DirectDirect, t_Theta, t_Phi );
		double Rb = t_Layer.getPropertySimple( PropertySimple::R, Side::Back, Scattering::DirectDirect, t_Theta, t_Phi );
		m_BeamLayer->addLayer( Tf, Rf, Tb, Rb, t_Side );

		Tf = t_Layer.getPropertySimple( PropertySimple::T, Side::Front, Scattering::DiffuseDiffuse, t_Theta, t_Phi );
		Rf = t_Layer.getPropertySimple( PropertySimple::R, Side::Front, Scattering::DiffuseDiffuse, t_Theta, t_Phi );
		Tb = t_Layer.getPropertySimple( PropertySimple::T, Side::Back, Scattering::DiffuseDiffuse, t_Theta, t_Phi );
		Rb = t_Layer.getPropertySimple( PropertySimple::R, Side::Back, Scattering::DiffuseDiffuse, t_Theta, t_Phi );
		m_DiffuseLayer->addLayer( Tf, Rf, Tb, Rb, t_Side );
	}

}
