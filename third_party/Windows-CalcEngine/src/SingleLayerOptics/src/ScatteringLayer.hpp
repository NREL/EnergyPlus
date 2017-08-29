#ifndef SCATTERINGLAYER_H
#define SCATTERINGLAYER_H

#include <memory>
#include <map>

#include "BSDFLayerMaker.hpp"

namespace FenestrationCommon {

	enum class Side;
	enum class PropertySimple;
	enum class Scattering;
	enum class ScatteringSimple;
	class CSeries;

}

namespace SingleLayerOptics {

	class CScatteringSurface;
	class CLayerSingleComponent;
	class CMaterial;
	class CBSDFIntegrator;
	class CBSDFLayer;
	class ICellDescription;

	// Handles general case layer when properties can be direct, diffuse or combination between these two.
	class CScatteringLayer {
	public:
		CScatteringLayer( const std::shared_ptr< CScatteringSurface >& t_Front,
		                  const std::shared_ptr< CScatteringSurface >& t_Back );

		CScatteringLayer( const std::shared_ptr< CScatteringLayer >& t_Layer );

		CScatteringLayer(
			const double Tf_dir_dir, const double Rf_dir_dir,
			const double Tb_dir_dir, const double Rb_dir_dir,
			const double Tf_dir_dif, const double Rf_dir_dif,
			const double Tb_dir_dif, const double Rb_dir_dif,
			const double Tf_dif_dif, const double Rf_dif_dif,
			const double Tb_dif_dif, const double Rb_dif_dif );

		CScatteringLayer( const std::shared_ptr< CMaterial >& t_Material,
		                  std::shared_ptr< ICellDescription > t_Description = nullptr,
		                  const DistributionMethod t_Method = DistributionMethod::UniformDiffuse );

		void setSourceData( std::shared_ptr< FenestrationCommon::CSeries > t_SourceData ) const;

		std::shared_ptr< CScatteringSurface > getSurface( const FenestrationCommon::Side t_Side );

		double getPropertySimple(
			const FenestrationCommon::PropertySimple t_Property,
			const FenestrationCommon::Side t_Side,
			const FenestrationCommon::Scattering t_Scattering,
			const double t_Theta = 0,
			const double t_Phi = 0 );

		//void setPropertySimple( const FenestrationCommon::PropertySimple t_Property,
		//  const FenestrationCommon::Side t_Side, const FenestrationCommon::Scattering t_Scattering,
		//  const double value ) const;

		double getAbsorptance(
			const FenestrationCommon::Side t_Side,
			const FenestrationCommon::ScatteringSimple t_Scattering,
			const double t_Theta = 0,
			const double t_Phi = 0 );

		double getAbsorptance(
			const FenestrationCommon::Side t_Side,
			const double t_Theta = 0,
			const double t_Phi = 0 );

		std::shared_ptr< CLayerSingleComponent > getLayer(
			const FenestrationCommon::Scattering t_Scattering,
			const double t_Theta = 0,
			const double t_Phi = 0 );

	private:
		void createResultsAtAngle( const double t_Theta, const double t_Phi );

		std::shared_ptr< CScatteringSurface > createSurface( const FenestrationCommon::Side t_Side,
		                                                     const double t_Theta, const double t_Phi );

		bool checkCurrentAngles( const double t_Theta, const double t_Phi );

		std::map< FenestrationCommon::Side, std::shared_ptr< CScatteringSurface > > m_Surface;

		std::shared_ptr< CBSDFLayer > m_BSDFLayer;
		std::shared_ptr< CBaseCell > m_Cell;

		double m_Theta;
		double m_Phi;

	};

}

#endif
