#ifndef LAYERINTERFACES_H
#define LAYERINTERFACES_H

#include <memory>
#include <map>

#include "WCECommon.hpp"

namespace FenestrationCommon {

	enum class Side;

}

namespace Gases {

	class CGas;

}

namespace Tarcog {

	class ISurface;

	struct ForcedVentilation {
		ForcedVentilation() : Speed( 0 ), Temperature( 0 ) {
		};

		ForcedVentilation( double const t_Speed, double const t_Temperature ) :
			Speed( t_Speed ), Temperature( t_Temperature ) {
		};
		double Speed;
		double Temperature;
	};

	class CLayerGeometry : public virtual FenestrationCommon::CState {
	public:
		CLayerGeometry();
		CLayerGeometry( CLayerGeometry const& t_Layer );
		CLayerGeometry & operator=( CLayerGeometry const & t_Layer );

		virtual void setWidth( double const t_Width ) final;
		virtual void setHeight( double const t_Height ) final;
		virtual void setTilt( double const t_Tilt ) final;

	protected:
		double m_Width;
		double m_Height;
		double m_Tilt;
	};

	class CLayerHeatFlow : public virtual FenestrationCommon::CState {
	public:
		CLayerHeatFlow();
		CLayerHeatFlow( CLayerHeatFlow const& t_Layer );
		CLayerHeatFlow & operator=( CLayerHeatFlow const & t_Layer );
		CLayerHeatFlow( std::shared_ptr< ISurface > const& t_FrontSurface,
		                std::shared_ptr< ISurface > const& t_BackSurface );
		virtual double getHeatFlow() final;
		virtual double getGainFlow() final;
		virtual double getConductionConvectionCoefficient() final;
		virtual double getRadiationFlow() final;
		virtual double getConvectionConductionFlow() final;
		virtual std::shared_ptr< ISurface > getSurface( FenestrationCommon::Side const t_Position ) const final;
		virtual void setSurface( std::shared_ptr< ISurface > t_Surface,
		                         FenestrationCommon::Side const t_Position ) final;

	protected:
		virtual void calculateLayerHeatFlow() final;
		virtual void calculateRadiationFlow() = 0;
		virtual void calculateConvectionOrConductionFlow() = 0;
		bool areSurfacesInitalized() const;

		std::map< FenestrationCommon::Side, std::shared_ptr< ISurface > > m_Surface;
		double m_ConductiveConvectiveCoeff;
		double m_LayerGainFlow;
	};

	enum class AirVerticalDirection { None, Up, Down };

	enum class AirHorizontalDirection { None, Leeward, Windward };

	class CGasLayer : public virtual FenestrationCommon::CState {
	public:
		CGasLayer();
		explicit CGasLayer( double const t_Pressure );
		CGasLayer( double const t_Pressure, double const t_AirSpeed, AirVerticalDirection const t_AirDirection );
		CGasLayer( double const t_Pressure, double const t_AirSpeed, AirHorizontalDirection const t_AirDirection );
		CGasLayer( double const t_Pressure, std::shared_ptr< Gases::CGas > const& t_Gas );
		CGasLayer( CGasLayer const& t_Layer );
		CGasLayer & operator=( CGasLayer const & t_Layer );

		virtual double getPressure();

		virtual double getGasTemperature() = 0;

	protected:
		void initializeStateVariables() override;

		double m_Pressure;
		double m_AirSpeed;
		AirVerticalDirection m_AirVerticalDirection;
		AirHorizontalDirection m_AirHorizontalDirection;
		ForcedVentilation m_ForcedVentilation;

		std::shared_ptr< Gases::CGas > m_Gas;

	private:
		void onCreate();
	};

}

#endif
