#ifndef VENETIANCELL_H
#define VENETIANCELL_H

#include <memory>
#include <vector>
#include <map>

#include "UniformDiffuseCell.hpp"
#include "DirectionalDiffuseCell.hpp"

namespace FenestrationCommon {

	class CSquareMatrix;
	enum class Side;

}

namespace Viewer {

	struct BeamViewFactor;

}

namespace SingleLayerOptics {

	class ICellDescription;
	class CVenetianCellDescription;
	class CBeamDirection;

	class CVenetianBase : public CUniformDiffuseCell, public CDirectionalDiffuseCell {
	public:
		CVenetianBase( const std::shared_ptr< CMaterial >& t_MaterialProperties,
		               const std::shared_ptr< ICellDescription >& t_Cell );

	protected:
		std::shared_ptr< CVenetianCellDescription > getCellAsVenetian() const;
	};

	// Simple structure to hold backward and forward irradiances
	struct SegmentIrradiance {
	public:
		SegmentIrradiance() : E_f( 0 ), E_b( 0 ) {
		};
		double E_f;
		double E_b;
	};

	class CVenetianSlatEnergies {
	public:
		CVenetianSlatEnergies( const CBeamDirection& t_BeamDirection,
		                       const std::shared_ptr< std::vector< SegmentIrradiance > >& t_SlatIrradiances,
		                       const std::shared_ptr< std::vector< double > >& t_SlatRadiances );

		SegmentIrradiance& irradiances( const size_t index ) const;
		double radiances( const size_t index ) const;
		std::shared_ptr< const CBeamDirection > direction() const;
		size_t size() const;

	private:
		// Keep slat energies for certain beam incoming directions
		std::shared_ptr< std::vector< SegmentIrradiance > > m_SlatIrradiances;
		std::shared_ptr< std::vector< double > > m_SlatRadiances;
		// Direction for which energies are calculated
		std::shared_ptr< CBeamDirection > m_CalcDirection;
	};

	// Keeping intermediate results for backward and forward directions.
	class CVenetianCellEnergy {
	public:
		CVenetianCellEnergy();
		CVenetianCellEnergy( const std::shared_ptr< CVenetianCellDescription >& t_Cell,
		                     const double Tf, const double Tb, const double Rf, const double Rb );

		double T_dir_dir( const CBeamDirection& t_Direction );
		double T_dir_dif( const CBeamDirection& t_Direction );
		double R_dir_dif( const CBeamDirection& t_Direction );

		double T_dir_dif( const CBeamDirection& t_IncomingDirection,
		                  const CBeamDirection& t_OutgoingDirection );
		double R_dir_dif( const CBeamDirection& t_IncomingDirection,
		                  const CBeamDirection& t_OutgoingDirection );

		double T_dif_dif();
		double R_dif_dif();

	private:

		// Keeps information about beam view factor and percentage view
		struct BeamSegmentView {
		public:
			BeamSegmentView() : viewFactor( 0 ), percentViewed( 0 ) {
			};
			double viewFactor;
			double percentViewed;
		};

		class CSlatEnergyResults {
		public:
			CSlatEnergyResults();

			std::shared_ptr< CVenetianSlatEnergies > getEnergies(
				const CBeamDirection& t_BeamDirection ) const;

			std::shared_ptr< CVenetianSlatEnergies > append( const CBeamDirection& t_BeamDirection,
			                                                 const std::shared_ptr< std::vector< SegmentIrradiance > >& t_SlatIrradiances,
			                                                 const std::shared_ptr< std::vector< double > >& t_SlatRadiances );

		private:
			std::vector< std::shared_ptr< CVenetianSlatEnergies > > m_Energies;
		};

		// Create mapping from view factors matrix to front and back slats (fills b and f vectors of this class)
		void createSlatsMapping();

		// Energy matrix is valid for any incoming direction. Depends on geometry and will be caluculated only
		// once and stored into m_Energy field
		void formEnergyMatrix();

		// calculate slat irradiances and radiances based on incoming beam
		void calculateSlatEnergiesFromBeam( const CBeamDirection& t_Direction );

		// Irradiances for given incoming direction
		std::shared_ptr< std::vector< SegmentIrradiance > > slatIrradiances(
			const CBeamDirection& t_IncomingDirection );

		// Radiances for given incoming direction
		std::shared_ptr< std::vector< double > > slatRadiances(
			std::shared_ptr< std::vector< SegmentIrradiance > > t_Irradiances );

		// Creates diffuse to diffuse vector. Right hand side of the equation
		std::shared_ptr< std::vector< double > > diffuseVector();

		// Create beam to diffuse vector. Right hand side of the equation
		std::shared_ptr< std::vector< CVenetianCellEnergy::BeamSegmentView > >
		beamVector( const CBeamDirection& t_Direction, const FenestrationCommon::Side t_Side );

		std::shared_ptr< CVenetianCellDescription > m_Cell;
		double m_Tf;
		double m_Tb;
		double m_Rf;
		double m_Rb;
		std::shared_ptr< FenestrationCommon::CSquareMatrix > m_Energy;

		// Holds mappings for the slats. Used for mapping between view factors and energy matrix.
		std::vector< size_t > b;
		std::vector< size_t > f;

		// Keeps pointer to valid slat energies (for given direction). If result is not valid, it must be pulled
		// out of m_SlatEnergyResults and assigned to this pointer.
		std::shared_ptr< CVenetianSlatEnergies > m_CurrentSlatEnergies;

		// Keep results for slat radiances and irradiances for different directions.
		// Once radiances and irradiances are calculated for certain direction, results are stored here.
		// That reduces necessity to recalculate results multiple times for same direction.
		// Note that direction is always incoming direction.
		CSlatEnergyResults m_SlatEnergyResults;

	};

	class CVenetianEnergy {
	public:
		CVenetianEnergy();
		CVenetianEnergy( const CMaterial& t_Material, const std::shared_ptr< CVenetianCellDescription >& t_Cell );
		CVenetianEnergy( const double Tf, const double Tb, const double Rf, const double Rb,
		                 const std::shared_ptr< CVenetianCellDescription >& t_Cell );

		std::shared_ptr< CVenetianCellEnergy > getCell( const FenestrationCommon::Side t_Side ) const;

	private:
		// construction of forward and backward cells from both constructors have identical part of the code
		void createForwardAndBackward( const double Tf, const double Tb, const double Rf, const double Rb,
		                               const std::shared_ptr< CVenetianCellDescription >& t_Cell );

		std::map< FenestrationCommon::Side, std::shared_ptr< CVenetianCellEnergy > > m_CellEnergy;

	};

	class CVenetianCell : public CVenetianBase {
	public:
		CVenetianCell( const std::shared_ptr< CMaterial >& t_MaterialProperties,
		               const std::shared_ptr< ICellDescription >& t_Cell );

		void setSourceData( std::shared_ptr< FenestrationCommon::CSeries > t_SourceData );

		double T_dir_dir( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );
		std::vector< double > T_dir_dir_band( const FenestrationCommon::Side t_Side,
		                                      const CBeamDirection& t_Direction );

		/////////////////////////////////////////////////////////////////////////////////////////////
		// Uniform diffuse components
		/////////////////////////////////////////////////////////////////////////////////////////////
		double T_dir_dif( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );
		std::vector< double > T_dir_dif_band( const FenestrationCommon::Side t_Side,
		                                      const CBeamDirection& t_Direction );
		double R_dir_dif( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );
		std::vector< double > R_dir_dif_band( const FenestrationCommon::Side t_Side,
		                                      const CBeamDirection& t_Direction );

		/////////////////////////////////////////////////////////////////////////////////////////////
		// Directional diffuse components
		/////////////////////////////////////////////////////////////////////////////////////////////
		double T_dir_dif( const FenestrationCommon::Side t_Side,
		                  const CBeamDirection& t_IncomingDirection,
		                  const CBeamDirection& t_OutgoingDirection );
		double R_dir_dif( const FenestrationCommon::Side t_Side,
		                  const CBeamDirection& t_IncomingDirection,
		                  const CBeamDirection& t_OutgoingDirection );

		std::shared_ptr< std::vector< double > > T_dir_dif_band( const FenestrationCommon::Side t_Side,
		                                                         const CBeamDirection& t_IncomingDirection,
		                                                         const CBeamDirection& t_OutgoingDirection );

		std::shared_ptr< std::vector< double > > R_dir_dif_band( const FenestrationCommon::Side t_Side,
		                                                         const CBeamDirection& t_IncomingDirection,
		                                                         const CBeamDirection& t_OutgoingDirection );

		// Functions specific only for Venetian cell. Diffuse to diffuse component based only on
		// view factors
		double T_dif_dif( const FenestrationCommon::Side t_Side );
		double R_dif_dif( const FenestrationCommon::Side t_Side );

	private:
		void generateVenetianEnergy();
		// Energy calculations for whole band
		CVenetianEnergy m_Energy;

		// Energy calculations for material range (wavelengths)
		std::vector< CVenetianEnergy > m_EnergiesBand;

	};

}

#endif
