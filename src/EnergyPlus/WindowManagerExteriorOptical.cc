// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

#include <assert.h>

// EnergyPlus headers
#include <DataEnvironment.hh>
#include <DataSurfaces.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataGlobals.hh>
#include <InputProcessor.hh>
#include <General.hh>
#include <WindowManager.hh>

// Windows library headers
#include <WCEMultiLayerOptics.hpp>

#include "WindowManagerExteriorOptical.hh"
#include "WindowManagerExteriorData.hh"

namespace EnergyPlus {

	using namespace FenestrationCommon;
	using namespace SpectralAveraging;
	using namespace SingleLayerOptics;

	using namespace DataEnvironment;
	using namespace DataSurfaces;
	using namespace DataHeatBalance;
	using namespace DataHeatBalFanSys;
	using namespace DataGlobals;
	using namespace General;

	namespace WindowManager {

		std::shared_ptr< CBSDFLayer > getBSDFLayer(
			const std::shared_ptr< MaterialProperties >& t_Material,
			const WavelengthRange t_Range ) {
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Simon Vidanovic
			//       DATE WRITTEN   September 2016
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// BSDF will be created in different ways that is based on material type

			std::shared_ptr< CWCELayerFactory > aFactory = nullptr;
			if ( t_Material->Group == WindowGlass ) {
				aFactory = std::make_shared< CWCESpecularLayerFactory >( t_Material, t_Range );
			}
			else if ( t_Material->Group == WindowBlind ) {
				aFactory = std::make_shared< CWCEVenetianBlindLayerFactory >( t_Material, t_Range );
			}
			else if ( t_Material->Group == Screen ) {
				aFactory = std::make_shared< CWCEScreenLayerFactory >( t_Material, t_Range );
			}
			else if ( t_Material->Group == Shade ) {
				aFactory = std::make_shared< CWCEDiffuseShadeLayerFactory >( t_Material, t_Range );
			}
			return aFactory->getBSDFLayer();
		}

		std::shared_ptr< CScatteringLayer > getScatteringLayer(
			const std::shared_ptr< MaterialProperties >& t_Material,
			const WavelengthRange t_Range ) {
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Simon Vidanovic
			//       DATE WRITTEN   May 2017
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Scattering will be created in different ways that is based on material type

			std::shared_ptr< CWCELayerFactory > aFactory = nullptr;
			if ( t_Material->Group == WindowGlass ) {
				aFactory = std::make_shared< CWCESpecularLayerFactory >( t_Material, t_Range );
			}
			else if ( t_Material->Group == WindowBlind ) {
				aFactory = std::make_shared< CWCEVenetianBlindLayerFactory >( t_Material, t_Range );
			}
			else if ( t_Material->Group == Screen ) {
				aFactory = std::make_shared< CWCEScreenLayerFactory >( t_Material, t_Range );
			}
			else if ( t_Material->Group == Shade ) {
				aFactory = std::make_shared< CWCEDiffuseShadeLayerFactory >( t_Material, t_Range );
			}
			return aFactory->getLayer();
		}

		void InitWCE_BSDFOpticalData() {
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Simon Vidanovic
			//       DATE WRITTEN   September 2016
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Initialize BSDF construction layers in Solar and Visible spectrum.

			auto aWinConstBSDF = CWindowConstructionsBSDF::instance();
			for ( auto ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum ) {
				auto& construction( Construct( ConstrNum ) );
				if ( construction.isGlazingConstruction() ) {
					for ( auto LayNum = 1; LayNum <= construction.TotLayers; ++LayNum ) {
						auto& material( Material( construction.LayerPoint( LayNum ) ) );
						if ( material.Group != WindowGas && material.Group != WindowGasMixture &&
							material.Group != ComplexWindowGap && material.Group != ComplexWindowShade ) {
							auto aMaterial = std::make_shared< MaterialProperties >();
							*aMaterial = material;

							// This is necessary because rest of EnergyPlus code relies on TransDiff property 
							// of construction. It will basically trigger Window optical calculations if this
							// property is >0.
							construction.TransDiff = 0.1;

							auto aRange = WavelengthRange::Solar;
							auto aSolarLayer = getBSDFLayer( aMaterial, aRange );
							aWinConstBSDF.pushBSDFLayer( aRange, ConstrNum, aSolarLayer );

							aRange = WavelengthRange::Visible;
							auto aVisibleLayer = getBSDFLayer( aMaterial, aRange );
							aWinConstBSDF.pushBSDFLayer( aRange, ConstrNum, aVisibleLayer );
						}

					}
				}
			}
		}

		void InitWCE_SimplifiedOpticalData() {
			// SUBROUTINE INFORMATION:
			//       AUTHOR         Simon Vidanovic
			//       DATE WRITTEN   May 2017
			//       MODIFIED       na
			//       RE-ENGINEERED  na

			// PURPOSE OF THIS SUBROUTINE:
			// Initialize scattering construction layers in Solar and Visible spectrum.

			// Calculate optical properties of blind-type layers entered with MATERIAL:WindowBlind
			// Calculation from this is used for IR properties. Need to make sure that properties
			// are calculated with new WCE optical engine (for both blinds and screens)
			if ( TotBlinds > 0 ) CalcWindowBlindProperties();

			// Initialize SurfaceScreen structure
			NumSurfaceScreens = TotScreens;
			if ( NumSurfaceScreens > 0 ) CalcWindowScreenProperties();

			auto aWinConstSimp = CWindowConstructionsSimplified::instance();
			for ( auto ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum ) {
				auto& construction( Construct( ConstrNum ) );
				if ( construction.isGlazingConstruction() ) {
					for ( auto LayNum = 1; LayNum <= construction.TotLayers; ++LayNum ) {
						auto& material( Material( construction.LayerPoint( LayNum ) ) );
						if ( material.Group != WindowGas && material.Group != WindowGasMixture &&
							material.Group != ComplexWindowGap && material.Group != ComplexWindowShade ) {
							auto aMaterial = std::make_shared< MaterialProperties >();
							*aMaterial = material;

							// This is necessary because rest of EnergyPlus code relies on TransDiff property 
							// of construction. It will basically trigger Window optical calculations if this
							// property is >0.
							construction.TransDiff = 0.1;

							auto aRange = WavelengthRange::Solar;
							auto aSolarLayer = getScatteringLayer( aMaterial, aRange );
							aWinConstSimp.pushLayer( aRange, ConstrNum, aSolarLayer );

							aRange = WavelengthRange::Visible;
							auto aVisibleLayer = getScatteringLayer( aMaterial, aRange );
							aWinConstSimp.pushLayer( aRange, ConstrNum, aVisibleLayer );
						}

					}
				}
			}

			// Get effective glass and shade/blind emissivities for windows that have interior blind or
			// shade. These are used to calculate zone MRT contribution from window when
			// interior blind/shade is deployed.

			for ( auto SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				if ( ! Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) continue;
				if ( SurfaceWindow( SurfNum ).WindowModelType == WindowBSDFModel ) continue; //Irrelevant for Complex Fen
				if ( Construct( Surface( SurfNum ).Construction ).WindowTypeEQL ) continue; // not required
				auto ConstrNumSh = SurfaceWindow( SurfNum ).ShadedConstruction;
				if ( ConstrNumSh == 0 ) continue;
				auto TotLay = Construct( ConstrNumSh ).TotLayers;
				auto IntShade = false;
				auto IntBlind = false;
				auto ShadeLayPtr = 0;
				auto BlNum = 0;
				if ( Material( Construct( ConstrNumSh ).LayerPoint( TotLay ) ).Group == Shade ) {
					IntShade = true;
					ShadeLayPtr = Construct( ConstrNumSh ).LayerPoint( TotLay );
				}
				if ( Material( Construct( ConstrNumSh ).LayerPoint( TotLay ) ).Group == WindowBlind ) {
					IntBlind = true;
					BlNum = Material( Construct( ConstrNumSh ).LayerPoint( TotLay ) ).BlindDataPtr;
				}

				if ( IntShade || IntBlind ) {
					for ( auto ISlatAng = 1; ISlatAng <= MaxSlatAngs; ++ISlatAng ) {
						auto EpsGlIR = 0.0;
						auto RhoGlIR = 0.0;
						if ( IntShade || IntBlind ) {
							EpsGlIR = Material( Construct( ConstrNumSh ).LayerPoint( TotLay - 1 ) ).AbsorpThermalBack;
							RhoGlIR = 1 - EpsGlIR;
						}
						if ( IntShade ) {
							auto TauShIR = Material( ShadeLayPtr ).TransThermal;
							auto EpsShIR = Material( ShadeLayPtr ).AbsorpThermal;
							auto RhoShIR = max( 0.0, 1.0 - TauShIR - EpsShIR );
							SurfaceWindow( SurfNum ).EffShBlindEmiss( 1 ) = EpsShIR * ( 1.0 + RhoGlIR * TauShIR / ( 1.0 - RhoGlIR * RhoShIR ) );
							SurfaceWindow( SurfNum ).EffGlassEmiss( 1 ) = EpsGlIR * TauShIR / ( 1.0 - RhoGlIR * RhoShIR );
						}
						if ( IntBlind ) {
							auto TauShIR = Blind( BlNum ).IRFrontTrans( ISlatAng );
							auto EpsShIR = Blind( BlNum ).IRBackEmiss( ISlatAng );
							auto RhoShIR = max( 0.0, 1.0 - TauShIR - EpsShIR );
							SurfaceWindow( SurfNum ).EffShBlindEmiss( ISlatAng ) = EpsShIR * ( 1.0 + RhoGlIR * TauShIR / ( 1.0 - RhoGlIR * RhoShIR ) );
							SurfaceWindow( SurfNum ).EffGlassEmiss( ISlatAng ) = EpsGlIR * TauShIR / ( 1.0 - RhoGlIR * RhoShIR );
						}
						// Loop over remaining slat angles only if blind with movable slats
						if ( IntShade ) break; // Loop over remaining slat angles only if blind
						if ( IntBlind ) {
							if ( Blind( BlNum ).SlatAngleType == FixedSlats ) break;
						}
					} // End of slat angle loop
				} // End of check if interior shade or interior blind
			} // End of surface loop
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCEMaterialFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCEMaterialFactory::CWCEMaterialFactory( const std::shared_ptr< MaterialProperties >& t_Material,
		                                          const WavelengthRange t_Range ) :
			m_MaterialProperties( t_Material ), m_Range( t_Range ), m_Initialized( false ) {

		}

		std::shared_ptr< CMaterial > CWCEMaterialFactory::getMaterial() {
			if ( !m_Initialized ) {
				init();
				m_Initialized = true;
			}
			return m_Material;
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCESpecularMaterialsFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCESpecularMaterialsFactory::CWCESpecularMaterialsFactory(
			const std::shared_ptr< MaterialProperties >& t_Material,
			const WavelengthRange t_Range ) : CWCEMaterialFactory( t_Material, t_Range ) {

		}

		void CWCESpecularMaterialsFactory::init() {
			auto aSolarSpectrum = CWCESpecturmProperties::getDefaultSolarRadiationSpectrum();
			std::shared_ptr< CSpectralSampleData > aSampleData = nullptr;
			if ( m_MaterialProperties->GlassSpectralDataPtr > 0 ) {
				aSampleData = CWCESpecturmProperties::getSpectralSample( m_MaterialProperties->GlassSpectralDataPtr );
			}
			else {
				aSampleData = CWCESpecturmProperties::getSpectralSample( *m_MaterialProperties );
			}

			auto aSample = std::make_shared< CSpectralSample >( aSampleData, aSolarSpectrum );

			auto aType = MaterialType::Monolithic;
			auto aRange = CWavelengthRange( m_Range );
			auto lowLambda = aRange.minLambda();
			auto highLambda = aRange.maxLambda();

			if ( m_Range == WavelengthRange::Visible ) {
				auto aPhotopicResponse = CWCESpecturmProperties::getDefaultVisiblePhotopicResponse();
				aSample->setDetectorData( aPhotopicResponse );
			}

			auto thickness = m_MaterialProperties->Thickness;
			m_Material = std::make_shared< CMaterialSample >( aSample, thickness, aType, lowLambda, highLambda );
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCEMaterialDualBandFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCEMaterialDualBandFactory::CWCEMaterialDualBandFactory(
			const std::shared_ptr< MaterialProperties >& t_Material,
			const WavelengthRange t_Range ) : CWCEMaterialFactory( t_Material, t_Range ) {

		}

		void CWCEMaterialDualBandFactory::init() {
			if ( m_Range == WavelengthRange::Visible ) {
				m_Material = createVisibleRangeMaterial();
			}
			else {
				auto aVisibleRangeMaterial = createVisibleRangeMaterial();
				auto aSolarRangeMaterial = createSolarRangeMaterial();
				// Ratio visible to solar range. It can be calculated from solar spectrum.
				auto ratio = 0.49;
				m_Material = std::make_shared< CMaterialDualBand >( aVisibleRangeMaterial, aSolarRangeMaterial, ratio );
			}
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCEVenetianBlindMaterialsFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCEVenetianBlindMaterialsFactory::CWCEVenetianBlindMaterialsFactory(
			const std::shared_ptr< MaterialProperties >& t_Material,
			const WavelengthRange t_Range ) : CWCEMaterialDualBandFactory( t_Material, t_Range ) {

		}

		std::shared_ptr< CMaterialSingleBand > CWCEVenetianBlindMaterialsFactory::createVisibleRangeMaterial() {
			auto blindDataPtr = m_MaterialProperties->BlindDataPtr;
			auto& blind( Blind( blindDataPtr ) );
			assert( blindDataPtr > 0 );

			auto aRange = CWavelengthRange( WavelengthRange::Visible );
			auto lowLambda = aRange.minLambda();
			auto highLambda = aRange.maxLambda();

			auto Tf = blind.SlatTransVisDiffDiff;
			auto Tb = blind.SlatTransVisDiffDiff;
			auto Rf = blind.SlatFrontReflVisDiffDiff;
			auto Rb = blind.SlatBackReflVisDiffDiff;

			return std::make_shared< CMaterialSingleBand >( Tf, Tb, Rf, Rb, lowLambda, highLambda );
		}

		std::shared_ptr< CMaterialSingleBand > CWCEVenetianBlindMaterialsFactory::createSolarRangeMaterial() {
			auto blindDataPtr = m_MaterialProperties->BlindDataPtr;
			auto& blind( Blind( blindDataPtr ) );
			assert( blindDataPtr > 0 );

			auto aRange = CWavelengthRange( WavelengthRange::Solar );
			auto lowLambda = aRange.minLambda();
			auto highLambda = aRange.maxLambda();

			auto Tf = blind.SlatTransSolDiffDiff;
			auto Tb = blind.SlatTransSolDiffDiff;
			auto Rf = blind.SlatFrontReflSolDiffDiff;
			auto Rb = blind.SlatBackReflSolDiffDiff;

			return std::make_shared< CMaterialSingleBand >( Tf, Tb, Rf, Rb, lowLambda, highLambda );
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCEScreenMaterialsFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCEScreenMaterialsFactory::CWCEScreenMaterialsFactory( const std::shared_ptr< MaterialProperties >& t_Material,
		                                                        const WavelengthRange t_Range ) : CWCEMaterialDualBandFactory( t_Material, t_Range ) {
			// Current EnergyPlus model does not support material transmittance different from zero.
			// To enable that, it would be necessary to change input in IDF

		}

		std::shared_ptr< CMaterialSingleBand > CWCEScreenMaterialsFactory::createVisibleRangeMaterial() {
			auto aRange = CWavelengthRange( WavelengthRange::Visible );
			auto lowLambda = aRange.minLambda();
			auto highLambda = aRange.maxLambda();

			auto Tf = 0.0;
			auto Tb = 0.0;
			auto Rf = m_MaterialProperties->ReflectShadeVis;
			auto Rb = m_MaterialProperties->ReflectShadeVis;

			return std::make_shared< CMaterialSingleBand >( Tf, Tb, Rf, Rb, lowLambda, highLambda );
		}

		std::shared_ptr< CMaterialSingleBand > CWCEScreenMaterialsFactory::createSolarRangeMaterial() {
			auto aRange = CWavelengthRange( WavelengthRange::Solar );
			auto lowLambda = aRange.minLambda();
			auto highLambda = aRange.maxLambda();

			auto Tf = 0.0;
			auto Tb = 0.0;
			auto Rf = m_MaterialProperties->ReflectShade;
			auto Rb = m_MaterialProperties->ReflectShade;

			return std::make_shared< CMaterialSingleBand >( Tf, Tb, Rf, Rb, lowLambda, highLambda );
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCEDiffuseShadeMaterialsFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCEDiffuseShadeMaterialsFactory::CWCEDiffuseShadeMaterialsFactory( const std::shared_ptr< MaterialProperties >& t_Material,
		                                                                    const WavelengthRange t_Range ) : CWCEMaterialDualBandFactory( t_Material, t_Range ) {

		}

		std::shared_ptr< CMaterialSingleBand > CWCEDiffuseShadeMaterialsFactory::createVisibleRangeMaterial() {
			auto aRange = CWavelengthRange( WavelengthRange::Visible );
			auto lowLambda = aRange.minLambda();
			auto highLambda = aRange.maxLambda();

			auto Tf = m_MaterialProperties->TransVis;
			auto Tb = m_MaterialProperties->TransVis;
			auto Rf = m_MaterialProperties->ReflectShadeVis;
			auto Rb = m_MaterialProperties->ReflectShadeVis;

			return std::make_shared< CMaterialSingleBand >( Tf, Tb, Rf, Rb, lowLambda, highLambda );
		}

		std::shared_ptr< CMaterialSingleBand > CWCEDiffuseShadeMaterialsFactory::createSolarRangeMaterial() {
			auto aRange = CWavelengthRange( WavelengthRange::Solar );
			auto lowLambda = aRange.minLambda();
			auto highLambda = aRange.maxLambda();

			auto Tf = m_MaterialProperties->Trans;
			auto Tb = m_MaterialProperties->Trans;
			auto Rf = m_MaterialProperties->ReflectShade;
			auto Rb = m_MaterialProperties->ReflectShade;

			return std::make_shared< CMaterialSingleBand >( Tf, Tb, Rf, Rb, lowLambda, highLambda );
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCECellFactory
		///////////////////////////////////////////////////////////////////////////////
		IWCECellDescriptionFactory::IWCECellDescriptionFactory( const std::shared_ptr< MaterialProperties >& t_Material ) :
			m_Material( t_Material ) {

		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCESpecularCellFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCESpecularCellFactory::CWCESpecularCellFactory(
			const std::shared_ptr< EnergyPlus::DataHeatBalance::MaterialProperties >& t_Material ) :
			IWCECellDescriptionFactory( t_Material ) {

		}

		std::shared_ptr< ICellDescription > CWCESpecularCellFactory::getCellDescription() {
			return std::make_shared< CSpecularCellDescription >();
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCEVenetianBlindCellFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCEVenetianBlindCellFactory::CWCEVenetianBlindCellFactory(
			const std::shared_ptr< EnergyPlus::DataHeatBalance::MaterialProperties >& t_Material ) :
			IWCECellDescriptionFactory( t_Material ) {

		}

		std::shared_ptr< ICellDescription > CWCEVenetianBlindCellFactory::getCellDescription() {
			assert( m_Material != nullptr );
			auto blindDataPtr = m_Material->BlindDataPtr;
			auto& blind( Blind( blindDataPtr ) );
			assert( blindDataPtr > 0 );

			auto slatWidth = blind.SlatWidth;
			auto slatSpacing = blind.SlatSeparation;
			auto slatTiltAngle = 90 - blind.SlatAngle; // Need to convert to WCE system
			auto curvatureRadius = 0.0; // No curvature radius in current IDF definition
			size_t numOfSlatSegments = 5; // Number of segments to use in venetian calculations
			return std::make_shared< CVenetianCellDescription >( slatWidth, slatSpacing, slatTiltAngle,
			                                                curvatureRadius, numOfSlatSegments );
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCEScreenCellFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCEScreenCellFactory::CWCEScreenCellFactory(
			const std::shared_ptr< EnergyPlus::DataHeatBalance::MaterialProperties >& t_Material ) :
			IWCECellDescriptionFactory( t_Material ) {

		}

		std::shared_ptr< ICellDescription > CWCEScreenCellFactory::getCellDescription() {
			assert( m_Material != nullptr );
			auto diameter = m_Material->Thickness; // Thickness in this case is diameter
			// ratio is not saved withing material but rather calculated from transmittance
			auto ratio = 1.0 - sqrt( m_Material->Trans );
			auto spacing = diameter / ratio;
			return std::make_shared< CWovenCellDescription >( diameter, spacing );
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCEDiffuseShadeCellFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCEDiffuseShadeCellFactory::CWCEDiffuseShadeCellFactory(
			const std::shared_ptr< EnergyPlus::DataHeatBalance::MaterialProperties >& t_Material ) :
			IWCECellDescriptionFactory( t_Material ) {

		}

		std::shared_ptr< ICellDescription > CWCEDiffuseShadeCellFactory::getCellDescription() {
			return std::make_shared< CPerfectDiffuseCellDescription >();
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCEBSDFLayerFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCELayerFactory::CWCELayerFactory(
			const std::shared_ptr< MaterialProperties >& t_Material, const WavelengthRange t_Range ) :
			m_Material( t_Material ), m_Range( t_Range ), m_BSDFInitialized( false ),
			m_SimpleInitialized( false ), m_MaterialFactory( nullptr ) {

		}

		std::pair< std::shared_ptr< CMaterial >, std::shared_ptr< ICellDescription > > CWCELayerFactory::init() {
			createMaterialFactory();
			auto aMaterial = m_MaterialFactory->getMaterial();
			assert( aMaterial != nullptr );
			auto aCellDescription = getCellDescription();
			assert( aCellDescription != nullptr );

			return std::make_pair( aMaterial, aCellDescription );
		}

		std::shared_ptr< CBSDFLayer > CWCELayerFactory::getBSDFLayer() {
			if ( !m_BSDFInitialized ) {
				auto res = init();
				auto aBSDF = std::make_shared< CBSDFHemisphere >( BSDFBasis::Full );

				auto aMaker = CBSDFLayerMaker( res.first, aBSDF, res.second );
				m_BSDFLayer = aMaker.getLayer();
				m_BSDFInitialized = true;
			}
			return m_BSDFLayer;
		}

		std::shared_ptr< CScatteringLayer > CWCELayerFactory::getLayer() {
			if ( !m_SimpleInitialized ) {
				auto res = init();

				m_ScatteringLayer = std::make_shared< CScatteringLayer >( res.first, res.second );
				m_SimpleInitialized = true;
			}
			return m_ScatteringLayer;
		}

		std::shared_ptr< ICellDescription > CWCELayerFactory::getCellDescription() const {
			return m_CellFactory->getCellDescription();
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCESpecularLayerFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCESpecularLayerFactory::CWCESpecularLayerFactory(
			const std::shared_ptr< MaterialProperties >& t_Material,
			const WavelengthRange t_Range ) : CWCELayerFactory( t_Material, t_Range ) {
			m_CellFactory = std::make_shared< CWCESpecularCellFactory >( t_Material );
		}

		void CWCESpecularLayerFactory::createMaterialFactory() {
			m_MaterialFactory = std::make_shared< CWCESpecularMaterialsFactory >( m_Material, m_Range );
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCEVenetianBlindLayerFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCEVenetianBlindLayerFactory::CWCEVenetianBlindLayerFactory(
			const std::shared_ptr< MaterialProperties >& t_Material, const WavelengthRange t_Range ) :
			CWCELayerFactory( t_Material, t_Range ) {
			m_CellFactory = std::make_shared< CWCEVenetianBlindCellFactory >( t_Material );
		}

		void CWCEVenetianBlindLayerFactory::createMaterialFactory() {
			m_MaterialFactory = std::make_shared< CWCEVenetianBlindMaterialsFactory >( m_Material, m_Range );
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCEScreenLayerFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCEScreenLayerFactory::CWCEScreenLayerFactory(
			const std::shared_ptr< MaterialProperties >& t_Material, const WavelengthRange t_Range ) :
			CWCELayerFactory( t_Material, t_Range ) {
			m_CellFactory = std::make_shared< CWCEScreenCellFactory >( t_Material );
		}

		void CWCEScreenLayerFactory::createMaterialFactory() {
			m_MaterialFactory = std::make_shared< CWCEScreenMaterialsFactory >( m_Material, m_Range );
		}

		///////////////////////////////////////////////////////////////////////////////
		//   CWCEDiffuseShadeLayerFactory
		///////////////////////////////////////////////////////////////////////////////
		CWCEDiffuseShadeLayerFactory::CWCEDiffuseShadeLayerFactory(
			const std::shared_ptr< MaterialProperties >& t_Material, const WavelengthRange t_Range ) :
			CWCELayerFactory( t_Material, t_Range ) {
			m_CellFactory = std::make_shared< CWCEDiffuseShadeCellFactory >( t_Material );
		}

		void CWCEDiffuseShadeLayerFactory::createMaterialFactory() {
			m_MaterialFactory = std::make_shared< CWCEDiffuseShadeMaterialsFactory >( m_Material, m_Range );
		}

	}
}
