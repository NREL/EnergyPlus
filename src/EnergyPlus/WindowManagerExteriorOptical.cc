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
#include <algorithm>

// EnergyPlus headers
#include "DataEnvironment.hh"
#include "DataSurfaces.hh"
#include "DataHeatBalance.hh"
#include "DataHeatBalFanSys.hh"
#include "DataGlobals.hh"
#include "InputProcessor.hh"
#include "General.hh"
#include "WindowManager.hh"


// Windows library headers
#include "WindowManagerExteriorOptical.hh"
#include "WindowManagerExteriorData.hh"
#include "SpectralSample.hpp"
#include "SpecularLayer.hpp"
#include "MeasuredSampleData.hpp"
#include "FenestrationCommon.hpp"
#include "Series.hpp"
#include "MaterialDescription.hpp"
#include "AngularSpectralSample.hpp"
#include "Hemispherical2DIntegrator.hpp"
#include "IntegratorStrategy.hpp"
#include "WavelengthRange.hpp"
#include "CellDescription.hpp"
#include "VenetianCellDescription.hpp"
#include "WovenCellDescription.hpp"
#include "BSDFDirections.hpp"
#include "BSDFLayerMaker.hpp"
#include "BSDFLayer.hpp"
#include "BSDFResults.hpp"
#include "OpticalLayer.hpp"
#include "OpticalSurface.hpp"

namespace EnergyPlus {

  using namespace std;
  using namespace FenestrationCommon;
  using namespace SpectralAveraging;
  using namespace LayerOptics;

  using namespace DataEnvironment;
  using namespace DataSurfaces;
  using namespace DataHeatBalance;
  using namespace DataHeatBalFanSys;
  using namespace DataGlobals;
  using namespace General;

  namespace WindowManager {

    unique_ptr< CWCEIntegrator > getIntegrator( const MaterialProperties& material ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Integrators are dependent on type of material the layer.
      unique_ptr< CWCEIntegrator > aInteg = nullptr;
      if( material.Group == WindowGlass ) {
        aInteg = make_unique< CWCESpecularIntegrator >( material );
      } else if( material.Group == WindowBlind ) {
        aInteg = make_unique< CWCEVenetianBlindIntegrator >( material );
      } else if( material.Group == Screen ) {
        aInteg = make_unique< CWCEScreenIntegrator >( material );
      } else if( material.Group == Shade ) {
        aInteg = make_unique< CWCEDiffuseShadeIntegrator >( material );
      }
      return aInteg;
    }

    void InitWCEOpticalData() {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Calculation of diffuse optical properties for IGU layers. These properties that are calculated one
      // time only since they are independent on sun position.

      for( auto ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum ) {
        auto& construction( Construct( ConstrNum ) );
        if( construction.isGlazingConstruction() ) {
          for( auto LayNum = 1; LayNum <= construction.TotSolidLayers; ++LayNum ) {
            auto& material( Material( construction.LayerPoint( LayNum ) ) );
            CWCEIntegrator aInteg = *getIntegrator( material );

            StoreOpticalData( aInteg, WavelengthRange::Solar, ConstrNum );
            StoreOpticalData( aInteg, WavelengthRange::Visible, ConstrNum );
          }
        }
      }
    }

    void StoreOpticalData( const CWCEIntegrator& t_Integrator, const WavelengthRange t_Range,
      const int t_ConstrNum ) {
      // All window constructions used by Windows-CalcEngine are kept in separate locations
      CWindowConstructions aWinConst = CWindowConstructions::instance();
      shared_ptr< IGU_Layers > aLayers = aWinConst.getLayers( t_ConstrNum, t_Range );

      double Trans = t_Integrator.getProperty( Side::Front, Property::T, t_Range );
      double Rf = t_Integrator.getProperty( Side::Front, Property::R, t_Range );
      double Rb = t_Integrator.getProperty( Side::Back, Property::R, t_Range );

      // Only diffuse properties are calculated here so far. Direct-direct and direct-diffuse will
      // be calculated layer (at each timestep)
      shared_ptr< CLayer > aLayer = make_shared< CLayer >( 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        Trans, Rf, Trans, Rb );

      // This is first time we use created layer, so push it onto array
      aLayers->push_back( aLayer );

    }

    ///////////////////////////////////////////////////////////////////////////////
    //       CWCESpecturmProperties
    ///////////////////////////////////////////////////////////////////////////////
    shared_ptr< CSeries > CWCESpecturmProperties::getSolarRadiationSpecturm() {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Handles solar radiation spetrum from defalut location or IDF
      shared_ptr< CSeries > solarRadiation = make_shared< CSeries >();

      for( auto i = 1; i <= nume; ++i ) {
        solarRadiation->addProperty( wle( i ), e( i ) );
      }

      return solarRadiation;
    }

    ///////////////////////////////////////////////////////////////////////////////
    shared_ptr< CSeries > CWCESpecturmProperties::getVisiblePhotopicResponse() {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Handles solar radiation spetrum from defalut location or IDF
      shared_ptr< CSeries > visibleResponse = make_shared< CSeries >();

      for( auto i = 1; i <= numt3; ++i ) {
        visibleResponse->addProperty( wlt3( i ), y30( i ) );
      }

      return visibleResponse;
    }

    ///////////////////////////////////////////////////////////////////////////////
    shared_ptr< CSpectralSampleData > CWCESpecturmProperties::getSpectralSample( const int t_SampleDataPtr ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Reads spectral data values
      assert( t_SampleDataPtr != 0 ); // It must not be called for zero value
      shared_ptr< CSpectralSampleData > aSampleData = make_shared< CSpectralSampleData >();
      auto spectralData = SpectralData( t_SampleDataPtr );
      int numOfWl = spectralData.NumOfWavelengths;
      for( auto i = 1; i <= numOfWl; ++i ) {
        double wl = spectralData.WaveLength( i );
        double T = spectralData.Trans( i );
        double Rf = spectralData.ReflFront( i );
        double Rb = spectralData.ReflBack( i );
        aSampleData->addRecord( wl, T, Rf, Rb );
      }

      return aSampleData;
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCERangeIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    // CLASS INFORMATION:
    //       AUTHOR         Simon Vidanovic
    //       DATE WRITTEN   September 2016
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS CLASS:
    // Virtual class that provides common interface for calling in EnergyPlus routines
    CWCERangeIntegrator::CWCERangeIntegrator() {

    }

    ///////////////////////////////////////////////////////////////////////////////
    // CWCEIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    CWCEIntegrator::CWCEIntegrator() {
      m_Integrator[ WavelengthRange::Solar ] = nullptr;
      m_Integrator[ WavelengthRange::Visible ] = nullptr;
    }

    double CWCEIntegrator::getProperty( const Side t_Side, const Property t_Property, 
      const WavelengthRange t_Range ) const {
      return m_Integrator.at( t_Range )->getProperty( t_Side, t_Property );
    }

    ///////////////////////////////////////////////////////////////////////////////
    //       CWCESpecularRangeIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    CWCESpecularRangeIntegrator::CWCESpecularRangeIntegrator( const MaterialProperties &material,
      const WavelengthRange t_Range ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Integrator constructor for specular layers. It integratates optical properties over certain spectral range
      // and for angles between 0 and 90.
      shared_ptr< CSeries > aSolarSpectrum = CWCESpecturmProperties::getSolarRadiationSpecturm();
      shared_ptr< CSpectralSampleData > aSampleData = CWCESpecturmProperties::getSpectralSample( material.GlassSpectralDataPtr );
      shared_ptr< CSpectralSample > aSample = make_shared< CSpectralSample >( aSampleData, aSolarSpectrum );
      CWavelengthRangeFactory aWVFactory = CWavelengthRangeFactory();
      CWavelengthRange aRange = *aWVFactory.getWavelengthRange( t_Range );
      double lowLambda = aRange.minLambda();
      double highLambda = aRange.maxLambda();
      if( t_Range == WavelengthRange::Visible ) {
        shared_ptr< CSeries > aPhotopicResponse = CWCESpecturmProperties::getVisiblePhotopicResponse();
        aSample->setDetectorData( aPhotopicResponse );
      }

      m_AngularSample = make_shared< CAngularSpectralSample >( aSample, material.Thickness, 
        SpecularMaterialType::Monolithic );
      calculateProperties( Side::Front, lowLambda, highLambda );
      calculateProperties( Side::Back, lowLambda, highLambda );
    }

    double CWCESpecularRangeIntegrator::getProperty( const Side t_Side, const Property t_Property ) const {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Reads specific property from private map
      return m_Results.at( make_pair( t_Side, t_Property ) );
    }

    void CWCESpecularRangeIntegrator::calculateProperties( const Side t_Side, const double lowLambda, 
      const double highLambda ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Calculate all properties for given spectral range
      m_Results[ make_pair( t_Side, Property::T ) ] = calculateProperty( t_Side, Property::T, lowLambda, highLambda );
      m_Results[ make_pair( t_Side, Property::R ) ] = calculateProperty( t_Side, Property::R, lowLambda, highLambda );
      m_Results[ make_pair( t_Side, Property::Abs ) ] =
        1 - m_Results.at( make_pair( t_Side, Property::T ) ) - m_Results.at( make_pair( t_Side, Property::R ) );
    }

    double CWCESpecularRangeIntegrator::calculateProperty( const Side t_Side, const Property t_Property,
      const double lowLambda, const double highLambda ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Calculate single property for given spectral range.
      vector< double > angle{ 0, 10, 20, 30, 40, 50, 60, 70, 80, 90 };

      CSeries aSeries;

      for( auto i = 0; i < angle.size(); ++i ) {
        aSeries.addProperty( angle[ i ], m_AngularSample->getProperty( lowLambda, highLambda, 
          t_Property, t_Side, angle[ i ] ) );
      }

      CHemispherical2DIntegrator aIntegrator = CHemispherical2DIntegrator( aSeries, IntegrationType::Trapezoidal );

      return aIntegrator.value();
    }

    ///////////////////////////////////////////////////////////////////////////////
    //       CWCESpecularIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    CWCESpecularIntegrator::CWCESpecularIntegrator( const MaterialProperties &material ) : CWCEIntegrator() {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Provides integrators for several spectral ranges (Visible and Solar)
      m_Integrator[ WavelengthRange::Solar ] = 
        make_shared< CWCESpecularRangeIntegrator >( material, WavelengthRange::Solar );
      m_Integrator[ WavelengthRange::Visible ] = 
        make_shared< CWCESpecularRangeIntegrator >( material, WavelengthRange::Visible );
    }

    double CWCESpecularIntegrator::getProperty( const Side t_Side, const Property t_Property,
      const WavelengthRange t_Range ) const {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Returns property from private map field
      return m_Integrator.at( t_Range )->getProperty( t_Side, t_Property );
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEShadeMaterialFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEShadeMaterialFactory::CWCEShadeMaterialFactory() {

    }

    double CWCEShadeMaterialFactory::getProperty( const WavelengthRange t_Range, const Side t_Side,
      const Property t_Property ) const {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Provides common interface for all shade material factories (Venetian, Woven, etc)
      if( t_Range == WavelengthRange::Solar ) {
        return m_Solar.at( make_pair( t_Side, t_Property ) );
      } else {
        return m_Visible.at( make_pair( t_Side, t_Property ) );
      }
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEShadeRangeIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    CWCEShadeRangeIntegrator::CWCEShadeRangeIntegrator() : CWCERangeIntegrator() {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Common interface initialization for integrator over single spectral range
      m_Shade = nullptr;
    }

    double CWCEShadeRangeIntegrator::getProperty( const Side t_Side, const Property t_Property ) const {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Extraction of data from shadnig device results.
      shared_ptr< CBSDFResults > aResults = m_Shade->getResults();

      if( t_Property == Property::T ) {
        return aResults->TauDiff( t_Side );
      } else {
        return aResults->RhoDiff( t_Side );
      }
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEVenetianBlindMaterialsFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEVenetianBlindMaterialsFactory::CWCEVenetianBlindMaterialsFactory( const WindowBlindProperties& blind ) :
      CWCEShadeMaterialFactory() {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Reads optical data from blind object that has been read from IDF
      m_Solar[ make_pair( Side::Front, Property::T ) ] = blind.SlatTransSolDiffDiff;
      m_Solar[ make_pair( Side::Back, Property::T ) ] = blind.SlatTransSolDiffDiff;
      m_Solar[ make_pair( Side::Front, Property::R ) ] = blind.SlatFrontReflSolDiffDiff;
      m_Solar[ make_pair( Side::Back, Property::R ) ] = blind.SlatBackReflSolDiffDiff;

      m_Visible[ make_pair( Side::Front, Property::T ) ] = blind.SlatTransVisDiffDiff;
      m_Visible[ make_pair( Side::Back, Property::T ) ] = blind.SlatTransVisDiffDiff;
      m_Visible[ make_pair( Side::Front, Property::R ) ] = blind.SlatFrontReflVisDiffDiff;
      m_Visible[ make_pair( Side::Back, Property::R ) ] = blind.SlatBackReflVisDiffDiff;
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEVenetianBlindRangeIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    CWCEVenetianBlindRangeIntegrator::CWCEVenetianBlindRangeIntegrator( const MaterialProperties &material,
      const WavelengthRange t_Range ) : CWCEShadeRangeIntegrator() {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Constructor for venetian blind properties over single spectrum range
      int blindDataPtr = material.BlindDataPtr;
      auto& blind( Blind( blindDataPtr ) );
      assert( blindDataPtr > 0 );
      CWavelengthRangeFactory aWVFactory = CWavelengthRangeFactory();
      CWavelengthRange aRange = *aWVFactory.getWavelengthRange( t_Range );
      double lowLambda = aRange.minLambda();
      double highLambda = aRange.maxLambda();
      
      CWCEVenetianBlindMaterialsFactory aMatBld = CWCEVenetianBlindMaterialsFactory( blind );

      double Tf = aMatBld.getProperty( t_Range, Side::Front, Property::T );
      double Tb = aMatBld.getProperty( t_Range, Side::Back, Property::T );
      double Rf = aMatBld.getProperty( t_Range, Side::Front, Property::R );
      double Rb = aMatBld.getProperty( t_Range, Side::Back, Property::R );

      shared_ptr< CMaterialBand > aMaterial =
        make_shared< CMaterialSingleBand >( Tf, Tb, Rf, Rb, lowLambda, highLambda );

      double slatWidth = blind.SlatWidth;
      double slatSpacing = blind.SlatSeparation;
      double slatTiltAngle = blind.SlatAngle;
      double curvatureRadius = 0; // No curvature radius in current IDF definition
      size_t numOfSlatSegments = 5; // Number of segments to use in venetian calculations
      shared_ptr< CCellDescription > aCellDescription =
        make_shared< CVenetianCellDescription >( slatWidth, slatSpacing, slatTiltAngle,
          curvatureRadius, numOfSlatSegments );

      shared_ptr< CBSDFHemisphere > aBSDF = make_shared< CBSDFHemisphere >( BSDFBasis::Full );

      CBSDFLayerMaker aMaker = CBSDFLayerMaker( aMaterial, aBSDF, aCellDescription );
      m_Shade = aMaker.getLayer();

    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEVenetianBlindIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    CWCEVenetianBlindIntegrator::CWCEVenetianBlindIntegrator( const MaterialProperties &material ) :
      CWCEIntegrator() {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   September 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Intialization specific for venetian blind integrators
      m_Integrator[ WavelengthRange::Solar ] =
        make_shared< CWCEVenetianBlindRangeIntegrator >( material, WavelengthRange::Solar );
      m_Integrator[ WavelengthRange::Visible ] =
        make_shared< CWCEVenetianBlindRangeIntegrator >( material, WavelengthRange::Visible );
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEScreenMaterialsFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEScreenMaterialsFactory::CWCEScreenMaterialsFactory( const MaterialProperties& material ) :
      CWCEShadeMaterialFactory() {
      // Current EnergyPlus model does not support material transmittance different from zero.
      // To enable that, it would be necessary to change input in IDF
      m_Solar[ make_pair( Side::Front, Property::T ) ] = 0;
      m_Solar[ make_pair( Side::Back, Property::T ) ] = 0;
      m_Solar[ make_pair( Side::Front, Property::R ) ] = material.ReflectShade;
      m_Solar[ make_pair( Side::Back, Property::R ) ] = material.ReflectShade;

      m_Visible[ make_pair( Side::Front, Property::T ) ] = 0;
      m_Visible[ make_pair( Side::Back, Property::T ) ] = 0;
      m_Visible[ make_pair( Side::Front, Property::R ) ] = material.ReflectShadeVis;
      m_Visible[ make_pair( Side::Back, Property::R ) ] = material.ReflectShadeVis;
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEScreenRangeIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    CWCEScreenRangeIntegrator::CWCEScreenRangeIntegrator( const MaterialProperties &material,
      const WavelengthRange t_Range ) {

      CWavelengthRangeFactory aWVFactory = CWavelengthRangeFactory();
      CWavelengthRange aRange = *aWVFactory.getWavelengthRange( t_Range );
      double lowLambda = aRange.minLambda();
      double highLambda = aRange.maxLambda();

      CWCEScreenMaterialsFactory aMatBld = CWCEScreenMaterialsFactory( material );

      double Tf = aMatBld.getProperty( t_Range, Side::Front, Property::T );
      double Tb = aMatBld.getProperty( t_Range, Side::Back, Property::T );
      double Rf = aMatBld.getProperty( t_Range, Side::Front, Property::R );
      double Rb = aMatBld.getProperty( t_Range, Side::Back, Property::R );

      shared_ptr< CMaterialBand > aMaterial =
        make_shared< CMaterialSingleBand >( Tf, Tb, Rf, Rb, lowLambda, highLambda );

      double diameter = material.Thickness; // Thickness in this case is diameter
      //double spacing = diameter / material.ScreenDiameterToSpacingRatio;
      // Program did not keep spacing from input format but rather calculated transmittance
      // as material.Trans = pow_2( 1.0 - diameter / spacing );
      double ratio = 1.0 - std::sqrt( material.Trans );
      double spacing = diameter / ratio;
      shared_ptr< CCellDescription > aCellDescription =
        make_shared< CWovenCellDescription >( diameter, spacing );

      shared_ptr< CBSDFHemisphere > aBSDF = make_shared< CBSDFHemisphere >( BSDFBasis::Full );

      CBSDFLayerMaker aMaker = CBSDFLayerMaker( aMaterial, aBSDF, aCellDescription );
      m_Shade = aMaker.getLayer();
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEScreenIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    CWCEScreenIntegrator::CWCEScreenIntegrator( const MaterialProperties &material ) {
      m_Integrator[ WavelengthRange::Solar ] =
        make_shared< CWCEScreenRangeIntegrator >( material, WavelengthRange::Solar );
      m_Integrator[ WavelengthRange::Visible ] =
        make_shared< CWCEScreenRangeIntegrator >( material, WavelengthRange::Visible );
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEDiffuseShadeMaterialsFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEDiffuseShadeMaterialsFactory::CWCEDiffuseShadeMaterialsFactory( const MaterialProperties& material ) {
      m_Solar[ make_pair( Side::Front, Property::T ) ] = material.Trans;
      m_Solar[ make_pair( Side::Back, Property::T ) ] = material.Trans;
      m_Solar[ make_pair( Side::Front, Property::R ) ] = material.ReflectShade;
      m_Solar[ make_pair( Side::Back, Property::R ) ] = material.ReflectShade;

      m_Visible[ make_pair( Side::Front, Property::T ) ] = material.TransVis;
      m_Visible[ make_pair( Side::Back, Property::T ) ] = material.TransVis;
      m_Visible[ make_pair( Side::Front, Property::R ) ] = material.ReflectShadeVis;
      m_Visible[ make_pair( Side::Back, Property::R ) ] = material.ReflectShadeVis;
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEDiffuseShadeRangeIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    CWCEDiffuseShadeRangeIntegrator::CWCEDiffuseShadeRangeIntegrator( const MaterialProperties &material, 
      const WavelengthRange t_Range ) : CWCERangeIntegrator() {
      storeProperties( t_Range, material );
    }

    double CWCEDiffuseShadeRangeIntegrator::getProperty( const Side t_Side, const Property t_Property ) const {
      return m_Results.at( make_pair( t_Side, t_Property ) );
    }

    void CWCEDiffuseShadeRangeIntegrator::storeProperties( const WavelengthRange t_Range,
      const MaterialProperties &material ) {
      CWCEDiffuseShadeMaterialsFactory aFactory = CWCEDiffuseShadeMaterialsFactory( material );
      m_Results[ make_pair( Side::Front, Property::T ) ] = aFactory.getProperty( t_Range, Side::Front, Property::T );
      m_Results[ make_pair( Side::Back, Property::T ) ] = aFactory.getProperty( t_Range, Side::Back, Property::T );
      m_Results[ make_pair( Side::Front, Property::R ) ] = aFactory.getProperty( t_Range, Side::Front, Property::R );
      m_Results[ make_pair( Side::Back, Property::R ) ] = aFactory.getProperty( t_Range, Side::Back, Property::R );
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEDiffuseShadeIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    CWCEDiffuseShadeIntegrator::CWCEDiffuseShadeIntegrator( const MaterialProperties &material ) {
      m_Integrator[ WavelengthRange::Solar ] =
        make_shared< CWCEDiffuseShadeRangeIntegrator >( material, WavelengthRange::Solar );
      m_Integrator[ WavelengthRange::Visible ] =
        make_shared< CWCEDiffuseShadeRangeIntegrator >( material, WavelengthRange::Visible );
    }

  }
}