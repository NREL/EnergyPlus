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

#include <WindowManager.hh>
#include <DataHeatBalance.hh>
#include <WindowComplexManager.hh>
#include <DataBSDFWindow.hh>
#include <CurveManager.hh>

#include "WindowManagerExteriorData.hh"
#include "OpticalLayer.hpp"
#include "FenestrationCommon.hpp"
#include "CommonWavelengths.hpp"
#include "BSDFLayer.hpp"
#include "Series.hpp"
#include "EquivalentBSDFLayer.hpp"
#include "MultiBSDFLayer.hpp"
#include "FenestrationCommon.hpp"
#include "MeasuredSampleData.hpp"
#include "BSDFDirections.hpp"
#include "MaterialDescription.hpp"

namespace EnergyPlus {

  using namespace std;

  using namespace DataSurfaces;
  using namespace DataHeatBalance;
  using namespace DataGlobals;
  using namespace WindowComplexManager;
  using namespace CurveManager;

  using namespace SingleLayerOptics;
  using namespace FenestrationCommon;
  using namespace SpectralAveraging;
  using namespace MultiLayerOptics;

  namespace WindowManager {

    bool isSurfaceHit( const int t_SurfNum, const Vector& t_Ray ) {
      double DotProd = dot( t_Ray, Surface( t_SurfNum ).NewellSurfaceNormalVector );
      return ( DotProd > 0 );
    }

    pair< double, double > getBSDFCoordinates( const int t_SurfNum, const Vector& t_Ray,
      const BSDFHemisphere t_Direction ) {
      double Theta = 0;
      double Phi = 0;
      
      //get window tilt and azimuth
      double Gamma = DegToRadians * Surface( t_SurfNum ).Tilt;
      double Alpha = DegToRadians * Surface( t_SurfNum ).Azimuth;

      int RadType = Front_Incident;

      if( t_Direction == BSDFHemisphere::Outgoing ) {
        RadType = Back_Incident;
      }

      //get the corresponding local Theta, Phi for ray
      W6CoordsFromWorldVect( t_Ray, RadType, Gamma, Alpha, Theta, Phi );

      Theta = 180 / Pi * Theta;
      Phi = 180 / Pi * Phi;

      return make_pair( Theta, Phi );

    }

    pair< double, double > getSunBSDFCoordinates( const int t_SurfNum, const BSDFHemisphere t_Direction ) {
      std::pair< double, double > Angles;
      return getBSDFCoordinates( t_SurfNum, DataBSDFWindow::SUNCOSTS( TimeStep, HourOfDay, { 1, 3 } ), 
        t_Direction );
    }

	  ///////////////////////////////////////////////////////////////////////////////
	  //       CWCESpecturmProperties
	  ///////////////////////////////////////////////////////////////////////////////
	  shared_ptr< CSeries > CWCESpecturmProperties::getDefaultSolarRadiationSpectrum() {
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
	  shared_ptr< CSeries > CWCESpecturmProperties::getDefaultVisiblePhotopicResponse() {
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
		  // Reads spectral data value
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
    shared_ptr< CSpectralSampleData > CWCESpecturmProperties::getSpectralSample(
      const MaterialProperties& t_MaterialProperties ) {
      double Tsol = t_MaterialProperties.Trans;
      double Rfsol = t_MaterialProperties.ReflectSolBeamFront;
      double Rbsol = t_MaterialProperties.ReflectSolBeamBack;
      shared_ptr< CMaterial > aSolMat = 
        make_shared< CMaterialSingleBand >( Tsol, Tsol, Rfsol, Rbsol, 0.3, 2.5 );

      double Tvis = t_MaterialProperties.TransVis;
      double Rfvis = t_MaterialProperties.ReflectVisBeamFront;
      double Rbvis = t_MaterialProperties.ReflectVisBeamBack;
      shared_ptr< CMaterial > aVisMat =
        make_shared< CMaterialSingleBand >( Tvis, Tvis, Rfvis, Rbvis, 0.38, 0.78 );

      CMaterialDualBand aMat = CMaterialDualBand( aVisMat, aSolMat, 0.49 );
      vector< double > aWl = *aMat.getBandWavelengths();
      vector< double > aTf = *aMat.getBandProperties( Property::T, Side::Front );
      vector< double > aRf = *aMat.getBandProperties( Property::R, Side::Front );
      vector< double > aRb = *aMat.getBandProperties( Property::R, Side::Back );
      shared_ptr< CSpectralSampleData > aSampleData = make_shared< CSpectralSampleData >();
      for( size_t i = 0; i < aWl.size(); ++i ) {
        aSampleData->addRecord( aWl[ i ], aTf[ i ], aRf[ i ], aRb[ i ] );
      }

	  return aSampleData;
    }

	///////////////////////////////////////////////////////////////////////////////
	shared_ptr< CSpectralSampleData > CWCESpecturmProperties::getSpectralSample( const int i, const int t_TransTablePtr, const int t_FRefleTablePtr, const int t_BRefleTablePtr ) {
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   Feb. 2017
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reads spectral data value
		assert( t_TransTablePtr ); // It must be true for spectral and angle data
		assert( t_FRefleTablePtr ); // It must be true for spectral and angle data
		assert( t_BRefleTablePtr ); // It must be true for spectral and angle data
		shared_ptr< CSpectralSampleData > aSampleData = make_shared< CSpectralSampleData >( );

		auto TransTableData = TableData( PerfCurve( t_TransTablePtr ).TableIndex );
		auto FReleTableData = TableData( PerfCurve( t_FRefleTablePtr ).TableIndex );
		auto BReleTableData = TableData( PerfCurve( t_BRefleTablePtr ).TableIndex );
		int numOfIn = TableLookup( PerfCurve( t_TransTablePtr ).TableIndex ).NumX1Vars;
		int numOfWl = TableLookup( PerfCurve( t_TransTablePtr ).TableIndex ).NumX2Vars;
		int num;

		for (auto j = 1; j <= numOfWl; ++j) {
			num =  i * numOfIn + j;
			double wl = TransTableData.X2( num );
			double T = TransTableData.Y( num );
			double Rf = FReleTableData.Y( num );
			double Rb = BReleTableData.Y( num );
			aSampleData->addRecord( wl, T, Rf, Rb );
		}
		return aSampleData; 
	}

    ///////////////////////////////////////////////////////////////////////////////
    //       CWindowConstructionsBSDF
    ///////////////////////////////////////////////////////////////////////////////
    CWindowConstructionsBSDF& CWindowConstructionsBSDF::instance() {
      static CWindowConstructionsBSDF p_inst;
      return p_inst;
    }

    CWindowConstructionsBSDF::CWindowConstructionsBSDF() {
      m_Layers[ WavelengthRange::Solar ] = make_shared< Layers_Map >();
      m_Layers[ WavelengthRange::Visible ] = make_shared< Layers_Map >();
    }

    shared_ptr< vector< double > > CWindowConstructionsBSDF::getCommonWavelengths( const WavelengthRange t_Range,
      const int t_ConstrNum ) const {
      shared_ptr< IGU_Layers > iguLayers = getLayers( t_Range, t_ConstrNum );
      CCommonWavelengths aCommonWL;
      for( auto layer : *iguLayers ) {
        aCommonWL.addWavelength( layer->getBandWavelengths() );
      }
      
      return aCommonWL.getCombinedWavelengths( Combine::Interpolate );
    }

    shared_ptr< IGU_Layers > CWindowConstructionsBSDF::getLayers( const WavelengthRange t_Range,
      const int t_ConstrNum ) const {
      Layers_Map aMap = *m_Layers.at( t_Range );
      auto it = aMap.find( t_ConstrNum );
      if( it == aMap.end() ) {
        throw runtime_error( "Incorrect construction selection." );
      }
      return aMap.at( t_ConstrNum );
    }

    void CWindowConstructionsBSDF::pushBSDFLayer( const WavelengthRange t_Range, const int t_ConstrNum, 
      const shared_ptr< CBSDFLayer >& t_Layer ) {
      shared_ptr< Layers_Map > aMap = m_Layers.at( t_Range );
      auto it = aMap->find( t_ConstrNum );
      shared_ptr< IGU_Layers > iguLayers = nullptr;
      if( it != aMap->end() ) {
        iguLayers = it->second;
      } else {
        iguLayers = make_shared< IGU_Layers >();
        ( *aMap )[ t_ConstrNum ] = iguLayers;
      }

      iguLayers->push_back( t_Layer );
    }

    shared_ptr< CMultiBSDFLayer > CWindowConstructionsBSDF::getEquivalentLayer(
      const WavelengthRange t_Range, const int t_ConstrNum ) {
      auto it = m_Equivalent.find( make_pair( t_Range, t_ConstrNum ) );
      if( it == m_Equivalent.end() ) {
        // Layer was not requested before. Need to create it now.
        shared_ptr< vector< double > > commonWl = getCommonWavelengths( t_Range, t_ConstrNum );
        shared_ptr< CSeries > aSolarSpectrum = CWCESpecturmProperties::getDefaultSolarRadiationSpectrum();
        IGU_Layers iguLayers = *getLayers( t_Range, t_ConstrNum );
        size_t i = iguLayers.size();
        shared_ptr< CEquivalentBSDFLayer > aEqLayer = make_shared< CEquivalentBSDFLayer >( commonWl, iguLayers[ 0 ] );        
        for( auto i = 1; i < iguLayers.size(); ++i ) {
          aEqLayer->addLayer( iguLayers[ i ] );
        }
        shared_ptr< CMultiBSDFLayer > aMultiLayer =
          make_shared< CMultiBSDFLayer >( aEqLayer, aSolarSpectrum );
        m_Equivalent[ make_pair( t_Range, t_ConstrNum ) ] = aMultiLayer;
      }
    
      return m_Equivalent.at( make_pair( t_Range, t_ConstrNum ) );
    }

  }

}