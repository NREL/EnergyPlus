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

#include< memory >

// EnergyPlus headers
#include "DataEnvironment.hh"
#include "DataSurfaces.hh"
#include "DataHeatBalance.hh"
#include "DataHeatBalFanSys.hh"
#include "DataGlobals.hh"


// Windows library headers
#include "WindowManagerExterior.hh"
#include "WindowManager.hh"
#include "TarIGU.hpp"
#include "TarIGUSolidLayer.hpp"
#include "TarOutdoorEnvironment.hpp"
#include "TarIndoorEnvironment.hpp"
#include "LayerInterfaces.hpp"
#include "TarSurface.hpp"
#include "TarcogSystem.hpp"
#include "FenestrationCommon.hpp"
#include "GasProperties.hpp"
#include "GasData.hpp"
#include "GasItem.hpp"

using namespace std;
using namespace Tarcog;
using namespace Gases;
using namespace FenestrationCommon;


namespace EnergyPlus {

  using namespace DataEnvironment;
  using namespace DataSurfaces;
  using namespace DataHeatBalance;
  using namespace DataHeatBalFanSys;
  using namespace DataGlobals;

  namespace WindowManager {

    /////////////////////////////////////////////////////////////////////////////////////////
    void CalcWindowHeatBalanceExternalRoutines(
        int const SurfNum, // Surface number
        Real64 const HextConvCoeff, // Outside air film conductance coefficient
        Real64 & SurfInsideTemp, // Inside window surface temperature
        Real64 & SurfOutsideTemp // Outside surface temperature (C)
      ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   July 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Main wrapper routine to pick-up data from EnergyPlus and then call Windows-CalcEngine routines
      // to obtain results

      auto & window( SurfaceWindow( SurfNum ) );
      auto & surface( Surface( SurfNum ) );
      int ConstrNum = surface.Construction;
      auto & construction( Construct( ConstrNum ) );

      CWCELayerFactory aFactory = CWCELayerFactory();
      shared_ptr< CTarEnvironment > Indoor = aFactory.getIndoor( surface, SurfNum );
      shared_ptr< CTarEnvironment > Outdoor = aFactory.getOutdoor( surface, SurfNum, HextConvCoeff );
      shared_ptr< CTarIGU > aIGU = aFactory.getIGU( surface );

      // pick-up all layers and put them in IGU (this includes gap layers as well)
      int TotLay = construction.TotLayers;
      int SolidLayerIndex = 0;
      for( int i = 0; i < TotLay; ++i ) {
        int LayPtr = construction.LayerPoint( i + 1 );
        auto & material( Material( LayPtr ) );
        shared_ptr< CBaseIGUTarcogLayer > aLayer = nullptr;
        if( material.Group == WindowGlass || material.Group == WindowSimpleGlazing ) {
          ++SolidLayerIndex;
          aLayer = aFactory.getSolidLayer( surface, material, SolidLayerIndex, SurfNum );
        } else if( material.Group == WindowGas || material.Group == WindowGasMixture ) {
          aLayer = aFactory.getGapLayer( material );
        } else if( material.Group == ComplexWindowGap ) {
          aLayer = aFactory.getComplexGapLayer( material );
        }
        aIGU->addLayer( aLayer );
      }

      shared_ptr< CTarcogSystem > aSystem = make_shared< CTarcogSystem >( aIGU, Indoor, Outdoor );
      aSystem->setTolerance( 0.02 );

      // get previous timestep temperatures solution for faster iterations
      shared_ptr< vector< double > > Guess = make_shared< vector< double > >();
      for( int k = 1; k <= 2 * construction.TotSolidLayers; ++k ) {
        Guess->push_back( SurfaceWindow( SurfNum ).ThetaFace( k ) );
      }
      aSystem->setInitialGuess( Guess );
      aSystem->solve();
      
      vector< shared_ptr < CTarIGUSolidLayer > > aLayers = aSystem->getSolidLayers();
      int i = 1;
      for( shared_ptr< CTarIGUSolidLayer > aLayer : aLayers ) {
        double aTemp = 0;
        for( Side aSide : EnumSide() ) {
          aTemp = aLayer->getTemperature( aSide );
          thetas( i ) = aTemp;
          if( i == 1 ) {
            SurfOutsideTemp = aTemp - KelvinConv;
          }
          ++i;
        }
        SurfInsideTemp = aTemp - KelvinConv;
      }
      
      HConvIn( SurfNum ) = Indoor->getHc();
      double heatFlow = Indoor->getHeatFlow() * surface.Area;
      WinHeatGain( SurfNum ) = WinTransSolar( SurfNum ) - heatFlow;
      WinHeatTransfer( SurfNum ) = WinHeatGain( SurfNum );
      double TransDiff = Construct( ConstrNum ).TransDiff;
      WinHeatGain( SurfNum ) -= QS( Surface( SurfNum ).Zone ) * Surface( SurfNum ).Area * TransDiff;
      WinHeatTransfer( SurfNum ) -= QS( Surface( SurfNum ).Zone ) * Surface( SurfNum ).Area * TransDiff;
      for( int k = 1; k <= TotLay; ++k ) {
        SurfaceWindow( SurfNum ).ThetaFace( 2 * k - 1 ) = thetas( 2 * k - 1 );
        SurfaceWindow( SurfNum ).ThetaFace( 2 * k ) = thetas( 2 * k );

        // temperatures for reporting
        FenLaySurfTempFront( k, SurfNum ) = thetas( 2 * k - 1 ) - KelvinConv;
        FenLaySurfTempBack( k, SurfNum ) = thetas( 2 * k ) - KelvinConv;
      }
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    shared_ptr< CTarIGUSolidLayer > CWCELayerFactory::getSolidLayer( const SurfaceData &surface, const MaterialProperties &material,
      const int t_Index, const int t_SurfNum ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   July 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Creates solid layer object from material properties in EnergyPlus
      shared_ptr< CTarSurface > frontSurface = make_shared< CTarSurface >( material.AbsorpThermalFront, material.TransThermal );
      shared_ptr< CTarSurface > backSurface = make_shared< CTarSurface >( material.AbsorpThermalBack, material.TransThermal );
      shared_ptr< CTarIGUSolidLayer > aSolidLayer =
        make_shared< CTarIGUSolidLayer >( material.Thickness, material.Conductivity, frontSurface, backSurface );
      // double dirSolRad = QRadSWOutIncident( t_SurfNum ) + QS( Surface( t_SurfNum ).Zone );
      double swRadiation = surface.getSWIncident( t_SurfNum );
      if( swRadiation > 0 ) {
        double absCoeff = QRadSWwinAbs( t_Index, t_SurfNum ) / swRadiation;
        aSolidLayer->setSolarAbsorptance( absCoeff );
      }
      return aSolidLayer;
    }

    shared_ptr< CTarIGUGapLayer > CWCELayerFactory::getGapLayer( const MaterialProperties &material ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   July 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Creates gap layer object from material properties in EnergyPlus
      const double pres = 1e5; // Old code uses this constant pressure
      double thickness = material.Thickness;
      shared_ptr< CGas > aGas = getGas( material );
      shared_ptr< CTarIGUGapLayer > aLayer = make_shared< CTarIGUGapLayer >( thickness, pres, aGas );
      return aLayer;
    }

    shared_ptr< CTarIGUGapLayer > CWCELayerFactory::getComplexGapLayer( const MaterialProperties &material ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   July 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Creates gap layer object from material properties in EnergyPlus
      const double pres = 1e5; // Old code uses this constant pressure
      double thickness = material.Thickness;
      int gasPointer = material.GasPointer;
      auto & gasMaterial( Material( gasPointer ) );
      shared_ptr< CGas > aGas = getGas( gasMaterial );
      shared_ptr< CTarIGUGapLayer > aLayer = make_shared< CTarIGUGapLayer >( thickness, pres, aGas );
      return aLayer;
    }

    shared_ptr< CGas > CWCELayerFactory::getGas( const MaterialProperties &material ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   July 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Creates gap layer object from material properties in EnergyPlus
      int numGases = material.NumberOfGasesInMixture;
      const double vacuumCoeff = 1.4; //Load vacuum coefficient once it is implemented (Simon).
      string gasName = material.Name;
      shared_ptr< CGas > aGas = make_shared< CGas >();
      for( int i = 1; i <= numGases; ++i ) {
        double wght = material.GasWght( i );
        double fract = material.GasFract( i );
        vector< double > gcon;
        vector< double > gvis;
        vector< double > gcp;
        for( int j = 1; j <= 3; ++j ) {
          gcon.push_back( material.GasCon( j, i ) );
          gvis.push_back( material.GasCon( j, i ) );
          gcp.push_back( material.GasCon( j, i ) );
        }
        shared_ptr< CIntCoeff > aCon = make_shared< CIntCoeff >( gcon[ 0 ], gcon[ 1 ], gcon[ 2 ] );
        shared_ptr< CIntCoeff > aCp = make_shared< CIntCoeff >( gcp[ 0 ], gcp[ 1 ], gcp[ 2 ] );
        shared_ptr< CIntCoeff > aVis = make_shared< CIntCoeff >( gvis[ 0 ], gvis[ 1 ], gvis[ 2 ] );
        shared_ptr< CGasData > aData = make_shared< CGasData >( gasName, wght, vacuumCoeff, aCp, aCon, aVis );
        shared_ptr< CGasItem > aGasItem = make_shared< CGasItem >( fract, aData );
        aGas->addGasItem( aGasItem );
      }
      return aGas;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    shared_ptr< CTarEnvironment > CWCELayerFactory::getIndoor( const SurfaceData &surface, const int t_SurfNum ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   July 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Creates indoor environment object from surface properties in EnergyPlus
      double tin = surface.getInsideAirTemperature( t_SurfNum ) + KelvinConv;
      double hcin = HConvIn( t_SurfNum );

      double IR = surface.getInsideIR( t_SurfNum );

      shared_ptr< CTarEnvironment > Indoor = make_shared< CTarIndoorEnvironment >( tin, OutBaroPress );
      Indoor->setHCoeffModel( BoundaryConditionsCoeffModel::CalculateH, hcin );
      Indoor->setEnvironmentIR( IR );
      return Indoor;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    shared_ptr< CTarEnvironment > CWCELayerFactory::getOutdoor( const SurfaceData &surface, const int t_SurfNum, const double t_Hext ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   July 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Creates outdoor environment object from surface properties in EnergyPlus
      double tout = surface.getOutsideAirTemperature( t_SurfNum ) + KelvinConv;
      double IR = surface.getOutsideIR( t_SurfNum );
      // double dirSolRad = QRadSWOutIncident( t_SurfNum ) + QS( Surface( t_SurfNum ).Zone );
      double swRadiation = surface.getSWIncident( t_SurfNum );
      double tSky = SkyTempKelvin;
      double airSpeed = 0;
      if( surface.ExtWind ) {
        airSpeed = surface.WindSpeed;
      }
      double fclr = 1 - CloudFraction;
      AirHorizontalDirection airDirection = AirHorizontalDirection::Windward;
      shared_ptr< CTarEnvironment > Outdoor = make_shared< CTarOutdoorEnvironment >( tout, OutBaroPress,
        airSpeed, swRadiation, airDirection, tSky, SkyModel::AllSpecified, fclr );
      Outdoor->setHCoeffModel( BoundaryConditionsCoeffModel::HcPrescribed, t_Hext );
      Outdoor->setEnvironmentIR( IR );
      return Outdoor;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    shared_ptr< CTarIGU > CWCELayerFactory::getIGU( const SurfaceData &surface ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   July 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Creates IGU object from surface properties in EnergyPlus
      int tilt = surface.Tilt;
      double height = surface.Height;
      double width = surface.Width;

      shared_ptr< CTarIGU > aIGU = make_shared< CTarIGU >( surface.Width, surface.Height, surface.Tilt );
      return aIGU;
    }

  }
}