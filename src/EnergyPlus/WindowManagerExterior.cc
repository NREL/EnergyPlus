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

using namespace std;
using namespace Tarcog;
using namespace FenestrationCommon;


namespace EnergyPlus {

  using namespace DataEnvironment;
  using namespace DataSurfaces;
  using namespace DataHeatBalance;
  using namespace DataHeatBalFanSys;
  using namespace DataGlobals;

  namespace WindowManager {

    void CalcWindowHeatBalanceExternalRoutines(
        int const SurfNum, // Surface number
        Real64 const HextConvCoeff, // Outside air film conductance coefficient
        Real64 & SurfInsideTemp, // Inside window surface temperature
        Real64 & SurfOutsideTemp // Outside surface temperature (C)
      ) {
      auto & window( SurfaceWindow( SurfNum ) );
      auto & surface( Surface( SurfNum ) );
      int ConstrNum = surface.Construction;
      auto & construction( Construct( ConstrNum ) );

      // Create indoor environment
      tin = surface.getInsideAirTemperature( SurfNum ) + KelvinConv;
      double pressure = OutBaroPress; // Pascals
      double hcin = HConvIn( SurfNum );

      Rmir = surface.getInsideIR( SurfNum );

      shared_ptr< CTarEnvironment > Indoor = make_shared< CTarIndoorEnvironment >( tin, pressure );
      Indoor->setHCoeffModel( BoundaryConditionsCoeffModel::CalculateH, hcin );
      Indoor->setEnvironmentIR( Rmir );

      // Create outdoor environment
      tout = surface.getOutsideAirTemperature( SurfNum ) + KelvinConv;
      Outir = surface.getOutsideIR( SurfNum );
      double dirSolRad = QRadSWOutIncident( SurfNum ) + QS( Surface( SurfNum ).Zone );
      double tSky = SkyTempKelvin;
      double airSpeed = 0;
      if( surface.ExtWind ) {
        airSpeed = surface.WindSpeed;
      }
      double fclr = 1 - CloudFraction;
      AirHorizontalDirection airDirection = AirHorizontalDirection::Windward;
      shared_ptr< CTarEnvironment > Outdoor = make_shared< CTarOutdoorEnvironment >( tout, pressure, 
        airSpeed, dirSolRad, airDirection, tSky, SkyModel::AllSpecified, fclr );
      Outdoor->setHCoeffModel( BoundaryConditionsCoeffModel::HcPrescribed, HextConvCoeff );
      Outdoor->setEnvironmentIR( Outir );
      
      // Create IGU
      int tilt = surface.Tilt;
      double height = surface.Height;
      double width = surface.Width;

      shared_ptr< CTarIGU > aIGU = make_shared< CTarIGU >( surface.Width, surface.Height, surface.Tilt );

      // pick-up all layers and put them in IGU
      int TotLay = construction.TotLayers;
      shared_ptr< CTarIGUSolidLayer > aSolidLayer = nullptr;
      for( int i = 0; i < TotLay; ++i ) {
        int LayPtr = construction.LayerPoint( i + 1 );
        auto & material( Material( LayPtr ) );
        shared_ptr< CTarSurface > frontSurface = make_shared< CTarSurface >( material.AbsorpThermalFront, material.TransThermal );
        shared_ptr< CTarSurface > backSurface = make_shared< CTarSurface >( material.AbsorpThermalBack, material.TransThermal );
        aSolidLayer = make_shared< CTarIGUSolidLayer >( material.Thickness, material.Conductivity, frontSurface, backSurface );
        if( dirSolRad > 0 ) {
          double absCoeff = QRadSWwinAbs( i + 1, SurfNum ) / dirSolRad;
          aSolidLayer->setSolarAbsorptance( absCoeff );
        }
        aIGU->addLayer( aSolidLayer );
      }

      shared_ptr< CTarcogSystem > aSystem = make_shared< CTarcogSystem >( aIGU, Indoor, Outdoor );
      aSystem->solve();
      vector< shared_ptr < CBaseIGUTarcogLayer > > aLayers = aSystem->getLayers();
      int i = 1;
      for( shared_ptr< CBaseIGUTarcogLayer > aLayer : aLayers ) {
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

  }
}