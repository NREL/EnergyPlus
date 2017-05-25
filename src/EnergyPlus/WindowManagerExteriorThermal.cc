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

// EnergyPlus headers
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <General.hh>
#include <WindowManager.hh>

// Windows library headers
#include <WCETarcog.hpp>

#include "WindowManagerExteriorThermal.hh"

namespace EnergyPlus {

  using namespace std;
  using namespace Tarcog;
  using namespace Gases;
  using namespace FenestrationCommon;

  using namespace DataEnvironment;
  using namespace DataSurfaces;
  using namespace DataHeatBalance;
  using namespace DataHeatBalFanSys;
  using namespace DataGlobals;
  using namespace General;

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

      const double solutionTolerance = 0.02;

      // Tarcog thermal system for solving heat transfer through the window
      CWCEHeatTransferFactory aFactory = CWCEHeatTransferFactory( surface, SurfNum );
      shared_ptr< CSingleSystem > aSystem = aFactory.getTarcogSystem( HextConvCoeff );
      aSystem->setTolerance( solutionTolerance );

      // get previous timestep temperatures solution for faster iterations
      shared_ptr< vector< double > > Guess = make_shared< vector< double > >();
      int totSolidLayers = construction.TotSolidLayers;
      
      // Interior and exterior shading layers have gas between them and IGU but that gas
      // was not part of construction so it needs to be increased by one
      if( window.ShadingFlag == IntShadeOn || window.ShadingFlag == ExtShadeOn || 
        window.ShadingFlag == IntBlindOn || window.ShadingFlag == ExtBlindOn || 
        window.ShadingFlag == ExtScreenOn || window.ShadingFlag == BGShadeOn ||
        window.ShadingFlag == BGBlindOn ) {
        ++totSolidLayers;
      }

      for( int k = 1; k <= 2 * totSolidLayers; ++k ) {
        Guess->push_back( SurfaceWindow( SurfNum ).ThetaFace( k ) );
      }
      aSystem->setInitialGuess( Guess );
      aSystem->solve();
      
      vector< shared_ptr < CIGUSolidLayer > > aLayers = aSystem->getSolidLayers();
      int i = 1;
      for( shared_ptr< CIGUSolidLayer > aLayer : aLayers ) {
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
        if( window.ShadingFlag == IntShadeOn || window.ShadingFlag == IntBlindOn ) {
          double EffShBlEmiss = InterpSlatAng( window.SlatAngThisTS, window.MovableSlats, window.EffShBlindEmiss );
          double EffGlEmiss = InterpSlatAng( window.SlatAngThisTS, window.MovableSlats, window.EffGlassEmiss );
          window.EffInsSurfTemp = ( EffShBlEmiss * SurfInsideTemp + EffGlEmiss * ( thetas( 2 * totSolidLayers - 2 ) - TKelvin ) ) / ( EffShBlEmiss + EffGlEmiss );
        }
      }
      
      HConvIn( SurfNum ) = aSystem->getHc( Environment::Indoor );
      if( window.ShadingFlag == IntShadeOn || window.ShadingFlag == IntBlindOn || aFactory.isInteriorShade() ) {
        // It is not clear why EnergyPlus keeps this interior calculations separately for interior shade. This does create different
        // soltuion from heat transfer from tarcog itself. Need to confirm with LBNL team about this approach. Note that heat flow
        // through shade (consider case when openings are zero) is different from heat flow obtained by these equations. Will keep
        // these calculations just to confirm that current exterior engine is giving close results to what is in here. (Simon)
        size_t totLayers = aLayers.size();
        nglface = 2 * totLayers - 2;
        nglfacep = nglface + 2;
        shared_ptr< CIGUSolidLayer > aShadeLayer = aLayers[ totLayers - 1 ];
        shared_ptr< CIGUSolidLayer > aGlassLayer = aLayers[ totLayers - 2 ];
        double ShadeArea = Surface( SurfNum ).Area + SurfaceWindow( SurfNum ).DividerArea;
        shared_ptr< ISurface > frontSurface = aShadeLayer->getSurface( Side::Front );
        shared_ptr< ISurface > backSurface = aShadeLayer->getSurface( Side::Back );
        double EpsShIR1 = frontSurface->getEmissivity();
        double EpsShIR2 = backSurface->getEmissivity();
        double TauShIR = frontSurface->getTransmittance();
        double RhoShIR1 = max( 0.0, 1.0 - TauShIR - EpsShIR1 );
        double RhoShIR2 = max( 0.0, 1.0 - TauShIR - EpsShIR2 );
        // double RhoGlIR2 = 1.0 - emis( 2 * ngllayer );
        double glassEmiss = aGlassLayer->getSurface( Side::Back )->getEmissivity();
        double RhoGlIR2 = 1.0 - glassEmiss;
        double ShGlReflFacIR = 1.0 - RhoGlIR2 * RhoShIR1;
        double rmir = surface.getInsideIR( SurfNum );
        double NetIRHeatGainShade = ShadeArea * EpsShIR2 * ( sigma * pow( thetas( nglfacep ), 4 ) - rmir ) + 
          EpsShIR1 * ( sigma * pow( thetas( nglfacep - 1 ), 4 ) - rmir ) * RhoGlIR2 * TauShIR / ShGlReflFacIR;
        double NetIRHeatGainGlass = ShadeArea * ( glassEmiss * TauShIR / ShGlReflFacIR ) * 
          ( sigma * pow( thetas( nglface ), 4 ) - rmir );
        double tind = surface.getInsideAirTemperature( SurfNum ) + KelvinConv;
        double ConvHeatGainFrZoneSideOfShade = ShadeArea * HConvIn( SurfNum ) * ( thetas( nglfacep ) - tind );
        WinHeatGain( SurfNum ) = WinTransSolar( SurfNum ) + ConvHeatGainFrZoneSideOfShade + 
          NetIRHeatGainGlass + NetIRHeatGainShade;
        WinHeatTransfer( SurfNum ) = WinHeatGain( SurfNum );

        // Effective shade and glass emissivities that are used later for energy calculations.
        // This needs to be checked as well. (Simon)
        double EffShBlEmiss = EpsShIR1 * ( 1.0 + RhoGlIR2 * TauShIR / ( 1.0 - RhoGlIR2 * RhoShIR2 ) );
        SurfaceWindow( SurfNum ).EffShBlindEmiss = EffShBlEmiss;

        double EffGlEmiss = glassEmiss * TauShIR / ( 1.0 - RhoGlIR2 * RhoShIR2 );
        SurfaceWindow( SurfNum ).EffGlassEmiss = EffGlEmiss;

        double glassTemperature = aGlassLayer->getSurface( Side::Back )->getTemperature();
        //double EffShBlEmiss = EpsShIR2 * ( 1.0 + RhoGlIR2 * TauShIR / ( 1.0 - RhoGlIR2 * RhoShIR2 ) );
        SurfaceWindow( SurfNum ).EffInsSurfTemp = ( EffShBlEmiss * SurfInsideTemp + 
          EffGlEmiss * ( glassTemperature - KelvinConv ) ) / ( EffShBlEmiss + EffGlEmiss );


      } else {
        // Another adoptation to old source that looks suspicious. Check if heat flow through
        // window is actually matching these values. (Simon)
        // double heatFlow = aSystem->getHeatFlow() * surface.Area;
        // 
        size_t totLayers = aLayers.size();
        shared_ptr< CIGUSolidLayer > aGlassLayer = aLayers[ totLayers - 1 ];
        shared_ptr< ISurface > backSurface = aGlassLayer->getSurface( Side::Back );
        
        double h_cin = aSystem->getHc( Environment::Indoor );
        double ConvHeatGainFrZoneSideOfGlass = surface.Area * h_cin * 
          ( backSurface->getTemperature() - aSystem->getAirTemperature( Environment::Indoor ) );

        double rmir = surface.getInsideIR( SurfNum );
        double NetIRHeatGainGlass = surface.Area * backSurface->getEmissivity() *
          ( sigma * pow( backSurface->getTemperature(), 4 ) - rmir );

        WinHeatGain( SurfNum ) = WinTransSolar( SurfNum ) + ConvHeatGainFrZoneSideOfGlass + NetIRHeatGainGlass;

        WinHeatTransfer( SurfNum ) = WinHeatGain( SurfNum );
      }
      
      double TransDiff = construction.TransDiff;
      WinHeatGain( SurfNum ) -= QS( surface.Zone ) * surface.Area * TransDiff;
      WinHeatTransfer( SurfNum ) -= QS( surface.Zone ) * surface.Area * TransDiff;
      
      for( int k = 1; k <= surface.getTotLayers(); ++k ) {
        SurfaceWindow( SurfNum ).ThetaFace( 2 * k - 1 ) = thetas( 2 * k - 1 );
        SurfaceWindow( SurfNum ).ThetaFace( 2 * k ) = thetas( 2 * k );

        // temperatures for reporting
        FenLaySurfTempFront( k, SurfNum ) = thetas( 2 * k - 1 ) - KelvinConv;
        FenLaySurfTempBack( k, SurfNum ) = thetas( 2 * k ) - KelvinConv;
      }

    }

    /////////////////////////////////////////////////////////////////////////////////////////
    //  CWCEHeatTransferFactory
    /////////////////////////////////////////////////////////////////////////////////////////

    CWCEHeatTransferFactory::CWCEHeatTransferFactory( SurfaceData const & surface, int const t_SurfNum  ) : 
      m_Surface( surface ), m_SurfNum( t_SurfNum ), m_SolidLayerIndex( 0 ), m_InteriorBSDFShade( false ), 
      m_ExteriorShade( false ) {
      m_Window = SurfaceWindow( t_SurfNum );
      auto ShadeFlag = m_Window.ShadingFlag;

      m_ConstructionNumber = m_Surface.Construction;
      m_ShadePosition = ShadePosition::NoShade;

      if( ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == IntBlindOn ||
        ShadeFlag == ExtBlindOn || ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn ||
        ShadeFlag == ExtScreenOn ) {
        m_ConstructionNumber = m_Surface.ShadedConstruction;
        if( m_Window.StormWinFlag > 0 ) m_ConstructionNumber = m_Surface.StormWinShadedConstruction;
      }

      m_TotLay = getNumOfLayers();

      if( ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn ) {
        m_ShadePosition = ShadePosition::Interior;
      }

      if( ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn ) {
        m_ShadePosition = ShadePosition::Exterior;
      }

      if( ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn ) {
        m_ShadePosition = ShadePosition::Between;
      }

    }

    /////////////////////////////////////////////////////////////////////////////////////////
    shared_ptr< CSingleSystem > CWCEHeatTransferFactory::getTarcogSystem( double const t_HextConvCoeff ) {
      auto Indoor = getIndoor();
      auto Outdoor = getOutdoor( t_HextConvCoeff );
      auto aIGU = getIGU();

      // pick-up all layers and put them in IGU (this includes gap layers as well)
      for( auto i = 0; i < m_TotLay; ++i ) {
        auto aLayer = getIGULayer( i + 1 );
        assert( aLayer != nullptr );
        // IDF for "standard" windows do not insert gas between glass and shade. Tarcog needs that gas
        // and it will be created here
        if( m_ShadePosition == ShadePosition::Interior && i == m_TotLay - 1 ) {
          auto aAirLayer = getShadeToGlassLayer( i + 1 );
          aIGU->addLayer( aAirLayer );
        }
        aIGU->addLayer( aLayer );
        if( m_ShadePosition == ShadePosition::Exterior && i == 0 ) {
          auto aAirLayer = getShadeToGlassLayer( i + 1 );
          aIGU->addLayer( aAirLayer );
        }
      }

      auto aSystem = make_shared< CSingleSystem >( aIGU, Indoor, Outdoor );

      return aSystem;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    MaterialProperties* CWCEHeatTransferFactory::getLayerMaterial( int const t_Index ) const {
      auto ConstrNum = m_Surface.Construction;

      if( m_Window.ShadingFlag == IntShadeOn || m_Window.ShadingFlag == ExtShadeOn ||
        m_Window.ShadingFlag == IntBlindOn || m_Window.ShadingFlag == ExtBlindOn ||
        m_Window.ShadingFlag == BGShadeOn || m_Window.ShadingFlag == BGBlindOn ||
        m_Window.ShadingFlag == ExtScreenOn ) {
        ConstrNum = m_Surface.ShadedConstruction;
        if( m_Window.StormWinFlag > 0 ) ConstrNum = m_Surface.StormWinShadedConstruction;
      }

      auto & construction( Construct( ConstrNum ) );
      auto LayPtr = construction.LayerPoint( t_Index );
      return &Material( LayPtr );
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    shared_ptr< CBaseIGULayer > CWCEHeatTransferFactory::getIGULayer( int const t_Index ) {
      shared_ptr< CBaseIGULayer > aLayer = nullptr;

      auto material = getLayerMaterial( t_Index );

      auto matGroup = material->Group;

      if( matGroup == WindowGlass || matGroup == WindowSimpleGlazing ||
        matGroup == WindowBlind || matGroup == Shade || matGroup == Screen || 
        matGroup == ComplexWindowShade ) {
        ++m_SolidLayerIndex;
        aLayer = getSolidLayer( m_Surface, *material, m_SolidLayerIndex, m_SurfNum );
      } else if( matGroup == WindowGas || matGroup == WindowGasMixture ) {
        aLayer = getGapLayer( *material );
      } else if( matGroup == ComplexWindowGap ) {
        aLayer = getComplexGapLayer( *material );
      }

      return aLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    int CWCEHeatTransferFactory::getNumOfLayers() const {
      return Construct( m_ConstructionNumber ).TotLayers;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    shared_ptr< CBaseIGULayer > CWCEHeatTransferFactory::getSolidLayer( SurfaceData const & surface, 
      MaterialProperties const & material, int const t_Index, int const t_SurfNum ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   July 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Creates solid layer object from material properties in EnergyPlus
      double absFront = 0;
      double absBack = 0;
      double transThermalFront = 0;
      double transThermalBack = 0;
      double thickness = 0;
      double conductivity = 0;

      if( material.Group == WindowGlass || material.Group == WindowSimpleGlazing ) {
        absFront = material.AbsorpThermalFront;
        absBack = material.AbsorpThermalBack;
        transThermalFront = material.TransThermal;
        transThermalBack = material.TransThermal;
        thickness = material.Thickness;
        conductivity = material.Conductivity;
      }
      if( material.Group == WindowBlind ) {
        int blNum = m_Window.BlindNumber;
        auto blind = Blind( blNum );
        thickness = blind.SlatThickness;
        conductivity = blind.SlatConductivity;
        absFront = InterpSlatAng( m_Window.SlatAngThisTS, m_Window.MovableSlats, blind.IRFrontEmiss );
        absBack = InterpSlatAng( m_Window.SlatAngThisTS, m_Window.MovableSlats, blind.IRBackEmiss );
        transThermalFront = InterpSlatAng( m_Window.SlatAngThisTS, m_Window.MovableSlats, blind.IRFrontTrans );
        transThermalBack = InterpSlatAng( m_Window.SlatAngThisTS, m_Window.MovableSlats, blind.IRBackTrans );
        if( t_Index == 1 ) {
          m_ExteriorShade = true;
        }

      }
      if( material.Group == Shade ) {
        absFront = material.AbsorpThermal;
        absBack = material.AbsorpThermal;
        transThermalFront = material.TransThermal;
        transThermalBack = material.TransThermal;
        thickness = material.Thickness;
        conductivity = material.Conductivity;
        if( t_Index == 1 ) {
          m_ExteriorShade = true;
        }
      }
      if( material.Group == ComplexWindowShade ) {
        int shdPtr = material.ComplexShadePtr;
        auto &shade( ComplexShade( shdPtr ) );
        thickness = shade.Thickness;
        conductivity = shade.Conductivity;
        absFront = shade.FrontEmissivity;
        absBack = shade.BackEmissivity;
        transThermalFront = shade.IRTransmittance;
        transThermalBack = shade.IRTransmittance;
        m_InteriorBSDFShade = ( ( 2 * t_Index - 1 ) == m_TotLay );
      }

      shared_ptr< ISurface > frontSurface = make_shared< CSurface >( absFront, transThermalFront );
      shared_ptr< ISurface > backSurface = make_shared< CSurface >( absBack, transThermalBack );
      shared_ptr< CIGUSolidLayer > aSolidLayer =
        make_shared< CIGUSolidLayer >( thickness, conductivity, frontSurface, backSurface );
      double swRadiation = surface.getSWIncident( t_SurfNum );
      if( swRadiation > 0 ) {
        double absCoeff = 0;
        if( material.Group == WindowGlass || material.Group == WindowSimpleGlazing ||
          material.Group == ComplexWindowShade ) {
          int aIndex = t_Index;
          if( m_ExteriorShade ) {
            --aIndex;
          }
          absCoeff = QRadSWwinAbs( aIndex, t_SurfNum ) / swRadiation;          
        } else {
          absCoeff = ( m_Window.AbsFrontSide() + m_Window.AbsBackSide() ) / swRadiation;
        }
        if( ( 2 * t_Index - 1 ) == m_TotLay ) {
          absCoeff += QRadThermInAbs( t_SurfNum ) / swRadiation;
        }
        aSolidLayer->setSolarAbsorptance( absCoeff );
      }
      return aSolidLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    shared_ptr< CBaseIGULayer > CWCEHeatTransferFactory::getGapLayer( const MaterialProperties &material ) {
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
      shared_ptr< CBaseIGULayer > aLayer = make_shared< CIGUGapLayer >( thickness, pres, aGas );
      return aLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    shared_ptr< CBaseIGULayer > CWCEHeatTransferFactory::getShadeToGlassLayer( const int t_Index ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   August 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Creates gap layer object from material properties in EnergyPlus
      const double pres = 1e5; // Old code uses this constant pressure
      shared_ptr< CGas > aGas = getAir();
      double thickness = 0;
      
      if( m_Window.ShadingFlag == IntBlindOn || m_Window.ShadingFlag == ExtBlindOn ) {
        thickness = Blind( m_Window.BlindNumber ).BlindToGlassDist;
      }
      if( m_Window.ShadingFlag == IntShadeOn || m_Window.ShadingFlag == ExtShadeOn ) {
        MaterialProperties* material = getLayerMaterial( t_Index );
        thickness = material->WinShadeToGlassDist;
      }
      shared_ptr< CBaseIGULayer > aLayer = make_shared< CIGUGapLayer >( thickness, pres, aGas );
      return aLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    shared_ptr< CBaseIGULayer > CWCEHeatTransferFactory::getComplexGapLayer( const MaterialProperties &material ) {
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
      shared_ptr< CBaseIGULayer > aLayer = make_shared< CIGUGapLayer >( thickness, pres, aGas );
      return aLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    shared_ptr< CGas > CWCEHeatTransferFactory::getGas( const MaterialProperties &material ) {
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
          gvis.push_back( material.GasVis( j, i ) );
          gcp.push_back( material.GasCp( j, i ) );
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
    shared_ptr< CGas > CWCEHeatTransferFactory::getAir() {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   August 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Creates air gas layer for tarcog routines
      shared_ptr< CGas > aGas = make_shared< CGas >();
      return aGas;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    shared_ptr< CEnvironment > CWCEHeatTransferFactory::getIndoor() {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   July 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Creates indoor environment object from surface properties in EnergyPlus
      double tin = m_Surface.getInsideAirTemperature( m_SurfNum ) + KelvinConv;
      double hcin = HConvIn( m_SurfNum );

      double IR = m_Surface.getInsideIR( m_SurfNum );

      shared_ptr< CEnvironment > Indoor = make_shared< CIndoorEnvironment >( tin, OutBaroPress );
      Indoor->setHCoeffModel( BoundaryConditionsCoeffModel::CalculateH, hcin );
      Indoor->setEnvironmentIR( IR );
      return Indoor;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    shared_ptr< CEnvironment > CWCEHeatTransferFactory::getOutdoor( const double t_Hext ) {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   July 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Creates outdoor environment object from surface properties in EnergyPlus
      double tout = m_Surface.getOutsideAirTemperature( m_SurfNum ) + KelvinConv;
      double IR = m_Surface.getOutsideIR( m_SurfNum );
      // double dirSolRad = QRadSWOutIncident( t_SurfNum ) + QS( Surface( t_SurfNum ).Zone );
      double swRadiation = m_Surface.getSWIncident( m_SurfNum );
      double tSky = SkyTempKelvin;
      double airSpeed = 0;
      if( m_Surface.ExtWind ) {
        airSpeed = m_Surface.WindSpeed;
      }
      double fclr = 1 - CloudFraction;
      AirHorizontalDirection airDirection = AirHorizontalDirection::Windward;
      shared_ptr< CEnvironment > Outdoor = make_shared< COutdoorEnvironment >( tout, OutBaroPress,
        airSpeed, swRadiation, airDirection, tSky, SkyModel::AllSpecified, fclr );
      Outdoor->setHCoeffModel( BoundaryConditionsCoeffModel::HcPrescribed, t_Hext );
      Outdoor->setEnvironmentIR( IR );
      return Outdoor;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    shared_ptr< CIGU > CWCEHeatTransferFactory::getIGU() {
      // SUBROUTINE INFORMATION:
      //       AUTHOR         Simon Vidanovic
      //       DATE WRITTEN   July 2016
      //       MODIFIED       na
      //       RE-ENGINEERED  na

      // PURPOSE OF THIS SUBROUTINE:
      // Creates IGU object from surface properties in EnergyPlus

      shared_ptr< CIGU > aIGU = make_shared< CIGU >( m_Surface.Width, m_Surface.Height, m_Surface.Tilt );
      return aIGU;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    bool CWCEHeatTransferFactory::isInteriorShade() const {
      return m_InteriorBSDFShade;
    }

  }
}