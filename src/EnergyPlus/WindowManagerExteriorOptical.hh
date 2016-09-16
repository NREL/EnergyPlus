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

#ifndef WindowManagerExteriorOptical_hh_INCLUDED
#define WindowManagerExteriorOptical_hh_INCLUDED


#include <map>
#include <tuple>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace FenestrationCommon {

  class CSeries;
  enum class WavelengthRange;
  enum class Property;
  enum class Side;

}

namespace SpectralAveraging {

  class CSpectralSampleData;
  class CSpectralSample;
  class CAngularSpectralSample;

}

namespace LayerOptics {

  class CBSDFLayer;

}

namespace EnergyPlus {

  namespace WindowManager {

    class CWCEIntegrator;

    // Initialize window optical properties with Windows-CalcEngine routines
    void InitWCEOpticalData();
    void StoreOpticalData( const CWCEIntegrator& t_Integrator, const FenestrationCommon::WavelengthRange t_Range,
      const int t_ConstrNum );

    std::unique_ptr< CWCEIntegrator > getIntegrator( const EnergyPlus::DataHeatBalance::MaterialProperties& material );

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCESpecturmProperties
    ///////////////////////////////////////////////////////////////////////////////
    class CWCESpecturmProperties {
    public:
      static std::shared_ptr< SpectralAveraging::CSpectralSampleData > getSpectralSample( const int t_SampleDataPtr );
      static std::shared_ptr< FenestrationCommon::CSeries > getSolarRadiationSpecturm();
      static std::shared_ptr< FenestrationCommon::CSeries > getVisiblePhotopicResponse();
    };

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCERangeIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    class CWCERangeIntegrator {
    public:
      CWCERangeIntegrator();

      virtual double getProperty( const FenestrationCommon::Side t_Side, 
        const FenestrationCommon::Property t_Property ) const = 0;

    };

    ///////////////////////////////////////////////////////////////////////////////
    // CWCEIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    class CWCEIntegrator {
    public:
      CWCEIntegrator();

      double getProperty( const FenestrationCommon::Side t_Side, const FenestrationCommon::Property t_Property,
        const FenestrationCommon::WavelengthRange t_Range ) const;
    
    protected:
      std::map< FenestrationCommon::WavelengthRange, std::shared_ptr< CWCERangeIntegrator > > m_Integrator;
    };

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCESpecularRangeIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    class CWCESpecularRangeIntegrator : public CWCERangeIntegrator {
    public:
      CWCESpecularRangeIntegrator( const EnergyPlus::DataHeatBalance::MaterialProperties &material, 
        const FenestrationCommon::WavelengthRange t_Range );

      double getProperty( const FenestrationCommon::Side t_Side, const FenestrationCommon::Property t_Property ) const;

    private:
      void calculateProperties( const FenestrationCommon::Side t_Side, const double lowLambda, const double highLambda );
      double calculateProperty( const FenestrationCommon::Side t_Side,
        const FenestrationCommon::Property t_Property, const double lowLambda, const double highLambda );

      std::shared_ptr< SpectralAveraging::CAngularSpectralSample > m_AngularSample;
      std::map< std::pair< FenestrationCommon::Side, FenestrationCommon::Property >, double > m_Results;
    };

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCESpecularIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    class CWCESpecularIntegrator : public CWCEIntegrator {
    public:
      CWCESpecularIntegrator( const EnergyPlus::DataHeatBalance::MaterialProperties &material );

      double getProperty( const FenestrationCommon::Side t_Side, const FenestrationCommon::Property t_Property,
        const FenestrationCommon::WavelengthRange t_Range ) const;

    };

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEShadeMaterialFactory
    ///////////////////////////////////////////////////////////////////////////////
    class CWCEShadeMaterialFactory {
    public:
      CWCEShadeMaterialFactory();

      double getProperty( const FenestrationCommon::WavelengthRange t_Range,
        const FenestrationCommon::Side t_Side, const FenestrationCommon::Property t_Property ) const;

    protected:
      std::map< std::pair< FenestrationCommon::Side, FenestrationCommon::Property >, double > m_Solar;
      std::map< std::pair< FenestrationCommon::Side, FenestrationCommon::Property >, double > m_Visible;

    };

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEShadeRangeIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    class CWCEShadeRangeIntegrator : public CWCERangeIntegrator {
    public:
      CWCEShadeRangeIntegrator();

      double getProperty( const FenestrationCommon::Side t_Side, 
        const FenestrationCommon::Property t_Property ) const;

    protected:
      std::shared_ptr< LayerOptics::CBSDFLayer > m_Shade;
    };

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEVenetianBlindMaterialsFactory
    ///////////////////////////////////////////////////////////////////////////////
    class CWCEVenetianBlindMaterialsFactory : public CWCEShadeMaterialFactory {
    public:
      CWCEVenetianBlindMaterialsFactory( const EnergyPlus::DataHeatBalance::WindowBlindProperties& blind );

    };

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEVenetianBlindRangeIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    class CWCEVenetianBlindRangeIntegrator : public CWCEShadeRangeIntegrator {
    public:
      CWCEVenetianBlindRangeIntegrator( const EnergyPlus::DataHeatBalance::MaterialProperties &material,
        const FenestrationCommon::WavelengthRange t_Range );

    };

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEVenetianBlindIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    class CWCEVenetianBlindIntegrator : public CWCEIntegrator {
    public:
      CWCEVenetianBlindIntegrator( const EnergyPlus::DataHeatBalance::MaterialProperties &material );

    };

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEScreenMaterialsFactory
    ///////////////////////////////////////////////////////////////////////////////
    class CWCEScreenMaterialsFactory : public CWCEShadeMaterialFactory {
    public:
      CWCEScreenMaterialsFactory( const EnergyPlus::DataHeatBalance::MaterialProperties& material );

    };

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEScreenRangeIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    class CWCEScreenRangeIntegrator : public CWCEShadeRangeIntegrator {
    public:
      CWCEScreenRangeIntegrator( const EnergyPlus::DataHeatBalance::MaterialProperties &material,
        const FenestrationCommon::WavelengthRange t_Range );

    };

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEScreenIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    class CWCEScreenIntegrator : public CWCEIntegrator {
    public:
      CWCEScreenIntegrator( const EnergyPlus::DataHeatBalance::MaterialProperties &material );

    };

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEDiffuseShadeMaterialsFactory
    ///////////////////////////////////////////////////////////////////////////////
    class CWCEDiffuseShadeMaterialsFactory : public CWCEShadeMaterialFactory {
    public:
      CWCEDiffuseShadeMaterialsFactory( const EnergyPlus::DataHeatBalance::MaterialProperties& material );

    };

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEDiffuseShadeRangeIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    class CWCEDiffuseShadeRangeIntegrator : public CWCERangeIntegrator {
    public:
      CWCEDiffuseShadeRangeIntegrator( const EnergyPlus::DataHeatBalance::MaterialProperties &material,
        const FenestrationCommon::WavelengthRange t_Range );

      double getProperty( const FenestrationCommon::Side t_Side,
        const FenestrationCommon::Property t_Property ) const;

    private:
      void storeProperties( const FenestrationCommon::WavelengthRange t_Range,
        const EnergyPlus::DataHeatBalance::MaterialProperties &material );

      std::map< std::pair< FenestrationCommon::Side, FenestrationCommon::Property >, double > m_Results;

    };

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEDiffuseShadeIntegrator
    ///////////////////////////////////////////////////////////////////////////////
    class CWCEDiffuseShadeIntegrator : public CWCEIntegrator {
    public:
      CWCEDiffuseShadeIntegrator( const EnergyPlus::DataHeatBalance::MaterialProperties &material );

    };

  }

}

#endif