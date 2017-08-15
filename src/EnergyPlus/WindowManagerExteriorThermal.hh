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

#ifndef WindowManagerExteriorThermal_hh_INCLUDED
#define WindowManagerExteriorThermal_hh_INCLUDED

namespace EnergyPlus {
  
  namespace DataSurfaces {
    struct SurfaceData;
  }
  
  namespace DataHeatBalance {
    struct MaterialProperties;
  }

}

namespace Tarcog {

  class CEnvironment;
  class CBaseIGULayer;
  class CIGU;
  class CSingleSystem;

}

namespace Gases {

  class CGas;

}

namespace EnergyPlus {

  namespace WindowManager {

    enum class ShadePosition { NoShade, Interior, Exterior, Between };

    // Routine that calculates heat transfer balance by using Windows-CalcEngine routines
    void CalcWindowHeatBalanceExternalRoutines(
      int const SurfNum, // Surface number
      Real64 const HextConvCoeff, // Outside air film conductance coefficient
      Real64 & SurfInsideTemp, // Inside window surface temperature
      Real64 & SurfOutsideTemp // Outside surface temperature (C)
    );

    // Class that is used to create layers for Windows-CalcEngine
    class CWCEHeatTransferFactory {
    public:

      CWCEHeatTransferFactory( EnergyPlus::DataSurfaces::SurfaceData const & surface, int const t_SurfNum );
      
      std::shared_ptr< Tarcog::CSingleSystem > getTarcogSystem( double const t_HextConvCoeff );

      std::shared_ptr< Tarcog::CBaseIGULayer > getIGULayer( int const t_Index );
      std::shared_ptr< Tarcog::CEnvironment > getIndoor() const;
      std::shared_ptr< Tarcog::CEnvironment > getOutdoor( double const t_Hext ) const;
      std::shared_ptr< Tarcog::CIGU > getIGU( );

      // This special case of interior shade is necessary only because of strange calculation of heat flow on interior side
      // It probably needs to be removed since calculation is no different from any other case. It is left over from
      // old EnergyPlus code and it needs to be checked.
      bool isInteriorShade() const;

    private:
      DataSurfaces::SurfaceData m_Surface;
      DataSurfaces::SurfaceWindowCalc m_Window;
      ShadePosition m_ShadePosition;
      int m_SurfNum;
      size_t m_SolidLayerIndex;
      int m_ConstructionNumber;
      int m_TotLay;

      // Next two booleans are to keep track of order for shading devices. Some variables
      // are kept in separate arrays for specular glazings and shading devices and it is 
      // important to keep index numbering in order to extract correct results
      bool m_InteriorBSDFShade;
      bool m_ExteriorShade;

      int getNumOfLayers() const;

      std::shared_ptr< Tarcog::CBaseIGULayer > getSolidLayer(
        DataSurfaces::SurfaceData const & surface,
        DataHeatBalance::MaterialProperties const & material,
        int const t_Index, int const t_SurfNum );

      std::shared_ptr< Tarcog::CBaseIGULayer > getGapLayer(
        DataHeatBalance::MaterialProperties const & material ) const;

      std::shared_ptr< Tarcog::CBaseIGULayer > getShadeToGlassLayer( int const t_Index ) const;

      std::shared_ptr< Tarcog::CBaseIGULayer > getComplexGapLayer(
        DataHeatBalance::MaterialProperties const & material ) const;

      std::shared_ptr< Gases::CGas > getGas( DataHeatBalance::MaterialProperties const & material ) const;
      std::shared_ptr< Gases::CGas > getAir() const;
      DataHeatBalance::MaterialProperties* getLayerMaterial( int const t_Index ) const;

    };        
  }

}

#endif