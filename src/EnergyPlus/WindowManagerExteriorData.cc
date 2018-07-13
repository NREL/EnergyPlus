// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

#include <cassert>

#include <DataBSDFWindow.hh>
#include <DataHeatBalance.hh>
#include <WindowComplexManager.hh>
#include <WindowManager.hh>
#include <UtilityRoutines.hh>

#include <WCEMultiLayerOptics.hpp>

#include "WindowManagerExteriorData.hh"

namespace EnergyPlus {

using namespace DataSurfaces;
using namespace DataHeatBalance;
using namespace DataGlobals;
using namespace WindowComplexManager;

using namespace SingleLayerOptics;
using namespace FenestrationCommon;
using namespace SpectralAveraging;
using namespace MultiLayerOptics;

namespace WindowManager {

    bool isSurfaceHit(const int t_SurfNum, const Vector &t_Ray)
    {
        Real64 DotProd = dot(t_Ray, Surface(t_SurfNum).NewellSurfaceNormalVector);
        return (DotProd > 0);
    }

    std::pair<Real64, Real64> getWCECoordinates(int const t_SurfNum, Vector const &t_Ray, const BSDFHemisphere t_Direction)
    {
        Real64 Theta = 0;
        Real64 Phi = 0;

        // get window tilt and azimuth
        Real64 Gamma = DegToRadians * Surface(t_SurfNum).Tilt;
        Real64 Alpha = DegToRadians * Surface(t_SurfNum).Azimuth;

        int RadType = Front_Incident;

        if (t_Direction == BSDFHemisphere::Outgoing) {
            RadType = Back_Incident;
        }

        // get the corresponding local Theta, Phi for ray
        W6CoordsFromWorldVect(t_Ray, RadType, Gamma, Alpha, Theta, Phi);

        Theta = 180 / Pi * Theta;
        Phi = 180 / Pi * Phi;

        return std::make_pair(Theta, Phi);
    }

    std::pair<Real64, Real64> getSunWCEAngles(const int t_SurfNum, const BSDFHemisphere t_Direction)
    {
        std::pair<Real64, Real64> Angles;
        return getWCECoordinates(t_SurfNum, DataBSDFWindow::SUNCOSTS(TimeStep, HourOfDay, {1, 3}), t_Direction);
    }

    ///////////////////////////////////////////////////////////////////////////////
    //       CWCESpecturmProperties
    ///////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CSeries> CWCESpecturmProperties::getDefaultSolarRadiationSpectrum()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   September 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Handles solar radiation spetrum from defalut location or IDF
        std::shared_ptr<CSeries> solarRadiation = std::make_shared<CSeries>();

        for (auto i = 1; i <= nume; ++i) {
            solarRadiation->addProperty(wle(i), e(i));
        }

        return solarRadiation;
    }

    ///////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CSeries> CWCESpecturmProperties::getDefaultVisiblePhotopicResponse()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   September 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Handles solar radiation spetrum from defalut location or IDF
        std::shared_ptr<CSeries> visibleResponse = std::make_shared<CSeries>();

        for (auto i = 1; i <= numt3; ++i) {
            visibleResponse->addProperty(wlt3(i), y30(i));
        }

        return visibleResponse;
    }

    ///////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CSpectralSampleData> CWCESpecturmProperties::getSpectralSample(int const t_SampleDataPtr)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   September 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Reads spectral data value
        assert(t_SampleDataPtr != 0); // It must not be called for zero value
        std::shared_ptr<CSpectralSampleData> aSampleData = std::make_shared<CSpectralSampleData>();
        auto spectralData = SpectralData(t_SampleDataPtr);
        int numOfWl = spectralData.NumOfWavelengths;
        for (auto i = 1; i <= numOfWl; ++i) {
            Real64 wl = spectralData.WaveLength(i);
            Real64 T = spectralData.Trans(i);
            Real64 Rf = spectralData.ReflFront(i);
            Real64 Rb = spectralData.ReflBack(i);
            aSampleData->addRecord(wl, T, Rf, Rb);
        }

        return aSampleData;
    }

    ///////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CSpectralSampleData> CWCESpecturmProperties::getSpectralSample(MaterialProperties const &t_MaterialProperties)
    {
        Real64 Tsol = t_MaterialProperties.Trans;
        Real64 Rfsol = t_MaterialProperties.ReflectSolBeamFront;
        Real64 Rbsol = t_MaterialProperties.ReflectSolBeamBack;
        std::shared_ptr<CMaterial> aSolMat = std::make_shared<CMaterialSingleBand>(Tsol, Tsol, Rfsol, Rbsol, 0.3, 2.5);

        Real64 Tvis = t_MaterialProperties.TransVis;
        Real64 Rfvis = t_MaterialProperties.ReflectVisBeamFront;
        Real64 Rbvis = t_MaterialProperties.ReflectVisBeamBack;
        std::shared_ptr<CMaterial> aVisMat = std::make_shared<CMaterialSingleBand>(Tvis, Tvis, Rfvis, Rbvis, 0.38, 0.78);

        CMaterialDualBand aMat = CMaterialDualBand(aVisMat, aSolMat, 0.49);
        std::vector<Real64> aWl = aMat.getBandWavelengths();
        std::vector<Real64> aTf = aMat.getBandProperties(Property::T, Side::Front);
        std::vector<Real64> aRf = aMat.getBandProperties(Property::R, Side::Front);
        std::vector<Real64> aRb = aMat.getBandProperties(Property::R, Side::Back);
        std::shared_ptr<CSpectralSampleData> aSampleData = std::make_shared<CSpectralSampleData>();
        for (size_t i = 0; i < aWl.size(); ++i) {
            aSampleData->addRecord(aWl[i], aTf[i], aRf[i], aRb[i]);
        }

        return aSampleData;
    }

    // ///////////////////////////////////////////////////////////////////////////////
    // //       CWindowConstructionsBSDF
    // ///////////////////////////////////////////////////////////////////////////////
    // CWindowConstructionsBSDF& CWindowConstructionsBSDF::instance() {
    // 	static CWindowConstructionsBSDF p_inst;
    // 	return p_inst;
    // }
    //
    // CWindowConstructionsBSDF::CWindowConstructionsBSDF() {
    // 	m_Layers[ WavelengthRange::Solar ] = std::make_shared< LayersBSDF_Map >();
    // 	m_Layers[ WavelengthRange::Visible ] = std::make_shared< LayersBSDF_Map >();
    // }
    //
    // std::vector< Real64 > CWindowConstructionsBSDF::getCommonWavelengths( WavelengthRange const t_Range,
    //                                                                                          int const t_ConstrNum ) const {
    // 	std::shared_ptr< IGU_BSDFLayers > iguLayers = getLayers( t_Range, t_ConstrNum );
    // 	CCommonWavelengths aCommonWL;
    // 	for ( auto layer : *iguLayers ) {
    // 		aCommonWL.addWavelength( layer->getBandWavelengths() );
    // 	}
    //
    // 	return aCommonWL.getCombinedWavelengths( Combine::Interpolate );
    // }
    //
    // std::shared_ptr< IGU_BSDFLayers > CWindowConstructionsBSDF::getLayers( WavelengthRange const t_Range,
    //                                                                        int const t_ConstrNum ) const {
    // 	LayersBSDF_Map aMap = *m_Layers.at( t_Range );
    // 	auto it = aMap.find( t_ConstrNum );
    // 	if ( it == aMap.end() ) {
    // 		throw std::runtime_error( "Incorrect construction selection." );
    // 	}
    // 	return aMap.at( t_ConstrNum );
    // }
    //
    // void CWindowConstructionsBSDF::pushBSDFLayer( WavelengthRange const t_Range, int const t_ConstrNum,
    //                                               std::shared_ptr< CBSDFLayer > const& t_Layer ) {
    // 	std::shared_ptr< LayersBSDF_Map > aMap = m_Layers.at( t_Range );
    // 	auto it = aMap->find( t_ConstrNum );
    // 	std::shared_ptr< IGU_BSDFLayers > iguLayers = nullptr;
    // 	if ( it != aMap->end() ) {
    // 		iguLayers = it->second;
    // 	}
    // 	else {
    // 		iguLayers = std::make_shared< IGU_BSDFLayers >();
    // 		( *aMap )[ t_ConstrNum ] = iguLayers;
    // 	}
    //
    // 	iguLayers->push_back( t_Layer );
    // }
    //
    // std::shared_ptr< CMultiPaneBSDF > CWindowConstructionsBSDF::getEquivalentLayer(
    // 	WavelengthRange const t_Range, int const t_ConstrNum ) {
    // 	auto it = m_Equivalent.find( std::make_pair( t_Range, t_ConstrNum ) );
    // 	if ( it == m_Equivalent.end() ) {
    // 		// Layer was not requested before. Need to create it now.
    // 		std::vector< double > commonWl = getCommonWavelengths( t_Range, t_ConstrNum );
    // 		std::shared_ptr< CSeries > aSolarSpectrum = CWCESpecturmProperties::getDefaultSolarRadiationSpectrum();
    // 		IGU_BSDFLayers iguLayers = *getLayers( t_Range, t_ConstrNum );
    // 		std::shared_ptr< CEquivalentBSDFLayer > aEqLayer = std::make_shared< CEquivalentBSDFLayer >( commonWl, iguLayers[ 0 ] );
    // 		for ( auto i = 1u; i < iguLayers.size(); ++i ) {
    // 			aEqLayer->addLayer( iguLayers[ i ] );
    // 		}
    // 		std::shared_ptr< CMultiPaneBSDF > aMultiLayer =
    // 			std::make_shared< CMultiPaneBSDF >( aEqLayer, aSolarSpectrum );
    //
    // 		m_Equivalent[ std::make_pair( t_Range, t_ConstrNum ) ] = aMultiLayer;
    // 	}
    //
    // 	return m_Equivalent.at( std::make_pair( t_Range, t_ConstrNum ) );
    // }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWindowConstructionsSimplified
    ///////////////////////////////////////////////////////////////////////////////
    std::unique_ptr<CWindowConstructionsSimplified> CWindowConstructionsSimplified::p_inst = nullptr;

    CWindowConstructionsSimplified &CWindowConstructionsSimplified::instance()
    {
        if(p_inst == nullptr) {
	        p_inst =
	        		std::unique_ptr<CWindowConstructionsSimplified>(new CWindowConstructionsSimplified());
        }
        return *p_inst;
    }

    CWindowConstructionsSimplified::CWindowConstructionsSimplified()
    {
        m_Layers[WavelengthRange::Solar] = Layers_Map();
        m_Layers[WavelengthRange::Visible] = Layers_Map();
    }

    void CWindowConstructionsSimplified::pushLayer(WavelengthRange const t_Range, int const t_ConstrNum, std::shared_ptr<CScatteringLayer> const &t_Layer)
    {
        Layers_Map & aMap = m_Layers.at(t_Range);
        const auto it = aMap.find(t_ConstrNum);
        if (it == aMap.end()) {
            aMap[t_ConstrNum] = IGU_Layers();
        }
        aMap.at(t_ConstrNum).push_back( t_Layer );        
    }

    std::shared_ptr<CMultiLayerScattered> CWindowConstructionsSimplified::getEquivalentLayer(WavelengthRange const t_Range, int const t_ConstrNum)
    {
        auto it = m_Equivalent.find(std::make_pair(t_Range, t_ConstrNum));
        if (it == m_Equivalent.end()) {
            // Layer was not requested before. Need to create it now.
            // shared_ptr< vector< double > > commonWl = getCommonWavelengths( t_Range, t_ConstrNum );
            IGU_Layers iguLayers = getLayers(t_Range, t_ConstrNum);
            std::shared_ptr<CMultiLayerScattered> aEqLayer = std::make_shared<CMultiLayerScattered>(iguLayers[0]);
            for (auto i = 1u; i < iguLayers.size(); ++i) {
                aEqLayer->addLayer(iguLayers[i]);
            }

            std::shared_ptr<CSeries> aSolarSpectrum = CWCESpecturmProperties::getDefaultSolarRadiationSpectrum();
            aEqLayer->setSourceData(aSolarSpectrum);
            m_Equivalent[std::make_pair(t_Range, t_ConstrNum)] = aEqLayer;
        }

        return m_Equivalent.at(std::make_pair(t_Range, t_ConstrNum));
    }

    void CWindowConstructionsSimplified::clearState() {
        p_inst = nullptr;
    }

    IGU_Layers CWindowConstructionsSimplified::getLayers(WavelengthRange const t_Range, int const t_ConstrNum) const
    {
        Layers_Map aMap = m_Layers.at(t_Range);
        auto it = aMap.find(t_ConstrNum);
        if (it == aMap.end()) {
            ShowFatalError("Incorrect construction selection.");
            //throw std::runtime_error("Incorrect construction selection.");
        }
        return aMap.at(t_ConstrNum);
    }

} // namespace WindowManager

} // namespace EnergyPlus
