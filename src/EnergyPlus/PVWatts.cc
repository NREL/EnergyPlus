// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois,
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

// C++ Headers


// ObjexxFCL Headers

// EnergyPlus Headers

#include <PVWatts.hh>
#include <General.hh>
#include <InputProcessor.hh>

namespace EnergyPlus {

namespace PVWatts {

    std::map<std::string, PVWattsGenerator> PVWattsGenerators;
    
    PVWattsGenerator::PVWattsGenerator(const std::string &name, const double dcSystemCapacity, ModuleType moduleType, ArrayType arrayType, double systemLosses, GeometryType geometryType, double tilt, double azimuth, size_t surfaceNum, double groundCoverageRatio)
    {
        using General::RoundSigDigits;
        bool errorsFound(false);
        
        if (name.empty()) {
            ShowSevereError("PVWatts: name cannot be blank.");
            errorsFound = true;
        }
        m_name = name;
        
        if (dcSystemCapacity <= 0) {
            ShowSevereError("PVWatts: DC system capacity must be greater than zero.");
            errorsFound = true;
        }
        m_dcSystemCapacity = dcSystemCapacity;
        
        m_moduleType = moduleType;
        m_arrayType = arrayType;
        
        if (systemLosses > 1.0 || systemLosses < 0.0) {
            ShowSevereError("PVWatts: Invalid system loss value " + RoundSigDigits(systemLosses, 2));
            errorsFound = true;
        }
        m_systemLosses = systemLosses;
        
        m_geometryType = geometryType;
        
        if (m_geometryType == GeometryType::TILT_AZIMUTH) {
            if ( tilt < 0 || tilt > 90) {
                ShowSevereError("PVWatts: Invalid tilt: " + RoundSigDigits(tilt, 2));
                errorsFound = true;
            }
            m_tilt = tilt;
            if ( azimuth < 0 || azimuth >= 360) {
                ShowSevereError("PVWatts: Invalid azimuth: " + RoundSigDigits(azimuth, 2));
            }
            m_azimuth = azimuth;
        } else if (m_geometryType == GeometryType::SURFACE) {
            if (surfaceNum == 0 || surfaceNum > DataSurfaces::Surface.size()) {
                ShowSevereError("PVWatts: SurfaceNum not in Surfaces: " + std::to_string(surfaceNum));
                errorsFound = true;
            } else {
                m_surfacePtr = &DataSurfaces::Surface(surfaceNum);
                // TODO: Calculate tilt and azimuth from the surface.
            }
        } else {
            assert(false);
        }
        
        if (groundCoverageRatio > 1.0 || groundCoverageRatio < 0.0) {
            ShowSevereError("PVWatts: Invalid ground coverage ratio: " + RoundSigDigits(groundCoverageRatio, 2));
            errorsFound = true;
        }
        m_groundCoverageRatio = groundCoverageRatio;
        
        if (errorsFound) {
            ShowFatalError("Errors found in getting PVWatts input");
        }
    }
    
    double PVWattsGenerator::getDCSystemCapacity()
    {
        return m_dcSystemCapacity;
    }
    
    ModuleType PVWattsGenerator::getModuleType()
    {
        return m_moduleType;
    }
    
    ArrayType PVWattsGenerator::getArrayType()
    {
        return m_arrayType;
    }
    
    double PVWattsGenerator::getSystemLosses()
    {
        return m_systemLosses;
    }
    
    GeometryType PVWattsGenerator::getGeometryType()
    {
        return m_geometryType;
    }
    
    double PVWattsGenerator::getTilt()
    {
        return m_tilt;
    }
    
    double PVWattsGenerator::getAzimuth()
    {
        return m_azimuth;
    }
    
    DataSurfaces::SurfaceData* PVWattsGenerator::getSurface()
    {
        return m_surfacePtr;
    }
    
    double PVWattsGenerator::getGroundCoverageRatio()
    {
        return m_groundCoverageRatio;
    }
    
}

}
