// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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

#ifndef Vectors_hh_INCLUDED
#define Vectors_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataVectorTypes.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Fwd decl
struct EnergyPlusData;

namespace Vectors {

    // Using/Aliasing
    using DataVectorTypes::Polyhedron;

    // Functions

    Real64 CalcPolygonArea(Array1D<Vector3<Real64>> const &p, int n);

    void VecRound(Vector3<Real64> &vec, Real64 const roundto);

    std::pair<Real64, Real64> CalcAzimuthAndTilt(Array1D<Vector3<Real64>> const &Surf, // Surface Definition
                                                 Vector3<Real64> &lcsx,
                                                 Vector3<Real64> &lcsy,
                                                 Vector3<Real64> &lcsz,
                                                 Vector3<Real64> const &NewellNormVec);
        
    Vector4<Real64> CalcPlaneEquation(Array1D<Vector3<Real64>> const &verts, // Structure of the surface
                                      int const nverts,       // Number of vertices in the surface
                                      bool &error             // returns true for degenerate surface
                                      );

    Real64 Pt2Plane(Vector3<Real64> const &pt,   // Point for determining the distance
                    Vector4<Real64> const &pleq // Equation of the plane
    );

    Vector3<Real64> CalcNewellAreaVector(Array1D<Vector3<Real64>> const &VList, int const NSides);

    Vector3<Real64> CalcNewellNormalVector(Array1D<Vector3<Real64>> const &VList, int const NSides);

    bool VecEqualTol(Vector3<Real64> const &v1, // standard vector
                     Vector3<Real64> const &v2, // standard vector
                     Real64 const tol // specified tolerance
    );

    bool VecEqualTol(Vector2<Real64> const &v1, // standard vector
                     Vector2<Real64> const &v2, // standard vector
                     Real64 const tol // specified tolerance
    );

    std::pair<bool, Real64> CalcCoPlanarNess(Array1D<Vector3<Real64>> const &Surf, int const NSides, int &ErrorVertex);

    std::vector<int>
    PointsInPlane(Array1D<Vector3<Real64>> const &BaseSurf, int const BaseSides, Array1D<Vector3<Real64>> const &QuerySurf, int const QuerySides, bool &ErrorFound);

    Real64 CalcPolyhedronVolume(EnergyPlusData &state, Polyhedron const &Poly);

} // namespace Vectors

struct VectorsData : BaseGlobalStruct
{
    void clear_state() override {} 
};

} // namespace EnergyPlus

#endif
