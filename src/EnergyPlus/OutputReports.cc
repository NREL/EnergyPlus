// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/DXFEarClipping.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataDaylighting.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSurfaceColors.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/OutputReports.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

void ReportSurfaces(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   February 1999
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calls several optional routines to report
    // the surfaces to output formats that can render the data
    // into a descriptive picture.

    // METHODOLOGY EMPLOYED:
    // Use a REPORT command to determine if there should be
    // a file created.

    // Using/Aliasing
    using namespace DataSurfaceColors;
    using General::ScanForReports;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SurfDetails;
    bool SurfVert;
    bool SurfDet;
    bool DXFDone;
    bool VRMLDone;
    std::string Option1;
    std::string Option2;
    bool DoReport;

    state.dataErrTracking->AskForSurfacesReport = false;

    SurfDetails = 0;
    SurfVert = false;
    SurfDet = false;
    DXFDone = false;
    VRMLDone = false;
    Option1 = "";
    Option2 = "";

    ScanForReports(state, "Surfaces", DoReport, "Lines", Option1);
    if (DoReport) LinesOut(state, Option1);

    ScanForReports(state, "Surfaces", DoReport, "Vertices");
    if (DoReport) {
        if (!SurfVert) {
            ++SurfDetails;
            SurfVert = true;
        }
    }

    ScanForReports(state, "Surfaces", DoReport, "Details");
    if (DoReport) {
        if (!SurfDet) {
            SurfDetails += 10;
            SurfDet = true;
        }
    }

    ScanForReports(state, "Surfaces", DoReport, "DetailsWithVertices");
    if (DoReport) {
        if (!SurfDet) {
            SurfDetails += 10;
            SurfDet = true;
        }
        if (!SurfVert) {
            ++SurfDetails;
            SurfVert = true;
        }
    }

    ScanForReports(state, "Surfaces", DoReport, "DXF", Option1, Option2);
    if (DoReport) {
        if (!DXFDone) {
            if (Option2 != "") {
                SetUpSchemeColors(state, Option2, "DXF");
            }
            DXFOut(state, Option1, Option2);
            DXFDone = true;
        } else {
            ShowWarningError(state, "ReportSurfaces: DXF output already generated.  DXF with option=[" + Option1 + "] will not be generated.");
        }
    }

    ScanForReports(state, "Surfaces", DoReport, "DXF:WireFrame", Option1, Option2);
    if (DoReport) {
        if (!DXFDone) {
            if (Option2 != "") {
                SetUpSchemeColors(state, Option2, "DXF");
            }
            DXFOutWireFrame(state, Option2);
            DXFDone = true;
        } else {
            ShowWarningError(state, "ReportSurfaces: DXF output already generated.  DXF:WireFrame will not be generated.");
        }
    }

    ScanForReports(state, "Surfaces", DoReport, "VRML", Option1, Option2);
    if (DoReport) {
        if (!VRMLDone) {
            VRMLOut(state, Option1, Option2);
            VRMLDone = true;
        } else {
            ShowWarningError(state, "ReportSurfaces: VRML output already generated.  VRML with option=[" + Option1 + "] will not be generated.");
        }
    }

    ScanForReports(state, "Surfaces", DoReport, "CostInfo");
    if (DoReport) {
        CostInfoOut(state);
    }

    if (SurfDet || SurfVert) {
        DetailsForSurfaces(state, SurfDetails);
    }
}

void LinesOut(EnergyPlusData &state, std::string const &option)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   March 1999
    //       MODIFIED       March 2006 -- add option for "IDF segments out"
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine produces a file of lines in the surfaces.

    // METHODOLOGY EMPLOYED:
    // Use the surface absolute coordinate information to produce
    // lines.

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace DataHeatBalance;
    using namespace DataSurfaces;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:

    static constexpr std::string_view vertexstring("X,Y,Z ==> Vertex");

    if (state.dataSurface->TotSurfaces > 0 && !allocated(state.dataSurface->Surface)) {
        // no error needed, probably in end processing, just return
        return;
    }

    if (state.dataOutputReports->optiondone) {
        ShowWarningError(state, "Report of Surfaces/Lines Option has already been completed with option=" + state.dataOutputReports->lastoption);
        ShowContinueError(state, "..option=\"" + option + "\" will not be done this time.");
        return;
    }

    state.dataOutputReports->lastoption = option;
    state.dataOutputReports->optiondone = true;

    auto slnfile = state.files.sln.open(state, "LinesOut", state.files.outputControl.sln);

    if (option != "IDF") {
        for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::IntMass) continue;
            if (state.dataSurface->Surface(surf).Sides == 0) continue;
            print(slnfile, "{}:{}\n", state.dataSurface->Surface(surf).ZoneName, state.dataSurface->Surface(surf).Name);
            for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                static constexpr fmt::string_view fmt700("{:10.2F},{:10.2F},{:10.2F},{:10.2F},{:10.2F},{:10.2F}\n");

                if (vert != state.dataSurface->Surface(surf).Sides) {
                    print(slnfile,
                          fmt700,
                          state.dataSurface->Surface(surf).Vertex(vert).x,
                          state.dataSurface->Surface(surf).Vertex(vert).y,
                          state.dataSurface->Surface(surf).Vertex(vert).z,
                          state.dataSurface->Surface(surf).Vertex(vert + 1).x,
                          state.dataSurface->Surface(surf).Vertex(vert + 1).y,
                          state.dataSurface->Surface(surf).Vertex(vert + 1).z);
                } else {
                    print(slnfile,
                          fmt700,
                          state.dataSurface->Surface(surf).Vertex(vert).x,
                          state.dataSurface->Surface(surf).Vertex(vert).y,
                          state.dataSurface->Surface(surf).Vertex(vert).z,
                          state.dataSurface->Surface(surf).Vertex(1).x,
                          state.dataSurface->Surface(surf).Vertex(1).y,
                          state.dataSurface->Surface(surf).Vertex(1).z);
                }
            }
        }
    } else {
        print(slnfile, "{}\n", " Building North Axis = 0");
        print(slnfile, "{}\n", "GlobalGeometryRules,UpperLeftCorner,CounterClockwise,WorldCoordinates;");
        for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::IntMass) continue;
            if (state.dataSurface->Surface(surf).Sides == 0) continue;
            // process heat transfer surfaces
            print(slnfile,
                  " Surface={}, Name={}, Azimuth={:.1R}\n",
                  cSurfaceClass(state.dataSurface->Surface(surf).Class),
                  state.dataSurface->Surface(surf).Name,
                  state.dataSurface->Surface(surf).Azimuth);
            print(slnfile, "  {},  !- Number of (X,Y,Z) groups in this surface\n", state.dataSurface->Surface(surf).Sides);
            for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                std::string optcommasemi = ",";
                if (vert == state.dataSurface->Surface(surf).Sides) optcommasemi = ";";
                static constexpr fmt::string_view fmtcoord("  {:10.2F},{:10.2F},{:10.2F}{}  !- {} {}\n");
                print(slnfile,
                      fmtcoord,
                      state.dataSurface->Surface(surf).Vertex(vert).x,
                      state.dataSurface->Surface(surf).Vertex(vert).y,
                      state.dataSurface->Surface(surf).Vertex(vert).z,
                      optcommasemi,
                      vertexstring,
                      vert);
            }
        }
    }
}

static std::string normalizeName(std::string name)
{
    std::replace(begin(name), end(name), ' ', '_');
    std::replace(begin(name), end(name), ':', '_');
    return name;
}

static void WriteDXFCommon(EnergyPlusData &state, InputOutputFile &of, const std::string &ColorScheme)
{
    using namespace DataSurfaces;
    using namespace DataSurfaceColors;
    static constexpr fmt::string_view Format_800("  0\nTEXT\n  8\n1\n  6\nContinuous\n 62\n{:3}\n 10\n{:15.5F}\n 20\n{:15.5F}\n 30\n{:15.5F}\n 40\n .25\n  "
                                     "1\nTrue North\n 41\n 0.0\n  7\nMONOTXT\n210\n0.0\n220\n0.0\n230\n1.0\n");
    static constexpr fmt::string_view Format_801("  0\nTEXT\n  8\n1\n  6\nContinuous\n 62\n{:3}\n 10\n{:15.5F}\n 20\n{:15.5F}\n 30\n{:15.5F}\n 40\n .4\n  "
                                     "1\n{}\n 41\n 0.0\n  7\nMONOTXT\n210\n0.0\n220\n0.0\n230\n1.0\n");

    static constexpr fmt::string_view Format_703_0("  0\n3DFACE\n  8\n1\n 62\n{:3}\n");
    static constexpr fmt::string_view Format_703_1(" 10\n{:15.5F}\n 20\n{:15.5F}\n 30\n{:15.5F}\n");
    static constexpr fmt::string_view Format_703_2(" 11\n{:15.5F}\n 21\n{:15.5F}\n 31\n{:15.5F}\n");
    static constexpr fmt::string_view Format_703_3(" 12\n{:15.5F}\n 22\n{:15.5F}\n 32\n{:15.5F}\n");
    static constexpr fmt::string_view Format_703_4(" 13\n{:15.5F}\n 23\n{:15.5F}\n 33\n{:15.5F}\n");

    static constexpr fmt::string_view Format_708{"999\n{}{}{}\n"};
    static constexpr fmt::string_view Format_710{"999\n{}\n"};

    Array1D<Real64> StemX(4, -10.0);
    Array1D<Real64> StemY(4, {3.0, 3.0, 0.0, 0.0});
    Array1D<Real64> StemZ(4, {0.1, 0.0, 0.0, 0.1});
    Array1D<Real64> Head1X(4, {-10.0, -10.0, -10.5, -10.5});
    Array1D<Real64> Head1Y(4, {3.0, 3.0, 2.133975, 2.133975});
    Array1D<Real64> Head1Z(4, {0.1, 0.0, 0.0, 0.1});
    Array1D<Real64> Head2X(4, {-10.0, -10.0, -9.5, -9.5});
    Array1D<Real64> Head2Y(4, {3.0, 3.0, 2.133975, 2.133975});
    Array1D<Real64> Head2Z(4, {0.1, 0.0, 0.0, 0.1});
    Array1D<Real64> NSide1X(4, -10.5);
    Array1D<Real64> NSide1Y(4, {4.5, 4.5, 3.5, 3.5});
    Array1D<Real64> NSide1Z(4, {0.1, 0.0, 0.0, 0.1});
    Array1D<Real64> NSide2X(4, {-10.5, -10.5, -9.5, -9.5});
    Array1D<Real64> NSide2Y(4, {4.5, 4.5, 3.5, 3.5});
    Array1D<Real64> NSide2Z(4, {0.1, 0.0, 0.0, 0.1});
    Array1D<Real64> NSide3X(4, -9.5);
    Array1D<Real64> NSide3Y(4, {4.5, 4.5, 3.5, 3.5});
    Array1D<Real64> NSide3Z(4, {0.1, 0.0, 0.0, 0.1});

    if (ColorScheme.empty()) {
        print(of, Format_708, "Color Scheme", ",", "Default");
    } else {
        print(of, Format_708, "Color Scheme", ",", ColorScheme);
    }

    Real64 minx = 99999.0;
    Real64 miny = 99999.0;
    for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::IntMass) continue;
        for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
            minx = min(minx, state.dataSurface->Surface(surf).Vertex(vert).x);
            miny = min(miny, state.dataSurface->Surface(surf).Vertex(vert).y);
        }
    }

    for (int vert = 1; vert <= 4; ++vert) {
        StemX(vert) += minx;
        StemY(vert) += miny;
        Head1X(vert) += minx;
        Head1Y(vert) += miny;
        Head2X(vert) += minx;
        Head2Y(vert) += miny;
        NSide1X(vert) += minx;
        NSide1Y(vert) += miny;
        NSide2X(vert) += minx;
        NSide2Y(vert) += miny;
        NSide3X(vert) += minx;
        NSide3Y(vert) += miny;
    }

    auto &DXFcolorno = state.dataSurfColor->DXFcolorno;

    // This writes "True North" above the Arrow Head
    print(of, Format_710, "Text - True North");
    print(of, Format_800, DXFcolorno(static_cast<int>(ColorNo::Text)), StemX(1) - 1.0, StemY(1), StemZ(1));

    print(of, Format_710, "Text - Building Title");
    print(of,
          Format_801,
          DXFcolorno(static_cast<int>(ColorNo::Text)),
          StemX(1) - 4.0,
          StemY(1) - 4.0,
          StemZ(1),
          "Building - " + state.dataHeatBal->BuildingName);

    // We want to point the north arrow to true north
    print(of, Format_710, "North Arrow Stem");
    print(of, Format_703_0, DXFcolorno(static_cast<int>(ColorNo::Text)));
    print(of, Format_703_1, StemX(1), StemY(1), StemZ(1));
    print(of, Format_703_2, StemX(2), StemY(2), StemZ(2));
    print(of, Format_703_3, StemX(3), StemY(3), StemZ(3));
    print(of, Format_703_4, StemX(4), StemY(4), StemZ(4));

    print(of, Format_710, "North Arrow Head 1");
    print(of, Format_703_0, DXFcolorno(static_cast<int>(ColorNo::Text)));
    print(of, Format_703_1, Head1X(1), Head1Y(1), Head1Z(1));
    print(of, Format_703_2, Head1X(2), Head1Y(2), Head1Z(2));
    print(of, Format_703_3, Head1X(3), Head1Y(3), Head1Z(3));
    print(of, Format_703_4, Head1X(4), Head1Y(4), Head1Z(4));

    print(of, Format_710, "North Arrow Head 2");
    print(of, Format_703_0, DXFcolorno(static_cast<int>(ColorNo::Text)));
    print(of, Format_703_1, Head2X(1), Head2Y(1), Head2Z(1));
    print(of, Format_703_2, Head2X(2), Head2Y(2), Head2Z(2));
    print(of, Format_703_3, Head2X(3), Head2Y(3), Head2Z(3));
    print(of, Format_703_4, Head2X(4), Head2Y(4), Head2Z(4));

    print(of, Format_710, "North Arrow Side 1");
    print(of, Format_703_0, DXFcolorno(static_cast<int>(ColorNo::Text)));
    print(of, Format_703_1, NSide1X(1), NSide1Y(1), NSide1Z(1));
    print(of, Format_703_2, NSide1X(2), NSide1Y(2), NSide1Z(2));
    print(of, Format_703_3, NSide1X(3), NSide1Y(3), NSide1Z(3));
    print(of, Format_703_4, NSide1X(4), NSide1Y(4), NSide1Z(4));

    print(of, Format_710, "North Arrow Side 2");
    print(of, Format_703_0, DXFcolorno(static_cast<int>(ColorNo::Text)));
    print(of, Format_703_1, NSide2X(1), NSide2Y(1), NSide2Z(1));
    print(of, Format_703_2, NSide2X(2), NSide2Y(2), NSide2Z(2));
    print(of, Format_703_3, NSide2X(3), NSide2Y(3), NSide2Z(3));
    print(of, Format_703_4, NSide2X(4), NSide2Y(4), NSide2Z(4));

    print(of, Format_710, "North Arrow Side 3");
    print(of, Format_703_0, DXFcolorno(static_cast<int>(ColorNo::Text)));
    print(of, Format_703_1, NSide3X(1), NSide3Y(1), NSide3Z(1));
    print(of, Format_703_2, NSide3X(2), NSide3Y(2), NSide3Z(2));
    print(of, Format_703_3, NSide3X(3), NSide3Y(3), NSide3Z(3));
    print(of, Format_703_4, NSide3X(4), NSide3Y(4), NSide3Z(4));

    print(of, Format_710, "Zone Names");

    for (int zones = 1; zones <= state.dataGlobal->NumOfZones; ++zones) {
        print(of, Format_710, format("Zone={}:{}", zones, normalizeName(state.dataHeatBal->Zone(zones).Name)));
    }
}

static void DXFDaylightingReferencePoints(EnergyPlusData &state, InputOutputFile &of, bool const DELight)
{
    using namespace DataSurfaceColors;

    static constexpr fmt::string_view Format_709("  0\nCIRCLE\n  8\n{}\n 62\n{:3}\n 10\n{:15.5F}\n 20\n{:15.5F}\n 30\n{:15.5F}\n 40\n{:15.5F}\n");

    // Do any daylighting reference points on layer for zone
    for (int zones = 1; zones <= state.dataGlobal->NumOfZones; ++zones) {
        auto curcolorno = ColorNo::DaylSensor1;

        for (int refpt = 1; refpt <= state.dataDaylightingData->ZoneDaylight(zones).TotalDaylRefPoints; ++refpt) {
            print(of, "999\n{}:{}:{}\n", state.dataHeatBal->Zone(zones).Name, DELight ? "DEDayRefPt" : "DayRefPt", refpt);
            print(of,
                  Format_709,
                  normalizeName(state.dataHeatBal->Zone(zones).Name),
                  state.dataSurfColor->DXFcolorno(static_cast<int>(curcolorno)),
                  state.dataDaylightingData->ZoneDaylight(zones).DaylRefPtAbsCoord(1, refpt),
                  state.dataDaylightingData->ZoneDaylight(zones).DaylRefPtAbsCoord(2, refpt),
                  state.dataDaylightingData->ZoneDaylight(zones).DaylRefPtAbsCoord(3, refpt),
                  0.2);
            curcolorno = ColorNo::DaylSensor2; // ref pts 2 and later are this color
        }
    }
}

void DXFOut(EnergyPlusData &state,
            std::string const &PolygonAction,
            std::string const &ColorScheme // Name from user for color scheme or blank
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   March 1999
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine produces a file of DXF objects for the surfaces.

    // METHODOLOGY EMPLOYED:
    // Use the surface absolute coordinate information to produce
    // lines.

    // Using/Aliasing
    using namespace DataSurfaces;
    using namespace DataSurfaceColors;
    using namespace DXFEarClipping;

    bool ThickPolyline(false);
    bool RegularPolyline(false);
    std::string PolylineWidth(" 0.55");
    bool TriangulateFace(false);

    // Formats
    constexpr auto Format_702("  0\nSECTION\n  2\nENTITIES\n");
    constexpr auto Format_707("999\nDXF created from EnergyPlus\n");
    constexpr auto Format_708("999\n{}{}{}\n");

    constexpr auto Format_715("  0\nPOLYLINE\n  8\n{}\n 62\n{:3}\n 66\n  1\n 10\n 0.0\n 20\n 0.0\n 30\n{:15.5F}\n 70\n   9\n 40\n{}\n 41\n{}\n");
    constexpr auto Format_716("  0\nVERTEX\n  8\n{}\n 10\n{:15.5F}\n 20\n{:15.5F}\n 30\n{:15.5F}\n");
    constexpr auto Format_717("  0\nSEQEND\n  8\n{}\n");
    constexpr auto Format_704("  0\n3DFACE\n  8\n{}\n 62\n{:3}\n 10\n{:15.5F}\n 20\n{:15.5F}\n 30\n{:15.5F}\n 11\n{:15.5F}\n 21\n{:15.5F}\n "
                              "31\n{:15.5F}\n 12\n{:15.5F}\n 22\n{:15.5F}\n 32\n{:15.5F}\n");
    constexpr auto Format_704_0("  0\n3DFACE\n  8\n{}\n 62\n{:3}\n");
    constexpr auto Format_704_1(" 10\n{:15.5F}\n 20\n{:15.5F}\n 30\n{:15.5F}\n");
    constexpr auto Format_704_2(" 11\n{:15.5F}\n 21\n{:15.5F}\n 31\n{:15.5F}\n");
    constexpr auto Format_704_3(" 12\n{:15.5F}\n 22\n{:15.5F}\n 32\n{:15.5F}\n");
    constexpr auto Format_705(" 13\n{:15.5F}\n 23\n{:15.5F}\n 33\n{:15.5F}\n");
    constexpr auto Format_706("  0\nENDSEC\n  0\nEOF\n");
    constexpr auto Format_709("  0\nCIRCLE\n  8\n{}\n 62\n{:3}\n 10\n{:15.5F}\n 20\n{:15.5F}\n 30\n{:15.5F}\n 40\n{:15.5F}\n");
    constexpr auto Format_710("999\n{}\n");

    if (PolygonAction == "TRIANGULATE3DFACE" || PolygonAction == "TRIANGULATE" || PolygonAction == "") {
        TriangulateFace = true;
        RegularPolyline = false;
        ThickPolyline = false;
    } else if (PolygonAction == "THICKPOLYLINE") {
        ThickPolyline = true;
        RegularPolyline = false;
        TriangulateFace = false;
    } else if (PolygonAction == "REGULARPOLYLINE") {
        RegularPolyline = true;
        TriangulateFace = false;
        ThickPolyline = false;
        PolylineWidth = " 0";
    } else {
        ShowWarningError(state, "DXFOut: Illegal key specified for Surfaces with > 4 sides=" + PolygonAction);
        ShowContinueError(state, "...Valid keys are: \"ThickPolyline\", \"RegularPolyline\", \"Triangulate3DFace\".");
        ShowContinueError(state, "\"Triangulate3DFace\" will be used for any surfaces with > 4 sides.");
        TriangulateFace = true;
        RegularPolyline = false;
        ThickPolyline = false;
    }

    if (state.dataSurface->TotSurfaces > 0 && !allocated(state.dataSurface->Surface)) {
        // no error needed, probably in end processing, just return
        return;
    }

    auto dxffile = state.files.dxf.open(state, "DXFOut", state.files.outputControl.dxf);

    print(dxffile, Format_702); // Start of Entities section

    print(dxffile, Format_707); // Comment

    print(dxffile, Format_708, "Program Version", ",", state.dataStrGlobals->VerStringVar);

    if (PolygonAction.empty()) {
        print(dxffile, Format_708, "Polygon Action", ",", "ThickPolyline");
    } else {
        print(dxffile, Format_708, "Polygon Action", ",", PolygonAction);
    }

    WriteDXFCommon(state, dxffile, ColorScheme);
    auto &DXFcolorno = state.dataSurfColor->DXFcolorno;
    auto colorindex = ColorNo::ShdDetFix;
    //  Do all detached shading surfaces first
    for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
        std::string ShadeType;

        if (state.dataSurface->Surface(surf).HeatTransSurf) continue;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Shading) continue;
        if (state.dataSurface->Surface(surf).Sides == 0) continue;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_F) colorindex = ColorNo::ShdDetFix;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_B) colorindex = ColorNo::ShdDetBldg;
        if (state.dataSurface->SurfIsPV(surf)) colorindex = ColorNo::PV;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_F) {
            ShadeType = "Fixed Shading";
            print(dxffile, Format_710, "Fixed Shading:" + state.dataSurface->Surface(surf).Name);
        } else if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_B) {
            ShadeType = "Building Shading";
            print(dxffile, Format_710, "Building Shading:" + state.dataSurface->Surface(surf).Name);
        }
        if (state.dataSurface->Surface(surf).Sides <= 4) {
            print(dxffile, Format_704_0, ShadeType, DXFcolorno(static_cast<int>(colorindex)));
            print(dxffile,
                  Format_704_1,
                  state.dataSurface->Surface(surf).Vertex(1).x,
                  state.dataSurface->Surface(surf).Vertex(1).y,
                  state.dataSurface->Surface(surf).Vertex(1).z);
            print(dxffile,
                  Format_704_2,
                  state.dataSurface->Surface(surf).Vertex(2).x,
                  state.dataSurface->Surface(surf).Vertex(2).y,
                  state.dataSurface->Surface(surf).Vertex(2).z);
            print(dxffile,
                  Format_704_3,
                  state.dataSurface->Surface(surf).Vertex(3).x,
                  state.dataSurface->Surface(surf).Vertex(3).y,
                  state.dataSurface->Surface(surf).Vertex(3).z);
            if (state.dataSurface->Surface(surf).Sides == 3) {
                print(dxffile,
                      Format_705,
                      state.dataSurface->Surface(surf).Vertex(3).x,
                      state.dataSurface->Surface(surf).Vertex(3).y,
                      state.dataSurface->Surface(surf).Vertex(3).z);
            } else {
                print(dxffile,
                      Format_705,
                      state.dataSurface->Surface(surf).Vertex(4).x,
                      state.dataSurface->Surface(surf).Vertex(4).y,
                      state.dataSurface->Surface(surf).Vertex(4).z);
            }
        } else { // polygon
            if (!TriangulateFace) {
                Real64 minz = 99999.0;
                for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                    minz = min(minz, state.dataSurface->Surface(surf).Vertex(vert).z);
                }
                print(dxffile, Format_715, ShadeType, DXFcolorno(static_cast<int>(colorindex)), minz, PolylineWidth, PolylineWidth);
                for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                    print(dxffile,
                          Format_716,
                          ShadeType,
                          state.dataSurface->Surface(surf).Vertex(vert).x,
                          state.dataSurface->Surface(surf).Vertex(vert).y,
                          state.dataSurface->Surface(surf).Vertex(vert).z);
                }
                print(dxffile, Format_717, ShadeType);
            } else {
                Array1D<dTriangle> mytriangles;

                const auto ntri = Triangulate(state,
                                              state.dataSurface->Surface(surf).Sides,
                                              state.dataSurface->Surface(surf).Vertex,
                                              mytriangles,
                                              state.dataSurface->Surface(surf).Azimuth,
                                              state.dataSurface->Surface(surf).Tilt,
                                              state.dataSurface->Surface(surf).Name,
                                              state.dataSurface->Surface(surf).Class);
                for (int svert = 1; svert <= ntri; ++svert) {
                    const auto vv0 = mytriangles(svert).vv0;
                    const auto vv1 = mytriangles(svert).vv1;
                    const auto vv2 = mytriangles(svert).vv2;
                    print(dxffile,
                          Format_704,
                          ShadeType,
                          DXFcolorno(static_cast<int>(colorindex)),
                          state.dataSurface->Surface(surf).Vertex(vv0).x,
                          state.dataSurface->Surface(surf).Vertex(vv0).y,
                          state.dataSurface->Surface(surf).Vertex(vv0).z,
                          state.dataSurface->Surface(surf).Vertex(vv1).x,
                          state.dataSurface->Surface(surf).Vertex(vv1).y,
                          state.dataSurface->Surface(surf).Vertex(vv1).z,
                          state.dataSurface->Surface(surf).Vertex(vv2).x,
                          state.dataSurface->Surface(surf).Vertex(vv2).y,
                          state.dataSurface->Surface(surf).Vertex(vv2).z);
                    print(dxffile,
                          Format_705,
                          state.dataSurface->Surface(surf).Vertex(vv2).x,
                          state.dataSurface->Surface(surf).Vertex(vv2).y,
                          state.dataSurface->Surface(surf).Vertex(vv2).z);
                }
                mytriangles.deallocate();
            }
        }
    }

    // now do zone surfaces, by zone
    for (int zones = 1; zones <= state.dataGlobal->NumOfZones; ++zones) {
        const auto TempZoneName = normalizeName(state.dataHeatBal->Zone(zones).Name);

        for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
            if (state.dataSurface->Surface(surf).Zone != zones) continue;
            if (state.dataSurface->Surface(surf).Sides == 0) continue;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::IntMass) continue;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Wall) colorindex = ColorNo::Wall;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Roof) colorindex = ColorNo::Roof;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Floor) colorindex = ColorNo::Floor;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Door) colorindex = ColorNo::Door;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Window) {
                if (state.dataSurface->SurfWinOriginalClass(surf) == SurfaceClass::Window) colorindex = ColorNo::Window;
                if (state.dataSurface->SurfWinOriginalClass(surf) == SurfaceClass::GlassDoor) colorindex = ColorNo::GlassDoor;
                if (state.dataSurface->SurfWinOriginalClass(surf) == SurfaceClass::TDD_Dome) colorindex = ColorNo::TDDDome;
                if (state.dataSurface->SurfWinOriginalClass(surf) == SurfaceClass::TDD_Diffuser) colorindex = ColorNo::TDDDiffuser;
            }
            if (state.dataSurface->SurfIsPV(surf)) colorindex = ColorNo::PV;

            print(dxffile, Format_710, state.dataSurface->Surface(surf).ZoneName + ':' + state.dataSurface->Surface(surf).Name);
            if (state.dataSurface->Surface(surf).Sides <= 4) {
                print(dxffile, Format_704_0, TempZoneName, DXFcolorno(static_cast<int>(colorindex)));
                print(dxffile,
                      Format_704_1,
                      state.dataSurface->Surface(surf).Vertex(1).x,
                      state.dataSurface->Surface(surf).Vertex(1).y,
                      state.dataSurface->Surface(surf).Vertex(1).z);
                print(dxffile,
                      Format_704_2,
                      state.dataSurface->Surface(surf).Vertex(2).x,
                      state.dataSurface->Surface(surf).Vertex(2).y,
                      state.dataSurface->Surface(surf).Vertex(2).z);
                print(dxffile,
                      Format_704_3,
                      state.dataSurface->Surface(surf).Vertex(3).x,
                      state.dataSurface->Surface(surf).Vertex(3).y,
                      state.dataSurface->Surface(surf).Vertex(3).z);
                if (state.dataSurface->Surface(surf).Sides == 3) {
                    print(dxffile,
                          Format_705,
                          state.dataSurface->Surface(surf).Vertex(3).x,
                          state.dataSurface->Surface(surf).Vertex(3).y,
                          state.dataSurface->Surface(surf).Vertex(3).z);
                } else {
                    print(dxffile,
                          Format_705,
                          state.dataSurface->Surface(surf).Vertex(4).x,
                          state.dataSurface->Surface(surf).Vertex(4).y,
                          state.dataSurface->Surface(surf).Vertex(4).z);
                }
            } else { // polygon surface
                if (!TriangulateFace) {
                    Real64 minz = 99999.0;
                    for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                        minz = min(minz, state.dataSurface->Surface(surf).Vertex(vert).z);
                    }
                    print(dxffile, Format_715, TempZoneName, DXFcolorno(static_cast<int>(colorindex)), minz, PolylineWidth, PolylineWidth);
                    for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                        print(dxffile,
                              Format_716,
                              TempZoneName,
                              state.dataSurface->Surface(surf).Vertex(vert).x,
                              state.dataSurface->Surface(surf).Vertex(vert).y,
                              state.dataSurface->Surface(surf).Vertex(vert).z);
                    }
                    print(dxffile, Format_717, TempZoneName);
                } else {
                    Array1D<dTriangle> mytriangles;

                    const auto ntri = Triangulate(state,
                                                  state.dataSurface->Surface(surf).Sides,
                                                  state.dataSurface->Surface(surf).Vertex,
                                                  mytriangles,
                                                  state.dataSurface->Surface(surf).Azimuth,
                                                  state.dataSurface->Surface(surf).Tilt,
                                                  state.dataSurface->Surface(surf).Name,
                                                  state.dataSurface->Surface(surf).Class);
                    for (int svert = 1; svert <= ntri; ++svert) {
                        const auto vv0 = mytriangles(svert).vv0;
                        const auto vv1 = mytriangles(svert).vv1;
                        const auto vv2 = mytriangles(svert).vv2;
                        print(dxffile,
                              Format_704,
                              TempZoneName,
                              DXFcolorno(static_cast<int>(colorindex)),
                              state.dataSurface->Surface(surf).Vertex(vv0).x,
                              state.dataSurface->Surface(surf).Vertex(vv0).y,
                              state.dataSurface->Surface(surf).Vertex(vv0).z,
                              state.dataSurface->Surface(surf).Vertex(vv1).x,
                              state.dataSurface->Surface(surf).Vertex(vv1).y,
                              state.dataSurface->Surface(surf).Vertex(vv1).z,
                              state.dataSurface->Surface(surf).Vertex(vv2).x,
                              state.dataSurface->Surface(surf).Vertex(vv2).y,
                              state.dataSurface->Surface(surf).Vertex(vv2).z);
                        print(dxffile,
                              Format_705,
                              state.dataSurface->Surface(surf).Vertex(vv2).x,
                              state.dataSurface->Surface(surf).Vertex(vv2).y,
                              state.dataSurface->Surface(surf).Vertex(vv2).z);
                    }
                    mytriangles.deallocate();
                }
            }
        }
        // still have to do shading surfaces for zone
        for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
            // if (surface(surf)%heattranssurf) CYCLE ! Shading with a construction is allowed to be HT surf for daylighting shelves
            if (state.dataSurface->Surface(surf).Class != SurfaceClass::Shading) continue;
            if (state.dataSurface->Surface(surf).ZoneName != state.dataHeatBal->Zone(zones).Name) continue;
            if (state.dataSurface->Surface(surf).Sides == 0) continue;
            colorindex = ColorNo::ShdAtt;
            if (state.dataSurface->SurfIsPV(surf)) colorindex = ColorNo::PV;
            print(dxffile, Format_710, state.dataSurface->Surface(surf).ZoneName + ':' + state.dataSurface->Surface(surf).Name);
            if (state.dataSurface->Surface(surf).Sides <= 4) {
                print(dxffile, Format_704_0, TempZoneName, DXFcolorno(static_cast<int>(colorindex)));
                print(dxffile,
                      Format_704_1,
                      state.dataSurface->Surface(surf).Vertex(1).x,
                      state.dataSurface->Surface(surf).Vertex(1).y,
                      state.dataSurface->Surface(surf).Vertex(1).z);
                print(dxffile,
                      Format_704_2,
                      state.dataSurface->Surface(surf).Vertex(2).x,
                      state.dataSurface->Surface(surf).Vertex(2).y,
                      state.dataSurface->Surface(surf).Vertex(2).z);
                print(dxffile,
                      Format_704_3,
                      state.dataSurface->Surface(surf).Vertex(3).x,
                      state.dataSurface->Surface(surf).Vertex(3).y,
                      state.dataSurface->Surface(surf).Vertex(3).z);
                if (state.dataSurface->Surface(surf).Sides == 3) {
                    print(dxffile,
                          Format_705,
                          state.dataSurface->Surface(surf).Vertex(3).x,
                          state.dataSurface->Surface(surf).Vertex(3).y,
                          state.dataSurface->Surface(surf).Vertex(3).z);
                } else {
                    print(dxffile,
                          Format_705,
                          state.dataSurface->Surface(surf).Vertex(4).x,
                          state.dataSurface->Surface(surf).Vertex(4).y,
                          state.dataSurface->Surface(surf).Vertex(4).z);
                }
            } else { // polygon attached shading
                if (!TriangulateFace) {
                    Real64 minz = 99999.0;
                    for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                        minz = min(minz, state.dataSurface->Surface(surf).Vertex(vert).z);
                    }
                    print(dxffile, Format_715, TempZoneName, DXFcolorno(static_cast<int>(colorindex)), minz, PolylineWidth, PolylineWidth);
                    for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                        print(dxffile,
                              Format_716,
                              TempZoneName,
                              state.dataSurface->Surface(surf).Vertex(vert).x,
                              state.dataSurface->Surface(surf).Vertex(vert).y,
                              state.dataSurface->Surface(surf).Vertex(vert).z);
                    }
                    print(dxffile, Format_717, TempZoneName);
                } else {
                    Array1D<dTriangle> mytriangles;
                    int ntri = 0;
                    if (state.dataSurface->Surface(surf).Shape == SurfaceShape::RectangularOverhang) {
                        ntri = Triangulate(state,
                                           state.dataSurface->Surface(surf).Sides,
                                           state.dataSurface->Surface(surf).Vertex,
                                           mytriangles,
                                           state.dataSurface->Surface(surf).Azimuth,
                                           state.dataSurface->Surface(surf).Tilt,
                                           state.dataSurface->Surface(surf).Name,
                                           SurfaceClass::Overhang);
                    } else {
                        ntri = Triangulate(state,
                                           state.dataSurface->Surface(surf).Sides,
                                           state.dataSurface->Surface(surf).Vertex,
                                           mytriangles,
                                           state.dataSurface->Surface(surf).Azimuth,
                                           state.dataSurface->Surface(surf).Tilt,
                                           state.dataSurface->Surface(surf).Name,
                                           SurfaceClass::Fin);
                    }
                    for (int svert = 1; svert <= ntri; ++svert) {
                        const auto vv0 = mytriangles(svert).vv0;
                        const auto vv1 = mytriangles(svert).vv1;
                        const auto vv2 = mytriangles(svert).vv2;
                        print(dxffile,
                              Format_704,
                              TempZoneName,
                              DXFcolorno(static_cast<int>(colorindex)),
                              state.dataSurface->Surface(surf).Vertex(vv0).x,
                              state.dataSurface->Surface(surf).Vertex(vv0).y,
                              state.dataSurface->Surface(surf).Vertex(vv0).z,
                              state.dataSurface->Surface(surf).Vertex(vv1).x,
                              state.dataSurface->Surface(surf).Vertex(vv1).y,
                              state.dataSurface->Surface(surf).Vertex(vv1).z,
                              state.dataSurface->Surface(surf).Vertex(vv2).x,
                              state.dataSurface->Surface(surf).Vertex(vv2).y,
                              state.dataSurface->Surface(surf).Vertex(vv2).z);
                        print(dxffile,
                              Format_705,
                              state.dataSurface->Surface(surf).Vertex(vv2).x,
                              state.dataSurface->Surface(surf).Vertex(vv2).y,
                              state.dataSurface->Surface(surf).Vertex(vv2).z);
                    }
                    mytriangles.deallocate();
                }
            }
        }
    }

    //  711 format('  0',/,'LINE',/,'  8',/,A,/,' 62',/,I3)
    //  712 format(' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    //             ' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5)

    DXFDaylightingReferencePoints(state, dxffile, false);

    for (int zones = 1; zones <= state.dataGlobal->NumOfZones; ++zones) {
        const auto curcolorno = ColorNo::DaylSensor1;

        for (int mapnum = 1; mapnum <= state.dataDaylightingData->TotIllumMaps; ++mapnum) {
            if (state.dataDaylightingData->IllumMapCalc(mapnum).Zone != zones) continue;
            for (int refpt = 1; refpt <= state.dataDaylightingData->IllumMapCalc(mapnum).TotalMapRefPoints; ++refpt) {
                print(dxffile, Format_710, format("{}:MapRefPt:{}", state.dataHeatBal->Zone(zones).Name, refpt));
                print(dxffile,
                      Format_709,
                      normalizeName(state.dataHeatBal->Zone(zones).Name),
                      DXFcolorno(static_cast<int>(curcolorno)),
                      state.dataDaylightingData->IllumMapCalc(mapnum).MapRefPtAbsCoord(1, refpt),
                      state.dataDaylightingData->IllumMapCalc(mapnum).MapRefPtAbsCoord(2, refpt),
                      state.dataDaylightingData->IllumMapCalc(mapnum).MapRefPtAbsCoord(3, refpt),
                      0.05);
            }
        }
    }

    DXFDaylightingReferencePoints(state, dxffile, true);

    print(dxffile, Format_706);
}

void DXFOutLines(EnergyPlusData &state, std::string const &ColorScheme)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   August 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine produces a points file of lines in the surfaces.

    // METHODOLOGY EMPLOYED:
    // Use the surface absolute coordinate information to produce
    // lines.

    // Using/Aliasing
    using namespace DataSurfaces;
    using namespace DataSurfaceColors;

    // Formats
    constexpr auto Format_702("  0\nSECTION\n  2\nENTITIES\n");
    constexpr auto Format_707("999\nDXF created from EnergyPlus\n");
    constexpr auto Format_708("999\n{}{}{}\n");

    constexpr auto Format_706("  0\nENDSEC\n  0\nEOF\n");
    constexpr auto Format_710("999\n{}\n");

    constexpr auto Format_711("  0\nLINE\n  8\n{}\n 62\n{:3}\n");
    constexpr auto Format_712(" 10\n{:15.5F}\n 20\n{:15.5F}\n 30\n{:15.5F}\n 11\n{:15.5F}\n 21\n{:15.5F}\n 31\n{:15.5F}\n");

    if (state.dataSurface->TotSurfaces > 0 && !allocated(state.dataSurface->Surface)) {
        // no error needed, probably in end processing, just return
        return;
    }

    auto dxffile = state.files.dxf.open(state, "DXFOutLines", state.files.outputControl.dxf);

    print(dxffile, Format_702); // Start of Entities section

    print(dxffile, Format_707); // Comment

    print(dxffile, Format_708, "Program Version", ",", state.dataStrGlobals->VerStringVar);

    print(dxffile, Format_708, "DXF using Lines", ' ', ' ');

    WriteDXFCommon(state, dxffile, ColorScheme);

    //  Do all detached shading surfaces first
    int surfcount = 0;
    ColorNo colorindex = ColorNo::Unassigned;
    for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
        std::string ShadeType;
        if (state.dataSurface->Surface(surf).HeatTransSurf) continue;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Shading) continue;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_F) colorindex = ColorNo::ShdDetFix;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_B) colorindex = ColorNo::ShdDetBldg;
        if (state.dataSurface->SurfIsPV(surf)) colorindex = ColorNo::PV;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_F) {
            ShadeType = "Fixed Shading";
            print(dxffile, Format_710, "Fixed Shading:" + state.dataSurface->Surface(surf).Name);
        } else if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_B) {
            ShadeType = "Building Shading";
            print(dxffile, Format_710, "Building Shading:" + state.dataSurface->Surface(surf).Name);
        }
        ++surfcount;
        ShadeType += format("_{}", surfcount);
        Real64 minz = 99999.0;
        for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
            minz = min(minz, state.dataSurface->Surface(surf).Vertex(vert).z);
        }
        //      write(unit,711) TRIM(ShadeType),colorno(colorindex) !,minz ,TRIM(PolylineWidth),TRIM(PolylineWidth)
        for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
            int sptr = 0;
            if (vert != state.dataSurface->Surface(surf).Sides) {
                sptr = vert + 1;
            } else {
                sptr = 1;
            }
            print(dxffile,
                  Format_711,
                  ShadeType,
                  state.dataSurfColor->DXFcolorno(static_cast<int>(colorindex))); //,minz ,TRIM(PolylineWidth),TRIM(PolylineWidth)
            print(dxffile,
                  Format_712,
                  state.dataSurface->Surface(surf).Vertex(vert).x,
                  state.dataSurface->Surface(surf).Vertex(vert).y,
                  state.dataSurface->Surface(surf).Vertex(vert).z,
                  state.dataSurface->Surface(surf).Vertex(sptr).x,
                  state.dataSurface->Surface(surf).Vertex(sptr).y,
                  state.dataSurface->Surface(surf).Vertex(sptr).z);
        }
    }

    // now do zone surfaces, by zone
    for (int zones = 1; zones <= state.dataGlobal->NumOfZones; ++zones) {
        auto TempZoneName = normalizeName(state.dataHeatBal->Zone(zones).Name);

        surfcount = 0;
        for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
            if (state.dataSurface->Surface(surf).Zone != zones) continue;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::IntMass) continue;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Wall) colorindex = ColorNo::Wall;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Roof) colorindex = ColorNo::Roof;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Floor) colorindex = ColorNo::Floor;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Door) colorindex = ColorNo::Door;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Window) {
                if (state.dataSurface->SurfWinOriginalClass(surf) == SurfaceClass::Window) colorindex = ColorNo::Window;
                if (state.dataSurface->SurfWinOriginalClass(surf) == SurfaceClass::GlassDoor) colorindex = ColorNo::GlassDoor;
                if (state.dataSurface->SurfWinOriginalClass(surf) == SurfaceClass::TDD_Dome) colorindex = ColorNo::TDDDome;
                if (state.dataSurface->SurfWinOriginalClass(surf) == SurfaceClass::TDD_Diffuser) colorindex = ColorNo::TDDDiffuser;
            }
            if (state.dataSurface->SurfIsPV(surf)) colorindex = ColorNo::PV;
            ++surfcount;
            ++surfcount;

            print(dxffile, Format_710, state.dataSurface->Surface(surf).ZoneName + ':' + state.dataSurface->Surface(surf).Name);
            TempZoneName += format("_{}", surfcount);
            Real64 minz = 99999.0;
            for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                minz = min(minz, state.dataSurface->Surface(surf).Vertex(vert).z);
            }

            for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                int sptr = 0;
                if (vert != state.dataSurface->Surface(surf).Sides) {
                    sptr = vert + 1;
                } else {
                    sptr = 1;
                }
                print(dxffile,
                      Format_711,
                      TempZoneName,
                      state.dataSurfColor->DXFcolorno(static_cast<int>(colorindex))); //,minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
                print(dxffile,
                      Format_712,
                      state.dataSurface->Surface(surf).Vertex(vert).x,
                      state.dataSurface->Surface(surf).Vertex(vert).y,
                      state.dataSurface->Surface(surf).Vertex(vert).z,
                      state.dataSurface->Surface(surf).Vertex(sptr).x,
                      state.dataSurface->Surface(surf).Vertex(sptr).y,
                      state.dataSurface->Surface(surf).Vertex(sptr).z);
            }

            // 715 format('  0',/,'POLYLINE',/,'  8',/,A,/,' 62',/,I3,/,' 66',/,'  1',/,  &
            //    ' 10',/,' 0.0',/,' 20',/,' 0.0',/,' 30',/,f15.5,/,  &
            //    ' 70',/,'   1',/,' 40',/,A,/,' 41',/,A)
            // 716 format('  0',/'VERTEX',/,'  8',/,A,/,  &
            //    ' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5)
            // 717 format('  0',/'SEQEND',/,'  8',/,A)
        }
        // still have to do shading surfaces for zone
        surfcount = 0;
        for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
            // if (surface(surf)%heattranssurf) CYCLE ! Shading with a construction is allowed to be HT surf for daylighting shelves
            if (state.dataSurface->Surface(surf).Class != SurfaceClass::Shading) continue;
            if (state.dataSurface->Surface(surf).ZoneName != state.dataHeatBal->Zone(zones).Name) continue;
            colorindex = ColorNo::ShdAtt;
            if (state.dataSurface->SurfIsPV(surf)) colorindex = ColorNo::PV;
            ++surfcount;

            print(dxffile, Format_710, state.dataSurface->Surface(surf).ZoneName + ':' + state.dataSurface->Surface(surf).Name);
            TempZoneName += format("_{}", surfcount);
            Real64 minz = 99999.0;
            for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                minz = min(minz, state.dataSurface->Surface(surf).Vertex(vert).z);
            }

            for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                int sptr = 0;
                if (vert != state.dataSurface->Surface(surf).Sides) {
                    sptr = vert + 1;
                } else {
                    sptr = 1;
                }
                print(dxffile,
                      Format_711,
                      TempZoneName,
                      state.dataSurfColor->DXFcolorno(static_cast<int>(colorindex))); //,minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
                print(dxffile,
                      Format_712,
                      state.dataSurface->Surface(surf).Vertex(vert).x,
                      state.dataSurface->Surface(surf).Vertex(vert).y,
                      state.dataSurface->Surface(surf).Vertex(vert).z,
                      state.dataSurface->Surface(surf).Vertex(sptr).x,
                      state.dataSurface->Surface(surf).Vertex(sptr).y,
                      state.dataSurface->Surface(surf).Vertex(sptr).z);
            }
        }
    }

    DXFDaylightingReferencePoints(state, dxffile, false);
    DXFDaylightingReferencePoints(state, dxffile, true);

    print(dxffile, Format_706);
}

void DXFOutWireFrame(EnergyPlusData &state, std::string const &ColorScheme)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   August 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine produces a file of DXF objects for the surfaces (all lines -- wireframe).

    // METHODOLOGY EMPLOYED:
    // Use the surface absolute coordinate information to produce
    // lines.

    // Using/Aliasing
    using namespace DataSurfaces;
    using namespace DataSurfaceColors;

    std::string const PolylineWidth(" 0.55");

    constexpr auto Format_702("  0\nSECTION\n  2\nENTITIES\n");
    constexpr auto Format_707("999\nDXF created from EnergyPlus\n");
    constexpr auto Format_708("999\n{}{}{}\n");

    constexpr auto Format_715("  0\nPOLYLINE\n  8\n{}\n 62\n{:3}\n 66\n  1\n 10\n 0.0\n 20\n 0.0\n 30\n{:15.5F}\n 70\n   9\n 40\n{}\n 41\n{}\n");
    constexpr auto Format_716("  0\nVERTEX\n  8\n{}\n 10\n{:15.5F}\n 20\n{:15.5F}\n 30\n{:15.5F}\n");
    constexpr auto Format_717("  0\nSEQEND\n  8\n{}\n");
    constexpr auto Format_706("  0\nENDSEC\n  0\nEOF\n");
    constexpr auto Format_710("999\n{}\n");

    if (state.dataSurface->TotSurfaces > 0 && !allocated(state.dataSurface->Surface)) {
        // no error needed, probably in end processing, just return
        return;
    }

    auto dxffile = state.files.dxf.open(state, "DXFOutWireFrame", state.files.outputControl.dxf);

    print(dxffile, Format_702); // Start of Entities section

    print(dxffile, Format_707); // Comment

    print(dxffile, Format_708, "Program Version", ",", state.dataStrGlobals->VerStringVar);
    print(dxffile, Format_708, "DXF using Wireframe", ' ', ' ');

    WriteDXFCommon(state, dxffile, ColorScheme);

    //  Do all detached shading surfaces first
    int surfcount = 0;
    ColorNo colorindex = ColorNo::Unassigned;
    for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
        std::string ShadeType;

        if (state.dataSurface->Surface(surf).HeatTransSurf) continue;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Shading) continue;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_F) colorindex = ColorNo::ShdDetFix;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_B) colorindex = ColorNo::ShdDetBldg;
        if (state.dataSurface->SurfIsPV(surf)) colorindex = ColorNo::PV;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_F) {
            ShadeType = "Fixed Shading";
            print(dxffile, Format_710, "Fixed Shading:" + state.dataSurface->Surface(surf).Name);
        } else if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_B) {
            ShadeType = "Building Shading";
            print(dxffile, Format_710, "Building Shading:" + state.dataSurface->Surface(surf).Name);
        }
        ++surfcount;
        ShadeType += format("_{}", surfcount);
        Real64 minz = 99999.0;
        for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
            minz = min(minz, state.dataSurface->Surface(surf).Vertex(vert).z);
        }

        print(dxffile, Format_715, ShadeType, state.dataSurfColor->DXFcolorno(static_cast<int>(colorindex)), minz, PolylineWidth, PolylineWidth);
        for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
            print(dxffile,
                  Format_716,
                  ShadeType,
                  state.dataSurface->Surface(surf).Vertex(vert).x,
                  state.dataSurface->Surface(surf).Vertex(vert).y,
                  state.dataSurface->Surface(surf).Vertex(vert).z);
        }
        print(dxffile, Format_717, ShadeType);
    }

    // now do zone surfaces, by zone
    for (int zones = 1; zones <= state.dataGlobal->NumOfZones; ++zones) {
        const auto SaveZoneName = normalizeName(state.dataHeatBal->Zone(zones).Name);

        surfcount = 0;
        for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
            if (state.dataSurface->Surface(surf).Zone != zones) continue;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::IntMass) continue;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Wall) colorindex = ColorNo::Wall;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Roof) colorindex = ColorNo::Roof;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Floor) colorindex = ColorNo::Floor;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Door) colorindex = ColorNo::Door;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Window) {
                if (state.dataSurface->SurfWinOriginalClass(surf) == SurfaceClass::Window) colorindex = ColorNo::Window;
                if (state.dataSurface->SurfWinOriginalClass(surf) == SurfaceClass::GlassDoor) colorindex = ColorNo::GlassDoor;
                if (state.dataSurface->SurfWinOriginalClass(surf) == SurfaceClass::TDD_Dome) colorindex = ColorNo::TDDDome;
                if (state.dataSurface->SurfWinOriginalClass(surf) == SurfaceClass::TDD_Diffuser) colorindex = ColorNo::TDDDiffuser;
            }
            if (state.dataSurface->SurfIsPV(surf)) colorindex = ColorNo::PV;
            ++surfcount;

            print(dxffile, Format_710, state.dataSurface->Surface(surf).ZoneName + ':' + state.dataSurface->Surface(surf).Name);
            const auto TempZoneName = SaveZoneName + '_' + fmt::to_string(surfcount);
            Real64 minz = 99999.0;
            for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                minz = min(minz, state.dataSurface->Surface(surf).Vertex(vert).z);
            }

            print(
                dxffile, Format_715, TempZoneName, state.dataSurfColor->DXFcolorno(static_cast<int>(colorindex)), minz, PolylineWidth, PolylineWidth);
            for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                print(dxffile,
                      Format_716,
                      TempZoneName,
                      state.dataSurface->Surface(surf).Vertex(vert).x,
                      state.dataSurface->Surface(surf).Vertex(vert).y,
                      state.dataSurface->Surface(surf).Vertex(vert).z);
            }
            print(dxffile, Format_717, TempZoneName);
        }
        // still have to do shading surfaces for zone
        surfcount = 0;
        for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
            // if (surface(surf)%heattranssurf) CYCLE ! Shading with a construction is allowed to be HT surf for daylighting shelves
            if (state.dataSurface->Surface(surf).Class != SurfaceClass::Shading) continue;
            if (state.dataSurface->Surface(surf).ZoneName != state.dataHeatBal->Zone(zones).Name) continue;
            colorindex = ColorNo::ShdAtt;
            if (state.dataSurface->SurfIsPV(surf)) colorindex = ColorNo::PV;
            ++surfcount;

            print(dxffile, Format_710, state.dataSurface->Surface(surf).ZoneName + ':' + state.dataSurface->Surface(surf).Name);
            const auto TempZoneName = SaveZoneName + '_' + fmt::to_string(surfcount);
            Real64 minz = 99999.0;
            for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                minz = min(minz, state.dataSurface->Surface(surf).Vertex(vert).z);
            }

            print(
                dxffile, Format_715, TempZoneName, state.dataSurfColor->DXFcolorno(static_cast<int>(colorindex)), minz, PolylineWidth, PolylineWidth);
            for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                print(dxffile,
                      Format_716,
                      TempZoneName,
                      state.dataSurface->Surface(surf).Vertex(vert).x,
                      state.dataSurface->Surface(surf).Vertex(vert).y,
                      state.dataSurface->Surface(surf).Vertex(vert).z);
            }
            print(dxffile, Format_717, TempZoneName);
        }
    }

    //  711 format('  0',/,'LINE',/,'  8',/,A,/,' 62',/,I3)
    //  712 format(' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    //             ' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5)

    DXFDaylightingReferencePoints(state, dxffile, false);
    DXFDaylightingReferencePoints(state, dxffile, true);

    print(dxffile, Format_706);
}

void DetailsForSurfaces(EnergyPlusData &state, int const RptType) // (1=Vertices only, 10=Details only, 11=Details with vertices)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   February 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine provides an optional detailed surface report
    // for each surface in the input file.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace DataHeatBalance;
    using namespace DataSurfaces;
    using ScheduleManager::GetScheduleMaxValue;
    using ScheduleManager::GetScheduleMinValue;
    using ScheduleManager::GetScheduleName;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    static Array1D_string const ConvCoeffCalcs(
        {1, 9}, {"ASHRAESimple", "ASHRAETARP", "CeilingDiffuser", "TrombeWall", "TARP", "MoWitt", "DOE-2", "BLAST", "AdaptiveConvectionAlgorithm"});

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int vert;    // Loop counter
    int ZoneNum; // Loop counter
    std::string BaseSurfName;
    std::string ConstructionName;
    std::string ScheduleName;
    std::string IntConvCoeffCalc;
    std::string ExtConvCoeffCalc;
    Real64 NominalUwithConvCoeffs;
    std::string cNominalU;
    std::string cNominalUwithConvCoeffs;
    std::string cSchedMin;
    std::string cSchedMax;
    std::string SolarDiffusing;
    int fd;
    std::string AlgoName;

    if (state.dataSurface->TotSurfaces > 0 && !allocated(state.dataSurface->Surface)) {
        // no error needed, probably in end processing, just return
        return;
    }

    std::stringstream ss;
    auto *eiostream = &ss;
    //!!!    Write Header lines for report
    if (RptType == 10) {                                                                                          // Details only
        *eiostream << "! <Zone Surfaces>,Zone Name,# Surfaces\n";                                                 // Format_700
        *eiostream << "! <Shading Surfaces>,Number of Shading Surfaces,# Surfaces\n";                             // Format_700b
        *eiostream << "! <HeatTransfer Surface>,Surface Name,Surface Class,Base Surface,Heat Transfer Algorithm"; // Format_701
        *eiostream << ",Construction,Nominal U (w/o film coefs) {W/m2-K},Nominal U (with film coefs) {W/m2-K},Solar Diffusing,Area (Net) {m2},Area "
                      "(Gross) {m2},Area (Sunlit Calc) {m2},Azimuth {deg},Tilt {deg},~Width {m},~Height {m},Reveal "
                      "{m},ExtBoundCondition,ExtConvCoeffCalc,IntConvCoeffCalc,SunExposure,WindExposure,ViewFactorToGround,ViewFactorToSky,"
                      "ViewFactorToGround-IR,ViewFactorToSky-IR,#Sides\n";                                   // Format_7011
        *eiostream << "! <Shading Surface>,Surface Name,Surface Class,Base Surface,Heat Transfer Algorithm"; // Format_701b
        *eiostream << ",Transmittance Schedule,Min Schedule Value,Max Schedule Value,Solar Diffusing,Area (Net) {m2},Area (Gross) {m2},Area (Sunlit "
                      "Calc) {m2},Azimuth {deg},Tilt {deg},~Width {m},~Height {m},Reveal "
                      "{m},ExtBoundCondition,ExtConvCoeffCalc,IntConvCoeffCalc,SunExposure,WindExposure,ViewFactorToGround,ViewFactorToSky,"
                      "ViewFactorToGround-IR,ViewFactorToSky-IR,#Sides\n";                                         // Format_7011b
        *eiostream << "! <Frame/Divider Surface>,Surface Name,Surface Class,Base Surface,Heat Transfer Algorithm"; // Format_701c
        *eiostream << ",Construction,Nominal U (w/o film coefs) {W/m2-K},Nominal U (with film coefs) {W/m2-K},Solar Diffusing,Area (Net) {m2},Area "
                      "(Gross) {m2},Area (Sunlit Calc) {m2},Azimuth {deg},Tilt {deg},~Width {m},~Height {m},Reveal {m}\n"; // Format_7011c
    } else if (RptType == 11) {                                                                                            // Details with Vertices
        *eiostream << "! <Zone Surfaces>,Zone Name,# Surfaces";                                                            // Format_700
        *eiostream << ", Vertices are shown starting at Upper-Left-Corner => Counter-Clockwise => World Coordinates\n";    // Format_710
        *eiostream << "! <Shading Surfaces>,Number of Shading Surfaces,# Surfaces";                                        // Format_700b
        *eiostream << ", Vertices are shown starting at Upper-Left-Corner => Counter-Clockwise => World Coordinates\n";    // Format_710
        *eiostream << "! <HeatTransfer Surface>,Surface Name,Surface Class,Base Surface,Heat Transfer Algorithm";          // Format_701
        *eiostream << ",Construction,Nominal U (w/o film coefs) {W/m2-K},Nominal U (with film coefs) {W/m2-K},Solar Diffusing,Area (Net) {m2},Area "
                      "(Gross) {m2},Area (Sunlit Calc) {m2},Azimuth {deg},Tilt {deg},~Width {m},~Height {m},Reveal "
                      "{m},ExtBoundCondition,ExtConvCoeffCalc,IntConvCoeffCalc,SunExposure,WindExposure,ViewFactorToGround,ViewFactorToSky,"
                      "ViewFactorToGround-IR,ViewFactorToSky-IR,#Sides"; // Format_7011
        *eiostream << ",Vertex 1 X {m},Vertex 1 Y {m},Vertex 1 Z {m},Vertex 2 X {m},Vertex 2 Y {m},Vertex 2 Z {m},Vertex 3 X {m},Vertex 3 Y "
                      "{m},Vertex 3 Z {m},Vertex 4 X {m},Vertex 4 Z {m},Vertex 4 Z {m},{etc}\n";             // Format_707
        *eiostream << "! <Shading Surface>,Surface Name,Surface Class,Base Surface,Heat Transfer Algorithm"; // Format_701b
        *eiostream << ",Transmittance Schedule,Min Schedule Value,Max Schedule Value,Solar Diffusing,Area (Net) {m2},Area (Gross) {m2},Area (Sunlit "
                      "Calc) {m2},Azimuth {deg},Tilt {deg},~Width {m},~Height {m},Reveal "
                      "{m},ExtBoundCondition,ExtConvCoeffCalc,IntConvCoeffCalc,SunExposure,WindExposure,ViewFactorToGround,ViewFactorToSky,"
                      "ViewFactorToGround-IR,ViewFactorToSky-IR,#Sides"; // Format_7011b
        *eiostream << ",Vertex 1 X {m},Vertex 1 Y {m},Vertex 1 Z {m},Vertex 2 X {m},Vertex 2 Y {m},Vertex 2 Z {m},Vertex 3 X {m},Vertex 3 Y "
                      "{m},Vertex 3 Z {m},Vertex 4 X {m},Vertex 4 Z {m},Vertex 4 Z {m},{etc}\n";                   // Format_707
        *eiostream << "! <Frame/Divider Surface>,Surface Name,Surface Class,Base Surface,Heat Transfer Algorithm"; // Format_701c
        // Vertices are not applicable for window frame and divider, so skip 707
        *eiostream << ",Construction,Nominal U (w/o film coefs) {W/m2-K},Nominal U (with film coefs) {W/m2-K},Solar Diffusing,Area (Net) {m2},Area "
                      "(Gross) {m2},Area (Sunlit Calc) {m2},Azimuth {deg},Tilt {deg},~Width {m},~Height {m},Reveal {m}\n"; // Format_7011c
    } else {                                                                                                               // Vertices only
        *eiostream << "! <Zone Surfaces>,Zone Name,# Surfaces";                                                            // Format_700
        *eiostream << ", Vertices are shown starting at Upper-Left-Corner => Counter-Clockwise => World Coordinates\n";    // Format_710
        *eiostream << "! <Shading Surfaces>,Number of Shading Surfaces,# Surfaces";                                        // Format_700b
        *eiostream << ", Vertices are shown starting at Upper-Left-Corner => Counter-Clockwise => World Coordinates\n";    // Format_710
        *eiostream << "! <HeatTransfer Surface>,Surface Name,Surface Class,Base Surface,Heat Transfer Algorithm";          // Format_701
        *eiostream << ",#Sides";                                                                                           // Format_7012
        *eiostream << ",Vertex 1 X {m},Vertex 1 Y {m},Vertex 1 Z {m},Vertex 2 X {m},Vertex 2 Y {m},Vertex 2 Z {m},Vertex 3 X {m},Vertex 3 Y "
                      "{m},Vertex 3 Z {m},Vertex 4 X {m},Vertex 4 Z {m},Vertex 4 Z {m},{etc}\n";             // Format_707
        *eiostream << "! <Shading Surface>,Surface Name,Surface Class,Base Surface,Heat Transfer Algorithm"; // Format_701b
        *eiostream << ",#Sides";                                                                             // Format_7012
        *eiostream << ",Vertex 1 X {m},Vertex 1 Y {m},Vertex 1 Z {m},Vertex 2 X {m},Vertex 2 Y {m},Vertex 2 Z {m},Vertex 3 X {m},Vertex 3 Y "
                      "{m},Vertex 3 Z {m},Vertex 4 X {m},Vertex 4 Z {m},Vertex 4 Z {m},{etc}\n"; // Format_707
        // Vertices are not applicable for window frame and divider, so skip 701c here
    }

    // Do just "detached" shading first
    int surf2 = 0;
    for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
        surf2 = surf;
        if (state.dataSurface->Surface(surf).Zone != 0) break;
    }
    if ((surf2 - 1) > 0) {
        *eiostream << "Shading Surfaces,"
                   << "Number of Shading Surfaces," << surf2 - 1 << '\n';
        for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
            if (state.dataSurface->Surface(surf).Zone != 0) break;
            AlgoName = "None";
            *eiostream << "Shading Surface," << state.dataSurface->Surface(surf).Name << "," << cSurfaceClass(state.dataSurface->Surface(surf).Class)
                       << "," << state.dataSurface->Surface(surf).BaseSurfName << "," << AlgoName << ",";
            if (RptType == 10) {
                if (state.dataSurface->Surface(surf).SchedShadowSurfIndex > 0) {
                    ScheduleName = GetScheduleName(state, state.dataSurface->Surface(surf).SchedShadowSurfIndex);
                    cSchedMin = format("{:.2R}", GetScheduleMinValue(state, state.dataSurface->Surface(surf).SchedShadowSurfIndex));
                    cSchedMax = format("{:.2R}", GetScheduleMaxValue(state, state.dataSurface->Surface(surf).SchedShadowSurfIndex));
                } else {
                    ScheduleName = "";
                    cSchedMin = "0.0";
                    cSchedMax = "0.0";
                }
                *eiostream << ScheduleName << "," << cSchedMin << "," << cSchedMax << "," << ' ' << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Area) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).GrossArea) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).NetAreaShadowCalc) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Azimuth) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Tilt) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Width) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Height) << ",";
                *eiostream << ",,,,,,,,,," << fmt::to_string(state.dataSurface->Surface(surf).Sides) << '\n';
            } else if (RptType == 1) {
                *eiostream << fmt::to_string(state.dataSurface->Surface(surf).Sides) << ",";
            } else {
                if (state.dataSurface->Surface(surf).SchedShadowSurfIndex > 0) {
                    ScheduleName = GetScheduleName(state, state.dataSurface->Surface(surf).SchedShadowSurfIndex);
                    cSchedMin = format("{:.2R}", GetScheduleMinValue(state, state.dataSurface->Surface(surf).SchedShadowSurfIndex));
                    cSchedMax = format("{:.2R}", GetScheduleMaxValue(state, state.dataSurface->Surface(surf).SchedShadowSurfIndex));
                } else {
                    ScheduleName = "";
                    cSchedMin = "0.0";
                    cSchedMax = "0.0";
                }
                *eiostream << ScheduleName << "," << cSchedMin << "," << cSchedMax << "," << ' ' << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Area) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).GrossArea) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).NetAreaShadowCalc) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Azimuth) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Tilt) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Width) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Height) << ",";
                *eiostream << ",,,,,,,,,," << fmt::to_string(state.dataSurface->Surface(surf).Sides) << ",";
            }
            if (RptType == 10) continue;
            for (vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                if (vert != state.dataSurface->Surface(surf).Sides) {
                    *eiostream << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).x) << ","
                               << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).y) << ","
                               << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).z) << ",";
                } else {
                    *eiostream << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).x) << ","
                               << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).y) << ","
                               << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).z) << '\n';
                }
            }
            //  This shouldn't happen with shading surface -- always have vertices
            if (state.dataSurface->Surface(surf).Sides == 0) *eiostream << '\n';
        }
    }

    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
        *eiostream << "Zone Surfaces," << state.dataHeatBal->Zone(ZoneNum).Name << ","
                   << (state.dataHeatBal->Zone(ZoneNum).AllSurfaceLast - state.dataHeatBal->Zone(ZoneNum).AllSurfaceFirst + 1) << '\n';
        for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
            if (state.dataSurface->Surface(surf).Zone != ZoneNum) continue;
            SolarDiffusing = "";
            if (RptType == 10 || RptType == 11) { // Details and Details with Vertices
                if (state.dataSurface->Surface(surf).BaseSurf == surf) {
                    BaseSurfName = "";
                } else {
                    BaseSurfName = state.dataSurface->Surface(surf).BaseSurfName;
                }
                {
                    auto const SELECT_CASE_var(state.dataSurface->Surface(surf).HeatTransferAlgorithm);
                    if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::None) {
                        AlgoName = "None";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::CTF) {
                        AlgoName = "CTF - ConductionTransferFunction";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::CondFD) {
                        AlgoName = "CondFD - ConductionFiniteDifference";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::EMPD) {
                        AlgoName = "EMPD - MoisturePenetrationDepthConductionTransferFunction";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::HAMT) {
                        AlgoName = "HAMT - CombinedHeatAndMoistureFiniteElement";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::Kiva) {
                        AlgoName = "KivaFoundation - TwoDimensionalFiniteDifference";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::Window5) {
                        AlgoName = "Window5 Detailed Fenestration";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::ComplexFenestration) {
                        AlgoName = "Window7 Complex Fenestration";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::TDD) {
                        AlgoName = "Tubular Daylighting Device";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::AirBoundaryNoHT) {
                        AlgoName = "Air Boundary - No Heat Transfer";
                    }
                }
                // Default Convection Coefficient Calculation Algorithms
                IntConvCoeffCalc = ConvCoeffCalcs(state.dataHeatBal->Zone(ZoneNum).InsideConvectionAlgo);
                ExtConvCoeffCalc = ConvCoeffCalcs(state.dataHeatBal->Zone(ZoneNum).OutsideConvectionAlgo);

                *eiostream << "HeatTransfer Surface," << state.dataSurface->Surface(surf).Name << ","
                           << cSurfaceClass(state.dataSurface->Surface(surf).Class) << "," << BaseSurfName << "," << AlgoName << ",";

                // NOTE - THIS CODE IS REPEATED IN SurfaceGeometry.cc IN SetupZoneGeometry
                // Calculate Nominal U-value with convection/film coefficients for reporting by adding on
                // prescribed R-values for interior and exterior convection coefficients as found in ASHRAE 90.1-2004, Appendix A
                if (state.dataSurface->Surface(surf).Construction > 0 &&
                    state.dataSurface->Surface(surf).Construction <= state.dataHeatBal->TotConstructs) {
                    cNominalUwithConvCoeffs = "";
                    ConstructionName = state.dataConstruction->Construct(state.dataSurface->Surface(surf).Construction).Name;
                    {
                        auto const SELECT_CASE_var(state.dataSurface->Surface(surf).Class);
                        if (SELECT_CASE_var == SurfaceClass::Wall) {
                            // Interior:  vertical, still air, Rcin = 0.68 ft2-F-hr/BTU
                            // Exterior:  vertical, exterior wind exposure, Rcout = 0.17 ft2-F-hr/BTU
                            if (state.dataHeatBal->NominalU(state.dataSurface->Surface(surf).Construction) > 0.0) {
                                NominalUwithConvCoeffs =
                                    1.0 /
                                    (0.1197548 + (1.0 / state.dataHeatBal->NominalU(state.dataSurface->Surface(surf).Construction)) + 0.0299387);
                            } else {
                                cNominalUwithConvCoeffs = "[invalid]";
                            }
                        } else if (SELECT_CASE_var == SurfaceClass::Floor) {
                            // Interior:  horizontal, still air, heat flow downward, Rcin = 0.92 ft2-F-hr/BTU
                            // Exterior:  horizontal, semi-exterior (crawlspace), Rcout = 0.46 ft2-F-hr/BTU
                            if (state.dataHeatBal->NominalU(state.dataSurface->Surface(surf).Construction) > 0.0) {
                                NominalUwithConvCoeffs =
                                    1.0 /
                                    (0.1620212 + (1.0 / state.dataHeatBal->NominalU(state.dataSurface->Surface(surf).Construction)) + 0.0810106);
                            } else {
                                cNominalUwithConvCoeffs = "[invalid]";
                            }
                        } else if (SELECT_CASE_var == SurfaceClass::Roof) {
                            // Interior:  horizontal, still air, heat flow upward, Rcin = 0.61 ft2-F-hr/BTU
                            // Exterior:  horizontal, semi-exterior (attic), Rcout = 0.46 ft2-F-hr/BTU
                            if (state.dataHeatBal->NominalU(state.dataSurface->Surface(surf).Construction) > 0.0) {
                                NominalUwithConvCoeffs =
                                    1.0 /
                                    (0.1074271 + (1.0 / state.dataHeatBal->NominalU(state.dataSurface->Surface(surf).Construction)) + 0.0810106);
                            } else {
                                cNominalUwithConvCoeffs = "[invalid]";
                            }
                        } else {
                            if (state.dataHeatBal->NominalU(state.dataSurface->Surface(surf).Construction) > 0.0) {
                                NominalUwithConvCoeffs = state.dataHeatBal->NominalU(state.dataSurface->Surface(surf).Construction);
                            } else {
                                cNominalUwithConvCoeffs = "[invalid]";
                            }
                        }
                    }
                    if (cNominalUwithConvCoeffs == "") {
                        cNominalUwithConvCoeffs = format("{:.3R}", NominalUwithConvCoeffs);
                    } else {
                        cNominalUwithConvCoeffs = "[invalid]";
                    }
                    if ((state.dataSurface->Surface(surf).Class == SurfaceClass::Window) ||
                        (state.dataSurface->Surface(surf).Class == SurfaceClass::TDD_Dome)) {
                        // SurfaceClass::Window also covers glass doors and TDD:Diffusers
                        cNominalU = "N/A";
                        if (state.dataSurface->SurfWinSolarDiffusing(surf)) {
                            SolarDiffusing = "Yes";
                        } else {
                            SolarDiffusing = "No";
                        }
                    } else {
                        cNominalU = format("{:.3R}", state.dataHeatBal->NominalU(state.dataSurface->Surface(surf).Construction));
                    }
                } else {
                    cNominalUwithConvCoeffs = "**";
                    cNominalU = "**";
                    ConstructionName = "**invalid**";
                }

                *eiostream << ConstructionName << "," << cNominalU << "," << cNominalUwithConvCoeffs << "," << SolarDiffusing << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Area) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).GrossArea) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).NetAreaShadowCalc) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Azimuth) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Tilt) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Width) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Height) << ","
                           << format("{:.2R}", state.dataSurface->Surface(surf).Reveal) << ",";
                if (state.dataSurface->SurfIntConvCoeffIndex(surf) > 0) {
                    {
                        auto const SELECT_CASE_var(
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->SurfIntConvCoeffIndex(surf)).OverrideType);
                        if (SELECT_CASE_var == ConvCoefValue) {
                            IntConvCoeffCalc = "User Supplied Value";
                        } else if (SELECT_CASE_var == ConvCoefSchedule) {
                            IntConvCoeffCalc = "User Supplied Schedule";
                        } else if (SELECT_CASE_var == ConvCoefUserCurve) {
                            ExtConvCoeffCalc = "User Supplied Curve";
                        } else if (SELECT_CASE_var == ConvCoefSpecifiedModel) {
                            ExtConvCoeffCalc = "User Specified Model";
                        }
                    }
                } else if (state.dataSurface->SurfIntConvCoeffIndex(surf) < 0) { // not in use yet.
                    IntConvCoeffCalc = ConvCoeffCalcs(std::abs(state.dataSurface->SurfIntConvCoeffIndex(surf)));
                }
                if (state.dataSurface->SurfExtConvCoeffIndex(surf) > 0) {
                    {
                        auto const SELECT_CASE_var(
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->SurfExtConvCoeffIndex(surf)).OverrideType);
                        if (SELECT_CASE_var == ConvCoefValue) {
                            ExtConvCoeffCalc = "User Supplied Value";
                        } else if (SELECT_CASE_var == ConvCoefSchedule) {
                            ExtConvCoeffCalc = "User Supplied Schedule";
                        } else if (SELECT_CASE_var == ConvCoefUserCurve) {
                            ExtConvCoeffCalc = "User Supplied Curve";
                        } else if (SELECT_CASE_var == ConvCoefSpecifiedModel) {
                            ExtConvCoeffCalc = "User Specified Model";
                        }
                    }
                } else if (state.dataSurface->SurfExtConvCoeffIndex(surf) < 0) {
                    ExtConvCoeffCalc = ConvCoeffCalcs(std::abs(state.dataSurface->SurfExtConvCoeffIndex(surf)));
                }
                if (state.dataSurface->Surface(surf).ExtBoundCond == ExternalEnvironment) {
                    *eiostream << "ExternalEnvironment"
                               << "," << ExtConvCoeffCalc << "," << IntConvCoeffCalc << ",";
                } else if (state.dataSurface->Surface(surf).ExtBoundCond == Ground) {
                    *eiostream << "Ground"
                               << ","
                               << "N/A-Ground"
                               << "," << IntConvCoeffCalc << ",";
                } else if (state.dataSurface->Surface(surf).ExtBoundCond == GroundFCfactorMethod) {
                    *eiostream << "FCGround"
                               << ","
                               << "N/A-FCGround"
                               << "," << IntConvCoeffCalc << ",";
                } else if (state.dataSurface->Surface(surf).ExtBoundCond == KivaFoundation) {
                    *eiostream << "Foundation"
                               << ","
                               << "N/A-Foundation"
                               << "," << IntConvCoeffCalc << ",";
                } else if (state.dataSurface->Surface(surf).ExtBoundCond == OtherSideCoefNoCalcExt ||
                           state.dataSurface->Surface(surf).ExtBoundCond == OtherSideCoefCalcExt) {
                    *eiostream << state.dataSurface->OSC(state.dataSurface->Surface(surf).OSCPtr).Name << ","
                               << "N/A-OSC"
                               << "," << IntConvCoeffCalc << ",";
                } else if (state.dataSurface->Surface(surf).ExtBoundCond == OtherSideCondModeledExt) {
                    *eiostream << state.dataSurface->OSCM(state.dataSurface->Surface(surf).OSCMPtr).Name << ","
                               << "N/A-OSCM"
                               << "," << IntConvCoeffCalc << ",";
                } else {
                    *eiostream << state.dataSurface->Surface(surf).ExtBoundCondName << ","
                               << "Other/Same Surface Int Conv"
                               << "," << IntConvCoeffCalc << ",";
                }
                if (state.dataSurface->Surface(surf).ExtSolar) {
                    *eiostream << "SunExposed"
                               << ",";
                } else {
                    *eiostream << "NoSun"
                               << ",";
                }
                if (state.dataSurface->Surface(surf).ExtWind) {
                    *eiostream << "WindExposed"
                               << ",";
                } else {
                    *eiostream << "NoWind"
                               << ",";
                }
                if (RptType == 10) {
                    *eiostream << format("{:.2R}", state.dataSurface->Surface(surf).ViewFactorGround) << ","
                               << format("{:.2R}", state.dataSurface->Surface(surf).ViewFactorSky) << ","
                               << format("{:.2R}", state.dataSurface->Surface(surf).ViewFactorGroundIR) << ","
                               << format("{:.2R}", state.dataSurface->Surface(surf).ViewFactorSkyIR) << ","
                               << fmt::to_string(state.dataSurface->Surface(surf).Sides) << '\n';
                } else {
                    *eiostream << format("{:.2R}", state.dataSurface->Surface(surf).ViewFactorGround) << ","
                               << format("{:.2R}", state.dataSurface->Surface(surf).ViewFactorSky) << ","
                               << format("{:.2R}", state.dataSurface->Surface(surf).ViewFactorGroundIR) << ","
                               << format("{:.2R}", state.dataSurface->Surface(surf).ViewFactorSkyIR) << ","
                               << fmt::to_string(state.dataSurface->Surface(surf).Sides) << ",";
                    for (vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                        if (vert != state.dataSurface->Surface(surf).Sides) {
                            *eiostream << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).x) << ","
                                       << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).y) << ","
                                       << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).z) << ",";
                        } else {
                            *eiostream << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).x) << ","
                                       << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).y) << ","
                                       << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).z) << '\n';
                        }
                    }
                    if (state.dataSurface->Surface(surf).Sides == 0) *eiostream << '\n';
                }
                // if window, report frame/divider as appropriate
                if (state.dataSurface->Surface(surf).FrameDivider > 0) {
                    fd = state.dataSurface->Surface(surf).FrameDivider;
                    if (state.dataSurface->FrameDivider(fd).FrameWidth > 0.0) {
                        {
                            auto const SELECT_CASE_var(state.dataSurface->Surface(surf).HeatTransferAlgorithm);
                            if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::None) {
                                AlgoName = "None";
                            } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::CTF) {
                                AlgoName = "CTF - ConductionTransferFunction";
                            } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::CondFD) {
                                AlgoName = "CondFD - ConductionFiniteDifference";
                            } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::EMPD) {
                                AlgoName = "EMPD - MoisturePenetrationDepthConductionTransferFunction";
                            } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::HAMT) {
                                AlgoName = "HAMT - CombinedHeatAndMoistureFiniteElement";
                            } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::Kiva) {
                                AlgoName = "KivaFoundation - TwoDimensionalFiniteDifference";
                            } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::Window5) {
                                AlgoName = "Window5 Detailed Fenestration";
                            } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::ComplexFenestration) {
                                AlgoName = "Window7 Complex Fenestration";
                            } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::TDD) {
                                AlgoName = "Tubular Daylighting Device";
                            }
                        }
                        *eiostream << "Frame/Divider Surface," << state.dataSurface->FrameDivider(fd).Name << ","
                                   << "Frame," << state.dataSurface->Surface(surf).Name << "," << AlgoName << ",";
                        *eiostream << ",N/A,N/A,," << format("{:.2R}", state.dataSurface->SurfWinFrameArea(surf)) << ","
                                   << format("{:.2R}", state.dataSurface->SurfWinFrameArea(surf) / state.dataSurface->Surface(surf).Multiplier)
                                   << ",*"
                                   << ",N/A"
                                   << ",N/A," << format("{:.2R}", state.dataSurface->FrameDivider(fd).FrameWidth) << ",N/A" << '\n';
                    }
                    if (state.dataSurface->FrameDivider(fd).DividerWidth > 0.0) {
                        if (state.dataSurface->FrameDivider(fd).DividerType == DividedLite) {
                            *eiostream << "Frame/Divider Surface," << state.dataSurface->FrameDivider(fd).Name << ","
                                       << "Divider:DividedLite," << state.dataSurface->Surface(surf).Name << ",,";
                        } else {
                            *eiostream << "Frame/Divider Surface," << state.dataSurface->FrameDivider(fd).Name << ","
                                       << "Divider:Suspended," << state.dataSurface->Surface(surf).Name << ",,";
                        }
                        *eiostream << ",N/A,N/A,," << format("{:.2R}", state.dataSurface->SurfWinDividerArea(surf)) << ","
                                   << format("{:.2R}", state.dataSurface->SurfWinDividerArea(surf) / state.dataSurface->Surface(surf).Multiplier)
                                   << ",*"
                                   << ",N/A"
                                   << ",N/A," << format("{:.2R}", state.dataSurface->FrameDivider(fd).DividerWidth) << ",N/A" << '\n';
                    }
                }
            } else { // RptType=1  Vertices only
                if (state.dataSurface->Surface(surf).BaseSurf == surf) {
                    BaseSurfName = "";
                } else {
                    BaseSurfName = state.dataSurface->Surface(surf).BaseSurfName;
                }
                {
                    auto const SELECT_CASE_var(state.dataSurface->Surface(surf).HeatTransferAlgorithm);
                    if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::None) {
                        AlgoName = "None";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::CTF) {
                        AlgoName = "CTF - ConductionTransferFunction";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::CondFD) {
                        AlgoName = "CondFD - ConductionFiniteDifference";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::EMPD) {
                        AlgoName = "EMPD - MoisturePenetrationDepthConductionTransferFunction";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::HAMT) {
                        AlgoName = "HAMT - CombinedHeatAndMoistureFiniteElement";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::Kiva) {
                        AlgoName = "KivaFoundation - TwoDimensionalFiniteDifference";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::Window5) {
                        AlgoName = "Window5 Detailed Fenestration";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::ComplexFenestration) {
                        AlgoName = "Window7 Complex Fenestration";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::TDD) {
                        AlgoName = "Tubular Daylighting Device";
                    } else if (SELECT_CASE_var == DataSurfaces::iHeatTransferModel::AirBoundaryNoHT) {
                        AlgoName = "Air Boundary - No Heat Transfer";
                    }
                }
                *eiostream << "HeatTransfer Surface," << state.dataSurface->Surface(surf).Name << ","
                           << cSurfaceClass(state.dataSurface->Surface(surf).Class) << "," << BaseSurfName << "," << AlgoName << ",";
                *eiostream << fmt::to_string(state.dataSurface->Surface(surf).Sides) << ",";
                for (vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                    if (vert != state.dataSurface->Surface(surf).Sides) {
                        *eiostream << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).x) << ","
                                   << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).y) << ","
                                   << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).z) << ",";
                    } else {
                        *eiostream << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).x) << ","
                                   << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).y) << ","
                                   << format("{:.2R}", state.dataSurface->Surface(surf).Vertex(vert).z) << '\n';
                    }
                }
                if (state.dataSurface->Surface(surf).Sides == 0) *eiostream << '\n';
            }
        } // surfaces
    }     // zones

    print(state.files.eio, "{}", eiostream->str());
}

void CostInfoOut(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   April 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine produces a file with information about surfaces.
    // for the purpose of producing first cost estimates to include in objective value functions
    // for design optimization

    // METHODOLOGY EMPLOYED:
    // Access data in DataSurfaces and report

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace DataHeatBalance;
    using namespace DataSurfaces;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // na

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Array1D_bool uniqueSurf;

    if (state.dataSurface->TotSurfaces > 0 && !allocated(state.dataSurface->Surface)) {
        // no error needed, probably in end processing, just return
        return;
    }

    // need to determine unique surfacs... some surfaces are shared by zones and hence doubled
    uniqueSurf.dimension(state.dataSurface->TotSurfaces, true);

    for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
        if (state.dataSurface->Surface(surf).ExtBoundCond > 0) {
            if (state.dataSurface->Surface(surf).ExtBoundCond < surf) { // already cycled through
                uniqueSurf(surf) = false;
            }
        }
        if (state.dataSurface->Surface(surf).Construction == 0) { // throw out others for now
            uniqueSurf(surf) = false;
        }
    }

    auto scifile = state.files.sci.open(state, "CostInfoOut", state.files.outputControl.sci);

    print(scifile, "{:12}{:12}\n", state.dataSurface->TotSurfaces, count(uniqueSurf));
    print(scifile, "{}\n", " data for surfaces useful for cost information");
    print(scifile, "{}\n", " Number, Name, Construction, class, area, grossarea");

    for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
        // if (surface(surf)%class .eq. SurfaceClass::IntMass) CYCLE
        if (!uniqueSurf(surf)) continue;
        // why the heck are constructions == 0 ?
        if (state.dataSurface->Surface(surf).Construction != 0) {
            // Formats
            static constexpr fmt::string_view Format_801("{:5},{},{},{},{:14.5F},{:14.5F}\n");
            print(scifile,
                  Format_801,
                  surf,
                  state.dataSurface->Surface(surf).Name,
                  state.dataConstruction->Construct(state.dataSurface->Surface(surf).Construction).Name,
                  cSurfaceClass(state.dataSurface->Surface(surf).Class),
                  state.dataSurface->Surface(surf).Area,
                  state.dataSurface->Surface(surf).GrossArea);
        }
    }

    uniqueSurf.deallocate();
}

void VRMLOut(EnergyPlusData &state, const std::string &PolygonAction, const std::string &ColorScheme)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   August 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine produces a file of VRML output for the surfaces.

    // METHODOLOGY EMPLOYED:
    // Use the surface absolute coordinate information to produce
    // lines.

    // Using/Aliasing
    using namespace DataSurfaces;
    using namespace DXFEarClipping;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static Array1D_string const colorstring(7, {"WALL", "WINDOW", "FIXEDSHADE", "SUBSHADE", "ROOF", "FLOOR", "BLDGSHADE"});

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string ShadeType;
    bool ThickPolyline(false);
    bool RegularPolyline(false);
    std::string PolylineWidth(" 0.55");
    bool TriangulateFace(false);

    // Formats
    static constexpr fmt::string_view Format_702("#VRML V2.0 utf8\n");
    static constexpr fmt::string_view Format_707(
        "WorldInfo {{\n   title \"Building - {}\"\n   info [\"EnergyPlus Program Version {}\"]\n   info [\"Surface Color Scheme {}\"]\n}}\n");
    static constexpr fmt::string_view Format_800("Shape {{\nappearance DEF {} Appearance {{\nmaterial Material {{ diffuseColor {} }}\n}}\n}}\n");
    static constexpr fmt::string_view Format_801("Shape {{\nappearance USE {}\ngeometry IndexedFaceSet {{\nsolid TRUE\ncoord DEF {}{} Coordinate {{\npoint [\n");
    static constexpr fmt::string_view Format_802("{:15.5F} {:15.5F} {:15.5F},\n");
    static constexpr fmt::string_view Format_803("]\n}}\ncoordIndex [\n");
    static constexpr fmt::string_view Format_805("]\nccw TRUE\nsolid TRUE\n}}\n}}\n");

    if (PolygonAction == "TRIANGULATE3DFACE" || PolygonAction == "TRIANGULATE") {
        TriangulateFace = true;
    } else if (PolygonAction == "THICKPOLYLINE" || PolygonAction == "") {
        ThickPolyline = true;
    } else if (PolygonAction == "REGULARPOLYLINE") {
        RegularPolyline = true;
        PolylineWidth = " 0";
    } else {
        ShowWarningError(state, "VRMLOut: Illegal key specified for Surfaces with > 4 sides=" + PolygonAction);
        ShowContinueError(state, "\"TRIANGULATE 3DFACE\" will be used for any surfaces with > 4 sides.");
        TriangulateFace = true;
    }

    if (state.dataSurface->TotSurfaces > 0 && !allocated(state.dataSurface->Surface)) {
        // no error needed, probably in end processing, just return
        return;
    }

    auto wrlfile = state.files.wrl.open(state, "VRMLOut", state.files.outputControl.wrl);

    print(wrlfile, Format_702);

    if (ColorScheme == "") {
        print(wrlfile, Format_707, state.dataHeatBal->BuildingName, state.dataStrGlobals->VerStringVar, "Default"); // World Info
    } else {
        print(wrlfile, Format_707, state.dataHeatBal->BuildingName, state.dataStrGlobals->VerStringVar, ColorScheme); // World Info
    }

    print(wrlfile, "# Zone Names\n");
    for (int zones = 1; zones <= state.dataGlobal->NumOfZones; ++zones) {
        print(wrlfile, "# Zone={}:{}\n", zones, normalizeName(state.dataHeatBal->Zone(zones).Name));
    }

    // Define the colors:

    print(wrlfile, Format_800, "FLOOR", "0.502 0.502 0.502");
    print(wrlfile, Format_800, "ROOF", "1 1 0");
    print(wrlfile, Format_800, "WALL", "0 1 0");
    print(wrlfile, Format_800, "WINDOW", "0 1 1");
    print(wrlfile, Format_800, "DOOR", "0 1 1");
    print(wrlfile, Format_800, "GLASSDOOR", "0 1 1");
    print(wrlfile, Format_800, "FIXEDSHADE", "1 0 1");
    print(wrlfile, Format_800, "BLDGSHADE", "0 0 1");
    print(wrlfile, Format_800, "SUBSHADE", "1 0 1");
    print(wrlfile, Format_800, "BACKCOLOR", "0.502 0.502 0.784");

    int colorindex = 0;

    //  Do all detached shading surfaces first
    for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
        if (state.dataSurface->Surface(surf).HeatTransSurf) continue;
        if (state.dataSurface->Surface(surf).IsAirBoundarySurf) continue;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Shading) continue;
        if (state.dataSurface->Surface(surf).Sides == 0) continue;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_F) colorindex = 3;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_B) colorindex = 7;
        if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_F) {
            ShadeType = "Fixed Shading";
            print(wrlfile, "# Fixed Shading:{}\n", state.dataSurface->Surface(surf).Name);
        } else if (state.dataSurface->Surface(surf).Class == SurfaceClass::Detached_B) {
            ShadeType = "Building Shading";
            print(wrlfile, "# Building Shading:{}", state.dataSurface->Surface(surf).Name);
        }
        print(wrlfile, Format_801, colorstring(colorindex), "Surf", surf);
        for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
            print(wrlfile,
                  Format_802,
                  state.dataSurface->Surface(surf).Vertex(vert).x,
                  state.dataSurface->Surface(surf).Vertex(vert).y,
                  state.dataSurface->Surface(surf).Vertex(vert).z);
        }
        print(wrlfile, Format_803);
        if (state.dataSurface->Surface(surf).Sides <= 4 || !TriangulateFace) {
            for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                print(wrlfile, " {}", vert - 1);
                if (vert == state.dataSurface->Surface(surf).Sides) print(wrlfile, " -1\n");
            }
            print(wrlfile, Format_805);
        } else { // will be >4 sided polygon with triangulate option
            Array1D<dTriangle> mytriangles;
            const auto ntri = Triangulate(state,
                                          state.dataSurface->Surface(surf).Sides,
                                          state.dataSurface->Surface(surf).Vertex,
                                          mytriangles,
                                          state.dataSurface->Surface(surf).Azimuth,
                                          state.dataSurface->Surface(surf).Tilt,
                                          state.dataSurface->Surface(surf).Name,
                                          state.dataSurface->Surface(surf).Class);
            for (int svert = 1; svert <= ntri; ++svert) {
                const auto vv0 = mytriangles(svert).vv0;
                const auto vv1 = mytriangles(svert).vv1;
                const auto vv2 = mytriangles(svert).vv2;
                print(wrlfile, " {} {} {} -1\n", vv0 - 1, vv1 - 1, vv2 - 1);
            }
            print(wrlfile, Format_805);
            mytriangles.deallocate();
        }
    }
    //  ! now do zone surfaces, by zone
    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        int oldSurfNum = 0;
        for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
            ++oldSurfNum;
            if (state.dataSurface->Surface(surf).Zone != zoneNum) continue;
            if (state.dataSurface->Surface(surf).Sides == 0) continue;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::IntMass) continue;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Wall) colorindex = 1;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Roof) colorindex = 5;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::TDD_Dome) colorindex = 2;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Floor) colorindex = 6;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Window) colorindex = 2;
            if (state.dataSurface->Surface(surf).Class == SurfaceClass::Door) colorindex = 2;

            print(wrlfile, "# {}:{}\n", state.dataSurface->Surface(surf).ZoneName, state.dataSurface->Surface(surf).Name);
            print(wrlfile, Format_801, colorstring(colorindex), "Surf", oldSurfNum);
            for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                print(wrlfile,
                      Format_802,
                      state.dataSurface->Surface(surf).Vertex(vert).x,
                      state.dataSurface->Surface(surf).Vertex(vert).y,
                      state.dataSurface->Surface(surf).Vertex(vert).z);
            }
            print(wrlfile, Format_803);
            if (state.dataSurface->Surface(surf).Sides <= 4 || !TriangulateFace) {
                for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                    print(wrlfile, " {}", vert - 1);
                    if (vert == state.dataSurface->Surface(surf).Sides) print(wrlfile, " -1\n");
                }
                print(wrlfile, Format_805);
            } else { // will be >4 sided polygon with triangulate option
                Array1D<dTriangle> mytriangles;
                const auto ntri = Triangulate(state,
                                              state.dataSurface->Surface(surf).Sides,
                                              state.dataSurface->Surface(surf).Vertex,
                                              mytriangles,
                                              state.dataSurface->Surface(surf).Azimuth,
                                              state.dataSurface->Surface(surf).Tilt,
                                              state.dataSurface->Surface(surf).Name,
                                              state.dataSurface->Surface(surf).Class);
                for (int svert = 1; svert <= ntri; ++svert) {
                    const auto vv0 = mytriangles(svert).vv0;
                    const auto vv1 = mytriangles(svert).vv1;
                    const auto vv2 = mytriangles(svert).vv2;
                    print(wrlfile, " {} {} {} -1\n", vv0 - 1, vv1 - 1, vv2 - 1);
                }
                print(wrlfile, Format_805);
                mytriangles.deallocate();
            }
        }
        // still have to do shading surfaces for zone
        colorindex = 4;
        for (int surf : state.dataSurface->AllSurfaceListReportOrder) {
            //      !if (surface(surf)%heattranssurf) CYCLE ! Shading with a construction is allowed to be HT surf for daylighting shelves
            if (state.dataSurface->Surface(surf).Class != SurfaceClass::Shading) continue;
            if (state.dataSurface->Surface(surf).ZoneName != state.dataHeatBal->Zone(zoneNum).Name) continue;
            if (state.dataSurface->Surface(surf).Sides == 0) continue;
            print(wrlfile, "# {}:{}\n", state.dataSurface->Surface(surf).ZoneName, state.dataSurface->Surface(surf).Name);
            print(wrlfile, Format_801, colorstring(colorindex), "Surf", surf);
            for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                print(wrlfile,
                      Format_802,
                      state.dataSurface->Surface(surf).Vertex(vert).x,
                      state.dataSurface->Surface(surf).Vertex(vert).y,
                      state.dataSurface->Surface(surf).Vertex(vert).z);
            }
            print(wrlfile, Format_803);
            if (state.dataSurface->Surface(surf).Sides <= 4 || !TriangulateFace) {
                for (int vert = 1; vert <= state.dataSurface->Surface(surf).Sides; ++vert) {
                    print(wrlfile, " {}", vert - 1);
                    if (vert == state.dataSurface->Surface(surf).Sides) print(wrlfile, " -1\n");
                }
                print(wrlfile, Format_805);
            } else { // will be >4 sided polygon with triangulate option
                Array1D<dTriangle> mytriangles;
                const auto ntri = Triangulate(state,
                                              state.dataSurface->Surface(surf).Sides,
                                              state.dataSurface->Surface(surf).Vertex,
                                              mytriangles,
                                              state.dataSurface->Surface(surf).Azimuth,
                                              state.dataSurface->Surface(surf).Tilt,
                                              state.dataSurface->Surface(surf).Name,
                                              state.dataSurface->Surface(surf).Class);
                for (int svert = 1; svert <= ntri; ++svert) {
                    const auto vv0 = mytriangles(svert).vv0;
                    const auto vv1 = mytriangles(svert).vv1;
                    const auto vv2 = mytriangles(svert).vv2;
                    print(wrlfile, " {} {} {} -1\n", vv0 - 1, vv1 - 1, vv2 - 1);
                }
                print(wrlfile, Format_805);
                mytriangles.deallocate();
            }
        }
    }

    // vrml does not have daylighting reference points included
}

} // namespace EnergyPlus
