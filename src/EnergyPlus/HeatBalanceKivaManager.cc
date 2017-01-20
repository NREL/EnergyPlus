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

// C++ Headers

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// Lis Header
#include "lis.h"

#ifdef GROUND_PLOT
#include "libgroundplot/GroundPlot.hpp"
#endif

// EnergyPlus Headers
#include <HeatBalanceKivaManager.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataGlobals.hh>
#include <DataSurfaces.hh>
#include <DataStringGlobals.hh>
#include <DisplayRoutines.hh>
#include <General.hh>
#include <SurfaceGeometry.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {
namespace HeatBalanceKivaManager {

KivaInstanceMap::KivaInstanceMap(Kiva::Foundation& foundation,
  std::map<Kiva::Surface::SurfaceType, std::vector<Kiva::GroundOutput::OutputType>> oM,
  int floorSurface,
  std::vector< int > wallSurfaces,
  int zoneNum) :
  outputMap(oM),
  ground(foundation, outputMap),
  floorSurface(floorSurface),
  wallSurfaces(wallSurfaces),
  zoneNum(zoneNum)
{}

void KivaInstanceMap::setBoundaryConditions() {
  bcs.indoorTemp = DataHeatBalFanSys::MAT(zoneNum) + DataGlobals::KelvinConv;
  bcs.outdoorTemp = DataEnvironment::OutDryBulbTemp + DataGlobals::KelvinConv;
  bcs.localWindSpeed = DataEnvironment::WindSpeedAt(ground.foundation.surfaceRoughness);
  bcs.solarAzimuth = std::atan2( DataEnvironment::SOLCOS( 1 ), DataEnvironment::SOLCOS( 2 ) );
  bcs.solarAltitude = DataGlobals::PiOvr2 - std::acos( DataEnvironment::SOLCOS( 3 ) );
  bcs.directNormalFlux = DataEnvironment::BeamSolarRad;
  bcs.diffuseHorizontalFlux = DataEnvironment::DifSolarRad;
  bcs.skyEmissivity = pow4(DataEnvironment::SkyTempKelvin)/pow4(bcs.outdoorTemp);

  bcs.slabAbsRadiation =
    DataHeatBalSurface::NetLWRadToSurf( floorSurface ) +
    DataHeatBalSurface::QRadSWInAbs( floorSurface ) +
    DataHeatBalFanSys::QHTRadSysSurf( floorSurface ) +
    DataHeatBalFanSys::QHWBaseboardSurf( floorSurface ) +
    DataHeatBalFanSys::QSteamBaseboardSurf( floorSurface ) +
    DataHeatBalFanSys::QElecBaseboardSurf( floorSurface ) +
    DataHeatBalance::QRadThermInAbs( floorSurface );

  // Calculate area weighted average for walls
  Real64 QAtotal = 0.0;
  Real64 Atotal = 0.0;
  for (auto& wl : wallSurfaces) {
    Real64 Q =
      DataHeatBalSurface::NetLWRadToSurf( wl ) +
      DataHeatBalSurface::QRadSWInAbs( wl ) +
      DataHeatBalFanSys::QHTRadSysSurf( wl ) +
      DataHeatBalFanSys::QHWBaseboardSurf( wl ) +
      DataHeatBalFanSys::QSteamBaseboardSurf( wl ) +
      DataHeatBalFanSys::QElecBaseboardSurf( wl ) +
      DataHeatBalance::QRadThermInAbs( wl );

    Real64& A = DataSurfaces::Surface( wl ).Area;

    QAtotal += Q*A;
    Atotal += A;

  }

  if (Atotal > 0.0) {
    bcs.wallAbsRadiation = QAtotal/Atotal;
  }

}

void KivaInstanceMap::reportKivaSurfaces() {
  // Calculate inside face values
  Real64 const qFloor = -(bcs.slabAbsRadiation + DataHeatBalance::HConvIn(floorSurface)*(DataHeatBalFanSys::MAT(zoneNum) - DataHeatBalSurface::TempSurfIn(floorSurface)));

  DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( floorSurface ) = qFloor;
  DataHeatBalSurface::OpaqSurfInsFaceConduction( floorSurface ) = qFloor * DataSurfaces::Surface(floorSurface).Area;

  for (auto& wl : wallSurfaces) {
    Real64 Qrad =
      DataHeatBalSurface::NetLWRadToSurf( wl ) +
      DataHeatBalSurface::QRadSWInAbs( wl ) +
      DataHeatBalFanSys::QHTRadSysSurf( wl ) +
      DataHeatBalFanSys::QHWBaseboardSurf( wl ) +
      DataHeatBalFanSys::QSteamBaseboardSurf( wl ) +
      DataHeatBalFanSys::QElecBaseboardSurf( wl ) +
      DataHeatBalance::QRadThermInAbs( wl );

    Real64 const qWall = -(Qrad + DataHeatBalance::HConvIn(wl)*(DataHeatBalFanSys::MAT(zoneNum) - DataHeatBalSurface::TempSurfIn(wl)));

    DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( wl ) = qWall;
    DataHeatBalSurface::OpaqSurfInsFaceConduction( wl ) = qWall * DataSurfaces::Surface(wl).Area;

  }

}

KivaManager::Settings::Settings() :
  soilK(0.864),
  soilRho(1510),
  soilCp(1260),
  groundSolarAbs(0.9),
  groundThermalAbs(0.9),
  groundRoughness(0.9),
  farFieldWidth(40.0),
  deepGroundBoundary(AUTO),
  deepGroundDepth(40.0),
  minCellDim(0.02),
  maxGrowthCoeff(1.5),
  timestepType(HOURLY)
{}

KivaManager::KivaManager() :
  defaultSet(false),
  defaultIndex(0)
{

  // lis overhead
  LIS_INT dummy_argc = 0;
  char** dummy_argv;
  lis_initialize(&dummy_argc, &dummy_argv);

  // default
  defaultSet = false;
  defaultIndex = 0.0;
}

KivaManager::~KivaManager() {
  lis_finalize();
}

void KivaManager::setupKivaInstances() {

  auto& Surfaces = DataSurfaces::Surface;
  auto& Constructs = DataHeatBalance::Construct;
  auto& Materials = DataHeatBalance::Material;

  // Figure out number of instances (number of foundation coupled floors)
  int inst = 0;
  int surfNum = 1;

  for (auto& surface : Surfaces) {
    if ( surface.ExtBoundCond == DataSurfaces::KivaFoundation && surface.Class == DataSurfaces::SurfaceClass_Floor ) {

      // Copy foundation input for this instance
      Kiva::Foundation fnd = foundationInputs[surface.OSCPtr].foundation;

      // Find other surfaces associated with the same floor
      std::vector< int > wallSurfaces;

      for (auto& wl : foundationInputs[surface.OSCPtr].surfaces) {
        if ( Surfaces(wl).Zone == surface.Zone && wl != surfNum ) {
          if ( Surfaces(wl).Class != DataSurfaces::SurfaceClass_Wall ) {
            if ( Surfaces(wl).Class == DataSurfaces::SurfaceClass_Floor ) {
              // TODO Kiva: only one foundation floor per zone
              ShowSevereError( "only one foundation floor per zone" );
              ShowFatalError( "KivaManager: Program terminates due to preceding conditions." );
            } else {
              // TODO Kiva: only floor and wall surfaces allowed
              ShowSevereError( "only floor and wall surfaces allowed" );
              ShowFatalError( "KivaManager: Program terminates due to preceding conditions." );
            }
          } else {
            wallSurfaces.push_back(wl);
          }
        }
      }

      // Set wall construction

      Real64 wallHeight = 0.0;
      if (wallSurfaces.size() != 0) {
        Real64 minZ = Surfaces(wallSurfaces[0]).Vertex[0].z;
        Real64 maxZ = minZ;
        for (size_t i = 1; i < Surfaces(wallSurfaces[0]).Vertex.size(); ++i ) {
          if (Surfaces(wallSurfaces[0]).Vertex[i].z < minZ) {minZ = Surfaces(wallSurfaces[0]).Vertex[i].z;}
          if (Surfaces(wallSurfaces[0]).Vertex[i].z > maxZ) {maxZ = Surfaces(wallSurfaces[0]).Vertex[i].z;}
        }
        wallHeight = maxZ - minZ; // TODO Kiva: each wall with different height gets its own instance. Also use average height in case walls are on slope...
        int constructionNum = Surfaces(wallSurfaces[0]).Construction;
        for (auto& wl : wallSurfaces) {
          for (size_t i = 1; i < Surfaces(wl).Vertex.size(); ++i ) {
            if (Surfaces(wl).Vertex[i].z < minZ) {minZ = Surfaces(wl).Vertex[i].z;}
            if (Surfaces(wl).Vertex[i].z > maxZ) {maxZ = Surfaces(wl).Vertex[i].z;}
          }
          Real64 surfHeight = maxZ - minZ;
          if (std::abs(surfHeight - wallHeight) > 0.001) {
            // TODO Kiva: all walls must be the same height
            ShowSevereError( "all walls must be the same height" );
            ShowFatalError( "KivaManager: Program terminates due to preceding conditions." );
          }
          if (Surfaces(wl).Construction != constructionNum) {
            // TODO Kiva: all walls must have the same construction
            ShowSevereError( "all walls must have the same construction" );
            ShowFatalError( "KivaManager: Program terminates due to preceding conditions." );
          }
        }
        if (constructionNum != foundationInputs[surface.OSCPtr].wallConstructionIndex && foundationInputs[surface.OSCPtr].wallConstructionIndex != 0) {
          // TODO Kiva: foundation wall must have the same construction as walls (or be left blank)
          ShowSevereError( "foundation wall must have the same construction as walls (or be left blank)" );
          ShowFatalError( "KivaManager: Program terminates due to preceding conditions." );
        }
        foundationInputs[surface.OSCPtr].wallConstructionIndex = constructionNum;
      }

      if (foundationInputs[surface.OSCPtr].wallConstructionIndex > 0) {
        auto& c = Constructs(foundationInputs[surface.OSCPtr].wallConstructionIndex);

        // Clear layers
        fnd.wall.layers.clear();

        // Push back construction's layers
        for (int layer = 1; layer <= c.TotLayers; layer++ ) {
          auto& mat = Materials(c.LayerPoint(layer));

          Kiva::Layer tempLayer;

          tempLayer.material = Kiva::Material(mat.Conductivity, mat.Density, mat.SpecHeat);
          tempLayer.thickness = mat.Thickness;

          fnd.wall.layers.push_back(tempLayer);
        }
      }

      // Set slab construction
      for (int i = 0; i < Constructs( surface.Construction ).TotLayers; ++i ) {
        auto& mat = Materials(Constructs( surface.Construction ).LayerPoint[i]);

        Kiva::Layer tempLayer;

        tempLayer.material = Kiva::Material(mat.Conductivity, mat.Density, mat.SpecHeat);
        tempLayer.thickness = mat.Thickness;

        fnd.slab.layers.push_back(tempLayer);
      }

      fnd.slab.emissivity = 0.0; // Long wave included in rad BC. Materials(Constructs( surface.Construction ).LayerPoint(Constructs( surface.Construction ).TotLayers)).AbsorpThermal;

      fnd.foundationDepth = wallHeight;

      fnd.hasPerimeterSurface = false; // TODO Kiva: perimeter surface for zones without exposed perimeter
      fnd.perimeterSurfaceWidth = 0.0;

      // Add blocks
      auto intHIns = foundationInputs[surface.OSCPtr].intHIns;
      auto intVIns = foundationInputs[surface.OSCPtr].intVIns;
      auto extHIns = foundationInputs[surface.OSCPtr].extHIns;
      auto extVIns = foundationInputs[surface.OSCPtr].extVIns;
      auto footing = foundationInputs[surface.OSCPtr].footing;

      if (std::abs(intHIns.width) > 0.0) {
        intHIns.z += fnd.foundationDepth + fnd.slab.totalWidth();
        fnd.inputBlocks.push_back(intHIns);
      }
      if (std::abs(intVIns.width) > 0.0) {
        fnd.inputBlocks.push_back(intVIns);
      }
      if (std::abs(extHIns.width) > 0.0) {
        extHIns.z += fnd.wall.heightAboveGrade;
        extHIns.x = fnd.wall.totalWidth();
        fnd.inputBlocks.push_back(extHIns);
      }
      if (std::abs(extVIns.width) > 0.0) {
        extVIns.x = fnd.wall.totalWidth();
        fnd.inputBlocks.push_back(extVIns);
      }
      if (std::abs(footing.width) > 0.0) {
        footing.z = fnd.foundationDepth + fnd.slab.totalWidth() + fnd.wall.depthBelowSlab;
        footing.x = fnd.wall.totalWidth()/2.0 - footing.width/2.0;
        fnd.inputBlocks.push_back(footing);
      }

      // Exposed Perimeter
      bool userSetExposedPerimeter;
      auto& expPerimMap = SurfaceGeometry::exposedFoundationPerimeter.surfaceMap;
      if (expPerimMap.count(surfNum) == 1) {
        userSetExposedPerimeter = true;
        fnd.useDetailedExposedPerimeter = expPerimMap[surfNum].useDetailedExposedPerimeter;
        if ( fnd.useDetailedExposedPerimeter ) {
          for (auto s : expPerimMap[surfNum].isExposedPerimeter) {
            fnd.isExposedPerimeter.push_back(s);
          }
        } else {
          fnd.exposedFraction = expPerimMap[surfNum].exposedFraction;
        }
      } else {
        userSetExposedPerimeter = false;
        fnd.useDetailedExposedPerimeter = true;
      }

      // polygon

      Kiva::Polygon floorPolygon;
      if (DataSurfaces::CCW) {
        for (size_t i = 0; i < surface.Vertex.size(); ++i ) {
          auto& v = surface.Vertex[i];
          floorPolygon.outer().push_back(Kiva::Point(v.x,v.y));
          if (!userSetExposedPerimeter) {
            fnd.isExposedPerimeter.push_back(true);
          }
        }
      } else {
        for (auto i = surface.Vertex.size() - 1; i <= 0; --i ) {
          auto& v = surface.Vertex[i];
          floorPolygon.outer().push_back(Kiva::Point(v.x,v.y));
          if (!userSetExposedPerimeter) {
            fnd.isExposedPerimeter.push_back(true);
          }
        }
      }

      fnd.polygon = floorPolygon;

      // add new foundation instance to list of all instances
      foundationInstances[inst] = fnd;


      // create output map for ground instance. Calculate average temperature, flux, and convection for each surface
      std::map<Kiva::Surface::SurfaceType, std::vector<Kiva::GroundOutput::OutputType>> outputMap;

      outputMap[Kiva::Surface::ST_SLAB_CORE] = {
        Kiva::GroundOutput::OT_FLUX,
        Kiva::GroundOutput::OT_TEMP,
        Kiva::GroundOutput::OT_CONV
      };

      if (fnd.hasPerimeterSurface) {
        outputMap[Kiva::Surface::ST_SLAB_PERIM] = {
          Kiva::GroundOutput::OT_FLUX,
          Kiva::GroundOutput::OT_TEMP,
          Kiva::GroundOutput::OT_CONV
        };
      }

      if (fnd.foundationDepth > 0.0) {
        outputMap[Kiva::Surface::ST_WALL_INT] = {
          Kiva::GroundOutput::OT_FLUX,
          Kiva::GroundOutput::OT_TEMP,
          Kiva::GroundOutput::OT_CONV
        };
      }

      // point surface to corresponding ground intance(s)]
      kivaInstances.emplace_back(foundationInstances[inst],
        outputMap,surfNum,wallSurfaces,surface.Zone);

      // TODO Kiva: Change for walk-out basements (each floor can point to multiple instances)
      surfaceMap[surfNum] = {inst, Kiva::Surface::ST_SLAB_CORE};

      for (auto& wl : wallSurfaces) {
        surfaceMap[wl] = {inst, Kiva::Surface::ST_WALL_INT};
      }

      inst++;

    }

    surfNum++;
  }

  for (auto& kv : kivaInstances) {
    auto& grnd = kv.ground;

    if (!kv.ground.foundation.useDetailedExposedPerimeter || !Kiva::isConvex(kv.ground.foundation.polygon))
    {
      if (kv.ground.foundation.reductionStrategy == Kiva::Foundation::RS_BOUNDARY) {
        kv.ground.foundation.reductionStrategy = Kiva::Foundation::RS_AP;
      }
    }

    if (kv.ground.foundation.reductionStrategy == Kiva::Foundation::RS_BOUNDARY)
    {
      // Adjust for concave features using Boundary Layer Adjustment method
      grnd.calculateBoundaryLayer();
      grnd.setNewBoundaryGeometry();
    }

    grnd.buildDomain();

    gio::write( DataGlobals::OutputFileInits, "(A)" ) << "! <Kiva Foundation Name>, Horizontal Cells, Vertical Cells, Total Cells, Total Exposed Perimeter, Floor Surface, Wall Surface(s)";
    std::string fmt = "(A,',',I0',',I0',',I0',',A',',A,A)";

    std::string wallSurfaceString = "";
    for (auto& wl : kv.wallSurfaces) {
      wallSurfaceString += "," + DataSurfaces::Surface(wl).Name;
    }
    gio::write( DataGlobals::OutputFileInits, fmt ) << foundationInputs[DataSurfaces::Surface(kv.floorSurface).OSCPtr].name << grnd.nX << grnd.nZ << grnd.nX*grnd.nZ << General::RoundSigDigits( grnd.foundation.netPerimeter, 2 ) << DataSurfaces::Surface(kv.floorSurface).Name << wallSurfaceString;

  }

}

void KivaManager::initKivaInstances() {

  // initialize temperatures at the beginning of run environment
  if ( DataGlobals::BeginEnvrnFlag ) {
    for (auto& kv : kivaInstances) {
      auto& grnd = kv.ground;

      // Start with steady-state solution
      // TODO Kiva: Accelerated approach? Kusuda?
      grnd.foundation.numericalScheme = Kiva::Foundation::NS_STEADY_STATE;
      kv.setBoundaryConditions();
      grnd.calculate(kv.bcs);
      grnd.foundation.numericalScheme = Kiva::Foundation::NS_ADI;
    }
  }
}

void KivaManager::calcKivaInstances() {
  // calculate heat transfer through ground
  for (auto& kv : kivaInstances) {
    auto& grnd = kv.ground;

    kv.setBoundaryConditions();
    grnd.calculate(kv.bcs,DataGlobals::MinutesPerTimeStep*60.);
    grnd.calculateSurfaceAverages();
    kv.reportKivaSurfaces();
    if (DataEnvironment::Month == 1 && DataEnvironment::DayOfMonth == 10 && DataGlobals::HourOfDay == 1 && DataGlobals::TimeStep == 1) {
      kv.plotDomain();
    }
  }
}

void KivaInstanceMap::plotDomain() {

  #ifdef GROUND_PLOT

  Kiva::SnapshotSettings ss;
  ss.dir = DataStringGlobals::outDirPathName + "/" + DataSurfaces::Surface(floorSurface).Name;
  double& l = ground.foundation.reductionLength2;
  const double width = 6.0;
  const double depth = ground.foundation.foundationDepth + width/2.0;
  const double range = max(width, depth);
  ss.xRange = {l - range/2.0, l + range/2.0};
  ss.yRange = {0.5,0.5};
  ss.zRange = {-range, ground.foundation.wall.heightAboveGrade};

  Kiva::GroundPlot gp(ss,ground.domain,ground.foundation);

  std::size_t nI =  gp.iMax - gp.iMin + 1;
  std::size_t nJ = gp.jMax - gp.jMin + 1;

  for(size_t k = gp.kMin; k <= gp.kMax; k++)
  {
    for(size_t j = gp.jMin; j <= gp.jMax; j++)
    {
      for(size_t i = gp.iMin; i <= gp.iMax; i++)
      {
        std::size_t index = (i-gp.iMin)+nI*(j-gp.jMin)+nI*nJ*(k-gp.kMin);
        if (ss.plotType == Kiva::SnapshotSettings::P_TEMP)
        {
          if (ss.outputUnits == Kiva::SnapshotSettings::IP)
            gp.TDat.a[index] = (ground.TNew[i][j][k] - 273.15)*9/5 + 32.0;
          else
            gp.TDat.a[index] = ground.TNew[i][j][k] - 273.15;
        }
        else
        {
          double du = gp.distanceUnitConversion;
          std::vector<double> Qflux = ground.calculateHeatFlux(i,j,k);
          double Qx = Qflux[0];
          double Qy = Qflux[1];
          double Qz = Qflux[2];
          double Qmag = sqrt(Qx*Qx + Qy*Qy + Qz*Qz);

          if (ss.fluxDir == Kiva::SnapshotSettings::D_M)
            gp.TDat.a[index] = Qmag/(du*du);
          else if (ss.fluxDir == Kiva::SnapshotSettings::D_X)
            gp.TDat.a[index] = Qx/(du*du);
          else if (ss.fluxDir == Kiva::SnapshotSettings::D_Y)
            gp.TDat.a[index] = Qy/(du*du);
          else if (ss.fluxDir == Kiva::SnapshotSettings::D_Z)
            gp.TDat.a[index] = Qz/(du*du);
        }
      }
    }
  }

  gp.createFrame(std::to_string(DataEnvironment::Month) + "/" + std::to_string(DataEnvironment::DayOfMonth) + " " + std::to_string(DataGlobals::HourOfDay) + ":00");

  #endif

}

Real64 KivaManager::getValue(int surfNum,Kiva::GroundOutput::OutputType oT) {
  auto& kI = kivaInstances[surfaceMap[surfNum].first];
  auto& st = surfaceMap[surfNum].second;

  return kI.ground.getSurfaceAverageValue({st,oT});
}

Real64 KivaManager::getTemp(int surfNum) {
  return getValue(surfNum, Kiva::GroundOutput::OT_TEMP) - DataGlobals::KelvinConv;
}

Real64 KivaManager::getConv(int surfNum) {
  auto conv = getValue(surfNum, Kiva::GroundOutput::OT_CONV);
  assert(conv > 0.0);
  return conv;

}

void KivaManager::defineDefaultFoundation() {

  Kiva::Foundation defFnd;

  // From settings
  defFnd.soil = Kiva::Material(settings.soilK, settings.soilRho, settings.soilCp);
  defFnd.soilAbsorptivity = settings.groundSolarAbs;
  defFnd.soilEmissivity = settings.groundThermalAbs;
  defFnd.surfaceRoughness = settings.groundRoughness;
  defFnd.farFieldWidth = settings.farFieldWidth;

  Real64 waterTableDepth = 0.1022*DataEnvironment::Elevation;

  if (settings.deepGroundBoundary == Settings::AUTO) {
    if (waterTableDepth <= 40.) {
      defFnd.deepGroundDepth = waterTableDepth;
      defFnd.deepGroundBoundary = Kiva::Foundation::DGB_CONSTANT_TEMPERATURE;
      defFnd.deepGroundTemperature = DataEnvironment::AnnualAverageDrybulbTemp + DataGlobals::KelvinConv;
    } else {
      defFnd.deepGroundDepth = 40.;
      defFnd.deepGroundBoundary = Kiva::Foundation::DGB_ZERO_FLUX;
    }
  } else if (settings.deepGroundBoundary == Settings::ZERO_FLUX) {
    defFnd.deepGroundDepth = settings.deepGroundDepth;
    defFnd.deepGroundBoundary = Kiva::Foundation::DGB_ZERO_FLUX;
  } else /* if (settings.deepGroundBoundary == Settings::GROUNDWATER) */ {
    defFnd.deepGroundDepth = settings.deepGroundDepth;
    defFnd.deepGroundBoundary = Kiva::Foundation::DGB_CONSTANT_TEMPERATURE;
    defFnd.deepGroundTemperature = DataEnvironment::AnnualAverageDrybulbTemp + DataGlobals::KelvinConv;
  }

  defFnd.wall.heightAboveGrade = 0.2; // m

  Kiva::Material concrete;
  concrete.conductivity = 1.95;  // W/m-K
  concrete.density = 2400;  // kg/m3
  concrete.specificHeat = 900;  // J/kg-K

  Kiva::Layer defaultFoundationWall;
  defaultFoundationWall.thickness = 0.3; // m
  defaultFoundationWall.material = concrete;

  defFnd.wall.layers.push_back(defaultFoundationWall);

  defFnd.wall.interiorEmissivity = 0.9;
  defFnd.wall.exteriorEmissivity = 0.9;
  defFnd.wall.exteriorAbsorptivity = 0.9;

  defFnd.wall.depthBelowSlab = 0.0;

  defFnd.mesh.minCellDim = settings.minCellDim;
  defFnd.mesh.maxNearGrowthCoeff = settings.maxGrowthCoeff;
  defFnd.mesh.maxDepthGrowthCoeff = settings.maxGrowthCoeff;
  defFnd.mesh.maxInteriorGrowthCoeff = settings.maxGrowthCoeff;
  defFnd.mesh.maxExteriorGrowthCoeff = settings.maxGrowthCoeff;


  defaultFoundation.foundation = defFnd;
  defaultFoundation.name = "<Default Foundation>";


}

void KivaManager::addDefaultFoundation() {

  foundationInputs.push_back(defaultFoundation);
  defaultIndex = foundationInputs.size() - 1;
  defaultSet = true;

}

int KivaManager::findFoundation(std::string name) {
  int fndNum = 0;
  for (auto& fnd : foundationInputs) {
    // Check if foundation exists
    if ( fnd.name == name) {
      return fndNum;
    }
    fndNum++;
  }
  return (int)foundationInputs.size();

}

} // HeatBalanceKivaManager
} // EnergyPlus
