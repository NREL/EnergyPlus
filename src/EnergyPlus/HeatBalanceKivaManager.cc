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

  bcs.wallAbsRadiation = QAtotal/Atotal;

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

KivaManager::KivaManager() {
  LIS_INT dummy_argc = 0;
  char** dummy_argv;
  lis_initialize(&dummy_argc, &dummy_argv);
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
              // TODO Kiva: only one floundation floor per zone
              ShowSevereError( "only one foundation floor per zone" );
            } else {
              // TODO Kiva: only floor and wall surfaces allowed
              ShowSevereError( "only floor and wall surfaces allowed" );
            }
          } else {
            wallSurfaces.push_back(wl);
          }
        }
      }

      // All wall surfaces must have the same construction
      if (wallSurfaces.size() != 0) {
        int constructionNum = Surfaces(wallSurfaces[0]).Construction;
        for (auto& wl : wallSurfaces) {
          if (Surfaces(wl).Construction != constructionNum) {
            // TODO Kiva: all walls must have the same construction
            ShowSevereError( "all walls must have the same construction" );
          }
        }
      }

      // Set slab construction
      for (int i = 0; i < Constructs( surface.Construction ).TotLayers; ++i ) {
        auto& mat = Constructs( surface.Construction ).LayerPoint[i];

        Kiva::Material tempMat;

        tempMat.conductivity = Materials(mat).Conductivity;
        tempMat.density = Materials(mat).Density;
        tempMat.specificHeat = Materials(mat).SpecHeat;

        Kiva::Layer tempLayer;

        tempLayer.material = tempMat;
        tempLayer.thickness = Materials(mat).Thickness;

        fnd.slab.layers.push_back(tempLayer);
      }

      fnd.slab.emissivity = 0.0; // Long wave included in rad BC. Materials(Constructs( surface.Construction ).LayerPoint(Constructs( surface.Construction ).TotLayers)).AbsorpThermal;

      // Set wall construction

      // put together the rest of kiva Foundation instance based on E+ geometry
      Real64 wallHeight = 0.0;
      if (wallSurfaces.size() != 0) {
        wallHeight = Surfaces(wallSurfaces[0]).Height; // TODO Kiva: each wall with different height gets its own instance.
        for (auto& wl : wallSurfaces) {
          if (Surfaces(wl).Height != wallHeight) {
            // TODO Kiva: all walls must be the same height
            ShowSevereError( "all walls must be the same height" );
          }
        }
      }

      fnd.foundationDepth = wallHeight;

      fnd.hasPerimeterSurface = false; // TODO Kiva: perimeter surface for zones without exposed perimeter
      fnd.perimeterSurfaceWidth = 0.0;

      // polygon
      if (DataSurfaces::CCW) {
        for (size_t i = 0; i < surface.Vertex.size_; ++i ) {
          auto& v = surface.Vertex[i];
          fnd.polygon.outer().push_back(Kiva::Point(v.x,v.y));
        }
      } else {
        for (auto i = surface.Vertex.size_ - 1; i <= 0; --i ) {
          auto& v = surface.Vertex[i];
          fnd.polygon.outer().push_back(Kiva::Point(v.x,v.y));
        }
      }

      // add new foundation instance to list of all instances
      foundationInstances.push_back(fnd);


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
      kivaInstances.push_back(KivaInstanceMap(foundationInstances[inst],
        outputMap,surfNum,wallSurfaces,surface.Zone));

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

    // Adjust for concave features using Boundary Layer Adjustment method
    grnd.calculateBoundaryLayer();
    grnd.setNewBoundaryGeometry();

    grnd.buildDomain();

    // TODO Kiva: Add wall surfaces to EIO
    gio::write( DataGlobals::OutputFileInits, "(A)" ) << "! <Kiva Foundation Name>, Horizontal Cells, Vertical Cells, Total Cells, Floor Surface, Wall Surface(s)";
		gio::write( DataGlobals::OutputFileInits, "(A,',',I5',',I5',',I5',',A)" ) << foundationInputs[DataSurfaces::Surface(kv.floorSurface).OSCPtr].name << grnd.nX << grnd.nZ << grnd.nX*grnd.nZ << DataSurfaces::Surface(kv.floorSurface).Name;

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
    kv.plotDomain();

  }
}

void KivaInstanceMap::plotDomain() {

  #ifdef GROUND_PLOT

  if (DataEnvironment::Month == 1 && DataEnvironment::DayOfMonth == 2 && DataGlobals::HourOfDay == 2 && DataGlobals::TimeStep == 1) {
    Kiva::SnapshotSettings ss;
    ss.dir = DataStringGlobals::outDirPathName + "/snapshot";
    double& l = ground.foundation.reductionLength2;
    const double width = 6.0;
    const double depth = ground.foundation.foundationDepth + width/2.0;
    const double range = max(width, depth);
    ss.xRange = {l - range/2.0, l + range/2.0};
    ss.yRange = {0.5,0.5};
    ss.zRange = {-range, ground.foundation.wall.heightAboveGrade};

    Kiva::GroundPlot gp(ss,ground.domain,ground.foundation.blocks);

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

    gp.createFrame();
  }

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
  return getValue(surfNum, Kiva::GroundOutput::OT_CONV);
}

void KivaManager::defineDefaultFoundation() {


  Kiva::Foundation defFnd;

  defFnd.hasWall = true;
  defFnd.hasWall = true;
  defFnd.hasInteriorHorizontalInsulation = false;
  defFnd.hasExteriorHorizontalInsulation = false;
  defFnd.hasInteriorVerticalInsulation = false;
  defFnd.hasExteriorVerticalInsulation = false;

  defFnd.wall.heightAboveGrade = 0.2; // m

  Kiva::Material concrete;
  concrete.conductivity = 1.98;  // W/m-K
  concrete.density = 1900;  // kg/m3
  concrete.specificHeat = 1800;  // J/kg-K

  Kiva::Layer defaultFoundationWall;
  defaultFoundationWall.thickness = 0.3; // m
  defaultFoundationWall.material = concrete;

  defFnd.wall.layers.push_back(defaultFoundationWall);

  defFnd.wall.interiorEmissivity = 0.9;
  defFnd.wall.exteriorEmissivity = 0.9;
  defFnd.wall.exteriorAbsorptivity = 0.9;


  defFnd.wall.footerDepth = 0.3;

  Kiva::Material typicalSoil;
  typicalSoil.conductivity = 0.864;  // W/m-K
  typicalSoil.density = 1510;  // kg/m3
  typicalSoil.specificHeat = 1260;  // J/kg-K

  defFnd.soil = typicalSoil;

  defFnd.soilAbsorptivity = 0.9;
  defFnd.soilEmissivity = 0.9;
  defFnd.surfaceRoughness = 0.03; // m

  defFnd.farFieldWidth = 40.; // m

  Real64 waterTableDepth = 0.1022*DataEnvironment::Elevation;

  if (waterTableDepth <= 40.) {
    defFnd.deepGroundDepth = waterTableDepth;
    defFnd.deepGroundBoundary = Kiva::Foundation::DGB_CONSTANT_TEMPERATURE;
    defFnd.deepGroundTemperature = DataEnvironment::AnnualAverageDrybulbTemp + DataGlobals::KelvinConv;
  } else {
    defFnd.deepGroundDepth = 40.;
    defFnd.deepGroundBoundary = Kiva::Foundation::DGB_ZERO_FLUX;
  }

  defFnd.coordinateSystem = Kiva::Foundation::CS_CARTESIAN;
  defFnd.reductionStrategy = Kiva::Foundation::RS_BOUNDARY;
  defFnd.numberOfDimensions = 2;
  defFnd.useSymmetry = true;

  defFnd.buildingHeight = 0.0; // not used

  // Numeric settings
  defFnd.numericalScheme = Kiva::Foundation::NS_ADI;
  defFnd.fADI = 0.00001;
  defFnd.solver = "bicgstab";
  defFnd.preconditioner = "ilu";
  defFnd.maxIterations = 100000;
  defFnd.tolerance = 1.0e-6;

  defFnd.mesh.minCellDim = 0.02;
  defFnd.mesh.maxNearGrowthCoeff = 1.5;
  defFnd.mesh.maxDepthGrowthCoeff = 1.5;
  defFnd.mesh.maxInteriorGrowthCoeff = 1.5;
  defFnd.mesh.maxExteriorGrowthCoeff = 1.5;

  defFnd.convectionCalculationMethod = Kiva::Foundation::CCM_AUTO;
  defFnd.wallTopBoundary = Kiva::Foundation::WTB_ZERO_FLUX;

  defaultFoundation.foundation = defFnd;
  defaultFoundation.name = "<Default Foundation>";

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
