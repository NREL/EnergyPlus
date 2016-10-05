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

// EnergyPlus Headers
#include <HeatBalanceKivaManager.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataGlobals.hh>
#include <DataSurfaces.hh>
#include <DisplayRoutines.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {
namespace HeatBalanceKivaManager {

KivaInstanceMap::KivaInstanceMap(Kiva::Foundation& foundation,
  std::map<Kiva::Surface::SurfaceType, std::vector<Kiva::GroundOutput::OutputType>> oM,
  int floorSurface,
  Array1D< int > wallSurfaces,
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

}

void KivaInstanceMap::reportKivaSurfaces() {
  // Calculate inside face values
  Real64 const qFloor = -(bcs.slabAbsRadiation + DataHeatBalance::HConvIn(floorSurface)*(DataHeatBalFanSys::MAT(zoneNum) - DataHeatBalSurface::TempSurfIn(floorSurface)));

  DataHeatBalSurface::OpaqSurfInsFaceConductionFlux( floorSurface ) = qFloor;
  DataHeatBalSurface::OpaqSurfInsFaceConduction( floorSurface ) = qFloor * DataSurfaces::Surface(floorSurface).Area; // for reporting as in CTF, PT
  // TODO Kiva: Repeat for walls
}

KivaManager::KivaManager() {
  LIS_INT dummy_argc = 0;
  char** dummy_argv;
  lis_initialize(&dummy_argc, &dummy_argv);
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
      // TODO Kiva: Setup

      // Copy foundation input for this instance
      Kiva::Foundation fnd = foundationInputs[surface.OSCPtr].foundation;

      // TODO Kiva: Find other surfaces associated with the same floor

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

      // TODO Kiva: expose interior solar absorptivity

      // TODO Kiva: wall layers + emissivity/absorptivity

      // put together the rest of kiva Foundation instance based on E+ geometry
      Real64 wallHeight = 0.0; // TODO each wall with different height gets its own instance.
      fnd.foundationDepth = wallHeight;

      fnd.orientation = 0.0; // TODO orientation from Zone...do we need this?

      fnd.hasPerimeterSurface = false; // TODO perimeter surface for zones without exposed perimeter
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

      // TODO Kiva: create wallSurface list
      Array1D< int > wallSurfaces;

      // point surface to corresponding ground intance(s)]
      kivaInstances.push_back(KivaInstanceMap(foundationInstances[inst],
        outputMap,surfNum,wallSurfaces,surface.Zone));

      surfaceMap[surfNum] = {inst, Kiva::Surface::ST_SLAB_CORE};

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

    // TODO Kiva: Accelerated approach? Kusuda?
    kv.setBoundaryConditions();
    grnd.calculate(kv.bcs,DataGlobals::MinutesPerTimeStep*60.);
    grnd.calculateSurfaceAverages();
    kv.reportKivaSurfaces();

  }
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


  defFnd.wall.height = 0.3; // Footer depth? TODO make relative to foundation depth

  Kiva::Material typicalSoil;
  typicalSoil.conductivity = 0.864;  // W/m-K
  typicalSoil.density = 1510;  // kg/m3
  typicalSoil.specificHeat = 1260;  // J/kg-K

  defFnd.soil = typicalSoil;

  defFnd.soilAbsorptivity = 0.9;
  defFnd.soilEmissivity = 0.9;
  defFnd.surfaceRoughness = 0.03; // m

  // TODO Kiva: terrain pamarameters

  defFnd.farFieldWidth = 40.; // m

  Real64 waterTableDepth = 0.1022*DataEnvironment::Elevation;

  if (waterTableDepth <= 40.) {
    defFnd.deepGroundDepth = waterTableDepth;
    defFnd.deepGroundBoundary = Kiva::Foundation::DGB_CONSTANT_TEMPERATURE;
    defFnd.deepGroundTemperature = DataEnvironment::AnnualAverageDrybulbTemp;
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
  defaultFoundation.name = "Default Foundation";

  foundationInputs.push_back(defaultFoundation);

}

} // HeatBalanceKivaManager
} // EnergyPlus
