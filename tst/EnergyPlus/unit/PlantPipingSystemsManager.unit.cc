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

// EnergyPlus::PlantPipingSystemsManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include "EnergyPlus/DataSurfaces.hh"
#include "EnergyPlus/HeatBalanceManager.hh"
#include "EnergyPlus/PlantPipingSystemsManager.hh"
#include "EnergyPlus/SurfaceGeometry.hh"

using namespace EnergyPlus;
using namespace PlantPipingSystemsManager;
using DataSurfaces::Surface;
using HeatBalanceManager::GetMaterialData;
using SurfaceGeometry::GetOSCMData;

TEST_F( EnergyPlusFixture, SiteGroundDomainSlab_CheckInputs_CorrectInputs ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Slab,",
			"CoupledSlab,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,	!- Name of Floor Boundary Condition Model",
			"InGrade,		!- Slab Location (InGrade/OnGrade)",
			"Dummy Material,	!- Slab Material Name",
			"Yes,			!- Horizontal Insulation (Yes/No)",
			"Dummy Material,	!- Horizontal Insulation Material Name",
			"Full,			!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			",				!- Perimeter insulation width (m)",
			"Yes,			!- Vertical Insulation (Yes/No)",
			"Dummy Material,	!- Vertical Insulation Name",
			"1.5,			!- Vertical perimeter insulation depth from surface (m)",
			"Hourly;		!- Domain Simulation Interval. (Timestep/Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadZoneCoupledDomainInputs( 1, 1, errorsFound );

	EXPECT_FALSE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainSlab_CheckInputs_BadOSCMName ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Slab,",
			"CoupledSlab,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSC,	!- Name of Floor Boundary Condition Model",
			"InGrade,		!- Slab Location (InGrade/OnGrade)",
			"Dummy Material,	!- Slab Material Name",
			"Yes,			!- Horizontal Insulation (Yes/No)",
			"Dummy Material,	!- Horizontal Insulation Material Name",
			"Full,			!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			",				!- Perimeter insulation width (m)",
			"Yes,			!- Vertical Insulation (Yes/No)",
			"Dummy Material,	!- Vertical Insulation Name",
			"1.5,			!- Vertical perimeter insulation depth from surface (m)",
			"Hourly;		!- Domain Simulation Interval. (Timestep/Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadZoneCoupledDomainInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainSlab_CheckInputs_BadSlabLocation ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Slab,",
			"CoupledSlab,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,	!- Name of Floor Boundary Condition Model",
			"InGade,		!- Slab Location (InGrade/OnGrade)",
			"Dummy Material,	!- Slab Material Name",
			"Yes,			!- Horizontal Insulation (Yes/No)",
			"Dummy Material,	!- Horizontal Insulation Material Name",
			"Full,			!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			",				!- Perimeter insulation width (m)",
			"Yes,			!- Vertical Insulation (Yes/No)",
			"Dummy Material,	!- Vertical Insulation Name",
			"1.5,			!- Vertical perimeter insulation depth from surface (m)",
			"Hourly;		!- Domain Simulation Interval. (Timestep/Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadZoneCoupledDomainInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainSlab_CheckInputs_BadSlabMaterialName ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Slab,",
			"CoupledSlab,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,	!- Name of Floor Boundary Condition Model",
			"InGrade,		!- Slab Location (InGrade/OnGrade)",
			"Dummy Materia,	!- Slab Material Name",
			"Yes,			!- Horizontal Insulation (Yes/No)",
			"Dummy Material,	!- Horizontal Insulation Material Name",
			"Full,			!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			",				!- Perimeter insulation width (m)",
			"Yes,			!- Vertical Insulation (Yes/No)",
			"Dummy Material,	!- Vertical Insulation Name",
			"1.5,			!- Vertical perimeter insulation depth from surface (m)",
			"Hourly;		!- Domain Simulation Interval. (Timestep/Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadZoneCoupledDomainInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainSlab_CheckInputs_BadHorizInsSelection ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Slab,",
			"CoupledSlab,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,	!- Name of Floor Boundary Condition Model",
			"InGrade,		!- Slab Location (InGrade/OnGrade)",
			"Dummy Material,	!- Slab Material Name",
			"Ye,			!- Horizontal Insulation (Yes/No)",
			"Dummy Material,	!- Horizontal Insulation Material Name",
			"Full,			!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			",				!- Perimeter insulation width (m)",
			"Yes,			!- Vertical Insulation (Yes/No)",
			"Dummy Material,	!- Vertical Insulation Name",
			"1.5,			!- Vertical perimeter insulation depth from surface (m)",
			"Hourly;		!- Domain Simulation Interval. (Timestep/Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadZoneCoupledDomainInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainSlab_CheckInputs_BadHorizInsMaterialName ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Slab,",
			"CoupledSlab,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,	!- Name of Floor Boundary Condition Model",
			"InGrade,		!- Slab Location (InGrade/OnGrade)",
			"Dummy Material,	!- Slab Material Name",
			"Yes,			!- Horizontal Insulation (Yes/No)",
			"Dummy Materal,	!- Horizontal Insulation Material Name",
			"Full,			!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			",				!- Perimeter insulation width (m)",
			"Yes,			!- Vertical Insulation (Yes/No)",
			"Dummy Material,	!- Vertical Insulation Name",
			"1.5,			!- Vertical perimeter insulation depth from surface (m)",
			"Hourly;		!- Domain Simulation Interval. (Timestep/Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadZoneCoupledDomainInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainSlab_CheckInputs_BadHorizInsExtentsSelection ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Slab,",
			"CoupledSlab,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,	!- Name of Floor Boundary Condition Model",
			"InGrade,		!- Slab Location (InGrade/OnGrade)",
			"Dummy Material,	!- Slab Material Name",
			"Yes,			!- Horizontal Insulation (Yes/No)",
			"Dummy Material,	!- Horizontal Insulation Material Name",
			"Ful,			!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			",				!- Perimeter insulation width (m)",
			"Yes,			!- Vertical Insulation (Yes/No)",
			"Dummy Material,	!- Vertical Insulation Name",
			"1.5,			!- Vertical perimeter insulation depth from surface (m)",
			"Hourly;		!- Domain Simulation Interval. (Timestep/Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadZoneCoupledDomainInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainSlab_CheckInputs_PerimeterInsulationWidth ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Slab,",
			"CoupledSlab,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,	!- Name of Floor Boundary Condition Model",
			"InGrade,		!- Slab Location (InGrade/OnGrade)",
			"Dummy Material,	!- Slab Material Name",
			"Yes,			!- Horizontal Insulation (Yes/No)",
			"Dummy Material,	!- Horizontal Insulation Material Name",
			"Perimeter,			!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			",				!- Perimeter insulation width (m)",
			"Yes,			!- Vertical Insulation (Yes/No)",
			"Dummy Material,	!- Vertical Insulation Name",
			"1.5,			!- Vertical perimeter insulation depth from surface (m)",
			"Hourly;		!- Domain Simulation Interval. (Timestep/Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadZoneCoupledDomainInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainSlab_CheckInputs_BadVertInsSelection ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Slab,",
			"CoupledSlab,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,	!- Name of Floor Boundary Condition Model",
			"InGrade,		!- Slab Location (InGrade/OnGrade)",
			"Dummy Material,	!- Slab Material Name",
			"Yes,			!- Horizontal Insulation (Yes/No)",
			"Dummy Material,	!- Horizontal Insulation Material Name",
			"Full,			!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			",				!- Perimeter insulation width (m)",
			"Ye,			!- Vertical Insulation (Yes/No)",
			"Dummy Material,	!- Vertical Insulation Name",
			"1.5,			!- Vertical perimeter insulation depth from surface (m)",
			"Hourly;		!- Domain Simulation Interval. (Timestep/Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadZoneCoupledDomainInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainSlab_CheckInputs_BadVertInsMaterialName ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Slab,",
			"CoupledSlab,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,	!- Name of Floor Boundary Condition Model",
			"InGrade,		!- Slab Location (InGrade/OnGrade)",
			"Dummy Material,	!- Slab Material Name",
			"Yes,			!- Horizontal Insulation (Yes/No)",
			"Dummy Material,	!- Horizontal Insulation Material Name",
			"Full,			!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			",				!- Perimeter insulation width (m)",
			"Yes,			!- Vertical Insulation (Yes/No)",
			"Dummy Materia,	!- Vertical Insulation Name",
			"1.5,			!- Vertical perimeter insulation depth from surface (m)",
			"Hourly;		!- Domain Simulation Interval. (Timestep/Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadZoneCoupledDomainInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainSlab_CheckInputs_BadVertInsDepth ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Slab,",
			"CoupledSlab,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,	!- Name of Floor Boundary Condition Model",
			"InGrade,		!- Slab Location (InGrade/OnGrade)",
			"Dummy Material,	!- Slab Material Name",
			"Yes,			!- Horizontal Insulation (Yes/No)",
			"Dummy Material,	!- Horizontal Insulation Material Name",
			"Full,			!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			",				!- Perimeter insulation width (m)",
			"Yes,			!- Vertical Insulation (Yes/No)",
			"Dummy Material,	!- Vertical Insulation Name",
			",			!- Vertical perimeter insulation depth from surface (m)",
			"Hourly;		!- Domain Simulation Interval. (Timestep/Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadZoneCoupledDomainInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainSlab_CheckInputs_BadTimeStepSelection ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Slab,",
			"CoupledSlab,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,	!- Name of Floor Boundary Condition Model",
			"InGrade,		!- Slab Location (InGrade/OnGrade)",
			"Dummy Material,	!- Slab Material Name",
			"Yes,			!- Horizontal Insulation (Yes/No)",
			"Dummy Material,	!- Horizontal Insulation Material Name",
			"Full,			!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			",				!- Perimeter insulation width (m)",
			"Yes,			!- Vertical Insulation (Yes/No)",
			"Dummy Material,	!- Vertical Insulation Name",
			"1.5,			!- Vertical perimeter insulation depth from surface (m)",
			"Hourl;		!- Domain Simulation Interval. (Timestep/Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadZoneCoupledDomainInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainBasement_CheckInputs_CorrectInputs ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Basement,",
			"CoupledBasement,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Horizontal Underfloor Insulation Present (Yes/No)",
			"Dummy Material,		!- Basement Horizontal Insulation Underfloor Material Name",
			"Perimeter,				!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			"1,						!- Perimeter width (m)",
			"2.5,					!- Depth of Basement Wall In Ground Domain {m}",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Wall Vertical Insulation Present(Yes/No)",
			"Dummy Material,		!- Basement Wall Vertical Insulation Material Name",
			"2.3,					!- Vertical insulation depth from surface (m)",
			"timestep;				!- Domain Update interval. (Timestep, Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadBasementInputs( 1, 1, errorsFound );

	EXPECT_FALSE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainBasement_CheckInputs_BadOSCMName ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Basement,",
			"CoupledBasement,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSC,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Horizontal Underfloor Insulation Present (Yes/No)",
			"Dummy Material,		!- Basement Horizontal Insulation Underfloor Material Name",
			"Perimeter,				!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			"1,						!- Perimeter width (m)",
			"2.5,					!- Depth of Basement Wall In Ground Domain {m}",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Wall Vertical Insulation Present(Yes/No)",
			"Dummy Material,		!- Basement Wall Vertical Insulation Material Name",
			"2.3,					!- Vertical insulation depth from surface (m)",
			"timestep;				!- Domain Update interval. (Timestep, Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadBasementInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainBasement_CheckInputs_BadHorizInsSelection ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Basement,",
			"CoupledBasement,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Ye,					!- Basement Horizontal Underfloor Insulation Present (Yes/No)",
			"Dummy Material,		!- Basement Horizontal Insulation Underfloor Material Name",
			"Perimeter,				!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			"1,						!- Perimeter width (m)",
			"2.5,					!- Depth of Basement Wall In Ground Domain {m}",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Wall Vertical Insulation Present(Yes/No)",
			"Dummy Material,		!- Basement Wall Vertical Insulation Material Name",
			"2.3,					!- Vertical insulation depth from surface (m)",
			"timestep;				!- Domain Update interval. (Timestep, Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadBasementInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainBasement_CheckInputs_BadHorizInsMaterial ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Basement,",
			"CoupledBasement,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Horizontal Underfloor Insulation Present (Yes/No)",
			"Dummy Materia,			!- Basement Horizontal Insulation Underfloor Material Name",
			"Perimeter,				!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			"1,						!- Perimeter width (m)",
			"2.5,					!- Depth of Basement Wall In Ground Domain {m}",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Wall Vertical Insulation Present(Yes/No)",
			"Dummy Material,		!- Basement Wall Vertical Insulation Material Name",
			"2.3,					!- Vertical insulation depth from surface (m)",
			"timestep;				!- Domain Update interval. (Timestep, Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadBasementInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainBasement_CheckInputs_BadHorizInsExtents ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Basement,",
			"CoupledBasement,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Horizontal Underfloor Insulation Present (Yes/No)",
			"Dummy Material,			!- Basement Horizontal Insulation Underfloor Material Name",
			"Perimete,				!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			"1,						!- Perimeter width (m)",
			"2.5,					!- Depth of Basement Wall In Ground Domain {m}",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Wall Vertical Insulation Present(Yes/No)",
			"Dummy Material,		!- Basement Wall Vertical Insulation Material Name",
			"2.3,					!- Vertical insulation depth from surface (m)",
			"timestep;				!- Domain Update interval. (Timestep, Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadBasementInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainBasement_CheckInputs_BadBasementDepth ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Basement,",
			"CoupledBasement,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Horizontal Underfloor Insulation Present (Yes/No)",
			"Dummy Material,		!- Basement Horizontal Insulation Underfloor Material Name",
			"Perimeter,				!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			"1,						!- Perimeter width (m)",
			"25,					!- Depth of Basement Wall In Ground Domain {m}",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Wall Vertical Insulation Present(Yes/No)",
			"Dummy Material,		!- Basement Wall Vertical Insulation Material Name",
			"2.3,					!- Vertical insulation depth from surface (m)",
			"timestep;				!- Domain Update interval. (Timestep, Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadBasementInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainBasement_CheckInputs_BadFloorOSCMName ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Basement,",
			"CoupledBasement,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Horizontal Underfloor Insulation Present (Yes/No)",
			"Dummy Material,		!- Basement Horizontal Insulation Underfloor Material Name",
			"Perimeter,				!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			"1,						!- Perimeter width (m)",
			"2.5,					!- Depth of Basement Wall In Ground Domain {m}",
			"GroundCoupledOSC,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Wall Vertical Insulation Present(Yes/No)",
			"Dummy Material,		!- Basement Wall Vertical Insulation Material Name",
			"2.3,					!- Vertical insulation depth from surface (m)",
			"timestep;				!- Domain Update interval. (Timestep, Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadBasementInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainBasement_CheckInputs_BadVertInsSelection ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Basement,",
			"CoupledBasement,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Horizontal Underfloor Insulation Present (Yes/No)",
			"Dummy Material,		!- Basement Horizontal Insulation Underfloor Material Name",
			"Perimeter,				!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			"1,						!- Perimeter width (m)",
			"2.5,					!- Depth of Basement Wall In Ground Domain {m}",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Ye,					!- Basement Wall Vertical Insulation Present(Yes/No)",
			"Dummy Material,		!- Basement Wall Vertical Insulation Material Name",
			"2.3,					!- Vertical insulation depth from surface (m)",
			"timestep;				!- Domain Update interval. (Timestep, Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadBasementInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainBasement_CheckInputs_BadVertInsName ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Basement,",
			"CoupledBasement,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Horizontal Underfloor Insulation Present (Yes/No)",
			"Dummy Material,		!- Basement Horizontal Insulation Underfloor Material Name",
			"Perimeter,				!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			"1,						!- Perimeter width (m)",
			"2.5,					!- Depth of Basement Wall In Ground Domain {m}",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Wall Vertical Insulation Present(Yes/No)",
			"Dummy Materia,			!- Basement Wall Vertical Insulation Material Name",
			"2.3,					!- Vertical insulation depth from surface (m)",
			"timestep;				!- Domain Update interval. (Timestep, Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadBasementInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}

TEST_F( EnergyPlusFixture, SiteGroundDomainBasement_CheckInputs_BadTimestepSelection ) {

	std::string const idf_objects = delimited_string({
		"Version,8.5;",
		"Site:GroundDomain:Basement,",
			"CoupledBasement,	!- Name",
			"5,				!- Ground Domain Depth {m}",
			"1,				!- Aspect Ratio",
			"5,				!- Domain Perimeter Offset {m}",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"30,			!- Soil Moisture Content Volume Fraction {percent}",
			"50,			!- Soil Moisture Content Volume Fraction at Saturation {percent}",
			"Site:GroundTemperature:Undisturbed:KusudaAchenbach,	!- Type of Undisturbed Ground Temperature Model",
			"KATemps,		!- Name of Undisturbed Ground Temperature Model",
			"1,				!- Evapotranspiration Ground Cover Parameter",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Horizontal Underfloor Insulation Present (Yes/No)",
			"Dummy Material,		!- Basement Horizontal Insulation Underfloor Material Name",
			"Perimeter,				!- Full Horizontal or Perimeter Only (Full/Perimeter)",
			"1,						!- Perimeter width (m)",
			"2.5,					!- Depth of Basement Wall In Ground Domain {m}",
			"GroundCoupledOSCM,		!- Name of Basement Floor Boundary Condition Model",
			"Yes,					!- Basement Wall Vertical Insulation Present(Yes/No)",
			"Dummy Material,		!- Basement Wall Vertical Insulation Material Name",
			"2.3,					!- Vertical insulation depth from surface (m)",
			"timeste;				!- Domain Update interval. (Timestep, Hourly)",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
			"KATemps,		!- Name of object",
			"1.8,			!- Soil Thermal Conductivity {W/m-K}",
			"3200,			!- Soil Density {kg/m3}",
			"836,			!- Soil Specific Heat {J/kg-K}",
			"15.5,			!- Annual average surface temperature {C}",
			"12.8,			!- Annual amplitude of surface temperature {delta C}",
			"17.3;			!- Phase shift of minimum surface temperature {days}",
		"SurfaceProperty:OtherSideConditionsModel,",
			"GroundCoupledOSCM,		!- Name",
			"GroundCoupledSurface;	!- Type of Modeling",
		"Material,",
			"Dummy Material, !- Name",
			"MediumRough,	!- Roughness",
			"0.1397,		!- Thickness {m}",
			"1.8,			!- Conductivity {W/m-K}",
			"2400,			!- Density {kg/m3}",
			"750,			!- Specific Heat {J/kg-K}",
			"0.9,			!- Thermal Absorptance",
			"0.65,			!- Solar Absorptance",
			"0.65;			!- Visible Absorptance",
		});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Dummy surface
	Surface.allocate( 1 );
	Surface( 1 ).OSCMPtr = 1;
	Surface( 1 ).Area = 100;

	PipingSystemDomains.allocate( 1 );

	bool errorsFound = false;

	// Other necessary inputs
	GetOSCMData( errorsFound );
	GetMaterialData( errorsFound );

	ReadBasementInputs( 1, 1, errorsFound );

	EXPECT_TRUE( errorsFound );
}
