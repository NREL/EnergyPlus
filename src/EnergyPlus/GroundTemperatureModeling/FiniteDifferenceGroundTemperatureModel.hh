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

#ifndef FiniteDifferenceGroundTemperatureModel_hh_INCLUDED
#define FiniteDifferenceGroundTemperatureModel_hh_INCLUDED

// C++ Headers
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <GroundTemperatureModeling/BaseGroundTemperatureModel.hh>

namespace EnergyPlus {

	// Derived class for Finite-Difference Model
	class FiniteDiffGroundTempsModel : public BaseGroundTempsModel {
		
		public:
			Real64 baseConductivity;
			Real64 baseDensity;
			Real64 baseSpecificHeat;
			int totalNumCells;
			Real64 timeStepInSeconds;
			Real64 evapotransCoeff;
			Real64 saturatedWaterContent; 
			Real64 waterContent;
			Real64 annualAveAirTemp;
			Real64 minDailyAirTemp; // Set hi. Will be reset later
			Real64 maxDailyAirTemp; // Set low. Will be reset later
			Real64 dayOfMinDailyAirTemp;
			Real64 depth;
			Real64 simTimeInDays;

		// Default constructor
		FiniteDiffGroundTempsModel() :
			minDailyAirTemp( 100.0 ),
			maxDailyAirTemp( -100.0 ),
			dayOfMinDailyAirTemp( 1 )

		{}

		struct instanceOfCellData {

			struct properties
			{
				Real64 conductivity;
				Real64 density;
				Real64 specificHeat;
				Real64 diffusivity;
				Real64 rhoCp;
			};

			properties props;
		
			int index;
			Real64 thickness;
			Real64 minZValue;
			Real64 maxZValue;
			Real64 temperature;
			Real64 temperature_prevIteration;
			Real64 temperature_prevTimeStep;
			Real64 temperature_finalConvergence;
			Real64 beta;
			Real64 volume;
			Real64 conductionArea = 1.0; // Assumes 1 m2 
		
			};

		Array1D< instanceOfCellData > cellArray;

		struct instanceOfWeatherData
			{
				Real64 dryBulbTemp;
				Real64 relativeHumidity;
				Real64 windSpeed;
				Real64 horizontalRadiation;
				Real64 airDensity;
			};

		Array1D< instanceOfWeatherData > weatherDataArray;

		static std::shared_ptr< FiniteDiffGroundTempsModel > 
		FiniteDiffGTMFactory(
			int objectType,
			std::string objectName
		);

		void
		getWeatherData();

		void
		initAndSim();

		void
		developMesh();

		void
		performSimulation();

		void
		updateSurfaceCellTemperature();

		void
		updateGeneralDomainCellTemperature(
			int const cell
		);

		void
		updateBottomCellTemperature();

		void
		initDomain();

		bool
		checkFinalTemperatureConvergence();

		bool
		checkIterationTemperatureConvergence();

		void
		updateIterationTemperatures();

		void
		updateTimeStepTemperatures();

		void
		doStartOfTimeStepInits();

		Real64
		getGroundTemp();

		Real64
		getGroundTempAtTimeInSeconds(
			Real64 const depth,
			Real64 const timeInSecondsOfSim
		);

		Real64
		getGroundTempAtTimeInMonths(
			Real64 const depth,
			int const monthOfSim
		);

		void
		evaluateSoilRhoCp(
			Optional< int const > cell = _,
			Optional_bool_const InitOnly = _
		);

		Real64
		interpolate(
			Real64 const x,
			Real64 const x_hi,
			Real64 const x_low,
			Real64 const y_hi,
			Real64 const y_low
		);

		Array2D< Real64 > groundTemps;

		Array1D< Real64 > cellDepths;

		enum surfaceTypes {
			surfaceCoverType_bareSoil = 1,
			surfaceCoverType_shortGrass = 2,
			surfaceCoverType_longGrass = 3
		};

		// Destructor
		~FiniteDiffGroundTempsModel(){}

	};

}

#endif
