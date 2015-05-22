#ifndef GroundTempManager_hh_INCLUDED
#define GroundTempManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace GroundTempManager {

struct Cell
{
	Real64 Density;
	Real64 SpecificHeat;
	Real64 Conductivity;
	Real64 Thickness;
	Real64 Depth;
	Real64 Temp;

	void EvaluateCellTemp();

	void UpdateCellProps();

	void CalcCellResistance();
};

struct Canopy
{
	int CanopyType;
	Real64 VegitationDensity;

	void CalcCanopyHT();
};

struct WeatherData
{
	struct SingleWeatherDataPoint
	{
		Real64 DryBulbTemp;
		Real64 WetBulbTemp;
		Real64 WindSpeed;
	};

	Array1D< Real64 > SingleWeatherDataPoint;

	void GetWeatherData();
	
};

struct GroundSurface
{
	Real64 Qnet;
	Real64 SurfaceArea;
	bool SnowCover;

	void EvaluateGroundSurfHT();
};

class GroundTemperatureModel
{
	virtual Real64
	getCurrentGroundTemp( 
		Real64,
		Real64,
		Real64
	)=0;
};

class SimpleGroundTemps:GroundTemperatureModel
{
	public:
	int i;

	Real64
	getCurrentGroundTemp(
		Real64 const day,
		Real64 const hour,
		Real64 const depth
	);
};

class DetailedGroundTemps:GroundTemperatureModel
{
	Real64
	getCurrentGroundTemp(
		Real64 const day,
		Real64 const hour,
		Real64 const depth
	);
};

	//******************************************************************************

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

}

}

#endif