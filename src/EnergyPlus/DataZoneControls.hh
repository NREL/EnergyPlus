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

#ifndef DataZoneControls_hh_INCLUDED
#define DataZoneControls_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataZoneControls {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumTempControlledZones;
	extern int NumHumidityControlZones;
	extern int NumComfortControlledZones;
	extern int NumTStatStatements;
	extern int NumComfortTStatStatements;
	extern int NumOpTempControlledZones; // number of zones with operative temp control
	extern int NumTempAndHumidityControlledZones; // number of zones with over cool control
	extern bool AnyOpTempControl; // flag set true if any zones have op temp control
	extern bool AnyZoneTempAndHumidityControl; // flag set true if any zones have over cool control
	extern Array1D_bool StageZoneLogic; // Logical array, A zone with staged thermostat = .TRUE.
	extern Array1D< Real64 > OccRoomTSetPointHeat; // occupied heating set point for optimum start period
	extern Array1D< Real64 > OccRoomTSetPointCool; // occupied cooling set point for optimum start period
	extern bool GetZoneAirStatsInputFlag; // True when need to get input

	// Types

	// Clears the global data in DataZoneControls.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	struct ZoneTempControls
	{
		// Members
		std::string Name; // Name of the thermostat
		std::string ZoneName; // Name of the zone
		int ActualZoneNum;
		std::string ControlTypeSchedName; // Name of the schedule which determines the zone temp setpoint
		int CTSchedIndex; // Index for this schedule
		int NumControlTypes;
		Array1D_string ControlType;
		Array1D_string ControlTypeName;
		Array1D_int ControlTypeSchIndx;
		int SchIndx_SingleHeatSetPoint;
		int SchIndx_SingleCoolSetPoint;
		int SchIndx_SingleHeatCoolSetPoint;
		int SchIndx_DualSetPointWDeadBand;
		bool ManageDemand; // Flag to indicate whether to use demand limiting
		Real64 HeatingResetLimit; // Lowest heating setpoint that can be set by demand manager [C]
		Real64 CoolingResetLimit; // Highest cooling setpoint that can be set by demand manager [C]
		bool EMSOverrideHeatingSetPointOn; // EMS is calling to override heating setpoint
		Real64 EMSOverrideHeatingSetPointValue; // value EMS is directing to use for heating setpoint [C]
		bool EMSOverrideCoolingSetPointOn; // EMS is calling to override cooling setpoint
		Real64 EMSOverrideCoolingSetPointValue; // value EMS is directing to use for cooling setpoint [C]
		bool OperativeTempControl; // flag to indicate whether control based on Operative Temp
		bool OpTempCntrlModeScheduled; // flag to indicate if radiative fraction is scheduled,
		// else constant
		Real64 FixedRadiativeFraction; // weighting factor for mean radiant temp for Operative temperature
		int OpTempRadiativeFractionSched; // index of schedule for when fraction is scheduled
		Real64 ZoneOvercoolRange; // Zone overcool temperature range (max), deg C
		bool ZoneOvercoolControl; // Flag to indicate whether control is based on overcool
		bool OvercoolCntrlModeScheduled; // Flag to indicate if zone overcool range is scheduled
		//   or constant
		Real64 ZoneOvercoolConstRange; // Overcool Range for Zone Air Setpoint Temperature [deltaC]
		int ZoneOvercoolRangeSchedIndex; // Index for Overcool Range Schedule
		Real64 ZoneOvercoolControlRatio; // Zone relative humidity shift per dry-bulb temperature overcooling
		//      below the original cooling setpoint, %RH/deltaC
		std::string DehumidifyingSched; // Name of the schedule to determine the zone dehumidifying setpoint
		int DehumidifyingSchedIndex; // Index for dehumidifying schedule

		// Default Constructor
		ZoneTempControls() :
			ActualZoneNum( 0 ),
			CTSchedIndex( 0 ),
			NumControlTypes( 0 ),
			SchIndx_SingleHeatSetPoint( 0 ),
			SchIndx_SingleCoolSetPoint( 0 ),
			SchIndx_SingleHeatCoolSetPoint( 0 ),
			SchIndx_DualSetPointWDeadBand( 0 ),
			ManageDemand( false ),
			HeatingResetLimit( 0.0 ),
			CoolingResetLimit( 0.0 ),
			EMSOverrideHeatingSetPointOn( false ),
			EMSOverrideHeatingSetPointValue( 0.0 ),
			EMSOverrideCoolingSetPointOn( false ),
			EMSOverrideCoolingSetPointValue( 0.0 ),
			OperativeTempControl( false ),
			OpTempCntrlModeScheduled( false ),
			FixedRadiativeFraction( 0.0 ),
			OpTempRadiativeFractionSched( 0 ),
			ZoneOvercoolRange( 0.0 ),
			ZoneOvercoolControl( false ),
			OvercoolCntrlModeScheduled( false ),
			ZoneOvercoolConstRange( 0.0 ),
			ZoneOvercoolRangeSchedIndex( 0 ),
			ZoneOvercoolControlRatio( 0.0 ),
			DehumidifyingSchedIndex( 0 )
		{}

	};

	struct ZoneHumidityControls
	{
		// Members
		std::string ControlName; // Name of this humidity controller
		std::string ZoneName; // Name of the zone
		std::string HumidifyingSched; // Name of the schedule to determine the zone humidifying setpoint
		std::string DehumidifyingSched; // Name of the schedule to determine the zone dehumidifying setpoint
		int ActualZoneNum;
		int HumidifyingSchedIndex; // Index for humidifying schedule
		int DehumidifyingSchedIndex; // Index for dehumidifying schedule
		int ErrorIndex; // Error index when LowRH setpoint > HighRH setpoint
		bool EMSOverrideHumidifySetPointOn; // EMS is calling to override humidifying setpoint
		Real64 EMSOverrideHumidifySetPointValue; // value EMS is directing to use for humidifying setpoint
		bool EMSOverrideDehumidifySetPointOn; // EMS is calling to override dehumidifying setpoint
		Real64 EMSOverrideDehumidifySetPointValue; // value EMS is directing to use for dehumidifying setpoint

		// Default Constructor
		ZoneHumidityControls() :
			ActualZoneNum( 0 ),
			HumidifyingSchedIndex( 0 ),
			DehumidifyingSchedIndex( 0 ),
			ErrorIndex( 0 ),
			EMSOverrideHumidifySetPointOn( false ),
			EMSOverrideHumidifySetPointValue( 0.0 ),
			EMSOverrideDehumidifySetPointOn( false ),
			EMSOverrideDehumidifySetPointValue( 0.0 )
		{}

	};

	struct ZoneComfortControls
	{
		// Members
		std::string Name; // Name of the thermostat
		std::string ZoneName; // Name of the zone
		int ActualZoneNum; // Index number of zone
		std::string ControlTypeSchedName; // Name of the schedule which determines the zone temp setpoint
		int ComfortSchedIndex; // Index for this schedule
		int NumControlTypes; // Number of control types in ZoneControl:ThermalComfort object
		Array1D_string ControlType; // Type of control
		Array1D_string ControlTypeName; // Name of control type
		Array1D_int ControlTypeSchIndx; // Index to control type schedule
		int SchIndx_SglHeatSetPointFanger; // Index to fanger single heating setpoint schedule
		int SchIndx_SglCoolSetPointFanger; // Index to fanger single cooling setpoint schedule
		int SchIndx_SglHCSetPointFanger; // Index to fanger single heating/cooling setpoint schedule
		int SchIndx_DualSetPointFanger; // Index to fanger dual setpoint schedule
		int SchIndx_SglHeatSetPointPierce; // Index to pierce single heating setpoint schedule
		int SchIndx_SglCoolSetPointPierce; // Index to pierce single cooling setpoint schedule
		int SchIndx_SglHCSetPointPierce; // Index to pierce single heating/cooling setpoint schedule
		int SchIndx_DualSetPointPierce; // Index to pierce dual setpoint schedule
		int SchIndx_SglHeatSetPointKSU; // Index to KSU single heating setpoint schedule
		int SchIndx_SglCoolSetPointKSU; // Index to KSU single cooling setpoint schedule
		int SchIndx_SglHCSetPointKSU; // Index to KSU single heating/cooling setpoint schedule
		int SchIndx_DualSetPointKSU; // Index to KSU dual setpoint schedule
		bool ManageDemand; // Flag to indicate whether to use demand limiting
		Real64 HeatingResetLimit; // Lowest heating setpoint that can be set by demand manager [C]
		Real64 CoolingResetLimit; // Highest cooling setpoint that can be set by demand manager [C]
		bool EMSOverrideHeatingSetPointOn; // EMS is calling to override heating setpoint
		Real64 EMSOverrideHeatingSetPointValue; // value EMS is directing to use for heating setpoint
		bool EMSOverrideCoolingSetPointOn; // EMS is calling to override cooling setpoint
		Real64 EMSOverrideCoolingSetPointValue; // value EMS is directing to use for cooling setpoint
		Real64 TdbMaxSetPoint; // Maximum dry-bulb temperature setpoint [C]
		Real64 TdbMinSetPoint; // Minimum dry-bulb temperature setpoint [C]
		std::string AverageMethodName; // Averaging Method for Zones with Multiple People Objects
		std::string AverageObjectName; // Object Name for Specific Object Average
		int AverageMethodNum; // Numerical value for averaging method
		int SpecificObjectNum; // People Object number used for Specific people object choice
		int PeopleAverageErrIndex; // People average error index
		int TdbMaxErrIndex; // Single cooling setpoint error index
		int TdbMinErrIndex; // Single heating setpoint error index
		int TdbHCErrIndex; // Single heating cooling setpoint error index
		int TdbDualMaxErrIndex; // Dual cooling setpoint error index
		int TdbDualMinErrIndex; // Dual heating setpoint error index

		// Default Constructor
		ZoneComfortControls() :
			ActualZoneNum( 0 ),
			ComfortSchedIndex( 0 ),
			NumControlTypes( 0 ),
			SchIndx_SglHeatSetPointFanger( 0 ),
			SchIndx_SglCoolSetPointFanger( 0 ),
			SchIndx_SglHCSetPointFanger( 0 ),
			SchIndx_DualSetPointFanger( 0 ),
			SchIndx_SglHeatSetPointPierce( 0 ),
			SchIndx_SglCoolSetPointPierce( 0 ),
			SchIndx_SglHCSetPointPierce( 0 ),
			SchIndx_DualSetPointPierce( 0 ),
			SchIndx_SglHeatSetPointKSU( 0 ),
			SchIndx_SglCoolSetPointKSU( 0 ),
			SchIndx_SglHCSetPointKSU( 0 ),
			SchIndx_DualSetPointKSU( 0 ),
			ManageDemand( false ),
			HeatingResetLimit( 0.0 ),
			CoolingResetLimit( 0.0 ),
			EMSOverrideHeatingSetPointOn( false ),
			EMSOverrideHeatingSetPointValue( 0.0 ),
			EMSOverrideCoolingSetPointOn( false ),
			EMSOverrideCoolingSetPointValue( 0.0 ),
			TdbMaxSetPoint( 50.0 ),
			TdbMinSetPoint( 0.0 ),
			AverageMethodName( "PEOPLE AVERGAE" ),
			AverageMethodNum( 0 ),
			SpecificObjectNum( 0 ),
			PeopleAverageErrIndex( 0 ),
			TdbMaxErrIndex( 0 ),
			TdbMinErrIndex( 0 ),
			TdbHCErrIndex( 0 ),
			TdbDualMaxErrIndex( 0 ),
			TdbDualMinErrIndex( 0 )
		{}

	};

	struct ZoneStagedControls
	{
		// Members
		std::string Name; // Name of the thermostat
		std::string ZoneName; // Name of the zone
		int ActualZoneNum; // Index number of zone
		std::string HeatSetBaseSchedName; // Name of the schedule which provides zone heating setpoint base
		int HSBchedIndex; // Index for this schedule
		std::string CoolSetBaseSchedName; // Name of the schedule which provides zone cooling setpoint base
		int CSBchedIndex; // Index for this schedule
		int NumOfHeatStages; // Number of heating stages
		int NumOfCoolStages; // Number of cooling stages
		Real64 HeatThroRange; // Heating throttling tempeature range
		Real64 CoolThroRange; // Cooling throttling tempeature range
		Array1D< Real64 > HeatTOffset; // Heating temperature offset
		Array1D< Real64 > CoolTOffset; // Cooling temperature offset
		Real64 HeatSetPoint; // Heating throttling tempeature range
		Real64 CoolSetPoint; // Cooling throttling tempeature range
		int StageErrCount; // Staged setpoint erro count
		int StageErrIndex; // Staged setpoint erro index

		// Default Constructor
		ZoneStagedControls() :
			ActualZoneNum( 0 ),
			HSBchedIndex( 0 ),
			CSBchedIndex( 0 ),
			NumOfHeatStages( 0 ),
			NumOfCoolStages( 0 ),
			HeatThroRange( 0.0 ),
			CoolThroRange( 0.0 ),
			HeatSetPoint( 0.0 ),
			CoolSetPoint( 0.0 ),
			StageErrCount( 0 ),
			StageErrIndex( 0 )
		{}

	};

	struct TStatObject
	{
		// Members
		std::string Name;
		int ZoneOrZoneListPtr;
		int NumOfZones;
		int TempControlledZoneStartPtr;
		int ComfortControlledZoneStartPtr;
		int StageControlledZoneStartPtr;
		bool ZoneListActive;

		// Default Constructor
		TStatObject() :
			ZoneOrZoneListPtr( 0 ),
			NumOfZones( 0 ),
			TempControlledZoneStartPtr( 0 ),
			ComfortControlledZoneStartPtr( 0 ),
			StageControlledZoneStartPtr( 0 ),
			ZoneListActive( false )
		{}

	};

	// Object Data
	extern Array1D< ZoneHumidityControls > HumidityControlZone;
	extern Array1D< ZoneTempControls > TempControlledZone;
	extern Array1D< ZoneComfortControls > ComfortControlledZone;
	extern Array1D< TStatObject > TStatObjects;
	extern Array1D< TStatObject > ComfortTStatObjects;
	extern Array1D< TStatObject > StagedTStatObjects;
	extern Array1D< ZoneStagedControls > StageControlledZone;

} // DataZoneControls

} // EnergyPlus

#endif
