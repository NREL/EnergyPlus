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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <HybridModel.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRoomAirModel.hh>
#include <General.hh>
#include <HeatBalanceManager.hh>
#include <InputProcessor.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HybridModel {

	// MODULE INFORMATION:
	//       AUTHOR         Sang Hoon Lee, Tianzhen Hong, Rongpeng Zhang. LBNL 
	//       DATE WRITTEN   Oct 2015

	// PURPOSE OF THIS MODULE:
	// This module manages hybrid model.

	// METHODOLOGY EMPLOYED:
	//  The model uses measured zone air temperature to calculate internal thermal mass or infiltration air flow rate.

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataGlobals;
	using namespace DataHeatBalance;
	using namespace DataPrecisionGlobals;
	using namespace DataRoomAirModel;
	using namespace InputProcessor;
	using DataGlobals::ScheduleAlwaysOn;
	using General::CheckCreatedZoneItemName;
	
	bool FlagHybridModel( false ); // True if hybrid model is activated
	int NumOfHybridModelZones( 0 ); // Number of hybrid model zones in the model
	std::string CurrentModuleObject; // to assist in getting input

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	// Object Data
	Array1D< HybridModelProperties > HybridModelZone;

	// Functions

	void
	GetHybridModelZone()
	{

		using ScheduleManager::GetScheduleIndex;

		bool ErrorsFound( false ); // If errors detected in input
		Array1D_bool lAlphaFieldBlanks( 10, false );
		Array1D_bool lNumericFieldBlanks( 10, false );
		int NumAlphas; // Number of Alphas for each GetobjectItem call
		int NumNumbers; // Number of Numbers for each GetobjectItem call
		int IOStatus;
		int ZonePtr; // Pointer to the zone
		int ZoneListPtr; // Pointer to the zone list
		std::string CurrentModuleObject; // to assist in getting input
		Array1D_string cAlphaArgs( 10 ); // Alpha input items for object
		Array1D_string cAlphaFieldNames( 10 );
		Array1D_string cNumericFieldNames( 10 );
		Array1D< Real64 > rNumericArgs( 10 ); // Numeric input items for object
		int HybridModelStartMonth( 0 ); // Hybrid model start month 
		int HybridModelStartDate( 0 ); // Hybrid model start date of month 
		int HybridModelEndMonth( 0 ); // Hybrid model end month 
		int HybridModelEndDate( 0 ); // Hybrid model end date of month 
		int HMStartDay( 0 );
		int HMEndDay( 0 );

		// Read hybrid model input
		CurrentModuleObject = "HybridModel:Zone";
		NumOfHybridModelZones = GetNumObjectsFound( CurrentModuleObject );
		HybridModelZone.allocate( NumOfZones );

		if ( NumOfHybridModelZones > 0 ) {

			for ( int HybridModelNum = 1; HybridModelNum <= NumOfHybridModelZones; ++HybridModelNum ) {

				GetObjectItem( CurrentModuleObject, HybridModelNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				
				ZoneListPtr = 0;
				ZonePtr = FindItemInList( cAlphaArgs( 2 ), Zone );
				if ( ZonePtr == 0 && NumOfZoneLists > 0 ) ZoneListPtr = FindItemInList( cAlphaArgs( 2 ), ZoneList );
				if ( ZonePtr > 0 ) {
					HybridModelZone( ZonePtr ).Name = cAlphaArgs( 1 );
					HybridModelZone( ZonePtr ).InternalThermalMassCalc = SameString( cAlphaArgs( 3 ), "Yes" );
					HybridModelZone( ZonePtr ).InfiltrationCalc = SameString( cAlphaArgs( 4 ), "Yes" );

					// Zone Air Infiltration Rate and Zone Internal Thermal Mass calculations cannot be performed simultaneously
					if ( HybridModelZone( ZonePtr ).InternalThermalMassCalc && HybridModelZone( ZonePtr ).InfiltrationCalc ){
						HybridModelZone( ZonePtr ).InfiltrationCalc = false;
						ShowWarningError( CurrentModuleObject + "=\"" + HybridModelZone( ZonePtr ).Name + "\" invalid " + cAlphaFieldNames( 3 ) + " and " + cAlphaFieldNames( 4 ) + "." );
						ShowContinueError( "Field " + cAlphaFieldNames( 3 ) + " and " + cAlphaFieldNames( 4 ) + "\" cannot be both set to YES." );
						ShowContinueError( "Field " + cAlphaFieldNames( 4 ) + "\" is changed to NO for the hybrid modeling simulations." );
					}

					// Flags showing Hybrid Modeling settings
					if ( HybridModelZone( ZonePtr ).InternalThermalMassCalc || HybridModelZone( ZonePtr ).InfiltrationCalc ){
						FlagHybridModel = true;
					}

					if ( FlagHybridModel ){
						HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureSchedulePtr = GetScheduleIndex( cAlphaArgs( 5 ) );
						HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureStartMonth = rNumericArgs( 1 );
						HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureStartDate = rNumericArgs( 2 );
						HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureEndMonth = rNumericArgs( 3 );
						HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureEndDate = rNumericArgs( 4 );

						// prepare start and end date for Hybrid Modeling
						{
							int HMDayArr[ 12 ] = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };

							HybridModelStartMonth = HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureStartMonth;
							HybridModelStartDate = HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureStartDate;
							HybridModelEndMonth = HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureEndMonth;
							HybridModelEndDate = HybridModelZone( ZonePtr ).ZoneMeasuredTemperatureEndDate;

							if ( HybridModelStartMonth >= 1 && HybridModelStartMonth <= 12 ){
								HMStartDay = HMDayArr[ HybridModelStartMonth - 1 ];
							}
							else {
								HMStartDay = 0;
							}

							if ( HybridModelEndMonth >= 1 && HybridModelEndMonth <= 12 ){
								HMEndDay = HMDayArr[ HybridModelEndMonth - 1 ];
							}
							else {
								HMEndDay = 0;
							}

							HybridModelZone( ZonePtr ).HybridStartDayOfYear = HMStartDay + HybridModelStartDate;
							HybridModelZone( ZonePtr ).HybridEndDayOfYear = HMEndDay + HybridModelEndDate;
						}
					}
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found." );
					ErrorsFound = true;
				}
			}

			//ZoneAirMassFlowConservation should not be activated during the Hybrid Modeling infiltration calculations
			if ( HybridModelZone( ZonePtr ).InfiltrationCalc && ZoneAirMassFlow.EnforceZoneMassBalance ){
				ZoneAirMassFlow.EnforceZoneMassBalance = false;
				ShowWarningError( "ZoneAirMassFlowConservation is deactivated when Hybrid Modeling is performed." );
			}

			//RoomAirModelType should be Mixing if Hybrid Modeling is performed for the zone
			if( FlagHybridModel ){
				for ( ZonePtr = 1; ZonePtr <= NumOfZones; ZonePtr++ ) {
					if( ( HybridModelZone( ZonePtr ).InternalThermalMassCalc || HybridModelZone( ZonePtr ).InfiltrationCalc ) && ( AirModel( ZonePtr ).AirModelType != RoomAirModel_Mixing ) ){
						AirModel( ZonePtr ).AirModelType = RoomAirModel_Mixing;
						ShowWarningError( "Room Air Model Type should be Mixing if Hybrid Modeling is performed for the zone." );
					}
				}
			}

			if ( ErrorsFound ) {
				ShowFatalError( "Errors getting Hybrid Model input data. Preceding condition(s) cause termination." );
			}
		}
	}

	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{

		FlagHybridModel = false;
		NumOfHybridModelZones = 0;

	}

}

} // EnergyPlus
