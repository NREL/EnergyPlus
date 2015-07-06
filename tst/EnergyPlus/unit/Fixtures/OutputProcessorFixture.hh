#ifndef OutputProcessorFixture_hh_INCLUDED
#define OutputProcessorFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "SQLiteFixture.hh"
#include <EnergyPlus/OutputProcessor.hh>

#include <ObjexxFCL/gio.hh>

using namespace EnergyPlus::OutputProcessor;

namespace EnergyPlus {

	class OutputProcessorFixture : public SQLiteFixture
	{

	protected:
		static void SetUpTestCase() {
			// Needed to initialize IDD cache
			EnergyPlusFixture::SetUpTestCase();
		}
		static void TearDownTestCase() { }
		
		virtual void SetUp() {
			SQLiteFixture::SetUp();  // Sets up the base fixture first.

			// might want to call InitializeOutput()...
			ReportList.allocate( 500 );

			FreqNotice = Array2D_string( {1,2}, {-1,4} );
		}

		virtual void TearDown() {
			InstMeterCacheSize = 1000;
			InstMeterCacheSizeInc = 1000;
			InstMeterCache.deallocate();
			InstMeterCacheLastUsed = 0;
			CurrentReportNumber = 0;
			NumVariablesForOutput = 0;
			MaxVariablesForOutput = 0;
			NumOfRVariable_Setup = 0;
			NumTotalRVariable = 0;
			NumOfRVariable_Sum = 0;
			NumOfRVariable_Meter = 0;
			NumOfRVariable = 0;
			MaxRVariable = 0;
			NumOfIVariable_Setup = 0;
			NumTotalIVariable = 0;
			NumOfIVariable_Sum = 0;
			NumOfIVariable = 0;
			MaxIVariable = 0;
			OutputInitialized = false;
			ProduceReportVDD = ReportVDD_No;
			OutputFileMeterDetails = 0;
			NumHoursInDay = 24;
			NumHoursInMonth = 0;
			NumHoursInSim = 0;
			ReportList.deallocate();
			NumReportList = 0;
			NumExtraVars = 0;
			FreqNotice.deallocate();
			NumOfReqVariables = 0;
			NumVarMeterArrays = 0;
			NumEnergyMeters = 0;
			MeterValue.deallocate();
			TimeStepStampReportNbr = 0;
			TimeStepStampReportChr = "";
			TrackingHourlyVariables = false;
			DailyStampReportNbr = 0;
			DailyStampReportChr = "";
			TrackingDailyVariables = false;
			MonthlyStampReportNbr = 0;
			MonthlyStampReportChr = "";
			TrackingMonthlyVariables = false;
			RunPeriodStampReportNbr = 0;
			RunPeriodStampReportChr = "";
			TrackingRunPeriodVariables = false;
			TimeStepZoneSec = 0;
			ErrorsLogged = false;
			ProduceVariableDictionary = false;
			MaxNumSubcategories = 1;
			ReportNumberCounter = 0;
			LHourP = -1;
			LStartMin = -1.0;
			LEndMin = -1.0;
			EndTimeStepFlag = false;
			TimeValue.deallocate();
			RVariableTypes.deallocate();
			IVariableTypes.deallocate();
			DDVariableTypes.deallocate();
			RVariable.deallocate();
			IVariable.deallocate();
			RVar.deallocate();
			IVar.deallocate();
			ReqRepVars.deallocate();
			VarMeterArrays.deallocate();
			EnergyMeters.deallocate();
			EndUseCategory.deallocate();

			{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileMeterDetails, flags ); }

			SQLiteFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
		}

	};

	typedef OutputProcessorFixture OutputProcessorDeathTestFixture;

}

#endif
