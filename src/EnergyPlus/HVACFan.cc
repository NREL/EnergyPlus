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

// EnergyPlus Headers
#include <HVACFan.hh>
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <InputProcessor.hh>
#include <DataIPShortCuts.hh>
#include <ScheduleManager.hh>
#include <NodeInputManager.hh>
#include <DataLoopNode.hh>
#include <DataSizing.hh>
#include <CurveManager.hh>
#include <DataHeatBalance.hh>
#include <OutputProcessor.hh>
#include <General.hh>
#include <EMSManager.hh>
#include <ObjexxFCL/Optional.hh>
#include <DataAirLoop.hh>
#include <DataEnvironment.hh>
#include <ReportSizingManager.hh>
#include <OutputReportPredefined.hh>
#include <ReportSizingManager.hh>
#include <Psychrometrics.hh>
#include <DataContaminantBalance.hh>
#include <DataPrecisionGlobals.hh>
#include <BranchNodeConnections.hh>

namespace EnergyPlus {

namespace HVACFan {

	std::vector < std::unique_ptr <FanSystem> > fanObjs;

	int
	getFanObjectVectorIndex(  // lookup vector index for fan object name in object array EnergyPlus::HVACFan::fanObjs
		std::string const objectName  // IDF name in input
	)
	{
		int index = -1;
		bool found = false;
		for ( std::size_t loop = 0; loop < fanObjs.size(); ++loop ) {
			if ( objectName == fanObjs[ loop ]->name() ) {
				if ( ! found ) {
					index = loop;
					found = true;
				} else { // found duplicate
					//TODO throw warning?  
					index = -1;
				}
			}
		}
		return index;
	}

	bool
	checkIfFanNameIsAFanSystem( // look up to see if input contains a Fan:SystemModel with the name (for use before object construction
		std::string const objectName
	) {
	
		int testNum = InputProcessor::GetObjectItemNum("Fan:SystemModel", objectName );
		if ( testNum > 0 ) {
			return true;
		} else {
			return false;
		}
	
	}

	void
	FanSystem::simulate(
//		bool const firstHVACIteration,
		Optional< Real64 const > flowFraction,
		Optional_bool_const zoneCompTurnFansOn, // Turn fans ON signal from ZoneHVAC component
		Optional_bool_const zoneCompTurnFansOff, // Turn Fans OFF signal from ZoneHVAC component
		Optional< Real64 const > pressureRise // Pressure difference to use for DeltaPress, for rating DX coils without entire duct system
	)
	{

		objTurnFansOn_ = false;
		objTurnFansOff_ = false;

		init( );

		if ( present( zoneCompTurnFansOn ) && present( zoneCompTurnFansOff ) ) {
			// Set module-level logic flags equal to ZoneCompTurnFansOn and ZoneCompTurnFansOff values passed into this routine
			// for ZoneHVAC components with system availability managers defined.
			// The module-level flags get used in the other subroutines (e.g., SimSimpleFan,SimVariableVolumeFan and SimOnOffFan)
			objTurnFansOn_ = zoneCompTurnFansOn;
			objTurnFansOff_ = zoneCompTurnFansOff;
		} else {
			// Set module-level logic flags equal to the global LocalTurnFansOn and LocalTurnFansOff variables for all other cases.
			objTurnFansOn_ = DataHVACGlobals::TurnFansOn;
			objTurnFansOff_ = DataHVACGlobals::TurnFansOff;
		}
		if ( present( pressureRise ) &&  present( flowFraction ) ) {
			calcSimpleSystemFan( flowFraction, pressureRise );
		} else if ( present( pressureRise ) && ! present( flowFraction ) ){
			calcSimpleSystemFan( _, pressureRise );
		} else if ( ! present( pressureRise ) &&  present( flowFraction ) ) {
			calcSimpleSystemFan( flowFraction, _ );
		} else {
			calcSimpleSystemFan( _ , _ );
		}

		update();

		report();

	}

	void
	FanSystem::init()
	{ 
		if ( ! DataGlobals::SysSizingCalc && objSizingFlag_ ) {
			set_size();
			objSizingFlag_ = false;
			if ( DataSizing::CurSysNum > 0 ) {
				DataAirLoop::AirLoopControlInfo( DataSizing::CurSysNum ).CyclingFan = true;
			}
		}

		if ( DataGlobals::BeginEnvrnFlag && objEnvrnFlag_ ) {

			minAirFlowRate_ = designAirVolFlowRate_ * minPowerFlowFrac_;
			minAirMassFlowRate_ = minAirFlowRate_ * rhoAirStdInit_;

//			if ( Fan( FanNum ).NVPerfNum > 0 ) {
//				NightVentPerf( Fan( FanNum ).NVPerfNum ).MaxAirMassFlowRate = NightVentPerf( Fan( FanNum ).NVPerfNum ).MaxAirFlowRate * Fan( FanNum ).RhoAirStdInit;
//			}

			//Init the Node Control variables
			DataLoopNode::Node( outletNodeNum_ ).MassFlowRateMax = maxAirMassFlowRate_;
			DataLoopNode::Node( outletNodeNum_ ).MassFlowRateMin =minAirMassFlowRate_;

			//Initialize all report variables to a known state at beginning of simulation
			fanPower_ = 0.0;
			deltaTemp_ = 0.0;
			fanEnergy_ = 0.0;
			for ( auto loop = 0; loop < numSpeeds_; ++loop ) {
				fanRunTimeFractionAtSpeed_[ loop ] = 0.0;
			}
			objEnvrnFlag_ =  false;
		}

		if ( ! DataGlobals::BeginEnvrnFlag ) {
			objEnvrnFlag_ = true;
		}

		massFlowRateMaxAvail_ = min( DataLoopNode::Node( outletNodeNum_ ).MassFlowRateMax, DataLoopNode::Node( inletNodeNum_ ).MassFlowRateMaxAvail );
		massFlowRateMinAvail_ = min( max( DataLoopNode::Node( outletNodeNum_ ).MassFlowRateMin, DataLoopNode::Node( inletNodeNum_ ).MassFlowRateMinAvail ), DataLoopNode::Node( inletNodeNum_ ).MassFlowRateMaxAvail );

		// Load the node data in this section for the component simulation
		//First need to make sure that the MassFlowRate is between the max and min avail.
		inletAirMassFlowRate_ = min( DataLoopNode::Node( inletNodeNum_ ).MassFlowRate, massFlowRateMaxAvail_ );
		inletAirMassFlowRate_ = max( inletAirMassFlowRate_, massFlowRateMinAvail_ );

		//Then set the other conditions
		inletAirTemp_     = DataLoopNode::Node( inletNodeNum_ ).Temp;
		inletAirHumRat_   = DataLoopNode::Node( inletNodeNum_ ).HumRat;
		inletAirEnthalpy_ = DataLoopNode::Node( inletNodeNum_ ).Enthalpy;
	}

	void
	FanSystem::set_size()
	{
		std::string const routineName = "HVACFan::set_size ";
	
		Real64 tempFlow = designAirVolFlowRate_;
		bool bPRINT = true;
		DataSizing::DataAutosizable = true;
		DataSizing::DataEMSOverrideON = maxAirFlowRateEMSOverrideOn_;
		DataSizing::DataEMSOverride   = maxAirFlowRateEMSOverrideValue_;
		ReportSizingManager::RequestSizing(fanType_, name_, DataHVACGlobals::SystemAirflowSizing, "Design Maximum Air Flow Rate [m3/s]", tempFlow, bPRINT, routineName );
		designAirVolFlowRate_    = tempFlow;
		DataSizing::DataAutosizable   = true;
		DataSizing::DataEMSOverrideON = false;
		DataSizing::DataEMSOverride   = 0.0;


		if ( designElecPowerWasAutosized_ ) {

			switch ( powerSizingMethod_ )
			{
		
			case PowerSizingMethod::powerPerFlow: {
				designElecPower_ = designAirVolFlowRate_ * elecPowerPerFlowRate_;
				break;
			}
			case PowerSizingMethod::powerPerFlowPerPressure: {
				designElecPower_ = designAirVolFlowRate_ * deltaPress_ * elecPowerPerFlowRatePerPressure_;
				break;
			}
			case PowerSizingMethod::totalEfficiencyAndPressure: {
				designElecPower_ = designAirVolFlowRate_ * deltaPress_ / fanTotalEff_;
				break;
			}
		
			} // end switch

			//report design power
			ReportSizingManager::ReportSizingOutput( fanType_, name_, "Design Electric Power Consumption [W]", designElecPower_ );
		
		} // end if power was autosized

		rhoAirStdInit_ = DataEnvironment::StdRhoAir;
		maxAirMassFlowRate_ = designAirVolFlowRate_ * rhoAirStdInit_;

		//calculate total fan system efficiency at design
		fanTotalEff_ = designAirVolFlowRate_ * deltaPress_ / designElecPower_;

		if (numSpeeds_ > 1 ) { // set up values at speeds
			massFlowAtSpeed_.resize( numSpeeds_, 0.0 );
			totEfficAtSpeed_.resize( numSpeeds_, 0.0 );
			for ( auto loop=0; loop < numSpeeds_; ++loop ) {
				massFlowAtSpeed_[ loop ] = maxAirMassFlowRate_ * flowFractionAtSpeed_[ loop ];
				if ( powerFractionInputAtSpeed_[ loop ] ) { // use speed power fraction
					totEfficAtSpeed_[ loop ] = flowFractionAtSpeed_[ loop ] * designAirVolFlowRate_ * deltaPress_  / ( designElecPower_ * powerFractionAtSpeed_[ loop ] );
				} else { // use power curve
					totEfficAtSpeed_[ loop ] = flowFractionAtSpeed_[ loop ] * designAirVolFlowRate_ * deltaPress_ / ( designElecPower_ * CurveManager::CurveValue( powerModFuncFlowFractionCurveIndex_, flowFractionAtSpeed_[ loop ] ) );
					powerFractionAtSpeed_[ loop ] = CurveManager::CurveValue( powerModFuncFlowFractionCurveIndex_, flowFractionAtSpeed_[ loop ] );
				}
				
			}
		}

		OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchFanType, name_, fanType_ );
		OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchFanTotEff, name_, fanTotalEff_ );
		OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchFanDeltaP, name_, deltaPress_ );
		OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchFanVolFlow, name_, designAirVolFlowRate_ );

		OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchFanPwr, name_, designElecPower_ );
		if ( designAirVolFlowRate_ != 0.0 ) {
			OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchFanPwrPerFlow, name_, designElecPower_ / designAirVolFlowRate_ );
		}
		OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchFanMotorIn, name_, motorInAirFrac_ );
		OutputReportPredefined::PreDefTableEntry( OutputReportPredefined::pdchFanEndUse, name_, endUseSubcategoryName_ );

		objSizingFlag_ = false;
	}

	FanSystem::FanSystem( // constructor
		std::string const objectName
	):
		fanType_Num_( 0 ),
		availSchedIndex_( 0 ),
		inletNodeNum_( 0 ),
		outletNodeNum_( 0 ),
		designAirVolFlowRate_( 0.0 ),
		designAirVolFlowRateWasAutosized_( false),
		speedControl_( SpeedControlMethod::notSet ), 
		minPowerFlowFrac_( 0.0 ),
		deltaPress_( 0.0 ),
		motorEff_( 0.0 ),
		motorInAirFrac_( 0.0 ),
		designElecPower_( 0.0 ),
		designElecPowerWasAutosized_( false),
		powerSizingMethod_( PowerSizingMethod::powerSizingMethodNotSet),
		elecPowerPerFlowRate_( 0.0 ),
		elecPowerPerFlowRatePerPressure_( 0.0 ),
		fanTotalEff_( 0.0 ),
		powerModFuncFlowFractionCurveIndex_( 0 ),
		nightVentPressureDelta_( 0.0 ),
		nightVentFlowFraction_( 0.0 ),
		zoneNum_( 0 ),
		zoneRadFract_( 0.0 ),
		heatLossesDestination_( ThermalLossDestination::heatLossNotDetermined ),
		numSpeeds_( 0 ),
		inletAirMassFlowRate_( 0.0 ),
		outletAirMassFlowRate_( 0.0 ),
		minAirFlowRate_( 0.0 ),
		maxAirMassFlowRate_( 0.0 ),
		minAirMassFlowRate_( 0.0 ),
		inletAirTemp_( 0.0 ),
		outletAirTemp_( 0.0 ),
		inletAirHumRat_( 0.0 ),
		outletAirHumRat_( 0.0 ),
		inletAirEnthalpy_( 0.0 ),
		outletAirEnthalpy_( 0.0 ),
		objTurnFansOn_( false ),
		objTurnFansOff_( false ),
		objEnvrnFlag_( true ),
		objSizingFlag_( true ),
		fanPower_( 0.0 ),
		fanEnergy_( 0.0 ),
		maxAirFlowRateEMSOverrideOn_( false ),
		maxAirFlowRateEMSOverrideValue_( 0.0 ),
		eMSFanPressureOverrideOn_( false ),
		eMSFanPressureValue_( 0.0 ),
		eMSFanEffOverrideOn_( false ),
		eMSFanEffValue_( 0.0 ),
		eMSMaxMassFlowOverrideOn_( false ),
		eMSAirMassFlowValue_( 0.0 ),
		faultyFilterFlag_( false ),
		faultyFilterIndex_( 0 ),
		massFlowRateMaxAvail_( 0.0 ),
		massFlowRateMinAvail_( 0.0 ),
		rhoAirStdInit_( 0.0 ),
		oneTimePowerCurveCheck_( true ) 
	{

		std::string const routineName = "HVACFan constructor ";
		int numAlphas; // Number of elements in the alpha array
		int numNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		bool errorsFound = false;
		DataIPShortCuts::cCurrentModuleObject = "Fan:SystemModel";

		int objectNum = InputProcessor::GetObjectItemNum( DataIPShortCuts::cCurrentModuleObject, objectName );

		InputProcessor::GetObjectItem( DataIPShortCuts::cCurrentModuleObject, objectNum, DataIPShortCuts::cAlphaArgs, numAlphas, DataIPShortCuts::rNumericArgs, numNums, IOStat, DataIPShortCuts::lNumericFieldBlanks, DataIPShortCuts::lAlphaFieldBlanks, DataIPShortCuts::cAlphaFieldNames, DataIPShortCuts::cNumericFieldNames  );

		name_ = DataIPShortCuts::cAlphaArgs( 1 );
		//TODO how to check for unique names across objects during get input?
		fanType_ = DataIPShortCuts::cCurrentModuleObject;
		fanType_Num_ = DataHVACGlobals::FanType_SystemModelObject;
		if ( DataIPShortCuts::lAlphaFieldBlanks( 2 ) ) {
			availSchedIndex_ = DataGlobals::ScheduleAlwaysOn;
		} else {
			availSchedIndex_ = ScheduleManager::GetScheduleIndex( DataIPShortCuts::cAlphaArgs( 2 ) );
			if ( availSchedIndex_ == 0 ) {
				ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 2 ) + " = " + DataIPShortCuts::cAlphaArgs( 2 ) );
				errorsFound = true;
			}
		}
		inletNodeNum_ = NodeInputManager::GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 3 ), errorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs( 1 ), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent );
		outletNodeNum_ = NodeInputManager::GetOnlySingleNode( DataIPShortCuts::cAlphaArgs( 4 ), errorsFound, DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs( 1 ), DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent );

		BranchNodeConnections::TestCompSet( DataIPShortCuts::cCurrentModuleObject, DataIPShortCuts::cAlphaArgs( 1 ),  DataIPShortCuts::cAlphaArgs( 3 ),  DataIPShortCuts::cAlphaArgs( 4 ),"Air Nodes" );

		designAirVolFlowRate_ =  DataIPShortCuts::rNumericArgs( 1 );
		if ( designAirVolFlowRate_ == DataSizing::AutoSize ) {
			designAirVolFlowRateWasAutosized_ = true;
		}

		if ( DataIPShortCuts::lAlphaFieldBlanks( 5 ) ) {
			speedControl_ = SpeedControlMethod::discrete;
		} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 5 ), "Continuous") ) {
			speedControl_ = SpeedControlMethod::continuous;
		} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 5 ), "Discrete")  ) {
			speedControl_ = SpeedControlMethod::discrete;
		} else {
			ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
			ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 5 ) + " = " + DataIPShortCuts::cAlphaArgs( 5 ) );
			errorsFound = true;
		}

		minPowerFlowFrac_ = DataIPShortCuts::rNumericArgs( 2 );
		deltaPress_       = DataIPShortCuts::rNumericArgs( 3 );
		motorEff_         = DataIPShortCuts::rNumericArgs( 4 );
		motorInAirFrac_   = DataIPShortCuts::rNumericArgs( 5 );
		designElecPower_  = DataIPShortCuts::rNumericArgs( 6 );
		if ( designElecPower_ == DataSizing::AutoSize ) {
			designElecPowerWasAutosized_ = true;
		}
		if ( designElecPowerWasAutosized_ ) {
			if ( DataIPShortCuts::lAlphaFieldBlanks( 6 ) ) {
				powerSizingMethod_ = PowerSizingMethod::powerPerFlowPerPressure;
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "PowerPerFlow" ) ) {
				powerSizingMethod_ = PowerSizingMethod::powerPerFlow;
			} else if ( InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "PowerPerFlowPerPressure" ) ) {
				powerSizingMethod_ = PowerSizingMethod::powerPerFlowPerPressure;
			} else if (  InputProcessor::SameString( DataIPShortCuts::cAlphaArgs( 6 ), "TotalEfficiencyAndPressure" ) ) {
				powerSizingMethod_ = PowerSizingMethod::totalEfficiencyAndPressure;
			} else {
				ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 6 ) + " = " + DataIPShortCuts::cAlphaArgs( 6 ) );
				errorsFound = true;
			}
			elecPowerPerFlowRate_            = DataIPShortCuts::rNumericArgs( 7 );
			elecPowerPerFlowRatePerPressure_ = DataIPShortCuts::rNumericArgs( 8 );
			fanTotalEff_                     = DataIPShortCuts::rNumericArgs( 9 );
		}
		if ( ! DataIPShortCuts::lAlphaFieldBlanks( 7 ) ) {
			powerModFuncFlowFractionCurveIndex_ = CurveManager::GetCurveIndex( DataIPShortCuts::cAlphaArgs( 7 ) );
		}
		 nightVentPressureDelta_       = DataIPShortCuts::rNumericArgs( 10 );
		 nightVentFlowFraction_        = DataIPShortCuts::rNumericArgs( 11 );
		zoneNum_ = InputProcessor::FindItemInList( DataIPShortCuts::cAlphaArgs( 8 ), DataHeatBalance::Zone );
		if ( zoneNum_ > 0 ) heatLossesDestination_ = ThermalLossDestination::zoneGains;
		if ( zoneNum_ == 0 ) {
			if ( DataIPShortCuts::lAlphaFieldBlanks( 8 ) ) {
				heatLossesDestination_ = ThermalLossDestination::lostToOutside;
			} else {
				heatLossesDestination_ = ThermalLossDestination::lostToOutside;
				ShowWarningError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Invalid " + DataIPShortCuts::cAlphaFieldNames( 8 ) + " = " + DataIPShortCuts::cAlphaArgs( 8 ) );
				ShowContinueError( "Zone name not found. Fan motor heat losses will not be added to a zone" );
				// continue with simulation but motor losses not sent to a zone.
			}
		}
		zoneRadFract_ = DataIPShortCuts::rNumericArgs( 12 );
		if ( ! DataIPShortCuts::lAlphaFieldBlanks( 9 ) ) {
			endUseSubcategoryName_ = DataIPShortCuts::cAlphaArgs( 9 );
		} else {
			endUseSubcategoryName_ = "General";
		}
		
		if ( ! DataIPShortCuts::lNumericFieldBlanks( 13 ) ){
			numSpeeds_ =  DataIPShortCuts::rNumericArgs( 13 );
		} else {
			numSpeeds_ =  1;
		}
		fanRunTimeFractionAtSpeed_.resize( numSpeeds_, 0.0 );
		if ( speedControl_ == SpeedControlMethod::discrete && numSpeeds_ > 1 ) {
			//should have field sets 
			flowFractionAtSpeed_.resize( numSpeeds_, 0.0 );
			powerFractionAtSpeed_.resize( numSpeeds_, 0.0 );
			powerFractionInputAtSpeed_.resize( numSpeeds_, false );
			if ( numSpeeds_ == (( numNums - 13 ) / 2 ) || numSpeeds_ == (( numNums + 1 - 13 ) / 2 ) ) {
				for ( auto loopSet = 0 ; loopSet< numSpeeds_; ++loopSet ) {
					flowFractionAtSpeed_[ loopSet ]  = DataIPShortCuts::rNumericArgs( 13 + loopSet * 2 + 1 );
					if ( ! DataIPShortCuts::lNumericFieldBlanks( 13 + loopSet * 2 + 2  )  ) {
						powerFractionAtSpeed_[ loopSet ] = DataIPShortCuts::rNumericArgs( 13 + loopSet * 2 + 2 );
						powerFractionInputAtSpeed_[ loopSet ] = true;
					} else {
						powerFractionInputAtSpeed_[ loopSet ] = false;
					}
				}
			} else {
				// field set input does not match number of speeds, throw warning
				ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Fan with Discrete speed control does not have input for speed data that matches the number of speeds.");
				errorsFound = true;
			}
			// check that flow fractions are increasing
			bool increasingOrderError = false;
			for ( auto loop = 0; loop < (numSpeeds_ - 1); ++loop ) {
				if ( flowFractionAtSpeed_[ loop ] >  flowFractionAtSpeed_[ loop + 1 ]) {
					increasingOrderError = true;
				}
			}
			if ( increasingOrderError ) {
				ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Fan with Discrete speed control and multiple speed levels does not have input with flow fractions arranged in increasing order.");
				errorsFound = true;
			}
		}

		// check if power curve present when any speeds have no power fraction 
		if ( speedControl_ == SpeedControlMethod::discrete && numSpeeds_ > 1 && powerModFuncFlowFractionCurveIndex_ == 0 ) {
			bool foundMissingPowerFraction = false;
			for ( auto loop = 0 ; loop< numSpeeds_; ++loop ) {
				if ( ! powerFractionInputAtSpeed_[ loop ] ) {
					foundMissingPowerFraction = true;
				}
			}
			if ( foundMissingPowerFraction ) {
				// field set input does not match number of speeds, throw warning
				ShowSevereError( routineName + DataIPShortCuts::cCurrentModuleObject + "=\"" + DataIPShortCuts::cAlphaArgs( 1 ) + "\", invalid entry." );
				ShowContinueError( "Fan with Discrete speed control does not have input for power fraction at all speed levels and does not have a power curve.");
				errorsFound = true;
			}
		}

		if ( errorsFound ) {
			ShowFatalError( routineName + "Errors found in input.  Program terminates." );
		}

		SetupOutputVariable( "Fan Electric Power [W]", fanPower_, "System", "Average", name_ );
		SetupOutputVariable( "Fan Rise in Air Temperature [deltaC]", deltaTemp_, "System", "Average", name_ );
		SetupOutputVariable( "Fan Electric Energy [J]", fanEnergy_, "System", "Sum", name_, _, "Electric", "Fans", endUseSubcategoryName_, "System" );
		if ( speedControl_ == SpeedControlMethod::discrete && numSpeeds_ == 1 ) {
			SetupOutputVariable( "Fan Runtime Fraction []", fanRunTimeFractionAtSpeed_[ 0 ], "System", "Average", name_ );
		} else if ( speedControl_ == SpeedControlMethod::discrete && numSpeeds_ > 1 ) {
			for (auto speedLoop = 0; speedLoop < numSpeeds_; ++speedLoop) {
				SetupOutputVariable( "Fan Runtime Fraction Speed " + General::TrimSigDigits( speedLoop + 1 ) + " []", fanRunTimeFractionAtSpeed_[ speedLoop ], "System", "Average", name_ );
			}
		}

		if ( DataGlobals::AnyEnergyManagementSystemInModel ) {
			SetupEMSInternalVariable( "Fan Maximum Mass Flow Rate", name_ , "[kg/s]", maxAirMassFlowRate_ );
			SetupEMSActuator( "Fan", name_ , "Fan Air Mass Flow Rate", "[kg/s]", eMSMaxMassFlowOverrideOn_, eMSAirMassFlowValue_ );
			SetupEMSInternalVariable( "Fan Nominal Pressure Rise", name_ , "[Pa]", deltaPress_ );
			SetupEMSActuator( "Fan", name_ , "Fan Pressure Rise", "[Pa]", eMSFanPressureOverrideOn_, eMSFanPressureValue_ );
			SetupEMSInternalVariable( "Fan Nominal Total Efficiency", name_, "[fraction]", fanTotalEff_ );
			SetupEMSActuator( "Fan",name_ , "Fan Total Efficiency", "[fraction]", eMSFanEffOverrideOn_, eMSFanEffValue_ );
			SetupEMSActuator( "Fan", name_ , "Fan Autosized Air Flow Rate", "[m3/s]", maxAirFlowRateEMSOverrideOn_, maxAirFlowRateEMSOverrideValue_ );
		}
		bool anyEMSRan = false;
		EMSManager::ManageEMS( DataGlobals::emsCallFromComponentGetInput , anyEMSRan );
	}

	void
	FanSystem::calcSimpleSystemFan(
		Optional< Real64 const > flowFraction,
		Optional< Real64 const > pressureRise
	)
	{
		Real64 localPressureRise;
		Real64 localFlowFraction;
		Real64 localFanTotEff;
		Real64 localAirMassFlow;

		if ( DataHVACGlobals::NightVentOn ) {
		// assume if non-zero inputs for night data then this fan is to be used with that data
			if ( nightVentPressureDelta_ > 0.0 ) { 
				localPressureRise =  nightVentPressureDelta_;
			}
			if ( nightVentFlowFraction_ > 0.0 ) {
				localFlowFraction = nightVentFlowFraction_;
				localAirMassFlow = localFlowFraction * maxAirMassFlowRate_;
			}
		} else { // not in night mode
			if ( present( pressureRise ) ) {
				localPressureRise = pressureRise;
			} else {
				localPressureRise = deltaPress_;
			}
			if ( present( flowFraction ) ) {
				localFlowFraction = flowFraction;
				localAirMassFlow = localFlowFraction * maxAirMassFlowRate_;
			} else {
				localFlowFraction = inletAirMassFlowRate_ / maxAirMassFlowRate_;
				localAirMassFlow  = inletAirMassFlowRate_;
			}
		}

// TODO Faulty fan operation


		//EMS override MassFlow, DeltaPress, and FanEff
		if ( eMSFanPressureOverrideOn_ ) localPressureRise = eMSFanPressureValue_;
		if ( eMSFanEffOverrideOn_ ) localFanTotEff = eMSFanEffValue_;
		if ( eMSMaxMassFlowOverrideOn_ ) {
			localAirMassFlow = eMSAirMassFlowValue_;
		}

		localAirMassFlow = min( localAirMassFlow, maxAirMassFlowRate_ );
		localFlowFraction = localAirMassFlow / maxAirMassFlowRate_;

		if ( ( ScheduleManager::GetCurrentScheduleValue( availSchedIndex_ ) > 0.0 || objTurnFansOn_ ) && ! objTurnFansOff_ && localAirMassFlow > 0.0 ) {
			//fan is running

			switch ( speedControl_ ) {
			
			case SpeedControlMethod::discrete: {
				if ( numSpeeds_ == 1 ) { // CV or OnOff
					localFanTotEff = fanTotalEff_;
					fanRunTimeFractionAtSpeed_[ 0 ] = localFlowFraction;
					fanPower_ = fanRunTimeFractionAtSpeed_[ 0 ] * maxAirMassFlowRate_ * localPressureRise / ( localFanTotEff * rhoAirStdInit_ );
					Real64 fanShaftPower = motorEff_ * fanPower_;
					Real64 powerLossToAir = fanShaftPower + ( fanPower_ - fanShaftPower )* motorInAirFrac_;
					outletAirEnthalpy_ = inletAirEnthalpy_ + powerLossToAir / localAirMassFlow;
					outletAirHumRat_ = inletAirHumRat_;
					outletAirMassFlowRate_ =  localAirMassFlow;
					outletAirTemp_ = Psychrometrics::PsyTdbFnHW( outletAirEnthalpy_, outletAirHumRat_ );
				} else if ( numSpeeds_ > 1 ) { // multi speed

					// find which two speed levels bracket flow fraction and calculate runtimefraction
					int lowSideSpeed = -1;
					int hiSideSpeed  = -1;
					for ( auto loop = 0; loop < numSpeeds_; ++loop ) {
						fanRunTimeFractionAtSpeed_[ loop ] = 0.0;
					}

					if ( localFlowFraction < flowFractionAtSpeed_[ 0 ] ) { // on/off between zero and lowest speed
						hiSideSpeed  = 0;
						fanRunTimeFractionAtSpeed_[ 0 ] = localFlowFraction / flowFractionAtSpeed_[ 0 ];
					} else {
						for ( auto loop = 0; loop < numSpeeds_ - 1; ++loop ) {
							if ( ( flowFractionAtSpeed_[ loop ] <= localFlowFraction ) && ( localFlowFraction <= flowFractionAtSpeed_[ loop + 1 ] ) ) {
								lowSideSpeed = loop;
								hiSideSpeed = loop +1;
								break;
							}
						}
						fanRunTimeFractionAtSpeed_[ lowSideSpeed ] = ( flowFractionAtSpeed_[ hiSideSpeed ] - localFlowFraction ) / ( flowFractionAtSpeed_[ hiSideSpeed ] - flowFractionAtSpeed_[ lowSideSpeed ] );
						fanRunTimeFractionAtSpeed_[ hiSideSpeed ] = ( localFlowFraction - flowFractionAtSpeed_[ lowSideSpeed ] ) / ( flowFractionAtSpeed_[ hiSideSpeed ] - flowFractionAtSpeed_[ lowSideSpeed ] );
					}
					if ( lowSideSpeed != -1 && hiSideSpeed != -1 ) {
						fanPower_ = fanRunTimeFractionAtSpeed_[ lowSideSpeed ] * massFlowAtSpeed_[ lowSideSpeed ] * localPressureRise / ( totEfficAtSpeed_[ lowSideSpeed ] * rhoAirStdInit_ ) + fanRunTimeFractionAtSpeed_[ hiSideSpeed ] * massFlowAtSpeed_[ hiSideSpeed ] * localPressureRise / ( totEfficAtSpeed_[ hiSideSpeed ] * rhoAirStdInit_ );
					} else if ( lowSideSpeed == -1 && hiSideSpeed == 0 ) {
						fanPower_ = fanRunTimeFractionAtSpeed_[ hiSideSpeed ] * massFlowAtSpeed_[ hiSideSpeed ] * localPressureRise / ( totEfficAtSpeed_[ hiSideSpeed ] * rhoAirStdInit_ );
					}
					
					Real64 fanShaftPower = motorEff_ * fanPower_;
					Real64 powerLossToAir = fanShaftPower + ( fanPower_ - fanShaftPower )* motorInAirFrac_;
					outletAirEnthalpy_ = inletAirEnthalpy_ + powerLossToAir / localAirMassFlow;
					outletAirHumRat_ = inletAirHumRat_;
					outletAirMassFlowRate_ =  localAirMassFlow;
					outletAirTemp_ = Psychrometrics::PsyTdbFnHW( outletAirEnthalpy_, outletAirHumRat_ );
				}

				localFanTotEff = fanTotalEff_;
				break;
			}
			case SpeedControlMethod::continuous : {
				localFanTotEff = fanTotalEff_;
				Real64 localFlowFractionForPower = max( minPowerFlowFrac_, localFlowFraction );
				Real64 localPowerFraction = CurveManager::CurveValue( powerModFuncFlowFractionCurveIndex_, localFlowFractionForPower );
				fanPower_ = localPowerFraction * maxAirMassFlowRate_ * localPressureRise / ( localFanTotEff * rhoAirStdInit_ );
				Real64 fanShaftPower = motorEff_ * fanPower_;
				Real64 powerLossToAir = fanShaftPower + ( fanPower_ - fanShaftPower )* motorInAirFrac_;
				outletAirEnthalpy_ = inletAirEnthalpy_ + powerLossToAir / localAirMassFlow;
				outletAirHumRat_ = inletAirHumRat_;
				outletAirMassFlowRate_ =  localAirMassFlow;
				outletAirTemp_ = Psychrometrics::PsyTdbFnHW( outletAirEnthalpy_, outletAirHumRat_ );
				
				// When fan air flow is less than 10%, the fan power curve is linearized between the 10% to 0% to
			//  avoid the unrealistic high temperature rise across the fan.
				Real64 deltaTAcrossFan = outletAirTemp_ - inletAirTemp_;
				if ( deltaTAcrossFan > 20.0 ) {
					Real64 minFlowFracLimitFanHeat = 0.10;
					Real64 powerFractionAtLowMin = 0.0;
					Real64 fanPoweratLowMinimum = 0.0;
					if ( localFlowFractionForPower < minFlowFracLimitFanHeat ) {
						powerFractionAtLowMin = CurveManager::CurveValue( powerModFuncFlowFractionCurveIndex_, minFlowFracLimitFanHeat );

						fanPoweratLowMinimum = powerFractionAtLowMin * maxAirMassFlowRate_ * localPressureRise / ( localFanTotEff * rhoAirStdInit_ );
						fanPower_ = localFlowFractionForPower * fanPoweratLowMinimum / minFlowFracLimitFanHeat;
					} else if ( localFlowFraction < minFlowFracLimitFanHeat ) {

						powerFractionAtLowMin = CurveManager::CurveValue( powerModFuncFlowFractionCurveIndex_, minFlowFracLimitFanHeat );
						fanPoweratLowMinimum = powerFractionAtLowMin * maxAirMassFlowRate_ * localPressureRise / ( localFanTotEff * rhoAirStdInit_ );
						fanPower_ = localFlowFraction * fanPoweratLowMinimum / minFlowFracLimitFanHeat;
					}
					fanShaftPower = motorEff_ * fanPower_; // power delivered to shaft
					powerLossToAir = fanShaftPower + ( fanPower_ - fanShaftPower ) * motorInAirFrac_;
					outletAirEnthalpy_ = inletAirEnthalpy_ + powerLossToAir / localAirMassFlow;
					// This fan does not change the moisture or Mass Flow across the component
					outletAirHumRat_ = inletAirHumRat_;
					outletAirMassFlowRate_ = localAirMassFlow;
					outletAirTemp_ = Psychrometrics::PsyTdbFnHW( outletAirEnthalpy_, outletAirHumRat_ );
				}
				break;
			} // continuous speed control case
			} // end switch

		} else { // fan is off
			//Fan is off and not operating no power consumed and mass flow rate.
			fanPower_ = 0.0;
			outletAirMassFlowRate_ = 0.0;
			outletAirHumRat_ = inletAirHumRat_;
			outletAirEnthalpy_ = inletAirEnthalpy_;
			outletAirTemp_ = inletAirTemp_;
			// Set the Control Flow variables to 0.0 flow when OFF.
			massFlowRateMaxAvail_ = 0.0;
			massFlowRateMinAvail_ = 0.0;
		}
	}

	void
	FanSystem::update() const // does not change state of object, only update elsewhere
	{
		// Set the outlet air node of the fan
		DataLoopNode::Node( outletNodeNum_ ).MassFlowRate = outletAirMassFlowRate_;
		DataLoopNode::Node( outletNodeNum_ ).Temp         = outletAirTemp_;
		DataLoopNode::Node( outletNodeNum_ ).HumRat       = outletAirHumRat_;
		DataLoopNode::Node( outletNodeNum_ ).Enthalpy     = outletAirEnthalpy_;
		// Set the outlet nodes for properties that just pass through & not used
		DataLoopNode::Node( outletNodeNum_ ).Quality = DataLoopNode::Node( inletNodeNum_ ).Quality;
		DataLoopNode::Node( outletNodeNum_ ).Press   = DataLoopNode::Node( inletNodeNum_ ).Press;

		// Set the Node Flow Control Variables from the Fan Control Variables
		DataLoopNode::Node( outletNodeNum_ ).MassFlowRateMaxAvail = massFlowRateMaxAvail_;
		DataLoopNode::Node( outletNodeNum_ ).MassFlowRateMinAvail = massFlowRateMinAvail_;

		// make sure inlet has the same mass flow
		DataLoopNode::Node( inletNodeNum_ ).MassFlowRate = outletAirMassFlowRate_;

		if ( DataContaminantBalance::Contaminant.CO2Simulation ) {
			DataLoopNode::Node( outletNodeNum_ ).CO2 = DataLoopNode::Node( inletNodeNum_ ).CO2;
		}
		if ( DataContaminantBalance::Contaminant.GenericContamSimulation ) {
			DataLoopNode::Node( outletNodeNum_ ).GenContam = DataLoopNode::Node( inletNodeNum_ ).GenContam;
		}

		if ( heatLossesDestination_ == ThermalLossDestination::zoneGains ) {
	//TODO	
		
		}

		//oold fan had ugly use of globals here, use getter now
	//	DataHVACGlobals::FanElecPower = fanPower_;
		DataAirLoop::LoopOnOffFanRTF  = fanRunTimeFractionAtSpeed_[ numSpeeds_ - 1 ]; //fill with RTF from highest speed level

	}

	void
	FanSystem::report()
	{
		fanEnergy_ = fanPower_ * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
		deltaTemp_ = outletAirTemp_ - inletAirTemp_;
	}

	std::string const &
	FanSystem::name() const
	{
		return name_;
	}

	Real64
	FanSystem::fanPower() const
	{
		return fanPower_;
	}

	Real64
	FanSystem::designAirVolFlowRate() const
	{
		return designAirVolFlowRate_;
	}

	int
	FanSystem::inletNodeNum() const
	{
		return inletNodeNum_;
	}

	int
	FanSystem::outletNodeNum() const
	{
		return outletNodeNum_;
	}

	int
	FanSystem::availSchedIndex() const
	{
		return availSchedIndex_;
	}

	int
	FanSystem::getFanPowerCurveIndex() const
	{
		return powerModFuncFlowFractionCurveIndex_;
	}

	Real64
	FanSystem::getFanDesignTemperatureRise() const
	{
		if ( ! objSizingFlag_ ) {
			Real64 cpAir = Psychrometrics::PsyCpAirFnWTdb( DataPrecisionGlobals::constant_zero, DataPrecisionGlobals::constant_twenty );
			Real64 designDeltaT = ( deltaPress_ / ( rhoAirStdInit_ * cpAir * fanTotalEff_ ) ) * ( motorEff_ + motorInAirFrac_ * ( 1.0 - motorEff_ ) );
			return designDeltaT;
		} else {
			//TODO throw warning, exception, call sizing?
			ShowWarningError("FanSystem::getFanDesignTemperatureRise called before fan sizing completed ");
			return 0.0;
		}
	}

	Real64 
	FanSystem::getFanDesignHeatGain(
		Real64 const FanVolFlow // fan volume flow rate [m3/s]
	)
	{
		if ( ! objSizingFlag_ ) {
			Real64 fanPowerTot = ( FanVolFlow * deltaPress_ ) / fanTotalEff_ ;
			Real64 designHeatGain = motorEff_ * fanPowerTot + ( fanPowerTot - motorEff_ * fanPowerTot ) * motorInAirFrac_;
			return designHeatGain;
		} else {
			set_size();
			Real64 fanPowerTot = ( FanVolFlow * deltaPress_ ) / fanTotalEff_ ;
			Real64 designHeatGain = motorEff_ * fanPowerTot + ( fanPowerTot - motorEff_ * fanPowerTot ) * motorInAirFrac_;
			return designHeatGain;

		}
	}

	bool
	FanSystem::getIfContinuousSpeedControl() const 
	{
		if (speedControl_ == SpeedControlMethod::continuous ) {
			return true;
		} else {
			return false;
		}
	}

} //HVACFan namespace

} // EnergyPlus namespace
