#ifndef ObjexxFCL_Array1_Project_MArray_hh_INCLUDED
#define ObjexxFCL_Array1_Project_MArray_hh_INCLUDED

// Array1.Project.MArray: Project-Specific MArray Methods
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2015 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// Notes:
//  These are convenience methods to provide near-Fortran syntax for MArray usage

	// Types
	template< typename M > using MA1c = MArray1< Array1 const, M >;
	template< typename M > using MA1  = MArray1< Array1, M >;
	typedef  MA1c< bool >  MA1c_bool;
	typedef  MA1< bool >  MA1_bool;
	typedef  MA1c< int >  MA1c_int;
	typedef  MA1< int >  MA1_int;
	typedef  MA1c< double >  MA1c_double;
	typedef  MA1< double >  MA1_double;
	typedef  MA1c< std::string >  MA1c_string;
	typedef  MA1< std::string >  MA1_string;

	// bool const Members
	inline MA1c_bool AbsorpSolarEMSOverrideOn() const { return ma( &T::AbsorpSolarEMSOverrideOn ); }
	inline MA1c_bool AbsorpThermalEMSOverrideOn() const { return ma( &T::AbsorpThermalEMSOverrideOn ); }
	inline MA1c_bool AbsorpVisibleEMSOverrideOn() const { return ma( &T::AbsorpVisibleEMSOverrideOn ); }
	inline MA1c_bool Active() const { return ma( &T::Active ); }
	inline MA1c_bool active() const { return ma( &T::active ); }
	inline MA1c_bool activeNow() const { return ma( &T::activeNow ); }
	inline MA1c_bool AdaptiveASH55() const { return ma( &T::AdaptiveASH55 ); }
	inline MA1c_bool AdaptiveCEN15251() const { return ma( &T::AdaptiveCEN15251 ); }
	inline MA1c_bool Available() const { return ma( &T::Available ); }
	inline MA1c_bool CheckHeatRecoveryBypassStatus() const { return ma( &T::CheckHeatRecoveryBypassStatus ); }
	inline MA1c_bool ConvergedFlag() const { return ma( &T::ConvergedFlag ); }
	inline MA1c_bool CoolingActiveFlag() const { return ma( &T::CoolingActiveFlag ); }
	inline MA1c_bool DefinedFlag() const { return ma( &T::DefinedFlag ); }
	inline MA1c_bool EconomizerFlowLocked() const { return ma( &T::EconomizerFlowLocked ); }
	inline MA1c_bool EMSConstructionOverrideON() const { return ma( &T::EMSConstructionOverrideON ); }
	inline MA1c_bool FromWindow5DataFile() const { return ma( &T::FromWindow5DataFile ); }
	inline MA1c_bool HasInterZoneWindow() const { return ma( &T::HasInterZoneWindow ); }
	inline MA1c_bool HasWindow() const { return ma( &T::HasWindow ); }
	inline MA1c_bool HeatingActiveFlag() const { return ma( &T::HeatingActiveFlag ); }
	inline MA1c_bool HeatRecoveryBypass() const { return ma( &T::HeatRecoveryBypass ); }
	inline MA1c_bool HeatRecoveryResimFlag() const { return ma( &T::HeatRecoveryResimFlag ); }
	inline MA1c_bool HeatRecoveryResimFlag2() const { return ma( &T::HeatRecoveryResimFlag2 ); }
	inline MA1c_bool HybridCtrlGlobal() const { return ma( &T::HybridCtrlGlobal ); }
	inline MA1c_bool HybridCtrlMaster() const { return ma( &T::HybridCtrlMaster ); }
	inline MA1c_bool HybridVentClose() const { return ma( &T::HybridVentClose ); }
	inline MA1c_bool HybridVentMgrConnectedToAirLoop() const { return ma( &T::HybridVentMgrConnectedToAirLoop ); }
	inline MA1c_bool InitFlag() const { return ma( &T::InitFlag ); }
	inline MA1c_bool IsControlled() const { return ma( &T::IsControlled ); }
	inline MA1c_bool isReported() const { return ma( &T::isReported ); }
	inline MA1c_bool IsUsed() const { return ma( &T::IsUsed ); }
	inline MA1c_bool isUserDef() const { return ma( &T::isUserDef ); }
	inline MA1c_bool LoopFlowRateSet() const { return ma( &T::LoopFlowRateSet ); }
	inline MA1c_bool NightVent() const { return ma( &T::NightVent ); }
	inline MA1c_bool NoHeatToReturnAir() const { return ma( &T::NoHeatToReturnAir ); }
	inline MA1c_bool OASysComponentsSimulated() const { return ma( &T::OASysComponentsSimulated ); }
	inline MA1c_bool ONThisTimestep() const { return ma( &T::ONThisTimestep ); }
	inline MA1c_bool ResimAirLoopFlag() const { return ma( &T::ResimAirLoopFlag ); }
	inline MA1c_bool SimAirLoopsNeeded() const { return ma( &T::SimAirLoopsNeeded ); }
	inline MA1c_bool SimElectLoadCentrNeeded() const { return ma( &T::SimElectLoadCentrNeeded ); }
	inline MA1c_bool SimHybridVentSysAvailMgr() const { return ma( &T::SimHybridVentSysAvailMgr ); }
	inline MA1c_bool SimNonZoneEquipNeeded() const { return ma( &T::SimNonZoneEquipNeeded ); }
	inline MA1c_bool SimZoneEquipNeeded() const { return ma( &T::SimZoneEquipNeeded ); }
	inline MA1c_bool SourceSinkPresent() const { return ma( &T::SourceSinkPresent ); }
	inline MA1c_bool TypeIsIRT() const { return ma( &T::TypeIsIRT ); }
	inline MA1c_bool TypeIsWindow() const { return ma( &T::TypeIsWindow ); }
	inline MA1c_bool WindowTypeBSDF() const { return ma( &T::WindowTypeBSDF ); }
	inline MA1c_bool WindowTypeEQL() const { return ma( &T::WindowTypeEQL ); }
	inline MA1c_bool ZoneIsOccupied() const { return ma( &T::ZoneIsOccupied ); }

	// bool Members
	inline MA1_bool AbsorpSolarEMSOverrideOn() { return ma( &T::AbsorpSolarEMSOverrideOn ); }
	inline MA1_bool AbsorpThermalEMSOverrideOn() { return ma( &T::AbsorpThermalEMSOverrideOn ); }
	inline MA1_bool AbsorpVisibleEMSOverrideOn() { return ma( &T::AbsorpVisibleEMSOverrideOn ); }
	inline MA1_bool Active() { return ma( &T::Active ); }
	inline MA1_bool active() { return ma( &T::active ); }
	inline MA1_bool activeNow() { return ma( &T::activeNow ); }
	inline MA1_bool AdaptiveASH55() { return ma( &T::AdaptiveASH55 ); }
	inline MA1_bool AdaptiveCEN15251() { return ma( &T::AdaptiveCEN15251 ); }
	inline MA1_bool Available() { return ma( &T::Available ); }
	inline MA1_bool CheckHeatRecoveryBypassStatus() { return ma( &T::CheckHeatRecoveryBypassStatus ); }
	inline MA1_bool ConvergedFlag() { return ma( &T::ConvergedFlag ); }
	inline MA1_bool CoolingActiveFlag() { return ma( &T::CoolingActiveFlag ); }
	inline MA1_bool DefinedFlag() { return ma( &T::DefinedFlag ); }
	inline MA1_bool EconomizerFlowLocked() { return ma( &T::EconomizerFlowLocked ); }
	inline MA1_bool EMSConstructionOverrideON() { return ma( &T::EMSConstructionOverrideON ); }
	inline MA1_bool FromWindow5DataFile() { return ma( &T::FromWindow5DataFile ); }
	inline MA1_bool HasInterZoneWindow() { return ma( &T::HasInterZoneWindow ); }
	inline MA1_bool HasWindow() { return ma( &T::HasWindow ); }
	inline MA1_bool HeatingActiveFlag() { return ma( &T::HeatingActiveFlag ); }
	inline MA1_bool HeatRecoveryBypass() { return ma( &T::HeatRecoveryBypass ); }
	inline MA1_bool HeatRecoveryResimFlag() { return ma( &T::HeatRecoveryResimFlag ); }
	inline MA1_bool HeatRecoveryResimFlag2() { return ma( &T::HeatRecoveryResimFlag2 ); }
	inline MA1_bool HybridCtrlGlobal() { return ma( &T::HybridCtrlGlobal ); }
	inline MA1_bool HybridCtrlMaster() { return ma( &T::HybridCtrlMaster ); }
	inline MA1_bool HybridVentClose() { return ma( &T::HybridVentClose ); }
	inline MA1_bool HybridVentMgrConnectedToAirLoop() { return ma( &T::HybridVentMgrConnectedToAirLoop ); }
	inline MA1_bool InitFlag() { return ma( &T::InitFlag ); }
	inline MA1_bool IsControlled() { return ma( &T::IsControlled ); }
	inline MA1_bool isReported() { return ma( &T::isReported ); }
	inline MA1_bool IsUsed() { return ma( &T::IsUsed ); }
	inline MA1_bool isUserDef() { return ma( &T::isUserDef ); }
	inline MA1_bool LoopFlowRateSet() { return ma( &T::LoopFlowRateSet ); }
	inline MA1_bool NightVent() { return ma( &T::NightVent ); }
	inline MA1_bool NoHeatToReturnAir() { return ma( &T::NoHeatToReturnAir ); }
	inline MA1_bool OASysComponentsSimulated() { return ma( &T::OASysComponentsSimulated ); }
	inline MA1_bool ONThisTimestep() { return ma( &T::ONThisTimestep ); }
	inline MA1_bool ResimAirLoopFlag() { return ma( &T::ResimAirLoopFlag ); }
	inline MA1_bool SimAirLoopsNeeded() { return ma( &T::SimAirLoopsNeeded ); }
	inline MA1_bool SimElectLoadCentrNeeded() { return ma( &T::SimElectLoadCentrNeeded ); }
	inline MA1_bool SimHybridVentSysAvailMgr() { return ma( &T::SimHybridVentSysAvailMgr ); }
	inline MA1_bool SimNonZoneEquipNeeded() { return ma( &T::SimNonZoneEquipNeeded ); }
	inline MA1_bool SimZoneEquipNeeded() { return ma( &T::SimZoneEquipNeeded ); }
	inline MA1_bool SourceSinkPresent() { return ma( &T::SourceSinkPresent ); }
	inline MA1_bool TypeIsIRT() { return ma( &T::TypeIsIRT ); }
	inline MA1_bool TypeIsWindow() { return ma( &T::TypeIsWindow ); }
	inline MA1_bool WindowTypeBSDF() { return ma( &T::WindowTypeBSDF ); }
	inline MA1_bool WindowTypeEQL() { return ma( &T::WindowTypeEQL ); }
	inline MA1_bool ZoneIsOccupied() { return ma( &T::ZoneIsOccupied ); }

	// int const Members
	inline MA1c_int aggType() const { return ma( &T::aggType ); }
	inline MA1c_int AirLoopBranch() const { return ma( &T::AirLoopBranch ); }
	inline MA1c_int AirLoopComp() const { return ma( &T::AirLoopComp ); }
	inline MA1c_int AirLoopNum() const { return ma( &T::AirLoopNum ); }
	inline MA1c_int AirLoopSubComp() const { return ma( &T::AirLoopSubComp ); }
	inline MA1c_int AirLoopSubSubComp() const { return ma( &T::AirLoopSubSubComp ); }
	inline MA1c_int AvailStatus() const { return ma( &T::AvailStatus ); }
	inline MA1c_int avgSum() const { return ma( &T::avgSum ); }
	inline MA1c_int CommonPipeType() const { return ma( &T::CommonPipeType ); }
	inline MA1c_int Construction() const { return ma( &T::Construction ); }
	inline MA1c_int CoolCapNegativeCounter() const { return ma( &T::CoolCapNegativeCounter ); }
	inline MA1c_int CoolCapNegativeIndex() const { return ma( &T::CoolCapNegativeIndex ); }
	inline MA1c_int CoolDDNum() const { return ma( &T::CoolDDNum ); }
	inline MA1c_int CoolPowerNegativeCounter() const { return ma( &T::CoolPowerNegativeCounter ); }
	inline MA1c_int CoolPowerNegativeIndex() const { return ma( &T::CoolPowerNegativeIndex ); }
	inline MA1c_int ElapsedRotationTime() const { return ma( &T::ElapsedRotationTime ); }
	inline MA1c_int ElapsedTime() const { return ma( &T::ElapsedTime ); }
	inline MA1c_int EquipPtr() const { return ma( &T::EquipPtr ); }
	inline MA1c_int EquipType_Num() const { return ma( &T::EquipType_Num ); }
	inline MA1c_int ExtIntShadePrevTS() const { return ma( &T::ExtIntShadePrevTS ); }
	inline MA1c_int firstColumn() const { return ma( &T::firstColumn ); }
	inline MA1c_int FirstDemandSidePtr() const { return ma( &T::FirstDemandSidePtr ); }
	inline MA1c_int firstStep() const { return ma( &T::firstStep ); }
	inline MA1c_int FlowCtrl() const { return ma( &T::FlowCtrl ); }
	inline MA1c_int FluidStream() const { return ma( &T::FluidStream ); }
	inline MA1c_int GeneralEquipType() const { return ma( &T::GeneralEquipType ); }
	inline MA1c_int GlycolIndex() const { return ma( &T::GlycolIndex ); }
	inline MA1c_int HCoilType_Num() const { return ma( &T::HCoilType_Num ); }
	inline MA1c_int HeatCapNegativeCounter() const { return ma( &T::HeatCapNegativeCounter ); }
	inline MA1c_int HeatCapNegativeIndex() const { return ma( &T::HeatCapNegativeIndex ); }
	inline MA1c_int HeatDDNum() const { return ma( &T::HeatDDNum ); }
	inline MA1c_int HeatPowerNegativeCounter() const { return ma( &T::HeatPowerNegativeCounter ); }
	inline MA1c_int HeatPowerNegativeIndex() const { return ma( &T::HeatPowerNegativeIndex ); }
	inline MA1c_int HeatTransferAlgorithm() const { return ma( &T::HeatTransferAlgorithm ); }
	inline MA1c_int InsideConvectionAlgo() const { return ma( &T::InsideConvectionAlgo ); }
	inline MA1c_int IntConvCoeff() const { return ma( &T::IntConvCoeff ); }
	inline MA1c_int LastDemandSidePtr() const { return ma( &T::LastDemandSidePtr ); }
	inline MA1c_int LastLoopSideSimulated() const { return ma( &T::LastLoopSideSimulated ); }
	inline MA1c_int lastStep() const { return ma( &T::lastStep ); }
	inline MA1c_int LoopNum() const { return ma( &T::LoopNum ); }
	inline MA1c_int LoopType() const { return ma( &T::LoopType ); }
	inline MA1c_int Mode() const { return ma( &T::Mode ); }
	inline MA1c_int MundtZoneIndex() const { return ma( &T::MundtZoneIndex ); }
	inline MA1c_int n() const { return ma( &T::n ); }
	inline MA1c_int NodeNumber() const { return ma( &T::NodeNumber ); }
	inline MA1c_int NodeNumOut() const { return ma( &T::NodeNumOut ); }
	inline MA1c_int numColumns() const { return ma( &T::numColumns ); }
	inline MA1c_int NumCTFTerms() const { return ma( &T::NumCTFTerms ); }
	inline MA1c_int NumHistories() const { return ma( &T::NumHistories ); }
	inline MA1c_int NumOfDayltgExtWins() const { return ma( &T::NumOfDayltgExtWins ); }
	inline MA1c_int NumOfSurfs() const { return ma( &T::NumOfSurfs ); }
	inline MA1c_int NumRefDoorConnections() const { return ma( &T::NumRefDoorConnections ); }
	inline MA1c_int NumSubSurfaces() const { return ma( &T::NumSubSurfaces ); }
	inline MA1c_int NVert() const { return ma( &T::NVert ); }
	inline MA1c_int OutsideConvectionAlgo() const { return ma( &T::OutsideConvectionAlgo ); }
	inline MA1c_int PatrnID() const { return ma( &T::PatrnID ); }
	inline MA1c_int PlantLoopBranch() const { return ma( &T::PlantLoopBranch ); }
	inline MA1c_int PlantLoopComp() const { return ma( &T::PlantLoopComp ); }
	inline MA1c_int PlantLoopNum() const { return ma( &T::PlantLoopNum ); }
	inline MA1c_int PlantLoopType() const { return ma( &T::PlantLoopType ); }
	inline MA1c_int RotatedLoadNum() const { return ma( &T::RotatedLoadNum ); }
	inline MA1c_int ShadingFlag() const { return ma( &T::ShadingFlag ); }
	inline MA1c_int SolutionDimensions() const { return ma( &T::SolutionDimensions ); }
	inline MA1c_int SourceAfterLayer() const { return ma( &T::SourceAfterLayer ); }
	inline MA1c_int StartTime() const { return ma( &T::StartTime ); }
	inline MA1c_int stepType() const { return ma( &T::stepType ); }
	inline MA1c_int StopTime() const { return ma( &T::StopTime ); }
	inline MA1c_int SurfFirst() const { return ma( &T::SurfFirst ); }
	inline MA1c_int TempAfterLayer() const { return ma( &T::TempAfterLayer ); }
	inline MA1c_int TimeStepNumAtCoolMax() const { return ma( &T::TimeStepNumAtCoolMax ); }
	inline MA1c_int TimeStepNumAtHeatMax() const { return ma( &T::TimeStepNumAtHeatMax ); }
	inline MA1c_int TotalDaylRefPoints() const { return ma( &T::TotalDaylRefPoints ); }
	inline MA1c_int typeOfVar() const { return ma( &T::typeOfVar ); }
	inline MA1c_int varNum() const { return ma( &T::varNum ); }
	inline MA1c_int W5FrameDivider() const { return ma( &T::W5FrameDivider ); }
	inline MA1c_int ZoneEqCompNum() const { return ma( &T::ZoneEqCompNum ); }
	inline MA1c_int ZoneEqListNum() const { return ma( &T::ZoneEqListNum ); }
	inline MA1c_int ZoneEqSubCompNum() const { return ma( &T::ZoneEqSubCompNum ); }
	inline MA1c_int ZoneEqSubSubCompNum() const { return ma( &T::ZoneEqSubSubCompNum ); }
	inline MA1c_int ZoneNum() const { return ma( &T::ZoneNum ); }
	inline MA1c_int ZonePtr() const { return ma( &T::ZonePtr ); }

	// int Members
	inline MA1_int aggType() { return ma( &T::aggType ); }
	inline MA1_int AirLoopBranch() { return ma( &T::AirLoopBranch ); }
	inline MA1_int AirLoopComp() { return ma( &T::AirLoopComp ); }
	inline MA1_int AirLoopNum() { return ma( &T::AirLoopNum ); }
	inline MA1_int AirLoopSubComp() { return ma( &T::AirLoopSubComp ); }
	inline MA1_int AirLoopSubSubComp() { return ma( &T::AirLoopSubSubComp ); }
	inline MA1_int AvailStatus() { return ma( &T::AvailStatus ); }
	inline MA1_int avgSum() { return ma( &T::avgSum ); }
	inline MA1_int CommonPipeType() { return ma( &T::CommonPipeType ); }
	inline MA1_int Construction() { return ma( &T::Construction ); }
	inline MA1_int CoolCapNegativeCounter() { return ma( &T::CoolCapNegativeCounter ); }
	inline MA1_int CoolCapNegativeIndex() { return ma( &T::CoolCapNegativeIndex ); }
	inline MA1_int CoolDDNum() { return ma( &T::CoolDDNum ); }
	inline MA1_int CoolPowerNegativeCounter() { return ma( &T::CoolPowerNegativeCounter ); }
	inline MA1_int CoolPowerNegativeIndex() { return ma( &T::CoolPowerNegativeIndex ); }
	inline MA1_int ElapsedRotationTime() { return ma( &T::ElapsedRotationTime ); }
	inline MA1_int ElapsedTime() { return ma( &T::ElapsedTime ); }
	inline MA1_int EquipPtr() { return ma( &T::EquipPtr ); }
	inline MA1_int EquipType_Num() { return ma( &T::EquipType_Num ); }
	inline MA1_int ExtIntShadePrevTS() { return ma( &T::ExtIntShadePrevTS ); }
	inline MA1_int firstColumn() { return ma( &T::firstColumn ); }
	inline MA1_int FirstDemandSidePtr() { return ma( &T::FirstDemandSidePtr ); }
	inline MA1_int firstStep() { return ma( &T::firstStep ); }
	inline MA1_int FlowCtrl() { return ma( &T::FlowCtrl ); }
	inline MA1_int FluidStream() { return ma( &T::FluidStream ); }
	inline MA1_int GeneralEquipType() { return ma( &T::GeneralEquipType ); }
	inline MA1_int GlycolIndex() { return ma( &T::GlycolIndex ); }
	inline MA1_int HCoilType_Num() { return ma( &T::HCoilType_Num ); }
	inline MA1_int HeatCapNegativeCounter() { return ma( &T::HeatCapNegativeCounter ); }
	inline MA1_int HeatCapNegativeIndex() { return ma( &T::HeatCapNegativeIndex ); }
	inline MA1_int HeatDDNum() { return ma( &T::HeatDDNum ); }
	inline MA1_int HeatPowerNegativeCounter() { return ma( &T::HeatPowerNegativeCounter ); }
	inline MA1_int HeatPowerNegativeIndex() { return ma( &T::HeatPowerNegativeIndex ); }
	inline MA1_int HeatTransferAlgorithm() { return ma( &T::HeatTransferAlgorithm ); }
	inline MA1_int InsideConvectionAlgo() { return ma( &T::InsideConvectionAlgo ); }
	inline MA1_int IntConvCoeff() { return ma( &T::IntConvCoeff ); }
	inline MA1_int LastDemandSidePtr() { return ma( &T::LastDemandSidePtr ); }
	inline MA1_int LastLoopSideSimulated() { return ma( &T::LastLoopSideSimulated ); }
	inline MA1_int lastStep() { return ma( &T::lastStep ); }
	inline MA1_int LoopNum() { return ma( &T::LoopNum ); }
	inline MA1_int LoopType() { return ma( &T::LoopType ); }
	inline MA1_int Mode() { return ma( &T::Mode ); }
	inline MA1_int MundtZoneIndex() { return ma( &T::MundtZoneIndex ); }
	inline MA1_int n() { return ma( &T::n ); }
	inline MA1_int NodeNumber() { return ma( &T::NodeNumber ); }
	inline MA1_int NodeNumOut() { return ma( &T::NodeNumOut ); }
	inline MA1_int numColumns() { return ma( &T::numColumns ); }
	inline MA1_int NumCTFTerms() { return ma( &T::NumCTFTerms ); }
	inline MA1_int NumHistories() { return ma( &T::NumHistories ); }
	inline MA1_int NumOfDayltgExtWins() { return ma( &T::NumOfDayltgExtWins ); }
	inline MA1_int NumOfSurfs() { return ma( &T::NumOfSurfs ); }
	inline MA1_int NumRefDoorConnections() { return ma( &T::NumRefDoorConnections ); }
	inline MA1_int NumSubSurfaces() { return ma( &T::NumSubSurfaces ); }
	inline MA1_int NVert() { return ma( &T::NVert ); }
	inline MA1_int OutsideConvectionAlgo() { return ma( &T::OutsideConvectionAlgo ); }
	inline MA1_int PatrnID() { return ma( &T::PatrnID ); }
	inline MA1_int PlantLoopBranch() { return ma( &T::PlantLoopBranch ); }
	inline MA1_int PlantLoopComp() { return ma( &T::PlantLoopComp ); }
	inline MA1_int PlantLoopNum() { return ma( &T::PlantLoopNum ); }
	inline MA1_int PlantLoopType() { return ma( &T::PlantLoopType ); }
	inline MA1_int RotatedLoadNum() { return ma( &T::RotatedLoadNum ); }
	inline MA1_int ShadingFlag() { return ma( &T::ShadingFlag ); }
	inline MA1_int SolutionDimensions() { return ma( &T::SolutionDimensions ); }
	inline MA1_int SourceAfterLayer() { return ma( &T::SourceAfterLayer ); }
	inline MA1_int StartTime() { return ma( &T::StartTime ); }
	inline MA1_int stepType() { return ma( &T::stepType ); }
	inline MA1_int StopTime() { return ma( &T::StopTime ); }
	inline MA1_int SurfFirst() { return ma( &T::SurfFirst ); }
	inline MA1_int TempAfterLayer() { return ma( &T::TempAfterLayer ); }
	inline MA1_int TimeStepNumAtCoolMax() { return ma( &T::TimeStepNumAtCoolMax ); }
	inline MA1_int TimeStepNumAtHeatMax() { return ma( &T::TimeStepNumAtHeatMax ); }
	inline MA1_int TotalDaylRefPoints() { return ma( &T::TotalDaylRefPoints ); }
	inline MA1_int typeOfVar() { return ma( &T::typeOfVar ); }
	inline MA1_int varNum() { return ma( &T::varNum ); }
	inline MA1_int W5FrameDivider() { return ma( &T::W5FrameDivider ); }
	inline MA1_int ZoneEqCompNum() { return ma( &T::ZoneEqCompNum ); }
	inline MA1_int ZoneEqListNum() { return ma( &T::ZoneEqListNum ); }
	inline MA1_int ZoneEqSubCompNum() { return ma( &T::ZoneEqSubCompNum ); }
	inline MA1_int ZoneEqSubSubCompNum() { return ma( &T::ZoneEqSubSubCompNum ); }
	inline MA1_int ZoneNum() { return ma( &T::ZoneNum ); }
	inline MA1_int ZonePtr() { return ma( &T::ZonePtr ); }

	// double const Members
	inline MA1c_double ActualCondenserFanPower() const { return ma( &T::ActualCondenserFanPower ); }
	inline MA1c_double ActualEvapPumpPower() const { return ma( &T::ActualEvapPumpPower ); }
	inline MA1c_double ActualFanPower() const { return ma( &T::ActualFanPower ); }
	inline MA1c_double ActuatedValue() const { return ma( &T::ActuatedValue ); }
	inline MA1c_double AirDensity() const { return ma( &T::AirDensity ); }
	inline MA1c_double AirSup() const { return ma( &T::AirSup ); }
	inline MA1c_double AmbientZoneGain() const { return ma( &T::AmbientZoneGain ); }
	inline MA1c_double AncillACuseEnergy() const { return ma( &T::AncillACuseEnergy ); }
	inline MA1c_double AncillACuseRate() const { return ma( &T::AncillACuseRate ); }
	inline MA1c_double Area() const { return ma( &T::Area ); }
	inline MA1c_double AuxilHeat() const { return ma( &T::AuxilHeat ); }
	inline MA1c_double AvailCapacity() const { return ma( &T::AvailCapacity ); }
	inline MA1c_double AvailTemperature() const { return ma( &T::AvailTemperature ); }
	inline MA1c_double AverageCompressorCOP() const { return ma( &T::AverageCompressorCOP ); }
	inline MA1c_double Azimuth() const { return ma( &T::Azimuth ); }
	inline MA1c_double BalMassFlowRate() const { return ma( &T::BalMassFlowRate ); }
	inline MA1c_double BaseHeatPower() const { return ma( &T::BaseHeatPower ); }
	inline MA1c_double BmSolAbsdInsReveal() const { return ma( &T::BmSolAbsdInsReveal ); }
	inline MA1c_double BmSolAbsdInsRevealReport() const { return ma( &T::BmSolAbsdInsRevealReport ); }
	inline MA1c_double BmSolAbsdOutsReveal() const { return ma( &T::BmSolAbsdOutsReveal ); }
	inline MA1c_double BmSolRefldInsReveal() const { return ma( &T::BmSolRefldInsReveal ); }
	inline MA1c_double BmSolRefldInsRevealReport() const { return ma( &T::BmSolRefldInsRevealReport ); }
	inline MA1c_double BmSolRefldOutsRevealReport() const { return ma( &T::BmSolRefldOutsRevealReport ); }
	inline MA1c_double BmSolTransThruIntWinRep() const { return ma( &T::BmSolTransThruIntWinRep ); }
	inline MA1c_double BmSolTransThruIntWinRepEnergy() const { return ma( &T::BmSolTransThruIntWinRepEnergy ); }
	inline MA1c_double BulkTemperature() const { return ma( &T::BulkTemperature ); }
	inline MA1c_double BypassFrac() const { return ma( &T::BypassFrac ); }
	inline MA1c_double CoeffA0() const { return ma( &T::CoeffA0 ); }
	inline MA1c_double CoeffA1() const { return ma( &T::CoeffA1 ); }
	inline MA1c_double CoeffA10() const { return ma( &T::CoeffA10 ); }
	inline MA1c_double CoeffA11() const { return ma( &T::CoeffA11 ); }
	inline MA1c_double CoeffA12() const { return ma( &T::CoeffA12 ); }
	inline MA1c_double CoeffA13() const { return ma( &T::CoeffA13 ); }
	inline MA1c_double CoeffA14() const { return ma( &T::CoeffA14 ); }
	inline MA1c_double CoeffA15() const { return ma( &T::CoeffA15 ); }
	inline MA1c_double CoeffA16() const { return ma( &T::CoeffA16 ); }
	inline MA1c_double CoeffA17() const { return ma( &T::CoeffA17 ); }
	inline MA1c_double CoeffA18() const { return ma( &T::CoeffA18 ); }
	inline MA1c_double CoeffA19() const { return ma( &T::CoeffA19 ); }
	inline MA1c_double CoeffA2() const { return ma( &T::CoeffA2 ); }
	inline MA1c_double CoeffA20() const { return ma( &T::CoeffA20 ); }
	inline MA1c_double CoeffA21() const { return ma( &T::CoeffA21 ); }
	inline MA1c_double CoeffA22() const { return ma( &T::CoeffA22 ); }
	inline MA1c_double CoeffA23() const { return ma( &T::CoeffA23 ); }
	inline MA1c_double CoeffA24() const { return ma( &T::CoeffA24 ); }
	inline MA1c_double CoeffA25() const { return ma( &T::CoeffA25 ); }
	inline MA1c_double CoeffA26() const { return ma( &T::CoeffA26 ); }
	inline MA1c_double CoeffA3() const { return ma( &T::CoeffA3 ); }
	inline MA1c_double CoeffA4() const { return ma( &T::CoeffA4 ); }
	inline MA1c_double CoeffA5() const { return ma( &T::CoeffA5 ); }
	inline MA1c_double CoeffA6() const { return ma( &T::CoeffA6 ); }
	inline MA1c_double CoeffA7() const { return ma( &T::CoeffA7 ); }
	inline MA1c_double CoeffA8() const { return ma( &T::CoeffA8 ); }
	inline MA1c_double CoeffA9() const { return ma( &T::CoeffA9 ); }
	inline MA1c_double CoinCoolMassFlow() const { return ma( &T::CoinCoolMassFlow ); }
	inline MA1c_double CoinHeatMassFlow() const { return ma( &T::CoinHeatMassFlow ); }
	inline MA1c_double ColdMassFlowRate() const { return ma( &T::ColdMassFlowRate ); }
	inline MA1c_double ColdVolFlowRate() const { return ma( &T::ColdVolFlowRate ); }
	inline MA1c_double CompPowerLossFactor() const { return ma( &T::CompPowerLossFactor ); }
	inline MA1c_double CondEnergy() const { return ma( &T::CondEnergy ); }
	inline MA1c_double CondenserFanConsumption() const { return ma( &T::CondenserFanConsumption ); }
	inline MA1c_double CondLoad() const { return ma( &T::CondLoad ); }
	inline MA1c_double CondSen() const { return ma( &T::CondSen ); }
	inline MA1c_double CondSenGainJ() const { return ma( &T::CondSenGainJ ); }
	inline MA1c_double CondSenGainW() const { return ma( &T::CondSenGainW ); }
	inline MA1c_double CondSenLossJ() const { return ma( &T::CondSenLossJ ); }
	inline MA1c_double CondSenLossW() const { return ma( &T::CondSenLossW ); }
	inline MA1c_double CoolCap1() const { return ma( &T::CoolCap1 ); }
	inline MA1c_double CoolCap2() const { return ma( &T::CoolCap2 ); }
	inline MA1c_double CoolCap3() const { return ma( &T::CoolCap3 ); }
	inline MA1c_double CoolCap4() const { return ma( &T::CoolCap4 ); }
	inline MA1c_double CoolCap5() const { return ma( &T::CoolCap5 ); }
	inline MA1c_double CoolCycleTime() const { return ma( &T::CoolCycleTime ); }
	inline MA1c_double CoolingDemand() const { return ma( &T::CoolingDemand ); }
	inline MA1c_double CoolPower1() const { return ma( &T::CoolPower1 ); }
	inline MA1c_double CoolPower2() const { return ma( &T::CoolPower2 ); }
	inline MA1c_double CoolPower3() const { return ma( &T::CoolPower3 ); }
	inline MA1c_double CoolPower4() const { return ma( &T::CoolPower4 ); }
	inline MA1c_double CoolPower5() const { return ma( &T::CoolPower5 ); }
	inline MA1c_double CurrentMaxAvail() const { return ma( &T::CurrentMaxAvail ); }
	inline MA1c_double CurrentMinAvail() const { return ma( &T::CurrentMinAvail ); }
	inline MA1c_double DCElectProdRate() const { return ma( &T::DCElectProdRate ); }
	inline MA1c_double DCElectricityProd() const { return ma( &T::DCElectricityProd ); }
	inline MA1c_double DCpowerConditionLosses() const { return ma( &T::DCpowerConditionLosses ); }
	inline MA1c_double DecrementedEnergyStored() const { return ma( &T::DecrementedEnergyStored ); }
	inline MA1c_double DefEnergyCurveValue() const { return ma( &T::DefEnergyCurveValue ); }
	inline MA1c_double DefrostEnergy() const { return ma( &T::DefrostEnergy ); }
	inline MA1c_double DeltaPress() const { return ma( &T::DeltaPress ); }
	inline MA1c_double DeltaT() const { return ma( &T::DeltaT ); }
	inline MA1c_double DemandNotDispatched() const { return ma( &T::DemandNotDispatched ); }
	inline MA1c_double Density() const { return ma( &T::Density ); }
	inline MA1c_double DesCoolCoilInHumRat() const { return ma( &T::DesCoolCoilInHumRat ); }
	inline MA1c_double DesCoolCoilInTemp() const { return ma( &T::DesCoolCoilInTemp ); }
	inline MA1c_double DesCoolDens() const { return ma( &T::DesCoolDens ); }
	inline MA1c_double DesCoolLoad() const { return ma( &T::DesCoolLoad ); }
	inline MA1c_double DesCoolMassFlow() const { return ma( &T::DesCoolMassFlow ); }
	inline MA1c_double DesCoolVolFlow() const { return ma( &T::DesCoolVolFlow ); }
	inline MA1c_double DesHeatCoilInHumRat() const { return ma( &T::DesHeatCoilInHumRat ); }
	inline MA1c_double DesHeatCoilInTemp() const { return ma( &T::DesHeatCoilInTemp ); }
	inline MA1c_double DesHeatDens() const { return ma( &T::DesHeatDens ); }
	inline MA1c_double DesHeatLoad() const { return ma( &T::DesHeatLoad ); }
	inline MA1c_double DesHeatMassFlow() const { return ma( &T::DesHeatMassFlow ); }
	inline MA1c_double DesHeatVolFlow() const { return ma( &T::DesHeatVolFlow ); }
	inline MA1c_double DesignLevel() const { return ma( &T::DesignLevel ); }
	inline MA1c_double DesMainVolFlow() const { return ma( &T::DesMainVolFlow ); }
	inline MA1c_double DesVolFlowRate() const { return ma( &T::DesVolFlowRate ); }
	inline MA1c_double DiffLat() const { return ma( &T::DiffLat ); }
	inline MA1c_double DiffLatGainJ() const { return ma( &T::DiffLatGainJ ); }
	inline MA1c_double DiffLatGainW() const { return ma( &T::DiffLatGainW ); }
	inline MA1c_double DiffLatLossJ() const { return ma( &T::DiffLatLossJ ); }
	inline MA1c_double DiffLatLossW() const { return ma( &T::DiffLatLossW ); }
	inline MA1c_double DistPipeZoneHeatGain() const { return ma( &T::DistPipeZoneHeatGain ); }
	inline MA1c_double DividerQRadInAbs() const { return ma( &T::DividerQRadInAbs ); }
	inline MA1c_double DividerQRadOutAbs() const { return ma( &T::DividerQRadOutAbs ); }
	inline MA1c_double DividerTempSurfIn() const { return ma( &T::DividerTempSurfIn ); }
	inline MA1c_double DividerTempSurfInOld() const { return ma( &T::DividerTempSurfInOld ); }
	inline MA1c_double DividerTempSurfOut() const { return ma( &T::DividerTempSurfOut ); }
	inline MA1c_double DrainTemp() const { return ma( &T::DrainTemp ); }
	inline MA1c_double DrawnEnergy() const { return ma( &T::DrawnEnergy ); }
	inline MA1c_double DrawnPower() const { return ma( &T::DrawnPower ); }
	inline MA1c_double EEConvected() const { return ma( &T::EEConvected ); }
	inline MA1c_double EELatent() const { return ma( &T::EELatent ); }
	inline MA1c_double EELost() const { return ma( &T::EELost ); }
	inline MA1c_double EERadiated() const { return ma( &T::EERadiated ); }
	inline MA1c_double Efficiency() const { return ma( &T::Efficiency ); }
	inline MA1c_double EffInsSurfTemp() const { return ma( &T::EffInsSurfTemp ); }
	inline MA1c_double ElecAntiSweatConsumption() const { return ma( &T::ElecAntiSweatConsumption ); }
	inline MA1c_double ElecAntiSweatPower() const { return ma( &T::ElecAntiSweatPower ); }
	inline MA1c_double ElecConsumption() const { return ma( &T::ElecConsumption ); }
	inline MA1c_double ElecDefrostConsumption() const { return ma( &T::ElecDefrostConsumption ); }
	inline MA1c_double ElecDefrostPower() const { return ma( &T::ElecDefrostPower ); }
	inline MA1c_double ElecFanConsumption() const { return ma( &T::ElecFanConsumption ); }
	inline MA1c_double ElecFanPower() const { return ma( &T::ElecFanPower ); }
	inline MA1c_double ElecHeaterConsumption() const { return ma( &T::ElecHeaterConsumption ); }
	inline MA1c_double ElecHeaterPower() const { return ma( &T::ElecHeaterPower ); }
	inline MA1c_double ElecLightingConsumption() const { return ma( &T::ElecLightingConsumption ); }
	inline MA1c_double ElecLightingPower() const { return ma( &T::ElecLightingPower ); }
	inline MA1c_double ElecPower() const { return ma( &T::ElecPower ); }
	inline MA1c_double ElecStorage() const { return ma( &T::ElecStorage ); }
	inline MA1c_double ElectDemand() const { return ma( &T::ElectDemand ); }
	inline MA1c_double ElectEnergyinStorage() const { return ma( &T::ElectEnergyinStorage ); }
	inline MA1c_double ElectProdRate() const { return ma( &T::ElectProdRate ); }
	inline MA1c_double ElectricityProd() const { return ma( &T::ElectricityProd ); }
	inline MA1c_double Energy() const { return ma( &T::Energy ); }
	inline MA1c_double Enthalpy() const { return ma( &T::Enthalpy ); }
	inline MA1c_double EnthalpyLastTimestep() const { return ma( &T::EnthalpyLastTimestep ); }
	inline MA1c_double ERVMassFlowRate() const { return ma( &T::ERVMassFlowRate ); }
	inline MA1c_double EvapPumpConsumption() const { return ma( &T::EvapPumpConsumption ); }
	inline MA1c_double EvapWaterConsumpRate() const { return ma( &T::EvapWaterConsumpRate ); }
	inline MA1c_double EvapWaterConsumption() const { return ma( &T::EvapWaterConsumption ); }
	inline MA1c_double ExhaustHX() const { return ma( &T::ExhaustHX ); }
	inline MA1c_double ExhMassFlowRate() const { return ma( &T::ExhMassFlowRate ); }
	inline MA1c_double ExitTemp() const { return ma( &T::ExitTemp ); }
	inline MA1c_double ExternalEnergyRecovered() const { return ma( &T::ExternalEnergyRecovered ); }
	inline MA1c_double ExternalHeatRecoveredLoad() const { return ma( &T::ExternalHeatRecoveredLoad ); }
	inline MA1c_double ExtGrossWallArea() const { return ma( &T::ExtGrossWallArea ); }
	inline MA1c_double ExtNetWallArea() const { return ma( &T::ExtNetWallArea ); }
	inline MA1c_double ExtWindowArea() const { return ma( &T::ExtWindowArea ); }
	inline MA1c_double FanEff() const { return ma( &T::FanEff ); }
	inline MA1c_double FanElecEnergy() const { return ma( &T::FanElecEnergy ); }
	inline MA1c_double FCPM() const { return ma( &T::FCPM ); }
	inline MA1c_double FloorArea() const { return ma( &T::FloorArea ); }
	inline MA1c_double FLOW() const { return ma( &T::FLOW ); }
	inline MA1c_double FLOW2() const { return ma( &T::FLOW2 ); }
	inline MA1c_double FrameQRadInAbs() const { return ma( &T::FrameQRadInAbs ); }
	inline MA1c_double FrameQRadOutAbs() const { return ma( &T::FrameQRadOutAbs ); }
	inline MA1c_double FrameTempSurfIn() const { return ma( &T::FrameTempSurfIn ); }
	inline MA1c_double FrameTempSurfInOld() const { return ma( &T::FrameTempSurfInOld ); }
	inline MA1c_double FrameTempSurfOut() const { return ma( &T::FrameTempSurfOut ); }
	inline MA1c_double FuelEnergy() const { return ma( &T::FuelEnergy ); }
	inline MA1c_double GasCoolerEnergy() const { return ma( &T::GasCoolerEnergy ); }
	inline MA1c_double GasCoolerLoad() const { return ma( &T::GasCoolerLoad ); }
	inline MA1c_double GasPower() const { return ma( &T::GasPower ); }
	inline MA1c_double GCTime() const { return ma( &T::GCTime ); }
	inline MA1c_double Hc() const { return ma( &T::Hc ); }
	inline MA1c_double HeatCap() const { return ma( &T::HeatCap ); }
	inline MA1c_double HeatCap1() const { return ma( &T::HeatCap1 ); }
	inline MA1c_double HeatCap2() const { return ma( &T::HeatCap2 ); }
	inline MA1c_double HeatCap3() const { return ma( &T::HeatCap3 ); }
	inline MA1c_double HeatCap4() const { return ma( &T::HeatCap4 ); }
	inline MA1c_double HeatCap5() const { return ma( &T::HeatCap5 ); }
	inline MA1c_double HeatCycleTime() const { return ma( &T::HeatCycleTime ); }
	inline MA1c_double HeatGain() const { return ma( &T::HeatGain ); }
	inline MA1c_double HeatingDemand() const { return ma( &T::HeatingDemand ); }
	inline MA1c_double HeatingToZoneEnergy() const { return ma( &T::HeatingToZoneEnergy ); }
	inline MA1c_double HeatingToZoneRate() const { return ma( &T::HeatingToZoneRate ); }
	inline MA1c_double HeatLoss() const { return ma( &T::HeatLoss ); }
	inline MA1c_double HeatMixHumRat() const { return ma( &T::HeatMixHumRat ); }
	inline MA1c_double HeatMixTemp() const { return ma( &T::HeatMixTemp ); }
	inline MA1c_double HeatOutHumRat() const { return ma( &T::HeatOutHumRat ); }
	inline MA1c_double HeatOutTemp() const { return ma( &T::HeatOutTemp ); }
	inline MA1c_double HeatPower1() const { return ma( &T::HeatPower1 ); }
	inline MA1c_double HeatPower2() const { return ma( &T::HeatPower2 ); }
	inline MA1c_double HeatPower3() const { return ma( &T::HeatPower3 ); }
	inline MA1c_double HeatPower4() const { return ma( &T::HeatPower4 ); }
	inline MA1c_double HeatPower5() const { return ma( &T::HeatPower5 ); }
	inline MA1c_double HeatRetHumRat() const { return ma( &T::HeatRetHumRat ); }
	inline MA1c_double HeatRetTemp() const { return ma( &T::HeatRetTemp ); }
	inline MA1c_double HotDefrostCondCredit() const { return ma( &T::HotDefrostCondCredit ); }
	inline MA1c_double HotMassFlowRate() const { return ma( &T::HotMassFlowRate ); }
	inline MA1c_double HotVolFlowRate() const { return ma( &T::HotVolFlowRate ); }
	inline MA1c_double HumRat() const { return ma( &T::HumRat ); }
	inline MA1c_double HumRatMax() const { return ma( &T::HumRatMax ); }
	inline MA1c_double HumRatMin() const { return ma( &T::HumRatMin ); }
	inline MA1c_double HumRatSetPoint() const { return ma( &T::HumRatSetPoint ); }
	inline MA1c_double HWPower() const { return ma( &T::HWPower ); }
	inline MA1c_double In() const { return ma( &T::In ); }
	inline MA1c_double InfilAirChangeRate() const { return ma( &T::InfilAirChangeRate ); }
	inline MA1c_double InfilHeatGain() const { return ma( &T::InfilHeatGain ); }
	inline MA1c_double InfilHeatLoss() const { return ma( &T::InfilHeatLoss ); }
	inline MA1c_double InfilMass() const { return ma( &T::InfilMass ); }
	inline MA1c_double InfilVolume() const { return ma( &T::InfilVolume ); }
	inline MA1c_double InfMassFlowRate() const { return ma( &T::InfMassFlowRate ); }
	inline MA1c_double InitialVolume() const { return ma( &T::InitialVolume ); }
	inline MA1c_double InletNodeFlowrate() const { return ma( &T::InletNodeFlowrate ); }
	inline MA1c_double InletNodeTemperature() const { return ma( &T::InletNodeTemperature ); }
	inline MA1c_double InsRevealDiffIntoZone() const { return ma( &T::InsRevealDiffIntoZone ); }
	inline MA1c_double InsRevealDiffIntoZoneReport() const { return ma( &T::InsRevealDiffIntoZoneReport ); }
	inline MA1c_double InsRevealDiffOntoFrame() const { return ma( &T::InsRevealDiffOntoFrame ); }
	inline MA1c_double InsRevealDiffOntoFrameReport() const { return ma( &T::InsRevealDiffOntoFrameReport ); }
	inline MA1c_double InsRevealDiffOntoGlazing() const { return ma( &T::InsRevealDiffOntoGlazing ); }
	inline MA1c_double InsRevealDiffOntoGlazingReport() const { return ma( &T::InsRevealDiffOntoGlazingReport ); }
	inline MA1c_double InternalEnergyRecovered() const { return ma( &T::InternalEnergyRecovered ); }
	inline MA1c_double InternalHeatRecoveredLoad() const { return ma( &T::InternalHeatRecoveredLoad ); }
	inline MA1c_double IntMassFlowRate() const { return ma( &T::IntMassFlowRate ); }
	inline MA1c_double Inverter() const { return ma( &T::Inverter ); }
	inline MA1c_double IRfromParentZone() const { return ma( &T::IRfromParentZone ); }
	inline MA1c_double KgFrost() const { return ma( &T::KgFrost ); }
	inline MA1c_double KgFrostSaved() const { return ma( &T::KgFrostSaved ); }
	inline MA1c_double LaggedUsedHVACCoil() const { return ma( &T::LaggedUsedHVACCoil ); }
	inline MA1c_double LaggedUsedWaterHeater() const { return ma( &T::LaggedUsedWaterHeater ); }
	inline MA1c_double LastEventTime() const { return ma( &T::LastEventTime ); }
	inline MA1c_double LatCaseCreditToHVAC() const { return ma( &T::LatCaseCreditToHVAC ); }
	inline MA1c_double LatCaseCreditToZone() const { return ma( &T::LatCaseCreditToZone ); }
	inline MA1c_double LatCoolEnergyRate() const { return ma( &T::LatCoolEnergyRate ); }
	inline MA1c_double LatCoolingEnergy() const { return ma( &T::LatCoolingEnergy ); }
	inline MA1c_double LatCoolingEnergyRate() const { return ma( &T::LatCoolingEnergyRate ); }
	inline MA1c_double LatCoolingToZoneEnergy() const { return ma( &T::LatCoolingToZoneEnergy ); }
	inline MA1c_double LatCoolingToZoneRate() const { return ma( &T::LatCoolingToZoneRate ); }
	inline MA1c_double LatCreditEnergy() const { return ma( &T::LatCreditEnergy ); }
	inline MA1c_double LatCreditRate() const { return ma( &T::LatCreditRate ); }
	inline MA1c_double LatCreditToZoneEnergy() const { return ma( &T::LatCreditToZoneEnergy ); }
	inline MA1c_double LatCreditToZoneRate() const { return ma( &T::LatCreditToZoneRate ); }
	inline MA1c_double LatEnergyCurveValue() const { return ma( &T::LatEnergyCurveValue ); }
	inline MA1c_double LatentEnergy() const { return ma( &T::LatentEnergy ); }
	inline MA1c_double LatentRate() const { return ma( &T::LatentRate ); }
	inline MA1c_double LatentRateNoMultiplier() const { return ma( &T::LatentRateNoMultiplier ); }
	inline MA1c_double LatHeatEnergyRate() const { return ma( &T::LatHeatEnergyRate ); }
	inline MA1c_double LatHVACCredit() const { return ma( &T::LatHVACCredit ); }
	inline MA1c_double LatHVACCreditRate() const { return ma( &T::LatHVACCreditRate ); }
	inline MA1c_double LatKgPerS_ToZone() const { return ma( &T::LatKgPerS_ToZone ); }
	inline MA1c_double LatKgPerS_ToZoneRate() const { return ma( &T::LatKgPerS_ToZoneRate ); }
	inline MA1c_double LatZoneCredit() const { return ma( &T::LatZoneCredit ); }
	inline MA1c_double LatZoneCreditRate() const { return ma( &T::LatZoneCreditRate ); }
	inline MA1c_double LeakLat() const { return ma( &T::LeakLat ); }
	inline MA1c_double LeakLatGainJ() const { return ma( &T::LeakLatGainJ ); }
	inline MA1c_double LeakLatGainW() const { return ma( &T::LeakLatGainW ); }
	inline MA1c_double LeakLatLossJ() const { return ma( &T::LeakLatLossJ ); }
	inline MA1c_double LeakLatLossW() const { return ma( &T::LeakLatLossW ); }
	inline MA1c_double LeakSen() const { return ma( &T::LeakSen ); }
	inline MA1c_double LeakSenGainJ() const { return ma( &T::LeakSenGainJ ); }
	inline MA1c_double LeakSenGainW() const { return ma( &T::LeakSenGainW ); }
	inline MA1c_double LeakSenLossJ() const { return ma( &T::LeakSenLossJ ); }
	inline MA1c_double LeakSenLossW() const { return ma( &T::LeakSenLossW ); }
	inline MA1c_double LineSubTotal() const { return ma( &T::LineSubTotal ); }
	inline MA1c_double LoadSideInletTemp() const { return ma( &T::LoadSideInletTemp ); }
	inline MA1c_double LoadSideMassFlowRate() const { return ma( &T::LoadSideMassFlowRate ); }
	inline MA1c_double LoadSideOutletTemp() const { return ma( &T::LoadSideOutletTemp ); }
	inline MA1c_double LtsPower() const { return ma( &T::LtsPower ); }
	inline MA1c_double MassFlowAtCoolPeak() const { return ma( &T::MassFlowAtCoolPeak ); }
	inline MA1c_double MassFlowFromLower() const { return ma( &T::MassFlowFromLower ); }
	inline MA1c_double MassFlowFromUpper() const { return ma( &T::MassFlowFromUpper ); }
	inline MA1c_double MassFlowRate() const { return ma( &T::MassFlowRate ); }
	inline MA1c_double MassFlowRateMax() const { return ma( &T::MassFlowRateMax ); }
	inline MA1c_double MassFlowRateMaxAvail() const { return ma( &T::MassFlowRateMaxAvail ); }
	inline MA1c_double MassFlowRateMin() const { return ma( &T::MassFlowRateMin ); }
	inline MA1c_double MassFlowRateMinAvail() const { return ma( &T::MassFlowRateMinAvail ); }
	inline MA1c_double MassFlowRateSetPoint() const { return ma( &T::MassFlowRateSetPoint ); }
	inline MA1c_double MassFlowToLower() const { return ma( &T::MassFlowToLower ); }
	inline MA1c_double MassFlowToUpper() const { return ma( &T::MassFlowToUpper ); }
	inline MA1c_double MaxAirFlowRate() const { return ma( &T::MaxAirFlowRate ); }
	inline MA1c_double MaxAirMassFlowRate() const { return ma( &T::MaxAirMassFlowRate ); }
	inline MA1c_double maximum() const { return ma( &T::maximum ); }
	inline MA1c_double MaxProcAirInHumRat() const { return ma( &T::MaxProcAirInHumRat ); }
	inline MA1c_double MaxProcAirInTemp() const { return ma( &T::MaxProcAirInTemp ); }
	inline MA1c_double MeanAirTemp() const { return ma( &T::MeanAirTemp ); }
	inline MA1c_double Med() const { return ma( &T::Med ); }
	inline MA1c_double minimum() const { return ma( &T::minimum ); }
	inline MA1c_double MinProcAirInHumRat() const { return ma( &T::MinProcAirInHumRat ); }
	inline MA1c_double MinProcAirInTemp() const { return ma( &T::MinProcAirInTemp ); }
	inline MA1c_double MixedTemp() const { return ma( &T::MixedTemp ); }
	inline MA1c_double MixHumRatAtCoolPeak() const { return ma( &T::MixHumRatAtCoolPeak ); }
	inline MA1c_double MixMass() const { return ma( &T::MixMass ); }
	inline MA1c_double MixTempAtCoolPeak() const { return ma( &T::MixTempAtCoolPeak ); }
	inline MA1c_double MixVolume() const { return ma( &T::MixVolume ); }
	inline MA1c_double MotEff() const { return ma( &T::MotEff ); }
	inline MA1c_double MotInAirFrac() const { return ma( &T::MotInAirFrac ); }
	inline MA1c_double MultiZoneInfiLatGainJ() const { return ma( &T::MultiZoneInfiLatGainJ ); }
	inline MA1c_double MultiZoneInfiLatGainW() const { return ma( &T::MultiZoneInfiLatGainW ); }
	inline MA1c_double MultiZoneInfiLatLossJ() const { return ma( &T::MultiZoneInfiLatLossJ ); }
	inline MA1c_double MultiZoneInfiLatLossW() const { return ma( &T::MultiZoneInfiLatLossW ); }
	inline MA1c_double MultiZoneInfiSenGainJ() const { return ma( &T::MultiZoneInfiSenGainJ ); }
	inline MA1c_double MultiZoneInfiSenGainW() const { return ma( &T::MultiZoneInfiSenGainW ); }
	inline MA1c_double MultiZoneInfiSenLossJ() const { return ma( &T::MultiZoneInfiSenLossJ ); }
	inline MA1c_double MultiZoneInfiSenLossW() const { return ma( &T::MultiZoneInfiSenLossW ); }
	inline MA1c_double MultiZoneLat() const { return ma( &T::MultiZoneLat ); }
	inline MA1c_double MultiZoneMixLatGainJ() const { return ma( &T::MultiZoneMixLatGainJ ); }
	inline MA1c_double MultiZoneMixLatGainW() const { return ma( &T::MultiZoneMixLatGainW ); }
	inline MA1c_double MultiZoneMixLatLossJ() const { return ma( &T::MultiZoneMixLatLossJ ); }
	inline MA1c_double MultiZoneMixLatLossW() const { return ma( &T::MultiZoneMixLatLossW ); }
	inline MA1c_double MultiZoneMixSenGainJ() const { return ma( &T::MultiZoneMixSenGainJ ); }
	inline MA1c_double MultiZoneMixSenGainW() const { return ma( &T::MultiZoneMixSenGainW ); }
	inline MA1c_double MultiZoneMixSenLossJ() const { return ma( &T::MultiZoneMixSenLossJ ); }
	inline MA1c_double MultiZoneMixSenLossW() const { return ma( &T::MultiZoneMixSenLossW ); }
	inline MA1c_double MultiZoneSen() const { return ma( &T::MultiZoneSen ); }
	inline MA1c_double MyLoad() const { return ma( &T::MyLoad ); }
	inline MA1c_double NatMassFlowRate() const { return ma( &T::NatMassFlowRate ); }
	inline MA1c_double NetHeatRejectEnergy() const { return ma( &T::NetHeatRejectEnergy ); }
	inline MA1c_double NetHeatRejectLoad() const { return ma( &T::NetHeatRejectLoad ); }
	inline MA1c_double NewTemp() const { return ma( &T::NewTemp ); }
	inline MA1c_double NOFOCC() const { return ma( &T::NOFOCC ); }
	inline MA1c_double NominalCapacity() const { return ma( &T::NominalCapacity ); }
	inline MA1c_double NonCoinCoolMassFlow() const { return ma( &T::NonCoinCoolMassFlow ); }
	inline MA1c_double NonCoinHeatMassFlow() const { return ma( &T::NonCoinHeatMassFlow ); }
	inline MA1c_double OffCycParaFuelEnergy() const { return ma( &T::OffCycParaFuelEnergy ); }
	inline MA1c_double OnCycParaFuelEnergy() const { return ma( &T::OnCycParaFuelEnergy ); }
	inline MA1c_double OperativeTemp() const { return ma( &T::OperativeTemp ); }
	inline MA1c_double Out() const { return ma( &T::Out ); }
	inline MA1c_double OutDryBulbTemp() const { return ma( &T::OutDryBulbTemp ); }
	inline MA1c_double OutHumRatAtCoolPeak() const { return ma( &T::OutHumRatAtCoolPeak ); }
	inline MA1c_double OutHumRatAtHeatPeak() const { return ma( &T::OutHumRatAtHeatPeak ); }
	inline MA1c_double OutletNodeFlowrate() const { return ma( &T::OutletNodeFlowrate ); }
	inline MA1c_double OutletNodeTemperature() const { return ma( &T::OutletNodeTemperature ); }
	inline MA1c_double OutRoom() const { return ma( &T::OutRoom ); }
	inline MA1c_double OutsRevealDiffOntoFrame() const { return ma( &T::OutsRevealDiffOntoFrame ); }
	inline MA1c_double OutsRevealDiffOntoGlazing() const { return ma( &T::OutsRevealDiffOntoGlazing ); }
	inline MA1c_double OutTempAtCoolPeak() const { return ma( &T::OutTempAtCoolPeak ); }
	inline MA1c_double OutTempAtHeatPeak() const { return ma( &T::OutTempAtHeatPeak ); }
	inline MA1c_double OutWetBulbTemp() const { return ma( &T::OutWetBulbTemp ); }
	inline MA1c_double PastBulkTemperature() const { return ma( &T::PastBulkTemperature ); }
	inline MA1c_double PelFromStorage() const { return ma( &T::PelFromStorage ); }
	inline MA1c_double PelIntoStorage() const { return ma( &T::PelIntoStorage ); }
	inline MA1c_double PelNeedFromStorage() const { return ma( &T::PelNeedFromStorage ); }
	inline MA1c_double PipeHeatLoad() const { return ma( &T::PipeHeatLoad ); }
	inline MA1c_double PipeHeatLoadLT() const { return ma( &T::PipeHeatLoadLT ); }
	inline MA1c_double PipeHeatLoadMT() const { return ma( &T::PipeHeatLoadMT ); }
	inline MA1c_double Power() const { return ma( &T::Power ); }
	inline MA1c_double PreheatCap() const { return ma( &T::PreheatCap ); }
	inline MA1c_double Press() const { return ma( &T::Press ); }
	inline MA1c_double PumpElecEnergyTotal() const { return ma( &T::PumpElecEnergyTotal ); }
	inline MA1c_double PumpHeatToFluid() const { return ma( &T::PumpHeatToFluid ); }
	inline MA1c_double PumpPowerTotal() const { return ma( &T::PumpPowerTotal ); }
	inline MA1c_double QBBCON() const { return ma( &T::QBBCON ); }
	inline MA1c_double QBBRAD() const { return ma( &T::QBBRAD ); }
	inline MA1c_double QconvZone() const { return ma( &T::QconvZone ); }
	inline MA1c_double QdotConvZone() const { return ma( &T::QdotConvZone ); }
	inline MA1c_double QdotRadZone() const { return ma( &T::QdotRadZone ); }
	inline MA1c_double QEECON() const { return ma( &T::QEECON ); }
	inline MA1c_double QEELAT() const { return ma( &T::QEELAT ); }
	inline MA1c_double QEELost() const { return ma( &T::QEELost ); }
	inline MA1c_double QEERAD() const { return ma( &T::QEERAD ); }
	inline MA1c_double QGECON() const { return ma( &T::QGECON ); }
	inline MA1c_double QGELAT() const { return ma( &T::QGELAT ); }
	inline MA1c_double QGELost() const { return ma( &T::QGELost ); }
	inline MA1c_double QGERAD() const { return ma( &T::QGERAD ); }
	inline MA1c_double QHWCON() const { return ma( &T::QHWCON ); }
	inline MA1c_double QHWLAT() const { return ma( &T::QHWLAT ); }
	inline MA1c_double QHWLost() const { return ma( &T::QHWLost ); }
	inline MA1c_double QHWRAD() const { return ma( &T::QHWRAD ); }
	inline MA1c_double QLoad() const { return ma( &T::QLoad ); }
	inline MA1c_double QLoadEnergy() const { return ma( &T::QLoadEnergy ); }
	inline MA1c_double QLTCON() const { return ma( &T::QLTCON ); }
	inline MA1c_double QLTCRA() const { return ma( &T::QLTCRA ); }
	inline MA1c_double QLTRAD() const { return ma( &T::QLTRAD ); }
	inline MA1c_double QLTSW() const { return ma( &T::QLTSW ); }
	inline MA1c_double QLTTOT() const { return ma( &T::QLTTOT ); }
	inline MA1c_double QOCCON() const { return ma( &T::QOCCON ); }
	inline MA1c_double QOCLAT() const { return ma( &T::QOCLAT ); }
	inline MA1c_double QOCRAD() const { return ma( &T::QOCRAD ); }
	inline MA1c_double QOCSEN() const { return ma( &T::QOCSEN ); }
	inline MA1c_double QOCTOT() const { return ma( &T::QOCTOT ); }
	inline MA1c_double QOECON() const { return ma( &T::QOECON ); }
	inline MA1c_double QOELAT() const { return ma( &T::QOELAT ); }
	inline MA1c_double QOELost() const { return ma( &T::QOELost ); }
	inline MA1c_double QOERAD() const { return ma( &T::QOERAD ); }
	inline MA1c_double QradZone() const { return ma( &T::QradZone ); }
	inline MA1c_double Qrec() const { return ma( &T::Qrec ); }
	inline MA1c_double QSECON() const { return ma( &T::QSECON ); }
	inline MA1c_double QSELAT() const { return ma( &T::QSELAT ); }
	inline MA1c_double QSELost() const { return ma( &T::QSELost ); }
	inline MA1c_double QSERAD() const { return ma( &T::QSERAD ); }
	inline MA1c_double QskinLoss() const { return ma( &T::QskinLoss ); }
	inline MA1c_double QSource() const { return ma( &T::QSource ); }
	inline MA1c_double QSourceEnergy() const { return ma( &T::QSourceEnergy ); }
	inline MA1c_double Quality() const { return ma( &T::Quality ); }
	inline MA1c_double RackCompressorPower() const { return ma( &T::RackCompressorPower ); }
	inline MA1c_double RackElecConsumption() const { return ma( &T::RackElecConsumption ); }
	inline MA1c_double RadCoolingEnergy() const { return ma( &T::RadCoolingEnergy ); }
	inline MA1c_double RadCoolingPower() const { return ma( &T::RadCoolingPower ); }
	inline MA1c_double RadHeatingEnergy() const { return ma( &T::RadHeatingEnergy ); }
	inline MA1c_double RadHeatingPower() const { return ma( &T::RadHeatingPower ); }
	inline MA1c_double RatedCapCool() const { return ma( &T::RatedCapCool ); }
	inline MA1c_double RatedCapHeat() const { return ma( &T::RatedCapHeat ); }
	inline MA1c_double RatedLoadVolFlowCool() const { return ma( &T::RatedLoadVolFlowCool ); }
	inline MA1c_double RatedLoadVolFlowHeat() const { return ma( &T::RatedLoadVolFlowHeat ); }
	inline MA1c_double RatedPowerCool() const { return ma( &T::RatedPowerCool ); }
	inline MA1c_double RatedPowerHeat() const { return ma( &T::RatedPowerHeat ); }
	inline MA1c_double RatedSourceVolFlowCool() const { return ma( &T::RatedSourceVolFlowCool ); }
	inline MA1c_double RatedSourceVolFlowHeat() const { return ma( &T::RatedSourceVolFlowHeat ); }
	inline MA1c_double ReceiverZoneHeatGain() const { return ma( &T::ReceiverZoneHeatGain ); }
	inline MA1c_double RelHumidity() const { return ma( &T::RelHumidity ); }
	inline MA1c_double Report() const { return ma( &T::Report ); }
	inline MA1c_double ReportEnthalpy() const { return ma( &T::ReportEnthalpy ); }
	inline MA1c_double ReportH20RemovedKgPerS_FromZoneRate() const { return ma( &T::ReportH20RemovedKgPerS_FromZoneRate ); }
	inline MA1c_double ReportHeatingCreditEnergy() const { return ma( &T::ReportHeatingCreditEnergy ); }
	inline MA1c_double ReportHeatingCreditRate() const { return ma( &T::ReportHeatingCreditRate ); }
	inline MA1c_double ReportLatCreditToZoneEnergy() const { return ma( &T::ReportLatCreditToZoneEnergy ); }
	inline MA1c_double ReportLatCreditToZoneRate() const { return ma( &T::ReportLatCreditToZoneRate ); }
	inline MA1c_double ReportSenCoolingToZoneEnergy() const { return ma( &T::ReportSenCoolingToZoneEnergy ); }
	inline MA1c_double ReportSenCoolingToZoneRate() const { return ma( &T::ReportSenCoolingToZoneRate ); }
	inline MA1c_double ReportSensCoolCreditEnergy() const { return ma( &T::ReportSensCoolCreditEnergy ); }
	inline MA1c_double ReportSensCoolCreditRate() const { return ma( &T::ReportSensCoolCreditRate ); }
	inline MA1c_double ReportTotalCoolCreditEnergy() const { return ma( &T::ReportTotalCoolCreditEnergy ); }
	inline MA1c_double ReportTotalCoolCreditRate() const { return ma( &T::ReportTotalCoolCreditRate ); }
	inline MA1c_double ReportTotCoolingToZoneEnergy() const { return ma( &T::ReportTotCoolingToZoneEnergy ); }
	inline MA1c_double ReportTotCoolingToZoneRate() const { return ma( &T::ReportTotCoolingToZoneRate ); }
	inline MA1c_double ReqSupplyFrac() const { return ma( &T::ReqSupplyFrac ); }
	inline MA1c_double RetHumRatAtCoolPeak() const { return ma( &T::RetHumRatAtCoolPeak ); }
	inline MA1c_double RetTempAtCoolPeak() const { return ma( &T::RetTempAtCoolPeak ); }
	inline MA1c_double SavedTemp() const { return ma( &T::SavedTemp ); }
	inline MA1c_double SenCaseCreditToHVAC() const { return ma( &T::SenCaseCreditToHVAC ); }
	inline MA1c_double SenCaseCreditToZone() const { return ma( &T::SenCaseCreditToZone ); }
	inline MA1c_double SenCaseCreditToZoneEnergy() const { return ma( &T::SenCaseCreditToZoneEnergy ); }
	inline MA1c_double SenCoolingToZoneEnergy() const { return ma( &T::SenCoolingToZoneEnergy ); }
	inline MA1c_double SenCoolingToZoneRate() const { return ma( &T::SenCoolingToZoneRate ); }
	inline MA1c_double SenCreditToZoneEnergy() const { return ma( &T::SenCreditToZoneEnergy ); }
	inline MA1c_double SenCreditToZoneRate() const { return ma( &T::SenCreditToZoneRate ); }
	inline MA1c_double SensCoolCap() const { return ma( &T::SensCoolCap ); }
	inline MA1c_double SensCoolEnergyRate() const { return ma( &T::SensCoolEnergyRate ); }
	inline MA1c_double SensCoolingEnergy() const { return ma( &T::SensCoolingEnergy ); }
	inline MA1c_double SensCoolingEnergyRate() const { return ma( &T::SensCoolingEnergyRate ); }
	inline MA1c_double SensCreditRate() const { return ma( &T::SensCreditRate ); }
	inline MA1c_double SensHeatEnergyRate() const { return ma( &T::SensHeatEnergyRate ); }
	inline MA1c_double SensHeatRatio() const { return ma( &T::SensHeatRatio ); }
	inline MA1c_double SensHVACCreditCool() const { return ma( &T::SensHVACCreditCool ); }
	inline MA1c_double SensHVACCreditCoolRate() const { return ma( &T::SensHVACCreditCoolRate ); }
	inline MA1c_double SensHVACCreditHeat() const { return ma( &T::SensHVACCreditHeat ); }
	inline MA1c_double SensHVACCreditHeatRate() const { return ma( &T::SensHVACCreditHeatRate ); }
	inline MA1c_double SensHVACCreditRate() const { return ma( &T::SensHVACCreditRate ); }
	inline MA1c_double SensibleEnergy() const { return ma( &T::SensibleEnergy ); }
	inline MA1c_double SensibleRate() const { return ma( &T::SensibleRate ); }
	inline MA1c_double SensibleRateNoMultiplier() const { return ma( &T::SensibleRateNoMultiplier ); }
	inline MA1c_double SensZoneCreditCool() const { return ma( &T::SensZoneCreditCool ); }
	inline MA1c_double SensZoneCreditCoolRate() const { return ma( &T::SensZoneCreditCoolRate ); }
	inline MA1c_double SensZoneCreditHeat() const { return ma( &T::SensZoneCreditHeat ); }
	inline MA1c_double SensZoneCreditHeatRate() const { return ma( &T::SensZoneCreditHeatRate ); }
	inline MA1c_double SensZoneCreditRate() const { return ma( &T::SensZoneCreditRate ); }
	inline MA1c_double SMMaxVal() const { return ma( &T::SMMaxVal ); }
	inline MA1c_double SMMinVal() const { return ma( &T::SMMinVal ); }
	inline MA1c_double SMValue() const { return ma( &T::SMValue ); }
	inline MA1c_double SourceMassFlowRate() const { return ma( &T::SourceMassFlowRate ); }
	inline MA1c_double SourceSideInletTemp() const { return ma( &T::SourceSideInletTemp ); }
	inline MA1c_double SourceSideMassFlowRate() const { return ma( &T::SourceSideMassFlowRate ); }
	inline MA1c_double SourceSideOutletTemp() const { return ma( &T::SourceSideOutletTemp ); }
	inline MA1c_double StackCooler() const { return ma( &T::StackCooler ); }
	inline MA1c_double StartingEnergyStored() const { return ma( &T::StartingEnergyStored ); }
	inline MA1c_double SteamPower() const { return ma( &T::SteamPower ); }
	inline MA1c_double StockingEnergy() const { return ma( &T::StockingEnergy ); }
	inline MA1c_double StoredEnergy() const { return ma( &T::StoredEnergy ); }
	inline MA1c_double StoredPower() const { return ma( &T::StoredPower ); }
	inline MA1c_double sum() const { return ma( &T::sum ); }
	inline MA1c_double sum2() const { return ma( &T::sum2 ); }
	inline MA1c_double SumMCp() const { return ma( &T::SumMCp ); }
	inline MA1c_double SumMCpT() const { return ma( &T::SumMCpT ); }
	inline MA1c_double SumMechSCBenefit() const { return ma( &T::SumMechSCBenefit ); }
	inline MA1c_double SumMHr() const { return ma( &T::SumMHr ); }
	inline MA1c_double SumMHrCO() const { return ma( &T::SumMHrCO ); }
	inline MA1c_double SumMHrGC() const { return ma( &T::SumMHrGC ); }
	inline MA1c_double SumMHrW() const { return ma( &T::SumMHrW ); }
	inline MA1c_double SumMMCp() const { return ma( &T::SumMMCp ); }
	inline MA1c_double SumMMCpT() const { return ma( &T::SumMMCpT ); }
	inline MA1c_double SumMMHr() const { return ma( &T::SumMMHr ); }
	inline MA1c_double SumMMHrCO() const { return ma( &T::SumMMHrCO ); }
	inline MA1c_double SumMMHrGC() const { return ma( &T::SumMMHrGC ); }
	inline MA1c_double SumMMHrW() const { return ma( &T::SumMMHrW ); }
	inline MA1c_double SumSecondaryLoopLoad() const { return ma( &T::SumSecondaryLoopLoad ); }
	inline MA1c_double TadjacentAir() const { return ma( &T::TadjacentAir ); }
	inline MA1c_double Temp() const { return ma( &T::Temp ); }
	inline MA1c_double TempAvg() const { return ma( &T::TempAvg ); }
	inline MA1c_double TempLastTimestep() const { return ma( &T::TempLastTimestep ); }
	inline MA1c_double TempMax() const { return ma( &T::TempMax ); }
	inline MA1c_double TempMin() const { return ma( &T::TempMin ); }
	inline MA1c_double tempp1() const { return ma( &T::tempp1 ); }
	inline MA1c_double TempSetPoint() const { return ma( &T::TempSetPoint ); }
	inline MA1c_double TempSetPointHi() const { return ma( &T::TempSetPointHi ); }
	inline MA1c_double TempSetPointLo() const { return ma( &T::TempSetPointLo ); }
	inline MA1c_double TempSum() const { return ma( &T::TempSum ); }
	inline MA1c_double ThermalProd() const { return ma( &T::ThermalProd ); }
	inline MA1c_double ThermalProdRate() const { return ma( &T::ThermalProdRate ); }
	inline MA1c_double ThermLossEnergy() const { return ma( &T::ThermLossEnergy ); }
	inline MA1c_double ThermLossRate() const { return ma( &T::ThermLossRate ); }
	inline MA1c_double ThicknessPerpend() const { return ma( &T::ThicknessPerpend ); }
	inline MA1c_double ThisTimeStepStateOfCharge() const { return ma( &T::ThisTimeStepStateOfCharge ); }
	inline MA1c_double ThisTimeStepVolume() const { return ma( &T::ThisTimeStepVolume ); }
	inline MA1c_double Tilt() const { return ma( &T::Tilt ); }
	inline MA1c_double TimeElapsed() const { return ma( &T::TimeElapsed ); }
	inline MA1c_double TotalCO2() const { return ma( &T::TotalCO2 ); }
	inline MA1c_double TotalCondDefrostCredit() const { return ma( &T::TotalCondDefrostCredit ); }
	inline MA1c_double TotalCoolingEnergy() const { return ma( &T::TotalCoolingEnergy ); }
	inline MA1c_double TotalCoolingLoad() const { return ma( &T::TotalCoolingLoad ); }
	inline MA1c_double TotalCoolingLoadLT() const { return ma( &T::TotalCoolingLoadLT ); }
	inline MA1c_double TotalCoolingLoadMT() const { return ma( &T::TotalCoolingLoadMT ); }
	inline MA1c_double TotalElecConsumption() const { return ma( &T::TotalElecConsumption ); }
	inline MA1c_double TotalElecPower() const { return ma( &T::TotalElecPower ); }
	inline MA1c_double TotalGC() const { return ma( &T::TotalGC ); }
	inline MA1c_double TotalHeatRecoveredEnergy() const { return ma( &T::TotalHeatRecoveredEnergy ); }
	inline MA1c_double TotalHeatRecoveredLoad() const { return ma( &T::TotalHeatRecoveredLoad ); }
	inline MA1c_double TotalLat() const { return ma( &T::TotalLat ); }
	inline MA1c_double TotalLatGainJ() const { return ma( &T::TotalLatGainJ ); }
	inline MA1c_double TotalLatGainW() const { return ma( &T::TotalLatGainW ); }
	inline MA1c_double TotalLatLossJ() const { return ma( &T::TotalLatLossJ ); }
	inline MA1c_double TotalLatLossW() const { return ma( &T::TotalLatLossW ); }
	inline MA1c_double TotalMassFlowRate() const { return ma( &T::TotalMassFlowRate ); }
	inline MA1c_double TotalPowerRequest() const { return ma( &T::TotalPowerRequest ); }
	inline MA1c_double TotalSen() const { return ma( &T::TotalSen ); }
	inline MA1c_double TotalSenGainJ() const { return ma( &T::TotalSenGainJ ); }
	inline MA1c_double TotalSenGainW() const { return ma( &T::TotalSenGainW ); }
	inline MA1c_double TotalSenLossJ() const { return ma( &T::TotalSenLossJ ); }
	inline MA1c_double TotalSenLossW() const { return ma( &T::TotalSenLossW ); }
	inline MA1c_double TotalSurfArea() const { return ma( &T::TotalSurfArea ); }
	inline MA1c_double TotalThermalPowerRequest() const { return ma( &T::TotalThermalPowerRequest ); }
	inline MA1c_double TotalVolFlowRate() const { return ma( &T::TotalVolFlowRate ); }
	inline MA1c_double TotCompCapacity() const { return ma( &T::TotCompCapacity ); }
	inline MA1c_double TotCompCapacityHP() const { return ma( &T::TotCompCapacityHP ); }
	inline MA1c_double TotCompCapacityLP() const { return ma( &T::TotCompCapacityLP ); }
	inline MA1c_double TotCompCoolingEnergy() const { return ma( &T::TotCompCoolingEnergy ); }
	inline MA1c_double TotCompElecConsump() const { return ma( &T::TotCompElecConsump ); }
	inline MA1c_double TotCompElecConsumpTwoStage() const { return ma( &T::TotCompElecConsumpTwoStage ); }
	inline MA1c_double TotCompPower() const { return ma( &T::TotCompPower ); }
	inline MA1c_double TotCompPowerHP() const { return ma( &T::TotCompPowerHP ); }
	inline MA1c_double TotCompPowerLP() const { return ma( &T::TotCompPowerLP ); }
	inline MA1c_double TotCoolCap() const { return ma( &T::TotCoolCap ); }
	inline MA1c_double TotCoolEnergyRate() const { return ma( &T::TotCoolEnergyRate ); }
	inline MA1c_double TotCoolingToZoneEnergy() const { return ma( &T::TotCoolingToZoneEnergy ); }
	inline MA1c_double TotCoolingToZoneRate() const { return ma( &T::TotCoolingToZoneRate ); }
	inline MA1c_double TotHeatEnergyRate() const { return ma( &T::TotHeatEnergyRate ); }
	inline MA1c_double TotHiStageCompCapacity() const { return ma( &T::TotHiStageCompCapacity ); }
	inline MA1c_double TotHiStageCompCoolingEnergy() const { return ma( &T::TotHiStageCompCoolingEnergy ); }
	inline MA1c_double TotHiStageCompElecConsump() const { return ma( &T::TotHiStageCompElecConsump ); }
	inline MA1c_double TotHiStageCompPower() const { return ma( &T::TotHiStageCompPower ); }
	inline MA1c_double TotHtXferToZoneEnergy() const { return ma( &T::TotHtXferToZoneEnergy ); }
	inline MA1c_double TotHtXferToZoneRate() const { return ma( &T::TotHtXferToZoneRate ); }
	inline MA1c_double TotLatCoolingEnergy() const { return ma( &T::TotLatCoolingEnergy ); }
	inline MA1c_double TotLatCoolingEnergyRate() const { return ma( &T::TotLatCoolingEnergyRate ); }
	inline MA1c_double TotOccupants() const { return ma( &T::TotOccupants ); }
	inline MA1c_double TotSensCoolingEnergy() const { return ma( &T::TotSensCoolingEnergy ); }
	inline MA1c_double TotSensCoolingEnergyRate() const { return ma( &T::TotSensCoolingEnergyRate ); }
	inline MA1c_double TransmittedSolar() const { return ma( &T::TransmittedSolar ); }
	inline MA1c_double TransSolBeam() const { return ma( &T::TransSolBeam ); }
	inline MA1c_double TransSolDiff() const { return ma( &T::TransSolDiff ); }
	inline MA1c_double TransVisBeam() const { return ma( &T::TransVisBeam ); }
	inline MA1c_double TransVisDiff() const { return ma( &T::TransVisDiff ); }
	inline MA1c_double TSValue() const { return ma( &T::TSValue ); }
	inline MA1c_double Ujet() const { return ma( &T::Ujet ); }
	inline MA1c_double UnmetDemand() const { return ma( &T::UnmetDemand ); }
	inline MA1c_double UnmetEnergy() const { return ma( &T::UnmetEnergy ); }
	inline MA1c_double Urec() const { return ma( &T::Urec ); }
	inline MA1c_double UsedHVACCoil() const { return ma( &T::UsedHVACCoil ); }
	inline MA1c_double UsedWaterHeater() const { return ma( &T::UsedWaterHeater ); }
	inline MA1c_double UseMassFlowRate() const { return ma( &T::UseMassFlowRate ); }
	inline MA1c_double VentilFanElec() const { return ma( &T::VentilFanElec ); }
	inline MA1c_double VolFLOW() const { return ma( &T::VolFLOW ); }
	inline MA1c_double VolFLOW2() const { return ma( &T::VolFLOW2 ); }
	inline MA1c_double VolFlowRateCrntRho() const { return ma( &T::VolFlowRateCrntRho ); }
	inline MA1c_double VolFlowRateStdRho() const { return ma( &T::VolFlowRateStdRho ); }
	inline MA1c_double WarmEnvEnergy() const { return ma( &T::WarmEnvEnergy ); }
	inline MA1c_double WaterSup() const { return ma( &T::WaterSup ); }
	inline MA1c_double WetBulbTemp() const { return ma( &T::WetBulbTemp ); }
	inline MA1c_double WindModifier() const { return ma( &T::WindModifier ); }
	inline MA1c_double x() const { return ma( &T::x ); }
	inline MA1c_double y() const { return ma( &T::y ); }
	inline MA1c_double z() const { return ma( &T::z ); }
	inline MA1c_double ZoneHeatGainRate() const { return ma( &T::ZoneHeatGainRate ); }
	inline MA1c_double ZoneHumRatAtCoolPeak() const { return ma( &T::ZoneHumRatAtCoolPeak ); }
	inline MA1c_double ZoneHumRatAtHeatPeak() const { return ma( &T::ZoneHumRatAtHeatPeak ); }
	inline MA1c_double ZoneRetTempAtCoolPeak() const { return ma( &T::ZoneRetTempAtCoolPeak ); }
	inline MA1c_double ZoneRetTempAtHeatPeak() const { return ma( &T::ZoneRetTempAtHeatPeak ); }
	inline MA1c_double ZoneTempAtCoolPeak() const { return ma( &T::ZoneTempAtCoolPeak ); }
	inline MA1c_double ZoneTempAtHeatPeak() const { return ma( &T::ZoneTempAtHeatPeak ); }

	// double Members
	inline MA1_double ActualCondenserFanPower() { return ma( &T::ActualCondenserFanPower ); }
	inline MA1_double ActualEvapPumpPower() { return ma( &T::ActualEvapPumpPower ); }
	inline MA1_double ActualFanPower() { return ma( &T::ActualFanPower ); }
	inline MA1_double ActuatedValue() { return ma( &T::ActuatedValue ); }
	inline MA1_double AirDensity() { return ma( &T::AirDensity ); }
	inline MA1_double AirSup() { return ma( &T::AirSup ); }
	inline MA1_double AmbientZoneGain() { return ma( &T::AmbientZoneGain ); }
	inline MA1_double AncillACuseEnergy() { return ma( &T::AncillACuseEnergy ); }
	inline MA1_double AncillACuseRate() { return ma( &T::AncillACuseRate ); }
	inline MA1_double Area() { return ma( &T::Area ); }
	inline MA1_double AuxilHeat() { return ma( &T::AuxilHeat ); }
	inline MA1_double AvailCapacity() { return ma( &T::AvailCapacity ); }
	inline MA1_double AvailTemperature() { return ma( &T::AvailTemperature ); }
	inline MA1_double AverageCompressorCOP() { return ma( &T::AverageCompressorCOP ); }
	inline MA1_double Azimuth() { return ma( &T::Azimuth ); }
	inline MA1_double BalMassFlowRate() { return ma( &T::BalMassFlowRate ); }
	inline MA1_double BaseHeatPower() { return ma( &T::BaseHeatPower ); }
	inline MA1_double BmSolAbsdInsReveal() { return ma( &T::BmSolAbsdInsReveal ); }
	inline MA1_double BmSolAbsdInsRevealReport() { return ma( &T::BmSolAbsdInsRevealReport ); }
	inline MA1_double BmSolAbsdOutsReveal() { return ma( &T::BmSolAbsdOutsReveal ); }
	inline MA1_double BmSolRefldInsReveal() { return ma( &T::BmSolRefldInsReveal ); }
	inline MA1_double BmSolRefldInsRevealReport() { return ma( &T::BmSolRefldInsRevealReport ); }
	inline MA1_double BmSolRefldOutsRevealReport() { return ma( &T::BmSolRefldOutsRevealReport ); }
	inline MA1_double BmSolTransThruIntWinRep() { return ma( &T::BmSolTransThruIntWinRep ); }
	inline MA1_double BmSolTransThruIntWinRepEnergy() { return ma( &T::BmSolTransThruIntWinRepEnergy ); }
	inline MA1_double BulkTemperature() { return ma( &T::BulkTemperature ); }
	inline MA1_double BypassFrac() { return ma( &T::BypassFrac ); }
	inline MA1_double CoeffA0() { return ma( &T::CoeffA0 ); }
	inline MA1_double CoeffA1() { return ma( &T::CoeffA1 ); }
	inline MA1_double CoeffA10() { return ma( &T::CoeffA10 ); }
	inline MA1_double CoeffA11() { return ma( &T::CoeffA11 ); }
	inline MA1_double CoeffA12() { return ma( &T::CoeffA12 ); }
	inline MA1_double CoeffA13() { return ma( &T::CoeffA13 ); }
	inline MA1_double CoeffA14() { return ma( &T::CoeffA14 ); }
	inline MA1_double CoeffA15() { return ma( &T::CoeffA15 ); }
	inline MA1_double CoeffA16() { return ma( &T::CoeffA16 ); }
	inline MA1_double CoeffA17() { return ma( &T::CoeffA17 ); }
	inline MA1_double CoeffA18() { return ma( &T::CoeffA18 ); }
	inline MA1_double CoeffA19() { return ma( &T::CoeffA19 ); }
	inline MA1_double CoeffA2() { return ma( &T::CoeffA2 ); }
	inline MA1_double CoeffA20() { return ma( &T::CoeffA20 ); }
	inline MA1_double CoeffA21() { return ma( &T::CoeffA21 ); }
	inline MA1_double CoeffA22() { return ma( &T::CoeffA22 ); }
	inline MA1_double CoeffA23() { return ma( &T::CoeffA23 ); }
	inline MA1_double CoeffA24() { return ma( &T::CoeffA24 ); }
	inline MA1_double CoeffA25() { return ma( &T::CoeffA25 ); }
	inline MA1_double CoeffA26() { return ma( &T::CoeffA26 ); }
	inline MA1_double CoeffA3() { return ma( &T::CoeffA3 ); }
	inline MA1_double CoeffA4() { return ma( &T::CoeffA4 ); }
	inline MA1_double CoeffA5() { return ma( &T::CoeffA5 ); }
	inline MA1_double CoeffA6() { return ma( &T::CoeffA6 ); }
	inline MA1_double CoeffA7() { return ma( &T::CoeffA7 ); }
	inline MA1_double CoeffA8() { return ma( &T::CoeffA8 ); }
	inline MA1_double CoeffA9() { return ma( &T::CoeffA9 ); }
	inline MA1_double CoinCoolMassFlow() { return ma( &T::CoinCoolMassFlow ); }
	inline MA1_double CoinHeatMassFlow() { return ma( &T::CoinHeatMassFlow ); }
	inline MA1_double ColdMassFlowRate() { return ma( &T::ColdMassFlowRate ); }
	inline MA1_double ColdVolFlowRate() { return ma( &T::ColdVolFlowRate ); }
	inline MA1_double CompPowerLossFactor() { return ma( &T::CompPowerLossFactor ); }
	inline MA1_double CondEnergy() { return ma( &T::CondEnergy ); }
	inline MA1_double CondenserFanConsumption() { return ma( &T::CondenserFanConsumption ); }
	inline MA1_double CondLoad() { return ma( &T::CondLoad ); }
	inline MA1_double CondSen() { return ma( &T::CondSen ); }
	inline MA1_double CondSenGainJ() { return ma( &T::CondSenGainJ ); }
	inline MA1_double CondSenGainW() { return ma( &T::CondSenGainW ); }
	inline MA1_double CondSenLossJ() { return ma( &T::CondSenLossJ ); }
	inline MA1_double CondSenLossW() { return ma( &T::CondSenLossW ); }
	inline MA1_double CoolCap1() { return ma( &T::CoolCap1 ); }
	inline MA1_double CoolCap2() { return ma( &T::CoolCap2 ); }
	inline MA1_double CoolCap3() { return ma( &T::CoolCap3 ); }
	inline MA1_double CoolCap4() { return ma( &T::CoolCap4 ); }
	inline MA1_double CoolCap5() { return ma( &T::CoolCap5 ); }
	inline MA1_double CoolCycleTime() { return ma( &T::CoolCycleTime ); }
	inline MA1_double CoolingDemand() { return ma( &T::CoolingDemand ); }
	inline MA1_double CoolPower1() { return ma( &T::CoolPower1 ); }
	inline MA1_double CoolPower2() { return ma( &T::CoolPower2 ); }
	inline MA1_double CoolPower3() { return ma( &T::CoolPower3 ); }
	inline MA1_double CoolPower4() { return ma( &T::CoolPower4 ); }
	inline MA1_double CoolPower5() { return ma( &T::CoolPower5 ); }
	inline MA1_double CurrentMaxAvail() { return ma( &T::CurrentMaxAvail ); }
	inline MA1_double CurrentMinAvail() { return ma( &T::CurrentMinAvail ); }
	inline MA1_double DCElectProdRate() { return ma( &T::DCElectProdRate ); }
	inline MA1_double DCElectricityProd() { return ma( &T::DCElectricityProd ); }
	inline MA1_double DCpowerConditionLosses() { return ma( &T::DCpowerConditionLosses ); }
	inline MA1_double DecrementedEnergyStored() { return ma( &T::DecrementedEnergyStored ); }
	inline MA1_double DefEnergyCurveValue() { return ma( &T::DefEnergyCurveValue ); }
	inline MA1_double DefrostEnergy() { return ma( &T::DefrostEnergy ); }
	inline MA1_double DeltaPress() { return ma( &T::DeltaPress ); }
	inline MA1_double DeltaT() { return ma( &T::DeltaT ); }
	inline MA1_double DemandNotDispatched() { return ma( &T::DemandNotDispatched ); }
	inline MA1_double Density() { return ma( &T::Density ); }
	inline MA1_double DesCoolCoilInHumRat() { return ma( &T::DesCoolCoilInHumRat ); }
	inline MA1_double DesCoolCoilInTemp() { return ma( &T::DesCoolCoilInTemp ); }
	inline MA1_double DesCoolDens() { return ma( &T::DesCoolDens ); }
	inline MA1_double DesCoolLoad() { return ma( &T::DesCoolLoad ); }
	inline MA1_double DesCoolMassFlow() { return ma( &T::DesCoolMassFlow ); }
	inline MA1_double DesCoolVolFlow() { return ma( &T::DesCoolVolFlow ); }
	inline MA1_double DesHeatCoilInHumRat() { return ma( &T::DesHeatCoilInHumRat ); }
	inline MA1_double DesHeatCoilInTemp() { return ma( &T::DesHeatCoilInTemp ); }
	inline MA1_double DesHeatDens() { return ma( &T::DesHeatDens ); }
	inline MA1_double DesHeatLoad() { return ma( &T::DesHeatLoad ); }
	inline MA1_double DesHeatMassFlow() { return ma( &T::DesHeatMassFlow ); }
	inline MA1_double DesHeatVolFlow() { return ma( &T::DesHeatVolFlow ); }
	inline MA1_double DesignLevel() { return ma( &T::DesignLevel ); }
	inline MA1_double DesMainVolFlow() { return ma( &T::DesMainVolFlow ); }
	inline MA1_double DesVolFlowRate() { return ma( &T::DesVolFlowRate ); }
	inline MA1_double DiffLat() { return ma( &T::DiffLat ); }
	inline MA1_double DiffLatGainJ() { return ma( &T::DiffLatGainJ ); }
	inline MA1_double DiffLatGainW() { return ma( &T::DiffLatGainW ); }
	inline MA1_double DiffLatLossJ() { return ma( &T::DiffLatLossJ ); }
	inline MA1_double DiffLatLossW() { return ma( &T::DiffLatLossW ); }
	inline MA1_double DistPipeZoneHeatGain() { return ma( &T::DistPipeZoneHeatGain ); }
	inline MA1_double DividerQRadInAbs() { return ma( &T::DividerQRadInAbs ); }
	inline MA1_double DividerQRadOutAbs() { return ma( &T::DividerQRadOutAbs ); }
	inline MA1_double DividerTempSurfIn() { return ma( &T::DividerTempSurfIn ); }
	inline MA1_double DividerTempSurfInOld() { return ma( &T::DividerTempSurfInOld ); }
	inline MA1_double DividerTempSurfOut() { return ma( &T::DividerTempSurfOut ); }
	inline MA1_double DrainTemp() { return ma( &T::DrainTemp ); }
	inline MA1_double DrawnEnergy() { return ma( &T::DrawnEnergy ); }
	inline MA1_double DrawnPower() { return ma( &T::DrawnPower ); }
	inline MA1_double EEConvected() { return ma( &T::EEConvected ); }
	inline MA1_double EELatent() { return ma( &T::EELatent ); }
	inline MA1_double EELost() { return ma( &T::EELost ); }
	inline MA1_double EERadiated() { return ma( &T::EERadiated ); }
	inline MA1_double Efficiency() { return ma( &T::Efficiency ); }
	inline MA1_double EffInsSurfTemp() { return ma( &T::EffInsSurfTemp ); }
	inline MA1_double ElecAntiSweatConsumption() { return ma( &T::ElecAntiSweatConsumption ); }
	inline MA1_double ElecAntiSweatPower() { return ma( &T::ElecAntiSweatPower ); }
	inline MA1_double ElecConsumption() { return ma( &T::ElecConsumption ); }
	inline MA1_double ElecDefrostConsumption() { return ma( &T::ElecDefrostConsumption ); }
	inline MA1_double ElecDefrostPower() { return ma( &T::ElecDefrostPower ); }
	inline MA1_double ElecFanConsumption() { return ma( &T::ElecFanConsumption ); }
	inline MA1_double ElecFanPower() { return ma( &T::ElecFanPower ); }
	inline MA1_double ElecHeaterConsumption() { return ma( &T::ElecHeaterConsumption ); }
	inline MA1_double ElecHeaterPower() { return ma( &T::ElecHeaterPower ); }
	inline MA1_double ElecLightingConsumption() { return ma( &T::ElecLightingConsumption ); }
	inline MA1_double ElecLightingPower() { return ma( &T::ElecLightingPower ); }
	inline MA1_double ElecPower() { return ma( &T::ElecPower ); }
	inline MA1_double ElecStorage() { return ma( &T::ElecStorage ); }
	inline MA1_double ElectDemand() { return ma( &T::ElectDemand ); }
	inline MA1_double ElectEnergyinStorage() { return ma( &T::ElectEnergyinStorage ); }
	inline MA1_double ElectProdRate() { return ma( &T::ElectProdRate ); }
	inline MA1_double ElectricityProd() { return ma( &T::ElectricityProd ); }
	inline MA1_double Energy() { return ma( &T::Energy ); }
	inline MA1_double Enthalpy() { return ma( &T::Enthalpy ); }
	inline MA1_double EnthalpyLastTimestep() { return ma( &T::EnthalpyLastTimestep ); }
	inline MA1_double ERVMassFlowRate() { return ma( &T::ERVMassFlowRate ); }
	inline MA1_double EvapPumpConsumption() { return ma( &T::EvapPumpConsumption ); }
	inline MA1_double EvapWaterConsumpRate() { return ma( &T::EvapWaterConsumpRate ); }
	inline MA1_double EvapWaterConsumption() { return ma( &T::EvapWaterConsumption ); }
	inline MA1_double ExhaustHX() { return ma( &T::ExhaustHX ); }
	inline MA1_double ExhMassFlowRate() { return ma( &T::ExhMassFlowRate ); }
	inline MA1_double ExitTemp() { return ma( &T::ExitTemp ); }
	inline MA1_double ExternalEnergyRecovered() { return ma( &T::ExternalEnergyRecovered ); }
	inline MA1_double ExternalHeatRecoveredLoad() { return ma( &T::ExternalHeatRecoveredLoad ); }
	inline MA1_double ExtGrossWallArea() { return ma( &T::ExtGrossWallArea ); }
	inline MA1_double ExtNetWallArea() { return ma( &T::ExtNetWallArea ); }
	inline MA1_double ExtWindowArea() { return ma( &T::ExtWindowArea ); }
	inline MA1_double FanEff() { return ma( &T::FanEff ); }
	inline MA1_double FanElecEnergy() { return ma( &T::FanElecEnergy ); }
	inline MA1_double FCPM() { return ma( &T::FCPM ); }
	inline MA1_double FloorArea() { return ma( &T::FloorArea ); }
	inline MA1_double FLOW() { return ma( &T::FLOW ); }
	inline MA1_double FLOW2() { return ma( &T::FLOW2 ); }
	inline MA1_double FrameQRadInAbs() { return ma( &T::FrameQRadInAbs ); }
	inline MA1_double FrameQRadOutAbs() { return ma( &T::FrameQRadOutAbs ); }
	inline MA1_double FrameTempSurfIn() { return ma( &T::FrameTempSurfIn ); }
	inline MA1_double FrameTempSurfInOld() { return ma( &T::FrameTempSurfInOld ); }
	inline MA1_double FrameTempSurfOut() { return ma( &T::FrameTempSurfOut ); }
	inline MA1_double FuelEnergy() { return ma( &T::FuelEnergy ); }
	inline MA1_double GasCoolerEnergy() { return ma( &T::GasCoolerEnergy ); }
	inline MA1_double GasCoolerLoad() { return ma( &T::GasCoolerLoad ); }
	inline MA1_double GasPower() { return ma( &T::GasPower ); }
	inline MA1_double GCTime() { return ma( &T::GCTime ); }
	inline MA1_double Hc() { return ma( &T::Hc ); }
	inline MA1_double HeatCap() { return ma( &T::HeatCap ); }
	inline MA1_double HeatCap1() { return ma( &T::HeatCap1 ); }
	inline MA1_double HeatCap2() { return ma( &T::HeatCap2 ); }
	inline MA1_double HeatCap3() { return ma( &T::HeatCap3 ); }
	inline MA1_double HeatCap4() { return ma( &T::HeatCap4 ); }
	inline MA1_double HeatCap5() { return ma( &T::HeatCap5 ); }
	inline MA1_double HeatCycleTime() { return ma( &T::HeatCycleTime ); }
	inline MA1_double HeatGain() { return ma( &T::HeatGain ); }
	inline MA1_double HeatingDemand() { return ma( &T::HeatingDemand ); }
	inline MA1_double HeatingToZoneEnergy() { return ma( &T::HeatingToZoneEnergy ); }
	inline MA1_double HeatingToZoneRate() { return ma( &T::HeatingToZoneRate ); }
	inline MA1_double HeatLoss() { return ma( &T::HeatLoss ); }
	inline MA1_double HeatMixHumRat() { return ma( &T::HeatMixHumRat ); }
	inline MA1_double HeatMixTemp() { return ma( &T::HeatMixTemp ); }
	inline MA1_double HeatOutHumRat() { return ma( &T::HeatOutHumRat ); }
	inline MA1_double HeatOutTemp() { return ma( &T::HeatOutTemp ); }
	inline MA1_double HeatPower1() { return ma( &T::HeatPower1 ); }
	inline MA1_double HeatPower2() { return ma( &T::HeatPower2 ); }
	inline MA1_double HeatPower3() { return ma( &T::HeatPower3 ); }
	inline MA1_double HeatPower4() { return ma( &T::HeatPower4 ); }
	inline MA1_double HeatPower5() { return ma( &T::HeatPower5 ); }
	inline MA1_double HeatRetHumRat() { return ma( &T::HeatRetHumRat ); }
	inline MA1_double HeatRetTemp() { return ma( &T::HeatRetTemp ); }
	inline MA1_double HotDefrostCondCredit() { return ma( &T::HotDefrostCondCredit ); }
	inline MA1_double HotMassFlowRate() { return ma( &T::HotMassFlowRate ); }
	inline MA1_double HotVolFlowRate() { return ma( &T::HotVolFlowRate ); }
	inline MA1_double HumRat() { return ma( &T::HumRat ); }
	inline MA1_double HumRatMax() { return ma( &T::HumRatMax ); }
	inline MA1_double HumRatMin() { return ma( &T::HumRatMin ); }
	inline MA1_double HumRatSetPoint() { return ma( &T::HumRatSetPoint ); }
	inline MA1_double HWPower() { return ma( &T::HWPower ); }
	inline MA1_double In() { return ma( &T::In ); }
	inline MA1_double InfilAirChangeRate() { return ma( &T::InfilAirChangeRate ); }
	inline MA1_double InfilHeatGain() { return ma( &T::InfilHeatGain ); }
	inline MA1_double InfilHeatLoss() { return ma( &T::InfilHeatLoss ); }
	inline MA1_double InfilMass() { return ma( &T::InfilMass ); }
	inline MA1_double InfilVolume() { return ma( &T::InfilVolume ); }
	inline MA1_double InfMassFlowRate() { return ma( &T::InfMassFlowRate ); }
	inline MA1_double InitialVolume() { return ma( &T::InitialVolume ); }
	inline MA1_double InletNodeFlowrate() { return ma( &T::InletNodeFlowrate ); }
	inline MA1_double InletNodeTemperature() { return ma( &T::InletNodeTemperature ); }
	inline MA1_double InsRevealDiffIntoZone() { return ma( &T::InsRevealDiffIntoZone ); }
	inline MA1_double InsRevealDiffIntoZoneReport() { return ma( &T::InsRevealDiffIntoZoneReport ); }
	inline MA1_double InsRevealDiffOntoFrame() { return ma( &T::InsRevealDiffOntoFrame ); }
	inline MA1_double InsRevealDiffOntoFrameReport() { return ma( &T::InsRevealDiffOntoFrameReport ); }
	inline MA1_double InsRevealDiffOntoGlazing() { return ma( &T::InsRevealDiffOntoGlazing ); }
	inline MA1_double InsRevealDiffOntoGlazingReport() { return ma( &T::InsRevealDiffOntoGlazingReport ); }
	inline MA1_double InternalEnergyRecovered() { return ma( &T::InternalEnergyRecovered ); }
	inline MA1_double InternalHeatRecoveredLoad() { return ma( &T::InternalHeatRecoveredLoad ); }
	inline MA1_double IntMassFlowRate() { return ma( &T::IntMassFlowRate ); }
	inline MA1_double Inverter() { return ma( &T::Inverter ); }
	inline MA1_double IRfromParentZone() { return ma( &T::IRfromParentZone ); }
	inline MA1_double KgFrost() { return ma( &T::KgFrost ); }
	inline MA1_double KgFrostSaved() { return ma( &T::KgFrostSaved ); }
	inline MA1_double LaggedUsedHVACCoil() { return ma( &T::LaggedUsedHVACCoil ); }
	inline MA1_double LaggedUsedWaterHeater() { return ma( &T::LaggedUsedWaterHeater ); }
	inline MA1_double LastEventTime() { return ma( &T::LastEventTime ); }
	inline MA1_double LatCaseCreditToHVAC() { return ma( &T::LatCaseCreditToHVAC ); }
	inline MA1_double LatCaseCreditToZone() { return ma( &T::LatCaseCreditToZone ); }
	inline MA1_double LatCoolEnergyRate() { return ma( &T::LatCoolEnergyRate ); }
	inline MA1_double LatCoolingEnergy() { return ma( &T::LatCoolingEnergy ); }
	inline MA1_double LatCoolingEnergyRate() { return ma( &T::LatCoolingEnergyRate ); }
	inline MA1_double LatCoolingToZoneEnergy() { return ma( &T::LatCoolingToZoneEnergy ); }
	inline MA1_double LatCoolingToZoneRate() { return ma( &T::LatCoolingToZoneRate ); }
	inline MA1_double LatCreditEnergy() { return ma( &T::LatCreditEnergy ); }
	inline MA1_double LatCreditRate() { return ma( &T::LatCreditRate ); }
	inline MA1_double LatCreditToZoneEnergy() { return ma( &T::LatCreditToZoneEnergy ); }
	inline MA1_double LatCreditToZoneRate() { return ma( &T::LatCreditToZoneRate ); }
	inline MA1_double LatEnergyCurveValue() { return ma( &T::LatEnergyCurveValue ); }
	inline MA1_double LatentEnergy() { return ma( &T::LatentEnergy ); }
	inline MA1_double LatentRate() { return ma( &T::LatentRate ); }
	inline MA1_double LatentRateNoMultiplier() { return ma( &T::LatentRateNoMultiplier ); }
	inline MA1_double LatHeatEnergyRate() { return ma( &T::LatHeatEnergyRate ); }
	inline MA1_double LatHVACCredit() { return ma( &T::LatHVACCredit ); }
	inline MA1_double LatHVACCreditRate() { return ma( &T::LatHVACCreditRate ); }
	inline MA1_double LatKgPerS_ToZone() { return ma( &T::LatKgPerS_ToZone ); }
	inline MA1_double LatKgPerS_ToZoneRate() { return ma( &T::LatKgPerS_ToZoneRate ); }
	inline MA1_double LatZoneCredit() { return ma( &T::LatZoneCredit ); }
	inline MA1_double LatZoneCreditRate() { return ma( &T::LatZoneCreditRate ); }
	inline MA1_double LeakLat() { return ma( &T::LeakLat ); }
	inline MA1_double LeakLatGainJ() { return ma( &T::LeakLatGainJ ); }
	inline MA1_double LeakLatGainW() { return ma( &T::LeakLatGainW ); }
	inline MA1_double LeakLatLossJ() { return ma( &T::LeakLatLossJ ); }
	inline MA1_double LeakLatLossW() { return ma( &T::LeakLatLossW ); }
	inline MA1_double LeakSen() { return ma( &T::LeakSen ); }
	inline MA1_double LeakSenGainJ() { return ma( &T::LeakSenGainJ ); }
	inline MA1_double LeakSenGainW() { return ma( &T::LeakSenGainW ); }
	inline MA1_double LeakSenLossJ() { return ma( &T::LeakSenLossJ ); }
	inline MA1_double LeakSenLossW() { return ma( &T::LeakSenLossW ); }
	inline MA1_double LineSubTotal() { return ma( &T::LineSubTotal ); }
	inline MA1_double LoadSideInletTemp() { return ma( &T::LoadSideInletTemp ); }
	inline MA1_double LoadSideMassFlowRate() { return ma( &T::LoadSideMassFlowRate ); }
	inline MA1_double LoadSideOutletTemp() { return ma( &T::LoadSideOutletTemp ); }
	inline MA1_double LtsPower() { return ma( &T::LtsPower ); }
	inline MA1_double MassFlowAtCoolPeak() { return ma( &T::MassFlowAtCoolPeak ); }
	inline MA1_double MassFlowFromLower() { return ma( &T::MassFlowFromLower ); }
	inline MA1_double MassFlowFromUpper() { return ma( &T::MassFlowFromUpper ); }
	inline MA1_double MassFlowRate() { return ma( &T::MassFlowRate ); }
	inline MA1_double MassFlowRateMax() { return ma( &T::MassFlowRateMax ); }
	inline MA1_double MassFlowRateMaxAvail() { return ma( &T::MassFlowRateMaxAvail ); }
	inline MA1_double MassFlowRateMin() { return ma( &T::MassFlowRateMin ); }
	inline MA1_double MassFlowRateMinAvail() { return ma( &T::MassFlowRateMinAvail ); }
	inline MA1_double MassFlowRateSetPoint() { return ma( &T::MassFlowRateSetPoint ); }
	inline MA1_double MassFlowToLower() { return ma( &T::MassFlowToLower ); }
	inline MA1_double MassFlowToUpper() { return ma( &T::MassFlowToUpper ); }
	inline MA1_double MaxAirFlowRate() { return ma( &T::MaxAirFlowRate ); }
	inline MA1_double MaxAirMassFlowRate() { return ma( &T::MaxAirMassFlowRate ); }
	inline MA1_double maximum() { return ma( &T::maximum ); }
	inline MA1_double MaxProcAirInHumRat() { return ma( &T::MaxProcAirInHumRat ); }
	inline MA1_double MaxProcAirInTemp() { return ma( &T::MaxProcAirInTemp ); }
	inline MA1_double MeanAirTemp() { return ma( &T::MeanAirTemp ); }
	inline MA1_double Med() { return ma( &T::Med ); }
	inline MA1_double minimum() { return ma( &T::minimum ); }
	inline MA1_double MinProcAirInHumRat() { return ma( &T::MinProcAirInHumRat ); }
	inline MA1_double MinProcAirInTemp() { return ma( &T::MinProcAirInTemp ); }
	inline MA1_double MixedTemp() { return ma( &T::MixedTemp ); }
	inline MA1_double MixHumRatAtCoolPeak() { return ma( &T::MixHumRatAtCoolPeak ); }
	inline MA1_double MixMass() { return ma( &T::MixMass ); }
	inline MA1_double MixTempAtCoolPeak() { return ma( &T::MixTempAtCoolPeak ); }
	inline MA1_double MixVolume() { return ma( &T::MixVolume ); }
	inline MA1_double MotEff() { return ma( &T::MotEff ); }
	inline MA1_double MotInAirFrac() { return ma( &T::MotInAirFrac ); }
	inline MA1_double MultiZoneInfiLatGainJ() { return ma( &T::MultiZoneInfiLatGainJ ); }
	inline MA1_double MultiZoneInfiLatGainW() { return ma( &T::MultiZoneInfiLatGainW ); }
	inline MA1_double MultiZoneInfiLatLossJ() { return ma( &T::MultiZoneInfiLatLossJ ); }
	inline MA1_double MultiZoneInfiLatLossW() { return ma( &T::MultiZoneInfiLatLossW ); }
	inline MA1_double MultiZoneInfiSenGainJ() { return ma( &T::MultiZoneInfiSenGainJ ); }
	inline MA1_double MultiZoneInfiSenGainW() { return ma( &T::MultiZoneInfiSenGainW ); }
	inline MA1_double MultiZoneInfiSenLossJ() { return ma( &T::MultiZoneInfiSenLossJ ); }
	inline MA1_double MultiZoneInfiSenLossW() { return ma( &T::MultiZoneInfiSenLossW ); }
	inline MA1_double MultiZoneLat() { return ma( &T::MultiZoneLat ); }
	inline MA1_double MultiZoneMixLatGainJ() { return ma( &T::MultiZoneMixLatGainJ ); }
	inline MA1_double MultiZoneMixLatGainW() { return ma( &T::MultiZoneMixLatGainW ); }
	inline MA1_double MultiZoneMixLatLossJ() { return ma( &T::MultiZoneMixLatLossJ ); }
	inline MA1_double MultiZoneMixLatLossW() { return ma( &T::MultiZoneMixLatLossW ); }
	inline MA1_double MultiZoneMixSenGainJ() { return ma( &T::MultiZoneMixSenGainJ ); }
	inline MA1_double MultiZoneMixSenGainW() { return ma( &T::MultiZoneMixSenGainW ); }
	inline MA1_double MultiZoneMixSenLossJ() { return ma( &T::MultiZoneMixSenLossJ ); }
	inline MA1_double MultiZoneMixSenLossW() { return ma( &T::MultiZoneMixSenLossW ); }
	inline MA1_double MultiZoneSen() { return ma( &T::MultiZoneSen ); }
	inline MA1_double MyLoad() { return ma( &T::MyLoad ); }
	inline MA1_double NatMassFlowRate() { return ma( &T::NatMassFlowRate ); }
	inline MA1_double NetHeatRejectEnergy() { return ma( &T::NetHeatRejectEnergy ); }
	inline MA1_double NetHeatRejectLoad() { return ma( &T::NetHeatRejectLoad ); }
	inline MA1_double NewTemp() { return ma( &T::NewTemp ); }
	inline MA1_double NOFOCC() { return ma( &T::NOFOCC ); }
	inline MA1_double NominalCapacity() { return ma( &T::NominalCapacity ); }
	inline MA1_double NonCoinCoolMassFlow() { return ma( &T::NonCoinCoolMassFlow ); }
	inline MA1_double NonCoinHeatMassFlow() { return ma( &T::NonCoinHeatMassFlow ); }
	inline MA1_double OffCycParaFuelEnergy() { return ma( &T::OffCycParaFuelEnergy ); }
	inline MA1_double OnCycParaFuelEnergy() { return ma( &T::OnCycParaFuelEnergy ); }
	inline MA1_double OperativeTemp() { return ma( &T::OperativeTemp ); }
	inline MA1_double Out() { return ma( &T::Out ); }
	inline MA1_double OutDryBulbTemp() { return ma( &T::OutDryBulbTemp ); }
	inline MA1_double OutHumRatAtCoolPeak() { return ma( &T::OutHumRatAtCoolPeak ); }
	inline MA1_double OutHumRatAtHeatPeak() { return ma( &T::OutHumRatAtHeatPeak ); }
	inline MA1_double OutletNodeFlowrate() { return ma( &T::OutletNodeFlowrate ); }
	inline MA1_double OutletNodeTemperature() { return ma( &T::OutletNodeTemperature ); }
	inline MA1_double OutRoom() { return ma( &T::OutRoom ); }
	inline MA1_double OutsRevealDiffOntoFrame() { return ma( &T::OutsRevealDiffOntoFrame ); }
	inline MA1_double OutsRevealDiffOntoGlazing() { return ma( &T::OutsRevealDiffOntoGlazing ); }
	inline MA1_double OutTempAtCoolPeak() { return ma( &T::OutTempAtCoolPeak ); }
	inline MA1_double OutTempAtHeatPeak() { return ma( &T::OutTempAtHeatPeak ); }
	inline MA1_double OutWetBulbTemp() { return ma( &T::OutWetBulbTemp ); }
	inline MA1_double PastBulkTemperature() { return ma( &T::PastBulkTemperature ); }
	inline MA1_double PelFromStorage() { return ma( &T::PelFromStorage ); }
	inline MA1_double PelIntoStorage() { return ma( &T::PelIntoStorage ); }
	inline MA1_double PelNeedFromStorage() { return ma( &T::PelNeedFromStorage ); }
	inline MA1_double PipeHeatLoad() { return ma( &T::PipeHeatLoad ); }
	inline MA1_double PipeHeatLoadLT() { return ma( &T::PipeHeatLoadLT ); }
	inline MA1_double PipeHeatLoadMT() { return ma( &T::PipeHeatLoadMT ); }
	inline MA1_double Power() { return ma( &T::Power ); }
	inline MA1_double PreheatCap() { return ma( &T::PreheatCap ); }
	inline MA1_double Press() { return ma( &T::Press ); }
	inline MA1_double PumpElecEnergyTotal() { return ma( &T::PumpElecEnergyTotal ); }
	inline MA1_double PumpHeatToFluid() { return ma( &T::PumpHeatToFluid ); }
	inline MA1_double PumpPowerTotal() { return ma( &T::PumpPowerTotal ); }
	inline MA1_double QBBCON() { return ma( &T::QBBCON ); }
	inline MA1_double QBBRAD() { return ma( &T::QBBRAD ); }
	inline MA1_double QconvZone() { return ma( &T::QconvZone ); }
	inline MA1_double QdotConvZone() { return ma( &T::QdotConvZone ); }
	inline MA1_double QdotRadZone() { return ma( &T::QdotRadZone ); }
	inline MA1_double QEECON() { return ma( &T::QEECON ); }
	inline MA1_double QEELAT() { return ma( &T::QEELAT ); }
	inline MA1_double QEELost() { return ma( &T::QEELost ); }
	inline MA1_double QEERAD() { return ma( &T::QEERAD ); }
	inline MA1_double QGECON() { return ma( &T::QGECON ); }
	inline MA1_double QGELAT() { return ma( &T::QGELAT ); }
	inline MA1_double QGELost() { return ma( &T::QGELost ); }
	inline MA1_double QGERAD() { return ma( &T::QGERAD ); }
	inline MA1_double QHWCON() { return ma( &T::QHWCON ); }
	inline MA1_double QHWLAT() { return ma( &T::QHWLAT ); }
	inline MA1_double QHWLost() { return ma( &T::QHWLost ); }
	inline MA1_double QHWRAD() { return ma( &T::QHWRAD ); }
	inline MA1_double QLoad() { return ma( &T::QLoad ); }
	inline MA1_double QLoadEnergy() { return ma( &T::QLoadEnergy ); }
	inline MA1_double QLTCON() { return ma( &T::QLTCON ); }
	inline MA1_double QLTCRA() { return ma( &T::QLTCRA ); }
	inline MA1_double QLTRAD() { return ma( &T::QLTRAD ); }
	inline MA1_double QLTSW() { return ma( &T::QLTSW ); }
	inline MA1_double QLTTOT() { return ma( &T::QLTTOT ); }
	inline MA1_double QOCCON() { return ma( &T::QOCCON ); }
	inline MA1_double QOCLAT() { return ma( &T::QOCLAT ); }
	inline MA1_double QOCRAD() { return ma( &T::QOCRAD ); }
	inline MA1_double QOCSEN() { return ma( &T::QOCSEN ); }
	inline MA1_double QOCTOT() { return ma( &T::QOCTOT ); }
	inline MA1_double QOECON() { return ma( &T::QOECON ); }
	inline MA1_double QOELAT() { return ma( &T::QOELAT ); }
	inline MA1_double QOELost() { return ma( &T::QOELost ); }
	inline MA1_double QOERAD() { return ma( &T::QOERAD ); }
	inline MA1_double QradZone() { return ma( &T::QradZone ); }
	inline MA1_double Qrec() { return ma( &T::Qrec ); }
	inline MA1_double QSECON() { return ma( &T::QSECON ); }
	inline MA1_double QSELAT() { return ma( &T::QSELAT ); }
	inline MA1_double QSELost() { return ma( &T::QSELost ); }
	inline MA1_double QSERAD() { return ma( &T::QSERAD ); }
	inline MA1_double QskinLoss() { return ma( &T::QskinLoss ); }
	inline MA1_double QSource() { return ma( &T::QSource ); }
	inline MA1_double QSourceEnergy() { return ma( &T::QSourceEnergy ); }
	inline MA1_double Quality() { return ma( &T::Quality ); }
	inline MA1_double RackCompressorPower() { return ma( &T::RackCompressorPower ); }
	inline MA1_double RackElecConsumption() { return ma( &T::RackElecConsumption ); }
	inline MA1_double RadCoolingEnergy() { return ma( &T::RadCoolingEnergy ); }
	inline MA1_double RadCoolingPower() { return ma( &T::RadCoolingPower ); }
	inline MA1_double RadHeatingEnergy() { return ma( &T::RadHeatingEnergy ); }
	inline MA1_double RadHeatingPower() { return ma( &T::RadHeatingPower ); }
	inline MA1_double RatedCapCool() { return ma( &T::RatedCapCool ); }
	inline MA1_double RatedCapHeat() { return ma( &T::RatedCapHeat ); }
	inline MA1_double RatedLoadVolFlowCool() { return ma( &T::RatedLoadVolFlowCool ); }
	inline MA1_double RatedLoadVolFlowHeat() { return ma( &T::RatedLoadVolFlowHeat ); }
	inline MA1_double RatedPowerCool() { return ma( &T::RatedPowerCool ); }
	inline MA1_double RatedPowerHeat() { return ma( &T::RatedPowerHeat ); }
	inline MA1_double RatedSourceVolFlowCool() { return ma( &T::RatedSourceVolFlowCool ); }
	inline MA1_double RatedSourceVolFlowHeat() { return ma( &T::RatedSourceVolFlowHeat ); }
	inline MA1_double ReceiverZoneHeatGain() { return ma( &T::ReceiverZoneHeatGain ); }
	inline MA1_double RelHumidity() { return ma( &T::RelHumidity ); }
	inline MA1_double Report() { return ma( &T::Report ); }
	inline MA1_double ReportEnthalpy() { return ma( &T::ReportEnthalpy ); }
	inline MA1_double ReportH20RemovedKgPerS_FromZoneRate() { return ma( &T::ReportH20RemovedKgPerS_FromZoneRate ); }
	inline MA1_double ReportHeatingCreditEnergy() { return ma( &T::ReportHeatingCreditEnergy ); }
	inline MA1_double ReportHeatingCreditRate() { return ma( &T::ReportHeatingCreditRate ); }
	inline MA1_double ReportLatCreditToZoneEnergy() { return ma( &T::ReportLatCreditToZoneEnergy ); }
	inline MA1_double ReportLatCreditToZoneRate() { return ma( &T::ReportLatCreditToZoneRate ); }
	inline MA1_double ReportSenCoolingToZoneEnergy() { return ma( &T::ReportSenCoolingToZoneEnergy ); }
	inline MA1_double ReportSenCoolingToZoneRate() { return ma( &T::ReportSenCoolingToZoneRate ); }
	inline MA1_double ReportSensCoolCreditEnergy() { return ma( &T::ReportSensCoolCreditEnergy ); }
	inline MA1_double ReportSensCoolCreditRate() { return ma( &T::ReportSensCoolCreditRate ); }
	inline MA1_double ReportTotalCoolCreditEnergy() { return ma( &T::ReportTotalCoolCreditEnergy ); }
	inline MA1_double ReportTotalCoolCreditRate() { return ma( &T::ReportTotalCoolCreditRate ); }
	inline MA1_double ReportTotCoolingToZoneEnergy() { return ma( &T::ReportTotCoolingToZoneEnergy ); }
	inline MA1_double ReportTotCoolingToZoneRate() { return ma( &T::ReportTotCoolingToZoneRate ); }
	inline MA1_double ReqSupplyFrac() { return ma( &T::ReqSupplyFrac ); }
	inline MA1_double RetHumRatAtCoolPeak() { return ma( &T::RetHumRatAtCoolPeak ); }
	inline MA1_double RetTempAtCoolPeak() { return ma( &T::RetTempAtCoolPeak ); }
	inline MA1_double SavedTemp() { return ma( &T::SavedTemp ); }
	inline MA1_double SenCaseCreditToHVAC() { return ma( &T::SenCaseCreditToHVAC ); }
	inline MA1_double SenCaseCreditToZone() { return ma( &T::SenCaseCreditToZone ); }
	inline MA1_double SenCaseCreditToZoneEnergy() { return ma( &T::SenCaseCreditToZoneEnergy ); }
	inline MA1_double SenCoolingToZoneEnergy() { return ma( &T::SenCoolingToZoneEnergy ); }
	inline MA1_double SenCoolingToZoneRate() { return ma( &T::SenCoolingToZoneRate ); }
	inline MA1_double SenCreditToZoneEnergy() { return ma( &T::SenCreditToZoneEnergy ); }
	inline MA1_double SenCreditToZoneRate() { return ma( &T::SenCreditToZoneRate ); }
	inline MA1_double SensCoolCap() { return ma( &T::SensCoolCap ); }
	inline MA1_double SensCoolEnergyRate() { return ma( &T::SensCoolEnergyRate ); }
	inline MA1_double SensCoolingEnergy() { return ma( &T::SensCoolingEnergy ); }
	inline MA1_double SensCoolingEnergyRate() { return ma( &T::SensCoolingEnergyRate ); }
	inline MA1_double SensCreditRate() { return ma( &T::SensCreditRate ); }
	inline MA1_double SensHeatEnergyRate() { return ma( &T::SensHeatEnergyRate ); }
	inline MA1_double SensHeatRatio() { return ma( &T::SensHeatRatio ); }
	inline MA1_double SensHVACCreditCool() { return ma( &T::SensHVACCreditCool ); }
	inline MA1_double SensHVACCreditCoolRate() { return ma( &T::SensHVACCreditCoolRate ); }
	inline MA1_double SensHVACCreditHeat() { return ma( &T::SensHVACCreditHeat ); }
	inline MA1_double SensHVACCreditHeatRate() { return ma( &T::SensHVACCreditHeatRate ); }
	inline MA1_double SensHVACCreditRate() { return ma( &T::SensHVACCreditRate ); }
	inline MA1_double SensibleEnergy() { return ma( &T::SensibleEnergy ); }
	inline MA1_double SensibleRate() { return ma( &T::SensibleRate ); }
	inline MA1_double SensibleRateNoMultiplier() { return ma( &T::SensibleRateNoMultiplier ); }
	inline MA1_double SensZoneCreditCool() { return ma( &T::SensZoneCreditCool ); }
	inline MA1_double SensZoneCreditCoolRate() { return ma( &T::SensZoneCreditCoolRate ); }
	inline MA1_double SensZoneCreditHeat() { return ma( &T::SensZoneCreditHeat ); }
	inline MA1_double SensZoneCreditHeatRate() { return ma( &T::SensZoneCreditHeatRate ); }
	inline MA1_double SensZoneCreditRate() { return ma( &T::SensZoneCreditRate ); }
	inline MA1_double SMMaxVal() { return ma( &T::SMMaxVal ); }
	inline MA1_double SMMinVal() { return ma( &T::SMMinVal ); }
	inline MA1_double SMValue() { return ma( &T::SMValue ); }
	inline MA1_double SourceMassFlowRate() { return ma( &T::SourceMassFlowRate ); }
	inline MA1_double SourceSideInletTemp() { return ma( &T::SourceSideInletTemp ); }
	inline MA1_double SourceSideMassFlowRate() { return ma( &T::SourceSideMassFlowRate ); }
	inline MA1_double SourceSideOutletTemp() { return ma( &T::SourceSideOutletTemp ); }
	inline MA1_double StackCooler() { return ma( &T::StackCooler ); }
	inline MA1_double StartingEnergyStored() { return ma( &T::StartingEnergyStored ); }
	inline MA1_double SteamPower() { return ma( &T::SteamPower ); }
	inline MA1_double StockingEnergy() { return ma( &T::StockingEnergy ); }
	inline MA1_double StoredEnergy() { return ma( &T::StoredEnergy ); }
	inline MA1_double StoredPower() { return ma( &T::StoredPower ); }
	inline MA1_double sum() { return ma( &T::sum ); }
	inline MA1_double sum2() { return ma( &T::sum2 ); }
	inline MA1_double SumMCp() { return ma( &T::SumMCp ); }
	inline MA1_double SumMCpT() { return ma( &T::SumMCpT ); }
	inline MA1_double SumMechSCBenefit() { return ma( &T::SumMechSCBenefit ); }
	inline MA1_double SumMHr() { return ma( &T::SumMHr ); }
	inline MA1_double SumMHrCO() { return ma( &T::SumMHrCO ); }
	inline MA1_double SumMHrGC() { return ma( &T::SumMHrGC ); }
	inline MA1_double SumMHrW() { return ma( &T::SumMHrW ); }
	inline MA1_double SumMMCp() { return ma( &T::SumMMCp ); }
	inline MA1_double SumMMCpT() { return ma( &T::SumMMCpT ); }
	inline MA1_double SumMMHr() { return ma( &T::SumMMHr ); }
	inline MA1_double SumMMHrCO() { return ma( &T::SumMMHrCO ); }
	inline MA1_double SumMMHrGC() { return ma( &T::SumMMHrGC ); }
	inline MA1_double SumMMHrW() { return ma( &T::SumMMHrW ); }
	inline MA1_double SumSecondaryLoopLoad() { return ma( &T::SumSecondaryLoopLoad ); }
	inline MA1_double TadjacentAir() { return ma( &T::TadjacentAir ); }
	inline MA1_double Temp() { return ma( &T::Temp ); }
	inline MA1_double TempAvg() { return ma( &T::TempAvg ); }
	inline MA1_double TempLastTimestep() { return ma( &T::TempLastTimestep ); }
	inline MA1_double TempMax() { return ma( &T::TempMax ); }
	inline MA1_double TempMin() { return ma( &T::TempMin ); }
	inline MA1_double tempp1() { return ma( &T::tempp1 ); }
	inline MA1_double TempSetPoint() { return ma( &T::TempSetPoint ); }
	inline MA1_double TempSetPointHi() { return ma( &T::TempSetPointHi ); }
	inline MA1_double TempSetPointLo() { return ma( &T::TempSetPointLo ); }
	inline MA1_double TempSum() { return ma( &T::TempSum ); }
	inline MA1_double ThermalProd() { return ma( &T::ThermalProd ); }
	inline MA1_double ThermalProdRate() { return ma( &T::ThermalProdRate ); }
	inline MA1_double ThermLossEnergy() { return ma( &T::ThermLossEnergy ); }
	inline MA1_double ThermLossRate() { return ma( &T::ThermLossRate ); }
	inline MA1_double ThicknessPerpend() { return ma( &T::ThicknessPerpend ); }
	inline MA1_double ThisTimeStepStateOfCharge() { return ma( &T::ThisTimeStepStateOfCharge ); }
	inline MA1_double ThisTimeStepVolume() { return ma( &T::ThisTimeStepVolume ); }
	inline MA1_double Tilt() { return ma( &T::Tilt ); }
	inline MA1_double TimeElapsed() { return ma( &T::TimeElapsed ); }
	inline MA1_double TotalCO2() { return ma( &T::TotalCO2 ); }
	inline MA1_double TotalCondDefrostCredit() { return ma( &T::TotalCondDefrostCredit ); }
	inline MA1_double TotalCoolingEnergy() { return ma( &T::TotalCoolingEnergy ); }
	inline MA1_double TotalCoolingLoad() { return ma( &T::TotalCoolingLoad ); }
	inline MA1_double TotalCoolingLoadLT() { return ma( &T::TotalCoolingLoadLT ); }
	inline MA1_double TotalCoolingLoadMT() { return ma( &T::TotalCoolingLoadMT ); }
	inline MA1_double TotalElecConsumption() { return ma( &T::TotalElecConsumption ); }
	inline MA1_double TotalElecPower() { return ma( &T::TotalElecPower ); }
	inline MA1_double TotalGC() { return ma( &T::TotalGC ); }
	inline MA1_double TotalHeatRecoveredEnergy() { return ma( &T::TotalHeatRecoveredEnergy ); }
	inline MA1_double TotalHeatRecoveredLoad() { return ma( &T::TotalHeatRecoveredLoad ); }
	inline MA1_double TotalLat() { return ma( &T::TotalLat ); }
	inline MA1_double TotalLatGainJ() { return ma( &T::TotalLatGainJ ); }
	inline MA1_double TotalLatGainW() { return ma( &T::TotalLatGainW ); }
	inline MA1_double TotalLatLossJ() { return ma( &T::TotalLatLossJ ); }
	inline MA1_double TotalLatLossW() { return ma( &T::TotalLatLossW ); }
	inline MA1_double TotalMassFlowRate() { return ma( &T::TotalMassFlowRate ); }
	inline MA1_double TotalPowerRequest() { return ma( &T::TotalPowerRequest ); }
	inline MA1_double TotalSen() { return ma( &T::TotalSen ); }
	inline MA1_double TotalSenGainJ() { return ma( &T::TotalSenGainJ ); }
	inline MA1_double TotalSenGainW() { return ma( &T::TotalSenGainW ); }
	inline MA1_double TotalSenLossJ() { return ma( &T::TotalSenLossJ ); }
	inline MA1_double TotalSenLossW() { return ma( &T::TotalSenLossW ); }
	inline MA1_double TotalSurfArea() { return ma( &T::TotalSurfArea ); }
	inline MA1_double TotalThermalPowerRequest() { return ma( &T::TotalThermalPowerRequest ); }
	inline MA1_double TotalVolFlowRate() { return ma( &T::TotalVolFlowRate ); }
	inline MA1_double TotCompCapacity() { return ma( &T::TotCompCapacity ); }
	inline MA1_double TotCompCapacityHP() { return ma( &T::TotCompCapacityHP ); }
	inline MA1_double TotCompCapacityLP() { return ma( &T::TotCompCapacityLP ); }
	inline MA1_double TotCompCoolingEnergy() { return ma( &T::TotCompCoolingEnergy ); }
	inline MA1_double TotCompElecConsump() { return ma( &T::TotCompElecConsump ); }
	inline MA1_double TotCompElecConsumpTwoStage() { return ma( &T::TotCompElecConsumpTwoStage ); }
	inline MA1_double TotCompPower() { return ma( &T::TotCompPower ); }
	inline MA1_double TotCompPowerHP() { return ma( &T::TotCompPowerHP ); }
	inline MA1_double TotCompPowerLP() { return ma( &T::TotCompPowerLP ); }
	inline MA1_double TotCoolCap() { return ma( &T::TotCoolCap ); }
	inline MA1_double TotCoolEnergyRate() { return ma( &T::TotCoolEnergyRate ); }
	inline MA1_double TotCoolingToZoneEnergy() { return ma( &T::TotCoolingToZoneEnergy ); }
	inline MA1_double TotCoolingToZoneRate() { return ma( &T::TotCoolingToZoneRate ); }
	inline MA1_double TotHeatEnergyRate() { return ma( &T::TotHeatEnergyRate ); }
	inline MA1_double TotHiStageCompCapacity() { return ma( &T::TotHiStageCompCapacity ); }
	inline MA1_double TotHiStageCompCoolingEnergy() { return ma( &T::TotHiStageCompCoolingEnergy ); }
	inline MA1_double TotHiStageCompElecConsump() { return ma( &T::TotHiStageCompElecConsump ); }
	inline MA1_double TotHiStageCompPower() { return ma( &T::TotHiStageCompPower ); }
	inline MA1_double TotHtXferToZoneEnergy() { return ma( &T::TotHtXferToZoneEnergy ); }
	inline MA1_double TotHtXferToZoneRate() { return ma( &T::TotHtXferToZoneRate ); }
	inline MA1_double TotLatCoolingEnergy() { return ma( &T::TotLatCoolingEnergy ); }
	inline MA1_double TotLatCoolingEnergyRate() { return ma( &T::TotLatCoolingEnergyRate ); }
	inline MA1_double TotOccupants() { return ma( &T::TotOccupants ); }
	inline MA1_double TotSensCoolingEnergy() { return ma( &T::TotSensCoolingEnergy ); }
	inline MA1_double TotSensCoolingEnergyRate() { return ma( &T::TotSensCoolingEnergyRate ); }
	inline MA1_double TransmittedSolar() { return ma( &T::TransmittedSolar ); }
	inline MA1_double TransSolBeam() { return ma( &T::TransSolBeam ); }
	inline MA1_double TransSolDiff() { return ma( &T::TransSolDiff ); }
	inline MA1_double TransVisBeam() { return ma( &T::TransVisBeam ); }
	inline MA1_double TransVisDiff() { return ma( &T::TransVisDiff ); }
	inline MA1_double TSValue() { return ma( &T::TSValue ); }
	inline MA1_double Ujet() { return ma( &T::Ujet ); }
	inline MA1_double UnmetDemand() { return ma( &T::UnmetDemand ); }
	inline MA1_double UnmetEnergy() { return ma( &T::UnmetEnergy ); }
	inline MA1_double Urec() { return ma( &T::Urec ); }
	inline MA1_double UsedHVACCoil() { return ma( &T::UsedHVACCoil ); }
	inline MA1_double UsedWaterHeater() { return ma( &T::UsedWaterHeater ); }
	inline MA1_double UseMassFlowRate() { return ma( &T::UseMassFlowRate ); }
	inline MA1_double VentilFanElec() { return ma( &T::VentilFanElec ); }
	inline MA1_double VolFLOW() { return ma( &T::VolFLOW ); }
	inline MA1_double VolFLOW2() { return ma( &T::VolFLOW2 ); }
	inline MA1_double VolFlowRateCrntRho() { return ma( &T::VolFlowRateCrntRho ); }
	inline MA1_double VolFlowRateStdRho() { return ma( &T::VolFlowRateStdRho ); }
	inline MA1_double WarmEnvEnergy() { return ma( &T::WarmEnvEnergy ); }
	inline MA1_double WaterSup() { return ma( &T::WaterSup ); }
	inline MA1_double WetBulbTemp() { return ma( &T::WetBulbTemp ); }
	inline MA1_double WindModifier() { return ma( &T::WindModifier ); }
	inline MA1_double x() { return ma( &T::x ); }
	inline MA1_double y() { return ma( &T::y ); }
	inline MA1_double z() { return ma( &T::z ); }
	inline MA1_double ZoneHeatGainRate() { return ma( &T::ZoneHeatGainRate ); }
	inline MA1_double ZoneHumRatAtCoolPeak() { return ma( &T::ZoneHumRatAtCoolPeak ); }
	inline MA1_double ZoneHumRatAtHeatPeak() { return ma( &T::ZoneHumRatAtHeatPeak ); }
	inline MA1_double ZoneRetTempAtCoolPeak() { return ma( &T::ZoneRetTempAtCoolPeak ); }
	inline MA1_double ZoneRetTempAtHeatPeak() { return ma( &T::ZoneRetTempAtHeatPeak ); }
	inline MA1_double ZoneTempAtCoolPeak() { return ma( &T::ZoneTempAtCoolPeak ); }
	inline MA1_double ZoneTempAtHeatPeak() { return ma( &T::ZoneTempAtHeatPeak ); }

	// string const Members
	inline MA1c_string AirLoopName() const { return ma( &T::AirLoopName ); }
	inline MA1c_string AirPriLoopName() const { return ma( &T::AirPriLoopName ); }
	inline MA1c_string AssignedLoopName() const { return ma( &T::AssignedLoopName ); }
	inline MA1c_string CompName() const { return ma( &T::CompName ); }
	inline MA1c_string ComponentListName() const { return ma( &T::ComponentListName ); }
	inline MA1c_string CompType() const { return ma( &T::CompType ); }
	inline MA1c_string computeName() const { return ma( &T::computeName ); }
	inline MA1c_string ConstituentName() const { return ma( &T::ConstituentName ); }
	inline MA1c_string ControllerListName() const { return ma( &T::ControllerListName ); }
	inline MA1c_string ControllerName() const { return ma( &T::ControllerName ); }
	inline MA1c_string ControlName() const { return ma( &T::ControlName ); }
	inline MA1c_string CoolDesDay() const { return ma( &T::CoolDesDay ); }
	inline MA1c_string DamperName() const { return ma( &T::DamperName ); }
	inline MA1c_string EquipID() const { return ma( &T::EquipID ); }
	inline MA1c_string EquipListName() const { return ma( &T::EquipListName ); }
	inline MA1c_string EquipName() const { return ma( &T::EquipName ); }
	inline MA1c_string EquipType() const { return ma( &T::EquipType ); }
	inline MA1c_string EvapCoolerName() const { return ma( &T::EvapCoolerName ); }
	inline MA1c_string ExhaustAirFanName() const { return ma( &T::ExhaustAirFanName ); }
	inline MA1c_string ExtFanName() const { return ma( &T::ExtFanName ); }
	inline MA1c_string FanName() const { return ma( &T::FanName ); }
	inline MA1c_string HeatDesDay() const { return ma( &T::HeatDesDay ); }
	inline MA1c_string HeatExchangerName() const { return ma( &T::HeatExchangerName ); }
	inline MA1c_string InverterName() const { return ma( &T::InverterName ); }
	inline MA1c_string keyValue() const { return ma( &T::keyValue ); }
	inline MA1c_string LoopName() const { return ma( &T::LoopName ); }
	inline MA1c_string LoopType_s() const { return ma( &T::LoopType ); } // Also used as int so name decorated
	inline MA1c_string MatchValue() const { return ma( &T::MatchValue ); }
	inline MA1c_string MatchValue1() const { return ma( &T::MatchValue1 ); }
	inline MA1c_string MatchValue2() const { return ma( &T::MatchValue2 ); }
	inline MA1c_string MixerName() const { return ma( &T::MixerName ); }
	inline MA1c_string Name() const { return ma( &T::Name ); }
	inline MA1c_string name() const { return ma( &T::name ); }
	inline MA1c_string NameElecStorage() const { return ma( &T::NameElecStorage ); }
	inline MA1c_string NameExhaustHX() const { return ma( &T::NameExhaustHX ); }
	inline MA1c_string NameFCAirSup() const { return ma( &T::NameFCAirSup ); }
	inline MA1c_string NameFCAuxilHeat() const { return ma( &T::NameFCAuxilHeat ); }
	inline MA1c_string NameFCPM() const { return ma( &T::NameFCPM ); }
	inline MA1c_string NameFCWaterSup() const { return ma( &T::NameFCWaterSup ); }
	inline MA1c_string NameInverter() const { return ma( &T::NameInverter ); }
	inline MA1c_string NameStackCooler() const { return ma( &T::NameStackCooler ); }
	inline MA1c_string NodeName() const { return ma( &T::NodeName ); }
	inline MA1c_string ObjName() const { return ma( &T::ObjName ); }
	inline MA1c_string OldName() const { return ma( &T::OldName ); }
	inline MA1c_string PlantLoopName() const { return ma( &T::PlantLoopName ); }
	inline MA1c_string SFanName() const { return ma( &T::SFanName ); }
	inline MA1c_string SplitterName() const { return ma( &T::SplitterName ); }
	inline MA1c_string StorageName() const { return ma( &T::StorageName ); }
	inline MA1c_string SupplyAirFanName() const { return ma( &T::SupplyAirFanName ); }
	inline MA1c_string SurfName() const { return ma( &T::SurfName ); }
	inline MA1c_string SysName() const { return ma( &T::SysName ); }
	inline MA1c_string Title() const { return ma( &T::Title ); }
	inline MA1c_string units() const { return ma( &T::units ); }
	inline MA1c_string VarName() const { return ma( &T::VarName ); }
	inline MA1c_string varName() const { return ma( &T::varName ); }
	inline MA1c_string VarNameOnly() const { return ma( &T::VarNameOnly ); }
	inline MA1c_string ZoneHVACUnitName() const { return ma( &T::ZoneHVACUnitName ); }
	inline MA1c_string ZoneName() const { return ma( &T::ZoneName ); }
	inline MA1c_string ZonePlenumName() const { return ma( &T::ZonePlenumName ); }

	// string Members
	inline MA1_string AirLoopName() { return ma( &T::AirLoopName ); }
	inline MA1_string AirPriLoopName() { return ma( &T::AirPriLoopName ); }
	inline MA1_string AssignedLoopName() { return ma( &T::AssignedLoopName ); }
	inline MA1_string CompName() { return ma( &T::CompName ); }
	inline MA1_string ComponentListName() { return ma( &T::ComponentListName ); }
	inline MA1_string CompType() { return ma( &T::CompType ); }
	inline MA1_string computeName() { return ma( &T::computeName ); }
	inline MA1_string ConstituentName() { return ma( &T::ConstituentName ); }
	inline MA1_string ControllerListName() { return ma( &T::ControllerListName ); }
	inline MA1_string ControllerName() { return ma( &T::ControllerName ); }
	inline MA1_string ControlName() { return ma( &T::ControlName ); }
	inline MA1_string CoolDesDay() { return ma( &T::CoolDesDay ); }
	inline MA1_string DamperName() { return ma( &T::DamperName ); }
	inline MA1_string EquipID() { return ma( &T::EquipID ); }
	inline MA1_string EquipListName() { return ma( &T::EquipListName ); }
	inline MA1_string EquipName() { return ma( &T::EquipName ); }
	inline MA1_string EquipType() { return ma( &T::EquipType ); }
	inline MA1_string EvapCoolerName() { return ma( &T::EvapCoolerName ); }
	inline MA1_string ExhaustAirFanName() { return ma( &T::ExhaustAirFanName ); }
	inline MA1_string ExtFanName() { return ma( &T::ExtFanName ); }
	inline MA1_string FanName() { return ma( &T::FanName ); }
	inline MA1_string HeatDesDay() { return ma( &T::HeatDesDay ); }
	inline MA1_string HeatExchangerName() { return ma( &T::HeatExchangerName ); }
	inline MA1_string InverterName() { return ma( &T::InverterName ); }
	inline MA1_string keyValue() { return ma( &T::keyValue ); }
	inline MA1_string LoopName() { return ma( &T::LoopName ); }
	inline MA1_string LoopType_s() { return ma( &T::LoopType ); } // Also used as int so name decorated
	inline MA1_string MatchValue() { return ma( &T::MatchValue ); }
	inline MA1_string MatchValue1() { return ma( &T::MatchValue1 ); }
	inline MA1_string MatchValue2() { return ma( &T::MatchValue2 ); }
	inline MA1_string MixerName() { return ma( &T::MixerName ); }
	inline MA1_string Name() { return ma( &T::Name ); }
	inline MA1_string name() { return ma( &T::name ); }
	inline MA1_string NameElecStorage() { return ma( &T::NameElecStorage ); }
	inline MA1_string NameExhaustHX() { return ma( &T::NameExhaustHX ); }
	inline MA1_string NameFCAirSup() { return ma( &T::NameFCAirSup ); }
	inline MA1_string NameFCAuxilHeat() { return ma( &T::NameFCAuxilHeat ); }
	inline MA1_string NameFCPM() { return ma( &T::NameFCPM ); }
	inline MA1_string NameFCWaterSup() { return ma( &T::NameFCWaterSup ); }
	inline MA1_string NameInverter() { return ma( &T::NameInverter ); }
	inline MA1_string NameStackCooler() { return ma( &T::NameStackCooler ); }
	inline MA1_string NodeName() { return ma( &T::NodeName ); }
	inline MA1_string ObjName() { return ma( &T::ObjName ); }
	inline MA1_string OldName() { return ma( &T::OldName ); }
	inline MA1_string PlantLoopName() { return ma( &T::PlantLoopName ); }
	inline MA1_string SFanName() { return ma( &T::SFanName ); }
	inline MA1_string SplitterName() { return ma( &T::SplitterName ); }
	inline MA1_string StorageName() { return ma( &T::StorageName ); }
	inline MA1_string SupplyAirFanName() { return ma( &T::SupplyAirFanName ); }
	inline MA1_string SurfName() { return ma( &T::SurfName ); }
	inline MA1_string SysName() { return ma( &T::SysName ); }
	inline MA1_string Title() { return ma( &T::Title ); }
	inline MA1_string units() { return ma( &T::units ); }
	inline MA1_string VarName() { return ma( &T::VarName ); }
	inline MA1_string varName() { return ma( &T::varName ); }
	inline MA1_string VarNameOnly() { return ma( &T::VarNameOnly ); }
	inline MA1_string ZoneHVACUnitName() { return ma( &T::ZoneHVACUnitName ); }
	inline MA1_string ZoneName() { return ma( &T::ZoneName ); }
	inline MA1_string ZonePlenumName() { return ma( &T::ZonePlenumName ); }

	// Array1D< int >
	inline MA1< Array1D< int > > adjs() { return ma( &T::adjs ); }
	inline MA1< Array1D< int > > adjsl() { return ma( &T::adjsl ); }
	inline MA1c< Array1D< int > > adjs() const { return ma( &T::adjs ); }
	inline MA1c< Array1D< int > > adjsl() const { return ma( &T::adjsl ); }

	// Array1D< double >
	inline MA1c< Array1D< double > > hrly() const { return ma( &T::hrly ); }
	inline MA1< Array1D< double > > hrly() { return ma( &T::hrly ); }
	inline MA1c< Array1D< double > > InOutProjSLFracMult() const { return ma( &T::InOutProjSLFracMult ); }
	inline MA1< Array1D< double > > InOutProjSLFracMult() { return ma( &T::InOutProjSLFracMult ); }
	inline MA1c< Array1D< double > > mnth() const { return ma( &T::mnth ); }
	inline MA1< Array1D< double > > mnth() { return ma( &T::mnth ); }
	inline MA1c< Array1D< double > > OutProjSLFracMult() const { return ma( &T::OutProjSLFracMult ); }
	inline MA1< Array1D< double > > OutProjSLFracMult() { return ma( &T::OutProjSLFracMult ); }
	inline MA1c< Array1D< double > > RatedCOP() const { return ma( &T::RatedCOP ); }
	inline MA1< Array1D< double > > RatedCOP() { return ma( &T::RatedCOP ); }
	inline MA1c< Array1D< double > > RatedTotCap() const { return ma( &T::RatedTotCap ); }
	inline MA1< Array1D< double > > RatedTotCap() { return ma( &T::RatedTotCap ); }
	inline MA1c< Array1D< double > > ThetaFace() const { return ma( &T::ThetaFace ); }
	inline MA1< Array1D< double > > ThetaFace() { return ma( &T::ThetaFace ); }

#endif // ObjexxFCL_Array1_Project_MArray_hh_INCLUDED
