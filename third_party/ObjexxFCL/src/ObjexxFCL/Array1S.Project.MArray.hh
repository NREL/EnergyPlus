#ifndef ObjexxFCL_Array1S_Project_MArray_hh_INCLUDED
#define ObjexxFCL_Array1S_Project_MArray_hh_INCLUDED

// Array1S.Project.MArray: Project-Specific MArray Methods
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

// Forward
template< typename > class Array1D;

	// Types
	template< typename M > using MA1Sc = MArray1< Array1S const, M >;
	template< typename M > using MA1S  = MArray1< Array1S, M >;
	typedef  MA1Sc< bool >  MA1Sc_bool;
	typedef  MA1S< bool >  MA1S_bool;
	typedef  MA1Sc< int >  MA1Sc_int;
	typedef  MA1S< int >  MA1S_int;
	typedef  MA1Sc< double >  MA1Sc_double;
	typedef  MA1S< double >  MA1S_double;
	typedef  MA1Sc< std::string >  MA1Sc_string;
	typedef  MA1S< std::string >  MA1S_string;

	// bool const Members
	inline MA1Sc_bool DefinedFlag() const { return ma( &T::DefinedFlag ); }

	// bool Members
	inline MA1S_bool DefinedFlag() { return ma( &T::DefinedFlag ); }

	// int const Members
	inline MA1Sc_int CoolingPriority() const { return ma( &T::CoolingPriority ); }
	inline MA1Sc_int EquipPtr() const { return ma( &T::EquipPtr ); }
	inline MA1Sc_int EquipType_Num() const { return ma( &T::EquipType_Num ); }
	inline MA1Sc_int HeatingPriority() const { return ma( &T::HeatingPriority ); }
	inline MA1Sc_int HeatTransferAlgorithm() const { return ma( &T::HeatTransferAlgorithm ); }
	inline MA1Sc_int NumDays() const { return ma( &T::NumDays ); }
	inline MA1Sc_int NumOfNodesInList() const { return ma( &T::NumOfNodesInList ); }
	inline MA1Sc_int SupNode() const { return ma( &T::SupNode ); }
	inline MA1Sc_int TAirRef() const { return ma( &T::TAirRef ); }
	inline MA1Sc_int Xnum() const { return ma( &T::Xnum ); }
	inline MA1Sc_int Ynum() const { return ma( &T::Ynum ); }

	// int Members
	inline MA1S_int CoolingPriority() { return ma( &T::CoolingPriority ); }
	inline MA1S_int EquipPtr() { return ma( &T::EquipPtr ); }
	inline MA1S_int EquipType_Num() { return ma( &T::EquipType_Num ); }
	inline MA1S_int HeatingPriority() { return ma( &T::HeatingPriority ); }
	inline MA1S_int HeatTransferAlgorithm() { return ma( &T::HeatTransferAlgorithm ); }
	inline MA1S_int NumDays() { return ma( &T::NumDays ); }
	inline MA1S_int NumOfNodesInList() { return ma( &T::NumOfNodesInList ); }
	inline MA1S_int SupNode() { return ma( &T::SupNode ); }
	inline MA1S_int TAirRef() { return ma( &T::TAirRef ); }
	inline MA1S_int Xnum() { return ma( &T::Xnum ); }
	inline MA1S_int Ynum() { return ma( &T::Ynum ); }

	// double const Members
	inline MA1Sc_double Centroid() const { return ma( &T::Centroid ); }
	inline MA1Sc_double DesVolFlowRate() const { return ma( &T::DesVolFlowRate ); }
	inline MA1Sc_double ExtGrossGroundWallArea_Multiplied() const { return ma( &T::ExtGrossGroundWallArea_Multiplied ); }
	inline MA1Sc_double ExtGrossWallArea_Multiplied() const { return ma( &T::ExtGrossWallArea_Multiplied ); }
	inline MA1Sc_double IRfromParentZone() const { return ma( &T::IRfromParentZone ); }
	inline MA1Sc_double OutDryBulbTemp() const { return ma( &T::OutDryBulbTemp ); }
	inline MA1Sc_double OutWetBulbTemp() const { return ma( &T::OutWetBulbTemp ); }
	inline MA1Sc_double Qrec() const { return ma( &T::Qrec ); }
	inline MA1Sc_double rDimension() const { return ma( &T::rDimension ); }
	inline MA1Sc_double RemainingOutputRequired() const { return ma( &T::RemainingOutputRequired ); }
	inline MA1Sc_double TotalOutputRequired() const { return ma( &T::TotalOutputRequired ); }
	inline MA1Sc_double Ujet() const { return ma( &T::Ujet ); }
	inline MA1Sc_double Urec() const { return ma( &T::Urec ); }
	inline MA1Sc_double WindSpeed() const { return ma( &T::WindSpeed ); }
	inline MA1Sc_double X() const { return ma( &T::X ); }
	inline MA1Sc_double x() const { return ma( &T::x ); }
	inline MA1Sc_double Y() const { return ma( &T::Y ); }
	inline MA1Sc_double y() const { return ma( &T::y ); }
	inline MA1Sc_double z() const { return ma( &T::z ); }

	// double Members
	inline MA1S_double Centroid() { return ma( &T::Centroid ); }
	inline MA1S_double DesVolFlowRate() { return ma( &T::DesVolFlowRate ); }
	inline MA1S_double ExtGrossGroundWallArea_Multiplied() { return ma( &T::ExtGrossGroundWallArea_Multiplied ); }
	inline MA1S_double ExtGrossWallArea_Multiplied() { return ma( &T::ExtGrossWallArea_Multiplied ); }
	inline MA1S_double IRfromParentZone() { return ma( &T::IRfromParentZone ); }
	inline MA1S_double OutDryBulbTemp() { return ma( &T::OutDryBulbTemp ); }
	inline MA1S_double OutWetBulbTemp() { return ma( &T::OutWetBulbTemp ); }
	inline MA1S_double Qrec() { return ma( &T::Qrec ); }
	inline MA1S_double rDimension() { return ma( &T::rDimension ); }
	inline MA1S_double RemainingOutputRequired() { return ma( &T::RemainingOutputRequired ); }
	inline MA1S_double TotalOutputRequired() { return ma( &T::TotalOutputRequired ); }
	inline MA1S_double Ujet() { return ma( &T::Ujet ); }
	inline MA1S_double Urec() { return ma( &T::Urec ); }
	inline MA1S_double WindSpeed() { return ma( &T::WindSpeed ); }
	inline MA1S_double X() { return ma( &T::X ); }
	inline MA1S_double x() { return ma( &T::x ); }
	inline MA1S_double Y() { return ma( &T::Y ); }
	inline MA1S_double y() { return ma( &T::y ); }
	inline MA1S_double z() { return ma( &T::z ); }

	// string const Members
	inline MA1Sc_string ComponentTypeName() const { return ma( &T::ComponentTypeName ); }
	inline MA1Sc_string ControlTypeName() const { return ma( &T::ControlTypeName ); }
	inline MA1Sc_string DataTypeName() const { return ma( &T::DataTypeName ); }
	inline MA1Sc_string EquipName() const { return ma( &T::EquipName ); }
	inline MA1Sc_string EquipType() const { return ma( &T::EquipType ); }
	inline MA1Sc_string FieldName() const { return ma( &T::FieldName ); }
	inline MA1Sc_string Name() const { return ma( &T::Name ); }
	inline MA1Sc_string SurfaceName() const { return ma( &T::SurfaceName ); }
	inline MA1Sc_string VarName() const { return ma( &T::VarName ); }
	inline MA1Sc_string VarNameOnly() const { return ma( &T::VarNameOnly ); }

	// string Members
	inline MA1S_string ComponentTypeName() { return ma( &T::ComponentTypeName ); }
	inline MA1S_string ControlTypeName() { return ma( &T::ControlTypeName ); }
	inline MA1S_string DataTypeName() { return ma( &T::DataTypeName ); }
	inline MA1S_string EquipName() { return ma( &T::EquipName ); }
	inline MA1S_string EquipType() { return ma( &T::EquipType ); }
	inline MA1S_string FieldName() { return ma( &T::FieldName ); }
	inline MA1S_string Name() { return ma( &T::Name ); }
	inline MA1S_string SurfaceName() { return ma( &T::SurfaceName ); }
	inline MA1S_string VarName() { return ma( &T::VarName ); }
	inline MA1S_string VarNameOnly() { return ma( &T::VarNameOnly ); }

	// Array1D< double >
	inline MA1Sc< Array1D< double > > origin() const { return ma( &T::origin ); }
	inline MA1S< Array1D< double > > origin() { return ma( &T::origin ); }

#endif // ObjexxFCL_Array1SSS_Project_MArray_hh_INCLUDED
