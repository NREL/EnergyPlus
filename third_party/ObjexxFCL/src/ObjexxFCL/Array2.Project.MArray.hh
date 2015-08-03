#ifndef ObjexxFCL_Array2_Project_MArray_hh_INCLUDED
#define ObjexxFCL_Array2_Project_MArray_hh_INCLUDED

// Array2.Project.MArray: Project-Specific MArray Methods
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
	template< typename M > using MA2c = MArray2< Array2 const, M >;
	template< typename M > using MA2  = MArray2< Array2, M >;
	typedef  MA2c< bool >  MA2c_bool;
	typedef  MA2< bool >  MA2_bool;
	typedef  MA2c< int >  MA2c_int;
	typedef  MA2< int >  MA2_int;
	typedef  MA2c< double >  MA2c_double;
	typedef  MA2< double >  MA2_double;
	typedef  MA2c< std::string >  MA2c_string;
	typedef  MA2< std::string >  MA2_string;

	// int const Members
	inline MA2c_int ClassType() const { return ma( &T::ClassType ); }
	inline MA2c_int CoolDDNum() const { return ma( &T::CoolDDNum ); }
	inline MA2c_int HeatDDNum() const { return ma( &T::HeatDDNum ); }
	inline MA2c_int TimeStepNumAtCoolMax() const { return ma( &T::TimeStepNumAtCoolMax ); }
	inline MA2c_int TimeStepNumAtHeatMax() const { return ma( &T::TimeStepNumAtHeatMax ); }

	// int Members
	inline MA2_int ClassType() { return ma( &T::ClassType ); }
	inline MA2_int CoolDDNum() { return ma( &T::CoolDDNum ); }
	inline MA2_int HeatDDNum() { return ma( &T::HeatDDNum ); }
	inline MA2_int TimeStepNumAtCoolMax() { return ma( &T::TimeStepNumAtCoolMax ); }
	inline MA2_int TimeStepNumAtHeatMax() { return ma( &T::TimeStepNumAtHeatMax ); }

	// double const Members
	inline MA2c_double Area() const { return ma( &T::Area ); }
	inline MA2c_double DesCoolCoilInHumRat() const { return ma( &T::DesCoolCoilInHumRat ); }
	inline MA2c_double DesCoolCoilInTemp() const { return ma( &T::DesCoolCoilInTemp ); }
	inline MA2c_double DesCoolDens() const { return ma( &T::DesCoolDens ); }
	inline MA2c_double DesCoolLoad() const { return ma( &T::DesCoolLoad ); }
	inline MA2c_double DesCoolMassFlow() const { return ma( &T::DesCoolMassFlow ); }
	inline MA2c_double DesCoolVolFlow() const { return ma( &T::DesCoolVolFlow ); }
	inline MA2c_double DesHeatCoilInHumRat() const { return ma( &T::DesHeatCoilInHumRat ); }
	inline MA2c_double DesHeatCoilInTemp() const { return ma( &T::DesHeatCoilInTemp ); }
	inline MA2c_double DesHeatDens() const { return ma( &T::DesHeatDens ); }
	inline MA2c_double DesHeatLoad() const { return ma( &T::DesHeatLoad ); }
	inline MA2c_double DesHeatMassFlow() const { return ma( &T::DesHeatMassFlow ); }
	inline MA2c_double DesHeatVolFlow() const { return ma( &T::DesHeatVolFlow ); }
	inline MA2c_double Hc() const { return ma( &T::Hc ); }
	inline MA2c_double Height() const { return ma( &T::Height ); }
	inline MA2c_double OutHumRatAtCoolPeak() const { return ma( &T::OutHumRatAtCoolPeak ); }
	inline MA2c_double OutHumRatAtHeatPeak() const { return ma( &T::OutHumRatAtHeatPeak ); }
	inline MA2c_double OutTempAtCoolPeak() const { return ma( &T::OutTempAtCoolPeak ); }
	inline MA2c_double OutTempAtHeatPeak() const { return ma( &T::OutTempAtHeatPeak ); }
	inline MA2c_double Temp() const { return ma( &T::Temp ); }
	inline MA2c_double TMeanAir() const { return ma( &T::TMeanAir ); }
	inline MA2c_double Ujet() const { return ma( &T::Ujet ); }
	inline MA2c_double Urec() const { return ma( &T::Urec ); }
	inline MA2c_double ZoneHumRatAtCoolPeak() const { return ma( &T::ZoneHumRatAtCoolPeak ); }
	inline MA2c_double ZoneHumRatAtHeatPeak() const { return ma( &T::ZoneHumRatAtHeatPeak ); }
	inline MA2c_double ZoneRetTempAtCoolPeak() const { return ma( &T::ZoneRetTempAtCoolPeak ); }
	inline MA2c_double ZoneRetTempAtHeatPeak() const { return ma( &T::ZoneRetTempAtHeatPeak ); }
	inline MA2c_double ZoneTempAtCoolPeak() const { return ma( &T::ZoneTempAtCoolPeak ); }
	inline MA2c_double ZoneTempAtHeatPeak() const { return ma( &T::ZoneTempAtHeatPeak ); }

	// double Members
	inline MA2_double Area() { return ma( &T::Area ); }
	inline MA2_double DesCoolCoilInHumRat() { return ma( &T::DesCoolCoilInHumRat ); }
	inline MA2_double DesCoolCoilInTemp() { return ma( &T::DesCoolCoilInTemp ); }
	inline MA2_double DesCoolDens() { return ma( &T::DesCoolDens ); }
	inline MA2_double DesCoolLoad() { return ma( &T::DesCoolLoad ); }
	inline MA2_double DesCoolMassFlow() { return ma( &T::DesCoolMassFlow ); }
	inline MA2_double DesCoolVolFlow() { return ma( &T::DesCoolVolFlow ); }
	inline MA2_double DesHeatCoilInHumRat() { return ma( &T::DesHeatCoilInHumRat ); }
	inline MA2_double DesHeatCoilInTemp() { return ma( &T::DesHeatCoilInTemp ); }
	inline MA2_double DesHeatDens() { return ma( &T::DesHeatDens ); }
	inline MA2_double DesHeatLoad() { return ma( &T::DesHeatLoad ); }
	inline MA2_double DesHeatMassFlow() { return ma( &T::DesHeatMassFlow ); }
	inline MA2_double DesHeatVolFlow() { return ma( &T::DesHeatVolFlow ); }
	inline MA2_double Hc() { return ma( &T::Hc ); }
	inline MA2_double Height() { return ma( &T::Height ); }
	inline MA2_double OutHumRatAtCoolPeak() { return ma( &T::OutHumRatAtCoolPeak ); }
	inline MA2_double OutHumRatAtHeatPeak() { return ma( &T::OutHumRatAtHeatPeak ); }
	inline MA2_double OutTempAtCoolPeak() { return ma( &T::OutTempAtCoolPeak ); }
	inline MA2_double OutTempAtHeatPeak() { return ma( &T::OutTempAtHeatPeak ); }
	inline MA2_double Temp() { return ma( &T::Temp ); }
	inline MA2_double TMeanAir() { return ma( &T::TMeanAir ); }
	inline MA2_double Ujet() { return ma( &T::Ujet ); }
	inline MA2_double Urec() { return ma( &T::Urec ); }
	inline MA2_double ZoneHumRatAtCoolPeak() { return ma( &T::ZoneHumRatAtCoolPeak ); }
	inline MA2_double ZoneHumRatAtHeatPeak() { return ma( &T::ZoneHumRatAtHeatPeak ); }
	inline MA2_double ZoneRetTempAtCoolPeak() { return ma( &T::ZoneRetTempAtCoolPeak ); }
	inline MA2_double ZoneRetTempAtHeatPeak() { return ma( &T::ZoneRetTempAtHeatPeak ); }
	inline MA2_double ZoneTempAtCoolPeak() { return ma( &T::ZoneTempAtCoolPeak ); }
	inline MA2_double ZoneTempAtHeatPeak() { return ma( &T::ZoneTempAtHeatPeak ); }

	// string const Members
	inline MA2c_string AirNodeName() const { return ma( &T::AirNodeName ); }
	inline MA2c_string CoolDesDay() const { return ma( &T::CoolDesDay ); }
	inline MA2c_string HeatDesDay() const { return ma( &T::HeatDesDay ); }

	// string Members
	inline MA2_string AirNodeName() { return ma( &T::AirNodeName ); }
	inline MA2_string CoolDesDay() { return ma( &T::CoolDesDay ); }
	inline MA2_string HeatDesDay() { return ma( &T::HeatDesDay ); }

	// Array1D< double >
	inline MA2c< Array1D< double > > hrly() const { return ma( &T::hrly ); }
	inline MA2< Array1D< double > > hrly() { return ma( &T::hrly ); }
	inline MA2c< Array1D< double > > mnth() const { return ma( &T::mnth ); }
	inline MA2< Array1D< double > > mnth() { return ma( &T::mnth ); }

#endif // ObjexxFCL_Array2_Project_MArray_hh_INCLUDED
