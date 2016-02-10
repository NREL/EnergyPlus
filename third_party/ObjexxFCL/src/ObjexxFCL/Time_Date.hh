#ifndef ObjexxFCL_Time_Date_hh_INCLUDED
#define ObjexxFCL_Time_Date_hh_INCLUDED

// Time and Date Functions
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.1.0
//
// Language: C++
//
// Copyright (c) 2000-2016 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// C++ Headers
#include <cstdint>
#include <string>

namespace ObjexxFCL {

// Forward
template< typename > class Array1;

// Current Time: HH, MM, SS
void
itime( Array1< std::int32_t > & timearray );

// Current Time: HH, MM, SS, CC
void
gettim( std::int16_t & h, std::int16_t & m, std::int16_t & s, std::int16_t & c );

// Current Time: HH, MM, SS, CC
void
gettim( std::int32_t & h, int & m, std::int32_t & s, std::int32_t & c );

// Current Time: HH:MM:SS
void
TIME( std::string & time );

// Time in Seconds Since Epoch
std::int64_t
TIME();

// Clock
std::string
CLOCK();

// System Clock
void
SYSTEM_CLOCK( Optional< std::int64_t > count = _, Optional< std::int64_t > count_rate = _, Optional< std::int64_t > count_max = _ );

// System Clock
void
SYSTEM_CLOCK32( Optional< std::int32_t > count = _, Optional< std::int32_t > count_rate = _, Optional< std::int32_t > count_max = _ );

// Current Date: DD, MM, YYYY
void
idate( Array1< std::int32_t > & datearray );

// Current Date: MM, DD, YY (Not Y2K Compliant)
void
idate( std::int16_t & month, std::int16_t & day, std::int16_t & year );

// Current Date: MM, DD, YY (Not Y2K Compliant)
void
idate( std::int32_t & month, std::int32_t & day, std::int32_t & year );

// Current Date: DD, MM, YYYY (Year < 2000 is offset from 1900)
void
idate4( Array1< std::int32_t > & datearray );

// Current Date: MM, DD, YYYY (Year is offset from 1900)
void
idate4( std::int32_t & month, std::int32_t & day, std::int32_t & year );

// Current Date: YYYY, MM, DD
void
getdat( std::int16_t & year, std::int16_t & month, std::int16_t & day );

// Current Date: YYYY, MM, DD
void
getdat( std::int32_t & year, std::int32_t & month, std::int32_t & day );

// Current Date String: DD-MMM-YY (Not Y2K Compliant)
void
date( std::string & day );

// Current Date String: YYDDD (Not Y2K Compliant)
std::string
jdate();

// Current Date and Time
void
date_and_time( Optional< std::string > date = _, Optional< std::string > time = _, Optional< std::string > zone = _, Optional< Array1D< int > > values = _ );

} // ObjexxFCL

#endif // ObjexxFCL_Time_Date_hh_INCLUDED
