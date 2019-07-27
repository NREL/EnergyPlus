#ifndef ObjexxFCL_time_hh_INCLUDED
#define ObjexxFCL_time_hh_INCLUDED

// Time and Date Functions
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.2.0
//
// Language: C++
//
// Copyright (c) 2000-2017 Objexx Engineering, Inc. All Rights Reserved.
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
ITIME( Array1< std::int32_t > & timearray );

// Current Time: HH, MM, SS, CC
void
GETTIM( std::int64_t & h, std::int64_t & m, std::int64_t & s, std::int64_t & c );

// Current Time: HH, MM, SS, CC
void
GETTIM( std::int32_t & h, std::int32_t & m, std::int32_t & s, std::int32_t & c );

// Current Time: HH, MM, SS, CC
void
GETTIM( std::int16_t & h, std::int16_t & m, std::int16_t & s, std::int16_t & c );

// Time in Seconds Since Epoch
std::int64_t // Using 64-bit integer to avoid Year 2038 problem
TIME();

// Current Time: HH:MM:SS
void
TIME( std::string & time );

// Current Time: HH:MM:SS
std::string
CLOCK();

// System Clock
void
SYSTEM_CLOCK_64(
 Optional< std::int64_t > count = _,
 Optional< std::int64_t > count_rate = _,
 Optional< std::int64_t > count_max = _
);

// System Clock
void
SYSTEM_CLOCK_32(
 Optional< std::int32_t > count = _,
 Optional< std::int32_t > count_rate = _,
 Optional< std::int32_t > count_max = _
);

// System Clock
void
SYSTEM_CLOCK(
 Optional< std::int32_t > count = _,
 Optional< std::int32_t > count_rate = _,
 Optional< std::int32_t > count_max = _
);

// System Clock
void
SYSTEM_CLOCK_16(
 Optional< std::int16_t > count = _,
 Optional< std::int16_t > count_rate = _,
 Optional< std::int16_t > count_max = _
);

// Process CPU Time (s)
void
CPU_TIME( double & time );

// Process CPU Time (us)
void
CLOCKX( double & time );

// Current Date: DD, MM, YYYY
void
IDATE( Array1< std::int32_t > & datearray );

// Current Date: MM, DD, YY (Not Y2K Compliant)
void
IDATE( std::int64_t & month, std::int64_t & day, std::int64_t & year );

// Current Date: MM, DD, YY (Not Y2K Compliant)
void
IDATE( std::int32_t & month, std::int32_t & day, std::int32_t & year );

// Current Date: MM, DD, YY (Not Y2K Compliant)
void
IDATE( std::int16_t & month, std::int16_t & day, std::int16_t & year );

// Current Date: DD, MM, YYYY (Year < 2000 is offset from 1900)
void
IDATE4( Array1< std::int32_t > & datearray );

// Current Date: MM, DD, YYYY (Year is offset from 1900)
void
IDATE4( std::int64_t & month, std::int64_t & day, std::int64_t & year );

// Current Date: MM, DD, YYYY (Year is offset from 1900)
void
IDATE4( std::int32_t & month, std::int32_t & day, std::int32_t & year );

// Current Date: MM, DD, YYYY (Year is offset from 1900)
void
IDATE4( std::int16_t & month, std::int16_t & day, std::int16_t & year );

// Current Julian Date String: YYDDD (Not Y2K Compliant)
std::string
JDATE();

// Current Julian Date String: YYDDD (Not Y2K Compliant)
std::string
jdate();

// Current Julian Date String: YYYYDDD
std::string
JDATE4();

// Current Julian Date String: YYYYDDD
std::string
jdate4();

// Current Date: YYYY, MM, DD
void
GETDAT( std::int64_t & year, std::int64_t & month, std::int64_t & day );

// Current Date: YYYY, MM, DD
void
GETDAT( std::int32_t & year, std::int32_t & month, std::int32_t & day );

// Current Date: YYYY, MM, DD
void
GETDAT( std::int16_t & year, std::int16_t & month, std::int16_t & day );

// Current Date String: MM/DD/YY (Not Y2K Compliant)
std::string
DATE();

// Current Date String: MM/DD/YY (Not Y2K Compliant)
std::string
date();

// Current Date String: DD-MMM-YY (Not Y2K Compliant)
void
DATE( std::string & day );

// Current Date String: DD-MMM-YYYY
void
DATE4( std::string & day );

// Current Date and Time
void
DATE_AND_TIME(
 Optional< std::string > date = _,
 Optional< std::string > time = _,
 Optional< std::string > zone = _,
 Optional< Array1D< int > > values = _
);

// Current Date and Time
void
date_and_time(
 Optional< std::string > date = _,
 Optional< std::string > time = _,
 Optional< std::string > zone = _,
 Optional< Array1D< int > > values = _
);

// Sleep for Given Seconds
void
SLEEP( double const sec );

} // ObjexxFCL

#endif // ObjexxFCL_time_hh_INCLUDED
