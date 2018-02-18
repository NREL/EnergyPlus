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
#include <ObjexxFCL/time.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <cstdlib>
#include <ctime>
#include <limits>
#include <sstream>
#include <thread>

namespace ObjexxFCL {

// Current Time: HH, MM, SS
void
ITIME( Array1< std::int32_t > & timearray )
{
	assert( timearray.l() <= 1 );
	assert( timearray.u() >= 3 );
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	timearray( 1 ) = timeinfo->tm_hour;
	timearray( 2 ) = timeinfo->tm_min;
	timearray( 3 ) = timeinfo->tm_sec;
}

// Current Time: HH, MM, SS, CC
void
GETTIM( std::int64_t & h, std::int64_t & m, std::int64_t & s, std::int64_t & c )
{
	using std::chrono::system_clock;
	using std::chrono::duration_cast;
	using std::chrono::milliseconds;
	system_clock::time_point const now( system_clock::now() );
	int const ms( static_cast< int >( duration_cast< milliseconds >( now.time_since_epoch() ).count() % 1000 ) ); // msec
	std::time_t const current_time( system_clock::to_time_t( now ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	h = timeinfo->tm_hour;
	m = timeinfo->tm_min;
	s = timeinfo->tm_sec;
	c = ms / 10;
}

// Current Time: HH, MM, SS, CC
void
GETTIM( std::int32_t & h, std::int32_t & m, std::int32_t & s, std::int32_t & c )
{
	using std::chrono::system_clock;
	using std::chrono::duration_cast;
	using std::chrono::milliseconds;
	system_clock::time_point const now( system_clock::now() );
	int const ms( static_cast< int >( duration_cast< milliseconds >( now.time_since_epoch() ).count() % 1000 ) ); // msec
	std::time_t const current_time( system_clock::to_time_t( now ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	h = timeinfo->tm_hour;
	m = timeinfo->tm_min;
	s = timeinfo->tm_sec;
	c = ms / 10;
}

// Current Time: HH, MM, SS, CC
void
GETTIM( std::int16_t & h, std::int16_t & m, std::int16_t & s, std::int16_t & c )
{
	using std::chrono::system_clock;
	using std::chrono::duration_cast;
	using std::chrono::milliseconds;
	system_clock::time_point const now( system_clock::now() );
	int const ms( static_cast< int >( duration_cast< milliseconds >( now.time_since_epoch() ).count() % 1000 ) ); // msec
	std::time_t const current_time( system_clock::to_time_t( now ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	h = timeinfo->tm_hour;
	m = timeinfo->tm_min;
	s = timeinfo->tm_sec;
	c = ms / 10;
}

// Time in Seconds Since Epoch
std::int64_t // Using 64-bit integer to avoid Year 2038 problem
TIME()
{
	using std::chrono::system_clock;
	system_clock::time_point const now( system_clock::now() );
	system_clock::duration const dur( now.time_since_epoch() );
	return dur.count() * system_clock::period::num / system_clock::period::den;
}

// Current Time: HH:MM:SS
void
TIME( std::string & time )
{
	using std::setw;
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	int const hh( timeinfo->tm_hour );
	int const mm( timeinfo->tm_min );
	int const ss( timeinfo->tm_sec );
	std::stringstream time_stream;
	time_stream << std::setfill( '0' ) << setw( 2 ) << hh << ':' << setw( 2 ) << mm << ':' << setw( 2 ) << ss;
	time = time_stream.str();
}

// Current Time: HH:MM:SS
std::string
CLOCK()
{
	using std::setw;
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	int const hh( timeinfo->tm_hour );
	int const mm( timeinfo->tm_min );
	int const ss( timeinfo->tm_sec );
	std::stringstream time_stream;
	time_stream << std::setfill( '0' ) << setw( 2 ) << hh << ':' << setw( 2 ) << mm << ':' << setw( 2 ) << ss;
	return time_stream.str();
}

// System Clock
void
SYSTEM_CLOCK_64(
 Optional< std::int64_t > count,
 Optional< std::int64_t > count_rate,
 Optional< std::int64_t > count_max
)
{
	using std::chrono::system_clock;
	system_clock::time_point const now( system_clock::now() );
	if ( count.present() ) count = now.time_since_epoch().count();
	if ( count_rate.present() ) count_rate = system_clock::period::den / system_clock::period::num;
	if ( count_max.present() ) count_max = system_clock::duration::max().count();
}

// System Clock
void
SYSTEM_CLOCK_32(
 Optional< std::int32_t > count,
 Optional< std::int32_t > count_rate,
 Optional< std::int32_t > count_max
)
{
	using std::chrono::system_clock;
	system_clock::time_point const now( system_clock::now() );
	std::int64_t count_64( now.time_since_epoch().count() );
	std::int64_t count_rate_64( system_clock::period::den / system_clock::period::num );
	std::int64_t count_max_64( system_clock::duration::max().count() );
	std::int64_t mult( count_rate_64 / std::numeric_limits< std::int32_t >::max() );
	if ( mult > 0 ) { // Scale rate down
		mult = static_cast< std::int64_t >( std::pow( 10.0, std::ceil( std::log10( double( mult ) ) ) ) );
		count_64 /= mult;
		count_rate_64 /= mult;
		count_max_64 /= mult;
	}
	mult = count_max_64 / std::numeric_limits< std::int32_t >::max();
	if ( mult > 0 ) { // Scale range down
		mult = static_cast< std::int64_t >( std::pow( 10.0, std::ceil( std::log10( double( mult ) ) ) ) );
		count_max_64 /= mult;
		count_64 %= count_max_64;
	}
	if ( count.present() ) count = count_64;
	if ( count_rate.present() ) count_rate = count_rate_64;
	if ( count_max.present() ) count_max = count_max_64;
}

// System Clock
void
SYSTEM_CLOCK(
 Optional< std::int32_t > count,
 Optional< std::int32_t > count_rate,
 Optional< std::int32_t > count_max
)
{
	using std::chrono::system_clock;
	system_clock::time_point const now( system_clock::now() );
	std::int64_t count_64( now.time_since_epoch().count() );
	std::int64_t count_rate_64( system_clock::period::den / system_clock::period::num );
	std::int64_t count_max_64( system_clock::duration::max().count() );
	std::int64_t mult( count_rate_64 / std::numeric_limits< std::int32_t >::max() );
	if ( mult > 0 ) { // Scale rate down
		mult = static_cast< std::int64_t >( std::pow( 10.0, std::ceil( std::log10( double( mult ) ) ) ) );
		count_64 /= mult;
		count_rate_64 /= mult;
		count_max_64 /= mult;
	}
	mult = count_max_64 / std::numeric_limits< std::int32_t >::max();
	if ( mult > 0 ) { // Scale range down
		mult = static_cast< std::int64_t >( std::pow( 10.0, std::ceil( std::log10( double( mult ) ) ) ) );
		count_max_64 /= mult;
		count_64 %= count_max_64;
	}
	if ( count.present() ) count = count_64;
	if ( count_rate.present() ) count_rate = count_rate_64;
	if ( count_max.present() ) count_max = count_max_64;
}

// System Clock
void
SYSTEM_CLOCK_16(
 Optional< std::int16_t > count,
 Optional< std::int16_t > count_rate,
 Optional< std::int16_t > count_max
)
{
	using std::chrono::system_clock;
	system_clock::time_point const now( system_clock::now() );
	std::int64_t count_64( now.time_since_epoch().count() );
	std::int64_t count_rate_64( system_clock::period::den / system_clock::period::num );
	std::int64_t count_max_64( system_clock::duration::max().count() );
	std::int64_t mult( count_rate_64 / std::numeric_limits< std::int16_t >::max() );
	if ( mult > 0 ) { // Scale rate down
		mult = static_cast< std::int64_t >( std::pow( 10.0, std::ceil( std::log10( double( mult ) ) ) ) );
		count_64 /= mult;
		count_rate_64 /= mult;
		count_max_64 /= mult;
	}
	mult = count_max_64 / std::numeric_limits< std::int16_t >::max();
	if ( mult > 0 ) { // Scale range down
		mult = static_cast< std::int64_t >( std::pow( 10.0, std::ceil( std::log10( double( mult ) ) ) ) );
		count_max_64 /= mult;
		count_64 %= count_max_64;
	}
	if ( count.present() ) count = count_64;
	if ( count_rate.present() ) count_rate = count_rate_64;
	if ( count_max.present() ) count_max = count_max_64;
}

// Process CPU Time (s)
void
CPU_TIME( double & time )
{
	time = double( std::clock() ) / CLOCKS_PER_SEC;
}

// Process CPU Time (us)
void
CLOCKX( double & time )
{
	time = 1.0e6 * double( std::clock() ) / CLOCKS_PER_SEC;
}

// Current Date: DD, MM, YYYY
void
IDATE( Array1< std::int32_t > & datearray )
{
	assert( datearray.l() <= 1 );
	assert( datearray.u() >= 3 );
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	datearray( 1 ) = timeinfo->tm_mday; // Day of month: 1-31
	datearray( 2 ) = timeinfo->tm_mon + 1; // Month of year: 1-12
#ifdef OBJEXXFCL_IDATE_INTEL
	datearray( 3 ) = std::max( timeinfo->tm_year, 1969 ) % 100; // Not Y2K safe (Intel)
#else
	datearray( 3 ) = timeinfo->tm_year + 1900; // Year
#endif
}

// Current Date: MM, DD, YY (Not Y2K Compliant)
void
IDATE( std::int64_t & month, std::int64_t & day, std::int64_t & year )
{
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	month = timeinfo->tm_mon + 1; // Month of year: 1-12
	day = timeinfo->tm_mday; // Day of month: 1-31
#ifdef OBJEXXFCL_IDATE_PORTABILITY
	year = timeinfo->tm_year; // Year offset from 1900 (portability)
#else
	year = timeinfo->tm_year % 100; // 2-Digit Year: 0-99 (original)
#endif
}

// Current Date: MM, DD, YY (Not Y2K Compliant)
void
IDATE( std::int32_t & month, std::int32_t & day, std::int32_t & year )
{
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	month = timeinfo->tm_mon + 1; // Month of year: 1-12
	day = timeinfo->tm_mday; // Day of month: 1-31
#ifdef OBJEXXFCL_IDATE_PORTABILITY
	year = timeinfo->tm_year; // Year offset from 1900 (portability)
#else
	year = timeinfo->tm_year % 100; // 2-Digit Year: 0-99 (original)
#endif
}

// Current Date: MM, DD, YY (Not Y2K Compliant)
void
IDATE( std::int16_t & month, std::int16_t & day, std::int16_t & year )
{
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	month = timeinfo->tm_mon + 1; // Month of year: 1-12
	day = timeinfo->tm_mday; // Day of month: 1-31
#ifdef OBJEXXFCL_IDATE_PORTABILITY
	year = timeinfo->tm_year; // Year offset from 1900 (portability)
#else
	year = timeinfo->tm_year % 100; // 2-Digit Year: 0-99 (original)
#endif
}

// Current Date: DD, MM, YYYY (Year < 2000 is offset from 1900)
void
IDATE4( Array1< std::int32_t > & datearray )
{
	assert( datearray.l() <= 1 );
	assert( datearray.u() >= 3 );
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	datearray( 1 ) = timeinfo->tm_mday; // Day of month: 1-31
	datearray( 2 ) = timeinfo->tm_mon + 1; // Month of year: 1-12
	datearray( 3 ) = timeinfo->tm_year + ( timeinfo->tm_year >= 100 ? 1900 : 0 ); // Year if >= 2000 else 2-digit year offset from 1900
}

// Current Date: MM, DD, YYYY (Year is offset from 1900)
void
IDATE4( std::int64_t & month, std::int64_t & day, std::int64_t & year )
{
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	month = timeinfo->tm_mon + 1; // Month of year: 1-12
	day = timeinfo->tm_mday; // Day of month: 1-31
	year = timeinfo->tm_year; // Year offset from 1900
}

// Current Date: MM, DD, YYYY (Year is offset from 1900)
void
IDATE4( std::int32_t & month, std::int32_t & day, std::int32_t & year )
{
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	month = timeinfo->tm_mon + 1; // Month of year: 1-12
	day = timeinfo->tm_mday; // Day of month: 1-31
	year = timeinfo->tm_year; // Year offset from 1900
}

// Current Date: MM, DD, YYYY (Year is offset from 1900)
void
IDATE4( std::int16_t & month, std::int16_t & day, std::int16_t & year )
{
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	month = timeinfo->tm_mon + 1; // Month of year: 1-12
	day = timeinfo->tm_mday; // Day of month: 1-31
	year = timeinfo->tm_year; // Year offset from 1900
}

// Current Date String: YYDDD (Not Y2K Compliant)
std::string
JDATE()
{
	using std::setw;
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	std::int16_t const day( timeinfo->tm_yday + 1 ); // Day of year: 1-366
	std::int16_t const year( timeinfo->tm_year % 100 ); // 2-Digit Year: 0-99
	std::stringstream s;
	s << std::setfill( '0' ) << setw( 2 ) << year << setw( 3 ) << day;
	return s.str();
}

// Current Date String: YYDDD (Not Y2K Compliant)
std::string
jdate()
{
	using std::setw;
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	std::int16_t const day( timeinfo->tm_yday + 1 ); // Day of year: 1-366
	std::int16_t const year( timeinfo->tm_year % 100 ); // 2-Digit Year: 0-99
	std::stringstream s;
	s << std::setfill( '0' ) << setw( 2 ) << year << setw( 3 ) << day;
	return s.str();
}

// Current Julian Date String: YYYYDDD
std::string
JDATE4()
{
	using std::setw;
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	std::int16_t const day( timeinfo->tm_yday + 1 ); // Day of year: 1-366
	std::int16_t const year( timeinfo->tm_year + 1900 ); // Year
	std::stringstream s;
	s << std::setfill( '0' ) << setw( 4 ) << year << setw( 3 ) << day;
	return s.str();
}

// Current Julian Date String: YYYYDDD
std::string
jdate4()
{
	using std::setw;
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	std::int16_t const day( timeinfo->tm_yday + 1 ); // Day of year: 1-366
	std::int16_t const year( timeinfo->tm_year + 1900 ); // Year
	std::stringstream s;
	s << std::setfill( '0' ) << setw( 4 ) << year << setw( 3 ) << day;
	return s.str();
}

// Current Date: YYYY, MM, DD
void
GETDAT( std::int64_t & year, std::int64_t & month, std::int64_t & day )
{
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	year = timeinfo->tm_year + 1900; // Year
	month = timeinfo->tm_mon + 1; // Month of year: 1-12
	day = timeinfo->tm_mday; // Day of month: 1-31
}

// Current Date: YYYY, MM, DD
void
GETDAT( std::int32_t & year, std::int32_t & month, std::int32_t & day )
{
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	year = timeinfo->tm_year + 1900; // Year
	month = timeinfo->tm_mon + 1; // Month of year: 1-12
	day = timeinfo->tm_mday; // Day of month: 1-31
}

// Current Date: YYYY, MM, DD
void
GETDAT( std::int16_t & year, std::int16_t & month, std::int16_t & day )
{
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	year = timeinfo->tm_year + 1900; // Year
	month = timeinfo->tm_mon + 1; // Month of year: 1-12
	day = timeinfo->tm_mday; // Day of month: 1-31
}

// Month Name: MMM
std::string
MMM( int const m )
{
	switch ( m ) {
	case 1:
		return "JAN";
	case 2:
		return "FEB";
	case 3:
		return "MAR";
	case 4:
		return "APR";
	case 5:
		return "MAY";
	case 6:
		return "JUN";
	case 7:
		return "JUL";
	case 8:
		return "AUG";
	case 9:
		return "SEP";
	case 10:
		return "OCT";
	case 11:
		return "NOV";
	case 12:
		return "DEC";
	default:
		assert( false );
		return "???";
	}
}

// Current Date: MM/DD/YY (Not Y2K Compliant)
std::string
DATE()
{
	using std::setw;
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	std::int16_t const month( timeinfo->tm_mon + 1 ); // Month of year: 1-12
	std::int16_t const day( timeinfo->tm_mday ); // Day of month: 1-31
	std::int16_t const year( timeinfo->tm_year % 100 ); // 2-Digit Year: 0-99
	std::stringstream s;
	s << std::setfill( '0' ) << setw( 2 ) << month << '/' << setw( 2 ) << day << '/' << setw( 2 ) << year;
	return s.str();
}

// Current Date: MM/DD/YY (Not Y2K Compliant)
std::string
date()
{
	using std::setw;
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	std::int16_t const month( timeinfo->tm_mon + 1 ); // Month of year: 1-12
	std::int16_t const day( timeinfo->tm_mday ); // Day of month: 1-31
	std::int16_t const year( timeinfo->tm_year % 100 ); // 2-Digit Year: 0-99
	std::stringstream s;
	s << std::setfill( '0' ) << setw( 2 ) << month << '/' << setw( 2 ) << day << '/' << setw( 2 ) << year;
	return s.str();
}

// Current Date: DD-MMM-YY (Not Y2K Compliant)
void
DATE( std::string & date )
{
	using std::setw;
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	std::int16_t const day( timeinfo->tm_mday ); // Day of month: 1-31
	std::int16_t const month( timeinfo->tm_mon + 1 ); // Month of year: 1-12
	std::int16_t const year( timeinfo->tm_year % 100 ); // 2-Digit Year: 0-99
	std::stringstream s;
	s << std::setfill( '0' ) << setw( 2 ) << day << '-' << MMM( month ) << '-' << setw( 2 ) << year;
	date = s.str();
}

// Current Date: DD-MMM-YYYY
void
DATE4( std::string & date )
{
	using std::setw;
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	std::int16_t const day( timeinfo->tm_mday ); // Day of month: 1-31
	std::int16_t const month( timeinfo->tm_mon + 1 ); // Month of year: 1-12
	std::int16_t const year( timeinfo->tm_year + 1900 ); // Year
	std::stringstream s;
	s << std::setfill( '0' ) << setw( 2 ) << day << '-' << MMM( month ) << '-' << setw( 4 ) << year;
	date = s.str();
}

// Local Time Zone Offset from UTC in Seconds
int
time_zone_offset_seconds()
{
	std::time_t const current_time( std::time( nullptr ) );
	std::time_t const ltime( std::mktime( std::localtime( &current_time ) ) ); // Local
#if defined(_WIN32)
	std::time_t const gtime( _mkgmtime( std::localtime( &current_time ) ) ); // UTC
#elif defined(__linux__)
	std::time_t const gtime( timegm( std::localtime( &current_time ) ) ); // UTC
#else
	std::time_t const gtime( std::mktime( std::gmtime( &current_time ) ) ); // UTC
#endif
	return static_cast< int >( gtime - ltime );
}

// Current Date and Time
void
DATE_AND_TIME(
 Optional< std::string > date,
 Optional< std::string > time,
 Optional< std::string > zone,
 Optional< Array1D< int > > values
)
{
	using std::chrono::system_clock;
	using std::chrono::duration_cast;
	using std::chrono::milliseconds;
	using std::setw;
	system_clock::time_point const now( system_clock::now() );
	int const ms( static_cast< int >( duration_cast< milliseconds >( now.time_since_epoch() ).count() % 1000 ) ); // msec
	std::time_t const current_time( system_clock::to_time_t( now ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );

	int const DD( timeinfo->tm_mday ); // Day of month: 1-31
	int const MM( timeinfo->tm_mon + 1 ); // Month of year: 1-12
	int const YY( timeinfo->tm_year + 1900 ); // Year
	if ( date.present() ) {
		std::stringstream date_stream;
		date_stream << std::setfill( '0' ) << setw( 4 ) << YY << setw( 2 ) << MM << setw( 2 ) << DD;
		date = date_stream.str();
	}

	int const hh( timeinfo->tm_hour );
	int const mm( timeinfo->tm_min );
	int const ss( timeinfo->tm_sec );
	if ( time.present() ) {
		std::stringstream time_stream;
		time_stream << std::setfill( '0' ) << setw( 2 ) << hh << setw( 2 ) << mm << setw( 2 ) << ss << '.' << setw( 3 ) << ms;
		time = time_stream.str();
	}

	int const zs( time_zone_offset_seconds() );
	if ( zone.present() ) {
		int const zh( std::abs( zs ) / 3600 );
		int const zm( ( std::abs( zs ) - ( zh * 3600 ) ) / 60 );
		std::stringstream zone_stream;
		zone_stream << std::setfill( '0' ) << ( zs >= 0 ? '+' : '-' ) << setw( 2 ) << zh << setw( 2 ) << zm;
		zone = zone_stream.str();
	}

	if ( values.present() ) {
		assert( ( values().l() <= 1 ) && ( values().u() >= 8 ) );
		values()( 1 ) = YY;
		values()( 2 ) = MM;
		values()( 3 ) = DD;
		values()( 4 ) = zs / 60;
		values()( 5 ) = hh;
		values()( 6 ) = mm;
		values()( 7 ) = ss;
		values()( 8 ) = ms;
	}
}

// Current Date and Time
void
date_and_time(
 Optional< std::string > date,
 Optional< std::string > time,
 Optional< std::string > zone,
 Optional< Array1D< int > > values
)
{
	using std::chrono::system_clock;
	using std::chrono::duration_cast;
	using std::chrono::milliseconds;
	using std::setw;
	system_clock::time_point const now( system_clock::now() );
	int const ms( static_cast< int >( duration_cast< milliseconds >( now.time_since_epoch() ).count() % 1000 ) ); // msec
	std::time_t const current_time( system_clock::to_time_t( now ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );

	int const DD( timeinfo->tm_mday ); // Day of month: 1-31
	int const MM( timeinfo->tm_mon + 1 ); // Month of year: 1-12
	int const YY( timeinfo->tm_year + 1900 ); // Year
	if ( date.present() ) {
		std::stringstream date_stream;
		date_stream << std::setfill( '0' ) << setw( 4 ) << YY << setw( 2 ) << MM << setw( 2 ) << DD;
		date = date_stream.str();
	}

	int const hh( timeinfo->tm_hour );
	int const mm( timeinfo->tm_min );
	int const ss( timeinfo->tm_sec );
	if ( time.present() ) {
		std::stringstream time_stream;
		time_stream << std::setfill( '0' ) << setw( 2 ) << hh << setw( 2 ) << mm << setw( 2 ) << ss << '.' << setw( 3 ) << ms;
		time = time_stream.str();
	}

	int const zs( time_zone_offset_seconds() );
	if ( zone.present() ) {
		int const zh( std::abs( zs ) / 3600 );
		int const zm( ( std::abs( zs ) - ( zh * 3600 ) ) / 60 );
		std::stringstream zone_stream;
		zone_stream << std::setfill( '0' ) << ( zs >= 0 ? '+' : '-' ) << setw( 2 ) << zh << setw( 2 ) << zm;
		zone = zone_stream.str();
	}

	if ( values.present() ) {
		assert( ( values().l() <= 1 ) && ( values().u() >= 8 ) );
		values()( 1 ) = YY;
		values()( 2 ) = MM;
		values()( 3 ) = DD;
		values()( 4 ) = zs / 60;
		values()( 5 ) = hh;
		values()( 6 ) = mm;
		values()( 7 ) = ss;
		values()( 8 ) = ms;
	}
}

// Sleep for Given Seconds
void
SLEEP( double const sec )
{
	std::this_thread::sleep_for( std::chrono::microseconds( std::int64_t( sec * 1.0e6 ) ) );
}

} // ObjexxFCL
