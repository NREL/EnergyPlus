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
#include <ObjexxFCL/Time_Date.hh>

// C++ Headers
#include <cassert>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <cstdlib>
#include <ctime>
#include <limits>
#include <sstream>

namespace ObjexxFCL {

// Current Time: HH, MM, SS
void
itime( Array1< std::int32_t > & timearray )
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
gettim( std::int16_t & h, std::int16_t & m, std::int16_t & s, std::int16_t & c )
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
gettim( std::int32_t & h, std::int32_t & m, std::int32_t & s, std::int32_t & c )
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
	time_stream << std::setfill( '0' ) << setw( 2 ) << hh << setw( 2 ) << mm << setw( 2 ) << ss;
	time = time_stream.str();
}

// Time in Seconds Since Epoch
std::int64_t
TIME()
{
	using std::chrono::system_clock;
	system_clock::time_point const now( system_clock::now() );
	system_clock::duration const dur( now.time_since_epoch() );
	return dur.count() * system_clock::period::num / system_clock::period::den; // Eventually this doesn't fit in a 32-bit int so we're thinking ahead and returning a 64-bit!
}

// Clock
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
	time_stream << std::setfill( '0' ) << setw( 2 ) << hh << ":" << setw( 2 ) << mm << ":" << setw( 2 ) << ss;
	return time_stream.str();
}

// System Clock
void
SYSTEM_CLOCK( Optional< std::int64_t > count, Optional< std::int64_t > count_rate, Optional< std::int64_t > count_max )
{
	using std::chrono::system_clock;
	system_clock::time_point const now( system_clock::now() );
	if ( count.present() ) count = now.time_since_epoch().count();
	if ( count_rate.present() ) count_rate = system_clock::period::den / system_clock::period::num;
	if ( count_max.present() ) count_max = system_clock::duration::max().count();
}

// System Clock
void
SYSTEM_CLOCK32( Optional< std::int32_t > count, Optional< std::int32_t > count_rate, Optional< std::int32_t > count_max )
{
	using std::chrono::system_clock;
	system_clock::time_point const now( system_clock::now() );
	std::int64_t count_64( now.time_since_epoch().count() );
	std::int64_t count_rate_64( system_clock::period::den / system_clock::period::num );
	std::int64_t count_max_64( system_clock::duration::max().count() );
	std::int64_t mult( count_rate_64 / std::numeric_limits< int >::max() );
	if ( mult > 0 ) { // Scale rate down
		mult = std::pow( 10.0, std::ceil( std::log10( double( mult ) ) ) );
		count_64 /= mult;
		count_rate_64 /= mult;
		count_max_64 /= mult;
	}
	mult = count_max_64 / std::numeric_limits< int >::max();
	if ( mult > 0 ) { // Scale range down
		mult = std::pow( 10.0, std::ceil( std::log10( double( mult ) ) ) );
		count_max_64 /= mult;
		count_64 %= count_max_64;
	}
	if ( count.present() ) count = count_64;
	if ( count_rate.present() ) count_rate = count_rate_64;
	if ( count_max.present() ) count_max = count_max_64;
}

// Current Date: DD, MM, YYYY
void
idate( Array1< std::int32_t > & datearray )
{
	assert( datearray.l() <= 1 );
	assert( datearray.u() >= 3 );
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	datearray( 1 ) = timeinfo->tm_mday; // Day of month: 1,2,...
	datearray( 2 ) = timeinfo->tm_mon + 1; // Month of year: 1-12
	datearray( 3 ) = timeinfo->tm_year + 1900; // Year
}

// Current Date: MM, DD, YY (Not Y2K Compliant)
void
idate( std::int16_t & month, std::int16_t & day, std::int16_t & year )
{
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	month = timeinfo->tm_mon + 1; // Month of year: 1-12
	day = timeinfo->tm_mday; // Day of month: 1,2,...
	year = timeinfo->tm_year - ( timeinfo->tm_year / 100 ) * 100; // 2-Digit Year: 0-99
}

// Current Date: MM, DD, YY (Not Y2K Compliant)
void
idate( std::int32_t & month, std::int32_t & day, std::int32_t & year )
{
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	month = timeinfo->tm_mon + 1; // Month of year: 1-12
	day = timeinfo->tm_mday; // Day of month: 1,2,...
	year = timeinfo->tm_year - ( timeinfo->tm_year / 100 ) * 100; // 2-Digit Year: 0-99
}

// Current Date: DD, MM, YYYY (Year < 2000 is offset from 1900)
void
idate4( Array1< std::int32_t > & datearray )
{
	assert( datearray.l() <= 1 );
	assert( datearray.u() >= 3 );
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	datearray( 1 ) = timeinfo->tm_mday; // Day of month: 1,2,...
	datearray( 2 ) = timeinfo->tm_mon + 1; // Month of year: 1-12
	int year( timeinfo->tm_year ); // Year offset from 1900
	if ( year >= 100 ) year += 1900; // Year if >= 2000
	datearray( 3 ) = year;
}

// Current Date: MM, DD, YYYY (Year is offset from 1900)
void
idate4( std::int32_t & month, std::int32_t & day, std::int32_t & year )
{
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	month = timeinfo->tm_mon + 1; // Month of year: 1-12
	day = timeinfo->tm_mday; // Day of month: 1,2,...
	year = timeinfo->tm_year; // Year offset from 1900
}

// Current Date: YYYY, MM, DD
void
getdat( std::int16_t & year, std::int16_t & month, std::int16_t & day )
{
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	month = timeinfo->tm_mon + 1; // Month of year: 1-12
	day = timeinfo->tm_mday; // Day of month: 1,2,...
	year = timeinfo->tm_year + 1900; // Year offset from 1900
}

// Current Date: YYYY, MM, DD
void
getdat( std::int32_t & year, std::int32_t & month, std::int32_t & day )
{
	std::time_t const current_time( std::time( NULL ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );
	month = timeinfo->tm_mon + 1; // Month of year: 1-12
	day = timeinfo->tm_mday; // Day of month: 1,2,...
	year = timeinfo->tm_year + 1900; // Year offset from 1900
}

// Current Date String: DD-MMM-YY (Not Y2K Compliant)
void
date( std::string & day )
{
	int m, d, y;
	idate( m, d, y );
	std::stringstream s;
	s << std::setfill( '0' ) << std::setw( 2 ) << d;
	day = "  -   -  ";
	day.replace( 0, 2, s.str() );
	s.str( "" );
	s << std::setfill( '0' ) << std::setw( 2 ) << y;
	day.replace( 7, 2, s.str() );
	std::string mmm;
	switch ( m ) {
	case 1:
		mmm = "JAN";
		break;
	case 2:
		mmm = "FEB";
		break;
	case 3:
		mmm = "MAR";
		break;
	case 4:
		mmm = "APR";
		break;
	case 5:
		mmm = "MAY";
		break;
	case 6:
		mmm = "JUN";
		break;
	case 7:
		mmm = "JUL";
		break;
	case 8:
		mmm = "AUG";
		break;
	case 9:
		mmm = "SEP";
		break;
	case 10:
		mmm = "OCT";
		break;
	case 11:
		mmm = "NOV";
		break;
	case 12:
		mmm = "DEC";
		break;
	default:
		assert( false );
		mmm + "   ";
		break;
	}
	day.replace( 3, 3, mmm );
}

// Current Date String: YYDDD (Not Y2K Compliant)
std::string
jdate()
{
	int m, d, y;
	idate( m, d, y );
	y += 8000;
	if ( m < 3 ) {
		--y;
		m += 12;
	}
	std::stringstream s;
	s << std::setfill( '0' ) << std::setw( 5 ) << std::size_t( y * 365 ) + ( y / 4 ) - ( y / 100 ) + ( y / 400 ) - 1200820 + ( ( ( m * 153 ) + 3 ) / 5 ) - 92 + d - 1;
	return s.str();
}

// Local Time Zone Offset from UTC in Seconds
int
time_zone_offset_seconds()
{
	std::time_t const current_time( std::time( NULL ) );

	std::tm const * const ltimeinfo( std::localtime( &current_time ) ); // Local
	int const ld( ltimeinfo->tm_mday );
	int const lh( ltimeinfo->tm_hour );
	int const lm( ltimeinfo->tm_min );
	int const ls( ( 3600 * lh ) + ( 60 * lm ) );

	std::tm const * const gtimeinfo( std::gmtime( &current_time ) ); // UTC
	int const gd( gtimeinfo->tm_mday );
	int const gh( gtimeinfo->tm_hour );
	int const gm( gtimeinfo->tm_min );
	int const gs( ( 3600 * gh ) + ( 60 * gm ) );

	if ( gd == ld ) {
		if ( gs < ls ) {
			return ls - gs;
		} else if ( gs > ls ) {
			return ls - gs;
		}
	} else if ( gd < ld ) {
		return ( 86400 - gs ) + ls;
	} else {
		return -( ( 86400 - ls ) + gs );
	}
	return 0; // Don't get here
}

// Current Date and Time
void
date_and_time( Optional< std::string > date, Optional< std::string > time, Optional< std::string > zone, Optional< Array1D< int > > values )
{
	using std::chrono::system_clock;
	using std::chrono::duration_cast;
	using std::chrono::milliseconds;
	using std::setw;
	system_clock::time_point const now( system_clock::now() );
	int const ms( static_cast< int >( duration_cast< milliseconds >( now.time_since_epoch() ).count() % 1000 ) ); // msec
	std::time_t const current_time( system_clock::to_time_t( now ) );
	std::tm const * const timeinfo( std::localtime( &current_time ) );

	int const DD( timeinfo->tm_mday ); // Day of month: 1,2,...
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

} // ObjexxFCL
