#ifndef ObjexxFCL_MArray6_io_hh_INCLUDED
#define ObjexxFCL_MArray6_io_hh_INCLUDED

// MArray6 Input/Output Functions
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2014 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/MArray6.hh>
#include <ObjexxFCL/MArray.io.hh>

namespace ObjexxFCL {

// stream >> MArray6
template< class A, typename T >
std::istream &
operator >>( std::istream & stream, MArray6< A, T > & a )
{
	if ( ( stream ) && ( a.size() > 0u ) ) { // Read array from stream in row-major order
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
								stream >> a( i1, i2, i3, i4, i5, i6 );
								if ( ! stream ) break;
							} if ( ! stream ) break;
						} if ( ! stream ) break;
					} if ( ! stream ) break;
				} if ( ! stream ) break;
			} if ( ! stream ) break;
		}
	}
	return stream;
}

// stream << MArray6
template< class A, typename T >
std::ostream &
operator <<( std::ostream & stream, MArray6< A, T > const & a )
{
	if ( ( stream ) && ( a.size() > 0u ) ) { // Write array to stream in row-major order

		// Types
		using std::setw;
		typedef  TypeTraits< T >  Traits;

		// Save current stream state and set persistent state
		std::ios_base::fmtflags const old_flags( stream.flags() );
		std::streamsize const old_precision( stream.precision( Traits::precision() ) );
		stream << std::right << std::showpoint << std::uppercase;

		// Output array to stream
		int const w( Traits::iwidth() );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 < e6; ++i6 ) {
								stream << setw( w ) << a( i1, i2, i3, i4, i5, i6 ) << ' ';
								if ( ! stream ) break;
							} stream << setw( w ) << a( i1, i2, i3, i4, i5, a.u6() ) << '\n';
						} if ( ! stream ) break;
					} if ( ! stream ) break;
				} if ( ! stream ) break;
			} if ( ! stream ) break;
		}

		// Restore previous stream state
		stream.precision( old_precision );
		stream.flags( old_flags );

	}

	return stream;
}

// Read an MArray6 from a Binary File
template< class A, typename T >
std::istream &
read_binary( std::istream & stream, MArray6< A, T > & a )
{
	std::size_t const n( a.size() );
	if ( ( stream ) && ( n > 0u ) ) { // Read array from stream in column-major (Fortran) order
		std::size_t const type_size( sizeof( T ) / sizeof( std::istream::char_type ) );
		for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
			for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
							for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
								if ( stream ) {
									stream.read( ( std::istream::char_type * )&a( i1, i2, i3, i4, i5, i6 ), type_size );
								} else {
									break;
								}
							}
						}
					}
				}
			}
		}
	}
	return stream;
}

// Write an MArray6 to a Binary File
template< class A, typename T >
std::ostream &
write_binary( std::ostream & stream, MArray6< A, T > const & a )
{
	std::size_t const n( a.size() );
	if ( ( stream ) && ( n > 0u ) ) { // Write array to stream in column-major (Fortran) order
		std::size_t const type_size( sizeof( T ) / sizeof( std::ostream::char_type ) );
		for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
			for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
				for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
					for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
						for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
							for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
								if ( stream ) {
									stream.write( ( std::ostream::char_type const * )&a( i1, i2, i3, i4, i5, i6 ), type_size );
								} else {
									break;
								}
							}
						}
					}
				}
			}
		}
	}
	return stream;
}

namespace fmt {

// List-Directed Format: MArray6
template< class A, typename T >
inline
std::string
LD( MArray6< A, T > const & a )
{
	std::string s;
	std::size_t const n( a.size() );
	if ( n > 0u ) {
		s.reserve( n * TypeTraits< T >::width() );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5 ) {
							for ( int i6 = 1, e6 = a.u6(); i6 <= e6; ++i6 ) {
								s.append( fmt::LD( a( i1, i2, i3, i4, i5, i6 ) ) );
							}
						}
					}
				}
			}
		}
	}
	return s;
}

} // fmt

} // ObjexxFCL

#endif // ObjexxFCL_MArray6_io_hh_INCLUDED
