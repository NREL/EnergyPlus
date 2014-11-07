#ifndef ObjexxFCL_FArrayTail_hh_INCLUDED
#define ObjexxFCL_FArrayTail_hh_INCLUDED

// FArrayTail: Fortran-Compatible Contiguous Array Tail Proxy
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
#include <ObjexxFCL/FArrayTail.fwd.hh>

namespace ObjexxFCL {

// Forward
template< typename > class FArray;

// FArrayTail: Fortran-Compatible Contiguous Array Tail Proxy
template< typename T >
class FArrayTail
{

private: // Friend

	friend class FArray< T >;

public: // Types

	// STL style
	typedef  T  value_type;
	typedef  T &  reference;
	typedef  T const &  const_reference;
	typedef  T *  pointer;
	typedef  T const *  const_pointer;
	typedef  std::size_t  size_type;
	typedef  std::ptrdiff_t  difference_type;

	// C++ style
	typedef  T  Value;
	typedef  T &  Reference;
	typedef  T const &  ConstReference;
	typedef  T *  Pointer;
	typedef  T const *  ConstPointer;
	typedef  std::size_t  Size;
	typedef  std::ptrdiff_t  Difference;

public: // Creation

	// Copy Constructor
	inline
	FArrayTail( FArrayTail const & s ) :
	 data_( s.data_ ),
	 size_( s.size_ )
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	 , const_proxy_( true )
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	{}

	// Pointer + Size Constructor
	inline
	FArrayTail( T const * array, size_type const size ) :
	 data_( const_cast< T * >( array ) ),
	 size_( size )
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	 , const_proxy_( true )
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	{}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS

	// Non-Const Copy Constructor
	inline
	FArrayTail( FArrayTail & s ) :
	 data_( s.data_ ),
	 size_( s.size_ ),
	 const_proxy_( false )
	{}

	// Non-Const Pointer + Size Constructor
	inline
	FArrayTail( T * array, size_type const size ) :
	 data_( array ),
	 size_( size ),
	 const_proxy_( false )
	{}

#endif // OBJEXXFCL_PROXY_CONST_CHECKS

	// Destructor
	inline
	~FArrayTail()
	{}

public: // Assignment

	// Copy Assignment
	inline
	FArrayTail &
	operator =( FArrayTail const & s )
	{
		if ( this != &s ) {
			data_ = s.data_;
			size_ = s.size_;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
			const_proxy_ = s.const_proxy_;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		}
		return *this;
	}

public: // Inspector

	// Size
	inline
	size_type
	size() const
	{
		return size_;
	}

	// Size
	inline
	int
	isize() const
	{
		return static_cast< int >( size_ );
	}

private: // Data

	T * data_; // Pointer (non-owning) to data array

	size_type size_; // Size of data array

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	bool const_proxy_; // Proxy for const data array?
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

}; // FArrayTail

} // ObjexxFCL

#endif // ObjexxFCL_FArrayTail_hh_INCLUDED
