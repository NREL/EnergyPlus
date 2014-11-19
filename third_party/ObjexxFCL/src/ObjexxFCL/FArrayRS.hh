#ifndef ObjexxFCL_FArrayRS_hh_INCLUDED
#define ObjexxFCL_FArrayRS_hh_INCLUDED

// FArrayRS: Rank Slice Array Proxy Abstract Base Class Template
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
#include <ObjexxFCL/FArrayRS.fwd.hh>
#include <ObjexxFCL/FArrayS.hh>
#include <ObjexxFCL/CArray.hh>

namespace ObjexxFCL {

// FArrayRS: Rank Slice Array Proxy Abstract Base Class Template
template< typename T, int Rank >
class FArrayRS : public FArrayS< T >
{

private: // Types

	typedef  FArrayS< T >  Super;

private: // Friend

	template< typename, int > friend class FArrayRS;

public: // Types

	typedef  typename Super::Base  Base;
	typedef  typename Super::Traits  Traits;
	typedef  typename Super::IR  IR;
	typedef  typename Super::IS  IS;
	typedef  typename Super::DS  DS;

	// STL Style
	typedef  typename Super::value_type  value_type;
	typedef  typename Super::reference  reference;
	typedef  typename Super::const_reference  const_reference;
	typedef  typename Super::pointer  pointer;
	typedef  typename Super::const_pointer  const_pointer;
	typedef  typename Super::size_type  size_type;
	typedef  typename Super::difference_type  difference_type;

	// C++ Style
	typedef  typename Super::Value  Value;
	typedef  typename Super::Reference  Reference;
	typedef  typename Super::ConstReference  ConstReference;
	typedef  typename Super::Pointer  Pointer;
	typedef  typename Super::ConstPointer  ConstPointer;
	typedef  typename Super::Size  Size;
	typedef  typename Super::Difference  Difference;

	// Using
	using Super::in_range;
	using Super::isize;
	using Super::overlap;
	using Super::size;
	using Super::slice_k;
	using Super::data_;
	using Super::data_beg_;
	using Super::data_end_;
	using Super::size_;

protected: // Creation

	// Default Constructor
	inline
	FArrayRS()
	{}

	// Copy Constructor
	inline
	FArrayRS( FArrayRS const & a ) :
	 Super( a )
	{}

	// Data Constructor
	inline
	FArrayRS( T const * data, size_type const size ) :
	 Super( data, size )
	{}

	// Non-Const Data Constructor
	inline
	FArrayRS( T * data, size_type const size ) :
	 Super( data, size )
	{}

public: // Creation

	// Destructor
	inline
	virtual
	~FArrayRS()
	{}

public: // Predicate

	// Conformable?
	template< template< typename > class ArrayType, typename U >
	inline
	bool
	conformable( ArrayType< U > const & a ) const
	{
		if ( Rank != a.rank() ) return false;
		for ( int i = 1; i <= Rank; ++i ) if ( size( i ) != a.size( i ) ) return false;
		return true;
	}

public: // Inspector

	// Rank
	inline
	int
	rank() const
	{
		return Rank;
	}

}; // FArrayRS

} // ObjexxFCL

#endif // ObjexxFCL_FArrayRS_hh_INCLUDED
