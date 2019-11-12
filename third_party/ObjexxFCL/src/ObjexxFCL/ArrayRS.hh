#ifndef ObjexxFCL_ArrayRS_hh_INCLUDED
#define ObjexxFCL_ArrayRS_hh_INCLUDED

// Rank Slice Array Proxy Abstract Base Class Template
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2019 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/ArrayRS.fwd.hh>
#include <ObjexxFCL/ArrayS.hh>
#include <ObjexxFCL/CArray.hh>

namespace ObjexxFCL {

// Rank Slice Array Proxy Abstract Base Class Template
template< typename T, int Rank >
class ArrayRS : public ArrayS< T >
{

private: // Types

	using Super = ArrayS< T >;

private: // Friend

	template< typename, int > friend class ArrayRS;

public: // Types

	using Base = typename Super::Base;
	using Traits = typename Super::Traits;
	using IR = typename Super::IR;
	using IS = typename Super::IS;
	using DS = typename Super::DS;

	// STL Style
	using value_type = typename Super::value_type;
	using reference = typename Super::reference;
	using const_reference = typename Super::const_reference;
	using pointer = typename Super::pointer;
	using const_pointer = typename Super::const_pointer;
	using size_type = typename Super::size_type;
	using difference_type = typename Super::difference_type;

	// C++ Style
	using Value = typename Super::Value;
	using Reference = typename Super::Reference;
	using ConstReference = typename Super::ConstReference;
	using Pointer = typename Super::Pointer;
	using ConstPointer = typename Super::ConstPointer;
	using Size = typename Super::Size;
	using Difference = typename Super::Difference;

	using Super::isize;
	using Super::overlap;
	using Super::size;

protected: // Types

	using Super::in_range;
	using Super::slice_k;

	using Super::contiguous_;
	using Super::data_;
	using Super::data_beg_;
	using Super::data_end_;
	using Super::size_;

protected: // Creation

	// Default Constructor
	ArrayRS()
	{}

	// Copy Constructor
	ArrayRS( ArrayRS const & a ) :
	 Super( a )
	{}

	// Data Constructor
	ArrayRS( T const * data, size_type const size ) :
	 Super( data, size )
	{}

	// Non-Const Data Constructor
	ArrayRS( T * data, size_type const size ) :
	 Super( data, size )
	{}

public: // Predicate

	// Conformable?
	template< template< typename > class A, typename U >
	bool
	conformable( A< U > const & a ) const
	{
		if ( Rank != a.rank() ) return false;
		for ( int i = 1; i <= Rank; ++i ) if ( size( i ) != a.size( i ) ) return false;
		return true;
	}

public: // Inspector

	// Rank
	int
	rank() const
	{
		return Rank;
	}

}; // ArrayRS

} // ObjexxFCL

#endif // ObjexxFCL_ArrayRS_hh_INCLUDED
