#ifndef ObjexxFCL_MArrayR_hh_INCLUDED
#define ObjexxFCL_MArrayR_hh_INCLUDED

// MArrayR: Rank Member Array Proxy Abstract Base Class Template
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
#include <ObjexxFCL/MArray.hh>

namespace ObjexxFCL {

// MArrayR: Rank Member Array Proxy Abstract Base Class Template
template< class A, typename T, int Rank >
class MArrayR : public MArray< A, T >
{

private: // Types

	typedef  MArray< A, T >  Super;

private: // Friend

	template< typename, typename, int > friend class MArrayR;

public: // Types

	typedef  typename Super::ArrayType  ArrayType;
	typedef  typename Super::Class  Class;
	typedef  typename Super::MPtr  MPtr;
	typedef  typename Super::Traits  Traits;
	typedef  typename Super::IR  IR;

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
	using Super::l;
	using Super::u;
	using Super::size;
	using Super::array_;
	using Super::pmem_;

protected: // Creation

	// Copy Constructor
	MArrayR( MArrayR const & a ) :
	 Super( a )
	{
		assert( a.rank() == Rank );
	}

	// Constructor
	MArrayR( A & a, T Class::* pmem ) :
	 Super( a, pmem )
	{}

public: // Creation

	// Destructor
	virtual
	~MArrayR()
	{}

protected: // Assignment

	// Copy Assignment
	MArrayR &
	operator =( MArrayR const & a ); // Disallow

public: // Inspector

	// Rank
	int
	rank() const
	{
		return Rank;
	}

}; // MArrayR

} // ObjexxFCL

#endif // ObjexxFCL_MArrayR_hh_INCLUDED
